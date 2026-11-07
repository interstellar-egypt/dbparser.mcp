#' The parsers and sources the server supports
#'
#' What the \code{list_parsers} tool returns. Callable without a client.
#'
#' @keywords internal
#' @return named list
mcp_list_parsers <- function() {
  registry <- parser_registry()
  list(
    dbparser_version = as.character(utils::packageVersion("dbparser")),
    parsers = unname(lapply(names(registry), function(name) {
      entry  <- registry[[name]]
      sample <- sample_path(entry$sample)
      second <- sample_path(entry$second_sample)
      list(parser        = name,
           source        = entry$source,
           reads         = entry$reads,
           tables        = as.list(entry$tables),
           licence       = if (is.na(entry$licence)) "not stated"
                           else entry$licence,
           licence_url   = entry$licence_url,
           since         = entry$since,
           sample_path   = sample,
           sample_second_path = second)
    })),
    note = paste("Each source keeps its own licence; check its terms before",
                 "use. Sample paths are small extracts bundled with dbparser,",
                 "for trying the server; DrugBank has no sample here and is",
                 "parsed only from your own download."))
}


#' Parse a local file or folder into a dvobject kept in the session
#'
#' What the \code{parse_file} tool returns. Callable without a client.
#'
#' @param parser one of the parsers in \code{parser_registry()}
#' @param path local file or folder
#' @param second_path HGNC \code{withdrawn.txt}, or the second openFDA file
#' @param release,release_date,retrieved_at optional metadata
#'   (\code{"YYYY-MM-DD"} dates)
#' @param drugbank_sections "default" or "all" (DrugBank only)
#' @keywords internal
#' @return named list
mcp_parse_file <- function(parser,
                           path,
                           second_path       = NULL,
                           release           = NULL,
                           release_date      = NULL,
                           retrieved_at      = NULL,
                           drugbank_sections = "default") {
  registry <- parser_registry()
  check_exact_name(parser, names(registry), "parser",
                   "Call list_parsers for the parser names and what each reads.")
  entry <- registry[[parser]]
  check_memory_limit()

  path <- check_local_path(path, "path", entry$input)
  check_drugbank_source(parser, path)
  if (!is.null(second_path) && !identical(second_path, "")) {
    if (!(parser %in% c("parseHGNC", "parseOpenFDA"))) {
      mcp_stop(paste0("`second_path` is used only by parseHGNC ",
                      "(withdrawn.txt) and parseOpenFDA (the second ",
                      "download); ", parser, " reads one path."),
               "Call parse_file again without second_path.")
    }
    second_path <- check_local_path(second_path, "second_path", "file")
  } else {
    second_path <- NULL
  }
  for (date in list(list("release_date", release_date),
                    list("retrieved_at", retrieved_at))) {
    if (!is.null(date[[2]]) &&
        !grepl("^[0-9]{4}-[0-9]{2}-[0-9]{2}$", date[[2]])) {
      mcp_stop(paste0("`", date[[1]], "` must be a date written YYYY-MM-DD."),
               paste0("Give `", date[[1]], "` as YYYY-MM-DD, or leave it out."))
    }
  }
  check_exact_name(drugbank_sections, c("default", "all"),
                   "drugbank_sections",
                   "Use \"default\" (drug general information) or \"all\".")

  # zipped samples or downloads of the CSV parsers go to a temp folder
  tmp_dir <- tempfile("dbparser_mcp_")
  on.exit(unlink(tmp_dir, recursive = TRUE), add = TRUE)
  read_path <- path
  if ((parser %in% c("parseOnSIDES", "parseTWOSIDES")) &&
      grepl("\\.zip$", path, ignore.case = TRUE)) {
    extracted <- utils::unzip(path, exdir = tmp_dir)
    read_path <- if (identical(parser, "parseOnSIDES")) {
      dirname(extracted[grepl("product_label\\.csv$", extracted)][1])
    } else {
      extracted[grepl("\\.csv(\\.gz)?$", extracted)][1]
    }
    if (is.na(read_path)) {
      mcp_stop(paste0("The zip `", basename(path), "` holds no ",
                      entry$source, " files."),
               "Give the folder or file itself (see list_parsers).")
    }
  }

  started <- Sys.time()
  object  <- run_parser(parser, read_path, second_path, release,
                        release_date, retrieved_at, drugbank_sections)
  seconds <- round(as.numeric(difftime(Sys.time(), started, units = "secs")),
                   1)

  files <- c(path, second_path)
  id    <- store_add(object, parser, files)
  list(object_id = id,
       parser    = parser,
       files     = as.list(basename(files)),
       sample    = in_dbparser_samples(path),
       tables    = table_summary(object, columns = FALSE),
       memory_mb = the$objects[[id]]$memory_mb,
       session_memory_mb = store_total_mb(),
       seconds   = seconds,
       source    = source_block(object))
}


#' Run one dbparser parser with the arguments it takes
#' @inheritParams mcp_parse_file
#' @keywords internal
#' @return dvobject
run_parser <- function(parser, path, second_path, release, release_date,
                       retrieved_at, drugbank_sections) {
  object <- switch(
    parser,
    parseDrugBank = if (identical(drugbank_sections, "all")) {
      dbparser::parseDrugBank(
        path,
        drug_options       = dbparser::drug_node_options(),
        parse_salts        = TRUE,
        parse_products     = TRUE,
        references_options = dbparser::references_node_options(),
        cett_options       = dbparser::cett_nodes_options())
    } else {
      dbparser::parseDrugBank(path)
    },
    parseOnSIDES  = dbparser::parseOnSIDES(path, db_version = release,
                                           db_exported_date = release_date),
    parseTWOSIDES = dbparser::parseTWOSIDES(path, db_version = release,
                                            db_exported_date = release_date),
    parseHPO      = dbparser::parseHPO(path, db_version = release,
                                       db_exported_date = release_date),
    parseHGNC     = dbparser::parseHGNC(path, withdrawn_path = second_path,
                                        release = release,
                                        release_date = release_date,
                                        retrieved_at = retrieved_at),
    parseHealthCanadaDPD = dbparser::parseHealthCanadaDPD(
      path, release = release, release_date = release_date,
      retrieved_at = retrieved_at),
    parseOpenFDA  = {
      openfda <- openfda_paths(c(path, second_path))
      dbparser::parseOpenFDA(drugsfda_path = openfda$drugsfda,
                             ndc_path      = openfda$ndc,
                             release       = release,
                             release_date  = release_date,
                             retrieved_at  = retrieved_at)
    })

  if (NROW(dbparser::get_source_metadata(object)) == 0L) {
    mcp_stop(paste("The parser returned no data from", basename(path), "."),
             "Check that the file is the one list_parsers names.")
  }

  # the older parsers take no retrieved_at: record the caller's value
  if (!is.null(retrieved_at) && !isTRUE(parser_registry()[[parser]]$new_args)) {
    blocks <- attr(object, "source_metadata", exact = TRUE)
    blocks[[1]]$retrieved_at <- as.Date(retrieved_at)
    attr(object, "source_metadata") <- blocks
  }
  object
}


#' Which openFDA file is which, from openFDA's own file names
#' @param paths one or two paths
#' @keywords internal
#' @return list(drugsfda, ndc)
openfda_paths <- function(paths) {
  names <- tolower(basename(paths))
  drugsfda <- paths[grepl("drugsfda", names)]
  ndc      <- paths[grepl("(^|-)ndc(-|\\.)", names)]
  if ((length(drugsfda) + length(ndc) != length(paths)) ||
      (length(drugsfda) > 1L) || (length(ndc) > 1L)) {
    mcp_stop(paste("Can't tell the openFDA files apart from their names;",
                   "openFDA names them drug-drugsfda-... and drug-ndc-...."),
             paste("Give the Drugs@FDA and/or NDC downloads with the names",
                   "openFDA gives them."))
  }
  list(drugsfda = if (length(drugsfda)) drugsfda else NULL,
       ndc      = if (length(ndc)) ndc else NULL)
}


#' Is a path one of dbparser's bundled sample files?
#' @param path normalized path
#' @keywords internal
#' @return logical(1)
in_dbparser_samples <- function(path) {
  installed <- normalizePath(system.file(package = "dbparser"),
                             mustWork = FALSE)
  nzchar(installed) &&
    startsWith(path, paste0(installed, .Platform$file.sep))
}


#' Tables, rows and (optionally) columns of a dvobject
#' @param object dvobject
#' @param columns include column names and types
#' @param prefix path so far
#' @keywords internal
#' @return list of named lists
table_summary <- function(object, columns = TRUE, prefix = "") {
  if (is.data.frame(object)) {
    entry <- list(table = prefix, rows = NROW(object))
    if (columns) {
      entry$columns <- unname(lapply(names(object), function(name) {
        list(name = name, type = class(object[[name]])[1])
      }))
    }
    return(list(entry))
  }
  if (is.list(object) && length(object) > 0L) {
    return(unlist(lapply(names(object), function(name) {
      table_summary(object[[name]], columns,
                    if (prefix == "") name else paste0(prefix, "$", name))
    }), recursive = FALSE))
  }
  list()
}


#' Tables, columns and source metadata of a parsed object
#'
#' What the \code{describe} tool returns. Callable without a client.
#'
#' @param object_id id from \code{mcp_parse_file()}
#' @keywords internal
#' @return named list
mcp_describe <- function(object_id) {
  entry <- store_get(object_id)
  list(object_id = object_id,
       parser    = entry$parser,
       files     = as.list(entry$files),
       memory_mb = entry$memory_mb,
       tables    = table_summary(entry$object, columns = TRUE),
       source    = source_block(entry$object, full = TRUE))
}
