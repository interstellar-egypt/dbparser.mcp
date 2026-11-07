# most rows read_table returns in one call
read_table_max_rows <- 100L

# longest text cell read_table returns before cutting it, in characters
read_table_max_chars <- 500L


#' Rows of one table of a parsed object
#'
#' What the \code{read_table} tool returns. Callable without a client. At
#' most \code{read_table_max_rows} rows come back per call; text cells longer
#' than \code{read_table_max_chars} characters are cut, and the result says
#' which columns were.
#'
#' @param object_id id from \code{mcp_parse_file()}
#' @param table table name as \code{describe} lists it, e.g.
#'   \code{"drugs$general_information"}
#' @param n rows to return (default 20, at most 100)
#' @param offset rows to skip first (default 0)
#' @keywords internal
#' @return named list
mcp_read_table <- function(object_id, table, n = 20L, offset = 0L) {
  entry <- store_get(object_id)
  data  <- get_table(entry$object, table, object_id)
  n      <- check_count(n, "n", minimum = 1L)
  offset <- check_count(offset, "offset", minimum = 0L)

  total    <- NROW(data)
  notes    <- if (total == 0L) "This table has no rows." else character(0)
  if (n > read_table_max_rows) {
    notes <- c(notes, sprintf("n is capped at %d; returning %d rows.",
                              read_table_max_rows, read_table_max_rows))
    n <- read_table_max_rows
  }
  rows     <- if (offset < total) {
    seq.int(offset + 1L, min(offset + n, total))
  } else {
    integer(0)
  }
  returned <- length(rows)

  cells   <- json_cells(data[rows, , drop = FALSE])
  cut     <- truncate_cells(cells, read_table_max_chars)
  if (length(cut$columns) > 0L) {
    notes <- c(notes, sprintf(paste(
      "%d cell(s) longer than %d characters were cut, ending in \"...\"",
      "(columns: %s). export_csv writes them in full."),
      cut$count, read_table_max_chars, paste(cut$columns, collapse = ", ")))
  }
  if (offset >= total && total > 0L) {
    notes <- c(notes, sprintf(paste(
      "offset %d is past the end of the table (%d rows); no rows returned."),
      offset, total))
  } else if (offset + returned < total) {
    notes <- c(notes, sprintf(paste(
      "Rows %d-%d of %d. Use offset to read more (up to %d rows a call),",
      "or export_csv for the whole table."),
      offset + 1L, offset + returned, total, read_table_max_rows))
  }

  list(object_id     = object_id,
       table         = table,
       offset        = offset,
       returned_rows = returned,
       total_rows    = total,
       columns       = unname(lapply(names(data), function(name) {
         list(name = name, type = class(data[[name]])[1])
       })),
       rows          = cells_as_records(cut$cells, returned),
       notes         = as.list(notes),
       source        = source_block(entry$object))
}


#' Write one table of a parsed object to a new CSV file
#'
#' What the \code{export_csv} tool returns. Callable without a client. It never
#' overwrites: the path must not exist yet. The result carries the licence and
#' attribution of the data written.
#'
#' @inheritParams mcp_read_table
#' @param path full local path of the new \code{.csv} file
#' @keywords internal
#' @return named list
mcp_export_csv <- function(object_id, table, path) {
  entry <- store_get(object_id)
  data  <- get_table(entry$object, table, object_id)
  path  <- check_new_csv_path(path)

  # write next to the target, then move it into place, so a failed write
  # never leaves a partial file at `path`
  tmp <- tempfile(".dbparser_mcp_", tmpdir = dirname(path), fileext = ".csv")
  on.exit(unlink(tmp), add = TRUE)
  utils::write.csv(as.data.frame(json_cells(data), optional = TRUE,
                                 stringsAsFactors = FALSE),
                   tmp, row.names = FALSE, na = "", fileEncoding = "UTF-8")
  if (file.exists(path) || !file.rename(tmp, path)) {
    mcp_stop(paste0("Could not create `", path, "`",
                    if (file.exists(path)) "; a file appeared there while writing."
                    else "."),
             "Choose a new file path in a folder you can write to.")
  }

  source <- source_block(entry$object)
  list(path         = path,
       object_id    = object_id,
       table        = table,
       rows         = NROW(data),
       columns      = length(data),
       bytes        = file.size(path),
       licence_note = licence_note(source),
       source       = source)
}


#' A table of an object by its describe name, or a tool error listing them
#' @param object dvobject
#' @param table name such as \code{"drugs$general_information"}
#' @param object_id for the error
#' @keywords internal
#' @return data.frame
get_table <- function(object, table, object_id) {
  valid <- vapply(table_summary(object, columns = FALSE),
                  function(x) x$table, "")
  check_exact_name(table, valid, "table",
                   paste0("Use a table name exactly as describe(\"", object_id,
                          "\") lists it."))
  Reduce(function(x, name) x[[name]], strsplit(table, "$", fixed = TRUE)[[1]],
         object)
}


#' Check a count argument is a whole number at least \code{minimum}
#' @param value value given by the client
#' @param arg argument name
#' @param minimum smallest allowed value
#' @keywords internal
#' @return integer(1)
check_count <- function(value, arg, minimum) {
  if (!is.numeric(value) || (length(value) != 1L) || is.na(value) ||
      (value != round(value)) || (value < minimum)) {
    mcp_stop(paste0("`", arg, "` must be a whole number, at least ", minimum,
                    "."),
             paste0("Give `", arg, "` as a whole number, or leave it out."))
  }
  as.integer(value)
}


#' Check a path for a new CSV file: local, ends in .csv, does not exist yet,
#' and its folder does
#' @param path value passed by the client
#' @keywords internal
#' @return the normalized path
check_new_csv_path <- function(path) {
  if (!is.character(path) || (length(path) != 1L) || is.na(path) ||
      !nzchar(path)) {
    mcp_stop("`path` must be one file path.",
             "Give the full path of the new .csv file on this computer.")
  }
  if (grepl("^[A-Za-z][A-Za-z0-9+.-]*://", path)) {
    mcp_stop("`path` is a URL; this server writes only local files.",
             "Give the full path of the new .csv file on this computer.")
  }
  if (!grepl("\\.csv$", path, ignore.case = TRUE)) {
    mcp_stop("`path` must end in .csv.",
             "Give a file name ending in .csv.")
  }
  path <- path.expand(path)
  if (file.exists(path)) {
    mcp_stop(paste0("`", path, "` already exists; export_csv never ",
                    "overwrites a file."),
             "Choose a new file name, or move the existing file first.")
  }
  folder <- dirname(path)
  if (!dir.exists(folder)) {
    mcp_stop(paste0("The folder `", folder, "` does not exist."),
             "Give a path in an existing folder (the full path).")
  }
  file.path(normalizePath(folder, mustWork = TRUE), basename(path))
}


#' Columns of a data frame as JSON- and CSV-ready vectors
#'
#' Factors, dates and times become text; list columns become their values
#' joined by "; "; non-finite numbers become NA.
#'
#' @param data data.frame
#' @keywords internal
#' @return named list of atomic vectors
json_cells <- function(data) {
  lapply(as.list(data), function(column) {
    if (is.list(column)) {
      vapply(column, function(value) {
        value <- unlist(value, use.names = FALSE)
        if (length(value) == 0L || all(is.na(value))) NA_character_
        else paste(as.character(value), collapse = "; ")
      }, character(1))
    } else if (inherits(column, "POSIXt")) {
      format(column, "%Y-%m-%dT%H:%M:%S%z")
    } else if (is.factor(column) || inherits(column, "Date") ||
               !(is.logical(column) || is.numeric(column) ||
                 is.character(column))) {
      as.character(column)
    } else if (is.double(column)) {
      column[!is.finite(column)] <- NA
      as.vector(column)
    } else {
      as.vector(column)
    }
  })
}


#' Cut text cells longer than \code{limit} characters
#' @param cells named list from \code{json_cells()}
#' @param limit characters kept
#' @keywords internal
#' @return list(cells, count, columns)
truncate_cells <- function(cells, limit) {
  count   <- 0L
  columns <- character(0)
  for (name in names(cells)) {
    column <- cells[[name]]
    if (!is.character(column)) {
      next
    }
    long <- !is.na(column) &
      (nchar(column, type = "chars", allowNA = TRUE) > limit) %in% TRUE
    if (any(long)) {
      column[long] <- paste0(substr(column[long], 1L, limit), "...")
      cells[[name]] <- column
      count   <- count + sum(long)
      columns <- c(columns, name)
    }
  }
  list(cells = cells, count = count, columns = columns)
}


#' Rows as a list of named lists, NA as JSON null
#' @param cells named list of vectors
#' @param n number of rows
#' @keywords internal
#' @return list
cells_as_records <- function(cells, n) {
  lapply(seq_len(n), function(i) {
    lapply(cells, function(column) {
      value <- column[[i]]
      if (is.na(value)) NULL else value
    })
  })
}


#' What to keep with an exported file: each source's licence and attribution
#' @param source list from \code{source_block()}
#' @keywords internal
#' @return character(1)
licence_note <- function(source) {
  parts <- vapply(source, function(block) {
    name    <- if (is.na(block$source_name)) block$source else block$source_name
    release <- if (is.na(block$release)) "" else
      paste0(" (release ", block$release, ")")
    licence <- if (is.na(block$licence)) {
      paste0("licence not stated by dbparser; your use is governed by the ",
             "terms you downloaded it under",
             if (!is.na(block$licence_url)) paste0(" (", block$licence_url, ")")
             else "")
    } else {
      paste0("licence ", block$licence,
             if (!is.na(block$licence_url)) paste0(" (", block$licence_url, ")")
             else "")
    }
    attribution <- if (is.na(block$attribution)) "" else
      paste0(" Attribution: ", block$attribution)
    paste0(name, release, ": ", licence, ".", attribution)
  }, character(1))
  paste("This file holds data from", paste(parts, collapse = " "),
        "Keep this licence and attribution with the file and with anything",
        "made from it; the file is yours to manage under those terms.")
}
