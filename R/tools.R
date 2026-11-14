#' Run a tool body for the MCP server
#'
#' Sends anything printed to stderr, so it can't corrupt the stdio stream, and
#' turns errors into an \code{ellmer::ContentToolResult} with its error set,
#' which mcptools sends as a result with \code{isError: true}. The error text
#' is JSON with \code{error} and \code{next_step}.
#'
#' @param fun the implementation function
#' @keywords internal
#' @return a function with the same arguments
as_server_tool <- function(fun) {
  force(fun)
  wrapped <- function() {
    args <- as.list(environment())
    # mcptools unlist()s array arguments, which loses where one filter ends
    # when a value is a list; use the arguments as the client sent them
    if ("filters" %in% names(args)) {
      raw <- mcp_raw_arguments()
      if (!is.null(raw)) args["filters"] <- list(raw$filters)
    }
    sink(stderr(), type = "output")
    on.exit(sink(type = "output"), add = TRUE)
    tryCatch(do.call(fun, args), error = tool_error_result)
  }
  formals(wrapped) <- formals(fun)
  wrapped
}


#' The tool call's arguments as parsed from the client's JSON, before mcptools
#' flattens array arguments
#'
#' mcptools (1.0.3) keeps the parsed JSON-RPC request in \code{data} in the
#' frame that calls the tool. The stdio round-trip test checks this still
#' holds. The filter format may move to a JSON object (\code{{"column":
#' [values]}}) in a later version, which mcptools passes through unchanged.
#'
#' @keywords internal
#' @return named list, or NULL when not called from mcptools
mcp_raw_arguments <- function() {
  for (frame in rev(sys.frames())) {
    if (exists("data", envir = frame, inherits = FALSE)) {
      data <- get("data", envir = frame, inherits = FALSE)
      if (is.list(data) && is.list(data$params) &&
          !is.null(data$params$name)) {
        arguments <- data$params$arguments
        return(if (is.list(arguments)) arguments else list())
      }
    }
  }
  NULL
}


#' The error result for a failed tool call
#' @param e condition
#' @keywords internal
#' @return ellmer::ContentToolResult
tool_error_result <- function(e) {
  next_step <- if (inherits(e, "dbparser_mcp_error")) {
    e$next_step
  } else {
    "Check the inputs against list_parsers and describe, then try again."
  }
  ellmer::ContentToolResult(
    error = jsonlite_text(list(error = conditionMessage(e),
                               next_step = next_step)))
}


#' One-line JSON text
#' @param x list
#' @keywords internal
#' @return character(1)
jsonlite_text <- function(x) {
  as.character(jsonlite::toJSON(x, auto_unbox = TRUE, null = "null"))
}


#' The dbparser MCP tools
#'
#' The tool definitions \code{serve()} gives to \code{mcptools::mcp_server()}.
#' Each tool is read-only except \code{export_csv}, which only writes a new
#' file at a path the user gives.
#'
#' @return list of \code{ellmer::tool()} objects
#' @export
mcp_tools <- function() {
  parsers <- names(parser_registry())
  read_only <- ellmer::tool_annotations(read_only_hint = TRUE,
                                        open_world_hint = FALSE)

  list(
    ellmer::tool(
      as_server_tool(mcp_list_parsers),
      name = "list_parsers",
      description = paste(
        "List the dbparser parsers: the source each reads, the file names it",
        "expects, the tables it returns, the licence of the source data and",
        "the dbparser version that added it. Sample paths are small extracts",
        "bundled with dbparser, for trying the server without downloading",
        "anything; DrugBank has no sample and is parsed only from the user's",
        "own download."),
      annotations = c(read_only,
                      ellmer::tool_annotations(title = "List parsers"))),

    ellmer::tool(
      as_server_tool(mcp_parse_file),
      name = "parse_file",
      description = paste(
        "Parse a local file or folder with one dbparser parser and keep the",
        "result in this server session; returns an object_id for describe.",
        "Reads only local paths and never downloads anything. DrugBank is",
        "parsed only from the user's own DrugBank file, never from a package",
        "library. The bundled sample files from list_parsers are small",
        "extracts. Large files take time and memory: the full openFDA",
        "Drugs@FDA and NDC files take about a minute and roughly 300 MB, a",
        "full DrugBank XML several minutes and GBs. memory_mb reports the",
        "parsed object's size."),
      arguments = list(
        parser = ellmer::type_enum(
          parsers, "The parser to use (see list_parsers)."),
        path = ellmer::type_string(
          "Full local path of the file or folder to parse."),
        second_path = ellmer::type_string(
          paste("parseHGNC: path of withdrawn.txt. parseOpenFDA: the second",
                "download (Drugs@FDA or NDC). Not used by other parsers."),
          required = FALSE),
        release = ellmer::type_string(
          "Release label of the source files, e.g. 2026-10.",
          required = FALSE),
        release_date = ellmer::type_string(
          "Release date of the source files, YYYY-MM-DD.",
          required = FALSE),
        retrieved_at = ellmer::type_string(
          "Date the files were downloaded, YYYY-MM-DD.", required = FALSE),
        drugbank_sections = ellmer::type_enum(
          c("default", "all"),
          paste("parseDrugBank only: \"default\" parses drug general",
                "information; \"all\" parses every drug, reference and",
                "carrier/enzyme/target/transporter section."),
          required = FALSE)),
      annotations = ellmer::tool_annotations(
        title = "Parse a local file", read_only_hint = TRUE,
        open_world_hint = FALSE, idempotent_hint = FALSE)),

    ellmer::tool(
      as_server_tool(mcp_describe),
      name = "describe",
      description = paste(
        "Describe a parsed object: every table with its row count and",
        "columns (name and R type), and the source metadata (release,",
        "licence, licence URL, attribution, and each file read with its",
        "SHA-256 hash)."),
      arguments = list(
        object_id = ellmer::type_string("An object_id from parse_file.")),
      annotations = c(read_only,
                      ellmer::tool_annotations(title = "Describe an object"))),

    ellmer::tool(
      as_server_tool(mcp_list_objects),
      name = "list_objects",
      description = paste(
        "List the objects parsed in this server session: each object_id",
        "with its parser, files, tables and row counts, memory_mb and source",
        "(release, licence, attribution), plus the session's memory use and",
        "limit. Objects do not survive a server restart."),
      annotations = c(read_only,
                      ellmer::tool_annotations(title = "List parsed objects"))),

    ellmer::tool(
      as_server_tool(mcp_read_table),
      name = "read_table",
      description = paste(
        "Read rows of one table of a parsed object: 20 by default, at most",
        sprintf("%d per call; use offset to page.", read_table_max_rows),
        "The result always gives returned_rows and total_rows. Text cells",
        sprintf("longer than %d characters are cut", read_table_max_chars),
        "and a note names their columns; export_csv writes them in full.",
        "Optional filters keep only rows where a column equals a value",
        "exactly (case-sensitive, no partial match). A filter's value may be",
        "a list of up to 100 values, any of which may match, e.g. several",
        "gene symbols in one call. Filters on different columns combine with",
        "AND. total_rows then counts the matching rows, and a note states the",
        "filter. Every answer carries the source block."),
      arguments = list(
        object_id = ellmer::type_string("An object_id from parse_file."),
        table = ellmer::type_string(paste(
          "Table name exactly as describe lists it, e.g.",
          "drugs$general_information or gene.")),
        n = ellmer::type_integer(
          sprintf("Rows to return: default 20, at most %d.",
                  read_table_max_rows), required = FALSE),
        offset = ellmer::type_integer(
          "Rows to skip first (after filtering): default 0.",
          required = FALSE),
        filters = filters_type()),
      annotations = c(read_only,
                      ellmer::tool_annotations(title = "Read table rows"))),

    ellmer::tool(
      as_server_tool(mcp_export_csv),
      name = "export_csv",
      description = paste(
        "Write one table of a parsed object to a new CSV file (UTF-8) at a",
        "local path: the whole table, or with filters only the matching rows",
        "(same filters as read_table: exact match, a value list matches any",
        "of its values, AND across columns).",
        "It never overwrites: the file must not exist yet,",
        "and its folder must. The result gives the rows written and each",
        "source's licence and attribution, which the user must keep with the",
        "file. The only tool that writes anything."),
      arguments = list(
        object_id = ellmer::type_string("An object_id from parse_file."),
        table = ellmer::type_string(
          "Table name exactly as describe lists it."),
        path = ellmer::type_string(
          "Full local path of the new file, ending in .csv."),
        filters = filters_type()),
      annotations = ellmer::tool_annotations(
        title = "Export a table to CSV", read_only_hint = FALSE,
        destructive_hint = FALSE, idempotent_hint = FALSE,
        open_world_hint = FALSE))
  )
}


#' The \code{filters} argument of read_table and export_csv
#' @keywords internal
#' @return an ellmer type
filters_type <- function() {
  ellmer::type_array(
    ellmer::type_object(
      column = ellmer::type_string(
        "Column name exactly as describe lists it for this table."),
      value = ellmer::type_array(
        ellmer::type_string(),
        paste("Values the column must equal exactly (compared as text,",
              "case-sensitive); a row matches if it equals any of them.",
              "One value is fine too. At most 100."))),
    paste("Optional. Keep only rows matching every filter (AND across",
          "columns), e.g. [{\"column\": \"symbol\", \"value\":",
          "[\"PDCD1\", \"SNCA\", \"SPATA2\"]}] or",
          "[{\"column\": \"name\", \"value\": [\"PD1\"]}]."),
    required = FALSE)
}
