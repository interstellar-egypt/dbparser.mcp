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
    sink(stderr(), type = "output")
    on.exit(sink(type = "output"), add = TRUE)
    tryCatch(do.call(fun, args), error = tool_error_result)
  }
  formals(wrapped) <- formals(fun)
  wrapped
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
                      ellmer::tool_annotations(title = "Describe an object")))
  )
}
