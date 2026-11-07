#' Signal a tool error the client can act on
#'
#' @param error what went wrong
#' @param next_step what the user or model should do next
#' @keywords internal
#' @return never returns; signals a \code{dbparser_mcp_error} condition
mcp_stop <- function(error, next_step) {
  condition <- structure(
    class   = c("dbparser_mcp_error", "error", "condition"),
    list(message = error, next_step = next_step, call = NULL))
  stop(condition)
}


#' Check a path is an existing local file or folder, never a URL
#'
#' @param path value passed by the client
#' @param arg argument name, for the error
#' @param input "file", "dir" or "either"
#' @keywords internal
#' @return the normalized path
check_local_path <- function(path, arg = "path", input = "either") {
  if (!is.character(path) || (length(path) != 1L) || is.na(path) ||
      !nzchar(path)) {
    mcp_stop(paste0("`", arg, "` must be one file or folder path."),
             "Give the full path of a file or folder on this computer.")
  }
  if (grepl("^[A-Za-z][A-Za-z0-9+.-]*://", path)) {
    mcp_stop(paste0("`", arg, "` is a URL; this server reads only local ",
                    "files and never downloads anything."),
             paste("Download the file yourself from the source, under its",
                   "terms, then give its local path."))
  }
  path <- path.expand(path)
  if (!file.exists(path)) {
    mcp_stop(paste0("Nothing found at `", path, "`."),
             "Check the path (use the full path), then try again.")
  }
  is_dir <- dir.exists(path)
  if (identical(input, "file") && is_dir) {
    mcp_stop(paste0("`", path, "` is a folder; this parser reads a file."),
             "Give the path of the file itself (see list_parsers).")
  }
  if (identical(input, "dir") && !is_dir &&
      !grepl("\\.zip$", path, ignore.case = TRUE)) {
    mcp_stop(paste0("`", path, "` is a file; this parser reads a folder."),
             "Give the folder that holds the files (see list_parsers).")
  }
  normalizePath(path, mustWork = TRUE)
}


#' Is a path inside an installed R package library?
#' @param path normalized path
#' @keywords internal
#' @return logical(1)
in_package_library <- function(path) {
  libraries <- normalizePath(.libPaths(), mustWork = FALSE)
  any(vapply(libraries, function(library) {
    startsWith(path, paste0(library, .Platform$file.sep))
  }, logical(1)))
}


#' BR-1: DrugBank content only from the user's own file
#'
#' DrugBank is not an open database. The server parses DrugBank only from a
#' file the user gives, never from a package library (dbparser's bundled
#' record, dbdataset or any other).
#'
#' @param parser parser name
#' @param path normalized path
#' @keywords internal
#' @return NULL, invisibly
check_drugbank_source <- function(parser, path) {
  if (identical(parser, "parseDrugBank") && in_package_library(path)) {
    mcp_stop(paste("DrugBank is parsed only from your own DrugBank download;",
                   "this path is inside an R package library."),
             paste("Download the DrugBank XML under your own DrugBank",
                   "account and licence, then give its path."))
  }
  invisible(NULL)
}


#' Check a name against the valid names, exactly
#'
#' @param value name given by the client
#' @param valid valid names
#' @param what what kind of name, for the error
#' @param next_step hint
#' @keywords internal
#' @return value
check_exact_name <- function(value, valid, what, next_step) {
  if (!is.character(value) || (length(value) != 1L) || is.na(value) ||
      !(value %in% valid)) {
    mcp_stop(paste0("Unknown ", what, " `", paste(value, collapse = ", "),
                    "`. Valid: ", paste(valid, collapse = ", "), "."),
             next_step)
  }
  value
}


#' Refuse a new parse when the session already holds too much
#'
#' The size of a parse can't be known in advance, so the check is on what is
#' already loaded.
#'
#' @keywords internal
#' @return NULL, invisibly
check_memory_limit <- function() {
  limit <- memory_limit_mb()
  used  <- store_total_mb()
  if (used > limit) {
    mcp_stop(sprintf(paste("This session already holds %.0f MB of parsed",
                           "objects, over the %.0f MB limit",
                           "(DBPARSER_MCP_MAX_MB)."), used, limit),
             paste("Start a new session (restart the MCP server) to free",
                   "memory, or raise DBPARSER_MCP_MAX_MB."))
  }
  invisible(NULL)
}


#' The session memory limit, in MB (\code{DBPARSER_MCP_MAX_MB}, default 4000)
#' @keywords internal
#' @return numeric(1)
memory_limit_mb <- function() {
  limit <- suppressWarnings(as.numeric(Sys.getenv("DBPARSER_MCP_MAX_MB",
                                                  "4000")))
  if (is.na(limit)) 4000 else limit
}
