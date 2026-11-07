the <- new.env(parent = emptyenv())
the$objects <- list()
the$counter <- 0L


#' Keep a parsed object in the server session
#'
#' @param object dvobject
#' @param parser parser name
#' @param files paths read
#' @keywords internal
#' @return the new object id
store_add <- function(object, parser, files) {
  metadata <- dbparser::get_source_metadata(object)
  prefix   <- if (NROW(metadata) > 0L) metadata$source[1] else "object"
  the$counter <- the$counter + 1L
  id <- paste0(prefix, "_", the$counter)
  the$objects[[id]] <- list(object    = object,
                            parser    = parser,
                            files     = basename(files),
                            memory_mb = object_mb(object))
  id
}


#' A stored object, or a tool error naming the stored ids
#' @param object_id id from parse_file
#' @keywords internal
#' @return the stored entry
store_get <- function(object_id) {
  ids <- names(the$objects)
  if (!is.character(object_id) || (length(object_id) != 1L) ||
      !(object_id %in% ids)) {
    mcp_stop(paste0("No parsed object `", paste(object_id, collapse = ", "),
                    "` in this session.",
                    if (length(ids) > 0L) {
                      paste0(" Parsed objects: ", paste(ids, collapse = ", "),
                             ".")
                    } else {
                      " Nothing has been parsed yet."
                    }),
             paste("Use an object_id returned by parse_file in this session;",
                   "objects do not survive a server restart."))
  }
  the$objects[[object_id]]
}


#' Total memory of the stored objects, in MB
#' @keywords internal
#' @return numeric(1)
store_total_mb <- function() {
  sum(vapply(the$objects, function(entry) entry$memory_mb, numeric(1)))
}


#' Forget every stored object (for tests)
#' @keywords internal
#' @return NULL, invisibly
store_clear <- function() {
  the$objects <- list()
  the$counter <- 0L
  invisible(NULL)
}


#' Size of an object in MB
#' @param object any R object
#' @keywords internal
#' @return numeric(1)
object_mb <- function(object) {
  round(as.numeric(utils::object.size(object)) / 1024^2, 1)
}
