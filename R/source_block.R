#' The source block every answer carries
#'
#' One entry per source of the object (a merged object has several), from
#' \code{dbparser::get_source_metadata()}.
#'
#' @param object dvobject
#' @param full also include the file names and SHA-256 hashes
#' @keywords internal
#' @return list of named lists
source_block <- function(object, full = FALSE) {
  metadata <- dbparser::get_source_metadata(object)
  lapply(seq_len(NROW(metadata)), function(i) {
    text <- function(field) {
      value <- metadata[[field]][i]
      if (is.na(value)) NA_character_ else as.character(value)
    }
    block <- list(source       = text("source"),
                  source_name  = text("source_name"),
                  release      = text("release"),
                  release_date = text("release_date"),
                  retrieved_at = text("retrieved_at"),
                  licence      = text("licence"),
                  licence_url  = text("licence_url"),
                  attribution  = text("attribution"))
    if (full) {
      files <- metadata$files[[i]]
      block$source_url <- text("source_url")
      block$parser     <- text("parser")
      block$files      <- lapply(seq_len(NROW(files)), function(j) {
        list(name = files$name[j], sha256 = files$sha256[j])
      })
    }
    block
  })
}
