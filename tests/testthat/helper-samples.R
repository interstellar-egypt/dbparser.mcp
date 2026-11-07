# copy a bundled dbparser file into a temp folder, so it is "the user's own
# file" and not a package-library path
user_copy <- function(..., env = parent.frame()) {
  source <- system.file(..., package = "dbparser")
  dir    <- withr_tempdir(env)
  file.copy(source, dir, recursive = TRUE)
  file.path(dir, basename(source))
}

withr_tempdir <- function(env) {
  dir <- tempfile("dbparser_mcp_test_")
  dir.create(dir)
  do.call(on.exit, list(substitute(unlink(dir, recursive = TRUE),
                                   list(dir = dir)), add = TRUE),
          envir = env)
  dir
}

expect_mcp_error <- function(expr, pattern) {
  condition <- tryCatch(expr, dbparser_mcp_error = function(e) e)
  expect_s3_class(condition, "dbparser_mcp_error")
  expect_match(conditionMessage(condition), pattern)
  expect_true(nzchar(condition$next_step))
  invisible(condition)
}
