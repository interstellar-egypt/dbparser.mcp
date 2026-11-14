# A real MCP client exchange: start `serve()` in a child R process, speak
# JSON-RPC over its stdin and stdout, and check that every line it writes to
# stdout is a JSON-RPC message.

server_command <- function() {
  # under devtools::test() the source tree is loaded, not installed
  if (requireNamespace("pkgload", quietly = TRUE) &&
      pkgload::is_dev_package("dbparser.mcp")) {
    sprintf("pkgload::load_all(%s, quiet = TRUE, helpers = FALSE); serve()",
            deparse(pkgload::pkg_path()))
  } else {
    "dbparser.mcp::serve()"
  }
}

read_until <- function(process, lines, ids, timeout = 60) {
  deadline <- Sys.time() + timeout
  seen <- function() {
    got <- lapply(lines, function(line) {
      tryCatch(jsonlite::parse_json(line)$id, error = function(e) NULL)
    })
    all(ids %in% unlist(got))
  }
  while (!seen() && Sys.time() < deadline) {
    process$poll_io(1000)
    lines <- c(lines, process$read_output_lines())
  }
  lines
}

test_that("the server answers tools/list and tools/call over stdio", {
  skip_on_cran()
  skip_if_not_installed("processx")

  libs <- paste(.libPaths(), collapse = .Platform$path.sep)
  server <- processx::process$new(
    file.path(R.home("bin"), "Rscript"), c("-e", server_command()),
    stdin = "|", stdout = "|", stderr = "|",
    env = c("current", R_LIBS = libs))
  on.exit(server$kill(), add = TRUE)

  send <- function(message) {
    server$write_input(paste0(jsonlite_text(message), "\n"))
  }
  send(list(jsonrpc = "2.0", id = 1, method = "initialize",
            params = list(protocolVersion = "2025-06-18",
                          capabilities = structure(list(), names = character(0)),
                          clientInfo = list(name = "dbparser.mcp-tests",
                                            version = "0"))))
  lines <- read_until(server, character(0), 1)
  send(list(jsonrpc = "2.0", method = "notifications/initialized"))
  send(list(jsonrpc = "2.0", id = 2, method = "tools/list"))
  send(list(jsonrpc = "2.0", id = 3, method = "tools/call",
            params = list(name = "list_parsers",
                          arguments = structure(list(),
                                                names = character(0)))))
  lines <- read_until(server, lines, c(2, 3))

  # filters arrive as a JSON array of objects
  hgnc <- sample_path(parser_registry()$parseHGNC$sample)
  send(list(jsonrpc = "2.0", id = 4, method = "tools/call",
            params = list(name = "parse_file",
                          arguments = list(parser = "parseHGNC",
                                           path = hgnc))))
  lines <- read_until(server, lines, 4)
  parsed <- Filter(function(m) identical(m$id, 4L),
                   lapply(lines, function(l) tryCatch(jsonlite::parse_json(l),
                                                      error = function(e) NULL)))
  object_id <- jsonlite::parse_json(
    parsed[[1]]$result$content[[1]]$text)$object_id
  send(list(jsonrpc = "2.0", id = 5, method = "tools/call",
            params = list(name = "read_table",
                          arguments = list(
                            object_id = object_id, table = "gene_name",
                            filters = list(list(column = "kind",
                                                value = list("symbol")),
                                           list(column = "name",
                                                value = list("ACTB", "F11",
                                                             "no-such")))))))
  lines <- read_until(server, lines, 5)

  # closing stdin ends the server; collect anything it wrote on the way out
  close(server$get_input_connection())
  server$wait(10000)
  lines <- c(lines, server$read_all_output_lines())
  stderr <- server$read_all_error()
  expect_false(server$is_alive())

  # every stdout line is a JSON-RPC 2.0 message
  lines <- lines[nzchar(lines)]
  expect_gt(length(lines), 0L)
  messages <- lapply(lines, function(line) {
    message <- tryCatch(jsonlite::parse_json(line), error = function(e) NULL)
    expect_false(is.null(message), info = paste("not JSON:", line))
    expect_identical(message$jsonrpc, "2.0", info = line)
    expect_true(!is.null(message$method) ||
                  (!is.null(message$id) &&
                     xor(!is.null(message$result), !is.null(message$error))),
                info = line)
    message
  })
  response <- function(id) {
    found <- Filter(function(m) identical(m$id, id), messages)
    expect_length(found, 1L)
    found[[1]]
  }

  initialized <- response(1L)
  expect_null(initialized$error)
  expect_false(is.null(initialized$result$capabilities$tools))

  listed <- response(2L)
  expect_null(listed$error, info = stderr)
  names <- vapply(listed$result$tools, function(tool) tool$name, "")
  expect_equal(names, vapply(mcp_tools(), function(tool) tool@name, ""))
  parse <- listed$result$tools[[which(names == "parse_file")]]
  expect_equal(unlist(parse$inputSchema$required), c("parser", "path"))
  export <- listed$result$tools[[which(names == "export_csv")]]
  expect_false(export$annotations$readOnlyHint)

  called <- response(3L)
  expect_null(called$error, info = stderr)
  expect_false(isTRUE(called$result$isError))
  text <- jsonlite::parse_json(called$result$content[[1]]$text)
  expect_equal(vapply(text$parsers, function(x) x$parser, ""),
               names(parser_registry()))
  expect_equal(text$dbparser_version,
               as.character(utils::packageVersion("dbparser")))

  read <- response(5L)
  expect_false(isTRUE(read$result$isError), 
               info = read$result$content[[1]]$text)
  rows <- jsonlite::parse_json(read$result$content[[1]]$text)
  expect_equal(rows$total_rows, 2L)
  expect_equal(vapply(rows$rows, function(x) x$name, ""), c("ACTB", "F11"))
  expect_equal(rows$notes[[1]], paste(
    "Filter: kind = symbol and name in (ACTB, F11, no-such).",
    "2 of 2 matching row(s) returned."))
})
