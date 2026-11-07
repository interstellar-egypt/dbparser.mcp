test_that("the tools have MCP-valid names, descriptions and annotations", {
  tools <- mcp_tools()
  names <- vapply(tools, function(tool) tool@name, "")

  expect_equal(names, c("list_parsers", "parse_file", "describe",
                         "list_objects", "read_table", "export_csv"))
  expect_true(all(grepl("^[A-Za-z0-9_.-]{1,128}$", names)))
  expect_false(anyDuplicated(names) > 0)

  for (tool in tools) {
    expect_true(nchar(tool@description) > 40, info = tool@name)
    expect_false(isTRUE(tool@annotations$open_world_hint), info = tool@name)
    expect_true(nzchar(tool@annotations$title), info = tool@name)
    # export_csv is the only tool that writes, and it never overwrites
    expect_identical(isTRUE(tool@annotations$read_only_hint),
                     tool@name != "export_csv", info = tool@name)
  }
  export <- tools[[6]]
  expect_false(export@annotations$destructive_hint)
  expect_match(export@description, "never overwrites")
  expect_match(export@description, "licence and attribution")
  expect_match(tools[[2]]@description, "small extracts")
  expect_match(tools[[2]]@description, "300 MB")
  expect_match(tools[[5]]@description, "at most 100")
})

test_that("the tool arguments map to the implementations", {
  tools <- mcp_tools()
  parse <- tools[[2]]

  expect_equal(names(parse@arguments@properties),
               names(formals(mcp_parse_file)))
  required <- vapply(parse@arguments@properties, function(x) x@required,
                     logical(1))
  expect_equal(names(required)[required], c("parser", "path"))
  expect_equal(parse@arguments@properties$parser@values,
               names(parser_registry()))
  expect_equal(length(tools[[1]]@arguments@properties), 0L)
  expect_equal(names(tools[[3]]@arguments@properties), "object_id")
  expect_equal(length(tools[[4]]@arguments@properties), 0L)

  # every tool's arguments are its implementation's formals, in order
  impls <- list(list_parsers = mcp_list_parsers, parse_file = mcp_parse_file,
                describe = mcp_describe, list_objects = mcp_list_objects,
                read_table = mcp_read_table, export_csv = mcp_export_csv)
  for (tool in tools) {
    expect_equal(names(tool@arguments@properties),
                 names(formals(impls[[tool@name]])), info = tool@name)
  }
  read <- vapply(tools[[5]]@arguments@properties, function(x) x@required,
                 logical(1))
  expect_equal(names(read)[read], c("object_id", "table"))
})

test_that("a failing tool returns an error result with error and next_step", {
  store_clear()
  on.exit(store_clear(), add = TRUE)
  result <- mcp_tools()[[3]](object_id = "missing_1")

  expect_true(inherits(result, "ellmer::ContentToolResult"))
  parsed <- jsonlite::fromJSON(result@error)
  expect_match(parsed$error, "No parsed object `missing_1`")
  expect_match(parsed$next_step, "parse_file")
})

test_that("a tool's printed output goes to stderr, not stdout", {
  noisy  <- as_server_tool(function(x) {
    print("noise")
    cat("more noise\n")
    list(value = x)
  })
  stdout <- utils::capture.output(result <- noisy(x = 1), type = "output")
  expect_equal(stdout, character(0))
  expect_equal(result, list(value = 1))
})
