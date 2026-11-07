test_that("describe lists tables, columns and the full source metadata", {
  store_clear()
  on.exit(store_clear(), add = TRUE)
  registry <- parser_registry()
  dpd      <- suppressMessages(mcp_parse_file(
    "parseHealthCanadaDPD", sample_path(registry$parseHealthCanadaDPD$sample),
    release = "2026-10"))

  described <- mcp_describe(dpd$object_id)
  expect_equal(described$object_id, dpd$object_id)
  expect_equal(described$parser, "parseHealthCanadaDPD")
  expect_equal(vapply(described$tables, function(x) x$table, ""),
               registry$parseHealthCanadaDPD$tables)

  status <- described$tables[[which(vapply(described$tables,
                                           function(x) x$table, "") ==
                                      "status")]]
  types  <- stats::setNames(vapply(status$columns, function(x) x$type, ""),
                            vapply(status$columns, function(x) x$name, ""))
  expect_equal(types[["drug_code"]], "integer")
  expect_equal(types[["history_date"]], "Date")
  expect_equal(types[["status"]], "character")

  source <- described$source[[1]]
  expect_equal(source$licence, "OGL-Canada-2.0")
  expect_match(source$attribution, "Open Government Licence")
  expect_match(source$parser, "^dbparser ")
  expect_gt(length(source$files), 0L)
  expect_equal(names(source$files[[1]]), c("name", "sha256"))
})

test_that("describe names the parsed objects when the id is unknown", {
  store_clear()
  on.exit(store_clear(), add = TRUE)
  expect_mcp_error(mcp_describe("hgnc_1"), "Nothing has been parsed yet")

  suppressMessages(mcp_parse_file(
    "parseHGNC", sample_path(parser_registry()$parseHGNC$sample)))
  error <- expect_mcp_error(mcp_describe("hgnc_9"), "Parsed objects: hgnc_1")
  expect_match(error$next_step, "parse_file")
})
