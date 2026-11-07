test_that("list_parsers covers every exported dbparser parser", {
  result   <- mcp_list_parsers()
  exported <- sort(grep("^parse", getNamespaceExports("dbparser"),
                        value = TRUE))

  expect_equal(sort(vapply(result$parsers, function(x) x$parser, "")),
               exported)
  expect_equal(result$dbparser_version,
               as.character(utils::packageVersion("dbparser")))
  expect_match(result$note, "small extracts")

  for (entry in result$parsers) {
    expect_true(all(c("parser", "source", "reads", "tables", "licence",
                      "licence_url", "since") %in% names(entry)),
                info = entry$parser)
    expect_false(is.na(entry$licence), info = entry$parser)
  }

  drugbank <- result$parsers[[which(vapply(result$parsers,
                                           function(x) x$parser, "") ==
                                      "parseDrugBank")]]
  expect_null(drugbank$sample_path)
  expect_equal(drugbank$licence, "not stated")
})

test_that("the registry's licences match what each parser records", {
  store_clear()
  on.exit(store_clear(), add = TRUE)
  registry <- parser_registry()

  for (parser in setdiff(names(registry), "parseDrugBank")) {
    entry  <- registry[[parser]]
    parsed <- suppressMessages(mcp_parse_file(
      parser, sample_path(entry$sample),
      second_path = sample_path(entry$second_sample)))
    source <- parsed$source[[1]]
    expect_identical(source$licence, entry$licence, info = parser)
    expect_identical(source$licence_url, entry$licence_url, info = parser)
    expect_true(all(vapply(parsed$tables, function(x) x$table, "") %in%
                      entry$tables), info = parser)
  }

  drugbank <- suppressMessages(mcp_parse_file(
    "parseDrugBank",
    user_copy("extdata", "drugbank_record_biotech.xml")))
  expect_identical(drugbank$source[[1]]$licence,
                   registry$parseDrugBank$licence)
  expect_identical(drugbank$source[[1]]$licence_url,
                   registry$parseDrugBank$licence_url)
})
