source_fields <- c("source", "source_name", "release", "release_date",
                   "retrieved_at", "licence", "licence_url", "attribution")

test_that("parse_file parses the bundled samples of the open-data parsers", {
  store_clear()
  on.exit(store_clear(), add = TRUE)
  registry <- parser_registry()

  hgnc <- suppressMessages(mcp_parse_file(
    "parseHGNC", sample_path(registry$parseHGNC$sample),
    second_path  = sample_path(registry$parseHGNC$second_sample),
    release      = "2026-10",
    retrieved_at = "2026-10-16"))

  expect_equal(hgnc$object_id, "hgnc_1")
  expect_true(hgnc$sample)
  expect_equal(hgnc$files, list("hgnc_complete_set.txt", "withdrawn.txt"))
  expect_equal(vapply(hgnc$tables, function(x) x$table, ""),
               c("gene", "gene_name", "withdrawn"))
  expect_equal(hgnc$tables[[1]]$rows, 24L)
  expect_type(hgnc$memory_mb, "double")
  expect_equal(hgnc$session_memory_mb, hgnc$memory_mb)

  # every answer carries the source block
  expect_equal(names(hgnc$source[[1]]), source_fields)
  expect_equal(hgnc$source[[1]]$release, "2026-10")
  expect_equal(hgnc$source[[1]]$retrieved_at, "2026-10-16")
  expect_equal(hgnc$source[[1]]$licence, "CC0-1.0")

  # zipped samples of the CSV parsers are read from a temp folder
  onsides  <- suppressMessages(mcp_parse_file(
    "parseOnSIDES", sample_path(registry$parseOnSIDES$sample)))
  twosides <- suppressMessages(mcp_parse_file(
    "parseTWOSIDES", sample_path(registry$parseTWOSIDES$sample)))
  expect_equal(length(onsides$tables), 8L)
  expect_equal(twosides$tables[[1]]$table, "drug_drug_interactions")
  expect_equal(onsides$object_id, "onsides_2")
})

test_that("parse_file records retrieved_at for the older parsers too", {
  store_clear()
  on.exit(store_clear(), add = TRUE)
  hpo <- suppressMessages(mcp_parse_file(
    "parseHPO", sample_path(parser_registry()$parseHPO$sample),
    release = "2015", release_date = "2015-07-01",
    retrieved_at = "2026-11-07"))
  expect_equal(hpo$source[[1]]$release, "2015")
  expect_equal(hpo$source[[1]]$release_date, "2015-07-01")
  expect_equal(hpo$source[[1]]$retrieved_at, "2026-11-07")
})

test_that("parse_file tells the openFDA files apart by openFDA's names", {
  store_clear()
  on.exit(store_clear(), add = TRUE)
  registry <- parser_registry()
  ndc      <- sample_path(registry$parseOpenFDA$second_sample)

  only_ndc <- suppressMessages(mcp_parse_file("parseOpenFDA", ndc))
  expect_true(all(startsWith(vapply(only_ndc$tables, function(x) x$table,
                                    ""), "ndc_")))

  renamed <- file.path(tempdir(), "openfda_download.json.zip")
  file.copy(ndc, renamed, overwrite = TRUE)
  on.exit(unlink(renamed), add = TRUE)
  expect_mcp_error(mcp_parse_file("parseOpenFDA", renamed),
                   "Can't tell the openFDA files apart")
})

test_that("BR-1: DrugBank is parsed only from the user's own file", {
  store_clear()
  on.exit(store_clear(), add = TRUE)

  error <- expect_mcp_error(
    mcp_parse_file("parseDrugBank",
                   system.file("extdata", "drugbank_record_biotech.xml",
                               package = "dbparser")),
    "only from your own DrugBank download")
  expect_match(error$next_step, "your own DrugBank")
  expect_equal(length(the$objects), 0L)

  own <- user_copy("extdata", "drugbank_record_biotech.xml")
  drugbank <- suppressMessages(mcp_parse_file("parseDrugBank", own))
  expect_false(drugbank$sample)
  expect_equal(drugbank$source[[1]]$source, "drugbank")
  expect_equal(drugbank$tables[[1]]$table, "drugs$general_information")

  all_sections <- suppressMessages(mcp_parse_file(
    "parseDrugBank", own, drugbank_sections = "all"))
  expect_gt(length(all_sections$tables), length(drugbank$tables))
})

test_that("parse_file never reads a URL", {
  for (url in c("https://example.org/TWOSIDES.csv.gz",
                "http://example.org/x.txt", "ftp://example.org/x.zip")) {
    expect_mcp_error(mcp_parse_file("parseTWOSIDES", url),
                     "is a URL; this server reads only local files")
  }
  expect_mcp_error(
    mcp_parse_file("parseHGNC", sample_path(parser_registry()$parseHGNC$sample),
                   second_path = "https://example.org/withdrawn.txt"),
    "`second_path` is a URL")
})

test_that("parse_file checks names and inputs exactly, with a next step", {
  hgnc <- sample_path(parser_registry()$parseHGNC$sample)

  error <- expect_mcp_error(mcp_parse_file("parseHgnc", hgnc),
                            "Unknown parser `parseHgnc`")
  expect_match(conditionMessage(error), "parseHGNC")
  expect_match(error$next_step, "list_parsers")

  expect_mcp_error(mcp_parse_file("parseHGNC", "/no/such/file.txt"),
                   "Nothing found")
  expect_mcp_error(mcp_parse_file("parseHGNC", dirname(hgnc)),
                   "is a folder")
  expect_mcp_error(mcp_parse_file("parseHealthCanadaDPD", hgnc),
                   "is a file")
  expect_mcp_error(mcp_parse_file("parseHPO",
                                  sample_path(parser_registry()$parseHPO$sample),
                                  second_path = hgnc),
                   "`second_path` is used only by")
  expect_mcp_error(mcp_parse_file("parseHGNC", hgnc, release_date = "07-11-2026"),
                   "YYYY-MM-DD")
  expect_mcp_error(mcp_parse_file("parseHGNC", hgnc,
                                  drugbank_sections = "everything"),
                   "Unknown drugbank_sections")
})

test_that("parse_file refuses a new parse once the session is over the limit", {
  store_clear()
  on.exit(store_clear(), add = TRUE)
  old <- Sys.getenv("DBPARSER_MCP_MAX_MB", unset = NA)
  on.exit(if (is.na(old)) Sys.unsetenv("DBPARSER_MCP_MAX_MB")
          else Sys.setenv(DBPARSER_MCP_MAX_MB = old), add = TRUE)
  hgnc <- sample_path(parser_registry()$parseHGNC$sample)

  Sys.setenv(DBPARSER_MCP_MAX_MB = "0")
  # nothing loaded yet: the first parse goes ahead
  first <- suppressMessages(mcp_parse_file("parseHGNC", hgnc))
  expect_gt(first$memory_mb, 0)
  # now over the limit: refused, with what to do
  error <- expect_mcp_error(mcp_parse_file("parseHGNC", hgnc),
                            "over the 0 MB limit")
  expect_match(error$next_step, "Start a new session")
})
