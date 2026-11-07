test_that("list_objects lists what this session has parsed", {
  store_clear()
  on.exit(store_clear(), add = TRUE)

  empty <- mcp_list_objects()
  expect_equal(empty$objects, list())
  expect_equal(empty$session_memory_mb, 0)
  expect_match(empty$note, "call parse_file first")

  registry <- parser_registry()
  hgnc <- suppressMessages(mcp_parse_file(
    "parseHGNC", sample_path(registry$parseHGNC$sample), release = "2026-10"))
  hpo  <- suppressMessages(mcp_parse_file(
    "parseHPO", sample_path(registry$parseHPO$sample)))

  listed <- mcp_list_objects()
  expect_equal(vapply(listed$objects, function(x) x$object_id, ""),
               c(hgnc$object_id, hpo$object_id))
  first <- listed$objects[[1]]
  expect_equal(first$parser, "parseHGNC")
  expect_equal(first$files, list("hgnc_complete_set.txt"))
  expect_equal(first$tables, hgnc$tables)
  expect_equal(first$source[[1]]$release, "2026-10")
  expect_equal(first$source[[1]]$licence, "CC0-1.0")
  expect_equal(listed$session_memory_mb, store_total_mb())
  expect_equal(listed$memory_limit_mb, memory_limit_mb())
  # its result goes over the wire as JSON
  expect_silent(jsonlite::parse_json(jsonlite_text(listed)))
})

test_that("read_table returns 20 rows by default, with returned vs total", {
  store_clear()
  on.exit(store_clear(), add = TRUE)
  hgnc <- suppressMessages(mcp_parse_file(
    "parseHGNC", sample_path(parser_registry()$parseHGNC$sample)))

  read <- mcp_read_table(hgnc$object_id, "gene")
  expect_equal(read$returned_rows, 20L)
  expect_equal(read$total_rows, 24L)
  expect_equal(length(read$rows), 20L)
  expect_equal(read$offset, 0L)
  expect_match(read$notes[[1]], "Rows 1-20 of 24")
  expect_equal(names(read$rows[[1]]),
               vapply(read$columns, function(x) x$name, ""))
  expect_equal(read$rows[[1]]$hgnc_id, the$objects[[1]]$object$gene$hgnc_id[1])
  expect_equal(read$source[[1]]$licence, "CC0-1.0")

  # paging, and the last page says nothing is left out
  rest <- mcp_read_table(hgnc$object_id, "gene", n = 10, offset = 20)
  expect_equal(rest$returned_rows, 4L)
  expect_equal(rest$total_rows, 24L)
  expect_equal(rest$rows[[1]]$hgnc_id,
               the$objects[[1]]$object$gene$hgnc_id[21])
  expect_equal(rest$notes, list())

  past <- mcp_read_table(hgnc$object_id, "gene", offset = 24)
  expect_equal(past$returned_rows, 0L)
  expect_equal(past$rows, list())
  expect_match(past$notes[[1]], "past the end")
})

test_that("read_table returns at most 100 rows and says so", {
  store_clear()
  on.exit(store_clear(), add = TRUE)
  twosides <- suppressMessages(mcp_parse_file(
    "parseTWOSIDES", sample_path(parser_registry()$parseTWOSIDES$sample)))
  total <- twosides$tables[[1]]$rows
  skip_if(total <= 100L, "TWOSIDES sample has 100 rows or fewer")

  read <- mcp_read_table(twosides$object_id, "drug_drug_interactions",
                         n = 5000)
  expect_equal(read$returned_rows, 100L)
  expect_equal(length(read$rows), 100L)
  expect_equal(read$total_rows, total)
  expect_match(read$notes[[1]], "n is capped at 100")
  expect_match(read$notes[[2]], sprintf("Rows 1-100 of %d", total))

  # near the end of a table the cap note claims no row count
  tail <- mcp_read_table(twosides$object_id, "drug_drug_interactions",
                         n = 1000, offset = total - 3)
  expect_equal(tail$returned_rows, 3L)
  expect_equal(tail$notes, list("n is capped at 100 rows a call."))
})

test_that("read_table cuts long cells and names their columns", {
  store_clear()
  on.exit(store_clear(), add = TRUE)
  own      <- user_copy("extdata", "drugbank_record_biotech.xml")
  drugbank <- suppressMessages(mcp_parse_file("parseDrugBank", own))
  full     <- the$objects[[1]]$object$drugs$general_information$description[1]
  expect_gt(nchar(full), 500L)

  read <- mcp_read_table(drugbank$object_id, "drugs$general_information")
  cut  <- read$rows[[1]]$description
  expect_equal(nchar(cut), 503L)
  expect_true(endsWith(cut, "..."))
  expect_equal(substr(cut, 1L, 500L), substr(full, 1L, 500L))
  expect_match(read$notes[[1]], "1 cell\\(s\\) longer than 500 characters")
  expect_match(read$notes[[1]], "columns: description")
  expect_match(read$notes[[1]], "export_csv writes them in full")
  # short cells are untouched
  expect_equal(read$rows[[1]]$name,
               the$objects[[1]]$object$drugs$general_information$name[1])
})

test_that("read_table turns NA, dates and list columns into JSON values", {
  data  <- data.frame(id = 1:3, when = as.Date(c("2026-01-02", NA, "2026-03-04")),
                      score = c(1.5, NaN, Inf), kind = factor(c("a", "b", NA)))
  data$tags <- list(c("x", "y"), character(0), NA)
  cells <- json_cells(data)
  rows  <- cells_as_records(cells, 3L)

  expect_equal(rows[[1]], list(id = 1L, when = "2026-01-02", score = 1.5,
                               kind = "a", tags = "x; y"))
  expect_null(rows[[2]]$when)
  expect_null(rows[[2]]$score)
  expect_null(rows[[3]]$score)
  expect_null(rows[[2]]$tags)
  expect_null(rows[[3]]$kind)
  json <- jsonlite::parse_json(jsonlite_text(rows))
  expect_null(json[[2]]$when)
})

test_that("read_table checks the object, table and counts exactly", {
  store_clear()
  on.exit(store_clear(), add = TRUE)
  expect_mcp_error(mcp_read_table("hgnc_1", "gene"),
                   "Nothing has been parsed yet")

  hgnc <- suppressMessages(mcp_parse_file(
    "parseHGNC", sample_path(parser_registry()$parseHGNC$sample)))
  error <- expect_mcp_error(mcp_read_table(hgnc$object_id, "genes"),
                            "Unknown table `genes`. Valid: gene, gene_name")
  expect_match(error$next_step, "describe\\(\"hgnc_1\"\\)")
  expect_mcp_error(mcp_read_table(hgnc$object_id, "Gene"), "Unknown table")
  expect_mcp_error(mcp_read_table(hgnc$object_id, "gene", n = 0),
                   "`n` must be a whole number, at least 1")
  expect_mcp_error(mcp_read_table(hgnc$object_id, "gene", n = 2.5),
                   "`n` must be a whole number")
  expect_mcp_error(mcp_read_table(hgnc$object_id, "gene", offset = -1),
                   "`offset` must be a whole number, at least 0")
})
