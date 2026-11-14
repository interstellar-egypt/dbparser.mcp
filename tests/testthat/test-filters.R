# filters on read_table and export_csv, on the sample hgnc_complete_set.txt
sample_hgnc <- function() {
  suppressMessages(mcp_parse_file(
    "parseHGNC", sample_path(parser_registry()$parseHGNC$sample),
    release = "2026-10"))
}

test_that("read_table filters on exact equality and counts matching rows", {
  store_clear()
  on.exit(store_clear(), add = TRUE)
  hgnc  <- sample_hgnc()
  names <- the$objects[[1]]$object$gene_name
  first <- names[1, ]

  read <- mcp_read_table(hgnc$object_id, "gene_name",
                         filters = list(list(column = "hgnc_id",
                                             value = first$hgnc_id)))
  expected <- names[names$hgnc_id == first$hgnc_id, ]
  expect_equal(read$total_rows, nrow(expected))
  expect_equal(read$returned_rows, nrow(expected))
  expect_equal(vapply(read$rows, function(x) x$name, ""), expected$name)
  expect_equal(read$filters, list(list(column = "hgnc_id",
                                       value = first$hgnc_id)))
  expect_equal(read$notes[[1]], sprintf(
    "Filter: hgnc_id = %s. %d of %d matching row(s) returned.",
    first$hgnc_id, nrow(expected), nrow(expected)))
  expect_equal(read$source[[1]]$licence, "CC0-1.0")

  # AND: the symbol row of that gene only
  both <- mcp_read_table(hgnc$object_id, "gene_name", filters = list(
    list(column = "hgnc_id", value = first$hgnc_id),
    list(column = "kind", value = "symbol")))
  expect_equal(both$total_rows, 1L)
  expect_equal(both$rows[[1]]$name, first$name)

  # exact: no partial, no case-folding match
  partial <- mcp_read_table(hgnc$object_id, "gene_name", filters = list(
    list(column = "name", value = substr(first$name, 1, 2))))
  expect_false(any(vapply(partial$rows, function(x) x$name, "") == first$name))
  upper <- mcp_read_table(hgnc$object_id, "gene_name", filters = list(
    list(column = "name", value = tolower(first$name))))
  expect_equal(upper$total_rows,
               sum(names$name == tolower(first$name), na.rm = TRUE))

  none <- mcp_read_table(hgnc$object_id, "gene_name", filters = list(
    list(column = "name", value = "no-such-gene-name")))
  expect_equal(none$total_rows, 0L)
  expect_equal(none$rows, list())
  expect_match(none$notes[[1]], "0 of 0 matching row(s) returned",
               fixed = TRUE)
  expect_silent(jsonlite::parse_json(jsonlite_text(none)))

  # an empty list means no filter
  all <- mcp_read_table(hgnc$object_id, "gene_name", filters = list())
  expect_equal(all$total_rows, nrow(names))
})

test_that("read_table pages within the matching rows", {
  store_clear()
  on.exit(store_clear(), add = TRUE)
  hgnc  <- sample_hgnc()
  names <- the$objects[[1]]$object$gene_name
  filters <- list(list(column = "kind", value = "symbol"))
  symbols <- names$name[names$kind == "symbol"]
  expect_gt(length(symbols), 3L)

  page <- mcp_read_table(hgnc$object_id, "gene_name", n = 2, offset = 1,
                         filters = filters)
  expect_equal(page$total_rows, length(symbols))
  expect_equal(page$returned_rows, 2L)
  expect_equal(vapply(page$rows, function(x) x$name, ""), symbols[2:3])
  expect_match(page$notes[[1]], sprintf("2 of %d matching", length(symbols)))
  expect_match(page$notes[[2]], sprintf("Rows 2-3 of %d", length(symbols)))
})

test_that("filters compare non-text columns as their text", {
  store_clear()
  on.exit(store_clear(), add = TRUE)
  hgnc <- sample_hgnc()
  gene <- the$objects[[1]]$object$gene
  date <- as.character(gene$date_modified[1])

  read <- mcp_read_table(hgnc$object_id, "gene", filters = list(
    list(column = "date_modified", value = date)))
  expect_equal(read$total_rows,
               sum(as.character(gene$date_modified) == date, na.rm = TRUE))
  expect_gt(read$total_rows, 0L)
})

test_that("an unknown or malformed filter is an error listing the columns", {
  store_clear()
  on.exit(store_clear(), add = TRUE)
  hgnc <- sample_hgnc()

  wrong <- expect_mcp_error(
    mcp_read_table(hgnc$object_id, "gene_name",
                   filters = list(list(column = "Name", value = "x"))),
    "Unknown column `Name`. Valid: hgnc_id, kind, name.")
  expect_match(wrong$next_step, "describe")
  expect_mcp_error(
    mcp_read_table(hgnc$object_id, "gene_name",
                   filters = list(list(col = "name", value = "x"))),
    "exactly `column` and `value`")
  expect_mcp_error(
    mcp_read_table(hgnc$object_id, "gene_name",
                   filters = list(list(column = "name",
                                       value = list(list("a", "b"))))),
    "single texts or numbers")
  expect_mcp_error(
    mcp_read_table(hgnc$object_id, "gene_name", filters = "name=x"),
    "list of \\{column, value\\}")
})

test_that("export_csv writes only the matching rows", {
  store_clear()
  on.exit(store_clear(), add = TRUE)
  hgnc  <- sample_hgnc()
  names <- the$objects[[1]]$object$gene_name
  dir   <- withr_tempdir(environment())
  path  <- file.path(dir, "symbols.csv")
  filters <- list(list(column = "kind", value = "symbol"))

  result <- mcp_export_csv(hgnc$object_id, "gene_name", path,
                           filters = filters)
  written <- utils::read.csv(path, colClasses = "character",
                             encoding = "UTF-8")
  expect_equal(written$name, names$name[names$kind == "symbol"])
  expect_equal(result$rows, nrow(written))
  expect_equal(result$filters, filters)
  expect_match(result$licence_note, "licence CC0-1.0", fixed = TRUE)

  # a bad column fails before any file is written
  bad <- file.path(dir, "bad.csv")
  expect_mcp_error(
    mcp_export_csv(hgnc$object_id, "gene_name", bad,
                   filters = list(list(column = "symbol", value = "x"))),
    "Unknown column `symbol`")
  expect_false(file.exists(bad))
})

test_that("a value list matches any of its values, AND across columns", {
  store_clear()
  on.exit(store_clear(), add = TRUE)
  hgnc  <- sample_hgnc()
  names <- the$objects[[1]]$object$gene_name
  picks <- names$name[names$kind == "symbol"][1:3]

  # as JSON gives it: a list of values
  read <- mcp_read_table(hgnc$object_id, "gene_name", filters = list(
    list(column = "name", value = as.list(picks))))
  expected <- names[names$name %in% picks, ]
  expect_equal(read$total_rows, nrow(expected))
  expect_setequal(vapply(read$rows, function(x) x$name, ""), picks)
  expect_equal(read$notes[[1]], sprintf(
    "Filter: name in (%s). %d of %d matching row(s) returned.",
    paste(picks, collapse = ", "), nrow(expected), nrow(expected)))

  # a character vector from R works the same
  same <- mcp_read_table(hgnc$object_id, "gene_name",
                         filters = list(list(column = "name", value = picks)))
  expect_equal(same$rows, read$rows)

  # list + a second column: AND
  both <- mcp_read_table(hgnc$object_id, "gene_name", filters = list(
    list(column = "name", value = as.list(picks)),
    list(column = "kind", value = "symbol")))
  expect_equal(both$total_rows, 3L)
  expect_match(both$notes[[1]], paste0(
    "Filter: name in (", paste(picks, collapse = ", "), ") and kind = symbol.",
    " 3 of 3"), fixed = TRUE)

  # case-sensitive, no partial match inside a list either
  case <- mcp_read_table(hgnc$object_id, "gene_name", filters = list(
    list(column = "name", value = list(tolower(picks[1]),
                                       substr(picks[2], 1, 2)))))
  expect_equal(case$total_rows,
               sum(names$name %in% c(tolower(picks[1]), substr(picks[2], 1, 2))))
})

test_that("an empty or over-long value list is an error with a next step", {
  store_clear()
  on.exit(store_clear(), add = TRUE)
  hgnc <- sample_hgnc()

  empty <- expect_mcp_error(
    mcp_read_table(hgnc$object_id, "gene_name",
                   filters = list(list(column = "name", value = list()))),
    "value list for column `name` is empty")
  expect_match(empty$next_step, "at least one value")

  long <- expect_mcp_error(
    mcp_read_table(hgnc$object_id, "gene_name", filters = list(
      list(column = "name", value = as.list(sprintf("G%d", 1:101))))),
    "has 101 values; a filter takes at most 100")
  expect_match(long$next_step, "several calls")

  # exactly 100 is fine
  ok <- mcp_read_table(hgnc$object_id, "gene_name", filters = list(
    list(column = "name", value = as.list(sprintf("G%d", 1:100)))))
  expect_equal(ok$total_rows, 0L)

  expect_mcp_error(
    mcp_read_table(hgnc$object_id, "gene_name",
                   filters = list(list(column = "name", value = list("a", NA)))),
    "missing \\(null\\) value")
})

test_that("export_csv takes value lists and states the filter", {
  store_clear()
  on.exit(store_clear(), add = TRUE)
  hgnc  <- sample_hgnc()
  gene  <- the$objects[[1]]$object$gene
  picks <- gene$symbol[c(2, 5, 7)]
  dir   <- withr_tempdir(environment())
  path  <- file.path(dir, "three.csv")

  result <- mcp_export_csv(hgnc$object_id, "gene", path, filters = list(
    list(column = "symbol", value = as.list(picks))))
  written <- utils::read.csv(path, colClasses = "character",
                             encoding = "UTF-8")
  expect_equal(written$symbol, gene$symbol[gene$symbol %in% picks])
  expect_equal(ncol(written), ncol(gene))
  expect_equal(result$rows, 3L)
  expect_equal(result$note, sprintf(
    "Filter: symbol in (%s). 3 matching row(s) written.",
    paste(picks, collapse = ", ")))

  for (bad in list(list(), as.list(sprintf("G%d", 1:101)))) {
    out <- file.path(dir, "bad.csv")
    expect_mcp_error(
      mcp_export_csv(hgnc$object_id, "gene", out,
                     filters = list(list(column = "symbol", value = bad))),
      "value list for column `symbol`")
    expect_false(file.exists(out))
  }
})
