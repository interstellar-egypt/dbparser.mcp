test_that("export_csv writes the whole table and returns licence and attribution", {
  store_clear()
  on.exit(store_clear(), add = TRUE)
  hgnc <- suppressMessages(mcp_parse_file(
    "parseHGNC", sample_path(parser_registry()$parseHGNC$sample),
    release = "2026-10"))
  dir  <- withr_tempdir(environment())
  path <- file.path(dir, "gene.csv")

  result <- mcp_export_csv(hgnc$object_id, "gene", path)
  expect_true(file.exists(path))
  expect_equal(result$path, file.path(normalizePath(dir), "gene.csv"))
  expect_equal(result$rows, 24L)
  expect_equal(result$bytes, file.size(path))

  written <- utils::read.csv(path, colClasses = "character",
                             encoding = "UTF-8")
  gene    <- the$objects[[1]]$object$gene
  expect_equal(nrow(written), nrow(gene))
  expect_equal(names(written), names(gene))
  expect_equal(written$hgnc_id, gene$hgnc_id)
  expect_equal(result$columns, ncol(gene))

  # the licence and attribution are in the result
  expect_equal(result$source[[1]]$licence, "CC0-1.0")
  expect_match(result$licence_note, "licence CC0-1.0", fixed = TRUE)
  expect_match(result$licence_note, result$source[[1]]$attribution,
               fixed = TRUE)
  expect_match(result$licence_note, "(release 2026-10)", fixed = TRUE)
  expect_match(result$licence_note, "Keep this licence and attribution")
  # no temp file is left behind
  expect_equal(list.files(dir, all.files = TRUE, no.. = TRUE), "gene.csv")
})

test_that("export_csv writes long cells in full", {
  store_clear()
  on.exit(store_clear(), add = TRUE)
  own      <- user_copy("extdata", "drugbank_record_biotech.xml")
  drugbank <- suppressMessages(mcp_parse_file("parseDrugBank", own))
  path     <- file.path(dirname(own), "general_information.csv")

  result  <- mcp_export_csv(drugbank$object_id, "drugs$general_information",
                            path)
  # compare the file's bytes: read.csv() would drop the \r of the text's
  # CRLF line breaks
  bytes <- readBin(path, "raw", file.size(path))
  full  <- the$objects[[1]]$object$drugs$general_information$description[1]
  quoted <- charToRaw(enc2utf8(paste0("\"", gsub("\"", "\"\"", full), "\"")))
  expect_gt(length(quoted), 500L)
  expect_true(grepl(rawToChar(quoted), rawToChar(bytes), fixed = TRUE,
                    useBytes = TRUE))
  # DrugBank states no licence: the note says the user's own terms apply
  expect_match(result$licence_note, "licence not stated by dbparser")
  expect_match(result$licence_note, "drugbank-terms-of-service")
})

test_that("export_csv never overwrites a file", {
  store_clear()
  on.exit(store_clear(), add = TRUE)
  hgnc <- suppressMessages(mcp_parse_file(
    "parseHGNC", sample_path(parser_registry()$parseHGNC$sample)))
  dir  <- withr_tempdir(environment())
  path <- file.path(dir, "gene.csv")
  writeLines("keep me", path)

  error <- expect_mcp_error(mcp_export_csv(hgnc$object_id, "gene", path),
                            "already exists; export_csv never overwrites")
  expect_match(error$next_step, "new file name")
  expect_equal(readLines(path), "keep me")

  # a second export to the same new path is refused too
  other <- file.path(dir, "other.csv")
  mcp_export_csv(hgnc$object_id, "gene", other)
  before <- readLines(other)
  expect_mcp_error(mcp_export_csv(hgnc$object_id, "gene_name", other),
                   "already exists")
  expect_equal(readLines(other), before)
})

test_that("export_csv checks the path, object and table", {
  store_clear()
  on.exit(store_clear(), add = TRUE)
  hgnc <- suppressMessages(mcp_parse_file(
    "parseHGNC", sample_path(parser_registry()$parseHGNC$sample)))
  dir  <- withr_tempdir(environment())
  id   <- hgnc$object_id

  expect_mcp_error(mcp_export_csv(id, "gene", "https://example.org/x.csv"),
                   "is a URL")
  expect_mcp_error(mcp_export_csv(id, "gene", file.path(dir, "gene.txt")),
                   "must end in .csv")
  expect_mcp_error(mcp_export_csv(id, "gene",
                                  file.path(dir, "no_such", "gene.csv")),
                   "does not exist")
  expect_mcp_error(mcp_export_csv(id, "gene", ""), "must be one file path")
  expect_mcp_error(mcp_export_csv(id, "genes", file.path(dir, "g.csv")),
                   "Unknown table `genes`")
  expect_mcp_error(mcp_export_csv("hgnc_9", "gene", file.path(dir, "g.csv")),
                   "No parsed object `hgnc_9`")
  expect_equal(list.files(dir, all.files = TRUE, no.. = TRUE), character(0))
})
