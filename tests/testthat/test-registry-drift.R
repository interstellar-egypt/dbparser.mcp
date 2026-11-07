# When dbparser gains (or drops) a parse* function, these tests fail until
# parser_registry() (R/registry.R) and run_parser() (R/impl.R) follow.

registry_drift <- function(exported, registered) {
  parsers <- grep("^parse", exported, value = TRUE)
  list(unlisted = sort(setdiff(parsers, registered)),
       gone     = sort(setdiff(registered, parsers)))
}

drift_message <- function(drift) {
  paste0(
    if (length(drift$unlisted)) {
      paste0("dbparser exports parsers the registry does not list: ",
             paste(drift$unlisted, collapse = ", "),
             ". Add each to parser_registry() in R/registry.R (source, reads, ",
             "input, tables, licence, licence_url, since, sample) and to ",
             "run_parser() in R/impl.R. ")
    },
    if (length(drift$gone)) {
      paste0("The registry lists parsers dbparser no longer exports: ",
             paste(drift$gone, collapse = ", "), ". Remove them.")
    })
}

switch_arms <- function(fun) {
  find <- function(expr) {
    if (is.call(expr)) {
      if (identical(expr[[1]], as.name("switch"))) {
        return(setdiff(names(as.list(expr)), ""))
      }
      for (part in as.list(expr)) {
        found <- find(part)
        if (length(found)) return(found)
      }
    }
    character(0)
  }
  find(body(fun))
}

test_that("the registry lists exactly the parse* functions dbparser exports", {
  drift <- registry_drift(getNamespaceExports("dbparser"),
                          names(parser_registry()))
  expect(length(drift$unlisted) == 0L && length(drift$gone) == 0L,
         drift_message(drift))
})

test_that("run_parser has a branch for every registry entry", {
  expect_setequal(switch_arms(run_parser), names(parser_registry()))
})

test_that("the drift check catches a new dbparser parser", {
  # a future dbparser that exports parseChEMBL: the check must fail and say how
  exported <- c(getNamespaceExports("dbparser"), "parseChEMBL")
  drift    <- registry_drift(exported, names(parser_registry()))
  expect_equal(drift$unlisted, "parseChEMBL")
  expect_match(drift_message(drift), "parseChEMBL. Add each to parser_registry")

  # and one that drops a parser
  drift <- registry_drift(setdiff(getNamespaceExports("dbparser"), "parseHPO"),
                          names(parser_registry()))
  expect_equal(drift$gone, "parseHPO")
  expect_match(drift_message(drift), "no longer exports: parseHPO")
})
