#' @srrstats {G5.0} *Where applicable or practicable, tests should use standard data sets with known properties (for example, the [NIST Standard Reference Datasets](https://www.itl.nist.gov/div898/strd/), or data sets provided by other widely-used R packages).*
#' @srrstats {G5.1} *Data sets created within, and used to test, a package should be exported (or otherwise made generally available) so that users can confirm tests and run examples.*
#' @srrstats {G5.2} *Appropriate error and warning behaviour of all functions should be explicitly demonstrated through tests. In particular,*
#' @srrstats {G5.2a} *Every message produced within R code by `stop()`, `warning()`, `message()`, or equivalent should be unique*
#' @srrstats {G5.2b} *Explicit tests should demonstrate conditions which trigger every one of those messages, and should compare the result with expected values.*


test_that("assess formula manipulation", {
  res <-
    update_formula_and_data(
      formula = Y ~ X + amp_acro(time, n_components = 2, group = "X", period = c(12, 10)),
      data = vitamind
    )
  expect_true(all.equal(res$newformula, Y ~ X + X:main_rrr1 + X:main_sss1 + X:main_rrr2 + X:main_sss2))
})

test_that("warning for missing data", {
  data(vitamind)
  vitamind$Y[1] <- NA
  res <- function() {
    update_formula_and_data(
      formula = Y ~ X + amp_acro(time, n_components = 2, group = "X", period = c(12, 10)),
      data = vitamind, quietly = FALSE
    )
  }
  suppressMessages(expect_message(res()))
})
