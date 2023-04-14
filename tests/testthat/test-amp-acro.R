#' @srrstats {G5.0} *Where applicable or practicable, tests should use standard data sets with known properties (for example, the [NIST Standard Reference Datasets](https://www.itl.nist.gov/div898/strd/), or data sets provided by other widely-used R packages).*
#' @srrstats {G5.1} *Data sets created within, and used to test, a package should be exported (or otherwise made generally available) so that users can confirm tests and run examples.*
#' @srrstats {G5.2} *Appropriate error and warning behaviour of all functions should be explicitly demonstrated through tests. In particular,*
#' @srrstats {G5.2a} *Every message produced within R code by `stop()`, `warning()`, `message()`, or equivalent should be unique*
#' @srrstats {G5.2b} *Explicit tests should demonstrate conditions which trigger every one of those messages, and should compare the result with expected values.*

test_that("multiplication works", {
  # test 1
  amp.acro(
    time_col = time, n_components = 2, group = "X", period = c(24, 12), .data = vitamind,
    .formula = Y ~ X + amp.acro(time, n_components = 2, group = "X", period = c(24, 12))
  )

  f <- Y ~ X + amp.acro(time, n_components = 2, group = c(NA, "X"))

  cosinor.glmm(Y ~ X + amp.acro(time, n_components = 2, group = c(NA, "X"), period = c(12, 24)), data = vitamind)
  expect(ok = TRUE, "unsuccessful multiplication of group to match n_components")
})

test_that("bad inputs return useful errors", {
  # test 2
  f <- function() {
    amp.acro(
      time_col = time, n_components = 1.1, group = "X", period = 12,
      .data = vitamind,
      .formula = Y ~ X + amp.acro(time, n_components = 1.1, group = "X", period = 12)
    )
  }
  expect_error( # non-count n_components
    f(),
    regexp = "assertthat::is.count(n_components) is not TRUE", fixed = TRUE
  )

  # test 3
  f <- function() {
    amp.acro(
      time_col = time, n_components = 1, group = "X", period = -12,
      .data = vitamind,
      .formula = Y ~ X + amp.acro(time, n_components = 1, group = "X", period = -12)
    )
  }
  expect_error(f(), regexp = "period > 0") # negative period

  # test 4
  data(vitamind)
  vitamind_mod <- vitamind
  vitamind_mod$time <- as.factor(rbinom(length(vitamind$time), 1, 0.5))
  f <- function() {
    amp.acro(
      time_col = time, n_components = 1, group = "X", period = 12,
      .data = vitamind_mod,
      .formula = Y ~ X + amp.acro(time, n_components = 1, group = "X", period = 12)
    )
  }
  expect_error(f(), regexp = "ttt is not a numeric or integer vector")

  # test 5
  data(vitamind)
  vitamind_2 <- vitamind
  vitamind_2$time <- cbind(vitamind_2$time, vitamind_2$time) # two time columns
  f <- function() {
    amp.acro(
      time_col = time, n_components = 1, group = "X", period = 12,
      .data = vitamind_2,
      .formula = Y ~ X + amp.acro(time, n_components = 1, group = "X", period = 12)
    )
  }
  expect_error(f(), regexp = "time_col must be univariate")

  # test 6
  data(vitamind)
  f <- function() {
    amp.acro(
      time_col = "time", n_components = 1, group = "X", period = 12,
      .data = vitamind,
      .formula = Y ~ X + amp.acro(time, n_components = 1, group = "X", period = 12)
    )
  }
  expect_error(f(), regexp = "time_col argument must not be a string")

  # test 7
  data(vitamind)
  f <- function() {
    amp.acro(
      time_col = time_values, n_components = 1, group = "X", period = 12,
      .data = vitamind,
      .formula = Y ~ X + amp.acro(time, n_components = 1, group = "X", period = 12)
    )
  }
  expect_error(f(), regexp = "time_col must be the name of a column in dataframe")

  # test 8
  data(vitamind)
  vitamind_mod <- as.list(vitamind)
  f <- function() {
    amp.acro(
      time_col = time, n_components = 1, group = "X", period = 12,
      .data = vitamind_mod,
      .formula = Y ~ X + amp.acro(time, n_components = 1, group = "X", period = 12)
    )
  }
  expect_error(f(), regexp = "'data' must be of class 'data.frame', 'matrix', or 'tibble'")

  # test 9
  data(vitamind)
  vitamind_mod <- vitamind
  colnames(vitamind_mod)[1] <- "rrr2"
  f <- function() {
    amp.acro(
      time_col = time, n_components = 1, group = "rrr2", period = 12,
      .data = vitamind_mod,
      .formula = Y ~ X + amp.acro(time, n_components = 1, group = "rrr2", period = 12)
    )
  }
  expect_error(f(), regexp = "Group variable names cannot contain 'rrr' or 'sss'")

  # test 10
  data(vitamind)
  vitamind_two_groups <- vitamind
  vitamind_two_groups["X2"] <- vitamind_two_groups["X"]
  f <- function() {
    amp.acro(
      time_col = time, n_components = 1, group = c("X", "X2"), period = 12,
      .data = vitamind_two_groups,
      .formula = Y ~ X + amp.acro(time, n_components = 1, group = c("X", "X2"), period = 12)
    )
  }
  expect_error(f(), regexp = "Grouping variable in amp.acro() must be of length 1 or the same as n_components", fixed = TRUE)

  # test 11
  data(vitamind)
  f <- function() {
    amp.acro(
      time_col = time, n_components = 1, group = "X", period = c(8, 12),
      .data = vitamind,
      .formula = Y ~ X + amp.acro(time, n_components = 1, group = "X", period = c(8, 12))
    )
  }
  expect_error(f(), regexp = "period value(s) in amp.acro() must be of length 1 or the same as n_components", fixed = TRUE)

  # test 12
  data(vitamind)
  f <- function() {
    amp.acro(
      time_col = time, n_components = 1, group = "Z", period = c(8, 12),
      .data = vitamind,
      .formula = Y ~ X + amp.acro(time, n_components = 1, group = "Z", period = c(8, 12))
    )
  }
  expect_error(f(), regexp = "Grouping variable(s) not found in input data:", fixed = TRUE)
})

test_that("matrix, or tibble inputs are converted to dataframe ", {
  # test 13
  data(vitamind)
  vitamind_mod <- dplyr::as_tibble(vitamind)

  f <- function() {
    amp.acro(
      time_col = time, n_components = 1, group = "X", period = 12,
      .data = vitamind_mod,
      .formula = Y ~ X + amp.acro(time, n_components = 1, group = "X", period = 12),
      .quietly = FALSE
    )
  }
  suppressMessages(expect_message(f(), regexp = "main_rrr1 and main_sss1 have been added to dataframe"))
  # test 14
  suppressMessages(expect_message(f(), regexp = "Data has been reformatted as dataframe"))


  # test 15
  vitamind_mod <- as.matrix(vitamind)
  f <- function() {
    amp.acro(
      time_col = time, n_components = 1, group = "X", period = 12,
      .data = vitamind_mod,
      .formula = Y ~ X + amp.acro(time, n_components = 1, group = "X", period = 12)
    )
  }
  expect(f(), ok = TRUE, "unsuccessful conversion of matrix to dataframe")
})
