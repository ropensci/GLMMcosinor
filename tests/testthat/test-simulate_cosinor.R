#' @srrstats {G5.0} *Where applicable or practicable, tests should use standard data sets with known properties (for example, the [NIST Standard Reference Datasets](https://www.itl.nist.gov/div898/strd/), or data sets provided by other widely-used R packages).*
#' @srrstats {G5.1} *Data sets created within, and used to test, a package should be exported (or otherwise made generally available) so that users can confirm tests and run examples.*
#' @srrstats {G5.2} *Appropriate error and warning behaviour of all functions should be explicitly demonstrated through tests. In particular,*
#' @srrstats {G5.2a} *Every message produced within R code by `stop()`, `warning()`, `message()`, or equivalent should be unique*
#' @srrstats {G5.2b} *Explicit tests should demonstrate conditions which trigger every one of those messages, and should compare the result with expected values.*


test_that("assess error messaging", {
  # bad 'n'
  f <- function() {
    simulate_cosinor(
      n = 100.1,
      mesor = 1,
      amp = 1,
      acro = 1,
      period = 24,
      family = "gaussian"
    )
  }
  expect_error(
    f(),
    regex = "n must be an integer greater than 0", fixed = TRUE
  )

  # bad 'n_components'
  f <- function() {
    simulate_cosinor(
      n = 100,
      mesor = 1,
      amp = 1,
      acro = 1,
      period = 24,
      n_components = 0,
      family = "gaussian"
    )
  }
  expect_error(
    f(),
    regex = "n_components must be an integer greater than 0", fixed = TRUE
  )

  # bad 'mesor'
  f <- function() {
    simulate_cosinor(
      n = 100,
      mesor = c(1, 2),
      amp = 1,
      acro = 1,
      period = 24,
      n_components = 1,
      family = "gaussian"
    )
  }
  expect_error(
    f(),
    regex = "mesor must a single number", fixed = TRUE
  )

  # bad 'amp'
  f <- function() {
    simulate_cosinor(
      n = 100,
      mesor = 1,
      amp = c(1, 2),
      acro = 1,
      period = 24,
      n_components = 1,
      family = "gaussian"
    )
  }
  expect_error(
    f(),
    regex = "amp must be a vector containing numbers, with length equal to n_components", fixed = TRUE
  )

  # bad 'acro'
  f <- function() {
    simulate_cosinor(
      n = 100,
      mesor = 1,
      amp = 1,
      acro = c(1, 2),
      period = 24,
      n_components = 1,
      family = "gaussian"
    )
  }
  expect_error(
    f(),
    regex = "acro must be a vector containing numbers, with length equal to n_components", fixed = TRUE
  )

  # bad 'period'
  f <- function() {
    simulate_cosinor(
      n = 100,
      mesor = 1,
      amp = 1,
      acro = 1,
      period = c(24, 12),
      n_components = 1,
      family = "gaussian"
    )
  }
  expect_error(
    f(),
    regex = "period must be a vector containing numbers, with length equal to n_components", fixed = TRUE
  )

  # bad 'family'
  f <- function() {
    simulate_cosinor(
      n = 100,
      mesor = 1,
      amp = 1,
      acro = 1,
      period = 24,
      n_components = 1,
      family = "bad family"
    )
  }
  expect_error(
    f(),
    regex = '\'arg\' should be one of "gaussian", "poisson", "binomial", "gamma"', fixed = TRUE
  )

  # bad 'beta.mesor'
  f <- function() {
    simulate_cosinor(
      n = 100,
      mesor = 1,
      amp = 1,
      acro = 1,
      period = 24,
      n_components = 1,
      family = "gaussian",
      beta.group = TRUE,
      beta.mesor = "2",
      beta.amp = 2,
      beta.acro = 2
    )
  }
  expect_error(
    f(),
    regex = "beta.mesor must be a single number", fixed = TRUE
  )

  # bad 'beta.amp'
  f <- function() {
    simulate_cosinor(
      n = 100,
      mesor = 1,
      amp = 1,
      acro = 1,
      period = 24,
      n_components = 1,
      family = "gaussian",
      beta.group = TRUE,
      beta.mesor = 2,
      beta.amp = "2",
      beta.acro = 2
    )
  }
  expect_error(
    f(),
    regex = "beta.amp must be a vector containing numbers, with length equal to n_components", fixed = TRUE
  )

  # bad 'beta.acro'
  f <- function() {
    simulate_cosinor(
      n = 100,
      mesor = 1,
      amp = 1,
      acro = 1,
      period = 24,
      n_components = 1,
      family = "gaussian",
      beta.group = TRUE,
      beta.mesor = 2,
      beta.amp = 2,
      beta.acro = "2"
    )
  }
  expect_error(
    f(),
    regex = "beta.acro must be a vector containing numbers, with length equal to n_components", fixed = TRUE
  )

  # bad 'beta.group'
  f <- function() {
    simulate_cosinor(
      n = 100,
      mesor = 1,
      amp = 1,
      acro = 1,
      period = 24,
      n_components = 1,
      family = "gaussian",
      beta.group = "TRUE",
      beta.mesor = 2,
      beta.amp = 2,
      beta.acro = 2
    )
  }
  expect_error(
    f(),
    regex = "beta.group argument must be logical", fixed = TRUE
  )

  # missing 'beta.acro'
  f <- function() {
    simulate_cosinor(
      n = 100,
      mesor = 1,
      amp = 1,
      acro = 1,
      period = 24,
      n_components = 1,
      family = "gaussian",
      beta.group = TRUE,
      beta.mesor = 2,
      beta.amp = 2
    )
  }
  expect_error(
    f(),
    regex = '"beta.acro" is missing, with no default', fixed = TRUE
  )
})
