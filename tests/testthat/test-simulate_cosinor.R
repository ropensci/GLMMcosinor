#' @srrstats {G5.0}
#' @srrstats {G5.1}
#' @srrstats {G5.2}
#' @srrstats {G5.2a}
#' @srrstats {G5.2b}

test_that("simulation works", {
  df_gaussian <-
    simulate_cosinor(
      n = 100,
      mesor = 1,
      amp = 1,
      acro = 1,
      period = 24,
      family = "gaussian"
    )
  expect_s3_class(df_gaussian, "data.frame")
})


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
    regex = paste("amp must be a vector containing numbers, with",
                  "length equal to n_components"), fixed = TRUE
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
    regex = paste("acro must be a vector containing numbers, with",
                  "length equal to n_components"), fixed = TRUE
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
    regex = paste("period must be a vector containing numbers, with",
                  "length equal to n_components"), fixed = TRUE
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
    regex = paste('\'arg\' should be one of "gaussian", "poisson",',
                  '"binomial", "gamma"'), fixed = TRUE
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
    regex = paste("beta.amp must be a vector containing numbers,",
                  "with length equal to n_components"), fixed = TRUE
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
    regex = paste("beta.acro must be a vector containing numbers,",
                  "with length equal to n_components"), fixed = TRUE
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
