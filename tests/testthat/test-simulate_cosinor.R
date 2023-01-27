test_that("multiplication works", {
  # Test 1
  f <- function() {
    simulate_cosinor(100.1,
      mesor = c(1, 2, 4),
      amp = c(2, 1, 0.5),
      acro = c(1, 1.5, 0.1),
      beta.mesor = c(1, 2, 3),
      beta.amp = c(2, 1, 0.4),
      beta.acro = c(1, 1.5, -1),
      family = "gamma",
      period = c(12, 6, 8),
      n_components = 3,
      alpha = 5,
      beta.group = FALSE
    )
  }
  expect_error(
    f(),
    regex = "n must be an integer greater than 0", fixed = TRUE
  )

  # Test 2
  f <- function() {
    simulate_cosinor(100,
      mesor = 4,
      amp = c(2, 1, 0.5),
      acro = c(1, 1.5, 0.1),
      beta.mesor = 3,
      beta.amp = c(2, 1, 0.4),
      beta.acro = c(1, 1.5, -1),
      family = "gamma",
      period = c(12, 6, 8),
      n_components = 0,
      alpha = 5,
      beta.group = FALSE
    )
  }
  expect_error(
    f(),
    regex = "n_components must be an integer greater than 0", fixed = TRUE
  )

  # Test 3
  f <- function() {
    simulate_cosinor(100,
      mesor = c(1, 4),
      amp = c(2, 1, 0.5),
      acro = c(1, 1.5, 0.1),
      beta.mesor = 4,
      beta.amp = c(2, 1, 0.4),
      beta.acro = c(1, 1.5, -1),
      family = "gamma",
      period = c(12, 6, 8),
      n_components = 3,
      alpha = 5,
      beta.group = FALSE
    )
  }
  expect_error(
    f(),
    regex = "mesor must a single number", fixed = TRUE
  )

  # Test 4
  f <- function() {
    simulate_cosinor(100,
      mesor = 4,
      amp = c(2, 1),
      acro = c(1, 1.5, 0.1),
      beta.mesor = 4,
      beta.amp = c(2, 1, 0.4),
      beta.acro = c(1, 1.5, -1),
      family = "gamma",
      period = c(12, 6, 8),
      n_components = 3,
      alpha = 5,
      beta.group = FALSE
    )
  }
  expect_error(
    f(),
    regex = "amp must be a vector containing numbers, with length equal to n_components", fixed = TRUE
  )

  # Test 5
  f <- function() {
    simulate_cosinor(100,
      mesor = 3,
      amp = c(2, 1, 3),
      acro = c(1, 1.5),
      beta.mesor = 4,
      beta.amp = c(2, 1, 0.4),
      beta.acro = c(1, 1.5, -1),
      family = "gamma",
      period = c(12, 6, 8),
      n_components = 3,
      alpha = 5,
      beta.group = FALSE
    )
  }
  expect_error(
    f(),
    regex = "acro must be a vector containing numbers, with length equal to n_components", fixed = TRUE
  )

  # Test 6
  f <- function() {
    simulate_cosinor(100,
      mesor = 2,
      amp = c(2, 1, 3),
      acro = c(1, 1.5, 2),
      beta.mesor = c(1, 2),
      beta.amp = c(2, 1, 0.4),
      beta.acro = c(1, 1.5, -1),
      family = "gamma",
      period = c(12, 6, 8),
      n_components = 3,
      alpha = 5,
      beta.group = FALSE
    )
  }
  expect_error(
    f(),
    regex = "beta.mesor must be a single number", fixed = TRUE
  )

  # Test 6
  f <- function() {
    simulate_cosinor(100,
      mesor = 4,
      amp = c(2, 1, 3),
      acro = c(1, 1.5, 2),
      beta.mesor = 4,
      beta.amp = c(2, 1),
      beta.acro = c(1, 1.5, -1),
      family = "gamma",
      period = c(12, 6, 8),
      n_components = 3,
      alpha = 5,
      beta.group = FALSE
    )
  }
  expect_error(
    f(),
    regex = "beta.amp must be a vector containing numbers, with length equal to n_components", fixed = TRUE
  )

  # Test 7
  f <- function() {
    simulate_cosinor(100,
      mesor = 3,
      amp = c(2, 1, 3),
      acro = c(1, 1.5, 2),
      beta.mesor = 4,
      beta.amp = c(2, 1, 0.4),
      beta.acro = c(1, 1.5),
      family = "gamma",
      period = c(12, 6, 8),
      n_components = 3,
      alpha = 5,
      beta.group = FALSE
    )
  }
  expect_error(
    f(),
    regex = "beta.acro must be a vector containing numbers, with length equal to n_components", fixed = TRUE
  )

  # Test 8
  f <- function() {
    simulate_cosinor(100,
      mesor = 3,
      amp = c(2, 1, 3),
      acro = c(1, 1.5, 2),
      beta.mesor = 4,
      beta.amp = c(2, 1, 0.4),
      beta.acro = c(1, 1.5, 0.4),
      family = "gamma",
      period = c(12, 6),
      n_components = 3,
      alpha = 5,
      beta.group = FALSE
    )
  }
  expect_error(
    f(),
    regex = "period must be a vector containing numbers, with length equal to n_components", fixed = TRUE
  )

  # Test 9
  f <- function() {
    simulate_cosinor(100,
      mesor = 3,
      amp = c(2, 1, 3),
      acro = c(1, 1.5, 2),
      beta.mesor = 4,
      beta.amp = c(2, 1, 0.4),
      beta.acro = c(1, 1.5, 0.4),
      family = "gamma",
      period = c(12, 6, 5),
      n_components = 3,
      beta.group = 10,
      alpha = 5
    )
  }
  expect_error(
    f(),
    regex = "beta.group argument must be logical", fixed = TRUE
  )

  # Test 10
  f <- function() {
    simulate_cosinor(100,
      mesor = 3,
      amp = c(2, 1, 3),
      acro = c(1, 1.5, 2),
      beta.mesor = 4,
      beta.amp = c(2, 1, 0.4),
      beta.acro = c(1, 1.5, 0.4),
      family = "continuous",
      period = c(12, 6, 5),
      n_components = 3,
      beta.group = TRUE,
      alpha = 5
    )
  }
  expect_error(
    f(),
    regex = 'family argument must be a string that matches one of: "poisson", "binomial", "gamma", "gaussian"', fixed = TRUE
  )
})
