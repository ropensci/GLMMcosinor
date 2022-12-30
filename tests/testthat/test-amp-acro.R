test_that("multiplication works", {
  amp.acro(
    time_col = time, n_components = 1, group = c(NA, "X"), period = c(24, 12), .data = vitamind,
    .formula = Y ~ X + amp.acro(time, n_components = 2, group = c(NA, "X"), period = c(24, 12)),
    test = TRUE
  )

  f <- Y ~ X + amp.acro(time, n_components = 2, group = c(NA, "X"))

  cosinor.glmm(Y ~ X + amp.acro(time, n_components = 2, group = c(NA, "X"), period = c(12, 24)), data = vitamind)
})


test_that("bad inputs return useful errors", {
  f <- function() {
    amp.acro(
      time_col = time, n_components = 1.1, group = "X", period = 12,
      .data = vitamind,
      .formula = Y ~ X + amp.acro(time, n_components = 1.1, group = "X", period = 12)
    )
  }

  expect_error( # non-count n_components
    f(),
    regexp = "assertthat::is.count(n_components) is not TRUE",
    fixed = TRUE
  )

  f <- function() {
    amp.acro(
      time_col = time, n_components = 1, group = "X", period = -12,
      .data = vitamind,
      .formula = Y ~ X + amp.acro(time, n_components = 1, group = "X", period = -12)
    )
  }

  expect_error(f(), regexp = "period > 0") # negative period
})
