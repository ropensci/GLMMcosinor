test_that("multiplication works", {
  amp.acro(
    time_col = time, n_components = 2, group = "X", period = c(24, 12), .data = vitamind,
    .formula = Y ~ X + amp.acro(time, n_components = 2, group = "X", period = c(24, 12))
  )

  f <- Y ~ X + amp.acro(time, n_components = 2, group = c(NA, "X"))

  cosinor.glmm(Y ~ X + amp.acro(time, n_components = 2, group = c(NA, "X"), period = c(12, 24)), data = vitamind)
  expect(ok = TRUE, "unsuccessful multiplication of group to match n_components")
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

  data(vitamind)
  vitamind_mod <- vitamind
  vitamind_mod$time <- as.factor(rbinom(length(vitamind$time),1,0.5))
  f <- function() {
    amp.acro(
      time_col = time, n_components = 1, group = "X", period = 12,
      .data = vitamind_mod,
      .formula = Y ~ X + amp.acro(time, n_components = 1, group = "X", period = 12)
    )
  }
  expect_error(f(), regexp = "ttt is not a numeric or integer vector")
})
