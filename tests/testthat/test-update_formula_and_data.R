test_that("assess formula manipulation", {
  res <-
    update_formula_and_data(
      formula = Y ~ X + amp.acro(time, n_components = 2, group = "X", period = c(12, 10)),
      data = vitamind
    )
  expect_true(all.equal(res$newformula, Y ~ X + X:main_rrr1 + X:main_sss1 + X:main_rrr2 + X:main_sss2))
})

test_that("warning for missing data", {
  data(vitamind)
  vitamind$Y[1] <- NA
  res <- function() {
    update_formula_and_data(
      formula = Y ~ X + amp.acro(time, n_components = 2, group = "X", period = c(12, 10)),
      data = vitamind, quietly = FALSE
    )
  }
  suppressMessages(expect_message(res()))
})
