test_that("assess formula manipulation", {
  res <-
    update_formula_and_data(
      formula = Y ~ X + amp.acro(time, n_components = 2, group = "X", period = c(12, 10)),
      data = vitamind
    )
  expect_true(all.equal(res$newformula, Y ~ X + X:rrr1 + X:sss1 + X:rrr2 + X:sss2))
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
  expect_message(res())
})

# not sure if this is relevant?
#  dat <- vitamind
#  dat$ID <- sample(LETTERS[1:5], size = nrow(dat), replace = TRUE)
#  res <- update_formula_and_data(
#    formula = Y ~ X + amp.acro(time, n_components = 1, group = "X", period = 24),
#    data = dat
#  )
#  expected_formula <- Y ~ X + X:rrr1 + X:sss1 + (X:rrr1 + X:sss1 | ID)
#
#  Y <- 1 + X + (1 | ID)
