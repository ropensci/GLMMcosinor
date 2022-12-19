test_that("assess formula manipulation", {
  res <-
    update_formula_and_data(
      formula = Y ~ group+amp.acro(time, n_components = 2, group = "X", period = c(12,10)),
      data = vitamind
    )

  expect_true(all.equal(res$formula, Y ~ X + rrr1 + sss1 + rrr2 + sss2 + X:rrr1 + X:sss1 + X:rrr2 + X:sss2))



  dat <- vitamind
  dat$ID <- sample(LETTERS[1:5], size=nrow(dat), replace = TRUE)
  res <- update_formula_and_data(
    formula = Y ~ group + amp.acro(time, n_components = 1, group = "X", period = 24),
    data = dat
  )
  expected_formula <- Y ~ X + rrr1 + sss1 + X:rrr1 + X:sss1 + (rrr1 + sss1 + X:rrr1 + X:sss1|ID)
  expected_formula <- Y ~ X + rrr1 + sss1 + X:rrr1 + X:sss1 + (rrr1 + sss1 | ID) + (X:rrr1 + X:sss1|ID)

  Y <- 1 + group + (1|ID)
  Y <- 1 + group + (1 + group|ID)

})
