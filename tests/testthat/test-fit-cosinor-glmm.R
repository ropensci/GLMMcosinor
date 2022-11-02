test_that("can fit simple model", {
  model <- cosinor.glmm(Y ~ time(time) + X + amp.acro(X), data = vitamind)
  expect_true(inherits(model, "cosinor.glmm"))
})
