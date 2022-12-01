test_that("can fit simple model", {
  model <- cosinor.glmm(Y ~ time(time) + X + amp.acro(X), data = vitamind)
  expect_true(inherits(model, "cosinor.glmm"))
  summary(model)
})


# test_that("can fit poisson model", {
#   poisson_df <-
#     simulate_cosinor(100,
#                      mesor = 5,
#                      amp = 1,
#                      acro = pi,
#                      beta.mesor = 0,
#                      beta.amp = 0.1,
#                      beta.acro = 0.5,
#                      dist="poisson")
#
#   model <- cosinor.glmm(Y ~ time(time) + X + amp.acro(X), data = poisson_df, family = poisson())
#   # reassess the simulate_cosinor() function to ensure that it's simulating poisson data properly
#   # if it is, check that the parameter estimates from the model above are pretty close
# })




