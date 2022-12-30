test_that("can fit simple model", {
  model <- cosinor.glmm(Y ~ 0 + time(time) + X + amp.acro(X), data = mutate(vitamind, X = as.factor(X)))
  model <- cosinor.glmm(Y ~ 1 + time(time) + X + amp.acro(X), data = vitamind)

  model <- cosinor.glmm(Y ~ 1 + amp.acro(time = time, n_components = 1), data = vitamind)

  model <- cosinor.glmm(Y ~ 1 + time(time, components = 2) + amp.acro(X), data = vitamind)
  # formula 1 + rrr + sss + rrr2 + sss2 + rrrX + sssX + rrr2X + sss2X
  model <- cosinor.glmm(Y ~ 1 + time(time, components = 1), data = vitamind)
  # formula = 1 + rrr + sss
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
