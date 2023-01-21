test_that("model returns accurate parameters", {
  TrueAmp_a <- 2
  TrueMesor_a <- 1
  TrueAcr_a <- 3
  TrueAmp_b <- 1
  TrueMesor_b <- 0.5
  TrueAcr_b <- 0.3
  comod <- simulate_cosinor(10000, mesor = TrueMesor_a, amp = TrueAmp_a, acro = TrueAcr_a, beta.mesor = TrueMesor_b, beta.amp = TrueAmp_b, beta.acro = TrueAcr_b, dist = "gaussian")
  object <- cosinor.glmm(Y ~ group + amp.acro(times, n_components = 1, group = "group", period = c(12)), data = comod)


  group_a_amp <- object$coefficients["group0:amp"]
  expect_true((TrueAmp_a * 0.9 < group_a_amp) & (group_a_amp < TrueAmp_a * 1.1))

  group_a_acr <- object$coefficients["group0:acr"]
  expect_true((abs(TrueAcr_a * 0.9) < abs(group_a_acr)) & (abs(group_a_acr) < abs(TrueAcr_a * 1.1)))

  group_a_mes <- object$coefficients[1]
  expect_true((TrueMesor_a * 0.9 < group_a_mes) & (group_a_mes < TrueMesor_a * 1.1))


  group_b_amp <- object$coefficients["group1:amp"]
  expect_true((TrueAmp_b * 0.9 < group_b_amp) & (group_b_amp < TrueAmp_b * 1.1))

  group_b_acr <- object$coefficients["group1:acr"]
  expect_true((abs(TrueAcr_b * 0.9) < abs(group_b_acr)) & (abs(group_b_acr) < abs(TrueAcr_b * 1.1)))

  group_b_mes <- object$coefficients[2]
  expect_true(((TrueMesor_b - TrueMesor_a) * 1.1 < group_b_mes) & (group_b_mes < (TrueMesor_b - TrueMesor_a) * 0.9))
})
test_that("model output is class cosinor.glmm", {
  data(vitamind)
  object <- cosinor.glmm(Y ~ X + amp.acro(time, group = "X"), data = vitamind)
  expect_true(inherits(object, "cosinor.glmm"))
})
