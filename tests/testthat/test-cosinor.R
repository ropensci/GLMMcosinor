test_that("model returns accurate parameters", {
  set.seed(50)
  TrueAmp_a <- 2
  TrueMesor_a <- 1
  TrueAcr_a <- 3
  TrueAmp_b <- 1
  TrueMesor_b <- 0.5
  TrueAcr_b <- 0.3
  comod <- simulate_cosinor(10000, mesor = TrueMesor_a, amp = TrueAmp_a, acro = TrueAcr_a, beta.mesor = TrueMesor_b, beta.amp = TrueAmp_b, beta.acro = TrueAcr_b, family = "gaussian")
  object <- cosinor.glmm(Y ~ group + amp.acro(times, n_components = 1, group = "group", period = c(12)), data = comod)

  group_a_amp <- object$coefficients["group0:amp"]
  testthat::expect_true((TrueAmp_a * 0.95 < group_a_amp) & (group_a_amp < TrueAmp_a * 1.05))

  group_a_acr <- object$coefficients["group0:acr"]
  testthat::expect_true((abs(TrueAcr_a * 0.95) < abs(group_a_acr)) & (abs(group_a_acr) < abs(TrueAcr_a * 1.05)))

  group_a_mes <- object$coefficients[1]
  testthat::expect_true((TrueMesor_a * 0.95 < group_a_mes) & (group_a_mes < TrueMesor_a * 1.05))


  group_b_amp <- object$coefficients["group1:amp"]
  testthat::expect_true((TrueAmp_b * 0.95 < group_b_amp) & (group_b_amp < TrueAmp_b * 1.05))

  group_b_acr <- object$coefficients["group1:acr"]
  testthat::expect_true((abs(TrueAcr_b * 0.95) < abs(group_b_acr)) & (abs(group_b_acr) < abs(TrueAcr_b * 1.05)))

  group_b_mes <- object$coefficients[2]
  testthat::expect_true(((TrueMesor_b - TrueMesor_a) * 1.05 < group_b_mes) & (group_b_mes < (TrueMesor_b - TrueMesor_a) * 0.95))
})
test_that("model output is class cosinor.glmm", {
  data(vitamind)
  object <- cosinor.glmm(Y ~ X + amp.acro(time, group = "X"), data = vitamind)
  expect_true(inherits(object, "cosinor.glmm"))

  data(vitamind)
  object <- cosinor.glmm(Y ~ X + amp.acro(time, group = "X"), data = vitamind,
                         dispformula = ~ 0 + amp.acro(time, group = "X"),
                         ziformula = ~ 0  + amp.acro(time,group="X"))
  testthat::expect_no_error(object)
  testthat::expect_true(inherits(object, "cosinor.glmm"))

})
