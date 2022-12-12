test_that("multiplication works", {

  amp.acro(time_col=time, n_components=1, group=c(NA, "X"), period = c(24, 12), .data=vitamind,
           .formula = Y ~ X + amp.acro(time, n_components=2, group=c(NA, "X"), period=c(24, 12)),
           test=TRUE)

  f <- Y ~ X + amp.acro(time, n_components=2, group=c(NA, "X"))

  cosinor.glmm(Y ~ X + amp.acro(time,  n_components = 2, group = c(NA, "X"), period = c(12, 24)), data=vitamind)

})
