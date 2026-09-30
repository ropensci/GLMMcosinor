test_that("ranef_estimates() extracts correct point estimates (#37)", {
  withr::local_seed(42)

  f_sample_id <- function(
    id_num,
    n = 30,
    mesor = rnorm(1),
    amp = rnorm(1, mean = 3, sd = 0.5),
    acro = rnorm(1, mean = 1.5, sd = 0.2),
    family = "gaussian",
    sd = 0.2,
    period = 24,
    n_components = 1
  ) {
    data <- simulate_cosinor(
      n = n,
      mesor = mesor,
      amp = amp,
      acro = acro,
      family = family,
      sd = sd,
      period = period,
      n_components = n_components
    )
    data$subject <- id_num
    data
  }

  dat_mixed <- do.call("rbind", lapply(1:15, function(x) f_sample_id(id_num = x)))
  dat_mixed$subject <- as.factor(dat_mixed$subject)

  # no fixed group
  object <- cglmm(
    Y ~ amp_acro(times, n_components = 1, period = 24) +
      (1 + amp_acro1 | subject),
    data = dat_mixed
  )

  est <- ranef_estimates(object)
  expect_s3_class(est, "data.frame")
  expect_setequal(names(est), c("subject", "component", "amp", "acr", "mesor"))
  expect_setequal(unique(est$subject), levels(dat_mixed$subject))
  expect_false(anyNA(est$amp[est$component == "1"]))
  expect_false(anyNA(est$mesor[est$component == "mesor"]))

  # cross-check against raw_coefficients + glmmTMB::ranef() manually, for
  # one subject
  re <- glmmTMB::ranef(object$fit)$cond$subject
  subject_2 <- as.character(2)
  r_tot <- object$raw_coefficients[["main_rrr1"]] + re["2", "main_rrr1"]
  s_tot <- object$raw_coefficients[["main_sss1"]] + re["2", "main_sss1"]
  expect_equal(
    est$amp[est$subject == subject_2 & est$component == "1"],
    sqrt(r_tot^2 + s_tot^2)
  )
  expect_equal(
    est$acr[est$subject == subject_2 & est$component == "1"],
    atan2(s_tot, r_tot)
  )
  mesor_expected <- object$raw_coefficients[["(Intercept)"]] +
    re["2", "(Intercept)"]
  expect_equal(
    est$mesor[est$subject == subject_2 & est$component == "mesor"],
    mesor_expected
  )

  # with a fixed group alongside the random effect
  dat_mixed$X <- rep(sample(0:1, 15, replace = TRUE), each = 30)
  object_grp <- cglmm(
    Y ~ X +
      amp_acro(times, n_components = 1, group = "X", period = 24) +
      (1 + amp_acro1 | subject),
    data = dat_mixed
  )
  est_grp <- ranef_estimates(object_grp)
  expect_false(anyNA(est_grp$amp[est_grp$component == "1"]))
  expect_false(anyNA(est_grp$mesor[est_grp$component == "mesor"]))
})

test_that("ranef_estimates() handles partial random slopes and errors (#37)", {
  withr::local_seed(42)

  f_sample_id <- function(
    id_num,
    n = 20,
    mesor = 0,
    amp = c(1, rnorm(1, mean = 5, sd = 1)),
    acro = c(1, rnorm(1, mean = pi, sd = 1)),
    family = "gaussian",
    period = c(12, 6),
    n_components = 2
  ) {
    data <- simulate_cosinor(
      n = n,
      mesor = mesor,
      amp = amp,
      acro = acro,
      family = family,
      period = period,
      n_components = n_components
    )
    data$id <- id_num
    data
  }
  df_mixed <- dplyr::bind_rows(lapply(1:15, f_sample_id))
  df_mixed$id <- as.factor(df_mixed$id)

  # random slope only for component 2 - component 1 should have no
  # per-subject variation (same value repeated for every subject)
  object <- cglmm(
    Y ~ amp_acro(times, n_components = 2, period = c(12, 6)) +
      (0 + amp_acro2 | id),
    data = df_mixed
  )
  est <- ranef_estimates(object)
  comp1 <- est[est$component == "1", ]
  comp2 <- est[est$component == "2", ]
  expect_equal(length(unique(comp1$amp)), 1L)
  expect_true(length(unique(comp2$amp)) > 1L)

  # error: no random effects
  object_no_ranef <- cglmm(
    vit_d ~ amp_acro(time, group = "X", period = 12),
    data = vitamind
  )
  expect_error(
    ranef_estimates(object_no_ranef),
    regexp = "does not have any random effects"
  )

  # error: multiple ranef groups, none specified
  dat2 <- simulate_cosinor(n = 100, mesor = 1, amp = 2, acro = 1, period = 24)
  dat2$id1 <- factor(sample(1:5, 100, replace = TRUE))
  dat2$id2 <- factor(sample(1:5, 100, replace = TRUE))
  object_multi <- cglmm(
    Y ~ amp_acro(times, n_components = 1, period = 24) +
      (1 | id1) +
      (1 | id2),
    data = dat2
  )
  expect_error(
    ranef_estimates(object_multi),
    regexp = "more than one random-effect grouping variable"
  )
  expect_no_error(ranef_estimates(object_multi, ranef_group = "id1"))
  expect_error(
    ranef_estimates(object_multi, ranef_group = "bogus"),
    regexp = "must be one of the random-effect grouping variables"
  )
})
