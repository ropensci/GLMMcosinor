test_that("add_ranef_boots() attaches bootstrap estimates and errors sensibly (#37)", {
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

  object <- cglmm(
    Y ~ amp_acro(times, n_components = 1, period = 24) +
      (1 + amp_acro1 | subject),
    data = dat_mixed
  )

  object <- add_ranef_boots(object, nsim = 25)

  expect_type(object$ranef_boots, "list")
  expect_setequal(
    names(object$ranef_boots),
    c("estimates", "ranef_group", "nsim", "n_success")
  )
  expect_equal(object$ranef_boots$ranef_group, "subject")
  expect_equal(object$ranef_boots$nsim, 25)
  # add_ranef_boots() keeps retrying until nsim replicates actually succeed,
  # so n_success should always equal nsim on return (never less)
  expect_equal(object$ranef_boots$n_success, 25)
  expect_setequal(
    names(object$ranef_boots$estimates),
    c("subject", "component", "amp", "acr", "mesor", ".rep")
  )
  expect_equal(
    length(unique(object$ranef_boots$estimates$.rep)),
    object$ranef_boots$n_success
  )
  # each replicate's draw should differ (guards against a seed-reuse bug
  # where every retry produces an identical simulated response)
  amp1_draws <- object$ranef_boots$estimates$amp[
    object$ranef_boots$estimates$subject == "1" &
      object$ranef_boots$estimates$component == "1"
  ]
  expect_equal(length(unique(amp1_draws)), 25)

  # error: no random effects
  object_no_ranef <- cglmm(
    vit_d ~ amp_acro(time, group = "X", period = 12),
    data = vitamind
  )
  expect_error(
    add_ranef_boots(object_no_ranef),
    regexp = "does not have any random effects"
  )

  # error: bad nsim
  expect_error(
    add_ranef_boots(object, nsim = -1),
    regexp = "'nsim' must be a positive integer"
  )
  expect_error(
    add_ranef_boots(object, nsim = 1.5),
    regexp = "'nsim' must be a positive integer"
  )

  # error: max_attempts must be at least nsim
  expect_error(
    add_ranef_boots(object, nsim = 25, max_attempts = 5),
    regexp = "'max_attempts' must be a positive integer, at least 'nsim'"
  )
  # the boundary (max_attempts == nsim) is valid and succeeds when every
  # attempt converges (as is the case for this well-behaved model)
  boundary_object <- add_ranef_boots(object, nsim = 10, max_attempts = 10)
  expect_equal(boundary_object$ranef_boots$n_success, 10)

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
    add_ranef_boots(object_multi, nsim = 5),
    regexp = "more than one random-effect grouping variable"
  )
  expect_no_error(add_ranef_boots(object_multi, nsim = 5, ranef_group = "id1"))

  # reproducible when a seed is supplied, even though simulate() is now
  # called once per attempt internally (rather than once for all nsim)
  boot_a <- add_ranef_boots(object, nsim = 5, seed = 999)
  boot_b <- add_ranef_boots(object, nsim = 5, seed = 999)
  expect_equal(
    boot_a$ranef_boots$estimates$amp,
    boot_b$ranef_boots$estimates$amp
  )
})

test_that("ranef_ci = 0.95 works across ranef_estimates/polar_plot/autoplot (#37)", {
  withr::local_seed(42)

  dat_mixed <- do.call(
    "rbind",
    lapply(1:15, function(id) {
      d <- simulate_cosinor(
        n = 20,
        mesor = rnorm(1),
        amp = rnorm(1, mean = 3, sd = 0.5),
        acro = rnorm(1, mean = 1.5, sd = 0.2),
        family = "gaussian",
        period = 24,
        n_components = 1
      )
      d$subject <- id
      d
    })
  )
  dat_mixed$subject <- as.factor(dat_mixed$subject)

  object <- cglmm(
    Y ~ amp_acro(times, n_components = 1, period = 24) +
      (1 + amp_acro1 | subject),
    data = dat_mixed
  )

  # informative error before bootstrapping, from all three entry points
  expect_error(
    ranef_estimates(object, ranef_ci = 0.95),
    regexp = "add_ranef_boots\\(\\)"
  )
  expect_error(
    polar_plot(object, ranef_plot = "subject", ranef_ci = 0.95),
    regexp = "add_ranef_boots\\(\\)"
  )
  expect_error(
    autoplot(object, ranef_lines = "subject", ranef_ci = 0.95),
    regexp = "add_ranef_boots\\(\\)"
  )

  # ranef_ci must be NULL or a single number in (0, 1) - the old
  # logical TRUE/FALSE API is no longer accepted
  expect_error(
    ranef_estimates(object, ranef_ci = TRUE),
    regexp = "'ranef_ci' must be NULL"
  )
  expect_error(
    ranef_estimates(object, ranef_ci = 1.5),
    regexp = "single numeric value in \\[0, 1\\]"
  )
  expect_error(
    ranef_estimates(object, ranef_ci = c(0.9, 0.95)),
    regexp = "'ranef_ci' must be NULL"
  )
  expect_no_error(ranef_estimates(object, ranef_ci = NULL))

  object <- add_ranef_boots(object, nsim = 25)

  est_ci <- ranef_estimates(object, ranef_ci = 0.95)
  expect_true(all(
    c("amp_lower", "amp_upper", "acr_lower", "acr_upper", "mesor_lower", "mesor_upper") %in%
      names(est_ci)
  ))
  # note: since these are percentile bootstrap intervals around a shrinkage
  # (BLUP) point estimate, the original point estimate is not guaranteed to
  # fall inside its own interval for every subject - just check the
  # intervals are valid (non-NA, lower <= upper)
  comp_rows <- est_ci[est_ci$component == "1", ]
  expect_false(anyNA(comp_rows$amp_lower))
  expect_false(anyNA(comp_rows$amp_upper))
  expect_true(all(comp_rows$amp_lower <= comp_rows$amp_upper))

  mesor_rows <- est_ci[est_ci$component == "mesor", ]
  expect_false(anyNA(mesor_rows$mesor_lower))
  expect_true(all(mesor_rows$mesor_lower <= mesor_rows$mesor_upper))

  expect_no_error(polar_plot(object, ranef_plot = "subject", ranef_ci = 0.95))
  expect_no_error(autoplot(object, ranef_lines = "subject", ranef_ci = 0.95))
  # a different confidence level is also accepted
  expect_no_error(polar_plot(object, ranef_plot = "subject", ranef_ci = 0.8))
  expect_no_error(autoplot(object, ranef_lines = "subject", ranef_ci = 0.8))

  # ranef_ci requires the matching overlay argument
  expect_error(
    polar_plot(object, ranef_ci = 0.95),
    regexp = "'ranef_ci' requires 'ranef_plot'"
  )
  expect_error(
    autoplot(object, ranef_ci = 0.95),
    regexp = "'ranef_ci' requires 'ranef_lines'"
  )

  # invalid ranef_ci type/range is rejected on both plotting functions too
  expect_error(
    polar_plot(object, ranef_plot = "subject", ranef_ci = TRUE),
    regexp = "'ranef_ci' must be NULL"
  )
  expect_error(
    autoplot(object, ranef_lines = "subject", ranef_ci = TRUE),
    regexp = "'ranef_ci' must be NULL"
  )
})
