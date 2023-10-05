#' @srrstats {G5.0}
#' @srrstats {G5.1}
#' @srrstats {G5.2}
#' @srrstats {G5.2a}
#' @srrstats {G5.2b}
#' @srrstats {G5.5}
#' @srrstats {G5.4}
#' @srrstats {G5.4a}
#' @srrstats {G5.4b}
#' @srrstats {G5.6}
#' @srrstats {G5.6a}
#' @srrstats {G5.6b}
#' @srrstats {G5.9b}
#' @srrstats {G2.11}


test_that("model returns accurate parameters", {
  f_round <- function(x) {
    unname(round(x, digits = 4))
  }

  TrueMesor_a <- 1
  TrueMesor_b <- 0.5
  TrueAmp_a <- 2
  TrueAmp_b <- 1
  TrueAcr_a <- 3
  TrueAcr_b <- 0.3
  TruePeriod <- 12

  # test parameter estimation of Gaussian simulated data
  withr::with_seed(
    50,
    {
      comod <- simulate_cosinor(
        n = 10000,
        mesor = TrueMesor_a,
        amp = TrueAmp_a,
        acro = TrueAcr_a,
        beta.mesor = TrueMesor_b,
        beta.amp = TrueAmp_b,
        beta.acro = TrueAcr_b,
        family = "gaussian",
        period = TruePeriod,
        beta.group = TRUE
      )
      object <- cglmm(
        Y ~ group + amp_acro(times,
          n_components = 1,
          group = "group",
          period = 12
        ),
        data = comod
      )
      cosinor_lm_mod <- cosinor::cosinor.lm(
        Y ~ time(times) + group + amp.acro(group),
        data = comod
      )
    }
  )
  testthat::expect_true(all.equal(
    f_round(object$coefficients),
    c(1.0030, -0.4966, 2.0122, 0.9948, 3.0115, 0.3175)
  ))

  # test similarity to cosinor::cosinor.lm()
  comparison_df <- cbind(
    cosinor_lm_mod$coefficients,
    object$coefficients
  )

  comparison_df <- as.data.frame(
    comparison_df[rownames(comparison_df) != "acr", ]
  )
  expect_equal(comparison_df$V1, comparison_df$V2, tolerance = 0.1)



  # test another parameter estimation of Gaussian simulated data
  withr::with_seed(
    100,
    {
      comod <- simulate_cosinor(
        n = 10000,
        mesor = TrueMesor_a,
        amp = TrueAmp_a,
        acro = TrueAcr_a,
        beta.mesor = TrueMesor_b,
        beta.amp = TrueAmp_b,
        beta.acro = TrueAcr_b,
        family = "gaussian",
        period = TruePeriod,
        beta.group = TRUE
      )
      object <- cglmm(
        Y ~ group + amp_acro(times,
          n_components = 1,
          group = "group",
          period = 12
        ),
        data = comod
      )
    }
  )
  testthat::expect_true(all.equal(
    f_round(object$coefficients),
    c(0.9905, -0.4932, 1.9737, 1.0221, 3.0066, 0.2965)
  ))

  # test parameter estimation of poisson simulated data
  withr::with_seed(
    50,
    {
      comod <- simulate_cosinor(
        n = 10000,
        mesor = TrueMesor_a,
        amp = TrueAmp_a,
        acro = TrueAcr_a,
        beta.mesor = TrueMesor_b,
        beta.amp = TrueAmp_b,
        beta.acro = TrueAcr_b,
        family = "poisson",
        period = TruePeriod,
        beta.group = TRUE
      )
      object <- cglmm(
        Y ~ group + amp_acro(times,
          n_components = 1,
          group = "group",
          period = 12
        ),
        data = comod,
        family = poisson
      )
    }
  )
  testthat::expect_true(all.equal(
    f_round(object$coefficients),
    c(1.0032, -0.4886, 1.9980, 0.9941, 3.0031, 0.2965)
  ))

  # test parameter estimation of Gamma(link="log") simulated data
  withr::with_seed(
    50,
    {
      comod <- simulate_cosinor(
        n = 10000,
        mesor = TrueMesor_a,
        amp = TrueAmp_a,
        acro = TrueAcr_a,
        beta.mesor = TrueMesor_b,
        beta.amp = TrueAmp_b,
        beta.acro = TrueAcr_b,
        family = "gamma",
        period = TruePeriod,
        beta.group = TRUE
      )
      object <- cglmm(
        Y ~ group + amp_acro(times,
          n_components = 1,
          group = "group",
          period = 12
        ),
        data = comod,
        family = Gamma(link = "log")
      )
    }
  )
  testthat::expect_true(all.equal(
    f_round(object$coefficients),
    c(1.0002, -0.4996, 2.0156, 0.9755, 2.9891, 0.3179)
  ))


  # test parameter estimation of binomial simulated data
  withr::with_seed(
    50,
    {
      comod <- simulate_cosinor(
        n = 10000,
        mesor = TrueMesor_a,
        amp = TrueAmp_a,
        acro = TrueAcr_a,
        beta.mesor = TrueMesor_b,
        beta.amp = TrueAmp_b,
        beta.acro = TrueAcr_b,
        family = "binomial",
        period = TruePeriod,
        beta.group = TRUE
      )
      object <- cglmm(
        Y ~ group + amp_acro(times,
          n_components = 1,
          group = "group",
          period = 12
        ),
        data = comod,
        family = binomial
      )
    }
  )
  testthat::expect_true(all.equal(
    f_round(object$coefficients),
    c(0.9773, -0.4527, 1.9399, 1.0482, 3.0007, 0.2849)
  ))
})
test_that("model output is class cglmm", {
  withr::with_seed(
    50,
    {
      data(vitamind)
      object <- cglmm(
        Y ~ X + amp_acro(time, group = "X", period = 12),
        data = vitamind
      )
      expect_true(inherits(object, "cglmm"))

      object <- cglmm(
        Y ~ X + amp_acro(time,
          group = "X",
          period = 12
        ),
        data = vitamind,
        dispformula = ~ 0 + amp_acro(time, group = "X", period = 12),
        ziformula = ~ 0 + amp_acro(time, group = "X", period = 12)
      )
      testthat::expect_no_error(object)
      testthat::expect_snapshot_output(print(object, digits = 2))
      testthat::expect_true(inherits(object, "cglmm"))

      #' @srrstats {RE7.2}
      #' @srrstats {RE7.3}

      # check if the column names from vitamind are present in object_cols
      vitamind_cols <- colnames(vitamind)
      object_cols <- colnames(object$newdata)
      testthat::expect_true(all(vitamind_cols %in% object_cols))

      # test that coefficients and formula can be accessed from object
      testthat::expect_no_error(coefficients(object))
      testthat::expect_no_error(formula(object))

      # testing mixed model specification
      f <- function() {
        cglmm(Y ~ X + amp_acro(time,
          n_components = 1,
          group = "X",
          period = TruePeriod
        ) + (1 | X) + (0 + amp_acro1 | X), data = vitamind)
      }
      testthat::expect_no_error(f)

      sim_data <- simulate_cosinor(
        n = 500,
        mesor = 5,
        amp = c(2, 1),
        acro = c(1, 1.5),
        beta.mesor = 2,
        beta.amp = c(2, 1),
        beta.acro = c(1, 1.5),
        family = "gaussian",
        period = c(12, 6),
        n_components = 2,
        beta.group = TRUE,
      )

      suppressWarnings({
        object <- cglmm(
          Y ~ group + amp_acro(times,
            n_components = 2,
            group = "group",
            period = c(6, 12)
          ) +
            (0 + amp_acro2 | group),
          data = sim_data,
          family = gaussian
        )
      })

      testthat::expect_equal(
        ignore_attr = TRUE,
        object$formula,
        Y ~ group + group:main_rrr1 + group:main_sss1 + group:main_rrr2 +
          group:main_sss2 + (0 + main_rrr2 + main_sss2 | group)
      )
      testthat::expect_snapshot_output(print(object, digits = 2))
    }
  )
})


test_that("mixed model estimates parameters well", {
  f_sample_id <- function(id_num,
                          n = 30,
                          mesor = rnorm(1),
                          amp = c(1, rnorm(n = 1, mean = 5, sd = 1)),
                          acro = c(1, rnorm(n = 1, mean = pi, sd = 1)),
                          family = "gaussian",
                          sd = 0.2,
                          period = c(12, 6),
                          n_components = 2,
                          beta.group = TRUE) {
    data <- simulate_cosinor(
      n = n,
      mesor = mesor,
      amp = amp,
      acro = acro,
      family = family,
      sdv = sd,
      period = period,
      n_components = n_components
    )

    data$id <- id_num
    data
  }
  withr::with_seed(42, {
    df_mixed <- do.call("rbind", lapply(1:10, f_sample_id))
  })

  f <- function() {
    object <- cglmm(
      Y ~ amp_acro(times,
        n_components = 2,
        period = c(6, 12)
      ) +
        (0 + amp_acro2 | id),
      data = dplyr::mutate(df_mixed, id = as.factor(id)),
      family = gaussian
    )
  }

  withr::with_seed(42, {
    df_mixed <- do.call("rbind", lapply(1:75, f_sample_id))
  })

  f <- function() {
    object <- cglmm(
      Y ~ amp_acro(times,
        n_components = 2,
        period = c(12, 6)
      ) +
        (0 + amp_acro2 | id),
      data = dplyr::mutate(df_mixed, id = as.factor(id)),
      family = gaussian
    )
  }

  expect_s3_class(f(), "cglmm")
})


test_that("alternative inputs work", {
  testthat::expect_no_error(cglmm(
    Y ~ amp_acro(time, group = "X", period = 12),
    data = vitamind
  ))
  testthat::expect_no_error(cglmm(
    Y ~ amp_acro(time, group = X, period = 12),
    data = vitamind
  ))
})
