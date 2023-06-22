#' @srrstats {G5.0} *Where applicable or practicable, tests should use standard data sets with known properties (for example, the [NIST Standard Reference Datasets](https://www.itl.nist.gov/div898/strd/), or data sets provided by other widely-used R packages).*
#' @srrstats {G5.1} *Data sets created within, and used to test, a package should be exported (or otherwise made generally available) so that users can confirm tests and run examples.*
#' @srrstats {G5.2} *Appropriate error and warning behaviour of all functions should be explicitly demonstrated through tests. In particular,*
#' @srrstats {G5.2a} *Every message produced within R code by `stop()`, `warning()`, `message()`, or equivalent should be unique*
#' @srrstats {G5.2b} *Explicit tests should demonstrate conditions which trigger every one of those messages, and should compare the result with expected values.*
#' @srrstats {G5.5} *Correctness tests should be run with a fixed random seed*
#' @srrstats {G5.4} **Correctness tests** *to test that statistical algorithms produce expected results to some fixed test data sets (potentially through comparisons using binding frameworks such as [RStata](https://github.com/lbraglia/RStata)).*
#' @srrstats {G5.4a} *For new methods, it can be difficult to separate out correctness of the method from the correctness of the implementation, as there may not be reference for comparison. In this case, testing may be implemented against simple, trivial cases or against multiple implementations such as an initial R implementation compared with results from a C/C++ implementation.*
#' @srrstats {G5.6} **Parameter recovery tests** *to test that the implementation produce expected results given data with known properties. For instance, a linear regression algorithm should return expected coefficient values for a simulated data set generated from a linear model.*
#' @srrstats {G5.6a} *Parameter recovery tests should generally be expected to succeed within a defined tolerance rather than recovering exact values.*
#' @srrstats {G5.6b} *Parameter recovery tests should be run with multiple random seeds when either data simulation or the algorithm contains a random component. (When long-running, such tests may be part of an extended, rather than regular, test suite; see G4.10-4.12, below).*
#' @srrstats {G5.9b} *Running under different random seeds or initial conditions does not meaningfully change results*
#' @srrstats {G2.11} *Software should ensure that `data.frame`-like tabular objects which have columns which do not themselves have standard class attributes (typically, `vector`) are appropriately processed, and do not error without reason. This behaviour should be tested. Again, columns created by the [`units` package](https://github.com/r-quantities/units/) provide a good test case.*



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
      object <- cosinor.glmm(
        Y ~ group + amp_acro(times, n_components = 1, group = "group", period = 12),
        data = comod
      )
    }
  )

  testthat::expect_true(all.equal(
    f_round(object$coefficients),
    c(1.0030, -0.4966, 2.0181, 0.9858, -2.9907, -0.2885)
  ))

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
      object <- cosinor.glmm(
        Y ~ group + amp_acro(times, n_components = 1, group = "group", period = 12),
        data = comod
      )
    }
  )

  testthat::expect_true(all.equal(
    f_round(object$coefficients),
    c(0.9905, -0.4932, 1.9785, 1.0203, -2.9900, -0.2906)
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
      object <- cosinor.glmm(
        Y ~ group + amp_acro(times, n_components = 1, group = "group", period = 12),
        data = comod,
        family = poisson
      )
    }
  )

  testthat::expect_true(all.equal(
    f_round(object$coefficients),
    c(0.9980, -0.4849, 1.9973, 0.9947, -3.0045, -0.2858)
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
      object <- cosinor.glmm(
        Y ~ group + amp_acro(times, n_components = 1, group = "group", period = 12),
        data = comod,
        family = Gamma(link = "log")
      )
    }
  )

  testthat::expect_true(all.equal(
    f_round(object$coefficients),
    c(1.0002, -0.4996, 2.0089, 0.9698, -3.0127, -0.2995)
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
      object <- cosinor.glmm(
        Y ~ group + amp_acro(times, n_components = 1, group = "group", period = 12),
        data = comod,
        family = binomial
      )
    }
  )

  testthat::expect_true(all.equal(
    f_round(object$coefficients),
    c(0.9822, -0.4799, 1.9368, 1.0394, -2.9841, -0.3197)
  ))
})
test_that("model output is class cosinor.glmm", {
  withr::with_seed(
    50,
    {
      data(vitamind)
      object <- cosinor.glmm(
        Y ~ X + amp_acro(time, group = "X", period = 12),
        data = vitamind
      )
      expect_true(inherits(object, "cosinor.glmm"))

      object <- cosinor.glmm(Y ~ X + amp_acro(time, group = "X", period = 12),
        data = vitamind,
        dispformula = ~ 0 + amp_acro(time, group = "X", period = 12),
        ziformula = ~ 0 + amp_acro(time, group = "X", period = 12)
      )
      testthat::expect_no_error(object)
      testthat::expect_snapshot_output(print(object, digits = 2))
      testthat::expect_true(inherits(object, "cosinor.glmm"))

      #' @srrstats {RE7.2} Demonstrate that output objects retain aspects of input data such as row or case names (see **RE1.3**).
      #' @srrstats {RE7.3} Demonstrate and test expected behaviour when objects returned from regression software are submitted to the accessor methods of **RE4.2**--**RE4.7**.

      # check if the column names from vitamind are present in object_cols
      vitamind_cols <- colnames(vitamind)
      object_cols <- colnames(object$newdata)
      testthat::expect_true(all(vitamind_cols %in% object_cols))

      # test that coefficients and formula can be accessed from object
      testthat::expect_no_error(coefficients(object))
      testthat::expect_no_error(formula(object))

      # testing mixed model specification
      f <- function() {
        cosinor.glmm(Y ~ X + amp_acro(time,
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
        object <- cosinor.glmm(
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
      sd = sd,
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
    object <- cosinor.glmm(
      Y ~ amp_acro(times,
        n_components = 2,
        period = c(6, 12)
      ) +
        (0 + amp_acro2 | id),
      data = dplyr::mutate(df_mixed, id = as.factor(id)),
      family = gaussian
    )
  }

  suppressWarnings(expect_warning(f()))

  withr::with_seed(42, {
    df_mixed <- do.call("rbind", lapply(1:75, f_sample_id))
  })

  f <- function() {
    object <- cosinor.glmm(
      Y ~ amp_acro(times,
        n_components = 2,
        period = c(12, 6)
      ) +
        (0 + amp_acro2 | id),
      data = dplyr::mutate(df_mixed, id = as.factor(id)),
      family = gaussian
    )
  }

  expect_s3_class(f(), "cosinor.glmm")
})


test_that("alternative inputs work", {
  testthat::expect_no_error(cosinor.glmm(Y ~ amp_acro(time, group = "X", period = 12), data = vitamind))
  testthat::expect_no_error(cosinor.glmm(Y ~ amp_acro(time, group = X, period = 12), data = vitamind))
#  testthat::expect_error(cosinor.glmm(Y ~ amp_acro(time, group = X, period = 12), data = units::set_units#(vitamind$Y, "units")),
#                         regexp = "'data' must be of class 'data.frame', 'matrix', or 'tibble'")

})
