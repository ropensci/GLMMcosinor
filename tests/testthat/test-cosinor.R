#' @srrstats {G5.0} *Where applicable or practicable, tests should use standard data sets with known properties (for example, the [NIST Standard Reference Datasets](https://www.itl.nist.gov/div898/strd/), or data sets provided by other widely-used R packages).*
#' @srrstats {G5.1} *Data sets created within, and used to test, a package should be exported (or otherwise made generally available) so that users can confirm tests and run examples.*
#' @srrstats {G5.2} *Appropriate error and warning behaviour of all functions should be explicitly demonstrated through tests. In particular,*
#' @srrstats {G5.2a} *Every message produced within R code by `stop()`, `warning()`, `message()`, or equivalent should be unique*
#' @srrstats {G5.2b} *Explicit tests should demonstrate conditions which trigger every one of those messages, and should compare the result with expected values.*
#' @srrstats {G5.5} *Correctness tests should be run with a fixed random seed*



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

  # test parameter estimation of guassian simulated data
  withr::with_seed(
    50,
    {
      comod <- simulate_cosinor(10000, mesor = TrueMesor_a, amp = TrueAmp_a, acro = TrueAcr_a, beta.mesor = TrueMesor_b, beta.amp = TrueAmp_b, beta.acro = TrueAcr_b, family = "gaussian")
      object <- cosinor.glmm(Y ~ group + amp.acro(times, n_components = 1, group = "group", period = c(12)), data = comod)
    }
  )

  testthat::expect_equal(f_round(object$coefficients[1]), 1.0030)
  testthat::expect_equal(f_round(object$coefficients[2]), -0.4966)
  testthat::expect_equal(f_round(object$coefficients[3]), 2.0181)
  testthat::expect_equal(f_round(object$coefficients[4]), 0.9858)
  testthat::expect_equal(f_round(object$coefficients[5]), 2.9907)
  testthat::expect_equal(f_round(object$coefficients[6]), 0.2885)

  # test parameter estimation of poisson simulated data
  withr::with_seed(
    50,
    {
      comod <- simulate_cosinor(10000, mesor = TrueMesor_a, amp = TrueAmp_a, acro = TrueAcr_a, beta.mesor = TrueMesor_b, beta.amp = TrueAmp_b, beta.acro = TrueAcr_b, family = "poisson")
      object <- cosinor.glmm(Y ~ group + amp.acro(times, n_components = 1, group = "group", period = c(12)), data = comod, family = poisson)
    }
  )

  testthat::expect_equal(f_round(object$coefficients[1]), 0.9980)
  testthat::expect_equal(f_round(object$coefficients[2]), -0.4849)
  testthat::expect_equal(f_round(object$coefficients[3]), 1.9973)
  testthat::expect_equal(f_round(object$coefficients[4]), 0.9947)
  testthat::expect_equal(f_round(object$coefficients[5]), 3.0045)
  testthat::expect_equal(f_round(object$coefficients[6]), 0.2858)

  # test parameter estimation of Gamma(link="log") simulated data
  withr::with_seed(
    50,
    {
      comod <- simulate_cosinor(10000, mesor = TrueMesor_a, amp = TrueAmp_a, acro = TrueAcr_a, beta.mesor = TrueMesor_b, beta.amp = TrueAmp_b, beta.acro = TrueAcr_b, family = "gamma")
      object <- cosinor.glmm(Y ~ group + amp.acro(times, n_components = 1, group = "group", period = c(12)), data = comod, family = Gamma(link = "log"))
    }
  )

  testthat::expect_equal(f_round(object$coefficients[1]), 1.0028)
  testthat::expect_equal(f_round(object$coefficients[2]), -0.4999)
  testthat::expect_equal(f_round(object$coefficients[3]), 2.0004)
  testthat::expect_equal(f_round(object$coefficients[4]), 1.0045)
  testthat::expect_equal(f_round(object$coefficients[5]), 3.0063)
  testthat::expect_equal(f_round(object$coefficients[6]), 0.2951)


  # test parameter estimation of binomial simulated data
  withr::with_seed(
    50,
    {
      comod <- simulate_cosinor(10000, mesor = TrueMesor_a, amp = TrueAmp_a, acro = TrueAcr_a, beta.mesor = TrueMesor_b, beta.amp = TrueAmp_b, beta.acro = TrueAcr_b, family = "binomial")
      object <- cosinor.glmm(Y ~ group + amp.acro(times, n_components = 1, group = "group", period = c(12)), data = comod, family = binomial)
    }
  )

  testthat::expect_equal(f_round(object$coefficients[1]), 0.9822)
  testthat::expect_equal(f_round(object$coefficients[2]), -0.4799)
  testthat::expect_equal(f_round(object$coefficients[3]), 1.9368)
  testthat::expect_equal(f_round(object$coefficients[4]), 1.0394)
  testthat::expect_equal(f_round(object$coefficients[5]), 2.9841)
  testthat::expect_equal(f_round(object$coefficients[6]), 0.3197)
})
test_that("model output is class cosinor.glmm", {
  withr::with_seed(
    50,
    {
      data(vitamind)
      object <- cosinor.glmm(Y ~ X + amp.acro(time, group = "X"), data = vitamind)
      expect_true(inherits(object, "cosinor.glmm"))

      object <- cosinor.glmm(Y ~ X + amp.acro(time, group = "X"),
        data = vitamind,
        dispformula = ~ 0 + amp.acro(time, group = "X"),
        ziformula = ~ 0 + amp.acro(time, group = "X")
      )
      testthat::expect_no_error(object)
      testthat::expect_snapshot_output(print(object, digits = 2))
      testthat::expect_true(inherits(object, "cosinor.glmm"))

      #' @srrstats {RE7.2} Demonstrate that output objects retain aspects of input data such as row or case names (see **RE1.3**).
      #' @srrstats {RE7.3} Demonstrate and test expected behaviour when objects returned from regression software are submitted to the accessor methods of **RE4.2**--**RE4.7**.
      #' @noRd

      # check if the column names from vitamind are present in object_cols
      vitamind_cols <- colnames(vitamind)
      object_cols <- colnames(object$newdata)
      testthat::expect_true(all(vitamind_cols %in% object_cols))

      # test that coefficients and formula can be accessed from object
      testthat::expect_no_error(coefficients(object))
      testthat::expect_no_error(formula(object))

      # testing mixed model specification
      f <- function() {
        cosinor.glmm(Y ~ X + amp.acro(time,
          n_components = 1,
          group = "X",
          period = c(12)
        ) + (1 | X) + (0 + amp.acro1 | X), data = vitamind)
      }
      testthat::expect_no_error(f)

      sim_data <- simulate_cosinor(500,
        mesor = 5,
        amp = c(2, 1),
        acro = c(1, 1.5),
        beta.mesor = 2,
        beta.amp = c(2, 1),
        beta.acro = c(1, 1.5),
        family = "gaussian",
        period = c(12, 6),
        n_components = 2,
        beta.group = TRUE
      )
      object <- cosinor.glmm(
        Y ~ group + amp.acro(times,
          n_components = 2,
          group = "group",
          period = c(6, 12)
        ) +
          (amp.acro1 | 1) + (0 + amp.acro2 | group),
        data = sim_data,
        family = gaussian
      )

      testthat::expect_snapshot_output(print(object, digits = 2))
    }
  )
})
