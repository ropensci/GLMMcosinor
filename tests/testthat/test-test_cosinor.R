#' @srrstats {G5.0}
#' @srrstats {G5.0}
#' @srrstats {G5.1}
#' @srrstats {G5.2}
#' @srrstats {G5.2a}
#' @srrstats {G5.2b}

test_that("script works and warnings are displayed appropriately", {
  # Test the class output
  data(vitamind)
  object <- cosinor.glmm(Y ~ amp_acro(time,
                                      group = "X",
                                      period = 12),
                         data = vitamind)
  test_object <- test_cosinor_levels(object, x_str = "X")
  testthat::expect_true(inherits(test_object, "test_cosinor"))

  # Test the comparison_type variable, and test the print output
  object <- cosinor.glmm(Y ~ amp_acro(time,
                                      group = "X",
                                      n_components = 2,
                                      period = c(12, 11)),
                         data = vitamind)

  expect_no_error(test_cosinor_components(object,
                                          x_str = "X",
                                          level_index = 1,
                                          comparison_A = 1,
                                          comparison_B = 2))

  expect_no_error(print(test_cosinor_components(object,
                                                x_str = "X",
                                                level_index = 1,
                                                comparison_A = 1,
                                                comparison_B = 2)))


  # Test a simple input
  data(vitamind)
  object <- cosinor.glmm(Y ~ amp_acro(time,
                                      group = "X",
                                      period = 12),
                         data = vitamind)
  f <- function() {
    test_cosinor_levels(object, x_str = "X")
  }
  expect_no_error(f)

  # Testing error messages
  # Error message test 1
  data(vitamind)
  object <- cosinor.glmm(Y ~ amp_acro(time,
                                      group = "X",
                                      period = 12),
                         data = vitamind)
  f <- function() {
    test_cosinor_levels(object, x_str = 10)
  }
  expect_error(
    f(),
    regex = "is.character(x_str) is not TRUE", fixed = TRUE
  )

  # Error message test 2
  data(vitamind)
  object <- cosinor.glmm(Y ~ amp_acro(time,
                                      group = "X",
                                      period = 12),
                         data = vitamind)
  f <- function() {
    test_cosinor_levels(object, x_str = "Y")
  }
  expect_error(
    f(),
    regex = "x_str must be the name of a group in object", fixed = TRUE
  )

  # Error message test 3
  data(vitamind)
  object <- cosinor.glmm(Y ~ amp_acro(time,
                                      group = "X",
                                      period = 12),
                         data = vitamind)
  f <- function() {
    test_cosinor_levels(object,
                        x_str = "X",
                        param = "phase")
  }
  expect_error(
    f(),
    regex = "Invalid parameter. Expected 'amp' or 'acr'", fixed = TRUE
  )

  # Error message test 4
  data(vitamind)
  object <- cosinor.glmm(Y ~ amp_acro(time,
                                      group = "X",
                                      period = 12),
                         data = vitamind)
  f <- function() {
    test_cosinor_levels(object, x_str = "X", param = "phase")
  }
  expect_error(
    f(),
    regex = "Expected 'amp' or 'acr'.", fixed = TRUE
  )

  # Error message test 5
  data(vitamind)
  object <- cosinor.glmm(Y ~ amp_acro(time,
                                      group = "X",
                                      period = 12),
                         data = vitamind)
  f <- function() {
    test_cosinor_levels(object,
                        x_str = "X",
                        comparison_A = 4, )
  }
  expect_error(
    f(),
    regex = "'comparison_A' must correspond to a level within the group
    specified by 'x_str'", fixed = TRUE
  )

  # Error message test 6
  data(vitamind)
  object <- cosinor.glmm(Y ~ amp_acro(time,
                                      group = "X",
                                      period = 12),
                         data = vitamind)
  f <- function() {
    test_cosinor_components(object,
                            x_str = "X",
                            comparison_A = 4)
  }
  expect_error(
    f(),
    regex = "'comparison_A' and 'comparison_B' must be numbers corresponding to a component in the model", fixed = TRUE
  )



  # Error message test 7
  data(vitamind)
  object <- cosinor.glmm(Y ~ amp_acro(time,
                                      group = "X",
                                      period = 12),
                         data = vitamind)
  f <- function() {
    test_cosinor_levels(object,
                        x_str = "X",
                        component_index = 10)
  }
  expect_error(
    f(),
    regex = "'component_index' must be supplied and it must be a number corresponding to a component in the model", fixed = TRUE
  )

  # Error message test 9
  data(vitamind)
  object <- cosinor.glmm(Y ~ amp_acro(time,
                                      group = "X",
                                      n_components = 2,
                                      period = c(12, 11)),
                         data = vitamind)
  f <- function() {
    test_cosinor_components(object,
                            x_str = "X",
                            level_index = 10,
                            comparison_A = 1,
                            comparison_B = 2)
  }
  expect_error(
    f(),
    regex = "'level_index' must be supplied and it must be a number corresponding to a level in the model", fixed = TRUE
  )


  # Error message test 10
  data(vitamind)
  obj <- test_cosinor_levels(object, x_str = "X")
  expect_true(inherits(obj, "test_cosinor"))
})

test_that("multi-component comparison works, print functions work", {
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
      comod <- simulate_cosinor(10000,
        mesor = c(TrueMesor_a),
        amp = c(TrueAmp_a, TrueAmp_a),
        acro = c(TrueAcr_a, TrueAcr_a),
        beta.mesor = c(TrueMesor_b),
        beta.amp = c(TrueAmp_b, TrueAmp_b),
        beta.acro = c(TrueAcr_b, TrueAcr_b),
        family = "gaussian",
        n_components = 2,
        period = c(10, 12),
        beta.group = TRUE
      )
      object <- cosinor.glmm(Y ~ group + amp_acro(times,
                                                  n_components = 2,
                                                  group = "group",
                                                  period = c(10, 12)),
                             data = comod)
    }
  )

  f <- function() {
    test_cosinor_components(object,
      x_str = "group",
      comparison_A = 1,
      comparison_B = 2,
      level_index = 1
    )
  }
  testthat::expect_no_error(f)
  testthat::expect_snapshot_output(print(object, digits = 2))
})
