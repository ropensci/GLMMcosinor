test_that("multiplication works", {
  #Test 1
  data(vitamind)
  object <- cosinor.glmm(Y ~ amp.acro(time, group = "X"), data = vitamind)
  f <- function() {
    test_cosinor(object, x_str = "X")
  }
  expect_no_error(f)

  #Test 2
  data(vitamind)
  object <- cosinor.glmm(Y ~ amp.acro(time, group = "X"), data = vitamind)
  f <- function() {
    test_cosinor(object, x_str = 10)
  }
  expect_error(
    f(),
    regex = "is.character(x_str) is not TRUE", fixed = TRUE
  )

  #Test 3
  data(vitamind)
  object <- cosinor.glmm(Y ~ amp.acro(time, group = "X"), data = vitamind)
  f <- function() {
    test_cosinor(object, x_str = "Y")
  }
  expect_error(
    f(),
    regex = "x_str must be the name of a group in object", fixed = TRUE
  )

  #Test 4
  data(vitamind)
  object <- cosinor.glmm(Y ~ amp.acro(time, group = "X"), data = vitamind)
  f <- function() {
    test_cosinor(object, x_str = "X", param = "phase")
  }
  expect_error(
    f(),
    regex = "'param' must be either 'amp' and 'acr'", fixed = TRUE
  )

  #Test 5
  data(vitamind)
  object <- cosinor.glmm(Y ~ amp.acro(time, group = "X"), data = vitamind)
  f <- function() {
    test_cosinor(object, x_str = "X", param = "phase")
  }
  expect_error(
    f(),
    regex = "'param' must be either 'amp' and 'acr'", fixed = TRUE
  )

  #Test 6
  data(vitamind)
  object <- cosinor.glmm(Y ~ amp.acro(time, group = "X"), data = vitamind)
  f <- function() {
    test_cosinor(object, x_str = "X", comparison_A = 4)
  }
  expect_error(
    f(),
    regex = "'comparison_A' and 'comparison_B' must be numbers corresponding to levels within group specified by 'x_str'", fixed = TRUE
  )

  #Test 7
  data(vitamind)
  object <- cosinor.glmm(Y ~ amp.acro(time, group = "X"), data = vitamind)
  f <- function() {
    test_cosinor(object, x_str = "X", comparison_A = 4, comparison_type = "components")
  }
  expect_error(
    f(),
    regex = "'comparison_A' and 'comparison_B' must be numbers corresponding to a component in the model", fixed = TRUE
  )

  #Test 8
  data(vitamind)
  object <- cosinor.glmm(Y ~ amp.acro(time, group = "X"), data = vitamind)
  f <- function() {
    test_cosinor(object, x_str = "X", comparison_type = "NULL")
  }
  expect_error(
    f(),
    regex = "'comparison_type' must be one of the following strings:'levels', or 'components'", fixed = TRUE
  )

  #Test 9
  data(vitamind)
  object <- cosinor.glmm(Y ~ amp.acro(time, group = "X"), data = vitamind)
  f <- function() {
    test_cosinor(object, x_str = "X", comparison_type = "levels", component_index = 10)
  }
  expect_error(
    f(),
    regex = "'component_index' must be supplied. Ensure that it is a number corresponding to a component in the model", fixed = TRUE
  )

  #Test 10
  data(vitamind)
  object <- cosinor.glmm(Y ~ amp.acro(time, group = "X", n_components = 2, period = c(12,11)), data = vitamind)
  f <- function() {
    test_cosinor(object, x_str = "X", comparison_type = "components", level_index = 10, comparison_A = 1, comparison_B = 2)
  }
  expect_error(
    f(),
    regex = "'level_index' must be supplied. Ensure that it is a number corresponding to a level in the model", fixed = TRUE
  )


})
