test_that("polar_plot input checks work", {
  # Test 1
  data(vitamind)
  object <- cosinor.glmm(Y ~ amp_acro(time, group = "X", period = 12), data = vitamind)
  object_bad <- lm(Y ~ X, data = vitamind)
  f <- function() {
    polar_plot(object = object_bad)
  }
  expect_error(
    f(),
    regex = "no applicable method", fixed = TRUE
  )

  # Test 2
  data(vitamind)
  object <- cosinor.glmm(Y ~ amp_acro(time, group = "X", period = 12), data = vitamind)
  f <- function() {
    polar_plot(object, contour_interval = -1)
  }
  expect_error(
    f(),
    regex = "'contour_interval' must be a number greater than 0", fixed = TRUE
  )

  # Test 3
  data(vitamind)
  object <- cosinor.glmm(Y ~ amp_acro(time, group = "X", period = 12), data = vitamind)
  f <- function() {
    polar_plot(object, quietly = "true")
  }
  expect_error(
    f(),
    regex = "'quietly' must a logical argument, either TRUE or FALSE", fixed = TRUE
  )

  # Test 4
  data(vitamind)
  object <- cosinor.glmm(Y ~ amp_acro(time, group = "X", period = 12), data = vitamind)
  f <- function() {
    polar_plot(object, radial_units = "angle")
  }
  expect_error(
    f(),
    regex = 'should be one of "radians", "degrees", "period"', fixed = TRUE
  )

  # Test 5
  data(vitamind)
  object <- cosinor.glmm(Y ~ amp_acro(time, group = "X", period = 12), data = vitamind)
  f <- function() {
    polar_plot(object, clockwise = 10)
  }
  expect_error(
    f(),
    regex = "'clockwise' must be a logical argument, either TRUE or FALSE", fixed = TRUE
  )

  # Test 6
  data(vitamind)
  object <- cosinor.glmm(Y ~ amp_acro(time, group = "X", period = 12), data = vitamind)
  f <- function() {
    polar_plot(object, text_size = -1)
  }
  expect_error(
    f(),
    regex = "'text_size' must be a number greater than 0", fixed = TRUE
  )

  # Test 7
  data(vitamind)
  object <- cosinor.glmm(Y ~ amp_acro(time, group = "X", period = 12), data = vitamind)
  f <- function() {
    polar_plot(object, text_opacity = 2)
  }
  expect_error(
    f(),
    regex = "'text_opacity' must be a number between 0 and 1 inclusive", fixed = TRUE
  )

  # Test 8
  data(vitamind)
  object <- cosinor.glmm(Y ~ amp_acro(time, group = "X", period = 12), data = vitamind)
  f <- function() {
    polar_plot(object, ellipse_opacity = 2)
  }
  expect_error(
    f(),
    regex = "'ellipse_opacity' must be a number between 0 and 1 inclusive", fixed = TRUE
  )

  # Test 10
  data(vitamind)
  object <- cosinor.glmm(Y ~ amp_acro(time, group = "X", period = 12), data = vitamind)
  f <- function() {
    polar_plot(object, component_index = 2)
  }
  expect_error(
    f(),
    regex = "'component_index' must be an integer between 1 and n_components (total number of components in model) inclusive", fixed = TRUE
  )

  # Test 11
  data(vitamind)
  object <- cosinor.glmm(Y ~ amp_acro(time, group = "X", period = 12), data = vitamind)
  f <- function() {
    polar_plot(object, circle_linetype = 2)
  }
  expect_error(
    f(),
    regex = "'circle_linetype' must be a character. See ?linetype for more details", fixed = TRUE
  )

  expect_no_error(polar_plot(object, ci_level = 0.9))
  expect_no_error(polar_plot(object, contour_interval = 0.5))
  expect_no_error(polar_plot(object, make_cowplot = FALSE))
  expect_no_error(polar_plot(object, overlay_parameter_info = TRUE))
  expect_no_error(polar_plot(object, fill_colours = c("blue", "red")))
  expect_no_error(polar_plot(object, radial_units = "degrees"))
  expect_no_error(polar_plot(object, radial_units = "period"))
  expect_no_error(polar_plot(object, view = "zoom"))
  expect_no_error(polar_plot(object, view = "zoom_origin"))
  # # Test 12
  # data(vitamind)
  # object <- cosinor.glmm(Y ~ amp_acro(time, group = "X"), data = vitamind)
  # f <- function() {
  #   polar_plot(object, fill_colours = 2)
  # }
  # expect_error(
  #   f(),
  #   regex = "fill_colours must be of class character, and must be a valid colour", fixed = TRUE
  # )

  # Test 13
  data(vitamind)
  object <- cosinor.glmm(Y ~ amp_acro(time, group = "X", period = 12), data = vitamind)
  f <- function() {
    polar_plot(object, start = "side")
  }
  expect_error(
    f(),
    regex = 'should be one of "right", "left", "top", "bottom"', fixed = TRUE
  )

  # Test 14
  data(vitamind)
  object <- cosinor.glmm(Y ~ amp_acro(time, group = "X", period = 12), data = vitamind)
  f <- function() {
    polar_plot(object, view = "all")
  }
  expect_error(
    f(),
    regex = 'should be one of "full", "zoom", "zoom_origin"', fixed = TRUE
  )

  # Test 15
  data(vitamind)
  object <- cosinor.glmm(Y ~ amp_acro(time, group = "X", period = 12), data = vitamind)
  f <- function() {
    polar_plot(object, overlay_parameter_info = 2)
  }
  expect_error(
    f(),
    regex = "'overlay_parameter_info' must be a logical argument, either TRUE or FALSE", fixed = TRUE
  )
})



test_that("polar_plot messages work", {
  # Test 1
  data(vitamind)
  object <- cosinor.glmm(Y ~ amp_acro(time, group = "X", period = 12), data = vitamind)
  f <- function() {
    polar_plot(object, quietly = FALSE)
  }

  suppressMessages(expect_message(
    f(),
    regex = "Angle in units of radians", fixed = TRUE
  ))
})
