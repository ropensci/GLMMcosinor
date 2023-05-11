#' @srrstats {G5.0} *Where applicable or practicable, tests should use standard data sets with known properties (for example, the [NIST Standard Reference Datasets](https://www.itl.nist.gov/div898/strd/), or data sets provided by other widely-used R packages).*
#' @srrstats {G5.1} *Data sets created within, and used to test, a package should be exported (or otherwise made generally available) so that users can confirm tests and run examples.*
#' @srrstats {G5.2} *Appropriate error and warning behaviour of all functions should be explicitly demonstrated through tests. In particular,*
#' @srrstats {G5.2a} *Every message produced within R code by `stop()`, `warning()`, `message()`, or equivalent should be unique*
#' @srrstats {G5.2b} *Explicit tests should demonstrate conditions which trigger every one of those messages, and should compare the result with expected values.*


test_that("polar_plot input checks work", {
  # Test 1
  data(vitamind)
  object <- cosinor.glmm(Y ~ amp.acro(time, group = "X"), data = vitamind)
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
  object <- cosinor.glmm(Y ~ amp.acro(time, group = "X"), data = vitamind)
  f <- function() {
    polar_plot(object, contour_interval = -1)
  }
  expect_error(
    f(),
    regex = "'contour_interval' must be a number greater than 0", fixed = TRUE
  )

  # Test 3
  data(vitamind)
  object <- cosinor.glmm(Y ~ amp.acro(time, group = "X"), data = vitamind)
  f <- function() {
    polar_plot(object, quietly = "true")
  }
  expect_error(
    f(),
    regex = "'quietly' must a logical argument, either TRUE or FALSE", fixed = TRUE
  )

  # Test 4
  data(vitamind)
  object <- cosinor.glmm(Y ~ amp.acro(time, group = "X"), data = vitamind)
  f <- function() {
    polar_plot(object, radial_units = "angle")
  }
  expect_error(
    f(),
    regex = "'radial_units' must be either 'radians', 'degrees', or 'period'", fixed = TRUE
  )

  # Test 5
  data(vitamind)
  object <- cosinor.glmm(Y ~ amp.acro(time, group = "X"), data = vitamind)
  f <- function() {
    polar_plot(object, clockwise = 10)
  }
  expect_error(
    f(),
    regex = "'clockwise' must be a logical argument, either TRUE or FALSE", fixed = TRUE
  )

  # Test 6
  data(vitamind)
  object <- cosinor.glmm(Y ~ amp.acro(time, group = "X"), data = vitamind)
  f <- function() {
    polar_plot(object, text_size = -1)
  }
  expect_error(
    f(),
    regex = "'text_size' must be a number greater than 0", fixed = TRUE
  )

  # Test 7
  data(vitamind)
  object <- cosinor.glmm(Y ~ amp.acro(time, group = "X"), data = vitamind)
  f <- function() {
    polar_plot(object, text_opacity = 2)
  }
  expect_error(
    f(),
    regex = "'text_opacity' must be a number between 0 and 1 inclusive", fixed = TRUE
  )

  # Test 8
  data(vitamind)
  object <- cosinor.glmm(Y ~ amp.acro(time, group = "X"), data = vitamind)
  f <- function() {
    polar_plot(object, ellipse_opacity = 2)
  }
  expect_error(
    f(),
    regex = "'ellipse_opacity' must be a number between 0 and 1 inclusive", fixed = TRUE
  )

  # Test 9
  data(vitamind)
  object <- cosinor.glmm(Y ~ amp.acro(time, group = "X"), data = vitamind)
  f <- function() {
    polar_plot(object, make_cowplot = 2)
  }
  expect_error(
    f(),
    regex = "'make_cowplot' must be a logical argument, either TRUE or FALSE", fixed = TRUE
  )

  # Test 10
  data(vitamind)
  object <- cosinor.glmm(Y ~ amp.acro(time, group = "X"), data = vitamind)
  f <- function() {
    polar_plot(object, component_index = 2)
  }
  expect_error(
    f(),
    regex = "'component_index' must be an integer between 1 and n_components (total number of components in model) inclusive", fixed = TRUE
  )

  # Test 11
  data(vitamind)
  object <- cosinor.glmm(Y ~ amp.acro(time, group = "X"), data = vitamind)
  f <- function() {
    polar_plot(object, circle_linetype = 2)
  }
  expect_error(
    f(),
    regex = "'circle_linetype' must be a character. See ?linetype for more details", fixed = TRUE
  )

  #testing various inputs
  expect_no_error(autoplot(object, superimpose.data = TRUE))
  expect_no_error(autoplot(object, predict.ribbon = TRUE))

  expect_no_error(polar_plot(object, ci_level = 0.9))
  expect_no_error(polar_plot(object, contour_interval = 0.5))
  expect_no_error(polar_plot(object, make_cowplot = FALSE))
  expect_no_error(polar_plot(object, overlay_parameter_info = TRUE))
  expect_no_error(polar_plot(object, fill_colours = c('blue','red')))
  expect_no_error(polar_plot(object, radial_units = "degrees"))
  expect_no_error(polar_plot(object, radial_units = "period"))
  expect_no_error(polar_plot(object, view = "zoom"))
  expect_no_error(polar_plot(object, view = "zoom_origin"))
  # # Test 12
  # data(vitamind)
  # object <- cosinor.glmm(Y ~ amp.acro(time, group = "X"), data = vitamind)
  # f <- function() {
  #   polar_plot(object, fill_colours = 2)
  # }
  # expect_error(
  #   f(),
  #   regex = "fill_colours must be of class character, and must be a valid colour", fixed = TRUE
  # )

  # Test 13
  data(vitamind)
  object <- cosinor.glmm(Y ~ amp.acro(time, group = "X"), data = vitamind)
  f <- function() {
    polar_plot(object, start = "side")
  }
  expect_error(
    f(),
    regex = "'start' argument must be either 'right', 'left', 'bottom', or 'top'", fixed = TRUE
  )

  # Test 14
  data(vitamind)
  object <- cosinor.glmm(Y ~ amp.acro(time, group = "X"), data = vitamind)
  f <- function() {
    polar_plot(object, view = "all")
  }
  expect_error(
    f(),
    regex = "'view' argument must be either 'full', 'zoom', or 'zoom_origin'", fixed = TRUE
  )

  # Test 15
  data(vitamind)
  object <- cosinor.glmm(Y ~ amp.acro(time, group = "X"), data = vitamind)
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
  object <- cosinor.glmm(Y ~ amp.acro(time, group = "X"), data = vitamind)
  f <- function() {
    polar_plot(object, quietly = FALSE)
  }
  suppressMessages(expect_message(
    capture.output(f()),
    regex = "Circular contours every 1 unit(s)", fixed = TRUE
  ))

  suppressMessages(expect_message(
    f(),
    regex = "Angle in units of radians", fixed = TRUE
  ))
})

test_that("plot produces error messages", {
  data(vitamind)
  object <- cosinor.glmm(Y ~ 1 + amp.acro(time, group = "X"), data = vitamind)

  # Test 1
  data(vitamind)
  object <- cosinor.glmm(Y ~ 1 + amp.acro(time, group = "X"), data = vitamind)
  f <- function() {
    autoplot(object, x_str = 10)
  }

  expect_error(
    f(),
    regex = "'x_str' must be string corresponding to a group name in cosinor.glmm object", fixed = TRUE
  )

  # Test 2
  data(vitamind)
  vitamind$Z <- rbinom(length(vitamind$X), 3, prob = 0.5)
  object <- cosinor.glmm(Y ~ X + amp.acro(time, n_components = 3, group = c("Z", NA, "X"), period = c(12, 10, 8)), data = vitamind)

  f <- function() {
    autoplot(object, x_str = c("X", "Z"))
  }

  expect_no_error(f())

  f <- function() {
    autoplot(object)
  }

  # Test 3
  expect_no_error(f())

  # Test 4
  data(vitamind)
  object <- cosinor.glmm(Y ~ 1 + amp.acro(time, group = "X"), data = vitamind)
  f <- function() {
    autoplot(object, type = 20)
  }

  expect_error(
    f(),
    regex = "'type' must be a string. See type in ?predict for more information about valid inputs", fixed = TRUE
  )

  # Test 5
  data(vitamind)
  object <- cosinor.glmm(Y ~ 1 + amp.acro(time, group = "X"), data = vitamind)
  f <- function() {
    autoplot(object, xlims = c(2, 1))
  }

  expect_error(
    f(),
    regex = "'xlims' must be a vector with the first element being the lower x coordinate, and the second being the upper x coordinate", fixed = TRUE
  )

  # Test 6
  data(vitamind)
  object <- cosinor.glmm(Y ~ 1 + amp.acro(time, group = "X"), data = vitamind)
  f <- function() {
    autoplot(object, pred.length.out = 100.5)
  }

  expect_error(
    f(),
    regex = "'pred.length.out' must be an integer greater than 0", fixed = TRUE
  )

  # Test 7
  data(vitamind)
  object <- cosinor.glmm(Y ~ 1 + amp.acro(time, group = "X"), data = vitamind)
  f <- function() {
    autoplot(object, superimpose.data = 10)
  }

  expect_error(
    f(),
    regex = "'superimpose.data' must be a logical argument, either TRUE or FALSE", fixed = TRUE
  )

  # Test 8
  data(vitamind)
  object <- cosinor.glmm(Y ~ 1 + amp.acro(time, group = "X"), data = vitamind)
  f <- function() {
    autoplot(object, data_opacity = 1.5)
  }

  expect_error(
    f(),
    regex = "data_opacity' must be a number between 0 and 1 inclusive", fixed = TRUE
  )

  # Test 9
  data(vitamind)
  object <- cosinor.glmm(Y ~ 1 + amp.acro(time, group = "X"), data = vitamind)
  f <- function() {
    autoplot(object, predict.ribbon = 10)
  }

  expect_error(
    f(),
    regex = "'predict.ribbon' must be a logical argument, either TRUE or FALSE", fixed = TRUE
  )
})


# test_that("plot gives appropriate plot output", {
#  # Test 1
#  data(vitamind)
#  object <- cosinor.glmm(Y ~ 1 + amp.acro(time, group = "X"), data = vitamind)
#  f <- function() {
#    plot(object)
#  }
#  vdiffr::expect_doppelganger("vitamind_plot", f)
#
#  # Test 2
#  data(vitamind)
#  object <- cosinor.glmm(Y ~ 1 + amp.acro(time, group = NA), data = vitamind)
#  f <- function() {
#    plot(object)
#  }
#  vdiffr::expect_doppelganger("vitamind_plot_no_group", f)
#
#  # Test 3
#  data(vitamind)
#  object <- cosinor.glmm(Y ~ 1 + amp.acro(time, group = "X"), data = vitamind)
#  f <- function() {
#    plot(object, superimpose.data = TRUE)
#  }
#  vdiffr::expect_doppelganger("vitamind_plot_si_data", f)
#
#  # Test 5
#  data(vitamind)
#  object <- cosinor.glmm(Y ~ 1 + amp.acro(time, group = "X"), data = vitamind)
#  f <- function() {
#    plot(object, superimpose.data = TRUE, predict.ribbon = TRUE)
#  }
#  vdiffr::expect_doppelganger("vitamind_plot_si_data_and_pred_ribbon", f)
# })
