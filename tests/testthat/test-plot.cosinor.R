test_that("ggplot.cosinor.glmm.polar input checks work", {
  #Test 1
  data(vitamind)
  object <- cosinor.glmm(Y ~  amp.acro(time, group = "X"), data = vitamind)
  object_bad <- lm(Y ~ X, data = vitamind)
  f <- function() {ggplot.cosinor.glmm.polar(object = object_bad)}
  expect_error(
    f(),
    regex = 'object must be of class cosinor.glmm', fixed = TRUE
  )

  #Test 2
  data(vitamind)
  object <- cosinor.glmm(Y ~  amp.acro(time, group = "X"), data = vitamind)
  f <- function() {ggplot.cosinor.glmm.polar(object, contour_interval = -1)}
  expect_error(
    f(),
    regex = 'contour_interval must be a number greater than 0', fixed = TRUE
  )

  #Test 3
  data(vitamind)
  object <- cosinor.glmm(Y ~  amp.acro(time, group = "X"), data = vitamind)
  f <- function() {ggplot.cosinor.glmm.polar(object, quietly = "true")}
  expect_error(
    f(),
    regex = 'quietly must a logical argument, either TRUE or FALSE', fixed = TRUE
  )

  #Test 4
  data(vitamind)
  object <- cosinor.glmm(Y ~  amp.acro(time, group = "X"), data = vitamind)
  f <- function() {ggplot.cosinor.glmm.polar(object, radial_units = 'angle')}
  expect_error(
    f(),
    regex = "radial_units must be either 'radians', 'degrees', or 'period'", fixed = TRUE
  )

  #Test 5
  data(vitamind)
  object <- cosinor.glmm(Y ~  amp.acro(time, group = "X"), data = vitamind)
  f <- function() {ggplot.cosinor.glmm.polar(object, clockwise = 10)}
  expect_error(
    f(),
    regex = "clockwise must be a logical argument, either TRUE or FALSE", fixed = TRUE
  )

  #Test 6
  data(vitamind)
  object <- cosinor.glmm(Y ~  amp.acro(time, group = "X"), data = vitamind)
  f <- function() {ggplot.cosinor.glmm.polar(object, text_size = -1)}
  expect_error(
    f(),
    regex = "text_size must be a number greater than 0", fixed = TRUE
  )

  #Test 7
  data(vitamind)
  object <- cosinor.glmm(Y ~  amp.acro(time, group = "X"), data = vitamind)
  f <- function() {ggplot.cosinor.glmm.polar(object, text_opacity = 2)}
  expect_error(
    f(),
    regex = "text_opacity must be a number between 0 and 1 inclusive", fixed = TRUE
  )

  #Test 8
  data(vitamind)
  object <- cosinor.glmm(Y ~  amp.acro(time, group = "X"), data = vitamind)
  f <- function() {ggplot.cosinor.glmm.polar(object, ellipse_opacity = 2)}
  expect_error(
    f(),
    regex = "ellipse_opacity must be a number between 0 and 1 inclusive", fixed = TRUE
  )

  #Test 9
  data(vitamind)
  object <- cosinor.glmm(Y ~  amp.acro(time, group = "X"), data = vitamind)
  f <- function() {ggplot.cosinor.glmm.polar(object, make_cowplot = 2)}
  expect_error(
    f(),
    regex = "make_cowplot must be a logical argument, either TRUE or FALSE", fixed = TRUE
  )

  #Test 10
  data(vitamind)
  object <- cosinor.glmm(Y ~  amp.acro(time, group = "X"), data = vitamind)
  f <- function() {ggplot.cosinor.glmm.polar(object, component_index = 2)}
  expect_error(
    f(),
    regex = "component_index must be an integer between 1 and n_components (total number of components in model) inclusive", fixed = TRUE
  )

  #Test 11
  data(vitamind)
  object <- cosinor.glmm(Y ~  amp.acro(time, group = "X"), data = vitamind)
  f <- function() {ggplot.cosinor.glmm.polar(object, circle_linetype = 2)}
  expect_error(
    f(),
    regex = "circle_linetype must be a character. See ?linetype for more details", fixed = TRUE
  )

  #Test 12
  data(vitamind)
  object <- cosinor.glmm(Y ~  amp.acro(time, group = "X"), data = vitamind)
  f <- function() {ggplot.cosinor.glmm.polar(object, fill_colours = 2)}
  expect_error(
    f(),
    regex = "fill_colours must be of class character, and must be a valid colour", fixed = TRUE
  )

  #Test 13
  data(vitamind)
  object <- cosinor.glmm(Y ~  amp.acro(time, group = "X"), data = vitamind)
  f <- function() {ggplot.cosinor.glmm.polar(object, start = "side")}
  expect_error(
    f(),
    regex = "'start' argument must be either 'right', 'left', 'bottom', or 'top'", fixed = TRUE
  )

  #Test 14
  data(vitamind)
  object <- cosinor.glmm(Y ~  amp.acro(time, group = "X"), data = vitamind)
  f <- function() {ggplot.cosinor.glmm.polar(object, view = "all")}
  expect_error(
    f(),
    regex = "'view' argument must be either 'full', 'zoom', or 'zoom_origin'", fixed = TRUE
  )

  #Test 15
  data(vitamind)
  object <- cosinor.glmm(Y ~  amp.acro(time, group = "X"), data = vitamind)
  f <- function() {ggplot.cosinor.glmm.polar(object, overlay_parameter_info = 2)}
  expect_error(
    f(),
    regex = "overlay_parameter_info must be a logical argument, either TRUE or FALSE", fixed = TRUE
  )
})

test_that("ggplot.cosinor.glmm.polar messages work", {
  #Test 1
  data(vitamind)
  object <- cosinor.glmm(Y ~  amp.acro(time, group = "X"), data = vitamind)
  f <- function() {ggplot.cosinor.glmm.polar(object, quietly = FALSE)}
  suppressMessages(expect_message(
    capture.output(f()),
    regex = "Circular contours every 2.0344 unit(s)", fixed = TRUE
  ))

  suppressMessages(expect_message(
    f(),
    regex = "Angle in units of radians", fixed = TRUE
  ))
})

test_that("ggplot.cosinor.glmm produces error messages", {
  #Test 1
  data(vitamind)
  object <- cosinor.glmm(Y ~ 1 + amp.acro(time, group = "X"), data = vitamind)
  object_bad <- lm(Y ~ X, data = vitamind)
  f <- function(){ggplot.cosinor.glmm(object_bad)}

  expect_error(
    f(),
    regex = "object must be of class cosinor.glmm", fixed = TRUE
  )

  #Test 2
  data(vitamind)
  object <- cosinor.glmm(Y ~ 1 + amp.acro(time, group = "X"), data = vitamind)
  f <- function(){ggplot.cosinor.glmm(object, x_str = 10)}

  expect_error(
    f(),
    regex = "x_str must be string corresponding to a group name in cosinor.glmm object", fixed = TRUE
  )

  #Test 3
  data(vitamind)
  object <- cosinor.glmm(Y ~ 1 + amp.acro(time, group = "X"), data = vitamind)
  f <- function(){ggplot.cosinor.glmm(object, type = 20)}

  expect_error(
    f(),
    regex = "type must be a string. See type in ?predict for more information about valid inputs", fixed = TRUE
  )

  #Test 4
  data(vitamind)
  object <- cosinor.glmm(Y ~ 1 + amp.acro(time, group = "X"), data = vitamind)
  f <- function(){ggplot.cosinor.glmm(object, xlims = c(2,1))}

  expect_error(
    f(),
    regex = "xlims must be a vector with the first element being the lower x coordinate, and the second being the upper x coordinate", fixed = TRUE
  )

  #Test 5
  data(vitamind)
  object <- cosinor.glmm(Y ~ 1 + amp.acro(time, group = "X"), data = vitamind)
  f <- function(){ggplot.cosinor.glmm(object, pred.length.out = 100.5)}

  expect_error(
    f(),
    regex = "pred.length.out must be an integer greater than 0", fixed = TRUE
  )

  #Test 6
  data(vitamind)
  object <- cosinor.glmm(Y ~ 1 + amp.acro(time, group = "X"), data = vitamind)
  f <- function(){ggplot.cosinor.glmm(object, superimpose.data = 10)}

  expect_error(
    f(),
    regex = "superimpose.data must be a logical argument, either TRUE or FALSE", fixed = TRUE
  )

  #Test 7
  data(vitamind)
  object <- cosinor.glmm(Y ~ 1 + amp.acro(time, group = "X"), data = vitamind)
  f <- function(){ggplot.cosinor.glmm(object, data_opacity = 1.5)}

  expect_error(
    f(),
    regex = "data_opacity must be a number between 0 and 1 inclusive", fixed = TRUE
  )

  #Test 8
  data(vitamind)
  object <- cosinor.glmm(Y ~ 1 + amp.acro(time, group = "X"), data = vitamind)
  f <- function(){ggplot.cosinor.glmm(object, predict.ribbon = 10)}

  expect_error(
    f(),
    regex = "predict.ribbon must be a logical argument, either TRUE or FALSE", fixed = TRUE
  )
})
