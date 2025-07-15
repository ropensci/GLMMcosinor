test_that("polar_plot input checks work", {
  withr::local_seed(50)

  object <- cglmm(
    vit_d ~ amp_acro(time, group = "X", period = 12),
    data = vitamind
  )
  object_bad <- lm(vit_d ~ X, data = vitamind)

  expect_error(
    polar_plot(object = object_bad),
    regex = "no applicable method",
    fixed = TRUE
  )

  object <- cglmm(
    vit_d ~ amp_acro(time, group = "X", period = 12),
    data = vitamind
  )

  expect_error(
    polar_plot(object, quietly = "true"),
    regex = "'quietly' must a logical argument, either TRUE or FALSE",
    fixed = TRUE
  )

  object <- cglmm(
    vit_d ~ amp_acro(time, group = "X", period = 12),
    data = vitamind
  )

  expect_error(
    polar_plot(object, radial_units = "angle"),
    regex = 'should be one of "radians", "degrees", "period"', fixed = TRUE
  )

  object <- cglmm(
    vit_d ~ amp_acro(time, group = "X", period = 12),
    data = vitamind
  )

  expect_error(
    polar_plot(object, clockwise = 10),
    regex = "'clockwise' must be a logical argument, either TRUE or FALSE",
    fixed = TRUE
  )

  object <- cglmm(
    vit_d ~ amp_acro(time, group = "X", period = 12),
    data = vitamind
  )

  expect_error(
    polar_plot(object, text_size = -1),
    regex = "'text_size' must be a number greater than 0", fixed = TRUE
  )

  object <- cglmm(
    vit_d ~ amp_acro(time, group = "X", period = 12),
    data = vitamind
  )

  expect_error(
    polar_plot(object, text_opacity = 2),
    regex = "'text_opacity' must be a number between 0 and 1 inclusive",
    fixed = TRUE
  )

  object <- cglmm(
    vit_d ~ amp_acro(time, group = "X", period = 12),
    data = vitamind
  )

  expect_error(
    polar_plot(object, ellipse_opacity = 2),
    regex = "'ellipse_opacity' must be a number between 0 and 1 inclusive",
    fixed = TRUE
  )

  object <- cglmm(
    vit_d ~ amp_acro(time, group = "X", period = 12),
    data = vitamind
  )

  expect_error(
    polar_plot(object, component_index = 2),
    regex = paste(
      "'component_index' must be an integer between 1",
      "and n_components (total number of components in model)",
      "inclusive"
    ),
    fixed = TRUE
  )

  object <- cglmm(
    vit_d ~ amp_acro(time, group = "X", period = 12),
    data = vitamind
  )

  expect_error(
    polar_plot(object, circle_linetype = as.factor(2)),
    regex = paste(
      "'circle_linetype' must be a character or numeric.",
      "See ?linetype for more details"
    ),
    fixed = TRUE
  )

  vdiffr::expect_doppelganger(
    "test polar plot configuration 1",
    polar_plot(object, ci_level = 0.9)
  )
  vdiffr::expect_doppelganger(
    "test polar plot configuration 2",
    polar_plot(object, n_breaks = 5)
  )
  vdiffr::expect_doppelganger(
    "test polar plot configuration 3",
    polar_plot(object, make_cowplot = FALSE)
  )
  vdiffr::expect_doppelganger(
    "test polar plot configuration 4",
    polar_plot(object, overlay_parameter_info = TRUE)
  )
  vdiffr::expect_doppelganger(
    "test polar plot configuration 5",
    polar_plot(object, fill_colors = c("blue", "red"))
  )
  vdiffr::expect_doppelganger(
    "test polar plot configuration 6",
    polar_plot(object, radial_units = "degrees")
  )
  vdiffr::expect_doppelganger(
    "test polar plot configuration 7",
    polar_plot(object, radial_units = "period")
  )
  vdiffr::expect_doppelganger(
    "test polar plot configuration 8",
    polar_plot(object, view = "zoom")
  )
  vdiffr::expect_doppelganger(
    "test polar plot configuration 9",
    polar_plot(object, view = "zoom_origin")
  )

  object <- cglmm(
    vit_d ~ amp_acro(time, group = "X", period = 12),
    data = vitamind
  )

  expect_error(
    polar_plot(object, start = "side"),
    regex = 'should be one of "right", "left", "top", "bottom"',
    fixed = TRUE
  )


  object <- cglmm(
    vit_d ~ amp_acro(time, group = "X", period = 12),
    data = vitamind
  )

  expect_error(
    polar_plot(object, view = "all"),
    regex = 'should be one of "full", "zoom", "zoom_origin"',
    fixed = TRUE
  )

  object <- cglmm(
    vit_d ~ amp_acro(time, group = "X", period = 12),
    data = vitamind
  )

  expect_error(
    polar_plot(object, overlay_parameter_info = 2),
    regex = paste(
      "'overlay_parameter_info' must be a logical argument,",
      "either TRUE or FALSE"
    ),
    fixed = TRUE
  )
})

test_that("simple multicomponent model", {
  d_multi_comp <- readRDS(test_path("fixtures", "d_multi_comp.rds"))

  object <- cglmm(
    Y ~ group + amp_acro(time_col = "times", n_components = 2, group = "group", period = c(12, 24)),
    data = d_multi_comp
  )
  vdiffr::expect_doppelganger(
    "plot with multi-component",
    polar_plot(object)
  )
})

test_that("polar_plot works without grouping", {
  object <- cglmm(vit_d ~ amp_acro(time, period = 12), data = vitamind)
  polar_plot(object)
  vdiffr::expect_doppelganger(
    "polar_plot-without grouping",
    polar_plot(object)
  )
})


test_that("polar_plot messages work", {
  object <- cglmm(
    vit_d ~ amp_acro(time, group = "X", period = 12),
    data = vitamind
  )

  suppressMessages(expect_message(
    polar_plot(object, quietly = FALSE),
    regex = "Angle in units of radians", fixed = TRUE
  ))
})

# TODO: this would be the test to assess whether the (non-implemented) ability
# to fit a model with two groups interacting on the same component works
# test_that("simple multigroup (same period) model", {
#
#   d_multi_grp_same_period <- readRDS(test_path("fixtures", "d_multi_grp_same_period.rds"))
#
#   object <- cglmm(
#     Y ~ group +
#       amp_acro(time_col = "times", n_components = 1, group = "g1", period = 24) +
#       amp_acro(time_col = "times", n_components = 1, group = "g2", period = 24),
#     data = d_multi_grp_same_period
#   )
#
#   vdiffr::expect_doppelganger(
#     "plot with multi-grp on same component",
#     polar_plot(object)
#   )
# })
