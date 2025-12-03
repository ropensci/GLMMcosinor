#' Generates a polar plot with elliptical confidence intervals
#'
#' @param x An object of class \code{cglmm}
#' @param ci_level The level for calculated confidence ellipses.
#' Defaults to 0.95.
#' @param n_breaks The number of concentric circles that will be plotted using
#' the \code{scales::breaks_pretty()} function. By default, 5 breaks will be
#' used. The number of breaks may be adjusted to result in an even interval.
#' For example, if n_breaks is 3, but the maximum plot radius is 8, instead of
#' plotting circles in intervals in 1.6, this interval will be rounded to 2
#' to result in the sequence: 0, 2, 4, 6, 8. See \code{?scales::breaks_pretty}
#' for more details.
#' @param component_index A number that corresponds to a particular component
#' from the \code{cglmm()} object that will be used to create polar plot.
#' If missing (default), then plots for all components will be arranged in the
#' returned plot. If a single or multiple values are provided, then these
#' components will be returned. (for example \code{component_index = 1},
#' \code{component_index = c(1, 3)}).
#' @param grid_angle_segments An \code{integer}. Determines the total number of
#' segments in the background of the polar plot. For example, a value of 4 will
#' create quadrants around the origin. Defaults to 8.
#' @param radial_units A \code{character} specifying the angular units of the
#' plot. Possible values are one of \code{c('radians', 'degrees', 'period')}.
#' These units relate to the period of the component being visualized.
#' \describe{
#'   \item{\code{'radians'}: \eqn{[0, 2\pi]}}{}
#'   \item{\code{'degrees'}: \eqn{[0, 360]}}{}
#'   \item{\code{'period'}: \eqn{[0, period]}}{}
#'  }
#' @param clockwise A \code{logical}. If \code{TRUE}, the angles increase in a
#' clockwise fashion. If \code{FALSE}, anti-clockwise. Defaults to \code{FALSE}.
#' @param text_size A number controlling the font size of the text labels.
#' Defaults to 3.
#' @param text_opacity A \code{numeric} between 0 and 1 inclusive that
#' controls the opacity of the text labels.
#' @param fill_colors A \code{character} vector containing colors that will
#' be mapped to levels within a group. If the model has components with
#' different number of levels per factor, the length of this input should match
#' the greatest number of levels. If not, or if the number of levels exceeds the
#' length of the default argument (8), colors are generated using
#' \code{rainbow()}.
#' @param ellipse_opacity A \code{numeric} between 0 and 1 inclusive that
#' controls the opacity of the confidence ellipses. Defaults to 0.3.
#' @param circle_linetype A \code{character} or \code{numeric} that determines
#' the \code{linetype} of the radial circles in background of the polar plot.
#' See \code{?linetype}
#' for more details.
#' @param xlims A vector of length two containing the limits for the x-axis.
#' @param ylims A vector of length two containing the limits for the y-axis.
#' @param start A \code{character}, within
#' \code{c("right", "left", "top", "bottom")} that determines where angle 0 is
#' located. If \code{start = "top"}, and \code{clockwise = TRUE}, the angle
#' will rotate clockwise, starting at the '12 o-clock' position on a clock.
#' @param view A \code{character}, within
#' \code{c("full", "zoom", "zoom_origin")} that controls the view of the plots.
#' \describe{
#'   \item{\code{'full'}: maintains a full view of the polar plot, including
#'   the background radial circles.}{}
#'   \item{\code{'zoom'}: finds the minimum view window which contains all
#'   confidence ellipses.}{}
#'   \item{\code{'zoom_origin'}: zooms into the confidence ellipses (like
#'   "zoom"), but also keeps the origin within frame.}{}
#' }
#' @param overlay_parameter_info A \code{logical} argument. If \code{TRUE},
#' more information about the acrophase and amplitude are displayed on the
#' polar plots.
#' @param quietly Analogous to verbose, this \code{logical} argument controls
#' whether messages are displayed in the console.
#' @param show_component_labels Logical argument, TRUE by default. When TRUE,
#' the polar plots have labels corresponding to their components.
#' @param ... Additional, ignored arguments.
#'
#' @srrstats {G1.4}
#'
#' @return Returns a \code{ggplot} object.
#' @export
#'
#' @examples
#' data(vitamind)
#' model <- cglmm(
#'   vit_d ~ X + amp_acro(time, group = "X", period = 12),
#'   data = vitamind
#' )
#' polar_plot(model, radial_units = "period")
polar_plot <- function(
  x,
  ci_level = 0.95,
  n_breaks = 5,
  component_index = NULL,
  grid_angle_segments = 8,
  radial_units = c("radians", "degrees", "period"),
  clockwise = FALSE,
  text_size = 3,
  text_opacity = 0.5,
  fill_colors,
  ellipse_opacity = 0.3,
  circle_linetype = "dotted",
  start = c("right", "left", "top", "bottom"),
  view = c("full", "zoom", "zoom_origin"),
  overlay_parameter_info = FALSE,
  quietly = TRUE,
  show_component_labels = TRUE,
  xlims,
  ylims,
  ...
) {
  UseMethod("polar_plot")
}

#' Generates a polar plot with elliptical confidence intervals
#'
#' @param x An object of class \code{cglmm}
#' @param ci_level The level for calculated confidence ellipses.
#' Defaults to 0.95.
#' @param n_breaks The number of concentric circles that will be plotted using
#' the \code{scales::breaks_pretty()} function. By default, 5 breaks will be
#' used. The number of breaks may be adjusted to result in an even interval.
#' For example, if n_breaks is 3, but the maximum plot radius is 8, instead of
#' plotting circles in intervals in 1.6, this interval will be rounded to 2
#' to result in the sequence: 0, 2, 4, 6, 8. See \code{?scales::breaks_pretty}
#' for more details.
#' @param component_index A number that corresponds to a particular component
#' from the \code{cglmm()} object that will be used to create polar plot.
#' If missing (default), then plots for all components will be arranged in the
#' returned plot. If a single or multiple values are provided, then these
#' components will be returned. (for example \code{component_index = 1},
#' \code{component_index = c(1, 3)}).
#' @param grid_angle_segments An \code{integer}. Determines the total number of
#' segments in the background of the polar plot. For example, a value of 4 will
#' create quadrants around the origin. Defaults to 8.
#' @param radial_units A \code{character} specifying the angular units of the
#' plot. Possible values are one of \code{c('radians', 'degrees', 'period')}.
#' These units relate to the period of the component being visualized.
#' \describe{
#'   \item{\code{'radians'}: \eqn{[0, 2\pi]}}{}
#'   \item{\code{'degrees'}: \eqn{[0, 360]}}{}
#'   \item{\code{'period'}: \eqn{[0, period]}}{}
#'  }
#' @param clockwise A \code{logical}. If \code{TRUE}, the angles increase in a
#' clockwise fashion. If \code{FALSE}, anti-clockwise. Defaults to \code{FALSE}.
#' @param text_size A number controlling the font size of the text labels.
#' Defaults to 3.
#' @param text_opacity A \code{numeric} between 0 and 1 inclusive that
#' controls the opacity of the text labels.
#' @param fill_colors A \code{character} vector containing colors that will
#' be mapped to levels within a group. If the model has components with
#' different number of levels per factor, the length of this input should match
#' the greatest number of levels. If not, or if the number of levels exceeds the
#' length of the default argument (8), colors are generated using
#' \code{rainbow()}.
#' @param ellipse_opacity A \code{numeric} between 0 and 1 inclusive that
#' controls the opacity of the confidence ellipses. Defaults to 0.3.
#' @param circle_linetype A \code{character} or \code{numeric} that determines
#' the \code{linetype} of the radial circles in background of the polar plot.
#' See \code{?linetype}
#' for more details.
#' @param xlims A vector of length two containing the limits for the x-axis.
#' @param ylims A vector of length two containing the limits for the y-axis.
#' @param start A \code{character}, within
#' \code{c("right", "left", "top", "bottom")} that determines where angle 0 is
#' located. If \code{start = "top"}, and \code{clockwise = TRUE}, the angle
#' will rotate clockwise, starting at the '12 o-clock' position on a clock.
#' @param view A \code{character}, within
#' \code{c("full", "zoom", "zoom_origin")} that controls the view of the plots.
#' \describe{
#'   \item{\code{'full'}: maintains a full view of the polar plot, including
#'   the background radial circles.}{}
#'   \item{\code{'zoom'}: finds the minimum view window which contains all
#'   confidence ellipses.}{}
#'   \item{\code{'zoom_origin'}: zooms into the confidence ellipses (like
#'   "zoom"), but also keeps the origin within frame.}{}
#' }
#' @param overlay_parameter_info A \code{logical} argument. If \code{TRUE},
#' more information about the acrophase and amplitude are displayed on the
#' polar plots.
#' @param quietly Analogous to verbose, this \code{logical} argument controls
#' whether messages are displayed in the console.
#' @param show_component_labels Logical argument, TRUE by default. When TRUE,
#' the polar plots have labels corresponding to their components.
#' @param ... Additional, ignored arguments.
#'
#' @srrstats {G1.4}
#'
#' @return Returns a \code{ggplot} object.
#' @export
#'
#' @examples
#' model <- cglmm(
#'   vit_d ~ X + amp_acro(time, group = "X", period = 12),
#'   data = vitamind
#' )
#' polar_plot(model, radial_units = "period")
polar_plot.cglmm <- function(
  x,
  ci_level = 0.95,
  n_breaks = 5,
  component_index = NULL,
  grid_angle_segments = 8,
  radial_units = c(
    "radians",
    "degrees",
    "period"
  ),
  clockwise = FALSE,
  text_size = 3.5,
  text_opacity = 1,
  fill_colors,
  ellipse_opacity = 0.3,
  circle_linetype = "dotted",
  start = c(
    "right",
    "left",
    "top",
    "bottom"
  ),
  view = c(
    "full",
    "zoom",
    "zoom_origin"
  ),
  overlay_parameter_info = FALSE,
  quietly = TRUE,
  show_component_labels = TRUE,
  xlims,
  ylims,
  ...
) {
  # checking the quality of inputs
  assertthat::assert_that(
    inherits(x, "cglmm"),
    msg = "'x' must be of class cglmm"
  )

  validate_ci_level(ci_level)
  radial_units <- match.arg(radial_units)
  start <- match.arg(start)
  view <- match.arg(view)

  assertthat::assert_that(
    grid_angle_segments == floor(grid_angle_segments) & grid_angle_segments > 0,
    msg = "'grid_angle_segments' must be an integer greater than 0"
  )
  assertthat::assert_that(
    is.logical(quietly),
    msg = "'quietly' must a logical argument, either TRUE or FALSE"
  )
  assertthat::assert_that(
    is.logical(clockwise),
    msg = "'clockwise' must be a logical argument, either TRUE or FALSE "
  )

  assertthat::assert_that(
    is.logical(show_component_labels),
    msg = paste(
      "'show_component_labels' must be a",
      " logical argument, either TRUE or FALSE"
    )
  )

  assertthat::assert_that(
    is.numeric(text_size) & text_size > 0,
    msg = "'text_size' must be a number greater than 0"
  )
  assertthat::assert_that(
    is.numeric(text_opacity) & text_opacity >= 0 & text_opacity <= 1,
    msg = "'text_opacity' must be a number between 0 and 1 inclusive"
  )
  assertthat::assert_that(
    is.numeric(ellipse_opacity) & ellipse_opacity >= 0 & ellipse_opacity <= 1,
    msg = "'ellipse_opacity' must be a number between 0 and 1 inclusive"
  )

  if (!missing(component_index)) {
    assertthat::assert_that(
      all(component_index == floor(component_index)) &
        all(component_index > 0) &
        all(component_index <= x$n_components),
      msg = paste(
        "'component_index' must be an integer between 1 and",
        "n_components (total number of components in model)",
        "inclusive"
      )
    )
  }

  if (!missing(xlims)) {
    assertthat::assert_that(
      length(xlims) == 2 & is.numeric(xlims) & xlims[1] < xlims[2],
      msg = paste(
        "'xlims' must be a vector with the first element being the",
        "lower x coordinate, and the second being the upper",
        "x coordinate"
      )
    )
    xlims_check <- TRUE
  } else {
    xlims_check <- FALSE
  }

  if (!missing(ylims)) {
    assertthat::assert_that(
      length(xlims) == 2 & is.numeric(ylims) & ylims[1] < ylims[2],
      msg = paste(
        "'ylims' must be a vector with the first element being the",
        "lower y coordinate, and the second being the upper",
        "y coordinate"
      )
    )
    ylims_check <- TRUE
  } else {
    ylims_check <- FALSE
  }

  assertthat::assert_that(
    is.character(circle_linetype) || is.numeric(circle_linetype),
    msg = paste(
      "'circle_linetype' must be a character or numeric. See ?linetype",
      "for more details"
    )
  )
  assertthat::assert_that(
    is.logical(overlay_parameter_info),
    msg = paste(
      "'overlay_parameter_info' must be a logical argument,",
      "either TRUE or FALSE"
    )
  )

  # get summary statistics of cglmm object
  sum <- summary(x, ci_level = ci_level)

  # convert user input for zoom level into logical arguments
  if (view == "full") {
    zoom <- FALSE
    zoom_origin <- FALSE
  } else if (view == "zoom_origin") {
    zoom <- TRUE
    zoom_origin <- TRUE
  } else if (view == "zoom") {
    zoom <- TRUE
    zoom_origin <- FALSE
  }

  # check if there is a contour argument & store this check in local environment
  n_components <- x$n_components

  fill_colors_check <- !missing(fill_colors)
  # set direction of increasing angle based on user input of clockwise argument
  direction <- ifelse(clockwise, -1, 1)

  # convert user input for starting position (on Cartesian plane) into logical
  # arguments by default, ggplot() ellipse and circle functions use unit circle
  # angles where 0 degrees starts at the '3pm' position and rotates
  # counterclockwise. However, the ggforce function geom_arc() starts at upwards
  # 12pm position, also rotating coutnerclockwise. Hence, offset, and
  # overlay_param_offset are different to ultimately describe the same
  # angle position
  if (start == "top") {
    offset <- pi / 2
    overlay_start <- 0
  } else if (start == "left") {
    offset <- pi
    overlay_start <- -pi / 2
  } else if (start == "bottom") {
    offset <- 3 * pi / 2
    overlay_start <- pi
  } else if (start == "right") {
    offset <- 0
    overlay_start <- pi / 2
  }

  if (!is.null(component_index)) {
    make_cowplot <- FALSE
  } else {
    make_cowplot <- TRUE
  }

  # remove component labels if there is only one component
  if (n_components == 1) {
    show_component_labels <- FALSE
  }

  # plot multiple component plots in cowplot or plot a single component plot
  get_sub_component_polar_plot <- function(component) {
    sub_ggplot.cglmm.polar(
      comp = component,
      x = x,
      sum = sum,
      direction = direction,
      offset = offset,
      radial_units = radial_units,
      grid_angle_segments = grid_angle_segments,
      n_breaks = n_breaks,
      zoom = zoom,
      circle_linetype = circle_linetype,
      text_size = text_size,
      text_opacity = text_opacity,
      ellipse_opacity = ellipse_opacity,
      overlay_parameter_info = overlay_parameter_info,
      fill_colors_check = fill_colors_check,
      xlims_check = xlims_check,
      xlims = xlims,
      ylims_check = ylims_check,
      ylims = ylims,
      quietly = quietly,
      overlay_start = overlay_start,
      fill_colors = fill_colors,
      zoom_origin = zoom_origin
    )
  }

  if (make_cowplot == TRUE & n_components > 1) {
    plot_list <- NULL
    for (i in seq_len(n_components)) {
      plot_obj <- get_sub_component_polar_plot(i)
      assign(paste0("plot_obj", i), plot_obj)

      # show labels for each component
      if (show_component_labels) {
        plot_obj <- plot_obj + ggplot2::ggtitle(paste("Component", i))
      }

      plot_list[[i]] <- ggplot2::ggplotGrob(plot_obj)
    }

    final_obj <- cowplot::plot_grid(
      plotlist = plot_list
    )
    final_obj
  }
  if (make_cowplot == TRUE & n_components == 1) {
    plot_list <- NULL
    for (i in seq_len(n_components)) {
      plot_obj <- get_sub_component_polar_plot(i)
      assign(paste0("plot_obj", i), plot_obj)
      # show labels for each component
      if (show_component_labels) {
        plot_obj <- plot_obj + ggplot2::ggtitle(paste("Component", i))
      }
      plot_list[[i]] <- ggplot2::ggplotGrob(plot_obj)
    }
    final_obj <- cowplot::plot_grid(
      plotlist = plot_list
    )
    final_obj
  }
  if (make_cowplot == FALSE) {
    if (length(component_index) == 1) {
      plot_obj <- get_sub_component_polar_plot(component_index)

      # show labels
      if (show_component_labels) {
        final_obj <- plot_obj +
          ggplot2::ggtitle(paste("Component", component_index))
      } else {
        final_obj <- plot_obj
      }
      final_obj
    } else {
      plot_list <- NULL
      for (i in component_index) {
        plot_obj <- get_sub_component_polar_plot(i)
        assign(paste0("plot_obj", i), plot_obj)
        # show labels for each component
        if (show_component_labels) {
          plot_obj <- plot_obj + ggplot2::ggtitle(paste("Component", i))
        }

        plot_list[[i]] <- ggplot2::ggplotGrob(plot_obj)
      }

      final_obj <- cowplot::plot_grid(
        plotlist = plot_list,
        labels = NULL
      )

      final_obj
    }
  }

  final_obj
}
