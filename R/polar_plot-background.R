# generates the background grid for the polar plot
#' Create the background for a polar plot.
#'
#' @param n_breaks The number of concentric circles that will be plotted using
#' the \code{scales::breaks_pretty()} function. By default, 5 breaks will be
#' used. The number of breaks may be adjusted to result in an even interval.
#' For example, if n_breaks is 3, but the maximum plot radius is 8, instead of
#' plotting circles in intervals in 1.6, this interval will be rounded to 2
#' to result in the sequence: 0, 2, 4, 6, 8. See \code{?scales::breaks_pretty}
#' for more details.
#' @param max_plot_radius The maximum radius of the background
#' @param circle_linetype The linetype of the concentric rings.
#' @param dial_pos_full_x Dimensions of the background.
#' @param dial_pos_full_y Dimensions of the background.
#' @param time_labels Labels for the tick marks (time).
#' @param text_size Size of label size.
#' @param text_opacity Opactiy of labels.
#' @param contour_labels Labels to use for the concentric rings (amplitude).
#' @param grid_angle_segments How many segments to split the plot into.
#'
#' @returns a \code{ggplot2} object.
#'
#' @examples
#' GLMMcosinor:::get_background_grid(
#'   n_breaks = 5,
#'   max_plot_radius = 10,
#'   circle_linetype = "dotted",
#'   dial_pos_full_x = c(10, 7.08, 0.008, -7.1, -10, -7.05, -0.02, 7.09, 10),
#'   dial_pos_full_y = c(0, 7.07, 10, 7.04, 0.016, -7.09, -10, -7.06, -0.03),
#'   time_labels = c(0, 1.5, 3, 4.5, 6, 7.5, 9, 10.5, 12),
#'   text_size = 3.5,
#'   text_opacity = 1,
#'   contour_labels = c(0, 2, 4, 6, 8, 10),
#'   grid_angle_segments = 8
#' )
get_background_grid <- function(
  n_breaks,
  max_plot_radius,
  circle_linetype,
  dial_pos_full_x,
  dial_pos_full_y,
  time_labels,
  text_size,
  text_opacity,
  contour_labels,
  grid_angle_segments
) {
  ggplot2::ggplot() +
    ggforce::geom_circle(
      # plots the background circles
      ggplot2::aes(
        x0 = 0,
        y0 = 0,
        r = scales::breaks_pretty(n = n_breaks)(c(0, max_plot_radius))
      ),
      alpha = 0.01,
      linetype = circle_linetype
    ) +
    ggplot2::geom_segment(
      # plots the background grid
      ggplot2::aes(
        x = dial_pos_full_x,
        y = dial_pos_full_y,
        xend = -dial_pos_full_x,
        yend = -dial_pos_full_y
      ),
      linetype = 10,
      alpha = 0.4
    ) +
    ggplot2::geom_text(
      # adds the radial labels (amplitude)
      ggplot2::aes(label = time_labels[-length(time_labels)]),
      x = 1.05 * dial_pos_full_x[-length(dial_pos_full_x)],
      y = 1.05 * dial_pos_full_y[-length(dial_pos_full_y)],
      size = text_size,
      alpha = text_opacity
    ) +
    ggplot2::geom_text(
      # adds the angle labels (acrophase)
      ggplot2::aes(
        label = contour_labels,
        x = contour_labels * (cos(pi / grid_angle_segments)),
        y = contour_labels * (sin(pi / grid_angle_segments))
      ),
      size = text_size,
      alpha = text_opacity
    ) +
    ggplot2::guides(colour = "none") +
    ggplot2::theme(
      axis.title.x = ggplot2::element_blank(),
      axis.title.y = ggplot2::element_blank(),
      axis.text.x = ggplot2::element_blank(),
      axis.text.y = ggplot2::element_blank(),
      axis.ticks = ggplot2::element_blank(),
      panel.grid.major = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank()
    )
}
