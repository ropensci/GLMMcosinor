#' create a (sub) polar plot (iterated over for each component)
#'
#' @param comp Component.
#' @param x cglmm object.
#' @param sum Model summary.
#' @param direction \code{ifelse(clockwise, -1, 1)}.
#' @param offset Applied to start position of phase angle.
#' @param radial_units Units for phase.
#' @param grid_angle_segments Number of sugments.
#' @param n_breaks Number of breaks for amplitude.
#' @param zoom Whether or not it is zoomed in.
#' @param circle_linetype linetype for concentric circles.
#' @param text_size Text size for axes.
#' @param text_opacity Text opacity for axes.
#' @param ellipse_opacity Opacity of confidence ellipses.
#' @param overlay_parameter_info Whether or not to show amplitude/phase
#' estimates.
#' @param fill_colors_check \code{!missing(fill_colors)}.
#' @param xlims_check \code{TRUE} if limits are unspecified.
#' @param xlims User-specified limits.
#' @param ylims_check \code{TRUE} if limits are unspecified.
#' @param ylims User-specified limits.
#' @param quietly Send possibly unnecessary messages to console.
#' @param overlay_start background axes positioning.
#' @param fill_colors Colours for ellipses.
#' @param zoom_origin Zoom position if used.
#'
#' @returns A \code{ggplot2} object.
#'
#' @examples
#' model <- cglmm(
#'   vit_d ~ amp_acro(time_col = time, group = "X", period = 12),
#'   data = vitamind
#' )
#' GLMMcosinor:::sub_ggplot.cglmm.polar(
#'   comp = 1,
#'   x = model,
#'   sum = summary(model),
#'   direction = 1,
#'   offset = 0,
#'   radial_units = "period",
#'   grid_angle_segments = 8,
#'   n_breaks = 5,
#'   zoom = FALSE,
#'   circle_linetype = "dotted",
#'   text_size = 3.5,
#'   text_opacity = 1,
#'   ellipse_opacity = 0.3,
#'   overlay_parameter_info = FALSE,
#'   fill_colors_check = FALSE,
#'   xlims_check = FALSE,
#'   ylims_check = FALSE,
#'   quietly = TRUE,
#'   overlay_start = 0,
#'   zoom_origin = FALSE
#' )
#'
sub_ggplot.cglmm.polar <- function(
  comp,
  x,
  sum,
  direction,
  offset,
  radial_units,
  grid_angle_segments,
  n_breaks,
  zoom,
  circle_linetype,
  text_size,
  text_opacity,
  ellipse_opacity,
  overlay_parameter_info,
  fill_colors_check,
  xlims_check,
  xlims,
  ylims_check,
  ylims,
  quietly,
  overlay_start,
  fill_colors,
  zoom_origin
) {
  # get the component that is going to plotted
  component_index <- comp
  # get the arguments from the function wrapping this function
  args <- match.call()[-1]
  period <- x$period[component_index]
  max_period <- period
  group_check <- (x$group[component_index] != 0)
  if (group_check) {
    x_str <- x$group_original[component_index]
    group <- x_str
    level <- x$group_stats[[group]]

    # create an index that will be used to grab the correct transformed
    # summary stats
    string_index <- paste0("[", group, "=")
    # create an index that grabs the corresponding raw summary stats
    string_index_raw <- paste0(group)
  } else {
    group <- NULL
    string_index <- ""
    string_index_raw <- ""
  }

  amp_index <- paste0("amp", component_index)
  acr_index <- paste0("acr", component_index)

  # grab and store the summary statistics for amp and acr
  amp_row_idx <- which(
    grepl(string_index, rownames(sum$transformed.table), fixed = TRUE) &
      grepl(amp_index, rownames(sum$transformed.table), fixed = TRUE)
  )

  est_amp <- sum$transformed.table$estimate[amp_row_idx]
  l_est_amp <- sum$transformed.table$lower.CI[amp_row_idx]
  u_est_amp <- sum$transformed.table$upper.CI[amp_row_idx]

  acr_row_idx <- which(
    grepl(string_index, rownames(sum$transformed.table), fixed = TRUE) &
      grepl(acr_index, rownames(sum$transformed.table), fixed = TRUE)
  )

  est_acr <- sum$transformed.table$estimate[acr_row_idx]
  l_est_acr <- sum$transformed.table$lower.CI[acr_row_idx]
  u_est_acr <- sum$transformed.table$upper.CI[acr_row_idx]

  # an index of the names of the summary statistics used in iteration of loop
  name_index <- rownames(sum$transformed.table)[amp_row_idx]
  group_level <- array(dim = length(name_index))

  # obtain an index of group levels
  if (group_check) {
    for (i in level) {
      group_ind <- paste0(group, "=", i)
      group_level[which(grepl(group_ind, name_index))] <- paste(i)
    }
  }

  # determine the estimated rrr and sss used parameter estimates
  est_rrr <- est_amp * cos(direction * (est_acr) + offset)
  est_sss <- est_amp * sin(direction * (est_acr) + offset)

  # for confidence ellipses, get the long_axis width "a_trans" (amplitude),
  # and the short-axis height "b_trans" (acrophase)
  # Note that both values are halved because they are technically radii
  a_trans <- est_amp - l_est_amp
  b_trans <- tan((u_est_acr - l_est_acr) / 2) * est_amp

  # determine the maximum radius in a single plot. This will be used
  # for formatting plot features
  max_radius <- max(abs(u_est_amp), abs(l_est_amp))

  # change 'max_period' to correspond to units specified by the user
  # conversion_factor is used to convert from radians (
  # default acrophase output) to whatever radial_units is
  if (radial_units == "radians") {
    max_period <- 2 * pi
    conversion_factor <- 1
  } else if (radial_units == "degrees") {
    max_period <- 360
    conversion_factor <- (1 / (2 * pi)) * 360
  } else if (radial_units == "period") {
    max_period <- max_period
    conversion_factor <- (1 / (2 * pi)) * max_period
  }

  # create a sequence of labels for time (to be inserted around
  # the polar plot)
  time_labels <- signif(
    seq(from = 0, to = max_period, by = max_period / grid_angle_segments),
    3
  )

  # create a sequence of labels for the contours.
  contour_labels <- scales::breaks_pretty(n = n_breaks)(c(0, max_radius))

  # determine largest contour, and use this as a plot limit
  max_plot_radius <- max(contour_labels)

  # convert time_labels to polar coordinates to determine position of
  # where they should be placed
  dial_pos_full_x <- round(
    max_plot_radius *
      cos(
        direction * time_labels * 2 * pi / max_period + offset
      ),
    digits = 5
  )

  dial_pos_full_y <- round(
    max_plot_radius *
      sin(
        direction * time_labels * 2 * pi / max_period + offset
      ),
    digits = 5
  )

  # determining the bounds to plot if zoom = TRUE
  # designed to find the minimum plot window that contains
  # all confidence ellipses.
  if (zoom) {
    xmax_zoom <- max(est_rrr) + max(max(a_trans), max(b_trans))
    xmin_zoom <- min(est_rrr) - max(max(a_trans), max(b_trans))
    ymax_zoom <- max(est_sss) + max(max(a_trans), max(b_trans))
    ymin_zoom <- min(est_sss) - max(max(a_trans), max(b_trans))

    # if view = "zoom_origin", anchor the view window to the origin
    if (zoom_origin) {
      xmin_zoom <- min(xmin_zoom, 0)
      xmax_zoom <- max(xmax_zoom, 0)
      ymin_zoom <- min(ymin_zoom, 0)
      ymax_zoom <- max(ymax_zoom, 0)
    }

    # ensure that contour labels are always within the view window
    contour_x_zoom <- cos(direction * mean(est_acr) + offset) * contour_labels
    contour_y_zoom <- sin(direction * mean(est_acr) + offset) * contour_labels
  }

  # adding special symbols to time_labels (π for radians, ° for degrees )
  if (radial_units == "radians") {
    pi_string <- paste(round(time_labels / pi, 1))
    time_labels <- paste0(pi_string, "\U03C0")
    acr_overlay <- paste0(
      signif(conversion_factor * est_acr / pi, 2),
      "\U03C0"
    )
  } else if (radial_units == "degrees") {
    time_labels <- paste0(time_labels, "\U00B0")
    acr_overlay <- paste0(
      signif(conversion_factor * est_acr, 2),
      "\U00B0"
    )
  } else if (radial_units == "period") {
    acr_overlay <- paste0(
      signif(conversion_factor * est_acr, 2)
    )
  }

  # create the main plot object
  if (is.na(x$group_original[component_index])) {
    group_level <- NULL
    group_level_colour_index <- 1
  } else {
    group_level_colour_index <- length(group_level)
  }

  # get the plot background
  plot_background <- get_background_grid(
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
  )

  plot_obj <- get_point_estimate_plot(
    est_rrr,
    est_sss,
    a_trans,
    b_trans,
    offset,
    direction,
    est_acr,
    group_level,
    group_level_colour_index,
    group_check,
    ellipse_opacity,
    plot_background
  )

  if (x$group_check) {
    plot_obj <- plot_obj + ggplot2::labs(fill = x_str, colour = NULL)
  }
  # OPTIONAL: overlays lines connecting the parameter estimates to the
  # origin, and displays estimates in plot
  if (overlay_parameter_info) {
    radius_sequence <- seq(
      0.65 * min(l_est_amp),
      0.80 * min(l_est_amp),
      length.out = length(est_amp)
    )
    overlay_labels <- paste(
      paste0("A = ", signif(est_amp, 2)),
      paste0("\U03D5 = ", acr_overlay),
      sep = "\n"
    )
    plot_obj <- plot_obj +
      ggplot2::geom_segment(
        ggplot2::aes(
          x = 0,
          y = 0,
          xend = est_rrr,
          yend = est_sss,
          colour = group_level
        )
      ) +
      ggforce::geom_arc(
        ggplot2::aes(
          x0 = 0,
          y0 = 0,
          r = radius_sequence,
          start = overlay_start,
          end = (overlay_start - direction * est_acr),
          colour = group_level
        )
      ) +
      ggplot2::geom_text(
        ggplot2::aes(
          label = overlay_labels,
          est_rrr,
          y = est_sss
        ),
        size = text_size,
        alpha = text_opacity
      )
  }

  # apply colors chosen by user input to the fill and colour aesthetics
  if (fill_colors_check) {
    plot_obj <- plot_obj +
      ggplot2::scale_fill_manual(
        values = fill_colors,
        aesthetics = c("fill", "colour")
      )
  } else {
    plot_obj <- plot_obj +
      ggplot2::scale_fill_manual(
        values = grDevices::rainbow(group_level_colour_index),
        aesthetics = c("fill", "colour")
      )
  }

  # if the view argument is 'zoom', or 'zoom_origin', apply
  # transformed view_limits
  if (zoom) {
    plot_obj <- plot_obj +
      ggplot2::geom_text(
        ggplot2::aes(
          label = contour_labels,
          x = contour_x_zoom,
          y = contour_y_zoom
        ),
        size = text_size,
        alpha = text_opacity
      )
    plot_obj <- plot_obj +
      ggplot2::coord_equal(
        xlim = c(xmin_zoom, xmax_zoom),
        ylim = c(ymin_zoom, ymax_zoom)
      )
  } else {
    # plot full polar plot if view = "full"
    plot_obj <- plot_obj + ggplot2::coord_fixed()
    # if an xlims argument was passed, set the xlims accordingly
    if (xlims_check && !ylims_check) {
      plot_obj <- plot_obj +
        ggplot2::coord_fixed(xlim = c(xlims[1], xlims[2]))
    }

    # if a ylims argument was passed, set the xlims accordingly
    if (ylims_check && !xlims_check) {
      plot_obj <- plot_obj +
        ggplot2::coord_fixed(ylim = c(ylims[1], ylims[2]))
    }

    # if both xlims and ylims are passed, set coordinate accordingly
    if (ylims_check && xlims_check) {
      plot_obj <- plot_obj +
        ggplot2::coord_fixed(
          xlim = c(xlims[1], xlims[2]),
          ylim = c(ylims[1], ylims[2])
        )
    }
  }

  # OPTIONAL: print information about the polar grid
  if (!quietly & length(contour_labels) > 1) {
    message(
      "Concentric circles every ",
      contour_labels[2] - contour_labels[1],
      " unit(s)"
    )
    message("Angle in units of ", radial_units)
  }

  # return the plot object
  plot_obj
}
