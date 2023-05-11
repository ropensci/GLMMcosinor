#' Generates a polar plot with elliptical confidence intervals
#'
#' @param x An object of class \code{cosinor.glmm}
#' @param ci_level The level for calculated confidence ellipses. Defaults to 0.95.
#' @param contour_interval The distance bewteen adjacent circular contours in the background of the polar plot
#' @param make_cowplot A logical argument. If TRUE, plots polar plots for each component and displays the results as a single output with several plots. If make_cowplot is TRUE, specifying component_index is redundant
#' @param component_index A number that corresponds to a particular component from the cosinor.glmm() object that will be used to create polar plot. If make_cowplot is FALSE, then component_index controls which component is plotted
#' @param grid_angle_segments An integer that determines the total number of segments in the background of the polar plot. For example, a value of 4 will create quadrants around the origin.
#' @param radial_units A string controlling the angle units. Valid arguments are: 'radians', 'degrees', or 'period'. Radians plots from 0 to 2*pi; degrees plots from 0 to 360, and period plots from 0 to the maximum period in the component
#' @param clockwise A logical argument. If TRUE, the angles increase in a clockwise fashion
#' @param text_size A number controlling the size of the text labels
#' @param text_opacity A number between 0 and 1 inclusive which determines the opacity of the text labels
#' @param fill_colours A vector containing colours (expressed as strings) that will be used to delineate levels within a group. If the model has components with different number of levels per factor, the length of this input should match the greatest number of levels. If not, or if the number of levels exceeds the length of the default argument (8), colours are generated using rainbow()
#' @param ellipse_opacity A number between 0 and 1 inclusive which determines the opacity of the confidence ellipses
#' @param circle_linetype A string which determines the linetype of the radial circles in background of the polar plot. See ?linetype for more details
#' @param start A character, either "right", "left", "top", or "bottom" which determines where angle 0 is located. If start = "top", and clockwise = TRUE, the angle will rotate clockwise, starting at the '12' position on a clock
#' @param view A character, either "full", "zoom", or "zoom_origin" which controls the view of the plots. "full" maintains a full view of the polar plot, including the background radial circles. "zoom" finds the minimum viewwindow which contains all confidence ellipses. "zoom_origin" zooms into the confidence ellipses (like "zoom"), but also keeps the origin within frame
#' @param overlay_parameter_info A logical argument. If TRUE, more information about the acrophase and amplitude are overlayed onto the polar plots.
#' @param quietly Analagous to verbose, this logical argument controls whether messages are displayed in the console.
#' @param ... Additional, ignored arguments.
#'
#' @srrstats {G1.4} *Software should use [`roxygen2`](https://roxygen2.r-lib.org/) to document all functions.*
#'
#' @return Returns a `ggplot` graphics object.
#' @export
#'
#'
#' @examples
#' model <- cosinor.glmm(Y ~ X + amp.acro(time, group = "X"), data = vitamind)
#' polar_plot(model)
polar_plot <- function(x,
                       ci_level = 0.95,
                       contour_interval = 1,
                       make_cowplot = TRUE,
                       component_index = 1,
                       grid_angle_segments = 8,
                       radial_units = "radians",
                       clockwise = FALSE,
                       text_size = 3,
                       text_opacity = 0.5,
                       fill_colours,
                       ellipse_opacity = 0.3,
                       circle_linetype = "dotted",
                       start = "right",
                       view = "full",
                       overlay_parameter_info = FALSE,
                       quietly = TRUE,
                       ...) {
  UseMethod("polar_plot")
}


#' Generates a polar plot with elliptical confidence intervals
#'
#' @param x An \code{cosinor.glmm} object.
#' @param ci_level The level for calculated confidence ellipses. Defaults to 0.95.
#' @param contour_interval The distance bewteen adjacent circular contours in the background of the polar plot
#' @param make_cowplot A logical argument. If TRUE, plots polar plots for each component and displays the results as a single output with several plots. If make_cowplot is TRUE, specifying component_index is redundant
#' @param component_index A number that corresponds to a particular component from the cosinor.glmm() object that will be used to create polar plot. If make_cowplot is FALSE, then component_index controls which component is plotted
#' @param grid_angle_segments An integer that determines the total number of segments in the background of the polar plot. For example, a value of 4 will create quadrants around the origin.
#' @param radial_units A string controlling the angle units. Valid arguments are: 'radians', 'degrees', or 'period'. Radians plots from 0 to 2*pi; degrees plots from 0 to 360, and period plots from 0 to the maximum period in the component
#' @param clockwise A logical argument. If TRUE, the angles increase in a clockwise fashion
#' @param text_size A number controlling the size of the text labels
#' @param text_opacity A number between 0 and 1 inclusive which determines the opacity of the text labels
#' @param fill_colours A vector containing colours (expressed as strings) that will be used to delineate levels within a group. If the model has components with different number of levels per factor, the length of this input should match the greatest number of levels. If not, or if the number of levels exceeds the length of the default argument (8), colours are generated using rainbow()
#' @param ellipse_opacity A number between 0 and 1 inclusive which determines the opacity of the confidence ellipses
#' @param circle_linetype A string which determines the linetype of the radial circles in background of the polar plot. See ?linetype for more details
#' @param start A character, either "right", "left", "top", or "bottom" which determines where angle 0 is located. If start = "top", and clockwise = TRUE, the angle will rotate clockwise, starting at the '12' position on a clock
#' @param view A character, either "full", "zoom", or "zoom_origin" which controls the view of the plots. "full" maintains a full view of the polar plot, including the background radial circles. "zoom" finds the minimum viewwindow which contains all confidence ellipses. "zoom_origin" zooms into the confidence ellipses (like "zoom"), but also keeps the origin within frame
#' @param overlay_parameter_info A logical argument. If TRUE, more information about the acrophase and amplitude are overlayed onto the polar plots.
#' @param quietly Analagous to verbose, this logical argument controls whether messages are displayed in the console.
#' @param ... Additional, ignored arguments.
#'
#' @srrstats {G1.4} *Software should use [`roxygen2`](https://roxygen2.r-lib.org/) to document all functions.*
#'
#' @return Returns a `ggplot` graphics object.
#' @export
#'
#'
#' @examples
#' model <- cosinor.glmm(Y ~ X + amp.acro(time, group = "X"), data = vitamind)
#' polar_plot(model)
polar_plot.cosinor.glmm <- function(x,
                                    ci_level = 0.95,
                                    contour_interval = 1,
                                    make_cowplot = TRUE,
                                    component_index = 1,
                                    grid_angle_segments = 8,
                                    radial_units = "radians",
                                    clockwise = FALSE,
                                    text_size = 3,
                                    text_opacity = 0.5,
                                    fill_colours,
                                    ellipse_opacity = 0.3,
                                    circle_linetype = "dotted",
                                    start = "right",
                                    view = "full",
                                    overlay_parameter_info = FALSE,
                                    quietly = TRUE,
                                    ...) {
  # checking the quality of inputs

  assertthat::assert_that(inherits(x, "cosinor.glmm"),
                          msg = "'x' must be of class cosinor.glmm"
  )

  validate_ci_level(ci_level)

  if (!missing(contour_interval)) {
    assertthat::assert_that(is.numeric(contour_interval) & contour_interval > 0,
                            msg = "'contour_interval' must be a number greater than 0"
    )
  }
  assertthat::assert_that(
    grid_angle_segments == floor(grid_angle_segments) & grid_angle_segments > 0,
    msg = "'grid_angle_segments' must be an integer greater than 0"
  )
  assertthat::assert_that(is.logical(quietly),
                          msg = "'quietly' must a logical argument, either TRUE or FALSE"
  )
  assertthat::assert_that(is.character(radial_units) & radial_units %in% c("radians", "degrees", "period"),
                          msg = "'radial_units' must be either 'radians', 'degrees', or 'period'  "
  )
  assertthat::assert_that(is.logical(clockwise),
                          msg = "'clockwise' must be a logical argument, either TRUE or FALSE "
  )
  assertthat::assert_that(is.numeric(text_size) & text_size > 0,
                          msg = "'text_size' must be a number greater than 0"
  )
  assertthat::assert_that(is.numeric(text_opacity) & text_opacity >= 0 & text_opacity <= 1,
                          msg = "'text_opacity' must be a number between 0 and 1 inclusive"
  )
  assertthat::assert_that(is.numeric(ellipse_opacity) & ellipse_opacity >= 0 & ellipse_opacity <= 1,
                          msg = "'ellipse_opacity' must be a number between 0 and 1 inclusive"
  )
  assertthat::assert_that(is.logical(make_cowplot),
                          msg = "'make_cowplot' must be a logical argument, either TRUE or FALSE"
  )
  assertthat::assert_that(component_index == floor(component_index) & component_index > 0 & component_index <= x$n_components,
                          msg = "'component_index' must be an integer between 1 and n_components (total number of components in model) inclusive"
  )
  assertthat::assert_that(is.character(circle_linetype),
                          msg = "'circle_linetype' must be a character. See ?linetype for more details"
  )
  # assertthat::assert_that(is.character(fill_colours),
  #  msg = "fill_colours must be of class character, and must be a valid colour"
  # )
  assertthat::assert_that(is.character(start) & start %in% c("right", "left", "bottom", "top"),
                          msg = "'start' argument must be either 'right', 'left', 'bottom', or 'top'"
  )
  assertthat::assert_that(is.character(view) & view %in% c("full", "zoom", "zoom_origin"),
                          msg = "'view' argument must be either 'full', 'zoom', or 'zoom_origin'"
  )
  assertthat::assert_that(is.logical(overlay_parameter_info),
                          msg = "'overlay_parameter_info' must be a logical argument, either TRUE or FALSE"
  )

  sum <- summary(x, ci_level = ci_level) # get summary statistics of cosinor.glmm object


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
  contour_interval_check <- !missing(contour_interval)
  fill_colours_check <- !missing(fill_colours)


  # set direction of increasing angle based on user input of clockwise argument
  direction <- ifelse(clockwise, -1, 1)

  # convert user input for starting position (on Cartesian plane) into logical arguments
  # by default, ggplot() ellipse and circle functions use unit circle angles where 0 degrees
  # starts at the '3pm' position and rotates counterclockwise
  # However, the ggforce function geom_arc() starts at upwards 12pm position, also rotating coutnerclockwise
  # Hence, offset, and overlay_param_offset are different to ultimately describe the same angle position
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

  if (!missing(make_cowplot) & !missing(component_index)) {
    make_cowplot <- FALSE
  }


  # get ggplot for a single component. Function will then be looped for multiple components
  sub_ggplot.cosinor.glmm.polar <- function(comp, ...) {
    component_index <- comp # get the component that is going to plotted
    args <- match.call()[-1] # get the arguments from the function wrapping this function
    period <- x$period[component_index]
    max_period <- period
    group_check <- (x$group[component_index] != 0)
    if (group_check) {
      x_str <- x$group_original[component_index]
      group <- x_str
      level <- x$group_stats[[group]]
      string_index <- paste0("[", group, "=") # create an index that will be used to grab the correct transformed summary stats
      string_index_raw <- paste0(group) # create an index that grabs the corresponding raw summary stats
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
        group_level[which(grepl(group_ind, name_index))] <- paste(group, "=", i)
      }
    }


    # determine the estimated rrr and sss used parameter estimates
    est_rrr <- est_amp * cos(direction * (est_acr) + offset)
    est_sss <- est_amp * sin(direction * (est_acr) + offset)

    # for confidence ellipses, get the long_axis width "a_trans" (amplitude), and the short-axis height "b_trans" (acrophase)
    # note that both values are halved because they are technically radii,
    a_trans <- est_amp - l_est_amp
    b_trans <- tan((u_est_acr - l_est_acr) / 2) * est_amp

    # determine the maximum radius in a single plot. This will be used for formatting plot features
    max_radius <- max(abs(u_est_amp), abs(l_est_amp))
    if (contour_interval_check) {
      if (contour_interval > max_radius) {
        contour_interval <- max_radius / 5 # reformat the contour_interval if the user supplies an inappropriate value
        warning("contour_interval ignored because it is too high")
      }
    } else {
      contour_interval <- 1 # a default if no contour_interval argument is supplied
    }

    # change 'max_period' to correspond to units specified by the user
    # conversion_factor is used to convert from radians (default acrophase output) to whatever radial_units is
    if (radial_units == "radians") {
      max_period <- 2 * pi
      conversion_factor <- 1
    } else if (radial_units == "degrees") {
      max_period <- 360
      conversion_factor <- (1 / 2 * pi) * 360
    } else if (radial_units == "period") {
      max_period <- max_period
      conversion_factor <- (1 / 2 * pi) * max_period
    }

    # create a sequence of labels for time (to be inserted around the polar plot)
    time_labels <- signif(seq(from = 0, to = max_period, by = max_period / grid_angle_segments), 3)


    # create a sequence of labels for the contours.
    contour_labels <- signif(seq(from = 0, to = max_radius + contour_interval, by = contour_interval), 3)

    # determine largest contour, and use this as a plot limit
    max_plot_radius <- max(contour_labels)

    # convert time_labels to polar coordinates to determine position of where they should be placed
    dial_pos_full_x <- round(max_plot_radius * cos(direction * time_labels * 2 * pi / max_period + offset), digits = 5)
    dial_pos_full_y <- round(max_plot_radius * sin(direction * time_labels * 2 * pi / max_period + offset), digits = 5)

    # determining the bounds to plot if zoom = TRUE
    # designed to find the minimum plot window that contains all confidence ellipses.
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
      conversion_factor * est_acr
      acr_overlay <- paste0(
        signif(conversion_factor * est_acr / 360, 2),
        "\U00B0"
      )
    }


    # create the main plot object
    if (is.na(x$group_origina[component_index])) { # TODO typo?
      group_level <- NULL
    }

    plot_obj <-
      ggplot2::ggplot() +
      ggforce::geom_circle(
        ggplot2::aes(
          x0 = 0,
          y0 = 0,
          r = seq(from = 0, to = max_plot_radius, by = contour_interval)
        ),
        alpha = 0.3,
        linetype = circle_linetype
      ) +
      ggforce::geom_ellipse(
        ggplot2::aes(
          x0 = est_rrr,
          y0 = est_sss,
          a = a_trans,
          b = b_trans,
          angle = offset + direction * est_acr,
          fill = group_level,
          colour = group_level
        ),
        alpha = ellipse_opacity
      ) +
      ggplot2::geom_point(
        ggplot2::aes(x = est_rrr, y = est_sss)
      ) +
      ggplot2::geom_segment(
        ggplot2::aes(
          x = dial_pos_full_x,
          y = dial_pos_full_y,
          xend = -dial_pos_full_x,
          yend = -dial_pos_full_y
        ),
        linetype = circle_linetype
      ) +
      ggplot2::geom_text(
        ggplot2::aes(label = time_labels[-length(time_labels)]),
        x = 1.05 * dial_pos_full_x[-length(dial_pos_full_x)],
        y = 1.05 * dial_pos_full_y[-length(dial_pos_full_y)],
        size = text_size,
        alpha = text_opacity
      ) +
      ggplot2::geom_text(
        ggplot2::aes(label = contour_labels, x = contour_labels, y = 0),
        hjust = 1, vjust = -1, size = text_size, alpha = text_opacity
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

    if (x$group_check) {
      plot_obj <- plot_obj + ggplot2::labs(fill = "Group level", colour = NULL)
    }
    # OPTIONAL: overlays lines connecting the parameter estimates to the origin, and displays estimates in plot
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
            label = overlay_labels, est_rrr, y = est_sss
          ),
          size = text_size,
          alpha = text_opacity
        )
    }



    # apply colours chosen by user input to the fill and colour aesthetics
    if (fill_colours_check) {
      plot_obj <- plot_obj +
        ggplot2::scale_fill_manual(
          values = fill_colours,
          aesthetics = c("fill", "colour")
        )
    }

    # if the view argument is 'zoom', or 'zoom_origin', apply transformed view_limits
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
      plot_obj <- plot_obj + ggplot2::coord_fixed() # plot full polar plot if view = "full"
    }

    # OPTIONAL: print information about the polar grid
    if (!quietly) {
      message(
        "Circular contours every ", signif(contour_interval, 5), " unit(s)"
      )
      message("Angle in units of ", radial_units)
    }

    # return the plot object
    plot_obj
  }


  # plot multiple component plots in cowplot or plot a single component plot
  if (make_cowplot == TRUE & n_components > 1) {
    plot_list <- NULL
    for (i in 1:n_components) {
      assign(paste0("plot_obj", i), sub_ggplot.cosinor.glmm.polar(i))
      plot_list[[i]] <- ggplot2::ggplotGrob(sub_ggplot.cosinor.glmm.polar(i))
    }
    final_obj <- cowplot::plot_grid(
      plotlist = plot_list,
      labels = paste("Component", 1:n_components)
    )
    final_obj
  } else {
    final_obj <- sub_ggplot.cosinor.glmm.polar(component_index)
    final_obj
  }
}
