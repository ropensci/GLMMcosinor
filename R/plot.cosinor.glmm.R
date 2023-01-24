#' Plot a cosinor model
#'
#' Given a cosinor.glmm model fit, generate a plot of the data with the fitted values.
#' Optionally allows for plotting by covariates
#'
#'
#' @param object An object of class \code{cosinor.glmm}
#' @param x_str Character vector naming the covariate(s) to be plotted. May be NULL to plot overall curve
#'
#'
#' @examples
#'
#' object <- cosinor.glmm(Y ~ X + amp.acro(time, group = "X"), data = vitamind)
#' ggplot.cosinor.glmm(object, x_str = "X")
#'
#' @export ggplot.cosinor.glmm
#' @export
#'
#'
#'
#
# vdiffr - check out this package to see tests for plots
# cowplot - check this out to plot several plots within a single output check out: https://github.com/gentrywhite/DSSP/blob/master/R/plot.R
# Add ability to specify plotting of original dataset overalyed by trendline (off by default)
ggplot.cosinor.glmm <- function(object, x_str = NULL, type = "response", xlims, pred.length.out = 200, superimpose.data = FALSE, data_opacity = 0.3, predict.ribbon = TRUE) {
  if (!missing(xlims)) {
    timeax <- seq(xlims[1], xlims[2], length.out = pred.length.out) # with multiple periods, largest is used for timeax simulation
  } else {
    timeax <- seq(0, max(object$period), length.out = pred.length.out) # with multiple periods, largest is used for timeax simulation
  }

  data_processor_plot <- function(object, newdata, x_str) {
    covars <- names(object$group_stats)
    for (j in covars) {
      ref_level <- unlist(object$group_stats[j])[[1]]
      newdata[, j] <- factor(ref_level)
    }
    newdata <- update_formula_and_data(
      data = newdata, # pass new dataset that's being used for prediction in this function
      formula = eval(object$cosinor.glmm.calls$cosinor.glmm$formula) # get the formula that was originally to cosinor.glmm()
    )$newdata # only keep the newdata that's returned from update_formula_and_data()

    if (!is.null(x_str)) {
      for (d in x_str) {
        for (k in unlist(object$group_stats[[d]])[-1]) {
          tdat <- newdata
          tdat[, d] <- factor(k)
          newdata <- rbind(newdata, tdat, stringsAsFactors = FALSE)
        }
      }
      newdata$levels <- ""
      for (d in x_str) {
        # newdata$levels <- paste(newdata$levels, paste(d, "=", newdata[, d]))
        newdata$levels <- newdata[, d]
      }
    }
    newdata
  }

  newdata <- data.frame(time = timeax, stringsAsFactors = FALSE)
  colnames(newdata)[1] <- object$time_name
  newdata_processed <- data_processor_plot(object, newdata, x_str)
  y_name <- object$response_var

  pred_obj <- predict.cosinor.glmm(object, newdata = newdata_processed, type = type)
  newdata_processed[[y_name]] <- pred_obj$fit # adjust Y-axis name to correspond to whatever is in the dataframe
  newdata_processed$y_min <- pred_obj$fit - 1.96 * pred_obj$se.fit
  newdata_processed$y_max <- pred_obj$fit + 1.96 * pred_obj$se.fit

  if (superimpose.data) {
    original_data <- object$newdata
    original_data_processed <- object$newdata

    original_data_processed["levels"] <- original_data_processed[x_str]
  }
  if (!superimpose.data) {
    if (missing(x_str) || is.null(x_str)) {
      plot_object <- ggplot2::ggplot(newdata_processed, ggplot2::aes_string(x = paste(object$time_name), y = y_name)) +
        ggplot2::geom_line()
    } else {
      plot_object <- ggplot2::ggplot() +
        geom_line(data = newdata_processed, ggplot2::aes_string(x = paste(object$time_name), y = y_name, col = "levels"))
    }
  }

  if (superimpose.data) {
    if (missing(x_str) || is.null(x_str)) {
      plot_object <- ggplot2::ggplot(newdata_processed, ggplot2::aes_string(x = paste(object$time_name), y = y_name)) +
        ggplot2::geom_line() +
        geom_point(data = original_data_processed, ggplot2::aes_string(x = paste(object$time_name), y = y_name), alpha = data_opacity) +
        facet_grid(rows = vars(NULL))
    } else {
      plot_object <- ggplot2::ggplot() +
        geom_line(data = newdata_processed, ggplot2::aes_string(x = paste(object$time_name), y = y_name, col = "levels")) +
        geom_point(data = original_data_processed, ggplot2::aes_string(paste(object$time_name), y = y_name, col = "levels"), alpha = data_opacity) +
        facet_grid(rows = vars(NULL))
    }
  }

  if (predict.ribbon) {
    if (missing(x_str) || is.null(x_str)) {
      plot_object <- plot_object + ggplot2::geom_ribbon(data = newdata_processed, ggplot2::aes(x = !!sym(object$time_name), ymin = y_min, ymax = y_max), alpha = 0.5) +
        facet_grid(rows = vars(NULL))
    } else {
      plot_object <- plot_object + ggplot2::geom_ribbon(data = newdata_processed, ggplot2::aes(x = !!sym(object$time_name), ymin = y_min, ymax = y_max, col = levels, fill = levels), alpha = 0.5) +
        facet_grid(rows = vars(NULL))
    }
  }
  print(plot_object)
}


#' Generates a polar plot with elliptical confidence intervals
#'
#' @return
#' @export
#'
#' @examples
ggplot.cosinor.glmm.polar <- function(object,
                                      contour_interval,
                                      grid_angle_segments = 8,
                                      quietly = TRUE,
                                      radial_units = "radians",
                                      clockwise = FALSE,
                                      text_size = 3,
                                      text_opacity = 0.5,
                                      component_index = 1,
                                      make_cowplot = TRUE,
                                      circle_linetype = "dotted",
                                      ellipse_opacity = 0.3,
                                      fill_colours = c("red" ,"green", "blue", "purple", "pink", "yellow", "orange", "black"),
                                      view = "full",
                                      start = "right",
                                      overlay_parameter_info = FALSE) {
  sum <- summary.cosinor.glmm(object) # get summary statistics of cosinor.glmm object

  browser()
  #checking the quality of inputs
  assertthat::assert_that(inherits(object,"cosinor.glmm"),
                          msg = "object must be of class cosinor.glmm")

  assertthat::assert_that(is.numeric(contour_interval) & contour_interval > 0,
                          msg = "contour_interval must be a number greater than 0")



 if (length(fill_colours) < max(unlist(lapply(object$group_stats, length)))) {
   fill_colours <- rainbow(max(unlist(lapply(object$group_stats, length))), start = 0)
 }
    # convert user input for zoom level into logical arguments
  if (view == "full") {
    zoom <- FALSE
    zoom_origin <- FALSE
  }
  if (view == "zoom_origin") {
    zoom <- TRUE
    zoom_origin <- TRUE
  }
  if (view == "zoom") {
    zoom <- TRUE
    zoom_origin <- FALSE
  }

  # check if there is a contour argument & store this check in local environment
  n_components <- object$n_components
  if (!missing(contour_interval)) {
    contour_interval_check <- TRUE
  } else {
    contour_interval_check <- FALSE
  }

  # check if there is a fill_colours argument & store this check in local environment

  # convert user input for starting position (on Cartesian plane) into logical arguments
  # by default, ggplot() ellipse and circle functions use unit circle angles where 0 degrees
  # starts at the '3pm' position and rotates counterclockwise
  # However, the ggforce function geom_arc() starts at upwards 12pm position, also rotating coutnerclockwise
  # Hence, offset, and overlay_param_offset are different to ultimately describe the same angle position
  if (start == "top") {
    offset <- pi / 2
    overlay_param_offset <- offset - pi / 2
  }
  if (start == "left") {
    offset <- pi
    overlay_param_offset <- offset + pi / 2
  }
  if (start == "bottom") {
    offset <- 3 * pi / 2
    overlay_param_offset <- offset - pi / 2
  }
  if (start == "right") {
    offset <- 0
    overlay_param_offset <- offset + pi / 2
  }

  if (!missing(make_cowplot) & !missing(component_index)) {
    make_cowplot <- FALSE
  }


  # get ggplot for a single component. Function will then be looped for multiple components
  sub_ggplot.cosinor.glmm.polar <- function(comp, ...) {

    component_index <- comp # get the component that is going to plotted
    args <- match.call()[-1] # get the arguments from the function wrapping this function
    period <- object$period[component_index]
    max_period <- period
    group_check <- (object$group[component_index] != 0)
      if (group_check) {
      x_str <- object$group_original[component_index]
      group <- x_str
      level <- object$group_stats[[group]]
      string_index <- paste0("[", group, "=") # create an index that will be used to grab the correct transformed summary stats
      string_index_raw <- paste0(group) # create an index that grabs the corresponding raw summary stats
    } else {
      group = NULL
      string_index <- ""
      string_index_raw <- ""
    }

    amp_index <- paste0("amp", component_index)
    acr_index <- paste0("acr", component_index)

    # grab and store the summary statistics for amp and acr
    est_amp <- sum$transformed.table$estimate[which(grepl(string_index, rownames(sum$transformed.table), fixed = TRUE) & grepl(amp_index, rownames(sum$transformed.table), fixed = TRUE))]
    l_est_amp <- sum$transformed.table$lower.CI[which(grepl(string_index, rownames(sum$transformed.table), fixed = TRUE) & grepl(amp_index, rownames(sum$transformed.table), fixed = TRUE))]
    u_est_amp <- sum$transformed.table$upper.CI[which(grepl(string_index, rownames(sum$transformed.table), fixed = TRUE) & grepl(amp_index, rownames(sum$transformed.table), fixed = TRUE))]

    est_acr <- sum$transformed.table$estimate[which(grepl(string_index, rownames(sum$transformed.table), fixed = TRUE) & grepl(acr_index, rownames(sum$transformed.table), fixed = TRUE))]
    l_est_acr <- sum$transformed.table$lower.CI[which(grepl(string_index, rownames(sum$transformed.table), fixed = TRUE) & grepl(acr_index, rownames(sum$transformed.table), fixed = TRUE))]
    u_est_acr <- sum$transformed.table$upper.CI[which(grepl(string_index, rownames(sum$transformed.table), fixed = TRUE) & grepl(acr_index, rownames(sum$transformed.table), fixed = TRUE))]

    # an index of the names of the summary statistics used in iteration of loop
    name_index <- rownames(sum$transformed.table)[which(grepl(string_index, rownames(sum$transformed.table), fixed = TRUE) & grepl(amp_index, rownames(sum$transformed.table), fixed = TRUE))]
    group_level <- array(dim = length(name_index))

    # obtain an index of group levels
    if (group_check) {
    for (i in level) {
      group_ind <- paste0(group, "=", i)
      group_level[which(grepl(group_ind, name_index))] <- paste(group, "=", i)
    }
    }

    # set direction of increasing angle based on user input of clockwise argument
    if (clockwise) {
      direction <- -1
    } else {
      direction <- 1
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
      contour_interval <- max_radius / 5 # a default if no contour_interval argument is supplied
    }

    # change 'max_period' to correspond to units specified by the user
    if (radial_units == "radians") {
      max_period <- 2 * pi
    }
    if (radial_units == "degrees") {
      max_period <- 360
    }
    if (radial_units == "period") {
      max_period <- max_period
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
      contour_x_zoom <- cos(direction*mean(est_acr) + offset) * contour_labels
      #contour_x_zoom <- cos(direction*est_acr + offset)*t(replicate(length(est_amp), contour_labels))
      contour_y_zoom <- sin(direction*mean(est_acr) + offset) * contour_labels
      #contour_y_zoom <- sin(direction*est_acr + offset)*t(replicate(length(est_amp), contour_labels))
    }

    # adding special symbols to time_labels (π for radians, ° for degrees )
    if (radial_units == "radians") {
      pi_string <- paste(round(time_labels / pi, 1))
      time_labels <- paste0(pi_string, "π")
    }

    if (radial_units == "degrees") {
      time_labels <- paste0(time_labels, "°")
    }


    # create the main plot object
    plot_obj <-
      ggplot2::ggplot() +
      ggforce::geom_circle(ggplot2::aes(x0 = 0, y0 = 0, r = seq(from = 0, to = max_plot_radius, by = contour_interval)), alpha = 0.3, linetype = circle_linetype) +
      ggforce::geom_ellipse(ggplot2::aes(x0 = est_rrr, y0 = est_sss, a = a_trans, b = b_trans, angle = offset + direction * est_acr, fill = group_level, colour = group_level), alpha = ellipse_opacity) +
      ggplot2::geom_point(ggplot2::aes(x = est_rrr, y = est_sss)) +
      ggplot2::geom_segment(ggplot2::aes(x = dial_pos_full_x, y = dial_pos_full_y, xend = -dial_pos_full_x, yend = -dial_pos_full_y), linetype = circle_linetype) +
      ggplot2::geom_text(ggplot2::aes(label = time_labels[-length(time_labels)]), x = 1.05 * dial_pos_full_x[-length(dial_pos_full_x)], y = 1.05 * dial_pos_full_y[-length(dial_pos_full_y)], size = text_size, alpha = text_opacity) +
      ggplot2::geom_text(ggplot2::aes(label = contour_labels, x = contour_labels, y = 0), hjust = 1, vjust = -1, size = text_size, alpha = text_opacity) +
      ggplot2::labs(fill = "Group level", colour = NULL) +
      guides(colour = "none") +
      ggplot2::theme(axis.title.x = element_blank(), axis.title.y = element_blank(), axis.text.x = element_blank(), axis.text.y = element_blank(), axis.ticks = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank())

    # OPTIONAL: overlays lines connecting the parameter estimates to the origin, and displays estimates in plot
    if (overlay_parameter_info) {
      radius_sequence <- seq(0.20 * min(l_est_amp), 0.80 * min(l_est_amp), length.out = length(est_amp))
      overlay_labels <- paste(paste0("A = ", signif(est_amp, 3)), paste0("ϕ = ", signif(est_acr, 3)), sep = "\n")
      plot_obj <- plot_obj +
        ggplot2::geom_segment(ggplot2::aes(x = 0, y = 0, xend = est_rrr, yend = est_sss, colour = group_level)) +
        ggforce::geom_arc(aes(x0 = 0, y0 = 0, r = radius_sequence, start = -direction * overlay_param_offset, end = -direction * (overlay_param_offset + est_acr), colour = group_level)) +
        ggplot2::geom_text(
          ggplot2::aes(
            label = overlay_labels, x = cos(direction * est_acr * 1.05 + offset) * radius_sequence,
            y = sin(direction * est_acr * 1.05 + offset) * radius_sequence
          ),
          size = text_size, alpha = text_opacity
        )
    }

    # apply colours chosen by user input to the fill and colour aesthetics
    plot_obj <- plot_obj + scale_fill_manual(values = fill_colours, aesthetics = c("fill", "colour"))

    # if the view argument is 'zoom', or 'zoom_origin', apply transformed view_limits
    if (zoom) {
      plot_obj <- plot_obj + ggplot2::geom_text(ggplot2::aes(label = contour_labels, x = contour_x_zoom, y = contour_y_zoom), size = text_size, alpha = text_opacity)
      plot_obj <- plot_obj + ggplot2::coord_cartesian(xlim = c(xmin_zoom, xmax_zoom), ylim = c(ymin_zoom, ymax_zoom))
    } else {
      plot_obj <- plot_obj + ggplot2::coord_fixed() # plot full polar plot if view = "full"
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
    final_obj <- cowplot::plot_grid(plotlist = plot_list, labels = paste("Component", seq(from = 1, to = n_components, by = 1)))
    print(final_obj)
  } else {
    final_obj <- sub_ggplot.cosinor.glmm.polar(component_index)
    print(final_obj)
  }

  # OPTIONAL: print information about the polar grid
  if (!quietly) {
    message("Circular contours every ", contour_interval, " unit(s)")
    message("Angle in units of ", radial_units)
  }
}
