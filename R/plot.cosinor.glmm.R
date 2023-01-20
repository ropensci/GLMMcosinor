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
#vdiffr - check out this package to see tests for plots
#cowplot - check this out to plot several plots within a single output check out: https://github.com/gentrywhite/DSSP/blob/master/R/plot.R
#Add ability to specify plotting of original dataset overalyed by trendline (off by default)
ggplot.cosinor.glmm <- function(object, x_str = NULL, type = "response", xlims, pred.length.out = 200, superimpose.data = FALSE, data_opacity = 0.3, predict.ribbon = TRUE) {

  if(!missing(xlims)) {
    timeax <- seq(xlims[1], xlims[2], length.out = pred.length.out) #with multiple periods, largest is used for timeax simulation
  } else {
    timeax <- seq(0, max(object$period), length.out = pred.length.out) #with multiple periods, largest is used for timeax simulation
  }

  data_processor_plot <- function(object, newdata, x_str) {

  covars <- names(object$group_stats)
  for (j in covars) {
    ref_level <- unlist(object$group_stats[j])[[1]]
    newdata[,j] <- factor(ref_level)
  }
  newdata <- update_formula_and_data(
    data = newdata, # pass new dataset that's being used for prediction in this function
    formula = eval(object$cosinor.glmm.calls$cosinor.glmm$formula) # get the formula that was originally to cosinor.glmm()
  )$newdata # only keep the newdata that's returned from update_formula_and_data()

  if (!is.null(x_str)) {
    for (d in x_str) {
      for (k in unlist(object$group_stats[[d]])[-1]) {
      tdat <- newdata
      tdat[,d] <- factor(k)
      newdata <- rbind(newdata, tdat, stringsAsFactors = FALSE)
      }
    }
    newdata$levels <- ""
    for (d in x_str) {
      #newdata$levels <- paste(newdata$levels, paste(d, "=", newdata[, d]))
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
  newdata_processed[[y_name]] <- pred_obj$fit#adjust Y-axis name to correspond to whatever is in the dataframe
  newdata_processed$y_min <-pred_obj$fit-1.96*pred_obj$se.fit
  newdata_processed$y_max <-pred_obj$fit+1.96*pred_obj$se.fit

    if (superimpose.data) {
  original_data <- object$newdata
  original_data_processed <- object$newdata

  original_data_processed["levels"] <-  original_data_processed[x_str]
  }
  if (!superimpose.data) {
    if (missing(x_str) || is.null(x_str)) {
      plot_object <- ggplot2::ggplot(newdata_processed, aes_string(x = paste(object$time_name), y = y_name)) +
        ggplot2::geom_line()
      } else {
      plot_object <-ggplot2::ggplot() +
        geom_line(data = newdata_processed, aes_string(x = paste(object$time_name), y = y_name, col = "levels"))
    }
  }

  if (superimpose.data) {
    if (missing(x_str) || is.null(x_str)) {
      plot_object <- ggplot2::ggplot(newdata_processed, aes_string(x = paste(object$time_name), y = y_name)) +
        ggplot2::geom_line() +
        geom_point(data = original_data_processed, aes_string(x = paste(object$time_name), y = y_name), alpha = data_opacity) +
        facet_grid(rows = vars(NULL))
    } else {
      plot_object <- ggplot2::ggplot() +
        geom_line(data = newdata_processed, aes_string(x = paste(object$time_name), y = y_name, col = "levels")) +
        geom_point(data = original_data_processed, aes_string(paste(object$time_name), y = y_name, col = "levels"), alpha = data_opacity) +
        facet_grid(rows = vars(NULL))
    }

  }

  if (predict.ribbon) {
    if (missing(x_str) || is.null(x_str)) {
    plot_object <-  plot_object + ggplot2::geom_ribbon(data = newdata_processed,aes(x = !!sym(object$time_name), ymin = y_min, ymax = y_max), alpha = 0.5) +
      facet_grid(rows = vars(NULL))
    } else {
    plot_object <-  plot_object + ggplot2::geom_ribbon(data = newdata_processed,aes(x = !!sym(object$time_name), ymin = y_min, ymax = y_max, col = levels, fill = levels), alpha = 0.5) +
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
                                      x_str,
                                      contour_interval,
                                      grid_angle_segments = 8,
                                      plot_info = FALSE,
                                      radial_units = "radians",
                                      clockwise = FALSE,
                                      text_size = 3,
                                      text_opacity = 0.5,
                                      component_index = 1,
                                      make_cowplot = FALSE,
                                      show_polar_grid = TRUE) {
  sum <- summary.cosinor.glmm(object) #get summary statistics of cosinor.glmm object

  # check if there is a countour argument
  n_components <- object$n_components
  if (!missing(contour_interval)) {
    contour_interval_check = TRUE
  } else {
    contour_interval_check = FALSE
  }

  # get ggplot for a single component. Function will then be looped for multiple components
  sub_ggplot.cosinor.glmm.polar <- function(comp, ...) {

  component_index <- comp
  args <- match.call()[-1]
  period <- object$period[component_index]
  max_period <- period
  x_str <- object$group_original[component_index]
  group <- x_str
  level <- object$group_stats[[x_str]]

    string_index <- paste0("[",group,"=")
    string_index_raw <- paste0(group)
    amp_index <- paste0("amp", component_index)
    acr_index <- paste0("acr", component_index)

    est_amp <- sum$transformed.table$estimate[which(grepl(string_index, rownames(sum$transformed.table), fixed = TRUE) & grepl(amp_index, rownames(sum$transformed.table), fixed = TRUE) )]
    l_est_amp <- sum$transformed.table$lower.CI[which(grepl(string_index, rownames(sum$transformed.table), fixed = TRUE)& grepl(amp_index, rownames(sum$transformed.table), fixed = TRUE) )]
    u_est_amp <- sum$transformed.table$upper.CI[which(grepl(string_index, rownames(sum$transformed.table), fixed = TRUE)& grepl(amp_index, rownames(sum$transformed.table), fixed = TRUE) )]

    est_acr   <- sum$transformed.table$estimate[which(grepl(string_index, rownames(sum$transformed.table), fixed = TRUE)& grepl(acr_index, rownames(sum$transformed.table), fixed = TRUE) )]
    l_est_acr <- sum$transformed.table$lower.CI[which(grepl(string_index, rownames(sum$transformed.table), fixed = TRUE)& grepl(acr_index, rownames(sum$transformed.table), fixed = TRUE) )]
    u_est_acr <- sum$transformed.table$upper.CI[which(grepl(string_index, rownames(sum$transformed.table), fixed = TRUE)& grepl(acr_index, rownames(sum$transformed.table), fixed = TRUE) )]

    name_index <- rownames(sum$transformed.table)[which(grepl(string_index, rownames(sum$transformed.table), fixed = TRUE) & grepl(amp_index, rownames(sum$transformed.table), fixed = TRUE) )]
    group_level <- array(dim = length(name_index))
    for (i in level) {
      group_ind <- paste0(group,"=",i )
      group_level[which(grepl(group_ind, name_index))] <- paste(group,"=",i)
    }


    if (clockwise) {
      direction = -1 #used to reverse the counter-clockwise direction of the unit circle
      offset = pi/2 #angle used to rotate the polar plots
      xlab = "sss" #swapping the 'rrr' and 'sss' axis to reflect the angle offset
      ylab = "rrr"
    } else {
      direction = 1
      offset = 0
      xlab = "rrr"
      ylab = "sss"
    }

    est_rrr <- est_amp*cos(direction*(est_acr) + offset)
    est_sss <- est_amp*sin(direction*(est_acr) + offset)
    b_trans <- tan((u_est_acr-l_est_acr)/2)*est_amp
    a_trans <- est_amp - l_est_amp

    max_radius <- max(abs(u_est_amp),abs(l_est_amp))
    if (contour_interval_check) {
      if (contour_interval > max_radius) {
        contour_interval <- max_radius/5
        warning("contour_interval ignored because it is too high")
      }
    } else {
      contour_interval <- max_radius/5
    }

    #TODO: change this to work with different units (radians, degrees, period etc)
    if (radial_units == "radians") {
      max_period <- 2*pi
    }
    if (radial_units == "degrees") {
      max_period <- 360
    }
    if (radial_units == "period") {
      max_period <- max_period
    }

    time_labels <- signif(seq(from = 0, to = max_period, by = max_period/grid_angle_segments),3)

    contour_labels <- signif(seq(from = 0, to = max_radius + contour_interval, by = contour_interval), 3)
    max_plot_radius <- max(contour_labels)
    dial_pos_full_x <- round(max_plot_radius*cos(direction*time_labels*2*pi/max_period  + offset), digits = 5)
    dial_pos_full_y <- round(max_plot_radius*sin(direction*time_labels*2*pi/max_period  + offset), digits = 5)

    if (radial_units == "radians") {
      pi_string <- paste(round(time_labels/pi,1))
      time_labels <- paste0(pi_string,"π")
    }

    if (show_polar_grid) {
    plot_obj <-
           ggplot2::ggplot() +
      ggforce::geom_circle(aes(x0 = 0, y0 = 0, r = seq(from = 0, to = max_plot_radius, by = contour_interval)), alpha = 0.5, linetype = "dotted") +
      ggforce::geom_ellipse(aes(x0 = est_rrr , y0 = est_sss , a = a_trans, b = b_trans, angle = offset +direction*est_acr, fill = group_level), alpha = 0.3) +
      ggplot2::geom_point(aes(x = est_rrr, y = est_sss)) +
      ggplot2::geom_segment(aes(x = dial_pos_full_x, y = dial_pos_full_y, xend = -dial_pos_full_x, yend = -dial_pos_full_y), linetype = "dotted") +
      ggplot2::geom_text(aes(label = time_labels[-length(time_labels)]), x = 1.03*dial_pos_full_x[-length(dial_pos_full_x)], y = 1.03*dial_pos_full_y[-length(dial_pos_full_y)], size = text_size, alpha = text_opacity) +
      ggplot2::geom_text(aes(label = contour_labels, x = contour_labels, y = 0.1*contour_interval), size = text_size, alpha = text_opacity) +
      ggplot2::labs(x = xlab, y = ylab, fill = "Group level") +
      ggplot2::coord_fixed()
    }
  if (!show_polar_grid){
    plot_obj <-
      ggplot2::ggplot() +
      ggforce::geom_ellipse(aes(x0 = est_rrr , y0 = est_sss , a = a_trans, b = b_trans, angle = offset +direction*est_acr, fill = group_level), alpha = 0.3) +
      ggplot2::geom_point(aes(x = est_rrr, y = est_sss)) +
      ggplot2::labs(x = xlab, y = ylab, fill = "Group level") +
      ggplot2::coord_fixed()
  }

    plot_obj
  }


  # plot multiple component plots in cowplot or plot a single component plot
  if (make_cowplot == TRUE & n_components > 1) {
    plot_list <- NULL
    for (i in 1:n_components) {
      assign(paste0("plot_obj", i), sub_ggplot.cosinor.glmm.polar(i))
      plot_list[[i]] <- ggplotGrob(sub_ggplot.cosinor.glmm.polar(i))
    }
    final_obj <- cowplot::plot_grid(plotlist= plot_list, labels = paste("Component",seq(from = 1, to = n_components, by = 1)))
    print(final_obj)
  } else {
    final_obj <- sub_ggplot.cosinor.glmm.polar(component_index)
    print(final_obj)
  }

  # print information about the polar grid, if the user requests
  if (plot_info & show_polar_grid) {
    print(paste("Circular contours every",contour_interval,"unit(s)"))
    print(paste("Angle in units of",radial_units))
  }

}



