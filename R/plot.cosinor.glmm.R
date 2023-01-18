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

#Add ability to specify plotting of original dataset overalyed by trendline (off by default)
ggplot.cosinor.glmm <- function(object, x_str = NULL, type = "response", xlims, pred.length.out = 200, transpose_data = FALSE, data_opacity = 0.3) {
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
  newdata_processed[[y_name]] <- predict.cosinor.glmm(object, newdata = newdata_processed, type = type) #adjust Y-axis name to correspond to whatever is in the dataframe
  browser()
  if (transpose_data) {
  original_data <- object$newdata
  original_data_processed <- object$newdata
  original_data_processed["levels"] <-  original_data_processed[x_str]
  }

  if (!transpose_data) {
    if (missing(x_str) || is.null(x_str)) {
      plot_object <- ggplot2::ggplot(newdata_processed, aes_string(x = paste(object$time_name), y = y_name)) +
        ggplot2::geom_line()
    } else {
      plot_object <-ggplot2::ggplot() +
        geom_line(data = newdata_processed, aes_string(x = paste(object$time_name), y = y_name, col = "levels"))
    }
  }

  if (transpose_data) {
    if (missing(x_str) || is.null(x_str)) {
      plot_object <- ggplot2::ggplot(newdata_processed, aes_string(x = paste(object$time_name), y = y_name)) +
        ggplot2::geom_line()
        geom_point(data = original_data_processed, aes_string(x = paste(object$time_name), y = y_name), alpha = data_opacity) +
        facet_grid(rows = vars(NULL))
    } else {
      plot_object <- ggplot2::ggplot() +
        geom_line(data = newdata_processed, aes_string(x = paste(object$time_name), y = y_name, col = "levels")) +
        geom_point(data = original_data_processed, aes_string(x = paste(object$time_name), y = y_name, col = "levels"), alpha = data_opacity) +
        facet_grid(rows = vars(NULL))
    }




}
print(plot_object)
  }


#' Generates a polar plot with elliptical confidence intervals: PROOF OF CONCEPT
#'
#' @return
#' @export
#'
#' @examples
ggplot.cosinor.glmm.polar <- function(object, distinc_colours = TRUE, component_index = 1, xlims, pred.length.out = 200) {
  sum <- summary.cosinor.glmm(object)
  group <- names(object$group_stats)[1]
  component <- 1
  n_components <- object$n_components
  period <- object$period[1]
  get_ellipse <- function(group, level, component, n_components, period) {

     string_index <- paste0("[",group,"=")
     string_index_raw <- paste0(group)

    est_amp <- sum$transformed.table$estimate[which(grepl(string_index, rownames(sum$transformed.table), fixed = TRUE) & grepl("amp", rownames(sum$transformed.table), fixed = TRUE) )]
    l_est_amp <- sum$transformed.table$lower.CI[which(grepl(string_index, rownames(sum$transformed.table), fixed = TRUE)& grepl("amp", rownames(sum$transformed.table), fixed = TRUE) )]
    u_est_amp <- sum$transformed.table$upper.CI[which(grepl(string_index, rownames(sum$transformed.table), fixed = TRUE)& grepl("amp", rownames(sum$transformed.table), fixed = TRUE) )]

    est_acr   <- sum$transformed.table$estimate[which(grepl(string_index, rownames(sum$transformed.table), fixed = TRUE)& grepl("acr", rownames(sum$transformed.table), fixed = TRUE) )]
    l_est_acr <- sum$transformed.table$lower.CI[which(grepl(string_index, rownames(sum$transformed.table), fixed = TRUE)& grepl("acr", rownames(sum$transformed.table), fixed = TRUE) )]
    u_est_acr <- sum$transformed.table$upper.CI[which(grepl(string_index, rownames(sum$transformed.table), fixed = TRUE)& grepl("acr", rownames(sum$transformed.table), fixed = TRUE) )]

    est_rrr <- sum$raw.table$estimate[which(grepl(string_index_raw, rownames(sum$raw.table), fixed = TRUE) & grepl("rrr", rownames(sum$raw.table), fixed = TRUE))]
    est_sss <- sum$raw.table$estimate[which(grepl(string_index_raw, rownames(sum$raw.table), fixed = TRUE) & grepl("sss", rownames(sum$raw.table), fixed = TRUE))]

    #a_trans <- tan(((u_est_acr - l_est_acr)*2*pi/period)/2)*est_amp
    b_trans <- tan((u_est_acr-l_est_acr)/2)*est_amp
    a_trans <- est_amp - l_est_amp

    plot_obj <- ggplot() + geom_ellipse(aes(x0 = est_rrr , y0 = est_sss , a = a_trans, b = b_trans, angle = pi/2 - est_acr)) +
      coord_fixed()
    browser()
  }


  plot <- get_ellipse(group = group, level = 1, component = component, n_components = n_components, period = period)
  browser()




  #if(!missing(xlims)) {
  #  timeax <- seq(xlims[1], xlims[2], length.out = pred.length.out) #with multiple periods, largest is used for timeax simulation
  #} else {
  #  timeax <- seq(0, max(object$period), length.out = pred.length.out) #with multiple periods, largest is used for timeax simulation
  #}
#
  #browser()
  ## Create elliptical confidence interval
  #lower_params <- c(-1.6560,-2.0814  )
  #upper_params <- c(13.2081, 0.4634  )
  #estimates <- c( 5.7760 ,-0.1810)
#
  #y_lower = lower_params[1] * cos(timeax + lower_params[2])
  #y_upper = upper_params[1] * cos(timeax + upper_params[2])
#
#
  #ell <- ellipse(covm, centre = means, level = 0.95)
#
  #ell <- data.frame(amp, acr)
#
  ## Create polar plot
  #ggplot(data = ell, aes(xacr, amp)) +
  #  geom_path(data = ell, aes(acr, amp), color = "red") +
  #  coord_polar(theta = "x")

#df <- object$fit$frame
#sum_obj <- summary.cosinor.glmm(object)
#cov <- sum_obj$raw.covariance
#n_components <- object$n_components
#nc <- component_index
#group_names <- names(object$group_stats)
#group_stats <- object$group_stats
#plot_obj_final <- NULL
#plot_obj_comp <- NULL
#
#assertthat::assert_that(component_index <= n_components,
#  msg = "component_index must be an integer less than n_components specified in model")
#group <- object$group_original
#if (length(group) != n_components) {
#  if (length(group) == 1) {
#    group <- rep(group, n_components)
#    group_stats <- rep(group_stats, n_components)
#  } else {
#    stop("Grouping variable in amp.acro() must be of length 1 or the same as n_components")
#  }
#}
#
#covnames <- NULL
#plot_obj <- NULL
#center_vals <- NULL
#df_circ <- NULL
#
#for (nc in 1:n_components)
#{
#if (nc>0) {
#  center_vals <- rbind(center_vals,center_vals)
#  df_circ <- rbind(df_circ,df_circ)
#}
#  for (i in group_names) {
#    # get the names of the covariates alone
#    for (j in group_stats[nc]) {
#      covnames <- append(covnames,paste0(i,j))
#    }
#  }
#
#comp_ind <- NULL
#
#  for (i in 1:length(group_stats[[nc]])) {
#  comp_ind <- append(comp_ind,(paste0(covnames[i],":")))
#  }
#
#
#df_circ <- df_circ
#center_vals <- center_vals
#
#if (distinc_colours & length(comp_ind) <= 10) {
#colours_vector <- c("blue", "red", "green", "purple", "orange", "pink", "yellow","aquamarine", "brown", "black")
#} else {
#  colours_vector <- rep("blue", length(comp_ind))
#}
#
#
#for (i in 1:length(comp_ind)) {
#subset_matrix = cov[[nc]][which(grepl(comp_ind[i], rownames(cov[[nc]]))),
#                               which(grepl(comp_ind[i], colnames(cov[[nc]])))]
#center_vals[[nc]][[i]] <- sum_obj$raw.table$estimate[which(grepl(comp_ind[i], rownames(sum_obj$raw.table)))]
#coords <- ellipse::ellipse(subset_matrix, centre = c(center_vals[[nc]][[i]] [1],center_vals[[nc]][[i]] [2]), npoints = 500)
#
#
#
#
#df_circle <- NULL
#df_circle$X <- coords[,1]
#df_circle$Y <- coords[,2]
#df_circ[[nc]][[i]] <- data.frame(df_circle)
#plot_obj <- paste0(plot_obj,"geom_point(data = df_circ[[",nc,"]][[",i,"]], aes_string(x = 'X', y = 'Y'), colour = '",colours_vector[i],"') +",
#                   "geom_point(aes(x = center_vals[[",nc,"]][[",i,"]][1], y = center_vals[[",nc,"]][[",i,"]][2]), colour = '",colours_vector[i],"') + ")
#}
#
#}
#plot_obj_final <- paste0("ggplot2::ggplot() + ", toString(unlist(plot_obj)), "facet_grid(rows = vars(NULL))")
#eval(parse(text = plot_obj_final))
}
