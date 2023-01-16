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
ggplot.cosinor.glmm <- function(object, x_str = NULL, type = "response", xlims, pred.length.out = 200) {
  if(!missing(xlims)) {
    timeax <- seq(xlims[1], xlims[2], length.out = pred.length.out) #with multiple periods, largest is used for timeax simulation
  } else {
    timeax <- seq(0, 2*max(object$period), length.out = pred.length.out) #with multiple periods, largest is used for timeax simulation
  }
  covars <- names(object$group_stats)

  newdata <- data.frame(time = timeax, stringsAsFactors = FALSE)
  colnames(newdata)[1] <- object$time_name
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
      newdata$levels <- paste(newdata$levels, paste(d, "=", newdata[, d]))
    }
  }


  y_name <- object$response_var
  newdata[[y_name]] <- predict.cosinor.glmm(object, newdata = newdata, type = type) #adjust Y-axis name to correspond to whatever is in the dataframe

  if (missing(x_str) || is.null(x_str)) {
    ggplot2::ggplot(newdata, aes_string(x = paste(object$time_name), y = y_name)) +
      ggplot2::geom_line()
  } else {
    ggplot2::ggplot(newdata, aes_string(x = paste(object$time_name), y = y_name, col = "levels")) +
      ggplot2::geom_line()
  }
}


#' Generates a polar plot with elliptical confidence intervals
#'
#' @return
#' @export
#'
#' @examples
#ggplot.cosinor.glmm.polar <- function(object) {
#  df <- object$fit$frame
#  sum_obj <- summary(object)
#  cov <- sum_obj$transformed.covariance
#}
