#' @import ggplot2
#' @import ellipse
NULL

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
ggplot.cosinor.glmm <- function(object, x_str = NULL) {
  timeax <- seq(0, 2*max(object$period), length.out = 200) #with multiple periods, largest is used for timeax simulation
  covars <- names(object$group_stats)

  #newdata <- data.frame(
  #  time = timeax, rrr = cos(2 * pi * timeax / object$period),
  #  sss = sin(2 * pi * timeax / object$period)
  #)
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
    #for (k in unlist(object$group_stats[[x_str]])) {
      newdata$levels <- paste(newdata$levels, paste(d, "=", newdata[, d]))
    #}
    }
  }


  newdata$Y.hat <- predict.cosinor.glmm(object, newdata = newdata)

  if (missing(x_str) || is.null(x_str)) {
    ggplot(newdata, aes_string(x = paste(object$time_name), y = "Y.hat")) +
      geom_line()
  } else {
    ggplot(newdata, aes_string(x = paste(object$time_name), y = "Y.hat", col = "levels")) +
      geom_line()
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
