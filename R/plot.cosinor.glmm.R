#' @import ggplot2
NULL

#' Plot a cosinor model
#'
#' Given a cosinor.glmm model fit, generate a plot of the data with the fitted values.
#' Optionally allows for plotting by covariate levels 0 and 1.
#'
#'
#' @param object An object of class \code{cosinor.glmm}
#' @param x_str Character vector naming the covariate(s) to be plotted. May be NULL to plot overall curve
#'
#'
#' @examples
#'
#' fit <- cosinor.glmm(Y ~ time(time) + X + amp.acro(X), data = vitamind)
#' ggplot.cosinor.glmm(fit, "X")
#'
#' @export ggplot.cosinor.glmm
#' @export
#'
#'
ggplot.cosinor.glmm <- function(object, x_str = NULL) {
  browser()
  timeax <- seq(0, max(object$period), length.out = 200) #with multiple periods, largest is used for timeax simulation
  covars <- names(object$group_stats)

  #newdata <- data.frame(
  #  time = timeax, rrr = cos(2 * pi * timeax / object$period),
  #  sss = sin(2 * pi * timeax / object$period)
  #)

  newdata <- data.frame(time = timeax)
  for (j in covars) {
    newdata[, j] <- 0
  }
  newdata <- update_formula_and_data(
    data = newdata, # pass new dataset that's being used for prediction in this function
    formula = eval(object$cosinor.glmm.calls$cosinor.glmm$formula) # get the formula that was originally to cosinor.glmm()
  )$newdata # only keep the newdata that's returned from update_formula_and_data()



  if (!is.null(x_str)) {
    for (d in x_str) {
      tdat <- newdata
      tdat[, d] <- 1
      newdata <- rbind(newdata, tdat)
    }
    newdata$levels <- ""
    for (d in x_str) {
      newdata$levels <- paste(newdata$levels, paste(d, "=", newdata[, d]))
    }
  }


  #newdata$Y.hat <- stats::predict(object$fit, newdata = newdata)
  newdata$Y.hat <- predict.cosinor.glmm(object, newdata = newdata)

  if (missing(x_str) || is.null(x_str)) {
    ggplot(newdata, aes_string(x = "time", y = "Y.hat")) +
      geom_line()
  } else {
    ggplot(newdata, aes_string(x = "time", y = "Y.hat", col = "levels")) +
      geom_line()
  }
}
