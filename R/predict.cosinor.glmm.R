#' Predict from a cosinor model
#'
#' Given a time variable and optional covariates, generate predicted values from
#' a cosinor fit. Default prediction is the mean value, optionally can predict
#' at a given month
#'
#' @param object An object of class \code{cosinor.glmm}
#' @param newdata Optional new data
#' @param ... other arguments
#'
#'
#' @examples
#'
#' fit <- cosinor.glmm(Y ~ time(time) + X + amp.acro(X), data = vitamind)
#' predict(fit)
#'
#' @export
#'

predict.cosinor.glmm <- function(object, newdata, ...) {
  if (missing(newdata) || is.null(newdata)) {
    Y <- object$fit$model[, paste(attr(object$Terms, "variables")[1 + attr(object$Terms, "response")])]
    Y.hat <- stats::fitted(object$fit)
  } else {
    Y <- newdata[, paste(attr(object$Terms, "variables")[1 + attr(object$Terms, "response")])]
    Y.hat <- stats::predict(object$fit, newdata = newdata)
  }

  mu.hat <- object$fit$coefficients[1]
  return(Y - Y.hat + mu.hat)
}
