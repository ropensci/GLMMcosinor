#' Predict from a cosinor model
#'
#' Given a time variable and optional covariates, generate predicted values from
#' a cosinor fit. Default prediction is the mean value, optionally can predict
#' at a given month
#'
#' @param object An object of class \code{cosinor.glmm}.
#' @param newdata Optional new data.
#' @param ... other arguments passed to \code{glmmTMB:::predict.glmmTMB}.
#'
#' @return Returns predicted values from the cosinor model.
#'
#' @srrstats {RE4.9}
#' @srrstats {G1.4}
#'
#' @examples
#'
#' fit <- cosinor.glmm(Y ~ X + amp_acro(time,
#'   group = "X",
#'   n_components = 1,
#'   period = 12
#' ), data = vitamind)
#' predict(fit)
#'
#' @export
#'


predict.cosinor.glmm <- function(object, newdata, ...) {
  if (missing(newdata)) {
    return(stats::predict(object$fit, ...))
  }

  # all(names(newdata) %in% names(object$fit$frame))
  #TODO: modify dataset to include NA in random effects columns for which you
  # want a population estimate instead of a random effect. Population effect by
  # default, and the user should specify which variables they want as random
  # effects. This should be done in the autoplot script

  browser()
  # pass new dataset that's being used for prediction in this function
  nd <- update_formula_and_data(
    data = newdata,
    # get the formula that was originally to cosinor.glmm()
    formula = eval(object$cosinor.glmm.calls$cosinor.glmm$formula)
  )$newdata
  # only keep the newdata that's returned from update_formula_and_data()

  return(stats::predict(object$fit, newdata = nd, ...))
}
