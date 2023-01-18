#' Predict from a cosinor model
#'
#' Given a time variable and optional covariates, generate predicted values from
#' a cosinor fit. Default prediction is the mean value, optionally can predict
#' at a given month
#'
#' @param object An object of class \code{cosinor.glmm}
#' @param newdata Optional new data
#' @param ... other arguments passed to \code{glmmTMB:::predict.glmmTMB}
#'
#'
#' @examples
#'
#' fit <- cosinor.glmm(Y ~ X + amp.acro(X), data = vitamind)
#' predict(fit, newdata=vitamind)
#'
#' @export
#'

predict.cosinor.glmm <- function(object, newdata, ...) {
  if(missing(newdata)) {
    return(stats::predict(object$fit, ...))
  }

  # if the code runs this far, it's because the user has specified something in 'newdata'
  # you'll need to apply the same wrangling steps to ensure that the following code is TRUE:
  all(names(newdata) %in% names(object$fit$frame))

  # I have made a start on this by recording calls used in the parent functions:
    # check out 'match.call()'
    # check out the content of object$cosinor.glmm.calls

  nd <- update_formula_and_data(
    data = newdata, # pass new dataset that's being used for prediction in this function
    formula = eval(object$cosinor.glmm.calls$cosinor.glmm$formula) # get the formula that was originally to cosinor.glmm()
  )$newdata # only keep the newdata that's returned from update_formula_and_data()

  # NOTE: you'll need to first check that the new dataset has everything that's required to do the wrangling steps:
    # are all the columns that are required to do the wrangling steps in newdata?
    # are they of the same type as were in the original dataset used to fit the model?
  # make some useful error messages here - maybe catch the messages when evaluating the code used to wrangle the data using previous call
  # and then append on that this was happening when wrangling newdata.
    # you can use capture.ouput() to evaluate some code and record any output, including error messages.
  return(stats::predict(object$fit, newdata = nd, se.fit = FALSE))
}
