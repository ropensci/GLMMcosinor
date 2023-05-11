#' @importFrom ggplot2 autoplot
#' @export
ggplot2::autoplot

#' Plot a cosinor model
#'
#' Given a cosinor.glmm model fit, generate a plot of the data with the fitted values.
#' Optionally allows for plotting by covariates
#'
#'
#' @param object An \code{cosinor.glmm} object.
#' @param ci_level The level for calculated confidence intervals. Defaults to 0.95.
#' @param x_str Character vector naming covariate(s) to be plotted. Default has no value and plots all groups
#' @param type Character that will be passed as an argument to the predict.cosinor.glmm() function, specifying the type of prediction (e.g, "response", or "link")
#' @param xlims A vector of length two containing the lower and upper x limit to be plotted
#' @param pred.length.out An integer value that specifies the number of predicted datapoints. The larger the value, the more smooth the fit will appear
#' @param superimpose.data A logical argument (TRUE or FALSE). If TRUE, data from the original cosinor.glmm() object will be superimposed over the predicted fit
#' @param data_opacity A number bewteen 0 and 1 inclusive that controls the opacity of the superimposed data
#' @param predict.ribbon A logical argument (TRUE or FALSE). If TRUE, a prediction interval is plotted
#' @param ... Additional, ignored arguments.
#'
#' @srrstats {G1.4} *Software should use [`roxygen2`](https://roxygen2.r-lib.org/) to document all functions.*
#'
#' @srrstats {RE6.0} *Model objects returned by Regression Software (see* **RE4***) should have default `plot` methods, either through explicit implementation, extension of methods for existing model objects, or through ensuring default methods work appropriately.*
#' @srrstats {RE6.2} *The default `plot` method should produce a plot of the `fitted` values of the model, with optional visualisation of confidence intervals or equivalent.*
#' @srrstats {RE6.3} *Where a model object is used to generate a forecast (for example, through a `predict()` method), the default `plot` method should provide clear visual distinction between modelled (interpolated) and forecast (extrapolated) values.*
#'
#' @return Returns a `ggplot` graphics object.
#' @examples
#'
#' model <- cosinor.glmm(Y ~ X + amp.acro(time, group = "X"), data = vitamind)
#' autoplot(model, x_str = "X")
#'
#' @export
#'
#'

autoplot.cosinor.glmm <- function(object,
                                  ci_level = 0.95,
                                  x_str,
                                  type = "response",
                                  xlims,
                                  pred.length.out = 200,
                                  superimpose.data = FALSE,
                                  data_opacity = 0.3,
                                  predict.ribbon = TRUE,
                                  ...) {

  # Validating user inputs
  assertthat::assert_that(inherits(object, "cosinor.glmm"),
    msg = "'object' must be of class 'cosinor.glmm'"
  )

  validate_ci_level(ci_level)

  if (!missing(x_str)) {
    for (i in x_str) {
      assertthat::assert_that(i %in% names(object$group_stats),
        msg = "'x_str' must be string corresponding to a group name in cosinor.glmm object"
      )
    }
  }
  assertthat::assert_that(is.character(type),
    msg = "'type' must be a string. See type in ?predict for more information about valid inputs"
  )
  if (!missing(xlims)) {
    assertthat::assert_that(length(xlims) == 2 & is.numeric(xlims) & xlims[1] < xlims[2],
      msg = "'xlims' must be a vector with the first element being the lower x coordinate, and the second being the upper x coordinate"
    )
  }
  assertthat::assert_that(pred.length.out == floor(pred.length.out) & pred.length.out > 0,
    msg = "'pred.length.out' must be an integer greater than 0 "
  )
  assertthat::assert_that(is.logical(superimpose.data),
    msg = "'superimpose.data' must be a logical argument, either TRUE or FALSE"
  )
  assertthat::assert_that(is.numeric(data_opacity) & data_opacity >= 0 & data_opacity <= 1,
    msg = "'data_opacity' must be a number between 0 and 1 inclusive"
  )
  assertthat::assert_that(is.logical(predict.ribbon),
    msg = "'predict.ribbon' must be a logical argument, either TRUE or FALSE"
  )

  # default to plotting all groups if x_str is missing
  if (missing(x_str)) {
    x_str <- names(object$group_stats)
  }


  # generate the time values for the x-axis
  if (!missing(xlims)) {
    timeax <- seq(xlims[1], xlims[2], length.out = pred.length.out) # with multiple periods, largest is used for timeax simulation
  } else {
    timeax <- seq(0, max(object$period), length.out = pred.length.out) # with multiple periods, largest is used for timeax simulation
  }

  # this is the function that generates the plots and can be looped iteratively for different x_str
  data_processor_plot <- function(x, newdata, x_str) {
    # get the names of the covariates (factors)
    covars <- names(x$group_stats)

    # obtain the reference level of each factor
    for (j in covars) {
      ref_level <- unlist(x$group_stats[j])[[1]]
      newdata[, j] <- factor(ref_level)
    }

    # process the data. This step mimics the first step of a cosinor.glmm() call
    newdata <- update_formula_and_data(
      data = newdata, # pass new dataset that's being used for prediction in this function
      formula = eval(x$cosinor.glmm.calls$cosinor.glmm$formula) # get the formula that was originally to cosinor.glmm()
    )$newdata # only keep the newdata that's returned from update_formula_and_data()

    # repeat dataset for every level in x_str (factor), with an additional column in each corresponding to factor name and level
    if (!is.null(x_str)) {
      for (d in x_str) {
        for (k in unlist(x$group_stats[[d]])[-1]) {
          tdat <- newdata
          tdat[, d] <- factor(k)
          newdata <- rbind(newdata, tdat, stringsAsFactors = FALSE)
        }
      }

      newdata$levels <- ""
      for (d in x_str) {
        newdata$levels <- paste0(newdata$levels, "[", d, "=", newdata[, d], "] ")
      }
    }
    newdata
  }

  # format the newdata dataframe before passing to data_processor_plot()
  newdata <- data.frame(time = timeax, stringsAsFactors = FALSE)
  colnames(newdata)[1] <- object$time_name
  newdata_processed <- data_processor_plot(object, newdata, x_str)
  y_name <- object$response_var # get the response data from the cosinor.glmm object

  # get the predicted response values using the predict.cosinor.glmm() function
  pred_obj <- stats::predict(
    object,
    newdata = newdata_processed,
    type = type,
    se.fit = TRUE
  )
  newdata_processed[[y_name]] <- pred_obj$fit # adjust Y-axis name to correspond to whatever is in the dataframe
  # newdata_processed$y_min <- pred_obj$fit - 1.96 * pred_obj$se.fit # determine the upper predicted interval
  # newdata_processed$y_max <- pred_obj$fit + 1.96 * pred_obj$se.fit # determine the lower predicted interval

  zt <- stats::qnorm((1 - ci_level) / 2, lower.tail = F)

  y_min <- pred_obj$fit - zt * pred_obj$se.fit
  y_max <- pred_obj$fit + zt * pred_obj$se.fit
  # get the original data from the cosinor.glmm object to be superimposed
  if (superimpose.data) {
    original_data <- object$newdata
    original_data_processed <- object$newdata
    original_data_processed$levels <- ""
    for (d in x_str) {
      original_data_processed$levels <- paste0(
        original_data_processed$levels,
        "[",
        d,
        "=",
        original_data_processed[, d],
        "] "
      )
    }
  }
  # get the plot object
  if (!superimpose.data) {
    if (missing(x_str) || is.null(x_str)) {
      plot_object <- ggplot2::ggplot(
        data = newdata_processed,
        ggplot2::aes(
          x = !!rlang::sym(paste(object$time_name)),
          y = !!rlang::sym(y_name)
        )
      ) +
        ggplot2::geom_line()
    } else {
      plot_object <- ggplot2::ggplot() +
        ggplot2::geom_line(
          data = newdata_processed,
          ggplot2::aes(
            x = !!rlang::sym(paste(object$time_name)),
            y = !!rlang::sym(y_name),
            col = levels
          )
        )
    }
  }

  # superimpose original data from cosinor.glmm() object
  if (superimpose.data) {
    if (missing(x_str) || is.null(x_str)) {
      plot_object <- ggplot2::ggplot(
        data = newdata_processed,
        ggplot2::aes(
          x = !!rlang::sym(paste(object$time_name)),
          y = !!rlang::sym(y_name)
        )
      ) +
        ggplot2::geom_line() +
        ggplot2::geom_point(
          data = original_data_processed,
          ggplot2::aes(
            x = !!rlang::sym(paste(object$time_name)),
            y = !!rlang::sym(y_name)
          ),
          alpha = data_opacity
        ) +
        ggplot2::facet_grid(rows = ggplot2::vars(NULL))
    } else {
      plot_object <- ggplot2::ggplot() +
        ggplot2::geom_line(
          data = newdata_processed,
          ggplot2::aes(
            x = !!rlang::sym(paste(object$time_name)),
            y = !!rlang::sym(y_name),
            col = levels
          )
        ) +
        ggplot2::geom_point(
          data = original_data_processed,
          ggplot2::aes(
            x = !!rlang::sym(paste(object$time_name)),
            y = !!rlang::sym(y_name),
            col = levels
          ),
          alpha = data_opacity
        ) +
        ggplot2::facet_grid(rows = ggplot2::vars(NULL))
    }
  }

  # plot the prediction interval
  if (predict.ribbon) {
    if (missing(x_str) || is.null(x_str)) {
      plot_object <- plot_object +
        ggplot2::geom_ribbon(
          data = newdata_processed,
          ggplot2::aes(
            x = !!rlang::sym(object$time_name),
            ymin = y_min,
            ymax = y_max
          ),
          alpha = 0.5
        ) +
        ggplot2::facet_grid(rows = ggplot2::vars(NULL))
    } else {
      plot_object <- plot_object +
        ggplot2::geom_ribbon(
          data = newdata_processed,
          ggplot2::aes(
            x = !!rlang::sym(object$time_name),
            ymin = y_min,
            ymax = y_max,
            col = levels,
            fill = levels
          ),
          alpha = 0.5
        ) +
        ggplot2::facet_grid(rows = ggplot2::vars(NULL))
    }
  }
  plot_object
}
