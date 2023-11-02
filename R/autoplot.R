#' @importFrom ggplot2 autoplot
#' @export
ggplot2::autoplot

#' Plot a cosinor model
#'
#' Given a cglmm model fit, generate a plot of the data with the fitted
#' values.
#' Optionally allows for plotting by covariates
#'
#'
#' @param object An \code{cglmm} object.
#' @param ci_level The level for calculated confidence intervals. Defaults to
#' \code{0.95}.
#' @param x_str A \code{character} vector naming variable(s) to be plotted.
#' Default has no value and plots all groups.
#' @param type A \code{character} that will be passed as an argument to
#' \code{predict.cglmm()}, specifying the type of prediction
#' (e.g, "response", or "link"). See \code{?glmmTMB::predict.glmmTMB} for full
#' list of possible inputs.
#' @param xlims A vector of length two containing the limits for the x-axis.
#' @param pred.length.out An integer value that specifies the number of
#' predicted data points. The larger the value, the more smooth the fitted line
#' will appear. If missing, uses \code{points_per_min_cycle_length} to generate
#' a sensible default value.
#' @param superimpose.data A \code{logical}. If \code{TRUE}, data from the
#' original data used to fit the model (\code{object}) will be superimposed
#' over the predicted fit.
#' @param data_opacity A number between 0 and 1 inclusive that controls the
#' opacity of the superimposed data. (Used as the \code{alpha} when calling
#' \code{ggplot2::geom_point()}).
#' @param predict.ribbon A code{logical}. If \code{TRUE}, a prediction interval
#' is plotted.
#' @param points_per_min_cycle_length Used to determine the number of samples
#' to create plot if \code{pred.length.out} is missing.
#' @param ranef_plot Specify the random effects variables that you wish to plot.
#'  If not specified, only the fixed effects will be visualised.
#' @param quietly A \code{logical}. If \code{TRUE}, shows warning messages when
#' wrangling data and fitting model. Defaults to \code{TRUE}.
#' @param cov_list Specify the levels of the covariates that you wish to plot
#'  If not specified, the reference level of the covariate(s) will be used
#' \code{points_per_min_cycle_length} is the number of points plotted per the
#' minimum cycle length (period) of all cosinor components in the model.
#' @param ... Additional, ignored arguments.
#'
#' @srrstats {G1.4}
#' @srrstats {RE6.1}
#' @srrstats {RE6.0}
#' @srrstats {RE6.2}
#' @srrstats {RE6.3}
#'
#' @return Returns a `ggplot` object.
#' @examples
#' model <- cglmm(
#'   vit_d ~ X + amp_acro(time, group = "X", period = 12),
#'   data = vitamind
#' )
#' autoplot(model, x_str = "X")
#' @export
autoplot.cglmm <- function(object,
                           ci_level = 0.95,
                           x_str,
                           type = "response",
                           xlims,
                           pred.length.out,
                           points_per_min_cycle_length = 20,
                           superimpose.data = FALSE,
                           data_opacity = 0.3,
                           predict.ribbon = TRUE,
                           ranef_plot = NULL,
                           cov_list = NULL,
                           quietly = TRUE,
                           ...) {
  # Validating user inputs
  assertthat::assert_that(inherits(object, "cglmm"),
    msg = "'object' must be of class 'cglmm'"
  )

  validate_ci_level(ci_level)

  if (!missing(x_str)) {
    for (i in x_str) {
      assertthat::assert_that(i %in% names(object$group_stats),
        msg = paste(
          "'x_str' must be string corresponding to a group name",
          "in cglmm object"
        )
      )
    }
  }


  if (!is.null(cov_list)){
    for (i in names(cov_list)) {
      assertthat::assert_that(i %in% colnames(object$newdata) &&
                                i %in% object$covariates,
                              msg = paste(
                                "'cov_list' must be a list corresponding to",
                                "covariates specified in the cglmm object: ",
                                paste(object$covariates, collapse = ", ")
                              )
      )
    }
  }

  #if cov_list isn't specified, the first entry for each covariate will assigned
  #as the reference level
  if(is.null(cov_list) && !is.null(object$covariates)) {
    message_cov_list <- NULL
    for (i in object$covariates) {
      reference_level <- object$newdata[[i[1]]][1]

      cov_list[[i]] <- reference_level

      #this ensures that the class of the reference level is maintained in
      #the error message. Consequently, the user can use this 'cov_list' arg
      #or a modified version in their original autoplot() call if they wish to.
      formatted_reference_level <- ifelse(is.character(reference_level),
                                    paste0("'", reference_level, "'"),
                                    as.character(reference_level))

      message_cov_list[[i]] <- paste0(i, " = ", formatted_reference_level)
    }
    if(!quietly) {
    message(paste0("'cov_list' was not specified, but there are covariates in ",
                  "the original model; the first element of each covariate",
                  "column from the original dataframe will be used as ",
                  "reference levels:", '\n',
                  "cov_list = list(",
                paste(message_cov_list, collapse = ', '), ")"))
    }
  }




  if (!is.null(ranef_plot)){
    for (i in ranef_plot) {
      assertthat::assert_that(i %in% object$ranef_groups,
                              msg = paste(
                                "'ranef_plot' must be string corresponding to",
                                "the name of a random effect column in the",
                                "original dataset from the cglmm object",
                                "Column(s) with random effect variable:",
                                paste(object$ranef_groups, collapse = ", ")
                              )
      )
    }
  }

  assertthat::assert_that(is.character(type),
    msg = paste(
      "'type' must be a string. See type in ?predict for more",
      "information about valid inputs"
    )
  )

  if (!missing(xlims)) {
    assertthat::assert_that(
      length(xlims) == 2 & is.numeric(xlims) & xlims[1] < xlims[2],
      msg = paste(
        "'xlims' must be a vector with the first element being the",
        "lower x coordinate, and the second being the upper",
        "x coordinate"
      )
    )
  }

  if (!missing(pred.length.out)) {
    assertthat::assert_that(
      pred.length.out == floor(pred.length.out) & pred.length.out > 0,
      msg = "'pred.length.out' must be an integer greater than 0 "
    )
  }

  assertthat::assert_that(is.logical(superimpose.data),
    msg = "'superimpose.data' must be a logical argument, either TRUE or FALSE"
  )
  assertthat::assert_that(
    is.numeric(data_opacity) & data_opacity >= 0 & data_opacity <= 1,
    msg = "'data_opacity' must be a number between 0 and 1 inclusive"
  )
  assertthat::assert_that(is.logical(predict.ribbon),
    msg = "'predict.ribbon' must be a logical argument, either TRUE or FALSE"
  )

  # default to plotting all groups if x_str is missing
  if (missing(x_str)) {
    x_str <- names(object$group_stats)
  }

  # Vector of time values from the original dataset
  time_vec <- object$newdata[[object$time_name]]
  min_period_cycle_count <- round(
    (max(time_vec) - min(time_vec)) / min(object$period)
  )


  # By default, the predicted length out is calculated to give sufficient
  # resolution to the smallest period.
  if (missing(pred.length.out)) {
    pred.length.out <- max(
      min_period_cycle_count * points_per_min_cycle_length,
      400
    )
  }


  # generate the time values for the x-axis
  if (!missing(xlims)) {
    # with multiple periods, largest is used for timeax simulation
    timeax <- seq(xlims[1], xlims[2], length.out = pred.length.out)
  } else {
    # the fitted model has bounds corresponding to initial dataframe
    timeax <- seq(min(object$newdata[object$time_name]),
      max(object$newdata[object$time_name]),
      length.out = pred.length.out
    )
  }


  # this is the function that generates the plots and can be looped iteratively
  # for different x_str
  data_processor_plot <- function(x, newdata, x_str) {
    # get the names of the covariates (factors)
    covars <- names(x$group_stats)

    # obtain the reference level of each factor
    for (j in covars) {
      ref_level <- unlist(x$group_stats[j])[[1]]
      newdata[, j] <- factor(ref_level)
    }
    # process the data. This step mimics the first step of a cglmm() call
    newdata <- update_formula_and_data(
      # pass new dataset that's being used for prediction in this function
      data = newdata,
      # get the formula that was originally to cglmm()
      formula = eval(x$cglmm.calls$cglmm$formula)
    )$newdata
    # only keep the newdata that's returned from update_formula_and_data()

    if (!is.null(ranef_plot)) {
      for (d in x_str) {
        newdata <- newdata[, -which(names(newdata) == d)]
      }
    }
    # repeat dataset for every level in x_str (factor), with an additional
    # column in each corresponding to factor name and level
    if (!is.null(x_str) && is.null(ranef_plot)) {
      for (d in x_str) {
        for (k in unlist(x$group_stats[[d]])[-1]) {
          tdat <- newdata
          tdat[, d] <- factor(k)
          newdata <- rbind(newdata, tdat, stringsAsFactors = FALSE)
        }
      }

      newdata$levels <- ""
      i <- 1 # used as a counter for formatting purposes
      for (d in x_str) {
        newdata$levels <- paste0(
          newdata$levels,
          d, "=", newdata[, d]
        )
        # if there are multiple group levels, separate them by "|"
        if (i < length(x_str)) {
          newdata$levels <- paste0(newdata$levels, " | ")
        }
        i <- i + 1
      }
    }


    # adding the random effect variables in the dataframe
    if (any(!is.na(object$ranef_groups))) {
      replicated_dfs_total <- NULL
      data_ranef <- newdata

      unique_counts <- sapply(
        object$newdata[ranef_plot],
        function(col) length(unique(col))
      )

      # Identify the column with the maximum number of unique values
      max_unique_col <- names(unique_counts)[which.max(unique_counts)]
      # Create a new vector excluding the max_unique_col
      new_ranef_groups <- ranef_plot[ranef_plot != max_unique_col]

      for (i in x$ranef_groups) {
        replicated_dfs <- NULL
        subjects <- as.factor(levels(x$newdata[[i]]))

        #
        if (!is.null(ranef_plot) && i %in% ranef_plot && i == max_unique_col) {
          for (j in subjects) {
            appendvec <- data_ranef
            appendvec[[i]] <- j
            for (k in new_ranef_groups) {
              appendvec[[k]] <- unique(
                object$newdata[object$newdata[[i]] == j, k]
              )
            }
            if (!is.null(x_str)) {
              for (d in x_str) {
                appendvec[[d]] <- unique(
                  object$newdata[object$newdata[[i]] == j, d]
                )
              }
            }

            replicated_dfs[[j]] <- appendvec
          }
        } else {
          appendvec <- data_ranef
          appendvec[[i]] <- rep(NA, length(ncol(appendvec)))
          replicated_dfs[[i]] <- appendvec
        }



        replicated_dfs_total <- do.call(rbind, replicated_dfs)
        data_ranef <- replicated_dfs_total
      }
      # combined_df <- do.call(rbind, replicated_dfs_total)
      newdata <- data_ranef
    }
    newdata
  }

  # format the newdata dataframe before passing to data_processor_plot()
  unique_counts <- sapply(
    object$newdata[ranef_plot],
    function(col) length(unique(col))
  )

  # Identify the column with the maximum number of unique values
  max_unique_col <- names(unique_counts)[which.max(unique_counts)]

  newdata <- data.frame(time = timeax, stringsAsFactors = FALSE)
  colnames(newdata)[1] <- object$time_name
  #
  #adding covariate columns
  for (i in names(cov_list)) {
    fixed_value <- cov_list[[i]]
    newdata[[i]] <- fixed_value
  }

  #
  newdata_processed <- data_processor_plot(object, newdata, x_str)

  # get the response data from the cglmm object
  y_name <- object$response_var
  # get the predicted response values using the predict.cglmm() function
  pred_obj <- stats::predict(
    object,
    newdata = newdata_processed,
    type = type,
    se.fit = TRUE
  )

  # adjust Y-axis name to correspond to whatever is in the dataframe
  newdata_processed[[y_name]] <- pred_obj$fit


  zt <- stats::qnorm((1 - ci_level) / 2, lower.tail = F)

  y_min <- pred_obj$fit - zt * pred_obj$se.fit
  y_max <- pred_obj$fit + zt * pred_obj$se.fit

  # get the original data from the cglmm object to be superimposed


  ##


  if (any(!is.na(object$ranef_groups))) {

    if (superimpose.data) {
      original_data <- object$newdata
      original_data_processed <- object$newdata
      original_data_processed$levels <- ""
      i <- 1 # used as a counter for formatting purposes
      for (d in x_str) {
        original_data_processed$levels <- paste0(
          original_data_processed$levels,
          d,
          "=",
          original_data_processed[, d]
        )
        # if there are multiple group levels, separate them by "|"
        if (i < length(x_str)) {
          original_data_processed$levels <- paste0(
            original_data_processed$levels, " | "
          )
        }
        i <- i + 1
      }
    }

    #generating the plots
    if (!is.null(ranef_plot)) {
      if (missing(x_str) || is.null(x_str)) {
        plot_object <- ggplot2::ggplot() +
          ggplot2::geom_line(
            data = newdata_processed,
            ggplot2::aes(
              x = !!rlang::sym(paste(object$time_name)),
              y = !!rlang::sym(y_name),
              col = !!rlang::sym(max_unique_col)
            )
          )
        if (superimpose.data) {
          plot_object <- plot_object + ggplot2::geom_line() +
            ggplot2::geom_point(
              data = original_data_processed,
              ggplot2::aes(
                x = !!rlang::sym(paste(object$time_name)),
                y = !!rlang::sym(y_name),
                col = !!rlang::sym(max_unique_col)
              ),
              alpha = data_opacity
            ) +
            ggplot2::facet_grid(rows = ggplot2::vars(NULL))
        }
      } else {
        plot_object <- ggplot2::ggplot() +
          ggplot2::geom_line(
            data = newdata_processed,
            ggplot2::aes(
              x = !!rlang::sym(paste(object$time_name)),
              y = !!rlang::sym(y_name),
              col = !!rlang::sym(max_unique_col),
              linetype = !!rlang::sym(x_str)
            )
          )


        if (superimpose.data) {
          plot_object <- plot_object + ggplot2::geom_line() +
            ggplot2::geom_point(
              data = original_data_processed,
              ggplot2::aes(
                x = !!rlang::sym(paste(object$time_name)),
                y = !!rlang::sym(y_name),
                col = !!rlang::sym(max_unique_col),
                shape = !!rlang::sym(x_str)
              ),
              alpha = data_opacity
            ) +
            ggplot2::facet_grid(rows = ggplot2::vars(NULL))
        }
      }
    } else {#if a model has random effects, but no ranef is specified, then
            #fixed effects will be used to generate the plot
      if (missing(x_str) || is.null(x_str)) {
        plot_object <- ggplot2::ggplot() +
          ggplot2::geom_line(
            data = newdata_processed,
            ggplot2::aes(
              x = !!rlang::sym(paste(object$time_name)),
              y = !!rlang::sym(y_name)
            )
          )
        if (superimpose.data) {
          plot_object <- plot_object + ggplot2::geom_line() +
            ggplot2::geom_point(
              data = original_data_processed,
              ggplot2::aes(
                x = !!rlang::sym(paste(object$time_name)),
                y = !!rlang::sym(y_name)
              ),
              alpha = data_opacity
            ) +
            ggplot2::facet_grid(rows = ggplot2::vars(NULL))
        }
      } else {
        plot_object <- ggplot2::ggplot() +
          ggplot2::geom_line(
            data = newdata_processed,
            ggplot2::aes(
              x = !!rlang::sym(paste(object$time_name)),
              y = !!rlang::sym(y_name),
              col = !!rlang::sym(x_str)
            )
          )


        if (superimpose.data) {
          plot_object <- plot_object + ggplot2::geom_line() +
            ggplot2::geom_point(
              data = original_data_processed,
              ggplot2::aes(
                x = !!rlang::sym(paste(object$time_name)),
                y = !!rlang::sym(y_name),
                col = !!rlang::sym(x_str)
              ),
              alpha = data_opacity
            ) +
            ggplot2::facet_grid(rows = ggplot2::vars(NULL))
        }
      }
    }
  } else { #this separates random effect models from fixed effects models
    if (superimpose.data) {
      original_data <- object$newdata
      original_data_processed <- object$newdata
      original_data_processed$levels <- ""
      i <- 1 # used as a counter for formatting purposes
      for (d in x_str) {
        original_data_processed$levels <- paste0(
          original_data_processed$levels,
          d,
          "=",
          original_data_processed[, d]
        )
        # if there are multiple group levels, separate them by "|"
        if (i < length(x_str)) {
          original_data_processed$levels <- paste0(
            original_data_processed$levels, " | "
          )
        }
        i <- i + 1
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

    # superimpose original data from cglmm() object
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
  }
  plot_object
}
