#' @importFrom ggplot2 autoplot
#' @export
ggplot2::autoplot

#' Plot a cosinor model
#'
#' Given a cosinor.glmm model fit, generate a plot of the data with the fitted
#' values.
#' Optionally allows for plotting by covariates
#'
#'
#' @param object An \code{cosinor.glmm} object.
#' @param ci_level The level for calculated confidence intervals. Defaults to
#' \code{0.95}.
#' @param x_str A \code{character} vector naming variable(s) to be plotted.
#' Default has no value and plots all groups.
#' @param type A \code{character} that will be passed as an argument to
#' \code{predict.cosinor.glmm()}, specifying the type of prediction
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
#' model <- cosinor.glmm(
#'   Y ~ X + amp_acro(time, group = "X", period = 12),
#'   data = vitamind
#' )
#' autoplot(model, x_str = "X")
#' @export
autoplot.cosinor.glmm <- function(object,
                                  ci_level = 0.95,
                                  x_str,
                                  type = "response",
                                  xlims,
                                  pred.length.out,
                                  points_per_min_cycle_length = 20,
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
        msg = paste("'x_str' must be string corresponding to a group name",
                    "in cosinor.glmm object")
      )
    }
  }
  assertthat::assert_that(is.character(type),
    msg = paste("'type' must be a string. See type in ?predict for more",
                "information about valid inputs")
  )
  if (!missing(xlims)) {
    assertthat::assert_that(
      length(xlims) == 2 & is.numeric(xlims) & xlims[1] < xlims[2],
      msg = paste("'xlims' must be a vector with the first element being the",
                  "lower x coordinate, and the second being the upper",
                  "x coordinate")
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


  #if there are multiple random effects groups, the time vector must be scaled
  #appropriately. Otherwise, the number of simulated datapoints may be
  #extremely large. For example, if pred.length.out = 400, and there are two
  #random effects groups with 30 subjects each, then the final dataframe will
  #be 400*30*30 = 360000 rows. Instead, this code restricts the number of data
  #points to 10000. In future versions, this could be set to a variable that
  #the user can input
  ranef_scale <- 1
  for (i in object$ranef_groups) {
  ranef_scale <- ranef_scale*length(levels(object$newdata[[i]]))
  }

  if(ranef_scale*pred.length.out >= 10000) {
    pred.length.out <- 10000 %/% (ranef_scale)
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
    # process the data. This step mimics the first step of a cosinor.glmm() call
    newdata <- update_formula_and_data(
      # pass new dataset that's being used for prediction in this function
      data = newdata,
      # get the formula that was originally to cosinor.glmm()
      formula = eval(x$cosinor.glmm.calls$cosinor.glmm$formula)
    )$newdata
    # only keep the newdata that's returned from update_formula_and_data()

    # repeat dataset for every level in x_str (factor), with an additional
    # column in each corresponding to factor name and level
    if (!is.null(x_str)) {
      for (d in x_str) {
        for (k in unlist(x$group_stats[[d]])[-1]) {
          tdat <- newdata
          tdat[, d] <- factor(k)
          newdata <- rbind(newdata, tdat, stringsAsFactors = FALSE)
        }
      }

      newdata$levels <- ""
      i = 1 #used as a counter for formatting purposes
      for (d in x_str) {
        newdata$levels <- paste0(
          newdata$levels,
           d, "=", newdata[, d]

        )
        # if there are multiple group levels, separate them by "|"
        if (i < length(x_str)) {
          newdata$levels <- paste0(newdata$levels, " | ")
        }
        i = i + 1
      }
    }

    ##
    #ranef_part <- lapply(lme4::findbars(object$formula), deparse1)
    #ranef_part_group <- gsub(".*\\|\\s*(.*)", "\\1", ranef_part)
    ##
   # ## Here, 'x_str' is taken to be the string corresponding to the subjects
   # if (!is.null(ranef_part)) {
   #   if(is.null(x_str)) {
   #     x_str <- ranef_part_group[1]
   #   }
    #
   #   ###
   #   mixed_plot <- TRUE
   #   mixed_effects_vec <- NULL
   #   replicated_dfs <- NULL

    if(any(!is.na(object$ranef_groups))) {
   replicated_dfs_total <- NULL
    data_ranef <- newdata
   for (i in x$ranef_groups) {
     replicated_dfs <- NULL
     subjects <- as.factor(levels(x$newdata[[i]]))
     for (j in subjects){
       appendvec <- data_ranef
       appendvec[[i]] <- j
       replicated_dfs[[j]] <- appendvec
     }
     replicated_dfs_total <- do.call(rbind,replicated_dfs)
     data_ranef <- replicated_dfs_total

   }
   #combined_df <- do.call(rbind, replicated_dfs_total)
   newdata <- data_ranef
    }
   #   subjects <- as.factor(levels(object$newdata[[x_str]]))
   #   for (m in subjects) {
  #
   #     appendvec <- newdata
   #     appendvec$x_str <- m
   #     replicated_dfs[[m]] <- appendvec
   #   }
   #   combined_df <- do.call(rbind, replicated_dfs)
   #   ###
   #   newdata <- combined_df
   # }
    ##

    newdata
  }

  # format the newdata dataframe before passing to data_processor_plot()
  newdata <- data.frame(time = timeax, stringsAsFactors = FALSE)
  colnames(newdata)[1] <- object$time_name
  newdata_processed <- data_processor_plot(object, newdata, x_str)

  # get the response data from the cosinor.glmm object
  y_name <- object$response_var
  # get the predicted response values using the predict.cosinor.glmm() function
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

  # get the original data from the cosinor.glmm object to be superimposed



##

if(any(!is.na(object$ranef_groups))) {
  # Prompt the user to choose from ranef_groups
  if(length(unique(object$ranef_groups))>1) {
  selected_group <- utils::select.list(
    object$ranef_groups,
    title = "Select a random varialbe to plot:",
    multiple = FALSE
  )

  # Check if the selected_group is valid
  if (selected_group %in% object$ranef_groups) {
    cat("You selected:", selected_group, "\n")

    # Determine the other groups
    other_groups <- setdiff(object$ranef_groups, selected_group)

    # Create an empty list to store user-defined values for other groups
    other_values <- list()

    # Prompt the user to input numbers for other groups
    for (group in other_groups) {
      fixed_number <- as.numeric(readline(paste("Enter the desired subject number from", group, ": ")))
      # Check if fixed_number is a valid number within unique elements of ranef_groups
      if (!is.na(fixed_number) && fixed_number %in% levels(object$newdata[[group]])) {
        cat("You entered:", fixed_number, "for", group, "\n")
        other_values[[group]] <- fixed_number
      } else {
        cat("Invalid input. Please enter a valid number within unique elements of ranef_groups.\n")
      }
      newdata_processed <-
        newdata_processed[newdata_processed[[group]] == fixed_number,]

    }

    # Your plotting code for the selected group and other_values goes here
  } else {
    cat("Invalid selection. Please choose a valid group.\n")
  }
  } else {
  selected_group <- object$ranef_groups
}


  ##plotting

  if (missing(x_str) || is.null(x_str)) {
  plot_object <- ggplot2::ggplot() +
    ggplot2::geom_line(
      data = newdata_processed,
      ggplot2::aes(
        x = !!rlang::sym(paste(object$time_name)),
        y = !!rlang::sym(y_name),
        col = !!rlang::sym(selected_group)
      )
    )
  }

   else {
  plot_object <- ggplot2::ggplot() +
    ggplot2::geom_line(
      data = newdata_processed,
      ggplot2::aes(
        x = !!rlang::sym(paste(object$time_name)),
        y = !!rlang::sym(y_name),
        col = !!rlang::sym(selected_group),
        linetype = !!rlang::sym(x_str)
      )
    )
  }




}
##
else {
  if (superimpose.data) {
    original_data <- object$newdata
    original_data_processed <- object$newdata
    original_data_processed$levels <- ""
    i = 1 #used as a counter for formatting purposes
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
          original_data_processed$levels, " | ")
      }
      i = i + 1


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
}
  plot_object
}
