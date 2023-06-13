#' Test for differences in a cosinor model
#'
#' Given a time variable and optional covariates, generate inference a cosinor
#' fit. For the covariate named (or vector of covariates), this function
#' performs a Wald test comparing the group with covariates equal to 1 to the
#' group with covariates equal to 0. This may not be the desired result for
#' continuous covariates.
#'
#'
#' @param x An \code{cosinor.glmm} object.
#' @param x_str A \code{character}. The name of the grouping variable within
#' which differences in the selected cosinor characteristic (amplitude or
#' acrophase) will be tested.
#' @param param A \code{character}. Either \code{"amp"} or \code{"acr"} for
#' testing differences in amplitude or acrophase, respectively.
#' @param comparison_A An \code{integer}. Refers to the level (within the
#' grouping variable) or component number that is to act as the reference group
#' for the comparison.
#' @param comparison_B An \code{integer}. Refers to the level (within the
#' grouping variable) or component number that is to act as the comparator group
#' for the comparison.
#' @param comparison_type A \code{character}. Indicates whether the comparison
#' to be performed is to be between levels of a grouping variable
#' (\code{"levels"}) or indices of components in a multiple-component cosinor
#' model (\code{"components"}).
#' @param component_index An \code{integer}. If
#' \code{comparison_type = "levels"}, \code{component_index} indicates which
#' component is being compared between the levels of the grouping variable.
#' @param level_index An \code{integer}. If
#' \code{comparison_type = "components"}, \code{level_index} indicates which
#' level of the grouping variable is being used for the comparison between
#' components.
#' @param ci_level The level for calculated confidence intervals. Defaults to
#' \code{0.95}.
#'
#' @return Returns a \code{test_cosinor} object.
#' @examples
#' fit <- cosinor.glmm(Y ~ X + amp_acro(time,
#'   group = "X",
#'   n_components = 1,
#'   period = 12
#' ), data = vitamind)
#'
#' test_cosinor(fit, "X", "amp")
#' @export
test_cosinor <- function(x,
                         x_str,
                         param = c("amp", "acr"),
                         comparison_A = 0,
                         comparison_B = 1,
                         comparison_type = c("levels", "components"),
                         component_index = 1,
                         level_index = 0,
                         ci_level = 0.95) {

  param <- match.arg(param)
  comparison_type <- match.arg(comparison_type)

  .validate_test_cosinor_inputs(
    x,
    x_str,
    param,
    comparison_A,
    comparison_B,
    comparison_type,
    component_index,
    level_index,
    ci_level
  )

  summary.fit <- summary.cosinor.glmm(x)
  index <- matrix(0, ncol = length(x$coefficients), nrow = length(x_str))
  colnames(index) <- names(x$coefficients)

  if (comparison_type == "components") {
    for (i in seq_along(x_str)) {
      index[i, paste0(x_str[i], level_index, ":", param, comparison_A)] <- -1
      index[i, paste0(x_str[i], level_index, ":", param, comparison_B)] <- 1
    }
  }

  if (comparison_type == "levels") {
    if (x$n_components == 1) {
      component_index <- ""
    }

    for (i in seq_along(x_str)) {
      index[i, paste0(x_str[i], comparison_A, ":", param, component_index)] <- -1
      index[i, paste0(x_str[i], comparison_B, ":", param, component_index)] <- 1
    }
  }

  diff.est <- index %*% x$coefficients
  diff.var <- index[, grep("(amp|acr)", names(x$coefficients)), drop = FALSE] %*%
    summary.fit$transformed.covariance %*%
    t(index[, grep("(amp|acr)", names(x$coefficients)), drop = FALSE])

  glob.chi <- (diff.est %*% solve(diff.var) %*% t(diff.est))[1, 1]
  ind.Z <- diff.est / sqrt(diag(diff.var))

  zt <- stats::qnorm((1 - ci_level) / 2, lower.tail = F) # get the quantile corresponding to ci_level

  interval <- cbind(
    diff.est,
    diff.est - zt * sqrt(diag(diff.var)),
    diff.est + zt * sqrt(diag(diff.var))
  )

  global.test <- structure(list(
    statistic = glob.chi,
    df = dim(diff.var)[1],
    conf.int = NULL,
    p.value = stats::pchisq(glob.chi, df = dim(diff.var)[1], lower.tail = FALSE)
  ), class = "sub_test_cosinor")

  ind.test <- structure(list(
    statistic = ind.Z[, ],
    df = NULL,
    conf.int = interval,
    p.value = 2 * stats::pnorm(-abs(ind.Z))[, ],
    names = x_str
  ), class = "sub_test_cosinor")


  structure(
    list(global.test = global.test, ind.test = ind.test),
    class = "test_cosinor"
  )
}

#' Validate args passed to \code{test_cosinor()}.
#'
#' @param x Arg from \code{test_cosinor()}.
#' @param x_str Arg from \code{test_cosinor()}.
#' @param param Arg from \code{test_cosinor()}.
#' @param comparison_A Arg from \code{test_cosinor()}.
#' @param comparison_B Arg from \code{test_cosinor()}.
#' @param comparison_type Arg from \code{test_cosinor()}.
#' @param component_index Arg from \code{test_cosinor()}.
#' @param level_index Arg from \code{test_cosinor()}.
#' @param ci_level Arg from \code{test_cosinor()}.
#'
#' @return \code{NULL}
#'
#' @noRd
.validate_test_cosinor_inputs <- function(x,
                                          x_str,
                                          param,
                                          comparison_A,
                                          comparison_B,
                                          comparison_type,
                                          component_index,
                                          level_index,
                                          ci_level) {
  validate_ci_level(ci_level)

  assertthat::assert_that(
    inherits(x, "cosinor.glmm"),
    msg = "'x' must be of class 'cosinor.glmm'"
  )

  stopifnot(is.character(x_str))

  assertthat::assert_that(
    length(grep(x_str, names(x$coefficients))) > 0,
    msg = "x_str must be the name of a group in object"
  )

  if (comparison_type == "levels") {
    assertthat::assert_that(
      comparison_A %in% x$group_stats[[x_str]] & comparison_B %in% x$group_stats[[x_str]],
      msg = "'comparison_A' and 'comparison_B' must be numbers corresponding to levels within group specified by 'x_str'"
    )
    assertthat::assert_that(
      component_index %in% 1:x$n_components,
      msg = "'component_index' must be supplied and it must be a number corresponding to a component in the model"
    )
  }

  if (comparison_type == "components") {
    assertthat::assert_that(
      comparison_A %in% 1:x$n_components & comparison_B %in% 1:x$n_components,
      msg = "'comparison_A' and 'comparison_B' must be numbers corresponding to a component in the model"
    )
    assertthat::assert_that(
      level_index %in% x$group_stats[[x$group_original[comparison_A]]] &
        level_index %in% x$group_stats[[x$group_original[comparison_B]]],
      msg = "'level_index' must be supplied and it must be a number corresponding to a level in the model"
    )
  }
}

#' Print results of test of cosinor model
#'
#' @param x A \code{test_cosinor} object.
#' @param ... Arguments passed to \code{print}
#'
#' @return \code{print(x)} returns \code{x} invisibly.
#' @export
#'
print.test_cosinor <- function(x, ...) {
  cat("Global test: \n")
  # TODO: add the parameter being tested
  print(x$global.test, ...)
  cat("\n Individual tests: \n")
  print(x$ind.test, ...)

  invisible(x)
}

#' Print test of model
#'
#' @param x A \code{sub_test_cosinor} object.
#' @param ... Additional, ignored arguments.
#'
#' @return \code{print(x)} returns \code{x} invisibly.
#' @export
print.sub_test_cosinor <- function(x, ...) {
  if (length(x$statistic) == 1) {
    cat("Statistic: \n")
    print(round(x$statistic, 2))
    cat("\n\n P-value: \n")
    print(round(x$p.value, 4))

    if (!is.null(x$conf.int)) {
      ci <- round(x$conf.int, 2)
      cat("\n Estimate and confidence interval")
      print(paste0(ci[1], " (", ci[2], " to ", ci[3], ")"))
    }
  } else {
    ci <- round(x$conf.int, 2)

    msat <- data.frame(
      statistic = round(x$statistic),
      estimate = paste0(ci[, 1], " (", ci[, 2], " to ", ci[, 3], ")"),
      p.value = round(x$p.value, 4)
    )

    rownames(msat) <- x$names
  }
  invisible(x)
}
