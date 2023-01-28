#' Test for differences in a cosinor model
#'
#' Given a time variable and optional covariates, generate inference a cosinor
#' fit. For the covariate named (or vector of covariates), this function
#' performs a Wald test comparing the group with covariates equal to 1 to the
#' group with covariates equal to 0. This may not be the desired result for
#' continuous covariates.
#'
#'
#' @param object An object of class \code{cosinor.glmm}
#' @param x_str Character naming the covariate whose amplitude/acrophase will be tested
#' @param param Character string naming the parameter to test, either "amp" for
#'   amplitude or "acr" for acrophase
#'
#' @param comparison_A A number, referring to the level (within a group) or component number that is to be compared to comparison_B
#' @param comparison_B A number, referring to the level (within a group) or component number that is to be compared to comparison_A
#' @param comparison_type A string that is either: "levels" (default), or "components". If "levels", then comparison_A and comparison_B will refer to the two levels that are being compared. If comparison_type = "components", then comparison_A component will be compared to comparison_B component
#' @param component_index If comparison_type = "levels", this controls which single component the levels are being compared to. Note that component_index must be an integer, and must refer to a component within the model
#' @param level_index If comparison_type = "components", this controls which single level the components are being compared to. Note that level_index must be an integer, and must refer to a level within the model
#'
#' @examples
#'
#' fit <- cosinor.glmm(Y ~ X + amp.acro(time, group = "X", n_components = 1, period = 12), data = vitamind)
#' test_cosinor(fit, "X", "amp")
#'
#' @export
#'

test_cosinor <- function(object,
                         x_str,
                         param = "amp",
                         comparison_A = 0,
                         comparison_B = 1,
                         comparison_type = "levels",
                         component_index = 1,
                         level_index = 0) {
  stopifnot(is.character(x_str))
  stopifnot(length(grep(x_str, names(object$coefficients))) > 0)

  summary.fit <- summary.cosinor.glmm(object)
  index <- matrix(0, ncol = length(object$coefficients), nrow = length(x_str))
  colnames(index) <- names(object$coefficients)

  if (comparison_type == "components") {
    for (i in 1:length(x_str)) {
      index[i, paste0(x_str[i], level_index, ":", param, comparison_A)] <- -1

      index[i, paste0(x_str[i], level_index, ":", param, comparison_B)] <- 1
    }
  }

  if (comparison_type == "levels") {
    if (object$n_components == 1) {
      component_index <- ""
    }

    for (i in 1:length(x_str)) {
      index[i, paste0(x_str[i], comparison_A, ":", param, component_index)] <- -1

      index[i, paste0(x_str[i], comparison_B, ":", param, component_index)] <- 1
    }
  }

  diff.est <- index %*% object$coefficients
  diff.var <- index[, grep("(amp|acr)", names(object$coefficients)), drop = FALSE] %*% summary.fit$transformed.covariance %*%
    t(index[, grep("(amp|acr)", names(object$coefficients)), drop = FALSE])

  glob.chi <- (diff.est %*% solve(diff.var) %*% t(diff.est))[1, 1]
  ind.Z <- diff.est / sqrt(diag(diff.var))
  interval <- cbind(diff.est, diff.est - 1.96 * sqrt(diag(diff.var)), diff.est + 1.96 * sqrt(diag(diff.var)))

  global.test <- list(statistic = glob.chi, df = dim(diff.var)[1], conf.int = NULL, p.value = stats::pchisq(glob.chi, df = dim(diff.var)[1], lower.tail = FALSE))
  ind.test <- list(statistic = ind.Z[, ], df = NULL, conf.int = interval, p.value = 2 * stats::pnorm(-abs(ind.Z))[, ], names = x_str)

  class(global.test) <- class(ind.test) <- "test"

  structure(list(global.test = global.test, ind.test = ind.test), class = "test_cosinor")
}

#' Print results of test of cosinor model
#'
#' @param x test_cosinor object
#' @param ... Arguments passed to \code{print}
#'
#'
#' @export
#'

print.test_cosinor <- function(x, ...) {
  cat("Global test: \n")
  print(x$global.test, ...)
  cat("\n Individual tests: \n")
  print(x$ind.test, ...)
}

#' Print test of model
#'
#' @param x test object
#'
#'
#' @keywords Internal
#'

print.test <- function(x) {
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
    msat <- data.frame(statistic = round(x$statistic), estimate = paste0(ci[, 1], " (", ci[, 2], " to ", ci[, 3], ")"), p.value = round(x$p.value, 4))
    rownames(msat) <- x$names
  }
}
