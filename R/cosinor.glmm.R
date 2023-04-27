#' Fit cosinor model with \code{{glmmTMB}}
#'
#' Given an outcome and time variable, fit the cosinor model with optional
#' covariate effects.
#'
#' @param formula Formula specifying the model. Indicate the time variable with
#'   \code{time()} and covariate effects on the amplitude and acrophase with
#'   \code{amp.acro()}. See details for more information.
#' @param data Data frame where variable can be found.
#' Row names corresponding to variables specified in the formula will be inherited with their attributes; details from unused columns are not inherited.
#' The analysis assumes that the data follows the distribution specified in the 'family' parameter.
#' @param family a family function, see \code{?family} and \code{?glmmTMB::nbinom2}
#' @param quietly controls whether messages from amp.acro are displayed. TRUE by default
#' @param dispformula Formula specifying a dispersion model (optional). Use the same format as the main formula
#' @param ziformula Formula specifying a zero-inflation model (optional). Use the same format as the main formula
#' @param ... optional additional arguments passed to glmmTMB::glmmTMB()
#'
#' @details This defines special functions that are used in the formula to
#'   indicate the time variable and which covariates effect the amplitude. To
#'   indicate the time variable wrap the name of it in the function
#'   \code{time()}. To indicate a variable which affects the
#'   acrophase/amplitude, wrap the name in \code{amp.acro()}. This will then do
#'   all the transformations for you. See examples for usage.
#'
#'
#' @return Returns a fitted cosinor model as a `cosinor.glmm` object.
#'
#' @srrstats {G2.14} *Where possible, all functions should provide options for users to specify how to handle missing (`NA`) data, with options minimally including:*
#' @srrstats {G2.14a} *error on missing data*
#' @srrstats {G2.14b} *ignore missing data with default warnings or messages issued*
#' @srrstats {G2.14c} *replace missing data with appropriately imputed values*
#' @srrstats {G1.4} *Software should use [`roxygen2`](https://roxygen2.r-lib.org/) to document all functions.*
#' @srrstats {G3.0} *Statistical software should never compare floating point numbers for equality. All numeric equality comparisons should either ensure that they are made between integers, or use appropriate tolerances for approximate equality.*
#'
#'
#'
#' @examples
#'
#' cosinor.glmm(Y ~ X + amp.acro(time,
#'   n_components = 3,
#'   group = "X",
#'   period = c(12, 8, 9)
#' ), data = vitamind)
#'
#' @references Tong, YL. Parameter Estimation in Studying Circadian Rhythms, Biometrics (1976). 32(1):85--94.
#'
#'
#' @export
cosinor.glmm <- function(formula,
                         data,
                         family = stats::gaussian(),
                         quietly = TRUE,
                         dispformula = ~1,
                         ziformula = ~0,
                         ...) {
  updated_df_and_formula <- update_formula_and_data(
    data = data,
    formula = formula,
    family = family,
    quietly,
    dispformula = dispformula,
    ziformula = ziformula
  )

 cosinor.glmm.calls <- list(
    cosinor.glmm = match.call(),
    update_formula_and_data = updated_df_and_formula$Call
  )
  updated_df_and_formula$Call <- NULL



  do.call(
    data_processor,
    c(
      updated_df_and_formula,
      cosinor.glmm.calls = list(cosinor.glmm.calls),
      ...
    )
  )
}

#' Print cosinor model
#'
#' Given an outcome and time variable, fit the cosinor model with optional covariate effects.
#'
#' @param x cosinor.glmm object
#' @param digits Controls the number of digits displayed in the summary output
#' @param ... passed to summary
#'
#' @srrstats {RE4.17} *Model objects returned by Regression Software should implement or appropriately extend a default `print` method which provides an on-screen summary of model (input) parameters and (output) coefficients.*
#' @srrstats {G1.4} *Software should use [`roxygen2`](https://roxygen2.r-lib.org/) to document all functions.*
#'
#' @return `print` returns `x` invisibly.
#'
#' @export
#'
print.cosinor.glmm <- function(x, digits = getOption("digits"), ...) {
  # cat("Call: \n")
  # print(x$Call)
  cat("\n Conditional Model \n")
  cat("\n Raw formula: \n")
  cat(deparse(x$formula), "\n")
  cat("\n Raw Coefficients: \n")
  print(round(x$raw_coefficients, digits = digits))
  cat("\n Transformed Coefficients: \n")
  t.x <- x$coefficients
  if (x$group_check == TRUE) {
    names(t.x) <- update_covnames(names(t.x), group_stats = x$group_stats)
  }
  print(round(t.x, digits = digits))

  if (x$dispformula_check) {
    cat("\n***********************\n")
    cat("\n Dispersion Model \n")
    cat("\n Raw  Formula: \n")
    cat(deparse(x$disp_list$formula_disp), "\n")
    cat("\n Raw  Coefficients: \n")
    print(round(x$disp_list$raw_coefficients_disp, digits = digits))
    cat("\n Transformed  Coefficients: \n")
    td.x <- x$disp_list$coefficients_disp
    if (x$disp_list$group_check_disp == TRUE) {
      names(td.x) <- update_covnames(names(td.x), group_stats = x$disp_list$group_stats_disp)
    }
    print(round(td.x, digits = digits))
  }

  if (x$ziformula_check) {
    cat("\n***********************\n")
    cat("\n Zero-Inflation Model \n")
    cat("\n Raw  Formula: \n")
    cat(deparse(x$zi_list$formula_zi), "\n")
    cat("\n Raw  Coefficients: \n")
    print(round(x$zi_list$raw_coefficients_zi, digits = digits))
    cat("\n Transformed  Coefficients: \n")
    tzi.x <- x$zi_list$coefficients_zi
    if (x$zi_list$group_check_zi == TRUE) {
      names(tzi.x) <- update_covnames(names(tzi.x), group_stats = x$zi_list$group_stats_zi)
    }
    print(round(tzi.x, digits = digits))
  }
  invisible(x)
}

#' Extract variable names from terms object, handling specials
#'
#' @param Terms a terms object
#'
#' @srrstats {G1.4a} *All internal (non-exported) functions should also be documented in standard [`roxygen2`](https://roxygen2.r-lib.org/) format, along with a final `@noRd` tag to suppress automatic generation of `.Rd` files.*
#'
#' @keywords Internal
#' @noRd
get_varnames <- function(Terms) {
  spec <- names(attr(Terms, "specials"))
  tname <- attr(Terms, "term.labels")

  dex <- unlist(sapply(spec, function(sp) {
    attr(Terms, "specials")[[sp]] - 1
  }))

  tname2 <- tname
  for (jj in spec) {
    gbl <- grep(paste0(jj, "("), tname2, fixed = TRUE)
    init <- length(gbl) > 0
    if (init) {
      jlack <- gsub(paste0(jj, "("), "", tname2, fixed = TRUE)
      tname2[gbl] <- substr(jlack[gbl], 1, nchar(jlack[gbl]) - 1)
    }
  }
  tname2
}

#' Replace covariate names with descriptive text
#'
#' @param names Coefficient names to update
#'
#' @noRd
update_covnames <- function(names, group_stats) {
  # Present the covariate names with descriptive text
  group_names <- names(group_stats) # get the group names
  group_names_together <- NULL # a vector of the group_names of each level

  # creates a vector of group names corresponding to the number of levels in each group
  # Example: if groups are "X" and "Z" with 2 and 3 levels respectively, this 'for loop'
  # would create the vector: c("X","X","Z","Z","Z")
  covnames <- NULL
  for (i in group_names) {
    group_names_together <- append(
      group_names_together,
      rep(names(group_stats[i]), length(group_stats[[i]]))
    )
    # get the names of the covariates alone
    for (j in group_stats[i]) {
      covnames <- append(covnames, paste0(i, j))
    }
  }

  # get the names that covnames does not get:
  covnames_inv <- grep(paste0("(Intercept|", paste(covnames, collapse = "|"), ")"), invert = TRUE, names, value = TRUE)
  lack <- names
  for (i in seq_along(covnames)) {
    var <- group_names_together[i] # var is a group name corresponding to that in covnames
    var_number <- unlist(group_stats)[[i]] # get the group level
    lack <- gsub(paste0(covnames[i]), paste0("[", var, "=", var_number, "]"), lack)
    lack <- gsub(paste0("^", covnames[i], "$"), paste0("[", var, "=", var_number, "]"), lack)
  }

  lack
}
