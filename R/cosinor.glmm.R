#' Fit cosinor model with \code{{glmmTMB}}
#'
#' Given an outcome and time variable, fit the cosinor model with optional
#' covariate effects.
#'
#' @param formula A \code{formula} specifying the cosinor model to be fit.
#' The cosinor portion of the formula is controlled by including
#' \code{amp_acro()} on the right hand side of the formula.
#' See \code{\link{amp_acro}} for more details.
#' @param data A \code{data.frame} containing the variables used in the model.
#' @param family A \code{family} function or a character string naming a family
#' function. See \code{?family} and \code{?glmmTMB::family_glmmTMB} for options.
#' @param quietly A \code{logical}. If \code{TRUE}, shows warning messages when
#' wrangling data and fitting model. Defaults to \code{TRUE}.
#' @param dispformula A one-sided (i.e., no response variable) \code{formula}
#' for dispersion combining fixed and random effects, including cosinor
#' components using \code{amp_acro()}. Defaults to \code{~1}.
#' @param ziformula A one-sided (i.e., no response variable) \code{formula}
#' for zero-inflation combining fixed and random effects, including cosinor
#' components using \code{amp_acro()}. Defaults to \code{~0}.
#' @param ... Optional additional arguments passed to \code{glmmTMB::glmmTMB()}.
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
#' @examples
#' # Single component cosinor model
#' cosinor.glmm(
#'   Y ~ amp_acro(time_col = time, group = "X", period = 12),
#'   data = vitamind
#' )
#'
#' # 2-component cosinor model with simulated data
#' sim_data <- simulate_cosinor(
#'   n = 500,
#'   mesor = 5,
#'   amp = c(2, 1),
#'   acro = c(1, 1.5),
#'   beta.mesor = 2,
#'   beta.amp = c(2, 1),
#'   beta.acro = c(1, 1.5),
#'   family = "gaussian",
#'   period = c(12, 6),
#'   n_components = 2,
#'   beta.group = TRUE,
#' )
#'
#' cosinor.glmm(
#'   Y ~ group + amp_acro(times,
#'                        n_components = 2,
#'                        group = "group",
#'                        period = c(12, 6)),
#'   data = sim_data,
#'   family = gaussian
#' )
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

#' Extract variable names from terms object, handling specials.
#'
#' @param Terms A \code{terms} object.
#'
#' @srrstats {G1.4a} *All internal (non-exported) functions should also be documented in standard [`roxygen2`](https://roxygen2.r-lib.org/) format, along with a final `@noRd` tag to suppress automatic generation of `.Rd` files.*
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

#' Replace covariate names with descriptive text.
#'
#' @param names Coefficient names to update.
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
