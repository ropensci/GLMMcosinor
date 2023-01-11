#' Fit cosinor model with \code{{glmmTMB}}
#'
#' Given an outcome and time variable, fit the cosinor model with optional
#' covariate effects.
#'
#' @param formula Formula specifying the model. Indicate the time variable with
#'   \code{time()} and covariate effects on the amplitude and acrophase with
#'   \code{amp.acro()}. See details for more information.
#' @param data Data frame where variable can be found
#' @param family a family function, see \code{?family} and \code{?glmmTMB::nbinom2}
#' @param quietly controls whether messages from amp.acro are displayed. TRUE by default
#' @param ... optional additional arguments passed to glmmTMB::glmmTMB()
#' @details This defines special functions that are used in the formula to
#'   indicate the time variable and which covariates effect the amplitude. To
#'   indicate the time variable wrap the name of it in the function
#'   \code{time()}. To indicate a variable which affects the
#'   acrophase/amplitude, wrap the name in \code{amp.acro()}. This will then do
#'   all the transformations for you. See examples for usage.
#'
#' @srrstatsTODO {G2.14} *Where possible, all functions should provide options for users to specify how to handle missing (`NA`) data, with options minimally including:*
#' @srrstatsTODO {G2.14a} *error on missing data*
#' @srrstatsTODO {G2.14b} *ignore missing data with default warnings or messages issued*
#' @srrstatsTODO {G2.14c} *replace missing data with appropriately imputed values*
#'
#' @examples
#'
#' cosinor.glmm(Y ~ X + amp.acro(time, n_components = 3, group = "X", period = c(12, 8, 9)), data = vitamind)
#'
#' @references Tong, YL. Parameter Estimation in Studying Circadian Rhythms, Biometrics (1976). 32(1):85--94.
#'
#'
#' @export
cosinor.glmm <- function(formula,
                         data,
                         family = gaussian(),
                         quietly = TRUE,
                         dispformula = ~1,
                         ziformula = ~0,
                         ...) {
  updated_df_and_formula <- update_formula_and_data(data = data,
                                                    formula = formula,
                                                    family = family,
                                                    quietly,
                                                    dispformula = dispformula,
                                                    ziformula = ziformula)
  #updated_df_and_formula$newformula <- Y ~ X + rrr1 + sss1 + X:rrr1 + X:sss1 + (1|X) #Example
  #Alternatively:
  #updated_df_and_formula$newformula <- update.formula(updated_df_and_formula$newformula, ~. + (1|X))
  cosinor.glmm.calls <-list(
    cosinor.glmm = match.call(),
    update_formula_and_data = updated_df_and_formula$Call
  )
  updated_df_and_formula$Call <- NULL
  # browser()
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
#' @param ... passed to summary
#'
#'
#' @export
#'
print.cosinor.glmm <- function(x, ...) {
  #cat("Call: \n")
  #print(x$Call)
  cat("\n Conditional Model \n")
  cat("\n Raw formula: \n")
  print(x$formula)
  cat("\n Raw Coefficients: \n")
  print(x$raw_coefficients)
  cat("\n Transformed Coefficients: \n")
  t.x <- x$coefficients
 if (x$group_check == TRUE) {
   names(t.x) <- update_covnames(names(t.x), group_stats = x$group_stats)
 }
  print(t.x)

  if (x$dispformula_check) {
    cat("\n***********************\n")
    cat("\n Dispersion Model \n")
    cat("\n Raw  Formula: \n")
    print(x$disp_list$formula_disp)
    cat("\n Raw  Coefficients: \n")
    print(x$disp_list$raw_coefficients_disp)
    cat("\n Transformed  Coefficients: \n")
    td.x <- x$disp_list$coefficients_disp
    if (x$disp_list$group_check_disp == TRUE) {
      names(td.x) <- update_covnames(names(td.x), group_stats = x$disp_list$group_stats_disp)
    }
    print(td.x)
  }

  if (x$ziformula_check) {
    cat("\n***********************\n")
    cat("\n Zero-Inflation Model \n")
    cat("\n Raw  Formula: \n")
    print(x$zi_list$formula_zi)
    cat("\n Raw  Coefficients: \n")
    print(x$zi_list$raw_coefficients_zi)
    cat("\n Transformed  Coefficients: \n")
    tzi.x <- x$zi_list$coefficients_zi
    if (x$zi_list$group_check_zi == TRUE) {
      names(tzi.x) <- update_covnames(names(tzi.x), group_stats = x$zi_list$group_stats_zi)
    }
    print(tzi.x)
  }
}

#' Fit cosinor model
#'
#' Given an outcome and time variable, fit the cosinor model with optional covariate effects.
#'
#' @param formula Formula specifying the model. Indicate the time variable with \code{time()} and covariate effects on the
#' amplitude and acrophase with \code{amp.acro()}. See details.
#' @param ... other arguments
#'
#' @details This defines special functions that are used in the formula to indicate the time variable
#' and which covariates effect the amplitude. To indicate the time variable wrap the name of it in the function
#' \code{time()}. To indicate a variable which affects the acrophase/amplitude, wrap the name in
#' \code{amp.acro()}. This will then do all the transformations for you. See examples for usage.
#'
#' @examples
#'
#' cosinor.glmm(Y ~ time(time) + X + amp.acro(X), data = vitamind)
#'
#' @export
#'


cosinor.glmm.default <- function(formula, ...) {
  UseMethod("cosinor.glmm")
}

#' Extract variable names from terms object, handling specials
#'
#' @param Terms a terms object
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
    covnames <- append(covnames,paste0(i,j))
    }
  }

  # get the names that covnames does not get:
  covnames_inv <- grep(paste0("(Intercept|", paste(covnames, collapse = "|"), ")"), invert = TRUE, names, value = TRUE)
  lack <- names
  for (i in 1:length(covnames)) {
    var <- group_names_together[i] # var is a group name corresponding to that in covnames
    var_number <- unlist(group_stats)[[i]] # get the group level
    lack <- gsub(paste0(covnames[i]), paste0("[", var, "=", var_number, "]"), lack)
    lack <- gsub(paste0("^", covnames[i], "$"), paste0("[", var, "=", var_number, "]"), lack)
  }

  lack
}
