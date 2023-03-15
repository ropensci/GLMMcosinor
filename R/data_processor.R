#' Fit the cosinorGLMM model using the output from update_formula_and_data() and
#' a new formula
#'
#' @param obj output from `update_formula_and_data()`
#' @param formula new formula to use when fitting model (maybe with random effects)
#' @param ... optional additional arguments passed to `glmmTMB::glmmTMB()`
#'
#' @return Returns a fitted cosinor model as a `cosinor.glmm` object.
#' @export
#'
#' @examples
#' vitamind2 <- vitamind
#' vitamind2$patient <- sample(LETTERS[1:5], size = nrow(vitamind2), replace = TRUE)
#' updated_df_and_formula <- update_formula_and_data(
#'   data = vitamind2,
#'   formula = Y ~ X + amp.acro(time,
#'     group = "X",
#'     period = 12
#'   )
#' )
#' res <- fit_model_and_process(
#'   updated_df_and_formula,
#'   update.formula(updated_df_and_formula$newformula, . ~ . + (1 | patient))
#' )
fit_model_and_process <- function(obj, formula, ...) {
  if (!missing(formula)) {
    obj$newformula <- formula
  }
  do.call(data_processor, obj)
}


#' Process and fit the data using glmmTMB after initial processing by data_utils.R.
#'
#'
#' @param newdata processed dataframe with rrr and sss columns added
#' @param newformula processed formula wwith rrr and sss components
#' @param vec_sss a vector of sss for each component. (eg, "sss1, sss2")
#' @param vec_rrr a vector of sss for each component. (eg, "rrr1, rrr2")
#' @param n_components number of components specified
#' @param group_stats a list of levels per group
#' @param group the original group argument
#' @param group_check binary vector indicating whether a group arg is present
#' @param period a vector of periods for each component
#' @param family the data distribution family
#' @param Terms a list of  Terms from the original cosinor.glmm() call
#' @param ... extra parameters
#'
#' @return the model fit from glmmTMB (as well as some other inputs )
#' @noRd

data_processor <- function(newdata,
                           newformula,
                           vec_sss,
                           vec_rrr,
                           n_components,
                           group_stats,
                           group,
                           group_check,
                           period,
                           time_name,
                           family,
                           Terms,
                           cosinor.glmm.calls,
                           dispformula,
                           dispformula_check,
                           ziformula,
                           ziformula_check,
                           response_var,
                           group_original,
                           ...) {
  group_names <- names(group_stats)
  if (dispformula_check) {
    dispformula_val <- dispformula$formula
  } else {
    dispformula_val <- ~1
  }

  if (ziformula_check) {
    ziformula_val <- ziformula$formula
  } else {
    ziformula_val <- ~0
  }

  # Fit the data and formula to a model
  fit <- glmmTMB::glmmTMB(
    formula = newformula,
    data = newdata,
    family = family,
    dispformula = dispformula_val,
    ziformula = ziformula_val,
    ...
  )
  # Retrieve the fit, coefficients from the model and priming vectors
  # in preparation for transforming the raw coefficients
  mf <- fit

  # coefs_disp <- glmmTMB::fixef(mf)$disp

  main_coefs <- glmmTMB::fixef(mf)$cond
  conditional_model <- get_new_coefs(main_coefs, vec_rrr, vec_sss, n_components)

  items_keep <- c(
    "formula",
    "vec_rrr",
    "vec_sss",
    "n_components",
    "group_stats",
    "group_check"
  )

  if (dispformula_check) {
    disp_coefs <- glmmTMB::fixef(mf)$disp
    dispersion_model <- get_new_coefs(disp_coefs, dispformula$vec_rrr, dispformula$vec_sss, dispformula$n_components)

    disp_list <- c(
      dispformula[items_keep],
      list(
        coefficients = dispersion_model,
        raw_coefficients = disp_coefs,
        group = dispformula$group_disp
      )
    )
    names(disp_list) <- paste0(names(disp_list), "_disp")

  } else {
    disp_list <- NULL
  }

  if (ziformula_check) {
    zi_coefs <- glmmTMB::fixef(mf)$zi
    zi_model <- get_new_coefs(zi_coefs, ziformula$vec_rrr, ziformula$vec_sss, ziformula$n_components)

    zi_list <- c(
      ziformula[items_keep],
      list(
        coefficients = zi_model,
        raw_coefficients = zi_coefs,
        group = ziformula$group_disp
      )
    )
    names(zi_list) <- paste0(names(zi_list), "_zi")

  } else {
    zi_list <- NULL
  }

  # update calls
  if (missing(cosinor.glmm.calls)) {
    cosinor.glmm.calls <- list()
  }
  cosinor.glmm.calls$data_processor <- match.call()

  # Arrange the output
  structure(
    list(
      formula = newformula,
      fit = fit,
      cosinor.glmm.calls = cosinor.glmm.calls,
      Terms = Terms,
      coefficients = conditional_model,
      raw_coefficients = main_coefs,
      vec_sss = vec_sss,
      vec_rrr = vec_rrr,
      period = period,
      time_name = time_name,
      n_components = n_components,
      group_stats = group_stats,
      group = group,
      group_check = group_check,
      dispformula_check = dispformula_check,
      ziformula_check = ziformula_check,
      disp_list = disp_list,
      zi_list = zi_list,
      response_var = response_var,
      newdata = newdata,
      group_original = group_original
    ),
    class = "cosinor.glmm"
  )
}
