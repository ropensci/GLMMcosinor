#' Fit the cosinorGLMM model using the output from update_formula_and_data() and
#' a new formula
#'
#' @param obj output from `update_formula_and_data()`
#' @param formula new formula to use when fitting model (maybe with random effects)
#' @param ... optional additional arguments passed to `glmmTMB::glmmTMB()`
#'
#' @return Returns a fitted cosinor model as a `cosinor.glmm` object.
#' @srrstats {G1.4} *Software should use [`roxygen2`](https://roxygen2.r-lib.org/) to document all functions.*
#'
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
#'
#'
#'
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
#' @srrstats {RE1.0} *Regression Software should enable models to be specified via a formula interface, unless reasons for not doing so are explicitly documented.*
#' @srrstats {RE1.3} *Regression Software which passes or otherwise transforms aspects of input data onto output structures should ensure that those output structures retain all relevant aspects of input data, notably including row and column names, and potentially information from other `attributes()`.*
#' @srrstats {RE1.3a} *Where otherwise relevant information is not transferred, this should be explicitly documented.*
#' @srrstats {RE1.4} *Regression Software should document any assumptions made with regard to input data; for example distributional assumptions, or assumptions that predictor data have mean values of zero. Implications of violations of these assumptions should be both documented and tested.*
#' @srrstats {RE2.0} *Regression Software should document any transformations applied to input data, for example conversion of label-values to `factor`, and should provide ways to explicitly avoid any default transformations (with error or warning conditions where appropriate).*
#' @srrstats {RE2.1} *Regression Software should implement explicit parameters controlling the processing of missing values, ideally distinguishing `NA` or `NaN` values from `Inf` values (for example, through use of `na.omit()` and related functions from the `stats` package).*
#' @srrstats {RE4.0} *Regression Software should return some form of "model" object, generally through using or modifying existing class structures for model objects (such as `lm`, `glm`, or model objects from other packages), or creating a new class of model objects.*
#' @srrstats {RE4.2} *Model coefficients (via `coeff()` / `coefficients()`)*
#' @srrstats {RE4.4} *The specification of the model, generally as a formula (via `formula()`)*
#' @srrstats {G1.4} *Software should use [`roxygen2`](https://roxygen2.r-lib.org/) to document all functions.*
#' @srrstats {G2.13} *Statistical Software should implement appropriate checks for missing data as part of initial pre-processing prior to passing data to analytic algorithms.*

#'
#' The following standards are covered in the glmmTMB package
#' @srrstats {RE2.2} *Regression Software should provide different options for processing missing values in predictor and response data. For example, it should be possible to fit a model with no missing predictor data in order to generate values for all associated response points, even where submitted response values may be missing.*
#' @srrstats {RE3.0} *Issue appropriate warnings or other diagnostic messages for models which fail to converge.*
#' @srrstats {RE3.1} *Enable such messages to be optionally suppressed, yet should ensure that the resultant model object nevertheless includes sufficient data to identify lack of convergence.*
#' @srrstats {RE4.8} *Response variables, and associated "metadata" where applicable.*
#' @srrstats {RE4.10} *Model Residuals, including sufficient documentation to enable interpretation of residuals, and to enable users to submit residuals to their own tests.*
#' @srrstats {RE4.11} *Goodness-of-fit and other statistics associated such as effect sizes with model coefficients.*
#' @srrstats {RE4.12} *Where appropriate, functions used to transform input data, and associated inverse transform functions.*
#' @srrstats {RE4.13} *Predictor variables, and associated "metadata" where applicable.*
#' @srrstats {G1.4a} *All internal (non-exported) functions should also be documented in standard [`roxygen2`](https://roxygen2.r-lib.org/) format, along with a final `@noRd` tag to suppress automatic generation of `.Rd` files.*
#' @srrstats {G2.3} *For univariate character input:*
#' @srrstats {G2.3a} *Use `match.arg()` or equivalent where applicable to only permit expected values.*
#' @srrstats {G2.3b} *Either: use `tolower()` or equivalent to ensure input of character parameters is not case dependent; or explicitly document that parameters are strictly case-sensitive.*
#' @srrstats {G2.14} *Where possible, all functions should provide options for users to specify how to handle missing (`NA`) data, with options minimally including:*
#' @srrstats {G2.14a} *error on missing data*
#' @srrstats {G2.14b} *ignore missing data with default warnings or messages issued*
#' @srrstats {G2.14c} *replace missing data with appropriately imputed values*
#' @srrstats {G2.15} *Functions should never assume non-missingness, and should never pass data with potential missing values to any base routines with default `na.rm = FALSE`-type parameters (such as [`mean()`](https://stat.ethz.ch/R-manual/R-devel/library/base/html/mean.html), [`sd()`](https://stat.ethz.ch/R-manual/R-devel/library/stats/html/sd.html) or [`cor()`](https://stat.ethz.ch/R-manual/R-devel/library/stats/html/cor.html)).*
#' @srrstats {G2.16} *All functions should also provide options to handle undefined values (e.g., `NaN`, `Inf` and `-Inf`), including potentially ignoring or removing such values.*
#'
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

  #browser()
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
