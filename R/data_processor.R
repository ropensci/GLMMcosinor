#' Fit the cosinorGLMM model using the output from update_formula_and_data() and
#' a new formula
#'
#' @param obj output from `update_formula_and_data()`
#' @param formula new formula to use when fitting model (maybe with random effects)
#' @param ... optional additional arguments passed to `glmmTMB::glmmTMB()`
#'
#' @export
#'
#' @examples
#' vitamind2 <- vitamind
#' vitamind2$patient <- sample(LETTERS[1:5], size=nrow(vitamind2), replace=TRUE)
#' updated_df_and_formula <- update_formula_and_data(data = vitamind2, formula = Y ~ X + amp.acro(time, group="X", period = 12))
#' res <- fit_model_and_process(
#'   updated_df_and_formula,
#'   update.formula(updated_df_and_formula$newformula, .~.+(1|patient))
#' )
fit_model_and_process <- function(obj, formula, ...) {
  if(!missing(formula)) {
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
#' @export
#'
#' @examples
data_processor <- function(newdata,
                           newformula,
                           vec_sss,
                           vec_rrr,
                           n_components,
                           group_stats,
                           group,
                           group_check,
                           period,
                           family,
                           Terms,
                           cosinor.glmm.calls,
                           dispformula,
                           dispformula_check,
                           ziformula,
                           ziformula_check,
                           ...)  {
  group_names <- names(group_stats)

  if (dispformula_check){
    dispformula_val = dispformula$formula
  } else {
    dispformula_val = ~1
  }

  if (ziformula_check){
    ziformula_val = ziformula$formula
  } else {
    ziformula_val = ~0
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

  #coefs_disp <- glmmTMB::fixef(mf)$disp

  get_new_coefs <- function(coefs, vec_rrr, vec_sss, n_components){
  r.coef <- NULL
  s.coef <- NULL
  mu.coef <- NULL
  mu_inv <- rep(0, length(names(coefs)))


  # Get a Boolean vector for rrr, sss, and mu. This will be used to extract
  # the relevant raw parameters from the raw coefficient model output
  for (i in 1:n_components) {
    r.coef[[i]] <- grepl(paste0(vec_rrr[i]), names(coefs))
    s.coef[[i]] <- grepl(paste0(vec_sss[i]), names(coefs))

    mu_inv_carry <- r.coef[[i]] + s.coef[[i]] # Keep track of non-mesor terms
    mu_inv <- mu_inv_carry + mu_inv # Ultimately,every non-mesor term will be true
  }

  mu.coef <- c(!mu_inv) # invert 'mu_inv' to get a Boolean vector for mesor terms
  r.coef <- (t(matrix(unlist(r.coef), ncol = length(r.coef)))) # a matrix of rrr coefficients
  s.coef <- (t(matrix(unlist(s.coef), ncol = length(s.coef)))) # a matrix of sss coefficients

  # Calculate the parameter estimates for all components
  amp <- NULL
  acr <- NULL
  for (i in 1:n_components) {
    beta.s <- coefs[s.coef[i, ]]
    beta.r <- coefs[r.coef[i, ]]

    groups.r <- c(beta.r[1], beta.r[which(names(beta.r) != names(beta.r[1]))])
    groups.s <- c(beta.s[1], beta.s[which(names(beta.s) != names(beta.s[1]))])

    amp[[i]] <- sqrt(groups.r^2 + groups.s^2)
    names(amp[[i]]) <- gsub(vec_rrr[i], paste0("amp", i), names(beta.r))

    acr[[i]] <- -atan2(groups.s, groups.r)
    names(acr[[i]]) <- gsub(vec_sss[i], paste0("acr", i), names(beta.s))
  }
  new_coefs <- c(coefs[mu.coef], unlist(amp), unlist(acr))
  # if n_components = 1, then print "amp" and "acr" rather than "amp1", "acr1"
  if (n_components == 1) {
    names(amp[[1]]) <- gsub(vec_rrr[1], "amp", names(beta.r))
    names(acr[[1]]) <- gsub(vec_sss[1], "acr", names(beta.s))
    new_coefs <- c(coefs[mu.coef], unlist(amp), unlist(acr))
  }
  new_coefs
  }

  main_coefs <- glmmTMB::fixef(mf)$cond
  conditional_model <- get_new_coefs(main_coefs, vec_rrr, vec_sss, n_components)

  if (dispformula_check){
    disp_coefs<- glmmTMB::fixef(mf)$disp
    dispersion_model <- get_new_coefs(disp_coefs, dispformula$vec_rrr, dispformula$vec_sss, dispformula$n_components)
    disp_list <- list(formula_disp = dispformula$formula,
         coefficients_disp = dispersion_model,
         raw_coefficients_disp = disp_coefs,
         vec_sss_disp = dispformula$vec_sss,
         vec_rrr_disp = dispformula$vec_rrr,
         n_components_disp = dispformula$n_components,
         group_stats_disp = dispformula$group_stats,
         group_disp = dispformula$group_disp,
         group_check_disp = dispformula$group_check
         )
  } else {
    disp_list <- NULL
  }

  if (ziformula_check){
    zi_coefs<- glmmTMB::fixef(mf)$zi
    zi_model <- get_new_coefs(zi_coefs, ziformula$vec_rrr, ziformula$vec_sss, ziformula$n_components)
    zi_list <- list(formula_zi = ziformula$formula,
                      coefficients_zi = zi_model,
                      raw_coefficients_zi = zi_coefs,
                      vec_sss_zi = ziformula$vec_sss,
                      vec_rrr_zi = ziformula$vec_rrr,
                      n_components_zi = ziformula$n_components,
                      group_stats_zi = ziformula$group_stats,
                      group_zi = ziformula$group_zi,
                      group_check_zi = ziformula$group_check
    )
  } else {
    zi_list <- NULL
  }

  # update calls
  if(missing(cosinor.glmm.calls)) {
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
      n_components = n_components,
      group_stats = group_stats,
      group = group,
      group_check = group_check,
      dispformula_check = dispformula_check,
      ziformula_check = ziformula_check,
      disp_list = disp_list,
      zi_list = zi_list
    ),
    class = "cosinor.glmm"
  )

}
