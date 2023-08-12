#' Update data and formula for fitting cosinor.glmm model
#'
#' @param data input data for fitting cosinor.glmm model.
#' @param formula model formula, specified by user including \code{amp_acro()}.
#' @param family the model family.
#' @param quietly controls whether messages from amp_acro are displayed.
#' TRUE by default
#' @param dispformula The formula specifying the dispersion model
#' @param ziformula The formula specifying the zero-inflation model
#'
#' @srrstats {G2.13}
#' @srrstats {G2.14b}
#' @srrstats {G1.4}
#' @srrstats {G2.6}
#' @srrstats {G2.7}
#' @srrstats {RE2.0}
#' @srrstats {RE2.1}
#' @srrstats {G2.8}
#' @srrstats {G2.9}
#' @srrstats {G2.10}
#' @srrstats {G2.13}

#' @return Returns a \code{list}.
#' @export
update_formula_and_data <- function(data,
                                    formula,
                                    family = "gaussian",
                                    quietly = TRUE,
                                    dispformula = ~1,
                                    ziformula = ~0) {
  # Extract only the amp_acro function from the call
  # check for missing data

  if (!quietly) {
    if (any(is.na(data))) {
      message("\n Missing data in the following dataframe columns: \n")
      print(colSums(is.na(data)))
      message("\n Missing data will be ignored ")
    }
  }

  # formatting data to be evaluated in amp_acro()
  # check for dispformula and/or ziformula argument
  dispformula_check <- !missing(dispformula) & dispformula != ~1
  ziformula_check <- !missing(ziformula) & ziformula != ~0


  formula_eval <- function(formula,
                           data,
                           quietly,
                           amp_acro_ind = -1,
                           data_prefix = "main_") {
    Terms <- stats::terms(formula, specials = c("amp_acro"))
    amp_acro_text <- attr(
      Terms, "term.labels"
    )[attr(Terms, "special")$amp_acro + amp_acro_ind]
    e <- str2lang(amp_acro_text)
    e$.data <- data # add data that will be called to amp_acro()
    e$.formula <- formula # add formula that will be called to amp_acro()
    e$.quietly <- quietly
    e$.amp_acro_ind <- amp_acro_ind
    e$.data_prefix <- data_prefix
    updated_df_and_formula <- eval(e) # evaluate amp_acro call
    c(updated_df_and_formula, list(
      Terms = Terms, family = family,
      dispformula_check = dispformula_check,
      ziformula_check = ziformula_check
    ))
  }

  main_output <- formula_eval(
    formula,
    data,
    quietly,
    amp_acro_ind = -1,
    data_prefix = "main_"
  )


  items_keep <- c(
    "newformula",
    "vec_rrr",
    "vec_sss",
    "n_components",
    "period",
    "group_stats",
    "group_check",
    "group"
  )

  if (dispformula_check) {
    data <- main_output$newdata
    dispformula <- formula_eval(
      formula = dispformula,
      data = data,
      quietly = quietly,
      amp_acro_ind = 0,
      data_prefix = "disp_"
    )
    main_output$newdata <- dispformula$newdata

    dispformula <- dispformula[items_keep]
    names(dispformula)[names(dispformula) == "newformula"] <- "formula"
    main_output$dispformula <- dispformula
  }
  if (ziformula_check) {
    data <- main_output$newdata
    ziformula <- formula_eval(
      formula = ziformula,
      data = data,
      quietly = quietly,
      amp_acro_ind = 0,
      data_prefix = "zi_"
    )
    main_output$newdata <- ziformula$newdata

    ziformula <- ziformula[items_keep]
    names(ziformula)[names(ziformula) == "newformula"] <- "formula"
    main_output$ziformula <- ziformula
  }
  main_output
}


#' Checks that the group names supplied by the user are in the dataframe
#'
#' @param .data dataframe
#' @param group group argument specified in the cosinor.glmm call
#' @srrstats {G1.4a}
#'
#' @return nothing if successful, an error message if not
#' @noRd


# check the group inputs
check_group_var <- function(.data, group) {
  grouping_vars <- group[!group %in% c(0, NA)]
  if (!all(grouping_vars %in% colnames(.data))) {
    bad_groups <- grouping_vars[which(!grouping_vars %in% colnames(.data))]
    stop(
      "Grouping variable(s) not found in input data: [",
      paste0(bad_groups, collapse = ", "),
      "]"
    )
  }
}

#' Checks that the `ci_level` values provided are reasonable
#'
#' @param ci_level Confidence level.
#' @srrstats {G1.4a}
#' @return nothing if successful, an error message if not
#' @noRd

validate_ci_level <- function(ci_level) {
  assertthat::is.number(ci_level)

  if (ci_level < 0 || ci_level > 1) {
    stop("'ci_level' must be a single numeric value in [0, 1].")
  }
}

# calculate the parameters from the raw estimates
get_new_coefs <- function(coefs, vec_rrr, vec_sss, n_components, period) {
  r.coef <- NULL
  s.coef <- NULL
  mu.coef <- NULL
  mu_inv <- rep(0, length(names(coefs)))


  # Get a Boolean vector for rrr, sss, and mu. This will be used to extract
  # the relevant raw parameters from the raw coefficient model output
  for (i in 1:n_components) {
    r.coef[[i]] <- grepl(paste0(vec_rrr[i]), names(coefs))
    s.coef[[i]] <- grepl(paste0(vec_sss[i]), names(coefs))

    # Keep track of non-mesor terms
    mu_inv_carry <- r.coef[[i]] + s.coef[[i]]
    # Ultimately,every non-mesor term will be true
    mu_inv <- mu_inv_carry + mu_inv
  }

  # invert 'mu_inv' to get a Boolean vector for mesor terms
  mu.coef <- c(!mu_inv)
  # a matrix of rrr coefficients
  r.coef <- (t(matrix(unlist(r.coef), ncol = length(r.coef))))
  # a matrix of sss coefficients
  s.coef <- (t(matrix(unlist(s.coef), ncol = length(s.coef))))

  # Calculate the parameter estimates for all components
  amp <- NULL
  acr <- NULL
  acr_adjusted <- NULL
  for (i in 1:n_components) {
    beta.s <- coefs[s.coef[i, ]]
    beta.r <- coefs[r.coef[i, ]]

    groups.r <- c(beta.r[1], beta.r[which(names(beta.r) != names(beta.r[1]))])
    groups.s <- c(beta.s[1], beta.s[which(names(beta.s) != names(beta.s[1]))])

    amp[[i]] <- sqrt(groups.r^2 + groups.s^2)
    names(amp[[i]]) <- gsub(vec_rrr[i], paste0("amp", i), names(beta.r))

    acr[[i]] <- atan2(groups.s, groups.r)
    # acr[[i]] <- atan2(-groups.s, groups.r)
    # acr[[i]] <- -atan2(groups.s, groups.r)

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
