data_processor <- function(newdata,newformula,vec_sss,vec_rrr,n_components,
                           group_stats,group,group_check, period,family_var,Terms, ...)  {

  # Fit the data and formula to a model
  fit <- glmmTMB::glmmTMB(
    formula = newformula,
    data = newdata,
    family = family_var,
    ...
  )

  # Retrieve the fit, coefficients from the model and priming vectors
  # in preparation for transforming the raw coefficients
  mf <- fit
  coefs <- glmmTMB::fixef(mf)$cond
  r.coef <- NULL
  s.coef <- NULL
  mu.coef <- NULL
  mu_inv <- rep(0, length(names(coefs)))

  # Get a Boolean vector for rrr, sss, and mu. This will be used to extract
  # the relevant raw parameters from the raw coefficient model output
  for (i in 1:n_components) {
    r.coef[[i]] <- grepl(paste0(vec_rrr[i], "$"), names(coefs))
    s.coef[[i]] <- grepl(paste0(vec_sss[i], "$"), names(coefs))

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

    groups.r <- c(beta.r[vec_rrr[i]], beta.r[vec_rrr[i]] + beta.r[which(names(beta.r) != vec_rrr[i])])
    groups.s <- c(beta.s[vec_sss[i]], beta.s[vec_sss[i]] + beta.s[which(names(beta.s) != vec_sss[i])])

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

  # Arrange and display the output
  structure(
    list(
      formula = newformula,
      fit = fit,
      Call = match.call(),
      Terms = Terms,
      coefficients = new_coefs,
      raw_coefficients = coefs,
      period = period,
      group_stats = group_stats,
      group = group,
      group_check = group_check
    ),
    class = "cosinor.glmm"
  )

}
