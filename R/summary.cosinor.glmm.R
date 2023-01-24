#' Summarize a cosinor model
#'
#' Given a time variable and optional covariates, generate inference a cosinor
#' fit. Gives estimates, confidence intervals, and tests for the raw parameters,
#' and for the mean, amplitude, and acrophase parameters. If the model includes
#' covariates, the function returns the estimates of the mean, amplitude, and
#' acrophase for the group with covariates equal to 1 and equal to 0. This may
#' not be the desired result for continuous covariates.
#'
#'
#' @param object An object of class \code{cosinor.glmm}
#' @param ... Currently unused
#'
#'
#' @examples
#'
#'
#' fit <- cosinor.glmm(Y ~ time(time) + X + amp.acro(X), data = vitamind)
#' summary(fit)
#'
#' @export

summary.cosinor.glmm <- function(object, ...) {
  mf <- object$fit
  n_components <- object$n_components
  vec_rrr <- object$vec_rrr
  vec_sss <- object$vec_sss


  # TOOD: validate summary outputs with test-script using parts of the simulation study code
  # TODO: structure output of summary script so that zi and disp are displayed in output

  # TODO: wrap in function from here
  coefs <- glmmTMB::fixef(mf)$cond # TODO: do this for zi and disp if present in initial formula
  r.coef <- NULL
  s.coef <- NULL
  mu.coef <- NULL
  mu_inv <- rep(0, length(names(coefs)))

  if (length(vec_rrr) > 1) {
    vec_rrr_spec <- vec_rrr[1]
    vec_sss_spec <- vec_sss[1]
    for (i in 2:length(vec_rrr)) {
      vec_rrr_spec <- paste0(vec_rrr_spec, "|", vec_rrr[i])
      vec_sss_spec <- paste0(vec_sss_spec, "|", vec_sss[i])
    }
  } else {
    vec_rrr_spec <- vec_rrr
    vec_sss_spec <- vec_sss
  }
  # Get a Boolean vector for rrr, sss, and mu. This will be used to extract
  # the relevant raw parameters from the raw coefficient model output

  r.coef <- grepl(vec_rrr_spec, names(coefs))
  s.coef <- grepl(vec_sss_spec, names(coefs))

  mu_inv_carry <- r.coef + s.coef # Keep track of non-mesor terms
  mu_inv <- mu_inv_carry + mu_inv # Ultimately,every non-mesor term will be true


  mu.coef <- c(!mu_inv) # invert 'mu_inv' to get a Boolean vector for mesor terms
  r.coef <- (t(matrix(unlist(r.coef), ncol = length(r.coef)))) # a matrix of rrr coefficients
  s.coef <- (t(matrix(unlist(s.coef), ncol = length(s.coef)))) # a matrix of sss coefficients


  beta.s <- coefs[s.coef]
  beta.r <- coefs[r.coef]

  groups.r <- c(beta.r, beta.r[which(names(beta.r) != names(beta.r))])
  groups.s <- c(beta.s, beta.s[which(names(beta.s) != names(beta.s))])

  amp <- sqrt(groups.r^2 + groups.s^2)
  acr <- -atan2(groups.s, groups.r)

  for (i in 1:n_components) {
    names(amp) <- gsub(vec_rrr[i], paste0("amp", i), names(amp))
    names(acr) <- gsub(vec_sss[i], paste0("acr", i), names(acr))
  }

  vmat <- vcov(mf)$cond[
    c(which(r.coef), which(s.coef)),
    c(which(r.coef), which(s.coef))
  ]
  ##
  # if n_components = 1, then print "amp" and "acr" rather than "amp1", "acr1"
  if (n_components == 1) {
    names(amp) <- gsub(vec_rrr, "amp", names(amp))
    names(acr) <- gsub(vec_sss, "acr", names(acr))
    new_coefs <- c(coefs[mu.coef], unlist(amp), unlist(acr))
  }
  ##

  index.s <- matrix(0, nrow = length(groups.r), ncol = length(groups.r))
  index.r <- matrix(0, nrow = length(groups.s), ncol = length(groups.s))

  index.r[, 1] <- index.s[, 1] <- 1
  diag(index.r) <- diag(index.s) <- 1
  indexmat <- rbind(
    cbind(index.r, index.s * 0),
    cbind(index.r * 0, index.s)
  )

  indVmat <- indexmat %*% vmat %*% t(indexmat)
  a_r <- (groups.r^2 + groups.s^2)^(-0.5) * groups.r
  a_s <- (groups.r^2 + groups.s^2)^(-0.5) * groups.s

  b_r <- (1 / (1 + (groups.s^2 / groups.r^2))) * (-groups.s / groups.r^2)
  b_s <- (1 / (1 + (groups.s^2 / groups.r^2))) * (1 / groups.r)

  if (length(groups.r) == 1) {
    jac <- matrix(c(a_r, a_s, b_r, b_s), byrow = TRUE, nrow = 2)
  } else {
    jac <- rbind(
      cbind(diag(a_r), diag(a_s)),
      cbind(diag(b_r), diag(b_s))
    )
  }

  cov.trans <- jac %*% indVmat %*% t(jac)
  se.trans <- sqrt(diag(cov.trans))

  ## assemble summary matrix
  coef <- c(coefs[mu.coef], unlist(amp), unlist(acr))
  se <- c(sqrt(diag(vcov(mf)$cond))[mu.coef], se.trans)


  zt <- stats::qnorm((1 - .95) / 2, lower.tail = F)
  raw.se <- sqrt(diag(vcov(mf)$cond))

  rawmat <- cbind(
    estimate = coefs, standard.error = raw.se,
    lower.CI = coefs - zt * raw.se, upper.CI = coefs + zt * raw.se,
    p.value = 2 * stats::pnorm(-abs(coefs / raw.se))
  )

  smat <- cbind(
    estimate = coef, standard.error = se, lower.CI = coef - zt * se,
    upper.CI = coef + zt * se, p.value = 2 * stats::pnorm(-abs(coef / se))
  )

  if (object$group_check) {
  rownames(smat) <- update_covnames(rownames(smat), object$group_stats)
  }




  ## Need to modify with additional zi and dispersion (or default)
  structure(list(transformed.table = as.data.frame(smat), raw.table = as.data.frame(rawmat), transformed.covariance = cov.trans, raw.covariance = vmat), class = "summary.cosinor.glmm")
  ### TO HERE
}


#' Print the summary of a cosinor model
#'
#' @param x An object of class \code{summary.cosinor.glmm}
#' @param ... Currently unused
#'
#'
#' @examples
#'
#' fit <- cosinor.glmm(Y ~ time(time) + X + amp.acro(X), data = vitamind)
#' summary(fit)
#'
#' @export
#'

# check if there is dispersion or zi (as opposed to default) then print
print.summary.cosinor.glmm <- function(x, ...) {
  cat("Raw model coefficients:\n")
  print(round(x$raw.table, 4))
  cat("\n***********************\n\n")
  cat("Transformed coefficients:\n")
  print(round(x$transformed.table, 4))
}
