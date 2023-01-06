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
#' fit <- cosinor.glmm(Y ~ time(time) + X + amp.acro(X), data = vitamind)
#' summary(fit)
#'
#' @export
#'

summary.cosinor.glmm <- function(object, ...) {
  mf <- object$fit
  n_components <- object$n_components
  vec_rrr <- object$vec_rrr
  vec_sss <- object$vec_sss

  coefs <- glmmTMB::fixef(mf)$cond
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
  smat_c <- NULL
  rawmat_c <- NULL
  cov.trans_c <- NULL
  cov.trans <- NULL
  se.trans <- NULL

  disp <- NULL
  for (i in 1:n_components) {
    #r.coef <- grepl(paste0(vec_rrr[i]), names(coefs))
    #s.coef <- grepl(paste0(vec_sss[i]), names(coefs))


    beta.s <- coefs[s.coef[i, ]]
    beta.r <- coefs[r.coef[i, ]]

    groups.r <- c(beta.r[1], beta.r[which(names(beta.r) != names(beta.r[1]))])
    groups.s <- c(beta.s[1], beta.s[which(names(beta.s) != names(beta.s[1]))])

    amp[[i]] <- sqrt(groups.r^2 + groups.s^2)
    names(amp[[i]]) <- gsub(vec_rrr[i], paste0("amp", i), names(beta.r))

    acr[[i]] <- -atan2(groups.s, groups.r)
    names(acr[[i]]) <- gsub(vec_sss[i], paste0("acr", i), names(beta.s))
    vmat <- vcov(mf)$cond[c(which(r.coef[i,]), which(s.coef[i,])),
                          c(which(r.coef[i,]), which(s.coef[i,]))]
    ##
    # if n_components = 1, then print "amp" and "acr" rather than "amp1", "acr1"
    if (n_components == 1) {
      names(amp[[1]]) <- gsub(vec_rrr[1], "amp", names(beta.r))
      names(acr[[1]]) <- gsub(vec_sss[1], "acr", names(beta.s))
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
    cov.trans_c[[i]] <- cov.trans

    se.trans <- append(se.trans,sqrt(diag(cov.trans)))

  }
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

    rownames(smat) <- update_covnames(rownames(smat), object$group_stats)


  names(cov.trans_c) <- paste("component number =",seq(from = 1, to = n_components, by = 1))
  structure(list(transformed.table = as.data.frame(smat), raw.table = as.data.frame(rawmat), transformed.covariance = cov.trans_c), class = "summary.cosinor.glmm")

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

print.summary.cosinor.glmm <- function(x, ...) {
  cat("Raw model coefficients:\n")
  print(round(x$raw.table, 4))
  cat("\n***********************\n\n")
  cat("Transformed coefficients:\n")
  print(round(x$transformed.table, 4))
}
