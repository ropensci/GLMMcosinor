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

#  ###
#  int = object$Call$int
#  if (int == 0) {
#    r.coef <- c(as.logical(attr(mf$modelInfo$terms$cond$fixed, "factors")["rrr", ]))
#    s.coef <- c(as.logical(attr(mf$modelInfo$terms$cond$fixed, "factors")["sss", ]))
#    mu.coef <- c(!(as.logical(attr(mf$modelInfo$terms$cond$fixed, "factors")["sss", ]) |
#                     as.logical(attr(mf$modelInfo$terms$cond$fixed, "factors")["rrr", ])))
#  }
#
#  if (int == 1) {
#
#    r.coef <- c(FALSE, as.logical(attr(mf$modelInfo$terms$cond$fixed, "factors")["rrr", ]))
#    s.coef <- c(FALSE, as.logical(attr(mf$modelInfo$terms$cond$fixed, "factors")["sss", ]))
#    mu.coef <- c(TRUE, !(as.logical(attr(mf$modelInfo$terms$cond$fixed, "factors")["sss", ]) |
#                           as.logical(attr(mf$modelInfo$terms$cond$fixed, "factors")["rrr", ])))
#
#  }
  ###

  r.coef <- c(FALSE, as.logical(attr(mf$modelInfo$terms$cond$fixed, "factors")["rrr", ]))
  s.coef <- c(FALSE, as.logical(attr(mf$modelInfo$terms$cond$fixed, "factors")["sss", ]))
  mu.coef <- c(TRUE, !(as.logical(attr(mf$modelInfo$terms$cond$fixed, "factors")["sss", ]) |
                         as.logical(attr(mf$modelInfo$terms$cond$fixed, "factors")["rrr", ])))



  coefs <- glmmTMB::fixef(mf)$cond


  ##
  coefs["group"]<- coefs[1]+coefs["group"]
  ##


  beta.s <- coefs[s.coef]
  beta.r <- coefs[r.coef]

  groups.r <- c(beta.r["rrr"], beta.r["rrr"] + beta.r[which(names(beta.r) != "rrr")])
  groups.s <- c(beta.s["sss"], beta.s["sss"] + beta.s[which(names(beta.s) != "sss")])

  amp <- sqrt(groups.r^2 + groups.s^2)
  names(amp) <- gsub("rrr", "amp", names(beta.r))

  acr <- -atan2(groups.s, groups.r)

  names(acr) <- gsub("sss", "acr", names(beta.s))

  ## delta method to get variance
  vmat <- vcov(mf)$cond[c(which(r.coef), which(s.coef)), c(which(r.coef), which(s.coef))]

  ## transform to get group coefficients

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
  coef <- c(coefs[mu.coef], amp, acr)
  se <- c(sqrt(diag(vcov(mf)$cond))[mu.coef], se.trans)

  zt <- stats::qnorm((1 - .95) / 2, lower.tail = F)
  raw.se <- sqrt(diag(vcov(mf)$cond))

  rawmat <- cbind(
    estimate = coefs, standard.error = raw.se,
    lower.CI = coefs - zt * raw.se, upper.CI = coefs + zt * raw.se,
    p.value = 2 * stats::pnorm(-abs(coefs / raw.se))
  )

  smat <- cbind(estimate = coef, standard.error = se, lower.CI = coef - zt * se,
                upper.CI = coef + zt * se, p.value = 2 * stats::pnorm(-abs(coef / se)))

  rownames(smat) <- update_covnames(rownames(smat))


  structure(list(transformed.table = as.data.frame(smat), raw.table = as.data.frame(rawmat), transformed.covariance = cov.trans), class = "summary.cosinor.glmm")
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



