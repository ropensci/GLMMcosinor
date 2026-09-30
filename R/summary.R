#' Summarize a cosinor model
#' Given a time variable and optional covariates, generate inference a cosinor
#' fit. Gives estimates, confidence intervals, and tests for the raw parameters,
#' and for the mean, amplitude, and acrophase parameters. If the model includes
#' covariates, the function returns the estimates of the mean, amplitude, and
#' acrophase for the group with covariates equal to 1 and equal to 0. This may
#' not be the desired result for continuous covariates.
#'
#' @param object An object of class \code{cglmm}
#' @param ci_level The level for calculated confidence intervals. Defaults to
#' 0.95.
#' @param ... Currently unused
#'
#' @srrstats {G1.4}
#' @srrstats {RE4.18}
#'
#' @return Returns a summary of the `cglmm` model as
#' a `cglmmSummary` object.
#' @examples
#'
#'
#' fit <- cglmm(vit_d ~ X + amp_acro(time,
#'   group = "X",
#'   n_components = 1,
#'   period = 12
#' ), data = vitamind)
#' summary(fit)
#'
#' @export

summary.cglmm <- function(object, ci_level = 0.95, ...) {
  # get the fitted model from the cglmm() output, along with
  # n_components, vec_rrr, and vec_sss
  mf <- object$fit
  n_components <- object$n_components
  vec_rrr <- object$vec_rrr
  vec_sss <- object$vec_sss

  validate_ci_level(ci_level)

  # this function can be looped if there is disp or zi formula present.
  # note that 'model_index' is a string: 'cond', 'disp', or 'zi'
  cglmmSubSummary <- function(model_index) {
    if (model_index == "disp") {
      n_components <- object$disp_list$n_components_disp
    }
    if (model_index == "zi") {
      n_components <- object$zi_list$n_components_zi
    }

    # get the arguments from the function wrapping this function
    args <- match.call()[-1]
    coefs <- glmmTMB::fixef(mf)[[model_index]]

    # reassign vec_rrr, vec_sss and components to those in the disp or zi
    # model, if necessary
    if (model_index == "disp") {
      vec_rrr <- object$disp_list$vec_rrr_disp
      vec_sss <- object$disp_list$vec_sss_disp
      components <- object$disp_list$components_disp
    } else if (model_index == "zi") {
      vec_rrr <- object$zi_list$vec_rrr_zi
      vec_sss <- object$zi_list$vec_sss_zi
      components <- object$zi_list$components_zi
    } else {
      components <- object$components
    }

    # create objects r.coef, s.coef, and mu.coef. This will be Boolean vectors
    # (one per component) that indicate the position of particular
    # coefficients in coefs. Components are matched via their `period_idx`
    # (not their component number) since components that share a period
    # share the same underlying rrr/sss columns, and are matched via their
    # `group` (when grouped) so that components sharing rrr/sss columns but
    # belonging to different groups are kept separate. This mirrors
    # `get_new_coefs()` in data_utils.R, used by `print()`.
    r.coef <- NULL
    s.coef <- NULL
    mu_inv <- rep(0, length(names(coefs)))

    for (i in seq_len(n_components)) {
      period_idx <- components[[i]]$period_idx
      group <- components[[i]]$group

      if (group != 0) {
        r.coef[[i]] <- grepl(
          paste0(group, ".*:", vec_rrr[period_idx]),
          names(coefs)
        )
        s.coef[[i]] <- grepl(
          paste0(group, ".*:", vec_sss[period_idx]),
          names(coefs)
        )
      } else {
        r.coef[[i]] <- grepl(vec_rrr[period_idx], names(coefs))
        s.coef[[i]] <- grepl(vec_sss[period_idx], names(coefs))
      }

      # Keep track of non-mesor terms
      mu_inv_carry <- r.coef[[i]] + s.coef[[i]]
      # Ultimately, every non-mesor term will be true
      mu_inv <- mu_inv_carry + mu_inv
    }

    # invert 'mu_inv' to get a Boolean vector for mesor terms
    mu.coef <- c(!mu_inv)
    # a matrix of rrr coefficients (one row per component)
    r.coef.mat <- (t(matrix(unlist(r.coef), ncol = length(r.coef))))
    # a matrix of sss coefficients (one row per component)
    s.coef.mat <- (t(matrix(unlist(s.coef), ncol = length(s.coef))))

    amp <- NULL
    acr <- NULL
    a_r <- NULL
    a_s <- NULL
    b_r <- NULL
    b_s <- NULL
    r_positions <- NULL
    s_positions <- NULL

    for (i in seq_len(n_components)) {
      period_idx <- components[[i]]$period_idx

      beta.s <- coefs[s.coef.mat[i, ]]
      beta.r <- coefs[r.coef.mat[i, ]]

      # convert beta.s and beta.r to groups
      groups.r <- c(beta.r[1], beta.r[which(names(beta.r) != names(beta.r[1]))])
      groups.s <- c(beta.s[1], beta.s[which(names(beta.s) != names(beta.s[1]))])

      # calculate parameters amp and acr for this component
      amp_label <- if (n_components == 1) "amp" else paste0("amp", i)
      acr_label <- if (n_components == 1) "acr" else paste0("acr", i)

      amp[[i]] <- sqrt(groups.r^2 + groups.s^2)
      names(amp[[i]]) <- gsub(vec_rrr[period_idx], amp_label, names(beta.r))

      # acr <- -atan2(groups.s, groups.r)
      acr[[i]] <- atan2(groups.s, groups.r)
      names(acr[[i]]) <- gsub(vec_sss[period_idx], acr_label, names(beta.s))

      # determine the partial derivatives of amplitude and acrophase

      # a_r is the partial derivative of amp with respect to r.
      # hence, a_r = d(amp)/d(groups.r),
      # where amp = sqrt(groups.r^2 + groups.s^2). Likewise for a_s
      a_r[[i]] <- (groups.r^2 + groups.s^2)^(-0.5) * groups.r
      a_s[[i]] <- (groups.r^2 + groups.s^2)^(-0.5) * groups.s

      # b_r is the partial derivative of acr with respect to r.
      # hence, b_r = d(acr)/d(groups.r),
      # where acr = arctan(s/r). Likewise for b_s
      b_r[[i]] <- (1 / (1 + (groups.s^2 / groups.r^2))) * (-groups.s / groups.r^2)
      b_s[[i]] <- (1 / (1 + (groups.s^2 / groups.r^2))) * (1 / groups.r)

      # track the positions (in `coefs`) used for this component's r/s
      # coefficients, in the same order as beta.r/beta.s, so the
      # variance-covariance submatrix can be extracted in matching order
      r_positions <- c(r_positions, which(r.coef.mat[i, ]))
      s_positions <- c(s_positions, which(s.coef.mat[i, ]))
    }

    a_r <- unlist(a_r)
    a_s <- unlist(a_s)
    b_r <- unlist(b_r)
    b_s <- unlist(b_s)

    # calculate the variance-covariance matrix, ordered to match a_r/a_s/b_r/b_s
    vmat <- stats::vcov(mf)[[model_index]][
      c(r_positions, s_positions),
      c(r_positions, s_positions)
    ]

    # jac is the jacobian matrix, a matrix of partial derivatives
    if (length(a_r) == 1) {
      jac <- matrix(c(a_r, a_s, b_r, b_s), byrow = TRUE, nrow = 2)
    } else {
      jac <- rbind(
        cbind(diag(a_r), diag(a_s)),
        cbind(diag(b_r), diag(b_s))
      )
    }

    # apply the delta method
    cov.trans <- (jac) %*% vmat %*% t(jac)
    se.trans <- sqrt(diag(cov.trans))

    # assemble summary matrix
    coef <- c(coefs[mu.coef], unlist(amp), unlist(acr))
    se <- c(sqrt(diag(stats::vcov(mf)[[model_index]]))[mu.coef], se.trans)

    zt <- stats::qnorm((1 - ci_level) / 2, lower.tail = F)
    raw.se <- sqrt(diag(stats::vcov(mf)[[model_index]]))

    rawmat <- cbind(
      estimate = coefs,
      standard.error = raw.se,
      lower.CI = coefs - zt * raw.se,
      upper.CI = coefs + zt * raw.se,
      p.value = 2 * stats::pnorm(-abs(coefs / raw.se))
    )

    smat <- cbind(
      estimate = coef,
      standard.error = se,
      lower.CI = coef - zt * se,
      upper.CI = coef + zt * se,
      p.value = 2 * stats::pnorm(-abs(coef / se))
    )

    if (object$group_check) {
      rownames(smat) <- update_covnames(rownames(smat), object$group_stats)
    }

    structure(
      list(
        transformed.table = as.data.frame(smat),
        raw.table = as.data.frame(rawmat),
        transformed.covariance = cov.trans,
        raw.covariance = vmat
      ),
      class = "cglmmSubSummary"
    )
  }

  # store the output from the conditional model
  main_output <- cglmmSubSummary(model_index = "cond")

  # store the output from the dispersion model (if present)
  if (object$dispformula_check) {
    output_disp <- cglmmSubSummary(model_index = "disp")
  } else {
    output_disp <- NULL
  }

  # store the output from the zero-inflation model (if present)
  if (object$ziformula_check) {
    output_zi <- cglmmSubSummary(model_index = "zi")
  } else {
    output_zi <- NULL
  }

  # here, the outputs from main_output are renamed to remove the main_output tag
  # this was done to remain cohesive with other parts of the package.
  structure(
    list(
      transformed.table = main_output$transformed.table,
      raw.table = main_output$raw.table,
      transformed.covariance = main_output$transformed.covariance,
      raw.covariance = main_output$raw.covariance,
      main_output = main_output,
      output_disp = output_disp,
      output_zi = output_zi,
      object = object
    ),
    class = "cglmmSummary"
  )
}


#' Print the summary of a cosinor model
#'
#' @param x An object of class \code{cglmmSummary}
#' @param digits Controls the number of digits displayed in the summary output
#' @param ... Currently unused
#'
#' @srrstats {G1.4}
#' @return `print` returns `x` invisibly.
#' @examples
#'
#' fit <- cglmm(vit_d ~ X + amp_acro(time,
#'   group = "X",
#'   n_components = 1,
#'   period = 12
#' ), data = vitamind)
#' summary(fit)
#'
#' @export
#'

# check if there is dispersion or zi (as opposed to default) then print
print.cglmmSummary <- function(x, digits = getOption("digits"), ...) {
  cat("\n Conditional Model \n")
  cat("Raw model coefficients:\n")
  stats::printCoefmat(
    x$main_output$raw.table,
    digits = digits,
    has.Pvalue = TRUE
  )
  cat("\n")
  cat("Transformed coefficients:\n")
  stats::printCoefmat(
    x$main_output$transformed.table,
    digits = digits,
    has.Pvalue = TRUE
  )

  # display the output from the dispersion model (if present)

  if (!is.null(x$output_disp)) {
    cat("\n***********************\n")
    cat("\n Dispersion Model \n")
    cat("Raw model coefficients:\n")
    stats::printCoefmat(
      x$output_disp$raw.table,
      digits = digits,
      has.Pvalue = TRUE
    )
    cat("\n")
    cat("Transformed coefficients:\n")
    stats::printCoefmat(
      x$output_disp$transformed.table,
      digits = digits,
      has.Pvalue = TRUE
    )
  }

  if (!is.null(x$output_zi)) {
    cat("\n***********************\n")
    cat("\n Zero-Inflation Model \n")
    cat("Raw model coefficients:\n")
    stats::printCoefmat(
      x$output_zi$raw.table,
      digits = digits,
      has.Pvalue = TRUE
    )
    cat("\n")
    cat("Transformed coefficients:\n")
    stats::printCoefmat(
      x$output_zi$transformed.table,
      digits = digits,
      has.Pvalue = TRUE
    )
  }
  invisible(x)
}
