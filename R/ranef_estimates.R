#' Extract rhythm parameter estimates at the random-effect level
#'
#' For a mixed \code{cglmm} model, computes the amplitude, acrophase, and
#' mesor for each level of a random-effect grouping variable, by combining
#' that level's conditional mode (BLUP) with the corresponding population
#' (fixed-effect) estimate. This gives a per-group (e.g. per-subject)
#' point estimate of the rhythm, complementing the population-level
#' estimates returned by \code{summary()}.
#'
#' By default, no standard errors are returned. \code{glmmTMB} does not
#' provide a method to obtain the joint uncertainty of the conditional
#' modes (random effects) together with the fixed effects, so a fully
#' correct delta-method confidence interval for these estimates isn't
#' available the way it is for \code{summary()}'s population-level
#' estimates. Instead, set \code{ranef_ci} to a confidence level (e.g.
#' \code{0.95}) to add percentile bootstrap confidence intervals, computed
#' from replicates added via \code{add_ranef_boots()} (run that first).
#' See \code{vignette("mixed-models")} for a worked example and further
#' discussion.
#'
#' This function assumes a common, simple random-effects structure: a
#' single grouping variable (\code{ranef_group}), with random terms
#' produced by referencing \code{amp_acroN} and/or an intercept in a
#' \code{(... | ranef_group)} term (as documented in \code{amp_acro()}).
#' If the model has additional covariates beyond the grouping variable(s)
#' used within \code{amp_acro()}, the mesor is computed holding those
#' covariates at their reference (baseline) level - it does not account
#' for their effect on individual groups.
#'
#' @param object A \code{cglmm} object with at least one random effect.
#' @param ranef_group A \code{character} naming the random-effect grouping
#' variable to compute estimates for. Required if \code{object} has more
#' than one random-effect grouping variable; otherwise inferred
#' automatically.
#' @param ranef_ci \code{NULL} (default) for no confidence interval, or a
#' single number giving the confidence level (e.g. \code{0.95}) at which
#' to add percentile bootstrap confidence interval columns, using
#' replicates added via \code{add_ranef_boots()}. Requires
#' \code{add_ranef_boots()} to have been run on \code{object} already.
#'
#' @return A \code{data.frame} with one row per component per level of
#' \code{ranef_group}, and columns for the grouping level, the component
#' index, amplitude, and acrophase (mesor is included in a separate
#' \code{component = "mesor"} block, since it doesn't vary by component).
#' If \code{ranef_ci} is not \code{NULL}, lower/upper confidence bound
#' columns are also included for amplitude, acrophase, and mesor.
#'
#' @examples
#' set.seed(1)
#' n_id <- 15
#' dat_mixed <- do.call(
#'   "rbind",
#'   lapply(seq_len(n_id), function(id) {
#'     d <- simulate_cosinor(
#'       n = 20,
#'       mesor = rnorm(1),
#'       amp = rnorm(1, mean = 3, sd = 0.5),
#'       acro = rnorm(1, mean = 1.5, sd = 0.2),
#'       family = "gaussian",
#'       period = 24,
#'       n_components = 1
#'     )
#'     d$subject <- id
#'     d
#'   })
#' )
#' dat_mixed$subject <- as.factor(dat_mixed$subject)
#'
#' mixed_mod <- cglmm(
#'   Y ~ amp_acro(times, n_components = 1, period = 24) +
#'     (1 + amp_acro1 | subject),
#'   data = dat_mixed
#' )
#'
#' ranef_estimates(mixed_mod)
#' @export
ranef_estimates <- function(
  object,
  ranef_group = NULL,
  ranef_ci = NULL
) {
  assertthat::assert_that(
    inherits(object, "cglmm"),
    msg = "'object' must be of class 'cglmm'"
  )
  assertthat::assert_that(
    !all(is.na(object$ranef_groups)),
    msg = "'object' does not have any random effects."
  )
  validate_ranef_ci(ranef_ci)
  ranef_group <- resolve_ranef_group(object, ranef_group)

  out <- ranef_estimates_core(
    fit = object$fit,
    raw_coefs = object$raw_coefficients,
    components = object$components,
    vec_rrr = object$vec_rrr,
    vec_sss = object$vec_sss,
    n_components = object$n_components,
    newdata = object$newdata,
    ranef_group = ranef_group
  )

  if (!is.null(ranef_ci)) {
    boots <- get_ranef_boots(object, ranef_group)
    out <- add_ranef_ci_columns(out, boots$estimates, ranef_group, ranef_ci)
  }

  out
}

#' Resolve and validate the random-effect grouping variable to use,
#' inferring it automatically when \code{object} has exactly one.
#' @noRd
resolve_ranef_group <- function(object, ranef_group) {
  ranef_groups <- object$ranef_groups
  if (is.null(ranef_group)) {
    assertthat::assert_that(
      length(ranef_groups) == 1,
      msg = paste0(
        "'object' has more than one random-effect grouping variable; ",
        "'ranef_group' must be specified as one of: ",
        paste(ranef_groups, collapse = ", ")
      )
    )
    return(ranef_groups)
  }
  assertthat::assert_that(
    ranef_group %in% ranef_groups,
    msg = paste0(
      "'ranef_group' must be one of the random-effect grouping ",
      "variables in 'object': ",
      paste(ranef_groups, collapse = ", ")
    )
  )
  ranef_group
}

#' Validate a \code{ranef_ci} argument: either \code{NULL} (no CI) or a
#' single number giving the confidence level to use (e.g. \code{0.95}).
#' @noRd
validate_ranef_ci <- function(ranef_ci) {
  if (is.null(ranef_ci)) {
    return(invisible(NULL))
  }
  assertthat::assert_that(
    is.numeric(ranef_ci) && length(ranef_ci) == 1,
    msg = paste(
      "'ranef_ci' must be NULL (no confidence interval), or a single",
      "number giving the confidence level, e.g. 'ranef_ci = 0.95'"
    )
  )
  validate_ci_level(ranef_ci)
  invisible(NULL)
}

#' Add percentile bootstrap CI columns to a \code{ranef_estimates()}-shaped
#' table, using stored bootstrap replicate estimates.
#' @noRd
add_ranef_ci_columns <- function(est, boot_estimates, ranef_group, ci_level) {
  alpha <- 1 - ci_level
  probs <- c(alpha / 2, 1 - alpha / 2)

  ci_bounds <- function(param) {
    t(vapply(
      seq_len(nrow(est)),
      function(i) {
        lv <- est[[ranef_group]][i]
        comp <- est$component[i]
        vals <- boot_estimates[[param]][
          boot_estimates[[ranef_group]] == lv & boot_estimates$component == comp
        ]
        if (length(vals) == 0 || all(is.na(vals))) {
          return(c(NA_real_, NA_real_))
        }
        stats::quantile(vals, probs = probs, na.rm = TRUE, names = FALSE)
      },
      numeric(2)
    ))
  }

  amp_ci <- ci_bounds("amp")
  acr_ci <- ci_bounds("acr")
  mesor_ci <- ci_bounds("mesor")

  est$amp_lower <- amp_ci[, 1]
  est$amp_upper <- amp_ci[, 2]
  est$acr_lower <- acr_ci[, 1]
  est$acr_upper <- acr_ci[, 2]
  est$mesor_lower <- mesor_ci[, 1]
  est$mesor_upper <- mesor_ci[, 2]
  est
}

#' Core computation shared by \code{ranef_estimates()} and
#' \code{add_ranef_boots()}, parameterised on \code{fit}/\code{raw_coefs} so
#' bootstrap refits (which share the same structure but have a different
#' fit and raw coefficients) can reuse it without needing a full
#' \code{cglmm} object.
#' @noRd
ranef_estimates_core <- function(
  fit,
  raw_coefs,
  components,
  vec_rrr,
  vec_sss,
  n_components,
  newdata,
  ranef_group
) {
  re <- glmmTMB::ranef(fit, condVar = FALSE)$cond[[ranef_group]]
  levels_ranef <- rownames(re)

  fixed_term_value <- function(group, group_level, term) {
    if (identical(group, 0) || is.null(group)) {
      val <- raw_coefs[[term]]
      return(if (is.null(val)) 0 else val)
    }
    candidates <- c(
      paste0(group, group_level, ":", term),
      paste0(term, ":", group, group_level)
    )
    hit <- candidates[candidates %in% names(raw_coefs)]
    if (length(hit) == 0) {
      return(0)
    }
    raw_coefs[[hit[1]]]
  }

  group_level_lookup <- function(group) {
    if (identical(group, 0) || is.null(group)) {
      return(NULL)
    }
    vals <- tapply(
      newdata[[group]],
      newdata[[ranef_group]],
      function(x) as.character(x[1])
    )
    vals[levels_ranef]
  }

  component_rows <- lapply(seq_len(n_components), function(i) {
    period_idx <- components[[i]]$period_idx
    group <- components[[i]]$group
    rrr_name <- vec_rrr[period_idx]
    sss_name <- vec_sss[period_idx]
    lvl_map <- group_level_lookup(group)

    fixed_r <- vapply(
      levels_ranef,
      function(lv) {
        fixed_term_value(group, if (is.null(lvl_map)) NULL else lvl_map[[lv]], rrr_name)
      },
      numeric(1)
    )
    fixed_s <- vapply(
      levels_ranef,
      function(lv) {
        fixed_term_value(group, if (is.null(lvl_map)) NULL else lvl_map[[lv]], sss_name)
      },
      numeric(1)
    )

    ranef_r <- if (rrr_name %in% colnames(re)) re[[rrr_name]] else rep(0, length(levels_ranef))
    ranef_s <- if (sss_name %in% colnames(re)) re[[sss_name]] else rep(0, length(levels_ranef))

    total_r <- fixed_r + ranef_r
    total_s <- fixed_s + ranef_s

    data.frame(
      ranef_group = levels_ranef,
      component = i,
      amp = sqrt(total_r^2 + total_s^2),
      acr = atan2(total_s, total_r),
      row.names = NULL
    )
  })

  fixed_groups <- unique(unlist(lapply(components, function(cmp) cmp$group)))
  fixed_groups <- fixed_groups[fixed_groups != 0]

  ranef_intercept <- if ("(Intercept)" %in% colnames(re)) {
    re[["(Intercept)"]]
  } else {
    rep(0, length(levels_ranef))
  }
  fixed_intercept <- raw_coefs[["(Intercept)"]]
  if (is.null(fixed_intercept)) {
    fixed_intercept <- 0
  }

  covariate_effect <- vapply(
    levels_ranef,
    function(lv) {
      sum(vapply(
        fixed_groups,
        function(group) {
          lvl_map <- group_level_lookup(group)
          candidate <- paste0(group, lvl_map[[lv]])
          if (!candidate %in% names(raw_coefs)) {
            return(0)
          }
          raw_coefs[[candidate]]
        },
        numeric(1)
      ))
    },
    numeric(1)
  )

  mesor_row <- data.frame(
    ranef_group = levels_ranef,
    component = "mesor",
    amp = NA_real_,
    acr = NA_real_,
    mesor = fixed_intercept + covariate_effect + ranef_intercept,
    row.names = NULL
  )

  component_table <- do.call(rbind, component_rows)
  component_table$mesor <- NA_real_

  out <- rbind(component_table, mesor_row)
  names(out)[names(out) == "ranef_group"] <- ranef_group
  out
}

#' Compute fitted rhythm curves at the random-effect level, for plotting.
#'
#' Uses \code{ranef_estimates()} to build each grouping level's fitted
#' curve, \code{mesor + sum(amp * cos(2*pi*time/period - acr))}, evaluated
#' at \code{time_vec} and mapped to the response scale via the model's
#' link function.
#'
#' @param object A \code{cglmm} object.
#' @param ranef_group A \code{character} naming the random-effect grouping
#' variable (see \code{ranef_estimates()}).
#' @param time_vec A numeric vector of time values at which to evaluate
#' each curve.
#'
#' @return A long-format \code{data.frame} with one row per grouping level
#' per element of \code{time_vec}, and columns for the grouping level,
#' time, and fitted response value.
#' @noRd
ranef_curve_data <- function(object, ranef_group, time_vec) {
  est <- ranef_estimates(object, ranef_group = ranef_group)
  periods <- vapply(object$components, function(cmp) cmp$period, numeric(1))
  linkinv <- stats::family(object$fit)$linkinv
  curve_from_estimates(est, ranef_group, periods, linkinv, time_vec)
}

#' Evaluate cosinor curves from a \code{ranef_estimates()}-shaped table.
#' Factored out of \code{ranef_curve_data()} so bootstrap replicates (which
#' produce the same shape of table) can reuse the curve evaluation.
#' @noRd
curve_from_estimates <- function(est, ranef_group, periods, linkinv, time_vec) {
  levels_ranef <- unique(est[[ranef_group]])

  curves <- lapply(levels_ranef, function(lv) {
    lv_rows <- est[est[[ranef_group]] == lv, ]
    mesor <- lv_rows$mesor[lv_rows$component == "mesor"]
    component_rows <- lv_rows[lv_rows$component != "mesor", ]

    eta <- mesor
    for (i in seq_len(nrow(component_rows))) {
      comp_idx <- as.integer(component_rows$component[i])
      eta <- eta +
        component_rows$amp[i] *
          cos(2 * pi * time_vec / periods[comp_idx] - component_rows$acr[i])
    }

    data.frame(
      ranef_group = lv,
      time = time_vec,
      fitted = linkinv(eta)
    )
  })

  out <- do.call(rbind, curves)
  names(out)[names(out) == "ranef_group"] <- ranef_group
  out
}

#' Compute bootstrap confidence-ellipse parameters for each level of a
#' random-effect grouping variable, for a given component, in the same
#' rrr/sss (x, y) coordinates used to plot the point estimate.
#'
#' Uses the empirical covariance of each level's bootstrap (rrr, sss) draws
#' (converted from the amplitude/acrophase replicates via the same
#' \code{direction}/\code{offset} transform used for the point estimate),
#' via a standard bivariate-normal confidence ellipse (eigendecomposition
#' of the covariance matrix, scaled by the chi-squared quantile for 2
#' degrees of freedom). The ellipse is centred on the point estimate
#' itself (not the bootstrap mean), so it lines up with the point already
#' plotted.
#'
#' @param object A \code{cglmm} object with bootstrap estimates attached
#' via \code{add_ranef_boots()}.
#' @param ranef_group A \code{character} naming the random-effect grouping
#' variable.
#' @param component_index Which component to compute ellipses for.
#' @param direction,offset Passed through from the calling plot function,
#' to match the point estimate's coordinate transform.
#' @param ci_level Confidence level for the ellipse.
#'
#' @return A \code{data.frame} with one row per grouping level, and
#' columns \code{x0}, \code{y0} (centre), \code{a}, \code{b} (semi-axis
#' lengths), and \code{angle} (radians), suitable for
#' \code{ggforce::geom_ellipse()}.
#' @noRd
ranef_ci_ellipse_data <- function(
  object,
  ranef_group,
  component_index,
  direction,
  offset,
  ci_level = 0.95
) {
  est <- ranef_estimates(object, ranef_group = ranef_group)
  est <- est[est$component == as.character(component_index), ]

  boots <- get_ranef_boots(object, ranef_group)$estimates
  boots <- boots[boots$component == as.character(component_index), ]

  chisq_val <- stats::qchisq(ci_level, df = 2)
  levels_ranef <- est[[ranef_group]]

  rows <- lapply(levels_ranef, function(lv) {
    lv_boots <- boots[boots[[ranef_group]] == lv, ]
    rrr_rep <- lv_boots$amp * cos(direction * lv_boots$acr + offset)
    sss_rep <- lv_boots$amp * sin(direction * lv_boots$acr + offset)

    lv_est <- est[est[[ranef_group]] == lv, ]
    x0 <- lv_est$amp * cos(direction * lv_est$acr + offset)
    y0 <- lv_est$amp * sin(direction * lv_est$acr + offset)

    if (length(rrr_rep) < 3 || stats::sd(rrr_rep) == 0 || stats::sd(sss_rep) == 0) {
      return(data.frame(x0 = x0, y0 = y0, a = 0, b = 0, angle = 0))
    }

    eig <- eigen(stats::cov(cbind(rrr_rep, sss_rep)))
    axes <- sqrt(pmax(eig$values, 0) * chisq_val)

    data.frame(
      x0 = x0,
      y0 = y0,
      a = axes[1],
      b = axes[2],
      angle = atan2(eig$vectors[2, 1], eig$vectors[1, 1])
    )
  })

  do.call(rbind, rows)
}

#' Compute pointwise bootstrap percentile bands for each level's fitted
#' rhythm curve, evaluating every bootstrap replicate's curve across
#' \code{time_vec} and taking quantiles across replicates at each time
#' point. This reflects the joint uncertainty in amplitude, acrophase, and
#' mesor together, since each replicate's curve is one coherent draw.
#'
#' @param object A \code{cglmm} object with bootstrap estimates attached
#' via \code{add_ranef_boots()}.
#' @param ranef_group A \code{character} naming the random-effect grouping
#' variable.
#' @param time_vec A numeric vector of time values at which to evaluate.
#' @param ci_level Confidence level for the band.
#'
#' @return A long-format \code{data.frame} with one row per grouping level
#' per element of \code{time_vec}, and columns for the grouping level,
#' time, and lower/upper fitted response bounds.
#' @noRd
ranef_ci_ribbon_data <- function(object, ranef_group, time_vec, ci_level = 0.95) {
  boots <- get_ranef_boots(object, ranef_group)$estimates
  periods <- vapply(object$components, function(cmp) cmp$period, numeric(1))
  linkinv <- stats::family(object$fit)$linkinv

  alpha <- 1 - ci_level
  probs <- c(alpha / 2, 1 - alpha / 2)

  rep_curves <- lapply(unique(boots$.rep), function(r) {
    curve_from_estimates(
      boots[boots$.rep == r, ],
      ranef_group,
      periods,
      linkinv,
      time_vec
    )
  })
  all_curves <- do.call(rbind, rep_curves)

  levels_ranef <- unique(all_curves[[ranef_group]])
  bands <- lapply(levels_ranef, function(lv) {
    lv_curves <- all_curves[all_curves[[ranef_group]] == lv, ]
    bounds <- vapply(
      time_vec,
      function(t) {
        stats::quantile(
          lv_curves$fitted[lv_curves$time == t],
          probs = probs,
          na.rm = TRUE,
          names = FALSE
        )
      },
      numeric(2)
    )
    data.frame(
      ranef_group = lv,
      time = time_vec,
      lower = bounds[1, ],
      upper = bounds[2, ]
    )
  })

  out <- do.call(rbind, bands)
  names(out)[names(out) == "ranef_group"] <- ranef_group
  out
}
