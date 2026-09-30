#' Extract rhythm parameter estimates at the random-effect level
#'
#' For a mixed \code{cglmm} model, computes the amplitude, acrophase, and
#' mesor for each level of a random-effect grouping variable, by combining
#' that level's conditional mode (BLUP) with the corresponding population
#' (fixed-effect) estimate. This gives a per-group (e.g. per-subject)
#' point estimate of the rhythm, complementing the population-level
#' estimates returned by \code{summary()}.
#'
#' No standard errors are returned. \code{glmmTMB} does not provide a
#' method to obtain the joint uncertainty of the conditional modes (random
#' effects) together with the fixed effects, so a fully correct confidence
#' interval for these estimates isn't currently available. See
#' \code{vignette("mixed-models")} for a discussion of possible approaches.
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
#'
#' @return A \code{data.frame} with one row per component per level of
#' \code{ranef_group}, and columns for the grouping level, the component
#' index, amplitude, and acrophase (mesor is included in a separate
#' \code{component = "mesor"} block, since it doesn't vary by component).
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
ranef_estimates <- function(object, ranef_group = NULL) {
  assertthat::assert_that(
    inherits(object, "cglmm"),
    msg = "'object' must be of class 'cglmm'"
  )
  assertthat::assert_that(
    !all(is.na(object$ranef_groups)),
    msg = "'object' does not have any random effects."
  )
  ranef_group <- resolve_ranef_group(object, ranef_group)

  ranef_estimates_core(
    fit = object$fit,
    raw_coefs = object$raw_coefficients,
    components = object$components,
    vec_rrr = object$vec_rrr,
    vec_sss = object$vec_sss,
    n_components = object$n_components,
    newdata = object$newdata,
    ranef_group = ranef_group
  )
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
