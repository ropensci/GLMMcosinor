#' Add parametric-bootstrap uncertainty for random-effect rhythm estimates
#'
#' Runs a parametric bootstrap to estimate the uncertainty of the
#' random-effect rhythm estimates produced by \code{ranef_estimates()}, and
#' attaches the result to \code{object} so it can be reused by
#' \code{ranef_estimates()}, \code{polar_plot()}, and \code{autoplot()}
#' (via their \code{ranef_ci} argument) without needing to re-run the
#' bootstrap each time.
#'
#' \code{glmmTMB} does not provide the joint uncertainty of the conditional
#' modes (random effects) together with the fixed effects, so a delta-method
#' confidence interval for these estimates - the way \code{summary()} does
#' for population-level estimates - isn't available (see
#' \code{vignette("mixed-models")} for a longer discussion). Instead, this
#' function simulates \code{nsim} new response vectors from the fitted
#' model (via \code{stats::simulate()}), refits the model to each, and
#' recomputes \code{ranef_estimates()} each time. The resulting empirical
#' distribution is used to form percentile confidence intervals wherever
#' \code{ranef_ci = TRUE} is used.
#'
#' Because this involves refitting the model \code{nsim} times, it can be
#' slow for larger datasets or models - this is why it's a separate,
#' explicit step rather than happening automatically: run it once, and
#' reuse the result for as many tables/plots as you like.
#'
#' Any replicate whose refit fails to converge (either raising an error, or
#' a warning - e.g. a singular fit) is dropped; at least one replicate must
#' succeed.
#'
#' @param object A \code{cglmm} object with at least one random effect.
#' @param nsim The number of bootstrap replicates. Defaults to \code{500}.
#' @param ranef_group A \code{character} naming the random-effect grouping
#' variable to bootstrap. Required if \code{object} has more than one
#' random-effect grouping variable; otherwise inferred automatically.
#' @param quietly A \code{logical}. If \code{FALSE}, prints progress
#' messages. Defaults to \code{TRUE}.
#' @param ... Additional arguments passed to \code{stats::simulate()}.
#'
#' @return The \code{object}, with a \code{ranef_boots} element added,
#' containing the bootstrap replicate estimates and metadata. Intended to
#' be reassigned over the original object, e.g. \code{model <-
#' add_ranef_boots(model)}.
#'
#' @examples
#' \donttest{
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
#' mixed_mod <- add_ranef_boots(mixed_mod, nsim = 20)
#' ranef_estimates(mixed_mod, ranef_ci = TRUE)
#' }
#' @export
add_ranef_boots <- function(
  object,
  nsim = 500,
  ranef_group = NULL,
  quietly = TRUE,
  ...
) {
  assertthat::assert_that(
    inherits(object, "cglmm"),
    msg = "'object' must be of class 'cglmm'"
  )
  assertthat::assert_that(
    !all(is.na(object$ranef_groups)),
    msg = "'object' does not have any random effects."
  )
  assertthat::assert_that(
    assertthat::is.count(nsim),
    msg = "'nsim' must be a positive integer"
  )
  assertthat::assert_that(
    is.logical(quietly),
    msg = "'quietly' must be a logical argument, either TRUE or FALSE"
  )

  ranef_group <- resolve_ranef_group(object, ranef_group)

  sim_responses <- stats::simulate(object$fit, nsim = nsim, ...)
  response_var <- object$response_var

  boot_formula <- object$fit$modelInfo$allForm$formula
  boot_family <- object$fit$modelInfo$family
  boot_disp <- object$fit$modelInfo$allForm$dispformula
  boot_zi <- object$fit$modelInfo$allForm$ziformula

  refit_one <- function(sim_data) {
    ok <- TRUE
    fit <- tryCatch(
      withCallingHandlers(
        glmmTMB::glmmTMB(
          formula = boot_formula,
          data = sim_data,
          family = boot_family,
          dispformula = boot_disp,
          ziformula = boot_zi
        ),
        warning = function(w) {
          ok <<- FALSE
          invokeRestart("muffleWarning")
        }
      ),
      error = function(e) NULL
    )
    if (is.null(fit) || !ok) {
      return(NULL)
    }
    fit
  }

  boot_estimates <- vector("list", nsim)
  n_success <- 0
  progress_every <- max(1, round(nsim / 10))

  for (i in seq_len(nsim)) {
    sim_data <- object$newdata
    sim_data[[response_var]] <- sim_responses[[i]]

    refit <- refit_one(sim_data)
    if (is.null(refit)) {
      next
    }

    est <- tryCatch(
      ranef_estimates_core(
        fit = refit,
        raw_coefs = glmmTMB::fixef(refit)$cond,
        components = object$components,
        vec_rrr = object$vec_rrr,
        vec_sss = object$vec_sss,
        n_components = object$n_components,
        newdata = object$newdata,
        ranef_group = ranef_group
      ),
      error = function(e) NULL
    )
    if (is.null(est)) {
      next
    }

    n_success <- n_success + 1
    est$.rep <- n_success
    boot_estimates[[n_success]] <- est

    if (!quietly && i %% progress_every == 0) {
      message(sprintf("add_ranef_boots(): %d/%d replicates attempted", i, nsim))
    }
  }

  assertthat::assert_that(
    n_success > 0,
    msg = paste(
      "All bootstrap refits failed to converge; could not estimate",
      "random-effect uncertainty. Try increasing 'nsim', or check that",
      "the original model converges cleanly."
    )
  )

  if (!quietly) {
    message(sprintf(
      "add_ranef_boots(): %d/%d replicates converged and were used.",
      n_success,
      nsim
    ))
  }

  object$ranef_boots <- list(
    estimates = do.call(rbind, boot_estimates[seq_len(n_success)]),
    ranef_group = ranef_group,
    nsim = nsim,
    n_success = n_success
  )

  object
}

#' Fetch bootstrap replicate estimates for a grouping variable, erroring
#' informatively (with example code) if unavailable.
#' @noRd
get_ranef_boots <- function(object, ranef_group) {
  if (is.null(object$ranef_boots)) {
    stop(
      "No bootstrapped random-effect estimates found on 'object'. ",
      "Run `add_ranef_boots()` first, e.g.:\n\n",
      "  model <- add_ranef_boots(model)\n\n",
      "then pass `ranef_ci = TRUE` again.",
      call. = FALSE
    )
  }
  if (!identical(object$ranef_boots$ranef_group, ranef_group)) {
    stop(
      "'object' has bootstrapped estimates for random-effect grouping ",
      "variable '",
      object$ranef_boots$ranef_group,
      "', not '",
      ranef_group,
      "'. Run `add_ranef_boots()` again for this grouping variable, e.g.:\n\n",
      "  model <- add_ranef_boots(model, ranef_group = \"",
      ranef_group,
      "\")\n\n",
      "then pass `ranef_ci = TRUE` again.",
      call. = FALSE
    )
  }
  object$ranef_boots
}
