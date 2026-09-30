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
#' \code{ranef_ci} is set to a confidence level (e.g. \code{ranef_ci =
#' 0.95}).
#'
#' Because this involves refitting the model (at least) \code{nsim} times,
#' it can be slow for larger datasets or models - this is why it's a
#' separate, explicit step rather than happening automatically: run it
#' once, and reuse the result for as many tables/plots as you like.
#'
#' Any replicate whose refit fails to converge (either raising an error, or
#' a warning - e.g. a singular fit) is dropped and re-tried with a new
#' simulated dataset, so the function keeps simulating until \code{nsim}
#' replicates have actually succeeded (up to \code{max_attempts} total
#' tries, to avoid looping indefinitely if the model rarely converges on
#' simulated data).
#'
#' @param object A \code{cglmm} object with at least one random effect.
#' @param nsim The number of successful bootstrap replicates to obtain.
#' Defaults to \code{500}.
#' @param ranef_group A \code{character} naming the random-effect grouping
#' variable to bootstrap. Required if \code{object} has more than one
#' random-effect grouping variable; otherwise inferred automatically.
#' @param quietly A \code{logical}. If \code{FALSE}, prints progress
#' messages. Defaults to \code{TRUE}.
#' @param max_attempts The maximum number of simulate-and-refit attempts
#' before giving up on reaching \code{nsim} successful replicates. Defaults
#' to \code{10 * nsim}.
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
#' ranef_estimates(mixed_mod, ranef_ci = 0.95)
#' }
#' @export
add_ranef_boots <- function(
  object,
  nsim = 500,
  ranef_group = NULL,
  quietly = TRUE,
  max_attempts = 10 * nsim,
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
  assertthat::assert_that(
    assertthat::is.count(max_attempts) && max_attempts >= nsim,
    msg = "'max_attempts' must be a positive integer, at least 'nsim'"
  )

  ranef_group <- resolve_ranef_group(object, ranef_group)
  response_var <- object$response_var

  # simulate() is called once per attempt below (rather than all nsim at
  # once), since we don't know in advance how many attempts will be needed
  # to reach nsim successes. A 'seed' passed via ... would otherwise reset
  # to the same state on every call (producing identical "replicates"), so
  # it's applied once here instead, and dropped before the per-attempt calls.
  dots <- list(...)
  if (!is.null(dots$seed)) {
    set.seed(dots$seed)
    dots$seed <- NULL
  }

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
  n_attempts <- 0
  progress_every <- max(1, round(nsim / 10))

  while (n_success < nsim && n_attempts < max_attempts) {
    n_attempts <- n_attempts + 1

    sim_data <- object$newdata
    sim_data[[response_var]] <- do.call(
      stats::simulate,
      c(list(object = object$fit, nsim = 1), dots)
    )[[1]]

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

    if (!quietly && n_success %% progress_every == 0) {
      message(sprintf(
        "add_ranef_boots(): %d/%d replicates succeeded (%d attempts so far)",
        n_success,
        nsim,
        n_attempts
      ))
    }
  }

  assertthat::assert_that(
    n_success == nsim,
    msg = paste0(
      "Only ", n_success, "/", nsim, " bootstrap refits converged after ",
      n_attempts, " attempts (max_attempts = ", max_attempts, "); could ",
      "not reach the requested 'nsim'. Try increasing 'max_attempts', or ",
      "check that the original model converges cleanly."
    )
  )

  if (!quietly) {
    message(sprintf(
      "add_ranef_boots(): %d/%d replicates succeeded, from %d attempts.",
      n_success,
      nsim,
      n_attempts
    ))
  }

  object$ranef_boots <- list(
    estimates = do.call(rbind, boot_estimates),
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
      "then pass `ranef_ci = 0.95` (or another confidence level) again.",
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
      "then pass `ranef_ci = 0.95` (or another confidence level) again.",
      call. = FALSE
    )
  }
  object$ranef_boots
}
