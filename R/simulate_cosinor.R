#' Simulate data from a cosinor model
#'
#' This function simulates data from a cosinor model with a single covariate,
#' where the time scale is month, and optionally
#' allows for single covariate effects on the mean,
#' amplitude, and acrophase.
#'
#' @param n Sample size. Integer greater than 0.
#' @param mesor Mesor parameter for group = 0.If multiple components, specify the parameters for each component as a vector. E.g: mesor = c(1,2) for two components.
#' @param amp Amplitude parameter term for group = 0.If multiple components, specify the parameters for each component as a vector. E.g: amp = c(1,2) for two components.
#' @param acro Acrophase parameter for group = 0. In units of radians.If multiple components, specify the parameters for each component as a vector. E.g: acr = c(1,2) for two components.
#' @param period The period of data. If multiple components, specify the parameters for each component as a vector. E.g: period = c(12,6) for two components.
#' @param n_components The number of components in the model. This must match the length of the parameter inputs.
#' @param beta.group A logical argument. TRUE if you want to simulate second group dataset. If FALSE, beta. arguments will be ignored.
#' @param beta.acro  Mesor parameter for group = 1.If multiple components, specify the parameters for each component as a vector. E.g: mesor = c(1,2) for two components.
#' @param beta.mesor Amplitude parameter term for group = 1.If multiple components, specify the parameters for each component as a vector. E.g: amp = c(1,2) for two components.
#' @param beta.amp Acrophase parameter for group = 1. In units of radians.If multiple components, specify the parameters for each component as a vector. E.g: acr = c(1,2) for two components.
#' @param family family of simulated dataset expressed as a string. Can take values: "poisson", "binomial", "gamma", "gaussian".
#' @param n_period is the number of periods that are simulated.
#' @param ... Extra arguments, such as alpha parameter for the gamma simulation, or sd (standard deviation) for gaussian simulation.
#'
#' @return Returns simulated data in a `data.frame`.
#'
#' @examples
#' simulate_cosinor(
#'   n = 100,
#'   mesor = 1,
#'   amp = 1,
#'   acro = 1,
#'   period = 24,
#'   family = "gaussian"
#' )
#' @srrstats {G5.1} *Data sets created within, and used to test, a package should be exported (or otherwise made generally available) so that users can confirm tests and run examples.*
#'
#' @export
#'
simulate_cosinor <- function(n,
                             mesor,
                             amp,
                             acro,
                             period = 24,
                             n_components,
                             beta.group = FALSE,
                             beta.mesor,
                             beta.amp,
                             beta.acro,
                             n_period = 1,
                             family = c("gaussian", "poisson", "binomial", "gamma"),
                             ...) {
  # attempt to infer n_components if missing
  if (missing(n_components)) {
    if (length(amp) == length(acro)) {
      n_components <- length(amp)
    }
  }

  assertthat::assert_that(
    assertthat::is.flag(beta.group),
    msg = "beta.group argument must be logical"
  )

  if (!beta.group & !missing(beta.mesor) & !missing(beta.amp) & !missing(beta.acro)) {
    beta.group <- TRUE
    message("all betas were present but beta.group was FALSE. beta.group has been changed to be TRUE.")
  }

  .validate_simulate_cosinor_inputs(
    n,
    mesor,
    amp,
    acro,
    period,
    n_components,
    beta.group,
    beta.mesor,
    beta.amp,
    beta.acro,
    n_period,
    family
  )

  family <- match.arg(family)

  # generate a time vector
  ttt <- stats::runif(n, min = 0, n_period * max(period))

  # create dataset for only one group if beta.group = FALSE
  if (!beta.group) {
    df <- .get_dataset(
      family = family,
      amp = amp,
      acro = acro,
      ttt = ttt,
      mesor = mesor,
      n_components = n_components,
      period = period,
      ...
    )
  }

  # create dataset for two groups if beta.group = TRUE
  if (beta.group) {
    data_A <- .get_dataset(
      family = family,
      amp = amp,
      acro = acro,
      ttt = ttt,
      mesor = mesor,
      n_components = n_components,
      period = period,
      ...
    )

    data_B <- .get_dataset(
      family = family,
      amp = beta.amp,
      acro = beta.acro,
      ttt = ttt,
      mesor = beta.mesor,
      n_components = n_components,
      period = period,
      ...
    )
    data_A$group <- 0
    data_B$group <- 1
    df <- rbind(data_A, data_B)
  }

  return(df)
}

#' Validate args passed to \code{simulate_cosinor()}.
#'
#' @param n Arg from \code{simulate_cosinor()}.
#' @param mesor Arg from \code{simulate_cosinor()}.
#' @param amp Arg from \code{simulate_cosinor()}.
#' @param acro Arg from \code{simulate_cosinor()}.
#' @param period Arg from \code{simulate_cosinor()}.
#' @param n_components Arg from \code{simulate_cosinor()}.
#' @param beta.group Arg from \code{simulate_cosinor()}.
#' @param beta.mesor Arg from \code{simulate_cosinor()}.
#' @param beta.amp Arg from \code{simulate_cosinor()}.
#' @param beta.acro Arg from \code{simulate_cosinor()}.
#' @param n_period Arg from \code{simulate_cosinor()}.
#' @param family Arg from \code{simulate_cosinor()}.
#'
#' @return \code{NULL}
#'
#' @noRd
.validate_simulate_cosinor_inputs <- function(n,
                                              mesor,
                                              amp,
                                              acro,
                                              period,
                                              n_components,
                                              beta.group,
                                              beta.mesor,
                                              beta.amp,
                                              beta.acro,
                                              n_period,
                                              family) {
  # validating inputs
  assertthat::assert_that(
    assertthat::is.count(n),
    msg = "n must be an integer greater than 0"
  )

  assertthat::assert_that(
    assertthat::is.count(n_components),
    msg = "n_components must be an integer greater than 0"
  )

  assertthat::assert_that(
    is.numeric(mesor) & length(mesor) == 1,
    msg = "mesor must a single number"
  )

  assertthat::assert_that(
    is.numeric(amp) & length(amp) == n_components,
    msg = "amp must be a vector containing numbers, with length equal to n_components"
  )

  assertthat::assert_that(
    is.numeric(acro) & length(acro) == n_components,
    msg = "acro must be a vector containing numbers, with length equal to n_components"
  )

  assertthat::assert_that(
    is.numeric(period) & length(period) == n_components,
    msg = "period must be a vector containing numbers, with length equal to n_components"
  )

  if (beta.group) {
    assertthat::assert_that(
      is.numeric(beta.mesor) & length(beta.mesor) == 1,
      msg = "beta.mesor must be a single number"
    )

    assertthat::assert_that(
      is.numeric(beta.amp) & length(beta.amp) == n_components,
      msg = "beta.amp must be a vector containing numbers, with length equal to n_components"
    )

    assertthat::assert_that(
      is.numeric(beta.acro) & length(beta.acro) == n_components,
      msg = "beta.acro must be a vector containing numbers, with length equal to n_components"
    )
  }
}


#' Simulate a dataset from a cosinor model.
#'
#' @param family Arg from \code{simulate_cosinor()}.
#' @param amp Arg from \code{simulate_cosinor()}.
#' @param acro Arg from \code{simulate_cosinor()}.
#' @param ttt Vector of time for which to sample points.
#' @param mesor Arg from \code{simulate_cosinor()}.
#' @param n_components Arg from \code{simulate_cosinor()}.
#' @param period Arg from \code{simulate_cosinor()}.
#' @param sd Standard deviation (used when \code{family = "gaussian")}.
#' @param alpha Used when sampling from gamma distribution.
#' @param ... Optional, unused args.
#'
#' @return A \code{data.frame}.
#' @noRd
.get_dataset <- function(family, amp, acro, ttt, mesor, n_components, period, sd = 1, alpha = 1, ...) {
  d_params <- .get_params(
    amp = amp,
    acro = acro,
    n_components = n_components,
    ttt = ttt,
    period = period
  )

  if (family == "gaussian") {
    d_params$param <- mesor + d_params$param
    d_params$Y <- stats::rnorm(n = length(ttt), mean = d_params$param, sd = sd)
  }
  if (family == "poisson") {
    d_params$param <- exp(mesor + d_params$param)
    d_params$Y <- stats::rpois(n = length(ttt), lambda = d_params$param)
  }
  if (family == "binomial") {
    d_params$param <- exp(mesor + d_params$param) / (1 + exp(mesor + d_params$param))
    d_params$Y <- stats::rbinom(n = length(ttt), size = 1, prob = d_params$param)
  }
  if (family == "gamma") {
    d_params$param <- alpha / exp(mesor + d_params$param)
    d_params$Y <- stats::rgamma(n = length(ttt), shape = alpha, rate = d_params$param)
  }

  with(d_params, data.frame(Y, times = ttt))
}

#' Get cosinor parameters given amplitude and acrophase inputs.
#'
#' @param amp Arg from \code{simulate_cosinor()}.
#' @param acro Arg from \code{simulate_cosinor()}.
#' @param n_components Arg from \code{simulate_cosinor()}.
#' @param ttt vector of time for which to sample points.
#' @param period Arg from \code{simulate_cosinor()}.
#'
#' @return A \code{data.frame}
#' @noRd
.get_params <- function(amp, acro, n_components, ttt, period) {
  param <- 0
  for (i in 1:n_components) {
    B <- amp[i] * cos(acro[i])
    G <- -amp[i] * sin(acro[i])
    rrr <- cos(2 * pi * (ttt) / period[i])
    sss <- sin(2 * pi * (ttt) / period[i])
    param <- param + B * rrr + G * sss
  }
  data.frame(ttt, param)
}
