#' Simulate data from a cosinor model
#'
#' This function simulates data from a cosinor model with a single covariate,
#' where the time scale is month, and optionally
#' allows for single covariate effects on the mean,
#' amplitude, and acrophase.
#'
#' @param n Sample size. Integer greater than 0.
#' @param mesor Mesor parameter for group = 0.If multiple components, specify the parameters for each component as a vector. E.g: mesor = c(1,2) for two components
#' @param amp Amplitude parameter term for group = 0.If multiple components, specify the parameters for each component as a vector. E.g: amp = c(1,2) for two components
#' @param acro Acrophase parameter for group = 0. In units of radians.If multiple components, specify the parameters for each component as a vector. E.g: acr = c(1,2) for two components
#' @param period The period of data. If multiple components, specify the parameters for each component as a vector. E.g: mesor = c(12,6) for two components
#' @param n_components The number of components in the model. This must match the length of the parameter inputs
#' @param beta.group A logical argument. TRUE if you want to simulate second group dataset. If FALSE, beta. arguments will be ignored
#' @param beta.acro  Mesor parameter for group = 1.If multiple components, specify the parameters for each component as a vector. E.g: mesor = c(1,2) for two components
#' @param beta.mesor Amplitude parameter term for group = 1.If multiple components, specify the parameters for each component as a vector. E.g: amp = c(1,2) for two components
#' @param beta.amp Acrophase parameter for group = 1. In units of radians.If multiple components, specify the parameters for each component as a vector. E.g: acr = c(1,2) for two components
#' @param family family of simulated dataset expressed as a string. Can take values: "poisson", "binomial", "gamma", "gaussian"
#' @param ... Extra arguments, such as alpha parameter for the gamma simulation, or sd (standard deviation) for gaussian simulation
#'
#' @return Returns simulated data in a `data.frame`.
#'
#' @srrstats {G5.1} *Data sets created within, and used to test, a package should be exported (or otherwise made generally available) so that users can confirm tests and run examples.*
#'
#' @export
#'
simulate_cosinor <- function(n,
                             mesor = 1,
                             amp = 2,
                             acro = 1.2,
                             period = 12,
                             n_components = 1,
                             beta.group = TRUE,
                             beta.mesor = 0.4,
                             beta.amp = 0.5,
                             beta.acro = 0.2,
                             family = c("poisson"),
                             ...) {
  # validating inputs
  assertthat::assert_that(n == floor(n) & n > 0,
    msg = "n must be an integer greater than 0"
  )
  assertthat::assert_that(n_components == floor(n_components) & n_components > 0,
    msg = "n_components must be an integer greater than 0"
  )
  assertthat::assert_that(is.numeric(mesor) & length(mesor) == 1,
    msg = "mesor must a single number"
  )
  assertthat::assert_that(is.numeric(amp) & length(amp) == n_components,
    msg = "amp must be a vector containing numbers, with length equal to n_components"
  )
  assertthat::assert_that(is.numeric(acro) & length(acro) == n_components,
    msg = "acro must be a vector containing numbers, with length equal to n_components"
  )
  assertthat::assert_that(is.numeric(beta.mesor) & length(beta.mesor) == 1,
    msg = "beta.mesor must be a single number"
  )
  assertthat::assert_that(is.numeric(beta.amp) & length(beta.amp) == n_components,
    msg = "beta.amp must be a vector containing numbers, with length equal to n_components"
  )
  assertthat::assert_that(is.numeric(beta.acro) & length(beta.acro) == n_components,
    msg = "beta.acro must be a vector containing numbers, with length equal to n_components"
  )
  assertthat::assert_that(is.numeric(period) & length(period) == n_components,
    msg = "period must be a vector containing numbers, with length equal to n_components"
  )
  assertthat::assert_that(family %in% c("poisson", "binomial", "gamma", "gaussian"),
    msg = 'family argument must be a string that matches one of: "poisson", "binomial", "gamma", "gaussian"'
  )
  assertthat::assert_that(is.logical(beta.group),
    msg = "beta.group argument must be logical"
  )



  # generate a time vector
  ttt <- stats::runif(n, min = 0, period)

  # Simulates Poisson circadian GLM
  get_dataset_poisson <- function(amp, acr, mesor, n_components, period) {
    lambda_argument <- 0
    for (i in 1:n_components) {
      B <- amp[i] * cos(acr[i])
      G <- -amp[i] * sin(acr[i])
      rrr <- cos(2 * pi * (ttt) / period[i])
      sss <- sin(2 * pi * (ttt) / period[i])
      lambda_argument <- lambda_argument + B * rrr + G * sss
    }

    lambda <- exp(mesor + lambda_argument)
    nsize <- length(ttt)
    y <- stats::rpois(nsize, lambda = lambda)
    df <- data.frame(y, rrr, sss, ttt)
    return(df)
  }

  # Simulates binomial circadian data
  get_dataset_bin <- function(amp, acr, mesor, n_components, period) {
    PSuccess_argument <- 0
    for (i in 1:n_components) {
      B <- amp[i] * cos(acr[i])
      G <- -amp[i] * sin(acr[i])
      rrr <- cos(2 * pi * (ttt) / period[i])
      sss <- sin(2 * pi * (ttt) / period[i])
      PSuccess_argument <- PSuccess_argument + B * rrr + G * sss
    }

    PSuccess <- exp(mesor + PSuccess_argument) / (1 + exp(mesor + PSuccess_argument))
    nsize <- length(ttt)
    y <- stats::rbinom(nsize, 1, PSuccess)
    df <- data.frame(y, rrr, sss, ttt)
    return(df)
  }

  # Simulates gamma circadian data
  get_dataset_gamma <- function(amp, acr, mesor, n_components, period, alpha) {
    if (missing(alpha)) {
      alpha <- 5
    }
    beta_argument <- 0
    for (i in 1:n_components) {
      B <- amp[i] * cos(acr[i])
      G <- -amp[i] * sin(acr[i])
      rrr <- cos(2 * pi * (ttt) / period[i])
      sss <- sin(2 * pi * (ttt) / period[i])
      beta_argument <- beta_argument + B * rrr + G * sss
    }

    beta <- alpha / exp(mesor + beta_argument)
    nsize <- length(ttt)
    y <- stats::rgamma(nsize, shap = alpha, rate = beta)
    df <- data.frame(y, rrr, sss, ttt)
    return(df)
  }

  # Simulates normal circadian data

  get_dataset_gaussian <- function(amp, acr, mesor, n_components, period, sd) {
    if (missing(sd)) {
      sd <- 1
    }
    lambda_argument <- 0
    for (i in 1:n_components) {
      B <- amp[i] * cos(acr[i])
      G <- -amp[i] * sin(acr[i])
      rrr <- cos(2 * pi * (ttt) / period[i])
      sss <- sin(2 * pi * (ttt) / period[i])
      lambda_argument <- lambda_argument + B * rrr + G * sss
    }

    lambda <- (mesor + lambda_argument)
    nsize <- length(ttt)

    y <- stats::rnorm(nsize, lambda, sd)
    df <- data.frame(y, rrr, sss, ttt)
    return(df)
  }

  # assign 'get_dataset' to the correct function corresponding to family specified by user
  if (family == "poisson") {
    get_dataset <- get_dataset_poisson
  }
  if (family == "binomial") {
    get_dataset <- get_dataset_bin
  }
  if (family == "gamma") {
    get_dataset <- get_dataset_gamma
  }
  if (family == "gaussian") {
    get_dataset <- get_dataset_gaussian
  }

  # create dataset for only one group if beta.group = FALSE
  if (!beta.group) {
    df <- get_dataset(amp, acro, mesor, n_components, period, ...)
    colnames(df) <- c("Y", "x", "z", "times")
  }

  # create dataset for two groups if beta.group = TRUE
  if (beta.group) {
    data_A <- get_dataset(amp, acro, mesor, n_components, period, ...)
    data_B <- get_dataset(amp = beta.amp, acr = beta.acro, mesor = beta.mesor, n_components, period, ...)
    data_A$group <- "A"
    data_B$group <- "B"
    df <- rbind(data_A, data_B)
    df$group <- as.numeric(as.factor(df$group)) - 1
    colnames(df) <- c("Y", "x", "z", "times", "group")
  }

  return(df)
}
