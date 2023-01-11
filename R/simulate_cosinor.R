#' Simulate data from a cosinor model
#'
#' This function simulates data from a cosinor model with a single covariate,
#' where the time scale is month, and optionally
#' allows for single covariate effects on the mean,
#' amplitude, and acrophase.
#'
#' @param n Sample size
#' @param beta.mean Effect on the mean (intercept)
#' @param beta.amp Effect on the amplitude
#' @param beta.acro Effect on the acrophase
#' @srrstatsTODO {G5.1} *Data sets created within, and used to test, a package should be exported (or otherwise made generally available) so that users can confirm tests and run examples.*
#'
#' @export
#'
simulate_cosinor <- function(n,
                             mesor = 1,
                             amp = 2,
                             acro = 1.2,
                             beta.mesor = 0.4,
                             beta.amp = 0.5,
                             beta.acro = 0.2,
                             period = 12,
                             dist = c("poisson")) {
  ttt <- runif(n, min = 0, period)
  cLevel <- 0.95


  # Simulates Poisson circadian GLM
  get_dataset_poisson <- function(amp, acr, mesor) {
    B <- amp * cos(acr)
    G <- -amp * sin(acr)
    rrr <- cos(2 * pi * (ttt) / period)
    sss <- sin(2 * pi * (ttt) / period)

    lambda <- exp(mesor + B * rrr + G * sss)
    nsize <- length(ttt)
    y <- rpois(nsize, lambda = lambda)
    df <- data.frame(y, rrr, sss, ttt)
    return(df)
  }

  # Simulates binomial circadian data
  get_dataset_bin <- function(amp, acr, mesor) {
    B <- amp * cos(acr)
    G <- -amp * sin(acr)
    rrr <- cos(2 * pi * (ttt) / period)
    sss <- sin(2 * pi * (ttt) / period)

    PSuccess <- exp(mesor + B * rrr + G * sss) / (1 + exp(M + B * rrr + G * sss))
    nsize <- length(ttt)
    y <- rbinom(nsize, 1, PSuccess)
    df <- data.frame(y, rrr, sss, ttt)
    return(df)
  }

  # Simulates gamma circadian data
  get_dataset_gamma <- function(amp, acr, mesor) {
    B <- amp * cos(acr)
    G <- -amp * sin(acr)
    rrr <- cos(2 * pi * (ttt) / period)
    sss <- sin(2 * pi * (ttt) / period)

    alpha <- 5
    beta <- alpha / exp(mesor + B * rrr + G * sss)
    nsize <- length(ttt)
    y <- stats::rgamma(nsize, shap = alpha, rate = beta)
    df <- data.frame(y, rrr, sss, ttt)
    return(df)
  }

  # Simulates normal circadian data
  get_dataset_gaussian <- function(amp, acr, mesor) {
    B <- amp * cos(acr)
    G <- -amp * sin(acr)
    rrr <- cos(2 * pi * (ttt) / period)
    sss <- sin(2 * pi * (ttt) / period)


    lambda <- (mesor + B * rrr + G * sss)
    nsize <- length(ttt)

    y <- rnorm(nsize, lambda, 1)
    df <- data.frame(y, rrr, sss, ttt)
    return(df)
  }

  get_dataset_combined_guassian <- function(amp, acr, mesor) {
    B1 <- amp * cos(acr)
    G1 <- -amp * sin(acr)
    rrr1 <- cos(2 * pi * (ttt) / 12)
    sss1 <- sin(2 * pi * (ttt) / 12)

    B2 <- (amp + 3) * cos(acr - 1)
    G2 <- -(amp + 3) * sin(acr - 1)
    rrr2 <- cos(2 * pi * (ttt) / 8)
    sss2 <- sin(2 * pi * (ttt) / 8)

    lambda1 <- (mesor + B1 * rrr1 + G1 * sss1)
    lambda2 <- (B2 * rrr2 + G2 * sss2)
    nsize <- length(ttt)
    # browser()
    # y <-  rnorm(nsize,lambda1,1)+rnorm(nsize,lambda2,1)
    y <- rnorm(nsize, lambda1 + lambda2, 1)
    df <- data.frame(y, rrr1, sss1, ttt)
    return(df)
  }

  if (dist == "poisson") {
    get_dataset <- get_dataset_poisson
  }
  if (dist == "binomial") {
    get_dataset <- get_dataset_bin
  }
  if (dist == "gamma") {
    get_dataset <- get_dataset_gamma
  }
  if (dist == "gaussian") {
    get_dataset <- get_dataset_gaussian
  }

  if (dist == "2_component") {
    get_dataset <- get_dataset_combined_guassian
  }

  data_A <- get_dataset(amp, acro, mesor)
  data_B <- get_dataset(beta.amp, beta.acro, beta.mesor)
  data_A$group <- "A"
  data_B$group <- "B"
  df <- rbind(data_A, data_B)
  df$group <- as.numeric(as.factor(df$group)) - 1

  colnames(df) <- c("Y", "x", "z", "times", "group")

  return(df)
}
