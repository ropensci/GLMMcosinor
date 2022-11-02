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
#'
#' @export
#'
simulate_cosinor <- function(n,
                             mesor = 5,
                             amp = 10,
                             acro = 0,
                             beta.mesor = 2,
                             beta.amp = 0,
                             beta.acro = 0,
                             period = 12,
                             dist = c("gaussian", "poisson")) {

  ttt <- runif(n, 0, period)
  X <- rbinom(n, 1, 0.3)
  rrr <- cos(2 * pi * ttt/period)
  sss <- sin(2 * pi * ttt/period)

  B1 <- amp*cos(acro)
  G1 <- -amp*sin(acro)

  B2 <- (amp + beta.amp)*cos(acro + beta.acro)
  G2 <- -(amp + beta.amp)*sin(acro + beta.acro)

  Y <- mesor + beta.mesor * X + ifelse(X == 0, B1, B2)*rrr + ifelse(X == 0, G1, G2)*sss
  if (dist[1] == "gaussian") {
    Y <- Y + stats::rnorm(n, sd = .1)
  } else if (dist[1] == "poisson") {
    # Y <- exp(Y)
    Y <- rpois(n=length(Y), lambda = Y)
  }
  data.frame(X = X, Y = Y, time = ttt)
}
