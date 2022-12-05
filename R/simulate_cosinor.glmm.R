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
simulate_cosinor.glmm <- function (n,
                                   amp = 5,
                                   acro = 10,
                                   mesor = 0,
                                   beta.amp = 2,
                                   beta.acr = 0,
                                   beta.mesor = 0,
                                   period = 12,
                                   dist = c("poisson","binomial","gamma","gaussian")) {
  ttt <- runif(n,min = 0, period)
  cLevel=0.95


  #Simulates Poisson circadian GLM
  get_dataset_poisson <- function(amp,acr,mesor) {
    B <- amp*cos(acr)
    G <- -amp*sin(acr)
    rrr <- cos(2*pi*(ttt)/period)
    sss <- sin(2*pi*(ttt)/period)

    lambda <- exp(mesor+B*rrr+G*sss)
    nsize <- length(ttt)
    y <- rpois(nsize,lambda=lambda)
    df <- data.frame(y,rrr,sss,ttt)
    return(df)
  }

  #Simulates binomial circadian data
  get_dataset_bin <- function(amp,acr,mesor) {
    B <- amp*cos(acr)
    G <- -amp*sin(acr)
    rrr <- cos(2*pi*(ttt)/period)
    sss <- sin(2*pi*(ttt)/period)

    PSuccess <- exp(mesor+B*rrr+G*sss)/(1+exp(M+B*rrr+G*sss))
    nsize <- length(ttt)
    y <- rbinom(nsize, 1, PSuccess)
    df <- data.frame(y,rrr,sss,ttt)
    return(df)
  }

  #Simulates gamma circadian data
  get_dataset_gamma <- function(amp,acr,mesor) {
    B <- amp*cos(acr)
    G <- -amp*sin(acr)
    rrr <- cos(2*pi*(ttt)/period)
    sss <- sin(2*pi*(ttt)/period)

    alpha <- 5
    beta <- alpha/exp(mesor+B*rrr+G*sss)
    nsize <- length(ttt)
    y <-rgamma(nsize,shap=alpha, rate = beta)
    df <- data.frame(y,rrr,sss,ttt)
    return(df)
  }

  #Simulates normal circadian data
  get_dataset_gaussian <- function(amp,acr,mesor) {
    B <- amp*cos(acr)
    G <- -amp*sin(acr)
    rrr <- cos(2*pi*(ttt)/period)
    sss <- sin(2*pi*(ttt)/period)


    lambda <- (mesor+B*rrr+G*sss)
    nsize <- length(ttt)

    y <-  rnorm(nsize,lambda,1)
    df <- data.frame(y,rrr,sss,ttt)
    return(df)

  }

  if (dist=="poisson") {
    get_dataset <- get_dataset_poisson
  }


  if (dist=="binomial") {
    get_dataset <- get_dataset_bin
  }


  if (dist=="gamma") {
    get_dataset <- get_dataset_gamma
  }

  if (dist=="gaussian") {
    get_dataset <- get_dataset_gaussian
  }



  data_A <- get_dataset(amp,acro,mesor)
  data_B <- get_dataset(beta.amp,beta.acr,beta.mesor)
  data_A$group <- "A"
  data_B$group <- "B"
  df <- rbind(data_A, data_B)
  df$group = as.numeric(as.factor(df$group))-1

  colnames(df) <- c("Y","rrr","sss","times","group")
  return(df)
}
