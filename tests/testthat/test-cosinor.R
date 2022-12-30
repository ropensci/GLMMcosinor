test_model <- function(family_name) {
  test_that("model work", {
    n <- 1000
    times <- runif(n, min = 0, max = 24)
    tau <- 12
    cLevel <- 0.95


    TrueAmp <- runif(1, 0, 5)
    TrueAcr <- runif(1, -pi, pi)
    TrueMesor <- runif(1, 0, 5)
    Amp_change <- runif(1, -TrueAmp, 5)
    Acr_change <- runif(1, -TrueAcr, pi)
    Mesor_change <- runif(1, -TrueMesor, 2)



    totalreps <- 500


    # Simulates Poisson circadian GLM
    get_dataset_poisson <- function(amp, acr, mesor) {
      B <- amp * cos(acr)
      G <- -amp * sin(acr)
      x <- cos(2 * pi * (times) / tau)
      z <- sin(2 * pi * (times) / tau)
      M <- mesor
      lambda <- exp(M + B * x + G * z)
      nsize <- length(times)
      y <- rpois(nsize, lambda = lambda)
      df <- data.frame(y, x, z, times)
      return(df)
    }

    # Simulates binomial circadian data
    get_dataset_bin <- function(amp, acr, mesor) {
      B <- amp * cos(acr)
      G <- -amp * sin(acr)
      x <- cos(2 * pi * (times) / tau)
      z <- sin(2 * pi * (times) / tau)
      M <- mesor

      PSuccess <- exp(M + B * x + G * z) / (1 + exp(M + B * x + G * z))
      nsize <- length(times)
      y <- rbinom(nsize, 1, PSuccess)
      df <- data.frame(y, x, z, times)
      return(df)
    }

    # Simulates gamma circadian data
    get_dataset_gamma <- function(amp, acr, mesor) {
      B <- amp * cos(acr)
      G <- -amp * sin(acr)
      x <- cos(2 * pi * (times) / tau)
      z <- sin(2 * pi * (times) / tau)
      M <- mesor

      alpha <- 5
      beta <- alpha / exp(M + B * x + G * z)
      nsize <- length(times)
      y <- rgamma(nsize, shap = alpha, rate = beta)
      df <- data.frame(y, x, z, times)
      return(df)
    }

    # Simulates normal circadian data
    get_dataset_gaussian <- function(amp, acr, mesor) {
      B <- amp * cos(acr)
      G <- -amp * sin(acr)
      x <- cos(2 * pi * (times) / tau)
      z <- sin(2 * pi * (times) / tau)
      M <- mesor


      lambda <- (M + B * x + G * z)
      nsize <- length(times)

      y <- rnorm(nsize, lambda, 1)
      df <- data.frame(y, x, z, times)
      return(df)
    }

    if (family_name == "poisson") {
      get_dataset <- get_dataset_poisson
    }


    if (family_name == "binomial") {
      get_dataset <- get_dataset_bin
    }


    if (family_name == "gamma") {
      get_dataset <- get_dataset_gamma
    }

    if (family_name == "gaussian") {
      get_dataset <- get_dataset_gaussian
    }



    actualdata <- get_dataset(TrueAmp, TrueAcr, TrueMesor)
    actualdata2 <- get_dataset(TrueAmp + Amp_change, TrueAcr + Acr_change, TrueMesor + Mesor_change)
    actualdata$group <- "A"
    actualdata2$group <- "B"
    combined_data <- rbind(actualdata, actualdata2)
    combined_data2 <- combined_data
    combined_data2$group <- as.numeric(as.factor(combined_data2$group)) - 1


    if (family_name == "poisson") {
      cosinor_glmm_model <- cosinor.glmm(y ~ time(times) + group + amp.acro(group), period = tau, family = poisson, data = combined_data2)
    }


    if (family_name == "binomial") {
      cosinor_glmm_model <- cosinor.glmm(y ~ time(times) + group + amp.acro(group), period = tau, family = binomial, data = combined_data2)
    }


    if (family_name == "gamma") {
      cosinor_glmm_model <- cosinor.glmm(y ~ time(times) + group + amp.acro(group), period = tau, family = Gamma(link = "log"), data = combined_data2)
    }

    if (family_name == "gaussian") {
      cosinor_glmm_model <- cosinor.glmm(y ~ time(times) + group + amp.acro(group), period = tau, family = gaussian, data = combined_data2)
    }

    group_a_amp <- cosinor_glmm_model$coefficients["amp"]
    expect_true((TrueAmp * 0.9 < group_a_amp) & (group_a_amp < TrueAmp * 1.1))

    group_a_acr <- cosinor_glmm_model$coefficients["acr"]
    expect_true((abs(TrueAcr * 0.9) < abs(group_a_acr)) & (abs(group_a_acr) < abs(TrueAcr * 1.1)))

    group_a_mes <- cosinor_glmm_model$coefficients[1]
    expect_true((TrueMesor * 0.9 < group_a_mes) & (group_a_mes < TrueMesor * 1.1))
  })
}
