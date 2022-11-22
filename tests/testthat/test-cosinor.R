test_that("poisson model work", {
  n <- 100
  times <- runif(n,min = 0, max =24)
  tau <- 12
  cLevel=0.95

  TrueAmp=2
  TrueAcr=1
  TrueMesor=4

  totalreps=500

  get_dataset <- function(amp,acr,mesor) {
    B <- amp*cos(acr)
    G <- -amp*sin(acr)
    x <- cos(2*pi*(times)/tau)
    z <- sin(2*pi*(times)/tau)
    M <- mesor
    lambda <- exp(M+B*x+G*z)
    nsize <- length(times)
    y <- rpois(nsize,lambda=lambda)
    df <- data.frame(y,x,z,times)
    return(df)
  }

  actualdata <- get_dataset(TrueAmp,TrueAcr,TrueMesor)
  model_glm <- glm(y ~ x + z, data = actualdata,family=poisson)
  model_glm
  Amp_change = 0.55
  Acr_change = 0.73
  Mesor_change = 0.79

  actualdata2 <- get_dataset(TrueAmp+Amp_change,TrueAcr+Acr_change,TrueMesor+Mesor_change)
  # model2 <- glm(y ~ x + z +x:z, data = actualdata2,family=poisson)

  actualdata$group <- "A"
  actualdata2$group <- "B"
  combined_data <- rbind(actualdata, actualdata2)
  # model_combined <- glm(y ~ group + x:group + z:group, data = combined_data,family=poisson)
  combined_data2 <- combined_data
  combined_data2$group = as.numeric(as.factor(combined_data2$group))-1
  cosinor_glmm_model <- cosinor.glmm(y~time(times) + group + amp.acro(group), period=tau, family=poisson, data=combined_data2)

  group_a_amp <- cosinor_glmm_model$coefficients['amp']
  expect_true((TrueAmp*0.9 < group_a_amp) & (group_a_amp < TrueAmp*1.1))

  group_a_acr <- cosinor_glmm_model$coefficients['acr']
  expect_true((TrueAcr*0.9 < group_a_acr) & (group_a_acr < TrueAcr*1.1))

  group_a_mes <- cosinor_glmm_model$coefficients[1]
  expect_true((TrueMesor*0.9 < group_a_mes) & (group_a_mes < TrueMesor*1.1))
})

