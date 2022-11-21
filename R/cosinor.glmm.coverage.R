library(matlib)
library(car)
library(tidyverse)

n <- 100
times <- runif(n,min = 0, max =24)
tau <- 12
cLevel=0.95

TrueAmp=2
TrueAcr=1
TrueMesor=4

Amp_change = 0.55
Acr_change = 0.73
Mesor_change = 0.79

totalreps=1000

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

combine_dataset <- function (TrueAmp,TrueAcr,TrueMesor, Amp_change, Acr_change, Mesor_change) {
  actualdata <- get_dataset(TrueAmp,TrueAcr,TrueMesor)
  actualdata2 <- get_dataset(TrueAmp+Amp_change,TrueAcr+Acr_change,TrueMesor+Mesor_change)
  actualdata$group <- "A"
  actualdata2$group <- "B"
  combined_data <- rbind(actualdata, actualdata2)


  combined_data2 <- combined_data
  combined_data2$group = as.numeric(as.factor(combined_data2$group))-1
  return(combined_data2)
}

combined_data_analysis = combine_dataset(TrueAmp,TrueAcr,TrueMesor, Amp_change, Acr_change, Mesor_change)

assess_glmm <- function(combined_data_analysis) {
  cosinor_glmm_model <- cosinor.glmm(y~time(times) + group + amp.acro(group), period=tau, family=poisson, data=combined_data_analysis)

  model.glmm_coverage <- summary.cosinor.glmm(cosinor_glmm_model)

  lower_CI <- model.glmm_coverage$transformed.table$lower.CI
  names(lower_CI) <- c('msr.A', 'msr.B_change','amp.A','amp.B_change','acr.A','acr.B_change')
  upper_CI <- model.glmm_coverage$transformed.table$upper.CI
  names(upper_CI) <- c('msr.A', 'msr.B_change','amp.A','amp.B_change','acr.A','acr.B_change')


  c(
    "ll_acr" = unname(lower_CI['acr.A']),
    "ul_acr" = unname(upper_CI['acr.A']),
    "ll_amp" = unname(lower_CI['amp.A']),
    "ul_amp" = unname(upper_CI['amp.A']),
    "ll_msr" = unname(lower_CI['msr.A']),
    "ul_msr" = unname(upper_CI['msr.A'])
)
}

f <- function(...) assess_glmm(combine_dataset(TrueAmp,TrueAcr,TrueMesor, Amp_change, Acr_change, Mesor_change))

res <- purrr::map_dfr(1:10, f)

library(parallel)
cl <- makeCluster(detectCores())


clusterEvalQ(cl, {
  library(matlib)
  library(car)
  library(tidyverse)
}
)

clusterExport(cl, c('Amp_change','Acr_change','Mesor_change','get_varnames','cosinor.glmm','update_covnames','combine_dataset','summary.cosinor.glmm',"assess_glmm", "get_dataset", "times", "tau", "cLevel","TrueAmp","TrueAcr","TrueMesor"))

res <- parLapply(
  cl,
  1:totalreps,
  f
)

res <- do.call("rbind", res)
res <- as.data.frame(res)

res %>%
  dplyr::summarize(
    msr_coverage = simMetric::coverage(TrueMesor, ll=ll_msr, ul=ul_msr, get="coverage"),
    acr_coverage = simMetric::coverage(TrueAcr, ll=ll_acr, ul=ul_acr, get="coverage"),
    amp_coverage = simMetric::coverage(TrueAmp, ll=ll_amp, ul=ul_amp, get="coverage")
  )
