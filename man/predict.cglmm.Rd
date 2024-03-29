% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/predict.R
\name{predict.cglmm}
\alias{predict.cglmm}
\title{Predict from a cosinor model}
\usage{
\method{predict}{cglmm}(object, newdata, ...)
}
\arguments{
\item{object}{An object of class \code{cglmm}.}

\item{newdata}{Optional new data.}

\item{...}{other arguments passed to \code{glmmTMB:::predict.glmmTMB}.}
}
\value{
Returns predicted values from the cosinor model.
}
\description{
Given a time variable and optional covariates, generate predicted values from
a cosinor fit. Default prediction is the mean value, optionally can predict
at a given month
}
\examples{

fit <- cglmm(vit_d ~ X + amp_acro(time,
  group = "X",
  n_components = 1,
  period = 12
), data = vitamind)
predict(fit)

}
