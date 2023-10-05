#' Vitamin D dataset for cosinor modelling examples.
#'
#' Simulated data set to illustrate the cosinor model. The \code{vit_d} column
#' contains the blood vitamin D levels which vary over time (\code{time}).
#' The rhythm of the vitamind D fluctuations follows a cosine function and can
#' be modelled with a cosinor model. The \code{X} column is a binary covariate
#' representing two groups of patients and is associated with the
#' characteristics of the rhythm. The rhythm has a period of about 12 hours.
#'
#' @format A \code{data.frame} with 3 variables: \code{vit_d}, \code{time}, and
#' \code{X}.

"vitamind"
