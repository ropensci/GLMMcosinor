#' Print a brief summary of the \code{cosinor.glmm} model.
#'
#' @param x A \code{cosinor.glmm} object.
#' @param digits Controls the number of digits displayed in the summary output.
#' @param ... Additional, ignored arguments.
#'
#' @srrstats {RE4.17} *Model objects returned by Regression Software should implement or appropriately extend a default `print` method which provides an on-screen summary of model (input) parameters and (output) coefficients.*
#' @srrstats {G1.4} *Software should use [`roxygen2`](https://roxygen2.r-lib.org/) to document all functions.*
#'
#' @return \code{print(x)} returns \code{x} invisibly.
#'
#' @export
print.cosinor.glmm <- function(x, digits = getOption("digits"), ...) {
  # cat("Call: \n")
  # print(x$Call)
  coef_list <- as.data.frame(x$raw_coefficients)
  colnames(coef_list) <- "Estimate"
  cat("\n Conditional Model \n")
  cat("\n Raw formula: \n")
  cat(deparse(x$formula), "\n")
  cat("\n Raw Coefficients: \n")
  stats::printCoefmat(coef_list, digits = digits)
  # print(round(x$raw_coefficients, digits = digits))
  cat("\n Transformed Coefficients: \n")
  t.x <- x$coefficients
  if (x$group_check == TRUE) {
    names(t.x) <- update_covnames(names(t.x), group_stats = x$group_stats)
  }
  coef_list <- as.data.frame(t.x)
  colnames(coef_list) <- "Estimate"
  stats::printCoefmat(coef_list, digits = digits)
  # print(round(t.x, digits = digits))

  if (x$dispformula_check) {
    coef_list <- as.data.frame(x$disp_list$raw_coefficients)
    colnames(coef_list) <- "Estimate"
    cat("\n***********************\n")
    cat("\n Dispersion Model \n")
    cat("\n Raw  Formula: \n")
    cat(deparse(x$disp_list$formula_disp), "\n")
    cat("\n Raw  Coefficients: \n")
    # stats::printCoefmat(as.data.frame(x$disp_list$raw_coefficients, col.names = c("Parameter","Estimate")), digits = digits)
    stats::printCoefmat(coef_list, digits = digits)
    # print(round(x$disp_list$raw_coefficients_disp, digits = digits))
    cat("\n Transformed  Coefficients: \n")
    td.x <- x$disp_list$coefficients_disp
    if (x$disp_list$group_check_disp == TRUE) {
      names(td.x) <- update_covnames(names(td.x), group_stats = x$disp_list$group_stats_disp)
    }
    coef_list <- as.data.frame(td.x)
    colnames(coef_list) <- "Estimate"
    stats::printCoefmat(coef_list, digits = digits)
    # print(round(td.x, digits = digits))
  }

  if (x$ziformula_check) {
    coef_list <- as.data.frame(x$zi_list$raw_coefficients)
    colnames(coef_list) <- "Estimate"
    cat("\n***********************\n")
    cat("\n Zero-Inflation Model \n")
    cat("\n Raw  Formula: \n")
    cat(deparse(x$zi_list$formula_zi), "\n")
    cat("\n Raw  Coefficients: \n")
    stats::printCoefmat(coef_list, digits = digits)
    # stats::printCoefmat(as.data.frame(x$zi_list$raw_coefficients, col.names = c("Parameter","Estimate")), digits = digits)
    # print(round(x$zi_list$raw_coefficients_zi, digits = digits))
    cat("\n Transformed  Coefficients: \n")
    tzi.x <- x$zi_list$coefficients_zi
    if (x$zi_list$group_check_zi == TRUE) {
      names(tzi.x) <- update_covnames(names(tzi.x), group_stats = x$zi_list$group_stats_zi)
    }
    coef_list <- as.data.frame(tzi.x)
    colnames(coef_list) <- "Estimate"
    stats::printCoefmat(coef_list, digits = digits)
    # print(round(tzi.x, digits = digits))
  }
  invisible(x)
}
