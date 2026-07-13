#' Flatten a left-associative "+" call tree into an ordered list of additive
#' terms.
#'
#' \code{a + b + c} is parsed as \code{(a + b) + c}; this returns
#' \code{list(a, b, c)}.
#'
#' @param expr A language object (call or symbol).
#'
#' @return A \code{list} of language objects.
#' @noRd
.flatten_additive_terms <- function(expr) {
  if (is.call(expr) && identical(expr[[1]], quote(`+`)) && length(expr) == 3) {
    c(.flatten_additive_terms(expr[[2]]), .flatten_additive_terms(expr[[3]]))
  } else {
    list(expr)
  }
}

#' Recursively replace an \code{amp_acroN} symbol with its rrr/sss expansion.
#'
#' Walks the call tree of \code{term} and replaces any symbol node named
#' \code{amp_acroN} with the call \code{{prefix}rrrN + {prefix}sssN}, leaving
#' every other symbol, operator, or grouping untouched.
#'
#' @param term A language object (call or symbol).
#' @param .data_prefix Prefix used for the rrr/sss column names (e.g.
#' \code{"main_"}).
#'
#' @return A language object with any \code{amp_acroN} symbols replaced.
#' @noRd
.replace_amp_acro_symbol <- function(term, .data_prefix) {
  if (is.symbol(term)) {
    term_name <- as.character(term)
    m <- regmatches(term_name, regexec("^amp_acro([0-9]+)$", term_name))[[1]]
    if (length(m) == 2) {
      n <- m[2]
      return(call(
        "+",
        as.name(paste0(.data_prefix, "rrr", n)),
        as.name(paste0(.data_prefix, "sss", n))
      ))
    }
    return(term)
  }
  if (is.call(term)) {
    return(as.call(lapply(as.list(term), function(sub) {
      if (identical(sub, quote(expr = ))) {
        return(sub)
      }
      .replace_amp_acro_symbol(sub, .data_prefix)
    })))
  }
  term
}

#' Substitute \code{amp_acroN} placeholders within a random-effects bar term.
#'
#' The top-level "+" chain of \code{expr} is flattened first so that a bare
#' \code{amp_acroN} term (the common case, e.g. \code{0 + amp_acro2}) splices
#' in as flat sibling terms rather than as a nested "+" call, avoiding
#' spurious parentheses. Terms where \code{amp_acroN} appears inside a larger
#' sub-expression (e.g. \code{amp_acro1 * treatment}) are substituted in
#' place, which correctly (and necessarily) parenthesizes the replacement.
#'
#' @param expr A language object representing the left-hand side of an
#' \code{lme4}-style bar term (e.g. \code{lme4::findbars(formula)[[1]][[2]]}).
#' @param .data_prefix Prefix used for the rrr/sss column names (e.g.
#' \code{"main_"}).
#'
#' @return A language object with any \code{amp_acroN} symbols replaced.
#' @noRd
.substitute_amp_acro_terms <- function(expr, .data_prefix) {
  additive_terms <- .flatten_additive_terms(expr)
  new_terms <- unlist(
    lapply(additive_terms, function(term) {
      if (is.symbol(term) && grepl("^amp_acro[0-9]+$", as.character(term))) {
        .flatten_additive_terms(.replace_amp_acro_symbol(term, .data_prefix))
      } else {
        list(.replace_amp_acro_symbol(term, .data_prefix))
      }
    }),
    recursive = FALSE
  )
  Reduce(function(a, b) call("+", a, b), new_terms)
}
