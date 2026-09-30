#' Combine multiple `amp_acro()` calls (from the same side of a model
#' formula) into a single, equivalent call.
#'
#' Allows the grouping structure to be spread over separate `amp_acro()`
#' terms (e.g. one per grouping variable) rather than requiring a single
#' call with a combined `group` vector. Each call's `n_components`,
#' `group`, and `period` are evaluated and recycled independently, then
#' concatenated in the order the calls appear in the formula - equivalent
#' to writing one call with `n_components` set to their sum and `group`/
#' `period` set to the concatenation of each call's (recycled) vectors.
#'
#' @param amp_acro_text A \code{character} vector of `amp_acro()` call text,
#' one per term (as returned by \code{attr(terms(...), "term.labels")}).
#' @param env The environment in which to evaluate each call's arguments.
#' @noRd
merge_amp_acro_calls <- function(amp_acro_text, env) {
  matched_calls <- lapply(amp_acro_text, function(text) {
    match.call(amp_acro, str2lang(text))
  })

  time_cols <- vapply(matched_calls, function(mc) deparse1(mc$time_col), "")
  if (length(unique(time_cols)) > 1) {
    stop(
      "When a model formula includes multiple amp_acro() terms, they must ",
      "all use the same 'time_col'."
    )
  }

  n_components_list <- lapply(matched_calls, function(mc) {
    if (is.null(mc$n_components)) 1 else eval(mc$n_components, envir = env)
  })

  recycle_to_n <- function(x, n) {
    if (length(x) == 1) rep(x, n) else x
  }

  group_list <- Map(
    function(mc, n) {
      g <- if (is.null(mc$group)) 0 else eval(mc$group, envir = env)
      recycle_to_n(g, n)
    },
    matched_calls,
    n_components_list
  )

  period_list <- Map(
    function(mc, n) recycle_to_n(eval(mc$period, envir = env), n),
    matched_calls,
    n_components_list
  )

  as.call(list(
    quote(amp_acro),
    time_col = matched_calls[[1]]$time_col,
    n_components = sum(unlist(n_components_list)),
    group = do.call(c, group_list),
    period = do.call(c, period_list)
  ))
}

#' Update data and formula for fitting cglmm model
#'
#' @param data input data for fitting cglmm model.
#' @param formula model formula, specified by user including \code{amp_acro()}.
#' @param family the model family.
#' @param quietly controls whether messages from amp_acro are displayed.
#' TRUE by default
#' @param dispformula The formula specifying the dispersion model
#' @param ziformula The formula specifying the zero-inflation model
#'
#' @srrstats {G2.13}
#' @srrstats {G2.14b}
#' @srrstats {G1.4}
#' @srrstats {G2.6}
#' @srrstats {G2.7}
#' @srrstats {RE2.0}
#' @srrstats {RE2.1}
#' @srrstats {G2.8}
#' @srrstats {G2.9}
#' @srrstats {G2.10}
#' @srrstats {G2.13}

#' @return Returns a \code{list}.
#' @examples
#' # Use vitamind data but add a "patient" identifier used as a random effect
#' vitamind2 <- vitamind
#' vitamind2$patient <- sample(
#'   LETTERS[1:5],
#'   size = nrow(vitamind2), replace = TRUE
#' )
#'
#' # Use update_formula_and_data() to perform wrangling steps of cglmm()
#' # without yet fitting the model
#' data_and_formula <- update_formula_and_data(
#'   data = vitamind2,
#'   formula = vit_d ~ X + amp_acro(time,
#'     group = "X",
#'     period = 12
#'   )
#' )
#'
#' # print formula from above
#' data_and_formula$newformula
#'
#' # fit model while adding random effect to cosinor model formula.
#' mod <- fit_model_and_process(
#'   obj = data_and_formula,
#'   formula = update.formula(
#'     data_and_formula$newformula, . ~ . + (1 | patient)
#'   )
#' )
#'
#' mod
#' mod$fit # printing the `glmmTMB` model within shows Std.Dev. of random effect
#' @export
update_formula_and_data <- function(
  data,
  formula,
  family = "gaussian",
  quietly = TRUE,
  dispformula = ~1,
  ziformula = ~0
) {
  # Extract only the amp_acro function from the call
  # check for missing data
  if (!quietly) {
    if (any(is.na(data))) {
      message("\n Missing data in the following dataframe columns: \n")
      print(colSums(is.na(data)))
      message("\n Missing data will be ignored ")
    }
  }

  # formatting data to be evaluated in amp_acro()
  formula_eval <- function(
    formula,
    data,
    quietly,
    amp_acro_ind = -1,
    data_prefix = "main_",
    dispformula_check = FALSE,
    ziformula_check = FALSE,
    no_amp_acro_vector = no_amp_acro_vector,
    cond_period = NULL
  ) {
    Terms <- stats::terms(formula, specials = c("amp_acro"))
    amp_acro_text <- attr(
      Terms,
      "term.labels"
    )[attr(Terms, "special")$amp_acro + amp_acro_ind]
    if (length(amp_acro_text) == 0) {
      e <- str2lang("amp_acro(no_amp_acro = TRUE)")
      e$n_components <- 0
    } else if (length(amp_acro_text) == 1) {
      e <- str2lang(amp_acro_text)
    } else {
      # more than one amp_acro() term on this side of the formula (e.g. the
      # grouping structure spread over separate calls) - merge them into a
      # single equivalent call
      e <- merge_amp_acro_calls(amp_acro_text, environment())
    }

    e$.data <- data # add data that will be called to amp_acro()
    e$.formula <- formula # add formula that will be called to amp_acro()
    e$.quietly <- quietly
    e$.amp_acro_ind <- amp_acro_ind
    e$.data_prefix <- data_prefix
    e$no_amp_acro_vector <- no_amp_acro_vector
    e$cond_period <- cond_period

    ranef_part <- lapply(reformulas::findbars(formula), deparse1)
    ranef_part_group <- gsub(".*\\|\\s*(.*)", "\\1", ranef_part)
    # e$subject <- ranef_part_group

    c(
      eval(e), # evaluate amp_acro call: updated_df_and_formula
      list(
        Terms = Terms,
        family = family,
        dispformula_check = dispformula_check,
        ziformula_check = ziformula_check
      )
    )
  }

  main_output <- formula_eval(
    formula,
    data,
    quietly,
    amp_acro_ind = -1,
    data_prefix = "main_",
    no_amp_acro_vector = FALSE
  )

  items_keep <- c(
    "newformula",
    "vec_rrr",
    "vec_sss",
    "n_components",
    "components",
    "period",
    "group_stats",
    "group_check",
    "group"
  )

  # disp formula
  data <- main_output$newdata
  dispformula_eval <- formula_eval(
    formula = dispformula,
    data = data,
    quietly = quietly,
    amp_acro_ind = 0,
    data_prefix = "disp_",
    no_amp_acro_vector = main_output$no_amp_acro_vector,
    cond_period = main_output$period
  )
  main_output$newdata <- dispformula_eval$newdata
  main_output$no_amp_acro_vector <- dispformula_eval$no_amp_acro_vector

  dispformula_eval <- dispformula_eval[items_keep]
  names(dispformula_eval)[names(dispformula_eval) == "newformula"] <- "formula"
  main_output$dispformula <- dispformula_eval
  main_output$dispformula_check <- dispformula_eval$n_components != 0
  main_output$dispformula_used <- dispformula != ~1

  # zero-inflated formula
  data <- main_output$newdata

  ziformula_eval <- formula_eval(
    formula = ziformula,
    data = data,
    quietly = quietly,
    amp_acro_ind = 0,
    data_prefix = "zi_",
    no_amp_acro_vector = main_output$no_amp_acro_vector,
    cond_period = main_output$period
  )

  main_output$newdata <- ziformula_eval$newdata
  main_output$no_amp_acro_vector <- ziformula_eval$no_amp_acro_vector

  ziformula_eval <- ziformula_eval[items_keep]
  names(ziformula_eval)[names(ziformula_eval) == "newformula"] <- "formula"
  main_output$ziformula <- ziformula_eval
  main_output$ziformula_check <- ziformula_eval$n_components != 0
  main_output$ziformula_used <- ziformula != ~0

  main_output
}


#' Checks that the group names supplied by the user are in the dataframe
#'
#' @param .data dataframe
#' @param group group argument specified in the cglmm call
#' @srrstats {G1.4a}
#'
#' @return nothing if successful, an error message if not
#' @noRd

# check the group inputs
check_group_var <- function(.data, group) {
  grouping_vars <- group[!group %in% c(0, NA)]
  if (!all(grouping_vars %in% colnames(.data))) {
    bad_groups <- grouping_vars[which(!grouping_vars %in% colnames(.data))]
    stop(
      "Grouping variable(s) not found in input data: [",
      paste0(bad_groups, collapse = ", "),
      "]"
    )
  }
}

#' Checks that the `ci_level` values provided are reasonable
#'
#' @param ci_level Confidence level.
#' @srrstats {G1.4a}
#' @return nothing if successful, an error message if not
#' @noRd

validate_ci_level <- function(ci_level) {
  assertthat::is.number(ci_level)

  if (ci_level < 0 || ci_level > 1) {
    stop("'ci_level' must be a single numeric value in [0, 1].")
  }
}
#' Escape regex metacharacters in a string so it can be used as a literal
#' match within a larger regex pattern.
#' @noRd
escape_regex <- function(x) {
  # backslash must be escaped first, so escaping later characters doesn't
  # double-escape the backslashes it introduces
  specials <- c("\\", ".", "|", "(", ")", "[", "]", "{", "}", "^", "$", "*", "+", "?")
  for (s in specials) {
    x <- gsub(s, paste0("\\", s), x, fixed = TRUE)
  }
  x
}

#' Build a regex matching coefficient names for `group`'s interaction with
#' `rrr_or_sss` (or, if `group` is 0/unused, the bare `rrr_or_sss` term).
#'
#' Coefficient names concatenate a group's name and its level with no
#' separator (e.g. group "X2" at level "1" becomes "X21"). An unanchored
#' pattern built from another group's name (e.g. "X") would also match this
#' ("X21" contains "X"). This anchors the match to the start of the
#' coefficient name and excludes any other group name in `other_groups`
#' that has `group` as a prefix, so distinct groups are never conflated.
#' @noRd
group_coef_regex <- function(group, other_groups, rrr_or_sss) {
  rrr_or_sss <- escape_regex(rrr_or_sss)
  if (identical(group, 0) || identical(group, "0")) {
    return(paste0("^", rrr_or_sss, "$"))
  }

  group <- escape_regex(as.character(group))
  other_groups <- as.character(other_groups[other_groups != 0])
  colliding <- other_groups[
    other_groups != group & startsWith(other_groups, group)
  ]

  lookahead <- if (length(colliding) > 0) {
    suffixes <- escape_regex(substring(colliding, nchar(group) + 1))
    paste0("(?!", paste(suffixes, collapse = "|"), ")")
  } else {
    ""
  }

  paste0("^", group, lookahead, ".*:", rrr_or_sss, "$")
}

# calculate the parameters from the raw estimates
get_new_coefs <- function(coefs, vec_rrr, vec_sss, n_components, components) {
  r.coef <- NULL
  s.coef <- NULL
  mu.coef <- NULL
  mu_inv <- rep(0, length(names(coefs)))

  all_groups <- vapply(components, function(cmp) as.character(cmp$group), "")

  # Get a Boolean vector for rrr, sss, and mu. This will be used to extract
  # the relevant raw parameters from the raw coefficient model output
  for (i in seq_len(n_components)) {
    period_idx <- components[[i]]$period_idx

    group <- components[[i]]$group
    other_groups <- all_groups[-i]

    r.coef[[i]] <- grepl(
      group_coef_regex(group, other_groups, vec_rrr[period_idx]),
      names(coefs),
      perl = TRUE
    )
    s.coef[[i]] <- grepl(
      group_coef_regex(group, other_groups, vec_sss[period_idx]),
      names(coefs),
      perl = TRUE
    )

    # Keep track of non-mesor terms
    mu_inv_carry <- r.coef[[i]] + s.coef[[i]]
    # Ultimately, every non-mesor term will be true
    mu_inv <- mu_inv_carry + mu_inv
  }

  # invert 'mu_inv' to get a Boolean vector for mesor terms
  mu.coef <- c(!mu_inv)
  # a matrix of rrr coefficients
  r.coef <- (t(matrix(unlist(r.coef), ncol = length(r.coef))))
  # a matrix of sss coefficients
  s.coef <- (t(matrix(unlist(s.coef), ncol = length(s.coef))))

  # Calculate the parameter estimates for all components
  amp <- NULL
  acr <- NULL
  acr_adjusted <- NULL
  for (i in seq_len(n_components)) {
    period_idx <- components[[i]]$period_idx

    beta.s <- coefs[s.coef[i, ]]
    beta.r <- coefs[r.coef[i, ]]

    groups.r <- c(beta.r[1], beta.r[which(names(beta.r) != names(beta.r[1]))])
    groups.s <- c(beta.s[1], beta.s[which(names(beta.s) != names(beta.s[1]))])

    amp[[i]] <- sqrt(groups.r^2 + groups.s^2)
    names(amp[[i]]) <- gsub(
      vec_rrr[period_idx],
      paste0("amp", i),
      names(beta.r)
    )

    acr[[i]] <- atan2(groups.s, groups.r)
    names(acr[[i]]) <- gsub(
      vec_sss[period_idx],
      paste0("acr", i),
      names(beta.s)
    )
  }
  new_coefs <- c(coefs[mu.coef], unlist(amp), unlist(acr))
  # if n_components = 1, then print "amp" and "acr" rather than "amp1", "acr1"
  if (n_components == 1) {
    names(amp[[1]]) <- gsub(vec_rrr[1], "amp", names(beta.r))
    names(acr[[1]]) <- gsub(vec_sss[1], "acr", names(beta.s))
    new_coefs <- c(coefs[mu.coef], unlist(amp), unlist(acr))
  }
  new_coefs
}
