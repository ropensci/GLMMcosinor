
update_formula_and_data <- function(data, formula) {
  #Extract only the amp.acro function from the call
  Terms <- stats::terms(formula, specials = c("amp.acro"))
  amp.acro_text <- attr(Terms, "term.labels")[attr(Terms, "special")$amp.acro - 1]
  e <- str2lang(amp.acro_text)
  e$.data <- data # add data that will be called to amp.acro()
  e$.formula <- formula # add formula that will be called to amp.acro()
  updated_df_and_formula <- eval(e) # evaluate amp.acro call
  c(updated_df_and_formula, list(Terms = Terms))
}


amp.acro <- function(time_col, n_components = 1, group, .data, .formula, period = 12) {

  stopifnot(assertthat::is.count(n_components)) #Ensure n_components is an integer > 0
  lapply(period, function(period) stopifnot(assertthat::is.number(period))) # ensure period is numeric
  stopifnot(all(period > 0)) #ensure all periods are greater than 0
  stopifnot(inherits(.formula, "formula")) # check that .formula is of class 'formula'
  stopifnot(paste(substitute(time_col)) %in% colnames(.data)) # check for time column in .data

  #ensure time_col is not a character
  if (assertthat::is.string(substitute(time_col)) == TRUE) {
    stop("time_col argument must not be a string")
  }

  #ensure .data argument is a dataframe or matrix
  assertthat::assert_that(
    inherits(.data, "data.frame") | inherits(.data, "matrix"),
    msg = "'data' must be of class 'data.frame' or 'matrix'"
  )

  #NOT SURE IF THIS IS NECESSARY
  #assertthat::assert_that(
  #  isTRUE(
  #    all(unique(purrr::map_chr(.data, class)) %in% c("numeric", "integer","factor"))
  #  ),
  #  msg = "All columns of 'data' must be numeric, integer, or factor class"
  #)

  # allow the user to not have any grouping structure (if group argument is missing)
  if (missing(group) == TRUE) {
    group = 0
    group_check = FALSE
  } else {
    group_check = TRUE
    }
  #"group_check" variable can be passed to cosinor.glmm to indicate if there is a
  # group argument present in amp.acro()

  # test for whether the length of the grouping variable matches the value of n_components.
  # if one grouping variable is supplied but n_components > 1, then the one grouping
  # variable is repeated to match the value of n_components
  if (length(group) != n_components) {
    if (length(group) == 1) {
      group <- rep(group, n_components)
    } else {
      stop("grouping variable in amp.acro() must be of length 1 or the same as n_components.")
    }
  }

  # show error message if user uses 'rrr' or 'sss' in their grouping variable name
  if (any(grepl("rrr", group) == TRUE) | any(grepl("sss",group) == TRUE)) {
    stop("Group variable names cannot contain 'rrr' or 'sss' ")
  }

  # test for whether the length of the period matches the value of n_components
  # if one period is supplied but n_components > 1, then the period is repeated to
  # match the value of n_components
  if (length(period) != n_components) {
    if (length(period) == 1) {
      period <- rep(period, n_components)
    } else {
      stop("period value(s) in amp.acro() must be of length 1 or the same as n_components.")
    }
  }

  # check for NA group values supplied by the user and replaces with zeroes.
  # this is important when creating the formula: 'newformula'.
  for (i in 1:(length(group))) {
    if (is.na(group[i]) == TRUE) {
      group[i] <- 0
    }
  }

  # extract the time vector
  ttt <- eval(substitute(time_col), env = .data) # extract vector of "time" values from .data

  # create a vector with just the named groups, disregarding 'zero'/NA elements
  group_names <- group[group != 0]

  # get the terms and variable names from the amp.acro call
  Terms <- stats::terms(.formula)
  varnames <- get_varnames(Terms)

  # create the initial formula string
  newformula <- stats::as.formula(paste(all.vars(.formula, max.names=1), #rownames(attr(Terms, "factors"))[1],
                                        paste(c(attr(terms(.formula), "intercept"), group_names), collapse = " + "),
                                        sep = " ~ "
  ))

  # generate 'n_components' number of rrr and sss vectors
  n_count <- 1:n_components
  vec_rrr <- (paste0("rrr", n_count)) #vector of rrr names
  vec_sss <- (paste0("sss", n_count)) #vector of sss names

  # adding the rrr and sss columns to the dataframe
  for (i in 1:n_components) {
    rrr_names <- eval(vec_rrr[i])
    sss_names <- eval(vec_sss[i])
    .data[[rrr_names]] <- cos(2 * pi * ttt / period[i])
    .data[[sss_names]] <- sin(2 * pi * ttt / period[i])

    # if grouping variable is not 0 (NA), create interaction terms in the formula
    if (group[i] != 0) {
      acpart <- paste((rep(group[i], 2)), c(rrr_names, sss_names), sep = ":")
      acpart_combined <- paste(acpart[1], acpart[2], sep = " + ")
      formula_expr <- str2expression(noquote(paste(
        "update.formula(newformula, .~. +", rrr_names, "+", sss_names,
        "+", acpart_combined, ")"
      )))
    }

    # if grouping variable is 0 (or NA), do not create interaction terms in the formula
    if (group[i] == 0) {
      acpart_combined <- NULL
      formula_expr <- str2expression(noquote(paste("update.formula(newformula, .~. +", rrr_names, "+", sss_names, ")")))
    }

    # evaluate the formula string expression
    newformula <- eval(formula_expr)
  }

  # update the formula
  newformula <- update.formula(newformula, ~.)

  # create NULL vectors for group metrics. These will be updated if there is a group argument
  group_stats = NULL
  if (group_check == TRUE) {
  group_levels_total = rep(0,length(group_names))
  for (i in group_names) {
    single_group_level <- levels(as.factor(.data[[i]]))
    group_stats[[i]] <- as.array(single_group_level)
  }
 # colnames(group_stats) = group_names
  }
  return(list(data = .data, formula = newformula, vec_rrr = vec_rrr, vec_sss = vec_sss, n_components = n_components,
              group_stats = group_stats,
              group = group,
              group_check = group_check))
}