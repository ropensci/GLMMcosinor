
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
  ttt <- eval(substitute(time_col), env = .data) # extract vector of "time" values from .data
  #Test for whether n_components is an integer greater than 0

  stopifnot(assertthat::is.count(n_components))
  lapply(period, function(period) stopifnot(assertthat::is.number(period)))
  stopifnot(all(period > 0))
  stopifnot(inherits(.formula, "formula"))

  # add stopifnot for group and time_col as character (assertthat::is.string())
  # add stopifnot for .data being a data.frame or tibble or whatever would work for lm
  # add stopifnot for substitute(time_col) being a column within .data

  # if (n_components %% 1 != 0| n_components < 1) {
  #     stop("Number of components (n_components) must be an integer greater than 0")
  # }


  #Allow the user to not have any grouping structure (if group arg is missing)
  group_check = TRUE
  if (missing(group) == TRUE) {
    group = 0
    group_check = FALSE
  }

  #Test for whether the length of the grouping variable matches the value of n_components.
  #If one grouping variable is supplied but n_components > 1, then the one grouping
  #variable is repeated to match the value of n_components
  if (length(group) != n_components) {
    if (length(group) == 1) {
      group <- rep(group, n_components)
    } else {
      stop("grouping variable in amp.acro() must be of length 1 or the same as n_components.")
    }
  }

  #Show error message if user uses 'rrr' or 'sss' for their grouping variable name
  if (any(grepl("rrr", group) == TRUE) | any(grepl("sss",group) == TRUE)) {
    stop("Group variable names cannot contain 'rrr' or 'sss' ")
  }

  #Test for whether the length of the period matches the value of n_components
  #If one period is supplied but n_components > 1, then the period is repeated to
  #match the value of n_components
  if (length(period) != n_components) {
    if (length(period) == 1) {
      period <- rep(period, n_components)
    } else {
      stop("period value(s) in amp.acro() must be of length 1 or the same as n_components.")
    }
  }

  #Checks for NA group values supplied by the user and replaces with zeroes.
  #This is important when creating the formula: 'newformula'.
  for (i in 1:(length(group))) {
    if (is.na(group[i]) == TRUE) {
      group[i] <- 0
    }
  }
  #Create a vector with just the named groups, disregarding 'zero'/NA elements
  group_names <- group[group != 0]

  #Get the terms and variable names from the amp.acro call
  Terms <- stats::terms(.formula)
  varnames <- get_varnames(Terms)

  #Create the initial formula string
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

    #If grouping variable is not 0 (NA), create interaction terms in the formula
    if (group[i] != 0) {
      acpart <- paste((rep(group[i], 2)), c(rrr_names, sss_names), sep = ":")
      acpart_combined <- paste(acpart[1], acpart[2], sep = " + ")
      formula_expr <- str2expression(noquote(paste(
        "update.formula(newformula, .~. +", rrr_names, "+", sss_names,
        "+", acpart_combined, ")"
      )))
    }

    #If grouping variable is 0 (NA), do not create interaction terms in the formula
    if (group[i] == 0) {
      acpart_combined <- NULL
      formula_expr <- str2expression(noquote(paste("update.formula(newformula, .~. +", rrr_names, "+", sss_names, ")")))
    }

    #Evaluate the formula string expression
    newformula <- eval(formula_expr)
  }

  #Update the formula
  newformula <- update.formula(newformula, ~.)
  group_levels <- NULL
  group_levels <- NULL
  group_levels_total <- NULL
  ref_group_level <- NULL

  if (group_check == TRUE) {
  group_levels_total = rep(0,length(group_names))
  for (i in 1:length(group_names)) {
    single_group_level <- levels(as.factor(.data[[group_names[i]]]))
    ref_group_level <- append(single_group_level[1],ref_group_level)
    single_group_level <- single_group_level[-1]
    group_levels <- append(group_levels, single_group_level)
    group_levels_total[[i]] <- length(single_group_level)
  }
  }

  group_stats = list(group = group,group_names = group_names,
                     group_levels = group_levels,
                     group_levels_total = group_levels_total,
                     group_check = group_check,
                     ref_group_level = ref_group_level)
  return(list(data = .data, formula = newformula, vec_rrr = vec_rrr, vec_sss = vec_sss, n_components = n_components,
              group_stats = group_stats))
}
