
#' Update data and formula for fitting cosinor.glmm model
#'
#' @param data input data for fitting cosinor.glmm model.
#' @param formula model formula, specified by user including \code{amp.acro()}.
#' @param family the model family.
#'
#' @return Returns a \code{list}.
#' @export
update_formula_and_data <- function(data, formula, family = "gaussian", verbose = FALSE) {
  # Extract only the amp.acro function from the call

  #check if family argument is of type family
  stopifnot(inherits(family, "family"))

  #formatting data to be evaluated in amp.acro()
  Terms <- stats::terms(formula, specials = c("amp.acro"))
  amp.acro_text <- attr(Terms, "term.labels")[attr(Terms, "special")$amp.acro - 1]
  e <- str2lang(amp.acro_text)
  e$.data <- data # add data that will be called to amp.acro()
  e$.formula <- formula # add formula that will be called to amp.acro()
  e$.verbose <- verbose
 # e$.verbose <- verbose
  updated_df_and_formula <- eval(e) # evaluate amp.acro call
  c(updated_df_and_formula, list(Terms = Terms, family = family, Call = match.call())) # TODO: extract period from amp.acro call and return here
}


#' Checks the validity of user inputs and creates formula and modifies dataframe
#'
#' @param time_col a column of time values in the dataframe
#' @param n_components number of components in the model
#' @param group a vector of the names of group factors. The levels of each factor
#'              should be ordered, with the first level of each factor being the reference level
#' @param .data the dataframe from the original cosinor.glmm() function
#' @param .formula the formula from the original cosinor.glmm() function
#' @param period the period of each component
#'
#' @srrstatsTODO {G2.1} *Implement assertions on types of inputs (see the initial point on nomenclature above).*
#' @srrstatsTODO {G2.0} *Implement assertions on lengths of inputs, particularly through asserting that inputs expected to be single- or multi-valued are indeed so.*
#' @srrstatsTODO {G2.2} *Appropriately prohibit or restrict submission of multivariate input to parameters expected to be univariate.*
#' @srrstatsTODO {G2.4} *Provide appropriate mechanisms to convert between different data types, potentially including:*
#' @srrstatsTODO {G2.4} *Provide appropriate mechanisms to convert between different data types, potentially including:*
#' @srrstatsTODO {G2.4a} *explicit conversion to `integer` via `as.integer()`*
#' @srrstatsTODO {G2.4c} *explicit conversion to character via `as.character()` (and not `paste` or `paste0`)*
#' @srrstatsTODO {G2.5} *Where inputs are expected to be of `factor` type, secondary documentation should explicitly state whether these should be `ordered` or not, and those inputs should provide appropriate error or other routines to ensure inputs follow these expectations.*
#' @srrstatsTODO {G2.9} *Software should issue diagnostic messages for type conversion in which information is lost (such as conversion of variables from factor to character; standardisation of variable names; or removal of meta-data such as those associated with [`sf`-format](https://r-spatial.github.io/sf/) data) or added (such as insertion of variable or column names where none were provided).*
#'
#' @return
#' @noRd
#'
#' @examples
amp.acro <- function(time_col, n_components = 1, group, .data, .formula, period = 12, .verbose = FALSE) {
  #' @srrstatsTODO {G2.1} *Implement assertions on types of inputs (see the initial point on nomenclature above).*
  stopifnot(assertthat::is.count(n_components)) # Ensure n_components is an integer > 0
  lapply(period, function(period) stopifnot(assertthat::is.number(period))) # ensure period is numeric
  stopifnot(all(period > 0)) # ensure all periods are greater than 0
  stopifnot(inherits(.formula, "formula")) # check that .formula is of class 'formula'
  stopifnot(paste(substitute(time_col)) %in% colnames(.data)) # check for time column in .data

  # checking time_col data
   # ensure time_col is of the right class (most likely a character)
  if (assertthat::is.string(substitute(time_col))) {
    stop("time_col argument must not be a string")
  }

  if (!inherits(substitute(time_col), "name")) {
    stop("time_col must be name of column in data.")
  }


  # ensure .data argument is a dataframe or matrix
  assertthat::assert_that(
    inherits(.data, "data.frame") | inherits(.data, "matrix"),
    msg = "'data' must be of class 'data.frame' or 'matrix'"
  )

  # ensure group argument is a string or character
  if (assertthat::is.string(group) == FALSE) {
    stop("group argument must be a string or character")
  }

  # allow the user to not have any grouping structure (if group argument is missing)
  if (missing(group)) {
    group <- 0
    group_check <- FALSE
  } else {
    group_check <- TRUE
    check_group_var(.data = .data, group = group)
  }
  # "group_check" variable is passed to cosinor.glmm to indicate if there is a
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
  if (any(grepl("rrr", group) == TRUE) | any(grepl("sss", group) == TRUE)) {
    stop("Group variable names cannot contain 'rrr' or 'sss' ")
  }

  # test for whether the length of the period matches the value of n_components
  # if one period is supplied but n_components > 1, then the period is repeated to
  # match the value of n_components
  #' @srrstatsTODO {G2.0} *Implement assertions on lengths of inputs, particularly through asserting that inputs expected to be single- or multi-valued are indeed so.*

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

  #Formatting the group columns in .data as factors
    for (i in group_names) {
    .data[[i]] <- factor(.data[[i]])
  }
  # get the terms and variable names from the amp.acro call
  # Terms <- stats::terms(.formula)
  Terms <- terms(.formula, specials = "amp.acro")
  Terms$factors <- group_names
  varnames <- get_varnames(Terms)
  # create the initial formula string
  spec_dex <- unlist(attr(Terms, "special")$amp.acro) - 1
  non_acro_formula <- attr(Terms,"term.labels")[-spec_dex]

  # generate 'n_components' number of rrr and sss vectors
  n_count <- 1:n_components
  vec_rrr <- (paste0("rrr", n_count)) # vector of rrr names
  vec_sss <- (paste0("sss", n_count)) # vector of sss names
  formula_expr <- NULL
  # adding the rrr and sss columns to the dataframe
  for (i in 1:n_components) {
    #browser()
    rrr_names <- eval(vec_rrr[i])
    sss_names <- eval(vec_sss[i])
    .data[[rrr_names]] <- cos(2 * pi * ttt / period[i])
    .data[[sss_names]] <- sin(2 * pi * ttt / period[i])

    # add a warning message that columns have been added to the dataframe
    browser()
    if (.verbose) {
    message(paste(rrr_names,"and",sss_names,"have been added to dataframe"))
    }

    # if grouping variable is not 0 (NA), create interaction terms in the formula
    if (group[i] != 0) {

      acpart <- paste((rep(group[i], 2)), c(rrr_names, sss_names), sep = ":")
      acpart_combined <- paste(acpart[1], acpart[2], sep = " + ")
      formula_expr <- paste(formula_expr,"+",acpart_combined)
    }

    # if grouping variable is 0 (or NA), do not create interaction terms in the formula
    if (group[i] == 0) {
      acpart_combined <- NULL
      formula_expr <- paste(formula_expr, "+",rrr_names, "+", sss_names)
      #formula_expr <- str2expression(noquote(paste("update(newformula, .~. +", rrr_names, "+", sss_names, ")")))
    }

    #newformula <- eval(formula_expr)
  }
  newformula <- stats::as.formula(paste(all.vars(.formula, max.names = 1), # rownames(attr(Terms, "factors"))[1],
                                        paste(c(attr(terms(.formula), "intercept"),non_acro_formula, formula_expr), collapse = " + "),
                                        sep = " ~ "
  ))
  newformula <- update.formula(newformula, .~. )
  # update the formula

  # create NULL vectors for group metrics. These will be updated if there is a group argument
  group_stats <- NULL
  if (group_check == TRUE) {
    for (i in group_names) {
      single_group_level <- levels(as.factor(.data[[i]]))
      group_stats[[i]] <- as.array(single_group_level)
    }
    # colnames(group_stats) = group_names
  }
  return(list(
    newdata = .data, newformula = newformula, vec_rrr = vec_rrr, vec_sss = vec_sss, n_components = n_components,
    period = period,
    group_stats = group_stats,
    group = group,
    group_check = group_check
  ))
}


#' Checks that the group names supplied by the user are in the dataframe
#'
#' @param .data dataframe
#' @param group group argument specified in the cosinor.glmm call
#'
#' @return nothing if successful, an error message if not
#' @noRd
#'
#' @examples
check_group_var <- function(.data, group) {
  grouping_vars <- group[!group %in% c(0, NA)]
  if(!all(grouping_vars %in% colnames(.data))) {
    bad_groups <- grouping_vars[which(!grouping_vars %in% colnames(.data))]
    stop(
      "Grouping variable(s) not found in input data: [",
      paste0(bad_groups, collapse = ", "),
      "]"
    )
  }
}

