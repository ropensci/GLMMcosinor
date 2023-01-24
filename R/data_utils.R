#' Update data and formula for fitting cosinor.glmm model
#'
#' @param data input data for fitting cosinor.glmm model.
#' @param formula model formula, specified by user including \code{amp.acro()}.
#' @param family the model family.
#' @param quietly controls whether messages from amp.acro are displayed. TRUE by default
#'
#' @srrstatsTODO {G2.13} *Statistical Software should implement appropriate checks for missing data as part of initial pre-processing prior to passing data to analytic algorithms.*
#' @srrstatsTODO {G2.14b} *ignore missing data with default warnings or messages issued*
#'
#' @return Returns a \code{list}.
#' @export
update_formula_and_data <- function(data, formula,
                                    family = "gaussian",
                                    quietly = TRUE,
                                    dispformula = ~1,
                                    ziformula = ~0) {
  # Extract only the amp.acro function from the call
  # check for missing data
  if (!quietly) {
    if (any(is.na(data))) {
      message("\n Missing data in the following dataframe columns: \n")
      print(colSums(is.na(data)))
      message("\n Missing data will be ignored ")
    }
  }

  # formatting data to be evaluated in amp.acro()
  # check for dispformula and/or ziformula argument
  dispformula_check <- (!missing(dispformula) & dispformula != ~1)
  ziformula_check <- (!missing(ziformula) & ziformula != ~0)


  formula_eval <- function(formula,
                           data,
                           quietly,
                           amp.acro_ind = -1,
                           data_prefix = "main_") {
    Terms <- stats::terms(formula, specials = c("amp.acro"))
    amp.acro_text <- attr(Terms, "term.labels")[attr(Terms, "special")$amp.acro + amp.acro_ind]
    e <- str2lang(amp.acro_text)
    e$.data <- data # add data that will be called to amp.acro()
    e$.formula <- formula # add formula that will be called to amp.acro()
    e$.quietly <- quietly
    e$.amp.acro_ind <- amp.acro_ind
    e$.data_prefix <- data_prefix
    updated_df_and_formula <- eval(e) # evaluate amp.acro call
    c(updated_df_and_formula, list(
      Terms = Terms, family = family,
      dispformula_check = dispformula_check,
      ziformula_check = ziformula_check
    ))
  }
  main_output <- formula_eval(formula,
    data,
    quietly,
    amp.acro_ind = -1,
    data_prefix = "main_"
  )
  if (dispformula_check) {
    data <- main_output$newdata
    dispformula <- formula_eval(formula = dispformula, data = data, quietly = quietly, amp.acro_ind = 0, data_prefix = "disp_")
    main_output$newdata <- dispformula$newdata
    main_output$dispformula <- list()
    main_output$dispformula$formula <- dispformula$newformula
    main_output$dispformula$vec_rrr <- dispformula$vec_rrr
    main_output$dispformula$vec_sss <- dispformula$vec_sss
    main_output$dispformula$n_components <- dispformula$n_components
    main_output$dispformula$period <- dispformula$period
    main_output$dispformula$group_stats <- dispformula$group_stats
    main_output$dispformula$group_check <- dispformula$group_check
    main_output$dispformula$group <- dispformula$group
  }
  if (ziformula_check) {
    data <- main_output$newdata
    ziformula <- formula_eval(formula = ziformula, data = data, quietly = quietly, amp.acro_ind = 0, data_prefix = "zi_")
    main_output$newdata <- ziformula$newdata
    main_output$ziformula <- list()
    main_output$ziformula$formula <- ziformula$newformula
    main_output$ziformula$vec_rrr <- ziformula$vec_rrr
    main_output$ziformula$vec_sss <- ziformula$vec_sss
    main_output$ziformula$n_components <- ziformula$n_components
    main_output$ziformula$period <- ziformula$period
    main_output$ziformula$group_stats <- ziformula$group_stats
    main_output$ziformula$group_check <- ziformula$group_check
    main_output$ziformula$group <- ziformula$group
  }
  main_output
}


#' Checks the validity of user inputs and creates formula and modifies dataframe
#'
#' @param time_col a column of time values in the dataframe
#' @param n_components number of components in the model
#' @param group a vector of the names of group factors. The levels of each factor should be ordered, with the first level of each factor being the reference level
#' @param .data the dataframe from the original cosinor.glmm() function
#' @param .formula the formula from the original cosinor.glmm() function
#' @param period the period of each component
#' @param .quietly controls whether messages from amp.acro are displayed. TRUE by default
#' @srrstatsTODO {G2.1} *Implement assertions on types of inputs (see the initial point on nomenclature above).*
#' @srrstatsTODO {G2.0} *Implement assertions on lengths of inputs, particularly through asserting that inputs expected to be single- or multi-valued are indeed so.*
#' @srrstatsTODO {G2.2} *Appropriately prohibit or restrict submission of multivariate input to parameters expected to be univariate.*
#' @srrstatsTODO {G2.4} *Provide appropriate mechanisms to convert between different data types, potentially including:*
#' @srrstatsTODO {G2.4} *Provide appropriate mechanisms to convert between different data types, potentially including:*
#' @srrstatsTODO {G2.4a} *explicit conversion to `integer` via `as.integer()`*
#' @srrstatsTODO {G2.4c} *explicit conversion to character via `as.character()` (and not `paste` or `paste0`)*
#' @srrstatsTODO {G2.5} *Where inputs are expected to be of `factor` type, secondary documentation should explicitly state whether these should be `ordered` or not, and those inputs should provide appropriate error or other routines to ensure inputs follow these expectations.*
#' @srrstatsTODO {G2.9} *Software should issue diagnostic messages for type conversion in which information is lost (such as conversion of variables from factor to character; standardisation of variable names; or removal of meta-data such as those associated with [`sf`-format](https://r-spatial.github.io/sf/) data) or added (such as insertion of variable or column names where none were provided).*
#' @srrstatsTODO {G2.6} *Software which accepts one-dimensional input should ensure values are appropriately pre-processed regardless of class structures.*
#' @srrstatsTODO {G2.0} *Implement assertions on lengths of inputs, particularly through asserting that inputs expected to be single- or multi-valued are indeed so.*
#' @srrstatsTODO {G2.4b} *explicit conversion to continuous via `as.numeric()`*
#' @srrstatsTODO {G2.2} *Appropriately prohibit or restrict submission of multivariate input to parameters expected to be univariate.* #ensure time_col is univariate
#' @srrstatsTODO {G5.0} *Where applicable or practicable, tests should use standard data sets with known properties (for example, the [NIST Standard Reference Datasets](https://www.itl.nist.gov/div898/strd/), or data sets provided by other widely-used R packages).*
#'
#' @return updated dataframe and formula to then be processed by data_processor()
#' @noRd

amp.acro <- function(time_col,
                     n_components = 1,
                     group,
                     .data,
                     .formula,
                     period = 12,
                     .quietly = TRUE,
                     .amp.acro_ind = -1,
                     .data_prefix = "main_") {
  # checking dataframe

  # ensure .data argument is a dataframe, matrix, or tibble (tested)
  assertthat::assert_that(
    inherits(.data, "data.frame") | inherits(.data, "matrix") | inherits(.data, "tbl"),
    msg = "'data' must be of class 'data.frame', 'matrix', or 'tibble'"
  )

  # Formatting .data argument as dataframe if matrix or tibble (tested)

  if (inherits(.data, "matrix") | inherits(.data, "tbl")) {
    .data <- as.data.frame(.data)
    if (!.quietly) {
      message("Data has been reformatted as dataframe") # (tested)
    }
  }

  env <- environment() # preserve environment of amp.acro to be passed into amp.acro_iteration
  amp.acro_iteration <- function(time_col, n_components, group, .formula, period, quietly = TRUE, .data, .amp.acro_ind = -1) {
    # assess the quality of the inputs
    stopifnot(assertthat::is.count(n_components)) # Ensure n_components is an integer > 0
    lapply(period, function(period) stopifnot(assertthat::is.number(period))) # ensure period is numeric
    stopifnot(all(period > 0)) # ensure all periods are greater than 0
    stopifnot(inherits(.formula, "formula")) # check that .formula is of class 'formula'

    # checking time_col data

    # check for time column in .data (tested)
    assertthat::assert_that((paste(substitute(time_col, env)) %in% colnames(.data)),
      msg = "time_col must be the name of a column in dataframe"
    )


    # ensure time_col is of the right class (most likely a character) (tested)
    if (assertthat::is.string(substitute(time_col, env))) {
      stop("time_col argument must not be a string")
    }

    # ensure time_col is within the dataframe
    if (!inherits(substitute(time_col, env), "name")) {
      stop("time_col must be name of column in data.")
    }

    # extract the time vector
    ttt <- eval(substitute(time_col, env), env = .data) # extract vector of "time" values from .data

    # ensure ttt contains numeric values only (tested)
    if (!assertthat::assert_that(is.numeric(ttt))) {
      stop("time column in dataframe must contain numeric values")
    }


    # ensure time_col is univariate (tested)
    assertthat::assert_that(is.vector(ttt),
      msg = "time_col must be univariate"
    )


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

    # ensure the length of the grouping variable matches the value of n_components. (tested)
    # if one grouping variable is supplied but n_components > 1, then the one grouping
    # variable is repeated to match the value of n_components
    if (length(group) != n_components) {
      if (length(group) == 1) {
        group <- rep(group, n_components)
      } else {
        stop("Grouping variable in amp.acro() must be of length 1 or the same as n_components")
      }
    }
    group_original <- group

    # show error message if user uses 'rrr' or 'sss' in their grouping variable name (tested)
    if (any(grepl("rrr", group) == TRUE) | any(grepl("sss", group) == TRUE)) {
      stop("Group variable names cannot contain 'rrr' or 'sss'")
    }

    # ensure the length of the period matches the value of n_components (tested)
    # if one period is supplied but n_components > 1, then the period is repeated to
    # match the value of n_components

    if (length(period) != n_components) {
      if (length(period) == 1) {
        period <- rep(period, n_components)
      } else {
        stop("period value(s) in amp.acro() must be of length 1 or the same as n_components")
      }
    }

    # check for NA group values supplied by the user and replaces with zeroes.
    # this is important when creating the formula: 'newformula'.
    for (i in 1:(length(group))) {
      if (is.na(group[i]) == TRUE) {
        group[i] <- 0
      }
    }

    # create a vector with just the named groups, disregarding 'zero'/NA elements
    group_names <- group[group != 0]

    # Formatting the group columns in .data as factors
    for (i in group_names) {
      .data[[i]] <- factor(.data[[i]])
    }
    # get the terms and variable names from the amp.acro call
    # Terms <- stats::terms(.formula)
    Terms <- terms(.formula, specials = "amp.acro")
    Terms$factors <- group_names
    varnames <- get_varnames(Terms)
    # create the initial formula string

    spec_dex <- unlist(attr(Terms, "special")$amp.acro) + .amp.acro_ind
    non_acro_formula <- attr(Terms, "term.labels")[-spec_dex]


    # generate 'n_components' number of rrr and sss vectors
    n_count <- 1:n_components
    vec_rrr <- (paste0(.data_prefix, "rrr", n_count)) # vector of rrr names
    vec_sss <- (paste0(.data_prefix, "sss", n_count)) # vector of sss names
    formula_expr <- NULL
    # adding the rrr and sss columns to the dataframe
    for (i in 1:n_components) {
      #
      rrr_names <- eval(vec_rrr[i])
      sss_names <- eval(vec_sss[i])
      .data[[rrr_names]] <- cos(2 * pi * ttt / period[i])
      .data[[sss_names]] <- sin(2 * pi * ttt / period[i])

      # add a warning message that columns have been added to the dataframe
      if (!.quietly) {
        message(paste(rrr_names, "and", sss_names, "have been added to dataframe"))
      }

      # if grouping variable is not 0 (NA), create interaction terms in the formula
      if (group[i] != 0) {
        acpart <- paste((rep(group[i], 2)), c(rrr_names, sss_names), sep = ":")
        acpart_combined <- paste(acpart[1], acpart[2], sep = " + ")
        formula_expr <- paste(formula_expr, "+", acpart_combined)
      }

      # if grouping variable is 0 (or NA), do not create interaction terms in the formula
      if (group[i] == 0) {
        acpart_combined <- NULL
        formula_expr <- paste(formula_expr, "+", rrr_names, "+", sss_names)
        # formula_expr <- str2expression(noquote(paste("update(newformula, .~. +", rrr_names, "+", sss_names, ")")))
      }

      # newformula <- eval(formula_expr)
    }

    if (.amp.acro_ind == -1) {
      left_part <- all.vars(.formula, max.names = 1)
    } else {
      left_part <- NULL
    }
    newformula <- stats::as.formula(paste(left_part, # rownames(attr(Terms, "factors"))[1],
      paste(c(attr(terms(.formula), "intercept"), non_acro_formula, formula_expr), collapse = " + "),
      sep = " ~ "
    ))
    newformula <- update.formula(newformula, ~.)
    # update the formula
    time_name <- paste(substitute(time_col, env))
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
      newdata = .data,
      newformula = newformula,
      vec_rrr = vec_rrr,
      vec_sss = vec_sss,
      n_components = n_components,
      period = period,
      group_stats = group_stats,
      group = group,
      group_check = group_check,
      time_name = time_name,
      response_var = left_part,
      group_original = group_original
    ))
  }
  amp.acro_iteration(
    time_col = time_col,
    n_components = n_components,
    group = group,
    .formula = .formula,
    period = period,
    quietly = quietly,
    .data = .data,
    .amp.acro_ind = .amp.acro_ind
  )
}




#' Checks that the group names supplied by the user are in the dataframe
#'
#' @param .data dataframe
#' @param group group argument specified in the cosinor.glmm call
#'
#' @return nothing if successful, an error message if not
#' @noRd


# (tested)
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
