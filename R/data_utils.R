#' Update data and formula for fitting cosinor.glmm model
#'
#' @param data input data for fitting cosinor.glmm model.
#' @param formula model formula, specified by user including \code{amp.acro()}.
#' @param family the model family.
#' @param quietly controls whether messages from amp.acro are displayed. TRUE by default
#' @param dispformula The formula specifying the dispersion model
#' @param ziformula The formula specifying the zero-inflation model
#'
#' @srrstats {G2.13} *Statistical Software should implement appropriate checks for missing data as part of initial pre-processing prior to passing data to analytic algorithms.*
#' @srrstats {G2.14b} *ignore missing data with default warnings or messages issued*
#' @srrstats {G1.4} *Software should use [`roxygen2`](https://roxygen2.r-lib.org/) to document all functions.*
#' @srrstats {G2.6} *Software which accepts one-dimensional input should ensure values are appropriately pre-processed regardless of class structures.*
#' @srrstats {G2.7} *Software should accept as input as many of the above standard tabular forms as possible, including extension to domain-specific forms.*
#'
#' @srrstats {RE2.0} *Regression Software should document any transformations applied to input data, for example conversion of label-values to `factor`, and should provide ways to explicitly avoid any default transformations (with error or warning conditions where appropriate).*
#' @srrstats {RE2.1} *Regression Software should implement explicit parameters controlling the processing of missing values, ideally distinguishing `NA` or `NaN` values from `Inf` values (for example, through use of `na.omit()` and related functions from the `stats` package).*
#' @srrstats {G2.8} *Software should provide appropriate conversion or dispatch routines as part of initial pre-processing to ensure that all other sub-functions of a package receive inputs of a single defined class or type.*
#' @srrstats {G2.9} *Software should issue diagnostic messages for type conversion in which information is lost (such as conversion of variables from factor to character; standardisation of variable names; or removal of meta-data such as those associated with [`sf`-format](https://r-spatial.github.io/sf/) data) or added (such as insertion of variable or column names where none were provided).*
#' @srrstats {G2.10} *Software should ensure that extraction or filtering of single columns from tabular inputs should not presume any particular default behaviour, and should ensure all column-extraction operations behave consistently regardless of the class of tabular data used as input.*
#' @srrstats {G2.13} *Statistical Software should implement appropriate checks for missing data as part of initial pre-processing prior to passing data to analytic algorithms.*

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


  items_keep <- c(
    "newformula",
    "vec_rrr",
    "vec_sss",
    "n_components",
    "period",
    "group_stats",
    "group_check",
    "group"
  )

  if (dispformula_check) {
    data <- main_output$newdata
    dispformula <- formula_eval(formula = dispformula, data = data, quietly = quietly, amp.acro_ind = 0, data_prefix = "disp_")
    main_output$newdata <- dispformula$newdata

    dispformula <- dispformula[items_keep]
    names(dispformula)[names(dispformula) == "newformula"] <- "formula"
    main_output$dispformula <- dispformula
  }
  if (ziformula_check) {
    data <- main_output$newdata
    ziformula <- formula_eval(formula = ziformula, data = data, quietly = quietly, amp.acro_ind = 0, data_prefix = "zi_")
    main_output$newdata <- ziformula$newdata

    ziformula <- ziformula[items_keep]
    names(ziformula)[names(ziformula) == "newformula"] <- "formula"
    main_output$ziformula <- ziformula
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
#'
#' @srrstats {G1.4a} *All internal (non-exported) functions should also be documented in standard [`roxygen2`](https://roxygen2.r-lib.org/) format, along with a final `@noRd` tag to suppress automatic generation of `.Rd` files.*
#' @srrstats {G2.0} *Implement assertions on lengths of inputs, particularly through asserting that inputs expected to be single- or multi-valued are indeed so.*
#' @srrstats {G2.1} *Implement assertions on types of inputs (see the initial point on nomenclature above).*
#' @srrstats {G2.2} *Appropriately prohibit or restrict submission of multivariate input to parameters expected to be univariate.*
#
#' @srrstats {G2.4} *Provide appropriate mechanisms to convert between different data types, potentially including:*
#' @srrstats {G2.4a} *explicit conversion to `integer` via `as.integer()`*
#' @srrstats {G2.4b} *explicit conversion to continuous via `as.numeric()`*
#' @srrstats {G2.4c} *explicit conversion to character via `as.character()` (and not `paste` or `paste0`)*
#' @srrstats {G2.4d} *explicit conversion to factor via `as.factor()`*
#' @srrstats {G2.5} *Where inputs are expected to be of `factor` type, secondary documentation should explicitly state whether these should be `ordered` or not, and those inputs should provide appropriate error or other routines to ensure inputs follow these expectations.*
#'
#'
#' @srrstats {G2.9} *Software should issue diagnostic messages for type conversion in which information is lost (such as conversion of variables from factor to character; standardisation of variable names; or removal of meta-data such as those associated with [`sf`-format](https://r-spatial.github.io/sf/) data) or added (such as insertion of variable or column names where none were provided).*
#' @srrstats {G2.6} *Software which accepts one-dimensional input should ensure values are appropriately pre-processed regardless of class structures.*
#'
#' @srrstats {G5.0} *Where applicable or practicable, tests should use standard data sets with known properties (for example, the [NIST Standard Reference Datasets](https://www.itl.nist.gov/div898/strd/), or data sets provided by other widely-used R packages).*
#'
#' @srrstats {RE1.2} *Regression Software should document expected format (types or classes) for inputting predictor variables, including descriptions of types or classes which are not accepted.*
#'
#'
#' @return updated dataframe and formula to then be processed by data_processor()

amp.acro <- function(time_col,
                     n_components = 1,
                     group,
                     .data,
                     .formula,
                     period,
                     .quietly = TRUE,
                     .amp.acro_ind = -1,
                     .data_prefix = "main_") {
  # checking dataframe


  ## Check if 'group' is a non-string and convert it to a string if necessary
  #if (!is.character(group)) {
  #  group <- as.character(group)
  #}


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
  amp.acro_iteration <- function(time_col,
                                 n_components,
                                 group,
                                 .formula,
                                 period,
                                 .quietly = TRUE,
                                 .data,
                                 .amp.acro_ind = -1) {


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
    ttt <- eval(substitute(time_col, env), envir = .data) # extract vector of "time" values from .data

    # ensure ttt contains numeric values only (tested)
    if (!assertthat::assert_that(is.numeric(ttt))) {
      stop("time column in dataframe must contain numeric values")
    }


    # ensure time_col is univariate (tested)
    assertthat::assert_that(is.vector(ttt),
      msg = "time_col must be univariate"
    )

    # Check if 'group' is a non-string and convert it to a string if necessary
    if (all(!is.character(substitute(group,env))) & !missing(group)) {
      group_change <- as.character(substitute(group,env))
      if(length(group_change) != 1) {
        group <- group
      }
      else {
        group <- group_change
      }
    }

    # allow the user to not have any grouping structure (if group argument is missing)
    if (missing(group)) {
      group <- 0
      group_check <- FALSE
    } else {
      if (all(is.na(group)) | all(is.null(group))) {
        group <- 0
        group_check <- FALSE
      } else {
        group_check <- TRUE
        check_group_var(.data = .data, group = group)
      }
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
    for (i in seq_along(group)) {
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
    Terms <- stats::terms(.formula, specials = "amp.acro")
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
      paste(c(attr(stats::terms(.formula), "intercept"), non_acro_formula, formula_expr), collapse = " + "),
      sep = " ~ "
    ))
    newformula <- stats::update.formula(newformula, ~.)

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
  res <- amp.acro_iteration(
    time_col = time_col,
    n_components = n_components,
    group = group,
    .formula = lme4::nobars(.formula),
    period = period,
    .quietly = .quietly,
    .data = .data,
    .amp.acro_ind = .amp.acro_ind
  )

  if (!is.null(lme4::findbars(.formula))) {
    ranef_part <- sapply(lme4::findbars(.formula), deparse1)
    ranef_parts_replaced <- lapply(ranef_part, function(x) {
      component_num <- regmatches(x, regexpr("(?<=amp\\.acro)[0-9]+", x, perl = TRUE))
      if (length(component_num) == 0) {
        return(x)
      }

      gsub("amp\\.acro[0-9]+", paste0("main_rrr", component_num, "+", "main_sss", component_num), x, perl = TRUE)
    })

    ranef_part_updated <- paste(sprintf("(%s)", ranef_parts_replaced), collapse = "+")

    main_part <- paste(paste(deparse(res$newformula), collapse = ""), ranef_part_updated, collapse = "", sep = "+")
    res$newformula <- stats::as.formula(main_part)
  }

  # As a test:
  # obj <- cosinor.glmm(Y ~ X + amp.acro(time,
  # n_components = 3,
  # group = "X",
  # period = c(12, 8, 9)
  # ) + (1|amp.acro1) + (X|amp.acro2), data = vitamind)

  res
}




#' Checks that the group names supplied by the user are in the dataframe
#'
#' @param .data dataframe
#' @param group group argument specified in the cosinor.glmm call
#' @srrstats {G1.4a} *All internal (non-exported) functions should also be documented in standard [`roxygen2`](https://roxygen2.r-lib.org/) format, along with a final `@noRd` tag to suppress automatic generation of `.Rd` files.*
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

#' Checks that the `ci_level` values provided are reasonable
#'
#' @param ci_level Confidence level.
#' @srrstats {G1.4a} *All internal (non-exported) functions should also be documented in standard [`roxygen2`](https://roxygen2.r-lib.org/) format, along with a final `@noRd` tag to suppress automatic generation of `.Rd` files.*
#'
#' @return nothing if successful, an error message if not
#' @noRd

validate_ci_level <- function(ci_level) {
  assertthat::is.number(ci_level)

  if (ci_level < 0 || ci_level > 1) {
    stop("'ci_level' must be a single numeric value in [0, 1].")
  }
}

# calculate the parameters from the raw estimates
get_new_coefs <- function(coefs, vec_rrr, vec_sss, n_components) {
  r.coef <- NULL
  s.coef <- NULL
  mu.coef <- NULL
  mu_inv <- rep(0, length(names(coefs)))


  # Get a Boolean vector for rrr, sss, and mu. This will be used to extract
  # the relevant raw parameters from the raw coefficient model output
  for (i in 1:n_components) {
    r.coef[[i]] <- grepl(paste0(vec_rrr[i]), names(coefs))
    s.coef[[i]] <- grepl(paste0(vec_sss[i]), names(coefs))

    mu_inv_carry <- r.coef[[i]] + s.coef[[i]] # Keep track of non-mesor terms
    mu_inv <- mu_inv_carry + mu_inv # Ultimately,every non-mesor term will be true
  }

  mu.coef <- c(!mu_inv) # invert 'mu_inv' to get a Boolean vector for mesor terms
  r.coef <- (t(matrix(unlist(r.coef), ncol = length(r.coef)))) # a matrix of rrr coefficients
  s.coef <- (t(matrix(unlist(s.coef), ncol = length(s.coef)))) # a matrix of sss coefficients

  # Calculate the parameter estimates for all components
  amp <- NULL
  acr <- NULL
  for (i in 1:n_components) {
    beta.s <- coefs[s.coef[i, ]]
    beta.r <- coefs[r.coef[i, ]]

    groups.r <- c(beta.r[1], beta.r[which(names(beta.r) != names(beta.r[1]))])
    groups.s <- c(beta.s[1], beta.s[which(names(beta.s) != names(beta.s[1]))])

    amp[[i]] <- sqrt(groups.r^2 + groups.s^2)
    names(amp[[i]]) <- gsub(vec_rrr[i], paste0("amp", i), names(beta.r))

    acr[[i]] <- -atan2(groups.s, groups.r)
    names(acr[[i]]) <- gsub(vec_sss[i], paste0("acr", i), names(beta.s))
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
