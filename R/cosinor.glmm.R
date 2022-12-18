#' Fit cosinor model with \code{{glmmTMB}}
#'
#' Given an outcome and time variable, fit the cosinor model with optional
#' covariate effects.
#'
#' @param formula Formula specifying the model. Indicate the time variable with
#'   \code{time()} and covariate effects on the amplitude and acrophase with
#'   \code{amp.acro()}. See details for more information.
#' @param period Length of time for a complete period of the sine curve.
#' @param data Data frame where variable can be found
#' @param family a family function, see \code{?family} and \code{?glmmTMB::nbinom2}
#' @param ... optional additional arguments passed to glmmTMB::glmmTMB()
#' @details This defines special functions that are used in the formula to
#'   indicate the time variable and which covariates effect the amplitude. To
#'   indicate the time variable wrap the name of it in the function
#'   \code{time()}. To indicate a variable which affects the
#'   acrophase/amplitude, wrap the name in \code{amp.acro()}. This will then do
#'   all the transformations for you. See examples for usage.
#'
#' @examples
#'
#' cosinor.glmm(Y ~ X + amp.acro(time, n_components = 3, group = "X", period = c(12, 8, 9)), data = vitamind)
#'
#' @references Tong, YL. Parameter Estimation in Studying Circadian Rhythms, Biometrics (1976). 32(1):85--94.
#'
#'
#' @export
#'


cosinor.glmm <- function(formula,
                         period = 12,
                         data,
                         family = gaussian(),
                         ...) {

  #Extract only the amp.acro function from the call
  Terms <- stats::terms(formula, specials = c("amp.acro"))
  amp.acro_text <- attr(Terms, "term.labels")[attr(Terms, "special")$amp.acro - 1]
  e <- str2lang(amp.acro_text)
  e$.data <- data # add data that will be called to amp.acro()
  e$.formula <- formula # add formula that will be called to amp.acro()
  updated_df_and_formula <- eval(e) # evaluate amp.acro call

  #Extract the data and updated formula from amp.acro()
  data <- updated_df_and_formula$data
  newformula <- updated_df_and_formula$formula
  vec_rrr <- updated_df_and_formula$vec_rrr #vector of rrr names for each component
  vec_sss <- updated_df_and_formula$vec_sss #vector of sss names for each component
  n_components <- updated_df_and_formula$n_components

  #Fit the data and formula to a model
  fit <- glmmTMB::glmmTMB(
    formula = newformula,
    data = data,
    family = family,
    ...
  )

  #Retrieve the fit, coefficients from the model and priming vectors
  #in preparation for transforming the raw coefficients
  mf <- fit
  coefs <- glmmTMB::fixef(mf)$cond
  r.coef <- NULL
  s.coef <- NULL
  mu.coef <- NULL
  mu_inv <- rep(0, length(names(coefs)))

  #Get a Boolean vector for rrr, sss, and mu. This will be used to extract
  #the relevant raw parameters from the raw coefficient model output
  for (i in 1:n_components) {
    r.coef[[i]] <- !is.na(str_extract(names(coefs),vec_rrr[i]))
    s.coef[[i]] <- !is.na(str_extract(names(coefs),vec_sss[i]))
    mu_inv_carry <- r.coef[[i]] + s.coef[[i]] #Keep track of non-mesor terms
    mu_inv <- mu_inv_carry + mu_inv #Ultimately,every non-mesor term will be true
  }

  mu.coef <- c(!mu_inv) #invert 'mu_inv' to get a Boolean vector for mesor terms
  r.coef <- (t(matrix(unlist(r.coef), ncol = length(r.coef)))) #a matrix of rrr coefficients
  s.coef <- (t(matrix(unlist(s.coef), ncol = length(s.coef)))) #a matrix of sss coefficients

  #Calculate the parameter estimates for all components
  amp <- NULL
  acr <- NULL
  for (i in 1:n_components) {
    beta.s <- coefs[s.coef[i, ]]
    beta.r <- coefs[r.coef[i, ]]

    groups.r <- c(beta.r[vec_rrr[i]], beta.r[vec_rrr[i]] + beta.r[which(names(beta.r) != vec_rrr[i])])
    groups.s <- c(beta.s[vec_sss[i]], beta.s[vec_sss[i]] + beta.s[which(names(beta.s) != vec_sss[i])])

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

  # Arrange and display the output
  structure(
    list(
      formula = newformula,
      fit = fit,
      Call = match.call(),
      Terms = Terms,
      coefficients = new_coefs,
      raw_coefficients = coefs,
      period = period
    ),
    class = "cosinor.glmm"
  )
}

#' Print cosinor model
#'
#' Given an outcome and time variable, fit the cosinor model with optional covariate effects.
#'
#' @param x cosinor.glmm object
#' @param ... passed to summary
#'
#'
#' @export
#'


print.cosinor.glmm <- function(x, ...) {
  cat("Call: \n")
  print(x$Call)
  cat("\n Raw formula: \n")
  print(x$formula)
  cat("\n Raw Coefficients: \n")
  print(x$raw_coefficients)
  cat("\n Transformed Coefficients: \n")
  t.x <- x$coefficients
  # names(t.x) <- update_covnames(names(t.x))
  print(t.x)
}

#' Fit cosinor model
#'
#' Given an outcome and time variable, fit the cosinor model with optional covariate effects.
#'
#' @param formula Forumla specifying the model. Indicate the time variable with \code{time()} and covariate effects on the
#' amplitude and acrophase with \code{amp.acro()}. See details.
#' @param ... other arguments
#'
#' @details This defines special functions that are used in the formula to indicate the time variable
#' and which covariates effect the amplitude. To indicate the time variable wrap the name of it in the function
#' \code{time()}. To indicate a variable which affects the acrophase/amplitude, wrap the name in
#' \code{amp.acro()}. This will then do all the transformations for you. See examples for usage.
#'
#' @examples
#'
#' cosinor.glmm(Y ~ time(time) + X + amp.acro(X), data = vitamind)
#'
#' @export
#'


cosinor.glmm.default <- function(formula, ...) {
  UseMethod("cosinor.glmm")
}

#' Extract variable names from terms object, handling specials
#'
#' @param Terms a terms object
#'
#' @keywords Internal
#'

get_varnames <- function(Terms) {
  spec <- names(attr(Terms, "specials"))
  tname <- attr(Terms, "term.labels")

  dex <- unlist(sapply(spec, function(sp) {
    attr(Terms, "specials")[[sp]] - 1
  }))

  tname2 <- tname
  for (jj in spec) {
    gbl <- grep(paste0(jj, "("), tname2, fixed = TRUE)
    init <- length(gbl) > 0
    if (init) {
      jlack <- gsub(paste0(jj, "("), "", tname2, fixed = TRUE)
      tname2[gbl] <- substr(jlack[gbl], 1, nchar(jlack[gbl]) - 1)
    }
  }
  tname2
}

#' Replace covariate names with descriptive text
#'
#' @param names Coefficient names to update
#'
#' @export
#'

update_covnames <- function(names) {
  covnames <- grep("(amp|acr|Intercept)", names, invert = TRUE, value = TRUE)

  lack <- names
  for (n in covnames) {
    lack <- gsub(paste0(n, ":"), paste0("[", n, " = 1]:"), lack)
    lack <- gsub(paste0("^", n, "$"), paste0("[", n, " = 1]"), lack)
  }
  lack
}


amp.acro <- function(time_col, n_components = 1, group, .data, .formula, period = 12) {
  ttt <- eval(substitute(time_col), env = .data) # extract vector of "time" values from .data
  #Test for whether n_components is an integer greater than 0
  if (n_components %% 1 != 0| n_components < 1) {
      stop("Number of components (n_components) must be an integer greater than 0")
  }

  #Test for whether the length of the grouping variable matches the value of n_components.
  #If one grouping variable is supplied but n_components > 1, then the one grouping
  #variable is repeated to match the value of n_components
  #match the length of n_components
  if (length(group) != n_components) {
    if (length(group) == 1) {
      group <- rep(group, n_components)
    } else {
      stop("grouping variable in amp.acro() must be of length 1 or the same as n_components.")
    }
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

  ###
  #iter_df <- data.frame(
  #  component = 1:n_components,
  #  group = group,
  #  period = period
  #)
  ###

  #Checks for NA group values supplied by the user and replaces with zeroes.
  #This is important when creating the formula: 'newformula'.
  for (i in seq_along(length(group))) {
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

  return(list(data = .data, formula = newformula, vec_rrr = vec_rrr, vec_sss = vec_sss, n_components = n_components))
}
