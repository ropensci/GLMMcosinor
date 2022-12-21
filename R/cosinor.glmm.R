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
  updated_df_and_formula <- update_formula_and_data(data = data, formula = formula)
  #Extract the data and updated formula from amp.acro()
  data <- updated_df_and_formula$data #data from amp.acro()
  newformula <- updated_df_and_formula$formula #formula from amp.acro()
  vec_rrr <- updated_df_and_formula$vec_rrr #vector of rrr names for each component
  vec_sss <- updated_df_and_formula$vec_sss #vector of sss names for each component
  n_components <- updated_df_and_formula$n_components #number of model components
  group_stats <- updated_df_and_formula$group_stats #a list of levels per group
  group <- updated_df_and_formula$group #group arguments repeated to match n_components
  group_check <- updated_df_and_formula$group_check #TRUE if there was a group argument in amp.acro()

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
    r.coef[[i]] <- grepl(paste0(vec_rrr[i], "$"), names(coefs))
    s.coef[[i]] <- grepl(paste0(vec_sss[i], "$"), names(coefs))

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
      Terms = updated_df_and_formula$Terms,
      coefficients = new_coefs,
      raw_coefficients = coefs,
      period = period,
      group_stats = group_stats,
      group = group,
      group_check = group_check
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
  if (x$group_check == TRUE) {
  names(t.x) <- update_covnames(names(t.x), group_stats = x$group_stats, group = x$group)
  }
  print(t.x)
}

#' Fit cosinor model
#'
#' Given an outcome and time variable, fit the cosinor model with optional covariate effects.
#'
#' @param formula Formula specifying the model. Indicate the time variable with \code{time()} and covariate effects on the
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

update_covnames <- function(names, group_stats, group) {
  #Present the covariate names with descriptive text
  group_names <- names(group_stats) #get the group names
  group_stats_without_first <- NULL # a vector without the first level of each group
  for (i in group_names) {
    group_stats_without_first[[i]] = group_stats[[i]][-1]
  }

  # get the names of the covariates alone
  covnames <- grep("(amp|acr|Intercept)", names, invert = TRUE, value = TRUE)

  # get the names that covnames does not get:
  covnames_inv <- grep(paste0("(Intercept|",paste(covnames, collapse = "|"),")"), invert = TRUE, names, value = TRUE)
  lack <- names
    for (i in 1:length(covnames)) {
      var <- str_extract(covnames[i],group_names) #get the group name from covnames
      var <- var[!is.na(var)] # remove NA values
      var_number = unlist(group_stats_without_first)[[i]] #get the group level
      lack <- gsub(paste0(covnames[i], ":"), paste0("[",var, "=",var_number,"]:"), lack)
      lack <- gsub(paste0("^", covnames[i], "$"), paste0("[",var, "=",var_number,"]"), lack)
    }
  #get a vector of group names repeated so that the length matches that of covnames_inv
  var = rep(group,length(covnames_inv)/length(group))
  # name the reference group of each covariate accordingly
    for (i in 1:length(covnames_inv)) {
      if (var[i] != 0) {
      var_number <- group_stats[[var[i]]][1]
      lack <- gsub(paste0("^",covnames_inv[i],"$"),paste0("[",var[i],"=",var_number,"]:",covnames_inv[i]),lack)
      }
      }
  lack
}


