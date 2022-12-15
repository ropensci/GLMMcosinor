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
#Examples for meeting: 15/12/2022
#Evidence that the multi-component estimation works. In this example, amp2 = amp1 + 2, acr2 = acr1-1.
  #comod = simulate_cosinor(1000,mesor = 1,amp = 2,acro = 3,beta.mesor = 0.1,beta.amp = 0.2, beta.acro = 0.3, dist = "2_component")
  #cosinor.glmm(Y ~ group+amp.acro(times, n_components = 2, group = "group", period = c(12, 8)), data = comod)
#
#You can now have different groups assigned to different components. For example:
  #vitamind$Z = rbinom(length(vitamind$X),1,prob = 0.5)
  #cosinor.glmm(Y~ X + amp.acro(time, n_components = 3, group = c("X",NA,"Z"), period = c(12,10,8)),data = vitamind)


cosinor.glmm <- function(formula,
                         period = 12,
                         data,
                         family = gaussian(),
                         ...) {
  ## build time transformations
  Terms <- stats::terms(formula, specials = c("amp.acro"))
  special_text <- attr(Terms, "term.labels")[attr(Terms, "special")$amp.acro - 1]

  e <- str2lang(special_text)
  e$.data <- data # add data to call to amp.acro()
  e$.formula <- formula # add formula to call to amp.acro()


  updated_df_and_formula <- eval(e) # evaluate amp.acro call




  # update these to comef rom updated_df_and_formula
  data <- updated_df_and_formula$data
  newformula <- updated_df_and_formula$formula
  vec_rrr <- updated_df_and_formula$vec_rrr
  vec_sss <- updated_df_and_formula$vec_sss
  data <- stats::model.frame(newformula, data)
  fit <- glmmTMB::glmmTMB(
    formula = newformula,
    data = data,
    family = family,
    ...
  )

  # Get a truth array for the coefficients
  mf <- fit
  r.coef <- NULL
  s.coef <- NULL
  mu.coef <- NULL
  for (jj in 1:length(vec_rrr)) {
    r.coef[[jj]] <- c(FALSE, as.logical(attr(mf$modelInfo$terms$cond$fixed, "factors")[vec_rrr[jj], ]))
    s.coef[[jj]] <- c(FALSE, as.logical(attr(mf$modelInfo$terms$cond$fixed, "factors")[vec_sss[jj], ]))
  }

  mu_inv <- rep(0, length(s.coef[[1]]))
  for (jj in 1:length(vec_sss)) {
    mu_inv_carry <- r.coef[[jj]] + s.coef[[jj]]
    mu_inv <- mu_inv_carry + mu_inv
  }

  mu.coef <- c(!mu_inv)
  s.coef <- (t(matrix(unlist(s.coef), ncol = length(s.coef))))
  r.coef <- (t(matrix(unlist(r.coef), ncol = length(r.coef))))
  coefs <- glmmTMB::fixef(mf)$cond

  # calculate the parameter estimates
  amp <- NULL
  acr <- NULL
  for (jj in 1:length(vec_rrr)) {
    beta.s <- coefs[s.coef[jj, ]]
    beta.r <- coefs[r.coef[jj, ]]

    groups.r <- c(beta.r[vec_rrr[jj]], beta.r[vec_rrr[jj]] + beta.r[which(names(beta.r) != vec_rrr[jj])])
    groups.s <- c(beta.s[vec_sss[jj]], beta.s[vec_sss[jj]] + beta.s[which(names(beta.s) != vec_sss[jj])])

    amp[[jj]] <- sqrt(groups.r^2 + groups.s^2)
    names(amp[[jj]]) <- gsub(vec_rrr[jj], paste0("amp", jj), names(beta.r))

    acr[[jj]] <- -atan2(groups.s, groups.r)
    names(acr[[jj]]) <- gsub(vec_sss[jj], paste0("acr", jj), names(beta.s))
  }
  new_coefs <- c(coefs[mu.coef], unlist(amp), unlist(acr))

  # if n_components = 1, then print "amp" and "acr" rather than "amp1", "acr1"
  if (length(vec_rrr) == 1) {
    names(amp[[1]]) <- gsub(vec_rrr[1], "amp", names(beta.r))
    names(acr[[1]]) <- gsub(vec_sss[1], "acr", names(beta.s))
    new_coefs <- c(coefs[mu.coef], unlist(amp), unlist(acr))
  }
  browser()

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

  # c(attr(Terms, "intercept"), tname2)
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
  # browser()

  if(length(group) != n_components){
    if(length(group) == 1) {
      group <- rep(group, n_components)
    } else {
      stop("grouping variable in amp.acro() must be of length 1 or the same as n_components.")
    }
  }

  if(length(period) != n_components){
    if(length(period) == 1) {
      period <- rep(period, n_components)
    } else {
      stop("period value(s) in amp.acro() must be of length 1 or the same as n_components.")
    }
  }
  iter_df <- data.frame(
    component = 1:n_components,
    group = group,
    period = period
  )

  # replace everything below here to occur within a for loop which iteratively
  # (1) adds on columns to the .data
  # (2) adds covariates to the formula
  # (3) adds to the vec_rrr and vec_sss
  # ... for each additional component

####
#####
i=1
for (i in 1:length(group)) {
  if (is.na(group[i]) == TRUE)
    group[i] = 0
}
group_names <- group[group != 0]

Terms <- stats::terms(.formula)
varnames <- get_varnames(Terms)
newformula <- stats::as.formula(paste(rownames(attr(Terms, "factors"))[1],
                                      paste(c(attr(terms(.formula), "intercept"),group_names), collapse = " + "),
                                      sep = " ~ "))
# if n_components = 1, generate a vector of rrr and sss
  i <- 1
  if (n_components == 1) {
    .data$rrr <- cos(2 * pi * ttt / period[i])
    .data$sss <- sin(2 * pi * ttt / period[i])
    vec_rrr <- "rrr"
    vec_sss <- "sss"
    acpart <- paste((rep(group[i], 2)), c(vec_rrr, vec_sss), sep = ":")
    acpart_comb <- paste(acpart[1],acpart[2], sep = " + ")
    form_expr <- str2expression(noquote(paste("update.formula(newformula, .~. +",vec_rrr,"+", vec_sss,
                                              "+",acpart_comb,")")))
    newformula = eval(form_expr)
  }
  #

  # if n_components > 1, generate n_components number of rrr and sss vectors
  if (n_components != 1) {
    n_count <- 1:n_components
    vec_rrr <- (paste0("rrr", n_count))
    vec_sss <- (paste0("sss", n_count))

    # adding the rrr and sss columns to the dataframe
    i <- 1
    for (i in 1:n_components) {
      rrr_names <- eval(vec_rrr[i])
      sss_names <- eval(vec_sss[i])
      .data[[rrr_names]] <- cos(2 * pi * ttt / period[i])
      .data[[sss_names]] <- sin(2 * pi * ttt / period[i])
        if (group[i] != 0) {
        acpart <- paste((rep(group[i], 2)), c(rrr_names, sss_names), sep = ":")
        acpart_comb <- paste(acpart[1],acpart[2], sep = " + ")
        form_expr <- str2expression(noquote(paste("update.formula(newformula, .~. +",rrr_names,"+", sss_names,
                                                  "+",acpart_comb,")")))
        }

        if (group[i] == 0) {
          acpart_comb <- NULL
          form_expr <- str2expression(noquote(paste("update.formula(newformula, .~. +",rrr_names,"+", sss_names,")")))
        }

      newformula = eval(form_expr)

    }
    newformula = update.formula(newformula, ~.)
  }
#update the formula
#####
####
  return(list(data=.data, formula=newformula, vec_rrr=vec_rrr, vec_sss=vec_sss))
}
