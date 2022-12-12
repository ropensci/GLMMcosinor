library(rlang)
library(lobstr)
data(vitamind)

###
amp.acro <- function(time_col, n_components = 1, group, .data, .formula, period = 12) {
  ttt <- eval(substitute(time_col), env=.data) # extract vector of "time" values from .data
  #browser()
  # if n_components = 1, generate a vector of rrr and sss
  i = 1
  if (n_components ==1) {
    .data$rrr <- cos(2 * pi * ttt/ period[i])
    .data$sss <- sin(2 * pi * ttt/ period[i])
    vec_rrr <- "rrr"
    vec_sss <- "sss"
  }
  #

  #if n_components > 1, generate n_components number of rrr and sss vectors
  if (n_components != 1) {
    n_count = 1:n_components
    vec_rrr <- (paste0("rrr",n_count))
    vec_sss <- (paste0("sss",n_count))

    #adding the rrr and sss columns to the dataframe
    i = 1
    for (i in 1:n_components){
      rrr_names <- eval(vec_rrr[i])
      sss_names <- eval(vec_sss[i])
      .data[[rrr_names]] <- cos(2 * pi * ttt/ period[i])
      .data[[sss_names]] <- sin(2 * pi * ttt/ period[i])
    }

  }

  #update the formula
  Terms <- stats::terms(.formula, specials = c("time", "amp.acro"))
  varnames <- get_varnames(Terms)
  spec_dex <- unlist(attr(Terms, "special")$amp.acro) - 2
  mainpart <- c(varnames[c(-2,0)],vec_rrr,vec_sss)
  #mainpart <- c(vec_rrr, vec_sss)
  acpart <- paste(sort(rep(varnames[spec_dex],2)),c(vec_rrr,vec_sss),sep = ":")
  newformula <- stats::as.formula(paste(rownames(attr(Terms, "factors"))[1],
                                        paste(c(attr(terms(.formula), "intercept"), mainpart, acpart), collapse = " + "),
                                        sep = " ~ "
  ))

  # time_col is the name of the column in .data which should be used to calculate rrr and sss
  # create multiple rrr and sss columns (1 for each of 1:n_components)
  # add interactions with the group column
  # update .formula and return

  return(list(.data, newformula,vec_rrr,vec_sss))
}


f <- function(formula, data, period) {
  Terms <- stats::terms(formula, specials = c("time", "amp.acro"))

  get_varnames(Terms)
  attr(Terms, "special")$amp.acro
  special_text <- attr(Terms,"term.labels")[attr(Terms, "special")$amp.acro - 1]

  e <- str2lang(special_text)
  e$.data <- data # add data to call to amp.acro()
  e$.formula <- formula # add formula to call to amp.acro()


  updated_df_and_formula <- eval(e) # evaluate amp.acro call
  updated_df_and_formula
}
###

f(Y ~ X + amp.acro(time,  n_components = 4, period = c(1,2,3,4)), data=vitamind)
#cosinor.glmm(Y ~ X + amp.acro(time,  n_components = 2, group = "X", period = c(12,8)), data=vitamind)

#
## head(vitamind)
##> dplyr::select(head(vitamind), X, "Y")
#


###
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
###
