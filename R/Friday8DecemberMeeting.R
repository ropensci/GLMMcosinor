amp.acro <- function(time_col, n_components = 1, group, .data, .formula) {
  # browser()
  ttt <- eval(substitute(time_col), env=.data) # extract vector of "time" values from .data
  return(ttt)


 # time_col is the name of the column in .data which should be used to calculate rrr and sss
 # create multiple rrr and sss columns (1 for each of 1:n_components)
 # add interactions with the group column
 # update .formula and return

 return(list(.data, .formula))
}



f <- function(formula, data) {

  # browser()
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


f(Y ~ X + amp.acro(time), data=vitamind)
#
## head(vitamind)
##> dplyr::select(head(vitamind), X, "Y")
#
#
