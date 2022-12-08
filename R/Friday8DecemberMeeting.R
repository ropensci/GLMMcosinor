#amp.acro <- function(time_col, n_components = 1, group, .data, .formula) {
#  browser()
#
#  # time_col is the name of the column in .data which should be used to calculate rrr and sss
#  # create multiple rrr and sss columns (1 for each of 1:n_components)
#  # add interactions with the group column
#  # update .formula and return
#
#  return(list(.data, .formula))
#}
#
#
#
#f <- function(formula, data) {
#  browser()
#  Terms <- stats::terms(formula, specials = c("time", "amp.acro"))
#  # browser()
#  get_varnames(Terms)
#  attr(Terms, "special")$amp.acro
#  special_text <- attr(Terms,"term.labels")[attr(Terms, "special")$amp.acro - 1]
#  special_text <- gsub(pattern=")", replacement = ", .data=data, .formula=formula)", special_text)
#
#  eval(parse(text=paste0("updated_df_and_formula <- ", special_text)))
#  updated_df_and_formula
#}
#
#
#f(Y ~ X + amp.acro(time), data=vitamind)
#
## head(vitamind)
##> dplyr::select(head(vitamind), X, "Y")
#
#
