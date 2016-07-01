#' @title Get ACS variable names from text.
#' @description The \code{getvars} function gets ACS variable names from text containing formulas with addition, substraction, and/or division operators.
#' @param formula Character or vector of characters with formulas using ACS variable names.
#' @return Returns a vector with uppercase ACS variables names.
#' @details Text can be upper or lower case, include parentheses or "* 100".
#' @examples
#' getvars("b16004_004 + b16004_026 + b16004_048 / b16004_001")
getvars <- function(formula) {
constr <- gsub("\\(|\\)", "", formula) #
constr <- gsub("\\* 100", "", constr)
vars  <- unlist(strsplit(constr, "[\\+]|[\\-]|[\\/]|[\\*]"))
vars <- gsub("[[:space:]]", "", vars)
vars <- toupper(vars) # I am getting all variables
vars <- vars[!duplicated(vars)]
return(vars)
}
