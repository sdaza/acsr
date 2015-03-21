#' Add together two numbers.
#'
#' @param x A number.
#' @param y A number.
#' @return The sum of \code{x} and \code{y}.
#' @examples
#' add(1, 1)
#' add(10, 1)

getvars <- function(formula) {

# formula : text, + - are needed, / define a numerator and denominator
# example: b16004_004 + b16004_026 + b16004_048 / b16004_001
constr <- gsub("\\(|\\)", "", formula) #
constr <- gsub("\\* 100", "", constr)
vars  <- unlist(strsplit(constr, "[\\+]|[\\-]|[\\/]|[\\*]"))
vars <- gsub("[[:space:]]", "", vars)
vars <- toupper(vars)
vars <- vars[!duplicated(vars)]
return(vars)
}
