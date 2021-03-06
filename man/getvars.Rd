% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_vars.R
\name{getvars}
\alias{getvars}
\title{Get ACS variable names from text.}
\usage{
getvars(formula)
}
\arguments{
\item{formula}{Character or vector of characters with formulas using ACS variable names.}
}
\value{
Returns a vector with uppercase ACS variables names.
}
\description{
The \code{getvars} function gets ACS variable names from text containing formulas with addition, substraction, and/or division operators.
}
\details{
Text can be upper or lower case, include parentheses or "* 100".
}
\examples{
getvars("b16004_004 + b16004_026 + b16004_048 / b16004_001")
}
