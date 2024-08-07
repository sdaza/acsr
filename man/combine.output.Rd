% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/combine_output.R
\name{combine.output}
\alias{combine.output}
\title{Estimate proportions, ratios, aggregations and the respective margins
  of error (MOEs) of custom combinations of geographic units.}
\usage{
combine.output(
  formula = formula,
  varname = varname,
  method = method,
  level = level,
  combine.names,
  dataset = "acs",
  endyear = 2014,
  span = 5,
  conf.level = 0.9,
  one.zero = TRUE,
  trace = TRUE,
  format.out = "wide",
  file = NULL,
  print.levels = TRUE,
  region = "*",
  division = "*",
  state = "WI",
  county = "*",
  county.subdivision = "*",
  place = "*",
  tract = "*",
  block.group = "*",
  msa = "*",
  csa = "*",
  necta = "*",
  urban.area = "*",
  congressional.district = "*",
  state.legislative.district.upper = "*",
  state.legislative.district.lower = "*",
  puma = "*",
  zip.code = "*",
  american.indian.area = "*",
  school.district.elementary = "*",
  school.district.secondary = "*",
  school.district.unified = "*"
)
}
\arguments{
\item{formula}{A character or vector of characters containing formulas using
ACS variables or just variables. + - operators can be included. / defines a
division. When the formula contains "* 100", the final statistic and MOE is
multiply by 100.}

\item{varname}{A character or vector of characters containing the new
variables to be created. This vector must have same length as
\code{formula} and \code{method}, and it is not needed when getting only
variables.}

\item{method}{A character or vector of characters defining the type of
estimate expected: "proportion", "ratio", "aggregation", "variables". This
vector must have same length as \code{formula} and \code{varname}. It is
not needed when getting only variables.}

\item{level}{A list specifying the geographic level of the data. It may be necessary to specify values to the
  corresponding levels. For instance, when \code{level = list("county")}, you have
  to specify a state (e.g., \code{state = list("WI")}, the default state in this
  package), and the counties you want to combine: \code{county = list(1,2)}. They may
  be also nested lists (e.g., \code{county = list( list(1:2, 3:4), list(5:6, 7:8) )}).
  The number of elements of the level list should be the same as \code{combine.names)} (names of combined groups, see below).

  The required combinations of different summary levels are:

  \cr 010 us \cr 020 region \cr 030 division \cr 040 state \cr 050 state,
  county \cr 060 state, county, county.subdivision \cr 140 state, county,
  tract \cr 150 state, county, tract, block.group \cr 160 state, place \cr
  250 american.indian.area \cr 320 state, msa \cr 340 state, csa \cr 350
  necta \cr 400 urban.area \cr 500 state, congressional.district \cr 610
  state, state.legislative.district.upper \cr 620 state,
  state.legislative.district.lower \cr 795 state, puma \cr 860 zip.code \cr
  950 state, school.district.elementary \cr 960 state,
  school.district.secondary \cr 970 state, school.district.unified \cr}

\item{combine.names}{Labels for the aggregate geographies.
It should be the same as the number of elements of the list \code{level}.}

\item{dataset}{A string or vector of strings specifying the data set to be used: acs, sf1 or sf1.
The default value is "acs".}

\item{endyear}{An integer or vector of integers (defaults to 2014) indicating the latest year of
the data in the survey or Census year.}

\item{span}{An integer indicating the span (in years) of the desired ACS data
(should be 1, 3, or 5), defaults to 5.}

\item{conf.level}{Confidence level to estimate MOEs. The default value is
0.90.}

\item{one.zero}{Whether to include standard errors for only one zero-value
(max value) of columns or all. The default is TRUE.}

\item{trace}{Shows progress of the variable creation. The default is TRUE.}

\item{format.out}{Format of the output: "wide" or "long". The default is
"wide".}

\item{file}{The resulting output is exported to a CSV file rather than to the
R prompt. The file name must be specified as a character string.}

\item{print.levels}{Boolean that print levels generated by the \code{geo.make} function.}

\item{data}{Input data generated by the \code{\link{acsdata}} function.
Variables and levels must be the same as those specified in the
\code{sumacs} function.}
}
\value{
Returns a \code{data.table/data.frame} object with the estimates and
  MOEs.
}
\description{
Using the \code{sumacs} function, it  computes proportions, ratios and
  aggregations based on text formulas, or simply extract variables of combinations of geographic units.
  It uses the same procedures of the \code{sumacs} function, but the geographic level and
  information have to be specified in lists (they can be nested).
}
\details{
When the standard error of a proportion cannot be estimated, the
  "ratio" option is used. This adjustment is done row by row.
}
\note{
Depending on the quality of the Internet connection, number of
  variables and levels, getting the ACS data can be slow, especially for the
  levels "county.subdivision", "block.group", and "tract" (it might take more
  than 30 minutes).  It is recommended to get the data using the function
  \code{\link{acsdata}} first, and then to use \code{sumacs}.
}
\examples{
# api.key.install(key="*")
  combine.output("(b16004_004 + b16004_026 + b16004_048 / b16004_001)",
    varname = "myvar",
    method = "prop",
    level = list("division", "division"),
    division = list(list(1:2, 3), list(1:2, 3:4)),
    combine.names = c("g1", "g2"))
}
