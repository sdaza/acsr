#' acsr: A package with wrapped functions to manage ACS files.
#'
#' @description Wrapped functions to download, manage, and presenting data from the U.S. Census American Community Survey (ACS).
#'
#' @author Sebastian Daza \email{sdaza@@ssc.wisc.edu}
#'
#' @section Functions:
#'
#' \itemize{
#'   \item \code{\link{acsdata}}: Create a list containing ACS data files to be used with the function  \code{\link{sumacs}}.
#'   \item \code{\link{sumacs}}: Estimate proportions, ratios, and aggregations with their respective MOEs.
#'   \item \code{\link{getvars}}: Get ACS variable names from formulas.
#' }
#'
#' @details
#'
#' \itemize{
#'   \item Version: 0.2
#'   \item Date: 2015-04-02
#'   \item Depends: acs (>= 1.2), data.table (>= 1.9.5)
#'   \item URL: \url{http://github.com/sdaza/acsr}
#' }
#'
#' @docType package
#' @name acsr
NULL
