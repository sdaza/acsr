#' acsr: A package with wrapped functions to manage ACS files.
#'
#' @description Wrapped functions to download, manage, and presenting data from the U.S. Census American Community Survey (ACS).
#'
#' @author Sebastian Daza \email{sebastian.daza@gmail.com}
#'
#' @section Functions:
#'
#' \itemize{
#'   \item \code{\link{acsdata}}: Create a list containing ACS data files to be used with the function  \code{\link{sumacs}}.
#'   \item \code{\link{sumacs}}: Estimate proportions, ratios, and aggregations with their respective MOEs.
#'   \item \code{\link{combine.output}}: Estimate proportions, ratios, and aggregations with their respective MOEs using custom geographic units.
#' }
#'
#' @details
#'
#' \itemize{
#'   \item Version: 0.3
#'   \item Date: 2024-07-08
#'   \item Depends: acs (>= 2.1.2), data.table (>= 1.10.4)
#'   \item URL: \url{http://github.com/sdaza/acsr}
#' }
#'
#' @docType package
#' @name acsr
NULL
