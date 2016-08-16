#' @title Estimate proportions, ratios, aggregations and the respective margins
#'   of error (MOEs) using custom geographic combinations.
#' @description The \code{combine.output} function uses outputs from the
#'   \code{\link{acs}} package to compute proportions, ratios and aggregations
#'   based on text formulas, or simply extract variables. The function
#'   downloads the data and then estimate the formulas. If the function is used
#'   without specifying any \code{data}, remember to define a key using the
#'   \code{\link{acs}} command \code{api.key.install(key="*")}.
#' @param formula A character or vector of characters containing formulas using
#'   ACS variables or just variables. + - operators can be included. / defines a
#'   division. When the formula contains "* 100", the final statistic and MOE is
#'   multiply by 100.
#' @param varname A character or vector of characters containing the new
#'   variables to be created. This vector must have same length as
#'   \code{formula} and \code{method}, and it is not needed when getting only
#'   variables.
#' @param method A character or vector of characters defining the type of
#'   estimate expected: "proportion", "ratio", "aggregation", "variables". This
#'   vector must have same length as \code{formula} and \code{varname}. It is
#'   not needed when getting only variables.
#' @param level A character or vector of characters specifying the geographic
#'   level of the data. It has to have length one (only one level can be specified). You may need to specificy values of the
#'   corresponding levels. For instance, when \code{level = "county"}, you have
#'   to specify a state (e.g., \code{state = "WI"}, the default state in this
#'   package). You can also use a wildcard method (\code{state = "*"}) to
#'   include all the states. Below, you can see the required combinations of
#'   different summary levels.
#'
#'   \cr 010 us \cr 020 region \cr 030 division \cr 040 state \cr 050 state,
#'   county \cr 060 state, county, county.subdivision \cr 140 state, county,
#'   tract \cr 150 state, county, tract, block.group \cr 160 state, place \cr
#'   250 american.indian.area \cr 320 state, msa \cr 340 state, csa \cr 350
#'   necta \cr 400 urban.area \cr 500 state, congressional.district \cr 610
#'   state, state.legislative.district.upper \cr 620 state,
#'   state.legislative.district.lower \cr 795 state, puma \cr 860 zip.code \cr
#'   950 state, school.district.elementary \cr 960 state,
#'   school.district.secondary \cr 970 state, school.district.unified \cr
#'
#' @param endyear An integer (defaults to 2014) indicating the latest year of
#'   the data in the survey.
#' @param span An integer indicating the span (in years) of the desired ACS data
#'   (should be 1, 3, or 5), defaults to 5.
#' @param conf.level Confidence level to estimate MOEs. The default value is
#'   0.90.
#' @param one.zero Whether to include standard errors for only one zero-value
#'   (max value) of columns or all. The default is TRUE.
#' @param trace Shows progress of the variable creation. The default is TRUE.
#' @param format.out Format of the output: "wide" or "long". The default is
#'   "wide".
#' @param file The resulting output is exported to a CSV file rather than to the
#'   R prompt. The file name must be specified as a character string.
#' @param combine.names Labels for the aggregate geography. It should have the same length as the lowest level grouping list.
#' @return Returns a \code{data.table/data.frame} object with the estimates and
#'   MOEs.
#' @details Combinations use the one.zero method in aggregation. When the standard error of a proportion cannot be estimated, the
#'   "ratio" option is used. This adjustment is done row by row.
#' @examples
#' # api.key.install(key="*")
#' combine.output( "(b16004_004 + b16004_026 + b16004_048 / b16004_001)",
#'               varname = "langspan0913",
#'               method = "prop",
#'               level = "block.group",
#'               state = "MA",
#'               county = "Middlesex",
#'               tract = 387201,
#'               block.group = list(1:2, 2:3, 3:4),
#'               combine.names = c("g1", "g2", "g3"))
combine.output  <- function(formula, varname = NULL, method = NULL, level = NULL, endyear = 2014, span = 5, conf.level = 0.90, one.zero = TRUE, trace = TRUE, format.out = "wide", file = NULL,
	                    combine.names = NULL,
                        us = "*",
                        region = "*",
                        division = "*",
                        state = "WI",
                        county = "*",
                        county.subdivision ="*",
                        place ="*",
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
                        )  {

if ( is.null(combine.names) )
	{ stop("You have to specify combine.names, e.g., c('group1', 'group2)") }

if ( is.null(level) )
	{ stop("You have to specify a level (e.g., state)") }

if ( length(level) > 1 ) {
	stop("Level should be only one when merging outputs!") }

# output by level
# list to save results
output <- list()

# region

if ( level == "region") {

if ( length(combine.names) != length(region) ) {
	stop("Combine names don't match the number of elements in the list region" )}


for ( i in seq_along(combine.names) ) {

 print(paste0(". . . . . .  Defining ", combine.names[i]))

 output[[i]] <- acsr::sumacs(formula = formula, varname = varname, method = method, level = level, endyear = endyear, span = span, conf.level = conf.level, one.zero = one.zero, trace = trace, format.out= format.out, file = NULL,
	 combine = TRUE,
	 combine.name = combine.names[i],
	 region = region[[i]]
      )


}

}

# end region level

# division

if ( level == "division") {

if ( length(combine.names) != length(division) ) {
	stop("Combine names don't match the number of elements in the list division" )}


for ( i in seq_along(combine.names) ) {

 print(paste0(". . . . . .  Defining ", combine.names[i]))

 output[[i]] <- acsr::sumacs(formula = formula, varname = varname, method = method, level = level, endyear = endyear, span = span, conf.level = conf.level, one.zero = one.zero, trace = trace, format.out= format.out, file = NULL,
	 combine = TRUE,
	 combine.name = combine.names[i],
	 division = division[[i]]
      )


}

}

# end division level

# state

if ( level == "state") {

if ( length(combine.names) != length(state) ) {
	stop("Combine names don't match the number of elements in the list state" )}


for ( i in seq_along(combine.names) ) {

 print(paste0(". . . . . .  Defining ", combine.names[i]))

 output[[i]] <- acsr::sumacs(formula = formula, varname = varname, method = method, level = level, endyear = endyear, span = span, conf.level = conf.level, one.zero = one.zero, trace = trace, format.out= format.out, file = NULL,
	 combine = TRUE,
	 combine.name = combine.names[i],
	 state = state[[i]]
      )


}

}

# end state level

# county

if ( level == "county") {

if ( length(state) > 1) { stop("Only one state can be specified!") }

if ( length(combine.names) != length(county) ) {
	stop("Combine names don't match the number of elements in the list county" )}


for ( i in seq_along(combine.names) ) {

 print(paste0(". . . . . .  Defining ", combine.names[i]))

 output[[i]] <- acsr::sumacs(formula = formula, varname = varname, method = method, level = level, endyear = endyear, span = span, conf.level = conf.level, one.zero = one.zero, trace = trace, format.out= format.out, file = NULL,
	 combine = TRUE,
	 combine.name = combine.names[i],
	 state = state,
	 county = county[[i]]
      )


}

}

# end county level

# county.subdivision

if ( level == "county.subdivision") {

if ( length(state) > 1) { stop("Only one state can be specified!") }
if ( length(county) > 1) { stop("Only one county can be specified!") }

if ( length(combine.names) != length(county.subdivision) ) {
	stop("Combine names don't match the number of elements in the list county.subdivision" )}


for ( i in seq_along(combine.names) ) {

 print(paste0(". . . . . .  Defining ", combine.names[i]))

 output[[i]] <- acsr::sumacs(formula = formula, varname = varname, method = method, level = level, endyear = endyear, span = span, conf.level = conf.level, one.zero = one.zero, trace = trace, format.out= format.out, file = NULL,
	 combine = TRUE,
	 combine.name = combine.names[i],
	 state = state,
	 county = county,
	 county.subdivision = county.subdivision[[i]]
      )

}

}

# end county.subdivision level

# tract

if ( level == "tract") {

if ( length(state) > 1) { stop("Only one state can be specified!") }
if ( length(county) > 1) { stop("Only one county can be specified!") }

if ( length(combine.names) != length(tract) ) {
	stop("Combine names don't match the number of elements in the list tract" )}


for ( i in seq_along(combine.names) ) {

 print(paste0(". . . . . .  Defining ", combine.names[i]))

 output[[i]] <- acsr::sumacs(formula = formula, varname = varname, method = method, level = level, endyear = endyear, span = span, conf.level = conf.level, one.zero = one.zero, trace = trace, format.out= format.out, file = NULL,
	 combine = TRUE,
	 combine.name = combine.names[i],
	 state = state,
	 county = county,
	 tract = tract[[i]]
      )

}

}

# end tract level

# block.group

if ( level == "block.group") {

if ( length(state) > 1) { stop("Only one state can be specified!") }
if ( length(county) > 1) { stop("Only one county can be specified!") }
if ( length(tract) > 1) { stop("Only one tract can be specified!") }

if ( length(combine.names) != length(block.group) ) {
	stop("Combine names don't match the number of elements in the list block.group" )}


for ( i in seq_along(combine.names) ) {

 print(paste0(". . . . . .  Defining ", combine.names[i]))

 output[[i]] <- acsr::sumacs(formula = formula, varname = varname, method = method, level = level, endyear = endyear, span = span, conf.level = conf.level, one.zero = one.zero, trace = trace, format.out = format.out, file = NULL,
	 combine = TRUE,
	 combine.name = combine.names[i],
	 state = state,
	 county = county,
	 tract = tract,
	 block.group = block.group[[i]]
      )

}

}

# end block.group level

# place

if ( level == "place") {

if ( length(state) > 1) { stop("Only one state can be specified!") }

if ( length(combine.names) != length(place) ) {
	stop("Combine names don't match the number of elements in the list place" )}

for ( i in seq_along(combine.names) ) {

 print(paste0(". . . . . .  Defining ", combine.names[i]))

 output[[i]] <- acsr::sumacs(formula = formula, varname = varname, method = method, level = level, endyear = endyear, span = span, conf.level = conf.level, one.zero = one.zero, trace = trace, format.out = format.out, file = NULL,
	 combine = TRUE,
	 combine.name = combine.names[i],
	 state = state,
	 place = place[[i]]
      )

}

}

# end place level

# american.indian.area

if ( level == "american.indian.area") {

if ( length(combine.names) != length(american.indian.area) ) {
	stop("Combine names don't match the number of elements in the list american.indian.area" )}

for ( i in seq_along(combine.names) ) {

 print(paste0(". . . . . .  Defining ", combine.names[i]))

 output[[i]] <- acsr::sumacs(formula = formula, varname = varname, method = method, level = level, endyear = endyear, span = span, conf.level = conf.level, one.zero = one.zero, trace = trace, format.out = format.out, file = NULL,
	 combine = TRUE,
	 combine.name = combine.names[i],
	 american.indian.area = american.indian.area[[i]]
      )

}

}

# end american.indian.area level


# msa

if ( level == "msa") {

if ( length(state) > 1) { stop("Only one state can be specified!") }

if ( length(combine.names) != length(msa) ) {
	stop("Combine names don't match the number of elements in the list msa" )}

for ( i in seq_along(combine.names) ) {

 print(paste0(". . . . . .  Defining ", combine.names[i]))

 output[[i]] <- acsr::sumacs(formula = formula, varname = varname, method = method, level = level, endyear = endyear, span = span, conf.level = conf.level, one.zero = one.zero, trace = trace, format.out = format.out, file = NULL,
	 combine = TRUE,
	 combine.name = combine.names[i],
	 state = state,
	 msa = msa[[i]]
      )

}

}

# end msa level

# necta

if ( level == "necta") {

if ( length(combine.names) != length(necta) ) {
	stop("Combine names don't match the number of elements in the list necta" )}

for ( i in seq_along(combine.names) ) {

 print(paste0(". . . . . .  Defining ", combine.names[i]))

 output[[i]] <- acsr::sumacs(formula = formula, varname = varname, method = method, level = level, endyear = endyear, span = span, conf.level = conf.level, one.zero = one.zero, trace = trace, format.out = format.out, file = NULL,
	 combine = TRUE,
	 combine.name = combine.names[i],
	 necta = necta[[i]]
      )

}

}

# end necta level

# urban.area

if ( level == "urban.area") {

if ( length(combine.names) != length(urban.area) ) {
	stop("Combine names don't match the number of elements in the list urban.area" )}

for ( i in seq_along(combine.names) ) {

 print(paste0(". . . . . .  Defining ", combine.names[i]))

 output[[i]] <- acsr::sumacs(formula = formula, varname = varname, method = method, level = level, endyear = endyear, span = span, conf.level = conf.level, one.zero = one.zero, trace = trace, format.out = format.out, file = NULL,
	 combine = TRUE,
	 combine.name = combine.names[i],
	 urban.area = urban.area[[i]]
      )

}

}

# end urban.area level

# congressional.district

if ( level == "congressional.district") {

if ( length(combine.names) != length(congressional.district) ) {
	stop("Combine names don't match the number of elements in the list congressional.district" )}

for ( i in seq_along(combine.names) ) {

 print(paste0(". . . . . .  Defining ", combine.names[i]))

 output[[i]] <- acsr::sumacs(formula = formula, varname = varname, method = method, level = level, endyear = endyear, span = span, conf.level = conf.level, one.zero = one.zero, trace = trace, format.out = format.out, file = NULL,
	 combine = TRUE,
	 combine.name = combine.names[i],
	 state = state,
	 congressional.district = congressional.district[[i]]
      )

}

}

# end congressional.district level

# state.legislative.district.upper

if ( level == "state.legislative.district.upper") {

if ( length(state) > 1) { stop("Only one state can be specified!") }

if ( length(combine.names) != length(state.legislative.district.upper) ) {
	stop("Combine names don't match the number of elements in the list state.legislative.district.upper" )}

for ( i in seq_along(combine.names) ) {

 print(paste0(". . . . . .  Defining ", combine.names[i]))

 output[[i]] <- acsr::sumacs(formula = formula, varname = varname, method = method, level = level, endyear = endyear, span = span, conf.level = conf.level, one.zero = one.zero, trace = trace, format.out = format.out, file = NULL,
	 combine = TRUE,
	 combine.name = combine.names[i],
	 state = state,
	 state.legislative.district.upper = state.legislative.district.upper[[i]]
      )

}

}

# end state.legislative.district.lower level

# state.legislative.district.lower

if ( level == "state.legislative.district.lower") {

if ( length(state) > 1) { stop("Only one state can be specified!") }

if ( length(combine.names) != length(state.legislative.district.lower) ) {
	stop("Combine names don't match the number of elements in the list state.legislative.district.lower" )}

for ( i in seq_along(combine.names) ) {

 print(paste0(". . . . . .  Defining ", combine.names[i]))

 output[[i]] <- acsr::sumacs(formula = formula, varname = varname, method = method, level = level, endyear = endyear, span = span, conf.level = conf.level, one.zero = one.zero, trace = trace, format.out = format.out, file = NULL,
	 combine = TRUE,
	 combine.name = combine.names[i],
	 state = state,
	 state.legislative.district.lower = state.legislative.district.lower[[i]]
      )

}

}

# end state.legislative.district.lower level

# zip.code

if ( level == "zip.code") {

if ( length(combine.names) != length(zip.code) ) {
	stop("Combine names don't match the number of elements in the list zip.code" )}

for ( i in seq_along(combine.names) ) {

 print(paste0(". . . . . .  Defining ", combine.names[i]))

 output[[i]] <- acsr::sumacs(formula = formula, varname = varname, method = method, level = level, endyear = endyear, span = span, conf.level = conf.level, one.zero = one.zero, trace = trace, format.out = format.out, file = NULL,
	 combine = TRUE,
	 combine.name = combine.names[i],
	 zip.code = zip.code[[i]]
      )

}

}

# end zip.code level

# school.district.elementary

if ( level == "school.district.elementary") {

if ( length(state) > 1) { stop("Only one state can be specified!") }

if ( length(combine.names) != length(school.district.elementary) ) {
	stop("Combine names don't match the number of elements in the list school.district.elementary" )}

for ( i in seq_along(combine.names) ) {

 print(paste0(". . . . . .  Defining ", combine.names[i]))

 output[[i]] <- acsr::sumacs(formula = formula, varname = varname, method = method, level = level, endyear = endyear, span = span, conf.level = conf.level, one.zero = one.zero, trace = trace, format.out = format.out, file = NULL,
	 combine = TRUE,
	 combine.name = combine.names[i],
	 state = state,
	 school.district.elementary = school.district.elementary[[i]]
      )

}

}

# end school.distric.elementary level

# school.district.secondary

if ( level == "school.district.secondary") {

if ( length(state) > 1) { stop("Only one state can be specified!") }

if ( length(combine.names) != length(school.district.secondary) ) {
	stop("Combine names don't match the number of elements in the list school.district.secondary" )}

for ( i in seq_along(combine.names) ) {

 print(paste0(". . . . . .  Defining ", combine.names[i]))

 output[[i]] <- acsr::sumacs(formula = formula, varname = varname, method = method, level = level, endyear = endyear, span = span, conf.level = conf.level, one.zero = one.zero, trace = trace, format.out = format.out, file = NULL,
	 combine = TRUE,
	 combine.name = combine.names[i],
	 state = state,
	 school.district.secondary = school.district.secondary[[i]]
      )

}

}

# end school.distric.elementary level

# create final data set

fdata <- rbindlist(output, fill = TRUE)
fdata <- fdata[, which (unlist(lapply(fdata, function(x) !all(is.na(x))))), with = FALSE]

# write csv

  if (is.null(file)) {
    print(". . . . . .  Done!")
    return(fdata)
  }

  else {
    write.csv(fdata, file = file)
    print(". . . . . .  Data exported to a CSV file! Done!")
  }


}