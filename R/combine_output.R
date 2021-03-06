#' @title Estimate proportions, ratios, aggregations and the respective margins
#'   of error (MOEs) of custom combinations of geographic units.
#' @description Using the \code{sumacs} function, it  computes proportions, ratios and
#'   aggregations based on text formulas, or simply extract variables of combinations of geographic units.
#'   It uses the same procedures of the \code{sumacs} function, but the geographic level and
#'   information have to be specified in lists (they can be nested).
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
#' @param level A list specifying the geographic level of the data. It may be necessary to specify values to the
#'   corresponding levels. For instance, when \code{level = list("county")}, you have
#'   to specify a state (e.g., \code{state = list("WI")}, the default state in this
#'   package), and the counties you want to combine: \code{county = list(1,2)}. They may
#'   be also nested lists (e.g., \code{county = list( list(1:2, 3:4), list(5:6, 7:8) )}).
#'   The number of elements of the level list should be the same as \code{combine.names)} (names of combined groups, see below).
#'
#'   The required combinations of different summary levels are:
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
#' @param combine.names Labels for the aggregate geographies.
#' It should be the same as the number of elements of the list \code{level}.
#' @param dataset A string or vector of strings specifying the data set to be used: acs, sf1 or sf1.
#' The default value is "acs".
#' @param endyear An integer or vector of integers (defaults to 2014) indicating the latest year of
#'   the data in the survey or Census year.
#' @param span An integer indicating the span (in years) of the desired ACS data
#'   (should be 1, 3, or 5), defaults to 5.
#' @param conf.level Confidence level to estimate MOEs. The default value is
#'   0.90.
#' @param one.zero Whether to include standard errors for only one zero-value
#'   (max value) of columns or all. The default is TRUE.
#' @param trace Shows progress of the variable creation. The default is TRUE.
#' @param data Input data generated by the \code{\link{acsdata}} function.
#'   Variables and levels must be the same as those specified in the
#'   \code{sumacs} function.
#' @param format.out Format of the output: "wide" or "long". The default is
#'   "wide".
#' @param file The resulting output is exported to a CSV file rather than to the
#'   R prompt. The file name must be specified as a character string.
#' @param print.levels Boolean that print levels generated by the \code{geo.make} function.
#' @return Returns a \code{data.table/data.frame} object with the estimates and
#'   MOEs.
#' @details When the standard error of a proportion cannot be estimated, the
#'   "ratio" option is used. This adjustment is done row by row.
#' @note Depending on the quality of the Internet connection, number of
#'   variables and levels, getting the ACS data can be slow, especially for the
#'   levels "county.subdivision", "block.group", and "tract" (it might take more
#'   than 30 minutes).  It is recommended to get the data using the function
#'   \code{\link{acsdata}} first, and then to use \code{sumacs}.
#' @examples
#' # api.key.install(key="*")
#'   combine.output("(b16004_004 + b16004_026 + b16004_048 / b16004_001)",
#'     varname = "myvar",
#'     method = "prop",
#'     level = list("division", "division"),
#'     division = list(list(1:2, 3), list(1:2, 3:4)),
#'     combine.names = c("g1", "g2"))
combine.output <- function(formula = formula, varname = varname, method = method,
                           level = level, combine.names, dataset = "acs", endyear = 2014,
                           span = 5, conf.level = 0.90, one.zero = TRUE,
                           trace = TRUE, format.out = "wide",
                           file = NULL, print.levels = TRUE,
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
    school.district.unified = "*") {

 if ( is.list(level) == FALSE ) { stop("level has to be a list when combining output")}

 if (length(level) != length(combine.names) ) { stop("level and combine.names should have the same length") }

 # list to save output
 output <- list()

 # loop by groups

 for ( i in seq_along(combine.names) ) {

 print(paste0(". . . . . .  Defining ", combine.names[i]))

 # get output per level

 # region
if (level[[i]] == "region") {
 output[[i]] <- acsr::sumacs(formula = formula, varname = varname, method = method,
                             level = level[[i]], dataset = dataset, endyear = endyear,
                             span = span, conf.level = conf.level, one.zero = one.zero,
                             trace = trace, format.out = format.out, file = NULL,
                             print.levels = print.levels,
 	 region = region[[i]],
   	 combine = TRUE,
   	 combine.name = combine.names[i])
}

 # division
if (level[[i]] == "division") {
 output[[i]] <- acsr::sumacs(formula = formula, varname = varname, method = method,
                             level = level[[i]], dataset = dataset, endyear = endyear,
                             span = span, conf.level = conf.level, one.zero = one.zero,
                             trace = trace, format.out = format.out, file = NULL,
                             print.levels = print.levels,
     division = division[[i]],
     combine = TRUE,
     combine.name = combine.names[i])
    }

 # american.indian.area
if (level[[i]] == "american.indian.area") {
 output[[i]] <- acsr::sumacs(formula = formula, varname = varname, method = method,
                             level = level[[i]], dataset = dataset, endyear = endyear,
                             span = span, conf.level = conf.level, one.zero = one.zero,
                             trace = trace, format.out = format.out, file = NULL,
                             print.levels = print.levels,
     american.indian.area = american.indian.area[[i]],
     combine = TRUE,
     combine.name = combine.names[i])
    }

# necta
if (level[[i]] == "necta") {
 output[[i]] <- acsr::sumacs(formula = formula, varname = varname, method = method,
                             level = level[[i]], dataset = dataset, endyear = endyear,
                             span = span, conf.level = conf.level, one.zero = one.zero,
                             trace = trace, format.out = format.out, file = NULL,
                             print.levels = print.levels,
     necta = necta[[i]],
     combine = TRUE,
     combine.name = combine.names[i])
    }

# urban.area
if (level[[i]] == "urban.area") {
 output[[i]] <- acsr::sumacs(formula = formula, varname = varname, method = method,
                             level = level[[i]], dataset = dataset, endyear = endyear,
                             span = span, conf.level = conf.level, one.zero = one.zero,
                             trace = trace, format.out = format.out, file = NULL,
                             print.levels = print.levels,
     urban.area = urban.area[[i]],
     combine = TRUE,
     combine.name = combine.names[i])
    }

 # state
if (level[[i]] == "state") {
 output[[i]] <- acsr::sumacs(formula = formula, varname = varname, method = method,
                             level = level[[i]], dataset = dataset, endyear = endyear,
                             span = span, conf.level = conf.level, one.zero = one.zero,
                             trace = trace, format.out = format.out, file = NULL,
                             print.levels = print.levels,
     state = state[[i]],
     combine = TRUE,
     combine.name = combine.names[i])
    }

 # county
if (level[[i]] == "county") {
 output[[i]] <- acsr::sumacs(formula = formula, varname = varname, method = method,
                             level = level[[i]], dataset = dataset, endyear = endyear,
                             span = span, conf.level = conf.level, one.zero = one.zero,
                             trace = trace, format.out = format.out, file = NULL,
                             print.levels = print.levels,
     state = state[[i]],
     county = county[[i]],
     combine = TRUE,
     combine.name = combine.names[i])
    }

 # county.subdivision
if (level[[i]] == "county.subdivision") {
 output[[i]] <- acsr::sumacs(formula = formula, varname = varname, method = method,
                             level = level[[i]], dataset = dataset, endyear = endyear,
                             span = span, conf.level = conf.level, one.zero = one.zero,
                             trace = trace, format.out = format.out, file = NULL,
                             print.levels = print.levels,
     state = state[[i]],
     county = county[[i]],
     county.subdivision = county.subdivision[[i]],
     combine = TRUE,
     combine.name = combine.names[i])
    }

 # tract
if (level[[i]] == "tract") {
 output[[i]] <- acsr::sumacs(formula = formula, varname = varname, method = method,
                             level = level[[i]], dataset = dataset, endyear = endyear,
                             span = span, conf.level = conf.level, one.zero = one.zero,
                             trace = trace, format.out = format.out, file = NULL,
                             print.levels = print.levels,
     state = state[[i]],
     county = county[[i]],
     tract = tract[[i]],
     combine = TRUE,
     combine.name = combine.names[i])
    }

 # block.group
if (level[[i]] == "block.group") {
 output[[i]] <- acsr::sumacs(formula = formula, varname = varname, method = method,
                             level = level[[i]], dataset = dataset, endyear = endyear,
                             span = span, conf.level = conf.level, one.zero = one.zero,
                             trace = trace, format.out = format.out, file = NULL,
                             print.levels = print.levels,
     state = state[[i]],
     county = county[[i]],
     tract = tract[[i]],
     block.group = block.group[[i]],
     combine = TRUE,
     combine.name = combine.names[i])
    }

# place
if (level[[i]] == "place") {
 output[[i]] <- acsr::sumacs(formula = formula, varname = varname, method = method,
                             level = level[[i]], dataset = dataset, endyear = endyear,
                             span = span, conf.level = conf.level, one.zero = one.zero,
                             trace = trace, format.out = format.out, file = NULL,
                             print.levels = print.levels,
     state = state[[i]],
     place = place[[i]],
     combine = TRUE,
     combine.name = combine.names[i])
    }

# puma
if (level[[i]] == "puma") {
 output[[i]] <- acsr::sumacs(formula = formula, varname = varname, method = method,
                             level = level[[i]], dataset = dataset, endyear = endyear,
                             span = span, conf.level = conf.level, one.zero = one.zero,
                             trace = trace, format.out = format.out, file = NULL,
                             print.levels = print.levels,
     state = state[[i]],
     puma = puma[[i]],
     combine = TRUE,
     combine.name = combine.names[i])
    }

# msa
if (level[[i]] == "msa") {
 output[[i]] <- acsr::sumacs(formula = formula, varname = varname, method = method,
                             level = level[[i]], dataset = dataset, endyear = endyear,
                             span = span, conf.level = conf.level, one.zero = one.zero,
                             trace = trace, format.out = format.out, file = NULL,
                             print.levels = print.levels,
     state = state[[i]],
     msa = msa[[i]],
     combine = TRUE,
     combine.name = combine.names[i])
    }

# csa
if (level[[i]] == "csa") {
 output[[i]] <- acsr::sumacs(formula = formula, varname = varname, method = method,
                             level = level[[i]], dataset = dataset, endyear = endyear,
                             span = span, conf.level = conf.level, one.zero = one.zero,
                             trace = trace, format.out = format.out, file = NULL,
                             print.levels = print.levels,
     state = state[[i]],
     csa = csa[[i]],
     combine = TRUE,
     combine.name = combine.names[i])
    }

# congressional.district
if (level[[i]] == "congressional.district") {
 output[[i]] <- acsr::sumacs(formula = formula, varname = varname, method = method,
                             level = level[[i]], dataset = dataset, endyear = endyear,
                             span = span, conf.level = conf.level, one.zero = one.zero,
                             trace = trace, format.out = format.out, file = NULL,
                             print.levels = print.levels,
     state = state[[i]],
     congressional.district = congressional.district[[i]],
     combine = TRUE,
     combine.name = combine.names[i])
    }

# state.legislative.district.lower
if (level[[i]] == "state.legislative.district.lower") {
 output[[i]] <- acsr::sumacs(formula = formula, varname = varname, method = method,
                             level = level[[i]], dataset = dataset, endyear = endyear,
                             span = span, conf.level = conf.level, one.zero = one.zero,
                             trace = trace, format.out = format.out, file = NULL,
                             print.levels = print.levels,
     state = state[[i]],
     state.legislative.district.lower = state.legislative.district.lower[[i]],
     combine = TRUE,
     combine.name = combine.names[i])
    }

# state.legislative.district.upper
if (level[[i]] == "state.legislative.district.upper") {
 output[[i]] <- acsr::sumacs(formula = formula, varname = varname, method = method,
                             level = level[[i]], dataset = dataset, endyear = endyear,
                             span = span, conf.level = conf.level, one.zero = one.zero,
                             trace = trace, format.out = format.out, file = NULL,
                             print.levels = print.levels,
     state = state[[i]],
     state.legislative.district.upper = state.legislative.district.upper[[i]],
     combine = TRUE,
     combine.name = combine.names[i])
    }

# school.district.elementary
if (level[[i]] == "school.district.elementary") {
 output[[i]] <- acsr::sumacs(formula = formula, varname = varname, method = method,
                             level = level[[i]], dataset = dataset, endyear = endyear,
                             span = span, conf.level = conf.level, one.zero = one.zero,
                             trace = trace, format.out = format.out, file = NULL,
                             print.levels = print.levels,
     state = state[[i]],
     school.district.elementary = school.district.elementary[[i]],
     combine = TRUE,
     combine.name = combine.names[i])
    }

# school.district.secondary
if (level[[i]] == "school.district.secondary") {
 output[[i]] <- acsr::sumacs(formula = formula, varname = varname, method = method,
                             level = level[[i]], dataset = dataset, endyear = endyear,
                             span = span, conf.level = conf.level, one.zero = one.zero,
                             trace = trace, format.out = format.out, file = NULL,
                             print.levels = print.levels,
     state = state[[i]],
     school.district.secondary = school.district.secondary[[i]],
     combine = TRUE,
     combine.name = combine.names[i])
    }

}

 # create final data set
fdata <- rbindlist(output, fill = TRUE)
fdata <- fdata[, which (unlist(lapply(fdata, function(x) !all(is.na(x))))), with = FALSE]


# write csv

  if (is.null(file)) {
    return(fdata)
  }

  else {
    write.csv(fdata, file = file)
    print(". . . . . .  Data exported to a CSV file!")
  }

}
