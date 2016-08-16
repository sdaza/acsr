#' @title Create a list containing ACS data files (\code{\link{acs}}).
#' @description Because downloading the data to compute a proportion, ratio or
#'   aggregation can be slow, the \code{acsdata} function creates an object that
#'   can be used with the \code{\link{sumacs}} function. Before using this
#'   function remember to define a key using the \code{\link{acs}} command
#'   \code{api.key.install(key="*")}.
#' @param formula A character or vector of characters containing formulas using
#'   ACS variables. + - operators can be included. / defines a division.
#' @param level A character or vector of characters specifying the geographic
#'   level of the data. It may be necessary to specificy values to the
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
#'   (should be 1, 3,or 5), defaults to 5.
#' @param combine Whether the geographies are to be combined when the data is downloaded.
#' @param combine.name Label for the aggregate geography when data are combined. The default value is \code{aggregate}.
#' @return Returns a list of ACS objects for different levels to be used with
#'   the \code{\link{sumacs}} function.
#' @note Depending on the quality of the internet connection, number of
#'   variables and levels, getting the ACS data can be slow especially for the
#'   levels "county.subdivision", "block.group", and "tract" (it might take more
#'   than 30 minutes).
#' @examples
#' api.key.install(key="*")
#' acsdata("(b16004_004 + b16004_026 + b16004_048 / b16004_001)", level = "county")
#' acsdata(c("b16004_004", "b16004_026"), level = "county")

acsdata <- function(formula, level = "state", endyear = 2014, span = 5,
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
                        school.district.unified = "*",
                        combine = FALSE,
                        combine.name = "aggregate"
  ) {


lnames <- c("us","region","division","state","county","county.subdivision","place","tract","block.group","msa","csa","necta","urban.area","congressional.district","state.legislative.district.upper","state.legislative.district.lower","puma","zip.code","american.indian.area","school.district.elementary","school.district.secondary","school.district.unified")


for (i in seq_along(level)) {
if ( !level[i] %in% lnames ) { stop("Some levels were not found, please check!")}
}

  variables <- acsr::getvars(formula)
  variables_aux <- variables[1] # just to speed auxiliary data extraction when downloading directly doesn't

  nvars <- length(variables)
  nlevels <- length(level)

  #print (paste0(". . . . . .  ACS variables : ", nvars))
  #print (paste0(". . . . . .  Levels : ", nlevels))

  ldata <- list()

  for (tlev in 1:length(level) ) {

    # first get the data, this could be slow, specially for county subdivision and block group

    if (level[tlev] == "us") {
      print (". . . . . .  Getting us data")
      ldata[["us"]] <- suppressWarnings(acs::acs.fetch( acs::geo.make(us = us), variable = variables, endyear = endyear, span = span))

      if (length(ldata[["us"]]@acs.colnames) != length(variables)) {
        stop("Not all the ACS variables were found, check variable names in your formulas!")
      }

    }

   else if (level[tlev] == "region") {
      print (". . . . . .  Getting region data")
      ldata[["region"]] <- suppressWarnings(acs::acs.fetch( acs::geo.make(region = region, combine = combine, combine.term = combine.name), variable = variables, endyear = endyear, span = span, one.zero = TRUE))

      if (length(ldata[["region"]]@acs.colnames) != length(variables)) {
        stop("Not all the ACS variables were found, check variable names in your formulas!")
      }

    }

   else if (level[tlev] == "division") {
      print (". . . . . .  Getting division data")
      ldata[["division"]] <- suppressWarnings(acs::acs.fetch( acs::geo.make(division = division, combine = combine, combine.term = combine.name), variable = variables, endyear = endyear, span = span, one.zero = TRUE))

      if (length(ldata[["division"]]@acs.colnames) != length(variables)) {
        stop("Not all the ACS variables were found, check variable names in your formulas!")
      }

    }


    else if (level[tlev] == "american.indian.area") {
      print (". . . . . .  Getting american indian area data")
      ldata[["american.indian.area"]] <- suppressWarnings( acs::acs.fetch( acs::geo.make(american.indian.area = american.indian.area, combine = combine, combine.term = combine.name), variable = variables, endyear = endyear, span = span, one.zero = TRUE) )

      if (length(ldata[["american.indian.area"]]@acs.colnames) != length(variables)) {
        stop("Not all the ACS variables were found, check variable names in your formulas!")
      }

    }


    else if (level[tlev] == "state") {
      print (". . . . . .  Getting state data")
      ldata[["state"]] <- suppressWarnings(acs::acs.fetch( acs::geo.make(state = state, combine = combine, combine.term = combine.name), variable = variables, endyear = endyear, span = span, one.zero = TRUE))

      if (length(ldata[["state"]]@acs.colnames) != length(variables)) {
        stop("Not all the ACS variables were found, check variable names in your formulas!")
      }

    }

    else if (level[tlev] == "county") {
      print (". . . . . .  Getting county data")
      ldata[["county"]] <- suppressWarnings(acs::acs.fetch( acs::geo.make(state = state, county = county, combine = combine, combine.term = combine.name), variable = variables, endyear = endyear, span = span, one.zero = TRUE))

      if (length(ldata[["county"]]@acs.colnames) != length(variables)) {
        stop("Not all the ACS variables were found, check variable names in your formulas!")
      }

    }

    else if (level[tlev] == "county.subdivision") {

    # I didn't need an auxiliary procedure to get the data

      print (". . . . . .  Getting county subdivision data")


      ldata[["county.subdivision"]] <- suppressWarnings( acs::acs.fetch( acs::geo.make(state = state, county = county, county.subdivision = county.subdivision, combine = combine, combine.term = combine.name), variable = variables, endyear = endyear, span = span, one.zero = TRUE) )

      if (length(ldata[["county.subdivision"]]@acs.colnames) != length(variables)) {
        stop("Not all the ACS variables were found, check variable names in your formulas!")
      }
    }

    else if (level[tlev] == "place") {

      print (". . . . . .  Getting place data")
      ldata[["place"]] <- suppressWarnings( acs::acs.fetch( acs::geo.make(state = state, place = place, combine = combine, combine.term = combine.name), variable = variables, endyear = endyear, span = span, one.zero = TRUE) )

      if (length(ldata[["place"]]@acs.colnames) != length(variables)) {
        stop("Not all the ACS variables were found, check variable names in your formulas!")
      }
    }

    else if (level[tlev] == "tract") {


      print (". . . . . .  Getting tract data")
      ldata[["tract"]] <- suppressWarnings( acs::acs.fetch( acs::geo.make(state = state, county = county, tract = tract, combine = combine, combine.term = combine.name), variable = variables, endyear = endyear, span = span, one.zero = TRUE) )

      if (length(ldata[["tract"]]@acs.colnames) != length(variables)) {
        stop("Not all the ACS variables were found, check variable names in your formulas!")
      }
    }

    else if (level[tlev] == "block.group") {

      print (". . . . . .  Getting block group data")

      # auxiliary procedure to get the data
      if (county != "*") {

      ldata[["block.group"]] <-  suppressWarnings( acs::acs.fetch( acs::geo.make(state= state, county = county, tract = tract, block.group = block.group, combine = combine, combine.term = combine.name), variable = variables, endyear = endyear, span = span, one.zero = TRUE))

      }
      else {

      county.data <-  suppressWarnings( acs::acs.fetch( acs::geo.make(state= state, county = "*", ), variable = variables_aux, endyear = endyear, span = span))

      ncounty <- as.numeric(geography(county.data)$county)

      ldata[["block.group"]] <- suppressWarnings ( acs::acs.fetch( acs::geo.make(state = state, county = ncounty, tract = tract, block.group = block.group, combine = combine, combine.term = combine.name), variable = variables, endyear = endyear, span = span, one.zero = TRUE) )

      }

      if (length(ldata[["block.group"]]@acs.colnames) != length(variables)) {
        stop("Not all the ACS variables were found, check variable names in your formulas!")
      }
    }

    else if (level[tlev] == "msa") {

      print (". . . . . .  Getting msa data")
      ldata[["msa"]] <- suppressWarnings ( acs::acs.fetch( acs::geo.make(state = state,  msa = msa, combine = combine, combine.term = combine.name), variable = variables, endyear = endyear, span = span, one.zero = TRUE) )

      if (length(ldata[["msa"]]@acs.colnames) != length(variables)) {
        stop("Not all the ACS variables were found, check variable names in your formulas!")
      }

    }

    else if (level[tlev] == "csa") {

      print (". . . . . .  Getting csa data")
      ldata[["csa"]] <- suppressWarnings ( acs::acs.fetch( acs::geo.make(state = state,  csa = csa, combine = combine, combine.term = combine.name), variable = variables, endyear = endyear, span = span, one.zero = TRUE) )

      if (length(ldata[["csa"]]@acs.colnames) != length(variables)) {
        stop("Not all the ACS variables were found, check variable names in your formulas!")
      }

    }

    else if (level[tlev] == "necta") {

      print (". . . . . .  Getting necta data")
      ldata[["necta"]] <- suppressWarnings ( acs::acs.fetch( acs::geo.make(necta = necta, combine = combine, combine.term = combine.name), variable = variables, endyear = endyear, span = span, one.zero = TRUE) )

      if (length(ldata[["necta"]]@acs.colnames) != length(variables)) {
        stop("Not all the ACS variables were found, check variable names in your formulas!")
      }

    }

    else if (level[tlev] == "urban.area") {

      print (". . . . . .  Getting urban area data")
      ldata[["urban.area"]] <- suppressWarnings ( acs::acs.fetch( acs::geo.make(urban.area = urban.area, combine = combine, combine.term = combine.name), variable = variables, endyear = endyear, span = span, one.zero = TRUE) )

      if (length(ldata[["urban.area"]]@acs.colnames) != length(variables)) {
        stop("Not all the ACS variables were found, check variable names in your formulas!")
      }

    }

    else if (level[tlev] == "congressional.district") {
      print (". . . . . .  Getting congressional district data")

      ldata[["congressional.district"]] <- suppressWarnings( acs::acs.fetch( acs::geo.make(state = state, congressional.district = congressional.district, combine = combine, combine.term = combine.name), variable = variables, endyear = endyear, span = span, one.zero = TRUE) )

      if (length(ldata[["congressional.district"]]@acs.colnames) != length(variables)) {
        stop("Not all the ACS variables were found, check variable names in your formulas!")
      }

    }

    else if (level[tlev] == "state.legislative.district.upper") {
      print (". . . . . .  Getting state legislative district upper data")
      ldata[["state.legislative.district.upper"]] <- suppressWarnings( acs::acs.fetch( acs::geo.make(state = state,  state.legislative.district.upper = state.legislative.district.upper, combine = combine, combine.term = combine.name), variable = variables, endyear = endyear, span = span, one.zero = TRUE) )

      if (length(ldata[["state.legislative.district.upper"]]@acs.colnames) != length(variables)) {
        stop("Not all the ACS variables were found, check variable names in your formulas!")
      }

    }

    else if (level[tlev] == "state.legislative.district.lower") {
      print (". . . . . .  Getting state legislative district lower data")
      ldata[["state.legislative.district.lower"]] <- suppressWarnings( acs::acs.fetch( acs::geo.make(state = state,  state.legislative.district.lower = state.legislative.district.lower, combine = combine, combine.term = combine.name), variable = variables, endyear = endyear, span = span, one.zero = TRUE) )

      if (length(ldata[["state.legislative.district.lower"]]@acs.colnames) != length(variables)) {
        stop("Not all the ACS variables were found, check variable names in your formulas!")
      }

    }

    else if (level[tlev] == "puma") {
      print (". . . . . .  Getting puma data")
      ldata[["puma"]] <- suppressWarnings( acs::acs.fetch( acs::geo.make(state = state,  puma = puma, combine = combine, combine.term = combine.name), variable = variables, endyear = endyear, span = span, one.zero = TRUE) )

      if (length(ldata[["puma"]]@acs.colnames) != length(variables)) {
        stop("Not all the ACS variables were found, check variable names in your formulas!")
      }

    }

    else if (level[tlev] == "zip.code") {
      print (". . . . . .  Getting zip code data")
      ldata[["zip.code"]] <- suppressWarnings( acs::acs.fetch( acs::geo.make(zip.code = zip.code, combine = combine, combine.term = combine.name), variable = variables, endyear = endyear, span = span, one.zero = TRUE) )

      if (length(ldata[["zip.code"]]@acs.colnames) != length(variables)) {
        stop("Not all the ACS variables were found, check variable names in your formulas!")
      }

    }


    else if (level[tlev] == "school.district.elementary") {
      print (". . . . . .  Getting school district elementary data")
      ldata[["school.district.elementary"]] <- suppressWarnings ( acs::acs.fetch( acs::geo.make(state= state, school.district.elementary = school.district.elementary, combine = combine, combine.term = combine.name), variable = variables, endyear = endyear, span = span, one.zero = TRUE) )

      if (length(ldata[["school.district.elementary"]]@acs.colnames) != length(variables)) {
        stop("Not all the ACS variables were found, check variable names in your formulas!")
      }

    }

    else if (level[tlev] == "school.district.secondary") {
      print (". . . . . .  Getting school district secondary data")
      ldata[["school.district.secondary"]] <- suppressWarnings( acs::acs.fetch( acs::geo.make(state= state, school.district.secondary = school.district.secondary, combine = combine, combine.term = combine.name), variable = variables, endyear = endyear, span = span, one.zero = TRUE) )

      if (length(ldata[["school.district.secondary"]]@acs.colnames) != length(variables)) {
        stop("Not all the ACS variables were found, check variable names in your formulas!")
      }

    }

    else if (level[tlev] == "school.district.unified") {
      print (". . . . . .  Getting school district unified data")
      ldata[["school.district.unified"]] <- suppressWarnings( acs::acs.fetch( acs::geo.make(state = state, school.district.unified = school.district.unified, combine = combine, combine.term = combine.name), variable = variables, endyear = endyear, span = span, one.zero = TRUE) )

      if (length(ldata[["school.district.unified"]]@acs.colnames) != length(variables)) {
        stop("Not all the ACS variables were found, check variable names in your formulas!")
      }

    }

  } # end level loop

  print(". . . . . .  Done!")
  return(ldata)

}
