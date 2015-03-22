#' @title Create a list containing ACS data files.
#' @description Because downloading the data each time one wants to compute a proportion, ratio or aggregation can be time consuming, the \code{acsdata} function allow to save an object that can be used as input in the \code{\link{sumacs}} function. Before using this function remember to define the key to download the data using the \code{\link{acs}} command: \code{api.key.install(key="*")}.
#' @param formula A character object or vector of characters containing formulas using ACS variables following the format.
#' @param level A character or vector of character specifying the geographic level of the data. The levels included in this functions are: "state", "county", "county.subdivision", "tract", "block.group", "congressional.district", "school.district.secondary", "school.district.elementary". The default value is "county". 
#' @param endyear An integer (defaults to 2013) indicating the latest year of the data in the survey.
#' @param state Either the two-digit numeric FIPS code for the state, the two-letter postal abbreviation, or a character string to match in the state name (or wildcard "*" for all); setting state without other options corresponds to using census summary level 040, but it may be used in conjunction with other summary levels below. The default value is "WI".
#' @param county Either the numeric FIPS code (or wildcard "*" for all) for the county or a character string to match in the county name; setting state and county without other options corresponds to using census summary level 050, but they may be used in conjunction with other summary levels below. The default value is "*".
#' @param county_subdivision Either the numeric FIPS code (or wildcard "*" for all) for the county subdivision or a character string to match in the county subdivision name; setting state, county, and county.subdivision without other options corresponds to using census summary level 060.
#' @param tract A six digit numeric FIPS code (or wildcard "*" for all) for the census tract, including trailing zeroes; remove decimal points; leading zeroes may be omitted; see description; tract may be used with state and county for census summary levels 140, and with state, county, and block.group for summary level 150.
#' @param block_group The numeric FIPS code (or wildcard "*" for all) for the block.group.  block.group may be used with state, county, and tract for census summary levels 150. The default value is "*".
#' @param congressional_district A numeric code (or wildcard "*" for all) corresponding to the desired FIPS congressional district (e.g., state="WI" and congressional.district=1); setting state and congressional.district without other options corresponds to using census summary level 500, but they may be used in conjunction with county for summary level 510. The default value is "*".
#' @param school_district_elementary A numeric code (or wildcard "*" for all) corresponding to the desired FIPS state school district (elementary), or a character string to search for in the names of these districts; setting state and school.district.elementary without other options corresponds to using census summary level 950. The default value is "*".
#' @param school_district_secondary A numeric code (or wildcard "*" for all) corresponding to the desired FIPS state school district (secondary), or a character string to search for in the names of these districts; setting state and school.district.secondary without other options corresponds to using census summary level 960. The default value is "*".
#' @return Returns a list of ACS objects for different levels to be used with the \code{\link{sumacs}} function.
#' @note Depending on the quality of the internet connection, number of variables and level, getting the ACS data can be slow (it might take more than 30 minutes), especially for the levels "county.subdivision", "block.group", and "tract".
#' @examples
#' acsdata("(b16004_004 + b16004_026 + b16004_048 / b16004_001)", level = "tract")
acsdata <- function(formula, level = "county", endyear = 2013,
                        state = "WI",
                        county = "*",
                        county_subdivision ="*",
                        tract = "*",
                        block_group = "*",
                        congressional_district = "*",
                        school_district_secondary = "*",
                        school_district_elementary = "*") {
  
  variables <- getvars(formula)
  variables1 <- variables[1] # JUST TO SPEED AUXILIARY DATA EXTRACTION

  nvars <- length(variables)
  nlevels <- length(level)

  print (paste0(". . . . . .  ACS variables : ", nvars, "  . . . . . "))
  print (paste0(". . . . . .  Levels : ", nlevels, "  . . . . . "))

  ldata <- list()

  for (tlev in 1:length(level) ) {

    # FIRST GET THE DATA, THIS COULD BE SLOW, SPECIALLY FOR COUNTY SUBDIVISION AND BLOCK GROUP

    if (level[tlev] == "state") {
      print (". . . . . .  Getting state data . . . . . ")
      ldata[["state"]] <- suppressWarnings(acs.fetch(geo = geo.make(state = state), variable = variables, endyear = endyear))

      if (length(ldata[["state"]]@acs.colnames) != length(variables)) {
        stop("Not all the ACS variables were found, check variable names in your formulas!")
      }

    }

    else if (level[tlev] == "county") {
      print (". . . . . .  Getting county data  . . . . . ")
      ldata[["county"]] <- suppressWarnings(acs.fetch(geo = geo.make(state = state, county = county), variable = variables, endyear = endyear))

      if (length(ldata[["county"]]@acs.colnames) != length(variables)) {
        stop("Not all the ACS variables were found, check variable names in your formulas!")
      }

    }

    else if (level[tlev] == "county.subdivision") {

      print (". . . . . .  Getting county subdivision data  . . . . . ")
      county.data <-  suppressWarnings( acs.fetch(geo = geo.make(state= state, county = "*"), variable = variables1, endyear = endyear) )
      county <- as.numeric(geography(county.data)$county)
      ldata[["county.subdivision"]] <- suppressWarnings( acs.fetch(geo = geo.make(state = state, county = county, county.subdivision = county_subdivision), variable = variables, endyear = endyear) )

      if (length(ldata[["county.subdivision"]]@acs.colnames) != length(variables)) {
        stop("Not all the ACS variables were found, check variable names in your formulas!")
      }

    }

    else if (level[tlev] == "tract") {

      print (". . . . . .  Getting tract data  . . . . . ")
      ldata[["tract"]] <- suppressWarnings( acs.fetch(geo = geo.make(state = state, county = county, tract = tract), variable = variables, endyear = endyear) )

      if (length(ldata[["tract"]]@acs.colnames) != length(variables)) {
        stop("Not all the ACS variables were found, check variable names in your formulas!")
      }


    }

    else if (level[tlev] == "block.group") {

      print (". . . . . .  Getting block group data  . . . . . ")
      county.data <-  suppressWarnings( acs.fetch(geo = geo.make(state= state, county = "*"), variable = variables1, endyear = endyear))
      county <- as.numeric(geography(county.data)$county)

      ldata[["block.group"]] <- suppressWarnings ( acs.fetch(geo = geo.make(state = state, county = county, tract = tract, block.group = block_group), variable = variables, endyear = endyear) )

      if (length(ldata[["block.group"]]@acs.colnames) != length(variables)) {
        stop("Not all the ACS variables were found, check variable names in your formulas!")
      }

    }
    else if (level[tlev] == "congressional.district") {
      print (". . . . . .  Getting congressional district data  . . . . . ")
      ldata[["congressional.district"]] <- suppressWarnings( acs.fetch(geo = geo.make(state= state, congressional.district = congressional_district), variable = variables, endyear = endyear) )

      if (length(ldata[["congressional.district"]]@acs.colnames) != length(variables)) {
        stop("Not all the ACS variables were found, check variable names in your formulas!")
      }

    }
    else if (level[tlev] == "school.district.secondary") {
      print (". . . . . .  Getting school district secondary data  . . . . . ")
      ldata[["school.district.secondary"]] <- suppressWarnings( acs.fetch(geo = geo.make(state= state, school.district.secondary = school_district_secondary), variable = variables, endyear = endyear) )

      if (length(ldata[["school.district.secondary"]]@acs.colnames) != length(variables)) {
        stop("Not all the ACS variables were found, check variable names in your formulas!")
      }

    }
    else if (level[tlev] == "school.district.elementary") {
      print (". . . . . .  Getting school district elementary data  . . . . . ")
      ldata[["school.district.elementary"]] <- suppressWarnings ( acs.fetch(geo = geo.make(state= state, school.district.elementary = school_district_elementary), variable = variables, endyear = endyear) )

      if (length(ldata[["school.district.elementary"]]@acs.colnames) != length(variables)) {
        stop("Not all the ACS variables were found, check variable names in your formulas!")
      }

    }

  } # END LEVEL LOOP

  print(". . . . . .  Done!  . . . . . ")
  return(ldata)

}
