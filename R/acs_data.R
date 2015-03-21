#' Create a list containing ACS data
#'
#' @param formula A vector or character object following the format.
#' @param state A number or character specifying state.
#' @param level A character or vector specifying the
#' @return List of ACS objects of different levels.
#' @examples
#' add(1, 1)
#' add(10, 1)
acsdata <- function(formula, state = "WI", level = "county", endyear = 2013,
                         county = "*",
                         county_subdivision ="*",
                         tract = "*",
                         block_group = "*",
                         congressional_district = "*",
                         school_district_secondary = "*",
                         school_district_elementary = "*") {

  # formula : character, + - are needed, / define numerator and denominator
  # example: b16004_004 + b16004_026 + b16004_048 / b16004_001
  # state: by default WI
  # level: "state", "county", "county.subdivision", "tract", "block.group", "congressional.district", "school.district.secondary", "school.district.elementary"
  # endyear: by default 2013
  # to extract especific units using API, define specific level variables (e.g., block_group)

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
