sumacs  <- function(formula, varname, method= "proportion", state="WI", level = "county",
                     conf.level=.90, endyear = "2013", one.zero = TRUE, data = NULL, file = NULL,
                     county = "*",
                     county_subdivision ="*",
                     tract = "*",
                     block_group = "*",
                     congressional_district = "*",
                     school_district_secondary = "*",
                     school_district_elementary = "*")  {

  # formula : character, + - are needed, / define numerator and denominator
  # varname : name of new variables created, this vector must have same length as formula and method
  # method  : by default "proportion
  # example : b16004_004 + b16004_026 + b16004_048 / b16004_001
  # state   : by default "WI"
  # level   : "state", "county", "county.subdivision", "tract", "block.group", "congressional.district", "school.district.secondary", "school.district.elementary"
  # endyear : by default "2013"
  # to extract especific units using API, define specific level variables (e.g., block_group). this does not work when using previous data.

  # CHECKS

  if (length(method) != length(varname)) {
    stop("Methods must have the same length as new variables")
  }

  if (identical(length(formula), length(varname), length(method)) == 0) {
    stop("Vector of formulas, variable names and methods must to have the same length!")
  }

  # LIBRARIES

  library(acs)
  library(data.table)

  conf.level <- round(qnorm( (1 + conf.level) / 2), digits =3)
  variables <- getvars(formula)
  variables1 <- variables[1] # TO SOLVE PROBLEMS WITH API

  nvars <- length(variables)
  nlevels <- length(level)
  newvars <- length(varname)

  print (paste0(". . . . . .  ACS variables : ", nvars, "  . . . . . "))
  print (paste0(". . . . . .  Levels : ", nlevels, "  . . . . . "))
  print (paste0(". . . . . .  New variables : ", newvars, "  . . . . . "))

  #########################################
  # GET DATA FOR ALL THE VARIABLES FIRST
  #########################################

  if (is.null(data)) {

    print (". . . . . .  Getting the data  . . . . . ")
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

  } # FIRST CONDITION

  else if (!is.null(data)) {

    if ( is.list(data) == FALSE ) {
      stop("The data must be a list!")
    }
    else if ( is.list(data) & !(all(sapply(data, class) ==  "acs")) )
    {
      stop("The list must cointain ACS objecs!")
    }
    else if ( !(all(level %in% names(data)))) {
      stop("The data must cointain all the expected levels!")
    }
    else if ( !(all(variables %in% ldata[[1]]@acs.colnames)) ) {
      stop("The data must cointain all the expected variables")
    }

    ldata <- data

  } # END CONDITION ABOUT DATA

  ################################
  # LOOP VARIABLE AND LEVEL
  ################################

  print(". . . . . .  Creating variables  . . . . . ")

  vdata <- data.table()


  for (v in 1:length(varname) ) {

    # CREATE FORMULAS FROM TEXT
    constr <- gsub("\\(|\\)", "", formula[v] ) # REMOVE PARENTHESES
    constr <- gsub("\\* 100", "", constr) # REMOVE * 100

    # TODO: TO CHECK IN THE FUTURE VERSION
    division <- grepl("\\/", constr)

    #####################
    # CONDITIONS
    #####################

    if (tolower(method[v]) %in% c("proportion", "prop", "ratio") & division == TRUE) {

      # DIVISION (PROPORTION OR RATIO)
      form <- strsplit(constr, "/") # SPLIT TO GET NUMERATOR AND DENOMINATOR

      if ( length(form[[1]]) == 1 | length(form[[1]]) > 2 ) {
        stop(paste0("There is no or multiple division operators: ", varname[v]))
      }

      # NUMERATOR
      # EXTRACT OPERATORS
      noper <- strsplit(form[[1]][1], "[aA-zZ]*[0-9]*")
      noper <- grep("\\+|\\-", noper[[1]], value = TRUE)

      # EXTRACT VARIABLES
      nume  <- strsplit(form[[1]][1], "[\\+]|[\\-]")
      nume <- gsub("[[:space:]]", "", nume[[1]])
      nume <- toupper(nume)

      # DENOMINATOR
      # EXTRACT OPERATORS
      doper <- strsplit(form[[1]][2], "[aA-zZ]*[0-9]*")
      doper <- grep("\\+|\\-", doper[[1]], value=TRUE)

      # EXTRACT VARIABLES
      deno  <- strsplit(form[[1]][2], "[\\+]|[\\-]")
      deno <- gsub("[[:space:]]", "", deno[[1]])
      deno <- toupper(deno)

    }

    if (tolower(method[v]) %in% c("aggregation", "agg") & division == FALSE) {

      # EXTRACT OPERATORS
      oper <- strsplit(constr, "[aA-zZ]*[0-9]*")
      oper <- grep("\\+|\\-", oper[[1]], value=TRUE)

      # EXTRACT VARIABLES
      variable  <- strsplit(constr, "[\\+]|[\\-]")
      variable <-  gsub("^\\s+|\\s+$", "", variable[[1]])
      variable <-  toupper(variable)

    }

    ###################################
    # CREATE AND COMPUTE FORMULAS
    ###################################

    # NESTED LEVEL LOOP

    for (l in 1:length(level)) {

      dat <- ldata[[level[l]]]
      geo <- dat@geography

      ######################
      # PROPORTION OR RATIOS
      ######################

      if (tolower(method[v]) %in% c("proportion", "prop", "ratio") & division == TRUE) {

        wn <- which(dat@acs.colnames %in% nume)
        wd <- which(dat@acs.colnames %in% deno)

        nt <- vector()
        for (i in 1:length(nume)) {
          if ( i == max(length(nume))) {
            x <- paste0("dat[,", wn[i], "]")
          }
          else {
            x <- paste0("dat[,", wn[i], "]", noper[i])
          }
          nt <- paste0(nt, x)
        }

        dt <- vector()
        for (i in 1:length(deno)) {
          if ( i == max(length(deno))) {
            x <- paste0("dat[,", wd[i], "]")
          }
          else {
            x <- paste0("dat[,", wd[i], "]", doper[i])
          }
          dt <- paste0(dt, x)
        }

        # ESTIMATES AND ERRORS
        est <- estimate(dat)
        err <- standard.error(dat)

        num <- estimate(eval(parse(text=nt)))
        den <- estimate(eval(parse(text=dt)))

        p <- num / den
        # p[den == 0] <- NA

        # DEFINITION OF ERROR

        if ( length(p) == 1 ) {

          if ( one.zero == TRUE ) {

            if (length(wn) > 1) {
              err_zero_num <- max(err[, wn] ^ 2 * (est[, wn] == 0))
              err_num <- sum(err[, wn] ^ 2 * (est[, wn] != 0))
            }
            else {
              err_zero_num <- err[, wn] ^ 2 * (est[, wn] == 0)
              err_num <- err[, wn] ^ 2 * (est[, wn] != 0)
            }

            if (length(wd) > 1 ) {
              err_zero_den <- max(err[, wd] ^ 2 * (est[, wd] == 0))
              err_den <- sum(err[, wd] ^ 2 * (est[, wd] != 0))
            }
            else {
              err_zero_den <- err[, wd] ^ 2 * (est[, wd] == 0)
              err_den <- err[, wd] ^ 2 * (est[, wd] != 0)
            }

            err_num <- err_zero_num + err_num
            err_den <- err_zero_den + err_den

          }

          else if ( one.zero == FALSE ) {

            if (length(wn) > 1) {
              err_num <- sum(err[, wn] ^ 2)
            }

            else {
              err_num <- err[, wn] ^ 2
            }

            if (length(wd) > 1 ) {
              err_den <- sum(err[, wd] ^ 2)
            }
            else {
              err_den <- err[, wd] ^ 2
            }

          }

        }

        else if ( length(p) > 1 ) {

          if ( one.zero == TRUE ) {

            if (length(wn) > 1) {
              err_zero_num <- apply(err[, wn] ^ 2 * (est[, wn] == 0), 1, max)
              err_num <- apply(err[, wn] ^ 2 * (est[, wn] != 0), 1, sum)
            }
            else {
              err_zero_num <- err[, wn] ^ 2 * (est[, wn] == 0)
              err_num <- err[, wn] ^ 2 * (est[, wn] != 0)
            }

            if (length(wd) > 1 ) {
              err_zero_den <- apply(err[, wd] ^ 2 * (est[, wd] == 0), 1, max)
              err_den <- apply(err[, wd] ^ 2 * (est[, wd] != 0), 1, sum)
            }
            else {
              err_zero_den <- err[, wd] ^ 2 * (est[, wd] == 0)
              err_den <- err[, wd] ^ 2 * (est[, wd] != 0)
            }

            err_num <- err_zero_num + err_num
            err_den <- err_zero_den + err_den

          }

          else if ( one.zero == FALSE ) {

            if (length(wn) > 1) {
              err_num <- apply(err[, wn] ^ 2 , 1, sum)
            }

            else {
              err_num <- err[, wn] ^ 2
            }

            if (length(wd) > 1 ) {
              err_den <- apply(err[, wd] ^ 2, 1, sum)
            }
            else {
              err_den <- err[, wd] ^ 2
            }

          }
        }

        # COMPUTING STANDARD ERRORS FOR PROPORTION AND RATIOS (CHECK)


        if ( tolower(method[v]) %in% c("proportion", "prop") ) {

          suppressWarnings(
            new_error <- ifelse((err_num - ( p ^ 2 * err_den) ) < 0 | is.na( err_num - (p ^ 2 * err_den)),
                                sqrt(err_num + (p ^ 2 * err_den)) / den,
                                sqrt(err_num - (p ^ 2 * err_den)) / den)
          )

        }

        if (tolower(method[v]) %in% c("ratio")) {
          suppressWarnings(
            new_error <-  sqrt(err_num + (p ^ 2 * err_den)) / den
          )
        }



      } # END PROPORTION

      #################
      # AGGREGATION
      ##################

      if (tolower(method[v]) %in% c("aggregation", "agg") & division == FALSE) {

        wa <- which(dat@acs.colnames %in% variable)

        ft <- vector()
        for (i in 1:length(variable)) {

          if ( i == max(length(variable))) {
            x <- paste0("dat[,", wa[i], "]")
          }
          else {
            x <- paste0("dat[,", wa[i], "]", oper[i])
          }

          ft <- paste0(ft, x)

        }

        # ESTIMATES AND ERRORS

        est <- estimate(dat)
        err <- standard.error(dat)
        p  <-  estimate(eval(parse(text=ft)))

        # ONE ZERO  COMPUTATION

        if ( length(p) == 1 ) {

          if ( one.zero == TRUE ) {

            if (length(wa) > 1) {
              err_zero_agg <- max(err[, wa] ^ 2 * (est[, wa] == 0))
              err_agg <- sum(err[, wa] ^ 2 * (est[, wa] != 0))
            }
            else {
              err_zero_agg <- err[, wa] ^ 2 * (est[, wa] == 0)
              err_agg <- err[, wa] ^ 2 * (est[, wa] != 0)
            }

            err_agg <- err_zero_agg + err_agg

          }

          else if ( one.zero == FALSE ) {

            if (length(wa) > 1) {
              err_agg <- sum(err[, wa] ^ 2)
            }
            else {
              err_agg <- err[, wa] ^ 2
            }

          }

        }

        else if ( length(p) > 1 ) {

          if ( one.zero == TRUE ) {

            if (length(wa) > 1) {
              err_zero_agg <- apply(err[, wa] ^ 2 * (est[, wa] == 0), 1, max)
              err_agg <- apply(err[, wa] ^ 2 * (est[, wa] != 0), 1, sum)
            }
            else {
              err_zero_agg <- err[, wa] ^ 2 * (est[, wa] == 0)
              err_agg <- err[, wa] ^ 2 * (est[, wa] != 0)
            }

            err_agg <- err_zero_agg + err_agg

          }

          else if ( one.zero == FALSE ) {

            if (length(wa) > 1) {
              err_agg <- apply(err[, wa] ^ 2 , 1, sum)
            }
            else {
              err_agg <- err[, wa] ^ 2
            }

          }
        }

        # COMPUTING STANDARD ERRORS FOR AGGREGATION
        new_error <- sqrt(err_agg)

      } # END AGGREGATION

      #######################
      # CREATE DATASET
      #######################

      # CONDITIONS

      if (level[l] == "state") {
        output <- data.table(
          stfid = sprintf("%02d", as.numeric(geo$state)),
          sumlevel = "040",
          st_fips = geo$state,
          cnty_fips = NA,
          cnty_sub_fips = NA,
          tract_fips = NA,
          block_group = NA,
          cong_dist = NA,
          sch_dist_sec = NA,
          sch_dist_ele = NA,
          var_name = varname[v],
          est = as.vector(p),
          moe = as.vector(new_error * conf.level)
        )
      }

      if (level[l] == "county") {
        output <- data.table(
          stfid = paste0(sprintf("%02d", as.numeric(geo$state)), sprintf("%03d", as.numeric(geo$county))),
          sumlevel = "050",
          st_fips = geo$state,
          cnty_fips = geo$county,
          cnty_sub_fips = NA,
          tract_fips = NA,
          block_group = NA,
          cong_dist = NA,
          sch_dist_sec = NA,
          sch_dist_ele = NA,
          var_name = varname[v],
          est = as.vector(p),
          moe = as.vector(new_error * conf.level)
        )
      }


      if (level[l] == "county.subdivision") {
        output <- data.table(
          stfid = paste0(sprintf("%02d", as.numeric(geo$state)), sprintf("%03d", as.numeric(geo$county)), sprintf("%05d", as.numeric(geo$countysubdivision))),
          sumlevel = "060",
          st_fips = geo$state,
          cnty_fips = geo$county,
          cnty_sub_fips = geo$countysubdivision,
          tract_fips = NA,
          block_group = NA,
          cong_dist = NA,
          sch_dist_sec = NA,
          sch_dist_ele = NA,
          var_name = varname[v],
          est = as.vector(p),
          moe = as.vector(new_error * conf.level)
        )
      }

      if (level[l] == "tract") {
        output <- data.table(
          stfid = paste0(sprintf("%02d", as.numeric(geo$state)), sprintf("%03d", as.numeric(geo$county)), sprintf("%06d", as.numeric(geo$tract))),
          sumlevel = "140",
          st_fips = geo$state,
          cnty_fips = geo$county,
          cnty_sub_fips = NA,
          tract_fips = geo$tract,
          block_group = NA,
          cong_dist = NA,
          sch_dist_sec = NA,
          sch_dist_ele = NA,
          var_name = varname[v],
          est = as.vector(p),
          moe = as.vector(new_error * conf.level)
        )
      }



      if (level[l] == "block.group") {
        output <- data.table(
          stfid = paste0(geo$state, sprintf("%03d", as.numeric(geo$county)), sprintf("%06d", as.numeric(geo$tract)), as.numeric(geo$blockgroup)),
          sumlevel = "150",
          st_fips = geo$state,
          cnty_fips = geo$county,
          cnty_sub_fips = NA,
          tract_fips = geo$tract,
          block_group = geo$blockgroup,
          cong_dist = NA,
          sch_dist_sec = NA,
          sch_dist_ele = NA,
          var_name = varname[v],
          est = as.vector(p),
          moe = as.vector(new_error * conf.level)
        )
      }


      if (level[l] == "congressional.district") {
        output <- data.table(
          stfid = paste0(sprintf("%02d", as.numeric(geo$state)), sprintf("%02d", as.numeric(geo$congressionaldistrict))),
          sumlevel = "500",
          st_fips = geo$state,
          cnty_fips = NA,
          cnty_sub_fips = NA,
          tract_fips = NA,
          block_group = NA,
          cong_dist = geo$congressionaldistrict,
          sch_dist_sec = NA,
          sch_dist_ele = NA,
          var_name = varname[v],
          est = as.vector(p),
          moe = as.vector(new_error * conf.level)
        )
      }

      if (level[l] == "school.district.elementary") {
        output <- data.table(
          stfid = paste0(sprintf("%02d", as.numeric(geo$state)), sprintf("%05d", as.numeric(geo$schooldistrictelementary))),
          sumlevel = "950",
          st_fips = geo$state,
          cnty_fips = NA,
          cnty_sub_fips = NA,
          tract_fips = NA,
          block_group = NA,
          cong_dist = NA,
          sch_dist_sec = NA,
          sch_dist_ele = geo$schooldistrictelementary,
          var_name = varname[v],
          est = as.vector(p),
          moe = as.vector(new_error * conf.level)
        )
      }

      if (level[l] == "school.district.secondary") {
        output <- data.table(
          stfid = paste0(sprintf("%02d", as.numeric(geo$state)), sprintf("%05d", as.numeric(geo$schooldistrictsecondary))),
          sumlevel = "960",
          st_fips = geo$state,
          cnty_fips = NA,
          cnty_sub_fips = NA,
          tract_fips = NA,
          block_group = NA,
          cong_dist = NA,
          sch_dist_sec = geo$schooldistrictsecondary,
          sch_dist_ele = NA,
          var_name = varname[v],
          est = as.vector(p),
          moe = as.vector(new_error * conf.level)
        )
      }

      vdata <- rbind(vdata, output)

    } # END LEVEL LOOP

  } # END VARIABLE LOO

  print(". . . . . .  Formating output  . . . . . ")

  wdata <- dcast(vdata, stfid + sumlevel + st_fips + cnty_fips + cnty_sub_fips
                 + tract_fips + block_group + cong_dist + sch_dist_sec + sch_dist_ele
                 ~ var_name, value.var = c("est", "moe"))

  vars  <- names(wdata)
  setnames(wdata, names(wdata), gsub("_est", "", vars))

  ids <- c("stfid","sumlevel","st_fips","cnty_fips","cnty_sub_fips","tract_fips","block_group","cong_dist","sch_dist_sec","sch_dist_ele")

  vars  <- names(wdata[,!ids, with=FALSE])
  vars <- sort(vars)
  fdata <- wdata[, c(ids, vars), with = FALSE]
  setkey(fdata, sumlevel)

  # WRITE CSV


  if (is.null(file)) {
    print(". . . . . .  Done!  . . . . . ")
    return(fdata)
  }

  else {
    write.csv(fdata, file=file)
    print(". . . . . .  Data exported to a CSV file  . . . . . ")
  }

} # END FUNCTION
