#' Get list of countries in modelling region
#'
#' @description Pull a country list based on your data & region of choice
#'
#' @param df The dataframe output by the dcp_merge_with_gbd_locations() function
#' @param region Name of the region to be modeled
#' @param region_title Title used for plots: default is region to be modeled
#'
#' @return returns a list with the following 3 objects -
#' * 'reg_title': A character string of the region
#' * 'region_list': A vector of the modelling regions associated with the region passed in
#' * 'country_list': A vector of country ISO3 codes within the region
#'
#' @export
get_country_list <- function(df, region, region_title) {

  # Note: need to figure out how to deal with Oceania and Central Asia

  if (region == "africa") {
    reg_title <- "Africa"
    region_list <- c(
      "North Africa",
      "Central Sub-Saharan Africa",
      "Eastern Sub-Saharan Africa",
      "Western Sub-Saharan Africa",
      "Southern Sub-Saharan Africa"
    )
    country_list <- unique(df[region_name %in% region_list]$country)
    country_list <- unique(c(country_list, "YEM"))

    # This is tough because of NAME including middle east - need to manually remove and add some
    remove_countries <- c("AFG", "ARE", "IRN", "IRQ", "JOR", "OMN", "PSE", "SAU", "SYR", "TUR", "KWT", "LBN", "QAT", "BHR", "CPV")

    country_list <- country_list[!(country_list %in% remove_countries)]
  }

  else if (region == "africa_no_yem") {
    reg_title <- "Africa"
    region_list <- c(
      "North Africa",
      "Central Sub-Saharan Africa",
      "Eastern Sub-Saharan Africa",
      "Western Sub-Saharan Africa",
      "Southern Sub-Saharan Africa"
    )
    country_list <- unique(df[region_name %in% region_list]$country)
    # This is tough because of NAME including middle east - need to manually remove and add some
    remove_countries <- c(
      "AFG", "ARE", "IRN", "IRQ", "JOR", "OMN", "PSE", "SAU",
      "SYR", "TUR", "KWT", "LBN", "QAT", "BHR", "CPV", "YEM"
    )
    country_list <- country_list[!(country_list %in% remove_countries)]
  }

  else if (region %in% c("south_asia", "south_asia_ind_collaborators")) {
    reg_title <- "South Asia"
    region_list <- c("South Asia")
    country_list <- unique(df[region_name %in% region_list]$country)

    # add Sri Lanka
    country_list <- unique(c(country_list, "LKA"))
  }

  else if (region == "se_asia") {
    reg_title <- "East and Southeast Asia"
    region_list <- c(
      "East Asia",
      "Southeast Asia"
    )
    country_list <- unique(df[region_name %in% region_list]$country)

    # move Sri Lanka & the Maldives to south_asia
    remove_countries <- c("LKA", "MDV")
    country_list <- country_list[!(country_list %in% remove_countries)]

    # add PNG and Mongolia
    country_list <- unique(c(country_list, "PNG", "MNG"))
  }

  else if (region == "latin_america") {
    reg_title <- "Latin America and Caribbean"
    region_list <- c(
      "Andean Latin America",
      "Caribbean",
      "Central Latin America",
      "Tropical Latin America"
    )
    country_list <- unique(df[region_name %in% region_list]$country)
    country_list <- unique(c(country_list, "CUB"))
  }

  else if (region == "south_america") {
    reg_title <- "South America"
    region_list <- c(
      "Andean Latin America",
      "Tropical Latin America"
    )
    country_list <- unique(df[region_name %in% region_list]$country)

    # add Venezuela & others
    add_countries <- remove_countries <- c("VEN", "COL", "GUY", "SUR")
    country_list <- unique(c(country_list, add_countries))
  }

  else if (region == "south_america_mex") {
    reg_title <- "South America"
    region_list <- c(
      "Andean Latin America",
      "Tropical Latin America"
    )
    country_list <- unique(df[region_name %in% region_list]$country)

    # add Venezuela & others
    add_countries <- remove_countries <- c("VEN", "COL", "GUY", "SUR", "MEX")
    country_list <- unique(c(country_list, add_countries))
  }

  else if (region == "central_america") {
    reg_title <- "Central Latin America and Carribean"
    region_list <- c(
      "Central Latin America",
      "Caribbean"
    )
    country_list <- unique(df[region_name %in% region_list]$country)

    # remove Venezuela & others
    remove_countries <- c("VEN", "COL", "GUY", "SUR")
    country_list <- country_list[!(country_list %in% remove_countries)]
  }

  else if (region == "central_america_no_mex") {
    reg_title <- "Central Latin America and Carribean"
    region_list <- c(
      "Central Latin America",
      "Caribbean"
    )
    country_list <- unique(df[region_name %in% region_list]$country)

    # remove Venezuela & others
    remove_countries <- c("VEN", "COL", "GUY", "SUR", "MEX")
    country_list <- country_list[!(country_list %in% remove_countries)]
  }

  else if (region == "eastern_europe") {
    reg_title <- "Eastern Europe"
    region_list <- c("Eastern Europe")
    country_list <- unique(df[region_name %in% region_list]$country)

    remove_countries <- c("RUS")
    country_list <- country_list[!(country_list %in% remove_countries)]
  }

  else if (region == "middle_east") {
    reg_title <- "Middle East and Central Asia"
    region_list <- c("Middle East")
    country_list <- unique(df[region_name %in% region_list]$country)

    remove_countries <- c("EGY", "SDN", "TUN", "DZA", "LBY", "MAR")
    country_list <- country_list[!(country_list %in% remove_countries)]

    # Add in several Central Asia countries:
    # Uzbekistan, Turkmenistan, Tajikistan, Kyrgyzstan, Lebanon
    country_list <- unique(c(country_list, "UZB", "TKM", "TJK", "KGZ", "LBN"))
  }

  else if (region == "stage2") {
    reg_title <- "Low and Middle-Income Countries"
    # Load stage metadata and subset to stage 2
    lookup_table <- load_gaul_lookup_table()
    stage2 <- lookup_table[Stage != "3"]
    # Get all unique regions and countries in stage 2
    region_list <- unique(stage2[, reg_name])
    country_list <- toupper(unique(stage2[, iso3]))
  }

  else if (region == "stage3") {
    reg_title <- "High-Income Countries"
    # Load stage metadata and subset to stage 3
    lookup_table <- load_gaul_lookup_table()
    stage3 <- lookup_table[Stage = "3"]
    # Get all unique regions and countries in stage 3
    region_list <- unique(stage3[, reg_name])
    country_list <- toupper(unique(stage3[, iso3]))
  }

  # statement to deal with custom cases
  else {
    # can manually specify region title for plot
    if (is.null(region_title)) {
      reg_title <- toupper(region)
    } else {
      reg_title <- region_title
    }

    gaul_list <- get_gaul_codes(region)

    # create dictionary
    stage_master_list <- read.csv("/snfs1/WORK/11_geospatial/10_mbg/stage_master_list.csv")

    # create gaul to iso3 name dictionary
    iso3_names <- as.list(stage_master_list$iso3)
    iso3_names <- lapply(iso3_names, as.character)
    names(iso3_names) <- lapply(stage_master_list$GAUL_CODE, as.character)
    iso3_dict <- list2env(iso3_names, hash = TRUE)

    country_list <- c()
    # add to country list
    for (gc in gaul_list) {
      iso3_nm <- get(as.character(gc), envir = iso3_dict)
      country_list <- unique(c(country_list, iso3_nm))
    } # looks up country name in dictionary

    region_list <- unique(df[country %in% country_list]$region_name)
    region_list <- sort(region_list)
  }

  return(list(
    "reg_title" = reg_title,
    "region_list" = region_list,
    "country_list" = country_list
  ))
}
