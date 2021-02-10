#' Prep input data.table for map-making
#'
#' @description: Merge on country identifiers and get the input data into a
#' standardized format.
#'
#' @param df Input data.table, after it has been cleaned
#' @param country_table Table of countries (admin0s) from the GBD shared DB
#' @param region Name of the region to be modeled
#'
#' @return Returns a list of four data.tables:
#' * 'df': The full prepped data, merged with GBD locations
#' * 'df_summary': The prepped data, collapsed to 1 row per country-year-NID
#' * 'df_point': The prepped data, subset to point data only
#' * 'df_poly': The prepped data, subset to poly data only
#'
#' @export
dcp_merge_with_gbd_locations <- function(df, country_table, region) {
  message("Combining dataframe with GBD country data...")
  # 1. Merge on country identifiers ------------------------------------------

  df <- merge(df, country_table, by = "country", all = T)

  if (region %in% c("africa", "africa_no_yem")) {
    # Rename "North Africa and Middle East" to just "North Africa"
    df <- df[
      region_name == "North Africa and Middle East",
      region_name := "North Africa"
    ]
  } else if (region == "middle_east") {
    # Rename "North Africa and Middle East" to just "Middle East"
    df <- df[
      region_name == "North Africa and Middle East",
      region_name := "Middle East"
    ]
  }

  # Drop non-matched countries & notify user
  num_na_rows <- nrow(df[is.na(country)])
  if (num_na_rows > 0) {
    message(paste0(
      "  Dropping ", nrow(df[is.na(country)]),
      " rows without matches in GBD country table."
    ))
    message(paste0(
      "  Countries affected: ",
      paste(unique(df[is.na(country), location_name]), collapse = ", ")
    ))
    message(paste0(
      "  Cluster_ids affected: ",
      paste(unique(df[is.na(country), cluster_id]), collapse = ", ")
    ))
  }
  df <- df[!is.na(country), ]

  # Truncate long country names
  df <- df[location_name == "Democratic Republic of the Congo", location_name := "DRC"]
  df <- df[location_name == "Central African Republic", location_name := "CAR"]
  df <- df[location_name == "Sao Tome and Principe", location_name := "STP"]
  df <- df[location_name == "United Arab Emirates", location_name := "UAE"]
  df <- df[location_name == "Equatorial Guinea", location_name := "Eq. Guinea"]
  df <- df[
    location_name == "Saint Vincent and the Grenadines",
    location_name := "St. Vin. & Grenadines"
  ]


  # 2. Generate subsets of data for further analysis -------------------------

  # Split off a point & polygon data set for use later
  df_point <- df[pointpoly == "Point"]
  df_poly <- df[pointpoly == "Polygon"]

  # Sum over the N of the group - includes rows for countries with no data
  df_summary <- df[, .(n = sum(N), count = .N), by = .(
    source, country, year,
    pointpoly, location_name,
    region_name, nid
  )]
  return(list(
    "df" = df,
    "df_point" = df_point,
    "df_poly" = df_poly,
    "df_summary" = df_summary
  ))
}
