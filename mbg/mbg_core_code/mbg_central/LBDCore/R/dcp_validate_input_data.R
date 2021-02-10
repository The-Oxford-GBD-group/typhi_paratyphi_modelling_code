#' Validate input data.frame
#'
#' Description: Checks that the input data contains the minimum columns needed
#' to create the data coverage plots, and that those columns are coerced to the
#' correct data types before continuing. This function also changes the mapped
#' variable column to a column named 'outcome' in order to simplify future
#' functions.
#'
#' @param df The input data.frame. Must contain at least the following columns,
#' which will be coerced to the following data types:
#' * 'nid' (integer):  This field was called 'svy_id' in the original code
#' * 'country' (char): ISO3 code associated with the survey country
#' * 'source' (char):  Name of the data source category (eg, "MACRO_DHS")
#' * 'year' (integer): Year, either when the data was collected or when an
#' event occurred.
#' * 'N' (numeric):    The sample size at the given location.
#' * <VAR> (numeric):  The value of the outcome of interest at the survey site.
#' Often expressed as a rate (eg. num events / sample size).
#' * 'cluster_id' (integer): Index assigning unique unique observations from
#' the input data. Typically used for debugging.
#' * 'latitude' (numeric):  Latitude associated with the observation, if available
#' * 'longitude' (numeric): Longitude associated with the observation, if available
#' * 'shapefile' (char):    Shapefile associated with the observation. NOTE:
#' each observation must have either a valid
#' latitude and longitude or a shapefile and location
#' code, or else it will be dropped
#' * 'location_code' (integer): Location code in the given polygon that is
#' associated with the observation. In the survey
#' shapefile library and the codebooks, these are
#' currently called the "GAUL_CODE"
#' Rows containing NA or empty values in the first seven fields will be
#' dropped. All rows must have either 'latitude' and 'longitude' fields filled,
#' or they will be dropped.
#' @param var The outcome that is being mapped. Must correspond to a field
#' in the data.frame
#' @param year_var the field in the data.frame containing the year.
#' @param debug Stops execution of the function rather than dropping any rows
#' in the input data that do not meet the criteria above. Default FALSE.
#'
#' @return Returns a data.table with the following changes made:
#' * Converted to a data.table
#' * Columns coerced to their corresponding type
#' * Mapped variable field (defined by param 'var') renamed "outcome"
#' * All extra columns dropped
#' * Adds a new field, 'pointpoly', indicating whether latitude and longitude
#' data is available
#' * Rows not meeting the required completeness criteria dropped
#'
#' @export
dcp_validate_input_data <- function(df,
                                    var = "outcome",
                                    year_var = "year",
                                    debug = FALSE) {
  message("Validating input data...")
  # Check that the input data type is a data.frame
  if (!("data.frame" %in% class(df))) {
    stop(paste0(
      "The input data type must be a",
      " data.frame or data.table."
    ))
  }
  # Coerces input data to data.table
  df <- as.data.table(df)
  # Check that all required columns are in the data
  keep_cols <- c(
    "nid", "country", "source", "N", "cluster_id", "latitude",
    "longitude", "shapefile", "location_code", year_var, var
  )
  missing_cols <- keep_cols[!(keep_cols %in% names(df))]
  if (length(missing_cols) > 0) {
    stop(paste0(
      "The input data is missing the ",
      "following fields: ",
      paste(missing_cols, collapse = ", ")
    ))
  }
  # Rename the var column to 'outcome'
  if (var != "outcome") df[, outcome := get(var)]
  # Rename the year_var column to 'year'
  if (year_var != "year") df[, year := get(year_var)]

  # Subset to only necessary columns
  keep_cols[length(keep_cols)] <- "outcome"
  keep_cols[length(keep_cols) - 1] <- "year"
  df <- df[, keep_cols, with = FALSE]

  # Helper function for coercing data fields
  coerce_field <- function(field, field_name, coerce_type) {
    start_nas <- sum(is.na(field))
    # Convert the field to the specified data type,
    field <- suppressWarnings(eval(parse(text = paste0("as.", coerce_type, "(field)"))))
    # Remove empty character strings, replacing with NA
    if (coerce_type == "character") {
      field[field == ""] <- NA
    }
    end_nas <- sum(is.na(field))
    if (end_nas > 0) {
      message(paste0(
        "  ", end_nas, " NAs in field '", field_name,
        "' (", end_nas - start_nas,
        " introduced during coercion to ",
        coerce_type, ")"
      ))
    }
    return(field)
  }
  # Coerce the following fields to integer vectors
  for (col in c("nid", "year", "location_code")) {
    df[, temp := coerce_field(df[, get(col)], col, "integer")]
    df[, (col) := NULL]
    setnames(df, "temp", col)
  }
  # Coerce the following fields to numeric vectors
  for (col in c("N", "outcome", "latitude", "longitude")) {
    df[, temp := coerce_field(df[, get(col)], col, "numeric")]
    df[, (col) := NULL]
    setnames(df, "temp", col)
  }
  # Coerce the following fields to character vectors
  for (col in c("country", "source", "cluster_id", "shapefile")) {
    df[, temp := coerce_field(df[, get(col)], col, "character")]
    df[, (col) := NULL]
    setnames(df, "temp", col)
  }
  # Helper function defining behavior for dropping rows based on a true-false
  #  series
  informatively_drop <- function(in_df, keep_condition, drop_reason, debug = FALSE) {
    start_nrow <- nrow(in_df)
    problem_rows <- unique(in_df[!keep_condition, cluster_id])
    sub_df <- in_df[keep_condition, ]
    dropped_nrow <- start_nrow - nrow(sub_df)
    if (dropped_nrow > 0) {
      message(paste0(
        "  ", dropped_nrow, " rows out of ", start_nrow, " total dropped",
        " from the input data due to ", drop_reason, "."
      ))
      message(paste0("  Cluster_ids dropped: ", paste(problem_rows, collapse = ", ")))
      if (debug) {
        stop("Validation stopped due to dropped rows ('debug' is on).")
      }
    }
    return(sub_df)
  }
  # Drop any rows containing NA values in any field that does NOT define
  #  geography
  geog_cols <- c("latitude", "longitude", "shapefile", "location_code")
  survey_cols <- keep_cols[!(keep_cols %in% geog_cols)]
  not_missing_survey_cols <- apply(!is.na(df[, survey_cols, with = F]), 1, FUN = all)
  df <- informatively_drop(df,
    keep_condition = not_missing_survey_cols,
    drop_reason = paste0(
      "missing identifiers\n   (not ",
      "including missing geography ",
      "information)"
    ),
    debug = debug
  )
  # Drop any rows that do not have the required geography data (either 'latitude'
  #  and 'longitude' or 'shapefile' and 'location_code')
  has_latlong <- !is.na(df$latitude) & !is.na(df$longitude)
  has_poly <- !is.na(df$shapefile) & !is.na(df$location_code)
  has_geo_data <- has_latlong | has_poly
  df <- informatively_drop(df,
    keep_condition = has_geo_data,
    drop_reason = "missing geographic data",
    debug = debug
  )
  final_nrow <- nrow(df)
  # The input data has been cleaned and validated.
  message(
    "  After input validation and cleaning, ", final_nrow, " rows of data ",
    "remain."
  )
  # Add a new field, 'pointpoly', indicating whether the data has latitude and
  #  longitude data available
  df[, pointpoly := "Point"]
  df[, is_point := !is.na(latitude) & !is.na(longitude)]
  df[ !(is_point), pointpoly := "Polygon"]
  df[, is_point := NULL]
  # Formatting for the field "source" - truncate to the first 15 characters
  df[, source := substr(source, 1, 15)]
  return(df)
}
