#' Make df_graph
#'
#' @description Make the df_graph object - subsetted to countries in country_list & formatted
#'
#' @param df_summary The summary prepped data.table object output by the
#' dcp_merge_with_gbd_locations() function
#' @param country_list Vector of ISO3 codes from get_country_list() function
#'
#' @return The df_summary df subsetted to the countries in country list and
#' formatted to handle na values in geospatial variables and new_data
#'
#' @export
dcp_make_df_graph <- function(df_summary, country_list) {
  df_graph <- df_summary[country %in% country_list]

  # Fix NAs so that they don't graph
  message(paste0("\nFound ", nrow(df_graph[is.na(pointpoly)]), " rows not designated by point or poly."))
  message("Typically, this indicates that there are countries with no data - check this assumption if a large number")
  message("Fixing to ensure no NAs in legend...")
  df_graph[is.na(pointpoly), n := 0]
  df_graph[is.na(pointpoly), source := unique(df_graph$source[!is.na(df_graph$source)])[1]]
  df_graph[is.na(pointpoly), pointpoly := "Point"]

  df_graph$location_name <- factor(df_graph$location_name,
    levels = rev(sort(unique(df_graph$location_name)))
  )

  df_graph[new_data == 0, new_data_lab := "No"]
  df_graph[new_data == 1, new_data_lab := "Yes"]
  df_graph[is.na(new_data), new_data_lab := "No"]

  return(df_graph)
}
