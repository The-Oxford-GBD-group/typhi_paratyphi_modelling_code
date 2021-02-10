## SECTION 4 - GRAPHICS FUNCTIONS: SCATTER PLOTS
#' Make Table
#'
#' @description Makes the table to the right of the data scatterplot with the counts of
#' points and polygons in each country
#'
#' @param df_summary The df_summary data.table outputted by `dcp_merge_with_gbd_locations()`
#' @param country_list list of ISO3s outputted by `get_country_list()`
#' @param year_min earliest year, subsets the data to the minimum year
#' @param year_max latest year, subsets the data to the maximum year
#'
#' @return A data.table with the number of cumulative points and polygons in the
#' year range by country
#'
#' @export
dcp_make_table_new <- function(df_summary, country_list, year_min, year_max) {
  td <- as.data.table(df_summary)
  td <- td[(year >= year_min) &
    (year <= year_max) &
    (country %in% country_list), ]
  td <- td[, c("country", "pointpoly", "n", "count"), with = F]
  td <- td[, Count := sum(count), by = .(country, pointpoly)]
  td <- td[, N := sum(n), by = .(country, pointpoly)]
  td <- distinct(td, country, pointpoly, N, Count)

  td[is.na(pointpoly), N := 0]
  td[is.na(pointpoly), Count := 0]

  td_n <- td[, .(country, N)]
  td_n <- td[, N := sum(N), by = country]
  td_n <- distinct(td, country, N)

  td_count <- td %>%
    .[, N := NULL] %>%
    spread(pointpoly, Count, fill = 0) %>%
    as.data.table() %>%
    merge(., td_n, by = "country") %>%
    setnames("country", "Country")

  # Catch if either no point or no poly data
  if (!("Point" %in% names(td_count))) {
    td_count[, Point := 0]
  }
  if (!("Polygon" %in% names(td_count))) {
    td_count[, Polygon := 0]
  }

  setnames(td_count, c("Point", "Polygon"), c("Points", "Polygons"))

  # Add in countries with no data
  no_data_countries <- country_list[!(country_list %in% unique(td_count$Country))]
  ## Catch the cases where all countries are filled
  if (length(no_data_countries) > 0) {
    no_data_td <- data.table(
      Country = no_data_countries,
      Points = 0,
      Polygons = 0,
      N = 0
    )

    td_count <- rbind(td_count, no_data_td)
  }

  if ("<NA>" %in% names(td)) td[, "<NA>" := NULL]

  setcolorder(td_count, c("Country", "Points", "Polygons", "N"))

  return(td_count)
}
