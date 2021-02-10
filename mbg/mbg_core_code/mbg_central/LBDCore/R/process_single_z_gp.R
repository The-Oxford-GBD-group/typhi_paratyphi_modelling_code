#' @title Process single z group for aggregation
#'
#' @description Determines aggregation weights for aggregated z data using
#' worldpop numbers.
#'
#' @param ages String that indicates the z values that are being aggregated over.
#' @param reg String, name of country.
#' @param age_table Data table that has unique z aggregations, countries, years,
#' longitude, and latitude.
#' @param z_map Dataframe with mapping from z values to worldpop age groups.
#' @param simple_polygon The simple polygon of the modeling region.
#' @param pop_release The population measure release to use.
#' @param interval_mo The number of months between time steps.
#' @param type String indicating what z represents. Currently only age is supported.
#'
#' @return age_table that has 2 additional columns: z (list of the values that
#' make up z_ag) and agg_weight (list of the aggregation weights for each z).
#' @export
process_single_z_gp <- function(ages, reg, age_table, z_map,
                                simple_polygon,
                                pop_release, interval_mo,
                                type = "age") {

  # User does not specify weights
  # Need to read in from worldpop
  if (type == "age") {
    ages_needed <- eval(parse(text = as.character(ages)))

    for (year_needed in unique(age_table[z_ag == ages & country == reg]$year)) {
      worldpop_cov_rasters <- lapply(ages_needed, function(age) {
        suppressMessages(load_worldpop_covariate(simple_polygon,
          covariate = "worldpop",
          pop_measure = paste0("a", z_map$value[z_map$z == age], "t"),
          pop_release = pop_release,
          start_year = year_needed,
          end_year = year_needed,
          interval = as.numeric(interval_mo)
        )$worldpop)
      })
      worldpop_stack <- stack(worldpop_cov_rasters)
      longlats <- age_table[country == reg & z_ag == ages & year == year_needed, .(longitude, latitude)]
      loc_rel <- SpatialPoints(
        as.matrix(longlats),
        crs(simple_polygon)
      )
      pop_vals <- extract(worldpop_stack, loc_rel)
      if (any(is.na(pop_vals))) {
        stop(paste0(
          "There are missing population values for country: ", reg,
          " in year ", year_needed
        ))
      }
      pop_vals <- pop_vals / apply(pop_vals, 1, sum)
      age_vals <- ages_needed
      # iterate through each long/lat
      for (i in 1:nrow(longlats)) {
        age_table[
          country == reg & z_ag == ages & year == year_needed &
            longitude == longlats$longitude[i] & latitude == longlats$latitude[i],
          c("agg_weight", "z") := list(list(as.vector(pop_vals[i, ])), list(age_vals))
        ]
      }
    }
  } else {
    stop("The type of z aggregation must be set to 'age'")
  }
  return(age_table)
}
