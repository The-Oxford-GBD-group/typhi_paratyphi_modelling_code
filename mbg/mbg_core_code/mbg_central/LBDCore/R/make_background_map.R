## SECTION 5 - GRAPHICS FUNCTIONS: DATA COVERAGE MAPS
#' Build background map
#'
#' @description Builds a background map of a region with just the country borders. Grays out
#' stage 3 countries if stage_3_gray is true
#'
#' @param region name of region to be mapped
#' @param endemic_gauls list of gaul codes. if non-null, the maps will be gray except
#' for these countries
#' @param simplify_polys boolean. if true, simplifies shapefiles by reducing
#' the number of vertices
#' @param tolerance numeric value that influences the degree of simplification of polygons
#' (see Douglas-Peuker algorithm)
#' @param master_shape_all a combined SPDF with all polygons necessary to make maps
#' @param disputed_shp an SPDF of all polygons in disputed regions, which will
#' be plotted separately
#' @param stage3 data.table of stage 3 countries to be removed
#' @param stage_3_gray boolean. If true, removes stage 3 countries from scatterplot
#'
#' @return returns a list of 4 objects -
#' * 'background_outline_gg': gg geom_path, outline of region
#' * 'background_map_gg': gg geom_poly, background map of region with country borders
#' * 'background_map_gg_not_endemic': gg geom_poly, background map
#' with countries marked non-endemic or stage 3 grayed out
#' * 'extent(background_map)': extent of polygon defining region
#'
#' @export
make_background_map <- function(region,
                                endemic_gauls,
                                simplify_polys,
                                tolerance,
                                fast_shapefiles,
                                master_shape_all,
                                disputed_shp,
                                stage3,
                                stage_3_gray) {
  # Define a list of data coverage plot regions and their corresponding regions
  #  in the `get_adm0_codes()` function
  dcp_region_list <- list(
    "africa" = "africa_dcp",
    "africa_no_yem" = "africa_dcp-yem",
    "south_asia" = "south_asia+LKA+MDV",
    "south_asia_ind_collaborators" = "south_asia+LKA+MDV+PAK",
    "se_asia" = "se_asia_dcp-LKA-MDV+PNG+TWN",
    "latin_america" = "latin_america_dcp+GUF+CUB",
    "central_america_no_mex" = "central_america-MEX",
    "south_america" = "south_america+GUF",
    "south_america_mex" = "south_america+GUF+MEX",
    "middle_east" = "middle_east_dcp",
    "stage1" = "all",
    "stage2" = "all",
    "stage3" = "all"
  )
  # Load the adm0 lookup table to use throughout this function (to avoid having
  #  to read the same table repeatedly)
  lookup_table <- load_adm0_lookup_table()
  # Get the region that will be used for the background map
  if (region %in% names(dcp_region_list)) {
    gaul_list <- suppressMessages(get_adm0_codes(
      dcp_region_list[[region]],
      lookup_table = lookup_table
    ))
  } else {
    gaul_list <- suppressMessages(get_adm0_codes(
      region,
      lookup_table = lookup_table
    ))
  }
  # Subset the background map to just these admin0 codes
  background_map <- master_shape_all[master_shape_all@data$GAUL_CODE %in% gaul_list, ]

  if (region == "south_asia_ind_collaborators") {
    message("  This region requires some extra shapefile processing...")
    disputed_shp <- disputed_shp[FALSE, ]
  }

  # Subset the disputed shapefile to keep only countries within the DCP region
  check_in_gaul_list <- function(this_reg) {
    this_reg_gauls <- suppressMessages(get_adm0_codes(
      this_reg,
      lookup_table = lookup_table
    ))
    overlapping_gauls <- intersect(this_reg_gauls, gaul_list)
    return((length(overlapping_gauls) > 0))
  }
  disputed_shp_sub <- disputed_shp[
    sapply(disputed_shp$claimants, FUN = check_in_gaul_list),
  ]

  ## Simplify, if specified, and then convert background SHP to a ggplot object
  if (simplify_polys == T) background_map <- simplify_spdf(background_map, tol = tolerance)
  background_map@data$id <- 1:dim(background_map@data)[1]
  background_map_df <- suppressMessages(fortify(background_map))
  background_map_df <- merge(background_map_df, background_map@data[, c("id", "GAUL_CODE")], by = "id")
  background_map_df <- as.data.table(background_map_df)

  background_outline_gg <- suppressMessages(
    geom_path(
      data = background_map,
      aes(
        x = long,
        y = lat,
        group = group
      )
    )
  )

  background_map_gg <- suppressMessages(
    geom_polygon(
      data = background_map,
      aes(
        x = long,
        y = lat,
        group = group
      ),
      fill = "white"
    )
  )

  ## Convert disputed shapefile into a ggplot object
  disputed_shp_df <- as.data.table(suppressMessages(fortify(disputed_shp_sub)))
  if (nrow(disputed_shp_df) == 0) {
    disputed_shp_df <- data.table(
      lat = numeric(0), long = integer(0), group = character(0)
    )
  }
  disputed_bg_gg <- suppressMessages(
    geom_path(
      data = disputed_shp_df,
      aes(x = long, y = lat, group = group),
      color = "white", size = .7
    )
  )
  disputed_shp_gg <- suppressMessages(
    geom_path(
      data = disputed_shp_df,
      aes(x = long, y = lat, group = group),
      color = "black", linetype = "dotted", size = .5
    )
  )

  if (!is.null(endemic_gauls)) {
    background_map_df_not_endemic <- background_map_df[!(GAUL_CODE %in% endemic_gauls), ]
    background_map_gg_not_endemic <- geom_polygon(
      data = background_map_df_not_endemic,
      aes(
        x = long,
        y = lat,
        group = group
      ),
      fill = "lightgray"
    )
  } else if (stage_3_gray) {
    background_map_df_not_endemic <- background_map_df[(GAUL_CODE %in% stage3$gadm_geoid), ]
    background_map_gg_not_endemic <- geom_polygon(
      data = background_map_df_not_endemic,
      aes(
        x = long,
        y = lat,
        group = group
      ),
      fill = "lightgray"
    )
  } else {
    background_map_gg_not_endemic <- NULL
  }

  return(list(
    background_outline_gg, background_map_gg, background_map_gg_not_endemic,
    extent(background_map), disputed_shp_gg, disputed_bg_gg
  ))
}
