#' Pull polygons for a single shapefile
#'
#' @description Pulls the polygons from a master shapefile based on input data.table.
#'
#' @param shape_loc A data.table with columns 'shapefile' and 'location_code', where
#' 'location_code' corresponds to the GAUL_CODE column of corresponding 'shapefile'
#' @param fast_shapefiles Boolean. If true, reads in shapefile from RDS
#' (see lbd_core/mbg_central/polygon_functions.R), else reads from shapefile
#' directory
#'
#' @return A SpatialPolygonsDataFrame with polygons specified in shape_loc
#'
#' @export
pull_polys <- function(shape_loc, fast_shapefiles) {
  # Function to pull in a shapefile and assign each polygon the latest year
  # that any data was collected within it (determines fill color)

  # This is computationally intensive if many shapefiles

  shape <- unique(shape_loc$shapefile)
  loc_codes <- unique(shape_loc$location_code)

  message(paste0("  Working on ", shape))

  # Read in the master shapefile
  if (fast_shapefiles == T) {
    master_shape <- fast_load_shapefile(shape)
  } else {
    master_shape <- readOGR(
      dsn = paste0(j_root, "WORK/11_geospatial/05_survey shapefile library/Shapefile directory"),
      layer = shape
    )
  }

  message(paste0("    CRS: ", crs(master_shape)))

  names(master_shape)[names(master_shape) == "GAUL_Code"] <- "GAUL_CODE"

  # Subsetting will break if NAs in row index (GAUL_CODE)
  master_shape <- master_shape[!is.na(master_shape$GAUL_CODE), ]

  # Custom fixes for broken shapefiles
  if (shape == "AZE_DHS_2006" & "GAUL_CODE" %in% names(master_shape) == F) {
    master_shape$GAUL_CODE <- master_shape$REGCODE
  }

  # Subset to the relevant data & shapefile bits (by gaul code)
  subset_shape <- master_shape[master_shape@data$GAUL_CODE %in% loc_codes, ]

  # Remove the data that we don't need & make a standard output format
  return_shape <- subset_shape[names(subset_shape) %in% c("GAUL_CODE")]

  # Remove any duplicates
  return_shape <- return_shape[duplicated(return_shape$GAUL_CODE) == F, ]

  # Convert GAUL_CODE to numeric
  return_shape@data$GAUL_CODE <- as.numeric(as.character(return_shape@data$GAUL_CODE))

  return(return_shape)
}
