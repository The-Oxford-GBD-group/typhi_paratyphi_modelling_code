## dcp_refresh_background_shps ------------------------------------------------>
#'
#' @title Data Coverage Plots: refresh the DCP background and disputed shapefile
#'
#' @description Load the newest version of the background shapefile (called
#' "master_shape_all" in the code) and the disputed borders shapefile to a
#' central directory, "/share/geospatial/rds_shapefiles/gdcv_custom/"
#'
#' @param core_repo LBD core path, used to load MBG functions and libraries
#'
#' @return NULL
#'
#' @export
dcp_refresh_background_shps <- function(
                                        core_repo = "/share/code/geospatial/lbd_core/") {
  # Requires sourcing 'mbg_central/setup.R' from LBD core
  source(paste0(core_repo, "/mbg_central/setup.R"))
  load_R_packages(package_list = c("sf", "sp", "data.table", "dplyr"))
  # Define output folder
  save_dir <- "/share/geospatial/rds_shapefiles/"

  # Load most recent shapefiles
  ad0_shp <- sf::read_sf(
    get_admin_shapefile(admin_level = 0, type = "admin", version = "current")
  ) %>% as(., "Spatial")
  ad0_shp@data <- as.data.table(ad0_shp@data)
  disp_shp <- sf::read_sf(
    get_admin_shapefile(type = "disputed_mask", version = "current")
  ) %>% as(., "Spatial")
  disp_shp@data <- as.data.table(disp_shp@data)
  disp_shp@data[, row_id := .I ]

  # Load metadata for the disputed mask and merge onto the dataset
  # NOTE: In the future, this is something that could be added directly to the
  #  disputed shapefile in J:/WORK/11_geospatial
  disp_meta <- fread(paste0(save_dir, "disputed_meta.csv"))
  disp_meta_merged <- merge(
    x = disp_shp@data,
    y = disp_meta,
    by = "ADM0_NAME",
    all.x = TRUE
  )
  disp_meta_merged <- disp_meta_merged[order(row_id)]
  disp_shp@data$claimants <- disp_meta_merged$claimants

  # Save both shapefiles to the 'quick shapefile' directory
  saveRDS(ad0_shp, file = paste0(save_dir, "gdcv_background_shp.rds"))
  saveRDS(disp_shp, file = paste0(save_dir, "gdcv_disputed_shp.rds"))
  message("New background and disputed shapefiles saved successfully.")
}
