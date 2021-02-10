#' Check for missing shapefiles
#'
#' @description Checks that all shapefiles listed in the df_graph_poly data.table exist
#' in the location they are being pulled from (determined by `fast_shapefiles`).
#'
#' @param df_graph_poly A data.table that only has data matched to polygons
#' @param fast_shapefiles Boolean. If true, reads in shapefile from RDS,
#' else reads from shapefile directory using readOGR
#'
#' @return A list with 2 objects -
#' * 'shapefiles': A list of shapefiles that exist in directory
#' * 'not_real_shapefiles': A list of shapefiles that do not exist in directory
#'
#' @export
dcp_check_for_missing_shapefiles <- function(df_graph_poly, fast_shapefiles) {
  # Pull a list of shapefiles for the polygon data for the region in question
  shapefiles <- unique(df_graph_poly$shapefile) %>%
    as.character() %>%
    tolower()

  ## Check that all shapefile entries are real shapefiles and report bad entries
  if (fast_shapefiles) {
    real_shapefiles <- gsub(
      ".rds", "",
      tolower(list.files("/share/geospatial/rds_shapefiles",
        pattern = ".rds"
      ))
    )
  } else {
    real_shapefiles <- gsub(
      ".shp", "",
      tolower(list.files(paste0(
        j_root, "WORK/11_geospatial/",
        "05_survey shapefile library/",
        "Shapefile directory"
      ),
      pattern = ".shp"
      ))
    )
  }
  not_real_shapefiles <- shapefiles[!(shapefiles %in% real_shapefiles)]

  shapefiles <- shapefiles[(shapefiles %in% real_shapefiles)]

  return(list(
    "shapefiles" = shapefiles,
    "not_real_shapefiles" = not_real_shapefiles
  ))
}
