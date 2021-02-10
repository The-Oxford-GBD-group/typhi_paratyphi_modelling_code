## SECTION 3 - SHAPEFILE PREP AND VALIDATION FUNCTIONS
#' @title Simplify SpatialPolygonsDataFrame
#'
#' @description Reduce the number of vertices in a polygon,
#' wrapper function for gSimplify.
#'
#' @param spdf A SpatialPolygonsDataFrame to be simplified
#' @param tol The tolerance which determines the degree of simplification
#' (see Douglas-Peuker algorithm)
#'
#' @return A simplified SpatialPolygonsDataFrame
#'
#' @export
simplify_spdf <- function(spdf, tol = tolerance) {
  df_spdf <- data.frame(spdf)
  spdf <- gSimplify(spdf, tol = tol, topologyPreserve = T)
  spdf <- SpatialPolygonsDataFrame(spdf, df_spdf)
  return(spdf)
}
