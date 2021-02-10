# # Benchmarking
# library(tictoc)
# tic.clearlog()
# shapes <- c("ALB_2000_MICS", "ALB_2005_MICS", "2007_ETH_Admin2_Altered", "1994_ETH_Admin2_Altered")
# tic("ReadOGR from shapefile")
# tmp <- lapply(shapes, function(shape) readOGR(dsn = in_dir, layer = shape))
# toc(log = T)
# tic("readRDS from rds file via new function", log = T)
# tmp2 <- lapply(shapes, function(shape) fast_load_shapefile(shape))
# toc(log = T)
# tic.log(format = T)
# compare_log <- tic.log(format = F)
# compare_log <- lapply(compare_log, function(x) {
#                        x <- unlist(x)
#                        x <- t(x)
#                        x <- as.data.table(x)
#                        })
# compare_log <- rbindlist(compare_log)
# compare_log[, time := as.numeric(toc.elapsed) - as.numeric(tic.elapsed)]
# compare_log[1, time] / compare_log[2, time]
## Functions relating to offical world shapefiles
## get_admin_shape_dir
#' @title Return path to admin shapes directory
#'
#' @description
#' Returns path to the official LBD administrative shape file directory. This
#' actually includes non-shape data that is important for mapping, notably
#' the standard link table and standard id raster.
#'
#' @param version Admin shapefile version to pull
#' @export
get_admin_shape_dir <- function(version = "current") {
  paste0("/snfs1/WORK/11_geospatial/admin_shapefiles/", version, "/")
}
