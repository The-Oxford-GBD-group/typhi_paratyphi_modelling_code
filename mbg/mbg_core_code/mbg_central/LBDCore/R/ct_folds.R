#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param xy PARAM_DESCRIPTION
#' @param ct PARAM_DESCRIPTION
#' @param ss PARAM_DESCRIPTION, Default: 1
#' @param n_folds PARAM_DESCRIPTION, Default: 5
#' @param ... PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if (interactive()) {
#'   # EXAMPLE1
#' }
#' }
#' @rdname ct_folds
#' @export
ct_folds <- function(xy, ## xy location matrix
                     ct, ## country vec
                     ss = 1, ## sample size vec (or 1 if equi-ss)
                     n_folds = 5,
                     ...) {
  ## ## example
  ## df <- read.csv('J:/temp/geospatial/U5M_africa/data/clean/fully_processed.csv',
  ##                stringsAsFactors = FALSE)
  ## df$long <- as.numeric(as.character(gsub(",", "", df$long)))
  ## df$lat  <- as.numeric(as.character(gsub(",", "", df$lat)))
  ## df <- df[-which(df$lat>90), ]
  ## data <- df
  ## shp_full <- shapefile('J:/temp/geospatial/U5M_africa/data/clean/shapefiles/africa_ad2.shp')
  ## folds <- ad2_folds(admin_raster='J:/temp/geospatial/U5M_africa/data/clean/shapefiles/ad2_raster.grd',
  ##                   shape_ident="gaul_code",
  ##                   admin_shps='J:/temp/geospatial/U5M_africa/data/clean/shapefiles/africa_ad2.shp',
  ##                   data=df,
  ##                   strat_cols=NULL,
  ##                   ss=data$exposed,
  ##                   xy=cbind(data$long, data$lat)
  ##                   n_folds=5,
  ##                   mask_shape='J:/temp/geospatial/U5M_africa/data/clean/shapefiles/africa_simple.shp',
  ##                   mask_raster='J:/temp/geospatial/U5M_africa/data/clean/shapefiles/ad0_raster')
  ## library(scales)
  ## cols <- folds
  ## cols[which(cols==1)] <- "cyan"
  ## cols[which(cols==2)] <- "red"
  ## cols[which(cols==3)] <- "blue"
  ## cols[which(cols==4)] <- "green"
  ## cols[which(cols==5)] <- "magenta"
  ## png("~/check_folds_plot.png", width=1080, height=1080)
  ## plot(shp_full)
  ## points(df$long, df$lat, col=alpha(cols, alpha=0.01), pch=16)
  ## dev.off()
  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~##
  ## (4) countries in space
  ##
  ## INPUTS:
  ##
  ## OUTPUTS:
  ##
  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~##

  if (length(unique(yr) < n_folds)) {
    message("Too many folds for too few countries! Expand your horizons")
    stop()
  }

  if (length(ss) == 1) ss <- rep(1, nrow(xy))

  ## first we find the sample size in each of the countries
  dt <- data.table(
    long = xy[, 1],
    lat = xy[, 2],
    ss = ss,
    ct = ct
  )

  ## get sample size totals in each country
  cts_in_ct <- dt[, sum(ss), by = ct]

  ## make the folds
  fold_vec <- make_folds_by_poly(
    cts_in_polys = as.data.frame(cts_in_ct),
    pt_poly_map = as.character(dt[, ct]),
    n_folds = n_folds
  )


  return(fold_vec)
}
