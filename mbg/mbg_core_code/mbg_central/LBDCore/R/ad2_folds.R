#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param admin_raster PARAM_DESCRIPTION
#' @param shape_ident PARAM_DESCRIPTION, Default: 'gaul_code'
#' @param admin_shps PARAM_DESCRIPTION
#' @param ss PARAM_DESCRIPTION, Default: 1
#' @param xy PARAM_DESCRIPTION
#' @param n_folds PARAM_DESCRIPTION
#' @param mask_shape PARAM_DESCRIPTION
#' @param mask_raster PARAM_DESCRIPTION
#' @param ... PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if (interactive()) {
#'   # EXAMPLE1
#' }
#' }
#' @rdname ad2_folds
#' @export
ad2_folds <- function(admin_raster,
                      shape_ident = "gaul_code",
                      admin_shps,
                      ss = 1,
                      xy,
                      n_folds,
                      mask_shape,
                      mask_raster,
                      ...) {
  ## ## example
  ## df <- fread('J:/temp/geospatial/U5M_africa/data/clean/fully_processed.csv',
  ##            stringsAsFactors = FALSE)
  ## clean the df
  ## df$long <- as.numeric(as.character(gsub(",", "", df$long)))
  ## df$lat  <- as.numeric(as.character(gsub(",", "", df$lat)))
  ## df <- df[-which(df$lat > 90), ]
  ## data <- df
  ## shp_full <- plot_shp <- shapefile('J:/temp/geospatial/U5M_africa/data/clean/shapefiles/africa_ad2.shp')
  ## xy <- data[,c('long', 'lat')]
  ## ss <- data$exposed
  ## ts <- 1e6
  ## plot_fn <- 'quadtree.png'
  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~##
  ## (2) small aggregates in space - WEIGHTED K-MEANS
  ## not yet (or ever?) implemented. quadtree looks pretty good
  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~##
  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~##
  ## (3) admin2 in space
  ##
  ## INPUTS:
  ##
  ## OUTPUTS:
  ##
  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~##
  ## this function takes in admin2 (or any mutually exclusive and
  ## collectively exhaustive) shapefiles covering the domain of your
  ## data and splits your data into folds of approximately equal
  ## sample size using admin2 units to split the data

  ## admin_raster: file location of all pertinent shapefiles to use when folding (.grd)
  ## shape_ident: string identifying data col in shapefile used to uniquely identify polygons
  ## admin_shps: file location of associated raster for admin_raster (.shp)
  ## data: complete dataset that you want to fold
  ## strat_cols: vector of column string names to
  ##    stratify over when making folds. if NULL, holdout
  ##    sets are made across the entire data structure
  ## ss: vector of sample sizes for each row. if <1>, assumes all have ss=1
  ## xy: matrix of xy coordinates
  ## n_folds: number of folds to make
  ## mask_shape: shapefile file location for boundary of area to be folded
  ## mask_raster: raster to project mask_shape onto

  ## make a mask for ther region we care about
  mask <- rasterize_check_coverage(shapefile(mask_shape), raster(mask_raster), field = names(shapefile(mask_shape))[1]) * 0

  ## get raster cells in mask
  cell_idx <- cellIdx(mask)

  ## load raster and shapefile for admin units
  rast <- raster(admin_raster)
  rast_cell <- extract(rast, cell_idx)
  shp_full <- shapefile(admin_shps)
  shp <- shp_full@data[shape_ident]
  ## plot(shp_full, col=1:length(shp_full))

  ## get number of datapoints in shapefiles
  shp_cts <- get_sample_counts(
    ss = ss,
    xy = xy,
    shapes = shp_full,
    shape_ident = shape_ident
  )
  pt_shp_map <- shp_cts$pt_poly_map
  cts_in_shps <- shp_cts$ct_mat

  ## make the folds by using the polygons (ad2 units) as the

  ## sampling unit to divide the data into the folds while keeping a
  ## similar sample size in each fold
  fold_vec <- make_folds_by_poly(
    cts_in_polys = cts_in_shps,
    pt_poly_map = pt_shp_map,
    n_folds = n_folds
  )
  ho_id <- pt_shp_map



  return(cbind(
    fold_vec,
    ho_id
  ))
}
