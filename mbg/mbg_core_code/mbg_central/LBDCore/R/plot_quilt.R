#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param gaul_list PARAM_DESCRIPTION
#' @param df PARAM_DESCRIPTION
#' @param sample PARAM_DESCRIPTION
#' @param subset_shape PARAM_DESCRIPTION
#' @param shapefile_version PARAM_DESCRIPTION, Default: 'current'
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if (interactive()) {
#'   # EXAMPLE1
#' }
#' }
#' @seealso
#'  \code{\link[fields]{quilt.plot}}
#' @rdname plot_quilt
#' @export
#' @importFrom fields quilt.plot
plot_quilt <- function(gaul_list, df, sample, subset_shape, shapefile_version = "current") { ## sample == "IS" or "OOS"

  df <- df[!is.na(get(sample)), ]
  subset_shape <- subset_shape[subset_shape$GAUL_CODE %in% gaul_list, ]
  df <- as.data.table(df)
  loc_names <- get_location_code_mapping(shapefile_version = shapefile_version)
  setnames(df, "country", "ihme_lc_id")
  df <- merge(df, loc_names, by = "ihme_lc_id")
  df <- df[GAUL_CODE %in% gaul_list, ]
  if (length(df[, GAUL_CODE]) != 0) {
    make_quilt_matrix <- function(this_year) {

      ## Run quilt function from fields library, suppress inline plot
      df_year <- df[year == this_year, ]

      if (nrow(unique(df_year[, c("longitude", "latitude")])) > 1) {
        # checks to see if there are at least 2 points, otherwise quilt.plot fails
        quilt_x <- df_year[, longitude]
        quilt_y <- df_year[, latitude]
        quilt_z <- df_year[, get(sample)]
        pdf("NULL")
        quilt <- fields::quilt.plot(quilt_x, quilt_y, quilt_z, main = "Absolute error")
        dev.off()

        ## Pull spatial info from quilt object
        long_matrix <- melt(quilt$z) # Make matrix of values from quilt.plot long df
        z_raster <- rasterFromXYZ(long_matrix) # Convert first two columns as lon-lat and third as value
        proj4string(z_raster) <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0")
        z_raster <- setExtent(z_raster, extent(min(quilt$x), max(quilt$x), min(quilt$y), max(quilt$y)))

        # Convert raster to SpatialPointsDataFrame
        z_raster.sp <- rasterToPoints(z_raster, spatial = TRUE)
        projection <- proj4string(z_raster.sp)

        # reproject sp object
        z_raster.sp <- spTransform(z_raster.sp, CRS(projection))
        z_raster.sp@data <- data.frame(z_raster.sp@data, long = coordinates(z_raster.sp)[, 1], lat = coordinates(z_raster.sp)[, 2])
        z_raster.dt <- data.table(z_raster.sp@data)
        names(z_raster.dt)[names(z_raster.dt) == "lat"] <- "latitude"
        names(z_raster.dt)[names(z_raster.dt) == "long"] <- "longitude"
        z_raster.dt <- z_raster.dt[, year := this_year]
      } else {
        z_raster.dt <- data.table(
          value = numeric(),
          longitude = numeric(),
          latitude = numeric(),
          year = numeric()
        )
      }
      return(z_raster.dt)
    }
    quilt_list <- rbindlist(lapply(sort(unique(df[, year])), make_quilt_matrix))
    color_list <- c("#fff7ec", "#fee8c8", "#fdd49e", "#fdbb84", "#fc8d59", "#ef6548", "#d7301f", "#b30000", "#7f0000")
    this_shape.dt <- data.table(fortify(subset_shape))
    redblue <- c("#313695", "#ffffff", "#a50026")
    # plot gg
    quilt_gg <- ggplot(quilt_list, aes(longitude, latitude)) +
      geom_raster(aes(fill = value)) +
      coord_fixed() +
      theme_minimal() +
      geom_path(data = this_shape.dt, aes(x = long, y = lat, group = group), color = "black", lwd = .1) +
      # scale_fill_gradientn(colours=(color_list), limits=c(min(df[, get(sample)]), max(df[, get(sample)])), na.value = "#000000") +
      scale_fill_gradientn(colours = redblue, values = c(min(df[, get(sample)]), 0, max(df[, get(sample)])), limits = c(min(df[, get(sample)]), max(df[, get(sample)])), na.value = "#000000", rescaler = function(x, ...) x, oob = identity) +
      guides(fill = guide_colorbar(title = "Absolute\nerror", label = TRUE, ticks = FALSE)) +
      scale_x_continuous("", breaks = NULL) +
      scale_y_continuous("", breaks = NULL) +
      theme(panel.margin = unit(0, "lines"), plot.margin = unit(c(0, 0, 0, 0), "lines")) +
      facet_wrap(~year)
    return(quilt_gg)
  }
  if (length(df[, GAUL_CODE]) == 0) {
    return(NULL)
  }
}
