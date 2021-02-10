## SECTION 6 - SHINY PREP FUNCTIONS
#' Save data for shiny
#'
#' @description Save the requisite objects for the data coverage shiny
#'
#' @param df data.table output from `dcp_merge_with_gbd_locations()`
#' @param df_graph_poly data.table output of `dcp_make_df_graph()`, subsetted to countries
#' in region
#' @param poly_shapes_all a combined SPDF with all needed polygons,
#' output by `pull_polys_in_parallel()`
#' @param var Name of the field in `df` that will be plotted on the maps.
#' @param indicator The indicator being estimated for this data coverage plot.
#' This argument controls where the data coverage plots will be saved
#'
#' @export
prep_data_coverage_shiny <- function(df,
                                     df_graph_poly,
                                     poly_shapes_all,
                                     var,
                                     indicator) {

  ## Save polygons with data for Data Coverage Shiny
  makeID_save <- function(shape) {
    shapefile <- all_polygon_data[[shape]]
    if (length(shapefile) != 0) {
      shapefile <- spChFIDs(shapefile, paste0(shape, "_", row.names(shapefile@data)))
      return(shapefile)
    }
    else {
      message(paste0("Not saving for geo_data Shiny: ", unique(df_graph_poly[, survey])[shape]))
    }
  }

  # Check if any polygons
  if (nrow(df_graph_poly) > 0) {

    ## Merge polygons for this period to master spdf and add each one to map
    merge_poly_data <- function(this_survey) {
      survey_subset <- df_graph_poly[survey == this_survey, ]
      survey_shapes <- try(merge(poly_shapes_all,
        survey_subset,
        by = c("location_code", "shapefile"),
        duplicateGeoms = TRUE
      ))
      if (class(survey_shapes) == "try-error") {
        message(paste(this_survey, "is not merging properly. Please ensure unique location codes in geography codebook."))
        return(NULL)
      }
      survey_shapes <- survey_shapes[!is.na(survey_shapes@data$outcome), ]
      return(survey_shapes)
    }

    all_polygon_data <- lapply(unique(df_graph_poly[, survey]), merge_poly_data)
    all_polygon_data <- lapply(1:length(all_polygon_data), makeID_save)
    all_polygon_data <- all_polygon_data[!sapply(all_polygon_data, is.null)]
    all_polygon_data <- do.call(rbind, all_polygon_data)
    all_polygon_data$polygon_survey <- paste0(all_polygon_data$source, "_", all_polygon_data$shapefile, "_", all_polygon_data$year)
    just_polygons <- all_polygon_data[c("GAUL_CODE", "shapefile")]
    just_polygons$gaul_shape <- paste0(just_polygons$GAUL_CODE, just_polygons$shapefile)
    just_polygons <- just_polygons[which(!duplicated(just_polygons$gaul_shape)), ]
  } else {
    just_polygons <- list(NULL)
  }

  save(list = "just_polygons", file = paste0("/snfs1/WORK/11_geospatial/10_mbg/data_coverage_shiny/", indicator, "_polygons.RData"))
  setnames(df, var, indicator)
  write.csv(df, paste0("/snfs1/WORK/11_geospatial/10_mbg/data_coverage_shiny/", indicator, "_points.csv"))
}
