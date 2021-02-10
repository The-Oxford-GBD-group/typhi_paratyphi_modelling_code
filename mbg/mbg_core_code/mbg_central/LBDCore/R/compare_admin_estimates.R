#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param nid_list PARAM_DESCRIPTION
#' @param results_raster PARAM_DESCRIPTION
#' @param compare_source_title PARAM_DESCRIPTION
#' @param raked_title PARAM_DESCRIPTION
#' @param outcome_title PARAM_DESCRIPTION
#' @param compare_data PARAM_DESCRIPTION
#' @param compare_spdf PARAM_DESCRIPTION
#' @param master_list PARAM_DESCRIPTION
#' @param shapefile_version PARAM_DESCRIPTION
#' @param by_source PARAM_DESCRIPTION, Default: FALSE
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if (interactive()) {
#'   # EXAMPLE1
#' }
#' }
#' @rdname compare_admin_estimates
#' @export
compare_admin_estimates <- function(nid_list, results_raster,
                                    compare_source_title, raked_title,
                                    outcome_title, compare_data,
                                    compare_spdf, master_list,
                                    shapefile_version,
                                    by_source = FALSE) {

  ## Define function to extract preds/pops over comparison polygons, and calculate pop-weighted mean outcome over polygons.
  compile_polygons <- function(this_nid, estimates_raster) {
    message(this_nid)
    nid_results <- compare_data[nid == this_nid, ]

    gaul <- gaul_convert(nid_results[, iso3][1], shapefile_version = shapefile_version)
    message(paste0("gaul", gaul))
    country_pops <- master_list[[paste0("list_", gaul, ".pops_", gaul)]]
    country_pops <- crop(country_pops, extent(master_list[[paste0("list_", gaul, ".simple_", gaul)]]))
    country_pops <- setExtent(country_pops, master_list[[paste0("list_", gaul, ".simple_", gaul)]])
    country_pops <- mask(country_pops, master_list[[paste0("list_", gaul, ".simple_", gaul)]])
    message("crop")
    country_estimates <- crop(estimates_raster, extent(master_list[[paste0("list_", gaul, ".simple_", gaul)]]))
    country_estimates <- setExtent(country_estimates, master_list[[paste0("list_", gaul, ".simple_", gaul)]])
    country_estimates <- mask(country_estimates, master_list[[paste0("list_", gaul, ".simple_", gaul)]])

    all_data <- merge(compare_spdf, nid_results, by = c("location_code", "shapefile"))
    all_data <- all_data[!is.na(all_data@data$outcome), ]
    all_data$geo_mean <- 0
    for (shape_x in unique(all_data$location_code)) {
      message(shape_x)
      test_poly <- all_data[all_data$location_code == shape_x, ]
      period <- test_poly$year[1] - 2000 + 1
      preds <- extract(country_estimates[[period]], test_poly)
      pops <- extract(country_pops[[period]], test_poly)
      all_data$geo_mean[all_data$location_code == shape_x] <- weighted.mean(preds[[1]], pops[[1]], na.rm = T)
    }
    this_data <- as.data.table(all_data)
    return(this_data)
  }

  ## Pull unraked estimates
  all_data <- rbindlist(lapply(unique(compare_data[year > 2000 & nid %in% nid_list, nid]), compile_polygons,
    estimates_raster = results_raster
  ))

  ## Calculate super simple bias estimate
  ## Fit simple lm
  all_data <- all_data[!is.na(outcome) & !is.na(geo_mean), ]
  bias_model <- lm(outcome ~ geo_mean, data = all_data)
  ## Intercept
  intercept <- summary(bias_model)$coefficients[1, 1]
  intercept_p <- summary(bias_model)$coefficients[1, 4]
  if (intercept_p <= 0.0005) intercept_p <- 0.0005
  ## Slope
  slope <- summary(bias_model)$coefficients[2, 1]
  slope_p <- summary(bias_model)$coefficients[2, 4]
  if (slope_p <= 0.0005) slope_p <- 0.0005
  summary(bias_model)
  ## Predict fitted values for line
  all_data$lm_pred <- predict(bias_model)

  ## Make title for plot depending on if comparing a single source (grab year/iso3) or many sources
  if (length(unique(all_data[, nid])) == 1) add_title <- paste0("\n", all_data[, iso3][1], " ", all_data[, year][1], " ", compare_source_title, "\n")
  if (length(unique(all_data[, nid])) != 1) add_title <- "\n"
  main_gg_title <- paste0(compare_source_title, " vs. aggregated ", raked_title, " MBG estimates", add_title, "Intercept: ", round(intercept, 2), " (p <= ", round(intercept_p, 4), ")\nSlope: ", round(slope, 2), " (p <= ", round(slope_p, 4), ")")

  ## Plot gg for unraked
  comparison_gg <- ggplot() +
    geom_line(
      data = all_data,
      aes(
        x = lm_pred,
        y = geo_mean
      )
    ) +
    geom_point(
      data = all_data,
      aes(
        x = outcome,
        y = geo_mean
      )
    ) +
    geom_abline(slope = 1, intercept = 0, colour = "red") +
    xlab(paste0(outcome_title, ", ", compare_source_title)) +
    ylab(paste0(outcome_title, ", MBG estimates")) +
    ggtitle(main_gg_title) +
    theme_minimal()

  return(comparison_gg)
}
