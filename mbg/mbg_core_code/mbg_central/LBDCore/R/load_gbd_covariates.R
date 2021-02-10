#' @title Load GBD Covariates
#' @description A faster version of load_gbd_covariates. I've left the old one for backwards capability (dccasey 8/23/2017)
#' @param covs A character vector listing GBD covariates/outputs to extract. For covariates, this
#' should be indicated by covariate_name_short, while for outputs, this should be indicated by
#' acause. Usually fulfilled by gbd_fixed_effects.
#' @param measures A character vector coresponding to 'covs' listing the type of GBD quantity for
#' each item. Options are 'covariate' and 'outcome'. Usually fulfilled by gbd_fixed_effects_measures.
#' @param year_ids A numeric vector of year_ids. Usually fulfilled by year_list.
#' @param age_ids A string of age_ids. Usually fulfilled by gbd_fixed_effects_age.
#' @param template A raster layer of the buffered modelling area. usually it will be cov_layers[[1]][[1]].
#' If NULL, a default template is loaded using load_and_crop_covariates_annual()
#' @param use_subnationals Logical. If true, the function will replace admin0 with a subnational units
#' where possible. Use with caution because it's not been tested outside of Africa. It might not
#' work for countries with multiple levels of subnational units (e.g. UK or India).
#' @param simple_polygon simple_polygon object used for the modeling region. made in load_simple_polygon
#' @param interval_mo number of months in a time unit. usually 12 to correspond 'annual' year_loadxs
#' @return A list of covariates
#' @export
load_gbd_covariates <- function(covs, measures, year_ids, age_ids,
                                template, use_subnationals = F,
                                simple_polygon, interval_mo) {

  # check to see if the template is class raster, if not it is most
  # likely NULL since we pass in cov_layers[[1]][[1]] by default and
  # that is only created if we loaded in geospatial covs. otherwise,
  # we load in a geospatial cov to use as template
  if (class(template) != "RasterLayer") {
    message("Loading in raster template for GBD covs since template argument was not a RasterLayer")
    template <- load_and_crop_covariates_annual(
      covs = "evi",
      measures = "median",
      simple_polygon = simple_polygon,
      start_year = min(year_ids),
      end_year = max(year_ids),
      interval_mo = as.numeric(interval_mo)
    )[[1]][[1]]
  }

  # Load the analysis shapefile
  ## since this is fixed and is not related to shapefile_version, we
  ## also fix the shapefile_version passed to load_gbd_data inside
  ## fetch_gbd_covs to be one that worked for this shapeset
  world_shape <- readRDS("/share/geospatial/rds_shapefiles/GBD2016_analysis_final.rds")
  shapefile_version <- "2018_08_28" ## to match the entries of world_shape

  # If we are not using subnationals, keep only national units; otherwise remove the parent units
  if (!use_subnationals) {
    world_shape <- world_shape[world_shape$level == 3, ]
  } else {
    world_shape <- world_shape[!world_shape$loc_id %in% unique(world_shape$parent_id), ]
  }

  world_shape <- crop(world_shape, template)
  # we must skip using the link_table as it is not relevant to this shapefile
  afras <- rasterize_check_coverage(world_shape, template, "GAUL_CODE", fun = "last", link_table = NULL)
  afras <- crop_set_mask(afras, template)

  # Check to make sure all of the relevant gauls are in the file
  if (!all(world_shape$GAUL_CODE %in% unique(afras))) {
    afras <- rasterize_check_coverage(world_shape[!world_shape$GAUL_CODE %in% unique(afras), ], afras, "GAUL_CODE", fun = "first", update = T)
    afras <- crop_set_mask(afras, template)
  }


  # Loop over requested covariates
  fetch_gbd_cov <- function(name, measure, afras) {

    # Load country-level results
    message("  Loading: ", name)
    gbd <- load_gbd_data(
      gbd_type = measure,
      gbd_name = name,
      gaul_list = unique(afras),
      measure_id = 5,
      age_group_id = age_ids,
      metric_id = 3,
      year_ids = year_ids,
      return_by_age_sex = "no",
      shapefile_version = shapefile_version, ## should be most up-to-date modified GAUL
      ## to match GBD2016_analysis_final.rds
      ## world shapefile
      collapse_age_sex = TRUE
    )


    if (nrow(gbd) != nrow(unique(gbd[, list(name, year)]))) stop(paste0(name, "is not unique by location-year"))

    # For each gaul code and year, update the values
    blank <- brick(lapply(year_list, function(x) afras * NA))

    for (yyy in 1:length(year_ids)) {
      for (ggg in unique(afras)) {
        blank[[yyy]][which(raster::getValues(afras) == ggg)] <- gbd[name == ggg & year == year_list[yyy], mean]
      }
    }

    names(blank) <- rep(name, times = dim(blank)[3])

    return(blank)
  }

  all_gbd_layers <- lapply(1:length(covs), function(x) fetch_gbd_cov(covs[x], measures[x], afras))
  names(all_gbd_layers) <- covs

  return(all_gbd_layers)
}
