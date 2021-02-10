#' @title Plot Stackers
#' @description Plot maps of stackers, mean raster of covariate, and data informing model
#'
#' @param reg region
#' @param ig indicator group
#' @param ind indicator
#' @param rd run date
#' @param ss vector of stacker names
#' @param yl year list (in vector form, e.g. `c(2000:2015)`)
#' @param zmin minimum value for color scheme (if NULL will calculate from data)
#' @param zmax maximum value for color scheme (if NULL will calculate from data)
#' @param sh_dir `/share` directory, including run date
#' @param highisbad should high values be colored in red ("bad")? Logical.
#' @param o_dir output directory
#' @param individual_countries should individual countries be graphed as well? Logical.
#' @param shapefile_version string specifies shapefile version to be used
#' @return Writes a series of image fileswith maps of each stacker, mean covariate raster, and
#' a map of input data for each year-region combination (and year-country if individual_countries = T)
#' in prespecified folder structure
#' @examples
#' \dontrun{
#' mclapply(Regions, function(r) {
#' message(paste0("Making stacker maps for region: ", r))
#' plot_stackers(reg = r, highisbad = F, individual_countries = T)
#' }, mc.cores = 5)
# plot stackers & mean outcome
#' }
#' @export
plot_stackers <- function(reg,
                          ig = indicator_group,
                          ind = indicator,
                          rd = run_date,
                          ss = stackers,
                          yl = year_list,
                          zmin = NULL, zmax = NULL,
                          sh_dir = sharedir,
                          highisbad = F,
                          o_dir = out_dir,
                          individual_countries = T,
                          shapefile_version = "current") {

  # Load master shape for outlines
  # master_shape <- readRDS('/share/geospatial/rds_shapefiles/gdcv_custom/master_shape_all.rds')
  master_shape <- readRDS(get_admin_shapefile(admin_level = 0, version = shapefile_version, suffix = ".rds"))
  master_shape <- subset(master_shape, ADM0_CODE %in% get_adm0_codes(reg, shapefile_version = shapefile_version))

  # Set up output dir
  o_dir <- paste0(o_dir, "/stacker_maps/")
  dir.create(o_dir, recursive = T, showWarnings = F)

  # Load stacker objects
  load(paste0(
    "/share/geospatial/mbg/", ig, "/", ind, "/model_image_history/",
    rd, "_bin0_", reg, "_0.RData"
  ))
  # subset cov_list to stackers
  stacker_list <- cov_list[which(names(cov_list) %in% ss)]


  # load mean unraked raster for estimates. Try region rasters first; whole raster & crop if not.
  if (file.exists(paste0(sh_dir, ind, "_", reg, "_unraked_mean_raster.tif"))) {
    result_brick <- brick(paste0(sh_dir, ind, "_", reg, "_unraked_mean_raster.tif"))
  } else if (file.exists(paste0(sh_dir, ind, "_", reg, "_mean_raster.tif"))) {
    result_brick <- brick(paste0(sh_dir, ind, "_", reg, "_mean_raster.tif"))
  } else if (file.exists(paste0(sh_dir, ind, "_unraked_mean_raster.tif"))) {
    result_brick <- brick(paste0(sh_dir, ind, "_unraked_mean_raster.tif"))
    result_brick <- crop(result_brick, master_shape)
    result_brick <- mask(result_brick, master_shape)
  } else if (file.exists(paste0(sh_dir, ind, "_mean_raster.tif"))) {
    result_brick <- brick(paste0(sh_dir, ind, "_mean_raster.tif"))
    result_brick <- crop(result_brick, master_shape)
    result_brick <- mask(result_brick, master_shape)
  } else {
    stop("Could not find unraked raster .tif.")
  }

  # add mean raster to stacker list
  stacker_list[[ind]] <- result_brick

  # load input data from csv if present; re-generate if needed
  if (file.exists(paste0(sh_dir, "input_data_bin0_", reg, "_0.csv"))) {
    input_df <- read.csv(paste0(sh_dir, "input_data_bin0_", reg, "_0.csv"),
      stringsAsFactors = F
    ) %>%
      as.data.table()
  } else {
    gaul_list <- get_adm0_codes(reg,
      shapefile_version = shapefile_version
    )
    simple_polygon_list <- load_simple_polygon(
      gaul_list = gaul_list,
      buffer = 0.4,
      shapefile_version = shapefile_version
    )
    subset_shape <- simple_polygon_list[[1]]
    simple_polygon <- simple_polygon_list[[2]]
    raster_list <- build_simple_raster_pop(subset_shape)
    simple_raster <- raster_list[["simple_raster"]]
    pop_raster <- raster_list[["pop_raster"]]

    input_df <- load_input_data(
      indicator = indicator,
      simple = simple_polygon,
      removeyemen = TRUE,
      yl = yl
    )
  }

  input_df[, outcome := get(indicator) / N]

  # Define function to save maps
  save_maps <- function(input_df, stacker_list, master_shape,
                          result_brick, zmin, zmax, yl, ind, ig, sh_dir,
                          highisbad, o_dir, ctry = NULL,
                          shapefile_version) {
    if (!is.null(ctry)) {
      input_df <- subset(input_df, country == ctry)
      master_shape <- subset(master_shape, ADM0_CODE == get_adm0_codes(ctry, shapefile_version = shapefile_version))
      stacker_list <- lapply(stacker_list, function(x) {
        x <- suppressMessages(crop(x, extent(master_shape)))
        x <- suppressMessages(mask(x, master_shape))
        extent(x) <- extent(master_shape)
        return(x)
      })
    }

    master_shape_df <- suppressMessages(fortify(master_shape))

    # Cap N at 90% for plot interpretability
    input_df[, cap_N := ifelse(N >= quantile(N, 0.9), quantile(N, 0.9), N)]

    # define mins / maxes from global values across all years in stacker_list & input data
    if (is.null(zmin)) {
      zmin <- min(
        sapply(stacker_list, function(x) min(minValue(x))),
        min(input_df$outcome)
      )
    }
    if (is.null(zmax)) {
      zmax <- max(
        sapply(stacker_list, function(x) max(maxValue(x))),
        max(input_df$outcome)
      )
    }

    # Check # years correct
    if (nlayers(result_brick) != length(yl)) stop("Number of mean raster brick layers does not equal length of year list")

    # rearrange
    for (i in 1:length(yl)) {
      message(paste0("   year ", yl[i], "..."))

      make_gg_map <- function(rbrick, title, i) {
        r_df <- rbrick[[i]] %>%
          as("SpatialPixelsDataFrame") %>%
          as.data.frame()
        names(r_df) <- c("value", "x", "y")
        gg_result <- ggplot() +
          geom_raster(data = r_df, aes(x = x, y = y, fill = value)) +
          coord_equal() +
          theme_empty() +
          labs(title = title) +
          scale_fill_distiller(
            palette = "RdYlBu",
            direction = ifelse(highisbad, -1, 1),
            limits = c(zmin, zmax)
          ) +
          geom_path(
            data = master_shape_df,
            aes(x = long, y = lat, group = group)
          )
        return(gg_result)
      }

      gg_stackers_results <- lapply(1:length(stacker_list), function(n) {
        the_title <- paste0(names(stacker_list)[n], ": ", yl[i])
        the_rbrick <- stacker_list[[n]]
        return(make_gg_map(the_rbrick, the_title, i))
      })

      # Make data plot
      gg_data <- ggplot() +
        geom_point(
          data = subset(input_df, year == yl[i] & weight < 1),
          aes(x = longitude, y = latitude, size = cap_N, alpha = weight, color = outcome)
        ) +
        geom_point(
          data = subset(input_df, year == yl[i] & weight == 1),
          aes(x = longitude, y = latitude, size = cap_N, color = outcome)
        ) +
        coord_equal() +
        theme_empty() +
        scale_color_distiller(
          palette = "RdYlBu",
          direction = ifelse(highisbad, -1, 1),
          limits = c(zmin, zmax)
        ) +
        geom_path(
          data = master_shape_df,
          aes(x = long, y = lat, group = group)
        ) +
        #  scale_alpha(range = c(0,1)) +
        scale_size_continuous(limits = c(NA, max(input_df$cap_N))) +
        labs(title = paste0("data: ", yl[i]))

      gg_stackers_results[[length(gg_stackers_results) + 1]] <- gg_data

      # Use first legend only
      the_legend <- g_legend(gg_stackers_results[[1]])
      gg_stackers_results <- lapply(gg_stackers_results, function(x) return(x + theme(legend.position = "none")))

      if (is.null(ctry)) {
        reg_dir <- paste0(o_dir, reg, "/")
        dir.create(reg_dir, recursive = T, showWarnings = F)
        fn <- paste0(reg_dir, "stacker_map_", reg, "_", yl[i], ".png")
      } else if (!is.null(ctry)) {
        ctry_dir <- paste0(o_dir, ctry, "/")
        dir.create(ctry_dir, recursive = T, showWarnings = F)
        fn <- paste0(ctry_dir, "stacker_map_", ctry, "_", yl[i], ".png")
      }

      png(
        filename = fn,
        width = 16, height = 9, units = "in",
        res = 200, pointsize = 10,
        type = "cairo-png"
      )

      multiplot(
        plotlist = gg_stackers_results,
        cols = ceiling(length(gg_stackers_results) / 2),
        legend = the_legend
      )

      dev.off()
    }
  }

  # Save map for entire region
  save_maps(input_df, stacker_list, master_shape, result_brick, zmin,
    zmax, yl, ind, ig, sh_dir, highisbad, o_dir,
    ctry = NULL,
    shapefile_version = shapefile_version
  )

  # Save maps for individual countries
  if (individual_countries == T) {
    gaul_to_loc_id <- get_location_code_mapping(shapefile_version = shapefile_version)
    gaul_list <- data.table(GAUL_CODE = get_adm0_codes(reg, shapefile_version = shapefile_version))
    gaul_list <- merge(gaul_list, gaul_to_loc_id, by = "GAUL_CODE")
    for (c in unique(gaul_list$ihme_lc_id)) {
      if (!(get_adm0_codes(c, shapefile_version = shapefile_version) %in% master_shape$ADM0_CODE)) {
        message(paste0("No shapes in master_shape corresponding to admin 0 code for ", c, " - skipping..."))
      } else {
        message(paste0("Saving stacker maps for country: ", c))
        save_maps(input_df, stacker_list, master_shape, result_brick, zmin, zmax, yl, ind, ig, sh_dir, highisbad, o_dir, ctry = c, shapefile_version = shapefile_version)
      }
    }
  }
}
