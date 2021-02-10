##
## GRAPH DATA COVERAGE
## Authors: Jon Mosser (jmosser), Nat Henry (nathenry), Michael Collison (mlc314)
## Purpose: Functions to graph data coverage and completeness for the LBD team
##
## SECTIONS
##   1 - Main function (graph_data_coverage_values)
##   2 - Data prep and validation functions
##   3 - Shapefile prep and validation functions
##   4 - Graphics functions: scatter plots
##   5 - Graphics functions: data coverage maps
##   6 - Graphics functions: Shiny prep
##
## SECTION 1 - MAIN FUNCTION
## graph_data_coverage_values() ----------------------------------------------
#'
#' @title Graph Data Coverage Values
#'
#' @description Produces standard diagnostic maps used to assess input data
#' coverage across Local Burden of Disease teams.
#'
#' @details This function should be run regularly by data analysts and
#' researchers to diagnose data availability across their modeling regions.
#' For additional information about required input fields and data types,
#' see the functions `dcp_validate_input_args()` and
#' `dcp_validate_input_data()`.
#'
#' ** ## PARAMETERS ## **
#' ** Basic arguments **
#' @param df The input data.frame or data.table used to create the plot.
#' Must contain at least the following columns, which will be coerced to the
#' following data types:
#' * 'nid' (integer):  This field was called 'svy_id' in the original code
#' * 'country' (char): ISO3 code associated with the survey country
#' * 'source' (char):  Name of the data source category (eg, "MACRO_DHS")
#' * 'year' (integer): Year, either when the data was collected or when an
#' event occurred.
#' * 'N' (numeric):    The sample size at the given location.
#' * <VAR> (numeric):  The value of the outcome of interest at the survey site.
#' Often expressed as a rate (eg. num events / sample size).
#' * 'cluster_id' (character): Index assigning unique unique observations from
#' the input data. Typically used for debugging.
#' * 'latitude' (numeric):  Latitude associated with the observation, if available
#' * 'longitude' (numeric): Longitude associated with the observation, if available
#' * 'shapefile' (char):    Shapefile associated with the observation. NOTE:
#' each observation must have either a valid
#' latitude and longitude or a shapefile and location
#' code, or else it will be dropped
#' * 'location_code' (integer): Location code in the given polygon that is
#' associated with the observation. In the survey
#' shapefile library and the codebooks, these are
#' currently called the "GAUL_CODE"
#' Rows containing NA or empty values in the first seven fields will be
#' dropped. All rows must have either 'latitude' and 'longitude' fields filled,
#' or they will be dropped. For more information about how input data is
#' validated, see the function `dcp_validate_input_data()`.
#' @param var Name of the field in `df` that will be plotted on the maps.
#' @param title Title stub for the map. Full title will take the form
#' "<title>: <region name>".
#' @param legend_title Title of the legend (color scale) on the map.
#' @param year_var Name of the field in `df` used to indicate which year an
#' observation falls within. Years not between `year_min` and `year_max` will
#' be dropped.
#' @param year_min The earliest year of data to include in plots. The standard
#' for LBD is currently 1998.
#' @param year_max The latest year of data to include in plots. The standard
#' for LBD is currently 2017.
#' @param region Name of region that has been specified in ref_reg_list in
#' prep_functions. Region must be associated with "standard" (non-custom)
#' modeling regions that can be directly interpreted by get_adm0_codes().
#' Dictates which area of the world will be shown on the plots.
#' @param region_title Custom region title that will be used to title plots and
#' create file names for saving. Defaults to NULL which uses the name of the
#' specified custom region.
#'
#' ** Arguments controlling code execution and saving **
#' @param cores The number of CPUs available to the function. On prod nodes,
#' this conservatively works out to about (# slots) / 2. On geos nodes, this
#' is approximately the same as the number of slots requested in your
#' qlogin. For more info, see Ian Davis' great writeup on the subject at
#' (https://hub.ihme.washington.edu/display/~imdavis/Cores\%2C+memory\%2C+and+slots)
#' @param indicator The indicator being estimated for this data coverage plot.
#' This argument controls where the data coverage plots will be saved: for
#' example, if `save_on_share` is `FALSE`, then this will save the output maps
#' to `"/home/j/WORK/11_geospatial/10_mbg/data_coverage_plots/<indicator>/"`.
#' @param extra_file_tag (default `''`) If set to something other than an empty
#' string, adds additional text to the filepath that would end in
#' `.../<indicator>/` by default.
#' @param save_on_share (default `FALSE`) If set to `FALSE`, the base save
#' directory will be `"/home/j/WORK/11_geospatial/10_mbg/data_coverage_plots/"`.
#' If set to `TRUE`, the base directory will be
#' `"/share/geospatial/mbg/data_coverage_plots/"`.
#' @param out_dir (default `NULL`) Set a custom save directory. Overrides all
#' other parameters controlling filepaths above.
#' @param core_repo (default `"/share/code/geospatial/lbd_core"`) The directory
#' used to read in other MBG functions.
#' @param log_dir (default `NULL`) Path to a directory where a log file will
#' optionally be saved. Useful mainly for debugging.
#'
#' ** Arguments related to speedups and custom outputs **
#' @param fast_shapefiles (default `TRUE`) Load geometries in RDS format rather
#' than interacting with shapefiles directly. This function should almost
#' always be set to TRUE, and the RDS files prepared beforehand using the
#' `synchronize_shapefile_directories()` function in
#' `"mbg_central/shapefile_functions.R"`.
#' @param new_data_plots (default `FALSE`) Toggles whether to create New Data
#' plots. Leaving this as the default, `FALSE`, speeds up function execution.
#' @param since_date (default `NULL`) If New Data plots are created, specifies
#' the date beyond which all data is considered "new". Must be either `NULL`
#' (in which case the default is the last date when this function was run)
#' or a character of form `"YYYY-MM-DD"` (including leading zeroes for months
#' and days).
#' @param annual_period_maps (default `FALSE`) Toggles whether to period maps
#' for each year between the `year_min` and `year_max` rather than the
#' standard five-year binned maps. If set to `TRUE`, ONLY annual maps will
#' be created, and the standard four-period coverage plots will not be
#' produced. Useful for team-specific data validation.
#' @param save_period_maps (default `TRUE`) Toggles whether or not to save
#' maps for individual time periods in addition to the standard four-period
#' data coverage plot. If `annual_period_maps` is `TRUE`, this argument must
#' also be `TRUE`.
#' @param prep_shiny (default `FALSE`) Toggles whether or not to save data
#' objects for the interactive Shiny visualization tool.
#' @param return_maps (default `TRUE`) Toggles whether or not to return ggplot
#' objects for all period-specific maps.
#' @param debug (default `FALSE`) If set to `TRUE`, the function will fail if
#' any rows of data are dropped due to mismatched types or missing data.
#'
#' ** Arguments controlling map and scatter symbology **
#' @param color_scheme (default `"classic"`)
#' @param color_scheme_scatter (default `"brewer"`)
#' @param high_is_bad (default `TRUE`)
#' @param cap (default `90`)
#' @param cap_type (default `"percentile"`) The type of numeric cap used to set
#' the legend maximum. Must be one of `'percentile'`, `'absolute'`,
#' or `'none'`.
#' @param legend_min (default `NA`) Absolute lower bound for the map legend.
#' Defaults to the lowest observation in the dataset.
#' @param legend_max (default `NA`) Absolute upper bound for the map legend. If
#' something other than `NA`, overrides the `cap` and `cap_type` arguments.
#' @param endemic_gauls (default `NULL`) This argument can be set as a character
#' vector containing ISO3 codes. If not `NULL`, all countries not in this
#' list will be greyed out on the final plot. This argument overrides the
#' `stage_3_gray` argument.
#' @param stage_3_gray (default `TRUE`) If `TRUE`, greys out all stage 3
#' countries on the map. Data from these countries will still be displayed.
#' @param simplify_polys (default `TRUE`) If `TRUE`, simplifies input polygons
#' to speed up map saving. While this step requires an extra initial processing
#' step, it typically reduces execution time overall.
#' @param tolerance (default `0.03`) If `simplify_polys` is `TRUE`, this argument
#' controls the degree to which polygons will be simplified.
#' @param base_font_size (default `18`) The default text size used in the plots.
#' @param map_point_size (default `0.8`) The default size of point data on the
#' map.
#' @param poly_line_width (default `0.2`) The default width of white lines
#' surrounding polygons on the map. If you are visualizing very high-resolution
#' polygon data, it might be a good idea to set this argument to `0`.
#' @param remove_rank (default `TRUE`) Testing this parameter - keep as default.
#'
#' @return If `return_maps` is `TRUE`, returns ggplot objects for maps made
#' for each time period. Otherwise, returns `NULL`.
#'
#' @family Data Coverage Plot functions
#'
#' @export
graph_data_coverage_values <- function(df,
                                       var,
                                       title,
                                       legend_title,
                                       year_var,
                                       year_min,
                                       year_max,
                                       region,
                                       region_title = NULL,

                                       cores,
                                       indicator,
                                       extra_file_tag = "",
                                       save_on_share = FALSE,
                                       out_dir = NULL,
                                       core_repo = "/share/code/geospatial/lbd_core/",
                                       log_dir = NULL,

                                       fast_shapefiles = TRUE,
                                       new_data_plots = FALSE,
                                       since_date = NULL,
                                       annual_period_maps = FALSE,
                                       save_period_maps = TRUE,
                                       prep_shiny = FALSE,
                                       return_maps = TRUE,
                                       debug = FALSE,

                                       color_scheme = "classic",
                                       color_scheme_scatter = "brewer",
                                       high_is_bad = TRUE,
                                       cap = 90,
                                       cap_type = "percentile",
                                       legend_min = NA,
                                       legend_max = NA,
                                       endemic_gauls = NULL,
                                       stage_3_gray = TRUE,
                                       simplify_polys = TRUE,
                                       tolerance = 0.03,
                                       base_font_size = 18,
                                       map_point_size = 0.8,
                                       poly_line_width = 0.2,

                                       remove_rank = TRUE) {

  message(paste0("** Generating ", indicator, " graphs for ", region, " **\n"))

  ## Load packages
  message("Loading packages...")
  # Source package imports function
  source(paste0(core_repo, "/mbg_central/setup.R"))
  # Load all external packages
  package_list <- c(
    "data.table", "ggplot2", "parallel", "doParallel", "grid",
    "gridExtra", "stringr", "RColorBrewer", "rgdal", "sp",
    "raster", "magrittr", "dplyr", "RMySQL", "rgeos", "tidyr"
  )
  load_R_packages(package_list)
  # Load other MBG-specific functions
  source(paste0(core_repo, "/mbg_central/gbd_functions.R"))
  source(paste0(core_repo, "/mbg_central/prep_functions.R"))
  source(paste0(core_repo, "/mbg_central/shapefile_functions.R"))

  ## Manually keep order_var space_rank for now
  order_var <- "space_rank"

  ## Define root
  assign("j_root", "/home/j/", envir = .GlobalEnv)

  ## Set up ggplot base font size
  theme_set(theme_minimal(base_size = base_font_size))

  # pull in list of stage 3 countries to grey out and remove from scatterplots
  stage3 <- load_adm0_lookup_table()
  stage3 <- stage3[Stage == "3", ]
  stage3[, iso3 := toupper(iso3)]


  ## 1. Validate input arguments and dataframe -------------------------------
  in_args_all <- as.list(mget(
    names(formals()),
    sys.frame(sys.nframe())
  ))
  dcp_validate_input_args(in_args_all)

  df_prepped <- dcp_validate_input_data(df,
    var = var,
    year_var = year_var,
    debug = debug
  )

  ## 2. Pull in country table and merge onto input data ----------------------
  message("Pulling GBD region list...")
  # Pull in list of regions from GBD
  country_table <- suppressMessages(suppressWarnings(
    data.table(get_location_hierarchy(41))[, .(
      ihme_loc_id,
      level,
      location_name,
      region_name
    )]
  ))
  # For now, subset to countries only
  #   Note: could change this for subnationals if desired!
  #   May be helpful if looking at a country for which we have tons of data (e.g. India)
  country_table <- country_table[level == 3]
  country_table[, level := NULL]
  setnames(country_table, "ihme_loc_id", "country")


  ## 2. Load data & split off relevant data sets -----------------------------

  # 2. Load data & split off relevant data sets ------------------------------
  message("Preparing data sets...")
  # Create df_poly, df_point, and df_summary
  with_gbd_locs <- dcp_merge_with_gbd_locations(df_prepped, country_table, region)
  df <- with_gbd_locs[["df"]] # Full data set
  df_poly <- with_gbd_locs[["df_poly"]] # Polygon data only
  df_point <- with_gbd_locs[["df_point"]] # Point data only
  df_summary <- with_gbd_locs[["df_summary"]] # One-row-per-NID summary
  rm(with_gbd_locs)

  # If new_data_plots is TRUE, check for new data
  # Save most recent data summary to a CSV
  message("Comparing against data from previous coverage plots...")
  df_summary <- dcp_find_new_data(df_summary, indicator, since_date)

  # Grab a country list
  list_output <- get_country_list(df, region, region_title)
  reg_title <- list_output[["reg_title"]] # Formatted region title
  region_list <- list_output[["region_list"]] # Subheadings for regions
  country_list <- list_output[["country_list"]] # ISO3 codes for countries
  rm(list_output)

  # Make a summary data set to use for graphing, subset to region
  df_graph <- dcp_make_df_graph(df_summary, country_list)

  # Similarly subset df_point and df_poly to just the region of interest
  df_graph_point <- df_point[country %in% country_list]
  df_graph_poly <- df_poly[country %in% country_list]



  # 1. Load polygons for the right (map) side of the figure ------------------

  # The polygon map works by pulling shapefiles for each polygon in the
  #   data set, and then coloring those by the lastest year. Polygons are
  #   overlaid chronologically, so the most recent of two overlapping
  #   polygons will be visible.

  # Set up point / polygon graphing data sets
  #   will create plots for polygons (latest year of data = color)
  #   and points (color = year of data) separately

  # IF annual plots are being made, each individual year will be plotted
  if (annual_period_maps) {
    df_graph_poly[, plot_year := year]
    df_graph_point[, plot_year := year]
  } else {
    # Otherwise, bin years to the four standard age bins
    df_graph_poly <- dcp_bin_years(df_graph_poly)
    df_graph_point <- dcp_bin_years(df_graph_point)
  }

  # Only do this bit if there are polygons!
  if (nrow(df_graph_poly) > 0) {
    message("Loading individual shapefiles for mapping...")

    # Check for missing shapefiles & report if present
    shapefile_list <- dcp_check_for_missing_shapefiles(
      df_graph_poly,
      fast_shapefiles = fast_shapefiles
    )
    shapefiles <- shapefile_list[["shapefiles"]]
    not_real_shapefiles <- shapefile_list[["not_real_shapefiles"]]

    if (length(not_real_shapefiles) > 0) {
      warning(paste0(
        "  DROPPING ", length(not_real_shapefiles),
        " MISSING SHAPES (in shapefile column but not shapefile dir)!"
      ))
      if (!is.null(log_dir)) {
        warning(paste0(
          "  Writing list of missing shapefiles to ", log_dir,
          "missing_shapes.csv"
        ))
        not_real_shapefiles %>%
          as.data.table() %>%
          setnames(., ".", "Missing shapefiles") %>%
          write.csv(., file = paste0(log_dir, "missing_shapes.csv"))
      } else {
        warning(paste0("  Missing shapefiles: ", paste(not_real_shapefiles,
          collapse = ", "
        )))
      }
    }

    rm(shapefile_list) # clean up

    # Only pull shapefiles if there are any 'real' shapefiles left
    if (length(shapefiles) > 0) {
      ## Pull shapes in parallel ---------------------------------------------

      # Make a table of shapefiles & associated location codes
      df_shape_loc <- unique(df_graph_poly[, c("shapefile", "location_code")])
      # Pull all polygons in parallel
      poly_list <- pull_polys_in_parallel(
        shape_loc_list = df_shape_loc,
        shapefile_col = "shapefile",
        location_code_col = "location_code",
        cores = cores,
        fast_shapefiles = fast_shapefiles
      )
      message("Done pulling polys.")

      # Find if any broken shapes and, if so, report
      poly_shapes_all <- poly_list[["poly_shapes_all"]]
      broken_shapes <- poly_list[["broken_shapes"]]

      if (length(broken_shapes) > 0) {
        message("Warning: the following shapes encountered errors :")
        print(broken_shapes)
      }

      if (simplify_polys == T) {
        poly_shapes_all <- simplify_spdf(poly_shapes_all, tol = tolerance)
      }
      rm(poly_list)
    }
  } else {
    message("There is no polygon data, so individual shapefiles were not pulled.")

    # If no polygons, set products of the above to NULL
    poly_shapes_all <- NULL
    not_real_shapefiles <- NULL
    broken_shapes <- NULL
  }

  # At this point, all errors should be in the code, not from user inputs
  message(paste0(
    "Note: At this point, all data and shapefiles have been loaded ",
    "successfully.\n  If the function breaks after this point, there ",
    "is likely an issue with the code.\n  For assistance, please ",
    "message Nat Henry or Michael Collison."
  ))

  # 2. Load master and disputed shapefiles ------------------------------------>

  # This takes a while, so let's do it once only
  if (!("master_shape_all" %in% ls())) {
    message("\nFast-loading master shapefile... ")
    assign(
      "master_shape_all",
      readRDS("/share/geospatial/rds_shapefiles/gdcv_background_shp.rds"),
      envir = globalenv()
    )
  } else {
    message("'master_shape_all' is already in the environment.")
  }
  # Load the disputed shapefile
  disputed_shp <- readRDS("/share/geospatial/rds_shapefiles/gdcv_disputed_shp.rds")
  # Rename ADM0_CODE fields to GAUL_CODE
  # (Note: These are really GADM codes, and the names should be changed in the
  #   future)
  setnames(master_shape_all@data, "ADM0_CODE", "GAUL_CODE")
  setnames(disputed_shp@data, "ADM0_CODE", "GAUL_CODE")



  # 0. Make sure that all other devices outside of the null device are turned
  #  off, then sink all random graphical output to NULL; function specifies png
  #  where graphs actually desired
  pdf(NULL) # Failsafe to ensure that dev.off() does not throw an error
  graphics_level <- 2
  while (graphics_level > 1) {
    # Keep turning off graphics devices until the null device (1) is reached
    graphics_level <- dev.off()
  }
  pdf(NULL)

  # 1. Make table and scatter plots for the left side of the graph -------------
  # Only execute this step if annual maps are not being made, as the annual
  #   map option does not produce the 4-period map!
  if (!annual_period_maps) {
    # 1a. Make table for the left side of graph---------------------------------
    table_data <- dcp_make_table_new(df_summary, country_list, year_min, year_max)
    polys_total <- sum(table_data$Polygons)
    points_total <- sum(table_data$Points)
    n_total <- sum(table_data$N)
    # 1b. Make plot for left side of graph -------------------------------------
    message("Making scatter plot...")
    data_scatter_list <- make_data_scatterplots(
      df_graph = df_graph,
      df_summary = df_summary,
      title = title,
      reg_title = reg_title,
      year_min = year_min,
      year_max = year_max,
      table_data = table_data,
      base_font_size = base_font_size,
      region_name = region,
      color_scheme_scatter = color_scheme_scatter,
      stage3 = stage3,
      stage_3_gray = stage_3_gray,
      new_data_plots = new_data_plots
    )
    g_data <- data_scatter_list[["g_data"]] # scatter for all data
    g_data_new <- data_scatter_list[["g_data_new"]] # scatter for just new data
    g_data_legend <- data_scatter_list[["g_data_legend"]] # legend for g_data
    g_data_new_legend <- data_scatter_list[["g_data_new_legend"]] # legend for g_data_new
    rm(data_scatter_list)
  }

  # 2a: Define map basics --------------------------------------------------

  # Create a background map for the plot (just the country polygons)
  message("Constructing background map...")
  background_map_list <- make_background_map(
    region, endemic_gauls, simplify_polys,
    tolerance, fast_shapefiles,
    master_shape_all, disputed_shp,
    stage3, stage_3_gray
  )
  background_outline <- background_map_list[[1]]
  background_map <- background_map_list[[2]]
  background_map_not_endemic <- background_map_list[[3]]
  background_extent <- background_map_list[[4]]
  disputed_map <- background_map_list[[5]]
  disputed_bg <- background_map_list[[6]]
  rm(background_map_list)
  message("  Finished constructing background map.")

  color_list <- get_color_list(color_scheme)
  if (high_is_bad == TRUE) color_list <- rev(color_list)

  # 2b: Make some graphs ---------------------------------------------------
  if (annual_period_maps) {
    # IF annual plots are being made, each year will have its own period map
    map_these_periods <- year_min:year_max
  } else {
    # Otherwise, plot the standard 4 maps
    map_these_periods <- c(2000, 2005, 2010, 2015)
  }

  # Create a list to store period objects
  period_map_storage <- vector("list", length = length(map_these_periods))

  for (period in map_these_periods) {
    message(paste0("Making period map for ", period, "..."))
    g_map <- make_a_period_map(
      period, df, region, poly_shapes_all, background_map, background_outline,
      background_map_not_endemic, background_extent, disputed_map, disputed_bg,
      df_graph_poly, df_graph_point, not_real_shapefiles, color_list,
      legend_title, log_dir, base_font_size, map_point_size, cap, cap_type,
      legend_min, legend_max, poly_line_width, annual_period_maps
    )
    period_map_storage[[as.character(period)]] <- g_map
  }



  if (is.null(out_dir)) {
    out_dir <- paste0(
      "/snfs1/WORK/11_geospatial/10_mbg/data_coverage_plots/",
      indicator, extra_file_tag, "/"
    )
  }
  if (save_on_share == TRUE) {
    out_dir <- paste0(
      "/share/geospatial/mbg/data_coverage_plots/",
      indicator, extra_file_tag, "/"
    )
  }
  dir.create(out_dir, showWarnings = FALSE, recursive = T)

  # Make the main plots - 4-up
  # SKIP THIS STEP IF ANNUAL PERIOD MAPS ARE BEING MADE, as the binned period
  #  maps that go into the main plot have not been created.
  if (!annual_period_maps) {
    if (is.null(region_title)) {
      out_file <- paste0(out_dir, "data_coverage_", region, ".png")
    } else {
      out_file <- paste0(out_dir, "data_coverage_", region_title, ".png")
    }
    message(paste0("Saving ", out_file))
    unlink(out_file) # Delete any old versions

    png(
      filename = out_file,
      units = "in",
      width = 24.33,
      height = 12,
      pointsize = base_font_size,
      res = 450
    )
    dcp_make_4up_map(
      g_datamap = g_data,
      g_data_legend = g_data_legend,
      map_list = list(
        period_map_storage[["2000"]],
        period_map_storage[["2005"]],
        period_map_storage[["2010"]],
        period_map_storage[["2015"]]
      ),
      n_countries = length(unique(df_graph$country)),
      reg_title = reg_title,
      title = title,
      base_font_size = base_font_size,
      n_total = n_total,
      polys_total = polys_total,
      points_total = points_total
    )
    dev.off()

    if (new_data_plots) {
      # Repeat the main plot, but for new data
      if (is.null(region_title)) {
        out_file <- paste0(out_dir, "data_coverage_", region, "_new.png")
      } else {
        out_file <- paste0(out_dir, "data_coverage_", region_title, "_new.png")
      }
      message(paste0("Saving ", out_file))
      unlink(out_file) # Delete any old versions

      png(
        filename = out_file,
        units = "in",
        width = 24.33,
        height = 12,
        pointsize = base_font_size,
        res = 450
      )
      dcp_make_4up_map(
        g_datamap = g_data_new,
        g_data_legend = g_data_new_legend,
        map_list = list(
          period_map_storage[["2000"]],
          period_map_storage[["2005"]],
          period_map_storage[["2010"]],
          period_map_storage[["2015"]]
        ),
        n_countries = length(unique(df_graph$country)),
        reg_title = reg_title,
        title = title,
        base_font_size = base_font_size,
        n_total = n_total,
        polys_total = polys_total,
        points_total = points_total
      )
      dev.off()
    }
  }

  # Make the year bin plots - one for each of four years
  # Save these plots only if anunual period plots are being made or if they have
  #  been specified in the input arguments!
  if (annual_period_maps | save_period_maps) {
    for (period in map_these_periods) {
      if (is.null(region_title)) {
        out_file <- paste0(out_dir, "map_", period, "_", region, ".png")
      } else {
        out_file <- paste0(out_dir, "map_", period, "_", region_title, ".png")
      }
      message(paste0("Saving ", out_file))
      unlink(out_file) # Delete any old versions

      png(
        filename = out_file,
        units = "in",
        width = 17,
        height = 10,
        pointsize = 24,
        res = 450
      )

      print(period_map_storage[[as.character(period)]])

      dev.off()
    }
  }


  # V. Finish up -------------------------------------------------------------

  if (prep_shiny) {
    # Save some shiny outputs
    prep_data_coverage_shiny(df, df_graph_poly, poly_shapes_all, var, indicator)
  }

  ## Return individual maps if requested
  if (return_maps == TRUE) {
    return(period_map_storage)
  }
}
