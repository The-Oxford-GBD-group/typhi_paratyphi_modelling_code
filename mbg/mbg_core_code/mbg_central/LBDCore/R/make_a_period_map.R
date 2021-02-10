#' Make a data coverage map
#'
#' @description Takes a background map and adds polygon and point data to it
#'
#' @param period the year or set of years to make a map for
#' @param df data.table output from `dcp_merge_with_gbd_locations()`
#' @param region name of region to be mapped
#' @param poly_shapes_all a combined SPDF with all needed polygons,
#' output by `pull_polys_in_parallel()`
#' @param background_map gg geom_path, outline of region. output of `make_background_map()`
#' @param background_outline gg geom_poly, background map of region with country borders.
#' output of `make_background_map()`
#' @param background_map_not_epidemic gg geom_poly, background map
#' with countries marked non-endemic or stage 3 grayed out. output of `make_background_map()`
#' @param background_extent extent of polygon defining region.
#' output of `make_background_map()`
#' @param disputed_map ggplot object for disputed territories
#' @param disputed_bg ggplot BACKGROUND (white) object for disputed territories
#' @param df_graph_poly data.table output of `dcp_make_df_graph()`, subsetted to countries
#' in region
#' @param df_graph_point data.table output of `dcp_make_df_graph()`, subsetted to countries
#' in region
#' @param not_real_shapefiles list of shapefiles not found in loading directory,
#' output of `dcp_check_for_missing_shapefiles()`
#' @param color_list list of hex colors for map color scale, output of `get_color_list()`
#' @param legend_title Title of the legend (color scale) on the map
#' @param log_dir Path to a directory where a log file will
#' optionally be saved. Useful mainly for debugging
#' @param base_font_size font size. All font sizes in the plots are scaled off this
#' @param map_point_size Size of points on map
#' @param cap numeric value, see `cap_type`
#' @param cap_type The type of numeric cap used to set
#' the legend maximum. Must be one of `'percentile'`, `'absolute'`,
#' or `'none'`.
#' @param legend_min Absolute lower bound for the map legend.
#' Defaults to the lowest observation in the dataset.
#' @param legend_max Absolute upper bound for the map legend. If
#' something other than `NA`, overrides the `cap` and `cap_type` arguments.
#' @param poly_line_width The default width of white lines
#' surrounding polygons on the map.
#' @param annual_period_maps boolean. If true, map title is the year, else a year bin
#'
#' @return returns a complete data coverage map for one period
#'
#' @export
make_a_period_map <- function(period,
                              df,
                              region,
                              poly_shapes_all,
                              background_map,
                              background_outline,
                              background_map_not_endemic,
                              background_extent,
                              disputed_map,
                              disputed_bg,
                              df_graph_poly,
                              df_graph_point,
                              not_real_shapefiles,
                              color_list,
                              legend_title,
                              log_dir,
                              base_font_size,
                              map_point_size,
                              cap,
                              cap_type,
                              legend_min,
                              legend_max,
                              poly_line_width,
                              annual_period_maps) {

  # A blank theme
  theme_empty <- theme_classic(base_size = base_font_size) +
    theme(
      axis.line = element_blank(), axis.text.x = element_blank(),
      axis.text.y = element_blank(), axis.ticks = element_blank(),
      axis.title.x = element_blank(),
      axis.title.y = element_blank()
    )

  ## Subset data to this period
  df_period_polys <- df_graph_poly[plot_year == period, ]
  df_period_points <- df_graph_point[plot_year == period, ]

  ## Drop if shapefile not found
  df_period_polys <- df_period_polys[!(shapefile %in% not_real_shapefiles)]

  ## Initialize map
  g_map <- ggplot() +
    background_map +
    background_map_not_endemic

  # Merge polygons for this period to master spdf and add each one to map
  merge_poly_data <- function(this_survey, poly_shapes_all) {
    survey_subset <- df_graph_poly[nid == this_survey, ]
    survey_subset[, outcome_weighting := outcome * N]
    survey_subset <- survey_subset[, lapply(.SD, sum),
      .SDcols = c("N", "outcome_weighting"),
      by = c("location_code", "shapefile", "nid", "source")
    ]
    survey_subset[, outcome := outcome_weighting / N]
    survey_subset[, outcome_weighting := NULL]
    survey_shapes <- try(sp::merge(poly_shapes_all, survey_subset,
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

  # Function for printing informative warning messages about erroneous input data
  geog_warnings <- function(bad_data_df, geog_col, msg) {
    setnames(bad_data_df, geog_col, "geog_col")
    # List translating between geography field names and reporting names
    geog_report_names <- list(
      cluster_id = "Cluster IDs",
      location_code = "GAUL Codes"
    )
    # Get all unique NIDs in the bad data
    for (bad_nid in unique(bad_data_df[, nid])) {
      nid_df <- bad_data_df[nid == bad_nid, ]
      source_name <- paste(unique(nid_df[, source]), collapse = ", ")
      geogs <- paste(unique(nid_df[, geog_col]), collapse = ", ")
      message(paste0(
        "    ", msg,
        "\n      Source: ", source_name,
        "\n      NID:    ", bad_nid,
        "\n      ", geog_report_names[[geog_col]], ": ", geogs
      ))
    }
  }

  # error checking - only do this if there is polygon data
  # otherwise, will leave the map as is - empty.

  # set up list of bad polys
  bad_polys <- c()
  poly_outside_list <- list()

  if (nrow(df_period_polys) > 0) {
    all_period_polys <- lapply(unique(df_period_polys[, nid]), function(this_survey) {
      merge_poly_data(this_survey, poly_shapes_all)
    })
    assign(paste0("poly_data_", period), all_period_polys)

    for (i in 1:length(all_period_polys)) {
      survey_spdf <- all_period_polys[[i]]

      if (length(survey_spdf) == 0) {
        bad_polys <- c(bad_polys, unique(df_period_polys[, survey])[i])
      }
      else {
        poly_dt <- suppressMessages(fortify(survey_spdf) %>% as.data.table())
        poly_dt <- try(merge(poly_dt, survey_spdf@data, by = "id"))
        if ("try-error" %in% class(poly_dt)) {
          message("printing head of survey_spdf@data to see survey details")
          head(survey_spdf@data)
          next
        }

        # new table for polys that are outside the extent of the background
        poly_outside <- poly_dt[lat < background_extent@ymin | lat > background_extent@ymax |
          long < background_extent@xmin | long > background_extent@xmax, ]

        poly_drop <- poly_dt[lat < background_extent@ymin - 0.5 |
          lat > background_extent@ymax + 0.5 |
          long < background_extent@xmin - 0.5 |
          long > background_extent@xmax + 0.5, ]

        ## if a given polygon falls outside of the extend of the background region, throw a warning
        if (nrow(poly_outside) > 0) {
          geog_warnings(
            bad_data_df = poly_outside,
            geog_col = "location_code",
            msg = "There are polygons outside of the background region range."
          )
          poly_outside_list[[length(poly_outside_list) + 1]] <- poly_outside
        }

        if (nrow(poly_drop) > 0) {
          geog_warnings(
            bad_data_df = poly_drop,
            geog_col = "location_code",
            msg = paste0(
              "The following polygons are VERY far away",
              " from background polygon and will be dropped.\n",
              "    Check the location of these polygons!"
            )
          )
          for (grp in unique(poly_drop$group)) {
            poly_dt <- subset(poly_dt, group != grp)
          }
        }

        g_map <- g_map +
          geom_polygon(
            data = poly_dt,
            aes(
              x = long,
              y = lat,
              group = group,
              fill = outcome
            ),
            color = "white",
            size = poly_line_width
          )
      }
    }

    # Drop if more than 0.5 degrees away from extent of background map
    if (length(poly_outside_list) > 0) {
      poly_outside_list <- rbindlist(poly_outside_list)
    }
  } else {
    assign(paste0("poly_data_", period), NULL)
  }

  # Report out bad polys
  if (length(bad_polys) > 0) {
    warning("BAD SPECIFIC POLYGON MERGES - polygons have been dropped")
    if (is.null(log_dir)) {
      warning(paste0("List of affected surveys: ", paste(bad_polys, collapse = ", ")))
    } else if (!is.null(log_dir)) {
      as.data.table(bad_polys) %>%
        write.csv(., file = paste0(log_dir, "bad_polys.csv"))
    }
  }

  ## determine if there are points that fall outside the background extent
  points_outside <- df_period_points[latitude < background_extent@ymin | latitude > background_extent@ymax |
    longitude < background_extent@xmin | longitude > background_extent@xmax, ]

  point_drop <- df_period_points[latitude < background_extent@ymin - 0.5 |
    latitude > background_extent@ymax + 0.5 |
    longitude < background_extent@xmin - 0.5 |
    longitude > background_extent@xmax + 0.5, ]

  ## if there are points outside of the extent, throw a warning and print the survey names + number of points outside
  if (nrow(points_outside) > 0) {
    geog_warnings(
      bad_data_df = points_outside,
      geog_col = "cluster_id",
      msg = "There are points outside of the background region range."
    )
  }
  if (nrow(point_drop) > 0) {
    geog_warnings(
      bad_data_df = point_drop,
      geog_col = "cluster_id",
      msg = paste0(
        "The following surveys have points that are VERY",
        " far away from background polygon and will be dropped.\n",
        "    Check the location of these points!"
      )
    )
  }

  df_period_points <- subset(
    df_period_points,
    !(latitude < background_extent@ymin - 0.5 | latitude > background_extent@ymax + 0.5 |
      longitude < background_extent@xmin - 0.5 | longitude > background_extent@xmax + 0.5)
  )


  ## Set up color scale
  range <- range(df$outcome, na.rm = T)
  ## If specified, set manual legend range values
  range[2] <- ifelse(!(is.na(legend_max)), legend_max, range[2])
  ## Set legend max based on percentiles, if specified
  ## This will only take effect if the percentile cap is greater than the minimum
  cap_greater <- (quantile(df$outcome, cap / 100, na.rm = T) > range[1])
  if (cap_type == "percentile" & is.na(legend_max)) {
    if (cap_greater == TRUE) {
      range[2] <- quantile(df$outcome, cap / 100, na.rm = T)
    } else {
      # Print a warning message
      message(paste0(
        "WARNING: The ", cap, "% upper legend bound specified is not greater ",
        "than the legend minimum. The legend upper bound will remain ",
        "at ", range[2], "."
      ))
    }
  }
  ## Set legend based on absolute value, if specified
  ## This will only take effect if the absolute cap is greater than the minimum
  cap_greater <- (cap > range[1])
  if (cap_type == "absolute" & is.na(legend_max)) {
    if (cap_greater) {
      range[2] <- cap
    } else {
      # Print a warning message
      message(paste0(
        "WARNING: The ", cap, " absolute legend bound specified is ",
        "not greater than the legend minimum. The legend upper ",
        "bound will remain at ", range[2], "."
      ))
    }
  }

  ## If the legend bounds are the same, add a very small number to the upper bound
  if (diff(range) <= 0) {
    message(paste0(
      "WARNING: The legend showed no variation in data. Increasing ",
      "the upper bound slightly to avoid plotting erorrs."
    ))
    range[2] <- range[1] + 1E-5
  }

  digits <- 10^(floor(log10(diff(range))) - 1)
  limits <- c(digits * floor(range[1] / digits), digits * ceiling(range[2] / digits))
  brks <- round(seq(limits[1], limits[2], length.out = 5), -1 * log10(digits))
  brks[c(1, 5)] <- limits
  labs <- format(brks)
  if (cap_type != "none" | !(is.na(legend_max))) labs[5] <- paste0(labs[5], "+")

  ## Finally, set up title text based on whether or not annual period maps are
  ##  being made
  if (annual_period_maps) {
    title_text <- as.character(period)
  } else {
    title_text <- paste0(period - 2, "-", period + 2)
  }

  ## Add points and finishing touches
  g_map <- g_map +
    background_outline +
    disputed_bg +
    disputed_map +
    geom_point(
      data = df_period_points,
      aes(
        x = longitude,
        y = latitude,
        fill = outcome
      ),
      alpha = 1,
      size = map_point_size,
      color = "black",
      shape = 21,
      stroke = 0.1
    ) +
    coord_equal() +
    theme_empty +
    scale_fill_gradientn(
      colours = color_list, limits = limits, breaks = brks, labels = labs,
      na.value = color_list[length(color_list)], name = paste0(legend_title, "\n")
    ) +
    guides(fill = guide_colorbar(barheight = 15, nbin = 1000)) +
    labs(
      fill = "Outcome",
      title = title_text
    ) +
    theme(plot.title = element_text(
      size = rel(1.5),
      face = "bold"
    ))
  return(g_map)
}
