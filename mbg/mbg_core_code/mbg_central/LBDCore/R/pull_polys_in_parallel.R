#' Pull polygons in parallel
#'
#' @description Pulls the polygons from a master shapefile based on input data.table.
#' Calls `pull_polys()` in parallel and combines shapefiles into a single SPDF.
#'
#' @param shape_loc_list A data.table with columns 'shapefile' and 'location_code', where
#' 'location_code' corresponds to the GAUL_CODE column of corresponding 'shapefile'
#' @param shapefile_col name of shapefile column in `shape_loc_list`
#' @param location_code_col name of location_code column in `shape_loc_list`
#' @param cores number of cores to use in parallelization
#' (see `graph_data_coverage_values()` for more information)
#' @param fast_shapefiles Boolean. If true, reads in shapefile from RDS,
#' else reads from shapefile directory
#'
#'
#' @return A list with 2 objects -
#' * 'poly_shapes_all': A combined SPDF with all polygons combined
#' * 'broken_shapes': A list of shapefiles that did not load correctly
#'
#' @export
pull_polys_in_parallel <- function(shape_loc_list,
                                   shapefile_col = "shapefile",
                                   location_code_col = "location_code",
                                   cores,
                                   fast_shapefiles = fast_shapefiles) {


  # Format table & rename to standard format
  shape_loc_list <- shape_loc_list %>%
    as.data.table() %>%
    unique() %>%
    setnames(
      ., c(shapefile_col, location_code_col),
      c("shapefile", "location_code")
    )

  # Check for NAs
  na_rows <- shape_loc_list[is.na(shapefile) | is.na(location_code)]
  if (nrow(na_rows) > 0) {
    message("WARNING: You have NAs in your input data.  Check original data source & fix, as these will be dropped!")
    message("Affected rows:")
    print(na_rows)
    warning(paste0("NAs found in input data (", nrow(na_rows), " rows affected). These are being dropped! Check your input data & original source!"))
  }

  # Drop NAs
  shape_loc_list <- shape_loc_list[!is.na(shapefile) & !is.na(location_code)]

  # Get shapefiles
  shapefiles <- unique(shape_loc_list$shapefile)

  # Set up cluster
  cores <- min(c(cores, length(shapefiles))) # just get enough cores for shapefiles

  cl <- makeCluster(cores)
  registerDoParallel(cl)

  if (fast_shapefiles == T) {
    message("Fast-pulling polys...")
    poly_shape_list <- lapply(1:length(shapefiles), function(i) {
      shape <- shapefiles[i]
      shape_loc <- shape_loc_list[shapefile == shape]
      pull_polys(shape_loc, fast_shapefiles = T)
    })
  } else {
    message("Pulling polys in parallel. This may take a while ...")
    # Distribute packages
    clusterCall(cl, function(x) {
      .libPaths("/home/j/temp/geospatial/packages")
      library(rgdal)
      library(data.table)
    })

    poly_shape_list <- foreach(
      i = 1:length(shapefiles),
      .export = c("j_root", "pull_polys", "fast_load_shapefile"),
      .errorhandling = "pass"
    ) %dopar% {
      shape <- shapefiles[i]
      shape_loc <- shape_loc_list[shapefile == shape]
      pull_polys(shape_loc, fast_shapefiles = FALSE)
    }

    stopCluster(cl)
  }

  # Find broken shapefiles
  find_broken_shapes <- function(shape) {
    if ("error" %in% class(shape)) {
      return(TRUE)
    } else {
      return(FALSE)
    }
  }

  names(poly_shape_list) <- shapefiles

  broken_shape_names <- names(poly_shape_list)[unlist(lapply(poly_shape_list, find_broken_shapes))]
  working_shape_names <- names(poly_shape_list)[!(names(poly_shape_list) %in% broken_shape_names)]

  broken_shape_list <- lapply(broken_shape_names, function(x) poly_shape_list[[x]])
  names(broken_shape_list) <- broken_shape_names
  working_shape_list <- lapply(working_shape_names, function(x) poly_shape_list[[x]])
  names(working_shape_list) <- working_shape_names

  # Remake IDs to be unique across all of poly_shape_list (needed for rbind below)
  makeID_shiny <- function(shape) {
    shapefile <- poly_shape_list[[shape]]
    shapefile <- spChFIDs(shapefile, paste0(shape, "_", row.names(shapefile@data)))
    return(shapefile)
  }

  working_shape_list <- lapply(names(working_shape_list), makeID_shiny)

  ## check to make sure that all the projections are the same
  proj.strings <- unlist(lapply(working_shape_list, proj4string))
  if (length(unique(proj.strings)) > 1) {
    ## take the most common one and assign it to the others
    tab.strings <- table(proj.strings)
    common.proj <- names(tab.strings[which(tab.strings == max(tab.strings))])
    diff.strings.idx <- which(proj.strings != common.proj)
    for (ss in diff.strings.idx) {
      message(sprintf("changing the projection on shapefile: %s to match the majority of the other proj strings", shapefiles[ss]))
      message(sprintf("--- it will be changed from: %s", proj4string(working_shape_list[[ss]])))
      message(sprintf("--- to: %s", common.proj))
      message(sprintf("if those look like substantially different projections you should go back and fix the shapefile"))
      proj4string(working_shape_list[[ss]]) <- common.proj
    }
  }

  ## Combine all into one big SPDF
  poly_shapes_all <- do.call(rbind, working_shape_list)
  rm(working_shape_list)

  # Pull out the shapefile name and assign this as a new field
  poly_shapes_all@data$id <- rownames(poly_shapes_all@data)
  poly_shapes_all$shapefile <- gsub("_[^_]+$", "", poly_shapes_all$id)

  poly_shapes_all$location_code <- as.numeric(poly_shapes_all$GAUL_CODE)

  return(list("poly_shapes_all" = poly_shapes_all, "broken_shapes" = broken_shape_list))
}
