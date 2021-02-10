#' @title Read z mapping file
#'
#' @description Read in a file that links z values to age values.
#'
#' @param z_map_file String that is the path to a csv file that has 2 columns.
#' The first contains the z values and the second contains the age mapping.
#' @param type String indicating what z represents. Currently only age is supported.
#'
#' @return A dataframe with 2 columns: the z value and corresponding age group.
#' @export
read_z_mapping <- function(z_map_file, type = "age") {
  ## NOTE: only works for age aggregation
  z_map <- read.csv(z_map_file)
  colnames(z_map) <- c("z", "value")
  if (type == "age") {
    ## Run a check that it matches the worldpop labeling scheme
    z_vals <- gsub("-", "", z_map$value) # remove any "-"
    z_vals <- str_pad(z_vals, width = 4, side = "left", pad = "0")

    pop_dir <- "/home/j/WORK/11_geospatial/01_covariates/00_MBG_STANDARD/worldpop/"
    pop_files <- list.files(pop_dir)
    # Restrict to "a#..." files
    pop_files <- pop_files[grep("^a[0-9]", pop_files)]

    valid_z_vals <- unique(substr(pop_files, 2, 5))

    if (!(all(z_vals %in% valid_z_vals))) {
      stop(paste0(
        "z_vals must match worldpop age groups.\nFor example, a valid z-value is ",
        valid_z_vals[6], ".\nSee ", pop_dir, " for more."
      ))
    }
    z_map$value <- z_vals
    return(z_map)
  } else {
    stop("The type of z aggregation must be set to 'age'")
  }
}
