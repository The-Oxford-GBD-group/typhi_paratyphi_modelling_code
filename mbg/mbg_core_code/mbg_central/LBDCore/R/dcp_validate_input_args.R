## SECTION 2 - DATA PREP AND VALIDATION FUNCTIONS
#' Validate input arguments (excluding input data.frame)
#'
#' @description: Make sure that all input arguments are valid before beginning
#' processing.
#'
#' @param input_list The input arguments to graph_data_coverage_values(), stored
#' as a list.
#'
#' @return Returns NULL (stops execution if any arguments are invalid)
#'
#' @export
dcp_validate_input_args <- function(input_list) {
  message("Validating input arguments...")
  # Evaluate all arguments so you don't get the variables containing the arguments
  input_list <- lapply(input_list, eval)
  # Ensure that all TRUE/FALSE arguments are actually booleans
  boolean_args <- c(
    "high_is_bad", "return_maps", "save_on_share", "fast_shapefiles",
    "simplify_polys", "remove_rank", "prep_shiny", "new_data_plots",
    "stage_3_gray", "annual_period_maps", "save_period_maps"
  )
  for (b_a in boolean_args) {
    if ("name" %in% class(input_list[[b_a]])) input_list[[b_a]] <- eval(input_list[[b_a]])
    if (!("logical" %in% class(input_list[[b_a]]))) {
      stop(paste0("'", b_a, "' must be either TRUE or FALSE"))
    }
  }
  # If 'annual_period_maps' is true, then 'save_period_maps' must be on
  if (input_list[["annual_period_maps"]] & !(input_list[["save_period_maps"]])) {
    stop(paste0(
      "If 'annual_period_maps' is TRUE, then 'save_period_maps' must",
      " also be TRUE."
    ))
  }
  # Check that all required character fields are actually characters
  char_args <- c(
    "var", "title", "year_var", "region", "indicator", "color_scheme",
    "color_scheme_scatter", "cap_type", "core_repo"
  )
  for (c_a in char_args) {
    if (!("character" %in% class(input_list[[c_a]]))) {
      stop(paste0("'", c_a, "' must be a of type 'character'."))
    }
  }
  # Check that all required numeric fields are actually numeric
  num_args <- c(
    "year_min", "year_max", "cores", "tolerance", "base_font_size",
    "map_point_size", "cap"
  )
  for (n_a in num_args) {
    if (!("numeric" %in% class(input_list[[n_a]]))) {
      stop(paste0("'", n_a, "' must be a of type 'numeric'."))
    }
  }
  # Check that the core_repo path actually exists
  core_repo <- gsub("/$", "", input_list[["core_repo"]])
  if (!file.exists(core_repo)) stop("Check that the 'core_repo' path is correct.")
  # Check that the 'cap_type' is in a list of valid options
  if (!(input_list[["cap_type"]] %in% c("percentile", "absolute", "none"))) {
    stop("'cap_type' must be one of 'percentile','absolute', or 'none'.")
  }
  # Check that if out_dir is not null, then it is a character vector
  out_dir <- input_list[["out_dir"]]
  if (!is.null(out_dir)) {
    if (!("character" %in% class(out_dir))) {
      stop("'out_dir' must be either a character vector or NULL.")
    }
  }
  # Check that the year ranges are somewhat reasonable and year_min < year_max
  year_min <- input_list[["year_min"]]
  year_max <- input_list[["year_max"]]
  if ((year_min < 1900) | (year_min > 2018) | (year_max < 1900) | (year_max > 2018)) {
    stop("Check that your year_min and year_max are in a reasonable range...")
  }
  if (year_min > year_max) {
    stop("Check that year_min is less than or equal to year_max.")
  }


  # # Check that the region is valid
  # # This check must be kept up to date with the get_country_list() function!

  # checks the input region against the reference list to see if valid
  # outputs WARNING with undefined region and ERROR directing to ref list
  if (length(get_gaul_codes(input_list[["region"]])) == 0) {
    stop(paste0("The mapping region must be defined in the
                get_admin0_code() reference list."))
  }

  # Check that the color scheme names are valid
  valid_color_schemes <- c(
    "classic", "darker_middle", "red_blue", "carto_red_blue",
    rownames(brewer.pal.info[brewer.pal.info$category == "seq", ])
  )
  if (!(input_list[["color_scheme"]] %in% valid_color_schemes)) {
    stop(paste0(
      "'color_scheme' must be one of the following:\n -",
      paste(valid_color_schemes, collapse = "\n -")
    ))
  }
  valid_scatter_color_schemes <- c("brewer", "binary", "carto1", "carto2", "carto3")
  if (!(input_list[["color_scheme_scatter"]] %in% valid_scatter_color_schemes)) {
    stop(paste0(
      "'color_scheme_scatter' must be one of the following:\n -",
      paste(valid_scatter_color_schemes, collapse = "\n -")
    ))
  }
  # Check that 'since_date' is either NULL or in the proper date format
  if (!is.null(input_list[["since_date"]])) {
    if (!(grepl(
      "[[:digit:]]{4}-[[:digit:]]{2}-[[:digit:]]{2}",
      input_list[["since_date"]]
    ))) {
      stop(paste0(
        "'since_date' must either be NULL or a character vector with",
        " the format YYYY-MM-DD"
      ))
    }
  }

  # The input arguments have been successfully validated
  message("All non-data input arguments are valid.")
  return(NULL)
}
