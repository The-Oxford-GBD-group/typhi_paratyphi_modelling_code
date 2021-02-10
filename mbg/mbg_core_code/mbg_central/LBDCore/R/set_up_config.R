#' @title Set up config
#' @description Setting up configuration variables for an MBG run
#' @param repo Location where you've cloned the MBG repository for your indicator.
#' @param core_repo Location where you've cloned the lbd_core repository. Not necessary in the package version.
#' @param indicator_group Category of indicator, i.e. "education"
#' @param indicator Specific outcome to be modeled within indicator category, i.e. "edu_0"
#' @param config_name Name of configuration file in the indicator folder, Default: NULL
#' @param config_file Full path to configuration file that overrides \code{config_name}, Default: NULL
#' @param covs_name Name of covariates configuration file, Default: NULL
#' @param covs_file Full path to covariates configuration file that overrides \code{covs_name}, Default: NULL
#' @param post_est_only Set up only for post estimation? Default: FALSE
#' @param run_date Run date, Default: ''
#' @param push_to_global_env Should the config parameters be pushed to the global environment? Default: TRUE
#' @param run_tests Run the assertion tests? This will run the tests and error out if there's an 
#' inconsistent config parameter. Default: TRUE
#' @param return_list Return a list result or just the config? Default FALSE
#' @return Depends on return_list. If FALSE (default) returns just the MBG config (a list). If True, returns a
#' named list of configs, where "config" is the usual MBG config, and "fixed_effects_config" is the config info
#' of the fixed effects
#' @details DETAILS
#' @examples
#' \dontrun{
#' if (interactive()) {
#' config <- load_config(repo = core_repo,
#' indicator_group = indicator_group,
#' indicator = indicator,
#' config_name = 'config_training',
#' covs_name = 'covs_training')
#' }
#' }
#' @rdname set_up_config
#' @importFrom assertthat is.flag is.string is.number
#' @export
set_up_config <- function(repo, 
                          core_repo = repo,
                          indicator_group, 
                          indicator, 
                          config_name = NULL, 
                          config_file = NULL, 
                          covs_name = NULL, 
                          covs_file = NULL,
                          post_est_only = FALSE, 
                          run_date = "", 
                          push_to_global_env = TRUE,
                          run_tests = TRUE,
                          return_list = FALSE) {
  
  ###### Block 1: Equivalent to load_config ######
  
  print("[1/6] Load the configs")
  
  ####### Logic checking for model config ####### 
  ## Make sure only one of config_name or config_file are not null
  if (!is.null(config_name) & !is.null(config_file)) {
    stop("You must specify just one of config_name or config_file, not both", call. = FALSE)
  }
  
  ## Pull config from indicator repo
  if (is.null(config_name) & is.null(config_file)) {
    message("Pulling config from default folder, since config_name and config_file are NULL")
    ## If new model run, pull config from /share repo
    if (post_est_only == FALSE) 
      config <- data.table::fread(paste0(repo, "/", indicator_group, "/config_", indicator, ".csv"), header = FALSE)
    ## If running analysis on existing model, use config from that model's outputs folder
    if (post_est_only == TRUE) 
      config <- data.table::fread(paste0("/share/geospatial/mbg/", indicator_group, "/", indicator, "/output/", run_date, "/config.csv"))
  }
  
  ## Pull by specific config name
  if (!is.null(config_name) & is.null(config_file)) {
    message("Pulling config from specified name")
    config <- data.table::fread(paste0(repo, "/", indicator_group, "/", config_name, ".csv"), header = FALSE)
  }
  ## Pull specified config file
  if (is.null(config_name) & !is.null(config_file)) {
    message("Pulling config from specified filepath")
    config <- data.table::fread(config_file, header = FALSE)
  }
  
  ####### Logic checking for covariates config ####### 
  ## Make sure only one of covs_name or covs_file are not null
  if (!is.null(covs_name) & !is.null(covs_file)) {
    stop("You must specify just one of covs_name or covs_file, not both", call. = FALSE)
  }
  
  ## Covs not pulled
  if (is.null(covs_name) & is.null(covs_file)) {
    message("Not pulling covs since covs_name and covs_file are NULL")
    covs <- NULL
  }
  
  ## Pull by specific covs name
  if (!is.null(covs_name) & is.null(covs_file)) {
    message("Pulling covs from specified name")
    covs <- read_covariate_config(paste0(repo, "/", indicator_group, "/", covs_name, ".csv"))
  }
  
  ## Pull specified covs file
  if (is.null(covs_name) & !is.null(covs_file)) {
    message("Pulling covs from specified filepath")
    covs <- read_covariate_config(covs_file)
  }
  
  ## For parsimony, let's make sure that the config column names are V1 and V2
  config <- data.table(config)
  if(colnames(config)[1] != "V1" & colnames(config)[2] != "V2") {
    warning("Renaming config column names to V1 and V2. Please verify that 'config' is properly built")
    colnames(config) <- c("V1", "V2")
  }
  
  
  # If a covariate .csv file exists, use that instead
  if (!is.null(covs)) {
    
    # Grab fixed effects & measures (and gbd fixed effects & measures) from CSV if present
    
    # After update to data.table 1.11.4, 'T' and 'F' are not read in as logical, 
    ## but as characters, which we need to remedy here. 
    ## We are assuming that the 'covs.csv' has an 'include' and 'gbd' column here
    covs[, `:=`(gbd, as.logical(gbd))]
    covs[, `:=`(include, as.logical(include))]
    covs <- subset(covs, include == T)  # Use only those where include flag set to true
    fe <- subset(covs, gbd == F)
    update_fixed_effect_config_with_missing_release(fe)
    gbd <- subset(covs, gbd == T)
    gbd[measure != "output", `:=`(measure, "covariate")]  # FIXME: This is a hack for backwards compatability -- basically it assumes you meant 'covariate' if you specified anything other than 'outcome' (eg, mean or NA)
    fixed_effects <- paste0(fe$covariate, collapse = " + ")
    fixed_effects_measures <- paste0(fe$measure, collapse = " + ")
    gbd_fixed_effects <- paste0(gbd$covariate, collapse = " + ")
    gbd_fixed_effects_measures <- paste0(gbd$measure, collapse = " + ")

    if(!("constraint" %in% names(covs))){
      fixed_effects_constraints <- paste0("c(", paste(rep(0, nrow(fe)), collapse=", "), ")")
      gbd_fixed_effects_constraints <- paste0("c(", paste(rep(0, nrow(gbd)), collapse=", "), ")")
    }
    else{
      fixed_effects_constraints <- paste0("c(", paste(unname(fe$constraint), collapse=", "), ")")
      gbd_fixed_effects_constraints <- paste0("c(", paste(unname(gbd$constraint), collapse=", "), ")")
    }
    
    # Remove any other versions from original config and 
    # override with covariates config outputs
    all_varz <- c(
      "fixed_effects", "fixed_effects_measures", "fixed_effects_constraints",
      "gbd_fixed_effects", "gbd_fixed_effects_measures", "gbd_fixed_effects_constraints"
    )
    for(varz in all_varz) {
      if(!(varz %in% colnames(config))) {
        config <- rbindlist(list(config, data.table(V1 = varz, V2 = get(varz))))
      } else {
        config[V1 == varz, V2:= get(varz)]
      }
    }
    
  }
  
  
  ###### Block 2: Add fields in config that are not in the default set ######
  
  print("[2/6] Add fields that are in the default config set but not in user's config")
  
  ## Load in the default config dataset
  if (.in.package()) {
    data("default_config_values", package = packageName())
  } else {
    default_config_values <- data.table::fread(file.path(core_repo, '/mbg_central/share_scripts/common_inputs/config_values.csv'), header = TRUE, stringsAsFactors = FALSE)
  }

  ## Now, go through each of the values in `config_values` and 
  ## add on all the fields that are not in the user-specified config
  config <- set_default_config_values(config, default_config_values)
  
  
  ###### Block 3: Extra parameters in config ######
  
  print("[3/6] Add fields that are in user's config but not in the default config set")
  message("\nAdditional covariates: ")
  extras <- config$V1[!(config$V1 %in% names(default_config_values))]
  for (extra in extras) {
    message(paste0("  ", extra, ": ", config[V1 == extra, V2] ))
  }

  ###### Block 4: Print out shapefile info from config. Resolve 'current' to fixed version date ######
  
  print("[4/6] Print out shapefile info from config")

  ## get the shapefile info
  m.sf.info <- detect_adm_shapefile_date_type(shpfile_path = get_admin_shapefile(version = config[V1 == 'modeling_shapefile_version', V2]))
  r.sf.info <- detect_adm_shapefile_date_type(shpfile_path = get_admin_shapefile(version = config[V1 == 'raking_shapefile_version', V2]))

  ## replace shapefile version in config (and env variable) with the actual version date
  ## if a specific date was already set, nothing changes.
  ## if 'current' had been selected, then it will be replaced by the version date that is currently symlinked to 'current'
  config[V1 == 'modeling_shapefile_version', V2 := m.sf.info$shpfile_date]
  config[V1 == 'raking_shapefile_version', V2 := r.sf.info$shpfile_date]

  ## print out the shapefile info
  message("\n\n\nSHAPEFILE VERSION INFORMATION: ")
  message(sprintf("\n--MODELING SHAPEFILE VERSION: %s -- which contains %s codes", m.sf.info$shpfile_date, toupper(m.sf.info$shpfile_type)))
  message(sprintf("\n--RAKING SHAPEFILE VERSION:   %s -- which contains %s codes\n", r.sf.info$shpfile_date, toupper(r.sf.info$shpfile_type)))
  
  
  ###### Block 5: Run tests on all the configuration variables loaded ######
  if(run_tests) {
    print("[5/6] Running simple type-assertion tests on config parameters")
    if (.in.package()) {
      data("config_tests", package = packageName())
    } else {
      config_tests <- data.table::fread(paste0(core_repo, '/mbg_central/share_scripts/common_inputs/config_tests.csv'), header = TRUE, stringsAsFactors = FALSE)
    }

    ## Test for params only in the config_tests list of params
    for (param in sort(config[, V1])) {
      cat(paste0("Testing config parameter: ", param, " "))
      if(param %in% config_tests$variable) {
        test_call_1 <- config_tests[variable == param, test_call]
        test_call_2 <- config_tests[variable == param, extra_test1]
        test_call_3 <- config_tests[variable == param, extra_test2]
        
        if(test_call_1 != "") {
          ## For a string in the config file, the eval-parse combo will
          ## fail to evaluate it, and so we build in this exception for that
          tryCatch(
            get(test_call_1)(ez_evparse(config[V1 == param, ], "V2")),
            error = function(e) {
              if(attributes(e)$class[[1]] == 'simpleError') {
                if (test_call_1 == "is.string") {
                  message(paste0("Assertion on ", param, " errored out because it's tested as a string. Please check for the real type manually"))
                } else {
                  stop(sprintf("%s errored with message: %s", test_call_1, geterrmessage()))
                }
              }
            }
          )
        }
        if(test_call_2 != ""  ) {
          tryCatch(
            assertthat::assert_that(eval(parse(text = test_call_2))),
            error = function(e) {
              stop(paste0("The following test failed: ", test_call_2) )
            }
          )
        }
        if(test_call_3 != ""  ) {
          tryCatch(
            assertthat::assert_that(eval(parse(text = test_call_3))),
            error = function(e) {
              stop(paste0("The following test failed: ", test_call_3) )
            }
          )
        }
        cat(" OK. \n")
      }
    }
    
    ## Stop if using z or poly aggregation strategies without TMB
    if(as.logical(config[V1 == "poly_ag", "V2"]) | config[V1 == "zcol_ag", "V2"] != "NULL") {
      if(!as.logical(config[V1 == "fit_with_tmb", "V2"])) {
        stop("Must use TMB when incorporating polygon or aggregated z data")
      }
      if(as.logical(config[V1 == "makeholdouts", "V2"])) {
        stop("There is aggregated data and functionality for making holdouts is 
             not yet implemented. Set makeholdouts to FALSE.")
      }
      if(as.logical(config[V1 == "test", "V2"])) {
        stop("Testing with aggregated data not yet implemented. Set test to FALSE.")
      }
    }
      
  } else {
    warning("[5/6] Skipping over type-assertion")
  }
  
  
  

  ###### Final Block :  Assign all the covariates to the environment if desired ######
  
  if(push_to_global_env) {
    print("[6/6] Pushing config parameters into global environment")
    for (param in config[, V1]) { 
      assign(param, config[V1 == param, V2], envir = globalenv())
    }
    if (!is.null(covs)) {
      assign("fixed_effects_config", fe, envir = globalenv())
      assign("gbd_fixed_effects_config", gbd, envir = globalenv())
    }
    # Processing of z config arguments
    if (zcol_ag != "NULL") {
      if (exists("z_ag_mat_file")) {
        assign("z_ag_mat", read.csv(z_ag_mat_file, header=F), envir = globalenv())
      } else {
        assign("z_ag_mat", NULL, envir = globalenv())
        assign("zcol_ag_id", NULL, envir = globalenv())
      }
      if (exists("z_map_file")) {
        assign("z_map", read_z_mapping(z_map_file), envir = globalenv())
      } else {
        assign("z_map", NULL, envir = globalenv())
      }
    } else {
      assign("zcol_ag", NULL, envir = globalenv())
      assign("z_map", NULL, envir = globalenv())
      assign("z_ag_mat", NULL, envir = globalenv())
      assign("zcol_ag_id", NULL, envir = globalenv())
    }

    #convert seed into int from character if set in config
    if (!is.null(config[V1 == "seed", V2])) assign("seed", eval(parse(text=seed)), envir = globalenv()) 
  } else {
    print("[6/6] Config parameters not passed into global environment")
  }
  
  ## Return the config data.table
  message("Saving out config...")
  if (return_list) {
    return(list("config" = config, "fixed_effects_config" = fe))
  } else {
    return(config)
  }
}
