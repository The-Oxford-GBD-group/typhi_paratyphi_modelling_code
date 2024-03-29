

# Preamble ----------------------------------------------------------------


print(Sys.time())

## Point to personal directory (create if needed)
personal_lib <- sprintf(
  "~/R/x86_64-pc-linux-gnu-library/%s.%sgeo/",
  R.Version()$major, R.Version()$minor
)
Sys.setenv(R_LIBS_USER = personal_lib)
if (!dir.exists(personal_lib)) dir.create(personal_lib, recursive = TRUE)

## Set up .libPaths()
.libPaths(c(Sys.getenv("R_LIBS_USER"), .libPaths()))

## Set up MKLROOT directory (needed if using RStudio)
Sys.setenv(MKLROOT = "/opt/intel/compilers_and_libraries/linux/mkl")

## Load the LBDCore library
## The `suppressMessages()` is there just to remove all the intermediate
## packages' loading messages.
suppressMessages(library(LBDCore))

## Setup preambles
pipeline_preamble(headnode = FALSE)



# Prep For MBG ------------------------------------------------------------

# Bound GBM to 0-1 if desired
gbm_bounded_0_1 <- TRUE
if (exists("gbm_bounded_0_1")) {
  if (as.logical(gbm_bounded_0_1) == T & "gbm" %in% names(cov_list)) {
    message("Truncating GBM values >= 1 to 0.999 and <= 0 to 1e-4")
    values(cov_list[["gbm"]])[values(
      cov_list[["gbm"]]
    ) >= 1 & !is.na(values(cov_list[["gbm"]]))] <- 0.999
    values(cov_list[["gbm"]])[values(
      cov_list[["gbm"]]
    ) <= 0 & !is.na(values(cov_list[["gbm"]]))] <- 1e-4
    gbm_cols <- grep(paste0("(gbm)(.*_pred)"),
      names(df),
      value = T
    )
    replace_one <- function(x) {
      x[x >= 1 & !is.na(x)] <- 0.999
      return(x)
    }
    replace_zero <- function(x) {
      x[x <= 0 & !is.na(x)] <- 1e-4
      return(x)
    }
    df[, (gbm_cols) := lapply(.SD, replace_one), .SDcols = gbm_cols]
    df[, (gbm_cols) := lapply(.SD, replace_zero), .SDcols = gbm_cols]
  }
}

## convert stackers to transform space, if desired
## NOTE: we do this here to ensure that the stacker rasters are
## saved in prevalence/untransformed space
## this is useful for diagnostics and other code that was built
## expecting the untransformed rasters
if (
  as.logical(DAG_obj$pipeline$config_list$stackers_in_transform_space)
  && as.logical(DAG_obj$pipeline$config_list$use_stacking_covs)
  && DAG_obj$pipeline$config_list$indicator_family == "binomial") {
  message("Converting stackers to logit space")

  ## transform the rasters
  for (ii in child_model_names) {

    ## Preserve variable names in the raster first
    tmp_rastvar <- names(cov_list[[ii]])

    ## Logit
    cov_list[[ii]] <- logit(cov_list[[ii]])

    ## Reassign names
    names(cov_list[[ii]]) <- tmp_rastvar
    rm(tmp_rastvar)
  }

  ## transform the stacker values that are in df
  stacker_col_regexp <- sprintf(
    "(%s)(.*_pred)", paste(child_model_names, collapse = "|")
  )
  stacker_cols <- grep(stacker_col_regexp, names(df), value = TRUE)
  df[, (stacker_cols) := lapply(.SD, logit), .SDcols = stacker_cols]
}




# Prep Objects For MBG ----------------------------------------------------


## for stacking, overwrite the columns matching the model_names
## so that we can trick inla into being our stacker
if (DAG_obj$pipeline$config_list$use_stacking_covs) {
  df[, paste0(child_model_names) := lapply(
    child_model_names, function(x) get(paste0(x, "_cv_pred"))
  )]
}

## Generate MBG formula for INLA call (will run but not used by TMB)
mbg_formula <- build_mbg_formula_with_priors(
  fixed_effects = all_fixed_effects,
  add_nugget = DAG_obj$pipeline$config_list$use_inla_nugget,
  nugget_prior = DAG_obj$pipeline$config_list$nugget_prior,
  add_ctry_res = DAG_obj$pipeline$config_list$use_country_res,
  ctry_re_prior = DAG_obj$pipeline$config_list$ctry_re_prior,
  temporal_model_theta1_prior = DAG_obj$pipeline$config_list$rho_prior,
  no_gp = !as.logical(DAG_obj$pipeline$config_list$use_gp),
  use_space_only_gp           = as.logical(DAG_obj$pipeline$config_list$use_space_only_gp),
  use_time_only_gmrf          = as.logical(DAG_obj$pipeline$config_list$use_time_only_gmrf),
  time_only_gmrf_type         = DAG_obj$pipeline$config_list$time_only_gmrf_type,
  coefs.sum1 = DAG_obj$pipeline$config_list$coefs_sum1,
  subnat_RE                   = as.logical(DAG_obj$pipeline$config_list$use_subnat_res),
  subnat_country_to_get       = DAG_obj$pipeline$config_list$subnat_country_to_get,
  subnat_re_prior             = DAG_obj$pipeline$config_list$subnat_re_prior,
  timebycountry_RE = as.logical(DAG_obj$pipeline$config_list$use_timebyctry_res),
  adm0_list = gaul_list
)

## If needed, add fake data to make sure INLA estimates all years
missing_years <- base::setdiff(
  DAG_obj$pipeline$config_list$year_list,
  df$year
)

print("Missing years: ")
print(missing_years)

## For INLA we need to add data for missing time points to ensure we get predictions
##  for all relevant time points. The 0 observations do not contribute to the 
##  model fitting but they prevent INLA from auto-removing 
##  random effects that (conditionally) have no data impacting their fit
if (!as.logical(DAG_obj$pipeline$config_list$fit_with_tmb)) {
  if(as.logical(DAG_obj$pipeline$config_list$use_timebyctry_res)) {
    ## If we are using a time only effect by country then we need to make sure 
    ##  all year effects are estimated for each country.
    df$adm0code <- gaul_convert(df$country)
    for(adm0_code in gaul_list) {
      dfsub <- df[df$adm0code == adm0_code, ]
      if (length(missing_years) > 0) {
        fake_data <- dfsub[1:length(missing_years), ]
        fake_data[, year := missing_years]
        fake_data[, c(indicator, 'N', 'weight') := 0]
        fake_data[, period := NULL]
        fake_data <- merge(fake_data, period_map)
        df <- rbind(df, fake_data)
      }
    }
  } else {
    ## If not, we only need to make sure we have an observation for each missing
    ##  year (country irrelevant)
    if (length(missing_years) > 0) {
      fake_data <- df[1:length(missing_years), ]
      fake_data[, year := missing_years]
      fake_data[, c(indicator, 'N', 'weight') := 0]
      fake_data[, period := NULL]
      fake_data <- merge(fake_data, period_map)
      df <- rbind(df, fake_data)
    }
  }
}

#get covariate constraints for data stack
cov_constraints <- covariate_constraint_vectorize(DAG_obj$pipeline$config_list$fixed_effects, 
                                                  DAG_obj$pipeline$config_list$gbd_fixed_effects, 
                                                  DAG_obj$pipeline$config_list$fixed_effects_constraints, 
                                                  DAG_obj$pipeline$config_list$gbd_fixed_effects_constraints)

## Create SPDE INLA stack
## note that merge (if using TMB) will return data in a different
## but internally consistent) order, just different than df
input_data <- build_mbg_data_stack(
  df = df,
  yl = DAG_obj$pipeline$config_list$year_list,
  fixed_effects = all_fixed_effects,
  mesh_s = mesh_s,

  # mest_t not currently implemented with tmb
  mesh_t = mesh_t,
  use_ctry_res = DAG_obj$pipeline$config_list$use_country_res,

  use_nid_res = as.logical(DAG_obj$pipeline$config_list$use_nid_res),

  # nuggest implemented with tmb
  use_nugget = DAG_obj$pipeline$config_list$use_inla_nugget,

  # raw covs will get center scaled here though (see notes above)
  exclude_cs = child_model_names,
  
  # prior for Matern GP
  spde_prior = DAG_obj$pipeline$config_list$spde_prior,

  #  sum-to-1 not currenlty implemented tmb
  coefs.sum1 = DAG_obj$pipeline$config_list$coefs_sum1,
  tmb = DAG_obj$pipeline$config_list$fit_with_tmb,
  scale_gaussian_variance_N = DAG_obj$pipeline$config_list$scale_gaussian_variance_N,
  shapefile_version = DAG_obj$pipeline$config_list$modeling_shapefile_version,

  # if zl is not zero and tmb==TRUE,
  # it will trigger 3rd kronecker and fixed effects
  zl = DAG_obj$pipeline$config_list$z_list,
  zcol = DAG_obj$pipeline$config_list$zcol,
  cov_constraints = cov_constraints,
  use_gp = as.logical(DAG_obj$pipeline$config_list$use_gp), 
  use_space_only_gp = as.logical(DAG_obj$pipeline$config_list$use_space_only_gp),
  use_time_only_gmrf = as.logical(DAG_obj$pipeline$config_list$use_time_only_gmrf),
  timebycountry_RE = as.logical(DAG_obj$pipeline$config_list$use_timebyctry_res),
  adm0_list = gaul_list,
  st_gp_int_zero = as.logical(DAG_obj$pipeline$config_list$st_gp_int_zero),
  s_gp_int_zero = as.logical(DAG_obj$pipeline$config_list$s_gp_int_zero),
  use_age_only_gmrf = as.logical(DAG_obj$pipeline$config_list$use_age_only_gmrf),
  seed = seed,
  
  #... args (config args to be passed to build_mbg_data_stack_tmb)
  indicator_family = DAG_obj$pipeline$config_list$indicator_family,
  nugget_prior = DAG_obj$pipeline$config_list$nugget_prior,
  ctry_re_prior = DAG_obj$pipeline$config_list$ctry_re_prior,
  nid_re_prior = DAG_obj$pipeline$config_list$nid_re_prior,
  use_s2_mesh = DAG_obj$pipeline$config_list$use_s2_mesh,
  mesh_s_max_edge = DAG_obj$pipeline$config_list$mesh_s_max_edge,
  mesh_s_offset = DAG_obj$pipeline$config_list$mesh_s_offset,
  s2_mesh_params = DAG_obj$pipeline$config_list$s2_mesh_params
)

## combine all the inputs, other than cs_df
## these are not used if you are using TMB
stacked_input <- input_data[[1]]
spde <- input_data[[2]]
cs_df <- input_data[[3]]

## Generate other inputs necessary
outcome <- df[[indicator]] # N+_i - event obs in cluster
N <- df$N # N_i - total obs in cluster
weights <- df$weight

## catch in case there is no weight column
if (is.null(weights)) {
  weights <- rep(1, nrow(df))
}

set.seed(seed)
seed <- increment_seed(seed)

# Fit The MBG -------------------------------------------------------------

if (!as.logical(DAG_obj$pipeline$config_list$skipinla)) {
  if (!as.logical(DAG_obj$pipeline$config_list$fit_with_tmb)) {
    message("Fitting model with R-INLA")

    model_fit <- fit_mbg(
      indicator_family = DAG_obj$pipeline$config_list$indicator_family,
      stack.obs = stacked_input,
      spde = spde,
      cov = outcome,
      N = N,
      int_prior_mn = DAG_obj$pipeline$config_list$intercept_prior,
      f_mbg = mbg_formula,
      run_date = run_date,
      keep_inla_files = DAG_obj$pipeline$config_list$keep_inla_files,
      cores = Sys.getenv("SGE_HGR_fthread"),
      wgts = weights,
      intstrat = DAG_obj$pipeline$config_list$intstrat,
      fe_sd_prior = 1 / 9,
      verbose_output = TRUE
    ) ## this actually sets precision!. prec=1/9 -> sd=3
  } else {
    message("Fitting model with TMB")
    message(sprintf(
      "%s Data points and %s mesh nodes",
      nrow(df),
      length(input_data$Parameters$Epsilon_stz)
    ))

    # save RDS file of input data for replication
    saveRDS(
      object = input_data, ## save this here in case predict dies
      file = sprintf(
        "/share/geospatial/mbg/%s/%s/output/%s/%s_TMB_data_input_list_%s_holdout_%s_agebin_%s.RDS",
        indicator_group, indicator, run_date,
        ifelse(
          DAG_obj$pipeline$config_list$fit_with_tmb, "tmb", "inla"
        ),
        reg, holdout, age
      )
    )
    # run the model
    model_fit <- fit_mbg_tmb(
      lbdcorerepo = DAG_obj$pipeline$core_repo,
      cpp_template = "mbg_tmb_model",
      tmb_input_stack = input_data,
      control_list = list(
        trace = 1,
        eval.max = 500, iter.max = 300, abs.tol = 1e-20
      ),
      optimizer = "nlminb", # TODO add optimx
      ADmap_list = NULL,
      seed       = seed
    )

    # clamping
    clamp_covs <- DAG_obj$pipeline$config_list$clamp_covs
  }

  saveRDS(
    object = model_fit, ## save this here in case predict dies
    file = sprintf(
      "/share/geospatial/mbg/%s/%s/output/%s/%s_model_fit_pre_preds_%s_holdout_%s_agebin_%s.RDS",
      indicator_group, indicator, run_date,
      ifelse(
        DAG_obj$pipeline$config_list$fit_with_tmb, "tmb", "inla"
      ),
      reg, holdout, age
    )
  )
} else {
  ## skipped fitting INLA so just load model and move to predict
  model_fit <- readRDS(
    file = sprintf(
      "/share/geospatial/mbg/%s/%s/output/%s/%s_model_fit_pre_preds_%s_holdout_%s_agebin_%s.RDS",
      indicator_group, indicator, run_date,
      ifelse(
        DAG_obj$pipeline$config_list$fit_with_tmb, "tmb", "inla"
      ), reg, holdout, age
    )
  )
}



# Postamble ---------------------------------------------------------------


pipeline_postamble(addl_objs_to_save = "seed")
q("no")
