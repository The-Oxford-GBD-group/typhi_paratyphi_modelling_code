###########################################################
### Author: Patrick Liu
### Date: 1/26/2015
### Project: ST-GPR
### Purpose: Utility functions for modeling
###########################################################

####################################################################################################################################################
# 															   Table of Contents
####################################################################################################################################################

## Background : ST-GPR codebase was rewritten into Python for GBD2019. Many file formats changed
## between the old and new codebases and had to be adjusted for during GBD2019 decomp.
## (In case you were wondering about all the almost-duplicate functions labeled _old)


## ST-GPR output-pulling functions

  ## stgpr_path : kind of like the pathlib for ST-GPR in R
  ## stgpr_path_old : old (R codebase) version of stgpr_path, paths for old formatting system

	## model_path : uses stgpr_path to return correct path for a given ST-GPR output object
  ## model_path_old : old (R codebase) version of model_path

	## model_load : returns data.table of requested ST-GPR output object
  ## model_load : old (R codebase) version of model_load

	## get_parameters : returns model parameters for given run_id - works for old and new runs

  ## h5read.py : Psychotic piece of code needed to read h5 files written in Python>
    # Note: starts failing on character columns. Hope that tidbit saves someone some work in the future
  ## get_data : pull the full range of ST-GPR outputs - only works for runs in new codebase

## Math Functions

	## logit
	## inv.logit
	## transform_data : convert data variable in dataframe to correct space
	## delta_transform : delta transform variance given data and variance cols in data.table

## Random stuff

  ## check_run : Checks progress of a given run_id
    ## returns 0 if run broke, 1 if run completed successfully, and 2 if still running
  ## is.blank : guess it checks for NAs or NaNs? dunno. old patty code. scared to delete.
  ## append_load : think it appends a bunch of files while loading. also old patty code.

## Amplitude functions

  ## count.country.years : counts the number of location-years for a given location or level
  ## calculate.mad : calculates Median Absolute Deviation
  ## calculate.amplitude : calculate amplitude given one of the many terrible methods in ST-GPR
  ## plot_amp : plot amplitude for a given cutoff
  # amplitude_by_cutoff : see what GLOBAL amplitude would be for any given cutoff value


####################################################################################################################################################
# 															 ST-GPR Output-Pulling Functions
####################################################################################################################################################
source('/ihme/code/st_gpr/central/stgpr/r_functions/utilities/db_tools.r')

library(rhdf5)
library(data.table)

stgpr_path <- function(obj) {

  ## main roots
  central_root <- '/ihme/code/st_gpr/central'
  cluster_model_output <- '/ihme/covariates/ubcov/model/output'

  # subdirs of central_root
  model_root <- sprintf('%s/model', central_root)
  r_functions_root <- sprintf('%s/r_functions', central_root)
  shells_root <- sprintf('%s/shells', central_root)

  # subdirs of r_functions_root
  r_registration_root <- sprintf('%s/registration', r_functions_root)
  r_utitlies_root <- sprintf('%s/utilities', r_functions_root)

  # references to old filepaths that are useful
  lib_root <- '/ihme/covariates/ubcov/model/st_gpr_library/databases'
  me_db <- sprintf('%s/me_db.csv', lib_root)
  model_db <- sprintf('%s/model_db.csv', lib_root)
  run_db <- sprintf('%s/run_db.csv', lib_root)

  return(get(obj))

}

stgpr_path_old <- function(obj) {

  ## Central Root
  central_root <- '/ihme/code/st_gpr/beta'

  if (obj == "central_root") {
    path_to_obj <- central_root
  } else {

    ## Load paths file
    paths <- fread(sprintf("%s/paths.csv", central_root))

    ## Check that object is in path list
    if (length(paths[object==obj, object]) != 1) stop(paste0("Check that object: ", paste0(obj), " is in ~/paths.csv"))

    ## Collapse paths together
    paths$path_full <- str_trim(apply(paths[,grep("path", names(paths), value=T), with=F], 1, paste, collapse=" "))

    ## Find object
    path_to_obj <- as.character(paths[object==obj, .(path_full)])

    ## Find parent object if there's a "," character
    while (grepl(",", path_to_obj)) {

      ## Find path dependencies
      dependency <- unlist(strsplit(path_to_obj, ","))[1]

      ## Run function to find path to dependency
      if (dependency != "central_root") {
        assign(dependency, stgpr_path_old(dependency))
      }

      ## Replace with path
      path_to_obj <- paste0(get(dependency), str_trim(unlist(strsplit(path_to_obj, ","))[2]))
    }
  }

  return(path_to_obj)
}

model_path <- function(run_id, obj, holdout = 0, param_set = 0) {

  #enforce data types
  run_id <- as.integer(run_id)
  holdout <- as.integer(holdout)
  param_set <- as.integer(param_set)

  #get output_root
  run_root <- stgpr_path('cluster_model_output')

  #connect objects to h5 files
  data = c('data', 'prepped')
  square = c('populations', 'square')
  parameters = c('parameters', 'density_cutoffs')
  temp = c('stage1')
  output = c('st', 'amp_nsv', 'adj_data', 'gpr', 'raked')
  objs <- c(data, square, parameters, temp, output)

  ## Verify object is in scope
  if (!(obj %in% objs)) stop(paste0(obj, " not in ", toString(objs)))

  #Find paths for all options
  for (f in c('data', 'square', 'parameters')){
    if (obj %in% get(f)){
      path = sprintf('%s/%i/%s.h5', run_root, run_id, f)
    }
  }

  for (f in c('temp')){
    if (obj %in% get(f)){
      path = sprintf('%s/%i/%s_%i.h5', run_root, run_id, f, holdout)
    }
  }

  for (f in c('output')){
    if (obj %in% get(f)){
      path = sprintf('%s/%i/%s_%i_%i.h5', run_root, run_id, f, holdout, param_set)
    }
  }

  return(path)
}

model_path_old <- function(run_id, obj, holdout=1) {

  ## Group objects
  data <- c("data")
  param <- c("parameters", "covariate_version", "location_hierarchy", "square", "kos", "st_amp", 'populations')
  temp <- c("preko", "prepped", "prior", "st", "adj_data", "gpr", "raked")
  output <- c("flat", "draws")
  objs <- c(data, param, temp, output)

  ## Check
  if (!(obj %in% objs)) stop(paste0(obj, " not in ", toString(objs)))

  ## Return Path
  if (obj %in% data) path <- sprintf('%s/data.h5', stgpr_path_old("cluster_model_output"))
  for (file in c("param", "temp", "output")) {
    if (obj %in% get(file)) path <-  paste0(stgpr_path_old("cluster_model_output"), "/", run_id, "/", file, "_", holdout, ".h5")
  }
  return(path)
}

model_load <- function(run_id, obj, holdout=0 , param_set = 0, last_old_run_id = 46904) {
  run_id <- as.integer(run_id)
  if(run_id < last_old_run_id){
    if(obj == 'stage1') obj = 'prior'
    if(obj == 'amp_nsv') obj = 'st_amp'
    return(model_load_old(run_id, obj))
  }else{
    path <- model_path(run_id, obj, holdout, param_set)
    if (obj == 'stage1') {
      return(h5read(path, obj) %>% data.table)
    } else if ((obj == 'parameters')){
      return(fread(sprintf('%s/%i/parameters.csv', stgpr_path('cluster_model_output'), run_id)))
    } else{
      return(h5read.py(path, obj) %>% data.table)
    }
  }
  print(paste("Loaded", obj, sep=" "))
}

model_load_old <- function(run_id, obj, holdout=1) {
  path <- model_path_old(run_id, obj, holdout)
  if (obj %in% c("kos")) {
    return(h5read.py(path, obj) %>% data.table)
  } else {
    return(h5read(path, obj) %>% data.table)
  }
  print(paste("Loaded", obj, sep=" "))
}

get_parameters <- function(run_id){
  
  #paths and stuff
  output_root <- stgpr_path('cluster_model_output')
  run_root <- sprintf('%s/%i', output_root, as.integer(run_id))
  
  tryCatch({
    
    param_path <- sprintf('%s/parameters.csv', run_root)
    parameters <- fread(param_path)
    # Back fill modelable_entity_id, path_to_custom_stage_1, and path_to_custom_covarates
    # with what's been ETL'd in the real database (for ME ID) or NA
    if (is.na(parameters$modelable_entity_id)) {
      parameters$modelable_entity_id <- get_modelable_entity_id(run_id)
      parameters$path_to_custom_stage_1 <- NA
      parameters$path_to_custom_covarates <- NA
    }
    parameters <- convert_param_names(parameters)
    return(parameters)
    
  }, error = function(cond){
    parameters <- get_parameters_old(run_id)
    parameters <- convert_param_names(parameters)
    return(parameters)
  })
}

get_parameters_old <- function(run_id) {
  ## Drop columns
  cols <- c("author", "uploader", "date", "notes", "best")
  drop_cols <- function(df, cols) {
    list <- paste(cols, collapse="|")
    return(df[, -grep(list, names(df), value=T), with=F])
  }
  
  ## MR 8.14.17: Adding tryCatch because running into errors where people are reading and writing simultaneously
  param_db <- tryCatch({
    param_db <- stgpr_path_old("me_db") %>% fread(., na.strings=c("NA", ""), showProgress=FALSE)
    param_db <- merge(param_db, stgpr_path_old("data_db") %>% fread(., na.strings=c("NA", ""), showProgress=FALSE) %>% drop_cols(., cols), by="me_name")
    param_db <- merge(param_db, stgpr_path_old("model_db") %>% fread(., na.strings=c("NA", ""), showProgress=FALSE) %>% drop_cols(., cols), by="me_name", allow.cartesian=TRUE)
    param_db <- merge(param_db, stgpr_path_old("run_db") %>% fread(., na.strings=c("NA", ""), showProgress=FALSE) %>% drop_cols(., cols), by=c("me_name","model_id", "data_id"))
  },
  error = function(cond) {
    Sys.sleep(10)
    param_db <- stgpr_path_old("me_db") %>% fread(., na.strings=c("NA", ""), showProgress=FALSE)
    param_db <- merge(param_db, stgpr_path_old("data_db") %>% fread(., na.strings=c("NA", ""), showProgress=FALSE) %>% drop_cols(., cols), by="me_name")
    param_db <- merge(param_db, stgpr_path_old("model_db") %>% fread(., na.strings=c("NA", ""), showProgress=FALSE) %>% drop_cols(., cols), by="me_name", allow.cartesian=TRUE)
    param_db <- merge(param_db, stgpr_path_old("run_db") %>% fread(., na.strings=c("NA", ""), showProgress=FALSE) %>% drop_cols(., cols), by=c("me_name","model_id", "data_id"))
  }
  )
  ## Subset based on parameters
  ifstatement <- "1==1"
  if (!is.null(get('run_id')))
    ifstatement <- paste(ifstatement, "&", 'run_id', "%in% c(", toString(get('run_id')), ")")
  parameters <- param_db[eval(parse(text=ifstatement))]
  
  ## If run_id given, check that only 1 row per run_id)
  if (!is.null(run_id)) if (length(run_id) != nrow(parameters)) stop("duplicates in run_id")
  
  # Back fill modelable_entity_id, path_to_custom_stage_1, and path_to_custom_covarates
  # with what's been ETL'd in the real database (for ME ID) or NA
  if (is.na(parameters$modelable_entity_id)) {
    parameters$modelable_entity_id <- get_modelable_entity_id(run_id)
    parameters$path_to_custom_stage_1 <- NA
    parameters$path_to_custom_covarates <- NA
  }
  
  
  
  return(parameters)
}

get_modelable_entity_id <- function(run_id) {
  res <- query(paste0("SELECT modelable_entity_id FROM stgpr.stgpr_version ",
                      "WHERE stgpr_version_id = ", run_id), conn_def = "epi")
  return(res$modelable_entity_id)
}

h5read.py = function(h5File, name) {

  listing = h5ls(h5File)

  if (!(name %in% listing$name)) stop(paste0(name, " not in HDF5 file"))

  # only take requested group (df) name
  listing = listing[listing$group==paste0('/',name),]

  # Find all data nodes, values are stored in *_values and corresponding column
  # titles in *_items
  data_nodes = grep("_values", listing$name)
  name_nodes = grep("_items", listing$name)

  data_paths = paste(listing$group[data_nodes], listing$name[data_nodes], sep = "/")
  name_paths = paste(listing$group[name_nodes], listing$name[name_nodes], sep = "/")

  columns = list()
  for (idx in seq(data_paths)) {
    data <- data.frame(t(h5read(h5File, data_paths[idx])))
    names <- t(h5read(h5File, name_paths[idx]))

    if (length(names) != length(data)){
      #print("There were issues with loading this sooo I skipped the problem columns. Hope they don't matter!")
    }else{
      entry <- data.frame(data)
      colnames(entry) <- names
      columns <- append(columns, entry)
    }
  }

  data <- data.frame(columns)

  return(data)
}

get_data <- function(run_id) {

  #get run root
  run_root <- sprintf('%s/%i', stgpr_path('cluster_model_output'), as.integer(run_id))

  # prep
  vars <- c("location_id", "year_id", "age_group_id", "sex_id")
  data_transform <- get_parameters(run_id = run_id)$data_transform

  #get best param set for pulling values
  fit_stats <- fread(sprintf('%s/fit_stats.csv', run_root))
  best_param <- fit_stats[best == 1, parameter_set] %>% unique

  ## Adjusted Data
  data <- model_load(run_id, 'adj_data', param_set = best_param)
  if(nrow(data)<=1) data <- model_load(run_id, 'adj_data_1')

  #calculate data upper and lower without nsv
  data[, tmp_data:=transform_data(data, data_transform, reverse = T)]
  if(!('original_variance' %in% names(data)) & ('nsv' %in% names(data))){
    data[, original_variance:=delta_transform(tmp_data, variance, data_transform, reverse = T)]
    data[, original_variance:=original_variance - nsv]
  }
  data[, variance_no_nsv:=delta_transform(original_data, original_variance, data_transform)]
  data[, data_upper_no_nsv:=data + 1.96*sqrt(variance_no_nsv)]
  data[, data_lower_no_nsv:=data - 1.96*sqrt(variance_no_nsv)]

  #back to stuff
  if(nrow(data)<=1) return(NULL)
  cols <- c(vars, "data", "variance", "nid", 'data_upper_no_nsv', 'data_lower_no_nsv', grep("cv_", names(data), value=T))
  if ("outlier_value" %in% names(data)) cols <- c(cols, "outlier_value")
  if (!("nid" %in% names(data))) data[, nid := NA]
  data <- data[, cols, with=F]
  data[, data_mod_space:=data]
  #calculate data upper and lower in modeling space
  data[, data_upper:=data + 1.96*sqrt(variance)]
  data[, data_lower:=data - 1.96*sqrt(variance)]
  data[, data_lower_mod_space:=data_lower]
  data[, data_upper_mod_space:=data_upper]
  #transform out
  transformed <- c('data', 'data_lower', 'data_upper', 'data_upper_no_nsv', 'data_lower_no_nsv')
  data[, (transformed):= lapply(.SD, function(x) transform_data(x, data_transform, reverse=T)), .SDcols = transformed]
  data[, variance := delta_transform(data, variance, data_transform, reverse=T)]

  ## Prior
  first_stage <- model_load(run_id, 'stage1')
  if(nrow(first_stage)<=1) return(NULL)
  first_stage[, stage1 := transform_data(stage1, data_transform, reverse=T)]

  ## Spacetime
  st <-  model_load(run_id, 'st', param_set = best_param)
  if(nrow(st)<=1) return(NULL)
  st[, st := transform_data(st, data_transform, reverse=T)]

  #Amplitude and scale info
  amp <- model_load(run_id, 'amp_nsv')

  ## GPR - already back-transformed to level space - verified 2/3/19
  gpr <- model_load(run_id, 'gpr', param_set = best_param)
  if(nrow(gpr)<=1) return(NULL)
  gpr_cols =c("gpr_mean","gpr_lower","gpr_upper")
  gpr_unraked_cols <-paste0(gpr_cols,'_unraked')
  setnames(gpr,gpr_cols,gpr_unraked_cols)
  #this is awwwwful but we're saving gpr means for models with draws in level space, without draws in modeling space. UGH.
  #10/12/18 dropping for now bc not true for new st-gpr version have to think of how to deal with this for GBD

  ## GPR Raked - already back-transformed to level space
  raked <- model_load(run_id, 'raked', param_set = best_param)
  if(nrow(raked)<=1) return(NULL)

  ## Merge
  dt <- raked
  dt <- merge(dt, gpr, by=vars, all.x=T)
  dt <- merge(dt, st, by=vars, all.x=T)
  dt <- merge(dt, amp, by = c('location_id', 'sex_id'), all.x = T)
  dt <- merge(dt, first_stage, by=vars, all.x=T)
  dt <- merge(dt, data, by=vars, all.x=T)

  # return dt
  return(dt)

}

get_data_old <- function(run_id, output_root = OUTPUT_ROOT, model_root = '/ihme/code/st_gpr/beta/model/'){
  
  #get run root
  run_root <- sprintf('%s/%i', stgpr_path('cluster_model_output'), as.integer(run_id))
  draws <- ifelse('draws_temp_1' %in% list.files(run_root), 1,0)
  
  # prep
  setwd(model_root)
  vars <- c("location_id", "year_id", "age_group_id", "sex_id")
  
  params <- get_parameters_old(run_id)
  data_transform <- params$data_transform
  gbd_round_id <- params$gbd_round_id
  gbd_round_id <- ifelse(is.null(gbd_round_id), get_gbd_round_id(params$location_set_version_id), gbd_round_id)
  
  ## Adjusted Data
  data <- model_load(run_id, 'adj_data')
  if(nrow(data)<=1) data <- model_load(run_id, 'adj_data_1')
  
  #calculate data upper and lower without nsv
  if(!('original_variance' %in% names(data)) & ('nsv' %in% names(data))){
    data[, tmp_data:=transform_data(data, data_transform, reverse = T)]
    data[, original_variance:=delta_transform(tmp_data, variance, data_transform, reverse = T)]
    data[, original_variance:=original_variance - nsv]
  }
  data[, variance_no_nsv:=delta_transform(original_data, original_variance, data_transform)]
  data[, data_upper_no_nsv:=data + 1.96*sqrt(variance_no_nsv)]
  data[, data_lower_no_nsv:=data - 1.96*sqrt(variance_no_nsv)]
  
  #back to stuff
  if(nrow(data)<=1) return(NULL)
  cols <- c(vars, "data", "variance", "nid", 'data_upper_no_nsv', 'data_lower_no_nsv', grep("cv_", names(data), value=T))
  if ("outlier" %in% names(data)) cols <- c(cols, "outlier")
  if (!("nid" %in% names(data))) data[, nid := NA]
  data <- data[, cols, with=F]
  data[, data_mod_space:=data]
  #calculate data upper and lower in modeling space
  data[, data_upper:=data + 1.96*sqrt(variance)]
  data[, data_lower:=data - 1.96*sqrt(variance)]
  data[, data_lower_mod_space:=data_lower]
  data[, data_upper_mod_space:=data_upper]
  #transform out
  transformed <- c('data', 'data_lower', 'data_upper', 'data_upper_no_nsv', 'data_lower_no_nsv')
  data[, (transformed):= lapply(.SD, function(x) transform_data(x, data_transform, reverse=T)), .SDcols = transformed]
  data[, variance := delta_transform(data, variance, data_transform, reverse=T)]
  if ('outlier' %in% names(data))
    setnames(data, 'outlier', 'outlier_value')
  
  ## Prior
  prior <- model_load(run_id, 'prior')
  if(nrow(prior)<=1) prior <- model_path(run_id, 'prior') %>% h5read(., '/prior/ELT1') %>% data.table
  if(nrow(prior)<=1) return(NULL)
  prior[, prior := transform_data(prior, data_transform, reverse=T)]
  
  ## Spacetime
  st <-  model_load(run_id, 'st')
  if(nrow(st)<=1) st <-  model_load(run_id, 'st_1')
  if(nrow(st)<=1) return(NULL)
  st[, st := transform_data(st, data_transform, reverse=T)]
  
  #Amplitude and scale info
  amp <- model_load(run_id, 'st_amp')
  
  ## GPR
  gpr <- model_load(run_id, 'gpr')
  if(nrow(gpr)<=1) return(NULL)
  gpr <- gpr[,c("location_id","year_id","age_group_id","sex_id","gpr_mean","gpr_lower","gpr_upper"),with=F]
  old <- c("gpr_mean", "gpr_lower", "gpr_upper")
  new <- paste(old, "_unraked", sep="")
  setnames(gpr, old, new)
  #this is awwwwful but we're saving gpr means for models with draws in level space, without draws in modeling space. UGH.
  if(draws == 0 & run_id < 45020){
    gpr[, (new) := lapply(.SD, function(x) transform_data(x, data_transform, reverse=T)), .SDcols=new]
  }
  
  ## GPR Raked
  raked <- model_load(run_id, 'raked')
  if(nrow(raked)<=1) return(NULL)
  
  ## Merge
  dt <- raked
  dt <- merge(dt, gpr, by=vars, all.x=T)
  dt <- merge(dt, st, by=vars, all.x=T)
  dt <- merge(dt, amp, by = vars, all.x = T)
  dt <- merge(dt, prior, by=vars, all.x=T)
  dt <- merge(dt, data, by=vars, all.x=T)
  
  #put in gbd round id
  dt[, gbd_round_id:=gbd_round_id]
  
  #format to new version
  setnames(dt, 'prior', 'stage1')
  
  # return dt
  return(dt)
  
}

####################################################################################################################################################
# 															 ST-GPR Reformatting Functions
####################################################################################################################################################

convert_param_names <- function(params){
  # Update parameter names in whatever comes through

  old_varnames <- c('st_lambdaa', 'custom_age_vector', 'amp_cutoff',
                    'amp_factor', 'amp_method', 'y_axis_title',
                    'data_offset', 'logit_raking', 'level_4_to_3_agg',
                    'level_5_to_4_agg', 'level_6_to_5_agg', 'stage1_model',
                    'covariates', 'prior_model')
  new_varnames <- c('st_lambda', 'st_custom_age_vector', 'gpr_amp_cutoff',
                    'gpr_amp_factor', 'gpr_amp_method', 'prediction_units',
                    'transform_offset', 'rake_logit', 'agg_level_4_to_3',
                    'agg_level_5_to_4', 'agg_level_6_to_5', 'stage_1_model_formula',
                    'gbd_covariates', 'stage_1_model_formula')
  df <- data.table(old = old_varnames, new = new_varnames)

  for(i in 1:nrow(df)){
    old_var <- df[i, old]
    new_var <- df[i, new]
    if(old_var %in% names(params)){
      setnames(params, old_var, new_var)
    }
  }

  return(params)
}

####################################################################################################################################################
# 															 Math Functions
####################################################################################################################################################

logit <- function(x) {
	return(log(x/(1-x)))
}

logit_offset <- function(x, offset) {

	x_len = length(x)

	value <- vector(mode="numeric", length=x_len)

	for (i in 1:x_len) {

		if (x[i]==1) {
			value[i] <- x[i] - offset
		} else if (x[i]==0)  {
			value[i] <- x[i] + offset
		} else value[i] <- x[i]

	}

	return(log(value/(1-value)))
}

inv.logit <- function(x) {
	return(exp(x)/(exp(x)+1))
}

transform_data <- function(var, space, reverse=F) {

  if (space %in% c('NA', NA, '', 'none', 'None')){
    # no need to do a thing, untransformed
  }else{
  	if (space == "logit" & reverse==F) {
  		var <- logit(var)
  	} else if (space == "logit" & reverse==T) {
  		var <- inv.logit(var)
  	} else if (space == "log" & reverse==F) {
  		var <- log(var)
  	} else if (space == "log" & reverse==T) {
  		var <- exp(var)
  	}
  }
	return(var)

}

delta_transform <- function(data, variance, space, reverse=F) {

  if (space %in% c('NA', NA, '', 'none', 'None')){
    # no need to do a thing, untransformed
  }else{

  	if (space == "logit" & reverse==F) {
  		variance <- variance * (1/(data*(1-data)))^2
  	} else if (space == "logit" & reverse==T) {
  		variance <- variance / (1/(data*(1-data)))^2
  	} else if (space == "log" & reverse==F) {
  		variance <- variance * (1/data)^2
  	} else if (space == "log" & reverse==T) {
  		 variance <- variance / (1/data)^2
  	}
  }
  return(variance)
}

####################################################################################################################################################
# 															 Random Functions
####################################################################################################################################################

check_run <- function(run_id) {
  ## Check output
  root <- sprintf('%s/%s', stgpr_path('cluster_model_output'), run_id)
  rakepath <- model_path(run_id, 'raked')
  ## Check if job done
  check <- as.integer(system(sprintf("qstat -u \\* | grep _%i | wc -l",run_id), intern=T))
  if (check == 0) {
    if('model_complete.csv' %in% list.files(root)){
      print('Run complete')
      flag <- 1
    }else{
      flag <- 0
      message("Run broke :( ")
    }
  } else {
    flag <- 2
    print("Still running")
  }
  return(flag)
}

is.blank <- function(x) {
	 any(is.null(x))  || any(is.na(x))  || any(is.nan(x))
}

append_load <- function(output, files, delete=FALSE) {
    if (file.exists(output)) unlink(output)
	script <- paste0(stgpr_path("shells_root"), "/append_csv.sh")
	files.str <- gsub(",", "", toString(files))
	cmd <- paste("bash", script, output, files.str, sep=" ")
	system(cmd)
	df <- fread(output)
	if (delete) unlink(output)
    return(df)
}

substrRight <- function(x, n){
  substr(x, nchar(x)-n+1, nchar(x))
}

####################################################################################################################################################
# 															 Amplitude Functions
####################################################################################################################################################

count.country.years <- function(df, location, year, age, sex, dt, lvl){

  #identify level we're looking at based on location, and restrict inputs to those above that level if a "level_n" count
  if(grepl("level", location)){
    lev <- str_split_fixed(location, "_", 2)[, 2] %>% as.numeric()
    counts <- df[!is.na(get(location)) & !is.na(get(dt)) & get(lvl) <= max(lev, 3), c(location, year, age, sex), with = F] %>% unique
  } else{
    counts <- df[!is.na(get(location)) & !is.na(get(dt)), c(location, year, age, sex), with = F] %>% unique
  }

  #save an original copy with all location/year/age/sex combos for later
  cj <- copy(df[!is.na(get(location)), c(location, sex), with = F]) %>% unique

  #get # of data points by country and strat_group
  counts <- counts[, .N, by = c(location, age, sex)]
  col <- paste0(location, "_count")

  #keep max for each country/strat - only do if an age or sex specific model
  counts <- counts[, paste0(col):=max(N), by = c(location, sex)]

  #grab missing locs (ie had zero data)
  counts <- counts[, c(location, sex, col), with = F] %>% unique
  counts <- merge(counts, cj, by = c(location, sex), all = T)
  #make sure missing locations aren't left out
  counts[, (col):=ifelse(is.na(get(col)), 0, get(col))]



  df <- merge(df, counts, by = c(location, sex), all.x = T)

  return(df)
}

calculate.mad <- function(location_id, stage1, prediction, level, amp_factor) {
  ## Setup
  resid <- stage1 - prediction
  tst <- data.table(location_id, resid, level)
  tst[, key:=seq(.N)]
  ## Merge location hierarchy
  hierarchy <- get_location_hierarchy(location_set_id, gbd_round_id, decomp_step)
  hierarchy <- hierarchy[, grep("location_id|level_", names(hierarchy)), with=F]
  tst <- merge(tst, hierarchy, by="location_id", all.x=T)
  ## Calculate MAD by level specified
  tst[, mad := median(abs(resid - median(resid, na.rm=T)), na.rm=T), by = eval(paste0("level_", level))]
  #multiply amplitude by a user-specified factor (usually if model UIs are too low due to data scarcity)
  if(!is.na(amp_factor)){
    tst[, mad:=mad*amp_factor]
  }
  setkeyv(tst, 'key')
  ## Return MAD at the specified level
  return(tst$mad)
}

calculate.amplitude <- function(df, method = 'prod', cutoff = NA , broken_stick = F, constrained = T){

  if(method == 'global_above_cutoff' | method == 'broken_stick'){

    #Calculate global MAD for countries above a given cutoff
    print(paste0('Calculating global amplitude at cutoff ', cutoff))
    gamp <- copy(df[level_3_count >= cutoff])
    gamp[, lvl_3_mad:=calculate.mad(location_id, stage1, st, 3, amp_factor), by = 'sex_id']
    global_amp_by_sex <- gamp[, mean(unique(lvl_3_mad)), by = 'sex_id']
    setnames(global_amp_by_sex, 'V1','mean_amp')
    #print(global_amp_by_sex)

    if(method == 'broken_stick'){

      print('Running broken stick regression')
      #capture input cols
      incols <- names(copy(df))

      #Calculate
      print("Calculating level-3 amplitude for countries below cutoff.")
      df[, amp:=calculate.mad(location_id, stage1, st, 3, amp_factor), by = 'sex_id']
      df <- merge(df, global_amp_by_sex, by = 'sex_id', all.x = T)

      #adjust x-values (data density) for level-3s below data cutoff x (ie 19 cy --> adj_lvl3_count = 1)
      df[, adj_lvl3_count:= cutoff - level_3_count]
      #adjust y-values (MAD of level-3) by subtracting off the mean of level-3 amplitudes above cutoff
      df[, adj_amp:=amp - mean_amp]

      #create separate df for running regression on values below cutoff after adjustment
      tst <- df[level_3_count < cutoff, c('level_3', 'sex_id','level_3_count', 'amp', 'adj_lvl3_count', 'adj_amp', 'mean_amp'), with = F] %>% unique

      #regress adjusted amplitude on adjusted data density
      print("Running broken stick regression.")
      formula <- as.formula(adj_amp ~ -1 + adj_lvl3_count)
      for(sex in unique(tst$sex_id)){
        #run simple logistic regression
        mod <- lm(formula, data = tst[sex_id == sex])
        tst[sex_id==sex, slope:=coef(mod)]
      }

      #get all values for the dataset and merge in regression values for things below cutoff
      tst <- tst[, .(sex_id, slope)] %>% unique
      df <- merge(df, tst[, .(sex_id, slope)], all.x = T, by = 'sex_id')

      #adjustments to slope based on possible missingness, constrained or unconstrained, and above-cutoff points
      df[, pre_adj_slope:=slope]
      df[level_3_count >= cutoff, slope:=0]
      df[is.na(slope), slope:=0] #make sure regions/SRs/etc (vars) with no observations aren't dropped, just defaulting to global amplitude
      if(constrained){
        df[slope < 0, slope:=0]
      }

      # calculate acual values of mad based on regression for each country-year!!
      print('Calculating amplitude for each data density below cutoff.')
      df[, out_mad:= slope*(cutoff - level_3_count) + mean_amp]

      #subset df back to original cols plus out_mad
      df <- df[, c(incols, 'out_mad'), with = F]

      setnames(df, 'out_mad', 'st_amp')
      return(df)

    }else{
      df <- merge(df, global_amp_by_sex, all.x = T, by = 'sex_id')
      setnames(df, 'mean_amp', 'st_amp')

      return(df)
    }
  } else{
    #Calculate MAD by data density
    df[, st_amp:= calculate.density.mad(location_id, stage1, st, amp_unit, amp_factor), by="sex_id"]

  }

}

plot_amp <- function(run_id, amp_method = 'global_above_cutoff', amp_cutoff = NA) {

  ids <- c('location_id', 'year_id', 'age_group_id', 'sex_id')

  #get amplitude and data inputs
  dt <- model_load(run_id, 'prepped')
  st <- model_load(run_id, 'st')
  stage1 <- model_load(run_id, 'stage1')
  params <- fread(sprintf('%s/%i/parameters.csv', stgpr_path('cluster_model_output'), run_id))

  #get location hierarchy and amp_cutoff
  amp_method <- ifelse(is.na(amp_method), params$gpr_amp_method, amp_method)
  amp_cutoff <- ifelse(is.na(amp_cutoff), as.integer(params$gpr_amp_cutoff), amp_cutoff)
  amp_cutoff <- ifelse(is.null(amp_cutoff), NA, amp_cutoff)
  assign('gpr_amp_factor', 1, envir = globalenv())
  assign('modelable_entity_id', params$modelable_entity_id, envir = globalenv())
  assign('location_set_id', params$location_set_id, envir = globalenv())
  assign('gbd_round_id', params$gbd_round_id, envir = globalenv())
  assign('decomp_step', params$decomp_step, envir = globalenv())

  #merge together
  tst <- merge(dt, st, by = ids)
  tst <- merge(tst, stage1, by = ids)

  #get locations and merge on level_3
  locs <- get_location_hierarchy(location_set_id, gbd_round_id, decomp_step)
  tst <- merge(tst, locs[, grepl("location_id|super_region_name", names(locs)), with = F], by = 'location_id')
  lvlcols <- grep("level_", names(tst), value = T)
  for(i in c("location_id", lvlcols)){tst <- count.country.years(tst, i,"year_id", "age_group_id","sex_id", "data", "level")}

  #Calculate amp_cutoff as the level_3_count above the 80th percentile if not user-specified
  if(is.na(amp_cutoff)){
    quants <- unique(tst[, .(level_3, sex_id, level_3_count)])
    amp_cutoff <- floor(quants[, level_3_count] %>% quantile(., .8) %>% unname)
  }

  #get level_3 counts for x-axis
  tst[, level_3_amp:=calculate.mad(location_id, stage1, st, 3, amp_factor = 1), by = 'sex_id']
  tst <- calculate.amplitude(tst, method = amp_method, cutoff = amp_cutoff)

  tst <- unique(tst, by = c('level_3', 'sex_id'))

  #try transforming to level space before plotting
  cols <- c('st_amp', 'level_3_amp')
  #but waaait first apply that normalization scalar as is done during gpr
  tst[, (cols):=lapply(.SD, function(x) x * 1.4826), .SDcols = cols]
  #tst[, (cols):=lapply(.SD, function(x) transform_data(x, data_transform, reverse = T)), .SDcols = cols]

  #create printout for amplitude by sex
  tst[,sex:=factor(sex_id, levels = c(1,2,3), labels = c('Male', 'Female', "Both-Sex"))]
  print_amp <- sprintf('Cutoff %i | Global Amplitude', amp_cutoff)
  for(sx in unique(tst$sex)){
    if(amp_method == 'global_above_cutoff'){
      print_amp <- sprintf("%s %.3f (%s) ", print_amp, unique(tst[sex == sx & !is.na(st_amp), st_amp]), sx)
    }else{
      print_amp <- sprintf("%s %.3f (%s) ", print_amp, unique(tst[sex == sx & !is.na(st_amp) & level_3_count >= amp_cutoff , st_amp]), sx)
    }
  }

  final_plt <- ggplot(unique(tst[, c('super_region_name','sex_id', 'level_3_count','level_3_amp','st_amp'), with = F])) +
    geom_line(aes(x = level_3_count, y = st_amp)) +
    geom_point(aes(x = level_3_count, y = level_3_amp, col = factor(super_region_name))) +
    facet_wrap(~sex_id) +
    geom_vline(xintercept = amp_cutoff, col = 'red', linetype = 'dashed') +
    labs(title = 'All country-level MADs (points) with final amplitude outputs(line)',
         subtitle = print_amp) +
    theme(legend.position = 'bottom') +
    labs(color = 'Super-region')

  return(final_plt)
}

amplitude_by_cutoff <- function(run_id, outpath = NA) {

  ids <- c('location_id', 'year_id', 'age_group_id', 'sex_id')

  #get amplitude and data inputs
  dt <- model_load(run_id, 'prepped')
  st <- model_load(run_id, 'st')
  stage1 <- model_load(run_id, 'stage1')
  params <- fread(sprintf('%s/%i/parameters.csv', stgpr_path('cluster_model_output'), run_id))

  #get location hierarchy and amp_cutoff
  assign('location_set_id', params$location_set_id, envir = globalenv())
  assign('gbd_round_id', params$gbd_round_id, envir = globalenv())
  assign('decomp_step', params$decomp_step, envir = globalenv())
  data_transform <- params$data_transform
  assign('gpr_amp_factor', 1, envir = globalenv())
  assign('modelable_entity_id', params$modelable_entity_id, envir = globalenv())

  #merge together
  tst <- merge(dt, st, by = ids)
  tst <- merge(tst, stage1, by = ids)
  tst[,sex:=factor(sex_id, levels = c(1,2,3), labels = c('Male', 'Female', "Both-Sex"))]

  #get locations and merge on level_3
  locs <- get_location_hierarchy(location_set_id, gbd_round_id, decomp_step)
  tst <- merge(tst, locs[, grepl("location_id|super_region_name", names(locs)), with = F], by = 'location_id')
  lvlcols <- grep("level_", names(tst), value = T)
  for(i in c("location_id", lvlcols)){tst <- count.country.years(tst, i,"year_id", "age_group_id","sex_id", "data", "level")}

  #assign cutoffs if NA, else verify that none of them are impossible
  cutoffs <- seq(0, max(tst$level_3_count))

  #Plot all possible amplitude cutoffs until they level out (ie only one location's residuals are still in there)
  print(sprintf('Calculating global amplitude for cutoffs %i-%i', min(cutoffs), max(cutoffs)))
  amps <- data.table(sex = unique(tst$sex))
  for(x in cutoffs){

    #Calculate MAD by data density
    df <- calculate.amplitude(df = tst, method = 'global_above_cutoff', cutoff = x)

    #subset and format for merge
    df <- df[, c('sex', 'st_amp'), with = F] %>% unique
    setnames(df, 'st_amp', paste0('amp_', x))

    #merge
    amps <- merge(amps, df, by = 'sex', all.x = T)

  }

  #reshape and format tst for output
  ampcols <- paste0('amp_', cutoffs)
  out <- melt(amps, id.vars = c('sex'), measure = patterns('amp_'), variable.name = 'cutoff', value.name = 'amplitude')
  out[, cutoff:=as.numeric(str_split_fixed(cutoff, '_', 2)[,2])]
  final <- out[, .(sex, cutoff, amplitude)] %>% unique

  #multiply by pinche normalization scalar
  final[, amplitude:=amplitude * 1.4826]

  plt <-  ggplot(final) + geom_line(aes(x = cutoff,  y = amplitude)) +
    #geom_point(aes(x = level_3_count, y = level_3_amp , col = super_region_name)) +
    facet_wrap(~sex) +
    ggtitle(sprintf('%s (run_id %i)\n Global amplitude by sex for each possible cutoff',modelable_entity_id, run_id))

  if(!is.na(outpath)){
    pdf(outpath)
    print(plt)
    dev.off()
  }

  print(plt)


  return(final)
}
