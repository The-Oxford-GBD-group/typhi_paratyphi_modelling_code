###############################################################################
###############################################################################
## Basic Launch for antibiotic use
##
###############################################################################
###############################################################################

# Qsub:
# qsub -e /share/homes/annieb6/logs/errors/ -o /share/homes/annieb6/logs/output/ -P proj_geospatial -N job_launch_antibiotics -q all.q -cwd -l archive=TRUE -l m_mem_free=15G -l fthread=2 -l h_rt=02:00:00:00  -v sing_image=default -p 0  /share/code/geospatial/annieb6/lbd_core/mbg_central/share_scripts/shell_sing.sh /share/code/geospatial/annieb6/lbd_amr/antibiotics/2_modelling/launch.R diarrhea_antibiotics fin

###############################################################################
## SETUP
###############################################################################

## clear environment
rm(list=ls())

## Set core_repo location and indicator group
user              <- Sys.info()['user']
user_repo         <- paste0('/share/code/geospatial/',user,'/lbd_amr/typhi_paratyphi/mbg/')
core_repo         <- '/share/code/geospatial/annieb6/lbd_core/'
indicator_group   <- 'lbd_amr'
#indicator         <- as.character(commandArgs()[4])
indicator         <- 'mdr_typhi'
nid_holdouts      <- TRUE

## sort some directory stuff and pull newest code into share
setwd(core_repo)
sharedir       <- paste('/share/geospatial/mbg', indicator_group, indicator, sep = '/')

source(paste0(core_repo, '/mbg_central/setup.R'))
package_list <- readLines(paste0(core_repo, "/mbg_central/share_scripts/common_inputs/package_list.csv"))
load_R_packages(package_list)
mbg_setup(package_list = package_list, repos = c(core_repo))

create_dirs(indicator_group = indicator_group, indicator = indicator)

## Create run date in correct format
run_date <- make_time_stamp(TRUE)

## Read config file and save all parameters in memory
config <- set_up_config(repo          = core_repo,
                        config_file   = paste0(user_repo, '2_modelling/mdr_typhi/config_', indicator, '.csv'),
                        covs_file     = paste0(user_repo, '2_modelling/mdr_typhi/covlist.csv'))
                      
## Create a few objects from the config file loaded above
if (class(Regions) == "character" & length(Regions) == 1) Regions <- eval(parse(text=Regions))
if (class(year_list) == "character") year_list <- eval(parse(text=year_list))
if (length(summstats) == 1 & grepl(",", summstats)) summstats <- eval(parse(text=summstats))

## Load gaul list
gaul_list <- get_adm0_codes(Regions,
                            shapefile_version = modeling_shapefile_version)

## If running individual countries, get set up
if (as.logical(individual_countries) == TRUE) {
  # Convert all Regions to individual countries
  Regions <- get_individual_countries(gaul_list)
  
  # Turn off all FEs
  use_child_country_fes <- FALSE
  use_inla_country_fes  <- FALSE
  use_inla_country_res  <- FALSE
}

#include the antibiotic covariates
abx_covs <- c('ddd_per_1000')
###############################################################################
## Make Holdouts
###############################################################################
# load the full input data
df <- load_input_data(indicator   = indicator,
                      removeyemen = FALSE,
                      years       = yearload,
                      yl          = year_list,
                      withtag     = as.logical(withtag),
                      datatag     = datatag,
                      use_share   = as.logical(use_share))

# add in location information
df <- merge_with_ihme_loc(df,
                          shapefile_version = modeling_shapefile_version)

if(as.logical(makeholdouts)){
  # make a list of dfs for each region, with 5 qt folds identified in each
  stratum_ho <- make_folds(data       = df,
                           n_folds    = as.numeric(n_ho_folds),
                           spat_strat = 'rand',
                           temp_strat = 'rand',
                           strat_cols = 'region',
                           ts         = as.numeric(ho_ts),
                           mb         = as.numeric(ho_mb))
} else {

  df$t_fold <-  0
  df$fold <- 0
  df$ho_id <- 0
  saveRDS(df, sprintf('/share/geospatial/mbg/%s/%s/output/%s/stratum.rds', indicator_group, indicator, run_date))

}
rm(df)

## Create holdouts based on NID
if(nid_holdouts == TRUE){
  for(i in 1:length(stratum_ho)){
    nid <- unique(stratum_ho[[i]]$nid)
    nid <- sample(nid)
    nid <- data.table(nid)
    nid$fold <- cut(seq(1,nrow(nid)),breaks=5,labels=FALSE)
    stratum_ho[[i]]$fold <- NULL
    stratum_ho[[i]]$ho_id <- NULL
    stratum_ho[[i]] <- merge(stratum_ho[[i]], nid)
    stratum_ho[[i]]$ho_id <- stratum_ho[[i]]$fold
  }
  saveRDS(stratum_ho, sprintf('/share/geospatial/mbg/%s/%s/output/%s/stratum.rds', indicator_group, indicator, run_date))
}

###############################################################################
## Launch Parallel Script
###############################################################################

## Make loopvars aka strata grid (format = regions, ages, holdouts)
if(as.logical(makeholdouts)) loopvars <- expand.grid(Regions, 0, 0:n_ho_folds) else loopvars <- expand.grid(Regions, 0, 0)

## loop over them, save images and submit qsubs
for(i in 1:nrow(loopvars)){

  message(paste(loopvars[i,2],as.character(loopvars[i,1]),loopvars[i,3]))

  # make a qsub string
  qsub <- make_qsub_share(age           = loopvars[i,2],
                          reg           = as.character(loopvars[i,1]),
                          holdout       = loopvars[i,3],
                          test          = as.logical(test),
                          indic         = indicator,
                          ig            = indicator_group,
                          saveimage     = TRUE,
                          addl_job_name = indicator, ## from config eval(parse(text = jn))
                          memory        = 150,
                          cores         = 10,
                          geo_nodes     = as.logical(use_geos_nodes),
                          proj          = ifelse(as.logical(use_geos_nodes) == TRUE, 'proj_geo_nodes', 'proj_geospatial'),
                          singularity   = 'default',
                          run_time      = '24:00:00',
                          queue         = ifelse(as.logical(use_geos_nodes) == TRUE, 'geospatial.q', 'all.q')
  )

  system(qsub)
}


## check to make sure models are done before continuing
waitformodelstofinish(lv = cbind(as.character(loopvars[,1]),loopvars[,3]),sleeptime=300)

###############################################################################
## Post-Estimation
###############################################################################

## Save strata for Shiny to use in producing aggregated fit statistics
strata <- unique(as.character(loopvars[,1]))
dir.create(paste0(sharedir, '/fit_stats'), showWarnings = F)
save(strata, file = paste0(sharedir, '/fit_stats/strata.RData'))

gbd = NULL

# Prepare for parallel post-estimation - save file with objs to re-load in child processes
prep_postest(indicator = indicator,
             indicator_group = indicator_group,
             run_date = run_date,
             save_objs = c("core_repo", "year_list", "summstats", "pop_measure", "gbd"))

## Parallelized post-estimation over region
postest_script <- "postest_script"

for (s in strata) {
  qsub <- make_qsub_postest(code                        = postest_script,
                            stratum                     = s,
                            log_location                = 'sharedir',
                            memory                      = 100,
                            singularity                 = "default",
                            subnat_raking               = subnational_raking,
                            modeling_shapefile_version  = modeling_shapefile_version,
                            raking_shapefile_version    = raking_shapefile_version,
                            geo_nodes                   = as.logical(use_geos_nodes),
                            proj                        = ifelse(as.logical(use_geos_nodes) == TRUE, 'proj_geo_nodes', 'proj_geospatial'),                            cores                       = 10,
                            run_time                    = '6:00:00',
                            queue                       = ifelse(as.logical(use_geos_nodes) == TRUE, 'geospatial.q', 'all.q'))
  system(qsub)
}

## check to make sure post-est done before continuing
waitformodelstofinish(lv = cbind(strata, 0), sleeptime=300)

## Combine post est stuff across regions and save needed outputs
post_load_combine_save(indic = indicator,
                       ig = indicator_group,
                       summstats = summstats,
                       raked = 'unraked',
                       rf_table   = FALSE,
                       run_summ   = FALSE)

# Clean up / delete unnecessary files
clean_after_postest(indicator             = indicator,
                    indicator_group       = indicator_group,
                    run_date              = run_date,
                    strata                = strata,
                    delete_region_rasters = F)

###############################################################################
## Aggregate to admin2, admin1, and national levels
###############################################################################

submit_aggregation_script(indicator                  = indicator,
                          indicator_group            = indicator_group,
                          run_date                   = run_date,
                          raked                      = c(FALSE),
                          pop_measure                = pop_measure,
                          overwrite                  = T,
                          ages                       = 0, # Note: can take vector of ages
                          holdouts                   = 0,
                          regions                    =  Regions,
                          corerepo                   = core_repo,
                          log_dir                    = paste0(sharedir, "/output/", run_date, "/"),
                          geo_nodes                  = as.logical(use_geos_nodes),
                          singularity                = 'default',  
                          proj                       = ifelse(as.logical(use_geos_nodes) == TRUE, 'proj_geo_nodes', 'proj_geospatial'),     
                          slots                      = 8,
                          modeling_shapefile_version = modeling_shapefile_version,
                          raking_shapefile_version   = raking_shapefile_version,
                          memory                     = 100,
                          run_time                   = '08:00:00',
                          queue                      = ifelse(as.logical(use_geos_nodes) == TRUE, 'geospatial.q', 'all.q'))

waitforaggregation(rd = run_date, indic = indicator, ig = indicator_group,
                   ages     = 0,
                   regions  = strata,
                   holdouts = 0,
                   raked    = F)

combine_aggregation(rd = run_date, 
                    indic = indicator, 
                    ig = indicator_group,
                    ages     = 0,
                    regions  = Regions,
                    holdouts = 0,
                    raked    = F,
                    delete_region_files = F)


summarize_admins(ind = indicator,
                 ig = indicator_group,
                 summstats = summstats,
                 raked = F,
                 ad_levels = c(0,1,2),
                 file_addin = NULL)

# Combine csv files
csvs <- list.files(paste0(sharedir, '/output/', run_date, '/'),
                   pattern = "input_data(.*).csv",
                   full.names = T)

csv_master <- lapply(csvs, fread) %>%
  rbindlist(fill = TRUE) %>%
  subset(., select = names(.) != "V1")
write.csv(csv_master, file=paste0(sharedir, '/output/', run_date, '/input_data.csv'))

#~~~~~~~~~~~~~~~~~~~~~~~~~#
# model validation script #
#~~~~~~~~~~~~~~~~~~~~~~~~~#
qsub <- paste0('qsub -e /ihme/geospatial/mbg/lbd_amr/mdr_typhi/output/', run_date, '/errors ',
               '-o //ihme/geospatial/mbg/lbd_amr/mdr_typhi/output/', run_date, '/output ',
               '-P proj_geo_nodes -N mdr_checks ',
               '-q geospatial.q -cwd -l archive=TRUE ',
               '-l m_mem_free=30G -l fthread=6 -l h_rt=00:12:00:00 ',
               '-v sing_image=default -p 0  ',
               '/share/code/geospatial/annieb6/lbd_core//mbg_central/share_scripts/shell_sing.sh ',
               '/share/code/geospatial/annieb6/lbd_amr/typhi_paratyphi/mbg/2_modelling/mdr_typhi/post_model_checks.R ', run_date, ' fin')
system(qsub)

#~~~~~~~~~~~~~#
# END OF FILE #
#~~~~~~~~~~~~~#