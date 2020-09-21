########################################################################################################
# Register and launch an ST-GPR model from R
#
# Description: Use python fire to send arguments
#              for ST-GPR registration and launch
#              functions to python functions
#
# Author: Hayley Tymeson
# Date: January 14, 2019
########################################################################################################
rm(list = ls())

#~~~~~~~~~~~~~~~~~~#
# run the STGPR ####
#~~~~~~~~~~~~~~~~~~#

central_root <- '/ihme/code/geospatial/annieb6/st_gpr/stgpr'
setwd(central_root)

source('r_functions/registration/register.R')
source('r_functions/registration/sendoff.R')

# Arguments
#me_name <- 'mdr_typhi'
path_to_config <- '/ihme/code/geospatial/annieb6/lbd_amr/typhi_paratyphi/st_gpr/STGPR_typhi_config3.csv'
model_index_id<- 112

project <- 'proj_geospatial'

# Registration
run_id <- register_stgpr_model(
  path_to_config = path_to_config,
  model_index_id = model_index_id)

# Sendoff
stgpr_sendoff(run_id,
               project,
               log_path = '/ihme/code/geospatial/annieb6/lbd_amr/typhi_paratyphi/st_gpr/errors')


#add runID to the config
config  <- read.csv(path_to_config, stringsAsFactors = F)
config$run_id[config$model_index_id == model_index_id] <- run_id
write.csv(config, path_to_config, row.names = F, na = "")

