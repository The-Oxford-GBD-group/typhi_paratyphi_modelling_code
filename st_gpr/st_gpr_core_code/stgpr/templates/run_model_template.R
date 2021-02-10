central_root <- '/ihme/code/st_gpr/central/stgpr'
setwd(central_root)
source('r_functions/registration/register.R')
source('r_functions/registration/sendoff.R')


# Register model
path_to_config <- '/path/to/my/config.csv'
model_index_id <- NULL
run_id <- register_stgpr_model(path_to_config, model_index_id)

# Submit model
project <- 'proj_custom_models'
stgpr_sendoff(run_id, project)
