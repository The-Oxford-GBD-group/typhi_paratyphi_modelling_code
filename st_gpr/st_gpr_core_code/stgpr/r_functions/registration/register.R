library(stringr)

root <- getwd()
PYTHON_PATH <- '/ihme/code/st_gpr/miniconda3/envs/stgpr/bin/python'
REGISTER_PATH <- file.path(root, 'r_functions/registration/register_stgpr_model_cli.py')

register_stgpr_model <- function(path_to_config, model_index_id = NULL){
###################################################################################################
#   Create an ST-GPR model version. Populates run root with needed inputs
# 	for model flow given the settings related in the config. If a decomp step
#   is specified, model settings and data are pulled from past results, so
#   you don't need to provide a path_to_config or model_index_id (and if you do, you'll be
#   told to stop.
#   
#   Args:
#   
#   * path_to_config (str) : A path (available on the cluster) to an ST-GPR model config,
#                           with all entries filled out in ways that will pass validations.
#                           See example configs on the HUB if your config fails validations!
#   
#   * model_index_id (int) : In the config, there should be a column called model_index_id with unique
#                         integer values for each entry in the config. Specify model_index_id to
#                         indicate which row of the config you want to upload.
#
###################################################################################################
    # Build system call.
    args <- c(PYTHON_PATH, REGISTER_PATH)
    if (!is.null(path_to_config)) {
        args <- append(args, c(path_to_config))
    }
    if (!is.null(model_index_id)) {
        args <- append(args, c('--model_index_id', model_index_id))
    }
    call <- paste(args, collapse = ' ')

    # Run registration and print output.
    result <- system(call, intern = TRUE)
    cat(result, sep = '\n')

    # Check for run ID in last line of stdout.
    # Return it if present. Otherwise return NA.
    run_id <- NA
    for (line in result) {
        match = str_match(line, 'Created ST-GPR version with ID (\\d+)')
        if (any(!is.na(match))) {
            run_id <- as.integer(match[2])
        }
    }
    return(run_id)
}
