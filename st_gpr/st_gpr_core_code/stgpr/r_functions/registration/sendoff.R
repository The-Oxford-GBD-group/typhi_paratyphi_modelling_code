library(stringr)

root <- getwd()
PYTHON_PATH <- '/ihme/code/st_gpr/miniconda3/envs/stgpr/bin/python'
SENDOFF_PATH <- file.path(root, 'r_functions/registration/st_gpr_sendoff_cli.py')

stgpr_sendoff <- function(
    run_id,
    project,
    log_path = NULL,
    nparallel = NULL
){
###################################################################################################
# Function to submit ST-GPR models to the cluster! This is it!
# This is where it all begins!
# 
# Args:
#
# * run_id (int) : A legit ST-GPR run_id
#   
# * project (str) : The cluster project to run jobs from (ex. proj_custom_models). If
#                           you don't know what this is, ask your PO.
# 
# * nparallel (int) : Number of parallelizations to split your data over (by location_id).
#                     TLDR; set to 50 for small datasets, 100 for large datasets
# 
#                     More parallelizations means each job is running fewer locations,
#                     which makes each job faster. But if the cluster is swamped,
#                     it takes more time for each of these jobs to get space,
#                     so that makes it slower...I usually just set to 50 for
#                     smaller datasets (ie all-age, both-sex models)
#                     or 100 for larger datasets (by-sex, by-age models)
# 
#                     Default: 50
# 
# * log_path (str) : Path to a directory for saving logfiles.
#                 Strongly recommended: '/share/temp/sgeoutput/<USERNAME>/errors' (with a warning
#                                         these delete automatically after 12 hours, so if you're
#                                         running overnight, maybe elsewhere on /share/scratch/)
#                                         DO NOT:  put these on h/ or j/. Super bad practice
# 
# 
# It's time to toss the dice!
###################################################################################################
    # Build system call.
    args <- c(PYTHON_PATH, SENDOFF_PATH)
    if (!is.null(log_path)) {
        args <- append(args, c('--log_path', log_path))
    }
    if (!is.null(nparallel)) {
        args <- append(args, c('--nparallel', nparallel))
    }
    args <- append(args, c(run_id, project))
    call <- paste(args, collapse = ' ')

    # Run sendoff and print output.
    result <- system(call, intern = TRUE)
    cat(result, sep = '\n')
}
