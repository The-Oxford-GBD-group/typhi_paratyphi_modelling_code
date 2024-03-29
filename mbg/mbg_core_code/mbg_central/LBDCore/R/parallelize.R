#' @title Parallelize
#' @description parallelize() is a versatile function to take an R script and run it in
#' parallel on the cluster combination of an arbitrary number of variables.
#'
#' This function is meant to replace the many qsub functions that are
#' floating around and provide a single new function that can be used in
#' almost all circumstances, with several additional features:
#'
#' By pairing this function with a \code{load_from_parallelize()} call in the
#' child script itself, objects are loaded into the child script's
#' environment without having to ensure that a series of \code{commandArgs()}
#' are in the appropriate order
#'
#' \code{parallelize()} returns a list of job_ids and loop variables, along
#' with the original qsub call.  This object - when paired with \code{monitor_jobs()},
#' which is a replacement for \code{waitformodelstofinish()} - allows closer tracking
#' of the jobs with respect to their status on the cluster, and eliminates
#' the need to write clunky empty files like \code{fin_[whatever]} to mark that
#' jobs are done.  Finally, the \code{monitor_jobs()} function can automatically
#' resubmit jobs and notify the user (in progress; Pushover notifications only
#' currently supported) when jobs fail.
#'
#' @param user current user [default = user name]
#' @param slots number of slots to request for each job [required]
#' @param memory memory to request (in GB) [required]
#' @param script R script to be run in parallel. Assumes that script name
#' ends with '.R'. This script should have a
#' \code{load_from_parallelize()} call towards the top. [required]
#'
#' @param geo_nodes If TRUE, your job will be submitted to the geos (LBD)
#' cluster, if FALSE, it will be submitted to the prod cluster. Note that if
#' using the 'proj' argument, make sure to use project name which is valid on
#' the cluster you are submitting to. [default = FALSE]
#'
#' @param use_c2_nodes If TRUE, your job will be submitted to the C2 nodes on
#' the prod cluster, if FALSE, the C2 nodes are not specified. Note that if
#' FALSE, your job may land on a node with much less memory or your node may
#' still land on a C2 node anyway. If both the 'use_c2_nodes' and 'geo_nodes'
#' arguments are set to TRUE, then the code will issue a warning and default
#' to the geos nodes. [default = FALSE]
#'
#' @param proj Can pass in a project name to submit your job under. If default
#' and the 'geo_nodes' argument is left as its default of 'FALSE', jobs
#' will be submitted to the prod cluster under the default project
#' 'proj_geospatial'. If default and with 'geos_nodes = TRUE', jobs will be
#' submitted to the geos (LBD) nodes under the default project
#' 'proj_geo_nodes'. If a project name is passed in for 'proj' the job will
#' be submitted under that project. Note that this function does not check for
#' valid project names since these are likely to change often and likely
#' valid project names are different on each cluster. [default = NULL]
#'
#' @param ig indicator group [default = indicator_group]
#' @param indic indicator [default = indicator]
#' @param rd run date [default = run_date]
#' @param expand_vars a named list of objects to \code{grid.expand()} over.  One job
#' will be submitted for each named item in the list, using
#' the name of the item as the variable name.  For instance:
#' \code{expand_vars = list(region = c("cssa", "essa", "sssa"))}
#' would submit one job for each region with \code{region = "cssa"}
#' in the first job, \code{region = "essa"} in the second job, and
#' so forth. If a second item were added to that list, then
#' all combinations will be submitted:
#' \code{expand_vars = list(region = c("cssa", "essa", "sssa"),
#' raked = c(T,F))}
#' would submit cssa-raked, cssa-unraked, essa-raked,
#' essa-unraked, etc...
#' Only \code{expand_vars} or \code{lv_table} can be given, but not both.
#' [default = NULL]
#' @param save_objs character vector of objects that should be available to all
#' child scripts. Unlike \code{expand_vars}, these will be the *same*
#' throughout all of the child scripts - they are saved to a
#' temporary file and loaded by each child script. For instance,
#' \code{save_objs = c("run_date", "indicator_group")} would load the
#' \code{run_date} and \code{indicator_group} objects into the environment
#' of each child script. [default = NULL]
#' @param lv_table pass in the loop vars table? Pass in this table if you want to
#' supply something more fine-tuned than what can by given by
#' \code{grid.expand()} with `expand_vars`. Only \code{expand_vars} or
#' \code{lv_table`} can be given, but not both. [default = NULL]
#' @param script_dir directory to look in for the R script that is to be run in
#' parallel. If this is NULL, then script will look in
#' 'corerepo/mbg_central/share_scripts' (see \code{corerepo} below)
#' for the script given in \code{script}. [default = NULL]
#' @param prefix prefix to be appended to all jobs.  The jobs will have the naming
#' convention of prefix_[first_expand_var]_[second_expand_var]_[etc]
#' [default = 'job']
#' @param log_location where to save the logs? [default = 'sgeoutput']
#' @param corerepo location of the lbd_core repo [default = core_repo]
#' @param geos_nodes run on the geos nodes or not? Defaults to running on prod
#' with the 'proj_geospatial' project (see the 'proj' arg).
#' [default = FALSE]
#' @param runtime Run-time for usage in the fair cluster (unused with prod)
#' @param priority Job priority that can be deprioritized if needed, and can only be used for values in [-1023,0]. Default = 0.
#' This value will get bounded to 0 or -1023 if the user supplies a value outside those bounds.
#' @param threads numeric number of threads to request on fair cluster (unused with prod)
#' @param singularity Launch R from a Singularity image. The default is
#   'default' indicating that you wish to launch a Singularity container from
#' the default image. You may also provide a string which can be either a complete
#' path to a Singularity image that is not located at the default image
#' location, or just the name of the Singularity image that is assumed located
#' at the default image location. NULL is also accepted, which will launch R
#' using the default R installation on the geos or prod nodes, but this is
#' no longer recommended and will likely be deprecated at some point in the
#' future.
#'
#' If 'default' is chosen, the default image is defined in the shell script
#' executed by this R script ('shell_sing.sh') so that no R code need be
#' updated when the default image is updated. Different versions of a
#' Singularity image or test versions may be specified by providing the name
#' or path of the image. Currently, all standard images for LBD are kept at
#' the default location of /share/singularity-images/lbd.
#' [default = 'default']
#' @param singularity_opts pass in a named list of environmental variables.
#' \code{qsub_sing_envs} will check that the names of the list members passed
#' in match the environmental variables that the shell_sing.sh script knows
#' about: 'SET_OMP_THREADS' and/or 'SET_MKL_THREADS'. Passing in other
#' environmental names in the list will result in an error. If this is left
#' as 'NULL' and a Singularity image is used, SET_OMP_THREADS and
#' SET_MKL_THREADS will remain unset and the shell_sing.sh script will use
#' the default setting of SET_OMP_THREADS=1 and SET_MKL_THREADS={max_threads}
#' (see shell_sing.sh comments). For example SET_OMP_THREADS=1 and
#' SET_MKL_THREADS=4 can be achieved by passing in
#' \code{envs = list(SET_OMP_THREADS=1, SET_MKL_THREADS=4)}
#' [default = NULL]
#'
#' @return a list containing:
#' -\code{lv}: data table of loop variables including qsub commands
#' -\code{fname}: filename of the temporary file containing `save_objs`
#'
#' @examples
#' \dontrun{
#'
#' # In master script:
#'
#' if (as.logical(makeholdouts) == T) holdout_vector <- 0:as.numeric(n_ho_folds)
#' if (as.logical(makeholdouts) == F) holdout_vector <- 0
#'
#' combine_lv <- list(
#'   region = Regions,
#'   holdout = holdout_vector,
#'   doses = 3
#' )
#'
#' combination_output <- parallelize(
#'   script = "indicator_specific_script",
#'   script_dir = "/path/to/indicator_specific_repo",
#'   expand_vars = combine_lv,
#'   save_objs = c(
#'     "indicator_group", "run_date",
#'     "vaccine"
#'   ),
#'   prefix = "combine",
#'   cores = 20,
#'   memory = 100,
#'   geo_nodes = TRUE
#' )
#'
#' monitor_jobs(combination_output)
#' }
#'
#' @seealso \code{\link{load_from_parallelize}}: Child scripts launched
#' by parallelize should call this function near the top.
#' \code{\link{get_singularity}}: Determines which Singularity to use.
#' \code{get_singularity}: Which Singularity image to use
#' \code{qsub_sing_envs}: Adds environmental variables to a qsub string
#'
#' @export
parallelize <- function(user = Sys.info()["user"],
                        slots,
                        memory,
                        script,
                        proj = NULL,
                        ig = indicator_group,
                        indic = indicator,
                        rd = run_date,
                        expand_vars = NULL,
                        save_objs = NULL,
                        lv_table = NULL,
                        script_dir = NULL,
                        prefix = "job",
                        log_location = "sgeoutput",
                        corerepo = core_repo,
                        geo_nodes = FALSE,
                        use_c2_nodes = FALSE,
                        queue = NULL,
                        run_time = NULL,
                        priority = 0,
                        threads = 2,
                        singularity = singularity_version,
                        singularity_opts = NULL) {

  # Setup ---------------------------------------------------------------
  # allocate cores based on threads argument if on the new cluster. Otherwise respect historical "slot" usage
  cores <- ifelse(is_new_cluster(), threads, slots)
  # Attempt to locate the R script we intend to run and verify that it
  # actually exists
  # We assume that the script ends with '.R'. If the user supplied
  # the file name with a '.R' appended make sure not to append one again
  script <- ifelse(stringr::str_sub(script, -2) == ".R", script, paste0(script, ".R"))
  if (is.null(script_dir)) script_dir <- paste0(corerepo, "/mbg_central/share_scripts")
  script_file <- paste0(script_dir, "/", script)
  if (!file.exists(script_file)) {
    stop(paste0("Could not locate R script: ", script_file))
  }

  # Define project first (necessary to validate node options)
  proj <- get_project(proj, use_geo_nodes = geo_nodes)

  # Validate arguments
  validate_singularity_options(singularity, singularity_opts)
  validate_node_option(geo_nodes, use_c2_nodes, proj)

  # Determine where stdout and stderr files will go
  output_err <- setup_log_location(log_location, user, indic, ig, rd)
  output_log_dir <- output_err[[1]]
  error_log_dir <- output_err[[2]]

  # Define remaining attributes
  run_time <- get_run_time(use_geo_nodes = geo_nodes, use_c2_nodes = use_c2_nodes, queue = queue, run_time = run_time)
  queue <- get_queue(use_geo_nodes = geo_nodes, use_c2_nodes = use_c2_nodes, queue = queue, run_time = run_time)
  shell <- paste0(corerepo, "/mbg_central/share_scripts/shell_sing.sh")
  sing_image <- get_singularity(image = singularity)

  # resources are all the -l qsub arguments
  resources <- get_resources(use_geo_nodes = geo_nodes, cores = cores, ram_gb = memory, runtime = run_time)

  # Expand loop variables from expand_vars or lv_table (depending on what was provided)
  if (is.null(lv_table) & is.null(expand_vars)) {
    stop("Need to have one of either lv_table or expand_vars")
  }
  if (!is.null(lv_table) & !is.null(expand_vars)) {
    stop("Can only have one of either lv_table or expand_vars")
  }

  if (is.null(lv_table) & !is.null(expand_vars)) {
    lv <- data.table(expand.grid(expand_vars, stringsAsFactors = F))
  } else if (!is.null(lv_table) & is.null(expand_vars)) {
    lv <- copy(as.data.table(lv_table))
  }

  lv$jobname <- paste0(prefix, "_", apply(lv, 1, paste, collapse = "_"))

  # Ensure all whitespace collapsed
  lv[, jobname := gsub(" ", "", jobname, fixed = TRUE)]

  # Save objects ---------------------------------------------------------

  # Create filename using time stamp
  tmp_dir <- "/share/geospatial/tmp/"
  # Append random string on the end to avoid overlapping filenames for
  # rapidly-submitted jobs
  fname <- paste0(user, "_", gsub("-|:| ", "_", Sys.time()), sample(1:100000, 1))

  values_to_save <- c("lv")
  if (!is.null(save_objs)) values_to_save <- c(values_to_save, save_objs)
  save(
    file = paste0(tmp_dir, fname, ".RData"),
    list = values_to_save
  )

  # Qsub over lv rows
  for (i in 1:nrow(lv)) {
    job_name <- lv[i, jobname]

    qsub <- generate_qsub_command(
      # qsub-specific arguments
      stderr_log = error_log_dir,
      stdout_log = output_log_dir,
      project = proj,
      resources = resources,
      job_name = job_name,
      singularity_str = qsub_sing_envs("", singularity_opts, sing_image),
      cores = cores,
      queue = queue,
      priority = priority,
      # Command to qsub
      shell, script_file, fname, i
    )

    returned <- system(qsub, intern = T)
    message(returned)
    job_id <- as.numeric(stringr::str_match(returned, "Your job ([0-9]*) ")[, 2])
    lv[i, jobid := job_id]
    lv[i, the_qsub := qsub]
  }
  return(list(lv, fname, qsub))
}
