#' @title Monitor and resubmit jobs
#' @description Function to monitor & resubmit jobs if they fail
#' This is intended as a replacement for \code{waitformodelstofinish()}
#' and its ilk.  This function takes output from \code{parallelize()}
#' and periodically submits qstat and qacct requests to see how
#' the jobs are running on the cluster.
#'
#' @param parallelize_output output from the `parallelize()` function, where
#' `parallelize_output[[1]] is a data.table of loopvars
#' and `parallelize_output[[2]] is the filename that
#' contains the `save_objs` from parallelize
#' @param sleeptime how long to sleep for between checking the status of the jobs
#' running on the cluster? (numeric, in seconds)
#' @param title title for the looping output
#' @param keep_temp_file keep the temp file after this function exits?
#' logical; if `keep_temp_file = F` then temp file is deleted
#' @param return_lv should this function return the loopvars?
#' @param max_tries maximum number of times to resubmit a job before giving up
#' @param notification how would you like to be notified when jobs fail?
#' The only current option is "pushover" (via `pushover_notify()`)
#' but could be expanded to include email, etc. if desired
#' @return loopvars in data table (if `return_lv = T`)
#' @examples
#' \dontrun{
#' # see example for `parallelize()` for workflow
#'
#' # In master script:
#' monitor_jobs(output_from_parallelize,
#' max_tries = 3,
#' notification = "pushover")
#' }
#' @export
monitor_jobs <- function(parallelize_output,
                         sleeptime = 100,
                         title = "Job Monitor",
                         keep_temp_file = F,
                         return_lv = F,
                         max_tries = 1,
                         notification = "none") {
  lv <- parallelize_output[[1]]
  fname <- parallelize_output[[2]]

  str_match <- stringr::str_match

  # Wait a minute to let all jobs be submitted
  Sys.sleep(60)

  # Add a column to lv to hold exit statuses
  if (!("exit_status" %in% names(lv))) lv[, exit_status := numeric()]
  if (!("tries" %in% names(lv))) lv[, tries := 1]
  if (!("give_up" %in% names(lv))) lv[, give_up := F]

  # Function for updating of loopvars table
  update_lv_table <- function(lv) {

    # Grab and parse qstat
    get_qstat_table <- function() {
      qs <- system("qstat", intern = T)
      qs <- qs[3:length(qs)] # Trim headers
      qs <- lapply(qs, function(x) gsub("^ *|(?<= ) | *$", "", x, perl = TRUE)) %>% unlist()
      qs <- lapply(qs, function(x) unlist(strsplit(x, " ", fixed = T)))
      qs <- lapply(qs, function(x) return(x[1:5])) # Trim to just the useful stuff
      qs <- rbindlist(lapply(qs, function(x) setDT(as.list(x))[]))
      names(qs) <- c("jobid", "prior", "name", "user", "state")
      qs[, jobid := as.numeric(jobid)]
      return(qs)
    }

    qstat <- get_qstat_table()
    if ("state" %in% names(lv)) lv[, state := NULL] # clear state if present
    lv <- merge(lv, subset(qstat, select = c("jobid", "state")), by = "jobid", all.x = T, all.y = F)

    # For any jobs without an exit status that have closed, grab exit status
    get_qacct_exit_status <- function(jobid) {
      qa <- system(paste0("qacct -j ", jobid), intern = T)
      qa <- str_match(qa, "exit_status\\s+([0-9]*)")[, 2]
      qa <- as.numeric(qa[!is.na(qa)])
      return(qa)
    }

    get_qa_wrapper <- function(jobids) {
      Sys.sleep(30) # Give a bit of time to make sure that exit status generated
      return(sapply(jobids, get_qacct_exit_status))
    }

    lv[
      is.na(state) & is.na(exit_status),
      exit_status := get_qa_wrapper(jobid)
    ]

    # update states
    lv[is.na(state), state := "x"]

    return(lv)
  }

  lv <- update_lv_table(lv)
  n_finished <- nrow(lv[state == "x" & exit_status == 0, ])

  while (n_finished < nrow(lv)) {
    lv <- update_lv_table(lv)
    n_finished <- nrow(lv[state == "x" & exit_status == 0, ])

    message("\n====================================================================================")
    message(sprintf("==============================      %s      ===============================", title))
    message(paste0("\nAt ", Sys.time(), " .... ", n_finished, " of ", nrow(lv), " jobs have finished."))
    message("\nJob status:")
    for (i in 1:nrow(lv)) {
      message(paste0(
        "  Job: ", lv[i, "jobname"],
        " | ID: ", lv[i, "jobid"],
        " | Tries: ", lv[i, "tries"],
        " | State: ", lv[i, "state"],
        " | Exit status: ", lv[i, "exit_status"]
      ))
    }
    message("\n====================================================================================")
    message("====================================================================================")
    message("\n")

    check_if_nonzero_exit <- function(lv, notification) {
      for (i in 1:nrow(lv)) {
        es <- lv[i, "exit_status"]
        if (!is.na(es) & (es != 0)) {
          if (lv[i, "tries"] <= max_tries - 1) {
            qs <- lv[i, "the_qsub"]
            returned <- system(as.character(qs), intern = T)
            new_job_id <- as.numeric(str_match(returned, "Your job ([0-9]*) ")[, 2])
            if (notification == "pushover") {
              pushover_notify(paste0(
                "Resubmitted ", lv[i, "jobname"], " with new job id ", new_job_id, ". ",
                "Failed job id: ", lv[i, "jobid"], " | exit status: ", lv[i, "exit_status"], "."
              ),
              title = paste0("Job failed: ", lv[i, "jobname"])
              )
            }
            lv[i, "exit_status"] <- NA
            lv[i, "jobid"] <- new_job_id
            lv[i, "tries"] <- lv[i, "tries"] + 1
          } else if (lv[i, tries] == max_tries) {
            if (lv[i, give_up] == F) {
              if (notification == "pushover") {
                pushover_notify(paste0(
                  "Job ", lv[i, "jobname"], " was resubmitted ", max_tries, " times and will not be resubmitted again.",
                  "Most recent job id was ", lv[i, "jobid"], "."
                ))
              }
              lv[i, give_up := TRUE]
            }
          }
        } # close if statement to catch non-zero exit statuses
      } # close for loop over lv rows
      return(lv)
    } # close check_if_nonzero_exit()
    lv <- check_if_nonzero_exit(lv, notification)
    Sys.sleep(sleeptime)
  } # close while loop
  # Exiting function...
  if (return_lv == T) {
    return(lv)
  }
  if (keep_temp_file == F) unlink(paste0("/share/geospatial/tmp/", fname, ".RData"))
}
