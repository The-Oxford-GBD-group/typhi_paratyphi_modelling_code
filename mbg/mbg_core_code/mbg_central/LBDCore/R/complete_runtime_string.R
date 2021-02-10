#' @title Complete user-supplied runtime string into DHMS format
#'
#' @description Check the input string of runtime and convert its values into a DD:HH:MM:SS format
#'
#' @param run_time String. User supplied run-time in format \code{[DD]:[HH]:[MM]:[SS]}
#'
#' @return The same run-time supplied by user once validations have passed,
#' but with days appended if not supplied, in format \code{DD:HH:MM:SS}
#'
#' @importFrom stringr str_split
#' @export
complete_runtime_string <- function(run_time) {

  ## Split runtime into its components
  ## We could have 3 (H:M:S) or 4 (D:H:M:S) components here
  rt_split <- as.numeric(stringr::str_split(run_time, ":")[[1]])

  ## Validate for the entries in the timestamp
  ## Return a 'complete' timestamp
  ## with 'DD' added if the validations are correct

  ## DD:HH:MM:SS
  if (length(rt_split) == 4) {
    return(run_time)
  } else if (length(rt_split) == 3) {
    return(paste0("00:", run_time))
  } else if (length(rt_split) == 2) {
    return(paste0("00:00:", run_time))
  } else if (length(rt_split) == 1) {
    return(paste0("00:00:00:", run_time))
  } else {
    stop("Run-time is not in correct format. Exiting.")
  }
}
