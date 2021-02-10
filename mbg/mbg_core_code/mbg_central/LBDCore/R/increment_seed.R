#' @title Increment seed
#'
#' @description Increment seed by 1 if not null
#' 
#' @param seed integer to be passed to set.seed for reproducibility
#' 
#' @return seed integer incremented by 1, or null
increment_seed <- function(seed) {
  if(!is.null(seed)) {
    message("Seed is set to ", seed, "; incrementing by 1")
    seed <- seed + 1
    assign("seed", seed, envir = .GlobalEnv)
  }
  return(seed)
}