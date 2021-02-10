#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param x PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if (interactive()) {
#'   # EXAMPLE1
#' }
#' }
#' @rdname cellIdx
#' @export
# use my condSim update until forked repo is pulled by NG
# source('../seegMBG/R/gis_functions.R')
cellIdx <- function(x) which(!is.na(getValues(x[[1]])))
