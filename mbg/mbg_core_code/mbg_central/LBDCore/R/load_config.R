#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param ... PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if (interactive()) {
#'   # EXAMPLE1
#' }
#' }
#' @rdname load_config
#' @export
load_config <- function(...) {
  ##### Overloading load_config to point to set_up_config in misc_functions
  warning("load_config() and check_config() will be deprecated in favor of set_up_config()")
  set_up_config(...)
}
