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
#' @seealso
#'  \code{\link[ggplot2]{geom_path}}
#' @rdname geom_path_quiet
#' @export
#' @importFrom ggplot2 geom_path
geom_path_quiet <- function(...) {
  suppressMessages(
    ggplot2::geom_path(...)
  )
}
