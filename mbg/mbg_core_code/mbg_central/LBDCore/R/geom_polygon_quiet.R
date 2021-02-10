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
#'  \code{\link[ggplot2]{geom_polygon}}
#' @rdname geom_polygon_quiet
#' @export
#' @importFrom ggplot2 geom_polygon
geom_polygon_quiet <- function(...) {
  # Quiet version of geom polygon and path to supress messages
  suppressMessages(ggplot2::geom_polygon(...))
}
