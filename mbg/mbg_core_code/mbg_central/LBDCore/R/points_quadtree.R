#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param q PARAM_DESCRIPTION
#' @param alpha PARAM_DESCRIPTION, Default: 0.1
#' @param ... PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if (interactive()) {
#'   # EXAMPLE1
#' }
#' }
#' @rdname points.quadtree
#' @export
points.quadtree <- function(q, alpha = 0.1, ...) {
  ## plotting functions
  points(q$lower, alpha, ...)
  points(q$upper, alpha, ...)
}
