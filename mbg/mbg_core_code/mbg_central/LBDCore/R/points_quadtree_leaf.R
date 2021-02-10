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
#' @rdname points.quadtree.leaf
#' @export
points.quadtree.leaf <- function(q, alpha = 0.1, ...) {
  points(q$value, col = alpha(q$id, alpha = alpha), ...)
}
