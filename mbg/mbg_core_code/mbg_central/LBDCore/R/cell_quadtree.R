#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param q PARAM_DESCRIPTION
#' @param xylim PARAM_DESCRIPTION
#' @param ... PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if (interactive()) {
#'   # EXAMPLE1
#' }
#' }
#' @rdname cell.quadtree
#' @export
cell.quadtree <- function(q, xylim, ...) {
  i <- q$index
  j <- 3 - q$index
  clip <- function(xylim_clip, i, upper) {
    if (upper) {
      xylim_clip[1, i] <- max(q$threshold, xylim_clip[1, i])
    } else {
      xylim_clip[2, i] <- min(q$threshold, xylim_clip[2, i])
    }
    xylim_clip
  }
  d <- data.frame(id = NULL, x = NULL, y = NULL)
  if (q$threshold > xylim[1, i]) d <- cell(q$lower, clip(xylim, i, FALSE), ...)
  if (q$threshold < xylim[2, i]) d <- rbind(d, cell(q$upper, clip(xylim, i, TRUE), ...))
  d
}
