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
#' @rdname lines.quadtree
#' @export
lines.quadtree <- function(q, xylim, ...) {
  i <- q$index
  j <- 3 - q$index
  clip <- function(xylim.clip, i, upper) {
    if (upper) {
      xylim.clip[1, i] <- max(q$threshold, xylim.clip[1, i])
    } else {
      xylim.clip[2, i] <- min(q$threshold, xylim.clip[2, i])
    }
    xylim.clip
  }
  if (q$threshold > xylim[1, i]) lines(q$lower, clip(xylim, i, FALSE), ...)
  if (q$threshold < xylim[2, i]) lines(q$upper, clip(xylim, i, TRUE), ...)
  xlim <- xylim[, j]
  xy <- cbind(c(q$threshold, q$threshold), xlim)
  lines(xy[, order(i:j)], ...)
}
