#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param xy PARAM_DESCRIPTION
#' @param ss PARAM_DESCRIPTION
#' @param target_ss PARAM_DESCRIPTION
#' @param min_in_bin PARAM_DESCRIPTION, Default: 5
#' @param rand PARAM_DESCRIPTION, Default: T
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if (interactive()) {
#'   # EXAMPLE1
#' }
#' }
#' @rdname quadtree_ct
#' @export
quadtree_ct <- function(xy, ss, target_ss, min_in_bin = 5, rand = T) {
  ## my hacked version. quadtree by sum of value at each point.
  ## is ss=1 for all points you get back to original quadtree
  ## this function quadtrees by sample size
  ## rand introduces some randomness to the "median"
  print(paste0("Aiming for ts between: ", target_ss, " and ", 2 * target_ss))
  d <- dim(xy)[2]
  quad <- function(xy, i, id = 1, ss, target_ss) {
    if (sum(ss) < target_ss * 2 | length(xy) / 2 <= min_in_bin) {
      rv <- list(id = id, value = xy)
      class(rv) <- "quadtree.leaf"
    }
    else {
      if (rand) {
        q0 <- (1 + runif(1, min = -1 / 10, max = 1 / 10) / dim(xy)[1]) / 2 # Random quantile near the median
      } else {
        q0 <- 1 / 2 ## no randomness, just the median
      }
      x0 <- quantile(xy[, i], q0)
      j <- i %% d + 1 # (Works for octrees, too...)
      rv <- list(
        index = i, threshold = x0,
        lower = quad(xy[xy[, i] <= x0, ], j, id * 2, ss[xy[, i] <= x0], target_ss),
        upper = quad(xy[xy[, i] > x0, ], j, id * 2 + 1, ss[xy[, i] > x0], target_ss)
      )
      class(rv) <- "quadtree"
    }
    return(rv)
  }
  quad(xy = xy, i = 1, id = 1, ss = ss, target_ss = target_ss)
}
