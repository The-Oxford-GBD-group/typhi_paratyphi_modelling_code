#' @title Extract Legend
#'
#' @description Extracts the legend from a 5-yr bin graph for final plot
#'
#' @param a.plot a ggplot or grob object
#'
#' @return A grob legend object
#'
#' @export
gLegend <- function(a.plot) {
  if ("ggplot" %in% class(a.plot)) {
    tmp <- ggplot_gtable(ggplot_build(a.plot))
  } else if ("grob" %in% class(a.plot)) {
    tmp <- .gplot
  }

  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)
}
