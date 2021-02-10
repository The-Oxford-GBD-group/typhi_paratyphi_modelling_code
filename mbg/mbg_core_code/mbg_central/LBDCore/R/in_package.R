#' @title in a package?
#' @description Predicate: is this code running in a package?
#'
#' @return TRUE if code is running in a package, FALSE otherwise.
#' @export
.in.package <- function() {
  !is.null(utils::packageName())
}
