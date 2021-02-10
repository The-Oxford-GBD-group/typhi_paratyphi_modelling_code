#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param l PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if (interactive()) {
#'   # EXAMPLE1
#' }
#' }
#' @rdname parseArgsS
#' @export
parseArgsS <- function(l) {
  # for parsing gams string functions
  # stolen from the seeg stuff
  # parse a list of additional arguments to smoothers in gamTrans
  stopifnot(is.list(l))
  l_string <- paste(names(l),
    lapply(l, addQuotes),
    sep = " = ",
    collapse = ", "
  )
  return(l_string)
}
