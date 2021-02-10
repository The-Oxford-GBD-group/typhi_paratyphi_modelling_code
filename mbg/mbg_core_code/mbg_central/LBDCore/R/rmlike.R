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
#' @rdname rmlike
#' @export
rmlike <- function(...) {
  # for cleaning clutter
  # https://stackoverflow.com/a/11625075
  names <- sapply(
    match.call(expand.dots = FALSE)$..., as.character
  )
  names <- paste(names, collapse = "|")
  Vars <- ls(1)
  r <- Vars[grep(paste("^(", names, ").*", sep = ""), Vars)]
  rm(list = r, pos = 1)
}
