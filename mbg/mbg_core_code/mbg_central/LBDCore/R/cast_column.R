#' @title Cast column
#' @description Cast a column in a dataframe given a class
#'
#' @param df a dataframe or datatable
#' @param column the name of a column in `df`
#' @param class one of "character", "numeric", "integer", "factor" or "logical"
#'
#' @return `df` with column `column` typecast to `class`
#'
#' @export
cast_column <- function(df,
                        column,
                        class) {
  if (class == "character") {
    df[[column]] <- as.character(df[[column]])
  } else if (class == "numeric") {
    df[[column]] <- as.numeric(df[[column]])
  } else if (class == "integer") {
    df[[column]] <- as.integer(df[[column]])
  } else if (class == "factor") {
    df[[column]] <- as.factor(df[[column]])
  } else if (class == "logical") {
    df[[column]] <- as.logical(df[[column]])
  } else {
    message("For column ", column, ": ", class, " is not an available type to cast to")
  }
  return(df)
}
