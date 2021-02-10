#' @title Return value if provided in function, else return global of same name.
#'
#' @description Returns the value named \code{name} from the calling function's environment. If that
#' results in an error (e.g., beause the value was not provided) then return an identically
#' named value from the global environment. If no such value exists in the global
#' environment then error.
#'
#' @param name character name of the value to return.
#'
#' @return the value.
#'
#' @export
use_global_if_missing <- function(name) {
  tryCatch(
    {
      return(get(name, pos = parent.frame(1)))
    },
    error = function(e) {
      if (name %in% names(.GlobalEnv)) {
        return(.GlobalEnv[[name]])
      } else {
        stop(sprintf("Variable %s not provided to function and not available in global environment", name))
      }
    }
  )
}