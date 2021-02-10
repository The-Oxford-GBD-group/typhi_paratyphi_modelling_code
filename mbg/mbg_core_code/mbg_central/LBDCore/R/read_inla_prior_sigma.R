#' @title Read INLA SD prior for TMB
#'
#' @description Read in a prior specification that is suited for INLA and make
#' it TMB readable.
#'
#' @param prior_string character, character vec of length 1 specifying priors
#'
#' @return List specifying a TMB prior, containing three elements:
#' - type: Is the prior normal, loggamma, or pc.prec
#' - par1: The first shape parameter. In the lognormal case, the mean
#' - par2: The second shape parameter. In the lognormal case, the variance
#'
#' @export
read_inla_prior_sigma <- function(prior_string) {
  prior_list <- eval(parse(text = prior_string[1]))
  if (!(prior_list$prior %in% c("normal", "loggamma", "pc.prec"))) {
    stop("TMB implementation only supports normal, loggamma, or PC priors for 
         SD parameters.")
  }
  return(list(
    type = prior_list$prior,
    par1 = prior_list$param[1],
    par2 = prior_list$param[2]
  ))
}
