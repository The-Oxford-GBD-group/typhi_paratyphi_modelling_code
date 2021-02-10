#' @title Read INLA Matern GP priors for TMB
#'
#' @description Read in a prior specification from config and make
#' it TMB readable.
#'
#' @param prior_string character, character vec of length 1 specifying priors
#'
#' @return List containing (1) spde object and (2) list specifying a TMB prior,
#' containing three elements:
#' - type: Is the prior pc or nonpc (i.e. normal)
#' - par1: Vector of length 2 for the first parameter. In the nonpc case,
#' corresponds to mean and precision for logtau. In the pc case,
#' corresponds to range0 and prange.
#' - par2: Vector of length 2 for the first parameter. In the nonpc case,
#' corresponds to mean and precision for logkappa. In the pc case,
#' corresponds to sigma0 and psigma.
#' @export
read_inla_prior_matern <- function(prior_string, mesh_s) {
  prior_list <- eval(parse(text = prior_string[1]))

  spde_list <- build_spde_prior(prior_list, mesh_s, st_gp_int_zero = FALSE)
  spde_prior <- spde_list$spde_prior
  spde <- spde_list$spde

  if (spde_prior$type == "nonpc") {
    par1 <- c(spde$param.inla$theta.mu[1], spde$param.inla$theta.Q[1, 1])
    par2 <- c(spde$param.inla$theta.mu[2], spde$param.inla$theta.Q[2, 2])
  } else {
    par1 <- spde_prior$prior$range
    par2 <- spde_prior$prior$sigma
  }


  return(list(
    spde = spde,
    prior = list(
      type = prior_list$type,
      par1 = par1,
      par2 = par2
    )
  ))
}
