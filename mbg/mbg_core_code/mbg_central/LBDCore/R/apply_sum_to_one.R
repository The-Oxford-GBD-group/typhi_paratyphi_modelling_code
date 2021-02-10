#' @title Apply sum-to-one to fitted value draws
#'
#' @description Given draws of fixed effect values generated from a fitted TMB
#' model and a vector of which columns are stackers with a sum-to-one in that
#' model, apply transformations on constrained fixed effects to reproduce how
#' they were incorporated in the model (constraining them sum-to-one). See
#' https://en.wikipedia.org/wiki/Dirichlet_distribution#Gamma_distribution for
#' details on how a transformation of (log) gamma distributed RVs can be
#' transformed into RVs with a dirichlet distribution. If a stacker column has
#' a label of 0 (unconstrained, the default) or for any non-stacker columns,
#' the untransformed draws will be returned.
#'
#' @param stacker_col_id int vector, which columns have sum-to-one (1) or not (0)
#' @param FE_draws matrix, fitted draws of coefficients
#'
#' @return matrix of transformed beta coefficients for fixed effect draws
#'
#' @export
apply_sum_to_one <- function(stacker_col_id, FE_draws) {
  unnormalized_draws <- t(exp(FE_draws[stacker_col_id == 1, ]))
  normalized_draws <- unnormalized_draws / rowSums(unnormalized_draws)
  FE_draws[stacker_col_id == 1, ] <- t(normalized_draws)

  return(FE_draws)
}
