#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param nids PARAM_DESCRIPTION
#' @param yr PARAM_DESCRIPTION
#' @param ss PARAM_DESCRIPTION
#' @param n_folds PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if (interactive()) {
#'   # EXAMPLE1
#' }
#' }
#' @rdname nid_folds
#' @export
nid_folds <- function(nids, yr, ss, n_folds) {
  ## simulate some data
  ## n <- 500
  ## yr <- c(2000, 2005, 2010, 2015)
  ## xy <- matrix(runif(n * length(yr) * 2), ncol = 2)
  ## ss <- sample(1:100, size = n * length(yr), replace = TRUE)
  ## age <- sample(1:4, size=n*length(yr), replace=TRUE)
  ## yrs <- sample(yr, size = n * length(yr), replace = TRUE)
  ## df <- as.data.frame(cbind(xy, ss, yrs, age))
  ## colnames(df) <- c("longitude", "latitude", "ss", "year", "age")
  ## ss_col   = "ss"  ;     yr_col = 'year'     ; spat_strat = 'rand'; temp_strat = 'prop'
  ## long_col = 'longitude'; lat_col = 'latitude'; n_folds = 5; strat_cols = 'age'
  ## folds = make_folds(data = df, n_folds = 5, spat_strat = 'rand', temp_strat = 'prop',
  ##                    long_col = 'longitude', lat_col = 'latitude', strat_cols = 'age')
  # holdout strategy based on randomly dropping NIDs

  # calculate the total sample size by NID
  ss_by_nid <- tapply(ss, nids, sum)

  # randomly sort the NIDs
  ss_by_nid <- ss_by_nid[sample(1:length(ss_by_nid))]

  # calculate a running total sample size in the new sort order
  cumulative_ss <- Reduce(sum, ss_by_nid, accumulate = T)

  # identify five roughly equal folds based on the cumulative sample size
  target_fold_ss <- sum(ss) / n_folds
  brks <- sapply(1:5, function(x) which.min(abs(cumulative_ss - x * target_fold_ss)))
  folds <- cut(cumulative_ss, breaks = c(0, cumulative_ss[brks]), labels = F)

  # return in the proper format
  fold_list <- lapply(1:n_folds, function(x) {
    which(nids %in% names(ss_by_nid)[folds == x])
  })
  return(fold_list)
}
