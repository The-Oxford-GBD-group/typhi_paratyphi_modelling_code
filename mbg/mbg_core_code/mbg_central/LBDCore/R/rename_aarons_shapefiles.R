#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param len PARAM_DESCRIPTION, Default: (\n#'length(Regions) * 4
#' @param prefix PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if (interactive()) {
#'   # EXAMPLE1
#' }
#' }
#' @rdname rename_aarons_shapefiles
#' @export
rename_aarons_shapefiles <- function(len = (length(Regions) * 4), prefix) {
  ## quadtree example
  ## n <- 25000         # Points per cluster
  ## n_centers <- 40  # Number of cluster centers
  ## sd <- 1/2        # Standard deviation of each cluster
  ## set.seed(17)
  ## centers <- matrix(runif(n_centers*2, min=c(-90, 30), max=c(-75, 40)), ncol=2, byrow=TRUE)
  ## xy <- matrix(apply(centers, 1, function(x) rnorm(n*2, mean=x, sd=sd)), ncol=2, byrow=TRUE)
  ## k <- 5
  ## system.time(qt <- quadtree(xy, k))
  ##
  ## xylim <- cbind(x=c(min(xy[,1]), max(xy[,1])), y=c(min(xy[,2]), max(xy[,2])))
  ## plot(xylim, type="n", xlab="x", ylab="y", main="Quadtree")
  ## lines(qt, xylim, col="Gray")
  ## points(qt, pch=16, cex=0.5)
  ## quadtree_ct example
  ## ## test it out on some simulated data with different sample sizes at each point
  ## n <- 20          # Points per cluster
  ## n_centers <- 10  # Number of cluster centers
  ## sd <- 1/2        # Standard deviation of each cluster
  ## set.seed(17)
  ## centers <- matrix(runif(n_centers*2, min=c(-90, 30), max=c(-75, 40)), ncol=2, byrow=TRUE)
  ## xy <- matrix(apply(centers, 1, function(x) rnorm(n*2, mean=x, sd=sd)), ncol=2, byrow=TRUE)
  ## ss <- c(rep(1, n*n_centers/2), rep(5, n*n_centers/2))
  ## n <- 1           # Points per cluster
  ## n_centers <- 5   # Number of cluster centers
  ## sd <- 1/2        # Standard deviation of each cluster
  ## centers <- matrix(runif(n_centers*2, min=c(-90, 30), max=c(-75, 40)), ncol=2, byrow=TRUE)
  ## xy <- rbind(xy,
  ##             matrix(apply(centers, 1, function(x) rnorm(n*2, mean=x, sd=sd)), ncol=2, byrow=TRUE))
  ## ss <- c(ss, rep(20, n*n_centers))
  ## k <- 5
  ## system.time(qt <- quadtree_ct(xy, ss, 20, min_in_bin=1))
  ## ## plot
  ## xylim <- cbind(x=c(min(xy[,1]), max(xy[,1])), y=c(min(xy[,2]), max(xy[,2])))
  ## png("qt_test.png")
  ## plot(xylim, type="n", xlab="x", ylab="y", main="Quadtree w/ max SS = 20")
  ## lines(qt, xylim, col="Gray")
  ## points(qt, pch=16, cex=0.5, alpha=0.9)
  ## points(xy, col=as.factor(ss), pch=16)
  ## legend('bottomright', legend=c('1', '5', '20'), col=1:3, pch=16)
  ## dev.off()
  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## done
  ## example for discussion only. can delete later
  ## t1 <- function(t1, t2, t11, t12, ...){...}
  ## t2 <- function(t1, t2, t21, t22, ...){...}
  ## s1 <- function(s1, s2, s11, s12, ...){...}
  ## s2 <- function(s1, s2, s21, s22, ...){...}
  ## wrapper <- function(t_fun, s_fun, t1, t2, s1, s2, ...){
  ##   ## t_fun: either '1' or '2'
  ##   ## s_fun: either '1' or '2'
  ##   ## dictionary to match choice to function
  ##   t_dict <- list('1' = t1,
  ##                  '2' = t2)
  ##   t_use  <- s_dict[[t_fun]]
  ##   ## run t function
  ##   t_res  <- t_use(t1, t2, ...)
  ##   ## dictionary to match choice to function
  ##   s_dict <- list('1' = s1,
  ##                  '2' = s2)
  ##   s_use  <- s_dict[[s_fun]]
  ##   ## run s function
  ##   s_res  <- s_use(s1, s2, ...)
  ## }
  # sneaky guy for roy :)
  for (i in 1:len) {
    for (f in list.files(paste0(sharedir, "/output/", run_date, "/"), pattern = paste0("spat_holdout_stratum_", i, "_t"))) {
      file.rename(paste0(sharedir, "/output/", run_date, "/", f), paste0(sharedir, "/output/", run_date, "/", prefix, "_", gsub(paste0("spat_holdout_stratum_", i), names(stratum_qt)[[i]], f)))
    }
  }
}
