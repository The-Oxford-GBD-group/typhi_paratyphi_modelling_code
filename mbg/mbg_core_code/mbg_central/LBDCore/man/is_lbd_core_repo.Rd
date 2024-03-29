% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/is_lbd_core_repo.R
\name{is_lbd_core_repo}
\alias{is_lbd_core_repo}
\title{A function to detect if a directory path is the central lbd_core repo or
(likely) a fork of that repo.}
\usage{
is_lbd_core_repo(path)
}
\arguments{
\item{path}{A path}
}
\value{
TRUE/FALSE
}
\description{
\code{is_lbd_core_repo} returns TRUE/FALSE if final subdirectory is
'lbd_core'
}
\details{
A function to detect if a directory path is the central lbd_core repo or
likely a fork of that repo. This is necessary for \code{mbg_setup} to know
whether or not to search only the 'mbg_central' subdirectory or not.
}
\seealso{
This function is used by:
\code{\link{mbg_setup}}

Other MBG setup functions: \code{\link{is_integer}},
  \code{\link{is_lbd_singularity}},
  \code{\link{is_rstudio}}, \code{\link{is_singularity}},
  \code{\link{load_R_packages}},
  \code{\link{load_mbg_functions}},
  \code{\link{load_setthreads}}, \code{\link{mbg_setup}},
  \code{\link{set_original_threads}},
  \code{\link{set_serial_threads}},
  \code{\link{source_functions}}
}
\author{
Ian M. Davis, \email{imdavis@uw.edu}
}
\concept{MBG setup functions}
