% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/setmkldynamic.R
\name{setmkldynamic}
\alias{setmkldynamic}
\title{Enables MKL to dynamically change the number of OpenMP threads}
\usage{
setmkldynamic(enable = FALSE)
}
\arguments{
\item{enable}{A single logical indicating whether or not to enable MKL
dynamic. [default = FALSE]}
}
\value{
None
}
\description{
\code{setmkldynamic()} Uses a function in the "setthreads.so" shared library
built into LBD Singularity images to enable/disable MKL's ability to change
the number of OpenMP threads dynamically.
}
\details{
Uses a function in the "setthreads.so" shared library built into LBD
Singularity images to enable/disable MKL's ability to change the number of
OpenMP threads dynamically. Since many of the packages we use in R rely on
OpenMP we normally want to disable MKL dynamic, which is done by default here
(0 is a FALSE, i.e. requests disabling dynamic adjustment, see:
https://software.intel.com/en-us/mkl-developer-reference-c-mkl-set-dynamic).
The shared library should exist in the LBD Singularity image and should have
already been loaded by \code{mbg_setup()}. This function checks to make sure
that it is loaded and if not, attempts to use \code{load_setthreads()} to do
so. A warning is generated if this is run outside of an LBD Singularity image
and no MKL dynamic adjustment is done. This function is normally used along
with \code{setompnested()} as follows:
\code{setmkldynamic(enable = FALSE)} and \code{setompnested(enable = TRUE)}
as described here: https://software.intel.com/en-us/articles/recommended-settings-for-calling-intel-mkl-routines-from-multi-threaded-applications
}
\examples{
\dontrun{
setmkldynamic(enable = FALSE) # disables MKL dynamic
}
}
\seealso{
This function depends on:
\code{\link{load_setthreads()}}
And is related to:
\code{\link{setompnested()}}
\code{\link{setmklthreads()}}
\code{\link{setompthreads()}}

Other Mutlti-threading Functions: \code{\link{get_max_forked_threads}},
  \code{\link{get_mkl_threads}},
  \code{\link{get_omp_threads}},
  \code{\link{get_total_threads}},
  \code{\link{setmklthreads}}, \code{\link{setompnested}},
  \code{\link{setompthreads}}
}
\author{
Ian M. Davis, \email{imdavis@uw.edu}
}
\concept{Mutlti-threading Functions}
