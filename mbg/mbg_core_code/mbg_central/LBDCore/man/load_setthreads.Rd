% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/load_setthreads.R
\name{load_setthreads}
\alias{load_setthreads}
\title{Loads a pre-compiled executable for setting MKL/OMP threads}
\usage{
load_setthreads()
}
\value{
None
}
\description{
\code{load_setthreads()} \code{dyn.loads()} the pre-compiled setthreads.so if
in an LBD Singularity image.
}
\details{
This function will first check to see if it is being run in an LBD '
Singularity image, and if so, \code{dyn.load()} the pre-compiled shared
library "setthreads.so" from the specified location. If it also detects that
we are running in RStudio, then it will go ahead and set MKL_NUM_THREADS and
OMP_NUM_THREADS to 1 since:
  * Since RStudio cannot see environmental variables outside of those set in
    the Renviron, we have no way to fire up RStudio in a container with a
    specified number threads
  * We do not want to bake in serial MKL/OMP execution into the Renviron
  * The user can always use \code{setthreads()} to set the number of MKL/OMP
    threads to whatever they choose afterward.
  * This is the safest thing to do considering issues with \code{mclapply()}
    and potentially other functions
  * Attempt to set MKL DYNAMIC to FALSE and OMP_NESTED to TRUE as is the
    recommendation for programs with OpenMP parallel regions which may also
    call multi-threaded MKL processes. This is just a precaution in case
    the user later decides to enable multi-threading for either/both OpenMP
    or MKL. Older LBD images don't have the "setMKLdynamic" or "setOMPnested"
    functions built into "setthreads.so", in which case these won't be set.
This function won't do anything if it is run outside of an LBD Singularity
container.

WARNING: The "OMP_NESTED=true" and "MKL_DYNAMIC=false" that are normally set
         when an LBD Singularity container is launched through various shell
         scripts (e.g. 'singR.sh' and 'shell_sing.sh') may not be set if
         running RStudio from an LBD image that is R3.5.0 or older, or if
         the image is not an LBD image. If these settings are unable to be
         made, you may see some unexpected behavior when running programs
         with both MKL and OMP parallel regions (like TMB) where both have
         been set to multicore. For more recent LBD images, this script will
         set those below.
}
\seealso{
Threads for MKL and OpenMP as well as MKL DYNAMIC and OMP NESTED
are set here automatically and can be set to different values using these
related functions:
\code{\link{setmklthreads()}}
\code{\link{setompthreads()}}
\code{\link{setmkldynamic()}}
\code{\link{setompnested()}}

Other MBG setup functions: \code{\link{is_integer}},
  \code{\link{is_lbd_core_repo}},
  \code{\link{is_lbd_singularity}},
  \code{\link{is_rstudio}}, \code{\link{is_singularity}},
  \code{\link{load_R_packages}},
  \code{\link{load_mbg_functions}},
  \code{\link{mbg_setup}},
  \code{\link{set_original_threads}},
  \code{\link{set_serial_threads}},
  \code{\link{source_functions}}
}
\author{
Ian M. Davis, \email{imdavis@uw.edu}
}
\concept{MBG setup functions}
