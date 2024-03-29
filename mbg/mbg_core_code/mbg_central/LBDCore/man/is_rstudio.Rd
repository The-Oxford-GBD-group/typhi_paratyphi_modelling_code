% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/is_rstudio.R
\name{is_rstudio}
\alias{is_rstudio}
\title{Tests if within RStudio}
\usage{
is_rstudio(check_singularity = FALSE)
}
\arguments{
\item{check_singularity}{Logical to check if RStudio is running in a
Singularity container [default = FALSE]}
}
\value{
TRUE/FALSE
}
\description{
\code{is_rstudio} returns TRUE if within RStudio, FALSE otherwise.
}
\details{
This function can tell if it is being run from within RStudio. If the
'check_singularity' argument is FALSE (default) it will return TRUE if it is
in any RStudio session. If the argument is set to TRUE, it will return TRUE
only if 'singularity' is found within 'LD_LIBRARY_PATH'. Unfortunately, we
have to do it this way because rstudio-server seems to mask all of the normal
system environmental variables, so we can't check for SINGULARITY_NAME in the
environment as we do above in \code{is_singularity}.
}
\examples{
\dontrun{
# return TRUE if in RStudio / FALSE otherwise
is_rstudio()
# TRUE only if RStudio lives within a Singularity container
is_rstudio(check_singularity = FALSE)
}

}
\seealso{
This function is used by:
  \code{\link{load_R_packages}}

Other MBG setup functions: \code{\link{is_integer}},
  \code{\link{is_lbd_core_repo}},
  \code{\link{is_lbd_singularity}},
  \code{\link{is_singularity}},
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
