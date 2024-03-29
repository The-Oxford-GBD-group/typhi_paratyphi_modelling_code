% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fix_raster_tmpdir.R
\name{fix_raster_tmpdir}
\alias{fix_raster_tmpdir}
\title{Set temporary directory used by the raster package and clear old files.}
\usage{
fix_raster_tmpdir()
}
\description{
\code{fix_raster_tmpdir()} loads and configures the raster package.
}
\details{
By default the raster package uses /tmp to store temporary files by default.
This is problematic as IHME machines are not configured to have a large
amount of /tmp space, and multiple users will quickly fill the directory
leading to a non-functioning computer. This function does two things: set the
temporary directory to /share/scratch/tmp/geospatial-tempfiles/$USER (a location
agreed to by IHME infrastructure) and also delete all files a week or older
in that directory owned by whomever is running the function.
}
\seealso{
This is called by:
\code{\link{mbg_setup}}
}
\author{
Mike Richards, \email{miker985@uw.edu}
}
