% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/make_custom_raking_shapefile.R
\name{make_custom_raking_shapefile}
\alias{make_custom_raking_shapefile}
\title{Make Custom Raking Shapefile}
\usage{
make_custom_raking_shapefile(countries, raking_shapefile_version)
}
\arguments{
\item{countries}{character vector of iso3 codes to change from subnational to national. Any subnational not in this list (out of "ETH", "KEN", "CHN", "ZAF", "BRA", "IND", "IDN", "IRN", "MEX", "NGA", "PAK", "PHL") will stay as ADM1}

\item{raking_shapefile_version}{standard shapefile version date (YYYY_MM_DD)}
}
\value{
Returns a new raking shapefile with adm1 polygons switched out with adm0 polygons for the specified country. stops if invalid country iso3s are passed in.
}
\description{
A function used to generate a new shapefile where adm1 polygons in `countries` are replaced with their corresponding adm0 polygon
}
\details{
Make Custom Raking Shapefile
}
