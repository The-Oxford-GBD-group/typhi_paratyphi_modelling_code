% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/rasterize_check_coverage.R
\name{rasterize_check_coverage}
\alias{rasterize_check_coverage}
\title{Rasterize with border checks}
\usage{
rasterize_check_coverage(shapes, template_raster, field, ...,
  link_table = modeling_shapefile_version)
}
\arguments{
\item{shapes}{SpatialPolygonDataFrame.. Input shapefile}

\item{template_raster}{SpatialPolygonDataFrame.. The reference raster (usually WorldPop)}

\item{field}{String The field with appropriate administrative unit information (usually ADM0_CODE)}

\item{link_table}{String or data.table. If data.table it is used as-is. If String: either an absolute
file path to an RDS file OR a short name for the administrative shape file e.g., "2019_02_27" or "current".}
}
\value{
A raster with border and insides properly filled
}
\description{
Rasterizing using a shapefile and a template raster, such that
we account for any pixels that are on the border of \code{field} units, which
could have been lost due to raster::rasterize only evaluating on centroids
}
\details{
rasterize_check_coverage has three distinct use cases based off of the value of link_table

1. \code{link_table} is NULL. In this case rasterize_check_coverage will behave identically to raster::rasterize

2. \code{link_table} is a String referring to relase of admin shapefiles ("current" or e.g., "2019_02_27"). In this case
   \code{field} should be "ADM0_CODE", "ADM1_CODE" or "ADM2_CODE". This will load the lbd_standard_link.rds file,
   from the related admin shapefile directory, aggregate area_fraction as necessary to match the level of \code{field},
   and then apply those values to pixels in the space defined by \code{shapes}.

3. \link{link_table} is a data.table OR a String absolute path to a RDS file containing a data.table. This will use the
   provided \code{link_table} to assign values to the result raster similarly to use case #2.

Note that for both use cases 2 and 3 all pixel_id coordinates must be in the same raster space. This is currently the
area defined by cropping the world raster to the pixels occupied by stage 1 and stage 2 countries.
}
