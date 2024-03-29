% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fractional_rake_rates.R
\name{fractional_rake_rates}
\alias{fractional_rake_rates}
\title{Rakes a cell_pred containing rates to GBD targets}
\usage{
fractional_rake_rates(cell_pred = cell_pred,
  simple_raster = simple_raster, simple_polygon = simple_polygon,
  pixel_id = pixel_id, shapefile_version = shapefile_version,
  reg = reg, pop_measure = pop_measure, year_list = year_list,
  use_intermediate_years = TRUE, interval_mo = interval_mo,
  rake_subnational = rake_subnational, age_group = age_group,
  sex_id = sex_id, sharedir = sharedir, run_date = run_date,
  indicator = indicator, gbd = gbd, rake_method = "linear",
  gbd_pops = gbd_pops, countries_not_to_rake = NULL,
  countries_not_to_subnat_rake = NULL, custom_output_folder = NULL)
}
\arguments{
\item{cell_pred}{cell_pred object from mbg models.  Each cell must have a rate in it}

\item{simple_raster}{the simple raster that the cell_pred is based on}

\item{simple_polygon}{the simple polygon that the cell_pred is based on}

\item{pixel_id}{list of the pixels in the simple raster that have non na values}

\item{shapefile_version}{which shapefile geographies are being used}

\item{reg}{the modeling region}

\item{pop_measure}{the worldpop agegroup on which the model is built}

\item{year_list}{the modeled years}

\item{use_intermediate_years}{Boolean to indicate whether or not to rake to intermediate years. Default: TRUE}

\item{interval_mo}{the time in months between the modeled years}

\item{rake_subnational}{a logical value indicating the use of subnational raking targets or not}

\item{age_group}{the gbd age group that the model is built on}

\item{sex_id}{the gbd sex group that the model is built on}

\item{sharedir}{sharedir       <- sprintf('/share/geospatial/mbg/\%s/\%s',indicator_group,indicator)}

\item{run_date}{model run date}

\item{indicator}{modeled indicator}

\item{gbd}{gbd object prepared containing the raking targets}

\item{rake_method}{if set to "logit" creates raking factors in logit space, otherwise assumes linear raking}

\item{gbd_pops}{output from central code "get_population" function}

\item{countries_not_to_rake}{countries (vector or summed string) to not rake to GBD (we set rake factor to 1 for those)}

\item{countries_not_to_subnat_rake}{character vector of iso3 codes for countries not to subnationally rake}

\item{custom_output_folder}{Output the rake factors and outputs to custom folder path if specified. Default: NULL}
}
\value{
automatically saves out two raking factors tables, one for population and one for the indicator
}
\description{
This function:
\enumerate{
  \item Links it to a fractional raking and aggregation link table
  \item Adds population per fractional cell
  \item Rakes that population to ensure matching to GBD
  \item Saves those raking factors
  \item Uses that population to create a linked counts cell_pred
  \item Aggregates the cell_pred to the GBD geography level
  \item Converts the aggregations back to rates and rakes to GBD targets
  \item Saves those raking fators, to a custom location if desired
}
}
