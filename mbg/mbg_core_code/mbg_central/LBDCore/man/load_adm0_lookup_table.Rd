% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/load_adm0_lookup_table.R
\name{load_adm0_lookup_table}
\alias{load_adm0_lookup_table}
\title{Load the GAUL lookup table}
\usage{
load_adm0_lookup_table()
}
\value{
Returns a data.frame of the ADM0 lookup table. If package data.table
  is loaded, also converts the lookup table to a data.table
}
\description{
Loads the most recent version of the lookup table that links
  ADM0 codes with other identifiers such as GBD location IDs, MBG modeling
  regions, and ISO codes, among others.
}
