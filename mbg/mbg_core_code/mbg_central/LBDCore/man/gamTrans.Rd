% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/gamTrans.R
\name{gamTrans}
\alias{gamTrans}
\title{Carry out a model-based covariate transformation using a GAM}
\usage{
gamTrans(coords, response, covs, family = gaussian, condition = NULL,
  condition_covs = NULL, extra_terms = NULL, extra_data = NULL,
  bam = TRUE, s_args = list(), predict = TRUE, ...)
}
\arguments{
\item{coords}{a two-column matrix of coordinates of records}

\item{response}{an object acting as thge response object in the GAM
model (e.g. a vector of counts, or a matrix for binomial data)}

\item{covs}{a \code{Raster*} object giving the spatial covariates
for the main part of the model}

\item{family}{the distribution family for the gam}

\item{condition}{an optional vector of 1s and 0s of the same length as
the number of records in \code{coords} and \code{response} and stating
whether the record should also be modelled using covariates in
\code{condition_covs} (1 if so and 0 if not). This enables the construction
of slightly more complex models, such as those with an explicitly modelled
observation process. This is achieved by passing \code{condition} to the
\code{by} argument in \code{mgcv::s} when fitting smooths for
the condition covariates, as well as adding the condition as an intercept.}

\item{condition_covs}{an optional \code{Raster*} object giving the spatial covariates
for the conditional part of the model}

\item{extra_terms}{an optional formula object (of the form \code{~ s(x, k = 2)}
or similar which can be concatenated onto the model formula)
specifying further model components (in \code{extra_data}) not provided in
the spatial covariates.}

\item{extra_data}{an optional dataframe giving the covariates referred to in
\code{extra_terms}}

\item{bam}{whether to fit the model using \code{mgcv::bam} (the default),
otherwise \code{mgcv::gam} is used instead}

\item{s_args}{a named list of additional arguments to pass to the smoother on
each covariate. For example, this may include the smoother type (\code{bs})
or the basis dimension (\code{k}). See \code{\link[mgcv]{s}} for the list
of available arguments.}

\item{predict}{whether to transform the rasters after fitting the model.
If set to \code{FALSE} this can enable model tweaking before the final
transformations are applied, without the computational cost of prediction}

\item{\dots}{other arguments to be passed to \code{mgcv::bam} or
\code{mgcv::gam}}
}
\value{
a three-element named list containing:
 \itemize{
   \item{model}{the fitted \code{bam} or \code{gam} model object}
   \item{trans}{if \code{predict = TRUE} a \code{Raster*} object of the
    same extent, resolution and number of layers as \code{covs}, but with
    the values of each layer having been optimally spline-transformed.
    Otherwise \code{NULL}}
   \item{trans_cond}{if \code{predict = TRUE} and \code{condition} is not
    \code{NULL} a \code{Raster*} object of the same extent, resolution and
    number of layers as \code{condition_covs}, but with the values of each layer
    having been optimally spline-transformed. Otherwise \code{NULL}}
 }
}
\description{
Define an optimal set of univariate covariate
 transformations of a set of model covariates by fitting a generalised
 additive model with univariate smoothers to data, and then using the
 smoothers to spline-transform the covariates.
 This makes use of the\code{type = 'terms'} argument in
 \code{\link{predict.gam}}.
 This function also makes use of
}
