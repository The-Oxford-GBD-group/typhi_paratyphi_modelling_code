% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/multiplot.R
\name{multiplot}
\alias{multiplot}
\title{Multiple ggplots}
\source{
Adapted from \url{http://www.cookbook-r.com/Graphs/Multiple_graphs_on_one_page_(ggplot2)/}
}
\usage{
multiplot(..., plotlist = NULL, cols = 1, layout = NULL,
  legend = NULL)
}
\arguments{
\item{...}{Can pass in ggplot objects in \code{...} or in plotlist}

\item{plotlist}{Can pass in ggplot objects as a list with this argument instead of in \code{...}}

\item{cols}{Number of columns in layout}

\item{layout}{Matrix specifying the layout; i.e. \code{matrix(c(1,2,3,3), nrow = 2, byrow = T)}.
If \code{layout} is specified, then \code{cols} is ignored}

\item{legend}{A legend object.  If legend is passed, then this will add an extra cell at the
end of the grid layout and insert the legend there (good, for instance, if you
have common legends for all of your plots and only want to show it once).}
}
\value{
Prints a gridded output of your ggplot objects to the active graphical device
}
\description{
Function to plot multiple ggplot objects together#'
}
\note{
gg_stackers_results is a list of ggplot objects
}
\examples{
\dontrun{
# Use first legend only
the_legend <- g_legend(gg_stackers_results[[1]])
gg_stackers_results <- lapply(gg_stackers_results, function(x) return(x + theme(legend.position = "none"))) #'
multiplot(
  plotlist = gg_stackers_results,
  cols = ceiling(length(gg_stackers_results) / 2),
  legend = the_legend
)
}
}
