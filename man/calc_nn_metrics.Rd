% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/metrics_calc_pairwise.R
\name{calc_nn_metrics}
\alias{calc_nn_metrics}
\title{Relative position of nearest neighbor function to parallelize}
\usage{
calc_nn_metrics(thists, geo)
}
\arguments{
\item{thists}{A timestep of individual positions, ids and headings.}

\item{geo}{Logical, whether positions are geographic coordinates.}
}
\value{
The input dataframe with new columns for nnd (nearest neighbor distance),
nn_id (its id) and bangl (nearest neighbor bearing angle).
}
\description{
Calculates the bearing angle and distance from a focal
individual to its nearest neighbor.
}
\author{
Marina Papadopoulou \email{m.papadopoulou.rug@gmail.com}
}
\keyword{internal}