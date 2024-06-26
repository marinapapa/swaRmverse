% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/velocities.R
\name{perIdVels}
\alias{perIdVels}
\title{Speed and Heading Calculation based on \code{swaRm} Package.}
\usage{
perIdVels(per_id, geo)
}
\arguments{
\item{per_id}{Time series of 1 individual's positional data.}

\item{geo}{whether positions are geographic coordinates, default = FALSE.}
}
\value{
the input dataframe with a new speed and heading (rotational) column.
}
\description{
This function calculates headings and speeds based on
consecutive locations of 1 individual. Based on the \code{heading} and the
\code{linear_speed} functions of \code{swaRm}.
}
\author{
Marina Papadopoulou \email{m.papadopoulou.rug@gmail.com}
}
\keyword{internal}
