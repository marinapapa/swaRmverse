% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/wrap_col_motion_metrics.R
\name{col_motion_metrics_from_raw}
\alias{col_motion_metrics_from_raw}
\title{Collective Motion Metrics from Raw Data}
\usage{
col_motion_metrics_from_raw(
  data,
  mov_av_time_window,
  step2time = 1,
  geo = FALSE,
  verbose = FALSE,
  speed_lim = NA,
  pol_lim = NA,
  parallelize_all = FALSE,
  noise_thresh = 0
)
}
\arguments{
\item{data}{A data frame with time series of individual's positional
data through time. Columns must include: \code{id}, \code{set}, \code{t}, \code{x}, \code{y}.}

\item{mov_av_time_window}{Numeric, a time window to average over for
speed and polarization timeseries (in timesteps).}

\item{step2time}{Numeric, the sampling frequency of the dateset
(the relation between a time step and real time in seconds).}

\item{geo}{Logical, whether positions are geographic coordinates,
default = FALSE.}

\item{verbose}{Logical, whether to post updates on progress, default = FALSE.}

\item{speed_lim}{Numeric, the threshold of speed for the definition of
an event. For more info see: \code{\link{pick_threshold}}.}

\item{pol_lim}{Numeric, the threshold of polarization for the definition of
an event. For more info see: \code{\link{pick_threshold}}.}

\item{parallelize_all}{Logical, whether or not to parallelize over timesteps.}

\item{noise_thresh}{Numeric, the limit of time difference between consecutive events
to be considered the same event. Default value is 0 (no event merging).}
}
\value{
A dataframe with metrics of collective motion per event.
}
\description{
This function calculates metrics of collective motion across sets and events.
}
\examples{
data <- data.frame(
 set = rep(1, 75),
 x = rnorm(75, sd = 3),
 y = rnorm(75, sd = 3),
 t = as.POSIXct(rep(1:25, 3), origin = Sys.time()),
 id = c(rep(1, 25), rep(2, 25), rep(3, 25))
 )

metrics <- col_motion_metrics_from_raw(data,
 mov_av_time_window = 5,
 step2time = 1,
 geo = FALSE,
 speed_lim = 0,
 pol_lim = 0,
 noise_thresh = 1
 )

}
\seealso{
\code{\link{add_velocities}, \link{group_metrics}, \link{pairwise_metrics}, \link{moving_average}}
}
\author{
Marina Papadopoulou \email{m.papadopoulou.rug@gmail.com}
}
