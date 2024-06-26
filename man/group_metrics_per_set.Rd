% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/metrics_calc_group.R
\name{group_metrics_per_set}
\alias{group_metrics_per_set}
\title{Group Metrics of Collective Motion in a Dataset}
\usage{
group_metrics_per_set(
  data_list,
  mov_av_time_window,
  geo,
  step2time,
  parallelize = FALSE
)
}
\arguments{
\item{data_list}{A list of dataframes with groups timeseries per set.
Columns must include: \code{id}, \code{t}, \code{set}, \code{head},
\code{x}, \code{y}, \code{speed}.}

\item{mov_av_time_window}{Integer, timesteps to use as a sliding window
for average speed and polarization.}

\item{geo}{Logical, whether positions are geographic coordinates,
default = FALSE.}

\item{step2time}{Double, the sampling frequency of the data (the relation
between a time step and real time in seconds).}

\item{parallelize}{Logical, whether or not to parallelize over the
timesteps of each set.}
}
\value{
A dataframe with the group average timeseries for each set, with columns:
 \code{set}, \code{t}, \code{pol}, \code{speed}, \code{shape}, \code{N} (number of individuals),
 \code{missing_ind} (whether some individuals are missing), \code{pol_av} (moving average
 of polarization based on input time window) and \code{speed_av} (moving average of
 speed based on input time window).
}
\description{
This function calculates the timeseries of average speed,
 polarization and shape of all set in a dataset
}
\examples{

data <- data.frame(
 set = rep("1", 50),
 t = as.POSIXct(rep(1:25, 2), origin = Sys.time()),
 id = c(rep(1, 25), rep(2, 25)),
 x = rnorm(50),
 y = rnorm(50),
 head = runif(50, 0, 2 * pi),
 speed = rnorm(50)
 )

gm <- group_metrics_per_set(list(data),
 mov_av_time_window = 5,
 geo = FALSE,
 step2time = 1
 )
}
\seealso{
\code{\link{group_metrics}}, \code{\link{moving_average}}
}
\author{
Marina Papadopoulou \email{m.papadopoulou.rug@gmail.com}
}
