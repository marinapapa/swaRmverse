#' @title Turns real time to timesteps
#' @description Replaces time with seconds since the beginning of the timeseries.
#' @param data A dataframe of time series. Column names must contain: time.
#' @param start_t The real time from which to start the time series count in seconds.
#' @return The input dataframe with the initial time column as 'real_time' and the overwritten time column in seconds after 'start_t'
#' @author Marina Papadopoulou \email{m.papadopoulou.rug@@gmail.com}
#' @export
transform_time2secs <- function(
    data,
    start_t
)
{
  data$real_time <- data$time
  time_dates <- paste(data$date, data$time, sep = " ")
  start_t <- paste(data$date[1], start_t, sep = " ")
  data$time <-  as.numeric(difftime(time_dates, start_t, units = 'secs'))
  return(data)
}

