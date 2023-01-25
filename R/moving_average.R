#' @title Moving average
#' @description Calculates the moving average of a time series.
#' @param timeseries Vector of doubles (timeseries)
#' @param window Time-window to average over (in timesteps)
#' @return a vector of doubles (average over the window)
#' @author Marina Papadopoulou \email{m.papadopoulou.rug@@gmail.com}
#' @export
moving_average <- function(timeseries, window)
{
  if (window < 2) { stop('Moving average needs a time window larger than 1 time step.' )}
  if (window/2 > length(timeseries)) { stop('Time window too large for this timeseries, please check that all days have enough data points.' )}

   mov_av <- rep(NA, length(timeseries))
  for (t in (1+window/2):(length(timeseries) - window/2))
  {
    mov_av[t] <- mean(timeseries[(t-window/2):(t+window/2)], na.rm = T)
  }
  return(mov_av)
}
