#' @title Moving average
#'
#' @description Calculates the moving average of a time series.
#'
#' @param timeseries Vector of doubles representing a timeseries.
#'
#' @param window Double, the time-window to average over (in timesteps).
#'
#' @return A vector of doubles (average over the window).
#'
#' @author Marina Papadopoulou \email{m.papadopoulou.rug@@gmail.com}
#'
#' @examples
#' bs <- rnorm(20, mean = 10, sd = 1)
#' moving_average(bs, 5)
#'
#' @export
moving_average <- function(timeseries,
                           window
                           ) {
  if (window < 2) {
    stop("Moving average needs a time window larger than 1 time step.")
  }

  mov_av <- rep(NA, length(timeseries))

  if (window / 2 > length(timeseries)) {
    warning("Time series shorter than smoothing window,
             NAs returned for that set.")
    return(mov_av)
  }
  for (t in (1 + window / 2):(length(timeseries) - window / 2)) {
    mov_av[t] <- mean(timeseries[(t - window / 2):(t + window / 2)],
                      na.rm = TRUE)
  }
  return(mov_av)
}
