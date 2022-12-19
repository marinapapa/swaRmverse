#' @title Group polarization
#' @description Calculates the timeseries of polarization of a group
#' @param data A dataframe of (ordered) time series of heading per individual. The dataframe may contain several individuals.
#' Column names must be: id, headx, heady, time.
#' @return A vector of polarization over time
#' @author Marina Papadopoulou \email{m.papadopoulou.rug@@gmail.com}
#' @export
group_polarization <- function(
    data
)
{
  if (!is.data.frame(data) ||
      !('id' %in% colnames(data)) ||
      !('time' %in% colnames(data)) ||
      !('headx' %in% colnames(data)) ||
      !('heady' %in% colnames(data)) ||
      length(unique(data$id)) < 2 )
  {
    stop("Data should be a dataframe with columns: id, headx, heady, time and containing more than 1 individuals.")
  }

  N <- length(unique(data$id)) # in case it is a factor with levels not represented

  magns <- vector_magnitude(data$headx, data$heady)
  data$normHeadX <- data$headx/magns
  data$normHeadY <- data$heady/magns

  per_time <- split(data, data$time)
  polars <- lapply(per_time, function(x) {
    sumx <- sum(x$normHeadX)
    sumy <- sum(x$normHeadY)
    D <- vector_magnitude(sumx, sumy)/N

    return(D)
  })

  polardf <- data.frame(pol = unlist(polars))
  polardf$time <- rownames(polardf)
  return(polardf)
}
