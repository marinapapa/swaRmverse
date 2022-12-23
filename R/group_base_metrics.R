#' @title Calculation of group metrics in parallel
#' @description Calculates the average speed, polarization and shape of the group through time.
#' @param data A dataframe of (ordered) time series of headings, positions and speeds per individual.
#' The dataframe may contain several individuals. Should include column for: id, time, speed, headx, heady, posx, posy.
#' @return A dataframe with the group average timeseries, with columns:
#'  date, time, pol, speed, shape, N (number of individuals), missing_ind (whether some individuals are missing).
#' @author Marina Papadopoulou \email{m.papadopoulou.rug@@gmail.com}
#' @export
group_metrics_parallel <- function(data)
{
  if (!is.data.frame(data) ||
      !('time' %in% colnames(data)) ||
      !('speed' %in% colnames(data)))
    {
      stop("Data should be a dataframe, with columns: time and speed")
    }

  if (length(unique(data$id)) < 2)
    {
      stop("Data should contain more than 1 individual.")
    }

  if (length(unique(data$date)) > 1)
    {
      stop('Data should be from one date only.')
    }


  per_time <- split(data, data$time)

  numCores <- parallel::detectCores()
  cl <- parallel::makeCluster(numCores-1)

  group_prop <- parallel::parLapply(cl, per_time, function(x) {

    N <- length(unique(x$id))
    x <- x[stats::complete.cases(x), ]
    Nnew <- length(unique(x$id))
    missing_ind <- N-Nnew
    N <- Nnew

    t <- x$time[1]
    day <- x$date[1]

    if (N < 2) {return(data.frame(date = day, time = t, pol = NA, speed = NA, shape = NA, N = 1, missing_ind = NA))}

    sumx <-sum(x$headx)
    sumy <-sum(x$heady)
    D <- vector_magnitude(sumx, sumy)/N

    av_speed <- mean(x$speed, na.rm = TRUE)

    shape <- group_shape_oblong_dev(x)
    df <- data.frame(date = day, time = t, pol = D, speed = av_speed, shape = shape, N = N, missing_ind = missing_ind)
    return(df)
  })

  group_prop <- dplyr::bind_rows(group_prop)
  group_prop <- group_prop[stats::complete.cases(group_prop),]
  parallel::stopCluster(cl)

  return(group_prop)
}
