#' @title Calculation of group metrics in parallel
#' @description Calculates the average speed, polarization and shape of the group through time.
#' @param data A dataframe of (ordered) time series of headings, positions and speeds per individual.
#' The dataframe may contain several individuals. Should include column for: id, time, speed, headx, heady, posx, posy.
#' @param lonlat logical, whether positions are geographic coordinates, default = FALSE.
#' @return A dataframe with the group average timeseries, with columns:
#'  date, time, pol, speed, shape, N (number of individuals), missing_ind (whether some individuals are missing).
#' @author Marina Papadopoulou \email{m.papadopoulou.rug@@gmail.com}
#' @export
group_metrics_parallel <- function(data, lonlat)
{
  if (!is.data.frame(data) ||
      !('time' %in% colnames(data)) ||
      !('head' %in% colnames(data)) ||
      !('speed' %in% colnames(data)))
    {
      stop("Data should be a dataframe, with columns: time, head, and speed")
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
  cl <- parallel::makeCluster(numCores)

  group_prop <-  tryCatch({

    pbapply::pblapply(per_time, function(x) {

    N <- length(unique(x$id))
    t <- x$time[1]
    day <- x$date[1]

    x <- x[stats::complete.cases(x), ]
    if (nrow(x) < 1 ){
      return(data.frame(date = day, time = t, pol = NA, speed = NA, shape = NA, N = N, missing_ind = NA))}
    Nnew <- length(unique(x$id))
    missing_ind <- N-Nnew
    N <- Nnew

    if (N < 2) {return(data.frame(date = day, time = t, pol = NA, speed = NA, shape = NA, N = 1, missing_ind = NA))}

    D <- swaRm::pol_order(x$head)
    av_speed <- mean(x$speed, na.rm = TRUE)
    x$headx <- cos(x$head)
    x$heady <- sin(x$head)

    obb <- group_shape(x = x$x, y = x$y, hs = x$head, geo = lonlat)
    shape <- as.numeric(obb$shape)

    df <- data.frame(date = day, time = t, pol = D, speed = av_speed, shape = shape, N = N, missing_ind = missing_ind)
    return(df)
  },
  cl = cl)
    },
  error = function(cond) {
  parallel::stopCluster(cl)
  stop(cond)
})

  parallel::stopCluster(cl)

  names(group_prop) <- NULL
  group_prop <- do.call(rbind, group_prop)
  group_prop <- group_prop[stats::complete.cases(group_prop),]

  return(group_prop)
}
