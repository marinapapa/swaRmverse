#' @title Calculation of group metrics in parallel
#' @description Calculates the average speed, polarization and shape of the group through time.
#' @param data A dataframe of (ordered) time series of headings, positions and speeds per individual.
#' The dataframe may contain several individuals. Should include column for: id, time, speed, headx, heady, posx, posy.
#' @param id_label the name of the column with individual id information
#' @param date_label the name of the column with date information
#' @param pos_label_x the name of the column with x coordinate of individual position
#' @param pos_label_y the name of the column with y coordinate of individual position
#' @param head_label_x the name of the column with x coordinate of individual heading
#' @param head_label_y the name of the column with y coordinate of individual heading
#' @return A dataframe with the group average timeseries, with columns:
#'  date, time, pol, speed, shape, N (number of individuals), missing_ind (whether some individuals are missing).
#' @author Marina Papadopoulou \email{m.papadopoulou.rug@@gmail.com}
#' @export
group_metrics_parallel <- function(
    data,
    id_label = 'id',
    date_label = 'date',
    pos_label_x = 'posx',
    pos_label_y = 'posy',
    head_label_x = 'headx',
    head_label_y = 'heady'
)
{
  if (!is.data.frame(data) ||
      !(id_label  %in% colnames(data)) ||
      !('time' %in% colnames(data)) ||
      !('speed' %in% colnames(data)) ||
      !(head_label_x %in% colnames(data)) ||
      !(head_label_y %in% colnames(data)) ||
      !(pos_label_x %in% colnames(data)) ||
      !(pos_label_y %in% colnames(data)))
    {
      stop("Data should be a dataframe with columns: time and speed, as well as the input labels for date, id, pos and head.")
    }

  if (length(unique(data[,id_label])) < 2)
    {
      stop("Data should contain more than 1 individual.")
    }

  if (length(unique(data[,date_label])) > 1)
    {
      stop('Data should be from one date only.')
    }


  per_time <- split(data, data$time)

  numCores <- parallel::detectCores()
  cl <- parallel::makeCluster(numCores-1)

  group_prop <- parallel::parLapply(cl, per_time, function(x) {

    N <- length(unique(x[,id_label]))
    x <- x[stats::complete.cases(x), ]
    Nnew <- length(unique(x[,id_label]))
    missing_ind <- N-Nnew
    N <- Nnew

    t <- x$time[1]
    day <- x[1, date_label]

    if (N < 2) {return(data.frame(date = day, time = t, pol = NA, speed = NA, shape = NA, N = 1, missing_ind = NA))}

    sumx <-sum(x[,head_label_x])
    sumy <-sum(x[,head_label_y])
    D <- vector_magnitude(sumx, sumy)/N

    av_speed <- mean(x$speed, na.rm = TRUE)

    shape <- group_shape_oblong_dev(x, pos_label_x, pos_label_y)
    df <- data.frame(date = day, time = t, pol = D, speed = av_speed, shape = shape, N = N, missing_ind = missing_ind)
    return(df)
  })

  group_prop <- dplyr::bind_rows(group_prop)
  group_prop <- group_prop[stats::complete.cases(group_prop),]
  parallel::stopCluster(cl)

  return(group_prop)
}
