#' @title Calculation of group metrics of collective motion
#'
#' @description Calculates the average speed, polarization
#' and shape of the group through time.
#'
#' @param data A dataframe of (ordered) time series of headings,
#'  positions and speeds per individual. The dataframe may contain
#'  several individuals. Should include column for:
#'  id, t, speed, x, y, head, set.
#'
#' @param geo Logical, whether positions are geographic coordinates,
#'  default = FALSE.
#'
#' @param step2time Numeric, the sampling frequency of the data
#' (the relation between a row in the data and real time in seconds).
#'
#' @param parallelize Logical, whether to parallelize over time.
#' Suggested only for very large datasets.
#'
#' @return A dataframe with the group average timeseries, with columns:
#'  set, t, pol, speed, shape, N (number of individuals),
#'  missing_ind (whether some individuals are missing).
#'
#' @author Marina Papadopoulou \email{m.papadopoulou.rug@@gmail.com}
#'
#' @seealso \code{\link{group_shape}, \link{group_vels}}
#'
#' @export
global_metrics <- function(data,
                           geo,
                           step2time = 1,
                           parallelize = FALSE) {

  if (!is.data.frame(data) ||
      !("t" %in% colnames(data)) ||
      !("set" %in% colnames(data)) ||
      !("head" %in% colnames(data)) ||
      !("x" %in% colnames(data)) ||
      !("y" %in% colnames(data)) ||
      !("speed" %in% colnames(data))) {
      stop("Data should be a dataframe that includes columns: set, t, x, y, head, and speed")
    }

  if (length(unique(data$id)) < 2) {
      warning("Some sets have group sizes of 1.")
    }

  ## the split function doesn't recognise decimal seconds in datetime format, so:
  if (step2time < 1){
    data$only_time <- format(data$t, "%H:%M:%OS2")
  } else{
    data$only_time <- format(data$t, "%H:%M:%S")
  }
  per_time <- split(data, data$only_time)

  if (parallelize) {
    gm <- par_calc_global_metrics(per_time, geo)
  } else {
    gm <- lapply(X = per_time, FUN = calc_global_metrics, geo = geo)
  }

  names(gm) <- NULL
  gm <- do.call(rbind, gm)

  return(gm)
}

#' @title Calculation of group metrics in parallel
#'
#' @description Calls calc_global_metrics in parallel.
#'
#' @param dfs_per_time A list of dataframes per timestep.
#'
#' @param geo Logical, whether positions are
#' geographic coordinates, default = FALSE.
#'
#' @return A dataframe with the group average timeseries, with columns:
#'  set, time, pol, speed, shape, N (number of individuals),
#'  missing_ind (whether some individuals are missing).
#'
#' @author Marina Papadopoulou \email{m.papadopoulou.rug@@gmail.com}
#'
#' @keywords internal
par_calc_global_metrics <- function(dfs_per_time,
                                    geo
                                    ) {
  num_cores <- parallel::detectCores()
  cl <- parallel::makeCluster(num_cores - 1)

  df <-  tryCatch({
    pbapply::pblapply(X = dfs_per_time,
                      FUN = calc_global_metrics,
                      geo = geo,
                      cl = cl)
    },
    error = function(cond) {
      parallel::stopCluster(cl)
      stop(cond)
      }
    )

  parallel::stopCluster(cl)
  return(df)
}

#' @title Calculation of group metrics
#'
#' @description Calculates the average speed,
#'  polarization and shape of the group through time.
#'
#' @param x A dataframe of a timestep of a group.
#'
#' @param geo Logical, whether positions are
#' geographic coordinates, default = FALSE.
#'
#' @return A dataframe with one row,
#' with the group metrics for that timesteps, with columns:
#' set, t, pol, speed, shape, N (number of individuals),
#' missing_ind (whether some individuals are missing).
#'
#' @author Marina Papadopoulou \email{m.papadopoulou.rug@@gmail.com}
#'
#' @keywords internal
calc_global_metrics <- function(x, geo) {

  N <- length(unique(x$id))
  t <- x$t[1]
  day <- x$set[1]

  x <- x[stats::complete.cases(x), ]
  if (nrow(x) < 1) {
    return(data.frame(set = day,
                      t = t,
                      pol = NA,
                      speed = NA,
                      shape = NA,
                      N = N,
                      missing_ind = NA))
    }
  Nnew <- length(unique(x$id))
  missing_ind <- N - Nnew
  N <- Nnew

  if (N < 2) {
    return(data.frame(set = day,
                      t = t,
                      pol = NA,
                      speed = NA,
                      shape = NA,
                      N = 1,
                      missing_ind = NA))
    }

  D <- swaRm::pol_order(x$head)
  av_speed <- mean(x$speed, na.rm = TRUE)
  x$headx <- cos(x$head)
  x$heady <- sin(x$head)

  obb <- group_shape(x = x$x, y = x$y, hs = x$head, geo = geo)

  data.frame(set = day,
             t = t,
             pol = D,
             speed = av_speed,
             shape = as.numeric(obb$shape),
             N = N,
             missing_ind = missing_ind)
}
