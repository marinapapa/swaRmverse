#' @title Calculation of group metrics in parallel
#' @description Calculates the average speed, polarization
#' and shape of the group through time.
#' @param data A dataframe of (ordered) time series of headings,
#'  positions and speeds per individual. The dataframe may contain
#'  several individuals. Should include column for:
#'  id, time, speed, headx, heady, posx, posy.
#' @param lonlat logical, whether positions are geographic coordinates,
#'  default = FALSE.
#' @param parallelize whether to parallelize over time.
#' @return A dataframe with the group average timeseries, with columns:
#'  set, time, pol, speed, shape, N (number of individuals),
#'  missing_ind (whether some individuals are missing).
#' @author Marina Papadopoulou \email{m.papadopoulou.rug@@gmail.com}
#' @export
global_metrics <- function(data,
                           lonlat,
                           parallelize = FALSE) {
  if (!is.data.frame(data) ||
      !("t" %in% colnames(data)) ||
      !("head" %in% colnames(data)) ||
      !("speed" %in% colnames(data))) {
      stop("Data should be a dataframe, with columns: t, head, and speed")
    }

  if (length(unique(data$id)) < 2) {
      stop("Data should contain more than 1 individual.")
    }

  data$only_time <- format(data$t, "%H:%M:%OS2")
  per_time <- split(data, data$only_time) ## right?, instead of data$t

  if (parallelize) {
    gm <- par_calc_global_metrics(per_time, lonlat)
  } else {
    gm <- lapply(X = per_time, FUN = calc_global_metrics, lonlat = lonlat)
  }

  names(gm) <- NULL
  gm <- do.call(rbind, gm)
  gm <- gm[stats::complete.cases(gm), ]

  return(gm)
}

#' @title Calculation of group metrics in parallel
#' @description Calls calc_global_metrics in parallel
#' @param x A list of dataframes per timestep.
#' @param lonlat logical, whether positions are
#' geographic coordinates, default = FALSE.
#' @return A dataframe with the group average timeseries, with columns:
#'  set, time, pol, speed, shape, N (number of individuals),
#'  missing_ind (whether some individuals are missing).
#' @author Marina Papadopoulou \email{m.papadopoulou.rug@@gmail.com}
#' @keywords internal
par_calc_global_metrics <- function(dfs_per_time, lonlat) {
  num_cores <- parallel::detectCores()
  cl <- parallel::makeCluster(num_cores)

  df <-  tryCatch({
    pbapply::pblapply(X = dfs_per_time,
                      FUN = calc_global_metrics,
                      lonlat = lonlat,
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
#' @description Calculates the average speed,
#'  polarization and shape of the group through time.
#' @param x A dataframe timestep of a group.
#' @param lonlat logical, whether positions are
#' geographic coordinates, default = FALSE.
#' @return A dataframe with the group average timeseries, with columns:
#'  set, t, pol, speed, shape, N (number of individuals),
#'  missing_ind (whether some individuals are missing).
#' @author Marina Papadopoulou \email{m.papadopoulou.rug@@gmail.com}
#' @keywords internal
calc_global_metrics <- function(x, lonlat) {

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

  obb <- group_shape(x = x$x, y = x$y, hs = x$head, geo = lonlat)

  data.frame(set = day,
             t = t,
             pol = D,
             speed = av_speed,
             shape = as.numeric(obb$shape),
             N = N,
             missing_ind = missing_ind)
}
