#' @title Nearest neighbor metrics
#' @description Calculates the bearing angle and distance from
#' a focal individual of a group to its nearest neighbor over time.
#' @param data Dataframe with group's timeseries for one set.
#' Column names must include: id, time.
#' @param lonlat whether positions are geographic coordinates, default = FALSE.
#' @param add_coords whether data on relative positions are converted into
#' geographic coordinates, default = 'FALSE'.
#' @param verbose whether to post updates on progress
#' @param parallelize whether to parallelize the function over time
#' @return a dataframe with a column for neighbor id, bearing angle,
#' distance and heading deviation for each individual through time.
#' @author Marina Papadopoulou \email{m.papadopoulou.rug@@gmail.com}
#' @export
nn_metrics <- function(data,
                       add_coords = FALSE,
                       lonlat = FALSE,
                       verbose = FALSE,
                       parallelize = FALSE
                       ) {

  data$only_time <- format(data$t, "%H:%M:%OS2")
  per_time <- split(data, data$only_time)

  if (parallelize) {
   if (verbose) {
      print("Measuring nearest neighbors relative positions in parallel...")
    }
    nm <- par_nn_metrics(per_time, lonlat)
  } else {
    nm <- lapply(X = per_time,
                 FUN = calc_nn_metrics,
                 lonlat = lonlat)
  }

  nm <- nm[sapply(nm, function(x) nrow(x) > 0)]
  names(nm) <- NULL
  nm <- do.call(rbind, nm)

  if (add_coords) {
    nm <- add_rel_pos_coords(nm)
  }
  return(nm)
}

#' @title Adding motion properties in parallel - verbose
#' @description Calculates headings and speeds based on
#' two location points and the time taken to travel between those points
#' @param per_time Time series with groups positional data through time.
#' Columns must include: id, time, lon, lat.
#' @param lonlat whether positions are geographic coordinates, default = FALSE.
#' @param verbose whether to post updates on progress
#' @return the input dataframe with new speed and heading (rotational) column
#' @author Marina Papadopoulou \email{m.papadopoulou.rug@@gmail.com}
#' @keywords internal
par_nn_metrics <- function(per_time,
                          lonlat = FALSE
                          ) {

 num_cores <- parallel::detectCores()
  cl <- parallel::makeCluster(num_cores)

  res <- tryCatch({
    pbapply::pblapply(per_time,
                      calc_nn_metrics,
                      lonlat = lonlat,
                      cl = cl)
   },
   error = function(cond) {
    parallel::stopCluster(cl)
    stop(cond)
  })

  parallel::stopCluster(cl)
  return(res)
}


#' @title Relative position of nearest neighbor function to parallelize
#' @description Calculates the bearing angle and distance from a focal
#' individual of a group to its nearest neighbor.
#' @param thists A timestep of individual positions and ids.
#' @param lonlat whether positions are geographic coordinates.
#' @return the input dataframe with a column for neighbor id, bearing angle,
#' distance and heading deviation for each individual through time.
#' @author Marina Papadopoulou \email{m.papadopoulou.rug@@gmail.com}
#' @keywords internal
calc_nn_metrics <- function(thists, lonlat) {
  thists$nn_id <- swaRm::nn(thists$x, thists$y, geo = lonlat, id = thists$id)
  thists$nnd <- as.numeric(swaRm::nnd(thists$x, thists$y, geo = lonlat))
  thists$bangl <- nnba(thists$x, thists$y, hs = thists$head, geo = lonlat)

  return(thists)
}
