#' @title Relative position of nearest neighbor in parallel
#' @description Calculates the bearing angle and distance from a focal individual of a group to its nearest neighbor over time.
#' @param data Dataframe with group's timeseries. Column names must include: id, time.
#' @param lonlat whether positions are geographic coordinates, default = FALSE.
#' @param add_coords whether data is converted to geographic coordinates, default = 'FALSE'.
#' @param verbose whether to post updates on progress
#' @return a dataframe with a column for neighbor id, bearing angle, distance and heading deviation for each individual through time.
#' @author Marina Papadopoulou \email{m.papadopoulou.rug@@gmail.com}
#' @export
nn_rel_pos_timeseries_parallel <- function(
    data,
    add_coords = FALSE,
    lonlat = FALSE,
    verbose = FALSE
)
{
  if (verbose) { print('Measuring nearest neighbors relative positions in parallel...')}

  thists <- split(data, data$time)


  numCores <- parallel::detectCores()
  cl <- parallel::makeCluster(numCores)

  res <- tryCatch({
    pbapply::pblapply(thists,
                      pairwise_info,
                      lonlat = lonlat,
                      cl = cl)
   },
   error = function(cond) {
    parallel::stopCluster(cl)
    stop(cond)
  })

  parallel::stopCluster(cl)
  if (verbose) { print('Parallel run done! Preparing output...')}

  res <- res[sapply(res, function(x) nrow(x) > 0)]
  names(res) <- NULL
  res <- do.call(rbind, res)

  if ( add_coords )
  {
    res <- add_rel_pos_coords(res)
  }

  return(res)
}

#' @title Relative position of nearest neighbor function to parallelize
#' @description Calculates the bearing angle and distance from a focal individual of a group to its nearest neighbor.
#' @param thists A timestep of individual positions and ids.
#' @param lonlat whether positions are geographic coordinates.
#' @return the input dataframe with a column for neighbor id, bearing angle, distance and heading deviation for each individual through time.
#' @author Marina Papadopoulou \email{m.papadopoulou.rug@@gmail.com}
#' @keywords internal
pairwise_info <- function(thists,
                          lonlat)


{
  thists <- as.data.frame(thists)
  timestep <- as.character(thists$time[1])
  id_names <- unique(thists$id)
  N <- length(id_names)

  thists$nn_id <- as.numeric(swaRm::nn(thists$x, thists$y, geo = lonlat, id = thists$id))
  thists$nnd <- as.numeric(swaRm::nnd(thists$x, thists$y, geo = lonlat))

  thists$bangl <- nnba(thists$x, thists$y, hs = thists$head, geo = lonlat)

  return(thists)
}
