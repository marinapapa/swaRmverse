#' @title Pairwise metrics of collective motion in dataset
#'
#' @description Calculates the bearing angle and distance from each
#' focal individual of a group to its nearest neighbor over time, across
#' the sets of a dataset.
#'
#' @param data_list A list of dataframes with groups timeseries per set.
#' Columns must include: id, t, set, head, x, y.
#'
#' @param geo Logical, whether positions are geographic coordinates,
#' default = FALSE.
#'
#' @param verbose Logical, whether to post updates on progress, default = FALSE.
#'
#' @param add_coords Logical, whether data on relative positions are converted
#' into geographic coordinates, default = 'FALSE'.
#'
#' @param parallelize Logical, whether to run the function in parallel over
#' timesteps, default = FALSE.
#'
#' @return A dataframe format of the input list, with new columns for nearest neighbor id (nn_id),
#' bearing angles (bangl), and distances (nnd). If add_coords is TRUE, the columns
#' nnx and nny are also added.
#'
#' @author Marina Papadopoulou \email{m.papadopoulou.rug@@gmail.com}
#'
#' @seealso \code{\link{nn_metrics}, \link{group_metrics_per_set}}
#'
#' @export
pairwise_metrics <- function(data_list,
                             geo = FALSE,
                             verbose = FALSE,
                             parallelize = FALSE,
                             add_coords = FALSE
                             ) {
  if (verbose) print("Pairwise analysis started..")

  toret <- lapply(X = data_list,
                  FUN = nn_metrics,
                  add_coords = add_coords,
                  geo = geo,
                  verbose = verbose,
                  parallelize = parallelize
  )

  names(toret) <- NULL
  toret <- do.call(rbind, toret)
  return(toret)
}


#' @title Nearest neighbour metrics
#'
#' @description Calculates the bearing angle and distance from
#' all focal individuals in a group to their nearest neighbor over time.
#'
#' @param data A dataframe with the group's positional timeseries for one set.
#' Column names must include: id, x, y, t. The calculations are based on
#' the \code{swaRm} package.
#'
#' @param geo Logical, whether positions are geographic coordinates, default = FALSE.
#'
#' @param add_coords Logical, whether the data on relative positions of
#' nearest neighbours should be converted into
#' coordinates in the reference frame of the focal individual (nnx, nny). This
#' can be useful for visualization purposes but it is not used in the package
#' pipeling. Default = 'FALSE'.
#'
#' @param verbose Logical, whether to post updates on progress.
#'
#' @param parallelize Logical, whether to parallelize the function over time.
#'
#' @return The input dataframe with new columns for nearest neighbor id (nn_id),
#' bearing angle (bangl), and distance (nnd).
#' If add_coords is TRUE, the columns nnx and nny are  added.
#'
#' @author Marina Papadopoulou \email{m.papadopoulou.rug@@gmail.com}
#'
#' @seealso \code{\link{add_rel_pos_coords}, \link{group_metrics}}
#'
#' @export
nn_metrics <- function(data,
                       add_coords = FALSE,
                       geo = FALSE,
                       verbose = FALSE,
                       parallelize = FALSE
                       ) {

  data$only_time <- format(data$t, "%H:%M:%OS2")
  per_time <- split(data, data$only_time)

  if (parallelize) {
   if (verbose) {
      print("Calculating nearest neighbor metrics in parallel...")
    }
    nm <- par_nn_metrics(per_time, geo)
    if (verbose) {
      print("Done!")
    }
  } else {
    nm <- lapply(X = per_time,
                 FUN = calc_nn_metrics,
                 geo = geo)
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
#'
#' @description Calculates the bearing angle and distance from
#' a focal individual to its nearest neighbor at the given timestep.
#'
#' @param per_time A groups positional data at a single timestep.
#' Columns must include: id, t, x, y, head.
#'
#' @param geo Logical, whether positions are geographic coordinates, default = FALSE.
#'
#' @param verbose Logical,whether to post updates on progress
#'
#' @return The input dataframe with new columns for nnd (nearest neighbor distance),
#' nn_id (its id) and bangl (nearest neighbor bearing angle).
#'
#' @author Marina Papadopoulou \email{m.papadopoulou.rug@@gmail.com}
#'
#' @keywords internal
par_nn_metrics <- function(per_time,
                          geo = FALSE
                          ) {

 num_cores <- parallel::detectCores()
  cl <- parallel::makeCluster(num_cores - 1)

  res <- tryCatch({
    pbapply::pblapply(per_time,
                      calc_nn_metrics,
                      geo = geo,
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
#'
#' @description Calculates the bearing angle and distance from a focal
#' individual to its nearest neighbor.
#'
#' @param thists A timestep of individual positions, ids and headings.
#'
#' @param geo Logical, whether positions are geographic coordinates.
#'
#' @return The input dataframe with new columns for nnd (nearest neighbor distance),
#' nn_id (its id) and bangl (nearest neighbor bearing angle).
#'
#' @author Marina Papadopoulou \email{m.papadopoulou.rug@@gmail.com}
#'
#' @keywords internal
calc_nn_metrics <- function(thists,
                            geo
                            ) {
  thists$nn_id <- swaRm::nn(thists$x, thists$y, geo = geo, id = thists$id)
  thists$nnd <- as.numeric(swaRm::nnd(thists$x, thists$y, geo = geo))
  thists$bangl <- nnba(thists$x, thists$y, hs = thists$head, geo = geo)

  return(thists)
}
