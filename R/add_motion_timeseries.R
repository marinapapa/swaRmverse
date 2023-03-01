#' @title Add motion properties in parallel
#' @description Calculates headings and speeds based on two location points and the time taken to travel between those points
#' @param data Time series with individual's positional data through time. Columns must include: id, time, lon, lat.
#' @param lonlat whether positions are geographic coordinates, default = FALSE.
#' @param verbose whether to post updates on progress
#' @return the input dataframe with new speed and heading (rotational) column
#' @author Marina Papadopoulou \email{m.papadopoulou.rug@@gmail.com}
#' @export
padd_motion_properties <- function(
    data,
    lonlat = FALSE,
    verbose = FALSE
)
{
  if (!(all(c('time', 'x', 'y', 'id') %in% colnames(data)))) {
    stop('Input dataframe should include columns: time, x, y, and id.')}

  if (verbose) {
    do_add_motion_properties_verb(data, lonlat)
  }
  else {
    do_add_motion_properties(data, lonlat)
  }

}


#' @title Adding motion properties in parallel without progress updates
#' @description Calculates headings and speeds based on two location points and the time taken to travel between those points
#' @param data Time series with individual's positional data through time. Columns must include: id, time, lon, lat.
#' @param lonlat whether positions are geographic coordinates, default = FALSE.
#' @return the input dataframe with new speed and heading (rotational) column
#' @author Marina Papadopoulou \email{m.papadopoulou.rug@@gmail.com}
#' @keywords internal
do_add_motion_properties <- function(
    data,
    lonlat = FALSE
    )
{
  data$head <- data$speed <- NA_real_

  data$id <- as.character(data$id)
  per_id <- split(data, data$id)


  numCores <- parallel::detectCores()
  cl <- parallel::makeCluster(numCores )

  res <- tryCatch({

   parallel::parLapply(cl = cl,
                               X = per_id,
                               fun = parallel_per_id,
                               lonlat = lonlat
    )
    },
    error = function(cond) {
      parallel::stopCluster(cl)
      stop(cond)
    })


  parallel::stopCluster(cl)
  names(res) <- NULL
  res <- do.call(rbind, res)

  return(res)
}



#' @title Adding motion properties in parallel - verbose
#' @description Calculates headings and speeds based on two location points and the time taken to travel between those points
#' @param data Time series with individual's positional data through time. Columns must include: id, time, lon, lat.
#' @param lonlat whether positions are geographic coordinates, default = FALSE.
#' @param verbose whether to post updates on progress
#' @return the input dataframe with new speed and heading (rotational) column
#' @author Marina Papadopoulou \email{m.papadopoulou.rug@@gmail.com}
#' @keywords internal
do_add_motion_properties_verb <- function(
    data,
    lonlat = FALSE
)
{
  print('Calculating heading timeseries in parallel...')
  data$head <- data$speed <- NA_real_

  data$id <- as.character(data$id)
  per_id <- split(data, data$id)

  numCores <- parallel::detectCores()
  cl <- parallel::makeCluster(numCores)

  res <- tryCatch({
    pbapply::pblapply(per_id,
                             parallel_per_id,
                             lonlat = lonlat,
                             cl = cl
    ) },
    error = function(cond) {
      parallel::stopCluster(cl)
      stop(cond)
    })

  parallel::stopCluster(cl)
  print('Parallel computation done, preparing return data...')

  names(res) <- NULL
  res <- do.call(rbind, res)

  return(res)
}


#' @title Speed and heading calculation
#' @description Calculates headings and speeds based on two location points of 1 individual
#' @param data Time series of 1 individual's positional data.
#' @param lonlat whether positions are geographic coordinates, default = FALSE.
#' @return the input dataframe with new speed and heading (rotational) column
#' @author Marina Papadopoulou \email{m.papadopoulou.rug@@gmail.com}
#' @keywords internal
parallel_per_id <- function(per_id, lonlat)
{
  per_id <- as.data.frame(per_id)

  if (nrow(per_id) < 2)
  {
    warning('Id with only one data point detected.')
    return(per_id)
  }

  per_id[, 'head'] <- swaRm::heading(x = per_id$x, y = per_id$y, geo = lonlat)
  per_id[, 'speed'] <- linSpeed_s(x = per_id$x, y = per_id$y, t = per_id$time, geo = lonlat)

  return(per_id)
}
