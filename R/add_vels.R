#' @title Add motion properties in parallel
#' @description Calculates headings and speeds based on
#' two location points and the time taken to travel between those points
#' @param data Time series with individual's positional data through time.
#' Columns must include: id, time, lon, lat.
#' @param lonlat whether positions are geographic coordinates, default = TRUE.
#' @param verbose whether to post updates on progress, default = FALSE.
#' @param parallelize whether to run the function in parallel, default = FALSE.
#' @return the input dataframe with new speed and heading (rotational) column
#' @author Marina Papadopoulou \email{m.papadopoulou.rug@@gmail.com}
#' @export
add_vels <- function(
    data,
    lonlat = TRUE,
    verbose = FALSE,
    parallelize = FALSE
    ) {
  if (!(all(c("t", "x", "y", "id") %in% colnames(data)))) {
    stop("Input dataframe should include columns: t, x, y, and id.")
    }

  data$head <- data$speed <- rep(NA_real_, nrow(data))
  per_id <- split(data, data$id)

  if (parallelize) {
    if (verbose) {
      per_id <- parAddVelsVerb(per_id, lonlat)
    } else {
      per_id <- parAddVels(per_id, lonlat)
    }
    data <- do.call(rbind, per_id)
    return(data)
  } else {
    per_id <- do_add_vels(per_id, lonlat)
  }
  data <- do.call(rbind, per_id)
  return(data)
}


#' @title Adding motion properties in parallel without progress updates
#' @description Calculates headings and speeds based on
#' two location points and the time taken to travel between those points
#' @param per_id Time series with individual's positional data through time.
#' Columns must include: id, time, lon, lat.
#' @param lonlat whether positions are geographic coordinates, default = FALSE.
#' @return the input dataframe with new speed and heading (rotational) column
#' @author Marina Papadopoulou \email{m.papadopoulou.rug@@gmail.com}
#' @keywords internal
parAddVels <- function(
    per_id,
    lonlat = FALSE
    ) {
  num_cores <- parallel::detectCores()
  cl <- parallel::makeCluster(num_cores)

  res <- tryCatch({
   parallel::parLapply(cl = cl,
                       X = per_id,
                       fun = perIdVels,
                       lonlat = lonlat
                       )
    },
    error = function(cond) {
      parallel::stopCluster(cl)
      stop(cond)
    })
  parallel::stopCluster(cl)
  names(res) <- NULL
  return(res)
}

#' @title Adding motion properties in parallel - verbose
#' @description Calculates headings and speeds based on
#' two location points and the time taken to travel between those points
#' @param per_id Time series with individual's positional data through time.
#' Columns must include: id, time, lon, lat.
#' @param lonlat whether positions are geographic coordinates, default = FALSE.
#' @param verbose whether to post updates on progress
#' @return the input dataframe with new speed and heading (rotational) column
#' @author Marina Papadopoulou \email{m.papadopoulou.rug@@gmail.com}
#' @keywords internal
parAddVelsVerb <- function(
    per_id,
    lonlat = FALSE
    ) {
  print("Calculating heading timeseries in parallel...")

  num_cores <- parallel::detectCores()
  cl <- parallel::makeCluster(num_cores)

  res <- tryCatch({
    pbapply::pblapply(X = per_id,
                      FUN = perIdVels,
                      lonlat = lonlat,
                      cl = cl
                      )
    },
    error = function(cond) {
      parallel::stopCluster(cl)
      stop(cond)
    })

  parallel::stopCluster(cl)
  print("Parallel computation done, preparing return data...")

  names(res) <- NULL
  return(res)
}


#' @title Adding motion properties in parallel without progress updates
#' @description Calculates headings and speeds based on
#' two location points and the time taken to travel between those points
#' @param per_id Time series with individual's positional data through time.
#' Columns must include: id, time, lon, lat.
#' @param lonlat whether positions are geographic coordinates, default = FALSE.
#' @return the input dataframe with new speed and heading (rotational) column
#' @author Marina Papadopoulou \email{m.papadopoulou.rug@@gmail.com}
#' @keywords internal
do_add_vels <- function(
    per_id,
    lonlat = FALSE
    ) {
  res <- lapply(X = per_id, FUN = perIdVels, lonlat = lonlat)
  names(res) <- NULL
  return(res)
}



#' @title Speed and heading calculation
#' @description Calculates headings and speeds based on
#' two location points of 1 individual.
#' @param data Time series of 1 individual's positional data.
#' @param lonlat whether positions are geographic coordinates, default = FALSE.
#' @return the input dataframe with new speed and heading (rotational) column
#' @author Marina Papadopoulou \email{m.papadopoulou.rug@@gmail.com}
#' @keywords internal
perIdVels <- function(
  per_id,
  lonlat
  ) {
    per_id <- as.data.frame(per_id)

    if (nrow(per_id) < 2) {
      warning("Id with only one data point detected.")
      return(per_id)
    }

    per_id[, "head"] <- swaRm::heading(x = per_id$x, y = per_id$y, geo = lonlat) #swaRm_heading
    per_id[, "speed"] <- swaRm::linear_speed(
      x = per_id$x,
      y = per_id$y,
      t = per_id$t,
      geo = lonlat
      )

    return(per_id)
}
