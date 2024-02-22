#' @title Add velocity timeseries across sets
#'
#' @description Calculates and adds the speed and heading of each individual
#' over time in the dataset, and splits it in a list of dataframes based
#' on the defined sets.
#'
#' @param data A data frame with time series of individual's positional data,
#' as exported by the \code{set_data_format} function.
#' Columns needed: 't', 'x', 'y', 'id', 'set'.
#'
#' @param geo Logical, whether positions are geographic coordinates,
#'  default = FALSE.
#'
#' @param verbose Logical, whether to post updates on progress, default = FALSE.
#'
#' @param parallelize Logical, whether to run the function in parallel over individuals,
#'  default = FALSE.
#'
#' @return A list of dataframes, an element per set from the input dataframe
#' with new columns: head and speed.
#'
#' @author Marina Papadopoulou \email{m.papadopoulou.rug@@gmail.com}
#'
#' @seealso \code{\link{add_set_vels}}, \code{\link{set_data_format}}
#'
#' @export
add_velocities <- function(data,
                           geo = TRUE,
                           verbose = FALSE,
                           parallelize = FALSE
) {

  if (!("set" %in% colnames(data))){
    stop("The data should include a set column, as standardised by the
         set_data_format function.")
  }

  splitted_data <- split(data, data$set)

  if (verbose) {
    print("Adding velocity info to every set of the dataset..")
  }
  toret <- lapply(X = splitted_data,
                  FUN = add_set_vels,
                  verbose = verbose,
                  geo = geo,
                  parallelize = parallelize
  )
  if (verbose) {
    print("Done!")
  }
  return(toret)
}


#' @title Add velocity timeseries
#'
#' @description Calculates the headings and speeds of individuals based on
#' two location points and the time taken to travel between those points.
#'
#' @param data A dataframe with the time series of individuals' positions.
#' Columns must include: id, t, x, y.
#'
#' @param geo Logical, whether positions are geographic coordinates, default = TRUE.
#'
#' @param verbose Logical, whether to post updates on progress, default = FALSE.
#'
#' @param parallelize Logical, whether to run the function in parallel, default = FALSE.
#'
#' @return The input dataframe with a new speed and heading (rotational, in rads) columns.
#'
#' @author Marina Papadopoulou \email{m.papadopoulou.rug@@gmail.com}
#'
#' @seealso \code{\link{add_velocities}}
#'
#' @export
add_set_vels <- function(
    data,
    geo = TRUE,
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
      per_id <- parAddVelsVerb(per_id, geo)
    } else {
      per_id <- parAddVels(per_id, geo)
    }
    data <- do.call(rbind, per_id)
    return(data)
  } else {
    per_id <- do_add_vels(per_id, geo)
  }
  data <- do.call(rbind, per_id)

  ## headings of 0 (from not moving) being replaced with NA for safety in group properties calculations
  idx_without_head <- which(abs(data[, "head"]) < 0.00000000001)
  if (length(idx_without_head) > 0){
    warning(paste0("Individuals ",
                   paste(unique(data[idx_without_head,"id"]), collapse = ", "),
                   " have not-moving instances. Their headings have been set to NA for these timesteps."))
    data[idx_without_head ,"head"] <- NA
  }

  return(data)
}


#' @title Adding motion properties in parallel without progress updates
#'
#' @description Calculates headings and speeds based on
#' two location points and the time taken to travel between those points.
#'
#' @param per_id Dataframe with the time series of individuals' positional data.
#' Columns must include: id, t, x, y.
#'
#' @param geo Logical, whether positions are geographic coordinates, default = FALSE.
#'
#' @return the input dataframe with a new speed and heading (rotational) column.
#'
#' @author Marina Papadopoulou \email{m.papadopoulou.rug@@gmail.com}
#'
#' @keywords internal
parAddVels <- function(
    per_id,
    geo = FALSE
    ) {
  num_cores <- parallel::detectCores()
  cl <- parallel::makeCluster(num_cores)

  res <- tryCatch({
   parallel::parLapply(cl = cl,
                       X = per_id,
                       fun = perIdVels,
                       geo = geo
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


#' @title Adding individual velocity information in parallel - verbose
#'
#' @description Calculates headings and speeds based on
#' two location points and the time taken to travel between those points.
#'
#' @param per_id Dataframe, the time series of individuals' positional data.
#' Columns must include: id, t, x, y.
#'
#' @param geo Logical, whether positions are geographic coordinates, default = FALSE.
#'
#' @param verbose Logical, whether to post updates on progress.
#'
#' @return the input dataframe with a new speed and heading (rotational) column.
#'
#' @author Marina Papadopoulou \email{m.papadopoulou.rug@@gmail.com}
#'
#' @keywords internal
parAddVelsVerb <- function(
    per_id,
    geo = FALSE
    ) {
  print("Calculating heading timeseries in parallel...")

  num_cores <- parallel::detectCores()
  cl <- parallel::makeCluster(num_cores)

  res <- tryCatch({
    pbapply::pblapply(X = per_id,
                      FUN = perIdVels,
                      geo = geo,
                      cl = cl
                      )
    },
    error = function(cond) {
      parallel::stopCluster(cl)
      stop(cond)
    })

  parallel::stopCluster(cl)
  print("Parallel computation done!")

  names(res) <- NULL
  return(res)
}


#' @title Adding velocity information for individuals in parallel without progress updates
#'
#' @description Calculates headings and speeds based on
#' two location points and the time taken to travel between those points.
#'
#' @param per_id Dataframe with the time series of individuals' positional data.
#' Columns must include: id, t, x, y.
#'
#' @param geo Logical, whether positions are geographic coordinates, default = FALSE.
#'
#' @return the input dataframe with a new speed and heading (rotational) column.
#'
#' @author Marina Papadopoulou \email{m.papadopoulou.rug@@gmail.com}
#'
#' @keywords internal
do_add_vels <- function(
    per_id,
    geo = FALSE
    ) {
  res <- lapply(X = per_id, FUN = perIdVels, geo = geo)
  names(res) <- NULL
  return(res)
}



#' @title Speed and heading calculation based on swaRm package.
#'
#' @description Calculates headings and speeds based on
#' consecutive locations of 1 individual. Based on the \code{heading} and the
#' \code{linear_speed} functions of \code{swaRm}.
#'
#' @param data Time series of 1 individual's positional data.
#'
#' @param geo whether positions are geographic coordinates, default = FALSE.
#'
#' @return the input dataframe with a new speed and heading (rotational) column.
#'
#' @author Marina Papadopoulou \email{m.papadopoulou.rug@@gmail.com}
#'
#' @keywords internal
perIdVels <- function(
  per_id,
  geo
  ) {
    per_id <- as.data.frame(per_id)

    if (nrow(per_id) < 2) {
      warning(paste0("Id ", per_id[,'id'], "has only one data point, returning NA for its heading."))
      return(per_id)
    }

    if (any(duplicated(per_id$t))){
      warning(paste0("Duplicated times found for individual ",
                     per_id[,'id'],
                     ". Speed calculations will be affected."))
    }
    per_id[, "head"] <- swaRm::heading(x = per_id$x,
                                       y = per_id$y,
                                       geo = geo)

    per_id[, "speed"] <- swaRm::linear_speed(
      x = per_id$x,
      y = per_id$y,
      t = per_id$t,
      geo = geo
      )

    return(per_id)
}
