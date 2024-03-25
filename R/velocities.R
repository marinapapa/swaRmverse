#' @title Add Velocity Timeseries Across Sets
#'
#' @description This function calculates and adds the speed and heading of each individual
#' over time in the dataset, and splits it in a list of dataframes based
#' on the defined sets.
#'
#' @param data A data frame with time series of individual's positional data,
#' as exported by the \code{set_data_format} function.
#' Columns needed: \code{set}, \code{t}, \code{id}, \code{x}, \code{y}.
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
#' with new columns: \code{head} and \code{speed}.
#'
#' @author Marina Papadopoulou \email{m.papadopoulou.rug@@gmail.com}
#'
#' @seealso \code{\link{add_set_vels}}, \code{\link{set_data_format}}
#'
#' @examples
#' data <- data.frame(
#' set = rep(1, 25),
#' x = rnorm(25, sd = 3),
#' y = rnorm(25, sd = 3),
#' t = as.POSIXct(1:25, origin = Sys.time()),
#' id = rep(1, 25)
#' )
#'
#' data_list <- add_velocities(data, geo = FALSE)
#'
#' @export
add_velocities <- function(data,
                           geo = FALSE,
                           verbose = FALSE,
                           parallelize = FALSE
) {

  if (!("set" %in% colnames(data))){
    stop("The data should include a set column, as standardised by the
         set_data_format function.")
  }

  splitted_data <- split(data, data$set)

  if (verbose) {
    cat("Adding velocity info to every set of the dataset..\n")
  }
  toret <- lapply(X = splitted_data,
                  FUN = add_set_vels,
                  verbose = verbose,
                  geo = geo,
                  parallelize = parallelize,
                  independent_call = FALSE
  )

  sets_torep <- lapply(toret, function(x){
    if (!is.na(x$message)){
      return(x$data$set[1])
    }})
  sets_torep <- unlist(sets_torep)
  if (length(sets_torep) > 0){
    sets <- paste(sets_torep, collapse = ", ")
    message(paste0("Note: in sets ",
                   sets,
                   " there are individuals with not-moving instances. Their headings have been set to NA for these timesteps."))
  }

  if (verbose) {
    cat("Done!\n")
  }

  lapply(toret, function(x){ x$data})
}

#' @title Add Velocity Timeseries
#'
#' @description This function calculates the headings and speeds of individuals based on
#' two location points and the time taken to travel between those points.
#'
#' @param data A dataframe with the time series of individuals' positions.
#' Columns must include: \code{t}, \code{id}, \code{x}, \code{y}.
#'
#' @param geo Logical, whether positions are geographic coordinates, default = TRUE.
#'
#' @param verbose Logical, whether to post updates on progress, default = FALSE.
#'
#' @param parallelize Logical, whether to run the function in parallel, default = FALSE.
#'
#' @param independent_call Logical, whether the function is called by itself or
#' as part of the package pipeline (through \code{\link{add_velocities}}). The default is
#' set to TRUE, reflecting the direct call of the function by the user.
#'
#' @return The input dataframe with a new speed and heading (rotational, in rads) columns.
#'
#' @author Marina Papadopoulou \email{m.papadopoulou.rug@@gmail.com}
#'
#' @seealso \code{\link{add_velocities}}
#'
#' @examples
#'
#' data <- data.frame(
#' x = rnorm(25, sd = 3),
#' y = rnorm(25, sd = 3),
#' t = as.POSIXct(1:25, origin = Sys.time()),
#' id = rep(1, 25)
#' )
#'
#' data <- add_set_vels(data, geo = FALSE)
#'
#' @export
add_set_vels <- function(
    data,
    geo = FALSE,
    verbose = FALSE,
    parallelize = FALSE,
    independent_call = TRUE
    ) {

  if (!(all(c("t", "x", "y", "id") %in% colnames(data)))) {
    stop("Input dataframe should include columns: t, x, y, and id.")
    }

  res <- do_add_set_vels(data, geo, verbose, parallelize)

  if (independent_call){
    if (!is.na(res$message)){
      message(paste0("Individuals ",
                     res$message,
                     " have not-moving instances. Their headings have been set to NA for these timesteps."))
    }
    return(res$data)
  }

  res
}


#' @title Call Add Velocity Timeseries
#'
#' @description This function calculates the headings and speeds of individuals based on
#' two location points and the time taken to travel between those points.
#'
#' @param data A dataframe with the time series of individuals' positions.
#' Columns must include: \code{t}, \code{id}, \code{x}, \code{y}.
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
#' @keywords internal
do_add_set_vels <- function(
    data,
    geo = FALSE,
    verbose = FALSE,
    parallelize = FALSE
) {

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

  mess_ids <- NA
  if (length(idx_without_head) > 0){
    mess_ids <- paste(unique(data[idx_without_head,"id"]), collapse = ", ")
    data[idx_without_head ,"head"] <- NA
  }

  list(data = data, message = mess_ids)
}

#' @title Adding Motion Properties in Parallel without Progress Updates
#'
#' @description This function calculates headings and speeds based on
#' two location points and the time taken to travel between those points.
#'
#' @param per_id Dataframe with the time series of individuals' positional data.
#' Columns must include: \code{t}, \code{id}, \code{x}, \code{y}.
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
  res
}


#' @title Adding Individual Velocity Information in Parallel - Verbose
#'
#' @description This function calculates headings and speeds based on
#' two location points and the time taken to travel between those points.
#'
#' @param per_id Dataframe, the time series of individuals' positional data.
#' Columns must include: \code{t}, \code{id}, \code{x}, \code{y}.
#'
#' @param geo Logical, whether positions are geographic coordinates, default = FALSE.
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
  message(paste0("Calculating heading timeseries in parallel for set ", per_id[[1]][1, "set"],
               " with ", length(per_id), " individuals..."))

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
  message("Parallel computation done!")

  names(res) <- NULL
  res
}


#' @title Adding Velocity Information for Individuals in Parallel without Progress Updates
#'
#' @description This function calculates headings and speeds based on
#' two location points and the time taken to travel between those points.
#'
#' @param per_id Dataframe with the time series of individuals' positional data.
#' Columns must include: \code{t}, \code{id}, \code{x}, \code{y}.
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
  res
}



#' @title Speed and Heading Calculation based on \code{swaRm} Package.
#'
#' @description This function calculates headings and speeds based on
#' consecutive locations of 1 individual. Based on the \code{heading} and the
#' \code{linear_speed} functions of \code{swaRm}.
#'
#' @param per_id Time series of 1 individual's positional data.
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
      message(paste0("Id ", per_id[,"id"], " has only one data point, returning NA for its heading.
                     We recommend cleaning your data before continuing."))
      return(per_id)
    }

    if (any(duplicated(per_id$t))){
      warning(paste0("Duplicated times found for individual ",
                     per_id[,'id'],
                     ". Speed calculations may be affected."))
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

    per_id
}
