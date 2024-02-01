#' @title Group velocity timeseries
#' @description Adds velocity and heading calculations to dataframe and
#' transforms time to seconds from start per day.
#' @param data A data frame with time series of individual's positional data
#' through time.
#' @param lonlat logical, whether positions are geographic coordinates,
#'  default = FALSE.
#' @param verbose whether to post updates on progress, default = FALSE.
#' @param parallelize whether to run the function in parallel over individuals,
#'  default = FALSE.
#' @return a list of dataframes, an element per set from the input dataframe
#' with new columns: headx, heady, velx, vely, speed, real_time
#' @author Marina Papadopoulou \email{m.papadopoulou.rug@@gmail.com}
#' @seealso \code{\link{add_vels}}
#' @export
group_vels <- function(data,
                       lonlat = TRUE,
                       verbose = FALSE,
                       parallelize = FALSE
                       ) {
  splitted_data <- split(data, data$set)

  if (verbose) {
    print("Adding velocity info to every set of the dataset..")
  }
  toret <- lapply(X = splitted_data,
                  FUN = add_vels,
                  verbose = verbose,
                  lonlat = lonlat,
                  parallelize = parallelize
  )
  if (verbose) {
    print("Done!")
  }
  return(toret)
}
