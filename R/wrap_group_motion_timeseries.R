#' @title Group collective motion timeseries
#' @description Adds velocity and heading calculations to dataframe and transforms time to seconds from start per day.
#' @param data A data frame with time series of individual's positional data through time.
#' @param lonlat logical, whether positions are geographic coordinates, default = FALSE.
#' @param verbose whether to post updates on progress
#' @return a list of dataframes, an element per set from the input dataframe with new columns: headx, heady, velx, vely, speed, real_time
#' @author Marina Papadopoulou \email{m.papadopoulou.rug@@gmail.com}
#' @seealso \code{\link{padd_motion_properties}}
#' @export
group_motion_timeseries <- function(data,
                                    lonlat = TRUE,
                                    verbose = TRUE
                                    )
{
  # # filter out days that have data for only 1 individual?
  splitted_data <- split(data, data$set)
  spl_vec <- length(splitted_data)
  toret <- vector('list', length = length(spl_vec))

  if (verbose) {
    print('Going through every set of the dataset:')
  }

  for (pg_i in 1:spl_vec)
  {
    df <- splitted_data[[pg_i]]
    df <- padd_motion_properties(df,
                                  verbose = verbose,
                                  lonlat = lonlat)

    toret[[pg_i]] <- df
  }
  return(toret)
}

