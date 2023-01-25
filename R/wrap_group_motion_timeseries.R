#' @title Group collective motion timeseries
#' @description Adds velocity and heading calculations to dataframe and transforms time to seconds from start per day.
#' @param data A data frame with time series of individual's positional data through time.
#' @param lonlat logical, whether positions are geographic coordinates, default = FALSE.
#' @param verbose whether to post updates on progress
#' @return a list of dataframes, an element per date from the input dataframe with new columns: headx, heady, velx, vely, speed, real_time
#' @author Marina Papadopoulou \email{m.papadopoulou.rug@@gmail.com}
#' @seealso \code{\link{add_velocities_parallel}}, \code{\link{transform_time2secs}}
#' @export
group_motion_timeseries <- function(data,
                                    lonlat = TRUE,
                                    verbose = TRUE
                                    )
{
  # # filter out days that have data for only 1 individual?
  dates_in <- unique(data$date)
  toret <- vector('list', length = length(dates_in))

  if (verbose) {
    print('Going through every day of the dataset:')}
  pg_i = 1
  for (adate in dates_in)
  {
    df <- data[data$date == adate, ]
    df <- padd_motion_properties(df,
                                  verbose = verbose,
                                  lonlat = lonlat)

    # if individuals are not moving we assume same heading
    df <- tidyr::fill(df, head, .direction = "up") # headx and heady added by velocities parallel function

    toret[[pg_i]] <- df
    pg_i <- pg_i + 1
  }
  return(toret)
}

