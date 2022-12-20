#' @title Group collective motion timeseries
#' @description Adds velocity and heading calculations to dataframe and transforms time to seconds from start per day.
#' @param data A data frame with time series of individual's positional data through time. Columns must include: id, date, time, posx, posy
#' @param pos_label_x column name of x position column, default = 'posx'.
#' @param pos_label_y column name of y position column, default = 'posy'.
#' @param id_label column name of individual local identifier column, default = 'id'.
#' @param date_label column name for date, default = 'date'.
#' @param step2time the sampling frequency, the relation between a time step and real time in seconds
#' @param speed_sliding_window An integer, the step over which to calculate the velocities, in timesteps.
#' @param lonlat logical, whether positions are geographic coordinates, default = FALSE.
#' @param verbose whether to post updates on progress
#' @return a list of dataframes, an element per date from the input dataframe with new columns: headx, heady, velx, vely, speed, real_time
#' @author Marina Papadopoulou \email{m.papadopoulou.rug@@gmail.com}
#' @seealso \code{\link{add_velocities_parallel}}, \code{\link{transform_time2secs}}
#' @export
group_motion_timeseries <- function(data,
                                    speed_sliding_window = 1,
                                    step2time = 1,
                                    lonlat = TRUE,
                                    date_label = 'date',
                                    pos_label_x = 'posx',
                                    pos_label_y = 'posy',
                                    id_label = 'id',
                                    verbose = TRUE
                                    )
{
  # # filter out days that have data for only 1 individual
  # dates_in <- dsum[dsum$N > 1, 'date']
  dates_in <- unique(data[, date_label])
  toret <- vector('list', length = length(dates_in))

  #pb = txtProgressBar(min = 0, max = length(dates_in), initial = 0, style = 3)
  pg_i = 1
  for (adate in dates_in)
  {
    df <- data[data$date == adate, ]
    start_t <- min(df$time)

    df <- transform_time2secs(df, start_t = start_t)

    df <- add_velocities_parallel(df,
                                  pos_label_x = pos_label_x,
                                  pos_label_y = pos_label_y,
                                  id_label = id_label,
                                  sample_step = speed_sliding_window,
                                  verbose = verbose,
                                  lonlat = lonlat,
                                  step2time = step2time)

    # if individuals are not moving we assume same heading
    df <- tidyr::fill(df, headx, heady, .direction = "up") # headx and heady added by velocities parallel function

    toret[[pg_i]] <- df
    # write.csv(df, paste0(save_dir, date, '.csv'), row.names = FALSE)

    # setTxtProgressBar(pb, pg_i)
    pg_i <- pg_i + 1
  }
  return(toret)
}

