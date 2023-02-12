#' @title Global group metrics calculation
#' @description Calculates the average speed, polarization and shape of all groups through time.
#' @param data_dates_list A list of dataframes with groups timeseries per day. Column names must include: id, time, date, headx, heady, posx, posy
#' @param mov_av_time_window Integer, timesteps to use as a sliding window for average speed and polarization. Set NA if calculation is not needed.
#' @param return_df logical, whether to return the result dataframes or not.
#' @param lonlat logical, whether positions are geographic coordinates, default = FALSE.
#' @param out_csv_dir directory output is saved to.
#' @return a dataframe with a column for neighbor id, bearing angle, distance and heading deviation for each individual through time.
#' @author Marina Papadopoulou \email{m.papadopoulou.rug@@gmail.com}
#' @seealso \code{\link{group_metrics_parallel}}, \code{\link{moving_average}}
#' @export
calc_global_group_metrics <- function(data_dates_list,
                                      mov_av_time_window,
                                      lonlat,
                                      return_df = TRUE,
                                      out_csv_dir = NA
    )
{
  savecsv <- FALSE
  if (!return_df & is.na(out_csv_dir)){
    stop("No output selected: either set return_df to TRUE or input a valid saving directory and filename.")
  }
  if (!is.na(out_csv_dir)) {
    if (dir.exists(out_csv_dir)) {
      savecsv <- TRUE
    } else { stop('Input saving directory does not exist, check your out_csv_dir variable.') }
  }

  toret <- vector('list', length(data_dates_list))
  pg_i = 1
  for (df in data_dates_list)
  {
    group_prop <- group_metrics_parallel(df, lonlat)

    if (!(is.na(mov_av_time_window)))
    {
      group_prop$speed_av <- moving_average(group_prop$speed, mov_av_time_window)
      group_prop$pol_av <-  moving_average(group_prop$pol, mov_av_time_window)
    }

    toret[[pg_i]] <- group_prop
    pg_i <- pg_i + 1
  }

  names(toret) <- NULL
  toret <- do.call(rbind, toret)

  if (savecsv) {
    utils::write.csv(toret, paste0(out_csv_dir, '/group_props_', mov_av_time_window , 's.csv'), row.names = FALSE)
  }
  if (return_df){
    return(toret)
  }
  print('Global metric calculations done without returned dataframe.')
}
