#' @title Global group metrics calculation
#' @description Calculates the average speed, polarization and shape of all groups through time.
#' @param data_dates_list A list of dataframes with groups timeseries per day. Column names must include: id, time, date, headx, heady, posx, posy
#' @param mov_av_time_window Integer, timesteps to use as a sliding window for average speed and polarization. Set NA if calculation is not needed.
#' @param return_df logical, whether to return the result dataframes or not.
#' @param pos_label_x column name of x position column, default = 'posx'.
#' @param pos_label_y column name of y position column, default = 'posy'.
#' @param head_label_x column name of x heading column, default = 'headx'.
#' @param head_label_y column name of y heading column, default = 'heady'.
#' @param date_label column name of date, default = 'date'.
#' @param id_label column name of individual local identifier column, default = 'id'.
#' @param out_csv_dir directory output is saved to.
#' @return a dataframe with a column for neighbor id, bearing angle, distance and heading deviation for each individual through time.
#' @author Marina Papadopoulou \email{m.papadopoulou.rug@@gmail.com}
#' @seealso \code{\link{neighb_rel_pos_timeseries_parallel}}, \code{\link{bearing_angle}}, \code{\link{rad_between}}
#' @export
calc_global_group_metrics <- function(data_dates_list,
                                      mov_av_time_window,
                                      return_df = TRUE,
                                      out_csv_dir = NA,
                                      id_label = 'id',
                                      date_label = 'date',
                                      pos_label_x = 'posx',
                                      pos_label_y = 'posy',
                                      head_label_x = 'headx',
                                      head_label_y = 'heady'
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
  #pb = txtProgressBar(min = 0, max = length(data_dates_list), initial = 0, style = 3)
  pg_i = 1
  for (df in data_dates_list)
  {
    group_prop <- group_metrics_parallel(df,
                                         id_label = id_label,
                                         pos_label_x = pos_label_x,
                                         pos_label_y = pos_label_y,
                                         head_label_x = head_label_x,
                                         head_label_y = head_label_y,
                                         date_label = date_label)

    if (!(is.na(mov_av_time_window)))
    {
      group_prop$speed_av <- moving_average(group_prop$speed, mov_av_time_window)
      group_prop$pol_av <-  moving_average(group_prop$pol, mov_av_time_window)
    }

    toret[[pg_i]] <- group_prop
    pg_i <- pg_i + 1
  }
  toret <- dplyr::bind_rows(toret)

  if (savecsv) {
    data.table::fwrite(toret, paste0(out_csv_dir, '/group_props_', mov_av_time_window , 's.csv'), row.names = FALSE)
  }
  if (return_df){
    return(toret)
  }
  print('Global metric calculations done without returned dataframe.')
}
