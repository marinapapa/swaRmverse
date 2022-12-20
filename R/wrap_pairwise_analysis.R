#' @title Pairwise analysis
#' @description Calculates the bearing angle and distance from a focal individual of a group to all neighbors over time.
#' @param data_dates_list A list of dataframes with groups timeseries per day. Column names must include: id, time, date, headx, heady, posx, posy
#' @param max_n_rank_to_return Max rank of neighbor (e.g. 4th closest) to save/return data for
#' @param return_df logical, whether to return the result dataframes or not.
#' @param lonlat logical, whether positions are geographic coordinates, default = FALSE.
#' @param pos_label_x column name of x position column, default = 'lon'.
#' @param pos_label_y column name of y position column, default = 'lat'.
#' @param head_label_x column name of x heading column, default = 'headx'.
#' @param head_label_y column name of y heading column, default = 'heady'.
#' @param date_label column name of date, default = 'date'.
#' @param out_csv_dir directory output is saved to.
#' @param out_csv_name names of files that will be exported.
#' @param add_coords whether data is converted to geographic coordinates, default = 'FALSE'.
#' @param verbose whether to post updates on progress
#' @return either a list of dataframes with neighbor ids, bearing angles, distances and heading deviations for each individual through time, or saves individual csvs per day, depending on input.
#' @author Marina Papadopoulou \email{m.papadopoulou.rug@@gmail.com}
#' @seealso \code{\link{neighb_rel_pos_timeseries_parallel}}, \code{\link{bearing_angle}}, \code{\link{rad_between}}
#' @export
pairwise_analysis <- function(data_dates_list,
                              max_n_rank_to_return = 4,
                              date_label = 'date',
                              return_df = TRUE,
                              out_csv_dir = NA,
                              out_csv_name = 'pairwise_',
                              add_coords = FALSE,
                              lonlat = FALSE,
                              pos_label_x = 'lon',
                              pos_label_y = 'lat',
                              head_label_x = 'headx',
                              head_label_y = 'heady',
                              verbose = FALSE)
{
  if (verbose) {print('Pairwise analysis started, this may take a while..') }

  toret <- vector('list', length = length(data_dates_list))

  #pb = txtProgressBar(min = 0, max = length(data_files), initial = 0, style = 3)
  pg_i = 1
  for (df in data_dates_list)
  {
    thisdate <- df[1, date_label]
    rel_pos <- neighb_rel_pos_timeseries_parallel(df,
                                                  return_df = return_df,
                                                  out_csv_dir = out_csv_dir,
                                                  out_csv_name = paste0('pairwise_1to', max_n_rank_to_return, '_', thisdate, '.csv'),
                                                  add_coords = add_coords,
                                                  pos_label_x = pos_label_x,
                                                  pos_label_y = pos_label_y,
                                                  head_label_x = head_label_x,
                                                  head_label_y = head_label_y,
                                                  lonlat = lonlat,
                                                  verbose = verbose )
    if (return_df) { toret[[pg_i]] <- rel_pos }
   # setTxtProgressBar(pb, pg_i)
    pg_i <- pg_i + 1
  }
  if (return_df) { return(toret) }
  print('Pairwise analysis done with no return object')
}
