#' @title Nearest neighbor analysis
#' @description Calculates the bearing angle and distance from a focal individual of a group to its nearest neighbor over time.
#' @param data_dates_list A list of dataframes with groups timeseries per day. Column names must include: id, time, date, headx, heady, posx, posy
#' @param lonlat logical, whether positions are geographic coordinates, default = FALSE.
#' @param verbose whether to post updates on progress
#' @return either a list of dataframes with neighbor ids, bearing angles, distances and heading deviations for each individual through time.
#' @author Marina Papadopoulou \email{m.papadopoulou.rug@@gmail.com}
#' @seealso \code{\link{nn_rel_pos_timeseries_parallel}}
#' @export
nn_analysis <- function(data_dates_list,
                              lonlat = FALSE,
                              verbose = FALSE)
{
  if (verbose) {print('Pairwise analysis started, this may take a while..') }

  toret <- vector('list', length = length(data_dates_list))

  i <- 1
  for (df in data_dates_list)
  {
    thisdate <- df$date[1]
    toret[[i]] <- nn_rel_pos_timeseries_parallel(df,
                                                  add_coords = FALSE,
                                                  lonlat = lonlat,
                                                  verbose = verbose )
    i <- i + 1
  }
  names(toret) <- NULL
  toret <- do.call(cbind, toret)
  return(toret)

  print('Pairwise analysis done with no return object')
}
