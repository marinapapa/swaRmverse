#' @title Global group metrics calculation
#' @description Calculates the average speed, polarization and
#' shape of all groups through time.
#' @param data_list A list of dataframes with groups timeseries per day.
#'  Column names must include: id, time, set, headx, heady, posx, posy
#' @param mov_av_time_window Integer, timesteps to use as a sliding window
#' for average speed and polarization. Set NA if calculation is not needed.
#' @param lonlat logical, whether positions are geographic coordinates,
#' default = FALSE.
#' @param step2time the sampling frequency, the relation between a time
#' step and real time in seconds
#' @param parallelize whether or not to parallelize over ids.
#' @return a dataframe with a column for neighbor id, bearing angle,
#' distance and heading deviation for each individual through time.
#' @author Marina Papadopoulou \email{m.papadopoulou.rug@@gmail.com}
#' @seealso \code{\link{global_metrics}}, \code{\link{moving_average}}
#' @export
group_global_metrics <- function(data_list,
                                 mov_av_time_window,
                                 lonlat,
                                 step2time,
                                 parallelize = FALSE
                                 ) {

toret <- lapply(X = data_list, FUN = function(x, lonlat, par, tw, st2t) {
                  gm <- global_metrics(x,
                                       lonlat = lonlat,
                                       parallelize = par,
                                       step2time = st2t)
                  gm$speed_av <- moving_average(gm$speed, tw)
                  gm$pol_av <-  moving_average(gm$pol, tw)
                  return(gm)
                },
                lonlat = lonlat,
                st2t = step2time,
                par = parallelize,
                tw = mov_av_time_window)

  names(toret) <- NULL
  toret <- do.call(rbind, toret)
  return(toret)
}
