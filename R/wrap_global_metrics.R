#' @title Global group metrics calculation
#' @description Calculates the average speed, polarization and
#' shape of all groups through time.
#' @param data_list A list of dataframes with groups timeseries per day.
#'  Column names must include: id, time, set, headx, heady, posx, posy
#' @param mov_av_time_window Integer, timesteps to use as a sliding window
#' for average speed and polarization. Set NA if calculation is not needed.
#' @param return_df logical, whether to return the result dataframes or not.
#' @param lonlat logical, whether positions are geographic coordinates,
#' default = FALSE.
#' @param out_csv_dir directory output is saved to.
#' @param parallelize whether or not to parallelize over ids.
#' @return a dataframe with a column for neighbor id, bearing angle,
#' distance and heading deviation for each individual through time.
#' @author Marina Papadopoulou \email{m.papadopoulou.rug@@gmail.com}
#' @seealso \code{\link{global_metrics}}, \code{\link{moving_average}}
#' @export
group_global_metrics <- function(data_list,
                                 mov_av_time_window,
                                 lonlat,
                                 return_df = TRUE,
                                 out_csv_dir = NA,
                                 parallelize = FALSE
                                 ) {
  savecsv <- FALSE
  if (!return_df && is.na(out_csv_dir)) {
    stop("No output selected: either set return_df to TRUE
           or input a valid saving directory and filename.")
  }
  if (!is.na(out_csv_dir)) {
    if (dir.exists(out_csv_dir)) {
      savecsv <- TRUE
    } else {
      stop("Input saving directory does not exist,
           check your out_csv_dir variable.")
     }
  }

toret <- lapply(X = data_list, FUN = function(x, lonlat, par, tw) {
                  gm <- global_metrics(x,
                                       lonlat = lonlat,
                                       parallelize = par)
                  gm$speed_av <- moving_average(gm$speed, tw)
                  gm$pol_av <-  moving_average(gm$pol, tw)
                  return(gm)
                },
                lonlat = lonlat,
                par = parallelize,
                tw = mov_av_time_window)

  names(toret) <- NULL
  toret <- do.call(rbind, toret)

  if (savecsv) {
    utils::write.csv(toret,
                     paste0(out_csv_dir,
                            "/group_props_",
                            mov_av_time_window,
                            "s.csv"), row.names = FALSE)
  }
  if (return_df) {
    return(toret)
  }
  print("Global metric calculations done without returned dataframe.")
}
