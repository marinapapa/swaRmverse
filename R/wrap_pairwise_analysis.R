#' @title Nearest neighbor analysis
#' @description Calculates the bearing angle and distance from a focal individual of a group to its nearest neighbor over time.
#' @param data_list A list of dataframes with groups timeseries per day. Column names must include: id, t, set, head, x, y
#' @param lonlat logical, whether positions are geographic coordinates, default = FALSE.
#' @param out_csv_dir a path to save the results of each day as a separate csv file. If NA (default), no csv is created and resutls are returned.
#' @param verbose whether to post updates on progress
#' @return either a list of dataframes with neighbor ids, bearing angles, distances and heading deviations for each individual through time.
#' @author Marina Papadopoulou \email{m.papadopoulou.rug@@gmail.com}
#' @seealso \code{\link{nn_rel_pos_timeseries_parallel}}
#' @export
nn_analysis <- function(data_list,
                        lonlat = FALSE,
                        verbose = FALSE,
                        out_csv_dir = NA)
{
  if (verbose) {print('Pairwise analysis started, this may take a while..') }
  savecsvs <- FALSE

  if (!is.na(out_csv_dir)) {
    if (dir.exists(out_csv_dir)) {
      savecsvs <- TRUE
    } else { stop('Input saving directory does not exist, check your out_csv_dir variable.') }
  }

  toret <- vector('list', length = length(data_list))

  i <- 1
  for (df in data_list)
  {
    toret[[i]] <- nn_rel_pos_timeseries_parallel(df,
                                                  add_coords = FALSE,
                                                  lonlat = lonlat,
                                                  verbose = verbose)
    if (savecsvs) {
      thisset <- df$set[1]
      utils::write.csv(toret[[i]], paste0(out_csv_dir, '/nn_data_', thisset , '.csv'), row.names = FALSE)
    }
    i <- i + 1
  }
  names(toret) <- NULL
  toret <- do.call(rbind, toret)
  return(toret)
}
