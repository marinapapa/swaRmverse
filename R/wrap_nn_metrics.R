#' @title Nearest neighbor analysis
#' @description Calculates the bearing angle and distance from a
#' focal individual of a group to its nearest neighbor over time.
#' @param data_list A list of dataframes with groups timeseries per day.
#' Column names must include: id, t, set, head, x, y.
#' @param lonlat logical, whether positions are geographic coordinates,
#' default = FALSE.
#' @param verbose whether to post updates on progress
#' @param parallelize whether to run the function in parallel over individuals,
#'  default = FALSE.
#' @return either a list of dataframes with neighbor ids, bearing angles,
#' distances and heading deviations for each individual through time.
#' @author Marina Papadopoulou \email{m.papadopoulou.rug@@gmail.com}
#' @seealso \code{\link{nn_metrics}}
#' @export
group_nn_metrics <- function(data_list,
                        lonlat = FALSE,
                        verbose = FALSE,
                        parallelize = FALSE
) {
  if (verbose) print("Pairwise analysis started..")

  toret <- lapply(X = data_list,
                  FUN = nn_metrics,
                  add_coords = FALSE,
                  lonlat = lonlat,
                  verbose = verbose,
                  parallelize = parallelize
                  )

  names(toret) <- NULL
  toret <- do.call(rbind, toret)
  return(toret)
}
