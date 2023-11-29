#' @title Relative position coordinates
#' @description Calculates the x and y coordinates of a neighbor
#' in the reference frame of each focal individual
#' @param data Dataframe with the bearaing angle and
#' distance of each individual to specific neighbors.
#' Column names must include: id, time.
#' Can be the output of the neighb_rel_pos_timeseries in long_dataframe format.
#' @param focal_heading the heading of the focal individual,
#' default = c(0,1) for plotting neighbor heading north.
#' @return the input dataframe with additional x and y columns
#' @author Marina Papadopoulou \email{m.papadopoulou.rug@@gmail.com}
#' @export
add_rel_pos_coords <- function(data,
                               focal_heading = c(0, 1)
                               ) {
  if (!("bangl" %in% colnames(data)) || !("nnd" %in% colnames(data))) {
    stop("The input dataframe needs to contain a bearing angle and
          a distance column, please check your parameters.")
  }
  x <- cos(data$bangl) * focal_heading[1] -
    sin(data$bangl) * focal_heading[2]
  y <- sin(data$bangl) * focal_heading[1] +
    cos(data$bangl) * focal_heading[2]
  data$nnx <- x * data$nnd
  data$nny <- y * data$nnd

  return(data)
}
