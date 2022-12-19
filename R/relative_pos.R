#' @title Relative position coordinates
#' @description Calculates the x and y coordinates of a neighbor in the reference frame of each focal individual
#' @param data Dataframe with the bearaing angle and distance of each individual to specific neighbors.
#' Column names must include: id, time. Can be the output of the neighb_rel_pos_timeseries in long_dataframe format.
#' @param bearing_angle_label column name of bearing angle of neighbor, default = 'bangl'.
#' @param distance_label column name of distance to neighbor, default = 'dist'.
#' @param focal_heading the heading of the focal individual, default = c(0,1) for plotting neighbor heading north.
#' @return the input dataframe with additional x and y columns
#' @author Marina Papadopoulou \email{m.papadopoulou.rug@@gmail.com}
#' @seealso \code{\link{bearing_angle}}, \code{\link{perpDot}}
#' @export
add_rel_pos_coords <- function(
    data,
    bearing_angle_label = 'bangl',
    distance_label = 'dist',
    focal_heading = c(0,1)
    )
{
  if (!(bearing_angle_label %in% colnames(data)) || !(distance_label %in% colnames(data)) )
  {
    stop('The input dataframe needs to contain a bearing angle and a distance column, please check your parameters.')
  }
  x <- cos(data[,bearing_angle_label]) * focal_heading[1] -
    sin(data[,bearing_angle_label]) * focal_heading[2]
  y <- sin(data[,bearing_angle_label]) * focal_heading[1] +
    cos(data[,bearing_angle_label]) * focal_heading[2]
  data$x <- x * data[,distance_label]
  data$y <- y * data[,distance_label]

  return(data)
}

