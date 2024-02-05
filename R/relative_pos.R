#' @title Frontness
#'
#' @description Given the bearing angle of an object to another, this function
#'  calculates the frontness, a value that ranges from 0 to 1 and represents how
#'  in front the focal object is from its neighbor.
#'
#' @param bs A vector of bearing angles (in rad) between objects.
#'
#' @return A vector of the same length as bs representing the frontness of
#' a focal object to its neighbor.
#'
#' @author Simon Garnier, \email{garnier@@njit.edu},
#'  Marina Papadopoulou, \email{m.papadopoulou.rug@@gmail.com}
#'
#' @seealso \code{\link{nnba}}
#'
#' @examples
#' bs <- rnorm(25, sd = 1)
#' frontness(bs)
#'
#' @export
frontness <- function(bs) {

  if (!is.numeric(bs))
    stop("bs should be numeric.")

  if (any(abs(bs) > pi,  na.rm = TRUE)) {
    stop("The bearing angle values should range between -pi and pi rad.")
  }
  abs(bs) / pi
}


#' @title Relative position coordinates.
#'
#' @description Calculates the x and y coordinates of a neighbor
#' in the reference frame of a focal individual.
#'
#' @param data Dataframe with the bearing angle and
#' distance of each individual to specific neighbors.
#' Column names must include: bangl, nnd.
#'
#' @param focal_heading the heading of the focal individual,
#' default = c(0,1) for plotting neighbor heading north.
#'
#' @return the input dataframe with additional nnx (nearest neighbor x coordinate)
#'  and nny (nearest neighbor y coordinate) columns.
#'
#' @author Marina Papadopoulou \email{m.papadopoulou.rug@@gmail.com}
#'
#' @export
add_rel_pos_coords <- function(data,
                               focal_heading = c(0, 1)
                               ) {
  if (!("bangl" %in% colnames(data)) || !("nnd" %in% colnames(data))) {
    stop("The input data need to contain a bearing angle (bangl) and
          a distance (nnd) column.")
  }
  x <- cos(data$bangl) * focal_heading[1] -
    sin(data$bangl) * focal_heading[2]
  y <- sin(data$bangl) * focal_heading[1] +
    cos(data$bangl) * focal_heading[2]

  data$nnx <- x * data$nnd
  data$nny <- y * data$nnd

  return(data)
}
