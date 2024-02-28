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
#' @author Marina Papadopoulou, \email{m.papadopoulou.rug@@gmail.com}
#'
#' @seealso \code{\link{nnba}}
#'
#' @examples
#' bs <- runif(25, max = pi)
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


#' @title Relative Position Coordinates
#'
#' @description This function calculates the x and y coordinates of a neighbor
#' in the reference frame of the focal individual.
#'
#' @param data Dataframe with the bearing angle and
#' distance of each individual to specific neighbors.
#' Column names must include: bangl, nnd.
#'
#' @param focal_heading The heading of the focal individual,
#' default = c(0,1) for plotting neighbor heading north.
#'
#' @return The input dataframe with additional \code{nnx} (nearest neighbor x coordinate)
#'  and \code{nny} (nearest neighbor y coordinate) columns.
#'
#' @author Marina Papadopoulou \email{m.papadopoulou.rug@@gmail.com}
#'
#' @examples
#'
#' data <- data.frame(
#' bangl = runif(25, 0, pi),
#' nnd = runif(25)
#' )
#' data <- add_rel_pos_coords(data)
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

  data
}
