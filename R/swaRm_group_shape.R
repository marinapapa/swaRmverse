#' @title Bearing Angle to Nearest Neighbor
#'
#' @description Given the locations and headings of different objects, this function
#'  determines the angle between the heading of each object and the position to the nearest neighboring object.
#'
#' @param x A vector of x (or longitude) coordinates.
#'
#' @param y A vector of y (or latitude) coordinates.
#'
#' @param hs A vector of headings (angle in rads).
#'
#' @param geo A logical value indicating whether the locations are defined by
#'  geographic coordinates (pairs of longitude/latitude values). Default: FALSE.
#'
#' @return A vector of the same length as x and y representing the distance to
#'  the nearest neighboring object for each object.
#'
#' @author Simon Garnier, \email{garnier@@njit.edu}, Marina Papadopoulou, \email{m.papadopoulou.rug@@gmail.com}
#'
#' @seealso \code{\link{pdist}}
#'
#' @examples
#' x <- rnorm(25)
#' y <- rnorm(25, sd = 3)
#' hs <- 1:25
#' bearing_angle(x, y, hs)
#'
#' @export
pca_obb <- function(x, y, hs, geo = FALSE) {
  if (!all(length(x) == c(length(y), length(hs))))
    stop("x, y and hs should have the same length.")

  if (!is.numeric(x) | !is.numeric(y) | !is.numeric(hs))
    stop("x, y and hs should be numeric.")

  perim_points <- swaRm::chull_perimeter
  ch <- is_chull(x, y)
  idx <- which(ch > 0)
  xx <- x[idx]
  yy <- y[idx]



}

oblong_dev <- function(hs, obb)
