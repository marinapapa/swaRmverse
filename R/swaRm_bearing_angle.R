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
bearing_angle <- function(x, y, hs, geo = FALSE) {
  if (!all(length(x) == c(length(y), length(hs))))
    stop("x, y and hs should have the same length.")

  if (!is.numeric(x) | !is.numeric(y) | !is.numeric(hs))
    stop("x, y and hs should be numeric.")

  d <- swaRm::pdist(x, y, geo = geo)
  diag(d) <- NA
  d[is.na(x) | is.na(y), ] <- NA
  d[, is.na(x) | is.na(y)] <- NA
  idx <- apply(d, 2, function(x) {
    if (sum(is.na(x)) != length(x)) {
      which(x == min(x, na.rm = TRUE))[1]
    }
    else {
      as.numeric(NA)
    }
  })

  if (geo) {
      m1 <- cbind(x, y)
      m2 <- cbind(x[idx], y[idx])
      br <- geosphere::bearing(m1, m2) * pi/180
  } else {
      dx <- y[idx]- y
      dy <- x[idx]- x
      br <- atan2(y = dy, x = dx)
  }
  db <- hs - br

  db[db <= (-pi) & !is.na(db)] <- 2 * pi + db[db <= (-pi) & !is.na(db)]
  db[db > pi & !is.na(db)] <- db[db > pi & !is.na(db)] - 2 * pi

  return(db)
}


#' @title Frontness
#'
#' @description Given the bearing angle of an object to another, this function
#'  calculates the frontness, a value that ranges from 0 to 1 and represents how
#'  in front the focal object is from its neighbor.
#'
#' @param bs A vector of bearing angles (in rad) between objects.
#'
#' @return A vector of the same length as bs representing the frontness of a focal object to
#' its neighbor.
#'
#' @author Simon Garnier, \email{garnier@@njit.edu}, Marina Papadopoulou, \email{m.papadopoulou.rug@@gmail.com}
#'
#' @seealso \code{\link{bearing_angle}}
#'
#' @examples
#' bs <- rnorm(25, sd = 1)
#' frontness(bs)
#'
#' @export
frontness <- function(bs) {

  if (!is.numeric(bs))
    stop("bs should be numeric.")

  if (any(abs(bs) > pi,  na.rm = T)) {
    stop('The bearing angle values should range between -pi and +pi rad.')}

  abs(bs)/pi
}
