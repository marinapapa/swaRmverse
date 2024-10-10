#' @title Bearing Angle to Nearest Neighbor
#'
#' @description Given the locations and headings of different objects,
#'  this function determines the angle between the heading of each object
#'  and the position to the nearest neighboring object.
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
#' @author Simon Garnier, \email{garnier@@njit.edu},
#'  Marina Papadopoulou, \email{m.papadopoulou.rug@@gmail.com}
#'
#' @seealso \code{\link{swaRm::pdist}}
#'
#' @examples
#' x <- rnorm(25)
#' y <- rnorm(25, sd = 3)
#' hs <- rnorm(25, sd = 1)
#' nnba(x, y, hs)
#'
#' @export
nnba <- function(x, y, hs, geo = FALSE) {
  if (!all(length(x) == c(length(y), length(hs))))
    stop("x, y and hs should have the same length.")

  if (!is.numeric(x) || !is.numeric(y) || !is.numeric(hs))
    stop("x, y and hs should be numeric.")

  d <- swaRm::pdist(x, y, geo = geo)
  diag(d) <- NA
  d[is.na(x) | is.na(y), ] <- NA
  d[, is.na(x) | is.na(y)] <- NA
  idx <- apply(d, 2, function(x) {
    if (sum(is.na(x)) != length(x)) {
      which(x == min(x, na.rm = TRUE))[1]
    } else {
      as.numeric(NA)
    }
  })

  if (geo) {
      m1 <- cbind(x, y)
      m2 <- cbind(x[idx], y[idx])
      br <- geosphere::bearing(m1, m2) * pi / 180
  } else {
      dy <- y[idx] - y
      dx <- x[idx] - x
      br <- atan2(y = dy, x = dx)
  }
  db <- hs - br

  db[db <= (-pi) & !is.na(db)] <- 2 * pi + db[db <= (-pi) & !is.na(db)]
  db[db > pi & !is.na(db)] <- db[db > pi & !is.na(db)] - 2 * pi

  db
}

