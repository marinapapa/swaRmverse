#' @title Headings
#'
#' @description Given a set of locations defining a trajectory, this function
#'  approximates their instantaneous headings computed as the direction of the
#'  vectors between successive locations along the trajectory.
#'
#' @param x A vector of x (or longitude) coordinates corresponding to a single
#'  trajectory.
#'
#' @param y A vector of y (or latitude) coordinates corresponding to a single
#'  trajectory.
#'
#' @param geo A logical value indicating whether the locations are defined by
#'  geographic coordinates (pairs of longitude/latitude values). Default: FALSE.
#'
#' @return A vector of the same length as x and y corresponding to the
#'  approximated headings along the trajectory.
#'
#' @author Simon Garnier, \email{garnier@@njit.edu}
#'
#' @seealso \code{\link{ang_speed}}, \code{\link{ang_acc}}
#'
#' @examples
#' x <- rnorm(25)
#' y <- rnorm(25, sd = 3)
#' heading(x, y)
#'
#' @export
swaRm_heading <- function (x, y, geo = FALSE)
{
  if (length(x) != length(y))
    stop("x and y should have the same length.")
  if (!is.numeric(x) | !is.numeric(y))
    stop("x and y should be numeric.")
  if (geo) {
    l <- length(x)
    m1 <- cbind(x[1:(l - 1)], y[1:(l - 1)])
    m2 <- cbind(x[2:l], y[2:l])
    dm <- geosphere::bearing(m1, m2) * pi/180
    c(0, ifelse(dm < 0.00001, NA, dm))
  }
  else {
    dx <- diff(x)
    dy <- diff(y)
    dxy <- atan2(dy, dx)
    c(NA, ifelse(dxy < 0.00001, NA, dxy))
  }
}
