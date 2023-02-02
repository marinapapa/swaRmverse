#' @title Linear speeds along a trajectory (from swaRm) in seconds
#'
#' @description TOADJUST Given a set of cartesian coordinates representing an object's
#'  trajectory, this function computes the linear speeds between each pair of
#'  successive locations along the trajectory.
#'
#' @param x A vector of x (or longitude) coordinates corresponding to a single
#'  animal trajectory.
#'
#' @param y A vector of y (or latitude) coordinates corresponding to a single
#'  animal trajectory.
#'
#' @param t A vector of time in seconds corresponding to a single animal trajectory.
#'
#' @param geo A logical value indicating whether the locations are defined by
#'  geographic coordinates (pairs of longitude/latitude values). Default: FALSE.
#'
#' @return A vector of the same length as x and y corresponding to the linear
#'  speeds between each pair of successive locations along the trajectory.
#'
#' @author Simon Garnier, \email{garnier@@njit.edu}, Marina Papadopoulou
#'
#' @examples
#' # TODO
#'
#' @export
linSpeed_s <- function(x, y, t, geo = FALSE) {
  if (!all(length(x) == c(length(y), length(t))))
    stop("x, y and time should have the same length.")

  if (!is.numeric(x) | !is.numeric(y) | !is.numeric(t))
    stop("x and y should be numeric.")

  dt <- diff(t)
  if (any(dt == 0)) {stop('Not unique timesteps as input.')}
  dp <- swaRm::linear_dist(x, y, geo = geo)
  c(NA, dp[2:length(dp)] / dt)
}
