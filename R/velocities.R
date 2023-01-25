#' @title Velocity calculations in parallel
#' @description Calculates velocity and headings based on two location points and the time taken to travel between those points
#' @param data Time series with individual's positional data through time. Columns must include: id, time, lon, lat.
#' @param sample_step An integer, the step over which to calculate the velocities, in timesteps.
#' @param lonlat whether positions are geographic coordinates, default = FALSE.
#' @param verbose whether to post updates on progress
#' @param step2time the sampling frequency, the relation between a time step and real time in seconds
#' @return a list of velocities over time
#' @author Marina Papadopoulou \email{m.papadopoulou.rug@@gmail.com}
#' @export
add_velocities_parallel <- function(
    data,
    sample_step = 1,
    lonlat = FALSE,
    verbose = FALSE,
    step2time = 1
)
{
  if (sample_step < 1) { stop('sample_step is in timesteps not real time, use an integer larger or equal to 1.')}

  if (verbose) { print('Calculating heading timeseries in parallel...') }
  secs <- sample_step * step2time
  data$headx <- data$heady <- data$velx <- data$vely <- data$speed <- NA_real_

  data$id <- as.character(data$id)
  per_id <- split(data, data$id)

  parallel_per_id <- function(per_id, sample_step, lonlat, secs)
  {
    per_id <- as.data.frame(per_id)

    if (nrow(per_id) < sample_step+1)
    {
      warning('Some ids have not enough data for the input sample step.')
      return(per_id)
    }

    for (i in (sample_step+1):nrow(per_id))
    {
      x0 <- per_id$x[i-sample_step]
      x1 <- per_id$x[i]
      y0 <- per_id$y[i-sample_step]
      y1 <- per_id$y[i]

      dx <- x1-x0
      dy <- y1-y0

      dist_moved <- raster::pointDistance(c(x0, y0), c(x1, y1), lonlat = lonlat)

      if (length(dist_moved) < 1 || is.na(dist_moved) ) {
        per_id[i-sample_step, 'speed'] <- 0
        per_id[i-sample_step, 'headx'] <- per_id[i-sample_step, 'heady'] <- NA
        per_id[i-sample_step, 'velx'] <- per_id[i-sample_step, 'vely'] <- NA
        next
      }

      per_id[i-sample_step, 'speed'] <- dist_moved/secs
      per_id[i-sample_step, 'velx'] <- dx
      per_id[i-sample_step, 'headx'] <- dx/vector_magnitude(dx, dy)
      per_id[i-sample_step, 'vely'] <- dy
      per_id[i-sample_step, 'heady'] <- dy/vector_magnitude(dx, dy)
    }
    return(per_id)
  }

  numCores <- parallel::detectCores()
  cl <- parallel::makeCluster(numCores - 2)

  res <- parallel::parLapply(cl,
                             per_id,
                             parallel_per_id,
                             sample_step = sample_step,
                             lonlat = lonlat,
                             secs = secs

  )

  parallel::stopCluster(cl)
  if (verbose) { print('Parallel computation done, preparing return data...') }

  res <- dplyr::bind_rows(res)

  return(res)
}



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
  dp <- swaRm::linDist(x, y, geo = geo)
  c(NA, dp[2:length(dp)] / dt)
}
