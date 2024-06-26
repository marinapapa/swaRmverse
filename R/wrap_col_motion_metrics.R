#' @title Collective Motion Metrics from Raw Data
#'
#' @description This function calculates metrics of collective motion across sets and events.
#'
#' @param data A data frame with time series of individual's positional
#' data through time. Columns must include: \code{id}, \code{set}, \code{t}, \code{x}, \code{y}.
#'
#' @param mov_av_time_window Numeric, a time window to average over for
#' speed and polarization timeseries (in timesteps).
#'
#' @param step2time Numeric, the sampling frequency of the dateset
#' (the relation between a time step and real time in seconds).
#'
#' @param geo Logical, whether positions are geographic coordinates,
#'  default = FALSE.
#'
#' @param verbose Logical, whether to post updates on progress, default = FALSE.
#'
#' @param speed_lim Numeric, the threshold of speed for the definition of
#' an event. For more info see: \code{\link{pick_threshold}}.
#'
#' @param pol_lim Numeric, the threshold of polarization for the definition of
#' an event. For more info see: \code{\link{pick_threshold}}.
#'
#' @param parallelize_all Logical, whether or not to parallelize over timesteps.
#'
#' @param noise_thresh Numeric, the limit of time difference between consecutive events
#'  to be considered the same event. Default value is 0 (no event merging).
#'
#' @return A dataframe with metrics of collective motion per event.
#'
#' @author Marina Papadopoulou \email{m.papadopoulou.rug@@gmail.com}
#'
#' @seealso \code{\link{add_velocities}, \link{group_metrics}, \link{pairwise_metrics}, \link{moving_average}}
#'
#' @examples
#' data <- data.frame(
#'  set = rep(1, 75),
#'  x = rnorm(75, sd = 3),
#'  y = rnorm(75, sd = 3),
#'  t = as.POSIXct(rep(1:25, 3), origin = Sys.time()),
#'  id = c(rep(1, 25), rep(2, 25), rep(3, 25))
#'  )
#'
#' metrics <- col_motion_metrics_from_raw(data,
#'  mov_av_time_window = 5,
#'  step2time = 1,
#'  geo = FALSE,
#'  speed_lim = 0,
#'  pol_lim = 0,
#'  noise_thresh = 1
#'  )
#'
#' @export
col_motion_metrics_from_raw <- function(data,
                               mov_av_time_window,
                               step2time = 1,
                               geo = FALSE,
                               verbose = FALSE,
                               speed_lim = NA,
                               pol_lim = NA,
                               parallelize_all = FALSE,
                               noise_thresh = 0
                               ) {
  sets_dfs <- add_velocities(data,
                             geo = geo,
                             verbose = verbose,
                             parallelize = parallelize_all
                             )

  gm_all <- vector("list", length = length(sets_dfs))
  nn_all <- vector("list", length = length(sets_dfs))
  k <- 1
  for (adf in sets_dfs) {
    nn_m <- nn_metrics(adf,
                      add_coords = FALSE,
                      geo = geo,
                      verbose = verbose,
                      parallelize = parallelize_all
                      )
    gl_m <- group_metrics(adf, geo, step2time = step2time,
                           parallelize = parallelize_all)

    gl_m$speed_av <- moving_average(gl_m$speed, mov_av_time_window)
    gl_m$pol_av <-  moving_average(gl_m$pol, mov_av_time_window)

    gm_all[[k]] <- gl_m
    nn_all[[k]] <- nn_m[, c("set", "t", "only_time", "nn_id", "nnd", "bangl")]
    k <- k + 1
  }
  names(gm_all) <- names(nn_all) <- NULL
  gm_all <- do.call(rbind, gm_all)
  nn_all <- do.call(rbind, nn_all)

  col_motion_metrics(timeseries_data = nn_all,
                     global_metrics = gm_all,
                     step2time = step2time,
                     verbose = verbose,
                     speed_lim = speed_lim,
                     pol_lim = pol_lim,
                     noise_thresh = noise_thresh
                  )
}


#' @title Collective Motion Metrics
#'
#' @description This function calculates metrics of collective motion across sets
#' and events.
#'
#' @param timeseries_data A data frame with time series of individual's
#' positional data through time with nearest neighbor analysis conducted
#'
#' @param global_metrics A data frame with the global metrics timeseries.
#'
#' @param step2time Numeric, the sampling frequency of the dataset
#' (the relation between a time step and real time in seconds).
#'
#' @param verbose Logical, whether to post updates on progress.
#'
#' @param speed_lim Numeric, the threshold of speed for the definition of
#' an event. For more info see: \code{\link{pick_threshold}}.
#'
#' @param pol_lim Numeric, the threshold of polarization for the definition of
#' an event. For more info see: \code{\link{pick_threshold}}.
#'
#' @param noise_thresh Numeric, the limit of time difference between consecutive
#' events to be considered the same event. Default value is 0 (no event merging).
#'
#' @return A dataframe with metrics of collective motion per event.
#'
#' @author Marina Papadopoulou \email{m.papadopoulou.rug@@gmail.com}
#'
#' @seealso \code{\link{define_events}, \link{group_metrics}, \link{pairwise_metrics}}
#'
#' @examples
#'
#' ## A dataframe with group timeseries
#' g_df <- data.frame(
#'  t = as.POSIXct(1:25, origin = "2024-03-18 14:56:05"),
#'  set = rep(1, 25),
#'  pol = c(rnorm(25)),
#'  pol_av = c(rnorm(25)),
#'  speed = c(rnorm(25)),
#'  speed_av = c(rnorm(25)),
#'  shape = c(rnorm(25)),
#'  event = rep(1, 25),
#'  N = rep(3, 25)
#'  )
#'
#' ## A dataframe with individual timeseries
#' p_df <- data.frame(
#'  t = as.POSIXct(rep(1:25, 3), origin = "2024-03-18 14:56:05"),
#'  set = rep(1, 75),
#'  nnd = c(rnorm(75)),
#'  bangl = runif(75, 0, pi),
#'  id = c(rep(1, 25), rep(2, 25), rep(3, 25)),
#'  nn_id = c(
#'   sample(c(2,3), 25, replace = TRUE),
#'   sample(c(1,3), 25, replace = TRUE),
#'   sample(c(2,1), 25, replace = TRUE)),
#'  event = rep(1, 75)
#' )
#' p_df$only_time <- format(p_df$t, "%H:%M:%OS2")
#'
#' metrics <- col_motion_metrics(
#'  timeseries_data = p_df,
#'  global_metrics = g_df,
#'  step2time = 1,
#'  speed_lim = 0,
#'  pol_lim = 0,
#'  noise_thresh = 1
#' )
#'
#' @export
col_motion_metrics <- function(timeseries_data,
                               global_metrics,
                               step2time = 1,
                               verbose = TRUE,
                               speed_lim = NA,
                               pol_lim = NA,
                               noise_thresh = 0
                               ) {

  sp_lim <- pick_threshold(global_metrics$speed_av,
                                  "speed",
                                  speed_lim)
  pl_lim <- pick_threshold(global_metrics$pol_av,
                                  "pol",
                                  pol_lim)

  gm_all <- define_events(global_metrics,
                          sp_lim = sp_lim,
                          pol_lim = pl_lim,
                          step2time = step2time,
                          noise_thresh = noise_thresh)

  gm_all <- gm_all[!is.na(gm_all$keep), ]

  gm_all$event <- get_event_ids(gm_all)
  gm_all <- gm_all[gm_all$keep, ]

  gm_spl <- split(gm_all, gm_all$set)
  nn_all <- lapply(gm_spl, function(x, ts) {
    x$only_time <- format(x$t, "%H:%M:%OS2")
    ts <- ts[ts$set == x$set[1],]
    ts[ts$only_time %in% x$only_time,
       c("set", "t", "only_time", "nn_id", "nnd", "bangl")]
 },
 ts = timeseries_data)
  nn_all <- do.call(rbind, nn_all)

  message("Calculating metrics for each event, this might take a while...")
  toret <- event_metrics(gm_all, nn_all)
  if (nrow(toret) < 1) {
    toret$event_dur <- numeric(0)
    return(toret)
  }
  event_sum <- calc_dur_per_event(gm_all, step2time)
  toret <- merge(toret, event_sum, all.x = TRUE)

  toret
}
