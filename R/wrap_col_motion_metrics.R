#' @title Collective motion metrics from raw data
#' @description Calculates metrics of collective motion across sets and events
#' @param data A data frame with time series of individual's positional
#' data through time. Columns must include: id, set, time, posx, posy
#' @param mov_av_time_window to average over (in timesteps)
#' @param step2time the sampling frequency, the relation between a time
#' step and real time in seconds
#' @param geo logical, whether positions are geographic coordinates,
#'  default = FALSE.
#' @param verbose whether to post updates on progress
#' @param speed_lim threshold for speed if interactive mode id off
#' @param pol_lim threshold for polarization if interactive mode id off
#' @param parallelize_all whether or not to parallelize over ids and time.
#' @param noise_thresh The limit of time difference between consecutive events
#'  to be considered the same event. Default value is 0 (no event merging).
#' @return either a list of dataframes with neighbor ids, bearing angles,
#' distances and heading deviations for each individual through time,
#' or saves individual csvs per day, depending on input.
#' @author Marina Papadopoulou \email{m.papadopoulou.rug@@gmail.com}
#' @seealso \code{\link{group_vels}}, \code{\link{nn_metrics}}
#' @export
col_motion_metrics_from_raw <- function(data,
                               mov_av_time_window,
                               step2time = 1,
                               geo = TRUE,
                               verbose = TRUE,
                               speed_lim = NA,
                               pol_lim = NA,
                               parallelize_all = FALSE,
                               noise_thresh = 0
                               ) {
  sets_dfs <- group_vels(data,
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
    gl_m <- global_metrics(adf, geo, step2time = step2time,
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

#' @title Collective motion metrics
#' @description Calculates metrics of collective motion across sets and events
#' @param timeseries_data A data frame with time series of individual's
#' positional data through time with nearest neighbor analysis conducted
#' @param global_metrics A data frame with the global metrics timeseries.
#' @param step2time the sampling frequency, the relation between a time step
#' and real time in seconds
#' @param verbose whether to post updates on progress
#' @param speed_lim threshold for speed if interactive mode id off
#' @param pol_lim threshold for polarization if interactive mode id off
#' @param noise_thresh The limit of time difference between consecutive events
#'  to be considered the same event. Default value is 0 (no event merging).
#' @return a dataframe with metrics of collective motion per event
#' @author Marina Papadopoulou \email{m.papadopoulou.rug@@gmail.com}
#' @seealso \code{\link{moving_average}}
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

  toret <- event_metrics(gm_all, nn_all)
  if (nrow(toret) < 1) {
    toret$event_dur <- numeric(0)
    return(toret)
  }
  event_sum <- calc_dur_per_event(gm_all, step2time)
  toret <- merge(toret, event_sum, all.x = TRUE)
  return(toret)
}
