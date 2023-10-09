#' @title Collective motion metrics from raw data
#' @description Calculates metrics of collective motion across sets and events
#' @param data A data frame with time series of individual's positional
#' data through time. Columns must include: id, set, time, posx, posy
#' @param mov_av_time_window to average over (in timesteps)
#' @param step2time the sampling frequency, the relation between a time
#' step and real time in seconds
#' @param lonlat logical, whether positions are geographic coordinates,
#'  default = FALSE.
#' @param verbose whether to post updates on progress
#' @param interactive_mode whether to take the threshold for event
#' definition from user.
#' @param speed_lim threshold for speed if interactive mode id off
#' @param pol_lim threshold for polarization if interactive mode id off
#' @param parallelize_all whether or not to parallelize over ids and time.
#' @return either a list of dataframes with neighbor ids, bearing angles,
#' distances and heading deviations for each individual through time,
#' or saves individual csvs per day, depending on input.
#' @author Marina Papadopoulou \email{m.papadopoulou.rug@@gmail.com}
#' @seealso \code{\link{group_vels}}, \code{\link{nn_metrics}}
#' @export
col_motion_metrics_from_raw <- function(data,
                               mov_av_time_window,
                               step2time = 1,
                               lonlat = TRUE,
                               verbose = TRUE,
                               interactive_mode = TRUE,
                               speed_lim = NA,
                               pol_lim = NA,
                               parallelize_all = FALSE
                               ) {
  sets_dfs <- group_vels(data,
                         lonlat = lonlat,
                         verbose = verbose,
                         parallelize = parallelize_all
                         )

  gm_all <- vector("list", length = length(sets_dfs))
  nn_all <- vector("list", length = length(sets_dfs))
  k <- 1
  for (adf in sets_dfs) {
    nn_m <- nn_metrics(adf,
                      add_coords = FALSE,
                      lonlat = lonlat,
                      verbose = verbose,
                      parallelize = parallelize_all
                      )
    gl_m <- global_metrics(adf, lonlat, parallelize = parallelize_all)

    gl_m$speed_av <- moving_average(gl_m$speed, mov_av_time_window)
    gl_m$pol_av <-  moving_average(gl_m$pol, mov_av_time_window)

    gm_all[[k]] <- gl_m
    nn_all[[k]] <- nn_m[, c("set", "t", "nn_id", "nnd", "bangl")]
    k <- k + 1
  }
  names(gm_all) <- names(nn_all) <- NULL
  gm_all <- do.call(rbind, gm_all)
  nn_all <- do.call(rbind, nn_all)

  sp_lim <- pick_events_threshold(gm_all$speed_av,
                                  "speed",
                                  interactive_mode,
                                  speed_lim)
  pl_lim <- pick_events_threshold(gm_all$pol_av,
                                  "pol",
                                  interactive_mode,
                                  pol_lim)

  gm_all <- define_events(gm_all,
                          sp_lim = sp_lim,
                          pol_lim = pl_lim,
                          step2time = step2time
                          )

  gm_all <- gm_all[!is.na(gm_all$keep), ]
  gm_all <- gm_all[gm_all$keep, ]

  gm_all$event <- event_ids(gm_all, step2time = step2time)
  nn_all <- nn_all[nn_all$t %in% gm_all$t, ]

  toret <- event_metrics(gm_all, nn_all)
  if (nrow(toret) < 1) {
    toret$event_dur <- numeric(0)
    return(toret)
  }
  event_sum <- calc_dur_per_event(gm_all, step2time)
  toret <- merge(toret, event_sum, all.x = TRUE)
  return(toret)
}

#' @title Collective motion metrics
#' @description Calculates metrics of collective motion across sets and events
#' @param timeseries_data A data frame with time series of individual's
#' positional data through time with nearest neighbor analysis conducted
#' @param global_metrics A data frame with the global metrics timeseries.
#' @param step2time the sampling frequency, the relation between a time step
#' and real time in seconds
#' @param verbose whether to post updates on progress
#' @param interactive_mode whether to take the threshold for
#' event definition from user.
#' @param speed_lim threshold for speed if interactive mode id off
#' @param pol_lim threshold for polarization if interactive mode id off
#' @return a dataframe with metrics of collective motion per event
#' @author Marina Papadopoulou \email{m.papadopoulou.rug@@gmail.com}
#' @seealso \code{\link{moving_average}}
#' @export
col_motion_metrics <- function(timeseries_data,
                               global_metrics,
                               step2time = 1,
                               verbose = TRUE,
                               interactive_mode = TRUE,
                               speed_lim = NA,
                               pol_lim = NA
                               ) {

  sp_lim <- pick_events_threshold(global_metrics$speed_av,
                                  "speed",
                                  interactive_mode,
                                  speed_lim)
  pl_lim <- pick_events_threshold(global_metrics$pol_av,
                                  "pol",
                                  interactive_mode,
                                  pol_lim)

  gm_all <- define_events(global_metrics,
                          sp_lim = sp_lim,
                          pol_lim = pl_lim,
                          step2time = step2time)

  gm_all <- gm_all[!is.na(gm_all$keep), ]
  gm_all <- gm_all[gm_all$keep, ]

  gm_all$event <- event_ids(gm_all, step2time = step2time)
  nn_all <- timeseries_data[timeseries_data$t %in% gm_all$t,
                            c("set", "t", "nn_id", "nnd", "bangl")]

  toret <- event_metrics(gm_all, nn_all)
  if (nrow(toret) < 1) {
    toret$event_dur <- numeric(0)
    return(toret)
  }
  event_sum <- calc_dur_per_event(gm_all, step2time)
  toret <- merge(toret, event_sum, all.x = TRUE)
  return(toret)
}
