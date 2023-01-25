#' @title Collective motion metrics from raw data
#' @description Calculates metrics of collective motion across dates and events
#' @param data A data frame with time series of individual's positional data through time. Columns must include: id, date, time, posx, posy
#' @param mov_av_time_window to average over (in timesteps)
#' @param step2time the sampling frequency, the relation between a time step and real time in seconds
#' @param lonlat logical, whether positions are geographic coordinates, default = FALSE.
#' @param verbose whether to post updates on progress
#' @param interactive_mode whether to take the threshold for event definition from user.
#' @param speed_lim threshold for speed if interactive mode id off
#' @param pol_lim threshold for polarization if interactive mode id off
#' @return either a list of dataframes with neighbor ids, bearing angles, distances and heading deviations for each individual through time, or saves individual csvs per day, depending on input.
#' @author Marina Papadopoulou \email{m.papadopoulou.rug@@gmail.com}
#' @seealso \code{\link{group_motion_timeseries}}, \code{\link{group_metrics_parallel}}
#' @export
col_motion_metrics_from_raw <- function(data,
                               mov_av_time_window,
                               step2time = 1,
                               lonlat = TRUE,
                               verbose = TRUE,
                               interactive_mode = TRUE,
                               speed_lim = NA,
                               pol_lim = NA
                               )
{
  date_dfs <- group_motion_timeseries(data,
                                      lonlat = lonlat,
                                      verbose = verbose
                                      )

  allgroup_props <- vector('list', length = length(date_dfs))
  allrel_pos <- vector('list', length = length(date_dfs))
  k <- 1
  for (adf in date_dfs)
  {
    rel_pos <- nn_rel_pos_timeseries_parallel(adf,
                                              add_coords = FALSE,
                                              lonlat = lonlat
    )

    group_prop <- group_metrics_parallel(adf)

    group_prop$speed_av <- moving_average(group_prop$speed, mov_av_time_window)
    group_prop$pol_av <-  moving_average(group_prop$pol, mov_av_time_window)
    allgroup_props[[k]] <- group_prop
    allrel_pos[[k]] <- rel_pos[, c('date', 'time', 'nnd', 'bangl')]
    k <- k + 1
  }
    allgroup_props <- data.table::rbindlist(allgroup_props)
    allrel_pos <- data.table::rbindlist(allrel_pos)

    sp_lim <- pick_events_threshold(allgroup_props$speed_av, 'speed', interactive_mode, speed_lim)
    pol_lim <- pick_events_threshold(allgroup_props$pol_av, 'pol', interactive_mode, pol_lim)

    allgroup_props <- define_events(allgroup_props, sp_lim = sp_lim, pol_lim = pol_lim, step2time = step2time)
    allgroup_props <- allgroup_props[!is.na(allgroup_props$keep),]
    allgroup_props <- allgroup_props[allgroup_props$keep,]

    allgroup_props <- add_event_idx(allgroup_props, step2time = step2time)
    times_in <- paste(allgroup_props$date, allgroup_props$time)

    allrel_pos <- allrel_pos[paste(allrel_pos$date, allrel_pos$time) %in% times_in,]

    toret <- calc_metrics_per_event(allgroup_props, allrel_pos)
    return(as.data.frame(toret))
}

#' @title Collective motion metrics
#' @description Calculates metrics of collective motion across dates and events
#' @param timeseries_data A data frame with time series of individual's positional data through time with nearest neighbor analysis conducted
#' @param global_metrics A data frame with the global metrics timeseries.
#' @param mov_av_time_window to average over (in timesteps)
#' @param step2time the sampling frequency, the relation between a time step and real time in seconds
#' @param verbose whether to post updates on progress
#' @param interactive_mode whether to take the threshold for event definition from user.
#' @param speed_lim threshold for speed if interactive mode id off
#' @param pol_lim threshold for polarization if interactive mode id off
#' @return a dataframe with metrics of collective motion per event
#' @author Marina Papadopoulou \email{m.papadopoulou.rug@@gmail.com}
#' @seealso \code{\link{group_motion_timeseries}}, \code{\link{group_metrics_parallel}}
#' @export
col_motion_metrics <- function(timeseries_data,
                               global_metrics,
                               mov_av_time_window,
                               step2time = 1,
                               verbose = TRUE,
                               interactive_mode = TRUE,
                               speed_lim = NA,
                               pol_lim = NA
)
{
  alldates <- unique(global_metrics$date)
  allgroup_props <- vector('list', length = length(alldates))
  k <- 1
  for (adf in alldates)
  {
    a_group_metrics <- global_metrics[global_metrics$date == adf, ]
    a_group_metrics$speed_av <- moving_average(a_group_metrics$speed, mov_av_time_window)
    a_group_metrics$pol_av <-  moving_average(a_group_metrics$pol, mov_av_time_window)

    allgroup_props[[k]] <- a_group_metrics
    k <- k + 1
  }

  allgroup_props <- data.table::rbindlist(allgroup_props)
  allgroup_props <- as.data.frame(allgroup_props)
  sp_lim <- pick_events_threshold(allgroup_props$speed_av, 'speed', interactive_mode, speed_lim)
  pol_lim <- pick_events_threshold(allgroup_props$pol_av, 'pol', interactive_mode, pol_lim)


  allgroup_props <- define_events(allgroup_props, sp_lim = sp_lim, pol_lim = pol_lim, step2time = step2time)
  allgroup_props <- allgroup_props[!is.na(allgroup_props$keep),]
  allgroup_props <- allgroup_props[allgroup_props$keep,]

  allgroup_props <- add_event_idx(allgroup_props, step2time = step2time)
  times_in <- paste(allgroup_props$date, allgroup_props$time)

  paiwise_data <- timeseries_data[paste(timeseries_data$date, timeseries_data$time) %in% times_in,
                                  c('date', 'time', 'nnd', 'bangl')]

  toret <- calc_metrics_per_event(allgroup_props, paiwise_data)
  event_sum <- calc_dur_per_event(allgroup_props, step2time)

  toret <- dplyr::left_join(toret, event_sum)
  return(as.data.frame(toret))
}

