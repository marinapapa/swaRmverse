#' @title Collective motion metrics
#' @description Calculates metrics of collective motion across dates and events
#' @param data A data frame with time series of individual's positional data through time. Columns must include: id, date, time, posx, posy
#' @param mov_av_time_window to average over (in timesteps)
#' @param pos_label_x column name of x position column, default = 'posx'.
#' @param pos_label_y column name of y position column, default = 'posy'.
#' @param id_label column name of individual local identifier column, default = 'id'.
#' @param date_label column name for date, default = 'date'.
#' @param step2time the sampling frequency, the relation between a time step and real time in seconds
#' @param speed_sliding_window An integer, the step over which to calculate the velocities, in timesteps.
#' @param lonlat logical, whether positions are geographic coordinates, default = FALSE.
#' @param verbose whether to post updates on progress
#' @return either a list of dataframes with neighbor ids, bearing angles, distances and heading deviations for each individual through time, or saves individual csvs per day, depending on input.
#' @author Marina Papadopoulou \email{m.papadopoulou.rug@@gmail.com}
#' @seealso \code{\link{neighb_rel_pos_timeseries_parallel}}, \code{\link{group_motion_timeseries}}, \code{\link{group_metrics_parallel}}
#' @export
col_motion_metrics <- function(data,
                               mov_av_time_window,
                               speed_sliding_window = 1,
                               step2time = 1,
                               lonlat = TRUE,
                               date_label = 'date',
                               pos_label_x = 'posx',
                               pos_label_y = 'posy',
                               id_label = 'id',
                               verbose = TRUE
                               )
{
  date_dfs <- group_motion_timeseries(data,
                                      speed_sliding_window = speed_sliding_window,
                                      step2time = step2time,
                                      lonlat = lonlat,
                                      date_label = date_label,
                                      pos_label_x =pos_label_x,
                                      pos_label_y = pos_label_y,
                                      id_label = id_label,
                                      verbose = verbose
                                      )

  toret <- vector('list', length = length(date_dfs))
  k <- 1
  for (adf in date_dfs)
  {
    rel_pos <- neighb_rel_pos_timeseries_parallel(adf,
                                                  return_df = TRUE,
                                                  out_csv_dir = NA,
                                                  out_csv_name = NA,
                                                  add_coords = FALSE,
                                                  pos_label_x = pos_label_x,
                                                  pos_label_y = pos_label_y,
                                                  head_label_x = 'headx',
                                                  head_label_y = 'heady',
                                                  date_label = date_label,
                                                  lonlat = lonlat,
                                                  verbose = verbose )
    rel_pos <- rel_pos[rel_pos$rank == 1]

    group_prop <- group_metrics_parallel(adf,
                                         id_label = id_label,
                                         pos_label_x = pos_label_x,
                                         pos_label_y = pos_label_y,
                                         date_label = date_label
                                         )

    group_prop$speed_av <- moving_average(group_prop$speed, mov_av_time_window)
    group_prop$pol_av <-  moving_average(group_prop$pol, mov_av_time_window)

    sp_lim <- pick_events_threshold(group_prop$speed_av, 'speed')
    pol_lim <- pick_events_threshold(group_prop$pol_av, 'pol')

    group_prop <- define_events(group_prop, sp_lim == sp_lim, pol_lim = pol_lim)

    # Keep only events in
    group_prop <- group_prop[!is.na(group_prop$keep),]
    group_prop <- group_prop[group_prop$keep,]

    group_prop <- add_event_idx(group_prop, step2time = step2time)
    times_in <- paste(group_prop$date, group_prop$time)
    rel_pos <- rel_pos[paste(rel_pos$date, rel_pos$time) %in% times_in,]

    toret[[k]] <- calc_metrics_per_event(group_prop, rel_pos)
    k <- k + 1
  }
  toret <- data.table::rbindlist(toret)
  return(toret)
}


