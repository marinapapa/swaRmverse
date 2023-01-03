#' @title Calculate metrics of collective motion
#' @description Calculates metrics of collective motion across dates and events
#' @param global_df A data frame with time series of global group measurements. Columns must include:  date, time, event, pol, shape, speed
#' @param pairwise_df A data frame with time series of pairwise measurements. Columns must include:  date, time, id, dist, bangl
#' @importFrom dplyr "%>%"
#' @return A dataframe with 10 metrics per event
#' @author Marina Papadopoulou \email{m.papadopoulou.rug@@gmail.com}
#' @seealso \code{\link{neighb_rel_pos_timeseries_parallel}}, \code{\link{group_motion_timeseries}}, \code{\link{group_metrics_parallel}}
#' @export
calc_metrics_per_event <- function(global_df, pairwise_df)
{
  if (!(all(c('pol', 'speed', 'shape', 'nnd', 'bangl', 'date', 'time', 'event') %in% c(colnames(global_df), colnames(pairwise_df)))))
  {stop("Input dataframes should include the following columns: 'pol', 'speed', 'shape', 'nnd', 'bangl', 'date', 'time', 'event' ")}

  pairwise_df$frontness <- calc_frontness(pairwise_df$bangl)
  pairwise_df <- pairwise_df %>%
    dplyr::group_by(date, time) %>%
    dplyr::summarise(mean_nnd = mean(nnd, na.rm = T),
                     sd_nnd = stats::sd(nnd, na.rm = T),
                     sd_front = stats::sd(frontness, na.rm = T),
                     mean_bangl = mean(abs(bangl), na.rm = T),
                     group_size = dplyr::n())

  df <- dplyr::left_join(global_df, pairwise_df)

  df <- df %>%
    dplyr::group_by(event) %>%
    dplyr::summarise(mean_mean_nnd = mean(mean_nnd, na.rm = T),
                     mean_sd_nnd = mean(sd_nnd, na.rm = T),
                     sd_mean_nnd = stats::sd(mean_nnd, na.rm = T),
                     mean_pol = mean(pol, na.rm = T),
                     sd_pol = stats::sd(pol, na.rm = T),
                     stdv_speed = stats::sd(speed, na.rm = T)/mean(speed, na.rm = T),
                     mean_sd_front = mean(sd_front, na.rm = T),
                     mean_mean_bangl = mean(mean_bangl, na.rm = T),
                     mean_shape = mean(shape, na.rm = T),
                     sd_shape = stats::sd(shape, na.rm = T),
                     group_size = min(group_size, na.rm = T))
  return(df)
}
