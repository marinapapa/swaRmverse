#' @title Calculate metrics of collective motion
#' @description Calculates metrics of collective motion across sets and events
#' @param global_df A data frame with time series of global group measurements.
#' Columns must include:  set, t, event, pol, shape, speed
#' @param pairwise_df A data frame with time series of pairwise measurements.
#'  Columns must include:  set, t, id, dist, bangl
#' @return A dataframe with 10 metrics per event
#' @author Marina Papadopoulou \email{m.papadopoulou.rug@@gmail.com}
#' @seealso \code{\link{global_metrics}}
#' @export
event_metrics <- function(global_df, pairwise_df) {

  if (!(all(c("pol", "speed", "shape", "nnd", "bangl", "set", "t", "event")
        %in% c(colnames(global_df), colnames(pairwise_df))))) {
          stop("Input dataframes should include the following columns:
               'pol', 'speed', 'shape', 'nnd', 'bangl', 'set, 't', 'event'")
    }

  if (nrow(global_df) < 1) {
    print("No collective motion events identified with given thresholds.")
    empt_ret <- as.data.frame(matrix(NA, ncol = 13, nrow = 0))
    colnames(empt_ret) <- c("event", "group_size", "set", "mean_mean_nnd",
                            "mean_sd_nnd", "sd_mean_nnd", "mean_pol", "sd_pol",
                            "stdv_speed", "mean_sd_front", "mean_mean_bangl",
                            "mean_shape", "sd_shape")
    return(empt_ret)
  }

  pairwise_df <- calc_pairwise_metrics(pairwise_df)
  retdf <- merge(global_df, pairwise_df, all.x = TRUE)
  retdf <- calc_events_averages(retdf)
  return(retdf)
}

#' @title Calculate pairwise metrics
#' @description Calculating group averages of pairwise measures
#' @param df  data frame with time series of pairwise measurements.
#'  Columns must include:  set, t, id, dist, bangl
#' @return a dataframe with average and sd of pairwise measurement
#' of a group at a single time step.
#' @author Marina Papadopoulou \email{m.papadopoulou.rug@@gmail.com}
#' @keywords internal
calc_pairwise_metrics <- function(df) {
  df$frontness <- frontness(df$bangl)
  df <- by(df,
          list(format(df$t, "%Y-%m-%d %H:%M:%OS2")),
              function(df) {
                with(df,
                      data.frame(
                      set = set[[1]],
                      t = t[[1]],
                      mean_nnd = mean(nnd, na.rm = TRUE),
                      sd_nnd = stats::sd(nnd, na.rm = TRUE),
                      sd_front = stats::sd(frontness, na.rm = TRUE),
                      mean_bangl = mean(abs(bangl), na.rm = TRUE),
                      group_size = nrow(df)
                      )
                    )
                }
          )
  names(df) <- NULL
  df <- do.call(rbind, df)
  return(df)
}

#' @title Calculate events averages
#' @description Calculating all metrics per event
#' @param df data frame with time series of group measurements
#'  Columns must include:  event, set, group_size, mean_nnd,
#'  sd_nnd, pol, speed, sd_front, mean_bangl, and shape.
#' @return a dataframe with a metric value per event.
#' @author Marina Papadopoulou \email{m.papadopoulou.rug@@gmail.com}
#' @keywords internal
calc_events_averages <- function(df) {

  df <- by(df, df$event, function(df) {
    with(df,
         data.frame(event = event[[1]],
                    group_size = min(group_size, na.rm = TRUE),
                    set = set[[1]],
                    mean_mean_nnd = mean(mean_nnd, na.rm = TRUE),
                    mean_sd_nnd = mean(sd_nnd, na.rm = TRUE),
                    sd_mean_nnd = stats::sd(mean_nnd, na.rm = TRUE),
                    mean_pol = mean(pol, na.rm = TRUE),
                    sd_pol = stats::sd(pol, na.rm = TRUE),
                    stdv_speed = stats::sd(speed, na.rm = TRUE) /
                                 mean(speed, na.rm = TRUE),
                    mean_sd_front = mean(sd_front, na.rm = TRUE),
                    mean_mean_bangl = mean(mean_bangl, na.rm = TRUE),
                    mean_shape = mean(shape, na.rm = TRUE),
                    sd_shape = stats::sd(shape, na.rm = TRUE)
                    )
    )
  })
  names(df) <- NULL
  df <- do.call(rbind, df)
  return(df)
}
