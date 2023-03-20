#' @title Pick thresholds
#' @description Interactive function that prints the quantiles of the
#'  input distribution and asks the user to input the threshold value they want.
#' @param data_distr A numeric vector to pick a threshold for
#' @param var The name of the distribution.
#' @param interactive Whether to ask the user for a threshold or not
#' @param input_threshold Returned in case interactive is off.
#' @return the input value of the user, representing the
#' threshold for the definition of an event.
#' @author Marina Papadopoulou \email{m.papadopoulou.rug@@gmail.com}
#' @export
pick_events_threshold <- function(data_distr,
                                  var,
                                  interactive = TRUE,
                                  input_threshold = NA
                                  ) {
  if (interactive) {
    print(round(stats::quantile(data_distr, seq(0, 1, 0.05), na.rm = TRUE), 2))
    threshold_chosen <- readline(paste0(
      "Given the above quantiles of ", var, ",\n
       please input a threshold value for the events definition\n
       (input should be numeric): "))
    while (is.na(as.numeric(threshold_chosen))) {
      threshold_chosen <- readline("Input not numeric,
       please try again (or type \'abort\' to exit): ")
      if (regexpr(threshold_chosen, "abort", ignore.case = TRUE) == 1) {
        stop("Aborting threshold selection from user.")
      }
    }
    return(as.numeric(threshold_chosen))
  } else {
    return(input_threshold)
  }
}


#' @title Defines events
#' @description Adds a keep TRUE/FALSE column in the input
#' dataframe based on the input thresholds
#' @param df A dataframe with a pol_av and speed_av columns
#' for polarization and speed
#' @param sp_lim The threshold of speed to use for events calculation
#' @param pol_lim The threshold of polarization to use for events calculation
#' @param step2time Sampling frequency, relation between
#' time steps and real time
#' @return the dataframe with a keep column added.
#' Also prints the number and duration of events defined.
#' @author Marina Papadopoulou \email{m.papadopoulou.rug@@gmail.com}
#' @export
define_events <- function(df, sp_lim, pol_lim, step2time) {
  df$keep <- (df$pol_av > pol_lim) & (df$speed_av > sp_lim)
  print("Given thresholds return a total of:")
  print(paste0(events_n(df),
               " events, over ",
               calc_events_duration(df, step2time = step2time) / 60,
               " min.")
       )
  return(df)
}