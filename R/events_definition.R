#' @title Pick the thresholds for the events' definition
#'
#' @description An interactive function that calculates and prints
#'  the quantiles of the input distribution and asks the user to
#'  input the threshold value they want to keep. If a threshold
#'  is given as input, then the function checks that the threshold type
#'  is correct and returns it. In the swaRmverse framework,
#'  the timesteps with lower values than the thershold will be labelled as not
#'  part of an event.
#'
#' @param data_distr A numeric vector to pick a threshold for. In
#' the package's pipeline it is the timeseries of polarization and
#' average speed of a group.
#'
#' @param var A string, the of the distribution to use at the interactive
#' step to ask the user for input.
#'
#' @param threshold Default NA. If NA, the function runs an interactive
#' mode and the user inputs a given value to return. If numeric, the
#' function just returns this input (interactive case is off).
#'
#' @return the selected or input value of the user for the lower threshold,
#' of the variable to be used for the definition of an event.
#'
#' @author Marina Papadopoulou \email{m.papadopoulou.rug@@gmail.com}
#'
#' @seealso \code{\link{define_events}}
#'
#' @export
pick_threshold <- function(data_distr,
                           var,
                           threshold = NA
                           ) {
  if (length(threshold) > 1){
    stop("The input threshold should be a single value or NA")
  }

  if (is.na(threshold)) {
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
  } else {
    if (is.numeric(threshold)){
      threshold_chosen <- threshold
      } else{
        stop("The input threshold should be numeric or NA")
    }
  }

  if (threshold_chosen > max(data_distr, na.rm = T)){
    warning(paste0("The chosen threshold is larger than the max value of ", var,
            " in the data, no events will be identified."))
  }
  if (threshold_chosen < min(data_distr, na.rm = T)){
    warning(paste0("The chosen threshold is smaller than the min value of ", var,
                   " in the data, each set will consist of a single event."))
  }
  return(as.numeric(threshold_chosen))
}


#' @title Define events of collective motion
#'
#' @description Adds a keep TRUE/FALSE column in the input
#' dataframe based on whether the average speed and
#' polarization of the group is larger than the input
#' thresholds, reflecting whether a timestep is considered
#' part of an event or not.
#'
#' @param df A dataframe with a pol_av and speed_av columns
#' for polarization and speed (calculated by the \code{\link{group_metrics_per_set}}
#' function).
#'
#' @param sp_lim The (lower) threshold of speed to use for defining which
#' timesteps are part of an events of collective motion.
#' In other words, during an event the group should have an average speed of
#' at least sp_lim.
#'
#' @param pol_lim The (lower) threshold of polarization to use for defining which
#' timesteps are part of an events of collective motion.
#' In other words, during an event the group's polarization should be
#' at least pol_lim.
#'
#' @param step2time Sampling frequency, the relation between
#' time steps (rows) in the input dataframe and real time (in seconds).
#'
#' @param noise_thresh The limit of time difference between consecutive events
#'  to be considered the same event. Default value is 0 (no event merging).
#'
#' @return the dataframe that was given as input with an extra \'keep\' column.
#' The function also prints the number and duration of the defined events.
#'
#' @author Marina Papadopoulou \email{m.papadopoulou.rug@@gmail.com}
#'
#' @seealso \code{\link{pick_threshold}, \link{group_metrics_per_set}}
#'
#' @export
define_events <- function(df,
                          sp_lim,
                          pol_lim,
                          step2time,
                          noise_thresh = 0
                          ){

  if (!all(is.numeric(sp_lim) & is.numeric(pol_lim))){
    stop('The input limits for speed and polarization should be numeric.')
  }

  df$keep <- (df$pol_av >= pol_lim) & (df$speed_av >= sp_lim)
  rlengths <- rle(df$keep)
  tofix <- which(rlengths$lengths <= noise_thresh & rlengths$values == FALSE)
  idxs  <- sapply(tofix, function(x, rl) { (sum(rl$lengths[1:(x-1)])+1):sum(rl$lengths[1:x])}, rl = rlengths)
  df$keep[unlist(idxs)] <- TRUE

  print("Given thresholds return a total of:")
  print(paste0(events_n(df),
               " events, over ",
               events_dur(df, step2time = step2time) / 60,
               " min.")
       )
  return(df)
}
