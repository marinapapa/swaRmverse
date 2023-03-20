#' @title Number of events
#' @description  Calculates the number of events of collective motion in dataset
#' @param data A dataframe with a keep column
#' (representing which rows are defined as events of collective motion)
#' and a set column (by default representing date).
#' @return an integer with the number of events of colletive motion
#' @author Marina Papadopoulou \email{m.papadopoulou.rug@@gmail.com}
#' @export
events_n <- function(data) {
  data <- data[!is.na(data$keep), ]
  splitted_data <- split(data, data$set)
  events_n <- lapply(splitted_data, calc_n_events)
  return(sum(unlist(events_n)))
}

#' @title Caculates the number of events
#' @param data A dataframe with a keep column.
#' @return an integer with the number of events of colletive motion
#' @author Marina Papadopoulou \email{m.papadopoulou.rug@@gmail.com}
#' @keywords internal
calc_n_events <- function(df) {
  keep_t1 <- df$keep[1:(length(df$keep) - 1)]
  keep_t2 <- df$keep[2:length(df$keep)]

  event_start  <- (keep_t1 != keep_t2) & (keep_t2 == TRUE)
  if (all(keep_t1 == TRUE)) return(1)
  if (keep_t1[1]) event_start[1] <- TRUE
  if (any(event_start == TRUE, na.rm = TRUE)) {
    return(table(event_start)["TRUE"])
  } else {
    return(0)
  }
}

#' @title Events total duration
#' @description Calculates the total duration of events of
#' collective motion in a dataset.
#' @param data A dataframe with a keep column
#' (representing which rows are defined as events of collective motion)
#' and a set column.
#' @param step2time the sampling frequency of the dataframe.
#' @return a double, the total duration of events in the dataset
#' @author Marina Papadopoulou \email{m.papadopoulou.rug@@gmail.com}
#' @export
calc_events_duration <- function(data, step2time) {
  return(nrow(data[data$keep == TRUE, ]) * step2time)
}

#' @title Events duration
#' @description Calculates the duration of each event of
#' collective motion in a dataset.
#' @param data A dataframe with a keep column
#' (representing which rows are defined as events of collective motion)
#' and a set column.
#' @param step2time the sampling frequency of the dataframe.
#' @return a dataframe of duration per event
#' @author Marina Papadopoulou \email{m.papadopoulou.rug@@gmail.com}
#' @export
calc_dur_per_event <- function(data, step2time) {
  retdf <- by(data, data$event, function(df) {
    with(df, data.frame(event = event[[1]],
                        event_dur = nrow(df) * step2time
    ))
  })
  do.call(rbind, retdf)
}

#' @title Events summary
#' @description Provides a summary with the number of events,
#'  their start time and duration in the dataset.
#' @param data A dataframe with a keep column
#' (representing which rows are defined as events of collective motion)
#' and a set column.
#' @param step2time the sampling frequency of the dataframe.
#' @return a dataframe with 3 columns: set, ev_count (number of events), and
#'  dur (duration of events in seconds).
#' @author Marina Papadopoulou \email{m.papadopoulou.rug@@gmail.com}
#' @export
events_summary <- function(data, step2time) {

  splitted_data <- split(data, data$set)
  events_n <- lapply(splitted_data,
                     function(x, step2time) {
                        keep_t1 <- x$keep[1:(length(x$keep) - 1)]
                        keep_t2 <- x$keep[2:length(x$keep)]
                        dur <- nrow(x[x$keep == TRUE, ]) * step2time
                        event_start  <- (keep_t1 != keep_t2) & (keep_t2 == TRUE)
                        if (all(keep_t1 == TRUE)) nev <- 1
                        if (any(event_start == TRUE, na.rm = TRUE)) {
                          nev <- table(event_start)["TRUE"]
                        } else {
                           if (dur > 0) {
                            nev <- 1
                           } else {
                             nev <- 0
                             }
                          }
                         return(c(x$set[1], nev, dur))
                        },
                        step2time = step2time)

  data.frame(set = unlist(lapply(events_n, function(x) return(x[1]))),
             ev_count = unlist(lapply(events_n, function(x) return(x[2]))),
             dur = unlist(lapply(events_n, function(x) return(x[3]))))
}
