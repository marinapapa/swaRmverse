#' @title Number of events
#'
#' @description  Calculates the number of events of collective motion
#'  in a dataset.
#'
#' @param data A dataframe with a keep column
#' (representing which rows are defined as events of collective motion)
#' and a set column.
#'
#' @return an integer with the number of events of collective motion
#' (sequences of keep == TRUE).
#'
#' @author Marina Papadopoulou \email{m.papadopoulou.rug@@gmail.com}
#'
#' @seealso \code{\link{define_events}}
#'
#' @export
events_n <- function(data) {

  if (!("keep" %in% colnames(data))){
    stop("A \'keep\' column for whether or not a timestep is part of an event is necessary. This is added by the define_events function.")
  }

  data <- data[!is.na(data$keep), ]
  splitted_data <- split(data, data$set)
  events_n <- lapply(splitted_data, calc_n_events)
  return(sum(unlist(events_n)))
}

#' @title Calculates the number of events
#' @description Makes the calculation of number of events
#' of collective motion per set, given by \code{events_n}.
#'
#' @param data The dataframe of one set (splitted by the \code{events_n}
#' function) with a keep column.
#'
#' @return an integer, the number of events of collective motion
#' (sequences of keep == TRUE).
#'
#' @author Marina Papadopoulou \email{m.papadopoulou.rug@@gmail.com}
#'
#' @keywords internal
calc_n_events <- function(data) {

  keep_t1 <- data$keep[1:(length(data$keep) - 1)]
  keep_t2 <- data$keep[2:length(data$keep)]

  event_start  <- (keep_t1 != keep_t2) & (keep_t2 == TRUE)
  if (all(keep_t1 == TRUE)) return(1)
  if (keep_t1[1]) event_start[1] <- TRUE
  if (any(event_start == TRUE, na.rm = TRUE)) {
    return(table(event_start)["TRUE"])
  } else {
    return(0)
  }
}

#' @title Total duration of all events
#'
#' @description Calculates the total duration (in seconds) of events of
#' collective motion in a dataset.
#'
#' @param data A dataframe with a keep column,
#' representing which rows are defined as events of collective motion
#' (added by the \code{define_events} function).
#'
#' @param step2time the sampling frequency of the dataframe (how many
#' seconds are between each row of the data).
#'
#' @return a double, the total duration of events in the dataset in seconds.
#'
#' @author Marina Papadopoulou \email{m.papadopoulou.rug@@gmail.com}
#'
#' @seealso \code{\link{define_events}, \link{events_n}}
#'
#' @export
events_dur <- function(data,
                       step2time
                       ) {
  return(nrow(data[data$keep == TRUE, ]) * step2time)
}


#' @title The duration of each event
#'
#' @description Calculates the duration of each event of
#' collective motion in a dataset.
#'
#' @param data A dataframe with an event column (added by \code{get_event_ids}),
#' indicating the event id that each timestep belongs to. Timesteps
#' that are not part of an event should not be included in the data.
#'
#' @param step2time the sampling frequency of the dataframe (how many
#' seconds are between each row of the data).
#'
#' @return a dataframe of two columns, event id and duration in seconds.
#'
#' @author Marina Papadopoulou \email{m.papadopoulou.rug@@gmail.com}
#'
#' @seealso \code{\link{events_dur}, \link{get_event_ids}}
#'
#' @export
calc_dur_per_event <- function(data,
                               step2time
                               ) {

  retdf <- by(data, data$event, function(df) {
    with(df, data.frame(event = event[[1]],
                        event_dur = nrow(df) * step2time
    ))
  })
  do.call(rbind, retdf)
}

#' @title Events summary
#'
#' @description Provides a summary with the number of events and their
#'  total duration in the dataset.
#'
#' @param data A dataframe with a keep column
#' (representing which rows are defined as events of collective motion)
#' and a set column.
#'
#' @param step2time the sampling frequency of the dataframe (how many
#' seconds are between each row of the data).
#'
#' @return a dataframe with 3 columns: set, ev_count (number of events), and
#'  dur (duration of events in seconds).
#'
#' @author Marina Papadopoulou \email{m.papadopoulou.rug@@gmail.com}
#'
#' @seealso \code{\link{events_dur}, \link{events_n}}
#'
#' @export
events_summary <- function(data, step2time) {

  splitted_data <- split(data, data$set)
  evsums <- lapply(splitted_data,
                     function(x, s2t) {
                        d <- events_dur(x, s2t)
                        nev <- events_n(x)
                        return(c(x$set[1], nev, d))
                        },
                        s2t = step2time)

  data.frame(set = unlist(lapply(evsums, function(x) return(x[1]))),
             ev_count = unlist(lapply(evsums, function(x) return(x[2]))),
             dur = unlist(lapply(evsums, function(x) return(x[3]))))
}
