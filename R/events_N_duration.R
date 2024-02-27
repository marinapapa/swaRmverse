#' @title Number of Events
#'
#' @description This function calculates the number of events of collective
#'  motion in a dataset.
#'
#' @param data A dataframe with a \code{keep} column (representing which rows
#'  are defined as events of collective motion) and a \code{set} column.
#'
#' @return an integer with the number of events of collective motion (sequences
#'  of \code{keep == TRUE}).
#'
#' @author Marina Papadopoulou \email{m.papadopoulou.rug@@gmail.com}
#'
#' @seealso \code{\link{define_events}}
#'
#' @export
events_n <- function(data) {
  if (!("keep" %in% colnames(data))) {
    stop("A \'keep\' column for whether or not a timestep is part of an event is necessary. This is added by the define_events function.")
  }

  data <- data[!is.na(data$keep), ]
  splitted_data <- split(data, data$set)
  events_n <- lapply(splitted_data, calc_n_events)
  sum(unlist(events_n))
}


#' @title Number of Events (Internal)
#'
#' @description This function calculates the number of events of collective
#'  motion per set, given by \code{\link{events_n}}.
#'
#' @param data The dataframe of one set (splitted by the \code{\link{events_n}}
#'  function) with a \code{keep} column.
#'
#' @return An integer corresponding to the number of events of collective motion
#'  (sequences of \code{keep == TRUE}).
#'
#' @author Marina Papadopoulou \email{m.papadopoulou.rug@@gmail.com}
#'
#' @keywords internal
calc_n_events <- function(data) {
  keep_t1 <- data$keep[1:(length(data$keep) - 1)]
  keep_t2 <- data$keep[2:length(data$keep)]

  event_start <- (keep_t1 != keep_t2) & (keep_t2 == TRUE)
  if (all(keep_t1 == TRUE)) {
    return(1)
  }
  if (keep_t1[1]) event_start[1] <- TRUE
  if (any(event_start == TRUE, na.rm = TRUE)) {
    table(event_start)["TRUE"]
  } else {
    0
  }
}


#' @title Total Duration of All Events
#'
#' @description This function calculates the total duration (in seconds) of
#'  events of collective motion in a dataset.
#'
#' @param data A dataframe with a \code{keep} column, representing which rows
#'  are defined as events of collective motion (added by the
#'  \code{\link{define_events}} function).
#'
#' @param step2time The sampling frequency of the dataframe (how many seconds
#'  are between each row of the data).
#'
#' @return A numeric corresponding to the total duration of events in the
#'  dataset in seconds.
#'
#' @author Marina Papadopoulou \email{m.papadopoulou.rug@@gmail.com}
#'
#' @seealso \code{\link{define_events}}, \code{\link{events_n}}
#'
#' @export
events_dur <- function(data,
                       step2time) {
  nrow(data[data$keep == TRUE, ]) * step2time
}


#' @title Duration of Each Event
#'
#' @description This function calculates the duration of each event of
#'  collective motion in a dataset.
#'
#' @param data A dataframe with an \code{event} column (added by
#'  \code{\link{get_event_ids}}), indicating the event ID that each timestep
#'  belongs to. Timesteps that are not part of an event should not be included
#'  in the data.
#'
#' @param step2time The sampling frequency of the dataframe (how many seconds
#'  are between each row of the data).
#'
#' @return A dataframe with two columns, event ID and duration in seconds.
#'
#' @author Marina Papadopoulou \email{m.papadopoulou.rug@@gmail.com}
#'
#' @seealso \code{\link{events_dur}}, \code{\link{get_event_ids}}
#'
#' @export
calc_dur_per_event <- function(data,
                               step2time) {
  retdf <- by(data, data$event, function(df) {
    with(df, data.frame(
      event = event[[1]],
      event_dur = nrow(df) * step2time
    ))
  })
  do.call(rbind, retdf)
}


#' @title Events Summary
#'
#' @description This function summarizes the number of events and their total
#'  duration in the dataset.
#'
#' @param data A dataframe with a \code{keep} column (representing which rows
#'  are defined as events of collective motion) and a set column.
#'
#' @param step2time The sampling frequency of the dataframe (how many seconds
#'  are between each row of the data).
#'
#' @return A dataframe with 3 columns: \code{set}, \code{ev_count} (number of
#'  events), and \code{dur} (duration of events in seconds).
#'
#' @author Marina Papadopoulou \email{m.papadopoulou.rug@@gmail.com}
#'
#' @seealso \code{\link{events_dur}}, \code{\link{events_n}}
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
    s2t = step2time
  )

  data.frame(
    set = unlist(lapply(evsums, function(x) {
      return(x[1])
    })),
    ev_count = unlist(lapply(evsums, function(x) {
      return(x[2])
    })),
    dur = unlist(lapply(evsums, function(x) {
      return(x[3])
    }))
  )
}
