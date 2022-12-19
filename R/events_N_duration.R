#' @title Number of events
#' @description  Calculates the number of events of collective motion in dataset
#' @param data A dataframe with a keep (representing which rows are defined as events of collective motion) and a date column
#' @param date_label the name of the column with date information
#' @return an integer with the number of events of colletive motion
#' @author Marina Papadopoulou \email{m.papadopoulou.rug@@gmail.com}
#' @export
calc_events_N <- function(data, date_label = 'date')
{
  events_n <- lapply( split(data, data[,date_label]), function(x)
  {
    keep_t1 <- x$keep[1:(length(x$keep)-1)]
    keep_t2 <- x$keep[2:length(x$keep)]

    event_start  <- (keep_t1 != keep_t2) & (keep_t2 == TRUE)
    if (all(keep_t1 == TRUE)){ return(1)}
    if (any(event_start == TRUE, na.rm = T))
    {
      return(table(event_start)['TRUE'])
    } else {return(0) }

  })
  return(sum(unlist(events_n)))
}

#' @title Events duration
#' @description Calculates the total duration of events of collective motion in a dataset
#' @param data A dataframe with a keep (representing which rows are defined as events of collective motion) and a date column
#' @param step2time the sampling frequency of the dataframe.
#' @return a double, the total duration of events in the dataset
#' @author Marina Papadopoulou \email{m.papadopoulou.rug@@gmail.com}
#' @export
calc_events_duration <- function(data, step2time)
{
  return(nrow(data[data$keep == TRUE,]) * step2time)
}

#' @title Events duration summry
#' @description Provides a summary with the number of events, their start time and duration in the dataset
#' @param data A dataframe with a keep (representing which rows are defined as events of collective motion) and a date column
#' @param step2time the sampling frequency of the dataframe.
#' @param date_label the name of the column with date information
#' @return a dataframe with 3 columns: date, ev_count (number of events), dur (duration of events in seconds)
#' @author Marina Papadopoulou \email{m.papadopoulou.rug@@gmail.com}
#' @export
events_dur_summary <- function(data, date_label, step2time)
{
  events_n <- lapply( split(data, data[,date_label]), function(x, step2time, date_label)
  {
    keep_t1 <- x$keep[1:(length(x$keep)-1)]
    keep_t2 <- x$keep[2:length(x$keep)]

    dur <- nrow(x[x$keep == TRUE,]) * step2time

    event_start  <- (keep_t1 != keep_t2) & (keep_t2 == TRUE)
    if (all(keep_t1 == TRUE)){ nev <- 1 }
    if (any(event_start == TRUE, na.rm = T))
    {
      nev <- table(event_start)['TRUE']
    } else {
      if (dur > 0) {
        nev <- 1
      } else{ nev <- 0 }
    }

    return(c(x[1, date_label], nev, dur))
  },
  step2time = step2time,
  date_label = date_label)

  return(data.frame(date = unlist(lapply(events_n, function(x){return(x[1])})),
                    ev_count = unlist(lapply(events_n, function(x){return(x[2])})),
                    dur = unlist(lapply(events_n, function(x){return(x[3])}))))
}
