#' @title Number of events
#' @description  Calculates the number of events of collective motion in dataset
#' @param data A dataframe with a keep (representing which rows are defined as events of collective motion) and a set column
#' @return an integer with the number of events of colletive motion
#' @author Marina Papadopoulou \email{m.papadopoulou.rug@@gmail.com}
#' @export
calc_events_N <- function(data)
{
  splitted_data <- split(data, data$set)

  events_n <- lapply( splitted_data, function(x)
  {
    keep_t1 <- x$keep[1:(length(x$keep)-1)]
    keep_t2 <- x$keep[2:length(x$keep)]

    event_start  <- (keep_t1 != keep_t2) & (keep_t2 == TRUE)
    if (all(keep_t1 == TRUE)){ return(1)}
    if (any(event_start == TRUE, na.rm = T))
    {
      return(table(event_start)['TRUE'])
    } else {return(0)}

  })
  return(sum(unlist(events_n)))
}

#' @title Events total duration
#' @description Calculates the total duration of events of collective motion in a dataset
#' @param data A dataframe with a keep (representing which rows are defined as events of collective motion) and a set column
#' @param step2time the sampling frequency of the dataframe.
#' @return a double, the total duration of events in the dataset
#' @author Marina Papadopoulou \email{m.papadopoulou.rug@@gmail.com}
#' @export
calc_events_duration <- function(data, step2time)
{
  return(nrow(data[data$keep == TRUE,]) * step2time)
}

#' @title Events duration
#' @description Calculates the duration of each event of collective motion in a dataset
#' @param data A dataframe with a keep (representing which rows are defined as events of collective motion) and a set column
#' @param step2time the sampling frequency of the dataframe.
#' @return a dataframe of duration per event
#' @author Marina Papadopoulou \email{m.papadopoulou.rug@@gmail.com}
#' @export
calc_dur_per_event <- function(data, step2time)
{
  retdf <- by(data, data$event, function(df) {
    with(df, data.frame(event = event[[1]],
                        event_dur = nrow(df) * step2time

    ))
  })
  do.call(rbind, retdf)
}

#' @title Events duration summary
#' @description Provides a summary with the number of events, their start time and duration in the dataset
#' @param data A dataframe with a keep (representing which rows are defined as events of collective motion) and a set column
#' @param step2time the sampling frequency of the dataframe.
#' @return a dataframe with 3 columns: set, ev_count (number of events), dur (duration of events in seconds)
#' @author Marina Papadopoulou \email{m.papadopoulou.rug@@gmail.com}
#' @export
events_dur_summary <- function(data, step2time)
{
  splitted_data <- split(data, data$set)

  events_n <- lapply( splitted_data, function(x, step2time)
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

    return(c(x$tosplit[1], nev, dur))
  },
  step2time = step2time)

  return(data.frame(set = unlist(lapply(events_n, function(x){return(x[1])})),
                    ev_count = unlist(lapply(events_n, function(x){return(x[2])})),
                    dur = unlist(lapply(events_n, function(x){return(x[3])}))))
}
