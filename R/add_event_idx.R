#' @title Add event index
#' @description Adds a column to the input dataframe with event id number according to the continuity of date and time columns.
#' @param df a dataframe with at least 2 columns for date and time. There should be no replication in date and time across rows.
#' @param step2time the sampling frequency of the dataframe.
#' @return the input dataframe with an added 'event' column with the event id.
#' @author Marina Papadopoulou \email{m.papadopoulou.rug@@gmail.com}
#' @export
add_event_idx <- function(df, step2time)
{
  if (!('time' %in% colnames(df)) || !('date' %in% colnames(df))){
    stop('Input dataframe needs columns time and date.')
  }

  if (any(duplicated(paste(df$date, df$time)))){
    warning('More than 1 rows have the same date and time, the return event ids may be faulty, are you sure you are inputing the right dataframe?')
  }

  df$time <- as.numeric(df$time)
  df$date <- as.character(df$date)
  event_n <- 1
  df$event <- NA
  for (i in 2:nrow(df))
  {
    df$event[i-1] <- event_n
    if ((df$date[i-1] != df$date[i]) || (round((df$time[i] - df$time[i-1]),2) > step2time))
    { event_n <- event_n + 1; next}
  }
  df$event[nrow(df)] <- event_n
  return(df)
}
