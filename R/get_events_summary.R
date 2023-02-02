#' @title Get events summary
#' @description Provides a summary of the number and duration of events of collective motion in a dataset.
#' @param df A dataframe, needs to have a keep and an event column
#' @param species_id A string representing the name of the species to add as a column
#' @param step2time the sampling frequency, the relation between a time step and real time in seconds
#' @return a dataframe with event id, event duration in timesteps and in seconds, and the species name
#' @author Marina Papadopoulou \email{m.papadopoulou.rug@@gmail.com}
#' @export
get_events_summary <- function(df,
                               step2time,
                               species_id
                               )
{
  df <- df[!is.na(df$keep),]
  df <- df[df$keep,]

  df$event <- event_ids(df$time, step2time = step2time)

  ## Calculate duration of each event and write summary
  evdurs <- data.frame(event = names(table(df$event)),
                       event_dur = as.numeric(table(df$event)))
  evdurs$species <- species_id
  evdurs$event_dur_s <- evdurs$event_dur * step2time
  return(evdurs)
}
