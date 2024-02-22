#' @title Event indexes
#'
#' @description Returns a vector with the timeseries of event ids
#' according to the input keep column of the dataframe.
#'
#' @param df A dataframe with a set and a keep column to get the
#' timeseries of event ids. The keep column is added by the
#' \code{define_events} function and represents whether each
#' timestep is part of an event or not (whether it should be kept
#' in for the rest of the analysis). Each set of the dataframe
#' should be ordered in time.
#'
#' @return a vector of the same length as the rows of the input
#' dataframe with the timeseries of event ids.
#'
#' @author Marina Papadopoulou \email{m.papadopoulou.rug@@gmail.com}
#'
#' @seealso \code{\link{define_events}}
#'
#' @export
get_event_ids <- function(df) {

  if (!("keep" %in% colnames(df))){
    stop("A \'keep\' column for whether or not a timestep is part of an event is necessary. This is added by the define_events function.")
  }

  sets <- unique(df$set)
  event_idxs <- c()
  cur_id <- 0
  for (i in sets){
    thiss <- df[df$set == i,]
    ev_ids <- event_ids_per_set(thiss, cur_id)
    cur_id <- max(ev_ids)
    event_idxs <- c(event_idxs, ev_ids)
  }

  return(event_idxs)
}

#' @title Event indexes per set
#'
#' @description The execution of \code{event_ids} per set.
#'
#' @param setdf The dataframe for one set of the data. Should not
#' have discontinuities in time.
#'
#' @param ev_idx The last event index added so far at the dataset
#' (e.g., from a previous set).
#'
#' @return a vector with the timeseries of event indexes for the input
#' set.
#'
#' @author Marina Papadopoulou \email{m.papadopoulou.rug@@gmail.com}
#'
#' @keywords internal
event_ids_per_set <- function(setdf, ev_idx) {

  keep_t1 <- setdf$keep[1:(length(setdf$keep) - 1)]
  keep_t2 <- setdf$keep[2:length(setdf$keep)]

  event_start  <- (keep_t1 != keep_t2) & (keep_t2 == TRUE)
  event_ids <- c(rep(ev_idx, nrow(setdf)))

  if (all(keep_t1 == TRUE)) return(c(rep(ev_idx+1, nrow(setdf))))
  if (keep_t1[1]) event_start[1] <- TRUE

  for (i in 1:length(event_start)){
    if (event_start[i]) {ev_idx <- ev_idx + 1}
    event_ids[i] <- ev_idx
  }
  event_ids[i+1] <- ev_idx # last timestep
  return(event_ids)
}