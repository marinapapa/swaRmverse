#' @title Event indexies
#' @description Returns a vector with event ids that corresponds
#' to the input time vector, according to the continuity of time.
#' @param df A dataframe with a set and t columns
#' for adding event ids.
#' @return a vector of the same size as t with event ids
#' @author Marina Papadopoulou \email{m.papadopoulou.rug@@gmail.com}
#' @export
event_ids <- function(df) {
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

#' @title Event indexies per set
#' @description Returns a vector with event ids that corresponds
#' to the input time vector, according to the continuity of time.
#' @param t a vector of time.
#' @return a vector of the same size as t with event ids
#' @author Marina Papadopoulou \email{m.papadopoulou.rug@@gmail.com}
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
