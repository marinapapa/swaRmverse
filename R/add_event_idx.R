#' @title Event indexies
#' @description Returns a vector with event ids that corresponds
#' to the input time vector, according to the continuity of time.
#' @param df A dataframe with a set and t columns
#' for adding event ids.
#' @param step2time the sampling frequency of t.
#' @return a vector of the same size as t with event ids
#' @author Marina Papadopoulou \email{m.papadopoulou.rug@@gmail.com}
#' @export
event_ids <- function(df, step2time) {
  counted_ev <- 0
  sets <- unique(df$set)
  event_idxs <- c()
  for (i in sets){
    counted_ev <- length(unique(event_idxs))
    ev_ids <- event_ids_per_set(df[df$set == i, "t"], step2time)
    ev_ids <- ev_ids + counted_ev
    event_idxs <- c(event_idxs, ev_ids)
  }
  return(event_idxs)
}


#' @title Event indexies per set
#' @description Returns a vector with event ids that corresponds
#' to the input time vector, according to the continuity of time.
#' @param t a vector of time.
#' @param step2time the sampling frequency of t.
#' @return a vector of the same size as t with event ids
#' @author Marina Papadopoulou \email{m.papadopoulou.rug@@gmail.com}
#' @keywords internal
event_ids_per_set <- function(t, step2time) {
  st <- t[c(TRUE, round(diff(t), 3) > step2time)]
  k <- length(st)
  event_idxs <- rep(k, length(t))
  for (i in rev(st)){
    k <- k - 1
    event_idxs[t < i] <- k
  }
  return(event_idxs)
}
