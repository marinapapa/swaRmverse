#' @title Event indexies
#' @description Returns a vector with event ids that corresponds to the input time vector, according to the continuity of time.
#' @param t a vector of time.
#' @param step2time the sampling frequency of t.
#' @return a vector of the same size as t with event ids
#' @author Marina Papadopoulou \email{m.papadopoulou.rug@@gmail.com}
#' @export
event_ids <- function(t, step2time)
{
  if (!is.numeric(t))
    stop("t (time) should be numeric.")

  st <- t[c(TRUE, diff(t) > step2time)]
  k <- length(st)
  event_idxs <- rep(k, length(t))
  for (i in rev(st)){
    k <- k - 1
    event_idxs[t < i] <- k
  }
  return(event_idxs)
}
