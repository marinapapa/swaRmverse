#' @title Normalize data
#'
#' @description Rescales a vector to values between 0 and 1.
#'
#' @param vec A numerical vector to normalize.
#'
#' @return A vector of doubles, the normalized values of the input vector.
#'
#' @author Marina Papadopoulou \email{m.papadopoulou.rug@@gmail.com}
#'
#' @export
normalize_data <- function(vec) {
  vmax <- max(vec, na.rm = TRUE)
  vmin <- min(vec, na.rm = TRUE)
  return((vec - vmin) / (vmax - vmin))
}
