#' @title Perpedicular dot product
#' @description Calculates the perpedicular dot product of 2 vectors
#' @param a a 2d vector
#' @param b a 2d vector
#' @return a double that is the perpedicular dot product of a and b
#' @author Marina Papadopoulou \email{m.papadopoulou.rug@@gmail.com}, Hanno Hildenbrandt
#' @export
perpDot <- function(a, b)
{
  if (!is.vector(a) || !is.vector(b) || length(a) != 2 || length(b) != 2) {stop("Input should be two vectors of size 2 (x,y)")}
  return(a[1] * b[2] - a[2] * b[1]);
}
