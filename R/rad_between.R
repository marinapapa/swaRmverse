#' @title Radius between
#' @description Calculates the angle in radius between two vectors.
#' @param a a 2d vector
#' @param b a 2d vector
#' @return a double representing the angle between a and b in rads
#' @author Marina Papadopoulou \email{m.papadopoulou.rug@@gmail.com}
#' @export
rad_between <- function(a, b)
{
  if (!is.vector(a) || !is.vector(b) || length(a) != 2 || length(b) != 2) {
    stop("Input should be two vectors of size 2 (x,y)")}
  c <- perpDot(a, b);
  d <- pracma::dot(a,b);
  return(atan2(c,d));
}
