#' @title Vector magnitude
#' @description Calculates the magnitude of a 2D vector.
#' @param x the x component of the vector
#' @param y the y component of the vector
#' @return a double, the magnitude of the input vector
#' @author Marina Papadopoulou \email{m.papadopoulou.rug@@gmail.com}
#' @export
vector_magnitude <- function(x, y)
{
  return(sqrt(x*x + y*y))
}
