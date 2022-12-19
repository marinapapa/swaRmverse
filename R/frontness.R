#' @title Frontness
#' @author Marina Papadopoulou
#' @description Calculates how in front or behind the neighbor is based on the bearing angle from the focal individual to the neighbor.
#' @param bangls A vector of bearing angles in rads.
#' @return a vector with doubles, the frontness metric (values 0 to 1) relative to the input vector
#' @author Marina Papadopoulou \email{m.papadopoulou.rug@@gmail.com}
#' @export
calc_frontness <- function(bangls)
{
  if (!is.vector(bangls) || !all(is.numeric(bangls))) {
    stop('Input should be a vector of bearing angles in rads.')}

  if (any(bangls > pi,  na.rm = T)) {
    warning('Bearing angle value larger than pi found, are you sure the angles are in rads?')}

  return(abs(bangls)/pi)
}
