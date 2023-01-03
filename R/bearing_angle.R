#' @title Bearing angle
#' @description Calculates the bearing angle from a focal individual (a) to a neighbor (b).
#' @param h_a A 2D vector representing the heading of focal individual.
#' @param pos_a A 2D vector representing the position of the focal individual.
#' @param pos_b A 2D vector representing the position of the neighboring individual.
#' @return a double, the bearing angle from individual a to individual b
#' @author Marina Papadopoulou \email{m.papadopoulou.rug@@gmail.com}
#' @export
bearing_angle <- function(
    h_a,
    pos_a,
    pos_b
)
{
  pos_dif <- pos_b - pos_a
  return(rad_between(h_a, pos_dif))
}
