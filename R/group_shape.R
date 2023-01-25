#' @title Make oriented bounding box
#' @author Marina Papadopoulou
#' @description Creates the oriented bounded box of a group in one time step
#' @param df_self_timestep Dataframe with focal individuals data for 1 timestep, needs position columns.
#' @return a bounding box around all input positions
#' @author Marina Papadopoulou \email{m.papadopoulou.rug@@gmail.com}
#' @export
make_obb <- function(
    df_self_timestep
)
{
  xy <- as.matrix(df_self_timestep[,c('x', 'y')], nrow = 2, ncol = 2)
  bb <- shotGroups::getMinBBox(xy)                     # minimum bounding box

  bb$dirx <- sin(pracma::deg2rad(bb$angle))
  bb$diry <- -cos(pracma::deg2rad(bb$angle))

  bb$area <- bb$width * bb$height                     # box area
  return(bb)
}


#' @title Bounding box angle deviation
#' @description Get the minimum deviation of the group's velocity and the bounding box of individual positions
#' @param flock_vel a 2D vector representing the group's velocity at one timestep
#' @param oobb the object oriented bounded box from the positions of all group members
#' @author Marina Papadopoulou \email{m.papadopoulou.rug@@gmail.com}
#' @export
bb_angl_dev <- function(
    flock_vel,
    oobb
)
{
  dev <- min(abs(rad_between(c(flock_vel[1], flock_vel[2]), c(oobb$dirx, oobb$diry))), abs(rad_between(c(flock_vel[1],  flock_vel[2]), c(- oobb$dirx, -oobb$diry))))
  return(dev)
}

#' @title Group shape - Oblong deviation
#' @description Calculates a metric of group shape: the deviation of the longest side of a bounding box around all group member from the groups' heading.
#' @param df_self Dataframe with individual data over time, needs a time and position columns.
#' @return a vector of doubles, the deviation (angle) of the bounding box of a group to its heading over time.
#' @author Marina Papadopoulou \email{m.papadopoulou.rug@@gmail.com}
#' @export
group_shape_oblong_dev <- function(
    df_self
)
{
  tstps <- split(df_self, df_self$time)
  devs <- rep(NA, length(tstps))
  devs <- lapply(tstps, function(tt)
  {
    group_headx <- mean(tt$headx)
    group_heady <-  mean(tt$heady)

    dev <- bb_angl_dev(c(group_headx, group_heady), make_obb(tt))
    return(dev)
  })
  return(unlist(devs))
}
