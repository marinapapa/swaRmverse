#' @title Relative position coordinates
#' @description Calculates the x and y coordinates of a neighbor
#' in the reference frame of each focal individual
#' @param data Dataframe with the bearaing angle and
#' distance of each individual to specific neighbors.
#' Column names must include: id, time.
#' Can be the output of the neighb_rel_pos_timeseries in long_dataframe format.
#' @param focal_heading the heading of the focal individual,
#' default = c(0,1) for plotting neighbor heading north.
#' @return the input dataframe with additional x and y columns
#' @author Marina Papadopoulou \email{m.papadopoulou.rug@@gmail.com}
#' @export
add_neighbour_stability <- function(data,
                        M,
                        time_window,
                        step2time,
                        lonlat

){
  n_df <- get_neighbourhoods(data, lonlat = lonlat)

  nstabs <- end_neighbor_stability(neighbours_info = n_df,
                                   M = M,
                                   time_window = time_window,
                                   sample_step = step2time
                                   )
  colnames(nstabs)[1] <- 'only_time'
  data <- dplyr::left_join(data, nstabs[,c(1,2,6)], by = c('id', 'only_time'))
  return(data)
}

#' @description Exports dataframe for one timestep with ids and neighobrs info (rank and distance)
neighbourhoods <- function(thists,
                           lonlat)
{
  thists <- as.data.frame(thists)
  timestep <- as.character(thists$only_time[1])
  id_names <- unique(thists$id)
  def_head <- c('t', 'id')
  if (nrow(thists) <= 1){
    h <- c("t", "id", "neighb", "dist", "rank")
    emptydf <- data.frame(matrix(NA, ncol = length(h), nrow = 0))
    colnames(emptydf) <- h
    return(emptydf)
  }

  df_dists <- data.frame(matrix(NA, ncol = length(def_head) + length(id_names), nrow = length(id_names)))
  df_rank <- data.frame(matrix(NA, ncol = length(def_head) + length(id_names), nrow = length(id_names)))

  colNnames <- rep(NA, length(def_head) + length(id_names))
  colNnames[1:length(def_head)] <- def_head
  cnam_dist <- cnam_rank  <- colNnames

  k <- length(def_head) + 1
  for (i in 1:length(id_names))
  {
    cnam_dist[k] <- paste0('dist2n', id_names[i]);
    cnam_rank[k] <- paste0('rankOfn', id_names[i]);
    k <- k +1
  }
  colnames(df_dists) <- cnam_dist
  colnames(df_rank) <- cnam_rank

  dfrow <- 1
  for (id in id_names)
  {
    posa_x <- thists$x[thists$id == id] # one individual
    posa_y <- thists$y[thists$id == id]

    other_ids <- id_names[id_names != id]
    df_dists[dfrow, 1:length(def_head)] <- df_rank[dfrow, 1:length(def_head)] <- c(timestep, id)
    for (m in other_ids)
    {
      posb_x <- as.numeric(unlist(thists$x[thists$id == m])) # vector of all else
      posb_y <-  as.numeric(unlist(thists$y[thists$id == m]))

      df_dists[dfrow,  paste0('dist2n', m)] <- raster::pointDistance(c(posa_x[1], posa_y[1]), c(posb_x[1], posb_y[1]), lonlat = lonlat)

    }
    if (length(other_ids) < 2) # if a pair, so only 1 neighbor
    {
      df_rank[dfrow,  paste0('rankOfn', other_ids)] <- 1
      dfrow <- dfrow + 1
      next
    }
    t_dists <- unlist(df_dists[dfrow, paste0('dist2n', other_ids)])

    names(t_dists) <- gsub("dist2n","", names(t_dists))
    t_dists <- sort(t_dists, na.last = TRUE)
    for (n in 1:length(t_dists))
    {
      df_rank[dfrow,  paste0('rankOfn', names(t_dists)[n])] <- n
    }
    dfrow <- dfrow + 1
  }

  df_dists <- reshape2::melt(df_dists, id.vars = c("t", "id"))
  df_dists$variable <- gsub("dist2n","", df_dists$variable)
  colnames(df_dists) <- c('t', 'id', 'neighb', 'dist')
  df_rank <- reshape2::melt(df_rank, id.vars = c("t", "id"))
  df_rank$variable <- gsub("rankOfn", "", df_rank$variable)
  colnames(df_rank) <- c('t', 'id', 'neighb', 'rank')

  toreturn <- dplyr::full_join(df_rank, df_dists, by = c("t", "id", "neighb"))
  toreturn <- toreturn[toreturn$id != toreturn$neighb,]
  return(toreturn)
}

#' @description Calls neighbour calculating function for every timestep
get_neighbourhoods <- function(data, lonlat){

  tsdata <- split(data, data$only_time)
  res <- lapply(tsdata, neighbourhoods, lonlat = lonlat)

  res <- purrr::keep(res, function(x) nrow(x) > 0)
  res <- dplyr::bind_rows(res)
  return(res)
}

#' @title Stability of neighbors over time per turn
#' @export
end_neighbor_stability <- function(neighbours_info, time_window, sample_step, M)
{
  cname <- paste0('Q', M,'_',time_window)
  ind_dfs <- split(neighbours_info, neighbours_info$id)

  time_steps <- time_window / sample_step
  # dfres <- data.frame(t = neighbours_info$t, id = neighbours_info$id)
  # dfres[cname] <- NA

  for (k in 1:length(ind_dfs))
  {
    tdf <- ind_dfs[[k]]
    ind_dfs[[k]][cname] <- NA
    time_vec <- unique(tdf$t)
    tend <- length(time_vec) - time_steps - 1
    if (tend < 1) { print("Time window too large for these timeseries."); next}

    for (t0 in 1:tend)
    {
      ind_dfs[[k]][t0, cname] <- neighb_stability_i(tdf, time_vec[t0], time_vec[t0 + time_steps], M)
    }
  }

  res <- purrr::keep(ind_dfs, function(x) nrow(x) > 0)
  res <- dplyr::bind_rows(res)
  return(res)
}


#' @description calculates the neighbor stability timeserie of one individual
neighb_stability_i <- function(ind_timeseries, t_start, t_end, M)
{
  init_neighbs <- get_neighbors_at_t(ind_timeseries, t_start, M)
  end_neighbs <- get_neighbors_at_t(ind_timeseries, t_end, M)

  s_ti <- length(intersect(end_neighbs, init_neighbs)) / length(init_neighbs)

  return(s_ti)
}

#' @description exctract ids of M closest neihbors at time t
get_neighbors_at_t <- function(ind_timeseries, t, M)
{
  thecutdf <- ind_timeseries[ind_timeseries$t == t, ]
  Mi <- as.vector(unlist(thecutdf[order(thecutdf$rank), 'neighb']))
  return(Mi[1:min(M, length(Mi))])
}

#' #' @description check whether the input of neighbor stability estimations is correct
#' check_stability_input <- function(timeseries, t0, t1, M)
#' {
#'   if (!(t0 %in% timeseries$time) |
#'       !(t1 %in% timeseries$time)
#'   )
#'   {
#'     stop('Given time point(s) not included
#'          in the timeseries dataframe.')
#'   }
#'   if (t1 < t0) { stop('Ending input time point is before the starting one.') }
#'   if (M < 1) { stop('Number of neighbors (M) should be a positive integer.')}
#'
#'   if (M > max(timeseries$n_i))
#'   {
#'     warning(paste0('Timeseries contain less than ', M ,' neighbors. Stability is being calculated for ' ,max(timeseries$n_i), '.'))
#'   }
#'
#' }
