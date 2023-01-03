#' @title Relative position of neighbors
#' @description Calculates the bearing angle and distance from a focal individual of a group to all neighbors over time.
#' @param data Dataframe with group's timeseries. Column names must include: id, time.
#' @param return_df logical, whether to return the result dataframe or not.
#' @param lonlat whether positions are geographic coordinates, default = FALSE.
#' @param out_csv_dir directory output is saved to.
#' @param out_csv_name names of files that will be exported.
#' @param add_coords whether data is converted to geographic coordinates, default = 'FALSE'.
#' @param verbose whether to post updates on progress
#' @return a dataframe with a column for neighbor id, bearing angle, distance and heading deviation for each individual through time.
#' @author Marina Papadopoulou \email{m.papadopoulou.rug@@gmail.com}
#' @seealso \code{\link{rad_between}}, \code{\link{bearing_angle}}, \code{\link{perpDot}}
#' @export
neighb_rel_pos_timeseries_parallel <- function(
    data,
    return_df = TRUE,
    out_csv_dir = NA,
    out_csv_name = NA,
    add_coords = FALSE,
    lonlat = FALSE,
    verbose = FALSE
)
{
  savecsv = FALSE

  if (!return_df & is.na(out_csv_dir) & is.na(out_csv_name)){
    stop("No output selected: either set return_df to TRUE or input a valid saving directory and filename.")
  }
  if (is.na(out_csv_dir) & !(is.na(out_csv_name))){
    question1 <- readline("Your output input is a csv name without a path. Results won't be saved, \n are you sure you
                          want to continue? (Y/N)")
    if(regexpr(question1, 'y', ignore.case = TRUE) == 1){
    } else if (regexpr(question1, 'n', ignore.case = TRUE) == 1){
      stop("Check input variables: out_csv_dir and out_csv_name.")
    }
  }
  if (!(is.na(out_csv_dir)) & is.na(out_csv_name)){
    question1 <- readline("You output input is a saving csv path without a file name. Results won't be saved,\n are you sure you
                          want to continue? (Y/N)")
    if(regexpr(question1, 'y', ignore.case = TRUE) == 1){
    } else if (regexpr(question1, 'n', ignore.case = TRUE) == 1){
      stop("Check input variables: out_csv_dir and out_csv_name.")
    }
  }
  if (!(is.na(out_csv_dir))) {
    if (dir.exists(out_csv_dir)) {
      savecsv <- TRUE
    } else { stop('Input saving directory does not exist, check your out_csv_dir variable.') }
  }

  #N <- length(unique(data$id))
  if (verbose) { print('Measuring relative positions in parallel...')}

  thists <- split(data, data$time)

  pairwise_data <- function(thists,
                            lonlat = lonlat)


  {
    thists <- as.data.frame(thists)
    timestep <- as.character(thists$time[1])
    id_names <- unique(thists$id)
    N <- length(id_names)
    def_head <- c("date", 'time', 'id')

    df_bangls <- data.frame(matrix(NA, ncol = length(def_head) + N, nrow = N))
    df_dists <- data.frame(matrix(NA, ncol = length(def_head) + N, nrow = N))
    df_devs <- data.frame(matrix(NA, ncol = length(def_head) + N, nrow = N))
    df_rank <- data.frame(matrix(NA, ncol = length(def_head) + N, nrow = N))

    colNnames <- rep(NA, length(def_head) + N)
    colNnames[1:length(def_head)] <- def_head
    cnam_bangl <- cnam_dist <- cnam_rank  <- cnam_devs <- colNnames

    k <- length(def_head) + 1
    for (i in 1:length(id_names))
    {
      cnam_bangl[k] <- paste0('bAngl2n', id_names[i]);
      cnam_dist[k] <- paste0('dist2n', id_names[i]);
      cnam_devs[k] <- paste0('headdev2n', id_names[i]);
      cnam_rank[k] <- paste0('rankOfn', id_names[i]);
      k <- k +1
    }
    colnames(df_bangls) <- cnam_bangl
    colnames(df_dists) <- cnam_dist
    colnames(df_rank) <- cnam_rank
    colnames(df_devs) <- cnam_devs

    dfrow <- 1
    for (id in id_names)
    {
      posa_x <- thists$x[thists$id == id] # one individual
      posa_y <- thists$y[thists$id == id]
      ha_x <- thists$headx[thists$id == id] # one individual
      ha_y <- thists$heady[thists$id == id]

      if (length(posa_x) > 1){warning('Duplicated ids present.')}
      posa_x <- posa_x[1]
      posa_y <- posa_y[1]

      ha_x <- ha_x[1]
      ha_y <- ha_y[1]


      other_ids <- id_names[id_names != id]
      df_bangls[dfrow, 1:length(def_head)] <- df_dists[dfrow, 1:length(def_head)] <- df_rank[dfrow, 1:length(def_head)] <- c(as.character(thists$date[1]), timestep, id)
      df_devs[dfrow, 1:length(def_head)] <- c(as.character(thists$date[1]), timestep, id)
      for (m in other_ids)
      {
        posb_x <- as.numeric(unlist(thists[thists$id == m, "x"])) # vector of all else
        posb_y <-  as.numeric(unlist(thists[thists$id == m, "y"]))

        headb_x <- as.numeric(unlist(thists[thists$id == m, "headx"])) # vector of all else
        headb_y <-  as.numeric(unlist(thists[thists$id == m, "heady"]))

        if (length(posb_x) > 1) { warning('Duplicated ids per timestep present..') }
        posb_x <- posb_x[1]
        posb_y <- posb_y[1]
        headb_x <- headb_x[1]
        headb_y <- headb_y[1]

        df_bangls[dfrow, paste0('bAngl2n',m)] <- bearing_angle(c(ha_x, ha_y), c(posa_x, posa_y), c(posb_x, posb_y))
        df_dists[dfrow,  paste0('dist2n', m)] <- raster::pointDistance(c(posa_x, posa_y), c(posb_x, posb_y), lonlat = lonlat)
        df_devs[dfrow,  paste0('headdev2n', m)] <- pracma::rad2deg(rad_between(c(ha_x, ha_y), c(headb_x, headb_y)))
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

    df_dists <- reshape2::melt(df_dists, variables = c("date", "time", "id"))
    df_dists$variable <- gsub("dist2n","", df_dists$variable)
    colnames(df_dists) <- c("date", 'time', 'id', 'neighb', 'dist')

    df_bangls <- reshape2::melt(df_bangls, variables = c("date","time", "id"))
    df_bangls$variable <- gsub("bAngl2n","", df_bangls$variable)
    colnames(df_bangls) <- c("date", 'time', 'id', 'neighb', 'bangl')

    df_rank <- reshape2::melt(df_rank, variables = c("date", "time", "id"))
    df_rank$variable <- gsub("rankOfn", "", df_rank$variable)
    colnames(df_rank) <- c("date", 'time', 'id', 'neighb', 'rank')

    df_devs <- reshape2::melt(df_devs, variables = c("date", "time", "id"))
    df_devs$variable <- gsub("headdev2n", "", df_devs$variable)
    colnames(df_devs) <- c("date", 'time', 'id', 'neighb', 'head_dev')


    toreturn <- dplyr::full_join(df_bangls, df_dists, by = c("date", "time", "id", "neighb"))
    toreturn <- dplyr::full_join(toreturn, df_rank, by = c("date", "time", "id", "neighb"))
    toreturn <- dplyr::full_join(toreturn, df_devs, by = c("date", "time", "id", "neighb"))

    toreturn <- toreturn[toreturn$id != toreturn$neighb,]
    toreturn$time <- as.numeric(toreturn$time)
    return(toreturn)
  }

  numCores <- parallel::detectCores()
  cl <- parallel::makeCluster(numCores-2)

  res <- parallel::parLapply(cl,
                             thists,
                             pairwise_data,
                             lonlat = lonlat)

 # pbapply::pblapply()
  parallel::stopCluster(cl)
  if (verbose) { print('Parallel run done! Preparing output...')}

  res <- purrr::keep(res, function(x) nrow(x) > 0)
  res <- dplyr::bind_rows(res)

  if ( add_coords )
  {
    res$time <- as.numeric(res$time)
    res <- add_rel_pos_coords(res)
  }

  if (savecsv) { data.table::fwrite(res, paste0(out_csv_dir, out_csv_name), row.names = FALSE) }
  if (return_df) { return(res)}
}

#' @title Relative position of nearest neighbor in parallel
#' @description Calculates the bearing angle and distance from a focal individual of a group to its nearest neighbor over time.
#' @param data Dataframe with group's timeseries. Column names must include: id, time.
#' @param lonlat whether positions are geographic coordinates, default = FALSE.
#' @param add_coords whether data is converted to geographic coordinates, default = 'FALSE'.
#' @param verbose whether to post updates on progress
#' @return a dataframe with a column for neighbor id, bearing angle, distance and heading deviation for each individual through time.
#' @author Marina Papadopoulou \email{m.papadopoulou.rug@@gmail.com}
#' @seealso \code{\link{rad_between}}, \code{\link{bearing_angle}}, \code{\link{perpDot}}
#' @export
nn_rel_pos_timeseries_parallel <- function(
    data,
    add_coords = FALSE,
    lonlat = FALSE,
    verbose = FALSE
)
{
  if (verbose) { print('Measuring nearest neighbors relative positions in parallel...')}

  thists <- split(data, data$time)
  pairwise_data <- function(thists,
                            lonlat = lonlat)


  {
    thists <- as.data.frame(thists)
    timestep <- as.character(thists$time[1])
    id_names <- unique(thists$id)
    N <- length(id_names)
    def_head <- c("date", 'time', 'id')

    df_bangls <- data.frame(matrix(NA, ncol = length(def_head) + N, nrow = N))

    thists$nn_idx <- as.numeric(swaRm::nn(thists$x, thists$y, geo = lonlat, id = thists$id))
    thists$nnd <- as.numeric(swaRm::nnd(thists$x, thists$y, geo = lonlat))

    thists$bangl  <- unlist(lapply(seq_along(id_names), function(x){
       bearing_angle(c(cos(thists$head[x]), sin(thists$head[x])),
                               c(thists$x[x], thists$y[x]),
                               c(thists[thists$id == thists$nn_idx[x], 'x'],
                                 thists[thists$id == thists$nn_idx[x], 'y']))
    })
    )

    return(thists)
  }

  numCores <- parallel::detectCores()
  cl <- parallel::makeCluster(numCores-2)

  res <- parallel::parLapply(cl,
                             thists,
                             pairwise_data,
                             lonlat = lonlat)

  # pbapply::pblapply()
  parallel::stopCluster(cl)
  if (verbose) { print('Parallel run done! Preparing output...')}

  res <- purrr::keep(res, function(x) nrow(x) > 0)
  res <- dplyr::bind_rows(res)

  if ( add_coords )
  {
    res$time <- as.numeric(res$time)
    res <- add_rel_pos_coords(res)
  }

  return(res)
}

