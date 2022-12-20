#' @title Relative position of neighbors
#' @description Calculates the bearing angle and distance from a focal individual of a group to all neighbors over time.
#' @param data Dataframe with group's timeseries. Column names must include: id, time.
#' @param return_df logical, whether to return the result dataframe or not.
#' @param lonlat whether positions are geographic coordinates, default = FALSE.
#' @param pos_label_x column name of x position column, default = 'lon'.
#' @param pos_label_y column name of y position column, default = 'lat'.
#' @param head_label_x column name of x heading column, default = 'headx'.
#' @param head_label_y column name of y heading column, default = 'heady'.
#' @param date_label column name of date, default = 'date'.
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
    date_label = 'date',
    return_df = TRUE,
    out_csv_dir = NA,
    out_csv_name = NA,
    add_coords = FALSE,
    lonlat = FALSE,
    pos_label_x = 'lon',
    pos_label_y = 'lat',
    head_label_x = 'headx',
    head_label_y = 'heady',
    verbose = FALSE
)
{
  savecsv = FALSE

  if (!return_df & is.na(out_csv_dir) & is.na(out_csv_name)){
    stop("No output selected: either set return_df to TRUE or input a valid saving directory and filename.")
  }
  if (is.na(out_csv_dir) & !is.na(out_csv_name)){
    question1 <- readline("Your output input is a csv name without a path. Results won't be saved, are you sure you
                          want to continue? (Y/N)")
    if(regexpr(question1, 'y', ignore.case = TRUE) == 1){
    } else if (regexpr(question1, 'n', ignore.case = TRUE) == 1){
      stop("Check input variables: out_csv_dir and out_csv_name.")
    }
  }
  if (!is.na(out_csv_dir) & is.na(out_csv_name)){
    question1 <- readline("You output input is a saving csv path without a file name. Results won't be saved, are you sure you
                          want to continue? (Y/N)")
    if(regexpr(question1, 'y', ignore.case = TRUE) == 1){
    } else if (regexpr(question1, 'n', ignore.case = TRUE) == 1){
      stop("Check input variables: out_csv_dir and out_csv_name.")
    }
  }
  if (!is.na(out_csv_dir)) {
    if (dir.exists(out_csv_dir)) {
      savecsv <- TRUE
    } else { stop('Input saving directory does not exist, check your out_csv_dir variable.') }
  }

  N <- length(unique(data$id))
  if (verbose) { print('Measuring relative positions in parallel...')}

  thists <- split(data, data$time)

  pairwise_data <- function(thists,
                            N,
                            pos_label_x,
                            pos_label_y,
                            head_label_x,
                            head_label_y,
                            date_label,
                            lonlat = lonlat)


  {
    # rad_between <- function(a, b)
    # {
    #   if (!is.vector(a) || !is.vector(b) || length(a) != 2 || length(b) != 2) {
    #     print(a)
    #     print(b)
    #     stop("Input should be two vectors of size 2 (x,y)")}
    #   c <- perpDot(a,b);
    #   d <- pracma::dot(a,b);
    #   return(atan2(c,d));
    # }
    #
    #
    # bearing_angle <- function(
    # h_a,
    # pos_a,
    # pos_b
    # )
    # {
    #   pos_dif = pos_b - pos_a
    #   return(rad_between(h_a, pos_dif))
    # }
    #
    #
    # perpDot <- function(a, b)
    # {
    #   if (!is.vector(a) || !is.vector(b) || length(a) != 2 || length(b) != 2) {stop("Input should be two vectors of size 2 (x,y)")}
    #   return(a[1] * b[2] - a[2] * b[1]);
    # }

    thists <- as.data.frame(thists)
    timestep <- as.character(thists$time[1])
    id_names <- unique(thists$id)
    def_head <- c(date_label, 'time', 'id')
    if (length(id_names) < N){
      h <- c(date_label, "time", "id", "neighb", "dist", "bangl", "rank", "head_dev")
      emptydf <- data.frame(matrix(NA, ncol = length(h), nrow = 0))
      colnames(emptydf) <- h
      return(emptydf)
    }

    df_bangls <- data.frame(matrix(NA, ncol = length(def_head) + N, nrow = N))
    df_dists <- data.frame(matrix(NA, ncol = length(def_head) + N, nrow = N))
    df_devs <- data.frame(matrix(NA, ncol = length(def_head) + N, nrow = N))
    df_rank <- data.frame(matrix(NA, ncol = length(def_head) + N, nrow = N))

    colNnames <- rep(NA, length(def_head) + N)
    colNnames[1:length(def_head)] <- def_head
    cnam_bangl <- cnam_dist <- cnam_rank  <- cnam_devs <- colNnames

    k <- length(def_head) + 1
    for (i in 1:N)
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
      posa_x <- thists[thists$id == id, pos_label_x] # one individual
      posa_y <- thists[thists$id == id, pos_label_y]
      ha_x <- thists[thists$id == id, head_label_x] # one individual
      ha_y <- thists[thists$id == id, head_label_y]

      if (length(posa_x) > 1){warning('Duplicated ids present.')}
      posa_x <- posa_x[1]
      posa_y <- posa_y[1]

      ha_x <- ha_x[1]
      ha_y <- ha_y[1]


      other_ids <- id_names[id_names != id]
      df_bangls[dfrow, 1:length(def_head)] <- df_dists[dfrow, 1:length(def_head)] <- df_rank[dfrow, 1:length(def_head)] <- c(as.character(thists[1, date_label]), timestep, id)
      df_devs[dfrow, 1:length(def_head)] <- c(as.character(thists[1, date_label]), timestep, id)
      for (m in other_ids)
      {
        posb_x <- as.numeric(unlist(thists[thists$id == m, pos_label_x])) # vector of all else
        posb_y <-  as.numeric(unlist(thists[thists$id == m, pos_label_y]))

        headb_x <- as.numeric(unlist(thists[thists$id == m, head_label_x])) # vector of all else
        headb_y <-  as.numeric(unlist(thists[thists$id == m, head_label_y]))

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

    df_dists <- reshape2::melt(df_dists, variables = c(date_label, "time", "id"))
    df_dists$variable <- gsub("dist2n","", df_dists$variable)
    colnames(df_dists) <- c(date_label, 'time', 'id', 'neighb', 'dist')

    df_bangls <- reshape2::melt(df_bangls, variables = c(date_label,"time", "id"))
    df_bangls$variable <- gsub("bAngl2n","", df_bangls$variable)
    colnames(df_bangls) <- c(date_label, 'time', 'id', 'neighb', 'bangl')

    df_rank <- reshape2::melt(df_rank, variables = c(date_label, "time", "id"))
    df_rank$variable <- gsub("rankOfn", "", df_rank$variable)
    colnames(df_rank) <- c(date_label, 'time', 'id', 'neighb', 'rank')

    df_devs <- reshape2::melt(df_devs, variables = c(date_label, "time", "id"))
    df_devs$variable <- gsub("headdev2n", "", df_devs$variable)
    colnames(df_devs) <- c(date_label, 'time', 'id', 'neighb', 'head_dev')


    toreturn <- dplyr::full_join(df_bangls, df_dists, by = c(date_label, "time", "id", "neighb"))
    toreturn <- dplyr::full_join(toreturn, df_rank, by = c(date_label, "time", "id", "neighb"))
    toreturn <- dplyr::full_join(toreturn, df_devs, by = c(date_label, "time", "id", "neighb"))

    toreturn <- toreturn[toreturn$id != toreturn$neighb,]
    toreturn$time <- as.numeric(toreturn$time)
    return(toreturn)
  }

  numCores <- parallel::detectCores()
  cl <- parallel::makeCluster(numCores-2)

  res <- parallel::parLapply(cl,
                             thists,
                             pairwise_data,
                             N = N,
                             pos_label_x = pos_label_x,
                             pos_label_y = pos_label_y,
                             head_label_x = head_label_x,
                             head_label_y = head_label_y,
                             date_label = date_label,
                             lonlat = lonlat)

  parallel::stopCluster(cl)
  if (verbose) { print('Parallel run done! Preparing output...')}

  res <- purrr::keep(res, function(x) nrow(x) > 0)
  res <- dplyr::bind_rows(res)

  if ( add_coords )
  {
    res$time <- as.numeric(res$time)
    res <- add_rel_pos_coords(res,  bearing_angle_label = 'bangl')
  }

  if (savecsv) { data.table::fwrite(res, paste0(out_csv_dir, out_csv_name), row.names = FALSE) }
  if (return_df) { return(res)}
}

