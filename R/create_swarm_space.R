#' @title Create swarm space
#'
#' @description Runs a PCA (Principal component analysis)
#' or a t-SNE (t-distributed Stochastic Neighbor Embedding) over the
#' global and pairwise metrics of collective motion per each event to
#' produce a swarm space.
#'
#' @param metrics_data Metrics of collective motion per event.
#'
#' @param space_type Choice between PCA and t-SNE.
#'
#' @param event_dur_limit Filter out events that are shorter
#' than a given duration. Default NA, no filtering is applied.
#'
#' @param tsne_rand_seed Random seed for the t-SNE analysis.
#'
#' @param tsne_perplexity Perplexity parameter for the t-SNE analysis.
#' Usually between 10-50, default 25.
#'
#' @return the swarm space, x and y coordinates per event of each species
#'
#' @author Marina Papadopoulou \email{m.papadopoulou.rug@@gmail.com}
#'
#' @seealso \code{\link{global_metrics}, \link{nn_metrics}, \link{col_motion_metrics}}
#'
#' @export
create_swarm_space <- function(metrics_data,
                        space_type = "pca",
                        event_dur_limit = NA,
                        tsne_rand_seed = NA,
                        tsne_perplexity = 25
                        ) {
  if (!(is.na(event_dur_limit))) {
    if (!(any(colnames(metrics_data) == "event_dur"))) {
      stop("A column named 'event_dur' is needed to
      apply an event duration limit.")
    }
    metrics_data <- metrics_data[metrics_data$event_dur_s > event_dur_limit, ]
  }

  metrics_data <- metrics_data[stats::complete.cases(metrics_data), ]
  df <- metrics_data[, !(
            names(metrics_data) %in% c("event_dur", "N", "start_time", "set")
            )]

  if (space_type == "pca") {
    swarm_space <- do_pca(df)

    } else if (space_type == "tsne") {
    swarm_space <- do_tsne(df, tsne_rand_seed, tsne_perplexity)
  }

  swarm_space$ref <- metrics_data[, names(metrics_data) %in% c("species", "event", "set", "start_time", "event_dur", "N")]

  return(swarm_space)
}

#' @title Perform PCA
#'
#' @description Run a PCA to place events on collective motion into less-dimentional space.
#'
#' @param df A dataframe with the metrics of collective motion
#' per event.
#'
#' @return a list with the swarm space (a dataframe with PC1, PC2, PC3
#' coordinates per event) and the complete pca object.
#'
#' @author Marina Papadopoulou \email{m.papadopoulou.rug@@gmail.com}
#'
#' @keywords internal
do_pca <- function(df) {
 pca_res <- stats::prcomp(df[, !(names(df) %in% c("event", "species"))],
                             center = TRUE,
                             scale. = TRUE)

 pca_data <- data.frame(df$event, df$species, pca_res$x[, 1:3])
 colnames(pca_data) <- c("event", "species", "PC1", "PC2", "PC3")
 toret <- list(swarm_space = pca_data, pca = pca_res)

 return(toret)
}


#' @title Performs t-SNE
#'
#' @description Calculating the coordinates of each event in the t-SNE space.
#'
#' @param df A dataframe with the metrics of collective motion
#'per event.
#'
#' @param tsne_rand_seed Random seed to be able to reproduce the t-SNE analysis.
#'
#' @param tsne_perplexity Perplexity parameter for tsne. Usually between 10-50.
#'
#' @return A dataframe with X and Y coordinates per event in the t-SNE space.
#'
#' @author Marina Papadopoulou \email{m.papadopoulou.rug@@gmail.com}
#'
#' @keywords internal
do_tsne <- function(df,
                    tsne_rand_seed,
                    tsne_perplexity) {

  if (is.na(tsne_rand_seed)) {
      stop("Please give a random seed for the tsne analysis.")
  }
  set.seed(tsne_rand_seed)
  tsne <- Rtsne::Rtsne(df[,!(names(df) %in% c("event", "species"))],
                       dims = 2,
                       perplexity = tsne_perplexity,
                       verbose = FALSE,
                       max_iter = 10000
                       )
  tsnedata <- data.frame(df$event, df$species, tsne$Y[, 1:2])
  colnames(tsnedata) <- c("event", "species", "X", "Y")

  tsne_prop <- data.frame(prop = c("perplexity", "random_seed", "max_iter"),
                          vals = c(tsne_perplexity, tsne_rand_seed, 10000))

  toret <- list(swarm_space = tsnedata, tsne_setup = tsne_prop)
  return(toret)
}

#' @title Expands existing swarm space (PCA)
#'
#' @description Predicts the positions of new event data in existing PCA space.
#'
#' @param metrics_data New metrics data to add in swarm space.
#'
#' @param event_duration_limit Filter out events that are shorter
#' than given duration.
#'
#' @param pca_data Coordinates of initial data in swarm space.
#'
#' @param pca_space The PCA output to predict from.
#'
#' @return Τhe extended swarm space, x and y coordinates
#' per event of each species.
#'
#' @author Marina Papadopoulou \email{m.papadopoulou.rug@@gmail.com}
#'
#' @seealso \code{\link{create_swarm_space}}
#'
#' @export
expand_pca_swarm_space <- function(metrics_data,
                        pca_space,
                        pca_data,
                        event_duration_limit = NA
                        ) {
  if (!(is.na(event_duration_limit))) {
    if (!(any(colnames(metrics_data) == "event_dur_s"))) {
      stop("A column named 'event_dur_s' is
            needed to apply an event duration limit.")
    }
    metrics_data <- metrics_data[
                      metrics_data$event_dur_s > event_duration_limit, ]
  }

  metrics_data <- metrics_data[stats::complete.cases(metrics_data), ]
  topca <- metrics_data[, !(names(metrics_data) %in%
                            c("species", "event", "event_dur"))]

  newpca <- as.data.frame(stats::predict(pca_space, topca))
  newpca$species <- metrics_data$species

  newpca <- newpca[, names(newpca) %in% c("species", "PC1", "PC2", "PC3")]
  alldata <- rbind(pca_data, newpca)
  return(alldata)
}
