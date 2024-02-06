#' @title Create a swarm space
#'
#' @description Runs a PCA (Principal component analysis)
#' or a t-SNE (t-distributed Stochastic Neighbor Embedding) over the
#' global and pairwise metrics of collective motion per each event to
#' produce a swarm space. The PCA is computed with the \code{stats::prcomp}
#' function and the t-SNE with the \code{Rtsne::Rtsne} function.
#'
#' @param metrics_data A dataframe with metrics of collective motion per event.
#'
#' @param space_type A string, stating the choice between PCA ("pca")
#' and t-SNE ("tsne"), default = "pca".
#'
#' @param event_dur_limit Numeric, capturing an event duration value in seconds.
#' Used to filter out events that are shorter that this value.
#' Default = NA, no filtering is applied.
#'
#' @param tsne_rand_seed Numeric, the random seed for the t-SNE analysis, to
#' ensure reproducibility. Default = NA, but a value should be given if the
#' t-SNE analysis is selected.
#'
#' @param tsne_perplexity Numeric, the perplexity parameter for the t-SNE analysis.
#' Usually between 10-50, default = 25.
#'
#' @return A list with 3 elements: a dataframe representing the
#' swarm space (x and y coordinates per event of each species), a reference dataframe
#' (ref) including all the additional event information from the input metric data
#' dataframe, a dataframe for the t-SNE analysis (tsne_setup) that includes the
#' input parameters used, and a list for the PCA analysis (pca) with the output
#' of the \code{stats::prcomp} command.
#'
#' @author Marina Papadopoulou \email{m.papadopoulou.rug@@gmail.com}
#'
#' @seealso \code{\link{group_metrics}, \link{pairwise_metrics}, \link{nn_metrics}, \link{col_motion_metrics}}
#'
#' @export
swarm_space <- function(metrics_data,
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


#' @title Expand existing swarm space (PCA)
#'
#' @description Predicts the positions of new event data in an
#' existing PCA space using the \code{stats::predict} function.
#'
#' @param metrics_data A dataframe with the new metrics data to add in swarm space.
#'
#' @param event_dur_limit Numeric, capturing an event duration value in seconds.
#' Used to filter out events that are shorter that this value.
#' Default = NA, no filtering is applied.
#'
#' @param pca_space The PCA object to predict from, the output of the
#' \code{stats::prcomp} function or the pca element of the list output
#' of the \code{swarm_space} function.
#'
#' @return A dataframe with the x and y coordinates in the input swarm space
#' per event of the new species.
#'
#' @author Marina Papadopoulou \email{m.papadopoulou.rug@@gmail.com}
#'
#' @seealso \code{\link{swarm_space}}
#'
#' @export
expand_pca_swarm_space <- function(metrics_data,
                                   pca_space,
                                   event_dur_limit = NA
                        ) {
  if (!(is.na(event_dur_limit))) {
    if (!(any(colnames(metrics_data) == "event_dur_s"))) {
      stop("A column named 'event_dur_s' is
            needed to apply an event duration limit.")
    }
    metrics_data <- metrics_data[
                      metrics_data$event_dur_s > event_dur_limit, ]
  }

  metrics_data <- metrics_data[stats::complete.cases(metrics_data), ]
  topca <- metrics_data[, !(names(metrics_data) %in%
                            c("species", "event", "event_dur", "N", "start_time"))]

  newpca <- as.data.frame(stats::predict(pca_space, topca))
  newpca$species <- metrics_data$species
  newpca$event <- metrics_data$event

  newpca <- newpca[, names(newpca) %in% c("species", "PC1", "PC2", "PC3")]
  return(newpca)
}
