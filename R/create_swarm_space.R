#' @title Create swarm space
#' @description Runs a pca or a tsne to produce a swarm space.
#' @param metrics_data Metrics of collective motion per species per event.
#' @param space_type Choice between pca and tsne.
#' @param event_duration_limit Filter out events that are shorter than given duration.
#' @param tsne_rand_seed Random seed for tsne
#' @param tsne_perplexity Perplexity parameter for tsne. Usually between 10-50.
#' @param save_pca_path Directory to save the results of the PCA analysis (instead of just returning the PC1-3 points).
#' If NULL (default), the pca is not saved.
#' @return the swarm space, x and y coordinates per event of each species
#' @author Marina Papadopoulou \email{m.papadopoulou.rug@@gmail.com}
#' @export
create_swarm_space <- function(metrics_data,
                        space_type = 'pca',
                        event_duration_limit = NA,
                        tsne_rand_seed = NA,
                        tsne_perplexity = 10,
                        save_pca_path = NULL
                        )

{
  if (!(is.na(event_duration_limit)))
  {
    if (!(any(colnames(metrics_data) == 'event_dur_s'))){
      stop("A column named 'event_dur_s' is needed to apply an event duration limit.")
    }
    metrics_data <- metrics_data[metrics_data$event_dur_s > event_duration_limit, ]
  }

  metrics_data <- metrics_data[stats::complete.cases(metrics_data),]
  topca <- metrics_data[, !(names(metrics_data) %in% c('event', 'event_dur', 'event_dur_s'))]

  if (space_type == 'pca')
  {
    pca_res <- stats::prcomp(topca[,names(topca) != 'species'], center = TRUE, scale. = TRUE)
    pca_data <- data.frame(topca$species, pca_res$x[,1:3])
    colnames(pca_data) <- c('species', 'PC1', 'PC2', 'PC3')
    if (!is.null(save_pca_path))
    {
      save(pca_res, file = paste0(save_pca_path, '/pca_res.Rdata'))
    }

    return(pca_data)
  }

  if (space_type == 'tsne')
  {
    if (is.na(tsne_rand_seed)) {stop('Please give a random seed for the tsne analysis.')}
    set.seed(tsne_rand_seed)
    tsne <- Rtsne::Rtsne(topca[,names(topca) != 'species'], dims = 2, perplexity = tsne_perplexity, verbose = FALSE, max_iter = 10000)
    tsnedata <- data.frame(topca$species, tsne$Y[,1:2])
    colnames(tsnedata) <- c('species', 'X', 'Y')
    return(tsnedata)
  }
}


#' @title Expands existins swarm space (pca)
#' @description Predicts the positions of new event data in existing pca space
#' @param metrics_data New metrics data to add in swarm space
#' @param event_duration_limit Filter out events that are shorter than given duration.
#' @param pca_data Coordinates of initial data in swarm space
#' @param pca_space The pca output to predict from
#' @return the extended swarm space, x and y coordinates per event of each species
#' @author Marina Papadopoulou \email{m.papadopoulou.rug@@gmail.com}
#' @export
expand_pca_swarm_space <- function(metrics_data,
                        pca_space,
                        pca_data,
                        event_duration_limit = NA
)
{
  if (!(is.na(event_duration_limit)))
  {
    if (!(any(colnames(metrics_data) == 'event_dur_s'))){
      stop("A column named 'event_dur_s' is needed to apply an event duration limit.")
    }
    metrics_data <- metrics_data[metrics_data$event_dur_s > event_duration_limit, ]
  }

  metrics_data <- metrics_data[stats::complete.cases(metrics_data),]
  topca <- metrics_data[, !(names(metrics_data) %in% c('species', 'event', 'event_dur', 'event_dur_s'))]

  newpca <- as.data.frame(stats::predict(pca_space, topca))
  newpca$species <- metrics_data$species

  newpca <- newpca[, names(newpca) %in% c('species', 'PC1', 'PC2', 'PC3')]
  alldata <- rbind(pca_data, newpca)
  return(alldata)
}
