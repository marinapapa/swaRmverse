## ----message=FALSE, warning=FALSE---------------------------------------------
library(swaRmverse)

# load pacakge data for many species
data("multi_species_metrics")

## A] Create the swarm space for this data only:
all_data <- multi_species_metrics

## B] Or bind with new data if continuing from step2
data("new_species_metrics") ## loads the output of step 2

new_species_tobind <- new_species_metrics[,!colnames(new_species_metrics) %in% c('event_dur', 'N', 'set', 'start_time')] # remove columns not needed for the swarm space
all_data <- rbind(multi_species_metrics, new_species_tobind)

## C] Or to use just the new data (overwrites previous command, comment out to compare with the other species):
all_data <- new_species_metrics


## -----------------------------------------------------------------------------
new_pca <- swarm_space(metrics_data = all_data,
                       space_type = "pca"
                       )

ggplot2::ggplot(new_pca$swarm_space,
                 ggplot2::aes(x = PC1, y = PC2, color = species)
                ) +
                ggplot2::geom_point() +
                ggplot2::theme_bw()


## -----------------------------------------------------------------------------
pca_info <- new_pca$pca$rotation[, new_pca$pca$sdev > 1]
print(pca_info)

ref_data <- new_pca$ref
head(ref_data)


## -----------------------------------------------------------------------------

new_tsne <- swarm_space(metrics_data = all_data,
                              space_type = "tsne",
                              tsne_rand_seed = 2023,
                              tsne_perplexity = 10
                              )

print("t-SNE was run with the following parameters:")
print(new_tsne$tsne_setup)

ggplot2::ggplot(new_tsne$swarm_space, ggplot2::aes(x = X, y = Y, color = species)) +
  ggplot2::geom_point() +
  ggplot2::theme_bw()


## -----------------------------------------------------------------------------
data("multi_species_pca")
data("multi_species_pca_data")

new_pca_data <- expand_pca_swarm_space(metrics_data = new_species_metrics,
                                       pca_space = multi_species_pca)

expanded_pca <- rbind(multi_species_pca_data, 
                      new_pca_data)

ggplot2::ggplot(expanded_pca,
                ggplot2::aes(x = PC1, y = PC2, color = species)) +
   ggplot2::geom_point() +
   ggplot2::theme_bw()


## ----message=FALSE, warning=FALSE---------------------------------------------

data("new_species_metrics") ## loads the output of step 2

## Use another dataset:
data_df <- get(data("tracks", package = "trackdf"))
data_df$set <- as.Date(data_df$t)

another_species <- col_motion_metrics_from_raw(data_df,
                                mov_av_time_window = 10,
                                step2time = 1,
                                geo = TRUE,
                                verbose = FALSE,
                                speed_lim = 0,
                                pol_lim = 0.3,
                                parallelize_all = FALSE
                                )

another_species$species <- "new_species_2"

## Bind all the datasets you want to compare here
all_data <- rbind(another_species, new_species_metrics)

new_pca <- swarm_space(metrics_data = all_data,
                       space_type = "pca"
                       )

ggplot2::ggplot(new_pca$swarm_space,
                ggplot2::aes(x = PC1, y = PC2, color = species)
                ) +
  ggplot2::geom_point() +
  ggplot2::theme_bw()


