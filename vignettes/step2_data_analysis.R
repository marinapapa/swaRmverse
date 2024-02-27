## ----message=FALSE, warning=FALSE---------------------------------------------
library(swaRmverse)
#data_df <- trackdf::tracks
#raw$set <- c(rep('ctx1', nrow(raw)/2 ), rep('ctx2', nrow(raw)/2))
raw <- read.csv(system.file("extdata/video/01.csv", package = "trackdf"))
raw <- raw[!raw$ignore, ]

## Add fake context
raw$context <- c(rep("ctx1", nrow(raw) / 2), rep("ctx2", nrow(raw) / 2))

data_df <- set_data_format(raw_x = raw$x,
                          raw_y = raw$y,
                          raw_t = raw$frame,
                          raw_id = raw$id,
                          origin = "2020-02-1 12:00:21",
                          period = "0.04S",
                          tz = "America/New_York",
                          raw_context = raw$context
                          )

is_geo <- FALSE
data_dfs <- add_velocities(data_df,
                           geo = is_geo,
                           verbose = TRUE,
                           parallelize = FALSE
                           ) ## A list of dataframes
#head(data_dfs[[1]])
print(paste("Velocity information added for", length(data_dfs), "sets."))

## ----message=FALSE, warning=FALSE---------------------------------------------

sampling_timestep <- 0.04
time_window <- 1 # seconds
smoothing_time_window <- time_window / sampling_timestep

g_metr <- group_metrics_per_set(data_list = data_dfs,
                               mov_av_time_window = smoothing_time_window,
                               step2time = sampling_timestep,
                               geo = is_geo,
                               parallelize = FALSE
                               )
summary(g_metr)


## ----message=FALSE, warning=FALSE---------------------------------------------

data_df <- pairwise_metrics(data_list = data_dfs,
                            geo = is_geo,
                            verbose = TRUE,
                            parallelize = FALSE,
                            add_coords = FALSE # could be set to TRUE if the relative positions of neighbors are needed 
                            )

#tail(data_df)

## ----message=FALSE, warning=FALSE---------------------------------------------

### Interactive mode, if the limits of speed and polarization are unknown
# new_species_metrics <- col_motion_metrics(data_df,
#                                            global_metrics = g_metr,
#                                            step2time = sampling_timestep,
#                                            verbose = TRUE,
#                                            speed_lim = NA,
#                                            pol_lim = NA
#                                             
# )

new_species_metrics <- col_motion_metrics(data_df,
                                           global_metrics = g_metr,
                                           step2time = sampling_timestep,
                                           verbose = TRUE,
                                           speed_lim = 150,
                                           pol_lim = 0.3
)

# summary(new_species_metrics)


## ----message=FALSE, warning=FALSE---------------------------------------------

new_species_metrics <- col_motion_metrics_from_raw(data_df,
                                mov_av_time_window = smoothing_time_window,
                                step2time = sampling_timestep,
                                geo = is_geo,
                                verbose = TRUE,
                                speed_lim = 150,
                                pol_lim = 0.3,
                                parallelize_all = FALSE
                                )

# summary(new_species_metrics)


## ----message=FALSE, warning=FALSE---------------------------------------------

new_species_metrics$species <- "new_species_1"

head(new_species_metrics)

## Un-comment bellow to save the output in order to combine it with other datasets (replace 'path2file' with appropriate local path and name).
# write.csv(new_species_metrics, file = path2file.csv, row.names = FALSE) # OR R object
# save(new_species_metrics, file = path2file.rda) 


