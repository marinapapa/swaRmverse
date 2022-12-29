#' @title Mutli-species collective motion metrics
#' @docType data
#'
#' @description  A dataset containing the metrics of collective motion for 4 species:
#' stickleback fish, homing pigeons, goats and chacma baboons. They were used for the
#' construction of the initial swarm space.
#'
#' @format A dataframe with 118 rows and 12 columns:
#' \describe{
#'   \item{mean_mean_nnd}{Average nearest neighbor distance}
#'   \item{mean_sd_nnd}{Average within-group variation in nearest neighbor distance}
#'   \item{sd_mean_nnd}{Temporal variation in average nearest neighbor distance}
#'   \item{mean_pol}{Average polarization}
#'   \item{sd_pol}{Temporal variation in polarization}
#'   \item{stdv_speed}{Temporal variation in speed}
#'   \item{mean_sd_front}{Average within-group variation in frontness}
#'   \item{mean_mean_bangl}{Average bearing angle}
#'   \item{mean_shape}{Average group shape (rads)}
#'   \item{sd_shape}{Temporal variation in group shape (rads)}
#'   \item{species}{Species id}
#'   \item{event}{Event id}
#' }
#'
#' @usage data('multi_species_metrics')
"multi_species_metrics"


