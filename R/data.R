#' @title Multi-Species Collective Motion Metrics
#'
#' @docType data
#'
#' @description A dataset containing the metrics of collective motion for 4
#'  species: stickleback fish, homing pigeons, goats, and chacma baboons. They
#'  were used for the construction of the initial swarm space in:
#'
#'  Papadopoulou Marina, Fürtbauer Ines, O'Bryan Lisa R., Garnier Simon,
#'  Georgopoulou Dimitra G., Bracken Anna M., Christensen Charlotte and King
#'  Andrew J. 2023. Dynamics of collective motion across time and species. Phil.
#'  Trans. R. Soc. B 378: 20220068. http://doi.org/10.1098/rstb.2022.0068
#'
#' @format A dataframe with 118 rows and 12 columns:
#'  \describe{
#'      \item{mean_mean_nnd}{Average nearest neighbor distance}
#'      \item{mean_sd_nnd}{Average within-group variation in nearest neighbor
#'          distance}
#'      \item{sd_mean_nnd}{Temporal variation in average nearest neighbor
#'          distance}
#'      \item{mean_pol}{Average polarization}
#'      \item{sd_pol}{Temporal variation in polarization}
#'      \item{stdv_speed}{Temporal variation in speed}
#'      \item{mean_sd_front}{Average within-group variation in frontness}
#'      \item{mean_mean_bangl}{Average bearing angle}
#'      \item{mean_shape}{Average group shape (rads)}
#'      \item{sd_shape}{Temporal variation in group shape (rads)}
#'      \item{species}{Species id}
#'      \item{event}{Event id}
#' }
#'
#' @references
#'  Papadopoulou Marina, Fürtbauer Ines, O'Bryan Lisa R., Garnier Simon,
#'  Georgopoulou Dimitra G., Bracken Anna M., Christensen Charlotte and King
#'  Andrew J. 2023. Dynamics of collective motion across time and species. Phil.
#'  Trans. R. Soc. B 378: 20220068. http://doi.org/10.1098/rstb.2022.0068
#'
#' @usage data('multi_species_metrics')
"multi_species_metrics"


#' @title Multi-Species PCA
#'
#' @docType data
#'
#' @description The swarm space PCA of 4 species: stickleback fish, homing
#'  pigeons, goats and chacma baboons. First published as part of:
#'
#'  Papadopoulou Marina, Fürtbauer Ines, O'Bryan Lisa R., Garnier Simon,
#'  Georgopoulou Dimitra G., Bracken Anna M., Christensen Charlotte and King
#'  Andrew J. 2023. Dynamics of collective motion across time and species. Phil.
#'  Trans. R. Soc. B 378: 20220068. http://doi.org/10.1098/rstb.2022.0068
#'
#' @format A list of 5 elements, exported by the \code{stats::prcomp function}.
#'
#' @references
#'  Papadopoulou Marina, Fürtbauer Ines, O'Bryan Lisa R., Garnier Simon,
#'  Georgopoulou Dimitra G., Bracken Anna M., Christensen Charlotte and King
#'  Andrew J. 2023. Dynamics of collective motion across time and species. Phil.
#'  Trans. R. Soc. B 378: 20220068. http://doi.org/10.1098/rstb.2022.0068
#'
#' @seealso \code{\link{multi_species_pca_data}}
#'
#' @usage data('multi_species_pca')
"multi_species_pca"


#' @title Multi-Species PCA Data
#'
#' @docType data
#'
#' @description The positions of events from 4 species: stickleback fish, homing
#'  pigeons, goats and chacma baboons, in the PCA swarm space (see
#'  \code{\link{multi_species_pca}}. First published as part of:
#'
#'  Papadopoulou Marina, Fürtbauer Ines, O'Bryan Lisa R., Garnier Simon,
#'  Georgopoulou Dimitra G., Bracken Anna M., Christensen Charlotte and King
#'  Andrew J. 2023. Dynamics of collective motion across time and species. Phil.
#'  Trans. R. Soc. B 378: 20220068. http://doi.org/10.1098/rstb.2022.0068
#'
#' @format A dataframe of 3 columns: species, PC1, PC2, PC3.
#'
#' @references
#'  Papadopoulou Marina, Fürtbauer Ines, O'Bryan Lisa R., Garnier Simon,
#'  Georgopoulou Dimitra G., Bracken Anna M., Christensen Charlotte and King
#'  Andrew J. 2023. Dynamics of collective motion across time and species. Phil.
#'  Trans. R. Soc. B 378: 20220068. http://doi.org/10.1098/rstb.2022.0068
#'
#' @seealso \code{\link{multi_species_pca}}
#'
#' @usage data('multi_species_pca_data')
"multi_species_pca_data"


#' @title The Collective Motion Metrics of a New Species
#'
#' @docType data
#'
#' @description  The output dataset of vignette 2, containing the metrics of 
#'  collective motion for a new species.
#'
#' @usage data('new_species_metrics')
"new_species_metrics"
