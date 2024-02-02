#' @title Group velocity timeseries
#'
#' @description Calculates and adds the speed and heading of each individual
#' over time in the dataset, and splits it in a list of dataframes based
#' on the defined sets.
#'
#' @param data A data frame with time series of individual's positional data,
#' as exported by the \code{set_data_format} function.
#' Columns needed: 't', 'x', 'y', 'id', 'set'.
#'
#' @param geo Logical, whether positions are geographic coordinates,
#'  default = FALSE.
#'
#' @param verbose Logical, whether to post updates on progress, default = FALSE.
#'
#' @param parallelize Logical, whether to run the function in parallel over individuals,
#'  default = FALSE.
#'
#' @return A list of dataframes, an element per set from the input dataframe
#' with new columns: head and speed.
#'
#' @author Marina Papadopoulou \email{m.papadopoulou.rug@@gmail.com}
#'
#' @seealso \code{\link{add_vels}}, \code{\link{set_data_format}}
#'
#' @export
group_vels <- function(data,
                       geo = TRUE,
                       verbose = FALSE,
                       parallelize = FALSE
                       ) {

  if (!("set" %in% colnames(data))){
    stop("The data should include a set column, as standardised by the
         set_data_format function.")
  }

  splitted_data <- split(data, data$set)

  if (verbose) {
    print("Adding velocity info to every set of the dataset..")
  }
  toret <- lapply(X = splitted_data,
                  FUN = add_vels,
                  verbose = verbose,
                  geo = geo,
                  parallelize = parallelize
  )
  if (verbose) {
    print("Done!")
  }
  return(toret)
}
