#' @title Data Formatting
#'
#' @description This function is a wrapper for the  \code{track} function of the \code{trackdf} package.
#'
#' @param raw_x A numeric vector representing the x coordinates of individual(s).
#'
#' @param raw_y A numeric vector representing the y coordinates of individual(s).
#'
#' @param raw_t A numeric vector that can be coerced to date-time objects by as
#'  datetime representing the times (or frames) at which each location
#'  was recorded.
#'
#' @param raw_id A vector representing the identity of each coordinate
#' recording.
#'
#' @param proj A character string or a \code{sp::CRS} object representing
#' the projection of the coordinates. Leave empty if the coordinates are
#' not projected (e.g., output of video tracking). "+proj=longlat" is suitable
#' for the output of most GPS trackers.
#'
#' @param origin Something that can be coerced to a date-time object by as_
#' datetime representing the start date and time of the observations when t is
#' a numeric vector.
#'
#' @param tz A time zone name. See OlsonNames.
#'
#' @param period A character vector in a shorthand format (e.g. "1 second")
#' or ISO 8601 specification. This is used when t is a numeric vector to
#' represent time unit of the observations.
#'
#' @param format A character string indicating the formatting of 't'.
#' See strptime for how to specify this parameter.
#'
#' @param ...  Additional vectors representing categories that the data
#' should be split by. If none, only the date will be used as a unit of data
#' separation.
#'
#' @return A track dataframe table, which is a colloquial term for an
#'  object of class track.
#'
#' @author Marina Papadopoulou \email{m.papadopoulou.rug@@gmail.com}
#'
#' @examples
#' \dontrun{
#' data(raw_df)
#' t_df <- set_data_format(x = raw_df$x,
#'  y = raw_df$y, t = raw_df$t, id = raw_df$id)
#' }
#' @export
set_data_format <- function(raw_x,
                            raw_y,
                            raw_t,
                            raw_id,
                            origin,
                            period,
                            tz,
                            proj,
                            format,
                            ...
                            ) {
  if (length(raw_x) != length(raw_y) ||
      length(raw_y) != length(raw_t) ||
      length(raw_t) != length(raw_id)) {
    warning("Input position, time and id vectors are of different lengths,
             shorter ones have been recycled.")
  }

  tracked_df <- trackdf::track(x = raw_x,
                               y = raw_y,
                               t = raw_t,
                               id = raw_id,
                               origin = origin,
                               period = period,
                               tz = tz,
                               proj = proj,
                               format = format,
                               table = "df")

  group_vars <- list(...)

  if (length(group_vars) != 0) {
    if (any(lengths(group_vars) != nrow(tracked_df))) {
      warning("Extra set information is of different lengths, they are being ignored.")
    } else {
      extr_df <- as.data.frame(group_vars)
      group_id <- do.call(paste, c(extr_df[seq_len(ncol(extr_df))], sep = "_"))
      date_df <- data.frame(do.call("rbind",
                                     strsplit(as.character(tracked_df$t),
                                              " ",
                                              fixed = TRUE)))
      tracked_df$set <- paste(date_df[, 1], group_id, sep = "_")
      return(tracked_df)
    }
   }

  date_df <- data.frame(do.call("rbind",
                                strsplit(as.character(tracked_df$t),
                                         " ",
                                         fixed = TRUE)))
  tracked_df$set <- date_df[, 1]

  tracked_df
}
