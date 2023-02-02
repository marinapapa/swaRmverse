#' @title Data formatting
#' @description A wrapper for the track_df function of \code{trackdf} package.
#' @param raw_x A numeric vector representing the x coordinates of individual(s)
#' @param raw_y A numeric vector representing the y coordinates of individual(s)
#' @param raw_t A numeric vector that can be coerced to date-time objects by as_datetime representing the times (or frames) at which each location was recorded.
#' @param raw_id A vector representing the identity of each coordinate recording.
#' @param proj  A character string or a sp::CRS object representing the projection of the coordinates. Leave empty if the coordinates are not projected (e.g., output of video tracking). "+proj=longlat" is suitable for the output of most GPS trackers.
#' @param origin  Something that can be coerced to a date-time object by as_datetime representing the start date and time of the observations when t is a numeric vector.
#' @param tz  A time zone name. See OlsonNames.
#' @param period A character vector in a shorthand format (e.g. "1 second") or ISO 8601 specification. This is used when t is a numeric vector to represent time unit of the observations.
#' @param format  A character string indicating the formatting of 't'. See strptime for how to specify this parameter..
#' @return A track dataframe table, which is a colloquial term for an object of class track.
#' @author Marina Papadopoulou \email{m.papadopoulou.rug@@gmail.com}, Simon Garnier \email{garnier@@njit.edu}
#' @export
set_data_format <- function(raw_x,
                            raw_y,
                            raw_t,
                            raw_id,
                            origin,
                            period,
                            tz,
                            proj,
                            format
                            )
{
  if (length(raw_x) != length(raw_y) ||
      length(raw_y) != length(raw_t) ||
      length(raw_t) != length(raw_id))
  {
    warning("Input position, time and id vectors are of different lengths, shorter ones have been recycled.")
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
  colnames(tracked_df)[colnames(tracked_df) == "t"] <- "date_time"

  tracked_df <- set_date_time_format(tracked_df)
  return(tracked_df)
}


#' @title Set format of date and time
#' @description Splits the date_time column of a track dataframe to separate columns and transforms real time to seconds from beginning
#' @param df_track A trackdf dataframe with a date_time column
#' @return the input dataframe with separated 'date', 'real_time' and 'time' columns
#' @author Marina Papadopoulou \email{m.papadopoulou.rug@@gmail.com}
#' @export
set_date_time_format <- function(df_track)
{
  df_track[, c('date', 'time')] <- data.frame(do.call('rbind', strsplit(as.character(df_track$date_time), ' ', fixed=TRUE)))

  df_track <- lapply(split(df_track, df_track$date),
                     function(x){
                       start_t <- min(x$time)
                       x <- transform_time2secs(x, start_t = start_t)
                       return(x)
                     })

  names(df_track) <- NULL
  df_track <- do.call(rbind, df_track)
  df_track$date <- as.factor(df_track$date)

  return(df_track)
}
