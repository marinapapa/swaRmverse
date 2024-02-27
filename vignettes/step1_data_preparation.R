## ----message=FALSE, warning=FALSE---------------------------------------------
library(swaRmverse)

raw <- read.csv(system.file("extdata/video/01.csv", package = "trackdf"))
raw <- raw[!raw$ignore, ]
head(raw)


## ----message=FALSE, warning=FALSE---------------------------------------------

data_df <- set_data_format(raw_x = raw$x,
                          raw_y = raw$y,
                          raw_t = raw$frame,
                          raw_id = raw$track_fixed,
                          origin = "2020-02-1 12:00:21",
                          period = "0.04S",
                          tz = "America/New_York"
                          )

head(data_df)

## ----message=FALSE, warning=FALSE---------------------------------------------
# dummy column
raw$context <- c(rep("ctx1", nrow(raw) / 2), rep("ctx2", nrow(raw) / 2))


## ----message=FALSE, warning=FALSE---------------------------------------------

data_df <- set_data_format(raw_x = raw$x,
                          raw_y = raw$y,
                          raw_t = raw$frame,
                          raw_id = raw$track_fixed,
                          origin = "2019-03-24 12:55:23",
                          period = "1 sec",
                          tz = "America/New_York",
                          raw_context = raw$context
                          )

head(data_df)

