test_that("moving average", {
  testthat::expect_length(moving_average(timeseries = c(1, 1, 1, 1), window = 2), n = 4)
  testthat::expect_equal(moving_average(timeseries = c(1, 1, 1, 1), window = 2), c(NA, 1, 1, NA))
  testthat::expect_error(moving_average(timeseries = c(1, 1, 1, 1), window = 1))
  testthat::expect_error(moving_average(timeseries = c(1, 1, 1, 1), window = 5))
})

test_that("normalizing a vector", {
  testthat::expect_length(normalize_data(vec = c(1, 1, 1, 1)), n = 4)
  testthat::expect_equal(normalize_data(vec = seq(0, 5)), seq(0, 1, 0.2))
})
