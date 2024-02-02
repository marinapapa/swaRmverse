test_that("counting events works", {
  testdf <- data.frame(set = c(rep(1, 10), rep(2, 15)),
                       keep = c(rep(FALSE, 5), rep(TRUE,5),
                                rep(TRUE, 3), rep(FALSE, 5),
                                rep(TRUE, 5), rep(FALSE, 2))
  )
  testthat::expect_length(events_n(data = testdf), n = 1)
  testthat::expect_equal(events_n(data = testdf), 3)
  testthat::expect_error(events_n(data = testdf[,1]))
})

test_that("measuring events duration works", {
  testdf <- data.frame(event = c(rep(1, 5), rep(2, 10), rep(3, 15)))
  testthat::expect_equal(calc_dur_per_event(data = testdf, step2time = 1)[,2],
                         c(5, 10, 15))
  testthat::expect_equal(calc_dur_per_event(data = testdf, step2time = 0.5)[,2],
                         c(2.5, 5, 7.5))
})
