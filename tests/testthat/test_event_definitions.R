test_that("adding event ids works", {
  testdf <- data.frame(set = c(rep(1, 10), rep(2, 15)),
                        keep = c(rep(FALSE, 5), rep(TRUE,5),
                                 rep(TRUE, 3), rep(FALSE, 5),
                                 rep(TRUE, 5), rep(FALSE, 2))
  )
  testthat::expect_length(get_event_ids(df = testdf), n = 25)
  testthat::expect_equal(get_event_ids(df = testdf), c(rep(0, 4),
                                                        rep(1,6),
                                                        rep(2, 3),
                                                        rep(2, 4),
                                                        rep(3, 6),
                                                        rep(3, 2)))
  testthat::expect_error(get_event_ids(df = testdf[,1]))
})

test_that("event threshold picking works", {
  testthat::expect_length(pick_threshold(data_distr = c(1,2,3,4,5),
                                                var = 'something',
                                                threshold = 2
                                                ), n = 1)
  testthat::expect_equal(pick_threshold(data_distr = c(1,2,3,4,5),
                                                var = 'something',
                                                threshold = 2), 2)
  testthat::expect_error(pick_threshold(data_distr = c(1,2,3,4,5),
                                               var = 'something',
                                               threshold = 'not a number'))
  testthat::expect_warning(pick_threshold(data_distr = c(1,2,3,4,5),
                                               var = 'something',
                                               threshold = 6))
})


