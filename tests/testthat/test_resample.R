library(GGIRread)
context("resample time series")
test_that("resample produces correct interpolations according to interpolation technique", {
  skip_on_cran()
  old <- options(digits = 10)
  raw = cbind(1:10, 1:10, 1:10)
  rawTime = seq(0.1, 1, by = 0.1)
  time = seq(0.15001, 1.05001, by = 0.1)
  stop = 10
  
  # linear interpolations
  dat_lin = resample(raw, rawTime, time, stop, type = 1)
  expect_equal(nrow(dat_lin), 9)
  expect_equal(ncol(dat_lin), 3)
  expect_equal(dat_lin[,1], seq(1.5001, 9.5001, by = 1))
  expect_equal(dat_lin[,1], dat_lin[,2])
  
  # nearest neighbour
  dat_nn = resample(raw, rawTime, time, stop, type = 2)
  expect_equal(nrow(dat_nn), 9)
  expect_equal(ncol(dat_nn), 3)
  expect_equal(dat_nn[,1], 2:10)
  expect_equal(dat_nn[,1], dat_nn[,2])
  options(old)
})
