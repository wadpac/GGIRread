library(GGIRread)
context("read ActiGraph csv files")
test_that("ActiGraph61 is correctly read", {
  file = system.file("testfiles/ActiGraph61.csv", package = "GGIRread")
  D = readActiGraphCount(filename = file, timeformat = "%m/%d/%Y %H:%M:%S", tz =  "")
  expect_equal(D$deviceSerialNumber, "MOS2D16160581")
  expect_equal(D$epochSize, 5)
  expect_equal(format(D$startTime), "2016-08-15 21:35:00")
  expect_equal(nrow(D$data), 990)
  expect_equal(ncol(D$data), 5)
  expect_equal(sum(D$data[, c("y", "x", "z", "vm")]), 63952.33)
  expect_equal(sum(D$data[, c("steps")]), 253)
})

test_that("ActiGraph31 is correctly read", {
  file = system.file("testfiles/ActiGraph13.csv", package = "GGIRread")
  D = readActiGraphCount(filename = file, timeformat = "%m/%d/%Y %H:%M:%S", tz =  "")
  expect_equal(D$deviceSerialNumber, "CLE2A2123456")
  expect_equal(D$epochSize, 15)
  expect_equal(format(D$startTime), "2013-08-26 09:00:00")
  expect_equal(nrow(D$data), 990)
  expect_equal(ncol(D$data), 5)
  expect_equal(sum(D$data[, c("y", "x", "z", "vm")]), 272870.6, tol = 0.1)
  expect_equal(sum(D$data[, c("steps")]), 1118)
})

test_that("ActiGraph13_timestamps_headers.csv is correctly read", {
  file = system.file("testfiles/ActiGraph13_timestamps_headers.csv", package = "GGIRread")
  D = readActiGraphCount(filename = file, timeformat = "%d-%m-%Y %H:%M:%S", tz =  "")
  expect_equal(D$deviceSerialNumber, "TAS1D48140206")
  expect_equal(D$epochSize, 1)
  expect_equal(format(D$startTime), "2017-12-09 15:00:00")
  expect_equal(nrow(D$data), 1000)
  expect_equal(ncol(D$data), 5)
  expect_equal(sum(D$data[, c("y", "x", "z", "vm")]), 255707.4, tol = 0.1)
  expect_equal(sum(D$data[, c("steps")]), 442)
})

test_that("Actiwatch csv error correctly", {
  file = system.file("testfiles/ActiGraph13_timestamps_headers.csv", package = "GGIRread")
  expect_error(readActiGraphCount(filename = file,
                                  timeformat = "%m/%d/%Y %H:%M:%S"),
               regexp = "Time format*")
})