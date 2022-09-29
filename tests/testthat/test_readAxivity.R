library(GGIRread)
context("reading Axivity .cwa data")
test_that("readAxivity reads data from file correctly", {
  cwafile  = system.file("testfiles/ax3_testfile.cwa", package = "GGIRread")[1]
  AX3 = readAxivity(filename = cwafile, desiredtz = "Europe/Berlin", start = 1, end = 4)
  expect_equal(AX3$header$device, "Axivity")
  expect_equal(nrow(AX3$data), 900)
  expect_equal(ncol(AX3$data), 7)
  expect_equal(AX3$data$time[5], 1551174909)
  expect_equal(AX3$data$temp[3], 18.65)
  expect_equal(floor(sum(abs(AX3$data[,2:4]))), 1407)
  expect_equal(AX3$data[1,2], 0.8845225, tolerance = 3)
  expect_equal(AX3$data[4,3], -0.34375, tolerance = 3)
 
  # ask for more data then there is in the file
  AX3b = readAxivity(filename = cwafile, desiredtz = "Europe/Berlin", start = 1, end = 1000)
  expect_equal(AX3b$header$device, "Axivity")
  expect_equal(nrow(AX3b$data), 17299)
  expect_equal(ncol(AX3b$data), 7)
  expect_equal(AX3b$data$time[5], 1551174909)
  expect_equal(AX3b$data$temp[3], 18.65)
  expect_equal(floor(sum(abs(AX3b$data[,2:4]))), 24873)
  
})

test_that("readAxivity reads timezones correctly", {
  cwafile  = system.file("testfiles/ax3_testfile.cwa", package = "GGIRread")[1]
  old <- options(digits.secs = 3)
  tzAms = "Europe/Amsterdam"
  tzLon = "Europe/London"
  # desiredtz == configtz
  tzequal = readAxivity(filename = cwafile, desiredtz = tzLon, configtz = tzLon, start = 1, end = 4)
  expect_equal(tzequal$data$time[1], 1551178509)
  expect_equal(format(as.POSIXlt(tzequal$data$time[1], tz = tzLon,
                                       origin = "1970-01-01")), "2019-02-26 10:55:09.000")
  
  # desiredtz < configtz
  tzwest = readAxivity(filename = cwafile, desiredtz = tzLon, configtz = tzAms, start = 1, end = 4)
  expect_equal(tzwest$data$time[1], 1551174909)
  expect_equal(format(as.POSIXlt(tzwest$data$time[1], tz = tzLon, 
                                       origin = "1970-01-01")), "2019-02-26 09:55:09.000")
  
  # desiredtz > configtz
  tzeast = readAxivity(filename = cwafile, desiredtz = tzAms, configtz = tzLon, start = 1, end = 4)
  expect_equal(tzeast$data$time[1], 1551178509)
  expect_equal(format(as.POSIXlt(tzeast$data$time[1], tz = tzAms, 
                                       origin = "1970-01-01")), "2019-02-26 11:55:09.000")
  options(old)
})