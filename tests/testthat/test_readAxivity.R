library(GGIRread)
context("reading Axivity .cwa data")
test_that("readAxivity reads data from AX3 file correctly", {
  cwafile  = system.file("testfiles/ax3_testfile.cwa", package = "GGIRread")[1]
  AX3 = readAxivity(filename = cwafile, desiredtz = "Europe/Berlin", start = 1, end = 4)
  expect_equal(AX3$header$device, "Axivity")
  expect_equal(nrow(AX3$data), 364)
  expect_equal(ncol(AX3$data), 7)
  expect_equal(AX3$data$time[5], 1551174907.255, tolerance = .001, scale = 1)
  expect_equal(AX3$data$temp[3], 26.46484, tolerance = 0.0001)
  expect_equal(floor(sum(abs(AX3$data[,2:4]))), 603)
  expect_equal(AX3$data[1,2], 0.7656, tolerance = .0001, scale = 1)
  expect_equal(AX3$data[4,3], -0.3125, tolerance = .0001, scale = 1)
  expect_true(is.null(AX3$QClog))
  
  # ask for more data then there is in the file
  AX3b = readAxivity(filename = cwafile, desiredtz = "Europe/Berlin", start = 0, end = 1000)
  expect_equal(AX3b$header$device, "Axivity")
  expect_equal(nrow(AX3b$data), 17599)
  expect_equal(ncol(AX3b$data), 7)
  expect_equal(AX3b$data$time[5], 1551174906.040, tolerance = .001, scale = 1)
  expect_equal(AX3b$data$temp[3], 25.5859, tolerance = 0.0001, scale = 1)
  expect_equal(floor(sum(abs(AX3b$data[,2:4]))), 25367)
  
})

test_that("readAxivity reads data from AX6 file correctly", {
  cwafile  = system.file("testfiles/ax6_testfile.cwa", package = "GGIRread")[1]
  AX6 = readAxivity(filename = cwafile, desiredtz = "Europe/Berlin", start = 1, end = 4)
  expect_equal(AX6$header$device, "Axivity")
  expect_equal(nrow(AX6$data), 121)
  expect_equal(ncol(AX6$data), 10)
  expect_equal(AX6$data$time[5], 1577131447.144, tolerance = .001, scale = 1)
  expect_equal(AX6$data$temp[3], 27.34375, tolerance = 0.0001)
  expect_equal(floor(sum(abs(AX6$data[,2:4]))), 2007)
  expect_equal(floor(sum(abs(AX6$data[,5:7]))), 10)
  expect_equal(AX6$data[1,2], 0.2670288, tolerance = 0.0000001, scale = 1)
  expect_equal(AX6$data[4,3], -0.5026558, tolerance = 0.0000001, scale = 1)
  expect_equal(AX6$data[30,5], -0.009452909, tolerance = 0.0000001, scale = 1)
  expect_true(is.null(AX6$QClog))
  
  
  # ask for more data then there is in the file
  AX6b = readAxivity(filename = cwafile, desiredtz = "Europe/Berlin", start = 0, end = 1000)
  expect_equal(AX6b$header$device, "Axivity")
  expect_equal(nrow(AX6b$data), 11428)
  expect_equal(ncol(AX6b$data), 10)
  expect_equal(AX6b$data$time[5], 1577131446.740, tolerance = .001, scale = 1)
  expect_equal(AX6b$data$temp[3], 27.34375, tolerance = 0.0001)
  expect_equal(floor(sum(abs(AX6b$data[,2:4]))), 960046)
  expect_true(is.null(AX6b$QClog))
  
})


test_that("readAxivity reads timezones correctly", {
  cwafile  = system.file("testfiles/ax3_testfile.cwa", package = "GGIRread")[1]
  old <- options(digits.secs = 3)
  tzAms = "Europe/Amsterdam"
  tzLon = "Europe/London"
  # desiredtz == configtz
  tzequal = readAxivity(filename = cwafile, desiredtz = tzLon, configtz = tzLon, start = 1, end = 4)
  expect_equal(tzequal$data$time[1], 1551178507.215, tolerance = .001, scale = 1)
  expect_equal(format(as.POSIXlt(tzequal$data$time[1], tz = tzLon,
                                       origin = "1970-01-01")), "2019-02-26 10:55:07.215")
  
  # desiredtz < configtz
  tzwest = readAxivity(filename = cwafile, desiredtz = tzLon, configtz = tzAms, start = 1, end = 4)
  expect_equal(tzwest$data$time[1], 1551174907.215, tolerance = .001, scale = 1)
  expect_equal(format(as.POSIXlt(tzwest$data$time[1], tz = tzLon, 
                                       origin = "1970-01-01")), "2019-02-26 09:55:07.215")
  
  # desiredtz > configtz
  tzeast = readAxivity(filename = cwafile, desiredtz = tzAms, configtz = tzLon, start = 1, end = 4)
  expect_equal(tzeast$data$time[1], 1551178507.215, tolerance = .001, scale = 1)
  expect_equal(format(as.POSIXlt(tzeast$data$time[1], tz = tzAms, 
                                       origin = "1970-01-01")), "2019-02-26 11:55:07.215")
  options(old)
})