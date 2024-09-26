library(GGIRread)
context("read ActiGraph csv files")
test_that("ActiGraph61 is correctly read", {
  file = system.file("testfiles/ActiGraph61.csv", package = "GGIRread")
  D = readActiGraphCount(filename = file, desiredEpochSize = 10, timeformat = "%m/%d/%Y %H:%M:%S", tz =  "")
  expect_equal(D$deviceSerialNumber, "MOS2D16160581")
  expect_equal(D$epochSize, 10)
  expect_equal(format(D$startTime), "2016-08-15 21:35:00")
  expect_equal(nrow(D$data), 495)
  expect_equal(ncol(D$data), 4)
  expect_equal(sum(D$data), 63952.33)
  
  D = readActiGraphCount(filename = file, desiredEpochSize = 5, timeformat = "%m/%d/%Y %H:%M:%S", tz =  "")
  expect_equal(D$deviceSerialNumber, "MOS2D16160581")
  expect_equal(D$epochSize, 5)
  expect_equal(format(D$startTime), "2016-08-15 21:35:00")
  expect_equal(nrow(D$data), 990)
  expect_equal(ncol(D$data), 4)
  expect_equal(sum(D$data), 63952.33)
})

test_that("ActiGraph31 is correctly read", {
  file = system.file("testfiles/ActiGraph13.csv", package = "GGIRread")
  D = readActiGraphCount(filename = file, desiredEpochSize = 15, timeformat = "%m/%d/%Y %H:%M:%S", tz =  "")
  expect_equal(D$deviceSerialNumber, "CLE2A2123456")
  expect_equal(D$epochSize, 15)
  expect_equal(format(D$startTime), "2013-08-26 09:00:00")
  expect_equal(nrow(D$data), 990)
  expect_equal(ncol(D$data), 4)
  expect_equal(sum(D$data), 272870.6, tol = 0.1)
  
  D = readActiGraphCount(filename = file, desiredEpochSize = 30, timeformat = "%m/%d/%Y %H:%M:%S", tz =  "")
  expect_equal(D$deviceSerialNumber, "CLE2A2123456")
  expect_equal(D$epochSize, 30)
  expect_equal(format(D$startTime), "2013-08-26 09:00:00")
  expect_equal(nrow(D$data), 495)
  expect_equal(ncol(D$data), 4)
  expect_equal(sum(D$data), 272870.6, tol = 0.1)
})

test_that("ActiGraph13_timestamps_headers.csv is correctly read", {
  file = system.file("testfiles/ActiGraph13_timestamps_headers.csv", package = "GGIRread")
  D = readActiGraphCount(filename = file, desiredEpochSize = 1, timeformat = "%d-%m-%Y %H:%M:%S", tz =  "")
  expect_equal(D$deviceSerialNumber, "TAS1D48140206")
  expect_equal(D$epochSize, 1)
  expect_equal(format(D$startTime), "2017-12-09 15:00:00")
  expect_equal(nrow(D$data), 1000)
  expect_equal(ncol(D$data), 4)
  expect_equal(sum(D$data), 256047)
  
  
  D = readActiGraphCount(filename = file, desiredEpochSize = 5, timeformat = "%d-%m-%Y %H:%M:%S", tz =  "")
  expect_equal(D$deviceSerialNumber, "TAS1D48140206")
  expect_equal(D$epochSize, 5)
  expect_equal(format(D$startTime), "2017-12-09 15:00:00")
  expect_equal(nrow(D$data), 200)
  expect_equal(ncol(D$data), 4)
  expect_equal(sum(D$data), 256047)
  
  expect_error(readActiGraphCount(filename = file,
                                desiredEpochSize = 5,
                                timeformat = "%m/%d/%Y %H:%M:%S"))
  expect_error(readActiGraphCount(filename = file,
                                desiredEpochSize = 0.5,
                                timeformat = "%d-%m-%Y %H:%M:%S"))
})