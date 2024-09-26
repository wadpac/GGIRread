library(GGIRread)
context("read Actiwatch files")
test_that("Actiwatch csv is correctly read", {
  file = system.file("testfiles/Actiwatch.csv", package = "GGIRread")
  D = readActiwatchCount(filename = file, desiredEpochSize = 15, timeformat = "%d/%m/%Y %H:%M:%S", tz =  "")
  expect_equal(D$epochSize, 15)
  expect_equal(format(D$startTime), "2019-11-23 06:00:00")
  expect_equal(nrow(D$data), 860)
  expect_equal(ncol(D$data), 1)
  expect_equal(sum(D$data, na.rm = TRUE), 4589)
  
  D = readActiwatchCount(filename = file, desiredEpochSize = 30, timeformat = "%d/%m/%Y %H:%M:%S", tz =  "")
  expect_equal(D$epochSize, 30)
  expect_equal(format(D$startTime), "2019-11-23 06:00:00")
  expect_equal(nrow(D$data), 430)
  expect_equal(ncol(D$data), 1)
  expect_equal(sum(D$data, na.rm = TRUE), 4569)
})
test_that("Actiwatch awd is correctly read", {
  file = system.file("testfiles/Actiwatch.AWD", package = "GGIRread")
  D = readActiwatchCount(filename = file, desiredEpochSize = 60, timeformat = "%d-%b-%Y %H:%M:%S", tz =  "")
  expect_equal(D$epochSize, 60)
  expect_equal(format(D$startTime), "2009-10-01 17:00:00")
  expect_equal(nrow(D$data), 329)
  expect_equal(ncol(D$data), 1)
  expect_equal(sum(D$data, na.rm = TRUE), 108864)
  
  D = readActiwatchCount(filename = file, desiredEpochSize = 300, timeformat =  "%d-%b-%Y %H:%M:%S", tz =  "")
  expect_equal(D$epochSize, 300)
  expect_equal(format(D$startTime), "2009-10-01 17:00:00")
  expect_equal(nrow(D$data), 65)
  expect_equal(ncol(D$data), 1)
  expect_equal(sum(D$data, na.rm = TRUE), 108713)
})

test_that("Actiwatch awd error correctly", {
  file = system.file("testfiles/Actiwatch.AWD", package = "GGIRread")
  expect_error(readActiwatchCount(filename = file,
                                  desiredEpochSize = 60,
                                  timeformat = "%d-%m-%Y %H:%M:%S"),
               regexp = "Time format*")

  expect_error(readActiwatchCount(filename = file,
                                  desiredEpochSize = 5,
                                  timeformat = "%d-%b-%Y %H:%M:%S"),
               regexp = "The short*")
  
  expect_error(readActiwatchCount(filename = "",
                                  desiredEpochSize = 60,
                                  timeformat = "%d-%b-%Y %H:%M:%S"),
               regexp = "Cannot")
  
})


"\nThe short epoch size as specified by the user (5 seconds) does NOT match the short epoch size we see in the data (60 seconds). Please correct."