library(GGIRread)
context("read Actiwatch files")
test_that("Actiwatch csv is correctly read", {
  file = system.file("testfiles/Actiwatch.csv", package = "GGIRread")
  D = readActiwatchCount(filename = file, timeformat = "%d/%m/%Y %H:%M:%S",
                         desiredtz =  "Europe/Amsterdam")
  expect_equal(D$epochSize, 15)
  expect_equal(format(D$startTime), "2019-11-23 06:00:00")
  expect_equal(nrow(D$data), 860)
  expect_equal(ncol(D$data), 3)
  expect_equal(sum(D$data[, "counts"], na.rm = TRUE), 4589)
  expect_equal(sum(D$data[, "sleep"], na.rm = TRUE), 55)
  expect_equal(sum(D$data[, "nonwear"], na.rm = TRUE), 797)
})
test_that("Actiwatch awd is correctly read", {
  file = system.file("testfiles/Actiwatch.AWD", package = "GGIRread")
  D = readActiwatchCount(filename = file, timeformat = "%d-%b-%Y %H:%M:%S",
                         desiredtz =  "Europe/Amsterdam")
  expect_equal(D$epochSize, 60)
  expect_equal(format(D$startTime), "2009-10-01 17:00:00")
  expect_equal(nrow(D$data), 329)
  expect_equal(ncol(D$data), 3)
  expect_equal(sum(D$data[, "counts"], na.rm = TRUE), 108864)
  expect_equal(sum(D$data[, "light"], na.rm = TRUE), 0)
  expect_equal(sum(D$data[, "marker"], na.rm = TRUE), 2)
})

test_that("Actiwatch awd error correctly", {
  file = system.file("testfiles/Actiwatch.AWD", package = "GGIRread")
  expect_error(readActiwatchCount(filename = file,
                                  timeformat = "%d-%m-%Y %H:%M:%S"),
               regexp = "Time format*")

  expect_error(readActiwatchCount(filename = "",
                                  timeformat = "%d-%b-%Y %H:%M:%S"),
               regexp = "Cannot")
  
})

test_that("checkTimeFormat also detects implausible year", {
  rawValue = "6/28/21 10:10:10"
  timeformat = "%m/%d/%y %H:%M:%S"
  timestamp_POSIX = as.POSIXlt("6/28/21 10:10:10", format = "%m/%d/%Y %H:%M:%S")
  expect_error(checkTimeFormat(timestamp_POSIX, rawValue = rawValue, timeformat = timeformat,
                             timeformatName = NULL),
               regexpr = "Timestamp recognised as 0021*")

})

test_that("Timezones are correctly handled", {
  file = system.file("testfiles/Actiwatch.csv", package = "GGIRread")
  # Configured and worn in same place
  D = readActiwatchCount(filename = file, timeformat = "%d/%m/%Y %H:%M:%S",
                         desiredtz =  "Europe/Amsterdam")
  expect_equal(format(D$startTime), "2019-11-23 06:00:00")
  
  # Configured 1 hour earlier than timezone where device was worn
  D = readActiwatchCount(filename = file, timeformat = "%d/%m/%Y %H:%M:%S",
                         desiredtz =  "Europe/London", configtz =  "Europe/Amsterdam")
  expect_equal(format(D$startTime), "2019-11-23 05:00:00")
  
  # Configured 6 hours later than timezone where device was worn
  D = readActiwatchCount(filename = file, timeformat = "%d/%m/%Y %H:%M:%S",
                         desiredtz =  "Europe/Amsterdam", configtz =  "America/New_York")
  expect_equal(format(D$startTime), "2019-11-23 12:00:00")
})