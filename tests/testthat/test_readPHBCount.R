library(GGIRread)
context("read PHB files")
test_that("PHB Datalist xlsx is correctly read", {
  file = system.file("testfiles/DataList_AH1234567890_PhilipsHealthBand.xlsx", package = "GGIRread")
  D = readPHBCount(filename = file, timeformat = "%m-%d-%Y %H:%M:%S",
                       desiredtz =  "Europe/Amsterdam")
  expect_equal(format(D$data$timestamp[1]), "2022-11-04 23:58:00")
  expect_equal(nrow(D$data), 491)
  expect_equal(ncol(D$data), 18)
  expect_equal(sum(D$data$counts, na.rm = TRUE), 240393)
})
 

test_that("PHB Sleep_wake xlsx is correctly read", {
  file = system.file("testfiles/Sleep_Wake_AH1234567890_PhilipsHealthBand.xlsx", package = "GGIRread")
  D = readPHBCount(filename = file, timeformat = "%m-%d-%Y %H:%M:%S",
                   desiredtz =  "Europe/Amsterdam")
  expect_equal(format(D$data$timestamp[1]), "2022-11-05 01:10:00")
  expect_equal(nrow(D$data), 491)
  expect_equal(ncol(D$data), 3)
  expect_equal(sum(D$data$sleep, na.rm = TRUE), 445)
})

test_that("PHB xlsx error correctly", {
  file = system.file("testfiles/Sleep_Wake_AH1234567890_PhilipsHealthBand.xlsx", package = "GGIRread")
  expect_error(readPHBCount(filename = file, 
                                timeformat = "%d/%m/%Y %H:%M",
                                desiredtz =  "Europe/Amsterdam"),
               regexp = "Time format*")
})


test_that("Timezones are correctly handled", {
  file = system.file("testfiles/DataList_AH1234567890_PhilipsHealthBand.xlsx", package = "GGIRread")
  # Configured and worn in same place
  D = readPHBCount(filename = file, timeformat = "%m-%d-%Y %H:%M:%S",
                         desiredtz =  "Europe/Amsterdam")
  expect_equal(format(D$data$timestamp[1]), "2022-11-04 23:58:00")
  
  # Configured 1 hour earlier than timezone where device was worn
  D = readPHBCount(filename = file, timeformat = "%m-%d-%Y %H:%M:%S",
                         desiredtz =  "Europe/London", configtz =  "Europe/Amsterdam")
  expect_equal(format(D$data$timestamp[1]), "2022-11-04 22:58:00")
  
  # Configured 6 hours later than timezone where device was worn
  D = readPHBCount(filename = file, timeformat = "%m-%d-%Y %H:%M:%S",
                         desiredtz =  "Europe/Amsterdam", configtz =  "America/New_York")
  expect_equal(format(D$data$timestamp[1]), "2022-11-05 04:58:00")
})
