library(GGIRread)
context("read Actical files")
test_that("Actical csv is correctly read", {
  file = system.file("testfiles/Actical.csv", package = "GGIRread")
  D = readActicalCount(filename = file, timeformat = "%d-%b-%y %H:%M",
                       desiredtz =  "Europe/Amsterdam")
  expect_equal(D$epochSize, 60)
  expect_equal(format(D$startTime), "2021-05-13")
  expect_equal(nrow(D$data), 501)
  expect_equal(ncol(D$data), 2)
  expect_equal(sum(D$data, na.rm = TRUE), 8436)
})
 
test_that("Actical csv error correctly", {
  file = system.file("testfiles/Actical.csv", package = "GGIRread")
  expect_error(readActicalCount(filename = file, 
                                timeformat = "%d/%m/%Y %H:%M",
                                desiredtz =  "Europe/Amsterdam"),
               regexp = "Time format*")
})


test_that("Timezones are correctly handled", {
  file = system.file("testfiles/Actical.csv", package = "GGIRread")
  # Configured and worn in same place
  D = readActicalCount(filename = file, timeformat = "%d-%b-%y %H:%M",
                         desiredtz =  "Europe/Amsterdam")
  expect_equal(format(D$startTime), "2021-05-13")
  
  # Configured 1 hour earlier than timezone where device was worn
  D = readActicalCount(filename = file, timeformat = "%d-%b-%y %H:%M",
                         desiredtz =  "Europe/London", configtz =  "Europe/Amsterdam")
  expect_equal(format(D$startTime), "2021-05-12 23:00:00")
  
  # Configured 6 hours later than timezone where device was worn
  D = readActicalCount(filename = file, timeformat = "%d-%b-%y %H:%M",
                         desiredtz =  "Europe/Amsterdam", configtz =  "America/New_York")
  expect_equal(format(D$startTime), "2021-05-13 06:00:00")
})
