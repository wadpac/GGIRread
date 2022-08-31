library(GGIRread)
context("reading GENEActiv binary data")
test_that("GENEActivReader reads data from file correctly", {
  binfile  = system.file("testfiles/GENEActiv_testfile.bin", package = "GGIRread")[1]
  cppdata = GENEActivReader(filename = binfile, start = 1, end = 1, progress_bar = TRUE)
  
  expect_equal(cppdata$info$ReadOK, 1)
  expect_equal(cppdata$info$ReadErrors, 0)
  expect_equal(cppdata$info$SampleRate, 85.7)
  expect_equal(cppdata$info$numBlocksTotal, 222048)
  expect_equal(length(cppdata$time), 300)
  expect_equal(length(cppdata$x), 300)
  expect_equal(length(cppdata$y), 300)
  expect_equal(length(cppdata$z), 300)
  expect_equal(length(cppdata$temperature), 300)
  expect_equal(cppdata$temperature[1], 21.5)
  expect_equal(cppdata$z[300], -0.80836403369903564453, tolerance = 15)
  expect_equal(cppdata$time[1], 0)
})

test_that("readGENEActiv reads data from file correctly", {
  old <- options(digits.secs = 3)
  binfile  = system.file("testfiles/GENEActiv_testfile.bin", package = "GGIRread")[1]
  rdata = readGENEActiv(filename = binfile, start = 1, end = 1, desiredtz = "Europe/London")
  
  expect_equal(rdata$header$ReadOK, 1)
  expect_equal(rdata$header$ReadErrors, 0)
  expect_equal(rdata$header$SampleRate, 85.7)
  expect_equal(rdata$header$numBlocksTotal, 222048)
  expect_equal(rdata$header$Handedness, "")
  expect_equal(rdata$header$RecordingID, "")
  expect_equal(rdata$header$DeviceLocation, "")
  expect_equal(rdata$header$DeviceModel, "1.1")
  expect_equal(length(rdata$data$time), 300)
  expect_equal(length(rdata$data$x), 300)
  expect_equal(length(rdata$data$y), 300)
  expect_equal(length(rdata$data$z), 300)
  expect_equal(length(rdata$data$temperature), 300)
  expect_equal(length(rdata$data$light), 300)
  expect_equal(rdata$data$temperature[1], 21.5)
  expect_equal(rdata$data$light[2], 2.666667, tolerance = 4)
  expect_equal(rdata$data$z[300], -0.80836403369903564453, tolerance = 15)
  expect_equal(rdata$data$time[1], 1369905174.500) # output is now expressed in seconds rather than milliseconds
  
  
  tzAms = "Europe/Amsterdam"
  tzLon = "Europe/London"
  
  # desiredtz == configtz
  rdata2 = readGENEActiv(filename = binfile, start = 1, end = 1, desiredtz = tzLon, configtz = tzLon)
  expect_equal(rdata2$data.out$time[1], 1369905174.500)
  expect_equal(as.character(as.POSIXlt(rdata2$data.out$time[1], tz = tzLon, origin = "1970-01-01")), "2013-05-30 10:12:54.5")
  
  # desiredtz < configtz
  rdata3 = readGENEActiv(filename = binfile, start = 1, end = 1, desiredtz = tzLon, configtz = tzAms)
  expect_equal(rdata3$data.out$time[1], 1369901574.500)
  expect_equal(as.character(as.POSIXlt(rdata3$data.out$time[1], tz = tzLon, origin = "1970-01-01")), "2013-05-30 09:12:54.5")
  
  # desiredtz > configtz
  rdata4 = readGENEActiv(filename = binfile, start = 1, end = 1, desiredtz = tzAms, configtz = tzLon)  
  expect_equal(rdata4$data.out$time[1], 1369905174.500)
  expect_equal(as.character(as.POSIXlt(rdata4$data.out$time[1], tz = tzAms, origin = "1970-01-01")), "2013-05-30 11:12:54.5")
  options(old)
})
