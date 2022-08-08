library(GGIRread)
context("reading GENEActiv binary data")
test_that("GENEActivReader reads data from file correctly", {
  skip_on_cran()
  binfile  = system.file("testfiles/GENEActiv_testfile.bin", package = "GGIRread")[1]
  cppdata = GENEActivReader(filename = binfile, start = 1, end = 1)
  
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
  skip_on_cran()
  binfile  = system.file("testfiles/GENEActiv_testfile.bin", package = "GGIRread")[1]
  rdata = readGENEActiv(filename = binfile, start = 1, end = 1, desiredtz = "Europe/London")
  
  expect_equal(rdata$header$ReadOK, 1)
  expect_equal(rdata$header$ReadErrors, 0)
  expect_equal(rdata$header$SampleRate, 85.7)
  expect_equal(rdata$header$numBlocksTotal, 222048)
  expect_equal(length(rdata$data$time), 300)
  expect_equal(length(rdata$data$x), 300)
  expect_equal(length(rdata$data$y), 300)
  expect_equal(length(rdata$data$z), 300)
  expect_equal(length(rdata$data$temperature), 300)
  expect_equal(length(rdata$data$light), 300)
  expect_equal(rdata$data$temperature[1], 21.5)
  expect_equal(rdata$data$light[2], 2.666667, tolerance = 4)
  expect_equal(rdata$data$z[300], -0.80836403369903564453, tolerance = 15)
  expect_equal(rdata$data$time[1], 1369908774.500) # output is now expressed in seconds rather than milliseconds
  
})
