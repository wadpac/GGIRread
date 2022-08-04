library(GGIRread)
context("GENEActivReader")
test_that("GENEActivReader reads data from file correctly", {
  skip_on_cran()
  binfile  = system.file("testfiles/GENEActiv_testfile.bin", package = "GGIRread")[1]
  GENEActiv = GENEActivReader(filename = binfile, start = 1, end = 1, tzone = 3600)
  
  expect_equal(GENEActiv$info$ReadOK, 1)
  expect_equal(GENEActiv$info$ReadErrors, 0)
  expect_equal(GENEActiv$info$SampleRate, 85.7)
  expect_equal(GENEActiv$info$numBlocksTotal, 222048)
  expect_equal(length(GENEActiv$time), 300)
  expect_equal(length(GENEActiv$x), 300)
  expect_equal(length(GENEActiv$y), 300)
  expect_equal(length(GENEActiv$z), 300)
  expect_equal(length(GENEActiv$T), 300)
  expect_equal(GENEActiv$T[1], 21.5)
  expect_equal(GENEActiv$z[300], -0.80836403369903564453, tolerance = 15)
  expect_equal(GENEActiv$time[1], 1369908774500)
  
  GBR = readGENEActiv(filename = binfile, start = 1, end = 1)
  expect_equal(GBR$header$ReadOK, 1)
  expect_equal(GBR$header$ReadErrors, 0)
  expect_equal(GBR$header$SampleRate, 85.7)
  expect_equal(GBR$header$numBlocksTotal, 222048)
  expect_equal(length(GBR$data$time), 300)
  expect_equal(length(GBR$data$x), 300)
  expect_equal(length(GBR$data$y), 300)
  expect_equal(length(GBR$data$z), 300)
  expect_equal(length(GBR$data$temperature), 300)
  expect_equal(length(GBR$data$light), 300)
  expect_equal(GBR$data$temperature[1], 21.5)
  expect_equal(GBR$data$light[2], 2.666667, tolerance = 4)
  expect_equal(GBR$data$z[300], -0.80836403369903564453, tolerance = 15)
  cat(paste0("\nUnit test, first timestamp: ", GBR$data$time[1], "\n"))
  expect_equal(GBR$data$time[1], 1369908774500) # output is now expressed in seconds rather than milliseconds

})

