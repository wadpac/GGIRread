library(GGIRread)
context("reading Axivity .cwa data")
test_that("readAxivity reads data from file correctly", {
  skip_on_cran()
  cwafile  = system.file("testfiles/ax3_testfile.cwa", package = "GGIRread")[1]
  AX3 = readAxivity(filename = cwafile, desiredtz = "Europe/Berlin", start = 1, end = 4)
  expect_equal(AX3$header$device, "Axivity")
  expect_equal(nrow(AX3$data), 900)
  expect_equal(ncol(AX3$data), 7)
  expect_equal(AX3$data$time[5], 1551174909)
  expect_equal(AX3$data$temp[3], 18.65)
  expect_equal(floor(sum(abs(AX3$data[,2:4]))), 1407)
})