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
  
  # ask for more data then there is in the file
  AX3b = readAxivity(filename = cwafile, desiredtz = "Europe/Berlin", start = 1, end = 1000)
  expect_equal(AX3b$header$device, "Axivity")
  expect_equal(nrow(AX3b$data), 17299)
  expect_equal(ncol(AX3b$data), 7)
  expect_equal(AX3b$data$time[5], 1551174909)
  expect_equal(AX3b$data$temp[3], 18.65)
  expect_equal(floor(sum(abs(AX3b$data[,2:4]))), 24873)
  
})