library(GGIRread)
context("reading Parmay Matrix .bin data")
test_that("File including only accelerometer data at 12.5Hz", {
  binfile  = system.file("testfiles/mtx_12.5Hz_acc.BIN", package = "GGIRread")[1]
  BIN = readParmayMatrix(bin_file = binfile, desiredtz = "Europe/Berlin", start = 1, end = NULL)
  expect_equal(BIN$sf, 12.5)
  expect_equal(BIN$acc_dynrange, 8)
  expect_equal(as.numeric(BIN$starttime), BIN$data$time[1])
  expect_equal(nrow(BIN$data), 12587)
  expect_equal(ncol(BIN$data), 5) # time, x, y, z, remarks
  expect_equal(nrow(BIN$QClog), 2) # 2 packets in file
  expect_true(all(BIN$QClog$checksum_pass))
})

test_that("File including accelerometer and heart rate data at 25Hz", {
  binfile  = system.file("testfiles/mtx_25Hz_acc_HR.BIN", package = "GGIRread")[1]
  BIN = readParmayMatrix(bin_file = binfile, desiredtz = "Europe/Berlin", start = 1, end = NULL)
  expect_equal(BIN$sf, 25)
  expect_equal(BIN$acc_dynrange, 8)
  expect_equal(as.numeric(BIN$starttime), BIN$data$time[1])
  expect_equal(nrow(BIN$data), 24525)
  expect_equal(ncol(BIN$data), 7) # time, x, y, z, hr_raw, hr, remarks
  expect_equal(nrow(BIN$QClog), 4) # 4 packets in file
  expect_true(all(BIN$QClog$checksum_pass))
})

test_that("File including accelerometer, heart rate, and temperature data at 100Hz", {
  binfile  = system.file("testfiles/mtx_100Hz_acc_HR_temp.BIN", package = "GGIRread")[1]
  BIN = readParmayMatrix(bin_file = binfile, desiredtz = "Europe/Berlin", start = 1, end = NULL)
  expect_equal(BIN$sf, 100)
  expect_equal(BIN$acc_dynrange, 8)
  expect_equal(as.numeric(BIN$starttime), BIN$data$time[1])
  expect_equal(nrow(BIN$data), 39400)
  expect_equal(ncol(BIN$data), 9) # time, x, y, z, boyd temp, ambient temp, hr_raw, hr, remarks
  expect_equal(nrow(BIN$QClog), 4) # 4 packets in file
  expect_true(all(BIN$QClog$checksum_pass))
})
