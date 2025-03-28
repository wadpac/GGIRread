library(GGIRread)
context("reading Parmay Matrix .BIN data")
test_that("File including only accelerometer data at 12.5Hz", {
  binfile  = system.file("testfiles/mtx_12.5Hz_acc.BIN", package = "GGIRread")[1]
  BIN = readParmayMatrix(filename = binfile, desiredtz = "Europe/Berlin", start = 1, end = NULL)
  expect_equal(BIN$header$sf, 12.5)
  expect_equal(BIN$header$acc_dynrange, 8)
  expect_equal(as.numeric(BIN$header$starttime), BIN$data$time[1])
  expect_equal(nrow(BIN$data), 12587)
  expect_equal(ncol(BIN$data), 5) # time, x, y, z, remarks
  expect_equal(nrow(BIN$QClog), 2) # 2 packets in file
  expect_true(all(BIN$QClog$checksum_pass))
})

test_that("File including accelerometer and heart rate data at 25Hz", {
  binfile  = system.file("testfiles/mtx_25Hz_acc_HR.BIN", package = "GGIRread")[1]
  BIN = readParmayMatrix(filename = binfile, desiredtz = "Europe/Berlin", start = 1, end = NULL)
  expect_equal(BIN$header$sf, 25)
  expect_equal(BIN$header$acc_dynrange, 8)
  expect_equal(as.numeric(BIN$header$starttime), BIN$data$time[1])
  expect_equal(nrow(BIN$data), 24525)
  expect_equal(ncol(BIN$data), 5) # time, x, y, z, remarks (no hr because deactivated by default)
  expect_equal(nrow(BIN$QClog), 4) # 4 packets in file
  expect_true(all(BIN$QClog$checksum_pass))
  BIN = readParmayMatrix(filename = binfile, desiredtz = "Europe/Berlin", start = 1, end = NULL,
                         read_heart = TRUE)
  expect_equal(ncol(BIN$data), 7) # time, x, y, z, hr_raw, hr, remarks
})

test_that("File including accelerometer, heart rate, and temperature data at 100Hz", {
  binfile  = system.file("testfiles/mtx_100Hz_acc_HR_temp.BIN", package = "GGIRread")[1]
  BIN = readParmayMatrix(filename = binfile, desiredtz = "Europe/Berlin", start = 1, end = NULL)
  expect_equal(BIN$header$sf, 100)
  expect_equal(BIN$header$acc_dynrange, 8)
  expect_equal(as.numeric(BIN$header$starttime), BIN$data$time[1])
  expect_equal(nrow(BIN$data), 39400)
  expect_equal(ncol(BIN$data), 7) # time, x, y, z, body temp, ambient temp,remarks
  expect_equal(nrow(BIN$QClog), 4) # 4 packets in file
  expect_true(all(BIN$QClog$checksum_pass))
  BIN = readParmayMatrix(filename = binfile, desiredtz = "Europe/Berlin", start = 1, end = NULL,
                         read_heart = TRUE)
  expect_equal(ncol(BIN$data), 9) # time, x, y, z, body temp, ambient temp, hr_raw, hr, remarks
})


test_that("File including accelerometer and gyroscope", {
  binfile  = system.file("testfiles/mtx_12.5Hz_acc_gyro.BIN", package = "GGIRread")[1]
  BIN = readParmayMatrix(filename = binfile, desiredtz = "Europe/Berlin", start = 1, end = NULL)
  expect_equal(BIN$header$sf, 12.5)
  expect_equal(BIN$header$acc_dynrange, 8)
  expect_equal(as.numeric(BIN$header$starttime), BIN$data$time[1])
  expect_equal(nrow(BIN$data), 1425)
  expect_equal(ncol(BIN$data), 7) # time, x, y, z, body temp, ambient temp,remarks
  expect_equal(nrow(BIN$QClog), 1) # 1 packet in file
  expect_true(all(BIN$QClog$checksum_pass))
  BIN = readParmayMatrix(filename = binfile, desiredtz = "Europe/Berlin", start = 1, end = NULL,
                         read_gyro = TRUE)
  expect_equal(ncol(BIN$data), 10) # time, x, y, z, gyro (x, y, z) body temp, ambient temp, hr_raw, hr, remarks
})


test_that("Identification of corrupt files and foreseen errors", {
  binfile  = system.file("testfiles/mtx_corrupted.bin", package = "GGIRread")[1]
  BIN = readParmayMatrix(filename = binfile, desiredtz = "Europe/Berlin", start = 1, end = NULL)
  expect_equal(BIN$QClog$checksum_pass, c(F, T, T, T))
  expect_equal(BIN$header$sf, 100)
  expect_equal(BIN$header$acc_dynrange, 8)
  expect_equal(as.numeric(BIN$header$starttime), BIN$data$time[1])
  expect_equal(nrow(BIN$data), 39400)
  # in corrupt packet (#1), acc data should be 0,0,1, and temperature should be NA
  # check this in first row
  expect_equal(BIN$data$acc_x[1], 0)
  expect_equal(BIN$data$acc_y[1], 0)
  expect_equal(BIN$data$acc_z[1], 1)
  expect_true(is.na(BIN$data$bodySurface_temp[1]))
  expect_true(is.na(BIN$data$ambient_temp[1]))
  # also, identification of full corrupted file
  binfile  = system.file("testfiles/mtx_corrupted_full.bin", package = "GGIRread")[1]
  expect_error(readParmayMatrix(filename = binfile, desiredtz = "Europe/Berlin", 
                                start = 1, end = NULL), 
               regexp = "Invalid header recognition string")
  # wrong indexing
  binfile  = system.file("testfiles/mtx_12.5Hz_acc_gyro.BIN", package = "GGIRread")[1]
  BIN = readParmayMatrix(filename = binfile, desiredtz = "Europe/Berlin", start = -1, end = NULL)
  expect_equal(nrow(BIN$QClog), 1) # 1 packet in file
  BIN = readParmayMatrix(filename = binfile, desiredtz = "Europe/Berlin", start = 8, end = NULL)
  expect_true(is.null(BIN$QClog)) # start is beyond total packets, meaning file was already fully read and this should be NULL
})