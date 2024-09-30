library(GGIRread)
context("merge PHB files")
test_that("merging of PHB files goes correctly", {
  file1 = system.file("testfiles/DataList_AH1234567890_PhilipsHealthBand.xlsx", package = "GGIRread")
  file2 = system.file("testfiles/Sleep_Wake_AH1234567890_PhilipsHealthBand.xlsx", package = "GGIRread")
  # apply function to merge the files
  D = mergePHBdata(filenames = c(file1, file2),
                    timeformat = "%m-%d-%Y %H:%M:%S",
                    desiredtz =  "Europe/Amsterdam")
  expect_equal(nrow(D$data), 246)
  expect_equal(ncol(D$data), 19)
  expect_equal(sum(D$data$counts), 50898)
  expect_equal(sum(D$data$steps), 1)
  expect_equal(format(D$data$timestamp[1]), "2022-11-05 01:10:00")
  expect_equal(D$deviceSN, "000100621938")
})
 