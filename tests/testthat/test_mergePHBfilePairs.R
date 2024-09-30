library(GGIRread)
context("merge PHB files")
test_that("merging of PHB files goes correctly", {
  # prepare folder with test files
  file1 = system.file("testfiles/DataList_AH1234567890_PhilipsHealthBand.xlsx", package = "GGIRread")
  file2 = system.file("testfiles/Sleep_Wake_AH1234567890_PhilipsHealthBand.xlsx", package = "GGIRread")
  path = "./phb_test_folder"
  if (!dir.exists(path)) {
    dir.create(path)
  }
  invisible(file.copy(from = file1, to = path, overwrite = TRUE))
  invisible(file.copy(from = file2, to = path, overwrite = TRUE))
  
  # apply function to merge the files
  mergePHBfilePairs(inputPath = path, outputPath = path,
                    timeformat = "%m-%d-%Y %H:%M:%S",
                    desiredtz =  "Europe/Amsterdam")
  
  # check whether merged file exists
  newfiles = dir(path, full.names = TRUE)
  newFile = newfiles[grep(pattern = "def_AH1234567890_PhilipsHealthBand_000100621938.csv", x = newfiles)]
  expect_true(length(newFile) == 1)
  
  # check file content
  data = read.csv(newFile)
  expect_equal(nrow(data), 246)
  expect_equal(ncol(data), 19)
  expect_equal(sum(data$counts), 50898)
  expect_equal(sum(data$steps), 1)
  expect_equal(data$timestamp[1], "2022-11-05 01:10:00")
  
  # clean up
  if (dir.exists(path))  unlink(path, recursive = TRUE)
})
 