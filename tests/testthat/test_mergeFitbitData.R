library(GGIRread)
context("merge PHB files")
test_that("merging of PHB files goes correctly", {
  file1 = system.file("testfiles/steps-1995-06-23_Fitbit.json", package = "GGIRread")
  file2 = system.file("testfiles/calories-1995-06-23_Fitbit.json", package = "GGIRread")
  file3 = system.file("testfiles/sleep-1995-06-23_Fitbit.json", package = "GGIRread")

  # apply function to merge the files
  D = mergeFitbitData(filenames = c(file1, file2),
                    desiredtz =  "Europe/Amsterdam")
  expect_equal(nrow(D), 93)
  expect_equal(ncol(D), 3)
  expect_equal(sum(D$steps, na.rm = TRUE), 607)
  expect_equal(sum(D$calories, na.rm = TRUE), 68.82, tol = 0.01)
  expect_equal(format(D$dateTime[1]), "1995-06-24 16:00:00")
  
  # apply function to merge the files
  expect_warning(mergeFitbitData(filenames = c(file1, file2, file3),
                      desiredtz =  "Europe/Amsterdam"),
               regexp = "Time series*")
})
 