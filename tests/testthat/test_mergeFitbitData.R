library(GGIRread)
context("merge PHB files")
test_that("merging of PHB files goes correctly", {
  
  file1 = system.file("testfiles/steps-1995-06-23_Fitbit.json", package = "GGIRread")
  file2 = system.file("testfiles/calories-1995-06-23_Fitbit.json", package = "GGIRread")
  file3 = system.file("testfiles/sleep-1995-06-23_Fitbit.json", package = "GGIRread")
  file4 = system.file("testfiles/heart_rate-1995-06-24.json", package = "GGIRread")

  # apply function to merge the files
  D = mergeFitbitData(filenames = c(file1, file2),
                    desiredtz =  "Europe/Amsterdam")
  expect_equal(nrow(D), 93)
  expect_equal(ncol(D), 3)
  expect_equal(sum(D$steps, na.rm = TRUE), 607)
  expect_equal(sum(D$calories, na.rm = TRUE), 68.82, tol = 0.01)
  expect_equal(format(D$dateTime[1]), "1995-06-24 16:00:00")
  
  # apply function to merge tree files
  D2 = mergeFitbitData(filenames = c(file1, file2, file3),
                      desiredtz =  "Europe/Amsterdam")
  expect_true(all(colnames(D2) %in% c("dateTime", "steps", "calories", "sleeplevel")))
  expect_equal(nrow(D2), 47874)
  
  
  # apply function to merge all four files
  D3 = mergeFitbitData(filenames = c(file1, file2, file3, file4),
                       desiredtz =  "Europe/Amsterdam")
  expect_true(all(colnames(D2) %in% c("dateTime", "steps", "calories", "sleeplevel", "heart_rate")))
  expect_equal(nrow(D2), 47874)
  expect_equal(mean(D3$heart_rate, na.rm = TRUE), 77.18201, tolerance = 0.0001)
  expect_equal(sum(D3$steps, na.rm = TRUE), 607)
  rm(D3, D2, D)
})
 