library(GGIRread)
context("read Fitbit json")
test_that("Fitbit json is correctly read", {
  # Sleep
  file = system.file("testfiles/sleep-1995-06-23_Fitbit.json", package = "GGIRread")
  D = readFitbit(filename = file)
  expect_equal(nrow(D), 695)
  expect_equal(ncol(D), 3)
  expect_equal(format(D$dateTime[1]), "1995-07-11 02:28:30")
  TB = table(D$sleeplevel)
  expect_equal(names(TB), c("asleep", "awake", "deep", "light", "rem", "restless", "wake"))
  expect_equal(as.numeric(TB), c(146, 2, 118, 283, 71, 10, 65))
  
  # Steps
  file = system.file("testfiles/steps-1995-06-23_Fitbit.json", package = "GGIRread")
  D = readFitbit(filename = file)
  expect_equal(nrow(D), 34)
  expect_equal(ncol(D), 2)
  expect_equal(format(D$dateTime[1]), "1995-06-24 16:00:00")
  expect_equal(sum(D$steps), 607)
  
  # Calories
  file = system.file("testfiles/calories-1995-06-23_Fitbit.json", package = "GGIRread")
  D = readFitbit(filename = file)
  expect_equal(nrow(D), 47)
  expect_equal(ncol(D), 2)
  expect_equal(format(D$dateTime[1]), "1995-06-23")
  expect_equal(sum(D$calories), 69.56)
})