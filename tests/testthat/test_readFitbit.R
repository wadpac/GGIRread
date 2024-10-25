library(GGIRread)
context("read Fitbit json")
test_that("Fitbit json is correctly read", {
  # Sleep
  file = system.file("testfiles/sleep-1995-06-23_Fitbit.json", package = "GGIRread")
  D = readFitbit(filename = file, desiredtz =  "Europe/Amsterdam")
  expect_equal(nrow(D), 695)
  expect_equal(ncol(D), 3)
  expect_equal(format(D$dateTime[1]), "1995-07-11 02:28:30")
  TB = table(D$sleeplevel)
  expect_equal(names(TB), c("asleep", "awake", "deep", "light", "rem", "restless", "wake"))
  expect_equal(as.numeric(TB), c(146, 2, 118, 283, 71, 10, 65))
  
  # Steps
  file = system.file("testfiles/steps-1995-06-23_Fitbit.json", package = "GGIRread")
  D = readFitbit(filename = file, desiredtz =  "Europe/Amsterdam")
  expect_equal(nrow(D), 71)
  expect_equal(ncol(D), 2)
  expect_equal(format(D$dateTime[1]), "1995-06-24 16:00:00")
  expect_equal(sum(D$steps, na.rm = TRUE), 607)
  
  # Calories
  file = system.file("testfiles/calories-1995-06-23_Fitbit.json", package = "GGIRread")
  D = readFitbit(filename = file, desiredtz =  "Europe/Amsterdam")
  expect_equal(nrow(D), 93)
  expect_equal(ncol(D), 2)
  expect_equal(format(D$dateTime[1]), "1995-06-24 16:00:00")
  expect_equal(sum(D$calories, na.rm = TRUE), 68.82, tol = 0.01)
})

test_that("Timezones are correctly handled", {
  file = system.file("testfiles/sleep-1995-06-23_Fitbit.json", package = "GGIRread")
  # Configured and worn in same place
  D = readFitbit(filename = file, desiredtz =  "Europe/Amsterdam")
  expect_equal(format(D$dateTime[1]), "1995-07-11 02:28:30")
  
  # Configured 1 hour earlier than timezone where device was worn
  D = readFitbit(filename = file, desiredtz =  "Europe/London", configtz =  "Europe/Amsterdam")
  expect_equal(format(D$dateTime[1]), "1995-07-11 01:28:30")
  
  # Configured 6 hours later than timezone where device was worn
  D = readFitbit(filename = file, desiredtz =  "Europe/Amsterdam",
                 configtz =  "America/New_York")
  expect_equal(format(D$dateTime[1]), "1995-07-11 08:28:30")
})
