library(GGIRread)
context("readWav")
test_that("readWav reads data in file correctly", {
  wavfile  = system.file("testfiles/ax3test.wav", package = "GGIRread")[1]
  WAV = expect_warning(readWav(filename = wavfile, units = "samples", start = 1, end = 100))
  expect_equal(nrow(WAV$rawxyz), 100)
  expect_equal(round(sum(WAV$rawxyz),digits = 2), -97.08)
  expect_equal(as.character(WAV$header$hnames[10]), "Channel-1")
  expect_equal(as.character(WAV$header$hvalues[11]), "8")
})