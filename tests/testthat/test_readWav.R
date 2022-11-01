library(GGIRread)
context("readWav")
test_that("readWav reads data in file correctly", {
  wavfile  = system.file("testfiles/ax3test.wav", package = "GGIRread")[1]
  WAV = readWav(filename = wavfile, units = "samples", start = 1, end = 100)
  expect_equal(nrow(WAV$rawxyz), 100)
  expect_equal(round(sum(WAV$rawxyz),digits = 2), -97.08)
  expect_equal(as.character(WAV$header$hvalues[which(as.character(WAV$header$hnames) == "Channel-1")]), "Accel-X")
  expect_equal(as.character(WAV$header$hvalues[which(as.character(WAV$header$hnames) == "Scale-1")]), "8")
})