library(GGIRread)
context("reading Axivity .cwa data")
test_that("readAxivity reads data from AX3 file correctly", {
  cwafile  = system.file("testfiles/ax3_testfile.cwa", package = "GGIRread")[1]
  AX3 = readAxivity(filename = cwafile, desiredtz = "Europe/Berlin", start = 1, end = 4)
  expect_equal(AX3$header$device, "Axivity")
  expect_equal(nrow(AX3$data), 364)
  expect_equal(ncol(AX3$data), 7)
  expect_equal(AX3$data$time[5], 1551174907.255, tolerance = .001, scale = 1)
  expect_equal(AX3$data$temp[3], 26.46484, tolerance = 0.0001)
  expect_equal(floor(sum(abs(AX3$data[,2:4]))), 603)
  expect_equal(AX3$data[1,2], 0.7656, tolerance = .0001, scale = 1)
  expect_equal(AX3$data[4,3], -0.3125, tolerance = .0001, scale = 1)
  expect_true(is.null(AX3$QClog))
  
  # ask for more data then there is in the file
  AX3b = readAxivity(filename = cwafile, desiredtz = "Europe/Berlin", start = 0, end = 1000)
  expect_equal(AX3b$header$device, "Axivity")
  expect_equal(nrow(AX3b$data), 17599)
  expect_equal(ncol(AX3b$data), 7)
  expect_equal(AX3b$data$time[5], 1551174906.040, tolerance = .001, scale = 1)
  expect_equal(AX3b$data$temp[3], 25.5859, tolerance = 0.0001, scale = 1)
  expect_equal(floor(sum(abs(AX3b$data[,2:4]))), 25367)
  
})

test_that("readAxivity handles failed checksums correctly", {  
  # read the blocks 1-11 from the non-corrupt file
  cwafile  = system.file("testfiles/ax3_testfile.cwa", package = "GGIRread")[1]
  AX3 = readAxivity(filename = cwafile, desiredtz = "Europe/Berlin", start = 1, end = 12)

  # From now on we'll be reading a file with corrupt blocks 0, 13, 14, 142, 143 and 144 (which is the last block)
  cwafile  = system.file("testfiles/ax3_testfile_corrupt_blocks_0_13_14_142_143_144.cwa", package = "GGIRread")[1]

  # Since block 0 is corrupt, then if we start reading from that block, it gets skipped, and the results will be
  # as if we read starting from block 1. No imputation will be done.
  expect_warning(AX3_0_12 <- readAxivity(filename = cwafile, desiredtz = "Europe/Berlin", start = 0, end = 12),
               "Skipping corrupt block #0")
  expect_equal(AX3_0_12$header$device, "Axivity")
  expect_equal(nrow(AX3_0_12$data), nrow(AX3$data))
  expect_equal(floor(sum(abs(AX3_0_12$data[,2:4]))), floor(sum(abs(AX3$data[,2:4]))))

  expect_false(is.null(AX3_0_12$QClog))
  expect_equal(nrow(AX3_0_12$QClog),1)
  expect_false(AX3_0_12$QClog$checksum_pass[1]) 
  expect_false(AX3_0_12$QClog$imputed[1])
  expect_equal(AX3_0_12$QClog$blockID_current[1], 0)
  expect_equal(AX3_0_12$QClog$blockID_next[1], 1)

  # if we read blocks 1-11, results should be the same as for the non-corrupt file,
  # and there should be nothing in QClog
  expect_warning(AX3_1_12 <- readAxivity(filename = cwafile, desiredtz = "Europe/Berlin", start = 1, end = 12),
                 "Skipping corrupt block #0") # the warning is there b/c block 0 is always read, and it's corrupt
  
  expect_equal(AX3_1_12$header$device, "Axivity")
  expect_equal(nrow(AX3_1_12$data), nrow(AX3$data))
  expect_equal(floor(sum(abs(AX3_1_12$data[,2:4]))), floor(sum(abs(AX3$data[,2:4]))))
  expect_true(is.null(AX3_1_12$QClog))
  
  # Now read a bigger chunk of the file, so that the corrupt blocks 13-14 fall in the middle
  expect_warning(AX3_1_20 <- readAxivity(filename = cwafile, desiredtz = "Europe/Berlin", start = 1, end = 20),
                 "Skipping corrupt block #13")
  expect_equal(AX3_1_20$header$device, "Axivity")
  expect_equal(nrow(AX3_1_20$data), 2306)
  expect_equal(ncol(AX3_1_20$data), 7)
  expect_equal(floor(sum(abs(AX3_1_20$data[,2:4]))), 3244)
  
  # there should be 3 entries in QClog: one each for blocks 13 and 14 with failed checksums (these initially get skipped, not imputed),
  # and one for block 12 that preceeds the corrupt blocks, because data needs to be imputed from block 12 (inclusive) to 15 (exclusive)
  expect_false(is.null(AX3_1_20$QClog))
  expect_equal(nrow(AX3_1_20$QClog),3)

  expect_false(AX3_1_20$QClog$checksum_pass[1])
  expect_false(AX3_1_20$QClog$imputed[1])
  expect_equal(AX3_1_20$QClog$blockID_current[1], 13)
  expect_equal(AX3_1_20$QClog$blockID_next[1], 14)

  expect_false(AX3_1_20$QClog$checksum_pass[2])
  expect_false(AX3_1_20$QClog$imputed[2])
  expect_equal(AX3_1_20$QClog$blockID_current[2], 14)
  expect_equal(AX3_1_20$QClog$blockID_next[2], 15) 

  expect_true(AX3_1_20$QClog$checksum_pass[3]) 
  expect_true(AX3_1_20$QClog$imputed[3])
  expect_equal(AX3_1_20$QClog$blockID_current[3], 12) # we finally found the first valid block after #12, so we imputed block #12, up to 15
  expect_equal(AX3_1_20$QClog$blockID_next[3], 15) # the first valid block after 12 is 15

  # check that data in blocks 12-14 got imputed with the correct values

  nrow11 = nrow(AX3_1_12$data)
  imputedValues = AX3_1_12$data[nrow11,2:4]
  VectorG = sqrt(sum(imputedValues^2))
  imputedValues = imputedValues / VectorG

  expect_true(all(abs(imputedValues - AX3_1_20$data[nrow11+1,2:4] ) < .0001))
  expect_true(all(abs(imputedValues - AX3_1_20$data[nrow11+2,2:4] ) < .0001))
  expect_true(all(abs(imputedValues - AX3_1_20$data[nrow11+100,2:4] ) < .0001))
  expect_true(all(abs(imputedValues - AX3_1_20$data[nrow11+200,2:4] ) < .0001))
  expect_true(all(abs(imputedValues - AX3_1_20$data[nrow11+360,2:4] ) < .0001))
  expect_equal(sum(abs(imputedValues)), 
               sum(abs(AX3_1_20$data[(nrow11+1) : (nrow11+360), 2:4])) / 360, 
               tolerance = .0001, scale = 1)

  # make sure a request for only corrupt blocks returns an error but doesn't crash
  expect_warning(
    expect_error(readAxivity(filename = cwafile, desiredtz = "Europe/Berlin", start = 13, end = 14),
                 "All requested blocks are corrupt"),
    "Skipping corrupt start block #13")
  expect_warning(
    expect_error(readAxivity(filename = cwafile, desiredtz = "Europe/Berlin", start = 14, end = 15),
                 "All requested blocks are corrupt"),
    "Skipping corrupt start block #14")
  expect_warning(
    expect_error(readAxivity(filename = cwafile, desiredtz = "Europe/Berlin", start = 13, end = 15),
                 "All requested blocks are corrupt"),
    "Skipping corrupt start block #13")

  # Now see what happens if the corrupt blocks fall in the very beginning of the requested interval.
  # These blocks should be skipped and the file should be read from the first non-corrupt block.
  # No blocks will be imputed.
  # So reading blocks 13-20 or 14-20 should give the same output as reading blocks 15-20.

  expect_warning(AX3_13_20 <- readAxivity(filename = cwafile, desiredtz = "Europe/Berlin", start = 13, end = 20),
                 "Skipping corrupt start block #13")
  expect_warning(AX3_14_20 <- readAxivity(filename = cwafile, desiredtz = "Europe/Berlin", start = 14, end = 20),
                 "Skipping corrupt start block #14")
  expect_warning(AX3_15_20 <- readAxivity(filename = cwafile, desiredtz = "Europe/Berlin", start = 15, end = 20),
                 "Skipping corrupt block #0") # the warning is there b/c block 0 is always read, and it's corrupt

  expect_equal(nrow(AX3_13_20$data), nrow(AX3_15_20$data))
  expect_equal(floor(sum(abs(AX3_13_20$data[,2:4]))), floor(sum(abs(AX3_15_20$data[,2:4]))))

  expect_equal(nrow(AX3_14_20$data), nrow(AX3_15_20$data))
  expect_equal(floor(sum(abs(AX3_14_20$data[,2:4]))), floor(sum(abs(AX3_15_20$data[,2:4]))))

  expect_false(is.null(AX3_13_20$QClog))
  expect_equal(nrow(AX3_13_20$QClog),2)

  expect_false(AX3_13_20$QClog$checksum_pass[1])
  expect_false(AX3_13_20$QClog$imputed[1])
  expect_equal(AX3_13_20$QClog$blockID_current[1], 13)
  expect_equal(AX3_13_20$QClog$blockID_next[1], 14)

  expect_false(AX3_13_20$QClog$checksum_pass[2])
  expect_false(AX3_13_20$QClog$imputed[2])
  expect_equal(AX3_13_20$QClog$blockID_current[2], 14)
  expect_equal(AX3_13_20$QClog$blockID_next[2], 15) 
  
  # Now see what happens if the corrupt blocks 13-14 fall at the very end of the requested interval
  # (but not at the end of the file).
  # These blocks should be skipped, and the file should be read until
  # the first non-corrupt block that follows the corrupt ones (so block 15).
  # Data will be imputed from block 12 (inclusive) to 15 (exclusive).
  # So reading blocks 1-13 or 1-14 should give the same output as reading blocks 1-15.

  expect_warning(AX3_1_13 <- readAxivity(filename = cwafile, desiredtz = "Europe/Berlin", start = 1, end = 13),
                 "Skipping corrupt end block #13")
  expect_warning(AX3_1_14 <- readAxivity(filename = cwafile, desiredtz = "Europe/Berlin", start = 1, end = 14),
                 "Skipping corrupt end block #14")
  expect_warning(AX3_1_15 <- readAxivity(filename = cwafile, desiredtz = "Europe/Berlin", start = 1, end = 15),
                 "Skipping corrupt block #13")

  expect_equal(nrow(AX3_1_13$data), nrow(AX3_1_15$data))
  expect_equal(floor(sum(abs(AX3_1_13$data[,2:4]))), floor(sum(abs(AX3_1_15$data[,2:4]))))

  expect_equal(nrow(AX3_1_14$data), nrow(AX3_1_15$data))
  expect_equal(floor(sum(abs(AX3_1_14$data[,2:4]))), floor(sum(abs(AX3_1_15$data[,2:4]))))

  expect_false(is.null(AX3_1_13$QClog))
  expect_equal(nrow(AX3_1_13$QClog),3)

  expect_false(AX3_1_13$QClog$checksum_pass[1])
  expect_false(AX3_1_13$QClog$imputed[1])
  expect_equal(AX3_1_13$QClog$blockID_current[1], 13)
  expect_equal(AX3_1_13$QClog$blockID_next[1], 14)

  expect_false(AX3_1_13$QClog$checksum_pass[2])
  expect_false(AX3_1_13$QClog$imputed[2])
  expect_equal(AX3_1_13$QClog$blockID_current[2], 14)
  expect_equal(AX3_1_13$QClog$blockID_next[2], 15) 

  expect_true(AX3_1_13$QClog$checksum_pass[3]) 
  expect_true(AX3_1_13$QClog$imputed[3])
  expect_equal(AX3_1_13$QClog$blockID_current[3], 12) # we finally found the first valid block after #12, so we imputed blocks [12, 15)
  expect_equal(AX3_1_13$QClog$blockID_next[3], 15) # the first valid block after 12 is 15

  # Now make sure corrupt blocks at the very end of the file are handled correctly (blocks 142-144).

  # make sure a request for only corrupt blocks returns an error but doesn't crash
  expect_warning(
    expect_error(readAxivity(filename = cwafile, desiredtz = "Europe/Berlin", start = 142, end = 143),
                 "All requested blocks are corrupt"),
    "Skipping corrupt end block")
  expect_warning(
    expect_error(readAxivity(filename = cwafile, desiredtz = "Europe/Berlin", start = 142, end = 144),
                 "All requested blocks are corrupt"),
    "Skipping corrupt end block")
  expect_warning(
    expect_error(readAxivity(filename = cwafile, desiredtz = "Europe/Berlin", start = 142, end = 145),
                 "All requested blocks are corrupt"),
    "Skipping corrupt end block")
  expect_warning(
    expect_error(readAxivity(filename = cwafile, desiredtz = "Europe/Berlin", start = 143, end = 144),
                 "All requested blocks are corrupt"),
    "Skipping corrupt end block")
  expect_warning(
    expect_error(readAxivity(filename = cwafile, desiredtz = "Europe/Berlin", start = 143, end = 145),
                 "All requested blocks are corrupt"),
    "Skipping corrupt end block")
  expect_warning(
    expect_error(readAxivity(filename = cwafile, desiredtz = "Europe/Berlin", start = 144, end = 145),
                 "All requested blocks are corrupt"),
  "Skipping corrupt end block")
  
  # If there are corrupt blocks at the very end of the file, then the last non-corrupt block is
  # treated as the end block and no imputation is done.
  # So in our case, since the end blocks 142-144 are corrupt, they are ignored and the file is 
  # treated as if it had only 142 blocks (0-141)
  expect_warning(AX3_140_141 <- readAxivity(filename = cwafile, desiredtz = "Europe/Berlin", start = 140, end = 141),
                 "Skipping corrupt block #0")
  expect_warning(AX3_140_142 <- readAxivity(filename = cwafile, desiredtz = "Europe/Berlin", start = 140, end = 142),
                 "Skipping corrupt end block")
  expect_warning(AX3_140_143 <- readAxivity(filename = cwafile, desiredtz = "Europe/Berlin", start = 140, end = 143),
                 "Skipping corrupt block")
  expect_warning(AX3_140_144 <- readAxivity(filename = cwafile, desiredtz = "Europe/Berlin", start = 140, end = 144),
                 "Skipping corrupt end block")
  expect_warning(AX3_140_145 <- readAxivity(filename = cwafile, desiredtz = "Europe/Berlin", start = 140, end = 145),
                 "Skipping corrupt end block")

  expect_equal(nrow(AX3_140_141$data), nrow(AX3_140_142$data))
  expect_equal(floor(sum(abs(AX3_140_141$data[,2:4]))), floor(sum(abs(AX3_140_142$data[,2:4]))))

  expect_equal(nrow(AX3_140_141$data), nrow(AX3_140_143$data))
  expect_equal(floor(sum(abs(AX3_140_141$data[,2:4]))), floor(sum(abs(AX3_140_143$data[,2:4]))))

  expect_equal(nrow(AX3_140_141$data), nrow(AX3_140_144$data))
  expect_equal(floor(sum(abs(AX3_140_141$data[,2:4]))), floor(sum(abs(AX3_140_144$data[,2:4]))))

  expect_equal(nrow(AX3_140_141$data), nrow(AX3_140_145$data))
  expect_equal(floor(sum(abs(AX3_140_141$data[,2:4]))), floor(sum(abs(AX3_140_145$data[,2:4]))))

  # If the file has more consecutive corrupt blocks than allowed by maxAllowedCorruptBlocks,
  # we should get an error.

  expect_warning(
    expect_error(readAxivity(filename = cwafile, desiredtz = "Europe/Berlin", start = 1, end = 20, maxAllowedCorruptBlocks=1),
                 "Error reading file. 2 consecutive blocks are corrupt."),
    "Skipping corrupt block #13")

  expect_warning(
    expect_error(readAxivity(filename = cwafile, desiredtz = "Europe/Berlin", start = 140, end = 144, maxAllowedCorruptBlocks=2),
                 "Error reading file. The last 3 blocks are corrupt."),
    "Skipping corrupt end block #144")
})


test_that("readAxivity reads data from AX6 file correctly", {
  cwafile  = system.file("testfiles/ax6_testfile.cwa", package = "GGIRread")[1]
  AX6 = readAxivity(filename = cwafile, desiredtz = "Europe/Berlin", start = 1, end = 4)
  expect_equal(AX6$header$device, "Axivity")
  expect_equal(nrow(AX6$data), 121)
  expect_equal(ncol(AX6$data), 10)
  expect_equal(AX6$data$time[5], 1577131447.144, tolerance = .001, scale = 1)
  expect_equal(AX6$data$temp[3], 27.34375, tolerance = 0.0001)
  expect_equal(floor(sum(abs(AX6$data[,2:4]))), 2007)
  expect_equal(floor(sum(abs(AX6$data[,5:7]))), 10)
  expect_equal(AX6$data[1,2], 0.2670288, tolerance = 0.0000001, scale = 1)
  expect_equal(AX6$data[4,3], -0.5026558, tolerance = 0.0000001, scale = 1)
  expect_equal(AX6$data[30,5], -0.009452909, tolerance = 0.0000001, scale = 1)
  expect_true(is.null(AX6$QClog))
  
  
  # ask for more data then there is in the file
  AX6b = readAxivity(filename = cwafile, desiredtz = "Europe/Berlin", start = 0, end = 1000)
  expect_equal(AX6b$header$device, "Axivity")
  expect_equal(nrow(AX6b$data), 11428)
  expect_equal(ncol(AX6b$data), 10)
  expect_equal(AX6b$data$time[5], 1577131446.740, tolerance = .001, scale = 1)
  expect_equal(AX6b$data$temp[3], 27.34375, tolerance = 0.0001)
  expect_equal(floor(sum(abs(AX6b$data[,2:4]))), 960046)
  expect_true(is.null(AX6b$QClog))
  
})


test_that("readAxivity reads timezones correctly", {
  cwafile  = system.file("testfiles/ax3_testfile.cwa", package = "GGIRread")[1]
  old <- options(digits.secs = 3)
  tzAms = "Europe/Amsterdam"
  tzLon = "Europe/London"
  # desiredtz == configtz
  tzequal = readAxivity(filename = cwafile, desiredtz = tzLon, configtz = tzLon, start = 1, end = 4)
  expect_equal(tzequal$data$time[1], 1551178507.215, tolerance = .001, scale = 1)
  expect_equal(format(as.POSIXlt(tzequal$data$time[1], tz = tzLon,
                                       origin = "1970-01-01")), "2019-02-26 10:55:07.215")
  
  # desiredtz < configtz
  tzwest = readAxivity(filename = cwafile, desiredtz = tzLon, configtz = tzAms, start = 1, end = 4)
  expect_equal(tzwest$data$time[1], 1551174907.215, tolerance = .001, scale = 1)
  expect_equal(format(as.POSIXlt(tzwest$data$time[1], tz = tzLon, 
                                       origin = "1970-01-01")), "2019-02-26 09:55:07.215")
  
  # desiredtz > configtz
  tzeast = readAxivity(filename = cwafile, desiredtz = tzAms, configtz = tzLon, start = 1, end = 4)
  expect_equal(tzeast$data$time[1], 1551178507.215, tolerance = .001, scale = 1)
  expect_equal(format(as.POSIXlt(tzeast$data$time[1], tz = tzAms, 
                                       origin = "1970-01-01")), "2019-02-26 11:55:07.215")
  options(old)
})