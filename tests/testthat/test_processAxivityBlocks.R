library(GGIRread)

test_that("native loop crosses input buffers and calendar days at a low sample rate", {
  # Two days at 1 Hz exercise date changes without allocating a multi-day
  # 100 Hz output. Payloads are constant; timestamps and checksums are genuine.
  fixture = system.file("testfiles/ax3_testfile.cwa", package = "GGIRread")
  bytes = readBin(fixture, "raw", n = 1536)
  packet = bytes[1025:1536]
  packet[25] = as.raw(3) # round(3200 / 2^(15 - 3)) = 1 Hz
  packet[5:6] = as.raw(0)
  packet[27:28] = as.raw(0)
  packet[31:510] = rep(writeBin(256L, raw(), size = 4, endian = "little"), 120)
  blocks = 1442L
  origin = as.POSIXct("2020-01-01", tz = "UTC")
  times = as.POSIXlt(origin + (seq_len(blocks) - 1) * 120, tz = "UTC")
  coded = bitwShiftL(as.integer(times$year - 100), 26) +
    bitwShiftL(as.integer(times$mon + 1), 22) + bitwShiftL(as.integer(times$mday), 17) +
    bitwShiftL(as.integer(times$hour), 12) + bitwShiftL(as.integer(times$min), 6)
  packets = matrix(rep(packet, blocks), nrow = 512)
  packets[11:14,] = writeBin(seq_len(blocks) - 1L, raw(), size = 4, endian = "little")
  packets[15:18,] = writeBin(as.integer(coded), raw(), size = 4, endian = "little")
  packets[511:512,] = as.raw(0)
  for (i in seq_len(blocks)) {
    checksum = (-sum(readBin(packets[,i], integer(), n = 256, size = 2,
                              signed = FALSE, endian = "little"))) %% 65536
    packets[511:512,i] = writeBin(as.integer(checksum), raw(), size = 2, endian = "little")
  }
  bytes[37] = as.raw(3)
  filename = tempfile(fileext = ".cwa")
  on.exit(unlink(filename))
  writeBin(c(bytes[1:1024], as.vector(packets)), filename)
  result = readAxivity(filename, start = 0, end = blocks - 1, configtz = "UTC")
  samples = (blocks - 1) * 120
  expect_equal(nrow(result$data), samples)
  expect_identical(result$data$time, as.numeric(origin) + seq_len(samples) - 1)
  scale = 1 / 2^(8 + bitwShiftR(as.integer(packet[20]), 5))
  expect_true(all(result$data$x == 256 * scale))
  expect_true(all(result$data$y == 0))
  expect_true(all(result$data$z == 0))
  expect_null(result$QClog)
})

test_that("native buffering preserves corrupt-packet logs and limits", {
  fixture = system.file("testfiles/ax6_testfile.cwa", package = "GGIRread")
  bytes = readBin(fixture, "raw", n = file.size(fixture))
  # After the initial packet, each input buffer contains another 128 packets.
  # Corrupt packets straddle a refill boundary.
  for (block in 128:129) {
    offset = 1024 + block * 512 + 1
    bytes[offset] = as.raw(bitwXor(as.integer(bytes[offset]), 1L))
  }
  filename = tempfile(fileext = ".cwa")
  on.exit(unlink(filename))
  writeBin(bytes, filename)
  expect_warning(result <- readAxivity(filename, start = 0, end = 132, configtz = "UTC"),
                   "Skipping corrupt block #128")
  failed = result$QClog[!result$QClog$checksum_pass,]
  expect_identical(failed$blockID_current, 128:129)
  expect_identical(failed$blockID_next, c(129, 130))
  expect_true(any(result$QClog$blockID_current == 127 & result$QClog$imputed))
  expect_warning(expect_error(readAxivity(filename, start = 0, end = 132,
                                           configtz = "UTC", maxAllowedCorruptBlocks = 1),
                               "2 consecutive blocks are corrupt"),
                   "Skipping corrupt block")
})

test_that("repeated imputation retains the R normalisation arithmetic", {
  filename = system.file("testfiles/ax3_testfile.cwa", package = "GGIRread")
  result = readAxivity(filename, start = 13, end = 16,
                       configtz = "UTC", frequency_tol = 0)
  expected = c(62, 12, 11) / 64
  for (block in 1:3) {
    expected = expected / sqrt(sum(expected^2))
    first = which(result$data$time >= result$QClog$start[block])[1]
    expect_identical(as.numeric(result$data[first, c("x", "y", "z")]), expected)
  }
})

test_that("AX6 gaps impute acceleration orientation and zero angular velocity", {
  fixture = system.file("testfiles/ax6_testfile.cwa", package = "GGIRread")
  original = readBin(fixture, "raw", n = 1024 + 6 * 512)
  first = GGIRread:::decodeAxivityBlock(original[1025:1536], TRUE, NULL, FALSE)
  filename = tempfile(fileext = ".cwa")
  on.exit(unlink(filename))

  # Exercise both normalisation of a plausible orientation and the 1 g fallback.
  # Non-zero gyro values must never be used as the acceleration orientation.
  for (orientation in list(c(0.6, 0.8, 0.2), c(2, 0, 0))) {
    bytes = original
    rawAccel = round(orientation / first$parameters$accelScale)
    acceleration = rawAccel * first$parameters$accelScale
    norm = sqrt(sum(acceleration^2))
    expected = if (norm > 0.8 && norm < 1.2) acceleration / norm else c(0, 0, 1)
    for (block in 0:5) {
      positions = 1024 + block * 512 + seq_len(512)
      packet = bytes[positions]
      count = readBin(packet[29:30], integer(), size = 2, signed = FALSE,
                      endian = "little")
      packet[30 + seq_len(count * 12)] = writeBin(
        as.integer(rep(c(1000, 2000, 3000, rawAccel), count)), raw(),
        size = 2, endian = "little")
      packet[511:512] = as.raw(0)
      checksum = (-sum(readBin(packet, integer(), n = 256, size = 2,
                               signed = FALSE, endian = "little"))) %% 65536
      packet[511:512] = writeBin(as.integer(checksum), raw(), size = 2,
                                 endian = "little")
      bytes[positions] = packet
    }
    # Corrupt block 2, leaving recorded data on both sides of the gap.
    bytes[1024 + 2 * 512 + 1] = as.raw(0)
    writeBin(bytes, filename)
    for (interpolation in 1:2) {
      expect_warning(result <- readAxivity(filename, start = 0, end = 5,
                                            configtz = "UTC",
                                            interpolationType = interpolation),
                     "Skipping corrupt block #2")
      gap = result$QClog[result$QClog$imputed, ]
      expect_equal(nrow(gap), 1)
      # Stay inside the imputed span, away from interpolation at its boundary.
      rows = result$data$time >= gap$start & result$data$time < gap$end - 0.1
      expect_true(any(rows))
      expect_true(all(as.matrix(result$data[rows, c("gx", "gy", "gz")]) == 0))
      expect_equal(unname(as.matrix(result$data[rows, c("x", "y", "z")])),
                   matrix(rep(expected, each = sum(rows)), ncol = 3))
    }
  }
})

test_that("a corrupt tail without a usable endpoint fails explicitly", {
  filename = system.file("testfiles/ax3_testfile_corrupt_blocks_0_13_14_142_143_144.cwa",
                          package = "GGIRread")
  # The former R loop left 'last' undefined after skipping every lookahead
  # packet. Do not return a successful but empty data frame in this case.
  expect_warning(expect_error(readAxivity(filename, start = 141, end = 145,
                                           configtz = "UTC"),
                               "No samples could be resampled from the Axivity chunk"),
                   "Skipping corrupt")
})

test_that("a device sequence reset refreshes the native timestamp cache", {
  fixture = system.file("testfiles/ax6_testfile.cwa", package = "GGIRread")
  bytes = readBin(fixture, "raw", n = file.size(fixture))
  blocks = (length(bytes) - 1024) / 512
  for (block in 130:(blocks - 1)) {
    positions = 1024 + block * 512 + seq_len(512)
    packet = bytes[positions]
    packet[11:14] = writeBin(as.integer(block - 130), raw(), size = 4, endian = "little")
    packet[511:512] = as.raw(0)
    checksum = (-sum(readBin(packet, integer(), n = 256, size = 2,
                              signed = FALSE, endian = "little"))) %% 65536
    packet[511:512] = writeBin(as.integer(checksum), raw(), size = 2, endian = "little")
    bytes[positions] = packet
  }
  filename = tempfile(fileext = ".cwa")
  on.exit(unlink(filename))
  writeBin(bytes, filename)
  original = readAxivity(fixture, start = 0, end = 200, configtz = "Europe/Berlin")
  reset = readAxivity(filename, start = 0, end = 200, configtz = "Europe/Berlin")
  expect_identical(reset, original)
})
