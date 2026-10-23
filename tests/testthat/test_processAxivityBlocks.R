library(GGIRread)

test_that("exiting warning handlers release native output buffers", {
  fixture = system.file("testfiles/ax3_testfile.cwa", package = "GGIRread")
  packet = readBin(fixture, "raw", n = 1536)[1025:1536]
  first = GGIRread:::decodeAxivityBlock(packet, TRUE, NULL, FALSE)
  first$start = 0
  first$struc = list(0, 0L, 0)
  packet[1] = as.raw(0) # A failed checksum triggers the native warning path.
  samples = 100000L
  time = (seq_len(samples) - 1) / first$frequency
  read = function() GGIRread:::processAxivityBlocks(
    function() packet, function(x) 0, NULL, first, time,
    0, 2, 2, 0.1, 20L, FALSE, 1L)
  old = options(warn = 1)
  on.exit(options(old))

  condition = tryCatch(read(), warning = identity)
  expect_s3_class(condition, "warning")
  expect_identical(conditionMessage(condition), "Skipping corrupt block #1")
  expect_null(conditionCall(condition))
  options(warn = 2)
  expect_error(read(), "(converted from warning) Skipping corrupt block #1", fixed = TRUE)

  for (warn in c(1, 2)) {
    options(warn = warn)
    # Warm up condition handling before measuring live R vector cells. RSS
    # need not fall after gc(), because the allocator can retain freed pages.
    interrupt = if (warn == 1) {
      function() tryCatch(read(), warning = function(w) NULL)
    } else {
      function() tryCatch(read(), error = function(e) NULL)
    }
    expect_null(interrupt())
    before = gc()["Vcells", "used"]
    for (i in 1:3) expect_null(interrupt())
    retained = gc()["Vcells", "used"] - before
    # Allow a whole output allocation of headroom; three leaked allocations
    # retain at least 18 vector cells per sample (six numeric columns each).
    expect_lt(retained, samples * 6)
  }
})

test_that("packet boundaries interpolate through raw samples on every axis", {
  filename = tempfile(fileext = ".cwa")
  on.exit(unlink(filename))
  origin = as.numeric(as.POSIXct("2020-01-01", tz = "UTC"))
  for (device in c("ax3", "ax6")) {
    fixture = system.file("testfiles", paste0(device, "_testfile.cwa"),
                           package = "GGIRread")
    bytes = readBin(fixture, "raw", n = 1536)
    axes = if (device == "ax3") 3L else 6L
    count = 32L
    packets = list()
    values = list()
    for (block in 0:3) {
      packet = bytes[1025:1536]
      elapsed = block * 0.375
      fraction = elapsed %% 1
      packet[5:6] = writeBin(as.integer(0x8000 + fraction * 32768), raw(),
                              size = 2, endian = "little")
      packet[11:14] = writeBin(block, raw(), size = 4, endian = "little")
      packet[15:18] = writeBin(as.integer(20 * 2^26 + 2^22 + 2^17 + floor(elapsed)),
                                 raw(), size = 4, endian = "little")
      packet[25] = as.raw(10) # Nominal output rate: 100 Hz.
      packet[26] = as.raw(axes * 16 + 2) # Unpacked signed 16-bit samples.
      packet[27:28] = writeBin(as.integer(-floor(fraction * 100)), raw(),
                                size = 2, endian = "little")
      packet[29:30] = writeBin(count, raw(), size = 2, endian = "little")
      samples = matrix((seq_len(count * axes) * 137 + block * 997) %% 4000 - 2000,
                         ncol = axes)
      packet[30 + seq_len(count * axes * 2)] = writeBin(
        as.integer(t(samples)), raw(), size = 2, endian = "little")
      packet[511:512] = as.raw(0)
      checksum = (-sum(readBin(packet, integer(), n = 256, size = 2,
                               signed = FALSE, endian = "little"))) %% 65536
      packet[511:512] = writeBin(as.integer(checksum), raw(), size = 2,
                                 endian = "little")
      packets[[block + 1]] = packet
      values[[block + 1]] = GGIRread:::decodeAxivityBlock(packet, TRUE, NULL, FALSE)$data
    }
    bytes[37] = as.raw(10)
    writeBin(c(bytes[1:1024], unlist(packets)), filename)
    rawTime = origin + rep((0:2) * 0.375, each = count) +
      rep((0:(count - 1)) * (0.375 / count), 3)
    rawValues = do.call(rbind, values[1:3])
    for (interpolation in 1:2) {
      result = readAxivity(filename, start = 0, end = 3, configtz = "UTC",
                             interpolationType = interpolation, frequency_tol = 0.5)
      expect_false(any(result$QClog$imputed))
      for (axis in colnames(rawValues)) {
        if (interpolation == 1) {
          expected = approx(rawTime, rawValues[,axis], xout = result$data$time)$y
        } else {
          nearest = vapply(result$data$time, function(t) which.min(abs(rawTime - t)), 1L)
          expected = rawValues[nearest, axis]
        }
        expect_equal(result$data[[axis]], expected, tolerance = 1e-9)
      }
    }
  }
})

test_that("single final packets retain their last sample despite epoch rounding", {
  for (length in c(40L, 120L)) {
    rate = 1.5625
    start = 1577836800.5 + 5 / rate
    data = cbind(x = seq_len(length), y = 0, z = 0)
    first = list(start = start, frequency = rate, temperature = 20, battery = 0,
                 light = 0, length = length, blockID = 0L, data = data,
                 parameters = list(Naxes = 3L), struc = list(0, 0L, 0))
    time = start + (seq_len(length) - 1) * (1 / rate)
    for (interpolation in 1:2) {
      result = GGIRread:::processAxivityBlocks(
        function() stop("No lookahead is needed"), function(x) 0, NULL,
        first, time, 0, 1, 1, 0.1, 20L, FALSE, interpolation)
      expect_equal(result$last, length)
      expect_equal(unname(result$accel), unname(data))
      expect_null(result$QClog)
    }
  }
})

test_that("native loop crosses input buffers and calendar days at a low sample rate", {
  # Two days at 0.78125 Hz exercise date changes without allocating a multi-day
  # 100 Hz output. Payloads are constant; timestamps and checksums are genuine.
  fixture = system.file("testfiles/ax3_testfile.cwa", package = "GGIRread")
  bytes = readBin(fixture, "raw", n = 1536)
  packet = bytes[1025:1536]
  packet[25] = as.raw(3) # 3200 / 2^(15 - 3) = 0.78125 Hz
  rate = 0.78125
  count = 100L # 128 seconds per packet, keeping calendar timestamps integral.
  packet[29:30] = writeBin(count, raw(), size = 2, endian = "little")
  packet[5:6] = as.raw(0)
  packet[27:28] = as.raw(0)
  packet[31:510] = rep(writeBin(256L, raw(), size = 4, endian = "little"), 120)
  blocks = 1442L
  origin = as.POSIXct("2020-01-01", tz = "UTC")
  times = as.POSIXlt(origin + (seq_len(blocks) - 1) * count / rate, tz = "UTC")
  coded = bitwShiftL(as.integer(times$year - 100), 26) +
    bitwShiftL(as.integer(times$mon + 1), 22) + bitwShiftL(as.integer(times$mday), 17) +
    bitwShiftL(as.integer(times$hour), 12) + bitwShiftL(as.integer(times$min), 6) +
    as.integer(times$sec)
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
  samples = (blocks - 1) * count
  expect_equal(nrow(result$data), samples)
  expect_identical(result$data$time, as.numeric(origin) + (seq_len(samples) - 1) / rate)
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

test_that("corrupt tails retain the last valid packet as an effective final packet", {
  filename = tempfile(fileext = ".cwa")
  trimmed = tempfile(fileext = ".cwa")
  on.exit(unlink(c(filename, trimmed)))
  for (device in c("ax3", "ax6")) {
    fixture = system.file("testfiles", paste0(device, "_testfile.cwa"),
                           package = "GGIRread")
    bytes = readBin(fixture, "raw", n = file.size(fixture))
    blocks = (length(bytes) - 1024) / 512
    validBlocks = blocks - 3
    writeBin(bytes[seq_len(1024 + validBlocks * 512)], trimmed)
    for (block in validBlocks:(blocks - 1)) {
      bytes[1024 + block * 512 + 1] = as.raw(0)
    }
    writeBin(bytes, filename)
    for (interpolation in 1:2) {
      # Cover whole-file reads, two valid packets and a single valid packet.
      for (start in c(0, validBlocks - 2, validBlocks - 1)) {
        expected = readAxivity(trimmed, start = start, end = validBlocks,
                                configtz = "UTC", interpolationType = interpolation)
        expect_gt(nrow(expected$data), 0)
        # Include requests whose endpoint search reaches the corrupt tail.
        for (end in validBlocks:blocks) {
          expect_warning(actual <- readAxivity(filename, start = start, end = end,
                                                 configtz = "UTC",
                                                 interpolationType = interpolation),
                         "Skipping corrupt end block")
          expect_identical(actual$data, expected$data)
          expect_identical(actual$QClog, expected$QClog)
          expect_equal(actual$header$blocks, blocks)
        }
      }
    }
  }
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
