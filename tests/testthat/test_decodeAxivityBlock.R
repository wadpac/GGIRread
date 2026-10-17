library(GGIRread)

axivity_checksum = function(packet) {
  packet[511:512] = as.raw(0)
  checksum = (-sum(readBin(packet, integer(), n = 256, size = 2,
                           signed = FALSE, endian = "little"))) %% 65536
  packet[511:512] = writeBin(as.integer(checksum), raw(), size = 2,
                            endian = "little")
  packet
}

axivity_packet = function(device = "ax3") {
  filename = system.file(paste0("testfiles/", device, "_testfile.cwa"),
                          package = "GGIRread")
  readBin(filename, "raw", n = 1536)[1025:1536]
}

test_that("native decoder matches fixture samples and auxiliary measurements", {
  for (device in c("ax3", "ax6")) {
    filename = system.file(paste0("testfiles/", device, "_testfile.cwa"),
                            package = "GGIRread")
    bytes = readBin(filename, "raw", n = file.size(filename))
    n = (length(bytes) - 1024) / 512
    parameters = NULL
    for (i in seq_len(n)) {
      packet = bytes[1024 + (i - 1) * 512 + seq_len(512)]
      decoded = GGIRread:::decodeAxivityBlock(packet, TRUE, parameters, TRUE)
      count = readBin(packet[29:30], integer(), size = 2, signed = FALSE,
                       endian = "little")
      lightScale = readBin(packet[19:20], integer(), size = 2, signed = FALSE,
                            endian = "little")
      if (device == "ax3") {
        packed = readBin(packet[31:510], integer(), n = count, size = 4,
                          endian = "little")
        expected = GGIRread:::AxivityNumUnpack(packed) / 2^(8 + bitwShiftR(lightScale, 13))
      } else {
        expected = matrix(readBin(packet[31:510], integer(), n = count * 6,
                                    size = 2, endian = "little"), ncol = 6, byrow = TRUE)
        expected[,1:3] = expected[,1:3] / 32768 * (8000 / 2^(floor(lightScale / 1024) %% 8))
        expected[,4:6] = expected[,4:6] / 2^(8 + bitwShiftR(lightScale, 13))
      }
      expect_identical(unname(decoded$data), unname(expected))
      expect_equal(decoded$light, bitwAnd(lightScale, 1023L))
      expect_equal(decoded$temperature,
                   bitwAnd(readBin(packet[21:22], integer(), size = 2,
                                    signed = FALSE, endian = "little"), 1023L) * 75 / 256 - 50)
      expect_equal(decoded$battery, 3 * (as.integer(packet[24]) / 256 + 1))
      parameters = decoded$parameters
    }
    metadata = GGIRread:::decodeAxivityBlock(packet, FALSE, parameters, FALSE)
    expect_false("data" %in% names(metadata))
    expect_equal(metadata$battery, 0)
    expect_identical(metadata$parameters, parameters)
  }
})

test_that("packed decoding handles signed ten-bit values and all exponents", {
  packet = axivity_packet()
  # x = -512, y = -1, z = 511. Build words as bytes to include high-bit values.
  for (exponent in 0:3) {
    word = 512 + 1023 * 2^10 + 511 * 2^20 + exponent * 2^30
    packet[31:34] = as.raw(floor(word / 256^(0:3)) %% 256)
    packet = axivity_checksum(packet)
    decoded = GGIRread:::decodeAxivityBlock(packet, TRUE, NULL, FALSE)
    expect_equal(unname(decoded$data[1,]),
                 c(-512, -1, 511) * 2^exponent * decoded$parameters$accelScale)
  }
})

test_that("unpacked decoding supports accelerometer-only packets and signed limits", {
  packet = axivity_packet("ax6")
  packet[26] = as.raw(0x32) # Three axes, signed 16-bit unpacked samples.
  packet[29:30] = writeBin(80L, raw(), size = 2, endian = "little")
  values = rep(c(-32768L, -1L, 32767L), 80)
  packet[31:510] = writeBin(values, raw(), size = 2, endian = "little")
  packet = axivity_checksum(packet)
  decoded = GGIRread:::decodeAxivityBlock(packet, TRUE, NULL, FALSE)
  expect_equal(unname(decoded$data), matrix(values, ncol = 3, byrow = TRUE) *
                 decoded$parameters$accelScale)
  expect_identical(colnames(decoded$data), c("x", "y", "z"))
})

test_that("native decoding preserves timestamp offsets and parameter reuse", {
  packet = axivity_packet()
  packet[5:6] = writeBin(as.integer(0x8000 + 12345), raw(), size = 2, endian = "little")
  packet[27:28] = writeBin(-23L, raw(), size = 2, endian = "little")
  packet = axivity_checksum(packet)
  decoded = GGIRread:::decodeAxivityBlock(packet, FALSE, NULL, FALSE)
  expect_equal(decoded$fractional, 24690)
  expect_equal(decoded$shift,
               -(-23 + floor(24690 * decoded$frequency / 65536)) / decoded$frequency)
  expect_identical(GGIRread:::decodeAxivityBlock(packet, FALSE, decoded$parameters, FALSE), decoded)

  packet[5:6] = as.raw(0)
  packet = axivity_checksum(packet)
  decoded = GGIRread:::decodeAxivityBlock(packet, FALSE, NULL, FALSE)
  expect_equal(decoded$fractional, 0)
  expect_equal(decoded$shift, 23 / decoded$frequency)

  # Older packets encode frequency directly, without checksum or time shift.
  packet[25] = as.raw(0)
  packet[27:28] = writeBin(100L, raw(), size = 2, endian = "little")
  decoded = GGIRread:::decodeAxivityBlock(packet, FALSE, NULL, FALSE)
  expect_true(decoded$checksum_pass)
  expect_equal(decoded$frequency, 100)
  expect_equal(decoded$shift, 0)
  expect_identical(GGIRread:::decodeAxivityBlock(packet, FALSE, decoded$parameters, FALSE), decoded)
  packet[27:28] = writeBin(200L, raw(), size = 2, endian = "little")
  expect_equal(GGIRread:::decodeAxivityBlock(packet, FALSE, decoded$parameters, FALSE)$frequency, 200)
})

test_that("native decoding applies all accelerometer and gyroscope scale codes", {
  for (device in c("ax3", "ax6")) {
    original = axivity_packet(device)
    lightScale = readBin(original[19:20], integer(), size = 2,
                          signed = FALSE, endian = "little")
    base = GGIRread:::decodeAxivityBlock(original, TRUE, NULL, FALSE)
    originalGyroCode = floor(lightScale / 1024) %% 8
    for (code in 0:7) {
      packet = original
      replacement = lightScale %% 1024 + code * 1024 + code * 8192
      packet[19:20] = writeBin(as.integer(replacement), raw(), size = 2, endian = "little")
      packet = axivity_checksum(packet)
      decoded = GGIRread:::decodeAxivityBlock(packet, TRUE, NULL, FALSE)
      expect_equal(decoded$data[,c("x", "y", "z")],
                   base$data[,c("x", "y", "z")] *
                     (1 / 2^(8 + code)) / base$parameters$accelScale)
      if (device == "ax6") {
        expect_equal(decoded$data[,c("gx", "gy", "gz")],
                     base$data[,c("gx", "gy", "gz")] * 2^(originalGyroCode - code))
      }
      # Accelerometer scale is cached, whereas gyro range is packet-specific.
      reused = GGIRread:::decodeAxivityBlock(packet, TRUE, base$parameters, FALSE)
      expect_identical(reused$data[,c("x", "y", "z")], base$data[,c("x", "y", "z")])
    }
  }
})

test_that("native decoder validates packets before accessing sample payloads", {
  packet = axivity_packet()
  expect_error(GGIRread:::decodeAxivityBlock(packet[-1], TRUE, NULL, FALSE), "512 bytes")
  packet[1] = as.raw(0)
  expect_identical(GGIRread:::decodeAxivityBlock(packet, TRUE, NULL, FALSE),
                   list(checksum_pass = FALSE))
  expect_error(GGIRread:::decodeAxivityBlock(axivity_checksum(packet), TRUE, NULL, FALSE),
               "Packet header is incorrect")
  packet = axivity_packet()
  packet[3:4] = as.raw(0)
  expect_error(GGIRread:::decodeAxivityBlock(axivity_checksum(packet), TRUE, NULL, FALSE),
               "Packet length is incorrect")
  packet = axivity_packet()
  packet[29:30] = writeBin(121L, raw(), size = 2, endian = "little")
  expect_error(GGIRread:::decodeAxivityBlock(axivity_checksum(packet), TRUE, NULL, FALSE),
               "Invalid Axivity sample count")
  packet = axivity_packet("ax6")
  packet[29:30] = writeBin(41L, raw(), size = 2, endian = "little")
  expect_error(GGIRread:::decodeAxivityBlock(axivity_checksum(packet), TRUE, NULL, FALSE),
               "Invalid Axivity sample count")
})
