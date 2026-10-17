# Source this file after loading the development package, then run:
# benchmark_readAxivity("/path/to/previous/readAxivity.R")
# Named paths can compare several stages in the same run:
# benchmark_readAxivity(c(original = "original.R", decoder = "decoder.R"))
# The reference is a saved copy of R/readAxivity.R from before the optimisation.
# Synthetic data repeat sensor payloads, but have continuous timestamps,
# consecutive sequence IDs and valid checksums. They measure throughput, not
# compatibility with the full variation of real recordings.

make_axivity_benchmark_file = function(device, blocks = 10000L) {
  stopifnot(device %in% c("ax3", "ax6"), blocks > 0)
  fixture = system.file(paste0("testfiles/", device, "_testfile.cwa"),
                         package = "GGIRread")
  bytes = readBin(fixture, "raw", n = 1536)
  packet = bytes[1025:1536]
  samples = readBin(packet[29:30], integer(), size = 2, signed = FALSE, endian = "little")
  rate = 3200 / 2^(15 - bitwAnd(as.integer(packet[25]), 15L))
  times = as.POSIXlt(as.POSIXct("2020-01-01", tz = "UTC") +
                      (seq_len(blocks) - 1) * samples / rate, tz = "UTC")
  coded = bitwShiftL(as.integer(times$year - 100), 26) +
    bitwShiftL(as.integer(times$mon + 1), 22) + bitwShiftL(as.integer(times$mday), 17) +
    bitwShiftL(as.integer(times$hour), 12) + bitwShiftL(as.integer(times$min), 6) + as.integer(times$sec)
  packets = matrix(rep(packet, blocks), nrow = 512)
  packets[5:6,] = as.raw(0)
  packets[11:14,] = writeBin(as.integer(seq_len(blocks) - 1), raw(), size = 4, endian = "little")
  packets[15:18,] = writeBin(as.integer(coded), raw(), size = 4, endian = "little")
  offsets = -as.integer(round((times$sec - floor(times$sec)) * rate))
  packets[27:28,] = writeBin(offsets, raw(), size = 2, endian = "little")
  packets[511:512,] = as.raw(0)
  for (i in seq_len(blocks)) {
    checksum = (-sum(readBin(packets[,i], integer(), n = 256, size = 2,
                              signed = FALSE, endian = "little"))) %% 65536
    packets[511:512,i] = writeBin(as.integer(checksum), raw(), size = 2, endian = "little")
  }
  filename = tempfile(fileext = ".cwa")
  writeBin(c(bytes[1:1024], as.vector(packets)), filename)
  filename
}

benchmark_readAxivity = function(reference_file, blocks = c(1000L, 10000L), repetitions = 5L) {
  readers = lapply(reference_file, function(filename) {
    reference = new.env(parent = asNamespace("GGIRread"))
    sys.source(filename, reference)
    reference$readAxivity
  })
  if (is.null(names(readers))) {
    names(readers) = if (length(readers) == 1) "previous" else paste0("reference", seq_along(readers))
  }
  readers$native = GGIRread::readAxivity
  results = list()
  for (device in c("ax3", "ax6")) {
    for (n in blocks) {
      filename = make_axivity_benchmark_file(device, n)
      tryCatch({
        # Warm up both paths, and compare the entire output before timing.
        outputs = lapply(readers, function(reader) {
          reader(filename, start = 0, end = n, configtz = "UTC")
        })
        stopifnot(all(vapply(outputs, identical, logical(1), outputs$native)))
        rm(outputs)
        timings = matrix(NA_real_, repetitions, length(readers), dimnames = list(NULL, names(readers)))
        for (i in seq_len(repetitions)) {
          # Alternate order to reduce systematic cache/order effects.
          order = if (i %% 2) seq_along(readers) else rev(seq_along(readers))
          for (j in order) {
            gc()
            timings[i,j] = system.time(invisible(readers[[j]](
              filename, start = 0, end = n, configtz = "UTC")))[["elapsed"]]
          }
        }
        elapsed = apply(timings, 2, median)
        row = data.frame(device = device, blocks = n)
        for (name in names(elapsed)) row[[paste0(name, "_seconds")]] = elapsed[[name]]
        row$speedup = unname(elapsed[1] / elapsed["native"])
        print(row)
        results[[length(results) + 1L]] = row
      }, finally = unlink(filename))
    }
  }
  invisible(do.call(rbind, results))
}
