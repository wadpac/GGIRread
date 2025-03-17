readParmayMatrix = function(bin_file, output = c("all", "sf", "dynrange")[1],
                            start = 1, end = NULL,
                            desiredtz = "", configtz = NULL,
                            interpolationType = 1,
                            read_acc = TRUE, read_gyro = FALSE, 
                            read_temp = TRUE, read_heart = FALSE) {
  
  # Matrix devices binary files are organized in packets of data.
  
  # The header information contains:
  #  - remarks (empty in all files I have tested): bytes 1:512
  #  - Count of the total number of packets in file (bytes 513:516)
  #  - header string = "MDTC" (bytes 517:520, if not there -> file corrupt)
  #  - range of the acc (bytes 521:522) and the gyro sensors (bytes 523:524)
  
  # Each packet contains the following information:
  #  - 8-byte package header
  #  - 4-byte CRC32 indicator
  #  - 4-byte start timestamp of the packet
  #  - 4-byte end timestamp of the packet
  #  - 4-byte accelerometer recordings in packet (a)
  #  - 4-byte gyroscope recordings in packet (g)
  #  - 4-byte temperature recordings in packet (t)
  #  - 4-byte heart rate recordings in packet (h)
  #  - (6*a)-byte accelerometer recordings
  #  - (6*a)-byte gyroscope recordings
  #  - (4*a)-byte temperature recordings
  #  - (4*a)-byte heart rate recordings
  
  # Notes:
  #  - Packet lengths vary with the number of recordings in each packet, so dynamic definition of the length of each packet is needed
  #  - Sampling frequency is not stable, similar to axivity, similar approach is followed (checksum and interpolation)
  #  - Gaps in data are not expected
  
  # -------------------------------------------------------------------------
  # 1 - read file and extract header information

  # open connection to read the binary file
  con = file(bin_file, "rb")
  on.exit(close(con))
  # bin_data = readBin(bin_file, "raw", file.info(bin_file)$size)
  
  # Validate the header recognition string (bytes 513:516)
  seek(con, where = 512, origin = "start")
  header_bytes = readBin(con, "raw", n = 4)
  header_string = rawToChar(header_bytes[header_bytes != 0], multiple = FALSE)
  if (header_string != "MDTC") {
    stop("Invalid header recognition string.")
  }
  
  # Extract the total number of packets (bytes 517:520)
  seek(con, where = 516, origin = "start")
  tPackets_bytes = readBin(con, "raw", n = 4)
  total_packets = readBin(tPackets_bytes, "integer", size = 4, endian = "little")
  
  # Validate start and end packets
  lastchunk = FALSE
  if (is.null(end)) end = total_packets
  if (end >= total_packets) {
    end = total_packets
    lastchunk = TRUE
  }
  if (start < 1 || start > end) {
    stop("Invalid start or end packet range.")
  }
  
  # acc dynrange (bytes 521:522) and gyro_range (bytes 523:524)
  seek(con, where = 520, origin = "start")
  acc_dynrange_bytes = readBin(con, "raw", n = 2)
  acc_dynrange = readBin(acc_dynrange_bytes, "integer", size = 2, endian = "little")
  if (output == "dynrange") return(acc_dynrange)
  seek(con, where = 522, origin = "start")
  gyro_range_bytes = readBin(con, "raw", n = 2)
  gyro_range = readBin(gyro_range_bytes, "integer", size = 2, endian = "little")
  
  # -------------------------------------------------------------------------
  # 2 - Define packets limits (packets' length is not stable)
  
  # find packets start
  packet_header_bytes = as.raw(c(0x4D, 0x44, 0x54, 0x43, 0x50, 0x41, 0x43, 0x4B))
  packet_starti = find_matrix_packet_start(bin_file, packet_header_bytes)
  packet_endi = c(packet_starti[-1] - 1, file.info(bin_file)$size)
  
  # validate packet headers (i.e., nr. of appearances of MDTCPACK in files)
  if (length(packet_starti) != total_packets) {
    stop("Probably corrupted file")
  }
  
  # start time
  # starttime_indices = (packet_starti[1] + 12):(packet_starti[1] + 15)
  seek(con, where = packet_starti[1] + 11, origin = "start")
  starttime_bytes = readBin(con, "raw", n = 4)
  starttime = readBin(starttime_bytes,"integer", size = 4, endian = "little")
  if (is.null(configtz)) {
    starttime_posix = as.POSIXct(starttime, origin = "1970-1-1", tz = desiredtz)
  } else {
    starttime_posix = as.POSIXct(starttime, origin = "1970-1-1", tz = configtz)
  }  
  
  # Select chunk of interest
  packet_starti = packet_starti[start:end]
  packet_endi = packet_endi[start:end]
  
  # -------------------------------------------------------------------------
  # 3 - Check packets integrity with CRC32 checksum
  
  # stored CRC32
  crc32_stored_signed = unlist(lapply(packet_starti + 8, function(pos) {
    seek(con, pos - 1, "start")
    readBin(con, what = "integer", size = 4, n = 1, endian = "little")
  }))
  crc32_stored = ifelse(crc32_stored_signed < 0, crc32_stored_signed + 2^32, crc32_stored_signed)
  
  # observed CRC32
  crc32_observed <- vapply(seq_along(packet_starti), function(i) {
    seek(con, packet_starti[i] + 11, "start")  # Move to correct position
    
    size <- packet_endi[i] - packet_starti[i] - 11  # Calculate the actual size
    if (size <= 0) return(NA_real_)  # Handle edge cases where size is invalid
    
    crc32_observed_raw <- readBin(con, what = "raw", n = size, endian = "little")
    
    crc32_hex <- digest::digest(crc32_observed_raw, algo = "crc32", serialize = FALSE)
    as.numeric(paste0("0x", crc32_hex))
  }, numeric(1))
  
  # unmatching CRC32 means corrupt packet (store this info for later use and log)
  corrupt_packets = which(crc32_observed != crc32_stored)
  
  # Initialize Quality-Check (QC) data frame
  QClog = data.frame(
    checksum_pass = rep(TRUE, length(start:end)),
    blockID = start:end,
    start = integer(length(start:end)),
    end = integer(length(start:end)),
    blockLengthSeconds = numeric(length(start:end)),
    frequency_set = numeric(length(start:end)),
    frequency_observed = numeric(length(start:end)),
    imputed = logical(length(start:end)),
    gap_with_previous_block_secs = numeric(length(start:end)), 
    start_time_adjustment_secs = numeric(length(start:end))
  )
  
  # log corrupt packets
  if (length(corrupt_packets) > 0) {
    QClog$checksum_pass[corrupt_packets] = FALSE
    QClog$imputed[corrupt_packets] = TRUE
  }
  
  # -------------------------------------------------------------------------
  # 4 - Process packets' data 
  
  # start timestamp
  packets_t0 = vapply(seq_along(packet_starti), function(i) {
    seek(con, packet_starti[i] + 11, "start")  # Move to correct position
    readBin(con, what = "integer", size = 4, endian = "little")
  }, numeric(1))
  # end timestamp
  packets_t1 = vapply(seq_along(packet_starti), function(i) {
    seek(con, packet_starti[i] + 15, "start")  # Move to correct position
    readBin(con, what = "integer", size = 4, endian = "little")
  }, numeric(1))
  # acc n recordings
  acc_count = vapply(seq_along(packet_starti), function(i) {
    seek(con, packet_starti[i] + 19, "start")  # Move to correct position
    readBin(con, what = "integer", size = 4, endian = "little")
  }, numeric(1))
  # gyro n recordings
  gyro_count = vapply(seq_along(packet_starti), function(i) {
    seek(con, packet_starti[i] + 23, "start")  # Move to correct position
    readBin(con, what = "integer", size = 4, endian = "little")
  }, numeric(1))
  # temp n recordings
  temp_count = vapply(seq_along(packet_starti), function(i) {
    seek(con, packet_starti[i] + 27, "start")  # Move to correct position
    readBin(con, what = "integer", size = 4, endian = "little")
  }, numeric(1))
  # heart n recordings
  heart_count = vapply(seq_along(packet_starti), function(i) {
    seek(con, packet_starti[i] + 31, "start")  # Move to correct position
    readBin(con, what = "integer", size = 4, endian = "little")
  }, numeric(1))
  
  # Process timestamps and calculate sampling frequency
  packets_dur_s = packets_t1 - packets_t0
  sf_acc_observed = acc_count / packets_dur_s
  sf_acc_p1 = sf_acc_observed[which(packets_dur_s >= 10)[1]]
  expected_sf = c(12.5, 25, 50, 100)
  sf = expected_sf[which.min(abs(expected_sf - sf_acc_p1[1]))]
  if (output == "sf") return(sf)
  
  # log set and observed sf in each packet
  QClog[, "frequency_set"] = sf
  QClog[, "frequency_observed"] = sf_acc_observed
  
  # build data structure lists
  # acc
  acc_starts = packet_starti + 36; acc_stops = acc_starts + 6*acc_count - 1
  if (read_acc) {
    acc_readings = lapply(seq_along(acc_starts), function(i) {
      seek(con, acc_starts[i] - 1, "start")
      readings = readBin(con, what = "integer", size = 2, n = 3 * acc_count[i], endian = "little")
      matrix(readings * (acc_dynrange / 32767), ncol = 3, byrow = T)
    })
  }
  
  # gyro
  prev_stops = acc_stops
  if (any(gyro_count > 0)) { # gyroscope has been activated
    gyro_starts = prev_stops + 1; gyro_stops = gyro_starts + 6*gyro_count - 1
    if (read_gyro) {
      gyro_readings = lapply(seq_along(gyro_starts), function(i) {
        seek(con, gyro_starts[i] - 1, "start")  
        readings = readBin(con, what = "integer", size = 2, n = 3 * gyro_count[i], endian = "little")
        matrix(readings * (gyro_range / 32767), ncol = 3, byrow = T)
      })
    }
    prev_stops = gyro_stops
  }
  # temp
  if (any(temp_count > 0)) { # temperature has been activated
    temp_starts = prev_stops + 1; temp_stops = temp_starts + 4*temp_count - 1
    if (read_temp) {
      temp_readings = lapply(seq_along(temp_starts), function(i) {
        seek(con, temp_starts[i] - 1, "start")
        readings = readBin(con, what = "integer", size = 2, n = 2 * temp_count[i], endian = "little")
        matrix(readings / 10, ncol = 2, byrow = T)
      })
    }
    prev_stops = temp_stops
  }
  # heart
  if (any(heart_count > 0)) { # heart rate has been activated
    heart_starts = prev_stops + 1; heart_stops = heart_starts + 4*heart_count - 1
    if (read_heart) {
      heart_readings = lapply(seq_along(heart_starts), function(i) {
        seek(con, heart_starts[i] - 1, "start")
        readings = readBin(con, what = "integer", size = 2, n = 2 * heart_count[i], endian = "little")
        matrix(readings, ncol = 2, byrow = T)
      })
    }
    prev_stops = heart_stops
  }
  
  # imputation of data in the presence of corrupt packets
  # if there are corrupt packets, then 0,0,1
  if (length(corrupt_packets) > 0) {
    for (i in corrupt_packets) {
      # acceleration to 0,0,1
      if (any(acc_count > 0) & read_acc) {
        acc_readings[[i]] = matrix(c(0,0,1), nrow = nrow(acc_readings[[i]]), 
                                   ncol = 3, byrow = T)
      }
      # gyroscope to 0's
      if (any(gyro_count > 0) & read_gyro) {
        gyro_readings[[i]] = matrix(c(0,0,0), nrow = nrow(gyro_readings[[i]]), 
                                    ncol = 3, byrow = T)
      }
      # temperature to mean temp between prev and next packet
      if (any(temp_count > 0) & read_temp) {
        x = (1:length(packet_starti))
        x[corrupt_packets] = NA
        prev_index = ifelse(any(!is.na(x[1:(i - 1)])), 
                            max(x[1:(i - 1)], na.rm = T), NA)
        prev_temp = temp_readings[[prev_index]][nrow(temp_readings[[prev_index]]), ]
        next_index = ifelse(any(!is.na(x[(i + 1):length(x)])), 
                            min(x[(i + 1):length(x)], na.rm = T), NA)
        next_temp = temp_readings[[next_index]][1, ]
        mean_temp = (prev_temp + next_temp) / 2
        temp_readings[[i]] = matrix(mean_temp, nrow = nrow(temp_readings[[i]]),
                                    ncol = 2, byrow = T)
      }
      # heart to NA
      if (any(temp_count > 0) & read_heart) {
        heart_readings[[i]] = matrix(NA, nrow = nrow(heart_readings[[i]]),
                                     ncol = 2, byrow = T)
      }
    }
  }
  
  # lists to data frames
  if (any(acc_count > 0) & read_acc) acc_data = do.call(rbind, acc_readings)
  if (any(gyro_count > 0) & read_gyro) gyro_data = do.call(rbind, gyro_readings)
  if (any(temp_count > 0) & read_temp) temp_data = do.call(rbind, temp_readings)
  if (any(heart_count > 0) & read_heart) {
    heart_data = do.call(rbind, heart_readings)
    heart_data[heart_data == 0] = NA
  }
  
  # Handle between-packets timestamp gaps and corrections
  gaps = mapply(function(a, b) a - b, packets_t0[-1], packets_t1[-length(packets_t1)])
  lagging_packets = which(gaps >= 1) + 1
  if (length(lagging_packets) > 0) { 
    packets_t0[lagging_packets] = packets_t0[lagging_packets] - gaps[lagging_packets - 1]
    QClog[lagging_packets, "gap_with_previous_block_secs"] = gaps[lagging_packets - 1]
    QClog[lagging_packets, "start_time_adjustment_secs"] = -gaps[lagging_packets - 1]
  }
  
  # as the end timestamp is read as integer (seconds), it needs to be adjusted to the sf
  packets_t1 = packets_t1 - 1 / sf
  QClog[,"start"] = packets_t0
  QClog[,"end"] = packets_t1
  QClog[,"blockLengthSeconds"] = packets_t1 - packets_t0
  
  # Block 3 - Process data ---------------------
  
  # Parse remarks (first 512 bytes)
  seek(con, 0, "start")
  remarks_raw = readBin(con, what = "raw", n = 512, endian = "little")
  remarks = rawToChar(remarks_raw, multiple = F)
  
  # Timestamps (approach by manufacturer: evenly allocate n-recordings timestamps in packet)
  if (any(acc_count > 0) & read_acc) { # accelerometer has been activated
    acc_timestamps = unlist(lapply(seq_along(packets_t0), function(i) {
      seq(from = packets_t0[i], to = packets_t1[i], 
          length.out = acc_count[i])
    }))
  }
  if (any(gyro_count > 0) & read_gyro) { # gyroscope has been activated
    gyro_timestamps = unlist(lapply(seq_along(packets_t0), function(i) {
      seq(from = packets_t0[i], to = packets_t1[i], 
          length.out = gyro_count[i])
    }))
  }
  if (any(temp_count > 0) & read_temp) { # temperature has been activated
    temp_timestamps = unlist(lapply(seq_along(packets_t0), function(i) {
      seq(from = packets_t0[i], to = packets_t1[i], 
          length.out = temp_count[i])
    }))
  }
  if (any(heart_count > 0) & read_heart) { # heart rate has been activated
    heart_timestamps = unlist(lapply(seq_along(packets_t0), function(i) {
      seq(from = packets_t0[i], to = packets_t1[i], 
          length.out = heart_count[i])
    }))
  }
  
  # Resample data to defined sampling frequency
  if (any(acc_count > 0) & read_acc) { # accelerometer has been activated
    required_timepoints = seq(from = acc_timestamps[1], to = acc_timestamps[length(acc_timestamps)], 
                              by = 1/sf)
    acc_resampled = resample(raw = acc_data, rawTime = acc_timestamps, 
                             time = required_timepoints, 
                             stop = nrow(acc_data), type = interpolationType)
  }
  if (any(gyro_count > 0) & read_gyro) { # gyroscope has been activated
    # make sure measurement covers full recording length:
    if (min(gyro_timestamps) > min(required_timepoints)) {
      gyro_timestamps = rbind(c(NA, NA), gyro_data)
    }
    if (max(gyro_timestamps) < max(required_timepoints)) {
      gyro_timestamps = c(gyro_timestamps, max(required_timepoints))
      gyro_data = rbind(gyro_data, c(NA, NA))
    }
    gyro_resampled = resample(raw = gyro_data, rawTime = gyro_timestamps, 
                              time = required_timepoints, 
                              stop = nrow(gyro_data), type = interpolationType)
  }
  if (any(temp_count > 0) & read_temp) { # temperature has been activated
    # make sure measurement covers full recording length:
    if (min(temp_timestamps) > min(required_timepoints)) {
      temp_timestamps = rbind(c(NA, NA), temp_data)
    }
    if (max(temp_timestamps) < max(required_timepoints)) {
      temp_timestamps = c(temp_timestamps, max(required_timepoints))
      temp_data = rbind(temp_data, c(NA, NA))
    }
    temp_resampled = resample(raw = temp_data, rawTime = temp_timestamps, 
                              time = required_timepoints, 
                              stop = nrow(temp_data), type = interpolationType)
  }
  if (any(heart_count > 0) & read_heart) { # heart rate has been activated
    # make sure measurement covers full recording length:
    if (min(heart_timestamps) > min(required_timepoints)) {
      heart_timestamps = rbind(c(NA, NA), heart_data)
    }
    if (max(heart_timestamps) < max(required_timepoints)) {
      heart_timestamps = c(heart_timestamps, max(required_timepoints))
      heart_data = rbind(heart_data, c(NA, NA))
    }
    heart_resampled = resample(raw = heart_data, rawTime = heart_timestamps, 
                               time = required_timepoints, 
                               stop = nrow(heart_data), type = interpolationType)
  }
  
  # build output data -----------
  data = data.frame(time = required_timepoints)
  
  # add sensors if available
  if (any(acc_count > 0) & read_acc) data[, c("acc_x", "acc_y", "acc_z")] = acc_resampled
  if (any(gyro_count > 0) & read_gyro) data[, c("gyro_x", "gyro_y", "gyro_z")] = gyro_resampled
  if (any(temp_count > 0) & read_temp) data[, c("bodySurface_temp", "ambient_temp")] = temp_resampled
  if (any(heart_count > 0) & read_heart) data[, c("hr_raw", "hr")] = heart_resampled
  # add remarks
  data$remarks = remarks
  
  # Return full output
  return(list(
    QClog = QClog,
    data = data,
    sf = sf,
    acc_dynrange = acc_dynrange,
    starttime = starttime_posix,
    lastchunk = lastchunk
  ))
}
