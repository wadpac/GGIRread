readParmayMatrix = function(bin_file, return = c("all", "sf", "dynrange")[1],
                      start = 1, end = NULL,
                      desiredtz = "", configtz = NULL,
                      interpolationType = 1) {
  
  # Matrix devices binary files are organized in packets of data.
  
  # The header information contains:
  #  - remarks (empty in all files I have tested): bytes 1:512
  #  - Count of the total number of packetss in file (bytes 513:516)
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
  
  # Read the binary file
  bin_data = readBin(bin_file, "raw", file.info(bin_file)$size)
  
  # Validate the header recognition string (bytes 513:516)
  header_bytes = bin_data[513:516]
  header_string = rawToChar(header_bytes[header_bytes != 0], multiple = FALSE)
  if (header_string != "MDTC") {
    stop("Invalid header recognition string.")
  }
  
  # Extract the total number of packets (bytes 517:520)
  total_packets = readBin(bin_data[517:520], "integer", size = 4, endian = "little")
  
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
  acc_dynrange = readBin(bin_data[521:522], "integer", size = 2, endian = "little")
  if (return == "dynrange") return(acc_dynrange)
  gyro_range = readBin(bin_data[523:524], "integer", size = 2, endian = "little")
  
  # -------------------------------------------------------------------------
  # 2 - Define packets limits (packets' length is not stable)
  
  # find packets start
  packet_header_bytes = as.raw(c(0x4D, 0x44, 0x54, 0x43, 0x50, 0x41, 0x43, 0x4B))
  packet_starti = find_matrix_packet_start(bin_data, packet_header_bytes)
  packet_endi = c(packet_starti[-1] - 1, length(bin_data))
  
  # validate packet headers (i.e., nr. of appearances of MDTCPACK in files)
  if (length(packet_starti) != total_packets) {
    stop("Probably corrupted file")
  }
  
  # start time
  starttime_indices = (packet_starti[1] + 12):(packet_starti[1] + 15)
  starttime = readBin(bin_data[starttime_indices],"integer", size = 4, endian = "little")
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
  crc32_stored_indices = unlist(lapply(packet_starti + 8, function(x) seq(x, x + 3)))
  crc32_stored_signed = readBin(bin_data[crc32_stored_indices], "integer", size = 4, n = total_packets, endian = "little")
  crc32_stored = ifelse(crc32_stored_signed < 0, crc32_stored_signed + 2^32, crc32_stored_signed)
  
  # observed CRC32
  crc32_observed_raw = Map(function(start, stop) bin_data[start:stop], packet_starti + 12, packet_endi)
  crc32_observed = unlist(lapply(seq_along(packet_starti), function(i) {
    digest_crc32 = digest::digest(crc32_observed_raw[[i]], 
                                  algo = "crc32", serialize = FALSE)
    as.numeric(paste0("0x", unlist(digest_crc32)))
  }))
  
  # unmatching CRC32 means corrupt packet (store this infor for later use and log)
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
  start_timestamp_indices = unlist(lapply(packet_starti + 12, function(x) seq(x, x + 3)))
  start_timestamp_raw = matrix(bin_data[start_timestamp_indices], byrow = T, ncol = 4)
  # end timestamp
  end_timestamp_indices = unlist(lapply(packet_starti + 16, function(x) seq(x, x + 3)))
  end_timestamp_raw = matrix(bin_data[end_timestamp_indices], byrow = T, ncol = 4)
  # acc n recordings
  acc_count_indices = unlist(lapply(packet_starti + 20, function(x) seq(x, x + 3)))
  acc_count = readBin(bin_data[acc_count_indices], "integer", size = 4, n = total_packets, endian = "little")
  # gyro n recordings
  gyro_count_indices = unlist(lapply(packet_starti + 24, function(x) seq(x, x + 3)))
  gyro_count = readBin(bin_data[gyro_count_indices], "integer", size = 4, n = total_packets, endian = "little")
  # temp n recordings
  temp_count_indices = unlist(lapply(packet_starti + 28, function(x) seq(x, x + 3)))
  temp_count = readBin(bin_data[temp_count_indices], "integer", size = 4, n = total_packets, endian = "little")
  # heart n recordings
  heart_count_indices = unlist(lapply(packet_starti + 32, function(x) seq(x, x + 3)))
  heart_count = readBin(bin_data[heart_count_indices], "integer", size = 4, n = total_packets, endian = "little")
  
  # Process timestamps and calculate sampling frequency
  start_timestamps = apply(start_timestamp_raw, 1, function(x) readBin(x, "integer", size = 4, endian = "little"))
  end_timestamps = apply(end_timestamp_raw, 1, function(x) readBin(x, "integer", size = 4, endian = "little"))
  packets_dur_s = end_timestamps - start_timestamps
  sf_acc_observed = acc_count / packets_dur_s
  sf_acc_p1 = sf_acc_observed[which(packets_dur_s >= 10)[1]]
  expected_sf = c(12.5, 25, 50, 100)
  sf = expected_sf[which.min(abs(expected_sf - sf_acc_p1[1]))]
  if (return == "sf") return(sf)
  
  # log set and observed sf in each packet
  QClog[, "frequency_set"] = sf
  QClog[, "frequency_observed"] = sf_acc_observed
  
  # build data structure lists
  # acc
  acc_starts = packet_starti + 36; acc_stops = acc_starts + 6*acc_count - 1
  acc_data_raw = Map(function(start, stop) bin_data[start:stop], acc_starts, acc_stops)
  prev_stops = acc_stops
  # gyro
  if (any(gyro_count) > 0) { # gyroscope has been activated
    gyro_starts = prev_stops + 1; gyro_stops = gyro_starts + 6*gyro_count - 1
    gyro_data_raw = Map(function(start, stop) bin_data[start:stop], gyro_starts, gyro_stops)
    prev_stops = gyro_stops
  }
  # temp
  if (any(temp_count) > 0) { # temperature has been activated
    temp_starts = prev_stops + 1; temp_stops = temp_starts + 4*temp_count - 1
    temp_data_raw = Map(function(start, stop) bin_data[start:stop], temp_starts, temp_stops)
    prev_stops = temp_stops
  }
  # heart
  if (any(heart_count) > 0) { # heart rate has been activated
    heart_starts = prev_stops + 1; heart_stops = heart_starts + 4*heart_count - 1
    heart_data_raw = Map(function(start, stop) bin_data[start:stop], heart_starts, heart_stops)
    prev_stops = heart_stops
  }
  
  # Handle between-packets timestamp gaps and corrections
  gaps = mapply(function(a, b) a - b, start_timestamps[-1], end_timestamps[-length(end_timestamps)])
  lagging_packets = which(gaps >= 1) + 1
  if (length(lagging_packets) > 0) { 
    start_timestamps[lagging_packets] = start_timestamps[lagging_packets] - gaps[lagging_packets - 1]
    QClog[lagging_packets, "gap_with_previous_block_secs"] = gaps[lagging_packets - 1]
    QClog[lagging_packets, "start_time_adjustment_secs"] = -gaps[lagging_packets - 1]
  }
  
  # as the end timestamp is read as integer (seconds), it needs to be adjusted to the sf
  end_timestamps = end_timestamps - 1 / sf
  QClog[,"start"] = start_timestamps
  QClog[,"end"] = end_timestamps
  QClog[,"blockLengthSeconds"] = end_timestamps - start_timestamps
  
  # Block 3 - Process data ---------------------
  
  # Parse remarks (first 512 bytes)
  remarks_raw = bin_data[1:512]
  remarks = rawToChar(remarks_raw, multiple = F)
  
  # Timestamps (approach by manufacturer, evenly allocate n-recordings timestamps in packet)
  packet_t0 = start_timestamps
  packet_t1 = end_timestamps
  acc_timestamps = unlist(lapply(seq_along(packet_t0), function(i) {
    seq(from = packet_t0[i], to = packet_t1[i], 
        length.out = acc_count[i])
  }))
  if (any(gyro_count) > 0) { # gyroscope has been activated
    gyro_timestamps = unlist(lapply(seq_along(packet_t0), function(i) {
      seq(from = packet_t0[i], to = packet_t1[i], 
          length.out = gyro_count[i])
    }))
  }
  if (any(temp_count) > 0) { # temperature has been activated
    temp_timestamps = unlist(lapply(seq_along(packet_t0), function(i) {
      seq(from = packet_t0[i], to = packet_t1[i], 
          length.out = temp_count[i])
    }))
  }
  if (any(heart_count) > 0) { # heart rate has been activated
    heart_timestamps = unlist(lapply(seq_along(packet_t0), function(i) {
      seq(from = packet_t0[i], to = packet_t1[i], 
          length.out = heart_count[i])
    }))
  }
  
  # acc sensor data
  acc_readings = lapply(seq_along(packet_t0), function(i) {
    readings = readBin(acc_data_raw[[i]], "integer", size = 2, n = acc_count[i] * 3, endian = "little")
    readings * (acc_dynrange / 32767)
  })
  if (length(corrupt_packets) > 0) {
    for (i in corrupt_packets) {
      acc_readings[[i]] = rep(c(0,0,1), length(acc_readings[[i]]) / 3)
    }
  }
  acc_data = matrix(unlist(acc_readings), ncol = 3, byrow = T)
  
  # gyro sensor data
  if (any(gyro_count) > 0) { # gyroscope has been activated
    gyro_readings = lapply(seq_along(packet_t0), function(i) {
      readings = readBin(gyro_data_raw[[i]], "integer", size = 2, n = gyro_count[i] * 3, endian = "little")
      readings * (gyro_range / 32767)
    })
    if (length(corrupt_packets) > 0) {
      for (i in corrupt_packets) {
        gyro_readings[[i]] = rep(0, length(gyro_readings[[i]]))
      }
    }
    gyro_data = matrix(unlist(gyro_readings), ncol = 3, byrow = T)
  }
  # temp sensor data
  if (any(temp_count) > 0) { # temperature has been activated
    temp_readings = lapply(seq_along(packet_t0), function(i) {
      readings = readBin(temp_data_raw[[i]], "integer", size = 2, n = temp_count[i] * 3, endian = "little")
      readings / 10
    })
    if (length(corrupt_packets) > 0) {
      for (i in corrupt_packets) {
        # for imputing temperature, I use the average value between the previous
        # valid value and the following valid value
        x = (1:length(packet_starti))
        x[corrupt_packets] = NA
        prev_temp = ifelse(any(!is.na(x[1:(i - 1)])), 
                           tail(x[which(!is.na(x[1:(i - 1)]))], 1), NA)
        next_temp = ifelse(any(!is.na(x[1:(i - 1)])), 
                           head(x[which(!is.na(x[1:(i - 1)]))], 1), NA)
        temp_readings[[i]] = rep(mean(unlist(temp_readings[prev_temp:next_temp]), na.rm = T), 
                                 times = length(temp_readings[[i]]))
      }
    }
    temp_data = matrix(unlist(temp_readings), ncol = 2, byrow = T)
  }
  # heart sensor data
  if (any(heart_count) > 0) { # heart rate has been activated
    heart_readings = lapply(seq_along(packet_t0), function(i) {
      readings = readBin(heart_data_raw[[i]], "integer", size = 2, n = heart_count[i] * 3, endian = "little")
      readings
    })
    if (length(corrupt_packets) > 0) {
      for (i in corrupt_packets) {
        heart_readings[[i]] = rep(NA, length(heart_readings[[i]]))
      }
    }
    heart_data = matrix(unlist(heart_readings), ncol = 2, byrow = T)
  }
  # Resample data to defined sampling frequency
  required_timepoints = seq(from = acc_timestamps[1], to = acc_timestamps[length(acc_timestamps)], 
                            by = 1/sf)
  acc_resampled = resample(raw = acc_data, rawTime = acc_timestamps, 
                           time = required_timepoints, 
                           stop = nrow(acc_data), type = interpolationType)
  if (any(gyro_count) > 0) { # gyroscope has been activated
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
  if (any(temp_count) > 0) { # temperature has been activated
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
  if (any(heart_count) > 0) { # heart rate has been activated
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
  
  # data
  data = data.frame(time = required_timepoints)
  
  # add sensors if available
  data[, c("acc_x", "acc_y", "acc_z")] = acc_resampled
  if (any(gyro_count > 0)) data[, c("gyro_x", "gyro_y", "gyro_z")] = gyro_resampled
  if (any(temp_count > 0)) data[, c("bodySurface_temp", "ambient_temp")] = temp_resampled
  if (any(heart_count > 0)) data[, c("hr_raw", "hr")] = heart_resampled
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
