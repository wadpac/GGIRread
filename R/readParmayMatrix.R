readParmayMatrix = function(filename, output = c("all", "sf", "dynrange")[1],
                            start = 1, end = NULL,
                            desiredtz = "", configtz = NULL,
                            interpolationType = 1,
                            read_acc = TRUE, read_gyro = FALSE, 
                            read_temp = TRUE, read_heart = FALSE) {
  
  # Matrix devices binary files are organized in packets of data.
  # https://www.parmaytech.com/devices/en-matrix
  # https://drive.google.com/drive/folders/1WkeBUjcP52GFwaJPkTL9sQ1YcX-_P8pJ
  
  # The header information contains:
  #  - remarks (empty in all files I have tested): bytes 1:512
  #  - header string = "MDTC" (bytes 513:516, if not there -> file corrupt)
  #  - Count of the total number of packets in file (bytes 517:520)
  #  - range of the acc (bytes 521:522) and the gyro sensors (bytes 523:524)
  
  # Each packet contains the following information:
  #  - 8-byte package header
  #  - 4-byte CRC32 indicator
  #  - 4-byte start timestamp of the packet
  #  - 4-byte end timestamp of the packet
  #  - 4-byte number of accelerometer recordings in packet (a)
  #  - 4-byte number of gyroscope recordings in packet (g)
  #  - 4-byte number of temperature recordings in packet (t)
  #  - 4-byte number of heart rate recordings in packet (h)
  #  - (6*a)-byte accelerometer recordings
  #  - (6*g)-byte gyroscope recordings
  #  - (4*t)-byte temperature recordings
  #  - (4*h)-byte heart rate recordings
  
  # Notes:
  #  - Packet lengths vary with the number of recordings in each packet, so dynamic definition of the length of each packet is needed
  #  - Sampling frequency is not stable, with different number of sensor recordings across packets. 
  #        - checksum is used to verify integrity of data in each packet 
  #        - resampling is used with resample function to make the frequency stable
  #  - Gaps in data are not expected
  # -------------------------------------------------------------------------
  # Helper function to resample matrix sensor signals onto a common time grid, 
  # usually the accelerometer's reference timeline.
  # data = matrix with the sensor data
  # timestamps = numeric vector of timestamps corresponding to rows fo `data`
  # grid = numeric vector of target timestamps (usually, accelerometer resampled timestamps)
  # interpolationType = 
  resample_to_grid = function(data, timestamps, grid, interpolationType = 1) {
    
    ## ---- 0. Coerce shapes early ----------------------------------------------
    if (is.matrix(timestamps) || is.data.frame(timestamps)) {
      timestamps = timestamps[, 1]
    }
    timestamps = as.vector(timestamps)
    data = as.matrix(data)
    
    common_len = min(NROW(data), length(timestamps))
    if (common_len == 0L) {
      out = matrix(NA_real_, nrow = length(grid), ncol = NCOL(data))
      colnames(out) = colnames(data)
      return(out)
    }
    data = data[seq_len(common_len), , drop = FALSE]
    timestamps = timestamps[seq_len(common_len)]
    
    ## ---- 1. Normalize input types ------------------------------------------
    # Convert POSIXct timestamps to numeric (seconds since epoch) if necessary
    is_posix = inherits(timestamps, "POSIXct")
    tz = if (is_posix) attr(timestamps, "tzone") else NULL
    ts_num = if (is_posix) as.numeric(timestamps) else as.numeric(timestamps)
    grid_num = if (inherits(grid, "POSIXct")) as.numeric(grid) else as.numeric(grid)
    
    data = as.matrix(data)
    
    ## ---- 2. Clean and sort input data --------------------------------------
    # Remove rows with NA timestamps or NA values in any data column
    ok = is.finite(ts_num) & rowSums(!is.finite(data)) == 0
    ts_num = ts_num[ok]
    data = data[ok, , drop = FALSE]
    
    # Ensure strictly increasing timestamps
    ord = order(ts_num)
    ts_num = ts_num[ord]
    data = data[ord, , drop = FALSE]
    
    ## ---- 3. Remove duplicate timestamps ------------------------------------
    # Duplicate timestamps cause division-by-zero errors in interpolation.
    # Combine duplicates by taking the column-wise mean.
    if (any(duplicated(ts_num))) {
      split_ix = split(seq_along(ts_num), ts_num)
      ts_unique = as.numeric(names(split_ix))
      data_unique = do.call(rbind, lapply(split_ix, function(ix) {
        colMeans(data[ix, , drop = FALSE])
      }))
      ord2 = order(ts_unique)
      ts_num = ts_unique[ord2]
      data = data_unique[ord2, , drop = FALSE]
    }
    
    ## ---- 4. Ensure matching length and valid range -------------------------
    # The C++ function assumes length(rawTime) == nrow(raw)
    n_raw = min(length(ts_num), nrow(data))
    if (n_raw < 2L) {
      # Not enough data to interpolate -> return all NA
      out = matrix(NA_real_, nrow = length(grid_num), ncol = ncol(data))
      colnames(out) = colnames(data)
      return(out)
    }
    
    ts_num = ts_num[seq_len(n_raw)]
    data = data[seq_len(n_raw), , drop = FALSE]
    t0 = ts_num[1]
    t1 = ts_num[n_raw]
    
    ## ---- 5. Clip the grid to the sensor s valid range ----------------------
    # Only pass grid points within [t0, t1] to C++ for interpolation.
    # The rest will be filled with NA later.
    inside = (grid_num >= t0) & (grid_num <= t1)
    grid_in = grid_num[inside]
    
    ## ---- 6. Interpolate within valid range ---------------------------------
    res_in = matrix(NA_real_, nrow = sum(inside), ncol = ncol(data))
    if (length(grid_in) > 0L) {
      res_in = resample(
        raw     = data,
        rawTime = ts_num,
        time    = grid_in,
        stop    = nrow(data),
        type    = interpolationType
      )
    }
    
    ## ---- 7. Pad output to full grid length ---------------------------------
    # Build output with same number of rows as the target grid.
    # Fill with NA outside sensor coverage, and insert resampled data inside.
    out = matrix(NA_real_, nrow = length(grid_num), ncol = ncol(data))
    if (length(grid_in) > 0L) {
      out[inside, ] = res_in
    }
    
    # Preserve column names for clarity in downstream use
    colnames(out) = colnames(data)
    out
  }
  # -------------------------------------------------------------------------
  # Main code
  # 1 - read file and extract header information

  # open connection to read the binary file
  con = file(filename, "rb")
  on.exit(close(con))

  # Validate the header recognition string (bytes 513:516)
  seek(con, where = 512, origin = "start")
  header_bytes = readBin(con, "raw", n = 4)
  header_string = rawToChar(header_bytes[header_bytes != 0], multiple = FALSE)
  if (header_string != "MDTC") {
    stop(paste0(basename(filename), ": Invalid header recognition string."))
  }
  
  # Extract the total number of packets (bytes 517:520)
  seek(con, where = 516, origin = "start")
  tPackets_bytes = readBin(con, "raw", n = 4)
  total_packets = readBin(tPackets_bytes, "integer", size = 4, endian = "little")
  
  # Validate start and end packets
  if (is.null(end)) end = total_packets
  if (end >= total_packets) {
    end = total_packets
  }
  if (start < 1) start = 1
  if (start > total_packets) return(NULL)
  
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
  packet_starti = find_matrix_packet_start(filename, packet_header_bytes)
  packet_endi = c(packet_starti[-1] - 1, file.info(filename)$size)
  
  # save number of packets declared and observed
  n_declared = total_packets
  n_observed_file = length(packet_starti)
  
  # Use all packets available in the file (even if empty packets have been added at the end)
  n_use = n_observed_file
  packet_starti = packet_starti[seq_len(n_use)]
  packet_endi = packet_endi[seq_len(n_use)]
  
  # Apply chunking AFTER truncation
  i_from = max(1, start)
  i_to = n_use
  idx = seq.int(i_from, i_to)
  n_rows = length(idx)
  
  packet_starti = packet_starti[idx]
  packet_endi   = packet_endi[idx]
  
  # lastchunk should reflect the truncated space
  lastchunk = (i_to == n_use)
  
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
  
  # -------------------------------------------------------------------------
  # 3 - Check packets integrity with CRC32 checksum
  
  # stored CRC32
  crc32_stored_signed = unlist(lapply(packet_starti + 8, function(pos) {
    seek(con, pos - 1, "start")
    readBin(con, what = "integer", size = 4, n = 1, endian = "little")
  }))
  crc32_stored = ifelse(crc32_stored_signed < 0, crc32_stored_signed + 2^32, crc32_stored_signed)
  
  # observed CRC32
  crc32_observed = vapply(seq_along(packet_starti), function(i) {
    seek(con, packet_starti[i] + 11, "start")  # Move to correct position
    
    size = packet_endi[i] - packet_starti[i] - 11  # Calculate the actual size
    if (size <= 0) return(NA_real_)  # Handle edge cases where size is invalid
    
    crc32_observed_raw = readBin(con, what = "raw", n = size, endian = "little")
    
    crc32_hex = digest::digest(crc32_observed_raw, algo = "crc32", serialize = FALSE)
    as.numeric(paste0("0x", crc32_hex))
  }, numeric(1))
  
  # unmatching CRC32 means corrupt packet (store this info for later use and log)
  corrupt_packets = which(crc32_observed != crc32_stored)
  
  # Initialize Quality-Check (QC) data frame
  QClog = data.frame(
    checksum_pass = rep(TRUE, n_rows),
    blockID = idx,
    start = integer(n_rows),
    end = integer(n_rows),
    blockLengthSeconds = numeric(n_rows),
    frequency_set = numeric(n_rows),
    frequency_observed = numeric(n_rows),
    imputed = logical(n_rows),
    gap_with_previous_block_secs = numeric(n_rows), 
    start_time_adjustment_secs = numeric(n_rows),
    declared_packets = rep(n_declared, n_rows),
    observed_packets = rep(n_observed_file, n_rows),
    acc_recorded = TRUE
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
  
  # At the moment the function requires that accelerometer data has been
  # recorded to work, as accelerometer timestamps are used as reference time
  if (sum(acc_count) == 0) {
    QClog[, "acc_recording"] = FALSE
    return(list(
      QClog = QClog,
      data = matrix(nrow = 0, ncol = 3),
      header = list(sf = sf,
                    acc_dynrange = acc_dynrange,
                    starttime = starttime_posix),
      lastchunk = TRUE
    ))
  }
  
  # build data structure lists
  # acc
  acc_starts = packet_starti + 36; acc_stops = acc_starts + 6*acc_count - 1
  if (read_acc) {
    acc_readings = lapply(seq_along(acc_starts), function(i) {
      seek(con, acc_starts[i] - 1, "start")
      readings = readBin(con, what = "integer", size = 2, n = 3 * acc_count[i], endian = "little")
      # make sure number of readings is multiple of 3 (might not be in corrupted packets)
      n_triplets = length(readings) %/% 3
      readings_ok = 3 * n_triplets
      readings = readings[1:readings_ok]
      denominator = ifelse(readings >= 0, 32767, 32768)
      matrix(readings * (acc_dynrange / denominator), ncol = 3, byrow = T)
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
        # make sure number of readings is multiple of 3 (might not be in corrupted packets)
        n_triplets = length(readings) %/% 3
        readings_ok = 3 * n_triplets
        readings = readings[1:readings_ok]
        denominator = ifelse(readings >= 0, 32767, 32768)
        matrix(readings * (gyro_range / denominator), ncol = 3, byrow = T)
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
        # make sure number of readings is multiple of 2 (might not be in corrupted packets)
        n_triplets = length(readings) %/% 2
        readings_ok = 2 * n_triplets
        readings = readings[1:readings_ok]
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
        # make sure number of readings is multiple of 3 (might not be in corrupted packets)
        n_triplets = length(readings) %/% 2
        readings_ok = 2 * n_triplets
        readings = readings[1:readings_ok]
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
        if (!is.na(prev_index)) {
          if (temp_count[prev_index] > 0) {
            prev_temp = temp_readings[[prev_index]][nrow(temp_readings[[prev_index]]), ]
          } else {
            prev_temp = NA
          }
        } else {
          # if the first packet is corrupted, then impute by first non-corrupted temperature observed
          first_valid_temp = which(!1:length(temp_count) %in% corrupt_packets)[1]
          if (is.na(first_valid_temp)) {
            prev_temp = NA
          } else {
            prev_temp = temp_readings[[first_valid_temp]][1,]  
          }
        }
        # next index
        next_index = ifelse(any(!is.na(x[(i + 1):length(x)])), 
                            min(x[(i + 1):length(x)], na.rm = T), NA)
        if (!is.na(next_index)) {
          if (temp_count[next_index] > 0) {
            next_temp = temp_readings[[next_index]][1, ]
          } else {
            next_temp = NA
          }
        } else {
          next_temp = NA
        }
        if (!is.na(prev_temp[1]) & !is.na(next_temp[1])) {
          mean_temp = (prev_temp + next_temp) / 2
        } else if (is.na(prev_temp[1]) & !is.na(next_temp[1])) {
          mean_temp = next_temp
        } else if (!is.na(prev_temp[1]) & is.na(next_temp[1])) {
          # if the last packet is corrupted, then impute by last temperature observed
          mean_temp = prev_temp
        } else {
          # safe conduct, if both are NA, then leave it without imputation
          mean_temp = temp_readings[[i]]
        }
       
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
    t_start = acc_timestamps[1]
    t_end = acc_timestamps[nrow(acc_data)]
    n_required_timepoints = floor((t_end - t_start) * sf) + 1L
    required_timepoints = seq(from = acc_timestamps[1], by = 1/sf, 
                              length.out = n_required_timepoints)
    acc_resampled = resample(raw = acc_data, rawTime = acc_timestamps, 
                             time = required_timepoints, 
                             stop = nrow(acc_data), type = interpolationType)
  }
  if (any(gyro_count > 0) & read_gyro) { # gyroscope has been activated
    gyro_resampled = resample_to_grid(
      data = gyro_data,
      timestamps = gyro_timestamps,
      grid = required_timepoints,
      interpolationType = interpolationType
    )
  }
  if (any(temp_count > 0) & read_temp) {
    temp_resampled = resample_to_grid(
      data = temp_data,
      timestamps = temp_timestamps,
      grid = required_timepoints,
      interpolationType = interpolationType
    )
  }
  if (any(heart_count > 0) & read_heart) {
    heart_resampled = resample_to_grid(
      data = heart_data,
      timestamps = heart_timestamps,
      grid = required_timepoints,
      interpolationType = interpolationType
    )
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
    header = list(sf = sf,
                  acc_dynrange = acc_dynrange,
                  starttime = starttime_posix),
    lastchunk = lastchunk
  ))
}
