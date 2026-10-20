readAxivity = function(filename, start = 0, end = 0, progressBar = FALSE, desiredtz = "",
                       configtz = c(), interpolationType = 1, loadbattery = FALSE,
                       header = NULL, frequency_tol = 0.1,
                       maxAllowedCorruptBlocks = 20) {
  if (length(configtz) == 0) configtz = desiredtz
  blockBytes = 512
  headerBytes = 1024

  # Credits: The original version of this code developed outside GitHub was
  # contributed by Dr. Evgeny Mirkes (Leicester University, UK)
  #========================================================================
  # Documentation moved to standard place for documentation which is readAxivity.Rd
  # Background info on data format:
  # https://github.com/digitalinteraction/openmovement/blob/master/Docs/ax3/ax3-technical.md
  #############################################################################

  # Internal functions
  timestampDecoder = function(coded, fraction, shift, struc, configtz) {
    timestamp_numeric = struc[[1]]

    # make sure timestamps are somewhat continuous,
    # and there hasn't been a large gap since the previous timestamp
    coded_no_seconds = bitwShiftR(coded, 6)
    if (coded_no_seconds != struc[[3]]) {
      timestamp_numeric = 0
    }

    if (timestamp_numeric == 0) {
      # very first timestamp, or the first one after a gap

      # Extract parts of date
      year = bitwAnd(bitwShiftR(coded, 26), 0x3fL) + 2000
      month = bitwAnd(bitwShiftR(coded, 22), 0x0fL)
      day = bitwAnd(bitwShiftR(coded, 17), 0x1f)
      hours = bitwAnd(bitwShiftR(coded, 12), 0x1fL)
      mins = bitwAnd(coded_no_seconds, 0x3fL)
      secs = bitwAnd(coded, 0x3fL)
      # Form string representation of date and convert it to number
      timestamp_text = as.POSIXct(paste0(year, "-", month, "-", day, " ",
                                         hours, ":", mins, ":", secs),
                                  tz = configtz)
      timestamp_numeric = as.numeric(timestamp_text)
    } else {
      secs = bitwAnd(coded, 0x3fL)
      oldSecs = struc[[2]]
      if (secs < oldSecs) oldSecs = oldSecs - 60
      timestamp_numeric = timestamp_numeric + (secs - oldSecs)
    }
    struc <- list(timestamp_numeric, secs, coded_no_seconds)
    # Add fractional part and shift
    start = timestamp_numeric + fraction / 65536 + shift
    invisible(list(start = start, struc = struc))
  }

  readDataBlock = function(fid, complete = TRUE, struc = list(0,0L,0), parameters = NULL) {
    # Decode one packet in C++; keep timezone conversion and timestamp state in R.
    block = readBin(fid, raw(), n = blockBytes)
    if (length(block) < blockBytes) return(NULL)
    decoded = decodeAxivityBlock(block, complete, parameters, loadbattery)
    if (!decoded$checksum_pass) {
      return(invisible(list(struc = struc, parameters = parameters,
                            checksum_pass = FALSE)))
    }
    if (decoded$blockID == 0) {
      # Force full calendar conversion after a device sequence reset.
      struc[[1]] = 0
    }
    tsDeco = timestampDecoder(decoded$timeStamp, decoded$fractional,
                              decoded$shift, struc, configtz)
    rawdata_list = list(
      frequency = decoded$frequency,
      start = tsDeco$start,
      temperature = decoded$temperature,
      battery = decoded$battery,
      light = decoded$light,
      length = decoded$length,
      struc = tsDeco$struc,
      parameters = decoded$parameters,
      checksum_pass = TRUE,
      blockID = decoded$blockID
    )
    if (complete) rawdata_list$data = decoded$data
    return(invisible(rawdata_list))
  }

  readHeader = function(fid, numDBlocks) {
    # fid is file identifier
    # numDBlocks is number of data blocks
    #
    # Read file header and return it as a list with following elements
    #   uniqueSerialCode is unque serial code of used device
    #   frequency is measurement frequency. All data will be resampled
    #       for this frequency.
    #   start is timestamp in numeric form. To get text representation
    #       it is enough to use
    #           as.POSIXct(start, origin = "1970-01-01", tz=desiredtz)
    #   device is "Axivity"
    #   firmwareVersion is version of firmware
    #   blocks is number of datablocks with 80 or 120 observations in each
    #       Unfortunately frequency of measurement is varied in this device.
    #

    # Start from the file origin
    seek(fid,0)
    block = readBin(fid, raw(), n=headerBytes)

    # Read block header and check correctness of name
    idstr = readChar(block, 2, useBytes = TRUE) #offset 0 1
    if (idstr != "MD") {
      stop("Header block is incorrect. First two characters must be MD.")
    }

    # offset 4 encodes hardware type: AX6 or AX3
    hwType = readBin(block[5], integer(), size = 1, signed = FALSE)
    if (hwType == 0x64) {
      hardwareType = "AX6"
    } else {
      hardwareType = "AX3"
    }
    # session id and device id
    lowerDeviceId = readBin(block[6:7], integer(), size = 2, signed = FALSE, endian = "little") #offset 5 6
    sessionID = readBin(block[8:11], integer(), size = 4, endian = "little") #offset 7 8 9 10
    upperDeviceId = readBin(block[12:13], integer(), size = 2, signed = FALSE, endian = "little") #offset 11 12
    if (upperDeviceId == 65535) {
      upperDeviceId = 0
    }
    uniqueSerialCode = bitwOr(bitwShiftL(upperDeviceId, 16), lowerDeviceId)
    # sample rate and dynamic range accelerometer
    samplerate_dynrange = readBin(block[37], integer(), size = 1, signed = FALSE) #offset 36
    frequency_header = round( 3200 / bitwShiftL(1, 15 - bitwAnd(samplerate_dynrange, 15)))
    accrange = bitwShiftR(16, (bitwShiftR(samplerate_dynrange, 6)))
    version = readBin(block[42], integer(), size = 1, signed = FALSE) #offset 41

    # Read the first data block without data.
    # Skip any corrupt blocks, up to maxAllowedCorruptBlocks in number.
    is_corrupt = TRUE
    for (ii in 0:min(numDBlocks-1, maxAllowedCorruptBlocks)) {
      datas = readDataBlock(fid, complete = FALSE)

      if (is.null(datas)) {
        stop("Error reading the first data block.")
      }

      if (datas$checksum_pass) {
        is_corrupt = FALSE
        break
      }

      warning("Skipping corrupt block #", ii)
    }
    if (is_corrupt) {
      if (ii==numDBlocks-1) {
        stop("Error reading file. Every block is corrupt.")
      }
      stop("Error reading file. The first ", maxAllowedCorruptBlocks+1, " blocks are corrupt.")
    }

    if (frequency_header != datas$frequency) {
      warning("Inconsistent value of measurement frequency: there is ",
              frequency_header, " in header and ", datas$frequency, " in the first data block. ")
    }

    blockLength = datas$length # number of samples in a block

    start = as.POSIXct(datas$start, origin = "1970-01-01", tz = desiredtz)

    return(invisible(list(
      uniqueSerialCode = uniqueSerialCode, frequency = frequency_header,
      start = start,
      device = "Axivity", firmwareVersion = version, blocks = numDBlocks,
      accrange = accrange, hardwareType = hardwareType, blockLength = blockLength
    )))
  }


  ################################################################################################
  # Main function

  # Parse input arguments
  nargin = nargs()
  if (nargin < 1) {
    stop("At least file must be specified")
  }
  # Get file size in data blocks
  numDBlocks = round(file.size(filename) / blockBytes) - 2
  # Open file
  fid = file(filename,"rb")
  on.exit({
    close(fid)
  })

  QClog = NULL # Initialise log of data quality issues

  #############################################################################
  # read header
  if (is.null(header)) {
    header = readHeader(fid, numDBlocks)
  }
  blockLength = header$blockLength # number of samples in a block
  step = 1/header$frequency

  if (start < 0) {
    start = 0
  }
  if (end > numDBlocks) {
    end = numDBlocks
  }
  # If data is not necessary then stop work
  if (end <= start) {
    return(invisible(list(header = header, data = NULL)))
  }

  # Read the end block, to determine the end timestamp.
  struc = list(0,0L,0)
  if (end < numDBlocks) { # the end block isn't part of the data we'll read, but its start will be our ending timestamp
    seek(fid, headerBytes + blockBytes * end, origin = 'start')

    # Skip any corrupt blocks, up to maxAllowedCorruptBlocks in number.
    # Skip forward, so we'll end up reading more blocks than requested.
    # This is important because when corrupt blocks are in the beginning of the requested interval,
    # we also skip forward, ending up with fewer blocks than requested.
    # for example, if we have a file with corrupt blocks 8-12, and we read the file 10 blocks at a time,
    # then the 1st request will end up with blocks 1-12, and the 2nd request with blocks 13-20,
    # and between the two requests, all 20 blocks will be accounted for.
    is_corrupt = TRUE
    for (ii in end : min(numDBlocks-1, end+maxAllowedCorruptBlocks)) {
      endBlock = readDataBlock(fid, struc = struc)

      if (endBlock$checksum_pass) {
        is_corrupt = FALSE
        break
      }

      warning("Skipping corrupt end block #", ii)
      end = end + 1
    }
    if (is_corrupt && ii == end+maxAllowedCorruptBlocks) {
      stop("Error reading file. The last ", maxAllowedCorruptBlocks+1, " blocks are corrupt.")
    }
    endTimestamp = as.numeric(endBlock$start)
  }

  if (end == numDBlocks) {
    # end == numDBlocks, meaning we'll be reading all the remaining blocks.
    # There is no block #numDBlocks, so we can't get the ending timestamp from the start of that block.
    # Instead read the very last block of the file (if the last block is corrupt, fing the last non-corrupt one),
    # then project what the ending timestamp should be.

    # Skip any corrupt blocks, up to maxAllowedCorruptBlocks in number.
    # SInce we are at the last block, we have to go backwards.
    is_corrupt = TRUE
    for (ii in (end-1) : max(start, end-maxAllowedCorruptBlocks-1)) {
      seek(fid, headerBytes + blockBytes * ii, origin = 'start')
      lastBlock = readDataBlock(fid, struc = struc)

      if (lastBlock$checksum_pass) {
        is_corrupt = FALSE
        break
      }

      warning("Skipping corrupt end block #", ii)
      end = end - 1
    }
    if (is_corrupt) {
      if (start == end) {
        stop("Error reading file. All requested blocks are corrupt.")
      }
      stop("Error reading file. The last ", maxAllowedCorruptBlocks+1, " blocks are corrupt.")
    }
    # Treat the last valid packet as the physical end for native processing.
    # Otherwise the loop expects a lookahead packet and never resamples it.
    # The header retains the original file's block count.
    numDBlocks = end
    # the end timestamp should fall right after the actual very last timestamp of the file
    endTimestamp = as.numeric(lastBlock$start) + blockLength * step
    # now pad it generously in case there are gaps in the last block
    endTimestamp = endTimestamp + 2 * blockLength * step
  }

  # Read the start block.
  # Reinitiate file and skip header as well as the initial start-1 blocks
  seek(fid, headerBytes + blockBytes * start, origin = 'start')
  pos = 1 # position of the first element to complete in data

  # Skip any corrupt blocks, up to maxAllowedCorruptBlocks in number.
  is_corrupt = TRUE
  for (ii in 1 : min((end-start), maxAllowedCorruptBlocks+1)) {
    prevRaw = readDataBlock(fid, struc = struc)

    if (prevRaw$checksum_pass) {
      is_corrupt = FALSE
      break
    }
    QClog = rbind(QClog, data.frame(checksum_pass = FALSE,
                                    blockID_current = start, # the actual block ID wasn't read b/c the block is corrupt
                                    blockID_next = start+1,
                                    start = 0,
                                    end = 0,
                                    blockLengthSeconds = 0,
                                    frequency_blockheader = 0,
                                    frequency_observed = 0,
                                    imputed = FALSE))

    warning("Skipping corrupt start block #", start)
    start = start + 1
  }
  if (is_corrupt) {
    if (start == end) {
      stop("Error reading file. All requested blocks are corrupt.")
    }
    stop("Error reading file. The first ", maxAllowedCorruptBlocks+1, " blocks are corrupt.")
  }

  if (is.null(prevRaw)) {
    return(invisible(list(header = header, data = NULL)))
  }
  struc = prevRaw$struc
  startTimestamp = as.numeric(prevRaw$start)

  # allocate memory for results
  timeRes = seq(startTimestamp, endTimestamp, step)
  nr = length(timeRes) - 1
  timeRes = as.vector(timeRes[1:nr])
  # Keep the R connection for platform-independent filenames and cleanup. The
  # native loop requests at most 128 packets at a time, independent of file size.
  readBlocks = function() readBin(fid, raw(), n = blockBytes * 128)
  calendar = function(coded) {
    timestampDecoder(coded, 0, 0, list(0, 0L, 0), configtz)$start
  }
  progress = NULL
  if (progressBar) {
    pb = txtProgressBar(1, nr, style = 3)
    progress = function(pos) setTxtProgressBar(pb, pos)
  }
  result = processAxivityBlocks(readBlocks, calendar, progress, prevRaw, timeRes,
                                start, end, numDBlocks, frequency_tol,
                                maxAllowedCorruptBlocks, loadbattery, interpolationType)
  accelRes = result$accel
  temp = result$temp
  battery = result$battery
  light = result$light
  last = result$last
  QClog = rbind(QClog, result$QClog)
  # Release the references to the full native buffers before trimming them.
  # Otherwise result keeps those buffers alive while the output is assembled.
  rm(result)
  #===============================================================================
  # If the user asked for more data than the length of the recording,
  # there will be 0s at the end of the result lists; get rid of them.
  if (last < nrow(accelRes)) {
    cut = c(last+1:nrow(accelRes))
    accelRes = accelRes[-cut,]
    battery = battery[-cut]
    light = light[-cut]
    temp = temp[-cut]
    timeRes = timeRes[-cut]
  }
  #===============================================================================
  # Form outcome
  return(invisible(list(
    header = header,
    data = cbind.data.frame(time = timeRes, accelRes, temp,  battery, light, stringsAsFactors = TRUE),
    QClog = QClog
  )))
}
