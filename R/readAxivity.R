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

  readDataBlock = function(fid, complete = TRUE, struc = list(0,0L,0), parameters = NULL){
    # Read one block of data and return list with following elements
    #   frequency is frequency recorded in this block
    #   start is start time in numeric form. To create string representation
    #       it is necessary to use
    #           as.POSIXct(start, origin = "1970-01-01", tz=desiredtz)
    #   temperature is temperature for the block
    #   battery is battery charge for the block
    #   light is light sensor measurement for the block
    #   length is number of observations in the block
    #   data is matrix with three columns "x", "y", and "z"
    #   matrix data is presented if complete == TRUE only.
    #
    if (!is.null(parameters)) {
      accelScaleCode = parameters$accelScaleCode
      accelScale = parameters$accelScale
      Naxes = parameters$Naxes
      frequency_data = parameters$frequency_data
      format = parameters$format
    }
    block = readBin(fid, raw(), n=blockBytes)
    if (length(block) < blockBytes) {
      return(NULL)
    }

    # sampling rate in one of file format U8 at offset 24
    samplerate_dynrange = readBin(block[25], integer(), size = 1, signed = FALSE)

    if (samplerate_dynrange != 0) { # Very old files that have zero at offset 24 don't have a checksum
      checksum = sum(readBin(block, n = 256,
                             integer(),
                             size = 2,
                             signed = FALSE,
                             endian = "little"))
      checksum = checksum %% 65536 # 65536 = 2^16; the checksum is calculated as a 16-bit integer
      if (checksum != 0) {
        # Checksum doesn't match. This means some bits in this block got corrupted.
        # We don't know which, so we can't trust this block. We skip it, and impute it later.
        rawdata_list = list(
          struc = struc,
          parameters = parameters,
          checksum_pass = FALSE
        )
        return(invisible(rawdata_list))
      }
    }

    idstr = readChar(block, 2, useBytes = TRUE)
    if (idstr != "AX") {
      stop("Packet header is incorrect. First two characters must be AX.")
    }

    packetLength = readBin(block[3:4], integer(), size = 2, signed = FALSE, endian = "little")
    if (packetLength != 508) {
      stop("Packet length is incorrect, should always be 508.")
    }

    # offset 4: if the top bit set, this contains a 15-bit fraction of a second for the timestamp
    tsOffset = readBin(block[5:6], integer(), size = 2, signed = FALSE, endian = "little")

    # offset 10: sequence ID
    blockID = readBin(block[11:14], integer(), size = 4, endian = "little")

    # read data for timestamp u32 at offset 14
    timeStamp = readBin(block[15:18], integer(), size = 4, endian = "little") # the "signed" flag of readBin only works when reading 1 or 2 bytes

    # Get light u16 at offset 18
    offset18 = readBin(block[19:20], integer(), size = 2, signed = FALSE, endian = "little")
    light = bitwAnd(offset18, 0x03ffL)

    # Read and recalculate temperature, lower 10 bits of u16 at offset 20.
    # Formula for the temperature is specified at
    # https://github.com/digitalinteraction/openmovement/blob/545564d3bf45fc19914de1ad1523ed86538cfe5e/Docs/ax3/cwa.h#L102
    # Also see the following discussion:
    # https://github.com/digitalinteraction/openmovement/issues/11#issuecomment-1622278513
    temperature = bitwAnd(readBin(block[21:22], integer(), size = 2, signed = FALSE, endian = "little"), 0x03ffL) * 75.0 / 256.0 - 50;
    if (loadbattery == TRUE) {
      # Read and recalculate battery charge u8 in offset 23
      # https://github.com/digitalinteraction/openmovement/blob/master/Docs/ax3/ax3-auxiliary.md#battery-voltage
      # Battery is sampled as a 10-bit ADC value, but only the middle 8 bits are stored (the lowest bit is lost, and the highest bit is always 1).
      # So to restore the ADC value, double the packed value and add 512.
      # Then voltage = ADC_value * 6 / 1024
      battery = 3.0 * (readBin(block[24], integer(), size = 1, signed = FALSE) / 256.0 + 1.0);
    } else {
      battery = 0
    }

    # offset 25, per documentation:
    # "top nibble: number of axes, 3=Axyz, 6=Gxyz/Axyz, 9=Gxyz/Axyz/Mxyz;
    # bottom nibble: packing format" (2 means unpacked, 0 packed).
    offset25 = readBin(block[26], integer(), size = 1, signed = FALSE)
    packed = (bitwAnd(offset25,15) == 0)

    # offset 26 has a int16 (not uint16) value.
    # It's the "relative sample index from the start of the buffer where the whole-second timestamp is valid"
    offset26 = readBin(block[27:28], integer(), size = 2, endian = "little")

    # number of observations in block U16 at offset 28
    # blockLength is expected to be 40 for AX6, 80 or 120 for AX3.
    # Note that if AX6 is configured to only collect accelerometer data
    # this will look as if it is a AX3
    blockLength = readBin(block[29:30], integer(), size = 2, signed = FALSE, endian = "little")

    if (is.null(parameters)) {
      accelScaleCode = bitwShiftR(offset18, 13)
      accelScale = 1 / (2^(8 + accelScaleCode))
      # top nibble of offset25 is the number of axes
      Naxes = bitwShiftR(offset25, 4)
    }

    # auxiliary variables
    shift = 0
    fractional = 0

    if (samplerate_dynrange == 0) {
      # Very old files have zero at offset 24 and frequency at offset 26
      frequency_data = offset26
    } else {
      # value at offset 26 is index of measurement with whole number of seconds
      shift = offset26
      if (is.null(parameters)) {
        frequency_data = round( 3200 / bitwShiftL(1, 15 - bitwAnd(samplerate_dynrange, 15)))
        # If the top bit of tsOffset is set, then timestamp offset was artificially
        # modified for backwards-compatibility ... therefore undo this...
        if (bitwAnd(tsOffset, 0x8000L) != 0) {
          format = 1
        } else {
          format = 2
        }
      }
      if (format == 1) {
        # Need to undo backwards-compatible shim:
        # Take into account how many whole samples the fractional part
        # of timestamp accounts for:
        #   relativeOffset = fifoLength
        #        - (short)(((unsigned long)timeFractional * AccelFrequency()) >> 16);
        #   nearest whole sample
        #       whole-sec   | /fifo-pos@time
        #          |        |/
        #    [0][1][2][3][4][5][6][7][8][9]
        # use 15-bits as 16-bit fractional time
        fractional = bitwShiftL(bitwAnd(tsOffset, 0x7fffL), 1);

        # frequency is truncated to int in firmware
        shift = shift + bitwShiftR((fractional * frequency_data), 16);
      }
    }

    # Read data if necessary
    if (complete) {

      if (packed) {
        # Read 4 bytes for three measurements
        packedData = readBin(block[31:510], integer(), size = 4, n = blockLength, endian = "little")
        # Unpack data
        data = AxivityNumUnpack(packedData)
      } else {
        # Read unpacked data
        xyz = readBin(block[31:510], integer(), size = 2, n = blockLength * Naxes, endian = "little")
        data = matrix(xyz, ncol = Naxes, byrow = T)
      }

      # Set names and Normalize accelerations
      if (Naxes == 3) {
        colnames(data) = c("x", "y", "z")
        data[,c("x", "y", "z")] = data[,c("x", "y", "z")] * accelScale  #/ 256
      } else {
        gyroRangeCode = floor(offset18 / 1024) %% 8
        gyroRange = 8000 / (2^gyroRangeCode)
        colnames(data) = c("gx", "gy", "gz", "x", "y", "z")
        data[,c("gx", "gy", "gz")] = (data[,c("gx", "gy", "gz")] / 2^15) * gyroRange
        data[,c("x", "y", "z")] = data[,c("x", "y", "z")] * accelScale
      }
    }
    if (is.null(parameters)) {
      parameters = list(accelScaleCode = accelScaleCode,
                        accelScale = accelScale,
                        Naxes = Naxes,
                        frequency_data = frequency_data,
                        format = format)
    }

    if (blockID == 0) {
      # force timestampDecoder to extract full timestamp
      # this could for example happen when curious participant plugs
      # AX device into computer
      struc[[1]] = 0
    }
    tsDeco = timestampDecoder(timeStamp, fractional, -shift / frequency_data, struc, configtz)
    start = tsDeco$start
    struc = tsDeco$struc
    rawdata_list = list(
      frequency = frequency_data,
      start = start,
      temperature = temperature,
      battery = battery,
      light = light,
      length = blockLength,
      struc = struc,
      parameters = parameters,
      checksum_pass = TRUE,
      blockID = blockID
    )
    if (complete) {
      rawdata_list$data = data
    }

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
  temp = vector(mode = "double", nr)
  battery = vector(mode = "double", nr)
  light = vector(mode = "double", nr)

  # Create progress bar if it is necessary
  if (progressBar) {
    pb = txtProgressBar(1, nr, style = 3)
  }

  # Allocate enough space for the expected number of samples in a block, plus an extra ones needed for resampling.
  # Don't rely on the type of device to determine the dimensionality of the data
  # because AX6 can be configured to only collect accelerometer data.
  if (prevRaw$parameters$Naxes == 3) { # AX3, or AX6 configured to only collect accelerometer data
    accelRes = matrix(0, nrow = nr, ncol = 3, dimnames = list(NULL, c("x", "y", "z")))
    rawAccel = matrix(0, nrow = blockLength + 1, ncol = 3)
  } else { # AX6 configured to collect gyroscope data
    accelRes = matrix(0, nrow = nr, ncol = 6, dimnames = list(NULL, c("gx", "gy", "gz", "x", "y", "z")))
    rawAccel = matrix(0, nrow = blockLength + 1, ncol = 6)
  }
  rawTime = vector(mode = "numeric", blockLength + 2)
  rawPos = 1

  consecutiveCorrupt = 0

  # Read the data
  for (ii in (start+1):end) {
    if (ii == numDBlocks) {
      # Process the last block in the file, if necessary

      # Calculate pseudo time for the "next" block
      newTimes = (prevRaw$start - prevStart) / prevLength * prevRaw$length + prevRaw$start
      prevLength = prevRaw$length

      # fill vector rawTime and matrix rawAccel for resampling
      rawLast = prevLength + 1
      rawTime[2:(rawLast+1)] = seq(prevStart, newTimes, length.out = rawLast) # rawTime[rawLast+1] will be ignored by resampling alg
    }
    else { # read a new block
      raw = readDataBlock(fid, struc = struc, parameters = prevRaw$parameters)
      if (!raw$checksum_pass) {
        # If the checksum doesn't match, we can't trust any of this block's data,
        # so we have to completely skip the block.
        # Depending on the nature of the faulty block, the data for the time period it represented
        # will probably get imputed later, once we encounter a block with a valid checksum.
        QClog = rbind(QClog, data.frame(checksum_pass = FALSE,
                                        blockID_current = ii, # the actual block ID wasn't read b/c the block is corrupt
                                        blockID_next = ii + 1,
                                        start = 0,
                                        end = 0,
                                        blockLengthSeconds = 0,
                                        frequency_blockheader = 0,
                                        frequency_observed = 0,
                                        imputed = FALSE))
        warning("Skipping corrupt block #", ii)

        consecutiveCorrupt = consecutiveCorrupt + 1
        if (consecutiveCorrupt > maxAllowedCorruptBlocks) {
          stop("Error reading file. ", maxAllowedCorruptBlocks+1, " consecutive blocks are corrupt.")
        }

        next
      }

      consecutiveCorrupt = 0

      if (is.null(raw)) {
        # this shouldn't happen
        stop(paste0("\nreadAxivity encountered unexpected empty block at #", ii))
      }

      # Save start and length of the previous block
      prevStart = prevRaw$start
      prevLength = prevRaw$length
      struc = raw$struc

      # fill vector rawTime and matrix rawAccel for resampling
      rawLast = prevLength + 1
      rawTime[2:(rawLast+1)] = seq(prevStart, raw$start, length.out = rawLast) # rawTime[rawLast+1] will be ignored by resampling alg

      if (rawPos == 1) {
        rawAccel[1,] = (prevRaw$data[1,])
        rawTime[1] = prevStart - 0.00001
        rawPos = 2
      }
    }

    frequency_observed = rawLast / (rawTime[rawLast] - rawTime[1])
    #------------------------------------------------------------
    # Check block integrity:
    # The following code checks whether any of the following conditions are met:
    # - blockID is not zero and not consecutive from previous blockID
    # - checksum_pass is FALSE
    # - observed and expected sampling frequency differ by a fraction larger
    #  than frequency_tol
    # If yes, then we consider the block faulty
    # and impute the acceleration and if applicable gyroscope values
    # We log this event in output object QClog, which will allow the user to
    # decide on alternative imputation strategies.

    impute = FALSE
    doQClog = FALSE
    frequency_bias = abs(frequency_observed - prevRaw$frequency) / prevRaw$frequency
    if ((ii < numDBlocks &&
         raw$blockID != 0 &&
         raw$blockID - prevRaw$blockID != 1) ||
        frequency_bias > frequency_tol) {
      # Log and impute this event
      doQClog = TRUE
      impute = TRUE

      # Prepare imputation with last recorded value, from the last non-faulty block,
      # normalized to vector 1 g
      imputedValues = rawAccel[1, 1:3]
      VectorG = sqrt(sum(imputedValues^2))
      if (VectorG > 0.8 & VectorG < 1.2) {
        # only trust vector as proxy for orientation if it is between 0.8 and 1.2
        imputedValues = imputedValues / VectorG
      } else {
        imputedValues = c(0, 0, 1)
      }
    } else {
      # integrity check passes, so use data
      rawAccel[2:rawLast,] = prevRaw$data
      # If frequency bias is larger than 0.05 then still log it even though it is
      # not imputed
      if (frequency_bias > 0.05) {
        doQClog = TRUE
      }
    }
    if (doQClog == TRUE) {
      # Note: This is always a description of the previous block
      QClog = rbind(QClog, data.frame(checksum_pass = TRUE,
                                      blockID_current = prevRaw$blockID,
                                      blockID_next = raw$blockID,
                                      start = prevRaw$start,
                                      end = raw$start,
                                      blockLengthSeconds = raw$start - prevRaw$start,
                                      frequency_blockheader = prevRaw$frequency,
                                      frequency_observed = frequency_observed,
                                      imputed = impute))
    }

    ###########################################################################
    # resampling of measurements
    last = pos + 200;
    if (last > nr) last = nr
    if (rawTime[rawLast] > timeRes[last]) {
      # there has been a time jump
      # so, time jump needs to be adjusted for in last index
      timejump = rawTime[rawLast] - timeRes[last]
      positions2add = floor(timejump * prevRaw$frequency)
      last = last + positions2add
      if (last > nr) last = nr
    }

    if (impute == FALSE) {
      tmp = resample(rawAccel, rawTime, timeRes[pos:last], rawLast, type = interpolationType)
    } else {
      # Impute the data because integrity check did not pass
      if (last - pos >  prevRaw$frequency * 259200) { # 3600 * 24 * 5 = 259200
        # Error if time gap is very large to avoid filling up memory
        stop(paste0("\nreadAxivity encountered a time gap in the file of ",
                    round((last - pos) / (3600 * 24) / prevRaw$frequency, digits = 2), " days"))
      }

      # Figure out the number of points to impute, numImp.
      # numImp should be such that timeRes[numImp + pos + 1] is the last point in timeRes[] < rawTime[rawLast]
      numImp = length(which(timeRes[pos:last]<rawTime[rawLast]))
      tmp = matrix(0, numImp, prevRaw$parameters$Naxes)
      for (axi in 1:3) tmp[, axi] = imputedValues[axi]
    }

    # put result into specified position
    last = nrow(tmp) + pos - 1

    # Fill light, temp and battery
    if (last >= pos) {
      accelRes[pos:last,] = tmp
      light[pos:last] = prevRaw$light
      temp[pos:last] = prevRaw$temperature
      battery[pos:last] = prevRaw$battery
    }
    # Remove all rawdata except for the last
    rawTime[1] = timeRes[last]
    rawAccel[1,] = accelRes[last, ]
    rawPos = 2
    # Now current becomes previous
    prevRaw = raw
    pos = last + 1
    # Refresh progress bar if it is necessary
    if (progressBar) {
      setTxtProgressBar(pb, pos)
    }
  }
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
