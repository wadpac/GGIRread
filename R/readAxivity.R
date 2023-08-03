readAxivity = function(filename, start = 0, end = 0, progressBar = FALSE, desiredtz = "",
                       configtz = c(), interpolationType = 1, loadbattery = FALSE,
                       header = NULL, frequency_tol = 0.1) {
  if (length(configtz) == 0) configtz = desiredtz
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

    # make sure timestamps are somewhat continous,
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
    block = readBin(fid, raw(), n=512)
    if (length(block) < 512) {
      return(NULL)
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
    # sampling rate in one of file format U8 at offset 24
    samplerate_dynrange = readBin(block[25], integer(), size = 1, signed = FALSE)

    checksum_pass = TRUE
    if (samplerate_dynrange != 0) { # Very old files that have zero at offset 24 don't have a checksum
      # Perform checksum
      checksum = sum(readBin(block, n = 256,
                             integer(),
                             size = 2,
                             signed = FALSE,
                             endian = "little"))
      checksum = checksum %% 65536 # equals 2^16 the checksum is calculated on a 16bit integer
      if (checksum != 0) {
        checksum_pass = FALSE
      }
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
      checksum_pass = checksum_pass,
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
    block = readBin(fid, raw(), n=1024)

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

    # Read the first data block without data
    datas = readDataBlock(fid, complete = FALSE)
    if (is.null(datas)) {
      stop("Error reading the first data block.")
    }
    if (frequency_header != datas$frequency) {
      warning("Inconsistent value of measurement frequency: there is ",
              frequency_header, " in header and ", datas$frequency, " in the first data block ")
    }

    start = as.POSIXct(datas$start, origin = "1970-01-01", tz = desiredtz)
    
    returnobject = list(
      uniqueSerialCode = uniqueSerialCode, frequency = frequency_header,
      start = start,
      device = "Axivity", firmwareVersion = version, blocks = numDBlocks,
      accrange = accrange, hardwareType = hardwareType
    )
    return(invisible(
      returnobject
    ))
  }


  ################################################################################################
  # Main function
  
  # Parse input arguments
  nargin = nargs()
  if (nargin < 1) {
    stop("At least file must be specified")
  }
  # Get file size in data blocks
  numDBlocks = round(file.info(filename)$size / 512) - 2
  pageLength = 300
  # Open file
  fid = file(filename,"rb")
  on.exit({
    close(fid)
  })
  #############################################################################
  # read header
  struc = list(0,0L,0)
  if (is.null(header)) {
    header = readHeader(fid, numDBlocks)
  }
  # preprocess start and stop
  origin = as.numeric(header$start)
  step = 1/header$frequency
  if (is.numeric(start)) {
    if (start < 0)
      start = 0
    start = origin + start * pageLength * step
  } else {
    start = as.numeric(as.POSIXct(start, tz = configtz))
  }
  if (is.numeric(end)) {
    end = end * pageLength
    if (end > numDBlocks * 150) {
      end = numDBlocks * 150
    }
    end = origin + end * step
  } else {
    end = as.numeric(as.POSIXct(end, tz = configtz))
  }
  # If data is not necessary then stop work
  if (end <= start) {
    return(invisible(list(header = header, data = NULL)))
  }
  #############################################################################
  # reinitiate file and start reading of data and search the beginning of required
  seek(fid,0)
  # skip header
  seek(fid, 1024, origin = 'current')
  # Create data for results
  timeRes = seq(start, end, step)
  nr = length(timeRes) - 1
  timeRes = as.vector(timeRes[1:nr])
  temp = vector(mode = "double", nr)
  battery = vector(mode = "double", nr)
  light = vector(mode = "double", nr)
  
  #############################################################################
  # Reading of data

  # Create progress bar if it is necessary
  if (progressBar) {
    pb = txtProgressBar(1, nr, style = 3)
  }
  pos = 1 # position of the first element to complete in data
  prevRaw = readDataBlock(fid, struc = struc) # Read the first block
  if (is.null(prevRaw)) {
    return(invisible(list(header = header, data = NULL)))
  }

  # a block has at most 120 samples (40 samples for AX6, 
  # 80 for unpacked AX3 or for AX6 only collecting accelerometer data, and 120 for packed AX3),
  # so allocate enough space for this number of samples, plus an extra ones needed for resampling.
  maxSamples = 120

  # Don't rely on the type of device to determine the dimentionality of the data
  # because AX6 can be configured to only collect accelerometer data.
  if (prevRaw$parameters$Naxes == 3) { # AX3, or AX6 configured to only collect accelerometer data
    accelRes = matrix(0, nrow = nr, ncol = 3, dimnames = list(NULL, c("x", "y", "z")))
    rawAccel = matrix(0, nrow = maxSamples + 1, ncol = 3)
  } else { # AX6 configured to collect gyroscope data
    accelRes = matrix(0, nrow = nr, ncol = 6, dimnames = list(NULL, c("gx", "gy", "gz", "x", "y", "z")))
    rawAccel = matrix(0, nrow = maxSamples + 1, ncol = 6)
  }
  rawTime = vector(mode = "numeric", maxSamples + 2)

  rawPos = 1
  i = 2
  samplingFrac = 0.97 # first assume that sampling rate is 97% of expected value or higher
  prevRaw_backup = prevRaw
  struc_backup = struc
  block1AfterSkip = FALSE
  skippedLast = FALSE
  blockDur = prevRaw$length / prevRaw$frequency

  QClog = NULL # Initialise log of data quality issues
  
  while (i <= numDBlocks + 1) {

    if (i == numDBlocks + 1) {
      # Process the last block of data if necessary
      if (prevRaw$start >= start & pos <= nr & exists("prevStart") & exists("prevLength")) {
        # Calculate pseudo time for the "next" block
        newTimes = (prevRaw$start - prevStart) / prevLength * prevRaw$length + prevRaw$start
        prevLength = prevRaw$length

        # fill vector rawTime and matrix rawAccel for resampling
        rawLast = prevLength + 1
        rawTime[2:(rawLast+1)] = seq(prevStart, newTimes, length.out = rawLast) # rawTime[rawLast+1] will be ignored by resampling alg
      }
      else {
        break
      }
    } else {
      time2Skip = start - prevRaw$start # once this gets negative we've passed the point
      if (skippedLast == FALSE) {
        Nblocks2Skip = floor((time2Skip/blockDur) * samplingFrac) 
      } else {
        Nblocks2Skip = 0
      }
      if (Nblocks2Skip <= 0) {
        # read block
        raw = readDataBlock(fid, struc = struc, parameters = prevRaw$parameters)
      } else {
        # skip series of blocks, but only do this once
        seek(fid, 512 * Nblocks2Skip, origin = 'current')
        prevRaw$start = prevRaw$start + (blockDur * Nblocks2Skip) + 1
        skippedLast = TRUE
        block1AfterSkip = TRUE
        i = i + Nblocks2Skip
        next
      }
      if (is.null(raw)) {
        break
      }
      # Save start and length of the previous block
      prevStart = prevRaw$start
      prevLength = prevRaw$length
      struc = raw$struc
      # Check are previous data block necessary

      if (raw$start < start) {
        # Ignore this block and go to the next
        prevRaw = raw
        i = i + 1
        block1AfterSkip = FALSE
        next
      }
      if (block1AfterSkip == TRUE) {
        # Oops start was missed, because sampling rate was lower than expected
        # Go back to beginning of file and use lower samplingFrac
        i = 2
        seek(fid, 0)
        seek(fid, 512 + 1024, origin = 'start') # skip header and one block of data
        struc = struc_backup
        prevRaw = prevRaw_backup
        skippedLast = FALSE
        block1AfterSkip = FALSE
        samplingFrac = samplingFrac - 0.1
        if (samplingFrac < 0.2) {
          skippedLast = TRUE #read file in old way block by block
          warning(paste0("GGIRread is having difficulty reading this .cwa file.",
                         " This could be an issue with the .cwa files. Please report",
                         " this issue to the GGIRread maintainers",
                         " via https://github.com/wadpac/GGIRread/issues and to ",
                         " Axivity Ltd."), call. = FALSE)
        }
        next
      }

      # fill vector rawTime and matrix rawAccel for resampling
      rawLast = prevLength + 1
      rawTime[2:(rawLast+1)] = seq(prevStart, raw$start, length.out = rawLast) # rawTime[rawLast+1] will be ignored by resampling alg
      # Following line is commented out and moved further down, because we only
      # want to use the data if it passes the integrity checks, otherwise we will impute the data.
      # rawAccel[2:rawLast,] = prevRaw$data 
    } 

    if (rawPos == 1) {
      rawAccel[1,] = (prevRaw$data[1,])
      rawTime[1] = prevStart - 0.00001
      rawPos = 2
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
    if ((i <= numDBlocks &
         raw$blockID != 0 &
         raw$blockID - prevRaw$blockID != 1) |
        prevRaw$checksum_pass == FALSE |
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
      QClog = rbind(QClog, data.frame(checksum_pass = prevRaw$checksum_pass,
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
      tmp = resample(rawAccel, rawTime, timeRes[pos:last], rawLast, type = interpolationType) #GGIRread:::
    } else {
      # Impute the data because integrity check did not pass
      if (last - pos >  prevRaw$frequency * 259200) { # 3600 * 24 * 5 = 259200
        # Error if time gap is very large to avoid filling up memory
        stop(paste0("\nreadAxivity encountered a time gap in the file of ",
                    round((last - pos) / (3600 * 24) / prevRaw$frequency, digits = 2), " days"))
      }
      
      tmp = matrix(0, last - pos + 1, prevRaw$parameters$Naxes)
      for (axi in 1:3) tmp[, axi] = imputedValues[axi]
    }
    
    # put result to specified position
    last = nrow(tmp) + pos - 1

    # Fill light, temp and battery
    if (last >= pos) {
      accelRes[pos:last,] = tmp
      light[pos:last] = prevRaw$light
      temp[pos:last] = prevRaw$temperature
      battery[pos:last] = prevRaw$battery
    }
    # Remove all rawdata except for the last
    rawTime[1] = rawTime[rawLast]
    rawAccel[1,] = rawAccel[rawLast,]
    rawPos = 2
    # Now current become previous
    prevRaw = raw
    pos = last + 1
    # Refresh progress bar if it is necessary
    if (progressBar) {
      setTxtProgressBar(pb, pos)
    }
    # Check do we need read any more data
    if (pos > nr) {
      break
    }
    i = i + 1
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
