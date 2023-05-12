readAxivity = function(filename, start = 0, end = 0, progressBar = FALSE, desiredtz = "",
                       configtz = c(), interpolationType = 1, loadbattery = FALSE) {
  if (length(configtz) == 0) configtz = desiredtz
  # Credits: The original version of the code in this function was 
  # contributed by Dr. Evgeny Mirkes (Leicester University, UK)
  #========================================================================
  # filename is namer of cwa file to read
  # start can be timestamp "year-month-day hr:min:sec" or non-negative integer
  #       which is page number. Page size is 300 of measurements with specified
  #       frequency.
  # end can be timestamp "year-month-day hr:min:sec" or non-negative integer
  #       which is page number. End must be not less than start. If end is
  #       less or equal to start then there is no data read. Page size is 300 of
  #       measurements with specified frequency.
  # progressBar is trigger to switch on/off the text progress bar. If progressBar
  #       is TRUE then the function displays the progress bar but it works
  #       slightly slower
  # desiredtz is desired time zone
  # Returned structure contains all data from start inclusive till end exclusive.
  # If start == end then data section of final structure is empty.
  # Page size is 300 observations per page
  #
  # Structure of output is list with following elements
  #   header is list of header information
  #       uniqueSerialCode is unque serial code of used device
  #       frequency is measurement frequency. All data will be resampled
  #           for this frequency.
  #       start is timestamp in numeric form. To get text representation
  #           it is enough to use
  #               as.POSIXct(start, origin = "1970-01-01", tz=desiredtz)
  #       device is "Axivity"
  #       firmwareVersion is version of firmware
  #       blocks is number of datablocks with 80 or 120 raw observations in each.
  #           Unfortunately frequency of measurement is varied in this device.
  #   data is data.frame with following columns
  #       time is timestamp in numeric form. To get text representation
  #           it is enough to use
  #               as.POSIXct(start, origin = "1970-01-01", tz=desiredtz)
  #       x, y, z are three accelerations
  #       temperature is temperature for the block
  #       battery is battery charge for the block
  #       light is light sensor measurement for the block
  #
  # Background info on data format:
  # https://github.com/digitalinteraction/openmovement/blob/master/Docs/ax3/ax3-technical.md
  #############################################################################
  
  # Internal functions
  timestampDecoder = function(coded, fraction, shift, struc, configtz) {
    year = struc[[1]]
    if (year == 0) {
      # Extract parts of date
      year = bitwAnd(bitwShiftR(coded, 26), 0x3fL) + 2000
      month = bitwAnd(bitwShiftR(coded, 22), 0x0fL)
      day = bitwAnd(bitwShiftR(coded, 17), 0x1f)
      hours = bitwAnd(bitwShiftR(coded, 12), 0x1fL)
      mins = bitwAnd(bitwShiftR(coded, 6), 0x3fL)
      secs = bitwAnd(coded, 0x3fL)
      # Form string representation of date and convert it to number
      year_raw = as.POSIXct(paste0(year, "-", month, "-", day, " ",
                                              hours, ":", mins, ":", secs),
                            tz = configtz)
      year = as.numeric(year_raw)
    } else {
      secs = bitwAnd(coded, 0x3fL)
      oldSecs = struc[[2]]
      if (secs < oldSecs) oldSecs = oldSecs - 60
      year = year + (secs - oldSecs)
    }
    struc <- list(year,secs)
    # Add fractional part and shift
    start = year + fraction / 65536 + shift
    invisible(list(start = start, struc = struc))
  }
  
  unsigned8 = function(x) {
    # Auxiliary function for normalisation of unsigned integers
    if (x < 0)
      return(x + 256) #2^8
    else
      return(x)
  }
  
  unsigned16 = function(x) {
    # Auxiliary function for normalisation of unsigned integers
    if (x < 0)
      return(x + 65536) #2^16
    else
      return(x)
  }
  readDataBlock = function(fid, complete = TRUE, struc = list(0,0L), header_accrange = NULL, parameters = NULL){
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
      blockLength = parameters$blockLength
      frequency_data = parameters$frequency_data
      format = parameters$format
    }
    # Check the block header
    # idstr = readChar(fid,2,useBytes = TRUE)
    # seek(fid, 2, origin = 'current') # idstr and assume it is AX
    # Read the data block. Extract several data fields
    # offset 4 contains u16 with timestamp offset
    seek(fid, 4, origin = 'current') # skip packetlength
    tsOffset = readBin(fid, integer(), size = 2)
    # read data for timestamp u32 in offset 14
    seek(fid, 8, origin = 'current') # skip sessionId and sequenceID
    timeStamp = readBin(fid, integer(), size = 4)
    # Get light u16 in offset 18
    offset18 = unsigned16(readBin(fid, integer(), size = 2))
    light = bitwAnd(offset18, 0x03ffL)
    # Read and recalculate temperature u16 in offset 20
    temperature = (150.0 * readBin(fid, integer(), size = 2) - 20500.0) / 1000.0;
    if (loadbattery == TRUE) {
      # Read and recalculate battery charge u8 in offset 23
      seek(fid, 1, origin = 'current') # skip events
      battery = 3.0 * (unsigned8(readBin(fid, integer(), size = 1)) / 512.0 + 1.0);
    } else {
      seek(fid, 2, origin = 'current') # skip events
      battery = 0
    }
    # sampling rate in one of file format U8 in offset 24
    samplerate_dynrange = readBin(fid, integer(), size = 1)
    # format of data in block u8  in offset 25
    # temp = readBin(fid, integer(), size = 1)
    temp_raw = readBin(fid, raw(), size = 1)
    temp = as.integer(temp_raw)
    packed = bitwAnd(temp,15) == 0
    # can be measurement with whole seconds or sample rate u16 in offset 26
    temp = readBin(fid, integer(), size = 2) # timestampOffset
    if (is.null(parameters)) {
      # number of observations in block U16 in offset 28
      blockLength = readBin(fid, integer(), size = 2) # blockLength is expected to be 40 for AX6, 80 or 120 for AX3
      accelScaleCode = bitwShiftR(offset18, 13)
      accelScale = 1 / (2^(8 + accelScaleCode)) # abs removed
      Naxes = as.integer(substr(temp_raw,1,1))
    } else {
      seek(fid, 2, origin = 'current') # skip events
    }
    # auxiliary variables
    shift = 0
    fractional = 0
    # Consider two possible formats.
    # Very old file have zero in offset 24 and frequency in offset 26
    if (samplerate_dynrange != 0) {
      # value in offset 26 is index of measurement with whole number of seconds
      shift = temp
      # If tsOffset is not null then timestamp offset was artificially
      # modified for backwards-compatibility ... therefore undo this...
      if (is.null(parameters)) {
        if (bitwAnd(tsOffset, 0x8000L) != 0) {
          format = 1
          frequency_data = round( 3200 / bitwShiftL(1, 15 - bitwAnd(samplerate_dynrange, 15)))
          accrange = bitwShiftR(16,(bitwShiftR(abs(samplerate_dynrange),6)))
        } else { # & class(frequency_data) ==  "function") {
          format = 2
          frequency_data = round( 3200 / bitwShiftL(1, 15 - bitwAnd(samplerate_dynrange, 15)))
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
    } else {
      #Very old format, where offset 26 contains frequency
      frequency_data = temp
    }
    # Read data if necessary
    if (complete) {
      
      if (packed) { #32 bit
        # Read 4 byte for three measurements
        packedData = readBin(fid, integer(), size = 4, n = blockLength)
        # Unpack data
        data = AxivityNumUnpack(packedData) #GGIRread:::
        # data2 = numUnpack2(packedData)
        # Calculate number of bytes to skip
        temp = 482 -  4 * (Naxes/3) * blockLength
      } else {
        # Read unpacked data
        xyz = readBin(fid, integer(), size = 2, n = blockLength * Naxes)
        data = matrix(xyz, ncol = Naxes, byrow = T)
        # Calculate number of bytes to skip
        temp = 482 - (2 * Naxes * blockLength)
      }
      # Skip the rest of block
      seek(fid, temp, origin = 'current')
      
      # Set names and Normalize accelerations
      if (is.na(header_accrange == TRUE)) {
        header_accrange = 8
      }
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
    } else {
      seek(fid, 482, origin = 'current')
    }
    if (is.null(parameters)) {
      parameters = list(accelScaleCode = accelScaleCode,
                        accelScale = accelScale,
                        Naxes = Naxes,
                        blockLength = blockLength,
                        frequency_data = frequency_data,
                        format = format)
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
      parameters = parameters
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
    # Read block header and check correctness of name
    idstr = readChar(fid, 2, useBytes = TRUE) #offset 0 1
    if (idstr == "MD") {
      # It is correct header block read information from it
      readChar(fid, 2, useBytes = TRUE) #offset 2 3
      # hardware type: AX6 or AX3
      hwType = readBin(fid, raw(), size = 1) #offset 4
      if (hwType == "64") {
        hardwareType = "AX6"
      } else {
        hardwareType = "AX3"
      }
      # session id and device id
      lowerDeviceId = readBin(fid, integer(), size = 2, signed = FALSE) #offset 5 6
      sessionID = readBin(fid, integer(), size = 4) #offset 7 8 9 10
      upperDeviceId = readBin(fid, integer(), size = 2, signed = FALSE) #offset 11 12
      if (upperDeviceId >= 65535) upperDeviceId = 0
      uniqueSerialCode = upperDeviceId * 65536 + lowerDeviceId
      seek(fid, 23, origin = 'current') #offset 13..35
      # sample rate and dynamic range accelerometer
      samplerate_dynrange = readBin(fid, integer(), size = 1) #offset 36
      frequency_header = round( 3200 / bitwShiftL(1, 15 - bitwAnd(samplerate_dynrange, 15)))
      if (samplerate_dynrange < 0) samplerate_dynrange = samplerate_dynrange + 256
      accrange = bitwShiftR(16, (bitwShiftR(abs(samplerate_dynrange), 6)))
      seek(fid, 4, origin = 'current') #offset 37..40
      version = readBin(fid, integer(), size = 1) #offset 41
      # Skip 982 bytes and go to the first data block
      seek(fid, 982, origin = 'current') #offset 42..1024
      # Read the first data block without data
      datas = readDataBlock(fid, complete = FALSE)
      if (is.null(datas)) {
        stop("Error in the first data block reading")
      }
      if (frequency_header != datas$frequency) {
        warning("Inconsistent value of measurement frequency: there is ",
                frequency_header, " in header and ", datas$frequency, " in the first data block ")
      }
    } else {
      return(invisible(NULL))
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
  struc = list(0,0L)
  header = readHeader(fid, numDBlocks)
  # preprocess start and stop
  origin = as.numeric(header$start)
  step = 1/header$frequency
  if (is.numeric(start)) {
    if (start < 0)
      start = 0
    start = origin + start * pageLength * step
  }
  if (is.numeric(end)) {
    end = end * pageLength
    if (end > numDBlocks * 150) {
      end = numDBlocks * 150
    }
    end = origin + end * step
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
  if (header$hardwareType == "AX3") {
    accelRes = matrix(0, nrow = nr, ncol = 3, dimnames = list(NULL, c("x", "y", "z")))
  } else if (header$hardwareType == "AX6") {
    accelRes = matrix(0, nrow = nr, ncol = 6, dimnames = list(NULL, c("gx", "gy", "gz", "x", "y", "z")))
  }
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
  prevRaw = readDataBlock(fid, struc = struc, header_accrange = header$accrange) # Read the first block
  if (is.null(prevRaw)) {
    return(invisible(list(header = header, data = NULL)))
  }
  Npages = (end - start) + 1
  rawTime = vector(mode = "numeric", 300 * Npages)
  if (header$hardwareType == "AX3") {
    rawAccel = matrix(0, nrow = 300 * Npages, ncol = 3)
  } else {
    rawAccel = matrix(0, nrow = 300 * Npages, ncol = 6)
  }
  rawPos = 1
  i = 2
  while (i <= numDBlocks) {
    time2Skip = start - prevRaw$start
    blockDur = prevRaw$length / prevRaw$frequency
    Nblocks2Skip = floor(time2Skip/blockDur) - 1
    
    if (i >= Nblocks2Skip) { # start of recording
      raw = readDataBlock(fid, header_accrange = header$accrange, struc = struc,
                          parameters = prevRaw$parameters)
    } else {
      # skip blocks
      seek(fid, 512 * Nblocks2Skip, origin = 'current')
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
      prevRaw = raw
      i = i + 1
      next
    }
    # Create array of times
    time = seq(prevStart, raw$start, length.out = prevLength + 1)
    
    # fill vector rawTime and matrix rawAccel for resampling
    if (rawPos == 1) {
      rawAccel[1,] = (prevRaw$data[1,])
      rawTime[1] = prevStart - 0.00001
      rawPos = 2
    }
    # Define number of rows in prevRaw$data
    rawLast = prevLength + rawPos - 1
    rawTime[rawPos:rawLast] = time[1:prevLength]
    rawAccel[rawPos:rawLast,] = as.matrix(prevRaw$data)
    lastTime = time[prevLength]
    
    ###########################################################################
    # resampling of measurements
    last = pos + 200;
    if (pos + 200 > nr) last = nr
    tmp = resample(rawAccel, rawTime, timeRes[pos:last], rawLast, type = interpolationType) #GGIRread:::
    # put result to specified position
    last = nrow(tmp) + pos - 1
    if (last >= pos) {
      accelRes[pos:last,] = tmp
    }
    
    # Remove all rawdata exclude the last
    rawTime[1] = rawTime[rawLast]
    rawAccel[1,] = rawAccel[rawLast,]
    rawPos = 2
    # Fill light, temp and battery
    if (last >= pos) {
      light[pos:last] = prevRaw$light
      temp[pos:last] = prevRaw$temperature
      battery[pos:last] = prevRaw$battery
    }
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
  #############################################################################
  # Process the last block of data if necessary
  if (pos <= nr & exists("prevStart") & exists("prevLength")) {
    # Calculate pseudo time for the "next" block
    newTimes = (prevRaw$start - prevStart) / prevLength * prevRaw$length + prevRaw$start
    prevLength = prevRaw$length
    # Create array of times
    time = seq(prevStart, newTimes, length.out = prevLength + 1) #Row eddited by EM 18/12/2017. Correction of the final time.
    # Fragment below was changed by EM 24.04.2017 to unify resampling process.
    # fill vector rawTime and matrix rawAccel for resampling
    if (rawPos == 1) {
      rawAccel[1,] = (prevRaw$data[1,])
      rawTime[1] = prevStart - 0.00001
      rawPos = 2
    }
    # Define number of rows in prevRaw$data
    rawLast = prevLength + rawPos - 1
    rawTime[rawPos:rawLast] = time[1:prevLength]
    rawAccel[rawPos:rawLast,] = as.matrix(prevRaw$data)
    lastTime = time[prevLength]
    
    ###########################################################################
    # resampling of measurements
    last = pos + 200;
    if (pos + 200 > nr) last = nr
    tmp = resample(rawAccel, rawTime, timeRes[pos:last], rawLast, type = interpolationType)
    # put result to specified position
    last = nrow(tmp) + pos - 1
    if (last >= pos) {
      accelRes[pos:last,] = tmp
      # Fill light, temp and battery
      light[pos:last] = prevRaw$light
      temp[pos:last] = prevRaw$temperature
      battery[pos:last] = prevRaw$battery
    }
  }
  #===============================================================================
  # Do not export sections of the data with zeros in all channels, because they were not actual recordings
  # zeros are introduced when the user asks for more data than then length of the recording
  emptydata = which(rowSums(accelRes) == 0 & temp == 0 & battery == 0 & light == 0)
  if (length(emptydata) > 0) {
    startends = which(diff(emptydata) != 1)
    if (length(startends) > 0) {
      lastmeasurement = max(startends)
    } else {
      lastmeasurement = emptydata[1]
    }
    if (length(lastmeasurement) > 0) {
      cut = c(lastmeasurement:nrow(accelRes))
      accelRes = accelRes[-cut,]
      battery = battery[-cut]
      light = light[-cut]
      temp = temp[-cut]
      timeRes = timeRes[-cut]
    }
  }
  #===============================================================================
  # Form outcome
  return(invisible(list(
    header = header,
    data = as.data.frame(cbind(time = timeRes, accelRes, temp,  battery, light), stringsAsFactors = TRUE)
  )))
}
