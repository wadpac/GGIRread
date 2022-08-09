readGENEActiv = function(filename, start = 0, end = 0, progress_bar = FALSE, 
                         desiredtz = "", configtz = NULL) {
  
  # Extract information from the fileheader
  suppressWarnings({fh = readLines(filename, 69)})
  
  SN = gsub(pattern = "Device Unique Serial Code:", replacement = "",
            x = fh[grep(pattern = "Device Unique Serial Code", x = fh)[1]])
  firmware = gsub(pattern = "Device Firmware Version:", replacement = "",
                  x = fh[grep(pattern = "Device Firmware Version", x = fh)[1]])
  Lux = as.numeric(gsub(pattern = "Lux:", replacement = "",
                  x = fh[grep(pattern = "Lux", x = fh)[1]]))
  Volts = as.numeric(gsub(pattern = "Volts:", replacement = "",
             x = fh[grep(pattern = "Volts", x = fh)[1]]))
  starttime = gsub(pattern = "Start Time:", replacement = "",
                  x = fh[grep(pattern = "Start Time", x = fh)[1]])
  
  tzone = gsub(pattern = "Time Zone:", replacement = "",
               x = fh[grep(pattern = "Time Zone", x = fh)[1]])
  tzone = as.numeric(unlist(strsplit(tzone, "[:]| "))[2]) * 3600
  
  # Read acceleration, lux and temperature data
  rawdata = GENEActivReader(filename = filename,
                            start = start, end = end, 
                            progress_bar = progress_bar)
  
  rawdata$time = rawdata$time / 1000
  
  # Construct header object
  header = list(serial_number = SN,
                      firmware = firmware,
                      tzone = tzone,
                      ReadOK = rawdata$info$ReadOK,
                      SampleRate = rawdata$info$SampleRate,
                      ReadErrors = rawdata$info$ReadErrors,
                      numBlocksTotal = rawdata$info$numBlocksTotal,
                      StarTime = starttime)
  
  s0 = unlist(strsplit(starttime, ":"))
  if (length(s0) == 4) {
    starttime = paste0(s0[1], ":", s0[2], ":", s0[3], ".", s0[4])
  }
  
  # Establish starttime in the correct timezone
  if (is.null(configtz)) {
    starttime_posix = as.POSIXlt(x = starttime, tz = desiredtz, format = "%Y-%m-%d %H:%M:%OS", origin = "1970-01-01")
  } else {
    starttime_posix = as.POSIXlt(starttime, tz = configtz, format = "%Y-%m-%d %H:%M:%OS", origin = "1970-01-01")
    starttime_posix = as.POSIXlt(as.numeric(starttime_posix), tz = desiredtz, origin = "1970-01-01")
  }
  
  # Correct timestamps
  page_offset = (((start - 1) * 300) / rawdata$info$SampleRate)
  starttime_num = as.numeric(starttime_posix) + 5 + page_offset #tzone +
  rawdata$time = rawdata$time + abs(rawdata$time[1]) + starttime_num
  
  return(invisible(list(
    header = header,
    data.out = data.frame(time = rawdata$time, 
                      x = rawdata$x, y = rawdata$y, z = rawdata$z,
                      light = rawdata$lux * (Lux/Volts) / 9, # divide by 9 to match GENEAread output values
                      temperature = rawdata$temperature,
                      stringsAsFactors = TRUE)
  )))
}
