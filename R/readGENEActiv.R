readGENEActiv = function(filename, start = 0, end = 0,
                              progress_bar = FALSE) {
  
  rawdata = GENEActivReader(filename = filename,
                            start = start, end = end, 
                            progress_bar = progress_bar)
  
  # Extract additional information from the fileheader
  suppressWarnings({fh = readLines(filename, 69)})
  
  SN = gsub(pattern = "Device Unique Serial Code:", replacement = "",
            x = fh[grep(pattern = "Device Unique Serial Code", x = fh)[1]])
  firmware = gsub(pattern = "Device Firmware Version:", replacement = "",
                  x = fh[grep(pattern = "Device Firmware Version", x = fh)[1]])
  Lux = as.numeric(gsub(pattern = "Lux:", replacement = "",
                  x = fh[grep(pattern = "Lux", x = fh)[1]]))
  Volts = as.numeric(gsub(pattern = "Volts:", replacement = "",
             x = fh[grep(pattern = "Volts", x = fh)[1]]))
  
  tzone = gsub(pattern = "Time Zone:", replacement = "",
               x = fh[grep(pattern = "Time Zone", x = fh)[1]])
  tz_offset = as.numeric(unlist(strsplit(tzone, "[:]| "))[2]) * 3600 * 1000
  
  rawdata$time  = rawdata$time + tz_offset 
  
  header = list(serial_number = SN,
                      firmware = firmware,
                      tzone = tzone,
                      ReadOK = rawdata$info$ReadOK,
                      SampleRate = rawdata$info$SampleRate,
                      ReadErrors = rawdata$info$ReadErrors,
                      numBlocksTotal = rawdata$info$numBlocksTotal)

  return(invisible(list(
    header = header,
    data = data.frame(time = rawdata$time / 1000, 
                      x = rawdata$x, y = rawdata$y, z = rawdata$z,
                      light = rawdata$lux * (Lux/Volts) / 9, # divide by 9 to match GENEAread output values
                      temperature = rawdata$T,
                      stringsAsFactors = TRUE)
  )))
}
