GENEActivBinReader = function(filename, start = 0, end = 0,
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
  tzone = gsub(pattern = "Time Zone:", replacement = "",
               x = fh[grep(pattern = "Time Zone", x = fh)[1]])
  tz_offset = as.numeric(unlist(strsplit(tzone, "[:]| "))[2]) * 3600 * 1000
  
  rawdata$time  = rawdata$time + tz_offset 
  
  header = data.frame(serial_number = SN,
                      firmware = firmware,
                      tzone = tzone,
                      ReadOK = rawdata$info$ReadOK,
                      SampleRate = rawdata$info$SampleRate,
                      ReadErrors = rawdata$info$ReadErrors,
                      numBlocksTotal = rawdata$info$numBlocksTotal)
  
  return(invisible(list(
    header = header,
    data = data.frame(time = rawdata$time, 
                      x = rawdata$x, y = rawdata$y, z = rawdata$z,
                      temperature = rawdata$T,
                      lux = rawdata$lux, stringsAsFactors = TRUE)
  )))
}
