readPHBCount = function(filename = NULL, timeformat = "%m/%d/%Y %H:%M:%S",
                        desiredtz = "", configtz = NULL,
                        timeformatName = "timeformat") {
  if (length(configtz) == 0) configtz = desiredtz
  deviceSN = NULL
  if (length(grep(pattern = "datalist", x = filename, ignore.case = TRUE)) > 0) {
    data = as.data.frame(readxl::read_excel(path = filename, 
                                             col_types = "text", skip = 8),
                          row.names = FALSE)
    header = as.data.frame(readxl::read_excel(path = filename, 
                                              col_types = "text", n_max = 8,
                                              .name_repair = "unique_quiet"),
                           row.names = FALSE)[, 1]
    SNlocation = grep(pattern = "deviceSN", x = header)
    if (length(SNlocation) > 0) {
      deviceSN = unlist(strsplit(header[grep(pattern = "deviceSN", x = header)], " "))
      deviceSN = deviceSN[length(deviceSN)]
   }
    colnames(data)[grep(pattern = "counts", x = colnames(data), ignore.case = TRUE)] = "counts"
    colnames(data)[grep(pattern = "offWrist", x = colnames(data), ignore.case = TRUE)] = "nonwear"
    data$counts = as.numeric(data$counts)
    data$nonwear = as.numeric(data$counts)
  } else {
    data = as.data.frame(readxl::read_excel(path = filename, col_types = "text", skip = 8), row.names = FALSE)
    colnames(data)[grep(pattern = "sleepWake", x = colnames(data), ignore.case = TRUE)] = "sleep"
    data$sleep = as.numeric(data$sleep)
  }
  colnames(data)[grep(pattern = "timeStamp", x = colnames(data))] = "timestamp"
  rawValue = data$timestamp[1]
  data$timestamp = as.POSIXct(data$timestamp, format = timeformat, tz = configtz,
                              origin = "1970-01-01")
  
  checkTimeFormat(data$timestamp[1],
                  rawValue = rawValue,
                  timeformat = timeformat,
                  timeformatName = timeformatName)
  # Establish starttime in the correct timezone
  if (configtz != desiredtz) {
    data$timestamp = as.POSIXct(x = as.numeric(data$timestamp), tz = desiredtz,
                                 origin = "1970-01-01")
  }
  invisible(list(data = data, deviceSN = deviceSN))
}