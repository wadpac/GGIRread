readActiwatchCount = function(filename = NULL,
                            timeformat = "%m/%d/%Y %H:%M:%S",
                            desiredtz = "",
                            configtz = NULL,
                            timeformatName = "timeformat") {
  # In GGIR set timeformatName to extEpochData_timeformat

  if (length(configtz) == 0) configtz = desiredtz
  fileExtension = tolower(getExtension(filename))

  if (fileExtension == "csv") {
    #=========================================================
    # CSV
    #=========================================================
    # ! Assumptions that timeseries start before line 1000
    startindex = 1000
    quote = detectQuote(filename = filename, skip = startindex)
    index = findStartData(filename, quote, startindex)
    D = data.table::fread(input = filename, sep = ",", skip = index, quote = quote, data.table = FALSE)
    # ! Assumption that column names are present 2 lines prior to timeseries
    colnames = data.table::fread(input = filename, data.table = FALSE,
                                 header = FALSE, sep = ",",
                                 skip = index - 2, nrows = 1, quote = quote)
    if (all(is.na(colnames))) {
      colnames = data.table::fread(input = filename,
                                   header = FALSE, sep = ",",
                                   skip = index - 4, nrows = 1, quote = quote)
    }
    colnames = colnames[!is.na(colnames)]
    D = D[, which(!is.na(colnames))]
    colnames(D) = tolower(as.character(colnames))
    # ! Assumptions about columns names
    # browser()
    colnames(D)[grep(pattern = "datum|date", x = colnames(D))] = "date"
    colnames(D)[grep(pattern = "tijd|time", x = colnames(D))] = "time"
    colnames(D)[grep(pattern = "activiteit|activity", x = colnames(D))] = "counts"
    colnames(D)[grep(pattern = "slapen|sleep", x = colnames(D))] = "sleep"
    colnames(D)[grep(pattern = "niet-om|wear|worn", x = colnames(D))] = "nonwear"
    D = D[, grep(pattern = "time|date|counts|sleep|nonwear", x = colnames(D))]
    timestamp_POSIX = as.POSIXct(x = paste(D$date[1:4], D$time[1:4], sep = " "),
                                 format = timeformat,
                                 tz = configtz)
    checkTimeFormat(timestamp_POSIX[1], 
                    rawValue = paste(D$date[1], D$time[1], sep = " "),
                    timeformat = timeformat,
                    timeformatName = timeformatName)
    epSizeShort = mean(diff(as.numeric(timestamp_POSIX)))
    
    timestamp_POSIX = timestamp_POSIX[1]
    D = D[, -which(colnames(D) %in% c("date", "time"))]
  } else if (fileExtension == "awd") {
    #=========================================================
    # AWD
    #=========================================================
    # ! Assumption that first data row equals the first row with 3 columns
    index = 0
    
    quote = detectQuote(filename = filename, skip = 50)
    NC = 1
    while (NC >= 3) {
      testraw = data.table::fread(input = filename,
                                  header = FALSE, sep = ",", skip = index,
                                  nrows = 1, data.table = TRUE, quote = quote)
      NC = ncol(testraw)
      if (NC >= 3) {
        break()
      } else {
        index = index + 1
      }
    }
    D = data.table::fread(input = filename, header = FALSE, sep = ",",
                          skip = index, quote = quote)
    D = D[, 1:2]
    colnames(D)[1:2] = c("counts", "light")
    header = data.table::fread(input = filename, header = FALSE, sep = ",", 
                               nrows =  7, quote = quote)
    # Get epoch size
    optionalEpochs = data.frame(code = c("1", "2", "4", "8", "20", "81", "C1", "C2"),
                                size = c(15, 30, 60, 120, 300, 2, 5, 10))
    epSizeShort = optionalEpochs$size[which(optionalEpochs$code == as.character(header[4]))]
    # Get starttime 
    timestampFormat = paste0(unlist(strsplit(timeformat, " "))[1], " %H:%M")
    timestamp_POSIX = as.POSIXct(x = paste(header[2], header[3], sep = " "),
                                 format = timestampFormat, tz = configtz)
    checkTimeFormat(timestamp_POSIX, 
                    rawValue = header[2],
                    timeformat = timeformat,
                    timeformatName = timeformatName)

  }
  D = as.matrix(D, drop = FALSE)
  if (quote == "") D = apply(D, 2, as.numeric)
  
  # Establish starttime in the correct timezone
  if (configtz != desiredtz) {
    timestamp_POSIX = as.POSIXct(x = as.numeric(timestamp_POSIX), tz = desiredtz,
                                 origin = "1970-01-01")
  }
  invisible(list(data = D, epochSize = epSizeShort,
                 startTime = timestamp_POSIX))
}