readActiwatchCount = function(filename = file, desiredEpochSize = NULL,
                            timeformat = "%m/%d/%Y %H:%M:%S", tz = "", timeformatName = "timeformat") {
  # In GGIR set timeformatName to extEpochData_timeformat

  fileExtension = tolower(getExtension(filename))

  if (fileExtension == "csv") {
    #=========================================================
    # CSV
    #=========================================================
    # ! Assumptions that timeseries start before line 1000
    index = 1000
    while (index > 0) {
      quote = detectQuote(fn = filename, index = index)
      testraw = data.table::fread(input = filename,
                                  header = FALSE, sep = ",", skip = index,
                                  nrows = 2, data.table = FALSE, quote = quote)
      if (length(testraw) > 0) {
        if (nrow(testraw) == 2) {
          if (testraw$V1[2] == testraw$V1[1] + 1) {
            break
          }
        }
      }
      index = index - 100
    }
    # ! Assumption that first column are the epoch numbers
    delta = 1 - testraw$V1[1]
    index = index + delta
    startFound = FALSE
    while (startFound == FALSE) {
      Dtest = data.table::fread(input = filename, sep = ",", skip = index, quote = quote, nrows = 1)  
      if (Dtest$V1[1] == 1) {
        startFound = TRUE
      } else {
        # This happens when file is has an empty row between each measurement point is stored
        index = index - 1
        if (index < 1) stop("Could not find start of recording", call. = FALSE)
      }
    }
    D = data.table::fread(input = filename, sep = ",", skip = index, quote = quote)
    # ! Assumption that column names are present 2 lines prior to timeseries
    colnames = data.table::fread(input = filename,
                                 header = FALSE, sep = ",",
                                 skip = index - 2, nrows = 1, quote = quote)
    if (all(is.na(colnames))) {
      colnames = data.table::fread(input = filename,
                                   header = FALSE, sep = ",",
                                   skip = index - 4, nrows = 1, quote = quote)
    }
    colnames(D) = as.character(colnames)[1:ncol(D)]
    # ! Assumptions about columns names
    colnames(D) = gsub(pattern = "datum|date", replacement = "date", x = colnames(D), ignore.case = TRUE)
    colnames(D) = gsub(pattern = "tijd|time", replacement = "time", x = colnames(D), ignore.case = TRUE)
    colnames(D) = gsub(pattern = "activiteit|activity", replacement = "ZCY", x = colnames(D), ignore.case = TRUE)
    timestamp_POSIX = as.POSIXct(x = paste(D$date[1:4], D$time[1:4], sep = " "),
                                 format = timeformat,
                                 tz = tz)
    checkTimeFormat(timestamp_POSIX[1], 
                    rawValue = paste(D$date[1], D$time[1], sep = " "),
                    timeformat = timeformat,
                    timeformatName = timeformatName)
    epSizeShort = mean(diff(as.numeric(timestamp_POSIX)))
    
    timestamp_POSIX = timestamp_POSIX[1]
    D = D[, "ZCY"]
  } else if (fileExtension == "awd") {
    #=========================================================
    # AWD
    #=========================================================
    # ! Assumption that first data row equals the first row with 3 columns
    index = 0
    
    quote = detectQuote(fn = filename, index = 50)
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
    D = D[,1]
    colnames(D)[1] = "ZCY"
    header = data.table::fread(input = filename, header = FALSE, sep = ",", 
                               nrows =  7, quote = quote)
    # Get epoch size
    optionalEpochs = data.frame(code = c("1", "2", "4", "8", "20", "81", "C1", "C2"),
                                size = c(15, 30, 60, 120, 300, 2, 5, 10))
    epSizeShort = optionalEpochs$size[which(optionalEpochs$code == as.character(header[4]))]
    # Get starttime 
    timestampFormat = paste0(unlist(strsplit(timeformat, " "))[1], " %H:%M")
    timestamp_POSIX = as.POSIXct(x = paste(header[2], header[3], sep = " "),
                                 format = timestampFormat, tz = tz)
    checkTimeFormat(timestamp_POSIX, 
                    rawValue = header[2],
                    timeformat = timeformat,
                    timeformatName = timeformatName)

  }
  D = as.matrix(D, drop = FALSE)
  
  # If requested, aggregate data to lower resolution to match desired 
  # epoch size in argument windowsizes
  if (!is.null(desiredEpochSize)) {
    if (desiredEpochSize > epSizeShort) {
      step = desiredEpochSize %/% epSizeShort
      D = sumAggregate(D, step)
      epSizeShort = epSizeShort * step
    }
    checkEpochMatch(desiredEpochSize, epSizeShort)
  }
  if (quote == "") D$ZCY = as.numeric(D$ZCY)
  invisible(list(data = D, epochSize = epSizeShort,
                 startTime = timestamp_POSIX))
}