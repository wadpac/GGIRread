readActicalCount = function(filename = NULL,
                            timeformat = "%m/%d/%Y %H:%M:%S",
                            desiredtz = "",
                            configtz = NULL,
                            timeformatName = "timeformat") {
  # In GGIR set timeformatName to extEpochData_timeformat
  if (length(configtz) == 0) configtz = desiredtz
  # ! Assumptions that timeseries start before line 1000
  startindex = 300
  quote = detectQuote(filename = filename, skip = startindex)
  startindex = findStartData(filename, quote, startindex)
  # -1 because Actical starts at epoch 0 while function looks for epoch 1
  startindex = startindex - 1 
  D = data.table::fread(input = filename, sep = ",", skip = startindex,
                        quote = quote, data.table = FALSE)
  # ! Assumption that column names are present 2 lines prior to timeseries
  dashedlineFound = FALSE
  dashedLineIndex = startindex
  while (dashedlineFound == FALSE) {
    linedata = data.table::fread(input = filename, data.table = FALSE,
                                 header = FALSE, sep = ",",
                                 skip = dashedLineIndex, nrows = 1, quote = quote)
    if (length(grep(pattern = "------", x = linedata[1])) == 1) {
      dashedlineFound = TRUE
    } else {
      dashedLineIndex = dashedLineIndex - 1
    }
  }
  colnames = data.table::fread(input = filename, data.table = FALSE,
                               header = FALSE, sep = ",",
                               skip = dashedLineIndex + 1,
                               nrows = (startindex - dashedLineIndex) - 2, quote = quote)
  collapse = function(x) {
    return(paste0(x, collapse = "_"))
  }
  colnames = tolower(as.character(apply(colnames, MARGIN = 2, FUN = collapse)))
  colnames(D) = colnames
  # ! Assumptions about columns names
  colnames(D)[grep(pattern = "time", x = colnames(D))] = "time"
  colnames(D)[grep(pattern = "date", x = colnames(D))] = "date"
  colnames(D)[grep(pattern = "activity_counts", x = colnames(D))] = "counts"
  colnames(D)[grep(pattern = "steps", x = colnames(D))] = "steps"
  D = D[, grep(pattern = "time|date|counts|steps", x = colnames(D))]
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