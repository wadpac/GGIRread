findStartData = function(filename, quote, startindex) {
  # Function used to find start of time series in Actiwatch and Actical data
  # ! Assumptions that timeseries start before line 1000
  while (startindex > 0) {
    testraw = data.table::fread(input = filename,
                                header = FALSE, sep = ",", skip = startindex,
                                nrows = 2, data.table = FALSE, quote = quote)
    if (length(testraw) > 0) {
      if (nrow(testraw) == 2) {
        if (testraw$V1[2] == testraw$V1[1] + 1) {
          break
        }
      }
    }
    startindex = startindex - 100
  }
  # ! Assumption that first column are the epoch numbers
  delta = 1 - testraw$V1[1]
  startindex = startindex + delta
  startFound = FALSE
  while (startFound == FALSE) {
    Dtest = data.table::fread(input = filename, sep = ",", skip = startindex, quote = quote, nrows = 1)
    if (Dtest$V1[1] == 1) {
      startFound = TRUE
    } else {
      # This happens when file is has an empty row between each measurement point is stored
      startindex = startindex - 1
      if (startindex < 1) stop("Could not find start of recording", call. = FALSE)
    }
  }
  return(startindex)
}