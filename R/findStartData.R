findStartData = function(filename, quote, startindex, blockname = NULL) {
  # Function used to find start of time series in Actiwatch and Actical data
  # ! Assumption that time series start in first 3000 lines
  # ! Assumption count data are preceded by a block header with name blockname
  if (!is.null(blockname)) {
    quote = detectQuote(filename = filename, skip = startindex)
    testraw = data.table::fread(input = filename,
                                header = FALSE, sep = ",",
                                nrows = 3000, data.table = FALSE, 
                                quote = quote, fill = TRUE)
    
    startindex_temp = grep(pattern = blockname, x = testraw[,1], ignore.case = TRUE)
    if (length(startindex_temp) != 0) {
      temp = testraw[(startindex_temp + 1):(startindex_temp + 20),1]
      temp = unlist(lapply(temp, FUN = function(x) unlist(strsplit(x, ","))[1]))
      epochnumbers = suppressWarnings(as.numeric(temp))
      startindex_temp = startindex_temp + which(!is.na(epochnumbers))[1]
    }
    if (length(startindex_temp) != 0) return(startindex_temp)
    startindex = startindex_temp
  }
  # Original approach:
  # ! Assumption that timeseries start before line 1000
  # ! Assumption that epoch column start with 1
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
  if (startindex < 32) {
    startindex = 32
  } else {
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
  }
  return(startindex)
}