findStartData = function(filename, quote, startindex, blockname = NULL) {
  # Function used to find start of time series in Actiwatch and Actical data
  if (!is.null(blockname)) {
    # Default approach when blockname is specified (used for Actical)
    # ! Assumption that time series start in first 3000 lines
    # ! Assumption count data are preceded by a block header with name blockname
    data_head = data.table::fread(input = filename,
                                header = FALSE, sep = ",",
                                nrows = 3000, data.table = FALSE, 
                                quote = quote, fill = TRUE)
    block_start = grep(pattern = blockname, x = data_head[, 1], ignore.case = TRUE)
    if (length(block_start) != 0) {
      block_head = data_head[(block_start + 1):(block_start + 20), 1]
      block_head = unlist(lapply(block_head, FUN = function(x) unlist(strsplit(x, ","))[1]))
      epochnumbers = suppressWarnings(as.numeric(block_head))
      block_start = block_start + which(!is.na(epochnumbers))[1]
    }
    if (length(block_start) != 0) return(block_start)
    startindex = block_start
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