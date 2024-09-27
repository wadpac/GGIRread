# Collection of short function used in functions
# - readActigraphCount
# - readActiwatchCount
#-----------------------------------------------------------------------------------------
checkTimeFormat = function(timestamp_POSIX, rawValue = " ?? ", timeformat = " ?? ",
                           timeformatName = NULL) {
  # If timestamp_POSIX is NA gieve error message to inform user that something went wrong.
  if (is.na(timestamp_POSIX)) {
    stop(paste0("\nTime format in data ", rawValue, 
                " does not match with time format ", timeformat,
                " as specified by argument ", timeformatName,
                ", please correct.\n"), call. = FALSE)
  } else {
    year = as.numeric(format(timestamp_POSIX, format = "%Y"))
    if (year < 1980 || year > 2500) {
      # Assumption that after 2500 no new ActiGraph data will be collected!
      stop(paste0("\nTimestamp recognised as ", format(timestamp_POSIX), 
                  " with year identified as ", year,
                  ". This does not seem to be correct. Raw timestamp value is stored as ",
                  rawValue, ". please change specification of ",
                  "argument ", timeformatName, " (currently ",
                  timeformat, ") to ensure correct interpretation of timestamp.\n"),
           call. = FALSE)
    }
  }
}

checkEpochMatch = function(desiredEpochSize, epSizeShort) {
  # Check whether desired and derived epoch size match
  if (!is.null(desiredEpochSize) && epSizeShort != desiredEpochSize) {
    stop(paste0("\nThe short epoch size as specified by the user (",
                desiredEpochSize, " seconds) does NOT match the short",
                " epoch size we see in the data (", epSizeShort,
                " seconds). Please correct."), call. = FALSE)
  }
  return()
}

detectQuote = function(fn, index) {
  # data.table::fread has argument quote.
  # On some computers the quotes in the files are
  # not recognised, to catch this first try to check whether this is the case:
  quote = "\""
  Dtest = NULL
  try(expr = {Dtest = data.table::fread(input = fn,
                                        header = FALSE, sep = ",", skip = index,
                                        nrows = 20, quote = quote)}, silent = TRUE)
  if (length(Dtest) == 0) {
    quote = ""
  } else {
    if (nrow(Dtest) <= 1) {
      quote = "" 
    }
  }
  return(quote)
}

getExtension <- function(filename){ 
  # Extract file extension
  ex <- unlist(strsplit(basename(filename), split = "[.]"))
  if (length(ex) < 2) stop(paste0("Cannot recognise extension from '", filename, "' as filename, please check"), call. = FALSE)
  return(ex[-1])
}

matAggregate = function(mat, step) {
  # Aggregate matrix mat by taking over step number of rows
  # as sum unless column names is sleep or nonwear in that case 
  # we take the rounded mean.
  mat = rbind(rep(0, ncol(mat)), mat)
  cumsum2 = function(x) {
    x = cumsum(ifelse(is.na(x), 0, x)) + x*0
    return(x)
  }
  mat = apply(mat, 2, cumsum2)
  mat = mat[seq(1, nrow(mat), by = step), , drop = FALSE]
  mat = apply(mat, 2, diff)
  # Correct non incremental variables
  for (niv in c("sleep", "nonwear")) {
    if (niv %in% colnames(D)) D[, niv] = round(D[, niv] / step)
  }
  return(mat)
}

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