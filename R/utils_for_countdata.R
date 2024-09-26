# Collection of short function used in functions readActigraphCount, readActiwatchCount
checkTimeFormat = function(timestamp_POSIX, rawValue = " ?? ", timeformat = " ?? ",
                           timeformatName = NULL) {
  if (is.na(timestamp_POSIX)) {
    stop(paste0("\nTime format in data ", rawValue, 
                " does not match with time format ", timeformat,
                " as specified by argument ", timeformatName,
                ", please correct.\n"), call. = FALSE)
  }
}

checkEpochMatch = function(desiredEpochSize, epSizeShort) {
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
  ex <- unlist(strsplit(basename(filename), split = "[.]"))
  if (length(ex) < 2) stop(paste0("Cannot recognise extension from '", filename, "' as filename, please check"), call. = FALSE)
  return(ex[-1])
}