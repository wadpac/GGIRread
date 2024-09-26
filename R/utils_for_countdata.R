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

sumAggregate = function(mat, step) {
  # Aggregate matrix mat by taking the sum over step number of rows
  mat = rbind(rep(0, ncol(mat)), mat)
  cumsum2 = function(x) {
    x = cumsum(ifelse(is.na(x), 0, x)) + x*0
    return(x)
  }
  mat = apply(mat, 2, cumsum2)
  mat = mat[seq(1, nrow(mat), by = step), , drop = FALSE]
  mat = apply(mat, 2, diff)
  return(mat)
}