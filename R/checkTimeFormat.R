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