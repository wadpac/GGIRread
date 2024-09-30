mergePHBdata = function(filenames = NULL,
                             timeformat = "%m/%d/%Y %H:%M:%S",
                             desiredtz = "", configtz = NULL,
                             timeformatName = "timeformat") {
  # merges Philips Health Band xlsx files per participant
  # as there can be multiple files per participant.
  if (length(filenames) != 2) {
    stop("Provide two filenames")
  }
  
  # Identify both file
  file1 = grep(pattern = "datalist", x = filenames, ignore.case = TRUE)
  file2 = grep(pattern = "sleep_wake", x = filenames, ignore.case = TRUE)

  # Datalist file (with all variables except sleep/wake scores)
  deviceSN = NULL
  if (length(file1) > 0) {
    data1 = readPHBCount(filename = filenames[file1], timeformat = timeformat,
                         desiredtz = desiredtz, configtz = configtz,
                         timeformatName = timeformatName)
    deviceSN = data1$deviceSN
  }
  # Sleep wake scores file
  if (length(file2) > 0) {
    data2 = readPHBCount(filename = filenames[file2], timeformat = timeformat,
                         desiredtz = desiredtz, configtz = configtz,
                         timeformatName = timeformatName)
  }
  if (length(file1) > 0 && length(file2) > 0) {
    data2$data = data2$data[, which(colnames(data2$data) != "sleepEventMarker")]
    data = merge(data1$data, data2$data, by = "timestamp")
  } else {
    if (length(file1) > 0) {
      data = data1$data
    } else {
      data = data2$data
    }
  }
  invisible(list(data = data, deviceSN = deviceSN))
}