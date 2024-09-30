mergeFitbitData = function(filenames = NULL, desiredtz = "", configtz = NULL) {
  if (length(filenames) < 2) {
    stop("Provide at least two filenames")
  }
  cnt = 1
  while (cnt <= length(filenames)) {
    D = readFitbit(filename = filenames[cnt], desiredtz = desiredtz, configtz = configtz)
    if (cnt == 1) {
      data = D
    } else {
      if (length(intersect(x = data$dateTime, D$dateTime)) == 0) {
        warning(paste0("Time series do not intersect for files ",
                       basename(filenames[cnt]), " and ", basename(filenames[cnt - 1])),
                call. = FALSE)
      }
      
      data = merge(data, D, by = "dateTime", all = TRUE)
    }
    cnt = cnt + 1
  }
  return(data)
}