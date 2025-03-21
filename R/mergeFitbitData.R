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
      # double names is possible when recording is split across json files
      # in that case there may be multiple calories, steps and sleep files
      doubleNames = colnames(D)[colnames(D) %in% colnames(data)] 
      new_times = which(D$dateTime %in% data$dateTime == FALSE)
      double_times = which(D$dateTime %in% data$dateTime == TRUE)
      if (length(new_times) > 0) {
        data = merge(data, D[new_times, ], by = doubleNames, all = TRUE)
      }
      if (length(double_times) > 0) {
        by_names = colnames(D)[colnames(D) %in% c("dateTime", "seconds")]
        data2 = merge(data, D[double_times,], by = by_names, all.x = TRUE)
        xcol = grep(pattern = "[.]x", x = colnames(data2))
        ycol = grep(pattern = "[.]y", x = colnames(data2))
        if (length(xcol) > 0 && length(ycol) > 0) {
          # check whether new column has values that were missing before
          replace_times = which(is.na(data2[, xcol]) & !is.na(data2[, ycol]))
          if (length(replace_times) > 0) {
            # replace previously missing values by new values
            data2[replace_times, xcol] = data2[replace_times, ycol]
          }
        }
        colnames(data2)[xcol] = gsub(pattern = "[.]x", replacement = "", x = colnames(data2)[xcol])
        data = data2[, -ycol]
      }
    }
    cnt = cnt + 1
  }
  data = data[order(data$dateTime),]
  return(data)
}