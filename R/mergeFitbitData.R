mergeFitbitData = function(filenames = NULL, desiredtz = "", configtz = NULL) {
  if (length(filenames) < 2) {
    stop("Provide at least two filenames")
  }
  cnt = 1
  
  # change order filenames such that
  # heart rate if present is at the end, because heart rate 
  # has irregular epoch length and needs to be interpolated
  filenames = filenames[c(grep(pattern = "sleep", x = basename(filenames), invert = FALSE),
                          grep(pattern = "calories", x = basename(filenames), invert = FALSE),
                          grep(pattern = "step", x = basename(filenames), invert = FALSE),
                          grep(pattern = "heart", x = basename(filenames), invert = FALSE))]
  
  while (cnt <= length(filenames)) {
    D = readFitbit(filename = filenames[cnt], desiredtz = desiredtz, configtz = configtz)
    if (cnt == 1) {
      data = D
    } else {
      if (length(grep("heart", x = colnames(D))) > 0) {
        # interpolate heart rate to 30 seconds to ease merging with
        # other data types
        t0 = round(as.numeric(D$dateTime[1]) / 30) * 30
        t1 = round(as.numeric(D$dateTime[nrow(D)]) / 30) * 30
        newTime = seq(t0, t1, by = 30)
        newD = as.data.frame(resample(raw = as.matrix(D$heart_rate), 
                                      rawTime = as.numeric(D$dateTime),
                                      time = newTime,
                                      stop = nrow(D)))
        colnames(newD)[1] = "heart_rate"
        if (length(newTime) > nrow(newD)) newTime = newTime[1:nrow(newD)]
        newD$dateTime = as.POSIXct(newTime, tz = desiredtz)
        rm(D)
        D = newD[, c("dateTime", "heart_rate")]
      }
      # double names is possible when recording is split across json files
      # in that case there may be multiple calories, steps and sleep files
      doubleNames = colnames(D)[colnames(D) %in% colnames(data)] 
      new_times = which(D$dateTime %in% data$dateTime == FALSE)
      double_times = which(D$dateTime %in% data$dateTime == TRUE)
      if (length(new_times) > 0) {
        if (all(colnames(data) %in% colnames(D))) {
          data = rbind(data, D[new_times, ])
          data = data[order(data$dateTime),]
        } else {
          data = merge(data, D[new_times, ], by = doubleNames, all = TRUE)
        }
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
          colnames(data2)[xcol] = gsub(pattern = "[.]x", replacement = "", x = colnames(data2)[xcol])
          data = data2[, -ycol]
        } else {
          data = data2
        }
      }
    }
    cnt = cnt + 1
  }
  data = data[order(data$dateTime),]
  # fill gaps with NA values
  timeRange = range(data$dateTime)
  epochSize = min(diff(as.numeric(data$dateTime[1:pmin(10, nrow(data))])))
  timeFrame = data.frame(dateTime = seq( timeRange[1], timeRange[2], by = epochSize))
  data = merge(data, timeFrame, by = c("dateTime"), all.y = TRUE)
  return(data)
}