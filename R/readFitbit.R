readFitbit = function(filename = NULL, desiredtz = "",
                      configtz = NULL) {
  # Assumptions made:
  # - sleep is sampled at 30 second resolution
  # - steps are sampled at 60 second resolution
  # - timestamp format is always the same per data type
  
  # Declare local functions
  handleTimeGaps = function(df, epochSize) {
    timeRange = range(df$dateTime)
    startTime = timeRange[1]
    endTime = timeRange[2]
    timeFrame = data.frame(dateTime = seq(startTime, endTime, by = epochSize))
    df = merge(df, timeFrame, by = c("dateTime"), all.y = TRUE)
    when_na = which(is.na(df$value))
    if (length(when_na) > 0) {
      df$value[when_na] = df$value[when_na - 1]
    }
    return(df)
  }
  
  #-------------------------------------------------
  # Main code
  if (length(configtz) == 0) configtz = desiredtz
  D = jsonlite::read_json(path = filename,
                simplifyVector = FALSE,
                flatten = FALSE)
  
  # extract dataType as json structure differs between types
  dataType = tolower(unlist(strsplit(basename(filename), "-"))[1])
  
  if (dataType == "sleep") {
    epochSize = 30
    # Put all data in data.frame
    first_block_found = FALSE
    for (i in 1:length(D)) {
      tmp = D[[i]][15]$levels
      data = as.data.frame(data.table::rbindlist(tmp$data, fill = TRUE))
      data$dateTime = as.POSIXct(data$dateTime, format = "%Y-%m-%dT%H:%M:%S", tz = configtz)
      if (i == 1) {
        all_data = data
      } else {
        all_data = rbind(all_data, data)
      }
      if ("shortData" %in% names(tmp)) {
        shortData = data.table::rbindlist(tmp$shortData, fill = TRUE)
        shortData$dateTime = as.POSIXct(shortData$dateTime, format = "%Y-%m-%dT%H:%M:%S", tz = configtz)
        if (first_block_found == FALSE) {
          all_shortData = shortData
          first_block_found = TRUE
        } else {
          all_shortData = rbind(all_shortData, shortData)
        }
      }
    }
    # Expand to full time series
    all_data = all_data[order(all_data$dateTime),]
    D = as.data.frame(lapply(all_data, rep, all_data$seconds/epochSize))
    D$index = unlist(mapply(seq, rep(0, nrow(all_data)), (all_data$seconds/epochSize) - 1))
    D$dateTime = D$dateTime + D$index * epochSize
    S = as.data.frame(lapply(all_shortData, rep, all_shortData$seconds/epochSize))
    S$index = unlist(mapply(seq, rep(0, nrow(all_shortData)), (all_shortData$seconds/epochSize) - 1))
    S$dateTime = S$dateTime + S$index * epochSize
    D = rbind(D, S)
    D = D[, -which(colnames(D) %in% c("seconds", "index"))]
    D = D[order(D$dateTime),]
    dup_times = unique(D$dateTime[duplicated(D$dateTime)])
    # wake overrules other classifications
    D = D[-which(D$dateTime %in% dup_times & D$level != "wake"), ]
    D = D[!duplicated(D),]
    colnames(D)[2] = "sleeplevel"
  } else if (dataType == "steps" || dataType == "calories") {
    data = as.data.frame(data.table::rbindlist(D, fill = TRUE))
    data$dateTime = as.POSIXct(data$dateTime, format = "%m/%d/%y %H:%M:%S", tz = configtz)
    # GGIR expects resolution to be consistent across variables
    # so interpolate at 30 seconds to match resolution of sleep
    D = handleTimeGaps(data, epochSize = 30)
    D$value = as.numeric(D$value) / 2
    colnames(D)[2] = dataType
  } else if (dataType == "heart_rate") {
    collapseHR = function(x) {
      y = data.frame(dateTime = x$dateTime, bpm = x$value$bpm, confidence = x$value$confidence)
      return(y)
    }
    D = as.data.frame(data.table::rbindlist(lapply(D, collapseHR), fill = TRUE))
    D = D[!duplicated(D),]
    D$dateTime = as.POSIXct(D$dateTime, format = "%m/%d/%y %H:%M:%S", tz = configtz)
    colnames(D)[2] = dataType
  } else {
    stop("File type not recognised")
  }
  # Establish starttime in the correct timezone
  if (configtz != desiredtz) {
    D$dateTime = as.POSIXct(x = as.numeric(D$dateTime), tz = desiredtz,
                               origin = "1970-01-01")
  }
  return(D)
}