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
        if (i == 1) {
          all_shortData = shortData
        } else {
          all_shortData = rbind(all_shortData, shortData)
        }
      }
    }
    # Expand to full time series
    D = as.data.frame(lapply(all_data, rep, all_data$seconds/epochSize))
    D$dateTime = seq(from = D$dateTime[1], length.out = nrow(D), by = epochSize) 
    D$seconds = epochSize
    D = handleTimeGaps(D, epochSize) # Handle time gaps, if any

    S = as.data.frame(lapply(all_shortData, rep, all_shortData$seconds/30))
    S$dateTime = seq(from = S$dateTime[1], length.out = nrow(S), by = 30) 
    S$seconds = epochSize
    
    # merge in shortData (S)
    matching_times = which(S$dateTime %in% D$dateTime ==  TRUE)
    non_matching_times = which(S$dateTime %in% D$dateTime == FALSE)
    if (length(matching_times) > 0) {
      times_to_replace = S$dateTime[matching_times]
      D[which(D$dateTime %in% times_to_replace), ] = S[matching_times,]
    }
    if (length(non_matching_times) > 0) {
      D = rbind(D, S[non_matching_times,])
    }
    D = handleTimeGaps(D, epochSize) # Handle new time gaps, if any
    
    # Order time stamps
    D = D[order(D$dateTime), ]
    colnames(D)[2] = "sleeplevel"
  } else if (dataType == "steps" || dataType == "calories") {
    data = as.data.frame(data.table::rbindlist(D, fill = TRUE))
    data$dateTime = as.POSIXct(data$dateTime, format = "%m/%d/%y %H:%M:%S", tz = configtz)
    # GGIR expects resolution to be consistent across variables
    # so interpolate at 30 seconds to match resolution of sleep
    D = handleTimeGaps(data, epochSize = 30)
    D$value = as.numeric(D$value) / 2
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