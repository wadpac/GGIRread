mergePHBfilePairs = function(inputPath = ".", outputPath = ".",
                             timeformat = "%m/%d/%Y %H:%M:%S",
                             desiredtz = "", configtz = NULL,
                             timeformatName = "timeformat") {
  # merges Philips Health Band xlsx files per participant
  # as there can be multiple files per participant.
  fnames = dir(inputPath, recursive = FALSE, full.names = TRUE, pattern = "[.]xlsx")
  fileOverview = data.frame(filename = fnames)
  extractID = function(x) {
    x = basename(x)
    x = gsub(pattern = "sleep_wake", replacement = "sleepwake", x = tolower(x))
    ID = unlist(strsplit(x, "_"))[2]
    return(ID)
  }
  fileOverview$ID = unlist(lapply(fileOverview$filename, FUN = extractID))
  
  uids = unique(fileOverview$ID)
  for (uid in uids) {
    filesForThisPerson = fileOverview$filename[which(fileOverview$ID == uid)]
    # Identify both file
    file1 = grep(pattern = "datalist", x = filesForThisPerson, ignore.case = TRUE)
    file2 = grep(pattern = "sleep_wake", x = filesForThisPerson, ignore.case = TRUE)
    if (length(file1) == 0 && length(file2) == 0) {
      next
    }
    # Data
    deviceSN = NULL
    if (length(file1) > 0) {
      data1 = readPHBCount(filename = filesForThisPerson[file1], timeformat = timeformat,
                   desiredtz = desiredtz, configtz = configtz,
                   timeformatName = timeformatName)
      deviceSN = data1$deviceSN
    }
    # Sleep wake scores
    if (length(file2) > 0) {
      data2 = readPHBCount(filename = filesForThisPerson[file2], timeformat = timeformat,
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
    colnames(data)[grep(pattern = "timestamp", x = colnames(data))] = "timestamp"
    newName = gsub(pattern = "Sleep_Wake", replacement = "def", x =  basename(filesForThisPerson[file2]), ignore.case = TRUE)
    newName = paste0(unlist(strsplit(newName, "[.]")) , collapse = paste0("_", deviceSN, "."))
    newName = gsub(pattern = "xlsx", replacement = "csv", x = newName)
    outputfile = paste0(outputPath, "/", newName)
    write.csv(x = data, file = outputfile, row.names = FALSE)
  }
}