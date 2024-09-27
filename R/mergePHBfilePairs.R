mergePHBfilePairs = function(inputPath = ".", outputPath = ".") {
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
    if (length(file1) > 0) {
      data1 = as.data.frame(readxl::read_excel(path = filesForThisPerson[file1], 
                                               col_types = "text", skip = 8),
                            row.names = FALSE)
      header = as.data.frame(readxl::read_excel(path = filesForThisPerson[file1], 
                                                col_types = "text", n_max = 8,
                                                .name_repair = "unique_quiet"),
                             row.names = FALSE)[, 1]
      SNlocation = grep(pattern = "deviceSN", x = header)
      if (length(SNlocation) > 0) {
        deviceSN = unlist(strsplit(header[grep(pattern = "deviceSN", x = header)], " "))
        deviceSN = deviceSN[length(deviceSN)]
      } else {
        deviceSN = NULL
      }
      colnames(data1)[grep(pattern = "counts", x = colnames(data1), ignore.case = TRUE)] = "counts"
      colnames(data1)[grep(pattern = "offWrist", x = colnames(data1), ignore.case = TRUE)] = "nonwear"
    }
    # Sleep wake scores
    if (length(file2) > 0) {
      data2 = as.data.frame(readxl::read_excel(path = filesForThisPerson[file2], col_types = "text", skip = 8), row.names = FALSE)
      colnames(data2)[grep(pattern = "sleepWake", x = colnames(data2), ignore.case = TRUE)] = "sleep"
    }
    if (length(file1) > 0 && length(file2) > 0) {
      data2 = data2[, which(colnames(data2) != "sleepEventMarker")]
      data = merge(data1, data2, by = "timeStamp")
    } else {
      if (length(file1) > 0) {
        data = data1
      } else {
        data = data2
      }
    }
    colnames(data)[grep(pattern = "timeStamp", x = colnames(data))] = "timestamp"
    newName = gsub(pattern = "Sleep_Wake", replacement = "def", x =  basename(filesForThisPerson[file2]))
    newName = paste0(unlist(strsplit(newName, "[.]")) , collapse = paste0("_", deviceSN, "."))
    newName = gsub(pattern = "xlsx", replacement = "csv", x = newName)
    outputfile = paste0(outputPath, "/", newName)
    write.csv(x = data, file = outputfile, row.names = FALSE)
  }
}