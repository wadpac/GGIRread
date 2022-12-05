readWav = function(filename, start = 1, end = 100, units = "minutes") {
  if (start == 0) start = 1
  #-----------------------------------------------------
  # get data
  S = tuneR::readWave(filename, from = start, to = end, units = units)
  B = tuneR::extractWave(S, from = start, to = length(S),xunit = c("samples", "time"))
  S = as.data.frame(S, stringsAsFactors = TRUE)
  B = as.data.frame(B, stringsAsFactors = TRUE)
  #-------------------------------------------------------
  # extract info from header: fileEncoding does not seem to be consistent, so try variants
  header = header_rownames = c()
  Nlines = 18
  while (length(header) == 0 | 
         length(grep("Scale-3",header)) == 0 |
         length(grep("Scale-2",header)) == 0 |
         length(grep("Scale-1",header)) == 0) { # as we do not know what header size is, search for it (needed in R version =< 3.1)
    
    try(expr = {header = suppressWarnings(read.csv(filename,
                                                   nrow = Nlines, header = TRUE))}, silent = TRUE)
    if (length(header) == 0) {
      header = suppressWarnings(read.csv(filename, skipNul = TRUE,
                                         nrow = Nlines, header = TRUE,
                                         fileEncoding = "WINDOWS-1252"))
    }
    if (length(header) == 0) {
      header = suppressWarnings(read.csv(filename, skipNul = TRUE,
                                         nrow = Nlines, header = TRUE,
                                         fileEncoding = "UTF-8"))
    }
    if (length(header) == 0) {
      header = suppressWarnings(read.csv(filename, skipNul = TRUE,
                                         nrow = Nlines, header = TRUE,
                                         fileEncoding = "latin1"))
    }
    if (length(header) > 0) {
      header_rownames = rownames(header)
      if (isTRUE(all.equal(as.character(1:Nlines), header_rownames))) {
        header = as.character(header[,1])
      } else {
        header = header_rownames
      }
    }
    Nlines = Nlines - 1
    if (Nlines == 10) print("Error: wav file header not recognized in function readWav from the GGIRread package")
  }
  scale3position = grep("Scale-3", header)
  header = header[1:scale3position]
  P = sapply(as.character(header),function(x) {
    tmp = unlist(strsplit(x,": "))
    if (length(tmp) == 1) {
      tmp = c(tmp, NA)
    } else {
      tmp = c(tmp[1],tmp[length(tmp)])
    }
  })
  P = as.data.frame(t(P), stringsAsFactors = TRUE)
  names(P) = c("hnames","hvalues")
  row.names(P) = 1:nrow(P)
  H = tuneR::readWave(filename, from = 1, to = 3600,units = c("seconds"), header = TRUE) #get wav file header
  #-----------------------------------------------
  # scale acceleration
  scale = as.numeric(as.character(P$hvalues[which(P$hnames == "Scale-1" | P$hnames == "Scale-2" | P$hnames == "Scale-3")]))
  
  if (length(scale) != 3) scale = rep(scale[1],3)
  range = 2^(H$bits - 1) # should be 32768 for 16 bit
  x = (B$C1/range) * scale[1]
  y = (B$C2/range) * scale[2]
  z = (B$C3/range) * scale[3]
  rawxyz = cbind(x,y,z)
  #---------------------------------------------
  # get time (we only need first timestamp) --> from header
  A = suppressWarnings(scan(filename, what = "character", nlines = 12,
                            quiet = TRUE, skipNul = TRUE)) #skipNul avoids undesired warning
  timestamp = paste0(A[grep("ICMTz", A) + 1:2], collapse = " ")
  if (length(timestamp) == 0 | timestamp == "") { #if not possible use other time in fileheader
    timestamp = as.character(P$hvalues[which(P$hnames == "Start")])
  }
  # Note: temperature information is available in channel 4, but documentation is unclear on how to interpret this information
  # therefore I am not using it and rely on auto-calibration as performed before .wav file is generated
  # g = (B$C4/ range) # temperature, light and battery ?
  invisible(list(rawxyz = rawxyz, header = P, timestamp = timestamp))
}
