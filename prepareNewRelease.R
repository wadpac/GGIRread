prepareNewRelease = function(version = c()) {
  # In preparation of a new package release this function helps
  # to check that version number and release date are correct
  # in the DESCRIPTION file, GGIRread-package.Rd, CITATION.cff, and NEWS.Rd
  # The function does not fix any errors, it only warns the user about
  # mistakes via messages in the console.
  # The function is not part of the package on CRAN, because it's name is
  # listed in the .RBuildignore file.
  # Argument version: a character specifying the expected version number, e.g. "1.8-1"
  date = unlist(strsplit(as.character(Sys.time())," "))[1]
  dateReversed = unlist(strsplit(date,"-"))
  dateReversed = paste0(dateReversed[3],"-",dateReversed[2],"-",dateReversed[1])
  # Check DESCRIPTION file
  D = read.csv(file = "./DESCRIPTION",sep="\n")
  errorfound = FALSE
  i = 1
  while (i <= nrow(D)) {
    tmp = as.character(unlist(strsplit(as.character(D[i,]),": ")))
    if (tmp[1] == "Version") {
      if (tmp[2] != version) {
        cat("\nError: Version number is not correct in DESCRIPTION file")
        errorfound = TRUE
      } 
    }
    if (tmp[1] == "Date") {
      if (tmp[2] != date) {
        cat("\nError: Date is not correct in DESCRIPTION file")
        errorfound = TRUE
      } 
    }
    i = i + 1
  }
  # Check GGIRread-package.Rd file
  P = read.csv(file = "./man/GGIRread-package.Rd",sep="\n")
  i = 1
  while (i <= nrow(P)) {
    tmp = as.character(unlist(strsplit(as.character(P[i,]),"[: \\]")))
    if (tmp[1] == "Version") {
      if (tmp[5] != version) {
        cat("\nError: Version number is not correct in GGIRread-package.Rd file")
        errorfound = TRUE
      } 
    }
    if (tmp[1] == "Date") {
      if (tmp[5] != date) {
        cat("\nError: Date is not correct in GGIRread-package.Rd file")
        errorfound = TRUE
      }
    }
    i = i + 1
  }
  if (errorfound == FALSE) cat(paste0("\nNo problem found. Package consistently uses version ",version," and release date ", dateReversed))
}