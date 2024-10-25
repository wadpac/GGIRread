.onAttach <- function(...) {
  if (!interactive()) return()
  # stop interactive calling from `library(GGIRread)`
  repos <- getOption("repos")
  if ("@CRAN@" %in% repos) {
    repos <- utils::getCRANmirrors()
    # choose cloud/first if option triggers `contrib.url`
    # to call `chooseCRANmirror`
    repos <- repos$URL[1]
    packageStartupMessage(
      "No CRAN mirror set, so using ", repos,
      " to check GGIRread package version")
  }
  pkgs <- available.packages(repos = repos)
  cran_version <- package_version(pkgs[which(pkgs[,1] == "GGIRread"),"Version"])
  if (length(cran_version) == 0) return() # handle no internet connection
  local_version <- utils::packageVersion("GGIRread")
  behind_cran <- cran_version > local_version
  if (interactive()) {
    if (behind_cran) {
      msg <- paste0("A newer version of GGIRread is available with bug fixes and new features. [", local_version," --> ", cran_version, "]")
      packageStartupMessage(msg)
    }
  }
}
