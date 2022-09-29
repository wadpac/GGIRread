.onAttach <- function(...) {
  if (!interactive()) return()
  pkgs <- utils::available.packages()
  cran_version <- package_version(pkgs[which(pkgs[,1] == "GGIRread"),"Version"])
  local_version <- utils::packageVersion("GGIRread")
  behind_cran <- cran_version > local_version
  if (interactive()) {
    if (behind_cran) {
      msg <- paste0("A newer version of GGIRread is available with bug fixes and new features. [", local_version," --> ", cran_version, "]")
      packageStartupMessage(msg)
    }   
  }
}
