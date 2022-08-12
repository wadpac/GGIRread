\name{readGenea}
\alias{readGenea}
\title{
  Function to read binary files as produced by the accelerometer named 'Genea',
  not to be confused with the 'GENEActiv' (see package GENEAread for this)
}
\description{
  For reading the binary data as collected with a Genea accelerometer 
  (Unilever Discover, UK). For reading GENEActive binary data, see package GENEAread.
}
\usage{
  readGenea(filename, start = 0, end = 0)
}
\arguments{
  \item{filename}{
    filename (required)
  }
  \item{start}{
     start point for reading data, this can either be a timestamp in POSIXct format
     "year-month-day hr:min:sec" or a page number (optional)
  }
  \item{end}{
    end point for reading data, this can either be a timestamp in POSIXct format
    "year-month-day hr:min:sec" or a page number (optional)
  }
}
\details{
  If only \code{start} is defined then \code{readGenea} will read all data beyond
  \code{start} until the end of the file is reached
}
\value{
 \item{\code{rawxyz}}{matrix with raw x, y, and, z acceleration values}
  \item{\code{header}}{file header}
  \item{\code{timestamps1}}{timestamps for \code{rawxyz} in seconds since 1970-01-01 00:00}
  \item{\code{timestamps2}}{timestamps for \code{rawxyz} in day time format }
  \item{\code{batt.voltage}}{matrix with battery voltage and corresponding timestamps}
}
\examples{
  geneafile  = system.file("testfiles/genea_testfile.bin", package = "GGIRread")[1]
  GENEA = readGenea(filename = geneafile, start = 0, end = 2)
}
\author{
  Vincent T van Hees <v.vanhees@accelting.com>
  Jing Hua Zhao <jinghua.zhao@mrc-epid.cam.ac.uk>
}