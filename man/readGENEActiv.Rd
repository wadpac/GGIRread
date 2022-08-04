\name{readGENEActiv}
\alias{readGENEActiv}
\title{
  Function to read binary files as produced by the GENEActiv accelerometer
}
\description{
  For reading the binary data as collected with a Genea accelerometer 
  (Unilever Discover, UK). For reading GENEActive binary data, see package GENEAread.
}
\usage{
  readGENEActiv(filename, start = 0L, end = 0L, progress_bar = FALSE, desiredtz = "")
}
\arguments{
  \item{filename}{
    filename (required)
  }
  \item{start}{
     start point for reading data
  }
  \item{end}{
    end point for reading data
  }
  \item{progress_bar}{
    Boolean
  }
  \item{desiredtz}{
    Character, timezone database name.
  }
}
\details{
  If only \code{start} is defined then \code{GeneaBinReader} will read all data beyond
  \code{start} until the end of the file is reached
}
\value{
 \item{\code{rawxyz}}{matrix with raw x, y, and, z acceleration values}
  \item{\code{header}}{file header}
  \item{\code{timestamps1}}{timestamps for \code{rawxyz} in seconds since 1970-01-01 00:00}
  \item{\code{timestamps2}}{timestamps for \code{rawxyz} in day time format }
  \item{\code{batt.voltage}}{matrix with battery voltage and corresponding timestamps}
}
\author{
  Vincent T van Hees <v.vanhees@accelting.com>
  Jing Hua Zhao <jinghua.zhao@mrc-epid.cam.ac.uk>
}