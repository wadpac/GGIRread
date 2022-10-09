\name{readWav}
\alias{readWav}
\title{
  Reads .wav files as can be stored with OMGUI software by Axivity Ltd
}
\description{
  Reads the wav accelerometer data format as stored with the OMGUI software
  by Axivity Ltd and documented \href{https://github.com/digitalinteraction/openmovement/wiki/AX3-GUI#export-re-sampled-wav}{here}.
}
\usage{
  readWav(filename, start = 1, end = 100,units="minutes")
}
\arguments{
  \item{filename}{
    filename (required)
  }
  \item{start}{
    start point for reading data, see also units
  }
  \item{end}{
    end point for reading data, see also units
  }
  \item{units}{
    units used for defining start and end
  }
}
\details{
  If only \code{start} is defined then \code{g.binread} will read all data beyond
  \code{start} until the end of the file is reached
}
\value{
  \item{\code{rawxyz}}{matrix with raw x, y, and, z acceleration values}
  \item{\code{header}}{file header}
  \item{\code{timestamps}}{local timestamps for \code{rawxyz}}
}
\author{
  Vincent T van Hees <v.vanhees@accelting.com>
}