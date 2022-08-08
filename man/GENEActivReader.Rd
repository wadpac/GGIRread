\name{GENEActivReader}
\alias{GENEActivReader}
\title{
  Function (C++) to read binary files as produced by the GENEActiv accelerometer
}
\description{
  Function to read binary files as produced by the GENEActiv accelerometer by
  ActivInsights Ltd https://activinsights.com/
}
\usage{
  GENEActivReader(filename, start = 0L, end = 0L, progress_bar = FALSE)
}
\arguments{
   \item{filename}{
    filename (required)
  }
  \item{start}{
    start page for reading data
  }
  \item{end}{
    end page for reading data
  }
  \item{progress_bar}{
    Boolean
  }
}
\details{
  If only \code{start} page is defined then all data is read beyond
  \code{start} until the end of the file is reached
}
\value{
  \item{info}{List with ReadOK (good=0 or error=1), ReadErrors (Count of pages with read errors), SampleRate (Hertz), numBlocksTotal}
  \item{time}{Numeric vector with time in miliseconds since start page}
  \item{x}{Numeric vector with x-axis acceleration in gravitational units}
  \item{y}{Numeric vector with y-axis acceleration in gravitational units}
  \item{z}{Numeric vector with z-axis acceleration in gravitational units}
  \item{temperature}{matrix with battery voltage and corresponding timestamps}
  \item{lux}{Numeric vector with lux values in Volts}
}
\author{
  Patrick Bos <egpbos@gmail.com>
  Vincent T van Hees <v.vanhees@accelting.com>
}