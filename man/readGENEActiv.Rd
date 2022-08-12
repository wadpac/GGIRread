\name{readGENEActiv}
\alias{readGENEActiv}
\title{
  Function (R) to read binary files as produced by the GENEActiv accelerometer
}
\description{
  R function wrapper around \link{GENEActivReader} to read binary files as produced
  by the GENEActiv accelerometer by ActivInsights Ltd https://activinsights.com/
}
\usage{
  readGENEActiv(filename, start = 0L, end = 0L, progress_bar = FALSE,
                desiredtz = "", configtz = NULL)
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
  \item{desiredtz}{
    Character, timezone database name of the timezone where the accelerometer
    was worn.
  }
  \item{configtz}{
    Character, timezone database name of the timezone where the accelerometer was
    configured. Leave NULL if equal to timezone where experiment took place.
  }
}
\details{
  If only \code{start} page is defined then all data is read beyond
  \code{start} until the end of the file is reached
}
\value{
  
  header:
  
  \item{serial_number}{Device serial number as extracted from file header}
  \item{firmware}{Firmware version}
  \item{tzone}{Time zone as extracted from file header}
  \item{ReadOK}{see \link{GENEActivReader}}
  \item{SampleRate}{matrix with battery voltage and corresponding timestamps}
  \item{ReadErrors}{matrix with battery voltage and corresponding timestamps}
  \item{numBlocksTotal}{matrix with battery voltage and corresponding timestamps}
  \item{StartTime}{Start time as extracted from file header}
  
  data.out:
  
  \item{time}{Unix time in seconds with decimal places for miliseconds}
  \item{x}{see \link{GENEActivReader}}
  \item{y}{see \link{GENEActivReader}}
  \item{z}{see \link{GENEActivReader}}
  \item{light}{Ligth values express in lux units}
  \item{temperature}{Temperature in Celsius}
}
\examples{
  binfile  = system.file("testfiles/GENEActiv_testfile.bin", package = "GGIRread")[1]
  rdata = readGENEActiv(filename = binfile, start = 1, end = 1, desiredtz = "Europe/London")
}

\author{
  Patrick Bos <egpbos@gmail.com>
  Vincent T van Hees <v.vanhees@accelting.com>
}