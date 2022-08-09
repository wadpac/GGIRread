\name{readAxivity}
\alias{readAxivity}
\title{
  Function to read .cwa-format files as produced by the accelerometer named 'Axivity'
}
\description{
  For reading .cwa-format data with the Axivity AX3 and AX6 sensors.
}
\usage{
  readAxivity(filename, start = 0, end = 0, progressBar = FALSE, 
    desiredtz = "", configtz = c(), interpolationType=1)
}
\arguments{
  \item{filename}{
    filename (required)
  }
  \item{start}{
    start point for reading data, this can either be a timestamp
    "year-month-day hr:min:sec" or a page number (optional)
  }
  \item{end}{
    end point for reading data, this can either be a timestamp 
    "year-month-day hr:min:sec" or a page number (optional)
  }
  \item{progressBar}{
    Is trigger to switch on/off the text progress bar. If progressBar
    is TRUE then the function displays the progress bar but it works
    slightly slower
  }
  \item{desiredtz}{
    Desired timezone, a character with timezone database name.
  }
  \item{configtz}{
    Only functional for AX3 cwa data at the moment. Timezone in which the accelerometer
    was configured. Only use this argument if the timezone of configuration and
    timezone in which recording took place are different.
  }
  \item{interpolationType}{
    Integer to indicate type of interpolation to be used, 1=linear, 2=nearest neighbour.
  }
}
\value{
  \item{\code{data}}{dataframe with timestamp, raw x, -y, and, -z acceleration values,
  temperature, battery and light}
  \item{\code{header}}{file header}
}
\examples{
  cwafile  = system.file("testfiles/ax3_testfile.cwa", package = "GGIRread")[1]
  AX3 = readAxivity(filename = cwafile, desiredtz = "Europe/Berlin", start = 1, end = 2)
}
\author{
  Evgeny Mirkes <em322@leicester.ac.uk>
  Vincent van Hees <v.vanhees@accelting.com>
}