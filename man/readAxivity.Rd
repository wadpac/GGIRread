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
    desiredtz = "", configtz = c(), interpolationType=1, loadbattery = FALSE,
    header = NULL, frequency_tol = 0.1)
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
  \item{loadbattery}{
    Boolean to indicate whether battery voltage should be loaded
  }
  \item{header}{
    Header to be reused if it was extracted earlier
  }
  \item{frequency_tol}{
    Numeric value representing the tolerance for frequency bias expressed
    as a fraction of the expected sampling frequency. For example, 0.10 indicates
    that we accept the sampling frequency configured at 100 Hertz to vary between
    90 and 110 Hertz. If this condition is not met the data will be imputed with
    a constant value and the start and end time will be logged in the QClog output object.
  }
  
}
\value{
  \item{\code{data}}{dataframe with timestamp, raw x, -y, and, -z acceleration values,
  temperature, battery and light}
  \item{\code{header}}{file header}
  \item{\code{QClog}}{dataframe with log of integrity checks on each block including:
  checksum, sequence ID and start of previuos and current block, and sampling frequency
  observed as well as sampling frequency from block header}
}
\examples{
  \donttest{
    cwafile  = system.file("testfiles/ax3_testfile.cwa", package = "GGIRread")[1]
    AX3 = readAxivity(filename = cwafile, desiredtz = "Europe/Berlin", start = 1, end = 2)
  }
}
\author{
  Evgeny Mirkes <em322@leicester.ac.uk>
  Vincent van Hees <v.vanhees@accelting.com>
}