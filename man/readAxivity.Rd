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
    filename (required) is name of cwa file to read
  }
  \item{start}{
    start can be timestamp "year-month-day hr:min:sec" or non-negative integer
    which is block number. Block size is 300 of measurements with specified 
    frequency.
  }
  \item{end}{
    end can be timestamp "year-month-day hr:min:sec" or non-negative integer
    which is block number. End must be not less than start. If end is less
    or equal to start then there is no data read. Block size is 300 of 
    measurements with specified frequency.
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
  \itemize{
  \item \code{data} dataframe with data from start inclusive till end exclusive.
    If start == end then data section of final structure is empty.
    \itemize{
      \item timestamp in numeric format
      \item gx gyroscope x-axis (only available in AX6)
      \item gy gyroscope y-axis (only available in AX6)
      \item gz gyroscope z-axis (only available in AX6)
      \item x accelerometer x-axis
      \item y accelerometer y-axis
      \item z accelerometer z-axis
      \item temperature in Celsius (only recorded once per block)
      \item battery one value per block (only recorded once per block)
      \item light in LUX on (only recorded once per block)
      }
  \item \code{header} file header. list of header information:
    \itemize{
    \item uniqueSerialCode is unque serial code of used device
    \item frequency is measurement frequency. All data will be resampled for this 
    frequency
    \item start is timestamp in numeric form. To get text representation it is
    enough to use as.POSIXct(start, origin = "1970-01-01", tz=desiredtz)
    \item device is "Axivity"
    \item firmwareVersion
    \item blocks is number of datablocks with 80 or 120 raw observations in each.
    }
  \item \code{QClog} dataframe with log of integrity checks on each block including:
    \itemize{
      \item checksum_pass Boolean to indicate whether checksum was zero.
      \item blockID_previous ID for previous block
      \item blockID_current ID for current block
      \item start_previous numeric start time of previous block
      \item start_current numeric start time of current block
      \item blockLengthSeconds length of previous block
      \item frequency_blockheader frequency derived from block header
      \item frequency_observed frequency as observed
      \item imputed Boolean to indicate whether this block was inputed
    }
  }
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