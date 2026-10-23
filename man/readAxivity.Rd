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
    header = NULL, frequency_tol = 0.1, maxAllowedCorruptBlocks = 20)
}
\arguments{
  \item{filename}{
    filename (required) is name of cwa file to read
  }
  \item{start}{
    Zero-based, non-negative integer index of the first data block to read.
    The metadata header is not counted as a data block.
  }
  \item{end}{
    Exclusive data block index. Use \code{Inf} to read through the last complete
    data block. Values beyond the file's block count are limited to that count.
    If \code{end <= start}, only the header is returned, with \code{data = NULL}.
    The default \code{start = end = 0} therefore reads only the header.
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
    Timezone in which the accelerometer was configured. 
    Only use this argument if the timezone of configuration and timezone
    in which recording took place are different.
  }
  \item{interpolationType}{
    Integer to indicate type of interpolation to be used, 1=linear, 2=nearest neighbour.
  }
  \item{loadbattery}{
    Boolean to indicate whether battery voltage should be loaded
  }
  \item{header}{
    Header returned by an earlier call for the same file, to avoid reading it
    again when processing chunks. Use the same timezone settings as in that call.
    Warnings about incomplete trailing bytes are issued only when \code{header = NULL}.
  }
  \item{frequency_tol}{
    Numeric value representing the tolerance for frequency bias expressed
    as a fraction of the expected sampling frequency. For example, 0.10 indicates
    that we accept the sampling frequency configured at 100 Hertz to vary between
    90 and 110 Hertz. If this condition is not met the data will be imputed with
    a constant value and the start and end time will be logged in the QClog output object.
    Regardless of the setting the function will always log instances when frequency 
    differs by more than 5\%, but if this is less than frequency_tol the block will
    not be imputed.
  }
  \item{maxAllowedCorruptBlocks}{
    Max number of consecutive blocks with a failed checksum that we'll tolerate.
  }
  
}
\value{
  \itemize{
  \item \code{data} dataframe with data from start inclusive till end exclusive.
    If start == end then data section of final structure is empty.
    \itemize{
      \item time numeric timestamp in seconds since the Unix epoch
      \item gx gyroscope x-axis in degrees per second (AX6 with gyroscope enabled)
      \item gy gyroscope y-axis in degrees per second (AX6 with gyroscope enabled)
      \item gz gyroscope z-axis in degrees per second (AX6 with gyroscope enabled)
      \item x accelerometer x-axis in g
      \item y accelerometer y-axis in g
      \item z accelerometer z-axis in g
      \item temp temperature in Celsius (recorded once per block)
      \item battery voltage in volts (recorded once per block), or zero when \code{loadbattery = FALSE}
      \item light raw light sensor value (recorded once per block), without conversion to lux
      }
  \item \code{header} file header. list of header information:
    \itemize{
    \item uniqueSerialCode is the device serial code
    \item frequency is the nominal measurement frequency in Hz, including
    fractional rates such as 12.5 Hz. All data are resampled at this frequency
    \item start is a \code{POSIXct} timestamp for the first valid data block,
    displayed in \code{desiredtz}
    \item device is "Axivity"
    \item firmwareVersion
    \item blocks is the number of complete data blocks, including corrupt blocks
    \item accrange is the accelerometer range in g (for example, 8 denotes +/-8 g)
    \item hardwareType is \code{"AX3"} or \code{"AX6"}
    \item blockLength is the sample count of the first valid data block,
    typically 40, 80 or 120 depending on the sensor configuration
    }
  \item \code{QClog} dataframe logging failed checksums, imputed intervals and
    frequency deviations above 5\%; \code{NULL} when there are no such events.
    It is omitted for header-only reads. Columns are:
    \itemize{
      \item checksum_pass Boolean to indicate whether checksum was zero.
      \item blockID_current ID of the block whose interval is being checked
      \item blockID_next ID of the following block
      \item start numeric start time of the checked interval, in seconds since the Unix epoch
      \item end numeric end time of the checked interval, in seconds since the Unix epoch
      \item blockLengthSeconds duration of the interval in seconds
      \item frequency_blockheader frequency derived from block header
      \item frequency_observed frequency as observed
      \item imputed Boolean indicating whether the interval was imputed
    }
  }
}
\details{
  Only complete 512-byte data packets are decoded. An incomplete final packet
  is excluded with a warning when reading the header. An error is raised when
  no complete data packets remain. Corrupt packets at the start or end of a
  requested interval are skipped up to \code{maxAllowedCorruptBlocks}; internal
  corruption can cause the intervening interval to be imputed.

  Acceleration is interpolated using the selected method. Rejected intervals
  are filled with the preceding acceleration orientation normalised to 1 g
  when its magnitude lies between 0.8 and 1.2 g, otherwise with \code{c(0, 0, 1)}.
  Gyroscope channels are filled with zero during imputation.

  For failed-checksum rows in \code{QClog}, the packet's contents cannot be
  trusted: block IDs are inferred from file positions and timing and frequency
  fields are zero. Such rows have \code{imputed = FALSE}; a separate row records
  any imputation between the surrounding valid packets.
}
\examples{
  \donttest{
    cwafile  = system.file("testfiles/ax3_testfile.cwa", package = "GGIRread")[1]
    AX3 = readAxivity(filename = cwafile, desiredtz = "Europe/Berlin", start = 1, end = 2)
  }
}
\author{
  Evgeny Mirkes <em322@leicester.ac.uk>
  Lena Kushleyeva
  Vincent van Hees <v.vanhees@accelting.com>
}
