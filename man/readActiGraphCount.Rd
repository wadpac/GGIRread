\name{readActiGraphCount}
\alias{readActiGraphCount}
\title{
  Read ActiGraph Count data files (csv)
}
\description{
  Reads ActiGraph Count data file. Currently a variety of csv format are facilitated.
}
\usage{
  readActiGraphCount(filename = file, desiredEpochSize = NULL,
                            timeformat = "\%m/\%d/\%Y \%H:\%M:\%S", tz = "")
}
\arguments{
  \item{filename}{
    filename (required)
  }
  \item{desiredEpochSize}{
    Numeric, desired epoch size in seconds. If set aggregate data by summation to
    this epochsize.
  }
  \item{timeformat}{
    Character, timestemp format.
  }
  \item{tz}{
    Character, timezone name from the timezone database names.
  }
}
\value{
  \item{data}{Matrix with one or multiple columns}
  \item{epochSize}{epoch size in seconds of data}
  \item{startTime}{POSIXlt format timestamp on which recording starts }
  \item{deviceSerialNumber}{Device serial number if it could be extracted 
  from the file header}
}
\author{
  Vincent T van Hees <v.vanhees@accelting.com>
}