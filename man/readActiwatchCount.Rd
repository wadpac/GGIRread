\name{readActiwatchCount}
\alias{readActiwatchCount}
\title{
  Read Actiwatch Count data files (csv)
}
\description{
  Reads Actiwatch Count data file. Currently a variety of csv format are facilitated.
}
\usage{
  readActiwatchCount(filename = NULL, desiredEpochSize = NULL,
                            timeformat = "\%m/\%d/\%Y \%H:\%M:\%S", tz = "",
                            timeformatName = "timeformat")
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
  \item{timeformatName}{
    Character, name of timeformat variable to print in error message when
    timeformat is incorrect, of use to GGIR where argument names can differ.
  }
}
\value{
  \item{data}{Matrix with one or multiple columns}
  \item{epochSize}{epoch size in seconds of data}
  \item{startTime}{POSIXlt format timestamp on which recording starts }
}
\author{
  Vincent T van Hees <v.vanhees@accelting.com>
}