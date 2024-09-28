\name{readActicalCount}
\alias{readActicalCount}
\title{
  Read Actical Count data files (csv)
}
\description{
  Reads Actical Count data file.
}
\usage{
  readActicalCount(filename = NULL,
                            timeformat = "\%m/\%d/\%Y \%H:\%M:\%S", tz = "",
                            timeformatName = "timeformat")
}
\arguments{
  \item{filename}{
    filename (required)
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