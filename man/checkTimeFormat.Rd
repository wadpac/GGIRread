\name{checkTimeFormat}
\alias{checkTimeFormat}
\title{
  Check timestamp format
}
\description{
  Helper function used internally. Generate informative error message if timestamp
  could not be recognisede.
}
\usage{
  checkTimeFormat(timestamp_POSIX, rawValue = " ?? ", timeformat = " ?? ",
                           timeformatName = NULL)
}
\arguments{
  \item{timestamp_POSIX}{
    POSIX object with timestamp
  }
  \item{rawValue}{
    Timestamp value as encounterd in the data before processing
  }
  \item{timeformat}{
    Timestap format used for converting timestamp
  }
  \item{timeformatName}{
    Character with the argument name to specify the timeformat.
    If used as dependency of GGIR then this argument name will be different.
  }
}
\value{
  No output value is generated, only an error message if timestamp could not be
  recognised.
}
\keyword{internal}
\author{
  Vincent T van Hees <v.vanhees@accelting.com>
}