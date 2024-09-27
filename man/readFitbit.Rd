\name{readFitbit}
\alias{readFitbit}
\title{
  Read Fitbit data files (json)
}
\description{
  Reads Fitbit data file (json) with sleep, steps or calories.
}
\usage{
  readFitbit(filename = NULL)
}
\arguments{
  \item{filename}{
    Character, filename (required) of json file
  }
}
\value{
  Data.frame with data converted to time series
}
\author{
  Vincent T van Hees <v.vanhees@accelting.com>
}