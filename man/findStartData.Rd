\name{findStartData}
\alias{findStartData}
\title{
  Find start of data inside text file.
}
\description{
  Helper function used internally. Finds variables start of the data in 
  Actiwatch and Actical data.
}
\usage{
  findStartData(filename, quote, startindex, blockname = NULL)
}
\arguments{
  \item{filename}{
    Character
  }
  \item{quote}{
    Quote as extracted with \link{detectQuote}
  }
  \item{startindex}{
    Start index where to start searching. For Actical we start at 300 while
    for Actiwatch we start at 1000.
  }
  \item{blockname}{
    Character with name of data block to search for.
    For Actical we use "epoch-by-epoch".
  }
}
\value{
  Start index
}
\keyword{internal}
\author{
  Vincent T van Hees <v.vanhees@accelting.com>
}