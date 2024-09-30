\name{detectQuote}
\alias{detectQuote}
\title{
  Detect quote
}
\description{
  Helper function used internally, used to find out what value should
  be used for argument quote in function fread.
}
\usage{
  detectQuote(filename, skip)
}
\arguments{
  \item{filename}{
    Character
  }
  \item{skip}{
    skip
  }
}
\value{
  Character with quote object to be used for fread
}
\keyword{internal}
\author{
  Vincent T van Hees <v.vanhees@accelting.com>
}