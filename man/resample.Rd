\name{resample}
\alias{resample}
\docType{package}
\title{
  Simple function using Rcpp
}
\description{
  Simple function using Rcpp
}
\usage{
  resample(raw, rawTime, time, stop, type=1)	
}
\arguments{
  \item{raw}{
    stop-by-3 matrix with raw values of x, y and z.
  }
  \item{rawTime}{
    vector with stop elements of raw time.
  }
  \item{time}{
    array with required time points.
  }
  \item{stop}{
    Number of rows in raw
  }
  \item{type}{
    integer to indicate type of interpolation, 1=linear, 2=nearest neighbour
  }
}
\examples{
  raw = cbind(1:10, 1:10, 1:10)
  rawTime = seq(0.1, 1, by = 0.1)
  time = seq(0.15001, 1.05001, by = 0.1)
  stop = 10
  dat_lin = resample(raw, rawTime, time, stop, type = 1)
}
