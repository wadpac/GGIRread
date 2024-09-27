\name{mergePHBfilePairs}
\alias{mergePHBfilePairs}
\title{
  Merge Philips Health Band xlsx file pairs into one csv file
}
\description{
  Read, merge, and save Philips Health Band xlsx file pairs to csv.
  If the pair is incomplete the function will still convert the available
  xlsx file to csv.
}
\usage{
  mergePHBfilePairs(inputPath = ".", outputPath = ".")
}
\arguments{
  \item{inputPath}{
    Character, file path where xlsx files are located that need to be merged. 
    Assumption is that there is either one file starting with "Datalist_", one starting
    with "Sleep_Wake_", or both for each recording.
  }
  \item{outputPath}{
    Character, path to store the merge output
  }
}
\value{
  Function does not output values. Instead it stores the merged data as file in the location
  as specified by outputPath.
}
\author{
  Vincent T van Hees <v.vanhees@accelting.com>
}