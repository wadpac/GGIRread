\name{mergeFitbitData}
\alias{mergeFitbitData}
\title{
  Merge Fitbit json files into one data.frame
}
\description{
  Read and merge Fitbit json files into one data.frame, and
  give warning if time series do not intersect.
}
\usage{
  mergeFitbitData(filenames = NULL,
                    desiredtz = "", configtz = NULL)
}
\arguments{
  \item{filenames}{
    Character vector, specifying the filenames (at least two) to be merged.
  }
  \item{desiredtz}{
    Character, timezone name where the accelerometer was worn. Timezone names are
    expected to be the timezone database names, e.g. Europe/London. See also:
    https://en.wikipedia.org/wiki/List_of_tz_database_time_zones
  }
  \item{configtz}{
    Character, timezone name where the accelerometer was configured. Leave NULL
    if equal to desiredtz. Timezones name are expected to be the timezone database
    names, e.g. Europe/London. See also:
    https://en.wikipedia.org/wiki/List_of_tz_database_time_zones
  }
}
\value{
  Merged data from the two or more files.
}
\author{
  Vincent T van Hees <v.vanhees@accelting.com>
}