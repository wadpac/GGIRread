\name{readFitbit}
\alias{readFitbit}
\title{
  Read Fitbit data files (json)
}
\description{
  Reads Fitbit data file (json) with sleep, steps or calories.
}
\usage{
  readFitbit(filename = NULL, desiredtz = "", configtz = NULL)
}
\arguments{
  \item{filename}{
    Character, filename (required) of json file
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
  Data.frame with data converted to time series
}
\author{
  Vincent T van Hees <v.vanhees@accelting.com>
}