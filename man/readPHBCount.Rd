\name{readPHBCount}
\alias{readPHBCount}
\title{
  Read PHB Count data files (xlsx)
}
\description{
  Reads Philips Health Band (PHB) Count data file.
}
\usage{
  readPHBCount(filename = NULL,
                timeformat = "\%m/\%d/\%Y \%H:\%M:\%S",
                desiredtz = "", configtz = NULL,
                timeformatName = "timeformat")
    }
\arguments{
  \item{filename}{
    filename (required)
  }
  \item{timeformat}{
    Character, timestemp format.
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
  \item{timeformatName}{
    Character, name of timeformat variable to print in error message when
    timeformat is incorrect, of use to GGIR where argument names can differ.
  }
}
\value{
  Data.frame with time series, exact column names can vary between xlsx files.
}
\author{
  Vincent T van Hees <v.vanhees@accelting.com>
}