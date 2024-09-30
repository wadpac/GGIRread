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
  mergePHBfilePairs(filenames = NULL,
                    timeformat = "\%m/\%d/\%Y \%H:\%M:\%S",
                    desiredtz = "", configtz = NULL,
                    timeformatName = "timeformat")
}
\arguments{
  \item{filenames}{
    Character vector, specifying the two filenames to be merged. 
    Assumption is that there is one file starting with "Datalist_" and one starting
    with "Sleep_Wake_".
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
  Merged data from the two files.
}
\author{
  Vincent T van Hees <v.vanhees@accelting.com>
}