\name{NEWS}
\title{News for Package \pkg{GGIRread}}
\newcommand{\cpkg}{\href{http://CRAN.R-project.org/package=#1}{\pkg{#1}}}
\section{Changes in version 0.2.8 (release date:26-05-2023)}{
    \itemize{
      \item Major bug fix introduced in 0.2.7 release affecting readAxivity #29
      \item resample function now able to handle timestamps outside block as
      a result of large time gaps in cwa files #32
    }
}
\section{Changes in version 0.2.7 (release date:17-05-2023)}{
    \itemize{
      \item Fixes bug in LUX extraction for GENEActiv .bin data (#27)
      \item Expanded Axivity .wav file header extraction with alternative method
      for sampling rate extraction (#26)
      \item Speeding up readAxivity function by approximately 75 percent.
    }
}
\section{Changes in version 0.2.6 (release date:05-12-2022)}{
    \itemize{
      \item New CRAN release, only minor improvements to syntax
    }
}
\section{Changes in version 0.2.5 (GitHub-only-release date:09-11-2022)}{
    \itemize{
      \item readWav now correctly identifies the header and first timestamp
    }
}
\section{Changes in version 0.2.4 (release date:09-10-2022)}{
    \itemize{
      \item Addressing POSIX errors in R-devel.
      \item Speeding up readAxivity function by approximately 40 percent.
      \item Adding readWav function for reading accelerometer data stored in
      wav(Audio) format. This function was formerly known as g.wavread in the
      GGIR package but has been migrated here to make the GGIR package lighter.
    }
}
\section{Changes in version 0.2.3 (release date:29-09-2022)}{
    \itemize{
      \item Changed POSIXlt to character conversion to be compatible with R-devel
      update svn revision r82904 (2022-09-24 19:32:52)
      \item Tidying up namespace references
      \item Added zzz function to auto-check onattach package version relative to CRAN
    }
}
\section{Changes in version 0.2.2 (release date:31-08-2022)}{
    \itemize{
      \item Now also extracting GENEActiv RecordingID, Handedness, DeviceLocation and DeviceModel from header
      \item Removed skin_on_cran() calls in unit test
      \item Improved description for resample function
      \item Fixed errors resulting from how lux values are read (issue #12)
    }
}
\section{Changes in version 0.2.1 (release date:16-08-2022)}{
    \itemize{
      \item Addressing request from CRAN to revise AxivityNumUnpack
    }
}
\section{Changes in version 0.2.0 (release date:12-08-2022)}{
    \itemize{
      \item First release with functionality for reading Axivity .cwa, GENEActiv .bin, and Genea, .bin data.
    }
}
