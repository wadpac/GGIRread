\name{GGIRread-package}
\alias{GGIRread-package}
\docType{package}
\title{
   A package to read data files from wearable accelerometers
}
\description{
  This package offers a collection of functions to read data files from
  wearable accelerometers. Some functions were migrated from R package GGIR
  to make GGIR more modular and to reduce it's complexity, while other functions
  such as readGENEActiv (R) and GENEActivReader (C++) are newly written.
}
\details{
  \tabular{ll}{
  Package: \tab GGIRread\cr
  Type: \tab Package\cr
  Version: \tab 0.2.4\cr
  Date: \tab 2022-10-09\cr
  License: \tab LGPL (>= 2.0, < 3)\cr
  }
}
\author{
  \itemize{
    \item Vincent T van Hees <v.vanhees@accelting.com> main creator and developer
    \item Patrick Bos developed function \link{GENEActivReader}
    \item Jing Hua Zhao <jinghua.zhao@mrc-epid.cam.ac.uk> co-developed function \link{readGenea}
    \item Evgeny Mirkes created function \link{readAxivity}
    \item Dan Jackson helped improve function \link{readAxivity}
  }
}