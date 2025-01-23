![GitHub Actions R-CMD-check](https://github.com/wadpac/GGIRread/workflows/R-CMD-check/badge.svg)
[![codecov](https://codecov.io/gh/wadpac/GGIRread/branch/main/graph/badge.svg?token=SNII9OKA4J)](https://app.codecov.io/gh/wadpac/GGIRread)

# GGIRread

Functions for reading accelerometer data from the following file formats:

Brand | Device name | File extension | Data type | GGIRread function
------ | ------- | ------- | ---------------- | ---------------------
Axivity Ltd https://axivity.com/ | AX3 and AX6 | .cwa | raw gravitational units |readAxivity
ActivInsights Ltd https://activinsights.com/ | GENEActiv Original and Sleep | .bin | raw gravitational units  | readGENEActiv
Unilever Discover Ltd | Genea (no longer manufactured) | .bin | raw gravitational units | readGenea
Parmay Tech https://www.parmaytech.com/ | Matrix | .bin | raw gravitational units | readMatrix
ActiGraph | ??? | .csv | count data | readActigraphCount
Actiwatch | ??? | .csv and .awd | count data | readActiwatchCount
Actical | ??? | .csv | count data | readActicalCount
Philips Health Band | ??? | .xlsx | count data | readPHBCount
Fitbit | ??? | .json | sleep, steps or calories data | readFitbit
