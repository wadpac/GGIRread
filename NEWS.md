# Changes in version 1.0.2 (release date:26-10-2024)

- Added a `NEWS.md` file to track changes to the package.
- Stops interactive calling of `chooseCRANmirror` on `.onAttach` if interactive and CRAN mirror not set GGIR #1141.
- GGIRread version look-up in .onattach() no longer crashes when computer is offline.
- Migrate read function for ActiGraph (csv) and Actiwatch (csv/awd) count data to GGIRread #68.
- Add function for reading Actical (csv) count data #68.
- Add functions for reading and merging Philips Health Band file pairs (xlsx) #68.
- Add functions for reading and merging Fitbit (json) files with sleep, steps, and/or calories #68.


# Changes in version 1.0.1 (release date:03-06-2024)

- Progress bar fixed, issue #63 (credits: John Muschelli)


# Changes in version 1.0.0 (release date:27-03-2024)

- GENEActiv no longer prints error to console when more data is requested 
than is in the file because this is not an error, issue #58



# Changes in version 0.3.3 (release date:24-01-2024)

- When GENEActiv device battery runs low before downloading data the timestamp in the file header will default to 2010-9-16. This is now corrected by using the timestamp from the first page header instead, issue #56


# Changes in version 0.3.2 (release date:05-12-2023)

- Improved handling of failed checksum in .cwa files #53 (credits: Lena Kushleyeva)


# Changes in version 0.3.1 (GitHub-only-release date:11-10-2023)

- Now iterates through cwa files using native cwa blocks instead of 300
sample pages.
- Now properly accounts for unexpected high sampling rate for the imputation.
- Deprecated option to iterate based on timestamps.
- Deprecated readWav function, see #48 for justification.
- This release already appeared on GitHub on 30 August but is now also submitted to CRAN


# Changes in version 0.3.0 (release date:07-08-2023)

- Improved imputation of faulty blocks readAxivity
- Unit test added for AX6 files
- License update to Apache 2.0.


# Changes in version 0.2.10 (GitHub-only-release date:03-08-2023)

- Fixed bug in temperature extraction Axivity cwa files (credits: Lena Kushleyeva)
- readAxivity now able to recognise when AX6 has been configured without 
gyroscope sensor and only provides accerometer data. Fixes #31 (credits: Lena Kushleyeva)
- readAxivity refactored and faster (credits: Lena Kushleyeva)
but also expanded with checksum check and sampling frequency check, 
data blocks are imputed when checks fail and this is logged in the output. 
The net speed improvement is approximately 15\%-20\%.


# Changes in version 0.2.9 (GitHub-only-release date:20-07-2023)

- Replace bug fix to resample function in 0.2.8 and instead fix the 
issue inside readAxivity #33
- Improvements to syntax for readGENEActiv and readGenea following R updates


# Changes in version 0.2.8 (release date:26-05-2023)

- Major bug fix introduced in 0.2.7 release affecting readAxivity #29
- resample function now able to handle timestamps outside block as
a result of large time gaps in cwa files #32


# Changes in version 0.2.7 (release date:17-05-2023)

- Fixes bug in LUX extraction for GENEActiv .bin data (#27)
- Expanded Axivity .wav file header extraction with alternative method
for sampling rate extraction (#26)
- Speeding up readAxivity function by approximately 75 percent.


# Changes in version 0.2.6 (release date:05-12-2022)

- New CRAN release, only minor improvements to syntax


# Changes in version 0.2.5 (GitHub-only-release date:09-11-2022)

- readWav now correctly identifies the header and first timestamp


# Changes in version 0.2.4 (release date:09-10-2022)

- Addressing POSIX errors in R-devel.
- Speeding up readAxivity function by approximately 40 percent.
- Adding readWav function for reading accelerometer data stored in
wav(Audio) format. This function was formerly known as g.wavread in the
GGIR package but has been migrated here to make the GGIR package lighter.


# Changes in version 0.2.3 (release date:29-09-2022)

- Changed POSIXlt to character conversion to be compatible with R-devel
update svn revision r82904 (2022-09-24 19:32:52)
- Tidying up namespace references
- Added zzz function to auto-check onattach package version relative to CRAN


# Changes in version 0.2.2 (release date:31-08-2022)

- Now also extracting GENEActiv RecordingID, Handedness, DeviceLocation and DeviceModel from header
- Removed skin_on_cran() calls in unit test
- Improved description for resample function
- Fixed errors resulting from how lux values are read (issue #12)


# Changes in version 0.2.1 (release date:16-08-2022)

- Addressing request from CRAN to revise AxivityNumUnpack


# Changes in version 0.2.0 (release date:12-08-2022)

- First release with functionality for reading Axivity .cwa, GENEActiv .bin, and Genea, .bin data.


