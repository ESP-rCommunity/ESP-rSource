C tdf.h Parameters for TDF program, July 1995 version.

C Allowed number of tabular entries in the TDF file.
      PARAMETER (MIT=36)

C Allowed columns of tabular data in TAB file.
      PARAMETER (MTABC=12)

C Allowed number of widgets entries in the WIDGET file.
      PARAMETER (MIW=24)

C Maximum number of help lines associated with a generic item.
      PARAMETER (MHEL=5)

C Maximum number of non-static bits in a generic item period data set.
      PARAMETER (MBITS=9)

C Maximum number of text fields.
      PARAMETER (MGAT=8)

C Maximum number of real fields.
      PARAMETER (MGAR=12)

C Maximum number of integer fields.
      PARAMETER (MGAI=8)

C Maximum number of diary periods in a day.
      PARAMETER (MDPERD=24)

C Maximum number of text alternatives in a text field.
      PARAMETER (MALT=6)

C Maximum number of timesteps in a day & hour.
      PARAMETER (MHTS=60)
      PARAMETER (MDTS=MDPERD*MHTS)
