C This file is part of the ESP-r system.
C Copyright Energy Systems Research Unit, University of
C Strathclyde, Glasgow, Scotland, 2001-.

C ESP-r is free software. You can redistribute it and/or
C modify it under the terms of the GNU General Public
C License as published by the Free Software Foundation
C (version 2 or later).

C ESP-r is distributed in the hope that it will be useful
C but WITHOUT ANY WARRANTY; without even the implied
C warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
C PURPOSE. See the GNU General Public License for more
C details.


C This file defines ESP-r weather metrics. The idea is that each climate
C metric has an integer index. Data structures of values, names, limits, 
C etc. are all referenced with this integer. Adding a new metric
C is then a simple procedure of defining it in this header and adding 
C code into ESP-r that uses it.

C Index | Metric
C 1     | Dry bulb temperature
C 2     | Diffuse horizontal solar intensity
C 3     | Direct normal solar intensity
C 4     | Global horizonal solar intensity
C 5     | Wind speed
C 6     | Wind direction
C 7     | Relative humidity
C 8     | Total cloud cover
C 9     | Opaque cloud cover
C 10    | Atmospheric pressure

C Parameter definitions. 

C MCM    - Current number of available climate metrics (*24 = record
C          width of climate database)
C NMCM   - Number of values in MCMALL (also the current climate database
C          version number)
C MCMALL - Values of MCM from all previous versions

C NOTE - Whenever the value of MCM is increased in the public release,
C the array MCMALL must be appended with the new value and NMCM must be
C incremented by 1. This maintains back-compatibility for old climate
C databases. The idea is that the ith element in MCMALL is the correct
C value of MCM to use for a version i database.

C CMNAMA - Abreviations (6 characters)
C CMNAMF - Full names (24 characters maximum)
C CMUNIT - Units for values (6 characters maximum)
C CFUNIT - Units for values in climate file (24 characters maximum)
C CFMAX  - Maximum values (in climate file units)
C CFMIN  - Minimum values (in climate file units)

      INTEGER,PARAMETER :: MCM=10

      INTEGER,PARAMETER :: NMCM=2

      INTEGER,PARAMETER :: MCMALL(NMCM)=
     &(/6,10/)

      CHARACTER*6,PARAMETER :: CMNAMA(MCM)=
     &(/'DBTemp',
     &  'DFHSol',
     &  'DRNSol',
     &  'GLHSol',
     &  'WndSpd',
     &  'WndDir',
     &  'RelHum',
     &  'TotCld',
     &  'OpqCld',
     &  'AtmPrs'/)

      CHARACTER*24,PARAMETER :: CMNAMF(MCM)=
     &(/'dry bulb temperature    ',
     &  'diffuse horizontal solar',
     &  'direct normal solar     ',
     &  'global horizontal solar ',
     &  'wind speed              ',
     &  'wind direction          ',
     &  'relative humidity       ',
     &  'total cloud cover       ',
     &  'opaque cloud cover      ',
     &  'atmospheric pressure    '/)

      CHARACTER*6,PARAMETER :: CMUNIT(MCM)=
     &(/'C     ',
     &  'W/m^2 ',
     &  'W/m^2 ',
     &  'W/m^2 ',
     &  'm/s   ',
     &  'deg   ',
     &  '%     ',
     &  'tenths',
     &  'tenths',
     &  'Pa    '/)

      CHARACTER*24,PARAMETER :: CFUNIT(MCM)=
     &(/'tenths C                ',
     &  'W/m^2                   ',
     &  'W/m^2                   ',
     &  'W/m^2                   ',
     &  'tenths m/s              ',
     &  'deg clockwise from north',
     &  '%                       ',
     &  'tenths of sky covered   ',
     &  'tenths of sky covered   ',
     &  'Pa                      '/)

      INTEGER,PARAMETER :: CFMAX(MCM)=
     &(/570,     ! highest ever recorded
     &  1362,    ! solar constant
     &  1362,    ! solar constant
     &  1362,    ! solar constant
     &  1130,    ! highest ever recorded
     &  360,     ! maximum degrees in a circle
     &  100,     ! maximum percentage
     &  10,      ! maximum tenths
     &  10,      ! maximum tenths
     &  120000/) ! matches EPW file limits

      INTEGER,PARAMETER :: CFMIN(MCM)=
     &(/-900,   ! lowest ever recorded
     &  0,      ! cannot be below zero
     &  0,      ! cannot be below zero
     &  0,      ! cannot be below zero
     &  0,      ! cannot be below zero
     &  0,      ! cannot be below zero
     &  0,      ! cannot be below zero
     &  0,      ! cannot be below zero
     &  0,      ! cannot be below zero
     &  31000/) ! matches EPW file limits

C Common block definitions.

C CLMFIL
C CFVER  - Weather file/database version
C CFYEAR - Weather year
C CFLOC  - Location
C CFLAT  - Latitude
C CFLONG - Longitude difference
C CFMCM  - Maximum number of metrics in this weather file (*24 = record width)
C CLMMET
C NCM    - Number of weather metrics
C CMCOL  - Columns in the weather file of each metric (not present if 0)
C CMXST  - Flags indicating which metrics are present
C CLMVAL
C CMIVAL - One day of integer values from weather file, plus an extra
C          time step for future values of the last hour
C CMRVAL - One day of real weather values
C CMDAYR - The day of data currently in CMRVAL, to prevent reading the same data again

      COMMON/CLMFIL/CFVER,CFYEAR,CFLAT,CFLONG,CFMCM,CFLOC
      integer CFVER,CFYEAR,CFMCM
      real CFLAT,CFLONG
      character CFLOC*42

      COMMON/CLMMET/MNCM,NCM,CMCOL(MCM),CMXST(MCM)
      integer MNCM,NCM,CMCOL
      logical CMXST

      COMMON/CLMVAL/CMDAYR,CMIVAL(MCM,25),CMRVAL(MCM,24)
      integer CMDAYR,CMIVAL
      real CMRVAL

C Note - subroutines are defined in commonclm.F, marked with tag <CLMNEW2OLD>,
C to map these commons to existing old data structures. 
C The old data structures should be depreciated in favour of this header.
C This is a work in progress.



