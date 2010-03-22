C
C  DHWR header file: Data structures for DHWR model
C
C------------------------------------------------------

C.....Flag that dhwr is active
      logical bDwhrActive

C.....Filename
      character*72 cDwhrFilename

C.....File version number
      integer iDwhrFileVersion 

C.....Maximum number of systems permitted.
      integer iMaxNumDwhr
      parameter ( iMaxNumDwhr = 20 )

    
C.....System name 
      character*72 cDwhrSystemName(iMaxNumDwhr)

C.....Rated efficiency
      real fDwhrRatedEfficiency(iMaxNumDwhr)
      
C.....Model Coefficients
      real fDwhrModelCoeff_a0(iMaxNumDwhr)
      real fDwhrModelCoeff_a1(iMaxNumDwhr) 
      real fDwhrModelCoeff_a2(iMaxNumDwhr)
      real fDwhrModelCoeff_a3(iMaxNumDwhr)
   
C.....User specified model Coefficients
      real fDwhrUserCoeff_a0(iMaxNumDwhr)
      real fDwhrUserCoeff_a1(iMaxNumDwhr) 
      real fDwhrUserCoeff_a2(iMaxNumDwhr)
      real fDwhrUserCoeff_a3(iMaxNumDwhr)
   
C.....Ground temperature calculation method
      integer iDwhrGroundTempCalc(iMaxNumDwhr)
      
C.....Named constants for various methods
      integer iMooreModel
      integer iUserSpecified
      integer iEsprStandard

      parameter( iMooreModel     = 1, 
     &           iUserSpecified  = 2, 
     &           iEsprStandard   = 3  )
     
C.....Specfied ground temperature (if any, oC)
      real fGroundTempSpecC(iMaxNumDwhr)
      
C.....Shower supply temperature (oC)
      real fShowerSupplyTempC(iMaxNumDwhr)
      
C.....Drain temperature (lpm)
      real fDrainWaterTempC(iMaxNumDwhr)
      
C.....Shower Flow Rate (lpm) 
      real fShowerFlowRateLPM(iMaxNumDwhr)
      
C.....Shower Duration (minutes)      
      real fShowerDurationMIN(iMaxNumDwhr)
      
C.....Number of showers per day 
      integer iNumShowersPerDay(iMaxNumDwhr)
      
C.....Shower Start Time (hour)
      real fShowerStartTimeHOUR(iMaxNumDwhr)
      real fShowerEndTimeHOUR(iMaxNumDwhr)

C.....Common to make data available in memory
      common/drain_water_HR/
     &   bDwhrActive,
     &   cDwhrFilename,
     &   iDwhrFileVersion,
     &   cDwhrSystemName,
     &   fDwhrRatedEfficiency,
     &   fDwhrModelCoeff_a0,
     &   fDwhrModelCoeff_a1,
     &   fDwhrModelCoeff_a2,
     &   fDwhrModelCoeff_a3,
     &   fDwhrUserCoeff_a0,
     &   fDwhrUserCoeff_a1,
     &   fDwhrUserCoeff_a2,
     &   fDwhrUserCoeff_a3,
     &   iDwhrGroundTempCalc,
     &   fGroundTempSpecC,
     &   fShowerSupplyTempC,
     &   fDrainWaterTempC,
     &   fShowerFlowRateLPM,
     &   fShowerDurationMIN,
     &   iNumShowersPerDay,
     &   fShowerStartTimeHOUR,
     &   fShowerEndTimeHOUR
      