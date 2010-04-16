C
C  DHWR header file: Data structures for DHWR model
C
C------------------------------------------------------

C.....Flag that dhwr is active
      logical bDwhrActive

C.....Filename
      character*72 cDwhrFilename
      
C.....File-handle for I/O
      integer iDwhrFileNumber       

C.....File version number
      integer iDwhrFileVersion 

C.....Maximum number of systems permitted.
      integer iMaxNumDwhr
      parameter ( iMaxNumDwhr = 20 )
      
C.....Maximum number of showers per day
      integer iMaxNumShowersPerDay
      parameter ( iMaxNumShowersPerDay = 24 )      

C.....Number of systems defined.
      integer iDwhrSystemCount
    
C.....System name 
      character*72 cDwhrSystemName(iMaxNumDwhr)

C.....Rated efficiency
      real fDwhrRatedEfficiency(iMaxNumDwhr)
      
C.....System configuration 
      integer iDwhrConfiguation(iMaxNumDwhr)
      
C.....Named constants for system configuration`
      integer iEqualFlow
      integer iUnequalFlow 
      parameter ( iEqualFlow = 1, iUnequalFlow = 2 )
      
C.....Model Coefficients
      real fDwhrModelCoeff_c (iMaxNumDwhr)
      real fDwhrModelCoeff_a0(iMaxNumDwhr)
      real fDwhrModelCoeff_a1(iMaxNumDwhr) 
      real fDwhrModelCoeff_a2(iMaxNumDwhr)
      real fDwhrModelCoeff_a3(iMaxNumDwhr)
   
C.....Default coefficient
      real fDwhrDefaultCoeff_c
      real fDwhrDefaultCoeff_a0 
      real fDwhrDefaultCoeff_a1  
      real fDwhrDefaultCoeff_a2 
      real fDwhrDefaultCoeff_a3 
      
      parameter ( fDwhrDefaultCoeff_c  =   0.9883991 ,
     &            fDwhrDefaultCoeff_a0 =  -9.4510200 ,
     &            fDwhrDefaultCoeff_a1 =  18.841395  ,
     &            fDwhrDefaultCoeff_a2 =   2.2255307 ,
     &            fDwhrDefaultCoeff_a3 =  -3.0458662   )
 
         
   
C.....User specified model Coefficients
      real fDwhrUserCoeff_c (iMaxNumDwhr)
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
      
C.....Shower period start and end times (hour)
      real fShowerPeriodStartHOUR(iMaxNumDwhr)
      real fShowerPeriodEndHOUR(iMaxNumDwhr)
      
C.....Shower start and end times (hour)      
      real fShowerStartHOUR(iMaxNumDwhr,iMaxNumShowersPerDay)
      real fShowerEndHOUR(iMaxNumDwhr,iMaxNumShowersPerDay)
      
C.....Parsing data.       
      integer iMaxTokens
      parameter (iMaxTokens=30)
      character*248 cTokens(iMaxNumDwhr,iMaxTokens) 
      character*248 cValues(iMaxNumDwhr,iMaxTokens)       

C.....Storage arrays that track deficits on water draws which occur 
C.....when the shower water usage exceeds the hourly water draw.                                   
      real fTSDeficitP(iMaxNumDwhr) 
      real fTSDeficitF(iMaxNumDwhr) 
      

      
C.....Common to make data available in memory
      common/drain_water_HR/
     &   bDwhrActive,
     &   iDwhrFileNumber,
     &   cDwhrFilename,
     &   iDwhrFileVersion,
     &   iDwhrSystemCount,
     &   cDwhrSystemName,
     &   iDwhrConfiguation,
     &   fDwhrRatedEfficiency,
     &   fDwhrModelCoeff_c,
     &   fDwhrModelCoeff_a0,
     &   fDwhrModelCoeff_a1,
     &   fDwhrModelCoeff_a2,
     &   fDwhrModelCoeff_a3,
     &   fDwhrUserCoeff_c,
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
     &   fShowerPeriodStartHOUR,
     &   fShowerPeriodEndHOUR,
     &   fShowerStartHour,
     &   fShowerEndHour,
     &   cTokens,
     &   cValues, 
     &   fTSDeficitP, fTSDeficitF
      
      