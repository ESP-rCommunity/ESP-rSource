C********************************************************************************
c Function Declarations

c Ground temperature profile variable names have been changed:
c fMeanGroundTem and bsTGavg become fTemp_Ground_Avg
c fAmpGroundTempVar and bsTGamp becomes fTemp_Ground_Amp
c bsTGps becomes fTemp_Ground_phaseshift

      REAL  fTemp_Ground_Avg  ! annually-averaged soil temperature
      REAL  fTemp_Ground_Amp  ! amplitude of ground-temperature's annual sine wave
      REAL  fTemp_Ground_phaseshift  ! phase lag of ground-temperature's annual sine wave

      REAL  fTemp_Ambient_Amp  ! amplitude of ambient-temperature's annual sine wave
      REAL  fTemp_Ambient_phaseshift  ! phase lag of ground-temperature's annual sine wave

      REAL fC_1  ! used to fit the monthly ground temperature profile to a cosine curve 
      REAL fS_1  ! used to fit the monthly ground temperature profile to a cosine curve 
  
       
C********************************************************************************
c Common Block Declarations

C The common block created for ground temperatures information. These values are calculated 
c once per simulation (/cetc/ground_temp_mains.F/Ground_Temp_Profile) 
c and then stored in the common block.
      COMMON/Ground_Temp/
     &       Temp_Ground_Avg,
     &       Temp_Ground_Amp,
     &       Temp_Ground_phaseshift
     
      REAL  Temp_Ground_Avg  ! annually-averaged soil temperature
      REAL  Temp_Ground_Amp  ! amplitude of ground-temperature's annual sine wave
      REAL  Temp_Ground_phaseshift ! phase lag of ground-temperature's annual sine wave     
c used in mznuma, Ground_Temp_Profile, fDHW_LimitMeanGroundTemp(), 
c iDHW_GroundOffset(), and fDHW_ColdMainTemp()


c The common block created for degree day information in /cetc/ground_temp_mains.F/CLM_DEG_DAY
      COMMON/DD/DEGREE_DAY

      REAL  DEGREE_DAY ! number of annual heating degree days for chosen climate 
c called from mznuma
c used in CLM_DEG_DAY, fTemp_Ground_Avg(), fTemp_Ground_Amp(), and fTemp_Ground_phaseshift()


c The common block created for monthly average ambient temperature in
c /cetc/ground_temp_mains.F/CLM_DEG_DAY
      COMMON/Month_Avg_DB/Temp_Ambient_Avg
      
      REAL  Temp_Ambient_Avg(12) ! Monthly average ambient dry bulb temperature array 
                                 ! for the chosen climate 
c used in fTemp_Ground_Avg(), fC_1(), fS_1() 

c Common block is created for storing the daily dry bulb temperatures in
c /cetc/ground_temp_mains.F/Daily_Dry_Bulb_Temps(iday)
      common/daily_dry_bulb/DBT_day

      REAL  DBT_day(24)  ! Hourly ambient dry bulb temperature for the chosen climate
c called from fTemp_Ambient_Avg and CLM_DEG_DAY  
