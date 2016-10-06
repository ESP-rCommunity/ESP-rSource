C      Module Ventilation_Data
c
c Copyright 2001: NRCan Buildings Group
c
C         Implicit None
c ---
c
c Part 1 : Central ventilation system  - HRV or Fans without heat recovery
c
c Central Ventilation System type
C         Integer iCVSType ! (1=None, 2=HRV, 3=Fans with no heat recovery)
c
C         Real HRVTest(2,3)
c i = 1 - "High" test temperature, 2 = "Low"  test temperature
c HRVTest(i,1) = test temperature (Degrees C)
c HRVTest(i,2) = Sensible Effectiveness
c HRVTest(i,3) = Total (Fan + Preheater) Power (watts)
c
C         Real CoolEF ! HRV total cooling efficiency
c
c Central ventilation system supply flow rate (L/s)
C         Real CVSSupplyRate
c Central ventilation system exhaust flow rate (L/s)
C         Real CVSExhaustRate
C         Real PERV5, MCP, HCF, MVAC
c Preheater capacity (watts)
C         Real PreHtrCap
c Fraction of time system is operating
C         Real CVSOpTime
c
c --- Duct Data
C         Integer IntDD(2,3)
c - (integer) Supply/Exhaust Duct data 1=Supply, 2=Exhaust
c    1 = duct location (1..4 List # 190)
c    2 = duct type (1,2,3 List # 191)
c    3 = duct sealing characteristics (1,2,3 List # 192)
c
C         Real RealDD(2,4)
c - (real)    Supply/Exhaust Duct data 1=Supply, 2=Exhaust
c    1 = duct length
c    2 = duct diameter
c    3 = duct insulation level
c    4 = calc. Annual duct heat gain from "room"
c
C         Real SupplyDuctRoomTemp, ExhaustDuctRoomTemp
c
c Central Ventilation System Operating Schedule type (List 549, 1..7)
C         Integer iCVSOpType
c Central Ventilation Sys. Lower limit for temperature control
c Central Ventilation Sys. Upper limit for temperature control
C         Real rCVSLowTempTrip, rCVSHighTempTrip
c
c Main floors temperature
C         Real TMAIN
c --- Calculation results
C         Real SensibleEff, HRVventHLR, HRVairHLR
c Temperature of the air downstream of the supply fan
C         Real CVSSupplyTemp
c HRV power consumption rate (Fans + Preheater) watts
C         Real TotalPower
c HRV exhaust fan power consumption,  Preheater power (watts)
C         Real ExhaustFanPower, PreheaterPower
c HRV supply and exhaust duct heat gain rate (watts)
C         Real HRVSupDuctHG, HRVExhDuctHG
c
c
c Part 2 : "Other" fans (point exhaust/supply)
c
C         Integer iOtherFans !  Code 1 = None,  2 = Other
c
C         Integer OtherFansSupplyRate  ! L/s
C         Integer OtherFansExhaustRate ! L/s
C         Integer OtherFansTotalPower ! watts
c ---
c If there is a crawl space defined, this is the zone number assigned to
c it (else = zero)
c        Integer iCrawl_Space_Zone  Moved to AIM2  BB Sep 27/01

C      End Module Ventilation_Data
c
c ---
c Edit history
c July 9, 2001, Jul 10, 12, Aug 2, 3, 15, 16, Sep 27, Oct 25  : BB
c
c ( JWH remove continuation lines)
      COMMON/  Ventilation_Data /
     & iCVSType ,HRVTest,CoolEF ,CVSSupplyRate,CVSExhaustRate,
     & PERV5, MCP, HCF, MVAC,PreHtrCap,CVSOpTime,IntDD,RealDD,
     & SupplyDuctRoomTemp, ExhaustDuctRoomTemp,
     & iCVSOpType,rCVSLowTempTrip, rCVSHighTempTrip,
     & TMAIN,SensibleEff, HRVventHLR, HRVairHLR,
     & CVSSupplyTemp,TotalPower,ExhaustFanPower, PreheaterPower,
c --- 27-08-2014: Patrice Pinel: Changed HRV ducts for CVS ducts
c	  & HRVSupDuctHG, HRVExhDuctHG,iOtherFans,OtherFansSupplyRate,
     & CVSSupDuctHG, CVSExhDuctHG,iOtherFans,OtherFansSupplyRate,
     & OtherFansExhaustRate,OtherFansTotalPower,ERVTest,
     & CVSZoneFrac,CVSmDotAir, bCVS_OFF,
     & CVSTLow, CVSTHigh,
     & CVSExhaustHRat, CVSSupplyHRat, AmbiantAirHRat
        Integer  iCVSType
        Real  HRVTest(2,3)
        Real  ERVTest(2,4)
        Real  CoolEF
        Real  CVSSupplyRate
        Real  CVSExhaustRate
        Real  PERV5, MCP, HCF, MVAC
        Real  PreHtrCap
        Real  CVSOpTime
        Integer  IntDD(2,3)
        Real  RealDD(2,4)
        Real  SupplyDuctRoomTemp, ExhaustDuctRoomTemp
        Integer  iCVSOpType
        Real  rCVSLowTempTrip, rCVSHighTempTrip
        Real  TMAIN
c ---   Humidity ratio of exhaust air
        Real  CVSExhaustHRat
c ---   Humidity ratio of ambient air
        Real  AmbiantAirHRat
        Real  SensibleEff, HRVventHLR, HRVairHLR
        Real  CVSSupplyTemp
c ---   Humidity ratio of supply air
        Real  CVSSupplyHRat
        Real  TotalPower
        Real  ExhaustFanPower, PreheaterPower
c --- 27-08-2014: Patrice Pinel: Changed HRV ducts for CVS ducts
        !Real  HRVSupDuctHG, HRVExhDuctHG
        Real  CVSSupDuctHG, CVSExhDuctHG
        Integer  iOtherFans
        Integer  OtherFansSupplyRate
        Integer  OtherFansExhaustRate
        Integer  OtherFansTotalPower
c --- Mass flow rate of air in a stream (kg/s)
        Real  CVSmDotAir			
c --- Zone volume fractions
        Real  CVSZoneFrac(MCOM)		
c --- Flag to indicate when temperature control results in the CVS being OFF
        Logical bCVS_OFF
c --- Temperature below which the ERV operates
        Real  CVSTLow
c --- Temperature above which the ERV operates
        Real  CVSTHigh



c ---
c Ventilation data parameters
c ---
c --- Temp. below which frost has impacts (C)
        Real  CVSTFrost
        Parameter (CVSTFrost = -10.)
c --- Max sensible effectiveness at low temp(frac)
        Real  CVSSnsEffLimLT
        Parameter (CVSSnsEffLimLT = 0.8)
c --- Max latent effectiveness at low temp(frac)
        Real  CVSLatEffLimLT
        Parameter (CVSLatEffLimLT = 0.8)
c
c -------------------------------------------------------------
c ERV data
c ---
c Added by: Patrice Pinel
c Date: August 25th 2014
c
c This common block was created to hold the pre-processing variables
c for the central ventilation system ERV model
      COMMON/  ERV_Data /
     & ERVPHrefT,ERVPHDPowDT,
     & ERVFanPow,ERVDFanPowDT,ERVHEESns,ERVDHEESnsDT,
     & ERVMaxHEESnsLT,ERVHEELatH,ERVDHEELatDT,ERVMaxHEELatLT,
     & ERVHEELatC, ERVPreHtr
c Preprocessing variables
c -----------------------
c --- T below which pre-heat is active (C)
        Real  ERVPHrefT
c --- Variation of pre-heat pow. with T (W/K)
        Real  ERVPHDPowDT
c --- Fan pow. in normal operation (W)
        Real  ERVFanPow
c --- Variation of fan pow. with T (low T) (W/K)
        Real  ERVDFanPowDT
c --- Sens HEE in normal operation
        Real  ERVHEESns
c --- Variation of sens HEE with T (low T)
        Real  ERVDHEESnsDT
c --- Maximum sens HEE at low T
        Real  ERVMaxHEESnsLT
c --- Lat HEE in normal heating
        Real  ERVHEELatH
c --- Variation of lat HEE with T (low T)
        Real  ERVDHEELatDT
c --- Maximum lat HEE at low T
        Real  ERVMaxHEELatLT
c --- Lat HEE in cooling
        Real  ERVHEELatC
c --- Flag indicating a pre-heater
        Logical  ERVPreHtr

c These parameters are referenced from the 
c Home Ventilating Institute (HVI) and used in the 	
c Central ventilation system model
c-------------------------------------------------
c      COMMON/  HVI_Data /
c     & HVIAirDensity,HVIAirCp,HVIVapCp,HVIHeatVapW,HVITexhH
c     & HVITexhC,HVITsupC,HVIRHexhH,HVIRHexhC,HVIRHsupH,
c     & HVIRHsupC,HVIStandPres
c --- Density of standard air from HVI (kg/m3)
        Real  HVIAirDensity
        Parameter (HVIAirDensity = 1.2)
c --- Specifc heat of standard air from HVI (J/kg K)
        Real  HVIAirCp
        Parameter (HVIAirCp = 1006.)
c --- Specifc heat of water vapour from HVI (J/kg K)
        Real  HVIVapCp
        Parameter (HVIVapCp = 1805.)
c --- Heat of vaporisation of water from HVI (J/kg)
        Real  HVIHeatVapW
        Parameter (HVIHeatVapW = 2501000.)
c --- Exhaust temp. (T3) for heating tests (C)
        Real  HVITexhH
        Parameter (HVITexhH = 22.)
c --- Exhaust temp. (T3) for cooling tests (C)
        Real  HVITexhC
        Parameter (HVITexhC = 24.)
c --- Supply temp. (T1) for cooling test (C)
        Real  HVITsupC
        Parameter (HVITsupC = 35.)
c --- Exhaust Relative humidity (3) for heating tests (frac)
        Real  HVIRHexhH
        Parameter (HVIRHexhH = 40.)
c --- Exhaust Relative humidity (3) for cooling tests (frac)
        Real  HVIRHexhC
        Parameter (HVIRHexhC = 50.)
c --- Supply Relative humidity (1) for heating tests (frac)
        Real  HVIRHsupH
        Parameter (HVIRHsupH = 75.)
c --- Supply Relative humidity (1) for cooling tests (frac)
        Real  HVIRHsupC
        Parameter (HVIRHsupC = 50.)
c --- Standard pressure (mBar)
        Real  HVIStandPres
        Parameter (HVIStandPres = 1013.25)
c-------------------------------------------------
c END ERV data Module
c-------------------------------------------------