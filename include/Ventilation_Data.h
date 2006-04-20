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
     & HRVSupDuctHG, HRVExhDuctHG,iOtherFans,OtherFansSupplyRate,
     & OtherFansExhaustRate,OtherFansTotalPower
        Integer  iCVSType
        Real  HRVTest(2,3)
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
        Real  SensibleEff, HRVventHLR, HRVairHLR
        Real  CVSSupplyTemp
        Real  TotalPower
        Real  ExhaustFanPower, PreheaterPower
        Real  HRVSupDuctHG, HRVExhDuctHG
        Integer  iOtherFans
        Integer  OtherFansSupplyRate
        Integer  OtherFansExhaustRate
        Integer  OtherFansTotalPower
