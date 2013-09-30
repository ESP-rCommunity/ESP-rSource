C This file is part of the ESP-r system.
C Copyright CANMET Energy Technology Centre 
C Natural Resources Canada, Government of Canada
C 2004. Please Contact Ian Beausoliel-Morrison for details 
C concerning licensing.

C ESP-r is free software.  You can redistribute it and/or
C modify it under the terms of the GNU General Public
C License as published by the Free Software Foundation 
C (version 2 or later).

C ESP-r is distributed in the hope that it will be useful
C but WITHOUT ANY WARRANTY; without even the implied
C warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
C PURPOSE. See the GNU General Public License for more
C details.

C You should have received a copy of the GNU General Public
C License along with ESP-r. If not, write to the Free
C Software Foundation, Inc., 59 Temple Place, Suite 330,
C Boston, MA 02111-1307 USA.

      Module Ventilation_Data
c
c Copyright 2001: NRCan Buildings Group
c
         Implicit None
c ---
c
c Part 1 : Central ventilation system  - HRV or Fans without heat recovery
c
c Central Ventilation System type
         Integer iCVSType ! (1=None, 2=HRV, 3=Fans with no heat recovery)
c
         Real HRVTest(2,3)
c i = 1 - "High" test temperature, 2 = "Low"  test temperature
c HRVTest(i,1) = test temperature (Degrees C)
c HRVTest(i,2) = Sensible Effectiveness
c HRVTest(i,3) = Total (Fan + Preheater) Power (watts)
c
         Real CoolEF ! HRV total cooling efficiency
c
c Central ventilation system supply flow rate (L/s)
         Real CVSSupplyRate
c Central ventilation system exhaust flow rate (L/s)
         Real CVSExhaustRate
         Real PERV5, MCP, HCF, MVAC
c Preheater capacity (watts)
         Real PreHtrCap
c Fraction of time system is operating
         Real CVSOpTime
c
c --- Duct Data
         Integer IntDD(2,3)
c - (integer) Supply/Exhaust Duct data 1=Supply, 2=Exhaust
c    1 = duct location (1..4 List # 190)
c    2 = duct type (1,2,3 List # 191)
c    3 = duct sealing characteristics (1,2,3 List # 192)
c
         Real RealDD(2,4)
c - (real)    Supply/Exhaust Duct data 1=Supply, 2=Exhaust
c    1 = duct length
c    2 = duct diameter
c    3 = duct insulation level
c    4 = calc. Annual duct heat gain from "room"
c
         Real SupplyDuctRoomTemp, ExhaustDuctRoomTemp
c
c Central Ventilation System Operating Schedule type (List 549, 1..7)
         Integer iCVSOpType
c Central Ventilation Sys. Lower limit for temperature control
c Central Ventilation Sys. Upper limit for temperature control
         Real rCVSLowTempTrip, rCVSHighTempTrip
c
c Main floors temperature
         Real TMAIN
c --- Calculation results
         Real SensibleEff, HRVventHLR, HRVairHLR
c Temperature of the air downstream of the supply fan
         Real CVSSupplyTemp
c HRV power consumption rate (Fans + Preheater) watts
         Real TotalPower
c HRV exhaust fan power consumption,  Preheater power (watts)
         Real ExhaustFanPower, PreheaterPower
c HRV supply and exhaust duct heat gain rate (watts)
         Real HRVSupDuctHG, HRVExhDuctHG
c
c
c Part 2 : "Other" fans (point exhaust/supply)
c
         Integer iOtherFans !  Code 1 = None,  2 = Other
c
         Integer OtherFansSupplyRate  ! L/s   ** REAL ** ???
         Integer OtherFansExhaustRate ! L/s   ** REAL ** ???
         Integer OtherFansTotalPower ! watts  ** REAL ** ???
c ---
c If there is a crawl space defined, this is the zone number assigned to
c it (else = zero)
c        Integer iCrawl_Space_Zone  Moved to AIM2  BB Sep 27/01

      End Module Ventilation_Data
c
c ---
c Edit history
c July 9, 2001, Jul 10, 12, Aug 2, 3, 15, 16, Sep 27, Oct 25  : BB
c
