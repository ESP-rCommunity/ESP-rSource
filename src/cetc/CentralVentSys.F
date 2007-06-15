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
c
      Subroutine MECH_VENT_INIT (  )
c
c Copyright 2001: NRCan Buildings Group
c Created October 4, 2001
c Edited: BB Oct 4, 15, Nov 2
c ---
      
c
      Implicit None
#include "aim2_parameters.h"
#include "aim2_common.h"
#include "Ventilation_Data.h"
      Integer iComp
      Integer iFun
      Real T, FlowRate
      Character Msg*128
c ---
      iFun = 0 ! Initialize

      FlowRate = Max ( CVSSupplyRate, CVSExhaustRate ) / 1000. ! L/s --> m3/s
c
c In HOT2000, the specific heat of air has been 1005 J/Kg Deg C, and the air density
c has been 1.177 Kg/m3.  These values are also used here.
      MCP = FlowRate * 1005. * 1.177
c
      If ( iCVSType .eq. 2 ) Then ! HRV
c
         Call HRV ( iFun, T )
c
      Else If ( iCVSType .eq. 3 ) Then ! Fans, no Heat recovery
c
         Call FANS_NO_HR ( iFun, T )
c
      Endif
c ---
c     Write (Msg,'(a,2f8.1)') 'MECH_VENT_INIT - HRVeff',
c    x HRVTest(1,2), HRVTest(2,2)
c     Call WINMSG ( Msg )
      Return
      END
c ---
      Subroutine MECH_VENT_CONTROL ( iComp, ZONE_infil_cond )
c
c Copyright 2001: NRCan Buildings Group
c Created August  15, 2001
c Edited: BB Aug 15, 16, 17, Oct 4, 10, 12, 23, 25, Nov 1, 2
c ---

      
c ---
      IMPLICIT NONE
#include "building.h"
#include "aim2_parameters.h"
#include "aim2_common.h"
#include "Ventilation_Data.h"
      COMMON/PREC2/VOL
      REAL VOL(MCOM)
      COMMON/C1/NCOMP,NCON
      INTEGER NCOMP,NCON

      COMMON/FVALA/TFA,QFA
      REAL TFA(MCOM),QFA(MCOM)

       COMMON/CLIMI/QFP,QFF,TP,TF,QDP,QDF,VP,VF,DP,DF,HP,HF
       REAL QFP,QFF,TP,TF,QDP,QDF,VP,VF,DP,DF,HP,HF
c
c     Implicit None
      INTEGER J
      Integer iComp
      Real ZONE_infil_cond
      Integer iFun, i, iCycle
      Real T, HLR, DeltaT, ZoneFraction
      Character Msg*128
      Logical ExitNOW
      
      save J, iFun, i, iCycle
      save T, HLR, DeltaT, ZoneFraction
      save Msg
      save ExitNOW
      
      Data iCycle / 9990 /
      If ( iCycle .lt. 1 ) Then
         Write (Msg,'(a,6f8.2)') 'MECH_VENT_CONTROL - ZF',
     & ( VOL(i), i = 1, 6 )
         Call WINMSG ( Msg )
         Write (Msg,'(a,6f8.2)') 'MECH_VENT_CONTROL - AV',AIM2_house_vol
         Call WINMSG ( Msg )
         Write (Msg,'(a,i3)') 'MECH_VENT_CONTROL - Az',
     & AIM2_num_zones_infil
         Call WINMSG ( Msg )
         Write (Msg,'(a,6i3)') 'MECH_VENT_CONTROL - zz',
     & ( AIM2_zone_infil(j), j = 1, 6)
         Call WINMSG ( Msg )
      Endif
c ---
      ZONE_infil_cond = 0.
c
c The Central Ventilation System (CVS) model calculates the ventilation
c heat loss rate for the whole house but we are currently within the
c zone loop. Therefore, execute the algorithm only for the model's first
c zone. This will calculate the heat loss rate rates for the whole
c house. Following this, apportion the losses to the individual zones.
c ---
      If ( iComp .eq. 1 ) Then  ! First zone, perform CVS calcs.
c
         Ifun = 1
         TMAIN = TFA(AIM2_ref_zone) ! Indoor temp
c
         T = TF ! "future" outdoor air temp
c ---
         If ( iCVSType .eq. 2 ) Then ! HRV
c
            i = Max ( 1, IntDD(1,1) ) ! Zone in which supply duct is located
            SupplyDuctRoomTemp   = TFA(i)
            i = Max ( 1, IntDD(2,1) ) ! Zone in which exhaust duct is located
            ExhaustDuctRoomTemp  = TFA(i)
c
            Call HRV ( iFun, T )
c
         Else If ( iCVSType .eq. 3 ) Then ! ! Fans, no Heat recovery
c
            Call FANS_NO_HR ( iFun, T )
c
c ---
         Endif
c
      Endif ! End of Zone 1 calculations
c ---
      If ( iComp .ne.  AIM2_CrawlSpace_zone ) Then
c
c Apportion ventilation to each zone.  For now just distribute the air flow
c in proportion to each room's volume
         ZoneFraction = 0.
c
         Do j = 1, AIM2_num_zones_infil
c
            If ( AIM2_zone_infil(j) .eq. iComp ) Then
c
               ZoneFraction = VOL(iComp) / AIM2_house_vol
               Exit
c 
            Endif
         End Do
c
c --- Air heat loss rate for zone
c
         HLR = MCP * ( TFA(iComp) - CVSSupplyTemp ) * ZoneFraction
c
c --- Convert air heat loss rate into an equivalent conductance (to outdoors)
c
         DeltaT = TFA(iComp) - T
         If ( Abs(DeltaT) .gt. 0.05 ) 
     &              ZONE_infil_cond = HLR / DeltaT
          
c
c --- Supply Duct heat gain
c
         If ( iComp .eq. IntDD(1,1) ) Then
c Supply duct is in zone
            If ( Abs(DeltaT) .gt. 0.05 ) ZONE_infil_cond =
     &                 ZONE_infil_cond + HRVSupDuctHG / DeltaT
         Endif
c
c --- Exhaust Duct heat gain
c
         If ( iComp .eq. IntDD(2,1) ) Then
c Exhaust duct is in zone
            If ( Abs(DeltaT) .gt. 0.05 ) ZONE_infil_cond =
     &                 ZONE_infil_cond + HRVExhDuctHG / DeltaT
         Endif
c ---
      Endif
      If ( iCycle .lt. 22 ) Then
         Write (Msg,'(a,7f8.2,2i5,f8.4)') 'MECH_VENT_CONTROL',
     & ZONE_infil_cond, HLR, TFA(iComp), TF, SupplyDuctRoomTemp,
     & ExhaustDuctRoomTemp, CVSSupplyTemp,
     & AIM2_ref_zone, iComp, ZoneFraction
         Call WINMSG ( Msg )
      Endif
c
      iCycle = iCycle + 1
      Return
      END
c$Message: ' File:Hrv.For'
      Subroutine HRV ( iFun, T )
c
c          Model for Heat Recovery Ventilator
c
c iFun    - 0 = Initialize
c         - 1..12 : Calculate for one time step during month
c
c OCT/85 - Jul/91
c
c Applicable only to an HRV (Fans only done separately)
c Develop Duct model Feb/93, Feb 4, 22-26, Mar 12-22, Apr 18
c Nov 24/93, Feb 17/94, Mar 28/95, Apr 6, November 20, 2000
c Revise calling methods (for ESPr) Jul 9/01, Jul 10, 12, 13, 26
c Aug 2, 15, 16, Oct 4, 25, Nov 1
c ---
c
c      Revised HRV model  UNIES 807-2  Feb 19/93  BcB
c
c  Tod ______ Tpre______  T7                                T2
c     |      |   |Pre-  |    ___________________________
c  ==>| Duct |=>=|Heater|==>|                Ts |Supply |====> House
c     |______|   |______|   |                   | Fan   |       Supply
c      Troom                |                   |_______|
c      ______               |_______                    |
c     |      |           Tx |Exhaust|                   |   T3
c  <==| Duct |========<=====| Fan   | Te                |==<== House
c  Tex|______|              |_______|___________________|       Exhaust
c
c
c ---

C Module for tracking electrical draws of HVAC equipment that is simulated
C in the building thermal domain (ie. the H3K HVAC models invoked below).
C This is used by the fuel cell controller to determine the house's total
C electrical demand during each time-step in the plant domain.

c
      Implicit None
#include "Ventilation_Data.h"
#include "SOFC.h"

      Integer iFun
      Real T
c ---
      Logical MonVar
      Logical PreHtr, Type2
      Integer m, i
c     Integer IO
      Real x, TENDEG, U4, U5
c ---
      Real DUCTHL, sDuctHG, eDuctHG
      Real EffHT, EffLT, dEffdT, FPc1, FPc2, PHrefT
      Real FanPwr, PrePwr, eHRVmax, eHRV
      Real Tpre, T7, Ts, Te, T2, Tx, T3, Tex
      Real OpTime, Tlow, Thigh
      Character Msg*128
C.....H3Kreports.(HVAC_ELEC_DEC).start......................
C.....DECLARATIONS for H3Kreporting object
      CHARACTER*128 H3K_rep_NAME
      INTEGER LNBLNK
C.....H3Kreports.(HVAC_ELEC_DEC).end........................
c ---
      Save MonVar, PreHtr, Type2, m, i, x, TENDEG, U4, U5
c ---
      Save sDuctHG, eDuctHG
      Save EffHT, EffLT, dEffdT, FPc1, FPc2, PHrefT
      Save FanPwr, PrePwr, eHRVmax, eHRV
      Save Tpre, T7, Ts, Te, T2, Tx, T3, Tex
      Save OpTime, Tlow, Thigh
      Save Msg
      Save H3K_rep_NAME
      
c
      If ( iFun .eq. 0 ) Then ! HRV INITIALIZATION
c
c The temperature below which HRV eff and powers vary
         TENDEG = -10.
c
         PreHtr = ( PreHtrCap .gt. 1. ) ! Pre-Heater
         U4 =  HRVtest(1,3) ! PHTKW1
         U5 =  HRVtest(2,3) ! PHTKW2
         FPc1 = U4 * 0.5
         T7 = TENDEG - HRVtest(2,1) ! T(-10) - T(Low Temperature)
c
c HRV effectiveness at 0 C (high temperature)
         x = MCP * T3
         EffHT = HRVtest(1,2)
c
c HRV effectiveness at "-25" C
         EffLT = HRVtest(2,2)
c
         If ( PreHtr ) Then
c
c Pre-Heater power model assumes a thermostat downstream attempts
c to hold temperature above -10 C (TENDEG).
c (But, the reference temperature calibrates power output at -25 C)
c
            PHrefT = HRVtest(2,1) + ( U5 - U4 ) / MCP
c
c Pre-heater fan power is constant : FP = FPc1 - FPc2 * ( T - TENDEG )
c
            FPc2 = 0.
c
         Else ! No Pre-heater
c
            FPc2 = 0.5 * ( U5 - U4 ) / T7
            PHrefT = 0.
         Endif
c
c Rate of change of HRV effectiveness with temperature (below TENDEG)
         dEffdT = ( EffHT - EffLT ) / T7
c
c Limiting effectiveness for a "Type 2" HRV
         Type2 = ( dEffdT .lt. -0.00001 )
         If ( Type2 ) eHRVmax = aMax1 ( 80., EffLT )
c ---
c2340 Format (' Hrv ini',1P7E14.5)
c        Write (Msg,2340) U4, U5, FPc1, FPc2, PHrefT
c        Call WINMSG (Msg)
c        Write (Msg,2340) EffHT, EffLT, dEffdT, MCP
c        Call WINMSG (Msg)
c
c --- Initialize duct data
         x = DUCTHL ( 0, T, x )
c Operating mode
c Central Ventilation System Operating Schedule type (List 549, 1..7)
c
         i = iCVSOpType ! GenDes(42)
         MonVar  = ( i .eq. 2 )
         If ( i .eq. 3 ) Then
c Central Ventilation Sys. Lower limit for temperature control
            Tlow  = rCVSLowTempTrip ! GenDat(44)
c Central Ventilation Sys. Upper limit for temperature control
            Thigh = rCVSHighTempTrip !GenDat(45)
         Else ! No temperature dependant operation
            Tlow  = -9999.
            Thigh = -Tlow
         Endif
c ---
      Else If ( iFun .gt. 0 .and. iFun .le. 12 ) Then ! During month m
c
         m = iFun
         OpTime = CVSOpTime
c ---
c
c T = Outdoor air temperature
c
         If ( T .gt. Tlow .and. T .lt. Thigh ) Then
c
c Current "house" air temperature (as delivered to HRV return ducts)
            T3 = TMAIN ! Local Tmain
c
c Supply duct gain (into HRV system)
            sDuctHG = DUCTHL ( 1, T, Tpre )
c
c Supply air temperature upstream of pre-heater
c  Tpre = calculated by DUCTHL
c Pre-heater
            If ( PreHtr ) Then
                  PrePwr = aMin1 ( aMax1 ( 0.,
     x MCP * ( PHrefT - Tpre ) ), PreHtrCap )
                  T7 = Tpre + PrePwr / MCP
c
            Else
c T7 = air temperature at inlet to HRV
               T7 = Tpre
c Pre-heater power
               PrePwr = 0.
            Endif
c
c HRV effectiveness
            If ( T7 .lt. TENDEG ) Then
c
               eHRV =  aMax1 ( EffHT + dEffdT * ( T7 - TENDEG ), 0. )
c For a Type 2, max eff is limited
               If ( Type2 ) eHRV = aMin1 ( eHRV, eHRVmax )
c
c Fan power varies with temperature
               FanPwr = aMax1 ( 0., FPc1 - FPc2 * ( T - TENDEG ) )
c ---
            Else ! At or above TENDEG
c
c Fan power (supply and exhaust) are constant
               FanPwr = FPc1
c Effectiveness of HRV above TENDEG is constant
               eHRV = EffHT
c
            Endif
c
c Temperatures at other stations
c
            Tx = eHRV * ( T3 - T7 ) * 0.01 ! HX delta T
c
            Ts = T7 +  Tx ! exit heat exchanger on s.s.
            T2 = Ts + FanPwr / MCP ! supply to house
            Te = T3 - Tx ! exit heat exchanger on e.s.
            Tx = Te + FanPwr / MCP ! entrance to exhaust duct
c Exhaust duct gain
            eDuctHG = DUCTHL ( 2, Tx, Tex )
c Sensible Heat recovery efficiency
            x = ( MCP * (T3 - T) + FanPwr )
            If ( Abs(x) .lt. 1.e-4 ) x = 1.e-4 ! Zero divide protect
            x = 100. *
     x ( MCP * (T2 - T) - (FanPwr+PrePwr+sDuctHG+eDuctHG) ) / x
c
c not correct above TMAIN (actually heating BP), but will be ignored
c !!!
            SensibleEff = aMin1 ( aMax1 (x, 0.), 100. )
c
            HRVairHLR = MCP * ( T3 - T2 ) * OpTime
c
c The temperature of the air stream at the outlet of the supply fan (which is
c usually dumped into the furnace plenum.
            CVSSupplyTemp = T2
c
c HRV ventilation air heat loss rate
            HRVventHLR = HRVairHLR + ( sDuctHG + eDuctHG ) * OpTime
c
            ExhaustFanPower  = FanPwr
            PreheaterPower = PrePwr
            HRVExhDuctHG = eDuctHG * OpTime
            HRVSupDuctHG = sDuctHG * OpTime
c Summary data
            TotalPower = FanPwr + FanPwr + PrePwr

C Record the electrical draw (W) by the HRV fans and pre-heater for use by the
C fuel cell controller.
            FCctl_HRV_draw = TotalPower

c
c           If (Dbg) Write (IO,1001) i, 
c    x ( 0. )  , T, Tpre, T7, PrePwr,
c    x eHRV, SensibleEff, FanPwr, Ts, T2, Te, Tx, Tex,
c    x sDuctHG, eDuctHG, T3
c
         Else ! Not operating
c           If (Dbg) Write (IO,1001) i
c
            SensibleEff = 0.
            HRVventHLR = 0.
            ExhaustFanPower = 0.
            PreheaterPower = 0.
            HRVExhDuctHG = 0.
            HRVSupDuctHG = 0.
            TotalPower = 0.
         Endif


C.....H3Kreports.(HVAC_ELEC_HRV).start......................
C........Log HRV  component electric load:
C........-> Set name
         WRITE(H3K_rep_NAME,'(A)') 'plant/CETC_HVAC/HRV/elec_load'        
C.,......-> update meta-data
         call REP_SET_META(
     &       H3K_rep_NAME(1:LNBLNK(H3K_rep_NAME)),
     &       'units',
     &       '(W)'
     &       )

C.......-> update data 
         call REP_REPORT(
     &        TotalPower,
     &        H3K_rep_NAME(1:LNBLNK(H3K_rep_NAME))
     &        )
C.....H3Kreports.(HVAC_ELEC_HRV).end.......................


      CALL H3KSTORE_HRV_DATA !This function stores monthly HRV values for sl5 output
c
c1001 Format ( i3, f9.5, 3f8.3, f8.1, 2f8.2, f8.1, 5f8.3,2f8.1,f8.3)
c
      Endif
c
      Return
      END
c$Message: ' File:FaNoHR.For'
      Subroutine FANS_NO_HR ( m, T )
c
c Monthly energy model for central Fan system (no heat recovery)
c
c m       - Function # 0=Initialization, >0 Time step
c Dbg     - Diagnostics output flag (H2k only)
c T       - Outdoor air temperature
c
c Revised model  UNIES 807-2  Feb 24/93  BcB
c Feb 25, Mar 15, Apr 18, Nov 24, Feb 17/94, Mar 26/95, Mar 28, Apr 6
c Revise calling methods (for ESPr) Jul 24/01, Aug 15, 16, Oct 25
c ---
C      USE Ventilation_data
c ---
      Implicit None
#include "Ventilation_Data.h"
      Integer m
      Real T
c ---
      Real Tlow, Thigh
      Real sFP, FanPwr, x
      Save Tlow,Thigh,sFP,FanPwr,x
c
c --- Initialization
c
      If ( m .lt. 1 ) Then
c
         If ( iCVSOpType .eq. 3 ) Then
            Tlow  = rCVSLowTempTrip  ! low  temp trip
            Thigh = rCVSHighTempTrip ! high temp trip
         Else
            Tlow  = -9999. ! works for terrestial planets
            Thigh = +9999.
         Endif
c
c Fan power is constant
         FanPwr = HRVtest(1,3)
c If the system is not balanced, the powers for supply and exhaust
c are estimated from the flows.
         x = FanPwr / ( CVSExhaustRate + CVSSupplyRate )
         sFP = CVSSupplyRate * x ! Supply fan power
c
c ---
      Else
c
c
         If ( T .gt. Tlow .and. T .lt. Thigh ) Then
c
c Ventilation air heat loss rate
c
c Temperature of air downstream of the supply fan
            CVSSupplyTemp = T + sFP / MCP
c
            HRVventHLR = ( MCP * ( TMAIN - T ) -
     x sFP ) * CVSOpTime
c
         Else ! Not operating
c
            HRVventHLR = 0.
c
            CVSSupplyTemp = T
c
         Endif
c ---
      Endif
c ---
      Return
      END
c$Message: ' File:DuctHL.For'
      Real Function DUCTHL ( i, T, Tds )
c
c Duct Heat loss Calculation
c     i = 1 - Ventilation supply duct
c       = 2 -    "        exhaust "
c       = Neither : initialization
c
c      (as a function of upstream air temperature Deg C)
c     Return value is heat loss rate in watts
c
c  Job 807-2 Feb 4/93  BcB, Feb 25-28, Mar 31, Feb 17/94, Feb 20/95
c  July 9, 2001, Jul 10, Aug 14, 16
c ---

c
      Implicit None
#include "Ventilation_Data.h"
      Real T, Tds
      Real Fleak, Diam, L, HTC, AvVel, CnstDD(2,3)
      Integer i, j, Duct
      Real HLcoef(3,4)
c ---
      
      Save Fleak, Diam, L, HTC, AvVel, CnstDD,  j, Duct
      Save HLcoef
      
c ---
c  Flexible                           * V    const    * R    Cl
      Data (HLcoef(1,j), j = 1, 4) / 0.095,  1.000,  0.950,  3. /
c Sheet metal with liner
      Data (HLcoef(2,j), j = 1, 4) / 0.090,  0.237,  1.222,  8. /
c Ext. insulated Sheet metal
      Data (HLcoef(3,j), j = 1, 4) / .0075,  1.000,  1.148, 30. /
c ---
      If ( i .lt. 1 .or. i .gt. 2 ) Then ! Initialization
         Do Duct = 1, 2
            j = IntDD(Duct,3) ! leakage characteristics
            L = RealDD(Duct,1) ! Length (m)
            Diam = RealDD(Duct,2) ! Diameter (mm)
c Leakage rate L/s
            Fleak = 5.54815e-5 * HLcoef(j,4) * Diam * L
c Supply duct leaks at PreHeater, Exhaust leaks at HRV outlet port
            AvVel = CVSSupplyRate - Fleak
c Average flow velocity in duct m/s
            AvVel = 1273.24 * AvVel / ( Diam * Diam )
            j = IntDD(Duct,2) ! Type
            HTC = HLcoef(j,1) * AvVel +
     x 1. / ( HLcoef(j,2) + HLcoef(j,3) * RealDD(Duct,3) )
c coefficient for conduction heat loss == y
            CnstDD(Duct,1) = 0.60501 * AvVel * Diam / ( HTC * L )
c coefficient for conduction heat loss eqn == UPL/1000 (P in mm)
            CnstDD(Duct,2) = 3.14159e-3 * HTC * Diam * L
c coefficient for leakage heat loss eqn == 1.204 * 1005 / 1000
            CnstDD(Duct,3) = Fleak ! 1.21002
         End Do
         DUCTHL = HTC ! just to return something
      Else
         Duct = i
c
         If ( Duct .eq. 1 ) Then ! Supply duct
            Diam = SupplyDuctRoomTemp
         Else ! Exhaust duct
            Diam = ExhaustDuctRoomTemp
         Endif
c Temperature at duct outlet
         L = ( T * ( CnstDD(Duct,1) - 1. ) + Diam + Diam ) /
     x             ( CnstDD(Duct,1) + 1. )
c Duct conduction heat gain rate (watts)
         AvVel = CnstDD(Duct,2) * ( Diam - ( T + L ) * 0.5 )
c
         If ( Duct .eq. 1 ) Then ! Supply duct
c Downstream temperature (after mixing duct + room air)
            Tds = L + CnstDD(Duct,3) * ( Diam - L ) /
     x                                 CVSSupplyRate
            HTC = Diam - Tds
         Else ! Exhaust duct
            Tds = L ! no temp change
            HTC = Diam - T
         Endif
c Duct leakage heat gain rate (watts)
         HTC = 1.21002 * CnstDD(Duct,3) * HTC
c Total heat gain rate
         DUCTHL = HTC + AvVel
      Endif
c
      Return
      END
c ===
      Subroutine WINMSG ( String )
c
c Send a string to the "console" of ESP-r
c
c Copyright 2001: NRCan Buildings Group
c Created October 10, 2001
c Edited: BB Oct 10
c ---
      Implicit None
      Character String*(*)
c
      Integer IUOUT, IUIN
      Common /OUTIN/ IUOUT, IUIN
c ---
      Call edisp ( IUOUT, String )
c
      Return
      END

c*****************************************************************
c H3KSTORE_HRV_ENERGY

c Created by: Phylroy A. Lopez
c Initial Creation Date:  February 13, 2002.
c Copyright CETC 2002.

c This routine gathers the energy used by the HRV systems

c INPUTS:
c OUTPUTS:
c	None. Sums up Monthly HRV energy in H3KSTORE_DATA module

      SUBROUTINE H3KSTORE_HRV_DATA()
           IMPLICIT NONE
#include "building.h"
#include "hvac_parameters.h"
#include "h3kstore_parameters.h"
#include "h3kstore_common.h"
#include "Ventilation_Data.h"
      COMMON/PERS/ISD1,ISM1,ISD2,ISM2,ISDS,ISDF,NTSTEP
      common/simtim/ihrp,ihrf,idyp,idyf,idwp,idwf,nsinc,its
      INTEGER ISD1,ISM1,ISD2,ISM2,ISDS,ISDF,NTSTEP
      INTEGER ihrp,ihrf,idyp,idyf,idwp,idwf,nsinc,its
      INTEGER IDAYN,iMonth
      LOGICAL bInStartup           !Function to see if still in start up mode.



c This IF ensures start-up is over.
      IF (bInStartUp()) then
        RETURN
      ELSE
      CALL EDAYR(idyp,IDAYN,imonth)

c Sum up monthly energies

      
      fHRV_Elec_Energy(imonth) = 
     &     fHRV_Elec_Energy(imonth) + TotalPower * 3600./REAL(NTSTEP) !(Watts)


      return
      ENDIF 
      END  
