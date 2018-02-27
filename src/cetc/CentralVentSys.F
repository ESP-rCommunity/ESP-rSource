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
c         P. Pinel Aug 27, 2014
c ---
      Implicit none
#include "building.h"
#include "Ventilation_Data.h"
c --- 27-08-2014: Patrice Pinel: iComp not used in this subroutine
c     Integer iComp

      COMMON/OUTIN/IUOUT,IUIN
      Integer iuout,iuin  ! used for warning output

      Integer iFun
c --- 27-08-2014: Patrice Pinel: Mass flow rate of air now calculated directly (CVSmDotAir) in CVSCommonInitialisation()
c     Real T, FlowRate
      Real T
c --- 27-08-2014: Patrice Pinel: Added outs string.      
      Character Msg*128, outs*128

c ---
      iFun = 0 ! Initialize

c      FlowRate = Max ( CVSSupplyRate, CVSExhaustRate ) / 1000. ! L/s --> m3/s
c
c In HOT2000, the specific heat of air has been 1005 J/Kg Deg C, and the air density
c has been 1.177 Kg/m3.  These values are also used here.
!     Changed value of air Cp to 1006 to reflect AIM
c      MCP = FlowRate * 1006. * 1.177
c
c      If ( iCVSType .eq. 2 ) Then ! HRV
c
c         Call HRV ( iFun, T )
c
c      Else If ( iCVSType .eq. 3 ) Then ! Fans, no Heat recovery
c
c         Call FANS_NO_HR ( iFun, T )
c
c      Endif


c --- 27-08-2014: Patrice Pinel: Changed the if/else structure for a Case statement.
c --- Moved evaluation of flow rates to CVSCommonInitialisation() subroutine
c --- Added a warning when iCVSType does not correspond to available choices: this is a redundancy since MECH_VENT_READIN() will only read values from 1 to 4
      
      ! Routine initialising flow rates, temperature control variables, and zones volume fractions
      Call CVSCommonInitialisation()

      Select Case ( iCVSType )
         Case ( 1 )  ! CVS Type None
         Case ( 2 )  ! CVS Type HRV
            Call HRV ( iFun, T )
         Case ( 3 )  ! CVS Type Fans, no Heat recovery
            Call FANS_NO_HR ( iFun, T )
         Case ( 4 )  ! CVS Type ERV
            Call ERVInit()
         Case default
            write(outs,'(a,I1,a)') 
     &      'MECH_VENT_INIT: iCVSType = ',iCVSType,
     &      ' is invalid. Setting to 1 (none).'
            Call edisp(iuout,outs)
            iCVSType = 1
      End Select
c ---
c     Write (Msg,'(a,2f8.1)') 'MECH_VENT_INIT - HRVeff',
c    x HRVTest(1,2), HRVTest(2,2)
c     Call WINMSG ( Msg )
      Return
      END   ! Routine
c ---

      Subroutine MECH_VENT_CONTROL ( iComp, ZONE_mechvent_cond, 
     &                               ZONE_mechvent_moistFlow,
     &                               ZONE_mechvent_airFlow )
c
c Copyright 2001: NRCan Buildings Group
c Created August  15, 2001
c Edited: BB Aug 15, 16, 17, Oct 4, 10, 12, 23, 25, Nov 1, 2
c ---


c ---
      USE AIM2_InputData, ONLY:AIM2_ref_zone,AIM2_CrawlSpace_zone,
     &            AIM2_attic_zone,AIM2_num_zones_infil,AIM2_zone_infil
      USE AIM2_CalcData, ONLY:AIM2_house_vol!AIM2_zone_infil
      IMPLICIT NONE
#include "building.h"
#include "geometry.h"
#include "Ventilation_Data.h"

      COMMON/C1/NCOMP,NCON
      INTEGER NCOMP,NCON

      COMMON/FVALA/TFA,QFA
      REAL TFA(MCOM),QFA(MCOM)

       COMMON/CLIMI/QFP,QFF,TP,TF,QDP,QDF,VP,VF,DP,DF,HP,HF
       REAL QFP,QFF,TP,TF,QDP,QDF,VP,VF,DP,DF,HP,HF

      COMMON/OUTIN/IUOUT,IUIN
      INTEGER :: IUOUT,IUIN
c
c     Implicit None
      INTEGER J
      Integer iComp

c --- 27-08-2014: Patrice Pinel: Replaced parameter ZONE_infil_cond with ZONE_mechvent_cond
      !Real ZONE_infil_cond
      Real ZONE_mechvent_cond
c --- 27-08-2014: Patrice Pinel: Added parameter representing equivalent ambient air flow resulting in equivalent moisture flow to zone
      Real ZONE_mechvent_moistFlow
c --- 27-08-2014: Patrice Pinel: Added parameter representing ventilation air flow rate
      Real ZONE_mechvent_airFlow
c --- 31-08-2014: Patrice Pinel: Added parameter representing specific heat of air with moisture
      Real MCPHumid

      Integer iFun, i, iCycle
      Real T, HLR, DeltaT, ZoneFraction

c --- 28-08-2014: Patrice Pinel: Added outs string.      
      Character Msg*128, outs*128
      Logical ExitNOW

      real fTempTol
      parameter ( fTempTol = 0.5 )
      
      ! Function returning the humidity ratio
      Real  HUMR  
      ! Function returning the zone averaged temperature and humidity ratio
      Real ZONEAVG      
      
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
c --- 27-08-2014: Patrice Pinel: Replaced parameter ZONE_infil_cond with ZONE_mechvent_cond
      !ZONE_infil_cond = 0.
      ZONE_mechvent_cond = 0.
      ZONE_mechvent_moistFlow = 0.
      ZONE_mechvent_airFlow = 0.
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
         
c --- 27-08-2014: Patrice Pinel: Replaced assignment of T(AIM2_ref_zone) as TMAIN
c --- By a call to a function ZONEAVG(1), which performs a volume weighted average of all zones
c --- The parameter 1 in ZONEAVG(1) stands for calculation of temperatures
         TMAIN = ZONEAVG(1)
         !TMAIN = TFA(AIM2_ref_zone) ! Indoor temp
         !The parameter 2 in ZONEAVG(2) stands for calculation of humidity ratios
         CVSExhaustHRat = ZONEAVG(2)
c
         T = TF ! "future" outdoor air temp
         ! Humidity ratio evaluated with HUMR() function from Psychro.F
         ! HF is the relative humidity from the CLIMI common block
         AmbiantAirHRat = HUMR(T, HF, HVIStandPres)
         
c --- 27-08-2014: Patrice Pinel: Added flag to indicate when temperature control results in the CVS being OFF
c --- CVS is off when ambient temperature is between CVSTLow and CVSTHigh
         bCVS_OFF = T < CVSTHigh .AND. T > CVSTLow

c If a CVS exists,call the appropriate subroutine to
C determine temperatures and flowrates
         
c --- 27-08-2014: Patrice Pinel: Initialise ducts zone temperatures also for ERVs
c --- Replaced If/Else structure with Case
         !If ( iCVSType .eq. 2 ) Then ! HRV
c
           ! i = Max ( 1, IntDD(1,1) ) ! Zone in which supply duct is located
           ! SupplyDuctRoomTemp   = TFA(i)
           ! i = Max ( 1, IntDD(2,1) ) ! Zone in which exhaust duct is located
           ! ExhaustDuctRoomTemp  = TFA(i)
c
           ! Call HRV ( iFun, T )
c
         !Else If ( iCVSType .eq. 3 ) Then ! ! Fans, no Heat recovery
c
          !  Call FANS_NO_HR ( iFun, T )
c
c ---
         !Endif
         
         If ((iCVSType == 2) .or. (iCVSType == 4) ) Then   ! HRV or ERV
            i = Max ( 1, IntDD(1,1) )                       ! Zone in which supply duct is located
            SupplyDuctRoomTemp   = TFA(i)
            i = Max ( 1, IntDD(2,1) )                       ! Zone in which exhaust duct is located
            ExhaustDuctRoomTemp  = TFA(i)
         Endif
         
         Select Case ( iCVSType )
            Case ( 1 )  ! CVS Type None
            Case ( 2 )  ! CVS Type HRV
                Call HRV ( iFun, T )
            Case ( 3 )  ! CVS Type Fans, no Heat recovery
                Call FANS_NO_HR ( iFun, T )
                ! Humidity ratio of supply air is that of ambient air
                CVSSupplyHRat = AmbiantAirHRat
            Case ( 4 )  ! CVS Type ERV
                Call ERVOperation (T, TMAIN)  
            Case default
                write(outs,'(a,I1,a)') 
     &          'MECH_VENT_INIT: iCVSType = ',iCVSType,
     &          ' is invalid. Setting to 1 (none).'
                Call edisp(iuout,outs)
                iCVSType = 1
         End Select
      Endif
c      
c --- 27-08-2014: Patrice Pinel: Individual calculations of zone fractions replaced with
c --- evaluation of a CVSZoneFrac() array during initialisation

c If the zone is connected to the CVS then distribute the air flow based on volume
C Otherwise follow the final else which sets it to zero
      !If ( iComp .ne.  AIM2_CrawlSpace_zone .and.
      !&     iComp .ne.  AIM2_attic_zone      ) Then
c
c Apportion ventilation to each zone.  For now just distribute the air flow
c in proportion to each room's volume
       !  ZoneFraction = 0.
c
C Check to ensure memory has been allocated
        ! IF (ALLOCATED(AIM2_zone_infil)) THEN
        !   Do j = 1, AIM2_num_zones_infil
        !      If ( AIM2_zone_infil(j) .eq. iComp ) Then
        !         ZoneFraction = VOL(iComp) / AIM2_house_vol
        !         Exit
        !      Endif
        !   End Do
        ! ELSE
        !  CALL EDISP(IUOUT,
      !&          ' Warning: array AIM2_zone_infil not allocated')
      !   END IF
c
c --- Air heat loss rate for zone
c
         !HLR = MCP * ( TFA(iComp) - CVSSupplyTemp ) * ZoneFraction
c --- 27-08-2014: Patrice Pinel: Added a test to insure a null heating load is returned when the CVS is turned OFF
c --- Equivalent ambiant air moisture flow is the air flow adjusted by the ratio of humidity ratios of the supply and ambient air
         If(bCVS_OFF) Then
            HLR = 0.
            ZONE_mechvent_moistFlow = 0.
            ZONE_mechvent_airFlow = 0.
         Else
            ! Inertia of humid airflow
            MCPHumid = CVSmDotAir*(HVIAirCp + HVIVapCp*CVSSupplyHRat)
            HLR = MCPHumid*(TFA(iComp) - CVSSupplyTemp)
     &            *CVSZoneFrac(iComp)
            ZONE_mechvent_moistFlow = CVSmDotAir * CVSZoneFrac(iComp) 
     &                                * CVSSupplyHRat/AmbiantAirHRat
            ZONE_mechvent_airFlow = CVSmDotAir * CVSZoneFrac(iComp)

         Endif

c
c --- Convert air heat loss rate into an equivalent conductance (to outdoors)
C For the zone called, assign the zone's calculated CVS infiltration conductance
C to ESP-r's standard infiltration variable, CVIF. CVIF is incorporated into the
C zone's energy balance by ESP-r's normal approach, so no further
C interactions with the zone matrix processor and solver are required.
C Note that both AIM (natural) [/cetc/aim2_timestep.F] and CVS (forced) [/cetc/CentralVentSys.F]
C infiltration exist and as such the values are summed in /esrubld/subsys.F.
c
         DeltaT = TFA(iComp) - T

c --- 27-08-2014: Patrice Pinel: Added a null conductance when the tolerance is not met
        ! If ( Abs(DeltaT) .gt. fTempTol )
      !&              ZONE_infil_cond = HLR / DeltaT
       If (Abs(DeltaT) .gt. fTempTol) Then
          ZONE_mechvent_cond = HLR / DeltaT
       Else
          ZONE_mechvent_cond = 0.
       Endif

c
c --- Supply Duct heat gain
c
         If ( iComp .eq. IntDD(1,1) ) Then
c Supply duct is in zone

c --- 27-08-2014: Patrice Pinel: Replaced parameter ZONE_infil_cond with ZONE_mechvent_cond
c --- Replaced HRVSupDuctHG with CVSSupDuctHG
            !If ( Abs(DeltaT) .gt. fTempTol ) ZONE_infil_cond =
      !&                 ZONE_infil_cond + HRVSupDuctHG / DeltaT
            If ( Abs(DeltaT) .gt. fTempTol ) ZONE_mechvent_cond =
     &                 ZONE_mechvent_cond + CVSSupDuctHG / DeltaT
         Endif
c
c --- Exhaust Duct heat gain
c
         If ( iComp .eq. IntDD(2,1) ) Then
c Exhaust duct is in zone

c --- 27-08-2014: Patrice Pinel: Replaced parameter ZONE_infil_cond with ZONE_mechvent_cond
c --- Replaced HRVExhDuctHG with CVSExxDuctHG            
            !If ( Abs(DeltaT) .gt. fTempTol ) ZONE_infil_cond =
      !&                 ZONE_infil_cond + HRVExhDuctHG / DeltaT
            If ( Abs(DeltaT) .gt. fTempTol ) ZONE_mechvent_cond =
     &                 ZONE_mechvent_cond + CVSExhDuctHG / DeltaT

         Endif
         

c ---
      If ( iCycle .lt. 22 ) Then
         Write (Msg,'(a,7f8.2,2i5,f8.4)') 'MECH_VENT_CONTROL',
c --- 27-08-2014: Patrice Pinel: Replaced parameter ZONE_infil_cond with ZONE_mechvent_cond
      !& ZONE_infil_cond, HLR, TFA(iComp), TF, SupplyDuctRoomTemp,
     & ZONE_mechvent_cond, HLR, TFA(iComp), TF, SupplyDuctRoomTemp,
     & ExhaustDuctRoomTemp, CVSSupplyTemp,
     & AIM2_ref_zone, iComp, CVSZoneFrac(iComp)
         Call WINMSG ( Msg )
      Endif
c
      iCycle = iCycle + 1
      Return
      END



C -------------------------------------------------------------------
C -------------------------------------------------------------------
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
c Aug 2, 15, 16, Oct 4, 25, Nov 1, Mar 3/08
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
      use h3kmodule
      Implicit None
#include "building.h"
#include "Ventilation_Data.h"
#include "SOFC.h"
#include "OffsiteUtilitiesPublic.h"
#include "h3k_report_data.h"

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
c --- 27-08-2014: Patrice Pinel: Moved the temperature control variables to Ventilation_Data.h
!      Real OpTime, Tlow, Thigh
      Real OpTime
c --- 27-08-2014: Patrice Pinel: Added humidity ratios
      Real w_pre, w_s
      ! humidity ratios at saturation
      Real w_satpre, w_sats
c --- 27-08-2014: Patrice Pinel: Added potential heat transfer in the exchanger      
      Real ExchPot
c --- Need a call to the function HUMR from Psychro.F to evaluate the humidity ratios
      Real HUMR
      
      Character Msg*128

c ---
      Save MonVar, PreHtr, Type2, m, i, x, TENDEG, U4, U5
c ---
      Save sDuctHG, eDuctHG
      Save EffHT, EffLT, dEffdT, FPc1, FPc2, PHrefT
      Save FanPwr, PrePwr, eHRVmax, eHRV
      Save Tpre, T7, Ts, Te, T2, Tx, T3, Tex
c --- 27-08-2014: Patrice Pinel: Moved the temperature control variables to Ventilation_Data.h
!      Save OpTime, Tlow, Thigh
      Save OpTime
      Save Msg


C Used in the xml reporting structure
C      real fH3KSumEnergyUseElec(iNumUses)        ! Energy consumption assoc. w/ electricuty (W)


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
c --- 29-08-2014: Patrice Pinel: Added conversion from SRE to HEE
        ! EffHT = HRVtest(1,2)
        ! Potential heat exchange from the exchanger
        ExchPot = MCP * (HVITexhH - HRVtest(1,1))
        EffHT = HRVtest(1,2) *(ExchPot+FPc1)/ExchPot


c
c HRV effectiveness at "-25" C
c --- 29-08-2014: Patrice Pinel: Added conversion from SRE to HEE
        ! EffLT = HRVtest(2,2)
        ! Potential heat exchange from the exchanger
        ExchPot = MCP * (HVITexhH - HRVtest(2,1))
        If(PreHtr) Then     ! Pre-heater is present
            ! Fan power is the same as during the HSP test
            EffLT = HRVtest(2,2) *(ExchPot+FPc1)/ExchPot
        Else                ! No pre-heater
            ! Fan power is half the power during the VLTT
            EffLT = HRVtest(2,2) *(ExchPot+0.5*U5)/ExchPot
        Endif
         
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

c --- 27-08-2014: Patrice Pinel: Moved initialisation of the temperature control variables to CVSCommonInitialisation()
!         If ( i .eq. 3 ) Then
c Central Ventilation Sys. Lower limit for temperature control
!            Tlow  = rCVSLowTempTrip ! GenDat(44)
c Central Ventilation Sys. Upper limit for temperature control
!            Thigh = rCVSHighTempTrip !GenDat(45)
!         Else ! No temperature dependant operation
!            Tlow  = -9999.
!            Thigh = -Tlow
!         Endif
c ---
      Else If ( iFun .gt. 0 .and. iFun .le. 12 ) Then ! During month m
c
         m = iFun
         OpTime = CVSOpTime
c ---
c
c T = Outdoor air temperature
c
c --- 27-08-2014: Patrice Pinel: Changed test for boolean variable bCVS_OFF evaluated in MECH_VENT_CONTROL()
c --- Evaluated the same way for all CVS systems
         !If ( T .gt. Tlow .and. T .lt. Thigh ) Then
         If(.not. bCVS_OFF) Then    ! CVS is not OFF
c
c Current "house" air temperature (as delivered to HRV return ducts)
            T3 = TMAIN ! Local Tmain
c
c Supply duct gain (into HRV system)
            sDuctHG = DUCTHL ( 1, T, Tpre )
            
c --- 27-08-2014: Patrice Pinel: Added evaluation of humidity ratio at station pre
            ! Humidity ratio at saturation
            w_satpre = HUMR(Tpre,100.,HVIStandPres)
            ! Humidity ratio is minimum between ambient and saturation
            w_pre = min(w_satpre, AmbiantAirHRat)
     
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
c --- HRV sensible efficiency
c
c The data tables are based on temperature upstream of the preheater
            If ( T7 .lt. TENDEG ) Then
c
c Mar 3/08     eHRV =  aMax1 ( EffHT + dEffdT * ( T7 - TENDEG ), 0. )
               eHRV =  aMax1 ( EffHT + dEffdT * ( Tpre - TENDEG ), 0. )
c For a Type 2, max eff is limited
               If ( Type2 ) eHRV = aMin1 ( eHRV, eHRVmax )
c
c Fan power varies with temperature
c Mar 3/08     FanPwr = aMax1 ( 0., FPc1 - FPc2 * ( T - TENDEG ) )
               FanPwr = aMax1 ( 0., FPc1 - FPc2 * ( Tpre - TENDEG ) )
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
            
c --- 27-08-2014: Patrice Pinel: Added evaluation of humidity ratio at station s
            ! Humidity ratio at saturation
            w_sats = HUMR(Ts,100.,HVIStandPres)
            ! Humidity ratio is minimum between station pre and saturation
            w_s = min(w_pre, w_sats)

            
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
c --- 27-08-2014: Patrice Pinel: Added evaluation of humidity ratio
            CVSSupplyHRat = w_s            
c
c HRV ventilation air heat loss rate
            HRVventHLR = HRVairHLR + ( sDuctHG + eDuctHG ) * OpTime
c
            ExhaustFanPower  = FanPwr
            PreheaterPower = PrePwr
c --- 27-08-2014: Patrice Pinel: Replaced HRVSupDuctHG and HRVExhDuctHG with CVSSupDuctHG and CVSExhDuctHG           
            !HRVExhDuctHG = eDuctHG * OpTime
            ! HRVSupDuctHG = sDuctHG * OpTime
            CVSExhDuctHG = eDuctHG * OpTime
            CVSSupDuctHG = sDuctHG * OpTime
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
c --- 27-08-2014: Patrice Pinel: Replaced HRVSupDuctHG and HRVExhDuctHG with CVSSupDuctHG and CVSExhDuctHG
            !HRVExhDuctHG = 0.
            !HRVSupDuctHG = 0.
            CVSExhDuctHG = 0.
            CVSSupDuctHG = 0.
            TotalPower = 0.
         Endif

         ! Set flag allowing h3kreports to scale this data for seasonal 
           ! reporting purposes.
           if ( bFirst_call_hrv  )
     &       call SetAdditionalValues(
     &         rvPltHvacHRVElecLd%Identifier,
     &         3,
     &         .true.)         
         
         Call AddToReport(rvPltHvacHRVElecLd%Identifier,
     &       TotalPower)        
         
        bFirst_call_hrv = .false.     
     
C Store aggregate electricity use for reporting in Site Utility facilitity.
      fSUFuelEnergyUse( iElectricity,iUseHRV )
     &                   = TotalPower

C     Pass fuel energy stroage data to site utilities
      call StoreSiteEnergyUse( iIdealVent, fSUFuelEnergyUse )

c
c1001 Format ( i3, f9.5, 3f8.3, f8.1, 2f8.2, f8.1, 5f8.3,2f8.1,f8.3)
c
      Endif
c
      Return
      END

C ---------------------------------------------------------------------------


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
      use h3kmodule
      Implicit None
#include "building.h"
#include "Ventilation_Data.h"
#include "SOFC.h"
#include "OffsiteUtilitiesPublic.h"
#include "h3k_report_data.h"
      Integer m
      Real T
c ---
c --- 27-08-2014: Patrice Pinel: Moved the temperature control variables to Ventilation_Data.h
!      Real Tlow, Thigh
      Real sFP, FanPwr, x
!      Save Tlow,Thigh,sFP,FanPwr,x
      Save sFP,FanPwr,x
c
c --- Initialization
c
      If ( m .lt. 1 ) Then
c
c --- 27-08-2014: Patrice Pinel: Moved initialisation of the temperature control variables to CVSCommonInitialisation()
!         If ( iCVSOpType .eq. 3 ) Then
!            Tlow  = rCVSLowTempTrip  ! low  temp trip
!            Thigh = rCVSHighTempTrip ! high temp trip
!         Else
!            Tlow  = -9999. ! works for terrestial planets
!            Thigh = +9999.
!         Endif
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
c --- 27-08-2014: Patrice Pinel: Changed test for boolean variable bCVS_OFF evaluated in MECH_VENT_CONTROL()
c --- Evaluated the same way for all CVS systems
         !If ( T .gt. Tlow .and. T .lt. Thigh ) Then

         If(.not. bCVS_OFF) Then    ! CVS is not OFF
c
c Ventilation air heat loss rate
c
c Temperature of air downstream of the supply fan
            CVSSupplyTemp = T + sFP / MCP
c
            HRVventHLR = ( MCP * ( TMAIN - T ) -
     x sFP ) * CVSOpTime

            TotalPower = HRVtest(1,3)

         Else ! Not operating
c
            HRVventHLR = 0.
c
            CVSSupplyTemp = T
c
            TotalPower = 0.
         Endif
c ---
c --- Reporting
      
         ! Set flag allowing h3kreports to scale this data for seasonal reporting purposes.
         If (bFirst_call_noHR) Then     ! First call to the erv model
             Call SetAdditionalValues(rvPltHvacHRVElecLd%Identifier, 
     &                              3, .true.)         
             ! Ensure any subsequent call is not treated as the first one   
             bFirst_call_noHR = .false.
         Endif
         
         ! Report electricity consumption
         Call AddToReport(rvPltHvacHRVElecLd%Identifier, TotalPower)      
         
         ! Store aggregate electricity use for reporting in Site Utility facilitity.
         fSUFuelEnergyUse(iElectricity,iUseHRV)= TotalPower
         
         ! Pass fuel energy stroage data to site utilities
         Call StoreSiteEnergyUse(iIdealVent, fSUFuelEnergyUse)         

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
#include "building.h"
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
c --- 27-08-2014: Patrice Pinel: Changed hard coded value: 0.60501 = rho Cp /2000
            !CnstDD(Duct,1) = 0.60501 * AvVel * Diam / ( HTC * L )
            CnstDD(Duct,1) = (HVIAirDensity * HVIAirCp / 2000.) 
     &                       * AvVel * Diam / ( HTC * L )
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
c --- 27-08-2014: Patrice Pinel: Changed hard coded value: 1.21002 = rho Cp /1000
         !HTC = 1.21002 * CnstDD(Duct,3) * HTC
         HTC = (HVIAirDensity * HVIAirCp / 1000.) 
     &          * CnstDD(Duct,3) * HTC
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
c  None. Sums up Monthly HRV energy in H3KSTORE_DATA module

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
c*****************************************************************
c ERVInit
c
c Created by: Patrice Pinel
c Initial Creation Date: August 25th 2014
c
c Subroutine to determine the global variables evaluated during the 
c preprocessing of the ERV model
 
c references:
c   1) "Development of an ERV model for ESP-r", P.Pinel, NRCAN Report 
c **********************************************************************
      Subroutine ERVInit()
      implicit none

c ********** Declaration

#include "building.h"
#include "Ventilation_Data.h"

      ! Access variables for warning output
      Integer iuout,iuin 
      COMMON/OUTIN/IUOUT,IUIN

      ! Local variables
      Real  PHPVLTT     ! Pre-heat power during VVLT test (W)
      Real  FanPowLT    ! Power of each fan during VVLT test (W)
      Real  w1CSP       ! CSP test humidity ratio at ERV supply inlet (kg_w/kg_da)
      Real  w3CSP       ! CSP test humidity ratio at ERV exhaust inlet (kg_w/kg_da)
      Real  h1CSP       ! CSP test enthalpy at ERV supply inlet (J/kg)
      Real  h3CSP       ! CSP test enthalpy at ERV exhaust inlet (J/kg)
      Real  ExchPot     ! Potential sensible heat transfer, HSP test (W)
      Real  ExchPotLT   ! Potential sensible heat transfer, VLTT test (W)
      Real  HEESnsLT    ! Sensible effectiveness (HEE) during VLTT test
      Real  T9CSP       ! Temperature of supply air leaving exchanger, CSP test (C)
      Real  h9CSP       ! Enthalpy of supply air leaving exchanger, CSP test (J/kg)
      Real  w9CSP       ! Humidity ratio of supply air leaving exchanger, CSP test (kg_w/kg_da)
      
      Real Dummy1, Dummy2, Dummy3 ! Dummy variables to initialize the duct model
      
      ! Functions used      
      Real  HUMR        ! Function returning the humidity ratio
      Real  DUCTHL      ! Function to calculate duct heat gains and airstream temperature at the exit of ducts

c ************ Begining of execution
      
c --- Evaluation of pre-heater power variables
      ! If pre-heater capacity > 1, and P(VLTT) > P(HSP), then .True.
      ERVPreHtr = ( PreHtrCap > 1. .and. ERVTest(2,4) > ERVTest(1,4))

      ! If there is a pre-heater, initialize additional global variables
      If (ERVPreHtr) Then
          ! Pre-heater is set to minimum of user provided capacity and 
          ! the difference in reported power between VLTT and HSP tests
          PHPVLTT = min(PreHtrCap,(ERVTest(2,4)-ERVTest(1,4)))
          ! Temperature below which the pre-heater is active (C)
          ERVPHrefT = ERVTest(2,1) + (PHPVLTT/MCP)
          ! Variation of pre-heater power with temp (W/K)
          ERVPHDPowDT = MCP
      Else ! There is no pre-heating
          PHPVLTT = 0.
      Endif

c --- Evaluation of fan power variables
      
      ! Power for each fan: assumed to be half reported power (W)
      ERVFanPow = ERVTest(1,4)/2.

      ! VVLT test fan power (W)
      FanPowLT = (ERVTest(2,4)-PHPVLTT)/2.

      ! Variation of fan pow. with T (low T) (W/K)
      ERVDFanPowDT = (FanPowLT-ERVFanPow)/(ERVTest(2,1)-CVSTFrost)

c --- Evaluation of air psychrometric state during tests
      ! Humidity ratio at ERV supply and exhaust inlets (CSP Test)
      w1CSP = HUMR(HVITsupC,HVIRHsupC,HVIStandPres)
      w3CSP = HUMR(HVITexhC,HVIRHexhC,HVIStandPres)
      
      ! Enthalpy at ERV supply and exhaust inlets (CSP Test)
      h1CSP = HVIAirCp*HVITsupC + 
     &       (w1CSP*(HVIHeatVapW+(HVIVapCp*HVITsupC)))
      h3CSP = HVIAirCp*HVITexhC + 
     &       (w3CSP*(HVIHeatVapW+(HVIVapCp*HVITexhC)))
     
c --- Evaluation of sensible effectiveness variables
      ! Potential sensible effectiveness during HSP test
      ExchPot = MCP * (HVITexhH - ERVTest(1,1))
      
      ! Sensible effectiveness (HEE) under normal operation
      ERVHEESns = ((ExchPot+ERVFanPow)/ExchPot) * 
     &            (ERVTest(1,2)/100.)
     
      ! Same approach for sensible effectiveness during the VLTT test
      ExchPotLT = MCP * (HVITexhH - ERVTest(2,1))
      HEESnsLT = ((ExchPotLT+FanPowLT)/ExchPotLT) * 
     &            (ERVTest(2,2)/100.)
     
      ! Variation of sensible effectiveness during low temperature operation
      ERVDHEESnsDT = (HEESnsLT - ERVHEESns)/(ERVTest(2,1) - CVSTFrost)
      
      ! Maximum sensible effectiveness at low temperature
      ERVMaxHEESnsLT = max(HEESnsLT,CVSSnsEffLimLT)
      
c --- Evaluation of latent effectiveness variables
      ! Latent effectiveness during the HSP test
      ERVHEELatH = ERVTest(1,3)
      
      ! Variation of latent effectiveness with temperature during low temperature operation
      ERVDHEELatDT = (ERVTest(2,3) - ERVHEELatH)/
     &               (ERVTest(2,1)-CVSTFrost)
     
      ! Maximum latent effectiveness at low temperature
      ERVMaxHEELatLT = max(ERVTest(2,3),CVSLatEffLimLT)
      
      ! Temperature of supply air leaving the exchanger, CSP Test
      T9CSP = HVITsupC + ERVHEESns * (HVITexhC - HVITsupC)
      
      ! Enthalpy of supply air leaving the exchanger, CSP Test
      h9CSP = h1CSP + ((CoolEF/100.) * (h3CSP - h1CSP - 
     &                (ERVFanPow/CVSmDotAir)))
     
      ! Humidity ratio of supply air leaving the exchanger
      w9CSP = (h9CSP - (HVIAirCp*T9CSP))/
     &        (HVIHeatVapW + (HVIVapCp*T9CSP))
     
      ! Latent effectiveness in cooling
      ERVHEELatC = (w9CSP - w1CSP)/(w3CSP - w1CSP)

c --- Initialize duct data by calling DUCTHL with flag 0
      Dummy1 = DUCTHL(0,Dummy2,Dummy2)
          
c ********** End of execution

      Return
      END   ! Routine
      
      
      
      
c*****************************************************************
c ERVOperation
c
c Created by: Patrice Pinel
c Initial Creation Date: August 27th 2014
c
c Subroutine to assess the ERV in operation mode
 
c references:
c   1) "Development of an ERV model for ESP-r", P.Pinel, NRCAN Report 
c   ERV model  
c
c  Tamb ______ T1______  T7                                T2
c     |      |   |Pre-  |    ___________________________
c  ==>| Duct |=>=|Heater|==>|                T9 |Supply |====> House
c     |______|   |______|   |                   | Fan   |       Supply
c      Troom                |                   |_______|
c      ______               |_______                    |
c     |      |           T4 |Exhaust|                   |   T3
c  <==| Duct |========<=====| Fan   | T10                |==<== House
c Tout|______|              |_______|___________________|       Exhaust
c
c NOTE: Location of humidity ratios w1, ... are the same
c **********************************************************************

      Subroutine ERVOperation(T_amb,T_3)

      ! Module for tracking electrical draws of HVAC equipment that is simulated
      ! in the building thermal domain (ie. the H3K HVAC models invoked below).
      ! This is used by the fuel cell controller to determine the house's total
      ! electrical demand during each time-step in the plant domain.
      use h3kmodule
      Implicit None
#include "building.h"
#include "Ventilation_Data.h"
#include "SOFC.h"
#include "OffsiteUtilitiesPublic.h"
#include "h3k_report_data.h"

c ********** Declaration
      ! Access variables for warning output
      Integer iuout,iuin  
      COMMON/OUTIN/IUOUT,IUIN

      ! Arguments/Inputs
      Real T_amb                            ! Temperature (deg-C) of ambient air
      Real T_3                              ! Temperature (deg-C) of exhaust air
      
      ! Local variables
      Real T_1, w_1                         ! Temperature and humidity ratio at station 1
      Real T_7, w_7                         ! Temperature and humidity ratio at station 7
      Real T_9, w_9                         ! Temperature and humidity ratio at station 9
      Real T_10, T_4, T_out                 ! Temperatures on the exhaust stream: stations 10, 4, and out
      Real w_sat1                           ! Humidity ration at saturation at station 1
      Real w_sat9                           ! Humidity ration at saturation at station 9
      Real PowPH                            ! Pre-heater power (W)
      Real PowF                             ! Fan power (W)
      Real HEESns, HEELat                   ! Sensible and latent Heat Exchanger Effectiveness

      ! Functions used      
      Real  HUMR                            ! Function returning the humidity ratio: located in psychro.F
      Real  DUCTHL                          ! Function to calculate duct heat loss

c **********  Begining of execution
          
      If(bCVS_OFF) Then         ! ERV is not operating -> don't bother calculating
      
         ! Set reported electricity use to null
         TotalPower = 0.
        
         ! Set heat exchange from zones to airstreams in ducts to null
         CVSExhDuctHG = 0.
         CVSSupDuctHG = 0.
  
      Else                      ! ERV is ON
        
      ! --- Evaluate conditions at station 1
       
         ! Duct model with 1 flag -> supply duct
         ! Air enters at T_amb, exits at T_1
         ! Heat exchange from zone to air in duct is CVSSupDuctHG
         CVSSupDuctHG = DUCTHL(1, T_amb, T_1)      
       
         ! Humidity ratio at saturation at station 1
         w_sat1 = HUMR(T_1,100.,HVIStandPres)
       
         ! Make sure humidity ratio at station 1 does not exceed saturation
         ! AmbiantAirHRat is defined in Ventilation_Data.h and evaluated in MECH_VENT_CONTROL()
         w_1 = min(AmbiantAirHRat, w_sat1)
        
      ! --- Evaluate pre-heater power
         ! Initialize
         PowPH = 0.
       
         If(T_1 < ERVPHrefT)    ! Temperature lower than the one at which the pre-heater is active
            ! Interpolate pre-heater power
            ! Make sure not to exceed the capacity
     &      PowPH = min(ERVPHDPowDT*(ERVPHrefT - T_1), PreHtrCap)
     
      ! --- Evaluate conditions at station 7
         
         ! Add effect of pre-heater on temperature of an air stream
         T_7 = T_1 + powPH/MCP
         
         ! Pre-heater has no effect on moisture content
         w_7 = w_1
         
      ! --- Evaluate fan power
         
         ! Fan power from HSP test 
         PowF = ERVFanPow
         
         If(T_1 < CVSTFrost)        ! Frost can have an effect on ERV operation
         ! Interpolate fan power
         ! Make sure it is not negative
     &      PowF = max(0., PowF + ERVDFanPowDT*(T_1 - CVSTFrost))
      
      ! --- Evaluate exchanger effectiveness
         
         ! Sensible effectiveness
         ! Effectiveness from HSP test
         HEESns = ERVHEESns
         
         If(T_1 < CVSTFrost)         ! Frost can have an effect on ERV operation
         ! Interpolate effectiveness
         ! Make sure it is not negative and does not exceed maximum allowable
     &       HEESns = min(ERVMaxHEESnsLT, 
     &                 max(0., HEESns + ERVDHEESnsDT*(T_1 - CVSTFrost)))
         
         ! Latent effectiveness
         If(T_7 > T_3) Then             ! Exchanger in cooling mode
            HEELat = ERVHEELatC         ! Effectiveness from CSP test
         Else                           ! Exchanger in heating mode
            ! Effectiveness from HSP test
            HEELat = ERVHEELatH         ! Effectiveness from HSP test
            
            If(T_1 < CVSTFrost)         ! Frost can have an effect on ERV operation
            ! Interpolate effectiveness
            ! Make sure it is not negative and does not exceed maximum allowable
     &       HEELat = min(ERVMaxHEELatLT, 
     &                 max(0., HEELat + ERVDHEELatDT*(T_1 - CVSTFrost)))
                 
         Endif
         
      ! --- Evaluate conditions at station 9
         
         ! Temperature
         T_9 = T_7 + HEESns * (T_3 - T_7)
         
         ! Condensation may occur at station 9 so the humidity ratio is calculated
         w_sat9 = HUMR(T_9,100.,HVIStandPres)
         
         ! Humidity ratio at station 9 can not exceed saturation
         w_9 = min(w_sat9, w_7 + HEELat*(CVSExhaustHRat - w_7))
         
      ! --- Evaluate temperature at station 10
         T_10 = T_3 - HEESns * (T_3 - T_7)
         
      ! --- Evaluate conditions at station 2
         
         ! Add effect of fan on temperature of an air stream -> T2
         CVSSupplyTemp = T_9 + PowF/MCP
         
         ! Fan has no effect on moisture content -> w2
         CVSSupplyHRat = w_9
      
      ! --- Evaluate temperature at station 4
         ! Add effect of fan on temperature of an air stream
         T_4 = T_10 + PowF/MCP
      
      ! --- Evaluate heat transfered from zone to airstream in exhaust duct
         ! Duct model with 2 flag -> exhaust duct
         ! Air enters at T_4, exits at T_out
         ! Heat exchange from zone to air is CVSExhDuctHG
         CVSExhDuctHG = DUCTHL(2, T_4, T_out)      
         
      ! --- Evaluate reported electricity use        
         TotalPower = 2*PowF + PowPH
        
      Endif

c --- Reporting
      
      ! Set flag allowing h3kreports to scale this data for seasonal reporting purposes.
      If (bFirst_call_erv) Then     ! First call to the erv model
        Call SetAdditionalValues(rvPltHvacHRVElecLd%Identifier, 
     &                           3, .true.)         
        ! Ensure any subsequent call is not treated as the first one   
        bFirst_call_erv = .false.
      Endif
      
      ! Report electricity consumption
      Call AddToReport(rvPltHvacHRVElecLd%Identifier, TotalPower)      
      
      ! Store aggregate electricity use for reporting in Site Utility facilitity.
      fSUFuelEnergyUse(iElectricity,iUseHRV)= TotalPower
      
      ! Pass fuel energy stroage data to site utilities
      Call StoreSiteEnergyUse(iIdealVent, fSUFuelEnergyUse)
      
c ********** End of execution
      
      Return
      END   ! Routine
      
c*****************************************************************
c CVSCommonInitialisation
c
c Created by: Patrice Pinel
c Initial Creation Date: August 27th 2014
c
c Subroutine to effect initialisation common to all Central Ventilation Models
c The goal is to have a common method to treat things like zone fractions and temperature control
c This avoids having to make change in three subroutines when the treatment is changed
c **********************************************************************

      Subroutine CVSCommonInitialisation()

      ! Access relevant variables from the AIM2 model
      USE AIM2_InputData, ONLY:AIM2_CrawlSpace_zone,
     &            AIM2_attic_zone,AIM2_num_zones_infil,AIM2_zone_infil
      USE AIM2_CalcData, ONLY:AIM2_house_vol
      Implicit None
#include "building.h"
#include "Ventilation_Data.h"
#include "geometry.h"

      ! Access NCOMP, the number of zones
      COMMON/C1/NCOMP,NCON
      INTEGER NCOMP,NCON

      ! Local variables
      Integer iComp, iAIM     ! Generic loop counters

c --- 27-08-2014: Patrice Pinel: Changed the hard coded density and specific heat for links to parameters HVIAirDensity and HVIAirCp defined in "Ventilation_Data.h".  
c --- Also added a CVSmDotAir variable, representing the mass flow rate of air, since tests in cooling mode deal with Enthalpy and not temperature
      CVSmDotAir = max(CVSSupplyRate, CVSExhaustRate)*HVIAirDensity/1000.   ! Mass flow rate of air (kg/s)
      MCP = CVSmDotAir*HVIAirCp                                             ! Thermal inertia of air stream (W/K)

! 27-08-2014: Patrice Pinel: Put the calculation of zone fractions here.  
! Since they are used many times during analysis, to evaluate the psychrometric state of exhaust air and the zone loads, 
! it makes more sens to have them initialized only once.  
      If (allocated(AIM2_zone_infil)) Then			                        ! The aim-2 data has been read in
         CVSZoneFrac = 0.                                                   ! Initialize all zone fractions to zero
         Do iComp = 1,NCOMP                                                 ! Go through all zones
            Do iAIM = 1, AIM2_num_zones_infil                               ! Go through all AIM2 zones
               If ( AIM2_zone_infil(iAIM) == iComp .and. 
     &         iComp /= AIM2_CrawlSpace_zone .and. 
     &         iComp /= AIM2_attic_zone) Then                               ! Zone is not an attic or crawlspace, and receives AIM-2 infiltration
                  CVSZoneFrac(iComp) = VOL(iComp) / AIM2_house_vol          ! Fraction of total volume receiving infiltrations
               Endif
            Enddo
         Enddo
      Else                                                                  ! AIM-2 initialization has not occurred, terminate program
         stop 'MECH_VENT_INIT: AIM2 data not initialized for CVS model'
      Endif
      
c --- Evaluation of temperature control variables
      If (iCVSOpType == 3 ) Then ! There is temperature control
          CVSTLow = rCVSLowTempTrip
          CVSTHigh = rCVSHighTempTrip
      Else ! ERV in continuous mode
          ! CVS on when Tamb < Tlow or Tamb > Thigh
          ! -> will be alway on if Thigh is low or Tlow is high  
          CVSTLow = 9999.
          CVSTHigh = -9999.
      Endif
      
      Return
      END   ! Routine
      
 
c*****************************************************************
c ZONEAVG
c
c Created by: Patrice Pinel
c Initial Creation Date: August 27th 2014
c
c Function returning the volume weighted average of a quantity
c A value of 1 for the Flag parameter returns a temperature
c A value of 2 for the Flag parameter returns a humidity ratio
c **********************************************************************
      Real Function ZONEAVG(Flag)
      
      
#include "building.h"
#include "Ventilation_Data.h"
#include "geometry.h"
      ! Access NCOMP, the number of zones in the building model
      COMMON/C1/NCOMP,NCON
      INTEGER NCOMP,NCON

      ! Access common block containing the zone temperatures TFA()
      COMMON/FVALA/TFA,QFA
      REAL TFA(MCOM),QFA(MCOM)
      
      ! Access common block containing the zone humidity ratios GFA()
      COMMON/FVALG/GFA
      REAL GFA(MCOM)

c --- Variables declaration
      ! Parameters
      Integer Flag      ! 1 for temperature, 2 for humidity ratio
      
      ! Local variables
      Integer iComp     ! Generic loop counter
      Real total
      
c --- Execution
      ! Initialise the total to zero
      total = 0.

      ! Go through al zones
      Do iComp = 1,ncomp                                     ! Go through all zones
         If (Flag == 1) THEN                                 ! Temperatures
            total = total + CVSZoneFrac(iComp)*TFA(iComp)
         Else                                                ! Humidity ratio
            total = total + CVSZoneFrac(iComp)*GFA(iComp)
         Endif
      Enddo
      
      ! Return the value of total
      ZONEAVG = total
      
      Return
      END   ! Function