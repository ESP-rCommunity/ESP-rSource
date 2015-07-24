C This file is part of the ESP-r system.
C Copyright Natural Resources Canada, Government
C of Canada 2004/2005. Please Contact Ian
C Beausoliel-Morrison for details concerning licensing.

C ESP-r is free software.  You can redistribute it and/or
C modify it under the terms of the GNU General Public
C License as published by the Free Software Foundation
C (version 2 orlater).

C ESP-r is distributed in the hope that it will be useful
C but WITHOUT ANY WARRANTY; without even the implied
C warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
C PURPOSE. See the GNU General Public License for more
C details.

C You should have received a copy of the GNU General Public
C License along with ESP-r. If not, write to the Free
C Software Foundation, Inc., 59 Temple Place, Suite 330,
C Boston, MA 02111-1307 USA.

C *********************************************************************
C POWOC_CETC_BATTERY
C
C Created by: Patrice Pinel
C Initial Creation Date: March 2005
C Modified by: Maria Mottillo
C
C - This code is an internal resistance model of a battery. Currently the model
C   only works together with a controller (RE-H2-ctl.F - subroutine RESH2_Ctl).
C   When more complex multi-input multi-output controllers in ESP-r will become
C   available, the model should be updated to work with these controllers.
C
C - Battery internal resistance, free voltage and capacity are determined
C   from input parameters and correlations
C   so the model can be used to model different technologies by modifying these parameters
C
C  This is the common interface for the following power-only components:
C                ID = 17 (lead-acid battery)
C                ID = 21 (VRB)
C                ID = 22 (Li-Ion)
C
C - The lead-acid battery model includes a crude thermal mass model of battery temperature
C   evolution as well as an optional thermal management system composed of electric resistances
C   that take their energy from the battery (added to the battery load) in discharge mode
C   and from the energy available in charge mode.
C   When there is no available energy, the battery temperature is floating
C
C - The lead-acid battery model also contains a battery life calculation module
C   and an optional battery life controller. The controller currently only works for
C   systems that have a back-up power source (e.g. grid connected PV-system).
C
C - A list of input parameters for the lead acid battery model is given at the very end of this file
C
C  Future improvements for VRB include:
C  - model the thermal aspect of VRB. This would require the tank, stack, and pump
C    be explicitly modeled.
C  - apply control on the electrolyte's flow rate. This needs a controller to vary
C    the flow rate with the current density. If so, the parasitic loss can no longer
C    be a constant.
C  - The overall thermal resistance of VRB could be correlated with SOC, current and
C    temperature.
C
C INPUTS:
C - IPWC: Power-only component index number
C
C REFERENCES:
C - H. Ribberink, W. Wang, "Improving ESP-r's Battery Model with Active Battery Life Control
C   and Coverage of Vanadium Redox Flow Batteries", Proceedings eSim2008 Conference, Quebec, May 2008.
C
C *********************************************************************
      SUBROUTINE POWOC_CETC_BATTERY(IPWC)
      use h3kmodule
      IMPLICIT NONE
#include "BATTERY.h"
#include "building.h"
#include "plant.h"
#include "power.h"

C External functions.
      integer lnblnk


C ESP-r COMMONs
       COMMON/SIMTIM/IHRP,IHRF,IDYP,IDYF,IDWP,IDWF,NSINC,ITS
       INTEGER IHRP            !-hour of present time-step
       INTEGER IHRF            !-hour of future time-step
       INTEGER IDYP            !-year day number of present day
       INTEGER IDYF            !-year day number of future day
       INTEGER IDWP            !-day of the week of present day
       INTEGER IDWF            !-day of the week of future day
       INTEGER NSINC           !-number of building-side time increments
                               !-since start of simulation
       INTEGER ITS             !-current building time-step within
                               !-current hour

       COMMON/PCTIME/TIMSEC
       REAL    TIMSEC          !-length of time increment (second)

C      COMMON/OUTIN/IUOUT,IUIN
C      INTEGER iuout            !- write unit number
C      INTEGER iuin             !- read unit number

       COMMON/FVALA/TFA(MCOM),QFA(MCOM)
       real    tfa            !- air node temperature, future time-row
       real    qfa            !- energy injected at air node, future time-row


C Common block holding information on the battery's current operation.
C This will be used by the RE-H2 controller for the time being.  A more
C general structure of transporting data regarding the battery will be
C devised in the future.
      COMMON/battery_op_data/maxPBatCharge_common,
     &          maxPBatDischarge_common, RE_H2_control_scenario !, mandChargeCycle
      REAL maxPBatCharge_common      ! Maximum charge rate of battery (W).
      REAL maxPBatDischarge_common   ! Maximum discharge rate of battery (W).
      INTEGER RE_H2_control_scenario     ! Param indicating which control scenario is chosen

C------------------------------------------------------------------------
C PARAMETERS FROM SIGNATURE
C------------------------------------------------------------------------

      INTEGER IPWC

C-------------------------------------------------------------------------
C Function declarations
C-------------------------------------------------------------------------
      REAL PowForCur,MaxCurrent, CurForPow, BatteryCap,
     &     ChargeCurConstVolt

C-------------------------------------------------------------------------
C Local variables
C-------------------------------------------------------------------------

      INTEGER batMode              ! 0 for charge, 1 for discharge, 2 for idle
      INTEGER nTimeStep            !- number of time steps since the beginning of the simulation
      INTEGER iZoneIndex           !- index number of zone containing battery
C      INTEGER DumTime              ! dummy variable for debugging
C      INTEGER StopTime             ! time step to start debugging
      INTEGER nParal, nSerie       ! Number of battery cells connected in paralel and in series

      REAL batTIni, batDODIni,batLoad, pBat, pHeaters,batVolt,
     &     maxPBat, maxPBatCharge, maxPBatDischarge, maxCBat,
     &     maxCBatCharge, maxCBatDischarge, batCurFin,
     &     pIntHeatFin, pParasitic

      REAL ChargeCur               ! Charge current during mandatory charge cycles (Amp)
      REAL ChargePower             ! Charge power during mandatory charge cycles (W)
      REAL CurCover5               ! Current that would recharge an empty battery in 5 hours (Amp)
      REAL CurCover20              ! Current that would recharge an empty battery in 20 hours (Amp)
      REAL batMaxChCur             ! Maximum allowable current during charge mode (Amp)

      REAL dtTStep                 !- Length of the time step (hour)
      REAL zoneT                   !- Zone ambient temperature (oC)
      REAL batSOC                  !- Battery SOC (%)
      REAL heatersRes              !  Resistance of the batteray heaters (Ohm)
      REAL maxPowHeat              !  Maximum power the heaters can provide (V**2/R) (W)
      REAL pBatSystem              !  Actual load of the battery system (part of the load
                                   !    (pDemandBat) that can be met)
      REAL batPowBal               !- Balance of power supplied to/requested from battery and
                                   !  actual power consumption (charge + heaters) or supply
                                   !  (discharge - heater)

      REAL pBatSysCharge           !- Power taken by the battery system in charge mode
      REAL pBatSysDischarge        !- Power provided by the battery system in discharge mode

      INTEGER iNameLength          ! Store the variable's name length (for Reporting)

C-------- For debugging only ----------------------------------------------------------------
C      DumTime = 0
C      StopTime = 2427
C      IF ( NSINC .EQ. StopTime ) THEN
C         DumTime = 1
C      ENDIF
C--------------------------------------------------------------------------------------------

C---------------------------------------------------------------------------

      nTimeStep = NSINC
      dtTStep = TIMSEC / 3600.           ! convert to hour

      IF( POWCOMID(IPWC) .EQ. pba) THEN      ! Lead-acid battery
         iZoneIndex = INT(powcdat(IPWC,47))
         activeBatLifeControl = INT(POWCDAT(IPWC,55))     ! Get info on active battery life control

      ELSEIF(POWCOMID(IPWC) .EQ. vrb) THEN   ! VRB
C     The zone information is not required for the current stage since the thermal aspect of VRB
C     is not considered. It is put here for the reference of future improvement.
         iZoneIndex = INT(powcdat(IPWC,26))

      ELSEIF(POWCOMID(IPWC) .EQ. lion) THEN !Lithium-ion battery
         iZoneIndex = INT(powcdat(IPWC,72))

C-----Add support for other battery types
C     ELSEIF (POWCOMID(IPWC) .EQ.??) THEN
      ENDIF

      zoneT = tfa(iZoneIndex)
      pBatSysCharge = 0.
      pBatSysDischarge = 0.

C************** Initialisation

C See if the time step has been changed and, if so, do the proper initialisation
      IF (nTimeStep .GT. nPreviousTS) THEN
C First time step -> initialise all parameters at the beginning of the simulation
        IF(nTimeStep .EQ. 1) THEN
          Call InitSimulation(IPWC,batTIni,batDODIni)
C Following time steps -> Initialise parameters to their final value at the previous time step
        ELSE
          Call InitTimeStep(IPWC,batTFin,batDODFin,batTIni,batDODIni,
     &                        dtTStep)
        ENDIF     ! 1st time step
      ENDIF     ! nTimeStep


C************** Get mode (charge/discharge)
      IF (mandChargeCycle .NE. 1) THEN      ! Normal operation, the battery mode is determined
                                            ! by the controller based upon the overall power balance
        IF (batDemandP .EQ. 0. ) THEN
           batMode = 2   ! Idle
           batLoad = batDemandP
        ELSEIF (batDemandP .GT. 0.0) THEN
           batMode = 1   ! Discharge
           batLoad = batDemandP
        ELSE
           batMode = 0   ! Charge
           batLoad = -batDemandP   ! set power back to a positive value
        ENDIF
      ELSE    ! The battery is performing a mandatory charge cycle
        batMode = 0
      ENDIF   ! IF (mandChargeCycle .NE. 1)


C************ Calculations

C Determine the maximum curent (maxCBat) and power (maxPBat) for this time-step.
      IF (mandChargeCycle .NE. 1) THEN      ! Normal operation
C Variables used:
C       IPWC is the powoc index number.
C       batTIni is the current temperature of the battery.
C       batDODIni is the current depth of discharge.
C       batMode is an index indicating the mode of operating (e.g. charging).
C       dtTStep is the duration of the simulation time-step (hours)
        maxCBatCharge = MaxCurrent(IPWC,batTIni,batDODIni,0,dtTStep)
        maxCBatDischarge = MaxCurrent(IPWC,batTIni,batDODIni,1,dtTStep)

        maxPBatCharge = PowForCur(IPWC,batTIni,batDODIni,0,dtTStep,
     &                             maxCBatCharge)
        maxPBatDischarge = PowForCur(IPWC,batTIni,batDODIni,1,dtTStep,
     &                             maxCBatDischarge)
      ELSE       ! Mandatory charge cycle
C     If the battery is performing a mandatory charge cycle, the controller's leadership in
C     assigning the load to the battery is carefully overruled. The specific action depends
C     on the phase of the mandatory charge cycle the battery is in.
        IF (mandChargePhase .EQ. 1) THEN     ! Constant current phase of mandatory charge cycle
                                             ! Charge battery with C/5 or batMaxChCur, whichever
                                             ! is smaller until voltage reaches a certain value
                                             ! (batMaxChVolt) or the DOD is smaller than 0.15.
                                             ! The check on this 'until' is done in the subroutine
                                             ! BatteryLife.
                                             ! Current is minimum of batMaxChCur or C/5 (the current
                                             ! that would completely recharge the battery in 5 hours)
C         Determine C/5 and BatMaxChCur - take smaller one
          CurCover5 = BatteryCap(IPWC,batTIni)/5.
          batMaxChCur = POWCDAT(IPWC,7)*POWCDAT(IPWC,1)
          ChargeCur = MIN(CurCover5,batMaxChCur)
C         Calculate MaxP based on this current
          ChargePower = PowForCur(IPWC,batTIni,batDODIni,batMode,
     &                  dtTStep,ChargeCur)
C         Check what to do with existing checks on max DOD etc.
        ELSEIF (mandChargePhase .EQ. 2) THEN     ! Constant voltage phase of mandatory charge cycle
                                                 ! Charge battery at constant voltage (batMaxVoltCh,
                                                 ! e.g. 2.4 V for lead-acid), until battery is full.
                                                 ! The check on this 'until' is done in the subroutine
                                                 ! BatteryLife.
          ChargeCur = ChargeCurConstVolt(IPWC,batTIni,batDODIni,0,
     &                 dtTStep)
          ChargePower = PowForCur(IPWC,batTIni,batDODIni,batMode,
     &                  dtTStep,ChargeCur)
        ELSEIF (mandChargePhase .EQ. 3) THEN     ! 3rd phase of mandatory charge cycle
C         Determine C/20 and BatMaxChCur - take smaller one
          CurCover20 = BatteryCap(IPWC,batTIni)/20.
          batMaxChCur = POWCDAT(IPWC,7)*POWCDAT(IPWC,1)
          ChargeCur = MIN(CurCover20,batMaxChCur)
C         Calculate MaxP based on this current
          ChargePower = PowForCur(IPWC,batTIni,batDODIni,batMode,
     &                  dtTStep,ChargeCur)
C         DOD will get lower than 0.00 . This may give warnings from other subroutines.
        ELSE
C       This should not be possible !!
        ENDIF
        maxPBatCharge = ChargePower
        maxPBatDischarge = 0.
        maxCBatCharge = CurForPow(IPWC,batTIni,batDODIni,batMode,
     &                dtTStep,maxPBatCharge)
        batLoad = ChargePower

      ENDIF ! (mandChargeCycle .NE. 1)

C-----Assign maxPBatCharge and maxPBat Discharge to a common block variable
C-----so that the maximum charge rate and maximum discharge rate
C-----of the battery can be transposed outside this subroutine.  This is an inelegant
C-----solution that should be replaced with a more generalized approach in the future.
      maxPBatCharge_common = maxPBatCharge
      maxPBatDischarge_common = maxPBatDischarge

      IF(batMode .EQ. 1 .OR. batMode .EQ. 2 ) THEN  ! Discharge or idle
         maxCBat = maxCBatDischarge
         maxPBat = maxPBatDischarge
C      ! First, try to see if putting the load on the battery will result in a thermal emergency
         batLoad = MIN(batDemandP, maxPBat)
         IF( POWCOMID(IPWC) .EQ. pba) THEN      ! Lead-acid battery
           CALL DoDischarge(IPWC,batTIni,zoneT,batDODIni,dtTStep,
     &                    maxCBat,maxPBat,batLoad,pBat,pHeaters,
     &                    batVolt,batTFin,batDODFin,batCurFin,
     &                    pIntHeatFin)

C    Evaluate the portion of the load that can be met
           pBatSystem = pBat - pHeaters
C    Evaluate the battery life fraction used in this timestep
           CALL BatteryLife(IPWC,dtTStep,batMode,batCurFin,
     &            batTFin,batDODFin)

         ELSEIF( POWCOMID(IPWC) .EQ. vrb) THEN      ! VRB
           CALL DoDischarge_VRB(IPWC,batTIni,zoneT,batDODIni,dtTStep,
     &               maxCBat,maxPBat,batLoad,pBat,pHeaters,pParasitic,
     &               batVolt,batTFin,batDODFin,batCurFin, pIntHeatFin)

         ELSEIF( POWCOMID(IPWC) .EQ. lion) THEN      ! Li-on
           CALL Lion_BMS(IPWC,batTIni,zoneT,batDODIni,dtTStep,batMode,
     &               maxCBat,maxPBat,batLoad,pBat,pHeaters,pParasitic,
     &               batVolt,batTFin,batDODFin,batCurFin, pIntHeatFin,
     &               hiVoltLion,loVoltLion,hiTempLion,loTempLion,
     &               gas_constant, cycles_used_Lion)
C    Evaluate the portion of the load that can be met
           pBatSystem = pBat - pHeaters - pParasitic
         ENDIF
C    Evaluate the total power balance of the system
         batPowBal = batDemandP - pBatSystem
         pBatSysDischarge = pBatSystem

      ELSE ! Charge
         maxCBat = maxCBatCharge
         maxPBat = maxPBatCharge

         IF( POWCOMID(IPWC) .EQ. pba) THEN      ! Lead-acid battery
           CALL DoCharge(IPWC,batTIni,zoneT,batDODIni,dtTStep,
     &                 maxCBat,maxPBat,batLoad,pBat,pHeaters,
     &                 batVolt,batTFin,batDODFin,batCurFin,
     &                 pIntHeatFin)

C    Evaluate the portion of the load that can be met
           pBatSystem = pBat + pHeaters
C    Evaluate the battery life fraction used in this timestep
           CALL BatteryLife(IPWC,dtTStep,batMode,batCurFin,
     &            batTFin,batDODFin)

         ELSEIF( POWCOMID(IPWC) .EQ. vrb) THEN      ! VRB
           CALL DoCharge_VRB(IPWC,batTIni,zoneT,batDODIni,dtTStep,
     &               maxCBat,maxPBat,batLoad,pBat,pHeaters,pParasitic,
     &               batVolt,batTFin,batDODFin,batCurFin, pIntHeatFin)

         ELSEIF( POWCOMID(IPWC) .EQ. lion) THEN      ! Li-on
           CALL Lion_BMS(IPWC,batTIni,zoneT,batDODIni,dtTStep,batMode,
     &               maxCBat,maxPBat,batLoad,pBat,pHeaters,pParasitic,
     &               batVolt,batTFin,batDODFin,batCurFin, pIntHeatFin,
     &               hiVoltLion,loVoltLion,hiTempLion,loTempLion,
     &               gas_constant, cycles_used_Lion)

C    Evaluate the portion of the load that can be met
           pBatSystem = pBat + pHeaters + pParasitic
         ENDIF

C    Evaluate the total power balance of the system
           batPowBal = batDemandP + pBatSystem
           pBatSysCharge = -1 * pBatSystem

      ENDIF

      nPreviousTS = nTimeStep

C Save variables that are required by control sensors.
      iPow_Time_Row_Count(IPWC) = 2                         ! # of additional data
      fPow_Time_Row_Future(IPWC,1) = (1. - batDODFin)*100.  ! battery SOC, %
      fPow_Time_Row_Future(IPWC,2) = batTFin                ! battery temperature
cx      fPow_Time_Row_Future(IPWC,3) = maxPBatDischarge       ! max discharge rate (alternative to _common)

C power returned/taken to meet the demand
      IF ( batMode .EQ. 1 .OR. batMode .EQ. 2 ) THEN  ! discharge
          ppowoc(ipwc) = pBatSystem                   ! generation is +ve
      ELSE                                            ! charge
          ppowoc(ipwc) = -1. * pBatSystem             ! load is negative
      ENDIF
      qpowoc(ipwc) = 0.                               ! reactive power

C Current and power draw reported should be negative when the battery is being charged.
      IF ( batMode .EQ. 0 ) THEN ! charge
        batCurFin = -1. * batCurFin
        pBatSystem = -1. * pBatSystem
        batPowBal = -1. * batPowBal
      ENDIF


C-------------------------------------------------------
C XML output
C-------------------------------------------------------


C.... Get component name's length
      iNameLength = lnblnk(powcomnam(IPWC))

      Call AddToReport(rvEPowBattDem%Identifier,
     &      batDemandP,
     &      powcomnam(IPWC)(1:iNameLength))

      Call AddToReport(rvEPowBattOper%Identifier,
     &      real(batMode),
     &      powcomnam(IPWC)(1:iNameLength))

      Call AddToReport(rvEPowBattVolt%Identifier,
     &      batVolt,
     &      powcomnam(IPWC)(1:iNameLength))

      Call AddToReport(rvEPowBattDOD%Identifier,
     &      batDODFin,
     &      powcomnam(IPWC)(1:iNameLength))

      Call AddToReport(rvEPowBattCurr%Identifier,
     &      batCurFin,
     &      powcomnam(IPWC)(1:iNameLength))

      Call AddToReport(rvEPowbattIntH%Identifier,
     &      pIntHeatFin,
     &      powcomnam(IPWC)(1:iNameLength))

C Calculate and report SOC in %
       batSOC = (1. - batDODFin ) * 100.

      Call AddToReport(rvEPowBattSOC%Identifier,
     &      batSOC,
     &      powcomnam(IPWC)(1:iNameLength))

      Call AddToReport(rvEPowBattLd%Identifier,
     &      pBat,
     &      powcomnam(IPWC)(1:iNameLength))

      Call AddToReport(rvEPowBattMaxPow%Identifier,
     &      maxPBat,
     &      powcomnam(IPWC)(1:iNameLength))

      Call AddToReport(rvEPowBattMaxPowChrg%Identifier,
     &      maxPBatCharge,
     &      powcomnam(IPWC)(1:iNameLength))

      Call AddToReport(rvEPowBattMaxDschrg%Identifier,
     &      maxPBatDischarge,
     &      powcomnam(IPWC)(1:iNameLength))

C    Calculate and report max battery heater power in W
      nParal = int(POWCDAT(IPWC,1))
      nSerie = int(POWCDAT(IPWC,2))
      heatersRes = POWCDAT(IPWC,13)/(nParal*nSerie)
      maxPowHeat = batVolt**2/heatersRes

      Call AddToReport(rvEPowBattMaxHPow%Identifier,
     &      maxPowHeat,
     &      powcomnam(IPWC)(1:iNameLength))

      Call AddToReport(rvEPowBattHPow%Identifier,
     &      pHeaters,
     &      powcomnam(IPWC)(1:iNameLength))

      Call AddToReport(rvEPowBattPowBal%Identifier,
     &      batPowBal,
     &      powcomnam(IPWC)(1:iNameLength))

      Call AddToReport(rvEPowBattSysLd%Identifier,
     &      pBatSystem,
     &      powcomnam(IPWC)(1:iNameLength))

      Call AddToReport(rvEPowBattSysChrgLd%Identifier,
     &      pBatSysCharge,
     &      powcomnam(IPWC)(1:iNameLength))

      Call AddToReport(rvEPowBattSysDschrgLd%Identifier,
     &      pBatSysDischarge,
     &      powcomnam(IPWC)(1:iNameLength))

      Call AddToReport(rvEPowBattTemp%Identifier,
     &      batTFin,
     &      powcomnam(IPWC)(1:iNameLength))

      Call AddToReport(rvEPowBattLfUsed%Identifier,
     &      batLifeUsed,
     &      powcomnam(IPWC)(1:iNameLength))

      Call AddToReport(rvEPowBattLfUsedCum%Identifier,
     &      cumBatLifeUsed,
     &      powcomnam(IPWC)(1:iNameLength))

      Call AddToReport(rvEPowBattLfUseFac%Identifier,
     &      real(lifeUseFactor),
     &      powcomnam(IPWC)(1:iNameLength))

      Call AddToReport(rvEPowBattChrgCyc%Identifier,
     &      real(mandChargeCycle),
     &      powcomnam(IPWC)(1:iNameLength))

      Call AddToReport(rvEPowBattManChrgPh%Identifier,
     &      real(mandChargePhase),
     &      powcomnam(IPWC)(1:iNameLength))

      Call AddToReport(rvEPowBattManChrgPhInc%Identifier,
     &      real(mandChargePhaseIncrease),
     &      powcomnam(IPWC)(1:iNameLength))

      Call AddToReport(rvEPowBattTmeLstFulChrg%Identifier,
     &      timeSinceLastFullCharge,
     &      powcomnam(IPWC)(1:iNameLength))

      Call AddToReport(rvEPowBattBdTreatFlg%Identifier,
     &      real(abuseFlag),
     &      powcomnam(IPWC)(1:iNameLength))

      Call AddToReport(rvEPowBattCtrlScn%Identifier,
     &      REAL(RE_H2_control_scenario),
     &      powcomnam(IPWC)(1:iNameLength))

      Call AddToReport(rvEPowBattLiOnCycUsed%Identifier,
     &      cycles_used_Lion,
     &      powcomnam(IPWC)(1:iNameLength))

      RETURN
      END

C**************************************************************************
C 1 InitSimulation
C**************************************************************************
C
C Created by: Patrice Pinel
C Initial Creation Date: March, 2005
C
C - This subroutine gets all battery parameters at the begining of the simulation
C
C - This routine is called from POWOC_CETC_Battery
C
C - This subroutine is called only once at the beginning of the simulation
C
C INPUTS: Data from the heather file containing the initial state of the batery
C -IPWC: Index of the power-only component associated with the model
C
C OUTPUTS:
C -BatTempInit: Battery temperature at the begining of the simulation (oC)
C -BatDODInit: Battery Depth Of Discharge at the beginning of  the simulation (%)
C
C See BATTERY.h for definition of other variables
C
C *********************************************************************
      SUBROUTINE InitSimulation(IPWC, batTempInit, batDODInit)
      IMPLICIT NONE
#include "building.h"
#include "plant.h"
#include "power.h"
#include "BATTERY.h"

C PARAMETERS FROM SIGNATURE
       REAL batTempInit, batDODInit
       INTEGER IPWC

C-------------------------------------------------------------------------
C Local variables
C-------------------------------------------------------------------------
C       INTEGER iii                  ! for debugging only

C ************** Initialisation


       batTempInit = POWCDAT(IPWC,9)
       batDODInit = POWCDAT(IPWC,10)

       IF( POWCOMID(IPWC) .EQ. pba) THEN      ! Lead-acid battery
         lifeUseFactor = 0
         batLifeUsed = 0.
         cumBatLifeUsed = 0.
         mandChargeCycle = 0
         mandChargePhase = 0
         mandChargePhaseIncrease = 0
         timeSinceLastFullCharge = 0.

       ELSEIF ( POWCOMID(IPWC) .EQ. lion ) THEN  ! Li-on
         batTempInit = POWCDAT(IPWC,9)
         batDODInit = POWCDAT(IPWC,10)
         mandChargeCycle = 0 ! This is to get by a road block set by the lead acid later
         if (abuseFlag.eq.0) then
           cycles_used_Lion = 0 ! This is the amount of cycles the cell has used for degradation
                                ! if abuseFlag is set .ne.0 (in Res_elec_Ctl.F, approx. line 1000),
                                ! the value is not changed, here!
         endif
cx         write(*,*)"*** cycles_used_Lion=",cycles_used_Lion
         loVoltLion = 0 ! These are flags initially set to zero to check that Li-on battery is within
         hiVoltLion = 0 ! safe limits
         loTempLion = 0
         hiTempLion = 0
       ENDIF

C   For debugging only:
C         iii = 1
C         DO WHILE (iii <= 60)
C           WRITE (IUOUT,*) ' item ', iii, ' = ', POWCDAT(IPWC,iii)
C           iii = iii + 1
C         ENDDO

      RETURN
      END

C**************************************************************************
C 2 InitTimeStep
C**************************************************************************
C
C Created by: Patrice Pinel
C Initial Creation Date: March, 2005
C
C - This subroutine initialises some battery parameters
C   at the begining of a time step
C   to their value at the end of the preceding time step
C
C - This routine is called from POWOC_CETC_Battery
C
C - This subroutine is called only once at the begining of every time step
C
C INPUTS:
C -IPWC: Index of the power-only component associated with the model
C -batTFinal: Battery temperature at the end of a time step (oC)
C -DODFinal: Battery Depth Of Discharge at the end of a time step (%)
C -dtTStep: Duration of the time step (hour)
C
C OUTPUTS:
C -batTInit: Battery temperature at the begining of a time step (oC)
C -DODInit: Battery Depth Of Discharge at the begining of a time step (%)
C
C See BATTERY.h for definition of other variables
C
C *********************************************************************
      SUBROUTINE InitTimeStep(IPWC,batTFinal,DODFinal,batTInit,DODInit,
     &                          dtTStep)
      IMPLICIT NONE
#include "building.h"
#include "plant.h"
#include "power.h"
#include "BATTERY.h"

C PARAMETERS FROM SIGNATURE
      REAL batTFinal,DODFinal,batTInit,DODInit,dtTStep
      INTEGER IPWC


C-------------------------------------------------------------------------
C Local variables
C-------------------------------------------------------------------------
      REAL durationChargeCycle     ! Approx. duration of mandatory charge cycle (hours)
      REAL mandChargeInterval      ! Interval between mandatory charge cycles (hours)

C Initialise values at the beginning of the time step to their value at the end of the previous one
      batTInit = batTFinal
      DODInit = DODFinal

C Update battery life parameters, decide upon starting / ending mandatory charge phase,
C set flag indicating battery is being abused.
      IF( POWCOMID(IPWC) .EQ. pba) THEN      ! Lead-acid battery
C       Add battery life used during previous time step to the total battery life used
        cumBatLifeUsed = cumBatLifeUsed + batLifeUsed

C       Active battery life preservation by performing mandatory charge cycles
        IF (activeBatLifeControl .EQ. 1) THEN     ! active control
C       Evaluate whether the battery must start a mandatory charge cycle
          IF (mandChargePhase .EQ. 0) THEN     ! Normal operation between mandatory charge phase
C           Check whether battery is full
            IF (batDODFin .LE. 0.001) THEN     ! Battery is full
C             Reset counter
              timeSinceLastFullCharge = 0.
            ELSE
C             Update counter
              timeSinceLastFullCharge = timeSinceLastFullCharge
     &                                         + dtTStep
C             Compare time elapsed since last full charge to mandatory charge interval
              mandChargeInterval = POWCDAT(IPWC,56) * 24.     ! To make it hours
              IF (timeSinceLastFullCharge .GE.
     &                     mandChargeInterval) THEN
                mandChargeCycle = 1
                mandChargePhase = 1
              ENDIF     ! timeSinceLastFullCharge
            ENDIF     ! batDODFin
          ELSE
C           Go to next phase of mandatory cycle if appropriate
            mandChargePhase = mandChargePhase +
     &                            mandChargePhaseIncrease
            IF (mandChargePhase .EQ. 4) THEN     ! Mandatory Charge Cycle completed !
                                                 ! Reset all variables to 0
              mandChargeCycle = 0
              mandChargePhase = 0
              timeSinceLastFullCharge = 0.
              DODInit = 0.                       ! Reset from -0.05 to account for losses during
                                                 ! last step of mandatory charge cycle
            ENDIF
            mandChargePhaseIncrease = 0
          ENDIF     ! mandChargePhase
        ELSE     ! No active battery life control.
                 ! Check to see whether abuseFlag should be set
C         Check whether battery is full
          IF (batDODFin .LE. 0.001) THEN     ! Battery is full
C           Reset counter
            timeSinceLastFullCharge = 0.
            abuseFlag = 0
          ELSE
C           Check whether time elapsed since last time battery was full is greater than
C           mandChargeInterval.
C           If so, set 'abuseFlag'
C           Update counter
            timeSinceLastFullCharge = timeSinceLastFullCharge
     &                                      + dtTStep
C             Compare time elapsed since last full charge to mandatory charge interval
            mandChargeInterval = POWCDAT(IPWC,56) * 24.   ! To make it hours
            durationChargeCycle = POWCDAT(IPWC,57)        ! Estimated maximum duration of
                                                          ! the mandatory charge cycle (hours)
            IF (timeSinceLastFullCharge .GE.
     &          (mandChargeInterval + durationChargeCycle)) THEN
              abuseFlag = 1
            ENDIF     ! timeSinceLastFullCharge
          ENDIF     ! batDODFin
        ENDIF     ! active battery life control


      ELSEIF ( POWCOMID(IPWC) .EQ. lion) THEN            ! Li-on check to see if battery is in safe limits

        hiTempLion = 0 ! set all the flags back to zero from the previous simulation
        loTempLion = 0
        hiVoltLion = 0
        loVoltLion = 0

        IF (batTFinal .GT. POWCDAT(IPWC,16))    THEN     ! Battery is too hot for safe operation
          hiTempLion = 1                                 ! Set high temp flag on
        ELSEIF (batTFinal .LT. POWCDAT(IPWC,17)) THEN    ! Battery is too cold for safe operation
          loTempLion = 1                                 ! Set low temp flag on
        ENDIF

        IF (DODFinal .LT. POWCDAT(IPWC,15))  THEN        ! Battery is overcharged for safe operation
          hiVoltLion = 1                                 ! Set overvoltage flag on
        ELSEIF (DODFinal .GT. POWCDAT(IPWC,14)) THEN     ! Battery is undercharged for safe operation
          loVoltLion = 1                                 ! Set undervoltage flag on
        ENDIF

      ENDIF     ! Lead-acid battery and lithium ion
      RETURN
      END

C**************************************************************************
C 3 FreeVoltage
C**************************************************************************
C
C Created by: Patrice Pinel
C Initial Creation Date: March, 2005
C
C - This subroutine returns the free voltage of the battery
C   as a function of its depth of discharge
C
C This routine is called from various points of the program
C
C INPUTS:
C -batDOD: The battery's depth of discharge
C -IPWC: Power-only component index
C
C OUTPUTS:
C -returns the battery's free voltage
C
C *********************************************************************
      REAL FUNCTION FreeVoltage(IPWC,batDOD)
      IMPLICIT NONE
#include "BATTERY.h"
#include "building.h"
#include "plant.h"
#include "power.h"

      COMMON/OUTIN/IUOUT,IUIN
      INTEGER iuout          !- write unit number
      INTEGER iuin           !- read unit number

C Common holding NSINC (no of building time step increments) for debugging only
       COMMON/SIMTIM/IHRP,IHRF,IDYP,IDYF,IDWP,IDWF,NSINC,ITS
       INTEGER IHRP            !-hour of present time-step
       INTEGER IHRF            !-hour of future time-step
       INTEGER IDYP            !-year day number of present day
       INTEGER IDYF            !-year day number of future day
       INTEGER IDWP            !-day of the week of present day
       INTEGER IDWF            !-day of the week of future day
       INTEGER NSINC           !-number of building-side time increments
                               !-since start of simulation
       INTEGER ITS             !-current building time-step within
                               !-current hour

C PARAMETERS FROM SIGNATURE
      REAL batDOD
      INTEGER IPWC

C Coefficients for the freeVoltage equation
      REAL E0CoefA, E0CoefB, E0CoefC, E0CoefD, E0ExpB, E0ExpC, E0ExpD
      INTEGER nSerie ! number of battery cells connected in series

C Battery Parameters (technology and configuration)
      REAL batMaxVolt      ! Maximum manufacturer voltage of a battery cell (0% DOD) (V)
      REAL batMinVolt      ! Minimum manufacturer voltage of a battery cell (100% DOD) (V)

C************** Initialisation

C These will be changed for the proper references once the "ACEP_BATTERY.h" will be replaced with the
C proper node in the electrical network
      E0CoefA = POWCDAT(IPWC,15)
      E0CoefB = POWCDAT(IPWC,16)
      E0CoefC = POWCDAT(IPWC,17)
      E0CoefD = POWCDAT(IPWC,18)
      E0ExpB = POWCDAT(IPWC,19)
      E0ExpC = POWCDAT(IPWC,20)
      E0ExpD = POWCDAT(IPWC,21)
      nSerie = int(POWCDAT(IPWC,2))

C     Voltages are multiplied by the number of units connected in series
      batMaxVolt = POWCDAT(IPWC,5) * nSerie
      batMinVolt = POWCDAT(IPWC,6) * nSerie

C ************** Calculations

C     Free voltage of the battery is the free voltage of a cell time the number of cells in series
      FreeVoltage = (E0CoefA + E0CoefB*batDOD**E0ExpB +
     &               E0CoefC*batDOD**E0ExpC +
     &               E0CoefD*batDOD**E0ExpD)*nSerie

C     Make sure value is within boundaries
C     This check is partly bypassed if the lead-acid battery is performing a mandatory
C     charge cycle, because then the voltage is expected to surpass the batMaxVolt
      IF (FreeVoltage .GT. batMaxVolt) THEN
        if (mandChargeCycle .NE. 1) then
          WRITE(IUOUT,*) 'Warning Battery Model: Free voltage above ',
     &                  'max in function FreeVoltage', (NSINC)
          FreeVoltage = batMaxVolt
        endif
      ELSEIF(FreeVoltage .LT. batMinVolt) THEN
        WRITE(IUOUT,*) 'Warning Battery Model: Free voltage below ',
     &                 'min in function FreeVoltage', (NSINC)
        FreeVoltage = batMinVolt
      ENDIF

      END

C**************************************************************************
C 4 BatteryCap
C**************************************************************************
C
C Created by: Patrice Pinel
C Initial Creation Date: March, 2005
C
C - This subroutine returns the capacity of the battery
C   as a function of its temperature
C   and the number of battery cells connected in parallel and in series
C
C This routine is called from various points of the program
C
C INPUTS:
C -batteryTemp: The battery's temperature (oC)
C -IPWC: Power-only component index number
C
C OUTPUTS:
C -returns the battery's capacity (Ah)
C
C *********************************************************************
      REAL FUNCTION BatteryCap(IPWC,batTemp)
      IMPLICIT NONE
#include "building.h"
#include "plant.h"
#include "power.h"

      COMMON/OUTIN/IUOUT,IUIN
      INTEGER iuout          !- write unit number
      INTEGER iuin           !- read unit number

C PARAMETERS FROM SIGNATURE
      REAL batTemp
      INTEGER IPWC

C Battery parameters
      REAL cellCapRef
      INTEGER nParal
C Coefficients for the capacity equation
      REAL capCoefA, capCoefB, capCoefC

C************** Initialisation

      capCoefA = POWCDAT(IPWC,22)
      capCoefB = POWCDAT(IPWC,23)
      capCoefC = POWCDAT(IPWC,24)
      cellCapRef = POWCDAT(IPWC,3)
      nParal = int(POWCDAT(IPWC,1))

C************** Calculations
C     Capacity of the battery is the capacity of one cell times the number of cells in parallel
C
      BatteryCap = cellCapRef*nParal*
     &             (capCoefA+capCoefB*batTemp+capCoefC*batTemp**2)

C     Make sure no negative value is returned
      IF (BatteryCap .LE. 0.0) THEN
        WRITE(IUOUT,*) 'Warning Battery Model: Negative or ',
     &                 'Nul capacity in function BatteryCap'
        BatteryCap = 0.01
      ENDIF

      END

C**************************************************************************
C 5 BatteryRInt
C**************************************************************************
C
C Created by: Patrice Pinel
C Initial Creation Date: March, 2005
C
C - This subroutine returns the internal resistance of the battery
C   as a function of its temperature, the current, the depth of discharge
C   and the number of battery cells connected in parallel and in series
C - The polynom will eventually be changed for a form that better suits available data
C
C This routine is called from various points of the program
C
C INPUTS:
C -batTemp: The battery's temperature (oC)
C -batDOD: The battery's depth of discharge
C -batI: The battery's current
C -BatMode: 0 for charge, 1 for discharge
C -IPWC: Power-only component index number
C
C OUTPUTS:
C -returns the battery's internal resistance (Ohm)
C
C *********************************************************************
      REAL FUNCTION BatteryRInt(IPWC,batTemp,batDOD,batI,batMode)
      IMPLICIT NONE
#include "building.h"
#include "plant.h"
#include "power.h"

      COMMON/OUTIN/IUOUT,IUIN
      INTEGER iuout          !- write unit number
      INTEGER iuin           !- read unit number

C PARAMETERS FROM SIGNATURE
      REAL batTemp,batDOD,batI
      INTEGER batMode,IPWC

C Battery parameters
      INTEGER nParal,nSerie ! number of battery cells connected in series and parallel
C Coefficients for the capacity equation
      REAL cellCapRef, rIntCA,rIntCB,rIntCC,rIntCD,rIntCH,
     &     rIntEB, rIntEC, rIntED, rIntEH1, rIntEH2, rIntEH3

C************** Initialisation

      IF(batMode .EQ. 0) THEN   ! charge mode -> use charge coefficients
         rIntCA = POWCDAT(IPWC,25)
         rIntCB = POWCDAT(IPWC,26)
         rIntCC = POWCDAT(IPWC,27)
         rIntCD = POWCDAT(IPWC,28)
         rIntCH = POWCDAT(IPWC,29)
         rIntEB = POWCDAT(IPWC,30)
         rIntEC = POWCDAT(IPWC,31)
         rIntED = POWCDAT(IPWC,32)
         rIntEH1 = POWCDAT(IPWC,33)
         rIntEH2 = POWCDAT(IPWC,34)
         rIntEH3 = POWCDAT(IPWC,35)
      ELSE   ! discharge mode -> use discharge coefficient
         rIntCA = POWCDAT(IPWC,36)
         rIntCB = POWCDAT(IPWC,37)
         rIntCC = POWCDAT(IPWC,38)
         rIntCD = POWCDAT(IPWC,39)
         rIntCH = POWCDAT(IPWC,40)
         rIntEB = POWCDAT(IPWC,41)
         rIntEC = POWCDAT(IPWC,42)
         rIntED = POWCDAT(IPWC,43)
         rIntEH1 = POWCDAT(IPWC,44)
         rIntEH2 = POWCDAT(IPWC,45)
         rIntEH3 = POWCDAT(IPWC,46)
      ENDIF
      nSerie = int(POWCDAT(IPWC,2))
      nParal = int(POWCDAT(IPWC,1))
      cellCapRef = POWCDAT(IPWC,3)

C************** Calculation
      !current for only one cell -> /nParal
      IF (nParal .GT. 0.0 .AND. cellCapRef .GT. 0.0) THEN
         BatteryRInt = FLOAT(nSerie)/FLOAT(nParal)*
     &   (rIntCA + rIntCB*batDOD**rIntEB +
     &    rIntCC*(batI/nParal/cellCapRef)**rIntEC +
     &    rIntCD*batTemp**rIntED+rIntCH*batDOD**rIntEH1+
     &    rIntCD*batTemp**rIntEH2*(batI/nParal/cellCapRef)**rIntEH3)

      ELSE
         WRITE(IUOUT,*) 'Error Battery Model: zero Parallel ',
     &                  'connections or reference battery ',
     &                  'capacity defined '
      ENDIF

C Make sure no negative value is returned
      IF (BatteryRInt .LE. 0.0) THEN
         WRITE(IUOUT,*) 'Warning Battery Model: Negative or Nul ',
     &                  ' resistance in routine BatteryRInt'
         BatteryRInt = 0.01
      ENDIF

      END

C**************************************************************************
C 6 MaxCurrent
C**************************************************************************
C
C Created by: Patrice Pinel
C Initial Creation Date: March, 2005
c
C - This subroutine returns the maximal current the battery can charge/discharge with
C   without crossing the voltage limits
C - The temperature at the begining of the time step is used to evaluate properties ->
C   the problem is decoupled as far as temperatures are concerned except for the lion
C   algorithm
C
C This routine is called from various points of the program
C
C INPUTS:
c -batTemp: The battery's temperature (oC)
c -DODInit: The battery's initial depth of discharge
C -batMode: 0 for charge, 1 for discharge
C -dtTStep: Duration of the time step (hour)
C -IPWC: power-only component index number
!
! OUTPUTS:
! -returns the maximum allowable current (Amp)
!
! *********************************************************************
      REAL FUNCTION MaxCurrent(IPWC,batTemp,DODInit,batMode,dtTStep)
      IMPLICIT NONE
#include "building.h"
#include "plant.h"
#include "power.h"
#include "BATTERY.h"

      COMMON/OUTIN/IUOUT,IUIN
      INTEGER iuout          !- write unit number
      INTEGER iuin           !- read unit number

C PARAMETERS FROM SIGNATURE
      REAL batTemp,DODInit,dtTStep
      INTEGER batMode,IPWC

C Battery Parameters (technology and configuration)
      REAL batMaxVolt      ! Maximum manufacturer voltage of a battery cell (0% DOD) (V)
      REAL batMaxVoltCh    ! Maximum manufacturer charging voltage of a battery cell (V)
      REAL batMinVolt      ! Minimum manufacturer voltage of a battery cell (100% DOD) (V)
      REAL batMaxCur     ! Maximum allowable current during charge mode (Amp)
      REAL upperBoundDOD   ! DOD limit, upper bound
      REAL lowerBoundDOD   ! DOD limit, lower bound

C Declaration of functions used
      REAL BatteryRInt,FreeVoltage,FinalDOD
      REAL RInt_VRB, OCV_VRB, FinalDOD_VRB
      REAL RInt_Lion, OCV_Lion, FinalDOD_Lion

C Local variables
      REAL relaxFact    ! Relaxation factor for iteration loop (to reduce oscillations)
      REAL convVal      ! Convergence is assumed when difference between 2 iterations is smaller than this factor
      REAL batCurrent   ! Battery current (Amp)
      REAL dumCurrent   ! Dummy current used to see the current evolution from time step to time step (Amp)
      REAL lastGoodCur  ! Last curent that did not exceed the DOD limits (0-100%)
      REAL DODEnd       ! Depth of discharge at the end of the time step
      REAL FreeVEnd     ! Free Voltage at the end of the time step (Volt)
      REAL FreeVAvg     ! Free Voltage at the center of the time step (Volt)
      REAL RIntEnd      ! Internal resistance at the end of the time step (Ohm)
      REAL RIntAvg      ! Internal resistance at the center of the time step (Ohm)
      INTEGER numIter   ! Number of iterrations done
      INTEGER maxIter   ! Max number of iterrations before we consider there is a convergence problem

C************** Initialisation
C     Voltages are multiplied by the number of units connected in series
      batMaxVolt = POWCDAT(IPWC,5)*POWCDAT(IPWC,2)
      batMinVolt = POWCDAT(IPWC,6)*POWCDAT(IPWC,2)
      batMaxVoltCh = POWCDAT(IPWC,8)*POWCDAT(IPWC,2)
C     Currents are multiplied by the number of units connected in parallel
      batMaxCur = POWCDAT(IPWC,7)*POWCDAT(IPWC,1)

      ! Assign value for relaxation factor
      relaxFact = 0.1
      ! Assign convergence criteria
      convVal = 0.001
      ! initialise the number of iterations
      numIter = 0
      maxIter = 100

C     Initialise curents to zero
      batCurrent = 0
      lastGoodCur = 0

C************** Calculation
      ! initialise the dummy current to the value of the current at the last iteration
610   dumCurrent = batCurrent

      IF( POWCOMID(IPWC) .EQ. pba) THEN      ! Lead-acid battery
C      The position 48, 49 is temporarily set here, needs to be changed once the
C      input parameters are finalized.
         upperBoundDOD = POWCDAT(IPWC,48)
         lowerBoundDOD = POWCDAT(IPWC,49)
         DODEnd = FinalDOD(IPWC,batTemp,DODInit,batMode,dtTStep,
     &             batCurrent)
         FreeVEnd = FreeVoltage(IPWC,DODEnd)
         FreeVAvg = FreeVoltage(IPWC,(DODInit+DODEnd)/2.0)
         RIntEnd = BatteryRInt(IPWC,batTemp,DODEnd,batCurrent,batMode)
         RIntAvg = BatteryRInt(IPWC,batTemp,(DODInit+DODEnd)/2.0,
     &             batCurrent,batMode)
      ELSEIF(POWCOMID(IPWC) .EQ. vrb) THEN   ! VRB
         upperBoundDOD = POWCDAT(IPWC,14)
         lowerBoundDOD = POWCDAT(IPWC,15)
         DODEnd = FinalDOD_VRB(IPWC,DODInit,batMode,dtTStep,
     &             batCurrent)
         FreeVEnd = OCV_VRB(IPWC,DODEnd, batCurrent,batMode)
         FreeVAvg = OCV_VRB(IPWC,(DODInit+DODEnd)/2.0, batCurrent,
     &                      batMode)
         RIntEnd = RInt_VRB(IPWC,batTemp,DODEnd,batCurrent,batMode)
         RIntAvg = RInt_VRB(IPWC,batTemp,(DODInit+DODEnd)/2.0,
     &             batCurrent,batMode)
      ELSEIF ( POWCOMID(IPWC) .EQ. lion) THEN  !Lion
         upperBoundDOD = POWCDAT(IPWC,14)
         lowerBoundDOD = POWCDAT(IPWC,15)
         DODEnd = FinalDOD_Lion(IPWC,batTemp,DODInit,batMode,dtTStep,
     &             batCurrent)
         FreeVEnd = OCV_Lion(IPWC,DODEnd,batTemp)
         FreeVAvg = OCV_Lion(IPWC,(DODInit+DODEnd)/2.0,batTemp)
         RIntEnd = RInt_Lion(IPWC, DODEnd,batMode,batCurrent)
         RIntAvg = RInt_Lion(IPWC,(DODInit+DODEnd)/2.0,batMode
     &             ,batCurrent)
      ENDIF

      IF(batMode .EQ. 0) THEN    ! charge
        IF (DODEnd .LT. lowerBoundDOD) THEN
           ! DOD exceeds limit -> current too high

           ! Bissectrice method
           ! Average of the actual current (that exceeds DOD limit) and last current that did not exceed
           batCurrent = (batCurrent+lastGoodCur)/2.0

         ELSE
            ! This current does not exceed limits

            ! Reset the good current to the actual current
            lastGoodCur = batCurrent

C           Adjust current so that (V-E0) = Rint*I results in V=Vmax_allowable
C           at the end of the time step
            batCurrent = relaxFact*(batMaxVoltCh - FreeVEnd)/RIntEnd
     &                   + (1-relaxFact) * batCurrent

C           Make sure the current does not exceed the maximum allowable current
            batCurrent = MIN(batCurrent,batMaxCur)
         ENDIF  ! Depth Of Discharge below lower limit
      ELSE  ! Discharge
         IF ((DODEnd .GT. upperBoundDOD) .OR.
     &      (batCurrent>FreeVAvg/2.0/RIntAvg)) THEN
         ! DOD exceeds limit -> current too high   or
         ! I=[E0+-sqrt(E0^2-4RP)]/2R => Pmax < E0^2/4R => I max disch = E0/2R

         ! Bissectrice method
         ! Average of the actual current (that exceeds DOD limit) and last current that did not exceed
            batCurrent = (batCurrent+lastGoodCur)/2.0

         ELSE
         ! This current does not exceed limits

         ! Reset the good current to the actual current
            lastGoodCur = batCurrent

C           Adjust current so that (E0 - V) = Rint*I results in V=Vmin at the end of the time step
            batCurrent = relaxFact*(FreeVEnd - batMinVolt)/RIntEnd
     &                      + (1-relaxFact) * batCurrent

        ENDIF ! DOD
      ENDIF ! Mode

C     Increment the number of iterations
      numIter = numIter + 1

      IF((ABS(batCurrent - dumCurrent).GE.convVal).AND.
     &   (numIter.LT.maxIter)) THEN
      ! Not converged and iteration limit not exceeded -> repeat loop
         GOTO 610
      ENDIF

      IF(numIter.GT.maxIter)  THEN
      !Max number of iterations exceeded -> give warning
        WRITE(IUOUT,*) 'Warning Battery Model: Troublesome ',
     &                 'convergence in function MaxCurrent '
      ENDIF

C     Assign function result
      MaxCurrent = batCurrent

      END

C**************************************************************************
C 7 PowForCur
C**************************************************************************
C
C Created by: Patrice Pinel
C Initial Creation Date: March, 2005
C
C Modified by Neil Saldanha January 2010
C
C - This subroutine returns the power the battery charges/discharges with
C   corresponding to a given current
C - The temperature at the begining of the time step is used to evaluate properties ->
C   the problem is decoupled as far as temperatures are concerned
C
C This routine is called from various points of the program
C
C INPUTS:
C -batTemp: The battery's temperature (oC)
C -DODInit: The battery's initial depth of discharge (%)
C -batMode: 0 for charge, 1 for discharge
C -dtTStep: Duration of the time step (hour)
C -IPWC: Power-only component index number
C -batCur: The battery current
C
C OUTPUTS:
C -returns the corresponding average power (W)
C
C *********************************************************************
      REAL FUNCTION PowForCur(IPWC,batTemp,DODInit,batMode,dtTStep,
     &                        batCur)
      IMPLICIT NONE
#include "building.h"
#include "plant.h"
#include "BATTERY.h"
#include "power.h"

      COMMON/OUTIN/IUOUT,IUIN
      INTEGER iuout          !- write unit number
      INTEGER iuin           !- read unit number

C PARAMETERS FROM SIGNATURE
      REAL batTemp, DODInit, dtTStep, batCur
      INTEGER batMode,IPWC

C Declaration of functions used
      REAL FreeVoltage,FinalDOD,BatteryRInt
      REAL OCV_VRB, FinalDOD_VRB, RInt_VRB
      REAL RInt_Lion, OCV_Lion, FinalDOD_Lion

C Battery Parameters (technology and configuration)
      REAL batMaxVoltCh    ! Maximum manufacturer charging voltage of a battery cell (V)
      REAL batMinVolt      ! Minimum manufacturer voltage of a battery cell (100% DOD) (V)

C Local variables
      REAL DODAvg       ! Average Depth of discharge during time step if max current is used (%)
      REAL E0Avg        ! Average free voltage during timestep at maxCur (V)
      REAL VAvg         ! Average voltage during timestep at maxCur (V)
      REAL RintAvg      ! Internal resistance at DODAvg during timestep at maxCur (Ohm)

C************** Initialisation
C     Voltages are multiplied by the number of units connected in series
      batMinVolt = POWCDAT(IPWC,6)*POWCDAT(IPWC,2)
      batMaxVoltCh = POWCDAT(IPWC,8)*POWCDAT(IPWC,2)

C************** Calculation

      IF( POWCOMID(IPWC) .EQ. pba) THEN      ! Lead-acid battery
         DODAvg = FinalDOD(IPWC,batTemp,DODInit,batMode,
     &                    dtTStep/2.0,batCur)
         E0Avg = FreeVoltage(IPWC,DODAvg)
         RintAvg = BatteryRInt(IPWC,batTemp,DODAvg,batCur,batMode)
      ELSEIF(POWCOMID(IPWC) .EQ. vrb) THEN   ! VRB
         DODAvg = FinalDOD_VRB(IPWC,DODInit,batMode,
     &                    dtTStep/2.0,batCur)
         E0Avg = OCV_VRB(IPWC,DODAvg,batCur,batMode)
         RintAvg = RInt_VRB(IPWC,batTemp,DODAvg,batCur,batMode)
      ELSEIF ( POWCOMID(IPWC) .EQ. lion) THEN  !Lion
         DODAvg = FinalDOD_Lion(IPWC,batTemp,DODInit,batMode,
     &                    dtTStep/2.0,batCur)
         E0Avg = OCV_Lion(IPWC,DODAvg,batTemp)
         RintAvg = RInt_Lion(IPWC,DODAvg,batMode,batCur)
      ENDIF


C     Patrice's original code
C     V=E0-RI
C      VAvg = E0Avg-batCur*RintAvg

C *** Modification by Hajo Ribberink to account for difference between charging and discharging
      IF (batMode .EQ. 0) THEN   ! charge
C        V=E0+RI
         VAvg = E0Avg+batCur*RintAvg
      ELSE  ! Discharge
C        V=E0-RI
         VAvg = E0Avg-batCur*RintAvg
      ENDIF
C ***********************************************************************************************

C     Make a check that the voltage in in the acceptable range
C     This check is partly bypassed if the lead-acid battery is performing a mandatory
C     charge cycle, because then the voltage is expected to surpass the batMaxVolt
      IF(VAvg.LT.batMinVolt) THEN
        !Abnormal Voltage -> give warning
           WRITE(IUOUT,*) 'Warning Battery Model: Abnormal voltage ',
     &                    ' in function PowForCur'
      ELSEIF (VAvg.GT.batMaxVoltCh)  THEN
        if (mandChargeCycle .NE. 1) then
          !Abnormal Voltage -> give warning
           WRITE(IUOUT,*) 'Warning Battery Model: Abnormal voltage ',
     &                    ' in function PowForCur'
        else
           PowForCur = batCur*VAvg
        endif
      ELSE
C          P=VI
           PowForCur = batCur*VAvg
      ENDIF

      END

C**************************************************************************
C 8 CurForPow
C**************************************************************************
C
C Created by: Patrice Pinel
C Initial Creation Date: March, 2005
C
C - This subroutine returns the current that must flow through the battery
C   in order to obtain a certain power
C
C This routine is called from various points of the program
C
C INPUTS:
C -batTemp: The battery's temperature (oC)
C -DODInit: The battery's initial depth of discharge (%)
C -batMode: 0 for charge, 1 for discharge
C -dtTStep: Duration of the time step (hour)
C -batPow: Load on the battery (W) (always positive)
C -IPWC: power-only component index number
C
C OUTPUTS:
C -returns the current corresponding to a certain power generated by the battery (Amp)
C  (always positive)
C
C *********************************************************************
      REAL FUNCTION CurForPow(IPWC,batTemp,DODInit,batMode,dtTStep,
     &                        batPow)
      IMPLICIT NONE
#include "building.h"
#include "plant.h"
#include "power.h"
#include "BATTERY.h"

      COMMON/OUTIN/IUOUT,IUIN
      INTEGER iuout          !- write unit number
      INTEGER iuin           !- read unit number

C PARAMETERS FROM SIGNATURE
      REAL batTemp, DODInit, dtTStep, batPow
      INTEGER batMode,IPWC

C Declaration of functions used
      REAL BatteryRInt, FreeVoltage, FinalDOD
      REAL OCV_VRB, RInt_VRB, FinalDOD_VRB
      REAL OCV_lion, RInt_lion, FinalDOD_lion

C Local variables
      REAL DODAvg       ! Average DOD during time step (DODInit+DODEnd)/2
      REAL E0Avg        ! Free voltage corresponding to the average conditions
      REAL RIntAvg      ! Internal resistance corresponding to the average conditions
      REAL relaxFact    ! Relaxation factor for iteration loop (to reduce oscillations)
      REAL convVal      ! Convergence is assumed when difference between 2 iterations is smaller than this factor
      REAL batCurrent   ! Battery current (Amp)
      REAL dumCurrent   ! Dummy current used to see the current evolution from time step to time step (Amp)
      INTEGER numIter   ! Number of iterrations done
      INTEGER maxIter   ! Max number of iterrations before we consider there is a convergence problem

C************** Initialisation
      ! Assign value for relaxation factor
      relaxFact = 0.1
      ! Assign convergence criteria
      convVal = 0.001
      ! initialise the number of iterations
      numIter = 0
      maxIter = 100

      batCurrent = 0.0

C************** Calculation
C initialise the dummy current to the value of the current at the last iteration
810   dumCurrent = batCurrent



      IF( POWCOMID(IPWC) .EQ. pba) THEN      ! Lead-acid battery
         DODAvg = FinalDOD(IPWC,batTemp,DODInit,batMode,dtTStep/2.0,
     &                  batCurrent)
         RIntAvg = BatteryRInt(IPWC,batTemp,DODAvg,batCurrent,batMode)
         E0Avg = FreeVoltage(IPWC,DODAvg)
      ELSEIF(POWCOMID(IPWC) .EQ. vrb) THEN   ! VRB
         DODAvg = FinalDOD_VRB(IPWC,DODInit,batMode,
     &                  dtTStep/2.0, batCurrent)
         RIntAvg = RInt_VRB(IPWC,batTemp,DODAvg,batCurrent,batMode)
         E0Avg = OCV_VRB(IPWC, DODAvg, batCurrent, batMode)
      ELSEIF(POWCOMID(IPWC) .EQ. lion) THEN   ! Li-on
         DODAvg = FinalDOD_Lion(IPWC,batTemp,DODInit,batMode,
     &                    dtTStep/2.0,batCurrent)
         RintAvg = RInt_Lion(IPWC,DODAvg,batMode,batCurrent)
         E0Avg = OCV_Lion(IPWC,DODAvg,batTemp)

      ENDIF

      IF (batMode .EQ. 0) THEN   ! charge
         batCurrent=(-E0Avg+(E0Avg**2.+4.*batPow*RIntAvg)**0.5)/2./
     &   RIntAvg
      ELSE  ! Discharge
         batCurrent=(E0Avg-(E0Avg**2.-4.*batPow*RIntAvg)**0.5)/2./
     &   RIntAvg
      ENDIF

      numIter = numIter + 1

      IF((ABS(batCurrent - dumCurrent).GT.convVal).AND.
     &   (numIter.LT.maxIter)) THEN
         ! Not converged -> repeat loop
         GOTO 810
      ENDIF

      IF(numIter.GE.maxIter)  THEN
C Max number of iterations exceeded -> give warning
        WRITE(IUOUT,*) 'Warning Battery Model: Troublesome ',
     &                 'convergence in function CurForPow '
      ENDIF

C     Assign function result
      CurForPow = batCurrent

      END

C**************************************************************************
C 9 InternalHeat
C**************************************************************************
C
C Created by: Patrice Pinel
C Initial Creation Date: March, 2005
C
C - This subroutine returns the heat generated by a battery's internal resistance
C - The temperature at the begining of the time step is used to evaluate properties ->
C   the problem is decoupled as far as temperatures are concerned
C
C This routine is called from various points of the program
C
C INPUTS:
C -batTemp: The battery's temperature (oC)
C -DODInit: The battery's initial depth of discharge
C -batMode: 0 for charge, 1 for discharge
C -dtTStep: Duration of the time step (hour)
C -batCur:  Current flowing through the battery (Amp)
C -IPWC: Power-only component index number
C
C OUTPUTS:
C -returns the heat generated by the current flowing through the
C  battery's internal resistance (W)
C
C *********************************************************************
      REAL FUNCTION InternalHeat(IPWC,batTemp,DODInit,batMode,dtTStep,
     &                           batCur)
      IMPLICIT NONE
#include "building.h"
#include "plant.h"
#include "power.h"
#include "BATTERY.h"

C PARAMETERS FROM SIGNATURE
      REAL batTemp,DODInit,batCur,dtTStep
      INTEGER batMode,IPWC

C Declaration of functions used
      REAL BatteryRInt,FinalDOD
      REAL RInt_VRB, FinalDOD_VRB
      REAL RInt_Lion, FinalDOD_Lion

C Local variables
      REAL DODAvg    ! Average DOD during time step (DODInit+DODEnd)/2
      REAL RIntAvg   ! Internal resistance corresponding to DODAvg (Ohm)

C************** Calculation
      IF( POWCOMID(IPWC) .EQ. pba) THEN      ! Lead-acid battery
          DODAvg = FinalDOD(IPWC,batTemp,DODInit,batMode,
     &                     dtTStep/2.0,batCur)
          RIntAvg = BatteryRInt(IPWC,batTemp,DODAvg,batCur,batMode)
      ELSEIF(POWCOMID(IPWC) .EQ. vrb) THEN   ! VRB
          DODAvg = FinalDOD_VRB(IPWC,DODInit,batMode,
     &                     dtTStep/2.0,batCur)
          RIntAvg = RInt_VRB(IPWC,batTemp,DODAvg,batCur,batMode)
      ELSEIF(POWCOMID(IPWC) .EQ. lion) THEN   ! Li-on
         DODAvg = FinalDOD_Lion(IPWC,batTemp,DODInit,batMode,
     &                    dtTStep/2.0,batCur)
         RintAvg = RInt_Lion(IPWC,DODAvg,batMode,batCur)
      ENDIF


      ! Heat is: RI^2
      InternalHeat = RIntAvg*batCur**2
      END

C**************************************************************************
C 10 BatFinalTemp
C**************************************************************************
c
C Created by: Patrice Pinel
C Initial Creation Date: May, 2005
C
C - This subroutine returns the final temperature of a battery
C   depending ont the internal heat generation
C   and the heat generated by its thermal management system
!
C This routine is called from various points of the program
C
C INPUTS:
C -batTempInit: The battery's initial temperature (oC)
C -dtTStep: Duration of the time step (hour)
C -pIntHeat: Heat generated internally (W)
C -pHeaters: Heat generated by the thermal management system(W)
C -zoneTemp: Zone ambient temperature (oC)
C -IPWC: Power-only component index number
C
C OUTPUTS:
C -returns the battery's final temperature (oC)
C
C *********************************************************************
      REAL FUNCTION BatFinalTemp(IPWC,batTempInit,dtTStep,pIntHeat,
     &                           pHeaters,zoneTemp)
      IMPLICIT NONE
#include "building.h"
#include "plant.h"
#include "power.h"

C PARAMETERS FROM SIGNATURE
      REAL batTempInit,dtTStep,pIntHeat,pHeaters,zoneTemp
      INTEGER IPWC

C Battery Parameters (technology and configuration)
      REAL thermalMass  ! Thermal mass of the battery-mass*Cp (J/oC)
      REAL heatLossFact ! Heat loss factor h*A (W/oC)
      REAL expValue     ! Temperature exponential exp(-hA/mCp * dt)
      REAL heatGen      ! Total heat generated (internal res and heaters)
      INTEGER nParal, nSerie    !Number of battery cells connected in paralel and in series

C************** Initialisation

      nParal = int(POWCDAT(IPWC,1))
      nSerie = int(POWCDAT(IPWC,2))
      thermalMass = POWCDAT(IPWC,11)*nParal*nSerie
      heatLossFact = POWCDAT(IPWC,12)*nParal*nSerie

C************** Calculation

C Patrice's code
C      expValue = EXP(thermalMass/heatLossFact*dtTStep)

C my code
      expValue = EXP(-1.*heatLossFact/thermalMass*dtTStep*3600)

      heatGen = pIntHeat + pHeaters
      BatFinalTemp = (zoneTemp + heatGen/heatLossFact)*(1.0-expValue)
     &               + batTempInit*expValue

      END

C**************************************************************************
C 11 BatFinalTempRes
C**************************************************************************
C
C Created by: Patrice Pinel
C Initial Creation Date: March, 2005
C
C - This subroutine returns the final temperature of a battery
C   with a certain load and operating resistive heaters
C
C This routine is called from various points of the program
C
C INPUTS:
C -batTempInit: The battery's initial temperature (oC)
C -DODInit: The battery's initial depth of discharge
C -batMode: 0 for charge, 1 for discharge
C -dtTStep: Duration of the time step (hour)
C -pLoad: The power to the load (W)
C -pHeaters: The power to the heaters (W)
C -zoneTemp: Zone ambient temperature (oC)
C -IPWC: Power-only component index number
C
C OUTPUTS:
C -returns the battery's final temperature (oC)
C
C *********************************************************************
      REAL FUNCTION BatFinalTempRes(IPWC,batTempInit,DODInit,batMode,
     &                              dtTStep,pLoad,pHeaters,zoneTemp)
      IMPLICIT NONE


C PARAMETERS FROM SIGNATURE
      REAL batTempInit,DODInit,dtTStep,pLoad,pHeaters,zoneTemp
      INTEGER batMode,IPWC

C Declaration of functions used
      REAL InternalHeat,CurForPow,BatFinalTemp

C Local variables
      REAL batCurrent   ! Current flowing through the battery
      REAL pIntHeat     ! Heat generated by the current flowing in the internal resistance of the battery
      REAL heatPow      ! Power coming from the battery

C************** Calculation
      IF (batMode .EQ. 0) THEN   !Charge
         ! Battery does not handle load on heaters
         heatPow = pLoad

      ELSE  !Discharge
         ! Battery does handle load on heaters
         heatPow = pLoad+pHeaters

      ENDIF

      batCurrent = CurForPow(IPWC,batTempInit,DODInit,batMode,dtTStep,
     &                         heatPow)

      pIntHeat = InternalHeat(IPWC,batTempInit,DODInit,batMode,dtTStep,
     &           batCurrent)

      BatFinalTempRes = BatFinalTemp(IPWC,batTempInit,dtTStep,pIntHeat,
     &                               pHeaters,zoneTemp)

      END

C**************************************************************************
C 12 AvgVolt
C**************************************************************************
C
C Created by: Patrice Pinel
C Initial Creation Date: March, 2005
C
C - This subroutine returns average voltage observed during a constant current time step
C
C INPUTS:
C -batTemp: The battery's initial temperature (oC)
C -DODInit: The battery's initial depth of discharge
C -batMode: 0 for charge, 1 for discharge
C -dtTStep: Duration of the time step (hour)
C -CDemand: The current demanded/offered to the battery (A)
C -IPWC: Power-only component index number
C
C OUTPUTS:
C -returns the average voltage observed during the time step
C
C *********************************************************************
      REAL Function AvgVolt(IPWC,batTemp,DODInit,batMode,dtTStep,batCur)
      IMPLICIT NONE
#include "building.h"
#include "plant.h"
#include "power.h"
#include "BATTERY.h"

C PARAMETERS FROM SIGNATURE
      REAL batTemp,DODInit,dtTStep,batCur
      INTEGER batMode,IPWC

C Declaration of functions used
      REAL FinalDOD,FreeVoltage,BatteryRInt
      REAL FinalDOD_VRB, OCV_VRB, RInt_VRB
      REAL FinalDOD_Lion, OCV_Lion, RInt_Lion
C Local variables
      REAL DODAvg       ! Average DOD during time step (DODInit+DODEnd)/2
      REAL E0Avg        ! Free voltage corresponding to the average conditions
      REAL RIntAvg      ! Internal resistance corresponding to the average conditions

C************** Calculation

      IF( POWCOMID(IPWC) .EQ. pba) THEN      ! Lead-acid battery
          DODAvg=FinalDOD(IPWC,batTemp,DODInit,batMode,
     &                   dtTStep/2.0,batCur)
          E0Avg = FreeVoltage(IPWC,DODAvg)
          RIntAvg = BatteryRInt(IPWC,batTemp,DODAvg,batCur,batMode)
      ELSEIF(POWCOMID(IPWC) .EQ. vrb) THEN   ! VRB
          DODAvg=FinalDOD_VRB(IPWC,DODInit,batMode,
     &                   dtTStep/2.0,batCur)
          E0Avg = OCV_VRB(IPWC,DODAvg,batCur, batMode)
          RIntAvg = RInt_VRB(IPWC,batTemp,DODAvg,batCur,batMode)
      ELSEIF ( POWCOMID(IPWC) .EQ. lion) THEN  !Lion
          DODAvg = FinalDOD_Lion(IPWC,batTemp,DODInit,batMode,
     &                            dtTStep/2.0,batCur)
          E0Avg = OCV_Lion(IPWC,DODAvg,batTemp)
          RintAvg = RInt_Lion(IPWC,DODAvg,batMode,batCur)
      ENDIF

      IF (batMode .EQ. 0) THEN   !Charge
      ! Observed voltage is higher than free voltage
         AvgVolt = E0Avg + batCur*RIntAvg
      ELSE  !discharge
      ! Observed voltage is lower than free voltage
         AvgVolt = E0Avg - batCur*RIntAvg
      ENDIF

      END

C**************************************************************************
C 13 FinalDOD
C**************************************************************************
C
C Created by: Patrice Pinel
C Initial Creation Date: March, 2005
C
C - This subroutine returns average depth of discharge during a constant current discharge step
C
C INPUTS:
C -batTemp: The battery's initial temperature (oC)
C -DODIni: The battery's initial depth of discharge
C -batMode: 0 for charge, 1 for discharge
C -dtTStep: Duration of the time step (hour)
C -batCur: The power demanded/offered to the battery (W)
C -IPWC: Identity of the plant component associated with the model
C
C OUTPUTS:
C -returns the DOD at the end of the time step
C
C *********************************************************************
      REAL Function FinalDOD(IPWC,batTemp,DODIni,batMode,dtTStep,batCur)
      IMPLICIT NONE

C PARAMETERS FROM SIGNATURE
      REAL batTemp,DODIni,dtTStep,batCur
      INTEGER batMode,IPWC

C Declaration of functions used
      REAL BatteryCap

C************** Calculation
      IF(batMode .EQ. 0) THEN   !Charge
         FinalDOD = DODIni - batCur*dtTStep/BatteryCap(IPWC,batTemp)
      ELSE  !Discharge
         FinalDOD = DODIni + batCur*dtTStep/BatteryCap(IPWC,batTemp)
      ENDIF

      END

C****************************************************************************
C 14 GetNeededHeat
C****************************************************************************
C
C Created by: Patrice Pinel
C Initial Creation Date: May, 2005
C
C - This subroutine returns the heat needed
C   to increase the battery temperature to a desired value
C
C This routine is called from various points of the program
C
C INPUTS:
C -batTIni: The battery's initial temperature (oC)
C -dtTStep: Duration of the time step (hour)
C -batTFin: The desired final temperature (oC)
C -zoneT: Zone ambient temperature (oC)
C -IPWC: Power-only component index number
C
C OUTPUTS:
C -returns the power needed (W)
C
C *********************************************************************
      REAL FUNCTION GetNeededHeat(IPWC,batTIni,dtTStep,batTFin,zoneT)
      IMPLICIT NONE
#include "building.h"
#include "plant.h"
#include "power.h"

C PARAMETERS FROM SIGNATURE
      REAL batTIni,dtTStep,batTFin,zoneT
      INTEGER IPWC

C Battery Parameters (technology and configuration)
      REAL thermalMass  ! Thermal mass of the battery-mass*Cp (J/oC)
      REAL heatLossFact ! Heat loss factor h*A (W/oC)
      REAL expValue     ! Temperature exponential exp(-hA/mCp * dt)
      REAL neededHeat   ! Heat needed (W)
      INTEGER nParal, nSerie    !Number of battery cells connected in paralel and in series

C************** Initialisation

      nParal = int(POWCDAT(IPWC,1))
      nSerie = int(POWCDAT(IPWC,2))
      thermalMass = POWCDAT(IPWC,11)*nParal*nSerie
      heatLossFact = POWCDAT(IPWC,12)*nParal*nSerie

C************** Calculation

C Patrice's original code
C      expValue = EXP(thermalMass/heatLossFact*dtTStep)

C      neededHeat = ((batTFin - batTIni*expValue)/(1.0-expValue)-zoneT)
C     &              *heatLossFact


C correction
      expValue = EXP(-1. * heatlossFact*dtTStep*3600/thermalMass)
      neededHeat = ((batTIni - zoneT)*expValue +
     &              (zoneT - batTfin)) /
     &             (1. - expValue) * (-1.) * heatLossFact


C     Make sure no values lower than 0 are returned.
      IF(neededHeat .LE. 0.0) THEN
         GetNeededHeat = 0.0
      ELSE
         GetNeededHeat = neededHeat
      ENDIF

      END

C**************************************************************************
C 15 HeaterForFinalTemp
C**************************************************************************
C
C Created by: Patrice Pinel
C Initial Creation Date: May, 2005
C
C - This subroutine returns the power required in the heaters
C   to increase the battery temp to a certain value
C - It takes into account the heat generated in the battery's internal resistance
C
C This routine is called from various points of the program
C
C INPUTS:
C -batTIni: The battery's initial temperature (oC)
C -batTFin: The battery's targetted final temperature (oC)
C -DODIni: The battery's initial depth of discharge
C -batMode: 0 for charge, 1 for discharge
C -dtTStep: Duration of the time step (hour)
C -pLoad: The power flowing into the battery (W)
C -pAvail: The power available (W) (= max discharge rate in discharge, = what is available in charge)
C -zoneT: Zone ambient temperature (oC)
C -IPWC: Power-only component index number
C
C OUTPUTS:
C -returns the power required for the heaters (W)
C
C *********************************************************************
      REAL FUNCTION HeaterForFinalTemp(IPWC,batTIni,batTFin,DODIni,
     &                            batMode,dtTStep,pAvail,pLoad,zoneT)
      IMPLICIT NONE

      COMMON/OUTIN/IUOUT,IUIN
      INTEGER iuout          !- write unit number
      INTEGER iuin           !- read unit number

C PARAMETERS FROM SIGNATURE
      REAL batTIni,batTFin,DODIni,dtTStep,pAvail,pLoad,zoneT
      INTEGER batMode,IPWC

C Declaration of functions used
      REAL InternalHeat,GetNeededHeat,CurForPow

C Local variables
      REAL batCur       ! Current flowing through the battery
      REAL reqHeat      ! Total required heat to get the battery to the final temperature
      REAL pIntHeat     ! Heat generated by the current flowing in the internal resistance of the battery
      REAL pHeaters     ! Power to the heaters (W)
      REAL pHDum        ! Dummy value of Power to the heaters used to verify convergence
      REAL batPow       ! Power coming from the battery
      REAL relaxFact    ! Relaxation factor for iteration loop (to reduce oscillations)
      REAL convVal      ! Convergence is assumed when difference between 2 iterations is smaller than this factor
      INTEGER numIter   ! Number of iterrations done
      INTEGER maxIter   ! Max number of iterrations before we consider there is a convergence problem

C************** Initialisation
      ! Assign value for relaxation factor
      relaxFact = 0.1
      ! Assign convergence criteria
      convVal = 0.001
      ! Initialise the number of iterations
      numIter = 0
      maxIter = 100

C************** Calculation
C     Total (heater+internal resistance) heat needed to obtain the desired final temp
      reqHeat = GetNeededHeat(IPWC,batTIni,dtTStep,batTFin,zoneT)

C Initialise heater power to a close value
C     Curent needed to take the load
      batCur=CurForPow(IPWC,batTIni,DODIni,batMode,dtTStep,pLoad)
C     Internal heat generated by that curent
      pIntHeat=InternalHeat(IPWC,batTIni,DODIni,batMode,dtTStep,batCur)
C     The power to the heaters is the difference between what is needed and what is generated internally
C     The power to the heater can not exceed what is available
      pHeaters = MIN(MAX(0.0,pAvail-pLoad),MAX(0.0,reqHeat - pIntHeat))

1510  pHDum = pHeaters  ! set dummy value to value at the end of the previous iterration

      IF (batMode .EQ. 0) THEN   !Charge
         ! Battery does not handle load on heaters
         batPow = pLoad
      ELSE  !Discharge
         ! Battery does handle load on heaters
         batPow = pLoad+pHeaters
      ENDIF

C     Curent flowing through the battery
      batCur = CurForPow(IPWC,batTIni,DODIni,batMode,dtTStep,batPow)

C     Heat generated by that curent flowing through the internal resistance
      pIntHeat=InternalHeat(IPWC,batTIni,DODIni,batMode,dtTStep,batCur)

C     The power to the heaters is the difference between what is needed and what is generated internally
C     Relaxed in order to avoid excessive oscillations
      pHeaters = MIN(MAX(0.0,pAvail-pLoad),
     &           relaxFact*MAX(0.0,(reqHeat-pIntHeat))
     &           + (1-relaxFact)*pHeaters)

C     increment the number of iterrations
      numIter = numIter + 1

      IF((ABS(pHeaters - pHDum).GT.convVal).AND.
     &   (numIter.LT.maxIter)) THEN
      ! Not converged -> repeat loop
         GOTO 1510
      ENDIF

      IF(numIter.GT.maxIter)  THEN
      !Max number of iterations exceeded -> give warning
        WRITE(IUOUT,*) 'Warning Battery Model: Troublesome ',
     &                 'convergence in function HeaterForFinalTemp'
      ENDIF

      HeaterForFinalTemp = pHeaters

      END

C**************************************************************************
C 16 DoCharge
C**************************************************************************
C
C Created by: Patrice Pinel
C Initial Creation Date: March, 2005
C
C - This subroutine returns the power repartition between the load and the heaters
C   during a time step in charge mode and the average voltage of the battery
C - Gives priority to the heaters using only part of the load to recharge when the temp is below operating conditions
C
C INPUTS:
C -batTIni: The battery's initial temperature (oC)
C -zoneT: Zone ambient temperature (oC)
C -DODIni: The battery's initial depth of discharge
C -batMode: 0 for charge, 1 for discharge
C -dtTStep: Duration of the time step (hour)
C -maxC: The maximum curent that can flow thriugh the battery (Amp)
C -maxP: The maximum power that can flow thriugh the battery (W)
C -IPWC: Power-only component index number
C -pLoad: The offer (what can be used) (W)
C
C OUTPUTS:
C -pHeaters: The power to the heaters (W)
C -pBat: The actual power used by the battery (W)
C -batVolt: Battery average voltage corresponding to these loads (V)
C -batTFin: Battery temperature at the end of the time step (oC)
C -batDODFin: Battery DOD at the end of the time step (%)
C -batCur: Battery current (A)
C -pIntHeat: Heat generated by the current flowing in the internal resistance of the battery
C
C NOTE:
C - batMode = 0 since this is in charge
c *********************************************************************
      SUBROUTINE DoCharge(IPWC,batTIni,zoneT,DODIni,dtTStep,
     &                    maxC,maxP,pLoad,pBat,pHeaters,
     &                    batVolt,batTFin,batDODFin,batCur,pIntHeat)
      IMPLICIT NONE
#include "building.h"
#include "plant.h"
#include "power.h"

      COMMON/OUTIN/IUOUT,IUIN
      INTEGER iuout          !- write unit number
      INTEGER iuin           !- read unit number

C PARAMETERS FROM SIGNATURE
      REAL batTIni,zoneT,DODIni,dtTStep,maxC,maxP,pLoad,
     &     pBat,pHeaters,batVolt,batTFin,batDODFin,batCur,pIntHeat
      INTEGER IPWC

C Declaration of functions used
      REAL AvgVolt,GetNeededHeat,InternalHeat,CurForPow,BatFinalTemp,
     &     FinalDOD

C Battery Parameters (technology and configuration)
      REAL heatersRes   ! Resistance of the heaters
      REAL batTOper     ! Battery operating temperature (oC)
      INTEGER nParal, nSerie    !Number of battery cells connected in paralel and in series
      INTEGER ThermalManagement    !Parameter indicating whether active thermal
                                   !management of the battery is required
                                   !ThermalManagement = 0 :no active thermal management
                                   !ThermalManagement = 1 :active thermal management

C Local variables
      REAL maxPheaters     ! Maximum power the heaters can provide (W)
      REAL availForCharge  ! Available heat for charging (Total available - Heaters) (W)
      REAL neededHeatTOper ! Heat needed to get to the operating temperature (W)
      REAL needHTOperLoad  ! Power to the heaters needed to reach the emergency temperature (W)
      REAL pHDum           ! Dummy heater power to verify convergence
      REAL relaxFact       ! Relaxation factor for iteration loop (to reduce oscillations)
      REAL convVal         ! Convergence is assumed when difference between 2 iterations is smaller than this factor
      INTEGER numIter      ! Number of iterrations done
      INTEGER maxIter      ! Max number of iterrations before we consider there is a convergence problem

C************** Initialisation
      ! Assign value for relaxation factor
      relaxFact = 0.1
      ! Assign convergence criteria
      convVal = 0.001

      ! Initialise the number of iterations
      numIter = 0
      maxIter = 100

      nParal = int(POWCDAT(IPWC,1))
      nSerie = int(POWCDAT(IPWC,2))
      heatersRes = POWCDAT(IPWC,13)/(nParal*nSerie)
      batTOper = POWCDAT(IPWC,4)
C      The position 50 is temporarily set here, needs to be changed once the
C      input parameters are finalized.
      ThermalManagement = POWCDAT(IPWC,50)

C************** Calculation

      IF (ThermalManagement .EQ. 1) THEN     !Active thermal management
        ! Initialise pBat to the highest rate possible -> maximum voltage
        pBat = maxP
        batCur = maxC

C       Heat required from the heaters to get to the operating temperature when going with no charging
        neededHeatTOper=GetNeededHeat(IPWC,batTIni,dtTStep,batTOper,
     &                                 zoneT)

C       Initiate the heat needed from the heater to the total heat needed, if > 0
        needHTOperLoad = MAX(0.0,neededHeatTOper)

1610    pHDum = pHeaters

C       Find voltage corresponding to charge rate
        batVolt = AvgVolt(IPWC,batTIni,DODIni,0,dtTStep,batCur)

C       Maximum power the heaters can provide = V^2/Rheaters
        maxPheaters = (batVolt**2)/heatersRes

C       Make sure the energy directed to the heaters does not exceed the maximums available
        maxPheaters = MIN(pLoad,maxPheaters)

C       Power used by the heaters
        pHeaters = MIN(maxPheaters, needHTOperLoad)

C       What is available for charging is the difference between what is available and what is used by the heaters
        availForCharge = pLoad - pHeaters

C       The power used by the battery is either what is available or the maximum
        pBat = MIN(availForCharge, maxP)

        batCur = CurForPow(IPWC,batTIni,DODIni,0,dtTStep,pBat)

C       Internal heat
        pIntHeat = InternalHeat(IPWC,batTIni,DODIni,0,dtTStep,batCur)

C       Recalculate the heater needs taking into acount the power flowin through the internal resistance of the battery
C       Used the InternalHeat function instead of the HeaterForFinalTemp one since we already know the current and
C       there is no need to go once more through the iterration loop
        needHTOperLoad=(1-relaxFact)*needHTOperLoad +
     &            relaxFact*(MAX(0.0,(neededHeatTOper - pIntHeat)))

C       increment the number of iterrations
        numIter = numIter + 1

        IF((ABS(pHeaters - pHDum).GT.convVal).AND.
     &     (numIter.LT.maxIter)) THEN
          ! Not converged -> repeat loop
          GOTO 1610
        ENDIF

        IF(numIter.GT.maxIter)  THEN
        !Max number of iterations exceeded -> give warning
          WRITE(IUOUT,*) 'Warning Battery Model: Troublesome ',
     &                   'convergence in function DoCharge'
        ENDIF

      ELSEIF (ThermalManagement .EQ. 0) THEN     !No active thermal management
        pHeaters = 0.

C       What is available for charging is the difference between what is available and what is used by the heaters.
C       In this case of no thermal management, all power is available for the battery.
        availForCharge = pLoad - pHeaters

C       The power used by the battery is either what is available or the maximum
        pBat = MIN(availForCharge, maxP)

        batCur = CurForPow(IPWC,batTIni,DODIni,0,dtTStep,pBat)

C       Find voltage corresponding to charge rate
        batVolt = AvgVolt(IPWC,batTIni,DODIni,0,dtTStep,batCur)

C       Internal heat
        pIntHeat = InternalHeat(IPWC,batTIni,DODIni,0,dtTStep,batCur)
      ELSE     !Incorrect value for ThermalManagement parameter
        WRITE(IUOUT,*) 'Error Battery Model: Incorrect value for',
     &           ' ThermalManagement parameter in fuction DoCharge'
        pBat = 0.
        batCur = 0.
        pIntHeat = 0.
      ENDIF     !Thermal management

C Get battery's final state
      batTFin=BatFinalTemp(IPWC,batTIni,dtTStep,pIntHeat,pHeaters,zoneT)

C --------------------------------------------------------------------
C     With no active thermal management of the battery, its temperature will be floating.
C     To allow the investigation of other aspects of the battery model without possible
C     interference of large temperature differences, the temperature can be fixed by
C     activating the follwing line of code:
C
C      batTFin = 20.
C
C     It should be noted that this is a 'hard' reset of the temperature to a specified value
C     at the end of each time step. This may introduce small errors in the battery's energy balance
C     because the energy to bring the battery to this fixed temperature is not accounted for.
C     The user should consider whether or not this could cause a problem in the specific
C     application the model is used in.
C --------------------------------------------------------------------

      batDODFin = FinalDOD(IPWC,batTIni,DODIni,0,dtTStep,batCur)

      END

C**************************************************************************
C 17 DoDischarge
C**************************************************************************
C
C Created by: Patrice Pinel
C Initial Creation Date: March, 2005
C
C - This subroutine returns the power repartition between the load and the heaters
C   during a time step in discharge mode
C - Gives priority to the heaters (meeting only part of the load) when the temp is below emergency conditions
C - Meets the load and maintain the temp as close as possible to operating conditions for non emergency conditions
C
C INPUTS:
C -batTIni: The battery's initial temperature (oC)
C -zoneT: Zone ambient temperature (oC)
C -DODIni: The battery's initial depth of discharge
C -batMode: 0 for charge, 1 for discharge
C -dtTStep: Duration of the time step (hour)
C -maxC: The maximum curent available from the battery (Amp)
C -maxP: The maximum power available from the battery (W)
C -IPWC: Power-only component index number
C -pLoad: The requested (demand) load (W)
C
C OUTPUTS:
C -pHeaters: The power to the heaters (W)
C -pBat: The actual power used by the battery (W)
C -batVolt: Battery average voltage corresponding to these loads (V)
C -batTFin: Battery temperature at the end of the time step (oC)
C -batDODFin: Battery DOD at the end of the time step (%)
C -batCur: Battery current (A)
C -pIntHeat: Heat generated by the current flowing in the internal resistance of the battery
C
C NOTE:
C - batMode = 1 since this is in discharge
C *********************************************************************
      SUBROUTINE DoDischarge(IPWC,batTIni,zoneT,DODIni,dtTStep,
     &                       maxC,maxP,pLoad,pBat,pHeaters,
     &                       batVolt,batTFin,batDODFin,batCur,pIntHeat)
      IMPLICIT NONE
#include "building.h"
#include "plant.h"
#include "power.h"

      COMMON/OUTIN/IUOUT,IUIN
      INTEGER iuout          !- write unit number
      INTEGER iuin           !- read unit number

C PARAMETERS FROM SIGNATURE
      REAL batTIni,zoneT,DODIni,dtTStep,maxC,maxP,pLoad,pBat,pHeaters,
     &     batVolt,batTFin,batDODFin,batCur,pIntHeat
      INTEGER IPWC

C Declaration of functions used
      REAL AvgVolt,GetNeededHeat,HeaterForFinalTemp,InternalHeat,
     &CurForPow,BatFinalTemp,FinalDOD

C Battery Parameters (technology and configuration)
      REAL heatersRes   ! Resistance of the heaters
      REAL batTEmer     ! Emergency temperature below which the thermal management system has priority over the load (oC)
      REAL batTOper     ! Battery operating temperature (oC)
      INTEGER nParal, nSerie    !Number of battery cells connected in paralel and in series
      INTEGER ThermalManagement    !Parameter indicating whether active thermal
                                   !management of the battery is required
                                   !ThermalManagement = 0 :no active thermal management
                                   !ThermalManagement = 1 :active thermal management

C Local variables
      REAL maxPheaters     ! Maximum power the heaters can provide (W)
      REAL availForHeaters ! Available heat for heaters (Total available - Load) (W)
      REAL batCurAvailHeat ! Current related to power available for heaters (A)
      REAL batCurMaxPHeaters ! Current through the battery when heaters get max power (A)
      REAL batPowAvailHeat ! Battery power when load is met and heaters get available heat (W)
      REAL neededHeatTEmer ! Heat needed to get to the emergency temperature (W)
      REAL needHTEmerAvail ! Power to the heaters needed to reach the emergency temperature when meeting the load (W)
      REAL needHTEmerLoad  ! Power to the heaters needed to reach the emergency temperature when meeting the load (W)
      REAL needHTEmerMaxPHeaters ! Power to the heaters needed to reach the emergency temperature when using the available
                                 ! heating capacity - which is either constrained by battery power or heater capacity (W)
      REAL neededHeatTOper ! Heat needed to get to the operating temperature (W)
      REAL needHTOperAvail ! Power to the heaters needed to reach the operating temperature when meeting the load (W)
      REAL needHTOperLoad  ! Power to the heaters needed to reach the operating temperature when meeting the load (W)

C************** Initialisation

      nParal = int(POWCDAT(IPWC,1))
      nSerie = int(POWCDAT(IPWC,2))
      heatersRes = POWCDAT(IPWC,13)/(nParal*nSerie)
      batTEmer = POWCDAT(IPWC,14)
      batTOper = POWCDAT(IPWC,4)
C      The position 50 is temporarily set here, needs to be changed once the
C      input parameters are finalized.
      ThermalManagement = POWCDAT(IPWC,50)

C************** Calculation

      IF (ThermalManagement .EQ. 1) THEN     !Active thermal management
C       If necessary power from the battery will be used to heat the battery.
C       The maximum power available to heat the battery is limited by
C       - the maximum power that can be discharged from the battery (maxP)
C       - the maximum power the heaters can provide (V^2/Rheaters)
        maxPheaters=((AvgVolt(IPWC,batTIni,DODIni,1,dtTStep,maxC))**2)
     &                  /heatersRes
C       Make sure the energy directed to the heaters does not exceed the maximum available
        maxPheaters = MIN(maxP,maxPheaters)

C       The corresponding current is
        batCurMaxPHeaters = CurForPow(IPWC,batTIni,DODIni,1,dtTStep,
     &                       maxPheaters)

C       If the load would get preference over the heaters, the power available for the
C       heaters would be
        availForHeaters = MIN( MAX(0.0,(maxP-pLoad)), maxPheaters)

        batPowAvailHeat = MIN (maxP, (pLoad+maxPHeaters))

C       The current flowing through the battery would be
        batCurAvailHeat = CurForPow(IPWC,batTIni,DODIni,1,dtTStep,
     &                       batPowAvailHeat)

C ---------------------------------------------------------------------
C       Calculate heat demand to bring the battery to the emergency temperature
        ! Heat needed to get the temperature from batTempInit to emergency temperature
        neededHeatTEmer=GetNeededHeat(IPWC,batTIni,dtTStep,batTEmer,
     &                                 zoneT)

C       Calculate the heat needed to raise the battery temperature to the emergency temperature
C       using the maximum power available (maxPHeaters)
        needHTEmerMaxPHeaters = neededHeatTEmer -
     &   InternalHeat(IPWC,batTIni,DODIni,1,dtTStep,batCurMaxPHeaters)
C       Make sure no values lower than 0 are used in the calculations
        needHTEmerMaxPHeaters=MAX(0.0,needHTEmerMaxPHeaters)

        needHTEmerLoad=HeaterForFinalTemp(IPWC,batTIni,batTEmer,DODIni,
     &                 1,dtTStep,maxP,pLoad,zoneT)

        needHTEmerAvail = neededHeatTEmer -
     &    InternalHeat(IPWC,batTIni,DODIni,1,dtTStep,batCurAvailHeat)
C       Make sure no values lower than 0 are used in the calculations
        needHTEmerAvail = MAX(0.0,needHTEmerAvail)

C ---------------------------------------------------------------------
C       Calculate heat demand to bring the battery to the operating temperature
        ! Heat needed to get the temperature from batTempInit to operating temperature
        neededHeatTOper=GetNeededHeat(IPWC,batTIni,dtTStep,batTOper,
     &                                 zoneT)
        needHTOperLoad=HeaterForFinalTemp(IPWC,batTIni,batTOper,DODIni,
     &                 1,dtTStep,maxP,pLoad,zoneT)

        needHTOperAvail = neededHeatTOper -
     &    InternalHeat(IPWC,batTIni,DODIni,1,dtTStep,batCurAvailHeat)
C       Make sure no values lower than 0 are used in the calculations
        needHTOperAvail = MAX(0.0,needHTOperAvail)


C 3 possible situations:
C 1- T below emergency
C 2- T between emergency and operation
C 3- T above operation

C       First situation
C        IF (needHTEmerMaxLoad .GT. availForHeaters) THEN   ! Incorrect !!!
        IF (needHTEmerMaxPHeaters .GT. maxPheaters) THEN
        ! Emergency situation, there is not enough power available for the heaters
        ! to increase the temperature to the emergency level -> heaters have the priority

C         The maximum power available for the heaters will be used
          pHeaters = maxPheaters
C         In case the heater capacity was limiting, there may be power left to meet (part) of the load
          pBat = MIN(pLoad+pHeaters,maxP)

C       Second situation
        ELSEIF(needHTOperAvail .GT. availForHeaters) THEN
C       There is enough energy for the heaters to exceed the emergency temperature
C       But not enough to be above the operating temperature. There are 2 situations possible

C       If there is not enough power to meet both the load and bring the battery temperature
C       above the emergency temperature, the heaters will get what they need for reaching the emergency temperature
C       Otherwise, the load gets priority and whatever is left is for the heaters.
          IF (needHTEmerLoad .GT. availForHeaters) THEN
            pHeaters = needHTEmerAvail
            pBat = MIN(pLoad+pHeaters,maxP)
          ELSE        ! Heaters get what is left for them
            pHeaters = availForHeaters
            pBat = MIN(pLoad+pHeaters,maxP)
          ENDIF

C       Third situation
        ELSE
C       There is sufficient power to meet both the load and to heat the battery.
C       If all available heat is thrown to the heaters, the temperature will exceed the operating temperature

C         Limit the power to the heaters to what is needed to maintain the temp to the operating point
          pHeaters = needHTOperLoad

C         Make sure that pHeaters does not exceed the heaters' capacity
          pHeaters = MIN(maxPheaters,pHeaters)

          pBat = MIN((pLoad+pHeaters),maxP)
        ENDIF

      ELSEIF (ThermalManagement .EQ. 0) THEN     !No active thermal management
        pHeaters = 0.
        pBat = MIN((pLoad+pHeaters),maxP)
      ELSE     !Incorrect value for ThermalManagement parameter
        WRITE(IUOUT,*) 'Error Battery Model: Incorrect value for',
     &         ' ThermalManagement parameter in fuction DoDischarge'
        pHeaters = 0.
      ENDIF     !Thermal management

C     Evaluate the load on the battery
      batCur = CurForPow(IPWC,batTIni,DODIni,1,dtTStep,pBat)

      batVolt = AvgVolt(IPWC,batTIni,DODIni,1,dtTStep,batCur)

      pIntHeat = InternalHeat(IPWC,batTIni,DODIni,1,dtTStep,batCur)

C     Get battery's final state
      batTFin=BatFinalTemp(IPWC,batTIni,dtTStep,pIntHeat,pHeaters,zoneT)

C --------------------------------------------------------------------
C     With no active thermal management of the battery, its temperature will be floating.
C     To allow the investigation of other aspects of the battery model without possible
C     interference of large temperature differences, the temperature can be fixed by
C     activating the follwing line of code:
C
C      batTFin = 20.
C
C     It should be noted that this is a 'hard' reset of the temperature to a specified value
C     at the end of each time step. This may introduce small errors in the battery's energy balance
C     because the energy to bring the battery to this fixed temperature is not accounted for.
C     The user should consider whether or not this could cause a problem in the specific
C     application the model is used in.
C --------------------------------------------------------------------

      batDODFin = FinalDOD(IPWC,batTIni,DODIni,1,dtTStep,batCur)

      END


C**************************************************************************
C 26 ChargeCurConstVolt
C**************************************************************************
C
C Created Date: January, 2008
c
C - This subroutine returns the current the battery can charge with without
C   crossing the voltage limit of the second step of a mandatory charge cyle
C - The temperature at the begining of the time step is used to evaluate properties ->
C   the problem is decoupled as far as temperatures are concerned
C
C This routine is called from POWOC_CETC_Battery
C
C INPUTS:
C -batTemp: The battery's temperature (oC)
c -DODInit: The battery's initial depth of discharge
C -batMode: 0 for charge, 1 for discharge
C -dtTStep: Duration of the time step (hour)
C -IPWC: power-only component index number
!
! OUTPUTS:
! -returns the charge current related to the constant voltage charge step (Amp)
!
! *********************************************************************
      REAL FUNCTION ChargeCurConstVolt(IPWC,batTemp,DODInit,batMode,
     &               dtTStep)
      IMPLICIT NONE
C     THIS FUNCTION MAY BE REPLACED BY THE MaxCurrent FUNCTION LATER

#include "building.h"
#include "plant.h"
#include "power.h"

      COMMON/OUTIN/IUOUT,IUIN
      INTEGER iuout          !- write unit number
      INTEGER iuin           !- read unit number

C PARAMETERS FROM SIGNATURE
      REAL batTemp,DODInit,dtTStep
      INTEGER batMode,IPWC

C Battery Parameters (technology and configuration)
C      REAL batMaxVolt      ! Maximum manufacturer voltage of a battery cell (0% DOD) (V)
      REAL batMaxVoltCh    ! Maximum manufacturer charging voltage of a battery cell (V)
C      REAL batMinVolt      ! Minimum manufacturer voltage of a battery cell (100% DOD) (V)
      REAL batMaxChCur     ! Maximum allowable current during charge mode (Amp)
C      REAL upperBoundDOD   ! DOD limit, upper bound
C      REAL lowerBoundDOD   ! DOD limit, lower bound

C Declaration of functions used
      REAL BatteryRInt,FreeVoltage,FinalDOD

C Local variables
      REAL relaxFact    ! Relaxation factor for iteration loop (to reduce oscillations)
      REAL convVal      ! Convergence is assumed when difference between 2 iterations is smaller than this factor
      REAL batCurrent   ! Battery current (Amp)
      REAL dumCurrent   ! Dummy current used to see the current evolution from time step to time step (Amp)
C      REAL lastGoodCur  ! Last curent that did not exceed the DOD limits (0-100%)
      REAL DODEnd       ! Depth of discharge at the end of the time step
      REAL FreeVEnd     ! Free Voltage at the end of the time step (Volt)
C      REAL FreeVAvg     ! Free Voltage at the center of the time step (Volt)
      REAL RIntEnd      ! Internal resistance at the end of the time step (Ohm)
C      REAL RIntAvg      ! Internal resistance at the center of the time step (Ohm)
      INTEGER numIter   ! Number of iterrations done
      INTEGER maxIter   ! Max number of iterrations before we consider there is a convergence problem

C************** Initialisation
C     Voltages are multiplied by the number of units connected in series
C      batMaxVolt = POWCDAT(IPWC,5)*POWCDAT(IPWC,2)
C      batMinVolt = POWCDAT(IPWC,6)*POWCDAT(IPWC,2)
      batMaxVoltCh = POWCDAT(IPWC,8)*POWCDAT(IPWC,2)
C     Currents are multiplied by the number of units connected in parallel
      batMaxChCur = POWCDAT(IPWC,7)*POWCDAT(IPWC,1)

      ! Assign value for relaxation factor
      relaxFact = 0.1
      ! Assign convergence criteria
      convVal = 0.001
      ! initialise the number of iterations
      numIter = 0
      maxIter = 100

C     Initialise curents to zero
      batCurrent = 0.
C      lastGoodCur = 0.

C************** Calculation
      ! initialise the dummy current to the value of the current at the last iteration
2610   dumCurrent = batCurrent


C      The position 48, 49 is temporarily set here, needs to be changed once the
C      input parameters are finalized.
C      upperBoundDOD = POWCDAT(IPWC,48)
C      lowerBoundDOD = POWCDAT(IPWC,49)
      DODEnd = FinalDOD(IPWC,batTemp,DODInit,batMode,dtTStep,
     &             batCurrent)
      FreeVEnd = FreeVoltage(IPWC,DODEnd)
C      FreeVAvg = FreeVoltage(IPWC,(DODInit+DODEnd)/2.0)
      RIntEnd = BatteryRInt(IPWC,batTemp,DODEnd,batCurrent,batMode)
C      RIntAvg = BatteryRInt(IPWC,batTemp,(DODInit+DODEnd)/2.0,
C     &             batCurrent,batMode)

C      IF (DODEnd .LT. lowerBoundDOD) THEN
           ! DOD exceeds limit -> current too high

           ! Bissectrice method
           ! Average of the actual current (that exceeds DOD limit) and last current that did not exceed
C           batCurrent = (batCurrent+lastGoodCur)/2.0

C         ELSE
            ! This current does not exceed limits

            ! Reset the good current to the actual current
C            lastGoodCur = batCurrent

C           Adjust current so that (V-E0) = Rint*I results in V=Vmax_allowable
C           at the end of the time step
            batCurrent = relaxFact*(batMaxVoltCh - FreeVEnd)/RIntEnd
     &                   + (1-relaxFact) * batCurrent

C           Make sure the current does not exceed the maximum allowable charging current
            batCurrent = MIN(batCurrent,batMaxChCur)
C         ENDIF  ! DOD

C     Increment the number of iterations
      numIter = numIter + 1

      IF((ABS(batCurrent - dumCurrent).GE.convVal).AND.
     &   (numIter.LT.maxIter)) THEN
      ! Not converged and iteration limit not exceeded -> repeat loop
         GOTO 2610
      ENDIF

      IF(numIter.GT.maxIter)  THEN
      !Max number of iterations exceeded -> give warning
        WRITE(IUOUT,*) 'Warning Battery Model: Troublesome ',
     &                 'convergence in function ChargeCurConstVolt '
      ENDIF

C     Assign function result
      ChargeCurConstVolt = batCurrent

      END

C**************************************************************************
C 29 BatteryLife
C**************************************************************************
C
C Created: December, 2007
C
C - This subroutine calculates the use of battery life during a time step, and
C   checks whether the battery should perform a mandatory charge cycle to
C   preserve battery life (if active battery life is selected by the user).
C   Parts of the code related to the battery life calculation and control are
C   placed in the main program ('POWOC_CETC_BATTERY'), and in subroutines 1
C   ('InitSimulation') and 2 ('InitTimeStep')
C - The aim of this feature is first to inform the user of the battery model of
C   the number of battery lives used during the simulation. With the active
C   battery life controller activated, the program interferes with the normal
C   charging pattern to prevent abuse of the battery.
C - The life use calculation does not take any temperature effects into account
C - There is no feedback of battery life used onto the performance of the battery
C
C INPUTS:
C - batMode: 0 for charge, 1 for discharge, 2 for idle
C - dtTStep: Duration of the time step (hour)
C - batCurFin: The curent available from the battery (Amp)
C - batTemp: The battery's temperature (oC)
C - DODEnd: The DOD of the battery at the end of the time step (-)
C - IPWC: Power-only component index number
C
C OUTPUTS:
C Through BATTERY.h variables
C *********************************************************************
      SUBROUTINE BatteryLife(IPWC,dtTStep,batMode,batCurFin,
     &         batTemp,DODEnd)
      IMPLICIT NONE
#include "building.h"
#include "plant.h"
#include "power.h"
#include "BATTERY.h"

      COMMON/OUTIN/IUOUT,IUIN
      INTEGER iuout          !- write unit number
      INTEGER iuin           !- read unit number

C PARAMETERS FROM SIGNATURE
      REAL dtTStep,batCurFin,batTemp,DODEnd
      INTEGER IPWC,batMode

C Declaration of functions used
      REAL FreeVoltage,BatteryRInt

C Battery Parameters (technology and configuration)
      REAL cellCapRef        ! Reference capacity of one battery cell (Ah)
      INTEGER nParal, nSerie ! Number of battery cells connected in paralel and in series
      REAL batteryCapacity   ! Total battery capacity (Ah)
      REAL batMaxChVolt      ! Maximum manufacturer charging voltage of a battery cell (V)

C Local variables
      REAL floatLife            ! Battery life under floating conditions (years)
      REAL floatLifeTimeStep    ! Float life impact for this time step (years)
      REAL cycleLife            ! Battery life under cyclic operation (Ah)
      REAL cycleLifeTimeStep    ! Cycle life impact for this time step (Ah)
      REAL cycleLifeNrCycles    ! Number of cycles defining cycle life (-)
      REAL cycleLifeDODCycles   ! DOD of cycles defining cycle life (-)
      REAL abuseLife            ! Battery life when battery is abused (years)
                                ! A battery is treated badly if it is not fully
                                ! charged regularly. The interval between full
                                ! charge cycles is a user input
      REAL abuseLifeTimeStep    ! Abuse impact for this time step (years)
      REAL FreeVEnd             ! Free Voltage at the end of the time step (Volt)
      REAL VEnd                 ! Voltage at the end of the time step (Volt)
      REAL RIntEnd              ! Internal resistance at the end of the time step (Ohm)

C See BATTERY.h for definition of other variables related to battery life
C
C************** Initialisation

      nParal = int(POWCDAT(IPWC,1))
      nSerie = int(POWCDAT(IPWC,2))
      cellCapRef = POWCDAT(IPWC,3)

C     The positions 51,52,53, and 54 are temporarily set here,
C     this needs to be changed once the input parameters are finalized.
C     Position 55, 56, and 57 are reserved for a switch indicating whether active
C     battery life control is requested, the interval between manadatory full
C     recharges, and the maximum duration of the mandatory charge phase.
C     Postion 58 could in a later addition be used to indicate a grid-connected
C     or a stand-alone system. A reordering of all parameters may also be done
C     to group the input paramaters together in a more logical way.
      floatLife            = POWCDAT(IPWC,51)
      cycleLifeNrCycles    = POWCDAT(IPWC,52)
      cycleLifeDODCycles   = POWCDAT(IPWC,53)
      abuseLife            = POWCDAT(IPWC,54)
      activeBatLifeControl = INT(POWCDAT(IPWC,55))
      batMaxChVolt = POWCDAT(IPWC,8) * nSerie


C************** Calculation

C     Evaluate the battery life use due to the several life use factors
      IF (floatLife .LE. 0.0) THEN
        WRITE(IUOUT,*) 'Warning Battery Model: Negative or ',
     &                 'Nul float life in subroutine BatteryLife'
        floatLifeTimeStep = 1.0E-06
      ELSE
        floatLifeTimeStep = dtTStep / (floatLife * 8760)
      ENDIF

C     For now battery capacity is calculated based upon the reference capacity
C     to decouple battery life calculations from the actual battery temperature.
      batteryCapacity = cellCapRef * nParal
      cycleLife = cycleLifeNrCycles * cycleLifeDODcycles
     &              * batteryCapacity
      IF (cycleLife .LE. 0.0) THEN
        WRITE(IUOUT,*) 'Warning Battery Model: Negative or ',
     &                 'Nul cycle life in subroutine BatteryLife'
        cycleLifeTimeStep = 1.0E-06
      ELSE
C       A cycle consist of charging as well as discharging.
C       To calculate the cycle life use during a time step,
C       the battery current (charge or discharge) is divided by 2
C       so charge and discharge parts of the cycle both add up to
C       the total cycle life (instead of having seperate counters
C       for charging and discharging).
        cycleLifeTimeStep = (batCurFin/2) * dtTStep / cycleLife
      ENDIF

      IF (abuseLife .LE. 0.0) THEN
        WRITE(IUOUT,*) 'Warning Battery Model: Negative or ',
     &        'Nul bad treatment life in subroutine BatteryLife'
        abuseLifeTimeStep = 1.0E-06
      ELSE
        IF (abuseFlag .NE. 1) THEN
          abuseLifeTimeStep = 0.
        ELSE
          abuseLifeTimeStep = dtTStep / (abuseLife * 8760)
        ENDIF
      ENDIF

C     Determine which life use factor has the greatest impact
C     One of the three will have the greatest impact.
C     At equal impact float life is prefered over cycle life,
C     which gets preference over bad treatment.

C  1) Float life is the dtermining factor
      IF ((floatLifeTimeStep .GE. cycleLifeTimeStep) .AND.
     &     (floatLifeTimeStep .GE. abuseLifeTimeStep)) THEN
        lifeUseFactor = 1
        batLifeUsed = floatLifeTimeStep
C  2) Cycle life is the determining factor
      ELSEIF ((cycleLifeTimeStep .GT. floatLifeTimeStep) .AND.
     &     (cycleLifeTimeStep .GE. abuseLifeTimeStep)) THEN
        lifeUseFactor = 2
        batLifeUsed = cycleLifeTimeStep
C  3) Bad treatment is the determining factor
      ELSEIF ((abuseLifeTimeStep .GT. floatLifeTimeStep) .AND.
     &     (abuseLifeTimeStep .GT. cycleLifeTimeStep)) THEN
        lifeUseFactor = 3
        batLifeUsed = abuseLifeTimeStep
      ENDIF

C     Active battery life preservation by performing mandatory charge cycles
      IF (activeBatLifeControl .EQ. 1) THEN      ! active control
C       Determine whether the battery should go to the next phase of a mandatory charge cycle
        IF (mandChargePhase .EQ. 0) THEN         ! Normal operation between mandatory charge phase
                                                 ! No action is required
        ELSEIF (mandChargePhase .EQ. 1) THEN     ! Constant current step of mandatory charge cycle
                                                 ! Completed when voltage >= batMaxChVolt or DOD <= 0.15
          FreeVEnd = FreeVoltage(IPWC,DODEnd)
          RIntEnd = BatteryRInt(IPWC,batTemp,DODEnd,batCurFin,batMode)
          VEnd = FreeVEnd + batCurFin * RintEnd
          IF ((VEnd .GE. batMaxChVolt) .OR. (DODEnd .LE. 0.15)) THEN     ! Constant current charge step completed
            mandChargePhaseIncrease = 1
          ELSE                ! Keep or reset value to 0 to prevent a value '1' from a previous iteration
                              ! in the same time step to be used incorrectly.
            mandChargePhaseIncrease = 0
          ENDIF

        ELSEIF (mandChargePhase .EQ. 2) THEN     ! Constant voltage step of mandatory charge cycle
          IF (DODEnd .LE. 0.) THEN               ! Constant voltage charge phase completed
            mandChargePhaseIncrease = 1
          ELSE                ! Keep or reset value to 0 to prevent a value '1' from a previous iteration
                              ! in the same time step to be used incorrectly.
            mandChargePhaseIncrease = 0
          ENDIF

        ELSEIF (mandChargePhase .EQ. 3) THEN     ! 3rd phase of mandatory charge cycle
                                                 ! In this phase chemical processes at the electrodes of the
                                                 ! battery are reversed by applying an even higher charge voltage.
                                                 ! However, the current battery model does not have self-discharge
                                                 ! built-in, nor other dissipation processes related to this electrode
                                                 ! regeneration. Therefore there is no easy way to incoporate the
                                                 ! power dissipation of this third charge. It is done now by
                                                 ! charging the battery to a DOD of -0.05 and resetting this
                                                 ! to 0.00 at the end of this phase.
          IF (DODEnd .LE. -0.05) THEN            ! Electrode regeneration phase completed
            mandChargePhaseIncrease = 1
          ELSE                 ! Keep or reset value to 0 to prevent a value '1' from a previous iteration
                               ! in the same time step to be used incorrectly.
            mandChargePhaseIncrease = 0
          ENDIF
        ELSE
C       This should not be possible !!
        ENDIF
      ENDIF     ! IF (activeBatLifeControl .EQ. 1)

      END


C**************************************************************************
C 41 OCV_VRB
C**************************************************************************
C
C Initial Creation Date: December, 2007
C
C - This subroutine returns the open circuit voltage (OCV) of VRB
C
C This routine is called from various points of the program
C
C INPUTS:
C -batDOD: The battery's depth of discharge
C -IPWC: Power-only component index
C -current: battery current
C -batMode: 0 for charge, 1 for discharge
C
C OUTPUTS:
C -returns the VRB's open circuit voltage
C
C *********************************************************************
      REAL FUNCTION OCV_VRB(IPWC,batDOD, current, batMode)
      IMPLICIT NONE
#include "building.h"
#include "plant.h"
#include "power.h"
#include "BATTERY.h"

      COMMON/OUTIN/IUOUT,IUIN
      INTEGER iuout          !- write unit number
      INTEGER iuin           !- read unit number

C Common holding NSINC (no of building time step increments) for debugging only
       COMMON/SIMTIM/IHRP,IHRF,IDYP,IDYF,IDWP,IDWF,NSINC,ITS
       INTEGER IHRP            !-hour of present time-step
       INTEGER IHRF            !-hour of future time-step
       INTEGER IDYP            !-year day number of present day
       INTEGER IDYF            !-year day number of future day
       INTEGER IDWP            !-day of the week of present day
       INTEGER IDWF            !-day of the week of future day
       INTEGER NSINC           !-number of building-side time increments
                               !-since start of simulation
       INTEGER ITS             !-current building time-step within
                               !-current hour

C PARAMETERS FROM SIGNATURE
      REAL batDOD
      REAL current
      INTEGER IPWC
      INTEGER batMode


C Battery Parameters (technology and configuration)
      INTEGER nSerie            ! number of battery cells connected in series
      INTEGER nParal            ! number of battery cells connected in parallel
      REAL batMaxVolt           ! Maximum manufacturer voltage of a battery cell (V)
      REAL batMinVolt           ! Minimum manufacturer voltage of a battery cell (V)
      REAL con_V3_discharged    ! concentration of V3+ at fully-discharged state (DOD = 100%)
      REAL con_H_discharged     ! concentration of H+ at fully-discharged state (DOD =100%)
      REAL electrolyteFlowRate ! electrolyte flow rate (L/s)
      REAL electrolyteTemp      ! electrolyte temperature (oC)

C Local variables
      REAL con_V2_in, con_V2_out, con_V2_avg  !V2+ concentration at inlet, outlet and average (M)
      REAL con_V3_in, con_V3_out, con_V3_avg  !V3+ concentration at inlet, outlet and average (M)
      REAL con_V4_in, con_V4_out, con_V4_avg  !V4+ concentration at inlet, outlet and average (M)
      REAL con_V5_in, con_V5_out, con_V5_avg  !V5+ concentration at inlet, outlet and average (M)
      REAL con_H_in, con_H_out, con_H_avg     ! H+ concentration at inlet, outlet and average (M)
      REAL electronFlowRate                   !electron molar flow rare (mole/s)
      REAL RT_over_nF                         ! RT/nF
      REAL unitVoltage                        ! voltage of a unit cell (V)
      REAL tempVariable                       ! temporary variable


C************** Initialisation
      nParal = int(POWCDAT(IPWC,1))
      nSerie = int(POWCDAT(IPWC,2))
      con_V3_discharged = POWCDAT(IPWC,3)
      con_H_discharged = POWCDAT(IPWC,4)
      electrolyteTemp = POWCDAT(IPWC,9)
      electrolyteFlowRate = POWCDAT(IPWC,12)


C     Voltages are multiplied by the number of units connected in series
      batMaxVolt = POWCDAT(IPWC,5) * nSerie
      batMinVolt = POWCDAT(IPWC,6) * nSerie



C ************** Calculations

C  Normally, the VRB DOD is defined as DOD = con_V3/(con_V2 + con_V3)
C  The chemical reactions for VRB are:
C  V5(+) + 2H(+) + e  <---> V4(+) + H2O
C          V2(+)      <---> V3(+) + e
C
C So, the amount (mole) of electron flow is equivalent to the amount of
C  vanadium ion changes.
C
C Since the ion concentrations vary while the chemical reactions occur, the
C concentrations at inlet and outlet are different. The average is used
C to calculate the open circuit voltage via the Nernst equation.

C     inlet ion concentrations
      con_V3_in = con_V3_discharged * batDOD
      con_V2_in = con_V3_discharged * (1-batDOD)
      con_V4_in = con_V3_discharged * batDOD
      con_V5_in = con_V3_discharged * (1-batDOD)
      con_H_in  = con_H_discharged + con_V2_in

C     electron flow rate
      electronFlowRate = current/Faraday_constant

      tempVariable = nSerie * electronFlowRate / electrolyteFlowRate

C     outlet ion concentrations
      IF(batMode .EQ. 1) THEN    !discharge
         con_V3_out = con_V3_in + tempVariable
         con_V2_out = con_V2_in - tempVariable
         con_V4_out = con_V4_in + tempVariable
         con_V5_out = con_V5_in - tempVariable
         con_H_out = con_H_in - tempVariable
      ELSEIF(batMode .EQ. 0) THEN   !charge
         con_V3_out = con_V3_in - tempVariable
         con_V2_out = con_V2_in + tempVariable
         con_V4_out = con_V4_in - tempVariable
         con_V5_out = con_V5_in + tempVariable
         con_H_out = con_H_in + tempVariable
      ELSE      !idle, batMode = 2
         con_V3_out = con_V3_in
         con_V2_out = con_V2_in
         con_V4_out = con_V4_in
         con_V5_out = con_V5_in
         con_H_out = con_H_in
      ENDIF

C    average ion concentrations
      con_V3_avg = (con_V3_in + con_V3_out)/2.0
      con_V2_avg = (con_V2_in + con_V2_out)/2.0
      con_V4_avg = (con_V4_in + con_V4_out)/2.0
      con_V5_avg = (con_V5_in + con_V5_out)/2.0
      con_H_avg = (con_H_in + con_H_out)/2.0


C    Use the Nernst equation to calculate the open circuit voltage
C    n =1 for VRB
      RT_over_nF = gas_constant * (electrolyteTemp + 273.15)
     &            / Faraday_constant

C   In the current stage, electrolyte temperature is regarded
C   as a constant for simplification. Therefore, the standard
C   potential VRB_E0 is a constant. Theoretically, however,VRB_E0
C   should be adjusted according to temperature when applying the
C   Nernst equation. This needs to be improved in the future.
C
      unitVoltage = VRB_E0 + RT_over_nF * log(con_V2_avg * con_V5_avg
     &                  * (con_H_avg**2)/(con_V3_avg * con_V4_avg))

      OCV_VRB = nSerie * unitVoltage

C     Make sure value is within boundaries
      IF (OCV_VRB .GT. batMaxVolt) THEN

         WRITE(IUOUT,*) 'Warning VRB Model: Free voltage above ',
     &                  ' max in function OCV_VRB', (NSINC)
         OCV_VRB = batMaxVolt

      ELSEIF(OCV_VRB .LT. batMinVolt) THEN

         WRITE(IUOUT,*) 'Warning VRB Model: Free voltage below ',
     &                  'min in function OCV_VRB', (NSINC)
         OCV_VRB = batMinVolt

      ENDIF

      END


C**************************************************************************
C 42 RInt_VRB
C**************************************************************************
C
C Initial Creation Date: December, 2007
C
C - This subroutine returns the internal resistance of the battery.
C   Ideally, the internal resistance should be modeled as a function of
C   temperature, the current, the depth of discharge.
C   That is why the function prototype still keeps the temperature and current as
C   its input parameters.
C   However, in the current stage, it is correlated to the state of charge only.
C
C - The polynom may be changed for a form that better suits available data
C
C This routine is called from various points of the program
C
C INPUTS:
C -batTemp: The battery's temperature (oC)
C -batDOD: The battery's depth of discharge
C -batI: The battery's current
C -BatMode: 0 for charge, 1 for discharge
C -IPWC: Power-only component index number
C
C OUTPUTS:
C -returns the battery's internal resistance (ohm)
C
C *********************************************************************
      REAL FUNCTION RInt_VRB(IPWC,batTemp,batDOD,batI,batMode)
      IMPLICIT NONE
#include "building.h"
#include "plant.h"
#include "power.h"

      COMMON/OUTIN/IUOUT,IUIN
      INTEGER iuout          !- write unit number
      INTEGER iuin           !- read unit number

C PARAMETERS FROM SIGNATURE
      REAL batTemp,batDOD,batI
      INTEGER batMode,IPWC


C Local variables

C Battery parameters
      INTEGER nParal,nSerie ! number of battery cells connected in series and parallel

C Coefficients for the correlated interal resistance equation
      REAL c0, c1, c2, c3, c4
      REAL unitRInt    !internal resistance for a unit cell
      REAL SOC_VRB     !state of charge

C************** Initialisation

      nParal = int(POWCDAT(IPWC,1))
      nSerie = int(POWCDAT(IPWC,2))

      IF(batMode .EQ. 0) THEN   ! charge mode -> use charge coefficients
         c0 = POWCDAT(IPWC,16)
         c1 = POWCDAT(IPWC,17)
         c2 = POWCDAT(IPWC,18)
         c3 = POWCDAT(IPWC,19)
         c4 = POWCDAT(IPWC,20)
      ELSE   ! discharge mode -> use discharge coefficient
         c0 = POWCDAT(IPWC,21)
         c1 = POWCDAT(IPWC,22)
         c2 = POWCDAT(IPWC,23)
         c3 = POWCDAT(IPWC,24)
         c4 = POWCDAT(IPWC,25)
      ENDIF

C************** Calculation
      SOC_VRB = 1- batDOD
      unitRInt = c0 + c1*SOC_VRB + c2*(SOC_VRB**2) + c3*(SOC_VRB**3)
     &              + c4*(SOC_VRB**4)

      IF (nParal .GT. 0.0 ) THEN
         RInt_VRB = nSerie * unitRInt/nParal
      ELSE
         WRITE(IUOUT,*) 'Error Battery Model: zero Parallel ',
     &                  'connections defined '
      ENDIF

C Make sure no negative value is returned
      IF (RInt_VRB .LE. 0.0) THEN
         WRITE(IUOUT,*) 'Warning Battery Model: Negative or Nul ',
     &                  ' resistance in routine RInt_VRB'
         RInt_VRB = 0.01
      ENDIF

      END



C**************************************************************************
C 43 FinalDOD_VRB
C**************************************************************************
C
C Initial Creation Date: December, 2007
C
C - This subroutine returns average depth of discharge during a constant current discharge step
C
C INPUTS:
C -DODIni: The battery's initial depth of discharge
C -batMode: 0 for charge, 1 for discharge
C -dtTStep: Duration of the time step (hour)
C -batCur: The power demanded/offered to the battery (W)
C -IPWC: Identity of the plant component associated with the model
C
C OUTPUTS:
C -returns the DOD at the end of the time step
C
C *********************************************************************
      REAL Function FinalDOD_VRB(IPWC,DODIni,batMode,dtTStep,batCur)
      IMPLICIT NONE
#include "building.h"
#include "plant.h"
#include "power.h"
#include "BATTERY.h"

C PARAMETERS FROM SIGNATURE
      REAL DODIni,dtTStep,batCur
      INTEGER batMode,IPWC

C Battery Parameters (technology and configuration)
      INTEGER nParal, nSerie    !Number of battery cells connected in paralel and in series
      REAL con_V3_discharged    ! concentration of V3+ at discharged state (DOD = 100%)
      REAL volume_electrolyte   !electrolyte volume (L)
C                               ! either anolyte or catholyte, not the total

C Local variables
      REAL timeStepSecond       ! time step in second
      REAL electron_mole        ! number of electrons in or out (mole)

C************** Initialisation
      nParal = int(POWCDAT(IPWC,1))
      nSerie = int(POWCDAT(IPWC,2))
      con_V3_discharged = POWCDAT(IPWC,3)
      volume_electrolyte = POWCDAT(IPWC, 11)

      timeStepSecond = dtTStep * 3600
      electron_mole = nSerie*batCur*timeStepSecond/Faraday_constant


C************** Calculation
      IF(batMode .EQ. 0) THEN   !Charge
         FinalDOD_VRB = DODIni - electron_mole/
     &                  (volume_electrolyte*con_V3_discharged)
      ELSE  !Discharge
         FinalDOD_VRB = DODIni + electron_mole/
     &                  (volume_electrolyte*con_V3_discharged)
      ENDIF

      END


C**************************************************************************
C 44 DoDischarge_VRB
C**************************************************************************
C
C Initial Creation Date: December, 2007
C
C - This subroutine returns the power repartition between the load and the auxiliary power
C   during a time step in discharge mode
C Power to heaters is not considered in the current stage.
C
C INPUTS:
C -batTIni: The battery's initial temperature (oC)
C -zoneT: Zone ambient temperature (oC)
C -DODIni: The battery's initial depth of discharge
C -batMode: 0 for charge, 1 for discharge
C -dtTStep: Duration of the time step (hour)
C -maxC: The maximum curent available from the battery (Amp)
C -maxP: The maximum power available from the battery (W)
C -IPWC: Power-only component index number
C -pLoad: The requested (demand) load (W)
C
C OUTPUTS:
C -pHeaters: The power to the heaters (W)
C -pBat: The actual power used by the battery (W)
C -batVolt: Battery average voltage corresponding to these loads (V)
C -batTFin: Battery temperature at the end of the time step (oC)
C -batDODFin: Battery DOD at the end of the time step (%)
C -batCur: Battery current (A)
C -pIntHeat: Heat generated by the current flowing in the internal resistance of the battery
C
C NOTE:
C - batMode = 1 since this is in discharge
C *********************************************************************
      SUBROUTINE DoDischarge_VRB(IPWC,batTIni,zoneT,DODIni,dtTStep,
     &                       maxC,maxP,pLoad,pBat,pHeaters,pParasitic,
     &                       batVolt,batTFin,batDODFin,batCur,pIntHeat)
      IMPLICIT NONE
#include "building.h"
#include "plant.h"
#include "power.h"

C PARAMETERS FROM SIGNATURE
      REAL batTIni,zoneT,DODIni,dtTStep,maxC,maxP,pLoad,pBat,pHeaters,
     &     pParasitic, batVolt,batTFin,batDODFin,batCur,pIntHeat
      INTEGER IPWC

C Declaration of functions used
      REAL AvgVolt,InternalHeat,CurForPow,FinalDOD_VRB

C Battery Parameters (technology and configuration)
      INTEGER nParal, nSerie    !Number of battery cells connected in paralel and in series

C Local variables
      LOGICAL close

C************** Initialisation

      nParal = int(POWCDAT(IPWC,1))
      nSerie = int(POWCDAT(IPWC,2))

C    Parasitic power takes a constant value in the current stage. This can be improved
C    in the future to vary with the current.
C    It is assumed that parasitic power is not required only VRB is idle. Therefore,
C    an if condition is used to consider this situation.
      CALL ECLOSE(pLoad, 0.0, 0.001, close)
      IF( close) THEN       ! no load applied
         pParasitic = 0.0
      ELSE
         pParasitic = POWCDAT(IPWC,13)
      ENDIF


C************** Calculation

C    Energy used for heaters is not considered for VRB in the current stage
C    It is set as zero.
      pHeaters = 0.

C    Evaluate the load on the battery
C    The load on the battery needs to consider the parasitic power
      pBat = MIN((pLoad+pHeaters+pParasitic),maxP)

      batCur = CurForPow(IPWC,batTIni,DODIni,1,dtTStep,pBat)

      batVolt = AvgVolt(IPWC,batTIni,DODIni,1,dtTStep,batCur)

      pIntHeat = InternalHeat(IPWC,batTIni,DODIni,1,dtTStep,batCur)

C     Get battery's final state
C     The temperature is not tracked in the current stage, so it is set as the inital value
      batTFin = batTIni
      batDODFin = FinalDOD_VRB(IPWC,DODIni,1,dtTStep,batCur)

      END



C**************************************************************************
C 45 DoCharge_VRB
C**************************************************************************
C
C Initial Creation Date: December, 2007
C
C - This subroutine returns the power repartition between the load and the auxiliary power
C   during a time step in charge mode and the average voltage of the battery
C  The power to heaters is not considered in the current stage.
C
C INPUTS:
C -batTIni: The battery's initial temperature (oC)
C -zoneT: Zone ambient temperature (oC)
C -DODIni: The battery's initial depth of discharge
C -batMode: 0 for charge, 1 for discharge
C -dtTStep: Duration of the time step (hour)
C -maxC: The maximum curent that can flow thriugh the battery (Amp)
C -maxP: The maximum power that can flow thriugh the battery (W)
C -IPWC: Power-only component index number
C -pLoad: The offer (what can be used) (W)
C
C OUTPUTS:
C -pHeaters: The power to the heaters (W)
C -pBat: The actual power used by the battery (W)
C -batVolt: Battery average voltage corresponding to these loads (V)
C -batTFin: Battery temperature at the end of the time step (oC)
C -batDODFin: Battery DOD at the end of the time step (%)
C -batCur: Battery current (A)
C -pIntHeat: Heat generated by the current flowing in the internal resistance of the battery
C
C NOTE:
C - batMode = 0 since this is in charge
c *********************************************************************
      SUBROUTINE DoCharge_VRB(IPWC,batTIni,zoneT,DODIni,dtTStep,
     &                    maxC,maxP,pLoad,pBat,pHeaters,pParasitic,
     &                    batVolt,batTFin,batDODFin,batCur,pIntHeat)
      IMPLICIT NONE
#include "building.h"
#include "plant.h"
#include "power.h"

      COMMON/OUTIN/IUOUT,IUIN
      INTEGER iuout          !- write unit number
      INTEGER iuin           !- read unit number

C PARAMETERS FROM SIGNATURE
      REAL batTIni,zoneT,DODIni,dtTStep,maxC,maxP,pLoad,pBat,pHeaters,
     &     pParasitic, batVolt,batTFin,batDODFin,batCur,pIntHeat
      INTEGER IPWC

C Declaration of functions used
      REAL AvgVolt,InternalHeat,CurForPow, FinalDOD_VRB

C Battery Parameters (technology and configuration)
      INTEGER nParal, nSerie    !Number of battery cells connected in paralel and in series

C Local variables
      REAL availForCharge  ! Available heat for charging (Total available - auxiliary power) (W)
      LOGICAL close


C************** Initialisation
C
      nParal = int(POWCDAT(IPWC,1))
      nSerie = int(POWCDAT(IPWC,2))

C    Parasitic power takes a constant value in the current stage. This can be improved
C    in the future to vary with the current.
      pParasitic = POWCDAT(IPWC,13)

C    Energy used for heaters is not considered for VRB in the current stage
C    It is set as zero.
      pHeaters = 0.

C     Calculate the available power for charging
      availForCharge = pLoad - pHeaters - pParasitic

C     The power used by the battery is either what is available or the maximum
C     Anyway, it cannot be less than zero.
      pBat = MAX(0.0, MIN(availForCharge, maxP))

C    VRB will not be charged if the available power (pLoad) is less than the parasitic
C    power (pParasitic). In this case, the parasitic power is reassigned as zero.
      CALL ECLOSE(pBat, 0.0, 0.001, close)
      IF( close) pParasitic = 0.0       ! no load applied

      batCur = CurForPow(IPWC,batTIni,DODIni,0,dtTStep,pBat)

C     Internal heat
      pIntHeat = InternalHeat(IPWC,batTIni,DODIni,0,dtTStep,batCur)

C Get battery's final state
C     The temperature is not tracked in the current stage, so it is set as the inital value
      batTFin = batTIni
      batDODFin = FinalDOD_VRB(IPWC,DODIni,0,dtTStep,batCur)

      END

C**************************************************************************
C 50 FinalDOD_Lion
C**************************************************************************
C
C Initial Creation Date: January, 2010 by Neil Saldanha
C
C - This subroutine returns average depth of discharge during a constant current discharge step
C
C INPUTS:
C -DODIni: The battery's initial depth of discharge
C -batMode: 0 for charge, 1 for discharge
C -dtTStep: Duration of the time step (hour)
C -batCur: The power demanded/offered to the battery (W)
C -IPWC: Identity of the plant component associated with the model
C
C OUTPUTS:
C -returns the DOD at the end of the time step
C
C *********************************************************************
      REAL Function FinalDOD_Lion(IPWC,batTemp,DODIni,batMode,dtTStep,
     &                            batCur)
      IMPLICIT NONE


C PARAMETERS FROM SIGNATURE
      REAL batTemp,DODIni,dtTStep,batCur
      INTEGER batMode,IPWC

C Declaration of functions used
      REAL BatteryCap_Lion
C************** Calculation
      IF(batMode .EQ. 0) THEN   !Charge
        FinalDOD_Lion = DODIni - batCur*dtTStep/
     &                  BatteryCap_Lion(IPWC,batTemp,batCur)
      ELSE  !Discharge
        FinalDOD_Lion = DODIni + batCur*dtTStep/
     &                  BatteryCap_Lion(IPWC,batTemp,batCur)
      ENDIF
      END

C**************************************************************************
C 51 BatteryCap_Lion
C**************************************************************************
C
C Initial Creation Date: January, 2010 by Neil Saldanha
C
C - This subroutine returns the capacity of the battery
C   as a function of its temperature, current
C   and the number of battery cells connected in parallel and in series
C
C This routine is called from various points of the program
C
C INPUTS:
C -batteryTemp: The battery's temperature (oC)
C -IPWC: Power-only component index number
C -batCur: The rate of charge/discharge (Amps)
C
C OUTPUTS:
C -returns the battery's capacity (Ah)
C
C *********************************************************************
      REAL FUNCTION BatteryCap_Lion(IPWC,batTemp,batCur)
      IMPLICIT NONE
#include "building.h"
#include "plant.h"
#include "power.h"
#include "BATTERY.h"

      COMMON/OUTIN/IUOUT,IUIN
      INTEGER iuout          !- write unit number
      INTEGER iuin           !- read unit number

C PARAMETERS FROM SIGNATURE
      REAL batTemp, batCur
      INTEGER IPWC

C Battery parameters
      REAL cellCapRef
      REAL cellCap
      INTEGER nParal

C Coefficients for the capacity equation
      REAL alpha, beta

C Declaration of functions used
      REAL temp_factor_lion
      REAL rate_factor_lion

C************** Initialisation

      cellCapRef = POWCDAT(IPWC,3)
      nParal = int(POWCDAT(IPWC,1))

C************** Calculations

C     Determine temperature (beta) and current (alpha) factors to scale reference capacity by

      beta = temp_factor_lion(IPWC,batTemp)

      alpha = rate_factor_lion(IPWC,batCur)

C     Account for degradation
      IF (cycles_used_Lion .LE. POWCDAT(IPWC,81)) THEN ! Battery is still in useful life region
        cellCap = (1.-0.2*cycles_used_Lion/POWCDAT(IPWC,81))
     &    *cellCapRef
      ELSE ! The battery is beyond useful life and is falling off
cx        cellCap = (8.8-.8*cycles_used_Lion/POWCDAT(IPWC,81))
cx     &    *cellCapRef
cx                <<add parameter "degradation factor" to change degradation
cx                  in post-useful life phase ??>>
        cellCap = (0.8-0.8*(1.-POWCDAT(IPWC,81)/cycles_used_Lion))
     &    *cellCapRef
      ENDIF

C     Capacity of the battery is the capacity of one cell times the number of cells in parallel
C
      BatteryCap_Lion = beta*alpha*cellCap*nParal

C     Make sure no negative value is returned
      IF (BatteryCap_Lion .LE. 0.0) THEN
        WRITE(IUOUT,*) 'Warning Battery Model: Negative or ',
     &                 'Nul capacity in function BatteryCap_Lion'
        BatteryCap_Lion = 0.01
      ENDIF

      END

C**************************************************************************
C 52 temp_factor_lion
C**************************************************************************
C
C Initial Creation Date: January, 2010 by Neil Saldanha
C
C - This subroutine returns temperature factor that
C   the battery capacity must scale by for the present
C   battery temperature
C
C This routine is called from various points of the program
C
C INPUTS:
C -batteryTemp: The battery's temperature (oC)
C
C OUTPUTS:
C -returns the temperature factor (beta)
C
C *********************************************************************
      REAL FUNCTION temp_factor_lion(IPWC,batTemp)
      IMPLICIT NONE
#include "building.h"
#include "plant.h"
#include "power.h"

      COMMON/OUTIN/IUOUT,IUIN
      INTEGER iuout          !- write unit number
      INTEGER iuin           !- read unit number

C PARAMETERS FROM SIGNATURE
      REAL batTemp
      INTEGER IPWC

C Temperature factors
      REAL beta_1, beta_2, beta_3, beta_4, beta_5,
     &     beta_6, beta_7, beta_8, t_1, t_2, t_3,
     &     t_4, t_5, t_6, t_7, t_8


C************** Initialisation

C These are the temperature correction factors and the
C corresponding temperatures discussed in Neil Saldanha's thesis

      t_1 = POWCDAT(IPWC,46)
      beta_1 = POWCDAT(IPWC,47)
      t_2 = POWCDAT(IPWC,49)
      beta_2 = POWCDAT(IPWC,50)
      t_3 = POWCDAT(IPWC,52)
      beta_3 = POWCDAT(IPWC,53)
      t_4 = POWCDAT(IPWC,55)
      beta_4 = POWCDAT(IPWC,56)
      t_5 = POWCDAT(IPWC,58)
      beta_5 = POWCDAT(IPWC,59)
      t_6 = POWCDAT(IPWC,61)
      beta_6 = POWCDAT(IPWC,62)
      t_7 = POWCDAT(IPWC,64)
      beta_7 = POWCDAT(IPWC,65)
      t_8 = POWCDAT(IPWC,67)
      beta_8 = POWCDAT(IPWC,68)


C************** Calculations

C     Linear interpolate to find temp factor based on present battery temperature
      IF (batTemp .LE. t_1) THEN ! battery is colder than lowest battery discharge temp
        temp_factor_lion = beta_1

      ELSEIF (batTemp .GE. t_1 .AND. batTemp .LE. t_2) THEN !between is between t_1 and t_2
        temp_factor_lion = (beta_2-beta_1)/(t_2-t_1)*batTemp +
     &                     (beta_1-(beta_2-beta_1)/(t_2-t_1)*t_1)

      ELSEIF (batTemp .GE. t_2 .AND. batTemp .LE. t_3) THEN !between is between t_2 and t_3
        temp_factor_lion = (beta_3-beta_2)/(t_3-t_2)*batTemp +
     &                     (beta_2-(beta_3-beta_2)/(t_3-t_2)*t_2)

      ELSEIF (batTemp .GE. t_3 .AND. batTemp .LE. t_4) THEN !between is between t_3 and t_4
        temp_factor_lion = (beta_4-beta_3)/(t_4-t_3)*batTemp +
     &                     (beta_3-(beta_4-beta_3)/(t_4-t_3)*t_3)

      ELSEIF (batTemp .GE. t_4 .AND. batTemp .LE. t_5) THEN !between is between t_4 and t_5
        temp_factor_lion = (beta_5-beta_4)/(t_5-t_4)*batTemp +
     &                     (beta_4-(beta_5-beta_4)/(t_5-t_4)*t_4)

      ELSEIF (batTemp .GE. t_5 .AND. batTemp .LE. t_6) THEN !between is between t_5 and t_6
        temp_factor_lion = (beta_6-beta_5)/(t_6-t_5)*batTemp +
     &                     (beta_5-(beta_6-beta_5)/(t_6-t_5)*t_5)

      ELSEIF (batTemp .GE. t_6 .AND. batTemp .LE. t_7) THEN !between is between t_6 and t_7
        temp_factor_lion = (beta_7-beta_6)/(t_7-t_6)*batTemp +
     &                     (beta_6-(beta_7-beta_6)/(t_7-t_6)*t_6)

      ELSEIF (batTemp .GE. t_7 .AND. batTemp .LE. t_8) THEN !between is between t_7 and t_8
        temp_factor_lion = (beta_8-beta_7)/(t_8-t_7)*batTemp +
     &                     (beta_7-(beta_8-beta_7)/(t_8-t_7)*t_7)

      ELSEIF (batTemp .GE. t_8) THEN ! battery is hotter than highest battery discharge temp
        temp_factor_lion = beta_8

      ELSE ! really shouldn't happen
        WRITE(IUOUT,*) 'Warning Battery Model: odd temperature ',
     &                 'factor in function temp_factor_lion'
        temp_factor_lion = 1.
      ENDIF

      END


C**************************************************************************
C 53 rate_factor_lion
C**************************************************************************
C
C Initial Creation Date: January, 2010 by Neil Saldanha
C
C - This subroutine returns current factor that
C   the battery capacity must scale by for the present
C   battery temperature
C
C This routine is called from various points of the program
C
C INPUTS:
C -batCur: The battery's current(oC)
C
C OUTPUTS:
C -returns the current factor (alpha)
C
C *********************************************************************
      REAL FUNCTION rate_factor_lion(IPWC,batCur)
      IMPLICIT NONE
#include "building.h"
#include "plant.h"
#include "power.h"

      COMMON/OUTIN/IUOUT,IUIN
      INTEGER iuout          !- write unit number
      INTEGER iuin           !- read unit number

C PARAMETERS FROM SIGNATURE
      REAL batCur
      INTEGER IPWC


C************** Initialisation

C Current factors
      REAL alpha_1, alpha_2, alpha_3, alpha_4, alpha_5,
     &     alpha_6, alpha_7, alpha_8, i_1, i_2, i_3,
     &     i_4, i_5, i_6, i_7, i_8


C These are the rate correction factors and the
C corresponding rates discussed in Neil Saldanha's thesis

      i_1 = POWCDAT(IPWC,30)
      alpha_1 = POWCDAT(IPWC,31)
      i_2 = POWCDAT(IPWC,32)
      alpha_2 = POWCDAT(IPWC,33)
      i_3 = POWCDAT(IPWC,34)
      alpha_3 = POWCDAT(IPWC,35)
      i_4 = POWCDAT(IPWC,36)
      alpha_4 = POWCDAT(IPWC,37)
      i_5 = POWCDAT(IPWC,38)
      alpha_5 = POWCDAT(IPWC,39)
      i_6 = POWCDAT(IPWC,40)
      alpha_6 = POWCDAT(IPWC,41)
      i_7 = POWCDAT(IPWC,42)
      alpha_7 = POWCDAT(IPWC,43)
      i_8 = POWCDAT(IPWC,44)
      alpha_8 = POWCDAT(IPWC,45)


C************** Calculations

C     Linear interpolate to find temp factor based on present battery temperature
      IF (batCur .LE. i_1) THEN ! battery is colder than lowest battery discharge temp
        rate_factor_lion = alpha_1

      ELSEIF (batCur .GE. i_1 .AND. batCur .LE. i_2) THEN !between is between i_1 and i_2
        rate_factor_lion = (alpha_2-alpha_1)/(i_2-i_1)*batCur +
     &                     (alpha_1-(alpha_2-alpha_1)/(i_2-i_1)*i_1)

      ELSEIF (batCur .GE. i_2 .AND. batCur .LE. i_3) THEN !between is between i_2 and i_3
        rate_factor_lion = (alpha_3-alpha_2)/(i_3-i_2)*batCur +
     &                     (alpha_2-(alpha_3-alpha_2)/(i_3-i_2)*i_2)

      ELSEIF (batCur .GE. i_3 .AND. batCur .LE. i_4) THEN !between is between i_3 and i_4
        rate_factor_lion = (alpha_4-alpha_3)/(i_4-i_3)*batCur +
     &                     (alpha_3-(alpha_4-alpha_3)/(i_4-i_3)*i_3)

      ELSEIF (batCur .GE. i_4 .AND. batCur .LE. i_5) THEN !between is between i_4 and i_5
        rate_factor_lion = (alpha_5-alpha_4)/(i_5-i_4)*batCur +
     &                     (alpha_4-(alpha_5-alpha_4)/(i_5-i_4)*i_4)

      ELSEIF (batCur .GE. i_5 .AND. batCur .LE. i_6) THEN !between is between i_5 and i_6
        rate_factor_lion = (alpha_6-alpha_5)/(i_6-i_5)*batCur +
     &                     (alpha_5-(alpha_6-alpha_5)/(i_6-i_5)*i_5)

      ELSEIF (batCur .GE. i_6 .AND. batCur .LE. i_7) THEN !between is between i_6 and i_7
        rate_factor_lion = (alpha_7-alpha_6)/(i_7-i_6)*batCur +
     &                     (alpha_6-(alpha_7-alpha_6)/(i_7-i_6)*i_6)

      ELSEIF (batCur .GE. i_7 .AND. batCur .LE. i_8) THEN !between is between i_7 and i_8
        rate_factor_lion = (alpha_8-alpha_7)/(i_8-i_7)*batCur +
     &                     (alpha_7-(alpha_8-alpha_7)/(i_8-i_7)*i_7)

      ELSEIF (batCur .GE. i_8) THEN ! battery is hotter than highest battery discharge temp
        rate_factor_lion = alpha_8

      ELSE ! really shouldn't happen
        WRITE(IUOUT,*) 'Warning Battery Model: odd current ',
     &                 'factor in function rate_factor_lion'
        rate_factor_lion = 1.
      ENDIF

      END


C**************************************************************************
C 54 OCV_Lion
C**************************************************************************
C
C Created by: Neil Saldanha, January 2010
C
C - This subroutine returns the free voltage of the battery
C   as a function of its depth of discharge
C
C This routine is called from various points of the program
C
C INPUTS:
C -batDOD: The battery's depth of discharge
C -IPWC: Power-only component index
C
C OUTPUTS:
C -returns the battery's free voltage
C
C *********************************************************************
      REAL FUNCTION OCV_Lion(IPWC,batDOD,batTemp)
      IMPLICIT NONE
#include "building.h"
#include "plant.h"
#include "power.h"

      COMMON/OUTIN/IUOUT,IUIN
      INTEGER iuout          !- write unit number
      INTEGER iuin           !- read unit number

C Common holding NSINC (no of building time step increments) for debugging only
       COMMON/SIMTIM/IHRP,IHRF,IDYP,IDYF,IDWP,IDWF,NSINC,ITS
       INTEGER IHRP            !-hour of present time-step
       INTEGER IHRF            !-hour of future time-step
       INTEGER IDYP            !-year day number of present day
       INTEGER IDYF            !-year day number of future day
       INTEGER IDWP            !-day of the week of present day
       INTEGER IDWF            !-day of the week of future day
       INTEGER NSINC           !-number of building-side time increments
                               !-since start of simulation
       INTEGER ITS             !-current building time-step within
                               !-current hour

C PARAMETERS FROM SIGNATURE
      REAL batDOD, batTemp
      INTEGER IPWC

C Declarations of function used
      REAL voltage_offset_temp

C Local variables
      REAL deltaVforT

C Coefficients for the freeVoltage equation
      REAL E5, E4, E3, E2, E1,E0
      INTEGER nSerie ! number of battery cells connected in series

C Battery Parameters (technology and configuration)
      REAL batMaxVolt      ! Maximum manufacturer voltage of a battery cell (0% DOD) (V)
      REAL batMinVolt      ! Minimum manufacturer voltage of a battery cell (100% DOD) (V)

C************** Initialisation

C These will be changed for the proper references once the "ACEP_BATTERY.h" will be replaced with the
C proper node in the electrical network
      E5 = POWCDAT(IPWC,18)
      E4 = POWCDAT(IPWC,19)
      E3 = POWCDAT(IPWC,20)
      E2 = POWCDAT(IPWC,21)
      E1 = POWCDAT(IPWC,22)
      E0 = POWCDAT(IPWC,23)

      nSerie = int(POWCDAT(IPWC,2))

C     Voltages are multiplied by the number of units connected in series
      batMaxVolt = POWCDAT(IPWC,5) * float(nSerie)
      batMinVolt = POWCDAT(IPWC,6) * float(nSerie)

C ************** Calculations


C************** Temperature offset
C This takes into account temperature's affect on voltage
C as discussed by Gao(2002) and in Neil Saldanha's thesis.
C It only affect the lion model but should be looked at for
C others

      deltaVforT = voltage_offset_temp(IPWC,batTemp)


C     Free voltage of the battery is the free voltage of a cell time the number of cells in series
      OCV_Lion = ((E5*batDOD**5. + E4*batDOD**4. + E3*batDOD**3. +
     &               E2*batDOD**2. + E1*batDOD + E0) - deltaVforT)
     &               *float(nSerie)

C     Make sure value is within boundaries
C     This check is partly bypassed if the lead-acid battery is performing a mandatory
C     charge cycle, because then the voltage is expected to surpass the batMaxVolt
      IF ((OCV_Lion + deltaVforT*float(nSerie)) .GT. batMaxVolt) THEN
        WRITE(IUOUT,*) 'Warning Battery Model: Free voltage above ',
     &                  ' max in function OCV_Lion', (NSINC)
        OCV_Lion = batMaxVolt - deltaVforT

      ELSEIF((OCV_Lion + deltaVforT*float(nSerie)) .LT. batMinVolt) THEN
        WRITE(IUOUT,*) 'Warning Battery Model: Free voltage below ',
     &                 'min in function OCV_Lion', (NSINC)
        OCV_Lion = batMinVolt - deltaVforT
      ENDIF

      END

C**************************************************************************
C 55 RInt_Lion
C**************************************************************************
C
C Created by: Neil Saldanha, January 2010
C
C - This subroutine returns the internal resistance of the battery
C   as a function of its temperature, the current, the depth of discharge
C   and the number of battery cells connected in parallel and in series
C
C This routine is called from various points of the program
C
C INPUTS:
C -batTemp: The battery's temperature (oC)
C -batDOD: The battery's depth of discharge
C -batI: The battery's current
C -BatMode: 0 for charge, 1 for discharge
C -IPWC: Power-only component index number
C
C OUTPUTS:
C -returns the battery's internal resistance (Ohm)
C
C *********************************************************************
      REAL FUNCTION RInt_Lion(IPWC,batDOD,batMode,batCur)
      IMPLICIT NONE
#include "building.h"
#include "plant.h"
#include "power.h"

      COMMON/OUTIN/IUOUT,IUIN
      INTEGER iuout          !- write unit number
      INTEGER iuin           !- read unit number

C PARAMETERS FROM SIGNATURE
      REAL batDOD, batCur
      INTEGER batMode,IPWC

C Battery parameters
      INTEGER nParal,nSerie ! number of battery cells connected in series and parallel
C Coefficients for the capacity equation
      REAL R5lo,R4lo,R3lo,R2lo,R1lo,R0lo,
     & R5hi,R4hi,R3hi,R2hi,R1hi,R0hi,Iflag

C************** Initialisation

      IF(batMode .EQ. 0) THEN   ! charge mode -> use charge coefficients
         R5lo = (POWCDAT(IPWC,24))
         R4lo = (POWCDAT(IPWC,25))
         R3lo = (POWCDAT(IPWC,26))
         R2lo = (POWCDAT(IPWC,27))
         R1lo = (POWCDAT(IPWC,28))
         R0lo = (POWCDAT(IPWC,29))
         R5hi = (POWCDAT(IPWC,74))
         R4hi = (POWCDAT(IPWC,75))
         R3hi = (POWCDAT(IPWC,76))
         R2hi = (POWCDAT(IPWC,77))
         R1hi = (POWCDAT(IPWC,78))
         R0hi = (POWCDAT(IPWC,79))
         Iflag = (POWCDAT(IPWC,80))
      ELSE   ! discharge mode -> use discharge coefficient
         R5lo = (POWCDAT(IPWC,24))
         R4lo = (POWCDAT(IPWC,25))
         R3lo = (POWCDAT(IPWC,26))
         R2lo = (POWCDAT(IPWC,27))
         R1lo = (POWCDAT(IPWC,28))
         R0lo = (POWCDAT(IPWC,29))
         R5hi = (POWCDAT(IPWC,74))
         R4hi = (POWCDAT(IPWC,75))
         R3hi = (POWCDAT(IPWC,76))
         R2hi = (POWCDAT(IPWC,77))
         R1hi = (POWCDAT(IPWC,78))
         R0hi = (POWCDAT(IPWC,79))
         Iflag = (POWCDAT(IPWC,80))
      ENDIF
      nSerie = int(POWCDAT(IPWC,2))
      nParal = int(POWCDAT(IPWC,1))
c      cellCapRef = POWCDAT(IPWC,3)

C************** Calculation


      IF (R5lo .GE. 999. .OR. !Check if a second set of Rint curves has been declared
     &    R4lo .GE. 999. .OR.
     &    R3lo .GE. 999. .OR.
     &    R2lo .GE. 999. .OR.
     &    R1lo .GE. 999. .OR.
     &    Iflag .GE. 999.) THEN !no it hasn't, use original curves

        !current for only one cell -> /nParal
        RInt_Lion = FLOAT(nSerie)/FLOAT(nParal)*
     &   (R5lo*batDOD**5. + R4lo*batDOD**4. + R3lo*batDOD**3. +
     &    R2lo*batDOD**2. + R1lo*batDOD + R0lo)

      ELSE !yes a second set has been declared
        IF (batCur .LT. Iflag) THEN !current is under flag, use lo set
          RInt_Lion = FLOAT(nSerie)/FLOAT(nParal)*
     &   (R5lo*batDOD**5. + R4lo*batDOD**4. + R3lo*batDOD**3. +
     &    R2lo*batDOD**2. + R1lo*batDOD + R0lo)

        ELSE !current is above flag use hi set
        !current for only one cell -> /nParal
          RInt_Lion = FLOAT(nSerie)/FLOAT(nParal)*
     &   (R5hi*batDOD**5. + R4hi*batDOD**4. + R3hi*batDOD**3. +
     &    R2hi*batDOD**2. + R1hi*batDOD + R0hi)
        ENDIF
      ENDIF

C Make sure no negative value is returned
      IF (RInt_Lion .LE. 0.0) THEN
         WRITE(IUOUT,*) 'Warning Battery Model: Negative or Nul ',
     &                  ' resistance in routine Rint_Lion'
         RInt_Lion = 0.01
      ENDIF

      END


C**************************************************************************
C voltage_offset_temp
C**************************************************************************
C
C Created By Neil Saldanha January 2010
C
C - This subroutine
C
C INPUTS:
C batTemp: battery temperature
C
C OUTPUTS:
C voltage_offset_temp: voltage offset due to temperature
c *********************************************************************
      REAL FUNCTION voltage_offset_temp(IPWC,batTemp)

      IMPLICIT NONE
#include "building.h"
#include "plant.h"
#include "power.h"

      COMMON/OUTIN/IUOUT,IUIN
      INTEGER iuout          !- write unit number
      INTEGER iuin           !- read unit number

C PARAMETERS FROM SIGNATURE
      REAL batTemp
      INTEGER IPWC

C Temperature factors
      REAL offset_1, offset_2, offset_3, offset_4, offset_5,
     &     offset_6, offset_7, offset_8, t_1, t_2, t_3,
     &     t_4, t_5, t_6, t_7, t_8


C************** Initialisation

C These are the voltage offset correction factors and the
C corresponding temperatures discussed in Neil Saldanha's thesis

      t_1 = POWCDAT(IPWC,46)
      offset_1 = POWCDAT(IPWC,48)
      t_2 = POWCDAT(IPWC,49)
      offset_2 = POWCDAT(IPWC,51)
      t_3 = POWCDAT(IPWC,52)
      offset_3 = POWCDAT(IPWC,54)
      t_4 = POWCDAT(IPWC,55)
      offset_4 = POWCDAT(IPWC,57)
      t_5 = POWCDAT(IPWC,58)
      offset_5 = POWCDAT(IPWC,60)
      t_6 = POWCDAT(IPWC,61)
      offset_6 = POWCDAT(IPWC,63)
      t_7 = POWCDAT(IPWC,64)
      offset_7 = POWCDAT(IPWC,66)
      t_8 = POWCDAT(IPWC,67)
      offset_8 = POWCDAT(IPWC,69)


C************** Calculations

C     Linear interpolate to find voltage offset factor based on present battery temperature
      IF (batTemp .LE. t_1) THEN ! battery is colder than lowest battery discharge temp
        voltage_offset_temp = offset_1

      ELSEIF (batTemp .GE. t_1 .AND. batTemp .LE. t_2) THEN !between is between t_1 and t_2
        voltage_offset_temp = (offset_2-offset_1)/(t_2-t_1)*batTemp +
     &                     (offset_1-(offset_2-offset_1)/(t_2-t_1)*t_1)

      ELSEIF (batTemp .GE. t_2 .AND. batTemp .LE. t_3) THEN !between is between t_2 and t_3
        voltage_offset_temp = (offset_3-offset_2)/(t_3-t_2)*batTemp +
     &                     (offset_2-(offset_3-offset_2)/(t_3-t_2)*t_2)

      ELSEIF (batTemp .GE. t_3 .AND. batTemp .LE. t_4) THEN !between is between t_3 and t_4
        voltage_offset_temp = (offset_4-offset_3)/(t_4-t_3)*batTemp +
     &                     (offset_3-(offset_4-offset_3)/(t_4-t_3)*t_3)

      ELSEIF (batTemp .GE. t_4 .AND. batTemp .LE. t_5) THEN !between is between t_4 and t_5
        voltage_offset_temp = (offset_5-offset_4)/(t_5-t_4)*batTemp +
     &                     (offset_4-(offset_5-offset_4)/(t_5-t_4)*t_4)

      ELSEIF (batTemp .GE. t_5 .AND. batTemp .LE. t_6) THEN !between is between t_5 and t_6
        voltage_offset_temp = (offset_6-offset_5)/(t_6-t_5)*batTemp +
     &                     (offset_5-(offset_6-offset_5)/(t_6-t_5)*t_5)

      ELSEIF (batTemp .GE. t_6 .AND. batTemp .LE. t_7) THEN !between is between t_6 and t_7
        voltage_offset_temp = (offset_7-offset_6)/(t_7-t_6)*batTemp +
     &                     (offset_6-(offset_7-offset_6)/(t_7-t_6)*t_6)

      ELSEIF (batTemp .GE. t_7 .AND. batTemp .LE. t_8) THEN !between is between t_7 and t_8
        voltage_offset_temp = (offset_8-offset_7)/(t_8-t_7)*batTemp +
     &                     (offset_7-(offset_8-offset_7)/(t_8-t_7)*t_7)

      ELSEIF (batTemp .GE. t_8) THEN ! battery is hotter than highest battery discharge temp
        voltage_offset_temp = offset_8

      ELSE ! really shouldn't happen
        WRITE(IUOUT,*) 'Warning Battery Model: odd voltage offset ',
     &                 'factor in function voltage_offset_temp'
        voltage_offset_temp = 0.
      ENDIF



      END

C**************************************************************************
C Lion_BMS
C**************************************************************************
C
C Created By Neil Saldanha January 2010
C
C - This subroutine
C
C INPUTS:
C -IPWC: Power-only component index number
C -batTIni: The battery's initial temperature (oC)
C -zoneT: Zone ambient temperature (oC)
C -DODIni: The battery's initial depth of discharge
C -dtTStep: Duration of the time step (hour)
C -batMode: 0 for charge, 1 for discharge
C -maxC: The maximum curent that can flow through the battery (Amp)
C -maxP: The maximum power that can flow through the battery (W)
C -pLoad: The offer (what can be used) (W)
C
C OUTPUTS:
C -pParasitic: The power to run the circuitry (W)
C -pHeaters: The power to the heaters (W)
C -pBat: The actual power used by the battery (W)
C -batVolt: Battery average voltage corresponding to these loads (V)
C -batTFin: Battery temperature at the end of the time step (oC)
C -batDODFin: Battery DOD at the end of the time step (%)
C -batCur: Battery current (A)
C -pIntHeat: Heat generated by the current flowing in the internal resistance of the battery
C
c *********************************************************************
      SUBROUTINE Lion_BMS(IPWC,batTIni,zoneT,DODIni,dtTStep,batMode,
     &                    maxC,maxP,pLoad,pBat,pHeaters,pParasitic,
     &                    batVolt,batTFin,batDODFin,batCur,pIntHeat,
     &                    hiVoltLion,loVoltLion,hiTempLion,loTempLion,
     &                    gas_constant, cycles_used_Lion)
      IMPLICIT NONE
#include "building.h"
#include "plant.h"
#include "power.h"

      COMMON/OUTIN/IUOUT,IUIN
      INTEGER iuout          !- write unit number
      INTEGER iuin           !- read unit number

      common/pctstp/ntstpp
      integer ntstpp            ! number of plant timesteps / building timestep

C PARAMETERS FROM SIGNATURE (see routine comments for description)
      REAL batTIni,zoneT,DODIni,dtTStep,maxC,maxP,pLoad,pBat,pHeaters,
     &     pParasitic, batVolt,batTFin,batDODFin,batCur,pIntHeat
      INTEGER IPWC, batMode
      INTEGER hiVoltLion,loVoltLion,hiTempLion,loTempLion

C Declaration of functions used
      REAL FinalDOD_Lion, CurForPow, AvgVolt, InternalHeat, RInt_Lion,
     &     BatteryCap_Lion, PowForCur, MaxCurrent

C Battery Parameters (technology and configuration)
      INTEGER nParal, nSerie,    !Number of battery cells connected in paralel and in series
     &        batMode_or,        !original batMode
     &        iteration,         !iterations
     &        max_iteration      !max iteration

C Local variables
C      REAL availpower,      ! Available power from the battery (W)
      REAL self_discharge,  ! self discharge of battery in amps
     &     ko,              ! self discharge constant in amps
     &     E_as,            ! activation energy
     &     heatersRes,      ! Heater resistance in ohms
     &     hA,              ! heat loss factor W/oC
     &     mCp,             ! thermal mass
     &     gas_constant,    ! gas constant (8.314..)
     &     needed_power,    ! charge/discharge needed to bring battery back into safe limits
     &     dummy,           ! dummy variable for iterations
     &     relax,           ! relaxation factor
     &     pLoad_or,        ! original load offered
     &     neededHeat,      ! heat needed to return to safe operation
     &     cycles_used_Lion,! number of cycles exhausted for degradation
     &     maxPheaters      ! maximum heat possible based on current

C************** Initialisation
      nParal = int(POWCDAT(IPWC,1))
      nSerie = int(POWCDAT(IPWC,2))
      pParasitic = (POWCDAT(IPWC,70))
      ko = (POWCDAT(IPWC,70))*float(nParal)
      E_as = (POWCDAT(IPWC,71))
      heatersRes = POWCDAT(IPWC,13)*float(nSerie)/(float(nParal)) !Should that be plus or time series & parrallel?
      hA = POWCDAT(IPWC,12) !This will now be treated as a single full battery *(float(nParal)+float(nSerie))
      mCp = POWCDAT(IPWC,11)*(float(nParal)+float(nSerie))

C Initial heater electric draw in W, this will change if the low temp flag is on
      pHeaters = 0.

C Same with self discharge (changes only if battery is in idle)
      self_discharge = 0.

C Store the original battery mode and load for reference
      batMode_or = batMode
      pLoad_or = pLoad

C Set a relaxation factor & iterations
      relax = 0.1
      iteration = 0
      max_iteration = 100

      IF (hiVoltLion .EQ. 1) THEN      ! Over voltage flag is on, battery needs to discharge
C evaluate how much the battery would have to dump over the next time step to bring into
C safe limits
        batMode = 1
        dummy = 0.
        batCur = 100. !initial high guess

        DO WHILE (abs(batCur-dummy) .GT. relax .AND. iteration
     &        .LT. max_iteration )
          dummy = batCur

C         The needed energy in watts to return to safe points
          needed_power = (POWCDAT(IPWC,15)-DODIni)                 !delta DOD
     &        *BatteryCap_Lion(IPWC,batTIni,batCur)                !battery capacity
     &        *AvgVolt(IPWC,batTIni,DODIni,batMode,dtTStep,dummy)  !voltage
     &        /dtTStep                                             !time step
          batCur = CurForPow(IPWC,batTIni,DODIni,batMode,dtTStep,
     &             needed_power)
          iteration = iteration + 1
        END DO

        pLoad = needed_power

        IF (iteration .EQ. max_iteration) THEN
          WRITE(IUOUT,*) 'Warning Battery Model: Troublesome',
     &                   'Convergeance in Lion_BMS hiVoltage flag'
        ENDIF

        IF (batMode_or .NE. 1) THEN
C         Originally in charge or idle mode. Re-evaluate the max current
C         and power that you could discharge now that you changed modes
          maxC = MaxCurrent(IPWC,batTIni,DODIni,batMode,dtTStep)
          maxP = PowForCur(IPWC,batTIni,DODIni,batMode,dtTStep,
     &                             maxC)
        endif

        IF (pLoad_or .GE. pLoad) THEN ! What can be offered exceeds what needs to be dumped
          pLoad = pLoad_or !assign load back to original because it is enough to safely return battery

        ELSE ! What can be offered doesn't exceed and some will dump to the grid

        ENDIF

      ELSEIF (loVoltLion .EQ. 1) THEN  ! Under voltage flag is on, battery needs to charge
C evaluate how much the battery would have to dump over the next time step to bring into
C safe limits
        batMode = 0
        dummy = 0.
        batCur = 100.

        DO WHILE (abs(batCur-dummy) .GT. relax .AND. iteration
     &        .LT. max_iteration )
          dummy = batCur
          needed_power = (DODIni-POWCDAT(IPWC,14))                 !delta DOD
     &        *BatteryCap_Lion(IPWC,batTIni,batCur)                !battery capacity
     &        *AvgVolt(IPWC,batTIni,DODIni,batMode,dtTStep,dummy)  !voltage
     &        /dtTStep                                             !time step
          batCur = CurForPow(IPWC,
     &             batTIni,DODIni,batMode,dtTStep,needed_power)
          iteration = iteration + 1
        END DO

        pLoad = needed_power

        IF (iteration .EQ. max_iteration) THEN
          WRITE(IUOUT,*) 'Warning Battery Model: Troublesome',
     &                   'Convergeance in Lion_BMS loVoltage flag'
        ENDIF

        IF (batMode_or .NE. 0) THEN ! discharge or idle, can't happen
                                 ! figure out charge needed over next time step
                                 ! to bring into safe limits

C         Re-evaluate the max current and power that you could discharge now that you changed modes
          maxC = MaxCurrent(IPWC,batTIni,DODIni,batMode,dtTStep)
          maxP = PowForCur(IPWC,batTIni,DODIni,batMode,dtTStep,maxC)

C << logic most likely also flawed, as with hiVolt above ... >>
        ELSE ! Battery in charge or idle determine how much excess power the batt needs to charge

          IF (pLoad_or .GE. pLoad) THEN ! What can be offered exceeds what needs to be dumped
            pLoad = pLoad_or !assign load back to original because it is enough to safely return battery

          ELSE ! What can be offered doesn't exceed and some will dump to the grid

          ENDIF

        ENDIF

      ELSEIF (hiTempLion .EQ. 1) THEN      ! Over temp flag is on, battery needs to idle
        batMode = 2
        pLoad = 0.
        self_discharge = max(ko*exponent(-E_as/(gas_constant*batTIni))
     &                   *(1-DODIni),0.)


      ELSEIF (loTempLion .EQ. 1) THEN  ! Under temp flag is on, battery needs heat
C figure out heat needed from battery to bring back into safe limits

        batMode = 1 ! Set the battery to discharge
        dummy = 100.
        batCur = 0.

        maxC = MaxCurrent(IPWC,batTIni,DODIni,batMode,dtTStep)
        maxP = PowForCur(IPWC,batTIni,DODIni,batMode,dtTStep,
     &                             maxC)



        DO WHILE (abs(batCur-dummy) .GT. relax .AND. iteration
     &        .LT. max_iteration )

          dummy = batCur
          maxPheaters = AvgVolt(IPWC,batTIni,DODIni,batMode,
     &    dtTStep,maxC)**2./
     &    heatersRes !maximum heat that can be drawn from battery

          neededHeat = ((POWCDAT(IPWC,17)-batTIni*exp(-hA*
     &    dtTStep*3600./mCp))
     &    /(1.-exp(-hA*dtTStep*3600./mCp))-zoneT)*hA -
     &    RInt_Lion(IPWC,DODIni,batMode,batCur)*dummy**2.

          neededHeat = max(0.,neededHeat) ! prevent a negative error

          pHeaters = min(neededHeat,maxPheaters,maxP) !needed heat can't exceed maximum

          pHeaters = max(0.,pHeaters) ! prevent a negative error

          if (batMode_or .eq. 0) then !this was originally in charge, none of the load can be met
            pLoad = 0.
          else
            pLoad = max((pLoad_or - pHeaters),0.) ! this is what's left to satisfy the imposed discharge
          endif

          batCur = CurForPow(IPWC,batTIni,
     &             DODIni,batMode,dtTStep,pLoad)

          iteration = iteration + 1

        END DO

        IF (iteration .EQ. max_iteration) THEN
          WRITE(IUOUT,*) 'Warning Battery Model: Troublesome',
     &                   'Convergeance in Lion_BMS loTemp flag'
        ENDIF

      ELSE ! Normal operation

        IF (batMode .EQ. 2) THEN  ! Idle, get self discharge
          self_discharge = max(ko*exponent(-E_as/(gas_constant*batTIni))
     &                     *(1-DODIni),0.)

        ELSEIF (batMode .EQ. 1) THEN ! Discharge

        ELSEIF (batMode .EQ. 0) THEN ! Charge

        ENDIF

      ENDIF

      IF (batMode .EQ. 0) THEN !Charge is the load minus what's needed for heaters and parasitic loads
        pBat = MIN((pLoad-pHeaters-pParasitic),maxP)

      ELSE !Discharge, the battery has to take up the extra loads
        pBat = MIN((pLoad+pHeaters+pParasitic),maxP)

      ENDIF


C     Evaluate the current on the battery
      batCur = CurForPow(IPWC,batTIni,DODIni,batMode,dtTStep,pBat)
     &         + self_discharge

C     Evaluate the voltage on the battery
      batVolt = AvgVolt(IPWC,batTIni,DODIni,batMode,dtTStep,batCur)

C     Evaluate the internal heat generated by the battery
      pIntHeat = InternalHeat(IPWC,batTIni,DODIni,batMode,dtTStep,
     &          batCur)

C     Get battery's final state
      batDODFin = FinalDOD_Lion(IPWC,batTIni,DODIni,batMode,
     &            dtTStep,batCur)

C     Get battery's final temperature
      batTFin = batTIni*exp(-hA*dtTStep*3600./mCp) + (zoneT +
     &  (RInt_Lion(IPWC,(DODIni+batDODFin)/2.,batMode,batCur)
     &  *batCur**2. + pHeaters)/hA)*(1. -
     &  exp(-hA*dtTStep*3600./mCp))

C**To keep the battery at 25 Celcius for debugging
C      batTFin = 25.

C  Find out how much of the battery cycle life has depleted
      cycles_used_Lion = cycles_used_Lion +
     &                   abs(batDODFin-DODIni)
     &                   /float(ntstpp) !account for no. of plant steps/building step
      END

C**************************************************************************
C 99 List of input parameters lead acid battery model
C**************************************************************************

C   POWCDAT(IPWC,1):  Number of unit cells connected in parallel (-)
C   POWCDAT(IPWC,2):  Number of unit cells connected in series (-)
C   POWCDAT(IPWC,3):  cellCapRef, reference capacity for a unit cell (Amp h)
C   POWCDAT(IPWC,4):  batTOper, battery operating temperature (°C)

C   POWCDAT(IPWC,5):  Max. voltage for a unit cell at 100% SOC (fully charged), DOD=0 (V)
C   POWCDAT(IPWC,6):  Min. voltage for a unit cell at 0% SOC (fully discharged), DOD=100% (V)
C   POWCDAT(IPWC,7):  Max. allowable current for a unit cell while charging (Amp)
C   POWCDAT(IPWC,8):  Max. allowable voltage for a unit cell while charging (V)

C   POWCDAT(IPWC,9):  batTempInit, initial battery temperature (°C)
C   POWCDAT(IPWC,10): batDODInit, initial battery DOD (-)

C   POWCDAT(IPWC,11): thermalMass, thermal mass of the battery-mass*Cp (J/°C)
C   POWCDAT(IPWC,12): heatLossFact, heat loss factor h*A (W/°C)
C   POWCDAT(IPWC,13): heatersRes, resistance of the heaters (Ohm)
C   POWCDAT(IPWC,14): batTEmer, emergency temperature below which the thermal management system has priority over the load (°C)

C ---------------------------------------------------------------
C   Free voltage is correlated as a function of DOD
C   FreeVoltage = (E0CoefA + E0CoefB*batDOD**E0ExpB +
C     &               E0CoefC*batDOD**E0ExpC +
C     &               E0CoefD*batDOD**E0ExpD)*nSerie
C
C   POWCDAT(IPWC,15): E0CoefA
C   POWCDAT(IPWC,16): E0CoefB
C   POWCDAT(IPWC,17): E0CoefC
C   POWCDAT(IPWC,18): E0CoefD
C   POWCDAT(IPWC,19): E0ExpB
C   POWCDAT(IPWC,20): E0ExpC
C   POWCDAT(IPWC,21): E0ExpD

C ---------------------------------------------------------------
C   Battery capacity is correlated as a function of temperature
C   BatteryCap = cellCapRef*nParal*
C     &             (capCoefA+capCoefB*batTemp+capCoefC*batTemp**2)
C
C   POWCDAT(IPWC,22): capCoefA
C   POWCDAT(IPWC,23): capCoefB
C   POWCDAT(IPWC,24): capCoefC

C ---------------------------------------------------------------
C   The internal resistance is correlated as a function of its temperature, the current,
C   the depth of discharge  and the number of battery cells connected in parallel
C
C   BatteryRInt = nSerie/nParal*(rIntCA + rIntCB*batDOD**rIntEB +
C     &    rIntCC*(batI/nParal/cellCapRef)**rIntEC +
C     &    rIntCD*batTemp**rIntED+rIntCH*batDOD**rIntEH1+
C     &    rIntCD*batTemp**rIntEH2*(batI/nParal/cellCapRef)**rIntEH3)
C
C   The above equation calculates the internal resistance for the whole battery.
C ---------------------------------------------------------------
C   Coefficients used to calculate the internal resistance while charging:
C
C   POWCDAT(IPWC,25): rIntCA
C   POWCDAT(IPWC,26): rIntCB
C   POWCDAT(IPWC,27): rIntCC
C   POWCDAT(IPWC,28): rIntCD
C   POWCDAT(IPWC,29): rIntCH
C   POWCDAT(IPWC,30): rIntEB
C   POWCDAT(IPWC,31): rIntEC
C   POWCDAT(IPWC,32): rIntED
C   POWCDAT(IPWC,33): rIntEH1
C   POWCDAT(IPWC,34): rIntEH2
C   POWCDAT(IPWC,35): rIntEH3
C ---------------------------------------------------------------
C   Coefficients used to calculate the internal resistance while discharging:
C
C   POWCDAT(IPWC,36): rIntCA
C   POWCDAT(IPWC,37): rIntCB
C   POWCDAT(IPWC,38): rIntCC
C   POWCDAT(IPWC,39): rIntCD
C   POWCDAT(IPWC,40): rIntCH
C   POWCDAT(IPWC,41): rIntEB
C   POWCDAT(IPWC,42): rIntEC
C   POWCDAT(IPWC,43): rIntED
C   POWCDAT(IPWC,44): rIntEH1
C   POWCDAT(IPWC,45): rIntEH2
C   POWCDAT(IPWC,46): rIntEH3
C ---------------------------------------------------------------

C   POWCDAT(IPWC,47): The index of the zone where the battery is located (-)

C   POWCDAT(IPWC,48): Maximum DOD of battery (-)
C   POWCDAT(IPWC,49): Minimum DOD of battery (-)

C   POWCDAT(IPWC,50): Switch indicating active thermal management (.EQ.1) (-)

C   POWCDAT(IPWC,51): Float life of battery (years)
C   POWCDAT(IPWC,52): Nr of cycles for cycle life (-)
C   POWCDAT(IPWC,53): DOD of cycles defining cycle life (-)
C   POWCDAT(IPWC,54): Abuse life (years)

C   POWCDAT(IPWC,55): Switch indicating active battery life control (.EQ.1) (-)

C   POWCDAT(IPWC,56): Mandatory charge interval (days)
C   POWCDAT(IPWC,57): Maximum duration mandatory charge cycle (hours)






C****************************************************
C List of inut parameters for lithium ion battery model (IPWC = 22)
C****************************************************
C---------------------------
C Lithium ion battery parameters
C---------------------------
C POWCDAT (IPWC,1):  nParal         Number of cells connected in parrallel
C POWCDAT (IPWC,2):  nSeries        Number of cells connected in series
C POWCDAT (IPWC,3):  cellCapRef     reference capacity for a unit cell (Amp h)
C POWCDAT (IPWC,4):  tempRef        battery reference operating temperature
C POWCDAT (IPWC,5):  maxVolt        DOD=0 (V) of a single cell
C POWCDAT (IPWC,6):  minVolt        DOD=100% (V) of a single cell
C POWCDAT (IPWC,7):  currentMax     Maximum charge/discharge current of one cell(Amps)
C POWCDAT (IPWC,8):  voltMax        Maximum charge/discharge voltage of one cell(V)
C POWCDAT (IPWC,9):  batTempInit    initial battery temperature  (�C)
C POWCDAT (IPWC,10): batinitDOD     initial depth of discharge (probably zero for a charged battery)
C POWCDAT (IPWC,11): thermalMass    thermal mass of a single cell -mass*Cp (J/°C)
C POWCDAT (IPWC,12): heatLossFact   heat loss factor h*A (W/°C)
C POWCDAT (IPWC,13): heatersRes     resistance of the heaters (Ohm)
C POWCDAT (IPWC,14): upperBoundDOD  maximum DOD of battery (in percentage... probably 0.8)
C POWCDAT (IPWC,15): lowerBoundDOD  minimum DOD of battery (in percentage... probably 0)
C POWCDAT (IPWC,16): tempMax        Maximum battery temperature (�C)
C POWCDAT (IPWC,17): tempMin        Minimum battery temperature (�C)
C---------------------------
C OCV voltage as a function of DOD of a single cell Vocv(DOD) = E5*DOD^5 + E4*DOD^4 + E3*DOD^3 + E2*DOD^2 + E1*DOD + E0
C---------------------------
C POWCDAT (IPWC,18): E5
C POWCDAT (IPWC,19): E4
C POWCDAT (IPWC,20): E3
C POWCDAT (IPWC,21): E2
C POWCDAT (IPWC,22): E1
C POWCDAT (IPWC,23): E0
C---------------------------
C Rint as a function of DOD at REFERENCE CURRENT of a single cell
C        Rint(DOD) = Rint5*DOD^5 +  Rint4*DOD^4 + Rint3*DOD^3 + Rint2*DOD^2 + Rint1*DOD + Rint0
C This is determined by polyfitting the difference between the OCV and Vref divided
C by the reference current using the OCV DOD scale (see Neil Saldanha's thesis for more info)
C---------------------------
C POWCDAT (IPWC,24):    Rint5
C POWCDAT (IPWC,25):    Rint4
C POWCDAT (IPWC,26):    Rint3
C POWCDAT (IPWC,27):    Rint2
C POWCDAT (IPWC,28):    Rint1
C POWCDAT (IPWC,29):    Rint0
C---------------------------
C current factors and alphas relative to OCV as described by Gao (2002) and in Neil Saldanha's master thesis
C---------------------------
C POWCDAT (IPWC,30):    c1  The first current discharged at (Amps)
C POWCDAT (IPWC,31):    alpha1  The alpha required to discharge/charge at first rate
C POWCDAT (IPWC,32):    c2  The second current discharged at (Amps)
C POWCDAT (IPWC,33):    alpha2  The alpha required to discharge/charge at second rate
C POWCDAT (IPWC,34):    c3  The third current discharged at (Amps)
C POWCDAT (IPWC,35):    alpha3  The alpha required to discharge/charge at third rate
C POWCDAT (IPWC,36):    c4  The fourth current discharged at (Amps)
C POWCDAT (IPWC,37):    alpha4  The alpha required to discharge/charge at fourth rate
C POWCDAT (IPWC,38):    c5  The fifth current discharged at (Amps)
C POWCDAT (IPWC,39):    alpha5  The alpha required to discharge/charge at fifth rate
C POWCDAT (IPWC,40):    c6  The sixth current discharged at (Amps)
C POWCDAT (IPWC,41):    alpha6  The alpha required to discharge/charge at sixth rate
C POWCDAT (IPWC,42):    c7  The seventh current discharged at (Amps)
C POWCDAT (IPWC,43):    alpha7  The alpha required to discharge/charge at seventh rate
C POWCDAT (IPWC,44):    c8  The eighth current discharged at (Amps)
C POWCDAT (IPWC,45):    alpha8  The alpha required to discharge/charge at eighth rate
C---------------------------
C temperature factors, betas and offsets relative to OCV as described by Gao (2002) and in Neil Saldanha's master thesis
C---------------------------
C POWCDAT (IPWC,46):    t1  The first temperature discharged/charged at in Celsius
C POWCDAT (IPWC,47):    beta1   The beta required to discharge/charge at the first temperature
C POWCDAT (IPWC,48):    offset1 The voltage offset required to discharge/charge at the first temperature (V)
C POWCDAT (IPWC,49):    t2  The second temperature discharged/charged at in Celsius
C POWCDAT (IPWC,50):    beta2   The beta required to discharge/charge at the second temperature
C POWCDAT (IPWC,51):    offset2 The voltage offset required to discharge/charge at the second temperature (V)
C POWCDAT (IPWC,52):    t3  The third temperature discharged/charged at in Celsius
C POWCDAT (IPWC,53):    beta3   The beta required to discharge/charge at the third temperature
C POWCDAT (IPWC,54):    offset3 The voltage offset required to discharge/charge at the third temperature (V)
C POWCDAT (IPWC,55):    t4  The fourth temperature discharged/charged at in Celsius
C POWCDAT (IPWC,56):    beta4   The beta required to discharge/charge at the fourth temperature
C POWCDAT (IPWC,57):    offset4 The voltage offset required to discharge/charge at the fourth temperature (V)
C POWCDAT (IPWC,58):    t5  The fifth temperature discharged/charged at in Celsius
C POWCDAT (IPWC,59):    beta5   The beta required to discharge/charge at the fifth temperature
C POWCDAT (IPWC,60):    offset5 The voltage offset required to discharge/charge at the fifth temperature (V)
C POWCDAT (IPWC,61):    t6  The sixth temperature discharged/charged at in Celsius
C POWCDAT (IPWC,62):    beta6   The beta required to discharge/charge at the sixth temperature
C POWCDAT (IPWC,63):    offset6 The voltage offset required to discharge/charge at the sixth temperature (V)
C POWCDAT (IPWC,64):    t7  The seventh temperature discharged/charged at in Celsius
C POWCDAT (IPWC,65):    beta7   The beta required to discharge/charge at the seventh temperature
C POWCDAT (IPWC,66):    offset7 The voltage offset required to discharge/charge at the seventh temperature (V)
C POWCDAT (IPWC,67):    t8  The eighth temperature discharged/charged at in Celsius
C POWCDAT (IPWC,68):    beta8   The beta required to discharge/charge at the eighth temperature
C POWCDAT (IPWC,69):    offset8 The voltage offset required to discharge/charge at the eighth temperature (V)
C---------------------------
C Extra parameters
C---------------------------
C POWCDAT (IPWC,70):    k_o self discharge constant in amps (keep zero if unknown or for zero self discharge)
C POWCDAT (IPWC,71):    E_as    Activation energy for self-discharge in kJ.kmol-1 (keep zero if unknown  or for zero self discharge)
C POWCDAT (IPWC,72):    iZoneIndex  The index of the zone where the battery is located (-)
C POWCDAT (IPWC,73):    pParasitic  The power required to run the li-on BMS circuitry (W)
C---------------------------
C Optional additional Rint leave 999. to override anywhere
C---------------------------
C POWCDAT (IPWC,74):    Rint5hi
C POWCDAT (IPWC,75):    Rint4hi
C POWCDAT (IPWC,76):    Rint3hi
C POWCDAT (IPWC,77):    Rint2hi
C POWCDAT (IPWC,78):    Rint1hi
C POWCDAT (IPWC,79):    Rint0hi
C POWCDAT (IPWC,80):    Iflag    Any current above this value will use these internal resistance values
C---------------------------
C Battery degradation
C---------------------------
C POWCDAT (IPWC,81):   Cycle_cruc   The crucial cycle to indicate the end of the cell's life
