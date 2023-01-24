C This file is part of the ESP-r system.
C
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

C-----------------------------------------------------------------------------------------
C This is a customized controller for purposes of modelling electrical nets featuring
C a battery and (typically) PV in models optionally also using explicit plants for HVAC.
C
C The structure of this controller is customized to quite specific system configurations.
C
C This controller is invoked from ENETSOLV at the beginning of the execution of the
C electric domain.  Consequently, it senses the state of electric domain components
C from the previous solution of the electric domain.  If bps is iterating within the
C time-step between the plant and electrical domains to converge a solution, then the
C controller is sensing the state of the electric domain components at the current
C time-step.  Otherwise, if this is the first pass through the electric domain for the
C current time-step then the controller is sensing the state of electric domain
C components at the previous time-step.
C
C Since the plant domain is solved prior to the invocation of the electric domain,
C the controller is sensing the state of plant components at the current time-step.
C
C If the controller actuates a plant domain component and such a control
C action results in a different operational mode for a plant component, a flag is set
C that forces another iteration through the plant and electric domains at the current
C time-step.
C
C This controller is adapted from src/cetc/RE-H2-ctl.F.
C
C For more information on the control strategies see
C   1.
C   X. Neil Saldanha, Carleton U's master thesis <<Li-Ion Battery model, mainly>>
C
C-----------------------------------------------------------------------------------------
C Subroutines
C
C  RES_elec_storage_ctl:   The main control routine. This routine is called in
C                          esrupfs/enetsolv.F, subroutine ENETSOLV and manages the control
C                          schemes available.
C
C  simple_ctl              Basic (simple) control
C
C  low_cost                Control based on algorithm to minimise cost. Reads time step
C                          data on electricity costs from an external file ...
C
C  thermal_follow          Control based on (currentl nothing implemented!)
C
C  RES_elec_ctl_read       Reads information required by RES_elec_storage_ctl
C
C  RES_elec_ctl_PostProcess() Writes output to h3k (currently not activated)
C-----------------------------------------------------------------------------------------
C Also contains the following subroutines for reading specific external data files:
C
C  CreateBinaryDataFile    Checks .fcl file and creates binary file with time series
C                          of data.
C  ReadBinaryDataFile      Retrieves the value for the current time-step.
C
C References:
C   esrupfs/powoc.F
C   cetc/FC_components.F and
C
C   are the basis for the subroutines defined herein
C
C-----------------------------------------------------------------------------------------
      SUBROUTINE RES_elec_storage_ctl
      IMPLICIT NONE
#include "building.h"
#include "plant.h"
#include "power.h"

      COMMON/OUTIN/IUOUT,IUIN,IEOUT
      INTEGER IUOUT           !-write unit number
      INTEGER IUIN            !-read unit number
      INTEGER IEOUT           !-error unit number

C ESP-r COMMONs
       common/simtim/ihrp,ihrf,idyp,idyf,idwp,idwf,nsinc,its,idynow
       INTEGER IHRP            !-hour of present time-step
       INTEGER IHRF            !-hour of future time-step
       INTEGER IDYP            !-year day number of present day
       INTEGER IDYF            !-year day number of future day
       INTEGER IDWP            !-day of the week of present day
       INTEGER IDWF            !-day of the week of future day
       INTEGER NSINC           !-number of building-side time increments
                               !-since start of simulation
       INTEGER ITS,idynow      !-current building time-step within
                               !-current hour

C Input file information
      common/reselecctl/iResElecCtl,ResElecFlnam
      integer ireselecctl       !- Flag indicating that file required by renewable energy
                                !- System controller is specified
      character*72 reselecflnam !- Name of file containing renewable energy system
                                !- Controller inputs

      common / reselec_ctl_status / bRESElec_ctl_unconverged
      logical bRESElec_ctl_unconverged         !-logical flag indicating additional iterations req'd

C Basic RES (elec) controller inputs
      common/reselec/RES_elec_ctl_mode, RES_elec_ctl_scenario, NumBat
      integer RES_elec_ctl_mode     ! control mode 1, 2 or 3 << Note: currently only 2 is coded >>
      integer RES_elec_ctl_scenario ! control scenario active
      integer NumBat                !- Number of batteries in system

C Local variables
      integer IPWC

      character*125 outstr

C-----------------------------------------------------------------------------------------
C     Check if controller is in use
C-----------------------------------------------------------------------------------------
      if ( iResElecCtl .ne. 1 ) then
C........Controller is not in use. Set flag preventing iteration
         bResElec_ctl_unconverged = .false.
      else ! closes at end of subroutine!
C........Controller is in use.

C      Count number of batteries ...
       if (NSINC .eq. 1) then
         NumBat=0

         do 5 IPWC=1,NPOWCOM

           IF( (POWCOMID(IPWC).EQ.pba) .or.  ! Lead-acid battery
     &         (POWCOMID(IPWC).EQ.vrb) .or.  ! VRB
     &         (POWCOMID(IPWC).EQ.lion)      ! Lithium-ion battery
     &       ) THEN
C            Battery found
             NumBat = NumBat +1
           ENDIF

5        continue
       endif

C <<   lion (and pba) battery behaviour very questionable for short time steps -
C      must a time step integration/lag occur for timesteps below a certain
C      threshold value ?? >>

        if (RES_elec_ctl_mode .eq. 1) then
          call simple_ctl          ! call up constant power draw
        elseif (RES_elec_ctl_mode .eq. 2) then
          call low_cost            ! call up low cost
        elseif (RES_elec_ctl_mode .eq. 3) then
          call thermal_follow      ! call up thermal following control
        else                       
          ! <<shouldn't be possible, i.e. should be caught during read of input file >>
C         Give warning and exit this subroutine
          write(outstr,'(2A)') 'Renewable energy system (elec)',
     &       ' undefined control mode was entered -- no control!'
          call edisp(iuout,outstr)
        endif

      endif ! iResElecCtl .ne.1

      END ! End main subroutine


C===================================================================================
C=============================== simple control mode ===============================
C This routine applies a power draw according to data from a file.
C
C Options:
C 1) battery/ies is/are connected with a POWOC of type 12, data is read
C    from ".fcl" file with a maximum resolution of 12 entries per hour
C    (5-min-data); load is evenly distributed across batteries in system.
C
C 2) battery/ies is/are connected with a POWOC of type 24, data is read
C    from TDF file with a maximum resolution according to TDF specs
C    (1-min-data tested); load is evenly distributed across batteries
C    in system.
C
C This control mode is meant mostly for testing / debugging purposes.
C
C Author: A. Geissler
C===================================================================================
      SUBROUTINE simple_ctl
      IMPLICIT NONE
#include "building.h"
#include "plant.h"
#include "power.h"
#include "BATTERY.h"

C Basic RES (elec) controller inputs
      common/reselec/RES_elec_ctl_mode, RES_elec_ctl_scenario, NumBat
      integer RES_elec_ctl_mode     ! control mode 1, 2 or 3 << Note: currently only 2 is coded >>
      integer RES_elec_ctl_scenario ! control scenario active
      integer NumBat                !- Number of batteries in system

C External load determined in subroutine POWOC_Outside_ld
C (Power only component type 12)
      common/Res_elec_ctl/house_load
      real     house_load ! The house load in (W)

C Local variables
      integer IPWC,IPWCLD,INODE

C     loop through all power only components
      do 10 IPWC=1,NPOWCOM

        IF( (POWCOMID(IPWC).EQ.pba) .or.  ! Lead-acid battery
     &      (POWCOMID(IPWC).EQ.vrb) .or.  ! VRB
     &      (POWCOMID(IPWC).EQ.lion)      ! Lithium-ion battery
     &    ) THEN
C         We have a battery!
C         +ve indicates discharging the battery, -ve charge
C         'house_load' and PPOWOC() are negative!

C         Again loop through all power only components to look for
C         which POWOC is connected to the same node as the battery
          do 15 IPWCLD=1,NPOWCOM
            do 16 INODE=1,3
              if(POWCONEN(IPWCLD,INODE).EQ.POWCONEN(IPWC,INODE))then
C               Current POWOC is connected to battery
                if (POWCOMID(IPWCLD) .EQ. 12) then
C                 load is being read from .fcl file
C                 'house_load' is negative! Split load evenly between
C                 batteries in system
                  batDemandP(IPWC) = -house_load/real(NumBat)

                elseif (POWCOMID(IPWCLD) .eq. 24) then
C                 Load is being read from TDF file
C                 'PPOWOC()' is negative! Split load evenly between
C                 batteries in system
                  batDemandP(IPWC) = -PPOWOC(IPWCLD)/real(NumBat)
                endif
              endif
   16       continue ! loop through phases
   15     continue ! loop through POWOCs

        ENDIF

   10 continue

      END ! End simple control algorithm


C===================================================================================
C============================ low cost control mode ================================
C
C This routine applies a low cost control strategy on the system
C renewable energy source in electrical network including PV, battery and
C others.
C
C Author: A. Geissler
C===================================================================================
      SUBROUTINE low_cost
      IMPLICIT NONE

#include "building.h"
#include "plant.h"
#include "power.h"
#include "BATTERY.h"
#include "CETC_definitions.h"

C Common block holding information on the battery's current operation.
      COMMON/battery_op_data/maxPBatCharge_common(MPOWCOM),
     &   maxPBatDischarge_common(MPOWCOM), RE_H2_control_scenario
      REAL maxPBatCharge_common    ! Maximum charge rate of battery (W) as determined
                                   ! for mode of operation at previous calulcation (charge or discharge).
      REAL maxPBatDischarge_common ! Maximum discharge rate of battery (W) as determined
                                   ! for mode of operation at previous calulcation (charge or discharge).
      INTEGER RE_H2_control_scenario ! Param indicating which control scenario is chosen


      common / reselec_ctl_status / bRESElec_ctl_unconverged
      logical bRESElec_ctl_unconverged  !-logical flag indicating additional iterations req'd

C Basic RES (elec) controller inputs
      common/reselec/RES_elec_ctl_mode, RES_elec_ctl_scenario, NumBat
      integer RES_elec_ctl_mode     ! control mode 1, 2 or 3 << Note: currently only 2 is coded >>
      integer RES_elec_ctl_scenario ! control scenario active
      integer NumBat                !- Number of batteries in system

C Various RES (elec) controller inputs
      common/reselec_setpts/SOC_PV_ON,SOC_PV_OFF,
     &   SOC_grid_ON,SOC_grid_OFF,fPV_deadband
      real      SOC_PV_ON           !-battery SOC at which PV is used
      real      SOC_PV_OFF          !-battery SOC at which PV use is ended
      real      SOC_grid_ON         !-battery SOC at which grid is used
      real      SOC_grid_OFF        !-battery SOC at which grid use is ended
      real      fPV_deadband        !-value is applied so that PV is used for battery charge
                                    !-only when the excess PV power is greater than power to meet
                                    !-the loads + power allowance for the heat pump

C Common for grid rate and grid refund data management
      common/GridFinancial/iGridRateToggle,iGridRefundToggle,
     &   GridRateThreshold,GridRefundThreshold,
     &   iFIL_GridRate,iFIL_GridRefund
      integer   iGridRateToggle     !
      integer   iGridRefundToggle   !
      real      GridRateThreshold   !
      real      GridRefundThreshold !
      integer   iFIL_GridRate       !
      integer   iFIL_GridRefund     !

C RES (elec) node information
      common/reselec_enode/IENOD_ACbus,IENOD_DCbus

      integer IENOD_ACbus,IENOD_DCbus

C Power-only component information for RES (elec) control
      common/reselec_powoc/IPWC_PVPCU,IPWC_gridconn,IPWC_ACDC,
     &                   IPWC_battery
      integer   IPWC_PVPCU      !-index number of PCU that is connected between the PV and the DC bus
      integer   IPWC_gridconn   !-index number of the grid-connection power-only component
      integer   IPWC_ACDC       !-index number of the DC-AC converter (a PCU power-only component)
      integer   IPWC_battery    !-index number of the battery power-only component

C Controller flag information
      common/RESElec_ctl_BatStat/bPV_active,bBattery_charge,
     &                    bBattery_discharge
      logical bPV_active,bBattery_charge,bBattery_discharge

C@@@@@@@@
C=== RESIDUAL from original controller ... probably obsolete:
C House non hvac load determined in subroutine POWOC_nonHVACld
      common/Res_elec_ctl/house_load
      real     house_load ! The house load in (W)

C---- local variables ---------------------------------------------------
      real hvacl
      real occupantl
      real P_total_load       ! sum of all loads this time step, W
      real P_available        ! power available this time step (PV - load)

      real P_residual         ! difference between PV and P_total_load
      real batDODMax          ! Maximum depth of discharge
      real fBatDemandP_old    !
      real fGridRate
      real fGridRefund

      REAL PV_power_to_DCbus  ! PV-produced power that is added to DC bus (W).
      REAL load_ACbus         ! Power load on AC bus (W).
      REAL DCAC_eff           ! Efficiency of DC-AC converter.
      REAL power_DC_to_AC_required ! Power required to DC-AC converter to meet load on AC bus (W).

CCC   ... these two need to be replaced by appropriate variables from
CCC       the electrical net ...
      real export_to_grid
      real import_from_grid

      integer iComponentType,iHyb,iIndex,iLoadIndex

      logical bBatteryFull    ! flag .true. if battery fully charged;
      logical bBatteryEmpty   ! flag .false. if battery has available capacity for discharge

      logical bGridRefundHigh ! flag .true. if exporting to grid has 'high rate'
      logical bGridRateLow    ! flag .true. if importing from grid is 'cheap'

      logical bBat_same_signal

      logical DEBUG

C---- function definitions ----------------------------------------------

      real elec_net_load_calc   ! miscellaneous function that retuns data
                                ! from electrical network depending on the
                                ! integer parameter passed to it.

C ---
      DEBUG = .false.          ! toggle debug mode

C---- Initiate variables according to battery type ----------------------

      IF(POWCOMID(IPWC_battery) .EQ. pba) THEN     ! Lead-acid battery
        batDODMax = POWCDAT(IPWC_battery,48)
      ELSEIF(POWCOMID(IPWC_battery) .EQ. vrb) THEN  ! VRB
cx        batDODMax = ??
      ELSEIF(POWCOMID(IPWC_battery) .EQ. lion) THEN ! Lithium-ion battery
        batDODMax = POWCDAT(IPWC_battery,14)
      ENDIF

C     Depth of discharge '0' with tolerance 0.005 (0.5%) is viewed as "full"
      call eclose(batDOD(IPWC_battery),0.0,0.005, bBatteryFull)

C     Check against depth of discharge 'batDODMax', for which battery is
C     considered "empty"
      call eclose(batDOD(IPWC_battery),batDODMax,0.01,bBatteryEmpty)

C----------------------------------------------------------------------------------------
C     Save battery data from last call
C----------------------------------------------------------------------------------------

      fBatDemandP_old = batDemandP(IPWC_battery)

C-----------------------------------------------------------------------------------------
C Sense power generation from PCU that is connected between PV and DC bus.  This
C is the PV-produced power that is added to the DC bus.  This is the value at the
C previous iteration through the electric domain (perhaps the same time-step).
C IPWC_PVPCU is the index number of the PCU powoc (i.e. the sensed component in the
C electrical domain).
C-----------------------------------------------------------------------------------------

cx    << how to consider 3phase PV setup?? >>
      PV_power_to_DCbus = fPow_Time_Row_Future(IPWC_PVPCU,1)

C@@@@@@@ EXTRACT FROM H2_CTL /=/ Useful? Necessary? Correct?

C-----------------------------------------------------------------------------------------
C Sense the loading on the AC bus from the previous iteration.
C IENOD_ACbus is the node number of the AC bus.
C The load or generation of the grid-connection powoc model should be excluded.
C IPWC_gridconn is the index number of the grid-connection powoc.
C
C Note: the function Elec_Net_Load_Calc cannot be used to evaluate the load
C on the AC bus because this function sums the loads and generation
C for all powoc and hybrid components on the entire electrical network.
C
C << is this a) correct (IMO, it depends on the call) and b) relevant here? >>
C-----------------------------------------------------------------------------------------

      load_ACbus = ABS(PENODL(IENOD_ACbus))
C                                - PPOWOC(IPWC_gridconn) ! - for load; UNCOMMENT WHEN GRID CONNECTED COMPONENT ADDED

C-----------------------------------------------------------------------------------------
C Determine how much power was required (at the previous iteration) to be supplied
C by the DC bus to the AC-DC converter to meet the load on the AC bus.
C
C This is not necessarily how much power flowed along this path because there may
C have been grid interaction on the AC bus.
C
C IPWC_ACDC is the index number of the AC-DC converter PCU powoc.
C-----------------------------------------------------------------------------------------

      DCAC_eff = fPow_Time_Row_Future(IPWC_ACDC,2)    ! Eff of DC-AC converter.

      if ( DCAC_eff .eq. 0. ) then
        DCAC_eff = 1.   ! avoid divide by zero error if DC/AC inverter is idle
      endif

      power_DC_to_AC_required = load_ACbus / DCAC_eff

C@@@@@@@ END EXTRACT

C-----
C     Retrieve current values for grid rate
      if (iGridRateToggle .eq. 0) then
        bGridRateLow = .false.
      elseif (iGridRateToggle .eq. 1) then
        bGridRateLow = .true.
      else ! if ( eq. 2) ... << explicit case for error catching? >>
C       Read current value from file
        call ReadBinaryDataFile(iFIL_GridRate,fGridRate)
        if (fGridRate .gt. GridRateThreshold) then
          bGridRateLow = .false.
        else
          bGridRateLow = .true.
        endif
      endif

C     Retrieve current values for grid refund
      if (iGridRefundToggle .eq. 0) then
        bGridRefundHigh = .false.    
      elseif (iGridRefundToggle .eq. 1) then
        bGridRefundHigh = .true.
      else
C       Read current value from file
        call ReadBinaryDataFile(iFIL_GridRefund,fGridRefund)
        if (fGridRefund .lt. GridRefundThreshold) then
          bGridRefundHigh = .false.
        else
          bGridRefundHigh = .true.
        endif
      endif


C---- Overall building electrical loads this time step (W) --------------
C     Logic from elec_net_load_calc (duplicated here because for some
C     reason occupant load results were always zero!)
      hvacl = 0.
      occupantl = 0.

      DO iHyb=1,nHYBcom
C........Determine the type of component.
         iComponentType = HYCOMTYP(iHyb)
C........Get required component index values.
         iIndex = HYLOC(iHyb,1)
         iLoadIndex = HYLOC(iHyb,2)

         IF ( iComponentType .eq. 1 ) THEN
C........Zone casual gains (ESRU coupling)
           occupantl = occupantl + PEZON( iIndex, iLoadIndex ) ! (W)
         ELSEIF ( iComponentType .eq. 3 ) THEN
C..............Plant component.
           if (PPCMP(iIndex) .lt.0) then
               hvacl = hvacl + PPCMP( iIndex )
           endif
         endif
      end do

      hvacl = abs(hvacl)
      occupantl = abs(occupantl) ! here, load is .gt. zero

cx      hvacl = elec_net_load_calc(hvac_load)
cx      occupantl = elec_net_load_calc(occupant_load)

      P_total_load = hvacl + occupantl

C--- Begin control logic

      if (PV_power_to_DCbus .gt. 0) then
C     PV generation available (W)
        if (PV_power_to_DCbus .ge. P_total_load) then

          P_available = PV_power_to_DCbus - P_total_load ! (W)

          batDemandP = 0.0

C         Is battery fully charged or can it take some additional charging?
          if (bBatteryFull) then
            RES_elec_ctl_scenario = 1
            export_to_grid = P_available

          else
            if (bGridRefundHigh) then
              RES_elec_ctl_scenario = 2
C             Refund rate for export to grid is high
              export_to_grid = P_available

            else
C             Refund for export to grid is low

              if (P_available .le.
     &                        maxPBatCharge_common(IPWC_battery)) then
                RES_elec_ctl_scenario = 3
C               Charge battery with all available power
                batDemandP = -1.0*P_available

              else
                RES_elec_ctl_scenario = 4
C               Charge battery until full, then export excess energy
C               available to grid (-ve is charge)
                batDemandP = -1.0*maxPBatCharge_common(IPWC_battery)
                export_to_grid = P_available
     &                          - maxPBatCharge_common(IPWC_battery)

              endif ! available .le. capacity
            endif ! grid_refund_high
          endif ! battery_full

        else
C       PV can not cover full load

          P_residual = P_total_load - PV_power_to_DCbus ! (W)

C         Does battery have available capacity for discharge?
          if ( .not. bBatteryEmpty ) then
C           Is electricity from grid cheap?
            if (bGridRateLow) then
              RES_elec_ctl_scenario = 5
C             Grid electricity is cheap, import everything not covered by PV
C             from grid
              import_from_grid = P_residual ! (W)

            else
C             Electricity from grid is expensive!
              if (P_residual .le.
     &                      maxPBatDischarge_common(IPWC_battery)) then
                RES_elec_ctl_scenario = 6
C               Available battery capacity can cover load in this time step
C               grid electricity is expensive, so use battery to cover 
C               residual load
                batDemandP = P_residual
              else
                RES_elec_ctl_scenario = 7
C               Grid electricity is expensive, use battery as far as it goes,
C               then import remaining energy required from grid
                batDemandP = maxPBatDischarge_common(IPWC_battery)
                import_from_grid = P_residual
     &                        - maxPBatDischarge_common(IPWC_battery)
              endif ! residual .le. capacity
            endif ! grid_rate_low

          else
            RES_elec_ctl_scenario = 8
C           Battery is empty, import residual load from grid
            batDemandP = 0.0
            import_from_grid = P_residual
          endif ! .not.BatteryEmpty
        endif ! PV .ge. E_ges

      else
C     No PV output available (night time)

        if ( .not. bBatteryEmpty ) then
C         Is import of electricity from grid cheap?
          if (bGridRateLow) then
            RES_elec_ctl_scenario = 9
C           Grid electricity is cheap, so use grid
            import_from_grid = P_total_load

          else

            if (P_total_load .le.
     &                  maxPBatDischarge_common(IPWC_battery)) then
              RES_elec_ctl_scenario = 10
C             Grid electricity is expensive, so use battery
              batDemandP = P_total_load

            else
              RES_elec_ctl_scenario = 11
C             Grid electricity is expensive, so discharge battery until empty,
C             then import remaining energy required from grid
              batDemandP = maxPBatDischarge_common(IPWC_battery)
cx              import_from_grid = P_total_load - maxPBatDischarge_common(IPWC_battery)

            endif ! E_ges .lt. capacity
          endif ! grid_rate_low

        else 
          RES_elec_ctl_scenario = 12
C         Battery is empty, import from grid
          batDemandP = 0.0
          import_from_grid = P_total_load

        endif ! .not.bBatteryEmpty
      endif ! PV .gt. 0

C*** DEBUG ===
      if (DEBUG) then
        write(*,*)'hvacl=',hvacl,
     &     ', occuptl=',occupantl,
     &     ', PVgen=',PV_power_to_DCbus,
c     &     ', BatFull ?',bBatteryFull,
c     &     ', BatEmpty?',bBatteryEmpty,
     &     ', batDOD(',IPWC_battery,')=',batDOD(IPWC_battery),
     &     ', Ctl-scen=',RES_elec_ctl_scenario,
     &     ', batDemandP(',IPWC_battery,')=',batDemandP(IPWC_battery)
      endif
C ====

C--- End control logic

C Check if plant domain controls have been adjusted. As battery can
C cover full load and plant loads (e.g. heat pump) tend to be dominating
C loads, this seems a plausible precaution
      call eclose ( fBatDemandP_old,
     &              batDemandP(IPWC_battery),
     &              2.0,                            ! 2W tolerance
     &              bBat_same_signal )

      if ( bBat_same_signal ) then
C       Plt controls have not changed, and additional interdomain iteration is unnecessary
        bRESElec_ctl_unconverged = .false.
      else
C       Plt controls have changed, and additional iteration is required.
Cx        bRESElec_ctl_unconverged = .true.
        bRESElec_ctl_unconverged = .false.
      endif

      END ! low cost control mode


C===================================================================================
C========================== thermal follow control mode ============================
C fill me in
C===================================================================================
      SUBROUTINE thermal_follow
      IMPLICIT NONE

c #include "building.h"
c #include "plant.h"
c #include "power.h"
c #include "BATTERY.h"

C Common declarations
      common/outin/iuout,iuin,ieout
      integer   iuout           !- write unit number
      integer   iuin            !- read unit number
      integer   ieout           !- error unit number

C Local variables
      character outstr*124            !- string containing error message

C This control mode has no functionality. Warn user.
        write(outstr,'(2A)') 'Renewable energy system (elec)',
     &        ' control mode 3 was entered -- *no* functionality!'
        call edisp(iuout,outstr)

      END ! thermal follow


C===================================================================================
C============================ RES_elec_ctl_read ====================================
C
C This subroutine reads the information required by the renewable energy
C systems (elec) controller (subroutine ??? above) and saves the information in
C the appropriate COMMON blocks.
C
C Based on RESH2_Ctl_Read
C===================================================================================
      SUBROUTINE RES_elec_ctl_read
      IMPLICIT NONE
#include "building.h"
#include "plant.h"
#include "power.h"
#include "BATTERY.h"
#include "BinaryDataFile.h"

C-----------------------------------------------------------------------------------------
C COMMON declarations

      common/outin/iuout,iuin,ieout
      integer   iuout           !- write unit number
      integer   iuin            !- read unit number
      integer   ieout           !- error unit number

      common/filep/ifil
      integer   ifil            !- basic file unit number

C Input file information
      common/reselecctl/ireselecctl,reselecflnam
      integer ireselecctl       !- flag indicating that file required by renewable energy
                                !- system controller is specified
      character*72 reselecflnam !- name of file containing renewable energy system
                                !- controller inputs

C Basic RES (elec) controller inputs
      common/reselec/RES_elec_ctl_mode, RES_elec_ctl_scenario, NumBat
      integer RES_elec_ctl_mode     ! control mode 1, 2 or 3 << Note: currently only 2 is coded >>
      integer RES_elec_ctl_scenario ! control scenario active
      integer NumBat                !- Number of batteries in system

C Power-only component information for RES (elec) control
      common/reselec_powoc/IPWC_PVPCU,IPWC_gridconn,IPWC_ACDC,
     &                   IPWC_battery
      integer   IPWC_PVPCU      !-index number of PCU that is connected between the PV and the DC bus
      integer   IPWC_gridconn   !-index number of the grid-connection power-only component
      integer   IPWC_ACDC       !-index number of the DC-AC converter (a PCU power-only component)
      integer   IPWC_battery    !-index number of the battery power-only component

C Plant component information for RES (elec) control
      common/reselec_plt/IPCOMP_heat_pump
      integer   IPCOMP_heat_pump  !-index number of the heat pump component

C Electric node information for RES (elec) control
      common/reselec_enode/IENOD_ACbus,IENOD_DCbus
      INTEGER IENOD_ACbus    ! Elec domain node number of AC bus.
      INTEGER IENOD_DCbus    ! Elec domain Node number of DC bus

C Various RES (elec) controller inputs
      common/reselec_setpts/SOC_PV_ON,SOC_PV_OFF,
     &   SOC_grid_ON,SOC_grid_OFF,fPV_deadband
      real      SOC_PV_ON           !-battery SOC at which PV is used
      real      SOC_PV_OFF          !-battery SOC at which PV use is ended
      real      SOC_grid_ON         !-battery SOC at which grid is used
      real      SOC_grid_OFF        !-battery SOC at which grid use is ended
      real      fPV_deadband        !-value is applied so that PV is used for battery charge
                                    !-only when the excess PV power is greater than power to meet
                                    !-the loads + power allowance for the heat pump

C Commons for grid rate and grid refund data management
      common/GridFinancial/iGridRateToggle,iGridRefundToggle,
     &   GridRateThreshold,GridRefundThreshold,
     &   iFIL_GridRate,iFIL_GridRefund
      integer   iGridRateToggle     !
      integer   iGridRefundToggle   !
      real      GridRateThreshold   !
      real      GridRefundThreshold !
      integer   iFIL_GridRate       !
      integer   iFIL_GridRefund     !

C-----------------------------------------------------------------------------------------
C Type declarations for local variables.
C-----------------------------------------------------------------------------------------

      character outstr*124            !- string containing error message
      character cErrLoc*32            !- string for error location information

      integer   ireselecfl_ascii      !- input file unit number
      integer   istat, ier            !- error flags
      integer   nd, k

      logical   bFileError            !- boolean signalling file error
      logical   xst                   !- boolean to signal that input file exists
      logical   bBatteryNew,bLegacyModel

      real      SOC_MAX               !- maximum state of charge of battery
      real      SOC_MIN               !- minimum state of charge of battery
      real      cycles_used           !- cycles LiIon battery has already used, for reading from
                                      !- file, used only with legacy ageing model

      parameter( SOC_MAX = 100.,
     &           SOC_MIN = 10. )

      integer   lnblnk                ! function returning non-blank string length

C-----------------------------------------------------------------------------------------
C Ensure that renewable energy system (elec) controller file exists.
C-----------------------------------------------------------------------------------------

      bFileError = .false.

      call FINDFIL(reselecflnam,xst)
      if ( .not. xst ) then
C filename is blank or unknown
        write(outstr,'(3A)') 'Renewable energy sys ctl file ',
     &                            reselecflnam(1:lnblnk(reselecflnam)),
     &                            ' was referenced but not found.'
        call edisp(iuout,outstr)
        STOP 'Need renewable energy sys ctl file: unresolvable error.'
      endif

C-----------------------------------------------------------------------------------------
C Read input file of format (example file):
C
C # Comments are denoted by the number sign and are ignored.
C # This file contains inputs required by the renewable energy systems
C # (elec) controller (subroutine RES_elec_ctl).
C # Inputs required by controller in the following order:
C 2                # (0)  control mode (1, 2 or 3) << only '1' & '2' usable >>
C 3                # (1)  node number of the AC bus
C 3                # (2)  node number of the DC bus
C 2                # (3)  index number of the battery power-only component
C 3                # (4)  index number of PCU that is connected between the PV and the DC bus
C 6                # (5)  index number of the DC-AC converter (a PCU power-only component)
C 5                # (6)  index number of the grid-connection power-only component
C 25               # (7)  battery SOC at which PV is used for charging
C 100              # (8)  battery SOC at which PV use for charging is ended
C 25               # (9)  battery SOC at which grid is used for charging
C 100              # (10) battery SOC at which grid use for charging is ended
C 400.             # (11) controller deadband for PV battery charge (W)
C 3000.            # (12) battery cycles already used at simulation start (legacy ageing model, only)
C 0                # (13)  grid rate toggle, 0/1 grid rate low false/true, 2 read file
C 0.16             # (13a) grid rate "low" threshold if (13) is set to "2"
C ./gridrate.fcl   # (13b) grid rate file if (13) is set to "2"
C 0                # (14)  grid refund toggle, 0/1 grid refund high false/true, 2 read file
C 0.22             # (14a) grid refund "high" threshold if (14) is set to "2"
C ./gridrefund.fcl # (14b) grid refund file if (14) is set to "2"
C-----------------------------------------------------------------------------------------

C Notify user that renewable energy system (elec) control file is being processed

      call usrmsg( ' Reading RES elec control file ... ',
     &             ' ','P')

C Open the input file
      ireselecfl_ascii = ifil + 41       !- file unit number, should be safe to use

      call erpfree(ireselecfl_ascii, istat)             !- closes file

      call efopseq(ireselecfl_ascii,reselecflnam,1,ier) !- opens a sequential ASCII file

      if ( ier .ne. 0 ) then
         write(iuout,*) 'Error opening renewabel energy sys ctl file. '
         bFileError = .true.
      endif

C     (0)
C     Read the control mode.
      call stripc(ireselecfl_ascii,outstr,0,nd,1,'Control mode',ier)
      if ( ier .ne. 0 ) then
         write(iuout,*) 'File read error reading control mode.'
         bFileError = .true.
         write(cErrLoc,'(A)')'file control mode'
      endif
      k = 0
      call egetwi(outstr,k,RES_elec_ctl_mode,1,3,'W','Control mode',ier)
      if ( ier .ne. 0 ) then
         write(iuout,*) 'Error reading control mode.'
         bFileError = .true.
         write(cErrLoc,'(A)')'control mode'
      endif

C     Read control mode dependant inputs ...

      !-------------------------------!
      ! Mode 1                        !
      !-------------------------------!
      if (RES_elec_ctl_mode .eq. 1) then
C       << no further action necessary, to date >>

CC << multiple battery system ?? add some input here?? >>

      !-------------------------------!
      ! Mode 2                        !
      !-------------------------------!
      elseif (RES_elec_ctl_mode .eq. 2) then

C << count number of lines to follow to check for file integrity??
C    Would be good, but difficult due to variable file length allowed for ... >>

C     (1)
C     Read the node number of the AC bus.
      call stripc(ireselecfl_ascii,outstr,0,nd,1,'AC bus node',ier)
      if ( ier .ne. 0 ) then
         write(iuout,*) 'File read error reading AC bus node number.'
         bFileError = .true.
      endif
      k = 0
      call egetwi(outstr,k,IENOD_ACbus,1,MENOD,'W','AC bus node',ier)
      if ( ier .ne. 0 ) then
         write(iuout,*) 'Error reading AC bus node number.'
         bFileError = .true.
      endif

C     (2)
C     Read the node number of the DC bus.
      call stripc(ireselecfl_ascii,outstr,0,nd,1,'DC bus node',ier)
      if ( ier .ne. 0 ) then
         write(iuout,*) 'File read error reading DC bus node number.'
         bFileError = .true.
      endif
      k = 0
      call egetwi(outstr,k,IENOD_DCbus,1,MENOD,'W','DC bus node',ier)
      if ( ier .ne. 0 ) then
         write(iuout,*) 'Error reading DC bus node number.'
         bFileError = .true.
      endif

C     (3)
C     Read the index number of the battery power-only component.
      call stripc(ireselecfl_ascii,outstr,0,nd,1,'battery',ier)
      if ( ier .ne. 0 ) then
         write(iuout,*) 'File read error reading battery index number.'
         bFileError = .true.
      endif
      k = 0
      call egetwi(outstr,k,IPWC_battery,1,MPOWCOM,'W','battery',ier)
      if ( ier .ne. 0 ) then
         write(iuout,*) 'Error reading battery index number. '
         bFileError = .true.
      endif

C     (4)
C     Read the index number of the PCU that is connected between the PV and the DC bus.
      call stripc(ireselecfl_ascii,outstr,0,nd,1,'pvpcu',ier)
      if ( ier .ne. 0 ) then
         write(iuout,*) 'File read error reading PV PCU index number . '
         bFileError = .true.
      endif
      k = 0
      call egetwi(outstr,k,IPWC_PVPCU,1,MPOWCOM,'W','pvpcu',ier)
      if ( ier .ne. 0 ) then
         write(iuout,*) 'Error reading PV PCU index number. '
         bFileError = .true.
      endif

C     (5)
C     Read the index number of the DC-AC converter.
      call stripc(ireselecfl_ascii,outstr,0,nd,1,'DC-AC pcu',ier)
      if ( ier .ne. 0 ) then
         write(iuout,*) 'File read error reading DC-AC converter',
     &       ' index number . '
         bFileError = .true.
      endif
      k = 0
      call egetwi(outstr,k,IPWC_ACDC,1,MPOWCOM,'W','DC-AC pcu',ier)
      if ( ier .ne. 0 ) then
         write(iuout,*) 'Error reading DC-AC converter index number. '
         bFileError = .true.
      endif

C     (6)
C     Read the index number of the grid-connection power-only component.
      call stripc(ireselecfl_ascii,outstr,0,nd,1,'grid conn',ier)
      if ( ier .ne. 0 ) then
         write(iuout,*)'File read error reading grid conn index number.'
         bFileError = .true.
      endif
      k = 0
      call egetwi(outstr,k,IPWC_gridconn,1,MPOWCOM,'W','grid conn',ier)
      if ( ier .ne. 0 ) then
         write(iuout,*) 'Error reading grid conn index number. '
         bFileError = .true.
      endif

C     (7)
C     Read the battery SOC at which the PV is used.
      call stripc(ireselecfl_ascii,outstr,0,nd,1,'SOC PV on',ier)
      if ( ier .ne. 0 ) then
         write(iuout,*) 'File read error reading SOC for PV on.'
         bFileError = .true.
      endif
      k = 0
      call egetwr(outstr,k,SOC_PV_ON,SOC_MIN,SOC_MAX,
     &            'W','soc elec on',ier)
      if ( ier .ne. 0 ) then
         write(iuout,*) 'Error reading SOC for PV on. '
         bFileError = .true.
      endif

C     (8)
C     Read the battery SOC at which PV use is ended.
      call stripc(ireselecfl_ascii,outstr,0,nd,1,'SOC PV off',ier)
      if ( ier .ne. 0 ) then
         write(iuout,*) 'File read error reading SOC for PV off. '
         bFileError = .true.
      endif
      k = 0
      call egetwr(outstr,k,SOC_PV_OFF,SOC_MIN,SOC_MAX,
     &            'W','soc elec off',ier)
      if ( ier .ne. 0 ) then
         write(iuout,*) 'Error reading SOC for PV off. '
         bFileError = .true.
      endif

C     (9)
C     Read the battery SOC at which the grid is used.
      call stripc(ireselecfl_ascii,outstr,0,nd,1,'File SOC grid on',ier)
      if ( ier .ne. 0 ) then
         write(iuout,*) 'File read error reading SOC for grid on. '
         bFileError = .true.
      endif
      k = 0
      call egetwr(outstr,k,SOC_grid_ON,SOC_MIN,SOC_MAX,
     &            'W','soc pem on',ier)
      if ( ier .ne. 0 ) then
         write(iuout,*) 'Error reading SOC for grid on. '
         bFileError = .true.
      endif

C     (10)
C     Read the battery SOC at which the grid use ends.
      call stripc(ireselecfl_ascii,outstr,0,nd,1,
     &                                         'File SOC grid off',ier)
      if ( ier .ne. 0 ) then
         write(iuout,*) 'File read error reading SOC for grid off.'
         bFileError = .true.
      endif
      k = 0
      call egetwr(outstr,k,SOC_grid_OFF,SOC_MIN,SOC_MAX,
     &            'W','SOC grid off',ier)
      if ( ier .ne. 0 ) then
         write(iuout,*) 'Error reading SOC for grid off. '
         bFileError = .true.
         write(cErrLoc,'(A)')'SOC grid off'
      endif

C     (11)
C     Read the controller deadband for PV battery loading
      call stripc(ireselecfl_ascii,outstr,0,nd,1,
     &                                  'file power deadband',ier)
      if ( ier .ne. 0 ) then
        write(iuout,*) 'File read error reading deadband for PV bat ld.'
        bFileError = .true.
      endif
      k = 0
      call egetwr(outstr,k,fPV_deadband,0.,1000.,
     &            'W','power deadband',ier)
      if ( ier .ne. 0 ) then
        write(iuout,*) 'Error reading deadband for PV bat ld.'
        bFileError = .true.
        write(cErrLoc,'(A)')'power deadband'
      endif

C     (12)
C     Read the number of cycles the battery has already used (this is
C     mainly for 2nd life usage ...

      call stripc(ireselecfl_ascii,outstr,0,nd,1,
     &                                  'file cycles used',ier)
      if ( ier .ne. 0 ) then
        write(iuout,*) 'File read error reading bat. cycles used'
        bFileError = .true.
      endif
      k = 0
      call egetwr(outstr,k,cycles_used,0.,50000.,
     &            'W','cycles used',ier)
      if ( ier .ne. 0 ) then
        write(iuout,*) 'Error reading bat. cycles used'
        bFileError = .true.
        write(cErrLoc,'(A)')'cycles used'
      endif
C     Set cycles used only if legacy ageing model is active
      call eclose(POWCDAT(IPWC_battery,82),0.0,0.01,bLegacyModel)
      if (bLegacyModel) then
        cycles_used_Lion(IPWC_battery) = cycles_used
      else
        ! do nothing, here
      endif
    ! Check if cycles used equal zero and set abuseFlag accordingly
    ! (avoid resetting of cycles_used_Lion in InitSimulation()
    ! of CETC_BATTERY.F (line 595 ff.))
      call eclose(cycles_used_Lion(IPWC_battery),0.0,0.1,bBatteryNew)
      if (bBatteryNew) then
        abuseFlag=0
      else
        abuseFlag=1
cx        write(*,*)"** abuseFlag=",abuseFlag
      endif

C     (13)
C     Read the grid rate toggle
C       0 ... bGridRateLow is set .false.
C       1 ... bGridRateLow is set .true.
C       2 ... grid rate is read from file, two additional lines are
C             read:
C             1) GridRateThreshold value (real number) and
C             2) valid file name with grid rate values
      call stripc(ireselecfl_ascii,outstr,0,nd,1,
     &                              'grid rate toggle stripc',ier)
      if ( ier .ne. 0 ) then
        write(iuout,*) 'File read error reading grid rate toggle'
        bFileError = .true.
      endif
      k = 0
      call egetwi(outstr,k,iGridRateToggle,0,2,
     &                           'W','grid rate toggle egetwi',ier)
      if ( ier .ne. 0 ) then
        write(iuout,*) 'Error reading grid rate toggle'
        bFileError = .true.
        write(cErrLoc,'(A)')'grid rate toggle'
      endif

      if (iGridRateToggle .eq. 2) then
C       (13a)
C       Read GridRateThreshold value
        call stripc(ireselecfl_ascii,outstr,0,nd,1,
     &                             'grid rate threshold stripc',ier)
        if ( ier .ne. 0 ) then
          write(iuout,*) 'File read error reading grid rate threshold'
          bFileError = .true.
        endif
        k = 0
        call egetwr(outstr,k,GridRateThreshold,0.,2.,
     &                         'W','grid rate threshold egetwr',ier)
        if ( ier .ne. 0 ) then
          write(iuout,*) 'Error reading grid rate threshold'
          bFileError = .true.
          write(cErrLoc,'(A)')'grid rate threshold'
        endif

C       Read data file name
C       (13b)
        call stripc(ireselecfl_ascii,outstr,0,nd,1,
     &                               'grid rate data filename',ier)
        if ( ier .ne. 0 ) then
          write(iuout,*)
     &       'File read error reading grid rate data filename'
          bFileError = .true.
        endif

        iFIL_GridRate=1
        write(fnamBDFlds(iFIL_GridRate),'(A)')
     &                             outstr(1:lnblnk(outstr))

      endif ! GridRateToggle .eq. 2

C     (14)
C     Read the file name for PV refund rate
C       0 ... bGridRefundHigh is set .false.
C       1 ... bGridRefundHigh is set .true.
C       2 ... grid refund is read from file, two additional lines are
C             read:
C             1) GridRefundThreshold value (real number) and
C             2) valid file name with grid refund values
      call stripc(ireselecfl_ascii,outstr,0,nd,1,
     &                              'grid refund toggle stripc',ier)
      if ( ier .ne. 0 ) then
        write(iuout,*) 'File read error reading grid refund toggle'
        bFileError = .true.
      endif
      k = 0
      call egetwi(outstr,k,iGridRefundToggle,0,2,
     &                          'W','grid refund toggle egetwi',ier)
      if ( ier .ne. 0 ) then
        write(iuout,*) 'Error reading grid refund toggle'
        bFileError = .true.
        write(cErrLoc,'(A)')'grid refund toggle'
      endif

      if (iGridRefundToggle .eq.2) then
C     Read GridRefundThreshold value
C       (14a)
        call stripc(ireselecfl_ascii,outstr,0,nd,1,
     &                                  'grid refund threshold',ier)
        if ( ier .ne. 0 ) then
          write(iuout,*) 'File read error reading grid refund threshold'
          bFileError = .true.
        endif
        k = 0
        call egetwr(outstr,k,GridRefundThreshold,0.,2.,
     &                           'W','grid refund threshold',ier)
        if ( ier .ne. 0 ) then
          write(iuout,*) 'Error reading grid refund threshold'
          bFileError = .true.
          write(cErrLoc,'(A)')'grid refund threshold'
        endif

C       Read data file name
C       (14b)
        call stripc(ireselecfl_ascii,outstr,0,nd,1,
     &                               'grid refund data filename',ier)
        if ( ier .ne. 0 ) then
          write(iuout,*)
     &       'File read error reading grid refund data filename'
          bFileError = .true.
        endif

        iFIL_GridRefund=2
        write(fnamBDFlds(iFIL_GridRefund),'(A)')
     &      outstr(1:lnblnk(outstr))

      endif ! iGridRefundToggle .eq. 2

      !-------------------------------!
      ! Mode 3                        !
      !-------------------------------!
      elseif (RES_elec_ctl_mode .eq.3) then
C     << to be defined >>

      else
        write(outstr,'(2A)')
     &      ' Undefined mode in renewable energy systems (elec)',
     &      ' control file (first parameter in file).'
        call edisp(iuout,outstr)
        STOP ' Renewable energy sys ctl file: unresolvable error.'
      endif ! mode specific input

C Error handling
      if ( bFileError ) then
        write(outstr,'(3A)') ' Error <',cErrLoc(1:lnblnk(cErrLoc)),
     &            '> in renewable energy systems (elec) control file.'
        call edisp(iuout,outstr)
         STOP ' Renewable energy sys ctl file fatal error '
      endif

C Reposition file to first line.
      rewind(ireselecfl_ascii)

C Notify user that control file processing is complete.
      call usrmsg( ' Reading RES elec control file ... done.',
     &             ' ', 'P')

      RETURN
      END

C===================================================================================
C============================= RES_elec_ctl_PostProcess ============================
C
C SYNOPSYS: This routine collects some interesting data pertaining
C to the renewable energy systems (elec) controller, and sends it on to
C h3kreports
C
C===================================================================================
      subroutine RES_elec_ctl_PostProcess()
      use h3kmodule
      implicit none
#include "building.h"
#include "plant.h"
#include "power.h"
#include "BATTERY.h"

C Input file information
      common/reselecctl/ireselecctl,reselecflnam
      integer ireselecctl       !- flag indicating that file required by renewable energy
                                !- system controller is specified
      character*72 reselecflnam !- name of file containing renewable energy system
                                !- controller inputs

C Basic RES (elec) controller inputs
      common/reselec/RES_elec_ctl_mode, RES_elec_ctl_scenario, NumBat
      integer RES_elec_ctl_mode     ! control mode 1, 2 or 3 << Note: currently only 2 is coded >>
      integer RES_elec_ctl_scenario ! control scenario active
      integer NumBat                !- Number of batteries in system

C Power-only component information for RES (elec) control
      common/reselec_powoc/IPWC_PVPCU,IPWC_gridconn,IPWC_ACDC,
     &                   IPWC_battery
      integer   IPWC_PVPCU      !-index number of PCU that is connected between the PV and the DC bus
      integer   IPWC_gridconn   !-index number of the grid-connection power-only component
      integer   IPWC_ACDC       !-index number of the DC-AC converter (a PCU power-only component)
      integer   IPWC_battery    !-index number of the battery power-only component

C Electric node information
      common/reselec_enode/IENOD_ACbus,IENOD_DCbus
      INTEGER IENOD_ACbus    ! Elec domain node number of AC bus.
      INTEGER IENOD_DCbus    ! Elec domain Node number of DC bus

C Flags indicating status of controller
      common/RESElec_ctl_BatStat/bPV_active,bBattery_charge,
     &                    bBattery_discharge

      logical bPV_active, bBattery_charge, bBattery_discharge

C 'Network interaction'
      real fNetworkBalance

      logical DEBUG
      DEBUG = .false.  ! toggle debug mode

C     Return if RESElec control not active.
      if ( iRESElecCtl .ne. 1 ) return

C     Get power balance on AC bus
C     (balance = generation + load, loads are -ive)

      fNetworkBalance = penodg(IENOD_ACbus) + penodl(IENOD_ACbus)

      IF (mandChargeCycle(IPWC_battery) .EQ. 1) THEN
C       When the battery performs a mandatory charge mode, the direction
C       of flow of power through the DC/AC converter could be reversed.
C       This will not be possible with the electric current network.
C       Instead of changing the electric network, the potential load of
C       the DC node (node 1) is added to the network balance of the AC
C       node (node 3) using an average efficiency of the then AC/DC
C       converter of 98%.
        IENOD_DCBUS = 1
        fNetworkBalance = fNetworkBalance +
     &               (penodg(IENOD_DCbus) + penodl(IENOD_DCbus))/0.98
      ENDIF

C*** DEBUG ===
      if (DEBUG) then
        write(*,*)'fNetworkBalance=',fNetworkBalance,' [W?]'
      endif
C ====

C@@@@@
C     Control scenario this TS
      Call AddToReport(rvRESElecCtlScenario%Identifier,
     &         real(RES_elec_ctl_scenario))
C@@@@@

C Determine operating mode, and report data to h3kreports

      if (  bBattery_charge ) then
C.......PV is charging battery
cx        Call AddToReport(rvCtrlReH2NPwBattChrg%Identifier,
cx     &         fNetworkBalance)
      else
cx        Call AddToReport(rvCtrlReH2NPwBattChrg%Identifier,0.0)
      endif

      if ( bBattery_discharge ) then
C.......Battery is responding to loads
cx        Call AddToReport(rvCtrlReH2NPwBattDschrg%Identifier,
cx     &         fNetworkBalance)
      else
cx        Call AddToReport(rvCtrlReH2NPwBattDschrg%Identifier,0.0)
      endif

      if ( bPV_active ) then
C.......Battery is responding to loads
cx        Call AddToReport(rvCtrlReH2NPwPEMFCAct%Identifier,
cx     &         fNetworkBalance)
      else
C.......Battery is responding to loads
cx        Call AddToReport(rvCtrlReH2NPwPEMFCAct%Identifier,0.0)
      endif

cx      Call AddToReport(rvElecNetCtrlScn%Identifier,
cx     &      REAL(RES_elec_ctl_scenario),
cx     &      powcomnam(IPWC)(1:iNameLength))

      return
      end


C *********************************************************************************************
C ******************************* CreateBinaryDataFile ****************************************
C Created by: Achim Geissler
C Initial Creation Date: July 7, 2014
C Copyright FHNW
C
C This subroutine is based on cetc/FC_components.F::FC_eloads_convert. 
C It checks the integrity of the user-specified data file and produces a binary format 
C data file that can be used for direct access during the time-step simulation. 
C This subroutine is executed once per simulation, prior to the time-step calculations.
C -------------------------------------------------------------------------------------------
      SUBROUTINE CreateBinaryDataFile(iBDF)
      IMPLICIT NONE
#include "BinaryDataFile.h"

      integer iBDF

C---------------------------------------------------------------------------------
C ESP-r commons
C---------------------------------------------------------------------------------
      COMMON/FILEP/IFIL
      INTEGER  IFIL

      COMMON/OUTIN/IUOUT,IUIN,IEOUT
      INTEGER  IUOUT,IUIN,IEOUT

C---------------------------------------------------------------------------------
C Declare local variables.
C---------------------------------------------------------------------------------
      INTEGER ND,K,num_recs,num_col,num_incbin,inc,incbi,ire
      integer iFIL_ascii  ! temporary local file number
      
      CHARACTER OUTSTR*124

      LOGICAL fclerror
      LOGICAL bdf_loads_exist

      REAL current,left,right
      REAL elecload(12),elecloadb(12)
      INTEGER ISTAT,IER,I
      INTEGER irec, incbin

C---------------------------------------------------------------------------------
C Read the header information from the user-specified data file (.fcl)
C and perform some checks to ensure that the correct number of data
C are specified. Note that this does not check the validity of the data,
C but rather ensures that the correct number of data items are specified.
C The existence of the .fcl file was confirmed when the .cfg file was read.((then why check below??))
C `fclerror' is a flag indicating whether there were errors reading the .fcl file.
C---------------------------------------------------------------------------------
C-----Notify user that data file is being processed (this can take some time).
      call usrmsg(' Processing BDF type input file...',
     &            ' ','-')
C-----Open the .fcl file.
      fclerror = .false.
      iFIL_ascii = IFIL+28  ! Should be a safe unit number to use.
      CALL ERPFREE(iFIL_ascii,ISTAT)
      CALL EFOPSEQ(iFIL_ascii,fnamBDFlds(iBDF),1,IER)
      IF(IER /= 0)THEN
        WRITE(IUOUT,*) ' Error opening BDF type input file, index ',
     &    iBDF,'.'
        fclerror = .true.
      ENDIF
C-----Read the flag indicating the method used for prescribing the loads.
      CALL STRIPC(iFIL_ascii,OUTSTR,0,ND,1,' BDF input type',IER)
      IF(IER /= 0)THEN
        WRITE(IUOUT,*)
     &    ' Error reading BDF input type, file index ',iBDF,'.'
        fclerror = .true.
      ENDIF
      K=0
      CALL EGETWI(OUTSTR,K,iBDF_lds_type(iBDF),1,2,'F',
     &                                        ' BDF loads type ',IER)
      IF(IER /= 0)THEN
        WRITE(IUOUT,*)
     &    ' Error reading BDF loads input type, file index ',iBDF,'.'
        fclerror = .true.
      ENDIF
C-----Read the number of data points specified for each record of loads data.
C-----Ensure that the maximum number of data points has not been exceeded
C-----for the selected input method.
      CALL STRIPC(iFIL_ascii,OUTSTR,0,ND,1,' BDF loads per hour',IER)
      IF(IER /= 0)THEN
        WRITE(IUOUT,*)
     &    ' Error reading number of data items per record,',
     &    ' file index ',iBDF,'.'
        fclerror = .true.
      ENDIF
      K=0
      CALL EGETWI(OUTSTR,K,BDF_lds_per_rec(iBDF),1,12,'F',
     &            ' FC loads per rec ',IER)
      IF(IER /= 0)THEN
        WRITE(IUOUT,*)
     &  ' Error reading number of data items per record, file index ',
     &    iBDF,'.'
        fclerror = .true.
      ENDIF
C-----Check that the correct number of loads per record are present
C-----and set number of records. 
      SELECT CASE (iBDF_lds_type(iBDF))
        CASE(BDF_lds_annual)
          if( BDF_lds_per_rec(iBDF) > BDF_lds_per_rec_annual )then
            WRITE(IUOUT,*)
     &        ' Too many data items per record, file index ',iBDF,'.'
            fclerror = .true.
          endif
          num_recs = 8760
        CASE(BDF_lds_daily)
          if( BDF_lds_per_rec(iBDF) > BDF_lds_per_rec_daily )then
            WRITE(IUOUT,*)
     &        ' Too many data items per record, file index ',iBDF,'.'
            fclerror = .true.
          endif
          num_recs = 1440
        CASE DEFAULT
          fclerror = .true.
      END SELECT

C-----Check that correct number of records is present.
      DO I=1,num_recs
        CALL STRIPC(iFIL_ascii,OUTSTR,99,num_col,1,
     &              ' Test read of a record',IER)
        if(IER /= 0)then
          WRITE(IUOUT,*) ' End of fuel cell elec loads file.'
          fclerror = .true.
        endif
        if( num_col/=BDF_lds_per_rec(iBDF) )then
          WRITE(IUOUT,*) ' Incorrect number of data items per record.'
          fclerror = .true.
        endif
      END DO

C-----Error handling on reading of .fcl file.
      IF(fclerror)THEN
        STOP ' Error in elec loads input file.'
      ENDIF

C-----Reposition .fcl file file to the first record containing loads data.
      REWIND(iFIL_ascii)
      DO I=1,2
        CALL STRIPC(iFIL_ascii,OUTSTR,0,ND,1,' skip header',IER)
      END DO


C---------------------------------------------------------------------------------
C Open a file with direct access (i.e. a binary file) that will be used during
C the time-step simulation to set the electrical load on the fuel cell. This
C temporary file will be deleted following completion of the time-step simulation.
C---------------------------------------------------------------------------------
      iFIL_BDF(iBDF) =  IFIL+29+iBDF
      CALL ERPFREE(iFIL_BDF(iBDF),ISTAT)
      INQUIRE (UNIT=iFIL_BDF(iBDF),EXIST=bdf_loads_exist) ! check if file exists
      IF (bdf_loads_exist) then
         CALL EFDELET(iFIL_BDF(iBDF),ISTAT) ! Delete file if it exists
      endif
      SELECT CASE (iBDF_lds_type(iBDF))
        CASE(BDF_lds_annual)
          CALL FPRAND(iFIL_BDF(iBDF),ISTAT,BDF_lds_per_rec_annual,3,
     &                'FCloads.tmp')  ! File may exist if prev. simulation was
                                      ! interrupted
          BDF_lds_per_rec_bin(iBDF) = BDF_lds_per_rec_annual
        CASE(BDF_lds_daily)
          CALL FPRAND(iFIL_BDF(iBDF),ISTAT,BDF_lds_per_rec_daily,3,
     &                'FCloads.tmp')
          BDF_lds_per_rec_bin(iBDF) = BDF_lds_per_rec_daily
        CASE DEFAULT  
          ! Clear by now that BDF_lds_type is valid.
      END SELECT

C---------------------------------------------------------------------------------
C Iterate through each record in the .fcl ASCII file. For each record of data in
C .fcl ASCII file, read the load at each time increment specified. Write the
C loads data to the binary file at the pre-determined time resolution
C (i.e. 5 minutes for `BDF_lds_annual' and 10 seconds for `BDF_lds_daily'. If user
C has specified loads at a coarser time increment, then assume the load is
C constant over a number of time increments when creating the binary file.
C---------------------------------------------------------------------------------

      DO irec=1,num_recs
C-------Read the loads data from the ASCII file.
        READ(iFIL_ascii,*) (elecload(inc),inc=1,BDF_lds_per_rec(iBDF))
C-------If user time-increment differs from time increment in binary file,
C-------then spread user data over number of time increments in binary file.
C-------Uses a step-function approach.
        DO inc=1,BDF_lds_per_rec(iBDF)
          DO incbin=1,BDF_lds_per_rec_bin(iBDF)
            current = float(incbin) / float(BDF_lds_per_rec_bin(iBDF))
            left    = float(inc-1)  / float(BDF_lds_per_rec(iBDF))
            right   = float(inc)    / float(BDF_lds_per_rec(iBDF))
            if( current > left .and. current <= right ) then
              elecloadb(incbin) = elecload(inc)
            endif
          END DO
        END DO
C-------Write the loads data to the binary file.
        WRITE(iFIL_BDF(iBDF),REC=irec,IOSTAT=ISTAT)
     &       (elecloadb(incbin),incbin=1,BDF_lds_per_rec_bin(iBDF))
      END DO
C-----Write header information at end of binary file.
      WRITE(iFIL_BDF(iBDF),REC=8761,IOSTAT=ISTAT)
     &      iBDF_lds_type(iBDF),BDF_lds_per_rec(iBDF)

C-----Set flag to "file created"
      iBDFlds(iBDF)=1

C Processing complete. Leave binary file open for use during time-step simulation.
      call usrmsg(' Processing input file... done.',
     &            ' ','-')

      RETURN
      END


C *********************************************************************************************
C ******************************* ReadBinaryDataFile ******************************************
C Created by: Achim Geissler
C Initial Creation Date: July 7, 2014
C Copyright FHNW
C
C This subroutine is based on FC_eloads_establish and retrieves the data value for the current 
C time-step. This subroutine reads the data value from the binary file created by
C CreateBinaryDataFile.
C This subroutine is executed each time the RES (elec) control subroutine "xxx" is executed, 
C which may be multiple times per plant simulation increment.

C -------------------------------------------------------------------------------------------
C INPUT:
C     iBDF             Index for BDF file
C
C OUTPUTS:
C     CurrentValue     The non-HVAC electrical load for the current plant simulation
C                      increment (W).
C -------------------------------------------------------------------------------------------
      SUBROUTINE ReadBinaryDataFile(iBDF,CurrentValue)
      IMPLICIT NONE
#include "BinaryDataFile.h"

C     Subroutine parameters
      integer iBDF
      real CurrentValue

C-------------------------------------------------------------------------------
C     ESP-r commons
C-------------------------------------------------------------------------------
      COMMON/OUTIN/IUOUT,IUIN,IEOUT           ! Number for default output and input screens
      INTEGER  IUOUT,IUIN,IEOUT

C Common holding simulation type (building, building+plant, or plant-only simulation)
      integer indcfg
      common/c6/indcfg

C---------------------------------------------------------------------------------
C Commons holding simulation time-clock variables.
C---------------------------------------------------------------------------------
      common/simtim/ihrp,ihrf,idyp,idyf,idwp,idwf,nsinc,its,idynow
      INTEGER IHRP,IHRF,IDYP,IDYF,IDWP,IDWF,NSINC,ITS,idynow
      

      common/btime/btimep,btimef
      common/ptime/ptimep,ptimef
      REAL BTIMEP,btimef,PTIMEP,ptimef

C---------------------------------------------------------------------------------
C Declare local variables.
C---------------------------------------------------------------------------------
      INTEGER record,incbin,after_midnight
      REAL elecloadb(12),frac_hr,frac_min,tenth
      REAL q_nonHVAC_elec
      LOGICAL CLOSE,XST
      character    outs*124
      integer ISTAT,itu
      REAL fFuture_Time           ! time on future time row.

C--------
      integer lnblnk
      ITU=IUOUT  ! assign unit for warning message.

C-------------------------------------------------------------------------------------------
C From: esrupfs/powow_calc.F, function fGet_nonHVACld(cFileName), line 357 ff.
C-----
C If this subroutine is being called for the first time then process the user-specified
C electrical loads data. Determine data type and convert from ASCII file to binary-format
C for direct access during simulation.
C-------------------------------------------------------------------------------------------
      IF(iBDFlds(iBDF).eq.0) THEN
         XST=.FALSE.
         call FINDFIL(fnamBDFlds(iBDF)(1:lnblnk(fnamBDFlds(iBDF))),XST)
         if(XST)then
            iBDFlds(iBDF)=1
         else
           write(outs,'(3a)')
     &       'Data file ',fnamBDFlds(iBDF)(1:lnblnk(fnamBDFlds(iBDF))),
     &                       ' was referenced but not found.'
           call edisp(iuout,outs)
           STOP ' RES (elec) read data file: unresolvable error.'
         endif
         CALL CreateBinaryDataFile(iBDF)
      END IF

C--------------------------------------------------------------------------------
C Determine the time corresponding to future time row
C--------------------------------------------------------------------------------
      if ( indcfg .eq. 1 ) then
C........Building+electrical net simulation --- elec. domain is processed at the
C........same frequency as building domain. Building time is 'future time'.
         fFuture_Time = btimef

      elseif ( (indcfg .eq. 2) .or. (indcfg .eq. 3) ) then
C........Plant+electrical net simulation --- elec. & plant domains are
C........processed at same frequency. Plant time is 'future time.
         fFuture_Time = ptimef

      else
C........Unsupported simulation type. 
         write (itu,'(A,I2,A)')
     &    ' ReadBinaryDataFile(): Specified simulation type (', 
     &                                                 indcfg,')'
         write (itu,'(A)')
     &    '                    is unsupported. See powoc_calc.F.'
         stop 'ReadBinaryDataFile(): Fatal error!'
      endif

      
C---------------------------------------------------------------------------------
C Determine which record of the binary file contains the correct data
C for the `future' time that is currently being simulated. There are two 
C possible formats for the binary file: one record for each
C hour of the year (indicated by `BDF_lds_annual') or one record for each minute
C of a typical day (indicated by `BDF_lds_daily').
C---------------------------------------------------------------------------------
      SELECT CASE (iBDF_lds_type(iBDF))

C---------------------------------------------------------------------------------
C One record for each hour of the year.
C The hour of the year defines the record in the file: the first
C record of the binary file contains the data for 0h00m00s to 0h59m59s of
C January 1; the 8760th record contains the data for 23h00m00s to 23h59m59s of
C December 31.
C Special attention has to be paid when the `future' time in the plant domain
C crosses over midnight, as the `present' day will not have yet been incremented.
C When this occurs, the `present' time in the building domain will be 24 hours
C and the `future' time in the plant domain will be less than or equal to 1 hour.
C Special attention must also be be paid when the `present' time in the building
C domain (BTIMEP) crosses over midnight and when sub-hourly building time-steps
C are used. In this case, the `present' day (IDYP) does not increment until
C BTIMEP=1.0.
C The electrical load at 24h00 is taken as the load at 0h00 of the next day
C (as described above, each record covers the period from 0h00m00s to 23h59m59s).
C Care has to be taken when the simulation crosses over the New Year: the load for
C midnight December 31 is set using the data at 0h00 on January 1.
C---------------------------------------------------------------------------------
        CASE(BDF_lds_annual)
C---------Have we just crossed over midnight?
          CALL ECLOSE(BTIMEP,24.0,0.0001,CLOSE)
          IF( CLOSE .and. fFuture_Time<=1. )THEN
            after_midnight = 24
          ELSEIF( BTIMEP<1.0 )THEN  ! IDYP does not increment until BTIMEP=1.0.
            after_midnight = 24
          ELSE
            after_midnight = 0
          END IF
C---------Determine which record to use.
          record = 24*(IDYP-1)+INT(fFuture_Time)+1+after_midnight
C---------Has midnight on New Year's eve just struck?
          CALL ECLOSE(fFuture_Time,24.0,0.0001,CLOSE)
          IF( CLOSE .and. IDYP==365 )THEN
            record = record - 8760
          END IF
          CALL ECLOSE(BTIMEP,24.0,0.0001,CLOSE)
          IF( CLOSE .and. fFuture_Time<=1. .and. IDYP==365 )THEN
            record = record - 8760
          END IF
          IF( BTIMEP<1.0 .and. IDYP==365 )THEN ! IDYP does not increment until BTIMEP=1.0.
            record = record - 8760
          END IF
C---------Read the loads for each time increment for the current hour.
          READ(iFIL_BDF(iBDF),REC=record,IOSTAT=ISTAT)
     &        (elecloadb(incbin),incbin=1,BDF_lds_per_rec_bin(iBDF))
C---------Now determine which of these increments corresponds to the current
C---------time-step. `frac_hr' represents the fraction of an hour that we are
C---------past the top of the hour.
          frac_hr = fFuture_Time - AINT(fFuture_Time)             ! AINT truncates a real.
          incbin = INT(frac_hr*BDF_lds_per_rec_bin(iBDF)+0.001)+1 ! 0.001 prevents rounding error.
C---------Set the electrical load for the current time-step.
          CurrentValue = elecloadb(incbin)

C---------------------------------------------------------------------------------
C One record for each minute of a typical day (used for very short time-step
C simulations). The minute of the day defines the record in the file: the first 
C record contains the data for 00h00m00s to 00h00m59s; the 1440th record contains 
C the data for 23h59m00s to 23h59m59.
C---------------------------------------------------------------------------------
        CASE(BDF_lds_daily)
C---------Determine which record to use.
          tenth = 0.1 / 60.  ! Tenth of sec: used because fFuture_Time sometimes has rounding errors.
          record = INT( fFuture_Time*60. + tenth ) + 1
C---------Has midnight just struck in the plant domain?
          CALL ECLOSE(fFuture_Time,24.0,0.0001,CLOSE)
          IF( CLOSE ) record = 1
C---------Read the loads for each time increment for the current minute.
          READ(iFIL_BDF(iBDF),REC=record,IOSTAT=ISTAT)
     &        (elecloadb(incbin),incbin=1,BDF_lds_per_rec_bin(iBDF))
C---------Now determine which of these increments corresponds to the current
C---------time-step. `frac_min' represents the fraction of a minute that we are
C---------past the top of the minute.
          frac_min = (fFuture_Time*60. + tenth) 
     &                   - AINT( fFuture_Time*60. + tenth )
          incbin = INT(frac_min*BDF_lds_per_rec_bin(iBDF)+0.001)+1 ! 0.001 prevents rounding error.
C---------Set the electrical load for the current time-step.
          CurrentValue = elecloadb(incbin)

C---------------------------------------------------------------------------------
        CASE DEFAULT
          ! This cannot happen as there are error traps upstream.
      END SELECT

C-----If  is less than or equal to 0., then set to 1. and
C-----issue warning to user
      IF(CurrentValue.LE.0.) THEN
        CurrentValue = 1.
        WRITE(IUOUT,*) ' Warning: Value from .fcl file ',iBDF,' is'
        WRITE(IUOUT,*) ' zero on day ',IDYP,' and hour',
     &  BTIMEP
      ENDIF

      RETURN
      END

C===================================================================================
C============================ RES_elec_Ctl_Status_Blockdata ===========================
C
C SYNOPSYS: Initialization of value of bRESElec_ctl_unconverged flag
C
C << copy from Resh2xxx, unclear at this point if actually necessary ... >>
C
C===================================================================================
      blockdata reselec_ctl_status_blockdata
      common / reselec_ctl_status / bRESElec_ctl_unconverged
      logical bRESElec_ctl_unconverged         !-logical flag indicating additional iterations req'd
      data bRESElec_ctl_unconverged/.false./   !initial value
      end blockdata reselec_ctl_status_blockdata
