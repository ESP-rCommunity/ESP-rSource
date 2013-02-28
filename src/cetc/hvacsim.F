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

c The file contains the following routines:
c  START_HVAC_SIM
c   start the simulation of HVAC systems performance by
c   calling HVACSIM
c  HVACSIM    control the simulation of the performance of the hvac
c   system
c  PLR   determine the part-load ratio of the hvac equipment
c *********************************************************************


      CHARACTER*16 FUNCTION HVAC_STRING(iHVAC_Num)

      IMPLICIT NONE
      INTEGER    FURNACE
      PARAMETER ( Furnace = 1 )

      INTEGER    BOILER
      PARAMETER ( BOILER = 2 )

      INTEGER   BASEBOARD
      PARAMETER( BASEBOARD = 3 )

      INTEGER   ASHP
      PARAMETER ( ASHP = 7 )

      INTEGER GSHP
      PARAMETER ( GSHP = 8 )

      INTEGER iHVAC_Num
      SELECT CASE (iHVAC_NUM)
      CASE (FURNACE)
         HVAC_STRING ='FURNACE'
      CASE (BOILER)
         HVAC_STRING ='BOILER'
      CASE (BASEBOARD)
         HVAC_STRING ='BASEBOARD'
      CASE (ASHP)
         HVAC_STRING ='ASHP'
      CASE (GSHP)
         HVAC_STRING ='GSHP'
      CASE DEFAULT
         HVAC_STRING = 'UNKNOWN_TYPE'
      END SELECT

      RETURN
      END


c *********************************************************************
c START_HVAC_SIM

c Created by: Kamel Haddad
c Initial Creation Date: May 1th 2001
c Copyright 2000: NRCan Buildings Group

c Subroutine to start HVAC systems simulation for the simulation period
c after the startup period

c     INPUTS:
c     iday  day corresponding to the current time step
c     ********************************************************************
      SUBROUTINE START_HVAC_SIM(iday)

      IMPLICIT NONE

      INTEGER iday

c Start HVAC simulation from the beginning of the sart up period to the end
c of the simulation period. Starting at the beginning of the sart up period
c is important especially for cooling systems where the moisture content
c inside the space is affected by the HVAC system operation. This is also
c needed for ground-source heat pump systems. The output for the start up
c period is not included in the H3K output report for save level 5.
      CALL HVACSIM

      return
      end

c     *********************************************************************
c     *********************************************************************
c     HVACSIM

c     Created by: Kamel Haddad
c     Initial Creation Date: February 17th 2000
c     Copyright 2000: NRCan Buildings Group

c     Controlling routine for the simulation of the energy consumption of
c     the HVAC system. This subroutine is called from MZNUMA each simulation
c     time-step following the simulation of the zones, in which the
c     building's heating or cooling requirements are determined.
c     The appropriate simulation subroutine is called based on
c     the HVAC system type.

c     INPUTS:
c     HVAC_INPUT_DATA
c                       common for hvac systems input data
c     UNDER_HEAT_COOL_HOURS
c                       common for total number of hours there is under
c                       heating or under cooling
c     HEAT_PUMP_INPUT_DATA
c                       common for heat pump data
c     pvala             common for the present time step loads
c     fvala             common for the future time step loads
c     GCEP_INPUT        common for parameters for GCEP
c     hvacinfo          common for hvac simulation flag

c     OUTPUTS:
c     HVAC_OUTPUT  common for system energy consumption for time
c     FC_ctl_h3khvacloads
c                       common for electrical draws of hvac systems
c     step
c     *********************************************************************
      SUBROUTINE HVACSIM
      use h3kmodule
      IMPLICIT NONE

#include "hvac_parameters.h"

#include "building.h"

#include "hvac_common.h"

#include "ashp_common.h"

#include "SOFC.h"

#include "gcep_common.h"

#include "OffsiteUtilitiesPublic.h"

#include "control.h"

c hvac system file and flag:  ihvacflag=1 there is hvac file
c ihvacflag=0 no hvac file
      common/hvacinfo/ihvacflag,hvacfile

      INTEGER ihvacflag
      CHARACTER*72 hvacfile
      CHARACTER*2, cHVACIndex
c Use common for HVAC system energy consumption
      common/HVAC_OUTPUT/fan_energy(max_sys),
     &draft_energy(max_sys), sys_energy(max_sys),
     &backup_energy(max_sys), pilot_energy(max_sys),
     &GSHP_pump_energy, GCEP_pump_energy

c Real variables in HVAC system energy consumption common
      REAL fan_energy, draft_energy, sys_energy, backup_energy,
     &pilot_energy, GSHP_pump_energy, GCEP_pump_energy

C Construction heat injection and temperature commons.
      COMMON/FVALC/             ! FUTURE:
     &     TFC(MCOM,MS,MN),     ! - construction temperature (oC)
     &     QFC(MCOM)            ! - construction heat injection (W)
      REAL TFC, QFC

      COMMON/PVALC/             ! PRESENT:
     &     TPC(MCOM,MS,MN),     ! - construction temperature (oC)
     &     QPC(MCOM)            ! - construction heat injection (W)
      REAL TPC, QPC

C Zone control data
      integer icascf
      common/cctl/icascf(mcom)  ! Zone control type

c Common for the present and the future zone temperatures and convective
c heat injections.
      common/pvala/tpa(mcom),qpa(mcom)
      common/fvala/tfa(mcom),qfa(mcom)

      REAL tpa,qpa,tfa,qfa

c Common for the current and future hours of the simulation
      common/simtim/ihrp,ihrf,idyp,idyf,idwp,idwf,nsinc,its

      INTEGER ihrp,ihrf,idyp,idyf,idwp,idwf,nsinc,its

c Common for simulation start and finish days
      common/simsdy/iss,isf

      INTEGER iss,isf

c Common for the number of time steps per hour
      COMMON/PERS/ISD1,ISM1,ISD2,ISM2,ISDS,ISDF,NTSTEP

      INTEGER ISD1,ISM1,ISD2,ISM2,ISDS,ISDF,NTSTEP

c Variables for save level 5
      real fPPLR, fBPLR, fb_pilot_energy, fP_pilot_energy,
     &     fHeat_energy, fCool_energy

c Local real variables
      REAL plr_furnace, plr_backup, plr_heat_pump, plr_boiler,
     &plr_baseboard, plf_backup, plf_heat_pump, plf_furnace,
     &plf_boiler, plf_baseboard, heating_load, cooling_load,
     &req_load, time_step, FAN_ENERGY_CON, DRAFT_FAN_ENERGY,
     &heat_pump_cap, GSHP_pump_power, GCEP_pump_power,
     &heat_pump_cooling_cap

c Local integer variables
      INTEGER i, j, month, iday, icomp, iCoolSys, ihour_year

c Logical variable for whether there is space cooling load or ventilation load
      LOGICAL cooling_vent_yes,closea,closeb

C.....Energy content of fuel used, dissaggretated by end-use
      real fH3KSumEnergyUseHardWood(iNumUses)    ! Energy consumption assoc. w/ wood (W)
      real fH3KSumEnergyUseSoftWood(iNumUses)    !
      real fH3KSumEnergyUseMixWood(iNumUses)     !
      real fH3KSumEnergyUsePellets(iNumUses)     !
      real fH3KSumEnergyUseOil(iNumUses)         ! Energy consumption assoc. w/ oil (W)
      real fH3KSumEnergyUseGas(iNumUses)         ! Energy consumption assoc. w/ N. Gas (W)
      real fH3KSumEnergyUsePropane(iNumUses)     ! Energy consumption assoc. w/ propane     (W)
      real fH3KSumEnergyUseElec(iNumUses)        ! Energy consumption assoc. w/ electricuty (W)
C     real fH3KEnergyUseElecAux(iNumUses)        ! Energy consumption assoc. w/ additional
                                                 !   auxilary subsystems (e.g. Fan, W)
      integer iSysEndUse                         ! End suse for system

C.....Total fan energy
      real fH3KSumFanEnergy


C.....Function returing fuel energy used:
      real fConvertEnergyToFuelAmount

C.....Storage varaibles used to rationalize H3Kreports reporting
      integer iBackup
      real fH3KStorePLRH(max_sys)        ! PLR ratio-Heating
      real fH3KStoreFanEH(max_sys)       ! Fan power (W)
      real fH3KStoreDraftE(max_sys)      ! Draft fan power (W)
      real fH3KStorePrimaryEH(max_sys)   ! Energy input for Heating (W)
      real fH3KStorePilotE(max_sys)      ! Energy input for pilot (W)
      real fH3KStorePumpEH(max_sys)      ! Energy input for pump (W)
      real fH3KStoreEfficiencyH(max_sys) ! System efficiency
      real fH3KStoreOutputH(max_sys)     ! Thermal output (W)
      real fH3KStoreCOPH(max_sys)        ! Coefficient of performance - heating

      real fH3KStorePLRC(max_sys)        ! PLR ratio-cooling
      real fH3KStoreFanEC(max_sys)       ! Fan power (W)
      real fH3KStorePrimaryEC(max_sys)   ! Energy input for cooling (W)
      real fH3KStorePumpEC(max_sys)      ! Energy input for pump (W)
      real fH3KStoreEfficiencyC(max_sys) ! System efficiency
      real fH3KStoreOutputC(max_sys)     ! Thermal output (W)
      real fH3KStoreCOPC(max_sys)        ! Coefficient of performance - cooling

!
! C.....Storage for reporting corresponding for heating/cooling heatpump
! C.....systems
!       real fH3KLinkedPLR(max_sys)       ! PLR ratio
!       real fH3KLinkedFanE(max_sys)      ! Fan power (W)
!       real fH3KLinkedDraftE(max_sys)    ! Draft fan power (W)
!       real fH3KLinkedPrimaryE(max_sys)  ! Energy input for cooling (W)
!       real fH3KLinkedPilotE(max_sys)    ! Energy input for pilot (W)
!       real fH3KLinkedPumpE(max_sys)     ! Energy input for pump (W)
!       real fH3KLinkedEfficiency(max_sys)! System efficiency
!       real fH3KLinkedOutput(max_sys)    ! Thermal output (W)
!       real fH3KLinkedCOP(max_sys)       ! Coefficient of performance


C.....Zero variables used to roll up energy use from all systems.
      use_loop: do iSysEndUse = 1, iNumUses
        fH3KSumEnergyUseElec    ( iSysEndUse ) =  0.0
        fH3KSumEnergyUseSoftWood( iSysEndUse ) =  0.0
        fH3KSumEnergyUseHardWood( iSysEndUse ) =  0.0
        fH3KSumEnergyUseMixWood ( iSysEndUse ) =  0.0
        fH3KSumEnergyUsePellets ( iSysEndUse ) =  0.0
        fH3KSumEnergyUseGas     ( iSysEndUse ) =  0.0
        fH3KSumEnergyUseOil     ( iSysEndUse ) =  0.0
        fH3KSumEnergyUsePropane ( iSysEndUse ) =  0.0
      enddo use_loop

C Reset variables that track electrical draws of HVAC equipment that is simulated
C in the building thermal domain (ie. the H3K HVAC models invoked below).
C This is used by the fuel cell controller to determine the house's total
C electrical demand during each time-step in the plant domain.
      FCctl_furnace_draft   = 0.
      FCctl_furnace_circfan = 0.
      FCctl_AC_compressor   = 0.
      FCctl_AC_circfan      = 0.
      FCctl_boiler_draft    = 0.
      FCctl_boiler_circfan  = 0.
      FCctl_baseboard_fan   = 0.
      FCctl_ashp_compressor = 0.
      FCctl_ashp_circfan    = 0.
      FCctl_gshp_pump       = 0.

C Length of time-step (s).
      time_step = 3600. / float(NTSTEP)

C Zero H3KReports storage routines. Note: this code must be kept apart
C from the main HVAC system loop (below) because the latter solves
C systems according to their primary/backup status (as opposed to
C sequentially)
      zero_storage_routines: do i = 1, num_hvac_sys

        fH3KStorePLRH(i)          = 0.0
        fH3KStoreFanEH(i)         = 0.0
        fH3KStoreDraftE(i)        = 0.0
        fH3KStorePrimaryEH(i)     = 0.0
        fH3KStorePilotE(i)        = 0.0
        fH3KStorePumpEH(i)        = 0.0
        fH3KStoreEfficiencyH(i)   = 0.0
        fH3KStoreOutputH(i)       = 0.0
        fH3KStoreCOPH(i)          = 0.0

        fH3KStorePLRC(i)          = 0.0
        fH3KStoreFanEC(i)         = 0.0
        fH3KStorePrimaryEC(i)     = 0.0
        fH3KStorePumpEC(i)        = 0.0
        fH3KStoreEfficiencyC(i)   = 0.0
        fH3KStoreOutputC(i)       = 0.0
        fH3KStoreCOPC(i)          = 0.0

      enddo zero_storage_routines

C Set zone call heating/cooling bools to false
      do 5 i = 1, num_hvac_sys
        do 4 j = 1, num_sys_zones(i)

            zone_call_heat(i,j) = .false.
            zone_call_cool(i,j) = .false.

  4     continue
  5   continue

c     For each of the HVAC systems, call the appropriate subroutine to determine
c     the energy consumption of the system for the current time step. Before the
c     specific system subroutine is called, the total heating or cooling load on the
c     HVAC system is determined. This is the sum of all the heating or the cooling
c     loads during the current time step for all the zones being served by the
c     HVAC system.
      do 10 i = 1,num_hvac_sys

c     Determine the total heating or cooling requirement of the zones
c     associated with the HVAC system i. This load is passed on to the
c     equipment simulation subroutine to determine the energy cosumption.
         heating_load = 0.
         cooling_load = 0.

c     The load for the current time step associated with the hvac system considered is
c     the sum of the loads for the zones served by the hvac system. The load for each
c     of the zones is the average of the load at the present and future time steps, qpa
c     and qfa respectively
         do 20 j=1,num_sys_zones(i)
            if ( iban(icascf( isys_zone_num(i,j)),2) .gt. 0 ) then
C...........PIN is within construction

               req_load =( QPC(isys_zone_num(i,j)) +
     &              QFC((isys_zone_num(i,j)) )) / 2.0

            else
               req_load = 0.5 * (qfa(isys_zone_num(i,j)) +
     &              qpa(isys_zone_num(i,j)))
                req_load = qfa(isys_zone_num(i,j))
            endif

            if(req_load.gt.0) then

               zone_call_heat(i,j) = .true.
               heating_load = heating_load + req_load

            elseif(req_load.lt.0) then

               zone_call_cool(i,j) = .true.
               cooling_load = cooling_load + abs(req_load)

            endif
 20      continue

c     Initialize the part-load ratios and part-load factors
c     Furnace part-load ratio
         plr_furnace = 0.
c     Backup system part-load ratio
         plr_backup = 0.
c     Heat pump part-load ratio
         plr_heat_pump = 0.
c     Boiler part-load ratio
         plr_boiler = 0.
c     Baseboard part-load ratio
         plr_baseboard = 0.
c     Backup system part-load factor
         plf_backup = 1.0
c     Heat pump part-load factorcHVACIndex
         plf_heat_pump = 1.0
c     Furnace part-load factor
         plf_furnace = 1.0
c     Boiler part-load factor
         plf_boiler = 1.0
c     Baseboard part-load factor
         plf_baseboard = 1.0
c     Primary system PLR for H3K save lavel 5
         fPPLR = 0
c     Backup system PLR for H3K save level 5
         fBPLR = 0.

c     Initialize the system energy consumptions for the time step
c     Primary system energy consumption
         sys_energy(i) = 0.
c     Indoor circulation fan energy consumption
         fan_energy(i) = 0.
c     Furnace/Boiler draft fan energy consumption
         draft_energy(i) = 0.
c     Backup system energy consumption
         backup_energy(i) = 0.
c     Furnace pilot energy consumption
         pilot_energy(i) = 0.
c     Furnace pilot energy consumption for H3K save level 5
         fP_pilot_energy = 0.
c     Furnace pilot energy consumption for H3K save level 5
         fB_pilot_energy = 0.
c     Primary system energy consumption for heating for H3K save level 5
         fHeat_Energy = 0.
c     Primary system energy consumption for cooling for H3K save level 5
         fCool_energy = 0.

c Define hour of the year (from 8760 hours) for the simulation during
c the current time step
         ihour_year = ihrp + 24 * (idyp - 1)

c     At this point the total heating or cooling that needs to be supplied by
c     the HVAC system is determined. Depending on the type of the HVAC system,
c     the appropriate subroutine is called to determine the energy consumption.

c     If the primary heating system is a furnace
         if((ihvac_type(i).eq.1).and.(ipriority(i).eq.1)) then
            call FURNACE(i,heating_load,sys_energy(i),
     &                   pilot_energy(i),plr_furnace,plf_furnace)
            fP_pilot_energy = pilot_energy(i)

c     Energy consumption of the circulation fan in J
            fan_energy(i) = FAN_ENERGY_CON(i,plr_furnace,plr_backup,
     &                      plf_furnace,plf_backup)

            fPPLR = plr_furnace
            fHeat_Energy = sys_energy(i)


c     If the draft fan power of the furnace is greater than 0, then the
c     fan energy consumption during the time step is calculated. Draft
c     fan power can be greater than zero only for induced draft and
c     condensing furnaces.
            if(draft_fan_power(i).gt.0.) then
               draft_energy(i) = DRAFT_FAN_ENERGY(i,draft_fan_power(i),
     &              plr_furnace,plf_furnace)
            endif

C Determine average electrical draws (W) by draft fan and circulating fan
C and record them for use by the fuel cell controller.
          FCctl_furnace_draft   = draft_energy(i) / time_step
          FCctl_furnace_circfan = fan_energy(i)   / time_step

C.........Store H3KReports data
          fH3KStorePLRH(i)       = fPPLR
          fH3KStoreFanEH(i)      = fan_energy(i)    / time_step
          fH3KStoreDraftE(i)     = draft_energy(i)  / time_step
          fH3KStorePrimaryEH(i)  = sys_energy(i)    / time_step
          fH3KStorePilotE(i)     = pilot_energy(i)  / time_step
          fH3KStoreOutputH(i)    = ss_capacity(i) * fH3KStorePLRH(i)
          if ( fH3KStorePrimaryEH(i) > 1.0 .or.
     &         fH3KStorePilotE(i)    > 1.0      ) then
            fH3KStoreEfficiencyH(i)=
     &         ( fH3KStoreOutputH(i) ) /
     &         ( fH3KStorePrimaryEH(i) + fH3KStorePilotE(i) )
          endif


c     If the primary heating system is a boiler
c     The model for a boiler is the same as that for a furnace
         elseif((ihvac_type(i).eq.2).and.(ipriority(i).eq.1)) then
            call FURNACE(i,heating_load,sys_energy(i),
     &           pilot_energy(i),plr_boiler,plf_boiler)

c     Energy consumption of the circulation fan in J
            fan_energy(i) = FAN_ENERGY_CON(i,plr_boiler,plr_backup,
     &                      plf_boiler,plf_backup)

            fPPLR = plr_boiler
            fP_pilot_energy = pilot_energy(i)
            fHeat_Energy = sys_energy(i)

c     If the draft fan power of the boiler is greater than 0, then the
c     fan energy consumption during the time step is calculated. Draft
c     fan power can be greater than zero only for induced draft and
c     condensing boilers.
            if(draft_fan_power(i).gt.0.) then
               draft_energy(i) = DRAFT_FAN_ENERGY(i,draft_fan_power(i),
     &              plr_boiler,plf_boiler)
            endif

C Determine average electrical draws (W) by draft fan and circulating fan
C and record them for use by the fuel cell controller. Note that circulating
C fan does not really make sense.
            FCctl_boiler_draft   = draft_energy(i) / time_step
            FCctl_boiler_circfan = fan_energy(i)   / time_step

C.........Store H3KReports data
          fH3KStorePLRH(i)       = fPPLR
          fH3KStoreFanEH(i)      = fan_energy(i)    / time_step
          fH3KStoreDraftE(i)     = draft_energy(i)  / time_step
          fH3KStorePrimaryEH(i)  = sys_energy(i)    / time_step
          fH3KStorePilotE(i)     = pilot_energy(i)  / time_step
          fH3KStoreOutputH(i)    = ss_capacity(i) * fH3KStorePLRH(i)
          if ( fH3KStorePrimaryEH(i) > 1.0 ) then
            fH3KStoreEfficiencyH(i)= fH3KStoreOutputH(i)
     &                                 / fH3KStorePrimaryEH(i)
          endif

c     If the primary heating system is a Baseboard/Hydronic/Plenum Heater
         elseif((ihvac_type(i).eq.3).and.(ipriority(i).eq.1)) then
            call BASEBOARD(i,heating_load,sys_energy(i),plr_baseboard,
     &                     plf_baseboard)

c     Energy consumption of the circulation fan in J
            fan_energy(i) = FAN_ENERGY_CON(i,plr_baseboard,plr_backup,
     &                      plf_baseboard,plf_backup)

            fPPLR = plr_baseboard
            fP_pilot_energy = 0.
            fHeat_Energy = sys_energy(i)

C Determine average electrical draws (W) by circulating fan
C and record for use by the fuel cell controller.
            FCctl_baseboard_fan = fan_energy(i)   / time_step

C.........Store H3KReports data
          fH3KStorePLRH(i)       = fPPLR
          fH3KStoreFanEH(i)      = fan_energy(i)    / time_step
          fH3KStorePrimaryEH(i)  = sys_energy(i)    / time_step
          fH3KStoreOutputH(i)    = ss_capacity(i) * fH3KStorePLRH(i)
          if ( fH3KStorePrimaryEH(i) > 1.0 ) then
            fH3KStoreEfficiencyH(i)= fH3KStoreOutputH(i)
     &                                  / fH3KStorePrimaryEH(i)
          endif

c     If the primary heating system is an air-source heat pump
c     It is confirmed here that the air-source heat pump
c     is a primary system
         elseif((ihvac_type(i).eq.7).and.(iunit_function(i).eq.1)
     &           .and.(ipriority(i).eq.1)) then

c     Call the air-source heat pump subroutine

            call ASHP_HEATING(i,heating_load,sys_energy(i),
     &           plr_heat_pump,plr_backup,plf_heat_pump)
            fPPLR = plr_heat_pump
            fHeat_Energy = sys_energy(i)
c     In case the backup heat source is a furnace and there is a load on the backup
c     source

            if((plr_backup.gt.0.0001).and.
     &           (ibackup_heat(i).eq.1)) then
               call FURNACE(ibackup_sys_num(i),heating_load,
     &              backup_energy(i),pilot_energy(i),plr_backup,
     &              plf_backup)

               fBPLR = plr_backup
               fB_pilot_energy = pilot_energy(i)

c     If the furnace draft fan power is > 0, then calculate the draft fan power
c     consumption for the time step.
               if(draft_fan_power(ibackup_sys_num(i)).gt.0.) then
                  draft_energy(i) =
     &                 DRAFT_FAN_ENERGY(i,
     &                 draft_fan_power(ibackup_sys_num(i)),plr_backup,
     &                                 plf_backup)
C     Record electric draw of backup furnace draft fan for use by fuel
C     cell controller.
                  FCctl_furnace_draft = draft_energy(i) / time_step
               endif

c     In case the backup heat source is a boiler. The model for a boiler is the
c     same as that for a furnace
            elseif((plr_backup.gt.0.0001).and.
     &              (ibackup_heat(i).eq.2)) then
               call FURNACE(ibackup_sys_num(i),heating_load,
     &              backup_energy(i),pilot_energy(i),plr_backup,
     &              plf_backup)

               fBPLR = plr_backup
               fB_pilot_energy = pilot_energy(i)

c     If the boiler draft fan power is > 0, then calculate the draft fan power
c     consumption for the time step.
               if(draft_fan_power(ibackup_sys_num(i)).gt.0.) then
                  draft_energy(i) =
     &                 DRAFT_FAN_ENERGY(i,
     &                 draft_fan_power(ibackup_sys_num(i)),plr_backup,
     &                 plf_backup)
C     Record electric draw of backup boiler draft fan for use by fuel
C     cell controller.
                  FCctl_boiler_draft = draft_energy(i) / time_step
               endif

c     In case the backup heat source is Baseboards/Plenum Heaters/Hydronic
            elseif((plr_backup.gt.0.0001).and.
     &              (ibackup_heat(i).eq.3)) then
               call BASEBOARD(ibackup_sys_num(i),heating_load,
     &              backup_energy(i),plr_backup,plf_backup)
               fBPLR = plr_backup
               fB_pilot_energy = 0.
            endif

c     System circulation fan energy consumption in J
            fan_energy(i) = FAN_ENERGY_CON(i,plr_heat_pump,plr_backup,
     &                      plf_heat_pump,plf_backup)


C Determine average electrical draws (W) of air source heat pump operating
C in heating mode and record for use by the fuel cell controller.
            FCctl_ashp_compressor = sys_energy(i) / time_step
            FCctl_ashp_circfan = fan_energy(i) / time_step

C.........Store H3KReports data: Primary system...


          fH3KStorePLRH(i)       = fPPLR
          fH3KStoreFanEH(i)      = fan_energy(i)    / time_step
          fH3KStorePrimaryEH(i)  = sys_energy(i)    / time_step
          fH3KStoreOutputH(i)    = fH3KActualCapacity(i)
     &                                          * fH3KStorePLRH(i)
          if ( fH3KStorePrimaryEH(i) > 1. ) then
            fH3KStoreCOPH(i)       = fH3KStoreOutputH(i)
     &                                   / fH3KStorePrimaryEH(i)
          endif

C......... ...and backup
          ASHP_backup_energy_report:
     &    if ( ibackup_heat(i) == 1 .or.
     &         ibackup_heat(i) == 2 .or.
     &         ibackup_heat(i) == 3      ) then

C...........Get system number
            iBackup = ibackup_sys_num(i)
C...........And store data ( back-up system's fan energy is aggregated
C...........into main system's fan energy)
            fH3KStorePLRH(iBackup)    = fBPLR
            fH3KStoreDraftE(iBackup) = draft_energy(iBackup)
     &                                            / time_step
            fH3KStorePrimaryEH(iBackup) = backup_energy(i)
     &                                            / time_step
            fH3KStorePilotE(iBackup) = pilot_energy(iBackup)
     &                                            / time_step
            fH3KStoreOutputH(iBackup)  = ss_capacity(iBackup) * fBPLR
            if ( fH3KStorePrimaryEH(iBackup) > 1.0 .or.
     &           fH3KStorePilotE(iBackup)   > 1.0     ) then
              fH3KStoreEfficiencyH(iBackup)= fH3KStoreOutputH(iBackup) /
     &         (fH3KStorePrimaryEH(iBackup) + fH3KStorePilotE(iBackup))
            endif
          endif ASHP_backup_energy_report

c GSHP
c For a ground-source heat pump system in the heating mode, first call the
c subroutine to calculate the correlation coefficients for the GSHP in heating
c mode. Once GSHP_HEAT_COEFF is called, the same ASHP heating routines are used -
c for both the primary system and the backup systems.

         elseif((ihvac_type(i).eq.8).and.(iunit_function(i).eq.1)
     &         .and.(ipriority(i).eq.1)) then


c If the HVAC system under consideration (do 10 loop) is a GSHP, then
c call to the GSHP_load subroutine to sum the daily heating and cooling
c loads. These daily loads are needed by the ground heat exchanger to
c determine the entering water temperature.
           CALL GSHP_load(heating_load,cooling_load)

c Call the air-source heat pump subroutine
           call GSHP_HEAT_COEFF(i)
           call ASHP_HEATING(i,heating_load,sys_energy(i),
     &                      plr_heat_pump,plr_backup,plf_heat_pump)

           fPPLR = plr_heat_pump
           fHeat_Energy = sys_energy(i)

c The amount of pumping power required for the circulation of the fluid
c through the ground loop and into the heat pump must be calculated.
           heat_pump_cap = ss_capacity(i)
           GSHP_pump_energy = GSHP_pump_power(heat_pump_cap)


c In case the backup heat source is a furnace and there is a load on the backup
c source

           if((plr_backup.gt.0.0001).and.
     &       (ibackup_heat(i).eq.1)) then
             call FURNACE(ibackup_sys_num(i),heating_load,
     &                    backup_energy(i),pilot_energy(i),plr_backup,
     &                    plf_backup)
             fBPLR = plr_backup
             fB_pilot_energy = pilot_energy(i)
c If the furnace draft fan power is > 0, then calculate the draft fan power
c consumption for the time step.
             if(draft_fan_power(ibackup_sys_num(i)).gt.0.) then
                draft_energy(i) =
     &                   DRAFT_FAN_ENERGY(i,
     &                   draft_fan_power(ibackup_sys_num(i)),plr_backup,
     &                   plf_backup)
C Record electric draw of backup furnace for use by fuel cell controller
                FCctl_furnace_draft = draft_energy(i) / time_step
             endif

c In case the backup heat source is a boiler. The model for a boiler is the
c same as that for a furnace
           elseif((plr_backup.gt.0.0001).and.
     &           (ibackup_heat(i).eq.2)) then
             call FURNACE(ibackup_sys_num(i),heating_load,
     &       backup_energy(i),pilot_energy(i),plr_backup,plf_backup)
             fBPLR = plr_backup
             fB_pilot_energy = pilot_energy(i)
c If the boiler draft fan power is > 0, then calculate the draft fan power
c consumption for the time step.
             if(draft_fan_power(ibackup_sys_num(i)).gt.0.) then
                draft_energy(i) =
     &                   DRAFT_FAN_ENERGY(i,
     &                   draft_fan_power(ibackup_sys_num(i)),plr_backup,
     &                   plf_backup)
C Record electric draw of backup boiler for use by fuel cell controller
                FCctl_boiler_draft = draft_energy(i) / time_step
             endif

c In case the backup heat source is Baseboards/Plenum Heaters/Hydronic
           elseif((plr_backup.gt.0.0001).and.
     &           (ibackup_heat(i).eq.3)) then
             call BASEBOARD(ibackup_sys_num(i),heating_load,
     &       backup_energy(i),plr_backup,plf_backup)
             fBPLR = plr_backup
             fB_pilot_energy = 0.
           endif
c System circulation fan energy consumption in J
           fan_energy(i) = FAN_ENERGY_CON(i,plr_heat_pump,plr_backup,
     &                     plf_heat_pump,plf_backup)

C Record electric draw of circulating fan, pump and compressor
c for use by fuel cell controller
           FCctl_gshp_pump = GSHP_pump_energy / time_step
           FCctl_ashp_compressor = sys_energy(i) / time_step
           FCctl_ashp_circfan = fan_energy(i) / time_step

C.........Store H3KReports data: Primary system...

          fH3KStorePLRH(i)       = fPPLR
          fH3KStoreFanEH(i)      = fan_energy(i)    / time_step
          fH3KStorePrimaryEH(i)     = sys_energy(i)    / time_step
          fH3KStorePumpEH(i)     = GSHP_pump_energy / time_step
          fH3KStoreOutputH(i)    = fH3KActualCapacity(i)
     &                                            * fH3KStorePLRH(i)
          if ( fH3KStorePrimaryEH(i) > 1.0 ) then
            fH3KStoreCOPH(i)       = fH3KStoreOutputH(i)
     &                                / fH3KStorePrimaryEH(i)
          endif


C......... ...and backup
          GSHP_backup_energy_report:
     &    if ( ibackup_heat(i) == 1 .or.
     &         ibackup_heat(i) == 2 .or.
     &         ibackup_heat(i) == 3      ) then

C...........Get system number
            iBackup = ibackup_sys_num(i)
C...........And store data ( back-up system's fan energy is aggregated
C...........into main system's fan energy)
            fH3KStorePLRH(iBackup)     = fBPLR
            fH3KStoreDraftE(iBackup) = draft_energy(iBackup)
     &                                            / time_step
            fH3KStorePrimaryEH(iBackup) = backup_energy(i)
     &                                            / time_step
            fH3KStorePilotE(iBackup) = pilot_energy(iBackup)
     &                                            / time_step
            fH3KStoreOutputH(iBackup)  = ss_capacity(iBackup) * fBPLR
            if ( fH3KStorePrimaryEH(iBackup) > 1.0 .or.
     &           fH3KStorePilotE(iBackup)   > 1.0     ) then
              fH3KStoreEfficiencyH(iBackup)= fH3KStoreOutputH(iBackup) /
     &          (fH3KStorePrimaryEH(iBackup) + fH3KStorePilotE(iBackup))
            endif
          endif GSHP_backup_energy_report



c GSHP: end

c GCEP: begin
c For a ground-source heat pump system in the heating mode, first call the
c subroutine to calculate the correlation coefficients for the GCEP in heating
c mode (same as for the GSHP systems).
c Once GSHP_HEAT_COEFF is called, the same ASHP heating routines are used -
c for both the primary system and the backup systems.

         elseif((ihvac_type(i).eq.9).and.(iunit_function(i).eq.1)
     &         .and.(ipriority(i).eq.1)) then

c For a ground-source heat pump system, call the GCEP_load subroutine
c to average the load to the ground over an hour
c the ground load is the minimum between the load and the heat pump capacity
c adjusted with the COP
           CALL GCEP_load(heating_load,cooling_load,
     &          heat_pump_cap,heat_pump_cooling_cap)

c Call the air-source heat pump subroutine
           call GCEP_HEAT_COEFF(i)
           call ASHP_HEATING(i,heating_load,sys_energy(i),
     &                      plr_heat_pump,plr_backup,plf_heat_pump)

           fPPLR = plr_heat_pump
           fHeat_Energy = sys_energy(i)
c The amount of pumping power required for the circulation of the fluid
c through the ground loop and into the heat pump must be calculated.
           heat_pump_cap = ss_capacity(i)
           GCEP_pump_energy = GCEP_pump_power(heat_pump_cap)

c In case the backup heat source is a furnace and there is a load on the backup
c source

           if((plr_backup.gt.0.0001).and.
     &       (ibackup_heat(i).eq.1)) then
             call FURNACE(ibackup_sys_num(i),heating_load,
     &                    backup_energy(i),pilot_energy(i),plr_backup,
     &                    plf_backup)
             fBPLR = plr_backup
             fB_pilot_energy = pilot_energy(i)
c If the furnace draft fan power is > 0, then calculate the draft fan power
c consumption for the time step.
             if(draft_fan_power(ibackup_sys_num(i)).gt.0.) then
                draft_energy(i) =
     &                   DRAFT_FAN_ENERGY(i,
     &                   draft_fan_power(ibackup_sys_num(i)),plr_backup,
     &                   plf_backup)
             endif

c In case the backup heat source is a boiler. The model for a boiler is the
c same as that for a furnace
           elseif((plr_backup.gt.0.0001).and.
     &           (ibackup_heat(i).eq.2)) then
             call FURNACE(ibackup_sys_num(i),heating_load,
     &       backup_energy(i),pilot_energy(i),plr_backup,plf_backup)
             fBPLR = plr_backup
             fB_pilot_energy = pilot_energy(i)
c If the boiler draft fan power is > 0, then calculate the draft fan power
c consumption for the time step.
             if(draft_fan_power(ibackup_sys_num(i)).gt.0.) then
                draft_energy(i) =
     &                   DRAFT_FAN_ENERGY(i,
     &                   draft_fan_power(ibackup_sys_num(i)),plr_backup,
     &                   plf_backup)
             endif

c In case the backup heat source is Baseboards/Plenum Heaters/Hydronic
           elseif((plr_backup.gt.0.0001).and.
     &           (ibackup_heat(i).eq.3)) then
             call BASEBOARD(ibackup_sys_num(i),heating_load,
     &       backup_energy(i),plr_backup,plf_backup)
             fBPLR = plr_backup
             fB_pilot_energy = 0.
           endif
c System circulation fan energy consumption in J
           fan_energy(i) = FAN_ENERGY_CON(i,plr_heat_pump,plr_backup,
     &                     plf_heat_pump,plf_backup)

C.........Store H3KReports data: Primary system...

          fH3KStorePLRH(i)       = fPPLR
          fH3KStoreFanEH(i)      = fan_energy(i)    / time_step
          fH3KStorePrimaryEH(i)  = sys_energy(i)    / time_step
          fH3KStorePumpEH(i)     = GCEP_pump_energy / time_step
          fH3KStoreOutputH(i)    = fH3KActualCapacity(i)
     &                                            * fH3KStorePLRH(i)
          if ( fH3KStorePrimaryEH(i) > 1.0 ) then
            fH3KStoreCOPH(i)     = fH3KStoreOutputH(i)
     &                              / fH3KStorePrimaryEH(i)
          endif
C......... ...and backup
          GCEP_backup_energy_report:
     &    if ( ibackup_heat(i) == 1 .or.
     &         ibackup_heat(i) == 2 .or.
     &         ibackup_heat(i) == 3      ) then

C...........Get system number
            iBackup = ibackup_sys_num(i)
C...........And store data ( back-up system's fan energy is aggregated
C...........into main system's fan energy)
            fH3KStorePLRH(iBackup)    = fBPLR
            fH3KStoreDraftE(iBackup) = draft_energy(iBackup)
     &                                            / time_step
            fH3KStorePrimaryEH(iBackup) = backup_energy(i)
     &                                            / time_step
            fH3KStorePilotE(iBackup) = pilot_energy(iBackup)
     &                                            / time_step
            fH3KStoreOutputH(iBackup)  = ss_capacity(iBackup) * fBPLR
            if ( fH3KStorePrimaryEH(iBackup) > 1.0 .or.
     &           fH3KStorePilotE(iBackup)   > 1.0     ) then
              fH3KStoreEfficiencyH(iBackup)= fH3KStoreOutputH(iBackup) /
     &          (fH3KStorePrimaryEH(iBackup) + fH3KStorePilotE(iBackup))
            endif
          endif GCEP_backup_energy_report


c GCEP: end

c     For an air-source heat pump system in the cooling mode, call the
c     cooling mode simulation subroutine
         elseif((ihvac_type(i).eq.7).and.(iunit_function(i).eq.2)
     &           .and.(ipriority(i).eq.1)) then

c Is there a space sensible cooling load or ventilation load for hvac system i?
          call eclose(cooling_load,0.0,1.e-6,closea)
          call eclose(outdoor_air_flow(i,ihour_year),0.0,1.e-6,closeb)
          if((.not.(closea)).or.(.not.(closeb))) then
            cooling_vent_yes = .true.
          else
            cooling_vent_yes = .false.
          endif

          call ASHP_COOLING(i,cooling_load,cooling_vent_yes,
     &         sys_energy(i),plr_heat_pump,plf_heat_pump)

c System circulation fan energy consumption in J
          fan_energy(i) = FAN_ENERGY_CON(i,plr_heat_pump,plr_backup,
     &                    plf_heat_pump,plf_backup)
          fPPLR = plr_heat_pump
          fCool_Energy = sys_energy(i)

C Determine average electrical draws (W) by A/C unit (compressor and
C condensor fan) and circulating fan and record them for use by the
C fuel cell controller.
          FCctl_AC_compressor = sys_energy(i) / time_step
          FCctl_AC_circfan    = fan_energy(i) / time_step

C.........Store H3KReports data: Primary system...

          fH3KStorePLRC(i)       = fPPLR
          fH3KStoreFanEC(i)      = fan_energy(i)    / time_step
          fH3KStorePrimaryEC(i)  = sys_energy(i)    / time_step
          fH3KStoreOutputC(i)    = fH3KActualCapacity(i)
     &                                     * fH3KStorePLRC(i)
          if ( fH3KStorePrimaryEC(i) > 1.0 ) then
            fH3KStoreCOPC(i)       = fH3KStoreOutputC(i)
     &                                / fH3KStorePrimaryEC(i)
          endif
c GSHP: begin
c For a ground-source heat pump system in cooling mode, first call the
c subroutine to calculate the correlation coefficients for the GSHP in cooling
c mode. Once GSHP_COOL_COEFF is called, the same ASHP cooling routines are used -
c for both the primary system and the backup systems.
         elseif((ihvac_type(i).eq.8).and.(iunit_function(i).eq.2)
     &   .and.(ipriority(i).eq.1)) then
c Is there a space sensible cooling load or ventilation load for hvac system i?
            call eclose(cooling_load,0.0,1.e-6,closea)
            call eclose(outdoor_air_flow(i,ihour_year),0.0,1.e-6,closeb)
            if((.not.(closea)).or.(.not.(closeb))) then
              cooling_vent_yes = .true.
            else
              cooling_vent_yes = .false.
            endif

            call GSHP_COOL_COEFF(i)
            call ASHP_COOLING(i,cooling_load,cooling_vent_yes,
     &           sys_energy(i),plr_heat_pump,plf_heat_pump)

c System circulation fan energy consumption in J
            fan_energy(i) = FAN_ENERGY_CON(i,plr_heat_pump,plr_backup,
     &                      plf_heat_pump,plf_backup)
            fPPLR = plr_heat_pump
            fCool_Energy = sys_energy(i)
c In addition, the amount of pumping power required for the
c circulation of the fluid through the ground loop and into the heat pump
c must be calculated.
            heat_pump_cooling_cap = ss_capacity(i)
            GSHP_pump_energy = GSHP_pump_power(heat_pump_cooling_cap)

C.........Store H3KReports data: Primary system...

          fH3KStorePLRC(i)       = fPPLR
          fH3KStoreFanEC(i)      = fan_energy(i)    / time_step
          fH3KStorePrimaryEC(i)  = sys_energy(i)    / time_step
          fH3KStoreOutputC(i)    = fH3KActualCapacity(i)
     &         * fH3KStorePLRC(i)
          if ( fH3KStorePrimaryEC(i) > 1.0 ) then
            fH3KStoreCOPC(i)       = fH3KStoreOutputC(i)
     &                                / fH3KStorePrimaryEC(i)
          endif
c GSHP: end

C Determine average electrical draws (W) by A/C unit (compressor and
C condensor fan) and circulating fan and record them for use by the
C fuel cell controller.
          FCctl_AC_compressor = sys_energy(i) / time_step
          FCctl_AC_circfan    = fan_energy(i) / time_step
C Determine electrical draw (W) of pump circulating fluid through
C the ground loop and into the heat pump
          FCctl_gshp_pump = GSHP_pump_energy / time_step

c GCEP: begin
c For a ground-source heat pump system in cooling mode, first call the
c subroutine to calculate the correlation coefficients for the GSHP in cooling
c mode. Once GSHP_COOL_COEFF is called, the same ASHP cooling routines are used -
c for both the primary system and the backup systems.
         elseif((ihvac_type(i).eq.9).and.(iunit_function(i).eq.2)
     &   .and.(ipriority(i).eq.1)) then
c Is there a space sensible cooling load or ventilation load for hvac system i?
            call eclose(cooling_load,0.0,1.e-6,closea)
            call eclose(outdoor_air_flow(i,ihour_year),0.0,1.e-6,closeb)
            if((.not.(closea)).or.(.not.(closeb))) then
              cooling_vent_yes = .true.
            else
              cooling_vent_yes = .false.
            endif

            call GCEP_COOL_COEFF(i)
            call ASHP_COOLING(i,cooling_load,cooling_vent_yes,
     &           sys_energy(i),plr_heat_pump,plf_heat_pump)

c System circulation fan energy consumption in J
            fan_energy(i) = FAN_ENERGY_CON(i,plr_heat_pump,plr_backup,
     &                      plf_heat_pump,plf_backup)
            fPPLR = plr_heat_pump
            fCool_Energy = sys_energy(i)
c In addition, the amount of pumping power required for the
c circulation of the fluid through the ground loop and into the heat pump
c must be calculated.
            heat_pump_cooling_cap = ss_capacity(i)
            GCEP_pump_energy = GCEP_pump_power(heat_pump_cooling_cap)

C...........Store H3KReports data: Primary system...

            fH3KStorePLRC(i)       = fPPLR
            fH3KStoreFanEC(i)      = fan_energy(i)    / time_step
            fH3KStorePrimaryEC(i)     = sys_energy(i)    / time_step
            fH3KStorePumpEC(i)     = GCEP_pump_energy / time_step
            fH3KStoreOutputC(i)    = fH3KActualCapacity(i)
     &                                    * fH3KStorePLRC(i)
            if ( fH3KStorePrimaryEC(i) > 1.0 ) then
              fH3KStoreCOPC(i)       = fH3KStoreOutputC(i)
     &                                / fH3KStorePrimaryEC(i)
            endif
c GCEP: end

c     If the hvac system is not a primary system (This can be a backup
c     furnace for an air-source heat pump system), the simulation is
c     skipped here as it is taken care of when simulating the primary
c     system performance
         elseif(ipriority(i).eq.2) then
            goto 10
         endif

c     Set the current month and day of the month using the day of the
c     year (integer between 1 and 365)
         if(idyp.le.31) then
            month = 1
            iday = idyp
         elseif((idyp.gt.31).and.(idyp.le.59)) then
            month = 2
            iday = idyp - 31
         elseif((idyp.gt.59).and.(idyp.le.90)) then
            month = 3
            iday = idyp - 59
         elseif((idyp.gt.90).and.(idyp.le.120)) then
            month = 4
            iday = idyp - 90
         elseif((idyp.gt.120).and.(idyp.le.151)) then
            month = 5
            iday = idyp - 120
         elseif((idyp.gt.151).and.(idyp.le.181)) then
            month = 6
            iday = idyp - 151
         elseif((idyp.gt.181).and.(idyp.le.212)) then
            month = 7
            iday = idyp - 181
         elseif((idyp.gt.212).and.(idyp.le.243)) then
            month = 8
            iday = idyp - 212
         elseif((idyp.gt.243).and.(idyp.le.273)) then
            month = 9
            iday = idyp - 243
         elseif((idyp.gt.273).and.(idyp.le.304)) then
            month = 10
            iday = idyp - 273
         elseif((idyp.gt.304).and.(idyp.le.334)) then
            month = 11
            iday = idyp - 304
         elseif((idyp.gt.334).and.(idyp.le.365)) then
            month = 12
            iday = idyp - 334
         endif

C     This is causing problems (see notes in cetc/hvac_output.F).
C     This is to be activated only for study of output from cooling
C     model
C     variable output_flag is used to start writing output to the hvac
C     file only after the start up period has elapsed. This seems to work
C     well when the simulation period starts at day 1. Otherwise there is
C     no output written to the file.
C      if(output_flag) then
C         write(202,100) month,iday,ihrp,i,heating_load,
C     &    cooling_load,sys_energy(i),backup_energy(i),
C     &    fan_energy(i),draft_energy(i),pilot_energy(i)
C       endif

C     Stores primary HVAC  energy data
         call H3KSTORE_HVAC_SUBSYS_ENERGY(
C     i is the system number of the primary HVAC system.
     &        i,
C     fheat_energy is the heat energy used during this timestep by the primary system
C     in Joules
     &        fheat_energy,
C     fcool_energy is the cooling energy used during this timestep by this primary system
C     in Joules
     &        fcool_energy,
C     fP_pilot_energy is the pilot energy used in Joule by the primary HVAC
C     system.
     &        fP_pilot_energy,
C     fPPLR is the Part load ratio of the primary hvac system.
     &        fPPLR)

C     Stores backup HVAC energy data
      if ( ibackup_sys_num (i) .ne. 0 ) then
         call H3KSTORE_HVAC_SUBSYS_ENERGY(
C     ibackup_sys_num(i) give the backup system number for the primary
C     system "i"
     &        ibackup_sys_num(i),
C     This is the energy used by the backup heating system during the timestep.
     &        backup_energy(i),
c     No backup cooling so hard coded to zero.
     &        0.,
C     fB_pilot_energy is the pilot energy used in Joules for the Backup
C     HVAC system.
     &        fB_pilot_energy,
C     fPPLR is the Part load ratio of the backup hvac system.
     &        fbPLR)
c     Stores fan energy, Heating and cooling loads.

         call H3KSTORE_HVAC_Packaged_DATA(
C     stores the heating and cooling loads for the timestep in Joules.
     &        Heating_load,
     &        Cooling_load,
C     determines the fan energy used in the primary HVAC system (the backup
C     uses the same fan.) in Joules.
     &        fan_energy(i),
     &        GSHP_pump_energy,
     &        GCEP_pump_energy)
        endif
C-----------------------------------------------------------------------
C       For linked systems (such as the cooling component of an GSHP),
C       store cooling energy use in linked system arrays
C-----------------------------------------------------------------------

        store_linked: if ( bLinked (i) ) then

          iCoolSys = iLinkedSystem(i)

          fH3KStorePLRC(iCoolSys)          =   fH3KStorePLRC(i)
          fH3KStoreFanEC(iCoolSys)         =   fH3KStoreFanEC(i)
          fH3KStorePrimaryEC(iCoolSys)     =   fH3KStorePrimaryEC(i)
          fH3KStorePumpEC(iCoolSys)        =   fH3KStorePumpEC(i)
          fH3KStoreEfficiencyC(iCoolSys)   =   fH3KStoreEfficiencyC(i)
          fH3KStoreOutputC(iCoolSys)       =   fH3KStoreOutputC(i)
          fH3KStoreCOPC(iCoolSys)          =   fH3KStoreCOPC(i)

        endif store_linked


  10  continue


C-----------------------------------------------------------------------
C     Loop through HVAC systems, and report data to H3Kreports.
C     Note: this code must be kept apart from the main HVAC system loop
C           (above) because the latter solves systems according to
C           their primary/backup status (as opposed to sequentially)
C-----------------------------------------------------------------------

C.....Set fan energy to zero
      fH3KSumFanEnergy = 0.

      HVAC_system_report_loop: do i = 1, num_hvac_sys

C.......Check if system is cooling portion of heating heatpump system,
C.......and if so, suppress system report.
        suppress_this_system:
     &  if (.not. bSuppressThisSystem(i) ) then
C.......Add fan power to running total
          fH3KSumFanEnergy = fH3KSumFanEnergy + fH3KStoreFanEC(i)
     &                                        + fH3KStoreFanEH(i)

C.......Prepare component name:
        if ( i .ge. 10 ) write (cHVACIndex, '(I2)') i
        if ( i .lt. 10 ) write (cHVACIndex, '(A,I1)') '0', i

C.......If this system is linked, link data

C.......Report system fuel use. Depends on whether it's electric,
C.......or not.

        if ( ifuel_type(i) == iElectricity ) then

C.........All energy is electric
          Call AddToReport(rvPltHvacComFuelAmnt%Identifier,
     &      fConvertEnergyToFuelAmount( fH3KStorePrimaryEH(i)
     &                                   + fH3KStorePrimaryEC(i)
     &                                   + fH3KStorePumpEH(i)
     &                                   + fH3KStorePumpEC(i)
     &                                   + fH3KStoreDraftE(i),
     &                                  ifuel_type(i) ),
     &      cHVACIndex // '_'
     &      // cHVACTypes(ihvac_type(i))(1:iHVACTypeLen(ihvac_type(i))),
     &      cFuelName(ifuel_type(i))(1:iFuelNameLen(ifuel_type(i))))

          Call AddToReportDetails(rvPltHvacComFuelAmnt%Identifier,
     &      cHVACIndex // '_'
     &      // cHVACTypes(ihvac_type(i))(1:iHVACTypeLen(ihvac_type(i))),
     &      cFuelName(ifuel_type(i))(1:iFuelNameLen(ifuel_type(i))),
     &      'units',
     &      cFuelUnit(ifuel_type(i))(1:iFuelUnitLen(ifuel_type(i))),
     &      'Idealized HVAC models: '
     &      // 'Amount of '
     &      // cFuelDesc(ifuel_type(i))(1:iFuelDescLen(ifuel_type(i)))
     &      // ' used by '
     &      // cHVACTypes(ihvac_type(i))(1:iHVACTypeLen(ihvac_type(i)))
     &      // ' system')

        else

C.........Heating energy reported by fuel type
          Call AddToReport(rvPltHvacComFuelAmnt%Identifier,
     &      fConvertEnergyToFuelAmount( fH3KStorePrimaryEH(i)
     &                                   + fH3KStorePilotE(i),
     &                                  ifuel_type(i) ),
     &      cHVACIndex // '_'
     &      // cHVACTypes(ihvac_type(i))(1:iHVACTypeLen(ihvac_type(i))),
     &      cFuelName(ifuel_type(i))(1:iFuelNameLen(ifuel_type(i))))

          Call AddToReportDetails(rvPltHvacComFuelAmnt%Identifier,
     &      cHVACIndex // '_'
     &      // cHVACTypes(ihvac_type(i))(1:iHVACTypeLen(ihvac_type(i))),
     &      cFuelName(ifuel_type(i))(1:iFuelNameLen(ifuel_type(i))),
     &     'units',
     &     cFuelUnit(ifuel_type(i))(1:iFuelUnitLen(ifuel_type(i))),
     &      'Idealized HVAC models: '
     &      // 'Amount of '
     &      // cFuelDesc(ifuel_type(i))(1:iFuelDescLen(ifuel_type(i)))
     &      // ' used by '
     &      // cHVACTypes(ihvac_type(i))(1:iHVACTypeLen(ihvac_type(i)))
     &      // ' system')


C.........Auxiliary energy consumption is electric
          call AddToReport(rvPltHvacComFuelAmnt%Identifier,
     &      fConvertEnergyToFuelAmount( fH3KStorePumpEH(i)
     &                                   + fH3KStorePumpEC(i)
     &                                   + fH3KStoreDraftE(i),
     &                                  iElectricity ),
     &      cHVACIndex // '_'
     &      // cHVACTypes(ihvac_type(i))(1:iHVACTypeLen(ihvac_type(i))),
     &      cFuelName(iElectricity)(1:iFuelNameLen(iElectricity)))

          Call AddToReportDetails(rvPltHvacComFuelAmnt%Identifier,
     &      cHVACIndex // '_'
     &      // cHVACTypes(ihvac_type(i))(1:iHVACTypeLen(ihvac_type(i))),
     &      cFuelName(iElectricity)(1:iFuelNameLen(iElectricity)),
     &      'units',
     &      cFuelUnit(iElectricity)(1:iFuelUnitLen(iElectricity)),
     &      'Idealized HVAC models: '
     &      // 'Amount of '
     &      // cFuelDesc(iElectricity)(1:iFuelDescLen(iElectricity))
     &      // ' used by '
     &      // cHVACTypes(ihvac_type(i))(1:iHVACTypeLen(ihvac_type(i)))
     &      // ' system')
        endif


C.......Determine if system is heating or cooling, and
C.......report type-specific data
        H3K_system_select: select case ( ihvac_type(i) )

C.........Heating systems
          case ( iSysFurnace, iSysBoiler, iSysBaseboard)

C...........Set end-use flag
            iSysEndUse = iUseSpaceHeat

C...........Test if pilot light actually exists.
            does_pilot_exist:
     &      if (  ( ifuel_type(i) == iNaturalGas .or.
     &              ifuel_type(i) == iOil        .or.
     &              ifuel_type(i) == iPropane        ) .and.
     &            ( ihvac_type(i) == iSysFurnace .or.
     &              ihvac_type(i) == iSysBoiler      ) .and.
     &            ( fH3KStorePilotE(i) > 0.1  )
     &          ) then

             Call AddToReport(rvPltHvacFuelEnPilot%Identifier,
     &         fH3KStorePilotE(i),
     &         cHVACIndex // '_'
     &      // cHVACTypes(ihvac_type(i))(1:iHVACTypeLen(ihvac_type(i))))

             Call AddToReportDetails(
     &         rvPltHvacFuelEnPilot%Identifier,
     &         cHVACIndex // '_'
     &      // cHVACTypes(ihvac_type(i))(1:iHVACTypeLen(ihvac_type(i))),
     &         'units',
     &         '(W)',
     &         'Idealized HVAC models: Energy used by '
     &      // cHVACTypes(ihvac_type(i))(1:iHVACTypeLen(ihvac_type(i)))
     &      // ' system pilot light')

            endif does_pilot_exist

C...........Efficiency:
            if ( fH3KStorePrimaryEH(i) > 1.0 ) then
            Call AddToReport(rvPltHvacEff%Identifier,
     &       fH3KStoreEfficiencyH(i),
     &       cHVACIndex // '_'
     &    // cHVACTypes(ihvac_type(i))(1:iHVACTypeLen(ihvac_type(i))))

            Call AddToReportDetails(rvPltHvacEff%Identifier,
     &          cHVACIndex // '_'
     &    // cHVACTypes(ihvac_type(i))(1:iHVACTypeLen(ihvac_type(i))),
     &       'units','(-)',
     &       'Idealized HVAC models: Efficiency of '
     &    // cHVACTypes(ihvac_type(i))(1:iHVACTypeLen(ihvac_type(i)))
     &    // ' system (thermal output / HHV Fuel input)')
             endif

C.........P.10 --- presently unsupported
          case ( iSysIMS_P10 )

C.........Heat-pumps.
          case ( iSysASHP, iSysGSHP, iSysGSHP_GCEP )

C...........Heat pumps can heat or cool. Test 'unit_function'
            H3K_function_select: select case ( iUnit_Function(i) )

              case ( iASHP_Heating )

C..............Set end-use flag
               iSysEndUse = iUseSpaceHeat

              case ( iASHP_Cooling )

C..............Set end-use flag
               iSysEndUse = iUseSpaceCool
          end select H3K_function_select

C.........COP:
          if ( fH3KStorePrimaryEH(i) > 1.0 ) then
            Call AddToReport(rvPltHvacCOPHeat%Identifier,
     &       fH3KStoreCOPH(i),
     &       cHVACIndex // '_'
     &    // cHVACTypes(ihvac_type(i))(1:iHVACTypeLen(ihvac_type(i))))

            Call AddToReportDetails(rvPltHvacCOPHeat%Identifier,
     &       cHVACIndex // '_'
     &    // cHVACTypes(ihvac_type(i))(1:iHVACTypeLen(ihvac_type(i))),
     &       'units','(-)',
     &       'Idealized HVAC models: '
     &    // 'Heating coefficient-of-performance of  '
     &    // cHVACTypes(ihvac_type(i))(1:iHVACTypeLen(ihvac_type(i)))
     &    // ' system (thermal output / electricity input)')
          endif

C.........COP:
          if ( fH3KStorePrimaryEC(i) > 1.0 ) then
            Call AddToReport(rvPltHvacCOPCool%Identifier,
     &       fH3KStoreCOPC(i),
     &       cHVACIndex // '_'
     &    // cHVACTypes(ihvac_type(i))(1:iHVACTypeLen(ihvac_type(i))))

            Call AddToReportDetails(rvPltHvacCOPCool%Identifier,
     &       cHVACIndex // '_'
     &    // cHVACTypes(ihvac_type(i))(1:iHVACTypeLen(ihvac_type(i))),
     &       'units','(-)',
     &       'Idealized HVAC models: '
     &    // 'Cooling coefficient-of-performance of  '
     &    // cHVACTypes(ihvac_type(i))(1:iHVACTypeLen(ihvac_type(i)))
     &    // ' system (thermal output / electricity input)')
          endif

        end select H3K_system_select

C.......Report energy use as heating or cooling end-use

        if ( bTwoModes(i) .or. iSysEndUse == iUseSpaceHeat ) then
          Call AddToReport(rvPltHvacFuelEnHeat%Identifier,
     &      fH3KStorePrimaryEH(i),
     &      cHVACIndex // '_'
     &   // cHVACTypes(ihvac_type(i))(1:iHVACTypeLen(ihvac_type(i))))

          Call AddToReportDetails(rvPltHvacFuelEnHeat%Identifier,
     &      cHVACIndex // '_'
     &   // cHVACTypes(ihvac_type(i))(1:iHVACTypeLen(ihvac_type(i))),
     &      'units', '(W)',
     &      'Idealized HVAC models: Primary energy used by '
     &   // cHVACTypes(ihvac_type(i))(1:iHVACTypeLen(ihvac_type(i)))
     &   // ' system for heating')


          System_is_on_H:
     &    if ( fH3KStorePrimaryEH(i) > 1.0 ) then
           Call AddToReport(rvPltHvacPrtLdHeat%Identifier,
     &      fH3KStorePLRH(i),
     &      cHVACIndex // '_'
     &   // cHVACTypes(ihvac_type(i))(1:iHVACTypeLen(ihvac_type(i))))

           Call AddToReportDetails(rvPltHvacPrtLdHeat%Identifier,
     &      cHVACIndex // '_'
     &   // cHVACTypes(ihvac_type(i))(1:iHVACTypeLen(ihvac_type(i))),
     &      'units', '(-)',
     &      'Idealized HVAC models: '
     &   // cHVACTypes(ihvac_type(i))(1:iHVACTypeLen(ihvac_type(i)))
     &   // ' heating part-load-ratio (PLR)')
          endif System_is_on_H

          Call AddToReport(rvPltHvacThrmOutHeat%Identifier,
     &      fH3KStoreOutputH(i),
     &      cHVACIndex // '_'
     &   // cHVACTypes(ihvac_type(i))(1:iHVACTypeLen(ihvac_type(i))))

          Call AddToReportDetails(rvPltHvacThrmOutHeat%Identifier,
     &      cHVACIndex // '_'
     &   // cHVACTypes(ihvac_type(i))(1:iHVACTypeLen(ihvac_type(i))),
     &      'units','(W)',
     &      'Idealized HVAC models: '
     &   // cHVACTypes(ihvac_type(i))(1:iHVACTypeLen(ihvac_type(i)))
     &   // ' heating thermal output')
        endif

        if ( bTwoModes(i) .or. iSysEndUse == iUseSpaceCool ) then


C...............Energy consumption for cooling
          Call AddToReport(rvPltHvacFuelEnCool%Identifier,
     &      fH3KStorePrimaryEC(i),
     &      cHVACIndex // '_'
     &   // cHVACTypes(ihvac_type(i))(1:iHVACTypeLen(ihvac_type(i))))

          Call AddToReportDetails(rvPltHvacFuelEnCool%Identifier,
     &      cHVACIndex
     &   // '_'
     &   // cHVACTypes(ihvac_type(i))(1:iHVACTypeLen(ihvac_type(i))),
     &      'units', '(W)',
     &      'Idealized HVAC models: Primary energy used by '
     &   // cHVACTypes(ihvac_type(i))(1:iHVACTypeLen(ihvac_type(i)))
     &   // ' system for cooling')

          System_is_on_C:
     &    if ( fH3KStorePrimaryEC(i) > 1.0 ) then
           Call AddToReport(rvPltHvacPrtCool%Identifier,
     &       fH3KStorePLRC(i),
     &       cHVACIndex // '_'
     &    // cHVACTypes(ihvac_type(i))(1:iHVACTypeLen(ihvac_type(i))))

           Call AddToReportDetails(rvPltHvacPrtCool%Identifier,
     &       cHVACIndex // '_'
     &    // cHVACTypes(ihvac_type(i))(1:iHVACTypeLen(ihvac_type(i))),
     &       'units', '(-)',
     &       'Idealized HVAC models: '
     &    // cHVACTypes(ihvac_type(i))(1:iHVACTypeLen(ihvac_type(i)))
     &    // ' cooling part-load-ratio (PLR)')
          endif System_is_on_C


          Call AddToReport(rvPltHvacThrmCool%Identifier,
     &      fH3KStoreOutputC(i),
     &      cHVACIndex // '_'
     &   // cHVACTypes(ihvac_type(i))(1:iHVACTypeLen(ihvac_type(i))))

          Call AddToReportDetails(rvPltHvacThrmCool%Identifier,
     &      cHVACIndex // '_'
     &   // cHVACTypes(ihvac_type(i))(1:iHVACTypeLen(ihvac_type(i))),
     &      'units','(W)',
     &      'Idealized HVAC models: '
     &   // cHVACTypes(ihvac_type(i))(1:iHVACTypeLen(ihvac_type(i)))
     &   // ' cooling thermal output')


          Call AddToReport(rvPltHvacFuelEnTtl%Identifier,
     &      fH3KStorePrimaryEH(i) + fH3KStorePrimaryEC(i),
     &      cHVACIndex // '_'
     &   // cHVACTypes(ihvac_type(i))(1:iHVACTypeLen(ihvac_type(i))))

          Call AddToReportDetails(rvPltHvacFuelEnTtl%Identifier,
     &      cHVACIndex // '_'
     &   // cHVACTypes(ihvac_type(i))(1:iHVACTypeLen(ihvac_type(i))),
     &      'units','(W)',
     &      'Idealized HVAC models: Primary energy used by '
     &   // cHVACTypes(ihvac_type(i))(1:iHVACTypeLen(ihvac_type(i)))
     &   // ' system for both heating and cooling')


C.......If component is heat pump with cooling mode, report
C.......interesting parameters from cooling simulation
        if ( bTwoModes(i) ) then

          iCoolSys = iLinkedSystem(i)

        else

          iCoolSys = i

        endif


C.......Report interesting parameters from
C.......ASHP cooling simulation: Coil loads
        Call AddToReport(rvPltHvacCoilCoolSns%Identifier,
     &    fASHP_AC_Coil_Load_Sensible(iCoolSys),
     &    cHVACIndex // '_'
     & // cHVACTypes(ihvac_type(iCoolSys))
     &            (1:iHVACTypeLen(ihvac_type(iCoolSys))))

        Call AddToReportDetails(rvPltHvacCoilCoolSns%Identifier,
     &    cHVACIndex // '_'
     & // cHVACTypes(ihvac_type(iCoolSys))
     &            (1:iHVACTypeLen(ihvac_type(iCoolSys))),
     &    'units', '(W)',
     &    'Idealized HVAC models: '
     & // cHVACTypes(ihvac_type(iCoolSys))
     &               (1:iHVACTypeLen(ihvac_type(iCoolSys)))
     & // ' sensible cooling load at coil')


        Call AddToReport(rvPltHvacCoilCoolLtnt%Identifier,
     &   fASHP_AC_Coil_Load_Latent(iCoolSys),
     &   cHVACIndex // '_'
     & // cHVACTypes(ihvac_type(iCoolSys))
     &               (1:iHVACTypeLen(ihvac_type(iCoolSys))))

        Call AddToReportDetails(rvPltHvacCoilCoolLtnt%Identifier,
     &   cHVACIndex // '_'
     & // cHVACTypes(ihvac_type(iCoolSys))
     &               (1:iHVACTypeLen(ihvac_type(iCoolSys))),
     &   'units', '(W)',
     &   'Idealized HVAC models: '
     & // cHVACTypes(ihvac_type(iCoolSys))
     &               (1:iHVACTypeLen(ihvac_type(iCoolSys)))
     & // ' latent cooling load at coil')


        Call AddToReport(rvPltHvacCoilCoolTtl%Identifier,
     &   fASHP_AC_Coil_Load_Total(iCoolSys),
     &   cHVACIndex // '_'
     & // cHVACTypes(ihvac_type(iCoolSys))
     &               (1:iHVACTypeLen(ihvac_type(iCoolSys))))

        Call AddToReportDetails(rvPltHvacCoilCoolTtl%Identifier,
     &   cHVACIndex // '_'
     & // cHVACTypes(ihvac_type(iCoolSys))
     &               (1:iHVACTypeLen(ihvac_type(iCoolSys))),
     &   'units', '(W)',
     &  'Idealized HVAC models: '
     &  // cHVACTypes(ihvac_type(iCoolSys))
     &               (1:iHVACTypeLen(ihvac_type(iCoolSys)))
     &  // ' total cooling load at coil')

        endif


C-------------------------------------------------------------------------
C       Report energy use by fuel type, and store for aggregation
C       with site-wide fuel use.
C-------------------------------------------------------------------------
        H3K_fuel_select: select case ( ifuel_type(i) )

          case ( iSoftWood )


            fH3KSumEnergyUseSoftWood( iSysEndUse )
     &                   =    fH3KSumEnergyUseSoftWood( iSysEndUse )
     &                      + fH3KStorePrimaryEH(i)
     &                      + fH3KStorePilotE(i)

          case ( iHardWood )

            fH3KSumEnergyUseHardWood( iSysEndUse )
     &                   =    fH3KSumEnergyUseHardWood( iSysEndUse )
     &                      + fH3KStorePrimaryEH(i)
     &                      + fH3KStorePilotE(i)

          case ( iMixedWood )

            fH3KSumEnergyUseMixWood( iSysEndUse )
     &                     =    fH3KSumEnergyUseMixWood( iSysEndUse )
     &                        + fH3KStorePrimaryEH(i)
     &                        + fH3KStorePilotE(i)

          case ( iWoodPellets )

            fH3KSumEnergyUsePellets( iSysEndUse )
     &                        =    fH3KSumEnergyUsePellets( iSysEndUse )
     &                           + fH3KStorePrimaryEH(i)
     &                           + fH3KStorePilotE(i)

          case ( iOil )

           fH3KSumEnergyUseOil( iSysEndUse )
     &                          =    fH3KSumEnergyUseOil( iSysEndUse )
     &                             + fH3KStorePrimaryEH(i)
     &                             + fH3KStorePilotE(i)

          case ( iNaturalGas )

           fH3KSumEnergyUseGas( iSysEndUse )
     &                          =    fH3KSumEnergyUseGas( iSysEndUse )
     &                             + fH3KStorePrimaryEH(i)
     &                             + fH3KStorePilotE(i)

          case ( iPropane )

           fH3KSumEnergyUsePropane( iSysEndUse )
     &                     =    fH3KSumEnergyUsePropane( iSysEndUse )
     &                        + fH3KStorePrimaryEH(i)
     &                        + fH3KStorePilotE(i)

          case ( iElectricity )

           fH3KSumEnergyUseElec( iUseSpaceHeat )
     &                      =    fH3KSumEnergyUseElec( iUseSpaceHeat )
     &                         + fH3KStorePrimaryEH(i)

           fH3KSumEnergyUseElec( iUseSpaceCool )
     &                      =    fH3KSumEnergyUseElec( iUseSpaceCool )
     &                         + fH3KStorePrimaryEC(i)


        end select H3K_fuel_select

C.......Append auxilary enery use (if any) to electric total:
        fH3KSumEnergyUseElec( iUseSpaceHeat )
     &                      =    fH3KSumEnergyUseElec( iUseSpaceHeat )
     &                         + fH3KStoreDraftE(i)
     &                         + fH3KStorePumpEH(i)

        fH3KSumEnergyUseElec( iUseSpaceCool )
     &                      =    fH3KSumEnergyUseElec( iUseSpaceCool )
     &                         + fH3KStorePumpEC(i)

        endif suppress_this_system

      enddo HVAC_system_report_loop

C Store fan electricity use in circulation/ventilation (W)
      fH3KSumEnergyUseElec( iUseHRV ) = fH3KSumFanEnergy

C Report fan power consumption and heat gains

C.......Fan energy
        Call AddToReport(rvPltHvacCrcFuelEnIn%Identifier,
     &   fH3KSumFanEnergy)

        Call AddToReport(rvPltHvacCrcFElecAmnt%Identifier,
     &   fConvertEnergyToFuelAmount( fH3KSumFanEnergy, iElectricity ))

        Call AddToReport(rvPltHvacCrcHeatTrn%Identifier,
     &   fH3KSumFanEnergy)

C Store aggregate fuel/electricity use for reporting in Site Utility facilitity.
      report_use_loop: do iSysEndUse = 1, iNumUses

        fSUFuelEnergyUse(iElectricity,iSysEndUse)
     &                   = fH3KSumEnergyUseElec(iSysEndUse)

        fSUFuelEnergyUse(iOil,iSysEndUse)
     &                   = fH3KSumEnergyUseOil(iSysEndUse)

        fSUFuelEnergyUse(iPropane,iSysEndUse)
     &                   = fH3KSumEnergyUsePropane(iSysEndUse)

        fSUFuelEnergyUse(iNaturalGas,iSysEndUse)
     &                   = fH3KSumEnergyUseGas(iSysEndUse)

        fSUFuelEnergyUse(iHardWood,iSysEndUse)
     &                   = fH3KSumEnergyUseHardWood(iSysEndUse)

        fSUFuelEnergyUse(iSoftWood,iSysEndUse)
     &                   = fH3KSumEnergyUseSoftWood(iSysEndUse)

        fSUFuelEnergyUse(iMixedWood,iSysEndUse)
     &                   = fH3KSumEnergyUseMixWood(iSysEndUse)

        fSUFuelEnergyUse(iWoodPellets,iSysEndUse)
     &                   = fH3KSumEnergyUsePellets(iSysEndUse)


      enddo report_use_loop

C     Pass fuel energy stroage data to site utilities
      call StoreSiteEnergyUse(iIdealHvac, fSUFuelEnergyUse )

c     In case we are at the end of simulation period, output the total number
c     of hours the space is under heated
      if(((idyp.eq.isf).or.(idyp.eq.isf-365)).and.(ihrp.eq.24)) then

C     This is causing problems (see notes in cetc/hvac_output.F).
C     do 30 i = 1,num_hvac_sys
C
C     if(ihrs_under_heat(i).gt.0) then
C     write(202,*) "Spaces associated with hvac system", i,
C     &      " are under conditioned for a total of",
C     &ihrs_under_heat(i),"hours"
C     endif
C
C     30    continue

      endif

 100  format(i2,3x,i2,3x,i2,3x,i2,3x,e15.6,3x,e15.6,3x,e15.6,3x,
     &     e15.6,3x,e16.5,3x,e16.5,3x,e16.5)

      return
      end

c     *********************************************************************
c     *********************************************************************
c     PLR

c     Created by: Kamel Haddad
c     Initial Creation Date: July 18th 2000
c     Copyright 2000: NRCan Buildings Group
c
c     Function to determine the part-load ratio of the hvac system based on
c     the mode of operation of the indoor fan of the air handler

c     References:  HOT2000 Technical Manual, Version 6, Canadian Home Builders
c     Association, August 1991

c     Furnace Model for Implementation in H3K, H3K model documentation,
c     Kamel Haddad, April 5th 2000

c     INPUTS:
c     fan_power indoor fan power in W
c     ss_cap    capacity of the hvac unit in W
c     req_load  load on the hvac equipment in W
c     ifan_operation  mode of operation of the indoor circulation fan
c     =0 no circulation fan
c     =1 fan in auto mode
c     =2 fan in continuous mode

c     OUTPUTS:
c     PLR     estimate of the part-load ratio on the equipment.
c     plr     is the ratio of the load on the system to the
c     steay-state capacity of the equipment. The capacity
c     accounts for the contribution of the circulation fan
c     **********************************************************************
      REAL FUNCTION PLR(req_load,ss_cap,fan_power,ifan_operation)

      IMPLICIT NONE

      REAL req_load,ss_cap,fan_power
      INTEGER ifan_operation

c     When the circulation fan is in continuous mode
      if(ifan_operation.eq.2) then
c     This is Equation 3 in Furnace Ref by Haddad
         PLR = req_load / ss_cap
c     When the fan is in auto-mode
      elseif(ifan_operation.eq.1) then
c     This is Equation 4 in Furnace Ref by Haddad
         PLR = req_load / (ss_cap + fan_power)
c     When there is no circulation fan
      elseif(ifan_operation.eq.0) then
c     This is Equation 3 in Furnace Ref by Haddad
         PLR = req_load / ss_cap
      endif

      return
      end

C-----------------------------------------------------------------------
C     Definition of system name types
C-----------------------------------------------------------------------

      blockdata initialize_system_types

      common/HVAC_H3kNames/iHVACTypeLen,cHVACTypes
      character*23 cHVACTypes(9)
      integer iHVACTypeLen(9)

      integer iName

      data (cHVACTypes(iName),iName=1,9) /'furnace                ',
     &                                    'boiler                 ',
     &                                    'baseboard              ',
     &                                    'IMS                    ',
     &                                    '                       ',
     &                                    '                       ',
     &                                    'air_source_heat_pump   ',
     &                                    'ground_source_heat_pump',
     &                                    'ground_source_heat_pump' /

      data iHVACTypeLen /7,6,9,3,1,1,20,23,23/

      end blockdata initialize_system_types




