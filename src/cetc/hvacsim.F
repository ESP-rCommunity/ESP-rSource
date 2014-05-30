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
c	START_HVAC_SIM
c		   start the simulation of HVAC systems performance by
c		   calling HVACSIM
c	HVACSIM    control the simulation of the performance of the hvac
c		   system
c 	PLR	   determine the part-load ratio of the hvac equipment
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
c     iday		day corresponding to the current time step
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
c     pvala 	        common for the present time step loads
c     fvala 	        common for the future time step loads
c     GCEP_INPUT        common for parameters for GCEP
c     hvacinfo          common for hvac simulation flag

c     OUTPUTS:
c     HVAC_OUTPUT	common for system energy consumption for time
c     FC_ctl_h3khvacloads
c                       common for electrical draws of hvac systems
c     step
c     *********************************************************************
      SUBROUTINE HVACSIM
      use h3kmodule
      IMPLICIT NONE

      include "hvac_parameters.h"
      
      include "building.h"
      
      include "control.h"
      
      include "hvac_common.h"
      
      include "ashp_common.h"
      
      include "SOFC.h"
      
      include "gcep_common.h"
      
      include "OffsiteUtilitiesPublic.h"
      
      include "IMS_common.h"

      include "Ventilation_Data.h"


c hvac system file and flag:  	ihvacflag=1 there is hvac file
c ihvacflag=0 no hvac file
      common/hvacinfo/ihvacflag,hvacfile

      INTEGER ihvacflag
      CHARACTER*72 hvacfile
      CHARACTER*2, cHVACIndex
c Use common for HVAC system energy consumption
      common/HVAC_OUTPUT/fan_energy(max_sys),
     &draft_energy(max_sys), sys_energy(max_sys),
     &backup_energy(max_sys), pilot_energy(max_sys),
     &pump_energy(max_sys) 

c Real variables in HVAC system energy consumption common
      REAL fan_energy, draft_energy, sys_energy, backup_energy,
     &pilot_energy, pump_energy

      real est_fan_energy

C Combined ventilation and circulation fan PLR (0->1)
      real fCirc_and_Vent_PLR, fFanTotPLR
      
C Max power for fan-control-loop (W) 
      real fFanMaxPower_hi, fFanMaxPower_lo, fFanPower, fSysFanPower
      real fFanPowerVent
      
      real fFanEffectivePLR
      common/FanCtl/fFanEffectivePLR 
      
C Number of zones (ncomp)
      common/c1/ncomp,ncon
      integer ncomp, ncon

    


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
      common/cctl/icascf(mcom)  ! Zone control type
      integer icascf

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

      real fPLR(max_sys), fPLF(max_sys) 
      real fPLR_backup(max_sys), fPLF_backup(max_sys)
      real fBackupHeatingLoad(max_sys)
      
c Local real variables
      REAL plr_furnace, plr_backup, plr_heat_pump, plr_boiler,
     &plr_baseboard, plf_backup, plf_heat_pump, plf_furnace,
     &plf_boiler, plf_baseboard, heating_load, cooling_load,
     &req_load, time_step, FAN_ENERGY_CON, DRAFT_FAN_ENERGY,
     &heat_pump_cap, GSHP_pump_power, GCEP_pump_power,
     &heat_pump_cooling_cap

c Local integer variables

      INTEGER i, j, month, iday, icomp, iCoolSys, ihour_year,iZoneIndex
      integer iFanLoop, iNumSystemsOnLoop

c Logical variable for whether there is space cooling load or ventilation load
      LOGICAL cooling_vent_yes,closea,closeb

C Function for picking up fan power for IMS
      real fIMS_Interpolate
        
      
C IMS variables
      real fRepIMS_SpaceHLoad        ! Space heating losad (W)
      real fRepIMS_WaterHLoad        ! Water heating load (W)
      real fRepIMS_WaterHDuration    ! Duration of water heating load (s)
      real fRepIMS_FuelUseRate       ! Fuel use (kg/s)
      real fRepIMS_ElecUseRate       ! Electricity use (W)
      real fRepIMS_FanUse            ! Fan power consumption (W)
      real fRepIMS_VentUse           ! Ventilation power consumption (W)
      real fRepIMS_VentFlowRate      ! IMS ventilation flow (kg/s)
      real fRepIMS_VentLoad          ! Thermal load associated with vent. (W)
      real fRepIMS_AddGains          ! Heat added/removed from zones
                                     !   by IMS fans/systems/ventilation (W)
      real fRepIMS_VaporChange       ! Moisture balance assoc. w/ IMS (kg/s)

      real fRepIMS_FanPower
      real fRepIMS_AuxPower
      real fRepIMS_VentPower
      real fRepIMS_TS_fan_fraction 
      real fRepIMS_WaterDraw
      
      
C.....Parameters used by IMS model to recovery water draws. 
      real fDHW_WaterDraw_byVolume
      real bLoadIsDefined
      real fTempHWT
      real fTempUsage 
      real fTempColdSupply
      
      
      
C.....H3Kreports.(HVAC_ELEC_DEC).start......................
C.....DECLARATIONS for H3Kreporting object
      CHARACTER*128 H3K_rep_NAME
      CHARACTER*12  H3K_format
      
      CHARACTER*16 HVAC_STRING
      CHARACTER*128 xmlHeat_Energy
      CHARACTER*128 xmlCool_Energy
      CHARACTER*128 xmlPilot_Energy
      CHARACTER*128 xmlPLR
      
      REAL H3K_ASHP_elec
      REAL H3K_GCEP_elec
      REAL H3K_GSHP_elec
      REAL H3K_fan_elec
      REAL H3K_HRV_elec
      REAL H3K_Current_elec
      INTEGER LNBLNK
C.....H3Kreports.(HVAC_ELEC_DEC).end........................


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

      real fPLR_back_temp
      real fPLF_back_temp
      
      
      
      
C.....variables for computing fan power with fan control loop.s       
      integer iPLRRows, iThisRow       
      integer iPLRResolution 
      parameter (iPLRResolution = 10) 
      real fFanSHCPower(iPLRResolution)       
      real fFanPowerSysIncremental(max_sys) 
      logical bFanCont
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


        logical bDebug
        
        bDebug = .false. 

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


C     Zero gain storage arrays.
      ZoneZero: do iZoneIndex= 1, ncomp
C       Set zone gain flag to false.
        bIdealHVACZoneGain(iZoneIndex) = .false.

C       Zero gain storage arrays
        fIdealHVACZoneConGain ( iZoneIndex ) = 0.0
        fIdealHVACZoneRadGain ( iZoneIndex ) = 0.0
        fIdealHVACZoneLatGain ( iZoneIndex ) = 0.0

      enddo ZoneZero


c     Initalize storage arrays for all HVAC systems 
      HVAC_sys_loop_init : do i = 1,num_hvac_sys

C     (Re)Initialize idealized HVAC system - zone gain coupling
C     to zero, and (re)set gain storage arrys to zero.
         bIdealHVACSysGain( i ) = .false. 

         fIdealHVACSysAddGain( i ) = 0.0
         fIdealHVACSysConGainFrac( i ) = 0.0
         fIdealHVACSysRadGainFrac( i ) = 0.0
         fIdealHVACSysLatGainFrac( i ) = 0.0


C.....H3Kreports.(HVAC_ELEC_CURR_INIT).start......................
C.....INITIALIZATION for H3Kreporting object
         H3K_Current_elec      = 0. 
C.....H3Kreports.(HVAC_ELEC_CURR_INIT).end........................


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
         fPPLR = 0.
c     Backup system PLR for H3K save level 5
         fBPLR = 0.

C     Backup system load 
         fBackupHeatingLoad (i) = 0. 
         
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
C     Pump energy 
          pump_energy(i)  = 0.

C     Partload factors 
          fPLR(i) = 0.
          fPLF(i) = 1.
          fPLR_backup(i) = 0.
          fPLF_backup(i) = 1.
          
c Define hour of the year (from 8760 hours) for the simulation during
c the current time step
         ihour_year = ihrp + 24 * (idyp - 1)

      enddo HVAC_sys_loop_init   
        
C     For each of the primary HVAC systems, call the appropriate subroutine to 
C     determine the energy consumption of the system for the current time 
C     step. Before the specific system subroutine is called, the total heating 
C     or cooling load on the HVAC system is determined. This is the sum of all 
C     the heating or the cooling loads during the current time step for all 
C     the zones being served by the HVAC system. 

      
      HVAC_primary_sys_loop: do i=1, num_hvac_sys
      
      
C.......Characterize primary systems first. Back-up systems handled in 
C.......A second loop below.       
        IfPrimary: if ( ipriority(i) .eq. 1 ) then  
c         Determine the total heating or cooling requirement of the zones
c         associated with the HVAC system i. This load is passed on to the
c         equipment simulation subroutine to determine the energy cosumption.
          heating_load = 0.
          cooling_load = 0.   

c     The load for the current time step associated with the hvac system considered is
c     the sum of the loads for the zones served by the hvac system. The load for each
c     of the zones is the average of the load at the present and future time steps, qpa
c     and qfa respectively
          ZoneLoads: do j=1,num_sys_zones(i)
            if ( iban(icascf( isys_zone_num(i,j)),2) .gt. 0 ) then
C...........PIN is within construction

               req_load =( QPC(isys_zone_num(i,j)) +
     &              QFC((isys_zone_num(i,j)) )) / 2.0

            else
               req_load = 0.5 * (qfa(isys_zone_num(i,j)) +
     &              qpa(isys_zone_num(i,j)))
                req_load = qfa(isys_zone_num(i,j))
            endif

            if(req_load.gt.0.) then

               zone_call_heat(i,j) = .true.
               heating_load = heating_load + req_load

            elseif(req_load.lt.0.) then

               zone_call_cool(i,j) = .true.
               cooling_load = cooling_load + abs(req_load)

            endif
          enddo ZoneLoads
          
          if ( bDebug) write (*,*) " HVAC module: system : ", i
          if ( bDebug) write (*,*) 
     &    "              calling characterization code for type ",
     &    iHVAC_type(i), iUnit_Function(i), ipriority(i)
          if ( bDebug) write (*,*) 
     &    "              Heating Load: ", heating_load
          if ( bDebug) write (*,*) 
     &    "              Cooling Load: ", cooling_load

          
     
     
C.........At this point the total heating or cooling that needs to be supplied by
C.........the HVAC system is determined. Depending on the type of the HVAC system,
C.........the appropriate subroutine is called to determine the energy consumption.          
          PrimarySelect: select case ( iHVAC_type (i) )

C-----------------------------------------------------------------------                        
C           Combustion-based heating systems (including furnaces, 
C           boiler-based forced air combo systems, and hydronic 
C           boiler systems) 
C-----------------------------------------------------------------------                               
            case (iSysFurnace,iSysBoiler) 
              if ( bDebug) write (*,*) 
     &    "              Calling Furnace code... "

              call FURNACE(i,heating_load,sys_energy(i),
     &                     pilot_energy(i),fPLR(i),fPLF(i) )
          if ( bDebug) write (*,*) 
     &    "              Done.  ", sys_energy(i), 
     &                             pilot_energy(i),fPLR(i),fPLF(i)




C...........If the draft fan power of the furnace is greater than 0, then the
C...........fan energy consumption during the time step is calculated. Draft
C...........f an power can be greater than zero only for induced draft and
C...........condensing furnaces.
            if(draft_fan_power(i).gt.0.) then
               draft_energy(i) = DRAFT_FAN_ENERGY(i,draft_fan_power(i),
     &              fPLR(i),fPLF(i))
     
            endif
    

C-----------------------------------------------------------------------                        
C           'Baseboard' systems (electic, and includes electric resisitnce
C            heaters in forced air plenums and in hydronic networks 
C            such as back-up systems for heat pumps). 
C-----------------------------------------------------------------------                        
            case (iSysBaseboard)

            call BASEBOARD(i,heating_load,sys_energy(i),fPLR(i),
     &                                                  fPLF(i) )


C-----------------------------------------------------------------------                        
C           Air-source heat pump (including cold-climate variety) 
C           Heating or cooling.
C-----------------------------------------------------------------------                        
            case (iSysASHP)
            
              ASHPHeatOrCool: 
     &        if ( iunit_function(i) == iFuncHeating ) then
c     Call the air-source heat pump subroutine
                call ASHP_HEATING(i,heating_load,sys_energy(i),
     &                            fPLR(i), fPLR_backup(i), fPLF(i) ) 
            
              else
C...............Cooling case 
C...............Is there a space sensible cooling load or 
C...............ventilation load for hvac system i?
                call eclose(cooling_load,0.0,1.e-6,closea)
                call eclose(outdoor_air_flow(i,ihour_year),
     &                      0.0,1.e-6,closeb)
                if((.not.(closea)).or.(.not.(closeb))) then
                  cooling_vent_yes = .true.
                else
                  cooling_vent_yes = .false.
                endif
                
                call ASHP_COOLING(i,cooling_load,cooling_vent_yes,
     &                            sys_energy(i),fPLR(i),fPLF(i) )

                    
              endif ASHPHeatOrCool

              
C-----------------------------------------------------------------------                        
C           Ground source HP: heating or cooling 
C-----------------------------------------------------------------------                                   
            case (iSysGSHP) 

              GSHPHeatOrCool: 
     &        if ( iunit_function(i) == iFuncHeating ) then

              if (bDebug) write (*,*) "                > GSHP / heat"

C.............If the HVAC system under consideration (do 10 loop) is a GSHP, then
C.............call to the GSHP_load subroutine to sum the daily heating and cooling
C.............loads. These daily loads are needed by the ground heat exchanger to
C.............determine the entering water temperature.
                CALL GSHP_load(heating_load,cooling_load)

C...............Call the air-source heat pump subroutine
                call GSHP_HEAT_COEFF(i)
                call ASHP_HEATING(i,heating_load,sys_energy(i),
     &                            fPLR(i), fPLR_backup(i), fPLF(i) ) 


              
C...............The amount of pumping power required for the circulation of the fluid
C...............through the ground loop and into the heat pump must be calculated.
                heat_pump_cap = ss_capacity(i)
                
                if ( sys_energy(i) > 1 ) then 
                    pump_energy(i)= GSHP_pump_power(heat_pump_cap,i)
                else 
                    pump_energy(i) = 0.
                endif 
               
              else
              
              
              if (bDebug) write (*,*) "                > GSHP / cool"
C...............For a ground-source heat pump system in cooling mode, first call the
C...............subroutine to calculate the correlation coefficients for the GSHP in cooling
C...............mode. Once GSHP_COOL_COEFF is called, the same ASHP cooling routines are used -
C...............for both the primary system and the backup systems.
            
C...............Is there a space sensible cooling load or ventilation load for hvac system i?
                call eclose(cooling_load,0.0,1.e-6,closea)
                call eclose(outdoor_air_flow(i,ihour_year),
     &                      0.0,1.e-6,closeb)
                if((.not.(closea)).or.(.not.(closeb))) then
                  cooling_vent_yes = .true.
                else
                  cooling_vent_yes = .false.
                endif

 
              
                if ( bDebug ) 
     &             write (*,*) "                > call cool-coef "  
                call GSHP_COOL_COEFF(i)
                if ( bDebug ) 
     &             write (*,*) "                > call cool-cooling "                  
                call ASHP_COOLING(i,cooling_load,cooling_vent_yes,
     &                            sys_energy(i),fPLR(i),fPLF(i) )

                heat_pump_cap = ss_capacity(i)
                if ( sys_energy(i) > 1 ) then 
                    pump_energy(i)= GSHP_pump_power(heat_pump_cap,i)
                else 
                    pump_energy(i) = 0.
                endif 
                 

              endif GSHPHeatOrCool

                if ( bDebug ) then 
                 write (*,*) "                > finish cool", i   
                 write (*,*) "                > load:",cooling_load
                 write (*,*) "                > ener:",sys_energy(i)
     &                             /3600.," (W)"
                 write (*,*) "                > COP: ",cooling_load/
     &                                  (sys_energy(i)/3600.+0.0001)
                endif 
             
C-----------------------------------------------------------------------                        
C           GCEP varient of  ground source HP: heating or cooling 
C-----------------------------------------------------------------------                                    
            case (iSysGSHP_GCEP)
            
            
              GCEPHeatOrCool: 
     &        if ( iunit_function(i) == iFuncHeating ) then 

C...............For a ground-source heat pump system, call the GCEP_load subroutine
C...............to average the load to the ground over an hour
C...............the ground load is the minimum between the load and the heat pump capacity
C...............adjusted with the COP
                CALL GCEP_load(heating_load,cooling_load,
     &                         heat_pump_cap,heat_pump_cooling_cap)

C...............Call the air-source heat pump subroutine
                call GCEP_HEAT_COEFF(i)
                call ASHP_HEATING(i,heating_load,sys_energy(i),
     &                        fPLR(i), fPLR_backup(i), fPLF(i) ) 



C...............The amount of pumping power required for the circulation of the fluid
C...............through the ground loop and into the heat pump must be calculated.
                heat_pump_cap  = ss_capacity(i)
                pump_energy(i) = GCEP_pump_power(heat_pump_cap)

!                print*, " GCEP heating call, ", i 
!                print*, "  - capH ", heat_pump_cap
!                print*, "  - capC ", heat_pump_cooling_cap
!                print*, "  - loadH", heating_load
!                print*, "  - loadC", cooling_load             
!                print*, "  - syse ", sys_energy(i) 
!                print*, "  - plr  ", fPLR(i)
!                print*, "  - plf  ", fPLF(i)
!                print*, "  - plrba", fPLR_backup(i) 
!                print*, "  - pump ", pump_energy(i)


              else
C...............GCEP variation on ground source HP
C...............For a ground-source heat pump system in cooling mode, first call the
C...............subroutine to calculate the correlation coefficients for the GSHP in cooling
C...............mode. Once GSHP_COOL_COEFF is called, the same ASHP cooling routines are used -
C...............for both the primary system and the backup systems.

C............... Is there a space sensible cooling load or ventilation load for hvac system i?
                 call eclose(cooling_load,0.0,1.e-6,closea)
                 call eclose(outdoor_air_flow(i,ihour_year),
     &                       0.0,1.e-6,closeb)
                 if((.not.(closea)).or.(.not.(closeb))) then
                   cooling_vent_yes = .true.
                 else
                   cooling_vent_yes = .false.
                 endif
                 
                 call GCEP_COOL_COEFF(i)
                 call ASHP_COOLING(i,cooling_load,cooling_vent_yes,
     &                            sys_energy(i),fPLR(i),fPLF(i) )

                heat_pump_cap  = ss_capacity(i)
                pump_energy(i) = GCEP_pump_power(heat_pump_cap)
              
!                print*, " GCEP cooling call, ", i 
!                print*, "  - capH ", heat_pump_cap
!                print*, "  - capC ", heat_pump_cooling_cap
!                print*, "  - loadH", heating_load
!                print*, "  - loadC", cooling_load
!                print*, "  - CVY  ", cooling_vent_yes                  
!                print*, "  - syse ", sys_energy(i) 
!                print*, "  - plr  ", fPLR(i)
!                print*, "  - plf  ", fPLF(i) 
!                print*, "  - pump ", pump_energy(i) 



              endif GCEPHeatOrCool            
            
C-----------------------------------------------------------------------                        
C           IMS in heating mode 
C-----------------------------------------------------------------------                        
            case (iSysIMS_P10) 
            
            
C.............Collect space and water heating loads.
              fRepIMS_SpaceHLoad     = heating_load  ! W

              if (bDebug ) then 
                write(*,*) "reported IMS DHW VOLUME: ", 
     &           fIMS_DHW_Water_volume_LpDay(i)
              endif 

C.............Get water volume for this hour. 
              fTempHWT = 55.
              fTempUsage = 60.
             
             
C.............Volume drawn during the hour. (L)            
              fRepIMS_WaterDraw  = 
     &           fDHW_WaterDraw_byVolume(
     &                   fIMS_DHW_Water_volume_LpDay(i),
     &                   .true.,
     &                   fTempHWT,
     &                   fTempUsage,
     &                   fTempColdSupply      )               

              if (bDebug) 
     &          write(*,*) " IMS->VOL/h ", fRepIMS_WaterDraw 
     
     
C.............Duration of draw. Assume average over hr for now. 
C.............IMS code seems to report wrong PLR for DHW. 
              fRepIMS_WaterHDuration = 3600. ! fRepIMS_WaterDraw / 5. * 60.
              
C.............Compute water heating power, in Watts:   
              if ( fRepIMS_WaterDraw > 0.1 ) then    
              fRepIMS_WaterHLoad = 
     &            fRepIMS_WaterDraw * ( fTempHWT - fTempColdSupply )
     &            * 4186. ! SH of water, use function later. 
     &            / fRepIMS_WaterHDuration
              else 
                fRepIMS_WaterDraw = 0.
              endif 
               
     
              !write (*,*) "IMS DHW : draw      ", fRepIMS_WaterDraw
              !write (*,*) "IMS DHW : duration  ", fRepIMS_WaterHDuration 
              !write (*,*) "IMS DHW : load      ", fRepIMS_WaterHLoad
              !write (*,*) "IMS SH  : load      ", fRepIMS_SpaceHLoad
C             Supervisory routine for IMS characteriztion. 
C             Here we call the routine with the 'no-vent' spec. 
C             Ventilation power is characterized separately.
              call IMS_CharPerformance ( i,                       
     &                                   iMode_NoVent,            
     &                                   fRepIMS_SpaceHLoad,         
     &                                   fRepIMS_WaterHLoad,         
     &                                   fRepIMS_WaterHDuration,     
     &                                   fRepIMS_FuelUseRate,        
     &                                   fRepIMS_ElecUseRate,        
     &                                   fRepIMS_FanPower,             
     &                                   fRepIMS_VentPower,            
     &                                   fRepIMS_AuxPower, 
     &                                   fRepIMS_TS_fan_fraction,    
     &                                   fRepIMS_AddGains )          
                                                                  
              fFanSysPLR_hi(i) = fRepIMS_TS_fan_fraction
              fFanSysPower_hi(i) = fRepIMS_FanPower + fRepIMS_AuxPower
              
              fFanSysPower_lo(i) = fRepIMS_AuxPower +       
     &               fIMS_Interpolate( i,
     &                                 0.05,
     &                                 iIMSVar_SpaceHFanPower ) ! (W)  
              
              
C.......      Translate IMS outputs into hvac-friendly params.
              sys_energy(i) =  fRepIMS_FuelUseRate*3600 ! (J)
              fHeat_energy  =  fRepIMS_FuelUseRate
              fP_pilot_energy = 0.0
              fan_energy(i) = fRepIMS_FanUse
              
              
              
              
              !write (*,*) "IMS ALL : fuelpower  ", fRepIMS_FuelUseRate
              if (bDebug) write(*,*) "--------------------------------"
              if (bDebug) write(*,*) "      IMS DATA (", i,")"
              if (bDebug) write(*,*) "--------------------------------"

              if (bDebug) write(*,*) " +FAN - PLR:", fFanSysPLR_hi(i)
              if (bDebug) write(*,*) " +FAN P Lo :", fFanSysPower_lo(i)
              if (bDebug) write(*,*) " +FAN P Hi :", fFanSysPower_hi(i)             
              
C             Save additional gains.
              bIdealHVACSysGain(i) = .true.
              fIdealHVACSysAddGain( i ) = fRepIMS_AddGains
C             Assume additional gains are 100% convective, as they
C             may include a 'space-heating deficit' during periods
C             of extended DHW loads.
              fIdealHVACSysConGainFrac( i ) = 1.0
              fIdealHVACSysRadGainFrac( i ) = 0.0
              fIdealHVACSysLatGainFrac( i ) = 0.0
            
            
          
          end select PrimarySelect

C.........Check to see if heating systems have back-up, and 
C.........store remainder of unmet load for back-up calculation 
          if ( bDebug) write (*,*) 
     &    "              done."
         
     
          SaveBackupLoad: 
     &    if ( iUnit_Function(i)  == iFuncHeating .and. 
     &         ibackup_heat(i)    /= 0 .and. 
     &         ibackup_sys_num(i) >  0             ) then 
     
          if ( bDebug) write (*,*) 
     &    "              Computing residual load "     
          if ( bDebug) write (*,*) 
     &    "              original load: ", heating_load     
            iBackup = ibackup_sys_num(i)
C...........Remainder of load (W)
C...........
C...........Remaining load = heating_load - part-load-ratio * capacity 
C...........             
            fBackupHeatingLoad( iBackup ) 
     &            = heating_load - fPLR(i) * ss_capacity(i) 
            
            if ( fBackupHeatingLoad(iBackup)  < 0. ) 
     &              fBackupHeatingLoad( iBackup ) = 0.

C...........Co
            fPLR(iBackup) = fPLR_backup(i)


          if ( bDebug) write (*,*) 
     &    "              remaining load: ", fBackupHeatingLoad(iBackup)
           endif SaveBackupLoad
          
        endif IfPrimary

      enddo HVAC_primary_sys_loop
      
      
C-----------------------------------------------------------------------
C     Loop through systems a second time, and determine energy 
C     consumption of backup systems 
C-----------------------------------------------------------------------      


      HVAC_backup_sys_loop: do i=1, num_hvac_sys

C.......Limit analysis to back-up systems. 
        IfBackup: if ( ipriority(i) .eq. 2 ) then  
C.........When specified with legacy hvac files, this if statement 
C.........causes back-up routines to be called only when there is 
C.........load on the system. This behavior is actually erroneous because
C.........it means that pilot energy consumption and other stand-by losses are 
C.........correctly evaluated for 

          V1Backup: 
     &    if ( .not. bHVACVersion2 .and. fPLR(i) <= 0.0001 ) then 

             ! Do nothing 
  
          else

          heating_load = fBackupHeatingLoad(i) 
          
C.........At this point the total heating or cooling that needs to be supplied by
C.........the HVAC system is determined. Depending on the type of the HVAC system,
C.........the appropriate subroutine is called to determine the energy consumption.          
          BackupSelect: select case ( iHVAC_type (i) )
          
C-----------------------------------------------------------------------                        
C           Combustion-based heating systems (including furnaces, 
C           boiler-based forced air combo systems, and hydronic 
C           boiler systems) 
C-----------------------------------------------------------------------                               
            case (iSysFurnace,iSysBoiler)
              
              call FURNACE(i,heating_load,sys_energy(i),
     &                     pilot_energy(i),fPLR(i),fPLF(i) )

C...........If the draft fan power of the furnace is greater than 0, then the
C...........fan energy consumption during the time step is calculated. Draft
C...........fan power can be greater than zero only for induced draft and
C...........condensing furnaces.
            if(draft_fan_power(i).gt.0.) then
               draft_energy(i) = DRAFT_FAN_ENERGY(i,draft_fan_power(i),
     &              fPLR(i),fPLF(i))
            endif
           
C-----------------------------------------------------------------------                        
C           'Baseboard' systems (electic, and includes electric resisitnce
C            heaters in forced air plenums and in hydronic networks 
C            such as back-up systems for heat pumps). 
C-----------------------------------------------------------------------                        
            case (iSysBaseboard)

              call BASEBOARD(i,heating_load,sys_energy(i),fPLR(i),
     &                                                       fPLF(i) )

C-----------------------------------------------------------------------                        
C           Heatpump systems. These currently cannot be called as 
C           backup. 
C-----------------------------------------------------------------------                              
            case (iSysASHP, iSysGSHP, iSysGSHP_GCEP, iSysIMS_P10 ) 
            
              ! Do nothing.
              
          end select BackupSelect
      
        endif V1Backup 

        endif IfBackup
      
     
      enddo HVAC_backup_sys_loop
      
C-----------------------------------------------------------------------
C       Loop through HVAC systems again, and characterize fan operation.
C       If a component is connected to a fan ctl loop, this block is 
C       skipped, and the next one is used to characterize fan power. 
C-----------------------------------------------------------------------        
        HVAC_Fan_Loop: do i = 1, num_hvac_sys
       
          if ( bDebug ) write (*,*) "++++++++++ Calling FAN for ", i
        
C.........Characterize primary systems.        
          if( ipriority(i) == 1 .and. 
     &           iHVAC_type(i) .ne. iSysIMS_P10 )then 
        
C...........Get index and PLR/PLF for backup system, if defined.
            if ( ibackup_heat(i) /= 0 ) then  

              iBackup = ibackup_sys_num(i)

              fPLR_back_temp = fPLR(iBackup)
              fPLF_back_temp = fPLF(iBackup)
 
            else 

              fPLR_back_temp = 0.
              fPLF_back_temp = 1.

            endif 

            est_fan_energy = FAN_ENERGY_CON(i,fPLR(i),fPLR_back_temp,
     &                                        fPLF(i),fPLF_back_temp )

     
                 
     
C           Note: fan_energy returns 0 if the fan-control-loop is 
C                 active. In this case, fan-power will be estimated
C                 after all hvac systems are characterized.           
            fan_energy(i) = est_fan_energy   
            
                if ( bDebug ) write(*,*) "Data from FAN(): ", i 
                if ( bDebug ) write(*,*) 
     &               "  +> Fan PLR h:    ",fFanSysPLR_hi(i)
     
                if ( bDebug ) write(*,*) 
     &               "  +> Fan PWR h:    ",fFanSysPower_hi(i)     
     
                if ( bDebug ) write(*,*) 
     &               "  +> Fan PWR l:    ",fFanSysPower_lo(i)              
            

          endif 
        
        
        enddo HVAC_Fan_Loop

        if (bDebug) write(*,*) "--------------------------------"
        if (bDebug) write(*,*) "      FAN CONTROL BLOCK --------"
        if (bDebug) write(*,*) "--------------------------------"
C-----------------------------------------------------------------------
C       Alternatively, HVAC systems specified in the hvac file with the 
C       '*fan-ctl-loop' tag have controls interlocked with ventilation,
C       and matching heating / cooling devices. Characterize these 
C       loops now. 
C-----------------------------------------------------------------------              
        FanCtlLoop: do iFanLoop = 1, max_fan_loops
        
C.........Is this loop active?
 
          if ( bDebug ) write(*,*)  "> fan loop ", iFanLoop , "?", 
     &             bFanCtlLoopActive(iFanLoop)
          LoopActive: if ( bFanCtlLoopActive(iFanLoop ) ) then 
           
C...........Zero PLR array.            
            do iThisRow =1, iPLRResolution
                
              fFanSHCPower(iThisRow) = 0 
                
            enddo 
              
            bFanCont = .false.   
            fFanTotPLR      = 0.
            fFanMaxPower_hi = 0.
            fFanMaxPower_lo = 0. 
            iNumSystemsOnLoop = 0

            
C...........Loop though all systems, and find components connected
C...........to this loop. Save highest fan power consumption 
            HVAC_FanPowerSearch: do i = 1, num_hvac_sys

              if ( bSysHasFanCtlLoop(i) .and. 
     &             iFanControlLoop(i) == iFanLoop ) then 


                if ( bDebug ) write(*,*) "Connected System: ", i 
                if ( bDebug ) write(*,*) 
     &               "  + Fan PLR h:    ",fFanSysPLR_hi(i)
     
                if ( bDebug ) write(*,*) 
     &               "  + Fan PWR h:    ",fFanSysPower_hi(i)     
     
                if ( bDebug ) write(*,*) 
     &               "  + Fan PWR l:    ",fFanSysPower_lo(i)      
     
                if ( fFanSysPower_lo(i) > fFanMaxPower_lo ) 
     &               fFanMaxPower_lo = fFanSysPower_lo(i)     
     
                if ( fFanSysPower_hi(i) > fFanMaxPower_hi ) 
     &               fFanMaxPower_hi = fFanSysPower_hi(i)      
     
     
                if ( iFan_Operation(i) == iFan_Cont ) bFanCont=.true.      
     
              endif 
            
            

            
            enddo HVAC_FanPowerSearch
            if (bDebug) write (*,*) 
     &          " > Search Fan power lo:", fFanMaxPower_lo
C...........Add ventilaton air distribution requirements to 
C...........vent draw. Compute number of slots that coorespond 
C...........to vent PLR.

            if ( bFanCont ) then 
                iPLRRows = iPLRResolution 
C...............Save ventilation power for use later.                 
                fFanPowerVent = fFanMaxPower_lo * 1.0
            else 
                iPLRRows =  nint ( fVentPLR 
     &                               * float (iPLRResolution) ) 
C...............Save ventilation power for use later.      
                fFanPowerVent = fFanMaxPower_lo * fVentPLR 
            endif

            
C...........Set power consumption in those slots to equal 
C...........system low operation. 
            do iThisRow =1, iPLRRows 
               
               if ( fFanSHCPower(iThisRow) < fFanMaxPower_lo ) then
                    fFanSHCPower(iThisRow) = fFanMaxPower_lo
               endif 

            enddo      


            
C...........Loop though all systems, and find components connected
C...........to this loop. Add incremental fan power used by these 
C...........components to the fan power array. 
            HVAC_FanCtlLoop: do i = 1, num_hvac_sys
              
              fFanPowerSysIncremental(i) = 0. 
              
              if ( bSysHasFanCtlLoop(i) .and. 
     &             iFanControlLoop(i) == iFanLoop ) then 

C...............Increment system counter. Not sure this is used.
                iNumSystemsOnLoop = iNumSystemsOnLoop + 1 

              
C...............Compute # of rows needed in fan power array 
C...............for this units PLR.                    
                iPLRRows =  nint ( fFanSysPLR_hi(i) 
     &                               * float (iPLRResolution) )  

C...............Loop through those rows, and add incremental fan 
C...............power.  


                TheseRows: do iThisRow =1, iPLRRows 

                
                  if ( fFanSHCPower(iThisRow) 
     &                          < fFanSysPower_hi(i) ) then

     
     
     
C....................Save incremental power.                      
                     fFanPowerSysIncremental(i) = 
     &                 fFanPowerSysIncremental(i) +  
     &                   (  fFanSysPower_hi(i) 
     &                           - fFanSHCPower(iThisRow) )
     &                  / float ( iPLRResolution) 
                     
C....................Update this array row.                      
                     fFanSHCPower(iThisRow) = fFanSysPower_hi(i)
                     

                     
                  endif 
                
                enddo TheseRows
                
                
                
                   
              endif 
              
            enddo HVAC_FanCtlLoop
              
C...........All systems & Ventilation characterized. Now compute 
C...........total fan use. 

            fFanPower = 0. 
            
            do iThisRow =1, iPLRResolution
            
C................Save incremental power.                      
                 fFanPower = fFanPower + 
     &               fFanSHCPower(iThisRow) 
     &              / float ( iPLRResolution) 
                                  
           
            enddo             
            
    
       
        
            
            if (bDebug) then 
            
              write (*,*) " Vent PLR: ", fVentPLR 
            
              do iThisRow =1, iPLRResolution
                write (*,*) " fFanSHCPower(",iThisRow,")=", 
     &          fFanSHCPower(iThisRow)                           
              enddo              
            
            endif 
            
            if ( bDebug ) write (*,*) 
     &           "    ->>>>>>> FAN POWER: ", fFanPower

            if ( bDebug ) write (*,*) 
     &           "    ->>>>>>> VNT POWER: ", fFanPowerVent
   
            if ( bDebug ) write (*,*) 
     &           "    ->>>>>>> MAX POWER: ", fFanMaxPower_hi   
 
            fFanEffectivePLR = 
     &          (fFanPower) / fFanMaxPower_hi
 
     
     
C...........Fan power is computed! Loop through the hvac systems 
C...........again and save fan power to fan_energy arrays
            HVAC_FanStorLoop: do i = 1, num_hvac_sys      
        
              SysOnLoop:        
     &        if ( bSysHasFanCtlLoop(i) .and. 
     &            iFanControlLoop(i) == iFanLoop ) then             

            
                fSysFanPower = fFanPowerSysIncremental(i)

            if ( bDebug ) write (*,*) 
     &           "    ->>>>>>> SYS POWER: ", fSysFanPower, i              
      
  
C...............Fan_energy is in joules, convert from watts.                    
                fan_energy(i) = fSysFanPower * time_step
            
              endif SysOnLoop
            
            enddo HVAC_FanStorLoop
            
          
          endif LoopActive
        
        enddo FanCtlLoop

        
C-----------------------------------------------------------------------
C      Code for reporting/storing data in h3kreports!
C-----------------------------------------------------------------------         
        HVAC_report_loop_A: do i = 1, num_hvac_sys
        
C.........Store H3KReports data (Furnace/Boiler/Baseboard)
          
C.........Heating systems           
 
          if ( bDebug) write (*,*) 
     &    ",,,,,,,,,,,,,,Storing data for system ", i
          if ( bDebug) write (*,*) 
     &    "              Report: Iunit func ", iUnit_Function(i), 
     &    "(heating=",iFuncHeating,")"
     
          RepHeatingCooling:     
     &    if ( iUnit_Function(i) == iFuncHeating ) then 
                      
            fH3KStorePLRH(i)       = fPLR(i)
            
            ! Variables ending in _energy are in joules. Convert to watts. 
            fH3KStoreFanEH(i)      = fan_energy(i)    / time_step                                                           = fan_energy(i)    / time_step
            fH3KStoreDraftE(i)     = draft_energy(i)  / time_step      ! Zero for HP, baseboard
            fH3KStorePrimaryEH(i)  = sys_energy(i)    / time_step      
            fH3KStorePilotE(i)     = pilot_energy(i)  / time_step      ! Zero for HP, baseboard
            fH3KStorePumpEH(i)     = pump_energy(i)   / time_step      ! Zero for combustion systems, ASHP

          if ( bDebug) write (*,*) 
     &    "              Storing: PriEHa", fH3KStorePrimaryEH(i),
     &                                     sys_energy(i), time_step
          
            

            
          if ( bDebug) write (*,*) 
     &    "              Storing: FanEH ", fH3KStoreFanEH(i),
     &                                     fan_energy(i), time_step              

          if ( bDebug) write (*,*) 
     &    "              Storing: DraftE ", fH3KStoreDraftE(i),
     &                                     draft_energy(i), time_step         
     
            
            ! SS-capacity is in watts. 
            
            select case ( iHVAC_type(i) ) 
            
              case ( iSysFurnace, iSysBoiler, 
     &               iSysBaseboard, iSysIMS_P10 ) 

                fH3KStoreOutputH(i) = ss_capacity(i) * fH3KStorePLRH(i)
                
              case ( iSysASHP, iSysGSHP, iSysGSHP_GCEP ) 
              
                fH3KStoreOutputH(i) = fH3KActualCapacity(i) 
     &                                     * fH3KStorePLRH(i)
              
            end select
          
            ! Report efficiency or COP depending on HVAC type. 
            select case ( ihvac_type(i) ) 
          
              case ( iSysFurnace, iSysBaseboard, 
     &               iSysIMS_P10     ) 
                
                if ( fH3KStorePrimaryEH(i) > 1.0 .or.
     &               fH3KStorePilotE(i)    > 1.0 )  then

                  fH3KStoreEfficiencyH(i)=
     &               ( fH3KStoreOutputH(i) ) /
     &               ( fH3KStorePrimaryEH(i) + fH3KStorePilotE(i) )

                endif
 
              case ( iSysBoiler ) 

                if ( bHVACVersion2 .and. 
     &               ( fH3KStorePrimaryEH(i) > 1.0 .or.
     &                 fH3KStorePilotE(i)    > 1.0      )
     &             ) then 

                  fH3KStoreEfficiencyH(i)=
     &               ( fH3KStoreOutputH(i) ) /
     &               ( fH3KStorePrimaryEH(i) + fH3KStorePilotE(i) )

                elseif (  fH3KStorePrimaryEH(i) > 1.0 )  then

                  fH3KStoreEfficiencyH(i)=
     &                fH3KStoreOutputH(i)  /
     &                fH3KStorePrimaryEH(i)

                endif      
          
              case ( iSysASHP, iSysGSHP, iSysGSHP_GCEP ) 
              
                if ( fH3KStorePrimaryEH(i) > 1.0 ) then
                  fH3KStoreCOPH(i)       = fH3KStoreOutputH(i)
     &                                / fH3KStorePrimaryEH(i)
                endif
              
            end select 
              
            
C.........Cooling systems           
          else 
            
            

            fH3KStorePLRC(i)       = fPLR(i) 
            fH3KStoreFanEC(i)      = fan_energy(i)   / time_step
            fH3KStorePrimaryEC(i)  = sys_energy(i)   / time_step   ! Compressor energy 
            fH3KStorePumpEC(i)     = pump_energy(i)  / time_step   ! Pump energy (GSHP/GCEP only) 
            fH3KStoreOutputC(i)    = fH3KActualCapacity(i)
     &                                    * fH3KStorePLRC(i)
            if ( fH3KStorePrimaryEC(i) > 1.0 ) then
              fH3KStoreCOPC(i)       = fH3KStoreOutputC(i)
     &                                / fH3KStorePrimaryEC(i)
            endif
          
          endif RepHeatingCooling
          
C.........Additional parameters for h3kstore?          
          fHeat_Energy = sys_energy(i)
          fP_pilot_energy = pilot_energy(i)
         
C-----------------------------------------------------------------------          
C         This next convoluted case statement stores control parameters
C         for use by the older Fuel cell controller, which expects HVAC 
C         loads to be spec'd through the following variables. 
C-----------------------------------------------------------------------          

          FCTctlSelect: select case ( ihvac_type(i) ) 

            case (iSysFurnace) 
C---------------------------------------------------------------------------------
C             Electrical draw (W) of gas-fired furnace.  One variable for the draft fan
C             and another for the ciculating fan that distributes warm air through the
C             house's ductwork. If the furnace is a backup system, the circulating fan
C             power draw will be contained in the FCctl_ashp_circfan variable.
C---------------------------------------------------------------------------------              
              FCctl_furnace_draft   = draft_energy(i) / time_step
              FCctl_furnace_circfan = fan_energy(i)   / time_step

            case (iSysBoiler) 
C---------------------------------------------------------------------------------
C             Electrical draw (W) of gas-fired boiler. One variable for the draft fan
C             (applicable for induced draft and condensing boilers). another for the
C             circulating fan (though likely there is no circulating fan power for a boiler).
C             Power draw of electric boiler not included. If the boiler is a backup system,
C             the circulating fan power draw will be contained in the FCctl_ashp_circfan variable.
C---------------------------------------------------------------------------------      
              FCctl_boiler_draft    = draft_energy(i) / time_step
              FCctl_boiler_circfan  = fan_energy(i)   / time_step
              
            case (iSysBaseboard) 
C---------------------------------------------------------------------------------
C             Electric draw (W) of circulating fan for baseboards. Power draw of 
C             electric baseboards not included. If the baseboard is a backup system,
C             the circulating fan power draw will be contained in the FCctl_ashp_circfan variable.
C---------------------------------------------------------------------------------
              FCctl_baseboard_fan   = fan_energy(i)   / time_step
          
            case (iSysASHP)
              if ( iUnit_Function(i) == iFuncHeating  ) then 
C---------------------------------------------------------------------------------
C               Electric draw (W) of air source heat pump in heating mode. One variable for the
C               compressor and condenser fan. One variable for the circulating fan that distributes
C               warm air to the house's ductwork. The fan power of the circulating fan for the 
C               backup furnace, boiler or baseboard is also included in FCctl_ashp_circfan.
C---------------------------------------------------------------------------------
                FCctl_ashp_compressor = sys_energy(i)   / time_step
                FCctl_ashp_circfan    = fan_energy(i)   / time_step           
              
              endif 
              
            case (iSysGSHP, iSysGSHP_GCEP)
              if ( iUnit_Function(i) == iFuncHeating  ) then 
C---------------------------------------------------------------------------------
C               Electric draw (W) of air source heat pump in heating mode. One variable for the
C               compressor and condenser fan. One variable for the circulating fan that distributes
C               warm air to the house's ductwork. The fan power of the circulating fan for the 
C               backup furnace, boiler or baseboard is also included in FCctl_ashp_circfan.
C---------------------------------------------------------------------------------
                FCctl_ashp_compressor = sys_energy(i)   / time_step
                FCctl_ashp_circfan    = fan_energy(i)   / time_step
                FCctl_gshp_pump       = pump_energy(i)  / time_step
              endif              
              
          end select FCTctlSelect

C---------------------------------------------------------------------------------
C Electrical draw (W) of A/C equipment (ie. ASHP operating in cooling mode).
C One variable for the A/C unit (compressor and condensor fan) and another for the
C ciculating fan that distributes cool air through the house's ductwork.
C---------------------------------------------------------------------------------             
          if ( iUnit_Function(i) == iFuncCooling ) then 
            FCctl_AC_compressor = sys_energy(i)   / time_step
            FCctl_AC_circfan    = fan_energy(i)   / time_step
          
          
            ! Pump_energy is zero for ASHP. 
            FCctl_gshp_pump       = pump_energy(i)  / time_step
          
          endif 
   
          CheckForGains: if ( bIdealHVACSysGain (i) ) then

C     Loop through each serviced zone
           LoopThroughZones: do j = 1, num_sys_zones(i)

C     Get zone index
              iZoneIndex = isys_zone_num( i, j )

C     Set flag indicating serviced zone has associated gains
C     from idealized HVAC equipment:
              bIdealHVACZoneGain( iZoneIndex ) = .true.

C     Add casual gains to running sum --- assume the gains are
C     distributed according to the specified zone capacity
C     fraction.
              fIdealHVACZoneConGain( iZoneIndex ) =
     &             fIdealHVACZoneConGain( iZoneIndex )
     &           + fIdealHVACSysAddGain ( i )
     &               * fIdealHVACSysConGainFrac( i )
     &               * sys_zone_cap_frac( i, j )

              fIdealHVACZoneRadGain( iZoneIndex ) =
     &             fIdealHVACZoneRadGain( iZoneIndex )
     &           + fIdealHVACSysAddGain ( i )
     &               * fIdealHVACSysRadGainFrac( i )
     &               * sys_zone_cap_frac( i, j )

              fIdealHVACZoneLatGain( iZoneIndex ) =
     &             fIdealHVACZoneLatGain( iZoneIndex )
     &           + fIdealHVACSysAddGain ( i )
     &               * fIdealHVACSysLatGainFrac( i )
     &               * sys_zone_cap_frac( i, j )

           enddo LoopThroughZones


          endif CheckForGains


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
     &        sys_energy(ibackup_sys_num(i)),
c     No backup cooling so hard coded to zero.
     &        0.,
C     fB_pilot_energy is the pilot energy used in Joules for the Backup
C     HVAC system.
     &        fB_pilot_energy,
C     fPPLR is the Part load ratio of the backup hvac system.
     &        fbPLR)
c     Stores fan energy, Heating and cooling loads.
         endif 

         if ( iHVAC_type(i) == iSysGSHP_GCEP ) then 
         
           call H3KSTORE_HVAC_Packaged_DATA(
     &        Heating_load,
     &        Cooling_load,
     &        fan_energy(i),
     &        0.0,
     &        pump_energy(i))   ! <- subroutine expects GCEP / GSHP pump power to 
                                !    to be reported separately, but we're using the
                                !    same variable (pump_energy) to store both data.  
                                !    therefore, we must set one or the other to zero. 
          else 
           call H3KSTORE_HVAC_Packaged_DATA(
     &        Heating_load,
     &        Cooling_load,
     &        fan_energy(i),
     &        pump_energy(i),
     &        0.0)              ! <- subroutine expects GCEP / GSHP pump power to 
                                !    to be reported separately, but we're using the
                                !    same variable to store both data. Therefore, 
                                !    we must pass it twice.           
          
          endif


C-----------------------------------------------------------------------
C     For linked systems (such as the cooling component of an GSHP),
C     store cooling energy use in linked system arrays
C-----------------------------------------------------------------------

        store_linked: if ( bLinked (i) ) then
        
          iCoolSys = iLinkedSystem(i)

          fH3KStorePLRC(iCoolSys)          =   fH3KStorePLRC(i)
          fH3KStoreFanEC(iCoolSys)         =   fH3KStoreFanEC(i)
          fH3KStorePrimaryEC(iCoolSys)     =   fH3KStorePrimaryEC(i)
          
          
          if ( iHVAC_type(i) == iSysGSHP_GCEP 
     &        .and. .not. bHVACVersion2  ) then 

            fH3KStorePumpEC(iCoolSys) = fH3KStorePumpEC(i)            

          else 

            fH3KStorePumpEH(iCoolSys) = MAX( fH3KStorePumpEH(iCoolSys),
     &                                     fH3KStorePumpEC(i) )
            fH3KStorePumpEC(iCoolSys) = 0.

          endif 

          fH3KStoreEfficiencyC(iCoolSys)   =   fH3KStoreEfficiencyC(i)
          fH3KStoreOutputC(iCoolSys)       =   fH3KStoreOutputC(i)
          fH3KStoreCOPC(iCoolSys)          =   fH3KStoreCOPC(i)
        
        endif store_linked 


      enddo HVAC_report_loop_A

C-----------------------------------------------------------------------
C     Loop through HVAC systems, and report data to H3Kreports.
C     Note: this code must be kept apart from the main HVAC system loop
C           (above) because the latter solves systems according to
C           their primary/backup status (as opposed to sequentially)
C-----------------------------------------------------------------------

C.....Set fan energy to zero
      fH3KSumFanEnergy = 0.

      HVAC_system_report_loop_B: do i = 1, num_hvac_sys
  
        if (bdebug) write(*,*) "----------- LOOP HVAC SYS  : ", i
      
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
          Call AddToReportWild2(rvPltHvacComFuelAmnt%Identifier,
     &      fConvertEnergyToFuelAmount( fH3KStorePrimaryEH(i)
     &                                   + fH3KStorePrimaryEC(i)
     &                                   + fH3KStorePumpEH(i)
     &                                   + fH3KStorePumpEC(i)
     &                                   + fH3KStoreDraftE(i),
     &                                  ifuel_type(i) ),
     &      cHVACIndex // '_'
     &      // cHVACTypes(ihvac_type(i))(1:iHVACTypeLen(ihvac_type(i))),
     &          cFuelName(ifuel_type(i))(1:iFuelNameLen(ifuel_type(i))))

          Call AddToReportDetailsWild2(rvPltHvacComFuelAmnt%Identifier,
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
          Call AddToReportWild2(rvPltHvacComFuelAmnt%Identifier,
     &      fConvertEnergyToFuelAmount( fH3KStorePrimaryEH(i)
     &                                   + fH3KStorePilotE(i),
     &                                  ifuel_type(i) ),
     &      cHVACIndex // '_'
     &      // cHVACTypes(ihvac_type(i))(1:iHVACTypeLen(ihvac_type(i))),
     &      cFuelName(ifuel_type(i))(1:iFuelNameLen(ifuel_type(i))))


          Call AddToReportDetailsWild2(rvPltHvacComFuelAmnt%Identifier,
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

          call AddToReportWild2(rvPltHvacComFuelAmnt%Identifier,
     &      fConvertEnergyToFuelAmount( fH3KStorePumpEH(i)
     &                                   + fH3KStorePumpEC(i)
     &                                   + fH3KStoreDraftE(i),
     &                                  iElectricity ),
     &      cHVACIndex // '_'
     &      // cHVACTypes(ihvac_type(i))(1:iHVACTypeLen(ihvac_type(i))),
     &      cFuelName(iElectricity)(1:iFuelNameLen(iElectricity)))

          Call AddToReportDetailsWild2(rvPltHvacComFuelAmnt%Identifier,
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
     &            ( fH3KStorePilotE(i) > 1.  )
     &          ) then

             Call AddToReportWild1(rvPltHvacFuelEnPilot%Identifier,
     &         fH3KStorePilotE(i),
     &         cHVACIndex // '_'
     &      // cHVACTypes(ihvac_type(i))(1:iHVACTypeLen(ihvac_type(i))))
 
             Call AddToReportDetailsWild1(
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
            Call AddToReportWild1(rvPltHvacEff%Identifier,
     &       fH3KStoreEfficiencyH(i),
     &       cHVACIndex // '_'
     &    // cHVACTypes(ihvac_type(i))(1:iHVACTypeLen(ihvac_type(i))))
 
            Call AddToReportDetailsWild1(rvPltHvacEff%Identifier,
     &          cHVACIndex // '_'
     &    // cHVACTypes(ihvac_type(i))(1:iHVACTypeLen(ihvac_type(i))),
     &       'units','(-)',
     &       'Idealized HVAC models: Efficiency of '
     &    // cHVACTypes(ihvac_type(i))(1:iHVACTypeLen(ihvac_type(i)))
     &    // ' system (thermal output / HHV Fuel input)')
             endif

C.........P.10 --- presently unsupported
          case ( iSysIMS_P10 )

C...........Set end-use flag
            iSysEndUse = iUseSpaceHeat          
          
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
            Call AddToReportWild1(rvPltHvacCOPHeat%Identifier,
     &       fH3KStoreCOPH(i),
     &       cHVACIndex // '_'
     &    // cHVACTypes(ihvac_type(i))(1:iHVACTypeLen(ihvac_type(i))))
 
            Call AddToReportDetailsWild1(rvPltHvacCOPHeat%Identifier,
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
            Call AddToReportWild1(rvPltHvacCOPCool%Identifier,
     &       fH3KStoreCOPC(i),
     &       cHVACIndex // '_'
     &    // cHVACTypes(ihvac_type(i))(1:iHVACTypeLen(ihvac_type(i))))

            Call AddToReportDetailsWild1(rvPltHvacCOPCool%Identifier,
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
        
          Call AddToReportWild1(rvPltHvacFuelEnHeat%Identifier,
     &      fH3KStorePrimaryEH(i),
     &      cHVACIndex // '_'
     &   // cHVACTypes(ihvac_type(i))(1:iHVACTypeLen(ihvac_type(i))))
 
          Call AddToReportDetailsWild1(rvPltHvacFuelEnHeat%Identifier,
     &      cHVACIndex // '_'
     &   // cHVACTypes(ihvac_type(i))(1:iHVACTypeLen(ihvac_type(i))),
     &      'units', '(W)',
     &      'Idealized HVAC models: Primary energy used by '
     &   // cHVACTypes(ihvac_type(i))(1:iHVACTypeLen(ihvac_type(i)))
     &   // ' system for heating')


          System_is_on_H:
     &    if ( fH3KStorePrimaryEH(i) > 1.0 ) then
           Call AddToReportWild1(rvPltHvacPrtLdHeat%Identifier,
     &      fH3KStorePLRH(i),
     &      cHVACIndex // '_'
     &   // cHVACTypes(ihvac_type(i))(1:iHVACTypeLen(ihvac_type(i))))

           Call AddToReportDetailsWild1(rvPltHvacPrtLdHeat%Identifier,
     &      cHVACIndex // '_'
     &   // cHVACTypes(ihvac_type(i))(1:iHVACTypeLen(ihvac_type(i))),
     &      'units', '(-)',
     &      'Idealized HVAC models: '
     &   // cHVACTypes(ihvac_type(i))(1:iHVACTypeLen(ihvac_type(i)))
     &   // ' heating part-load-ratio (PLR)')
          endif System_is_on_H

          Call AddToReportWild1(rvPltHvacThrmOutHeat%Identifier,
     &      fH3KStoreOutputH(i),
     &      cHVACIndex // '_'
     &   // cHVACTypes(ihvac_type(i))(1:iHVACTypeLen(ihvac_type(i))))

          Call AddToReportDetailsWild1(rvPltHvacThrmOutHeat%Identifier,
     &      cHVACIndex // '_'
     &   // cHVACTypes(ihvac_type(i))(1:iHVACTypeLen(ihvac_type(i))),
     &      'units','(W)',
     &      'Idealized HVAC models: '
     &   // cHVACTypes(ihvac_type(i))(1:iHVACTypeLen(ihvac_type(i)))
     &   // ' heating thermal output')
        endif

        if ( bTwoModes(i) .or. iSysEndUse == iUseSpaceCool ) then


C...............Energy consumption for cooling
          Call AddToReportWild1(rvPltHvacFuelEnCool%Identifier,
     &      fH3KStorePrimaryEC(i),
     &      cHVACIndex // '_'
     &   // cHVACTypes(ihvac_type(i))(1:iHVACTypeLen(ihvac_type(i))))
 
          Call AddToReportDetailsWild1(rvPltHvacFuelEnCool%Identifier,
     &      cHVACIndex
     &   // '_'
     &   // cHVACTypes(ihvac_type(i))(1:iHVACTypeLen(ihvac_type(i))),
     &      'units', '(W)',
     &      'Idealized HVAC models: Primary energy used by '
     &   // cHVACTypes(ihvac_type(i))(1:iHVACTypeLen(ihvac_type(i)))
     &   // ' system for cooling')
 
          System_is_on_C:
     &    if ( fH3KStorePrimaryEC(i) > 1.0 ) then
           Call AddToReportWild1(rvPltHvacPrtCool%Identifier,
     &       fH3KStorePLRC(i),
     &       cHVACIndex // '_'
     &    // cHVACTypes(ihvac_type(i))(1:iHVACTypeLen(ihvac_type(i))))

           Call AddToReportDetailsWild1(rvPltHvacPrtCool%Identifier,
     &       cHVACIndex // '_'
     &    // cHVACTypes(ihvac_type(i))(1:iHVACTypeLen(ihvac_type(i))),
     &       'units', '(-)',
     &       'Idealized HVAC models: '
     &    // cHVACTypes(ihvac_type(i))(1:iHVACTypeLen(ihvac_type(i)))
     &    // ' cooling part-load-ratio (PLR)')
          endif System_is_on_C


          Call AddToReportWild1(rvPltHvacThrmCool%Identifier,
     &      fH3KStoreOutputC(i),
     &      cHVACIndex // '_'
     &   // cHVACTypes(ihvac_type(i))(1:iHVACTypeLen(ihvac_type(i))))

          Call AddToReportDetailsWild1(rvPltHvacThrmCool%Identifier,
     &       cHVACIndex // '_'
     &    // cHVACTypes(ihvac_type(i))(1:iHVACTypeLen(ihvac_type(i))),
     &       'units', '(W)',
     &       'Idealized HVAC models: '
     &    // cHVACTypes(ihvac_type(i))(1:iHVACTypeLen(ihvac_type(i)))
     &    // ' cooling thermal output')     
     


          Call AddToReportWild1(rvPltHvacFuelEnTtl%Identifier,
     &      fH3KStorePrimaryEH(i) + fH3KStorePrimaryEC(i),
     &      cHVACIndex // '_'
     &   // cHVACTypes(ihvac_type(i))(1:iHVACTypeLen(ihvac_type(i))))

          Call AddToReportDetailsWild1(rvPltHvacFuelEnTtl%Identifier,
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
        Call AddToReportWild1(rvPltHvacCoilCoolSns%Identifier,
     &    fASHP_AC_Coil_Load_Sensible(iCoolSys),
     &    cHVACIndex // '_'
     & // cHVACTypes(ihvac_type(iCoolSys))
     &            (1:iHVACTypeLen(ihvac_type(iCoolSys))))

        Call AddToReportDetailsWild1(rvPltHvacCoilCoolSns%Identifier,
     &    cHVACIndex // '_'
     & // cHVACTypes(ihvac_type(iCoolSys))
     &            (1:iHVACTypeLen(ihvac_type(iCoolSys))),
     &    'units', '(W)',
     &    'Idealized HVAC models: '
     & // cHVACTypes(ihvac_type(iCoolSys))
     &               (1:iHVACTypeLen(ihvac_type(iCoolSys)))
     & // ' sensible cooling load at coil')


        Call AddToReportWild1(rvPltHvacCoilCoolLtnt%Identifier,
     &   fASHP_AC_Coil_Load_Latent(iCoolSys),
     &   cHVACIndex // '_'
     & // cHVACTypes(ihvac_type(iCoolSys))
     &               (1:iHVACTypeLen(ihvac_type(iCoolSys))))

        Call AddToReportDetailsWild1(rvPltHvacCoilCoolLtnt%Identifier,
     &   cHVACIndex // '_'
     & // cHVACTypes(ihvac_type(iCoolSys))
     &               (1:iHVACTypeLen(ihvac_type(iCoolSys))),
     &   'units', '(W)',
     &   'Idealized HVAC models: '
     & // cHVACTypes(ihvac_type(iCoolSys))
     &               (1:iHVACTypeLen(ihvac_type(iCoolSys)))
     & // ' latent cooling load at coil')


        Call AddToReportWild1(rvPltHvacCoilCoolTtl%Identifier,
     &   fASHP_AC_Coil_Load_Total(iCoolSys),
     &   cHVACIndex // '_'
     & // cHVACTypes(ihvac_type(iCoolSys))
     &               (1:iHVACTypeLen(ihvac_type(iCoolSys))))

        Call AddToReportDetailsWild1(rvPltHvacCoilCoolTtl%Identifier,
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
        if ( bDebug ) write (*,*) 
     &            " REPORT BY FUEL TYPE ", 
     &            "> system: ", i, "Fuel type", ifuel_type(i),
     &            " END USE: ", iSysEndUse
        
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

      enddo HVAC_system_report_loop_B

  
      
C Store fan electricity use in circulation/ventilation (W)




      fH3KSumEnergyUseElec( iUseHRV ) = fFanPowerVent

C Report fan power consumption and heat gains

C.......Fan energy
        Call AddToReport(rvPltHvacCrcFuelEnIn%Identifier,
     &   fH3KSumFanEnergy)

        Call AddToReport(rvPltHvacCrcFElecAmnt%Identifier,
     &   fConvertEnergyToFuelAmount( fH3KSumFanEnergy, iElectricity ))

        Call AddToReport(rvPltHvacCrcHeatTrn%Identifier,
     &   fH3KSumFanEnergy)
     
     
C Store aggregate electricity use for reporting in Site Utility facilitity.
      fSUFuelEnergyUse( iElectricity,iUseHRV ) = fFanPowerVent

  
     

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

c     References:	HOT2000 Technical Manual, Version 6, Canadian Home Builders
c     Association, August 1991

c     Furnace Model for Implementation in H3K, H3K model documentation,
c     Kamel Haddad, April 5th 2000

c     INPUTS:
c     fan_power	indoor fan power in W
c     ss_cap		capacity of the hvac unit in W
c     req_load	load on the hvac equipment in W
c     ifan_operation  mode of operation of the indoor circulation fan
c     =0 no circulation fan
c     =1 fan in auto mode
c     =2 fan in continuous mode

c     OUTPUTS:
c     PLR		estimate of the part-load ratio on the equipment.
c     plr is the ratio of the load on the system to the
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




