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

      IMPLICIT NONE

#include "hvac_parameters.h"

#include "building.h"

#include "hvac_common.h"

#include "ashp_common.h"

#include "SOFC.h"

#include "gcep_common.h"

#include "IMS_common.h"


c hvac system file and flag:  	ihvacflag=1 there is hvac file
c ihvacflag=0 no hvac file
      common/hvacinfo/ihvacflag,hvacfile

      INTEGER ihvacflag
      CHARACTER*72 hvacfile

c Use common for HVAC system energy consumption
      common/HVAC_OUTPUT/fan_energy(max_sys),
     &draft_energy(max_sys), sys_energy(max_sys),
     &backup_energy(max_sys), pilot_energy(max_sys),
     &GSHP_pump_energy, GCEP_pump_energy

c Real variables in HVAC system energy consumption common
      REAL fan_energy, draft_energy, sys_energy, backup_energy,
     &pilot_energy, GSHP_pump_energy, GCEP_pump_energy


C Common block containing miscellaneous zone data.
      common/bctl/              ! MISC zond data:
     &     ncf,ibsn(mcf,4),     ! not used 
     &     iban(mcf,3),         ! zone control data
     &     nbcdt(mcf),ibcdv(mcf,mbcdt,2),nbcdp(mcf,mbcdt), ! not used 
     &     tbcps(mcf,mbcdt,mbcdp),                         ! not used 
     &     ibctyp(mcf,mbcdt,mbcdp),ibclaw(mcf,mbcdt,mbcdp),! not used 
     &     bmiscd(mcf,mbcdt,mbcdp,misc)                    ! not used 
      integer ncf, ibsn, iban, idcdtl, ibcdv, nbcdp, ibctyp, ibclaw
      integer nbcdt
      real tbcps, bmiscd

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

c Local real variables
      REAL plr_furnace, plr_backup, plr_heat_pump, plr_boiler,
     &plr_baseboard, plf_backup, plf_heat_pump, plf_furnace,
     &plf_boiler, plf_baseboard, heating_load, cooling_load, 
     &req_load, time_step, FAN_ENERGY_CON, DRAFT_FAN_ENERGY,
     &heat_pump_cap, GSHP_pump_power, GCEP_pump_power,
     &heat_pump_cooling_cap

c Local integer variables
      INTEGER i, j, month, iday, icomp

C IMS variables
      real fIMS_SpaceHLoad        ! Space heating load (W)
      real fIMS_WaterHLoad        ! Water heating load (W)
      real fIMS_WaterHDuration    ! Duration of water heating load (s)
      real fIMS_FuelUseRate       ! Fuel use (kg/s)
      real fIMS_ElecUseRate       ! Electricity use (W)
      real fIMS_VentFlowRate      ! IMS ventilation flow (kg/s)
      real fIMS_VentLoad          ! Thermal load associated with vent. (W)
      real fIMS_AddGains          ! Heat added/removed from zones
                                  !   by IMS fans/systems/ventilation (W)
      real fIMS_VaporChange       ! Moisture balance assoc. w/ IMS (kg/s)

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

C.....H3Kreports.(HVAC_ELEC_INIT).start......................
C.....INITIALIZATION for H3Kreporting object
      H3K_ASHP_elec      = 0.
      H3K_GCEP_elec      = 0.
      H3K_GSHP_elec      = 0. 
      H3K_fan_elec       = 0. 
      H3K_HRV_elec       = 0. 
C.....H3Kreports.(HVAC_ELEC_INIT).end........................


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
               heating_load = heating_load + req_load
            elseif(req_load.lt.0) then
               cooling_load = cooling_load + abs(req_load)
            endif

 20      continue


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
c     Heat pump part-load factor
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


c     At this point the total heating or cooling that needs to be supplied by
c     the HVAC system is determined. Depending on the type of the HVAC system,
c     the appropriate subroutine is called to determine the energy consumption. 

c     If the primary heating system is a furnace
         HVAC_TYPE: if((ihvac_type(i).eq.1).and.
     &                (ipriority(i).eq.1)) then
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

C      If the primary system is an IMS system, call IMS characterization
C      routine 
         elseif (  ihvac_type(i) == iSysIMS_P10             .and.
     &             ipriority(i)  == iHVACPrimarySys  ) then
     &

C.........Collect space and water heating loads.
          fIMS_SpaceHLoad     = heating_load  ! W
          fIMS_WaterHLoad     = 0.0 !1000.0        ! W
          fIMS_WaterHDuration = 600.0         ! Seconds
C.........Superviosry routine for IMS characteriztion
          call IMS_CharPerformance ( i,
     &                               iMode_LowVent,
     &                               fIMS_SpaceHLoad,
     &                               fIMS_WaterHLoad,
     &                               fIMS_WaterHDuration,
     &                               fIMS_FuelUseRate,
     &                               fIMS_ElecUseRate,
     &                               fIMS_VentFlowRate,
     &                               fIMS_VentLoad,
     &                               fIMS_AddGains,
     &                               fIMS_VaporChange )

C.........Translate IMS outputs into hvac-friendly params.
 

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

c GCEP: end


c     For an air-source heat pump system in the cooling mode, call the 
c     cooling mode simulation subroutine
         elseif((ihvac_type(i).eq.7).and.(iunit_function(i).eq.2)
     &           .and.(ipriority(i).eq.1)) then
            call ASHP_COOLING(i,cooling_load,sys_energy(i),
     &           plr_heat_pump,plf_heat_pump)

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

c GSHP: begin
c For a ground-source heat pump system in cooling mode, first call the 
c subroutine to calculate the correlation coefficients for the GSHP in cooling
c mode. Once GSHP_COOL_COEFF is called, the same ASHP cooling routines are used -
c for both the primary system and the backup systems.
         elseif((ihvac_type(i).eq.8).and.(iunit_function(i).eq.2)
     &   .and.(ipriority(i).eq.1)) then 
            call GSHP_COOL_COEFF(i)  
            call ASHP_COOLING(i,cooling_load,sys_energy(i),
     &                      plr_heat_pump,plf_heat_pump)

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
            call GCEP_COOL_COEFF(i)  
            call ASHP_COOLING(i,cooling_load,sys_energy(i),
     &                      plr_heat_pump,plf_heat_pump)

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
c GCEP: end

c     If the hvac system is not a primary system (This can be a backup 
c     furnace for an air-source heat pump system), the simulation is 
c     skipped here as it is taken care of when simulating the primary 
c     system performance
         elseif(ipriority(i).eq.2) then
            goto 10 
         endif HVAC_TYPE

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
     


C XML reports.

         WRITE(H3K_rep_NAME,'(A,A)') 
     & 		'plant/CETC_HVAC/Primary_System/',
     &        	HVAC_STRING(ihvac_type(i))
     
         WRITE(xmlHeat_Energy,'(A,A)') 
     &  	H3K_rep_NAME(1:LNBLNK(H3K_rep_NAME)),
     &		'/Heating'
             
C.,......-> update meta-data
         call REP_SET_META(
     &        xmlHeat_Energy(1:LNBLNK(xmlHeat_Energy)),
     &        'units',
     &        '(W)'
     &        )

C.......-> update data 
         call REP_REPORT(
     &        fHeat_Energy/time_step,
     &          xmlHeat_Energy(1:LNBLNK(xmlHeat_Energy))
     &        )
     
C.....Cooling

     
         WRITE(xmlCool_Energy,'(A,A)') 
     &  	H3K_rep_NAME(1:LNBLNK(H3K_rep_NAME)),
     &		'/Cooling'
             
C.,......-> update meta-data
         call REP_SET_META(
     &        xmlCool_Energy(1:LNBLNK(xmlCool_Energy)),
     &        'units',
     &        '(W)'
     &        )

C.......-> update data 
         call REP_REPORT(
     &        fCool_Energy/time_step,
     &        xmlCool_Energy(1:LNBLNK(xmlCool_Energy))
     &        )  
     
C.....Pilot Energy
     
         WRITE(xmlPilot_Energy,'(A,A)') 
     &  	H3K_rep_NAME(1:LNBLNK(H3K_rep_NAME)),
     &		'/Pilot'
             
C.,......-> update meta-data
         call REP_SET_META(
     &        xmlPilot_Energy(1:LNBLNK(xmlPilot_Energy)),
     &        'units',
     &        '(W)'
     &        )

C.......-> update data 
         call REP_REPORT(
     &        fP_pilot_Energy/time_step,
     &        xmlPilot_Energy(1:LNBLNK(xmlPilot_Energy))
     &        )      
     
C.....Part load ratio of system
     
         WRITE(xmlPLR,'(A,A)') 
     &  	H3K_rep_NAME(1:LNBLNK(H3K_rep_NAME)),
     &		'/Part_Load_Ratio'
             
C.,......-> update meta-data
         call REP_SET_META(
     &        xmlPLR(1:LNBLNK(xmlPLR)),
     &        'units',
     &        '(%)'
     &        )

C.......-> update data 
         call REP_REPORT(
     &        fPPLR,
     &        xmlPLR(1:LNBLNK(xmlPLR))
     &        )        

     
     
     

C     Stores backup HVAC energy data
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
C XML reports.
         WRITE(H3K_rep_NAME,'(A,A)') 
     & 		'plant/CETC_HVAC/Secondary_System/',
     &        	HVAC_STRING(ihvac_type(ibackup_sys_num(i)))
     
         WRITE(xmlHeat_Energy,'(A,A)') 
     &  	H3K_rep_NAME(1:LNBLNK(H3K_rep_NAME)),
     &		'/Heating'
             
C.,......-> update meta-data
         call REP_SET_META(
     &        xmlHeat_Energy(1:LNBLNK(xmlHeat_Energy)),
     &        'units',
     &        '(W)'
     &        )

C.......-> update data 
         call REP_REPORT(
     &        backup_energy(i)/time_step,
     &          xmlHeat_Energy(1:LNBLNK(xmlHeat_Energy))
     &        )
     
C.....Cooling

     
         WRITE(xmlCool_Energy,'(A,A)') 
     &  	H3K_rep_NAME(1:LNBLNK(H3K_rep_NAME)),
     &		'/Cooling'
             
C.,......-> update meta-data
         call REP_SET_META(
     &        xmlCool_Energy(1:LNBLNK(xmlCool_Energy)),
     &        'units',
     &        '(W)'
     &        )

C.......-> update data 
         call REP_REPORT(
     &        0.0,
     &        xmlCool_Energy(1:LNBLNK(xmlCool_Energy))
     &        )  
     
C.....Pilot Energy
     
         WRITE(xmlPilot_Energy,'(A,A)') 
     &  	H3K_rep_NAME(1:LNBLNK(H3K_rep_NAME)),
     &		'/Pilot'
             
C.,......-> update meta-data
         call REP_SET_META(
     &        xmlPilot_Energy(1:LNBLNK(xmlPilot_Energy)),
     &        'units',
     &        '(W)'
     &        )

C.......-> update data 
         call REP_REPORT(
     &        fB_pilot_energy/time_step,
     &        xmlPilot_Energy(1:LNBLNK(xmlPilot_Energy))
     &        )      
     
C.....Part load ratio of system
     
         WRITE(xmlPLR,'(A,A)') 
     &  	H3K_rep_NAME(1:LNBLNK(H3K_rep_NAME)),
     &		'/Part_Load_Ratio'
             
C.,......-> update meta-data
         call REP_SET_META(
     &        xmlPLR(1:LNBLNK(xmlPLR)),
     &        'units',
     &        '(%)'
     &        )

C.......-> update data 
         call REP_REPORT(
     &        fbPLR,
     &        xmlPLR(1:LNBLNK(xmlPLR))
     &        )        

         call H3KSTORE_HVAC_Packaged_DATA(
C     stores the heating and cooling loads for the timestep in Joules.
     &        Heating_load,
     &        Cooling_load,
C     determines the fan energy used in the primary HVAC system (the backup 
C     uses the same fan.) in Joules.
     &        fan_energy(i),  
     &        GSHP_pump_energy,
     &        GCEP_pump_energy)


C.....H3Kreports.(HVAC_ELEC_COMP_CALC).start......................
C.....(i) LOG current component electrical use, and (ii) STORE
C.....total electrical by system type:
         H3K_fan_elec  = H3K_fan_elec    
     &        +  (draft_energy(i) + fan_energy(i)) / time_step
         H3K_ASHP_elec = H3K_ASHP_elec + sys_energy(i) / time_step
         H3K_GSHP_elec = H3K_GSHP_elec + GSHP_pump_energy / time_step
         H3K_GCEP_elec = H3K_GCEP_elec 
     &        + GCEP_pump_energy / time_step
         H3K_Current_elec = (fan_energy(i) + draft_energy(i) 
     &        +  sys_energy(i) +  GCEP_pump_energy ) / time_step
C........Log current component electric load:
C........-> Set format depending on number of zones
         if ( i .gt. 9 ) then
            H3K_FORMAT = '(A,I2,A)'
         else 
            H3K_FORMAT = '(A,I1,A)'
         endif
C........-> Set name
C << icomp may not have been defined >>
         WRITE(H3K_rep_NAME,H3K_FORMAT) 'plant/CETC_HVAC/Component_',
     &        i,
     &        '/elec_load'        
C.,......-> update meta-data
         call REP_SET_META(
     &        H3K_rep_NAME(1:LNBLNK(H3K_rep_NAME)),
     &        'units',
     &        '(W)'
     &        )
         call REP_SET_META(
     &        H3K_rep_NAME(1:LNBLNK(H3K_rep_NAME)),
     &        'WattsToGJ',
     &        ' '
     &        )
C.......-> update data 
         call REP_REPORT(
     &        H3K_Current_elec,
     &        H3K_rep_NAME(1:LNBLNK(H3K_rep_NAME))
     &        )
C.....H3Kreports.(HVAC_ELEC_COMP_CALC).end.......................


 10   continue


C.....H3Kreports.(HVAC_ELEC_COMP_CALC).start......................
C.....(i) LOG total electrical by system type:
      H3K_fan_elec  = H3K_fan_elec    
     &     +  (draft_energy(i) + fan_energy(i)) / time_step
      H3K_ASHP_elec = H3K_ASHP_elec + sys_energy(i) / time_step
      H3K_GSHP_elec = H3K_GSHP_elec + GSHP_pump_energy / time_step
      H3K_GCEP_elec = H3K_GCEP_elec 
     &     + GCEP_pump_energy / time_step
      H3K_Current_elec = (fan_energy(i) + draft_energy(i) 
     &     +  sys_energy(i) +  GCEP_pump_energy ) / time_step

C.....Log FAN component electric load:
C.....-> Set name
      WRITE(H3K_rep_NAME,'(A)') 'plant/CETC_HVAC/fan/elec_load'        
C.....-> update meta-data
      call REP_SET_META(
     &     H3K_rep_NAME(1:LNBLNK(H3K_rep_NAME)),
     &     'units',
     &     '(W)'
     &     )
      call REP_SET_META(
     &     H3K_rep_NAME(1:LNBLNK(H3K_rep_NAME)),
     &     'WattsToGJ',
     &     ' '
     &     )
C....-> update data 
      call REP_REPORT(
     &     H3K_fan_elec,
     &     H3K_rep_NAME(1:LNBLNK(H3K_rep_NAME))
     &     )


C.....Log ASHP component electric load:
C.....-> Set name
      WRITE(H3K_rep_NAME,'(A)') 'plant/CETC_HVAC/ASHP/elec_load'        
C.....-> update meta-data
      call REP_SET_META(
     &     H3K_rep_NAME(1:LNBLNK(H3K_rep_NAME)),
     &     'units',
     &     '(W)'
     &     )
      call REP_SET_META(
     &     H3K_rep_NAME(1:LNBLNK(H3K_rep_NAME)),
     &     'WattsToGJ',
     &     ' '
     &     )
C.....-> update data 
      call REP_REPORT(
     &     H3K_ASHP_elec,
     &     H3K_rep_NAME(1:LNBLNK(H3K_rep_NAME))
     &     )

C.....Log GSHP component electric load:
C.....-> Set name
      WRITE(H3K_rep_NAME,'(A)') 'plant/CETC_HVAC/GSHP/elec_load'        
C.....-> update meta-data
      call REP_SET_META(
     &     H3K_rep_NAME(1:LNBLNK(H3K_rep_NAME)),
     &     'units',
     &     '(W)'
     &     )
      call REP_SET_META(
     &     H3K_rep_NAME(1:LNBLNK(H3K_rep_NAME)),
     &     'WattsToGJ',
     &     ' '
     &     )
C.....-> update data 
      call REP_REPORT(
     &     H3K_GSHP_elec,
     &     H3K_rep_NAME(1:LNBLNK(H3K_rep_NAME))
     &     )

C.....Log GCEP component electric load:
C.....-> Set name
      WRITE(H3K_rep_NAME,'(A)') 'plant/CETC_HVAC/GCEP/elec_load'        
C.....-> update meta-data
      call REP_SET_META(
     &     H3K_rep_NAME(1:LNBLNK(H3K_rep_NAME)),
     &     'units',
     &     '(W)'
     &     )
      call REP_SET_META(
     &     H3K_rep_NAME(1:LNBLNK(H3K_rep_NAME)),
     &     'WattsToGJ',
     &     ' '
     &     )
C.....-> update data 
      call REP_REPORT(
     &     H3K_GCEP_elec,
     &     H3K_rep_NAME(1:LNBLNK(H3K_rep_NAME))
     &     )
C.....H3Kreports.(HVAC_ELEC_COMP_CALC).end.......................




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

