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

c This file contains the folowing functions:
c	FAN_ENERGY_CONSUMPTION
c			determines the energy consumption of the fan
c *********************************************************************

c FAN_SIZE_improved
C 
c This routine re-implements fan_size, above. Instead of trying
c to deduct the fan size for each system, it merely reports the
c computed fan-size for the specified system.
c INPUTS:
c       isys            number for hvac system
c       HVAC_INPUT_DATA common for HVAC input data
c       HEAT_PUMP_INPUT_DATA
c                       common for heat pump input data
c       
c OUTPUTS:
c       FAN_SIZE        size of the indoor circulation fan in W
c *********************************************************************
      real function Fan_AutoSize_improved(iSystem)
      implicit none 
#include "hvac_parameters.h"
#include "building.h"
#include "hvac_common.h"

      integer iSystem

C Make sure system has a fan!
      Sys_Has_Fan: if ( iFan_Operation(iSystem) == iFan_None ) then

        Fan_AutoSize_improved = 0.0

      else
C Check HVAC type, and compute fan size

        HVAC_Type: select case ( iHVAC_type(iSystem) )

          case ( iSysFurnace, iSysBoiler, iSysBaseboard )

            Fan_AutoSize_improved = 0.0194 * ss_capacity(iSystem)

          case ( iSysASHP, iSysGSHP, iSysGSHP_GCEP )

            Fan_AutoSize_improved = 0.0467 * ss_capacity(iSystem)

          case ( iSysIMS_P10 )

            ! What to do here?

          case default

            ! Can't happen - ihvac_type tested earlier

        end select HVAC_Type

      endif Sys_Has_Fan
      
      return
      end

c *********************************************************************
c *********************************************************************
c FAN_ENERGY_CON

c Created by: Kamel Haddad
c Initial Creation Date: July 18th 2000
c Copyright 2000: NRCan Buildings Group
c
c Function to determine the indoor circulation fan energy consumption 

c References:	
c		"Further developments to the HOT2000 subroutine - Part B:
c              	furnace an heat pump models", prepared for Energy Mines 
c              	and Resource Canada, Energy Efficiency Division. 
c              	Submitted by C. Barringer. Report No. ET-91-045

c		Furnace Model for Implementation in H3K
c		H3K model documentation,
c		Kamel Haddad, April 5th 2000

c		Air-Source Heat Pump Model for Implementation in H3k
c		H3k model documentation, Kamel Haddad, June 10 2000


c INPUTS:
c	isys		number for hvac system
c	plrp		part-load ratio of the primary system
c	plrb		part-load ratio of the backup system
c       plfp            part-load factor of primary system
c       plfb            part-load factor of backup system
c    	PERS		common containing number of time steps per hour
c	HVAC_INPUT_DATA	common for HVAC input data
c	HEAT_PUMP_INPUT_DATA
c			common for heat pump input data

c OUTPUTS:
c       FAN_ENERGY_CON	energy consumption of the indoor fan in J
c **********************************************************************

      REAL FUNCTION FAN_ENERGY_CON(isys,plrp,plrb,plfp,plfb)

      IMPLICIT NONE

c Variables passed in call
      INTEGER isys

      REAL plrp,plrb,plfp,plfb

      include "building.h"  
      include "hvac_parameters.h"
      include "hvac_common.h"
      include "ashp_common.h"


c Common for the number of time steps per hour
      COMMON/PERS/ISD1,ISM1,ISD2,ISM2,ISDS,ISDF,NTSTEP

      INTEGER ISD1,ISM1,ISD2,ISM2,ISDS,ISDF,NTSTEP

c Declare local variables
      REAL time_step

      REAL FanPlr
      real fHVAC_PL_primary
      real fHVAC_PL_backup 
      
      logical bZero  
      integer iBackupSysIndex

      iBackupSysIndex = iBackup_Sys_num(isys)
      
c Simulation time step
      time_step = 3600. / float(NTSTEP)
      write (*,*) " ---------- Fan --------------------------"
      write(*,*) "isys", isys
      write(*,*) "plfp, plfb", plfp, plfb
      write(*,*) "plrp, plrb", plrp, plrb
      write(*,*) "ihvac_type(isys)", ihvac_type(isys) 
      write(*,*) "iunit_function(isys)",iunit_function(isys)
      write(*,*) "ifan_operation(isys)", ifan_operation(isys)
      
      FanMode: if ( ifan_operation(isys) == 0 ) then 
c if there is no circulation fan, set power to zero. 
        fHVAC_PL_primary = 0.      
        fHVAC_PL_backup  = 0.
      elseif(ifan_operation(isys) .eq. 2) then
c If the circulation fan is in continuous mode, compute power.
c Equation 12 in Furnace Ref by Haddad
        fHVAC_PL_primary = 1.
        fHVAC_PL_backup  = 0.
      elseif(ifan_operation(iSys) .eq. 1) then 
C Circulation fan is in auto mode: calculation depends on 
C part-load characteristics of heating type, and HRV operation. 
               
C Compute part-load ratio of primary HVAC system                
        call eClose ( plrp, 0.0, 0.001, bZero )
        
        Primary_Zero: if ( bZero ) then
          fHVAC_PL_primary = 0. 
        else 
          fHVAC_PL_primary = plrp / (plfp + 0.00001)       
        endif  Primary_Zero
          
C Compute part-load ratio of secondary hvac system           
        call eClose ( plrb, 0.0, 0.001, bZero )
        
        Backup_Zero: if ( bZero ) then
          fHVAC_PL_backup = 0. 
        else         
          if ( ifan_operation(ibackup_sys_num(isys)) .ne. 0 ) then 
C Back-up system doesn't use a fan (distributed baseboard?)           
            fHVAC_PL_backup = 0.
          else         
          
            fHVAC_PL_backup = plrb / (plfb + 0.00001)
            
          endif 
        endif Backup_Zero
      
      endif FanMode

C Check if fan control loop is active for this system. If so, save
C part-load ratios for later and do not compute fan energy consumption
C ( it will be evaluated later ). Otherwise, compute fan energy consumption 
C normally 
      
      FanCtlLoopActive: if ( bSysHasFanCtlLoop(iSys) ) then

C.......Primary system fan requirement and power consumption 

C.......High-speed (on) 
        fFanSysPLR_hi(iSys)    = fHVAC_PL_primary 
        fFanSysPower_hi(iSys)  = fan_power(iSys)
        
C.......Low-speed stage is not fully supported as of yet.         
        fFanSysPLR_lo(iSys)    = 0.
        fFanSysPower_lo(iSys)  = 0.
        
C.......Off. Setting fan_power_auto to nonzero --- which is possible
C.......with the AC models---approximates 2-stage continuious operation. 
        fFanSysPower_off(iSys) = fan_power_auto(iSys)
        
C.......Backup system 
        BackupFan: if ( iBackupSysIndex > 0 ) then 
        
          fFanSysPLR_hi(iBackupSysIndex)    = fHVAC_PL_backup
          fFanSysPower_hi(iBackupSysIndex)  = fan_power(iBackupSysIndex)
          
C.........Low-speed stage is not supported as of yet.         
          fFanSysPLR_lo(iBackupSysIndex)    = 0.
          fFanSysPower_lo(iBackupSysIndex)  = 0.
          
C.........Off. Setting fan_power_auto to nonzero (which is possible
C.........with the AC models) approximates 2-stage continuious operation. 
          fFanSysPower_off(iBackupSysIndex) = 
     &                                 fan_power_auto(iBackupSysIndex)
        
        endif BackupFan
              
      
      else
      
        FanPlr = fHVAC_PL_primary + fHVAC_PL_backup 
        
        if ( FanPlr > 1. ) FanPlr = 1.

        FAN_ENERGY_CON = 
     &         FanPlr       * ( time_step * fan_power(isys) )
     &       + (1.-FanPlr ) * ( time_step * fan_power_auto(isys) )
        
      endif FanCtlLoopActive
        
      write(*,*) "time_step", time_step
      write(*,*) "fan_power", fan_power(isys)
      write(*,*) "fan_power_auto", fan_power_auto(isys) 
      write(*,*) "fHVAC_PL", fHVAC_PL_primary, fHVAC_PL_backup
      write(*,*) "FanPlr", FanPlr
      write(*,*) "FAN_ENERGY_CON", FAN_ENERGY_CON
      write(*,*) " done "
 
     
      return 
      end function FAN_ENERGY_CON
      
c *********************************************************************
c *********************************************************************
c DRAFT_FAN_ENERGY

c Created by: Kamel Haddad
c Initial Creation Date: October 11th 2001
c Copyright 2000: NRCan Buildings Group
c
c Function to determine the furnace/boiler draft fan energy consumption.

c INPUTS:
c       isys            hvac system number associated with furnace/boiler
c	plr		part-load ratio of furnace/boiler
c       plf             part-load factor of furnace/boiler
c    	PERS		common containing number of time steps per hour
c       fan_power	draft fan power

c OUTPUTS:
c       DRAFT_FAN_ENERGY	
c                       energy consumption of furnace/boiler draft fan
c                       in J
c *********************************************************************
      REAL FUNCTION DRAFT_FAN_ENERGY(isys,fan_power,plr,plf)

      IMPLICIT NONE

c Variables passed in call
      INTEGER isys

      REAL fan_power,plr,plf

c Common for the number of time steps per hour
      COMMON/PERS/ISD1,ISM1,ISD2,ISM2,ISDS,ISDF,NTSTEP

      INTEGER ISD1,ISM1,ISD2,ISM2,ISDS,ISDF,NTSTEP

c Declare local variables
      REAL time_step

c Simulation time step
      time_step = 3600. / float(NTSTEP)

c Draft fan energy consumption for the time step
      DRAFT_FAN_ENERGY = time_step * fan_power * plr / plf

      return
      end      
