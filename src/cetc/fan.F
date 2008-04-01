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
c	FAN_SIZE	determines the size of the indoor circulation fan
c	FAN_ENERGY_CONSUMPTION
c			determines the energy consumption of the fan
c *********************************************************************
c *********************************************************************
c FAN_SIZE

c Created by: Kamel Haddad
c Initial Creation Date: July 18th 2000
c Copyright 2000: NRCan Buildings Group
c
c Function to determine the size of the indoor circulation fan associated
c with the hvac system

c References:	
c		"Further developments to the HOT2000 subroutine - Part B:
c               furnace an heat pump models", prepared for Energy Mines 
c               and Resource Canada, Energy Efficiency Division. 
c               Submitted by C. Barringer. Report No. ET-91-045

c		Furnace Model for Implementation in H3K
c		H3K model documentation,
c		Kamel Haddad, April 5th 2000

c		Air-Source Heat Pump Model for Implementation in H3k
c		H3k model documentation, Kamel Haddad, June 10 2000


c INPUTS:
c	isys		number for hvac system
c	HVAC_INPUT_DATA	common for HVAC input data
c	HEAT_PUMP_INPUT_DATA
c			common for heat pump input data
c	
c OUTPUTS:
c       FAN_SIZE	size of the indoor circulation fan in W
c **********************************************************************
      REAL FUNCTION FAN_SIZE(isys)

      IMPLICIT NONE

c Variable passed in call
      INTEGER isys

#include "hvac_parameters.h"

#include "building.h"  

#include "hvac_common.h"

#include "ashp_common.h"

c Declaration of local variables
      REAL power1,power2

c The suggested fan power is based on the following report:
c       "Further developments to the HOT2000 subroutine - Part B:
c        furnace and heat pump models", prepared for Energy Mines 
c        and Resource Canada, Energy Efficiency Division. 
c        Submitted by C. Barringer. Report No. ET-91-045

c If the primary system is a furnaces, boilers, or baseboards, then fan power
c is based on the capacity of the heating equipment
      if((ihvac_type(isys) .eq. 1).or.(ihvac_type(isys) .eq. 2).or.
     &  (ihvac_type(isys) .eq. 3)) then
          FAN_SIZE = 0.0194 * ss_capacity(isys)
c For heat pumps, the circulation fan is sized based on the heating capacity
c of the heat pump or that of the backup system if this system employs a 
c circulation fan. The fan size chosen is the larger of the two fan sizes
c based on the heating capacity of the heat pump and the backup heat source.
c This is for heat pumps in the heating mode.
      elseif (((ihvac_type(isys) .eq. 7).and.
     &      (iunit_function(isys) .eq. 1))
     &       .or.((ihvac_type(isys) .eq. 8)
     &       .and.(iunit_function(isys) .eq. 1))
     &       .or.((ihvac_type(isys) .eq. 9)
     &       .and.(iunit_function(isys) .eq. 1))) then

c If the backup source is baseboards without a circulation fan, the capacity
c of the fan is based on the air-source heat pump capacity
        if((ibackup_heat(isys) .eq. 3).and.
     &    (ifan_operation(ibackup_sys_num(isys)) .eq. 0)) then
c Equation 27 in Air-source heat pump Ref by Haddad
          FAN_SIZE = 0.0467 * ss_capacity(isys)
c If the backup source is baseboards with a circulation fan, the capacity
c of the fan is based on the max of two capacities
        elseif((ibackup_heat(isys) .eq. 3).and.
     &  ((ifan_operation(ibackup_sys_num(isys)) .eq. 1).or.
     &  (ifan_operation(ibackup_sys_num(isys)) .eq. 2))) then 
          power1 = 0.0467 * ss_capacity(isys)
          power2 = 0.0194 * ss_capacity(ibackup_sys_num(isys))
          FAN_SIZE = amax1(power1,power2)
c If the backup source is a furnace or boiler, the size of the fan is 
c based on the max of two capacities
        elseif((ibackup_heat(isys) .eq. 1).or.
     &        (ibackup_heat(isys) .eq. 2)) then
          power1 = 0.0467 * ss_capacity(isys)
          power2 = 0.0194 * ss_capacity(ibackup_sys_num(isys))
          FAN_SIZE = amax1(power1,power2)
        endif

c For heat pumps in the cooling mode. There is no backup heat and
c fan size is set by cooling and heating capacities.
      elseif (((ihvac_type(isys) .eq. 7) .and.
     &         (iunit_function(isys) .eq. 2))
     &         .or. ((ihvac_type(isys) .eq. 8) 
     &         .and.(iunit_function(isys) .eq. 2))
     &         .or.((ihvac_type(isys) .eq. 9)
     &         .and.(iunit_function(isys) .eq. 2))) then

        if ( isys_num_heating(isys) == 0 ) then
          power1 = 0.
        else
          power1 = fan_power(isys_num_heating(isys))
        endif
        power2 = 0.0467 * ss_capacity(isys)
        FAN_SIZE = amax1(power1,power2)
      endif

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

#include "building.h"  

#include "hvac_parameters.h"

#include "hvac_common.h"

#include "ashp_common.h"

c Common for the number of time steps per hour
      COMMON/PERS/ISD1,ISM1,ISD2,ISM2,ISDS,ISDF,NTSTEP

      INTEGER ISD1,ISM1,ISD2,ISM2,ISDS,ISDF,NTSTEP

c Declare local variables
      REAL time_step

c Simulation time step
      time_step = 3600. / float(NTSTEP)

c For furnaces, boilers, baseboards:
      if((ihvac_type(isys) .eq. 1).or.(ihvac_type(isys) .eq. 2).or.
     &   (ihvac_type(isys) .eq. 3)) then
c if there is no circulation fan
        if(ifan_operation(isys) .eq. 0) then
          FAN_ENERGY_CON = 0.
c If the circulation fan is in auto-mode
        elseif(ifan_operation(isys) .eq. 1) then
c Equation 13 in Furnace Ref by Haddad
          FAN_ENERGY_CON = time_step * fan_power(isys) * 
     &                     plrp / (plfp + 0.00001)
c If the circulation fan is in continuous mode
        elseif(ifan_operation(isys) .eq. 2) then
c Equation 12 in Furnace Ref by Haddad
          FAN_ENERGY_CON = time_step * fan_power(isys)
        endif

c For air-source and ground-source heat pumps in the cooling mode:
      elseif(
     &   ((ihvac_type(isys).eq.7).and.(iunit_function(isys).eq.2)).or.
     &   ((ihvac_type(isys).eq.8).and.(iunit_function(isys).eq.2)).or.
     &   ((ihvac_type(isys).eq.9).and.(iunit_function(isys).eq.2))) then
c if there is no circulation fan
        if(ifan_operation(isys) .eq. 0) then
          FAN_ENERGY_CON = 0.
c If the circulation fan is in auto-mode
        elseif(ifan_operation(isys) .eq. 1) then
c Equation 13 in Furnace Ref by Haddad
          FAN_ENERGY_CON = time_step * fan_power_auto(isys) * 
     &    plrp / (plfp + 0.00001) + time_step * (fan_power(isys) -
     &    fan_power_auto(isys)) 
c If the circulation fan is in continuous mode
        elseif(ifan_operation(isys) .eq. 2) then
c Equation 12 in Furnace Ref by Haddad
          FAN_ENERGY_CON = time_step * fan_power(isys)
        endif

c For heat pumps in heating mode:
      elseif(((ihvac_type(isys).eq.7).and.(iunit_function(isys).eq.1))
     &       .or.((ihvac_type(isys) .eq. 8) .and.
     &       (iunit_function(isys) .eq. 1))
     &       .or.((ihvac_type(isys) .eq. 9) .and.
     &       (iunit_function(isys) .eq. 1))) then

c If the backup system is a furnace or boiler
        if((ibackup_heat(isys) .eq. 1).or.
     &     (ibackup_heat(isys) .eq. 2)) then

c If there is no circulation fan associated with the system
          if(ifan_operation(isys) .eq. 0) then
            FAN_ENERGY_CON = 0.
c If the circulation fan is in auto mode. Both primary and
c backup system have the same fan operation mode
          elseif(ifan_operation(isys) .eq. 1) then
c Equation 29 in Air-source heat pump Ref by Haddad
            FAN_ENERGY_CON = fan_power(isys) *
     &      (plrp / (plfp + 0.00001) + 
     &       plrb / (plfb + 0.00001)) * time_step
c If the circulation fan is in continuous mode
          elseif(ifan_operation(isys) .eq. 2) then
c Equation 30 in Air-source heat pump document by Haddad
            FAN_ENERGY_CON = fan_power(isys) * time_step
          endif

c For a baseboard/hydronic/plenum heater backup system
        elseif(ibackup_heat(isys) .eq. 3) then

c If there is no circulation fan
          if(ifan_operation(isys) .eq. 0) then
            FAN_ENERGY_CON = 0. 
c If the circulation fan is in auto mode and the part-load ratio
c of the primary system is close to 1 (primary system is on all
c the time and backup system is on part of the time). This occurs
c when the load is larger than the capacity of the primary system
          elseif((ifan_operation(isys) .eq. 1).and.
     &           (plrp .gt. 0.999)) then
c Equation 30 in Air-source heat pump document by Haddad
            FAN_ENERGY_CON = fan_power(isys) * time_step
c If the circulation fan in auto mode and part-load ratio less
c then 1 but not zero (primary system is on part of the time 
c and backup system is off). This happens when primary system has
c enough capacity to meet the load
          elseif((ifan_operation(isys) .eq. 1).and.
     &           (plrp .le. 0.999).and.
     &           (plrp .gt. 0.001)) then
c Equation 28 in Air-source heat pump Ref by Haddad
            FAN_ENERGY_CON = fan_power(isys) * plrp * time_step / 
     &                       (plfp + 0.00001)
c If the circulation fan is in auto-mode, part-load ratio of
c primary system is zero (primary system is off and the load is 
c met by backup system), and the baseboard system employs a fan
c in auto mode
          elseif((ifan_operation(isys) .eq. 1).and.
     &           (plrp .lt. 0.001).and.
     &           (ifan_operation(ibackup_sys_num(isys)) .eq. 1)) then
            FAN_ENERGY_CON = fan_power(isys) * plrb * time_step / 
     &                       (plfb + 0.00001)
c If the circulation fan is in auto-mode, part-load ratio of
c primary system is zero (primary system is off and the load is 
c met by backup system), and the baseboard system does not have
c a fan
          elseif((ifan_operation(isys) .eq. 1).and.
     &           (plrp .lt. 0.001).and.
     &           (ifan_operation(ibackup_sys_num(isys)) .eq. 0)) then
            FAN_ENERGY_CON = 0.       
c If the circulation fan is in continuous mode
          elseif(ifan_operation(isys) .eq. 2) then
c Equation 30 in Air-source heat pump document by Haddad
            FAN_ENERGY_CON = fan_power(isys) * time_step
          endif
        
        endif
           
      endif

      return
      end

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
