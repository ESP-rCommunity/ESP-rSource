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
c 	BASEBOARD  simulate performance of baseboard system
c *********************************************************************
c *********************************************************************
c BASEBOARD

c Created by: Kamel Haddad
c Initial Creation Date: December 12th 2000
c Copyright 2000: NRCan Buildings Group
c
c Subroutine to determine the energy consumption of a baseboard/hydronic/
c plenum heater system. For this system there is no loss of 
c efficiency at part-load

c INPUTS:
c	hvac_data	common containing the HVAC data
c	pers		common containing the number of time steps per hour
c	isys		HVAC system number
c       heating_load	total combined heating load of the zones served
c			by hvac system in W
c       HVAC_INPUT_DATA Common for HVAC input data
c       FURNACE_INPUT_DATA
c                       Furnace data common
c       UNDER_HEAT_COOL_HOURS
c                       Common for total hours of under heating and under 
c                       cooling

c OUTPUTS:
c	baseboard_energy	
c			total energy consumed by baseboard system in J.
c       plr_baseboard   part-load ratio of baseboard
c       plr_baseboard   part-load factor of baseboard
c       H3KSTORE_DATA   Common for save level 5 data
c ***********************************************************************
      SUBROUTINE BASEBOARD(isys,heating_load,baseboard_energy,
     &plr_baseboard,plf_baseboard)

      IMPLICIT NONE

c Variables passed in call
      INTEGER isys
      REAL heating_load, baseboard_energy, plr_baseboard, plf_baseboard
#include "building.h"
#include "h3kstore_parameters.h"
#include "hvac_parameters.h"
#include "hvac_common.h"
#include "furnace_common.h"
#include "h3kstore_common.h"



c hvac system file and flag:  	ihvacflag=1 there is hvac file
c				ihvacflag=0 no hvac file
      common/hvacinfo/ihvacflag,hvacfile
      CHARACTER*72 hvacfile
      INTEGER ihvacflag

c Common for the number of time steps per hour
      COMMON/PERS/ISD1,ISM1,ISD2,ISM2,ISDS,ISDF,NTSTEP

      INTEGER ISD1,ISM1,ISD2,ISM2,ISDS,ISDF,NTSTEP

c Local variables
      REAL time_step,PLR
      
c If the baseboard is not a backup system, the part-load ratio has not 
c been estimated yet at this point. If the baseboard is a backup system,
c the part-load ratio is set in the primary system subroutine and its 
c value is passed on to this subroutine in plr_baseboard.
      if(ipriority(isys).eq.1) then
        plr_baseboard = PLR(heating_load,ss_capacity(isys),
     &  fan_power(isys),ifan_operation(isys))
c Call subroutine to set the controller heating capacity for each zone 
c served by furnace
        call SET_CONT_FUNC_HEAT_CAP(isys,ss_capacity(isys))
      endif

c If plr_baseboard > 1, the baseboard does not have enough capacity to meet
c the load for the hour. In this case keep track of the number of hours the
c space is under heated.
      if(plr_baseboard.gt.0.999) then
        plr_baseboard = 1.
        ihrs_under_heat(isys) = ihrs_under_heat(isys) + 1
      endif

c Simulation time step in seconds based on the number of time steps per
c hour
      time_step = 3600. / float(ntstep)

c Part-load factor of baseboard system
      plf_baseboard = 1.0

c Baseboard energy consumption 
      baseboard_energy = ss_capacity(isys) * time_step * plr_baseboard / 
     &ss_efficiency(isys)

      return
      end
