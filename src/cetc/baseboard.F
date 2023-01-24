C This file is part of the ESP-r system.
C Copyright CANMET Energy Technology Centre 
C Natural Resources Canada, Government of Canada
C 2004-.

C ESP-r is free software.  You can redistribute it and/or
C modify it under the terms of the GNU General Public
C License as published by the Free Software Foundation 
C (version 2 or later).

C ESP-r is distributed in the hope that it will be useful
C but WITHOUT ANY WARRANTY; without even the implied
C warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
C PURPOSE. See the GNU General Public License for more
C details.



c ******************** BASEBOARD ********************
c Determines the energy consumption of a baseboard/hydronic/plenum
C heating system. There is no loss of efficiency at part-load.
c Created by Kamel Haddad, December 2000.

c INPUTS:
c  hvac_data    - common containing the HVAC data
c  pers         - common containing the number of time steps per hour
c  isys         - HVAC system number
c  heating_load - total combined heating load of the zones served
c                 by the hvac system (W)
c  HVAC_INPUT_DATA - common for HVAC input data
c  FURNACE_INPUT_DATA - common for furnace data
c  UNDER_HEAT_COOL_HOURS - total hours of under-heating and under-cooling

c OUTPUTS:
c  baseboard_energy - total energy consumed by baseboard system (J)
c  plr_baseboard    - part-load ratio of baseboard
c  plr_baseboard    - part-load factor of baseboard
c  H3KSTORE_DATA    - Common for save level 5 data

      SUBROUTINE BASEBOARD(isys,heating_load,baseboard_energy,
     &plr_baseboard,plf_baseboard)

      IMPLICIT NONE

      INTEGER isys
      REAL heating_load, baseboard_energy, plr_baseboard, plf_baseboard

#include "building.h"
#include "h3kstore_parameters.h"
#include "hvac_parameters.h"
#include "hvac_common.h"
#include "furnace_common.h"
#include "h3kstore_common.h"

c HVAC system file and flag: ihvacflag=1 HVAC file exists
c                            ihvacflag=0 no HVAC file
      common/hvacinfo/ihvacflag,hvacfile
      CHARACTER*72 hvacfile
      INTEGER ihvacflag

      COMMON/PERS/ISD1,ISM1,ISD2,ISM2,ISDS,ISDF,NTSTEP
      INTEGER ISD1,ISM1,ISD2,ISM2,ISDS,ISDF,NTSTEP

c Local variables.
      REAL time_step,PLR
      
c If the baseboard is not a backup system, the part-load ratio has not 
c been estimated at this point. If the baseboard is a backup system,
c the part-load ratio is set in the primary system subroutine and its 
c value is passed here in plr_baseboard.
      if(ipriority(isys).eq.1) then
        plr_baseboard = PLR(heating_load,ss_capacity(isys),
     &  fan_power(isys),ifan_operation(isys))

c Set the controller heating capacity for each zone  served by furnace.
        call SET_CONT_FUNC_HEAT_CAP(isys,ss_capacity(isys))
      endif

c If plr_baseboard > 1, the baseboard does not have enough capacity to meet
c the load for the hour. In this case, keep track of the number of hours the
c space is under-heated.
      if(plr_baseboard.gt.0.999) then
        plr_baseboard = 1.
        ihrs_under_heat(isys) = ihrs_under_heat(isys) + 1
      endif

c Simulation time step (s) based on the number of time steps per hour.
      time_step = 3600. / float(ntstep)

c Part-load factor of baseboard system.
      plf_baseboard = 1.0

c Baseboard energy consumption, 
      baseboard_energy = ss_capacity(isys) * time_step * plr_baseboard / 
     &ss_efficiency(isys)

      return
      end
