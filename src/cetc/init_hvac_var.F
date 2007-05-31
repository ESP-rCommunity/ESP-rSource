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

c *********************************************************************
c *********************************************************************
c ZERO_HVAC_VAR

c Created by: Kamel Haddad
c Initial Creation Date: April 9th 2001
c Copyright 2000: NRCan Buildings Group

c Subroutine to initialize variables associated with HVAC modeling

c INPUTS:
c	UNDER_HEAT_COOL_HOURS
c			common for number of hours zones are under 
c			heated or cooled
c	hvac_timestep_parameters
c			common for time step hvac parameters
c	output_acsystem	common for air-conditioning model output
c *********************************************************************
      SUBROUTINE ZERO_HVAC_VAR

      IMPLICIT NONE

      INCLUDE 'building.h'

      INCLUDE 'hvac_parameters.h'

      INCLUDE 'hvac_common.h'

      INCLUDE 'gshp_common.h'

      INCLUDE 'gcep_common.h'

      INCLUDE 'ashp_common.h'



c Declare local variables
      INTEGER i

c Initialize number of hours space is under heated or cooled for each
c of the HVAC systems
      do 10 i = 1,max_sys
        ihrs_under_heat(i) = 0
        ihrs_under_cool(i) = 0
  10  continue
  
c Initialize moisture parameters associated with cooling coil 
c operation
      do 20 i = 1,mcom
        coil_moist_in(i) = 0.
        coil_moist_out1(i) = 0.
        coil_moist_out2(i) = 0.
 20   continue

c Initialize variable cooling capacity of control functions
      do 30 i = 1,max_cont_fun
        cont_fun_cool_cap(i) = 0.
  30  continue

c Initialize conductance associated with outdoor air flow through HVAC
c system
      do 40 i = 1,mcom
        outdoor_air_inf_cond(i) = 0.
  40  continue

c Initialize output variables for air-conditioning system
      do 50 i = 1,max_sys
        total_elec_consumption(i) = 0.
        elec_compressor(i) = 0. 
        elec_outdoor_fan(i) = 0.
        elec_indoor_fan(i) = 0.
        average_cop(i) = 0.
        coil_sensible_load(i) = 0. 
        coil_latent_load(i) = 0.
        coil_total_load(i) = 0.
 50   continue

c Initialize output flag
      output_flag = .false.

c Initialize several variables associated with common GSHP_DAILY:

C Daily totals for heating and cooling loads, summed in GSHP_load subroutine,
c written to an array, so that previous days values can be accessed.
      daily_heat_load = 0.0
      daily_cool_load = 0.0

c Daily entering water temparature, as calculated for the specific ground
c loop configuration. It is initially set to 5.0 degrees Celcius.
      EWT = 5.0

c  The following is a dummy variable used to determine if the GSHP_init subroutine 
c has been called. If it is TRUE then the subroutine has not been called, 
c if it is FALSE, then the subroutine has been called and will not be called again.
      gshp_init_call = .TRUE.

c Load-weighted daily ground source heat pump COP, calculated in 
c weighted_GSHP_cop subroutine.
c      REAL load_weighted_GSHP_COP = 0.0

c Ground source heat pump COP, calculated for heating or cooling mode.
c      REAL GSHP_COP = 0.0

c The following is a dummy variable used to determine if the GCEP_init subroutine 
c has been called. If it is TRUE then the subroutine has not been called, 
c if it is FALSE, then the subroutine has been called and will not be called again.
c      LOGICAL gcep_init_call = .TRUE.

      return
      end
   
