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

C This file contains the following subroutines:
c   GCEP_load:       calculate the hourly load on the GSHP system

C *********************************************************************

c GCEP_load

c Created by: Patrice Pinel
c Initial Creation Date: October 2002
c
c This subroutine sums the heating and cooling loads for the 
c simulation of the GCEP system. This subroutine is called from HVACSIM 
c once every simulation timestep, once the zone loads are calculated.
c The hourly heating or cooling loads are passed to the main
c GCEP simulation subroutine GCEP_SIM, via the GCEP_INPUT module.
c
c INPUTS:
c     heating_load       heating load of zones served by GCEP system, per timestep
c     cooling_load       cooling load of zones served by GCEP system, per timestep 
C
c OUTPUTS:
c     hourly_HP_hload    hourly average heating ground load on the GCEP
c     hourly_HP_cload    hourly average cooling ground load on the GCEP
c     
c ***********************************************************************
      SUBROUTINE GCEP_load(heating_load,cooling_load,
     &                     HP_Hcapacity,HP_Ccapacity)

      IMPLICIT NONE
c Definition of module for daily loads and EWT. 
C The heating and cooling load will be read in each timestep.
#include "hvac_parameters.h"
#include "h3kstore_parameters.h"
#include "gcep_common.h"



      COMMON/PERS/ISD1,ISM1,ISD2,ISM2,ISDS,ISDF,NTSTEP

C Defining variables used in the subroutine   
      REAL  heating_load, cooling_load
C Heating and cooling capacity of the heat pumps
      REAL  HP_Hcapacity,HP_Ccapacity
      INTEGER  ISD1,ISM1,ISD2,ISM2,ISDS,ISDF,NTSTEP


c The total heating or cooling load on the GSHP system each timestep has been determined 
c in the HVACSIM subroutine. The daily load on the ground heat exchanger will be the 
c sum of the timestep loads over the simulation day - calculated below.

c if the COP is equal to zero, it is badlt or not calculated and is set to infinite
      if (GCEP_HEAT_COP.eq.0.0) then
        GCEP_HEAT_COP=999999
      endif 
      if (GCEP_COOL_COP.eq.0.0) then
        GCEP_COOL_COP=999999
      endif

c If there is a heating load, add the previous timesteps heating load (hourly_heat_load)
c to the current timestep heating load (heating_load) - as calculated in HVACSIM.
      if (heating_load .ge. 0) then 
        if(heating_load.le.HP_Hcapacity) then
           GCEP_ hourly_HP_hload = GCEP_hourly_HP_hload + 
     &                        heating_load/NTSTEP*(1-1/GCEP_HEAT_COP)
        else
            GCEP_hourly_HP_hload = GCEP_hourly_HP_hload +
     &                        HP_Hcapacity/NTSTEP*(1-1/GCEP_HEAT_COP)
          endif
      endif
c If there is a cooling load, add the previous timesteps cooling load (hourly_cool_load)
c to the current timestep cooling load (cooling_load) - as calculated in HVACSIM.
      if (cooling_load .ge. 0) then
         if(cooling_load.le.HP_Ccapacity) then
            GCEP_hourly_HP_cload =GCEP_hourly_HP_cload +
     &                        cooling_load/NTSTEP*(1+1/GCEP_COOL_COP)
         else
            GCEP_hourly_HP_cload =GCEP_hourly_HP_cload +
     &                        HP_Ccapacity/NTSTEP*(1+1/GCEP_COOL_COP)
          endif
      endif

c This subroutine is called every timestep from HVACSIM, whereas the ground loop is only
c simulated once per hour. As such, when the ground loop uses daily_heat/cool_load, these
c values will be the sum of the time step loads over the day.

      return
      end


C *********************************************************************
C *********************************************************************
C GCEP_HEAT_COEFF

c Created by: Julia Purdy
c Initial Creation Date: March 4, 2002
c Copyright 2000: NRCan Buildings Group
c
c Subroutine to set the degradation coefficients of a ground-source heat pump
c when a heating load is placed upon it.

c Reference:
c       Water Source Heat Pump Data for HOT3000 - Milestone #1
c       Caneta Research Inc., December 7, 2001

c INPUTS:
c    isys    number of HVAC system
c    GCEP_EWT        daily entering water temperature.
   
c OUTPUTS:
c     Cd        degradation coefficient

c **********************************************************************
       SUBROUTINE GCEP_HEAT_COEFF(isys)



c Use heat pump input data module (contains ahp, bhp, chp, dhp values)


       IMPLICIT NONE
c Use HVAC input data module (contains isys information)
#include "hvac_parameters.h"
#include "building.h"
#include "hvac_common.h"
#include "gcep_common.h"
#include "ashp_common.h"
#include "gshp_common.h"
       INTEGER isys
       REAL Cd, degrad_coeff

c The degradation coefficient, Cd, is a function of entering water temperature (EWT).
c The relationship between Cd and EWT is given in Figure 1 and 2 of the Caneta report,
c referenced above.
c In the report, the relationship is given in terms of EWT in degrees F, i.e., for 
c heating: Cd = -0.002069 * EWT + 0.271867. These relationships were modified 
c to allow the EWT to be input in degrees Celcius.
       Cd(isys) = -0.0037242 * GCEP_EWT + 0.205659

C This temporary variable is used for the call the the GSHP_COEFF subroutine.     
       degrad_coeff = Cd(isys)

c The following subroutine calculates the correlation coefficients based on Cd.  
       CALL GSHP_COEFF(degrad_coeff,isys)

       return
       end


C *********************************************************************
C *********************************************************************
C GCEP_COOL_COEFF

c Created by: Julia Purdy
c Initial Creation Date: March 4, 2002
c Copyright 2000: NRCan Buildings Group
c
c Subroutine to set the degradation coefficients of a ground-source heat pump
c system when a cooling load is placed upon it.

c Reference:
c       Water Source Heat Pump Data for HOT3000 - Milestone #1
c       Caneta Research Inc., December 7, 2001

c INPUTS:
c    isys   number of HVAC system
c    GCEP_EWT        entering water temperature
   
c OUTPUTS:
c     Cd        degradation coefficient

c **********************************************************************
       SUBROUTINE GCEP_COOL_COEFF(isys)

c entering water temperature data


       IMPLICIT NONE
#include "hvac_parameters.h"
#include "building.h"
#include "hvac_common.h"
#include "gcep_common.h"
#include "ashp_common.h"
#include "gshp_common.h"
       INTEGER isys
       REAL Cd, degrad_coeff

c The degradation coefficient, Cd, is a function of entering water temperature (EWT).
c The relationship between Cd and EWT is given in Figure 1 and 2 of the Caneta report,
c referenced above.
c In the report, the relationship is given in terms of EWT in degrees F, i.e.,
c Cd = 0.000949 * EWT + 0.083088. These relationships were modified to allow
c input of EWT in degrees Celcius.

c For GSHP in cooling mode:
       Cd(isys) = 0.0017082 * GCEP_EWT + 0.113456

C This temporary variable is used for the call the the GSHP_COEFF subroutine.     
       degrad_coeff = Cd(isys)

c The following subroutine calculates the correlation coefficients based on Cd.
       CALL GSHP_COEFF(degrad_coeff,isys)

       return
       end


C *********************************************************************
c *********************************************************************
c GCEP_pump_power

c Created by: Julia Purdy
c Initial Creation Date: June 25, 2002
c Copyright 2000: NRCan Buildings Group
c
c This function calculates the pump power required for a ground-
c source heat pump system.

c REFERENCE:
c       Water Source Heat Pump Data for HOT3000 - Milestone #1
c       Caneta Research Inc., December 7, 2001
c
c       ASHRAE Handbook of Fundamentals, 2001.

c INPUTS:
c       igshp_type        type of GSHP system selected in the input file
c       GSHP_CAP          system capacity at operting conditions, W
c       pipe_length       total ground circuit pipe length, m
c       HP_pressure_drop  pressure drop through heat pump loop, kPa
c       runout_press_drop run-out pressure drop, kPa
c       circ_press_drop   ground loop circuit pressure drop, kPa
c       runout_fit_drop   run-out fitting pressure drop, kPa
c       circ_fit_drop     ground loop circuit fitting pressure drop, kPa

c OUTPUTS:
c       GCEP_pump_power   circulating fluid pump power, J
c **********************************************************************

      REAL FUNCTION GCEP_pump_power(GCEP_CAP)


C       USE GCEP_INPUT
       IMPLICIT NONE
#include "gcep_parameters.h"
#include "gcep_common.h"

c Common for the number of time steps per hour
       COMMON/PERS/ISD1,ISM1,ISD2,ISM2,ISDS,ISDF,NTSTEP

       common/gcepinfo/igcep
       common/gcepfile/gcepfile

       INTEGER igcep  
       character gcepfile*72 
       INTEGER ISD1,ISM1,ISD2,ISM2,ISDS,ISDF,NTSTEP

c Local variables
       REAL time_step, GCEP_CAP
       REAL HP_pressure_drop, runout_press_drop, runout_fit_drop
       REAL circ_fit_drop, circ_press_drop
       REAL GCEP_pump_input


c The pumping power required depends on the configuration of the ground loop, the
c system capacity and the pressure drop across the loop.
c Based on the heat pump capacity and the ground loop configuration, correlations
c have been defined in Table 3 of the Caneta Report (2001) referenced above.

c In the Caneta report, four capacities have been specified: 2, 2.5, 3, and 4 tons. 
c These capacities convert to 7034 W, 8792.5 W, 10551 W, and 14068 W, 
c based on the conversion factor of 1 ton = 12 000 Btu/h = 3.517 kW
c defined in the ASHRAE Handbook of Fundamentals (2001) pg. 37.1

       if (GCEP_CAP .lt. 8792.5) then
          HP_pressure_drop = 26.7 
          runout_press_drop = 3.4
          runout_fit_drop = 0.48
       elseif ((GCEP_CAP .ge. 8792.5) .and. 
     &         (GCEP_CAP .lt. 10551.)) then
          HP_pressure_drop = 17.2 
          runout_press_drop = 4.8
          runout_fit_drop = 0.71   
       elseif ((GCEP_CAP .ge. 10551.) .and.
     &         (GCEP_CAP .lt. 14068.)) then
          HP_pressure_drop = 28.4 
          runout_press_drop = 6.6
          runout_fit_drop = 1.02
       elseif (GCEP_CAP .ge. 14068.) then
          HP_pressure_drop = 34.0 
          runout_press_drop = 11.0
          runout_fit_drop = 1.83          
       endif

c The pressure drop across the fittings in the circuit is not dependent 
c on the heat pump capacity, it is a constant for all capacities and all
c system configurations.
       circ_fit_drop = 0.33    

c The pressure drop across the circuit is not dependent on the heat pump capacity,
c but rather the ground loop configuration. The following defines the circuit
c pressure drop (circ_press_drop) based on system configuration (igcep_type).
c If the system is a vertical single U-tube, 
       circ_press_drop = 0.1855


c The circulating fluid pump input (GCEP_pump_input) in W is:
       GCEP_pump_input = 0.16 * (GCEP_CAP / 3517) * 4.22 * 
     &                   (HP_pressure_drop + runout_press_drop +
     &                   ((GCEP_pipe_length/GCEP_CAP)*circ_press_drop)+
     &                   runout_fit_drop + circ_fit_drop)

c The pump power is then a function of the simulation time step
       time_step = 3600. / float(NTSTEP)

       GCEP_pump_power = GCEP_pump_input * time_step


       return
       end
c******************************************************************************
       





