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
c This file contains the following routines:
c	ASHP_COOLING	  Model performance of air source heat pump in
c			  cooling mode. Same subrotuine is used to model
c                         ground-source heat pumps.
C       OUTDOOR_AIR_PROPERTIES
c                         Determine properties of outdoor air for the time step
c       RETURN_AIR_PROPERTIES
c                         Determine properties of return air from zones
c	ASHPC_COIL_INLET  Determines inlet conditions to cooling coil
c                         accounting for effect of outdoor air
c	RET_AIR_TEMP	  Determines average temperature of return air 
c			  from zones served by cooling system
c       RET_AIR_TEMP_OA_MIX
c                         Determines temperature of mixed air made up of 
c                         return air flow and outdoor air flow
c	RET_AIR_HUM_RATIO Determines average humidity ratio of return air
c       RET_AIR_HUM_RATIO_OA_MIX
c                         Determines average humidity ratio of mixed made up
c                         of return air flow and outdoor air flow
c	ENTH_MOIST_AIR	  Determines enthalpy of air given temperature and
c			  humidity ratio
c	SPEC_HEAT_AIR	  Determines specific heat of air given humidity
c			  ratio
c	ATM_PRESSURE	  Determines atmospheric pressure given site altitude
c	DENSITY_AIR	  Determines density of dry air given site altitude
c			  temperature and humidity ratio
c	PAR_PRES_AIR	  Determines partial pressure of air given humidity 
c                         ratio and total pressure
c	HUM_RATIO_OA	  Determines humidity ratio of outdoor air given
c			  temperature, relative humidity, and site altitude
c	SYS_FLOW_RATE     Sets value for system flow rate if not specified in 
c			  HVAC input file
c	BYPASS_FACTOR_R	  Determines coil bypass factor at rating conditions
c	ASHPC_CAP	  Determines total cooling capacity of air source
c			  heat pump at current outdoor temperature and inlet
c                         wet-bulb temperature to coil
c	ASHPC_CAP_FLOW_CORR		  
c			  Correction of total cooling capacity of heat 
c			  pump for effect of flow rate if different from that
c                         at rating conditions
c	BYPASS_FACTOR	  Determines coil bypass factor at operating flow
c			  rate
c	SENS_HEAT_RATIO	  Determines coil sensible heat ratio
c	ASHPC_COP	  Determines heat pump cooling mode COP at current
c                         outdoor temperature and inlet wet-bulb temperature
c                         to coil
c	ASHPC_COP_FLOW_CORR
c			  Correction of heat pump cooling mode COP for the 
c			  effect of flow rate
c	ASHP_PLF_OFFCYC_CORR
c			  Correction of part load factor of heat pump for
c			  effect of off cycle power consumption
c	ASHP_ENERGY	  Determines energy consumption of compressor and 
c                         outdoor fan for time step given part load ratio, 
c                         part load factor, capacity, cop, and time step
c	TIME_INTERVAL	  Determines duration of time step given number of time
c			  steps per hour
c       ZONE_MASS_FLOW_RATE
c                         Determines supply air mass flow rate from HVAC system
c                         to each of the zones served by the system
c	COIL_MOIST_BAL	  Determines cooling coil contribution toward the
c			  moisture balance of the zones served by hvac system
c       CONT_FUNC_CAP     Set the sensible cooling capacity of the control 
c                         function associated with the cooling system
c       WET_BULB_DRY_COIL Determines the highest wet-bulb inlet coil temperature
c                         leading to dry coil conditions 
c       HEATPC_ECONOMIZER_TEMP
c                         Simulate performance of temperature based economizer
c                         cycle
c       HEATPC_ECONOMIZER_ENTH
c                         Simulate performance of enthalpy based economizer cycle
C       ASHPC_PLR_PLF_LATENTLOAD
c                         Determine part-load ratio, part-load factor, and latent
c                         load of equipment for the time step
c       SET_TOTALCOOLINGCAP_SHR
c                         Calls appropriate routines to determine total cooling 
c                         capacity and sensible heat ratio of coil
c       SET_COP           Calls appropriate routines to determine equipment COP
c       HUMRATIO_MOISTBAL Determines equivalent humidity ratio with coil inlet
c                         conditions set to return air conditions
c       ACSYSTEM_OUTPUT   Evaluate output variables associated with AC system
c *********************************************************************
c *********************************************************************
c ASHP_COOLING

c Created by: Kamel Haddad
c Initial Creation Date: February 27th 2001
c Copyright 2000: NRCan Buildings Group

c Subroutine to estimate energy consumption of an air-source heat pump
c when used in cooling mode. This includes estimates for compressor and
c outdoor condenser fan
c References:	
c		1) "Air-source cooling mode model for implementation in 
c		    HOT3000", Kamel Haddad, NRCan Internal Report,
c		    February 20th 2001

c		2) "Modifications to the HOT2000 cooling subroutine",
c		    Chris Barringer, NRCan Report No. ES/ESC-87-93,
c		    July 31st 1987

c		3) "DOE-2 Supplement Manual", University of California
c		    Berkley, CA, January 1994

c		4) "Air-source heat pump heating load model for
c		    implementation in HOT3000", NRCan Internal Report

c		5) "Part-load curves for use in DOE-2", Prepared for the
c		    Lawrence Berkley Lab, CDH Energy Corporation, 
c		    Cazenovia N.Y.

c INPUTS:
c	isys		number of hvac system
c	sensible_cooling_load	
c			total sensible cooling load for zones served 
c                       by hvac system isys
c	PERS		time step common
c	HVAC_INPUT_DATA	common for HVAC input data
c	HEAT_PUMP_INPUT_DATA
c			common for heat pump input data
c	GSHP_DAILY	common for ground-source heat pump data
c	GCEP_INPUT	common for ground-source heat pump data
c			(Ecole Polytechnique model)
c       simtim          common for simulation days, hours, etc ...
c       hvac_timestep_parameters
c                       common for several time step HVAC parameters

c OUTPUTS:
c	sys_energy	total energy consumption of heat pump (compressor
c                       and outdoor condenser fan)
c	plr_heat_pump	part load ratio of heat pump
c       plf_heat_pump   part load factor of heat pump	
c **********************************************************************
      SUBROUTINE ASHP_COOLING(isys,sensible_cooling_load,sys_energy,
     &plr_heat_pump,plf_heat_pump)
 
      IMPLICIT NONE

c Variables passed in call
      REAL sensible_cooling_load,sys_energy,plr_heat_pump,plf_heat_pump

      INTEGER isys

#include "building.h"

#include "hvac_parameters.h"

#include "hvac_common.h"

#include "ashp_common.h"

#include "gshp_common.h"

#include "gcep_common.h"

c Common for the number of time steps per hour
      COMMON/PERS/ISD1,ISM1,ISD2,ISM2,ISDS,ISDF,NTSTEP

      INTEGER ISD1,ISM1,ISD2,ISM2,ISDS,ISDF,NTSTEP

c Common for the current and future hours of the simulation
      common/simtim/ihrp,ihrf,idyp,idyf,idwp,idwf,nsinc,its

       INTEGER ihrp,ihrf,idyp,idyf,idwp,idwf,nsinc,its

c Common for the weather data
      COMMON/CLIMI/QFP,QFF,TP,TF,QDP,QDF,VP,VF,DP,DF,HP,HF

      REAL QFP,QFF,TP,TF,QDP,QDF,VP,VF,DP,DF,HP,HF

      common/outin/iuout,iuin

      INTEGER iuout,iuin

c Local variables
      INTEGER ihour_year,i,GSHP_HP_mode

      REAL tout,rhout,wout,hout,cpout,den_out,tdb1,w1,h1,cp1,den1,
     &tdb1_oa_mix,RET_AIR_TEMP_OA_MIX,w1_oa_mix,
     &RET_AIR_HUM_RATIO_OA_MIX,tdb2,w,patm,ATM_PRESSURE,patm2,twb1,
     &TWB_ACMODEL,twb2,bf,BYPASS_FACTOR,wet_bulb_unity_shr,
     &WET_BULB_DRY_COIL,sensible_cooling_cap,ss_cooling_cap,shr,
     &rlatent_load,total_cooling_load,plf_heat_pump_corr,
     &ASHP_PLF_OFFCYC_CORR,cop,time_step,TIME_INTERVAL,
     &humratio_for_moistbal,economizer_cooling_max,w2,w4,
     &ASHP_ENERGY,zone_mass_flow(mcom) 

c Following moist air states are defined:
c	state 1		condition of return air from conditioned zones
c	state 2		condition of air entering cooling coil
c	state 3		apparatus dew point
c	state 4		condition at exit of heat exchanger
c	state 5		used to define sensible process through cooling
c			coil

c Define hour of the year (from 8760 hours) for the simulation during
c the current time step
      ihour_year = ihrp + 24 * (idyp - 1)

c Call subroutine to determine outdour air properties (temperature,
c relative humidity, humidity ratio, enthalpy, specific heat, and density)
      call OUTDOOR_AIR_PROPERTIES(tout,rhout,wout,hout,cpout,den_out)

c Call subroutine to determine properties of return air from zones
c served by HVAC system
      call RETURN_AIR_PROPERTIES(isys,tdb1,w1,h1,cp1,den1)

c If there is economizer control, then call either temperature or 
c enthalpy based economizer subroutine to determine the actual
c outdoor air flow. This outdoor air flow is needed to determine
c the proper inlet air conditions to the coil and to perform the 
c required moisture balance in the spaces.
c First for the case when we have economizer control (icooling_type(isys)=2)
c and the economizer control is temperature based 
c economizer_control(isys)=1 is for integrated temperature based control 
c and 
c ieconomizer_control(isys)=2 is for non-integrated temperature based control  
      if((icooling_type(isys).eq.2).and.
     &   ((ieconomizer_control(isys).eq.1).or.
     &    (ieconomizer_control(isys).eq.2))) then
        call HEATPC_ECONOMIZER_TEMP(sensible_cooling_load,tdb1,
     &       cp1,den1,tout,cpout,den_out,flow_rate(isys),
     &       economizer_min_out_air_flow(isys),
     &       economizer_ind_temp_setpoint(isys),
     &       ieconomizer_control(isys),
     &       economizer_out_temp_limitcon(isys),
     &       outdoor_air_flow(isys,ihour_year),
     &       economizer_cooling_max)
c Second for the case when we have economizer control (icooling_type(isys)=2)
c and the economizer control is enthalpy based 
c ieconomizer_control(isys)=3 is for integrated enthalpy based control 
c and 
c ieconomizer_control(isys)=4 is for non-integrated enthalpy based control 
      elseif((icooling_type(isys).eq.2).and.
     &       ((ieconomizer_control(isys).eq.3).or.
     &       (ieconomizer_control(isys).eq.4))) then
        call HEATPC_ECONOMIZER_ENTH(sensible_cooling_load,tdb1,
     &       h1,cp1,den1,tout,cpout,den_out,hout,flow_rate(isys),
     &       economizer_min_out_air_flow(isys),
     &       economizer_ind_temp_setpoint(isys),
     &       ieconomizer_control(isys),
     &       economizer_out_enth_limitcon(isys),
     &       outdoor_air_flow(isys,ihour_year),
     &       economizer_cooling_max)
      endif

c The return air and outdoor air are mixed. Next call determines
c temperature of mixed air tdb1_oa_mix
      tdb1_oa_mix = RET_AIR_TEMP_OA_MIX(tdb1,cp1,den1,tout,
     &         cpout,den_out,outdoor_air_flow(isys,ihour_year),
     &         flow_rate(isys))

c Determine humidity ratio of mixed air w1_oa_mix
      w1_oa_mix = RET_AIR_HUM_RATIO_OA_MIX(den1,w1,den_out,
     &            wout,outdoor_air_flow(isys,ihour_year),
     &            flow_rate(isys))

c Knowing the properties of the mixed air state, we can determine the 
c inlet conditions to the cooling coil. These conditions can be different
c from mixed ar conditions if there is a circulation fan in front of the coil.
      call ASHPC_COIL_INLET(isys,tdb1_oa_mix,w1_oa_mix,tdb2,w2,den1,cp1)

c Set atmospheric pressure which depends on the site altitude which comes
c from the HVAC input data file
      patm = ATM_PRESSURE(site_altitude)
 
c Atmospheric pressure in mbar
      patm2 = patm / 100.

c Wet-bulb temperature of return air from conditioned zones
      twb1 = TWB_ACMODEL(tdb1,w1,patm2,1)

c Set wet-bulb temperature of air entering coil
      twb2 = TWB_ACMODEL(tdb2,w2,patm2,1)

c Determine bypass factor for actual flow rate. This actual bypass factor
c is based on the bypass factor at rating conditions
      bf = BYPASS_FACTOR(flow_rate(isys),flow_rate_r(isys),
     &bfr(isys))

c Determine highest inlet wet-bulb temperature to the coil that 
c leads to dry coil conditions given the inlet dry bulb
c temperature to the coil. This temperature will be used to 
c determine whether the coil is dry or wet.
      wet_bulb_unity_shr = WET_BULB_DRY_COIL(ss_capacity(isys),
     &                     tout,tdb2,flow_rate(isys),
     &                     flow_rate_r(isys),
     &                     ifan_operation(isys),bf,patm2,
     &                     site_altitude)

c Set the cooling capacity of system accounting for actual operating
c conditions.
      call SET_TOTALCOOLINGCAP_SHR(ihvac_type(isys),GSHP_HP_mode,
     &     ss_cooling_cap,ss_capacity(isys),EWT,shr,flow_rate(isys),
     &     tdb2,w2,den1,bf,patm2,w4,GCEP_EWT,ifan_operation(isys),
     &     twb2,wet_bulb_unity_shr,flow_rate_r(isys),tout,GCEP_HP_mode)

c Determine sensible cooling capacity of unit
      sensible_cooling_cap = ss_cooling_cap * shr

c Set the maximum cooling capacity of the controller associated with
c the operation of the air-conditioning system 
      call CONT_FUNC_CAP(isys,sensible_cooling_cap,icooling_type(isys),
     &     ieconomizer_control(isys),economizer_cooling_max)   

c Given the total sensible load on the coil, the total capacity of
c the unit, and the sensible heat ratio, it is possible to determine
c the part-load ratio and the part-load factor for the time step
      call ASHPC_PLR_PLF_LATENTLOAD(isys,sensible_cooling_load,
     &     plr_heat_pump,plf_heat_pump,ifan_operation(isys),
     &     rlatent_load,shr,fan_power_auto(isys),ss_cooling_cap)


c Set total cooling load on coil: sum of sensible and latent load
      total_cooling_load = sensible_cooling_load + rlatent_load
  
c Correct part load factor for the effect of off-cycle power use for
c air-source heat pump system
      if((isys_type(isys).eq.1).or.(isys_type(isys).eq.2).or.
     &   (isys_type(isys).eq.3)) then
        plf_heat_pump_corr = ASHP_PLF_OFFCYC_CORR(isys_type(isys),
     &  plr_heat_pump,plf_heat_pump)
c For ground-source heat pump systems, no correction is applied
      elseif(isys_type(isys).eq.4) then
        plf_heat_pump_corr = plf_heat_pump
      endif

c Account for the COP variation with environmental conditions
      call SET_COP(ihvac_type(isys),GSHP_HP_mode,ss_cop(isys),EWT,
     &     cop,total_cooling_load,GCEP_EWT,GCEP_HP_mode,
     &     GCEP_COOL_COP,twb2,wet_bulb_unity_shr,tout,flow_rate(isys),
     &     flow_rate_r(isys),ifan_operation(isys))

c Time step for the simulation
      time_step = TIME_INTERVAL(ntstep)

c Set energy consumption of heat pump in the cooling mode
      sys_energy = ASHP_ENERGY(ss_cooling_cap,cop,plr_heat_pump,
     &                         plf_heat_pump_corr,time_step)

c Set mass flow rate to each of the zones served by hvac system isys
      call ZONE_MASS_FLOW_RATE(isys,den1,zone_mass_flow)

c Find an equivalent exit humidity ratio from coil that corresponds to
c same latent load on coil calculated previously (rlatent_load) assuming
c that inlet conditions to the coil are at the same as those of return air
c (state 1). This humidity ratio will be used in the space moisture balance
c to account for the effect of the condensation at the coil. The actual 
c exit humidity ratio from the coil w4 is not used in the space moisture 
c balance because it includes outdoor air contribution. Moisture contribution
c of outdoor air is already accounted for as part of the infiltration load. 
      call HUMRATIO_MOISTBAL(h1,ss_cooling_cap,den1,flow_rate(isys),
     &     tdb1,sensible_cooling_cap,cp1,humratio_for_moistbal)

c Call subroutine to set variables used in space moisture balance to
c account for condensation at the coil.
c If the indoor circulation fan is in auto mode, a supply air flow at
c the equivalent humidity ratio humratio_for_moistbal is delivered to 
c the space during plr_heat_pump fraction of the hour
      if(ifan_operation(isys).eq.1) then
        call COIL_MOIST_BAL(isys,plr_heat_pump,humratio_for_moistbal,
     &                      zone_mass_flow,ntstep)
c When the circulation fan is in continuous mode, the supply air flow
c to the space has a humidity ratio which is the average of the
c equivalent humidity ratio humratio_for_moistbal and the return air 
c humidity ratio w1 accounting for proper fraction cooling unit is on
c and off
      elseif(ifan_operation(isys).eq.2) then
        humratio_for_moistbal = plr_heat_pump * humratio_for_moistbal + 
     &                          (1 - plr_heat_pump) * w1
        call COIL_MOIST_BAL(isys,1.,humratio_for_moistbal,
     &       zone_mass_flow,ntstep)
      endif

c Set the appropriate conductance associated with the outdoor air flow 
c throug the HVAC system
c When the indoor circulation fan is in continuous mode, the outdoor
c air flow is on for the full time step
      if(ifan_operation(isys).eq.2) then
        do 10 i = 1,num_sys_zones(isys)
          outdoor_air_inf_cond(isys_zone_num(isys,i)) = 
     &    den_out * outdoor_air_flow(isys,ihour_year) * 
     &    cpout * sys_zone_cap_frac(isys,i)
 10     continue
c When the indoor circulation fan is in auto mode, the outdoor air flow
c is assumed to be on only for the fraction plr_heat_pump of the time 
c step
      elseif(ifan_operation(isys).eq.1) then
       do 20 i = 1,num_sys_zones(isys)
          outdoor_air_inf_cond(isys_zone_num(isys,i)) = 
     &    den_out * outdoor_air_flow(isys,ihour_year) * 
     &    cpout * sys_zone_cap_frac(isys,i) * plr_heat_pump
 20     continue
      endif

c Call Subroutine to determine various outputs associated with the 
c AC system
      if((idyp.eq.isd1).or.(output_flag)) then
        output_flag = .true.
        call ACSYSTEM_OUTPUT(isys,sys_energy,plr_heat_pump,
     &plf_heat_pump,ss_cooling_cap,shr,time_step)
      endif

c Following are activated only when the time step output for the 
c cooling model is desired
c      rh1 = PCRH2(tdb1,w1,patm2)
c      wf = HUM_RATIO_OA(tf,hf,site_altitude)
c      wp = HUM_RATIO_OA(tp,hp,site_altitude)
     
c      if(output_flag) then
c      write(202,*) wet_bulb_unity_shr,cop,shr,ss_cooling_cap,twb2
c      write(202,*) w2,tdb2,plr_heat_pump,plf_heat_pump,total_cooling_load
c      write(202,*) sensible_cooling_load,rlatent_load,tdb1,w1,rh1
c      write(202,*) tout,wout,hout,twb1,TF
c      write(202,*) TP,wf,wp,den_out,cpout
c      endif

      return
      end

c *********************************************************************
c *********************************************************************
c OUTDOOR_AIR_PROPERTIES

c Created by: Kamel Haddad
c Initial Creation Date: July 22nd 2003
c Copyright 2000: NRCan Buildings Group
c
c Subroutine to determine properties of outdoor air	

c INPUTS:
c     HVAC_INPUT_DATA   module for HVAC input data
c     CLIMI             common for weather data

c OUTPUTS:
c     tout              outdoor air temperature
c     rhout             outdoor air relative humidity
c     wout              outdoor air humidity ratio 
c     hout              outdoor air enthalpy
c     cpout             outdoor air specific heat
c     den_out           outdoor air density
c **********************************************************************
      SUBROUTINE OUTDOOR_AIR_PROPERTIES(tout,rhout,wout,hout,cpout,
     &                                  den_out)

      IMPLICIT NONE

c Variables passed in call
      REAL tout,rhout,wout,hout,cpout,den_out

#include "building.h"

#include "hvac_parameters.h"

#include "hvac_common.h"

c Common for the weather data
      COMMON/CLIMI/QFP,QFF,TP,TF,QDP,QDF,VP,VF,DP,DF,HP,HF

      REAL QFP,QFF,TP,TF,QDP,QDF,VP,VF,DP,DF,HP,HF

c Local variables
      REAL HUM_RATIO_OA,ENTH_MOIST_AIR,SPEC_HEAT_AIR,
     &DENSITY_AIR

c Average outdoor temperature for the time step
      tout = 0.5 * (TF + TP)

c Average outdoor relative humidity for time step
      rhout = 0.5 * (HF + HP)

c Set humidity ratio of outdoor air
      wout = HUM_RATIO_OA(tout,rhout,site_altitude)

c Set outside air enthalpy
      hout = ENTH_MOIST_AIR(tout,wout)
     
c Set specific heat of outside air
      cpout = SPEC_HEAT_AIR(wout)

c Set density of outside air
      den_out = DENSITY_AIR(tout,wout,site_altitude)

      return
      end

c *********************************************************************
c *********************************************************************
c RETURN_AIR_PROPERTIES

c Created by: Kamel Haddad
c Initial Creation Date: July 22nd 2003
c Copyright 2000: NRCan Buildings Group
c
c Subroutine to determine properties of return air	

c INPUTS:
c     HVAC_INPUT_DATA   common for HVAC input data
c     isys              number associated with HVAC system

c OUTPUTS:
c     tdb1              return air temperature
c     w1                return air humidity ratio 
c     h1                return air enthalpy
c     cp1               return air specific heat
c     den1              return air density
c **********************************************************************
      SUBROUTINE RETURN_AIR_PROPERTIES(isys,tdb1,w1,h1,cp1,den1)

      IMPLICIT NONE

c Variables passed in call
      REAL tdb1,w1,h1,cp1,den1

      INTEGER isys

#include "building.h"

#include "hvac_parameters.h"

#include "hvac_common.h"

c Local variables
      REAL RET_AIR_TEMP,RET_AIR_HUM_RATIO,ENTH_MOIST_AIR,
     &SPEC_HEAT_AIR,DENSITY_AIR

c Set dry-bulb temperature of return air from zones served by hvac
c system
      tdb1 = RET_AIR_TEMP(isys)
 
c Set humidity ratio of return air from zones served by hvac system
      w1 = RET_AIR_HUM_RATIO(isys)

c Set enthalpy of return air
      h1 = ENTH_MOIST_AIR(tdb1,w1)

c Set specific heat of moist air
      cp1 = SPEC_HEAT_AIR(w1)

c Set density of dry air
      den1 = DENSITY_AIR(tdb1,w1,site_altitude)

      return
      end

c *********************************************************************
c *********************************************************************
c ASHPC_COIL_INLET

c Created by: Kamel Haddad
c Initial Creation Date: February 27th 2001
c Copyright 2000: NRCan Buildings Group
c
c Subroutine to determine inlet conditions (temperature and humidity 
c ratio) to cooling coil of a heat pump	

c INPUTS:
c	isys		number of hvac system
c	temp		temperature of return air
c	hum_ratio	humidity ratio of return air
c	temp_inlet	actual inlet temp to cooling coil
c	hum_ratio_inlet	actual humidity ratio at the coil inlet
c	density		density of return air
c	specific_heat	specific heat of return air
c	hvac_data	common for hvac systems data
c	CLIMI		weather data common
c	HVAC_INPUT_DATA	HVAC input data common

c OUTPUTS:
c	temp_inlet	temperature of air entering cooling coil
c	hum_ratio_inlet	humidity ratio of air entering cooling coil
c **********************************************************************
      SUBROUTINE ASHPC_COIL_INLET(isys,temp,hum_ratio,temp_inlet,
     &                 hum_ratio_inlet,density,specific_heat)

      IMPLICIT NONE

c Variables passed in call
      REAL temp,hum_ratio,temp_inlet,hum_ratio_inlet,
     &density,specific_heat

      INTEGER isys

#include "building.h"

#include "hvac_parameters.h"

#include "hvac_common.h"

c Common for the weather data
      COMMON/CLIMI/QFP,QFF,TP,TF,QDP,QDF,VP,VF,DP,DF,HP,HF

      REAL QFP,QFF,TP,TF,QDP,QDF,VP,VF,DP,DF,HP,HF

c For conventional and ventilation, when
c the heat pump is on, it is using 100% return air
      hum_ratio_inlet = hum_ratio

c If there is no circulation fan
      if(ifan_operation(isys).eq.0) then
        temp_inlet = temp
c If there is a circulation fan and fan position is blow through
c then we need to account for effect of heat gain from fan
      elseif(((ifan_operation(isys).eq.1).or.
     &       (ifan_operation(isys).eq.2)).and.
     &       (ifan_position(isys).eq.1)) then
        temp_inlet = temp + fan_power(isys) / 
     &             (density * flow_rate(isys) * specific_heat)
c If there is a circulation fan and fan position is draw through
c then no need to account for heat gain from fan
      elseif(((ifan_operation(isys).eq.1).or.
     &       (ifan_operation(isys).eq.2)).and.
     &       (ifan_position(isys).eq.2)) then
        temp_inlet = temp  
      endif

      return
      end

c *********************************************************************
c *********************************************************************
c RET_AIR_TEMP

c Created by: Kamel Haddad
c Initial Creation Date: February 27th 2001
c Copyright 2000: NRCan Buildings Group

c Subroutine to determine average temperature of return air based on
c temperatures of air in the different zones served by hvac system

c INPUTS:
c	isys		number of hvac system
c	pvala		present temperature and sensible load common
c       fvala           future temperature and sensible load common
c	HVAC_INPUT_DATA	HVAC input data common

c OUTPUTS:
c	RET_AIR_TEMP	temperature of return air
c **********************************************************************
      REAL FUNCTION RET_AIR_TEMP(isys)

      IMPLICIT NONE 

      INTEGER isys

#include "building.h"

#include "hvac_parameters.h"

#include "hvac_common.h"

c Commons for present and future zone dry-bulb temperatures 
c and sensible loads
      common/pvala/tpa(mcom),qpa(mcom)
      common/fvala/tfa(mcom),qfa(mcom)

      REAL tpa,qpa,fvala,tfa,qfa

      INTEGER i

      RET_AIR_TEMP = 0.
      do 10 i = 1,num_sys_zones(isys)
        RET_AIR_TEMP = RET_AIR_TEMP + 
     &  sys_zone_cap_frac(isys,i) * 
     &  0.5 * (tfa(isys_zone_num(isys,i)) +
     &  tpa(isys_zone_num(isys,i)))
   10 continue

      return
      end

c *********************************************************************
c *********************************************************************
c RET_AIR_TEMP_OA_MIX

c Created by: Kamel Haddad
c Initial Creation Date: July 20th 2003
c Copyright 2000: NRCan Buildings Group

c Subroutine to determine temperature of mixed air

c INPUTS:
c       tdb1           return air temperature
c       cp1            return air specific heat
c       den1           return air density
c       tout_avg       outdoor air temperature
c       cpout          outdoor air specific heat
c       den_out        outdoor air density
c       outdoor_air_flow
c                      outdoor air flow rate
c       flow_rate      total air flow rate

c OUTPUTS:
c	RET_AIR_TEMP_OA_MIX	
c                      temperature of mixed air
c **********************************************************************
      REAL FUNCTION RET_AIR_TEMP_OA_MIX(tdb1,cp1,den1,tout_avg,
     &              cpout,den_out,outdoor_air_flow,flow_rate)

      IMPLICIT NONE

c Variables passed in call
      REAL tdb1,cp1,den1,tout_avg,cpout,den_out,outdoor_air_flow,
     &flow_rate

c Temperature of return air after mixing with outside air
      RET_AIR_TEMP_OA_MIX = (den1 * (flow_rate - 
     &outdoor_air_flow) * cp1 * tdb1 + 
     &den_out * outdoor_air_flow * 
     &cpout * tout_avg) / 
     &(den1 * (flow_rate - outdoor_air_flow) * 
     &cp1 + den_out * outdoor_air_flow * cpout)

      return
      end

c *********************************************************************
c *********************************************************************
c RET_AIR_HUM_RATIO(isys)

c Created by: Kamel Haddad
c Initial Creation Date: February 27th 2001
c Copyright 2000: NRCan Buildings Group

c Subroutine to determine average humidity ratio of return air
c from zones served by hvac system

c INPUTS:
c	isys		number of hvac system
c	pvalg		present time step humidity ratio common
c       fvalg           future time step humidity ratio common
c	HVAC_INPUT_DATA	HVAC input data common

c OUTPUTS:
c	RET_AIR_HUM_RATIO
c			humidity ratio of air mixture
c **********************************************************************
      REAL FUNCTION RET_AIR_HUM_RATIO(isys)

      IMPLICIT NONE

c Variables passed in call
      INTEGER isys

#include "building.h"

#include "hvac_parameters.h"

#include "hvac_common.h"

c Commons for present and future humidity ratios
      common/pvalg/gpa(mcom)
      common/fvalg/gfa(mcom)

      REAL gpa,gfa

c Local variable
      INTEGER i

      RET_AIR_HUM_RATIO = 0.
      do 10 i = 1,num_sys_zones(isys)
        RET_AIR_HUM_RATIO = RET_AIR_HUM_RATIO + 
     &  sys_zone_cap_frac(isys,i) *  
     &  0.5 * (gfa(isys_zone_num(isys,i)) +
     &  gpa(isys_zone_num(isys,i)))
   10 continue
      
      return
      end

c *********************************************************************
c *********************************************************************
c RET_AIR_HUM_RATIO_OA_MIX(isys)

c Created by: Kamel Haddad
c Initial Creation Date: July 20th 2003
c Copyright 2000: NRCan Buildings Group

c Subroutine to determine humidity of mixed air

c INPUTS:
c       den1            density of return air
c       w1              humidity ratio of return air
c       den_out         density of outdoor air
c       wout            humidity ratio of return air
c       outdoor_air_flow
c                       outdoor air flow rate
c       flow_rate       total air flow rate

c OUTPUTS:
c	RET_AIR_HUM_RATIO_OA_MIX
c			humidity ratio of mixed air
c **********************************************************************
      REAL FUNCTION RET_AIR_HUM_RATIO_OA_MIX(den1,w1,
     &            den_out,wout,outdoor_air_flow,flow_rate)

      IMPLICIT NONE

c Variables passed in call
      REAL den1,w1,den_out,wout,outdoor_air_flow,flow_rate

c Humidity ratio of return air after mixing with outside air
      RET_AIR_HUM_RATIO_OA_MIX = (den1 * (flow_rate - 
     &outdoor_air_flow) * w1 + den_out * 
     &outdoor_air_flow * wout) / 
     &(den1 * (flow_rate - outdoor_air_flow) + 
     &den_out * outdoor_air_flow)
      
      return
      end

c **********************************************************************
c **********************************************************************
c ENTH_MOIST_AIR

c Created by: Kamel Haddad
c Initial Creation Date: February 27th 2001
c Copyright 2000: NRCan Buildings Group

c Subroutine to determine the enthalpy of moist air

c INPUTS:
c	temp		air temperature
c	hum_ratio	humidity ratio of air mixture

c OUTPUTS:
c	ENTH_MOIST_AIR	enthalpy of moist air
c **********************************************************************
      REAL FUNCTION ENTH_MOIST_AIR(temp,hum_ratio)

      IMPLICIT NONE

c Variables passed in call
      REAL temp,hum_ratio
 
      ENTH_MOIST_AIR = 1006 * temp + 
     &                         hum_ratio * (2501000 + 1860 * temp)

      return
      end

c **********************************************************************
c **********************************************************************
c SPEC_HEAT_AIR

c Created by: Kamel Haddad
c Initial Creation Date: February 27th 2001
c Copyright 2000: NRCan Buildings Group

c Subroutine to determine the specific heat of moist air

c INPUTS:
c	hum_ratio	humidity ratio of air mixture

c OUTPUTS:
c	SPEC_HEAT_AIR	specific heat of moist air
c **********************************************************************
      REAL FUNCTION SPEC_HEAT_AIR(hum_ratio)

      IMPLICIT NONE

c Variable passed in call
      REAL hum_ratio

      SPEC_HEAT_AIR = 1006 + hum_ratio * 1860

      return
      end

c **********************************************************************
c **********************************************************************
c ATM_PRESSURE

c Created by: Kamel Haddad
c Initial Creation Date: February 27th 2001
c Copyright 2000: NRCan Buildings Group
c
c Subroutine to determine atmospheric pressure for a given
c site altitude
c
c INPUTS:
c	altitude	site altitude

c OUTPUTS:
c	ATM_PRESSURE	atmospheric pressure
c **********************************************************************
      REAL FUNCTION ATM_PRESSURE(altitude)

      IMPLICIT NONE

c Variables passed in call
      REAL altitude

      ATM_PRESSURE = 101325 * 
     &                   (1 - 2.25577e-5 * altitude)**5.2559

      return
      end

c **********************************************************************
c **********************************************************************
c DENSITY_AIR

c Created by: Kamel Haddad
c Initial Creation Date: February 27th 2001
c Copyright 2000: NRCan Buildings Group

c Subroutine to determine density of air

c INPUTS:
c	altitude	site altitude
c	temp		temperature
c	hum_ratio	humidity ratio

c OUTPUTS:
c	DENSITY_AIR	density of air
c **********************************************************************
      REAL FUNCTION DENSITY_AIR(temp,hum_ratio,altitude)

      IMPLICIT NONE

c Variables passed in call
      REAL temp,hum_ratio,altitude

c Local variables
      REAL patm,ATM_PRESSURE,pa,PAR_PRES_AIR

c Set atmospheric pressure
      patm = ATM_PRESSURE(altitude)

c Partial pressure of air in mixture
      pa = PAR_PRES_AIR(hum_ratio,patm)

c Density of air 
      DENSITY_AIR = pa / (287. * (temp + 273.15))

      return
      end

c **********************************************************************
c **********************************************************************
c PAR_PRES_AIR

c Created by: Kamel Haddad
c Initial Creation Date: February 27th 2001
c Copyright 2000: NRCan Buildings Group

c Subroutine to determine partial pressure of air in moist air 
c mixture

c INPUTS:
c	hum_ratio	humidity ratio
c	tot_pressure	total pressure

c OUTPUTS:
c	PAR_PRES_AIR	partial pressure of air
c ********************************************************************
      REAL FUNCTION PAR_PRES_AIR(hum_ratio,tot_pressure)

      IMPLICIT NONE

c Variables passed in call
      REAL hum_ratio,tot_pressure

      PAR_PRES_AIR = (0.6219 * tot_pressure) / (hum_ratio + 0.6219)

      return
      end

c *********************************************************************
c *********************************************************************
c HUM_RATIO_OA

c Created by: Kamel Haddad
c Initial Creation Date: February 27th 2001
c Copyright 2000: NRCan Buildings Group

c Subroutine to determine humidity ratio of outdoor air

c INPUTS:
c	temp		outdoor air temperature
c	rel_hum		outdoor air relative humidity	
c	altitude	site altitude

c OUTPUTS:
c	HUM_RATIO_OA	outdoor air humidity ratio
c **********************************************************************
      REAL FUNCTION HUM_RATIO_OA(temp,rel_hum,altitude)

      IMPLICIT NONE

c Variables passed in call
      REAL temp,rel_hum,altitude

c Local variables
      REAL patm,ATM_PRESSURE,pvsat,SATVP,pv

c Atmospheric Pressure
      patm = ATM_PRESSURE(altitude)

c Saturation pressure of water vapor at outside air temperature. 
c 100 to account for conversion from mbar to Pa
      pvsat = 100 * SATVP(temp)

c Partial pressure of water vapor in outdoor air
      pv = rel_hum * pvsat / 100.

c Humidity ratio of outdoor air
      HUM_RATIO_OA = 0.62198 * pv / (patm - pv)

      return
      end

c *********************************************************************
c *********************************************************************
c SYS_FLOW_RATE

c Created by: Kamel Haddad
c Initial Creation Date: February 27th 2001
c Copyright 2000: NRCan Buildings Group
c
c Subroutine to determine the flow rate for hvac system if there is 
c no value specified in the input file
c
c INPUTS:
c	isys		number of hvac system
c	HVAC_INPUT_DATA	HVAC input data common
	
c OUTPUTS:
c	SYS_FLOW_RATE	system flow rate
c *********************************************************************
      REAL FUNCTION SYS_FLOW_RATE(isys)

      IMPLICIT NONE

c Variables passed in call
      INTEGER isys
#include "building.h"

#include "hvac_parameters.h"

#include "hvac_common.h"

      SYS_FLOW_RATE = 6.037e-5 * ss_capacity(isys)

      return
      end

c *********************************************************************
c *********************************************************************
c BYPASS_FACTOR_R

c Created by: Kamel Haddad
c Initial Creation Date: February 27th 2001
c Copyright 2000: NRCan Buildings Group

c Subroutine to determine coil bypass factor at rating conditions
c from performance data at rating conditions

c INPUTS:
c	isys		number of hvac system
c	HVAC_INPUT_DATA	HVAC input data common
c	HEAT_PUMP_INPUT_DATA
c			heat pump input data common
	
c OUTPUTS:
c	bfr		bypass factor at rating conditions
c **********************************************************************
      SUBROUTINE BYPASS_FACTOR_R(isys)

      IMPLICIT NONE

c Variables passed in call
      INTEGER isys

#include "building.h"

#include "hvac_parameters.h"

#include "hvac_common.h"

#include "ashp_common.h"

c Local Variables
      REAL tdb1r,den1r,cp1r,w1r,fan_capr,tdb2r,h2r,
     &h4r,tdb4r,w4r,const1,const2,A,B,C,delta,tdb3r

      COMMON/OUTIN/IUOUT,IUIN
      INTEGER iuout,iuin

c Following moist air states are defined:
c	state 1		condition of return air from conditioned zones
c	state 2		condition of air entering cooling coil
c	state 3		apparatus dew point
c	state 4		condition at exit of heat exchanger
c	state 5		used to define sensible process through cooling
c			coil

c Inlet temperature of air at rating conditions
      tdb1r = 26.7

c Density of air at rating conditions
      den1r = 1.1565

c Specific heat of air at rating conditions
      cp1r = 1015

c Humidity ratio of inlet air at rating conditions
      w1r = 0.0112

c Fan capacity at rating
      fan_capr = fan_power_r(isys)

c Correct temperature of inlet air for heat gain from circulation fan.
c If circulation fan position during rating test is blow through
      if(ifan_position_r(isys).eq.1) then
        tdb2r = tdb1r + fan_capr  / 
     &               (den1r * flow_rate_r(isys) * cp1r)
c If the circulation fan during rating test is draw through
      elseif(ifan_position_r(isys).eq.2) then
        tdb2r = tdb1r
      endif

c Enthalpy of air at inlet of coil
      h2r = 1006 * tdb2r + w1r * (2501000 + 1860 * tdb2r)

c Enthalpy at exit of cooling coil
      h4r = h2r - ss_capacity(isys) / 
     &                      (den1r * flow_rate_r(isys))

c Temperature at exit of coil
      tdb4r = tdb2r - shrr(isys) * ss_capacity(isys) /
     &                 (den1r * flow_rate_r(isys) * cp1r)

c Humidity ratio at exit of coil
      w4r = (h4r - 1006 * tdb4r) / (2501000 + 1860 * tdb4r)

c Line between inlet state and apparatus dew point can be determined by
c two coefficients const1 and const2
      const1 = (w1r - w4r) / (tdb2r - tdb4r)
      const2 = w1r - const1 * tdb2r

c Define constants A, B, C
      A = 2.1539e-5
      B = 1.0776e-4 - const1
      C = 4.2065e-3 - const2

c Compute determinant of quadratic equation to be solved
      delta = B**2 - 4 * A * C

c If there is to be a solution, delta has to be greater or equal to zero
c This is temporary, it will have to be written to a report in the future.
      if(delta.lt.0) then
        write(iuout,*) 'Sensible Heat Factor too high or too low'
      elseif(delta.ge.0) then
c Apparatus dew point
        tdb3r = (-B + (B**2 - 4 * A * C)**0.5) / (2 * A)
      endif

c Bypass Factor at rating conditions
      bfr(isys) = (tdb4r - tdb3r) / (tdb2r - tdb3r)

      return
      end

c *********************************************************************
c *********************************************************************
c ASHPC_CAP

c Created by: Kamel Haddad
c Initial Creation Date: February 27th 2001
c Copyright 2000: NRCan Buildings Group
c
c Subroutine to determine total cooling capacity of heat pump from capacity 
c at rating conditions

c INPUTS:
c	ss_cap		steady state capacity at rating conditions
c	twb 		wet-bulb temperature 
c	tout		outdoor dry-bulb temperature
c	flow_rate	operating flow rate of heat pump
c	flow_rate_r	flow rate at rating conditions
c	ifan_operation	mode of operation of circulation fan
	
c OUTPUTS:
c	ASHPC_CAP	cooling capacity of heat pump
c **********************************************************************
      REAL FUNCTION ASHPC_CAP(ss_cap,twb,tout,flow_rate,flow_rate_r,
     &                        ifan_operation)

      IMPLICIT NONE

c Variables passed in call
      REAL ss_cap,twb,tout,flow_rate,flow_rate_r
     
      INTEGER ifan_operation

c Local variables
      REAL a_cap, b_cap, c_cap, d_cap, e_cap, f_cap, 
     &     cooling_cap

c Capcity correction factor. This is based on default curves in DOE-2.1E for 
c the RESYS system SDL-C1
      a_cap = 0.6003404
      b_cap = 0.0022873
      c_cap = -0.0000128
      d_cap = 0.0013898
      e_cap = -0.0000806
      f_cap = 0.0001412

c Set total cooling capacity of heat pump. DOE2.1E correlation based on
c using Deg F
      cooling_cap = ss_cap * (a_cap + b_cap * (9 * twb / 5 + 32) + 
     &c_cap * (9. * twb / 5. + 32.)**2 + 
     &d_cap * (9. * tout / 5. + 32.) +
     &e_cap * (9. * tout / 5. + 32.)**2 +
     &f_cap * (9. * twb / 5. + 32.) * (9. * tout / 5. + 32.))

c Correct capacity for the effect of the flow rate in case there is
c a circulation fan
      if(ifan_operation.ne.0) then
        call ASHPC_CAP_FLOW_CORR(cooling_cap,flow_rate,flow_rate_r)
      endif

      ASHPC_CAP = cooling_cap

      return
      end      

c *********************************************************************
c *********************************************************************
c ASHPC_CAP_FLOW_CORR

c Created by: Kamel Haddad
c Initial Creation Date: February 27th 2001
c Copyright 2000: NRCan Buildings Group

c Subroutine to correct cooling capacity for the effect of flow rate

c INPUTS:
c	cooling_cap	cooling capacity of heat pump
c	flow_rate	operating flow rate of heat pump
c	flow_rate_r	flow rate at rating conditions
	
c OUTPUTS:
c	cooling_cap	
c			capacity corrected for the effect of air flow
c *********************************************************************
      SUBROUTINE ASHPC_CAP_FLOW_CORR(cooling_cap,flow_rate,
     &                               flow_rate_r)

      IMPLICIT NONE

c Variables passed in call
      REAL cooling_cap, flow_rate, flow_rate_r

c Local variables
      REAL flow_ratio, a_flow, b_flow

c Flow ratio
      flow_ratio = flow_rate / flow_rate_r

c Correction correlation based on default curve in DOE-2.1E for system
c RESYS SDL-C76
      a_flow = 0.8
      b_flow = 0.2

c New cooling capacity
      cooling_cap = cooling_cap * (a_flow + b_flow * flow_ratio)

      return
      end

c *********************************************************************
c *********************************************************************
c BYPASS_FACTOR

c Created by: Kamel Haddad
c Initial Creation Date: February 27th 2001
c Copyright 2000: NRCan Buildings Group

c Function to determine coil bypass factor at actual operating conditions

c INPUTS:
c	flow_rate	actual system flow rate
c	flow_rate_r	flow rate at rating conditions
c	bfr		bypass factor at rating conditions
	
c OUTPUTS:
c	BYPASS_FACTOR	coil bypass factor at operating conditions
c **********************************************************************
      REAL FUNCTION BYPASS_FACTOR(flow_rate,flow_rate_r,bfr)

      IMPLICIT NONE

c Variables passed in call
      REAL flow_rate,flow_rate_r,bfr

C Local variables
      REAL const

c Determine constant based on bypass factor at rating conditions
      const = -alog(bfr) * flow_rate_r

c Bypass factor at actual flow rate
      BYPASS_FACTOR = exp(-const / flow_rate)

      return 
      end

c *********************************************************************
c *********************************************************************
c SENS_HEAT_RATIO

c Created by: Kamel Haddad
c Initial Creation Date: February 27th 2001
c Copyright 2000: NRCan Buildings Group

c Function to determine the sensible heat ratio of coil

c INPUTS:
c	ss_cap		steady-state cooling capacity
c	flow_rate	actual system flow rate
c	tdb2		temperature entering coil
c	w2 		humidity ratio entering coil
c	den1		density return air
c	bf		bypass factor of coil
c	patm2		atmospheric pressure in mbar
	
c OUTPUTS:
c	SENS_HEAT_RATIO	coil sensible heat ratio
c **********************************************************************
      REAL FUNCTION SENS_HEAT_RATIO(ss_cap,flow_rate,tdb2,w2,den1,
     &bf,patm2,w4)

      IMPLICIT NONE

c Variables passed in call
      REAL ss_cap,flow_rate,tdb2,w2,den1,bf,patm2,w4

c Local variables
      REAL h2,ENTH_MOIST_AIR,h4,h3,h3kJ,tdb3,TSATH0,w3,tdb4,h5

c Following moist air states are defined:
c	state 1		condition of return air from conditioned zones
c	state 2		condition of air entering cooling coil
c	state 3		apparatus dew point
c	state 4		condition at exit of heat exchanger
c	state 5		used to define sensible process through cooling
c			coil

c Set enthalpy of air entering coil h2
      h2 = ENTH_MOIST_AIR(tdb2,w2)
 
c Set enthalpy at exit of coil h4
      h4 = h2 - ss_cap / (den1 * flow_rate)
      
c Set enthalpy at apparatus dew point
      h3 = h2 - (h2 - h4) / (1 - bf)

c Convert enthalpy from J to kJ
      h3kJ = h3 /1000.

c Temperature at apparatus dew point from enthalpy
      tdb3 = TSATH0(h3kJ,patm2)

c Set humidity ratio at apparatus dew point
      w3 = (h3 - 1006 * tdb3) / (2501000 + 1860 * tdb3)

c Set temperature at exit of coil
      tdb4 = tdb3 + (tdb2 -tdb3) * bf

c Set humidity ratio at exit of coil
      w4 = w3 + (w2 - w3) * bf

c Set enthalpy to define sensible heat factor
      h5 = 1006 * tdb2 + w4 * (2501000 + 1860 * tdb2)

c Deduce sensible heat ratio of coil
      SENS_HEAT_RATIO = (h5 - h4) / (h2 - h4)

      return 
      end 

c *********************************************************************
c *********************************************************************
c ASHPC_COP

c Created by: Kamel Haddad
c Initial Creation Date: February 27th 2001
c Copyright 2000: NRCan Buildings Group
c
c Function to determine air-source heat pump COP in the cooling mode

c INPUTS:
c	ss_cop		steady-state cooling COP at rating conditions
c	twb		wet-bulb of air entering coil
c	tout		outdoor dry-bulb temperature
c	flow_rate	operating flow rate
c	flow_rate_r	flow rate at rating conditions
c	ifan_operation	mode of operation of circulation fan
	
c OUTPUTS:
c	ASHPC_COP	cooling COP at operating conditions
c **********************************************************************
      REAL FUNCTION ASHPC_COP(ss_cop,twb,tout,flow_rate,flow_rate_r,
     &                        ifan_operation)

      IMPLICIT NONE

c Variables passed in call
      REAL ss_cop,twb,tout,flow_rate,flow_rate_r

      INTEGER  ifan_operation

c Local Variables
      REAL a_cop,b_cop,c_cop,d_cop,e_cop,f_cop,cop

c The COP correlation is based on the default curve for system RESYS in
c DOE-2.1E SDL-C11
      a_cop = -0.9617787
      b_cop = 0.0481775
      c_cop = -0.0002311
      d_cop = 0.0032439
      e_cop = 0.0001488
      f_cop = -0.0002952

      cop = ss_cop / (a_cop + b_cop * (9 * twb / 5 + 32.0) + 
     &            c_cop * (9 * twb / 5 + 32.0)**2 + 
     &            d_cop * (9 * tout / 5 + 32.0) + 
     &            e_cop * (9 * tout / 5 +32.0)**2 + 
     &            f_cop * (9 * twb / 5 +32.0) * (9 * tout / 5 +32.0))

c Correct COP for the effect of flow rate if there is a circulation
c fan
      if(ifan_operation.ne.0) then
        call ASHPC_COP_FLOW_CORR(cop,flow_rate,flow_rate_r)
      endif

      ASHPC_COP = cop

      return
      end

c *********************************************************************
c *********************************************************************
c ASHPC_COP_FLOW_CORR

c Created by: Kamel Haddad
c Initial Creation Date: February 27th 2001
c Copyright 2000: NRCan Buildings Group

c Subroutine to correct cooling mode air-source heat pump COP for effect 
c of air flow

c INPUTS:
C	cop		cooling COP of air-source heat pump
c	flow_rate	operating flow rate
c	flow_rate_r	flow rate at rating conditions
	
c OUTPUTS:
c	cop		cooling COP at operating conditions corrected
c			for volume flow rate
c **********************************************************************
      SUBROUTINE ASHPC_COP_FLOW_CORR(cop,flow_rate,flow_rate_r)

      IMPLICIT NONE

c Variables passed in call
      REAL cop,flow_rate,flow_rate_r

c Local variables
      REAL flow_ratio,a_flow,b_flow,c_flow

c Define flow rate ratio
      flow_ratio = flow_rate / flow_rate_r

c Correlation to correct COP is based on default correlation in DOE-2.1E for
c system RESYS SDL-C91
      a_flow = 1.156
      b_flow = -0.1816
      c_flow = 0.0256

c New cooling COP
      cop = cop / (a_flow + b_flow * flow_ratio + 
     &             c_flow * flow_ratio**2)

      return
      end

c *********************************************************************
c *********************************************************************
c ASHP_PLF_OFFCYC_CORR

c Created by: Kamel Haddad
c Initial Creation Date: March 10th 2001
c Copyright 2000: NRCan Buildings Group

c Function to correct air source heat pump power for off cycle power
c consumption

c references:
c	      1)  Air-Source Heat Pump Model for Implementation in H3k
c	          H3k model documentation, Kamel Haddad, June 10 2000

c INPUTS:
c	isys_type	type of ashp equipment
c	plr_heat_pump	part load ratio of air-source heat pump
c       plf_heat_pump	part load factor of heat pump
	
c OUTPUTS:
c	ASHP_PLF_OFF_CYCLE_CORR
c			part load factor corrected for off-cycle power
c **********************************************************************
      REAL FUNCTION ASHP_PLF_OFFCYC_CORR(isys_type,plr_heat_pump,
     &plf_heat_pump)

      IMPLICIT NONE

c Variables passed in call
      REAL plr_heat_pump,plf_heat_pump

      INTEGER isys_type

c Local variables
      REAL pr

c Depending on the type of unit, Henderson defines a correlation constant pr
      if(isys_type.eq.1) then
        pr = 0.01
      elseif(isys_type.eq.2) then
        pr = 0.01
      elseif(isys_type.eq.3) then
        pr = 0.03
      elseif(isys_type.eq.4) then
        pr = 0.
      endif

c The modified part-load factor (according to the Henderson Report)
c Equation 8 in the reference listed
      ASHP_PLF_OFFCYC_CORR = plr_heat_pump / 
     &        (plr_heat_pump / (plf_heat_pump + 0.00001) + 
     &        (1.0 - plr_heat_pump / (plf_heat_pump + 0.00001)) * pr + 
     &         0.00001)

      return
      end

c *********************************************************************
c *********************************************************************
c ASHP_ENERGY

c Created by: Kamel Haddad
c Initial Creation Date: March 10th 2001
c Copyright 2000: NRCan Buildings Group

c Function to determine energy consumption of air source heat pump

c INPUTS:
c	cap		total cooling capacity of heat pump
c	plr_heat_pump	part load ratio of air-source heat pump
c       plf_heat_pump	part load factor of heat pump
c	time_step	time step of simulation
	
c OUTPUTS:
c	ASHP_ENERGY	heat pump energy consumption for the time step
c **********************************************************************
      REAL FUNCTION ASHP_ENERGY(cap,cop,plr_heat_pump,
     &plf_heat_pump,time_step)

      IMPLICIT NONE

c Variables passed in call
      REAL cap,cop,plr_heat_pump,plf_heat_pump,time_step

      ASHP_ENERGY = (cap / cop) * 
     &              (plr_heat_pump / (plf_heat_pump + 0.00001)) * 
     &               time_step

      return
      end

c *********************************************************************
c *********************************************************************
c TIME_INTERVAL

c Created by: Kamel Haddad
c Initial Creation Date: March 10th 2001
c Copyright 2000: NRCan Buildings Group

c Function to determine time step of simulation

c INPUTS:
c	ntstep		number of time steps per hour for simulation
	
c OUTPUTS:
c	TIME_INTERVAL	heat pump energy consumption for the time step
c **********************************************************************
      REAL FUNCTION TIME_INTERVAL(ntstep)

      IMPLICIT NONE

c Variable passed in call
      INTEGER ntstep

      TIME_INTERVAL = 3600. / float(ntstep)

      return
      end

c *********************************************************************
c *********************************************************************
c ZONE_MASS_FLOW_RATE

c Created by: Kamel Haddad
c Initial Creation Date: March 16th 2001
c Copyright 2000: NRCan Buildings Group

c Subroutine to determine mass flow rate for each of the zones served by
c hvac isys

c INPUTS:
c	isys		hvac system number
c	density		return air density
c	HVAC_INPUT_DATA	HVAC input data common
	
c OUTPUTS:
c	zone_mass_flow	hvac mass flow rate to the individual zones
c **********************************************************************
      SUBROUTINE ZONE_MASS_FLOW_RATE(isys,density,zone_mass_flow)

      IMPLICIT NONE

#include "building.h"

#include "hvac_parameters.h"

#include "hvac_common.h"

c Variables passed in call
      REAL density,zone_mass_flow(mcom)

      INTEGER isys

c Local variables 
      INTEGER i

      do 10 i = 1,num_sys_zones(isys)
        zone_mass_flow(isys_zone_num(isys,i)) = 
     &                   density * flow_rate(isys) * 
     &                   sys_zone_cap_frac(isys,i)
  10  continue

      return
      end

c *********************************************************************
c *********************************************************************
c COIL_MOIST_BAL

c Created by: Kamel Haddad
c Initial Creation Date: March 20th 2001
c Copyright 2000: NRCan Buildings Group

c Subroutine to determine contribution of coil cooling toward moisture 
c balance of zones served by hvac system

c INPUTS:
c	isys		hvac system number
c	plr		part-load ratio of hvac system
c	out_hum_ratio	humidity ratio at exit of coil
c	zone_mass_flow	mass flow rates to each of zones served hvac
c			system
c	ntstep		number of time steps per hour
c       fvalg           future time step space humidity ratio common
c	HVAC_INPUT_DATA	HVAC input data common
	
c OUTPUTS:
c	hvac_timestep_parameters		
c                       common for coil parameters affecting zone moisture
c			balance
c **********************************************************************
      SUBROUTINE COIL_MOIST_BAL(isys,plr,out_hum_ratio,zone_mass_flow,
     &                          ntstep)

      IMPLICIT NONE

#include "building.h"

#include "hvac_parameters.h"

#include "hvac_common.h"

c Variables passed in call
      REAL plr,out_hum_ratio,zone_mass_flow(mcom)

      INTEGER isys,ntstep  

c Commons for future zone humidity ratios
      common/fvalg/gfa(mcom)

      REAL gfa

c Local variables
      INTEGER i
 
c For each of the zones served by the HVAC system, determine the 
c contributions of the coil toward the moisture balance inside the space
      do 10 i = 1,num_sys_zones(isys)
        coil_moist_in(isys_zone_num(isys,i)) = 
     &      zone_mass_flow(isys_zone_num(isys,i)) * plr * 
     &      out_hum_ratio * 3600 / float(ntstep)
        coil_moist_out1(isys_zone_num(isys,i)) = 
     &      zone_mass_flow(isys_zone_num(isys,i)) * plr * 
     &      gfa(isys_zone_num(isys,i)) * 3600 / (2. * float(ntstep))  
        coil_moist_out2(isys_zone_num(isys,i)) =
     &      zone_mass_flow(isys_zone_num(isys,i)) * plr *
     &      3600 / (2.* float(ntstep))
   10 continue

      return
      end

c *********************************************************************
c *********************************************************************
c CONT_FUNC_CAP

c Created by: Kamel Haddad
c Initial Creation Date: March 25th 2001
c Copyright 2000: NRCan Buildings Group

c Subroutine to update cooling capacity of appropriate control function
c associated with cooling equipment

c INPUTS:
c	isys		hvac system number
c	sensible_cooling_cap
c                       sensible cooling capacity of equipment
c       icooling_type   type of cooling (conventional or economizer)
c       ieconomizer_control
c                       economizer control type (temperature based or
c                       enthalpy based and integrated or non-integrated)
c       economizer_cooling_max
c                       maximum cooling capacity available from economizer
c	HVAC_INPUT_DATA	HVAC input data common

c OUTPUTS:
c	cont_fun_cool_cap
c			new cooling capacity for hvac system isys. This 
c			new cooling capacity is used later to update the
c			maximum cooling capacity of the control function
c			associated with hvac system isys. Control function
c			capacity is updated in subroutine BCL01 in file 
c			bcfunc.F under esrubld directory. At this point, 
c			the control law associated with the air-source
c			heat pump in the cooling mode is ideal controller
c	hvac_timestep_parameters	
c                       common for updated capacity of control function
c **********************************************************************
      SUBROUTINE CONT_FUNC_CAP(isys,sensible_cooling_cap,icooling_type,
     &           ieconomizer_control,economizer_cooling_max)

      IMPLICIT NONE
#include "building.h"

#include "hvac_parameters.h"

#include "hvac_common.h"

c Variables passed in call
      REAL sensible_cooling_cap,economizer_cooling_max

      INTEGER isys,icooling_type,ieconomizer_control

c Local variables
      REAL controller_sens_cap

c Set cooling capacity of control function associated with 
c cooling equipment
c For conventional HVAC system, set controller capacity to the
c sensible system capacity for time step
      if(icooling_type.eq.1) then
        controller_sens_cap = sensible_cooling_cap
c If there is economizer control and economizer and cooling unit
c can be on at the same time to meet the load (integrated control), 
c then controller capacity is set to the sum of sensible unit capacity 
c and maximum cooling capacity of economizer
      elseif((icooling_type.eq.2).and.
     &        ((ieconomizer_control.eq.1).or.
     &         (ieconomizer_control.eq.3))) then
        controller_sens_cap = sensible_cooling_cap + 
     &                                          economizer_cooling_max
c If there is economizer control and economizer and cooling unit
c can not be on at the same time to meet the load (non-integrated control), 
c then controller capacity is set to sensible unit capacity
      elseif((icooling_type.eq.2).and.
     &        ((ieconomizer_control.eq.2).or.
     &         (ieconomizer_control.eq.4))) then
        controller_sens_cap = sensible_cooling_cap
      endif

c In the following the effect of the circulation fan is accounted for.
c If circulation fan is in auto mode, reduce the capacity of the controller
      if(ifan_operation(isys).eq.1) then
        cont_fun_cool_cap(icontrol_function(isys)) = controller_sens_cap 
     &                                            - fan_power_auto(isys)

c If circulation fan is in continuous mode, no effect on controller capacity
      elseif(ifan_operation(isys).eq.2) then
        cont_fun_cool_cap(icontrol_function(isys)) = controller_sens_cap

c If no circulation fan, no effect on controller capacity
      elseif(ifan_operation(isys).eq.0) then
        cont_fun_cool_cap(icontrol_function(isys)) = controller_sens_cap
      endif

      return
      end

c *********************************************************************
c *********************************************************************
c WET_BULB_DRY_COIL

c Created by: Kamel Haddad
c Initial Creation Date: April 8th 2003
c Copyright 2000: NRCan Buildings Group

c Function to determine the highest wet-bulb temperature leading to dry
c coil conditions for a certain inlet dry-bulb temperature to the coil

c INPUTS:
c	ss_capacity	Steady-state cooling capacity of coil at ARI rating 
c			conditions (W)
c	tout		Outdoor dry-bulb temperature (Deg C)
c	temp_inlet	Inlet dry-bulb temperature to the coil (Deg C)
c	actual_flow_rate
c			actual air flow rate through cooling coil (m3/s)
c	flow_rate_ref	air flow rate at rating test (m3/s)
c	ifan_operation	mode of operation of circulation fan
c	bypass_factor	actual bypass factor of cooling coil
c	pressure	total pressure inside space (mbar)
c	site_altitude	altitude of building site (m)
	
c OUTPUTS:
c	WET_BULB_DRY_COIL	
c			Maximum wet-bulb temperature leading to dry coil
c			conditions for inlet dry-bulb temperature (Deg C)
c **********************************************************************
      REAL FUNCTION WET_BULB_DRY_COIL(ss_capacity,tout,temp_inlet,
     &               actual_flow_rate,flow_rate_ref,
     &               ifan_operation,bypass_factor,pressure,
     &               site_altitude)

      IMPLICIT NONE

c Variables passed in call
      REAL ss_capacity,tout,temp_inlet,actual_flow_rate,flow_rate_ref,
     &     bypass_factor,pressure,site_altitude

      INTEGER ifan_operation

c Local variables
      REAL hum_ratio_dry_coil_upper,hum_ratio_dry_coil_lower,
     &     shr_difference,hum_ratio_dry_coil,ss_cooling_cap,
     &     ASHPC_CAP,density_inlet,DENSITY_AIR,shr,SENS_HEAT_RATIO,    
     &     TWB_ACMODEL,exit_hum_ratio
 
      common/outin/iuout,iuin

      INTEGER max_num_iterations,iuout,iuin
 
c Assume that upper limit of maximum humidity ratio for dry conditions 
c is equal to 0.1
      hum_ratio_dry_coil_upper = 0.1

c Assume that lower limit of maximum humidity ratio for dry conditions 
c is equal to 0.00001
      hum_ratio_dry_coil_lower = 0.00001

c Initialize difference between calculated sensible heat ratio and a
c sensible heat ratio of 1.
      shr_difference = 1.

c Maximum number of iterations
      max_num_iterations = 1

c Iterate to find actual maximum wet-bulb temperature giving dry coil 
c conditions
      do while((shr_difference.gt.0.01).and.
     &         (max_num_iterations.le.100))
c Set estimate for inlet humidity ratio
         hum_ratio_dry_coil = 0.5 * (hum_ratio_dry_coil_upper +
     &                               hum_ratio_dry_coil_lower)
c Find wet_bulb temperature at inlet of coil given the inlet dry-bulb
c temperature and humidity ratio
         wet_bulb_dry_coil = TWB_ACMODEL(temp_inlet,hum_ratio_dry_coil,
     &                           pressure,1)
c Get steady-state capacity
         ss_cooling_cap = ASHPC_CAP(ss_capacity,
     &                    wet_bulb_dry_coil,tout,actual_flow_rate,
     &                    flow_rate_ref,ifan_operation)
c Find density of air at the inlet of coil given the dry-bulb 
c temperature, humidity ratio, and site altitude
         density_inlet = DENSITY_AIR(temp_inlet,hum_ratio_dry_coil,
     &                               site_altitude)
c Determine sensible heat ratio
         shr = SENS_HEAT_RATIO(ss_cooling_cap,actual_flow_rate,
     &         temp_inlet,hum_ratio_dry_coil,density_inlet,
     &         bypass_factor,pressure,exit_hum_ratio)

c Update values of upper and lower humidity ratios for dry coil
c conditions
         if(shr.ge.1.) then
           hum_ratio_dry_coil_lower = hum_ratio_dry_coil
         elseif(shr.le.1.) then
           hum_ratio_dry_coil_upper = hum_ratio_dry_coil
         endif
c Find new sensible heat factor difference
         shr_difference = abs(1. - shr)

c Update maximum number of iterations
         max_num_iterations = max_num_iterations + 1
   
         if(max_num_iterations.eq.100) then
           write(iuout,*) 
     &    'Max number of iterations in FUNCTION WET_BULB_DRY_COIL'
         endif

      end do

c Update function value
      WET_BULB_DRY_COIL = wet_bulb_dry_coil

      return
      end

c *********************************************************************
c *********************************************************************
c SUBROUTINE HEATPC_ECONOMIZER_TEMP

c Created by: Kamel Haddad
c Initial Creation Date: July 20th 2003
c Copyright 2000: NRCan Buildings Group

c Function to determine effect of temperature based economizer on 
c outdoor air flow and space cooling loads

c INPUTS:
c       sensible_cooling_load
c                       space sensible cooling load
c       tdb1            return air dry bulb temperature
c       cp1             return air specific heat
c       den1            return air density
c       tout_avg        outdoor air temperature
c       cpout           outdoor air specific heat
c       den_out         outdoor air density
c       flow_rate       total air flow rate
c       economizer_min_out_air_flow
c                       minimum outdoor air flow for economizer
c       economizer_ind_temp_setpoint
c                       indoor temperature setpoint of economizer
c       ieconomizer_control
c                       economizer control (integrated or non-integrated) 
c       economizer_out_temp_limitcon
c                       maximum outdoor dry-bulb temperature for economizer 
c                       control operation
	
c OUTPUTS:
c       outdoor_air_flow
c                       outdoor air flow
c       economizer_cooling_max
c                       maximum amount of cooling that can be achieved by
c                       economizer
c **********************************************************************
      SUBROUTINE HEATPC_ECONOMIZER_TEMP(sensible_cooling_load,tdb1,
     &       cp1,den1,tout_avg,cpout,den_out,flow_rate,
     &       economizer_min_out_air_flow,economizer_ind_temp_setpoint,
     &       ieconomizer_control,economizer_out_temp_limitcon,
     &       outdoor_air_flow,economizer_cooling_max)

      IMPLICIT NONE

c Variables passed in call
      REAL sensible_cooling_load,tdb1,cp1,den1,tout_avg,cpout,
     &     den_out,flow_rate,economizer_min_out_air_flow,
     &     economizer_ind_temp_setpoint,economizer_out_temp_limitcon,
     &     outdoor_air_flow,economizer_cooling_max

      INTEGER ieconomizer_control

c Local variables
      REAL outdoor_air_fraction_volume,up_outdoor_air_fraction_volume,
     &     error,outdoor_air_mass_flow,recirculated_air_mass_flow,
     &     total_air_mass_flow,outdoor_air_fraction_mass,den_mixed_air,
     &     cp_mix_air,supply_temp,sum1,sum2,economizer_cooling

      INTEGER iteration_counter
 
      common/outin/iuout,iuin

      INTEGER iuout,iuin

c Maximum amount of possible economizer cooling
      if(tout_avg.lt.tdb1) then
        economizer_cooling_max = den_out * flow_rate * 
     &                       (0.5 * (cpout + cp1)) * (tdb1 - tout_avg)
      else
        economizer_cooling_max = 0.
      endif

c If space sensible cooling load > 0 and the outdoor temperature
c is less than maximum temperature for economizer operation and 
c maximum possible economizer cooling is > 0
      if((sensible_cooling_load.gt.0.).and.
     &   (economizer_out_temp_limitcon.ge.tout_avg).and.
     &   (economizer_cooling_max.gt.0.)) then

c If the outdoor temperature is <= to the indoor space temperature and
c the indoor temperature is >= to the indoor economizer set point temperature
c and the maximum cooling available from the economizer is >= to the space
c sensible cooling
        if((tout_avg.le.tdb1).and.
     &     (tdb1.ge.economizer_ind_temp_setpoint).and.
     &     (economizer_cooling_max.ge.sensible_cooling_load)) then

c At this point the outdoor air fraction by volume is not known. A value for
c the outdoor air fraction is assumed first. The assumed outdoor air fraction 
c is then used to find density and specific heat of the mixed air. The density
c and specific heat of mixed air are then used to find the temperature of mixed
c mixed air needed to satisfy the sensible cooling load. The temperature of the
c mixed air is then used to find an updated value for the outdoor air fraction.

c Assume an initial outdoor air fraction by volume
          outdoor_air_fraction_volume = 0.1
          up_outdoor_air_fraction_volume = 0.
          error = 1.
          iteration_counter = 0

c Iterate to find the appropriate outdoor air fraction
          do while((error.gt.0.01).and.(iteration_counter.le.10))
c Mass flow rate of outdoor air
            outdoor_air_mass_flow = 
     &                 den_out * outdoor_air_fraction_volume * flow_rate
c Mass flow rate of recirculated air
            recirculated_air_mass_flow =  
     &             den1 * (1. - outdoor_air_fraction_volume) * flow_rate
c Total air mass flow rate
            total_air_mass_flow = 
     &                outdoor_air_mass_flow + recirculated_air_mass_flow
c Outdoor air fraction on a mass basis
            outdoor_air_fraction_mass = 
     &                      outdoor_air_mass_flow /  total_air_mass_flow
c Set density and specific heat of mixed air
            den_mixed_air = outdoor_air_fraction_mass * den_out + 
     &        (1 - outdoor_air_fraction_mass) * den1
            cp_mix_air = outdoor_air_fraction_mass * cpout + 
     &        (1 - outdoor_air_fraction_mass) * cp1
 
c Find supply temperature of mixed air needed to meet the sensible
c cooling load
            supply_temp = tdb1 - sensible_cooling_load / 
     &                (total_air_mass_flow * (0.5 * (cp_mix_air + cp1)))
            sum1 = den_out * cpout * tout_avg
            sum2 = den1 * cp1 * tdb1

c Check that sum1 is not equal to sum2 to avoid division by 0
            if(sum1.ne.sum2) then
              up_outdoor_air_fraction_volume = 
     &                  (den_mixed_air * cp_mix_air * supply_temp - 
     &                  den1 * cp1 * tdb1) /
     &                  (den_out * cpout * tout_avg - den1 * cp1 * tdb1)
            endif

c Update error associated with value of outdoor_air_fraction_volume 
            error = abs(outdoor_air_fraction_volume - 
     &                                   up_outdoor_air_fraction_volume)

c Update value of outdoor air fraction by volume
            outdoor_air_fraction_volume = up_outdoor_air_fraction_volume
             
c Update value of iteration counter
            iteration_counter = iteration_counter + 1

c Limit the number of iterations to 10 before issuing a warning
            if(iteration_counter.eq.10) then
                write(iuout,*) 
     &    'Max number of iterations reached in HEATPC_ECONOMIZER_TEMP'
            endif
          
          end do

c Set the value of outdoor air flow needed to meet sensible cooling load
c based on economizer operation   
          outdoor_air_flow = outdoor_air_fraction_volume * flow_rate

c In case the required outdoor air flow is less than the minimum required
c outdoor air flow, then set the outdoor air flow to the minimum amount
c allowed.
          if(outdoor_air_flow.lt.economizer_min_out_air_flow) then
            outdoor_air_flow = economizer_min_out_air_flow
          endif

c Since the economizer is capable of meeting the total cooling load, then 
c there is no cooling load to be met by the compressor based air-conditioning 
c system
        sensible_cooling_load = 0.
          
c If the outdoor temperature <= than the indoor temperature and the indoor 
c temperature is >= than the economizer indoor set point temperature and
c the economizer can not meet the sensible load and the economizer control is 
c of the integrated type
        elseif((tout_avg.le.tdb1).and.
     &      (tdb1.ge.economizer_ind_temp_setpoint).and.
     &      (economizer_cooling_max.lt.sensible_cooling_load).and.
     &      (ieconomizer_control.eq.1)) then
c Cooling from the economizer is the total cooling possible from economizer 
c operation
          economizer_cooling = economizer_cooling_max         
c Difference between total sensible load and maximum economizer cooling will 
c be met by the compressor-based cooling system
          sensible_cooling_load = sensible_cooling_load -
     &                                                economizer_cooling
c Outdoor air flow in this case is set to the total air flow
          outdoor_air_flow = flow_rate

c If outdoor temperature <= than the indoor temperature and the indoor 
c temperature is >= than the economizer indoor set point temperature and
c the economizer can not meet the sensible load and the economizer control is 
c of the non-integrated type
        elseif((tout_avg.le.tdb1).and.
     &        (tdb1.ge.economizer_ind_temp_setpoint).and.
     &        (economizer_cooling_max.lt.sensible_cooling_load).and.
     &        (ieconomizer_control.eq.2)) then

c Set outdour air flow to the minimum possible outdoor air flow
          outdoor_air_flow = economizer_min_out_air_flow

c If outdoor temperature > than indoor temperature and indoor temperature
c >= economizer indoor temperature set point temperature
        elseif((tout_avg.gt.tdb1).and.
     &        (tdb1.ge.economizer_ind_temp_setpoint)) then

c Set outdoor air flow to minimum economizer outdoor air flow
          outdoor_air_flow = economizer_min_out_air_flow

c If indoor temperature <= economizer indoor temperature setpoint
        elseif(tdb1.lt.economizer_ind_temp_setpoint) then

c Set outdour air flow to the minimum possible outdoor air flow        
          outdoor_air_flow = economizer_min_out_air_flow
 
        endif
c If sensible cooling load  = 0 or economizer maximum outdoor temperature 
c for limit control <= outdoor temperature or economizer maximum possible cooling 
c = 0
      elseif((sensible_cooling_load.eq.0.).or.
     &       (economizer_out_temp_limitcon.lt.tout_avg).or.
     &       (economizer_cooling_max.eq.0.)) then

c Set outdour air flow to the minimum possible outdoor air flow
        outdoor_air_flow = economizer_min_out_air_flow

      endif

      return
      end

c *********************************************************************
c *********************************************************************
c SUBROUTINE HEATPC_ECONOMIZER_ENTH

c Created by: Kamel Haddad
c Initial Creation Date: July 20th 2003
c Copyright 2000: NRCan Buildings Group

c Function to determine effect of enthalpy based economizer on 
c outdoor air flow and space cooling loads

c INPUTS:
c       sensible_cooling_load
c                       space sensible cooling load
c       tdb1            return air dry bulb temperature
c       h1              enthalpy of return air
c       cp1             return air specific heat
c       den1            return air density
c       tout_avg        outdoor air temperature
c       cpout           outdoor air specific heat
c       den_out         outdoor air density
c       hout            outdoor air enthalpy
c       flow_rate       total air flow rate
c       economizer_min_out_air_flow
c                       minimum outdoor air flow for economizer
c       economizer_ind_temp_setpoint
c                       indoor temperature setpoint of economizer
c       ieconomizer_control
c                       economizer control (integrated or non-integrated) 
c       economizer_out_enth_limitcon
c                       maximum outdoor enthalpy for economizer control 
c                       operation

c OUTPUTS:
c       outdoor_air_flow
c                       outdoor air flow
c       economizer_cooling_max
c                       maximum amount of cooling that can be achieved by
c                       economizer
c **********************************************************************
      SUBROUTINE HEATPC_ECONOMIZER_ENTH(sensible_cooling_load,tdb1,
     &       h1,cp1,den1,tout_avg,cpout,den_out,hout,flow_rate,
     &       economizer_min_out_air_flow,economizer_ind_temp_setpoint,
     &       ieconomizer_control,economizer_out_enth_limitcon,
     &       outdoor_air_flow,economizer_cooling_max)

      IMPLICIT NONE

c Variables passed in call
      REAL sensible_cooling_load,tdb1,h1,cp1,den1,tout_avg,cpout,
     &     den_out,hout,flow_rate,economizer_min_out_air_flow,
     &     economizer_ind_temp_setpoint,economizer_out_enth_limitcon,
     &     outdoor_air_flow,economizer_cooling_max

      INTEGER ieconomizer_control

c Local variables
      REAL outdoor_air_fraction_volume,up_outdoor_air_fraction_volume,
     &     error,outdoor_air_mass_flow,recirculated_air_mass_flow,
     &     total_air_mass_flow,outdoor_air_fraction_mass,den_mixed_air,
     &     cp_mix_air,supply_temp,sum1,sum2,economizer_cooling

      INTEGER iteration_counter

      common/outin/iuout,iuin

      INTEGER iuout,iuin

c Maximum amount of possible economizer cooling
      if(tout_avg.lt.tdb1) then
        economizer_cooling_max = den_out * flow_rate * 
     &                        0.5 * (cpout + cp1) * (tdb1 - tout_avg)
      else
        economizer_cooling_max = 0.
      endif

c If sensible cooling load > 0 and economizer outdoor enthalpy limit
c >= than outdoor enthalpy and economizer maximum cooling load >= 0
      if((sensible_cooling_load.gt.0).and.
     &   (economizer_out_enth_limitcon.ge.hout).and.
     &   (economizer_cooling_max.gt.0.)) then
c If outdoor enthalpy <= return air enthalpy and return air temperature
c >= economizer indoor temperature set point and maximum possible
c economizer cooling >= sensible cooling load
        if((hout.le.h1).and.
     &     (tdb1.ge.economizer_ind_temp_setpoint).and.
     &     (economizer_cooling_max.ge.sensible_cooling_load)) then

c At this point the outdoor air fraction by volume is not known. A value for
c the outdoor air fraction is assumed first. The assumed outdoor air fraction 
c is then used to find density and specific heat of the mixed air. The density
c and specific heat of mixed air are then used to find the temperature of mixed
c mixed air needed to satisfy the sensible cooling load. The temperature of the
c mixed air is then used to find an updated value for the outdoor air fraction.

c Assume an initial outdoor air fraction by volume
          outdoor_air_fraction_volume = 0.5
          up_outdoor_air_fraction_volume = 0.
          error = 1.
          iteration_counter = 0

c Iterate to find the appropriate outdoor air fraction
          do while((error.gt.0.01).and.(iteration_counter.le.10))
c Mass flow rate of outdoor air
            outdoor_air_mass_flow = 
     &                 den_out * outdoor_air_fraction_volume * flow_rate
c Mass flow rate of recirculated air
            recirculated_air_mass_flow =  
     &             den1 * (1. - outdoor_air_fraction_volume) * flow_rate
c Total air mass flow rate
            total_air_mass_flow = 
     &                outdoor_air_mass_flow + recirculated_air_mass_flow
c Outdoor air fraction on a mass basis
            outdoor_air_fraction_mass = 
     &                      outdoor_air_mass_flow /  total_air_mass_flow
c Set density and specific heat of mixed air
            den_mixed_air = outdoor_air_fraction_mass * den_out + 
     &      (1 - outdoor_air_fraction_mass) * den1
            cp_mix_air = outdoor_air_fraction_mass * cpout + 
     &      (1 - outdoor_air_fraction_mass) * cp1
 
c Find supply temperature of mixed air needed to meet the sensible
c cooling load
            supply_temp = tdb1 - sensible_cooling_load / 
     &                (total_air_mass_flow * (0.5 * (cp_mix_air + cp1)))
            sum1 = den_out * cpout * tout_avg
            sum2 = den1 * cp1 * tdb1

c Check whether sum1 is equal to sum2 to avoid division by 0
            if(sum1.ne.sum2) then
              up_outdoor_air_fraction_volume = 
     &                  (den_mixed_air * cp_mix_air * supply_temp - 
     &                  den1 * cp1 * tdb1) /
     &                  (den_out * cpout * tout_avg - den1 * cp1 * tdb1)
            endif

c Update value of error associated with outdoor air flow volume fraction
            error = abs(outdoor_air_fraction_volume - 
     &                                   up_outdoor_air_fraction_volume)

c Update value of outdoor air volume fraction
            outdoor_air_fraction_volume = up_outdoor_air_fraction_volume

c Update value of iteration counter
            iteration_counter = iteration_counter + 1

c If iteration counter is greater than 10 then issue warning
            if(iteration_counter.eq.10) then
              write(iuout,*) 
     &    'Max number of iterations reached in HEATPC_ECONOMIZER_ENTH'
              endif

          end do

c In this case set value of outdoor air flow based on value of outdoor air 
c volume fraction
          outdoor_air_flow = outdoor_air_fraction_volume * flow_rate

c If the value of outdoor air flow is less than the minimum then set it 
c equal to minimum allowable value
          if(outdoor_air_flow.lt.economizer_min_out_air_flow) then
            outdoor_air_flow = economizer_min_out_air_flow
          endif

c In this case the economizer satisfies the total sensible cooling load and 
c there is no cooling load to be met by the compressor-based cooling system
          sensible_cooling_load = 0.

c If outdoor enthalpy <= return air enthalpy and return air dry-bulb temperature
c >= economizer indoor setpoint temperature and maximum economizer cooling <
c sensible cooling load and economizer control is integrated
        elseif((hout.le.h1).and.
     &      (tdb1.ge.economizer_ind_temp_setpoint).and.
     &      (economizer_cooling_max.lt.sensible_cooling_load).and.
     &      (ieconomizer_control.eq.3)) then
c Economizer cooling is set equal to the maximum cooling from economizer
          economizer_cooling = economizer_cooling_max
c The difference between total sensible cooling and economizer cooling will be
c satisfied by the compressor-based cooling system
          sensible_cooling_load = sensible_cooling_load -
     &                                                economizer_cooling
c Outdoor air flow is set equal to the total system air flow
          outdoor_air_flow = flow_rate

c If outdoor enthalpy <= return air enthalpy and return air dry-bulb temperature
c >= economizer indoor setpoint temperature and maximum economizer cooling <
c sensible cooling load and economizer control is non-integrated
        elseif((hout.le.h1).and.
     &        (tdb1.ge.economizer_ind_temp_setpoint).and.
     &        (economizer_cooling_max.lt.sensible_cooling_load).and.
     &        (ieconomizer_control.eq.4)) then

c Outdoor air flow is set to total system air flow 
          outdoor_air_flow = economizer_min_out_air_flow

c If outdoor enthalpy > return air enthalpy and return air temperature >=
c economizer indoor temperature set point
        elseif((hout.gt.h1).and.
     &        (tdb1.ge.economizer_ind_temp_setpoint)) then

c Set outdoor air flow to minimum economizer airflow
          outdoor_air_flow = economizer_min_out_air_flow

c If return air temperature < economizer indoor temperature set point
        elseif(tdb1.lt.economizer_ind_temp_setpoint) then

c Set outdoor air flow to minimum economizer air flow
          outdoor_air_flow = economizer_min_out_air_flow

        endif

c If sensible cooling load = 0 or economizer outdoor enthalpy limit < 
c outdoor enthalpy or maximum economizer cooling = 0
      elseif((sensible_cooling_load.eq.0.).or.
     &       (economizer_out_enth_limitcon.lt.hout).or.
     &       (economizer_cooling_max.eq.0.)) then

c Set outdoor air flow in this case to minimum economizer outdoor airflow
        outdoor_air_flow = economizer_min_out_air_flow

      endif

      return
      end


c *********************************************************************
c *********************************************************************
c ASHPC_PLR_PLF_LATENTLOAD

c Created by: Kamel Haddad
c Initial Creation Date: July 23rd 2003
c Copyright 2000: NRCan Buildings Group

c Subroutine to determine part-load ratio, part-load factor, and latent
c load of air conditioning unit knowing the space sensible load, the
c position and power of the circulation fan, and the sensible heat ratio
c of the coil

c INPUTS:
c       isys            Number associated with HVAC system
c	sensible_cooling_load
c                       Sensible cooling load imposed on cooling unit
c       ifan_operation  Operation of indoor circulation fan (auto or 
c                       continuous)
c       shr             Sensible heat ratio of coil
c       fan_power_auto  Power of circulation fan in auto mode
c       ss_cooling_cap  Steady-state cooling capacity
	
c OUTPUTS:
c	plr_heat_pump   Part-load ratio of unit
c       plf_heat_pump   Part-load factor of unit
c       rlatent_load    Latent load of unit
c **********************************************************************
      SUBROUTINE ASHPC_PLR_PLF_LATENTLOAD(isys,sensible_cooling_load,
     &           plr_heat_pump,plf_heat_pump,ifan_operation_local,
     &           rlatent_load,shr,fan_power_auto_local,ss_cooling_cap)

      IMPLICIT NONE
#include "building.h"

#include "hvac_parameters.h"
#include "hvac_common.h"
c Variables passed in call
      REAL sensible_cooling_load,plr_heat_pump,plf_heat_pump,
     &     rlatent_load,shr,fan_power_auto_local,ss_cooling_cap

      INTEGER isys,ifan_operation_local

C #include "hvac_parameters.h"
C#include "hvac_common.h"


c Local variables
      REAL error,total_cooling_load,fan_power_mod,PLR,store_plr,
     &     up_plf_heat_pump,ASHP_PLF

      INTEGER number_iterations

      common/outin/iuout,iuin

      INTEGER iuout,iuin

c Initialize latent load
      rlatent_load = 0.

c If the sensible cooling load is 0., then set part-load ratio to 0. as
c the unit will be off for the time step
      if(sensible_cooling_load.lt.0.000001) then
        plr_heat_pump = 0.
c When the sensible cooling load is greater than 0., need to find the
c actual part-load ratio, part-load factor and then deduce in the process
c the latent load of the unit
      else
        plr_heat_pump = 1.
        plf_heat_pump = 1.
        error = 1.
        number_iterations = 1
        do while((error.gt.0.01).and.(number_iterations.le.100))
c When the circulation fan is in continuous mode, the sensible heat
c ratio of the coil is the same as the sensible heat ratio of the unit.
c This is so because the fan power is included as an internal gain to the
c space and is then reflected in the sensible cooling load value
          if(ifan_operation_local.eq.2) then
            rlatent_load = (1. - shr) * sensible_cooling_load / shr
c When the circulation fan is in auto mode, the sensible heat ratio of
c the unit is slightly different from that of the space because of the
c effect of the fan power. In this case fan power is not reflected in
c the sensible cooling load
          elseif(ifan_operation_local.eq.1) then
            rlatent_load = (1. - shr) * (sensible_cooling_load +
     &      fan_power_auto_local * plr_heat_pump / plf_heat_pump)/ shr
          endif

c Set total load on coil which is sum of sensible and laten loads
          total_cooling_load = sensible_cooling_load + rlatent_load

c Assign negative sign to fan power as it reduces cooling capacity. This
c has an effect only when fan is in auto mode. In this case the fan power
c is not included as an internal gain and its effect is accounted for
c through effect on part-load ratio
          fan_power_mod = -fan_power_auto_local / plf_heat_pump

c Set part-load ratio of unit given total cooling load, total steady
c state cooling capacity, the modified fan power, and the operation of
c the circulation fan
          plr_heat_pump = PLR(total_cooling_load,ss_cooling_cap,
     &                  fan_power_mod,ifan_operation_local)

c If part-load ratio > 1, heat pump does not have enough cooling capacity
c to meet the load. Set part-load ratio to 1. in this case
          if(plr_heat_pump.gt.1) then
            store_plr = plr_heat_pump
            plr_heat_pump = 1.
c Keep track of number of hours space is under cooled
            ihrs_under_cool(isys) = ihrs_under_cool(isys) + 1

      write(iuout,*) 'ASHPC_PLR_PLF_LATENTLOAD: plr_heat_pump = '
      write(iuout,*) store_plr, ' > 1'
      write(iuout,*) 'AC does not have enough capacity to meet total'
      write(iuout,*) 'cooling load. Need to increase total capacity of'
      write(iuout,*) 'equipment'
          endif

c Set part load factor of heat pump
          up_plf_heat_pump = ASHP_PLF(isys,plr_heat_pump)

          error = abs(up_plf_heat_pump - plf_heat_pump)
          plf_heat_pump = up_plf_heat_pump
          number_iterations = number_iterations + 1
        end do

      endif

      return
      end

c *********************************************************************
c *********************************************************************
c SET_TOTALCOOLINGCAP_SHR

c Created by: Kamel Haddad
c Initial Creation Date: July 23rd 2003
c Copyright 2000: NRCan Buildings Group

c Subroutine to call appropriate routines to find total cooling capacity
c and sensible heat ratio of equipment depending on the type of cooling
c system (air cooled condenser system or ground-source heat pump system)

c INPUTS:
c       ihvac_type      Type of HVAC cooling system (7 for air cooled 
c                       condenser system,8 and 9 for ground-source heat
c                       pump system) 
c       ss_capacity     Steady state total cooling capacity of system at
c                       ARI rating conditions
c       EWT and GCEP_EWT             
c                       Water temperature on condenser side for ground-source
c                       heat pump systems
c       flow_rate       Air flow rate on the evaporator coil
c       flow_rate_r     Air flow rate on evaporator coil at ARI rating 
c                       conditions
c       tdb2            Inlet coil air dry-bulb temperature
c       w2              Inlet coil air humidity ratio
c       bf              Coil bypass factor
c       patm2           Atmospheric pressure in mbar
c       den1            Density of air
c       ifan_operation  Mode of operation of indoor circulation fan
c       twb2            Coil inlet air wet-bulb temperature
c       wet_bulb_unity_shr
c                       Maximum inlet coil wet-bulb temperature leading
c                       to dry coil conditions for the dry-bulb temperature
c                       tdb2 
c       tout            Outdoor temperature
	
c OUTPUTS:
c	ss_cooling_cap  Equipment cooling capacity corrected for actual
c                       operating conditions
c       GSHP_HP_mode and GCEP_HP_mode    
c                       Mode of operation for ground-source heat pump 
c                       system. Cooling mode in this case
c       w4              Exit humidity ratio from evaporator coil
c       shr             Sensbile heat ratio
c **********************************************************************
      SUBROUTINE SET_TOTALCOOLINGCAP_SHR(ihvac_type,GSHP_HP_mode,
     &           ss_cooling_cap,ss_capacity,EWT,shr,flow_rate,tdb2,w2,
     &           den1,bf,patm2,w4,GCEP_EWT,ifan_operation,twb2,
     &           wet_bulb_unity_shr,flow_rate_r,tout,GCEP_HP_mode)

      IMPLICIT NONE

c Variables passed in call
      REAL ss_cooling_cap,ss_capacity,EWT,shr,flow_rate,
     &     tdb2,w2,den1,bf,patm2,w4,GCEP_EWT,twb2,wet_bulb_unity_shr,
     &     flow_rate_r,tout
      INTEGER  GCEP_HP_mode,  GSHP_HP_mode    
      INTEGER ifan_operation,ihvac_type

      common/outin/iuout,iuin

      INTEGER iuout,iuin

c Local variables
      REAL GSHP_CAP,SENS_HEAT_RATIO,ASHPC_CAP

c If the system being modelled is a GSHP, then the total
c cooling capacity is not affected by the outdoor temperature,
c but instead by the calculated fluid entering water temperature.
      if (ihvac_type.eq.8) then
c set a variable to indicate that the HP is in cooling mode
        GSHP_HP_mode = -1        
        ss_cooling_cap = GSHP_CAP(ss_capacity,EWT,GSHP_HP_mode)
        shr = SENS_HEAT_RATIO(ss_cooling_cap,flow_rate,tdb2,w2,den1,bf,
     &                        patm2,w4) 
c If the system being modelled is a GCEP, then the total
c cooling capacity is not affected by the outdoor temperature,
c but instead by the calculated fluid entering water temperature.
      elseif (ihvac_type.eq.9) then
        GCEP_HP_mode = -1
        ss_cooling_cap = GSHP_CAP(ss_capacity,GCEP_EWT,GCEP_HP_mode)
        shr = SENS_HEAT_RATIO(ss_cooling_cap,flow_rate,tdb2,w2,den1,bf,
     &                        patm2,w4)
c If the system being modeled is an air-conditioner or an air-source
c heat pump in the cooling mode with an air cooled condenser
      elseif(ihvac_type.eq.7) then
c For the case when the entering wet-bulb temperature to the coil is 
c greater than the maximum wet-bulb temperature for dry-coil conditions,
c Set capacity based on actual wet-bulb temperature
        if(twb2.gt.wet_bulb_unity_shr) then
          ss_cooling_cap = ASHPC_CAP(ss_capacity,twb2,tout,flow_rate,
     &                     flow_rate_r,ifan_operation)
c Set the sensible heat ratio of coil
          shr = SENS_HEAT_RATIO(ss_cooling_cap,flow_rate,tdb2,w2,den1,
     &                          bf,patm2,w4)
c If the coil inlet wet bulb temperature is less than the maximum
c wet bulb for dry coil conditions, then all the heat transfer is 
c sensible as the coil is dry
        elseif(twb2.le.wet_bulb_unity_shr) then
          shr = 1.
          w4 = w2
          ss_cooling_cap = ASHPC_CAP(ss_capacity,wet_bulb_unity_shr,
     &                     tout,flow_rate,flow_rate_r,ifan_operation)
        endif
      endif

c In case the computed sensible heat ratio is greater than 1, then 
c set this back to 1
      if(shr.gt.1.0) then
        w4 = w2
        if(ihvac_type.eq.7) then
          ss_cooling_cap = ASHPC_CAP(ss_capacity,wet_bulb_unity_shr,
     &                   tout,flow_rate,flow_rate_r,ifan_operation)
        endif
        write(iuout,*) 
     &       'SET_TOTALCOOLINGCAP_SHR: calculated sensible heat'
        write(iuout,*) 'ratio=', shr, ' for coil is > 1.?'
        write(iuout,*) 
     &       'This can be caused by small unit capacity leading'
        write(iuout,*) 'to space conditions not being maintained at set'
        write(iuout,*) 'point'
        shr = 1.0
      endif

      return
      end

c *********************************************************************
c *********************************************************************
c SET_COP

c Created by: Kamel Haddad
c Initial Creation Date: July 23rd 2003
c Copyright 2000: NRCan Buildings Group

c Subroutine to call appropriate routines to find equipment COP at current
c operating conditions

c INPUTS:
c       ihvac_type      Type of HVAC cooling system (7 for air cooled 
c                       condenser system,8 and 9 for ground-source heat
c                       pump system)
c       total_cooling_load
c                       Total cooling load on unit
c       ss_cop          Steady state COP of system at ARI rating conditions
c       EWT and GCEP_EWT
c                       Water temperature on condenser side for ground-source
c                       heat pump systems
c       flow_rate       Air flow rate on the evaporator coil
c       fow_rate_r      Air flow rate on evaporator coil at ARI rating conditions
c       ifan_operation  Mode of operation of indoor circulation fan
c       twb2            Coil inlet air wet-bulb temperature
c       wet_bulb_unity_shr
c                       Maximum inlet coil wet-bulb temperature leading
c                       to dry coil conditions for the dry-bulb temperature
c                       tdb2 
c       tout            Outdoor dry-bulb temperature
	
c OUTPUTS:
c	cop             Equipment COP corrected for actual operating conditions
c       GSHP_HP_mode and GCEP_HP_mode
c                       Mode of operation for ground-source heat pump system. 
c                       Cooling mode in this case
c **********************************************************************
      SUBROUTINE SET_COP(ihvac_type,GSHP_HP_mode,ss_cop,EWT,
     &       cop,total_cooling_load,GCEP_EWT,GCEP_HP_mode,
     &       GCEP_COOL_COP,twb2,wet_bulb_unity_shr,tout,flow_rate,
     &       flow_rate_r,ifan_operation)

      IMPLICIT NONE

c Variables passed in call
      REAL ss_cop,EWT,cop,total_cooling_load,GCEP_EWT,
     &     GCEP_COOL_COP,twb2,wet_bulb_unity_shr,tout,
     &     flow_rate,flow_rate_r
      INTEGER GSHP_HP_mode, GCEP_HP_mode
      INTEGER ihvac_type,ifan_operation

c Local variables
      REAL ASHPC_COP

c If the system being modelled is a GSHP, then the COP
c is not affected by the outdoor temperature, but by the 
c entering water temperature.
      if (ihvac_type.eq.8) then
        GSHP_HP_mode = -1 
        call GSHP_operating_COP(ss_cop,EWT,GSHP_HP_mode,cop)
        call weighted_GSHP_cop(cop,total_cooling_load)
c If the system being modelled is a GCEP, then the COP
c is not affected by the outdoor temperature, but by the 
c entering water temperature.
      elseif (ihvac_type.eq.9) then
        GCEP_HP_mode = -1
        Call GSHP_operating_COP(ss_cop,GCEP_EWT,GCEP_HP_mode,cop)
        GCEP_COOL_COP = cop
c For an air-conditioner or an air-source heat pump in the cooling mode
      elseif(ihvac_type.eq.7) then
c Check whether the coil is dry or wet and then set COP
c For the case when the entering wet-bulb temperature is greater than
c the highest wet-bulb for dry coil conditions, coil is wet and use the 
c actual outdoor dry-bulb and wet-bulb temperatures to find the COP
        if(twb2.gt.wet_bulb_unity_shr) then
          cop = ASHPC_COP(ss_cop,twb2,tout,flow_rate,flow_rate_r,
     &          ifan_operation)
c If the coil inlet wet bulb temperature is less than the maximum
c wet bulb for dry coil conditions, then coil is dry. Use 
c outdoor dry-bulb temperature and maximum wet-bulb for dry coil 
c conditions to find COP
        elseif(twb2.le.wet_bulb_unity_shr) then
          cop = ASHPC_COP(ss_cop,wet_bulb_unity_shr,tout,flow_rate,
     &          flow_rate_r,ifan_operation)
        endif
      endif

      return
      end

c *********************************************************************
c *********************************************************************
c HUMRATIO_MOISTBAL

c Created by: Kamel Haddad
c Initial Creation Date: July 23rd 2003
c Copyright 2000: NRCan Buildings Group

c Subroutine to find equivalent exit coil humidity ratio corresponding to
c the latent capacity of the unit. This is assuming that inlet conditions
c are the same as those of return air (state1).

c INPUTS:
c       flow_rate       Air flow rate on the evaporator coil
c       h1              Enthalpy of return air
c       ss_cooling_cap  Steady-state total cooling capacity of unit
c       den1            Density of return air 
c       tdb1            Dry-bulb temperature of return air
c       sensible_cooling_cap
c                       Sensible cooling capacity of unit
c       cp1             Specific heat of return air
	
c OUTPUTS:
c       humratio_for_moistbal
c                       Equivalent moisture balance exiting coil
c **********************************************************************
      SUBROUTINE HUMRATIO_MOISTBAL(h1,ss_cooling_cap,den1,flow_rate,
     &           tdb1,sensible_cooling_cap,cp1,humratio_for_moistbal)

      IMPLICIT NONE

c Variables passed in call
      REAL h1,ss_cooling_cap,den1,flow_rate,tdb1,sensible_cooling_cap,
     &     cp1,humratio_for_moistbal

c Local variables
      REAL hcoilexit_moistbal,tempcoilexit_moistbal

c Find exit coil enthalpy given total capacity of unit
      hcoilexit_moistbal = h1 - ss_cooling_cap / 
     &                                        (den1 * flow_rate)
c Find exit dry-bulb temperature from coil given sensible 
c capacity
      tempcoilexit_moistbal = tdb1 - sensible_cooling_cap / 
     &                                  (den1 * flow_rate * cp1)
c Find equivalent humidity ratio
      humratio_for_moistbal = (hcoilexit_moistbal - 
     &                      1006 * tempcoilexit_moistbal) / 
     &                     (2501000 + 1860 *  tempcoilexit_moistbal)
    
      return
      end

c *********************************************************************
c *********************************************************************
c ACSYSTEM_OUTPUT

c Created by: Kamel Haddad
c Initial Creation Date: September 8th 2003
c Copyright 2000: NRCan Buildings Group

c Subroutine to track outputs related to operation of AC system

c INPUTS:
c       isys            HVAC system number
c       sys_elec_consumption
c                       electricity consumption of compressor and
c                       outdoor fan for the time step (Joules)
c       plr_heat_pump   part-load ratio of unit
c       plf_heat_pump   part-load factor of unit
c       ss_cooling_cap  steady-state cooling capacity of equipment
c       shr             sensible heat ratio of unit
c       time_step       simulation tinme step (seconds)
	
c OUTPUTS:
c       total_elec_consumption
c                       total electricity consumption since start of 
c                       simulation (for compressor + indoor fan + 
c                       outdoor fan) in kWh
c       elec_compressor compressor electricity consumption since start
c                       of simulation in kWh
c       elec_outdoor_fan
c                       outdoor fan electricity consumption since start 
c                       of simulation in kWh
c       elec_indoor_fan indoor fan electricity consumption since start of
c                       simulation in kWh
c       average_cop     average unit COP since start of simulation
c       coil_sensible_load
c                       coil sensible load since start of simulation in kWh
c       coil_latent_load
c                       coil latent load since start of simulation in kWh
c       coil_total_load coil total load since start of simulation in kWh
c **********************************************************************
      SUBROUTINE ACSYSTEM_OUTPUT(isys,sys_elec_consumption,
     &     plr_heat_pump,plf_heat_pump,ss_cooling_cap,shr,time_step)

      IMPLICIT NONE

c Variables passed in call
      REAL sys_elec_consumption,plr_heat_pump,plf_heat_pump,
     &     ss_cooling_cap,shr,time_step

      INTEGER isys

#include "building.h"

#include "hvac_parameters.h"

#include "hvac_common.h"

#include "ashp_common.h"

c Local variables
      REAL fan_energy,FAN_ENERGY_CON,outdoor_fan_energy,
     &     compressor_energy
      logical bCloseToZero
      real fSmall
      parameter ( fSmall = 1.0e-02 )

c Find indoor fan electricity consumption for the time step in kWatts
c 0. is for the plr of the backup system. There is no backup system for an
c air-conditioner
c 1. is for the part-load factor for the backup system
      fan_energy = FAN_ENERGY_CON(isys,plr_heat_pump,0.,plf_heat_pump,
     &                            1.) / (time_step * 1000.)

c Update sum for the total electricity consumption of the indoor fan in kWh
      elec_indoor_fan(isys) = elec_indoor_fan(isys) + 
     &                                 (time_step / 3600.) * fan_energy

c Update sum of total system electricity consumption (compressor + indoor fan +
c outdoor fan) in kWh
      total_elec_consumption(isys) = total_elec_consumption(isys) + 
     &                     (time_step / 3600.) * sys_elec_consumption / 
     &                     (time_step * 1000.) + (time_step / 3600.) *
     &                     fan_energy

c Find time step electricity consumption of outdoor system fan in kWatts
      outdoor_fan_energy = outdoor_fan_power(isys) * plr_heat_pump /
     &                     ((plf_heat_pump + 0.00001) * 1000.) 

c Update sum for total electricity consumption of outdoor fan in kWh
      elec_outdoor_fan(isys) = elec_outdoor_fan(isys) + 
     &                         (time_step / 3600.) * outdoor_fan_energy

c Find electricity consumption of compressor for the time step in kWatts
      compressor_energy = sys_elec_consumption / (time_step * 1000.) - 
     &                    outdoor_fan_energy

c Update sum of total electricity consumption of compressor in kWh
      elec_compressor(isys) = elec_compressor(isys) + 
     &                        (time_step / 3600.) * compressor_energy

c Update sum of sensible coil load in kWh
      coil_sensible_load(isys) = coil_sensible_load(isys) + 
     &    (time_step / 3600.) * shr * ss_cooling_cap * plr_heat_pump / 
     &     1000.          

c Updte sum of latent coil load in kWh
      coil_latent_load(isys) = coil_latent_load(isys) + 
     &    (time_step / 3600.) * (1 - shr) * ss_cooling_cap * 
     &     plr_heat_pump / 1000.

c Update sum of total coil load
      coil_total_load(isys) = coil_sensible_load(isys) + 
     &                                         coil_latent_load(isys)

c Average COP which accounts for electricity consumption of compressor and
c outdoor fan

C.....Trap potential div/0 error:
      call eclose ( total_elec_consumption(isys)-elec_indoor_fan(isys),
     &              0.0, fSmall, bCloseToZero )

      if ( bCloseToZero ) then
        average_cop(isys) = 0.0
      else
        average_cop(isys) = coil_total_load(isys) / 
     &      (total_elec_consumption(isys) - elec_indoor_fan(isys))
      endif 

c Following only activate when air-conditioner model output is desired
c      write(iuout,*) sys_elec_consumption,elec_indoor_fan(isys),
c     &           elec_outdoor_fan(isys),elec_compressor(isys),
c     &           total_elec_consumption(isys),coil_sensible_load(isys),
c     & coil_latent_load(isys),coil_total_load(isys),average_cop(isys)

      return

      end
   

      
