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
c  ASHP_HEATING    determine the energy consumption of the air-source heat pump
c                  when used in the heating mode
c  ASHP_CAP        determine the capacity of the air-source heat pump based on
c                  the outdoor temperature
c  ASHP_CAP_FLOW_CORR
c                  correction of heat pump heating capacity for the effect of 
c                  the flow rate
c  ASHP_COP   determine the steady-state COP of the air-source heat pump
c                  as a function of the outdoor temperature
c  ASHP_COP_FLOW_CORR
c                  correction of heat pump COP in the heating mode for the effect
c                  volume flow rate
c  ASHP_PLF        determine the part-load factor of the air-source heat pump
c                  when not operating simultaneously with a fossil fuel backup
c                  heat source
c  BACKUP_LOAD     determine the remaining space load to be supplied by a 
c                  baseboard/hydronic/plenum heater backup system when the 
c                  primary system does not have enough capacity to meet the
c                  load
c       ASHP_FURNACE_PLF
c                  determine the part-load factor of the air-source heat pump and
c                  the fossil fuel backup heat source when operating together to
c                  meet the load
c       SYS_SOL    solve a system of linear equation for the unknowns using the 
c                  back substitution method
c *********************************************************************
c *********************************************************************
c ASHP_HEATING

c Created by: Kamel Haddad
c Initial Creation Date: July 12th 2000
c Copyright 2000: NRCan Buildings Group
c
c Subroutine to determine the energy consumption of an air-source heat
c pump for the current time step. This subroutine is called from HVACSIM
c after the heating or the cooling load to be supplied by the HVAC 
c equipment is determined.
 
c references:
c   1) "Further improvements to the HOT2000 subroutine:-Part B:
c       Furnace and Heat Pump Models", NRCAN Report No. ET-91-045

c   2) "Part load curves for use in DOE-2"
c       Prepared for the Lawrence Berkley Laboratory by
c       Hugh Henderson, CDH Energy Corp, Cazenovia, NY

c   3)  DOE-2 Engineering Manual, Version 2.1A, Universitry of
c       California Berkley, CA, November 1981

c   4)  HOT2000 Technical Manual, Version 6, Canadian Home Builders
c       Association, August 1991

c   5)  Air-Source Heat Pump Model for Implementation in H3k
c      H3k model documentation, Kamel Haddad, June 10 2000

c INPUTS:
c  HVAC_INPUT_DATA      common for HVAC input data
c  HEAT_PUMP_INPUT_DATA common for heat pump input data
c  pers                 common containing the number of time steps per hour
c  isys                 HVAC system number
c  heating_load         total combined heating load of the zones served
c                       by the HVAC system isys 
c  CLIMI                common for weather data

c OUTPUTS:
c      heat_pump_energy total energy consumed by the air-source heat pump 
c                       system in J. This includes the energy required when the 
c                       heat pump is on, and the energy required to operate 
c                       the circulating fan of the forced-air system.
c      plrhp            part-load ratio of the heat pump system
c      plrb             part-load ratio of backup system
c      plfhp            part-load factor of heat pump system
c **********************************************************************
      SUBROUTINE ASHP_HEATING(isys,heating_load,heat_pump_energy,
     &                        plrhp,plrb,plfhp)

      IMPLICIT NONE

c Variables passed in call
      REAL heating_load,heat_pump_energy,plrhp,plrb,plfhp

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

c Common for the weather data
      COMMON/CLIMI/QFP,QFF,TP,TF,QDP,QDF,VP,VF,DP,DF,HP,HF

      REAL QFP,QFF,TP,TF,QDP,QDF,VP,VF,DP,DF,HP,HF

c Local variables

c Mode of operation of the GSHP, GSHP_HP_mode = 1 for heating 
c and -1 for cooling
C      integer GSHP_HP_mode,GCEP_HP_mode
      integer GSHP_HP_mode

      REAL ashp_temp_min,PLR,ss_heat_pump_cap,GSHP_CAP,ASHP_CAP,
     &     backup_heating_load,BACKUP_LOAD,plfhp2,ASHP_PLF_OFFCYC_CORR,
     &     ASHP_COP,cop,time_step,TIME_INTERVAL,ASHP_ENERGY,ASHP_PLF,
     &     total_sys_heat_cap


c GSHP 
c If the system being modelled is a GSHP, then the capacity
c is not affected by the outdoor temperature, but instead by the 
c entering water temperature.

      !print*, " ----> ASHP HEATING: "

      if (ihvac_type(isys) .eq. 8) then
        GSHP_HP_mode = 1         ! set the HP mode of operation to heating.
        ss_heat_pump_cap = GSHP_CAP(ss_capacity(isys),EWT,GSHP_HP_mode)

c If the system being modelled is a GCEP, then the capacity
c is not affected by the outdoor temperature, but instead by the 
c entering water temperature.

      elseif (ihvac_type(isys) .eq. 9) then
        GCEP_HP_mode = 1
        ss_heat_pump_cap = GSHP_CAP(ss_capacity(isys),GCEP_EWT,
     &                              GCEP_HP_mode)
      else

c The simulation procedure of the energy consumption of the air-source
c heat pump system depends on the initial estimate of the part-load ratio 
c plrhp and the temperature control strategy employed. Given this information
c it is also possible to estimate the energy consumption of the backup 
c heating system.

c Check whether the outdoor temperature is below the minimum operating
c temperature of the heat pump. In this case the heat pump is off and the 
c full load is satisfied by the backup heat source if it has enough 
c capacity. The minimum temperature is based on the default in the DOE-2
c computer program
        ashp_temp_min = cutoff_temp(isys)

        
        if(TF.lt.ashp_temp_min) then
c Estimate the plr of the backup heat source
          ss_heat_pump_cap = 0.
          plrb = PLR(heating_load,ss_capacity(ibackup_sys_num(isys)),
     &           fan_power(isys),ifan_operation(isys))
     
        else 
c Steady-state heat pump capacity accounting for the effect of the outdoor
c temperature
          ss_heat_pump_cap = ASHP_CAP(ss_capacity(isys),TF,
     &                       flow_rate(isys),
     &                       flow_rate_r(isys),ifan_operation(isys),
     &                       cutoff_temp(isys))


        endif 

     
      endif

      !print*, " ----> HP SS CAP:  ", ss_heat_pump_cap
      
C Save actual capacity of heat-pump system
      fH3KActualCapacity(iSYS) = ss_heat_pump_cap

c Initial estimate of the part-load ratio of the heat pump
      plrhp = PLR(heating_load,ss_heat_pump_cap,fan_power(isys),
     &            ifan_operation(isys))
      !print*, " ----> HP PLR (1):  ", plrhp 
c Depending on the first estimate of the part-load ratio and the temperature
c control option, different procedures are used to estimate the actual 
c part-load ratio and factor for the heat pump and the backup heating system.
      
c If the first estimate of plrhp < 1 and the temperature control is balanced
c point control, the heat pump satisfies the full load

C------------------------------------------------------------------------------
      if((plrhp.lt.0.99).and.(itemp_control(isys).eq.1)) then
c Part-load factor of the heat pump
        plfhp = ASHP_PLF(isys,plrhp)
c Part-load ratio of the backup heat source
        plrb = 0.
c Set total heating capacity available. This will be used to set 
c appropriate zone controller capacity for time step
        total_sys_heat_cap = ss_heat_pump_cap 
     &             + ss_capacity(ibackup_sys_num(isys))
c Call subroutine to set zone controller heating capacity based on
c total system heating capacity
        call SET_CONT_FUNC_HEAT_CAP(isys,total_sys_heat_cap)

C------------------------------------------------------------------------------		
c If plrhp > 1 and the temperature control option is balanced point
c control, the heat pump is off and the backup heat satisfies the full
c load if it has enough capacity

      elseif((plrhp.ge.0.99).and.(itemp_control(isys).eq.1)) then
        plrhp = 0.
c Set the part-load ratio of the backup heat source
        plrb = PLR(heating_load,ss_capacity(ibackup_sys_num(isys)),
     &         fan_power(isys),
     &         ifan_operation(ibackup_sys_num(isys)))
c Set total heating capacity available. This will be used to set 
c appropriate zone controller capacity for time step
        total_sys_heat_cap = ss_capacity(ibackup_sys_num(isys))
c Call subroutine to set zone controller heating capacity based on
c total system heating capacity
        call SET_CONT_FUNC_HEAT_CAP(isys,total_sys_heat_cap)
        return

C------------------------------------------------------------------------------
c For restricted control, outdoor temperature > cutoff point, and plrhp < 1,
c the heat pump satisfies the full load
      elseif((plrhp.lt.0.99).and.(itemp_control(isys).eq.2 .or. 
     &                            itemp_control(isys).eq.4      )
     &                      .and.(TF.ge.cutoff_temp(isys))) then
	 
c Part-load factor of the heat pump
        plfhp = ASHP_PLF(isys,plrhp)
c Part-load ratio of the backup heat source
        plrb = 0.     
c Set total heating capacity available. This will be used to set 
c appropriate zone controller capacity for time step
        total_sys_heat_cap = ss_heat_pump_cap 
     &          + ss_capacity(ibackup_sys_num(isys))
c Call subroutine to set zone controller heating capacity based on
c total system heating capacity
        call SET_CONT_FUNC_HEAT_CAP(isys,total_sys_heat_cap)

C------------------------------------------------------------------------------
c For restricted control, outdoor temperature < cutoff point, and plrhp < 1,
c the full load is satisfied by the backup heat source in case it has enough
c capacity
      elseif((plrhp.lt.0.99).and.(itemp_control(isys).eq.2 .or. 
     &                            itemp_control(isys).eq.4      )	 
     &                      .and.(TF.lt.cutoff_temp(isys))) then   
        plrhp = 0.
c Set the part-load ratio of the backup heat source
        plrb = PLR(heating_load,ss_capacity(ibackup_sys_num(isys)),
     &         fan_power(isys),
     &         ifan_operation(ibackup_sys_num(isys)))
c Set total heating capacity available. This will be used to set 
c appropriate zone controller capacity for time step
        total_sys_heat_cap = ss_capacity(ibackup_sys_num(isys))
c Call subroutine to set zone controller heating capacity based on
c total system heating capacity
        call SET_CONT_FUNC_HEAT_CAP(isys,total_sys_heat_cap)
        return

C------------------------------------------------------------------------------		
C If restricted cut-off, outdoor temperature > cutoff temperature, and
c plrhp > 1, the heat pump can not satisfy the full load and the backup
c heat source is employed
      elseif((plrhp.ge.0.99).and.( itemp_control(isys).eq.2 .or. 
     &                             itemp_control(isys).eq.4)
     &                      .and.( TF.ge.cutoff_temp(isys))) then

	 
C if backup is electric baseboard, or if the HP is displacement application 
C (ie minisplit,) HP operates at full capacity and back-up meets remainder 
C of load. 	  
        if ( ibackup_heat(isys).eq. 3 .or. 
     &       itemp_control(isys) .eq. 4 ) then 

          plrhp = 1.
   
		  plfhp = ASHP_PLF(isys,plrhp)			

C          backup_heating_load = BACKUP_LOAD(heating_load,
C     &    ss_heat_pump_cap,fan_power(isys),ifan_operation(isys))                   
          plrb = PLR(heating_load-ss_heat_pump_cap,
     &               ss_capacity(ibackup_sys_num(isys)),
     &               fan_power(isys),
     &                ifan_operation(ibackup_sys_num(isys)))
	 

c Set total heating capacity available. This will be used to set 
c appropriate zone controller capacity for time step
          total_sys_heat_cap = ss_heat_pump_cap + 
     &                         ss_capacity(ibackup_sys_num(isys))
c Call subroutine to set zone controller heating capacity based on
c total system heating capacity
          call SET_CONT_FUNC_HEAT_CAP(ibackup_sys_num(isys),
     &           total_sys_heat_cap)	 
        else 
	 
c If the backup heat source is a furnace/boiler, the heat pump and the furnace/
c boiler operate alternatively to satisfy the load
c Check whether the furnace system has enough capacity to meet the load
c by itself. To do this, the furnace part-load ratio is needed
          plrb = PLR(heating_load,ss_capacity(ibackup_sys_num(isys)),
     &           fan_power(isys),ifan_operation(isys))

c If plrb > 1, the furnace does not have enough power to meet the load. 
c In this case the furnace runs for the whole hour and the heat pump is off.
          if(plrb.ge.0.99) then
            plrhp = 0.
c Set total heating capacity available. This will be used to set 
c appropriate zone controller capacity for time step
            total_sys_heat_cap = ss_capacity(ibackup_sys_num(isys))
c Call subroutine to set zone controller heating capacity based on
c total system heating capacity
            call SET_CONT_FUNC_HEAT_CAP(isys,total_sys_heat_cap)
            return
c If plrb < 1, the furnace and the heat pump operating together will be able
c to meet the load. Call ASHP_FURNACE_PLF to find the fraction of time the
c furnace and the heat pump are on
          elseif(plrb.lt.0.99) then       
            call ASHP_FURNACE_PLF(isys,heating_load,ss_heat_pump_cap,
     &                            plrhp,plfhp,plrb)
c Set total heating capacity available. This will be used to set 
c appropriate zone controller capacity for time step
            total_sys_heat_cap = ss_capacity(ibackup_sys_num(isys))
c Call subroutine to set zone controller heating capacity based on
c total system heating capacity
            call SET_CONT_FUNC_HEAT_CAP(isys,total_sys_heat_cap)
            if(plrhp.lt.0.) then
              plrhp = 0.
              plrb = 1.0
              return
            endif
          endif

        endif

c If restricted control, outdoor temperature < cutoff temperature, and
c plrhp > 1
      elseif((plrhp.ge.0.99).and.(itemp_control(isys).eq.2).and.
     &(TF.lt.cutoff_temp(isys))) then
        plrhp = 0.
c Set the part-load ratio of the backup heat source
        plrb = PLR(heating_load,ss_capacity(ibackup_sys_num(isys)),
     &         fan_power(isys),
     &         ifan_operation(ibackup_sys_num(isys)))
c Set total heating capacity available. This will be used to set 
c appropriate zone controller capacity for time step
        total_sys_heat_cap = ss_capacity(ibackup_sys_num(isys))
c Call subroutine to set zone controller heating capacity based on
c total system heating capacity
        call SET_CONT_FUNC_HEAT_CAP(isys,total_sys_heat_cap)
        return

c Unrestricted control and plrhp < 1, the full load is satisfied by the 
c heat pump
      elseif((plrhp.lt.0.99).and.(itemp_control(isys).eq.3)) then
c Part-load factor of the heat pump
        plfhp = ASHP_PLF(isys,plrhp)
c Part-load ratio of the backup heat source
        plrb = 0. 
c Set total heating capacity available. This will be used to set 
c appropriate zone controller capacity for time step
        total_sys_heat_cap = ss_heat_pump_cap
c Call subroutine to set zone controller heating capacity based on
c total system heating capacity
        call SET_CONT_FUNC_HEAT_CAP(isys,total_sys_heat_cap)
 
c Unrestricted control and plrhp > 1, the heat can not satisfy the full load
c and the backup heat source is needed
      elseif((plrhp.ge.0.99).and.(itemp_control(isys).eq.3)) then
         
c If the backup heat source is a furnace, the heat pump and the furnace
c operate alternatively to satisfy the load
        if((ibackup_heat(isys).eq.1).or.
     &     (ibackup_heat(isys).eq.2)) then
c Check whether the furnace system has enough capacity to meet the load
c by itself. To do this, the furnace part-load ratio is needed
          plrb = PLR(heating_load,ss_capacity(ibackup_sys_num(isys)),
     &           fan_power(isys),ifan_operation(isys))

c If plrb > 1, the furnace does not have enough power to meet the load. 
c In this case the furnace runs for the whole hour and the heat pump is off.
          if(plrb.ge.0.99) then 
            plrhp = 0.
c Set total heating capacity available. This will be used to set 
c appropriate zone controller capacity for time step
            total_sys_heat_cap = ss_capacity(ibackup_sys_num(isys))
c Call subroutine to set zone controller heating capacity based on
c total system heating capacity
            call SET_CONT_FUNC_HEAT_CAP(isys,total_sys_heat_cap)
            return
c If plrb < 1, the furnace and the heat pump operating together will be able
c to meet the load. Call ASHP_FURNACE_PLF to find the fraction of time the
c furnace and the heat pump are on
          elseif(plrb.lt.0.99) then       
            call ASHP_FURNACE_PLF(isys,heating_load,ss_heat_pump_cap,
     &                            plrhp,plfhp,plrb)
c Set total heating capacity available. This will be used to set 
c appropriate zone controller capacity for time step
            total_sys_heat_cap = ss_capacity(ibackup_sys_num(isys))
c Call subroutine to set zone controller heating capacity based on
c total system heating capacity
            call SET_CONT_FUNC_HEAT_CAP(isys,total_sys_heat_cap)          
            if(plrhp.lt.0.) then
              plrhp = 0.
              plrb = 1.0
              return
            endif
          endif
c If the backup heat is Hydronic/Baseboard/Plenum Heaters
        elseif(ibackup_heat(isys).eq.3) then
          plrhp = 1.
          plfhp = 1.
c Space heat load remaining to be satisfied by backup heat source. Need
c to account for the contribution of the circulation fan
          backup_heating_load = BACKUP_LOAD(heating_load,
     &    ss_heat_pump_cap,fan_power(isys),ifan_operation(isys))                   
          plrb = backup_heating_load / 
     &                             ss_capacity(ibackup_sys_num(isys))
c Set total heating capacity available. This will be used to set 
c appropriate zone controller capacity for time step
          total_sys_heat_cap = ss_heat_pump_cap + 
     &                         ss_capacity(ibackup_sys_num(isys))
c Call subroutine to set zone controller heating capacity based on
c total system heating capacity
          call SET_CONT_FUNC_HEAT_CAP(isys,total_sys_heat_cap)
        endif

      endif

c The part-load factor of the heat pump plfhp does not account for the energy
c consumption when the unit is off cycle. The off-cyle power use is due to 
c crankcase heaters, controls, fans, etc ... The Henderson report accounts
c for this through a modified part-load factor plfhp2.

      plfhp2 = ASHP_PLF_OFFCYC_CORR(isys_type(isys),plrhp,plfhp)


c If the system being modelled is a GSHP, then the COP
c is not affected by the outdoor temperature, but instead by the 
c entering water temperature.
       if (ihvac_type(isys) .eq. 8) then
         GSHP_HP_mode = 1
         call GSHP_operating_COP(ss_cop(isys),EWT,GSHP_HP_mode, cop)
         call weighted_GSHP_cop(cop,heating_load)

c If the system being modelled is a GCEP, then the COP
c is not affected by the outdoor temperature, but instead by the 
c entering water temperature.
       elseif (ihvac_type(isys) .eq. 9) then
         GCEP_HP_mode = 1
         Call GSHP_operating_COP(ss_cop(isys),GCEP_EWT,GCEP_HP_mode,cop)
         GCEP_HEAT_COP = cop
       else
c Set COP of heat pump
         cop = ASHP_COP(isys,TF)
       endif
c GCEP (end) 

c Simulation time step in seconds based on the number of time steps per
c hour
       time_step = TIME_INTERVAL(ntstep)

c Energy consumption in J of the air-source heat pump during the current time
c step
       heat_pump_energy = ASHP_ENERGY(ss_heat_pump_cap,cop,plrhp,
     &                   plfhp2,time_step)
       
       
      !print*, " ----> HP PLR  (e):  ", plrhp        
      !print*, " ----> HP PL-b (e):  ", plrb
       
     
       return
       end

c *********************************************************************
c *********************************************************************
c ASHP_CAP

c Created by: Kamel Haddad
c Initial Creation Date: July 18th 2000
c Copyright 2000: NRCan Buildings Group
c
c Function to determine the steady-state capacity of an air-source heat
c pump as a function of the outdoor temperature

c References:HOT2000 Technical Manual, Version 6, Canadian Home Builders
c       Association, August 1991

c       Air-Source Heat Pump Model for Implementation in H3k
c       H3k model documentation, Kamel Haddad, June 10 2000

c INPUTS:
c   tout            outdoor temperature in Deg C
c   ss_cap          capacity of the heat pump at steady state in W
c   flow_rate       operating flow rate
c   flow_rate_r     flow rate at rating conditions
c   ifan_operation  mode of operation of indoor fan

c OUTPUTS:
c       ASHP_CAP    steady-state capacity of the heat pump at outdoor
c                   temperature tout
c **********************************************************************
      REAL FUNCTION ASHP_CAP(ss_cap,tout,flow_rate,flow_rate_r,
     &                       ifan_operation,cutoff_temp)

      IMPLICIT NONE

c Variables passed in call
      REAL ss_cap,tout,flow_rate,flow_rate_r, cutoff_temp
      REAL cap_adjust 
      INTEGER ifan_operation

c Local variables
      REAL a_cap,b_cap,c_cap,d_cap,e_cap,heating_cap

c First set the coefficients for the correlation equation for the steady-state
c capacity of the heat pump
      a_cap = 0.766836
      b_cap = 0.027487
      c_cap = 0.00028936
      d_cap = -1.4658e-5
      e_cap = -5.65296e-7

      
      if ( cutoff_temp > -20. ) then 
c Set the steady-state capacity for the current time step
c Equation 1 in Air-source heat pump Ref by Haddad
         cap_adjust = (a_cap + b_cap * tout + 
     &    c_cap * tout**2 + d_cap * tout**3 + e_cap * tout**4)
      else 
c Hard-coded capacity curve from Mitsibushi Zuba-Central 
        if ( tout > -15. ) then 
          cap_adjust = 1.0
        elseif ( tout < -25. ) then 
          cap_adjust = 0.0
        else 
          cap_adjust = 0.75 + 0.25 * ( tout - (-25.0) ) / 10. 
        endif 
      endif 
         
      heating_cap = ss_cap * cap_adjust
      
c Correct the capacity for the effect of air flow in case there is a
c circulation fan
c      if(ifan_operation.ne.0) then
c        call ASHP_CAP_FLOW_CORR(heating_cap,flow_rate,flow_rate_r)
c      endif

      ASHP_CAP = heating_cap
     
      return
      end

c *********************************************************************
c *********************************************************************
c ASHP_CAP_FLOW_CORR

c Created by: Kamel Haddad
c Initial Creation Date: March 14th 2001
c Copyright 2000: NRCan Buildings Group
c
c Subroutine to correct heating capacity of heat pump for the effect of
c flow rate

c INPUTS:
c  heating_cap      heating capacity
c  flow_rate        operating flow rate
c  flow_rate_r      flow rate at rating conditions

c OUTPUTS:
c  heating_cap      capacity corrected for the effect of flow rate
c **********************************************************************
      SUBROUTINE ASHP_CAP_FLOW_CORR(heating_cap,flow_rate,flow_rate_r)

      IMPLICIT NONE

c Variables passed in call
      REAL heating_cap,flow_rate,flow_rate_r

c Local variables
      REAL  flow_ratio,a_flow,b_flow

c Flow ratio
      flow_ratio = flow_rate / flow_rate_r

c Correction correlation based on default curve in DOE-2.1E for system
c RESYS SDL-C98
        a_flow = 0.84
        b_flow = 0.16

c New heating capacity
      heating_cap = heating_cap * (a_flow + b_flow * flow_ratio)

      return
      end

c *********************************************************************
c *********************************************************************
c ASHP_COP

c Created by: Kamel Haddad
c Initial Creation Date: July 18th 2000
c Copyright 2000: NRCan Buildings Group
c
c Function to determine the steady-state coefficient of performance
c of an air-source heat pump as a function of the outdoor temperature
c and the COP under rating conditions

c References: HOT2000 Technical Manual, Version 6, Canadian Home Builders
c       Association, August 1991

c       Air-Source Heat Pump Model for Implementation in H3k
c       H3k model documentation, Kamel Haddad, June 10 2000

c INPUTS:
c  tout            outdoor temperature in Deg C
c  ss_cop          cop of the heat pump at steady state
c  flow_rate       operating flow rate
c  flow_rate_r     flow rate at rating conditions
c  ifan_operation  mode of operation of circulation fan

c OUTPUTS:
c       ASHP_COP   steady-state cop of the heat pump at outdoor
c                  temperature tout
c **********************************************************************
      REAL FUNCTION ASHP_COP(iSys,tout)

      IMPLICIT NONE
      include "building.h"
      include "hvac_parameters.h"
      include "hvac_common.h"
      include "ashp_common.h"
c Variables passed in call


      REAL tout, cop, tReturn

      real temp_corr
      
      INTEGER iSys 


c Set the coefficients for the steady-state C.O.P. correlation 
c of the heat pump
      real, parameter :: a_cop = 0.846394
      real, parameter :: b_cop = 0.018819
      real, parameter :: c_cop = -6.37288e-5
      real, parameter :: d_cop = -9.165897e-6
      real, parameter :: e_cop = -1.99907e-8

c Set the steady-state C.O.P. for the current time step
c Equation 2 in Air-source heat pump by Haddad

      
      if ( iHeatPumpType(iSys) == iASHP_typical  .or.
     &     iHeatPumpType(iSys) == iASHP_good     .or.
     &     iHeatPumpType(iSys) == iASHP_poor    ) then 
           
        cop = ss_cop(isys) 
     &               * (a_cop + b_cop * tout + c_cop * tout**2 +
     &                  d_cop * tout**3 + e_cop * tout**4)            
           
           
      elseif ( iHeatPumpType(iSys) == iASHP_GasHP_a ) then 

C This is a hard-coded Performance curve for a gas-fired heat pump, 
C based on correlations developed by ORNL and Stone Mountain Technolgies:
C
C   Goeghegan et al, 2016; "Regional climate zone modelling of a 
C   commerical absorption heat pump water heater", 
C   16th International Refrigeration and Air Conditioning 
C   Conference at Purdue, July 11-14, 2016
      
        tReturn = 34.4  
      
        cop = 1.6322  - 0.005      * tReturn 
     &                - 2.054E-05  * tReturn ** 2.0   
     &                + 0.0057     * tout 
     &                - 2.7973E-07 * tout ** 2.0 
     &                + 1.1585E-06 * tout * tReturn
           
        !print*, "TOUT: ", tout, " COP: ", cop
           
      elseif ( iHeatPumpType(iSys) == iASHP_CCASHP_CEF ) then 

C This is a hard-coded Performance curve for a Cold-climate ASHP 
C inferred from manufactuer's literature. It assumes the COP 
C varies linerally between 3.19 (at 15oC) and 1.4 (at -25 oC). 
C This configuration was used in the original NRCan ANZEH optimiation
C work. 
      
        cop = 1.4 + (tout - (-25.) ) / ( 15. - ( -25. ) ) 
     &            * ( 3.19 - 1.4 ) 
      
        if ( COP < 1.4 ) COP = 1.4
        if ( COP > 3.19) COP = 3.19
      
      elseif ( iHeatPumpType(iSys) == iASHP_CCASHP_CCHT ) then 
      
        temp_corr = tout
        if ( temp_corr < - 21. ) temp_corr = -21.
        if ( temp_corr >   5.  ) temp_corr =   5.

C This is a hard-coded performance curve for a cold-cliamte ASHP 
C developed from testing at CCHT. It 
C has three parts: 
C
C   1. When -21oC < tout < 5oC, performance is provided by an 
C      4th-order polynomial based on a regression to measured data
C      collected at CCHT. The data 
C   
C   2. Above 5oC, performance is held constant at its 5oC value 
C      ( approx 3.4 ).
C 
C   3. Below -21 oC, performance is held constant at its -21oC 
C      value. 
C
C The data reflects 'full-load' performance; it must be combined 
C with the part-load factor (See ASHP_PLF and ASHP_PLF_OFFCYC_CORR).
C When combined with these other factors, agreement is excellent 
C with the CCHT test data. 

        cop =    1.42418e-005 * temp_corr ** 4 
     &         + 0.000687606  * temp_corr ** 3 
     &         + 0.0116993    * temp_corr ** 2 
     &         + 0.145709     * temp_corr  
     &         + 3.06098

      elseif ( iHeatPumpType(iSys) == iASHP_CCASHP_MANUF ) then  
         
C This is a hard-coded performance curve for a cold-climate ASHP 
C developed from detailed data supplied by the manufacturer. It 
C has three parts: 
C 
C
C   1. When -35oC < tout < 5oC, performance is provided by an 
C      8th-order polynomial based on a regression to manufacturer 
C      test data. 
C   
C   2. Above 5oC, performance is held constant at its 5oC value 
C      ( approx 3.4 ).
C 
C   3. Below -35 oC, performance is held constant at its -35 oC 
C      value. 
C
C Parts 2 and 3 are somewhat moot because the HP is usually disabled as 
C tout -> -30oC, and most homes need little heat when the temperature is 
C above 5oC. 
C   
C       
           
        temp_corr = tout
        if ( temp_corr < - 35. ) temp_corr = -35.
        if ( temp_corr >   15. ) temp_corr =   5.           
                  
        cop =    4.02222e-011 * temp_corr ** 8.0 
     &        +  4.37853e-009 * temp_corr ** 7.0
     &        +  1.50595e-007 * temp_corr ** 6.0 
     &        +  6.84488e-007 * temp_corr ** 5.0 
     &        -  5.71379e-005 * temp_corr ** 4.0 
     &        -  0.000781647  * temp_corr ** 3.0 
     &        +  0.00480432   * temp_corr ** 2.0 
     &        +  0.142471     * temp_corr 
     &        + 2.60172		  	           
 
            
           
       else 
       
         ! Shouldn't happen as iHeatPumpType is set programmically. 
       
       endif 
          

c Correct heat pump for the effect of flow rate in case there is a 
c circulation fan
      if(ifan_operation(iSys).ne.0) then
        call ASHP_COP_FLOW_CORR(cop,flow_rate(iSys),flow_rate_r(iSys))
      endif

      ASHP_COP = cop

      return
      end function ASHP_COP

c *********************************************************************
c *********************************************************************
c ASHP_COP_FLOW_CORR

c Created by: Kamel Haddad
c Initial Creation Date: March 14th 2001
c Copyright 2000: NRCan Buildings Group

c Function to correct heat pump heating mode COP for the effect of flow
c rate

c INPUTS:
c  cop          heat pump heating mode COP
c  flow_rate    operating flow rate
c  flow_rate_r  flow rate at rating conditions

c OUTPUTS:
c  cop          heating mode COP corrected for effect of flow rate
c **********************************************************************
      SUBROUTINE ASHP_COP_FLOW_CORR(cop,flow_rate,flow_rate_r)

      IMPLICIT NONE

c Variables passed in call
      REAL cop,flow_rate,flow_rate_r

c Local variables
      REAL flow_ratio,a_flow,b_flow,c_flow

c Define flow rate ratio
      flow_ratio = flow_rate / flow_rate_r

c Correlation to correct COP is based on default correlation in DOE-2.1E for
c system RESYS SDL-C105
      a_flow = 1.3824
      b_flow = -0.4336
      c_flow = 0.0512

c New cooling COP
      cop = cop / (a_flow + b_flow * flow_ratio + 
     &             c_flow * flow_ratio**2)

      return
      end

c *********************************************************************
c *********************************************************************
c ASHP_PLF

c Created by: Kamel Haddad
c Initial Creation Date: July 18th 2000
c Copyright 2000: NRCan Buildings Group
c
c Function to determine the part-load factor of an air-source heat pump

c References:
c       "Part load curves for use in DOE-2"
c       Prepared for the Lawrence Berkley Laboratory by
c       Hugh Henderson, CDH Energy Corp, Cazenovia, NY

c       Air-Source Heat Pump Model for Implementation in H3k
c       H3k model documentation, Kamel Haddad, June 10 2000

c INPUTS:
c   ashp_corr       common containing the correlation coefficients
c                   for the part-load performance of the heat pump
c   plrhp           part-load ratio of the heat pump
c   ASHP_INPUT_DATA common for ASHP input data

c OUTPUTS:
c   ASHP_PLF        estimate of the part-load factor
c **********************************************************************
       REAL FUNCTION ASHP_PLF(isys,plrhp)

       IMPLICIT NONE

c Variables passed in call
       REAL plrhp

       INTEGER isys

#include "hvac_parameters.h"

#include "ashp_common.h"

c Local variables
      REAL heat_eir_fplr

c Energy input ratio of the heat pump
c Equation 6 in Air-source heat pump Ref by Haddad
      heat_eir_fplr = ahp(isys) + bhp(isys) * plrhp + 
     &chp(isys) * plrhp**2 + dhp(isys) * plrhp**3

c Part-load factor of the heat pump
c Equation 7 in Air-source heat pump Ref by Haddad
      ASHP_PLF = plrhp / (heat_eir_fplr + 0.00001)
     
      return
      end

c *********************************************************************
c BACKUP_LOAD

c Created by: Kamel Haddad
c Initial Creation Date: December 16th 2000
c Copyright 2000: NRCan Buildings Group
c
c Function to determine the space heating load that still needs to be 
c satisfied by a baseboard/hydronic/plenum heater backup heat source.
c
c INPUTS:
c  heating_load total space heating load in W
c  primary_sys_cap  capacity of primary heat source in W
c  fan_power        circulation fan power in W
c  ifan_operation   mode of operation of circulation fan

c OUTPUTS:
c BACKUP_LOAD  remaining load to be satisfied by backup
c              system in W
c **********************************************************************
      REAL FUNCTION BACKUP_LOAD(heating_load,primary_sys_cap,fan_power,
     &ifan_operation)

      IMPLICIT NONE

c Variables passed in call
      REAL heating_load,primary_sys_cap,fan_power

      INTEGER ifan_operation

c When there is no circulation fan, the heat load is reduced by the 
c capacity of the primary system
      if(ifan_operation.eq.0) then
        BACKUP_LOAD = heating_load - primary_sys_cap
c When the circulation fan is in auto mode, the heat load is reduced by
c the primary system capacity and the fan power. Fan power is considered
c to be part of the system capacity
      elseif(ifan_operation.eq.1) then
        BACKUP_LOAD = heating_load - primary_sys_cap - fan_power
c When the circulation fan is in continuous mode, the load is reduced by 
c the primary system capacity. The effect of the fan power is included
c in the heating load as this is included as internal heat gain.
      elseif(ifan_operation.eq.2) then
        BACKUP_LOAD = heating_load - primary_sys_cap
      endif

      return
      end

c *********************************************************************
c *********************************************************************
c ASHP_FURNACE_PLF

c Created by: Kamel Haddad
c Initial Creation Date: July 25th 2000
c Copyright 2000: NRCan Buildings Group
c
c Subroutine to determine the part-load ratio and factor of the air-source
c heat pump and the furnace when operating together to meet the heating load.
c The solution procedure employs the Newton Raphson Method for a system of
c non linear equations
c
c References: HOT2000 Technical Manual, Version 6, Canadian Home Builders
c       Association, August 1991

c INPUTS:
c  isys  HVAC system number for heat pump system
c  heating_load  total heating load to be met by the 
c                operation of the heat pump and the furnace in W
c  ss_heat_pump_cap  capacity of the heat pump at steady state in W
c  HVAC_INPUT_DATA  common for HVAC input data
c  HEAT_PUMP_INPUT_DATA heat pump input data common
c  FURNACE_INPUT_DATA   furnace input data common

c OUTPUTS:
c   plrhp part-load ratio of the heat pump
c   plrb  part-load ratio of the backup source
c   plfhp part-load factor of the heat pump
c **********************************************************************
      SUBROUTINE ASHP_FURNACE_PLF(isys,heating_load,ss_heat_pump_cap,
     &                            plrhp,plfhp,plrb)

      IMPLICIT NONE

c Variables passed in call
      REAL heating_load,ss_heat_pump_cap,plrhp,plfhp,plrb

      INTEGER isys

#include "building.h"

#include "hvac_parameters.h"

#include "hvac_common.h"

#include "ashp_common.h"

#include "furnace_common.h"

c Local variables
      REAL jj(10,10),y(10),f(10),x(10),error,error_x4,x4_old,x4_new

      INTEGER i,j,m,icounter

      LOGICAL non_convergence

c The coefficient matrix jj and the constant matrix f are set based on the 
c method of Barringer for when the furnace and the heat pump operate 
c together to meet the heating load 

c First initialize all the coefficients and constants to zero. Note that
c the maximum number of unknowns is 7 depending on the furnace type. If the 
c furnace type is induced draft or condensing, then the total number of 
c unknowns is 6. For all other furnace types it is 7

c In the following formulation, the following nomenclature is adopted:
c 	x(1)	plfhp
c	x(2)	plrhp
c	x(4)	plfb
c	x(5)	plrb
c	x(5)	heat_eir_fplr (energy-input ratio of the heat pump)
c	x(6)	t (on-time fraction of the heat pump)
c	x(7)	hir (energy-input ratio of the furnace. This is needed only
c		     for induced draft and condensing furnaces)

c Logical variable to flag when a soluation is reached 
      non_convergence = .true.

c Counter to increment initial estimate for the heat pump part-load ratio x(2)
      icounter = 1

c Start loop for as long as solution is not reached
      do while(non_convergence)

c Initial estimates for all parameters x(1) through x(7)        
c These estimates are based on the equations governing the 
c operation of the heat pump

c The step for the initial estimate of the part-load ratio of the
c heat pump is 0.001. In case an initial estimate does not lead to a
c solution then that initial estimate is incremented by 0.001 at this
c point
        x(2) = 0.001 * icounter
        x(5) = ahp(isys) + bhp(isys) * x(2) + chp(isys) * x(2)**2 +
     &  dhp(isys) * x(2)**3
        x(1) = x(2) / x(5)
        x(6) = x(2) / x(1)
c For all furnace types, except condensing or induced draft
        if((isys_type(ibackup_sys_num(isys)).ne.8).and.
     &     (isys_type(ibackup_sys_num(isys)).ne.10)) then
          x(4) = af(ibackup_sys_num(isys)) * (1 - x(6)) - 
     &           bf(ibackup_sys_num(isys))
          x(3) = af(ibackup_sys_num(isys)) * x(4) / 
     &             (x(4) + bf(ibackup_sys_num(isys)))
c For induced draft or condensing furnaces
        elseif((isys_type(ibackup_sys_num(isys)).eq.8).or.
     &         (isys_type(ibackup_sys_num(isys)).eq.10)) then
          x(7) = 1 - x(6)
          error_x4 = 1
          x4_old = 0.5
c while loop to interatively solve for first estimate of x(4) for
c condensing and induced draft furnaces
          do while(error_x4.gt.1e-2)
            x4_new = (1 - x(6) - af(ibackup_sys_num(isys)) -
     &                cf(ibackup_sys_num(isys)) * x4_old**2 -
     &                df(ibackup_sys_num(isys)) * x4_old**3) / 
     &                bf(ibackup_sys_num(isys))
            error_x4 = abs(x4_old - x4_new)
            x4_old = x4_new
          end do
          x(4) = x4_new
          x(3) = x(4) / x(7)
        endif
       
c Initialize the error 
        error = 1.

        do while(error.gt.1e-4)
          do 10 i=1,10
            f(i) = 0.
            y(i) = 0.
            do 20 j=1,10
             jj(i,j) = 0.
  20        continue

  10      continue
c Use estimates for the variables to evaluate the coefficent and the constant
c matrix (jj and f)
          f(1) = - x(2) + x(1) * x(6)
          jj(1,1) = - x(6)
          jj(1,2) = 1.0
          jj(1,6) = - x(1)

          f(2) = - x(4) + x(3) * (1.0 - x(6))
          jj(2,3) = - (1.0 - x(6))
          jj(2,4) = 1.0
          jj(2,6) = x(3)

          f(3) = - x(1) * x(5) + x(2)
          jj(3,1) = x(5)
          jj(3,2) = -1.0
          jj(3,5) = x(1)

c Equation 4 is assciated with the part-load performance of the heat pump

          f(4) = ahp(isys)
     &         + bhp(isys) * x(2) 
     &         + chp(isys) * x(2)**2.0 
     &         + dhp(isys) * x(2)**3.0
     &         - x(5)

          jj(4,2) = - bhp(isys) - 2.0 * chp(isys) * x(2) - 
     &       3.0 * dhp(isys) * x(2)**2.0
          jj(4,5) = 1.0
        
c Equation 5 is associated with the energy balance on the space. The form of
c the equation depends on whether there is a circulation fan or not and on
c whether the circulation fan is in auto or continuous mode.

c If there is no circulation fan or the circulation fan is in continuous mode,
c the fan energy does not appear in the following energy balance. When the fan
c runs continuously, its effect is accounted for in the space heating load as 
c an internal gain.
          if((ifan_operation(isys).eq.0).or.
     &       (ifan_operation(isys).eq.2)) then
            f(5) = - (ss_heat_pump_cap / (heating_load + 0.00001)) * 
     &            x(2) - (ss_capacity(ibackup_sys_num(isys)) / 
     &           (heating_load + 0.00001)) * x(4) + 1
            jj(5,2) = ss_heat_pump_cap / (heating_load + 0.00001) 
            jj(5,4) = ss_capacity(ibackup_sys_num(isys)) / 
     &                (heating_load + 0.00001)
c If the circulation fan is in auto mode, the fan energy appears in the energy
c balance as reflected also in the definition of the part-load ratio
          elseif(ifan_operation(isys).eq.1) then
            f(5) = - ((ss_heat_pump_cap + fan_power(isys)) / 
     &               (heating_load + 0.00001)) * x(2) - 
     &               ((ss_capacity(ibackup_sys_num(isys)) + 
     &               fan_power(isys)) / (heating_load + 0.00001)) * 
     &               x(4) + 1
            jj(5,2) = (ss_heat_pump_cap + fan_power(isys)) / 
     &                (heating_load + 0.00001) 
            jj(5,4) = (ss_capacity(ibackup_sys_num(isys)) + 
     &                 fan_power(isys)) / (heating_load + 0.00001)
          endif

c For all furnace types, except condensing or induced draft, Equation 6
c is associated with the part-load performance equation
          if((isys_type(ibackup_sys_num(isys)).ne.8).and.
     &       (isys_type(ibackup_sys_num(isys)).ne.10)) then
            f(6) = - x(3) * (x(4) + bf(ibackup_sys_num(isys))) + 
     &             af(ibackup_sys_num(isys)) * x(4)
            jj(6,3) = x(4) + bf(ibackup_sys_num(isys))
            jj(6,4) = x(3) - af(ibackup_sys_num(isys))
            m = 6

c Call the solver to get the correction array y
            call SYS_SOL(jj,f,y,m)

c For induced draft or condensing furnaces
          elseif((isys_type(ibackup_sys_num(isys)).eq.8).or.
     &           (isys_type(ibackup_sys_num(isys)).eq.10)) then
            f(6) = - x(3) * x(7) + x(4)
            jj(6,3) = x(7)
            jj(6,4) = -1.0
            jj(6,7) = x(3)
c Equation 7 is based on the part-load performance of induced draft or
c condensing furnaces
            f(7) = - x(7) + af(ibackup_sys_num(isys)) + 
     &             bf(ibackup_sys_num(isys)) * x(4) + 
     &             cf(ibackup_sys_num(isys)) * x(4)**2 + 
     &             df(ibackup_sys_num(isys)) * x(4)**3
            jj(7,4) = - bf(ibackup_sys_num(isys)) - 
     &                2 * cf(ibackup_sys_num(isys)) * x(4) - 
     &                3 * df(ibackup_sys_num(isys)) * x(4)**2
            jj(7,7) = 1.      
            m = 7
            call SYS_SOL(jj,f,y,m)
          endif

c Update the values of the unknowns and the error
          error = 0.
          do 40 i = 1,7
            x(i) = x(i) + 0.75 * y(i)
            error = error + y(i)**2
 40       continue

c In the case the error associated with the iteration is too large,
c then it is assumed that the solution method is diverging. In this 
c case the icounter is updated so that new initial values are estimated. 
c The inner while loop is exited in this case.
c In the case that icounter is greater than 1000, it is an indication that
c the initial value of x(2) is greater than 1. This can not be and a 
c warning message is issued.
          if(error.gt.1e5) then
            icounter = icounter + 1
            if(icounter.gt.1000) then
              write(*,*) 'ASHP_FURNACE_PLF: estimated plr > 1'
            endif
            goto 101
          endif

c If the error is less than 1e-4 then we have reached convergence. In this
c update the non convergence logical variable to false
          if(error.lt.1e-4) then
            non_convergence = .false.
          endif
       
        end do
 101  end do
     
c Set the values of the part-load ratios and factors
      plrhp = x(2)
      plrb = x(4)
      plfhp = x(1)

      return
      end

c *********************************************************************
c *********************************************************************
c SYS_SOL

c Created by: Kamel Haddad
c Initial Creation Date: July 25th 2000
c Copyright 2000: NRCan Buildings Group
c
c Subroutine to solve a system of m linear equations in m unknowns of the form
c 		jj y = f
c using the back substitution method
c
c References:	Numerical Analysis, 3rd Edition
c		Richard L. Burden
c		J. Douglas Faires
c
c INPUTS:
c	jj	coefficient matrix of the system jj y = f
c	f	constant matrix of the system jj y = f
c	m	total number of equations and unknowns in the system

c OUTPUTS:
c	y	solution matrix
c **********************************************************************
      SUBROUTINE SYS_SOL(jj,f,y,m)

      IMPLICIT NONE

c Variables in call
      REAL jj(10,10),f(10),y(10)

      INTEGER m

c Local variables
      REAL sum,dummy2,dummy1,mm

      INTEGER i,j,kk,ii,p,k
       

      do 300 i=1,m-1

        do 310 j=i,m

          if(jj(j,i).ne.0) then  
            p=j
            goto 330
          endif

 310    continue

 330    if(p.ne.i)  then
          dummy2=f(i)
          f(i)=f(p)
          f(p)=dummy2

          do 320 j=1,m
            dummy1=jj(i,j)
            jj(i,j)=jj(p,j)
            jj(p,j)=dummy1
 320      continue

        endif

        do 340 j=i+1,m
       
          if(jj(i,i).eq.0.) then
             jj(i,i) = 0.00001
          endif
          mm=jj(j,i)/jj(i,i)
          f(j)=f(j)-mm*f(i)

          do 350 k=1,m
            jj(j,k)=jj(j,k)-mm*jj(i,k)
 350      continue

 340    continue

 300  continue

      if(jj(m,m).eq.0) then
        jj(m,m) = 0.00001
      endif
      y(m)=f(m)/jj(m,m)
      sum=0.

      do 360 i=1,m-1
        ii=m-i

        do 370 j=ii,m
          sum=sum+jj(ii,j)*y(j)
 370    continue

        if(jj(ii,ii).eq.0.) then
          jj(ii,ii) = 0.00001
        endif
        y(ii)=(f(ii)-sum)/jj(ii,ii)
        sum=0.
 360  continue

      return
      end

c *********************************************************************
c *********************************************************************
c SET_CONT_FUNC_HEAT_CAP

c Created by: Kamel Haddad
c Initial Creation Date: March 10th 2008
c Copyright 2000: NRCan Buildings Group

c Subroutine to update heating capacity of appropriate control function
c associated with heat pump in heating mode 

c INPUTS:
c	isys		hvac system number
c	sensible_heating_cap
c                       sensible heating capacity of equipment
c	HVAC_INPUT_DATA	HVAC input data common

c OUTPUTS:
c	cont_fun_heat_cap
c			new heating capacity for hvac system isys. This 
c			new heating capacity is used later to update the
c			maximum heating capacity of the control function
c			associated with hvac system isys. 
c	hvac_timestep_parameters	
c                       common for updated capacity of control function
c **********************************************************************
      SUBROUTINE SET_CONT_FUNC_HEAT_CAP(isys,sensible_heating_cap)

      IMPLICIT NONE
#include "building.h"

#include "hvac_parameters.h"

#include "hvac_common.h"

c Variables passed in call
      REAL sensible_heating_cap

      INTEGER isys

c Local Variables
      INTEGER icounter
      LOGICAL close
      REAL small,zone_heat_cap

      

      small = 1.e-6

c In the following the effect of the circulation fan is accounted for.
c If circulation fan is in auto mode, increase the capacity of the controller
      do 10 icounter = 1,num_sys_zones(isys)
        if(ifan_operation(isys).eq.1) then
          cont_fun_heat_cap(isys_zone_num(isys,icounter)) = 
     &    sys_zone_cap_frac(isys,icounter) * 
     &    (sensible_heating_cap + fan_power_auto(isys))

c If circulation fan is in continuous mode, no effect on controller capacity
        elseif(ifan_operation(isys).eq.2) then
          cont_fun_heat_cap(isys_zone_num(isys,icounter)) = 
     &    sys_zone_cap_frac(isys,icounter) * sensible_heating_cap

c If no circulation fan, no effect on controller capacity
        elseif(ifan_operation(isys).eq.0) then
          cont_fun_heat_cap(isys_zone_num(isys,icounter)) = 
     &    sys_zone_cap_frac(isys,icounter) * sensible_heating_cap

        endif

c Check whether capacity of controller for zone is zero
        zone_heat_cap = cont_fun_heat_cap(isys_zone_num(isys,icounter))
        call eclose(zone_heat_cap,0.0,small,close)
c If it is 0, then set capacity to small number to override controller
c capacity that might be listed in the control file.
        if(close) then
         cont_fun_heat_cap(isys_zone_num(isys,icounter)) = small
        endif

 10   continue

      return
      end
