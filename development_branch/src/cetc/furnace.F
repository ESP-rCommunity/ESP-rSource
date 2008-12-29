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
c	FURNACE	  determine the energy consumption of the furnace
c	FURNACE_PLF
c		  determine the furnace part-load factor
c	FURNACE_COEFF
c		  set the furnace coeff for the part-load performance
c
c *********************************************************************
c *********************************************************************
c FURNACE

c Created by: Kamel Haddad
c Initial Creation Date: February 17th 2000
c Copyright 2000: NRCan Buildings Group
c
c Modified by Ian Beausoleil-Morrison on July 13, 2001:
c   - Pass furnace part-load ratio to AIM-2 model.
c
c Subroutine to determine the energy consumption of the furnace for the 
c current time step. This subroutine is called from HVACSIM after the 
c heating or the cooling load to be supplied by the HVAC equipment is
c determined.
 
c references: "Further developments to the HOT2000 subroutine - Part B:
c              furnace an heat pump models", prepared for Energy Mines 
c              and Resource Canada, Energy Efficiency Division. 
c              Submitted by C. Barringer. Report No. ET-91-045

c             "Part load curves for use in DOE-2"
c              Prepared for the Lawrence Berkley Laboratory by
c              Hugh Henderson, CDH Energy Corp, Cazenovia, NY

c	      "Furnace Model for Implementation in H3K",
c	      H3K Model Documentation, Kamel Haddad, April 5th 2000

c INPUTS:
c	HVAC_INPUT_DATA
c			common for hvac input data
c	FURNACE_INPUT_DATA
c			common for furnace input data
c	UNDER_HEAT_COOL_HOURS
c			common for number of hours zones served by HVAC 
c			system are under heated or cooled
c	pers		common containing the number of time steps per hour
c	isys		HVAC system number
c       heating_load	total combined heating load of the zones served
c                       by the HVAC system isys 

c OUTPUTS:
c	furnace_energy	total energy consumed by the furnace system in J. This
c			includes the energy required when the furnace is on,
c			and the pilot energy when the furnace is off.
c       pilot_energy    energy consumption by the pilot of the furnace if
c                       applicable in J
c       plrf            part-load ratio of the equipment
c
c NOTE: Note that the model does not include at this point an estimate
c for the energy consumption of the draft fan for induced draft furnaces
c *********************************************************************
      SUBROUTINE FURNACE(isys,heating_load,furnace_energy,
     &pilot_energy_con,plrf,plff)

      IMPLICIT NONE

c Declare variables passed in call
      INTEGER isys
      REAL heating_load,furnace_energy,pilot_energy_con,plrf,
     &     plff

#include "building.h"
 
#include "h3kstore_parameters.h"

#include "hvac_parameters.h"

#include "hvac_common.h"

#include "furnace_common.h"

#include "h3kstore_common.h"


C HVAC_CALC_DATA common used to pass pass furnace part-load ratio to AIM-2 model
C on time-step basis.
      common/HVAC_CALC_DATA/HVAC_furnace_plr

c Declare real in HVAC_CALC_DATA common
      REAL HVAC_furnace_plr

c Common for the number of time steps per hour
      COMMON/PERS/ISD1,ISM1,ISD2,ISM2,ISDS,ISDF,NTSTEP

c Declare integers in PERS common
      INTEGER ISD1,ISM1,ISD2,ISM2,ISDS,ISDF,NTSTEP

c hvac system file and flag:  	ihvacflag=1 there is hvac file
c				ihvacflag=0 no hvac file
      common/hvacinfo/ihvacflag, hvacfile
      CHARACTER*72 hvacfile
      INTEGER ihvacflag

c Declare local variables
      REAL time_step,PLR,FURNACE_PLF,TIME_INTERVAL

c In order to determine the energy consumption of the furnace, the part
c load factor plff and the part load ratio plrf for the current time step are 
c needed. 
      
c If the furnace is not a backup system, the part-load ratio plrf has not 
c been estimated yet at this point. If the furnace is a backup system,
c the part-load ratio is set in the heat pump subroutine and its value is 
c passed on to the furnace subroutine in plrf.
      if(ipriority(isys).eq.1) then
        plrf = PLR(heating_load,ss_capacity(isys),fan_power(isys),
     &             ifan_operation(isys))
c Call subroutine to set the controller heating capacity for each zone 
c served by furnace
        call SET_CONT_FUNC_HEAT_CAP(isys,ss_capacity(isys))
      endif

c If plrf > 1, the furnace does not have enough capacity to meet the load for the
c hour. The undercapacity of the heating equipment is not communicated back to the
C zonal energy balances. As such, the temperature of the zone will be unaffected by
C the under capacity. However, the information is tracked in the form of recording
C the number of hours in which the space is under heated.
      if(plrf.gt.0.999) then
        plrf = 1.
        ihrs_under_heat(isys) = ihrs_under_heat(isys) + 1
      endif

c Calculate the part-load factor plff. This represents the degredation
c in furnace efficiency due to part-load operation. plff is multiplied
c by the steady-state efficiency to give the efficiency at the part-load
c operating point.
      if(plrf.gt.0) then
        plff = FURNACE_PLF(isys,isys_type(isys),plrf)
      endif

c Simulation time step in seconds based on the number of time steps per
c hour
      time_step = TIME_INTERVAL(ntstep)

c Furnace energy consumption for the hour in J
c Equation 1 in Furnace Ref by Haddad
      furnace_energy = time_step * (ss_capacity(isys) /
     &(ss_efficiency(isys))) * plrf / plff

c For furnaces with a continuous pilot, the energy consumption of the pilot
c has to be accounted for. Only natural gas and propane furnaces come with 
c a continuous pilot.
      if(isys_type(isys).eq.7) then
c Equation 14 in Furnace Ref by Haddad
        pilot_energy_con = time_step * pilot_power(isys) * (1 - plrf)
      endif

C Save part-load ratio to variable that can be passed by MODULE to
C AIM-2 model. This will be used in AIM-2 model as a proxy for the
C furnace's on-time fraction.
      HVAC_furnace_plr = plrf
      return
      end


c *********************************************************************
c *********************************************************************
c FURNACE_PLF

c Created by: Kamel Haddad
c Initial Creation Date: July 18th 2000
c Copyright 2000: NRCan Buildings Group
c
c Function to determine the part-load factor of a furnace

c References:
c		"Further developments to the HOT2000 subroutine - Part B:
c              	furnace and heat pump models", prepared for Energy Mines 
c             	and Resource Canada, Energy Efficiency Division. 
c              	Submitted by C. Barringer. Report No. ET-91-045

c             	"Part load curves for use in DOE-2"
c              	Prepared for the Lawrence Berkley Laboratory by
c              	Hugh Henderson, CDH Energy Corp, Cazenovia, NY

c	        "Furnace Model for Implementation in H3K",
c	        H3K Model Documentation, Kamel Haddad, April 5th 2000

c INPUTS:
c	isys		number of HVAC system
c	isys_type	type of furnace
c	plrf		part-load ratio of the heat pump
c       af,bf,cf,df     stored in common furnace_corr. These the correlation
c                       coefficients for the part load performance of the 
c                       furnace
c	FURNACE_INPUT_DATA
c			common for furnace input data

c OUTPUTS:
c       FURNACE_PLF	estimate of the part-load factor
c **********************************************************************    
      REAL FUNCTION FURNACE_PLF(isys,isys_type,plrf)

      IMPLICIT NONE

c Declare variables passed in call
      INTEGER isys,isys_type
      REAL plrf

#include "hvac_parameters.h"

#include "furnace_common.h"
      
c Declare local variables
      REAL hir

c For all furnace types,except induced draft and condensing,the
c Part-Load Factor plff is determined based on the formulation 
c of Barringer.

c For all furnace types except induced draft and condesning, the energy
c consumption is based on an equation listed in the report of Barringer
c listed at the beginning of the routine
      if((isys_type.ne.8).and.(isys_type.ne.10)) then
c Equation 5 in Furnace Ref by Haddad
        FURNACE_PLF = plrf * af(isys) / (plrf + bf(isys))
c For induced draft and condensing furnaces
      elseif((isys_type.eq.8).or.(isys_type.eq.10)) then
c hir ratio based on the correlation suggested in the LBL report listed 
c at the beginning of this subroutine
c Equation 7 in Furnace Ref by Haddad
        hir = af(isys) + bf(isys) * plrf + cf(isys) * plrf**2 + 
     &        df(isys) * plrf**3
        FURNACE_PLF = plrf / hir
      endif   
                    
      return
      end

c *********************************************************************
c *********************************************************************
c FURNACE_COEFF

c Created by: Kamel Haddad
c Initial Creation Date: July 25th 2000
c Copyright 2000: NRCan Buildings Group
c
c Subroutine to determine the correlation coefficients for the part-load
c performance of the furnace

c References:
c		"Further developments to the HOT2000 subroutine - Part B:
c              	furnace an heat pump models", prepared for Energy Mines 
c             	and Resource Canada, Energy Efficiency Division. 
c              	Submitted by C. Barringer. Report No. ET-91-045

c             	"Part load curves for use in DOE-2"
c              	Prepared for the Lawrence Berkley Laboratory by
c              	Hugh Henderson, CDH Energy Corp, Cazenovia, NY

c INPUTS:
c	isys		number for hvac system
c	isys_type	type of furnace
c	FURNACE_INPUT_DATA
c			common for furnace specific input data

c OUTPUTS:
c       af,bf,cf,df	part-load performance factors stored in common
c			FURNACE_INPUT_DATA
c ************************************************************************
      SUBROUTINE FURNACE_COEFF(isys,isys_type)
      
      IMPLICIT NONE

c Declare variables passed in call
      INTEGER isys,isys_type


#include "hvac_parameters.h"

#include "furnace_common.h"

C.....Common from hvac_common.h needed for fuel type (but rest
C.....of hvac_common.h conflicts with furnace_common.h!)
      common/HVAC_fuel_type/iFuel_type(max_sys)
      integer iFuel_type

c For all furnaces, except condensing and induced draft, the plff 
c equation is based on two correlation coefficients af and bf which depend
c on the furnace type. For condensing and induced draft furnaces, the plff 
c is derived from the heat input ratio hir. The quantity hir is based on 
c a correlation from the LBL report listed above. The correlation for hir 
c is a 3rd oder polynomial with coefficients af, bf, cf, and df.

c The fuel types associated with each of the values of the paramater 
c ifuel_type are: 
c 	=1 electric
c 	=2 natural gas
c 	=3 oil
c	=4 propane
c 	=5 wood

c The furnace types associated with each of the values of parameter
c isys_type are:
c 	=1 advanced air tight wood stove
c 	=2 catalytic converter
c 	=3 flame retention head
c  	=4 flue vent damper
c 	=5 spark-ignition
c	=6 spark ignition and vent damper
c 	=7 continuous pilot
c 	=8 condensing furnace
c	=9 direct vent non condensing
c  	=10 induced draft
c 	=11 mid efficiency
c 	=12 electric forced-air
c	=13 wood furnace
c	=14 oil furnace


c** Electric (type =1) can have systypes = 12**
c For electric furnaces
      if(ifuel_type(isys).eq.1) then

c For electric forced-air furnaces
        if(isys_type.eq.12) then
c Note that af and bf appear in Equation 5 in Furnace Ref by Haddad
          af(isys) = 1.
          bf(isys) = 0
        endif      
c** Natural Gas (type =2) can have systypes = 5,6,7,8,10**
c For natural gas furnaces
      elseif(ifuel_type(isys).eq.2) then

c For natural gas spark ignition furnaces
        if(isys_type.eq.5) then
          af(isys) = 1.008
          bf(isys) = 0.0166
c For natural gas with spark ignition and vent damper
        elseif(isys_type.eq.6) then
          af(isys) = 1.008
          bf(isys) = 0.0166
c For natural gas with continous pilot
        elseif(isys_type.eq.7) then
          af(isys) = 1.008
          bf(isys) = 0.0166
c For natural gas condensing furnaces
        elseif(isys_type.eq.8) then
c af, bf, cf, and df appear in Equation 7 in Furnace Ref by Haddad
          af(isys) = 0.0080472
          bf(isys) = 0.8756445
          cf(isys) = 0.2924994
          df(isys) = -0.176241   
c For natural gas induced draft fan furnaces
        elseif(isys_type.eq.10) then
          af(isys) = 0.0080472
          bf(isys) = 0.8756445
          cf(isys) = 0.2924994
          df(isys) = -0.176241
        endif       

c** Oil (type =3) can have sys_types = 3,4,8,9,11,14**
c For oil furnaces
      elseif(ifuel_type(isys).eq.3) then

c For oil flame retention head furnaces
        if(isys_type.eq.3) then
          af(isys) = 1.005
          bf(isys) = 0.0093
c For oil flue vent damper furnaces
        elseif(isys_type.eq.4) then
          af(isys) = 1.005
          bf(isys) = 0.0093
c For oil condensing furnaces
        elseif(isys_type.eq.8) then
          af(isys) = 0.0080472
          bf(isys) = 0.8756445
          cf(isys) = 0.2924994
          df(isys) = -0.176241
c For oil direct vent non-condensing furnaces
        elseif(isys_type.eq.9) then
          af(isys) = 1.005
          bf(isys) = 0.0093
c For oil mid-efficiency furnace
        elseif(isys_type.eq.11) then
          af(isys) = 1.005
          bf(isys) = 0.0093
c For oil furnace
        elseif(isys_type.eq.14) then
          af(isys) = 1.005
          bf(isys) = 0.0093
	endif
	
c** Propane (type =4) can have sys_types = 5,6,7,8,10**
c For propane furnaces
      elseif(ifuel_type(isys).eq.4) then

c For propane spark ignition furnaces
        if(isys_type.eq.5) then
          af(isys) = 1.008
          bf(isys) = 0.0166
c For propane spark ignition and vent damper
        elseif(isys_type.eq.6) then
          af(isys) = 1.008
          bf(isys) = 0.0166
c For propane continous pilot furnaces
        elseif(isys_type.eq.7) then
          af(isys) = 1.008
          bf(isys) = 0.0166
c For propane condensing furnaces
        elseif(isys_type.eq.8) then
          af(isys) = 0.0080472
          bf(isys) = 0.8756445
          cf(isys) = 0.2924994
          df(isys) = -0.176241
c For propane induced draft furnaces
         elseif(isys_type.eq.10) then
          af(isys) = 0.0080472
          bf(isys) = 0.8756445
          cf(isys) = 0.2924994
          df(isys) = -0.176241
         endif

c** Wood (type =5) can have sys_types = 1,2,13**
c For wood furnaces
      elseif(ifuel_type(isys).eq.5) then

c For advanced air tight wood furnace
          if(isys_type.eq.1) then
            af(isys) = 1.008
            bf(isys) = 0.0166
c For advanced air tight wood furnaces with catalytic converter
          elseif(isys_type.eq.2) then
            af(isys) = 1.008
            bf(isys) = 0.0166
c For wood furnaces
          elseif(isys_type.eq.13) then
           af(isys) = 1.008
           bf(isys) = 0.0166
          endif

      endif    

      return
      end 

