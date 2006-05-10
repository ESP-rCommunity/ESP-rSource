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
c	HVACINPUT  control reading the hvac data from the hvac input file
c 		   and set the size of the indoor circulation fan if not 
c		   specified
c 	FURNACE_INPUT
c		   read data for furnace system
c       BASEBOARD_INPUT
c  		   read data for Baseboards/Hydronics/Plenum Heaters
c	ASHP_INPUT read data for air-source heat pump system
c       ASHP_INPUT_VERSION1_2
C                  read input data for air-source heat pump system for
c                  version 1.2 and older of file hvacinput.F
c *********************************************************************
c *********************************************************************
c HVACINPUT

c Created by: Kamel Haddad
c Initial Creation Date: February 17th 2000
c Copyright 2000: NRCan Buildings Group

c Subroutine to control reading of data associated with the HVAC system.
c This routine is called from ERSYS. It is possible to simulate more than 
c 1 hvac system serving the building. The routine calls other subroutines
c to read data for specific hvac systems. The data is stored in common
c hvac_data. It is called only once when the problem is read in and is 
c used in susequent time step simulations.

c INPUTS:
c	hvacfile	name of hvac data file
c       hvac.h          include file for hvac parameters
c OUTPUTS:
c 	HVAC_INPUT_DATA
C			common for HVAC system input data
c *********************************************************************
      SUBROUTINE HVACINPUT(hvacfile)

      IMPLICIT NONE

c Variable passed in call
      character hvacfile*72
#include "building.h"
#include "hvac_parameters.h"

#include "hvac_common.h"

      COMMON/FILEP/IFIL
      COMMON/ER/IER
      COMMON/OUTIN/IUOUT,IUIN

      INTEGER IUNIT,IFIL,k,IER,ND,ival,i,ISTAT,IUOUT, IUIN
      REAL val

c Define the characters variables associated with the utility 
c functions STRIPC,EGETWI,and EGETWR
      character OUTSTR*124,word*124

c Declare function for determining fan size
      REAL FAN_SIZE
 
C Open the hvac file
      IUNIT=IFIL+20
      CALL EFOPSEQ(IUNIT,hvacfile,1,IER)

c Read the total number of HVAC systems serving the various zones in
c the building
      k = 0
      CALL STRIPC(IUNIT,OUTSTR,99,ND,1,'hvac data',IER)
      CALL EGETWI(OUTSTR,K,ival,1,10,'F',
     &'hvac data: num_hvac_sys',IER)
      num_hvac_sys=ival 
c Read site altitude
      CALL EGETWR(OUTSTR,K,val,0.,10000.,'F',
     &'hvac input: site_altitude',IER)
      site_altitude = val

c Read the following information about each of the hvac systems:

c   ihvac_type(i)	indicates the system type (furnace, heat pump,
c              		air conditioner, etc ...)
c                       =1 furnace
c                       =7 air-source heat pump
c                       =8 ground-source heat pump
c                       =9 ground-source heat pump (Ecole Polytechnique, GCEP)
c   ipriority(i) 	indicates whether the system is a primary heating
c			source or a backup system
c			=1 primary system
c			=2 backup system
c                               
c   num_sys_zones(i)	total number of zones served by an HVAC system

c Read the data associated with each of the HVAC systems
      do 10 i = 1,num_hvac_sys
        CALL STRIPC(IUNIT,OUTSTR,99,ND,1,'hvac input',IER)
        K = 0
c Read the type of the hvac system ihvac_type
        CALL EGETWI(OUTSTR,K,ival,1,9,'F','hvac input: ihvac_type',IER)
        ihvac_type(i) = ival
c Read whether the hvac system is a primary system or a backup system
        CALL EGETWI(OUTSTR,K,ival,1,2,'F','hvac input: ipriority',IER)
        ipriority(i) = ival
c Read the total number of zones being served by the HAVC system
c num_sys_zones
        CALL EGETWI(OUTSTR,K,ival,1,100,'F',
     &'hvac input: num_sys_zones',IER)
        num_sys_zones(i) = ival

c The data required to simulate the performance of the hvac system depends
c on the hvac system type. For each type of hvac system, call the appropriate
c subroutine to read input data

c If the hvac system is a furnace, call FURNACE_INPUT
        if(ihvac_type(i) .eq. 1 ) then
          call FURNACE_INPUT(i)
c If the hvac system is a boiler, call FURNACE_INPUT. Boiler model is
c the same as furnace model
        elseif(ihvac_type(i) .eq. 2) then
          call FURNACE_INPUT(i)
c If hvac system is Baseboards/Hydronics/Plenum Heater, call BASEBOARD_INPUT
        elseif(ihvac_type(i) .eq. 3) then
          call BASEBOARD_INPUT(i)
c If the hvac system is an air-source heat pump, call ASHP_INPUT
        elseif(ihvac_type(i) .eq. 7) then
          call ASHP_INPUT(i)
c GSHP: begin
C If the hvac system is a ground-source heat pump, call ASHP_INPUT
        elseif(ihvac_type(i) .eq. 8) then
          call ASHP_INPUT(i)
C GSHP: end

c GCEP: begin
C If the hvac system is a ground-source heat pump using the algorithm from Ecole Polytechnique,
C call ASHP_INPUT
        elseif(ihvac_type(i) .eq. 9) then
          call ASHP_INPUT(i)
C GCEP: end

        endif

c If there is a circulation fan associated with the hvac system
c (ifan_operation > 0), and the capacity of the fan is not specified by
c the user (fan_power < 0), then function FAN_SIZE is used to set an 
c appropriate fan size. For systems with backup heat, the backup heat
c data has to be read in before that of the primary system. 
        if((fan_power(i) .lt. 0).and.((ifan_operation(i) .eq. 1).or.
     &    (ifan_operation(i) .eq. 2)).and.(ipriority(i) .eq. 1)) then
          fan_power(i) = FAN_SIZE(i)
        endif
c If the fan power associated with the system in auto mode is < 0, then
c set this equal to the total fan power fan_power
        if(fan_power_auto(i) .lt. 0.) then
          fan_power_auto(i) = fan_power(i)
        endif

  10  end do

      CALL ERPFREE(IUNIT,ISTAT)
      return
      end

c *********************************************************************
c *********************************************************************
c FURNACE_INPUT

c Created by: Kamel Haddad
c Initial Creation Date: December 12th 2000
c Copyright 2000: NRCan Buildings Group

c Subroutine called by HVACINPUT to read data from hvac input file for
c a furnace system.

c INPUTS:
c       i 		system number associated with furnace system

c OUTPUTS:
c	hvac.h  	file for hvac hvac parameters
c	FURNACE_INPUT_DATA
c			common for furnace data
c *********************************************************************
      SUBROUTINE FURNACE_INPUT(i)

      IMPLICIT NONE

c Variable passed in call
      INTEGER i
#include "building.h"
#include "hvac_parameters.h"

#include "hvac_common.h"

#include "furnace_common.h"

      COMMON/FILEP/IFIL
      COMMON/ER/IER
      COMMON/OUTIN/IUOUT,IUIN

c Declare local variables
      INTEGER IUNIT,IFIL,ND,IER,k,ival,j,IUOUT,IUIN
      REAL val

c Define the characters variables associated with the utility 
c functions STRIPC,EGETWI,and EGETWR
      character OUTSTR*124,word*124

c Declare number of variables in input file record
      INTEGER num_furnace_var

      IUNIT=IFIL+20

      CALL STRIPC(IUNIT,OUTSTR,99,ND,1,'hvac input',IER)
      k = 0

c Find the total number of entries in the record in the hvac file 
c associated with furnace data when the power of the draft fan is
c part of the input data. This is needed to distinguish new hvac
c input files with draft fan power from the old files without 
c draft fan power for furnaces.
      num_furnace_var = 9 + 2 * num_sys_zones(i)

c In order to specify the furnace system, the furnace type (isys_type) 
c and the fuel type (ifuel_type) have to be specified. Once these two 
c parameters are specified, it is possible then to choose the part-load
c performance curve for the furnace. The coefficients for the part-load 
c performance are specified in subroutine FURNACE_COEFF in file furnace.F

c Read the variation of the HVAC system type isys_type
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
      CALL EGETWI(OUTSTR,K,ival,1,14,'F',
     &'hvac input: isys_type',IER)
      isys_type(i) = ival
c Read the fuel type used by the HVAC system ifuel_type
c 	=1 electric can have isys_types = {12}
c 	=2 natural gas can have isys_types = {5,6,7,8,10}
c 	=3 oil can have isys_types = {3,4,8,9,11,14}
c	=4 propane can have isys_types = {5,6,7,8,10}
c 	=5 wood can have isys_types = {1,2,13}
      CALL EGETWI(OUTSTR,K,ival,1,5,'F',
     &'hvac input: ifuel_type',IER)
      ifuel_type(i) = ival

      IF ( ifuel_type(i) .eq.1) then
         electric: SELECT CASE (isys_type(i))
      CASE(12)
         write(iuout,*)'Fuel/HVAC system Combination good'
      CASE DEFAULT
         write(iuout,*)'Incorrect Fuel/HVAC system in HVAC config file'
         write(iuout,*)'Correct Combinations are:'
         write(iuout,*)'electric can have isys_types = {12}'
         write(iuout,*)'natural gas can have isys_types = {5,6,7,8,10}'
         write(iuout,*)'oil can have isys_types = {3,4,8,9,11,14}'
         write(iuout,*)'propane can have isys_types = {5,6,7,8,10}'
         write(iuout,*)'wood can have isys_types = {1,2,13}'
         STOP
         END SELECT electric
      ENDIF

      IF ( ifuel_type(i).eq.2) then
      ngas: SELECT CASE (isys_type(i))
      CASE (5,6,7,8,10)
         write(iuout,*)'Fuel/HVAC system Combination good'
      CASE DEFAULT
         write(iuout,*)'Incorrect Fuel/HVAC system in HVAC config file'
         write(iuout,*)'Correct Combinations are:'
         write(iuout,*)'electric can have isys_types = {12}'
         write(iuout,*)'natural gas can have isys_types = {5,6,7,8,10}'
         write(iuout,*)'oil can have isys_types = {3,4,8,9,11,14}'
         write(iuout,*)'propane can have isys_types = {5,6,7,8,10}'
         write(iuout,*)'wood can have isys_types = {1,2,13}'
         STOP
         END SELECT ngas
      ENDIF

      IF ( ifuel_type(i) .eq.3) then
      oil: SELECT CASE (isys_type(i))
      CASE (3,4,8,9,11,14)
         write(iuout,*)'Fuel/HVAC system Combination good'
      CASE DEFAULT
         write(iuout,*)'Incorrect Fuel/HVAC system in HVAC config file'
         write(iuout,*)'Correct Combinations are:'
         write(iuout,*)'electric can have isys_types = {12}'
         write(iuout,*)'natural gas can have isys_types = {5,6,7,8,10}'
         write(iuout,*)'oil can have isys_types = {3,4,8,9,11,14}'
         write(iuout,*)'propane can have isys_types = {5,6,7,8,10}'
         write(iuout,*)'wood can have isys_types = {1,2,13}'
         STOP
         END SELECT oil
      ENDIF

      IF ( ifuel_type(i) .eq.4) then
      propane: SELECT CASE (isys_type(i))
      CASE (5,6,7,8,10)
         write(iuout,*)'Fuel/HVAC system Combination good'
      CASE DEFAULT
         write(iuout,*)'Incorrect Fuel/HVAC system in HVAC config file'
         write(iuout,*)'Correct Combinations are:'
         write(iuout,*)'electric can have isys_types = {12}'
         write(iuout,*)'natural gas can have isys_types = {5,6,7,8,10}'
         write(iuout,*)'oil can have isys_types = {3,4,8,9,11,14}'
         write(iuout,*)'propane can have isys_types = {5,6,7,8,10}'
         write(iuout,*)'wood can have isys_types = {1,2,13}'
         STOP
         END SELECT propane
      ENDIF

      IF ( ifuel_type(i) .eq.5) then
      wood: SELECT CASE (isys_type(i))
      CASE (1,2,13)
         write(iuout,*)'Fuel/HVAC system Combination good'
      CASE DEFAULT
         write(iuout,*)'Incorrect Fuel/HVAC system in HVAC config file'
         write(iuout,*)'Correct Combinations are:'
         write(iuout,*)'electric can have isys_types = {12}'
         write(iuout,*)'natural gas can have isys_types = {5,6,7,8,10}'
         write(iuout,*)'oil can have isys_types = {3,4,8,9,11,14}'
         write(iuout,*)'propane can have isys_types = {5,6,7,8,10}'
         write(iuout,*)'wood can have isys_types = {1,2,13}'
         STOP
         END SELECT wood
      ENDIF
         

c Set the correlation coefficients for the part-load performance by
c calling FURNACE_COEFF. The correlation coefficients are stored in 
c common furnace_corr
      call FURNACE_COEFF(i,isys_type(i))

c Read the actual zone numbers associated with the HVAC system 
c isys_zone_num and the capacity fraction delivered to each zone 
c isys_zone_cap_frac. The next loop is repeated as many times as 
c there are zones served by the HVAC system
      do 20 j = 1,num_sys_zones(i)
c Read zone numbers for zones served by system i
         CALL EGETWI(OUTSTR,K,ival,1,100,'F',
     &'hvac input: isys_zone_num',IER)
         isys_zone_num(i,j) = ival
c Read fraction of capacity of system delivered to zone j
         CALL EGETWR(OUTSTR,K,val,0.,1.,'F',
     &'hvac input: sys_zone_cap_frac',IER)
         sys_zone_cap_frac(i,j) = val  
   20 end do

c Read the steady-state capacity of the equipment ss_capacity 
      CALL EGETWR(OUTSTR,K,val,0.,1e6,'F',
     &'hvac input: ss_capacity',IER)
      ss_capacity(i) = val
c Read the steady-state efficiency of the equipment ss_efficiency
      CALL EGETWR(OUTSTR,K,val,0.,1.,'F',
     &'hvac input: ss_efficiency',IER)
      ss_efficiency(i) = val
c Read the circulation fan operation status ifan_operation. If the
c furnace is a backup system, the mode of operation of the fan is the
c same as that for the primary system
c	=0 no fan associated with hvac system
c	=1 fan in auto mode
c       =2 fan in continous mode
      CALL EGETWI(OUTSTR,K,ival,0,2,'F',
     &'hvac input: ifan_operation',IER)
      ifan_operation(i) = ival
c Read the indoor fan power. If the furnace is a backup
c system, the fan power entered here is the same as that for
c the primary system
      CALL EGETWR(OUTSTR,K,val,-10.,1.e4,'F',
     &'hvac input: fan_power',IER)
      fan_power(i) = val
c Read draft fan power. This is done only for the hvac files that were
c created after this change was made. This is the change where the draft
c fan power is read in the hvac input file. It is possible this way to 
c process old and new hvac files.
      if(ND.eq.num_furnace_var) then
        CALL EGETWR(OUTSTR,K,val,0.,200.,'F',
     &  'hvac input: draft_fan_power',IER)
        draft_fan_power(i) = val
      else
        draft_fan_power(i) = 0.
      endif
c Read the pilot power pilot_power. This is no zero only for furnaces with 
c a continuous pilot. Only propane and natural gas furnaces come with a 
c continuous pilot 
      CALL EGETWR(OUTSTR,K,val,0.,1e3,'F',
     &'hvac input: pilot_power',IER)
      pilot_power(i) = val
c Read whether there is a duct system associated with the HVAC system
      CALL EGETWI(OUTSTR,K,ival,0,1,'F',
     &'hvac input: iduct_sys_flag',IER)
      iduct_sys_flag(i) = ival

      return
      end

c *********************************************************************
c *********************************************************************
c ASHP_INPUT

c Created by: Kamel Haddad
c Initial Creation Date: December 12th 2000
c Copyright 2000: NRCan Buildings Group
c Updated: Julia Purdy, April 3, 2002.

c Subroutine called by HVACINPUT to read data from hvac input file for
c air-source heat pump and ground source heat pump system.

c INPUTS:
c       i 		system number associated with air-source heat
c 			pump system
c       hvac.h          include file for hvac parameters

c OUTPUTS:
c	HVAC_INPUT_DATA
c			common for HVAC input data
c	HEAT_PUMP_INPUT_DATA
c			common for heat pump input data
c *********************************************************************
      SUBROUTINE ASHP_INPUT(i)

      IMPLICIT NONE

c Variable passed in call
      INTEGER i
#include "building.h"
#include "hvac_parameters.h"

#include "hvac_common.h"

#include "ashp_common.h"


      COMMON/FILEP/IFIL
      COMMON/ER/IER
      COMMON/OUTIN/IUOUT,IUIN

c Declare local variables
      INTEGER IUNIT,IFIL,ND,IER,k,ival,j,IUOUT,IUIN
      REAL val

c Define the characters variables associated with the utility 
c functions STRIPC,EGETWI,and EGETWR
      CHARACTER OUTSTR*124,word*124

c Logical variable associated with version 1.3 of file hvacinput.F
      LOGICAL version1_3_flag

c Other local variables
      INTEGER num_out_air_day_types(max_sys),
     &        itotal_num_day_periods(i),
     &        num_outside_air_day_periods(max_sys,10),
     &        iend_hour_out_air_daytype(max_sys,8760),
     &        iend_hour_out_air_dayperiod(max_sys,100),
     &        num_heatpump_var,icounter,iperiod_counter,
     &        jcounter,idaytype_start_hour,ihour_year,
     &        ihour_day,iperiod_day_counter,
     &        iperiod_year_counter

      REAL day_period_out_air_flow(max_sys,100),
     &     SYS_FLOW_RATE,FAN_SIZE

      IUNIT=IFIL+20

      CALL STRIPC(IUNIT,OUTSTR,99,ND,1,'hvac input',IER)
      k = 0

c Number of variables on line 2 of the data associated with heat pump
c systems for version 1.3 of file hvacinput.F
      num_heatpump_var = 2 + 2 * num_sys_zones(i)
      if(num_heatpump_var.eq.ND) then
        version1_3_flag = .true.
      elseif(num_heatpump_var.ne.ND) then
        version1_3_flag = .false.
      endif

c In case we have input data associated with version 1.3 of file
c hvacinput
      if(version1_3_flag) then     
c Read heat pump function.
c	=1 heating
c	=2 cooling
        CALL EGETWI(OUTSTR,K,ival,1,2,'F',
     &  'hvac input: iunit_function',IER)
        iunit_function(i) = ival

c Read the type of air-source heat pump considered isys_type(i)
c	=1 Typical unit
c	=2 Good unit
c	=3 Poor unit
C GSHP: begin
C As a GSHP system will have a different part load ratio than these typical
C systems, a new system type is defined for GSHP systems.
C       =4 GSHP system
c GSHP: end
C GCEP: begin
C GCEP systems will also use a value of isys_type(i)=4
C GCEP: end

        CALL EGETWI(OUTSTR,K,ival,1,4,'F',
     &  'hvac input: isys_type',IER)
        isys_type(i) = ival

c GSHP & GCEP: begin
c If a GSH or a GCEP P system is being modelled, the correlation coefficients will
c be determined in a slightly different way than for ASHP systems. 
c GSHP coefficients will be based on entering water temperature (EWT) and as 
c such will vary every day of the simulation. For that reason, they are 
c calculated within the time-step simulation, instead of here. 
c The following initializes the correlation coefficients to zero. 
         if(isys_type(i).eq. 4) then
           ahp(i) = 0.0
           bhp(i) = 0.0
           chp(i) = 0.0
           dhp(i) = 0.0
         else
c GSHP & GCEP: end

c Set the correlation coefficients for the part-load performance
c depending on the type of air-source heat pump unit. This is
c done by calling the subroutine ASHP_COEFF. The correlation 
c coefficients are stored in common ashp_corr
           call ASHP_COEFF(i)
         end if

c Read the actual zone numbers associated with the HVAC system 
c isys_zone_num and the capacity fraction delivered to each zone 
c isys_zone_cap_frac. The next loop is repeated as many times as 
c there are zones served by the HVAC system
        do 30 j = 1,num_sys_zones(i)
c Read numbers j for zones served by hvac system i
          CALL EGETWI(OUTSTR,K,ival,1,100,'F',
     &'hvac input: isys_zone_num',IER)
          isys_zone_num(i,j) = ival
c Read capacity fraction delivered to each of the zones served by system
          CALL EGETWR(OUTSTR,K,val,0.,1.,'F',
     &'hvac input: sys_zone_cap_frac',IER)
          sys_zone_cap_frac(i,j) = val
  30    continue

        CALL STRIPC(IUNIT,OUTSTR,99,ND,1,'hvac input',IER)
        k = 0

c Read the steady-state capacity of the heat pump at rating conditions
c ss_capacity. This capacity is needed to find the available capacity of the
c heat pump at conditions other than the rating conditions. The capacity does
c not include contribution of indoor circulation fan power fan_power(i).
        CALL EGETWR(OUTSTR,K,val,0.,1e6,'F',
     &  'hvac input: ss_capacity',IER)
        ss_capacity(i) = val
c Read the steady-state C.O.P. of the heat pump ss_cop(i). COP does not 
c include the effect of indoor circulation fan power fan_power(i).
        CALL EGETWR(OUTSTR,K,val,0.,1000.,'F',
     &  'hvac input: ss_cop',IER)
        ss_cop(i) = val 

        CALL STRIPC(IUNIT,OUTSTR,99,ND,1,'hvac input',IER)
        k = 0

c Read the actual flow rate through the indoor circulating fan of 
c the system. This is the flow rate that occurs during the actual 
c operation of the heat pump system.
        CALL EGETWR(OUTSTR,K,val,-10.,10.,'F',
     &  'hvac input: flow_rate',IER)
        flow_rate(i) = val
c If the flow rate is less than zero, need to estimate one
        if(flow_rate(i).lt.0) then
          flow_rate(i) = SYS_FLOW_RATE(i)
        endif
c Read the flow rate through the indoor circulating fan of 
c the system at rating conditions.
        CALL EGETWR(OUTSTR,K,val,-10.,10.,'F',
     &  'hvac input: flow_rate_r',IER)
        flow_rate_r(i) = val
c If the flow rate is less than zero, then estimate one
        if(flow_rate_r(i).lt.0) then
          flow_rate_r(i) = SYS_FLOW_RATE(i)
        endif      
c Read the circulation fan operation status ifan_operation. The
c fan operation is the same as that for the backup system except 
c when the backup is baseboard/hydronic/plenum heater. In this case
c the backup can operate with no fan when the heat pump has a fan
c	=0 no fan associated with hvac system
c	=1 fan in auto mode
c       =2 fan in continous mode
        CALL EGETWI(OUTSTR,K,ival,0,2,'F',
     &  'hvac input: ifan_operation',IER)
        ifan_operation(i) = ival
c Position of circulation fan with respect to heat exchanger (heating
c or cooling)
c       =1 blow through
c       =2 draw through
        CALL EGETWI(OUTSTR,K,ival,1,2,'F',
     &  'hvac input: ifan_position',IER)
        ifan_position(i) = ival
c Read the indoor fan power. The power applies for the 
c operation of the heat pump and the backup system. Input
c the same power here as in the case of the backup system.
c If this value is less than 0, it will be estimated internally
c in subroutine FAN_SIZE at the end of subroutine HVACINPUT.
        CALL EGETWR(OUTSTR,K,val,-10.,1.e4,'F',
     &  'hvac input: fan_power',IER)
        fan_power(i) = val
c Read outdoor fan power
        CALL EGETWR(OUTSTR,K,val,-10.,1.e4,'F',
     &  'hvac input: outdoor_fan_power',IER)
        outdoor_fan_power(i) = val     
c There are systems with two fan speeds: one continuous and the other
c in auto mode. Read here the portion of the total fan power fan_power
c that is in auto mode with the system. This is the fan power that will 
c be used to find the part-load ratio when the circulation fan is 
c specified to be in the auto mode. If this is set to a value < 0, it is
c set equal to fan_power at the end of subroutine HVACINPUT.
        CALL EGETWR(OUTSTR,K,val,-10.,1.e4,'F',
     &  'hvac input: fan_power',IER)
        fan_power_auto(i) = val
c Position of circulation fan with respect to heat exchanger (heating
c or cooling) during rating test
c       =1 blow through
c       =2 draw through
        CALL EGETWI(OUTSTR,K,ival,1,2,'F',
     &  'hvac input: ifan_position_r',IER)
        ifan_position_r(i) = ival
c Read the indoor fan power during rating test. If this value is less
c than 0, it is estimated internally in FAN_SIZE
        CALL EGETWR(OUTSTR,K,val,-10.,1.e4,'F',
     &  'hvac input: fan_power_r',IER)
        fan_power_r(i) = val
c If indoor fan power during rating test is less than 0, it is 
c estimated internally in FAN_SIZE
        if(fan_power_r(i).lt.0.) then
          fan_power_r(i) = FAN_SIZE(i)
        endif

c Read different variables depending on the function of the 
c unit
        CALL STRIPC(IUNIT,OUTSTR,99,ND,1,'hvac input',IER)
        k = 0

c When the unit is in the heating mode
        if(iunit_function(i).eq.1) then
c Read the type of temperature control in the heating mode 
c (balance point; restricted; unrestricted)
c	=1 balance point control
c	=2 restricted control
c	=3 unrestricted control
          CALL EGETWI(OUTSTR,K,ival,1,3,'F',
     &    'hvac input: itemp_control',IER)
          itemp_control(i) = ival
c Read the cut-off temperature. This is only needed when the temperature
c control option is restricted
          CALL EGETWR(OUTSTR,K,val,-15.,50.,'F',
     &    'hvac input: cutoff_temp',IER)
          cutoff_temp(i) = val
c Read the backup heat source
c	=1 Furnace
c	=2 Boiler
c	=3 Baseboards/Hydronic/Plenum Heaters
          CALL EGETWI(OUTSTR,K,ival,1,3,'F',
     &    'hvac input: ibackup_heat',IER)
          ibackup_heat(i) = ival
c Read the hvac system number associated with the backup heat source.
c Data for backup system has to be read in before the heat pump data
c so that the sizing of the circulation fan is done properly
          CALL EGETWI(OUTSTR,K,ival,1,10,'F',
     &    'hvac input: ibackup_sys_num',IER)
          ibackup_sys_num(i) = ival

c When the unit is used in the cooling mode
        elseif(iunit_function(i).eq.2) then
c Read Sensible Heat Factor in the cooling mode at ARI rating conditions.
          CALL EGETWR(OUTSTR,K,val,0.,1.,'F',
     &    'hvac input: shrr',IER)
          shrr(i) = val   
c Read the cooling type (conventional, economizer, ventilation)
c	=1 conventional
c	=2 economizer
          CALL EGETWI(OUTSTR,K,ival,1,2,'F',
     &  'hvac input: icooling_type',IER)
          icooling_type(i) = ival

          CALL STRIPC(IUNIT,OUTSTR,99,ND,1,'hvac input',IER)
          k = 0

c For the case when cooling type is conventional, get the number
c of day types in the year with different outdoor air schedules. 
c Day types are specified from the beginning of year toward the end. 
c Each of these day types can have several periods with different
c outdoor air schedules.
          if(icooling_type(i).eq.1) then
            CALL EGETWI(OUTSTR,K,ival,1,10,'F',
     &      'hvac input: num_out_air_day_types',IER)
            num_out_air_day_types(i) = ival
  
            CALL STRIPC(IUNIT,OUTSTR,99,ND,1,'hvac input',IER)
            k = 0

c For each day type of the year, read the number of periods in the day type
c and the end hour (from 8760 hours in the year) for the day type. 
c Information about the number of periods within each day type is kept in a 
c two dimensional array num_outside_air_day_periods(i,icounter). The first
c index is the system number and the second is a number designating the day 
c type among all the day types of the year. The total number of day types in 
c the year is contained in num_out_air_day_types(i) read above.
            itotal_num_day_periods(i) = 0
            do 20 icounter = 1,num_out_air_day_types(i)
              CALL EGETWI(OUTSTR,K,ival,1,10,'F',
     &        'hvac input: num_outside_air_day_periods',IER)
              num_outside_air_day_periods(i,icounter) = ival
              itotal_num_day_periods(i) = itotal_num_day_periods(i) + 
     &        ival
              CALL EGETWI(OUTSTR,K,ival,1,8760,'F',
     &        'hvac input: iend_hour_out_air_daytype',IER)
              iend_hour_out_air_daytype(i,icounter) = ival
  20        continue

c For each day type, read the end hour and the flow in m3/s for each of the
c day periods
            iperiod_counter = 0
            do 300 icounter = 1,num_out_air_day_types(i)

              CALL STRIPC(IUNIT,OUTSTR,99,ND,1,'hvac input',IER)
              k = 0

              do 40 jcounter = 1,num_outside_air_day_periods(i,icounter)
                iperiod_counter = iperiod_counter + 1
                CALL EGETWI(OUTSTR,K,ival,1,24,'F',
     &          'hvac input: iend_hour_out_air_dayperiod',IER)
                iend_hour_out_air_dayperiod(i,iperiod_counter) = ival
                CALL EGETWR(OUTSTR,K,val,0.,10.,'F',
     &          'hvac input: day_period_out_air_flow',IER)
                day_period_out_air_flow(i,iperiod_counter) = val
  40          continue

  300       continue

c Array outdoor_air_flow(i,8760) holds the outside air intake through the
c forced air system associated with the air-conditioner for each hour of the
c year.
            idaytype_start_hour = 1
            ihour_year = 1
            ihour_day = 1
            iperiod_day_counter = 1
            iperiod_year_counter = 1
            do 50 icounter = 1,num_out_air_day_types(i)

              do 60 jcounter = idaytype_start_hour,
     &                        iend_hour_out_air_daytype(i,icounter)
                outdoor_air_flow(i,jcounter) = 
     &                day_period_out_air_flow(i,iperiod_year_counter)

                if((iend_hour_out_air_dayperiod(i,iperiod_year_counter)
     &          .eq.ihour_day).and.(ihour_day.lt.24).and.
     &          (ihour_year.ne.
     &          iend_hour_out_air_daytype(i,icounter))) then
                  iperiod_year_counter = iperiod_year_counter + 1
                  iperiod_day_counter = iperiod_day_counter + 1
                endif

                if((ihour_day.eq.24).and.(ihour_year.ne.
     &          iend_hour_out_air_daytype(i,icounter))) then
                  iperiod_year_counter = iperiod_year_counter -
     &                                     iperiod_day_counter + 1
                  iperiod_day_counter = 1
                  ihour_day = 0
                endif                  
   
                if(iend_hour_out_air_daytype(i,icounter).eq.
     &          ihour_year) then
                  idaytype_start_hour = 
     &                  iend_hour_out_air_daytype(i,icounter) + 1
                  iperiod_day_counter = 1
                  iperiod_year_counter = iperiod_year_counter + 1
                  ihour_day = 0
                endif

                ihour_day = ihour_day + 1
                ihour_year = ihour_year + 1

  60          continue

  50        continue

c For the case when cooling type is economizer control
          elseif(icooling_type(i).eq.2) then
c Read type of economizer control ieconomizer_control
c = 1 Indoor T/Outdoor T Integrated Compressor Control and option of
c     outdoor dry bulb limit control
c = 2 Indoor T/Outdoor T Non-Integrated Compressor Control and option of
c     outdoor dry bulb limit control
c = 3 Enthalpy Integrated Compressor Control with enthalpy limit control
c     option
c = 4 Enthalpy Non Integrated Compressor Control with enthalpy limit 
c     control option
            CALL EGETWI(OUTSTR,K,ival,1,4,'F',
     &      'hvac input: ieconomizer_control',IER)
            ieconomizer_control(i) = ival
            if((ieconomizer_control(i).eq.1).or.
     &         (ieconomizer_control(i).eq.2)) then
c Read the minimum outside air flow rate through the economizer
              CALL EGETWR(OUTSTR,K,val,0.,10.,'F',
     &        'hvac input: economizer_min_out_air',IER)
              economizer_min_out_air_flow(i) = val
c Indoor temperature set point associated with operation of economizer
              CALL EGETWR(OUTSTR,K,val,0.,100.,'F',
     &        'hvac input: economizer_ind_temp_setpoint',IER)
              economizer_ind_temp_setpoint(i) = val
c Outdoor dry bulb temperature for limit control
              CALL EGETWR(OUTSTR,K,val,0.,100.,'F',
     &        'hvac input: economizer_out_temp_limitcont',IER)
              economizer_out_temp_limitcon(i) = val
            elseif((ieconomizer_control(i).eq.3).or.
     &             (ieconomizer_control(i).eq.4)) then
c Read the minimum outside air flow rate through the economizer
              CALL EGETWR(OUTSTR,K,val,0.,10.,'F',
     &        'hvac input: economizer_min_out_air',IER)
              economizer_min_out_air_flow(i) = val
c Indoor temperature set point associated with operation of economizer
              CALL EGETWR(OUTSTR,K,val,0.,100.,'F',
     &        'hvac input: economizer_ind_temp_setpoint',IER)
              economizer_ind_temp_setpoint(i) = val
c Outdoor air enthalpy for limit control
              CALL EGETWR(OUTSTR,K,val,0.,100.,'F',
     &        'hvac input: economizer_out_enth_limitcont',IER)
              economizer_out_enth_limitcon(i) = val
            endif

c Initialize outdoor air flow array to zero
            do 400 icounter = 1,8760
              outdoor_air_flow(i,icounter) = 0.
  400       continue

          endif

          CALL STRIPC(IUNIT,OUTSTR,99,ND,1,'hvac input',IER)
          k = 0

c Read number associated with heat pump in heating mode
          CALL EGETWI(OUTSTR,K,ival,1,100,'F',
     &    'hvac input: isys_num_heating',IER)
          isys_num_heating(i) = ival
c Read control function number associated with hvac system
          CALL EGETWI(OUTSTR,K,ival,1,max_cont_fun,'F',
     &    'hvac input: icontrol_function',IER)
          icontrol_function(i) = ival

c For a heat pump in the cooling mode, it is possible using the 
c information at rating conditions to determine the bypass factor
c of the unit at these rating conditions. This will be used in
c subroutine ASHP_COOLING to estimate the energy consumption of unit.
          call BYPASS_FACTOR_R(i)
        endif
      else

c Call cubroutine to read heat pump input data for versions before 1.3
        call ASHP_INPUT_VERSION1_2(i,OUTSTR)

        do 500 icounter = 1,8760
          outdoor_air_flow(i,icounter) = 0.
  500   continue
        
      endif
      
      return

      end

c *********************************************************************
c *********************************************************************
c ASHP_INPUT_VERSION1_2

c Created by: Kamel Haddad
c Initial Creation Date: December 12th 2000
c Copyright 2000: NRCan Buildings Group
c Updated: Julia Purdy, April 3, 2002.

c Subroutine called by ASHP_INPUT to read data from hvac input
C file for air-source heat pump and ground source heat pump system. This
c routine is called to read input data for version earlier than 1.3 of file
c hvacinput.F

c INPUTS:
c       i 		system number associated with air-source heat
c 			pump system
c       OUTSTR          data string read in subroutine ASHP_INPUT
c       hvac.h          include file for hvac parameters
c       building.h      include file for building parameters

c OUTPUTS:
c	HVAC_INPUT_DATA
c			common for HVAC input data
c	HEAT_PUMP_INPUT_DATA
c			common for heat pump input data
c *********************************************************************
      SUBROUTINE ASHP_INPUT_VERSION1_2(i,OUTSTR)

      IMPLICIT NONE

c Variable passed in call
      INTEGER i

c Define the characters variables associated with the utility 
c functions STRIPC,EGETWI,and EGETWR
      character OUTSTR*124,word*124
#include "building.h"
#include "hvac_parameters.h"

#include "hvac_common.h"

#include "ashp_common.h"


      COMMON/FILEP/IFIL
      COMMON/ER/IER
      COMMON/OUTIN/IUOUT,IUIN

c Declare local variables
      INTEGER IUNIT,IFIL,ND,IER,k,ival,j,IUOUT,IUIN
      REAL val

c Other local variables
      REAL SYS_FLOW_RATE,FAN_SIZE,dummy
      IUNIT=IFIL+20

      k = 0

c Read heat pump function.
c	=1 heating
c	=2 cooling
      CALL EGETWI(OUTSTR,K,ival,1,2,'F',
     &'hvac input: iunit_function',IER)
      iunit_function(i) = ival

c Read the type of air-source heat pump considered isys_type(i)
c	=1 Typical unit
c	=2 Good unit
c	=3 Poor unit
C GSHP: begin
C As a GSHP system will have a different part load ratio than these typical
C systems, a new system type is defined for GSHP systems.
C       =4 GSHP system
c GSHP: end
C GCEP: begin
C GCEP systems will also use a value of isys_type(i)=4
C GCEP: end

      CALL EGETWI(OUTSTR,K,ival,1,4,'F',
     &'hvac input: isys_type',IER)
      isys_type(i) = ival

c GSHP & GCEP: begin
c If a GSH or a GCEP P system is being modelled, the correlation coefficients will
c be determined in a slightly different way than for ASHP systems. 
c GSHP coefficients will be based on entering water temperature (EWT) and as 
c such will vary every day of the simulation. For that reason, they are 
c calculated within the time-step simulation, instead of here. 
c The following initializes the correlation coefficients to zero. 
       if(isys_type(i).eq. 4) then
         ahp(i) = 0.0
         bhp(i) = 0.0
         chp(i) = 0.0
         dhp(i) = 0.0
       else
c GSHP & GCEP: end

c Set the correlation coefficients for the part-load performance
c depending on the type of air-source heat pump unit. This is
c done by calling the subroutine ASHP_COEFF. The correlation 
c coefficients are stored in common ashp_corr
       call ASHP_COEFF(i)
       end if

c Read the actual zone numbers associated with the HVAC system 
c isys_zone_num and the capacity fraction delivered to each zone 
c isys_zone_cap_frac. The next loop is repeated as many times as 
c there are zones served by the HVAC system
      do 30 j = 1,num_sys_zones(i)
c Read numbers j for zones served by hvac system i
        CALL EGETWI(OUTSTR,K,ival,1,100,'F',
     &'hvac input: isys_zone_num',IER)
        isys_zone_num(i,j) = ival
c Read capacity fraction delivered to each of the zones served by system
        CALL EGETWR(OUTSTR,K,val,0.,1.,'F',
     &'hvac input: sys_zone_cap_frac',IER)
        sys_zone_cap_frac(i,j) = val
  30  continue

c Read the steady-state capacity of the heat pump at rating conditions
c ss_capacity. This capacity is needed to find the available capacity of the
c heat pump at conditions other than the rating conditions.
      CALL EGETWR(OUTSTR,K,val,0.,1e6,'F',
     &'hvac input: ss_capacity',IER)
      ss_capacity(i) = val
c Read the steady-state C.O.P. of the heat pump ss_cop(i).
      CALL EGETWR(OUTSTR,K,val,0.,1000.,'F',
     &'hvac input: ss_cop',IER)
      ss_cop(i) = val 
c Read the actual flow rate through the indoor circulating fan of 
c the system. This is the flow rate that occurs during the actual 
c operation of the heat pump system.
      CALL EGETWR(OUTSTR,K,val,-10.,10.,'F',
     &'hvac input: flow_rate',IER)
      flow_rate(i) = val
c If the flow rate is less than zero, need to estimate one
      if(flow_rate(i).lt.0) then
        flow_rate(i) = SYS_FLOW_RATE(i)
      endif
c Read the flow rate through the indoor circulating fan of 
c the system at rating conditions.
      CALL EGETWR(OUTSTR,K,val,-10.,10.,'F',
     &'hvac input: flow_rate_r',IER)
      flow_rate_r(i) = val
c If the flow rate is less than zero, then estimate one
      if(flow_rate_r(i).lt.0) then
        flow_rate_r(i) = SYS_FLOW_RATE(i)
      endif      
c Read the circulation fan operation status ifan_operation. The
c fan operation is the same as that for the backup system except 
c when the backup is baseboard/hydronic/plenum heater. In this case
c the backup can operate with no fan when the heat pump has a fan
c	=0 no fan associated with hvac system
c	=1 fan in auto mode
c       =2 fan in continous mode
      CALL EGETWI(OUTSTR,K,ival,0,2,'F',
     &'hvac input: ifan_operation',IER)
      ifan_operation(i) = ival
c Read the indoor fan power. The power applies for the 
c operation of the heat pump and the backup system. Input
c the same power here as in the case of the backup system
      CALL EGETWR(OUTSTR,K,val,-10.,1.e4,'F',
     &'hvac input: fan_power',IER)
      fan_power(i) = val

c Read different variables depending on the function of the 
c unit
      CALL STRIPC(IUNIT,OUTSTR,99,ND,1,'hvac input',IER)
      k = 0

c When the unit is in the heating mode
      if(iunit_function(i).eq.1) then
c Read the type of temperature control in the heating mode 
c (balance point; restricted; unrestricted)
c	=1 balance point control
c	=2 restricted control
c	=3 unrestricted control
        CALL EGETWI(OUTSTR,K,ival,1,3,'F',
     &  'hvac input: itemp_control',IER)
        itemp_control(i) = ival
c Read the cut-off temperature. This is only needed when the temperature
c control option is restricted
        CALL EGETWR(OUTSTR,K,val,-15.,50.,'F',
     &  'hvac input: cutoff_temp',IER)
        cutoff_temp(i) = val
c Read the backup heat source
c	=1 Furnace
c	=2 Boiler
c	=3 Baseboards/Hydronic/Plenum Heaters
        CALL EGETWI(OUTSTR,K,ival,1,3,'F',
     &  'hvac input: ibackup_heat',IER)
        ibackup_heat(i) = ival
c Read the hvac system number associated with the backup heat source.
c Data for backup system has to be read in before the heat pump data
c so that the sizing of the circulation fan is done properly
        CALL EGETWI(OUTSTR,K,ival,1,10,'F',
     &  'hvac input: ibackup_sys_num',IER)
        ibackup_sys_num(i) = ival
c When the unit is used in the cooling mode
      elseif(iunit_function(i).eq.2) then
c Read Sensible Heat Factor in the cooling mode at ARI rating conditions.
        CALL EGETWR(OUTSTR,K,val,0.,1.,'F',
     &  'hvac input: shrr',IER)
        shrr(i) = val   
c Read the cooling type (conventional, economizer, ventilation)
c	=1 conventional
c	=2 economizer
c	=3 ventilation
        CALL EGETWI(OUTSTR,K,ival,1,3,'F',
     &  'hvac input: icooling_type',IER)
        icooling_type(i) = ival
c Read ventilation flow rate
        CALL EGETWR(OUTSTR,K,val,0.,10.,'F',
     &  'hvac input: vent_flow_rate',IER)
        dummy = val   
c Read control option for economizer
c	=1 temperature control
c	=2 enthalpy control
        CALL EGETWI(OUTSTR,K,ival,1,2,'F',
     &  'hvac input: ieconomizer_type',IER)
        dummy = ival
c Read openable window area for when heat pump is in cooling mode
        CALL EGETWR(OUTSTR,K,val,0.,1.,'F',
     &  'hvac input: open_win_area',IER)
        dummy = val
c Read number associated with heat pump in heating mode
        CALL EGETWI(OUTSTR,K,ival,1,100,'F',
     &  'hvac input: isys_num_heating',IER)
        isys_num_heating(i) = ival
c Read control function number associated with hvac system
        CALL EGETWI(OUTSTR,K,ival,1,max_cont_fun,'F',
     &  'hvac input: icontrol_function',IER)
        icontrol_function(i) = ival

c Set other variables that were defined starting with version 1.3
        ifan_position(i) = 1
        outdoor_fan_power(i) = 0.03 * ss_capacity(i)
        fan_power_auto(i) = fan_power(i)
        ifan_position_r(i) = 1
        fan_power_r(i) = FAN_SIZE(i)

c For a heat pump in the cooling mode, it is possible using the 
c information at rating conditions to determine the bypass factor
c of the unit at these rating conditions. This will be used in
c subroutine ASHP_COOLING to estimate the energy consumption of unit.
        call BYPASS_FACTOR_R(i)
      endif

      return
      end

c *********************************************************************
c *********************************************************************
c BASEBOARD_INPUT

c Created by: Kamel Haddad
c Initial Creation Date: December 12th 2000
c Copyright 2000: NRCan Buildings Group

c Subroutine called by HVACINPUT to read data from hvac input file for
c a baseboard/hydronic/plenum heater system

c INPUTS:
c       i 		system number associated with baseboard system
c       hvac.h          include file for hvac parameters

c OUTPUTS:
c	HVAC_INPUT_DATA
c			common for hvac input data
c	FURNACE_INPUT_DATA
c			common for furnace input data
c *********************************************************************
      SUBROUTINE BASEBOARD_INPUT(i)

      IMPLICIT NONE

c Variables in call
      INTEGER i

#include "building.h"

#include "hvac_parameters.h"

#include "hvac_common.h"

#include "furnace_common.h"

      COMMON/FILEP/IFIL
      COMMON/ER/IER
      COMMON/OUTIN/IUOUT,IUIN

c Declare local variables
      INTEGER IUNIT,IFIL,ND,IER,k,ival,j,IUOUT,IUIN
      REAL val

c Define the characters variables associated with the utility 
c functions STRIPC,EGETWI,and EGETWR
      character OUTSTR*124,word*124

      IUNIT=IFIL+20

      CALL STRIPC(IUNIT,OUTSTR,99,ND,1,'hvac input',IER)
      k = 0

c Read the actual zone numbers associated with the HVAC system 
c isys_zone_num. The next loop is repeated as many times as 
c there are zones served by the HVAC system
      do 10 j = 1,num_sys_zones(i)
c Read numbers for zones served by hvac system i
        CALL EGETWI(OUTSTR,K,ival,1,100,'F',
     &'hvac input: isys_zone_num',IER)
        isys_zone_num(i,j) = ival
c Read capacity fraction delivered to each of the zones served by system
        CALL EGETWR(OUTSTR,K,val,0.,1.,'F',
     &'hvac input: sys_zone_cap_frac',IER)
        sys_zone_cap_frac(i,j) = val
  10  continue
c Read steady-state capacity
      CALL EGETWR(OUTSTR,K,val,0.,1e6,'F',
     &'hvac input: ss_capacity',IER)
      ss_capacity(i) = val
c Read steady-state efficiency
      CALL EGETWR(OUTSTR,K,val,0.,1.,'F',
     &'hvac input: ss_efficiency',IER)
      ss_efficiency(i) = val
c Read the circulation fan operation status ifan_operation. If the
c system is a backup, this is the same as that for the primary system
c except when the primary system has a fan and the baseboard has no fan.
c In this case, ifan_operation can be 0 here and 1 or 2 for the primary
c system
c	=0 no fan associated with hvac system
c	=1 fan in auto mode
c       =2 fan in continous mode
      CALL EGETWI(OUTSTR,K,ival,0,2,'F',
     &'hvac input: ifan_operation',IER)
      ifan_operation(i) = ival
c Read the indoor fan power. If this system is a backup, enter same number
c here as that for primary system
      CALL EGETWR(OUTSTR,K,val,-10.,1.e4,'F',
     &'hvac input: fan_power',IER)
      fan_power(i) = val

      return
      end      
