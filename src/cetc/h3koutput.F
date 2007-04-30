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

c     The file contains the following routines:
c     SUM_HVAC_ENERGY_USED_MONTHLY 	Get/Stores Monthly energy values
c     OUTPUT_HVAC_ENERGY_USED_MONTHLY	Outputs Monthly energy values
c     H3KOUTPUT_SUM_PLR_BIN_HOURS	Get/Stores PLR Bin values
c     ZERO_H3KOUTPUT_DATA		Zeros output variables
c     GETDAY				Get the day of the month
c     GETMONTH			Get the Month number
c     OUTPUT_HVAC_PLR_PERCENT		Output PLR Bin Values
c     H3KOUTPUT			Main Output Routine.


c     *********************************************************************
c     H3KOUTPUT_MAIN
c     Output Monthly Energy Used by HVAC systems
c     Created by: Phylroy A. Lopez
c     Initial Creation Date:  March 23th 2001
c     Copyright 2000: NRCan Buildings Group
      
c     INPUTS:
c     IWU		Output File Number
c     OUTPUTS:
c
c     COMMENTS:
C     This is the main subroutine of the H3k Output. Iterates through systems  
      
      SUBROUTINE H3KOUTPUT_MAIN(IWU)
#include "building.h"
#include "esprdbfile.h"
#include "hvac_parameters.h"
#include "hvac_common.h"
#include "h3kstore_parameters.h"
#include "h3kstore_common.h"
#include "dhw_parameters.h"
#include "dhw_common.h"

      common/bsmtdef/iBSIMP(MCOM),LBSIMP(MCOM)
c     iBSIMP is used to determine if a zone is a Basement iBSIMP=1
      COMMON/C1/NCOMP,NCON
c     NCOMP is used for the number of zone present in the simulation.
      
      COMMON/PERS/ISD1,ISM1,ISD2,ISM2,ISDS,ISDF,NTSTEP
c     NTSTEP is used to norlamlise the values to one hour.
      
      COMMON/SET1/IYEAR,IBDOY,IEDOY,IFDAY,IFTIME
      COMMON/precz/zname(MCOM),zdesc(MCOM) 
c     zname(mcom) is zone name string.
      
      COMMON/RESLIB/RFILE,PFILE,MSTRFILE,LAFRES
c     RFILE is the the results library filename.
      
      common/C21/IFCFG,cfgroot,LCFGF
      common/c2/lsnam,nccode(mcom),lproj(mcom),lgeom(mcom),
     &     lshad(mcom),lthrm(mcom),indutl(mcom),lutil(mcom)
c     LCFGF is the configuration file used.
c     lsnam is the description of the building.
      character*72 LBSIMP
      character LTMP*72,h*72,zname*12,zdesc*64,outs*124
      character PFILE*72,MSTRFILE*72,LAFRES*72
      character cfgroot*24,tab*1
      character*72 lsnam,lproj,lgeom
      character*72 lshad,lthrm,lutil,lcfgf,rfile
      DIMENSION MTHNAM(12)
      CHARACTER DS*7,DS1*10,DE*7,DE1*10,MTHNAM*3
      logical OK
      DATA MTHNAM/'Jan','Feb','Mar','Apr','May','Jun','Jul','Aug',
     &     'Sep','Oct','Nov','Dec'/
      
      CALL EFDELET(101,ISTAT) !DELETES DECEMBER file from progress subroutine.
      write(outs,'(a)')' '
      call edisp(IWU,outs)      
      write(outs,'(a)')'********BUILDING INFORMATION********'
      call edisp(IWU,outs)
      write(outs,'(a)')' '
      call edisp(IWU,outs)
      
c     call aim2 output subroutine
      CALL H3KOUTPUT_AIMSDATA(IWU)
      write(outs,'(a)')' '
      call edisp(IWU,outs)
      write(outs,'(a)')'********SYSTEMS INFORMATION*********'
      call edisp(IWU,outs)
      write(outs,'(a)')' '
      call edisp(IWU,outs)
      call H3KOUTPUT_HVAC_Fan_HRV_Monthly(IWU)
      write(outs,'(a)')' '
      call edisp(IWU,outs)
c     iterate through all systems
      DO 45 isystem = 1,num_hvac_sys
         
c     outputs system data depending on system type (ashp, furnace,etc)
         CALL H3KOUTPUT_SYSTEM_DATA(isystem,IWU)
         
c     outputs the system energy use
         CALL H3KOUTPUT_HVAC_ENERGY_MONTHLY(isystem,IWU)
         
c     outputs system capacity reports
         CALL H3KOUTPUT_HVAC_PLR_PERCENT(isystem,IWU)
 45   continue 
      
      
      write(outs,'(a)')' '
      call edisp(IWU,outs)       
      write(outs,'(a)')'**********ZONE INFORMATION*********'
      call edisp(IWU,outs)
      write(outs,'(a)')' '
      call edisp(IWU,outs)
      DO 46 izone = 1,NCOMP
c     iterates through zones
         call H3KOUTPUT_Solar_Casual_Found(izone,IWU)
 46   CONTINUE
      CALL H3KOUTPUT_DHW(IWU)
      CALL H3KOUTPUT_FC(IWU)
      RETURN
      END  

C     *********************************************************************
C     H3KOUTPUT_SYSTEM_DATA
C     OUTPUT MONTHLY ENERGY USED BY HVAC SYSTEMS
C     CREATED BY: PHYLROY A. LOPEZ
C     INITIAL CREATION DATE:  MARCH 23TH 2001
C     COPYRIGHT 2000: NRCAN BUILDINGS GROUP
      
C     INPUTS:
C     ISYSTEM SYSTEM NUMBER
C     IWU	OUTPUT FILE NUMBER
C     OUTPUTS:
C     OUTPUTS DATA TO FILE IWU
C     COMMENTS:
C     THIS ROUTINE WILL CHOSSE WITH SYSTEM DATA 
C     OUTPUT FUNCTION TO USE DEPENDING ON THE SYSTEM TYPE.
C     IF NOT KNOWN IT WILL FLAG IT AS AN UNKNOWN SYSTEM.
C     CURRENTLY FURNACE =1, BOILER = 2 BASEBOARD =3 AIRSOURCE HEAT PUMP = 7
C**********************************************************************

      SUBROUTINE H3KOUTPUT_SYSTEM_DATA(ISYSTEM,IWU)
      IMPLICIT NONE
#include "building.h"
#include "hvac_parameters.h"
#include "h3kstore_parameters.h"
#include "hvac_common.h"
#include "h3kstore_common.h"
      CHARACTER OUTS*124
      INTEGER ISYSTEM
      INTEGER IWU
C     DEFINING SYSTEM TYPE PARAMETER HERE. SHOULD BE DONE IN HVAC MODULE. WILL ADD WHEN COMPLETE.
      INTEGER    FURNACE
      PARAMETER ( Furnace = 1 )
      INTEGER    BOILER
      PARAMETER ( BOILER = 2 )
      INTEGER   BASEBOARD
      PARAMETER( BASEBOARD = 3 )
      INTEGER   ASHP
      PARAMETER ( ASHP = 7 )
	          
      WRITE(OUTS,'(A,4X,I3)')'#HVAC SYSTEM NUMBER = ',ISYSTEM
      CALL EDISP(IWU,OUTS)
      
      SELECT CASE (IHVAC_TYPE(ISYSTEM))
      CASE (FURNACE)
         CALL H3KOUTPUT_FURNACE_DATA(ISYSTEM,IWU)
      CASE (BOILER)
         CALL H3KOUTPUT_BOILER_DATA(ISYSTEM,IWU)
      CASE (BASEBOARD)
         CALL H3KOUTPUT_BASEBOARD_DATA(ISYSTEM,IWU)
      CASE (ASHP)
         CALL H3KOUTPUT_ASHP_DATA(ISYSTEM,IWU)
      CASE DEFAULT
         WRITE(OUTS,'(A)')'HVAC SYSTEM TYPE = UNKNOWN TYPE'
         CALL EDISP(IWU,OUTS)
      END SELECT 
      RETURN
      END
C     *********************************************************************
C     H3KOUTPUT_HVAC_Fan_MONTHLY
C     OUTPUT MONTHLY ENERGY USED BY HVAC SYSTEMS
C     CREATED BY: PHYLROY A. LOPEZ
C     INITIAL CREATION DATE:  MARCH 23TH 2001
C     COPYRIGHT 2000: NRCAN BUILDINGS GROUP
      
C     INPUTS:
C     SYSTEM   	HVAC SYSTEM USED
C     IWU		OUTPUT FILE NUMBER
      
C     OUTPUTS:

      SUBROUTINE H3KOUTPUT_HVAC_Fan_HRV_Monthly(IWU)
C      USE Ventilation_Data
      IMPLICIT NONE
#include "building.h"
#include "hvac_parameters.h"
#include "h3kstore_parameters.h"
#include "Ventilation_Data.h"
#include "h3kstore_common.h"
      COMMON/PERS/ISD1,ISM1,ISD2,ISM2,ISDS,ISDF,NTSTEP
      CHARACTER OUTS*124
      DIMENSION MTHNAM(12)
      INTEGER   ISD1,ISM1,ISD2,ISM2,ISDS,ISDF,NTSTEP
      INTEGER   IWU,IMONTH
      REAL   
     &     fFAN_ENERGY_TOTAL,
     &     fHRV_Elec_Energy_TOTAL,
     &     fGSHP_pump_energy_TOTAL,
     &     fGCEP_pump_energy_TOTAL  

      CHARACTER DS*7,DS1*10,DE*7,DE1*10,MTHNAM*3
      DATA MTHNAM/'JAN','FEB','MAR','APR','MAY','JUN','JUL','AUG',
     &     'SEP','OCT','NOV','DEC'/      
C     OUTPUT HEADERS
      WRITE (OUTS,'(A)')' ' 
      CALL EDISP(IWU,OUTS)
      WRITE (OUTS,'(A)')'FAN, HRV, AND PUMP ELECTRIC ENERGY ' 
      CALL EDISP(IWU,OUTS)
      WRITE(OUTS,'(A15,4X,A17,1x,a17,1x,a17,1x,a17)')
     &     'MONTH',
     &     'FAN_ENERGY MJ',
     &     'HRV ENERGY MJ',
     &     'GSHP_PUMP MJ',
     &     'GCEP_PUMP MJ'
      CALL EDISP(IWU,OUTS) 

C     LOOP THROUGH MONTH IN TIME PERIOD
      DO 20 IMONTH=ISM1,ISM2
C     OUTPUT ENERGIES  
         WRITE(OUTS,'(A20,4X,E17.4,1x,E17.4,1x,E17.4,1x,E17.4)')
     &        MTHNAM(IMONTH),
C     DIVIDING BY 1000000 TO CONVERT FROM J TO MJ 

     &        fFAN_ENERGY_MONTHLY(IMONTH)/1000000.0,
     &        fHRV_Elec_Energy(imonth)/1000000.0,
     &        fGSHP_pump_energy_monthly(imonth)/1000000.0,
     &        fGCEP_pump_energy_monthly(imonth)/1000000.0 
         CALL EDISP(IWU,OUTS)

C     STORE TOTALS  
         fFAN_ENERGY_TOTAL = fFAN_ENERGY_TOTAL 
     &        + fFAN_ENERGY_MONTHLY(IMONTH)/1000000.0

         fHRV_Elec_Energy_TOTAL = fHRV_Elec_Energy_TOTAL 
     &        + fHRV_Elec_Energy(imonth)/1000000.0

         fGSHP_pump_energy_TOTAL = fGSHP_pump_energy_TOTAL 
     &        + fGSHP_pump_energy_monthly(imonth)/1000000.0

         fGCEP_pump_energy_TOTAL = fGCEP_pump_energy_TOTAL
     &        + fGCEP_pump_energy_monthly(imonth)/1000000.0  

 20   CONTINUE
         WRITE(OUTS,'(A20,4X,E17.4,1x,E17.4,1x,E17.4,1x,E17.4)')
     &        "TOTAL ELEC ENERGY",
C     DIVIDING BY 1000000 TO CONVERT FROM J TO MJ 
     &     fFAN_ENERGY_TOTAL,
     &     fHRV_Elec_Energy_TOTAL,
     &     fGSHP_pump_energy_TOTAL,
     &     fGCEP_pump_energy_TOTAL 
         CALL EDISP(IWU,OUTS)
      END            
C     *********************************************************************
C     H3KOUTPUT_HVAC_ENERGY_MONTHLY
C     OUTPUT MONTHLY ENERGY USED BY HVAC SYSTEMS
C     CREATED BY: PHYLROY A. LOPEZ
C     INITIAL CREATION DATE:  MARCH 23TH 2001
C     COPYRIGHT 2000: NRCAN BUILDINGS GROUP
      
C     INPUTS:
C     SYSTEM   	HVAC SYSTEM USED
C     IWU		OUTPUT FILE NUMBER
      
C     OUTPUTS:
C     OUTPUT MONTHLY ENERGY VALUES TO FILE IWU
C     SAMPLE:
C     ENERGY CONSUMED BY SYSTEM
C     MONTH      HL_ENERGY KW/H         SYS ENERGY MJ    BACKSYS ENERGY MJ        FAN_ENERGY MJ        PIL_ENERGY MJ
C     JAN               3097.2              11435.2                  0.0                212.7                  0.0
C     FEB               2782.3              10273.7                  0.0                191.0                  0.0
C     MAR               1651.9               6175.9                  0.0                113.4                  0.0

      SUBROUTINE H3KOUTPUT_HVAC_ENERGY_MONTHLY(ISYSTEM,IWU)
#include "building.h"
#include "hvac_parameters.h"
#include "h3kstore_parameters.h"
#include "h3kstore_common.h"
      COMMON/PERS/ISD1,ISM1,ISD2,ISM2,ISDS,ISDF,NTSTEP
      CHARACTER OUTS*124
      DIMENSION MTHNAM(12)
      CHARACTER DS*7,DS1*10,DE*7,DE1*10,MTHNAM*3
      DATA MTHNAM/'JAN','FEB','MAR','APR','MAY','JUN','JUL','AUG',
     &     'SEP','OCT','NOV','DEC'/      
      REAL 
     &     fHeat_ENERGY_TOTAL,
     &     fCool_ENERGY_TOTAL,
     &     fPILOT_ENERGY_TOTAL 
      fHeat_ENERGY_TOTAL = 0.0
      fCool_ENERGY_TOTAL = 0.0
      fPILOT_ENERGY_TOTAL = 0.0
C     OUTPUT HEADERS
      WRITE (OUTS,'(A)')' ' 
      CALL EDISP(IWU,OUTS)
      WRITE (OUTS,'(A)')'ENERGY CONSUMED BY HVAC SYSTEM ' 
      CALL EDISP(IWU,OUTS)
      WRITE(OUTS,'(A15,3(1x,a17))')
     &     'MONTH',
     &     'HEATING ENERGY MJ',
     &     'COOLING ENERGY MJ',
     &     'PIL_ENERGY MJ'
      CALL EDISP(IWU,OUTS) 
C     LOOP THROUGH MONTH IN TIME PERIOD
      DO 21 IMONTH=ISM1,ISM2
C     OUTPUT ENERGIES  
         WRITE(OUTS,'(A15,3(1x,e17.4))')
     &        MTHNAM(IMONTH),
C     DIVIDING BY 1000000 TO CONVERT FROM J TO MJ 
     &        fHeat_ENERGY_MONTHLY(ISYSTEM,IMONTH)/1000000.0,
     &        fCool_ENERGY_MONTHLY(ISYSTEM,IMONTH)/1000000.0,
     &        fPILOT_ENERGY_MONTHLY(ISYSTEM,IMONTH)/1000000.0       
         CALL EDISP(IWU,OUTS)

         fHeat_ENERGY_TOTAL = fHeat_ENERGY_TOTAL 
     &        + fHeat_ENERGY_MONTHLY(ISYSTEM,IMONTH)/1000000.0
         fCool_ENERGY_TOTAL = fCool_ENERGY_TOTAL 
     &        + fCool_ENERGY_MONTHLY(ISYSTEM,IMONTH)/1000000.0
         fCool_ENERGY_TOTAL = fCool_ENERGY_TOTAL 
     &        + fPILOT_ENERGY_MONTHLY(ISYSTEM,IMONTH)/1000000.0
 21   CONTINUE
C     OUTPUT ENERGIES  
         WRITE(OUTS,'(A15,3(1x,e17.4))')
     &        "TOTAL SYS ENRGY",
C     DIVIDING BY 1000000 TO CONVERT FROM J TO MJ 
     &        fHeat_ENERGY_TOTAL,
     &        fCool_ENERGY_TOTAL,
     &        fPILOT_ENERGY_TOTAL       
         CALL EDISP(IWU,OUTS)
      END
      
C     *********************************************************************
C     H3KOUTPUT_SUM_PERCENT_CAP_DATA(ISYSTEM,PLR)
C     OUTPUT MONTHLY ENERGY USED BY HVAC SYSTEMS
C     CREATED BY: PHYLROY A. LOPEZ
C     INITIAL CREATION DATE:  MARCH 23TH 2001
C     COPYRIGHT 2000: NRCAN BUILDINGS GROUP
      
C     INPUTS:
C     ISYSTEM   	HVAC SYSTEM USED
C     PLR 		PART LOAD RATIO
      
C     OUTPUTS:
C     NONE.
C     COMMENTS:
C     IT STORES THE PART LOAD HOURS IN THE VARIABLES BINS FOR EXAMPLE 
C     IF IPERCENT_CAPACITY = 10(%) (SET IN H3KOUTPUT.H)
C     PLR OF 0-10% FOR SYSTEM #1 IS STORED IN SUM_PLR_CAPACITY_BIN(1,1)
C     PLR OF 10-20% FOR SYSTEM # 1 IS STORED IN SUM_PLR_CAPACITY_BIN(1,2)
C     THE TOTAL UNDERHEATED HOURS ARE STORED IN UNDERHEATED_HOURS(ISYSTEM)
C     THE TOTAL HOURS OF HEATING (OR COOLING) IS STORED IN TOTAL_NUM_OF_HOURS(ISYSTEM)
C     SAMPLE:
C     SYSTEM CAPACITY REPORT
C     %CAPACITY         HOURS      %OFTOTAL
C     0.0%- 10.0%         2012.00         46.99
C     10.0%- 20.0%         1120.00         26.16
C     20.0%- 30.0%          587.00         13.71
C************************************************************************
      SUBROUTINE H3KOUTPUT_HVAC_PLR_PERCENT(ISYSTEM,IWU)

#include "building.h"
#include "hvac_parameters.h"
#include "hvac_common.h"
#include "h3kstore_parameters.h"
#include "h3kstore_common.h"

      CHARACTER OUTS*124
      WRITE (OUTS,'(A)')' ' 
      CALL EDISP(IWU,OUTS)
      WRITE(OUTS,'(A)')'SYSTEM CAPACITY REPORT'
      CALL EDISP(IWU,OUTS)     
C     HEADERS
      WRITE(OUTS,'(A15,2(1X,A17))')
     &     '%CAPACITY',
     &     'HOURS',
     &     '%OFTOTAL'
      CALL EDISP(IWU,OUTS)
     
      IF (TOTAL_NUM_OF_HOURS(ISYSTEM).ne.0)then
         
C     LOOP FROM 1 TO IPERCENT_CAPACITY THIS VARIABLE IS TE INCREMENT AT WHICH 
C     THE 100% IS DIVIDED INTO CURRENTLY SET TO 10% INCREMENTS IN H3KOUTPUT.H     
         DO 100 IPERCENT = 1,IPERCENT_CAPACITY
C     SETS RANGE 0-10% , OR 10-20% ETC....DEPENDING ON IPERCENT_CAPACITY
C     WHERE PERCUPPER IS THE UPPER BOUND AND PERCLOWERE IS THE LOWER BOUND
            PERCLOWER = 100/IPERCENT_CAPACITY*(IPERCENT-1)
            PERCUPPER = 100/IPERCENT_CAPACITY*(IPERCENT)
C     OUTPUT DATA.  
            WRITE(OUTS,'(F5.1,A2,F5.1,A1,2x,2(1x,e17.4))')
C     OUTPUT RANGE
     &           (PERCLOWER),
     &           '%-',
     &           (PERCUPPER),
     &           '%',
C     OUTPUT NUMBER OF HOUR OPERATING AT THAT RANGE
     &           ((SUM_PLR_CAPACITY_BIN(ISYSTEM,IPERCENT))),
C     OUTPUT PERCENTAGE OF TOTAL HOURS OPERATING. 
     &           100.0*(SUM_PLR_CAPACITY_BIN(ISYSTEM,IPERCENT)
     &           *recip(real(TOTAL_NUM_OF_HOURS(ISYSTEM))))
            CALL EDISP(IWU,OUTS)
 100     CONTINUE
C     OUTPUT UNDERHEATED HOURS
         WRITE(OUTS,'(A15,2(1x,e17.4))')'UNDERHEATED HRS. ',
C     OUTPUT HOURS
     &        (FLOAT(IHRS_UNDER_HEAT(ISYSTEM))),
C     OUTPUT PERCENT HOURS
     &        100.0*(FLOAT(IHRS_UNDER_HEAT(ISYSTEM))
     &        * recip (real (TOTAL_NUM_OF_HOURS(ISYSTEM))))
         CALL EDISP(IWU,OUTS)
C     OUTPUT UNDERHCOOL HOURS
         WRITE(OUTS,'(A15,2(1x,e17.4))')'UNDERCOOLED HRS. ',
C     OUTPUT HOURS
     &        (FLOAT(IHRS_UNDER_COOL(ISYSTEM))),
C     OUTPUT PERCENT HOURS
     &        100.0*(FLOAT(IHRS_UNDER_COOL(ISYSTEM))
     &        * recip (real (TOTAL_NUM_OF_HOURS(ISYSTEM))))
         CALL EDISP(IWU,OUTS)
C     OUTPUT TOTAL HOURS
         WRITE(OUTS,'(A15,1(1x,e17.4))')'TOTAL HOURS = ',
     &        (TOTAL_NUM_OF_HOURS(ISYSTEM))
         CALL EDISP(IWU,OUTS)
      ELSE
         WRITE (OUTS,'(A)')'This system has not been used' 
         CALL EDISP(IWU,OUTS)
      ENDIF 
      END
      
C********************************************************************
C     H3KOUTPUT_FURNACE_DATA
C     OUTPUT MONTHLY ENERGY USED BY HVAC SYSTEMS
C     CREATED BY: PHYLROY A. LOPEZ
C     INITIAL CREATION DATE:  MARCH 23TH 2001
C     COPYRIGHT 2000: NRCAN BUILDINGS GROUP
      
C     INPUTS:
C     ISYSTEM		SYSTEM NUMBER
C     IWU		OUTPUT FILE NUMBER
C     OUTPUTS:
C     
C     COMMENTS:
C     THIS ROUTINE OUTPUTS TO FILE NUMBER IWU THE CHARECTERISTIC OF THE 
C     FURNACE SYSTEM #(ISYSTEM)    
C**********************************************************************
      
      
      SUBROUTINE H3KOUTPUT_FURNACE_DATA(ISYSTEM,IWU)


#include "building.h"
#include "hvac_parameters.h"
#include "h3kstore_parameters.h"
#include "furnace_common.h"
#include "hvac_common.h"
#include "h3kstore_common.h"
      CHARACTER OUTS*124
      
      WRITE(OUTS,'(A)')'HVAC SYSTEM TYPE = FURNACE'
      CALL EDISP(IWU,OUTS)
C     THIS HAS BEEN REMARKED OUT SINCE FUEL TYPES HAVE NOT BEEN DEFINED.
C     WRITE(OUTS,'(A,I3)')'FUEL TYPE         = ',IFUEL_TYPE(MAX_FUELS)
C     CALL EDISP(IWU,OUTS)
      WRITE(OUTS,'(A,F6.2)')'STEADY STATE EFF. = ',
     &     SS_EFFICIENCY(ISYSTEM)
      CALL EDISP(IWU,OUTS)  
      WRITE(OUTS,'(A,E10.3)')'PILOT POWER       = ',PILOT_POWER(ISYSTEM)
      CALL EDISP(IWU,OUTS)
      END
C     *********************************************************************
C     H3KOUTPUT_BOILER_DATA
C     OUTPUT MONTHLY ENERGY USED BY HVAC SYSTEMS
C     CREATED BY: PHYLROY A. LOPEZ
C     INITIAL CREATION DATE:  MARCH 23TH 2001
C     COPYRIGHT 2000: NRCAN BUILDINGS GROUP
      
C     INPUTS:
C     ISYSTEM		SYSTEM NUMBER
C     IWU		OUTPUT FILE NUMBER
C     OUTPUTS:
C     
C     COMMENTS:
C     THIS ROUTINE OUTPUTS TO FILE NUMBER IWU THE CHARECTERISTIC OF THE 
C     BOILER SYSTEM #(ISYSTEM)  
C**********************************************************************
      SUBROUTINE H3KOUTPUT_BOILER_DATA(ISYSTEM,IWU)
#include "building.h"
#include "hvac_parameters.h"
#include "h3kstore_parameters.h"
#include "furnace_common.h"
#include "hvac_common.h"
#include "h3kstore_common.h"
      CHARACTER OUTS*124
      WRITE(OUTS,'(A)')'HVAC SYSTEM TYPE = BOILER'
      CALL EDISP(IWU,OUTS)
C     THIS HAS BEEN REMARKED OUT SINCE FUEL TYPES HAVE NOT BEEN DEFINED.
C     WRITE(OUTS,'(A,I3)')'FUEL TYPE         = ',IFUEL_TYPE(MAX_FUELS)
C     CALL EDISP(IWU,OUTS)
      WRITE(OUTS,'(A,E10.3)')'STEADY STATE EFF. = ',
     &     SS_EFFICIENCY(ISYSTEM)
      CALL EDISP(IWU,OUTS)  
      WRITE(OUTS,'(A,E10.3)')'PILOT POWER       = ',
     &     PILOT_POWER(ISYSTEM)
      CALL EDISP(IWU,OUTS)

      END
C     *********************************************************************
C     H3KOUTPUT_BASEBOARD_DATA
C     OUTPUT MONTHLY ENERGY USED BY HVAC SYSTEMS
C     CREATED BY: PHYLROY A. LOPEZ
C     INITIAL CREATION DATE:  MARCH 23TH 2001
C     COPYRIGHT 2000: NRCAN BUILDINGS GROUP
      
C     INPUTS:
C     ISYSTEM		SYSTEM NUMBER
C     IWU		OUTPUT FILE NUMBER
C     OUTPUTS:
C     
C     COMMENTS:
C     THIS ROUTINE OUTPUTS TO FILE NUMBER IWU THE CHARECTERISTIC OF THE 
C     BASEBOARD SYSTEM #(ISYSTEM)    
C**********************************************************************
      SUBROUTINE H3KOUTPUT_BASEBOARD_DATA(ISYSTEM,IWU)


#include "building.h"
#include "hvac_parameters.h"
#include "h3kstore_parameters.h"
#include "hvac_common.h"
#include "h3kstore_common.h"
#include "furnace_common.h"
      CHARACTER OUTS*124
      WRITE(OUTS,'(A)')'HVAC SYSTEM TYPE = BASEBOARD'
      CALL EDISP(IWU,OUTS)
C     THIS HAS BEEN REMARKED OUT SINCE FUEL TYPES HAVE NOT BEEN DEFINED.
C     WRITE(OUTS,'(A,I3)')'FUEL TYPE         = ',IFUEL_TYPE(MAX_FUELS)
C     CALL EDISP(IWU,OUTS)
      WRITE(OUTS,'(A,E10.3)')'STEADY STATE EFF. = ',
     &     SS_EFFICIENCY(ISYSTEM)
      CALL EDISP(IWU,OUTS)  
      WRITE(OUTS,'(A,E10.3)')'PILOT POWER       = ',
     &     PILOT_POWER(ISYSTEM)
      CALL EDISP(IWU,OUTS)
      END
C     *********************************************************************
C     H3KOUTPUT_ASHP_DATA
C     OUTPUT MONTHLY ENERGY USED BY HVAC SYSTEMS
C     CREATED BY: PHYLROY A. LOPEZ
C     INITIAL CREATION DATE:  MARCH 23TH 2001
C     COPYRIGHT 2000: NRCAN BUILDINGS GROUP

C     INPUTS:
C     ISYSTEM		SYSTEM NUMBER
C     IWU		OUTPUT FILE NUMBER
C     OUTPUTS:
C     
C     COMMENTS:
C     THIS ROUTINE OUTPUTS TO FILE NUMBER IWU THE CHARECTERISTIC OF THE
C     ASHP SYSTEM #(ISYSTEM) .   
C**********************************************************************
      SUBROUTINE H3KOUTPUT_ASHP_DATA(ISYSTEM,IWU)

#include "building.h"
#include "hvac_parameters.h"
#include "ashp_common.h"
#include "h3kstore_parameters.h"
#include "hvac_common.h"
#include "h3kstore_common.h"
      CHARACTER OUTS*124       
      WRITE(OUTS,'(A)')'HVAC SYSTEM TYPE = ASHP'
      CALL EDISP(IWU,OUTS)
      WRITE(OUTS,'(A,I3)')'UNIT FUNCTION     = ',
     &     IUNIT_FUNCTION(ISYSTEM)
      CALL EDISP(IWU,OUTS)
      WRITE(OUTS,'(A,E10.3)')'VENT FLOW RATE    = ',
     &     SS_COP(ISYSTEM)
      CALL EDISP(IWU,OUTS)
      WRITE(OUTS,'(A,E10.3)')'VENT FLOW RATE    = ',
     &     SHRR(ISYSTEM)
      CALL EDISP(IWU,OUTS)
      WRITE(OUTS,'(A,I3)')'TEMP CONTROL      = ',
     &     ITEMP_CONTROL(ISYSTEM)
      CALL EDISP(IWU,OUTS)
      WRITE(OUTS,'(A,E10.3)')'CUTOFF TEMP       = ',
     &     CUTOFF_TEMP(ISYSTEM)
      CALL EDISP(IWU,OUTS)
      WRITE(OUTS,'(A,I3)')'BACKUP HEAT       = ',
     &     IBACKUP_HEAT(ISYSTEM)
      CALL EDISP(IWU,OUTS)
      WRITE(OUTS,'(A,I3)')'BACKUP SYS NUMBER = ',
     &     IBACKUP_SYS_NUM(ISYSTEM)
      CALL EDISP(IWU,OUTS)
      WRITE(OUTS,'(A,I3)')'COOLING TYPE      = ',
     &     ICOOLING_TYPE(ISYSTEM)
      CALL EDISP(IWU,OUTS)
      WRITE(OUTS,'(A,E10.3)')'RATINGBYPASSFACTOR= ',
     &     BFR(ISYSTEM)
      CALL EDISP(IWU,OUTS)
      WRITE(OUTS,'(A,I3)')'SYS NUMBER HEATING= ',
     &     ISYS_NUM_HEATING(ISYSTEM)
      CALL EDISP(IWU,OUTS)
      
      END  
      
      
C     *********************************************************************
C     H3KOUTPUT_AIMSDATA
C     OUTPUT MONTHLY ENERGY USED BY HVAC SYSTEMS
C     CREATED BY: PHYLROY A. LOPEZ
C     INITIAL CREATION DATE:  MARCH 23TH 2001
C     COPYRIGHT 2000: NRCAN BUILDINGS GROUP
      
C     INPUTS:
C     IWU		OUTPUT FILE NUMBER
C     OUTPUTS:
C     WRITE TO FILE IWU THE AIMS INFILTRATION DATA.
C
C     COMMENTS:
C     THIS ROUTINE OUTPUTS TO FILE IWU THE AIM2 AIR CHANGE DATA FOR THE BUILDING. 
C     
C     SAMPLE:
C     AIM2 AIRCHANGE DATA FOR BUILDING
C     
C     MAX AIR             MIN AIR           AVRG CHANGE
C     MONTH     CHANGE DAY HOUR     CHANGE DAY HOUR     PER HOUR
C     JAN   0.17E+00  31    1   0.61E-01   3    8     0.10E+00
C     FEB   0.14E+00   1   20   0.56E-01  20   13     0.91E-01
C     MAR   0.16E+00   5   13   0.51E-01  24   15     0.82E-01  
C**********************************************************************
      
      
      SUBROUTINE H3KOUTPUT_AIMSDATA(IWU)
C      USE AIM2_CALC_DATA
      IMPLICIT NONE
#include "building.h"
#include "hvac_parameters.h"
#include "h3kstore_parameters.h"
#include "aim2_parameters.h"      
#include "aim2_common.h"
#include "h3kstore_common.h"
      COMMON/PERS/ISD1,ISM1,ISD2,ISM2,ISDS,ISDF,NTSTEP
      common/aim2def/iAIM2,LAIM2
      DIMENSION MTHNAM(12)
      INTEGER ISD1,ISM1,ISD2,ISM2,ISDS,ISDF,NTSTEP,iAIM2,IWU,IMONTH
      CHARACTER DS*7,DS1*10,DE*7,DE1*10,MTHNAM*3
      DATA MTHNAM/'JAN','FEB','MAR','APR','MAY','JUN','JUL','AUG',
     &     'SEP','OCT','NOV','DEC'/ 
      CHARACTER OUTS*124
      CHARACTER LAIM2*72
      
      IF (iAIM2 == 1) THEN
      WRITE(OUTS,'(A)')' '
      CALL EDISP(IWU,OUTS)
      WRITE(OUTS,'(A)')'INFILTRATION AIRCHANGE DATA FOR BUILDING'
      CALL EDISP(IWU,OUTS) 
      WRITE(OUTS,'(6X,A10,10X,A10,10X,A12)')
     &     'MAX AIR','MIN AIR','AVRG CHANGE'
      CALL EDISP(IWU,OUTS)
C     HEADERS FOR TABLE DATA   
      WRITE(OUTS,'(A5,1X,A10,1X,A3,1X,A4,1X,A10,1X,A3,1X,A4,1X,A12)')
     &     'MONTH ','CHANGE/H','DAY','HOUR',
     &'CHANGE/H','DAY','HOUR','PER HOUR'
      CALL EDISP(IWU,OUTS)
C     LOOP BY MONTH FROM START MONTH ISM1 TO END MONTH ISM2
      
      DO 10 IMONTH = ISM1, ISM2
         WRITE(OUTS,
     &        '(A5,1X,E10.2,1X,I3,1X,I4,1X,E10.2,1X,I3,1X,I4,1X,E12.2)')
C     MONTH NAME 
     &        MTHNAM(IMONTH),
C     MAX\MIN AIRCHANGE AND TIME DATA(DAY, HOUR) 
     &        FMAX_AIR_CHANGE(IMONTH),
     &        IMAX_AIR_CHANGE_DAY(IMONTH),
     &        IMAX_AIR_CHANGE_HOUR(IMONTH),
     &        FMIN_AIR_CHANGE(IMONTH),
     &        IMIN_AIR_CHANGE_DAY(IMONTH),
     &        IMIN_AIR_CHANGE_HOUR(IMONTH),
C     AVERAGE AIR CHANGE OVER THE MONTH
     &        ((FMONTHLY_AIR_INFILTRATION(IMONTH) 
     &        / FNUMBER_OF_HOURS(IMONTH))
     &        /AIM2_HOUSE_VOL)
         CALL EDISP(IWU,OUTS)
 10   CONTINUE
      
      WRITE(OUTS,'(A15,1(1x,e17.4),A)')'VOLUME OF BUILDING ',
     &     AIM2_HOUSE_VOL,
     &     ' M^3'
      CALL EDISP(IWU,OUTS)
      ENDIF
      END
      
      
C *********************************************************************
C     H3KOUTPUT_SOLAR_CASUAL_FOUND(ICOMP).
C     CREATED BY: PHYLROY A. LOPEZ
C     INITIAL CREATION DATE:  OCTOBER 23TH 2001
C     COPYRIGHT 2000: NRCAN BUILDINGS GROUP
      
C     THIS ROUTINE GATHERS THE ZONE ENERGY OF SOLAR,CASUAL AND FOUNDATION.
C     INPUTS:
C     ICOMP		ZONE NUMBER
C     IWU             Ouput file number
      
c     OUTPUTS:
c     None. Outputs up Monthly  losses of energy per zone
c     to file IWU
**************************************************************************
      SUBROUTINE H3KOUTPUT_Solar_Casual_Found(izone,IWU)
#include "building.h"
#include "hvac_parameters.h"
#include "h3kstore_parameters.h"
#include "h3kstore_common.h"

      COMMON/PERS/ISD1,ISM1,ISD2,ISM2,ISDS,ISDF,NTSTEP
c     ISM2 and ISM2 are required (startup and ending month)
      COMMON/precz/zname(MCOM),zdesc(MCOM) 
c     zname(izone) is the name of the zone 'izone'
      DIMENSION MTHNAM(12)
c     This is used for Month names.
      character LTMP*72,h*72,zname*12,zdesc*64,outs*256
      CHARACTER DS*7,DS1*10,DE*7,DE1*10,MTHNAM*3
      DATA MTHNAM/'Jan','Feb','Mar','Apr','May','Jun','Jul','Aug',
     &     'Sep','Oct','Nov','Dec'/
c     Outputs empty line
      write (outs,'(a)')' '
      call edisp(IWU,outs)
c     Outputs zone name and number. 
      write(outs,'(a15,I2,a,a)')'Zone(',izone,') ',zname(izone)
      
      call edisp(IWU,outs)
 
c     Outputs headers
      write(outs,'(a15,8(2x,a17))')
     &'Month',
     &     'Aver.Temp (oC)',
     &     'Solar Extern(MJ)',
     &     'Solar Intern(MJ)',
     &     'Sol Abs Trans(MJ)',
     &     'Sol Abs Opq.(MJ)',
     &     'Casual Rad. (MJ)',
     &     'Casual Conv. (MJ)',
     &     'Fndtn Losses(MJ)'
      call edisp(IWU,outs) 
c     Loop through Month
      DO 23 imonth=ISM1,ISM2
c     outputs energy loss in MJ  (Note foundation losses originally 
c     in J) for the current month 
         WRITE(outs,'(a15,8(2x,e17.4))')
     &        MTHNAM(imonth),
     &        fSummed_Zone_Temps(izone,imonth)
     &        /iNumberOfBuildingTimeSteps(imonth),
     &        Solar_From_Extern_Monthly(izone,imonth)/1e6,
     &        Solar_From_Intern_Monthly(izone,imonth)/1e6,
     &        Solar_Abs_From_Trans_Monthly(izone,imonth)/1e6,
     &        Solar_Abs_From_Opaque_Monthly(izone,imonth)/1e6,
     &        Casual_Radiant_Monthly(izone,imonth)/1e6,
     &        Casual_Conv_Monthly(izone,imonth)/1e6,
     &        foundation_losses(izone,imonth)/1e6 
         
         call edisp(IWU,outs)

c TESTING
         i1=iNumberOfBuildingTimeSteps(imonth)
C
 23   Continue
c     End Month Loop
      return
      END	

c     ******************** H3ZOUTP ********************
      
C     MZOUTP gives summary output for save option 0 only.
C     ztu,ztl are upper/lower zone temperature.
C     zph,zpc are zone heating/cooling capacities.
C     tzph,tzpc are zone heating/cooling use over time.
      SUBROUTINE H3KMZOUTP()
#include "building.h"
#include "esprdbfile.h"
#include "hvac_parameters.h"
#include "h3kstore_parameters.h"
#include "h3kstore_common.h"

      COMMON/OUTIN/IUOUT,IUIN
      common/pophelp/h(60)
      COMMON/SHOUT/ICOUT
      COMMON/FILEP/IFIL
      COMMON/SAVE0/TVMEM(MCOM,4,2),ZTU(MCOM),ZTL(MCOM),ZPH(MCOM),
     &     ZPC(MCOM),TZPH(MCOM),TZPC(MCOM),TZPHM(MCOM,12),TZPCM(MCOM,12)
      COMMON/SAVE1/LSAVE
      COMMON/SET1/IYEAR,IBDOY,IEDOY,IFDAY,IFTIME
      
      COMMON/PERS/ISD1,ISM1,ISD2,ISM2,ISDS,ISDF,NTSTEP
      COMMON/precz/zname(MCOM),zdesc(MCOM)

      COMMON/C1/NCOMP,NCON
      common/c2/lsnam,nccode(mcom),lproj(mcom),lgeom(mcom),
     &     lshad(mcom),lthrm(mcom),indutl(mcom),lutil(mcom)
      common/C21/IFCFG,cfgroot,LCFGF
      COMMON/RESLIB/RFILE,PFILE,MSTRFILE,LAFRES
      COMMON/rcmd/LCMDFL
      character LCMDFL*144
      character LTMP*72,h*72,zname*12,zdesc*64,outs*124
      character PFILE*72,MSTRFILE*72,LAFRES*72
      character cfgroot*24,tab*1
      character*72 lsnam,lproj,lgeom
      character*72 lshad,lthrm,lutil,lcfgf,rfile
      DIMENSION MTHNAM(12)
      CHARACTER DS*7,DS1*10,DE*7,DE1*10,MTHNAM*3
      logical OK

      DATA MTHNAM/'Jan','Feb','Mar','Apr','May','Jun','Jul','Aug',
     &     'Sep','Oct','Nov','Dec'/

C     Test save option selected.
C     [for NRCan]      tab=','
      
      tab=' '
      IWU = iSL5UnitFileNumber  !Assign Output file  number to IWU
c     Open output file
      open(unit=IWU,file=LCMDFL(1:LEN_TRIM(LCMDFL))//'.h3k'
     &     ,status='replace')
      
c     Save option is zero.
 16   SHTG=0.0
      SCLG=0.0
      VA=-1000.0
      VB=1000.0
      VC=-1000.0
      VD=1000.0
      
      call edisp(IWU,' ')
      call edisp(IWU,'Performance assessment report')
      write(outs,'(3a)')'Results library',tab,RFILE
      call edisp(IWU,outs)
      write(outs,'(3a)')'Climate file',tab,LCLIM
      call edisp(IWU,outs)
      write(outs,'(3a)')'Configuration file',tab,LCFGF
      call edisp(IWU,outs)
      write(outs,'(3a)')'Configuration descr',tab,LSNAM
      call edisp(IWU,outs)

C     Generate view period string.
      CALL EDAY(ISD1,ISM1,IODS)
      CALL STDATE(IYEAR,IODS,DS,DS1)
      CALL EDAY(ISD2,ISM2,IODF)
      CALL STDATE(IYEAR,IODF,DE,DE1)
      write(outs,'(7a,i4)') 'Period',tab,DS1,' to ',DE1,' Year',tab,
     &     IYEAR
      call edisp(IWU,outs)
      call edisp(IWU,' ')

C     Report on max and min air temperatures and times of occurance for
C     each zone.
      call edisp(IWU,
     &     ' Zone         max air T  (occurance) 
     &     min air T  (occurance)'
     &     )
      DO 10 I=1,NCOMP
         T1=TVMEM(I,1,1)
         T2=TVMEM(I,2,1)
         CALL STDATE(IYEAR,INT(TVMEM(I,1,2)),DS,DS1)
         CALL STDATE(IYEAR,INT(TVMEM(I,2,2)),DE,DE1)
         WRITE(outs,'(2a,f7.2,3a,f5.2,a,f7.2,3a,f5.2)')zname(I),tab,
     &        ZTU(I),tab,DS1,'@',T1,tab,ZTL(I),tab,DE1,'@',T2
         call edisp(IWU,outs)
 10   CONTINUE
      
C     Report on heating/cooling capacity and demands for each zone.
      call edisp(IWU,' ')
      write(outs,'(2a)')
     & ' Zone           max heat (occurance)      max cool (occurance)',
     &     '        heating     cooling'
      call edisp(IWU,outs)
      write(outs,'(2a)')
     & '                 (kW)                       (kW)              ',
     &     '        (MJ)        (MJ)'
      call edisp(IWU,outs)
      
      DO 20 I=1,NCOMP
         T1=TVMEM(I,1,1)
         T2=TVMEM(I,2,1)
         T3=TVMEM(I,3,1)
         T4=TVMEM(I,4,1)
         CALL EDAYR(INT(TVMEM(I,1,2)),ID1,IM1)
         CALL EDAYR(INT(TVMEM(I,2,2)),ID2,IM2)
         CALL EDAYR(INT(TVMEM(I,3,2)),ID3,IM3)
         CALL EDAYR(INT(TVMEM(I,4,2)),ID4,IM4)
         CALL STDATE(IYEAR,INT(TVMEM(I,3,2)),DS,DS1)
         CALL STDATE(IYEAR,INT(TVMEM(I,4,2)),DE,DE1)
         PH=ZPH(I)/1000.0
         PC=ZPC(I)/1000.0
         TPH=TZPH(I)/(1000.0*NTSTEP)
         TPC=TZPC(I)/(1000.0*NTSTEP)
         WRITE(outs,'(2a,f9.2,3a,f5.2,a,f9.2,3a,f5.2,a,f10.1,a,f10.1)')
     &        zname(I),tab,PH,tab,DS1,'@',T3,tab,PC,tab,DE1,'@',T4,tab,
     &        TPH*3.6,tab,TPC*3.6
         call edisp(IWU,outs)
         
C     Find diversifide totals for heating and cooling and occurance.
         SHTG=SHTG+TPH
         SCLG=SCLG+TPC
         IF(ZTU(I).LE.VA.AND.I.NE.1)goto 6
         IA=I
         VA=ZTU(I)
         TZA=T1
         LD1=ID1
         LM1=IM1
 6       IF(ZTL(I).GE.VB.AND.I.NE.1)goto 7
         IB=I
         VB=ZTL(I)
         TZB=T2
         LD2=ID2
         LM2=IM2
 7       IF(PH.LE.VC.AND.I.NE.1)goto 8
         IC=I
         VC=PH
         TZC=T3
         LD3=ID3
         LM3=IM3
 8       IF(PC.GE.VD.AND.I.NE.1)goto 20
         ID=I
         VD=PC
         TZD=T4
         LD4=ID4
         LM4=IM4
 20   CONTINUE
      
c     Write save option 0 data.
      call edisp(IWU,' ')
      call edisp(IWU,'All zones:')
      CALL EDAY(LD1,LM1,IODS)
      CALL STDATE(IYEAR,IODS,DS,DS1)
      write(outs,'(2a,f6.1,6a,f5.2)')' Max_Temp',tab,VA,tab,' in ',
     &     zname(IA)(1:lnblnk(zname(IA))),' on ',DS1,'@',TZA
      call edisp(IWU,outs)
      
      CALL EDAY(LD2,LM2,IODS)
      CALL STDATE(IYEAR,IODS,DS,DS1)
      write(outs,'(2a,f6.1,6a,f5.2)')' Min_Temp',tab,VB,tab,' in ',
     &     zname(IB)(1:lnblnk(zname(IB))),' on ',DS1,'@',TZB
      call edisp(IWU,outs)
      
      CALL EDAY(LD3,LM3,IODS)
      CALL STDATE(IYEAR,IODS,DS,DS1)
      write(outs,'(2a,f6.1,6a,f5.2)')' Max_Heat',tab,VC,tab,' in ',
     &     zname(IC)(1:lnblnk(zname(IC))),' on ',DS1,'@',TZC
      call edisp(IWU,outs)
      
      CALL EDAY(LD4,LM4,IODS)
      CALL STDATE(IYEAR,IODS,DS,DS1)
      write(outs,'(2a,f6.1,6a,f5.2)')' Max_Cool',tab,VD,tab,' in ',
     &     zname(ID)(1:lnblnk(zname(ID))),' on ',DS1,'@',TZD
      call edisp(IWU,outs)

      call edisp(IWU,' ')
      write(outs,'(2a,f10.1,a)')'Total heating requirements',
     &     tab,SHTG*3.6,' (MJ)'
      call edisp(IWU,outs)
      write(outs,'(2a,f10.2,a)')'Total cooling requirements',
     &     tab,SCLG*3.6,' (MJ)'
      call edisp(IWU,outs)
      
C     Monthly heating and cooling demands.
      call edisp(IWU,' ')
      call edisp(IWU,'Monthly metrics:')
      call edisp(IWU,'Month   Heating   Cooling')
      call edisp(IWU,'Month    (MJ)      (MJ) ')
      do 42 i = ISM1,ISM2
         HM=0.
         CM=0.
         do 43 j = 1,NCOMP
            HM=HM+(TZPHM(j,i)/1000.0/NTSTEP)
            CM=CM+(TZPCM(j,i)/1000.0/NTSTEP)
 43      continue
         write(outs,'(2a,f10.1,a,f10.1)') MTHNAM(i),tab,
     &        HM*3.6,tab,CM*3.6
         call edisp(IWU,outs)
 42   continue

C     H3KOUTPUT Routine
      call H3KOUTPUT_MAIN(IWU)
      
C     Close output file after writing.
      ENDFILE IWU
      close(unit=IWU)
      RETURN
      END
      
      
C     *********************************************************************      
      SUBROUTINE H3KOUTPUT_FC(IWU)
      IMPLICIT NONE
#include "building.h"
#include "hvac_parameters.h"
#include "h3kstore_parameters.h"
#include "h3kstore_common.h"
      COMMON/PERS/ISD1,ISM1,ISD2,ISM2,ISDS,ISDF,NTSTEP
      common/Fcell/iFClds,LFClds
      INTEGER   ISD1,ISM1,ISD2,ISM2,ISDS,ISDF,NTSTEP
      INTEGER   iFClds
      INTEGER   IWU
      INTEGER   IMONTH
      REAL  f1,f2,f3,f4,f5,f6,f7,f8
      INTEGER   i1
      REAL   recip
      CHARACTER OUTS*256
      CHARACTER LFClds*72
      DIMENSION MTHNAM(12)
      Logical period_too_short, FC_inactive
      REAL avg_tankE_temp, avg_tankF_temp, avg_elec_eff

      REAL   FYEARH3KSTORE_FC_Q_TOTAL  
      REAL   FYEARH3KSTORE_FC_Q_NET_ELEC  
      REAL   FYEARH3KSTORE_FC_VDOT_FUEL_STP 
      REAL   FYEARAVG_ELEC_EFF 
      REAL   FYEARH3KSTORE_EL_Q_ELEMENT  
      REAL   FYEARAVG_TANKE_TEMP 
      REAL   FYEARH3KSTORE_FF_VDOT_FUEL_STP
      REAL   FYEARAVG_TANKF_TEMP 
      REAL   FYEARH3KSTORE_FC_Q_PAR_ELEC  
      REAL   FYEARPLANT_PUMP_ELEC  
      REAL   FYEARPLANT_FAN_ELEC  
      REAL   FYEARH3KSTORE_FC_Q_NONHVAC_EL  
      REAL   FYEARH3KSTORE_FC_Q_HVAC_ELEC 



 


      CHARACTER DS*7,DS1*10,DE*7,DE1*10,MTHNAM*3
      DATA MTHNAM/'JAN','FEB','MAR','APR','MAY','JUN','JUL','AUG',
     &     'SEP','OCT','NOV','DEC'/   
c     Ensures that FC is being model before output is created.
      IF (iFClds /= 1) then
         RETURN
      ENDIF   
C     OUTPUT HEADERS
      WRITE (OUTS,'(A)')' ' 
      CALL EDISP(IWU,OUTS)
      WRITE (OUTS,'(A)')'FC Data' 
      CALL EDISP(IWU,OUTS)
      WRITE(OUTS,'(A15,13(1X,A17))')
     &     'Month          ',
     &     'Elec demand(kWhr)',
     &     'Elec output(kWhr)',
     &     'Fuel consump(m^3)',
     &     '   Elec effic (%)',
     &     ' Elec tank (kWhr)',
     &     '  Elec tank T(oC)',
     &     'Gas tank cons(m3)',
     &     '   Gas tank T(oC)',
     &     'Parasitics (kWhr)',
     &     ' Pump Elec (kWhr)',
     &     '  Fan Elec (kWhr)', 
     &     'non-hvac el(KWhr)',
     &     'hvac elec  (KWhr)'
      CALL EDISP(IWU,OUTS) 
C     LOOP THROUGH MONTH IN TIME PERIOD
      DO 20 IMONTH=ISM1,ISM2

C     Check if period is too short, or fc 
C     has  been inactive
         if (iNumberOfPlantTimeSteps(IMONTH).ne.0) then
            period_too_short=.false.
            avg_tankE_temp = fH3KSTORE_EL_T_Node1_future(IMONTH)/
     &           real(iNumberOfPlantTimeSteps(IMONTH))
            avg_tankF_temp = fH3KSTORE_FF_T_node1_future(IMONTH)/
     &           real(iNumberOfPlantTimeSteps(IMONTH))
         else
            period_too_short=.true.    
            avg_tankE_temp = -99.
            avg_tankF_temp = -99.
         endif

         if(iNumberTimeStepsFC_Active(IMONTH).eq.0) then
            FC_inactive = .true.
            avg_elec_eff = 0.
         else
            FC_inactive = .false.
            avg_elec_eff = fH3KSTORE_FC_eta_fc(IMONTH)
     &        /real(iNumberTimeStepsFC_Active(IMONTH))
         endif



C     YearTotals
      fYEARH3KSTORE_FC_q_total = 
     &        fYEARH3KSTORE_FC_q_total + 
     &        fH3KSTORE_FC_q_total(IMONTH)/1000.
 
c     Must divide by 1000 to convert from w-h to kW-h 
c     net electrical output (KW-h)
      fYEARH3KSTORE_FC_q_net_elec = 
     &     fYEARH3KSTORE_FC_q_net_elec
     &     + fH3KSTORE_FC_q_net_elec(IMONTH)/1000.

c     divide by 1000 to convert from litres to m^3
c     fuel consumption(m^3)
          fYEARH3KSTORE_FC_Vdot_fuel_STP = 
     &     fYEARH3KSTORE_FC_Vdot_fuel_STP
     &     + fH3KSTORE_FC_Vdot_fuel_STP(IMONTH)/1000. 
c     electrical efficiency(%)
         fYEARavg_elec_eff  =
     &   fYEARavg_elec_eff +  avg_elec_eff

c     element power (kW-h)
      fYEARH3KSTORE_EL_q_element =  
     &        fYEARH3KSTORE_EL_q_element 
     &        + fH3KSTORE_EL_q_element(IMONTH)/1000.

c     average elec tank temperature(oC)
      fYEARavg_tankE_temp = 
     &        fYEARavg_tankE_temp +   avg_tankE_temp

c     fossil fuel consumption(m^3)
        fYEARH3KSTORE_FF_Vdot_fuel_STP =  
     &        fYEARH3KSTORE_FF_Vdot_fuel_STP 
     &        + fH3KSTORE_FF_Vdot_fuel_STP(IMONTH)/1000. 

c     average fossil fuel tank temperature(oC)
      fYEARavg_tankF_temp  = 
     &        fYEARavg_tankF_temp   + avg_tankF_temp

c     Parasitic elec losses.(kW-h)
      fYEARH3KSTORE_FC_q_par_elec 
     &        = fYEARH3KSTORE_FC_q_par_elec 
     &        +   fH3KSTORE_FC_q_par_elec(iMonth)/1000.

c Explicit Plant Pump and Fan Electrical Outputs
c Pump Electrical Energy (kW-h)
      fYEAR Plant_Pump_Elec  =  
     &        fYEARPlant_Pump_Elec   
     &     +  fPlant_Pump_Elec(iMonth)/1000.
c Fan Electrical Energy.(kW-h)
           fYEARPlant_Fan_Elec   =  fYEARPlant_Fan_Elec   
     &     +  fPlant_Fan_Elec(iMonth)/1000.
c     non hvac elec  energy (KW-H)
      fYEARH3KSTORE_FC_q_nonHVAC_el   =  
     &     fYEARH3KSTORE_FC_q_nonHVAC_el   
     &     +   fH3KSTORE_FC_q_nonHVAC_el(iMonth)/1000.
c     HVAC elec energy
          fYEARH3KSTORE_FC_q_HVAC_elec   =  
     &     fYEARH3KSTORE_FC_q_HVAC_elec   
     &     +   fH3KSTORE_FC_q_HVAC_elec(iMonth)/1000. 



           

C     OUTPUT ENERGIES  
         WRITE(OUTS,'(A15,13(1X,e17.4))')
     &        MTHNAM(IMONTH),
c     total elec demand on FC controller(kW-h)
     &        fH3KSTORE_FC_q_total(IMONTH)/1000.,
 
c     Must divide by 1000 to convert from w-h to kW-h 
c     net electrical output (KW-h)
     &        fH3KSTORE_FC_q_net_elec(IMONTH)/1000., 

c     divide by 1000 to convert from litres to m^3
c     fuel consumption(m^3)
     &        fH3KSTORE_FC_Vdot_fuel_STP(IMONTH)/1000., 
c     electrical efficiency(%)
     &        avg_elec_eff,
c     element power (kW-h)
     &        fH3KSTORE_EL_q_element(IMONTH)/1000., 
c     average elec tank temperature(oC)
     &        avg_tankE_temp,
c     fossil fuel consumption(m^3)
     &        fH3KSTORE_FF_Vdot_fuel_STP(IMONTH)/1000., 
c     average fossil fuel tank temperature(oC)
     &        avg_tankF_temp,
c     Parasitic elec losses.(kW-h)
     &       fH3KSTORE_FC_q_par_elec(iMonth)/1000.,

c Explicit Plant Pump and Fan Electrical Outputs
c Pump Electrical Energy (kW-h)
     &       fPlant_Pump_Elec(iMonth)/1000.,
c Fan Electrical Energy.(kW-h)
     &       fPlant_Fan_Elec(iMonth)/1000.,
c non hvac elec  energy (KW-H)
     &        fH3KSTORE_FC_q_nonHVAC_el(iMonth)/1000.,
c HVAC elec energy
     &        fH3KSTORE_FC_q_HVAC_elec(iMonth)/1000.



         CALL EDISP(IWU,OUTS)
 20   CONTINUE
      
      WRITE(OUTS,'(A15,13(1X,e17.4))')
     &     'Total',
         
C     YearTotals
     &     fYEARH3KSTORE_FC_q_total,

         
c     Must divide by 1000 to convert from w-h to kW-h 
c     net electrical output (KW-h)
     &     fYEARH3KSTORE_FC_q_net_elec, 
         
c     divide by 1000 to convert from litres to m^3
c     fuel consumption(m^3)
     &     fYEARH3KSTORE_FC_Vdot_fuel_STP, 
         
         
c     electrical efficiency(%)
     &     fYEARavg_elec_eff/REAL(ISM2-ISM1+1),


c     element power (kW-h)
     & fYEARH3KSTORE_EL_q_element,  


c     average elec tank temperature(oC)
     & fYEARavg_tankE_temp/REAL(ISM2-ISM1+1),


c     fossil fuel consumption(m^3)
     &   fYEARH3KSTORE_FF_Vdot_fuel_STP, 
 

c     average fossil fuel tank temperature(oC)
     & fYEARavg_tankF_temp/REAL(ISM2-ISM1+1),


c     Parasitic elec losses.(kW-h)
     &     fYEARH3KSTORE_FC_q_par_elec,
         
c Explicit Plant Pump and Fan Electrical Outputs
c     Pump Electrical Energy (kW-h)
     &     fYEAR Plant_Pump_Elec,  
         
c     Fan Electrical Energy.(kW-h)
     &     fYEARPlant_Fan_Elec,   
         
c     non hvac elec  energy (KW-H)
     &     fYEARH3KSTORE_FC_q_nonHVAC_el,  

c     HVAC elec energy
     &     fYEARH3KSTORE_FC_q_HVAC_elec 

      CALL EDISP(IWU,OUTS)

      END


C     *********************************************************************
C     SUBROUTINE FC_Timestep_Out
C     Outputs the timstep data to a file. The output format is detailed in the write statement below. 
C     
C     CREATED BY: PHYLROY A. LOPEZ
C     INITIAL CREATION DATE:  MARCH 23TH 2001
C     COPYRIGHT 2000: NRCAN BUILDINGS GROUP
C     OUTPUTS:
C     OUTPUT MONTHLY ENERGY VALUES of FC TO FILE (iFC_TimeStep_File_Number) 
      SUBROUTINE FC_Timestep_Out()

      IMPLICIT NONE
#include "building.h"
#include "hvac_parameters.h"
#include "h3kstore_parameters.h"
#include "h3kstore_common.h"
      common/simtim/ihrp,ihrf,idyp,idyf,idwp,idwf,nsinc,its
      COMMON/rcmd/LCMDFL
      INTEGER   ihrp,ihrf,idyp,idyf,idwp,idwf,nsinc,its
      character outs*512
      character LCMDFL*144
      logical bInStartup        ! test for start-up period (function)
C if FC timestep output is set to true then open output file for FC data (cfg file/path name)+(.fcts)
C These if statements checks which month it is in and if the file is already open for that quarter.
C they then open teh appropriate file (fcts1-4) and close the previous file. 
C the open file is then used as teh output for that month set.

      IF (nsinc .eq. 1 ) THEN
         Return                 ! There's no data to report
      ENDIF


      IF ( (iPreviousTimeStepMonth .ge.1) 
     &    .and. (iPreviousTimeStepMonth .le. 3)
     &    .and. (.not.bQ1Started) ) THEN
         bQ1Started = .true.
         iCurrent_FCT_OUTPUT_FILE = iFC_TimeStep_File_Number1
         open(unit=iFC_TimeStep_File_Number1,
     &        file= LCMDFL(1:LEN_TRIM(LCMDFL))//'.fcts1'
     & ,status='replace')
      ENDIF
      
      IF (iPreviousTimeStepMonth >=4 
     &    .and. iPreviousTimeStepMonth <= 6
     &    .and. .not.bQ2Started ) THEN
         bQ2Started = .true.
         close(unit=iFC_TimeStep_File_Number1)
         iCurrent_FCT_OUTPUT_FILE = iFC_TimeStep_File_Number2
         open(unit=iFC_TimeStep_File_Number2,
     &        file=LCMDFL(1:LEN_TRIM(LCMDFL))//'.fcts2'
     &        ,status='replace')
      ENDIF    

      IF (iPreviousTimeStepMonth >=7 
     &    .and. iPreviousTimeStepMonth <= 9
     &    .and. .not.bQ3Started ) THEN
         bQ3Started = .true.
         close(unit=iFC_TimeStep_File_Number2)
         iCurrent_FCT_OUTPUT_FILE = iFC_TimeStep_File_Number3
         open(unit=iFC_TimeStep_File_Number3,
     &        file=LCMDFL(1:LEN_TRIM(LCMDFL))//'.fcts3'
     &        ,status='replace')
      ENDIF 
      
      IF (iPreviousTimeStepMonth >=10 
     &    .and. iPreviousTimeStepMonth <= 12
     &    .and. .not.bQ4Started
     &    .and. .not.bInStartUp() ) THEN
         bQ4Started = .true.
         close(unit=iFC_TimeStep_File_Number3)
         iCurrent_FCT_OUTPUT_FILE = iFC_TimeStep_File_Number4
         open(unit=iFC_TimeStep_File_Number4,
     &        file=LCMDFL(1:LEN_TRIM(LCMDFL))//'.fcts4'
     &        ,status='replace')
      ENDIF 
         
      WRITE(outs,'(I10,1x,I3,1x,I3,1x,f5.2,
     &             36(1x,e10.4)
     &     )')
     &     nsinc - 1 ,             !Timestep Number (for previous timestep)
     &     iPreviousTimeStepMonth, !Date information
     &     iPreviousTimeStepDay,   !Day of Month
     &     fPreviousTimeStepHour,  !Hour of Day

C data for the previous timstep..
C Note some data for the previous timestep is availabale only after the plant converges and thus present in the currnt timestep. so 
C that is the reason why some of the current values are used below.

C Data pertaining to fuel cell component.
     &     fPreviousTS_q_total_elec,       ! Total electric demand (W)                                    
     &     fPreviousTS_q_net_elec,         ! FC electrical output  (W)
     &     fPreviousTS_q_par_elec,         ! FC parasitical elec losses (W)
     &     fPreviousTS_eta_fc,             ! FC elec efficiency
     &     fPreviousTS_FC_Vdot_fuel_STP,   ! FC fuel consumption (slpm)    
     &     fPreviousTS_Vdot_air_STP,       ! FC air supply (slpm)     
     &     fPreviousTS_stoichs,            ! FC air supply (Stoichs)
     &     fPreviousTS_Vdot_liqwater,      ! FC water supply (L/min) 
     &     fPreviousTS_Vdot_exh_STP,       ! FC exhaust flow rate (slpm)
     &     fCurrentTS_T_exh,              ! Temp of exhaust gases into HX (oC)
     &     fCurrentTS_T_exit,             ! Temp of exhaust gases at outlet of HX (oC)
     &     fCurrentTS_Twater_enter,       ! Temperature of water entering HX (oC)
     &     fPreviousTS_UA_exh,             ! UA of HX (W/oC)
     &     fPreviousTS_fHeatToWater,       ! Heat transferred from exhaust gases to water at HX (W)(note uses current T of exh, water_enter)

C Data pertaining to electrically heated water storage tank (may not exist in user's .pln).
     &     fPreviousTS_q_EL_element,       ! Electric power dissipated in heater element (W)    
     &     fPreviousTS_q_EL_capture_fut,   ! Energy transferred from elec element to water (W)
     &     fCurrentTS_T_EL_Node1_fut,     ! Tank temperature (oC)
     &     fCurrentTS_T_EL_1a_present,    ! Temperature of water at first connection (oC)
     &     fCurrentTS_f_EL_First_Conn,    ! Flow rate of water at first connection (kg/s)
     &     fCurrentTS_T_EL_1b_present,    ! Temperature of water at second connection (oC)
     &     fCurrentTS_f_EL_Second_Conn,   ! Flow rate of water at second connection (kg/s)
     &     fPreviousTS_T_EL_makeup_fut,    ! Temperature of make-up water from mains (oC)
     &     fPreviousTS_f_EL_DHW_draw_fut,  ! DHW water draw (m3/s) at the future time-row
     &     fPreviousTS_f_EL_Jacketlosses, ! Jacket loss from tank to room (W)
     &     fPreviousTS_f_EL_heatdump,      ! Rate at which energy is dumped from tank to maintain temp (W) 

C Data pertaining to gas-fired water storage tank (may not exist in user's .pln).
     &     fPreviousTS_f_FF_Vdot_fuel_STP, ! Fuel consumption rate (slpm)
     &     fPreviousTS_q_FF_capture_fut,   ! Energy transferred from burner to water (W)
     &     fCurrentTS_T_FF_node1_fut,     ! Tank temperature (oC)
     &     fCurrentTS_f_FF_T_1a_present,  ! Temperature of water at first connection (oC)
     &     fCurrentTS_f_FF_First_Conn,    ! Flow rate of water at first connection (kg/s)
     &     fCurrentTS_f_FF_T_1b_present,  ! Temperature of water at second connection (oC)
     &     fCurrentTS_f_FF_Second_Conn,   ! Flow rate of water at second connection (kg/s)
     &     fPreviousTS_T_FF_makeup_fut,    ! Temperature of make-up water from mains (oC)
     &     fPreviousTS_f_FF_DHW_draw_fut,  ! DHW water draw (m3/s) at the future time-row
     &     fPreviousTS_f_FF_JacketLosses,  ! Jacket loss from tank to room (W)
     &     fPreviousTS_f_FF_heatdump       ! Rate at which energy is dumped from tank to maintain temp (W) 
      iNumber_Of_Lines_In_FCT_File = iNumber_Of_Lines_In_FCT_File + 1
      call edisp(iCurrent_FCT_OUTPUT_FILE,outs)
      RETURN
      END


      SUBROUTINE H3KSTORE_Progress()
           IMPLICIT NONE
#include "building.h"
#include "hvac_parameters.h"
#include "h3kstore_parameters.h"
#include "h3kstore_common.h"
      common/simtim/ihrp,ihrf,idyp,idyf,idwp,idwf,nsinc,its
      INTEGER   ihrp,ihrf,idyp,idyf,idwp,idwf,nsinc,its 
      INTEGER   iZONE,IDAYN,iMonth,ISTAT, I
      DIMENSION MTHNAM(12)
      CHARACTER MTHNAM*3
      DATA MTHNAM/'JAN','FEB','MAR','APR','MAY','JUN','JUL','AUG',
     &     'SEP','OCT','NOV','DEC'/
      REAL  fZone_temp
      LOGICAL bInStartup
      Character*72 outs
      IF( .not.bInStartUp() )then
         CALL EDAYR(idyp,IDAYN,iMonth)
         IF (iCurrentMonth == 0) then
C     erase old runs if present. 
            DO 41 I=1,12          
               OPEN (UNIT=iMonthStatusUnitNumber,
     &              FILE=MTHNAM(I),
     &              STATUS = "UNKNOWN")
               CALL EFDELET(iMonthStatusUnitNumber,
     &              ISTAT)
 41         CONTINUE
         ENDIF
         IF (iMonth == iCurrentMonth) then
            return
         endif
         iCurrentMonth = iMonth
 

         CALL EFDELET(iMonthStatusUnitNumber,ISTAT)
         OPEN (UNIT = iMonthStatusUnitNumber,
     &        FILE = MTHNAM(iMonth),
     &        STATUS = "UNKNOWN")
        


       
      ENDIF
      RETURN
      END
