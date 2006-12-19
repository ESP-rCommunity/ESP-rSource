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

c This file contains the following subroutines and function:
c Functions used to calculate the cold mains water inlet temperature.
c   fDHW_DepthAmplitudeMod()
c   fDHW_LimitMeanGroundTemp()
c   iDHW_GroundOffset()
c   fDHW_ColdMainTemp()
c 
c Routines to calculate the ground temperatures based on Moore's Model:
c   Ground_Temp_Profile -  a subroutine that writes the ground temperature parameters to
c                          a common block 
c   fTemp_Ground_Avg()
c   fTemp_Ground_Amp()
c   fTemp_Ground_phaseshift()
c   fTemp_Ambient_Amp()
c   fTemp_Ambient_phaseshift()
c   fS_1()
c   fC_1() 
c Note: The ground temperature is not calculated here - just the parameters for the 
c cosine wave: mean, amplitude and phase-shift.
 
c Subroutine used to calculate the number of heating degree days and average monthly ambient dry bulb 
c temperatures based on the chosen climate file:
c   CLM_DEG_DAY
c   Daily_DBT -  a function to recover one day's dry bulb temperature data from climate file
c                to a common block array 
 
c *********************************************************************
c fDHW_DepthAmplitudeMod 

c Returns the Depth Amplitude Modifier.
c This Formula was obtained originally from 
c SAR Engineerings LTD report on 'Domestic Hot Water Simulation Algorithms' by Ken Cooper for NRCan
c Section 4.2 Appendix. Where AMod = 0.2+0.04*(TGMean-5.0)

c Created by: Phylroy A. Lopez
c Initial Creation Date:  March 23th 2001
c Updated by: Julia Purdy, September 29th 2005
c Copyright 2000: NRCan Buildings Group

c OUTPUTS:
c   REAL    fDHW_DepthAmplitudeMod Depth Amplitude Modifier
c
c Modification made by Julia Purdy 
c   - including reference to ground_temp_mains_common.h
c----56--1---------2---------3---------4---------5---------6---------7---------8
 
           
      FUNCTION fDHW_DepthAmplitudeMod()
!Variables Used From Module
      
      
      IMPLICIT NONE
#include "dhw_parameters.h"
#include "dhw_common.h"
#include "ground_temp_mains_common.h"

!This line was obtained from B.Bradeley H2KDHW.F  line 205 and is related to the Ken Cooper model.
        fDHW_DepthAmplitudeMod = 0.2 
     &                           + 0.04  
     &                           * ( fDHW_LimitMeanGroundTemp()
     &                           - 5.0 )
      
      END


c *********************************************************************
c FUNCTION fDHW_LimitMeanGroundTemp

c Returns GroundTemp in Limit Bounds 
c Ground Temp is rarely beyond these values. 
c This is reference in the Ken Cooper model as well as B.Bradley's code in H2KDHW.F line 205-206

c Created by: Phylroy A. Lopez
c Initial Creation Date:  March 23th 2001
c Updated by: Julia Purdy, September 29th 2005
c Copyright 2000: NRCan Buildings Group

c Modification made by Julia Purdy
c   - references to fMeanGroundTemp updated to Temp_Ground_Avg
c   - including reference to ground_temp_mains_common.h
c
c INPUTS:
c   REAL Temp_Ground_Avg               Average Ground Temperature 
c OUTPUTS:
c   REAL fDHW_LimitMeanGroundTemp      limits Values of fMeangroundTemp 
c                                      to <15.0 and >5.0 C.
c----56--1---------2---------3---------4---------5---------6---------7---------8
        FUNCTION fDHW_LimitMeanGroundTemp()
        
     IMPLICIT NONE
#include "dhw_parameters.h"
#include "dhw_common.h"     
#include "ground_temp_mains_common.h"       
              
        IF (Temp_Ground_Avg .lt. 5.0) THEN
           fDHW_LimitMeanGroundTemp = 5.0
         ELSEIF (Temp_Ground_Avg .gt. 20.0) THEN 
           fDHW_LimitMeanGroundTemp = 20.0
         ELSE
           fDHW_LimitMeanGroundTemp = Temp_Ground_Avg
        END IF
        
        
      END

c *********************************************************************
c INTEGER FUNCTION iDHW_GroundOffset(fMeanGroundTemp)

c Returns the ground temperature offset value. This formula was obtained from 
c SAR Engineerings LTD report on 'Domestic Hot Water Simulation Algorithms' by Ken Cooper for NRCan
c Section 4.2 Appendix

c Created by: Phylroy A. Lopez
c Initial Creation Date:  March 23th 2001
c Updated by: Julia Purdy, September 29th 2005
c Copyright 2000: NRCan Buildings Group

c Modification made by Julia Purdy
c   - references to fMeanGroundTemp updated to fTemp_Ground_Avg
c   - removal of a typo
c   - including reference to ground_temp_mains_common.h
c   - including reference to ground temp common block
c
c INPUTS:
c   REAL     Temp_Ground_Avg         Average Ground Temperature 
c
c OUTPUTS:
c   INTEGER  iDHW_GroundOffset       Offset to account for depth temp. variance
c----56--1---------2---------3---------4---------5---------6---------7---------8

        FUNCTION iDHW_GroundOffset()
        
     IMPLICIT NONE
#include "dhw_parameters.h"
#include "dhw_common.h"        
#include "ground_temp_mains_common.h"


c Select Offset Modifier based on Mean Ground Temperature
         IF (Temp_Ground_Avg .lt. 5.0) THEN
           iDHW_GroundOffset = 6
         
         ELSEIF (Temp_Ground_Avg .gt. 15.0) THEN
          iDHW_GroundOffset = 8
       
         ELSE 
          iDHW_GroundOffset = 7

         END IF      
         
      return      
      END

c *********************************************************************
c FUNCTION fDHW_ColdMainTemp

c Returns the cold main value in degrees C. This formula was obtain from 
c SAR Engineerings LTD report on 'Domestic Hot Water Simulation Algorithms' by Ken Cooper for NRCan
c Section 4.2 Appendix

c Created by: Phylroy A. Lopez
c Initial Creation Date:  March 23th 2001
c Updated by: Julia Purdy, September 29th 2005
c Copyright 2000: NRCan Buildings Group
c
c Modification made by Julia Purdy
c   - including reference to ground_temp_mains_common.h
c   - updating equation to use amplitude modifier
c   - variable name changed from fAmpGroundTempVar to Temp_Ground_Amp
c
c INPUTS:
c     INTEGER   iMonth              month of current simulation timestep
c OUTPUTS:
c     REAL      fDHW_ColdMainTemp   cold water main temperature C
c----56--1---------2---------3---------4---------5---------6---------7---------8

      FUNCTION fDHW_ColdMainTemp()
      
      IMPLICIT NONE
#include "dhw_parameters.h"
#include "dhw_common.h"   
#include "ground_temp_mains_common.h"   
           
c Variable Definition        
c Formula  This is based on Ken Coopers Cold water model and B.Bradley's model used in H2KDHW.F 
c line 250     
c        fDHW_ColdMainTemp =
c     &     11.1 
c     &     + 3.0 
c     &     + fAmpGroundTempVar
c     &     * SIN(PI/6*(REAL(iMonth) + REAL(iDHW_GroundOffset())))             

c This above equation is updated to account for the amplitude modifier. This
c variable is a part of the model used in HOT2000 but was omitted in earlier ESP-r/HOT3000 versions.  
c In addition fAmpGroundTempVar becomes Temp_Ground_Amp - the calculated ground
c temperature amplitude.
        fDHW_ColdMainTemp =
     &     fDHW_LimitMeanGroundTemp() 
     &     + 3.0 
     &     + fDHW_DepthAmplitudeMod()
     &     * Temp_Ground_Amp
     &     * SIN(PI/6*(REAL(iMonth) + REAL(iDHW_GroundOffset())))
    
      return
      END
      


c *********************************************************************
c Ground_Temp_Profile

c Created by: Julia Purdy
c Initial Creation Date:  October 4, 2005

c Copyright 2005: NRCan Sustainable Buildings & Communities
c OUTPUTS:
c       REAL  Temp_Ground_Avg               !annually-averaged soil temperature.
c       REAL  Temp_Ground_Amp               !amplitude of ground-temperature's annual sine wave.
c       REAL  Temp_Ground_phaseshift        !phase lag of ground-temperature's annual sine wave.      
 

c Abstract
c This subroutine calls 3 functions to calculate the ground temperature profile based
c on the Moore Method. These 3 functions are:
c fTemp_Ground_Avg - returns the average ground temperature
c fTemp_Ground_Amp - returns the amplitude of ground-temperature's annual sine wave.
c fTemp_Ground_phaseshift - returns the phase lag of ground-temperature's annual sine wave.
c
c These variables are then stored in the common block 
c COMMON/Ground_Temp/Temp_Ground_Avg,Temp_Ground_Amp,Temp_Ground_phaseshift
c for use in various calculations (including basesimp and mains water temperature calculation)
c 
c----56--1---------2---------3---------4---------5---------6---------7---------8

      SUBROUTINE Ground_Temp_Profile      
        
      IMPLICIT NONE
#include "ground_temp_mains_common.h" 

c Call to the function fTemp_Ground_Avg to calculate the average ground temperature.       
         Temp_Ground_Avg = fTemp_Ground_Avg()
       
c Call to the function fTemp_Ground_Amp to calculate the 
c amplitude of ground-temperature's annual sine wave.       
         Temp_Ground_Amp = fTemp_Ground_Amp()
       
c Call to the function fTemp_Ground_phaseshift to calculate the 
c phaseshift of ground-temperature's annual sine wave.       
         Temp_Ground_phaseshift = fTemp_Ground_phaseshift()
          
          
        return
       END
       
  
             
c *********************************************************************
c fTemp_Ground_Avg 
c Returns the average ground temperature.
c This formula was adopted from the Moore Method
c "HOT-2000 Energy Analysis Program
c Soil Temperature Estimation Model"
c Glenn Moore, 1986
c 
c Created by: Julia Purdy
c Initial Creation Date:  September 29, 2005

c Copyright 2005: NRCan Sustainable Buildings & Communities
c INPUTS:
c       REAL Temp_Ambient_Avg     monthly average ambient DB temperature array(oC)
c                                 one value for each month of the year
c       REAL DEGREE_DAY           annual heating degree days
c OUTPUTS:
c       REAL fTemp_Ground_Avg     average ground temperature (oC)
c
c Abstract
c To calculate the annual average ground temperature requires the annual average
c ambient temperature. The annual average temperature is determined by summing the monthly 
c average ambient temperatures and dividing by the number of months considered. 
c----56--1---------2---------3---------4---------5---------6---------7---------8


      FUNCTION fTemp_Ground_Avg()
      
      IMPLICIT NONE
      
#include "ground_temp_mains_common.h"

       REAL Temp_Ambient_Avg_sum      ! sum of monthly average ambient DB temps
       REAL fAnnual_ambient_avg       ! average of the annual ambient DB temps
       REAL nmonth                    ! sum the number of months in the year
       INTEGER i

c set initial conditions
      Temp_Ambient_Avg_sum = 0.0
      nmonth = 0.0
c Calculate the annual average ambient temperature. Sum the monthly average
c ambient temperatures and divide by the number of months considered. 
      do i = 1,12
c -----Sum the monthly average ambient temperatures
         Temp_Ambient_Avg_sum = Temp_Ambient_Avg_sum 
     &                        + Temp_Ambient_Avg(i)
c -----sum the number of months in the year
         nmonth = nmonth + 1
      end do
        
c annual average ambient temperature
      fAnnual_ambient_avg = Temp_Ambient_Avg_sum / nmonth

c average ground temperature
      fTemp_Ground_Avg = fAnnual_ambient_avg                 
     &                 - 1.438 
     &                 + ( 9.189E-4 * DEGREE_DAY )

      return     
      END

      
c *********************************************************************
c fTemp_Ground_Amp 
c Returns the amplitude of ground-temperature's annual sine wave.
c This formula was adopted from the Moore Method
c "HOT-2000 Energy Analysis Program
c Soil Temperature Estimation Model"
c Glenn Moore, 1986
c 
c Created by: Julia Purdy
c Initial Creation Date:  September 29, 2005

c Copyright 2005: NRCan Sustainable Buildings & Communities

c INPUTS:
c          REAL fTemp_Ambient_Amp     temperature amplitude of ambient air (oC)
c       REAL DEGREE_DAY               annual heating degree days
c OUTPUTS:
c          REAL fTemp_Ground_Amp     amplitude of ground-temperature sine wave (oC)
c----56--1---------2---------3---------4---------5---------6---------7---------8

      FUNCTION fTemp_Ground_Amp()
      
      IMPLICIT NONE
      
#include "ground_temp_mains_common.h"


      fTemp_Ground_Amp = fTemp_Ambient_Amp()
     &                 - 7.875 
     &                 + ( 1.97E-3 * DEGREE_DAY )
     
       return
       END      


            
c *********************************************************************
c fTemp_Ground_phaseshift 
c Calculates the phase lag of ground-temperature's annual sine wave.
c This Formula was obtained originally from Moore Method
c 
c Created by: Julia Purdy
c Initial Creation Date:  September 29, 2005

c Copyright 2005: NRCan Sustainable Buildings & Communities

c INPUTS:
c     REAL fTemp_Ambient_phaseshift     phase shift of ambient air
c     REAL DEGREE_DAY                   annual heating degree days
c OUTPUTS:
c     REAL fTemp_Ground_phaseshift      phase lag of ground-temperature sine wave
c----56--1---------2---------3---------4---------5---------6---------7---------8

      FUNCTION fTemp_Ground_phaseshift()
      
      IMPLICIT NONE
      
#include "ground_temp_mains_common.h" 

      fTemp_Ground_phaseshift = fTemp_Ambient_phaseshift()
     &                        - 0.0756 
     &                        + ( 2.128E-5 * DEGREE_DAY )
     
      return
      END      
     

c *********************************************************************
c fTemp_Ambient_Amp 
c Returns the amplitude of ambient-temperature's annual sine wave.
c This formula was adopted from the Moore Method
c "HOT-2000 Energy Analysis Program
c Soil Temperature Estimation Model"
c Glenn Moore, 1986
c 
c Created by: Julia Purdy
c Initial Creation Date:  October 4, 2005

c Copyright 2005: NRCan Sustainable Buildings & Communities

c INPUTS:
c     REAL fS_1                    cosine curve-fit coefficient          
c     REAL fC_1                    cosine curve-fit coefficient
c OUTPUTS:
c     REAL fTemp_Ambient_Amp()     amplitude of ambient-temperature sine wave (oC)
c----56--1---------2---------3---------4---------5---------6---------7---------8

      FUNCTION fTemp_Ambient_Amp()
      
      IMPLICIT NONE
      
#include "ground_temp_mains_common.h"
    
c This equation calls two functions fS_1 and fC_1 which calculate the coefficients
c to fit the monthly cosine curve for ground temperature.
               
        fTemp_Ambient_Amp = -(sqrt((fS_1())**2 + (fC_1())**2))/6

      return
      END      

            
        
c *********************************************************************
c fTemp_Ambient_phaseshift() 
c Returns the phase-shift of the ambient-temperature's annual sine wave.
c This formula was adopted from the Moore Method
c "HOT-2000 Energy Analysis Program
c Soil Temperature Estimation Model"
c Glenn Moore, 1986
c 
c Created by: Julia Purdy
c Initial Creation Date:  October 4, 2005

c Copyright 2005: NRCan Sustainable Buildings & Communities

c INPUTS:
c     REAL fS_1                       curve-fit coefficient          
c     REAL fC_1                       curve-fit coefficient
c OUTPUTS:
c     REAL fTemp_Ambient_phaseshift() phaseshift of the ambient-temperature sine wave
c----56--1---------2---------3---------4---------5---------6---------7---------8

      FUNCTION fTemp_Ambient_phaseshift()
      
      IMPLICIT NONE
      
#include "ground_temp_mains_common.h"
                    
      fTemp_Ambient_phaseshift =  atan((fS_1())/(fC_1()))

      return
      END

            
c *********************************************************************
c fS_1 
c Calculates the coefficient fS_1 - which is used to fit the monthly data
c to a cosine curve for ground temperature profile.
c This Formula was obtained originally from Moore Method
c 
c Created by: Julia Purdy
c Initial Creation Date:  October 4, 2005

c Copyright 2005: NRCan Sustainable Buildings & Communities

c INPUTS:
c     REAL Temp_Ambient_Avg(12)
c OUTPUTS:
c     REAL fS_1
c----56--1---------2---------3---------4---------5---------6---------7---------8      
      
      FUNCTION fS_1()
      
      IMPLICIT NONE
      
#include "dhw_parameters.h"
#include "ground_temp_mains_common.h"

       INTEGER i

      fS_1 = 0.0
         do i = 1, 12          ! for each month of the year
           fS_1 = fS_1 
     &          +  Temp_Ambient_Avg(i) 
     &          * sin ((( 2 * PI )/12 )* ( real(i) - 0.5 ))
         end do
        
       return
       END            
      
      
c *********************************************************************
c fC_1 
c Calculates the coefficient fC_1 - which is used to fit the monthly data
c to a cosine curve for ground temperature profile.
c This Formula was obtained originally from Moore Method
c 
c Created by: Julia Purdy
c Initial Creation Date:  October 4, 2005

c Copyright 2005: NRCan Sustainable Buildings & Communities

c INPUTS:
c       REAL Temp_Ambient_Avg()     Average monthly ambient temperature
c OUTPUTS:
c       REAL fC_1
c----56--1---------2---------3---------4---------5---------6---------7---------8      
      
      FUNCTION fC_1()
      
      IMPLICIT NONE
      
#include "ground_temp_mains_common.h"
#include "dhw_parameters.h"

       INTEGER i     

      fC_1 = 0.0
        do i = 1, 12
          fC_1 = fC_1
     &         + Temp_Ambient_Avg(i) 
     &         * cos ((( 2 * PI )/12 )* ( i - 0.5 ))
        end do
        
      return
      END
        
 
c *********************************************************************
c Daily_DBT 
c Recover one day's data from climate file into common block for use.
c 
c Created by: Julia Purdy
c Initial Creation Date:  October 4, 2005

c Copyright 2005: NRCan Sustainable Buildings & Communities

c INPUTS:
c     INTEGER  iday_number          ! current day number, passed in the called statement
c OUTPUTS:
c     REAL     DBT_day(24)          ! a 24-hourly array of dry bulb temperatures
c Abstract
c This subroutine reads in the climate information for one day and then saves the 
c dry bulb temperatures to a common block for use in other subroutines.
C The appropriate weather file has already been opened in the calling subroutine.
c----56--1---------2---------3---------4---------5---------6---------7---------8         

      SUBROUTINE Daily_DBT(iday_number)
#include "esprdbfile.h"

      IMPLICIT NONE 
           
#include "ground_temp_mains_common.h"

       INTEGER j     ! hour counter, 1-24
       INTEGER irec, iday_number
       INTEGER istat
c climatic parameters 
       REAL idif(24)     ! diffuse radiation
       REAL itmp(24)     ! outdoor dry bulb temperature
       REAL idnr(24)     ! direct normal radiation
       REAL ivel(24)     ! wind speed
       REAL idir(24)     ! wind direction
       REAL ihum(24)     ! relative humidity
        
c read in temperature values for present day
c note: the climate file has already been opened - from the calling subroutine
      irec=iday_number
      read(iClim,rec=irec,iostat=istat,err=1000)
     &    (idif(j),itmp(j),idnr(j),ivel(j),idir(j),ihum(j),j=1,24)

c-----A new common block is created for storing the daily dry bulb temperatures
c-----common/daily_dry_bulb/DBT_day. 
c-----This common block is stored in cetc/ground_temp_mains_common.F
      DBT_day(j) = itmp(j) 

 1000 continue         ! this needs to be an error message
                    
      RETURN
      END
  
       
       
c *********************************************************************
c CLM_DEG_DAY

c Main subroutine for the calculation of the number of annual heating degree
c days and average monthly ambient dry bulb temperatures.

c Created by: Julia Purdy
c Initial Creation Date:  October 3, 2005

c Copyright 2005: NRCan Sustainable Buildings & Communities
c OUTPUTS:
c     REAL DEGREE_DAY             number of heating degree days for specified location
c     REAL Temp_Ambient_Avg     monthly average ambient DB temperature array(oC)
c                                 one value for each month of the year

c Abstract
c The calculation of the number of annual degree days (DD)is as follows:
c For every day, the maximum and minimum temperatures are defined.
c The average of the daily max & min temperatures is then calculated as (max+min)/2.
c If this average is less than the base temperature (hard coded to 18.2oC) then the number of 
c degree days for that day is zero. if this average is greater than the base, then the difference 
c is the number of degree days for that day.
c These daily degree days are summed for the entire year.

c This subroutine also returns an array of monthly average ambient dry bulb temperatures.
c The hourly ambient dry bulb temperatures from the climate file are averaged for every month.
c When the simulation reaches the next month, the average dry bulb temperature is written to
c the 12-value array Temp_Ambient_Avg

c This subroutine is called once per simulation, or once per climate file in the case of 
c multi-year simulations.
c----56--1---------2---------3---------4---------5---------6---------7---------8

      SUBROUTINE CLM_DEG_DAY

      IMPLICIT NONE
#include "ground_temp_mains_common.h"   

       INTEGER I,J                         ! counters
       REAL Base_Temp                      ! degree day calculation base temperature
       REAL DB_DAILY_MAX, DB_DAILY_MIN     ! daily max and min dry bulb temperatures
       REAL DB_DAILY_AVG                   ! average of max and min dry bulb temperature
       REAL DEGREE_DAY_DAILY               ! daily summation of degree days
       REAL HRTOT                          ! total number of hours in the month
       REAL monthly_DB_sum                 ! summation of the dry bulb temperatures for the month
       INTEGER iday_number,IDay            ! day of simulation
       INTEGER IMonth, MONTH               ! month of simulation 

                            
c Degree day calculations are derived from a specific base temperature
c This base temperature is set to 18.2 deg C. 
      Base_Temp = 18.2     ! degree day calculation base temperature
c Initialize parameters for monthly average temperature calculation. As this subroutine is called
c only once per simulation or once per climate file in the case of multi-year simulations, we can
c initialize these parameters within the subroutine.
      monthly_DB_sum = 0.
      HRTOT = 0.     
      MONTH = 1          
     
c For each day of the year, get climate data (in a 24 hour array) and calculate the maximum
c and minimum values.
      DO 10 I = 1,365     
c-----We do not use the simulation start and end dates here (IDS & IDF) because this 
c-----calculation must be performed over the entire year - and the simulation may only be 
c-----performed for a fraction of that. 

c Get the month for this day (I)
c EDAYR is a standard ESP-r subroutine that returns the day (IDAY) and month (IMONTH) numbers 
c from the day of the simulation 
        CALL EDAYR(I,IDAY,IMONTH)

c READ VALUES OF THE CLIMATIC PARAMETERS FOR THIS DAY.
c This call to Daily_DBT returns an array of 24 hourly values of the
c external dry bulb temperature (DBT_day) for the day I.               
        CALL Daily_DBT(IDAY)     
c DBT_day is an hourly array of dry bulb temps stored in the common block
c common/daily_dry_bulb/DBT_day


c Call to the subroutine that calculates the average ambient monthly temperatures
c        CALL fTemp_Ambient_Avg(IDAY)
c This subroutine has been incorporated into this one.
        
C Set initial parameters for daily max and min to be the temperatures for the 
c first hour of the day
        DB_DAILY_MAX = DBT_day(1)
        DB_DAILY_MIN = DBT_day(1)

                      
c Loop through the hours of the day
        DO 20 J = 1,24          ! hour of day
C analyse the hourly dry bulb temperatures for maximum and minimum values
C----calculate the daily MAX
          IF(DBT_day(J) .GT. DB_DAILY_MAX) THEN          
             DB_DAILY_MAX = DBT_day(J)
C----calculate the daily MIN
          ELSEIF(DBT_day(J) .LT. DB_DAILY_MIN) THEN
             DB_DAILY_MIN = DBT_day(J)
          ELSE 
             CONTINUE
          END IF

c For the calculation of the monthly average ambient temperatures, sum up the 
c dry bulb temperatures (DBT_day) for each 24 hour day                            
          monthly_DB_sum = monthly_DB_sum + DBT_day(J)
c---------HRTOT is a running total of the number of hours in the month.
          HRTOT = HRTOT + 1.
           
   20   CONTINUE
       
c Calculate the daily average of maximum and minimum temperatures before continuing on
c to the next day.
        DB_DAILY_AVG = ( DB_DAILY_MAX + DB_DAILY_MIN )/2.
        
C If the daily average temperature is greater than the degree day base temperature
c then add that difference to the running total of degree days.                   
        IF(DB_DAILY_AVG .GT. Base_Temp) THEN
            DEGREE_DAY_DAILY = DEGREE_DAY_DAILY 
     &                     + ( Base_Temp - DB_DAILY_AVG ) 
        ELSE        
            CONTINUE
        END IF

c If the day of the simulation falls in the next month, then calculate the
c monthly average temperature (the summed DB temperatures divided by the number of 
c hours in that month) and write to the array.         
        IF (IMONTH .NE. MONTH) THEN
            Temp_Ambient_Avg(IMONTH) = monthly_DB_sum / HRTOT
c----reset the monthly summation values
            monthly_DB_sum = 0.
            HRTOT = 0.
            MONTH = MONTH + 1
        ELSE
            CONTINUE
        END IF              
            
                        
C Continue to the next day of the year            
   10 CONTINUE

C The number of annual heating degree days (DEGREE_DAY) in the common block is 
c written as the sum of the daily degree days over the year.
      DEGREE_DAY = DEGREE_DAY_DAILY

c Write average ambient temperature to the array for the last day of the year
      Temp_Ambient_Avg(IMONTH)= monthly_DB_sum / HRTOT        
    
         
         
      return
      END SUBROUTINE CLM_DEG_DAY            
      
      
      
      
      
c *********************************************************************
c fTemp_Ambient_Avg 

c Created by: Julia Purdy
c Initial Creation Date:  September 29, 2005

c Copyright 2005: NRCan Sustainable Buildings & Communities
c INPUTS:
c     REAL DBT_day                 ambient DB temperature array (oC)
c OUTPUTS:
c     REAL Temp_Ambient_Avg     monthly average ambient DB temperature array(oC)
c                                        one value for each month of the year

c Abstract
c This subroutine returns an array of monthly average ambient dry bulb temperatures.
c The hourly ambient dry bulb temperatures from the climate file are averaged for every month.
c When the simulation reaches the next month, the average dry bulb temperature is written to
c the 12-value array Temp_Ambient_Avg
c 
c----56--1---------2---------3---------4---------5---------6---------7---------8
c
c      SUBROUTINE fTemp_Ambient_Avg(iday_number)
c      
c      IMPLICIT NONE
c      
c#include "ground_temp_mains_common.h"
c
c       REAL monthly_DB_sum          ! summation of the dry bulb temperatures for the month
c       REAL HRTOT                    ! total number of hours in the month
c       INTEGER iday_number,IDay     ! day of simulation
c       INTEGER IMonth, MONTH     ! month of simulation 
c       INTEGER J                    ! hour of the day 
      
C INITIALISE PARAMETERS
c      monthly_DB_sum = 0.
c      HRTOT = 0.     
c      MONTH = 1          
   
c Get the month for this day (I)
c EDAYR is a standard ESP-r subroutine that returns the day (IDAY) and month (IMONTH) numbers from
c the day of the simulation 
c      CALL EDAYR(iday_number,IDAY,IMONTH)

c Sum up the dry_bulb temperatures for each 24 hour day
c      DO 20 J = 1,24  
c        monthly_DB_sum = monthly_DB_sum + DBT_day(J)
c HRTOT is a running total of the number of hours in the month.
c        HRTOT = HRTOT + 1.
c   20  CONTINUE 

c If the day of the simulation falls in the next month, then calculate the
c monthly average temperature (the summed DB temperatures divided by the number of 
c hours in that month) and write to the array.         
c      IF (IMONTH .NE. MONTH) THEN
c        Temp_Ambient_Avg(IMONTH) = monthly_DB_sum / HRTOT
c----reset the monthly summation values
c         monthly_DB_sum = 0.
c         HRTOT = 0.
c         MONTH = MONTH + 1
c      ELSE
c         CONTINUE
c      END IF  
c            
c   10 CONTINUE          
     
c must write average ambient temperature to the array for the last day of the year
c      Temp_Ambient_Avg(IMONTH)= monthly_DB_sum / HRTOT        

                          
c      return     
c      END       
      
            
