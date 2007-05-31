C =============================================================================
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

C This file contains the following subroutines and function:
C Functions used to calculate the cold mains water inlet temperature.
C   fDHW_DepthAmplitudeMod()
C   fDHW_LimitMeanGroundTemp()
C   iDHW_GroundOffset()
C   fDHW_ColdMainTemp()
 
C Routines to calculate the ground temperatures based on Moore's Model:
C   Ground_Temp_Profile -  a subroutine that writes the ground temperature 
C                          parameters to a common block 
C   fTemp_Ground_Avg()
C   fTemp_Ground_Amp()
C   fTemp_Ground_phaseshift()
C   fTemp_Ambient_Amp()
C   fTemp_Ambient_phaseshift()
C   fS_1()
C   fC_1() 
 
C Subroutine used to calculate the number of heating degree days and 
C average monthly ambient dry bulb temperatures based on the chosen 
C climate file:
C   CLM_DEG_DAY
C   Daily_DBT -  a function to recover one day's dry bulb temperature data 
C                from climate file to a common block array
 
C ==========================fDHW_DepthAmplitudeMod============================
C Created by: Phylroy A. Lopez
C Initial Creation Date:  March 23th 2001
C Updated by: Julia Purdy, September 29th 2005
C Copyright 2000: NRCan Buildings Group
C----------------------------------------------------------------------------
C Abstract: 
C Returns the Depth Amplitude Modifier needed in the calculation of the mains
C water temperature. 

C References:
C SAR Engineerings LTD report on 'Domestic Hot Water Simulation Algorithms' 
C by Ken Cooper for Natural Resources Canada Section 4.2 Appendix. 
C Where AMod = 0.2+0.04*(TGMean-5.0)

C Inputs:
C   REAL fDHW_LimitMeanGroundTemp()  limit bound for ground temperature

C Output:
C   REAL fDHW_DepthAmplitudeMod()    Depth Amplitude Modifier
C ============================================================================

      FUNCTION fDHW_DepthAmplitudeMod()
     
      IMPLICIT NONE
      INCLUDE 'building.h'
      INCLUDE 'dhw_parameters.h'
      INCLUDE 'dhw_common.h'
      INCLUDE 'ground_temp_mains_common.h'

C This line was obtained from B.Bradley H2KDHW.F HOT2000 code file line 205 
C and is related to the Ken Cooper model.
      fDHW_DepthAmplitudeMod = 0.2 
     &                         + 0.04  
     &                         * ( fDHW_LimitMeanGroundTemp()
     &                         - 5.0 )
      
      return
      END

C ==========================fDHW_LimitMeanGroundTemp==========================
C Created by: Phylroy A. Lopez
C Initial Creation Date:  March 23th 2001
C Updated by: Julia Purdy, September 29th 2005
C Copyright 2000: NRCan Buildings Group
C ----------------------------------------------------------------------------
C Abstract:
C Returns GroundTemp in Limit Bounds 
C Ground Temp is rarely beyond these values.

C References:
C SAR Engineerings LTD report on 'Domestic Hot Water Simulation Algorithms' 
C by Ken Cooper for Natural Resources Canada Section 4.2 Appendix.  
C File H2KDHW.F file of HOT2000 code line 205-206

C Inputs:
C   REAL fDHW_Ground_Temp_Avg        Average ground temperature

C Outputs:
C   REAL fDHW_LimitMeanGroundTemp    Ground temperature limit bound
C===========================================================================

      FUNCTION fDHW_LimitMeanGroundTemp()
        
      IMPLICIT NONE
      INCLUDE 'building.h'
      INCLUDE 'dhw_parameters.h'
      INCLUDE 'dhw_common.h'     
      INCLUDE 'ground_temp_mains_common.h'       
              
      IF (fDHW_ground_temp_avg .lt. 5.0) THEN
        fDHW_LimitMeanGroundTemp = 5.0
      ELSEIF (fDHW_ground_temp_Avg .gt. 20.0) THEN 
        fDHW_LimitMeanGroundTemp = 20.0
      ELSE
        fDHW_LimitMeanGroundTemp = fDHW_ground_temp_avg
      END IF  ! Matches IF (fDHW_Ground_Temp_Avg .lt. 5.0)

      return  
      END

C ==========================iDHW_GroundOffset=============================
C Created by: Phylroy A. Lopez
C Initial Creation Date:  March 23th 2001
C Updated by: Julia Purdy, September 29th 2005
C Copyright 2000: NRCan Buildings Group
C ------------------------------------------------------------------------
C Abstract:
C Returns the ground temperature offset value. 

C References:
C SAR Engineerings LTD report on 'Domestic Hot Water Simulation Algorithms' 
C by Ken Cooper for Natural Resources Canada Section 4.2 Appendix.  

C Inputs:
C   REAL     fDHW_ground_temp_avg    Average Ground Temperature 

C Outputs:
C   INTEGER  iDHW_GroundOffset       Offset to account for depth temp. variance
C==========================================================================

      FUNCTION iDHW_GroundOffset()
        
      IMPLICIT NONE
      INCLUDE 'building.h'
      INCLUDE 'dhw_parameters.h'
      INCLUDE 'dhw_common.h'        
      INCLUDE 'ground_temp_mains_common.h'

C Select Offset Modifier based on Mean Ground Temperature
      IF (fDHW_ground_temp_avg .lt. 5.0) THEN
        iDHW_GroundOffset = 6
      ELSEIF (fDHW_ground_temp_avg .gt. 15.0) THEN
        iDHW_GroundOffset = 8
      ELSE 
        iDHW_GroundOffset = 7
      END IF  ! Matches IF (fDHW_ground_temp_avg .lt. 5.0)
         
      return      
      END

C ==========================fDHW_ColdMainTemp=============================
C Created by: Phylroy A. Lopez
C Initial Creation Date:  March 23th 2001
C Updated by: Julia Purdy, September 29th 2005
C Copyright 2000: NRCan Buildings Group
C-------------------------------------------------------------------------
C Abstract:
C Returns the cold main value in degrees C. This formula was obtain from 
C SAR Engineerings LTD report on 'Domestic Hot Water Simulation Algorithms' 
C by Ken Cooper for NRCan Section 4.2 Appendix

C References:
C SAR Engineerings LTD report on 'Domestic Hot Water Simulation Algorithms' 
C by Ken Cooper for Natural Resources Canada Section 4.2 Appendix.  

C Inputs:
C     INTEGER   iMonth                 month of current simulation timestep
C     REAL      fDHW_LimitMeanGroundTemp
C                                      ground temperature limit bound
C     REAL      fDHW_ground_temp_amp   Amplitude of ground temperature
C     REAL      iDHW_GroundOffset      Ground temperature offset

C Outputs:
C     REAL      fDHW_ColdMainTemp      cold water main temperature
C========================================================================

      FUNCTION fDHW_ColdMainTemp(iMonth)
      
      IMPLICIT NONE
      INCLUDE 'building.h'
      INCLUDE 'dhw_parameters.h'
      INCLUDE 'dhw_common.h'   
      INCLUDE 'ground_temp_mains_common.h'  

c First set the ground temperature average and amplitude to appropriate values
c based on flag set in subroutine DHW_READIN

c In the case that the ground parameters are to come from the dhw input file,
c the ground parameters fDHW_ground_temp_avg and fDHW_ground_temp_amp are already
c set to values entered by the user in dhw input file in subroutine DHW_READIN
      if((iDHW_ground_temp_cal_flag.eq.0).and.(iDHW_Flag.eq.1)) then          
        continue
c If the ground temperature parameters fDHW_ground_temp_avg and fDHW_ground_temp_amp
c are to be calculated internally from weather file based on Moore method, then set these
c two parameters to appropriate values from common block Ground_Temp
      elseif((iDHW_ground_temp_cal_flag.eq.1).and.(iDHW_Flag.eq.1)) then
        fDHW_ground_temp_avg = Temp_Ground_Avg
        fDHW_ground_temp_amp = Temp_Ground_Amp
c In the case that there is no dhw input file (iDHW_Flag not equal to 1) then
c the ground temperature parameters are set to appropriate values in 
c common block Ground_Temp
      elseif(iDHW_Flag.ne.1) then
        fDHW_ground_temp_avg = Temp_Ground_Avg
        fDHW_ground_temp_amp = Temp_Ground_Amp
      endif

      fDHW_ColdMainTemp =
     &     fDHW_LimitMeanGroundTemp() 
     &     + 3.0 
     &     + fDHW_DepthAmplitudeMod()
     &     * fDHW_ground_temp_amp
     &     * SIN(PI/6*(REAL(iMonth) + REAL(iDHW_GroundOffset())))

      return
      END

C ===========================Ground_Temp_Profile=========================
C Created by: Julia Purdy
C Initial Creation Date:  October 4, 2005
C -----------------------------------------------------------------------
C Abstract
C This subroutine calls 3 functions to calculate the ground temperature 
C profile based on the Moore Method. These 3 functions are:
C fTemp_Ground_Avg - returns the average ground temperature
C fTemp_Ground_Amp - returns the amplitude of ground-temperature's annual 
C sine wave.
C fTemp_Ground_phaseshift - returns the phase lag of ground-temperature's 
C annual sine wave.

C References:
C Report by Glenn Moore for R-2000 Homes Program for Energy, Mines, and 
C Resources Canada. Report Title: "Soil temperature estimation method".
C July 7th 1986

C Outputs:
C       REAL  Temp_Ground_Avg           !annually-averaged soil temperature.
C       REAL  Temp_Ground_Amp           !amplitude of ground-temperature's annual sine wave.
C       REAL  Temp_Ground_phaseshift    !phase lag of ground-temperature's annual sine wave.      
 
C These variables are then stored in the common block 
C COMMON/Ground_Temp/Temp_Ground_Avg,Temp_Ground_Amp,Temp_Ground_phaseshift
C for use in various calculations (including basesimp and mains water 
C temperature calculation)
C =======================================================================

      SUBROUTINE Ground_Temp_Profile      
        
      IMPLICIT NONE
      INCLUDE 'building.h'
      INCLUDE 'ground_temp_mains_common.h' 

C Call to the function fTemp_Ground_Avg to calculate the average ground 
C temperature.       
      Temp_Ground_Avg = fTemp_Ground_Avg()
       
C Call to the function fTemp_Ground_Amp to calculate the amplitude of 
C ground-temperature's annual sine wave.       
      Temp_Ground_Amp = fTemp_Ground_Amp()
       
C Call to the function fTemp_Ground_phaseshift to calculate the phaseshift 
C of ground-temperature's annual sine wave.       
      Temp_Ground_phaseshift = fTemp_Ground_phaseshift()
 
      return
      END

C============================== fTemp_Ground_Avg=============================
C Created by: Julia Purdy
C Initial Creation Date:  September 29, 2005
C----------------------------------------------------------------------------
C Abstract:
C Returns the average ground temperature. To calculate the annual average ground 
C temperature requires the annual average ambient temperature. The annual 
C average temperature is determined by summing the monthly average ambient 
C temperatures and dividing by the number of months considered. 

C References:
C Report by Glenn Moore for R-2000 Homes Program for Energy, Mines, and 
C Resources Canada. Report Title: "Soil temperature estimation method".
C July 7th 1986

C Inputs:
C       REAL Temp_Ambient_Avg     monthly average ambient DB temperature array(oC)
C                                 one value for each month of the year
C       REAL DEGREE_DAY           annual heating degree days

C Outputs:
C       REAL fTemp_Ground_Avg     average ground temperature (oC)
C============================================================================

      FUNCTION fTemp_Ground_Avg()
      
      IMPLICIT NONE
      INCLUDE 'building.h'      
      INCLUDE 'ground_temp_mains_common.h'

      REAL Temp_Ambient_Avg_sum      ! sum of monthly average ambient DB temps
      REAL fAnnual_ambient_avg       ! average of the annual ambient DB temps
      REAL nmonth                    ! sum the number of months in the year
      INTEGER i

C set initial conditions
      Temp_Ambient_Avg_sum = 0.0
      nmonth = 0.0

C Calculate the annual average ambient temperature. Sum the monthly average
C ambient temperatures and divide by the number of months considered. 
      do i = 1,12
C -----Sum the monthly average ambient temperatures
        Temp_Ambient_Avg_sum = Temp_Ambient_Avg_sum 
     &                        + Temp_Ambient_Avg(i)
C -----sum the number of months in the year
        nmonth = nmonth + 1.0
      end do
    
C Annual average ambient temperature
      fAnnual_ambient_avg = Temp_Ambient_Avg_sum / nmonth

C Average ground temperature. Equation 15 in Moore's report
      fTemp_Ground_Avg = fAnnual_ambient_avg                 
     &                 - 1.438 
     &                 + ( 9.189E-4 * DEGREE_DAY )

      return     
      END
      
C==============================fTemp_Ground_Amp===============================
C Created by: Julia Purdy
C Initial Creation Date:  September 29, 2005
C-----------------------------------------------------------------------------
C Abstract:
C Returns the amplitude of ground-temperature's annual sine wave.

C References:
C Report by Glenn Moore for R-2000 Homes Program for Energy, Mines, and 
C Resources Canada. Report Title: "Soil temperature estimation method".
C July 7th 1986

C Inputs:
C       REAL fTemp_Ambient_Amp        temperature amplitude of ambient air (oC)
C       REAL DEGREE_DAY               annual heating degree days

C Outputs:
C       REAL fTemp_Ground_Amp         amplitude of ground-temperature sine wave (oC)
C=============================================================================

      FUNCTION fTemp_Ground_Amp()
      
      IMPLICIT NONE
      INCLUDE 'building.h'      
      INCLUDE 'ground_temp_mains_common.h'

C Find amplitude of ground temperature profile. Equation 16 in Moore's report
      fTemp_Ground_Amp = fTemp_Ambient_Amp()
     &                 - 7.875 
     &                 + ( 1.97E-3 * DEGREE_DAY )

      return
      END      

C============================fTemp_Ground_phaseshift======================== 
C Created by: Julia Purdy
C Initial Creation Date:  September 29, 2005
C---------------------------------------------------------------------------
C Abstract:
C Calculates the phase lag of ground-temperature's annual sine wave.
C This Formula was obtained originally from Moore Method

C References:
C Report by Glenn Moore for R-2000 Homes Program for Energy, Mines, and 
C Resources Canada. Report Title: "Soil temperature estimation method".
C July 7th 1986

C Inputs:
C     REAL fTemp_Ambient_phaseshift     phase shift of ambient air
C     REAL DEGREE_DAY                   annual heating degree days

C Outputs:
C     REAL fTemp_Ground_phaseshift      phase lag of ground-temperature sine wave
C===========================================================================

      FUNCTION fTemp_Ground_phaseshift()
      
      IMPLICIT NONE
      INCLUDE 'building.h'      
      INCLUDE 'ground_temp_mains_common.h' 

C Find ground temperature phase shift. Equation 17 in Moore's report
      fTemp_Ground_phaseshift = fTemp_Ambient_phaseshift()
     &                        - 0.0756 
     &                        + ( 2.128E-5 * DEGREE_DAY )

      return
      END

C=========================fTemp_Ambient_Amp========================== 
C Created by: Julia Purdy
C Initial Creation Date:  October 4, 2005
C--------------------------------------------------------------------
C Abstract:
C Returns the amplitude of ambient-temperature's annual sine wave.

C This formula was adopted from the Moore Method

C References:
C Report by Glenn Moore for R-2000 Homes Program for Energy, Mines, and 
C Resources Canada. Report Title: "Soil temperature estimation method".
C July 7th 1986

C Inputs:
C     REAL fS_1                    cosine curve-fit coefficient          
C     REAL fC_1                    cosine curve-fit coefficient

C Outputs:
C     REAL fTemp_Ambient_Amp()     amplitude of ambient-temperature sine wave (oC)
C=====================================================================

      FUNCTION fTemp_Ambient_Amp()
      
      IMPLICIT NONE
      INCLUDE 'building.h'      
      INCLUDE 'ground_temp_mains_common.h'
    
C This equation calls two functions fS_1 and fC_1 which calculate the coefficients
C to fit the monthly cosine curve for ground temperature. Equation contained in
C Appendix A of Moore's report.
               
      fTemp_Ambient_Amp = (sqrt((fS_1())**2 + (fC_1())**2))/6

      return
      END      
        
C=========================fTemp_Ambient_phaseshift()=========================== 
C Created by: Julia Purdy
C Initial Creation Date:  October 4, 2005
C------------------------------------------------------------------------------
C Abstract:
C Returns the phase-shift of the ambient-temperature's annual sine wave.

C References:
C Report by Glenn Moore for R-2000 Homes Program for Energy, Mines, and 
C Resources Canada. Report Title: "Soil temperature estimation method".
C July 7th 1986

C Inputs:
C     REAL fS_1                       curve-fit coefficient          
C     REAL fC_1                       curve-fit coefficient

C Outputs:
C     REAL fTemp_Ambient_phaseshift() phaseshift of the ambient-temperature sine wave
C==============================================================================
      FUNCTION fTemp_Ambient_phaseshift()
      
      IMPLICIT NONE
      INCLUDE 'building.h'      
      INCLUDE 'ground_temp_mains_common.h'

C Equation in Appendix A of Moore's report
      fTemp_Ambient_phaseshift =  atan((fS_1())/(fC_1()))

      return
      END
            
C==============================fS_1========================================== 
C Created by: Julia Purdy
C Initial Creation Date:  October 4, 2005
C----------------------------------------------------------------------------
C Abstract:
C Calculates the coefficient fS_1 - which is used to fit the monthly data
C to a cosine curve for ground temperature profile.

C References:
C Report by Glenn Moore for R-2000 Homes Program for Energy, Mines, and 
C Resources Canada. Report Title: "Soil temperature estimation method".
C July 7th 1986

C Inputs:
C     REAL Temp_Ambient_Avg(12)     Monthly average ambient air temperatures

C Outputs:
C     REAL fS_1
C===========================================================================
      
      FUNCTION fS_1()
      
      IMPLICIT NONE
      INCLUDE 'building.h'      
      INCLUDE 'dhw_parameters.h'
      INCLUDE 'ground_temp_mains_common.h'

      INTEGER i

      fS_1 = 0.0
      do i = 1, 12          ! for each month of the year
C Equation in Appenx A in Moore's report
        fS_1 = fS_1 
     &          +  Temp_Ambient_Avg(i) 
     &          * sin ((( 2.0 * PI ) / 12.0 )* ( real(i) - 0.5 ))
      end do

      return
      END            
      
      
C====================================fC_1=================================== 
C Created by: Julia Purdy
C Initial Creation Date:  October 4, 2005
C---------------------------------------------------------------------------
C Abstract:
C Calculates the coefficient fC_1 - which is used to fit the monthly data
C to a cosine curve for ground temperature profile.
C 
C Inputs:
C       REAL Temp_Ambient_Avg()     Average monthly ambient temperature

C Outputs:
C       REAL fC_1
C===========================================================================
      
      FUNCTION fC_1()
      
      IMPLICIT NONE
      INCLUDE 'building.h'      
      INCLUDE 'ground_temp_mains_common.h'
      INCLUDE 'dhw_parameters.h'

      INTEGER i     

      fC_1 = 0.0
      do i = 1, 12
C Equation in Appendix A of Moore's report
        fC_1 = fC_1
     &         + Temp_Ambient_Avg(i) 
     &         * cos ((( 2.0 * PI ) / 12.0 )* ( i - 0.5 ))
      end do
      
      return
      END
 
C=============================Daily_DBT===================================
C Created by: Julia Purdy
C Initial Creation Date:  October 4, 2005
C-------------------------------------------------------------------------
C Abstract:
C Recover one day's data from climate file into common block for use.
C This subroutine reads in the climate information for one day and then saves the 
C dry bulb temperatures to a common block for use in other subroutines.
C The appropriate weather file has already been opened in the calling subroutin

C Inputs:
C     INTEGER  iday_number        current day number, passed in the called statement

C Outputs:
C     REAL     DBT_day(24)        a 24-hourly array of dry bulb temperatures
C==========================================================================
         
      SUBROUTINE Daily_DBT(iday_number)

      IMPLICIT NONE
      INCLUDE 'building.h'           
      INCLUDE 'ground_temp_mains_common.h'
      INCLUDE 'esprdbfile.h'
C esprdbfile.h supplies the following:
C ICLIM,LCLIM (for climate file)

      INTEGER j     ! hour counter, 1-24
      INTEGER irec, iday_number
      INTEGER istat,iostat
C climatic parameters 
      INTEGER idif(24)     ! diffuse radiation
      INTEGER itmp(24)     ! outdoor dry bulb temperature
      INTEGER idnr(24)     ! direct normal radiation
      INTEGER ivel(24)     ! wind speed
      INTEGER idir(24)     ! wind direction
      INTEGER ihum(24)     ! relative humidity
      REAL rec
        
C read in temperature values for present day
C note: the climate file has already been opened - from the calling subroutine
      irec=iday_number
      read(iClim,rec=irec,iostat=istat,err=1000)
     &    (idif(j),itmp(j),idnr(j),ivel(j),idir(j),ihum(j),j=1,24)

C A new common block is created for storing the daily dry bulb temperatures
C common/daily_dry_bulb/DBT_day. 
C This common block is stored in cetc/ground_temp_mains_common.h
      do 10 j = 1,24
        DBT_day(j) = real(itmp(j)) / 10.
 10   continue

 1000 continue         ! this needs to be an error message
                    
      RETURN
      END
       
C==============================CLM_DEG_DAY====================================
C Created by: Julia Purdy
C Initial Creation Date:  October 3, 2005
C-----------------------------------------------------------------------------
C Abstract:
C Main subroutine for the calculation of the number of annual heating degree
C days and average monthly ambient dry bulb temperatures.
C The calculation of the number of annual degree days (DD)is as follows:
C For every day, the maximum and minimum temperatures are defined.
C The average of the daily max & min temperatures is then calculated as (max+min)/2.
C If this average is larger than the base temperature (hard coded to 18.2oC) then the number of 
C degree days for that day is zero. if this average is less than the base, then the difference 
C is the number of degree days for that day.
C These daily degree days are summed for the entire year.

C This subroutine also returns an array of monthly average ambient dry bulb temperatures.
C The hourly ambient dry bulb temperatures from the climate file are averaged for every month.
C When the simulation reaches the next month, the average dry bulb temperature is written to
C the 12-value array Temp_Ambient_Avg

C This subroutine is called once per simulation, or once per climate file in the case of 
C multi-year simulations

C Copyright 2005: NRCan Sustainable Buildings & Communities
C Outputs:
C     REAL DEGREE_DAY             number of heating degree days for specified location
C     REAL Temp_Ambient_Avg       monthly average ambient DB temperature array(oC)
C                                 one value for each month of the year
C=============================================================================

      SUBROUTINE CLM_DEG_DAY

      IMPLICIT NONE
      INCLUDE 'building.h'
      INCLUDE 'ground_temp_mains_common.h'   
       
       INTEGER I,J                         ! counters
       REAL Base_Temp                      ! degree day calculation base temperature
       REAL DB_DAILY_MAX, DB_DAILY_MIN     ! daily max and min dry bulb temperatures
       REAL DB_DAILY_AVG                   ! average of max and min dry bulb temperature
       REAL DEGREE_DAY_DAILY               ! daily summation of degree days
       REAL HRTOT                          ! total number of hours in the month
       REAL monthly_DB_sum                 ! summation of the dry bulb temperatures for the month
       INTEGER iday_number,IDay            ! day of simulation
       INTEGER IMonth, MONTH               ! month of simulation

C Degree day calculations are derived from a specific base temperature
C This base temperature is set to 18.2 deg C. 
      Base_Temp = 18.2     ! degree day calculation base temperature

C Initialize parameters for monthly average temperature calculation. As this subroutine is called
C only once per simulation or once per climate file in the case of multi-year simulations, we can
C initialize these parameters within the subroutine.
C Sum for dry-bulb temperature
      monthly_DB_sum = 0.
C Number of hours in the month
      HRTOT = 0.     
C Counter to keep track on month number
      MONTH = 1          
C Sum of the degree days for the year
      DEGREE_DAY_DAILY = 0.
     
C For each day of the year, get climate data (in a 24 hour array) and calculate the maximum
C and minimum values.
      DO 10 I = 1,365     
C-----We do not use the simulation start and end dates here (IDS & IDF) because this 
C-----calculation must be performed over the entire year - and the simulation may only be 
C-----performed for a fraction of that. 

C Get the month for this day (I)
C EDAYR is a standard ESP-r subroutine that returns the day (IDAY) and month (IMONTH) numbers 
C from the day of the simulation 
        CALL EDAYR(I,IDAY,IMONTH)

C If the day of the simulation falls in the next month, then calculate the
C monthly average temperature (the summed DB temperatures divided by the number of 
C hours in that month) and write to the array.         
        IF (IMONTH .NE. MONTH) THEN
            Temp_Ambient_Avg(MONTH) = monthly_DB_sum / HRTOT
C----reset the monthly summation values
            monthly_DB_sum = 0.
            HRTOT = 0.
            MONTH = MONTH + 1
        ELSE
            CONTINUE
        END IF          

C READ VALUES OF THE CLIMATIC PARAMETERS FOR THIS DAY.
C This call to Daily_DBT returns an array of 24 hourly values of the
C external dry bulb temperature (DBT_day) for the day I.               
        CALL Daily_DBT(I)     
C DBT_day is an hourly array of dry bulb temps stored in the common block
C common/daily_dry_bulb/DBT_day

C Set initial parameters for daily max and min to be the temperatures for the 
C first hour of the day
        DB_DAILY_MAX = DBT_day(1)
        DB_DAILY_MIN = DBT_day(1)

C Loop through the hours of the day
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

C For the calculation of the monthly average ambient temperatures, sum up the 
C dry bulb temperatures (DBT_day) for each 24 hour day                            
          monthly_DB_sum = monthly_DB_sum + DBT_day(J)
C---------HRTOT is a running total of the number of hours in the month.
          HRTOT = HRTOT + 1.
   20   CONTINUE
       
C Calculate the daily average of maximum and minimum temperatures before continuing on
C to the next day.
        DB_DAILY_AVG = ( DB_DAILY_MAX + DB_DAILY_MIN )/2.

C If the daily average temperature is less than the degree day base temperature
C then add that difference to the running total of degree days.                   
        IF(DB_DAILY_AVG .LT. Base_Temp) THEN
            DEGREE_DAY_DAILY = DEGREE_DAY_DAILY 
     &                     + ( Base_Temp - DB_DAILY_AVG ) 
        ELSE        
            CONTINUE
        END IF
                
C Continue to the next day of the year            
   10 CONTINUE

C The number of annual heating degree days (DEGREE_DAY) in the common block is 
C written as the sum of the daily degree days over the year.
      DEGREE_DAY = DEGREE_DAY_DAILY

C Write average ambient temperature to the array for the last day of the year
      Temp_Ambient_Avg(IMONTH)= monthly_DB_sum / HRTOT 
       
      return
      END ! of CLM_DEG_DAY
      
