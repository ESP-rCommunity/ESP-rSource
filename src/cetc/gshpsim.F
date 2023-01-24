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
C   GSHP_HEAT_COEFF:    set the degradation coeff (Cd) for the part-load
c                       performance of the GSHP in heating mode
C   GSHP_COOL_COEFF:    set the Cd coeff for the part-load performance of 
c                       the GSHP in cooling mode
C   GSHP_COEFF:         set the correlation coefficients for the part-load
c                       performance of the GSHP
c   GSHP_load:          calculate the dailyload on the GSHP system
C   GSHPSIM:            main simulation subroutine. 
C   GSHP_HEATXV1:       simulation routine for vertical GSHP system
C   GSHP_HEATXHS:       simulation routine for horizontal 2-pipe GSHP system
C   GSHP_HEATXH4:       simulation routine for horizontal 4-pipe GSHP system
C   GSHP_HEATXSL:       simulation routine for horizontal slinky GSHP system
c   GSHP_COP:           determines the heat pump COP at operating conditions
c   GSHP_CAP:           determines the heat pump operating capacity
c   GSHP_pump_power:    determines the pump power 
c   weighted_GSHP_cop:  determines the daily load-weighted average GSHP COP
c   daily_GSHP_COP:     determines the average daily ground source heat pump COP
c
C *********************************************************************
C *********************************************************************
C GSHP_HEAT_COEFF

c Created by: Julia Purdy
c Initial Creation Date: March 4, 2002
c Copyright 2000: NRCan Buildings Group

c Subroutine to set the degradation coefficients of a ground-source heat pump
c when a heating load is placed upon it.

c Reference:
c     Water Source Heat Pump Data for HOT3000 - Milestone #1
c     Caneta Research Inc., December 7, 2001

c INPUTS:
c    isys      number of HVAC system
c    EWT       daily entering water temperature.
   
c OUTPUTS:
c     Cd       degradation coefficient

c **********************************************************************
       SUBROUTINE GSHP_HEAT_COEFF(isys)

c Daily ground heating load and entering water temperature data

c Use heat pump input data module (contains ahp, bhp, chp, dhp values)


       IMPLICIT NONE
c Use HVAC input data module (contains isys information)
#include "building.h"
#include "hvac_parameters.h"
#include "ashp_common.h"
#include "hvac_common.h"
#include "gshp_common.h"
       INTEGER  isys
       REAL  Cd, degrad_coeff

c The degradation coefficient, Cd, is a function of entering water temperature (EWT).
c The relationship between Cd and EWT is given in Figure 1 and 2 of the Caneta report,
c referenced above.
c In the report, the relationship is given in terms of EWT in degrees F, i.e., for 
c heating: Cd = -0.002069 * EWT + 0.271867. These relationships were modified 
c to allow the EWT to be input in degrees Celcius.
       Cd(isys) = -0.0037242 * EWT + 0.205659

C This temporary variable is used for the call the the GSHP_COEFF subroutine.
       degrad_coeff = Cd(isys)

c The following subroutine calculates the correlation coefficients based on Cd.  
       CALL GSHP_COEFF(degrad_coeff,isys)

       return
       end


C *********************************************************************
C *********************************************************************
C GSHP_COOL_COEFF

c Created by: Julia Purdy
c Initial Creation Date: March 4, 2002
c Copyright 2000: NRCan Buildings Group
c
c Subroutine to set the degradation coefficients of a ground-source heat pump
c system when a cooling load is placed upon it.

c Reference:
c     Water Source Heat Pump Data for HOT3000 - Milestone #1
c     Caneta Research Inc., December 7, 2001

c INPUTS:
c    isys      number of HVAC system
c    EWT       entering water temperature
   
c OUTPUTS:
c     Cd       degradation coefficient

c **********************************************************************
      SUBROUTINE GSHP_COOL_COEFF(isys)

c Daily ground cooling load and entering water temperature data

      IMPLICIT NONE
#include "building.h"
#include "hvac_parameters.h"
#include "ashp_common.h"
#include "gshp_common.h"
      INTEGER  isys
      REAL  Cd, degrad_coeff

c The degradation coefficient, Cd, is a function of entering water temperature (EWT).
c The relationship between Cd and EWT is given in Figure 1 and 2 of the Caneta report,
c referenced above.
c In the report, the relationship is given in terms of EWT in degrees F, i.e.,
c Cd = 0.000949 * EWT + 0.083088. These relationships were modified to allow
c input of EWT in degrees Celcius.

c For GSHP in cooling mode:
       Cd(isys) = 0.0017082 * EWT + 0.113456

C This temporary variable is used for the call the the GSHP_COEFF subroutine.
       degrad_coeff = Cd(isys)

c The following subroutine calculates the correlation coefficients based on Cd.
       CALL GSHP_COEFF(degrad_coeff,isys)

       return
       end


C *********************************************************************
C *********************************************************************
C GSHP_COEFF

c Created by: Julia Purdy
c Initial Creation Date: March 4, 2002
c Copyright 2000: NRCan Buildings Group
c
c Subroutine to set the correlation coefficients of a ground-source heat pump
c for a specified degradation coefficient.

c Reference:
c     Water Source Heat Pump Data for HOT3000 - Milestone #1
c     Caneta Research Inc., December 7, 2001

c INPUTS:
c    isys      number of HVAC system
c    Cd        degradation coefficient
   
c OUTPUTS:
c    ahp       coefficients for the heat pump part-load performance
c    bhp
c    chp
c    dhp

c **********************************************************************
      SUBROUTINE GSHP_COEFF(degrad_coeff,isys)

c Use heat pump input data module (contains ahp, bhp, chp, dhp values)


       IMPLICIT NONE
#include "building.h"
#include "hvac_parameters.h"
#include "hvac_common.h"
#include "ashp_common.h"

       INTEGER isys
       REAL Cd,degrad_coeff

c The coefficients for the part-load performance of the ground source
c heat pump system are based on the Caneta
c report referenced in the heading of this subroutine. These coefficients
c are dependent on the degradation coefficient, Cd. They range from Cd = 0.05 to
c Cd = 0.30. The following coefficients are taken from Table 1 of the report.

c The local variable for the degradation coefficient, Cd, for the system isys 
c is set to the the value brought from the GSHP_HEAT_COEFF and GSHP_COOL_COEFF
c subroutines.
       Cd(isys) = degrad_coeff

c When the degradation coefficent is less than or equal to 0.05, the following
c correlation coefficients will be used.      
       if(Cd(isys) .le. 0.05) then
         ahp(isys) = 2.73404E-6
         bhp(isys) = 1.05259
         chp(isys) = -0.0552087
         dhp(isys) = 0.00262236
c When the degradation coefficent is between 0.05 and 0.1, the following
c correlation coefficients will be used.   
       elseif((0.05 .lt. Cd(isys)) .and.  (Cd(isys).le. 0.10)) then
         ahp(isys) = 1.48147E-5
         bhp(isys) = 1.11079
         chp(isys) = -0.121905
         dhp(isys) = 0.0111199
c When the degradation coefficent is between 0.1 and 0.15, the following
c correlation coefficients will be used.
       elseif((0.1 .lt. Cd(isys)) .and. (Cd(isys) .le. 0.15)) then
         ahp(isys) = 6.25583E-5
         bhp(isys) = 1.17517
         chp(isys) = -0.201513
         dhp(isys) = 0.0263344

c When the degradation coefficent is between 0.15 and 0.2, the following
c correlation coefficients will be used.
       elseif((0.15 .lt. Cd(isys)) .and. (Cd(isys) .le. 0.20)) then
         ahp(isys) = 0.000164298
         bhp(isys) = 1.24656
         chp(isys) = -0.29607
         dhp(isys) = 0.0494917

c When the degradation coefficent is between 0.2 and 0.25, the following
c correlation coefficients will be used.
       elseif((0.2 .lt. Cd(isys)).and. (Cd(isys) .le. 0.25)) then
         ahp(isys) = 0.000362523
         bhp(isys) = 1.32573
         chp(isys) = -0.407603
         dhp(isys) = 0.0818194

c When the degradation coefficent is greater than 0.25, the following
c correlation coefficients will be used. 
       elseif(Cd(isys).gt. 0.25) then
         ahp(isys) = 0.000698967
         bhp(isys) = 1.41373
         chp(isys) = -0.538881
         dhp(isys) = 0.125027
       endif

       return
       end



c *********************************************************************
c *********************************************************************
c GSHP_load

c Created by: Julia Purdy
c Initial Creation Date: January 9, 2002
c Copyright 2000: NRCan Buildings Group

c This subroutine sums the heating and cooling building loads for the 
c simulation of the GSHP system. This subroutine is called from HVACSIM 
c once every simulation timestep, once the zone loads are calculated.
c The daily heating or cooling loads are passed to the main
c GSHP simulation subroutine GSHPSIM, via the GSHP_DAILY module, where they are 
c converted to ground loads.
c
c INPUTS:
c     heating_load       heating load of zones served by GSHP system, per timestep
c     cooling_load       cooling load of zones served by GSHP system, per timestep 
C
c OUTPUTS:
c     daily_heat_load    daily building heating load
c     daily_cool_load    daily building cooling load
c ***********************************************************************
      SUBROUTINE GSHP_load(heating_load,cooling_load)

      IMPLICIT NONE
c Definition of module for daily loads and EWT. 
C The heating and cooling load will be read in each timestep.
C Initiates reading of global variables (common for all)
#include "building.h"
#include "hvac_parameters.h"
#include "gshp_common.h"

      COMMON/PERS/ISD1,ISM1,ISD2,ISM2,ISDS,ISDF,NTSTEP

C Defining variables used in the subroutine   
      REAL heating_load, cooling_load
      INTEGER ISD1,ISM1,ISD2,ISM2,ISDS,ISDF,NTSTEP


c The total heating or cooling load on the GSHP system each timestep has been determined 
c in the HVACSIM subroutine. The daily load on the ground heat exchanger will be the 
c sum of the timestep loads over the simulation day - calculated below.

c If there is a heating load, add the previous timesteps heating load (daily_heat_load)
c to the current timestep heating load (heating_load) - as calculated in HVACSIM.
       if (heating_load .ge. 0) then 
          daily_heat_load = daily_heat_load + heating_load/NTSTEP
       endif
c If there is a cooling load, add the previous timesteps cooling load (daily_cool_load)
c to the current timestep cooling load (cooling_load) - as calculated in HVACSIM.
       if (cooling_load .ge. 0) then
          daily_cool_load = daily_cool_load + cooling_load/NTSTEP
       endif

c This subroutine is called every timestep from HVACSIM, whereas the ground loop is only
c simulated once per day. As such, when the ground loop uses daily_heat/cool_load, these
c values will be the sum of the time step loads over the day.

       return
       end


C *********************************************************************
C *********************************************************************
C GSHPSIM

C Created by: Julia Purdy
c Initial Creation Date: January 7, 2002
c Copyright 2000: NRCan Buildings Group

c This is the controlling routine for the simulation of the GSHP system. 
c This subroutine is called from MZNUMA once every day. 
C (within the 'do 10' loop.)
C The appropriate simulation subroutine is called based on the GSHP 
C system type defined in the .gshp file.

c The purpose of this subroutine is to set the daily summed heating load 
c and cooling load values equal to the local array dQ used in the Caneta subroutines.

C INPUTS:
C      igshp_type       type of GSHP system selected in the input file
C 
C OUTPUTS:
C      call to the GSHP system-specific calculation subroutine.
C
C **********************************************************************

      SUBROUTINE GSHPSIM


      IMPLICIT NONE
#include "building.h"
C Initiates reading of global variables (common for all)
#include "hvac_parameters.h"
#include "gshp_common.h"

      COMMON/OUTIN/IUOUT,IUIN,IEOUT
      INTEGER IUOUT,IUIN,IEOUT

      common/gshpinfo/igshp
      common/gshpfile/gshpfile

      INTEGER igshp, iPeriod
      character gshpfile*72

C Local Variables

      REAL COP
      REAL COP_HEAT
      REAL COP_COOL
C External Functions
      REAL daily_GSHP_COP

c Call a subroutine to initialize ground loop variables, including the entering
c water temperature and the simulation period and start and end days.
       CALL GSHP_init

c This value will be increased by 1 each day of the simulation. The variable
c Day is the current simulation day (conversation with Andrew Morrison of Caneta).
c This variable is initialized in the GSHP_init subroutine
       Day = Day + TimeDay
       iPeriod = Day


c If the heat pump is under a heating load, then the compressor waste heat displaces
c heat that must be extracted from the ground.
c If the heat pump is under a cooling load, then the compressor waste heat must be
c rejected into the ground along with the heat removed from the space and dQ
c will be a function of daily_cool_load.

c Note that because dQ is the ground load, then a heating load is negative, whereas
c a cooling load is positive. In addition, the loads are in kWh, so the daily building
c heating and cooling loads (in Wh) are converted accordingly.

c As per a conversation with Andrew Morrison of Caneta, the heat pump can operate in 
c both heating and cooling mode in the same seasaon. For days where there is a heating
c load and no cooling load, dQ will be a function of daily_heat_load and COP.

       if (GSHP_COP .eq. 0.0 ) then
         WRITE(IUOUT,*) ' Error GSHPSIM: COP is zero '
         return
       else

c If both the daily heating and the daily cooling loads are zero, then the ground 
c load for that day will be zero.
         if (daily_heat_load .eq. 0.0 .and. 
     &       daily_cool_load .eq. 0.0) then
            dQ(iPeriod) = 0.0

         elseif (daily_heat_load .gt. 0.0 .and. 
     &           daily_cool_load .eq. 0.0) then
c Calculate the daily heating COP. The coefficient of performance
c is calculated every timestep from the heat pump calculation. This value has been 
c load averaged for the ground loop calculation.

           COP = daily_GSHP_COP(load_weighted_GSHP_COP,daily_heat_load)

           dQ(iPeriod) = - daily_heat_load/1000. * (1 - 1/COP)

c For days where there is a cooling load and no heating load, dQ
c will be a function of daily_cool_load and COP.
         elseif (daily_cool_load .gt. 0.0 .and.
     &           daily_heat_load .eq. 0.0) then
c Calculate the daily cooling COP. The coefficient of performance
c is calculated every timestep for the heat pump calculation. This value will be 
c load averaged for the ground loop calculation, so we will not account for the
c COP when the load is zero.
             COP = daily_GSHP_COP(load_weighted_GSHP_COP,
     &                            daily_cool_load)   
             dQ(iPeriod) = daily_cool_load/1000. * (1 + 1/COP) 

         else
c For days where there are both heating and cooling loads and no cooling load, dQ
c will be a function of both daily_heat_load, daily_cool_load, and COP.

c Calculate the daily heating and  cooling COP. These values will be 
c load averaged for the ground loop calculation, so we will not account for the
c COP when the load is zero.
             COP_heat = daily_GSHP_COP(load_weighted_GSHP_cop,
     &                                 daily_heat_load)
             COP_cool = daily_GSHP_COP(load_weighted_GSHP_cop,
     &                                 daily_cool_load)

             dQ(iPeriod) = - daily_heat_load/1000. * (1 - 1/COP_heat) +
     &                     daily_cool_load/1000. * (1 + 1/COP_cool)
         endif 
       endif 
 
c If the system is a vertical single U-tube, call V1_INPUT
        if(igshp_type .eq.1 ) then
           call GSHP_HEATXV1(iPeriod)
c If the system is a horizontal 4 pipe 2x2 arrangement, call H4_INPUT
        elseif(igshp_type .eq. 2) then
           call GSHP_HEATXH4(iPeriod)
c If the system is horizontal 2 pipe side-by-side arrangement, call HS_INPUT
        elseif(igshp_type .eq. 3) then
           call GSHP_HEATXHS(iPeriod)
c If the system is a horizontal slinky, call SL_INPUT
        elseif(igshp_type .eq. 4) then
           call GSHP_HEATXSL(iPeriod)
        else
           WRITE(IUOUT,*) ' Error GSHPSIM: incorrect refernce number '
           WRITE(IUOUT,*) ' for the GSHP system type ',igshp_type
           return
        endif   

c Once the ground loop is completed, set COP as well as the heating and cooling loads
c to zero, so that the next day's sum will begin at zero.
       load_weighted_GSHP_COP = 0.0
       daily_heat_load = 0.0
       daily_cool_load = 0.0

       RETURN        
       END


c *********************************************************************
c *********************************************************************
c GSHP_HEATXV1
c                                                
c Developer: A.Morrison / M.Choy - Caneta Research Inc.                  
C Deliverable Date: November 27, 2001 - To JP of NRCan(MC)      
C Date of Last Revision: December 03, 2001 - IMPLICIT NONE incl.stat. (MC)
c Updated: Julia Purdy - CETC, April 4, 2002
c                                    
c DESCRIPTION:
c  This subroutine calculates the entering water temperature (EWT) to a ground-
c  source heat pump (GSHP) for a vertical ground heat exchanger. The ground,
c  regional weather and heat exchanger properties are passed from the main
c  program calling the subroutine. EWTs are returned.

c REFERENCE:
c  DESIGN OF VERTICAL GROUND SOURCE HEAT EXCHANGER - 1 U-TUBE/BOREHOLE 
c  Hart, David P. and Couvillion, Rick (1986) 'Earth-Coupled Heat Transfer
c
c INPUTS:
c    GSHP_INPUT  input file specific to the GSHP type  
c    dQ          daily heating load
c
c OUTPUTS:
c    EWT         Entering water temperature
C
c *********************************************************************
       SUBROUTINE GSHP_HEATXV1(iPeriod)


C      USE GSHP_INPUT
C      USE GSHP_DAILY
C Module variables specific to vertical heat exchangers.
C      USE GSHP_VERTICAL_INPUT

       IMPLICIT NONE
#include "building.h"
#include "hvac_parameters.h"
#include "gshp_common.h"
#include "gshp_v1_common.h"
#include "ground_temp_mains_common.h"


C Common for read and write statements
      COMMON/OUTIN/IUOUT,IUIN,IEOUT

C ----------------------  Definition of Data Types --------------------

      INTEGER IUOUT,IUIN,IEOUT
      
      ! Pre-initialized Variables 
      REAL timej                                   ![second]
      REAL Depth_cum, Dmean                        ![m],[m] 

      ! Calculated Variables
      REAL Rpipe, Rgrout, Dsoil, Ksoil, Rinf       ![mC/W].[mC/W],[m2/s],[W/mC],[m]
      REAL RS, sum, Lm, LMLSHE, LMLST, LMLS, X, Y  ![mC/W],[W/C],[m],[-],[-],[-]
      REAL Wattj, Wattj_1, dTtot, dTinc            ![W],[W],[C],[C]
      REAL kWj, a, b, dTinf                        ![kW],[units],[units],[C]
      REAL Tfluid, ODeq                            ![C],[m]

      ! Function Variables       
      REAL SINACOS, PWRFOR, PI                              
      INTEGER jPer, iPeriod, jPeriod, m, iTime, jTime
      REAL temp
      REAL gshp_ground_temp_avg,gshp_ground_temp_amp

C ----------------------  Variable Definitions ------------------------


C -----      Variable Initilizations
      PI = ACOS(-1.0)
        
      Depth_cum = 0                  ! Depth accumulation (m)
      Dmean = 0


C ---------------------------  Main Program  --------------------------
      
C ----- Determine mean diffusivity for given bore depth Dbottom         

      DO 107 m = 1, Nsoil
         IF (m .EQ. Nsoil) then 
            Depth(m) = Dbottom - Depth_cum - Dtop      
            IF (Depth(m) .LT. 0.0) then 
               Depth(m) = 0.0
            ENDIF
         ENDIF
         Depth_cum = Depth_cum + Depth(m)
         Dmean = Dmean + (Depth(m) * (0.75 * Dw(m) + 0.25 * Ds(m))
     &         / (2 * (Dbottom - Dtop)))

 107  CONTINUE      ! End of Nsoil LOOP

C ----- Calculate Resistivity of Pipe and Grout
C ----- CAMNET 1992, (Eq. A.6)
      ODeq   = SQRT(2.0) * OD                         ! Outside Diameter RMS
      Rpipe  = LOG(OD / ID) / (2 * PI * Kpipe)       ! mK/W
      Rgrout = LOG(Dbore / ODeq)/(2 * PI * Kgrout)  ! mK/W
        

C ----- For Multi-Year Analysis, Calc. Dummy Variable 
C ----- For time and energy arrays 
      iTime = iPeriod - INT((iPeriod - 1)/Period) * Period
      dTtot = 0
      Wattj = 0

C ----- jPeriod loop accumulates properties from beginning of precond. to iPeriod
C ----- Note: dTtot is only variable altered in jPer loop and used outside
      DO 110 jPer = StartDay , iPeriod ! +1  ! Modified March 18th

        
C ------- Subroutine will initialize jPeriod, timej, jTime variables
          CALL J_set (jPer, Period, jPeriod, iPeriod, 
     &                jTime, timej, TimeDay)
          sum = 0  ! set sum to zero (Denominator of Tinc)

C --------- Calculate soil properties Rinf and RS  **
          DO 120 m = 1, Nsoil  ! m = soil layer      
   
            CALL SOILV (m, Dsoil, Ksoil, jTime, Ks, Kw, Ds,
     &                  Dw, Period)
            CALL CalcLM (m, Dtop, Lm, Dbottom, Nsoil, Utube, Depth)

C --------- Hart & Couvillion, Earth-Coupled Heat Transfer, 1986 (EQ 2-8)
C --------- Far-field radius in metres
            Rinf = 4 * SQRT(Dsoil * timej)  
              
C --------- Hart & Couvillion, Earth-Coupled Heat Transfer, 1986 (EQ 3-3)
C --------- Far-field resistance
            RS = (PWRFOR(0.5 * OD / Rinf))/(2.0 * PI * Ksoil)

C --------- Calculate LM/LS for Period iPeriod,jPeriod,m  **
            X = (OD / 2) / Rinf
            Y = SP / Rinf
 
            IF (Y .GE. 1) THEN
               LMLSHE = 1.0
            ELSE
               LMLSHE = 1 + PWRFOR(Y) / PWRFOR(X)
            ENDIF
  
            IF (Y .GE. 2) THEN
               LMLST = 1.0
            ELSE
               LMLST = 1.0/(1.0 - (0.5 * SINACOS(Y / 2) *
     &                 PWRFOR(Y / 2) /PWRFOR(X)))
            ENDIF
           
            LMLS = LMLST*LMLSHE

            IF (LMLS .GT. 2.0) LMLS = 2.0
            IF (LMLS .LT. 1.0) LMLS = 1.0
        
C---------- Calculate denominator of Tinc eqn  **
C---------- CAMNET 1992, (Denominator of Eq A.4)
            sum = sum + 
     &               ((Lm * Nbore) / ((RS * LMLS) + Rpipe + Rgrout))

  120     CONTINUE ! Move to next soil layer, End of Nsoil LOOP

C-------- Calculate (Tf-Tinf)Total  
          Wattj_1 = Wattj
          Wattj = dQ(jPeriod) * (1000 / (TimeDay * 24))

C-------- CAMNET 1992, (Eq A.4)
          dTinc = (Wattj - Wattj_1) / sum
          dTtot = dTtot + dTinc


  110 CONTINUE ! Move to next 'previous' Period, End of jPer Loop


        kWj = dQ(iPeriod) / (TimeDay * 24)

C ----- Set appropriate values for ground temperature amplitude and average
C ----- If the gshp input data file has no version number or has a 0. version 
C ----- number, then ground temperature amplitude and average are to be read
C ----- from input data file
        if(gshp_grd_temp_cal_flag.eq.1) then
          gshp_ground_temp_avg = Tmean
          gshp_ground_temp_amp = Atemp
C ----- In case that the gshp input data file has version number > 0, then the
C ----- ground temperature parameters will be based on weather file and Moore model
        elseif(gshp_grd_temp_cal_flag.eq.2) then
          gshp_ground_temp_avg = Temp_Ground_Avg
          gshp_ground_temp_amp = Temp_Ground_Amp
        endif 
        
C ----- Calculate Tinf for period i  **
C ----- Hart & Couvillion, Earth-Coupled Heat Transfer, 1986 (EQ 10-3)
        a = - sqrt(PI / (365 * 24 * 3600 * Dmean))
C ----- Hart & Couvillion, Earth-Coupled Heat Transfer, 1986 (EQ 10-3)        
        b = 2 * PI / 365 * (Day -  Dayo)
C ----- Hart & Couvillion, Earth-Coupled Heat Transfer, 1986 (EQ 10-4)
        dTinf = (Gshp_Ground_Temp_Amp * exp(a * Dtop))/
     &    (2 * a * (Dbottom - Dtop))
     &  * (COS(b + a * Dtop)+ SIN(b + a * Dtop))

C ----- **** Calculate EWT ****
        Tfluid = dTtot + dTinf + Gshp_Ground_Temp_Avg


        EWT = Tfluid - (kWj / 2) / (Flow / 1000 * Cp * Density)

c Check to ensure that circulating fluid does not freeze, the entering water
c temperature should not drop below -10 degrees Celcius (note: this fluid is 
c assumed to be a 15% propylene glycol/water mixture.
c Currently, a message will be written to the screen
c indicating that the EWT is dropping below the freezing point, but the 
c simulation will not stop.
        IF( EWT .lt. -10.0 ) THEN
          WRITE(IUOUT,*) ' GSHPSIM GSHP_HEATXV1: water in ground loop '
          WRITE(IUOUT,*) ' is about to freeze. Check model '
          WRITE(IUOUT,*) ' parameters.'
c          STOP ' GSHP HEATXV1: unresolvable error'
        END IF


       RETURN  
       END


c *********************************************************************
c *********************************************************************
c GSHP_HEATXHS
c DESIGN OF HORIZONTAL GROUND SOURCE HEAT EXCHANGER - 2 PIPES/TRENCH 
c                                           (Side by side)
c
c Developer: A.Morrison / M.Choy - Caneta Research Inc.                  
C Deliverable Date: November 27, 2001 - To JP of NRCan(MC)      
C Date of Last Revision: March 2002
c Updated: Julia Purdy - CETC, March 25, 2002
c                                    
c DESCRIPTION:
C  This subroutine calculates the entering water temperature (EWT) to a ground-
C  source heat pump (GSHP) for a horizontal ground heat exchanger. The ground,
C  regional weather and heat exchanger properties are passed from the main      
C  program calling the subroutine. Daily EWTs are returned.
C
c REFERENCE:
c  DESIGN OF VERTICAL GROUND SOURCE HEAT EXCHANGER - 1 U-TUBE/BOREHOLE 
c  Hart, David P. and Couvillion, Rick (1986) 'Earth-Coupled Heat Transfer
c
c INPUTS:
c    GSHP_INPUT  input file specific to the GSHP type  
c    
c
c OUTPUTS:
c    EWT         Entering water temperature 
C
c *********************************************************************

      SUBROUTINE GSHP_HEATXHS(iPeriod)

      
C ----------------------  Definition of Data Types --------------------
C       USE GSHP_INPUT
C      USE GSHP_DAILY
C Module variables specific to horizontal heat exchangers.
C       USE GSHP_HORIZONTAL_INPUT

      IMPLICIT NONE
#include "building.h"
#include "hvac_parameters.h"
#include "gshp_common.h"
#include "gshp_hs_common.h"
#include "ground_temp_mains_common.h"

C Common for read and write statements
      COMMON/OUTIN/IUOUT,IUIN,IEOUT

C ----------------------  Definition of Data Types --------------------

      INTEGER IUOUT,IUIN,IEOUT
      
      ! Pre-initialized Variables
      REAL timej                           ![second]      
      ! Calculated Variables
      REAL Dmean,Rpipe                   ![m2/s],[mK/W]
      REAL sum, Dsoil, RS, Rinf            ![W/K],[m2/s],[W/mK],[m]
      REAL Ksoil, X, Y, K1, K2            ![W/mK],[-],[-],[-],[-]
      REAL LMLS, LMLSHE, LMLST            ![-],[-],[-]
      REAL Wattj, Wattj_1                  ![W],[W]
      REAL dTinc, dTtot, kWi             ![C],[C],[kW]
      REAL a, b, dTinf, Tfluid              ![],[],[C],[C]
      ! Function Variables  
      REAL PWRFOR, SINACOS, PI
      INTEGER jPer, iPeriod, jPeriod, m, iTime, jTime
      REAL gshp_ground_temp_avg,gshp_ground_temp_amp

      
C ----------------------  Variable Definitions ------------------------

C      Variable Initilizations
      PI = ACOS(-1.0)            
      
      
C ---------------------------  Main Program  --------------------------

C ----- Dmean = weighted average over 12 months (summer 25%, winter 75%)
      Dmean = 0.75 * Dw + 0.25 * Ds

C ----- Hart & Couvillion, Earth-Coupled Heat Transfer, 1986 (EQ 1-2)
      Rpipe = LOG(OD / ID) / (2 * PI * Kpipe)  ! mK/W
      
      
C ----- For Multi-Year Analysis, Calc. Dummy Variable
C ----- For time and energy arrays
      iTime = iPeriod - INT((iPeriod - 1) / Period) * Period
      dTtot = 0
      Wattj = 0

C ----- jPeriod loop accumulates properties from Jan - Startup to iPeriod
C ----- Note: dTtot is only variable altered in jPer loop and used outside

c This loop was written to account for the impact of previous timesteps
c on the ground and fluid temperatures. This code will be called once per
c day. The jPer value will be set to the day of the simulation.

      DO 110 jPer = StartDay , iPeriod ! +1  ! Modified March 19th

C ------- Subroutine will initialize jPeriod, timej, jTime variables
        CALL J_set (jPer, Period, jPeriod, iPeriod, jTime, timej,
     &              TimeDay)
        
        sum = 0  ! set sum to zero (Denominator of Tinc)
         
C ------- Calculate soil properties Rinf and RS  **      
        CALL SOILH (Dsoil, Ksoil, jTime, Ks, Kw, Ds, Dw, Period)

C ------- Far-field radius in metres
C ------- Hart & Couvillion, Earth-Coupled Heat Transfer, 1986 (EQ 2-8)          
        Rinf = 4 * SQRT(Dsoil * timej) 
          
C ------- Far-field resistance
C ------- Hart & Couvillion, Earth-Coupled Heat Transfer, 1986 (EQ 3-3)
        RS = (PWRFOR(0.5 * OD / Rinf)) / (2.0 * PI * Ksoil)

C ------- Calculating Ground Surface Effects
        CALL SURFACE (OD, Rinf, BD, K1, K2)

C ------- Calculate LM/LS for Period i,j,m 
        X = (OD / 2) / Rinf
        Y = SD / Rinf

        IF (Y .GE. 1) THEN
          LMLSHE = 1.0
        ELSE
          LMLSHE = 1 + PWRFOR(Y) / PWRFOR(X)
        ENDIF

        IF (Y .GE. 2) THEN
          LMLST = 1.0
          ELSE
            LMLST = 1.0 / (1.0 - (0.5 * SINACOS(Y / 2) *
     &              PWRFOR(Y / 2)/PWRFOR(X)))
        ENDIF
        LMLS = LMLST*LMLSHE
        IF (LMLS .GT. 2.0) LMLS = 2.0
          IF (LMLS .LT. 1.0) LMLS = 1.0

C ------- Calculate denominator of Tinc eqn
C ------- CAMNET 1992, (Denominator of Eq A.5)
        sum = sum + pipe_length/(RS * LMLS + Rpipe)

        Wattj_1 = Wattj
        Wattj = dQ(jPeriod) * (1000 / (TimeDay * 24))
                          
C ------- CAMNET 1992, (Eq A.5)         
        dTinc = (Wattj-Wattj_1) / sum       ! * (1-K1-K2) Note: This was not
                                            ! included in GS-2000
        dTtot = dTtot + dTinc


 110    CONTINUE ! Move to next 'previous' period - End of jPeriod LOOP
                 

        kWi = dQ(iPeriod) / (TimeDay * 24)

C ----- Set appropriate values for ground temperature amplitude and average
C ----- If the gshp input data file has no version number or has a 0. version 
C ----- number, then ground temperature amplitude and average are to be read
C ----- from input data file
        if(gshp_grd_temp_cal_flag.eq.1) then
          gshp_ground_temp_avg = Tmean
          gshp_ground_temp_amp = Atemp
C ----- In case that the gshp input data file has version number > 0, then the
C ----- ground temperature parameters will be based on weather file and Moore model
        elseif(gshp_grd_temp_cal_flag.eq.2) then
          gshp_ground_temp_avg = Temp_Ground_Avg
          gshp_ground_temp_amp = Temp_Ground_Amp
        endif   

C ----- Calculate Tinf for period i
C ----- Reference
      a = - sqrt(PI / (365 * 24 * 3600 * Dmean))
C ----- Reference
      b = 2 * PI / 365 * (Day -  Dayo)
C ----- Reference
      dTinf = 
     &  - 1 * Gshp_Ground_Temp_Amp * EXP(a * BD) * COS(b + a * BD)
C ----- **** Calculate EWT  ****
      Tfluid = dTtot + dTinf + Gshp_Ground_Temp_Avg

      CALL FREEZING (Tfluid, kWi, pipe_length, Rpipe, OD, BD, dTinf, 
     &               Gshp_Ground_Temp_Avg, dTtot)
                
      EWT = Tfluid - (kWi / 2) / (Flow / 1000 * Cp * Density)

c Check to ensure that circulating fluid does not freeze, the entering water
c temperature should not drop below -10 degrees Celcius (note: this fluid is 
c assumed to be a 15% propylene glycol/water mixture.
c Currently, a message will be written to the screen
c indicating that the EWT is dropping below the freezing point, but the 
c simulation will not stop.
      IF( EWT .lt. -10.0 ) THEN
          WRITE(IUOUT,*) ' GSHPSIM GSHP_HEATXHS: water in ground loop '
          WRITE(IUOUT,*) ' is about to freeze. Check model '
          WRITE(IUOUT,*) ' parameters.'
c          STOP ' GSHP HEATXHS: unresolvable error'
      END IF
      
 
      RETURN
      END


c *********************************************************************
c *********************************************************************
c GSHP_HEATXH4
c DESIGN OF HORIZONTAL GROUND SOURCE HEAT EXCHANGER - 4 PIPES/TRENCH 
C                                         (Square X-section)
c Developer: A.Morrison / M.Choy - Caneta Research Inc.                  
C Deliverable Date: November 27, 2001 - To JP of NRCan(MC)      
C Date of Last Revision: March 2002
c Updated: Julia Purdy - CETC, March 25, 2002
c                                    
c DESCRIPTION:
C  This subroutine calculates the entering water temperature (EWT) to a ground-
C  source heat pump (GSHP) for a horizontal ground heat exchanger. The ground,
C  regional weather and heat exchanger properties are passed from the main      
C  program calling the subroutine. Daily EWTs are returned.
C
c REFERENCE:
c  DESIGN OF VERTICAL GROUND SOURCE HEAT EXCHANGER - 1 U-TUBE/BOREHOLE 
c  Hart, David P. and Couvillion, Rick (1986) 'Earth-Coupled Heat Transfer
c
c INPUTS:
c    GSHP_INPUT  input file specific to the GSHP type  
c    
c
c OUTPUTS:
c    EWT         Entering water temperature 
C
c *********************************************************************

      SUBROUTINE GSHP_HEATXH4(iPeriod)
      
C ----------------------  Definition of Data Types --------------------
C       USE GSHP_INPUT
C       USE GSHP_DAILY
C Module variables specific to horizontal heat exchangers.
C       USE GSHP_HORIZONTAL_INPUT

       IMPLICIT NONE
#include "building.h"
#include "hvac_parameters.h"
#include "gshp_common.h"
#include "gshp_hs_common.h"
#include "ground_temp_mains_common.h"
C Common for read and write statements
      COMMON/OUTIN/IUOUT,IUIN,IEOUT


C ----------------------  Definition of Data Types --------------------

        INTEGER IUOUT,IUIN,IEOUT

      ! Pre-initialized Variables 
      REAL timej                          ![second]

      ! Calculated Variables [OA]            
      REAL SP, Dmean, Rpipe              ![m2/s],[mK/W]
      REAL sum, Dsoil, Ksoil            ![W/K],[m2/s],[W/mK]
      REAL Rinf, RS, K1, K2            ![m],[mK/W]
      REAL X, Y, Z, W                     ![-],[-],[-],[-]
      REAL LMLS, LMLSHE, LMLST            ![-],[-],[-]
      REAL Wattj, Wattj_1                  ![W],[W]
      REAL dTinc, dTtot, kWi            ![C],[C],[kW]
      REAL a, b, dTinf, Tfluid             ![],[],[C],[C]
      REAL Tfluid_temp                    ![C]
      REAL PWRFOR, SINACOS, PI                  
      INTEGER jPer, idepth, iPeriod, jPeriod, m, iTime, jTime
      REAL gshp_ground_temp_avg,gshp_ground_temp_amp

C ----------------------  Variable Definitions ------------------------
      
C -----      Variable Initilizations

      PI = ACOS(-1.0)            
      
C ---------------------------  Main Program  --------------------------

C ----- Dmean = weighted average over 12 months (summer = 25%, winter = 75%)
      Dmean = 0.75 * Dw + 0.25 * Ds        
      
C ----- Hart & Couvillion, Earth-Coupled Heat Transfer, 1986 (EQ 1-2):
      Rpipe = LOG(OD / ID) / (2 * PI * Kpipe) ! mK/W

C ----- Difference in depth of piping
      SP = ABS(PD1 - PD2)            
      
C ----- For Multi-Year Analysis, Calc. Dummy Variable
C ----- For time and energy arrays
      iTime = iPeriod

C ----- Calculate EWT for both pipe depths then take average
      DO 95 idepth = 1, 2


c If idepth is 1, then set the depth of the heat exchanger pipe (BD) to the
c depth of the first pipe. If idepth is 2, then use the depth of the 
c second pipe.
        IF (idepth .eq. 1) THEN
            BD = PD1
          ELSE
            BD = PD2
        ENDIF


        dTtot = 0
        Wattj = 0


c This loop was written to account  for the impact of previous
c timesteps on the ground and fluid temperatures. This code will be 
c called once per day.
        DO 110 jPer = StartDay , iPeriod ! +1  ! Modified March 19th

          
C --------- Subroutine will initialize jPeriod, timej, jTime variables
           CALL J_set (jPer, Period, jPeriod, iPeriod, 
     &                  jTime, timej, TimeDay)
          sum = 0

C --------- Calculate soil properties Rinf and RS  **
          CALL SOILH (Dsoil, Ksoil, jTime, Ks, Kw, Ds, Dw, Period)
            
C --------- Far-field radius in metres
C --------- Hart & Couvillion, Earth-Coupled Heat Transfer, 1986 (EQ 2-8):
            Rinf = 4 * SQRT(Dsoil * timej)

C --------- Far-field resistance
C --------- Hart & Couvillion, Earth-Coupled Heat Transfer, 1986 (EQ 3-3):
          RS = (PWRFOR(0.5 * OD / Rinf))/(2.0 * PI * Ksoil)

C --------- Calculating Ground Surface Effects      
          CALL SURFACE (OD, Rinf, BD, K1, K2)
            
C --------- Calculate LM/LS for Period iPeriod, jPeriod
          X = (OD/2.)/Rinf
          Y = SD/Rinf
          Z = SP/Rinf
          W = sqrt(SD**2 + SP**2)/Rinf
          
          IF (Y .GE. 1) LMLSHE = 1.0
          IF (Z .GE. 1) LMLSHE = 1 + PWRFOR(Y) / PWRFOR(X)
          IF (W .GE. 1) THEN
            LMLSHE = 1 + PWRFOR(Y) / PWRFOR(X) + PWRFOR(Z) / PWRFOR(X)
          ELSE
            LMLSHE = 1 + PWRFOR(Y) / PWRFOR(X) + PWRFOR(Z) / PWRFOR(X) 
     &                 + PWRFOR(W)/PWRFOR(X)
          ENDIF

          IF (Y .GE. 2) LMLST = 1.0
          IF (Z .GE. 2) THEN
            LMLST = 1.0/(1.0-(0.5*SINACOS(Y/2)*PWRFOR(Y/2)/PWRFOR(X)))
          ENDIF
          
          IF (W .GE. 2) THEN
            LMLST = 1.0/(1.0-(0.5*SINACOS(Y/2)*PWRFOR(Y/2)/PWRFOR(X))
     &              - (0.5*SINACOS(Z/2)*PWRFOR(Z/2)/PWRFOR(X)))
          ELSE
            LMLST = 1.0/(1.0-(0.5*SINACOS(Y/2)*PWRFOR(Y/2)/PWRFOR(X))
     &              - (0.5*SINACOS(Z/2)*PWRFOR(Z/2)/PWRFOR(X))
     &              - (0.5*SINACOS(W/2)*PWRFOR(W/2)/PWRFOR(X)))
          ENDIF

          LMLS = LMLST * LMLSHE

          IF (LMLS .GT. 4.0) LMLS = 4.0
          IF (LMLS .LT. 1.0) LMLS = 1.0

C --------- Calculate denominator of Tinc eqn
C --------- CAMNET 1992, (Denominator of EQ A.5)
            sum = sum + (pipe_length / (RS * LMLS + Rpipe))

            Wattj_1 = Wattj
            Wattj = dQ(jPeriod) * (1000 / (TimeDay * 24))

C --------- CAMNET 1992, (EQ A.5)
            dTinc = (Wattj - Wattj_1) / sum       !* (1-K1-K2) Note: This was not
            dTtot = dTtot + dTinc            ! included in GS-2000   
 
110      CONTINUE  ! Move to next Period between StartUp to iPeriod
                   ! End of jPer Loop

          kWi = dQ(iPeriod) / (TimeDay * 24)

C ----- Set appropriate values for ground temperature amplitude and average
C ----- If the gshp input data file has no version number or has a 0. version 
C ----- number, then ground temperature amplitude and average are to be read
C ----- from input data file
          if(gshp_grd_temp_cal_flag.eq.1) then
            gshp_ground_temp_avg = Tmean
            gshp_ground_temp_amp = Atemp
C ----- In case that the gshp input data file has version number > 0, then the
C ----- ground temperature parameters will be based on weather file and Moore model
          elseif(gshp_grd_temp_cal_flag.eq.2) then
            gshp_ground_temp_avg = Temp_Ground_Avg
            gshp_ground_temp_amp = Temp_Ground_Amp
          endif   

C ------- Calculate Tinf for period i
C ------- Hart & Couvillion, Earth-Coupled Heat Transfer, 1986 (EQ 10-6):
          a = - sqrt(PI / (365 * 24 * 3600 * Dmean))
C ------- Hart & Couvillion, Earth-Coupled Heat Transfer, 1986 (EQ 10-6):
          b = 2 * PI / 365 * (Day -  Dayo)
C ------- Hart & Couvillion, Earth-Coupled Heat Transfer, 1986 (EQ 10-6):
          dTinf = 
     &     - Gshp_Ground_Temp_Amp * EXP(a * BD) * COS(b + a * BD)
            
C ----- - **** Calculate EWT ****
          Tfluid = dTtot + dTinf + Gshp_Ground_Temp_Avg


        IF (idepth .EQ. 1) then
          Tfluid_temp = Tfluid
        ELSE
          Tfluid = (Tfluid_temp + Tfluid)/2.0
        END IF

        CALL FREEZING (Tfluid, kWi, pipe_length, Rpipe, OD, BD,dTinf, 
     &                   Gshp_Ground_Temp_Avg, dTtot)

  95    CONTINUE  ! Go to second pipe burial depth
      
C------ ** Calculate EWT **
      EWT = Tfluid - (kWi / 2) / (Flow / 1000 * Cp * Density)     

c Check to ensure that circulating fluid does not freeze, the entering water
c temperature should not drop below -10 degrees Celcius (note: this fluid is 
c assumed to be a 15% propylene glycol/water mixture.
c Currently, a message will be written to the screen
c indicating that the EWT is dropping below the freezing point, but the 
c simulation will not stop.
        IF( EWT .lt. -10.0 ) THEN
          WRITE(IUOUT,*) ' GSHPSIM GSHP_HEATXH4: water in ground loop '
          WRITE(IUOUT,*) ' is about to freeze. Check model '
          WRITE(IUOUT,*) ' parameters.'
c         STOP ' GSHP HEATXH4: unresolvable error'
        END IF

        RETURN
        END


c *********************************************************************
c *********************************************************************
c GSHP_HEATXSL
c DESIGN OF HORIZONTAL SLINKY GROUND SOURCE HEAT EXCHANGER - 
C                                         Slinky on Bottom of Trench
c Developer: A.Morrison / M.Choy - Caneta Research Inc.                  
C Deliverable Date: November 27, 2001 - To JP of NRCan(MC)      
C Date of Last Revision: March 2002
c Updated: Julia Purdy - CETC, March 25, 2002
c                                    
c DESCRIPTION:
C  This subroutine calculates the entering water temperature (EWT) to a ground-
C  source heat pump (GSHP) for a horizontal ground heat exchanger. The ground,
C  regional weather and heat exchanger properties are passed from the main      
C  program calling the subroutine. Daily EWTs are returned.
C
c REFERENCE:
c  DESIGN OF VERTICAL GROUND SOURCE HEAT EXCHANGER - 1 U-TUBE/BOREHOLE 
c  Hart, David P. and Couvillion, Rick (1986) 'Earth-Coupled Heat Transfer
c
c INPUTS:
c    GSHP_INPUT  input file specific to the GSHP type  
c    
c
c OUTPUTS:
c    EWT         Entering water temperature 
C
c *********************************************************************

      SUBROUTINE GSHP_HEATXSL(iPeriod) 

      
C ----------------------  Definition of Data Types --------------------
C      USE GSHP_INPUT
C      USE GSHP_DAILY
C Module variables specific to horizontal heat exchangers.
C       USE GSHP_SLINKY_INPUT

       IMPLICIT NONE
#include "building.h"
#include "hvac_parameters.h"
#include "gshp_common.h"
#include "gshp_sl_common.h"
#include "ground_temp_mains_common.h"
C Common for read and write statements
      COMMON/OUTIN/IUOUT,IUIN,IEOUT

C ----------------------  Definition of Data Types --------------------

        INTEGER IUOUT,IUIN,IEOUT
             
      
      ! Pre-initialized Variables [OA]
      REAL timej                           ![seconds]      
      
        ! Calculated Variables [OA]
      REAL Dmean, Rpipe                          ![W/mK],[mK/W]
      REAL sum, Dsoil, Ksoil                     ![W/K],[m2/s],[W/mK]
      REAL Rinf, RS, K1, K2                  ![m],[mK/W]
      REAL SD, X, Y                        ![m]
      REAL space, powerx
      REAL LMLS, LMLSHE(8), LMLST(8)              ![-],[-],[-]
      REAL LMLS_tot, LMLStemp, DENOM              ![-],[-],[-]
      REAL Wattj, Wattj_1                        ![W],[W]
      REAL dTinc, dTtot, kWi, a, b, dTinf, Tfluid  ![C],[C],[kW],[],[],[C],[C]      
      REAL PWRFOR, SINACOS, PI
      
      INTEGER jPer, iPeriod, jPeriod, iTime, jTime, NumPl

      INTEGER Ncount, Pcount, Numcount

      REAL gshp_ground_temp_avg,gshp_ground_temp_amp
      
C ----------------------  Variable Definitions ------------------------
            
C -----      Variable Initilizations

      PI = ACOS(-1.0)            
      
      
C ---------------------------  Main Program  --------------------------

C ----- Dmean = weighted average over 12 months (summer = 25%, winter 75%)
      Dmean = 0.75 * Dw + 0.25 * Ds
      
C ----- Hart & Couvillion, Earth-Coupled Heat Transfer, 1986 (EQ 1-2):
      Rpipe = LOG(OD / ID) / (2 * PI * Kpipe) ! mK/W


C ----- For Multi-Year Analysis, Calc. Dummy Variable
C ----- For time and energy arrays
      iTime = iPeriod

      dTtot = 0
      Wattj = 0


C ----- jPeriod loop accumulates properties from beginning of precond to iPeriod
C ----- Note: dTot is only variable altered in jPer loop and used outside

c This loop was written to account for the impact of previous timesteps 
c on the ground and fluid temperatures. This code will be called once per
c day.
        DO 110 jPer = StartDay , iPeriod ! +1  ! Modified March 19th

         
        ! Subroutine will initialize jPeriod, timej, jTime variables
        CALL J_set (jPer, Period, jPeriod, iPeriod, jTime, timej,
     &                TimeDay)

C ------- Calculate soil properties for Rinf and RS  **          
          CALL SOILH (Dsoil, Ksoil, jTime, Ks, Kw, Ds, Dw, Period)
          
C ------- Far-field radius in metres
C ------- Hart & Couvillion, Earth-Coupled Heat Transfer, 1986 (EQ 2-8):
        Rinf = 4 * SQRT(Dsoil * timej)

C ------- Far-field resistance
C ------- Hart & Couvillion, Earth-Coupled Heat Transfer, 1986 (EQ 3-3):
        RS = (PWRFOR(0.5 * OD / Rinf)) / (2.0 * PI * Ksoil)
          
C ------- Calculating Ground Surface Effects
        CALL SURFACE (OD, Rinf, BD, K1, K2)
          
C ------- Calculate LM/LS based on timej
          NumPl = INT(NumP)  ! Perform first run for lower range
          
        DO 130 Numcount = NumPl, NumPl + 1
          SD = Spiral/(Numcount-1)  ! Calc. spacing between pipes of equiv. HX
          X = (OD/2.)/Rinf
          Y = SD/Rinf

C -------- Modified Method for calculating heat exchanger interference 
C -------- Do not determine if normalized distances ( X, Y, etc.) are > 0
C -------- to select best formula.  This is checked in PWRFOR Subroutine             
          
          powerx = PWRFOR(X)  ! Reduces number of times calc. is performed
      
          LMLS_tot = 0       
             
          DO 125 Ncount = 1, Numcount
            LMLSHE(Ncount) = 1.
            DENOM = 1.
        
            DO 120 Pcount = 1, Numcount
              IF (Pcount .LT. Ncount) THEN
              space = Ncount-Pcount
              LMLSHE(Ncount) = 
     &              LMLSHE(Ncount) + PWRFOR(space * Y) / powerx
              DENOM = DENOM - 0.5 * SINACOS(space * Y / 2)
     &                  * PWRFOR(space * Y / 2) / powerx
                ELSEIF (Pcount .GT. Ncount) THEN
               space = Pcount-Ncount
                LMLSHE(Ncount) = 
     &              LMLSHE(Ncount) + PWRFOR(space * Y) / powerx
              DENOM = DENOM - 0.5 * SINACOS(space * Y / 2)
     &                  * PWRFOR(space * Y / 2) / powerx
              ENDIF

 120          CONTINUE  ! End of Pcount LOOP
 
            LMLST(Ncount) = 1/DENOM
              LMLS_tot = LMLS_tot + LMLST(Ncount) * LMLSHE(Ncount)
 
 125        CONTINUE  ! End of Ncount LOOP
 
          LMLS = LMLS_tot/Numcount
          IF (LMLS .GT. Numcount) LMLS = Numcount
          IF (LMLS .LT. 1.0) LMLS = 1.0
          IF (Numcount .EQ. NumPl) THEN
C ----------- store lower boundary result in temp variable
            LMLStemp = LMLS
          ELSE 
C ----------- Interpolate between upper and lower boundary results              
            LMLS = LMLStemp + (LMLS - LMLStemp) * (NumP - NumPl)
          ENDIF

 130      CONTINUE  ! Redo calc. for upper integer range of pipes
                   ! End of Numcount Loop
                 ! End of Modified method loop for HE interface calc.

C ------- Calculate denominator of Tinc eqn
C ------- CAMNET 1992, (Denominator of Eq A.5)
        sum = pipe_length / ((RS * LMLS) + Rpipe)  

        Wattj_1 = Wattj
        Wattj = dQ(jPeriod) * (1000 / (TimeDay * 24))

C ------- CAMNET 1992, (Eq A.5)
        dTinc = (Wattj - Wattj_1) / sum      ! * (1-K1-K2) NOTE: This was not 
        dTtot = dTtot + dTinc                ! included in GS-2000      
      
 110    CONTINUE ! Move to next Period between StartUp to i Period
              ! End of jPer Loop


       kWi = dQ(iPeriod) / (TimeDay * 24)

C ----- Calculate Tinf for period i
C ----- Hart & Couvillion, Earth-Coupled Heat Transfer, 1986 (EQ 10-6):       
        a = - sqrt(PI / (365 * 24 * 3600 * Dmean))
C ----- Hart & Couvillion, Earth-Coupled Heat Transfer, 1986 (EQ 10-6):
        b = 2 * PI / 365 * (Day -  Dayo)
C ----- Hart & Couvillion, Earth-Coupled Heat Transfer, 1986 (EQ 10-6):

C ----- Set appropriate values for ground temperature amplitude and average
C ----- If the gshp input data file has no version number or has a 0. version 
C ----- number, then ground temperature amplitude and average are to be read
C ----- from input data file
        if(gshp_grd_temp_cal_flag.eq.1) then
          gshp_ground_temp_avg = Tmean
          gshp_ground_temp_amp = Atemp
C ----- In case that the gshp input data file has version number > 0, then the
C ----- ground temperature parameters will be based on weather file and Moore model
        elseif(gshp_grd_temp_cal_flag.eq.2) then
          gshp_ground_temp_avg = Temp_Ground_Avg
          gshp_ground_temp_amp = Temp_Ground_Amp
        endif   

      dTinf = - Gshp_Ground_Temp_Amp * EXP(a * BD) * COS(b + a * BD)
            
C ------ **** Calculate EWT  ****
      Tfluid = dTtot + dTinf + Gshp_Ground_Temp_Avg


      CALL FREEZING (Tfluid,kWi,pipe_length,Rpipe,OD,BD,dTinf,
     &Gshp_Ground_Temp_Avg,dTtot)       
       
      EWT = Tfluid - (kWi / 2) / (Flow / 1000 * Cp * Density)


c Check to ensure that circulating fluid does not freeze, the entering water
c temperature should not drop below -10 degrees Celcius (note: this fluid is 
c assumed to be a 15% propylene glycol/water mixture.
c Currently, a message will be written to the screen
c indicating that the EWT is dropping below the freezing point, but the 
c simulation will not stop.
        IF( EWT .lt. -10.0 ) THEN
          WRITE(IUOUT,*) ' GSHPSIM GSHP_HEATXSL: water in ground loop '
          WRITE(IUOUT,*) ' is about to freeze. Check model '
          WRITE(IUOUT,*) ' parameters.'
c          STOP ' GSHP HEATXSL: unresolvable error'
        END IF


        RETURN
      END

      
      
c *********************************************************************
c *********************************************************************
C                                                            
C                      COMMON SUBROUTINES                        
C                                                      
c *********************************************************************

c *********************************************************************
C  SUBROUTINE J_set
C  Subroutine will initialize jPeriod, timej, jTime variables, it calls 
C  TimeCount subroutine to initialize timej
c *********************************************************************

       SUBROUTINE J_set (jPer, Period, jPeriod, iPeriod, 
     &                   jTime, timej, TimeDay)
      
       IMPLICIT NONE

       REAL TimeDay, timej

       INTEGER jPer, Period, jPeriod, iPeriod, jTime
       INTEGER StartLoop, EndLoop      
      
       ! Converts days before Jan. 1st from '-' values to positive (ie: -10 --> 355)
       IF (jPer .LT. 1) THEN
          jPeriod = Period + jPer 
       ELSE
          jPeriod = jPer
       END IF      
      
       jTime = jPeriod - INT((jPeriod - 1) / Period) * Period


       timej = 0

! Time between Period j and Period i. Replaces DO Loop 115 in GS 2000
! TimeCount finds time between jPeriod and iPeriod and stores in timej

       StartLoop = jPer  ! Updated March 18th
       EndLoop = iPeriod

       CALL TimeCount (StartLoop, EndLoop, Period, timej, TimeDay)


       RETURN
       END


c *********************************************************************
C  SUBROUTINE TimeCount
C  The TimeCount Subroutine dynamically determines the time (days)
C  between the jPeriod (from StartDay to the iPeriod) and the iPeriod
c *********************************************************************

       SUBROUTINE TimeCount (StartLoop, EndLoop, Period, timej, TimeDay)
      
       IMPLICIT NONE
             
       REAL TimeDay, timej

       INTEGER StartLoop, EndLoop, Period, count, jcount

       DO 115 count = StartLoop, EndLoop          
          jcount = count - INT((count - 1) / Period) * Period
        timej = timej + TimeDay * 24 * 3600
 115   CONTINUE  ! End of iPeriod Loop

       RETURN
       END


c *********************************************************************
C  SUBROUTINE SOILV                                                
C  Subroutine to determine Dsoil and Ksoil based on period and Depth            
C  Separates by seasonal period                  
C  Only used for vertical systems.                        
c *********************************************************************

       SUBROUTINE SOILV(m, Dsoil, Ksoil, jTime, Ks, Kw, Ds, Dw, Period)
          
       IMPLICIT NONE
       
       REAL Dsoil, Ksoil, Ds(10), Ks(10), Dw(10), Kw(10)
       INTEGER m, jTime, Ilayer, Period
        
       Ilayer = m

C GSHP

C The following code was used to determine if the current simulation day was
C in the summer or the winter. This subroutine would set the values of Dsoil
C and Ksoil accordingly.
C Since we are not using the same period information as the Caneta simulation,
c these simulations will require modification. The revised IF statement, will 
C only include the day numbers for July-Sept.

       IF ((jTime.GE.181).AND.(jTime .LE. 273)) THEN
          Dsoil = Ds(Ilayer)
          Ksoil = Ks(Ilayer)
       ELSE
          Dsoil = Dw(Ilayer)
          Ksoil = Kw(Ilayer)
       END IF

       RETURN  
       END


c *********************************************************************
C  SUBROUTINE CalcLM        
C  Subroutine to calculate Lm for each sublayer of a Vertical GSHP
C  This layer process is used in HEATX-V1      
C  Only used for vertical systems.
c *********************************************************************

       SUBROUTINE CalcLM (m, Dtop, Lm, Dbottom, Nsoil, Utube, Depth)
        
       IMPLICIT NONE
        
       REAL Dtop, Dbottom, Utube, Depth(10)
       REAL Lm, Layer_top, Layer_bot 
       INTEGER m, Icount, Nsoil
        
       Layer_top = 0        
       DO 700 Icount = 1, m-1
          Layer_top = Layer_top + Depth(Icount) !Calc. Depth of Layers' top
 700   CONTINUE ! Endo of m-1 loop
                 
       IF (Nsoil .EQ. m) Depth(m) = Dbottom - Layer_top
          Layer_bot = Layer_top + Depth(m)      ! Depth of Layers' bottom    
          IF (Layer_bot .LE. Dtop) THEN
             Lm = 0
          ELSE IF ((Layer_bot .GT. Dtop).AND.(Layer_top .LT. Dtop)) THEN 
             IF (Dbottom .LT. Layer_bot) THEN
                Lm = 2.0*(Dbottom-Dtop)*Utube
             ELSE
                Lm = 2.0*(Layer_bot-Dtop)*Utube
             ENDIF
          ELSE IF (Layer_top .GT. Dbottom) THEN
             Lm = 0.0              
          ELSE
             IF (Dbottom .LT. Layer_bot) THEN
                Lm = 2*(Dbottom-Layer_top)*Utube
             ELSE
                Lm = 2*Depth(m)*Utube
             ENDIF
          ENDIF

       RETURN 
       END


c *********************************************************************
C SUBROUTINE SOILH
C Subroutine to determine Dsoil and Ksoil based on period and Depth
C The process of selecting soil properties is outlined in REFERENCE
C Only used for horizontal systems.
c *********************************************************************

       SUBROUTINE SOILH(Dsoil, Ksoil, jTime, Ks, Kw, Ds, Dw, Period)
      
       IMPLICIT NONE

       REAL Dsoil, Ksoil
       REAL Ds, Ks, Dw, Kw
       INTEGER jTime, Period
      
C GSHP

C The following code was used to determine if the current simulation day was
C in the summer or the winter. This subroutine would set the values of Dsoil
C and Ksoil accordingly.
C
C Since we are not using the same period information as the Caneta simulation,
c these simulations will require modification. The revised IF statement, will 
C only include the day numbers for July-Sept.

       IF ((jTime.GE.181).AND.(jTime .LE. 273)) THEN
         Dsoil = Ds
         Ksoil = Ks
       ELSE
         Dsoil = Dw
         Ksoil = Kw
       END IF

       RETURN
       END


c *********************************************************************
C SUBROUTINE SURFACE
C Subroutine to calculate Ground Surface Effect Factors
C Only used for horizontal systems.
c *********************************************************************

       SUBROUTINE SURFACE (OD, Rinf, BD, K1, K2)

       IMPLICIT NONE
        
       REAL SINACOS, PWRFOR
       REAL OD, Rinf, BD, K1, K2, X, Y
      
        X = OD / 2 / Rinf
        Y = BD / Rinf

        IF (Y .GE. 0.5) K1 = 0
C ------- Hart & Couvillion, Earth-Coupled Heat Transfer, 1986 (EQ 9-14):
        IF (Y .LT. 0.5) K1 = PWRFOR(2 * Y) / PWRFOR(X)
        IF (Y .GE. 1) K2 = 0
C ------- Hart & Couvillion, Earth-Coupled Heat Transfer, 1986 (EQ 9-13):
        IF (Y .LT. 1) K2 = 0.5 * SINACOS (Y) * PWRFOR(Y) / PWRFOR(X)

       RETURN
       END

c *********************************************************************
C SUBROUTINE FREEZING
C Subroutine to Approximate Freezing Effects
C Only used for horizontal systems.
c *********************************************************************

       SUBROUTINE FREEZING (Tfluid, kWi, L, Rpipe, OD, BD, dTinf, Tmean,
     &                     dTtot)         
       IMPLICIT NONE

       REAL Tfluid, kWi, L, Rpipe, OD, BD, dTinf 
       REAL Tmean, dTtot
       REAL Xice, Rice, RSice, PI, Kice, PWRFOR, dTotot, To


        Kice = 1.88 ! W/mK
        PI = ACOS(-1.0)
        To = Tfluid - kWi*1000.0 / (L / Rpipe)

        IF (To .LT. 0) THEN
C --------- CAMNET 1992, (Eq A.32)
           Xice = (OD / 2.0) / (5.34 * BD)
           dTotot = dTtot - kWi * 1000.0 * Rpipe / L

C --------- CAMNET 1992, (Eq A.32)
           Rice = 5.34 * BD / exp((-(dTinf + Tmean) / dTotot * 
     &          PWRFOR(Xice)) + 0.981755)

           if (Rice .gt. -1E-4) then
             Tfluid = Tfluid
           else
C --------- CAMNET 1992, (Eq A.34)
           RSice = PWRFOR(0.5 * OD / Rice) / (2 * PI * Kice)
             Tfluid = kWi * 1000.0 / (L / (RSice + Rpipe))
           endif

        ENDIF


       RETURN 
       END


c *********************************************************************
c *********************************************************************
C
C                         Program Functions
C                                                
c *********************************************************************

C  Function to evaluate the series expansion  
C  Inputs must be dimensionless

       FUNCTION PWRFOR(X)
  
        REAL PWRFOR, X, FACT
        INTEGER N, series
        
         IF (X .LT. 0.1) THEN
            series = 2
         ELSEIF (X .LT. 0.7) THEN
            series = 8
         ELSE
            series = 12
         ENDIF

         PWRFOR = Log(1/X)-0.981755
         DO 500 N = 1, series
            PWRFOR = PWRFOR + ((-1)**(N+1))/REAL(2*N*FACT(N)) * 
     &               (2*X)**(2*N)
 500     CONTINUE

         IF (X .GE. 1.0) PWRFOR = 0.0

       END


C  Note: Function FACT had to be changed to a REAL function, because as an Integer  **
C  function it caused the term 2*N*FACT(N) to be an integer, and when N = 12  **
C  the term became too large for an integer and created errors.  **
       FUNCTION FACT (N)        

        REAL FACT
        INTEGER N, Istep

        FACT = 1.0
        DO 600 Istep = 1, N
           FACT = Istep*FACT
 600    CONTINUE  ! End of N LOOP
         
       END


C  Function to calculate SINACOS 
C  Inputs must be dimensionless
       FUNCTION SINACOS (W)
        
        REAL SINACOS,PI,W

        PI = ACOS(-1.0)
        IF (W .GE. 1.0) W = 1.0
        SINACOS = ACOS(W)/PI-W*SIN(ACOS(W))/PI

       END


c *********************************************************************
c *********************************************************************




c *********************************************************************
c *********************************************************************
c GSHP_operating_COP

c Created by: Julia Purdy
c Initial Creation Date: June 25, 2002
c Copyright 2000: NRCan Buildings Group
c
c Subroutine to determine water-source or ground source heat pump 
c COP in heating or cooling mode.

c REFERENCE:
c              SIMPLIFIED ENERGY ANALYSIS ALGORITHMS FOR GSHP 
c              SYSTEMS FOR HOT2000
c              Prepared by: Caneta Research Inc., March 1994.

c INPUTS:
c      ss_cop            steady-state COP at rating conditions
c      EWT             fluid entering water temperature
c       GSHP_HP_mode    the mode of operation of the HP, either heating (1) or cooling (-1)
      
c OUTPUTS:
c      GSHP_COP      COP at operating conditions
c **********************************************************************

      SUBROUTINE GSHP_operating_COP(ss_cop,local_EWT,GSHP_HP_mode,cop)


C       USE GSHP_COP_MODULE

c This subroutine is called from ASHP_HEATING or ASHP_COOLING depending on whether
c the HP is operating in heating or cooling mode.

c The COP correlations in the function are obtained from page 5 of the
c referenced report (Simplified Energy Analysis Algorithms for GSHP
c Systems for HOT2000) for closed-loop ground source heat pumps.

c Mode of operation of the GSHP, GSHP_HP_mode = 1 for heating
c and -1 for cooling

      IMPLICIT NONE
#include "hvac_parameters.h"
#include "gshp_common.h"


C Arguments
      REAL ss_cop
      REAL local_EWT
      INTEGER GSHP_HP_mode
      REAL cop


C Local Variables
      REAL a_cop
      REAL b_cop
      REAL c_cop

      REAL a_EER
      REAL b_EER
      REAL c_EER

      REAL SS_EER
      REAL EER
       if (GSHP_HP_mode .ge. 0) then
c For water-source and ground-source heat pumps in heating mode, the correlating
c coefficients are,
        a_cop = 1.00098
        b_cop = 0.01756
        c_cop = -0.00011
        cop = ss_cop
     &   * (a_cop + b_cop * local_EWT + c_cop * local_EWT**2)

c For systems in cooling mode, the reference provides the correlations for the
c Energy Efficiency Rating or EER. The conversion between COP and EER is 3.413 But/W hr,
c therefore, the correlation equation is valid for both COP and EER.
      else
        a_EER = 1.06396
        b_EER = -0.00607
        c_EER = -4.44980E-5
c The conversion between COP and EER is as follows:
        ss_EER = ss_cop * 3.413
c For water-source and ground-source heat pumps in cooling mode, the correlating 
c coefficients are,
        EER = ss_EER
     &  * (a_EER + b_EER * local_EWT + c_EER * local_EWT**2)
c and then,
        cop = EER / 3.413
      endif
      
      GSHP_COP = cop

      return
      end


c *********************************************************************
c *********************************************************************
c GSHP_CAP

c Created by: Julia Purdy
c Initial Creation Date: June 25, 2002
c Copyright 2000: NRCan Buildings Group
c
c Subroutine to determine total heating or cooling capacity of a water-
c or ground-source heat pump from the capacity at rating conditions.

c REFERENCE:
c              SIMPLIFIED ENERGY ANALYSIS ALGORITHMS FOR GSHP 
c              SYSTEMS FOR HOT2000
c              Prepared by: Caneta Research Inc., March 1994.

c INPUTS:
c      ss_cap            steady state capacity at rating conditions
c      EWT             fluid entering water temperature (degrees C)
c      GSHP_HP_mode    the mode of operation of the HP, either heating (1) or cooling (-1)
      
c OUTPUTS:
c      GSHP_CAP      heating or cooling capacity of heat pump
c **********************************************************************

      REAL FUNCTION GSHP_CAP(ss_cap,EWT,GSHP_HP_mode)

c This subroutine is called from ASHP_HEATING or ASHP_COOLING depending on whether
c the HP is operating in heating or cooling mode.

c Mode of operation of the GSHP, GSHP_HP_mode = 1 for heating 
c and -1 for cooling
       integer GSHP_HP_mode

c Capacity correction factors. These correlations are obtained from page 5 of the 
c referenced report (Simplified Energy Analysis Algorithms for GSHP 
c Systems for HOT2000) for closed-loop ground source heat pumps.

c For water-source and ground-source heat pump systems in heating mode, 
c GSHP_HP_mode = 1, the correlating coefficients are:
      if (GSHP_HP_mode .eq. 1) then   
        a_cap = 1.00158
        b_cap = 0.03141
        c_cap = -0.0008
c For water-source and ground-source heat pump systems in cooling mode, 
c GSHP_HP_mode = -1, the correlating coefficients are:
      else
        a_cap = 1.17355
        b_cap = -0.01839
        c_cap = 6.50434E-5
      endif

c Set total capacity of water-source or ground-source heat pump. 
      cap = ss_cap * (a_cap + b_cap * EWT + c_cap * EWT**2)
      GSHP_CAP = cap

      return
      end      




c *********************************************************************
c *********************************************************************
c GSHP_pump_power

c Created by: Julia Purdy
c Initial Creation Date: June 25, 2002
c Copyright 2000: NRCan Buildings Group
c
c This function calculates the pump power required for a ground-
c source heat pump system.

c REFERENCE:
c     Water Source Heat Pump Data for HOT3000 - Milestone #1
c     Caneta Research Inc., December 7, 2001
c
c     ASHRAE Handbook of Fundamentals, 2001.

c INPUTS:
c      igshp_type        type of GSHP system selected in the input file   
c      GSHP_CAP          system capacity at operting conditions, W
c      pipe_length       total ground circuit pipe length, m
c      HP_pressure_drop  pressure drop through heat pump loop, kPa
c      runout_press_drop run-out pressure drop, kPa
c      circ_press_drop   ground loop circuit pressure drop, kPa
c      runout_fit_drop   run-out fitting pressure drop, kPa
c      circ_fit_drop     ground loop circuit fitting pressure drop, kPa
      
c OUTPUTS:
c       GSHP_pump_power   circulating fluid pump power, J
c ********************************************************************** 

      REAL FUNCTION GSHP_pump_power(GSHP_CAP)


C       USE GSHP_INPUT
      IMPLICIT NONE
#include "hvac_parameters.h"
#include "gshp_common.h"

c Common for the number of time steps per hour
       COMMON/PERS/ISD1,ISM1,ISD2,ISM2,ISDS,ISDF,NTSTEP

       common/gshpinfo/igshp
       common/gshpfile/gshpfile

       INTEGER igshp  
       character gshpfile*72 
       INTEGER ISD1,ISM1,ISD2,ISM2,ISDS,ISDF,NTSTEP
       COMMON/OUTIN/IUOUT,IUIN,IEOUT
       INTEGER IUOUT,IUIN,IEOUT

c Local variables
       REAL time_step, GSHP_CAP
       REAL HP_pressure_drop, runout_press_drop, runout_fit_drop
       REAL circ_fit_drop, circ_press_drop
       REAL GSHP_pump_input


c The pumping power required depends on the configuration of the ground loop, the
c system capacity and the pressure drop across the loop.
c Based on the heat pump capacity and the ground loop configuration, correlations
c have been defined in Table 3 of the Caneta Report (2001) referenced above.

c In the Caneta report, four capacities have been specified: 2, 2.5, 3, and 4 tons. 
c These capacities convert to 7034 W, 8792.5 W, 10551 W, and 14068 W, 
c based on the conversion factor of 1 ton = 12 000 Btu/h = 3.517 kW
c defined in the ASHRAE Handbook of Fundamentals (2001) pg. 37.1

       if (GSHP_CAP .lt. 8792.5) then
          HP_pressure_drop = 26.7 
          runout_press_drop = 3.4
          runout_fit_drop = 0.48
       elseif ((GSHP_CAP .ge. 8792.5) .and. 
     &         (GSHP_CAP .lt. 10551.)) then
          HP_pressure_drop = 17.2 
          runout_press_drop = 4.8
          runout_fit_drop = 0.71   
       elseif ((GSHP_CAP .ge. 10551.) .and.
     &         (GSHP_CAP .lt. 14068.)) then
          HP_pressure_drop = 28.4 
          runout_press_drop = 6.6
          runout_fit_drop = 1.02
       elseif (GSHP_CAP .ge. 14068.) then
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
c pressure drop (circ_press_drop) based on system configuration (igshp_type).
c If the system is a vertical single U-tube, 
       if(igshp_type .eq. 1 ) then
          circ_press_drop = 0.1855
c If the system is a horizontal 4 pipe 2x2 arrangement,
       elseif(igshp_type .eq. 2) then
          circ_press_drop = 0.1855
c If the system is horizontal 2 pipe side-by-side arrangement,
       elseif(igshp_type .eq. 3) then
          circ_press_drop = 0.1855
c If the system is a horizontal slinky,
       elseif(igshp_type .eq. 4) then
          circ_press_drop = 0.2041
c In the case that igshp_type is not equal to 1,2,3,or 4, write an error to the 
c screen
       else
          WRITE(IUOUT,*) ' GSHP_pump_power: GSHP configuration '
          WRITE(IUOUT,*) ' number outside acceptable range ',igshp_type
          return
       endif

c The circulating fluid pump input (GSHP_pump_input) in W is:
       GSHP_pump_input = 0.16 * (GSHP_CAP / 3517) * 4.22 * 
     &                   (HP_pressure_drop + runout_press_drop +
     &                   ((pipe_length / GSHP_CAP)* circ_press_drop) +
     &                   runout_fit_drop + circ_fit_drop)

c The pump power in Joules is then a function of the simulation time step
       time_step = 3600. / float(NTSTEP)

       GSHP_pump_power = GSHP_pump_input * time_step

       return
       end


c *********************************************************************
c *********************************************************************
c weighted_GSHP_cop

c Created by: Julia Purdy
c Initial Creation Date: November 8, 2002
c Copyright 2002: NRCan Buildings Group

c This subroutine sums the load-weighted average GSHP coefficient of 
c performance (COP) over the simulation day. This subroutine is called
c once every simulation timestep, once the COP has been calculated.

c The load-weighted COP is passed to the main GSHP simulation subroutine 
c GSHPSIM, via GSHP_COP_MODULE, where the daily system COP is calculated.
c
c INPUTS:
c     GSHP_cop      the COP for heating or cooling operation, per timestep 
c     load          the heating or cooling load, per timestep
c
c OUTPUTS:
c     load_weighted_GSHP_cop   load-weighted average COP
c ***********************************************************************
      SUBROUTINE weighted_GSHP_cop(cop,load)


C      USE GSHP_COP_MODULE

      IMPLICIT NONE
#include "hvac_parameters.h"
#include "gshp_common.h"


      COMMON/PERS/ISD1,ISM1,ISD2,ISM2,ISDS,ISDF,NTSTEP

C Defining variables used in the subroutine   
      INTEGER ISD1,ISM1,ISD2,ISM2,ISDS,ISDF,NTSTEP

      REAL load,cop

c The timestep GSHP coefficient of performance has been determined in the 
c ASHP/ASHP_COOLING subroutine.

c The load-weighted average ground heat exchanger COP is the sum of the timestep 
c COP divided by the timestep load over the simulation day - calculated below. If
c there is no load for that timestep, then do not include it in the calculation.

       if (load .gt. 0.0) then
         load_weighted_GSHP_COP = load_weighted_GSHP_COP + 
     &                          (cop * load)/NTSTEP
       else
         load_weighted_GSHP_COP = load_weighted_GSHP_COP
       endif
       
       return
       end

c *********************************************************************
c *********************************************************************
c daily_GSHP_system_COP

c Created by: Julia Purdy
c Initial Creation Date: November 14, 2002
c Copyright 2002: NRCan Buildings Group
c
c This function calculates the daily ground-source heat pump system 
c coefficient of performance

c INPUTS:
c       cop     the sum of the timestep load-weighted COP values 
c       load    the sum of the timestep heating or cooling loads    
      
c OUTPUTS:
c       daily_GSHP_COP  the daily system COP
c ********************************************************************** 

       REAL FUNCTION daily_GSHP_COP(cop, load)     


       REAL cop, load

c The daily system coefficient of performance is equal to the daily laod-
c weighted COP multiplied by the load.
       daily_GSHP_COP = cop / load

       return
       end
