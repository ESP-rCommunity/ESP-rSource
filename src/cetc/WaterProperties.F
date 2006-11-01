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

C     For the Thermal Active Cooling (TAC) project, a model was required to 
C     obtain the  specific heat (Cp) and density (rho) of water for a given 
C     temperature and pressure. Esp-r, the simulator which the TAC model is 
C     developed for, did not have an adaquate water model. It was valid only 
C     for temperature ranges of 0-100oC and for 101.3KPa. So a new model 
C     was required. 

C     This unit contains functions to obtain physical properties of water. 
C     The code is based on The International Association for the Properties 
C     of Water and Steam: Release on the IAPWS Formulation 1995 for the 
C     Thermodynamic Properties of Ordinary Water Substances for General and 
C     Scientific Use" Sept 1996 (http://www.iapws.org/relguide/IAPWS95.pdf).
C
C     The implemented fortran 77 REAL*8  routines were Donated by Duncan 
C     Simpson from Cambridge. 
C     
C     The routines were massively modified and converted to f90 and Standard 
C     REAL by Phylroy Lopez.  
C ***************************************************************************
C     The following routines Have been developed by Phylroy Lopez.
C 
c     function ISINLIQUIDPHASE(Temp, Pressure)
C     Return True or False. If water is in liquid phase for given temperature
C     and Pressure.
C
C     function H20LIQUIDCP(Temp, Pressure)
C     Will return the specific heat (J/(kg*Kelvin))for the given 
C     temperature and pressure. Note: Depends on   H20LIQUIDDENSITY.
C
C     function H20LIQUIDDENSITY(Temp, Pressure)
C     Will return perform a check 'ISINLIQUIDPHASE'. If check is false then 
C     will display error message to standard out and stop program. If it is 
C     in liquid phase it will return the density (kg/m3)
C
C     function H20GASDENSITY(Temp, Pressure)
C     Will return perform a check 'ISINLIQUIDPHASE'. If check is true then 
C     will display error message to standard out and stop program. If it is 
C     in gasous phase it will return the density (kg/m3)
C   
C     The follwoing function are currently unused directly, but may be used 
C     indireclty from the functions above. These functions for enthalpy, 
C     entropy and Isochoric heat capacity may be of use in the future, 
C     but for the time being are left undocumented.    
c 
c     function IAPWSPRESS
c     function IAPWSENERGY
c     function IAPWSENTR
c     function IAPWSENTH
c     function IAPWSCV
c     function IAPWSCP
c     function IAPWSSOUND
c     function IAPWSSOUND2
c     function IAPWSJOULE
c     function IAPWSTHROTTLE
c     function IAPWSBETA
c     function IAPWSDPDR
c     function IAPWSDPDT
c     function PHIO
c     function PHIODELTA
c     function PHIODELTADELTA
c     function PHIOTAU
c     function PHIOTAUTAU
c     function PHIR
c     function PHIRDELTA
c     function PHIRDELTADELTA
c     function PHIRTAU
c     function PHIRTAUTAU
c     function PHIRDELTATAU
c     function FNPSI
c     function FNOMEGA
c     function FNCAPDELTA
c     function FNDCAPBD
c     function FNDCAPD
c     function FNDPSID
c     function FNDPSIT
c     function FNDCAPBT
c     block data IPAWSSCIENCE
c     subroutine UNWRAPDENL
c     subroutine UNWRAPDEN
c     subroutine UNWRAPEN
c     subroutine UNWRAPPRESS
c     subroutine UNWRAPENTR
c     function FUNPV
c     function FUNRHOL
c     function FUNRHOV
c     function FUNAUX
c     function FUNAUXPHI
c     function FUNTAU
c     function FUNDPDTSAT
c     function FUNHFG
c     function FUNHL
c     function FUNHV
c     function FUNSL
c     function FUNSV
c     function FUNSURF
c     function FUNTSAT
c     function FUNVIS
c     function FUNCOND
c     function FUNPRG
c     function FUNMFP
c     function FUNTSPTEMP
c     function SBACKWARD2A
c     block data IPAWS
c     subroutine INITIALDPDT
c     function DFUNPV
c     function DFUNRHOL
c     function DFUNRHOV
c     function DFUNAUX
c     function DFUNAUXPHI
c     function DFUNTAU
c     function DFUNDPDTSAT
c     function DFUNHFG
c     function DFUNHL
c     function DFUNHV
c     function DFUNSL
c     function DFUNSV
c     function DFUNSURF
c     function DFUNTSAT
c     function DFUNVIS
c     function DFUNCOND
c     function DFUNPRG
c     function DFUNMFP
c     function DFUNTSPTEMP
c     function DSBACKWARD2A

C **********************************H20LiquidCpCel ***************************************
c Percent Difference Test
C Created by: Phylroy Lopez
C Initial Creation Date: November, 2002
C Copyright CETC 2001
c     This function will determine the Cp of a given Temperature C and Pressure.
C INPUTS: 
c     REAL Temp  ! Degrees celsius
c     REAL Pressure       ! pascals.  
C OUTPUT: 
c     REAL  H20LiquidCpCel   ! joules/(kg Kelvin)
c Note no unit testing has been performed for this function.
C --------------------------------------------------------------------------------    
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      FUNCTION H20LiquidCpCel(Temp, Pressure)
      implicit none
c     Return type
      REAL H20LiquidCpCel
c     Kelvin version of call
      REAL H20LiquidCp
c     Liquid density function.
      REAL H20LiquidDensity
c     Temperature in Celsius!
      REAL Temp
c     Temperature in Kelvin.
      REAL TempKel
c     Pressure in Pa
      REAL Pressure
c     Switch to Kelvin
      TempKel = Temp + 273.15
c     Obtain Cp
      H20LiquidCpCel = H20LiquidCp(TempKel, Pressure)
      RETURN
      END
C***************************************************************************************


C **********************************IsInLiquidPhase***************************************
C     Created by: Phylroy Lopez
C     Initial Creation Date: November, 2002
C     Modified by: Maria Mottillo
C     Modified on: August 2006
C     Extended range of temperatures that function is applicable. Data obtained from
C     Fundamentals of Classical Thermodynamics, 3rd Ed. (Van Wylen and Sonntag, 1985)
C     Copyright CETC 2001
      
C     This function will determine if the water is in liquid phase. This is determined by
c     interpolating what the temperature is required from the saturation curve to ensure 
c     liquidity. 
C     The data for the curve was obtained 
C     from  Reynolds, Perkins,"Engineering Thermodynamics" Table B1
C     INPUTS: 
c     REAL Temp ! the temperature in Kelvin
c     REAL Pressure ! the pressure in Pa

C     OUTPUT: true if it liquid phase
C     *************************************************************************************

c     This function will determine if water is in liquid or gas phase.
      FUNCTION IsInLiquidPhase(Temp, Pressure)
      implicit none
c     flag that the interval where the temperature given fits it. T(i) on 
c     saturation curve. 
      INTEGER iIntervalFound
c     Current interval
      INTEGER iInterval
c     Return type
      LOGICAL IsInLiquidPhase
c     Given temperature in K
      REAL Temp 
c     Given Pressure Pa
      REAL Pressure
c     Minimun Pressure required for liquid phase.  
      REAL fSaturationPressure
c     SLope of interpolation line.
      REAL SLOPE 
c     Saturation curve data array.
      REAL T(37) 
      REAL P(37)
      
c     Water saturation curve..kelvin vs pa.
c     Obtained from Engineering Thermodynamics (Reynolds, Perkins 1977) 
      DATA T(1)/273.010/,     P(1)/   611.3 /      
      DATA T(2)/275.0 /,     P(2)/   705.6 /
      DATA T(3)/278.0 /,     P(3)/   872.1 /
      DATA T(4)/283.0 /,     P(4)/  1228.0 /
      DATA T(5)/288.0 /,     P(5)/  1705.0 /
      DATA T(6)/293.0 /,     P(6)/  2338.0 /
      DATA T(7)/298.0 /,     P(7)/  3169.0 /
      DATA T(8)/303.0 /,     P(8)/  4246.0 /
      DATA T(9)/308.0 /,     P(9)/  5628.0 /
      DATA T(10)/313.0 /,    P(10)/ 7383.0 /
      DATA T(11)/318.0 /,    P(11)/ 9593.0 /
      DATA T(12)/323.0 /,    P(12)/ 12350.0 /
      DATA T(13)/328.0 /,    P(13)/ 15760.0 /
      DATA T(14)/333.0 /,    P(14)/ 19940.0 /
      DATA T(15)/338.0 /,    P(15)/ 25030. 0/
      DATA T(16)/343.0 /,    P(16)/ 31190.0 /
      DATA T(17)/348.0 /,    P(17)/ 38580.0 /
      DATA T(18)/353.0 /,    P(18)/ 47390.0 /
      DATA T(19)/358.0 /,    P(19)/ 57830.0 /
      DATA T(20)/363.0 /,    P(20)/ 70130.0 /
      DATA T(21)/368.0 /,    P(21)/ 84550.0 /
      DATA T(22)/373.0 /,    P(22)/101300.0 /
      DATA T(23)/383.0 /,    P(23)/143300.0 /
      DATA T(24)/393.0 /,    P(24)/198500.0 /
      DATA T(25)/403.0 /,    P(25)/270100.0 /
      DATA T(26)/413.0 /,    P(26)/361300.0 /
      DATA T(27)/423.0 /,    P(27)/475800.0 /   
      DATA T(28)/433.0 /,    P(28)/617800.0 /
      DATA T(29)/443.0 /,    P(29)/791600.0 /
      DATA T(30)/448.0 /,    P(30)/892000.0 /
      DATA T(31)/453.0 /,    P(31)/1002100.0 /
      DATA T(32)/458.0 /,    P(32)/1122700.0 /
      DATA T(33)/463.0 /,    P(33)/1254400.0 /
      DATA T(34)/468.0 /,    P(34)/1397800.0 /
      DATA T(35)/473.0 /,    P(35)/1553800.0 /
      DATA T(36)/478.0 /,    P(36)/1723000.0 /
      DATA T(37)/483.0 /,    P(37)/1906200.0 /
c     set current interval to zero. 
      iInterval = 0
c     check if within range (curve is define only as above, since usuing linear 
c     interpolation, must be bound between two points. 
      IF (Temp .lt.483 
     &     .or. Temp .gt. 273.010
     &     .or. Pressure .lt. 1906200.0
     &     .or. Pressure .gt. 611.3 ) 
     &     THEN

c     set check to false that is has not found the interval yet.
         iIntervalFound = 0 
         DO WHILE  (iIntervalFound .eq. 0)
c     Interate through temperature array.
            iInterval = iInterval + 1
c     check to see if current interval has a range that contains given temp.
            If (T(iInterval) .gt. Temp) THEN
c     If so flag it found.
               iIntervalFound = 1
               
c     Determine matching pressure using linear interpolation between to points.
               SLOPE = ( ( P(iInterval) - P(iInterval-1) )
     &              /( T(iInterval) - T(iInterval-1) ))
c     using a linear equation determine required pressure.
               fSaturationPressure = P(iInterval-1)+ 
     &              SLOPE* (Temp-T(iInterval-1))
c     Check to see if given pressure is high enough to maintain a liquid phase.
               IF (Pressure .ge. fSaturationPressure) THEN
c     Then it is in liquid phase.
                  IsInLiquidPhase = .true.
               ELSE
c     Then it is gas phase.
                  IsInLiquidPhase = .false.
               ENDIF
            ENDIF
         END DO
         
      ELSE
c     Tell user that the pressure or/and temperature they entered is out of range,
         write (*,*)'Water temp and Pressure must be within limits'
         write (*,*)'273.01 K < Temp< 483.0 K     : T = ',Temp
         write (*,*)'611.3 pa < Temp< 1906200.0 pa : P = ',Pressure        
         STOP
      ENDIF
      RETURN
      END


C **********************************H20LiquidCp ***************************************
c Percent Difference Test
C Created by: Phylroy Lopez
C Initial Creation Date: November, 2002
C Copyright CETC 2001
c     This function will determine if the percent difference between two real*8 numbers 
c     is within the specified tolerance.
C INPUTS: 
c     REAL Temp  ! Temperature in Kelvin
c     REAL Pressure       ! in pascals.  
C OUTPUT: 
c     REAL  H20LiquidCP    joules/(kg Kelvin)
C --------------------------------------------------------------------------------    
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      FUNCTION H20LiquidCp(Temp, Pressure)
      implicit none
      REAL H20LiquidCp
      REAL H20LiquidDensity
      REAL IAPWSCP
      REAL Temp
      REAL Pressure
      REAL CV
      H20LiquidCp = 
     &     REAL(IAPWSCP(H20LiquidDensity(Temp, Pressure),Temp,CV))
      RETURN
      END
C***************************************************************************************



C **********************************H20LiquidDensity ***************************************
c Percent Difference Test
C Created by: Phylroy Lopez
C Initial Creation Date: November, 2002
C Copyright CETC 2001
c     This function will determine if the percent difference between two real*8 numbers 
c     is within the specified tolerance.
C INPUTS: 
c     REAL Temp  ! the number to be compared against.
c     REAL Pressure       ! the number to be comapred to.
c     REAL fTolerance ! the percentage difference that is tolerable.  
C OUTPUT: 
c     REAL  H20LiquidDensity   
C --------------------------------------------------------------------------------    
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      FUNCTION H20LiquidDensity(Temp, Pressure)
      implicit none
      REAL H20LiquidDensity
      LOGICAL IsInLiquidPhase
      REAL Temp
      REAL Pressure
      REAL Limit
      REAL density
      INTEGER nit
      nit = 100
      Limit = 1.0e-10
      if (IsInLiquidPhase(Temp, Pressure)) THEN
         call UNWRAPDENL(Temp,Pressure,density,nit,limit)
      ELSE
         write (*,*)' Water has boiled!! Temp = ', Temp, 'Pressure = '
     &        ,Pressure
         STOP
      ENDIF
      H20LiquidDensity = density
      RETURN
      END
C***************************************************************************************
C **********************************H20GasDensity ***************************************
c Percent Difference Test
C Created by: Phylroy Lopez
C Initial Creation Date: November, 2002
C Copyright CETC 2001
c     This function will determine the density of water steam 
c     for a given  pressure and temperature..
C INPUTS: 
c     REAL Temp  ! Temperature of gas
c     REAL Pressure       ! Pressure of gas
C OUTPUT: 
c     REAL  H20SteamDensity   
C --------------------------------------------------------------------------------    
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      FUNCTION H20GasDensity(Temp, Pressure)
      implicit none
      REAL H20GasDensity
      LOGICAL IsInLiquidPhase
      REAL Temp
      REAL Pressure
      REAL Limit
      INTEGER nit
      nit = 100
      Limit = 1.0 
      
      
      if (.not.IsInLiquidPhase(Temp, Pressure)) THEN
         call UNWRAPDEN(Temp,Pressure,H20GasDensity,nit,limit)
      ELSE
         write (*,*)' Steam has Condensed!'
         STOP
      ENDIF
      RETURN
      END
C***************************************************************************************

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C     THESE ROUTINES ARE BASED ON THE IAPWS 1995 STEAM AND WATER      C 
C     FORMULATIONS FOR GENERAL AND SCIENTIFIC USE                     C
C     (http://www.iapws.org/)                                         C
C                                                                     C
C     DUNCAN SIMPSON            CAMBRIDGE              30/05/2001     C
C                                                                     C
C     VERSION 1                 CREATED                               C
C     VERSION 2                 ADDED UNWRAPEN,UNWRAPDEN,DPDR,sound2  C
C                               CHANGED CP SO IT ALSO GIVES CV        C
C                                                                     C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C                                                                     C
C     THE CODE CAN BE SPLIT INTO FOUR SECTIONS.                       C
C     THE FIRST SECTION IS THE MOST USEFUL AS THESE ARE THE ROUTINES  C
C         TO FIND THE STEAM PROPERTIES AND THERE RELATIONS            C
C     THE SECOND FINDS THE DERIVATIVES                                C
C     THE THIRD IS THE COEFFICIENTS                                   C
C     FOURTH - UNWRAPING ROUTINES                                     C
C                                                                     C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC


C*************************** SECTION ONE *****************************C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
     

      FUNCTION IAPWSPRESS(RHO,T)
c     Finds the Pressure (Pa)
      implicit none
#include "WaterProperties.h"
      REAL TC,R,PRDELTA,PHIRDELTA,TAU,DELTA
      REAL RHOC
      COMMON /CRIT/ TC,R,RHOC
 
      TAU = TC/T
      DELTA = RHO/RHOC

      PRDELTA=PHIRDELTA(TAU,DELTA)

      IAPWSPRESS= RHO*R*T*(1. +DELTA*PRDELTA)

      RETURN
      END
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      FUNCTION IAPWSENERGY(RHO,T)
c     Finds the internal energy (J/Kg)
      implicit none
#include "WaterProperties.h"
      REAL TC,R,TAU,POTAU,PRTAU,PHIOTAU,PHIRTAU,DELTA
      REAL RHOC
      COMMON /CRIT/ TC,R,RHOC

      TAU = TC/T
      DELTA = RHO/RHOC     

      POTAU=PHIOTAU(TAU)
      PRTAU=PHIRTAU(TAU,DELTA)

      IAPWSENERGY=T*R*TAU*(POTAU+PRTAU)

      RETURN
      END
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      FUNCTION IAPWSENTR(RHO,T)
c     Finds the the Entropy (J/(KgK))
      implicit none
#include "WaterProperties.h"
      REAL TC,R,PHIOTAU,PHIRTAU,POTAU,PRTAU,PHIO,PHIR,TAU,DELTA,PO,PR
      REAL RHOC
      COMMON /CRIT/ TC,R,RHOC

      TAU = TC/T
      DELTA = RHO/RHOC 

      POTAU=PHIOTAU(TAU)
      PRTAU=PHIRTAU(TAU,DELTA)
      PO=PHIO(TAU,DELTA)
      PR=PHIR(TAU,DELTA)
      
      IAPWSENTR=R*(TAU*(POTAU+PRTAU)-PO-PR)

      RETURN
      END
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      FUNCTION IAPWSENTH(RHO,T)
c     Finds the enthalpy (J/Kg)
      implicit none
#include "WaterProperties.h"
      REAL TC,R,TAU,DELTA,PHIOTAU,POTAU,PHIRTAU,PRTAU,PHIRDELTA,
     &     PRDELTA
      REAL RHOC
      COMMON /CRIT/ TC,R,RHOC

      TAU = TC/T
      DELTA = RHO/RHOC 

      POTAU=PHIOTAU(TAU)
      PRTAU=PHIRTAU(TAU,DELTA)     
      PRDELTA=PHIRDELTA(TAU,DELTA)

      IAPWSENTH=T*R*(1.  + TAU*(POTAU+PRTAU)+DELTA*PRDELTA)

      RETURN
      END
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      FUNCTION IAPWSCV(RHO,T)
c     Finds the Isochoric heat capacity (J/(KgK))
      implicit none
#include "WaterProperties.h"
      REAL TC,R,TAU,DELTA,PHIOTAUTAU,POTAUTAU,PHIRTAUTAU,PRTAUTAU
      REAL RHOC
      COMMON /CRIT/ TC,R,RHOC

      TAU = TC/T
      DELTA = RHO/RHOC

      POTAUTAU=PHIOTAUTAU(TAU)
      PRTAUTAU=PHIRTAUTAU(TAU,DELTA)
      
      IAPWSCV=R*(-TAU*TAU*(POTAUTAU+PRTAUTAU))

      RETURN
      END
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      FUNCTION IAPWSCP(RHO,T,CV)
c     Finds the Isobaric heat capacity (J/(KgK))
c     As a side it also finds the Isochoric heat capacity (J/(KgK))
c     this could speed up some programs.
c     NOTE - CV is an output NOT an input
      implicit none
#include "WaterProperties.h"
      REAL TC,R,TAU,DELTA,PHIRDELTATAU,PRDELTA,PHIRDELTA,PRDELTADELTA,
     &     PHIRDELTADELTA,PRDELTATAU
      REAL RHOC
      COMMON /CRIT/ TC,R,RHOC

      TAU = TC/T
      DELTA = RHO/RHOC

      PRDELTA=PHIRDELTA(TAU,DELTA)
      PRDELTADELTA=PHIRDELTADELTA(TAU,DELTA)
      PRDELTATAU=PHIRDELTATAU(TAU,DELTA)
      
      CV=IAPWSCV(RHO,T)

      IAPWSCP=CV+R*( (1. +DELTA*PRDELTA-DELTA*TAU*PRDELTATAU)**2 
     &     /(1. +2. *DELTA*PRDELTA + DELTA**2 *PRDELTADELTA))

c      IAPWSCP=REAL(R*(-TAU*TAU*(POTAUTAU+PRTAUTAU)+
c     &        ( (1+DELTA*PRDELTA-DELTA*TAU*PRDELTATAU)*
c     &          (1+DELTA*PRDELTA-DELTA*TAU*PRDELTATAU) )/
c     &     (1+2*DELTA*PRDELTA+DELTA*DELTA*PRDELTADELTA)))

      RETURN
      END
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      FUNCTION IAPWSSOUND(RHO,T)
C     Finds the speed of sound (m/s)
      implicit none
#include "WaterProperties.h"
      REAL TC,R,TAU,DELTA,POTAUTAU,PHIOTAUTAU,PRTAUTAU,PHIRTAUTAU,
     &     PRDELTADELTA,PHIRDELTADELTA,PRDELTATAU,PHIRDELTATAU,
     &     PHIRDELTA,PRDELTA
      REAL RHOC
      COMMON /CRIT/ TC,R,RHOC

      TAU = TC/T
      DELTA = RHO/RHOC

      POTAUTAU=PHIOTAUTAU(TAU)
      PRTAUTAU=PHIRTAUTAU(TAU,DELTA)
      PRDELTA=PHIRDELTA(TAU,DELTA)
      PRDELTADELTA=PHIRDELTADELTA(TAU,DELTA)
      PRDELTATAU=PHIRDELTATAU(TAU,DELTA)

      IAPWSSOUND=T*R*((1.  + 2. *DELTA*PRDELTA + 
     &     DELTA*DELTA*PRDELTADELTA -
     &     ( (1. +DELTA*PRDELTA-DELTA*TAU*PRDELTATAU) *
     &       (1. +DELTA*PRDELTA-DELTA*TAU*PRDELTATAU) )/
     &     (TAU*TAU*(POTAUTAU+PRTAUTAU))))

c       IAPWSSOUND=SQRT(IAPWSSOUND)     
        IAPWSSOUND=SQRT(IAPWSSOUND)
      RETURN
      END
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      FUNCTION IAPWSSOUND2(RHO,T)
C     Finds the square of the speed of sound (m/s)
      implicit none
#include "WaterProperties.h"
      REAL TC,R,TAU,DELTA,POTAUTAU,PHIOTAUTAU,PRTAUTAU,PHIRTAUTAU,
     &     PRDELTADELTA,PHIRDELTADELTA,PRDELTATAU,PHIRDELTATAU,
     &     PHIRDELTA,PRDELTA
      REAL RHOC
      COMMON /CRIT/ TC,R,RHOC

      TAU = TC/T
      DELTA = RHO/RHOC

      POTAUTAU=PHIOTAUTAU(TAU)
      PRTAUTAU=PHIRTAUTAU(TAU,DELTA)
      PRDELTA=PHIRDELTA(TAU,DELTA)
      PRDELTADELTA=PHIRDELTADELTA(TAU,DELTA)
      PRDELTATAU=PHIRDELTATAU(TAU,DELTA)

      IAPWSSOUND2=T*R*((1.  + 2. *DELTA*PRDELTA + 
     &     DELTA*DELTA*PRDELTADELTA -
     &     ( (1. +DELTA*PRDELTA-DELTA*TAU*PRDELTATAU) *
     &       (1. +DELTA*PRDELTA-DELTA*TAU*PRDELTATAU) )/
     &     (TAU*TAU*(POTAUTAU+PRTAUTAU))))

c       IAPWSSOUND=SQRT(IAPWSSOUND)     

      RETURN
      END
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      FUNCTION IAPWSJOULE(RHO,T)
C     Finds the Joule-Thomson coefficient(dT/dP)_h  ()
      implicit none
#include "WaterProperties.h"
      REAL TC,R,TAU,DELTA,POTAUTAU,PHIOTAUTAU,PRTAUTAU,PHIRTAUTAU,
     &     PRDELTA,PHIRDELTA,PRDELTADELTA,PHIRDELTADELTA,PRDELTATAU,
     &     PHIRDELTATAU
      REAL RHOC
      COMMON /CRIT/ TC,R,RHOC

      TAU = TC/T
      DELTA = RHO/RHOC

      POTAUTAU=PHIOTAUTAU(TAU)
      PRTAUTAU=PHIRTAUTAU(TAU,DELTA)
      PRDELTA=PHIRDELTA(TAU,DELTA)
      PRDELTADELTA=PHIRDELTADELTA(TAU,DELTA)
      PRDELTATAU=PHIRDELTATAU(TAU,DELTA)

      IAPWSJOULE = -(DELTA*PRDELTA + DELTA*DELTA*PRDELTADELTA + 
     &                                           DELTA*TAU*PRDELTATAU) /
     &     (  (1.  + DELTA*PRDELTA - DELTA*TAU*PRDELTATAU)**2.  -
     &        TAU*TAU*(POTAUTAU + PRTAUTAU)*
     &       (1.  + 2. *DELTA*PRDELTA + DELTA*DELTA*PRDELTADELTA)  ) /
     &     (RHO*R)

      RETURN
      END
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      FUNCTION IAPWSTHROTTLE(RHO,T)
C     Finds the Isothermal throttling coefficient(dh/dp)_T ()
      implicit none
#include "WaterProperties.h"
      REAL TC,R,TAU,DELTA,PRDELTA,PHIRDELTA,PRDELTADELTA,
     &     PHIRDELTADELTA,PRDELTATAU,PHIRDELTATAU
      REAL RHOC
      COMMON /CRIT/ TC,R,RHOC

      TAU = TC/T
      DELTA = RHO/RHOC

      PRDELTA=PHIRDELTA(TAU,DELTA)
      PRDELTADELTA=PHIRDELTADELTA(TAU,DELTA)
      PRDELTATAU=PHIRDELTATAU(TAU,DELTA)

      IAPWSTHROTTLE =
     &     (1.  + DELTA*PRDELTA - DELTA*TAU*PRDELTATAU) /
     &     (1.  + 2. *DELTA*PRDELTA + DELTA*DELTA*PRDELTADELTA)/
     &     RHO

      RETURN
      END
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      FUNCTION IAPWSBETA(RHO,T)
C     Finds the Isentropic temperature-pressure coefficient(dT/dp)_s ()
      implicit none
#include "WaterProperties.h"
      REAL TC,R,TAU,DELTA,POTAUTAU,PHIOTAUTAU,PRTAUTAU,PHIRTAUTAU,
     &     PRDELTA,PHIRDELTA,PRDELTADELTA,PHIRDELTADELTA,PRDELTATAU,
     &     PHIRDELTATAU
      REAL RHOC
      COMMON /CRIT/ TC,R,RHOC

      TAU = TC/T
      DELTA = RHO/RHOC

      POTAUTAU=PHIOTAUTAU(TAU)
      PRTAUTAU=PHIRTAUTAU(TAU,DELTA)
      PRDELTA=PHIRDELTA(TAU,DELTA)
      PRDELTADELTA=PHIRDELTADELTA(TAU,DELTA)
      PRDELTATAU=PHIRDELTATAU(TAU,DELTA)

      IAPWSBETA = (1.  + DELTA*PRDELTA - DELTA*TAU*PRDELTATAU)/
     &     (  (1.  + DELTA*PRDELTA - DELTA*TAU*PRDELTATAU)*
     &        (1.  + DELTA*PRDELTA - DELTA*TAU*PRDELTATAU) -
     &        TAU*TAU*(POTAUTAU + PRTAUTAU)*
     &       (1.  + 2. *DELTA*PRDELTA + DELTA*DELTA*PRDELTADELTA)  ) /
     &     (RHO*R)

      RETURN
      END
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      FUNCTION IAPWSDPDR(RHO,T,P)
c     Finds (dpdr)_t
c     it also outputs P(Pa) so as to speed up UNWRAPDEN.
c     P is an output NOT an input
      implicit none
#include "WaterProperties.h"
      REAL TC,R,TAU,DELTA,PRDELTA,PHIRDELTA,PRDELTADELTA,
     &     PHIRDELTADELTA,bit3
      REAL RHOC
      COMMON /CRIT/ TC,R,RHOC

      TAU = TC/T
      DELTA = RHO/RHOC

      PRDELTA=PHIRDELTA(TAU,DELTA)
      PRDELTADELTA=PHIRDELTADELTA(TAU,DELTA)
      
      BIT3=1.  + 2. *DELTA*PRDELTA + DELTA*DELTA*PRDELTADELTA

      P=RHO*T*R*(1. +DELTA*PRDELTA)
      IAPWSDPDR=T*R*BIT3

      RETURN
      END
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      FUNCTION IAPWSDPDT(RHO,T,P)
c     Finds (dpdt)_r
c     it also outputs P(Pa) so as to speed up UNWRAPDEN.
c     P is an output NOT an input
      implicit none
      REAL RHOC
#include "WaterProperties.h"
      REAL TC,R,TAU,DELTA,PRDELTA,PHIRDELTA, PRDELTATAU,PHIRDELTATAU
      COMMON /CRIT/ TC,R,RHOC

      TAU = TC/T
      DELTA = RHO/RHOC

      PRDELTA=PHIRDELTA(TAU,DELTA)
      PRDELTATAU=PHIRDELTATAU(TAU,DELTA)

      P=RHO*T*R*(1. +DELTA*PRDELTA)
      IAPWSDPDT=R*RHO*(1.  + DELTA*PRDELTA - DELTA*TAU*PRDELTATAU)

      RETURN
      END
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC



C*************************** SECTION TWO *****************************C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      FUNCTION PHIO(TAU,DELTA)
C     Finds the ideal part of the dimensionless Helmholtz free energy
      REAL NIO(8),GAMMAIO(4:8),PHIO,TAU,DELTA
      INTEGER I
      COMMON /SCIENCE1/ NIO,GAMMAIO

      PHIO = LOG(DELTA) + NIO(1) + NIO(2)*TAU + NIO(3)*LOG(TAU)
      DO I=4,8
         PHIO=PHIO + NIO(I)*LOG( 1.  - EXP(-GAMMAIO(I)*TAU) )
      ENDDO
      
      RETURN
      END
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      FUNCTION PHIODELTA(DELTA)
C     Finds d(phi_o)/ddelta
      REAL PHIODELTA,DELTA

      PHIODELTA = 1. /DELTA
      
      RETURN
      END
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      FUNCTION PHIODELTADELTA(DELTA)
C     Finds d2(phi_o)/ddelta2
      REAL PHIODELTADELTA,DELTA

      PHIODELTADELTA = -1. /(DELTA*DELTA)
      
      RETURN
      END
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      FUNCTION PHIOTAU(TAU)
C     Finds d(phi_o)/dtau
      REAL NIO(8),GAMMAIO(4:8),TAU,PHIOTAU
      INTEGER I
      COMMON /SCIENCE1/ NIO,GAMMAIO
      
      PHIOTAU = NIO(2) + NIO(3)/TAU
      DO I=4,8
         PHIOTAU = PHIOTAU + NIO(I)*GAMMAIO(I)*
     &              (1. /(1.  - EXP(-GAMMAIO(I)*TAU)) - 1. )
      ENDDO
      
      RETURN
      END
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      FUNCTION PHIOTAUTAU(TAU)
C     Finds d2(phi_o)/dtau2
      REAL NIO(8),GAMMAIO(4:8),TAU,PHIOTAUTAU
      INTEGER I
      COMMON /SCIENCE1/ NIO,GAMMAIO

      PHIOTAUTAU = - NIO(3)/(TAU*TAU)
   
      DO I=4,8
         PHIOTAUTAU = PHIOTAUTAU - NIO(I)*GAMMAIO(I)*GAMMAIO(I)*
     &                             EXP(-GAMMAIO(I)*TAU) /
     &        ( (1. -EXP(-GAMMAIO(I)*TAU))*
     &          (1. -EXP(-GAMMAIO(I)*TAU)) )
      ENDDO

      RETURN
      END
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      FUNCTION PHIR(TAU,DELTA)
C     Finds the residual part of the dimensionless Helmholtz free energy
      implicit none
      REAL TI(54),NI(56),GAMMAI(52:54),AI(55:56),BI(55:56),OMEGA,
     &     BIGBI(55:56),BIGAI(55:56),PHIR,CAPDELTA,PSI,BETAI(52:56),
     &     DELTA,TAU
      REAL CI(8:51),DI(54),BIGCI(55:56),BIGDI(55:56),ALPHAI(52:54),
     &     EPSILONI(52:54),FNOMEGA,FNCAPDELTA,FNPSI
      INTEGER I     
      COMMON /SCIENCE2/ TI,NI,BETAI,GAMMAI,AI,BI,BIGBI,BIGAI,CI,DI,
     &     ALPHAI,BIGCI,BIGDI,EPSILONI

      PHIR=0. 
      DO I=1,7
         PHIR = PHIR + NI(I)*(DELTA**DI(I))*(TAU**TI(I))
      ENDDO

      DO I=8,51
         PHIR = PHIR + NI(I)*(DELTA**DI(I))*(TAU**TI(I))*
     &                 EXP(-DELTA**CI(I))
      ENDDO

      DO I=52,54
         PHIR = PHIR + NI(I)*(DELTA**DI(I))*(TAU**TI(I))*
     &                 EXP( -ALPHAI(I)*
     &                      (DELTA-EPSILONI(I))*(DELTA-EPSILONI(I)) -
     &                      BETAI(I)*(TAU-GAMMAI(I))*(TAU-GAMMAI(I)) )
      ENDDO

      DO I=55,56
         OMEGA=FNOMEGA(TAU,DELTA,I)
         CAPDELTA=FNCAPDELTA(OMEGA,DELTA,I)
         PSI = FNPSI(TAU,DELTA,I)

         PHIR = PHIR + NI(I)*(CAPDELTA**BI(I))*DELTA*PSI 
      ENDDO

      RETURN
      END
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      FUNCTION PHIRDELTA(TAU,DELTA)
C     Finds d(phi_r)/ddelta
      implicit none
      REAL TI(54),NI(56),GAMMAI(52:54),AI(55:56),BI(55:56),OMEGA,
     &     BIGBI(55:56),BIGAI(55:56),TAU,DELTA,BETAI(52:56),PHIRDELTA,
     &     CAPDELTA,DPSID,DCAPD,DCAPBD,PSI
      REAL CI(8:51),DI(54),BIGCI(55:56),BIGDI(55:56),ALPHAI(52:54),
     &     EPSILONI(52:54),FNOMEGA,FNCAPDELTA,FNPSI,FNDPSID,FNDCAPD,
     &     FNDCAPBD
      INTEGER I    
      COMMON /SCIENCE2/ TI,NI,BETAI,GAMMAI,AI,BI,
     &     BIGBI,BIGAI,CI,DI,ALPHAI,BIGCI,BIGDI,EPSILONI

      PHIRDELTA = 0. 
      DO i=1,7
         PHIRDELTA = PHIRDELTA + 
     &               NI(I)*DI(I)*DELTA**(DI(I)-1. )*TAU**TI(I)
      ENDDO

      DO i=8,51
         PHIRDELTA = PHIRDELTA + NI(I)*EXP(-DELTA**CI(I))*
     &                           (DELTA**(DI(I)-1. )*TAU**TI(I)*
     &                           (DI(I)-CI(I)*DELTA**CI(I)))   
      ENDDO
 
      DO i=52,54
         PHIRDELTA = PHIRDELTA + NI(I)*DELTA**DI(I)*TAU**TI(I)*
     &                           EXP( -ALPHAI(I)*
     &                      (DELTA-EPSILONI(I))**2.  -
     &                      BETAI(I)*(TAU-GAMMAI(I))**2.  ) *
     &                (DI(I)/DELTA - 2. *ALPHAI(I)*(DELTA-EPSILONI(I)))
      ENDDO

      DO i=55,56
         OMEGA=FNOMEGA(TAU,DELTA,I)
         CAPDELTA=FNCAPDELTA(OMEGA,DELTA,I)
         PSI = FNPSI(TAU,DELTA,I)

         DPSID = FNDPSID(PSI,DELTA,I)
         DCAPD = FNDCAPD(OMEGA,DELTA,I)
         DCAPBD = FNDCAPBD(CAPDELTA,DCAPD,I)

         PHIRDELTA = PHIRDELTA + NI(I)*(CAPDELTA**BI(I)*
     &                           (PSI +DELTA*DPSID)+DCAPBD*DELTA*PSI) 
      ENDDO    

      RETURN
      END
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      FUNCTION PHIRDELTADELTA(TAU,DELTA)
C     Finds d2(phi_r)/ddelta2
      implicit none
      REAL TI(54),NI(56),GAMMAI(52:54),AI(55:56),BI(55:56),CAPDELTA,
     &     BIGBI(55:56),BIGAI(55:56),BETAI(52:56),TAU,DELTA,OMEGA,
     &     PHIRDELTADELTA,PSI,DPSID,DCAPBD,DCAPD,DPSIDD,DCAPBDD,DCAPDD
      REAL CI(8:51),DI(54),BIGCI(55:56),BIGDI(55:56),ALPHAI(52:54),
     &     EPSILONI(52:54),FNOMEGA,FNCAPDELTA,FNPSI,FNDPSID,FNDCAPD,
     &     FNDCAPBD
      INTEGER I   
      COMMON /SCIENCE2/ TI,NI,BETAI,GAMMAI,AI,BI,
     &     BIGBI,BIGAI,CI,DI,ALPHAI,BIGCI,BIGDI,EPSILONI

      PHIRDELTADELTA=0. 
      DO I=1,7
         PHIRDELTADELTA = PHIRDELTADELTA + 
     &        NI(I)*DI(I)*(DI(I)-1. )*DELTA**(DI(I)-2. )*TAU**TI(I)
      ENDDO

      DO I=8,51
         PHIRDELTADELTA = PHIRDELTADELTA + 
     &        NI(I)*EXP(-DELTA**CI(I))*
     &      (DELTA**(DI(I)-2. )*TAU**TI(I)*((DI(I)-CI(I)*DELTA**CI(I))*
     &      (DI(I)-1. -CI(I)*DELTA**CI(I))-CI(I)*CI(I)*DELTA**CI(I)))
      ENDDO

      DO I=52,54
         PHIRDELTADELTA = PHIRDELTADELTA + NI(I)*TAU**TI(I)*
     &        EXP( -ALPHAI(I)*(DELTA-EPSILONI(I))*(DELTA-EPSILONI(I)) -
     &                      BETAI(I)*(TAU-GAMMAI(I))*(TAU-GAMMAI(I)) )*
     &        ( -2. *ALPHAI(I)*DELTA**DI(I)+
     &        4. *ALPHAI(I)*ALPHAI(I)*DELTA**DI(I)*(DELTA-EPSILONI(I))*
     &                                           (DELTA-EPSILONI(I))-
     &     4. *DI(I)*ALPHAI(I)*DELTA**(DI(I)-1. )*(DELTA-EPSILONI(I))+
     &     DI(I)*(DI(I)-1. )*DELTA**(DI(I)-2. ) )
      ENDDO

      DO I=55,56
         OMEGA=FNOMEGA(TAU,DELTA,I)
         CAPDELTA=FNCAPDELTA(OMEGA,DELTA,I)
         PSI = FNPSI(TAU,DELTA,I)

         DPSID = FNDPSID(PSI,DELTA,I)
         DCAPD = FNDCAPD(OMEGA,DELTA,I)
         DCAPBD = FNDCAPBD(CAPDELTA,DCAPD,I)

         DPSIDD=(2. *BIGCI(I)*(DELTA-1. )*(DELTA-1. ) - 1. )
     &        *2. *BIGCI(I)*PSI

         DCAPDD=DCAPD/(DELTA-1. ) + (DELTA-1. )*(DELTA-1. )*
     &        (4. *BIGBI(I)*AI(I)*(AI(I)-1. )*
     &        ((DELTA-1. )*(DELTA-1. ))**(AI(I)-2. ) +
     &        2. *BIGAI(I)*BIGAI(I)/(BETAI(I)*BETAI(I))*
     & (((DELTA-1. )*(DELTA-1. ))**(1. /(2. *BETAI(I))-1. ))**2.  
     &        +BIGAI(I)*OMEGA*4. /BETAI(I)*(1. /(2. *BETAI(I))-1. )*
     &       ((DELTA-1. )*(DELTA-1. ))**(1. /(2. *BETAI(I))-2. )  )

         DCAPBDD=BI(I)*(CAPDELTA**(BI(I)-1. )*DCAPDD+
     &        (BI(I)-1. )*CAPDELTA**(BI(I)-2. )*DCAPD*DCAPD)

         PHIRDELTADELTA = PHIRDELTADELTA + 
     &        NI(I)*(CAPDELTA**BI(I)*(2. *DPSID+DELTA*DPSIDD)+
     &        2. *DCAPBD*(PSI+DELTA*DPSID)+DCAPBDD*DELTA*PSI)
      ENDDO

      RETURN
      END
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      FUNCTION PHIRTAU(TAU,DELTA)
C     Finds d(phi_r)/dtau
      implicit none
      REAL TI(54),NI(56),GAMMAI(52:54),AI(55:56),BI(55:56),OMEGA,
     &     BIGBI(55:56),BIGAI(55:56),BETAI(52:56),TAU,DELTA,PHIRTAU,
     &     CAPDELTA,PSI,DCAPBT,DPSIT
      REAL CI(8:51),DI(54),BIGCI(55:56),BIGDI(55:56),ALPHAI(52:54),
     &     EPSILONI(52:54),FNOMEGA,FNCAPDELTA,FNPSI,FNDCAPBT,FNDPSIT
      INTEGER I      
      COMMON /SCIENCE2/ TI,NI,BETAI,GAMMAI,AI,BI,
     &     BIGBI,BIGAI,CI,DI,ALPHAI,BIGCI,BIGDI,EPSILONI

      PHIRTAU=0. 
      DO I=1,7
         PHIRTAU=PHIRTAU + NI(I)*TI(I)*DELTA**DI(I)*TAU**(TI(I)-1. )
      ENDDO

      DO I=8,51
         PHIRTAU=PHIRTAU + 
     &    NI(I)*TI(I)*DELTA**DI(I)*TAU**(TI(I)-1. )*EXP(-DELTA**CI(I))
      ENDDO

      DO I=52,54
         PHIRTAU=PHIRTAU + NI(I)*DELTA**DI(I)*TAU**TI(I)*
     &       EXP( -ALPHAI(I)*(DELTA-EPSILONI(I))*(DELTA-EPSILONI(I)) -
     &                      BETAI(I)*(TAU-GAMMAI(I))*(TAU-GAMMAI(I)) )*
     &        (TI(I)/TAU-2. *BETAI(I)*(TAU-GAMMAI(I)))
      ENDDO

      DO I=55,56
         OMEGA=FNOMEGA(TAU,DELTA,I)
         CAPDELTA=FNCAPDELTA(OMEGA,DELTA,I)
         PSI = FNPSI(TAU,DELTA,I)

         DCAPBT=FNDCAPBT(OMEGA,CAPDELTA,I)
         DPSIT=FNDPSIT(PSI,TAU,I)

         PHIRTAU=PHIRTAU + 
     &        NI(I)*DELTA*(DCAPBT*PSI+CAPDELTA**BI(I)*DPSIT)
      ENDDO

      RETURN
      END
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      FUNCTION PHIRTAUTAU(TAU,DELTA)
C     Finds d2(phi_r)/dtau2
      implicit none
      REAL TI(54),NI(56),GAMMAI(52:54),AI(55:56),BI(55:56),OMEGA,
     &     BIGBI(55:56),BIGAI(55:56),BETAI(52:56),TAU,DELTA,PHIRTAUTAU,
     &     CAPDELTA,PSI,DCAPBT,DPSIT,DPSITT,DCAPBTT
      REAL CI(8:51),DI(54),BIGCI(55:56),BIGDI(55:56),ALPHAI(52:54),
     &     EPSILONI(52:54),FNOMEGA,FNCAPDELTA,FNPSI,FNDCAPBT,FNDPSIT
      INTEGER I     
      COMMON /SCIENCE2/ TI,NI,BETAI,GAMMAI,AI,BI,
     &     BIGBI,BIGAI,CI,DI,ALPHAI,BIGCI,BIGDI,EPSILONI

      PHIRTAUTAU=0. 
      DO I=1,7
         PHIRTAUTAU = PHIRTAUTAU + 
     &        NI(I)*TI(I)*(TI(I)-1. )*DELTA**DI(I)*TAU**(TI(I)-2. )
      ENDDO

      DO I=8,51
         PHIRTAUTAU = PHIRTAUTAU + 
     &        NI(I)*TI(I)*(TI(I)-1. )*DELTA**DI(I)*TAU**(TI(I)-2. )*
     &        EXP(-DELTA**CI(I))
      ENDDO

      DO I=52,54
         PHIRTAUTAU = PHIRTAUTAU + NI(I)*DELTA**DI(I)*TAU**TI(I)*
     &        EXP( -ALPHAI(I)*(DELTA-EPSILONI(I))*(DELTA-EPSILONI(I)) -
     &                      BETAI(I)*(TAU-GAMMAI(I))*(TAU-GAMMAI(I)) )*
     &        ((TI(I)/TAU-2. *BETAI(I)*(TAU-GAMMAI(I)))**2.  - 
     &        TI(I)/(TAU*TAU) - 2. *BETAI(I))
      ENDDO

      DO I=55,56
         OMEGA=FNOMEGA(TAU,DELTA,I)
         CAPDELTA=FNCAPDELTA(OMEGA,DELTA,I)
         PSI = FNPSI(TAU,DELTA,I)

         DCAPBT=FNDCAPBT(OMEGA,CAPDELTA,I)
         DPSIT=FNDPSIT(PSI,TAU,I)
         
         DPSITT=(2. *BIGDI(I)*(TAU-1. )*(TAU-1. )-1. )
     &        *2. *BIGDI(I)*PSI
         DCAPBTT=2. *BI(I)*CAPDELTA**(BI(I)-1. ) +
     &        4. *OMEGA*OMEGA*BI(I)*(BI(I)-1. )*CAPDELTA**(BI(I)-2. )

         PHIRTAUTAU = PHIRTAUTAU + NI(I)*DELTA*
     &        (DCAPBTT*PSI + 2. *DCAPBT*DPSIT + CAPDELTA**BI(I)*DPSITT)
      ENDDO

      RETURN
      END
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      FUNCTION PHIRDELTATAU(TAU,DELTA)
C     Finds d2(phi_r)/ddeltadtau
      implicit none
      REAL TI(54),NI(56),GAMMAI(52:54),AI(55:56),BI(55:56),DCAPBDT,
     &    BIGBI(55:56),BIGAI(55:56),BETAI(52:56),TAU,DELTA,PHIRDELTATAU,
     &     PSI,OMEGA,CAPDELTA,DCAPBT,DPSIT,DCAPD,DPSID,DCAPBD,DPSIDT
      REAL CI(8:51),DI(54),BIGCI(55:56),BIGDI(55:56),ALPHAI(52:54),
     &     EPSILONI(52:54),FNOMEGA,FNCAPDELTA,FNPSI,FNDPSID,FNDCAPD,
     &     FNDCAPBD,FNDCAPBT,FNDPSIT
      INTEGER I   
      COMMON /SCIENCE2/ TI,NI,BETAI,GAMMAI,AI,BI,
     &     BIGBI,BIGAI,CI,DI,ALPHAI,BIGCI,BIGDI,EPSILONI

      PHIRDELTATAU=0. 
      DO I=1,7
         PHIRDELTATAU = PHIRDELTATAU + 
     &        NI(I)*DI(I)*TI(I)*DELTA**(DI(I)-1. )*TAU**(TI(I)-1. )
      ENDDO

      DO I=8,51
         PHIRDELTATAU = PHIRDELTATAU + 
     &        NI(I)*TI(I)*DELTA**(DI(I)-1. )*TAU**(TI(I)-1. )*
     &        (DI(I)-CI(I)*DELTA**CI(I))*EXP(-DELTA**CI(I))
      ENDDO

      DO I=52,54
         PHIRDELTATAU = PHIRDELTATAU + NI(I)*DELTA**DI(I)*TAU**TI(I)*
     &        EXP( -ALPHAI(I)*(DELTA-EPSILONI(I))*(DELTA-EPSILONI(I)) -
     &                      BETAI(I)*(TAU-GAMMAI(I))*(TAU-GAMMAI(I)) )*
     &        (DI(I)/DELTA-2. *ALPHAI(I)*(DELTA-EPSILONI(I)))*
     &        (TI(I)/TAU-2. *BETAI(I)*(TAU-GAMMAI(I)))
      ENDDO

      DO I=55,56
         OMEGA=FNOMEGA(TAU,DELTA,I)
         CAPDELTA=FNCAPDELTA(OMEGA,DELTA,I)
         PSI = FNPSI(TAU,DELTA,I)

         DCAPBT=FNDCAPBT(OMEGA,CAPDELTA,I)
         DPSIT=FNDPSIT(PSI,TAU,I)

         DPSID = FNDPSID(PSI,DELTA,I)
         DCAPD = FNDCAPD(OMEGA,DELTA,I)
         DCAPBD = FNDCAPBD(CAPDELTA,DCAPD,I)         

         DPSIDT=4. *BIGCI(I)*BIGDI(I)*(DELTA-1. )*(TAU-1. )*PSI
         DCAPBDT=-BIGAI(I)*BI(I)*2. /BETAI(I)*CAPDELTA**(BI(I)-1. )*
     &        (DELTA-1. )*
     &        ((DELTA-1. )*(DELTA-1. ))**(1. /(2. *BETAI(I))-1. ) -
     &        2. *OMEGA*BI(I)*(BI(I)-1. )*CAPDELTA**(BI(I)-2. )*DCAPD

         PHIRDELTATAU = PHIRDELTATAU + NI(I)*
     &        (CAPDELTA**BI(I)*(DPSIT+DELTA*DPSIDT) +
     &        DELTA*DCAPBD*DPSIT +
     &        DCAPBT*(PSI+DELTA*DPSID)+DCAPBDT*DELTA*PSI)
      ENDDO

      RETURN
      END
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      FUNCTION FNPSI(TAU,DELTA,I)
      implicit none
      REAL TI(54),NI(56),GAMMAI(52:54),AI(55:56),BI(55:56),
     &     BIGBI(55:56),BIGAI(55:56),BETAI(52:56)
      REAL CI(8:51),DI(54),BIGCI(55:56),BIGDI(55:56),ALPHAI(52:54),
     &     EPSILONI(52:54),FNPSI,TAU,DELTA
      INTEGER I
      COMMON /SCIENCE2/ TI,NI,BETAI,GAMMAI,AI,BI,BIGBI,BIGAI,CI,DI,
     &     ALPHAI,BIGCI,BIGDI,EPSILONI

      FNPSI = 
     &    EXP(-BIGCI(I)*(DELTA-1. )**2.  - BIGDI(I)*(TAU-1. )**2. )

      RETURN
      END
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      FUNCTION FNOMEGA(TAU,DELTA,I)
      implicit none
      REAL TI(54),NI(56),GAMMAI(52:54),AI(55:56),BI(55:56),
     &     BIGBI(55:56),BIGAI(55:56),BETAI(52:56)
      REAL CI(8:51),DI(54),BIGCI(55:56),BIGDI(55:56),ALPHAI(52:54),
     &     EPSILONI(52:54),FNOMEGA,TAU,DELTA
      INTEGER I
      COMMON /SCIENCE2/ TI,NI,BETAI,GAMMAI,AI,BI,BIGBI,BIGAI,CI,DI,
     &     ALPHAI,BIGCI,BIGDI,EPSILONI

      FNOMEGA = (1. -TAU) + BIGAI(I)*
     &     ((DELTA-1. )**2. )**(1. /(2. *BETAI(I)))

      RETURN
      END
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      FUNCTION FNCAPDELTA(OMEGA,DELTA,I)
      implicit none
      REAL TI(54),NI(56),GAMMAI(52:54),AI(55:56),BI(55:56),
     &     BIGBI(55:56),BIGAI(55:56),BETAI(52:56)
      REAL CI(8:51),DI(54),BIGCI(55:56),BIGDI(55:56),ALPHAI(52:54),
     &     EPSILONI(52:54),FNCAPDELTA,DELTA,OMEGA
      INTEGER I
      COMMON /SCIENCE2/ TI,NI,BETAI,GAMMAI,AI,BI,BIGBI,BIGAI,CI,DI,
     &     ALPHAI,BIGCI,BIGDI,EPSILONI

      FNCAPDELTA = OMEGA*OMEGA + 
     &     BIGBI(I)*((DELTA-1. )**2)**AI(I)

      RETURN
      END
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      FUNCTION FNDCAPBD(CAPDELTA,DCAPD,I)
      implicit none
      REAL TI(54),NI(56),GAMMAI(52:54),AI(55:56),BI(55:56),
     &     BIGBI(55:56),BIGAI(55:56),BETAI(52:56)
      REAL CI(8:51),DI(54),BIGCI(55:56),BIGDI(55:56),ALPHAI(52:54),
     &     EPSILONI(52:54),FNDCAPBD,DCAPD,CAPDELTA
      INTEGER I
      COMMON /SCIENCE2/ TI,NI,BETAI,GAMMAI,AI,BI,BIGBI,BIGAI,CI,DI,
     &     ALPHAI,BIGCI,BIGDI,EPSILONI

      FNDCAPBD = BI(I)*CAPDELTA**(BI(I)-1. )*DCAPD
c      IF(CAPDELTA.EQ.0 )FNDCAPBD=1  ! if t=TC

      RETURN
      END
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      FUNCTION FNDCAPD(OMEGA,DELTA,I)
      implicit none
      REAL TI(54),NI(56),GAMMAI(52:54),AI(55:56),BI(55:56),
     &     BIGBI(55:56),BIGAI(55:56),BETAI(52:56)
      REAL CI(8:51),DI(54),BIGCI(55:56),BIGDI(55:56),ALPHAI(52:54),
     &     EPSILONI(52:54),FNDCAPD,DELTA,OMEGA
      INTEGER I
      COMMON /SCIENCE2/ TI,NI,BETAI,GAMMAI,AI,BI,BIGBI,BIGAI,CI,DI,
     &     ALPHAI,BIGCI,BIGDI,EPSILONI

      FNDCAPD  = (DELTA-1. )*
     &           (BIGAI(I)*OMEGA*2. /BETAI(I)*
     &        ((DELTA-1. )**2. )**(1. /(2. *BETAI(I))-1. )+
     &    2. *BIGBI(I)*AI(I)*((DELTA-1. )**2. )**(AI(I)-1. ))

      RETURN
      END
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      FUNCTION FNDPSID(PSI,DELTA,I)
      implicit none
      REAL TI(54),NI(56),GAMMAI(52:54),AI(55:56),BI(55:56),
     &     BIGBI(55:56),BIGAI(55:56),BETAI(52:56)
      REAL CI(8:51),DI(54),BIGCI(55:56),BIGDI(55:56),ALPHAI(52:54),
     &     EPSILONI(52:54),FNDPSID,DELTA,PSI
      INTEGER I
      COMMON /SCIENCE2/ TI,NI,BETAI,GAMMAI,AI,BI,BIGBI,BIGAI,CI,DI,
     &     ALPHAI,BIGCI,BIGDI,EPSILONI

      FNDPSID = -2. *BIGCI(I)*(DELTA-1. )*PSI

      RETURN
      END
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      FUNCTION FNDPSIT(PSI,TAU,I)
      implicit none
      REAL TI(54),NI(56),GAMMAI(52:54),AI(55:56),BI(55:56),
     &     BIGBI(55:56),BIGAI(55:56),BETAI(52:56)
      REAL CI(8:51),DI(54),BIGCI(55:56),BIGDI(55:56),ALPHAI(52:54),
     &     EPSILONI(52:54),FNDPSIT,PSI,TAU
      INTEGER I
      COMMON /SCIENCE2/ TI,NI,BETAI,GAMMAI,AI,BI,BIGBI,BIGAI,CI,DI,
     &     ALPHAI,BIGCI,BIGDI,EPSILONI

      FNDPSIT=-2. *BIGDI(I)*(TAU-1. )*PSI

      RETURN
      END
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      FUNCTION FNDCAPBT(OMEGA,CAPDELTA,I)
      implicit none
      REAL TI(54),NI(56),GAMMAI(52:54),AI(55:56),BI(55:56),
     &     BIGBI(55:56),BIGAI(55:56),BETAI(52:56)
      REAL CI(8:51),DI(54),BIGCI(55:56),BIGDI(55:56),ALPHAI(52:54),
     &     EPSILONI(52:54),FNDCAPBT,OMEGA,CAPDELTA
      INTEGER I
      COMMON /SCIENCE2/ TI,NI,BETAI,GAMMAI,AI,BI,BIGBI,BIGAI,CI,DI,
     &     ALPHAI,BIGCI,BIGDI,EPSILONI

      FNDCAPBT=-2. *OMEGA*BI(I)*CAPDELTA**(BI(I)-1. )

c      if(CAPDELTA.eq.0 )FNDCAPBT=1  !to deal with t=TC

      RETURN
      END
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC







C*************************** SECTION THREE ***************************C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      BLOCK DATA IPAWSSCIENCE
      implicit none

      REAL TC,R
      REAL NIO(8),GAMMAIO(4:8)
      REAL CI(8:51),DI(54),BIGCI(55:56),BIGDI(55:56),ALPHAI(52:54),
     &     RHOC,EPSILONI(52:54)
      REAL TI(54),NI(56),GAMMAI(52:54),AI(55:56),
     &     BETAI(52:56),BI(55:56),BIGBI(55:56),BIGAI(55:56)

      COMMON /CRIT/ TC,R,RHOC
      COMMON /SCIENCE1/ NIO,GAMMAIO
      COMMON /SCIENCE2/ TI,NI,BETAI,GAMMAI,AI,BI,
     &     BIGBI,BIGAI,CI,DI,ALPHAI,BIGCI,BIGDI,EPSILONI

      
      DATA TC/647.096 /,RHOC/322. /,R/0.46151805D3/

      DATA NIO(1)/-8.32044648201 /
      DATA NIO(2)/ 6.6832105268  /
      DATA NIO(3)/ 3.00632       /
      DATA NIO(4)/ 0.012436      /,GAMMAIO(4)/ 1.28728967 /
      DATA NIO(5)/ 0.97315       /,GAMMAIO(5)/ 3.53734222 /
      DATA NIO(6)/ 1.27950       /,GAMMAIO(6)/ 7.74073708 /
      DATA NIO(7)/ 0.96956       /,GAMMAIO(7)/ 9.24437796 /
      DATA NIO(8)/ 0.24873       /,GAMMAIO(8)/27.5075105 /

      DATA         DI(1) / 1. /,TI(1)/-0.5 /,
     &                                     NI(1) / 0.12533547935523E-1/
      DATA         DI(2) / 1. /,TI(2)/ 0.875 /,
     &                                     NI(2) / 0.78957634722828D1/
      DATA         DI(3) / 1. /,TI(3)/ 1.0 /,     
     &                                     NI(3) /-0.87803203303561D1/
      DATA         DI(4) / 2. /,TI(4)/ 0.5 /,  
     &                                     NI(4) / 0.31802509345418 /
      DATA         DI(5) / 2. /,TI(5)/ 0.75 /, 
     &                                     NI(5) /-0.26145533859358 /
      DATA         DI(6) / 3. /,TI(6)/0.375 /,
     &                                     NI(6) /-0.78199751687981E-2/
      DATA         DI(7) / 4. /,TI(7)/ 1. /,   
     &                                     NI(7) / 0.88089493102134E-2/
      DATA CI(8) /1. /,DI(8) / 1. /,TI(8) / 4. /,
     &                                     NI(8) /-0.66856572307965 /
      DATA CI(9) /1. /,DI(9) / 1. /,TI(9) / 6. /,
     &                                     NI(9) / 0.20433810950965 /
      DATA CI(10)/1. /,DI(10)/1. /,TI(10)/12. /,
     &                                     NI(10)/-0.66212605039687E-4/
      DATA CI(11)/1. /,DI(11)/ 2. /,TI(11)/ 1. /,
     &                                     NI(11)/-0.19232721156002 /
      DATA CI(12)/1. /,DI(12)/ 2. /,TI(12)/ 5. /,
     &                                     NI(12)/-0.25709043003438 /
      DATA CI(13)/1. /,DI(13)/ 3. /,TI(13)/ 4. /,
     &                                     NI(13)/ 0.16074868486251 /
      DATA CI(14)/1. /,DI(14)/4. /,TI(14)/ 2. /,
     &                                     NI(14)/-0.40092828925807E-1/
      DATA CI(15)/1. /,DI(15)/4. /,TI(15)/13. /,
     &                                     NI(15)/ 0.39343422603254E-6/
      DATA CI(16)/1. /,DI(16)/5. /,TI(16)/ 9. /,
     &                                     NI(16)/-0.75941377088144E-5/
      DATA CI(17)/1. /,DI(17)/7. /,TI(17)/ 3. /,
     &                                     NI(17)/ 0.56250979351888E-3/
      DATA CI(18)/1. /,DI(18)/9. /,TI(18)/ 4. /,
     &                                     NI(18)/-0.15608652257135E-4/
      DATA CI(19)/1. /,DI(19)/10. /,TI(19)/11. /,
     &                                     NI(19)/0.11537996422951E-8/
      DATA CI(20)/1. /,DI(20)/11. /,TI(20)/4. /,
     &                                     NI(20)/ 0.36582165144204E-6/
      DATA CI(21)/1. /,DI(21)/13. /,TI(21)/13. /,
     &                                     NI(21)/-0.13251180074668E-11/
      DATA CI(22)/1. /,DI(22)/15. /,TI(22)/1. /,
     &                                     NI(22)/-0.62639586912454E-9/
      DATA CI(23)/2. /,DI(23)/ 1. /,TI(23)/ 7. /,
     &                                     NI(23)/-0.10793600908932 /
      DATA CI(24)/2. /,DI(24)/ 2. /,TI(24)/1. /,
     &                                     NI(24)/ 0.17611491008752E-1/
      DATA CI(25)/2. /,DI(25)/ 2. /,TI(25)/ 9. /,
     &                                     NI(25)/ 0.22132295167546 /
      DATA CI(26)/2. /,DI(26)/ 2. /,TI(26)/10. /,
     &                                     NI(26)/-0.40247669763528 /
      DATA CI(27)/2. /,DI(27)/ 3. /,TI(27)/10. /,
     &                                     NI(27)/ 0.58083399985759 /
      DATA CI(28)/2. /,DI(28)/ 4. /,TI(28)/3. /,
     &                                     NI(28)/ 0.49969146990806E-2/
      DATA CI(29)/2. /,DI(29)/ 4. /,TI(29)/7. /,
     &                                     NI(29)/-0.31358700712549E-1/
      DATA CI(30)/2. /,DI(30)/ 4. /,TI(30)/10. /,
     &                                     NI(30)/-0.74315929710341 /
      DATA CI(31)/2. /,DI(31)/ 5. /,TI(31)/10. /,
     &                                     NI(31)/ 0.47807329915480 /
      DATA CI(32)/2. /,DI(32)/ 6. /,TI(32)/6. /,
     &                                     NI(32)/ 0.20527940895948E-1/
      DATA CI(33)/2. /,DI(33)/ 6. /,TI(33)/10. /,
     &                                     NI(33)/-0.13636435110343 /
      DATA CI(34)/2. /,DI(34)/ 7. /,TI(34)/10. /,
     &                                     NI(34)/0.14180634400617E-1/
      DATA CI(35)/2. /,DI(35)/ 9. /,TI(35)/1. /,
     &                                     NI(35)/ 0.83326504880713E-2/
      DATA CI(36)/2. /,DI(36)/ 9. /,TI(36)/2. /,
     &                                     NI(36)/-0.29052336009585E-1/
      DATA CI(37)/2. /,DI(37)/ 9. /,TI(37)/3. /,
     &                                     NI(37)/ 0.38615085574206E-1/
      DATA CI(38)/2. /,DI(38)/ 9. /,TI(38)/4. /,
     &                                     NI(38)/-0.20393486513704E-1/
      DATA CI(39)/2. /,DI(39)/ 9. /,TI(39)/8. /,
     &                                     NI(39)/-0.16554050063734E-2/
      DATA CI(40)/2. /,DI(40)/10. /,TI(40)/6. /,
     &                                     NI(40)/ 0.19955571979541E-2/
      DATA CI(41)/2. /,DI(41)/10. /,TI(41)/9. /,
     &                                     NI(41)/ 0.15870308324157E-3/
      DATA CI(42)/2. /,DI(42)/12. /,TI(42)/8. /,
     &                                     NI(42)/-0.16388568342530E-4/
      DATA CI(43)/3. /,DI(43)/3. /,TI(43)/16. /,
     &                                     NI(43)/ 0.43613615723811E-1/
      DATA CI(44)/3. /,DI(44)/4. /,TI(44)/22. /,
     &                                     NI(44)/ 0.34994005463765E-1/
      DATA CI(45)/3. /,DI(45)/4. /,TI(45)/23. /,
     &                                     NI(45)/-0.76788197844621E-1/
      DATA CI(46)/3. /,DI(46)/5. /,TI(46)/23. /,
     &                                     NI(46)/ 0.22446277332006E-1/
      DATA CI(47)/4. /,DI(47)/14. /,TI(47)/10. /,
     &                                     NI(47)/-0.62689710414685E-4/
      DATA CI(48)/6. /,DI(48)/3. /,TI(48)/50. /,
     &                                     NI(48)/-0.55711118565645E-9/
      DATA CI(49)/6. /,DI(49)/ 6. /,TI(49)/44. /,
     &                                     NI(49)/-0.19905718354408 /
      DATA CI(50)/6. /,DI(50)/ 6. /,TI(50)/46. /,
     &                                     NI(50)/ 0.31777497330738 /
      DATA CI(51)/6. /,DI(51)/ 6. /,TI(51)/50. /,
     &                                     NI(51)/-0.11841182425981 /

      DATA           DI(52)/3. /, TI(52)/0. /,
     &                                     NI(52)/-0.31306260323435D2/
      DATA ALPHAI(52)/20. /,BETAI(52)/150. /
      DATA GAMMAI(52)/1.21 /, EPSILONI(52)/1. /

      DATA           DI(53)/3. /, TI(53)/1. /,      
     &                                     NI(53)/0.31546140237781D2/
      DATA ALPHAI(53)/20. /,BETAI(53)/150. /
      DATA GAMMAI(53)/1.21 /, EPSILONI(53)/1. /

      DATA           DI(54)/3. /, TI(54)/4. /,
     &                                     NI(54)/-0.25213154341695D4/
      DATA ALPHAI(54)/20. /,BETAI(54)/250. /
      DATA GAMMAI(54)/1.25 /, EPSILONI(54)/1. /

      DATA AI(55)/3.5 /,      BI(55)/0.85 /,     BIGBI(55)/0.2 /,
     &     NI(55)/-0.14874640856724 /,            BIGCI(55)/28. /,  
     &     BIGDI(55)/700. /,     BIGAI(55)/0.32 /,  BETAI(55)/0.3 /
      DATA AI(56)/3.5 /,      BI(56)/0.95 /,     BIGBI(56)/0.2 /,
     &     NI(56)/ 0.31806110878444 /,            BIGCI(56)/32. /,
     &     BIGDI(56)/800. /,     BIGAI(56)/0.32 /,  BETAI(56)/0.3 /

      END
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC




C*************************** SECTION FOUR ****************************C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      SUBROUTINE UNWRAPDENL(T,P,rho,nit,limit)
C     This will find density(kg/m**3) based on the temperature and 
c     pressure using a Newton-Raphson method.
C     THIS ROUTINE SHOULD BE USED SPARINGLY AS IT IS VERY TIME CONSUMING
C     OUTPUT is  r - the density
C     INPUTS are T - temp
c                P - pressure
c                nit - max. iterations
c                limit - max. percent. of drho/r
      implicit none
#include "WaterProperties.h"
      INTEGER i,nit
c give an estimate on liquid density
      rho = 1000.00
      lim = limit/100. 

      DO i = 1,nit
c      WRITE(*,*)'*** UNWRAPING DENSITY PROBLEM ***',rho,t,p/1D3
         dpdr=IAPWSDPDR(Rho,T,P1)
         drho= (P - P1) / dpdr
         rho = rho + drho
         drho=ABS(drho/rho)
cReal         drho=ABS(drho/rho)
         IF(drho.lt.lim)return
      ENDDO


      RETURN
      END
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      SUBROUTINE UNWRAPDEN(T,P,rho,nit,limit)
C     This will find density(kg/m**3) based on the temperature and 
c     pressure using a Newton-Raphson method.
C     THIS ROUTINE SHOULD BE USED SPARINGLY AS IT IS VERY TIME CONSUMING
C     OUTPUT is  r - the density
C     INPUTS are T - temp
c                P - pressure
c                nit - max. iterations
c                limit - max. percent. of drho/r
      implicit none
#include "WaterProperties.h"
      INTEGER i,nit
c     gives a quick estimate of density
      rho=p/(461.51 *t)
      lim = limit/100. 

      DO i = 1,100
c      WRITE(*,*)'*** UNWRAPING DENSITY PROBLEM ***',rho,t,p/1D3
         dpdr=IAPWSDPDR(Rho,T,P1)
         drho= (P - P1) / dpdr
         rho = rho + drho
         drho=ABS(drho/rho)
cReal         drho=ABS(drho/rho)
         IF(drho.lt.lim)return
      ENDDO


      RETURN
      END
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      SUBROUTINE UNWRAPEN(T,u,rho,nit,limit)
C     This will find temperature(k) based on the energy and 
c     density using a Newton-Raphson method.
C     THIS ROUTINE SHOULD BE USED SPARINGLY AS IT IS VERY TIME CONSUMING
C     OUTPUT is  T - Temp
C     INPUTS are u - energy
c                rho - density
c                nit - max. iterations
c                limit - max. percent. of dt/t

**********************************

C     THIS SUBROUTINE HAS A REAL PROBLEM FOR CONTIONS ABOUT 50 DEGRESS 
C     BELOW THE SATURATURTION LINE. AS THIS USES A NEWTON-RAPHSON METHOD 
C     IT CAN HAVE PROBLEM NEAR A MAX., MIN OR INFLECTION AS OCCURS IN 
C     THE TWO PHASE REGION. THEREFORE THIS NEEDS RE-WRITING TO USE 
C     ANOTHER MORE RELIABLE METHOD.

**********************************
      implicit none
#include "WaterProperties.h"
      INTEGER i,nit

      lim=limit/100. 
      DO i = 1,nit
         u1 = IAPWSENERGY(rho,T)
         cv=IAPWSCV(Rho,T)

         dt= (u - u1)/cv
         t = t +dt
         dt=ABS(dt/t)
cReal         dt=ABS(dt/t)
         IF(dt.lt.lim)goto 10
      ENDDO
      WRITE(*,*)'*** UNWRAPING ENERGY PROBLEM ***'
 10   continue

      RETURN
      END
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      SUBROUTINE UNWRAPPRESS(T,p,rho,nit,limit)
C     This will find temperature(k) based on the pressure and 
c     density using a Newton-Raphson method.
C     THIS ROUTINE SHOULD BE USED SPARINGLY AS IT IS VERY TIME CONSUMING
C     OUTPUT is  T - Temp
C     INPUTS are p - pressure
c                r - density
c                nit - max. iterations
c                limit - max. percent. of dt/t

      implicit none
#include "WaterProperties.h"
      INTEGER i,nit

c     gives a quick estimate of density
      t=p/(461.51 *rho)
       
      lim=limit/100. 
      DO i = 1,100
c         p2=IAPWSPRESS(rho,T)
         dpdt = IAPWSDPDT(rho,T,p2)
         dt = (p-p2)/dpdt

         t = t +dt
         dt=ABS(dt/t)
cReal         dt=ABS(dt/t)
         IF(dt.lt.lim)goto 10
      ENDDO
      WRITE(*,*)'*** UNWRAPING PRESSURE PROBLEM ***'
 10   continue

      RETURN
      END
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      SUBROUTINE UNWRAPENTR(T,p,s,nit,limit)
C     This will find temperature(k) based on the pressure and 
c     density using a Newton-Raphson method.
C     THIS ROUTINE SHOULD BE USED SPARINGLY AS IT IS VERY TIME CONSUMING
C     OUTPUT is  T - Temp
C     INPUTS are p - pressure
c                s - entropy
c                nit - max. iterations
c                limit - max. percent. of dt/t

      implicit none
#include "WaterProperties.h"
      INTEGER i,nit

c     gives a quick estimate of temperature
c      t=500. 

      lim=limit/100. 
      DO i = 1,nit
         CALL UNWRAPDEN(T,P,RHO,300,0.00001 )
         s2=IAPWSENTR(RHO,T)
         Cp=IAPWSCP(RHO,T,CV)
         dsdt = Cp/t
         dt = (s-s2)/dsdt

         t = t +dt

c         write(70,*)dt,s,s2,t

         dt=ABS(dt/t)
cReal         dt=ABS(dt/t)
         IF(dt.lt.lim)goto 10
      ENDDO
      WRITE(*,*)'*** UNWRAPING ENTROPY PROBLEM ***'
 10   continue

c      write(70,*)
      RETURN
      END
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCccccccccccccccccc

      FUNCTION FUNPV(T,tau)
C     THIS CALCULATES THE SATURATION VAPOUR PRESSURE IN (Pa) BASED ON 
C     THE IPAWS 1992 SUPPLEMENTARY RELEASE ON SATURATION PROPERTIES

      implicit none
      REAL Tc,Pc,RHOc,ALPHA0,a(6),atop(6) 
      REAL T,FUNPV,tau
      INTEGER i
      COMMON /CRIT1/ Tc,Pc,RHOc,ALPHA0
      COMMON /PRESSURE/ a,atop

      FUNPV=0.
      DO i=1,6
         FUNPV = REAL(DBLE(FUNPV) + a(i)*DBLE(tau)**atop(i))
      ENDDO

      FUNPV = REAL(Pc * exp(Tc*DBLE(FUNPV/T)))

      RETURN
      END
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      FUNCTION FUNRHOL(tau)
C     THIS CALCULATES THE SATURATED LIQUID DENSITY IN (Kg/m**3) BASED ON 
C     THE IPAWS 1992 SUPPLEMENTARY RELEASE ON SATURATION PROPERTIES
      implicit none
      REAL Tc,Pc,RHOc,ALPHA0,b(6),btop(6)
      REAL tau,FUNRHOL
      INTEGER i
      COMMON /CRIT1/ Tc,Pc,RHOc,ALPHA0
      COMMON /DENSITYL/ b,btop

      FUNRHOL=1.
      DO i=1,6
         FUNRHOL = REAL(DBLE(FUNRHOL) + b(i)*DBLE(tau)**btop(i))
      ENDDO

      FUNRHOL=REAL(RHOc*DBLE(FUNRHOL))

      RETURN
      END
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      FUNCTION FUNRHOV(tau)
C     THIS CALCULATES THE SATURATED VAPOUR DENSITY IN (Kg/m**3) BASED ON 
C     THE IPAWS 1992 SUPPLEMENTARY RELEASE ON SATURATION PROPERTIES
      implicit none
      REAL Tc,Pc,RHOc,ALPHA0,c(6),ctop(6)
      REAL tau,FUNRHOV
      INTEGER i
      COMMON /CRIT1/ Tc,Pc,RHOc,ALPHA0
      COMMON /DENSITYV/ c,ctop

      FUNRHOV=0.
      DO i=1,6
         FUNRHOV = REAL(DBLE(FUNRHOV) + c(i)*DBLE(tau)**ctop(i))
      ENDDO

      FUNRHOV=REAL(RHOc * DBLE(exp(FUNRHOV)))

      RETURN
      END
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      FUNCTION FUNAUX(T)
C     THIS IS AN AUXILIARY EQUATION SO THAT THE ENTHAPLIES CAN BE 
C     CALCULATED. IT IS BASED ON THE IPAWS 1992 SUPPLEMENTARY RELEASE 
C     ON SATURATION PROPERTIES
      implicit none
      REAL Tc,Pc,RHOc,ALPHA0,d(5),dtop(5),dalpha,dphi,phi0
      REAL T,FUNAUX,theta
      INTEGER i
      COMMON /CRIT1/ Tc,Pc,RHOc,ALPHA0
      COMMON /AUX/ d,dtop,dalpha,dphi,phi0

      theta = REAL(DBLE(T)/Tc)

      FUNAUX=REAL(dalpha)
      DO i=1,5
         FUNAUX = REAL(DBLE(FUNAUX) + d(i)*DBLE(theta)**dtop(i))
      ENDDO      

      FUNAUX = REAL(ALPHA0 * DBLE(FUNAUX))

      RETURN
      END
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      FUNCTION FUNAUXPHI(T)
C     THIS IS AN AUXILIARY EQUATION SO THAT THE ENTROPIES CAN BE 
C     CALCULATED. IT IS BASED ON THE IPAWS 1992 SUPPLEMENTARY RELEASE 
C     ON SATURATION PROPERTIES   

      implicit none
      real Tc,Pc,RHOc,ALPHA0,d(5),dtop(5),dalpha,dphi,phi0
      real theta,t,FUNAUXPHI
      COMMON /CRIT1/ Tc,Pc,RHOc,ALPHA0
      COMMON /AUX/ d,dtop,dalpha,dphi,phi0

      theta = REAL(DBLE(T)/Tc)

      FUNAUXPHI=REAL(dphi)
      FUNAUXPHI = FUNAUXPHI + REAL(19 /20 *d(1)*theta**(-20 ))
      FUNAUXPHI = FUNAUXPHI + REAL(d(2)*LOG(theta))
      FUNAUXPHI = FUNAUXPHI + REAL(9 /7 *d(3)*theta**3.5 )
      FUNAUXPHI = FUNAUXPHI + REAL(5 /4 *d(4)*theta**4 )
      FUNAUXPHI = FUNAUXPHI + REAL(109 /107 *d(5)*theta**53.5 )

      FUNAUXPHI = REAL(PHI0 * DBLE(FUNAUXPHI))

      RETURN
      END
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      FUNCTION FUNTAU(T)
C     THIS IS AN AUXILIARY EQUATION WHICH IS NEEDED BY MANY OF THE 
C     EQUATIONS AND IS FROM THE IPAWS 1992 SUPPLEMENTARY RELEASE 
C     ON SATURATION PROPERTIES
      implicit none

      REAL Tc,Pc,RHOc,ALPHA0
      REAL T,FUNTAU
      COMMON /CRIT1/ Tc,Pc,RHOc,ALPHA0

      FUNTAU = 1. - REAL(DBLE(T)/Tc)

      RETURN
      END
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      FUNCTION FUNDPDTSAT(T,Psat)
C     THIS CALCULATES (DP/DT). THIS IS NEEDED FOR THE CALCULATION OF 
C     THE ENTHALPIES. THIS IS BASED ON THE WORK FROM THE  IPAWS 1992 
C     SUPPLEMENTARY RELEASE ON SATURATION PROPERTIES
C
C     BEFORE THIS FUNCTION IS FIRST RUN INITIALDPDT MUST BE RUN TO FIND
C     VALUE FOR THE COMMON BLOCK /DPDT/
      implicit none
      REAL Tc,Pc,RHOc,ALPHA0,a(6),atop(6)
      REAL alpha(6),beta(6),gamma(6),delta(6),epsilon(6),eta(6)
      REAL T,Psat,FUNDPDTSAT
      INTEGER i
      COMMON /CRIT1/ Tc,Pc,RHOc,ALPHA0
      COMMON /PRESSURE/ a,atop
      COMMON /DPDT/alpha,beta,gamma,delta,epsilon,eta

      FUNDPDTSAT=REAL(-a(1)*tc/DBLE(t*t))
      DO i=2,6
         FUNDPDTSAT=REAL(DBLE(FUNDPDTSAT) + 
     &        (alpha(i)*(beta(i)*DBLE(t)**gamma(i) 
     &        +  delta(i)*DBLE(t)**epsilon(i)) ) *
     &        abs(tc*DBLE(t)**epsilon(i) - 
     &        DBLE(t)**(-delta(i)))**eta(i))
      ENDDO

      FUNDPDTSAT = FUNDPDTSAT*Psat     

      RETURN
      END
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      FUNCTION FUNHFG(RHOL,RHOV,DPDT,T)
C     THIS CALCULATES THE SPECFIC ENTHALPY OF EVAPOURATION (J/Kg)
C     NOTE THIS ROUTINE OS FASTER THAN CALCULTING HL AND HV AND 
C      SUPTRACTING THE TWO.

      implicit none
      REAL RHOL,RHOV,DPDT,T,FUNHFG

      FUNHFG = (1./RHOV - 1./RHOL)*T*DPDT

      RETURN
      END
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      FUNCTION FUNHL(RHOL,DPDT,ALPHA,T)
C     THIS CALCULATES THE ***SATURATED*** LIQUID ENTHAPLY (J/Kg) BASED ON 
C     THE IPAWS 1992 SUPPLEMENTARY RELEASE ON SATURATION PROPERTIES
      implicit none
      REAL RHOL,DPDT,ALPHA,T,FUNHL

      FUNHL=ALPHA + T/RHOL * DPDT

      RETURN
      END
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      FUNCTION FUNHV(RHOV,DPDT,ALPHA,T)
C     THIS CALCULATES THE ***SATURATED*** VAPOUR ENTHAPLY (J/Kg) BASED ON 
C     THE IPAWS 1992 SUPPLEMENTARY RELEASE ON SATURATION PROPERTIES
      implicit none
      REAL RHOV,DPDT,ALPHA,T,FUNHV

      FUNHV=ALPHA + T/RHOV * DPDT

      RETURN
      END
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      FUNCTION FUNSL(RHOL,DPDT,PHI,T)
C     THIS CALCULATES THE ***SATURATED*** LIQUID ENTROPY (J/Kg K) BASED ON 
C     THE IPAWS 1992 SUPPLEMENTARY RELEASE ON SATURATION PROPERTIES
      implicit none
      REAL RHOL,DPDT,PHI,T,FUNSL

      FUNSL=PHI + 1 /RHOL * DPDT

      RETURN
      END
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      FUNCTION FUNSV(RHOV,DPDT,PHI,T)
C     THIS CALCULATES THE ***SATURATED*** VAPOUR ENTROPY (J/Kg K) BASED ON 
C     THE IPAWS 1992 SUPPLEMENTARY RELEASE ON SATURATION PROPERTIES
      implicit none
      REAL RHOV,DPDT,PHI,T,FUNSV

      FUNSV=PHI + 1 /RHOV * DPDT

      RETURN
      END
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      FUNCTION FUNSURF(tau)
C     THIS CALCULATES THE SURFACE TENSION OF WATER (N/m) BASED ON THE 
C     IPAWS 1994 RELEASE
      implicit none
      REAL S1,S2,S3
      REAL tau,FUNSURF
      COMMON /SURF/ S1,S2,S3     

      FUNSURF = REAL(S1*(1+S3*DBLE(tau))*DBLE(tau)**S2)

      RETURN
      END
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      FUNCTION FUNTSAT(P,lim,lim2)
C     THIS CALULATES THE THE SATURATION TEMPERATURE FOR A 
C     GIVEN PRESSURE, HOWEVER SEE NOTE BELOW AND USE FUNTSATTEMP
      
      implicit none

      REAL FUNTSAT,P,PSAT,DPDT,FUNTAU,FUNPV,FUNDPDTSAT,TAU,FUNTSPTEMP,
     &     terror,lim2
      INTEGER i,lim
      REAL Tc,Pc,RHOc,ALPHA0

      COMMON /CRIT1/ Tc,Pc,RHOc,ALPHA0

C     THIS IS A VERY VERY SLOW FUNCTION
C     DO NOT USE IN MAIN PROGRAM, FIND FASTER APPROXIMATE CODE
C     LIM SHOULD NOT NEED TO BE LARGER THAN 10

      FUNTSAT = FUNTSPTEMP(P)
      IF(FUNTSAT.GT.TC)FUNTSAT=REAL(TC)
      DO i=1,lim
         tau   = FUNTAU(FUNTSAT)
         Psat  = FUNPV(FUNTSAT,tau)
         DPDT  = FUNDPDTSAT(FUNTSAT,Psat)
         terror = (P-PSAT)/DPDT
         FUNTSAT = FUNTSAT + terror
         IF(FUNTSAT.GT.TC)FUNTSAT=REAL(TC)
c         write(69,*)funtsat,p,terror

         IF(ABS(terror*100/FUNTSAT).LT.LIM2)GOTO 10
      ENDDO
      write(*,*)'** ERROR FINDING TSAT**'

 10   CONTINUE

      RETURN
      END
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      FUNCTION FUNVIS(T,RHO)
C     THIS CALCULATES THE VISCOSITY OF WATER BASED ON THE 1997 
C     IPAWS RELEASE.
      implicit none
      REAL T,RHO,Tbar,RHObar,FUNVIS
      REAL  MU1,MU0,MUbar,H1(0:3),H2(0:5,0:6),Tstar,RHOstar,RHObit,
     &     Tbit
      INTEGER i
      COMMON /VIS/ Tstar,RHOstar,H1,H2,MUbar

      Tbar   = REAL(DBLE(T) / Tstar)
      RHObar = REAL(DBLE(RHO) / RHOstar)

      MU0=H1(0)
      MU1=DBLE(0.)

      DO i=1,3
         MU0=MU0 + H1(i)/(DBLE(Tbar)**i)
      ENDDO
      MU0=DBLE(SQRT(Tbar))/MU0

      Tbit   = DBLE(1./Tbar - 1.)
      RHObit = DBLE(RHObar - 1.)

C     BELOW IS THE SLOW BUT EASY TO UNDERSTAND EQUATION
c      DO i=0,3
c         DO j=0,6
c            MU1 = MU1 + H2(i,j)*(Tbit**i)*RHObit**j
c         ENDDO
c      ENDDO
c      MU1 = MU1 + H2(4,0)*Tbit**4 + H2(5,0)*Tbit**5     

C     THIS IS THE FASTER VERSION OF ABOVE
      MU1 = MU1 +
     &      H2(0,0) +
     &      H2(0,1)*RHObit +
     &      H2(0,2)*RHObit*RHObit +
     &      H2(0,3)*RHObit*RHObit*RHObit +
     &      H2(0,4)*RHObit*RHObit*RHObit*RHObit 
      
      MU1 = MU1 +  
     &      H2(1,0)*Tbit +
     &      H2(4,0)*Tbit*Tbit*Tbit*Tbit + 
     &      H2(5,0)*Tbit*Tbit*Tbit*Tbit*Tbit       

      MU1 = MU1 +
     &      H2(1,1)*Tbit*RHObit +
     &      H2(1,2)*Tbit*RHObit*RHObit +
     &      H2(1,3)*Tbit*RHObit*RHObit*RHObit

      MU1 = MU1 + 
     &      H2(2,1)*(Tbit*Tbit)*RHObit    +
     &      H2(2,2)*(Tbit*Tbit)*RHObit*RHObit +
     &      H2(2,3)*(Tbit*Tbit)*RHObit*RHObit*RHObit +
     &      H2(3,1)*(Tbit*Tbit*Tbit)*RHObit    +
     &      H2(3,3)*(Tbit*Tbit*Tbit)*RHObit*RHObit*RHObit +
     &      H2(3,4)*(Tbit*Tbit*Tbit)*RHObit*RHObit*RHObit*RHObit +
     &      H2(3,6)*(Tbit*Tbit*Tbit)*
     &               RHObit*RHObit*RHObit*RHObit*RHObit*RHObit+
     &      H2(1,5)*Tbit*RHObit*RHObit*RHObit*RHObit*RHObit


      MU1=EXP(DBLE(RHOBAR)*MU1)
      FUNVIS=REAL(MUbar*MU0*MU1)

      RETURN
      END
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      FUNCTION FUNCOND(T,RHO)
C     CALCULATES THE THERMAL CONDUCTIVITY OF WATER BASED ON THE 
C     1998 IPAWS RELEASE
      implicit none
      REAL Tbar,RHObar,T,RHO,FUNCOND
      REAL TCstar,RHOCstar,AC(0:3),BC(0:2),BC1,BC2,CC(6),DC(4),
     &     LAM0,LAM1,LAM2,DELT,Q,S
      INTEGER k
      COMMON /COND/ TCstar,RHOCstar,AC,BC,BC1,BC2,CC,DC

C     THIS ROUTINE MIGHT BE ABLE TO BE SPEED UP A LITTLE
      Tbar   = REAL(DBLE(T) / TCstar)
      RHObar = REAL(DBLE(RHO) / RHOCstar)

      LAM0 = DBLE(0.)
      DO k=0,3
         LAM0 = LAM0 + AC(k)*(DBLE(Tbar)**k)
      ENDDO
      LAM0 = DBLE(SQRT(Tbar))*LAM0
      
      LAM1 = BC(0) + 
     &       BC(1)*DBLE(RHObar) + 
     &       BC(2)*EXP(BC1*(DBLE(RHObar) + BC2)*(DBLE(RHObar) + BC2) )


      DELT = DBLE(ABS(Tbar - 1.)) + CC(4)
      Q = DBLE(2) + (CC(5)/DELT**DBLE(3./5.))
      IF(Tbar.GE.1.)THEN
         S = DBLE(1)/DELT
      ELSE
         S = CC(6)/(DELT**DBLE(3./5.))
      ENDIF

      LAM2 = (DC(1)*(DBLE(Tbar)**(-10)) +DC(2))*
     &       (DBLE(RHObar)**DBLE(9./5.))*
     &       EXP(CC(1)*(DBLE(1) - DBLE(RHObar)**DBLE(14./5.))) + 
     &       DC(3)*S*(DBLE(RHObar)**Q)*EXP((Q/(DBLE(1)+Q))*
     &       (DBLE(1) - DBLE(RHObar)**(1+Q))) + 
     &       DC(4)*EXP(CC(2)*DBLE(Tbar)**DBLE(3./2.) + 
     &       CC(3)*(DBLE(RHObar)**(-5)))
   

      FUNCOND = REAL(LAM0 + LAM1 + LAM2)
      RETURN
      END
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      FUNCTION FUNPRG (VISC, COND, CP)
C     Prandtl number of vapour (m).

      FUNPRG = VISC * CP / COND
      RETURN
      END
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      FUNCTION FUNMFP (T, P, VISC)
C     Mean free path of vapour molecule (m).
C     TAKEN FROM JBY
      REAL RGAS
      COMMON /MAIN/ RGAS

      FUNMFP = 1.5 * VISC * SQRT(REAL(RGAS) * T) / P
      RETURN
      END
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      FUNCTION FUNTSPTEMP(P)
C     THESE ARE ROUTINES FROM J.B.YOUNG 1995 STEAMPRP.F CODE.  
C     THESE HAVE BEEN INCLUDED AS THEY ARE CONSIDERABLE FASTER THAN 
C     THE IPAWS EQUATIONS. HOWEVER THEY ARE ONLY VALID FOR PRESSURE 
C     UPTO 10BAR 

C     Saturation temperature (K), at pressure P (N/m**2).
      DATA  A0,A1,A2,A3 /4.81879E-3,-1.79298E-4,2.71010E-7,-7.07319E-8/

      PLOG  = LOG (P)
      RTSP  = A0 + PLOG*(A1 + PLOG*(A2 + PLOG*A3))
      FUNTSPTEMP = 1.0 / RTSP

      RETURN
      END
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      FUNCTION SBACKWARD2A(P,S)
C     THIS WILL FIND THE TEMPERATURE BASED ON THE PRESSURE AND ENTROPY.
C     THE VALUES ARE TAKEN FROM THE IPAWS INDUSTRIAL FORMULATION AND ARE 
C     FOR REGION 2, SUBREGION 2A (I.E. LESS THAN 40BAR AND VAPOUR)

      IMPLICIT NONE
      INTEGER JI(46),i,i2,j2
      REAL II(46),NI(46)
      REAL P,S,SBACKWARD2A,CV,CP,s1,RHG,tau,IAPWSENTR,IAPWSCP,FUNTAU,
     &     FUNRHOV,FNFIND
      COMMON /REVERSE/ II,JI,NI

      SBACKWARD2A=0.
      
      DO i=1,46
         SBACKWARD2A = REAL(DBLE(SBACKWARD2A) + NI(i) * 
     &        (DBLE(P*1E-6)**II(i)) * 
     &        (DBLE(S*5E-4 - 2.)**JI(i)))
      ENDDO

c      DO i = 1 , 10
c         CALL LOOKUP2(i2,j2,P,SBACKWARD2A)
c         RHG=FNFIND(i2,j2,'r',P,SBACKWARD2A)
c
c         s1 = IAPWSENTR(RHG,SBACKWARD2A)
c         cp = IAPWSCP(RHG,SBACKWARD2A,CV)
c         SBACKWARD2A  = SBACKWARD2A * EXP ((s-s1)/cp)
c         write(67,*)i,tau,rhg,s1,s,cp,SBACKWARD2A
c      ENDDO
c      write(67,*)

      RETURN
      END
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      BLOCK DATA IPAWS
      implicit none

      REAL RGAS
      REAL Tc,Pc,RHOc,ALPHA0,PHI0
      REAL a(6),atop(6)
      REAL b(6),btop(6)
      REAL c(6),ctop(6)
      REAL d(5),dtop(5),dalpha,dphi
      REAL S1,S2,S3
      REAL Tstar,RHOstar,H1(0:3),H2(0:5,0:6),MUbar
      REAL TCstar,RHOCstar,AC(0:3),BC(0:2),BC1,BC2,CC(6),DC(4)
      INTEGER JI(46)
      REAL II(46),NI(46)

      COMMON /REVERSE/ II,JI,NI
      COMMON /MAIN/ RGAS
      COMMON /CRIT1/ Tc,Pc,RHOc,ALPHA0
      COMMON /PRESSURE/ a,atop
      COMMON /DENSITYL/ b,btop
      COMMON /DENSITYV/ c,ctop
      COMMON /AUX/ d,dtop,dalpha,dphi,PHI0
      COMMON /SURF/ S1,S2,S3
      COMMON /VIS/ Tstar,RHOstar,H1,H2,MUbar
      COMMON /COND/ TCstar,RHOCstar,AC,BC,BC1,BC2,CC,DC


      DATA II(1) /-1.50 /, JI(1) /-24 /, NI(1) /-0.39235983861984E+6/
      DATA II(2) /-1.50 /, JI(2) /-23 /, NI(2) / 0.51526573827270E+6/
      DATA II(3) /-1.50 /, JI(3) /-19 /, NI(3) / 0.40482443161048E+5/
      DATA II(4) /-1.50 /, JI(4) /-13 /, NI(4) /-0.32193790923902E+3/
      DATA II(5) /-1.50 /, JI(5) /-11 /, NI(5) / 0.96961424218694E+2/
      DATA II(6) /-1.50 /, JI(6) /-10 /, NI(6) /-0.22867846371773E+2/
      DATA II(7) /-1.25 /, JI(7) /-19 /, NI(7) /-0.44942914124357E+6/
      DATA II(8) /-1.25 /, JI(8) /-15 /, NI(8) /-0.50118336020166E+4/
      DATA II(9) /-1.25 /, JI(9) /- 6 /, NI(9) / 0.35684463560015 /
      DATA II(10)/-1.00 /, JI(10)/-26 /, NI(10)/ 0.44235335848190E+5/
      DATA II(11)/-1.00 /, JI(11)/-21 /, NI(11)/-0.13673388811708E+5/
      DATA II(12)/-1.00 /, JI(12)/-17 /, NI(12)/ 0.42163260207864E+6/
      DATA II(13)/-1.00 /, JI(13)/-16 /, NI(13)/ 0.22516925837475E+5/
      DATA II(14)/-1.00 /, JI(14)/- 9 /, NI(14)/ 0.47442144865646E+3/
      DATA II(15)/-1.00 /, JI(15)/- 8 /, NI(15)/-0.14931130797647E+3/
      DATA II(16)/-0.75 /, JI(16)/-15 /, NI(16)/-0.19781126320452E+6/
      DATA II(17)/-0.75 /, JI(17)/-14 /, NI(17)/-0.23554399470760E+5/
      DATA II(18)/-0.50 /, JI(18)/-26 /, NI(18)/-0.19070616302076E+5/
      DATA II(19)/-0.50 /, JI(19)/-13 /, NI(19)/ 0.55375669883164E+5/
      DATA II(20)/-0.50 /, JI(20)/- 9 /, NI(20)/ 0.38293691437363E+4/
      DATA II(21)/-0.50 /, JI(21)/- 7 /, NI(21)/-0.60391860580567E+3/
      DATA II(22)/-0.25 /, JI(22)/-27 /, NI(22)/ 0.19363102620331E+4/
      DATA II(23)/-0.25 /, JI(23)/-25 /, NI(23)/ 0.42660643698610E+4/
      DATA II(24)/-0.25 /, JI(24)/-11 /, NI(24)/-0.59780638872718E+4/
      DATA II(25)/-0.25 /, JI(25)/- 6 /, NI(25)/-0.70401463926862E+3/
      DATA II(26)/ 0.25 /, JI(26)/  1 /, NI(26)/ 0.33836784107553E+3/
      DATA II(27)/ 0.25 /, JI(27)/  4 /, NI(27)/ 0.20862786635187E+2/
      DATA II(28)/ 0.25 /, JI(28)/  8 /, NI(28)/ 0.33834172656196E-1/
      DATA II(29)/ 0.25 /, JI(29)/ 11 /, NI(29)/-0.43124428414893E-4/
      DATA II(30)/ 0.50 /, JI(30)/  0 /, NI(30)/ 0.16653791356412E+3/
      DATA II(31)/ 0.50 /, JI(31)/  1 /, NI(31)/-0.13986292055898E+3/
      DATA II(32)/ 0.50 /, JI(32)/  5 /, NI(32)/-0.78849547999872 /
      DATA II(33)/ 0.50 /, JI(33)/  6 /, NI(33)/ 0.72132411753872E-1/
      DATA II(34)/ 0.50 /, JI(34)/ 10 /, NI(34)/-0.59754839398283E-2/
      DATA II(35)/ 0.50 /, JI(35)/ 14 /, NI(35)/-0.12141358953904E-4/
      DATA II(36)/ 0.50 /, JI(36)/ 16 /, NI(36)/ 0.23227096733871E-6/
      DATA II(37)/ 0.75 /, JI(37)/  0 /, NI(37)/-0.10538463566194E+2/
      DATA II(38)/ 0.75 /, JI(38)/  4 /, NI(38)/ 0.20718925496502E+1/
      DATA II(39)/ 0.75 /, JI(39)/  9 /, NI(39)/-0.72193155260427E-1/
      DATA II(40)/ 0.75 /, JI(40)/ 17 /, NI(40)/ 0.20749887081120E-6/
      DATA II(41)/ 1.00 /, JI(41)/  7 /, NI(41)/-0.18340657911379E-1/
      DATA II(42)/ 1.00 /, JI(42)/ 18 /, NI(42)/ 0.29036272348696E-6/
      DATA II(43)/ 1.25 /, JI(43)/  3 /, NI(43)/ 0.21037527893619 /
      DATA II(44)/ 1.25 /, JI(44)/ 15 /, NI(44)/ 0.25681239729999E-3/
      DATA II(45)/ 1.50 /, JI(45)/  5 /, NI(45)/-0.12799002933781E-1/
      DATA II(46)/ 1.50 /, JI(46)/ 18 /, NI(46)/-0.82198102652018E-5/

      DATA TCstar/647.26 /, RHOCstar/317.7 /
      DATA AC(0)/0.0102811 / , AC(1)/ 0.0299621 /
      DATA AC(2)/0.0156146 / , AC(3)/-0.00422464 /
      DATA BC(0)/-0.397070 / , BC(1)/0.400302 /, BC(2)/1.060000 /
      DATA BC1/-0.171587 /   , BC2/2.392190 /
      DATA DC(1)/0.0701309 / , DC(2)/0.0118520 /
      DATA DC(3)/0.00169937 /, DC(4)/-1.0200 /
      DATA CC(1)/0.642857 /,   CC(2)/-4.11717 /,  CC(3)/-6.17937 /
      DATA CC(4)/0.00308976 /, CC(5)/0.0822994 /, CC(6)/10.0932 /

      DATA H1(0)/1.000000 / , H1(1)/ 0.978197 /
      DATA H1(2)/0.579829 / , H1(3)/-0.202354 /
      DATA H2(0,0)/0.5132047 /,H2(1,0)/ 0.3205656 /,H2(2,0)/0.00000 /
      DATA H2(3,0)/0.00000 /,H2(4,0)/-0.7782567 /,H2(5,0)/0.1885447 /
      DATA H2(0,1)/0.2151778 /,H2(1,1)/0.7317883 /,H2(2,1)/1.241044 /
      DATA H2(3,1)/1.476783 /,H2(4,1)/0.0000000 /,H2(5,1)/0.000000 /
      DATA H2(0,2)/-0.2818107 /,H2(1,2)/-1.070786 /,
     &                                              H2(2,2)/-1.263184 /
      DATA H2(3,2)/ 0.0000000 /,H2(4,2)/ 0.000000 /,H2(5,2)/ 0.0000 /
      DATA H2(0,3)/0.1778064 /,H2(1,3)/0.460504 /,H2(2,3)/0.2340379 /
      DATA H2(3,3)/-0.4924179 /,H2(4,3)/0.0000000 /,H2(5,3)/0.00000 /
      DATA H2(0,4)/-0.04176610 /,H2(1,4)/0.000000 /,H2(2,4)/0.00000 /
      DATA H2(3,4)/ 0.1600435 /,H2(4,4)/0.000000 /,H2(5,4)/0.000000 /
      DATA H2(0,5)/0.0000000 /,H2(1,5)/-0.01578386 /,H2(2,5)/0.0000 /
      DATA H2(3,5)/0.0000000 /,H2(4,5)/ 0.00000000 /,H2(5,5)/0.0000 /
      DATA H2(0,6)/ 0.000000 /,H2(1,6)/ 0.00000000 /,H2(2,6)/0.0000 /
      DATA H2(3,6)/-0.003629481 /,H2(4,6)/0.0000000 /,H2(5,6)/0.000 /

      DATA RGAS /0.461526D3/

      DATA Tc/647.096 /, Pc/22.064D6/, RHOc/322. /, ALPHA0/1000. /
c     DATA PHI0 = 1000 /647.096 
      DATA PHI0 /1.54535757167 /
      DATA a(1)/-7.85951783 /, a(2)/ 1.84408259 /, a(3)/-11.7866497 /
      DATA a(4)/ 22.6807411 /, a(5)/-15.9618719 /, a(6)/1.80122502 /
      DATA atop(1)/1.0 /, atop(2)/1.5 /, atop(3)/3.0 /
      DATA atop(4)/3.5 /, atop(5)/4.0 /, atop(6)/7.5 /

      DATA b(1)/ 1.99274064 /, b(2)/ 1.09965342 /,b(3)/-0.510839303 /
      DATA b(4)/-1.75493479 /, b(5)/-45.5170352 /,b(6)/-6.74694450D5/
c     DATA btop(1)/(1 /3 )/, btop(2)/(2 /3 )/,btop(3)/(5 /3 )/
c     DATA btop(4)/(16 /3 )/, btop(5)/(43 /3 )/,btop(6)/(110 /3 )/
      DATA btop(1)/0.333333333 /, btop(2)/0.666666666 /
      DATA btop(3)/1.666666666 /
      DATA btop(4)/5.333333333 /, btop(5)/14.333333333 /
      DATA btop(6)/36.666666666 /
      DATA c(1)/-2.03150240 /, c(2)/-2.68302940 /, c(3)/-5.38626492 /
      DATA c(4)/-17.2991605 /, c(5)/-44.7586581 /, c(6)/-63.9201063 /
c     DATA ctop(1)/(2 /6 )/, ctop(2)/(4 /6 )/, ctop(3)/(8 /6 )/
      DATA ctop(1)/0.333333333 /, ctop(2)/0.666666666 / 
      DATA ctop(3)/1.3333333333 /
c     DATA ctop(4)/(18 /6 )/, ctop(5)/(37 /6 )/, ctop(6)/(71 /6 )/
      DATA ctop(4)/3 /, ctop(5)/6.166666666 /
      DATA ctop(6)/11.8333333333 /
      DATA d(1)/-5.65134998E-8/, d(2)/2690.66631 /, d(3)/127.287297 /
      DATA d(4)/-135.003439 /,    d(5)/0.981825814 /
      DATA dtop(1)/-19. /, dtop(2)/ 1. /, dtop(3)/4.5 /
      DATA dtop(4)/  5. /, dtop(5)/54.5 /
      DATA dalpha/-1135.905627715 /
      DATA dphi/2319.5246/

      DATA S1/235.8E-3/, S2/1.256 /, S3/-0.625 /

      DATA Tstar/647.226 /, RHOstar/317.763 /, MUbar/55.071E-6/


      END
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      SUBROUTINE INITIALDPDT()

      implicit none 
      REAL Tc,Pc,RHOc,ALPHA0
      REAL a(6),atop(6)
      REAL alpha(6),beta(6),gamma(6),delta(6),epsilon(6),eta(6)
      INTEGER i
      COMMON /CRIT1/ Tc,Pc,RHOc,ALPHA0 
      COMMON /PRESSURE/ a,atop
      COMMON /DPDT/alpha,beta,gamma,delta,epsilon,eta

      DO i=1,6
         alpha(i)= a(i)*atop(i)*tc**(DBLE(1)-atop(i))
         beta(i)= -tc/atop(i)
         gamma(i)= -(DBLE(1) + (DBLE(1)/atop(i)))
         delta(i)= (DBLE(1)/atop(i)) - DBLE(1)
         epsilon(i)= -DBLE(1)/atop(i)
         eta(i)= atop(i)-DBLE(1)
      ENDDO


      RETURN
      END
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
















CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      FUNCTION DFUNPV(T,tau)
C     THIS CALCULATES THE SATURATION VAPOUR PRESSURE IN (Pa) BASED ON 
C     THE IPAWS 1992 SUPPLEMENTARY RELEASE ON SATURATION PROPERTIES

      implicit none
      REAL Tc,Pc,RHOc,ALPHA0,a(6),atop(6),T,DFUNPV,tau
      INTEGER i
      COMMON /CRIT1/ Tc,Pc,RHOc,ALPHA0
      COMMON /PRESSURE/ a,atop

      DFUNPV=0 
      DO i=1,6
         DFUNPV = DFUNPV + a(i)*tau**atop(i)
      ENDDO

      DFUNPV = Pc * EXP(Tc*DFUNPV/T)

      RETURN
      END
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      FUNCTION DFUNRHOL(tau)
C     THIS CALCULATES THE SATURATED LIQUID DENSITY IN (Kg/m**3) BASED ON 
C     THE IPAWS 1992 SUPPLEMENTARY RELEASE ON SATURATION PROPERTIES
      implicit none
      REAL Tc,Pc,RHOc,ALPHA0,b(6),btop(6),tau,DFUNRHOL
      INTEGER i
      COMMON /CRIT1/ Tc,Pc,RHOc,ALPHA0
      COMMON /DENSITYL/ b,btop

      DFUNRHOL=1 
      DO i=1,6
         DFUNRHOL = DFUNRHOL + b(i)*tau**btop(i)
      ENDDO

      DFUNRHOL=RHOc*DFUNRHOL

      RETURN
      END
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      FUNCTION DFUNRHOV(tau)
C     THIS CALCULATES THE SATURATED VAPOUR DENSITY IN (Kg/m**3) BASED ON 
C     THE IPAWS 1992 SUPPLEMENTARY RELEASE ON SATURATION PROPERTIES
      implicit none
      REAL Tc,Pc,RHOc,ALPHA0,c(6),ctop(6),tau,DFUNRHOV
      INTEGER i
      COMMON /CRIT1/ Tc,Pc,RHOc,ALPHA0
      COMMON /DENSITYV/ c,ctop

      DFUNRHOV=0 
      DO i=1,6
         DFUNRHOV = DFUNRHOV + c(i)*tau**ctop(i)
      ENDDO

      DFUNRHOV=RHOc * EXP(DFUNRHOV)

      RETURN
      END
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      FUNCTION DFUNAUX(T)
C     THIS IS AN AUXILIARY EQUATION SO THAT THE ENTHAPLIES CAN BE 
C     CALCULATED. IT IS BASED ON THE IPAWS 1992 SUPPLEMENTARY RELEASE 
C     ON SATURATION PROPERTIES
      implicit none
      REAL Tc,Pc,RHOc,ALPHA0,d(5),dtop(5),dalpha,T,DFUNAUX,theta,
     &     dphi,phi0
      INTEGER i
      COMMON /CRIT1/ Tc,Pc,RHOc,ALPHA0
      COMMON /AUX/ d,dtop,dalpha,dphi,phi0

      theta = T/Tc

      DFUNAUX=dalpha
      DO i=1,5
         DFUNAUX = DFUNAUX + d(i)*theta**dtop(i)
      ENDDO      

      DFUNAUX = ALPHA0 * DFUNAUX

      RETURN
      END
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      FUNCTION DFUNAUXPHI(T)
C     THIS IS AN AUXILIARY EQUATION SO THAT THE ENTROPIES CAN BE 
C     CALCULATED. IT IS BASED ON THE IPAWS 1992 SUPPLEMENTARY RELEASE 
C     ON SATURATION PROPERTIES   

      implicit none
      real Tc,Pc,RHOc,ALPHA0,d(5),dtop(5),dalpha,dphi,phi0,theta,t,
     &     DFUNAUXPHI
      COMMON /CRIT1/ Tc,Pc,RHOc,ALPHA0
      COMMON /AUX/ d,dtop,dalpha,dphi,phi0

      theta = T/Tc

      DFUNAUXPHI=dphi
      DFUNAUXPHI = DFUNAUXPHI + 19 /20 *d(1)*theta**(-20 )
      DFUNAUXPHI = DFUNAUXPHI + d(2)*LOG(theta)
      DFUNAUXPHI = DFUNAUXPHI + 9 /7 *d(3)*theta**3.5 
      DFUNAUXPHI = DFUNAUXPHI + 5 /4 *d(4)*theta**4 
      DFUNAUXPHI = DFUNAUXPHI + 109 /107 *d(5)*theta**53.5 

      DFUNAUXPHI = PHI0*DFUNAUXPHI

      RETURN
      END
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      FUNCTION DFUNTAU(T)
C     THIS IS AN AUXILIARY EQUATION WHICH IS NEEDED BY MANY OF THE 
C     EQUATIONS AND IS FROM THE IPAWS 1992 SUPPLEMENTARY RELEASE 
C     ON SATURATION PROPERTIES
      implicit none

      REAL Tc,Pc,RHOc,ALPHA0,T,DFUNTAU
      COMMON /CRIT1/ Tc,Pc,RHOc,ALPHA0

      DFUNTAU = 1  - T/Tc

      RETURN
      END
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      FUNCTION DFUNDPDTSAT(T,Psat)
C     THIS CALCULATES (DP/DT). THIS IS NEEDED FOR THE CALCULATION OF 
C     THE ENTHALPIES. THIS IS BASED ON THE WORK FROM THE  IPAWS 1992 
C     SUPPLEMENTARY RELEASE ON SATURATION PROPERTIES
C
C     BEFORE THIS FUNCTION IS FIRST RUN INITIALDPDT MUST BE RUN TO FIND
C     VALUE FOR THE COMMON BLOCK /DPDT/
      implicit none
      REAL Tc,Pc,RHOc,ALPHA0,a(6),atop(6)
      REAL alpha(6),beta(6),gamma(6),delta(6),epsilon(6),eta(6)
      REAL T,Psat,DFUNDPDTSAT
      INTEGER i
      COMMON /CRIT1/ Tc,Pc,RHOc,ALPHA0
      COMMON /PRESSURE/ a,atop
      COMMON /DPDT/alpha,beta,gamma,delta,epsilon,eta

      DFUNDPDTSAT=-a(1)*tc/(t*t)
      DO i=2,6
         DFUNDPDTSAT=DFUNDPDTSAT + 
     &        (alpha(i)*(beta(i)*t**gamma(i) 
     &        +  delta(i)*t**epsilon(i)) ) *
     &        ABS(tc*t**epsilon(i) - 
     &        t**(-delta(i)))**eta(i)
      ENDDO

      DFUNDPDTSAT = DFUNDPDTSAT*Psat     

      RETURN
      END
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      FUNCTION DFUNHFG(RHOL,RHOV,DPDT,T)
C     THIS CALCULATES THE SPECFIC ENTHALPY OF EVAPOURATION (J/Kg)
C     NOTE THIS ROUTINE OS FASTER THAN CALCULTING HL AND HV AND 
C      SUPTRACTING THE TWO.

      implicit none
      REAL RHOL,RHOV,DPDT,T,DFUNHFG

      DFUNHFG = (1 /RHOV - 1 /RHOL)*T*DPDT

      RETURN
      END
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      FUNCTION DFUNHL(RHOL,DPDT,ALPHA,T)
C     THIS CALCULATES THE ***SATURATED*** LIQUID ENTHAPLY (J/Kg) BASED ON 
C     THE IPAWS 1992 SUPPLEMENTARY RELEASE ON SATURATION PROPERTIES
      implicit none
      REAL RHOL,DPDT,ALPHA,T,DFUNHL

      DFUNHL=ALPHA + T/RHOL * DPDT

      RETURN
      END
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      FUNCTION DFUNHV(RHOV,DPDT,ALPHA,T)
C     THIS CALCULATES THE ***SATURATED*** VAPOUR ENTHAPLY (J/Kg) BASED ON 
C     THE IPAWS 1992 SUPPLEMENTARY RELEASE ON SATURATION PROPERTIES
      implicit none
      REAL RHOV,DPDT,ALPHA,T,DFUNHV

      DFUNHV=ALPHA + T/RHOV * DPDT

      RETURN
      END
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      FUNCTION DFUNSL(RHOL,DPDT,PHI,T)
C     THIS CALCULATES THE ***SATURATED*** LIQUID ENTROPY (J/Kg K) BASED ON 
C     THE IPAWS 1992 SUPPLEMENTARY RELEASE ON SATURATION PROPERTIES
      implicit none
      REAL RHOL,DPDT,PHI,T,DFUNSL

      DFUNSL=PHI + 1 /RHOL * DPDT

      RETURN
      END
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      FUNCTION DFUNSV(RHOV,DPDT,PHI,T)
C     THIS CALCULATES THE ***SATURATED*** VAPOUR ENTROPY (J/Kg K) BASED ON 
C     THE IPAWS 1992 SUPPLEMENTARY RELEASE ON SATURATION PROPERTIES
      implicit none
      REAL RHOV,DPDT,PHI,T,DFUNSV

      DFUNSV=PHI + 1 /RHOV * DPDT

      RETURN
      END
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      FUNCTION DFUNSURF(tau)
C     THIS CALCULATES THE SURFACE TENSION OF WATER (N/m) BASED ON THE 
C     IPAWS 1994 RELEASE
      implicit none
      REAL S1,S2,S3,tau,DFUNSURF
      COMMON /SURF/ S1,S2,S3     

      DFUNSURF = S1*(1+S3*tau)*tau**S2

      RETURN
      END
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      FUNCTION DFUNTSAT(P,lim,lim2)
C     THIS CALULATES THE THE SATURATION TEMPERATURE FOR A 
C     GIVEN PRESSURE, HOWEVER SEE NOTE BELOW AND USE FUNTSATTEMP
      
      implicit none

      REAL DFUNTSAT,T,P,PSAT,DPDT,DFUNTAU,DFUNPV,DFUNDPDTSAT,TAU,
     &     DFUNTSPTEMP,terror,lim2
      INTEGER i,lim
      REAL Tc,Pc,RHOc,ALPHA0

      COMMON /CRIT1/ Tc,Pc,RHOc,ALPHA0

C     THIS IS A VERY VERY SLOW FUNCTION
C     DO NOT USE IN MAIN PROGRAM, FIND FASTER APPROXIMATE CODE
C     LIM SHOULD NOT NEED TO BE LARGER THAN 10

      DFUNTSAT = DFUNTSPTEMP(P)
      IF(DFUNTSAT.GT.TC)DFUNTSAT=TC
      DO i=1,lim
         tau   = DFUNTAU(DFUNTSAT)
         Psat  = DFUNPV(DFUNTSAT,tau)
         DPDT  = DFUNDPDTSAT(DFUNTSAT,Psat)
         terror = (P-PSAT)/DPDT
         DFUNTSAT = DFUNTSAT + terror
         IF(DFUNTSAT.GT.TC)DFUNTSAT=TC
c         write(69,*)funtsat,p,terror

         IF(ABS(terror*100/DFUNTSAT).LT.LIM2)GOTO 10
      ENDDO
      write(*,*)'** ERROR FINDING TSAT*',P/1D5,ABS(terror*100/DFUNTSAT)

 10   CONTINUE

      RETURN
      END
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      FUNCTION DFUNVIS(T,RHO)
C     THIS CALCULATES THE VISCOSITY OF WATER BASED ON THE 1997 
C     IPAWS RELEASE.
      implicit none
      REAL T,RHO,Tbar,RHObar,DFUNVIS
      REAL  MU1,MU0,MUbar,H1(0:3),H2(0:5,0:6),Tstar,RHOstar,RHObit,
     &     Tbit
      INTEGER i
      COMMON /VIS/ Tstar,RHOstar,H1,H2,MUbar

      Tbar   = T / Tstar
      RHObar = RHO / RHOstar

      MU0=H1(0)
      MU1=DBLE(0.)

      DO i=1,3
         MU0=MU0 + H1(i)/(Tbar**i)
      ENDDO
      MU0=SQRT(Tbar)/MU0

      Tbit   = 1 /Tbar - 1 
      RHObit = RHObar - 1 

C     BELOW IS THE SLOW BUT EASY TO UNDERSTAND EQUATION
c      DO i=0,3
c         DO j=0,6
c            MU1 = MU1 + H2(i,j)*(Tbit**i)*RHObit**j
c         ENDDO
c      ENDDO
c      MU1 = MU1 + H2(4,0)*Tbit**4 + H2(5,0)*Tbit**5     

C     THIS IS THE FASTER VERSION OF ABOVE
      MU1 = MU1 +
     &      H2(0,0) +
     &      H2(0,1)*RHObit +
     &      H2(0,2)*RHObit*RHObit +
     &      H2(0,3)*RHObit*RHObit*RHObit +
     &      H2(0,4)*RHObit*RHObit*RHObit*RHObit 
      
      MU1 = MU1 +  
     &      H2(1,0)*Tbit +
     &      H2(4,0)*Tbit*Tbit*Tbit*Tbit + 
     &      H2(5,0)*Tbit*Tbit*Tbit*Tbit*Tbit       

      MU1 = MU1 +
     &      H2(1,1)*Tbit*RHObit +
     &      H2(1,2)*Tbit*RHObit*RHObit +
     &      H2(1,3)*Tbit*RHObit*RHObit*RHObit

      MU1 = MU1 + 
     &      H2(2,1)*(Tbit*Tbit)*RHObit    +
     &      H2(2,2)*(Tbit*Tbit)*RHObit*RHObit +
     &      H2(2,3)*(Tbit*Tbit)*RHObit*RHObit*RHObit +
     &      H2(3,1)*(Tbit*Tbit*Tbit)*RHObit    +
     &      H2(3,3)*(Tbit*Tbit*Tbit)*RHObit*RHObit*RHObit +
     &      H2(3,4)*(Tbit*Tbit*Tbit)*RHObit*RHObit*RHObit*RHObit +
     &      H2(3,6)*(Tbit*Tbit*Tbit)*
     &               RHObit*RHObit*RHObit*RHObit*RHObit*RHObit+
     &      H2(1,5)*Tbit*RHObit*RHObit*RHObit*RHObit*RHObit


      MU1=EXP(RHOBAR*MU1)
      DFUNVIS=MUbar*MU0*MU1

      RETURN
      END
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      FUNCTION DFUNCOND(T,RHO)
C     CALCULATES THE THERMAL CONDUCTIVITY OF WATER BASED ON THE 
C     1998 IPAWS RELEASE
      implicit none
      REAL Tbar,RHObar,T,RHO,DFUNCOND
      REAL TCstar,RHOCstar,AC(0:3),BC(0:2),BC1,BC2,CC(6),DC(4),
     &     LAM0,LAM1,LAM2,DELT,Q,S
      INTEGER k
      COMMON /COND/ TCstar,RHOCstar,AC,BC,BC1,BC2,CC,DC

C     THIS ROUTINE MIGHT BE ABLE TO BE SPEED UP A LITTLE
      Tbar   = T / TCstar
      RHObar = RHO / RHOCstar

      LAM0 = 0 
      DO k=0,3
         LAM0 = LAM0 + AC(k)*(Tbar**k)
      ENDDO
      LAM0 = SQRT(Tbar)*LAM0
      
      LAM1 = BC(0) + 
     &       BC(1)*RHObar + 
     &       BC(2)*EXP(BC1*(RHObar + BC2)*(RHObar + BC2) )


      DELT = ABS(Tbar - 1 ) + CC(4)
      Q = 2  + (CC(5)/DELT**(3 /5 ))
      IF(Tbar.GE.1.)THEN
         S = 1 /DELT
      ELSE
         S = CC(6)/(DELT**(3 /5 ))
      ENDIF

      LAM2 = (DC(1)*(Tbar**(-10)) +DC(2))*
     &       (RHObar**DBLE(9 /5 ))*
     &       EXP(CC(1)*(1  - RHObar**(14 /5 ))) + 
     &       DC(3)*S*(RHObar**Q)*EXP((Q/(1 +Q))*
     &       (1  - RHObar**(1 +Q))) + 
     &       DC(4)*EXP(CC(2)*Tbar**DBLE(3./2.) + 
     &       CC(3)*(RHObar**(-5 )))
   

      DFUNCOND = LAM0 + LAM1 + LAM2
      RETURN
      END
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      FUNCTION DFUNPRG (VISC, COND, CP)
C     Prandtl number of vapour (m).
      implicit none
      real DFUNPRG,VISC,CP,COND

      DFUNPRG = VISC * CP / COND
      RETURN
      END
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      FUNCTION DFUNMFP (T, P, VISC)
C     Mean free path of vapour molecule (m).
C     TAKEN FROM JBY
      implicit none
      REAL RGAS,DFUNMFP,VISC,T,P
      COMMON /MAIN/ RGAS

      DFUNMFP = 1.5  * VISC * SQRT(RGAS * T) / P
      RETURN
      END
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      FUNCTION DFUNTSPTEMP(P)
C     THESE ARE ROUTINES FROM J.B.YOUNG 1995 STEAMPRP.F CODE.  
C     THESE HAVE BEEN INCLUDED AS THEY ARE CONSIDERABLE FASTER THAN 
C     THE IPAWS EQUATIONS. HOWEVER THEY ARE ONLY VALID FOR PRESSURE 
C     UPTO 10BAR 

C     Saturation temperature (K), at pressure P (N/m**2).
      implicit none
      real A0,A1,A2,A3,DFUNTSPTEMP,RTSP,PLOG,P

      DATA  A0,A1,A2,A3 /4.81879E-3,-1.79298E-4,2.71010E-7,-7.07319E-8/

      PLOG  = LOG(P)
      RTSP  = A0 + PLOG*(A1 + PLOG*(A2 + PLOG*A3))
      DFUNTSPTEMP = 1  / RTSP

      RETURN
      END
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      FUNCTION DSBACKWARD2A(P,S)
C     THIS WILL FIND THE TEMPERATURE BASED ON THE PRESSURE AND ENTROPY.
C     THE VALUES ARE TAKEN FROM THE IPAWS INDUSTRIAL FORMULATION AND ARE 
C     FOR REGION 2, SUBREGION 2A (I.E. LESS THAN 40BAR AND VAPOUR)

      IMPLICIT NONE
      INTEGER JI(46),i,i2,j2
      REAL II(46),NI(46)
      REAL  P,S,DSBACKWARD2A,CV,CP,s1,RHG,tau,IAPWSENTR,IAPWSCP,
     &     DFUNTAU,DFUNRHOV,DFNFIND
      COMMON /REVERSE/ II,JI,NI

      DSBACKWARD2A=0 
      
      DO i=1,46
         DSBACKWARD2A = DSBACKWARD2A + NI(i) * 
     &        ((P*1E-6)**II(i)) * 
     &        ((S*5E-4 - 2)**JI(i))
      ENDDO

c      DO i = 1 , 10
c         CALL LOOKUP2(i2,j2,P,SBACKWARD2A)
c         RHG=FNFIND(i2,j2,'r',P,SBACKWARD2A)
c
c         s1 = IAPWSENTR(RHG,SBACKWARD2A)
c         cp = IAPWSCP(RHG,SBACKWARD2A,CV)
c         SBACKWARD2A  = SBACKWARD2A * EXP ((s-s1)/cp)
c         write(67,*)i,tau,rhg,s1,s,cp,SBACKWARD2A
c      ENDDO
c      write(67,*)

      RETURN
      END



