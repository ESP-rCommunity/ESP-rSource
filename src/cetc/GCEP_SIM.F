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

C *********************************************************************
C GCEP_SIM
C
C Created by: Patrice Pinel
C Initial Creation Date: October 8, 2002
C
C  
C This subroutine calls the initialization routine at the begining of the simulation
C reorders the history term
C evaluates the agregated terms in the MLA
C calls ground temperature calculation routine when apropriate
C cells routine that calculates non agragated MLA terms and fluid temperatures
C 
C
C
C This is the controlling routine for the simulation of the HEAT PUMPS 
C This subroutine is called from MZNUMA once every hour 
C (within the 'do 20' loop.)
C
C
C INPUTS:
C	GCEPM_INPUT module
C 
C OUTPUTS:
C     Gload : load to the ground
C     agregated terms
C
C *********************************************************************
      SUBROUTINE GCEP_SIM

C Use module for GCEPM data
C      USE GCEP_INPUT
	
      IMPLICIT NONE

#include "building.h"
#include "gcep_parameters.h"
#include "gcep_common.h"
      common/gcepinfo/igcep
      common/gcepfile/gcepfile

      INTEGER igcep  
      character gcepfile*72

C Indicator to know if it is time to reevaluate the ground temperatures
      LOGICAL TgON

C Call subroutine that initialize simulation coefficient if this is the first time step
C and does noting if it is not
      CALL  GCEPInit

C incrementation of the time (time in hours)
      GCEP_Time=GCEP_Time+1.0


C Sets the hourly load as the sum of the heat and cooling loads calculated in the GCEP_load routine
     
      GCEP_hourly_load = GCEP_hourly_HP_hload - GCEP_hourly_HP_cload

c Once the ground loop is completed, set heating and cooling load to zero, so that 
c the next hour's sum will begin at zero.
      GCEP_hourly_HP_hload = 0.0
      GCEP_hourly_HP_cload = 0.0
        
C reorganise the load history vector to acount for the new time
      CALL InitHist(GCEP_nHis,GCEP_LoadHis)

C verifies if it is time to reevaluate the temperatures in the ground numerically
C i.e. if the time is a multiple of Inter_Ts (the number of hours between calculations)
C if it is, then call GCEPtground to calculate those temperatures
      CALL Eval_TgON(GCEP_Time,GCEP_Inter_Tg,GCEP_Time_Tg,TgON)
      IF (TgON) THEN
        CALL GCEPtground
      ENDIF

C evaluate the average loads on the agregation scheme periods
      CALL CalcPAve(INT(GCEP_Time+0.0001),GCEP_NHis,GCEP_LoadHis,
     &     GCEP_Xy,GCEP_Xm,GCEP_Xw,GCEP_Xd,GCEP_Xh,
     &     GCEP_Nyear,GCEP_Nmonth,GCEP_Nweek,GCEP_Nday,GCEP_Nhour,
     &     GCEP_Py,GCEP_Pm,GCEP_Pw,GCEP_Pd,GCEP_P) 

C evaluate the agregated terms of the MLA
      CALL CalcTerms(GCEP_Time,GCEP_Nyear,GCEP_Nmonth,GCEP_Nweek,
     &     GCEP_Nday,GCEP_Py,GCEP_Pm,GCEP_Pw,GCEP_Pd,
     &     GCEP_T_y,GCEP_T_m,GCEP_T_w,GCEP_T_d,GCEP_alpha_s,GCEP_D_bore)	
   
      GCEP_Gload=GCEP_hourly_load
   
C evaluate the temperature of the fluid (FUNCTION OF THE GROUND LOAD) coming out of the ground loop (into the HP)
      CALL GCEP_MLAiterate


C For testing: writes values calculated by the GCEP algorithm in fort.89 file
c     write(89,*) GCEP_Time,GCEP_hourly_load,GCEP_EWT,GCEP_OWT

      END

**************************************************************************
*1
C *********************************************************************
C CalcTerms
C
C Created by: Patrice Pinel
C Initial Creation Date: October 8, 2002
C
C This subroutine evaluates the agregated terms of the MLA
C
C This routine is called from GCEP_SIM
C
C This subroutine is called once at the begining of every time step
C By default this should be for every hour simulated
C
C INPUTS:
C    Time    Hours passed since the begining of the simulation
C Number of hours in the term i (0 or Xi)
C	Ny		rest (yearly)
C	Nm		month
C	Nw		week
C	Nd		day
C Average ground load on each period
C	Py          rest (yearly)
C	Pm          month
C	Pw          week
C	Pd          day
C	Alpha_s     thermal difusivity of the ground
C	D_bor       Borehole diameter
C 
C OUTPUTS:
C MLA (Multple load agregation) scheme terms
C	T_y     rest (yearly)
C	T_m     month
C	T_w     week
C	T_d     day
C
C *********************************************************************
      SUBROUTINE CalcTerms(Time,Ny,Nm,Nw,Nd,Py,Pm,Pw,Pd,
     &                     T_y,T_m,T_w,T_d,alpha_s,D_bore)

      IMPLICIT NONE
C     INPUTS
      INTEGER Ny,Nm,Nw,Nd
      REAL Time,Py,Pm,Pw,Pd,T_y,T_m,T_w,T_d,alpha_s,D_bore
C Function calculating the G function of the cylindrical heat source method
      REAL FonG
 
      T_y=Py*(FonG(alpha_s,D_bore,Time)-
     &        FonG(alpha_s,D_bore,Time-Ny))
      T_m=Pm*(FonG(alpha_s,D_bore,Time-Ny)-
     &        FonG(alpha_s,D_bore,Time-Ny-Nm))
      T_w=Pw*(FonG(alpha_s,D_bore,Time-Ny-Nm)-
     &        FonG(alpha_s,D_bore,Time-Ny-Nm-Nw))
      T_d=Pd*(FonG(alpha_s,D_bore,Time-Ny-Nm-Nw)-
     &        FonG(alpha_s,D_bore,Time-Ny-Nm-Nw-Nd))
      END

***********************************************************************
*2
C *********************************************************************
C TgON
C
C Created by: Patrice Pinel
C Initial Creation Date: October 10, 2002
C
C This subroutine determines if it is time to calculate the ground temperatures
C Returns true if it is and false if it is not
C
C This routine is called from GCEP_SIM
C
C This subroutine is called once at the begining of every time step
C By default this should be for every hour simulated
C
C INPUTS:
C    Time        number of hours since the begining of the simulation
C    Inter_Tg    number of hours between calculations of ground temperatures
C    Time_Tg     Time since the last calculation of the ground temperatures
C 
C OUTPUTS:
C	TgON       Indicates if it is time to recalculate ground temperatures
C
C *********************************************************************
      SUBROUTINE Eval_TgON(Time,Inter_Tg,Time_Tg,TgON)
      
	IMPLICIT NONE 
C     INPUTS
      REAL Time,Inter_Tg,Time_Tg
      Logical TgON

      IF ((Time-Time_Tg).GE.Inter_Tg) THEN
        Time_Tg=Time
        TgON=.TRUE.
      ELSE
        TgON=.FALSE.
      ENDIF

      END

***************************************************************
*3
C *********************************************************************
C CalcPAve
C
C Created by: Patrice Pinel
C Initial Creation Date: October 8, 2002
C
C This subroutine evaluates the average ground loads for the periods covered by the MLA
C
C This routine is called from GCEP_SIM
C
C This subroutine is called once at the begining of every time step
C By default this should be for every hour simulated
C
C INPUTS:
C	nHis    number of terms in the LoadHis vector
C	LoadHis	vector containing the history of the ground loads
C	Xm	    Number of hours in the aggregation period (m=month)
C	Xw	    Number of hours in the aggregation period (w=week)
C	Xd	    Number of hours in the aggregation period (d=day)
C	Xh	    Number of non agregated hours kept
C 
C OUTPUTS:
C	Xy	    Number of hours in the rest (y is for yearly)
C Number of hours in the term i (0 or Xi)
C	Ny	    rest (yearly)
C	Nm	    month
C	Nw	    week
C	Nd	    day
C	Nh	    hours
C Average ground load on each period
C	Py	    rest (yearly)
C	Pm      month
C	Pw      week
C	Pd      day
C	P(Xh)   Ground loads for the last non agregated Xh hours
C
C *********************************************************************
      SUBROUTINE CalcPAve(Time,nHis,LoadHis,Xy,Xm,Xw,Xd,Xh,
     &                    Ny,Nm,Nw,Nd,Nh,Py,Pm,Pw,Pd,P)

      IMPLICIT NONE 
C     INPUTS
      INTEGER Time,nHis,Xy,Xm,Xw,Xd,Xh,Ny,Nm,Nw,Nd,Nh,i
      REAL LoadHis(nHis),Py,Pm,Pw,Pd,P(Xh)
C Function which calculates the average load between two times
C Located in file GCEPtground
      REAL AveLoad
	 
C initialize Nm,Ns and Nj
      Nm=0
      Nw=0
      Nd=0
C evaluates Nh and P(Xh)
      IF(Time.LT.Xh) THEN
        Nh=Time
      ELSE
        Nh=Xh
      ENDIF
      DO 1210 i=1,Xh
        P(i)=LoadHis(nHis+i-Xh)
1210  CONTINUE
C evaluates Nd and Pd
      IF(Time.GE.(Xd+Xh)) THEN
        IF(Time.EQ.(Xd+Xh)) THEN
          Pd=AveLoad(nHis,LoadHis,2+Xm+Xw,1+Xm+Xw+Xd)
        ELSE
          Pd=Pd+(LoadHis(1+Xm+Xw+Xd)-LoadHis(1+Xm+Xw))/Xd
        ENDIF
        Nd=Xd
      ENDIF
C evaluates Nw and Pw
      IF(Time.GE.(Xw+Xd+Xh)) THEN
        IF(Time.EQ.(Xw+Xd+Xh)) THEN
          Pw=AveLoad(nHis,LoadHis,2+Xm,1+Xm+Xw)
        ELSE
          Pw=Pw+(LoadHis(1+Xm+Xw)-LoadHis(1+Xm))/Xw
        ENDIF
          Nw=Xw
      ENDIF	
C evaluates Nm and Pm
      IF(Time.GE.(Xm+Xw+Xd+Xh)) THEN
        IF(Time.EQ.(Xm+Xw+Xd+Xh)) THEN
          Pm=AveLoad(nHis,LoadHis,2,1+Xm)
        ELSE
          Pm=Pm+(LoadHis(1+Xm)-LoadHis(1))/Xm 
        ENDIF
          Nm=Xm
      ENDIF
C evaluates Xy,Ny and Py
      Xy=Time-Nm-Nw-Nd-Nh
      Ny=Xy
      IF(Ny.EQ.0) THEN
        Py=0.0
      ELSE
        Py=(Py*(Ny-1)+LoadHis(nHis-Nm-Nw-Nd-Nh))/Ny
      ENDIF
*      write(90,*) Ny,Nm,Nw,Nd,Nh,Pd
      END
	
***************************************************************
*4
C *********************************************************************
C FonG
C
C Created by: Patrice Pinel
C Initial Creation Date: October 8, 2002
C
C Function calculating the G function of the cylindrical heat source method
C
C This routine is called from CalcTermes
C
C This subroutine is called for the calculation of every term of the MLA scheme
C 
C
C INPUTS:
C	Time        Hours passed since the begining of the simulation
C	Alpha_s	    thermal difusivity of the ground
C	D_bor       Borehole diameter
C 
C OUTPUTS:
C	FunG        Value of the G function of the cylindrical heat source method
C
C *********************************************************************
      REAL FUNCTION FonG(Alpha_s,D_bore,Time)

      IMPLICIT NONE 
C     INPUTS
      INTEGER i
      REAL Alpha_s,D_bore,Time
C Fourrier number at the borehole wall
      REAL Fo
C Dumy variable
      REAL Z
C pi universal constant
      REAL pi
C Coefficients of Cooper's series
      REAL C(7)
C Euler's constant
      REAL Gam
      PARAMETER (Gam=0.57721)

C Init Cooper's coefficients, evaluate F0 and pi
      C(1)=1.128379
      C(2)=-0.5
      C(3)=0.2756227
      C(4)=-0.1499385
      C(5)=0.0617932
      C(6)=-0.01508767
      C(7)=0.001566857
      Fo=MAX(0.0,4.0*Alpha_s*Time/24.0/D_bore**2)
      Pi=ATAN(1.0)*4.0
C Use Cooper's equations
      IF (Fo.LE.6.124633) THEN
        Z=0.0
        DO 410 i=1,7
          Z=Z+C(i)*Fo**(i/2.0-0.5)
410     CONTINUE
          FonG=Z*Fo**0.5/2.0/pi
      ELSE
        Z=LOG(4*Fo/EXP(Gam))
        FonG=(2.0*Z*(8.0*Fo*(1.0+2.0*Fo)-1.0-3.0*Z)+16.0*Fo+pi**2.0+3.0)
     &/128.0/pi/Fo**2.0
      ENDIF

      END

***************************************************************
*5
C *********************************************************************
C InitHist
C
C Created by: Patrice Pinel
C Initial Creation Date: October 7, 2002
C
C This subroutine moves every term in the history vector by one space to account for the time increasing
C The last term (present time) is not initialised, this is done in GCEP_MLAiterate 
C as the load is changed with every iteration
C
C This routine is called from GCEP_SIM
C
C This subroutine is called once at the begining of every time step
C By default this should be for every hour simulated
C
C INPUTS:
C	nHis        number of terms in the LoadHis vector
C 
C OUTPUTS:
C	LoadHis	    vector containing the history of the ground loads
C
C *********************************************************************
      SUBROUTINE InitHist(nHis,LoadHis)

      IMPLICIT NONE 
C     INPUTS
      INTEGER nHis,i
      REAL LoadHis(nHis)

      DO 520 i=1,nHis-1
        LoadHis(i)=LoadHis(i+1)
520   CONTINUE

      END

***************************************************************
