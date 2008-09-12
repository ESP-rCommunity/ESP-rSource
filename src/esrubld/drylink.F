C This file is part of the ESP-r system.
C Copyright Energy Systems Research Unit, University of
C Strathclyde, Glasgow Scotland, 2001.

C ESP-r is free software.  You can redistribute it and/or
C modify it under the terms of the GNU General Public
C License as published by the Free Software Foundation 
C (version 2 orlater).

C ESP-r is distributed in the hope that it will be useful
C but WITHOUT ANY WARRANTY; without even the implied
C warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
C PURPOSE. See the GNU General Public License for more
C details.

C You should have received a copy of the GNU General Public
C License along with ESP-r. If not, write to the Free
C Software Foundation, Inc., 59 Temple Place, Suite 330,
C Boston, MA 02111-1307 USA.

      SUBROUTINE DRYLINK(icomp,qganp1,qganf1,qganp2,qganf2,wganp,wganf)
#include "building.h"

      PARAMETER (HF = 6.1, DIFFCO = 1.7E-10)

      DOUBLE PRECISION H2OF,H2OS,XMCS,XMCF,XMASTP,XMASRP,QWVTP,QWVRP
      DOUBLE PRECISION MTX, Y, X
      DOUBLE PRECISION AN, APP, AP, AM, AM1

      COMMON/SIMTIM/IHRP,IHRF,IDYP,IDYF,IDWP,IDWF,NSINC,ITS
      COMMON/pers/isd1,ism1,isd2,ism2,isds,isdf,ntstep
      COMMON/PREC7/ITCNST
      COMMON/ATPRES/PATMOS
      COMMON/WBULBO/IOPT
      COMMON/FVALG/GFA(MCOM)
      COMMON/PVALG/GPA(MCOM)
      COMMON/FVALA/TFA(MCOM),QFA(MCOM)
      COMMON/PVALA/TPA(MCOM),QPA(MCOM)
      COMMON/PREC6L/TQL1(MCOM,MT),TQL2(MCOM,MT),TQL3(MCOM,MT)

      COMMON/PVALAH/AHPA(MCOM)
      COMMON/FVALAH/AHFA(MCOM)

      COMMON/GANSAVE/qgans1(2),qgans2(2),qganst(2),wgans(2),h2os(2)
      COMMON/GANSAVE1/q24gp1,q24gf1,q24gp2,q24gf2,w24gp,w24gf
      COMMON/GANSAVE2/XEMC(2),xmcs(2),ATEMPP(2)              
      COMMON/DRYTIM1/AN, APP, AP, AM, AM1, JJ
      COMMON/DRYTIM2/MTX(4,5),X(4),Y(2,8),DELTAT,CONINI,TWPFA,MZI,KJ

      integer ncomp

C     ***************************************************************
C     Determine the Absolute Humidity (Kg/m^3) of air in the zone.
C     ***************************************************************
      
      AHPA(icomp) = AH(TPA(icomp),GPA(icomp),PATMOS,IOPT)
      AHFA(icomp) = AH(TFA(icomp),GFA(icomp),PATMOS,IOPT)
      AAH = (AHPA(icomp)+AHFA(icomp))*0.5

C     ***************************************************************
C     Skip this routine during the start-up period.
C     ***************************************************************

      JJ = 24*ntstep*ITCNST
      IF (NSINC .LT. JJ) GOTO 780

C     ***************************************************************
C     When icomp is 1 or 3, run through the whole routine.
C     When icomp is 2 or 4, pickup the values calculated for the 
C     icomp before it.
C     ***************************************************************     
      IF (icomp .EQ. 2 .OR. icomp .EQ. 4) GOTO 777
      IF (icomp .EQ. 1) THEN
         SGTIM = 0.7947
         THICK= 0.0275
         WIDTH=0.12
         XLGTH=3.9 
         TIMNUM=126.0
         XINIMC=0.67514
         MZI=1      
      ELSEIF (icomp .EQ. 3) THEN
         SGTIM = 0.7947
         THICK= 0.0275
         WIDTH=0.12
         XLGTH=3.9 
         TIMNUM=126.0
         XINIMC=0.7000
         MZI=2
      ENDIF

C     ***************************************************************
C     Calculate Average of Present and Future values of Air Dry-bulb 
C     temperature (deg C) and Humidity Ratio (KG/Kg). Determine the
C     Wet-bulb temperature (deg C), Saturation Humidity Ratio (Kg/Kg)
C     at Wet-bulb temperature and Moist air density (Kg/m3)    
C     ***************************************************************

      TPFA = (TPA(icomp)+TFA(icomp))*0.5
      GPFA = (GPA(icomp)+GFA(icomp))*0.5
      TWPFA=TWBCAL(TPFA,GPFA)
      YSWBPA = WHUMR (TWPFA)
      RHOAIRP = RHOAIR(TPFA,GPFA)

C     ***************************************************************
            
      IF (NSINC .EQ. JJ) THEN
           qganp1 = 0.0
           qganp2 = 0.0
           qganpt = 0.0
           wganp = 0.0
           qgans1(MZI) = 0.0
           qgans2(MZI) = 0.0
           qganst(MZI) = 0.0
           wgans(MZI) = 0.0
           XEMC(MZI) = 0.0
      ELSE
           qganp1=qgans1(MZI)
           qganp2=qgans2(MZI)
           qganpt=qganst(MZI)
           wganp=wgans(MZI)
      ENDIF

      DO 65 I=1,24
         ncomp = icomp + 1
         TQL1(ncomp,I)=0.0
         TQL2(ncomp,I)=0.0
         TQL3(ncomp,I)=0.0
  65  CONTINUE

C     ***************************************************************
C     Convective mass transfer coefficient using Chilton-Colburn 
C     analogies and  delta time (time-step)
C     *************************************************************** 
 
      HM = 8.3E-04 * HF   
      DELTAT = 3600.0/ntstep

C     ***************************************************************
C     Calculate basic info of the wood stack
C     VOLTOT - Total Volume (m3)
C     ODTOT  - Total OD weight of the stack (Kg)
C     H2OINI - Total initial amount of water in the stack (Kg)
C     SURTOT - Total surface area for convective heat and mass 
C              transfer (m2)
C     RHOTIM - Basic density of wood (Kg/m3)
C     CONINI - Initial moisture concentration (Kg/m3)
C     DELTAX - Cell thickness for numerical analysis of the partial 
C            differential equation using finite difference method (m)
C     ***************************************************************

      VOLTOT = THICK * WIDTH * XLGTH * TIMNUM
      ODTOT  = SGTIM * VOLTOT * 1000.00
      H2OINI = XINIMC * ODTOT 
      SURTOT = TIMNUM * WIDTH * XLGTH * 2.00
      RHOTIM = SGTIM * 1000.00
      DELTAX = THICK / (2.00*3.00)
      CONINI = XINIMC * SGTIM * 1000.00 
     
C     **************************************************************
C     Determine the Equilibrium Moisture Content (EMC) of the air
C     condition before entering the stack.
C     XEMC (ratio), TPFA (deg. C), GPFA(Kg/Kg)
C     **************************************************************

      XEMC(MZI) = EMC(TPFA,GPFA)

C Set the present value of moisture content
      IF (NSINC .EQ. JJ) THEN
        XMCP= XINIMC
        H2OP = H2OINI
        xmcs(MZI) = dble(0.0)
        h2os(MZI) = dble(0.0)
      ELSE
        XMCP=real(xmcs(MZI))
        H2OP=real(h2os(MZI))
      ENDIF     

C     ***************************************************************
C     Latent heat of evaporation, HWV (J/Kg), of water in wood.
C     XMC (ratio Kg/Kg), TDB (C)     ref:KEEY,1994
C     ***************************************************************

      TDBK = TPFA + 273.16
      IF (XMCP .GE. 0.30) THEN
         HWV = 2.792E06 - 160*TDBK - 3.43*TDBK*TDBK
      ELSE
         XMC14 = EXP(-14.0 * XMCP)
         HWV =2.792E06 - 160*TDBK - 3.43*TDBK*TDBK + 1.17E06*XMC14
      END IF

C Separate the heat requirement due to hygroscopic force from the 
C Latent heat of water in wood.
      IF (XMCP .GE. 0.30) THEN
        HWVL = HWV
        SHWV = 0.0
      ELSE
        HWVL = 2.792E06 - 160*TDBK - 3.43*TDBK*TDBK
        SHWV = 1.17E06*EXP(-14.0 * XMCP)
      ENDIF

C Calculate the thermal conductivity of moist wood
C XKMSTW (J/m C), RHOTIM (Kg/m3), XMC (ratio, Kg/Kg)
C ref: Wu, Q., Oliver, A.R. & Doe, P.E.
      IF (XMCP .LE. 0.40) THEN
           XKMSTW =0.04184 * ((SGTIM * (4.8 + 0.09*XMCP)) + 0.57)
      ELSE
           XKMSTW =0.04184 * ((SGTIM * (4.8 + 0.125*XMCP)) +0.57)
      END IF

C     ***************************************************************
C     Calculate the Specific heat capacity of moist wood (CPMW) base 
C     on the present moisture content.
C     CPMW (J/Kg K), XMC (ratio, Kg/Kg)          ref: Pang, S.
C     ***************************************************************

      CPMW = 4184.0*(XMCP+0.324)/(1.0+XMCP)

C     ***************************************************************
C     Calculate the coefficient for evaporative mass transfer from
C     the timber, ALPHA is a function of moisture content and species 
C     dependent.
C     ***************************************************************

      IF (XMCP .GE. 0.55) THEN
           ALPHA = 1.0
      ELSE
           ALPHA = 4.5541*XMCP*XMCP - 0.6985*XMCP
      END IF

C     ***************************************************************
C     Calculate the mass transfer of water vapour from the surface 
C     of the wood (XMASTRF) and latent heat transfer (QWVTP) due to 
C     evaporation.
C     XMASTP (Kg), XMASRP (Kg/s), QWVTP (J), QWVRP (J/s) 
C     RHOAIRP (Kg/m3), HM (m/s), SURTOT (m2), YSWBPA & GPA (Kg/Kg)
C     ***************************************************************

      XMASTP = RHOAIRP*SURTOT*HM*(YSWBPA - GPFA)*DELTAT*ALPHA
      QWVTP = HWV * XMASTP
      XMASRP = RHOAIRP*SURTOT*HM*(YSWBPA - GPFA)*ALPHA
      QWVRP = HWV * XMASRP
      qganft = real(QWVRP)
      wganf = real(XMASRP)

      IF (XMCP .GE. 0.30) THEN
        qganft = real(QWVRP)
        qgshwv = 0.0
      ELSE
        qganft = HWVL * real(XMASRP)
        qgshwv = SHWV * real(XMASRP)
      ENDIF

C     ***************************************************************
C     Sensible heat to increase temperature of water vapor from wet-
C     bulb temp to air dry-bulb temp.
C     ***************************************************************

C       SPHTCPA=SPHTC2(TPFA,GPFA)
      TG = TPFA + 273.16
      SPHWV = -247.5 + 22.2*TG - 8.043E-02*TG*TG +9.99E-05*TG*TG*TG
      SENHTC = real(XMASRP) * SPHWV * (TPFA - TWPFA)
      qganf2 = 0.0 - SENHTC

C     ***************************************************************
C     Coefficients of the finite differnce equations for temperature
C     profile in wood, AP, AN and APP. (Area = unity)
C     ***************************************************************

      KJ = 0
      AN = XKMSTW/DELTAX
      APP = RHOTIM*CPMW*DELTAX/DELTAT 
      AP = APP + 2.0*AN
      AM = APP*0.5 + AN + dble(HF)
      AM1 = HF*TPFA - ((QWVRP)/(SURTOT))
      CALL MATGAU

C     ***************************************************************
C     Coefficients of the finite difference equations for moisture
C     profile in wood, AP, AN and APP. Area = unity
C     DIFFCO - Diffusion coefficient (m2/s) 
C     ***************************************************************

      KJ = 1
      AN = DIFFCO / DELTAX
      APP = DELTAX / DELTAT
      AP = APP + 2.0*AN
      AM = AN + APP*0.5
      AM1 = 0.0  - ((XMASRP)/(SURTOT))
      CALL MATGAU 

C Calculate the new moisture content and average concentration
      H2OF = H2OP - XMASTP       
      XMCF = H2OF / ODTOT
      XCONF = REAL(H2OF/DBLE(VOLTOT))

C Calculate the average wood temperature and delta heat storage
      ATEMP = real((Y(MZI,1)+2.0*(Y(MZI,2)+Y(MZI,3)+Y(MZI,4)))/7.0)
      DELTMP = ATEMP - ATEMPP(MZI)
      ATEMPP(MZI) = ATEMP
      HSTR = RHOTIM * CPMW * VOLTOT * DELTMP

C Calculate the moisture content from the moisture concentration
C profile. CMC (Kg/Kg), XACON - average concentration
      XACON = real((Y(MZI,5) + 2.0*(Y(MZI,6)+Y(MZI,7)+Y(MZI,8)))/7.0)
      CMC = XACON/RHOTIM

C Rate of Heat transfer to the timber due to convection,QHFRF(J/s)
      QHFRF = HF * (TPFA- real(Y(MZI,4))) * SURTOT 
      qganf1 = QHFRF

C Fill the common block of Latent heat for moiture added to air
C due to evaporation from air.
      DO 66 I=1,24
          ncomp = icomp + 1
          TQL1(ncomp,I)=(qganpt+qganft)*0.5
          TQL2(ncomp,I)=(qganpt+qganft)*0.5
          TQL3(ncomp,I)=(qganpt+qganft)*0.5
  66  CONTINUE

      q24gp1=qganp1
      q24gf1=qganf1
      q24gp2=qganp2
      q24gf2=qganf2
      w24gp=wganp
      w24gf=wganf   

      qgans1(MZI)=qganf1
      qgans2(MZI)=qganf2
      qganst(MZI)=qganft
      wgans(MZI)=wganf
      xmcs(MZI)=XMCF
      h2os(MZI)=H2OF

      RETURN

 777  qganp1=q24gp1
      qganf1=q24gf1
      qganp2=q24gp2
      qganf2=q24gf2
      wganp=w24gp
      wganf=w24gf

      IF (icomp .EQ. 4 .AND. ITS .EQ. ntstep) CALL WRITC
      
      RETURN

 780  qganp1=0.0
      qganf1=0.0
      qganp2=0.0
      qganf2=0.0
      wganp=0.0
      wganf=0.0

      DO 67 I=1,24
          ncomp = icomp + 1
          TQL1(ncomp,I)=0.0
          TQL2(ncomp,I)=0.0
          TQL3(ncomp,I)=0.0
  67  CONTINUE

      RETURN
      END

C********************************************************************
      SUBROUTINE MATGAU
       
      DOUBLE PRECISION MTX, Y, X, ASTAR, XSTAR
      DOUBLE PRECISION AN, APP, AP, AM, AM1
      INTEGER FLAG

      COMMON/SIMTIM/IHRP,IHRF,IDYP,IDYF,IDWP,IDWF,NSINC,ITS
      COMMON/SET1/IYEAR,IBDOY,IEDOY,IFDAY,IFTIME
      COMMON/DRYTIM1/AN, APP, AP, AM, AM1, JJ
      COMMON/DRYTIM2/MTX(4,5),X(4),Y(2,8),DELTAT,CONINI,TWPFA,MZI,KJ      

C     ***************************************************************
C     Matrix of the equations at the nodes using finite difference
C     method. MTX(4,5)
C     ***************************************************************

      IF (KJ .EQ. 1) GOTO 800
      IF (NSINC .EQ. JJ) THEN
      DO 220 I=1,4
            X(I) = TWPFA
 220  CONTINUE 
      ELSE
      DO 230 I=1,4
            X(I) = Y(MZI,I)
 230  CONTINUE
      END IF
      GOTO 888
 800  IF (NSINC .EQ. JJ) THEN
      DO 260 I=1,4
            X(I) = CONINI
 260  CONTINUE 
      ELSE
      DO 270 I=1,4
             II=I+4
              X(I) = Y(MZI,II)
 270  CONTINUE
      END IF

 888   MTX(1,1) = AP
       MTX(2,2) = AP
       MTX(3,3) = AP
       MTX(1,2) = -2*AN
       MTX(2,1) = -AN
       MTX(3,2) = -AN
       MTX(4,3) = -AN
       MTX(2,3) = -AN
       MTX(3,4) = -AN
       MTX(4,4) = AM
       MTX(1,3) = 0.0
       MTX(1,4) = 0.0
       MTX(2,4) = 0.0
       MTX(3,1) = 0.0
       MTX(4,1) = 0.0
       MTX(4,2) = 0.0
       MTX(1,5) = APP*X(1)
       MTX(2,5) = APP*X(2)
       MTX(3,5) = APP*X(3)
       MTX(4,5) = APP*X(4)*0.5 + AM1

C     ***************************************************************
C                         GAUSS-SEIDEL ITERATION
C     ***************************************************************
C     Normalise diagonal element in each row.
C     ***************************************************************

      DO 35 I= 1,4
        ASTAR = MTX(I,I)
        DO 25 J=1,5
          IF (int(MTX(I,J)) .EQ. 0) GOTO 25
          MTX(I,J) = MTX(I,J) / ASTAR
  25    CONTINUE
  35  CONTINUE

C Begin Gauss-Seidel iterations
      DO 100 ITER=1,200
        FLAG = 1
        DO 40 I=1,4
          XSTAR =X(I)
          X(I) = MTX(I,5)

C Fine new solution value, X(I)
          DO 110 J=1,4
            IF (I .EQ. J) GOTO 110
            X(I) = X(I) - MTX(I,J)*X(J)
 110      CONTINUE

C Test X(I) for convergence
          CVERG = 0.0001
          IF (ABS(XSTAR - X(I)) .LE. CVERG) GOTO 40
          FLAG=0
 40     CONTINUE
        IF (FLAG .NE. 1) GOTO 100
        IF (KJ .EQ. 0) THEN 
          DO 41 I=1,4
            Y(MZI,I) = X(I)
 41       CONTINUE
        ELSE
          DO 42 I=1,4
            II=I+4
            Y(MZI,II) = X(I)
 42       CONTINUE
        END IF
      RETURN
 100  CONTINUE

C     ***************************************************************
C     Indicative value of 88 if method did not converge
C     ***************************************************************

      DO 88 I=1,4
           X(I) = dble(88.0)
  88  CONTINUE

      IF (KJ .EQ. 0) THEN
          DO 86 I=1,4
              Y(MZI,I) = X(I)
  86      CONTINUE 
      ELSE
          DO 87 I=1,4
              II=I+4
              Y(MZI,II) = X(I)
  87      CONTINUE
      END IF
      RETURN
      END

C********************************************************************

      SUBROUTINE WRITC

      DOUBLE PRECISION MTX, Y, X, XMCS

      COMMON/SIMTIM/IHRP,IHRF,IDYP,IDYF,IDWP,IDWF,NSINC,ITS
      COMMON/pers/isd1,ism1,isd2,ism2,isds,isdf,ntstep
      COMMON/DRYTIM2/MTX(4,5),X(4),Y(2,8),DELTAT,CONINI,TWPFA,MZI,KJ 

      COMMON/GANSAVE2/XEMC(2),xmcs(2),ATEMPP(2)
      logical close
   
      IF (IHRF .EQ. 1) THEN
           IDAY=IDYF
      ELSE
           IDAY=IDYP
      ENDIF
      DTIME = 3600.0/FLOAT(NTSTEP)
      TIMES=IDAY+(IHRF-1+ITS*DTIME/3600.0)/24.0
      IMSTR = 100

      DIFFT = TIMES - INT(TIMES)

      call eclose(difft,0.5,0.001,close)
      IF (close) THEN

        WRITE(IMSTR,212)TIMES,XEMC(1),XMCS(1),XEMC(2),XMCS(2)
 212    FORMAT (1X,F6.2,1X,F5.3,1X,F5.3,1X,F5.3,1X,F5.3,1X,E12.5)

C      WRITE(IMSTR,212)TIMES,XMCS(1),(Y(1,K),K=1,8),XMCS(2),
C     &(Y(2,K),K=1,8)
C 212  FORMAT (1X,F6.2,1X,F5.3,1X,F5.2,1X,F5.2,1X,F5.2,1X,F5.2,1X,F6.2,
C     &1X,F6.2,1X,F6.2,1X,F6.2,1X,F5.3,1X,F5.2,1X,F5.2,1X,F5.2,1X,F5.2,
C     &1X,F6.2,1X,F6.2,1X,F6.2,1X,F6.2)

C      WRITE(IMSTR,212)TIMES,XMCS(1),(Y(1,K),K=1,4),XMCS(2),
C     &(Y(2,K),K=1,4)
C 212  FORMAT (1X,F6.2,1X,F5.3,1X,F5.2,1X,F5.2,1X,F5.2,1X,F5.2,
C     &1X,F5.3,1X,F5.2,1X,F5.2,1X,F5.2,1X,F5.2)

C      WRITE(IMSTR,212)TIMES,XMCS(1),(Y(1,K),K=4,8),XMCS(2),
C     &(Y(2,K),K=4,8)
C 212  FORMAT (1X,F6.2,1X,F5.3,1X,F6.2,1X,F6.2,1X,F6.2,1X,F6.2,
C     &1X,F5.3,1X,F6.2,1X,F6.2,1X,F6.2,1X,F6.2)

      ENDIF 
   
      RETURN
      END

C********************************************************************
C      FUNCTIONS
C********************************************************************

C     ***************************************************************
C      Humidity ratio (Kg/Kg) at wet-bulb temperature
C     ***************************************************************
       FUNCTION WHUMR(TWBA)
       DOUBLE PRECISION PWSL,PWS
       PWSL=-7511.52/(TWBA+273.16)+89.63121+0.02399897*(TWBA+273.16)-
     & 1.1654551E-5*(TWBA+273.16)**2 -1.2810336E-8*(TWBA+273.16)**3 +
     & 2.0998405E-11*(TWBA+273.16)**4-12.150799*LOG(TWBA+273.16)
       PWS = EXP(PWSL)
       WHUMR = real(dble(0.62198) * PWS/(dble(101.325) - PWS))
       RETURN
       END

C     ***************************************************************
C      Determine the density, RHOAIR (kg/m^3), of moist air from 
C      Dry-bulb temperature (deg C) and humidity ratio (Kg/Kg)
C      RHOAIR from ASHRAE Brochure on Psychrometery, 1977 Pg 30
C     ***************************************************************

       FUNCTION RHOAIR(TDB,YAIR)
       SPVMA = 0.280705*(TDB+273.16)*(1.0+1.6078*YAIR)/101.325
       RHOAIR = (1.0+YAIR)/SPVMA
       RETURN
       END

C     ***************************************************************
C      Determine the Equilibrium Moisture Content (EMC) of timber 
C      under the climatic conditions in the dryer.
C     ***************************************************************

       FUNCTION EMC(TDB,GA)       
       DOUBLE PRECISION A1,A2,A3,EMC1,EMC2
       COMMON/ATPRES/PATMOS
 
       TDBF = 1.8*TDB + 32.0
       RH=PCRH2(TDB,GA,PATMOS)
       RH = RH*0.01
       A1 = 3.73 + TDBF*3.642E-02 - TDBF*TDBF*1.547E-04
       A2 = 0.674 + TDBF*1.053E-03 - TDBF*TDBF*1.714E-06
       A3 = 216.9 + TDBF*1.961E-02 + TDBF*TDBF*5.72E-03
       EMC1=(A1*A2*RH)/(dble(1.0)+A1*A2*RH)
       EMC2=(A2*RH)/(dble(1.0)-A2*RH)
       EMC=real((EMC1+EMC2)*18.0/A3)
       
C       EMC=(((A1*A2*RH)/(1.0+A1*A2*RH))+((A2*RH)/(1.0-A2*RH)))*18.0/A3
       RETURN
       END

C     ***************************************************************
C     Determine the Wet-bulb temperature (deg C) from Dry-bulb
C     temperature (deg C) and humidity ratio (Kg/Kg) readings.
C     ***************************************************************

       FUNCTION TWBCAL(TDB,YAIR)
       TWB = TDB
       DO 133 I=1,15000
           TWB = TWB - 0.001
           TWBK = TWB + 273.16
           PWSL = -7511.52/TWBK + 89.63121 + 0.02399897*TWBK -
     &  1.1654551E-5*TWBK**2 -1.2810336E-8*TWBK**3 + 
     &  2.0998405E-11*TWBK**4 - 12.150799*LOG(TWBK)
           PWS = EXP (PWSL)
           YSUR = 0.62198*PWS/(101.325 - PWS)
           WPWS1 =(2501.0 - 2.411*TWB)*YSUR - 1.006*(TDB-TWB)
           WPWS = WPWS1/(2501.0 + 1.775*TDB - 4.186*TWB)
           DIFF = WPWS - YAIR
           IF (ABS(DIFF) .LE. 0.00001) GOTO 20
  133   CONTINUE
  20    TWBCAL=TWB
        RETURN
        END

C      **************************************************************
C      Determine the absolute humidity (Kg/m^3)
C      **************************************************************

       FUNCTION AH(TPF,GPF,PATMOS,IOPT)

       TWSPV = TWBCAL(TPF,GPF)
       SPV = SPVOL1(TPF,TWSPV,PATMOS,IOPT)
       AH = GPF/SPV
       RETURN
       END
