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

C Air psychrometrics functions.
C The following functions represent the psychrometrics
C of air. Units are SI but with with kJ instead of J
C and mbar instead of Pa.

C ******************** TAIRFE  
C TAIRFE: Returns air temperature from enthalpy & moisture content.
      FUNCTION TAIRFE(ENTH,W)
      TAIRFE=(ENTH+0.026-(2501.0*W))/(1.006+1.805*W)
      RETURN
      END

C ******************** TSATH0
C TSATH0: Determines the saturation temperature from enthalpy.
      FUNCTION TSATH0(H1,PATM)
      DIMENSION A(5,3)

C Polynomial coefficients.
      DATA A/-5.80119,6.64782E-01,-5.01455E-03,2.49725E-05,-5.50151E-08,
     &-3.70951,5.50403E-01,-2.90194E-03,8.83702E-06,-1.0862E-08,
     &1.78663E01,1.72699E-01,-2.47968E-04,1.69221E-07,-2.95851E-11/

C Check range of H1.
      IF(H1.LT.-10.0)goto 1
      IF(H1.GT.600.0)goto 2

C Find relevant curve.
      I=1
      IF(H1.GT.75.0)I=2
      IF(H1.GT.250.0)I=3
      H2=H1*H1
      H3=H1*H2
      H4=H2*H2
      T1=A(1,I)+A(2,I)*H1+A(3,I)*H2+A(4,I)*H3+A(5,I)*H4
      goto 3
    1 T1=-13.4
      goto 3
    2 T1=65.0

C For the following iteration, set size of step (TINC) according to PATM.
    3 TINC=ABS(PATM-1013.25)/50.0
      IF(TINC.LT.0.02)TINC=0.02
      IF(TINC.GT.5.0)TINC=5.0
      TINC1=TINC

C Now use T1 as the first guess in an iteration.
  600 HGUESS=ENTHP1(T1,T1,PATM,1)
      IF(HGUESS.GE.H1.AND.TINC.GT.0.0)TINC=-0.5*TINC
      IF(HGUESS.LE.H1.AND.TINC.LT.0.0)TINC=-0.5*TINC
      HDIFF=H1-HGUESS
      IF(ABS(HDIFF).LT.0.01)goto 4
      T1=T1+TINC
      IF(ABS(TINC).GT.0.001)goto 600
      TINC=TINC1/2.0

    4 TSATH0=T1
      RETURN
      END

C ******************** TDB
C TDB calculates dry bulb temperature from enthalpy and
C moisture content. It is the reversal of the ENTHP2 function.
      FUNCTION TDB(H,GS)
C First find the relevant equation based on the fundamental data
C from B.S. Engineers, Section 8, equations 7-20.

C Estimate likely temperature.
      iok=0
      tde=0.
      TD=(H-2501.0*GS)/(1.006+1.84*GS)
      X=(TD+20.)/10.0
      ISW=IFIX(X)
      IF(ISW.LT.1)ISW=1
      IF(ISW.GT.7)ISW=7
   11 IF(ISW.EQ.1)THEN
        TD=(H-2500.822*GS)/(1.00545+1.8375*GS)
      elseif(ISW.EQ.2)THEN
        TD=(H-2500.92*GS)/(1.00597+1.84*GS)
      elseif(ISW.EQ.3)THEN
        TD=(H-2501.02*GS)/(1.00597+1.83*GS)
      elseif(ISW.EQ.4)THEN
        TD=(H-2501.22*GS+0.0045)/(1.0062+1.82*GS)
      elseif(ISW.EQ.5)THEN
        TD=(H-2501.82*GS+0.0213)/(1.00676+1.8*GS)
      elseif(ISW.EQ.6)THEN
        TD=(H-2503.02*GS+0.0337)/(1.00707+1.77*GS)
      elseif(ISW.EQ.7)THEN
        TD=(H-2503.52*GS+0.0692)/(1.00778+1.76*GS)
      ENDIF
      ISW1=IFIX((TD+20.0)/10.0)
      if(isw1.lt.1)isw1=1
      if(isw1.gt.7)isw1=7
      IF(ISW1.EQ.ISW)goto 9

      if(iok.eq.0)then
         iok=1
         ISW=ISW1
         tde=td
         goto 11
      else
         td=(tde+td)/2.
      endif

    9 TDB=TD
      RETURN
      END

C ******************** ENTHP1
C ENTHP1: Calculates enthalpy of moist air kJ/kg.
      FUNCTION ENTHP1(TD,TW,PATM,IOPT)
      GS=HUMRAT(TD,TW,PATM,IOPT)
      ENTHP1=ENTHP2(TD,GS)
      RETURN
      END

C ******************** ENTHP2
C ENTHP2: Calculates enthalpy of moist air kJ/kg.
C Find the relevant equation for enthalpy of air & water vapour based
C on equation from fundamental data for B.S. engineers, Section 8,
C Eqns 7-20.
      FUNCTION ENTHP2(TD,GS)
      ISW=IFIX((TD+20.0)/10.0)
      IF(ISW.LT.1)ISW=1
      IF(ISW.GT.7)ISW=7
      IF(ISW.EQ.1)THEN
        HA=1.00545*TD
        HG=2500.822+1.8375*TD
      elseif(ISW.EQ.2)THEN
        HA=1.00597*TD
        HG=2500.92+1.84*TD
      elseif(ISW.EQ.3)THEN
        HA=1.00597*TD
        HG=2519.32+1.83*(TD-10.0)
      elseif(ISW.EQ.4)THEN
        HA=1.0062*TD-0.0045
        HG=2537.62+1.82*(TD-20.0)
      elseif(ISW.EQ.5)THEN
        HA=1.00676*TD-0.0213
        HG=2555.82+1.8*(TD-30.0)
      elseif(ISW.EQ.6)THEN
        HA=1.00707*TD-0.0337
        HG=2573.82+1.77*(TD-40.0)
      elseif(ISW.EQ.7)THEN
        HA=1.00778*TD-0.0692
        HG=2591.52+1.76*(TD-50.0)
      ENDIF
      ENTHP2=HA+GS*HG
      RETURN
      END

C ******************** HUMRAT
C HUMRAT: Calculates humidity ratio kg/kg.
C Equation C1.3 IHVE(1975). Values of FS from fundemental data
C for B.S. engineers.
C NB FS depends on atmospheric pressure - see W P Jones page 34,
C table 2.1 and ASHRAE handbook of fundamentals 1981 page 5.4, table 2.
      FUNCTION HUMRAT(TD,TW,PATMOS,IOPT)
      PS=VAPRS1(TD,TW,PATMOS,IOPT)
      FS=-7.3E-06*TD+1.00444
      IF(TD.GE.11.0.AND.TD.LT.26.0)FS=1.32E-05*TD+1.004205
      IF(TD.GE.26.0)FS=4.05E-05*TD+1.003497

      HUMRAT=0.62197*FS*PS/(PATMOS-FS*PS)
      RETURN
      END

C ******************** HUMRT1
C HUMRT1: Calculates humidity ratio kg/kg.
C Equation C1.3 IHVE(1975). Values of FS from fundemental data
C for B.S. engineers.
C NB FS depends on atmospheric pressure - see W P Jones page 34,
C table 2.1 and ASHRAE handbook of fundamentals 1981 page 5.4, table 2.
C TD is the dry bulb temperature.
C RH is relative humidity between 0.0 and 100.0.
      FUNCTION HUMRT1(TD,RH,PATMOS,IOPT)
      PS=RH*SATVP(TD)/100.0

      FS=-7.3E-06*TD+1.00444
      IF(TD.GE.11.0.AND.TD.LT.26.0)FS=1.32E-05*TD+1.004205
      IF(TD.GE.26.0)FS=4.05E-05*TD+1.003497

      HUMRT1=0.62197*FS*PS/(PATMOS-FS*PS)
      RETURN
      END

C ******************** HUMR
C Calculate humidity ratio from Eastop & McConkey Applied Thermodynamics
C for Engineering Technologists. Equation 14.3 pp 492.

      FUNCTION HUMR(TD,RH,PATMOS)
      x1=(rh/100.)*satvp(td)
      x2=patmos-((rh/100.)*satvp(td))
      humr=0.62197*x1/x2
      RETURN
      END

C ******************** PSATR
C Calculate percentage saturation. from Eastop & McConkey Applied
C Thermodynamics for Engineering Technologists. Equation 14.7 pp 496.

      FUNCTION PSATR(TD,RH,PATMOS)
      psatr=humr(TD,RH,PATMOS)*(patmos-satvp(td))/(0.62197*satvp(td))
      psatr=psatr*100.
      RETURN
      END

C ******************** VAPRS2
C VAPRS2: Evaluates vapour pressure (mbar).
C Equation C1.3 IHVE(1975). Values of FS from fundemental data
C for B.S. engineers.
C NB FS depends on atmospheric pressure - See W P Jones page 34,
C table 2.1 and ASHRAE handbook of fundamentals 1981 page 5.4, table 2.
      FUNCTION VAPRS2(TD,GS,PATMOS)

      FS=-7.3E-06*TD+1.00444
      IF(TD.GE.11.0.AND.TD.LT.26.0)FS=1.32E-05*TD+1.004205
      IF(TD.GE.26.0)FS=4.05E-05*TD+1.003497

      VAPRS2=(GS*PATMOS)/(FS*(0.62197+GS))
      RETURN
      END

C ******************** SATVP
C SATVP: Evaluates saturated vapour pressure (mbar).
      FUNCTION SATVP(TD)
      DOUBLE PRECISION T
      T=dble(TD)+dble(273.16)

C Check for above ice/water.
      IF(TD.GE.0.0)goto 1

C Ice.
      SATVP=real(dble(10.0)**(dble(10.5380997)-
     &                (dble(2663.91)/(T- dble(0.01)))))
      RETURN

C Water.
    1 SVP=real((dble(28.59051)-dble(8.2)*
     &    DLOG10(T)+dble(2.4804E-03)*T-dble(3142.31)/T))
      SATVP=(10.0**(SVP))*1000.0
      RETURN
      END


C ******************** VAPRS1 
C VAPRS1: 
      FUNCTION VAPRS1(TD,TW,PATMOS,IOPT)
      DIMENSION FACTOR(2,2)
      DATA FACTOR/6.66E-4,5.94E-4,7.99E-4,7.2E-4/
      I=1
      IF(TW.LT.0.0)I=2
      VAPRS1=SATVP(TW)-FACTOR(I,IOPT)*PATMOS*(TD-TW)
      RETURN
      END

C ********************  TWB
C TWB: Calculates the wet bulb temperature from
C dry bulb temperature and moisture content.
      FUNCTION TWB(TD,GS,PATMOS,IOPT)
      H=ENTHP2(TD,GS)

C First guess is adiabatic saturation temperature.
      T1=TSATH0(H,PATMOS)
      TINC=0.5

   10 GS1=HUMRAT(TD,T1,PATMOS,IOPT)
      IF(GS1.GE.GS.AND.TINC.GT.0.0)TINC=-0.5*TINC
      IF(GS1.LE.GS.AND.TINC.LT.0.0)TINC=-0.5*TINC
      T1=T1+TINC
      GDIFF=GS1-GS
      IF(ABS(GDIFF).LT.0.00001)goto 1
      IF(ABS(TINC).GT.0.005)goto 10
      TINC=0.25
      goto 10

    1 TWB=T1
      RETURN
      END

C ********************  TWB_ACMODEL
C TWB: Calculates the wet bulb temperature from
C dry bulb temperature and moisture content.
C This subroutine is the same as TWB with the 
C exception that a maximum number of iterations is
C implemented here. If we reach the maximum number 
C of iterations then the iteration process  stops. 
      FUNCTION TWB_ACMODEL(TD,GS,PATMOS,IOPT)
      H=ENTHP2(TD,GS)

C First guess is adiabatic saturation temperature.
      T1=TSATH0(H,PATMOS)
      TINC=0.5

      iteration_counter = 1

   10 GS1=HUMRAT(TD,T1,PATMOS,IOPT)
      IF(GS1.GE.GS.AND.TINC.GT.0.0)TINC=-0.5*TINC
      IF(GS1.LE.GS.AND.TINC.LT.0.0)TINC=-0.5*TINC
      T1=T1+TINC
      GDIFF=GS1-GS
      IF(ABS(GDIFF).LT.0.00001)goto 1
      IF(ABS(TINC).GT.0.005)goto 10
      TINC=0.25
      iteration_counter = iteration_counter + 1
      if(iteration_counter.gt.100) then
        write(*,*)
     &  'Max number of iterations reached in FUNCTION TWB_ACMODEL'
        goto 1
      endif

      goto 10

    1 TWB_ACMODEL=T1

      RETURN
      END

C ******************** PCSAT1
C PCSAT1: Evaluates percentage saturation.
      FUNCTION PCSAT1(TD,TW,PATMOS,IOPT)
      PCSAT1=100.0*HUMRAT(TD,TW,PATMOS,IOPT)/HUMRAT(TD,TD,PATMOS,IOPT)
      RETURN
      END

C ******************** PCSAT2
C PCSAT2: Evaluates percentage saturation.
      FUNCTION PCSAT2(TD,GS,PATMOS)
      PCSAT2=100.0*GS/HUMRAT(TD,TD,PATMOS,IOPT)
      RETURN
      END

C ******************** PCRH1
C PCRH1: Evaluates relative humidity.
      FUNCTION PCRH1(TD,TW,PATMOS,IOPT)
      PCRH1=100.0*VAPRS1(TD,TW,PATMOS,IOPT)/SATVP(TD)
      RETURN
      END

C ******************** PCRH2
C PCRH2: Evaluates relative humidity.
      FUNCTION PCRH2(TD,GS,PATMOS)
      PCRH2=100.0*VAPRS2(TD,GS,PATMOS)/SATVP(TD)
      RETURN
      END

C ******************** SPVOL1
C SPVOL1: Calculates specific volume m^3/kg dry air.
      FUNCTION SPVOL1(TD,TW,PATMOS,IOPT)
      GS=HUMRAT(TD,TW,PATMOS,IOPT)
      SPVOL1=SPVOL2(TD,GS,PATMOS)
      RETURN
      END

C ******************** SPVOL2
C SPVOL2: Calculates specific volume m^3/kg dry air.
      FUNCTION SPVOL2(TD,GS,PATMOS)
      DIMENSION AAFACS(2,3),WWFACS(2,7)

C GASCON has the value 2.8704, made up of three factors:

C Universal gas constant [8.31441 kJ/(kmol K)], divided
C by the molecular mass of air [28.966 kg/kmol] and by a
C conversion factor [0.1 kpa/mbar].
      DATA GASCON/2.8704/

C Values for AAA, AAW  from fundamental data for B S Engineers.
      DATA AAFACS/4.56E-4,8.3E-6,4.43E-4,6.9E-6,4.048E-4,5.7E-6/
      DATA WWFACS/6.318,0.1622,6.318,0.1105,6.076,0.0863,5.634,0.0642,
     &            5.262,0.0518,4.862,0.0418,4.462,0.0338/
      I=1
      IF(TD.GT.10.0)I=2
      IF(TD.GT.30.0)I=3
      AAA=AAFACS(1,I)-TD*AAFACS(2,I)
      AAW=(1.45-0.0097*TD+0.32*(TD/100.0)*(TD/100.0))*0.001

      I=IFIX((TD+20.0)/10.0)
      IF(I.LE.1)I=1
      IF(I.GT.7)I=7

      XA=0.62197/(0.62197+GS)
      ONEMXA=1.0-XA

C Ideal gas (extra item for improved accuracy).
      SPVOL2=GASCON*(TD+273.15)/(PATMOS-VAPRS2(TD,GS,PATMOS))
     &-(XA*XA*AAA+XA*ONEMXA*2.0*AAW+ONEMXA*ONEMXA*AAW)

      RETURN
      END

C ******************** HUVOL1
C HUVOL1: Evaluates humid volume m^3/kg.
C Equation from IHVE App.C, p28.
C Factor of 100 is for mbar to kpa.
      FUNCTION HUVOL1(TD,TW,PATMOS,IOPT)
      PS=VAPRS1(TD,TW,PATMOS,IOPT)

      HUVOL1=(2.87*(273.15+TD))/(PATMOS-PS)

      RETURN
      END

C ******************** HUVOL2
C HUVOL2: Evaluates humid volume m^3/kg.
      FUNCTION HUVOL2(TD,GS,PATMOS)
      PS=VAPRS2(TD,GS,PATMOS)

C Equation from IHVE App.C, p28.
C Factor of 100 is for mbar to kpa.
      HUVOL2=(2.87*(273.15+TD))/(PATMOS-PS)

      RETURN
      END

C ******************** SPHTC1
C SPHTC1: Returns the specific heat capacity of air kJ/(kg K).
      FUNCTION SPHTC1(TD,TW,PATMOS,IOPT)
      GS=HUMRAT(TD,TW,PATMOS,IOPT)
      SPHTC1=SPHTC2(TD,GS)
      RETURN
      END

C ******************** SPHTC2
C SPHTC2: Returns the specific heat capacity of air kJ/(kg K).
      FUNCTION SPHTC2(TD,GS)
      TD1=TD+0.5
      TD2=TD-0.5
      SPHTC2=ENTHP2(TD1,GS)-ENTHP2(TD2,GS)
      RETURN
      END

C ********************  DEWPT 
C DEWPT: Makes a crude estimate of dew point based on moisture
C content using curve fit. Using this estimate it iterates to a more
C refined value.
      FUNCTION DEWPT(G1,PATMOS)
      DIMENSION A(4,3)
      logical CLOSE

      DATA A/-1.97465E1,7.19837E3,-5.99776E5,2.17091E7,
     &-4.05519,2.21854E3,-4.63971E4,4.03557E5,
     &1.5746E1,6.568E2,-3.80307E3,9.18275E3/

      IF(G1.LT.0.0)goto 500
      CALL ECLOSE(G1,0.0,0.001,CLOSE)
      IF(CLOSE)goto  650
      IF(G1.GT.0.16)goto 510
      G2=G1*G1
      G3=G2*G1

C Find range for polynomial.
      I=1
      IF(G1.GT.0.01)I=2
      IF(G1.GT.0.035)I=3
      T1=A(1,I)+A(2,I)*G1+A(3,I)*G2+A(4,I)*G3
      goto 550
  500 T1=-50.0
      goto 550
  510 T1=60.0

C For the iteration, set size of step (TINC) according to PATMOS.
  550 TINC=ABS(PATMOS-1013.25)/50.0
      IF(TINC.LT.0.02)TINC=0.02
      IF(TINC.GT.5.0)TINC=5.0

C Now use T1 as first guess in iteration.
  600 GUESS=HUMRAT(T1,T1,PATMOS,1)
      IF(GUESS.GE.G1.AND.TINC.GT.0.0)TINC=-0.5*TINC
      IF(GUESS.LE.G1.AND.TINC.LT.0.0)TINC=-0.5*TINC
      T1=T1+TINC
      GDIFF=GUESS-G1
      IF(ABS(GDIFF).LT.0.00001)goto 650
      IF(ABS(TINC).GT.0.005)goto 600
      TINC=0.05
      goto 600

  650 CALL ECLOSE(G1,0.0,0.001,CLOSE)
      IF(CLOSE)T1=-99.9

      DEWPT=T1
      RETURN
      END

C Properties of Water

C ******************** SHTH2O
C SHTH2O: Water specific heat capacity kJ/(kg K).
      FUNCTION SHTH2O(TEMP)
      SHTH2O=4.186
      RETURN
      END

C ******************** DENH2O
C DENH2O: Water density kg/m^3.
      FUNCTION DENH2O(TEMP)
      DENH2O=1000.0
      RETURN
      END

C ********************* AIRDEN *********************
C AIRDEN - air density as a function of temperature.
C Source J Denev.
      FUNCTION AIRDEN(T)

C the ideal gas correlation.
      DATA A1,A2,A3/1.1881,293.15,273.15/
      AIRDEN=A1*A2/(A3+T)
      RETURN
      END

C ********************* AIRBET *********************
C AIRBET - thermal expansion coefficient of the air as a function of 
C the temperature based on the ideal gas correlation.
      FUNCTION AIRBET(T)

      DATA A1/273.15/
      AIRBET=1.0/(T+A1)
      RETURN
      END
C ******************** CNDWAT1 ********************

C CNDWAT1 is a real function which returns the energy (J/kg)
C dissipated/necessary in case of condensation/evaporation
C of a unit mass of water as a function of (TEMP) the water
C temperature (C)
C Source for the expression: Recknagel-Sprenger 1979. "Taschenbuch fur
C heizung und klimatechnik", R.Oldenbourg Verlag, Munich

      FUNCTION CNDWAT1(TEMP)

      COMMON/SHOUT/ICOUT
      COMMON/WRNOFF/IWMOFF

      VTEMP=TEMP

      IF(TEMP.LT.0..OR.TEMP.GT.100.) THEN

         IF(TEMP.LT.0.0) THEN
            VTEMP=0.0
         ELSE
            VTEMP=100.
         ENDIF

         IF(IWMOFF.EQ.0) THEN
            WRITE(ICOUT,*) ' CNDWAT: water temperature [',TEMP,
     &                     '] outside valid range: 0 < T < 100'
            WRITE(ICOUT,*) ' CNDWAT: heat of condensation',
     &                     ' evaluated for T = ',VTEMP
         ENDIF
      ENDIF

      CNDWAT1=1000.*(3158.-2.43*(VTEMP+273.))

      RETURN
      END

C ********************* LESQRA *********************
C Subroutine LESQRA accepts IVAL (4<=IVAL) number of pairs 
C of coordinates and returns coefficients of a cubic polynomial 
C that best fits these
C Solution method is least squares curve fitting

      SUBROUTINE LESQRA(IVAL,XVAL,YVAL,SINGUL,RIHAS)

      PARAMETER(IDEG=4)
      DOUBLE PRECISION ELEMENTA,ELEMENTB,ELEMENTRA,ELEMENTRB,EPSIL
      DOUBLE PRECISION RIHAS(IDEG),DLIN(IDEG,IDEG),TRHS(IDEG)
      DOUBLE PRECISION XVAL(IVAL),YVAL(IVAL),ABSPIV
      LOGICAL SINGUL
      PARAMETER (EPSIL = 1D-20)

      ELEMENTA=0D0
      ELEMENTB=0D0

C Set up matrix to be solved to give least squares curve fit coefficients
        DO 177 I=1,IDEG
          DO 277 J=1,IDEG
            DO 377 K=1,IVAL
              ELEMENTRA=(XVAL(K)**(I+J-2))
              ELEMENTRB=(YVAL(K)*XVAL(K)**(J-1))
              ELEMENTA=ELEMENTA+ELEMENTRA
              ELEMENTB=ELEMENTB+ELEMENTRB
 377        CONTINUE
            DLIN(I,J)=ELEMENTA
            TRHS(J)=ELEMENTB
            ELEMENTA=0.0D0
            ELEMENTB=0.0D0
 277      CONTINUE
 177    CONTINUE
 
C Check for singularity 
      SINGUL = .FALSE.
      DO 50 I = 1, IDEG

C Locate pivot element
        ABSPIV = ABS(DLIN(I,I))
        DO 10 K = I + 1, IDEG
          IF (ABS(DLIN(K,I)) .GT. ABSPIV) THEN
            ABSPIV = ABS(DLIN(K,I))
          END IF
 10     CONTINUE

C Check if matrix is (nearly) singular
        IF (ABSPIV .LT. EPSIL) THEN
          SINGUL = .TRUE.
          RETURN
        END IF
 50   CONTINUE

C Call solver if not singular
      IF(.NOT.SINGUL) CALL JSLVD2(DLIN,IDEG,IDEG,RIHAS,TRHS)

      RETURN
      END

C ******************* JSLVD2
C This subroutine is a copy of JSOLVD in mfmach.F copied here so as not
C to modify esruprj/Makefile to include mfmach.F and mfutil.F
C JSLVD2 solves the matrix equation {A} {X} = {B} based
C on Gaussian elimination with backsubstitution and no pivoting.
C An efficient matrix technique is used to forward reduce the
C N*N matrix A (physical size NP*NP) halfway, to a matrix whose
C components on the diagonal and above remain nontrivial.
C The solution vector X (size N) is then generated through
C backsubstitution of the known right hand side vector B (size N)

      SUBROUTINE JSLVD2(A,N,NP,X,B)

      DOUBLE PRECISION A(NP,NP),X(N),B(N)
      DOUBLE PRECISION AB,AC,AD

C Forward reduce matrix A.
      DO 10 I=2,N
        A(1,I)=A(1,I)/A(1,1)
   10 continue
      DO 50 K=2,N
        K1=K-1
        DO 20 IK=K,N
        DO 20 J=1,K1
          AB=A(IK,J)
          AC=A(J,K)
          AD=AB*AC
          A(IK,K)=A(IK,K)-AD
   20   continue
        J1=K+1
        DO 30 J=J1,N
          DO 40 MJ=1,K1
            AB=A(K,MJ)
            AC=A(MJ,J)
            AD=AB*AC
            A(K,J)=A(K,J)-AD
   40     continue
          A(K,J)=A(K,J)/A(K,K)
   30   continue
   50 CONTINUE

C Conduct backward substitution to establish solution vextor X.
      B(1)=B(1)/A(1,1)
      DO 70 I=2,N
        K1=I-1
        DO 60 J=1,K1
          AB=B(J)
          AC=A(I,J)
          AD=AB*AC
          B(I)=B(I)-AD
   60   continue
        B(I)=B(I)/A(I,I)
   70 continue
      X(N)=B(N)
      N1=N-1
      DO 90 JJ=1,N1
        I=N-JJ
        IK=I+1
        DO 80 J=IK,N
          AB=A(I,J)
          AC=B(J)
          AD=AB*AC
          B(I)=B(I)-AD
   80   continue
        X(I)=B(I)
   90 continue

      RETURN
      END
