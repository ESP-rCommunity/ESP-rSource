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

C This file contains the following functions and subroutines
C in support of the simulation by primitive parts
C Subrountines :	SATMOIST
C			SURFWET
C			TRNSDLAY
C			TIMCONST
C			CVCOEFF1
C			CVCOEFF2
C			SATTEMP
C			SPECHEAT
C Functions :		DNSTY
C			LTHT
C			SPHT

C *********************** SUBROUTINE SATMOIST ***********************
C Subroutine to determine the saturated moisture content for temp range
C 0 to 200 deg C; SOURCE: ASHRAE HANDBOOK (FUNDAMENTAL)

      SUBROUTINE SATMOIST(Ts,Gs)

      T=Ts+273.15

      C8=-5800.2206
      C9=1.3914993
      C10=-0.04860239
      C11=0.41764768E-4
      C12=-0.14452093E-7
      C13=6.5459673

      F=C8/T+C9+C10*T+C11*(T**2)+C12*(T**3)+C13*LOG(T)
      Pws=EXP(F)
      Gs=0.62198*Pws/(101325.-Pws)

      RETURN
      END


C *********************** SUBROUTINE SURFWET ***********************
C Detect interior surface conditions: DRY or WET; formulae from 
C ASHRAE Handbook:Fundamental for 0 to 70 & -60 to 0 deg.C respectively

      Subroutine SURFWET(Ga,Ts,CdP,Surf)
      logical close

       If (Ga.le.0.) go to 10

       Pw=101325.*Ga/(0.62198+Ga)
       Td=-35.957-1.8726*LOG(Pw)+1.1689*(LOG(Pw)**2)

       IF(Td.LE.0.0) THEN
       Td=-60.45-7.0322*LOG(Pw)+0.3700*(LOG(Pw)**2)
       END IF

C if Td outside range, report (to be done later!)
 
       call eclose(CdP,0.00,0.0001,close)
       IF (Td.LE.Ts.AND.close) THEN
         Surf=0.
       ELSE
         Surf=1.
       END IF

   10 RETURN
      END

C *********************** SUBROUTINE TRNSDLAY ***********************

      Subroutine TRNSDLAY(IPCOMP,DELAY,TP0,TP1,Ts,N,DY,Cf1,Cfs,
     &                    TF0,TFm,TF1)
      INCLUDE 'plant.h'

      COMMON/Pctime/TIMSEC
      COMMON/DELAY/TEMPF(MPCOM,MDLY),YPOSF(MPCOM,MDLY),NSECTF(MPCOM),
     &             TEMPP(MPCOM,MDLY),YPOSP(MPCOM,MDLY),NSECTP(MPCOM)

      PARAMETER (SMALL=1.0E-20)
      DIMENSION YP(MDLY),TP(MDLY),Y(MDLY),T(MDLY),TF(MDLY),TDY(MDLY)
      logical close

C Common for primitive parts transport delay flag.
      COMMON/PP_DELAY/bDELAY_FLAG
      logical bDELAY_FLAG
C Set bDELAY to true, so that PP historical data will be transported 
C by MZNASS / plt_TimeRowManipulate
      bDELAY_FLAG = .TRUE.


C RESET DELAY COMMON BLOCK VARIABLES IF DELAY IS NO LONGER REQUIRED FOR
C THE FUTURE TIME STEP
      call eclose(DELAY,0.00,0.0001,close)
      IF (close) THEN
	NSECTF(IPCOMP)=1
	YPOSF(IPCOMP,1)=1.
	TEMPF(IPCOMP,1)=TP1
        GO TO 80
      ENDIF

C RESTORE DELAY COMMON BLOCK VARIABLES
        NP=NSECTP(IPCOMP)
      DO 10 J=1,NP
	YP(J)=YPOSP(IPCOMP,J)
	TP(J)=TEMPP(IPCOMP,J)
C Initialize the array data at start-up
          call eclose(TP(J),0.00,0.0001,close)
          IF (close) THEN
            TP(J)=TP0+(TP1-TP0)*FLOAT(J)/FLOAT(NP)
          ENDIF
   10 CONTINUE

C DETERMINE NUMBER OF SECTIONS FOR CURRENT TIME STEP
	IF (N.GT.MDLY) THEN
	N=MDLY
	ENDIF
C IF EXCEEDED, WARNING IS TO BE GIVEN (TO BE DONE LATER!)

C RENEW THE NORMALISED SECTION POSITION PARAMETERS AND PRESENT TEMPERATURE 
C VALUES AT FUTURE TIME STEP NOTATION
   	DO 20 I=1,N
        Y(I)=FLOAT(I)/FLOAT(N)
        IF (N.EQ.NP) THEN
   	 T(I)=TP(I)
         GO TO 20
        ENDIF
         DO 30 J=1,NP
         IF (YP(J).GE.Y(I)) THEN
            IF (J.EQ.1) THEN
            T(I)=TP0+(TP(1)-TP0)*Y(I)/YP(J)
            ELSE
            T(I)=TP(J-1)+(TP(J)-TP(J-1))*(Y(I)-YP(J-1))/(YP(J)-YP(J-1))
            ENDIF
         GO TO 20
         ENDIF
   30    CONTINUE
   20   CONTINUE

C DETERMINE PRESENT TIME STEP TEMPERATURES OF FLUID ELEMENTS AT POSITIONS
C [Y(I)-DY] (WHICH WILL REACH Y(I)'S AT FUTURE TIME STEP)

        TDY(1)=T(1)-(T(1)-TP0)*DY*FLOAT(N)
        DO 40 I=2,N
	  TDY(I)=T(I)-(T(I)-T(I-1))*DY*FLOAT(N)
   40	CONTINUE     

C DETERMINE FUTURE TIME STEP TEMPEATURES
   	DO 50 I=1,N
        TF(I)=(Cf1*TDY(I)+Cfs*DY*Ts)/(Cf1+Cfs*DY)
   50	CONTINUE

C DETERMINE outlet and MEAN FLUID TEMPERATURE
        TF1=TF(N)
	SUM=TF0+TF1
        DO 60 I=1,N-1
        SUM=SUM+2.*TF(I)
   60   CONTINUE
	TFm=0.5*SUM/FLOAT(N)

C UPDATE DELAY COMMON BLOCK VARIABLES
        NSECTF(IPCOMP)=N
      DO 70 I=1,N
	YPOSF(IPCOMP,I)=Y(I)
	TEMPF(IPCOMP,I)=TF(I)
  70  CONTINUE

 80   RETURN
      END

C *********************** SUBROUTINE TIMCONST ***********************

C Subroutines to calculate cascade time constant value
C iteration performed by Newton Raphson method; solution stopped 
C within 5% accuracy

      Subroutine TIMCONST(TC1,TC2,TC)

      T=AMAX1(TC1,TC2)
      DO 10 I=1,100
      FT=TC1*EXP(-T/TC1)/(TC1-TC2)+TC2*EXP(-T/TC2)/(TC2-TC1)-EXP(-1.)
      DFT=(EXP(-T/TC1)-EXP(-T/TC2))/(TC2-TC1)
      TC=T-FT/DFT

      IF(ABS((TC-T)/TC).LT.0.05) GO TO 20
      T=TC

10    CONTINUE

C     IF (I.EQ.100) report!  (to be done later)    

20    RETURN
      END

C *********************** CVCOEFF ***********************

C Subroutines to calculate the convective heat transfer coefficients
C CVCOEF1 for PIPED FLOW
C CVCOEF2 for CROSS-FLOW OVER CYLINDER
C
C Firstly for piped flow
C *********************** SUBROUTINE CVCOEFF1 ***********************

      Subroutine CVCOEFF1(N,Tf,u,D,h)

C CHECK THE TYPE OF CONVECTIVE HEAT TRANSFER COEFFICIENTS REQUIRED
C N = 1 AIR
C   = 3 WATER

C Assign properties values

      IF (N.EQ.1) THEN
	RHO=RHOFLD(1,Tf)
        Rk=0.02624+7.58E-5*(Tf-27.)
        Rmu=(1.8462+4.576E-3*(Tf-27.))*1.E-5
	Pr=0.708-2.2E-4*(Tf-27.)
      ELSEIF (N.EQ.3) THEN
	RHO=RHOFLD(3,Tf)
        Rk=-8.23171E-6*Tf**2+1.96785E-3*Tf+0.5665745
        Rmu=1.825372E-7*Tf**2-3.1195E-5*Tf+0.001639325
	Pr=1.480565E-3*Tf**2-0.245234*Tf+11.99837
      ENDIF

	Re=RHO*D*u/Rmu

C for turbulent flow required to check whether it is cooling or heating	
	IF (Re.GT.2300.) THEN
	h=0.023*(Rk/D)*Re**0.8*Pr**(1./3.)

C for laminar flow assuming constant wall temperature
	ELSE
	h=3.66*Rk/D
	ENDIF

 	RETURN
	END

C *********************** SUBROUTINE CVCOEFF2 ***********************
      Subroutine CVCOEFF2(N,Tf,Tw,u,D,h)
C computation based on Echert and Drake (ref. Heat Transfer by Holman)
C Note for gases, (Prf/Prw) may be dropped, Tf is the film temp;
C For liquids, Tf is at free-stream temperature
C N = 1 AIR
C   = 3 WATER

C Assign properties values

      IF (N.EQ.1) THEN
	RHO=RHOFLD(1,Tf)
        Rk=0.02624+7.58E-5*(Tf-27.)
        Rmu=(1.8462+4.576E-3*(Tf-27.))*1.E-5
	Pr=0.708-2.2E-4*(Tf-27.)
	Prw=0.708-2.2E-4*(Tw-27.)


      ELSEIF (N.EQ.3) THEN
	RHO=RHOFLD(3,Tf)
        Rk=-8.23171E-6*Tf**2+1.96785E-3*Tf+0.5665745
        Rmu=1.825372E-7*Tf**2-3.1195E-5*Tf+0.001639325
	Pr=1.480565E-3*Tf**2-0.245234*Tf+0.1199837
	Prw=1.480565E-3*Tw**2-0.245234*Tw+0.1199837

      ENDIF
	Re=RHO*D*u/Rmu
	Prf=Pr

	IF (Re.LT.1000.) THEN
	h=(Rk/D)*(0.43+0.5*Re**0.5*Pr**0.38*(Prf/Prw)**0.25)

	ELSEIF (Re.GE.1000.) THEN
	h=(Rk/D)*0.25*Re**0.6*Pr**0.38*(Prf/Prw)**0.25

	ENDIF

C give warning
C	IF ((Re.LT.1..or.Re.GT.20000.).and.(u.ne.0.)) THEN give warning


 	RETURN
	END


C *********************** DNSTY ***********************

C DNSTY is a real function which returns the fluid density at known 
C fluid temperature (C) TEMP
C of dry air (IFLD=1), sat steam (IFLD=2), or sat water (IFLD=3)
C for dry air    : 100 to 1000 K   (correlation coeff = 0.99880258)
C for sat steam  : 273.15 to 460 K (correlation coeff = 0.99838359)
C for sat water  : 273.15 to 460 K (correlation coeff = 0.99996780)

	FUNCTION DNSTY(IFLD,TEMP)

	T=TEMP+273.15

      IF (IFLD.EQ.1) THEN
	DNSTY=-8.05802E-14*T**5+2.547453E-10*T**4-3.10803E-7*T**3
     &	     +1.840738E-4*T**2-5.45393E-2*T+7.440235	

      ELSEIF (IFLD.EQ.2) THEN
	Vg=-1.97380E-8*T**5+3.783208E-5*T**4-2.88792E-2*T**3
     &	     +1.097570E1*T**2-2.07717E3*T+1.566384E5	
	DNSTY=1./Vg

      ELSEIF (IFLD.EQ.3) THEN
	Vf=-2.96053E-10*T**5+5.845781E-7*T**4-4.58057E-4*T**3
     &	     +1.812698E-1*T**2-3.59515E1*T+3.826453E3	
	DNSTY=1000./Vf

      ENDIF

 	RETURN
	END

C ********************** LTHT *************************
C LTHT is a real function which returns heat of vaporisation (J/kg) of
C steam/water (IFLD=2 or 3) as a function of (TEMP) saturation temperature
C (C) for water  : 273.15 to 460 K (correlation coeff = 0.99999752)

	FUNCTION LTHT(IFLD,TEMP)
        real LTHT

	T=TEMP+273.15

	IF(IFLD.EQ.2.OR.IFLD.EQ.3) THEN
	LTHT=-6.71719E-11*T**5+1.334845E-7*T**4-1.20261E-4*T**3
     &	     +5.428895E-2*T**2-1.41674E1*T+4.130857E3	

C new fluid type can be added here
	ENDIF

	LTHT=1000.*LTHT

	RETURN
	END

C ********************** SPHT *************************
C SPHT is a real function which returns the specific heat (J/kgK)
C of dry air (IFLD=1), saturated steam (IFLD=2), or saturated water (IFLD=3)
C as a function of (TEMP) the fluid temperature (C) at elevated temps.
C for dry air: 100 to 1000 K   (correlation coeff = 0.99954829)
C for steam  : 273.15 to 460 K (correlation coeff = 0.99999449)
C for water  : 273.15 to 460 K (correlation coeff = 0.99986847)

	FUNCTION SPHT(IFLD,TEMP)

	T=TEMP+273.15

	IF(IFLD.EQ.1) THEN
	  IF(T.GE.1000.) T=1000.
C	  Give warning (to be done later)
	SPHT=-3.15610E-16*T**5+1.177003E-12*T**4-1.96187E-9*T**3
     &	     +1.772826E-6*T**2-5.99718E-4*T+1.069159	

	ELSEIF(IFLD.EQ.2) THEN
	  IF(T.GE.460.) T=460.
C	  Give warning (to be done later)
	SPHT=1.924825E-12*T**5-3.37121E-9*T**4+2.442447E-6*T**3
     &       -8.87538E-4*T**2+1.600598E-1*T-9.58369

	ELSEIF(IFLD.EQ.3) THEN
	  IF(T.GE.460.) T=460.
C	  Give warning (to be done later)
	SPHT=-6.07571E-12*T**5+1.168151E-8*T**4-8.88692E-6*T**3
     &       +3.356291E-3*T**2-6.30218E-1*T+5.126842E1

	ENDIF

	SPHT=1000.*SPHT

	RETURN
	END

C ************************ SATTEMP ***********************
C This is a subroutine to determine the saturated temperature of
C water/steam in deg.C corresponging to a given pressure in bar
C at the pressure range of 1 to 15 bars (99.6 to 198.3 deg C)
C from Steam Table
       SUBROUTINE SATTEMP(Psat,Tsat)

	DIMENSION T(20),P(20)

	P(1)=1.0
	P(2)=1.5
	P(3)=2.0
	P(4)=2.5
	P(5)=3.0
	P(6)=3.5
	P(7)=4.0
	P(8)=4.5
	P(9)=5.0
	P(10)=5.5
	P(11)=6.0
	P(12)=7.0
	P(13)=8.0
	P(14)=9.0
	P(15)=10.0
	P(16)=11.0
	P(17)=12.0
	P(18)=13.0
	P(19)=14.0
	P(20)=15.0
 
	T(1)=99.6
	T(2)=111.4
	T(3)=120.2
	T(4)=127.4
	T(5)=133.5	
	T(6)=138.9
	T(7)=143.6
	T(8)=147.9
	T(9)=151.8
	T(10)=155.5
	T(11)=158.8
	T(12)=165.0	
	T(13)=170.4
	T(14)=175.4
	T(15)=179.9	
	T(16)=184.1
	T(17)=188.0
	T(18)=191.6
	T(19)=195.0
	T(20)=198.3

	DO 10 I=1,20
	IF(Psat.LE.P(I+1)) THEN
	Tsat=T(I)+(T(I+1)-T(I))*(Psat-P(I))/(P(I+1)-P(I))
	GO TO 20
	ENDIF

 10	CONTINUE

 20	RETURN
	END


C ************************ SPECHEAT ***********************
C This is a subroutine to determine the specific heat at constant pressure
C of water/steam in J/kg.K
C at saturated temperature range of 100 to 198 deg.C

       SUBROUTINE SPECHEAT(Temp,Tsat,CPw,CPv,Hfg)

      	PARAMETER (SMALL=1.0E-20)

	DIMENSION T(20),Hf(20),Hg(20),CPf(20),CPg(20)

	T(1)=99.6
	T(2)=111.4
	T(3)=120.2
	T(4)=127.4
	T(5)=133.5	
	T(6)=138.9
	T(7)=143.6
	T(8)=147.9
	T(9)=151.8
	T(10)=155.5
	T(11)=158.8
	T(12)=165.0	
	T(13)=170.4
	T(14)=175.4
	T(15)=179.9	
	T(16)=184.1
	T(17)=188.0
	T(18)=191.6
	T(19)=195.0
	T(20)=198.3

	Hf(1)=417.
	Hf(2)=467.
	Hf(3)=505.
	Hf(4)=535.
	Hf(5)=561.
	Hf(6)=584.
	Hf(7)=605.	
	Hf(8)=623.	
	Hf(9)=640.
	Hf(10)=656.
	Hf(11)=670.
	Hf(12)=697.
	Hf(13)=721.
	Hf(14)=743.
	Hf(15)=763.
	Hf(16)=781.
	Hf(17)=798.
	Hf(18)=815.
	Hf(19)=830.
	Hf(20)=845.

	Hg(1)=2675.
	Hg(2)=2693.
	Hg(3)=2707.
	Hg(4)=2717.
	Hg(5)=2725.
	Hg(6)=2732.
	Hg(7)=2739.
	Hg(8)=2744.
	Hg(9)=2749.
	Hg(10)=2753.
	Hg(11)=2757.
	Hg(12)=2764.
	Hg(13)=2769.
	Hg(14)=2774.
	Hg(15)=2778.
	Hg(16)=2781.
	Hg(17)=2784.
	Hg(18)=2787.
	Hg(19)=2790.
	Hg(20)=2792.

	DO 10 I=1,20
	IF(Tsat.LE.T(I+1)) THEN
	Hw=Hf(I)+(Hf(I+1)-Hf(I))*(Tsat-T(I))/(T(I+1)-T(I))
	Hv=Hg(I)+(Hg(I+1)-Hg(I))*(Tsat-T(I))/(T(I+1)-T(I))
	Hfg=1000.*(Hv-Hw)
	GO TO 20
	ENDIF
 10	CONTINUE

 20	Tk=Tsat+273.15
 
	T(1)=373.15
	T(2)=375.
	T(3)=380.
	T(4)=385.
	T(5)=390.
	T(6)=400.
	T(7)=410.
	T(8)=420.
	T(9)=430.
	T(10)=440.
	T(11)=450.
	T(12)=460.
	T(13)=470.
	T(14)=480.
	T(15)=490.
	T(16)=500.

	CPf(1)=4.217
	CPf(2)=4.220
	CPf(3)=4.226
	CPf(4)=4.232
	CPf(5)=4.239
	CPf(6)=4.256
	CPf(7)=4.278
	CPf(8)=4.302
	CPf(9)=4.331
	CPf(10)=4.36
	CPf(11)=4.40
	CPf(12)=4.44
	CPf(13)=4.48
	CPf(14)=4.53
	CPf(15)=4.59
	CPf(16)=4.66

	CPg(1)=2.029
	CPg(2)=2.036
	CPg(3)=2.057
	CPg(4)=2.080
	CPg(5)=2.104
	CPg(6)=2.158
	CPg(7)=2.221
	CPg(8)=2.291
	CPg(9)=2.369
	CPg(10)=2.46
	CPg(11)=2.56
	CPg(12)=2.68
	CPg(13)=2.79
	CPg(14)=2.94
	CPg(15)=3.10
	CPg(16)=3.27

	DO 25 I=1,16
	IF(Tk.LE.T(I+1)) THEN
	SCPf=CPf(I)+(CPf(I+1)-CPf(I))*(Tk-T(I))/(T(I+1)-T(I))
	SCPg=CPg(I)+(CPg(I+1)-CPg(I))*(Tk-T(I))/(T(I+1)-T(I))
	GO TO 30
	ENDIF
 25	CONTINUE

 30	IF (ABS(Temp-Tsat).LT.SMALL) THEN
C fluid is wet steam
		Cpw=1000.*Hw/Tsat
		Cpv=1000.*Hv/Tsat
	ELSE IF (Temp.LT.Tsat) then
C sub-cooled
		IF (Temp.GE.100.) THEN 
		Cpw=1000.*(Hw-SCpf*(Tsat-Temp))/Temp
		ELSE
		Cpw=SHTFLD(3,TEMP)
		ENDIF
	ELSE IF (Temp.GT.Tsat) then
C superheated
		Cpv=1000.*(Hv+SCpg*(Temp-Tsat))/Temp
	ENDIF

 	RETURN
	END
