C suple.f contains routines that are called by CPVELE to calculate
C pressure coefficients on the leewards orientated facade of the 
C building:

C CPBOLL 
C CPDENL
C CPRELL
C CPFARL
C CPSARL
C CPHORL

       SUBROUTINE CPBOLL (ZH,VeEXP,COBOL,*,KEY)
c************************************************************************
c Purpose: this routine calculates the Correction Factor for the 
c          reference CP(zh) value, in relation to the Boundary Layer, 
c          characteristic of the site wich the Building is located in.
c
c Module : # 4.3.1.2.1, TG#3, mxg/March 1,1989
c Changes: January 29, 1990 (built CPBOLL from CPBOUL )
c        : May 7,1993   (extended upper VeEXP limit from 0.33 to 0.45)  

c Limits : 0.10 _< VeEXP _< 0.45
c          0.1  _< ZH  _<  0.9  (if VeEXP =/= 0.22)
c
c Pass parameters:
c
c IO # Name    unit         description                  ref.value
c
c I  1 ZH      (-)   Relative vertical position of      (see data A)
c                    a facade element
c I  2 VeEXP   (-)   Wind Velocity Profile Exponent        (.22)
c O  3 COBOL   (-)   Correction Factor for CP(zh)           (1)
c O  4 RETURN1 (-)   error return
c O  5 KEY     (-)   error key; it is set to 1 if error is found.
c
c ERROR RETURN  IF   0.10 > VeEXP > 0.45
c                    0.1  > ZH  >  0.9  (if VeExp =/= 0.22)     
c
c example:
c CALL CPBOLL ( .6, .28, .770272, 999, 0)
c***********************************************************************
        COMMON /ERROR/ERF
        common /silmode/ issilent
        logical issilent
        INTEGER erf
        INTEGER KEY

        parameter (Nmax=5,Lmax=4)
        REAL ZH,VeEXP,polB(Nmax,Lmax) 

        REAL A(5),CORP1,CORP2,COBOL
        INTEGER N

c reference values for ZH
        DATA A/.1,.3,.5,.7,.9/

c coefficients for a 2nd order polinomial equation yelding
c COBOL as fitting function of VeEXP
        data polB/ 0., 0., 0., 0., 0.,
     & -14.368685, -13.490491, -8.775919, -4.662405, 2.382908,
     & +4.520431, +4.101437, +1.322245, -0.929782, -4.837467,
     & +0.667639, +0.706052, +1.088822, +1.395398, +1.940878/

c error return
        IF (VeEXP.LT..10.OR.VeEXP.GT..45) THEN
           if (.not.issilent) then
              WRITE (ERF,*)  'VeEXP out of range (.10-.45)'
           end if
           KEY = 1
           RETURN 1
        ENDIF

        if (VeEXP.EQ..22) go to 100

c check the position of ZH in relation to the reference
c values (A) and calculate the pointer number to search the 
c proper coefficients (polB) for the polinomial equation 
        K=0
        N=0
        DO 10 I=1,5
          IF (ZH.NE.A(I)) GO TO 10
          K=I
10      CONTINUE

        if (K.NE.0) N=K
 
        IF (N.NE.0) GO TO 200

c when ZH is different from a reference value
        CALL SEARCH (A,ZH,5,K1) 
        N=K1

        CALL CORPAN (VeEXP,polB,Nmax,Lmax,N,CORP1)
        CALL CORPAN (VeEXP,polB,Nmax,Lmax,N+1,CORP2)
        CALL LININT (ZH,A(N),A(N+1),CORP1,CORP2,COBOL)
        GO TO 999

c when ZH is equal to a reference value
200     CALL CORPAN (VeEXP,polB,Nmax,Lmax,N,COBOL)
        GO TO 999

c when VeEXP= .22
100     COBOL = 1.0       

999     CONTINUE
        RETURN
        END

        SUBROUTINE CPDENL (ZH,PAD,COPAD,*,KEY)
c************************************************************************
c Purpose: this routine calculates the Correction Factor for the
c          Reference CP(zh), in relation to the Plan Area Density 
c          characteristic of the Building's surroundings.
c
c Module : # 4.3.1.2.2, TG#3, mxg/March 1,1989
c Changes: January 29, 1990 (built CPDENL from CPDENS)
c Limits : 0 < PAD < 50.
c          0.07 < ZH < 0.93  (if PAD =/= 0)
c
c Pass parameters:
c
c IO # Name    unit         description                        ref.value
c
c I  1 ZH      (-)   Relative vertical position of a       (.5,.7,.8,.9,.93)
c                    facade element
c I  2 PAD     (-)   Plan Area Density (percentage ratio of the      (0)
c                    Plan Area of a Building to the Area of the 
c                    interspace among buildings, in a Normal Pattern
c                    Surrounding Layout (within a radius of about 
c                    200 m around the Building under consideration)
c O  3 COPAD   (-)   Correction Factor for CP(zh).                   (1)
c O  4 RETURN1 (-)   error return
c O  5 KEY     (-)   error key; it is set to 1 if error is found.
c
c ERROR RETURN  IF   0 > PAD > 50
c                    .07 > ZH > .93  (if PAD =/= 0) 
c example:
c CALL CPDENL (.6, 7.5, .69072, 999, 0)
c***********************************************************************
        COMMON /ERROR/ERF
        common /silmode/ issilent
        logical issilent
        INTEGER KEY
        INTEGER erf

        parameter (Nmax=6,Lmax=6)
        REAL ZH,PAD,polD(Nmax,Lmax),A(6)
        INTEGER K

        REAL COPAD

c reference values of ZH
        DATA A/.07, .2, .4, .6, .8, .93/

c coefficients for a 5th order polinomial equation yelding
c COPAD as fitting function of PAD
        data polD/ 9.118209e-08,5.934754e-08,5.052791e-08,5.595805e-08,
     & 5.553558e-08,6.211419e-08,-1.050363e-05,-6.708652e-06,
     & -5.537346e-06, -6.121612e-06, -5.931215e-06, -6.759794e-06,
     & +3.932533e-04, +2.340744e-04, +1.722449e-04, +1.8897e-04, 
     & +1.719758e-04, +2.024378e-04,-4.734698e-03, -1.943067e-03, 
     & -3.926684e-04, -3.177597e-04, +3.013991e-04, +1.182029e-04,
     & -0.015304, -0.031483, -0.046517, -0.051446, -0.059971,-0.065764,
     & +1.047295, +1.043295, +1.034663, +1.032759, +1.037969,+1.033975/
        
c error return
        IF (PAD.LT.0..OR.PAD.GT.50.) THEN
           if (.not.issilent) then
              WRITE (ERF,*)  'PAD out of range (0-50)'
           end if
           KEY=1
           RETURN 1
        ENDIF

        if (PAD.eq.0) go to 100

c check the position of ZH in relation to the reference
c values (A) and calculate the pointer number to search the 
c proper coefficients (polB) for the polinomial equation 
        K=0
        N=0
        DO 10 I=1,6
          IF (ZH.NE.A(I)) GO TO 10
          K=I
10      CONTINUE

        if (K.NE.0) N=K

        IF (N.NE.0) GO TO 200

c when ZH is different from a reference value
        CALL SEARCH (A,ZH,6,K1)
        N=K1

        CALL CORPAN (PAD,polD,Nmax,Lmax,N,CORP1)
        CALL CORPAN (PAD,polD,Nmax,Lmax,N+1,CORP2)
        CALL LININT (ZH,A(N),A(N+1),CORP1,CORP2,COPAD)
        GO TO 999

c when ZH is equal to a reference value
200     CALL CORPAN (PAD,polD,Nmax,Lmax,N,COPAD)
        GO TO 999

c when PAD = 0
100     COPAD = 1.0

999     CONTINUE
        RETURN
        END


        SUBROUTINE CPRELL (ZH,PAD,RbH,COREL,*,KEY)
c************************************************************************
c Purpose: this routine calculates the Correction Factor for the
c          reference CP(zh), in relation to the Relative Height of the
c          Building at a given PAD.

c Module : # 4.3.1.2.3, TG#3, mxg/March 1,1989
c Changes: June 28, 1989 (checked new limit for PAD)
c          January 29, 1990 (built CORELL from CORELH)
c          May 7,1993 (changed RbH limits from .5 _< RbH _< 4. to  0.< RbH _< 8.)
c Limits : 0. < RbH _< 8.0
c          .07 _< ZH _< .93, 0 _< PAD _< 25 (if RbH =/= 1)
c new limits: 0 _< PAD _< 12.5 (if RbH =/= 1)
c
c Pass parameters:
c
c IO # Name    unit         description                         ref.value
c
c I  1 ZH      (-)   Relative vertical position of             (see data A1)
c                    a facade element
c I  2 PAD     (-)   Plan Area Density                         (see data A2)
c I  3 RbH     (-)   Ratio of height of the Building to             (1)
c                    the average height of surroundings
c                    (block-shaped buildings, normal layout 
c                    pattern)
c O  4 COREL   (-)   Correction Factor for CP(zh)                   (1) 
c O  5 RETURN1 (-)   error return
c O  6 KEY     (-)   error key; it is set to 1 if error is found.
c
c ERROR RETURN  IF   0._> RbH > 8.0  
c                    PAD > 12.5 (if RbH =/= 1)
c                    .07 > ZH  > .93  (if RbH =/= 1)
c example:
c CALL CPRELL (.6, 7.5, 2., 1.18508, 999, 0)
c***********************************************************************
        COMMON /ERROR/ERF
        INTEGER erf
        INTEGER KEY

        parameter (Nmax=24,Lmax=3) 
        REAL ZH,PAD,RbH,polR(Nmax,Lmax)

        REAL A1(6),A2(4),CORP1,CORP2,COREL1,COREL2
        INTEGER K,K1,L,L1,N

        REAL COREL

c reference values for ZH
        DATA A1/.07,.2,.4,.6,.8,.93/

c reference values for PAD
        DATA A2/0.,5.,6.25,12.5/

c coefficients for 1st and 2nd order polinomial equations
c yelding COREL as fitting function of RbH
       data polR/0., 0., 0., 0., 0., 0., 0.123639, 0.080103,-0.043739,
     & 0.054539, 0.100427, 0.175853, -0.069086, 0.029377, 0.066082, 
     & 0.145046, -0.036376, 0.011009,-1.58012e-03,0.09395,2.138076e-03, 
     & 0.03126,0.102993,0.202243,0.547959,0.625743,0.859533, 1.710552,
     & 0.473757, 0.636732, +0.432008, +1.471191, +0.599345, +0.299349, 
     & +0.35117, +0.568029, +0.793503, +0.402683, +0.524015, +0.567979, 
     & +0.781825, +0.55164, +1.127839, +1.114736, +0.655048, +0.526521, 
     & +0.946754, +1.119405,+0.465538, +0.308268, +0.107587, -0.681624, 
     & +0.527487, +0.294108, +0.44064, -0.547645, +0.427938, +0.645489, 
     & +0.483096, +0.223168, +0.287883, +0.594877, +0.376383, +0.264523, 
     & +0.258777, +0.435343, -0.084281, -0.111437, +0.38064, +0.418668, 
     & -0.122071, -0.353569/

        IF (RbH.EQ.1.) GO TO 100

c check the position of ZH and PAD in relation to the reference
c values (A1,A2) and calculate the pointer number to search the 
c proper coefficients for the polinomial equation (polR)  
        K=0
        L=0
        N=0
        DO 10 I=1,6
          IF (ZH.NE.A1(I)) GO TO 10
          K=I
10      CONTINUE

        DO 20 J=1,4
          IF (PAD.NE.A2(J)) GO TO 20
          L=J
20      CONTINUE

        IF (K.NE.0.AND.L.NE.0) N=(K-1)*4+L
        IF (N.NE.0) GO TO 200
        IF (K.NE.0) GO TO 300
        IF (L.NE.0) GO TO 400

c when ZH and PAD are both different from  reference values
        CALL SEARCH (A1,ZH,6,K1)
        CALL SEARCH (A2,PAD,4,L1)

        N=(K1-1)*4+L1

        CALL CORPAN (RbH,polR,Nmax,Lmax,N,CORP1)
        CALL CORPAN (RbH,polR,Nmax,Lmax,N+1,CORP2)
        CALL LININT (PAD,A2(L1),A2(L1+1),CORP1,CORP2,COREL1)

        N=K1*4+L1

        CALL CORPAN (RbH,polR,Nmax,Lmax,N,CORP1)
        CALL CORPAN (RbH,polR,Nmax,Lmax,N+1,CORP2)
        CALL LININT (PAD,A2(L1),A2(L1+1),CORP1,CORP2,COREL2)
        CALL LININT (ZH,A1(K1),A1(K1+1),COREL1,COREL2,COREL)
        GO TO 999

c when only ZH is different from reference value
400     CALL SEARCH (A1,ZH,6,K1)

        N=(K1-1)*4+L

        CALL CORPAN (RbH,polR,Nmax,Lmax,N,CORP1)
        CALL CORPAN (RbH,polR,Nmax,Lmax,N+4,CORP2)
        CALL LININT (ZH,A1(K1),A1(K1+1),CORP1,CORP2,COREL)
        GO TO 999

c when only PAD is different from reference value 
300     CALL SEARCH (A2,PAD,4,L1)       

        N=(K-1)*4+L1

        CALL CORPAN (RbH,polR,Nmax,Lmax,N,CORP1)
        CALL CORPAN (RbH,polR,Nmax,Lmax,N+1,CORP2)
        CALL LININT (PAD,A2(L1),A2(L1+1),CORP1,CORP2,COREL)
        GO TO 999

c when ZH and PAD are equal to reference values
200     CALL CORPAN (RbH,polR,Nmax,Lmax,N,COREL)
        GO TO 999

c when RbH = 1
100     COREL = 1.0       

999     CONTINUE

        key=key
        RETURN
        END


        SUBROUTINE CPFARL (ZH,FAR,PAD,COFAR,*,KEY)
c************************************************************************
c Purpose: this routine calculates the Correction Factor for the
c          reference CP(zh), as function of the Frontal Aspect Ratio of 
c          the bldg. in relation to the considered wall.
c
c Module : # 4.3.1.2.4, TG#3, mxg/March 1,1989
c Changes: October 15, 1989 (written subroutines for FAR =/= 1)
c          January 29, 1990 (built CPFARL from CPASPF)
c          May 7,1993 (changed FAR limits from .5 _< FAR _< 4. to  0.< FAR _< 8.)
c Limits : 0.< FAR _< 8.   
c         .07 _< ZH _< .93  (if FAR =/= 1)
c          0. _< PAD _< 12.5 (if FAR =/= 1)
c
c Pass parameters:
c
c IO # Name    unit         description                     ref.value
c
c I  1 ZH      (-)   Relative vertical position of a       (see data A1)
c                    facade element
c I  2 FAR     (-)   Frontal Aspect Ratio for a facade,        (1)
c I  3 PAD     (-)   Plan Area Density                     (see data A2)
c O  4 COFAR   (-)   Correction Factor for CP(zh)              (1)
c O  5 RETURN1 (-)   Error return
c O  6 KEY     (-)   Error key; it is set to 1 if error is found.
c
c ERROR RETURN IF    0._> FAR > 8   
c                    PAD > 12.5  (if FAR =/= 1)
c                    .07 > ZH  > .93  (if FAR =/= 1)
c example:
c CALL CPFARL (.6, 2., 7.5, .72007, 999, 0)
c***********************************************************************
        COMMON /ERROR/ERF
        Common /unit/DOU,KBD,CRT,DIN
        common /silmode/ issilent
        logical issilent
        INTEGER KBD,CRT,DIN,DOU,ERF
        INTEGER KEY
 
        parameter (Nmax=30,Lmax=2)
        REAL ZH,PAD,FAR,polF(Nmax,Lmax),radF(30,3)

        REAL A1(6),A2(5),CORP1,CORP2,COFAR1,COFAR2
        INTEGER K,K1,L,L1,N

        REAL COFAR

c reference values for ZH
        DATA A1/.07,.2,.4,.6,.8,.93/

c reference values for PAD
        DATA A2/0.,5.,7.5,10.,12.5/

c coefficients for a linear equation yelding
c COFAR as fitting function of FAR if FAR < 1
        data polF/0.77, 0.694, 0.624, 0.6, 0.666, 0.55, 1.31, 1.096, 
     & 1.048, 1.096, 1.142, 1.042, 1.32, 1.17, 1.142, 1.17, 1.292,1.25, 
     & 1.302, 1.166, 1.12, 1.25, 1.428, 1.428, 1.366, 1.174, 1.166, 
     & 1.244, 1.4, 1.412,+0.23, +0.306, +0.376, +0.4, +0.334, +0.45, 
     & -0.31, -0.096, -0.048, -0.096, -0.142, -0.042, -0.32, -0.17, 
     & -0.142, -0.17, -0.292, -0.25, -0.302, -0.166, -0.12, -0.25, 
     & -0.428, -0.428, -0.366, -0.174, -0.166, -0.244, -0.4, -0.412/

c coefficients for a square root equation, such as 
c (a*x + b/x + c)**.5, yelding COFAR as fitting function
c of FAR if FAR > 1
       data radF/0.391319,0.208852,0.176644,0.222872,0.352525,0.409298,
     & 0.313066,0.262845,0.198393,0.202255,0.266436,0.378433,0.355636,
     & 0.256393,0.195066,0.179345,0.248347,0.286457,0.162696,0.14259,
     & 0.072493,0.062272,0.116832,0.111723,0.187639,0.113114,0.090391,
     & 0.058215,0.138563,0.115601,0.275277,0.045117,0.135403,0.219437,
     & 0.51124,0.101415,1.29096,1.187068,0.852449,0.824728,0.989084,
     & 0.831703,1.865418,1.501845,1.248485,1.132885,1.426085,1.200878,
     & 1.401255,1.382313,1.036706,0.956828,1.191314,0.959598,1.532033,
     & 1.30869,1.096843,0.921987,1.304438,1.108345,0.305879,0.727577,
     & 0.657545,0.5177,0.095033,0.461285,-0.679717,-0.511316,-0.107538,
     & -0.109405,-0.34636,-0.27258,-1.293254,-0.83996,-0.513001,
     & -0.406631,-0.79038,-0.562477,-0.650645,-0.611037,-0.199349, 
     & -0.131138,-0.445541,-0.190495,-0.830662,-0.518821,-0.281639,
     & -0.086177,-0.561468,-0.337801/

        IF (FAR.NE.1..AND.PAD.GT.12.5) THEN
           if (.not.issilent) then
              WRITE (ERF,*)  'PAD out of range (0-12.5)'
           end if
           KEY=1
           RETURN 1      
        ENDIF

        IF (FAR.EQ.1.0) GO TO 100

c check the position of ZH and PAD in relation to the reference
c values (A1,A2) and calculate the pointer number to search the 
c proper coefficients (polF,radF) for the fitting equations 
        K=0
        L=0
        N=0
        DO 10 I=1,6
          IF (ZH.NE.A1(I)) GO TO 10
          K=I
10      CONTINUE

        DO 20 J=1,5
          IF (PAD.NE.A2(J)) GO TO 20
          L=J
20      CONTINUE
        IF (K.NE.0.AND.L.NE.0) N=(L-1)*6+K
        IF (N.NE.0) GO TO 200
        IF (K.NE.0) GO TO 400
        IF (L.NE.0) GO TO 300

c when ZH and PAD are both different from  reference values
        CALL SEARCH (A1,ZH,6,K1)
        CALL SEARCH (A2,PAD,5,L1)

        N=(L1-1)*6+K1

        if (FAR.lt.1.0) then
          CALL CORPAN (FAR,polF,Nmax,Lmax,N,CORP1)
          CALL CORPAN (FAR,polF,Nmax,Lmax,N+1,CORP2)
        else
          CALL CORRAD (FAR,radF,Nmax,N,CORP1)
          CALL CORRAD (FAR,radF,Nmax,N+1,CORP2)
          if (corp1.eq.100..or.corp2.eq.100.)then
             write(DOU,*) 'FAR out of range (leeward facade)'
             cofar=10000.
             key=2
             GO to 999
          endif

        endif

        CALL LININT (ZH,A1(K1),A1(K1+1),CORP1,CORP2,COFAR1)

        N=L1*6+K1

        if (FAR.lt.1.0) then
          CALL CORPAN (FAR,polF,Nmax,Lmax,N,CORP1)
          CALL CORPAN (FAR,polF,Nmax,Lmax,N+1,CORP2)
        else
          CALL CORRAD (FAR,radF,Nmax,N,CORP1)
          CALL CORRAD (FAR,radF,Nmax,N+1,CORP2)
          if (corp1.eq.100..or.corp2.eq.100.)then
             write(DOU,*) 'FAR out of range (leeward facade)'
             cofar=10000.
             key=2
             GO to 999
          endif
        endif

        CALL LININT (ZH,A1(K1),A1(K1+1),CORP1,CORP2,COFAR2)
        CALL LININT (PAD,A2(L1),A2(L1+1),COFAR1,COFAR2,COFAR)
        GO TO 999

c when only PAD is different from reference value
400     CALL SEARCH (A2,PAD,5,L1)

        N=(L1-1)*6+K

        if (FAR.lt.1.0) then
          CALL CORPAN (FAR,polF,Nmax,Lmax,N,CORP1)
          CALL CORPAN (FAR,polF,Nmax,Lmax,N+6,CORP2)
        else
          CALL CORRAD (FAR,radF,Nmax,N,CORP1)
          CALL CORRAD (FAR,radF,Nmax,N+6,CORP2)
          if (corp1.eq.100..or.corp2.eq.100.)then
             write(DOU,*) 'FAR out of range (leeward facade)'
             cofar=10000.
             key=2
             GO to 999
          endif
        endif

        CALL LININT (PAD,A2(L1),A2(L1+1),CORP1,CORP2,COFAR)
        GO TO 999


c when only ZH is different from reference value 
300     CALL SEARCH (A1,ZH,6,K1)        

        N=(L-1)*6+K1

        if (FAR.lt.1.0) then
          CALL CORPAN (FAR,polF,Nmax,Lmax,N,CORP1)
          CALL CORPAN (FAR,polF,Nmax,Lmax,N+1,CORP2)
        else
          CALL CORRAD (FAR,radF,Nmax,N,CORP1)
          CALL CORRAD (FAR,radF,Nmax,N+1,CORP2)
          if (corp1.eq.100..or.corp2.eq.100.)then
             write(DOU,*) 'FAR out of range (leeward facade)'
             cofar=10000.
             key=2
             GO to 999
          endif

        endif

        CALL LININT (ZH,A1(K1),A1(K1+1),CORP1,CORP2,COFAR)

        GO TO 999

c when ZH and PAD are equal to reference values

200     if (FAR.lt.1.0) then
          CALL CORPAN (FAR,polF,Nmax,Lmax,N,COFAR)
        else
          CALL CORRAD (FAR,radF,Nmax,N,COFAR)
          if (cofar.eq.100.) then
             write(DOU,*) 'FAR out of range (leeward facade)'
             cofar=10000.
             key=2
          endif

        endif

        GO TO 999

c when FAR= 1
100     COFAR = 1.0       

999     CONTINUE

        RETURN
        END


        SUBROUTINE CPSARL (ZH,SAR,PAD,COSAR,*,KEY)
c************************************************************************
c Purpose: this routine calculates the Correction Factor for the
c          reference CP(zh), as function of the Side Aspect Ratio of 
c          the bldg. in relation to the considered wall.
c
c Module : # 4.3.1.2.4, TG#3, mxg/March 1,1989
c Changes: October 15, 1989 (written subroutines for SAR =/= 1)
c          January 29, 1990 (built CPSARL from CPASPS)
c          February 12, 1990 (raised upper limit from 2. to 4.)
c          May 7,1993 (changed SAR limits from .5 _< SAR _< 4. to  0.< SAR _< 8.)
c Limits : 0. < SAR _< 8.   
c          0. _< PAD _< 12.5  (if SAR =/= 1)
c          .07 _< ZH _< .93   (if SAR =/= 1)
c Pass parameters:
c
c IO # Name    unit         description                      ref.value
c
c I  1 ZH      (-)   Relative vertical position of a        (see data A1))
c                    facade element
c I  2 SAR     (-)   Side Aspect Ratio for a facade             (1)
c I  3 PAD     (-)   Plan Area Density                      (see data A2) 
c O  4 COSAR   (-)   Correction Factor for CP(zh)               (1)
c O  5 RETURN1 (-)   Error return
c O  6 KEY     (-)   Error key; it is set to 1 if error is found.
c
c ERROR RETURN IF    0._> SAR > 8.   
c                    PAD > 12.5  (if SAR =/= 1)
c                    .07 > ZH  > .93   (if SAR =/= 1)
c example:
c CALL CPSARL (.6, 1.5, 7.5, .566, 999, 0)
c***********************************************************************
        Common /unit/DOU,KBD,CRT,DIN
        COMMON /ERROR/ERF
        common /silmode/ issilent
        logical issilent
        INTEGER KBD,CRT,DIN,DOU,ERF
        INTEGER KEY
 
        parameter (Nmax=30,Lmax=2)
        REAL ZH,PAD,SAR,polS(Nmax,Lmax),radS(30,3)

        REAL A1(6),A2(5),CORP1,CORP2,COSAR1,COSAR2
        INTEGER K,K1,L,L1,N

        REAL COSAR

c reference values for ZH
        DATA A1/.07,.2,.4,.6,.8,.93/

c reference values for PAD
        DATA A2/0.,5.,7.5,10.,12.5/

c coefficients for a linear equation yelding
c COSAR as fitting function of SAR if SAR < 1
       data polS/ -0.462, -0.444, -0.5, -0.6, -0.666, -0.986, 0.62, 
     & 0.484, 0.286, 0.322, 0.358, 0.124, 0.56, 0.416, 0.358, 0.378, 
     & 0.416, 6.245004e-17, 0.418, 0.374, 0.28, 0.334, 0.286, 0.058, 
     & 0.586, 0.392, 0.208, 0.088, 0.2, -0.118,
     & +1.462, +1.444, +1.5, +1.6, +1.666, +1.986, +0.38, +0.516, 
     & +0.714, +0.678, +0.642, +0.876, +0.44, +0.584, +0.642, +0.622, 
     & +0.584, +1.0, +0.582, +0.626, +0.72, +0.666, +0.714, +0.942, 
     & +0.414, +0.608, +0.792, +0.912, +0.8, +1.118/

c coefficients for a square root equation, such as 
c (a*x + b/x + c)**.5, yelding COSAR 
c as fitting function of SAR if SAR > 1
       data radS/1.549121,1.293432,0.818276,0.622491,0.431822,1.15475, 
     & 1.234668,1.086419,1.110227,1.248462,1.158504,0.924129,1.6176, 
     & 1.405914, 1.39227, 1.446764, 1.541118, 1.395, 1.728091, 1.675056, 
     & 1.632, 1.623354, 2.133661, 2.099225, 2.249115, 2.121972, 1.99874, 
     & 2.373076, 2.133851, 2.204859,4.008955,3.376296,2.757414,2.463733, 
     & 2.206986, 3.567738, 3.821814, 3.381557, 3.330677, 3.745386, 
     & 3.817008, 3.214321, 4.7352, 4.082196, 4.047642, 4.209078, 
     & 4.623354,4.185,5.065453,4.762584,4.6368,4.746708, 5.767382, 
     & 5.670099,6.239501,5.826368,5.578012,6.268238,5.94792,6.021059,
     & -4.558076,-3.669728,-2.575691,-2.086225,-1.638808, 
     & -3.722488,-4.056482, -3.467976, -3.440903, -3.993848, 
     & -3.975512, -3.13845, -5.352801, -4.48811, -4.439912, -4.655842, 
     & -5.164472, -4.58, -5.793544, -5.437641, -5.2688, -5.370063, 
     & -6.900996, -6.769291, -7.488376, -6.948252, -6.576709, 
     & -7.641063, -7.081692, -7.225708/

        IF (SAR.NE.1..AND.PAD.GT.12.5) THEN
           if (.not.issilent) then
              WRITE (ERF,*)  'PAD out of range (0-12.5)'
           end if
           KEY=1
           RETURN 1      
        ENDIF

        IF (SAR.EQ.1.0) GO TO 100

c check the position of ZH and PAD in relation to the reference
c values (A1,A2) and calculate the pointer number to search the 
c proper coefficients     
        K=0
        L=0
        N=0
        DO 10 I=1,6
          IF (ZH.NE.A1(I)) GO TO 10
          K=I
10      CONTINUE

        DO 20 J=1,5
          IF (PAD.NE.A2(J)) GO TO 20
          L=J
20      CONTINUE

        IF (K.NE.0.AND.L.NE.0) N=(L-1)*6+K
        IF (N.NE.0) GO TO 200
        IF (K.NE.0) GO TO 400
        IF (L.NE.0) GO TO 300

c when ZH and PAD are both different from  reference values
        CALL SEARCH (A1,ZH,6,K1)
        CALL SEARCH (A2,PAD,5,L1)

        N=(L1-1)*6+K1

        if (SAR.lt.1.0) then
          CALL CORPAN (SAR,polS,Nmax,Lmax,N,CORP1)
          CALL CORPAN (SAR,polS,Nmax,Lmax,N+1,CORP2)
        else
          CALL CORRAD (SAR,radS,Nmax,N,CORP1)
          CALL CORRAD (SAR,radS,Nmax,N+1,CORP2)
          if (corp1.eq.100..or.corp2.eq.100.)then
             write(DOU,*) 'SAR out of range (leeward facade)'
             cosar=10000.
             key=2
             GO to 999
          endif
        endif

        CALL LININT (ZH,A1(K1),A1(K1+1),CORP1,CORP2,COSAR1)

        N=L1*6+K1

        if (SAR.lt.1.0) then
          CALL CORPAN (SAR,polS,Nmax,Lmax,N,CORP1)
          CALL CORPAN (SAR,polS,Nmax,Lmax,N+1,CORP2)
        else
          CALL CORRAD (SAR,radS,Nmax,N,CORP1)
          CALL CORRAD (SAR,radS,Nmax,N+1,CORP2)
          if (corp1.eq.100..or.corp2.eq.100.)then
             write(DOU,*) 'SAR out of range (leeward facade)'
             cosar=10000.
             key=2
             GO to 999
          endif

        endif

        CALL LININT (ZH,A1(K1),A1(K1+1),CORP1,CORP2,COSAR2)
        CALL LININT (PAD,A2(L1),A2(L1+1),COSAR1,COSAR2,COSAR)
        GO TO 999

c when only PAD is different from reference value
400     CALL SEARCH (A2,PAD,5,L1)

        N=(L1-1)*6+K

        if (SAR.lt.1.0) then
          CALL CORPAN (SAR,polS,Nmax,Lmax,N,CORP1)
          CALL CORPAN (SAR,polS,Nmax,Lmax,N+6,CORP2)
        else
          CALL CORRAD (SAR,radS,Nmax,N,CORP1)
          CALL CORRAD (SAR,radS,Nmax,N+6,CORP2)
          if (corp1.eq.100..or.corp2.eq.100.)then
             write(DOU,*) 'SAR out of range (leeward facade)'
             cosar=10000.
             key=2
             GO to 999
          endif
        endif

        CALL LININT (PAD,A2(L1),A2(L1+1),CORP1,CORP2,COSAR)
        GO TO 999


c when only ZH is different from reference value 
300     CALL SEARCH (A1,ZH,6,K1)        

        N=(L-1)*6+K1

        if (SAR.lt.1.0) then
          CALL CORPAN (SAR,polS,Nmax,Lmax,N,CORP1)
          CALL CORPAN (SAR,polS,Nmax,Lmax,N+1,CORP2)
        else
          CALL CORRAD (SAR,radS,Nmax,N,CORP1)
          CALL CORRAD (SAR,radS,Nmax,N+1,CORP2)
          if (corp1.eq.100..or.corp2.eq.100.)then
             write(DOU,*) 'SAR out of range (leeward facade)'
             cosar=10000.
             key=2
             GO to 999
          endif
        endif

        CALL LININT (ZH,A1(K1),A1(K1+1),CORP1,CORP2,COSAR)

        GO TO 999

c when ZH and PAD are equal to reference values

200     if (SAR.lt.1.0) then
          CALL CORPAN (SAR,polS,Nmax,Lmax,N,COSAR)
        else
          CALL CORRAD (SAR,radS,Nmax,N,COSAR)
          if (COSAR.eq.100.)then
            cosar=10000.
            key=2
            write(DOU,*) 'SAR out of range (leeward facade)'
          endif
        endif

        GO TO 999

c when SAR= 1
100     COSAR = 1.0       

999     CONTINUE

        RETURN
        END


        SUBROUTINE CPHORL (ZH,AnbW,XL,COHOR)
c************************************************************************
c Purpose: this routine calculates the Correction Factor for the
c          reference CP(zh), in relation to the Wind Direction and the
c          relative horizontal position of a facade element
c
c Module : # 4.3.1.2.5, TG#3, mxg/March 1,1989
c Changes: January 29, 1990 (built CPHORL from CPHORG)
c Limits : -180 _< AnbW < -90 
c            90 < AnbW _< 180
c
c Pass parameters:
c
c IO # Name    unit         description                        ref.value
c
c I  1 ZH      (-)   Relative vertical position of a           (.5,.7,.9)
c                    facade element
c I  2 AnbW    (deg) Angle between Wind Direction and normal  (90,110,130,160,180,
c                    to a facade (clockwise, 0 to +90,             val. abs.)
c                    counterclockwise, 0 to -90)              
c I  4 XL      (-)   Relative horiz. position of a                (0.5)
c                    facade element
c O  5 COHOR   (-)   Correction Factor for CP(zh)                  (1)
c
c example:
c CALL CPHORL (.6, 135., .5, .618534)
c***********************************************************************
        parameter (Nmax=15,Lmax=5)
        REAL ZH,AnbW,polH(Nmax,Lmax),XL,A(5)

        INTEGER K,N,L,L1

        REAL COHOR

c reference values for AnbW
        data A/90., 110., 130., 160., 180./

c coefficients for the 3rd and 4th order polinomial equations
c yelding COHOR as fitting function of XL
        data polH/0., 0., 0., 0., 0., 0., 0., 0., 0., 0., -13.234266, 
     & -18.269231, -9.985431, -8.458625, -6.555944, 9.325952,2.526807,
     & 0.200855, 0.861888, 4.145989e-16, 11.862859, 1.79934, -0.069542, 
     & 1.003108, 3.88578e-16, 47.482906,38.486402,17.831974,17.902681, 
     & 13.106061,-16.031002, -5.145221, -1.520047, -1.966841,-0.107692, 
     & -19.086364, -2.526981, 0.404196, -0.873077, 0.449883,-48.637238, 
     & -24.083741, -8.056789, -10.191521, -7.364394,6.08061, 3.28289,
     & 1.734472, 1.561282, 0.107692, 6.79763, 1.326103, 0.124611, 
     & 0.398465, -0.449883, 13.933178, 4.338003, 0.346156, 1.433689, 
     & 0.809767,2.162909, 1.400238, 1.275364, 0.923007, 0.975846, 
     & 2.204853, 1.631755, 1.506259, 1.093671, 1.102028, 2.493133, 
     & 1.973497, 1.844014, 1.232881, 1.244049/

        IF (AnbW.LT.0) XL=1.0-XL

c calculate the list number of the polinomial coefficients (polH)
c in relation to ZH
        K=INT((ZH-.6)*5+2)
        IF (K.LT.1.)  K=1

c check the position of AnbW in relation to the reference
c values (A) and calculate the pointer number to search the 
c proper coefficients (polH) for the polinomial equation 
        L=0
        N=0
        DO 10 I=1,5
          IF (ABS(AnbW).NE.A(I)) GO TO 10
          L=I
10      CONTINUE
 
        if (L.NE.0) N=(K-1)*5+L

        IF (N.NE.0) GO TO 100

c when AnbW is different from a reference value
        CALL SEARCH (A,ABS(AnbW),5,L1)
        N=(K-1)*5+L1

        CALL CORPAN (XL,polH,Nmax,Lmax,N,CORP1)
        CALL CORPAN (XL,polH,Nmax,Lmax,N+1,CORP2)
        CALL LININT (ABS(AnbW),A(L),A(L+1),CORP1,CORP2,COHOR)
        GO TO 999

c when AnbW is equal to a reference value
100     CALL CORPAN (XL,polH,Nmax,Lmax,N,COHOR)
        GO TO 999

999     CONTINUE

        return
        end
