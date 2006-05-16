C suple.f contains routines that are called by CPVELE to calculate
C pressure coefficients on the windwards orientated facade of the 
C building:
 
C CPBOUL 
C CPDENS
C CPRELH
C CPASPF 
C CPASPS
C CPHORG

       SUBROUTINE CPBOUL (ZH,VeEXP,COBOL,*,KEY)
c************************************************************************
c Purpose: this routine calculates the Correction Factor for the 
c          reference CP(zh) value, in relation to the Boundary Layer, 
c          characteristic of the site wich the Building is located in.
c
c Module : # 4.3.1.2.1, TG#3, mxg/March 1,1989
c Changes: December 1, 1989 (included equation coefficients data block)
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
c CALL CPBOUL (.65, .28, .849107, 999, 0)
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
     & -10.820106, -10.42328, -8.531746, -0.939153, 5.10582,
     & +2.312434, +1.268783, +0.688492, -1.691138, -3.350529,
     & +1.014958, +1.225354, +1.261468, +1.417505, +1.489995/

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


	 SUBROUTINE CPDENS (ZH,PAD,COPAD,*,KEY)
c************************************************************************
c Purpose: this routine calculates the Correction Factor for the
c    Reference CP(zh), in relation to the Plan Area Density 
c          characteristic of the Building's surroundings.
c
c Module : # 4.3.1.2.2, TG#3, mxg/March 1,1989
c Changes: December 1, 1989 (included equation coefficients data block) 
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
c CALL CPDENS (.65, 7.5, .489319, 999, 0)
c***********************************************************************
	COMMON /ERROR/ERF
        common /silmode/ issilent
        logical issilent
        INTEGER erf
	INTEGER KEY

	parameter (Nmax=5,Lmax=4)
	REAL ZH,PAD,polD(Nmax,Lmax)
	INTEGER K

	REAL COPAD

c coefficients for a 3rd order polinomial equation yelding
c COPAD as fitting function of PAD
	data polD/ -2.14966e-05, -1.775637e-05, -1.523628e-05, 
     & -1.571837e-05, -1.987115e-05,
     & +2.37444e-03, +2.034996e-03, +1.788998e-03, 
     & +1.693211e-03, +1.968606e-03,
     & -0.089797, -0.081741, -0.074881, -0.06647, -0.067063,
     & +0.979603, +0.995399, +1.00378, +0.994355, +0.966038/
        
c error return
	IF (PAD.LT.0..OR.PAD.GT.50.) THEN
           if (.not.issilent) then
              WRITE (ERF,*)  'PAD out of range (0-50)'
           end if
           KEY=1
	   RETURN 1
	ENDIF

c calculate the list number of the coefficients (polD)
c for the polinomial equation 
        K= INT((ZH-.65)*10.+2.)
	IF (K.LT.1) THEN 
	   K=1
	ENDIF
	
c calculate COPAD
	COPAD = 0
	DO 10 J=1,4
	   COPAD = COPAD + polD(K,J)*PAD**(4-J)
10	CONTINUE
	go to 999

100	COPAD = 1.0
 		
999	continue
	RETURN
	END


	SUBROUTINE CPRELH (ZH,PAD,RbH,COREL,*,KEY)
c************************************************************************
c Purpose: this routine calculates the Correction Factor for the
c          reference CP(zh), in relation to the Relative Height of the
c          Building at a given PAD.
c Module : # 4.3.1.2.3, TG#3, mxg/March 1,1989
c Changes: June 28, 1989 (checked new limit for PAD)
c          December 1, 1989 (included equation coefficients data block)
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
c CALL CPRELH (.65, 7.5, 2., 2.0274, 999, 0)
c***********************************************************************
	COMMON /ERROR/ERF
	INTEGER erf
	INTEGER KEY

	parameter (Nmax=28,Lmax=4) 
	REAL ZH,PAD,RbH,polR(Nmax,Lmax)

	REAL A1(7),A2(4),CORP1,CORP2,COREL1,COREL2
	INTEGER K,K1,L,L1,N

	REAL COREL

c reference values for ZH
	DATA A1/.07,.2,.5,.7,.8,.9,.93/

c reference values for PAD
	DATA A2/0.,5.5,12.5,25./

c coefficients for 1st,2nd and 3rd order polinomial equations
c yelding COREL as fitting function of RbH
	data polR/0.,0.,0.,-0.354662,0.,0.,0.,0.,0.,0.,0.,2.743878, 
     & 0.,0.,0.,-1.213787,0.,0.,0.,-0.403791,0.,0.,0.,0.,0.,0.,0.,0.,  
     & 0.,0.,0.,1.416299,0.,0.,0.,1.534332,0.,0.,0.,-18.09787,
     & 0.,0.,0.,6.301881,0.,0.,0.,1.579764,
     & 0.,-0.155809,-0.217166,-0.733177,0.,-0.17031,-0.235091,-0.62338,
     & 0.111687,0.303608,0.665827,3.925792,0.152862,0.35057,0.691757,
     & -17.32797,0.251497,0.661656,1.601127,13.731616,0.280233,0.693236, 
     & 1.566717,4.370901,0.338131,0.719554,1.373569,5.205654,0.436478, 
     & 1.523391,2.2467,6.203364,0.464299,1.579231,2.28368,5.154261,
     & 0.848151,0.693641,0.450229,-3.814382,0.78183,0.60962,0.407027, 
     & 14.40045,0.705487,0.348831,-0.424487,2.08857,0.697339,0.346922, 
     & -0.325088,-6.988637,0.637794,0.349286,
     & -0.175915,-4.533334,0.555708, 
     & -0.266623,-0.855572,-3.94136,0.535423, 
     & -0.294406,-0.853961,-3.165345/

	IF (RbH.EQ.1.) GO TO 100

c check the position of ZH and PAD in relation to the reference
c values (A1,A2) and calculate the pointer number to search the 
c proper coefficients for the polinomial equation (polR)  
	K=0
	L=0
	N=0
	DO 10 I=1,7
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
	CALL SEARCH (A1,ZH,7,K1)
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
400     CALL SEARCH (A1,ZH,7,K1)

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


	SUBROUTINE CPASPF (ZH,FAR,PAD,COFAR,*,KEY)
c************************************************************************
c Purpose: this routine calculates the Correction Factor for the
c          reference CP(zh), as function of the Frontal Aspect Ratio of 
c          the bldg. in relation to the considered facade.
c
c Date   : As a Module of COMIS (# 4.3.1.2.4, TG#3, mxg), March 1,1989
c Changes: October 15, 1989 (written subroutines for FAR =/= 1)
c          December 1, 1989 (included equation coefficients data blocks)
c
c Date   : As a Module of CPCALC-PC, November 1, 1992 (Changed error message 
c          parameter from UNIX Fortran - RETURN1 - to Microsoft 
c          Fortran - *)
c Changes: May 7,1993 (extended FAR limits from .5 _< FAR _< 4. to 
c          0.< FAR _< 8.)  
c                     
c Date   : As a Module of WIND.1, December 17,1993 (eliminated pre-defined 
c          limits for FAR; included error messages if a given value of FAR 
c          - for FAR > 1 - makes the radical in the fitting equation 
c          negative; transferred error messages for ZH and PAD out of range
c          to the main routine WIND.1)
c
c Limits : FAR =/= value which makes the radical of the fitting equation   
c          negative (see subroutine CORRAD)
c
c Pass parameters:
c
c IO # Name    unit         description                     ref.value
c
c I  1 ZH      (-)   Relative vertical position of a       (see data A1)
c                    facade element
c I  2 FAR     (-)   Frontal Aspect Ratio for a facade,         
c                    i.e., the ratio of the facade's 
c                    length to its height                      (1)
c I  3 PAD     (-)   Plan Area Density                     (see data A2)
c O  4 COFAR   (-)   Correction Factor for CP(zh)              (1)
c O  5 *       (-)   Error return
c O  6 KEY     (-)   Error key; it is set to 2 if error is found.
c
c ERROR RETURN IF    FAR > 1 and FAR = a value which makes the radical of 
c                    the fitting equation negative   
c
c example:
c CALL CPASPF (.65, 1.5, 7.5, 1.3529,*999, 0)
c***********************************************************************
	COMMON /ERROR/ERF
        Common /unit/DOU,KBD,CRT,DIN
        common /silmode/ issilent
	INTEGER KBD,CRT,DIN,DOU,ERF
        INTEGER KEY
        logical issilent

	parameter (Nmax=30,Lmax=4)
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
	data polF/ 0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0.,
     & 0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0.,
     & 0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0.,
     & 0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0.,
     & 0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0.,
     & 0.21, 0.166, 0.102, 0.066, -0.04, -0.292, 
     & 0.286, 0.21, 0.148, 0.156, 0.028, -0.364, 
     & 0.134, 0.12, 0.054, 6.245004e-17, 0.038, -0.352, 
     & 0.182, 0.046, -0.12, -0.166, -0.052, -0.428, 
     & 0.1, -0.068, -0.058, -0.044, 0.032, -0.334, 
     & 0.79, 0.834, 0.898, 0.934, 1.04, 1.292, 
     & 0.714, 0.79, 0.852, 0.844, 0.972, 1.364, 
     & 0.866, 0.88, 0.946, 1., 0.962, 1.352, 
     & 0.818, 0.954, 1.12, 1.166, 1.052, 1.428, 
     & 0.9, 1.068, 1.058, 1.044, 0.968, 1.334/

c coefficients for a square root equation, such as 
c (a*x + b/x + c)**.5, yelding COFAR as fitting function
c of FAR if FAR > 1
	data radF/ -0.070887, -0.061746, -0.071734, -0.075213, 
     & -0.081452, -0.05991, 
     & -0.625867, -0.700802, -0.551417, -0.394759, -0.384892, -0.471534, 
     & -0.464735, -0.484764, -0.357666, -0.430568, -0.538978, -0.295157, 
     & -0.445623, -0.562911, -0.303556, -0.396287, -0.326486, -0.491857, 
     & 0.39952, 0.560605, 0.460531, 0.052937, -0.17023, -0.489256,
     & 0.335565, 0.39232, 0.370249, 0.280472, 0.261036, 0.441293, 
     & -3.31499, -3.691923, -2.657088, -1.857109, -1.582766, -1.938719, 
     & -4.370468, -4.700937, -3.421083, -3.272576, -3.080677, -2.106807, 
     & -5.965503, -8.352512, -5.104654, -4.685712, -3.146084, -3.607476, 
     & -6.357705, -10.512008, -5.146305, -4.346084, -3.285382,-4.363034,
     & 0.741492, 0.670057, 0.700161, 0.799646, 0.821341, 0.620374, 
     & 4.938818, 5.39902, 4.208561, 3.243966, 2.964682, 3.408053, 
     & 5.827134, 6.175447, 4.761667, 4.686477, 4.608249, 3.39147, 
     & 7.414155, 9.919405, 6.409214, 6.096834, 4.485651, 5.109896, 
     & 6.938206, 10.939653, 5.668398, 5.273574, 4.448491, 5.840238/

	IF (FAR.EQ.1.) GO TO 100

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
          if (corp1.eq.100..or.corp2.eq.100.) then
             if (.not.issilent) then
                write(erf,*) 'FAR out of range (windward facade)'
             end if
             write(dou,*) 'FAR out of range (windward facade)'
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
             if (.not.issilent) then
                 write(erf,*) 'FAR out of range (windward facade)'
             end if
             write(dou,*) 'FAR out of range (windward facade)'
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
             if (.not.issilent) then
                write(erf,*) 'FAR out of range (windward facade)'
             end if
             write(dou,*) 'FAR out of range (windward facade)'
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
             if (.not.issilent) then
                 write(erf,*) 'FAR out of range (windward facade)'
             end if
             write(dou,*) 'FAR out of range (windward facade)'
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
	  if (COFAR.eq.100.)then
	     cofar=10000.
	     key=2
             if (.not.issilent) then
                write(erf,*) 'FAR out of range (windward facade)'
             end if
             write(dou,*) 'FAR out of range (windward facade)'
	 endif
	endif

	GO TO 999

c when FAR= 1
100     COFAR = 1.0       

999     CONTINUE
	RETURN
	END


	SUBROUTINE CPASPS (ZH,SAR,PAD,COSAR,*,KEY)
c************************************************************************
c Purpose: this routine calculates the Correction Factor for the
c          reference CP(zh), as function of the Side Aspect Ratio of 
c          the bldg. in relation to the considered facade.
c
c Date   : As a Module of COMIS (# 4.3.1.2.4, TG#3, mxg), March 1,1989
c Changes: October 15, 1989 (written subroutines for SAR =/= 1)
c          December 1, 1989 (included equation coefficients data blocks)
c          February 12, 1990 (raised upper limit from 2. to 4.)
c
c Date   : As a Module of CPCALC-PC, November 1, 1992 (changed error message 
c          parameter from UNIX Fortran - RETURN1 - to Microsoft 
c          Fortran - *)
c Changes: May 7,1993 (extended SAR limits from .5 _< SAR _< 4. to  
c          0.< SAR _< 8.)  
c
c Date   : As a Module of WIND.1, December 17, 1993 (eliminated pre-defined
c          limits for SAR; included error message for values of SAR > 1, 
c          which make the radical of the fitting equation negative; 
c          transferred error messages for PAD and ZH out of range to the 
c          main routine WIND.1)
c
c Limits : If SAR > 1, SAR =/= a value which makes the radical of the 
c          fitting equation negative (see subroutine CORRAD) 
c
c Pass parameters:
c
c IO # Name    unit         description                      ref.value
c
c I  1 ZH      (-)   Relative vertical position of a        (see data A1))
c                    facade element
c I  2 SAR     (-)   Side Aspect Ratio for a facade,             
c                    i.e., the ratio of the facade's length
c                    to the length of the adjacent one          (1)
c I  3 PAD     (-)   Plan Area Density                      (see data A2) 
c O  4 COSAR   (-)   Correction Factor for CP(zh)               (1)
c O  5 *       (-)   Error return
c O  6 KEY     (-)   Error key; it is set to 2 if error is found.
c
c ERROR RETURN IF    SAR > 1 and SAR = a value which makes the radical in    
c                    the fitting equation negative
c
c example:
c CALL CPASPS (.65, 2., 7.5, 1.5385, 999, 0)
c***********************************************************************
	COMMON /ERROR/ERF
        Common /unit/DOU,KBD,CRT,DIN
        common /silmode/ issilent
	INTEGER KBD,CRT,DIN,DOU,ERF
        INTEGER KEY
        logical issilent

	parameter (Nmax=30,Lmax=4)
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
	data polS/ 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
     & 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
     & 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
     & 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
     & 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
     & -0.022, 0.056, -0.03, 6.245004e-17, -0.02, -0.166, 
     & 0.172, 0.19, 0.334, 0.438, 0.31, -0.09, 
     & 0.266, 0.298, 0.46, 0.436, 0.324, -0.118, 
     & 0.328, 0.318, 0.8, 0.666, 0.206, -0.286, 
     & 0.75, 1.104, 1.428, 1.2, 0.634, 6.245004e-17,
     & 1.022, 0.944, 1.03, 1., 1.02, 1.166, 
     & 0.828, 0.81, 0.666, 0.562, 0.69, 1.09, 
     & 0.734, 0.702, 0.54, 0.564, 0.676, 1.118, 
     & 0.672, 0.682, 0.2, 0.334, 0.794, 1.286, 
     & 0.25, -0.104, -0.428, -0.2, 0.366, 1./

c coefficients for a square root equation, such as 
c (a*x + b/x + c)**.5, yelding COSAR 
c as fitting function of SAR if SAR > 1
	data radS/ 0.102648, -0.044242, -0.02005, -2.751206e-10, 
     & -0.127266, 0.175931, 
     & -0.61983, -0.455586, 0.01539, 8.495999e-03, 0.03363, -0.83599, 
     & -0.672534, -0.589638, 0.44217, 0.313214, 0.53643, -0.32829, 
     & -1.31805, -2.14576, 0.0608, 0.699422, 0.51795, -1.627836, 
     & 1.15625, 0.811914, 3.144588, 3.525422, 1.802288, -0.384444,
     & 0.307944, -0.132726, -0.06015, -5.399712e-10, -0.101574,0.527814, 
     & -2.745612, -2.714454, -1.522998, -1.108008, -0.665862, -2.639028, 
     & -4.465068, -4.571604, -2.377428, -2.334822, -1.011222, -2.984262, 
     & -7.924662, -11.416512, -6.2016, -3.950934, -2.521878, -6.191754, 
     & -5.8125, -10.848372, -2.954106, -0.048534, -0.832296, -4.326666,
     & 0.589408, 1.176968, 1.0802, 1., 1.22884, 0.296255, 
     & 4.365442, 4.17004, 2.507608, 2.099512, 1.632232, 4.475018, 
     & 6.137602, 6.161242, 2.935258, 3.021608, 1.474792, 4.312552, 
     & 10.242712, 14.562272, 7.1408, 4.251512, 3.003928, 8.81959, 
     & 5.65625, 11.036458, 0.809518, -2.476888, 0.030008, 5.71111/

	IF (SAR.EQ.1.) GO TO 100

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
             if (.not.issilent) then
                write(erf,*) 'SAR out of range (windward facade)'
             end if
             write(dou,*) 'SAR out of range (windward facade)'
	     cosar=10000.
	     key=2
	     go to 999
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
             if (.not.issilent) then
                 write(erf,*) 'SAR out of range (windward facade)'
             end if
             write(dou,*) 'SAR out of range (windward facade)'
	     cosar=10000.
	     key=2
	     go to 999
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
             if (.not.issilent) then
                write(erf,*) 'SAR out of range (windward facade)'
             end if
             write(dou,*) 'SAR out of range (windward facade)'
	     cosar=10000.
	     key=2
	     go to 999
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
             if (.not.issilent) then
                write(erf,*) 'SAR out of range (windward facade)'
             end if
             write(dou,*) 'SAR out of range (windward facade)'
	     cosar=10000.
	     key=2
	     go to 999
	 endif

	endif

	CALL LININT (ZH,A1(K1),A1(K1+1),CORP1,CORP2,COSAR)

	GO TO 999

c when ZH and PAD are equal to reference values
200     if (SAR.lt.1.0) then
	  CALL CORPAN (SAR,polS,Nmax,Lmax,N,COSAR)
	else
	  CALL CORRAD (SAR,radS,Nmax,N,COSAR)
	  if (cosar.eq.100.)then
	     cosar=10000.
	     key=2          
             if (.not.issilent) then
                write(erf,*) 'SAR out of range (windward facade)'
             end if
             write(dou,*) 'SAR out of range (windward facade)'
	 endif

	endif

	GO TO 999

c when SAR= 1
100     COSAR = 1.0       

999     CONTINUE

	RETURN
	END


	SUBROUTINE CPHORG (ZH,AnbW,XL,COHOR)
c************************************************************************
c Purpose: this routine calculates the Correction Factor for the
c          reference CP(zh), in relation to the Wind Direction and the
c          relative horizontal position of a facade element
c
c Module : # 4.3.1.2.5, TG#3, mxg/March 1,1989
c Changes: December 1, 1989 (included equation coefficients data block)
c Limits : -90 _< AnbW _< +90 
c
c Pass parameters:
c
c IO # Name    unit         description                        ref.value
c
c I  1 ZH      (-)   Relative vertical position of a           (.5,.7,.9)
c                    facade element
c I  2 AnbW    (deg) Angle between Wind Direction and normal  (0,10,20,30,40,
c                    to a facade (clockwise, 0 to +90,        50,60,70,80,90,
c                    counterclockwise, 0 to -90)              val. abs.)
c I  4 XL      (-)   Relative horiz. position of a                (0.5)
c                    facade element
c O  5 COHOR   (-)   Correction Factor for CP(zh)                  (1)
c
c example:
c CALL CPHORG (.65, 45., .35, .405199)
c***********************************************************************
	parameter (Nmax=30,Lmax=4)
	REAL ZH,AnbW,polH(Nmax,Lmax),XL

	INTEGER K,N,K1,L

	REAL COHOR

c coefficients for 2nd and 3rd order polinomial equations
c yelding COHOR as fitting function of XL
        data polH/0.,0.,0.,0.,0.,0.,0.,0.,0.,-5.984848,0.,0.,0.,0.,0.,
     & 0.,0.,0.,0.,-4.015152,0.,0.,0.,0.,0.,0.,0.,0.,0.,-6.959984,
     & -3.04662, -3.142447, -2.001166, -1.275862, -1.058275, 
     & -0.891626, -1.560755, -1.990676, -1.651067, 10.036713, 
     & -2.501166, -2.665435, -1.674825, -0.869048, -0.635198, 
     & -0.667077, -1.415846, -2.064103, -1.842775, 6.670746, 
     & -2.456876, -2.681034, -1.724942, -0.832512, -0.547786, 
     & -0.88711, -1.85509, -2.815851, -2.449507, 10.745338,
     & 3.04662, 2.873329, 1.398438, 0.278803, -0.01627, 
     & 0.247508, 1.496049, 2.614312, 2.530479, -3.883683, 
     & 2.501166, 2.355141, 1.008462, -0.176541, -0.467529, 
     & 3.841881e-03, 1.367316, 2.719557, 2.788363, -2.319231, 
     & 2.456876, 2.335446, 0.981305, -0.270429, -0.544942, 
     & 0.279757, 1.935973, 3.659487, 3.577449, -3.502836,
     & 0.268462, 0.38632, 0.693916, 0.935081, 0.871259, 
     & 0.428414, -0.257573, -0.994965, -1.359928, -0.778811, 
     & 0.401189, 0.523287, 0.802867, 1.051723, 0.973357, 
     & 0.485571, -0.231142, -1.005524, -1.37687, -0.836434, 
     & 0.451469, 0.581156, 0.888531, 1.118564, 0.992378, 
     & 0.426546, -0.375921, -1.236923, -1.585214, -0.877273/

c calculate the list number of the polinomial coefficients (polH)
c in relation to ZH
	K=INT((ZH-.6)*5+2)
	IF (K.LT.1.0) THEN
	   K=1
	ENDIF

c calculate the list number of the polinomial coefficients (polH)
c in relation to AnbW
	K1=INT((ABS(AnbW)-5.)/10+2)
	IF (K1.LT.1.) THEN
	   K1=1
	ENDIF

	N=(K-1)*10+K1

c calculate COHOR
	IF (AnbW.LT.0.0) THEN
	    XL=1.0-XL
	ENDIF

	COHOR=0.0
	DO 10 L=1,4
	   COHOR=COHOR+polH(N,L)*XL**(4-L)
10	CONTINUE
	RETURN
	END
