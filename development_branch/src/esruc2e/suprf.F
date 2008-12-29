C suple.f contains routines that are called by CPROOF to calculate
C pressure coefficients on the windwards orientated facade of the 
C building:

C BLROOF
C DENROOF
C RELHROOF
C FARROOF
C SARROOF
C HORROOF
C PHISING
C PHIXSIN
C PHIDOUB
C PHIXDOU

        SUBROUTINE BLROOF (ZH,VeEXP,AnbW,COBOL,*,KEY)
c************************************************************************
c  Purpose: this routine calculates the Correction Factor for the
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
c IO # Name    unit         description                  ref.value
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
c CALL BLROOF ( .6, .28, .770272, 999, 0)

c***********************************************************************
        COMMON /ERROR/ERF
        common /silmode/ issilent
        logical issilent
        INTEGER erf
        INTEGER KEY
 
        parameter (Nmax=20,Lmax=3)
        REAL ZH,VeEXP,AnbW,polB(Nmax,Lmax)

        REAL COBOL
        INTEGER N

c coefficients for a 2nd order polinomial equation yelding
c COBOL as fitting function of VeEXP
        data polB/ 0.736793,   0.823077,  5.0979,   4.04416,  -0.468661,
     &   0.961844,   4.09866,   2.7094,   3.50067,   3.69963,
     &   2.44702,   1.43712,   2.9943,  5.47503,   2.73932,
     &   5.52896,   5.0979,  5.0979,   5.0979,  5.52896,
     &   2.3355,    2.14835 , - 30.2797, - 22.4145,+ 12.5427,
     &   + 1.03022, - 22.0987, - 12.8205,- 17.834,- 19.1758,
     & - 9.48476, - 3.04029, - 14.9573,- 33.0364,- 13.1838,
     &  - 32.9487, - 30.2797, - 30.2797,- 30.2797,- 32.9487,
     &  - 5.09731, - 6.31868 ,+ 48.951,+ 35.9687,- 25.9972,
     &  - 4.19719, + 33.0268, + 21.3675,+ 26.6532,+ 28.3883,
     & + 11.2482, + 4.2735, + 24.9288,+ 53.3063,+ 22.4359,
     &  + 51.5195, + 48.951, + 48.951,+ 48.951, + 51.5195/

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
c values  and calculate the pointer number to search the
c proper coefficients (polB) for the polinomial equation 
        K=0
        N=0

          y=zh
          Teta=abs(AnbW)
          if (teta.gt.90.and.teta.le.180 )then
             y=1.0-y
             Teta=180.0-Teta
          endif
c   calculate the list number of the coefficients (polD)
c   for the polinomial equation
          K= INT(Teta/22.5+1.0)
          IF (K.gt.4) THEN
             K=4
          ENDIF
          N= INT(y*5.+1.0)
          IF (N.gt.5) THEN
                 N=5
          ENDIF
        K=(K-1)*5+N
c calculate COBOL
        COBOL = 0
        DO 10 J=1,3
           COBOL = COBOL + polB(K,J)*VeExp**(J-1)
10      CONTINUE
        go to 999

100     COBOL = 1.0
 
999     continue

        RETURN
        END


         SUBROUTINE DENROOF (ZH,PAD,COPAD,*,KEY)
c************************************************************************
c Purpose: this routine calculates the Correction Factor for the
c          Reference CP(zh), in relation to the Plan Area Density 
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

        parameter (Nmax=6,Lmax=5,Nval=6)
        REAL ZH,PAD,polD(Lmax,Nmax)
        INTEGER K

        REAL COPAD,A(nval)
c reference values for ZH
        DATA A/0.083,0.25,0.417,0.583,0.75,0.917/

c coefficients for a 4rd order polinomial equation yelding
c COPAD as fitting function of PAD
        data polD/ 1.05258,- 0.062932,+ 0.000821475,+ 0.0000206974,
     &  - 3.89421e-7,
     &   1.02747,- 0.115347,+ 0.00529545,- 0.000102754,+ 7.1812e-7,
     &  0.998148,- 0.139899,+ 0.00757453,- 0.000166495,+ 1.28945e-6,
     &  0.958771,- 0.1432,+ 0.00838456,- 0.000192437,+ 1.53236e-6,
     &  0.937033,- 0.113116,+ 0.00677578,- 0.000152213,+ 1.1718e-6,
     &  1.02769,-0.070838,+0.00312923,-0.0000429637,+1.33058e-7 /
        
c error return
        IF (PAD.LT.0..OR.PAD.GT.50.) THEN
           if (.not.issilent) then
              WRITE (ERF,*)  'PAD out of range (0-50)'
           end if
           KEY=1
           RETURN 1
        ENDIF
        IF(PAD.EQ.0.)go to 100

c check the position of ZH in relation to the reference
c values (A) and calculate the pointer number to search the 
c proper coefficients (pold) for the polinomial equation
        K=0
        N=0

        DO 10 I=1,Nval
          IF (ZH.NE.A(I)) GO TO 10
          K=I
10      CONTINUE

        if (K.NE.0) N=K
        if (ZH.lt.A(1)) N=1
        if (ZH.gt.A(Nval)) N=Nval

        IF (N.NE.0) GO TO 200

c when ZH is different from a reference value
        CALL SEARCH (A,ZH,Nval,K1)
        N=K1

        CALL CORPANS (PAD,polD,Nmax,Lmax,N,CORP1)
        CALL CORPANS (PAD,polD,Nmax,Lmax,N+1,CORP2)
        CALL LININT (ZH,A(N),A(N+1),CORP1,CORP2,COPAD)
        GO TO 999

c when ZH is equal to a reference value
200     CALL CORPANS (PAD,polD,Nmax,Lmax,N,COPAD)
        GO TO 999
 
c When Pad=0

100     COPAD = 1.0
 
999     continue

        RETURN
        END


        SUBROUTINE RELHROOF (ZH,PAD,RbH,COREL,*,KEY)
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

        parameter (Nmax=30,Lmax=4,Nval1=6,Nval2=5)
        REAL ZH,PAD,RbH,polR(lmax,Nmax)

        REAL A1(Nval1),A2(Nval2),CORP1,CORP2,COREL1,COREL2
	INTEGER K,K1,L,L1,N

	REAL COREL

c reference values for ZH
        DATA A1/.09,.27,.42,.59,.76,.92/

c reference values for PAD
        DATA A2/0.,5.,6.25,12.5,25./

        polR( 1,1)= 0.4712870121
        polR( 2,1)= 0.6429349780
        polR( 3,1)=-0.1279599965
        polR( 4,1)= 0.0120572001
        polR( 1,2)= 0.2098940015
        polR( 2,2)= 0.8137000203
        polR( 3,2)=-0.1597149968
        polR( 4,2)= 0.0125284996
        polR( 1,3)= 0.0795589983
        polR( 2,3)= 0.9518659711
        polR( 3,3)=-0.2116470039
        polR( 4,3)= 0.0186998006
        polR( 1,4)=-0.4332540035
        polR( 2,4)= 1.1418399811
        polR( 3,4)=-0.2142560035
        polR( 4,4)= 0.0140477996
        polR( 1,5)=-0.5556139946
        polR( 2,5)= 1.0714999437
        polR( 3,5)=-0.1052910015
        polR( 4,5)=-0.0066916398
        polR( 1,6)= 0.3039360046
        polR( 2,6)= 0.9008380175
        polR( 3,6)=-0.2360170037
        polR( 4,6)= 0.0296931993
        polR( 1,7)=-0.0616932996
        polR( 2,7)= 0.7961369753
        polR( 3,7)=-0.0597644001
        polR( 4,7)= 0.0054986598
        polR( 1,8)=-0.0973765031
        polR( 2,8)= 0.7284640074
        polR( 3,8)= 0.0054578502
        polR( 4,8)=-0.0146132996
        polR( 1,9)=-0.0312596001
        polR( 2,9)= 0.0286212005
        polR( 3,9)= 0.3571450114
        polR( 4,9)=-0.0643303022
        polR( 1,10)=-0.0079633500
        polR( 2,10)=-0.2084880024
        polR( 3,10)= 0.5181609988
        polR( 4,10)=-0.0897357985
        polR( 1,11)= 0.1651269943
        polR( 2,11)= 0.9321699739
        polR( 3,11)=-0.1655589938
        polR( 4,11)= 0.0162026007
        polR( 1,12)= 0.1577370018
        polR( 2,12)= 0.0563174002
        polR( 3,12)= 0.4051660001
        polR( 4,12)=-0.0759373978
        polR( 1,13)= 0.2192540020
        polR( 2,13)=-0.0909017026
        polR( 3,13)= 0.4679160118
        polR( 4,13)=-0.0837439969
        polR( 1,14)= 0.2040220052
        polR( 2,14)=-0.5105339885
        polR( 3,14)= 0.6146140099
        polR( 4,14)= 0.0925156
        polR( 1,15)= 0.323177
        polR( 2,15)=- 0.793107 
        polR( 3,15)= 0.767414
        polR( 4,15)= 0.113929
        polR( 1,16)= 0.0599793009
        polR( 2,16)= 1.0842900276
        polR( 3,16)=-0.2199289948
        polR( 4,16)= 0.0265992004
        polR( 1,17)= 0.4003199935
        polR( 2,17)=-0.2444719970
        polR( 3,17)= 0.4790740013
        polR( 4,17)=-0.0724171028
        polR( 1,18)= 0.3204270005
        polR( 2,18)=-0.2163919955
        polR( 3,18)= 0.4559080005
        polR( 4,18)=-0.0666704029
        polR( 1,19)= 0.1607870013
        polR( 2,19)=-0.3566929996
        polR( 3,19)= 0.4908510149
        polR( 4,19)=-0.0666785985
        polR( 1,20)= 0.6618080139
        polR( 2,20)=-1.3177299500
        polR( 3,20)= 1.0557800531
        polR( 4,20)=-0.1639149934
        polR( 1,21)=-0.0404880010
        polR( 2,21)= 1.2088600397
        polR( 3,21)=-0.2346889973
        polR( 4,21)= 0.0307158008
        polR( 1,22)= 0.5467249751
        polR( 2,22)=-0.1667329967
        polR( 3,22)= 0.3677360117
        polR( 4,22)=-0.0478180982
        polR( 1,23)= 0.3200809956
        polR( 2,23)= 0.0053974399
        polR( 3,23)= 0.3308269978
        polR( 4,23)=-0.0466367006
        polR( 1,24)= 0.0776846036
        polR( 2,24)=-0.1688529998
        polR( 3,24)= 0.3883509934
        polR( 4,24)=-0.0486754999
        polR( 1,25)= 0.3861849904
        polR( 2,25)=-0.4184699953
        polR( 3,25)= 0.4634979963
        polR( 4,25)=-0.0545464009
        polR( 1,26)= 0.0363828987
        polR( 2,26)= 1.1072000265
        polR( 3,26)=-0.1359820068
        polR( 4,26)= 0.0132202003
        polR( 1,27)= 0.5362600088
        polR( 2,27)= 0.3816539943
        polR( 3,27)= 0.0613023005
        polR( 4,27)=-0.0008560040
        polR( 1,28)= 0.1690749973
        polR( 2,28)= 0.5461480021
        polR( 3,28)= 0.1178160012
        polR( 4,28)=-0.0212909002
        polR( 1,29)=-0.3704699874
        polR( 2,29)= 0.9646149874
        polR( 3,29)=-0.2396660000
        polR( 4,29)= 0.0481170006
        polR( 1,30)= 0.0019528300
        polR( 2,30)= 0.5295400023
        polR( 3,30)=-0.0398886018
        polR( 4,30)= 0.0214510001

	IF (RbH.EQ.1.) GO TO 100

c check the position of ZH and PAD in relation to the reference
c values (A1,A2) and calculate the pointer number to search the 
c proper coefficients for the polinomial equation (polR)  
	K=0
	L=0
	N=0
        DO 10 I=1,Nval1
	   IF (ZH.NE.A1(I)) GO TO 10
	   K=I
10      CONTINUE
        if (ZH.lt.A1(1))K=1
        if (ZH.gt.A1(Nval1)) K=Nval1

        DO 20 J=1,Nval2
	   IF (PAD.NE.A2(J)) GO TO 20
	   L=J
20      CONTINUE

        IF (K.NE.0.AND.L.NE.0) N=(K-1)*Nval2+L
	IF (N.NE.0) GO TO 200
	IF (K.NE.0) GO TO 300
	IF (L.NE.0) GO TO 400

c when ZH and PAD are both different from  reference values
        CALL SEARCH (A1,ZH,Nval1,K1)
        CALL SEARCH (A2,PAD,Nval2,L1)

        N=(K1-1)*Nval2+L1

        CALL CORPANS (RbH,polR,Nmax,Lmax,N,CORP1)
        CALL CORPANS (RbH,polR,Nmax,Lmax,N+1,CORP2)
	CALL LININT (PAD,A2(L1),A2(L1+1),CORP1,CORP2,COREL1)

        N=K1*Nval2+L1

        CALL CORPANS (RbH,polR,Nmax,Lmax,N,CORP1)
        CALL CORPANS (RbH,polR,Nmax,Lmax,N+1,CORP2)
	CALL LININT (PAD,A2(L1),A2(L1+1),CORP1,CORP2,COREL2)
	CALL LININT (ZH,A1(K1),A1(K1+1),COREL1,COREL2,COREL)
	GO TO 999

c when only ZH is different from reference value
400     CALL SEARCH (A1,ZH,Nval1,K1)

        N=(K1-1)*Nval2+L

        CALL CORPANS (RbH,polR,Nmax,Lmax,N,CORP1)
        CALL CORPANS (RbH,polR,Nmax,Lmax,N+4,CORP2)
	CALL LININT (ZH,A1(K1),A1(K1+1),CORP1,CORP2,COREL)
	GO TO 999

c when only PAD is different from reference value 
300     CALL SEARCH (A2,PAD,Nval2,L1)

        N=(K-1)*Nval2+L1

        CALL CORPANS (RbH,polR,Nmax,Lmax,N,CORP1)
        CALL CORPANS (RbH,polR,Nmax,Lmax,N+1,CORP2)
	CALL LININT (PAD,A2(L1),A2(L1+1),CORP1,CORP2,COREL)
	GO TO 999

c when ZH and PAD are equal to reference values
200     CALL CORPANS (RbH,polR,Nmax,Lmax,N,COREL)
	GO TO 999

c when RbH = 1
100     COREL = 1.0       

999     CONTINUE

        RETURN
        END


        SUBROUTINE FARROOF (ZH,FAR,PAD,COFAR,*,KEY)
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
	INTEGER erf
	INTEGER KEY
 
        parameter (Nmax=6,Lmax=9,Nval=6)
        REAL ZH,PAD,FAR,polF(Lmax,Nmax)

        REAL A1(Nval),CORP1,CORP2
        INTEGER K,K1,L,N

	REAL COFAR

c reference values for ZH
        DATA A1/.083,.25,.417,.59,.75,.92/

c coefficients for  equation yelding
c COFAR as fitting function of FAR and PAD
        data polF/0.953098, +0.0859329,-0.0122126, -0.0660489,
     &  +0.0134128, -0.00242134,+0.00118249,-0.000479493,
     &  +0.0000837366,
     &    0.34999,+0.463081,-0.0825332, -0.0326255,-0.0159325,
     &  +0.00300864, +0.000785474,-0.0000500963, +2.26288e-6,
     &   -0.00242469, +0.811158, -0.132272,-0.012209,-0.0357301,
     &  +0.00569966, +0.000577829,+0.000175528,-0.0000269364,
     &    0.00152731, +0.78654, -0.108673, -0.00758088, -0.0362125,
     &  +0.00427929, +0.000513975,+0.000203232,-0.0000118529,
     &    0.0426317,+0.687308,-0.0793606, -0.015797,-0.0202936,
     &  +0.000488312,+0.000761725,-0.00012974, +0.0000610457,
     &    0.304574, +0.50833, -0.0385091, -0.0284582, -0.00378569,
     &  -0.00306442, +0.00100574,-0.000405361,+0.00011848/

	IF (FAR.NE.1..AND.PAD.GT.12.5) THEN
	   WRITE (ERF,*)  'PAD out of range (0-12.5)'
	   KEY=1
	   RETURN 1      
	ENDIF

	IF (FAR.EQ.1.) GO TO 100

c check the position of ZH  in relation to the reference
c values (A1) and calculate the pointer number to search the
c proper coefficients (polF) for the fitting equations
	K=0
	L=0
	N=0
        DO 10 I=1,Nval
	   IF (ZH.NE.A1(I)) GO TO 10
	   K=I
10      CONTINUE

        if (ZH.lt.A1(1))K=1
        if (ZH.gt.A1(Nval)) K=Nval

        IF (K.NE.0) N=K
	IF (N.NE.0) GO TO 200

c when ZH is different from  reference values
	CALL SEARCH (A1,ZH,6,K1)
        N=K1
        CALL CORSUP (FAR,PAD,polF,Nmax,Lmax,N,CORP1)
        CALL CORSUP (FAR,PAD,polF,Nmax,Lmax,N+1,CORP2)
        CALL LININT (ZH,A1(K1),A1(K1+1),CORP1,CORP2,COFAR)
	GO TO 999

c when ZH is  equal to reference values

200     CALL CORSUP (FAR,PAD,polF,Nmax,Lmax,N,COFAR)
	GO TO 999

c when FAR= 1
100     COFAR = 1.0       

999     CONTINUE

	RETURN
	END


        SUBROUTINE SARROOF (ZH,SAR,PAD,COSAR,*,KEY)
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
c Limits :
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
c CALL SARROOF (.65, 2., 7.5, 1.5385, 999, 0)
c***********************************************************************

	COMMON /ERROR/ERF
        common /silmode/ issilent
        logical issilent
        INTEGER erf
	INTEGER KEY
 
        parameter (Nmax=7,Lmax=9,Nval=7)
        REAL ZH,PAD,SAR,polS(Lmax,Nmax)

        REAL A1(Nval),CORP1,CORP2
        INTEGER K,K1,L,N

        REAL COSAR

c reference values for ZH
        DATA A1/.056,.167,.333,.5,.667,.833,.944/

c coefficients for a linear equation yelding
c COSAR as fitting function of SAR
        data polS/0.925418,+0.138776,+0.00582598,-0.0219305,-0.0517991,
     &  +0.0203018,+ 0.000280104,+ 0.000887654,- 0.000445398,
     &  0.955974,+0.0276617,-0.0503592,-0.0639506,+0.0143214,
     &  -0.00556718,+0.00167701,- 0.00141803,+ 0.000579135,
     &  1.33235,- 0.730018,+ 0.149367,-0.132379,+0.120601,-0.0368807,
     &  + 0.0035688,- 0.00424569,+ 0.00147372,
     &  2.51354,-2.43902,+0.675588,-0.262444,+0.309653,-0.0947274,
     &  +0.00709438,-0.00928894,+0.00301579,
     &  3.30061,- 3.66894,+ 1.11037,- 0.348423,+ 0.451721,- 0.144463,
     &  + 0.00935734,- 0.0128111,+ 0.00421033,
     &  3.31818,- 3.71761,+ 1.16,-0.318519,+0.4256,-0.141886,
     &  + 0.008603,- 0.0119864,+ 0.00405297,
     &  3.8108,- 4.16293,+ 1.34391,- 0.32804,+ 0.434989,-0.146962,
     &  + 0.00846599,- 0.0116593,+ 0.00397673/

        IF (SAR.EQ.1.) GO TO 100

c check the position of ZH  in relation to the reference
c values (A1) and calculate the pointer number to search the
c proper coefficients (polS) for the fitting equations
	K=0
	L=0
	N=0
        DO 10 I=1,Nval
	   IF (ZH.NE.A1(I)) GO TO 10
	   K=I
10      CONTINUE

        if (ZH.lt.A1(1))k=1
        if (ZH.gt.A1(Nval))k=Nval

        IF (K.NE.0) N=K
	IF (N.NE.0) GO TO 200

c when ZH is different from  reference values
        CALL SEARCH (A1,ZH,Nval,K1)
        N=K1
        CALL CORSUP (SAR,PAD,polS,Nmax,Lmax,N,CORP1)
        CALL CORSUP (SAR,PAD,polS,Nmax,Lmax,N+1,CORP2)
        CALL LININT (ZH,A1(K1),A1(K1+1),CORP1,CORP2,COSAR)
	GO TO 999

c when ZH is  equal to reference values
200     CALL CORSUP (SAR,PAD,polS,Nmax,Lmax,N,COSAR)
	GO TO 999

c when SAR= 1
100     COSAR = 1.0

999     CONTINUE

	RETURN
	END


        SUBROUTINE HORROOF (ZH,teta,XL,COHOR)
c************************************************************************
c Purpose: this routine calculates the Correction Factor for the
c          reference CP(zh), in relation to the Wind Direction and the
c          relative horizontal position of a facade element
c
c Module : # 4.3.1.2.5, TG#3, mxg/March 1,1989
c Changes: December 1, 1989 (included equation coefficients data block)
c Limits : -90 _< teta _< +90
c
c Pass parameters:
c
c IO # Name    unit         description                        ref.value
c
c I  1 ZH      (-)   Relative vertical position of a           (.5,.7,.9)
c                    facade element
c I  2 teta    (deg) Angle between Wind Direction and normal  (0,10,20,30,40,
c                    to a facade (clockwise, 0 to +90,        50,60,70,80,90,
c                    counterclockwise, 0 to -90)              val. abs.)
c I  4 XL      (-)   Relative horiz. position of a                (0.5)
c                    facade element
c O  5 COHOR   (-)   Correction Factor for CP(zh)                  (1)
c
c example:
c CALL CPHORG (.65, 45., .35, .405199)
c***********************************************************************
        parameter (Nmax=12,Lmax=5,Nval1=3,Nval2=4)
        REAL ZH,polH(lmax,Nmax),XL

        INTEGER K,N,K1,L,L1

        REAL COHOR ,teta
        REAL A1(Nval1),A2(Nval2),CORP1,CORP2,COHOR1,COHOR2

c reference values for ZH
        DATA A1/.07,.5,.93/

c reference values for teta
        DATA A2/0.,30.,60.,90./

c coefficients for 3nd  order polinomial equations
c yelding COHOR as fitting function of XL
        data polH/ 1.27693, - 1.10773, + 1.10773,  0.,0.,
     & 0.453533,+ 1.97823, - 1.77058,  0.,0.,
     & 0.585957,+ 1.41484, - 1.17351, 0.,0.,
     & 0.836749,- 2.87843, + 6.40987, 0.,0.,
     & 0.619135, + 3.589 , -11.7437,  +16.1933,  -8.02991,
     & 2.50939 , -7.98177, + 24.2933, - 43.2933, + 29.1172,
     & 0.349445, + 13.924, - 75.2324,  + 134.636,- 69.3254,
     & 0.423824, + 2.84497, - 18.7613,  + 43.8545, - 26.2048,
     & 1.,0.,0.,0.,0.,
     & 1.837 ,- 3.34801, + 3.34801, 0.,0.,
     & 0.968827,- 0.8766 , + 1.87789, 0.,0.,
     & 0.836749,- 2.87843, + 6.40987,0.,0./

        IF(XL.EQ.0.5)go to 100

c check the position of ZH and TETA in relation to the reference
c values (A1,A2) and calculate the pointer number to search the 
c proper coefficients for the polinomial equation (polH)
	K=0
	L=0
	N=0
        DO 10 I=1,Nval1
           IF (ZH.NE.A1(I)) GO TO 10
	   K=I
10      CONTINUE

        if (ZH.lt.A1(1))k=1
        if (ZH.gt.A1(Nval1)) k=Nval1

        DO 20 J=1,Nval2
           IF (teta.NE.A2(J)) GO TO 20
	   L=J
20      CONTINUE

        IF (K.NE.0.AND.L.NE.0) N=(K-1)*Nval2+L
	IF (N.NE.0) GO TO 200
	IF (K.NE.0) GO TO 300
	IF (L.NE.0) GO TO 400

c when ZH and teta are both different from  reference values
        CALL SEARCH (A1,ZH,Nval1,K1)
        CALL SEARCH (A2,teta,Nval2,L1)

        N=(K1-1)*Nval2+L1

        CALL CORPANS (XL,POLH,Nmax,Lmax,N,CORP1)
        CALL CORPANS (XL,POLH,Nmax,Lmax,N+1,CORP2)
        CALL LININT (teta,A2(L1),A2(L1+1),CORP1,CORP2,COHOR1)

        N=K1*Nval2+L1

        CALL CORPANS (XL,POLH,Nmax,Lmax,N,CORP1)
        CALL CORPANS (XL,POLH,Nmax,Lmax,N+1,CORP2)
        CALL LININT (teta,A2(L1),A2(L1+1),CORP1,CORP2,COHOR2)
        CALL LININT (ZH,A1(K1),A1(K1+1),COHOR1,COHOR2,COHOR)
	GO TO 999

c when only ZH is different from reference value
400     CALL SEARCH (A1,ZH,Nval1,K1)

        N=(K1-1)*Nval2+L

        CALL CORPANS (XL,POLH,Nmax,Lmax,N,CORP1)
        CALL CORPANS (XL,POLH,Nmax,Lmax,N+4,CORP2)
        CALL LININT (ZH,A1(K1),A1(K1+1),CORP1,CORP2,COHOR)
	GO TO 999

c when only teta is different from reference value
300     CALL SEARCH (A2,teta,Nval2,L1)

        N=(K-1)*Nval2+L1

        CALL CORPANS (XL,POLH,Nmax,Lmax,N,CORP1)
        CALL CORPANS (XL,POLH,Nmax,Lmax,N+1,CORP2)
        CALL LININT (teta,A2(L1),A2(L1+1),CORP1,CORP2,COHOR)
	GO TO 999

c when ZH and teta are equal to reference values
200     CALL CORPANS (XL,POLH,Nmax,Lmax,N,COHOR)
	GO TO 999

c when XL = 1 and teta=0.
100     COHOR = 1.0

999     CONTINUE

	RETURN
	END


        SUBROUTINE PHISING (ZH,teta,PHI,COHOR)
c************************************************************************
c Purpose: this routine calculates the Correction Factor for the
c          reference CP(zH), in relation to the ...............
c
c Module : # 4.3.1.2.5, TG#3, mxg/March 1,1989
c Changes: December 1, 1989 (included equation coefficients data block)
c Limits :
c
c Pass parameters:
c
c IO # Name    unit         description                        ref.value
c
c I  1 ZH      (-)   Relative vertical position of a           (.5,.7,.9)
c                    facade element
c I  2 teta    (deg) Angle between Wind Direction and normal  (0,10,20,30,40,
c                    to a facade (clockwise, 0 to +90,        50,60,70,80,90,
c                    counterclockwise, 0 to -90)              val. abs.)
c I  3 PHI      ....................................
c O  4 COHOR   (-)   Correction Factor for CP(zh)                  (1)
c
c example:
c CALL PHIROOF (.65, 45., .35, .405199)
c***********************************************************************
        parameter (Nmax=35,Lmax=4,Nval1=7,Nval2=5)
        REAL ZH,polH(lmax,Nmax),PHI

        INTEGER K,N,K1,L,L1

        REAL COHOR ,teta
        REAL A1(Nval1),A2(Nval2),CORP1,CORP2,COHOR1,COHOR2

c reference values for teta
        DATA A1/0.,30.,60.,90.,120.,150.,180./

c reference values for ZH
        DATA A2/0.07,0.24,0.5,0.73,0.93/

c coefficients for 3nd  order polinomial equations
c yelding COHOR as fitting function of XL

        polH( 1,1)= 1.0000000000
        polH( 2,1)= 0.0231759995
        polH( 3,1)=-0.0040026801
        polH( 4,1)= 0.0000567455
        polH( 1,2)= 1.0000000000
        polH( 2,2)=-0.0761905015
        polH( 3,2)= 0.0012698401
        polH( 4,2)=-0.0000126984
        polH( 1,3)= 1.0000000000
        polH( 2,3)=-0.0744108036
        polH( 3,3)= 0.0006734010
        polH( 4,3)=-0.0000104751
        polH( 1,4)= 1.0000000000
        polH( 2,4)=-0.0450617000
        polH( 3,4)=-0.0017284000
        polH( 4,4)= 0.0000192044
        polH( 1,5)= 1.0000000000
        polH( 2,5)=-0.1203700006
        polH( 3,5)= 0.0046913601
        polH( 4,5)=-0.0000740741
        polH( 1,6)= 1.0000000000
        polH( 2,6)= 0.0124338996
        polH( 3,6)=-0.0034127000
        polH( 4,6)= 0.0000523222
        polH( 1,7)= 1.0000000000
        polH( 2,7)=-0.0632850006
        polH( 3,7)= 0.0004830920
        polH( 4,7)=-0.0000021471
        polH( 1,8)= 1.0000000000
        polH( 2,8)= 0.0283950996
        polH( 3,8)=-0.0056790099
        polH( 4,8)= 0.0000713306
        polH( 1,9)= 1.0000000000
        polH( 2,9)= 0.0163742993
        polH( 3,9)=-0.0039766100
        polH( 4,9)= 0.0000519818
        polH( 1,10)= 1.0000000000
        polH( 2,10)=-0.0058201100
        polH( 3,10)=-0.0016931200
        polH( 4,10)= 0.0000258671
        polH( 1,11)= 1.0000000000
        polH( 2,11)=-0.0855346024
        polH( 3,11)= 0.0020964399
        polH( 4,11)=-0.0000167715
        polH( 1,12)= 1.0000000000
        polH( 2,12)=-0.0296295993
        polH( 3,12)=-0.0014814800
        polH( 4,12)= 0.0000329218
        polH( 1,13)= 1.0000000000
        polH( 2,13)=-0.0530864000
        polH( 3,13)= 0.0009876540
        polH( 4,13)=-0.0000109739
        polH( 1,14)= 1.0000000000
        polH( 2,14)= 0.0093567297
        polH( 3,14)=-0.0016374300
        polH( 4,14)= 0.0000207927
        polH( 1,15)= 1.0000000000
        polH( 2,15)=-0.0083333300
        polH( 3,15)=-0.0006790120
        polH( 4,15)= 0.0000082304
        polH( 1,16)= 1.0000000000
        polH( 2,16)= 0.0440329015
        polH( 3,16)=-0.0008230450
        polH( 4,16)= 0.0000128029
        polH( 1,17)= 1.0000000000
        polH( 2,17)= 0.0164982993
        polH( 3,17)=-0.0001346800
        polH( 4,17)= 0.0000074822
        polH( 1,18)= 1.0000000000
        polH( 2,18)= 0.0198653005
        polH( 3,18)=-0.0004713800
        polH( 4,18)= 0.0000149645
        polH( 1,19)= 1.0000000000
        polH( 2,19)= 0.0043771002
        polH( 3,19)= 0.0000000000
        polH( 4,19)= 0.0000074822
        polH( 1,20)= 1.0000000000
        polH( 2,20)= 0.0069958800
        polH( 3,20)= 0.0000000000
        polH( 4,20)= 0.0000018290
        polH( 1,21)= 1.0000000000
        polH( 2,21)=-0.0021604900
        polH( 3,21)= 0.0014814800
        polH( 4,21)=-0.0000233196
        polH( 1,22)= 1.0000000000
        polH( 2,22)= 0.1029239967
        polH( 3,22)=-0.0009356730
        polH( 4,22)=-0.0000051982
        polH( 1,23)= 1.0000000000
        polH( 2,23)= 0.1154320017
        polH( 3,23)=-0.0012345701
        polH( 4,23)=-0.0000027435
        polH( 1,24)= 1.0000000000
        polH( 2,24)= 0.0052910098
        polH( 3,24)= 0.0038095200
        polH( 4,24)=-0.0000658436
        polH( 1,25)= 1.0000000000
        polH( 2,25)= 0.0754716992
        polH( 3,25)=-0.0032285100
        polH( 4,25)= 0.0000363382
        polH( 1,26)= 1.0000000000
        polH( 2,26)=-0.1851850003
        polH( 3,26)= 0.0139683001
        polH( 4,26)=-0.0001928280
        polH( 1,27)= 1.0000000000
        polH( 2,27)=-0.0479531996
        polH( 3,27)= 0.0069005801
        polH( 4,27)=-0.0001065630
        polH( 1,28)= 1.0000000000
        polH( 2,28)= 0.1339509934
        polH( 3,28)=-0.0028395101
        polH( 4,28)= 0.0000219479
        polH( 1,29)= 1.0000000000
        polH( 2,29)= 0.1707729995
        polH( 3,29)=-0.0089855101
        polH( 4,29)= 0.0001170160
        polH( 1,30)= 1.0000000000
        polH( 2,30)= 0.0755290985
        polH( 3,30)=-0.0049470901
        polH( 4,30)= 0.0000681952
        polH( 1,31)= 1.0000000000
        polH( 2,31)= 0.2006170005
        polH( 3,31)=-0.0043209898
        polH( 4,31)= 0.0000219479
        polH( 1,32)= 1.0000000000
        polH( 2,32)= 0.2672840059
        polH( 3,32)=-0.0102468999
        polH( 4,32)= 0.0001207130
        polH( 1,33)= 1.0000000000
        polH( 2,33)= 0.1659930050
        polH( 3,33)=-0.0080808103
        polH( 4,33)= 0.0001062480
        polH( 1,34)= 1.0000000000
        polH( 2,34)= 0.0212698001
        polH( 3,34)=-0.0020952399
        polH( 4,34)= 0.0000324515
        polH( 1,35)= 1.0000000000
        polH( 2,35)=-0.0012048200
        polH( 3,35)=-0.0010709499
        polH( 4,35)= 0.0000196341

        IF(PHI.EQ.0.)go to 100

c calculate COHOR

c check the position of TETA and ZH in relation to the reference
c values (A1,A2) and calculate the pointer number to search the 
c proper coefficients for the polinomial equation (polH)
	K=0
	L=0
	N=0
        DO 10 I=1,Nval1
           IF (teta.NE.A1(I)) GO TO 10
	   K=I
10      CONTINUE

        DO 20 J=1,Nval2
           IF (ZH.NE.A2(J)) GO TO 20
	   L=J
20      CONTINUE
        if (ZH.lt.A2(1))L=1
        if (ZH.gt.A2(Nval2))L=Nval2

        IF (K.NE.0.AND.L.NE.0) N=(K-1)*Nval2+L
	IF (N.NE.0) GO TO 200
	IF (K.NE.0) GO TO 300
	IF (L.NE.0) GO TO 400

c when ZH and teta are both different from  reference values
        CALL SEARCH (A1,teta,Nval1,K1)
        CALL SEARCH (A2,ZH,Nval2,L1)

        N=(K1-1)*Nval2+L1

        CALL CORPANS (PHI,POLH,Nmax,Lmax,N,CORP1)
        CALL CORPANS (PHI,POLH,Nmax,Lmax,N+1,CORP2)
        CALL LININT (ZH,A2(L1),A2(L1+1),CORP1,CORP2,COHOR1)

        N=K1*Nval2+L1

        CALL CORPANS (PHI,POLH,Nmax,Lmax,N,CORP1)
        CALL CORPANS (PHI,POLH,Nmax,Lmax,N+1,CORP2)
        CALL LININT (ZH,A2(L1),A2(L1+1),CORP1,CORP2,COHOR2)
        CALL LININT (teta,A1(K1),A1(K1+1),COHOR1,COHOR2,COHOR)
	GO TO 999

c when only teta is different from reference value
400     CALL SEARCH (A1,teta,Nval1,K1)

        N=(K1-1)*Nval2+L

        CALL CORPANS (PHI,POLH,Nmax,Lmax,N,CORP1)
        CALL CORPANS (PHI,POLH,Nmax,Lmax,N+4,CORP2)
        CALL LININT (teta,A1(K1),A1(K1+1),CORP1,CORP2,COHOR)
	GO TO 999

c when only ZH is different from reference value
300     CALL SEARCH (A2,ZH,Nval2,L1)

        N=(K-1)*Nval2+L1

        CALL CORPANS (PHI,POLH,Nmax,Lmax,N,CORP1)
        CALL CORPANS (PHI,POLH,Nmax,Lmax,N+1,CORP2)
        CALL LININT (ZH,A2(L1),A2(L1+1),CORP1,CORP2,COHOR)
	GO TO 999

c when ZH and teta are equal to reference values
200     CALL CORPANS (PHI,POLH,Nmax,Lmax,N,COHOR)
	GO TO 999

c when PHI = 1
100     COHOR = 1.0

999     CONTINUE
	RETURN
	END


        SUBROUTINE PHIXSIN(ZH,teta,PHI,XL,COHOR)
c************************************************************************
c Purpose: this routine calculates the Correction Factor for the
c          reference CP(zH), in relation to the ...............
c
c Module : # 4.3.1.2.5, TG#3, mxg/March 1,1989
c Changes: December 1, 1989 (included equation coefficients data block)
c Limits :
c
c Pass parameters:
c
c IO # Name    unit         description                        ref.value
c
c I  1 ZH      (-)   Relative vertical position of a           (.07,.88)
c                    facade element
c I  2 teta    (deg) Angle between Wind Direction and normal  (0,30,60,90,
c                    to a facade (clockwise, 0 to +90,        120,150,180,
c                    counterclockwise, 0 to -90)              val. abs.)
c I  3 PHI     (deg) Roof tilt angle                           (0.)
c O  4 COHOR   (-)   Correction Factor for CP(zh)                  (1)
c
c example:
c CALL PHIROOF (.65, 45., .35, .405199)
c***********************************************************************
        parameter (Nmax=14,Lmax=9,Nval1=7,Nval2=2)
        REAL ZH,polH(lmax,Nmax),PHI,XL

        INTEGER K,N,K1,L,L1

        REAL COHOR ,teta
        REAL A1(Nval1),A2(Nval2),CORP1,CORP2,COHOR1,COHOR2
c reference values for teta
        DATA A1/0.,30.,60.,90.,120.,150.,180./

c reference values for ZH
        DATA A2/0.07,0.88/

c coefficients for 2nd  order polinomial equations
c yelding COHOR as fitting function of XL and PHI

        polH( 1,1)= 1.3299499750
        polH( 2,1)=-0.0521104001
        polH( 3,1)= 0.0002244060
        polH( 4,1)=-1.0785499811
        polH( 5,1)= 0.0635572001
        polH( 6,1)=-0.0016569600
        polH( 7,1)= 1.0833699703
        polH( 8,1)=-0.0665663034
        polH( 9,1)= 0.0017687600
        polH( 1,2)= 0.9394860268
        polH( 2,2)=-0.0479598008
        polH( 3,2)=-0.0000974832
        polH( 4,2)=-0.0983475000
        polH( 5,2)= 0.0366907008
        polH( 6,2)=-0.0012927901
        polH( 7,2)= 0.1136759967
        polH( 8,2)=-0.0462572016
        polH( 9,2)= 0.0016482000
        polH( 1,3)= 0.4876909852
        polH( 2,3)=-0.0019343200
        polH( 3,3)=-0.0002649470
        polH( 4,3)= 2.0316700935
        polH( 5,3)=-0.1657000035
        polH( 6,3)= 0.0022320901
        polH( 7,3)=-1.7756600380
        polH( 8,3)= 0.1629370004
        polH( 9,3)=-0.0027908499
        polH( 1,4)= 1.8550299406
        polH( 2,4)=-0.0200303998
        polH( 3,4)=-0.0003794550
        polH( 4,4)=-3.3266398907
        polH( 5,4)=-0.0507206991
        polH( 6,4)= 0.0034892899
        polH( 7,4)= 3.3515899181
        polH( 8,4)= 0.0464839004
        polH( 9,4)=-0.0051782401
        polH( 1,5)= 0.5894029737
        polH( 2,5)=-0.0221960004
        polH( 3,5)= 0.0004028630
        polH( 4,5)= 1.3389500380
        polH( 5,5)=-0.1696889997
        polH( 6,5)= 0.0017365200
        polH( 7,5)=-1.1058800220
        polH( 8,5)= 0.1574490070
        polH( 9,5)=-0.0012419800
        polH( 1,6)= 0.9489070177
        polH( 2,6)=-0.0213818997
        polH( 3,6)= 0.0004373320
        polH( 4,6)=-0.7794560194
        polH( 5,6)= 0.0078591499
        polH( 6,6)=-0.0019059100
        polH( 7,6)= 1.7936600447
        polH( 8,6)= 0.0018974000
        polH( 9,6)= 0.0015644600
        polH( 1,7)= 0.8629009724
        polH( 2,7)=-0.0452040993
        polH( 3,7)= 0.0008891040
        polH( 4,7)=-2.8666100502
        polH( 5,7)= 0.3019880056
        polH( 6,7)=-0.0033496199
        polH( 7,7)= 6.3300700188
        polH( 8,7)=-0.2996569872
        polH( 9,7)= 0.0032523400
        polH( 1,8)= 0.8913229704
        polH( 2,8)=-0.0073728100
        polH( 3,8)= 0.0001081340
        polH( 4,8)=-3.0564200878
        polH( 5,8)= 0.0513649993
        polH( 6,8)=-0.0010348100
        polH( 7,8)= 6.5691199303
        polH( 8,8)=-0.0547787994
        polH( 9,8)= 0.0021771900
        polH( 1,9)= 1.8182899952
        polH( 2,9)=-0.0537437983
        polH( 3,9)= 0.0019260800
        polH( 4,9)=-3.3361101151
        polH( 5,9)= 0.3345060050
        polH( 6,9)=-0.0061272401
        polH( 7,9)= 3.3825500011
        polH( 8,9)=-0.1008239985
        polH( 9,9)= 0.0007790100
        polH( 1,10)= 0.3514550030
        polH( 2,10)= 0.0266059004
        polH( 3,10)=-0.0004506370
        polH( 4,10)= 3.0474998951
        polH( 5,10)=-0.4486970007
        polH( 6,10)= 0.0087052304
        polH( 7,10)=-2.3812398911
        polH( 8,10)= 0.3783850074
        polH( 9,10)=-0.0077826600
        polH( 1,11)= 0.8840649724
        polH( 2,11)= 0.0302392002
        polH( 3,11)=-0.0001296330
        polH( 4,11)=-1.1122499704
        polH( 5,11)=-0.1233889982
        polH( 6,11)= 0.0034093501
        polH( 7,11)= 2.1049599648
        polH( 8,11)= 0.0594770014
        polH( 9,11)=-0.0024119399
        polH( 1,12)= 0.5441229939
        polH( 2,12)= 0.0771035030
        polH( 3,12)=-0.0015281400
        polH( 4,12)= 3.0628700256
        polH( 5,12)=-0.5186100006
        polH( 6,12)= 0.0098380903
        polH( 7,12)=-2.3775799274
        polH( 8,12)= 0.4188140035
        polH( 9,12)=-0.0083423899
        polH( 1,13)= 0.9878439903
        polH( 2,13)= 0.0834060013
        polH( 3,13)=-0.0004880570
        polH( 4,13)= 0.1300020069
        polH( 5,13)= 0.3806209862
        polH( 6,13)=-0.0095770601
        polH( 7,13)=-0.1224009991
        polH( 8,13)=-0.3853650093
        polH( 9,13)= 0.0097532999
        polH( 1,14)= 1.2780100107
        polH( 2,14)=-0.0393879004
        polH( 3,14)= 0.0005226020
        polH( 4,14)=-1.0300400257
        polH( 5,14)= 0.0693849027
        polH( 6,14)=-0.0010656300
        polH( 7,14)= 1.0301899910
        polH( 8,14)=-0.0694777966
        polH( 9,14)= 0.0010690800

        IF(PHI.EQ.0.)go to 100

c calculate COHOR

c check the position of TETA and ZH in relation to the reference
c values (A1,A2) and calculate the pointer number to search the 
c proper coefficients for the polinomial equation (polH)
	K=0
	L=0
	N=0
        DO 10 I=1,Nval1
           IF (teta.NE.A1(I)) GO TO 10
	   K=I
10      CONTINUE

        DO 20 J=1,Nval2
           IF (ZH.NE.A2(J)) GO TO 20
	   L=J
20      CONTINUE
        if (ZH.lt.A2(1))L=1
        if (ZH.gt.A2(Nval2))L=Nval2

        IF (K.NE.0.AND.L.NE.0) N=(K-1)*Nval2+L
	IF (N.NE.0) GO TO 200
	IF (K.NE.0) GO TO 300
	IF (L.NE.0) GO TO 400

c when ZH and teta are both different from  reference values
        CALL SEARCH (A1,teta,Nval1,K1)
        CALL SEARCH (A2,ZH,Nval2,L1)

        N=(K1-1)*Nval2+L1

        CALL CORSUP (PHI,XL,POLH,Nmax,Lmax,N,CORP1)
        CALL CORSUP (PHI,XL,POLH,Nmax,Lmax,N+1,CORP2)
        CALL LININT (ZH,A2(L1),A2(L1+1),CORP1,CORP2,COHOR1)

        N=K1*Nval2+L1

        CALL CORSUP (PHI,XL,POLH,Nmax,Lmax,N,CORP1)
        CALL CORSUP (PHI,XL,POLH,Nmax,Lmax,N+1,CORP2)
        CALL LININT (ZH,A2(L1),A2(L1+1),CORP1,CORP2,COHOR2)
        CALL LININT (teta,A1(K1),A1(K1+1),COHOR1,COHOR2,COHOR)
	GO TO 999

c when only teta is different from reference value
400     CALL SEARCH (A1,teta,Nval1,K1)

        N=(K1-1)*Nval2+L

        CALL CORSUP (PHI,XL,POLH,Nmax,Lmax,N,CORP1)
        CALL CORSUP (PHI,XL,POLH,Nmax,Lmax,N+4,CORP2)
        CALL LININT (teta,A1(K1),A1(K1+1),CORP1,CORP2,COHOR)
	GO TO 999

c when only ZH is different from reference value
300     CALL SEARCH (A2,ZH,Nval2,L1)

        N=(K-1)*Nval2+L1

        CALL CORSUP (PHI,XL,POLH,Nmax,Lmax,N,CORP1)
        CALL CORSUP (PHI,XL,POLH,Nmax,Lmax,N+1,CORP2)
        CALL LININT (ZH,A2(L1),A2(L1+1),CORP1,CORP2,COHOR)
	GO TO 999

c when ZH and teta are equal to reference values
200     CALL CORSUP (PHI,XL,POLH,Nmax,Lmax,N,COHOR)
	GO TO 999

c when PHI = 1
100     COHOR = 1.0

999     CONTINUE
	RETURN
	END


        SUBROUTINE PHIDOUB (ZH,teta,PHI,COHOR)
c************************************************************************
c Purpose: this routine calculates the Correction Factor for the
c          reference CP(zH), in relation to the ...............
c
c Module : # 4.3.1.2.5, TG#3, mxg/March 1,1989
c Changes: December 1, 1989 (included equation coefficients data block)
c Limits :
c
c Pass parameters:
c
c IO # Name    unit         description                        ref.value
c
c I  1 ZH      (-)   Relative vertical position of a           (.5,.7,.9)
c                    facade element
c I  2 teta    (deg) Angle between Wind Direction and normal  (0,10,20,30,40,
c                    to a facade (clockwise, 0 to +90,        50,60,70,80,90,
c                    counterclockwise, 0 to -90)              val. abs.)
c I  3 PHI      ....................................
c O  4 COPHI   (-)   Correction Factor for CP(zh)                  (1)
c
c example:
c CALL PHIROOF (.65, 45., .35, .405199)
c***********************************************************************
        parameter (Nmax=24,Lmax=4,Nval1=4,Nval2=6)
        REAL ZH,polH(lmax,Nmax),PHI

        INTEGER K,N,K1,L,L1

        REAL COHOR ,teta
        REAL A1(Nval1),A2(Nval2),CORP1,CORP2,COHOR1,COHOR2
c reference values for teta
        DATA A1/0.,30.,60.,90./

c reference values for ZH
        DATA A2/0.06,0.25,0.45,0.55,0.73,0.93/

c coefficients for 3nd  order polinomial equations
c yelding COHOR as fitting function of XL

        polH( 1,1)= 1.0000000000
        polH( 2,1)= 0.0704149976
        polH( 3,1)=-0.0048460499
        polH( 4,1)= 0.0000529526
        polH( 1,2)= 1.0000000000
        polH( 2,2)=-0.0212698001
        polH( 3,2)=-0.0017936500
        polH( 4,2)= 0.0000320988
        polH( 1,3)= 1.0000000000
        polH( 2,3)= 0.1727270037
        polH( 3,3)=-0.0123905996
        polH( 4,3)= 0.0001840630
        polH( 1,4)= 1.0000000000
        polH( 2,4)= 0.2198649943
        polH( 3,4)=-0.0126598999
        polH( 4,4)= 0.0001720910
        polH( 1,5)= 1.0000000000
        polH( 2,5)= 0.1512349993
        polH( 3,5)=-0.0070370398
        polH( 4,5)= 0.0000932785
        polH( 1,6)= 1.0000000000
        polH( 2,6)=-0.0401234999
        polH( 3,6)= 0.0043209898
        polH( 4,6)=-0.0000603567
        polH( 1,7)= 1.0000000000
        polH( 2,7)= 0.0248677004
        polH( 3,7)=-0.0031746000
        polH( 4,7)= 0.0000411523
        polH( 1,8)= 1.0000000000
        polH( 2,8)= 0.0012077300
        polH( 3,8)=-0.0027053100
        polH( 4,8)= 0.0000397209
        polH( 1,9)= 1.0000000000
        polH( 2,9)= 0.2802470028
        polH( 3,9)=-0.0153085999
        polH( 4,9)= 0.0001865570
        polH( 1,10)= 1.0000000000
        polH( 2,10)= 0.6271600127
        polH( 3,10)=-0.0327159986
        polH( 4,10)= 0.0004307270
        polH( 1,11)= 1.0000000000
        polH( 2,11)=-0.0543860011
        polH( 3,11)= 0.0049122800
        polH( 4,11)=-0.0000701754
        polH( 1,12)= 1.0000000000
        polH( 2,12)=-0.1132280007
        polH( 3,12)= 0.0085714301
        polH( 4,12)=-0.0001246330
        polH( 1,13)= 1.0000000000
        polH( 2,13)=-0.0153040001
        polH( 3,13)=-0.0012159300
        polH( 4,13)= 0.0000204985
        polH( 1,14)= 1.0000000000
        polH( 2,14)=-0.0671958029
        polH( 3,14)= 0.0019047600
        polH( 4,14)=-0.0000258671
        polH( 1,15)= 1.0000000000
        polH( 2,15)= 0.0808641985
        polH( 3,15)=-0.0033333299
        polH( 4,15)= 0.0000274348
        polH( 1,16)= 1.0000000000
        polH( 2,16)= 0.0969135985
        polH( 3,16)= 0.0006172840
        polH( 4,16)=-0.0000438957
        polH( 1,17)= 1.0000000000
        polH( 2,17)= 0.1011700034
        polH( 3,17)=-0.0023391801
        polH( 4,17)= 0.0000181936
        polH( 1,18)= 1.0000000000
        polH( 2,18)= 0.0317901000
        polH( 3,18)=-0.0004938270
        polH( 4,18)=-0.0000013717
        polH( 1,19)= 1.0000000000
        polH( 2,19)=-0.0020576101
        polH( 3,19)= 0.0009876540
        polH( 4,19)=-0.0000128029
        polH( 1,20)= 1.0000000000
        polH( 2,20)=-0.0239057001
        polH( 3,20)= 0.0016161600
        polH( 4,20)=-0.0000194538
        polH( 1,21)= 1.0000000000
        polH( 2,21)=-0.0124578997
        polH( 3,21)= 0.0006734010
        polH( 4,21)=-0.0000074822
        polH( 1,22)= 1.0000000000
        polH( 2,22)=-0.0124578997
        polH( 3,22)= 0.0006734010
        polH( 4,22)=-0.0000074822
        polH( 1,23)= 1.0000000000
        polH( 2,23)=-0.0239057001
        polH( 3,23)= 0.0016161600
        polH( 4,23)=-0.0000194538
        polH( 1,24)= 1.0000000000
        polH( 2,24)=-0.0020576101
        polH( 3,24)= 0.0009876540
        polH( 4,24)=-0.0000128029

        IF(PHI.EQ.0.)go to 100

c calculate COHOR

c check the position of TETA and ZH in relation to the reference
c values (A1,A2) and calculate the pointer number to search the 
c proper coefficients for the polinomial equation (polH)
	K=0
	L=0
	N=0
        DO 10 I=1,Nval1
           IF (teta.NE.A1(I)) GO TO 10
	   K=I
10      CONTINUE

        DO 20 J=1,Nval2
           IF (ZH.NE.A2(J)) GO TO 20
	   L=J
20      CONTINUE
        if (ZH.lt.A2(1))L=1
        if (ZH.gt.A2(Nval2))L=Nval2

        IF (K.NE.0.AND.L.NE.0) N=(K-1)*Nval2+L
	IF (N.NE.0) GO TO 200
	IF (K.NE.0) GO TO 300
	IF (L.NE.0) GO TO 400

c when ZH and teta are both different from  reference values
        CALL SEARCH (A1,teta,Nval1,K1)
        CALL SEARCH (A2,ZH,Nval2,L1)

        N=(K1-1)*Nval2+L1

        CALL CORPANS (PHI,POLH,Nmax,Lmax,N,CORP1)
        CALL CORPANS (PHI,POLH,Nmax,Lmax,N+1,CORP2)
        CALL LININT (ZH,A2(L1),A2(L1+1),CORP1,CORP2,COHOR1)

        N=K1*Nval2+L1

        CALL CORPANS (PHI,POLH,Nmax,Lmax,N,CORP1)
        CALL CORPANS (PHI,POLH,Nmax,Lmax,N+1,CORP2)
        CALL LININT (ZH,A2(L1),A2(L1+1),CORP1,CORP2,COHOR2)
        CALL LININT (teta,A1(K1),A1(K1+1),COHOR1,COHOR2,COHOR)
	GO TO 999

c when only teta is different from reference value
400     CALL SEARCH (A1,teta,Nval1,K1)

        N=(K1-1)*Nval2+L

        CALL CORPANS (PHI,POLH,Nmax,Lmax,N,CORP1)
        CALL CORPANS (PHI,POLH,Nmax,Lmax,N+4,CORP2)
        CALL LININT (teta,A1(K1),A1(K1+1),CORP1,CORP2,COHOR)
	GO TO 999

c when only ZH is different from reference value
300     CALL SEARCH (A2,ZH,Nval2,L1)

        N=(K-1)*Nval2+L1

        CALL CORPANS (PHI,POLH,Nmax,Lmax,N,CORP1)
        CALL CORPANS (PHI,POLH,Nmax,Lmax,N+1,CORP2)
        CALL LININT (ZH,A2(L1),A2(L1+1),CORP1,CORP2,COHOR)
	GO TO 999

c when ZH and teta are equal to reference values
200     CALL CORPANS (PHI,POLH,Nmax,Lmax,N,COHOR)
	GO TO 999

c when PHI = 1
100     COHOR = 1.0

999     CONTINUE
	RETURN
	END


        SUBROUTINE PHIXDOU(ZH,teta,PHI,XL,COHOR)
c************************************************************************
c Purpose: this routine calculates the Correction Factor for the
c          reference CP(zH), in relation to the ...............
c
c Module : # 4.3.1.2.5, TG#3, mxg/March 1,1989
c Changes: December 1, 1989 (included equation coefficients data block)
c Limits :
c
c Pass parameters:
c
c IO # Name    unit         description                        ref.value
c
c I  1 ZH      (-)   Relative vertical position of a        (.07,.43,.57,.93)
c                    facade element
c I  2 teta    (deg) Angle between Wind Direction and normal  (0,30,60,90,
c                    to a facade (clockwise, 0 to +90,        120,150,180,
c                    counterclockwise, 0 to -90)              val. abs.)
c I  3 PHI     (deg) Roof tilt angle                          (0.)
c O  4 COHOR   (-)   Correction Factor for CP(zh)                  (1)
c
c example:
c CALL PHIROOF (.65, 45., .35, .405199)
c***********************************************************************
        parameter (Nmax=16,Lmax=9,Nval1=4,Nval2=4)
        REAL ZH,polH(lmax,Nmax),PHI,XL

        INTEGER K,N,K1,L,L1

        REAL COHOR ,teta
        REAL A1(Nval1),A2(Nval2),CORP1,CORP2,COHOR1,COHOR2
c reference values for teta
        DATA A1/0.,30.,60.,90./

c reference values for ZH
        DATA A2/0.07,0.43,0.57,0.93/

c coefficients for 2nd  order polinomial equations
c yelding COHOR as fitting function of XL and PHI

        polH( 1,1)= 1.3639400005
        polH( 2,1)=-0.0360274985
        polH( 3,1)=-0.0000952431
        polH( 4,1)=-1.2413100004
        polH( 5,1)= 0.2017810047
        polH( 6,1)=-0.0047060400
        polH( 7,1)= 1.2413100004
        polH( 8,1)=-0.2017810047
        polH( 9,1)= 0.0047060400
        polH( 1,2)= 0.8217110038
        polH( 2,2)= 0.0261547994
        polH( 3,2)=-0.0012410200
        polH( 4,2)= 1.4586100578
        polH( 5,2)=-0.1922959983
        polH( 6,2)= 0.0050987601
        polH( 7,2)=-1.4586100578
        polH( 8,2)= 0.1922959983
        polH( 9,2)=-0.0050987601
        polH( 1,3)= 0.9259300232
        polH( 2,3)= 0.0611222982
        polH( 3,3)=-0.0014566800
        polH( 4,3)= 0.9985330105
        polH( 5,3)=-0.0949708000
        polH( 6,3)= 0.0016907901
        polH( 7,3)=-1.0038199425
        polH( 8,3)= 0.0969675034
        polH( 9,3)=-0.0017299399
        polH( 1,4)= 0.9125980139
        polH( 2,4)= 0.0394780003
        polH( 3,4)=-0.0003373190
        polH( 4,4)= 0.1051620021
        polH( 5,4)=-0.0630972013
        polH( 6,4)= 0.0023369300
        polH( 7,4)=-0.1051620021
        polH( 8,4)= 0.0630972013
        polH( 9,4)=-0.0023369300
        polH( 1,5)= 0.4726170003
        polH( 2,5)=-0.0024290299
        polH( 3,5)=-0.0001811360
        polH( 4,5)= 2.1087000370
        polH( 5,5)=-0.0031484200
        polH( 6,5)=-0.0017548500
        polH( 7,5)=-1.9412000179
        polH( 8,5)=-0.0585901998
        polH( 9,5)= 0.0026469301
        polH( 1,6)= 2.5627501011
        polH( 2,6)=-0.0576535985
        polH( 3,6)= 0.0004922250
        polH( 4,6)=-6.9166498184
        polH( 5,6)= 0.6442919970
        polH( 6,6)=-0.0122589003
        polH( 7,6)= 8.3378400803
        polH( 8,6)=-0.7261180282
        polH( 9,6)= 0.0116846999
        polH( 1,7)= 2.8240599632
        polH( 2,7)= 0.0521201007
        polH( 3,7)=-0.0016046700
        polH( 4,7)=-6.0847501755
        polH( 5,7)= 0.6795210242
        polH( 6,7)=-0.0126711996
        polH( 7,7)= 6.6176900864
        polH( 8,7)=-0.8808569908
        polH( 9,7)= 0.0171930995
        polH( 1,8)= 1.9257600307
        polH( 2,8)= 0.0284158997
        polH( 3,8)=-0.0004786800
        polH( 4,8)=-4.2382597923
        polH( 5,8)=-0.0654020980
        polH( 6,8)= 0.0033494399
        polH( 7,8)= 4.2687201500
        polH( 8,8)= 0.0914264992
        polH( 9,8)=-0.0041492502
        polH( 1,9)= 0.5782030225
        polH( 2,9)=-0.0030132600
        polH( 3,9)= 0.0000234361
        polH( 4,9)= 1.5343600512
        polH( 5,9)=-0.1164920032
        polH( 6,9)= 0.0002602120
        polH( 7,9)=-1.2985099554
        polH( 8,9)= 0.0971117988
        polH( 9,9)= 0.0000566911
        polH( 1,10)= 1.4720100164
        polH( 2,10)=-0.0083959298
        polH( 3,10)= 0.0005733500
        polH( 4,10)=-5.9888200760
        polH( 5,10)= 0.2336699963
        polH( 6,10)=-0.0057788999
        polH( 7,10)=10.2006998062
        polH( 8,10)=-0.2263489962
        polH( 9,10)= 0.0033384699
        polH( 1,11)= 1.7065199614
        polH( 2,11)= 0.1242159978
        polH( 3,11)=-0.0025067599
        polH( 4,11)=-6.8313999176
        polH( 5,11)= 0.4866740108
        polH( 6,11)=-0.0066051702
        polH( 7,11)=10.6589002609
        polH( 8,11)=-0.8968809843
        polH( 9,11)= 0.0138547001
        polH( 1,12)= 1.0705000162
        polH( 2,12)= 0.1040600017
        polH( 3,12)=-0.0022556600
        polH( 4,12)=-1.1015000343
        polH( 5,12)=-0.1028280035
        polH( 6,12)= 0.0030880901
        polH( 7,12)= 1.9154499769
        polH( 8,12)=-0.0776195005
        polH( 9,12)= 0.0005007710
        polH( 1,13)= 0.3940489888
        polH( 2,13)= 0.1497389972
        polH( 3,13)=-0.0031209800
        polH( 4,13)=-1.5707700253
        polH( 5,13)=-0.3751679957
        polH( 6,13)= 0.0097717196
        polH( 7,13)= 5.5134902000
        polH( 8,13)= 0.1973039955
        polH( 9,13)=-0.0065656700
        polH( 1,14)= 0.2120060027
        polH( 2,14)= 0.1302330047
        polH( 3,14)=-0.0026091100
        polH( 4,14)= 0.4525380135
        polH( 5,14)=-0.4281179905
        polH( 6,14)= 0.0088782199
        polH( 7,14)= 2.2165999413
        polH( 8,14)= 0.3171249926
        polH( 9,14)=-0.0066466001
        polH( 1,15)= 0.2120060027
        polH( 2,15)= 0.1302330047
        polH( 3,15)=-0.0026091100
        polH( 4,15)= 0.4525380135
        polH( 5,15)=-0.4281179905
        polH( 6,15)= 0.0088782199
        polH( 7,15)= 2.2165999413
        polH( 8,15)= 0.3171249926
        polH( 9,15)=-0.0066466001
        polH( 1,16)= 0.3940489888
        polH( 2,16)= 0.1497389972
        polH( 3,16)=-0.0031209800
        polH( 4,16)=-1.5707700253
        polH( 5,16)=-0.3751679957
        polH( 6,16)= 0.0097717196
        polH( 7,16)= 5.5134902000
        polH( 8,16)= 0.1973039955
        polH( 9,16)=-0.0065656700

        IF(PHI.EQ.0.)go to 100

c calculate COHOR

c check the position of TETA and ZH in relation to the reference
c values (A1,A2) and calculate the pointer number to search the 
c proper coefficients for the polinomial equation (polH)
	K=0
	L=0
	N=0
        DO 10 I=1,Nval1
           IF (teta.NE.A1(I)) GO TO 10
	   K=I
10      CONTINUE

        DO 20 J=1,Nval2
           IF (ZH.NE.A2(J)) GO TO 20
	   L=J
20      CONTINUE
        if (ZH.lt.A2(1))L=1
        if (ZH.gt.A2(Nval2)) L=Nval2

        IF (K.NE.0.AND.L.NE.0) N=(K-1)*Nval2+L
	IF (N.NE.0) GO TO 200
	IF (K.NE.0) GO TO 300
	IF (L.NE.0) GO TO 400

c when ZH and teta are both different from  reference values
        CALL SEARCH (A1,teta,Nval1,K1)
        CALL SEARCH (A2,ZH,Nval2,L1)

        N=(K1-1)*Nval2+L1

        CALL CORSUP (PHI,XL,POLH,Nmax,Lmax,N,CORP1)
        CALL CORSUP (PHI,XL,POLH,Nmax,Lmax,N+1,CORP2)
        CALL LININT (ZH,A2(L1),A2(L1+1),CORP1,CORP2,COHOR1)

        N=K1*Nval2+L1

        CALL CORSUP (PHI,XL,POLH,Nmax,Lmax,N,CORP1)
        CALL CORSUP (PHI,XL,POLH,Nmax,Lmax,N+1,CORP2)
        CALL LININT (ZH,A2(L1),A2(L1+1),CORP1,CORP2,COHOR2)
        CALL LININT (teta,A1(K1),A1(K1+1),COHOR1,COHOR2,COHOR)
	GO TO 999

c when only teta is different from reference value
400     CALL SEARCH (A1,teta,Nval1,K1)

        N=(K1-1)*Nval2+L

        CALL CORSUP (PHI,XL,POLH,Nmax,Lmax,N,CORP1)
        CALL CORSUP (PHI,XL,POLH,Nmax,Lmax,N+4,CORP2)
        CALL LININT (teta,A1(K1),A1(K1+1),CORP1,CORP2,COHOR)
	GO TO 999

c when only ZH is different from reference value
300     CALL SEARCH (A2,ZH,Nval2,L1)

        N=(K-1)*Nval2+L1

        CALL CORSUP (PHI,XL,POLH,Nmax,Lmax,N,CORP1)
        CALL CORSUP (PHI,XL,POLH,Nmax,Lmax,N+1,CORP2)
        CALL LININT (ZH,A2(L1),A2(L1+1),CORP1,CORP2,COHOR)
	GO TO 999

c when ZH and teta are equal to reference values
200     CALL CORSUP (PHI,XL,POLH,Nmax,Lmax,N,COHOR)
	GO TO 999

c when PHI = 1
100     COHOR = 1.0

999     CONTINUE
	RETURN
	END
