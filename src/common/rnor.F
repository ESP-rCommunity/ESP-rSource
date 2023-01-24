      REAL FUNCTION RNOR()
C***BEGIN PROLOGUE  RNOR
C***DATE WRITTEN   810915 (YYMMDD)
C***REVISION DATE  870419 (YYMMDD)
C***CATEGORY NO.  L6A14
C***KEYWORDS  RANDOM NUMBERS, NORMAL DEVIATES
C***AUTHOR    KAHANER, DAVID, SCIENTIFIC COMPUTING DIVISION, NBS
C             MARSAGLIA, GEORGE, SUPERCOMPUTER RES. INST., FLORIDA ST. U.
C
C***PURPOSE  GENERATES NORMAL RANDOM NUMBERS, WITH MEAN ZERO AND
C             UNIT STANDARD DEVIATION, OFTEN DENOTED N(0,1).
C***DESCRIPTION
C
C       RNOR generates normal random numbers with zero mean and
C       unit standard deviation, often denoted N(0,1).
C           From the book, "Numerical Methods and Software" by
C                D. Kahaner, C. Moler, S. Nash
C                Prentice Hall, 1988
C   Use
C       First time....
C                   Z = RSTART(ISEED)
C                     Here ISEED is any  n o n - z e r o  integer.
C                     This causes initialization of the program.
C                     RSTART returns a real (single precision) echo of ISEED.
C
C       Subsequent times...
C                   Z = RNOR()
C                     Causes the next real (single precision) random number
C                           to be returned as Z.
C
C.....................................................................
C                 Typical usage
C
C                    REAL RSTART,RNOR,Z
C                    INTEGER ISEED,I
C                    ISEED = 305
C                    Z = RSTART(ISEED)
C                    DO 1 I = 1,10
C                       Z = RNOR()
C                       WRITE(*,*) Z
C                 1  CONTINUE
C                    END
C
C
C***REFERENCES  MARSAGLIA & TSANG, "A FAST, EASILY IMPLEMENTED
C                 METHOD FOR SAMPLING FROM DECREASING OR
C                 SYMMETRIC UNIMODAL DENSITY FUNCTIONS", TO BE
C                 PUBLISHED IN SIAM J SISC 1983.
C***ROUTINES CALLED  (NONE)
C***END PROLOGUE  RNOR
      REAL AA,B,C,C1,C2,PC,X,Y,XN,V(65),RSTART,U(17),S,T,UN
      INTEGER J,IA,IB,IC,II,JJ,ID,III,JJJ
      SAVE U,II,JJ
C
      DATA AA,B,C/12.37586,.4878992,12.67706/
      DATA C1,C2,PC,XN/.9689279,1.301198,.1958303E-1,2.776994/
      DATA V/ .3409450, .4573146, .5397793, .6062427, .6631691
     +, .7136975, .7596125, .8020356, .8417227, .8792102, .9148948
     +, .9490791, .9820005, 1.0138492, 1.0447810, 1.0749254, 1.1043917
     +,1.1332738, 1.1616530, 1.1896010, 1.2171815, 1.2444516, 1.2714635
     +,1.2982650, 1.3249008, 1.3514125, 1.3778399, 1.4042211, 1.4305929
     +,1.4569915, 1.4834526, 1.5100121, 1.5367061, 1.5635712, 1.5906454
     +,1.6179680, 1.6455802, 1.6735255, 1.7018503, 1.7306045, 1.7598422
     +,1.7896223, 1.8200099, 1.8510770, 1.8829044, 1.9155830, 1.9492166
     +,1.9839239, 2.0198430, 2.0571356, 2.0959930, 2.1366450, 2.1793713
     +,2.2245175, 2.2725185, 2.3239338, 2.3795007, 2.4402218, 2.5075117
     +,2.5834658, 2.6713916, 2.7769943, 2.7769943, 2.7769943, 2.7769943/

C      Load data array in case user forgets to initialize.
C      This array is the result of calling UNI 100000 times
C         with seed 305.
      DATA U/
     *0.8668672834288,  0.3697986366357,  0.8008968294805,
     *0.4173889774680,  0.8254561579836,  0.9640965269077,
     *0.4508667414265,  0.6451309529668,  0.1645456024730,
     *0.2787901807898,  0.06761531340295, 0.9663226330820,
     *0.01963343943798, 0.02947398211399, 0.1636231515294,
     *0.3976343250467,  0.2631008574685/
C
      DATA II,JJ / 17, 5 /
C
C   Basic generator is Fibonacci
C
      UN = U(II)-U(JJ)
      IF(UN.LT.0.0) UN = UN+1.
      U(II) = UN
C           U(II) and UN are uniform on [0,1)
C           VNI is uniform on [-1,1)
      VNI = UN + UN -1.
      II = II-1
      IF(II.EQ.0)II = 17
      JJ = JJ-1
      IF(JJ.EQ.0)JJ = 17
C        INT(UN(II)*128) in range [0,127],  J is in range [1,64]
      J = MOD(INT(U(II)*128),64)+1
C        Pick sign as VNI is positive or negative
      RNOR = VNI*V(J+1)
      IF(ABS(RNOR).LE.V(J))RETURN
C
C Slow part; AA is a*f(0)
      X = (ABS(RNOR)-V(J))/(V(J+1)-V(J))
C          Y is uniform on [0,1)
      Y = U(II)-U(JJ)
      IF(Y.LT.0.0) Y = Y+1.
      U(II) = Y
      II = II-1
      IF(II.EQ.0)II = 17
      JJ = JJ-1
      IF(JJ.EQ.0)JJ = 17
C
      S = X+Y
      IF(S.GT.C2)GO TO 11
      IF(S.LE.C1)RETURN
      IF(Y.GT.C-AA*EXP(-.5*(B-B*X)**2))GO TO 11
      IF(EXP(-.5*V(J+1)**2)+Y*PC/V(J+1).LE.EXP(-.5*RNOR**2))RETURN
C
C Tail part; .3601016 is 1./XN
C       Y is uniform on [0,1)
   22 Y = U(II)-U(JJ)
      IF(Y.LE.0.0) Y = Y+1.
      U(II) = Y
      II = II-1
      IF(II.EQ.0)II = 17
      JJ = JJ-1
      IF(JJ.EQ.0)JJ = 17
C
      X = 0.3601016*LOG(Y)
C       Y is uniform on [0,1)
      Y = U(II)-U(JJ)
      IF(Y.LE.0.0) Y = Y+1.
      U(II) = Y
      II = II-1
      IF(II.EQ.0)II = 17
      JJ = JJ-1
      IF(JJ.EQ.0)JJ = 17
      IF( -2.*LOG(Y).LE.X**2 )GO TO 22
      RNOR = SIGN(XN-X,RNOR)
      RETURN
   11 RNOR = SIGN(B-B*X,RNOR)
      RETURN
C
C
C  Fill
      ENTRY RSTART(ISEED)
      IF(ISEED.NE.0) THEN
C
C          Set up ...
C              Generate random bit pattern in array based on given seed
C
        II = 17
        JJ = 5
        IA = MOD(ABS(ISEED),32707)
        IB = 1111
        IC = 1947
        DO 2 III = 1,17
          S = 0.0
          T = .50
C             Do for each of the bits of mantissa of word
C             Loop  over 64 bits, enough for all known machines
C                   in single precision
          DO 3 JJJ = 1,64
                  ID = IC-IA
                  IF(ID.GE.0)GOTO 4
                  ID = ID+32707
                  S = S+T
    4             IA = IB
                  IB = IC
                  IC = ID
    3     T = .5*T
    2   U(III) = S
      ENDIF
C       Return floating echo of ISEED
      RSTART=ISEED
      RETURN
      END
