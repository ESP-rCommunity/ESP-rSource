        SUBROUTINE HLR2(IEDGE)
C
      INCLUDE 'viewer.h2'

        COMMON/HLRV/DIV,TI,TJ,DZI,DZJ,DYI,DYJ,FX,FY,KS1,
     &  KS2,KS3,KS4,SX,SY,SZ,MAXX,DXI,KS,TT,MI,XMI,XMJ,
     &  XI1,YI1,ZI1,XI2,YI2,ZI2,XJ1,YJ1,ZJ1,XJ2,YJ2,ZJ2,
     &  DLIM,YJI,ZJI
         COMMON/HLRL/PG1,PG2,P1C,P2C

        double precision DIV,TI,TJ
        double precision DZJ,DZI,DYJ,DYI

        LOGICAL PG1,PG2,P1C,P2C

C***4.8 FACE INTERSECTION COUNT FOR CURRENT BODY
        DIMENSION ICFACE(MSB)

C-----------------------------------------------------
C      all edges J are tested with the specified edge I
C***3.7
      I=IEDGE
C      to find which edges J hide edge I
C
C   determine the first vertex of edge I
C
C     ARE VERTEX NATURES ALREADY KNOWN
12    PG1=INIZIO(KS1).NE.0
      PG2=INIZIO(KS2).NE.0
C ARE VERTICES CONVEX
      P1C=ISIPU(KS1).EQ.0
      P2C=ISIPU(KS2).EQ.0
      IF(PG1 .AND.P1C)GOTO 18

      IF(PG2 .AND. P2C)GOTO 17

      IF(PG1)GO TO 18

      IF(PG2)GO TO 17

        IF(P1C)GOTO 18

        IF(P2C)GOTO 17

      GO TO 18
C IT IS CONVENIENT TO REVERSE THE NATURAL ORDER
17    IKK=KS1
      KS1=KS2
      KS2=IKK

C-----------------------------------------------------
C  DETERMINING USEFUL QUANTITIES INDEPENDENT FROM J
18        XI1=PUPRVI(KS1,1)
          YI1=PUPRVI(KS1,2)
          ZI1=PUPRVI(KS1,3)
          XI2=PUPRVI(KS2,1)
          YI2=PUPRVI(KS2,2)
          ZI2=PUPRVI(KS2,3)
          V12=YI1*ZI2-ZI1*YI2
          SX=XI2
          SY=YI2
          SZ=ZI2
C***v3.5 change name of boxsize variable
          BXMN=AMIN1(XI1,XI2)
          BYMX=AMAX1(YI1,YI2)
          BYMN=AMIN1(YI1,YI2)
          BZMX=AMAX1(ZI1,ZI2)
          BZMN=AMIN1(ZI1,ZI2)

          DXI=XI2-XI1
          DYI=YI2-YI1
          DZI=ZI2-ZI1

C***DIAG
C***D     WRITE(20,*)' HLR2----'

C-----------------------------------------------------
C  EDGES I AND J ARE TESTED FOR INTERSECTION
          MAXX=0
          MAXX0=0
C   IE FIND EDGE J WHICH HIDES EDGE I

C***3.7    EDGES OF ALL BODY IBB ARE TESTED AGAINST ALL EDGES
C   OF BODIES JBB WHICH MAY HIDE THE EDGE OF IBB (IBINTF)

      DO 50 JBB=1,NB

C   IF BODY CANT HIDE(INCLUDING GLASS) OMIT
           IF(IBINTF(JBB).EQ.0)GO TO 50

C   SET FIRST AND LAST EDGES OF BODY JBB
           JE1=IBE1(JBB)
           JE2=IBE2(JBB)

C-----------------------------------------------------
           DO 40 J=JE1,JE2

C     IF EDGE J IS NOT ON VIEW IT CANNOT AFFECT NATURE
                IF(IOCV(J).LT.0) GO TO 40
C     EDGE IS NOT TESTED FOR INTERSECTION WITH ITSELF
                IF(I.EQ.J) GO TO 40
C***3.6
                KS3=IREPU1(J)
                KS4=IREPU2(J)

C-----------------------------------------------------
C  IF EDGES I AND J ARE ADJACENT TO THE SAME VERTEX, INTERSECTION IS NOT
C  COMPUTED
C***V3.2
C   USE ARITHMETIC IF :IT IS FASTER
                IF(KS1-KS3)181,40,181
 181            IF(KS1-KS4)182,40,182
 182            IF(KS2-KS3)183,40,183
 183            IF(KS2-KS4)184,40,184
 184            CONTINUE

C-----------------------------------------------------
19              XJ1=PUPRVI(KS3,1)
                YJ1=PUPRVI(KS3,2)
                ZJ1=PUPRVI(KS3,3)
                XJ2=PUPRVI(KS4,1)
                YJ2=PUPRVI(KS4,2)
                ZJ2=PUPRVI(KS4,3)
C         CRUDE BOX TEST SAVES A LOT OF TIME
C         AND CAN ELIMINATE SOME WINNING INTERSECTIONS AS WELL
                IF(AMAX1(XJ1,XJ2)-BXMN)40,40,195
 195            IF(AMIN1(YJ1,YJ2)-BYMX)196,40,40
 196            IF(AMAX1(YJ1,YJ2)-BYMN)40,40,197
 197            IF(AMIN1(ZJ1,ZJ2)-BZMX)198,40,40
 198            IF(AMAX1(ZJ1,ZJ2)-BZMN)40,40,199
 199            CONTINUE

C-----------------------------------------------------
C          DYJ=YJ2-YJ1
C          DZJ=ZJ2-ZJ1
C          YJI=YJ1-YI1
C          ZJI=ZJ1-ZI1
C
C         COMPUTE THE INTERSECTION POINT
                U22=YI2*ZJ2-ZI2*YJ2
                U11=YI1*ZJ1-ZI1*YJ1
                U12=YI1*ZJ2-ZI1*YJ2
                U21=YI2*ZJ1-ZI2*YJ1
                W12=YJ1*ZJ2-ZJ1*YJ2
                DIV=U22+U11-U21-U12
                TI=W12+U11-U12

C-----------------------------------------------------
                IF(ABS(DIV).GT.1E-10)GOTO 20
C         EDGES I AND J ARE PARALLEL
                IF(ABS(TI).GT.DLIM)GOTO 40

C   AND COINCIDENT
                GOTO 23

20              DIV=1./DIV
                TI=TI*DIV
                TJ=-V12+U11-U21
                TJ=TJ*DIV
C         TEST IF INTERSECTION OUTSIDE AT LEAST ONE LINE
                IF(TI.LT.-DLIM .OR. TI.GT.(1.+DLIM))GO TO 40
                IF( TJ.LT.-DLIM .OR. TJ.GT.(1.+DLIM))GOTO 40

C-----------------------------------------------------
C  TESTING FOR A WINNING OR LOSING INTERSECTION
C   COMPARE XI AND XJ OF INTERSECTION
                XMI=DXI*TI+XI1
                XMJ=(XJ2-XJ1)*TJ+XJ1
C   IF I IN FRONT OF J ,GET NEXT J
                IF(XMI.GE.XMJ)GOTO 40
C   OTHERWISE J HIDES I
C   TEST FOR PROPER BOUNDS ON EDGES T-PARAMETERS
C    IF ALL OK,GO TO STORE HIDING J-EDGE

C  IF THE PARAMETER FOR EDGE I IS NOT PROPERLY BOUNDED,
C   SHIFT THE POINT  OR GO AWAY
C   UNLESS-ONE END OF EDGE WAS BEHIND EYE
C   INTERIM?
C     IF(XI1.GT.BIG.OR.XI2.GT.BIG)GO TO 30
C   DONT BOTHER WITH BOUNDS ON I-EDGE
C   I.E. NEVER SHIFT I-VERTICES
C          IF(TI.LT.DLIM)GOTO 21
C          IF(TI.GT.(1.-DLIM))GOTO 22

C-----------------------------------------------------
C   T-PARAMTER FOR I-EDGE IS OK
C  IF THE PARAMETER FOR EDGE J IS NOT PROPERLY BOUNDED,
C   SHIFT THE POINT OR GO AWAY
C   UNLESS-ONE END OF LINE WAS BEHIND EYE
C     IF(XJ1.GT.BIG.OR.XJ2.GT.BIG)GO TO 30
                IF(TJ.LT.DLIM)GOTO 23
                IF(TJ.GT.(1.-DLIM))GOTO 24
C   T-PARAMETER FOR J-EDGE IS OK
                GOTO 30

C-----------------------------------------------------
C   T-PARAMETER NOT PROPERLY BOUNDED
21              KS=KS1
                INN=1
                GOTO 25

22              KS=KS2
                INN=2
                GOTO 25

23              KS=KS3
                INN=3
                GOTO 25
24              KS=KS4
                INN=3
C         THIS IS AN OUTRAGEOUS FUDGE
C         IF I KNEW HOW TO DO IT PROPERLY, I WOULD
25              PUPRVI(KS,2)=PUPRVI(KS,2)+FX
C     AT LEAST I DESTROY THE NATURE OF A SHIFTED POINT
                PUPRVI(KS,3)=PUPRVI(KS,3)+FY
                INIZIO(KS)=0
C-----------------------------------------------------
C***DIAGNOSTIC
C***D       WRITE(20,9806)J,I
C***D       WRITE(20,9807)KS,PUPRVI(KS,2),PUPRVI(KS,3)
C***D9807  FORMAT(' VERTEX',I4,' SHIFTED TO',2(1X,G16.8))
C***D9806  FORMAT(' EDGE',I4,' INTERSECTION TOO CLOSE TO EDGE',I4)
C***
C-----------------------------------------------------
C       Shift a bit further in a different direction next time
                 DIV=FX
                 FX=FY+FY
                 FY=-DIV
                 GOTO (12,18,19),INN

C-----------------------------------------------------
  30             CONTINUE
C  STORING A LOSING INTERSECTION
C   I.E. J-EDGE HIDES I-EDGE AT T=TI
                 MI=MAXX
                 IF(MI.EQ.0)GOTO 32

                 DO 31 MJ=1,MAXX
                      TT=RIORD(MI,1)
                      IF(TT.LE.TI)GOTO 32
                      RIORD(MI+1,1)=TT
                      RIORD(MI+1,2)=RIORD(MI,2)
31                    MI=MI-1
32               RIORD(MI+1,1)=TI
                 RIORD(MI+1,2)=J
                 IF(MI.NE.0)GOTO 33
C  DETERMINING THE COORDINATES OF THE POINT ON THE THREEDIMENSIONAL EDGE I
C  PROJECTED IN THE FIRST INTERSECTION
                 SX=XI1+TI*DXI
                 SY=YI1+TI*DYI
                 SZ=ZI1+TI*DZI
33               MAXX=MAXX+1
40         CONTINUE

C***4.8   PENETRATIONS
C   WE NOW HAVE ALL NEW PENETRATIONS (HIDING EDGES) FOR BODY JBB
C   TEST FOR PENETRATIONS OF EDGE I WITH FACES OF THIS BODY JBB
C   THESE SHOULD OCCUR WHERE A FACE HAS AN ODD NUMBER OF HIDING EDGES
C   IF IT HAS EVEN NUMBER THEN EDGE IS COMPLETELY BEHIND THE FACE

           INTSCT=0
           IF(INTSCT .EQ. 0) GO TO 50
C   FACE NUMBERS OF THIS BODY
           JF1=IBF1(JBB)
           JF2=IBF2(JBB)
C   IF NO EXTRA HIDING EDGES FOR THIS BODY , THEN OMIT
           IF (MAXX .LE. MAXX0) GO TO 50

           DO 60 MM=MAXX0+1,MAXX
C   GET EDGE
                J=RIORD(MM,2)
C   FACES
                K3=IREFA1(J)
                K4=IREFA2(J)
C   UPDATE FACE INTERSECTION COUNT
                ICFACE(K3-JF1+1)=ICFACE(K3-JF1+1)-1
                ICFACE(K4-JF1+1)=ICFACE(K4-JF1+1)-1
C   IE FACC COUNT =1 FOR ODD NUMBER OF INTERSECTING EDGES, 0 FOR EVEN
  60        CONTINUE

C   GET INTERSECTION POINTS WHERE EDGE I PENETRATES FACES WITH
C   ODD NUMBER OF HIDING EDGES
            DO 70 MF=JF1,JF2
                 IF(ICFACE(MF-JF1+1) .EQ. 1) THEN
C   THERE IS A PENETRATION
C   GET T-PARAMETER OF INTERSECTION USING
C   EQUN OF EDGE I AND PLANE EQUN OF FACE MF

C   SAVE IN RIORD

C   SORT, AND REMOVE ANY OUTSIDE EDGE : T<0 OR T>1
                 ENDIF
  70        CONTINUE

  50  CONTINUE

C   NOW HAVE ALL J-EDGES HIDING I-EDGE

C-----------------------------------------------------
C***DIAG
C***D      WRITE(20,9401)MAXX,I
C***D9401  FORMAT(' ',I3,' EDGES HIDING EDGE-',I3)
C***D      IF(MAXX.GT.0)WRITE(20,*)
C***D    & (RIORD(J,1),RIORD(J,2),J=1,MAXX)
C***

      RETURN
      END

