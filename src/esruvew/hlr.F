C       VIEWER VERSION 1.5 (PDP-10)
C       COPYRIGHT R. P. PARKINS, OF THE ABACUS UNIT, 1978
C        AND HS 1981-87
      SUBROUTINE HLR
C
C*** OLD COMMENTS
C  SUBROUTINE HLR PROVIDES HIDDEN LINE ELIMINATION AND OUTPUT.
C  MORE PRECISELY, IT WORKS AS FOLLOWS.
C  5) PARAMETERS T ARE DEFINED, VALUED 0 IN THE INITIAL AND 1 IN THE FINAL
C     EXTREME OF THE EDGES. THE VALUES OF T CORRESPONDING TO THE INTERSECTION BE
C     TWEEN THE STRAIGHT LINES TO WHICH EDGE I AND EDGE J BELONG IS THEN COMPUTE
C     D. IF BOTH T VALUES ARE INCLUDED BETWEEN +D AND 1-D WE COMPUTE ALSO IF
C     EDGE I IS WINNING OR LOSING. IF EDGE I IS LOSING (I.E. IF IT IS HIDDEN BY
C     J IN THE INTERSECTION POINT), WE STORE THE PARAMETER T RELATIVE TO EDGE I
C     IN TAT(J). IF ONE OR BOTH OF THE VALUES T ARE INCLUDED BETWEEN -D OR <D OR
C     BETWEEN 1-D AND 1+D THE CORRESPONDING PROJECTED POINT IS SLIGGHTLY MOVED
C     , AND THE ENTIRE INTERSECTION STEP FOR EDGE I IS INIZIALIZED AGAIN. OTHERW
C     ISE, NO OPERATION.
C  6) THE LOSING INTERSECTIONS ALONG EDGE I ARE ORDERED ACCORDING TO THE VALUE O
C     F PARAMETER T. T VALUES ARE STORED IN RIORD(K,1) AND J VALUE IN RIORD]K,2
C  7) IF THE NATURE OF THE INITIAL VERTEX IS NOT KNOWN, IT IS NOW COMPUTED. AN H
C     ALF STRAIGHT LINE STARTING FROM IT IS INTERSECTED WITH ALL EDGES AND,IF TH
C     E INTERSECTION EXISTS, A BOOLEAN INDICATOR IDEA(K) (ORIGINALLI .TRUE.! IS
C     CHANGED FOR BOTH THE ADJACENT FACES. THE INITIAL VERTEX IS INTERNAL TO THE
C     POLYGONS CORRESPONDING TO FACES K WITH IDEA(K)=.FALSE.. THEN SUCH FA
C     CES ARE THREEDIMENSIONALLY TESTEDAND THOSE WHICH HIDE V ARE STORED IN LIFA
C     IF THE NATURE OF V IS KNOWN, BUT V IS CONCAVE, THE SAME PROCEDURE IS APPLI
C     ED, BUT ONLY TO FACES OF WHICH V IS A VERTEX
C  8) NATURE VARIATIONS ARE DETERMINED WITH AN OBVIOUS ALGORITHM IN CORRESPONDEN
C     CE OF EVERY LOSING INTERSECTION. TRACING THE EDGE, IF THE NATURE GOES FROM
C     NONZERO TO ZERO FROM ZERO TO NONZERO , A PEN-DOWN OR A PEN-UP ORDER IS
C     GIVEN
C       At the end of an edge, tha nature of the final vertex is now
C       known, and it is stored if not aleady so. However faces incident
C       to the vertex are not part of its nature, and are not stored.
C       In theory this latter check is necessary only for concave
C       vertices, but it has been found that rounding errors can cause
C       an almost vertical edge to arrive at a convex vertex with its
C       nature containing a face of that vertex, so the removal of
C       faces incident to the vertex is done in all cases.

      INCLUDE 'viewer.h'
      INCLUDE 'viewer.h2'

C   common data for passing to HLR2

        COMMON/HLRV/DIV,TI,TJ,DZI,DZJ,DYI,DYJ,FX,FY,KS1,
     &  KS2,KS3,KS4,SX,SY,SZ,MAXX,DXI,KS,TT,MI,XMI,XMJ,
     &  XI1,YI1,ZI1,XI2,YI2,ZI2,XJ1,YJ1,ZJ1,XJ2,YJ2,ZJ2,
     &  DLIM,YJI,ZJI
        COMMON/HLRL/PG1,PG2,P1C,P2C

      LOGICAL PG1,PG2,IDEA(MSF),P1C,P2C
      DOUBLE PRECISION DIV,TI,TJ
      DOUBLE PRECISION DZJ,DZI,DYJ,DYI

C------------------------------------------------------------
C Argument for call to KDET
      N=4

C SET NATURE EMPTY
      LIR=1
      DO 2 J=1,NP
   2       INIZIO(J)=0

C Initialise parameters for point shifting
        FX=1.
        FY=1.

C COINCIDENCE LIMIT FOR T-PARAMETER OF EDGE INTERSECTIONS
      DLIM=1.0E-10
C EDGES FROM BEHIND EYE HAVE X,Y,Z  > BIG
      BIG=1.0E6
C INITIAL POINT-SHIFTING DISTANCE
C***V3.3.2
      FXI=1.0E-6*BX
      FYI=1.0E-6*BILDY

C   PICTUR BOUNDS IN ABS. COORDS. (+-1000)
      BX=1000.
      BY=1000.*BILDY/BILDX
C***DAG
C***D      WRITE(20,*)' HLR'

C---------------------------------------------------------
C***3.7  EXAMINE EACH BODY IN TURN

      DO 85  IBB=1,NB
C***DIAG
C***D      WRITE(20,*)' *****BODY',IBB

C***4.7 NEW VISIBILITY INDEX, IF NOT VISIBLE DONT DRAW
           IF(IBODG(IBB).EQ.0)GO TO 85
C***5.5 OMIT IF LAYER OFF
           IF(LAYER(IBLAY(IBB)).EQ.0)GO TO 85
C   IF BODY OUTSIDE COV DONT TEST IT
           IF(BBYMN(IBB).GE.BX)GO TO 85
           IF(BBYMX(IBB).LE.-BX)GO TO 85
           IF(BBZMN(IBB).GE.BY)GO TO 85
           IF(BBZMX(IBB).LE.-BY)GO TO 85

C  EXAMINE EACH OTHER BODY IN TURN TO SEE IF
C  IT WILL INTERFERE WITH (OBSCURE) BODY IBB
C   SAVE RESULTS IN IBINTF
C   NO OF BODIES INTERFERING
           NINTF=0

C-----------------------
           DO 5 JBB=1,NB
C  INITALLY SET NO INTERFERE
                IBINTF(JBB)=0

C***4.7  IF JBB IS GLASS/NOT HIDING, CANT INTERFERE
                IF(IBODG(JBB).LT.0)GO TO 5
C***5.5 OMIT IF LAYER OFF
           IF(LAYER(IBLAY(JBB)).EQ.0)GO TO 5

C  TEST FOR SAME BODY
C  IF I CONCAVE , CAN OBSCURE
                IF(IBB.EQ.JBB.AND.IES(JBB).EQ.0)GO TO 4
C   IF I CONVEX , CANNOT OBSCURE ITSELF
                IF(IBB.EQ.JBB.AND.IES(JBB).EQ.1)GO TO 5

C   IF JBB OUTSIDE C.O.V. , CASNT INTERFERE
                IF(BBYMN(JBB).GE.BX)GO TO 5
                IF(BBYMX(JBB).LE.-BX)GO TO 5
                IF(BBZMN(JBB).GE.BY)GO TO 5
                IF(BBZMX(JBB).LE.-BY)GO TO 5

C   IF NO OVERLAP ON DISPLAY AREA (YZ)
                IF(BBYMN(JBB).GE.BBYMX(IBB))GO TO 5
                IF(BBYMX(JBB).LE.BBYMN(IBB))GO TO 5
                IF(BBZMN(JBB).GE.BBZMX(IBB))GO TO 5
                IF(BBZMX(JBB).LE.BBZMN(IBB))GO TO 5

C   IF JBB BEHIND IBB NO POSSIBLE OVERLAP
                IF(BBXMX(JBB).LE.BBXMN(IBB))GO TO 5

C   OTHERWISE POTENTIAL OVERLAP
   4            NINTF=NINTF+1
                IBINTF(JBB)=1
   5       CONTINUE

C-------------------------
C***DIAG
C***D     WRITE(20,*)' OBSCURED BY',NINTF,':',(IBINTF(J),J=1,NINTF)

C   WE NOW HAVE ALL BODIES OVERLAPPING WITH IBB
C-----------------------------------------------
C***4.6
C   SET BODY COLOUR IF DIFFERENT FROM LAST
           IF(IBCOL(IBB).NE.ICOL)THEN
                ICOL=IBCOL(IBB)
                CALL PLOTTC(ICOL)
           ENDIF

C   IF NO BODIES INTERFERE WITH IBB, IT MUST BE CONVEX, SO
C   JUST GO AND DRAW IT
            IF(NINTF.GT.0)GO TO 8
            CALL PERS2(IBB)
            GO TO 85

C-----------
C   FIRST AND LAST EDES OF IBB
   8       IE1=IBE1(IBB)
           IE2=IBE2(IBB)

C   EMPTY NATURE, SINCE ALL PREVIOUS INFO IN NOFA REFERS TO
C   VERTICES OF PREVIOUS BODIES
           LIR=1

C***4.7
           IBVIS=ABS(IBODG(IBB))

C-----------------------------------------------
C  FROM NOW ON A SINGLE EDGE I IS EXAMINED FROM BODY IBB

C   SEARCH EACH EDGE I & COMPARE WITH EDGE J WHICH MAY HIDE IT
           DO 80 I=IE1,IE2

C     IF THIS EDGE IS OUTSIDE THE CONE OF VISION, IGNORE IT
                JCV=IOCV(I)

                KS1=IREPU1(I)
                KS2=IREPU2(I)
C***DIAG
C***D      WRITE(20,9700)I,KS1,KS2
C***D9700  FORMAT('***EDGE',I4,' FROM VERTEX',I4,' TO',I4)
C***D      IF(JCV.EQ.-2)WRITE(20,9800)
C***D9800  FORMAT(' OUTSIDE C. O. V., NOT ON VIEW')
C***D      IF(JCV.EQ.-1)WRITE(20,9801)
C***D9801  FORMAT(' INSIDE C. O. V., NOT ON VIEW')
C***D      IF(JCV.EQ.0)WRITE(20,9802)
C***D9802  FORMAT(
C***D     F' INSIDE C. O. V., NOT ON VIEW BUT RELEVANT')
C***D      IF(JCV.EQ.1)WRITE(20,9803)
C***D9803  FORMAT(' INSIDE C. O. V., AND ON VIEW SILHOUETTE')
C***D      IF(JCV.EQ.2)WRITE(20,9895)
C***D9895  FORMAT(' INSIDE C. O. V., AND ON VIEW')
C***D      IF(JCV.EQ.3)WRITE(20,9804)
C***D9804  FORMAT(' OUTSIDE C. O. V., AND ON VIEW')
C***

C***4.7   OUTSIDE CONE OF VISION:IGNORE IT
                IF(IABS(JCV).EQ.3)GO TO 80
C***4.7 FOR SILHOUETTE BODY, ONLY DRAW OUTSIDE EDGES
                IF(IBVIS.EQ.1 .AND. JCV.NE.1 .AND. JCV.NE.0)GO TO 80
C***D      WRITE(20,*)' EDGE',I,KS1,KS2

C       Reset point shifting to a short distance
                FX=FXI
                FY=FYI

C       ... and slope increment for test half-line
                DAP=0.1

C     IF NOFA HAS OVERFLOWED ...
                IF(LIR.LT.MED)GOTO 101
C         CLEAR IT OUT

                IP1=IBP1(IBB)
                IP2=IBP2(IBB)

                DO 10 J=IP1,IP2
10                   INIZIO(J)=0

                LIR=1

 101            CONTINUE

C   VERTICES OF THIS I-EDGE
C***3.6 NO LONGER PACK
                KS1=IREPU1(I)
                KS2=IREPU2(I)
C***5.4 fix for dashedline fault!
                XI1=PUPRVI(KS1,1)
                YI1=PUPRVI(KS1,2)
                ZI1=PUPRVI(KS1,3)
                XI2=PUPRVI(KS2,1)
                YI2=PUPRVI(KS2,2)
                ZI2=PUPRVI(KS2,3)

C***4.7     IF THE EDGE IS ON VIEW, DO THE INTERSECTION CHECK
                IF(JCV.EQ.1 .OR. JCV.EQ.2) GO TO 12

C  EDGES HIDDEN BY THEIR OWN VOLUME ARE PLOTTED DOTTED IF REQUESTED
C   UNLESS PART OF GLASS BODY

C   CHECK IF EDGE IS PART OF GLASS BODY

C   IF BODY IS GLASS WE MUST CHECK EDGE I AGAINST ALL J
C***4.7   SINCE GLASS EDGE NEVER PLOTTED DASHED
 9805           IF(IBVIS .EQ. 3)GO TO 12
C   OTHERWISE(NOT GLASS)CHECK HIDDEN LINE DASHED OR NOT
                IF(NTRAT.EQ.0) GO TO 80

C***4.7.2 PLOT 3D VECTOR
                CALL PLOTTR(XI1,YI1,ZI1,XI2,YI2,ZI2,-20)
                GO TO 80

C  EDGES I AND J ARE TESTED FOR INTERSECTION
C***V3.5  do edge searching in new routine HLR2
  12            IEDGE=I
                call hlr2(IEDGE)
C               ----------------

C-----------------------------------------------------
C  IF THE NATURE OF VERTEX1 OF EDGE I IS ALREADY KNOWN,
C   IT IS STORED IN LIFA
C***3.7
  20            KVOLT=0
                KSS1=INIZIO(KS1)
                IF(KSS1.EQ.0)GOTO 432
41              NOF=NOFA(KSS1)
                IF(NOF.EQ.(-1)) GO TO 42
                KVOLT=KVOLT+1
                LIFA(KVOLT)=NOF
                KSS1=KSS1+1
                GO TO 41

C-----------------------------------------------------
C   IF VERTEX IS CONVEX AND KNOWN, GO TO DRAW IT
42              IF(ISIPU(KS1).EQ.0)GOTO 70

C   OTHERWISE.........
C  A POINT OF THE EDGE MUST BE TESTED, BECAUSE THE INITIAL VETEX IS EITHER UNKNO
C  WN OR CONCAVE OR BOTH.  THE MIDDLE POINT IS CHOSEN BETWEEN THE INITIAL VERTEX
C  AND THE FIRST INTERSECTION OR THE MIDDLE POINT OF THE EDGE
C       NEVER SHIFT THE FIRST TIME
                 GOTO 432

C---------------
C   HERE WE SHIFT TEST POINT IF BAD TROUBLE WITH TEST LINE

43              TI=ABS(SY-AAA(2))
                TJ=ABS(SZ-AAA(3))
                IF(TI.GT..1*ABS(DYI) .OR. TJ.GT..1*ABS(DZI))GOTO 432
C       GIVE UP UTTERLY, SHIFT THE POINT AND GO RIGHT BACK

431             PUPRVI(KS1,2)=PUPRVI(KS1,2)+FX

                PUPRVI(KS1,3)=PUPRVI(KS1,3)+FY
                INIZIO(KS1)=0
C       Shift a bit further in a diffferent direction next time
                DIV=FX
                FX=FY+FY
                FY=-DIV
C***DIAG
C***D      WRITE(20,9807)KS1,PUPRVI(KS1,2),PUPRVI(KS1,3)
C***D9807  FORMAT( 'POINT',I5,' MOVED TO', 2G16.8)
C***

                GOTO 12

C----------------------------------------------------
C   FIND THE NATURE OF VERTEX KS1
C   I.E. FIRST VERTEX OF I-EDGE
432             AAA(1)=0.5*(SX+PUPRVI(KS1,1))
                AAA(2)=0.5*(SY+PUPRVI(KS1,2))
                AAA(3)=0.5*(SZ+PUPRVI(KS1,3))
                SX=AAA(1)
                SY=AAA(2)
                SZ=AAA(3)
C  A HALF STRAIGHT LINE IS CONSIDERED, STARTING FROM THE INITIAL VERTEX AND WITH
C  A SLOPE  AP.
C       Start with slope at -2.
                AP=-2.
C       Double the slope increment each time round
                DAP=DAP+DAP
C       If slope increment gets too big, drop it
                IF(DAP.GT.2.)DAP=1.
C       If we got too far, try moving the start point
44              AP=AP+DAP

                DO 45 J=1,NF
45                   IDEA(J)=.TRUE.

C***DIAG
C***D      WRITE(20,9701)AAA,AP
C***D9701  FORMAT(' TRYING TEST HALF-LINE FROM',3(1X,G16.8,1X)/
C***D    F' AT SLOPE',G16.8)
C***

C----------------------------------------------------
C  THE HALF STRAIGHT LINE IS INTERSECTED WITH ALL EDGES
C    BELONGING TO OBSCURING BODIES
C   TO DETERMINE THOSE FACES WHICH POTENTIALLY HIDE VERTEX 1 OF EDGE I

                DO 50 JBB=1,NB
                     IF(IBINTF(JBB).EQ.0)GO TO 50

C    EDGES
                     JE1=IBE1(JBB)
                     JE2=IBE2(JBB)

                     DO 49 J=JE1,JE2

C   IF EDGE NOT ON VIEW(BACK EDGE), IGNORE IT
                          IF(IOCV(J).LT.0)GO TO 49

C   IF EDGE =I IGNORE IT
                          IF (I.EQ.J) GO TO 49

C   OTHERWISE COMPUTE INTERSECTION
C***3.6  NO LONGER PACK
                          KF1=IREFA1(J)
                          KF2=IREFA2(J)

C       J if the vertex is not known
                          IF(KSS1.EQ.0)GOTO 46
C
C--------------------------
C       The vertex is known, so it must be concave
C       We are only interested in faces incident to the vertex

C       So for the first face of this edge ...
C       Mark it is not of interest so far ...
                          PG1=.FALSE.
C***3.6  NO LONGER PACK

                          K1=IPTR1(KF1)
                          K2=IPTR2(KF1)
C       For each of its vertices
                          DO 451 K3=K1,K2
C       If it is the vertex whose nature we want
C       Mark the face is of interest
                               IF(ISIDOR(K3).EQ.KS1)PG1=.TRUE.
451                       CONTINUE

C-------------------------
C       Same for second vertex
                          PG2=.FALSE.
C***3.6 NO LONGER PACK
                          K1=IPTR1(KF2)
                          K2=IPTR2(KF2)
C
                          DO 452 K3=K1,K2
                               IF(ISIDOR(K3).EQ.KS1)PG2=.TRUE.
452                       CONTINUE

C-------------------------
C       If the edge does not belong to an interesting face, ignore
                          IF(PG1 .OR. PG2)GOTO 461
                          GO TO 49
C------------------------

C       Both faces are of interest if nature not known at all
46                        PG1=.TRUE.
                          PG2=.TRUE.
C***3.6 NO LONGER PACK
 461                      K1=IREPU1(J)
                          K2=IREPU2(J)

                          YJ1=PUPRVI(K1,2)
                          ZJ1=PUPRVI(K1,3)
                          YJ2=PUPRVI(K2,2)
                          ZJ2=PUPRVI(K2,3)
                          DYJ=YJ2-YJ1
                          DZJ=ZJ2-ZJ1
                          YJI=YJ1-AAA(2)
                          ZJI=ZJ1-AAA(3)
                          IF(ABS(DYJ).GT..1E-10
     &                    .OR. ABS(DZJ).GT..1E-10)GOTO 462

C-------------------------
C       REPLACE VERTICAL EDGE BY ONE PARALLEL TO TEST LINE
C***V3.3.2
                          DYJ=BX/20.
                          DZJ=AP*DYJ

462                       DIV=DZJ-AP*DYJ
                          TI=YJI*DZJ-ZJI*DYJ
                          IF(ABS(DIV).GT..1E-20)GOTO 47
C
C   EDGE PARALLEL WITH TEST LINE
C   TEST IF INTERSECTION OUTSIDE TEST LINE. IF SO, OMIT
                          IF(ABS(TI).GT.DLIM)GOTO 49

C       EDGE COINCIDENT WITH TEST HALF-LINE
C***DIAG
C***D      WRITE(20,9702)J,DIV,TI
C***D9702  FORMAT(' EDGE',I4,' TOO CLOSE, DIV=',G16.8,' TI=',G16.8)
C***

C      GO BACK TO BEGINNING
C       Try another slope if we can
                          IF(AP.LT.2.)GOTO 44
C       Slope has got tto big, try moving the test point
                          GOTO 43

C-----------------------
47                        DIV=1./DIV
                          TI=TI*DIV*20./BX
C***V3.3.2
C       TEST IF INTERSECTION IN FIRST OR FOURTH QUADRANT
                          IF(TI.LT.-DLIM)GOTO 49

C   INTERSECTION POINT ON EDGE
                          TJ=(YJI*AP-ZJI)*DIV

C       TEST IF INTERSECTION OUTSIDE EDGE
                          IF(TJ.LT.-DLIM .OR. TJ.GT.(1.+DLIM))GOTO 49

C   TEST NEAR START OF EDGE
                          IF(TI.GT.DLIM)GOTO 471
C       EDGE TOO CLOSE TO START OF TEST HALF-LINE
C***DIAG
C***D      WRITE(20,9702)J,DIV,TI
C***

C       MOVE THE START, THEN
                          GOTO 43
C
C------------------------
471                       IF(TJ.LT.DLIM)GOTO 472
                          IF(TJ.GT.(1.-DLIM))GOTO 473
C       INTERSECTION IS WITHIN EDGE
                          GOTO 48
C------------------------
C       Trouble with first vertex of edge
472                       K2=K1
C       Trouble with second vertex
473                       CONTINUE
C***DIAG
C***D      WRITE(20,9703)K2,J
C***D9703  FORMAT(' VERTEX',I4,' OF EDGE',I4,' TOO CLOSE TO TEST LINE')
C***

C       If slope not too big, try another slope
                          IF(AP.LT.2.)GOTO 44
C       If it is the vertex whose nature we are trying to find
C       We will have to shift it anyway
                          IF(K2.EQ.KS1)GOTO 431

C       Otherwise try moving the test point first
                          GOTO 43

C------------------------
C      If the intersection is inside the edge, the Boolean label IDEA is
C       changed for either or both of the faces which were of interest
C       (Both were if we did not know the nature of this vertex at all, but only
C       one may be if the vertex was concave and we are only looking at faces
C       incident to it)
48                        IF(PG1)IDEA(KF1)=.NOT.IDEA(KF1)
                          IF(PG2)IDEA(KF2)=.NOT.IDEA(KF2)
49                   CONTINUE

  50            CONTINUE

C***DIAG
C***D      WRITE(20,*)' IDEA',(J,IDEA(J),J=1,NF)

C----------------------------------------------------
C***3.6 NO LONGER PACK
                KI1=IREFA1(I)
                KI2=IREFA2(I)

C       Three-dimensional test
C       If nature is not already known, prepare to store it
                IF(KSS1.EQ.0)INIZIO(KS1)=LIR
C
C   DO ALL FACES OF OBSCURING BODIES
C
                DO 65 JBB=1,NB

                     IF(IBINTF(JBB).EQ.0)GO TO 65
C   FIRST AND LAST FACES OF BODY JBB (MUST HAVE SOME IF INTERFERE)
                     JF1=IBF1(JBB)
                     JF2=IBF2(JBB)

                     DO 60 J=JF1,JF2
C  ONLY THE ON-VIEW FACES WITH IDEA=.FALSE., NOT ADJACENT TO THE EDGE I
C   ARE THREE-DIMENSIONALLY TESTED
                          IF(SITRA(J).LE.EPS.OR.IDEA(J))GO TO 60
C   IF FACE IS ADJACENT TO EDGE, OGNORE IT
                          IF (J.EQ.KI1.OR.J.EQ.KI2) GO TO 60
C***3.6
                          LIB=IPTR1(J)
                          LIB1=IPTR2(J)
                          M1=ISIDOR(LIB)
                          M2=ISIDOR(LIB+1)
                          M3=ISIDOR(LIB+2)
                          DO 63 K1=1,4
   63                          A(K1,4)=1.
                          DO 51 K1=1,3
                               A(1,K1)=AAA(K1)
                               A(2,K1)=PUPRVI(M1,K1)
                               A(3,K1)=PUPRVI(M2,K1)
   51                          A(4,K1)=PUPRVI(M3,K1)
                          CALL KDET(A,N,N,N,DET1)

                          IF(DET1.GT.0.0)GO TO 60
C  HIDDING FACES ARE STORED IN LIFA
                          KVOLT=KVOLT+1
                          LIFA(KVOLT)=J
C       If the face is adjacent to the vertex, it is not part of its nature
C       So for each of the vertices of the face
                          DO 52 K1=LIB,LIB1
C       If it is the vertex whose nature we are looking for ...
C       This face is not part of it, so skip round storing it
                               IF(ISIDOR(K1).EQ.KS1)GOTO 60
52                        CONTINUE
                          NOFA(LIR)=J
                          IF(LIR.LT.MED)LIR=LIR+1

   60                CONTINUE

  65            CONTINUE

C----------------------------------------------------
C       No nature was stored for this point if we already knew it
                IF(KSS1.NE.0)GOTO 70
                NOFA(LIR)=-1
                IF(LIR.LT.MED)LIR=LIR+1

C---------------------------------------------------------
C  LIFA CONTAINS THE NATURE SET
C  NFCMN IS THE CARDINALITY OF THE NATURE SET.
C    IFF NFCMN=0 , THE EDGE SEGMENT IS PLOTTED
C  THE INITIAL NATURE IS THE SAME OF THE INITIAL VERTEX

70              NFCMN=KVOLT

C***DIAG
C***D      WRITE(20,9900)KS1,I
C***D9900  FORMAT(' NATURE NEAR VERTEX',I4,' AT START OF EDGE',I4)
C***D      IF(NFCMN.EQ.0)WRITE(20,9901)
C***D9901  FORMAT(' NULL')
C***D      IF(NFCMN.NE.0)WRITE(20,9902)(LIFA(K),K=1,KVOLT)
C***D9902  FORMAT(1X,10I5)
C***

C***4.7.2 3D COORDS NOW X-DEPTH, YZ SCREEN
                XPR=XI1
                YPR=YI1
                ZPR=ZI1
C***3.9
                IF(MAXX.EQ.0)GO TO 77
C  WE EXAMINNE THE LOSING INTERSECTION OF EDGE I WITH EDGE J
C   TO DETERMINNE NATURE VARIATIONS
C   MAXX IS NR. OF J-EDGES HIDING I-EDGE

C-----------------------------------------------------
                DO 76 J=1,MAXX

                     IKK=RIORD(J,2)
C***4.8 INTERSECTING AND PENETRATING POINTS
                     IF(IKK .LT. 0)THEN
C   J IS A PENETRATING POINT WHERE
C   IKK= - HIDING SURFACE NUMBER AT T = RIORD(J,1)
                          KS3=-IKK
                          KS4=0
                     ELSE
C   J IS AN INTERSECTING (HIDING) POINT WHERE
C   IKK= HIDING EDGE NUMBER
C   FACES OF EDGE IKK
C***3.6
                          KS3=IREFA1(IKK)
                          KS4=IREFA2(IKK)
                     ENDIF

C       Contrary to what is intuitively obvious, if both faces are on-view
C       they can both need to be added to or subtracted from the nature
C       Of course this can only happen as a result of shifting points
C       What did you expect?
C       So we need to be rather careful here

C-----------------------------------
C       Initially assume nothing to be done to face KS3
                     KF3=0
C***4.8 AVOID IF SURFACE 0
                     IF(KS3 .LE. 0)GO TO 712
C       If not on-view, leave it so
                     IF(SITRA(KS3).LE.EPS)GOTO 712
C       If the nature is empty, do not look in it
                     IF(NFCMN.EQ.0)GOTO 711
C       For each face in the nature
                     DO 71 KF3=1,KVOLT
C       If it is KS3, it is to be removed
                          IF(LIFA(KF3).EQ.KS3)GOTO 712
71                   CONTINUE
C       It is on-view and not in the nature, it is to be added
711                  KF3=-1
C       KF3 now has -1 to add face KS3
C                    0 to do nothing about it
C                    + index in LIFA to remove it

C-----------------------------------
C       Now do the same for KF4, KS4
712                  KF4=0
                     IF(KS4.LE.0)GO TO 722
                     IF(SITRA(KS4).LE.EPS)GOTO 722
                     IF(NFCMN.EQ.0)GOTO 721
                     DO 72 KF4=1,KVOLT
                          IF(LIFA(KF4).EQ.KS4)GOTO 722
72                   CONTINUE
721                  KF4=-1

C-----------------------------------
C       Now split on KF3
722                  IF(KF3)731,74,735

C       KS3 is to be added, what about KS4
731                  IF(KF4)732,733,734

C       Both to be added
732                  KVOLT=KVOLT+1
                     LIFA(KVOLT)=KS3
                     KVOLT=KVOLT+1
                     LIFA(KVOLT)=KS4
                     NFCMN=NFCMN+2
C***DIAG
C***D      WRITE(20,9602)KS3,KS4,IKK
C***D9602  FORMAT(
C***D    F' FACES',I4,' AND',I4' ARE ADDED TO THE NATURE CROSSING EDGE',I4)
C***
C
C       Test if to put pen up if nature was empty before
                     IF(NFCMN.EQ.2)GOTO 752
                     GOTO 76

C----------------------------------
C       KS3 only to be added
733                  KVOLT=KVOLT+1
                     LIFA(KVOLT)=KS3

C***DIAG
C***D      WRITE(20,9603)KS3,IKK
C***D9603  FORMAT(' FACE',I4,' ADDED TO THE NATURE CROSSING EDGE',I4)
C***

                     GOTO 751

C----------------------------------
C       KS3 replaces KS4 at KF4
734                  LIFA(KF4)=KS3
C***DIAG
C***D      WRITE(20,9604),KS3,KS4,IKK
C***D9604  FORMAT(
C***D    &' FACE',I4,' REPLACES FACE',I4,' IN THE NATURE CROSSING EDGE',I4)
C***

C       Change of face cannot lead to pen up or down
                     GOTO 76

C----------------------------------
C       Face KS3 is to be removed, what about KS4
735                  IF(KF4)736,737,738

C       KS4 replaces KS3 at KF3
736                  LIFA(KF3)=KS4

C***DIAG
C***D      WRITE(20,9604)KS4,KS3,IKK
C***

                     GOTO 76

C----------------------------------
C       Face KS3 only to be removed
737                  LIFA(KF3)=-1

C***DIAG
C***D      WRITE(20,9607)KS3,IKK
C***D9607  FORMAT(' FACE',I4,' REMOVED FROM NATURE CROSSING EDGE',I4)
C***
                     GOTO 753

C----------------------------------
C       Both faces to be removed
738                  LIFA(KF3)=-1
                     LIFA(KF4)=-1

C***DIAG
C***D      WRITE(20,9608)KS3,KS4,IKK
C***D9608  FORMAT(
C***D    &' FACES',I4,' AND',I4,' REMOVED FROM NATURE CROSSING EDGE',I4)
C***

                     NFCMN=NFCMN-2
                     GOTO 754

C----------------------------------
C       No action on KS3, what about KS4
74                   IF(KF4)741,76,742

C       Adding face KS4 only
741                  KVOLT=KVOLT+1
                     LIFA(KVOLT)=KS4

C***DIAG
C***D      WRITE(20,9603)KS4,IKK
C***
                     GOTO 751

C----------------------------------
C       Removing face KS4 only
742                  LIFA(KF4)=-1

C***DIAG
C***D      WRITE(20,9607)KS4,IKK
C***
                     GOTO 753

C----------------------------------
C       Adding a face, increment NFCMN and see if to put pen up
751                  NFCMN=NFCMN+1
                     IF(NFCMN.NE.1)GOTO 76

C       Here after adding two faces
752                  RIO=RIORD(J,1)
C***4.7.2 3D COORDS
                     XX=DXI*RIO+XI1
                     YY=DYI*RIO+YI1
                     ZZ=DZI*RIO+ZI1
C   PLOT A LINE
                     CALL PLOTTR(XPR,YPR,ZPR,XX,YY,ZZ,1)
C   SAVE LAST POSITION
                     XPR=XX
                     YPR=YY
                     ZPR=ZZ
                     GOTO 76

C----------------------------------
C       Removing a face, decrement NFCMN and see if to put pen down
753                  NFCMN=NFCMN-1
C       Here after removing two faces
754                  IF(NFCMN.NE.0)GOTO 76
C     IT WAS THE LAST, PUT DOWN THE PEN
                     RIO=RIORD(J,1)
C***4.7.2
                     XX=DXI*RIO+XI1
                     YY=DYI*RIO+YI1
                     ZZ=DZI*RIO+ZI1
                     IF(NTRAT.NE.0)GOTO 755
C***3.9   JUST SAVE POSITION
                     XPR=XX
                     YPR=YY
                     ZPR=ZZ
                     GOTO 76

C     DRAW INVISIBLE PART OF EDGE DASHED
C***4.7.2
755                  CALL PLOTTR(XPR,YPR,ZPR,XX,YY,ZZ,-20)
C   SAVE LAST POSITION
                     XPR=XX
                     YPR=YY
                     ZPR=ZZ
76              CONTINUE

C--------------------------------------------
C     PLOT LAST BIT OF EDGE, IF ANY
77              IF(NFCMN.NE.0)GOTO 771
C***4.7.2
                CALL PLOTTR(XPR,YPR,ZPR,XI2,YI2,ZI2,1)
                GOTO 772

771             IF(NTRAT.NE.0)
C***4.7.2
     &          CALL PLOTTR(XPR,YPR,ZPR,XI2,YI2,ZI2,-20)
  772 CONTINUE
C***DIAG
C***D      WRITE(20,9906)KS2,I
C***D9906  FORMAT(' NATURE NEAR VERTEX',I4,' AT END OF EDGE',I4)
C***D      IF(NFCMN.EQ.0)WRITE(20,9901)
C***D      IF(NFCMN.NE.0)WRITE(20,9902)(LIFA(K),K=1,KVOLT)
C***

C----------------------------------------------------
                IF (INIZIO(KS2).NE.0) GO TO 80
C IF THE FINAL VERTEX IS UNKNOWN, THE NATURE OF THE FINAL VERTEX IS STORED
C  IN NOFA
                INIZIO (KS2 )=LIR
                IF (NFCMN.EQ.0) GO TO 791
C   THE ADJACENT FACES ARE NOT STORED

                DO 79 J=1,KVOLT
                     LIF=LIFA(J)
                     IF (LIF.EQ.(-1)) GO TO 79

C***3.6   NO LONGER PACK
                     LIB=IPTR1(LIF)
                     LIB1=IPTR2(LIF)

                     DO 78 K1=LIB,LIB1
                          IF(ISIDOR(K1).EQ.KS2)GOTO 781
78                   CONTINUE
                     NOFA(LIR)=LIF
                     IF(LIR.LT.MED)LIR=LIR+1
                     GOTO 79
781                  CONTINUE

C***DIAG
C***D      WRITE(20,9907)LIF,KS2
C***D9907  FORMAT(' FACE',I4,' ADJACENT TO VERTEX'
C***D    &,I4,' NOT INCLUDED IN NATURE')
C***

  79            CONTINUE

C------------
  791           NOFA(LIR)=-1
                IF(LIR.LT.MED)LIR=LIR+1
   80      CONTINUE

C---------------------
  85  CONTINUE

 1000 RETURN

      END
