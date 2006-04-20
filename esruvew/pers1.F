      SUBROUTINE PERS1
 
#include "viewer.h"
#include "viewer.h2"
 
      LOGICAL ER,PG1,PG2,PG3,PG4
 
C-------------------------------------------------------
C PICTURE SIZE IS NOW +-1000 IN X
      BX=1000.
      BY=1000.*BILDY/BILDX
 4    EPS=BX*BY*1.E-8
 
C-------------------------------------------------------
C FIND IOCV FOR EACH EDGE AND SET IT:
C***4.7 REDEFINE
C    -3   OUTSIDE COV AND BOUNDS 2 BACK FACES
C    -1   INSIDE  COV            2
C     0   INSIDE COV, CONCAVE,BACK AND FRONT FACES
C     1   INSIDE COV, ON VIEW ( 1 FRONT FACE)(SILHOUETTE EDGE)
C     2   INSIDE COV, ON VIEW ( 2 FRONT FACES)
C     3   OUTSIDE COV, AT LEAST 1 FRONT FACE
 
      DO 5 I=1,NR
C EDGES OUTSIDE CONE OF VISION ARE NOT ON VIEW
 
         KS1=IREPU1(I)
         KS2=IREPU2(I)
 
C USE X Y Z
         X1=PUPRVI(KS1,1)
         X2=PUPRVI(KS2,1)
         Y1=PUPRVI(KS1,2)
         Y2=PUPRVI(KS2,2)
         Z1=PUPRVI(KS1,3)
         Z2=PUPRVI(KS2,3)

         IOCV(I)=-3
C TEST FOR OUTSIDE VIEW
         IF(Y1.LT.-BX.AND.Y2.LT.-BX)GO TO 5
         IF(Y1.GT.BX .AND.Y2.GT.BX)GO TO 5
         IF(Z1.LT.-BY.AND.Z2.LT.-BY)GO TO 5
         IF(Z1.GT.BY .AND.Z2.GT.BY)GO TO 5
         IOCV(I)=-1
 5    CONTINUE
 
C-------------------------------------------------------
 
      IF(NTRAT.EQ.2)GO TO 11
 
C-------------------------------------------------------
C IF SITRA(I).GT.0 , FACE I IS ON VIEW WITH RESPECT TO ITS OWN VOLUME
 
C OMIT GLASS BODIES
C FACES NOT RELEVANT OR DONT EXIST
 
      DO 15 IBB=1,NB
         IF(IABS(IBODG(IBB)) .EQ. 3)GO TO 15
 
         IF1=IBF1(IBB)
         IF2=IBF2(IBB)
 
C IF NO FACES ATTACHED TO BODY
         IF(IF1.LE.0)GO TO 15
 
         DO 10 I=IF1,IF2
 
            LIB=IPTR1(I)
 
            I1=ISIDOR(LIB)
            I2=ISIDOR(LIB+1)
            I3=ISIDOR(LIB+2)
 
            X1=PUPRVI(I1,1)
            Y1=PUPRVI(I1,2)
            Z1=PUPRVI(I1,3)
            X2=PUPRVI(I2,1)
            Y2=PUPRVI(I2,2)
            Z2=PUPRVI(I2,3)
            X3=PUPRVI(I3,1)
            Y3=PUPRVI(I3,2)
            Z3=PUPRVI(I3,3)
            SITRA(I)=Y1*Z2-Y2*Z1+Y2*Z3-Y3*Z2+Y3*Z1-Y1*Z3
 10      CONTINUE
 
C IF SITRA(I).GT.0. EDGE I IS ON VIEW WITH RESPECT TO ITS OWN DIHEDRAL
 15   CONTINUE
 
 
C-------------------------------------------------------
C EXAMINE EACH EDGE
C IF GLASS BODY DONT TEST FACES , JUST REVERSE IOCV
 
      DO 25 IBB=1,NB
 
         IE1=IBE1(IBB)
         IE2=IBE2(IBB)
         IF(IABS(IBODG(IBB)) .EQ. 3)GO TO 22
 
         DO 20 I=IE1,IE2
 
C FACES OF EDGE
            KS1=IREFA1(I)
            KS2=IREFA2(I)
C CONVEX EDGE?
            ER=IREFAC(I).EQ.0
 
C FACE ON VIEW
            PG1=SITRA(KS1).GT.EPS
            PG2=SITRA(KS2).GT.EPS
C NEARLY EDGE-ON?
            PG3=SITRA(KS1).GE.-EPS.AND.SITRA(KS1).LE.EPS
            PG4=SITRA(KS2).GE.-EPS.AND.SITRA(KS2).LE.EPS
 
C MARK EDGES BELONGING TO AT LEAST ONE ON-VIEW FACE

            IF( (PG1 .AND. .NOT.PG2) .OR.
     &           (PG2 .AND. .NOT.PG1) .OR.
     &           (PG3 .AND. PG4)           )THEN
               IOCV(I)=-IOCV(I)
            ELSE IF( PG1 .AND. PG2 .AND. IOCV(I).EQ.-1)THEN
               IOCV(I)=2
            ELSE IF( PG1 .AND. PG2 .AND. IOCV(I).EQ.-3)THEN
               IOCV(I)=3
            ENDIF
 
C DETECT SPECIAL CASE OF EDGE RELEVANT BUT NOT ON-VIEW
            IF( (.NOT.ER .AND. .NOT.PG1 .AND. PG2
     &           .OR. .NOT.ER .AND. PG1 .AND. .NOT.PG2)
     &           .AND. IOCV(I).EQ.1 ) IOCV(I)=0
 
 20      CONTINUE
 
C-------------------------------------------------------
         GO TO 25
 
C GLASS BODY: ALL EDGES ON VIEW
 22      DO 24 I=IE1,IE2
            IOCV(I)=-IOCV(I)
 24      CONTINUE
 
 25   CONTINUE
  
C-------------------------------------------------------
C IF WE HAVE ONE CONVEX OBJECT ONLY, IT IS IMMMEDIATELY PLOTTED
C OR IF NTRAT EQ 2 - NO HIDDEN LINE REMOVAL
 
C IF MORE THAN ONE BODY CALL HLR
      IF(NB.GT.1)GO TO 950
 
C IF THE BODY IS GLASS, DRAW IT
      IF(IABS(IBODG(1)) .EQ. 3)GO TO 11
 
C IF THE ONE BODY IS CONCAVE CALL HLR
      IF(IES(1).EQ.0)GO TO 950
 
C OTHERWISE DRAW THIS BODY
C OR IF NTRAT=2,NO HLR, DRAW ALL BODIES
 
      ibc = 0
 11   DO 50 I=1,NB
         IBB=I
         if (ibc .ne. ibcol(i)) then
            ibc = ibcol(i)
            call plottc(ibc)
         endif
         CALL PERS2(IBB)
 50   CONTINUE
 
      RETURN
 
C-------------------------------------------------------
 950  CALL HLR
 
      RETURN
 
      END
