      SUBROUTINE VERT(ISID,*)

C ISID is the number of the face to create
C       $ is the exit label on an error

#include "viewer.h2"
      COMMON/OUTIN/IUOUT,IUIN

      DOUBLE PRECISION SIP,COS1,COS2,COS3,AB,S1,S2,S3
      character outs*124

C-------------------------------------------------
      LCNT = 0
      LIBS = LIB2
 1000 CONTINUE
      AB=0.
      COS1=0.
      COS2=0.
      COS3=0.
      LT=LIB

C-------------------------------------------------
C   COORDS OF FIRST VERTEX
      K0=ISIDOR(LIB)
      X0=PUNT(K0,1)
      Y0=PUNT(K0,2)
      Z0=PUNT(K0,3)

C  EXAMINATION OF EVERY COUPLE OF VERTICES
      DO 21 J=LIB,LIB1
        K1=J+1
        IF(J.EQ.LIB1)K1=LT
        IF(ISIDOR(K1).GE.NP) GO TO 10

        ISIDOR(K1)=NP+NP-ISIDOR(K1)
        K1=LT
10      IF(LIB2.GT.MB2)then
          call edisp(iuout,'MAX EDGES PER BODY EXCEEDED... ')
          return
        endif

C The ordered pair of vertices is stored in IORD1(LIB2)
        IF(ISIDOR(J).GT.ISIDOR(K1))GO TO 11

        IORDL(LIB2)=ISIDOR(J)
        IORDH(LIB2)=ISIDOR(K1)
        IORDR(LIB2)=1
        GO TO 12

  11    IORDL(LIB2)=ISIDOR(K1)
        IORDH(LIB2)=ISIDOR(J)
        IORDR(LIB2)=0

C  THE FACE CORRESPONDING TO THE COUPLE IS STORED IN IORD2(LIB2)
12      IORD2(LIB2)=ISID

        LIB2=LIB2+1
C  JUMP IF THE FIRST VERTEX OF THE COUPLE IS NOT THE FIRST ONE OF THE PRESENT
C  PERIMETER
        IF (LT.NE.K1) GO TO 21

C COMPUTING THE INWARD NORMAL VECTOR
C DO EACH EDGE OF THIS FACE IN TURN
        DO 13 K=LT,J
          K1=K+1
C IF LAST EDGE,POINT TO FIRST VERTEX
          IF(K.EQ.J)K1=LT
          K2=ISIDOR(K)
          K3=ISIDOR(K1)
C COORDS RELATIVE TO FIRST POINT OF SURFACE
          X2=PUNT(K2,1)-X0
          Y2=PUNT(K2,2)-Y0
          Z2=PUNT(K2,3)-Z0
          X3=PUNT(K3,1)-X0
          Y3=PUNT(K3,2)-Y0
          Z3=PUNT(K3,3)-Z0

C   AVERAGE NORMAL TO ALL SURFACE IS TOTAL OF(COS1,COS2,COS3)
          SIP=Y2*Z3-Y3*Z2
          COS1=COS1+SIP
          AB=AB+ABS(SIP)
          SIP=Z2*X3-Z3*X2
          COS2=COS2+SIP
          AB=AB+ABS(SIP)
          SIP=X2*Y3-X3*Y2
          COS3=COS3+SIP
          AB=AB+ABS(SIP)

C Compute normal to plane formed by first three points only
C NORMAL IS (S1,S2,S3)
C DO CALCULATION ONLY WHEN DOING 2ND EDGE ABOVE.
          IF(K .EQ. LIB+1)THEN

C THIRD EDGE OF TRIANGLE JOINS VERTICES 3 AND 1
            K2=ISIDOR(LT)
            X2=PUNT(K2,1)-X0
            Y2=PUNT(K2,2)-Y0
            Z2=PUNT(K2,3)-Z0
            SIP=Y3*Z2-Y2*Z3
            S1=COS1+SIP
            SIP=Z3*X2-Z2*X3
            S2=COS2+SIP
            SIP=X3*Y2-X2*Y3
            S3=COS3+SIP
          ENDIF
13      CONTINUE

        LT=J+1
21    CONTINUE

C-------------------------------------------------
C  SCALAR PRODUCT BETWEEN THE NORMAL TO THE FACE AND
C    THE NORMAL TO THE FIRST TRIANGLE

      IS=ISID-NF
      IF(IS.GT.MSB)then
        call edisp(iuout,'Maximum surfaces per body exceeded... ')
        RETURN 1
      endif

C SAVE AVERAGE NORMAL FOR LATER
      ZZZ(IS,1)=COS1
      ZZZ(IS,2)=COS2
      ZZZ(IS,3)=COS3
      IPTR1(ISID)=LIB
      IPTR2(ISID)=LIB1

C SCALAR PRODUCT OF TWO NORMALS ? IS COS OF ANGLE BETWEEN THEM
      SIP=S1*COS1+S2*COS2+S3*COS3

C   AB IS 'LENGTH' OF NORMALS(ALWAYS POSITIVE AND SMALL)
      AB=AB*(ABS(S1)+ABS(S2)+ABS(S3))*1.E-5

C   ?IF SIP NEGATIVE,NORMALS ARE IN OPPOSITE DIRECTIONS
C   ?IF SIP POSITIVE, NORMALS IN SAME DIRECTION
      IF(SIP.LT.-AB)GOTO 23

      IF(SIP.GT.AB)GOTO 22

C-------------------------------------------------
C   IF SIP ZERO, THEN FIRST TRAINGLE IS OF COLINEAR POINTS
      NN=LIB1-LIB+1
C   IF 3 VERTICES, DONT REPEAT, GO TO ERROR
      IF(NN.LE.3) GO TO 999

      LCNT = LCNT + 1
      IF (LCNT .GE. NN) GOTO  999

      K1 = ISIDOR(LIB)
      DO 1001 J1 = LIB,LIB1-1
        ISIDOR(J1) = ISIDOR(J1+1)
 1001 CONTINUE
      ISIDOR(LIB1) = K1
      LIB2 = LIBS

C RETURN TO START THIS SURFACE AGAIN
C WITH REORDERED VERTICES
      GOTO  1000

C ERROR
  999 CONTINUE
      WRITE(outs,998)IS
  998 FORMAT(' All vertices in surface',I4,' are colinear')
      call edisp(iuout,outs)
C RETURN TO CARRY ON READING FILE
      RETURN

C-------------------------------------------------
C       Normal is wrong way round; exchange first two vertices
22    CONTINUE
      K1=ISIDOR(LIB)
      ISIDOR(LIB)=ISIDOR(LIB+1)
      ISIDOR(LIB+1)=K1

23    LIB=LIB1+1

      RETURN
      END

C-----------------------------------------------------------------
      SUBROUTINE BODY(NP1,NP2,NF1,NF2)
#include "viewer.h2"
      COMMON/OUTIN/IUOUT,IUIN

      DIMENSION AV(3,3)
      character outs*124
C (old)      ENTRY BODY(NP1,NP2,NF1,NF2)

C   SHRINK ALL VERTICES OF THIS BODY
      CALL SHRINK(NP1,NP2)

C   SET FIRST AND LAST FACES
      IBF1(NB)=NF1
      IBF2(NB)=NF2
      NF=NF2
C***3.9.2
      IF(NF.GT.MSF)CALL BERROR(2,NB)

C   SET FIRST AND LAST POINTS OF BODY
      IBP1(NB)=NP1
      IBP2(NB)=NP2
      NP=NP2
C***3.9.2
      IF(NP.GT.MPT)CALL BERROR(1,NB)

      LIB2=LIB2-1
C   NO OF EDGES N BODY
      NR1=LIB2/2
C  FIRST EDGE
      N1=NR+1
C***3.7
C   LAST EDGE
      NR=NR+NR1
C   SAVE FIRST AND LAST EDGE OF THIS BODY
      IBE1(NB)=N1
      IBE2(NB)=NR

C   IF OVERFLOW.....
      IF(NR.GT.MED)CALL BERROR(3,NB)

C***3.8 SAVE CONVEX
      IES(NB)=1

C-------------------------------------------------
C  REORDERING OF COUPLES OF VERTICES ACCORDING TO(A) THE FIRST NODE
C  (B) THE SECOND NODE,(C) THE INVERSION LABEL

      MA=LIB2-1

      DO 26 I=1,MA
        MIN=I+1
        DO 26 J=MIN,LIB2
C***3.6
          IF(IORDL(I)-IORDL(J)) 26,31,25

  31      IF(IORDH(I)-IORDH(J)) 26,32,25

  32      IF(IORDR(I)-IORDR(J)) 26,26,25

C   REVERSE ORDER
  25      NTEMPL=IORDL(I)
          NTEMPH=IORDH(I)
          NTEMPR=IORDR(I)
          NTE=IORD2(I)

          IORDL(I)=IORDL(J)
          IORDH(I)=IORDH(J)
          IORDR(I)=IORDR(J)
          IORD2(I)=IORD2(J)

          IORDL(J)=NTEMPL
          IORDH(J)=NTEMPH
          IORDR(J)=NTEMPR
          IORD2(J)=NTE
26    CONTINUE

C-------------------------------------------------
C       Determination of IREPU and IREFA. Note that running along edge I from
C       vertex II1(IREPU(I)) to II2(IREPU(I)) face II1(IREFA(I)) is to the left
C       and face II2(IREFA(I)) to the right. This condition is essential
C       for concavity determinations.
        LIB2=1

      DO 60 I=1,NR1
C***3.6
        K3=IORDL(LIB2)
        K4=IORDH(LIB2)
        K5=IORDR(LIB2)

        J3=IORDL(LIB2+1)
        J4=IORDH(LIB2+1)
        J5=IORDR(LIB2+1)

        IF(K3.EQ.J3.AND.K4.EQ.J4.AND.K5.EQ.0.AND.J5.EQ.1)GO TO 28

C   INDEX RELATIVE TO OLD NP
        KS1=K3-(NP1-1)
        KS2=K4-(NP1-1)
C***3.6
        IF(K3.EQ.J3.AND.K4.EQ.J4.AND.K5.EQ.J5)GO TO 27

C   BERROR
        WRITE(outs,112)KS1,KS2,NB
112     FORMAT(' Unmatched edge from vertex',I4,' to',
     &                  I4,' of body',I4)
        call edisp(iuout,outs)
        LIB2=LIB2-1
        GOTO 60

C   BERROR
27      WRITE(outs,106)KS1,KS2,NB
106     FORMAT(' Edge twice in same direction from vertex',
     &           I4,' to',I4,' of body',I4)
        call edisp(iuout,outs)
        GOTO 60

28      II=N1+I-1

C***3.6
C   END VERTICES OF THIS EDGE
        IREPU1(II)=K3
        IREPU2(II)=K4

        K1=IORD2(LIB2+1)
        K2=IORD2(LIB2)

C***3.6
C   ADJACENT FACES TO EDGE
        IREFA1(II)=K1
        IREFA2(II)=K2

C  DET IS A MIXED PRODUCT AMONG THREE VECTORS,THE EDGE AND THE TWO NORMALS
C  TO THE ADJACENT FACES. IF DET.GE.0 THE EDGE IS CONVEX
52      DO 53 J=1,3
C  ZZZ INDEXED RELATIVE TO LAST NF
          AV(1,J)=ZZZ(K2-NF1+1,J)
          AV(2,J)=ZZZ(K1-NF1+1,J)
53      AV(3,J)=PUNT(K4,J)-PUNT(K3,J)

C***3.9.3  FIX ON DET
        DET=AV(1,1)*(AV(2,2)*AV(3,3)-AV(3,2)*AV(2,3))
     &     -AV(1,2)*(AV(2,1)*AV(3,3)-AV(3,1)*AV(2,3))
     &     +AV(1,3)*(AV(2,1)*AV(3,2)-AV(3,1)*AV(2,2))
        DAT=(ABS(AV(1,1))+ABS(AV(1,2))+ABS(AV(1,3)))*
     &      (ABS(AV(2,1))+ABS(AV(2,2))+ABS(AV(2,3)))*
     &      (ABS(AV(3,1))+ABS(AV(3,2))+ABS(AV(3,3)))

C***3.6
C   SET CONVEX
        IREFAC(II)=0

C    JUMP IF CONVEX, AVOIDING ROUNDING BERROR ON PLANE
        IF(DET.GE.-.00001*DAT) GO TO 59

C-------------------------------------------------
C  THE SOLID IS CONCAVE
        IES(NB)=0
C  THE EDGE  IS CONCAVE
C***3.6
        IREFAC(II)=1

C  THE ADJACENT VERTICES ARE CONCAVE
        ISIPU(K3)=1
        ISIPU(K4)=1

  59    CONTINUE

60    LIB2=LIB2+2

      RETURN
      END
