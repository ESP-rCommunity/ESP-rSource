      SUBROUTINE FATT(CXYZ,SXYZ,NC,TRA)

C SUBROUTINE FATT COMPUTES A MATRIX EQUIVALENT TO A SCALE FACTOR TRANSFORMATION

      DIMENSION SXYZ(*),CXYZ(*),TRA(4,4),COM(4,4),CX(4)
      logical close1,close2,close3

      NM=NC+1
      DO 5 J=1,NM
           DO 2 I=1,NM
    2           TRA(I,J)=0
    5      TRA(J,J)=SXYZ(J)
      TRA(NM,NM)=1.0
C IF CENTRE OF SCALE IS ZERO  OMIT 'SHUFFLE'
      call eclose(CXYZ(1),0.0,0.001,close1)
      call eclose(CXYZ(2),0.0,0.001,close2)
      call eclose(CXYZ(3),0.0,0.001,close3)
      IF(close1.AND.close2.AND.close3)GOTO 10
      DO 1 I=1,NC
    1      CX(I)=-CXYZ(I)
      CALL TRASL(CX,NC,COM)
      CALL PROMAT(NM,COM,TRA,TRA)
      CALL TRASL(CXYZ,NC,COM)
      CALL PROMAT(NM,TRA,COM,TRA)
      RETURN
   10 CONTINUE
      END

C-------------------------------------------------------
      SUBROUTINE KDET (A,N1,N2,N3,DET1)

C   COMPUTES DETERMINANT FOR POINT1 TO BE ABOVE OR BELOW
C   PLANE THROUGH POINTS 2 3 4

      DIMENSION A(4,4)

      DO 10 I=1,3
           DO 10 J=1,3
   10           A(I,J)=A(I,J)-A(4,J)
      DET1=A(1,1)*(A(2,2)*A(3,3)-A(2,3)*A(3,2))-A(1,2)*(A(2,1)*A(3,3)
     &    -A(2,3)*A(3,1))+A(1,3)*(A(2,1)*A(3,2)-A(2,2)*A(3,1))
      RETURN
      END

C-------------------------------------------------------------
      SUBROUTINE PROMAT(N,A,B,C)

C   SUBROUTINE PROMAT PROVIDES A MATRIX-MATRIX MULTIPLICATION

      DIMENSION A(4,4),B(4,4),C(4,4),D(4,4)

      DO 1 I=1,4
           DO 1 J=1,4
1               D(I,J)=0
      DO 2 I=1,N
           DO 2 J=1,N
                DO 2 K=1,N
2                    D(I,J)=D(I,J)+A(I,K)*B(K,J)
      DO 3 I=1,N
           DO 3 J=1,N
3               C(I,J)=D(I,J)
      RETURN
      END

C-----------------------------------------------------------
      SUBROUTINE PROPA(COSD,NC,APP,IHV)

C   SUBROUTINE PROPA COMPUTES A MATRIX EQUIVALENT TO A PARALLEL PROJECTION
C   IHV=1 PROJ ONTO VERT(YZ) PLANE
C   IHV=2 PROJ ONTO HORIZ(-XZ->YZ)PLANE

      DIMENSION COSD(4),APP(4,4)

      NM=NC+1
      DO 1 I=1,NM
           DO 2 J=1,NM
2               APP(I,J)=0.
1     APP(I,I)=1.
      APP(1,2)=-COSD(2)/COSD(1)
      APP(1,NC)=-COSD(NC)/COSD(1)
C***V3.4.1
C   HORIZ. OBLIQUE PARALLEL
C   CONVERT -X TO NEW Z FOR DISPLAY
      IF(IHV.EQ.1)GO TO 10
      APP(3,3)=COSD(1)/COSD(3)
      APP(1,3)=-1.
  10  RETURN
      END

C----------------------------------------------------------------
      SUBROUTINE PROSP(CXYZ,NC,APR)

C   SUBROUTINE PROSP COMPUTES A MATRIX EQUIVALENT TO A PERSPECTIVA PROJECTION

      DIMENSION CXYZ(4),APR(4,4)

      NM=NC+1
      DO 1 I=1,NM
           DO 2 J=1,NM
2                APR(I,J)=0.
1          APR(I,I)=CXYZ(1)
      APR(1,2)=-CXYZ(2)
      APR(1,NC)=-CXYZ(NC)
      APR(1,NM)=-1.
      RETURN
      END

C-------------------------------------------------
      SUBROUTINE PROVM(VE,AME,VU,NC)

C   SUBROUTINE PROVM PERFORMS A VECTOR-MATRIX MULTIPLICATION

      DIMENSION VE(3),VU(3),AME(4,4),V(4)

      NM=NC+1
      DO 11 I=1,NM
           VV=0.
           DO 1 J=1,NC
1               VV=VV+AME(J,I)*VE(J)
11         V(I)=VV+AME(NM,I)
      DO 2 I=1,NC
2          VU(I)=V(I)/V(NM)
      RETURN
      END

C------------------------------------------------------------
      SUBROUTINE ROT(CXYZ,QQQ,NC,TRA)

      DIMENSION TRA(4,4),TIN(4,4),T(4,4),T1(4,4),R(4,4),R1(4,4),R1T(4,4)
        DIMENSION RP(4,4),CXYZ(4),CX(4)
        DIMENSION QQQ(3)

C  SUBROUTINE ROT COMPUTES A MATRIX EQUIVALENT TO A ROTATION TRANSFORMATION

      NM=NC+1
        Q1=QQQ(1)
        Q2=QQQ(2)
        Q3=QQQ(3)
      N=3
        QQ=SQRT(Q1*Q1+Q2*Q2+Q3*Q3)
        COST=Q3/QQ
        SENT=SQRT(Q1*Q1+Q2*Q2)/QQ
        IF(SENT)100,90,100
90      COSF=1.
      SENF=0.
      GO TO 110
100     COSF=-Q2/(QQ*SENT)
        IF(ABS(COSF).GT.1.)COSF=SIGN(1.,COSF)
        SENF=SIN(ACOS(COSF))
110   R1(1,1)=COSF
      R1(1,2)=-COST*SENF
      R1(1,3)=SENF*SENT
      R1(2,1)=SENF
      R1(2,2)=COSF*COST
      R1(2,3)=-COSF*SENT
      R1(3,1)=0.
      R1(3,2)=SENT
      R1(3,3)=COST
      RP(1,1)=COS(QQ)
      RP(1,2)=-SIN(QQ)
      RP(1,3)=0.
      RP(2,1)=SIN(QQ)
      RP(2,2)=COS(QQ)
      RP(2,3)=0.
      RP(3,1)=0.
      RP(3,2)=0.
      RP(3,3)=1.
      DO 2 I=1,NM
           DO 2 J=1,NM
                R1T(I,J)=R1(I,J)
    2      TRA(I,J)=0.
      DO 30 I1=1,3
           DO 30 I2=1,3
30              R1T(I1,I2)=R1(I2,I1)
    4 CALL PROMAT(  4,R1,RP,TIN)
      CALL PROMAT(  4,TIN,R1T,R)
      DO 5 I=1,NC
           R(NM,I)=0.0
    5      R(I,NM)=0.0
      R(NM,NM)=1.0
      DO 7 II=1,3
7          CX(II)=-CXYZ(II)
      CALL TRASL(CX  ,NC,T)
      CALL PROMAT(   4 ,T,R,TIN)
      CALL TRASL (CXYZ,NC,T1)
      CALL PROMAT(   4 ,TIN,T1,TRA)
      RETURN
      END

C-------------------------------------------------------------------------
      SUBROUTINE TRASL(AV,NC,APP)

C  SUBROUTINE TRASL COMPUTES A MATRIX EQUIVALENT TO A TRANLATION TRANSFORMATION

      DIMENSION AV(4),APP(4,4)

      NM=NC+1
      DO 1 I=1,NM
           DO 2 J=1,NM
2               APP(I,J)=0.0
1     APP(I,I)=1.0
      DO 3 J=1,NC
3          APP(NM,J)=AV(J)
      RETURN
      END
