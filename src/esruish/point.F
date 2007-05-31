C POINT.F of ESRUSHD, this file contains the following routines:

C POINT1: Decides if a point is within a face boundary.
C POINT2: Decides if a point is within a shadow boundary.
C INWPT1: Decides if a point is within a internal surface 
C         transparent section.

C ************* POINT1 **********************
C 'POINT1' determines if a specified point is within or outwith any
C specified object body face boundary.
      SUBROUTINE POINT1(IS,XX,ZZ,IANS)
      INCLUDE 'building.h'
      COMMON/G1/X(MTV),Y(MTV),Z(MTV),NSUR,JVN(MS,MV),NVER(MS),NTV
      COMMON/G1T/XFT(MV),ZFT(MV),XWFT(MG,4),ZWFT(MG,4)
      DIMENSION C(2*MV+2)
      IANS=0

C Determine if point XX,ZZ is within this face.
      NP=NVER(IS)
      DO 10 I=1,NP
        II=2*I-1
        C(II)=XFT(I)
        II=II+1
        C(II)=ZFT(I)
   10 CONTINUE
      LAST1=2*NP+1
      LAST2=2*NP+2
      C(LAST1)=XFT(1)
      C(LAST2)=ZFT(1)
      LN=0
      IP=NP+1
      DO 20 I=2,IP
        N=2*I
        IF((C(N-2)-ZZ)*(ZZ-C(N)))20,1,2
    2   IF((ZZ-C(N-2))*(C(N-1)-C(N-3))/(C(N)-C(N-2))+C(N-3)-XX)20,3,4
    1   IF(C(N-2)-C(N))5,6,7
    6   IF((C(N-3)-XX)*(XX-C(N-1)))20,3,3
    5   IF((ZZ-C(N-2))*(C(N-1)-C(N-3))/(C(N)-C(N-2))+C(N-3)-XX)20,3,8
    7   IF((ZZ-C(N-2))*(C(N-1)-C(N-3))/(C(N)-C(N-2))+C(N-3)-XX)20,3,9
    9   LN=LN-2
    8   LN=LN-1
    4   LN=LN+2
   20 CONTINUE
      IF((LN/4)*4.NE.LN)goto 3
      goto 11
    3 IANS=1
   11 RETURN
      END

C ************* POINT2 **********************
C 'POINT2' decides if a specified point (XX,ZZ) is within or 
C outwith any shadow boundary from common SHAD2.
      SUBROUTINE POINT2(XX,ZZ,IANS)
      COMMON/SHAD2/ISHD,XS(6),ZS(6)
      DIMENSION C(14)

      IANS=0
      NP=6
      DO 10 I=1,NP
        II=2*I-1
        C(II)=XS(I)
        II=II+1
        C(II)=ZS(I)
   10 CONTINUE
      LAST1=2*NP+1
      LAST2=2*NP+2
      C(LAST1)=XS(1)
      C(LAST2)=ZS(1)
      LN=0
      IP=NP+1
      DO 20 I=2,IP
        N=2*I
        IF((C(N-2)-ZZ)*(ZZ-C(N)))20,1,2
    2   IF((ZZ-C(N-2))*(C(N-1)-C(N-3))/(C(N)-C(N-2))+C(N-3)-XX)20,3,4
    1   IF(C(N-2)-C(N))5,6,7
    6   IF((C(N-3)-XX)*(XX-C(N-1)))20,3,3
    5   IF((ZZ-C(N-2))*(C(N-1)-C(N-3))/(C(N)-C(N-2))+C(N-3)-XX)20,3,8
    7   IF((ZZ-C(N-2))*(C(N-1)-C(N-3))/(C(N)-C(N-2))+C(N-3)-XX)20,3,9
    9   LN=LN-2
    8   LN=LN-1
    4   LN=LN+2
   20 CONTINUE
      IF((LN/4)*4.NE.LN)goto 3
      goto 11
    3 IANS=1
   11 RETURN
      END


C  'INWPT1' decides if a specified point is within or outwith
C  the internal surface transparent section.
      SUBROUTINE INWPT1(IS,IG,XX,ZZ,IANS)
      INCLUDE 'building.h'
      COMMON/G2C/NSA(MS),XWPC(MS,MG,4),ZWPC(MS,MG,4)
      DIMENSION C(2*MV+2)
      IANS=0

C Determine if point XX,ZZ is within or outwith this face.
      NP=4
      DO 10 I=1,NP
        II=2*I-1
        C(II)=XWPC(IS,IG,I)
        II=II+1
        C(II)=ZWPC(IS,IG,I)
   10 CONTINUE
      LAST1=2*NP+1
      LAST2=2*NP+2
      C(LAST1)=XWPC(IS,IG,1)
      C(LAST2)=ZWPC(IS,IG,1)
      LN=0
      IP=NP+1
      DO 20 I=2,IP
        N=2*I
        IF((C(N-2)-ZZ)*(ZZ-C(N)))20,1,2
    2   IF((ZZ-C(N-2))*(C(N-1)-C(N-3))/(C(N)-C(N-2))+C(N-3)-XX)20,3,4
    1   IF(C(N-2)-C(N))5,6,7
    6   IF((C(N-3)-XX)*(XX-C(N-1)))20,3,3
    5   IF((ZZ-C(N-2))*(C(N-1)-C(N-3))/(C(N)-C(N-2))+C(N-3)-XX)20,3,8
    7   IF((ZZ-C(N-2))*(C(N-1)-C(N-3))/(C(N)-C(N-2))+C(N-3)-XX)20,3,9
    9   LN=LN-2
    8   LN=LN-1
    4   LN=LN+2
   20 CONTINUE
      IF((LN/4)*4.NE.LN)goto 3
      goto 11
    3 IANS=1
   11 RETURN
      END








