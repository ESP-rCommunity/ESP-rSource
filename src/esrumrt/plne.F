C ESP-r is free software.  You can redistribute it and/or
C modify it under the terms of the GNU General Public
C License as published by the Free Software Foundation 
C (version 2 orlater).

C ESP-r is distributed in the hope that it will be useful
C but WITHOUT ANY WARRANTY; without even the implied
C warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
C PURPOSE. See the GNU General Public License for more
C details.

C You should have received a copy of the GNU General Public
C License along with ESP-r. If not, write to the Free
C Software Foundation, Inc., 59 Temple Place, Suite 330,
C Boston, MA 02111-1307 USA.

C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      SUBROUTINE PLNE(NP,X,Y,Z,EQN,IERR)

      DIMENSION  X(4),Y(4),Z(4)
      DIMENSION  CG(3),VNORM(3),EQN(4)
      DIMENSION  EX(4),EY(4),EZ(4)
      DIMENSION  VNX(4),VNY(4),VNZ(4)
      DIMENSION  EV1(3),EV2(3),EV3(3)
      logical close

C THIS ROUTINE FINDS THE EQUATION TO A PLANE CONTAINING A POLYGON.
C THE POLYGON IS DEFINED AS A SET OF X Y Z VERTICIES TRACING IN ORDER
C THE EDGES OF THE POLYGON.
C THE PLANE WHOSE EQUATION IS RETURNED IS THE 'BEST FIT' PLANE PASSING
C THROUGH THE CENTROID OF THE SET OF VERTICES.

C THIS CENTROID POINT IS ALSO RETURNED.

C THE PLANE EQUATION IS IN THE FORM;

C       A*X + B*Y + C*Z = D

C WHERE THE VECTOR (A,B,C) IS THE UNIT NORMAL VECTOR TO THE PLANE.
C THE CONVENTION IS ADOPTED THAT THIS NORMAL WILL BE IN THE DIRECTION
C GIVEN BY THE RIGHT HAND ROTATION RULE REGARDLESS OF THE HANDEDNESS
C OF THE AXES DEFINING THE POLYGON VERTICIES.
C THIS MEANS THAT THIS HANDEDNESS MUST BE INPUT TO THE ROUTINE.
C D IS THE SMALLEST DISTANCE (I.E. NORMAL DISTANCE) FROM THE ORIGIN
C TO THE PLANE. THE DIRECTION FOR POSITIVE MEASUREMENT
C OF THIS DISTANCE IS THE DIRECTION FROM THE ORIGIN GIVEN BY THE
C UNIT NORMAL VECTOR FOR THE PLANE.

C THE ABOVE CONVENTION MEANS THAT FOR SOLID BODIES WHOSE FACES ARE
C DESCRIBED IN ANTI-CLOCKWISE FASHION (AS VIEWED FROM OUTSIDE THE BODY)
C THE UNIT NORMALS FOUND BY THIS ROUTINE WILL
C POINT OUTWARDS FROM THE BODY.

C       EQN(1) THE A COEFFICIENT OF THE EQUATION OF THE PLANE.
C       EQN(2) '   B   '
C       EQN(3) '   C   '
C       EQN(4) '   D   '
C       IERR   AN ERROR FLAG. IF .GE. 0 ALL O.K.
C            IF -1 THEN THE MAGNITUDE OF THE UNIT NORMAL IS .LT.10E-4
C            IF -2 THEN LESS THAN 3 POINTS IN THE POLYGON.
C            IF -3 THEN MORS IS OUT OF RANGE.

      IERR = 0

      DO 100 J = 1,NP
        J1 = J+1
        IF (J .EQ. NP)  J1 = 1
        EX(J) = X(J1) - X(J)
        EY(J) = Y(J1) - Y(J)
        EZ(J) = Z(J1) - Z(J)
  100 CONTINUE

      DO 200 J = 1,NP
        J1 = J+1
        IF (J .EQ. NP)  J1 = 1

        EV1(1) = EX(J)
        EV1(2) = EY(J)
        EV1(3) = EZ(J)

        EV2(1) = EX(J1)
        EV2(2) = EY(J1)
        EV2(3) = EZ(J1)

        EV3(1) = EV1(2)*EV2(3) - EV1(3)*EV2(2)
        EV3(2) = EV1(3)*EV2(1) - EV1(1)*EV2(3)
        EV3(3) = EV1(1)*EV2(2) - EV1(2)*EV2(1)

        SNORM = EV3(1)**2 + EV3(2)**2 + EV3(3)**2
        call eclose(SNORM,0.0,0.0001,close)
        IF (close)  GOTO  999
        SNORM = SQRT(SNORM)

        VNX(J) = EV3(1)/SNORM
        VNY(J) = EV3(2)/SNORM
        VNZ(J) = EV3(3)/SNORM
  200 CONTINUE

      CG(1) = 0.0
      CG(2) = 0.0
      CG(3) = 0.0
      DO 300 JJ = 1,NP
        CG(1) = CG(1) + X(JJ)
        CG(2) = CG(2) + Y(JJ)
        CG(3) = CG(3) + Z(JJ)
  300 CONTINUE
      CG(1) = CG(1)/NP
      CG(2) = CG(2)/NP
      CG(3) = CG(3)/NP

      VNORM(1) = 0.0
      VNORM(2) = 0.0
      VNORM(3) = 0.0
      DO 400 JJ = 1,NP
        VNORM(1) = VNORM(1) + VNX(JJ)
        VNORM(2) = VNORM(2) + VNY(JJ)
        VNORM(3) = VNORM(3) + VNZ(JJ)
  400 CONTINUE
      VNORM(1) = VNORM(1)/NP
      VNORM(2) = VNORM(2)/NP
      VNORM(3) = VNORM(3)/NP

      SNORM = SQRT(VNORM(1)**2+VNORM(2)**2+VNORM(3)**2)
      IF (SNORM .LT. 10E-4)  GOTO  999

      EQN(1) = VNORM(1)/SNORM
      EQN(2) = VNORM(2)/SNORM
      EQN(3) = VNORM(3)/SNORM

      EQN(4) = EQN(1)*CG(1)+EQN(2)*CG(2)+EQN(3)*CG(3)

      RETURN

  999 CONTINUE

      IERR = -1
      RETURN
      END


C ******************** MPLNEQN 
C MPLNEQN finds the equation EQN to a plane containing a polygon which
C is defined as a set of X Y Z verticies tracing in order the edges of 
C the polygon (this information is contained in PNTLST).  The plane is
C the best fit from PNTLST and its equation is in the form:
C       A*X + B*Y + C*Z = D
C where the vector (A<B<C) is the unit normal vecotr to the plane.
C This normal will be in the direction given by the right hand rotation
C rule MORS is the handedness of the axes defining the polygon
C verticies.  For solid bodies whose faces have been described anti-
C clockwise when viewed from the outside the unit normals within this
C routine will point outwards from the body.
C This version is sized for MRT in terms of PNTLST.
      SUBROUTINE MPLNEQN(ITRC,ITRU,MORS,ISTA,PNTLST,CG,EQN,IERR)
      INCLUDE 'building.h'

      DIMENSION PNTLST(1000),CG(3),VNORM(3),EQN(4),
     &          PNT(MV,3),E(MV,3),VN(MV,3),EV1(3),EV2(3),EV3(3)
      CHARACTER outs*124
      logical close

C INPUTS.

C    ISTA              START POINTER TO PNTLST.
C    PNTLST(ISTA)      NO. OF POINTS IN THE POLYGON.
C    PNTLST(ISTA+1)    FIRST VERTEX X COORD.
C    PNTLST(ISTA+2)    FIRST VERTEX Y COORD.
C    PNTLST(ISTA+3)    FIRST VERTEX Z COORD.
C    PNTLST(ISTA+4)    SECOND VERTEX X COORD. .....ETC.
C    MORS              (MODEL OR SCREEN) IS A FLAG TO INDICATE THE
C                      HANDEDNESS OF THE AXIS SYSTEM DEFINING THE POLYGON.
C                      IF MORS=1 FLAGS A RIGHT HANDED SYSTEM
C                                         E.G. MODEL COORDS.
C                      IF MORS=2 FLAGS A LEFT HANDED SYSTEM
C                                         E.G. SCREEN COORDS.

C OUTPUTS.
C    CG(1)  THE X COORD. OF THE CENTROID OF THE POLYGON.
C    CG(2)  '   Y   '
C    CG(3)  '      '
C    EQN(1) THE A COEFFICIENT OF THE EQUATION OF THE PLANE.
C    EQN(2) '   B   '
C    EQN(3) '   C   '
C    EQN(4) '   D   '
C    IERR   AN ERROR FLAG. IF .GE. 0 ALL O.K.
C           IF -1 THEN THE MAGNITUDE OF THE UNIT NORMAL IS .LT.10E-4
C           IF -2 THEN LESS THAN 3 POINTS IN THE POLYGON.
C           IF -3 THEN MORS IS OUT OF RANGE.

C INTERNAL VARIABLES.

C    PNT(MV,3)   AN ARRAY OF VERTEX POINTS.
C    E(MV,3)     AN ARRAY OF EDGE VECTORS.
C    VN(MV,3)    AN ARRAY OF NORMAL VECTORS.
C    EV*(3)       TEMPORARY STORAGE FOR VECTORS.
C    VNORM(1>3)   X Y Z COORDS OF THE UNIT NORMAL VECTOR TO THE PLANE.

      IERR=0

      NP=INT(PNTLST(ISTA))
      IF(NP.LT.3)THEN
        IERR=-2
        GO TO 999
      ENDIF

      IF(NP.GT.MV) NP=MV
      DO 20 J=1,NP
        J1=J+1
        IF(J1.GT.NP)J1=1
        INJ=ISTA+((J-1)*3)
        INJ1=ISTA+((J1-1)*3)
        DO 10 K=1,3
          KNJ=INJ+K
          KNJ1=INJ1+K
          PNT(J,K)=PNTLST(KNJ)
          E(J,K)=PNTLST(KNJ1)-PNTLST(KNJ)
   10   CONTINUE
   20 CONTINUE

      DO 50 J=1,NP
        J1=J+1
        IF(J1.GT.NP) J1=1
        DO 30 K=1,3
          EV1(K)=E(J,K)
          EV2(K)=E(J1,K)
   30   CONTINUE

        CALL CROSS(EV1,EV2,EV3)

        SNORM = 0.0
        SNORM=SQRT(EV3(1)**2+EV3(2)**2+EV3(3)**2)
        call eclose(SNORM,0.0,0.0001,close)
        IF (.NOT.close)  SNORM = SQRT(SNORM)
        DO 40 K=1,3
          VN(J,K) = 0.0
          IF (.NOT.close)  VN(J,K)=EV3(K)/SNORM
   40   CONTINUE
   50 CONTINUE

      CALL AVER(MV,NP,PNT,CG)

      CALL AVER(MV,NP,VN,VNORM)

      SNORM = 0.0
      SNORM=SQRT(VNORM(1)**2+VNORM(2)**2+VNORM(3)**2)
      IF(SNORM.LT.0.000001)THEN
        WRITE(outs,'(A,4F12.7)')' PLNEQN: SNORM=',SNORM,VNORM(1),
     &    VNORM(2),VNORM(3)
        CALL EDISP(ITRU,outs)
        WRITE(outs,'(A,3F12.7)')' PLNEQN: CG=',CG(1),CG(2),CG(3)
        CALL EDISP(ITRU,outs)
        IERR=-1
        GO TO 999
      ENDIF

      DO 80 J=1,3
        EQN(J)=VNORM(J)/SNORM
   80 CONTINUE

      EQN(4)=EQN(1)*CG(1)+EQN(2)*CG(2)+EQN(3)*CG(3)

      IF(MORS.EQ.1)THEN
        RETURN
      ELSEIF(MORS.EQ.2)THEN
        DO 100 J=1,4
          EQN(J)=-EQN(J)
  100   CONTINUE
        RETURN
      ELSE

C MORS flag not recognised.
        IERR=-3
        GO TO 999
      ENDIF

 999  WRITE(outs,'(A,I3)') ' PLNEQN: error flag IERR =',IERR
      CALL EDISP(ITRU,outs)

      RETURN
      END
