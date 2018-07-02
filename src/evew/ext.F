C----------------------------------------------------
      SUBROUTINE  EXT(NFILE,INUSE,ISCLOC,TEXT,LOOK)
#include "viewer.h2"

      COMMON/OUTIN/IUOUT,IUIN

      character text*25
      LOGICAL LOOK

C----------------------------------------------------
C   AXIS 1 = X  2 = Y  3 = Z( = REG)

      READ(INUSE,*,ERR = 401,END = 999)  NS,IEXT,Z0,Z1

      IF(IEXT.EQ.3)THEN
           J1 = 1
           J2 = 2
           J3 = 3
      ELSE IF(IEXT.EQ.1)THEN
           J1 = 2
           J2 = 3
           J3 = 1
      ELSE IF(IEXT.EQ.2)THEN
           J1 = 1
           J2 = 3
           J3 = 2
           ZZ = Z1
           Z1 = Z0
           Z0 = ZZ
      ELSE
           GOTO 401
      ENDIF

      NB = NB + 1
      IF(NB.GT.MBB)GOTO 415

      LOOK = .TRUE.

      NP1 = NP+1
      NP2 = NP+NS
      NF1 = NF+1

      READ(INUSE,*,ERR=404,END=999) (PUNT(I,J1),PUNT(I,J2),I=NP1,NP2)

      READ(INUSE,*,ERR=401,END=999)  (ISIDOR(K),K=LIB,LIB+NS-1)

C     MAKE BOTTOM PLANE,3D COORDS AND PLANE DEFINITION
C     MAKE TOP PLANE COORDS 3D

      DO 410 I = NP1,NP2
           PUNT(I,J3) = Z0
           J = I + NS
           PUNT(J,J1) = PUNT(I,J1)
           PUNT(J,J2) = PUNT(I,J2)
           PUNT(J,J3) = Z1
 410  CONTINUE

      NP2 = NP2 + NS

      CALL PCONVT(NFILE,NP1,NP2)

      NF2 = NF+1
      LIB1 = LIB+NS-1
      LIB0 = LIB

      IF (ISCLOC .EQ. 1)  CALL  INVERT

      LIB = LIB1 + 1
      LIB1 = LIB+NS-1
      J = LIB0
      DO 420 I = LIB,LIB1
           ISIDOR(I) = ISIDOR(J) + NP
           J = J + 1
 420  CONTINUE

      CALL VERT(NF2,*13)

C MAKE SIDE PLANE DEFINITIONS

      DO 430 I = 1,NS

           NF2 = NF2+1
           LIB1 = LIB+3

           J = LIB0 + I - 1
           J1 = J + 1
           IF (I .EQ. 1)  IP = ISIDOR(J)
           ISIDOR(LIB) =  IABS(ISIDOR(J)) + NP
           ISIDOR(LIB+1) = IABS(ISIDOR(J)) + NS + NP
           ISIDOR(LIB+2) = ISIDOR(J1) + NS + NP
           ISIDOR(LIB+3) = ISIDOR(J1) + NP
           IF (I .NE. NS .AND. ISIDOR(J1) .GT. 0)  GOTO  435
           ISIDOR(LIB+2) = IP + NS + NP
           ISIDOR(LIB+3) = IP + NP
           IP = IABS(ISIDOR(J1))
 435  CONTINUE
           CALL VERT(NF2,*13)
 430  CONTINUE

C MAKE TOP PLANE DEFINITION

      NF2 = NF2+1
      LIB1 = LIB+NS-1
      J = LIB0
      DO 460 I = LIB,LIB1
           ISIDOR(I) = IABS(ISIDOR(J)) + NS
           IF (ISIDOR(J) .LT. 0)  ISIDOR(I) = -ISIDOR(I)
           J = J + 1
 460  CONTINUE

      CALL  INVERT

      DO 470 I = LIB,LIB1
           ISIDOR(I) = ISIDOR(I) + NP
 470  CONTINUE

      CALL VERT(NF2,*13)

C GO TO FINISH BODY DEFINTION

      CALL BODY(NP1,NP2,NF1,NF2)

      RETURN

C----------------------------------------------------
C   READ BERRORS

 401  CONTINUE
      WRITE(IUOUT,901)  NB
      GOTO 13

 404  CONTINUE
      WRITE(IUOUT,902)  NB
      GOTO 13

C   MAX BODIES EXCEEDED
C   PROCEED WITH WHAT WE HAVE,IGNORE REST OF FILE

 415  CONTINUE
      WRITE(IUOUT,903)  NB
      GOTO  13

999   CONTINUE
      WRITE(IUOUT,904)  NB
      CALL EPAGEND
      STOP

13    CONTINUE
      LOOK = .FALSE.
      RETURN

  901 FORMAT(' BAD SURFACE data in BODY',I4)
  902 FORMAT(' BAD POINT data in BODY',I4)
  903 FORMAT(' MAX NUMBER of BODIES EXCEEDED in FILE:',/,
     &       ' rest of FILE IGNORED!')
  904 FORMAT(' WARNING: END-OF-FILE in BODY',I4,' ,IGNORED')

      END
