C This file is part of the ESP-r system.
C Copyright Energy Systems Research Unit, University of
C Strathclyde, Glasgow Scotland, 2001.

C ESP-r is free software.  You can redistribute it and/or
C modify it under the terms of the GNU General Public
C License as published by the Free Software Foundation 
C (version 2 orlater).

C ESP-r is distributed in the hope that it will be useful
C but WITHOUT ANY WARRANTY; without even the implied
C warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
C PURPOSE. See the GNU General Public License for more
C details.

C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C BOUND tests if k intersects the volume between I and J.
      SUBROUTINE  BOUND(I,J,K,IANS)

C     IANS = 0   NO INTERSECTION
C          = 1   INTERSECTS

#include "building.h"
      COMMON /SURPNT/ ISPNT(6,MSM), GAREA(MSM), EMISI(MS)
      logical closex,closey,closez, close

      DIMENSION  EQN(4)
      DIMENSION  IAPNT(100),APOL(1000)
      DIMENSION  IBPNT(100),BPOL(1000)
      DIMENSION  X1(4),Y1(4),Z1(4)
      DIMENSION  X2(4),Y2(4),Z2(4)
      DIMENSION  XP(4),YP(4),ZP(4)

      IANS = 1

      IREC = ISPNT(5,I)
      CALL  GETPOL(IREC,NA,NAP,IAPNT,APOL)

      X1(1) = APOL(2)
      Y1(1) = APOL(3)
      Z1(1) = APOL(4)
      X1(2) = APOL(5)
      Y1(2) = APOL(6)
      Z1(2) = APOL(7)
      X1(3) = APOL(8)
      Y1(3) = APOL(9)
      Z1(3) = APOL(10)
      X1(4) = APOL(11)
      Y1(4) = APOL(12)
      Z1(4) = APOL(13)

      IREC = ISPNT(5,J)
      CALL  GETPOL(IREC,NA,NAP,IAPNT,APOL)

      X2(2) = APOL(2)
      Y2(2) = APOL(3)
      Z2(2) = APOL(4)
      X2(1) = APOL(5)
      Y2(1) = APOL(6)
      Z2(1) = APOL(7)
      X2(4) = APOL(8)
      Y2(4) = APOL(9)
      Z2(4) = APOL(10)
      X2(3) = APOL(11)
      Y2(3) = APOL(12)
      Z2(3) = APOL(13)

      IREC = ISPNT(1,K)
      CALL  GETPOL(IREC,NB,NBP,IBPNT,BPOL)

      TX = (X1(1)+X1(2)+X1(3)+X1(4)+X2(1)+X2(2)+X2(3)+X2(4))/8
      TY = (Y1(1)+Y1(2)+Y1(3)+Y1(4)+Y2(1)+Y2(2)+Y2(3)+Y2(4))/8
      TZ = (Z1(1)+Z1(2)+Z1(3)+Z1(4)+Z2(1)+Z2(2)+Z2(3)+Z2(4))/8

      DO 200 N = 1,4
        NP = 1
        XP(NP) = X1(N)
        YP(NP) = Y1(N)
        ZP(NP) = Z1(N)

        call eclose(X1(N),X2(N),0.001,closex)
        call eclose(Y1(N),Y2(N),0.001,closey)
        call eclose(Z1(N),Z2(N),0.001,closez)
        if(closex.AND.closey.AND.closez)goto 100

        NP = NP + 1
        XP(NP) = X2(N)
        YP(NP) = Y2(N)
        ZP(NP) = Z2(N)

  100   CONTINUE

        M = N+1
        IF (M .EQ. 5)  M = 1

        NP = NP + 1
        XP(NP) = X2(M)
        YP(NP) = Y2(M)
        ZP(NP) = Z2(M)

        call eclose(X1(M),X2(M),0.001,closex)
        call eclose(Y1(M),Y2(M),0.001,closey)
        call eclose(Z1(M),Z2(M),0.001,closez)
        if(closex.AND.closey.AND.closez)goto 150

        NP = NP + 1
        XP(NP) = X1(M)
        YP(NP) = Y1(M)
        ZP(NP) = Z1(M)

  150 CONTINUE

        IF (NP .LT. 3)  GOTO  200

        CALL  PLNE(NP,XP,YP,ZP,EQN,IERR)
        IF (IERR .NE. 0)  GOTO  200

        TDIST = EQN(1)*TX + EQN(2)*TY + EQN(3)*TZ - EQN(4)
        call eclose(TDIST,0.0,0.001,close)
        IF (close)  GOTO  999

        IT = 1
        IF (TDIST .LT. 0.0)  IT = -1

        ID = 0

        DO 300 M = 1,NB
          IP = IABS(IBPNT(M))
          NP = INT(BPOL(IP))
          DO 400 L = 1,NP
            XX = BPOL(IP + (L-1)*3 + 1)
            YY = BPOL(IP + (L-1)*3 + 2)
            ZZ = BPOL(IP + (L-1)*3 + 3)

            D1 = EQN(1)*XX + EQN(2)*YY + EQN(3)*ZZ - EQN(4)
            call eclose(D1,0.0,0.001,close)
            IF (close)  GOTO  400
            IF (ID .EQ. 0)  GOTO  500
            IF (ID*D1 .LT. 0.0)  GOTO  200

            GOTO  400

  500       CONTINUE
            ID = 1
            IF (D1 .LT. 0.0)  ID = -1
  400     CONTINUE
  300   CONTINUE

        IF (IT*ID .LE. 0)  GOTO  900
  200 CONTINUE
      RETURN

  900 CONTINUE
      IANS = 0
      RETURN

  999 CONTINUE
      IANS = -1
      RETURN

      END
