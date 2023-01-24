C ESP-r is free software.  You can redistribute it and/or
C modify it under the terms of the GNU General Public
C License as published by the Free Software Foundation 
C (version 2 or later).

C ESP-r is distributed in the hope that it will be useful
C but WITHOUT ANY WARRANTY; without even the implied
C warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
C PURPOSE. See the GNU General Public License for more
C details.

C Compute srface visibility.
      SUBROUTINE  SURVIS

#include "building.h"
      COMMON /VISIB/ IVIS(MSM,MSM), IARECT(MSM,MSM)
      COMMON /TOTAL/ nsurmf, NGRID, NTRPOL
      COMMON /ZZZ/  ZMIN(MSM,MSM), ZMAX(MSM,MSM)

      DIMENSION IDPRI(MS)

      DO 100 I = 1,nsurmf

C Count number of visible surfaces (NOBS).
      NOBS = 0
      DO 200 J = 1,nsurmf

        IF (IVIS(I,J) .EQ. -1)  GOTO  200

        NOBS = NOBS + 1
        IF (NOBS .EQ. 1)  ID1 = J

        IDPRI(NOBS) = J

  200 CONTINUE

      IF (NOBS .NE. 1)  GOTO  300

C Only one surface visible.
      IVIS(I,ID1) = 1
      IVIS(ID1,I) = 1
      GOTO  100

  300 CONTINUE

      DO 400 J = 1,NOBS

        J1 = IDPRI(J)
        IF (IVIS(I,J1) .NE. 0)  GOTO  400
        DO 500 K = 1,NOBS
          IF (J .EQ. K) GOTO 500
          K1 = IDPRI(K)
          IF (ZMAX(I,J1) .LE. ZMIN(I,K1)) GOTO 500
          CALL  VTEST(I,J1,K1,IANS)
          IF (IANS .EQ. 1) GOTO 500
          CALL  BOUND(I,J1,K1,IANS)
          IF (IANS .EQ. 0) GOTO 500

C Fails test therefore assume obstructed.
          IVIS(I,J1) = 2
          IVIS(J1,I) = 2
          GOTO  400

  500   CONTINUE

C Has passed tests for all other visible surfaces
C assume totally visible.
        IVIS(I,J1) = 1
        IVIS(J1,I) = 1

  400 CONTINUE
  100 CONTINUE

      RETURN

      END

C ***************** VTEST
C Test if surface K obstructs visibility of I to J.
C IANS = 0 IF K OBSTRUCTS
C IANS = 1 IF K DOES NOT OBSTRUCT

      SUBROUTINE  VTEST(I,J,K,IANS)

#include "building.h"
      COMMON /XXX/  XMIN(MSM,MSM), XMAX(MSM,MSM)
      COMMON /YYY/  YMIN(MSM,MSM), YMAX(MSM,MSM)
      COMMON /ZZZ/  ZMIN(MSM,MSM), ZMAX(MSM,MSM)

C Test if K is further away from I then J.
      IF (ZMIN(I,K) .GE. ZMAX(I,J))  GOTO 999

C Test if K lies outside the bound box of I and J.
      IF (XMIN(I,K) .GE. XMAX(I,J) .AND.
     &          XMIN(I,K) .GE. XMAX(I,I))  GOTO  999
      IF (XMAX(I,K) .LE. XMIN(I,J) .AND.
     &          XMAX(I,K) .LE. XMIN(I,I))  GOTO  999
      IF (YMIN(I,K) .GE. YMAX(I,J) .AND.
     &          YMIN(I,K) .GE. YMAX(I,I))  GOTO  999
      IF (YMAX(I,K) .LE. YMIN(I,J) .AND.
     &          YMAX(I,K) .LE. YMIN(I,I))  GOTO  999

      IANS = 0
      GOTO  900

  999 CONTINUE

      IANS = 1

  900 CONTINUE

      RETURN
      END
