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

C COMPUTE SURFACE INVISIBILITY.

      SUBROUTINE  SURINV(I,TMAT)

      INCLUDE 'building.h'
      COMMON /SURPNT/ ISPNT(6,MSM), GAREA(MSM), EMISI(MS)
      COMMON /VISIB/ IVIS(MSM,MSM), IARECT(MSM,MSM)
      COMMON /XXX/  XMIN(MSM,MSM), XMAX(MSM,MSM)
      COMMON /YYY/  YMIN(MSM,MSM), YMAX(MSM,MSM)
      COMMON /ZZZ/  ZMIN(MSM,MSM), ZMAX(MSM,MSM)
      COMMON /TOTAL/ nsurmf, NGRID, NTRPOL

      DIMENSION  TMAT(4,4)
      DIMENSION  IAPNT(100),APOL(1000)
      DIMENSION  IBPNT(100),BPOL(1000)
      DIMENSION  SBBOX(3,2)

      DO 100 J = 1,nsurmf

        IF (J .EQ. I)  GOTO  175
        IF (IVIS(I,J) .EQ. -1)  GOTO  100
        IP = ISPNT(1,J)
        CALL  GETPOL(IP,NA,NAP,IAPNT,APOL)

        CALL  MATPOLA(NA,NAP,IAPNT,APOL,TMAT,0,
     &          SBBOX,NB,NBP,IBPNT,BPOL,IERR)

        XMIN(I,J) = SBBOX(1,1)
        XMAX(I,J) = SBBOX(1,2)

        YMIN(I,J) = SBBOX(2,1)
        YMAX(I,J) = SBBOX(2,2)

        ZMAX(I,J) = 1.0 - SBBOX(3,1)
        ZMIN(I,J) = 1.0 - SBBOX(3,2)

        DELTA = 0.000001
        IF (SBBOX(3,1) .GE. 1.0-DELTA)  GOTO  150
        IF (SBBOX(3,2) .GT. 1.0+DELTA)  GOTO  200
        GOTO  100

  150   CONTINUE

        IVIS(J,I) = -1

  175   CONTINUE

        IVIS(I,J) = -1

        GOTO  100

  200   CONTINUE

        IVIS(I,J) = 3
        IVIS(J,I) = 3

  100 CONTINUE

      RETURN
      END
