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

CC CALCULATE AREA OF SUPER-POLYGON.

      SUBROUTINE  SPAREA(NA,IAPNT,APOL,M3D,AREA)

      DIMENSION  IAPNT(100),APOL(1000)

      AREA = 0.0

      IF (NA .EQ. 0)  GOTO  999

      DO 10 I = 1,NA
        IP = IABS(IAPNT(I))
        AREA = AREA + SAREA(IP,APOL,M3D)
   10 CONTINUE

  999 CONTINUE

      RETURN
      END

C **************** SAREA
C  Determine the surface area of given Polygon in 3 dimensions.
      FUNCTION  SAREA(N,APOL,I3DA)

C  The polygon is divided into triangles. The first point of the
C  Polygon together with two other points form the triangles.
C  I3DA = 1 if surface is not normalised (normalised surfaces
C  only need x and y values)

#include "plylib.h"
      DIMENSION  APOL(MPL)

      REAL VECPROD(3),X0,Y0,Z0

      IAF = 2
      IF (I3DA .EQ. 1)  IAF = 3

      NP = INT(APOL(N))
      X0 = APOL(N+(NP-1)*IAF+1)
      Y0 = APOL(N+(NP-1)*IAF+2)
      Z0 = APOL(N+(NP-1)*IAF+3)

      DO 10 I=1,3
        VECPROD(I)= 0.0
10    CONTINUE

      X2 = 0.
      Y2 = 0.
      Z2 = 0.
      DO 100 I = 1,NP
        X1 = X2
        Y1 = Y2
        Z1 = Z2
        X2 = APOL(N+(I-1)*IAF+1) - X0
        Y2 = APOL(N+(I-1)*IAF+2) - Y0
        Z2 = APOL(N+(I-1)*IAF+3) - Z0

c The surface area of the 'diamond' between two vectors starting
c at (X0,Y0,Z0) is the length of the vector-product of the
c two vectors. We sum all vector-products including negative and
c positive sign. The length of the resulting vector is the
c surface-area.
         VECPROD(1) = VECPROD(1) + (Y1*Z2-Y2*Z1)
         VECPROD(2) = VECPROD(2) + (X2*Z1-X1*Z2)
         VECPROD(3) = VECPROD(3) + (X1*Y2-X2*Y1)
  100 CONTINUE

      SAREA = 0.0
      IF (I3DA.EQ.1) THEN
        DO 110 I=1,3
          SAREA = SAREA + VECPROD(I)**2
110     CONTINUE
      ELSE
	SAREA = VECPROD(3)**2
      ENDIF

c The surface area of the triangles is half that of the diamonds.
      SAREA = 0.5*(SAREA**0.5)
      RETURN
      END
