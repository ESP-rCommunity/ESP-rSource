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

C You should have received a copy of the GNU General Public
C License along with ESP-r. If not, write to the Free
C Software Foundation, Inc., 59 Temple Place, Suite 330,
C Boston, MA 02111-1307 USA.

C+++++++++++++++++++++++ INSIDEP
C INSIDEP Polygon point containment test.
C Returns .TRUE. or .FALSE. depending on whether (X,Y) is inside
C the polygon or not.
      LOGICAL FUNCTION  INSIDEP(X,Y,NA,IAPNT,APOL)

      DIMENSION  IAPNT(100),APOL(1000)

      INSIDEP = .FALSE.

      LN=0

      IP = IAPNT(1)
      NP = INT(APOL(IP))

      I = 1
      K = IP + NP*3 - 2
      X1 = APOL(K)
      Y1 = APOL(K+1)
      K = IP + 1
      X2 = APOL(K)
      Y2 = APOL(K+1)
      GOTO  21

   19 CONTINUE

      X1 = APOL(K-3)
      Y1 = APOL(K-2)
      X2 = APOL(K)
      Y2 = APOL(K+1)

   21 CONTINUE

      IF ((Y1-Y)*(Y-Y2))  20,1,2

    2 IF ((Y-Y1)*(X2-X1)/(Y2-Y1)+X1-X)  20,3,4
    1 IF (Y1-Y2)  5,6,7
    6 IF ((X1-X)*(X-X2))  20,3,3
    5 IF ((Y-Y1)*(X2-X1)/(Y2-Y1)+X1-X)  20,3,8
    7 IF ((Y-Y1)*(X2-X1)/(Y2-Y1)+X1-X)  20,3,9
    9 LN=LN-2
    8 LN=LN-1
    4 LN=LN+2

   20 CONTINUE

      I = I + 1
      K = K + 3
      IF (I .LE. NP)  GOTO  19

      IF ((LN/4)*4.NE.LN)  GOTO  3
      RETURN

    3 INSIDEP = .TRUE.

      RETURN
      END
