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

C This file contains the following routines:
C      MOREC1
C      MOREC2

C ******************** MOREC1 ********************

C MOREC1 identifies the starting record address
C for first day of recovery period.

      SUBROUTINE MOREC1(IDAY,IREC)
#include "building.h"
#include "geometry.h"

      COMMON/SIMPIK/ISIM,ISTADD,ID1,IM1,ID2,IM2,ISDS,ISDF,
     &NTS,ISAVE

      integer ncomp,ncon
      COMMON/C1/NCOMP,NCON

C Compute start record address for requested recovery day.
      if (ISAVE.eq.1) then
        IREC=ISTADD+24*NTS*NCOMP*(IDAY-ISDS)
      elseif (ISAVE.eq.2) then
        IREC=ISTADD+3*24*NTS*NCOMP*(IDAY-ISDS)
      elseif (ISAVE.eq.3) then
        ISUM=0
        DO 10 I=1,NCOMP
          NN=NZSUR(I)
          ISUM=ISUM+(3+NN)*24*NTS*(IDAY-ISDS)
   10   CONTINUE
        IREC=ISTADD+ISUM
      elseif (ISAVE.eq.4) then
        ISUM=0
        DO 20 I=1,NCOMP
          NN=NZSUR(I)
          ISUM=ISUM+(3+NN)*24*NTS*(IDAY-ISDS)
   20   CONTINUE
        IREC=ISTADD+ISUM
      endif

      RETURN
      END

C ******************** MOREC2 ********************

C MOREC2 increments IREC depending upon save option.

      SUBROUTINE MOREC2(NCN,IREC)

      COMMON/SIMPIK/ISIM,ISTADD,ID1,IM1,ID2,IM2,ISDS,ISDF,
     &NTS,ISAVE

C Increment IREC.
      IF(ISAVE.EQ.1)IREC=IREC+1
      IF(ISAVE.EQ.2)IREC=IREC+3
      IF(ISAVE.EQ.3)IREC=IREC+3+NCN
      IF(ISAVE.EQ.4)IREC=IREC+3+NCN
      RETURN
      END
