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

C PSHELL of ESRUpdb contains the following:
C   SUBROUTINE PSHELL
C   SUBROUTINE PCEXCH
C   LOGICAL FUNCTION COMPDS
C   LOGICAL FUNCTION COMPNO

C ******************************************************************
      SUBROUTINE PSHELL (N,COMP,EXCH)

C Shell sort using logical functions
C COMP and EXCH to compare and exchange
C keyed items. If there are no exchanges
C on a sweep, the comparison gap is halved.
      EXTERNAL COMP, EXCH
      LOGICAL COMP

      IGAP = N

C while (igap > 1)
  100 IF (IGAP .LE. 1)   GOTO 9000
         IGAP = IGAP / 2
         IMAX = N - IGAP

C do
  200    IEX = 0
         DO 300 I = 1,IMAX
            IPLUSG = I + IGAP
            IF ( COMP(I,IPLUSG) ) GOTO 300
            CALL EXCH(I,IPLUSG)
            IEX = 1
  300    CONTINUE

C while (iex > 0)
         IF (IEX .GT. 0)   GOTO 200
      GOTO 100

C Return
 9000 RETURN
      END

C ******************************************************************
      SUBROUTINE PCEXCH (N1,N2)
#include "plantdb.h"

C  Parameters
C Used by shell to exchange items
C ie PC index for print list
      COMMON/PL/INDXPR(MAXPC)
      common/OUTIN/IUOUT,IUIN

      ITMP = INDXPR(N2)
      INDXPR(N2) = INDXPR(N1)
      INDXPR(N1) = ITMP

      RETURN
      END

C ******************************************************************
C Used by shell to compare keyed items
C ie compare component descriptions
      LOGICAL FUNCTION COMPDS (N1,N2)
#include "plantdb.h"
#include "pdb_data.h"

      COMMON /PL/ INDXPR(MAXPC)
      common/OUTIN/IUOUT,IUIN

      CHARACTER*80 PZDESC

      NN = INDXPR(N1)
      CALL PCGET (NN)
      PZDESC = ZCDESC
      NN = INDXPR(N2)
      CALL PCGET (NN)
      IF (PZDESC .GT. ZCDESC) THEN
         COMPDS = .FALSE.
      ELSE
         COMPDS = .TRUE.
      ENDIF

      RETURN
      END

C ******************************************************************
      LOGICAL FUNCTION COMPNO (N1,N2)
#include "plantdb.h"

C Used by shell to compare keyed items
C ie compare component codes
      COMMON /PL/ INDXPR(MAXPC)
      common/OUTIN/IUOUT,IUIN
      COMMON /PCDATB/ ICODE,INDXPC,IRC,NNODE,NMATX,NMISC,
     &                nadata,nbdata,ncdata,napout,
     &                NDCON(MAXNOD),ISV(MAXNOD),NCPOS(MAXMTX),
     &                dtdesc(maxmsc),DATAMS(MAXMSC),range(maxmsc,2),
     &                adodsc(mxaout),noutyp(mxaout)

      character*68 dtdesc
      character*30 adodsc

      NN = INDXPR(N1)
      CALL PCGET (NN)
      NCODE = ICODE
      NN = INDXPR(N2)
      CALL PCGET (NN)
      IF (NCODE .GT. ICODE) THEN
         COMPNO = .FALSE.
      ELSE
         COMPNO = .TRUE.
      ENDIF

      RETURN
      END

