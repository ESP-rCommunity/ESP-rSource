C This file is part of the ESP-r system.
C Copyright Energy Systems Research Unit, University of
C Strathclyde, Glasgow Scotland, 2001.

C ESP-r is free software.  You can redistribute it and/or
C modify it under the terms of the GNU General Public
C License as published by the Free Software Foundation 
C (version 2 or later).

C ESP-r is distributed in the hope that it will be useful
C but WITHOUT ANY WARRANTY; without even the implied
C warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
C PURPOSE. See the GNU General Public License for more
C details.

C You should have received a copy of the GNU General Public
C License along with ESP-r. If not, write to the Free
C Software Foundation, Inc., 59 Temple Place, Suite 330,
C Boston, MA 02111-1307 USA.

C This file includes:
C  RegOveheat calculates the risk of overheating based on criteria for UK NCM method.  

C ********************* RegOveheat
C RegOveheat calculates the risk of overheating based on criteria for
C UK NCM method.
      SUBROUTINE RegOveheat(icomp)
      IMPLICIT NONE
#include "building.h"
#include "net_flow.h"
#include "tdf2.h"

C..............
C Need TFUT from this
      integer ICF,IDTYP,IPER,IICOMP
      real BB1,BB2,BB3,TNP,QFUT,TFUT
      COMMON/PSTSOL/ICF,IDTYP,IPER,BB1,BB2,BB3,IICOMP,TNP,
     &QFUT,TFUT

      COMMON/OUTIN/IUOUT,IUIN
      integer IUOUT,IUIN
      COMMON/PREC1/SNA(MCOM,MS)
      real sna
      COMMON/PREC9/NCONST(MCOM),NELTS(MCOM,MS),NGAPS(MCOM,MS),
     &NPGAP(MCOM,MS,MGP)
      integer NCONST,NELTS,NGAPS,NPGAP

      COMMON/FVALS/TFS(MCOM,MS),QFS(MCOM)
      real TFS,QFS
      
      integer NC
      real SUM,AREA
      real TMRT,TO
      integer J
      real TMIX
      character outs*124

C Common to count the number of occupied hours above 27oC (oper. temperature)
C Used for UK NCM purposes
      common/ncmHrOverheat/ihroverheats(MCOM),ihroccup(MCOM)
      integer ihroverheats,ihroccup

      common/btime/btimep,btimef      
      real btimep,btimef
      real VAL
      dimension VAL(MTABC+2)
      common/trc/itrc
      integer itrc
      integer ISD,IFOC,IER
      integer icomp
      
C First calculate the operative temperature

C Sum the product of internal surface temperatures (opaque
C and transparent), and corresponding areas.
      NC=NCONST(icomp)

C Initialise
      TMRT=TFUT
      SUM=0.
      AREA=0.
      
C Opaque internal surfaces.
      DO 20 J=1,NC
        TO=TFS(icomp,J)
        SUM=SUM+TO*SNA(icomp,J)
        AREA=AREA+SNA(icomp,J)
   20 CONTINUE

C Compute weighted temperature.
      IF(AREA.gt.0.)then
        TMRT=SUM/AREA
        TMIX=0.5*TFUT+0.5*TMRT
      else
        call edisp(iuout,
     &    ' zero area associated with the operative temperature!')
        write(outs,'(A,I3)')' Zone',icomp
        call edisp(iuout,outs)
      endif
      
      IFOC=IACTIV(icomp)
      CALL RCTDFB(itrc,btimef,VAL,ISD,IFOC,IER)

C Check if occupancy fraction from TDF is greater than 0 and if 
C Operative (TMIX) temperature is greated than 27
C 27 should be replaced by a variable if this routine is going 
C to be generalised

C NOTE: this is timestep overheating
      if(VAL(ISD).gt.0.0)then
        ihroccup(icomp)=ihroccup(icomp)+1 
        if(TMIX.gt.27.)then
          ihroverheats(icomp)=ihroverheats(icomp)+1
        endif
      endif

      RETURN
      END
