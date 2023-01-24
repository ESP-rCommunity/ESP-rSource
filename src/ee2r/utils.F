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
C PIKMRTS: Pick one or many MRT sensors from zone/ sensor list.

C ******************** PIKMRTS ********************
C PIKMRTS: call to enquire which MRT sensors to display sensed MRT for.
C Returns zones and sensor numbers via IZLST and ISLST respectively.
C NSEL is the maximum number of selections allowed, and is set to the
C number of selections made before returning.
C If IZONE>0 then will show sensors for that zone, if IZONE=0 will show
C sensors for all zones, if IZONE=-1 will show sensors for all currently
C selected zones

      SUBROUTINE PIKMRTS(IZLST,ISLST,NSEL,IZONE,IER)
#include "building.h"
#include "model.h"
#include "geometry.h"
#include "help.h"

      COMMON/OUTIN/IUOUT,IUIN,IEOUT
      COMMON/FILEP/IFIL

      integer ncomp,ncon
      COMMON/C1/NCOMP,NCON

      COMMON/ZONPIK/NZ,NZNO(MCOM)

      DIMENSION IV(60),IZLST(MCON),ISLST(MCON),ILZNO(MCON),ILSNO(MCON)

      character ST(60)*22,outs*124,LMRT*72

      helpinsub='utils'  ! set for subroutine

      IER=0

      if (IZONE.gt.0) then
        IST=IZONE
        IFI=IZONE
      elseif (IZONE.eq.-1) then
        IST=1
        IFI=NZ
      else
        IST=1
        IFI=NCOMP
      endif

C Create list of MRT sensors: loop through zones and read any view 
C factor files remembering sensor names only.
C Remember the total number of sensors.
      NTSEN=0
      do 10 I=IST,IFI
        if (IZONE.eq.-1) then
          IZ=NZNO(I)
        else
          IZ=I
        endif
        IF(IVF(IZ).EQ.1)THEN
          IUF=IFIL+1
          LMRT=LVIEW(IZ)
          call ERMRT(0,IUOUT,IUF,LMRT,IZ,IER)
          if (NCUB(IZ).gt.0) then
            do 20 J=1,NCUB(IZ)
              NTSEN=NTSEN+1
              write (ST(NTSEN),'(a,a,a)') zname(IZ),':',CUBN(J)
              ILZNO(NTSEN)=IZ
              ILSNO(NTSEN)=J
 20         continue
          endif
        endif
 10   continue

C Check that there are MRT sensors.
      if (NTSEN.eq.0) then
        call usrmsg('No MRT sensors exist.','  ','W')
        IER=1
        return
      endif

C Display menu of MRT sensors allow only one selection.
      helptopic='res_MRT_pick_menu'
      call gethelptext(helpinsub,helptopic,nbhelp)
      write(outs,'(a)')'Which MRT sensors to use?'
      IVLS=NTSEN
      if (NTSEN.gt.NSEL) NTSEN=NSEL
      write(6,*)NTSEN,IVLS
      CALL EPICKS(NTSEN,IV,outs,'  ',22,IVLS,ST,'MRT sensors',
     &  IER,nbhelp)

C Check for selections.
      if (NTSEN.gt.0) then

C Copy associated zones/surfaces to call parameters for passing back.
        do 801 I=1,NTSEN
          IZLST(I)=ILZNO(IV(I))
          ISLST(I)=ILSNO(IV(I))
 801    continue
      endif
      NSEL=NTSEN

      RETURN
      END
