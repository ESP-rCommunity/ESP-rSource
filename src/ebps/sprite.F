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

C This file contains the following subroutines:
C SPRCTL  - Main controller for sprites.
C SPRINIT - Initialise sprite commons
C SPRSTOP - Stop the simulation if there is a problem.
C SPR??   - Sprite models.

C -----------------------------------------------------------
C Main controller for sprites.
      subroutine SPRCTL(problem,ier)

#include "sprite.h"
#include "building.h"

      integer problem,ier

      COMMON/OUTIN/IUOUT,IUIN
      common/simtim/ihrp,ihrf,idyp,idyf,idwp,idwf,nsinc,its
      common/btime/btimep,btimef
      COMMON/SET1/IYEAR,IBDOY,IEDOY,IFDAY,IFTIME

C <SPR>
      common/occup/isocc(MCOM)
      logical isocc
C </SPR>
      character outs*128
      character*8 DESCR
      integer probtyp,probtyps(MSPR)
      integer cmptyp
      character dq*1
      dq = char(34)  ! double quote

C If there has already been a problem flagged, just return.
      if (problem.eq.1) goto 999

      problem=-1

      if (NSPR.eq.0) goto 999

      problem=0

C Query passive sprites.
      do ispr=1,NSPR
        probtyps(ISPR)=0
        if (SPRACT(ISPR)) cycle

C Check for occupancy.
        if (SPRZON(ISPR).gt.0) then

C Debug.
C          write(6,*) 'ispr sprzon isocc time',
C     &      ISPR,SPRZON(ISPR),isocc(SPRZON(ISPR)),btimef
          if (.not.isocc(SPRZON(ISPR))) cycle
        endif

        select case (SPRTYP(ISPR))
        case(1)
          call SPR01(ispr,probtyp,ier)
        case default
          IER=1
          write(outs,'(a,i2,a)')
     &      'SPRCTL: passive sprite type ',SPRTYP(ISPR),
     &      ' not recognised'
          call edisp(iuout,outs)
          goto 666
        end select

        if (IER.ne.0) goto 666
        if (probtyp.gt.0) then
          probtyps(ISPR)=probtyp

C Passive sprite SPR01 notices hot (1) and cold (2)
C Active sprite SPR02 notices number of complaints
C Log.

C Format date and time.
          call edayr(idyp,id,im)
          MINH=INT(btimef)
          MIN=INT((btimef-AINT(btimef))*60.+0.5)
          WRITE(DESCR,'(i2.2,a1,i2.2,a)')INT(btimef),':',MIN,':30'
          if(ISPR.eq.1.and.probtyp.eq.1)then
            write(42,'(2a,i2.2,a,i2.2,a,i4,4a)')
     &      DESCR,' ',id,'/',im,'/',IYEAR,' (ST) comfort sprite ',
     &      DQ,'warm',DQ
          elseif(ISPR.eq.1.and.probtyp.eq.2)then
            write(42,'(2a,i2.2,a,i2.2,a,i4,4a)')
     &      DESCR,' ',id,'/',im,'/',IYEAR,' (ST) comfort sprite ',
     &      DQ,'cold',DQ
          endif
          if(ISPR.eq.2.and.probtyp.eq.3)then
            write(42,'(2a,i2.2,a,i2.2,a,i4,4a)')
     &      DESCR,' ',id,'/',im,'/',IYEAR,' (ST) action sprite ',
     &      DQ,'lots of complaints',DQ
          endif
        endif
      enddo

C Query active sprites.
      do ispr=1,NSPR
        if (.not.SPRACT(ISPR)) cycle

C Check for occupancy.
        if (SPRZON(ISPR).gt.0) then
          if (.not.isocc(SPRZON(ISPR))) cycle
        endif

        select case (SPRTYP(ISPR))
        case(2)
          call SPR02(ispr,probtyp,probtyps,ier)
        case default
          IER=1
          write(outs,'(a,i2,a)')
     &      'SPRCTL: active sprite type ',SPRTYP(ISPR),
     &      ' not recognised'
          call edisp(iuout,outs)
          goto 666
        end select

        if (IER.ne.0) goto 666
        if (probtyp.gt.0) then
          probtyps(ISPR)=probtyp
          problem=1

C Format date and time.
          call edayr(idyp,id,im)
          MINH=INT(btimef)
          MIN=INT((btimef-AINT(btimef))*60.+0.5)
          WRITE(DESCR,'(i2.2,a1,i2.2,a)')INT(btimef),':',MIN,':30'
          if(ISPR.eq.1.and.probtyp.eq.1)then
            write(42,'(2a,i2.2,a,i2.2,a,i4,4a)')
     &      DESCR,' ',id,'/',im,'/',IYEAR,' (ST) active sprite ',
     &      DQ,'warm',DQ
          elseif(ISPR.eq.1.and.probtyp.eq.2)then
            write(42,'(2a,i2.2,a,i2.2,a,i4,4a)')
     &      DESCR,' ',id,'/',im,'/',IYEAR,' (ST) comfort sprite ',
     &      DQ,'cold',DQ
          endif
          if(ISPR.eq.2.and.probtyp.eq.3)then
            write(42,'(2a,i2.2,a,i2.2,a,i4,4a)')
     &      DESCR,' ',id,'/',im,'/',IYEAR,' (ST) action sprite ',
     &      DQ,'lots of complaints',DQ
          endif

C Log.
C          write(42,'(a,i2,a,i2,a,i3,a,i2,a,i2,a)')
C     &      '# Active sprite ',ISPR,' raised complaint type ',
C     &      probtyp,' on day ',idyp,' at hour ',ihrp,' timestep ',
C     &      its,'.'
          exit
        endif

      enddo

C Write report if there is a problem.
      if (problem.gt.0) then
        write(42,'(i2,1x,i2,1x,i2)')SPRNUM,PRBNUM,SUPDAT
      endif

 999  return

C Error.
 666  write(outs,'(a)')'SPRCTL: error while querying sprites'
      goto 999

      end


C -----------------------------------------------------------
C Initialise sprite data structures.
      subroutine SPRINIT

#include "sprite.h"

C <HARDCODE_SPRITES>
C TEMPORARY - hard code 2 sprites in zone 1, an active and a passive
      logical endless,issilient
      common/endlessmode/endless,issilient

C      endless=.true.

      if (endless) then
        NSPR=2 ! number of sprites
C Sprite 1
        SPRTYP(1)=1 ! sprite model
        SPRZON(1)=2 ! sprite zone (adjust to fit default_model)
        SPRACT(1)=.false. ! active sprite
C Sprite 2
        SPRTYP(2)=2
        SPRZON(2)=2
        SPRACT(2)=.true.
      else
        NSPR=0
      endif



      do i=1,MSPR
        do j=1,MPROB
          CMPLTS(i,j)=0
        enddo
      enddo

      SPRNUM=0
      PRBNUM=0
      SUPDAT=0

      RETURN
      END



C -----------------------------------------------------------
C Stop a simulation if there is a problem.
      subroutine SPRSTOP(IDAY)

      COMMON/PERS/ISD1,ISM1,ISD2,ISM2,ISDS,ISDF,NTSTEP

C Set simulation finish day to this day.
      CALL EDAYR(IDAY,ISD2,ISM2)
      ISDF=IDAY

C Rewrite results library header with this finish day.
      CALL MZLS2
      CALL MZSAVE(i,j)

C Kill simulation with error code 42.
      STOP 42

      RETURN
      END


C *** SPRITES ***
C Sprites are model entities intended to represent people. If certain
C criteria are met, they complain. There are two kinds - active and
C passive. Passive sprites are general building occupants; if they get
C uncomfortable they complain to someone in authority e.g. a facilities
C manager. Active sprites are these authority figures; they complain if
C the building is not performing as it should e.g. if they recieve too
C many complaints from passive sprites. Complaints from active sprites
C will halt the simulation, complaints from passive sprites will not.
C Sprite complaints have prescribed enumerations, detailed in the header
C file "sprite.h".

C -----------------------------------------------------------
C Type 1 - Basic prototype passive sprite.
C Criteria - Zone dry bulb temperature 18 - 23 deg. C
C Complaints - type 1 or 2
      subroutine SPR01(ispr,probtyp,IER)
#include "building.h"
#include "sprite.h"

      integer probtyp

      COMMON/FVALA/TFA(MCOM),QFA(MCOM)

      probtyp=0

C Check 16 <= temperature <= 24
      if (TFA(SPRZON(ISPR)).gt.24.0) then
        probtyp=1
      elseif (TFA(SPRZON(ISPR)).lt.16.0) then
        probtyp=2
      endif

      return
      end

C -----------------------------------------------------------
C Type 2 - Basic prototype active sprite.
C Criteria - >25 complaints of the same type.
C Complaints - type 3
      subroutine SPR02(ispr,probtyp,probtyps,IER)

#include "sprite.h"

      integer probtyp,cmptyp,probtyps(MSPR)

      probtyp=0

C Add complaints for this time step.
      do iprob=1,NSPR
        if (probtyps(iprob).ne.0) then
          CMPLTS(ISPR,probtyps(iprob))=CMPLTS(ISPR,probtyps(iprob))+1
        endif
      enddo

C Check complaints <= 50
      do iprob=1,MPROB
        if (CMPLTS(ISPR,iprob).gt.50) then
          probtyp=3
          SPRNUM=ispr
          PRBNUM=probtyp
          SUPDAT=iprob
        endif
      enddo

      return
      end
