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
C GRMCTL  - Main controller for gremlins.
C GRMINIT - Initialise gremlin commons.
C GRM??   - Gremlin models.

C --------------------------------------------------
C Main controller for gremlins.
      subroutine GRMCTL(ier)

#include "gremlin.h"
      
      COMMON/OUTIN/IUOUT,IUIN
      common/simtim/ihrp,ihrf,idyp,idyf,idwp,idwf,nsinc,its
      common/btime/btimep,btimef
      COMMON/SET1/IYEAR,IBDOY,IEDOY,IFDAY,IFTIME

      character outs*128
      character*8 DESCR
      character dq*1

C "mischief" returns from gremlin subs:
C 0  = no change
C -1 = finished mischief
C >0 = started mischief of this type
      integer mischief

      dq = char(34)  ! double quote

C Check for gremlin mischief.
      do IGRM=1,NGRM
        mischief=0
        select case (GRMTYP(IGRM))
        case(1)
          call GRM01(igrm,mischief,ier)
        case(2)
          call GRM02(igrm,mischief,ier)
        case default
          IER=1
          write(outs,'(a,i2,a)')
     &      'SPRCTL: gremlin type ',GRMTYP(IGRM),' not recognised'
          call edisp(iuout,outs)
          goto 666
        end select
        if (IER.ne.0) goto 666

C Mischief started.
        if (mischief.gt.0) then
          imcf=0
          NMCF=NMCF+1
          do i=1,NMCF
            if (MCFGRM(i).eq.0) then
              imcf=i
              exit
            endif
          enddo
          if (imcf.eq.0) goto 666
          GRMMCF(IGRM)=imcf
          MCFGRM(imcf)=IGRM
          MCFTYP(imcf)=mischief
          MCFSTS(imcf)=nsinc

C Debug.
c          write(6,*)'Gremlin mischief started'
c          write(6,*)'igrm,imcf',igrm,imcf
c          write(6,*)'mischief,nsinc',mischief,nsinc
C IGRM Type 1 - boiler gremlin  mischief 1=boiler failure
C IGRM Type 2 - climate gremlin mischief 2=heat wave, 3=cold snap

C Format date and time.
          call edayr(idyp,id,im)
          MINH=INT(btimef)
          MIN=INT((btimef-AINT(btimef))*60.+0.5)
          WRITE(DESCR,'(i2.2,a1,i2.2,a)')INT(btimef),':',MIN,':30'

C Log.
          if(IGRM.eq.1)then
            write(42,'(2a,i2.2,a,i2.2,a,i4,4a)')
     &      DESCR,' ',id,'/',im,'/',IYEAR,' (ST) boiler gremlin ',
     &      DQ,'caused failure',DQ
          elseif(IGRM.eq.2.and.mischief.eq.2)then
            write(42,'(2a,i2.2,a,i2.2,a,i4,4a)')
     &      DESCR,' ',id,'/',im,'/',IYEAR,' (ST) climate gremlin ',
     &      DQ,'caused heatwave',DQ
          elseif(IGRM.eq.2.and.mischief.eq.3)then
            write(42,'(2a,i2.2,a,i2.2,a,i4,4a)')
     &      DESCR,' ',id,'/',im,'/',IYEAR,' (ST) climate gremlin ',
     &      DQ,'caused cold snap',DQ
          endif
C          write(42,'(a,i2,a,i2,a,i3,a,i2,a,i2,a)')
C     &      '# Gremlin ',IGRM,' caused mischief type ',mischief,
C     &      ' on day ',idyp,' at hour ',ihrp,' timestep ',its,'.'

C Mischief managed.
        elseif (mischief.eq.-1) then
          imcf=GRMMCF(IGRM)
          GRMMCF(IGRM)=0
          if (imcf.lt.NMCF) then
            do i=imcf+1,NMCF
              GRMMCF(MCFGRM(i))=GRMMCF(MCFGRM(i))-1
              MCFGRM(i-1)=MCFGRM(i)
              MCFTYP(i-1)=MCFTYP(i)
              MCFSTS(i-1)=MCFSTS(i)
            enddo
          endif          
          MCFGRM(NMCF)=0
          MCFTYP(NMCF)=0
          MCFSTS(NMCF)=0
          NMCF=NMCF-1

C Debug.
c          write(6,*)'Gremlin mischief finished'
c          write(6,*)'igrm,imcf',igrm,imcf
c          write(6,*)'mischief,nsinc',mischief,nsinc
          
        endif
      enddo

 999  return

C Error.
 666  write(outs,'(a)')'GRMCTL: error while checking on gremlins'
      goto 999

      end

C --------------------------------------------------
C Initialise gremlin commons.
      subroutine GRMINIT

#include "gremlin.h"

      common/pers/isd1,ism1,isd2,ism2,isds,isdf,ntstep

      integer dat(8),seed

C <HARDCODE_GREMLINS>
C TEMPORARY - hard code one boiler gremlin and one climate gremlin.
      logical endless,issilient
      common/endlessmode/endless,issilient

c      endless=.true.

      if (endless) then
        NGRM=1 ! number of gremlins (only heat failure for now)
C Gremlin 1
        GRMTYP(1)=1 ! gremlin model
        GRMZON(1)=0 ! gremlin zone (zero applies to any zone)
C Gremlin 2
        GRMTYP(2)=0
        GRMZON(2)=0
      else
        NGRM=0
      endif



      NMCF=0
      do i=1,MGRM
        GRMMCF(i)=0
        MCFGRM(i)=0
        MCFTYP(i)=0
        MCFSTS(i)=0
      enddo

C Seed random number generator with number derived from current time.
      call date_and_time(values=dat)
      seed=sum(dat)
      call srand(seed)

C Calculate probability such that an event should occur once per simulation.
      PROB1PS=1./real((isdf-isds+1)*24*ntstep)

      return
      end



C *** GREMLINS ***
C Gremlins are model entities that cause mischief during a simulation.
C Generally they can only cause mischief when certain criteria are met,
C and there is a certain frequency of them doing so. For example, a
C gremlin might randomly cause a heating failure, but only during a cold
C period when the heating would be required. Gremlin mischief has
C prescribed enumerations, detailed in the header file "gremlin.h".

C --------------------------------------------------
C Type 1 - prototype boiler gremlin
C Criteria - outside dry bulb temperature <12 deg. C
C Frequency - random, approx. once per simulation
C Mischief - type 1 for 24 hours
      subroutine GRM01(igrm,mischief,ier)

#include "gremlin.h"
      
      COMMON/CLIMI/QFP,QFF,TP,TF,QDP,QDF,VP,VF,DP,DF,HP,HF

      real randnum,duration

      mischief=0

C Am I currently causing mischief?
      imcf=GRMMCF(IGRM)
      if (imcf.gt.0) then

C Have I finished my mischief?
        duration=get_mischief_duration(imcf)
        if (duration.ge.24.0) then
          mischief=-1
        endif

      else

C Do I feel like starting some mischief?
        if (TP.lt.10.0) then
          randnum=rand(0)
          if (randnum.le.PROB1PS) then
            mischief=1
          endif
        endif

      endif

 999  return
      end

C --------------------------------------------------
C Type 2 - prototype climate gremlin
C Criteria - none
C Frequency - random, approx. twice per simulation
C Mischief - type 2 or 3 for 72 hours
      subroutine GRM02(igrm,mischief,ier)

#include "gremlin.h"

      integer mischief
      real randnum

      mischief=0

C Am I currently causing mischief?
      imcf=GRMMCF(IGRM)
      if (imcf.gt.0) then

C Have I finished my mischief?
        duration=get_mischief_duration(imcf)
        if (duration.ge.72.0) then
          mischief=-1
        endif

      else

C Do I feel like starting some mischief?
        randnum=rand(0)
        if (randnum.ge.(1-PROB1PS)) then
          mischief=2
        elseif (randnum.le.PROB1PS) then
          mischief=3
        endif

      endif

 999  return
      end



C --------------------------------------------------
C get_duration returns the current duration of mischief in hours, given
C the mischief index, based on nsinc.
      function get_mischief_duration(imcf)

#include "gremlin.h"
      
      common/simtim/ihrp,ihrf,idyp,idyf,idwp,idwf,nsinc,its
      common/pers/isd1,ism1,isd2,ism2,isds,isdf,ntstep

      get_mischief_duration=(nsinc-MCFSTS(imcf))/ntstep

      return
      end
