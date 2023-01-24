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

C This file contains the following subroutines:
C GRM_CTL     - Main controller for gremlins.
C GRM_INITSIM - Initialise data structures.
C GRM_INITLOG - Initialise log file.
C GRM_LOG     - Write an entry to the log file.
C GRM??       - Gremlin models.

C ******************** GRM_CTL ********************
C Main controller for gremlins.
      subroutine GRM_CTL(ier)

#include "gremlin.h"
      
      COMMON/OUTIN/IUOUT,IUIN,IEOUT
      COMMON/SIMTIM/IHRP,IHRF,IDYP,IDYF,IDWP,IDWF,NSINC,ITS,IDYNOW

      character outs*128
      character dq*1

C "mischief" returns from gremlin subs:
C 0  = no change
C -1 = finished mischief
C -2 = waiting until criteria are met
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
     &      'GRM_CTL: gremlin type ',GRMTYP(IGRM),' not recognised'
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
          
C Waiting for the opportune moment.
        elseif (mischief.eq.-2) then
          GRMMCF(IGRM)=-2
        endif
      enddo

 999  return

C Error.
 666  call edisp(ieout,outs)
      goto 999

      end

C ******************** GRM_INITSIM ********************
C Initialise gremlin data structures.
      subroutine GRM_INITSIM

#include "gremlin.h"

      common/pers/isd1,ism1,isd2,ism2,isds,isdf,ntstep

      integer dat(8),seed

C Initialise data structures.
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
      
C ******************** GRM_INITLOG ********************
C Open gremlin log file and write header.
      subroutine GRM_INITLOG(IER)

#include "gremlin.h"

      COMMON/FILEP/IFIL

C Open log file and write header.
      GRMLGU=IFIL+34
      call efopseq(GRMLGU,GRMLOG,3,IER)
      if (IER.ne.0) goto 666
      write(GRMLGU,'(a)',IOSTAT=IER,ERR=667)
     &  '# ESP-r random perturbation (RP) log file.'
      write(GRMLGU,11,IOSTAT=IER,ERR=667)'# Date  ','Time ','RP',
     &  'Model','Description                   ','Zone','Zone name   ',
     &  'Event','Description                   ','State'

  999 return

   11 format(a,9(' | ',a))
   
  666 call edisp(IEOUT,'GRM_INITLOG: error opening gremlin log file.')
      goto 999
  667 call edisp(IEOUT,'GRM_INITLOG: error writing gremlin log header.')
      goto 999

      end

C ******************** GRM_LOG ********************
C Write a gremlin event (aka mischief) to the log file.
      subroutine GRM_LOG(igrm,imcf,ier)

#include "building.h"
#include "geometry.h"
#include "gremlin.h"

      COMMON/SET1/IYEAR,IBDOY,IEDOY,IFDAY,IFTIME
      COMMON/PERS/ISD1,ISM1,ISD2,ISM2,ISDS,ISDF,NTSTEP
      COMMON/SIMTIM/IHRP,IHRF,IDYP,IDYF,IDWP,IDWF,NSINC,ITS,IDYNOW
      COMMON/AVRAGE/IDAVER
      common/PREC7/ITCNST
      character*8 datestr,dum1,dum2
      character*5 timestr,dum3,dum4
      character state*8,zonam*12,pdesc*40

C Get time and date.
      call stdate(IYEAR,IDYP,dum1,dum2,datestr)
      iits=NSINC-((IDYNOW-ISDS+ITCNST)*24-1)*NTSTEP
      call estime(NTSTEP,IDAVER,iits,dum3,dum4,timestr,fdum)

C Write to log file.
      if (GRMZON(igrm).gt.0) then
        zonam=zname(GRMZON(igrm))
      else
        zonam='n/a         '
      endif
      if (imcf.gt.0) then
        ievt=imcf
        state='started '
      elseif (imcf.eq.-1) then
        ievt=MCFTYP(GRMMCF(igrm))
        state='finished'
      endif

      write(GRMLGU,11,IOSTAT=ier,ERR=666)datestr,timestr,igrm,
     &  GRMTYP(igrm),GRMDESC(GRMTYP(igrm)),GRMZON(igrm),zonam,ievt,
     &  MCFDESC(ievt),state
     
  999 return

   11 format(2(a,' | '),i2,' | ',i5,' | ',a,' | ',i4,' | ',a,' | ',i5,
     &       2(' | ',a))

  666 call edisp(ieout,'GRM_LOG: error writing to gremlin log file.')
      goto 999

      end      

C *** GREMLINS ***
C Gremlins are model entities that cause mischief during a simulation.
C Generally they can only cause mischief when certain criteria are met,
C and there is a certain frequency of them doing so. For example, a
C gremlin might randomly cause a heating failure, but only during a cold
C period when the heating would be required. Gremlin mischief has
C prescribed enumerations, detailed in the header file "gremlin.h".

C ******************** GRM01 ********************
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

C Am I waiting for the opportune moment to cause mischief?
      elseif (imcf.eq.-2) then
        if (TP.lt.12.0) mischief=1

C Do I feel like starting some mischief?
      else
        randnum=rand(0)
        if (randnum.le.PROB1PS) then
          if (TP.lt.12.0) then ! within criteria, start now
            mischief=1
          else ! mischief criteria not met, start next time it is
            mischief=-2
          endif
        endif
      endif

C Log mischief.
      if (mischief.gt.0.or.mischief.eq.-1) 
     &  CALL GRM_LOG(igrm,mischief,ier)

 999  return
      end

C ******************** GRM02 ********************
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

C Do I feel like starting some mischief?
      else
        randnum=rand(0)
        if (randnum.ge.(1-PROB1PS)) then
          mischief=2
        elseif (randnum.le.PROB1PS) then
          mischief=3
        endif

      endif

C Log mischief.
      if (mischief.gt.0.or.mischief.eq.-1) 
     &  CALL GRM_LOG(igrm,mischief,ier)

 999  return
      end

C ******************** get_mischief_duration ********************
C Returns the current duration of mischief in hours, given the mischief
C index, based on nsinc.
      function get_mischief_duration(imcf)

#include "gremlin.h"
      
      common/simtim/ihrp,ihrf,idyp,idyf,idwp,idwf,nsinc,its,idynow
      common/pers/isd1,ism1,isd2,ism2,isds,isdf,ntstep

      get_mischief_duration=(nsinc-MCFSTS(imcf))/ntstep

      return
      end
