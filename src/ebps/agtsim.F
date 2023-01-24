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
C AGT_CTL     - Main controller for agents.
C AGT_INITSIM - Initialise data structures.
C AGT_INITLOG - Initialise log file.
C AGT_LOG     - Write an entry to the log file.
C AGT_STOP    - Stop the simulation if there is a problem.
C AGT??       - Agent models.

C ******************** AGT_CTL ********************
C Main controller for agents.
      subroutine AGT_CTL(problem,ier)

#include "building.h"
#include "agent.h"

      integer problem,ier

      COMMON/OUTIN/IUOUT,IUIN,IEOUT
      common/occup/isocc(MCOM)
      logical isocc

      character outs*128
      integer probtyp,probtyps(MAGT)

C If there has already been a problem flagged, just return.
      if (problem.eq.1) goto 999

      problem=-1

      if (NAGT.eq.0) goto 999

      problem=0

C Query occupant agents.
      do iagt=1,NAGT
        probtyps(IAGT)=0
        if (AGTMAN(AGTTYP(IAGT))) cycle

C Check for occupancy.
        if (AGTZON(IAGT).gt.0) then
          if (.not.isocc(AGTZON(IAGT))) cycle
        endif

        select case (AGTTYP(IAGT))
        case(1)
          call AGT01(iagt,probtyp,ier)
        case default
          IER=1
          write(outs,'(a,i2,a)')
     &      'AGT_CTL: occupancy agent type ',AGTTYP(IAGT),
     &      ' not recognised'
          goto 666
        end select

        if (IER.ne.0) goto 666

C There has been a complaint, log this.
        if (probtyp.gt.0) then
          probtyps(IAGT)=probtyp
        endif
      enddo

C Query management agents.
      do iagt=1,NAGT
        if (.not.AGTMAN(AGTTYP(IAGT))) cycle

C Check for occupancy.
        if (AGTZON(IAGT).gt.0) then
          if (.not.isocc(AGTZON(IAGT))) cycle
        endif

        select case (AGTTYP(IAGT))
        case(2)
          call AGT02(iagt,probtyp,probtyps,ier)
        case default
          IER=1
          write(outs,'(a,i2,a)')
     &      'AGT_CTL: management agent type ',AGTTYP(IAGT),
     &      ' not recognised'
          goto 666
        end select

        if (IER.ne.0) goto 666
        if (probtyp.gt.0) then
          probtyps(IAGT)=probtyp
          problem=1
          exit
        endif

      enddo

 999  return

C Error.
 666  call edisp(ieout,outs)
      goto 999

      end

C ******************** AGT_INITSIM ********************
C Initialise agent data structures.
      subroutine AGT_INITSIM

#include "agent.h"

C Initialise data structures.
      do i=1,MAGT
        do j=1,MPROB
          CMPLTS(i,j)=0
        enddo
      enddo
      AGTNUM=0
      PRBNUM=0
      write(SUPDAT,'(i1)')0
     
      return

      end
      
C ******************** AGT_INITLOG ********************
C Open agent log file and write header.
      subroutine AGT_INITLOG(IER)

#include "agent.h"

      COMMON/FILEP/IFIL
      COMMON/OUTIN/IUOUT,IUIN,IEOUT

      AGTLGU=IFIL+33
      call efopseq(AGTLGU,AGTLOG,3,IER)
      if (IER.ne.0) goto 666
      write(AGTLGU,'(a)',IOSTAT=IER,ERR=667)'# ESP-r agent log file.'
      write(AGTLGU,11,IOSTAT=IER,ERR=667)'# Date  ','Time ','Agent',
     &  'Model','Description                   ','Type      ','Zone',
     &  'Zone name   ','Complaint','Description'

  999 return

   11 format(a,9(' | ',a))

  666 call edisp(IEOUT,'AGT_INITLOG: error opening agent log file.')
      goto 999
  667 call edisp(IEOUT,'AGT_INITLOG: error writing agent log header.')
      goto 999

      end

C ******************** AGT_LOG ********************
C Write an agent complaint to the log file.
      subroutine AGT_LOG(iagt,icmt,ier)

#include "building.h"
#include "geometry.h"
#include "agent.h"

      COMMON/SET1/IYEAR,IBDOY,IEDOY,IFDAY,IFTIME
      COMMON/PERS/ISD1,ISM1,ISD2,ISM2,ISDS,ISDF,NTSTEP
      COMMON/SIMTIM/IHRP,IHRF,IDYP,IDYF,IDWP,IDWF,NSINC,ITS,IDYNOW
      COMMON/AVRAGE/IDAVER
      common/PREC7/ITCNST
      character*8 datestr,dum1,dum2
      character*5 timestr,dum3,dum4
      character stype*10,zonam*12

C Get time and date.
      call stdate(IYEAR,IDYP,dum1,dum2,datestr)
      iits=NSINC-((IDYNOW-ISDS+ITCNST)*24-1)*NTSTEP
      call estime(NTSTEP,IDAVER,iits,dum3,dum4,timestr,fdum)

C Write to log file.
      if (AGTMAN(AGTTYP(iagt))) then
        stype='management'
      else
        stype='occupant  '
      endif
      if (AGTZON(iagt).gt.0) then
        zonam=zname(AGTZON(iagt))
      else
        zonam='n/a         '
      endif
      write(AGTLGU,11,IOSTAT=ier,ERR=666)datestr,timestr,iagt,
     &  AGTTYP(iagt),AGTDESC(AGTTYP(iagt)),stype,AGTZON(iagt),zonam,
     &  icmt,PROBDESC(icmt)
     
  999 return

   11 format(2(a,' | '),2(i5,' | '),2(a,' | '),i4,' | ',a,' | ',i9,
     &       ' | ',a)

  666 call edisp(ieout,'AGT_LOG: error writing to agent log file.') 
      goto 999

      end      

C ******************** AGT_STOP ********************
C Stop a simulation if there is a problem.
      subroutine AGT_STOP(IDAY)

#include "agent.h"

      COMMON/PERS/ISD1,ISM1,ISD2,ISM2,ISDS,ISDF,NTSTEP

C Set simulation finish day to this day.
      CALL EDAYR(IDAY,ISD2,ISM2)
      ISDF=IDAY

C Rewrite results library header with this finish day.
      CALL MZLS2
      CALL MZSAVE(i,j)

C Write to log file.
      select case (PRBNUM)
      case(3)
        write(AGTLGU,11,ERR=666)'# Agent ',AGTNUM,
     &    ' has halted the simulation',
     &    ' because they received too many complaints of type ',
     &    SUPDAT(1:lnblnk(SUPDAT)),'.'
      case default
        write(AGTLGU,99,ERR=666)'# Agent ',AGTNUM,
     &    ' has halted the simulation.'
      end select

C Kill simulation with error code 42.
      write(6,'(a)')'This is not an error.'
      write(6,'(a)')'A management agent has halted the simulation.'
      write(6,'(a)')'Results have been saved for the simulated period.'
  999 STOP 42

   11 format(a,i2,4a)
   99 format(a,i2,a)

  666 call edisp(ieout,'AGT_STOP: error writing to agent log file.')
      goto 999

      end

C *** AGENTS ***
C Agents are model entities intended to represent people. If certain
C criteria are met, they complain. There are two kinds - management and
C occupant. Occupant agents are general building occupants; if they get
C uncomfortable they complain to someone in authority e.g. a facilities
C manager. Management agents are these authority figures; they complain
C if the building is not performing as it should e.g. if they recieve
C too many complaints from occupant agents. Complaints from management
C agents will halt the simulation. Agent complaints have prescribed
C enumerations, detailed in the header file "agent.h". When a new agent
C is added here, the header file should be updated accordingly.

C ******************** AGT01 ********************
C Type 1 - Basic prototype occupant agent.
C Criteria - Zone dry bulb temperature 18 - 25 deg. C
C Complaints - type 1 or 2
      subroutine AGT01(iagt,probtyp,ier)
#include "building.h"
#include "agent.h"

      integer probtyp

      COMMON/FVALA/TFA(MCOM),QFA(MCOM)

      probtyp=0

C Check 18 <= temperature <= 25
      if (TFA(AGTZON(IAGT)).gt.25.0) then
        probtyp=1
      elseif (TFA(AGTZON(IAGT)).lt.18.0) then
        probtyp=2
      endif

C Log complaints.
      if (probtyp.gt.0) CALL AGT_LOG(iagt,probtyp,ier)

      return
      end

C ******************** AGT02 ********************
C Type 2 - Basic prototype management agent.
C Criteria - >50 complaints of the same type.
C Complaints - type 3
      subroutine AGT02(iagt,probtyp,probtyps,IER)

#include "agent.h"

      integer probtyp,probtyps(MAGT)

      probtyp=0

C Add complaints for this time step.
      do iprob=1,NAGT
        if (probtyps(iprob).ne.0) then
          CMPLTS(IAGT,probtyps(iprob))=CMPLTS(IAGT,probtyps(iprob))+1
        endif
      enddo

C Check complaints <= 50
      do iprob=1,MPROB
        if (CMPLTS(IAGT,iprob).gt.50) then
          probtyp=3
          AGTNUM=iagt
          PRBNUM=probtyp
          write(SUPDAT,'(i2)')iprob
        endif
      enddo
      
      if (probtyp.gt.0) CALL AGT_LOG(iagt,probtyp,ier)

      return
      end
