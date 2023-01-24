C ESP-r is free software.  You can redistribute it and/or
C modify it under the terms of the GNU General Public
C License as published by the Free Software Foundation 
C (version 2 or later).

C ESP-r is distributed in the hope that it will be useful
C but WITHOUT ANY WARRANTY; without even the implied
C warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
C PURPOSE. See the GNU General Public License for more
C details.

C This file contains the following subroutines.
C MOGCFG - Requests the system configuration file and reports
C          the files associated with the original simulations.
C SYNOPF - Displays the names of the files used in a simulation.

C ********** MOGCFG
C Requests the system configuration file and reports
C the files associated with the original simulations.
C (Similar to MZINPT in espbps/input.f.)

      SUBROUTINE MOGCFG
#include "building.h"
#include "site.h"
#include "model.h"
#include "schedule.h"
#include "net_flow.h"
#include "tdf2.h"
#include "esprdbfile.h"
#include "help.h"

      COMMON/OUTIN/IUOUT,IUIN,IEOUT
      COMMON/FILEP/IFIL
      common/trc/itrc

      integer ncomp,ncon
      COMMON/C1/NCOMP,NCON

      common/rpath/path
      COMMON/CCTLNM/CTLDOC,LCTLF
      COMMON/FOPENED/CFGOK,MLDBOK,MATDBOK,CTLOK,OPTKOK,CFCDBOK
      common/recver/izver,ipver,iever
      character xfile*144,tg*1,delim*1
      common/exporttg/xfile,tg,delim

      CHARACTER*72 path
      character longtmp*144
      CHARACTER MODE*4,OUTSTR*124,LCTLF*72,CTLDOC*248
      character outs*124
      LOGICAL CFGOK,MLDBOK,MATDBOK,CTLOK,OPTKOK,XST,ok,CFCDBOK
      logical dmdsok
      integer ISTRW

      helpinsub='replsim'       ! set for subroutine

C The common code method of reading in a system configuration file
C uses the same common blocks as MOINIT so there is no convenient way
C of checking that they are the same. The header does not contain
C the UTILITY file name etc. so must read in the system configuration
C file here.
 289  IAPROB=IFIL+2
      IFCFG=IFIL+1
      CALL ERPFREE(IFCFG,ISTAT)
      CALL ERPFREE(IUTDF,ISTAT)

C Open the cfg file.
      call FINDFIL(LCFGF,XST)
      IF(XST)THEN
        MODE='ALL'
        CALL ERSYS(LCFGF,IFCFG,IAPROB,MODE,ITRC,IER)
        IF(IER.NE.0)THEN
          WRITE(OUTSTR,'(A,2X,A)')' Problem reading ',LCFGF
          call edisp(iuout,outstr)
        ELSE
          CFGOK=.TRUE.
        ENDIF

C Set the default export file name based on model root name.
          write(xfile,'(2a)') cfgroot(1:lnblnk(cfgroot)),'.csv'

      ELSE

C Configuration file not found.
 45     WRITE(OUTSTR,'(A,A,A)')' Could not find ',LCFGF,'!'
        call edisp(iuout,' ')
        call edisp(iuout,outstr)

C If using X11 assume that the full path is less than 96 chars
C but if using GTK then the full length is needed to avoid truncation.
        helptopic='res_ask_cfg_file'
        call gethelptext(helpinsub,helptopic,nbhelp)
        CALL PHELPD('no system file',nbhelp,'-',0,0,IER)
        write(longtmp,'(a)') LCFGF(1:lnblnk(LCFGF))

        iglib = igraphiclib()  ! find out if X11 or GTK or text support only.
        if(iglib.eq.1.or.iglib.eq.3)then
          ISTRW=96
        elseif(iglib.eq.2)then
          ISTRW=144
        else
          ISTRW=96
        endif
        CALL EASKF(longtmp,' ',
     &    'Corresponding system configuration file name?',
     &    ISTR,' ','configuration file',IER,nbhelp)
        if(ier.eq.-3) return  ! cancel detected pass back -3 in ier.

        if (longtmp.eq.' ') goto 45
        call fdroot(longtmp,path,LCFGF)
        if(longtmp(1:2).ne.'  ')GOTO 289
        RETURN
      ENDIF

C Check the geometry and operation files. Note that
C if izver >=4 the zone operation file is not needed.
      DO 30 J=1,NCOMP
        IUNIT=IFIL+1
        call georead(IUNIT,LGEOM(J),J,1,IUOUT,IER)
        if(izver.eq.4)then
          continue
        else
          CALL EROPER(0,iuout,IUNIT,J,IER)  ! for older library versions
          if(ip3ver(J).eq.0)then
            write(outs,'(a,i3,a)') 
     &      'Operation file for zone ',J,' periods being sorted!'
            call edisp(iuout,outs)
            call PROCESSOLDCAS(J,0,iuout)
          endif
        endif
   30 CONTINUE

C Also scan the model dispersed demands file if it exists.
      IUNIT=IFIL+1
      call FINDFIL(bdmds,XST)
      IF(XST)THEN
        CALL ERPFREE(IUNIT,ISTAT)
        CALL ERBDMD(0,IUNIT,IER)
        dmdsok=.true.
      else
        dmdsok=.false.
      endif

C Check existence of system control file.
      if(CTLDOC(1:4).ne.'NONE')then
        if(LCTLF(1:2).ne.'  ')then
          CALL EZCTLR(IUNIT,0,IUOUT,IER)
          IF(IER.EQ.1)then
            call edisp (iuout,' ')
            call edisp(iuout,'System control file read error!')
          endif
        endif
      endif

C If tdf file used then open it so data is available for reading.
      if(ITDFLG.ne.0)then
        call supplyandcheck(ltdfa,'R',ier)
      endif

C Calculate default external air pressure and air density based on site altitude.
C Although atmospheric pressure may be available in the climate database,
C it is not currently use in psychrometric calculations in the results analyser.
      atmpres = PRESSURE_ALT(sitealt)

C For default air density, assume 0 degree Celsius and 50% RH.
C This will be recalculated as needed at prevailing conditions.
      extairden=AIR_DENSITY(atmpres,0.0,50.0)

C Ask user about description of the files used in the simulation.
      if(itrc.ne.0)then
        call easkok(' ','List files used in the simulation(s)?',
     &               ok,nbhelp)
        if(ok)then
          call SYNOPF
          RETURN
        else
          return
        endif
      else
        RETURN
      endif

      END

C ********** SYNOPF
C Displays the names of the files used in a simulation.

      SUBROUTINE SYNOPF
#include "building.h"
#include "model.h"
#include "CFC_common.h"

      COMMON/OUTIN/IUOUT,IUIN,IEOUT
      COMMON/OUTPCH/ICOUT

      integer ncomp,ncon
      COMMON/C1/NCOMP,NCON
      COMMON/CCTLNM/CTLDOC,LCTLF

      CHARACTER*72 LCTLF
      CHARACTER*248 CTLDOC
      CHARACTER*21 LABELS(8)

      character outs*124

      DATA LABELS/' Zone operation    : ',
     &            ' Zone geometry     : ',
     &            ' Zone construction : ',
     &            ' Zone view factor  : ',
     &            ' Zone shad/insolat : ',
     &            ' Zone transp walls : ',
     &            ' Zone cas gain ctrl: ',
     &            ' Zone cfc constr.  : '/

C Echo names of files for visual check.
      write(outs,59)modeltitle
   59 format('These files were used in model: ',A42)
      call edisp(iuout,outs)
      write(outs,'(A)') ' with control scheme '
      call edisp(iuout,outs)
      call edisp248(iuout,ctldoc,72)
      write(outs,'(A,A57)') ' and control file  : ',LCTLF
      call edisp(iuout,outs)

      DO 10 I=1,NCOMP
        write(outs,'(A,I3)') 'Files for ZONE ',I
        call edisp(iuout,outs)
        write(outs,86)LABELS(1),LPROJ(I)
        call edisp(iuout,outs)
        write(outs,86)LABELS(2),LGEOM(I)
        call edisp(iuout,outs)
        write(outs,86)LABELS(3),LTHRM(I)
        call edisp(iuout,outs)
        IF(IVF(I).EQ.1)THEN
          write(outs,86)LABELS(4),LVIEW(I)
          call edisp(iuout,outs)
        ENDIF
        IF(ISI(I).EQ.1)THEN
          write(outs,86)LABELS(5),LSHAD(I)
          call edisp(iuout,outs)
        ENDIF
        IF(ITW(I).EQ.1)THEN
          write(outs,86)LABELS(6),LTWIN(I)
          call edisp(iuout,outs)
        ENDIF
        IF(ICGC(I).EQ.1)THEN
          write(outs,86)LABELS(7),LCGCIN(I)
          call edisp(iuout,outs)
        ENDIF
        IF(icfc(I).EQ.1)THEN
          write(outs,86)LABELS(8),lcfcin(I)
          call edisp(iuout,outs)
        ENDIF
   10 CONTINUE
   86 FORMAT(A21,A57)
      RETURN
      END

