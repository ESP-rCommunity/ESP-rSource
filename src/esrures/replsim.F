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

C This file contains the following routines which recreate
C data included in sim but not saved in the results library.
C  MOGCFG  reports and checks main files associated with simulation.

C ******************** MOGCFG ********************

C MOGCFG asks for system configuration file and reports back all
C of the files associated with the original simulation.  Similar to
C MZINPT code in espbps/input.f.

      SUBROUTINE MOGCFG
#include "building.h"
#include "model.h"
#include "schedule.h"
#include "net_flow.h"
C #include "geometry.h"
#include "tdf2.h"
#include "esprdbfile.h"

      COMMON/OUTIN/IUOUT,IUIN
      COMMON/FILEP/IFIL
      common/trc/itrc

      integer ncomp,ncon
      COMMON/C1/NCOMP,NCON

      common/rpath/path
      COMMON/CCTLNM/CTLDOC,LCTLF
      COMMON/FOPENED/CFGOK,MLDBOK,MATDBOK,CTLOK,OPTKOK
      common/recver/izver,ipver,iever
      character xfile*144,tg*1,delim*1
      common/exporttg/xfile,tg,delim

      CHARACTER*72 path
      character longtmp*144
      CHARACTER MODE*4,OUTSTR*124,LCTLF*72,CTLDOC*248
      character outs*124
      LOGICAL CFGOK,MLDBOK,MATDBOK,CTLOK,OPTKOK,XST,ok,dok

C For help messages
      character helpinsub*24   ! subroutine name
      character helptopic*24 ! string (unique) for topic
      integer nbhelp     ! number of help lines found

      helpinsub='MOGCFG'  ! set for subroutine

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

C Not found, might be in a remote folder.  Warn user first.
 45     WRITE(OUTSTR,'(A,A)')' Could not find ',LCFGF
        call edisp(iuout,outstr)

C If using X11 assume that the full path is less than 96 chars
C but if using GTK then we need the full length to avoid truncation.
        helptopic='res_ask_cfg_file'
        call gethelptext(helpinsub,helptopic,nbhelp)
        CALL PHELPD('no system file',nbhelp,'-',0,0,IER)
        write(longtmp,'(a)') LCFGF(1:lnblnk(LCFGF))

        iglib = igraphiclib()  ! find out if X11 or GTK or text support only.
        if(iglib.eq.1.or.iglib.eq.3)then
          CALL EASKF(longtmp,' ',
     &      ' Corresponding system configuration file name ? ',
     &      96,' ','configuration file',IER,nbhelp)
        elseif(iglib.eq.2)then
          CALL EASKF(longtmp,' ',
     &      ' Corresponding system configuration file name ? ',
     &      144,' ','configuration file',IER,nbhelp)
        else
          CALL EASKF(longtmp,' ',
     &      ' Corresponding system configuration file name ? ',
     &      96,' ','configuration file',IER,nbhelp)
        endif
        if(ier.eq.-3) return  ! cancel detected pass back -3 in ier.

        if (longtmp.eq.' ') goto 45
        call fdroot(longtmp,path,LCFGF)
        if(longtmp(1:2).ne.'  ')GOTO 289
        RETURN
      ENDIF

C Check the geometry and operation files by reading them in. Note,
C if izver >=4 the zone operation file is not needed.
      call usrmsg(' ',' Checking existence of descriptive files...','-')
      DO 30 J=1,NCOMP
        IUNIT=IFIL+1
          call georead(IUNIT,LGEOM(J),J,1,IUOUT,IER)
        if(izver.eq.4)then
          continue
        else
          CALL EROPER(0,iuout,IUNIT,J,IER)  ! for older library versions
          if(ip3ver(J).eq.0)then
            write(outs,'(a,i2,a)') 
     &      'Operation file for zone ',J,' periods being sorted!'
            call edisp(iuout,outs)
            call PROCESSOLDCAS(J,0,iuout)
          endif
        endif
   30 CONTINUE
      call usrmsg(' ',
     &  ' Checking existence of descriptive files...done.','-')

C Check existance of system control file.
      if(CTLDOC(1:4).ne.'NONE')then
        if(LCTLF(1:2).ne.'  ')then
          CALL EZCTLR(IUNIT,0,IUOUT,IER)
          IF(IER.EQ.1)then
            call edisp(iuout,' System control file read error, or does')
            call edisp(iuout,' not exist.  Take care with analysis!')
          endif
        endif
      endif

C If tdf file used then open it so data is available for reading.
      if(ITDFLG.ne.0)then
        call supplyandcheck(ltdfa,'R',ier)
      endif

C Ask user about description of the files used in the simulation.
      if(itrc.ne.0)then
        dok=.false.
        call askok('Do you want a listing of the files ',
     &             'used in the simulation?',ok,dok,nbhelp)
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

C ******************** SYNOPF ********************
C SYNOPF displays the names of the files used in a simulation

      SUBROUTINE SYNOPF
#include "building.h"
#include "model.h"
#include "CFC_common.h"
      COMMON/OUTIN/IUOUT,IUIN
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
        write(outs,'(A,I2)') 'Files for ZONE ',I
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


C ******************** MZUTIL ********************
C MZUTIL no longer used (utility files scanned from configuration file.


c ******************** MZCGCF ********************
c No longer used - ercgcf(itrc,itru,LCGCIN,ICOMP,ier) instead.


C ******************** MOIHC ********************
C No longer used - convection routines in esrubld used instead.
