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

C This file contains the following routines which recreate
C data included in sim but not saved in the results library.
C  MOGCFG  reports and checks main files associated with simulation.
C  MZCGCF  reads in the casual gain control file.

C ******************** MOGCFG ********************

C MOGCFG asks for system configuration file and reports back all
C of the files associated with the original simulation.  Similar to
C MZINPT code in espbps/input.f.

      SUBROUTINE MOGCFG
#include "building.h"
#include "tdf2.h"
#include "esprdbfile.h"

      COMMON/OUTIN/IUOUT,IUIN
      common/pophelp/h(60)
      COMMON/FILEP/IFIL
      common/trc/itrc


      character CTYPE*4
      real gversion
      integer igupgrade
      COMMON/G0/CTYPE(MCOM),gversion(MCOM),igupgrade
      COMMON/C1/NCOMP,NCON
      COMMON/C2/LSNAM,NCCODE(MCOM),LPROJ(MCOM),LGEOM(MCOM),
     &          LSHAD(MCOM),LTHRM(MCOM),INDUTL(MCOM),LUTIL(MCOM)

C Version of operations file. ip3ver=0 standard, =1 sorted with header
      common/p3ver/ip3ver
      common/rpath/path
      common/C21/IFCFG,cfgroot,LCFGF
      COMMON/CCTLNM/CTLDOC,LCTLF
      COMMON/FOPENED/CFGOK,MLDBOK,MATDBOK,CTLOK,OPTKOK

C Temporal definition commons.
      COMMON/TDFI/IUTDF,ITDFLG,IUTDFA
      COMMON/TDFFT/LTDF,LTDFA

C Defaults.
      COMMON/DEFLT2/DFCFG,DFCTL,DEFRLB,DAFRES,DAPROB,DPNF,DSBEM
      character*72 DFCFG,DFCTL,DEFRLB,DAPROB,DAFRES,DPNF,DSBEM

      CHARACTER*72 LSNAM,LPROJ,LGEOM,LSHAD,LTHRM,LUTIL
      CHARACTER*72 LCFGF,path,LTDF,LTDFA,ltmp
      CHARACTER MODE*4,OUTSTR*124,H*72,LCTLF*72,CTLDOC*248
      character cfgroot*24,outs*124
      LOGICAL CFGOK,MLDBOK,MATDBOK,CTLOK,OPTKOK,XST,ok,dok
      logical newgeo  ! to use for testing if new/old geometry file.

C The common code method of reading in a system configuration file
C uses the same common blocks as MOINIT so there is no convenient way
C of checking that they are the same. The header does not contain
C the UTILITY file name etc. so must read in the system configuration
C file here.
      newgeo=.false.  ! assume older format geometry.
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
      ELSE

C Not found, might be in a remote folder.  Warn user first.
        WRITE(OUTSTR,'(A,A)')' Could not find ',LCFGF
        call edisp(iuout,outstr)
        H(1)='The model configuration file holds the definition of'
        H(2)='the building/plant which was simulated.'
        H(3)='This file was not found.  Please supply the model '
        H(4)='which MATCHES that within the results library.'
        H(5)='IF IT DOES NOT MATCH THEN RESULTS RECOVERY '
        H(6)='WILL BE COMPROMISED! '
        CALL PHELPD('no system file',6,'-',0,0,IER)

        H(1)='The model configuration file holds the definition '
        H(2)='of the building/ plant to be simulated.  It should'
        H(3)='match the contents held within the results library.'
        CALL EASKS(LTMP,' Model configuration file ? ',' ',72,DFCFG,
     &    'config file name',IER,3)
        if(ltmp(1:2).ne.'  ')GOTO 289
        RETURN
      ENDIF

C Check the geometry and utility files by reading them in.
      call usrmsg(' ',' Checking existence of descriptive files...','-')
      DO 30 J=1,NCOMP
        IUNIT=IFIL+1
        CALL EROPER(0,iuout,IUNIT,J,IER)
        if(ip3ver.eq.0)then
          write(outs,'(a,i2,a)') 
     &      'Operation file for zone ',J,' periods being sorted!'
          call edisp(iuout,outs)
          call PROCESSOLDCAS(J,0,iuout,IER)
        endif
        call eclose(gversion(J),1.1,0.01,newgeo)
        if(newgeo)then
          call georead(IUNIT,LGEOM(J),J,1,0,IUOUT,IER)
        else
          call egomin(IUNIT,LGEOM(J),J,1,0,IUOUT,IER)
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
        h(1)='If you say yes you will be presented with a list of'
        h(2)='the files which are associated with the model. '
        call askok(' Do you want a listing of the files ',
     &            ' used in the simulation?',ok,dok,2)
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

      COMMON/OUTIN/IUOUT,IUIN
      COMMON/OUTPCH/ICOUT

      COMMON/C1/NCOMP,NCON
      COMMON/CCTLNM/CTLDOC,LCTLF
      COMMON/C2/LSNAM,NCCODE(MCOM),LPROJ(MCOM),LGEOM(MCOM),
     &          LSHAD(MCOM),LTHRM(MCOM),INDUTL(MCOM),LUTIL(MCOM)

      COMMON/INDICS/IVF(MCOM),ISI(MCOM),IHC(MCOM),
     &              ITW(MCOM),ICGC(MCOM),IOBS(MCOM)
      common/UDESC/LVIEW(MCOM),LHCCO(MCOM),
     &             LTWIN(MCOM),LCGCIN(MCOM),ZOBS(MCOM)

      CHARACTER*72 LVIEW,LHCCO,LTWIN,LCGCIN,ZOBS
      CHARACTER*72 LSNAM,LPROJ,LGEOM,LSHAD,LTHRM,LUTIL,LCTLF

      CHARACTER*248 CTLDOC
      CHARACTER*21 LABELS(10)

      character outs*124

      DATA LABELS/' Zone operation    : ',
     &            ' Zone geometry     : ',
     &            ' Zone construction : ',
     &            ' Zone utilities    : ',
     &            ' Zone view factor  : ',
     &            ' Zone shad/insolat : ',
     &            ' Zone transp walls : ',
     &            ' Zone cas gain ctrl: ',
     &            ' Zone casual gain  : ',
     &            ' Zone blind/shutter: '/

C Echo names of files for visual check.
      write(outs,59)LSNAM
   59 format('These files were used in simulation: ',A42)
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
          write(outs,86)LABELS(5),LVIEW(I)
          call edisp(iuout,outs)
        ENDIF
        IF(ISI(I).EQ.1)THEN
          write(outs,86)LABELS(6),LSHAD(I)
          call edisp(iuout,outs)
        ENDIF
        IF(ITW(I).EQ.1)THEN
          write(outs,86)LABELS(7),LTWIN(I)
          call edisp(iuout,outs)
        ENDIF
        IF(ICGC(I).EQ.1)THEN
          write(outs,86)LABELS(8),LCGCIN(I)
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
