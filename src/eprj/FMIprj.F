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


C FMIprj.F contains specification fortran functions for the FMI
C implementation:
C FMI_ED    - Controlling routine for defining and editing FMI links.
C FMI_EDINP - Define and edit FMU inputs (ESP-r -> FMU).
C FMI_EDOUT - Define and edit FMU outputs (FMU -> ESP-r).

C ******************** FMI_ED
C FMI_ED controls the definition and editing of FMI links.

      SUBROUTINE FMI_ED(IER)
#include "building.h"
#include "model.h"
#include "FMI.h"
#include "espriou.h"
#include "help.h"

      common/FILEP/IFIL
      COMMON/OUTIN/IUOUT,IUIN,IEOUT

      DIMENSION ITEMS(14)
      character ITEMS*56
      character ctmp*72,fs*1,FMISFL_tmp*72
      logical unixok,xst,saved,none
      integer itmp,itmp2,itmp3,itmp4,IFMU,NITEMS,IPICK

      character*124 outs,errstr,errval
      character*72 sfile,snpfile

      helpinsub='FMIprj'

C Set folder separator (fs) to \ or / as required.
      call isunix(unixok)
      if(unixok)then
        fs = char(47)
      else
        fs = char(92)
      endif

C Ask for FMI specification file name.
    3 write(ctmp,'(a,a,a,a)')ctlpth(1:lnblnk(ctlpth)),fs,
     &  cfgroot(1:lnblnk(cfgroot)),'.fmi'
      if (FMISFL(1:7).eq.'UNKNOWN') then
        xst=.false.
        FMISFL_tmp=ctmp
        CALL EASKS2CMD(FMISFL_tmp,' FMI specification file?',' ',
     &    'cancel','browse',itmp,72,ctmp,'FMI spec file',IER,0)
        CALL USRMSG(' ',' ','-')
      else
        xst=.true.
        FMISFL_tmp=FMISFL
        CALL EASKS2CMD(FMISFL_tmp,' FMI specification file?',' ',
     &    'dereference','browse',itmp,72,ctmp,'FMI spec file',IER,
     &    0)
        CALL USRMSG(' ',' ','-')
      endif
C Cancelled or dereferenced FMI spec file.
C If dereferenced, deactivate FMI and save cfg file before returning.
      if (itmp.eq.1) then
        if (xst) then
          CALL FMI_CLEARALL()
          CALL EMKCFG('-',IER)
        endif
        goto 999
C Browse ctl directory.
      elseif (itmp.eq.2) then
        sfile='  '
        snpfile='  '
        call browsefilelist('?','ctl','fil',sfile,snpfile,nfile,IER)
        if (nfile.gt.0) then
          sfile='  '
          snpfile='  '
          call browsefilelist('b','ctl','fil',sfile,snpfile,nfile,IER)
          if (snpfile(1:2).ne.'  ') then
            write(FMISFL_tmp,'(3a)')ctlpth(1:lnblnk(ctlpth)),fs,
     &        snpfile(1:lnblnk(snpfile))
          else
            CALL edisp(iuout,' No file selection detected.')
            goto 3
          endif
        else
          CALL edisp(iuout,' No files in ctl directory.')
          goto 3
        endif
      endif

C FMI specification file name given, see if it exists.
C If it does, open and read file.
C If it doesn't, offer user choice to choose another or open a new file.
      IUF=IFIL+1
      CALL ERPFREE(IUF,ISTAT)
      CALL FINDFIL(FMISFL_tmp,xst)
      if (xst) then
        write(currentfile,'(a)')FMISFL_tmp(1:lnblnk(FMISFL_tmp))
        CALL EFOPSEQ(IUF,FMISFL_tmp,1,ISTAT)
        if (ISTAT.ne.0) then
          IER=3
          goto 666
        endif
        CALL FMI_RD(IUF,IER)
      else
        CALL EASKAB(' File not found.','Options:',
     &    'specify another','make new file',itmp,0)
        if (itmp.eq.1) goto 3
        write(outs,'(3a)')' New file ',
     &    FMISFL_tmp(1:lnblnk(FMISFL_tmp)),' will be created.'
        CALL edisp(iuout,outs)
      endif

C If we've made it this far, we should have a valid FMI spec file name.
C Save this to common.
      FMISFL=FMISFL_tmp

C Set up menu.
      IFMU=1
      saved=.true.
    4 if (FMUNOF.eq.0) then
        none=.true.
      else
        none=.false.
      endif
      IPICK=-4
      IER=0
      if (none) then
        ITEMS(1)=                  ' No FMU specifications found,'
        ITEMS(2)=                  ' please add one to proceed.'
        ITEMS(3)=                  ' ----------------------------------'
        ITEMS(4)=                  '+ add/delete/copy FMU specification'
        ITEMS(5)=                  '! listing not currently available '
        ITEMS(6)=                  '> save FMI file'
        ITEMS(7)=                  '? help'
        ITEMS(8)=                  '- exit menu'
        NITEMS=8
      else
        write(ITEMS(1),'(a,i1)')   'a FMU number >> ',IFMU
        if (lnblnk(FMUFIL(IFMU)).gt.44) then
          write(ITEMS(2),'(3a)') 'b FMU file: ',FMUFIL(IFMU)(1:41),'...'
        else
          write(ITEMS(2),'(2a)') 'b FMU file: ',
     &      FMUFIL(IFMU)(1:lnblnk(FMUFIL(IFMU)))
        endif
        write(ITEMS(3),'(2a)')     'c description: ',FMUDSC(IFMU)
        write(ITEMS(4),'(a,f10.2)')'d timeout: ',FMUTO(IFMU)
        if (FMULOG(IFMU).eq.0) then
          ITEMS(5)=                'e logging >> off'
        else
          ITEMS(5)=                'e logging >> on'
        endif
        ITEMS(6)=' ----------------------------------------------------'
        write(ITEMS(7),'(a,i3,a)') 'f inputs (',FMUNUMI(IFMU),
     &                                                       ' defined)'
        write(ITEMS(8),'(a,i3,a)') 'g outputs (',FMUNUMO(IFMU),
     &                                                       ' defined)'
        ITEMS(9)=' ----------------------------------------------------'
        ITEMS(10)=                 '+ add/delete/copy FMU specification'
        ITEMS(11)=                 '! list FMU specification data'
        ITEMS(12)=                 '> save FMI file'
        ITEMS(13)=                 '? help'
        ITEMS(14)=                 '- exit menu'
        NITEMS=14
      endif

      CALL EMENU('FMI specification',ITEMS,NITEMS,IPICK)

C Exit, offering user chance to save modified data.
      if (IPICK.eq.NITEMS) then
        if ((.not.saved).and.(.not.none)) then
          CALL EASKABC(' Modified data not saved!','Options:',
     &      'save and exit','exit without saving','cancel',itmp,0)
          if (itmp.eq.1) then
            xst=.false.
            do itmp=1,FMUNOF
              if (FMUFIL(IFMU)(1:7).eq.'UNKNOWN') then
                if (.not.xst) then
                  write(outs,'(a,i1)')
     &            ' Warning! Files not specified for FMU numbers: ',itmp
                  xst=.true.
                  K=48
                else
                  write(outs,'(2a,i1)')outs(1:K),', ',itmp
                  K=K+3
                endif
              endif
            enddo
            if (xst) then
              CALL edisp(iuout,outs)
              CALL EASKAB(' Some FMU files not specified!','Options:',
     &          'save anyway','cancel',itmp,0)
              if (itmp.eq.2) goto 4
            endif
            CALL FMI_WT(IUF,IER)
            if (FMUNOF.gt.0) then
              is_FMU=.true.
            else
              is_FMU=.false.
            endif
          elseif (itmp.eq.3) then
            goto 4
          endif
        else
          if (FMUNOF.gt.0) then
            is_FMU=.true.
          else
            is_FMU=.false.
          endif
        endif
        goto 999

C Show help.
      elseif (IPICK.eq.NITEMS-1) then
        if (none) then
          helptopic='FMI_spec_none'
        else
          helptopic='FMI_spec_some'
        endif
        CALL gethelptext(helpinsub,helptopic,nbhelp)
        CALL PHELPD('FMI specification help',nbhelp,'-',0,0,IER)

C Save, checking that all FMU files have been defined and warning user
C if not.
      elseif (IPICK.eq.NITEMS-2) then
        xst=.false.
        do itmp=1,FMUNOF
          if (FMUFIL(IFMU)(1:7).eq.'UNKNOWN') then
            if (.not.xst) then
              write(outs,'(a,i1)')
     &          ' Warning! FMU files not specified for numbers: ',itmp
              xst=.true.
              K=48
            else
              write(outs,'(2a,i1)')outs(1:K),', ',itmp
              K=K+3
            endif
          endif
        enddo
        if (xst) then
          CALL edisp(iuout,outs)
          CALL EASKAB(' Some FMU files not specified!','Options:',
     &      'save anyway','cancel',itmp,0)
          if (itmp.eq.2) goto 4
        endif
        CALL FMI_WT(IUF,IER)
        saved=.true.

C List data.
      elseif (IPICK.eq.NITEMS-3) then
        if (none) then
          CALL USRMSG(' No FMU specifications to list!',' ','-')
          goto 4
        endif
        CALL FMI_LIST(IUOUT,IFMU,'A',IER)

C Manage FMU specifications.
      elseif (IPICK.eq.NITEMS-4) then
        helptopic='manage_FMUs'
        CALL gethelptext(helpinsub,helptopic,nbhelp)
        CALL EASKABCD(' ','Options:',
     &    'add','delete','copy','cancel',itmp,nbhelp)

C Add an FMU spec at the end of the list. First check that it doesn't
C exceed the maximum. Commons should already be initialised.
        if (itmp.eq.1) then
          if ((FMUNOF+1).gt.MFMU) then
            write(outs,'(a,i1,a)')
     &        ' Cannot add another FMU; maximum is ',MFMU,'.'
            call USRMSG(outs,' ','-')
            goto 4
          endif
          FMUNOF=FMUNOF+1
          IFMU=FMUNOF
          if (saved) saved=.false.

C Delete an FMU spec. Ask user for what number.
C Need to shift all common data for subsequent sets down by an FMU
C index, clear the last set, and decrement the number of FMUs.
        elseif (itmp.eq.2) then
          if (none) then
            CALL USRMSG(' No FMU specifications to delete!',' ','-')
            goto 4
          endif
          itmp=IFMU
          CALL EASKI(itmp,' ',' Which number FMU to delete?',1,'F',
     &      FMUNOF,'F',IFMU,'FMU number',IER,nbhelp)
          if (itmp.lt.FMUNOF) then
            do itmp2=itmp+1,FMUNOF
              CALL FMI_CLEARFMU(itmp2-1)

              FMUTO(itmp2-1)=FMUTO(itmp2)
              FMULOG(itmp2-1)=FMULOG(itmp2)
              FMUFIL(itmp2-1)=FMUFIL(itmp2)
              FMUNIS(itmp2-1)=FMUNIS(itmp2)
              do itmp3=1,FMUNIS(itmp2)
                FMUINS(itmp2-1,itmp3)=FMUINS(itmp2,itmp3)
              enddo
              FMUDSC(itmp2-1)=FMUDSC(itmp2)

              FMUNUMI(itmp2-1)=FMUNUMI(itmp2)
              do itmp3=1,FMUNUMI(itmp2)
                FMUIZON(itmp2-1,itmp3)=FMUIZON(itmp2,itmp3)
                FMUIVAR(itmp2-1,itmp3)=FMUIVAR(itmp2,itmp3)
                do itmp4=1,MFMUSUP
                  FMUISUP(itmp2-1,itmp3,itmp4)=
     &              FMUISUP(itmp2,itmp3,itmp4)
                enddo
                FMUIINS(itmp2-1,itmp3)=FMUIINS(itmp2,itmp3)
                FMUIVNM(itmp2-1,itmp3)=FMUIVNM(itmp2,itmp3)
              enddo

              FMUNUMO(itmp2-1)=FMUNUMO(itmp2)
              do itmp3=1,FMUNUMO(itmp2)
                FMUOZON(itmp2-1,itmp3)=FMUOZON(itmp2,itmp3)
                FMUOVAR(itmp2-1,itmp3)=FMUOVAR(itmp2,itmp3)
                do itmp4=1,MFMUSUP
                  FMUOSUP(itmp2-1,itmp3,itmp4)=
     &              FMUOSUP(itmp2,itmp3,itmp4)
                enddo
                FMUOINS(itmp2-1,itmp3)=FMUOINS(itmp2,itmp3)
                FMUOVNM(itmp2-1,itmp3)=FMUOVNM(itmp2,itmp3)
              enddo
            enddo
          endif
          CALL FMI_CLEARFMU(FMUNOF)
          FMUNOF=FMUNOF-1
          if (saved) saved=.false.

C Copy an FMU spec. First check that it won't exceed maximum. Ask user
C for what number.
C Need to increment number of FMUs, then copy commons for selected FMU
C to the last (new) one.
        elseif (itmp.eq.3) then
          if (none) then
            CALL USRMSG(
     &        ' No FMU specifications to copy!',' ','-')
            goto 4
          endif
          if ((FMUNOF+1).gt.MFMU) then
            write(outs,'(a,i1,a)')
     &        ' Cannot add another FMU; maximum is ',MFMU,'.'
            call USRMSG(outs,' ','-')
            goto 4
          endif
          itmp=IFMU
          CALL EASKI(itmp,' ',' Which number FMU to copy?',1,'F',
     &      FMUNOF,'F',IFMU,'FMU number',IER,nbhelp)
          FMUNOF=FMUNOF+1

          FMUTO(FMUNOF)=FMUTO(itmp)
          FMULOG(FMUNOF)=FMULOG(itmp)
          FMUFIL(FMUNOF)=FMUFIL(itmp)
          FMUNIS(FMUNOF)=FMUNIS(itmp)
          do itmp3=1,FMUNIS(itmp)
            FMUINS(FMUNOF,itmp3)=FMUINS(itmp,itmp3)
          enddo
          FMUDSC(FMUNOF)=FMUDSC(itmp)

          FMUNUMI(FMUNOF)=FMUNUMI(itmp)
          do itmp3=1,FMUNUMI(itmp)
            FMUIZON(FMUNOF,itmp3)=FMUIZON(itmp,itmp3)
            FMUIVAR(FMUNOF,itmp3)=FMUIVAR(itmp,itmp3)
            do itmp4=1,MFMUSUP
              FMUISUP(FMUNOF,itmp3,itmp4)=FMUISUP(itmp,itmp3,itmp4)
            enddo
            FMUIINS(FMUNOF,itmp3)=FMUIINS(itmp,itmp3)
            FMUIVNM(FMUNOF,itmp3)=FMUIVNM(itmp,itmp3)
          enddo

          FMUNUMO(FMUNOF)=FMUNUMO(itmp)
          do itmp3=1,FMUNUMO(itmp)
            FMUOZON(FMUNOF,itmp3)=FMUOZON(itmp,itmp3)
            FMUOVAR(FMUNOF,itmp3)=FMUOVAR(itmp,itmp3)
            do itmp4=1,MFMUSUP
              FMUOSUP(FMUNOF,itmp3,itmp4)=FMUOSUP(itmp,itmp3,itmp4)
            enddo
            FMUOINS(FMUNOF,itmp3)=FMUOINS(itmp,itmp3)
            FMUOVNM(FMUNOF,itmp3)=FMUOVNM(itmp,itmp3)
          enddo

          IFMU=FMUNOF
          if (saved) saved=.false.
        endif

C These options are only available if there is at least one FMU.
      elseif (.not.none) then

C Cycle through FMUs.
C NOTE: In future when many FMUs are allowed, this toggle might need to
C be changed to an EASKI dialogue or a list.
        if (IPICK.eq.1) then
          IFMU=IFMU+1
          if (IFMU.gt.FMUNOF) IFMU=1

C Define FMU file. Ask user, then check to see if it exists, and query
C user if it does not.
        elseif (IPICK.eq.2) then
          write(ctmp,'(2a)')cfgroot(1:lnblnk(cfgroot)),'.fmu'
    6     helptopic='enter_FMU_file'
          CALL gethelptext(helpinsub,helptopic,nbhelp)
          CALL EASKS(FMUFIL(IFMU),' ',' FMU file?',72,ctmp,'FMU file',
     &      IER,nbhelp)

          CALL FINDFIL(FMUFIL(IFMU),xst)
          if (.not.xst) then
            helptopic='FMU_file_not_found'
            CALL gethelptext(helpinsub,helptopic,nbhelp)
            CALL EASKAB('Cannot find file!','Options:',
     &        'respectify','cancel',itmp,nbhelp)
            if (itmp.eq.1) goto 6
          endif
          if (saved) saved=.false.

C Edit description.
        elseif (IPICK.eq.3) then
          CALL EASKS(FMUDSC(IFMU),' ',' FMU description?',30,
     &      'This FMU is ...','FMU description',IER,nbhelp)
          if (saved) saved=.false.

C Define timeout.
        elseif (IPICK.eq.4) then
          CALL EASKR(FMUTO(IFMU),' ',' Timeout?',0.0,'F',1.0,'-',0.0,
     &      'FMU timeout',IER,0)
          if (saved) saved=.false.

C Toggle logging.
        elseif (IPICK.eq.5) then
          if (FMULOG(IFMU).eq.0) then
            FMULOG(IFMU)=1
          else
            FMULOG(IFMU)=0
          endif
          if (saved) saved=.false.

C Inputs.
        elseif (IPICK.eq.7) then
          CALL FMI_EDINP(IFMU,saved,IER)

C Outputs.
        elseif (IPICK.eq.8) then
          CALL FMI_EDOUT(IFMU,saved,IER)

        endif
      endif
      goto 4

  999 RETURN

C Error handling.
C IER = 3 - Cannot open file, errstr unused, errval = file path
  666 if (IER.eq.3) then
        write(outs,'(3a)')'FMI_ED: Cannot open FMI spec file ',
     &    errval(1:lnblnk(errval)),'.'
      endif
      call edisp(iuout,outs)
      goto 999

      END



C ******************** FMI_EDINP
C FMI_EDINP controls the definition and editing of FMU inputs. This
C creates directives for data to be passed from ESP-r to the FMU.

      SUBROUTINE FMI_EDINP(IFMU,saved,IER)

#include "building.h"
#include "FMI.h"
#include "epara.h"
#include "geometry.h"
#include "help.h"

      logical saved

      common/FOPENED/CFGOK,MLDBOK,MATDBOK,CTLOK,OPTKOK,CFCDBOK
      logical CFGOK,MLDBOK,MATDBOK,CTLOK,OPTKOK,CFCDBOK
      COMMON/C1/NCOMP,NCON
      INTEGER NCOMP,NCON
      COMMON/OUTIN/IUOUT,IUIN,IEOUT

      DIMENSION ITEMS(50)
      CHARACTER ITEMS*56

      DIMENSION arrtmp(10)
      INTEGER arrtmp

      CHARACTER KEY*1,ctmp_30*30,ctmp_32*32,ctmp1_56*56,ctmp2_56*56,
     &          outs*124
      INTEGER M,i,itmp,NITEMS,pagstr(2)
      logical get_pagstr
      real rtmp

      helpinsub='FMIprj'

      get_pagstr=.false.

C Set up multi-page menu.
  6   MHEAD=6
      MCTL=5
      ILEN=FMUNUMI(IFMU)
      IPACT=CREATE
      CALL EKPAGE(IPACT)
      if (get_pagstr) then
        ist=pagstr(1)
        ipm=pagstr(2)
        get_pagstr=.false.
      endif
  4   IPICK=-4
      IER=0

C Header.
      write(ITEMS(1),'(a,i1)')' FMU number: ',IFMU
      write(ITEMS(2),'(2a)')  ' description: ',FMUDSC(IFMU)
      write(ITEMS(3),'(a,i3)')' number of inputs: ',FMUNUMI(IFMU)
      ITEMS(4)=  ' ----------------------------------------------------'
      ITEMS(5)=  '     |     ESP-r     |      FMU      |      FMU      '
      ITEMS(6)=  ' zone| variable name | instance name | variable name '
      M=MHEAD

C Paginated part.
      if (FMUNUMI(IFMU).gt.0) then
        do i=1,FMUNUMI(IFMU)
          if (i.ge.IST.and.i.le.(IST+MIFULL)) then
            M=M+1
            CALL EMKEY(i,KEY,IER)
            write(ctmp1_56,'(A1,1X,I2,2X)')KEY,FMUIZON(IFMU,i)
            itmp=FMUIVAR(IFMU,i)
            if (itmp.gt.0.and.itmp.le.MFMIIREFS) then
              ctmp_30=FMIIREFS(itmp)
            else
              ctmp_30='UNKNOWN'
            endif
            itmp=lnblnk(ctmp_30)
            if (itmp.gt.15) then
              write(ctmp2_56,'(3A,1X)')ctmp1_56(1:6),ctmp_30(1:12),'...'
            else
              write(ctmp2_56,'(2A,1X)')ctmp1_56(1:6),ctmp_30(1:15)
            endif
            ctmp_30=FMUIINS(IFMU,i)
            itmp=lnblnk(ctmp_30)
            if (itmp.gt.15) then
              write(ctmp1_56,'(3A,1X)')ctmp2_56(1:22),ctmp_30(1:12),
     &          '...'
            else
              write(ctmp1_56,'(2A,1X)')ctmp2_56(1:22),ctmp_30(1:15)
            endif
            ctmp_30=FMUIVNM(IFMU,i)
            itmp=lnblnk(ctmp_30)
            if (itmp.gt.15) then
              write(ctmp2_56,'(3A)')ctmp1_56(1:38),ctmp_30(1:12),'...'
            else
              write(ctmp2_56,'(2A)')ctmp1_56(1:38),ctmp_30(1:15)
            endif
            ITEMS(M)=ctmp2_56
          endif
        enddo
      endif

C Control options.
      if (IPFLG.gt.0) then
        write(ITEMS(M+1),'(A,I2,A,I2,A)')
     & '0 Page --------------- Part: ',IPM,' of ',MPM,' ---------------'
      else
        ITEMS(M+1)=
     &           ' ----------------------------------------------------'
      endif
      ITEMS(M+2)='+ add/delete/copy input                              '
      ITEMS(M+3)='! list inputs                                        '
      ITEMS(M+4)='? help                                               '
      ITEMS(M+5)='- exit menu'

      NITEMS=M+MCTL
      CALL EMENU('FMU inputs',ITEMS,NITEMS,IPICK)

C Exit
      if (IPICK.eq.NITEMS) then
        goto 999

C Show help.
      elseif (IPICK.eq.NITEMS-1) then
        helptopic='FMU_input'
        CALL gethelptext(helpinsub,helptopic,nbhelp)
        CALL PHELPD('FMU inputs help',nbhelp,'-',0,0,IER)

C List data (more verbose than data shown in interface).
      elseif (IPICK.eq.NITEMS-2) then
        CALL FMI_LIST(IUOUT,IFMU,'I',IER)

C Manage inputs.
      elseif (IPICK.eq.NITEMS-3) then
        helptopic='manage_inputs'
        CALL gethelptext(helpinsub,helptopic,nbhelp)
        CALL EASKABCD(' ','Options:',
     &    'add','delete','copy','cancel',itmp,nbhelp)

C Add an input at end of list. First check that it doesn't exceed the
C maximum. Commons should already be initialised.
        if (itmp.eq.1) then
          if ((FMUNUMI(IFMU)+1).gt.MFMUI) then
            write(outs,'(a,i2,a)')
     &        ' Cannot add another input; maximum per FMU is ',MFMUI,'.'
            call USRMSG(outs,' ','-')
            goto 4
          endif
          FMUNUMI(IFMU)=FMUNUMI(IFMU)+1
          if (saved) saved=.false.

C If the menu is paged, store some variables so it will return to the
C same page when this menu is exited, and repopulate paging commons.
          if (IPFLG.gt.0) then
            pagstr(1)=IST
            pagstr(2)=IPM
            get_pagstr=.true.
          endif
          goto 6

C Delete an input. List inputs then ask user what number.
C Need to shift input common data for subsequent sets down by an input
C index, clear the last set, and decrement the number of inputs.
        elseif (itmp.eq.2) then
          if (FMUNUMI(IFMU).eq.0) then
            CALL USRMSG(' No inputs to delete!',' ','-')
            goto 4
          endif
          CALL FMI_LIST(IUOUT,IFMU,'I',IER)
          itmp=FMUNUMI(IFMU)
          CALL EASKI(itmp,' ',' Which number input to delete?',1,'F',
     &      FMUNUMI(IFMU),'F',FMUNUMI(IFMU),'input number',IER,nbhelp)
          if (itmp.lt.FMUNUMI(IFMU)) then
            do i=itmp+1,FMUNUMI(IFMU)
              FMUIZON(IFMU,i-1)=FMUIZON(IFMU,i)
              FMUIVAR(IFMU,i-1)=FMUIVAR(IFMU,i)
              do itmp=1,MFMUSUP
                FMUISUP(IFMU,i-1,itmp)=FMUISUP(IFMU,i,itmp)
              enddo
              FMUIINS(IFMU,i-1)=FMUIINS(IFMU,i)
              FMUIVNM(IFMU,i-1)=FMUIVNM(IFMU,i)
            enddo
          endif
          FMUIZON(IFMU,FMUNUMI(IFMU))=-1
          FMUIVAR(IFMU,FMUNUMI(IFMU))=0
          do itmp=1,MFMUSUP
            FMUISUP(IFMU,FMUNUMI(IFMU),itmp)=0.
          enddo
          FMUIINS(IFMU,FMUNUMI(IFMU))='UNKNOWN'
          FMUIVNM(IFMU,FMUNUMI(IFMU))='UNKNOWN'
          FMUNUMI(IFMU)=FMUNUMI(IFMU)-1
          if (saved) saved=.false.

C Copy an input. First check that it won't exceed maximum. List inputs
C then ask user what number.
C Need to increment number of inputs, then copy commons for selected one
C to the last (new) one.
        elseif (itmp.eq.3) then
          if ((FMUNUMI(IFMU)+1).gt.MFMUI) then
            write(outs,'(a,i2,a)')
     &        ' Cannot add another input; maximum per FMU is ',MFMUI,'.'
            call USRMSG(outs,' ','-')
            goto 4
          endif
          if (FMUNUMI(IFMU).eq.0) then
            CALL USRMSG(' No inputs to copy!',' ','-')
            goto 4
          endif
          CALL FMI_LIST(IUOUT,IFMU,'I',IER)
          itmp=FMUNUMI(IFMU)
          CALL EASKI(itmp,' ',' Which number input to copy?',1,'F',
     &      FMUNUMI(IFMU),'F',FMUNUMI(IFMU),'input number',IER,nbhelp)
          FMUNUMI(IFMU)=FMUNUMI(IFMU)+1
          FMUIZON(IFMU,FMUNUMI(IFMU))=FMUIZON(IFMU,itmp)
          FMUIVAR(IFMU,FMUNUMI(IFMU))=FMUIVAR(IFMU,itmp)
          do itmp2=1,MFMUSUP
            FMUISUP(IFMU,FMUNUMI(IFMU),itmp2)=FMUISUP(IFMU,itmp,itmp2)
          enddo
          FMUIINS(IFMU,FMUNUMI(IFMU))=FMUIINS(IFMU,itmp)
          FMUIVNM(IFMU,FMUNUMI(IFMU))=FMUIVNM(IFMU,itmp)
          if (saved) saved=.false.

C If the menu is paged, store some variables so it will return to the
C same page when this menu is exited, and repopulate paging commons.
          if (IPFLG.gt.0) then
            pagstr(1)=IST
            pagstr(2)=IPM
            get_pagstr=.true.
          endif
          goto 6
        endif

C Only active if there is paging.
      elseif (IPICK.eq.NITEMS-4) then
        if (IPFLG.gt.0) then
          IPACT=EDIT
          ILEN=FMUNUMI(IFMU)
          CALL EKPAGE(IPACT)
        endif

C Edit details for an input.
      elseif (IPICK.gt.MHEAD.and.IPICK.le.(NITEMS-MCTL)) then
        CALL KEYIND(NITEMS,IPICK,IFOC,IO)
        IINP=IFOC
    5   write(ITEMS(1),'(a,i1)')' FMU number: ',IFMU
        write(ITEMS(2),'(2a)')  ' description: ',FMUDSC(IFMU)
        write(ITEMS(3),'(a,i2)')' input number: ',(IINP)
        ITEMS(4)=' ----------------------------------------------------'
        itmp=FMUIZON(IFMU,IINP)
        if (itmp.eq.0) then
          ctmp_30='ambient environment'
        elseif (itmp.gt.0.and.itmp.le.NCOMP) then
          ctmp_30=zname(itmp)
        else
          ctmp_30='UNKNOWN'
        endif
        write(ITEMS(5),'(a,i2,3a)')'a zone: ',FMUIZON(IFMU,IINP),' (',
     &    ctmp_30(1:lnblnk(ctmp_30)),')'
        itmp=FMUIVAR(IFMU,IINP)
        if (itmp.gt.0.and.itmp.le.MFMIIREFS) then
          ctmp_30=FMIIREFS(FMUIVAR(IFMU,IINP))
        else
          ctmp_30='UNKNOWN'
        endif
        write(ITEMS(6),'(2a)')'b ESP-r variable: ',
     &    ctmp_30(1:lnblnk(ctmp_30))
        if (FMUIVAR(IFMU,IINP).gt.0) then
          if (FMIINSUP(FMUIVAR(IFMU,IINP)).gt.0) then
            write(ITEMS(7),'(a)')'c Supplementary data:'
            do itmp=1,FMIINSUP(FMUIVAR(IFMU,IINP))
              outs=ITEMS(7)
              write(ITEMS(7),'(a,1x,f4.2)')outs(1:lnblnk(outs)),
     &          FMUISUP(IFMU,IINP,itmp)
            enddo
          else
            write(ITEMS(7),'(a)')
     &        '  No supplementary data required for this variable type'
          endif
        else
          write(ITEMS(7),'(a)')'  Variable type not defined'
        endif
        write(ITEMS(8),'(2a)')'d FMU instance: ',FMUIINS(IFMU,IINP)
        write(ITEMS(9),'(2a)')'e FMU variable: ',FMUIVNM(IFMU,IINP)
        ITEMS(10)=' ---------------------------------------------------'
        ITEMS(11)= '? help'
        ITEMS(12)='- exit menu'

        NITEMS=12
        CALL EMENU('Edit FMU input data',ITEMS,NITEMS,IPICK)

C Return.
        if (IPICK.eq.NITEMS) then
          if (get_pagstr) goto 6
          goto 4

C Show help.
        elseif (IPICK.eq.NITEMS-1) then
          helptopic='edit_input'
          CALL gethelptext(helpinsub,helptopic,nbhelp)
          CALL PHELPD('FMU input editing help',nbhelp,'-',0,0,IER)

C Pick a zone. First give an option to select the ambient environment.
        elseif (IPICK.eq.5) then
          CALL EASKABC(' Ambient environment or a building zone?',' ',
     &      'ambient','building zone','cancel',itmp,nbhelp)
          if (itmp.eq.1) then
            FMUIZON(IFMU,IINP)=0
          elseif (itmp.eq.2) then
            pagstr(1)=IST
            pagstr(2)=IPM
            get_pagstr=.true.
            CALL EASKGEOF('Pick a zone.',CFGOK,itmp,'-',34,IER)
            FMUIZON(IFMU,IINP)=itmp
          elseif (itmp.eq.3) then
            goto 5
          endif
          if (saved) saved=.false.

C Based on the zone choice, ask user to pick from FMI input variables.
C If zone has not yet been defined, warn user and do no allow pick.
        elseif (IPICK.eq.6) then
          itmp=FMUIZON(IFMU,IINP)
          if (itmp.lt.0) then
            CALL USRMSG(' Please define zone before choosing variable.',
     &        ' ','-')

C Pick from ambient FMI input variables.
C Note: this may need to be a multi-page menu in the future.
          elseif (itmp.eq.0) then
            itmp=0
            do i=1,MFMIIREFS
              if (FMIIAorZ(i).eq.0.or.FMIIAorZ(i).eq.2) then
                itmp=itmp+1
                arrtmp(itmp)=i  ! remember indices
                CALL EMKEY(itmp,KEY,IER)
                write(ITEMS(itmp),'(a1,1x,a)')KEY,
     &            FMIIREFS(i)(1:lnblnk(FMIIREFS(i)))
              endif
            enddo
            ITEMS(itmp+1)=' -----------------------------'
            ITEMS(itmp+2)='- exit menu'
            IPICK=-4
            NITEMS=itmp+2
            CALL EMENU('Pick a variable',ITEMS,NITEMS,IPICK)
            if (IPICK.gt.0.and.IPICK.lt.NITEMS-1) then
              FMUIVAR(IFMU,IINP)=arrtmp(IPICK)
              if (saved) saved=.false.
            endif

C Pick from zone FMI input variables.
C Note: this may need to be a multi-page menu in the future.
          elseif (itmp.gt.0) then
            itmp=0
            do i=1,MFMIIREFS
              if (FMIIAorZ(i).eq.1.or.FMIIAorZ(i).eq.2) then
                itmp=itmp+1
                arrtmp(itmp)=i  ! remember indices
                CALL EMKEY(itmp,KEY,IER)
                write(ITEMS(itmp),'(a1,1x,a)')KEY,
     &            FMIIREFS(i)(1:lnblnk(FMIIREFS(i)))
              endif
            enddo
            ITEMS(itmp+1)=' -----------------------------'
            ITEMS(itmp+2)='- exit menu'
            IPICK=-4
            NITEMS=itmp+2
            CALL EMENU('Pick a variable',ITEMS,NITEMS,IPICK)
            if (IPICK.gt.0.and.IPICK.lt.NITEMS-1) then
              FMUIVAR(IFMU,IINP)=arrtmp(IPICK)
              if (saved) saved=.false.
            endif
          endif

C Define supplementary data.
        elseif (IPICK.eq.7) then
          if (FMUIVAR(IFMU,IINP).gt.0) then
            if (FMIINSUP(FMUIVAR(IFMU,IINP)).gt.0) then
              ctmp_30=''
              do itmp=1,FMIINSUP(FMUIVAR(IFMU,IINP))
                outs=ctmp_30
                write(ctmp_30,'(a,1x,f4.2)')outs(1:lnblnk(outs)),
     &            FMUISUP(IFMU,IINP,itmp)
              enddo
              helptopic='sup_data'
              CALL gethelptext(helpinsub,helptopic,nbhelp)
              CALL EASKS(ctmp_30,' ',' Supplementary data?',30,'  ',
     &          'FMU sup data',IER,nbhelp)
              if (ctmp_30(1:2).ne.'  ') then
                K=0
                do itmp=1,FMIINSUP(FMUIVAR(IFMU,IINP))
                  CALL EGETWR(
     &              ctmp_30,K,rtmp,0.,0.,'-','FMU sup data',IER)
                  FMUISUP(IFMU,IINP,itmp)=rtmp
                enddo              
                if (saved) saved=.false.
              endif             
            endif
          endif

C Define FMU instance name.
        elseif (IPICK.eq.8) then
          ctmp_30=FMUIINS(IFMU,IINP)
          CALL EASKS(ctmp_30,' ',' FMU instance name?',30,'  ',
     &      'FMU instance',IER,nbhelp)
          if (ctmp_30(1:2).ne.'  ') then
            FMUIINS(IFMU,IINP)=ctmp_30
            if (saved) saved=.false.
          endif

C Define FMU variable name.
        elseif (IPICK.eq.9) then
          ctmp_32=FMUIVNM(IFMU,IINP)
          CALL EASKS(ctmp_32,' ',' FMU variable name?',32,'  ',
     &      'FMU variable',IER,nbhelp)
          if (ctmp_32(1:2).ne.'  ') then
            FMUIVNM(IFMU,IINP)=ctmp_32
            if (saved) saved=.false.
          endif
        endif

        goto 5
      endif

      goto 4

  999 RETURN

      END



C ******************** FMI_EDOUT
C FMI_EDOUT controls the definition and editing of FMU outputs. This
C creates directives for data to be passed from the FMU to ESP-r.

C TODO: Maybe adapt to allow linking of outputs to specific control
C       loops as well as zones.

      SUBROUTINE FMI_EDOUT(IFMU,saved,IER)

#include "building.h"
#include "FMI.h"
#include "epara.h"
#include "geometry.h"
#include "help.h"

      logical saved

      common/FOPENED/CFGOK,MLDBOK,MATDBOK,CTLOK,OPTKOK,CFCDBOK
      logical CFGOK,MLDBOK,MATDBOK,CTLOK,OPTKOK,CFCDBOK
      COMMON/C1/NCOMP,NCON
      INTEGER NCOMP,NCON
      COMMON/OUTIN/IUOUT,IUIN,IEOUT

      DIMENSION ITEMS(50)
      CHARACTER ITEMS*56

      DIMENSION arrtmp(10)
      INTEGER arrtmp

      CHARACTER KEY*1,ctmp_30*30,ctmp_32*32,ctmp1_56*56,ctmp2_56*56,
     &          outs*124
      INTEGER M,i,itmp,NITEMS,pagstr(2)
      logical get_pagstr

      helpinsub='FMIprj'

      get_pagstr=.false.

C Set up multi-page menu.
  6   MHEAD=6
      MCTL=5
      ILEN=FMUNUMO(IFMU)
      IPACT=CREATE
      CALL EKPAGE(IPACT)
      if (get_pagstr) then
        ist=pagstr(1)
        ipm=pagstr(2)
        get_pagstr=.false.
      endif
  4   IPICK=-4
      IER=0

C Header.
      write(ITEMS(1),'(a,i1)')' FMU number: ',IFMU
      write(ITEMS(2),'(2a)')  ' description: ',FMUDSC(IFMU)
      write(ITEMS(3),'(a,i3)')' Number of outputs: ',FMUNUMO(IFMU)
      ITEMS(4)=  ' ----------------------------------------------------'
      ITEMS(5)=  '     |     ESP-r     |      FMU      |      FMU      '
      ITEMS(6)=  ' zone| variable name | instance name | variable name '
      M=MHEAD

C Paginated part.
      if (FMUNUMO(IFMU).gt.0) then
        do i=1,FMUNUMO(IFMU)
          if (i.ge.IST.and.i.le.(IST+MIFULL)) then
            M=M+1
            CALL EMKEY(i,KEY,IER)
            write(ctmp1_56,'(A1,1X,I2,2X)')KEY,FMUOZON(IFMU,i)
            itmp=FMUOVAR(IFMU,i)
            if (itmp.gt.0.and.itmp.le.MFMIOREFS) then
              ctmp_30=FMIOREFS(itmp)
            else
              ctmp_30='UNKNOWN'
            endif
            itmp=lnblnk(ctmp_30)
            if (itmp.gt.15) then
              write(ctmp2_56,'(3A,1X)')ctmp1_56(1:6),ctmp_30(1:12),'...'
            else
              write(ctmp2_56,'(2A,1X)')ctmp1_56(1:6),ctmp_30(1:15)
            endif
            ctmp_30=FMUOINS(IFMU,i)
            itmp=lnblnk(ctmp_30)
            if (itmp.gt.15) then
              write(ctmp1_56,'(3A,1X)')ctmp2_56(1:22),ctmp_30(1:12),
     &          '...'
            else
              write(ctmp1_56,'(2A,1X)')ctmp2_56(1:22),ctmp_30(1:15)
            endif
            ctmp_30=FMUOVNM(IFMU,i)
            itmp=lnblnk(ctmp_30)
            if (itmp.gt.15) then
              write(ctmp2_56,'(3A)')ctmp1_56(1:38),ctmp_30(1:12),'...'
            else
              write(ctmp2_56,'(2A)')ctmp1_56(1:38),ctmp_30(1:15)
            endif
            ITEMS(M)=ctmp2_56
          endif
        enddo
      endif

C Control options.
      if (IPFLG.gt.0) then
        write(ITEMS(M+1),'(A,I2,A,I2,A)')
     & '0 Page --------------- Part: ',IPM,' of ',MPM,' ---------------'
      else
        ITEMS(M+1)=
     &           ' ----------------------------------------------------'
      endif
      ITEMS(M+2)='+ add/delete/copy output                             '
      ITEMS(M+3)='! list outputs                                       '
      ITEMS(M+4)='? help                                               '
      ITEMS(M+5)='- exit menu'

      NITEMS=M+MCTL
      CALL EMENU('FMU outputs',ITEMS,NITEMS,IPICK)

C Exit
      if (IPICK.eq.NITEMS) then
        goto 999

C Show help.
      elseif (IPICK.eq.NITEMS-1) then
        helptopic='FMU_output'
        CALL gethelptext(helpinsub,helptopic,nbhelp)
        CALL PHELPD('FMU outputs help',nbhelp,'-',0,0,IER)

C List data (more verbose than data shown in interface).
      elseif (IPICK.eq.NITEMS-2) then
        CALL FMI_LIST(IUOUT,IFMU,'O',IER)

C Manage outputs.
      elseif (IPICK.eq.NITEMS-3) then
        helptopic='manage_outputs'
        CALL gethelptext(helpinsub,helptopic,nbhelp)
        CALL EASKABCD(' ','Options:',
     &    'add','delete','copy','cancel',itmp,nbhelp)

C Add an output at end of list. First check that it doesn't exceed the
C maximum. Commons should already be initialised.
        if (itmp.eq.1) then
          if ((FMUNUMO(IFMU)+1).gt.MFMUO) then
            write(outs,'(a,i2,a)')
     &       ' Cannot add another output; maximum per FMU is ',MFMUO,'.'
            call USRMSG(outs,' ','-')
            goto 4
          endif
          FMUNUMO(IFMU)=FMUNUMO(IFMU)+1
          if (saved) saved=.false.

C If the menu is paged, store some variables so it will return to the
C same page when this menu is exited, and repopulate paging commons.
          if (IPFLG.gt.0) then
            pagstr(1)=IST
            pagstr(2)=IPM
            get_pagstr=.true.
          endif
          goto 6

C Delete an output. List outputs then ask user what number.
C Need to shift output common data for subsequent sets down by an output
C index, clear the last set, and decrement the number of outputs.
        elseif (itmp.eq.2) then
          if (FMUNUMO(IFMU).eq.0) then
            CALL USRMSG(' No outputs to delete!',' ','-')
            goto 4
          endif
          CALL FMI_LIST(IUOUT,IFMU,'O',IER)
          itmp=FMUNUMO(IFMU)
          CALL EASKI(itmp,' ',' Which number output to delete?',1,'F',
     &      FMUNUMO(IFMU),'F',FMUNUMO(IFMU),'output number',IER,nbhelp)
          if (itmp.lt.FMUNUMO(IFMU)) then
            do i=itmp+1,FMUNUMO(IFMU)
              FMUOZON(IFMU,i-1)=FMUOZON(IFMU,i)
              FMUOVAR(IFMU,i-1)=FMUOVAR(IFMU,i)
              do itmp=1,MFMUSUP
                FMUOSUP(IFMU,i-1,itmp)=FMUOSUP(IFMU,i,itmp)
              enddo
              FMUOINS(IFMU,i-1)=FMUOINS(IFMU,i)
              FMUOVNM(IFMU,i-1)=FMUOVNM(IFMU,i)
            enddo
          endif
          FMUOZON(IFMU,FMUNUMO(IFMU))=-1
          FMUOVAR(IFMU,FMUNUMO(IFMU))=0
          do itmp=1,MFMUSUP
            FMUOSUP(IFMU,FMUNUMO(IFMU),itmp)=0.
          enddo
          FMUOINS(IFMU,FMUNUMO(IFMU))='UNKNOWN'
          FMUOVNM(IFMU,FMUNUMO(IFMU))='UNKNOWN'
          FMUNUMO(IFMU)=FMUNUMO(IFMU)-1
          if (saved) saved=.false.

C Copy an output. First check that it won't exceed maximum. List outputs
C then ask user what number.
C Need to increment number of outputs, then copy commons for selected one
C to the last (new) one.
        elseif (itmp.eq.3) then
          if ((FMUNUMO(IFMU)+1).gt.MFMUO) then
            write(outs,'(a,i2,a)')
     &       ' Cannot add another output; maximum per FMU is ',MFMUO,'.'
            call USRMSG(outs,' ','-')
            goto 4
          endif
          if (FMUNUMO(IFMU).eq.0) then
            CALL USRMSG(' No outputs to copy!',' ','-')
            goto 4
          endif
          CALL FMI_LIST(IUOUT,IFMU,'O',IER)
          itmp=FMUNUMO(IFMU)
          CALL EASKI(itmp,' ',' Which number output to copy?',1,'F',
     &      FMUNUMO(IFMU),'F',FMUNUMO(IFMU),'output number',IER,nbhelp)
          FMUNUMO(IFMU)=FMUNUMO(IFMU)+1
          FMUOZON(IFMU,FMUNUMO(IFMU))=FMUOZON(IFMU,itmp)
          FMUOVAR(IFMU,FMUNUMO(IFMU))=FMUOVAR(IFMU,itmp)
          do itmp2=1,MFMUSUP
            FMUOSUP(IFMU,FMUNUMO(IFMU),itmp2)=FMUOSUP(IFMU,itmp,itmp2)
          enddo
          FMUOINS(IFMU,FMUNUMO(IFMU))=FMUOINS(IFMU,itmp)
          FMUOVNM(IFMU,FMUNUMO(IFMU))=FMUOVNM(IFMU,itmp)
          if (saved) saved=.false.

C If the menu is paged, store some variables so it will return to the
C same page when this menu is exited, and repopulate paging commons.
          if (IPFLG.gt.0) then
            pagstr(1)=IST
            pagstr(2)=IPM
            get_pagstr=.true.
          endif
          goto 6
        endif

C Only active if there is paging.
      elseif (IPICK.eq.NITEMS-4) then
        if (IPFLG.gt.0) then
          IPACT=EDIT
          ILEN=FMUNUMO(IFMU)
          CALL EKPAGE(IPACT)
        endif

C Edit details for an output.
      elseif (IPICK.gt.MHEAD.and.IPICK.le.(NITEMS-MCTL)) then
        IOUT=IPICK-MHEAD
    5   write(ITEMS(1),'(a,i1)')' FMU number: ',IFMU
        write(ITEMS(2),'(2a)')  ' description: ',FMUDSC(IFMU)
        write(ITEMS(3),'(a,i2)')' output number: ',(IOUT)
        ITEMS(4)=' ----------------------------------------------------'
        itmp=FMUOZON(IFMU,IOUT)
        if (itmp.eq.0) then
          ctmp_30='ambient environment'
        elseif (itmp.gt.0.and.itmp.le.NCOMP) then
          ctmp_30=zname(itmp)
        else
          ctmp_30='UNKNOWN'
        endif
        write(ITEMS(5),'(a,i2,3a)')'a zone: ',FMUOZON(IFMU,IOUT),' (',
     &    ctmp_30(1:lnblnk(ctmp_30)),')'
        itmp=FMUOVAR(IFMU,IOUT)
        if (itmp.gt.0.and.itmp.le.MFMIOREFS) then
          ctmp_30=FMIOREFS(FMUOVAR(IFMU,IOUT))
        else
          ctmp_30='UNKNOWN'
        endif
        write(ITEMS(6),'(2a)')'b ESP-r variable: ',
     &    ctmp_30(1:lnblnk(ctmp_30))
        if (FMUOVAR(IFMU,IOUT).gt.0) then
          if (FMIONSUP(FMUOVAR(IFMU,IOUT)).gt.0) then
            write(ITEMS(7),'(a)')'c Supplementary data:'
            do itmp=1,FMIONSUP(FMUOVAR(IFMU,IOUT))
              outs=ITEMS(7)
              write(ITEMS(7),'(a,1x,f4.2)')outs(1:lnblnk(outs)),
     &          FMUOSUP(IFMU,IOUT,itmp)
            enddo
          else
            write(ITEMS(7),'(a)')
     &        '  No supplementary data required for this variable type'
          endif
        else
          write(ITEMS(7),'(a)')'  Variable type not defined'
        endif
        write(ITEMS(8),'(2a)')'d FMU instance: ',FMUOINS(IFMU,IOUT)
        write(ITEMS(9),'(2a)')'e FMU variable: ',FMUOVNM(IFMU,IOUT)
        ITEMS(10)=' ---------------------------------------------------'
        ITEMS(11)= '? help'
        ITEMS(12)='- exit menu'

        NITEMS=12
        CALL EMENU('Edit FMU output data',ITEMS,NITEMS,IPICK)

C Return.
        if (IPICK.eq.NITEMS) then
          if (get_pagstr) goto 6
          goto 4

C Show help.
        elseif (IPICK.eq.NITEMS-1) then
          helptopic='edit_output'
          CALL gethelptext(helpinsub,helptopic,nbhelp)
          CALL PHELPD('FMU output editing help',nbhelp,'-',0,0,IER)

C Pick a zone. First give an option to select the ambient environment.
        elseif (IPICK.eq.5) then
          CALL EASKABC(' Ambient environment or a building zone?',' ',
     &      'ambient','building zone','cancel',itmp,nbhelp)
          if (itmp.eq.1) then
            FMUOZON(IFMU,IOUT)=0
            if (saved) saved=.false.
          elseif (itmp.eq.2) then
C Store some variables to prevent zone pick corrupting paging on
C previous menu.
            pagstr(1)=IST
            pagstr(2)=IPM
            get_pagstr=.true.
            CALL EASKGEOF('Pick a zone.',CFGOK,itmp,'-',34,IER)
            FMUOZON(IFMU,IOUT)=itmp
            if (saved) saved=.false.
          elseif (itmp.eq.3) then
            goto 5
          endif
          if (saved) saved=.false.

C If the menu is paged, store some variables so it will return to the
C same page when this menu is exited, and repopulate paging commons.
          if (IPFLG.gt.0) then
            pagstr(1)=IST
            pagstr(2)=IPM
            get_pagstr=.true.
          endif

C Based on the zone choice, ask user to pick from FMI output variables.
C If zone has not yet been defined, warn user and do no allow pick.
        elseif (IPICK.eq.6) then
          itmp=FMUOZON(IFMU,IOUT)
          if (itmp.lt.0) then
            CALL USRMSG(' Please define zone before choosing variable.',
     &        ' ','-')

C Pick from ambient FMI output variables.
C Note: this may need to be a multi-page menu in the future.
          elseif (itmp.eq.0) then
            itmp=0
            do i=1,MFMIOREFS
              if (FMIOAorZ(i).eq.0.or.FMIOAorZ(i).eq.2) then
                itmp=itmp+1
                arrtmp(itmp)=i  ! remember indices
                CALL EMKEY(itmp,KEY,IER)
                write(ITEMS(itmp),'(a1,1x,a)')KEY,
     &            FMIOREFS(i)(1:lnblnk(FMIOREFS(i)))
              endif
            enddo
            ITEMS(itmp+1)=' -----------------------------'
            ITEMS(itmp+2)='- exit menu'
            IPICK=-4
            NITEMS=itmp+2
            CALL EMENU('Pick a variable',ITEMS,NITEMS,IPICK)
            if (IPICK.gt.0.and.IPICK.lt.NITEMS-1) then
              FMUOVAR(IFMU,IOUT)=arrtmp(IPICK)
              if (saved) saved=.false.
            endif

C Pick from zone FMI output variables.
C Note: this may need to be a multi-page menu in the future.
          elseif (itmp.gt.0) then
            itmp=0
            do i=1,MFMIOREFS
              if (FMIOAorZ(i).eq.1.or.FMIOAorZ(i).eq.2) then
                itmp=itmp+1
                arrtmp(itmp)=i  ! remember indices
                CALL EMKEY(itmp,KEY,IER)
                write(ITEMS(itmp),'(a1,1x,a)')KEY,
     &            FMIOREFS(i)(1:lnblnk(FMIOREFS(i)))
              endif
            enddo
            ITEMS(itmp+1)=' -----------------------------'
            ITEMS(itmp+2)='- exit menu'
            IPICK=-4
            NITEMS=itmp+2
            CALL EMENU('Pick a variable',ITEMS,NITEMS,IPICK)
            if (IPICK.gt.0.and.IPICK.lt.NITEMS-1) then
              FMUOVAR(IFMU,IOUT)=arrtmp(IPICK)
              if (saved) saved=.false.
            endif
          endif

C Define supplementary data.
        elseif (IPICK.eq.7) then
          if (FMUOVAR(IFMU,IOUT).gt.0) then
            if (FMIONSUP(FMUOVAR(IFMU,IOUT)).gt.0) then
              ctmp_30=''
              do itmp=1,FMIONSUP(FMUOVAR(IFMU,IOUT))
                outs=ctmp_30
                write(ctmp_30,'(a,1x,f4.2)')outs(1:lnblnk(outs)),
     &            FMUOSUP(IFMU,IOUT,itmp)
              enddo
              helptopic='sup_data'
              CALL gethelptext(helpinsub,helptopic,nbhelp)
              CALL EASKS(ctmp_30,' ',' Supplementary data?',30,'  ',
     &          'FMU sup data',IER,nbhelp)
              if (ctmp_30(1:2).ne.'  ') then
                K=0
                do itmp=1,FMIONSUP(FMUOVAR(IFMU,IOUT))
                  CALL EGETWR(
     &              ctmp_30,K,rtmp,0.,0.,'-','FMU sup data',IER)
                  FMUOSUP(IFMU,IOUT,itmp)=rtmp
                enddo              
                if (saved) saved=.false.
              endif
            endif
          endif

C Define FMU instance name.
        elseif (IPICK.eq.8) then
          ctmp_30=FMUOINS(IFMU,IOUT)
          CALL EASKS(ctmp_30,' ',' FMU instance name?',30,'  ',
     &      'FMU instance',IER,nbhelp)
          if (ctmp_30(1:2).ne.'  ') then
            FMUOINS(IFMU,IOUT)=ctmp_30
            if (saved) saved=.false.
          endif

C Define FMU variable name.
        elseif (IPICK.eq.9) then
          ctmp_32=FMUOVNM(IFMU,IOUT)
          CALL EASKS(ctmp_32,' ',' FMU variable name?',32,'  ',
     &      'FMU variable',IER,nbhelp)
          if (ctmp_32(1:2).ne.'  ') then
            FMUOVNM(IFMU,IOUT)=ctmp_32
            if (saved) saved=.false.
          endif
        endif

        goto 5
      endif

      goto 4

  999 RETURN

      END
