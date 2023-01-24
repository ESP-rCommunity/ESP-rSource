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

C FMIcom.F contains common fortran functions for the FMI
C implementation:
C FMI_RD       - Read FMI specification file.
C FMI_WT       - Write FMI specification file.
C FMI_CLEARCTL - Sets control commons such that FMI control will not be
C                applied.
C FMI_CLEARALL - Initialises all FMI commons to default values
C                (disables all FMI).
C FMI_CLEARFMU - Initialises all FMI commons to default values for a
C                single FMU.
C FMI_LIST     - List FMU data to specified output.
C FMI_CHK      - Check FMI definition in cfg file.

C ******************** FMI_RD
C FMI_RD reads an FMI specification file from file unit IUNIT.
C Data format is: *tag value

      SUBROUTINE FMI_RD(IUNIT,IER)

#include "building.h"
#include "FMI.h"

      common/OUTIN/IUOUT,IUIN,IEOUT
      COMMON/C1/NCOMP,NCON
      INTEGER NCOMP,NCON

      character*124 OUTSTR,outs,errstr,errval
      character WORD*50,longtfile*144
      integer DMY,K,IFMU,itmp,iinp,iout
      real VERSN,rtmp
      logical XST,concat

C Read header line to determine file version.
C VERSN currently not used, but in the future can be used to maintain
C back-compatibility. Original release file version is 2.0 (7/12/16).
    1 CALL STRIPC(IUNIT,OUTSTR,2,DMY,0,'FMU header',IER)
      if (IER.ne.0) then
        IER=3
        goto 666
      endif
      K=0
      CALL EGETW(OUTSTR,K,WORD,'F','FMU header',IER)
      if (IER.ne.0) then
        IER=3
        goto 666
      endif
      if (WORD(1:9).ne.'*FMI_SPEC') then
        IER=4
        errstr='"*FMI_SPEC"'
        errval=OUTSTR
        goto 666
      endif
      CALL EGETWR(OUTSTR,K,VERSN,0.,0.,'-','FMI_SPEC version',IER)
      if (IER.ne.0) then
        IER=3
        goto 666
      endif

C Read number of FMUs
      CALL STRIPC(IUNIT,OUTSTR,2,DMY,0,'Number of FMUs',IER)
      if (IER.ne.0) then
        IER=3
        goto 666
      endif
      K=0
      CALL EGETW(OUTSTR,K,WORD,'F','number of FMUs tag',IER)
      if (IER.ne.0) then
        IER=3
        goto 666
      endif
      if (WORD(1:9).ne.'*numFMUs') then
        IER=4
        errstr='"*numFMUs"'
        errval=OUTSTR
        goto 666
      endif
      CALL EGETWI(OUTSTR,K,itmp,0,MFMU,'F','number of FMUs value',IER)
      if (IER.ne.0) then
        IER=3
        goto 666
      endif
      FMUNOF=itmp

      if (FMUNOF.eq.0) goto 999
      do 10 IFMU=1,FMUNOF

C Read FMU file name.
        CALL STRIPC(IUNIT,OUTSTR,2,DMY,0,'FMU file',IER)
        if (IER.ne.0) then
          IER=3
          goto 666
        endif
2       K=0
        CALL EGETW(OUTSTR,K,WORD,'F','FMU file tag',IER)
        if (IER.ne.0) then
          IER=3
          goto 666
        endif
        if (WORD(1:9).ne.'*FileName') then
          IER=4
          errstr='"*FileName"'
          errval=WORD
          goto 666
        endif
        CALL EGETW(OUTSTR,K,WORD,'F','FMU file value',IER)
        if (IER.ne.0) then
          IER=3
          goto 666
        endif

C Adjust file name for location.
        call addpath(WORD,longtfile,concat)

C Check file exists.
        INQUIRE (FILE=longtfile,EXIST=XST)
        if (.not.XST) then
          write(outs,'(3a,i1,a)')
     &      ' File ',longtfile(1:lnblnk(longtfile)),
     &      ' not found for FMU ',IFMU,'.'
          CALL EDISP(iuout,outs)
        endif
        FMUFIL(IFMU)=WORD

C Read description.
        CALL STRIPC(IUNIT,OUTSTR,0,DMY,0,'FMU desc',IER)
        if (IER.ne.0) then
          IER=3
          goto 666
        endif
        K=0
        CALL EGETW(OUTSTR,K,WORD,'F','FMU desc tag',IER)
        if (IER.ne.0) then
          IER=3
          goto 666
        endif
        if (WORD(1:12).ne.'*Description') then
          IER=4
          errstr='"*Description"'
          errval=WORD
          goto 666
        endif
        write(FMUDSC(IFMU),'(a)')adjustl(trim(OUTSTR(K:(K+35))))

C Read timeout.
        CALL STRIPC(IUNIT,OUTSTR,2,DMY,0,'FMU timeout',IER)
        if (IER.ne.0) then
          IER=3
          goto 666
        endif
        K=0
        CALL EGETW(OUTSTR,K,WORD,'F','FMU timeout tag',IER)
        if (IER.ne.0) then
          IER=3
          goto 666
        endif
        if (WORD(1:8).ne.'*Timeout') then
          IER=4
          errstr='"*Timeout"'
          errval=WORD
          goto 666
        endif
        CALL EGETWR(OUTSTR,K,rtmp,0.0,0.0,'-','FMU timeout value',IER)
        if (IER.ne.0) then
          IER=3
          goto 666
        endif
        if (rtmp.lt.0.0) then
          IER=4
          errstr='real >0.0'
          write(errval,'(f6.1)')rtmp
          goto 666
        endif
        FMUTO(IFMU)=rtmp

C Read debug logging flag.
        CALL STRIPC(IUNIT,OUTSTR,2,DMY,0,'FMU logging',IER)
        if (IER.ne.0) then
          IER=3
          goto 666
        endif
        K=0
        CALL EGETW(OUTSTR,K,WORD,'F','FMU logging tag',IER)
        if (IER.ne.0) then
          IER=3
          goto 666
        endif
        if (WORD(1:8).ne.'*Logging') then
          IER=4
          errstr='"*Logging"'
          errval=WORD
          goto 666
        endif
        CALL EGETWI(OUTSTR,K,itmp,0,1,'F','FMU logging value',IER)
        if (IER.ne.0) then
          IER=3
          goto 666
        endif
        FMULOG(IFMU)=itmp

C Read number of FMU inputs.
        CALL STRIPC(IUNIT,OUTSTR,2,DMY,0,'FMU inputs',IER)
        if (IER.ne.0) then
          IER=3
          goto 666
        endif
        K=0
        CALL EGETW(OUTSTR,K,WORD,'F','FMU inputs tag',IER)
        if (IER.ne.0) then
          IER=3
          goto 666
        endif
        if (WORD(1:7).ne.'*Inputs') then
          IER=4
          errstr='"*Inputs"'
          errval=WORD
          goto 666
        endif
        CALL EGETWI(OUTSTR,K,itmp,0,MFMUI,'F','FMU inputs value',IER)
        if (IER.ne.0) then
          IER=3
          goto 666
        endif
        FMUNUMI(IFMU)=itmp

C *** INPUTS ***

C Read FMU input data in loop.
        DO 100 iinp=1,FMUNUMI(IFMU)

C Read zone index.
          CALL STRIPC(IUNIT,OUTSTR,2,DMY,0,'FMU input zone',IER)
          if (IER.ne.0) then
            IER=3
            goto 666
          endif
          K=0
          CALL EGETW(OUTSTR,K,WORD,'F','FMU input zone tag',IER)
          if (IER.ne.0) then
            IER=3
            goto 666
          endif
          if (WORD(1:5).ne.'*Zone') then
            IER=4
            errstr='"*Zone"'
            errval=WORD
            goto 666
          endif
          CALL EGETWI(OUTSTR,K,itmp,0,1,'-','FMU input zone value',IER)
          if (IER.ne.0) then
            IER=3
            goto 666
          endif
C Check if it is a valid zone index.
          if (itmp.lt.0.or.itmp.gt.NCOMP) then
            write(outs,'(a,i2,a,i1,a,i2,a)')' Invalid zone index "',
     &        itmp,'" for FMU ',IFMU,' input ',IINP,'.'
            CALL EDISP(iuout,outs)
          endif
          FMUIZON(IFMU,iinp)=itmp

C Read ESP-r variable required.
 3        CALL STRIPC(IUNIT,OUTSTR,2,DMY,0,'FMU input ESPvar',IER)
          if (IER.ne.0) then
            IER=3
            goto 666
          endif
          K=0
          CALL EGETW(OUTSTR,K,WORD,'F','FMU input ESPvar tag',IER)
          if (IER.ne.0) then
            IER=3
            goto 666
          endif
          if (WORD(1:7).ne.'*ESPvar') then
            IER=4
            errstr='"*ESPvar"'
            errval=WORD
            goto 666
          endif
          CALL EGETWI(OUTSTR,K,itmp,0,MFMIIREFS,'F',
     &      'FMU input ESPvar value',IER)
          if (IER.ne.0) then
            IER=3
            goto 666
          endif
          if (itmp.eq.0) then
            write(outs,'(a,i2,a,i1,a,i2,a)')
     &        ' Invalid ESP-r input variable index "',itmp,
     &        '" for FMU ',IFMU,' input ',IOUT,'.'
            CALL EDISP(iuout,outs)
          endif
          FMUIVAR(ifmu,iinp)=itmp

C Read supplementary data if required.
          if (itmp.gt.0) then
            if (FMIINSUP(itmp).gt.0) then
              do itmp2=1,FMIINSUP(itmp)
                CALL STRIPC(IUNIT,OUTSTR,2,DMY,0,'FMU input sup. data',
     &            IER)
                if (IER.ne.0) then
                  IER=3
                  goto 666
                endif
                K=0
                CALL EGETW(OUTSTR,K,WORD,'F','FMU input sup. data tag',
     &            IER)
                if (IER.ne.0) then
                  IER=3
                  goto 666
                endif
                if (WORD(1:7).ne.'*Data') then
                  IER=4
                  errstr='"*Data"'
                  errval=WORD
                  goto 666
                endif
                CALL EGETWR(OUTSTR,K,rtmp,0.,0.,'-',
     &            'FMU input sup. data value',IER)
                if (IER.ne.0) then
                  IER=3
                  goto 666
                endif
                FMUOSUP(ifmu,iinp,itmp2)=rtmp
              enddo
            endif
          endif

C Read FMU instance name.
          CALL STRIPC(IUNIT,OUTSTR,2,DMY,0,'FMU input instance',IER)
          if (IER.ne.0) then
            IER=3
            goto 666
          endif
          K=0
          CALL EGETW(OUTSTR,K,WORD,'F','FMU input instance tag',IER)
          if (IER.ne.0) then
            IER=3
            goto 666
          endif
          if (WORD(1:9).ne.'*Instance') then
            IER=4
            errstr='"*Instance"'
            errval=WORD
            goto 666
          endif
          CALL EGETW(OUTSTR,K,WORD,'F','FMU input instance value',IER)
          if (IER.ne.0) then
            IER=3
            goto 666
          endif
C Restriction on string length imposed in FMI.h.
          if (lnblnk(WORD).gt.30) then
            IER=4
            errstr='string <31 chars'
            errval=WORD
            goto 666
          endif
          FMUIINS(IFMU,iinp)=WORD(1:30)

C Read FMU variable name.
          CALL STRIPC(IUNIT,OUTSTR,2,DMY,0,'FMU input FMUvar',IER)
          if (IER.ne.0) then
            IER=3
            goto 666
          endif
          K=0
          CALL EGETW(OUTSTR,K,WORD,'F','FMU input FMUvar tag',IER)
          if (IER.ne.0) then
            IER=3
            goto 666
          endif
          if (WORD(1:7).ne.'*FMUvar') then
            IER=4
            errstr='"*FMUvar"'
            errval=WORD
            goto 666
          endif
          CALL EGETW(OUTSTR,K,WORD,'F','FMU input FMUvar value',IER)
          if (IER.ne.0) then
            IER=3
            goto 666
          endif
C See "modelDescription.xml" in an .fmu file for valid variable names.
C Restriction on string length imposed in FMI.h.
          if (lnblnk(WORD).gt.30) then
            IER=4
            errstr='string <31 chars'
            errval=WORD
            goto 666
          endif
          FMUIVNM(IFMU,iinp)=WORD(1:30)

 100    CONTINUE

C *** OUTPUTS ***

C Read number of FMU outputs.
        CALL STRIPC(IUNIT,OUTSTR,2,DMY,0,'FMU outputs',IER)
        if (IER.ne.0) then
          IER=3
          goto 666
        endif
        K=0
        CALL EGETW(OUTSTR,K,WORD,'F','FMU outputs tag',IER)
        if (IER.ne.0) then
          IER=3
          goto 666
        endif
        if (WORD(1:8).ne.'*Outputs') then
          IER=4
          errstr='"*Outputs"'
          errval=WORD
          goto 666
        endif
        CALL EGETWI(OUTSTR,K,itmp,0,MFMUO,'F','FMU outputs value',IER)
        if (IER.ne.0) then
          IER=3
          goto 666
        endif
        FMUNUMO(IFMU)=itmp

C Read FMU output data in loop.
        DO 200 iout=1,FMUNUMO(IFMU)

C Read zone index.
          CALL STRIPC(IUNIT,OUTSTR,2,DMY,0,'FMU output zone',IER)
          if (IER.ne.0) then
            IER=3
            goto 666
          endif
          K=0
          CALL EGETW(OUTSTR,K,WORD,'F','FMU output zone tag',IER)
          if (IER.ne.0) then
            IER=3
            goto 666
          endif
          if (WORD(1:5).ne.'*Zone') then
            IER=4
            errstr='"*Zone"'
            errval=WORD
            goto 666
          endif
          CALL EGETWI(OUTSTR,K,itmp,0,1,'-','FMU output zone value',IER)
          if (IER.ne.0) then
            IER=3
            goto 666
          endif
C Check if it is a valid zone index.
          if (itmp.lt.0.or.itmp.gt.NCOMP) then
            write(outs,'(a,i2,a,i1,a,i2,a)')' Invalid zone index "',
     &        itmp,'" for FMU ',IFMU,' output ',IOUT,'.'
            CALL EDISP(iuout,outs)
          endif
          FMUOZON(IFMU,iout)=itmp

C Read ESP-r variable required.
 4        CALL STRIPC(IUNIT,OUTSTR,2,DMY,0,'FMU output ESPvar',IER)
          if (IER.ne.0) then
            IER=3
            goto 666
          endif
          K=0
          CALL EGETW(OUTSTR,K,WORD,'F','FMU output ESPvar tag',IER)
          if (IER.ne.0) then
            IER=3
            goto 666
          endif
          if (WORD(1:7).ne.'*ESPvar') then
            IER=4
            errstr='"*ESPvar"'
            errval=WORD
            goto 666
          endif
          CALL EGETWI(OUTSTR,K,itmp,0,MFMIOREFS,'F',
     &      'FMU output ESPvar value',IER)
          if (IER.ne.0) then
            IER=3
            goto 666
          endif
          if (itmp.eq.0) then
            write(outs,'(a,i2,a,i1,a,i2,a)')
     &        ' Invalid ESP-r output variable index "',itmp,
     &        '" for FMU ',IFMU,' output ',IOUT,'.'
            CALL EDISP(iuout,outs)
          endif
          FMUOVAR(ifmu,iout)=itmp

C Read supplementary data if required.
          if (itmp.gt.0) then
            if (FMIONSUP(itmp).gt.0) then
              do itmp2=1,FMIONSUP(itmp)
                CALL STRIPC(IUNIT,OUTSTR,2,DMY,0,'FMU output sup. data',
     &            IER)
                if (IER.ne.0) then
                  IER=3
                  goto 666
                endif
                K=0
                CALL EGETW(OUTSTR,K,WORD,'F','FMU output sup. data tag',
     &            IER)
                if (IER.ne.0) then
                  IER=3
                  goto 666
                endif
                if (WORD(1:7).ne.'*Data') then
                  IER=4
                  errstr='"*Data"'
                  errval=WORD
                  goto 666
                endif
                CALL EGETWR(OUTSTR,K,rtmp,0.,0.,'-',
     &            'FMU output sup. data value',IER)
                if (IER.ne.0) then
                  IER=3
                  goto 666
                endif
                FMUOSUP(ifmu,iout,itmp2)=rtmp
              enddo
            endif
          endif  

C Read FMU instance name.
          CALL STRIPC(IUNIT,OUTSTR,2,DMY,0,'FMU output instance',IER)
          if (IER.ne.0) then
            IER=3
            goto 666
          endif
          K=0
          CALL EGETW(OUTSTR,K,WORD,'F','FMU output instance tag',IER)
          if (IER.ne.0) then
            IER=3
            goto 666
          endif
          if (WORD(1:9).ne.'*Instance') then
            IER=4
            errstr='"*Instance"'
            errval=WORD
            goto 666
          endif
          CALL EGETW(OUTSTR,K,WORD,'F','FMU output instance value',IER)
          if (IER.ne.0) then
            IER=3
            goto 666
          endif
C Restriction on string length imposed in FMI.h.
          if (lnblnk(WORD).gt.30) then
            IER=4
            errstr='string <31 chars'
            errval=WORD
            goto 666
          endif
          FMUOINS(IFMU,iout)=WORD(1:30)

C Read FMU variable name.
          CALL STRIPC(IUNIT,OUTSTR,2,DMY,0,'FMU output FMUvar',IER)
          if (IER.ne.0) then
            IER=3
            goto 666
          endif
          K=0
          CALL EGETW(OUTSTR,K,WORD,'F','FMU output FMUvar tag',IER)
          if (IER.ne.0) then
            IER=3
            goto 666
          endif
          if (WORD(1:7).ne.'*FMUvar') then
            IER=4
            errstr='"*FMUvar"'
            errval=WORD
            goto 666
          endif
          CALL EGETW(OUTSTR,K,WORD,'F','FMU output FMUvar value',IER)
          if (IER.ne.0) then
            IER=3
            goto 666
          endif
C See "modelDescription.xml" in an .fmu file for valid variable names.
C Restriction on string length imposed in FMI.h.
          if (lnblnk(WORD).gt.30) then
            IER=4
            errstr='string <31 chars'
            errval=WORD
            goto 666
          endif
          FMUOVNM(IFMU,iout)=WORD(1:30)

 200    CONTINUE

 10   CONTINUE

 999  RETURN

C Error handling.
C IER = 3 - Read error, errstr and errval unused
C IER = 4 - Unexpected file content, errstr = expected, errval = actual
 666  if (IER.eq.3) then
        outs='FMI_RD: Read error in FMI spec file.'
      elseif (IER.eq.4) then
        write(outs,'(5a)')
     &                'FMI_RD: Unexpected file content; expecting ',
     &                 errstr(1:lnblnk(errstr)),', got ',
     &                 errval(1:lnblnk(errval)),'.'
      else
        outs='FMI_RD: Unknown error.'
      endif

      call edisp(iuout,outs)
      goto 999

      END

C ******************** FMI_WT
C FMI_WT writes an FMI specification file to file unit IUNIT.
C Data format is: *tag value

      SUBROUTINE FMI_WT(IUNIT,IER)

#include "building.h"
#include "FMI.h"
#include "geometry.h"

      common/OUTIN/IUOUT,IUIN,IEOUT
      COMMON/C1/NCOMP,NCON
      INTEGER NCOMP,NCON

      character*124 outs,errstr,errval
      integer IFMU,iinp,iout,itmp
      real VERSN

C Open file.
      CALL EFOPSEQ(IUNIT,FMISFL,4,IER)
      if (IER.lt.0) goto 666

C *** HEADER ***
C Write file version and number of FMUs.
      VERSN=2.0
      write(IUNIT,'(a,f3.1)',ERR=666)'*FMI_SPEC ',VERSN
      write(IUNIT,'(a,i2)',ERR=666)'*numFMUs ',FMUNOF

C Blank line.
      write(IUNIT,'(a)',ERR=666)'#'

C *** GENERAL INFO ***
C For each FMU ...
      if (FMUNOF.eq.0) goto 999
      do 10 IFMU=1,FMUNOF

C Write label, FMU file name, , description, timeout and debug logging
C flag.
        write(IUNIT,'(a,i2,a)',ERR=666)'# -- FMU ',IFMU,' --'
        write(IUNIT,'(2a)',ERR=666)'*FileName ',FMUFIL(IFMU)
        write(IUNIT,'(2a)',ERR=666)'*Description ',FMUDSC(IFMU)
        write(IUNIT,'(a,f6.1)',ERR=666)'*Timeout ',FMUTO(IFMU)
        write(IUNIT,'(a,i1)',ERR=666)'*Logging ',FMULOG(IFMU)

C Blank line.
        write(IUNIT,'(a)',ERR=666)'#'

C *** INPUTS ***
C Write label and number of FMU inputs.
        write(IUNIT,'(a)',ERR=666)'# - INPUTS -'
        write(IUNIT,'(a,i3)',ERR=666)'*Inputs ',FMUNUMI(IFMU)
        write(IUNIT,'(a)',ERR=666)'#'

C For each input ...
        DO 100 IINP=1,FMUNUMI(IFMU)

C Write zone index and name.
          itmp=FMUIZON(IFMU,IINP)
          write(outs,'(a,i2,a)',ERR=666)'*Zone ',itmp,'  # '
          if (itmp.eq.0) then
            write(IUNIT,'(2a)',ERR=666)outs(1:12),'ambient environment'
          elseif (itmp.gt.0.and.itmp.le.NCOMP) then
            write(IUNIT,'(2a)',ERR=666)outs(1:12),
     &        zname(itmp)(1:lnzname(itmp))
          else
            write(IUNIT,'(2a)',ERR=666)outs(1:12),'UNKNOWN'
          endif

C Write ESP-r variable index and descriptive string.
          itmp=FMUIVAR(IFMU,IINP)
          write(outs,'(a,i2,a)',ERR=666)'*ESPvar ',itmp,'  # '
          if (itmp.gt.0.and.itmp.le.MFMIIREFS) then
            write(IUNIT,'(2a)',ERR=666)outs(1:14),
     &        FMIIREFS(itmp)(1:lnblnk(FMIIREFS(itmp)))
          else
            write(IUNIT,'(2a)',ERR=666)outs(1:14),'UNKNOWN'
          endif

C Write supplementary data, if needed.
          if (itmp.gt.0) then
            if (FMIINSUP(itmp).gt.0) then
              do itmp2=1,FMIINSUP(itmp)
                write(IUNIT,'(a,f5.2,a)')'*Data ',
     &            FMUISUP(IFMU,IINP,itmp2),'  # Supplementary data'
              enddo
            endif
          endif          

C Write FMU instance and variable names.
          write(IUNIT,'(2a)',ERR=666)'*Instance ',
     &      FMUIINS(IFMU,IINP)(1:lnblnk(FMUIINS(IFMU,IINP)))
          write(IUNIT,'(2a)',ERR=666)'*FMUvar ',
     &      FMUIVNM(IFMU,IINP)(1:lnblnk(FMUIVNM(IFMU,IINP)))

C Blank line.
          write(IUNIT,'(a)',ERR=666)'#'

 100    CONTINUE

        if (FMUNUMI(IFMU).eq.0) write(IUNIT,'(a)')'#'

C *** OUTPUTS ***
C Write label and number of FMU outputs.
        write(IUNIT,'(a)',ERR=666)'# - OUTPUTS -'
        write(IUNIT,'(a,i3)',ERR=666)'*Outputs ',FMUNUMO(IFMU)
        write(IUNIT,'(a)',ERR=666)'#'

C For each output ...
        DO 200 IOUT=1,FMUNUMO(IFMU)

C Write zone index and name.
          itmp=FMUOZON(IFMU,IOUT)
          write(outs,'(a,i2,a)',ERR=666)'*Zone ',itmp,'  # '
          if (itmp.eq.0) then
            write(IUNIT,'(2a)',ERR=666)outs(1:12),'ambient environment'
          elseif (itmp.gt.0.and.itmp.le.NCOMP) then
            write(IUNIT,'(2a)',ERR=666)outs(1:12),
     &        zname(itmp)(1:lnzname(itmp))
          else
            write(IUNIT,'(2a)',ERR=666)outs(1:12),'UNKNOWN'
          endif

C Write ESP-r variable index and descriptive string.
          itmp=FMUOVAR(IFMU,IOUT)
          write(outs,'(a,i2,a)',ERR=666)'*ESPvar ',itmp,'  # '
          if (itmp.gt.0.and.itmp.le.MFMIOREFS) then
            write(IUNIT,'(2a)',ERR=666)outs(1:14),
     &        FMIOREFS(itmp)(1:lnblnk(FMIOREFS(itmp)))
          else
            write(IUNIT,'(2a)',ERR=666)outs(1:14),'UNKNOWN'
          endif

C Write supplementary data, if needed.
          if (itmp.gt.0) then
            if (FMIONSUP(itmp).gt.0) then
              do itmp2=1,FMIONSUP(itmp)
                write(IUNIT,'(a,f5.2,a)')'*Data ',
     &            FMUOSUP(IFMU,IOUT,itmp2),'  # Supplementary data'
              enddo
            endif
          endif 

C Write FMU instance and variable names.
          write(IUNIT,'(2a)',ERR=666)'*Instance ',
     &      FMUOINS(IFMU,IOUT)(1:lnblnk(FMUOINS(IFMU,IOUT)))
          write(IUNIT,'(2a)',ERR=666)'*FMUvar ',
     &      FMUOVNM(IFMU,IOUT)(1:lnblnk(FMUOVNM(IFMU,IOUT)))

C Blank line.
          write(IUNIT,'(a)')'#'

 200    CONTINUE

        if (FMUNUMO(IFMU).eq.0) write(IUNIT,'(a)')'#'

 10   CONTINUE

 999  CALL ERPFREE(IUNIT,IER)
      RETURN

C Error handling.
 666  IER=1
      outs='FMI_WT: Error writing FMI spec file.'
      CALL EDISP(iuout,outs)
      goto 999

      END

C ******************** FMI_CLEARCTL
C FMI_CLEARCTL sets control commons such that FMI control will not be
C applied.
      SUBROUTINE FMI_CLEARCTL()

#include "building.h"
#include "FMI.h"

      do itmp=1,MCOM
        do itmp2=1,MFMIOREFS
          FMUDOCTL(itmp,itmp2)=.false.
        enddo
      enddo

      RETURN
      END

C ******************** FMI_CLEARALL
C FMI_CLEARALL initialises all FMI commons to default values.
      SUBROUTINE FMI_CLEARALL()

#include "building.h"
#include "FMI.h"

      IS_FMU=.false.
      FMISFL='UNKNOWN'
      FMUNOF=0

      do itmp=1,MFMU
        FMUTSA(itmp)=0.
        FMUTSO(itmp)=0.
        FMUTSE(itmp)=0.
        FMUTCU(itmp)=0.

        FMUTO(itmp)=0.
        FMULOG(itmp)=0
        FMUFIL(itmp)='UNKNOWN'
        FMUNIS(itmp)=0
        do itmp2=1,MFMUIS
          FMUINS(itmp,itmp2)='UNKNOWN'
        enddo
        FMUDSC(itmp)='This FMU is ...'

        FMUNUMI(itmp)=0
        do itmp2=1,MFMUI
          FMUIZON(itmp,itmp2)=-1
          FMUIVAR(itmp,itmp2)=0
          do itmp3=1,MFMUSUP
            FMUISUP(itmp,itmp2,itmp3)=0.
          enddo
          FMUIINS(itmp,itmp2)='UNKNOWN'
          FMUIVNM(itmp,itmp2)='UNKNOWN'
          FMUIRFN(itmp,itmp2)=0
        enddo

        FMUNUMO(itmp)=0
        do itmp2=1,MFMUO
          FMUOZON(itmp,itmp2)=-1
          FMUOVAR(itmp,itmp2)=0
          do itmp3=1,MFMUSUP
            FMUOSUP(itmp,itmp2,itmp3)=0.
          enddo
          FMUOINS(itmp,itmp2)='UNKNOWN'
          FMUOVNM(itmp,itmp2)='UNKNOWN'
          FMUORFN(itmp,itmp2)=0
        enddo
      enddo

      do itmp=1,MCOM
        do itmp2=1,MFMIOREFS
          FMUCTL(itmp,itmp2)=0.
          FMUDOCTL(itmp,itmp2)=.false.
          do itmp3=1,MFMUSUP
            FMUCTLSUP(itmp,itmp2,itmp3)=0.
          enddo
        enddo
      enddo

      RETURN
      END

C ******************** FMI_CLEARFMU
C FMI_CLEARFMU initialises all FMI commons to default values for a
C single FMU. Assumes commons are already populated correctly.
      SUBROUTINE FMI_CLEARFMU(IFMU)

#include "building.h"
#include "FMI.h"

      FMUTSA(IFMU)=0.
      FMUTSO(IFMU)=0.
      FMUTSE(IFMU)=0.
      FMUTCU(IFMU)=0.

      FMUTO(IFMU)=0.
      FMULOG(IFMU)=0
      FMUFIL(IFMU)='UNKNOWN'
      if (FMUNIS(IFMU).gt.0) then
        do itmp=1,FMUNIS(IFMU)
          FMUINS(IFMU,itmp)='UNKNOWN'
        enddo
        FMUNIS(IFMU)=0
      endif
      FMUDSC(IFMU)='This FMU is ...'

      if (FMUNUMI(IFMU).gt.0) then
        do itmp=1,FMUNUMI(IFMU)
          FMUIZON(IFMU,itmp)=-1
          if (FMUIVAR(IFMU,itmp).gt.0) then
            if (FMIINSUP(FMUIVAR(IFMU,itmp)).gt.0) then
              do itmp2=1,FMIINSUP(FMUIVAR(IFMU,itmp))
                FMUISUP(IFMU,itmp,itmp2)=0.
              enddo
            endif
          endif
          FMUIVAR(IFMU,itmp)=0
          FMUIINS(IFMU,itmp)='UNKNOWN'
          FMUIVNM(IFMU,itmp)='UNKNOWN'
          FMUIRFN(IFMU,itmp)=0
        enddo
        FMUNUMI(IFMU)=0
      endif

      if (FMUNUMO(IFMU).gt.0) then
        do itmp=1,FMUNUMO(IFMU)
          FMUOZON(IFMU,itmp)=-1
          if (FMUOVAR(IFMU,itmp).gt.0) then
            if (FMIONSUP(FMUOVAR(IFMU,itmp)).gt.0) then
              do itmp2=1,FMIONSUP(FMUOVAR(IFMU,itmp))
                FMUOSUP(IFMU,itmp,itmp2)=0.
              enddo
            endif
          endif
          FMUOVAR(IFMU,itmp)=0
          FMUOINS(IFMU,itmp)='UNKNOWN'
          FMUOVNM(IFMU,itmp)='UNKNOWN'
          FMUORFN(IFMU,itmp)=0
        enddo
        FMUNUMO(IFMU)=0
      endif

      RETURN
      END

C ******************** FMI_LIST
C FMI_LIST displays verbose FMU specification data for FMU number IFMU
C on output IUNIT.
C MOD: 'I'=input data, 'O'=output data, 'A'=all data
      SUBROUTINE FMI_LIST(IUNIT,IFMU,MOD,IER)

#include "building.h"
#include "FMI.h"
#include "geometry.h"

      INTEGER IUNIT,IFMU
      CHARACTER MOD*1

      CHARACTER*124 outs,outs2
      INTEGER len,itmp

      COMMON/C1/NCOMP,NCON
      INTEGER NCOMP,NCON

C Check MOD.
      if (MOD.ne.'I'.and.MOD.ne.'O'.and.MOD.ne.'A') then
        IER=3
        goto 666
      endif

C Blank line.
      CALL EDISP(IUNIT,' ')

C List general data.
      if (MOD.eq.'A') then
        write(outs,'(a,i1,a)')'*** General data for FMU ',IFMU,' ***'
        CALL EDISP(IUNIT,outs)

        len=lnblnk(FMUFIL(IFMU))
        write(outs,'(2a)')' FMU file: ',FMUFIL(IFMU)(1:len)
        CALL EDISP(IUNIT,outs)

        write(outs,'(2a)')' Description: ',FMUDSC

        write(outs,'(a,f6.1,a)')' Timeout: ',FMUTO(IFMU),' ms'
        CALL EDISP(IUNIT,outs)

        itmp=FMULOG(IFMU)
        if (itmp.eq.0) then
          write(outs,'(a)')' Debug logging: off'
        else
          write(outs,'(a)')' Debug logging: on'
        endif
        CALL EDISP(IUNIT,outs)

        CALL EDISP(IUNIT,' ')
      endif

C List inputs.
      if (MOD.eq.'A'.or.MOD.eq.'I') then
        write(outs,'(a,i1,a)')'*** List of inputs for FMU ',IFMU,' ***'
        CALL EDISP(IUNIT,outs)

        write(outs,'(a,i2)')' Number of inputs: ',FMUNUMI(IFMU)
        CALL EDISP(IUNIT,outs)

        if (FMUNUMI(IFMU).gt.0) then
          write(outs,'(2a)')
     &    ' |input|zone| zone name  |  ESP-r variable description  |  ',
     &  '                            |                                |'
          CALL EDISP(IUNIT,outs)
          write(outs,'(2a)')
     &    ' | no. |no. | or ambient |    and supplementary data    |  ',
     &  '    FMU instance name       |       FMU variable name        |'
          CALL EDISP(IUNIT,outs)

          do itmp=1,FMUNUMI(IFMU)
            write(outs,'(2X,I3,3X,I2,3X)')itmp,FMUIZON(IFMU,itmp)
            if (FMUIZON(IFMU,itmp).gt.0.and.
     &        FMUIZON(IFMU,itmp).lt.NCOMP) then
              write(outs2,'(A13,A12,1X)')outs,zname(FMUIZON(IFMU,itmp))
            elseif (FMUIZON(IFMU,itmp).eq.0) then
              write(outs2,'(A13,A12,1X)')outs,'ambient     '
            else
              write(outs2,'(A13,A12,1X)')outs,'UNKNOWN     '
            endif
            if (FMUIVAR(IFMU,itmp).gt.0.and.
     &        FMUIVAR(IFMU,itmp).le.MFMIIREFS) then
              write(outs,'(A26,A30,1X)')outs2,
     &          FMIIREFS(FMUIVAR(IFMU,itmp))
            else
              write(outs,'(A26,A30,1X)')outs2,
     &          'UNKNOWN                       '
            endif
            write(outs2,'(A57,A30,1X,A32)')outs,
     &        FMUIINS(IFMU,itmp),FMUIVNM(IFMU,itmp)
            CALL EDISP(IUNIT,outs2)
            if (FMUIVAR(IFMU,itmp).gt.0) then
              if (FMIINSUP(FMUIVAR(IFMU,itmp)).gt.0) then
                outs='                         x'
                do itmp2=1,FMIINSUP(FMUIVAR(IFMU,itmp))
                  outs2=outs
                  write(outs,'(a,1x,f4.2)')outs2(1:lnblnk(outs2)-1),
     &              FMUISUP(IFMU,itmp,itmp2)
                enddo
                CALL EDISP(IUNIT,outs)
              endif
            endif
          enddo
        endif

        CALL EDISP(IUNIT,' ')
      endif

C List outputs.
      if (MOD.eq.'A'.or.MOD.eq.'O') then
        write(outs,'(a,i1,a)')'*** List of outputs for FMU ',IFMU,' ***'
        CALL EDISP(IUNIT,outs)

        write(outs,'(a,i2)')' Number of outputs: ',FMUNUMO(IFMU)
        CALL EDISP(IUNIT,outs)

        if (FMUNUMO(IFMU).gt.0) then
          write(outs,'(2a)')
     &    ' |output|zone| zone name  |  ESP-r variable description  | ',
     & '                             |                                |'
          CALL EDISP(IUNIT,outs)
          write(outs,'(2a)')
     &    ' | no.  |no. | or ambient |    and supplementary data    | ',
     & '     FMU instance name       |       FMU variable name        |'
          CALL EDISP(IUNIT,outs)

          do itmp=1,FMUNUMO(IFMU)
            write(outs,'(2X,I3,4X,I2,3X)')itmp,FMUOZON(IFMU,itmp)
            if (FMUOZON(IFMU,itmp).gt.0.and.
     &        FMUOZON(IFMU,itmp).lt.NCOMP) then
              write(outs2,'(A14,A12,1X)')outs,zname(FMUOZON(IFMU,itmp))
            elseif (FMUOZON(IFMU,itmp).eq.0) then
              write(outs2,'(A14,A12,1X)')outs,'ambient     '
            else
              write(outs2,'(A14,A12,1X)')outs,'UNKNOWN     '
            endif
            if (FMUOVAR(IFMU,itmp).gt.0.and.
     &        FMUOVAR(IFMU,itmp).le.MFMIOREFS) then
              write(outs,'(A27,A30,1X)')outs2,
     &          FMIOREFS(FMUOVAR(IFMU,itmp))
            else
              write(outs,'(A27,A30,1X)')outs2,
     &          'UNKNOWN                       '
            endif
            write(outs2,'(A58,A30,1X,A32)')outs,
     &        FMUOINS(IFMU,itmp),FMUOVNM(IFMU,itmp)
            CALL EDISP(IUNIT,outs2)
            if (FMUOVAR(IFMU,itmp).gt.0) then
              if (FMIONSUP(FMUOVAR(IFMU,itmp)).gt.0) then
                outs='                          x'
                do itmp2=1,FMIONSUP(FMUOVAR(IFMU,itmp))
                  outs2=outs
                  write(outs,'(a,1x,f4.2)')outs2(1:lnblnk(outs2)-1),
     &              FMUOSUP(IFMU,itmp,itmp2)
                enddo
                CALL EDISP(IUNIT,outs)
              endif
            endif
          enddo
        endif

        CALL EDISP(IUNIT,' ')
      endif

 999  RETURN

C Error handling.
C IER = 3: Invalid MOD
 666  if (IER.eq.3) then
        write(outs,'(3a)')'FMI_LIST: Invalid value of MOD "',MOD,'".'
      endif
      goto 999

      END

C ******************** FMI_CHK ********************
C Check for FMUs and set commons accordingly.
C Called after we have detected the tag "*FMI" in the cfg file.
C LINE is the line from the cfg file which should have the path to the
C FMI spec file at position K.
C IUF is a temporary file unit.

      subroutine FMI_CHK(LINE,K,IUF,IER)

#include "building.h"
#include "FMI.h"

      character(*) LINE
      character outstr*124,word*20

C Readfile name.
      CALL EGETW(LINE,K,FMISFL,'W','FMI spec file',IER)
      if (IER.ne.0) goto 999

C Open file.
      CALL ERPFREE(IUF,IER)
      CALL EFOPSEQ(IUF,FMISFL,1,IER)
      if (IER.ne.0) goto 999

C Read first 2 lines.
      CALL STRIPC(IUF,outstr,0,ND,0,'FMI spec file 1st line',IER)
      CALL STRIPC(IUF,outstr,0,ND,0,'FMI spec file 2nd line',IER)
      if (IER.ne.0) goto 999

C Get number of FMUs.
      K=0
      CALL EGETW(outstr,K,word,'W','*numFMUs',IER)
      if (word(1:8).ne.'*numFMUs') goto 999
      CALL EGETWI(outstr,K,FMUNOF,0,MFMU,'W','number of FMUs',IER)
      if (IER.ne.0) goto 999

      if (FMUNOF.gt.0) IS_FMU=.true.
      CALL ERPFREE(IUF,IER)

  999 RETURN

      end
