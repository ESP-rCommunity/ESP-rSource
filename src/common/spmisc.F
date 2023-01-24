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

C ***************** SPMINIT
C This subroutine reads information for special building materials from the user 
C defined file SPMFIL. Data is read into the special material common blocks.
C 
C Details of common block variables for special materials:
C nspmnod  - number of user designated special materials node.
C ispmloc  - array holding location of material 1-zone, 2-surface, 3 -node.
C ispmtype - array holding details of special material type
C           1-type number      2-opaque/transparent flag.
C spmdat   - array holding miscellaneous data for special materials subroutines.
C spmlabel - text label describing special materal (max 16 chars).
C spmflg   - flag indicating whether special materials are active (0 or 1).
      subroutine SPMINIT

#include "building.h"
#include "help.h"
      COMMON/OUTIN/IUOUT,IUIN,IEOUT

C Special materials file flag.
      common/spmfxst/ispmxist,spflnam 
 
      common/spmatl/nspmnod,ispmloc(mspmnod,3),ispmtyp(mspmnod,2),
     &nnodat(mspmnod),spmdat(mspmnod,mspmdat) 
      common/spmatlbl/spmlabel(mspmnod)

C Path to problem and command line file (if any).
      common/rpath/path
      common/filep/ifil

C DSC data / spline function data
      common/pcmspline/temp1d(mspmnod,MSPMSPLM),
     &          heat1d(mspmnod,MSPMSPLM),
     &          cool1d(mspmnod,MSPMSPLM),heat2d(mspmnod,MSPMSPLM),
     &          cool2d(mspmnod,MSPMSPLM),nxy(mspmnod),
     &          bUseSpline(mspmnod),fnamDSCdat(mspmnod)
      real temp1d   ! DSC temperature data values, degC
      real heat1d   ! DSC heating enthalpy values, J/(kg K)
      real cool1d   ! DSC cooling enthalpy values, J/(kg K)
      real heat2d   ! 2nd derivatives at spline x values
      real cool2d
      integer nxy             ! number of data sets
      logical bUseSpline      ! .true. means DSC data file available, use
                              ! cubic spline function for apparent specific
                              ! heat function.
      character fnamDSCdat*72 ! DSC data file name for current spm entry

      real hold_data(mspmdat)

      character outs*124,outstr*124,spmlabel*16
      character*72 path,spflnam,ltmp*72

      helpinsub='spmisc'  ! set for subroutine
      helptopic='spmisc_file_contents'
      call gethelptext(helpinsub,helptopic,nbhelp)

C Set the number of special material data items per line
      nspmdatl=10

C Open special materials file on temporary file channel.
      ispmfil=ifil+1
      call erpfree(ispmfil,istat)
      call edisp(iuout,' ')
      write(outs,'(a,a)') ' Path is: ',path
      call edisp(iuout,outs)
      ltmp=spflnam
      if(ltmp(1:2).eq.'  '.or.ltmp(1:4).eq.'UNKN')then
      call easks(ltmp,' Special Materials file ?',
     &' ',72,' ','special materials file',IER,nbhelp)
      endif
      spflnam=ltmp
      if(IER.NE.0) then
        write(outs,'(a,a)') 'Could not find file ',spflnam
        call edisp(iuout,outs)      
        ispmxist=0
        return
      endif
      call efopseq(ispmfil,spflnam,1,IER)
      if(IER.NE.0) then
        write(outs,'(a,a)') 'Error opening file ',spflnam
        call edisp(iuout,outs)
        ispmxist=0
        return
      endif

      call edisp(iuout,'Reading special materials file ...')
      call edisp(iuout,spflnam) 

C Read data from opened special materials file to the common blocks.
C Get the number of entries in the file. 
      call stripc(ispmfil,outstr,1,ND,0,
     &  'special materials file error-1..',IER)
      if(IER.ne.0) goto 999
      k=0
      call egetwi(outstr,K,nspmnod,1,mspmnod,'W',
     &  'number spmatl node',IER)
      if(IER.ne.0) goto 999

C For each entry read the associated special materials data.
      do 10 I=1,nspmnod

C Firstly the special material label (first 16 characters).
        call stripc(ispmfil,outstr,1,ND,0,
     &    'special materials file error-2..',IER)
        if(IER.ne.0) goto 999
        spmlabel(I)=outstr(1:16)

C Next read the material info.
        call stripc(ispmfil,outstr,5,ND,0,
     &    'special materials file error-3..',IER)
        if(IER.ne.0) goto 999
        k=0
        call egetwi(outstr,K,ivar1,1,mcom,'W','-',IER)
        ispmloc(I,1)=ivar1
        call egetwi(outstr,K,ivar2,1,ms,'W','-',IER)
        ispmloc(I,2)=ivar2   
        call egetwi(outstr,K,ivar3,1,0,'-','-',IER)
        ispmloc(I,3)=ivar3
        call egetwi(outstr,K,ivar4,1,99,'-','-',IER)
        ispmtyp(I,1)=ivar4
        call egetwi(outstr,K,ivar5,0,1,'W','-',IER)
        ispmtyp(I,2)=ivar5
        if(IER.ne.0) goto 999

C Read the material miscellaneous data.
        call stripc(ispmfil,outstr,1,ND,0,
     &    'special materials file error-4..',IER)
        if(IER.ne.0) goto 999
C Read the number of data items for this entry
        K=0
        call egetwi(outstr,K,nspmdat,1,mspmdat,'W','-',IER)
        nnodat(I)=nspmdat
        if(IER.ne.0) goto 999

C Recover misellaneous data:
        call EGetWRA( ispmfil, hold_data, nspmdat,0.,0., '-',
     &      'special materials file error', IER )

          do j = 1,nspmdat

            spmdat(I,J) = hold_data(j)

          enddo

C Read optional DSC data file name for SPMCMP56 ...
        if (ispmtyp(I,1).eq.56) then
          if (nspmdat.eq.9) then
            if (spmdat(I,9).gt.1) then
C             We have SPMCMP56, only 9 data items and item 9 is not 0 or 1 ...
C             should mean DSC data file available
              call stripc(ispmfil,outstr,1,ND,0,' DSC file name',IER)
              L=0
              call EGETW(outstr,L,fnamDSCdat(I),'W',
     &                                      ' get DSC file name',IER)
              if (IER.eq.0) bUseSpline(I)=.true.
C DEBUG
c              write(*,*)'** found DSC file ',fnamDSCdat(I)
            endif
          endif
        endif

        if(IER.ne.0) goto 999

  10  continue

  99  close(ispmfil)
      return

 999  write(outs,'(a)')'An error has occurred while reading the '
      call edisp(iuout,outs)
      write(outs,'(a)')'special materials file - check file format!'
      call edisp(iuout,outs)
      ispmxist=0
      goto 99

      end

C ********* SPMWRIT
C Subroutine SPMWRIT writes out a special materials file if the
C flag ispmxist is greater than 0 i.e. when special materials have been
C defined.

      subroutine SPMWRIT
#include "building.h"
#include "model.h"
#include "help.h"

      integer lnblnk

      COMMON/OUTIN/IUOUT,IUIN,IEOUT

      common/spmatl/nspmnod,ispmloc(mspmnod,3),ispmtyp(mspmnod,2),
     &nnodat(mspmnod),spmdat(mspmnod,mspmdat) 
      common/spmatlbl/spmlabel(mspmnod)

      common/FILEP/IFIL

C Special materials file flag
      common/spmfxst/ispmxist,spflnam  

C DSC data / spline function data
      common/pcmspline/temp1d(mspmnod,MSPMSPLM),
     &          heat1d(mspmnod,MSPMSPLM),
     &          cool1d(mspmnod,MSPMSPLM),heat2d(mspmnod,MSPMSPLM),
     &          cool2d(mspmnod,MSPMSPLM),nxy(mspmnod),
     &          bUseSpline(mspmnod),fnamDSCdat(mspmnod)
      real temp1d   ! DSC temperature data values, degC
      real heat1d   ! DSC heating enthalpy values, J/(kg K)
      real cool1d   ! DSC cooling enthalpy values, J/(kg K)
      real heat2d   ! 2nd derivatives at spline x values
      real cool2d
      integer nxy             ! number of data sets
      logical bUseSpline      ! .true. means DSC data file available, use
                              ! cubic spline function for apparent specific
                              ! heat function.
      character fnamDSCdat*72 ! DSC data file name for current spm entry

      character outs*124,deflnam*72,spflnam*72
      character spmlabel*16,lfil*72
      logical ok

      integer linecount

C Set the number of special material data items per line
      nspmdatl=10
      helpinsub='spmisc'

C Determine whether a special materials file is to be created
      if(ispmxist.eq.1) then
        helptopic='spmisc_changes'
        call gethelptext(helpinsub,helptopic,nbhelp)
        call easkok(' ','Update special materials file?',ok,nbhelp)
        if(.not.ok) return
      endif
      deflnam='data.spm'
      lfil=spflnam
      helptopic='spmisc_info'
      call gethelptext(helpinsub,helptopic,nbhelp)
 4    call easks(lfil,' ','Special materials filename?',72,
     &  deflnam,' SPM filename',IER,2)
      if(lfil.ne.' ')spflnam=lfil
      if(IER.GT.0) return

C Open spm file with filename 'spflnam' unit number IFIL+1.
      ispmfil=ifil+1
      call efopseq(ispmfil,spflnam,4,IER)
      if(IER.GT.0) then
        write(outs,'(a,a)') 'an error has occurred opening ',spflnam
        call edisp(iuout,outs)
        goto 4
      endif

C Write header label for file
      write(ispmfil,'(a)')'# ESP-r special materials file used in '
      write(ispmfil,'(a,a)')'# configuration file ',
     &  LCFGF(1:lnblnk(LCFGF))

C Write out nodal information
      write(ispmfil,'(i4,a)') nspmnod,
     &'  # Number of special material nodes.'
      do 10 isp=1,nspmnod
        write(ispmfil,'(a,i4)') '# Node No:',isp
        write(ispmfil,'(a,a)') spmlabel(isp),' # label'
        write(ispmfil,'(a)')'# Zone Surf Node Type Opq/Trn'
        write(ispmfil,'(2x,i2,1x,i3,2x,i2,2x,i2,2x,i2)')
     &  ispmloc(isp,1),ispmloc(isp,2),ispmloc(isp,3),
     &ispmtyp(isp,1),ispmtyp(isp,2)
 
C Write node specific data. 
C << TODO expand feedback to use text in the mscomp file. >>
C << there is a pattern in edspmtl.F near line 530. >>
        write(ispmfil,'(a)')'# Number of data items.'
        write(ispmfil,'(2x,i2)') nnodat(isp)
        write(ispmfil,'(a)')'# Data:'
        if(ispmtyp(isp,1).eq.1)then
          write(ispmfil,'(a)')
     &    '# Open circuit voltage (V), Short circuit current. (I)'
          write(ispmfil,'(a)')
     &    '# Short circuit current (I) Voltage at maximum pwr pt (V)'
          write(ispmfil,'(a)')
     &  '# Current at maximum pwr pt (A), Reference insolation (W/m^2)'
          write(ispmfil,'(a)')
     & '# Reference temperature (K), Number series connected cells (-)'
          write(ispmfil,'(2a)')
     &    '# Number of parallel connected branches (-), ',
     &    'Number of panels in surface. (-)'
          write(ispmfil,'(2a)')
     &    '# Empirical value used in calculation of Io, ',
     &    'Load type (0-maximum power,1-fixed R,2-fixed V)'
          write(ispmfil,'(2a)')
     &    '# Load value - resistance or voltage (-), ',
     &    'Shading treatment (0-def,1-prop.,2-total,3-diff.)'
        endif
        istrt=1
        iend=nspmdatl
        linecount=1

 7      continue
        if(nnodat(isp).le.nspmdatl)then
C         Only one line of data neccessary
          write(ispmfil,'(6F10.4,F16.6,3F10.4)')(spmdat(isp,idat),
     &    idat=1,nnodat(isp))
          if (ispmtyp(isp,1).eq.56) then
            if (nnodat(isp).eq.9) then
C             For ispmtype.eq.56, 9 data items only and 9th item .gt.1
C             insert DSC data file option ...
C             nnodat(isp) should be 9, so only this "if" should be reached
              if (spmdat(isp,9).gt.1) then
                write(ispmfil,'(2a)')' ',
     &            fnamDSCdat(isp)(1:lnblnk(fnamDSCdat(isp)))
              endif
            endif
          endif
        else
C         More than one line of data is written
          if (ispmtyp(isp,1).eq.56) then
C           Cater for more detailed output due to latent heat function
C           very sensitive to parameter precision
            if (linecount.eq.1) then
C             1st line:
              write(ispmfil,'(7F10.4,F16.6,F10.4,E17.8)')
     &                           (spmdat(isp,idat),idat=istrt,iend)
              istrt=istrt+nspmdatl
              iend=iend+nspmdatl
              linecount=linecount+1
            else
C             2nd line (type 56 has only three):
              write(ispmfil,'(10E17.8)')(spmdat(isp,idat),
     &                                              idat=istrt,iend)
              istrt=istrt+nspmdatl
              iend=iend+nspmdatl
            endif

            if(iend.gt.nnodat(isp))then
C             Last data line
              iend=nnodat(isp)
              write(ispmfil,'(10E17.8)')(spmdat(isp,idat),
     &                                      idat=istrt,nnodat(isp))
            else
              goto 7
            endif
          else
            write(ispmfil,'(6F10.4,F16.6,3F10.4)')(spmdat(isp,idat),
     &      idat=istrt,iend)
            istrt=istrt+nspmdatl
            iend=iend+nspmdatl
            if(iend.gt.nnodat(isp))then
              iend=nnodat(isp)
              write(ispmfil,'(6F10.4,F16.6,3F10.4)')(spmdat(isp,idat),
     &        idat=istrt,nnodat(isp))
            else
              goto 7
            endif
          endif ! Type 56
        endif
 10   continue

C Set spm file flag to 'saved' 
      ispmxist=1

C Close the special materials file.
      call erpfree(ispmfil,istat)
      return
      end
