C This file is part of the ESP-r system.
C Copyright Sustainable Building Energy Systems Laboratory, Carleton University.
C Please contact Ian Beausoleil-Morrison for details concerning licensing.

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

C **********************************************************************
C SUBROUTINE: BASESIMP_INPUTS
C Author: Briana Paige Kemery, Carleton University, Ottawa.
C Function: This subroutine adds functionality to ESP-r project manager
C     to allow the user to create a basesimp input file.
C **********************************************************************
      SUBROUTINE BASESIMP_INPUTS(ICOMP,IER)

      IMPLICIT NONE

#include "building.h"
#include "model.h"
#include "help.h"

C Declare calling parameters 
      INTEGER, INTENT(IN)  :: ICOMP     
      INTEGER, INTENT(INOUT)  :: IER

      INTEGER LNBLNK   ! function definition
      
      COMMON/bsmtdef/iBSIMP(MCOM),LBSIMP(MCOM)
      COMMON/OUTIN/IUOUT,IUIN,IEOUT
      INTEGER iuout,iuin,ieout

C COMMONS FOR BASEREAD
      COMMON/BSdata/bsHEIG(MCOM),bsDEPT(MCOM),bsLENG(MCOM),bsWIDT(MCOM),
     &              bsOVER(MCOM),bsRSI(MCOM),bsSOIL(MCOM),bsWTD(MCOM)
      COMMON/BSvn/bsVrsnNmbr(MCOM)
      COMMON/BSwth/bsTGavg(MCOM),bsTGamp(MCOM),bsTGps(MCOM)

      CHARACTER*72 bsmFile
      CHARACTER*27 strHeight,strDepth,strLength,strWidth,strOverlap,
     &        strRSI,strSoil,strWater,strTGavg,strTGamp,strTGps
C      CHARACTER :: delimiter
      CHARACTER*72 sfile,snpfile,LBSIMP

      INTEGER iBSIMP ! iBSIMP flag to talk to cfg
      INTEGER x,iier,iwhich,nfile
      integer iln  ! length of file
      integer iuo  ! file unit for writing bsm file
      integer ios  ! file write status

      LOGICAL exists
      logical zonehasone   ! set to true if one is already defined
      logical unixok
      character fs*1       ! file separator
      character doit*124   ! for use in creating ../bsm folder
      character outs*124

      INTEGER :: fBASESIMPVersionNumber ! version number of BASESIMP file
      REAL bsmtHeight,bsmtDepth ! Foundation height & depth in meters
      REAL bsmtLength,bsmtWidth ! Foundation length & width in meters.
      REAL Insul_Overlap ! Insul overlap for combination configurations.
      REAL Insul_rsi ! Insulation resistance in RSI.
      REAL Soil_Cond ! Soil conductivity (W/m K)
      REAL Water_Table ! Water-table depth in meters.
      REAL TGavg ! Annual-averaged soil temperature.
      REAL TGamp ! Amplitude of ground-temperature's annual sine wave.
      REAL TGps ! Phase lag of ground-temperature's annual sine wave.


      REAL min_bsmtHeight,max_bsmtHeight,min_bsmtDepth,max_bsmtDepth
      REAL min_bsmtLength,max_bsmtLength,min_bsmtWidth,max_bsmtWidth
      REAL max_Insul_Overlap,min_Insul_Overlap
      REAL max_Insul_rsi,min_Insul_rsi
      REAL max_Soil_Cond,min_Soil_Cond
      REAL max_Water_Table,min_Water_Table
      REAL max_TGavg,min_TGavg,max_TGamp,min_TGamp,max_TGps,min_TGps

      REAL bsHEIG,bsDEPT,bsLENG,bsWIDT,
     &             bsOVER,bsRSI,bsSOIL, bsWTD
      REAL bsVrsnNmbr
      REAL bsTGavg,bsTGamp,bsTGps
      integer ISTRW

      helpinsub='basesimp_inputs'  ! set for subroutine
       
C Set folder separator (fs) to \ or / as required.
      call isunix(unixok)
      if(unixok)then
        fs = char(47)
      else
        fs = char(92)
      endif

C Check that the zone is known.
      if(ICOMP.eq.0)then
        ier=1
        return
      endif

C INITIALISE SELECT VARIABLES
C Declares initial default values for basesimp inputs.
      bsmtHeight = 2.5          ! meter
      bsmtDepth = 2.0           ! meter
      bsmtLength = 12.0         ! meter
      bsmtWidth = 8.0           ! meter
      Insul_Overlap = 0.        ! meter
      Insul_rsi = 2.72          ! m2K/W
      Soil_Cond = 0.85          ! W/mK
      Water_Table = 8.          ! meter
      TGavg = 0.1               ! degC
      TGamp = 0.1               ! degC
      TGps = 0.1                ! radians 

C Basesimp files go in the zones folder.
C Check if the zone bsm file already exists.
      call FINDFIL(LBSIMP(ICOMP),exists)
      IF(exists)THEN
        CALL EDISP(IUOUT,
     &    'The zone includes an existing BASESIMP file:')
        CALL EDISP(IUOUT,LBSIMP(ICOMP))
        zonehasone = .true.
      ELSE
        INQUIRE(FILE=zonepth,EXIST=exists)
        if(exists)then   ! the zones folder exists
          continue
        else

C Use the pattern in folders.F to issue mkdir command.
          write(doit,'(2a)') 'mkdir ',zonepth
          call usrmsg('Creating model folder:',doit,'-')
          call runit(doit,'-')
          zonehasone = .false.
        endif
      END IF ! the exists test

C HELP CONTENT for initial selection from the user and
C many of the initial dialogs.
      helptopic='basesimp_opening'
      call gethelptext(helpinsub,helptopic,nbhelp)

C USER INPUTS
C Upon exiting the SURFACE ATTRIBUTES the user is prompted for inputs
C BASESIMP is being used by one or more surfaces in the zone, the user
C must decide whether to create/edit the .BSM file now or later.
      if(zonehasone)then
        CALL EASKABCD('Edit existing *.bsm?',' ','yes','no ',
     &    'browse for another bsm file','dereference basesimp',
     &    IWHICH,nbhelp)
        if(iwhich.eq.4)then
          call usrmsg('Dereferencing the bsm file...',
     &      ' ','-')
          iBSIMP(ICOMP)=0
          LBSIMP(ICOMP)=' '
          return
        endif
      else
        CALL EASKABCD('Create new *.bsm?',' ','yes','no ',
     &    'browse for another bsm file','cancel',IWHICH,nbhelp)
        if(iwhich.eq.4)then
          call usrmsg('Continuing without creating bsm file...',
     &      ' ','-')
          return
        endif
      endif


C If using existing file.
      if(IWHICH.EQ.1)then
        if(zonehasone)then

C If user requested to edit the current bsm file then read its
C contents and then allow editing.
          x=1   ! signal to just read it
        else

C User requested a new file be created.
          x=0
        endif
      elseif(IWHICH.EQ.2)then
        if(zonehasone)then
          call usrmsg('Continuing without editing bsm file...',
     &      ' ','-')
          return
        else
          call usrmsg('Continuing without creating bsm file...',
     &      ' ','-')
          return
        endif
      elseif(IWHICH.EQ.3)then
        sfile=' '
        CALL browsefilelist('?','bsm','fil',sfile,snpfile,
     &    nfile,iier)
        IF(nfile.gt.0)THEN
          sfile=' '
          snpfile=' '
          CALL BROWSEFILELIST('b','bsm','fil',sfile,snpfile,
     &      nfile,iier)
          IF(snpfile(1:2).ne.'  ')THEN
            iBSIMP(ICOMP) = 1
            iln=lnblnk(snpfile)
            if(iln.lt.65)then
              write(LBSIMP(ICOMP),'(5a)') '..',fs,'bsm',fs,
     &          snpfile(1:iln)
            else
              write(LBSIMP(ICOMP),'(5a)') '..',fs,'bsm',fs,
     &          snpfile(1:64)
            endif
            x=1
          ELSE
            CALL EDISP(IUOUT,
     &        'A blank bsm file name returned. Clearing data.')
            CALL EDISP(IUOUT,'You have not used an existing BSM  ')
            CALL EDISP(IUOUT,'file. You must define a new one.   ')
            iBSIMP(ICOMP)=0
            LBSIMP(ICOMP)=' '
            return
          END IF

        ELSE
          CALL USRMSG('You have not selected an existing BSM   ',
     &                'file. You must define a new one.   ','p')
          RETURN
        END IF
      ELSEIF(IWHICH.EQ.4)THEN
        CALL EDISP(IUOUT,'Cancel request for edit or new so')
        CALL EDISP(IUOUT,'continuing...')
        RETURN
      END IF

C IF LOOP TO SKIP THE INPUT OF VARIABLES IF AN EXISTING FILE IS READ IN
      IF(x.EQ.1)THEN
        CALL BASEREAD(ICOMP,IER)

        bsmtHeight = bsHEIG(ICOMP)
        bsmtDepth = bsDEPT(ICOMP)
        bsmtLength = bsLENG(ICOMP)
        bsmtWidth = bsWIDT(ICOMP)
        Insul_Overlap = bsOVER(ICOMP)
        Insul_rsi = bsRSI(ICOMP)
        Soil_Cond = bsSOIL(ICOMP)
        Water_Table = bsWTD(ICOMP)
        TGavg = bsTGavg(ICOMP)
        TGamp = bsTGamp(ICOMP)
        TGps = bsTGps(ICOMP)
        fBASESIMPVersionNumber=INT(bsVrsnNmbr(ICOMP))

C Echo the current information to the user.
        call edisp(iuout,'Current BASESIMP information')
        write(outs,'(a,f10.4)') 'Foundation height: ',bsmtHeight
        call edisp(iuout,outs)
        write(outs,'(a,f10.4)') 'Foundation depth : ',bsmtDepth
        call edisp(iuout,outs)
        write(outs,'(a,f10.4)') 'Foundation length: ',bsmtLength
        call edisp(iuout,outs)
        write(outs,'(a,f10.4)') 'Foundation width : ',bsmtWidth
        call edisp(iuout,outs)
        write(outs,'(a,f10.4)') 
     &   'Insulation overlap for combination designs:',Insul_Overlap
        call edisp(iuout,outs)
        write(outs,'(a,f10.4)') 'Insulation resistance in RSI: ',
     &    Insul_rsi
        call edisp(iuout,outs)
        write(outs,'(a,f10.4)') 'Soil conductivity: ',Soil_Cond
        call edisp(iuout,outs)
        write(outs,'(a,f10.4)') 'Water-table depth: ',Water_Table
        call edisp(iuout,outs)
        write(outs,'(a,f10.4)') 'Anually-averaged soil temperature: ',
     &    TGavg
        call edisp(iuout,outs)
        write(outs,'(a,f10.4)') 
     &    'Amplitude of ground-temp annual sine wave: ',TGamp 
        call edisp(iuout,outs)
        write(outs,'(a,f10.4)') 
     &    'Phase lag of ground-temperature annual sine wave: ',TGps
        call edisp(iuout,outs)
        if(fBASESIMPVersionNumber.EQ.1)then
          write(outs,'(a)') 'Ground temp calc via Moore method'
          call edisp(iuout,outs)
        elseif(fBASESIMPVersionNumber.EQ.0)then
          write(outs,'(a)') 'Ground temp calc via: TGavg, TGamp, TGps'
          call edisp(iuout,outs)
        endif

C User edit choices.
        CALL EASKABC('Use current .bsm values or edit? ',' ',
     &    'use current data','edit','dereference basesimp definition',
     &    IWHICH,nbhelp)
        IF(IWHICH.EQ.1)THEN
          RETURN     ! nothing else to do
        ELSEIF(IWHICH.EQ.2)THEN
          CONTINUE   ! proceed to the next block of code
        ELSEIF(IWHICH.EQ.3)THEN
          CALL EDISP(IUOUT,'Removing references to existing BSM ')
          CALL EDISP(IUOUT,'(file will be preserved for later use).')
          iBSIMP(ICOMP)=0
          LBSIMP(ICOMP)=' '
          RETURN
        END IF
      ELSE
        CONTINUE   ! proceed to the next block of code
      END IF

C BASESIMP file inputs

C CONTENT for depth of basement.
      min_bsmtDepth = 0.05                    ! meter
      max_bsmtDepth = 2.4                     ! meter
      CALL EASKR(bsmtDepth,' ',' Depth of the basement (m) ? ',
     &  min_bsmtDepth,'F',max_bsmtDepth,'F',bsmtDepth,
     & 'depth of the basement',IER,nbhelp)

C CONTENT for height of basement.
      min_bsmtHeight = bsmtDepth+0.1          ! meter
      max_bsmtHeight = 2.5                    ! meter
      CALL EASKR(bsmtHeight,' ',' Height of the basement (m) ? ',
     &  min_bsmtHeight,'F',max_bsmtHeight,'F',bsmtHeight,
     &  'height of the basement',IER,nbhelp)

C CONTENT for width of basement.
      min_bsmtWidth = 2.                      ! meter
      max_bsmtWidth = 20.                     ! meter
      CALL EASKR(bsmtWidth,' ',' Width of the basement (m) ? ',
     &  min_bsmtWidth,'F',max_bsmtWidth,'F',bsmtWidth,
     &  'width of the basement',IER,nbhelp)

C CONTENT for length of basement.
      min_bsmtLength = bsmtWidth              ! meter
      max_bsmtLength = 20.                    ! meter
      CALL EASKR(bsmtLength,' ',' Length of the basement (m) ? ',
     &  min_bsmtLength,'F',max_bsmtLength,'F',bsmtLength,
     &  'width of the basement',IER,nbhelp)

C CONTENT for insulation overlap.
      min_Insul_Overlap = 0.                  ! meter
      max_Insul_Overlap = 2.4                 ! meter
      CALL EASKR(Insul_Overlap,' ',' Insulation overlap (m)? ',
     &  min_Insul_Overlap,'F',max_Insul_Overlap,'F',Insul_Overlap,
     &  'Insulation overlap',IER,nbhelp)

C CONTENT for insulation RSI.
      min_Insul_rsi = 0.                      ! rsi (m2K/W)
      max_Insul_rsi = 20.                     ! rsi (m2K/W)
      CALL EASKR(Insul_rsi,' ',' Insulation rsi (m2K/W)? ',
     &  min_Insul_rsi,'F',max_Insul_rsi,'F',Insul_rsi,
     &  'Insulation rsi',IER,nbhelp)

C CONTENT for soil conductivity.
      min_Soil_Cond = 0.1                     ! W/mK
      max_Soil_Cond = 10.                     ! W/mK
      CALL EASKR(Soil_Cond,' ',' Soil conductivity (W/mK) ? ',
     &  min_Soil_Cond,'F',max_Soil_Cond,'F',Soil_Cond,
     &  'Soil Conductivity',IER,nbhelp)

C CONTENT water table.
      min_Water_Table = 5.                     ! meter
      max_Water_Table = 20.                    ! meter
      CALL EASKR(Water_Table,' ',' Water table depth (m) ? ',
     &  min_Water_Table,'F',max_Water_Table,'F',Water_Table,
     &  'Water table depth',IER,nbhelp)

C SELECT METHOD OF HANDLING GROUND TEMPERATURE
C Using the Moore method for ground temperatures the TG values are
C calculated by another subroutine. Otherwise, they must enter these.

C HELP for Moore.
      helptopic='basesimp_moore_method'
      call gethelptext(helpinsub,helptopic,nbhelp)
      CALL EASKABC('Method for handling ground temp:',' ',
     &  'Use Moore method','Enter TGavg,TGamp,TGps','cancel',
     &  IWHICH,nbhelp)

C FILE VERSIONING
C Set the version number based on user's selection (1 or 0).
C If the version number does not exist/equals zero/equals 99, the
C values of TG avg, TGamp, TGps are read in.

      IF(IWHICH.EQ.1)THEN
        fBASESIMPVersionNumber = 1
      ELSEIF(IWHICH.EQ.2)THEN
        fBASESIMPVersionNumber = 0

C Average ground temperature.
        min_TGavg = -10.                  ! degC
        max_TGavg = 20.                   ! degC
        CALL EASKR(TGavg,' ',' TGavg (degC) ?',
     &    min_TGavg,'F',max_TGavg,'F',8.9,'TGavg',IER,nbhelp)

C Amplidude of ground temperature.
        min_TGamp = 0.                    ! degC
        max_TGamp = 25.                   ! degC
        CALL EASKR(TGamp,' ',' TGamp (degC) ?',
     &    min_TGamp,'F',max_TGamp,'F',14.20,'TGamp',IER,nbhelp)

C Phase lag.
        min_TGps = 0.                     ! radians
        max_TGps = 1.                     ! radians
        CALL EASKR(TGps,' ',' TGps (radians) ?',
     &    min_TGps,'F',max_TGps,'F',0.380,'TGps',IER,nbhelp)
      ELSEIF(IWHICH.EQ.3)THEN
        RETURN
      END IF ! END of "GROUND TEMPERATUE METHOD" IF LOOP

C FILE CHECK
C Checks whether or not default.bsm file exists,rewrites or creates it.
      if(zonehasone)then
        bsmFile=LBSIMP(ICOMP)
        iuo=66
        CALL EFOPSEQ(iuo,bsmFile,4,IER)
      else
        iuo=66
        IF(iBSIMP(ICOMP).EQ.0)THEN
          write(bsmFile,'(3a)') zonepth(1:lnblnk(zonepth)),fs,
     &      'default.bsm'
        ELSE
          bsmFile=snpfile
        END IF

        ISTRW=72
        CALL EASKF(bsmFile,'Confirm filename:',' ',ISTRW,bsmFile,
     &    'Invalid filename',IER,nbhelp)

        CALL EFOPSEQ(iuo,bsmFile,4,IER)
      endif
      if(ier.eq.0)then

C WRITE DATA TO FILE
C Writes the basesimp variables to the strings for formatted output.
        WRITE(strHeight,'(F10.4)') bsmtHeight
        WRITE(strDepth,'(F10.4)') bsmtDepth
        WRITE(strLength,'(F10.4)') bsmtLength
        WRITE(strWidth,'(F10.4)') bsmtWidth
        WRITE(strOverlap,'(F10.4)') Insul_Overlap
        WRITE(strRSI,'(F10.4)') Insul_rsi
        WRITE(strSoil,'(F10.4)') Soil_Cond
        WRITE(strWater,'(F10.4)') Water_Table
        WRITE(strHeight,'(F10.4)') bsmtHeight

C Removes the preceeding blanks from string form of variables.
        strHeight=ADJUSTL(strHeight)
        strDepth=ADJUSTL(strDepth)
        strLength=ADJUSTL(strLength)
        strWidth=ADJUSTL(strWidth)
        strOverlap=ADJUSTL(strOverlap)
        strRSI=ADJUSTL(strRSI)
        strSoil=ADJUSTL(strSoil)
        strWater=ADJUSTL(strWater)

C Writes the cleaned up strings to the output .bsm file with comments.
        IF(fBASESIMPVersionNumber.EQ.1)THEN
          WRITE(iuo,'(A)',IOSTAT=IOS,ERR=13) '*FileVersion1.0'
        ELSEIF(fBASESIMPVersionNumber.EQ.0)THEN
          WRITE(iuo,'(A)',IOSTAT=IOS,ERR=13) '*FileVersion0.0'
        END IF
        WRITE(iuo,'(A,A)',IOSTAT=IOS,ERR=13) strHeight,
     &    '# foundation height (m)'
        WRITE(iuo,'(A,A)',IOSTAT=IOS,ERR=13) strDepth,
     &    '# foundation depth (m)'
        WRITE(iuo,'(A,A)',IOSTAT=IOS,ERR=13) strLength,
     &    '# foundation length (m)'
        WRITE(iuo,'(A,A)',IOSTAT=IOS,ERR=13) strWidth,
     &    '# foundation width (m)'
        WRITE(iuo,'(A,A)',IOSTAT=IOS,ERR=13) strOverlap,
     &        '# insul overlap (for BCCN_1,BCCN_2)'
        WRITE(iuo,'(A,A)',IOSTAT=IOS,ERR=13) strRSI,
     &    '# insulation in RSI'
        WRITE(iuo,'(A,A)',IOSTAT=IOS,ERR=13) strSoil,
     &    '# soil conductivity (W/m K)'
        WRITE(iuo,'(A,A)',IOSTAT=IOS,ERR=13) strWater,
     &    '# water table (m)'
        IF(fBASESIMPVersionNumber.EQ.1)THEN
          continue
        ELSEIF(fBASESIMPVersionNumber.EQ.0)THEN

C If the user input the ground temperature variables, they are written
C to the .bsm file also.
C Writes the basesimp variables to the strings for formatted output.
          WRITE(strTGavg,'(F10.4)') TGavg
          WRITE(strTGamp,'(F10.4)') TGamp
          WRITE(strTGps,'(F10.4)') TGps

C Removes the preceeding blanks from string form of variables.
          strTGavg=ADJUSTL(strTGavg)
          strTGamp=ADJUSTL(strTGamp)
          strTGps=ADJUSTL(strTGps)

C Writes the cleaned up strings to the output .bsm file with comments.
          WRITE(iuo,'(A,A)',IOSTAT=IOS,ERR=13) strTGavg,'# TG,avg'
          WRITE(iuo,'(A,A)',IOSTAT=IOS,ERR=13) strTGamp,'# TG,amp'
          WRITE(iuo,'(A,A)',IOSTAT=IOS,ERR=13) strTGps,'# TG,ps'
        endif
        CALL ERPFREE(iuo,ios)
        iBSIMP(ICOMP)=1
        LBSIMP(ICOMP)=bsmFile
      endif

      RETURN

C Error messages.
   13 if(IOS.eq.2)then
        CALL USRMSG('No permission to write ',bsmFile,'W')
      else
        CALL USRMSG('File write error in ',bsmFile,'W')
      endif
      IER=1
      return

      END
