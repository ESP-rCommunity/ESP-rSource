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
C Declare calling parameters 
      INTEGER, INTENT(IN)  :: ICOMP     
      INTEGER, INTENT(INOUT)  :: IER

      INTEGER LNBLNK   ! function definition
      
      COMMON/pophelp/h(60)
      COMMON/bsmtdef/iBSIMP(MCOM),LBSIMP(MCOM)
      COMMON/OUTIN/IUOUT,IUIN

C COMMONS FOR BASEREAD
      COMMON/BSdata/bsHEIG(MCOM),bsDEPT(MCOM),bsLENG(MCOM),bsWIDT(MCOM),
     &              bsOVER(MCOM),bsRSI(MCOM),bsSOIL(MCOM),bsWTD(MCOM)
      COMMON/BSvn/bsVrsnNmbr(MCOM)
      COMMON/BSwth/bsTGavg(MCOM),bsTGamp(MCOM),bsTGps(MCOM)

      CHARACTER*72 H,bsmFile
      CHARACTER*27 strHeight,strDepth,strLength,strWidth,strOverlap,
     &        strRSI,strSoil,strWater,strTGavg,strTGamp,strTGps
C      CHARACTER :: delimiter
      CHARACTER*72 sfile,snpfile,LBSIMP

      INTEGER iBSIMP ! iBSIMP flag to talk to cfg
      INTEGER x,iier,iuout,iuin,iwhich,nfile
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

C Define the basesimp relative path (assuming user is in cfg folder).
      if(bsmpth(1:2).eq.'  ') then
        write(bsmpth,'(3a)') '..',fs,'bsm'
      endif

C Check if the zone bsm file already exists. If not it might be
C necessary to create a folder in the same directory as the
C standard set of folders called ../bsm if it does not already exist.
      call FINDFIL(LBSIMP(ICOMP),exists)
      IF(exists)THEN
        CALL EDISP(IUOUT,
     &    'The zone includes an existing BASESIMP file:')
        CALL EDISP(IUOUT,LBSIMP(ICOMP))
        zonehasone = .true.
      ELSE
        INQUIRE(FILE=bsmpth,EXIST=exists)
        if(exists)then   ! the folder exists
          continue
        else

C Use the pattern in folders.F to issue mkdir command.
          write(doit,'(2a)') 'mkdir ',bsmpth
          call usrmsg('Creating model folder:',doit,'-')
          call runit(doit,'-')
          zonehasone = .false.
        endif
      END IF ! the exists test

C HELP CONTENT for initial selection from the user.
      H(1)= 'One or more surfaces in the current zone have been'
      H(2)= 'attributed with the boundary condition BASESIMP.'
      H(3)= 'A .bsm file must be created or referenced.'
      H(4)= 'You may select one from a list, create a new one, or'
      H(5)= 'de-reference an exising definition (the file will be'
      H(6)= 'kept for use later). '

C USER INPUTS
C Upon exiting the SURFACE ATTRIBUTES the user is prompted for inputs
C BASESIMP is being used by one or more surfaces in the zone, the user
C must decide whether to create/edit the .BSM file now or later.
      if(zonehasone)then
        CALL EASKABCD('Edit existing *.bsm?',' ','yes','no ',
     &    'browse for another bsm file','dereference basesimp',
     &    IWHICH,6)
        if(iwhich.eq.4)then
          call usrmsg('Dereferencing the bsm file...',
     &      ' ','-')
          iBSIMP(ICOMP)=0
          LBSIMP(ICOMP)=' '
          return
        endif
      else
        CALL EASKABCD('Create new *.bsm?',' ','yes','no ',
     &    'browse for another bsm file','cancel',IWHICH,6)
        if(iwhich.eq.4)then
          call usrmsg('Continuing without creating bsm file...',
     &      ' ','-')
          return
        endif
      endif


C HELP CONTENT for using existing file.
      H(1)= 'If a file exists in the ../bsm directory, you may  '
      H(2)= 'select and use this file. Otherwise, create a new  '
      H(3)= 'file and input the parameters or use the defaults. '
      H(4)= '                                                   '
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

C HELP CONTENT for use edit choice.
        H(1)= 'For the current .bsm file you have several options:'
        H(2)= ' - You may use the values in the file as is (so no '
        H(3)= '   further actions are taken).'
        H(4)= ' - You may choose to edit it and save the changes.'
        H(5)= ' - You can defererence the BASESIMP definition (in'
        H(6)= '   which case the file is preserved for later use).'

        CALL EASKABC('Use current .bsm values or edit? ',' ',
     &    'use current data','edit','dereference basesimp definition',
     &    IWHICH,6)
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

C HELP CONTENT for depth of basement.
      min_bsmtDepth = 0.05                    ! meter
      max_bsmtDepth = 2.4                     ! meter
      H(1)= 'Depth: distance from the top of the floor slab to  '
      H(2)= 'the grade level.                                   '
      H(3)= 'Must be between 0.05 and 2.4m                      '
      H(4)= '                                                   '
      CALL EASKR(bsmtDepth,' ',' Depth of the basement (m) ? ',
     &  min_bsmtDepth,'F',max_bsmtDepth,'F',bsmtDepth,
     & 'depth of the basement',IER,4)

C HELP CONTENT for height of basement.
      min_bsmtHeight = bsmtDepth+0.1          ! meter
      max_bsmtHeight = 2.5                    ! meter
      H(1)= 'Height: distance from the top of the floor slab to '
      H(2)= 'the top of the basement structural wall.           '
      H(3)= 'Must be at least 0.1m greater than the depth.      '
      H(4)= 'Must also be between 1.0 and 2.5m                  '
      CALL EASKR(bsmtHeight,' ',' Height of the basement (m) ? ',
     &  min_bsmtHeight,'F',max_bsmtHeight,'F',bsmtHeight,
     &  'height of the basement',IER,4)

C HELP CONTENT for width of basement.
      min_bsmtWidth = 2.                      ! meter
      max_bsmtWidth = 20.                     ! meter
      H(1)= 'Width: exterior-of-structural-wall to exterior-of- '
      H(2)= 'structural-wall distance.                          '
      H(3)= 'Must be less than or equal to the length.          '
      H(4)= 'Value must be between 2.0 and 20m.                 '
      CALL EASKR(bsmtWidth,' ',' Width of the basement (m) ? ',
     &  min_bsmtWidth,'F',max_bsmtWidth,'F',bsmtWidth,
     &  'width of the basement',IER,4)

C HELP CONTENT for length of basement.
      min_bsmtLength = bsmtWidth              ! meter
      max_bsmtLength = 20.                    ! meter
      H(1)= 'Length: exterior-of-structural-wall to exterior-of-'
      H(2)= 'structural-wall distance.                          '
      H(3)= 'Must be greater than or equal to the width.        '
      H(4)= '                                                   '
      CALL EASKR(bsmtLength,' ',' Length of the basement (m) ? ',
     &  min_bsmtLength,'F',max_bsmtLength,'F',bsmtLength,
     &  'width of the basement',IER,4)

C HELP CONTENT for insulation overlap.
      min_Insul_Overlap = 0.                  ! meter
      max_Insul_Overlap = 2.4                 ! meter
      H(1)= 'Insulation overlap: value must be given but only   '
      H(2)= 'used for BCCN_1 and BCCN_2 combination cases.      '
      H(3)= 'Height of the wall covered by both interior and    '
      H(4)= 'exterior wall insulation (between 0 and 2.4m).     '
      CALL EASKR(Insul_Overlap,' ',' Insulation overlap (m)? ',
     &  min_Insul_Overlap,'F',max_Insul_Overlap,'F',Insul_Overlap,
     &  'Insulation overlap',IER,4)

C HELP CONTENT for insulation RSI.
      min_Insul_rsi = 0.                      ! rsi (m2K/W)
      max_Insul_rsi = 20.                     ! rsi (m2K/W)
      H(1)= 'Insulation: define the thermal resistance of the   '
      H(2)= 'insulation by entering the rsi value (m2K/W).      '
      H(3)= '                                                   '
      H(4)= 'Value must be between 0 and 20 m2K/W.              '
      CALL EASKR(Insul_rsi,' ',' Insulation rsi (m2K/W)? ',
     &  min_Insul_rsi,'F',max_Insul_rsi,'F',Insul_rsi,
     &  'Insulation rsi',IER,4)

C HELP CONTENT for soil conductivity.
      min_Soil_Cond = 0.1                     ! W/mK
      max_Soil_Cond = 10.                     ! W/mK
      H(1)= 'Thermal conductivity of soil surrounding the       '
      H(2)= 'foundation in W/mK.                                '
      H(3)= 'Value must be between 0.1 and 10 W/mK.             '
      H(4)= '                                                   '
      CALL EASKR(Soil_Cond,' ',' Soil conductivity (W/mK) ? ',
     &  min_Soil_Cond,'F',max_Soil_Cond,'F',Soil_Cond,
     &  'Soil Conductivity',IER,4)

C HELP CONTENT water table.
      min_Water_Table = 5.                     ! meter
      max_Water_Table = 20.                    ! meter
      H(1)= 'Water table depth: defines location of horizontal  '
      H(2)= 'thermal boundary. The deeper the water table, the  '
      H(3)= 'lower the foundation heat loss will be.            '
      H(4)= 'Value must be between 5 and 20m.                   '
      CALL EASKR(Water_Table,' ',' Water table depth (m) ? ',
     &  min_Water_Table,'F',max_Water_Table,'F',Water_Table,
     &  'Water table depth',IER,4)

C SELECT METHOD OF HANDLING GROUND TEMPERATURE
C Using the Moore method for ground temperatures the TG values are
C calculated by another subroutine. Otherwise, they must enter these.

C HELP CONTENT
      H(1)= 'If you choose to use the Moore Method,             '
      H(2)= 'TGavg, TGamp, TGps ground temperature parameters   '
      H(3)= 'will be calculated. Otherwise, you must enter      '
      H(4)= 'values for these parameters or use the defaults.   '
      H(5)= ' '
      H(6)= ' TGavg: Average ground temperature (degC)          '
      H(7)= ' TGamp: Amplitude of the ground temperature (degC) '
      H(8)= ' TGps: Phase lag of ground temperature (radians)   '
      CALL EASKABC('Method for handling ground temp:',' ',
     &  'Use Moore method','Enter TGavg,TGamp,TGps','cancel',
     &  IWHICH,8)

C FILE VERSIONING
C Set the version number based on user's selection (1 or 0).
C If the version number does not exist/equals zero/equals 99, the
C values of TG avg, TGamp, TGps are read in.

      IF(IWHICH.EQ.1)THEN
        fBASESIMPVersionNumber = 1
      ELSEIF(IWHICH.EQ.2)THEN
        fBASESIMPVersionNumber = 0

C HELP CONTENT or average ground temperature.
        H(1)= '                                                   '
        H(2)= 'TGavg: Average ground temperature (degC)           '
        H(3)= '                                                   '
        H(4)= 'Value must be between -10 and 20(degC).            '
        min_TGavg = -10.                  ! degC
        max_TGavg = 20.                   ! degC
        CALL EASKR(TGavg,' ',' TGavg (degC) ?',
     &    min_TGavg,'F',max_TGavg,'F',8.9,'TGavg',IER,4)

C HELP CONTENT for amplidude of ground temperature.
        min_TGamp = 0.                    ! degC
        max_TGamp = 25.                   ! degC
        H(1)= '                                                   '
        H(2)= 'TGamp: Amplitude of the ground temperature         '
        H(3)= '                                                   '
        H(4)= 'Value must be between 0 and 25(degC).              '
        CALL EASKR(TGamp,' ',' TGamp (degC) ?',
     &    min_TGamp,'F',max_TGamp,'F',14.20,'TGamp',IER,4)

C HELP CONTENT for phase lag.
        H(1)= '                                                   '
        H(2)= 'TGps: Phase lag of ground temperature              '
        H(3)= '                                                   '
        H(4)= 'Value must be between 0 and 1 radians.             '
        min_TGps = 0.                     ! radians
        max_TGps = 1.                     ! radians
        CALL EASKR(TGps,' ',' TGps (radians) ?',
     &    min_TGps,'F',max_TGps,'F',0.380,'TGps',IER,4)
      ELSEIF(IWHICH.EQ.3)THEN
        RETURN
      END IF ! END of "GROUND TEMPERATUE METHOD" IF LOOP

C FILE CHECK
C Checks whether or not default.bsm file exists,rewrites or creates it.

C HELP CONTENT
      H(1)= 'You may change the filename for the .BSM file.     '
      H(2)= 'If your project contains the standard folder set,  '
      H(3)= 'the file will be stored in a new folder ../bsm     '
      H(4)= '                                                   '
      if(zonehasone)then
        bsmFile=LBSIMP(ICOMP)
        iuo=66
        CALL EFOPSEQ(iuo,bsmFile,4,IER)
      else
        iuo=66
        IF(iBSIMP(ICOMP).EQ.0)THEN
          write(bsmFile,'(3a)') bsmpth(1:lnblnk(bsmpth)),fs,
     &      'default.bsm'
        ELSE
          bsmFile=snpfile
        END IF

        CALL EASKF(bsmFile,'Confirm filename:',' ',72,bsmFile,
     &    'Invalid filename',IER,4)

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
