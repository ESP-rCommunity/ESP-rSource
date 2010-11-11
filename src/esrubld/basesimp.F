C This file is part of the ESP-r system.
C Copyright Natural Resources Canada 1999
C Author: Ian Beausoleil-Morrison
C Revision Date: September 28, 1999

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

C This file contains the following routines related to the BASESIMP
C foundation heat loss model:
C BASEREAD     Reads the data contained in the .bsm file.
C BSFCCTL      Controls calculation of BASESIMP heat loss factors.
C BSFACS       Calcualtes heat-loss factors for BASESIMP configuration.
C BSTEMP       Calcualtes time-mean basement air-point temperature.
C BSHEAT       Calcualtes heat transfer to BASESIMP foundation for current time.
C BSCONV       Sets convection coefficients for BASESIMP BCs.


C ******************** BASEREAD ********************
C BASEREAD     Reads the data contained in the .bsm file.

      SUBROUTINE BASEREAD(ICOMP,IER)
#include "building.h"
#include "espriou.h"
c espriou.h provides currentfile
c common block for ground temperature profiles.
#include "ground_temp_mains_common.h"
      
      integer lnblnk  ! function definition

      COMMON/OUTIN/IUOUT,IUIN
      COMMON/FILEP/IFIL
      common/bsmtdef/iBSIMP(MCOM),LBSIMP(MCOM)
      COMMON/BSdata/bsHEIG(MCOM),bsDEPT(MCOM),bsLENG(MCOM),bsWIDT(MCOM),
     &              bsOVER(MCOM),bsRSI(MCOM),bsSOIL(MCOM),bsWTD(MCOM)
      COMMON/BSvn/bsVrsnNmbr(MCOM)
      COMMON/BSwth/bsTGavg(MCOM),bsTGamp(MCOM),bsTGps(MCOM)

C Current file (for use by low level I/O calls). Error subroutine
C and error details for dll mode.
      common/dllerr/dllsubr,dllmesg

C This common block holds input variables for basement type 999.
C This type takes the BASECALC results as inputs for the heat loss factors,
C instead of calculating the factors itself.
C Using type 999 enables BASESIMP results to closely match BASECALC results
C and also allows bigger basements up to the maximum size allowed by BASECALC
C (20 x 20 meters).
      COMMON/BS999HLF/Sag999(MCOM),Sbgavg999(MCOM),Sbgvar999(MCOM),
     &                Bphase999(MCOM)

      character*72 LBSIMP
      character outs*124,OUTSTR*124,dllsubr*12,dllmesg*124,t60*60
      logical dll

      REAL fBASESIMPVersionNumber	! version number of BASESIMP input file
C Limits for parameter values to be read in
      REAL fMinFoundLength, fMaxFoundLength, fMinFoundWidth,
     &     fMaxFoundWidth, fMinInsulRSI,  fMaxInsulRSI, fMinSoilCond,
     &     fMaxSoilCond, fMinWaterTable, fMaxWaterTable
      logical close,close1,close99

C This subroutine is called once for each zone which has a BASESIMP foundation.
C The data are read from the BASESIMP input data file (.bsm file) and placed
C into the BSsys, BSdata, and BSwth common blocks:
C    bsHEIG(i): Foundation height for zone `i'.
C    bsDEPT(i): Foundation depth for zone `i'.
C    bsLENG(i): Foundation length for zone `i'.
C    bsWIDT(i): Foundation width for zone `i'.
C    bsOVER(i): Insulation overlap for "combination" configurations for zone `i'.
C    bsRSI(i):  Insulation resistance in RSI for zone `i'.
C    bsSOIL(i): Soil conductivity for zone `i'.
C    bsWTD(i):  Water-table depth for zone `i'.
C    bsTGavg(i) Anually-averaged soil temperature	
C    bsTGamp(i) Amplitude of ground-temperature's annual sine wave 
C    bsTGps(i)  Phase lag of ground-temperature's annual sine wave
C NOTE: the references to the zone numbers have been removed, as these values are not
c       zone-dependent.
C**********************************************************************************
C Check if running in dll mode.
      call isadll(dll)

C Set the version number
      fBASESIMPVersionNumber = 0.

C Open BASESIMP input data file.
      IUNIT=IFIL+1
      CALL ERPFREE(IUNIT,ISTAT)
      CALL EFOPSEQ(IUNIT,LBSIMP(ICOMP),1,IER)
      IF(ier.lt.0)then
        write(outs,'(3a)') 'BASESIMP file ',
     &     LBSIMP(ICOMP)(1:lnblnk(LBSIMP(ICOMP))),
     &     ' could not be opened.'
        if(dll)then
          dllsubr='BASEREAD'
          dllmesg=outs
          ier=2
          return
        else
          call edisp(IUOUT,outs)
          IER=1
          RETURN
        endif
      endif
      write(currentfile,'(a)') LBSIMP(ICOMP)(1:lnblnk(LBSIMP(ICOMP)))

C Default error message.
      t60='low level I/O read error '

C Versioning Control
c Determine the version number of this file.
c If it is an old file (with no version number or 0 version number),
c or a file with version number 99, then we will
c read in the values of bsTGavg(i), bsTGamp(i)and bsTGps(i).
c For other versions (1 <= version number >= 98),
c these values won't be read in at all.
c They are however calculated in file /esrubld/ground_temp_mains.F 

C Get first line.
      CALL STRIPC(IUNIT,OUTSTR,0,ND,1,' BASESIMP data',IER)
      IF(IER.NE.0) goto 1000
      K=0

C-----Read the file version number, if applicable
      IF ( outstr(1:12).eq.'*FileVersion' ) then
        k = 12
C-------write version number to fBASESIMPVersionNumber
        call egetwr(outstr,k,fBASESIMPVersionNumber,0.0,100.,'F',
     &              'version number',IER)
c-------read in next line of the input file (to get foundation height)
        CALL STRIPC(IUNIT,OUTSTR,0,ND,1,' BASESIMP data',IER)
        k=0
        IF(IER.NE.0)goto 1000
      ELSE   
c-------if there is no version number, continue with reading in the BASESIMP data
        CONTINUE
      ENDIF
c-----Set logical variables related to version number
      CALL ECLOSE(fBASESIMPVersionNumber,0.0,0.0001,CLOSE)
      CALL ECLOSE(fBASESIMPVersionNumber,99.0,0.0001,CLOSE99)
      CALL ECLOSE(fBASESIMPVersionNumber,1.0,0.0001,CLOSE1)
c-----Write version number to common block for later use
      bsVrsnNmbr(icomp)=fBASESIMPVersionNumber

c-----BASESIMP was developed for basements with length between 5 and 100
c-----m and width between 5 and 12 m. However, type '999' follows
c-----the dimensions for BASECALC (max 20 by 20 m - with a minimum of 2 m).
c-----Maximum and minimum values for parameters to be read in are set
c-----for BASESIMP vs BASECALC ('999') types.
      IF(CLOSE99) THEN     ! set parameter windows on BASECALC dimensions (type 999)
        fMinFoundLength = 2.0   ! meter
        fMaxFoundLength = 20.0  ! meter
        fMinFoundWidth = 2.0    ! meter
        fMaxFoundWidth = 20.0   ! meter
        fMinInsulRSI = 0.0      ! m2K/W
        fMaxInsulRSI = 20.0     ! m2K/W
        fMinSoilCond = 0.1      ! W/mK
        fMaxSoilCond = 10.0     ! W/mK
        fMinWaterTable = 5.0    ! meter
        fMaxWaterTable = 20.0   ! meter
      ELSE                 ! set parameter windows on BASESIMP dimensions
        fMinFoundLength = 5.0   ! meter
        fMaxFoundLength = 100.0 ! meter
        fMinFoundWidth = 1.5    ! meter   ! updated May 6,08 based on testing by Brian Bradley.
        fMaxFoundWidth = 20.0   ! meter   ! updated May 6,08 based on testing by Brian Bradley.
        fMinInsulRSI = 0.0      ! m2K/W
        fMaxInsulRSI = 9.0      ! m2K/W
        fMinSoilCond = 0.85     ! W/mK
        fMaxSoilCond = 1.90     ! W/mK
        fMinWaterTable = 5.0    ! meter
        fMaxWaterTable = 15.0   ! meter
      ENDIF

      CALL EGETWR(OUTSTR,K,VAL,1.0,2.5,'F',' found. height',IER)
      if(IER.NE.0)then
        t60=' Cannot read height of foundation '
        goto 1000
      endif
      bsHEIG(icomp)=VAL

C Read foundation depth.
      CALL STRIPC(IUNIT,OUTSTR,1,ND,1,' BASESIMP data',IER)
      IF(IER.NE.0)goto 1000
      K=0
      CALL EGETWR(OUTSTR,K,VAL,0.05,2.4,'F',' found. depth',IER)
      if(IER.NE.0)then
        t60=' Cannot read depth of foundation '
        goto 1000
      endif
      bsDEPT(icomp)=VAL

C Check whether height and depth fall within ranges of applicability of
C BASESIMP correlations.
      if( (bsHEIG(icomp)-bsDEPT(icomp)) .lt.0.1 ) then
        t60=' Height must be 0.1m greater than depth '
        goto 1000
      endif
      if( bsDEPT(icomp).gt.0.05 .and. bsDEPT(icomp).lt.0.65 ) then
        t60='Slab-on-grade depth >0.05m or basement depth >0.65m '
        goto 1000
      endif

C Read foundation length.
      CALL STRIPC(IUNIT,OUTSTR,1,ND,1,' BASESIMP data',IER)
      IF(IER.NE.0)goto 1000
      K=0
      CALL EGETWR(OUTSTR,K,VAL,fMinFoundLength,fMaxFoundLength,
     &            'F',' found. length',IER)
      if(IER.NE.0)then
        t60=' Cannot read length of foundation '
        goto 1000
      endif
      bsLENG(icomp)=VAL

C Read foundation width.
      CALL STRIPC(IUNIT,OUTSTR,1,ND,1,' BASESIMP data',IER)
      IF(IER.NE.0)goto 1000
      K=0
      CALL EGETWR(OUTSTR,K,VAL,fMinFoundWidth,fMaxFoundWidth,
     &            'F',' found. width',IER)
      if(IER.NE.0)then
        t60=' Cannot read width of foundation '
        goto 1000
      endif
      bsWIDT(icomp)=VAL

C Check that width is not greater than length.
      if(bsWIDT(icomp) .gt. bsLENG(icomp)) then
        t60=' Foundation width cannot be greater than length '
        goto 1000
      endif

C Read overlap length for "combination" insulation. Data must be always be
C present, but only used for combination cases, eg. BCCN_1 and BCCN_2.
      CALL STRIPC(IUNIT,OUTSTR,1,ND,1,' BASESIMP data',IER)
      IF(IER.NE.0)goto 1000
      K=0
      CALL EGETWR(OUTSTR,K,VAL,0.,2.4,'F',' overlap',IER)
      if(IER.NE.0)then
        t60=' Cannot read inside/outside overlap '
        goto 1000
      endif
      bsOVER(icomp)=VAL

C Check that "combination" overlap is not greater than depth.
      if(bsOVER(icomp) .gt. bsDEPT(icomp)) then
        t60=' Overlap cannot be greater than depth '
        goto 1000
      endif

C Read insulation resistance.
      CALL STRIPC(IUNIT,OUTSTR,1,ND,1,' BASESIMP data',IER)
      IF(IER.NE.0)goto 1000
      K=0
      CALL EGETWR(OUTSTR,K,VAL,fMinInsulRSI,fMaxInsulRSI,
     &            'F',' insulation RSI',IER)
      if(IER.NE.0)then
        t60=' Cannot read insulation RSI '
        goto 1000
      endif
      bsRSI(icomp)=VAL

C Read soil conductivity.
      CALL STRIPC(IUNIT,OUTSTR,1,ND,1,' BASESIMP data',IER)
      IF(IER.NE.0)goto 1000
      K=0
      CALL EGETWR(OUTSTR,K,VAL,fMinSoilCond,fMaxSoilCond,
     &            'F',' soil cond.',IER)
      if(IER.NE.0)then
        t60=' Cannot read soil conductivity '
        goto 1000
      endif
      bsSOIL(icomp)=VAL

C Read depth of water table.
      CALL STRIPC(IUNIT,OUTSTR,1,ND,1,' BASESIMP data',IER)
      IF(IER.NE.0)goto 1000
      K=0
      CALL EGETWR(OUTSTR,K,VAL,fMinWaterTable,fMaxWaterTable,
     &            'F',' water table',IER)
      if(IER.NE.0)then
        t60=' Cannot read water-table depth '
        goto 1000
      endif
      bsWTD(icomp)=VAL


C-----If the BASESIMP input file has version number 1, then 
C-----the variables bsTGavg(i), bsTGamp(i)and bsTGps(i) are not read in.
C-----These will be set to values based on weather file and Moore model
C-----*******************************************************
C-----This has only been implemented for version number = 1 for now, 
C-----implementation for version number > 1 AND < 99 has not been done yet.
C-----*******************************************************
      IF(CLOSE1) then 
C-------Close BASESIMP input data file
        CALL ERPFREE(IUNIT,ISTAT)
C-------Set ground temperature calculation method flag to 2 indicating ground
C-------temperature parameters are determined based on Moore model and weather file
        Basesimp_grd_temp_cal_flag(icomp) = 2

C-----If there is no version number associated with the BASESIMP input file, 
C-----then, these values are read in
      ELSEIF(CLOSE .OR. CLOSE99) THEN
C-----Set ground temperature calculation method flag to 1 indicating that these
C-----ground temperature parameters are read in from basesimp file
        Basesimp_grd_temp_cal_flag(icomp) = 1

C Read annually-averaged soil temperature.
        CALL STRIPC(IUNIT,OUTSTR,1,ND,1,' BASESIMP data',IER)
        IF(IER.NE.0)goto 1000
        K=0
C modified February 10, 2004 JPurdy
C To allow for negative ground temperatures in permafrost 
C locations.
        CALL EGETWR(OUTSTR,K,VAL,-10.,20.,'F',' Tgavg ',IER)
        if(IER.NE.0)then
          t60=' Cannot read average of ground Temp '
          goto 1000
        endif
        bsTGavg(icomp)=VAL	

C Read amplitude of ground-temperature's annual sine wave.
        CALL STRIPC(IUNIT,OUTSTR,1,ND,1,' BASESIMP data',IER)
        IF(IER.NE.0)goto 1000
        K=0
        CALL EGETWR(OUTSTR,K,VAL,0.,25.,'F',' Tgamp ',IER)
        if(IER.NE.0)then
          t60=' Cannot read amplitude of ground Temp '
          goto 1000
        endif
        bsTGamp(icomp)=VAL	

C Read phase lag of ground-temperature's annual sine wave.
        CALL STRIPC(IUNIT,OUTSTR,1,ND,1,' BASESIMP data',IER)
        IF(IER.NE.0)goto 1000
        K=0
        CALL EGETWR(OUTSTR,K,VAL,0.,1.,'F',' Ps ',IER)
        if(IER.NE.0)then
          t60=' Cannot read phase lag of ground Temp '
          goto 1000
        endif
        bsTGps(icomp)=VAL

C If version number = 99, the BASECALC heat loss factors are read in
        IF(CLOSE99) THEN
C Read above-grade heat-loss factor (Sag).
          CALL STRIPC(IUNIT,OUTSTR,1,ND,1,' BASESIMP data',IER)
          IF(IER.NE.0)goto 1000
          K=0
          CALL EGETWR(OUTSTR,K,VAL,0.,100.,'F',' Sag ',IER)
          if(IER.NE.0)then
            t60=' Cannot read above-ground heat-loss factor '
            goto 1000
          endif
          Sag999(icomp)=VAL !this variable is only used for basement type 999

C Read below-grade average heat-loss factor (Sbg_avg).
          CALL STRIPC(IUNIT,OUTSTR,1,ND,1,' BASESIMP data',IER)
          IF(IER.NE.0)goto 1000
          K=0
          CALL EGETWR(OUTSTR,K,VAL,0.,1000.,'F',' Sbg,avg ',IER)
          if(IER.NE.0)then
            t60=' Cannot read below-grade average heat-loss factor '
             goto 1000
          endif
          Sbgavg999(icomp)=VAL !this variable is only used for basement type 999

C Read below-grade variable heat-loss factor (Sbg_var).
          CALL STRIPC(IUNIT,OUTSTR,1,ND,1,' BASESIMP data',IER)
          IF(IER.NE.0)goto 1000
          K=0
          CALL EGETWR(OUTSTR,K,VAL,0.,1000.,'F',' Sbg,var ',IER)
          if(IER.NE.0)then
            t60=' Cannot read below-grade variable heat-loss factor '
            goto 1000
          endif
          Sbgvar999(icomp)=VAL !this variabel is only used for basement type 999

C Read phase angle (phase).
          CALL STRIPC(IUNIT,OUTSTR,1,ND,1,' BASESIMP data',IER)
          IF(IER.NE.0)goto 1000
          K=0
          CALL EGETWR(OUTSTR,K,VAL,0.,5.,'F',' phase ',IER)
          if(IER.NE.0)then
            t60=' Cannot read phase angle (phase) '
            goto 1000
          endif
          Bphase999(icomp)=VAL !this variable is only used for basement type 999
        ENDIF               ! IF(CLOSE99)

C Close BASESIMP input data file.
        CALL ERPFREE(IUNIT,ISTAT)
      ENDIF                 ! ELSEIF(CLOSE .OR. CLOSE99)

      return
        
C Error handling.
 1000 continue
      write(outs,'(2a,i3,a)') t60(1:lnblnk(t60)),' in zone',ICOMP,
     &  ' BASESIMP file.'
      if(dll)then
        dllsubr='BASEREAD'
        dllmesg=outs
        ier=2
        return
      else
        call edisp(iuout,outs)
        IER=1
        CALL ERPFREE(IUNIT,ISTAT)
	
C modified Feb 4, 2004 SPH
C Exit instead of returning
        CALL EXIT(1)
      endif

      END


C ******************** BSFCCTL ********************
C BSFCCTL     Controls calculation of BASESIMP heat loss factors.

      SUBROUTINE BSFCCTL
#include "building.h"

      common/outin/iuout,iuin
      integer ncomp,ncon
      COMMON/C1/NCOMP,NCON
      COMMON/C3/IC1(MCON),IE1(MCON),ICT(MCON),IC2(MCON),IE2(MCON)
      COMMON/BSdata/bsHEIG(MCOM),bsDEPT(MCOM),bsLENG(MCOM),bsWIDT(MCOM),
     &              bsOVER(MCOM),bsRSI(MCOM),bsSOIL(MCOM),bsWTD(MCOM)
      COMMON/BSvn/bsVrsnNmbr(MCOM)
      COMMON/BSHLF/Sag(MCON),Sbgavg(MCON),Sbgvar(MCON),Bphase(MCON)
      COMMON/BS999HLF/Sag999(MCOM),Sbgavg999(MCOM),Sbgvar999(MCOM),
     &                Bphase999(MCOM)

      logical close99
      character outs*124

C This subroutine controls the calcualtion of the BASESIMP heat loss
C factors for the foundation under consideration. It makes appropriate
C calls to establish the BASESIMP and corner-correction coefficients
C and to calculate the four heat loss factors.

C If the BASESIMP configuration is insulated and if the insulation level
C is outside the range of the BASESIMP correlations (ie. RSI < 1.5), then the
C "exponential-interpolation" method must be applied to calculate the
C BASESIMP heat loss factors. This requires two passes at setting coefficients
C and calculating the heat loss factors. Only a single pass is required when
C the foundation is uninsulated or when the insulation is within the RSI range.
C intflg controls the application of the "exponential-interpolation" method:
C     intflg = 99 : no interpolation; use rsi as given by user.
C     intflg = 15 : first pass at interpolation; use rsi=1.5.
C     intflg = 0  : second pass at interpolation; use uninsulated foundation.

C Iterate through each connection, calculating the BASESIMP heat-loss factors
C for each connection with a BASESIMP foundation.
      DO 10 icon=1,NCON

C Does the current connection have a BASESIMP boundary condition?
        IF( ICT(icon).EQ.6 ) THEN

C Set the BASESIMP configuration and foundation insulation variables:
          iconfig = IC2(icon)
          icomp = IC1(icon)
          rsi = bsRSI(icomp)

C Debug (start)
C          write(6,*) 'NCON = ',NCON, '   icon = ', icon, 
C     &                 '   iconfig = ', iconfig, '  icomp = ', icomp
C Debug (end)

C If iconfig = 999 no calculation of heat loss factors is necessary.
C The heat loss factors have already been read in and will be passed on 
C onto the variables in the BSHLF common block (unless iconfig = 999 
C was spefied with the incorrect version number).
          IF(iconfig.eq.999) THEN
            CALL ECLOSE(bsVrsnNmbr(icomp),99.0,0.0001,CLOSE99)
            IF(CLOSE99) THEN
              Sag(icon) = Sag999(icomp)
              Sbgavg(icon) = Sbgavg999(icomp)
              Sbgvar(icon) = Sbgvar999(icomp)
              Bphase(icon) = Bphase999(icomp)
            ELSE
              write(outs,*) 'Version number does not match with ',
     &                     'BASESIMP configuration number.'
              call edisp(iuout,outs)
              write(outs,*) 'Configuration 999 requires an input file ',
     &                     'with version number 99 and ',
     &                      'corresponding structure.'
              call edisp(iuout,outs)
              STOP 'BASESIMP: unresolvable error'
            ENDIF
          ELSE
C Check first to see if version 99 was used with incorrect iconfig number
            CALL ECLOSE(bsVrsnNmbr(icomp),99.0,0.0001,CLOSE99)
            IF(CLOSE99 .and. iconfig.ne.999) THEN
              write(outs,*) 'BASESIMP configuration number does not ',
     &                     'match with version number.'
              call edisp(iuout,outs)
              write(outs,*) 'Version 99 requires BASESIMP ',
     &                     'configuration 999.'
              call edisp(iuout,outs)
              STOP 'BASESIMP: unresolvable error'
            ENDIF

C Calculation of heat loss factors

C If the foundation is insulated and the isulation is less than RSI 1.5, apply
C the "exponential-interpolation" method. BASESIMP configurations 9, 10, 13,
C 28, and 29 are uninsulated.
            IF( .not.(iconfig.eq.9 .or. iconfig.eq.10 
     &        .or. iconfig.eq.13 .or. iconfig.eq.28 .or. iconfig.eq.29)
     &        .and. rsi.lt.1.5 )THEN
C-------------RSI outside of range: apply "exponential-interpolation" method.
C-------------Step 1: calculate factors for rsi=1.5 and save to __15 variables.
              intflg = 15
              CALL BSCOEFF(iconfig)
              CALL BSCORNER
              CALL BSFACS(icon,intflg)
              Sag15 = Sag(icon)
              Sbga15 = Sbgavg(icon)
              Sbgv15 = Sbgvar(icon)
              Bph15 = Bphase(icon)
C-------------Step 2: calculate factors for uninsulated foundation and save to
C-------------__0 variables. Map to appropriate uninsulated config.
              if( (iconfig.ge.1 .and. iconfig.le.12) .or.
     &            (iconfig.ge.19 .and. iconfig.le.27) .or.
     &            (iconfig.ge.68 .and. iconfig.le.72) .or.
     &            (iconfig.ge.73 .and. iconfig.le.77) .or.
     &            (iconfig.ge.92 .and. iconfig.le.101) .or.
     &            (iconfig.ge.109 .and. iconfig.le.110) .or.
     &            (iconfig.ge.114 .and. iconfig.le.119) .or.
     &            (iconfig.ge.121 .and. iconfig.le.131) ) then
C---------------Concrete basement: map to BCNN_1.
                iconzero=9
              endif
              if( (iconfig.ge.13 .and. iconfig.le.18) .or.
     &            (iconfig.eq.78) .or.
     &            (iconfig.ge.102 .and. iconfig.le.103) .or.
     &            (iconfig.ge.132 .and. iconfig.le.139) ) then
C---------------Wood basement: map to BWNN_1.
                iconzero=13
              endif
              if( (iconfig.ge.28 .and. iconfig.le.67) .or.
     &            (iconfig.ge.81 .and. iconfig.le.86) .or.
     &            (iconfig.ge.104 .and. iconfig.le.107) ) then
C---------------Slab-on-grade: map to SCN_1.
                iconzero=28
              endif
              if( (iconfig.ge.79 .and. iconfig.le.80) .or.
     &            (iconfig.ge.87 .and. iconfig.le.91) .or.
     &            (iconfig.eq.108) .or.
     &            (iconfig.ge.111 .and. iconfig.le.113) .or.
     &            (iconfig.eq.120) .or.
     &            (iconfig.ge.140 .and. iconfig.le.145) ) then
C---------------Wood walls and concrete slab: map to BWNN_1 as this gives best
C---------------approximation of uninsulated basement with wood wall + conc slab.
                iconzero=13
              endif
              intflg = 0
              CALL BSCOEFF(iconzero)
              CALL BSCORNER
              CALL BSFACS(icon,intflg)
              Sag0 = Sag(icon)
              Sbga0 = Sbgavg(icon)
              Sbgv0 = Sbgvar(icon)
              Bph0 = Bphase(icon)
C-------------Step 3: interpolate. Interpolation factor (wint) is 2.29 for
C-------------basements and 1.77 for slabs-on-grade.
              if( (iconfig.ge.1 .and. iconfig.le.27) .or.
     &            (iconfig.ge.68 .and. iconfig.le.80) .or.
     &            (iconfig.ge.87 .and. iconfig.le.103) .or.
     &            (iconfig.ge.108 .and. iconfig.le.120) .or.
     &            (iconfig.ge.121 .and. iconfig.le.145) ) then
C---------------Basement.
                wint=2.29
              endif
              if( (iconfig.ge.28 .and. iconfig.le.67) .or.
     &            (iconfig.ge.81 .and. iconfig.le.86) .or.
     &            (iconfig.ge.104 .and. iconfig.le.107) ) then
C---------------Slab-on-grade.
                wint=1.77
              endif
              Sag(icon) = Sag15 + (Sag0-Sag15)/(exp(wint*rsi))
              Sbgavg(icon) = Sbga15 + (Sbga0-Sbga15)/(exp(wint*rsi))
              Sbgvar(icon) = Sbgv15 + (Sbgv0-Sbgv15)/(exp(wint*rsi))
              Bphase(icon) = Bph15 + (Bph0-Bph15)/(exp(wint*rsi))

            ELSE
C Within range of correlations: no interpolation required. Set BASESIMP
C coefficients, corner-correction coefficients, then calculate heat loss factors.
              intflg = 99
              CALL BSCOEFF(iconfig)
              CALL BSCORNER
              CALL BSFACS(icon,intflg)
            ENDIF
          ENDIF    !(iconfig.eq.999)
        ENDIF    ! (ICT(icon).EQ.6)
   10 CONTINUE

      RETURN
      END


C ******************** BSFACS ********************
C BSFACS     Calcualte heat-loss factors for BASESIMP foundation.

      SUBROUTINE BSFACS(icon,intflg)
#include "building.h"

      COMMON/C3/IC1(MCON),IE1(MCON),ICT(MCON),IC2(MCON),IE2(MCON)
      COMMON/BSdata/bsHEIG(MCOM),bsDEPT(MCOM),bsLENG(MCOM),bsWIDT(MCOM),
     &              bsOVER(MCOM),bsRSI(MCOM),bsSOIL(MCOM),bsWTD(MCOM)
      COMMON/Dsumuo/BSa1,BSb1,BSc1,BSd1,BSe1,BSf1,BSg1,BSh1,BSi1,BSj1
      COMMON/Dsumur/BSq2,BSr2,BSu2,BSv2,BSw2,BSx2,BSs2,BSt2,BSy2,BSa2,
     &              BSb2,BSc2,BSd2,BSe2,BSf2,BSg2,BSh2
      COMMON/Datten/BSa3,BSb3,BSc3,BSe3,BSf3,BSg3,BSh3,BSi3
      COMMON/Dphase/BSa4,BSb4,BSc4
      COMMON/Dcorn/icol
      COMMON/BSCORN/coeff(16,19)
      COMMON/BSHLF/Sag(MCON),Sbgavg(MCON),Sbgvar(MCON),Bphase(MCON)

      real height,depth,length,width,overlp,rsi,soilk,wtable
      real dept,widt,rs,soil,wtabl,wilen,welen


C This subroutine calculates the BASESIMP heat-loss factors for the foundation
C corresponding to the connection under consideration. Data on the foundation
C (geometry, insulation, soil) are specified on the zone level (ie. one foundation
C per zone) and have been previously read in. Correlation coefficients for the
C BASESIMP configuration have been previously set. Correlation coefficients for the
C `corner-correction method' have been previously set. The four BASESIMP heat-loss
C factors are calculated and placed into common BSHLF for later use in the
C heat-loss calculations. The correlation coefficients and heat-loss factors are
C associated with the connection rather than the zone for the purposes of the
C calculations. However, since only a single foundation can be used in each
C zone, all BASESIMP connections within that zone will have the same correlation
C coefficients and heat-loss factors.


C Set independent variables used in the correlations to the data read
C from .bsm file belonging to zone under consideration.
      icomp = IC1(icon)
      height = bsHEIG(icomp)
      depth = bsDEPT(icomp)
      length = bsLENG(icomp)
      width = bsWIDT(icomp)
      overlp = bsOVER(icomp)
      rsi = bsRSI(icomp)
      soilk = bsSOIL(icomp)
      wtable = bsWTD(icomp)

C Adjust rsi if the "exponential-interpolation" correction method is active.
C intflg set by BSFCCTL.
      if(intflg.eq.99)then
C-------No change.
      elseif(intflg.eq.15)then
        rsi = 1.5
      elseif(intflg.eq.0)then
        rsi = 0.
      endif

C Check if user input rsi=0.  If so, set rsi to a small value to
C avoid `divide by zero' values in the sumuo, sumur, atten, and phase
C correlations.  A user input of rsi=0 signifies an uninsulated
C foundation.  The correlation coefficients for uninsulated foundations
C (eg. BCNN_1, SCN_1) nullify the rsi input (ie. any_value^0.=1.).
C In other words the correlations are insensitive to rsi.  However,
C some compilers result in 0.^0.=0., which causes `divide by zero' 
C problems.  This simply avoids this problem without affecting the
C results.
      if(rsi.lt.0.01) rsi=0.1

C Set icol if the BASESIMP system is BCCN_1 or BCCN_2.  These two systems
C do not have fixed icol as they do not correspond to any of the eight
C modelled for the corner-correction method.  icol is 4 if there is less
C than 0.6m of overlap; ip is 5 if there is greater than 0.6m of overlap
C and the exterior coverage is greater than the interior coverage; and
C ip is 3 if the interior coverage is greater than the exterior coverage.
      if(icol.eq.99) then
        wilen = (height-depth) + overlp
        welen = 0.1 + depth
        if((overlp/0.6).gt.0.9999) then
          if((welen/wilen).gt.1.0) then
            icol = 5
          else
            icol = 3
          end if
        else
          icol = 4
        end if
      end if

C Calculate sumuo.
      rpart1 = (BSa1+BSb1*(height-depth)+BSc1/soilk)/rsi**BSd1
      rpart2 = 1.0/
     &      (BSe1+BSi1*(overlp**BSf1)*(rsi**BSg1)*(height-depth)**BSh1)
      rpart3 = BSj1
      sumuo = rpart1*rpart2 + rpart3

C Calculate Sag.
      Sag(icon) = sumuo*2.*(length+width)
     
C Calculate sumur.
      rpart1 = (BSq2+BSr2*width)*(BSu2+BSv2*soilk)*(BSw2+BSx2*depth)
      rpart2 = (wtable)**(BSs2+BSt2*width+BSy2*depth)
      rpart3 = BSa2*(depth**BSb2)*(soilk**BSc2)
      rpart4 = (wtable**BSd2)*
     &         (rsi**(BSe2+BSf2*soilk+BSg2*depth+BSh2*overlp))
      sumur = (rpart1/rpart2)+(rpart3/rpart4)

C Calculate the steady corner factor.
      soil = soilk
      dept = depth
      wtabl = wtable
      rs = rsi
      widt = width
      if(rsi.gt.5.) rs=5.
      if(width.gt.10.) widt=10.
      if(depth.gt.2.) dept=2.
      wby2 = widt/2.
      if(icol.eq.98) then
         icol = 3
         rs = 0.
      end if
      iuse = 2*(icol-1) + 1  
      rr1 = coeff(iuse,1) + coeff(iuse,2)*rs +
     &     coeff(iuse,3)*soil + coeff(iuse,4)*wby2 +
     &     coeff(iuse,5)*dept + coeff(iuse,6)*wtabl
      rr2 = coeff(iuse,7)*rs**2 + coeff(iuse,8)*soil*rs +
     &     coeff(iuse,9)*wby2*rs + coeff(iuse,10)*wby2*soil +
     &     coeff(iuse,11)*wby2**2
      rr3 = coeff(iuse,12)*dept*rs + coeff(iuse,13)*dept*soil +
     &     coeff(iuse,14)*dept*wby2 + coeff(iuse,15)*dept**2
      rr4 = coeff(iuse,16)*wtabl*rs + coeff(iuse,17)*wtabl*soil +
     &     coeff(iuse,18)*wtabl*wby2 + coeff(iuse,19)*wtabl*dept
      Fcs = rr1 + rr2 + rr3 + rr4

C Calculate Sbgavg.
      Sbgavg(icon) = sumur*(2.*(length-width)+4.*Fcs*width)

C Calculate atten.
      rpart1 = BSa3+BSb3*soilk+BSc3*depth
      rpart2 = BSe3+BSf3*soilk+BSg3*depth
      rpart3 = rsi**(BSh3+BSi3*overlp)
      atten = rpart1 + rpart2/rpart3

C Calculate the variable corner factor.
      iuse = 2*(icol-1) + 2  
      rr1 = coeff(iuse,1) + coeff(iuse,2)*rs +
     &     coeff(iuse,3)*soil + coeff(iuse,4)*wby2 +
     &     coeff(iuse,5)*dept + coeff(iuse,6)*wtabl
      rr2 = coeff(iuse,7)*rs**2 + coeff(iuse,8)*soil*rs +
     &     coeff(iuse,9)*wby2*rs + coeff(iuse,10)*wby2*soil +
     &     coeff(iuse,11)*wby2**2
      rr3 = coeff(iuse,12)*dept*rs + coeff(iuse,13)*dept*soil +
     &     coeff(iuse,14)*dept*wby2 + coeff(iuse,15)*dept**2
      rr4 = coeff(iuse,16)*wtabl*rs + coeff(iuse,17)*wtabl*soil +
     &     coeff(iuse,18)*wtabl*wby2 + coeff(iuse,19)*wtabl*dept
      Fcv = rr1 + rr2 + rr3 + rr4

C Calculate Sbgvar.
      Sbgvar(icon) = atten*(2.*(length-width)+4.*width*Fcv)

C Calculate phase.
      Bphase(icon) = BSa4+BSb4/(rsi**BSc4)


      RETURN
      END


C ******************** BSTEMP ********************
C BSTEMP     Calcualtes basement air-point temperature to use as driving potential
C            in calculating heat transfer to BASESIMP foundation.

      SUBROUTINE BSTEMP(ICOMP,TBSAVG)
#include "building.h"

      COMMON/PVALA/TPA(MCOM),QPA(MCOM)
      COMMON/FVALA/TFA(MCOM),QFA(MCOM)
      common/simtim/ihrp,ihrf,idyp,idyf,idwp,idwf,nsinc,its
      COMMON/BTIME/BTIMEP,BTIMEF
      common/simsdy/iss,isf

      real Twithin(30*24)


C The medium-term average of the zone air-point temperature is used as the
C driving potential in calculating the heat transfer to the BASESIMP foundation.
C The medium-term average is used rather than the instantaneous temperature
C because high-frequency fluctuations in the indoor temperature will not penetrate
C deep into the soil. The influence of the high-frequency fluctuations is
C considered in the heat storage of the foundation's structure through ESP-r's
C standard solution approach. Refer to the summary of the HOT3000 Loads Workshop
C of July 1997 for further details.


C Time variables used in this subroutine:
C   IDYP   : `present' day, counting from January 1; set in MZNUMA.
C   IHRP   : `present' hour of day; set in MZNUMA.
C   IHRF   : `future' hour of day; set in MZNUMA.
C   BTIMEF : `future' hour as fraction, eg. 1.5 for 1h30; set in MZNUMA.
C   NSINC  : counts number of simulation time steps; =1 first step.
C   ISS    : day number of start of start-up period, counting from January 1;
C            set in MZNUMA.

C Initialize variables.
      Izone = 0
      Iinc = 0

C Averaging period, in hours (one week).
      iper = 24*7

C Initialize matrix holding history of zone temperatures the first time through.
      IF(NSINC.EQ.1) then
        do 90 i=1,iper
          Twithin(i) = 15.
   90   continue
      ENDIF

C If BSTEMP has already been called for the current time step (for this zone)
C then do not update the running average. Rather use the value calculated for
C the previous call.
      IF( .not.(NSINC.eq.Iinc .and. ICOMP.eq.Izone) ) THEN

C In order to avoid an excessively large `Twithin' matrix and to avoid unnecessary
C computations, the running average is updated on the hour (ie. every hour on the
C hour) rather than each time step. Are we on the hour?
        IF(ABS(BTIMEF-FLOAT(IHRF)).LT..0001) THEN

C Determine now many data points are currently used in the time-averaging
C (`dennum'). Normally this will equal `iper', but there will be fewer data points
C until we have simulated `iper' hours. `iISS' is used to adjust `ISS' when we
C cross over into the new year.
          iISS = ISS
          if(IDYP.lt.ISS) iISS=iISS-365
          if( IDYP.ge.(iISS+iper/24) ) then
            dennum = float(iper)
          else
            dennum = float( 24*(IDYP-iISS) + IHRP - 1 )
          endif

C If we have not simulated `iper' hours then append the current hour's
C temperature (average of `present' and `future' values) to the list.
C If we have simulated more than `iper' hours then drop the oldest data from the
C list and add the current hour's value.
          if( ABS(dennum-float(iper)) .gt. 0.001 ) then
            Twithin(ifix(dennum+1.)) = (TPA(ICOMP)+TFA(ICOMP))*.5
            dennum = dennum + 1.
          else
            do 100 i=1,ifix(dennum)-1
              Twithin(i) = Twithin(i+1)
  100       continue
            Twithin(ifix(dennum)) = (TPA(ICOMP)+TFA(ICOMP))*.5
          endif

C Calculate the new time-averaged temperature.
          total = 0.
          do 110 i=1,ifix(dennum)
            total = total + Twithin(i)
  110     continue
          TBSAVG = total/dennum

        ENDIF
C Set the flags to indicate that TBSAVG has been calculated for this zone, this
C time step.
        Izone = ICOMP
        Iinc = NSINC

      ENDIF
      RETURN
      END


C ******************** BSHEAT ********************
C BSHEAT     Calcualtes heat transfer to soil and outdoor air through the
C            BASESIMP foundation for the current time step.

      SUBROUTINE BSHEAT(ICOMP,ICON,TBSAVG,Qtotal)
#include "building.h"
#include "ground_temp_mains_common.h"

      COMMON/BSHLF/Sag(MCON),Sbgavg(MCON),Sbgvar(MCON),Bphase(MCON)
      COMMON/BSwth/bsTGavg(MCOM),bsTGamp(MCOM),bsTGps(MCOM)
      COMMON/CLIMI/QFP,QFF,TP,TF,QDP,QDF,VP,VF,DP,DF,HP,HF
      common/simtim/ihrp,ihrf,idyp,idyf,idwp,idwf,nsinc,its

C Local variables for basesimp ground temperature parameters
      REAL basesimp_grd_temp_amp    ! Ground temperature amplitude
      REAL basesimp_grd_temp_avg    ! Ground temperature average
      REAL basesimp_grd_temp_pshift ! Ground temperature phase shift

      character*128 H3Kname         ! holding variable

C Set constants used in calculations.
      PI=4.0*atan(1.0)
      omega = 2.*PI/(365.*24.)

C Above-grade heat loss (W). The outdoor air temperature is set in climi.F.
      Qag = Sag(icon) * (TBSAVG-TF)

C Set temperature for determining below grade heat loss
C In the case that the basesimp data file does not have a version number or
C version number is set to 0.
      if(Basesimp_grd_temp_cal_flag(icomp).eq.1) then
C In this case set basesimp ground temperature parameters to values in
C basesimp file
        basesimp_grd_temp_avg = bsTGavg(icomp)
        basesimp_grd_temp_amp = bsTGamp(icomp)
        basesimp_grd_temp_pshift = bsTGps(icomp)
      elseif(Basesimp_grd_temp_cal_flag(icomp).eq.2) then
C In the case that the basesimp data file has a version number > 0,
C then the ground temperature parameters calculated internally using
C weather file and Moore model.
        basesimp_grd_temp_avg = Temp_Ground_Avg
        basesimp_grd_temp_amp = Temp_Ground_Amp
        basesimp_grd_temp_pshift = Temp_Ground_phaseshift
      endif    
       
C Steady component of below-grade heat loss (W).
	  Qbgavg = Sbgavg(icon) * 
     &             ( TBSAVG-basesimp_grd_temp_avg )

C Time-varying component of below-grade heat loss (W). `time' measures the
C time in hours from the beginning of the calendar year.
      time = float(idyp-1)*24. + float(ihrp)
      Qbgvar = Sbgvar(icon) * basesimp_grd_temp_amp  *
     &         SIN( omega*time+Bphase(icon)-PI/2.-
     &              basesimp_grd_temp_pshift )

C Sum components of heat loss to return total heat transfer to BASESIMP foundation.
C Units of `Qtotal' are W.
      Qtotal = Qag + Qbgavg + Qbgvar

C--------------------------------------------
      write(H3Kname,'(A,A)')
     &  'basesimp',
     &  '/Qag'
      call add_to_xml_reporting(
     &   Qag,
     &   H3Kname,
     &   'units', '(W)',
     &   'Basesimp: Above ground heat loss'
     &   )

C--------------------------------------------
      write(H3Kname,'(A,A)')
     &     'basesimp',
     &     '/Qbgavg'
      call add_to_xml_reporting(
     &   Qbgavg,
     &   H3Kname,
     &   'units', '(W)',
     &   'Basesimp: Average below ground heat loss'
     &   )

C--------------------------------------------
      write(H3Kname,'(A,A)')
     &  'basesimp',
     &  '/Qbgvar'
      call add_to_xml_reporting(
     &   Qbgvar,
     &   H3Kname,
     &   'units', '(W)',
     &   'Basesimp: Variable below ground heat loss'
     &   )

C-----------------------------------------
      write(H3Kname,'(A,A)')
     &  'basesimp',
     &  '/Qtotal'
      call add_to_xml_reporting(
     &   Qtotal,
     &   H3Kname,
     &   'units', '(W)',
     &   'Basesimp: Total heat loss'
     &   )

C-----------------------------------------
      write(H3Kname,'(A,A)')
     &  'basesimp',
     &  '/TBSAVG'
      call add_to_xml_reporting(
     &   TBSAVG,
     &   H3Kname,
     &   'units', '(oC)',
     &   'Basesimp: Total heat loss'
     &   )

C-----------------------------------------
      write(H3Kname,'(A,A)')
     &  'basesimp',
     &  '/Tamb'
      call add_to_xml_reporting(
     &   TF,
     &   H3Kname,
     &   'units', '(oC)',
     &   'Basesimp: Ambient temperature'
     &   )

C-----------------------------------------
      write(H3Kname,'(A,A)')
     &  'basesimp',
     &  '/bsTGavg'
      call add_to_xml_reporting(
     &   basesimp_grd_temp_avg,
     &   H3Kname,
     &   'units', '(oC)',
     &   'Basesimp: Average ground temperature'
     &   )

C-----------------------------------------

      RETURN
      END


C ******************** BSCONV ********************
C BSCONV     Sets `inside' and `outside' convection coefficients for surface
C            with BASESIMP boundary conditions.


      SUBROUTINE BSCONV(ICOMP,ISUR)
#include "building.h"

      COMMON/CONCOE/HCIP(MCOM,MS),HCIF(MCOM,MS),HCOP(MCOM,MS),
     &              HCOF(MCOM,MS)

C Ensure heat from basement zone is transferred to the BASESIMP sink by setting
C HC on `inside' to large number and HC on `outside' to zero. Setting HCT
C on the `outside' to zero forces all heat `radiating' to BASESIMP foundation
C to come from the mlc, and not from the outdoor air via convection. The inside
C HC is set to a high number because a convection coefficient was assumed in
C the generation of the BASECALC results, so this avoids double couting this
C resistance to heat flow.

      HCIF(ICOMP,ISUR)=100.
      HCOF(ICOMP,ISUR)=0.

      RETURN
      END
