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

C MKBCLM: Allows a period of climatic data in a suitable ASCII file
C         to be transferred to a binary weather file.
C COLTOBCLM: Allows a period of column climatic data in a suitable ASCII file
C         to be transferred to a binary weather file.
C MKACLM: Allows any period of climatic data in binary weather file to
C         be transferred to an ASCII file.
C MKOREANCLM: Allows a period of climatic data in a suitable ASCII file
C         (KOREAN) to be transferred to a binary weather file.
C CFILIN: Takes a weather file name from a browse list, command
C         line or default and intialise it or open.
C CLMPUT: Writes one day's weather data to the current weather file.
C EPWTOBCLM: Allows a period of climatic data from EPW format ASCII file
C         to be transferred to a binary weather file.

C ******************** MKBCLM ******************
C Allows a period of weather data in an ASCII weather file
C to be transferred to a binary file. The ASCII file should 
C contain weather metric values for each hour as defined in
C climate.h.

C *** Version 2+ weather files ***

C Header lines should be:
C WEATHER [version]
C [site name]
C [year],[latitude],[longitude difference]
C [some text] | [metric 1 column number or 'excluded'] [maybe some more text]
C [some text] | [metric 2 column number or 'excluded'] [maybe some more text]
C [some text] | [metric 3 column number or 'excluded'] [maybe some more text]
C ... and so on for each metric

C Columns can be in any order, defined by header (but should generally
C be in metric order). Metrics are defined in header file climate.h.

C *** Old version 2+ weather files ***

C Header lines should be:
C CLIMATE 2
C [site name]
C [year],[latitude],[longitude difference]
C [comma seperated metric flags (0 or column number) for each metric]
C [period start julian day],[period finish julian day]

C Columns can be in any order, defined by 4th header line. Metrics are
C defined in header file climate.h.

C *** Old version 1 climate files ***

C Header lines should be:
C CLIMATE
C [site name]
C [year],[latitude],[longitude difference],[global horizontal flag]
C [period start julian day],[period finish julian day]

C Columns are in the order:
C diffuse horizontal solar
C dry bulb temperature
C direct normal solar OR global horizontal solar (according to flag)
C wind speed
C wind direction
C relative humidity

C Each data line is comma separated and all values should be integers.
C Dry bulb temperature and wind speed metrics have been multiplied by 10
C ie.  12.3 degrees is represented in the file as 123.  There should be
C 24 lines for each day of the period being transferred.

C Assume that the weather database (unit ICLIM) has already been opened
C elsewhere.

      SUBROUTINE MKBCLM(ITRU,ICDESC,LASCI,IUNIT,IDS,IDF,silent,IER)
#include "esprdbfile.h"
#include "climate.h"
#include "help.h"

      COMMON/OUTIN/IUOUT,IUIN,IEOUT

C Session log file (for errors etc.).
      character isfile*72
      common/logs/isfile

      COMMON/CLMDT1/CLMLOC
      COMMON/CLMSET/ICYEAR,ICDNGH,CLAT,CLONG
      COMMON/SET1/IYEAR,IBDOY,IEDOY,IFDAY,IFTIME
      COMMON/RADTYP/IDNGH
      LOGICAL OK,silent
      dimension ICLM(24,MCM)

      CHARACTER CLMLOC*42
      CHARACTER LASCI*96,outs*124,OUTSTR*124,a*2
      integer IREAD  ! set to 1 if day demarcation lines

      integer CFIND(MCM) ! Metric indices of each column in the file

      helpinsub='cfiles'  ! set for cfiles

C Open the ASCII file for reading.
      if(iunit.eq.0)then
        call usrmsg(' Problem with file unit for...',LASCI,'W')
        ier = 1
        return
      endif

      CALL EFOPSEQ(IUNIT,LASCI,1,IER)
      if(ier.ne.0)then
        call usrmsg('Problem opening ',LASCI,'W')
        ier = 1
        return
      endif

      IREAD=0  ! assume no day demarcation lines
      if(silent)then
        IREAD=1
      else
        helptopic='confirm_marker_line'
        call gethelptext(helpinsub,helptopic,nbhelp)
        CALL EASKOK(' ',
     &    'Does file have day demarcation lines?',OK,nbhelp)
        IF(OK)IREAD=1
      endif

C Check and see if first line is *CLIMATE or *WEATHER, if so read in
C site data. If the user is not requesting new site data then skip over
C lines. 
      CALL STRIPC(IUNIT,OUTSTR,99,ND,1,'line 1',IER)
      if (ND.eq.2 .and. OUTSTR(1:9).eq.'*WEATHER ') then

C Check version number.
        K=8
        CALL EGETWI(OUTSTR,K,IVER,2,NMCM,'F','weather file version',IER)

C Set version number and number of metrics.
        CFVER=IVER
        CFMCM=MCMALL(CFVER)

        if(ICDESC.NE.1)then
          CALL STRIPC(IUNIT,OUTSTR,0,ND,1,'weather head 1',IER)
          CALL STRIPC(IUNIT,OUTSTR,0,ND,1,'weather head 2',IER)
          do i=1,CFMCM
            CALL STRIPC(IUNIT,OUTSTR,0,ND,1,'weather param head',IER)
          enddo
        elseif(ICDESC.eq.1)then

C Header line 1: location          
          CALL STRIPC(IUNIT,OUTSTR,0,ND,1,'weather head 1',IER)
          CFLOC=OUTSTR(1:42)

C Header line 2: year, latitude, longitude difference
          CALL STRIPC(IUNIT,OUTSTR,3,ND,1,'weather head 2',IER)
          K=0
          CALL EGETWI(OUTSTR,K,CFYEAR,1900,2051,'W','year',IER)
          CALL EGETWR(OUTSTR,K,CFLAT,-90.,90.,'W','CLAT',IER)
          CALL EGETWR(OUTSTR,K,CFLONG,-15.,15.,'W','CLONG',IER)

C Header lines 3+: metric description | column or 'excluded' | unit
          do i=1,CFMCM
            CALL STRIPC(IUNIT,OUTSTR,0,ND,1,'weather head 2',IER)
            K=0

C Get column field.
            outs=OUTSTR(30:38)
            if (outs(1:8).eq.'excluded') then
              CMCOL(i)=0
              CMXST(i)=.false.
            else
              read(outs(1:lnblnk(outs)),*)icol
              if (icol.lt.1.or.icol.gt.CFMCM) then                
                write(outs,'(a,i2.2)')
     &            'Invalid column for weather parameter ',i
                call edisp(iuout,outs)
                CALL ERPFREE(IUNIT,ISTAT)
                RETURN
              endif
              CMCOL(i)=icol
              CMXST(i)=.true.
              NCM=NCM+1
              CFIND(icol)=i
            endif
          enddo

C Check we have at least the required metrics.
          CALL CLMCHKMET(IER)
          if (IER.ne.0) goto 1003

C Period set elsewhere.
C          IDS=1
C          IDF=365
        endif

      elseif (ND.eq.2 .and. OUTSTR(1:9).eq.'*CLIMATE ') then

C Check version number.
        K=8
        CALL EGETWI(OUTSTR,K,IVER,2,NMCM,'F','clm dbs version',IER)

C Currently, only version number available is 2; otherwise if there is
C no version number it's version 1.
        CFVER=IVER
        CFMCM=MCMALL(CFVER)
        if(ICDESC.NE.1)then
          CALL STRIPC(IUNIT,OUTSTR,0,ND,1,'clm head 1',IER)
          CALL STRIPC(IUNIT,OUTSTR,0,ND,1,'clm head 2',IER)
          CALL STRIPC(IUNIT,OUTSTR,0,ND,1,'clm head 3',IER)
          CALL STRIPC(IUNIT,OUTSTR,0,ND,1,'clm head 4',IER)
        elseif(ICDESC.eq.1)then

C Header line 1: location          
          CALL STRIPC(IUNIT,OUTSTR,0,ND,1,'clm head 1',IER)
          CFLOC=OUTSTR(1:42)

C Header line 2: year, latitude, longitude difference
          CALL STRIPC(IUNIT,OUTSTR,3,ND,1,'clm head 2',IER)
          K=0
          CALL EGETWI(OUTSTR,K,CFYEAR,1900,2051,'W','year',IER)
          CALL EGETWR(OUTSTR,K,CFLAT,-90.,90.,'W','CLAT',IER)
          CALL EGETWR(OUTSTR,K,CFLONG,-15.,15.,'W','CLONG',IER)

C Header line 3: metric flags
          CALL STRIPC(IUNIT,OUTSTR,CFMCM,ND,1,'clm head 3',IER)
          K=0
          NCM=0
          do i=1,CFMCM
            write(outs,'(a,i2,a)')'clm met ',i,' flag'
            CALL EGETWI
     &        (OUTSTR,K,CMCOL(i),0,CFMCM,'W',outs(1:lnblnk(outs)),IER)
            if (CMCOL(i).gt.0) then
              CMXST(i)=.true.
              NCM=NCM+1
              CFIND(CMCOL(i))=i
            else
              CMXST(i)=.false.
            endif
          enddo
          if (MCM.gt.CFMCM) then
            do i=CFMCM+1,MCM
              CMCOL(i)=0
              CMXST(i)=.false.
            enddo
          endif

C Check we have at least the required metrics.
          CALL CLMCHKMET(IER)
          if (IER.ne.0) goto 1003

C Header line 4: period
          CALL STRIPC(IUNIT,OUTSTR,2,ND,1,'clm head 4',IER)
          K=0
          CALL EGETWI(OUTSTR,K,IDS,1,365,'W','clm start',IER)
          CALL EGETWI(OUTSTR,K,IDF,1,365,'W','clm end',IER)
        endif

      elseif(ND.eq.1 .and. OUTSTR(1:8).eq.'*CLIMATE')then
        CFVER=1
        CFMCM=MCMALL(CFVER)
        if(ICDESC.NE.1)then
          CALL STRIPC(IUNIT,OUTSTR,0,ND,1,'clm head 1',IER)
          CALL STRIPC(IUNIT,OUTSTR,0,ND,1,'clm head 2',IER)
          CALL STRIPC(IUNIT,OUTSTR,0,ND,1,'clm head 3',IER)
        elseif(ICDESC.eq.1)then
          CALL STRIPC(IUNIT,OUTSTR,0,ND,1,'clm head 1',IER)
          CFLOC=OUTSTR(1:42)

          CALL STRIPC(IUNIT,OUTSTR,99,ND,1,'clm head 2',IER)
          K=0
          CALL EGETWI(OUTSTR,K,CFYEAR,1900,2051,'W','year',IER)
          CALL EGETWR(OUTSTR,K,CFLAT,-90.,90.,'W','CLAT',IER)
          CALL EGETWR(OUTSTR,K,CFLONG,-15.,15.,'W','CLONG',IER)
          CALL EGETWI(OUTSTR,K,IDNGH,0,123,'W','IDNGH',IER)

          CALL STRIPC(IUNIT,OUTSTR,2,ND,1,'clm head 3',IER)
          K=0
          CALL EGETWI(OUTSTR,K,IDS,1,365,'W','clm start',IER)
          CALL EGETWI(OUTSTR,K,IDF,1,365,'W','clm end',IER)
        endif

        NCM=6
        CMCOL(1:2)=(/2,1/)
        if (IDNGH.eq.123) then
          CMCOL(3:4)=(/0,3/)
        else
          CMCOL(3:4)=(/3,0/)
        endif
        CMCOL(5:7)=(/4,5,6/)
        do k=8,MCM
          CMCOL(k)=0
        enddo

      else
        CFVER=1
        CFMCM=MCMALL(CFVER)

C First line was not *CLIMATE so original style, back up and to on to
C standard data reads.
        rewind(iunit,ERR=1001)

C If user has requested that new site information be inserted ask
C such questions now.
        IF(ICDESC.EQ.1)then

C Define the year if not already done.
          helptopic='define_year'
          call gethelptext(helpinsub,helptopic,nbhelp)
          CALL EASKI(CFYEAR,' ','Year?',
     &             1900,'W',2015,'W',2019,'year',IER,nbhelp)

          CFLOC=' '
          helptopic='define_station_name'
          call gethelptext(helpinsub,helptopic,nbhelp)
          CALL EASKS(CFLOC,' ','Weather station name?',
     &      42,'North_Pole','weather site. ',IER,nbhelp)

          helptopic='define_latitude'
          call gethelptext(helpinsub,helptopic,nbhelp)
          CALL EASKR(CFLAT,' ','Station latitude?',
     &      -89.9,'W',89.9,'W',30.0,'weather latitude',IER,nbhelp)
          
          helpinsub='common' ! get from common.help
          helptopic='define_longitude_diff'
          call gethelptext(helpinsub,helptopic,nbhelp)
          helpinsub='cfiles'  ! set for cfiles
          CALL EASKR(CFLONG,' ',
     &      'Station longitude difference?',
     &      -14.9,'W',14.9,'W',0.0,'weather long',IER,nbhelp)
        endif

        NCM=6
        CMCOL(1:2)=(/2,1/)
        if (IDNGH.eq.123) then
          CMCOL(3:4)=(/0,3/)
        else
          CMCOL(3:4)=(/3,0/)
        endif
        CMCOL(5:7)=(/4,5,6/)
        do k=8,MCM
          CMCOL(k)=0
        enddo
      endif

C Loop through days.
      DO 10 I=IDS,IDF
        IDD=I
        IF(IREAD.EQ.1)then
          CALL STRIPC(IUNIT,OUTSTR,0,ND,1,'day ln',IER)
        endif
        DO 20 J=1,24

C Loop through metrics and read them in.
          CALL STRIPC(IUNIT,OUTSTR,NCM,ND,1,'weather data',IER)

          if (CFVER.ge.2) then
            K=0
            do icol=1,MCM
              if (icol.gt.NCM) then
                ICLM(j,icol)=0
              else
                imet=CFIND(icol)
                write(outs,'(a,i2,a)')'clm col ',icol,' val'
                CALL EGETWI(OUTSTR,K,IVAL,CFMIN(imet),CFMAX(imet),'W',
     &            outs(1:lnblnk(outs)),IER)
                ICLM(J,icol)=IVAL
              endif
            enddo
          else

C Read one hour's data from ASCII file.
            K=0
            CALL EGETWI(OUTSTR,K,IDIF,0,0,'-','diffuse',IER)
            CALL EGETWI(OUTSTR,K,ITMP,0,0,'-','temperature',IER)
            CALL EGETWI(OUTSTR,K,IDNR,0,0,'-','direct',IER)
            CALL EGETWI(OUTSTR,K,IVEL,0,0,'-','velocity',IER)
            CALL EGETWI(OUTSTR,K,IDIR,0,0,'-','direction',IER)
            CALL EGETWI(OUTSTR,K,IHUM,0,0,'-','humidity',IER)

C Since going from integer ESP units to ESP units in binary file no
C need to worry about truncation errors.
            ICLM(J,1)=IDIF
            ICLM(J,2)=ITMP
            ICLM(J,3)=IDNR
            ICLM(J,4)=IVEL
            ICLM(J,5)=IDIR
            ICLM(J,6)=IHUM
            do icol=7,MCM
              ICLM(J,icol)=0
            enddo

          endif
   20   CONTINUE

C Transfer this to the binary file.
        IREC=IDD
        WRITE(ICLIM,REC=IREC,IOSTAT=ISTAT,ERR=1002)
     &       ((ICLM(J,K),K=1,MCM),J=1,24)
   10 CONTINUE

C Insert climatic year.
      IREC=366
      WRITE(ICLIM,REC=IREC,IOSTAT=ISTAT,ERR=1001)CFYEAR

C Insert location, latitude and longitude.
      IREC=IREC+1
      WRITE(ICLIM,REC=IREC,IOSTAT=ISTAT,ERR=1001)
     &  CFLOC,CFLAT,CFLONG

C Insert metric flags.
      IREC=IREC+1
      WRITE(ICLIM,REC=IREC,IOSTAT=ISTAT,ERR=1001)
     &  (CMCOL(i),i=1,MCM)

C Insert version - use latest version for the database no matter what
C version of ASCII we were given.
      CFVER=NMCM
      CFMCM=MCM
      IREC=IREC+1
      WRITE(ICLIM,REC=IREC,IOSTAT=ISTAT,ERR=1001)CFVER

      CALL CLMMDN2O

C Close ASCIIfile.
      CALL ERPFREE(IUNIT,ISTAT)
      RETURN

 1001 call edisp(ieout,outs)
      WRITE(outs,775)ISTAT
  775 FORMAT(' Error ',I7,' writing weather database metadata')
      call edisp(ieout,outs)
      CALL ERPFREE(IUNIT,ISTAT)
      RETURN

 1002 call edisp(iuout,outs)
      WRITE(outs,777)ISTAT
  777 FORMAT(' Error ',I7,' writing weather database')
      call edisp(iuout,outs)
      CALL ERPFREE(IUNIT,ISTAT)
      RETURN

 1003 outs=' Error: weather file does not include all required metrics'
      call edisp(iuout,outs)
      CALL ERPFREE(IUNIT,ISTAT)
      RETURN

      END


C 'COLTOBCLM' Allows a period of column weather data in a suitable
C ASCII file to be transferred to a binary weather file.  The ASCII file
C should have columns of data (integers or reals) for at least diffuse
C solar, direct or global solar, dry bulb, wind speed, wind direction
C and RH. The user is asked which column holds which data as well as the
C units of the data column. Columns can be space or tab or comma
C separated.

C For solar radiation units can be W/m2 real, W/m2 integer, integer
C tenths W/m2 or (sum direct+diffuse) KJ integer or real using one of
C three methods to evaluate the diffuse component.
C For dry bulb degrees C real, integer tenths deg C
C For wind speed m/s real, m/s integer, integer tenths m/s, knots
C integer or real, km/hr real.
C For wind direction degrees from north real or integer, integer tenths
C degrees
C For RH  % real, tenths % integer, between 0. and 1.
C For atmospheric pressure Pa, kPa, hPa, bar, psi atm.

C At the end of the process we have at least ...

C contain the following values for each hour:
C col     description                 unit            variable
C 1. External dry bulb temperature   (TENTHS C)       ITMP
C 2. Diffuse solar on the horizontal (W/m^2)          IDIF
C 3. Direct normal solar intensity   (W/m^2)          IDNR
C    or
C    Global horizontal radiation     (W/m^2)
C 4. Prevailing wind speed           (tenths m/s)     IVEL
C 5. Wind direction                  (degrees)        IDIR
C 6. Relative humidity               (%)              IHUM

C ... plus any additional parameters (see header climate.h for
C definitions).

C Assume that the weather file (unit ICLIM) has already been opened
C elsewhere.

      SUBROUTINE COLTOBCLM(ITRU,LASCI,IUNIT,IDS,IDF,IER)
#include "esprdbfile.h"
#include "espriou.h"
#include "climate.h"
#include "epara.h"
#include "help.h"

      COMMON/OUTIN/IUOUT,IUIN,IEOUT

      COMMON/CLMDT1/CLMLOC
      COMMON/CLMSET/ICYEAR,ICDNGH,CLAT,CLONG
      COMMON/SET1/IYEAR,IBDOY,IEDOY,IFDAY,IFTIME
      COMMON/RADTYP/IDNGH
      COMMON/APPRDIFF/iapprox_diff
      LOGICAL OK,silent
      dimension ICLM(24,MCM)

      CHARACTER CLMLOC*42
      CHARACTER DFILE*96
      CHARACTER LASCI*96,outs*124,OUTSTR*124,msgl2*48
      CHARACTER IMPT*50,KEY*1,units*20
      DIMENSION IMPT(35),IMSTC(MCM),ITUNITS(MCM),MITUNITS(MCM)
      character word*32  ! for skipped column data
      integer iwidest    ! the last column to look in
      integer IREAD  ! set to 1 if day demarcation lines
      real longit_for_diffuse_calcs,differ_from_Greenwich
      real dbtc,rval
      integer ival
      logical btog,bcol

      helpinsub='cfiles'  ! set for cfiles

C Initialiase defaults for easkr in the case of single dir+dif solar value
      longit_for_diffuse_calcs=0.
      differ_from_Greenwich=clong

C Open the ASCII file for reading.
      if(iunit.eq.0)then
        call usrmsg('Problem with file unit for ',LASCI,'W')
        ier = 1
        return
      endif

      CALL EFOPSEQ(IUNIT,LASCI,1,IER)
      if(ier.ne.0)then
        call usrmsg('Problem opening ',LASCI,'W')
        ier = 1
        return
      endif

C Clear column association.
      DO 27 I=1,7
        IMSTC(I)=0
        ITUNITS(I)=0
   27 CONTINUE
      IMSTL=1
      iwidest=0; IREAD=0
      if(IDS.eq.0)IDS=1    ! if not already set
      if(IDF.eq.0)IDF=365 

C Display the initial lines of the file for the user.

C Actual read of ASCII import file.
      K=0
    7 READ(IUNIT,'(a)',IOSTAT=ISTAT,END=102)OUTSTR
      K=K+1
      CALL  EDISP(IUOUT,OUTSTR)
      IF(K.LT.8)GOTO 7
      CALL EDISP(IUOUT,' ')
      CALL ERPFREE(IUNIT,ISTAT)

C End of file encountered, close the file and return to menu.
  102 msgl2=' End of file reached'
      CALL USRMSG(' ',msgl2,'-')
      CALL ERPFREE(IUNIT,ISTAT)

C Page this menu if needed.
      MHEAD=5
 28   WRITE(IMPT(1),'(A,A)') '1 Import file: ',LASCI(1:26)
      WRITE(IMPT(2),'(A,I3)') '2 Begin reading @ line: ',IMSTL
      if(IREAD.eq.0)then
        WRITE(IMPT(3),'(A)') '3 Demarcation lines: none'
      else
        WRITE(IMPT(3),'(A)') '3 Demarcation lines: yes skip'
      endif
      write(IMPT(4),'(A,A)') '4 Enter site data'
      IMPT(5) =      '  - - Column associations & units: - - - '

      ILEN=MCM*2
      MCTL=5
      IPACT=CREATE
      CALL EKPAGE(IPACT)

C Loop through the items until the page to be displayed. M is the
C current menu line index. Build up text strings for the menu.
      M=MHEAD
      btog=.true.
      imet=1
      DO L=1,ILEN
        IF(L.GE.IST.AND.(L.LE.(IST+MIFULL)))THEN
          M=M+1
          CALL EMKEY(L,KEY,IER)
          if (btog) then
            write(IMPT(M),'(a,1x,a,1x,a,1x,i2)')KEY,
     &        CMNAMF(imet)(1:lnblnk(CMNAMF(imet))),'column:',IMSTC(imet)
            btog=.false.
          else

C Different units for different metrics.
            if (imet.eq.1) then ! dry bulb temperature
              if(ITUNITS(imet).eq.0)then
                units='C real              '
              elseif(ITUNITS(imet).eq.1)then
                units='C integer           '
              elseif(ITUNITS(imet).eq.2)then
                units='tenths C integer    '
              endif
              MITUNITS(imet)=2

            elseif (imet.eq.2 .or. imet.eq.3) then ! diffuse or direct solar
              if(ITUNITS(imet).eq.0)then
                units='W/m^2 real          '
              elseif(ITUNITS(imet).eq.1)then
                units='W/m^2 integer       '
              elseif(ITUNITS(imet).eq.2)then
                units='tenths W/m^2 integer'
              endif
              MITUNITS(imet)=2

            elseif (imet.eq.4) then ! global horizontal solar
              if(ITUNITS(imet).eq.0)then
                units='W/m^2 real          '
              elseif(ITUNITS(imet).eq.1)then
                units='W/m^2 integer       '
              elseif(ITUNITS(imet).eq.2)then
                units='tenths W/m^2 integer'
              elseif(ITUNITS(imet).eq.3)then
                units='dir+dif KJ/m^2 real '
              elseif(ITUNITS(imet).eq.4)then
                units='dir+dif KJ/m^2 int  '
              endif
              MITUNITS(imet)=4

            elseif (imet.eq.5) then ! wind speed
              if(ITUNITS(imet).eq.0)then
                units='m/s real            '
              elseif(ITUNITS(imet).eq.1)then
                units='m/s integer         '
              elseif(ITUNITS(imet).eq.2)then
                units='tenths m/s integer  '
              elseif(ITUNITS(imet).eq.3)then
                units='knots real          '
              elseif(ITUNITS(imet).eq.4)then
                units='knots integer       '
              elseif(ITUNITS(imet).eq.5)then
                units='km/hr real       '
              endif
              MITUNITS(imet)=5

            elseif (imet.eq.6) then ! wind direction
              if(ITUNITS(imet).eq.0)then
                units='degrees real        '
              elseif(ITUNITS(imet).eq.1)then
                units='degrees integer     '
              elseif(ITUNITS(imet).eq.2)then
                units='tenths deg integer  '
              endif
              MITUNITS(imet)=2

            elseif (imet.eq.7) then ! relative humidity
              if(ITUNITS(imet).eq.0)then
                units='% real              '
              elseif(ITUNITS(imet).eq.1)then
                units='% integer           '
              elseif(ITUNITS(imet).eq.2)then
                units='tenths % integer    '
              elseif(ITUNITS(imet).eq.3)then
                units='0.0 to 1.0 real     '
              endif
              MITUNITS(imet)=3

            elseif (imet.eq.8) then ! total cloud cover
              if(ITUNITS(imet).eq.0)then
                units='tenths real         '
              elseif(ITUNITS(imet).eq.1)then
                units='tenths integer      '
              elseif(ITUNITS(imet).eq.2)then
                units='0.0 to 1.0 real     '
              endif
              MITUNITS(imet)=2

            elseif (imet.eq.9) then ! opaque cloud cover
              if(ITUNITS(imet).eq.0)then
                units='tenths real         '
              elseif(ITUNITS(imet).eq.1)then
                units='tenths integer      '
              elseif(ITUNITS(imet).eq.2)then
                units='0.0 to 1.0 real     '
              endif
              MITUNITS(imet)=2

            elseif (imet.eq.10) then ! atmospheric pressure
              if(ITUNITS(imet).eq.0)then
                units='Pa real             '
              elseif(ITUNITS(imet).eq.1)then
                units='Pa integer          '
              elseif(ITUNITS(imet).eq.2)then
                units='kPa real            '
              elseif(ITUNITS(imet).eq.3)then
                units='kPa integer         '
              elseif(ITUNITS(imet).eq.4)then
                units='tenths kPa integer  '
              elseif(ITUNITS(imet).eq.5)then
                units='hPa real            '
              elseif(ITUNITS(imet).eq.6)then
                units='bar real            '
              elseif(ITUNITS(imet).eq.7)then
                units='psi real            '
              elseif(ITUNITS(imet).eq.8)then
                units='atm real            '
              endif
              MITUNITS(imet)=8

            endif

            write(IMPT(M),'(a,1x,a,1x,a,1x,a)')KEY,
     &        CMNAMF(imet)(1:lnblnk(CMNAMF(imet))),'>>',units

            btog=.true.
            imet=imet+1
          endif
        endif
      enddo

      IMPTL=M+MCTL
      
C If a long list include page facility text.
      if (IPFLG.eq.0) then
        IMPT(M+1)='  __________________________________   '
      else
        write(IMPT(M+1),'(a,i2,a,i2)')'0 page --- part: ',IPM,' of ',MPM
      endif
      IMPT(M+2) = '! View start of import file            '
      IMPT(M+3) = '< Import data                          '
      IMPT(M+4) = '? Help                                 '
      IMPT(M+5) = '- Exit                                 '

C Help text for this menu.
      helptopic='clm_import_param'
      call gethelptext(helpinsub,helptopic,nbhelp)
 
      INODA=-2
      CALL EMENU('  Import specs',IMPT,IMPTL,INODA)

      IF(INODA.EQ.IMPTL)THEN
        RETURN

      ELSEIF(INODA.EQ.IMPTL-1)THEN
        helptopic='clm_import_param'
        call gethelptext(helpinsub,helptopic,nbhelp)
        CALL PHELPD('period fields',nbhelp,'-',0,0,IER)

      ELSEIF(INODA.EQ.1)THEN
        DFILE='data.imp'
        CALL EASKS(LASCI,'Import file name?',
     &   ' ',96,DFILE,'import file name',IER,nbhelp)

      ELSEIF(INODA.EQ.2)THEN

C Specify that read of import file begins at (non_comment) line IMSTL.
C Later extend logic to deal with specific file types such as timestep
C casual gain files.
        IV=IMSTL
        CALL EASKI(IV,' ','Begin reading @ import file line no:',
     &    0,'F',8760,'W',1,'begin read @',IER,nbhelp)
        IMSTL=IV
      ELSEIF(INODA.EQ.3)THEN

C Confirm if any day demarcation lines.
        helptopic='confirm_marker_line'
        call gethelptext(helpinsub,helptopic,nbhelp)
        CALL EASKOK(' ',
     &    'Does ASCII file have day demarcation lines?',OK,nbhelp)
        IF(OK)IREAD=1
      ELSEIF(INODA.EQ.4)THEN

C Define the year if not already done.
        helptopic='define_year'
        call gethelptext(helpinsub,helptopic,nbhelp)
        CALL EASKI(CFYEAR,' ','Year?',
     &           1900,'W',2050,'W',2019,'year',IER,nbhelp)

        CFLOC=' '
        helptopic='define_station_name'
        call gethelptext(helpinsub,helptopic,nbhelp)
        CALL EASKS(CFLOC,' ','Station name?',
     &    42,'North_Pole','weather site. ',IER,nbhelp)

        helptopic='define_latitude'
        call gethelptext(helpinsub,helptopic,nbhelp)
        CALL EASKR(CFLAT,' ','Station latitude?',
     &    -89.9,'W',89.9,'W',57.0,'weather latitude',IER,nbhelp)

        helptopic='define_longitude_diff'
        call gethelptext(helpinsub,helptopic,nbhelp)
        CALL EASKR(CFLONG,' ',
     &    'Station longitide difference?',
     &    -14.9,'W',14.9,'W',0.0,'weather long',IER,nbhelp)

        helptopic='define_day'
        call gethelptext(helpinsub,helptopic,nbhelp)
        CALL EASKI(IDS,' ','Start day?',
     &    1,'W',364,'W',1,'start day-of-year',IER,nbhelp)

        CALL EASKI(IDF,' ','Finish day?',
     &    2,'W',365,'W',365,'end day-of-year',IER,nbhelp)
        
      ELSEIF(INODA.EQ.5)THEN
        continue

      ELSEIF(INODA.GT.MHEAD .and. INODA.LE.(IMPTL-MCTL))THEN

C Item has been selected. Decode via KEYIND.
        CALL KEYIND(IMPTL,INODA,iind,io)
        if (MOD(iind,2).gt.0) then
          imet=iind/2+1
          bcol=.true.
        else
          imet=iind/2
          bcol=.false.
        endif

C Ask user for the column number.
        if (bcol) then
          write(outs,'(3a)')'Column for ',CMNAMF(imet),':'
          write(OUTSTR,'(2a)')CMNAMA(imet),' column'
          IV=IMSTC(imet)
          CALL EASKI(IV,' ',outs,0,'F',60,'W',1,OUTSTR,IER,nbhelp)
          IMSTC(imet)=IV
          if(iwidest.lt.IV) iwidest=IV  ! remember how many columns

C Change units.
        else
          ITUNITS(imet)=ITUNITS(imet)+1
          if(ITUNITS(imet).gt.MITUNITS(imet)) ITUNITS(imet)=0
        endif

      ELSEIF(INODA.EQ.IMPTL-4)THEN

C If long list, allow paging.
        IF(IPFLG.EQ.1)THEN
          IPACT=EDIT
          CALL EKPAGE(IPACT)
        ENDIF
      ELSEIF(INODA.EQ.IMPTL-3)THEN
        CALL EFOPSEQ(IUNIT,LASCI,1,IER)
        k=0
        goto 7
      ELSEIF(INODA.EQ.IMPTL-2)THEN

C If we are getting global horizontal, and using an algorithm to
C approximate diffuse from this, we need to confirm the longitude and
C time difference and ask which algorithm to use.
        if (IMSTC(4).gt.0) then
          IDNGH=123
          if (ITUNITS(4).eq.3 .or. ITUNITS(4).eq.4) then
            helptopic='define_latitude'
            call gethelptext(helpinsub,helptopic,nbhelp)
            CALL EASKR(longit_for_diffuse_calcs,' ',
     &       'Weather station longitude?',
     &       -180.0,'W',180.0,'W',30.0,'weather longitude',IER,nbhelp)

            helptopic='define_longitude_diff'
            call gethelptext(helpinsub,helptopic,nbhelp)
            CALL EASKR(differ_from_Greenwich,' ',
     &        'Weather station longitude difference?',
     &        -24.0,'W',24.0,'W',0.0,'time_difference',IER,nbhelp)

C Export weather data to ASCII file (with similar name).
            helptopic='korea_met_conversion'
            call gethelptext(helpinsub,helptopic,nbhelp)
            call easkabc
     &       (' ','Diffuse estimation model options:',
     &       'Muneer','Maxwell','Erbs Klein & Duffie',
     &       IDIFF,nbhelp)

            helptopic='clm_import_param'

C Set the global variable iapprox_diff to tell underlying calculation
C method which equation set to use.
            if(IDIFF.eq.1)then
              iapprox_diff=1
            elseif(IDIFF.eq.2)then  
              iapprox_diff=2
            elseif(IDIFF.eq.3)then  
              iapprox_diff=3
            endif
          endif
        endif

C Set up CMCOL and CMXST, and initialise ICLM.
        icol=0
        do imet=1,MCM
          do i=1,24
            ICLM(i,imet)=0
          enddo
          if (IMSTC(imet).gt.0) then
            icol=icol+1
            CMCOL(imet)=icol
            CMXST(imet)=.true.
          else
            CMCOL(imet)=0
            CMXST(imet)=.false.
          endif
        enddo
        if (CMXST(4) .and. .not.CMXST(2) .and.
     &    (ITUNITS(4).eq.3 .or. ITUNITS(4).eq.4)) then
          CMXST(2)=.true.
          CMCOL(2)=icol+1
        endif

C If user has given both direct normal and global horizontal, ignore
C global horizontal.
        if (CMXST(3) .and. CMXST(4)) then
          write(outs,'(2a)')'You have specified both direct normal ',
     &      'solar and global horizontal solar.'
          call edisp(iuout,outs)
          write(outs,'(2a)')'The latter is superfluous, and will be ',
     &      'omitted from the resulting weather database.'
          call edisp(iuout,outs)
          CMXST(4)=.false.
          do imet=1,MCM
            if (CMXST(imet) .and. CMCOL(imet).gt.CMCOL(4)) then
              CMCOL(imet)=CMCOL(imet)-1
            endif
          enddo
          CMCOL(4)=0
        endif

C Check we have all the required metrics.
        call CLMCHKMET(IER)
        if (IER.ne.0) then
          ok=.false.
          helptopic='not_enough_metrics'
          call gethelptext(helpinsub,helptopic,nbhelp)
          call EASKOK(
     &      'Not enough parameters for a usable weather file!',
     &      'Continue?',ok,nbhelp)

          helptopic='clm_import_param'
          IER=0
          if (.not.ok) goto 28
        endif

C Re-open the file and read how ever many lines prior to the data
C that the user requested.
        CALL EFOPSEQ(IUNIT,LASCI,1,IER)
        write(currentfile,'(a)') LASCI(1:lnblnk(LASCI))
        N=0
   70   N=N+1
        IF(N.GE.IMSTL) GOTO 71
        CALL STRIPC(IUNIT,OUTSTR,0,ND,1,'line 1',IER)
        GOTO 70

C Loop through days.
   71   DO 10 I=1,365
          IDD=I

C If we are before the start day, or after the end day, just write
C zeros.
          if (IDD.lt.IDS .or. IDD.gt.IDF) then
            do J=1,24
              do K=1,MCM
                ICLM(J,K)=0
              enddo
            enddo
            goto 21    ! jump past the do 20 loop
          endif

          IF(IREAD.EQ.1)then
            CALL STRIPC(IUNIT,OUTSTR,0,ND,1,'day ln',IER)
          endif
          DO 20 J=1,24

C Read one hour's data from ASCII file into outstr. Loop through
C each of the possible columns and if it matches one of the weather
C data columns defined by the user read and convert to the correct
C data format. If not a match just read the word and try the 
C next column. If reached EOF then jump to end of loop.
            CALL STRIPC(IUNIT,OUTSTR,0,ND,1,'weather data',IER)
            if(ier.ne.0) goto 20  ! EOF detected
            K=0
            do 6 N1=1,iwidest

C Different units for different metrics.
              if(N1.eq.IMSTC(1))then
                if(ITUNITS(1).eq.0)then
                  CALL EGETWR(OUTSTR,K,rval,0.0,0.0,'-',
     &              'deg C real',IER)
                  ICLM(J,CMCOL(1))= NINT(rval*10.0)
                elseif(ITUNITS(1).eq.1)then
                  CALL EGETWI(OUTSTR,K,ival,0,0,'-','deg C int',IER)
                  ICLM(J,CMCOL(1))=ival*10
                elseif(ITUNITS(1).eq.2)then
                  CALL EGETWI(OUTSTR,K,ival,0,0,'-','10th deg C',IER)
                  ICLM(J,CMCOL(1))=ival  ! no conversion needed
                endif

              elseif(N1.eq.IMSTC(2))then
                if(ITUNITS(2).eq.0)then
                  CALL EGETWR(OUTSTR,K,rval,0.0,0.0,'-',
     &              'dif w/m2 real',IER)
                  ICLM(J,CMCOL(2))= NINT(rval)
                elseif(ITUNITS(2).eq.1)then
                  CALL EGETWI(OUTSTR,K,ival,0,0,'-','dif w/m2 int',IER)
                  ICLM(J,CMCOL(2))=ival
                elseif(ITUNITS(2).eq.2)then
                 CALL EGETWI(OUTSTR,K,ival,0,0,'-','dif 10th w/m2',IER)
                  ICLM(J,CMCOL(2))=ival/10  ! to get back to w/m2
                endif

              elseif(N1.eq.IMSTC(3))then
                if(ITUNITS(3).eq.0)then
                  CALL EGETWR(OUTSTR,K,rval,0.0,0.0,'-',
     &              'dir w/m2 real',IER)
                  ICLM(J,CMCOL(3))= NINT(rval)
                elseif(ITUNITS(3).eq.1)then
                  CALL EGETWI(OUTSTR,K,ival,0,0,'-','dir w/m2 int',IER)
                  ICLM(J,CMCOL(3))=ival
                elseif(ITUNITS(3).eq.2)then
                 CALL EGETWI(OUTSTR,K,ival,0,0,'-','dir 10th w/m2',IER)
                  ICLM(J,CMCOL(3))=ival/10  ! to get back to w/m2
                endif

              elseif(N1.eq.IMSTC(4))then
                btog=.false.
                if(ITUNITS(4).eq.0)then
                  CALL EGETWR(OUTSTR,K,rval,0.0,0.0,'-',
     &              'glo w/m2 real',IER)
                  ICLM(J,CMCOL(4))= NINT(rval)
                elseif(ITUNITS(4).eq.1)then
                  CALL EGETWI(OUTSTR,K,ival,0,0,'-','glo w/m2 int',IER)
                  ICLM(J,CMCOL(4))=ival
                elseif(ITUNITS(4).eq.2)then
                 CALL EGETWI(OUTSTR,K,ival,0,0,'-','glo 10th w/m2',IER)
                  ICLM(J,CMCOL(4))=ival/10  ! to get back to w/m2
                elseif(ITUNITS(4).eq.3)then
                  CALL EGETWR(OUTSTR,K,rval,0.0,0.0,'-',
     &              'glo KJ/m2 real',IER)
                  ICLM(J,CMCOL(4))= NINT(rval*0.277777778)
                  btog=.true.
                elseif(ITUNITS(4).eq.4)then
                  CALL EGETWI(OUTSTR,K,ival,0,0,'-','glo KJ/m2 int',IER)
                  ICLM(J,CMCOL(4))= NINT(ival*0.277777778)
                  btog=.true.
                endif

C Also need to derive the diffuse as per the approach for decoding
C Korean MET office data.
                if (btog) then
                  call edayr(IDD,imonth,iday)
                  CALL APPRDIFFUSE(iyear,imonth,iday,J,ICLM(J,CMCOL(4)),
     &              longit_for_diffuse_calcs,differ_from_Greenwich,DRAD)
                  IDRAD=NINT(DRAD)
                  ICLM(J,CMCOL(2))=IDRAD
                endif

              elseif(N1.eq.IMSTC(5))then
                if(ITUNITS(5).eq.0)then
                  CALL EGETWR(OUTSTR,K,rval,0.0,0.0,'-',
     &              'vel m/s real',IER)
                  ICLM(J,CMCOL(5))= NINT(rval*10.0)
                elseif(ITUNITS(5).eq.1)then
                  CALL EGETWI(OUTSTR,K,ival,0,0,'-','vel m/s int',IER)
                  ICLM(J,CMCOL(5))=ival*10
                elseif(ITUNITS(5).eq.2)then
                  CALL EGETWI(OUTSTR,K,ival,0,0,'-','tenths m/s',IER)
                  ICLM(J,CMCOL(5))=ival  ! no conversion required
                elseif(ITUNITS(5).eq.3)then
                  CALL EGETWR(OUTSTR,K,rval,0.0,0.0,'-',
     &              'vel knots real',IER)
                  ICLM(J,CMCOL(5))= NINT((rval*10.0)*0.51444)
                elseif(ITUNITS(5).eq.4)then
                  CALL EGETWI(OUTSTR,K,ival,0,0,'-','vel knot int',IER)
                  ICLM(J,CMCOL(5))= NINT((ival*10)*0.51444)
                elseif(ITUNITS(5).eq.5)then
                  CALL EGETWR(OUTSTR,K,rval,0.0,0.0,'-',
     &              'vel km/hr real',IER)
                  ICLM(J,CMCOL(5))= NINT((rval*10.0)*0.2777)
                endif

              elseif(N1.eq.IMSTC(6))then
                if(ITUNITS(6).eq.0)then
                  CALL EGETWR(OUTSTR,K,rval,0.0,0.0,'-',
     &              'w dir real',IER)
                  ICLM(J,CMCOL(6))= NINT(rval)
                elseif(ITUNITS(6).eq.1)then
                  CALL EGETWI(OUTSTR,K,ival,0,0,'-','w dir int',IER)
                  ICLM(J,CMCOL(6))=ival
                elseif(ITUNITS(6).eq.2)then
                 CALL EGETWI(OUTSTR,K,ival,0,0,'-','tenths dir dg',IER)
                  ICLM(J,CMCOL(6))=ival/10  ! to get back to degrees
                endif

              elseif(N1.eq.IMSTC(7))then
                if(ITUNITS(7).eq.0)then
                  CALL EGETWR(OUTSTR,K,rval,0.0,0.0,'-','RH % real',IER)
                  ICLM(J,CMCOL(7))= NINT(rval)
                elseif(ITUNITS(7).eq.1)then
                  CALL EGETWI(OUTSTR,K,ival,0,0,'-','RH % int',IER)
                  ICLM(J,CMCOL(7))=ival
                elseif(ITUNITS(7).eq.2)then
                  CALL EGETWI(OUTSTR,K,ival,0,0,'-','RH 10th % int',IER)
                  ICLM(J,CMCOL(7))=ival/10 ! to get back to %
                elseif(ITUNITS(7).eq.3)then
                  CALL EGETWR(OUTSTR,K,rval,0.0,0.0,'-','RH 0.-1.',IER)
                  ICLM(J,CMCOL(7))=NINT(rval*100.0)
                endif

              elseif(N1.eq.IMSTC(8))then
                if(ITUNITS(8).eq.0)then
                  CALL EGETWR(OUTSTR,K,rval,0.0,0.0,'-','tot cld real',
     &              IER)
                  ICLM(J,CMCOL(8))= NINT(rval)
                elseif(ITUNITS(8).eq.1)then
                  CALL EGETWI(OUTSTR,K,ival,0,0,'-','tot cld int',IER)
                  ICLM(J,CMCOL(8))= ival
                elseif(ITUNITS(8).eq.2)then
                  CALL EGETWR(OUTSTR,K,rval,0.0,0.0,'-','tot cld 0.-1.',
     &              IER)
                  ICLM(J,CMCOL(8))= NINT(rval*10.0)
                endif

              elseif(N1.eq.IMSTC(9))then
                if(ITUNITS(9).eq.0)then
                  CALL EGETWR(OUTSTR,K,rval,0.0,0.0,'-','opq cld real',
     &              IER)
                  ICLM(J,CMCOL(9))= NINT(rval)
                elseif(ITUNITS(9).eq.1)then
                  CALL EGETWI(OUTSTR,K,ival,0,0,'-','opq cld int',IER)
                  ICLM(J,CMCOL(9))= ival
                elseif(ITUNITS(9).eq.2)then
                  CALL EGETWR(OUTSTR,K,rval,0.0,0.0,'-','opq cld 0.-1.',
     &              IER)
                  ICLM(J,CMCOL(9))= NINT(rval*10.0)
                endif

              elseif(N1.eq.IMSTC(10))then
                if(ITUNITS(10).eq.0)then
                  CALL EGETWR(OUTSTR,K,rval,0.0,0.0,'-','pres real',IER)
                  ICLM(J,CMCOL(10))= NINT(rval)
                elseif(ITUNITS(10).eq.1)then
                  CALL EGETWI(OUTSTR,K,ival,0,0,'-','pres int',IER)
                  ICLM(J,CMCOL(10))= ival
                elseif(ITUNITS(10).eq.2)then
                  CALL EGETWR(OUTSTR,K,rval,0.0,0.0,'-','pres kPa real',
     &              IER)
                  ICLM(J,CMCOL(10))= NINT(rval*1000.0)
                elseif(ITUNITS(10).eq.3)then
                  CALL EGETWI(OUTSTR,K,ival,0,0,'-','pres kPa int',IER)
                  ICLM(J,CMCOL(10))= ival*1000
                elseif(ITUNITS(10).eq.4)then
                  CALL EGETWI(OUTSTR,K,ival,0,0,'-','pres 10th kPa int',
     &              IER)
                  ICLM(J,CMCOL(10))= ival*100
                elseif(ITUNITS(10).eq.5)then
                  CALL EGETWR(OUTSTR,K,rval,0.0,0.0,'-','pres hPa real',
     &              IER)
                  ICLM(J,CMCOL(10))= NINT(rval*100.0)
                elseif(ITUNITS(10).eq.6)then
                  CALL EGETWR(OUTSTR,K,rval,0.0,0.0,'-','pres bar real',
     &              IER)
                  ICLM(J,CMCOL(10))= NINT(rval*100000.0)
                elseif(ITUNITS(10).eq.7)then
                  CALL EGETWR(OUTSTR,K,rval,0.0,0.0,'-','pres psi real',
     &              IER)
                  ICLM(J,CMCOL(10))= NINT(rval*6894.757)
                elseif(ITUNITS(10).eq.8)then
                  CALL EGETWR(OUTSTR,K,rval,0.0,0.0,'-','pres atm real',
     &              IER)
                  ICLM(J,CMCOL(10))= NINT(rval*101325.0)
                endif
              else

C The current column in the file is to be skipped over.
                CALL EGETW(OUTSTR,K,WORD,'W','un-used colm',IER)
              endif
    6       continue
   20     continue
   21     continue   ! jump point

C Transfer this to the binary file.
          IREC=IDD
          WRITE(ICLIM,REC=IREC,IOSTAT=ISTAT,ERR=1002)
     &         ((ICLM(J,K),K=1,MCM),J=1,24)
   10   CONTINUE

C Insert climatic year.
        IREC=366
        WRITE(ICLIM,REC=IREC,IOSTAT=ISTAT,ERR=1001)CFYEAR

C Insert location, latitude and longitude.
        IREC=IREC+1
        WRITE(ICLIM,REC=IREC,IOSTAT=ISTAT,ERR=1001)
     &    CFLOC,CFLAT,CFLONG

C Insert metric flags.
        IREC=IREC+1
        WRITE(ICLIM,REC=IREC,IOSTAT=ISTAT,ERR=1001)
     &    (CMCOL(i),i=1,MCM)

C Insert version - use latest version for the database no matter what
C version of ASCII we were given.
        CFVER=NMCM
        CFMCM=MCM
        IREC=IREC+1
        WRITE(ICLIM,REC=IREC,IOSTAT=ISTAT,ERR=1001)CFVER

        CALL CLMMDN2O

C Close ASCIIfile.
        CALL ERPFREE(IUNIT,ISTAT)
      endif
      goto 28

 1001 WRITE(outs,775)ISTAT
  775 FORMAT(' Error ',I7,' writing weather database metadata')
      call edisp(iuout,outs)
      CALL ERPFREE(IUNIT,ISTAT)
      RETURN

 1002 WRITE(outs,777)ISTAT
  777 FORMAT(' Error ',I7,' writing weather database')
      call edisp(iuout,outs)
      CALL ERPFREE(IUNIT,ISTAT)
      RETURN

      END

C MKACLM is the same code as in edatabase.f
C ************* MKACLM *************************************************
C 'MKACLM' Allows any period of climatic data in binary weather file to
C be transferred to an ASCII file - if act = E then and ESP-r file and
C if act = S an SQL tab separated file.  

C See comments for subroutine MKBCLM above for a detailed explaination.
      SUBROUTINE MKACLM(ACT,LASCI,IAF,IBD,IED,IER)
#include "esprdbfile.h"
#include "climate.h"
      
      integer lnblnk  ! function definition

      COMMON/OUTIN/IUOUT,IUIN,IEOUT
      COMMON/CLMSET/ICYEAR,ICDNGH,CLAT,CLONG
      COMMON/SET1/IYEAR,IBDOY,IEDOY,IFDAY,IFTIME
      COMMON/RADTYP/IDNGH
      COMMON/CLMDT1/CLMLOC
      DIMENSION ICLM(24,MCM),RCLM(MCM),iclmcolarr(MCM),iclmarr(24,MCM)
      logical hasarr
      CHARACTER LASCI*96,outs*124,CLMLOC*42,outsd*124
      character act*1,tab*1,msg*19

      tab = CHAR(9)

C Error if file unit is corrupt.
      if(iaf.eq.0)then
        call usrmsg('MKACLM: Problem with file unit for...',LASCI,'W')
        ier=1
        return
      endif

C Create file explicitly (not relative to problem configuration path).
      CALL ERPFREE(IAF,ISTAT)
      call FPOPEN(IAF,ISTAT,1,3,LASCI)

C Open ASCII file to receive weather data. If file exists, user is asked
C to confirm overwriting.
      IF(IER.LT.0)THEN
        IER=1
        RETURN
      ENDIF

C Rearrange the column order of climate metrics, if needed.
      call CLMARR(hasarr,iclmcolarr)

      if(act.eq.'E')then
        WRITE(IAF,'(A,I1)',IOSTAT=ISTAT,ERR=1003)'*WEATHER ',NMCM

C Create a header section with site details.
        WRITE(IAF,'(A)',IOSTAT=ISTAT,ERR=1003)CFLOC(1:lnblnk(CFLOC))
        
        WRITE(outs,'(I6,2F8.2)',IOSTAT=ISTAT,ERR=1003)CFYEAR,CFLAT,
     &    CFLONG
        call SDELIM(outs,outsd,'C',IW)
        write(IAF,'(2a)')outsd(1:lnblnk(outsd)),
     &    '   # year, latitude, longitude difference'
        outs=' '

C Write list of metrics.
        WRITE(IAF,'(A)')'# Parameter                | Column   | Unit'
        do imet=1,MCM
          if (iclmcolarr(imet).eq.0) then
            WRITE(IAF,'(2x,3a)')CMNAMF(imet),' | excluded | ',
     &        CFUNIT(imet)
          else
            WRITE(IAF,'(2x,2a,i8,2a)')CMNAMF(imet),' | ',
     &        iclmcolarr(imet),' | ',CFUNIT(imet)
          endif
        enddo

      elseif(act.eq.'S')then
        lcl=lnblnk(CFLOC)
        WRITE(IAF,'(15A)',IOSTAT=ISTAT,ERR=1003)'climate_site',tab,
     &    'datetime',tab,'db',tab,'dir_solar',tab,'diff_solar',tab,
     &    'wind_speed',tab,'wind_dir',tab,'rh'
      endif
 
      IDS=IBD
      IDF=IED
      DO 10 I=1,365
        IDD=I

C If we are before start day, or after end day, zero data.
        if (IDD.lt.IDS .or. IDD.gt.IDF) then
          do J=1,24
            do K=1,NCM
              ICLM(J,K)=0
            enddo
          enddo
        else

C Otherwise, read in one days data from binary file.
          IREC=IDD
          READ(ICLIM,REC=IREC,IOSTAT=ISTAT,ERR=1000)
     &      ((ICLM(J,K),K=1,CFMCM),J=1,24)
        endif

C Write day demarcation line.
        CALL EDAYR(IDD,IDY,IMN)
        if(act.eq.'E')then
          WRITE(IAF,12,IOSTAT=ISTAT,ERR=1001)IDY,IMN
  12      FORMAT('* day',I3,' month',I3)
        endif

        DO 20 J=1,24

C Now transfer data hour-by-hour to ASCII file.
          outs=' '
          if(act.eq.'E')then

C If columns have been re-arranged, we need to rearrange the data.
            if (hasarr) then
              do imet=1,MCM
                if (CMXST(imet)) then
                  iclmarr(J,iclmcolarr(imet))=ICLM(J,CMCOL(imet))
                endif
              enddo
              WRITE(outs,'(I7)',IOSTAT=ISTAT,ERR=1002)
     &          (iclmarr(J,K),K=1,NCM)
              call SDELIM(outs,outsd,'C',IW)
            else
              WRITE(outs,'(I7)',IOSTAT=ISTAT,ERR=1002)
     &          (ICLM(J,K),K=1,NCM)
              call SDELIM(outs,outsd,'C',IW)
            endif
            write(IAF,'(1x,a)')outsd(1:lnblnk(outsd))
          elseif(act.eq.'S')then

C Make up the date string. Ensure that the month, day, hour each take
C up two characters. 
            if(j.le.9)then
              if(IMN.le.9)then
                if(IDY.le.9)then
                  write(msg,'(i4,a,i1,a,i1,a,i1,a)') IYEAR,'-0',
     &              IMN,'-0',IDY,' 0',j,':00:00'
                elseif(IDY.ge.10)then
                  write(msg,'(i4,a,i1,a,i2,a,i1,a)') IYEAR,'-0',
     &              IMN,'-',IDY,' 0',j,':00:00'
                endif
              elseif(IMN.ge.10)then
                if(IDY.le.9)then
                  write(msg,'(i4,a,i2,a,i1,a,i1,a)') IYEAR,'-',
     &              IMN,'-',IDY,' 0',j,':00:00'
                elseif(IDY.ge.10)then
                  write(msg,'(i4,a,i2,a,i2,a,i1,a)') IYEAR,'-',
     &              IMN,'-',IDY,' 0',j,':00:00'
                endif
              endif
            else
              if(IMN.le.9)then
                if(IDY.le.9)then
                  write(msg,'(i4,a,i1,a,i1,a,i2,a)') IYEAR,'-0',
     &              IMN,'-0',IDY,' ',j,':00:00'
                elseif(IDY.ge.10)then
                  write(msg,'(i4,a,i1,a,i2,a,i2,a)') IYEAR,'-0',
     &              IMN,'-',IDY,' ',j,':00:00'
                endif
              elseif(IMN.ge.10)then
                if(IDY.le.9)then
                  write(msg,'(i4,a,i2,a,i1,a,i2,a)') IYEAR,'-',
     &              IMN,'-',IDY,' ',j,':00:00'
                elseif(IDY.ge.10)then
                  write(msg,'(i4,a,i2,a,i2,a,i2,a)') IYEAR,'-',
     &              IMN,'-',IDY,' ',j,':00:00'
                endif
              endif
            endif

C Convert integer values in iclm to normal units.
            do imet=1,MCM
              icol=CMCOL(imet)
              if (icol.gt.0) then
                if (imet.eq.1 .or. imet.eq.5) then
                  RCLM(icol)=real(ICLM(j,icol))/10.0
                else
                  RCLM(icol)=real(ICLM(j,icol))
                endif
              endif
            enddo

C Write tab separated data out. Prepend whatever the current weather
C location description is for the site.
            WRITE(IAF,'(3A,(a,f6.1))',IOSTAT=ISTAT,ERR=1003)
     &        CFLOC(1:lcl),tab,msg,(tab,RCLM(im),im=1,NCM)
          endif
   20   CONTINUE
   10 CONTINUE

C Close ASCII file.
      CALL ERPFREE(IAF,ISTAT)
      RETURN

 1000 WRITE(outs,776)ISTAT,IREC
  776 FORMAT(' Error ',I4,' reading binary file record ',I4)
      call edisp(iuout,outs)
      CALL ERPFREE(IAF,ISTAT)
      RETURN

 1001 WRITE(outs,775)ISTAT
  775 FORMAT(' Error ',I4,' writing demarcation')
      call edisp(iuout,outs)
      CALL ERPFREE(IAF,ISTAT)
      RETURN

 1002 WRITE(outs,777)ISTAT
  777 FORMAT(' Error ',I4,' writing hourly data')
      call edisp(iuout,' ')
      CALL ERPFREE(IAF,ISTAT)
      RETURN

 1003 WRITE(outs,778)ISTAT
  778 FORMAT(' Error ',I7,' writing site data')
      call edisp(iuout,outs)
      CALL ERPFREE(IAF,ISTAT)
      RETURN

      END

C 'MKOREANCLM' Allows a period of climatic data in a suitable ASCII file
C to be transferred to a binary weather file.  The ASCII file should 
C contain the following values for each hour (if it was created prior
C to ~2006:
C col     description                    unit            variable
C 1.     Site index (integer)
C 2.     Year (integer)
C 3.     Month (integer)
C 4.     Day-of-month (integer)
C 5-28   External dry bulb temperature   (TENTHS DEG.C)   ITMP
C 29-52  Wind direction (integer)        (DEGREES)        IDIR
C       index decodes as follows:
C       02=22, 05=45, 07=67, 09=90, 11=112, 14=135,
C       16=157, 18=180, 20=202, 23=225, 25=247, 
C       27=270, 29=292, 27=270, 29=292, 32=315, 34=337, 36=0
C 53-76  Prevailing wind speed           (TENTHS M/S)     IVEL
C 77-100 Relative humidity               (PERCENT)        IHUM
C Each data line is | separated and all values should be integers.

C Korean Met office files have pyronometer data (0.01MJ) and Cloud tenths.
C these are not yet dealt with.

C Recent files adopt a different format
C col     description                    unit            variable
C 1     site number (int), 
C 2     year month day as one string e.g. 20090703 is 2009 july 3
C 3-26 (24 values) amb temp (real),     (DEG.C)
C 27-50 (24 values) humidity (ints),    (PERCENT)
C 51-74 (24 values) wind direction (between 0-36) north is zero take number and mult by 10
C 75-98 (24 values) wind speed m/s (real), (M/S)
C 99-114 (16 values) solar radiation MJ/m2 (real) mult 277.7 from MJ/m2->W/M2
C 115-122 (8 values) cloud cover (0-10) (ints) probably multiply by 10 to get percent 
C 123-125 ground temps at 1m 3m 5m (real)  (DEG.C)


C Each data line is | or , separated and values are a mix of reals and integers.
C Assume that the weather file has already been opened elsewhere. Note some
C files may have first line STN_ID if this is found skip and try next.
      SUBROUTINE MKOREANCLM(LASCI,IUNIT,ACT,IER)
#include "esprdbfile.h"
#include "climate.h"
#include "help.h"

C Parameters
      character LASCI*96 ! file name to import
      integer IUNIT      ! unit number of the file (assumed already opened)
      character ACT*3    ! old pre 2006 files, new current files.

      COMMON/OUTIN/IUOUT,IUIN,IEOUT
      
      COMMON/CLMDT1/CLMLOC
      COMMON/CLMSET/ICYEAR,ICDNGH,CLAT,CLONG
      COMMON/SET1/IYEAR,IBDOY,IEDOY,IFDAY,IFTIME
      COMMON/RADTYP/IDNGH
      COMMON/APPRDIFF/iapprox_diff

      dimension ICLM(24,6)

      CHARACTER CLMLOC*42,outs*124
      character OUTSTR*1000   ! a 1K string buffer
      character word*24,t2m*2,t2d*2,t4*4
      integer isite    ! site index
      integer iyr      ! year of scan
      integer imonth   ! month of scan
      integer iday     ! day of month of scan
      integer ih       ! hour counter
      integer ijday    ! julian day
      integer idif,itmp,ivel,idir,ihum
      integer IGLOBH   ! to hold int version of global horiz W
      real value       ! for use in scan
      real watts       ! for use in conversion

      real longit_for_diffuse_calcs,differ_from_Greenwich
      integer IDRAD

      helpinsub='cfiles'  ! set for cfiles

C Open the ASCII file for reading.
      if(iunit.eq.0)then
        call usrmsg(' Problem with file unit for...',LASCI,'W')
        ier = 1
        return
      endif

      CALL EFOPSEQ(IUNIT,LASCI,1,IER)
      if(ier.ne.0)then
        call usrmsg(' Problem opening...',LASCI,'W')
        ier = 1
        return
      endif

C Initialiase defaults for easkr
      longit_for_diffuse_calcs=0.
      differ_from_Greenwich=0.
      
C Set the radiation flag to Global horizontal      
      IDNGH=123
      helptopic='confirm_lat_log_diff'
      call gethelptext(helpinsub,helptopic,nbhelp)
      CALL EASKR(longit_for_diffuse_calcs,' ',
     &   'Weather station longitude?',
     &   -180.0,'W',180.0,'W',30.0,'weather longitude',IER,nbhelp)

      helptopic='local_time_difference'
      call gethelptext(helpinsub,helptopic,nbhelp)
      CALL EASKR(differ_from_Greenwich,' ',
     &  'Weather station longitude difference?',
     &  -24.0,'W',24.0,'W',0.0,'time_difference',IER,nbhelp)

C Export weather data to ASCII file (with similar name).
      helptopic='korea_met_conversion'
      call gethelptext(helpinsub,helptopic,nbhelp)
      call easkabc
     &  ('Available models to approximate diffuse:',
     &  ' ','Muneer','Maxwell','Erbs Klein & Duffie',
     &  IDIFF,nbhelp)

C Set the global variable iapprox_diff to tell underlying calculation
C method which equation set to use.
      if(IDIFF.eq.1)then
        iapprox_diff=1
      elseif(IDIFF.eq.2)then  
        iapprox_diff=2
      elseif(IDIFF.eq.3)then  
        iapprox_diff=3
      endif

C Read each line of the data file and parse the data.
      scan: do
        if(act(1:3).eq.'old')then

C If the older format read a line of the file.
          CALL STRIPC1K(IUNIT,OUTSTR,0,ND,1,'data line',IER)
          if(ier.lt.0) exit scan
          if(OUTSTR(1:6).eq.'STN_ID')then  ! skip the header line
            CALL STRIPC1K(IUNIT,OUTSTR,0,ND,1,'data line',IER)
          endif
          K=0
          CALL EGETWI(OUTSTR,K,isite,0,999,'W','site index',IER)
          CALL EGETWI(OUTSTR,K,iyr,1900,2051,'W','year index',IER)
          if(iyr.lt.iyear) cycle   ! if not yet the correct year read again.
          if(iyr.gt.iyear) exit scan   ! if past the correct year write tail.

C We have a correct year so carry on scanning.
          CALL EGETWI(OUTSTR,K,imonth,1,12,'W','month index',IER)
          CALL EGETWI(OUTSTR,K,iday,1,31,'W','day index',IER)

          write(outs,'(a,i2,a,i2,3a)') 'Data for month ',imonth,' day ',
     &      iday,' ',outstr(1:60),'...'
          call edisp(iuout,outs)

C Read 24 hours of dry bulb temperature and assign to ICLM.
          do 43 ih=1,24
            CALL EGETWI(OUTSTR,K,ITMP,0,0,'-','db temperature',IER)
            ICLM(ih,2)=ITMP
 43       continue

C Read 24 hours of wind direction, decode and assign to IDIR.
C         02=22, 05=45, 07=67, 09=90, 11=112, 14=135,
C         16=157, 18=180, 20=202, 23=225, 25=247, 
C         27=270, 29=292, 27=270, 29=292, 32=315, 34=337, 36=0
          do 44 ih=1,24
            CALL EGETWI(OUTSTR,K,IDIR,0,36,'W','wind direction index',
     &        IER)
            if(idir.eq.2)then
              ICLM(ih,5)=22
            elseif(idir.eq.5)then 
              ICLM(ih,5)=45
            elseif(idir.eq.7)then
              ICLM(ih,5)=67
            elseif(idir.eq.9)then
              ICLM(ih,5)=90
            elseif(idir.eq.11)then
              ICLM(ih,5)=112
            elseif(idir.eq.14)then 
              ICLM(ih,5)=135
            elseif(idir.eq.16)then 
              ICLM(ih,5)=157
            elseif(idir.eq.18)then 
              ICLM(ih,5)=180
            elseif(idir.eq.20)then 
              ICLM(ih,5)=202
            elseif(idir.eq.23)then 
              ICLM(ih,5)=225
            elseif(idir.eq.25)then 
              ICLM(ih,5)=247
            elseif(idir.eq.27)then 
              ICLM(ih,5)=270
            elseif(idir.eq.29)then 
              ICLM(ih,5)=292
            elseif(idir.eq.32)then 
              ICLM(ih,5)=315
            elseif(idir.eq.34)then 
              ICLM(ih,5)=337
            elseif(idir.eq.36)then 
              ICLM(ih,5)=0
            elseif(idir.eq.0)then 
              ICLM(ih,5)=0
            else
              ICLM(ih,5)=0
            endif
 44       continue

C Read 24 hours of wind velocity and assign to ICLM.
          do 45 ih=1,24
            CALL EGETWI(OUTSTR,K,IVEL,0,0,'-','wind velocity',IER)
            ICLM(ih,4)=IVEL
 45       continue

C Read 24 hours of humidity and assign to ICLM.
          do 46 ih=1,24
            CALL EGETWI(OUTSTR,K,IHUM,0,100,'-','humidity %',IER)
            ICLM(ih,6)=IHUM
 46       continue

C Clear solar values.
          idif=0
          ighr=0
          do 47 ih=1,24
            ICLM(ih,1)=idif
            ICLM(ih,3)=ighr
 47       continue

          do 147 ih=6,20
            CALL EGETWI(OUTSTR,K,IGLOBH,0,100,'-','GLOB_HORIZO',IER)

C Convert MJ/m2 to W/m2
            IGLOBH=NINT(IGLOBH*277.777)
            ICLM(ih,3)=IGLOBH
            CALL APPRDIFFUSE(iyear,imonth,iday,ih,IGLOBH,
     &        longit_for_diffuse_calcs,differ_from_Greenwich,DRAD)
            IDRAD=NINT(DRAD)
            ICLM(ih,1)=IDRAD
 147      continue
 
C Convert month and day to julian day.
          call eday(iday,imonth,ijday)

C Transfer this to the binary file.
          IREC=ijday
          WRITE(ICLIM,REC=IREC,IOSTAT=ISTAT,ERR=1002)
     &      ((ICLM(J,K),K=1,6),J=1,24)

          cycle  ! go back and read another line of the file.
        else

C Read the newer format
          CALL STRIPC1K(IUNIT,OUTSTR,0,ND,1,'data line',IER)
          if(ier.lt.0) exit scan
          if(OUTSTR(1:6).eq.'STN_ID')then  ! skip the header line
            CALL STRIPC1K(IUNIT,OUTSTR,0,ND,1,'data line',IER)
          endif
          K=0
          CALL EGETWI(OUTSTR,K,isite,0,999,'W','site index',IER)
          CALL EGETW(OUTSTR,K,WORD,'W','string for date',IER)
          write(t4,'(a)') WORD(1:4)  ! string for year
          write(t2m,'(a)') WORD(5:6) ! string for month
          write(t2d,'(a)') WORD(7:8) ! string for day
          read(t4,*,IOSTAT=IOS,ERR=1002) iyr
          if(iyr.lt.iyear) cycle     ! if not yet the correct year read again.
          if(iyr.gt.iyear) exit scan ! if past the correct year write tail.
          read(t2m,*,IOSTAT=IOS,ERR=1002) imonth
          read(t2d,*,IOSTAT=IOS,ERR=1002) iday

C We have a correct year so carry on scanning.
          write(outs,'(a,i2,a,i2,3a)') 'Data for month ',imonth,' day ',
     &      iday,' ',outstr(1:60),'...'
          call edisp(iuout,outs)

C Read 24 hours of dry bulb temperature and assign to ICLM.
          do 243 ih=1,24
            CALL EGETWR(OUTSTR,K,value,0.,0.,'-','db temperature',IER)
            ICLM(ih,2)=NINT(value*10.0)  ! multiply by 10 so in tenths deg C
 243      continue

C Debug.
C          write(*,*) 'db t tenths degC ',ICLM(1,2),ICLM(2,2),ICLM(3,2)

C Read 24 hours of humidity and assign to ICLM.
          do 246 ih=1,24
            CALL EGETWI(OUTSTR,K,IHUM,0,100,'-','humidity %',IER)
            ICLM(ih,6)=IHUM
 246      continue

C Debug.
C          write(*,*) 'RH % ',ICLM(1,6),ICLM(2,6),ICLM(3,6)

C Read 24 hours of wind direction, decode and assign to IDIR.
C         02=22, 05=45, 07=67, 09=90, 11=112, 14=135,
C         16=157, 18=180, 20=202, 23=225, 25=247, 
C         27=270, 29=292, 27=270, 29=292, 32=315, 34=337, 36=0
          do 244 ih=1,24
            CALL EGETWI(OUTSTR,K,IDIR,0,36,'W','wind direction index',
     &        IER)
            if(idir.eq.2)then
              ICLM(ih,5)=22
            elseif(idir.eq.5)then 
              ICLM(ih,5)=45
            elseif(idir.eq.7)then
              ICLM(ih,5)=67
            elseif(idir.eq.9)then
              ICLM(ih,5)=90
            elseif(idir.eq.11)then
              ICLM(ih,5)=112
            elseif(idir.eq.14)then 
              ICLM(ih,5)=135
            elseif(idir.eq.16)then 
              ICLM(ih,5)=157
            elseif(idir.eq.18)then 
              ICLM(ih,5)=180
            elseif(idir.eq.20)then 
              ICLM(ih,5)=202
            elseif(idir.eq.23)then 
              ICLM(ih,5)=225
            elseif(idir.eq.25)then 
              ICLM(ih,5)=247
            elseif(idir.eq.27)then 
              ICLM(ih,5)=270
            elseif(idir.eq.29)then 
              ICLM(ih,5)=292
            elseif(idir.eq.32)then 
              ICLM(ih,5)=315
            elseif(idir.eq.34)then 
              ICLM(ih,5)=337
            elseif(idir.eq.36)then 
              ICLM(ih,5)=0
            elseif(idir.eq.0)then 
              ICLM(ih,5)=0
            else
              ICLM(ih,5)=0
            endif
 244      continue

C Debug.
C          write(*,*) 'wind dir deg ',ICLM(1,5),ICLM(2,5),ICLM(3,5)

C Read 24 hours of wind velocity and assign to ICLM.
          do 245 ih=1,24
            CALL EGETWR(OUTSTR,K,value,0.,0.,'-','wind velocity',IER)
            ICLM(ih,4)=NINT(value*10.0)  ! to get tenths m/s
 245      continue

C Debug.
C          write(*,*) 'wind vel tenths ms ',ICLM(1,4),ICLM(2,4),ICLM(3,4)

C Clear solar values.
          idif=0
          ighr=0
          do 247 ih=1,24
            ICLM(ih,1)=idif
            ICLM(ih,3)=ighr
 247      continue

          do 347 ih=6,20
            CALL EGETWR(OUTSTR,K,value,0.,100.,'-','GLOB_HORIZO MJ/m2',
     &        IER)

C Convert MJ/m2 to W/m2
            watts=value*277.78
            ICLM(ih,3)=NINT(watts)
            IGLOBH=NINT(watts)
            CALL APPRDIFFUSE(iyear,imonth,iday,ih,IGLOBH,
     &        longit_for_diffuse_calcs,differ_from_Greenwich,DRAD)
            IDRAD=NINT(DRAD)
            ICLM(ih,1)=IDRAD
 347      continue
 
C Convert month and day to julian day.
          call eday(iday,imonth,ijday)

C Transfer this to the binary file.
          IREC=ijday
          WRITE(ICLIM,REC=IREC,IOSTAT=ISTAT,ERR=1002)
     &      ((ICLM(J,K),K=1,6),J=1,24)

          cycle  ! go back and read another line of the file.

        endif
      end do scan

      NCM=6
      CMCOL(1:2)=(/2,1/)
      if (IDNGH.eq.123) then
        CMCOL(3:4)=(/0,3/)
      else
        CMCOL(3:4)=(/3,0/)
      endif
      CMCOL(5:7)=(/4,5,6/)
      do k=8,MCM
        CMCOL(k)=0
      enddo

C Insert climatic year.
      IREC=366
      WRITE(ICLIM,REC=IREC,IOSTAT=ISTAT,ERR=1001)CFYEAR

C Insert location, latitude and longitude.
      IREC=IREC+1
      WRITE(ICLIM,REC=IREC,IOSTAT=ISTAT,ERR=1001)
     &  CFLOC,CFLAT,CFLONG

C Insert metric flags.
      IREC=IREC+1
      WRITE(ICLIM,REC=IREC,IOSTAT=ISTAT,ERR=1001)
     &  (CMCOL(i),i=1,MCM)

C Insert version - use latest version for the database no matter what
C version of ASCII we were given.
      CFVER=NMCM
      CFMCM=MCM
      IREC=IREC+1
      WRITE(ICLIM,REC=IREC,IOSTAT=ISTAT,ERR=1001)CFVER

      CALL CLMMDN2O

C Close ASCIIfile.
      CALL ERPFREE(IUNIT,ISTAT)
      call edisp(iuout,'Closing the Korean Met office file.')

      return  ! normal exit from subroutine

 1001 WRITE(outs,775)ISTAT
  775 FORMAT(' Error ',I7,' writing weather database metadata')
      call edisp(iuout,outs)
      return

 1002 WRITE(outs,777)ISTAT
  777 FORMAT(' Error ',I7,' writing weather database')
      call edisp(iuout,outs)
      return

      end

C ----------- CFILIN 
C CFILIN takes a weather file name from a browse list, command
C line or default and intialise it or open a zero length file if
C a new one is requested.
C silent is a logical toggle: true keeps user interactions to a minimum.
      SUBROUTINE CFILIN(silent)
#include "esprdbfile.h"
#include "espriou.h"
#include "climate.h"
#include "help.h"
      
      integer lnblnk  ! function definition
      integer igraphiclib  ! external definition

      COMMON/SPAD/MMOD,LIMIT,LIMTTY
      COMMON/OUTIN/IUOUT,IUIN,IEOUT
      COMMON/INPER/INIT
      COMMON/FILEP/IFIL
      COMMON/CLMDT1/CLMLOC
      COMMON/SET1/IYEAR,IBDOY,IEDOY,IFDAY,IFTIME
      COMMON/CLMSET/ICYEAR,ICDNGH,CLAT,CLONG
      COMMON/PERC/ID1,IM1,IT1,ID2,IM2,IT2,IDS,IDF,INEW
      COMMON/RADTYP/IDNGH
      common/rcmd/LCMDFL
      integer menuchw,igl,igr,igt,igb,igw,igwh
      COMMON/VIEWPX/menuchw,igl,igr,igt,igb,igw,igwh

C Climate list common.
      character clmlnam*42,clmlaid*72,clmldbfile*144,clmlavail*10
      character clmlhelp*72
      common/clmltext/clmlnam,clmlaid,clmldbfile,clmlavail,clmlhelp(60)
      common/clmlnum/nbclmlhelp

      logical XST,ok,silent,unixok
      
C Logical rescan = true if climatelist needs to be rescanned to
C pick up typical periods (i.e. if weather file name typed in
C or passed into clm module).
      logical rescan

      CHARACTER CLMLOC*42,outs*124,tab*1
      character minu*1,second*1,longdms*14,latdms*14
      character outs248*248
      character LCMDFL*144,clmdb*144,L144*144,lltmp*144
      character fs*1
      character actreturn*1  ! users preference
      integer onleft  ! number of left axis
      integer onright ! number of right axis

      helpinsub='cfiles'  ! set for cfiles
      tab = CHAR(9)
      minu = CHAR(39)
      second = CHAR(34)

C Set folder separator (fs) to \ or / as required.
      call isunix(unixok)
      if(unixok)then
        fs = char(47)
      else
        fs = char(92)
      endif

C Get help message set.
      helptopic='clm_file_contents'
      call gethelptext(helpinsub,helptopic,nbhelp)
      rescan=.false.
      IUF=ifil+1

C If LCMDFL is 'UNKNOWN' then scan list of weather files 
C (the name of which is in the users .esprc file) (or from the
C standard '/opt/esp-r/climate/climatelist'. If clm is
C running in silent mode the user is not expected to select
C from a list so might as well exit the program.
      if(LCMDFL(1:7).eq.'UNKNOWN'.or.LCMDFL(1:2).eq.'  ')then
        if(silent)then
          write(outs248,'(3a)')'Weather exited because input file was',
     &      tab,LCMDFL(1:lnblnk(LCMDFL))
          call isunix(unixok)
          if(unixok)then  ! do not bother if DOS
            write(6,*)outs248(1:lnblnk(outs248))
          endif
          close(ieout)
          CALL ERPFREE(ieout,ISTAT)
          CALL EPAGEND
          STOP
        endif

        INQUIRE (FILE=cdblfil,EXIST=XST)
        if(XST)then

C rdblist returns typper common block for the selected
C weather file.
          if(IUF.eq.0)then
            IUF=ifil+1
          endif
C          call usrmsg('Please select a weather set from the ',
C     &                'list provided.','-')
          call rdblist(IUF,clmdb,'c',actreturn,ier)
          if(ier.ne.0)then
            write(LCMDFL,'(a)') DCLIM(1:lnblnk(DCLIM))
            rescan=.true.
          endif
          if(clmdb(1:12).eq.'USER_DEFINED')then

C The X11 version will be returning only the name of the
C file, while the GTK version will be returning the
C name with the full path.
            LCMDFL='newclim'
 291        continue
            iglib = igraphiclib()  ! find out if X11 or GTK or text support only.
            if(iglib.eq.1.or.iglib.eq.3)then
              CALL EASKF(LCMDFL,' ','Weather file name?',
     &         96,DCLIM,'new weather file',IER,nbhelp)
            elseif(iglib.eq.2)then
              CALL EASKF(LCMDFL,' ','Weather file name?',
     &         144,DCLIM,'new weather file',IER,nbhelp)
            else
              CALL EASKF(LCMDFL,' ','Weather file name?',
     &         96,DCLIM,'new weather file',IER,nbhelp)
            endif

            IF(LCMDFL.EQ.' ')GOTO 291
            IER=0
            rescan=.false.
          elseif(clmdb.eq.'UNKNOWN'.or.clmdb(1:2).eq.'  ')then
            call usrmsg(' Nothing selected from the list ',
     &                  ' using default...','W')
            write(LCMDFL,'(a)') DCLIM(1:lnblnk(DCLIM))
            rescan=.true.
          else
            write(LCMDFL,'(a)') clmdb(1:lnblnk(clmdb))
            rescan=.false.
          endif
        else
          call edisp(iuout,
     &    ' No weather file list available, using default.')
          call edisp(iuout,
     &    ' (Refer `climatelist` file)')
          write(LCMDFL,'(a)') DCLIM(1:lnblnk(DCLIM))
 289      continue
          iglib = igraphiclib()  ! find out if X11 or GTK or text support only.
          if(iglib.eq.1.or.iglib.eq.3)then
            CALL EASKF(LCMDFL,' ','Weather file name?',
     &        96,DCLIM,'weather file',IER,nbhelp)
          elseif(iglib.eq.2)then
            CALL EASKF(LCMDFL,' ','Weather file name?',
     &        144,DCLIM,'weather file',IER,nbhelp)
          else
            CALL EASKF(LCMDFL,' ','Weather file name?',
     &        96,DCLIM,'weather file',IER,nbhelp)
          endif
          IF(LCMDFL.EQ.' ')GOTO 289
          IER=0
          rescan=.true.
        endif
      else
        if(silent)then
          IER=0
          rescan=.true.
        else
          L144=LCMDFL

C Call EASKF depending on the current file name length.
C The X11 version will be returning only the name of the
C file, while the GTK version will be returning the
C name with the full path.
  290     llt=lnblnk(L144)
          iglib = igraphiclib()  ! find out if X11 or GTK or text support only.
          if(iglib.eq.1.or.iglib.eq.3)then
            if(llt.lt.96)then
              CALL EASKF(L144,' ','Weather file name?',
     &          96,DCLIM,'weather file name',IER,nbhelp)
            elseif(llt.ge.96.and.llt.lt.124)then
              CALL EASKF(L144,' ','Weather file name?',
     &          124,DCLIM,'weather file name',IER,nbhelp)
            elseif(llt.ge.124.and.llt.le.144)then
              CALL EASKF(L144,' ','Weather file name?',
     &          144,DCLIM,'weather file name',IER,nbhelp)
            endif
          elseif(iglib.eq.2)then
            CALL EASKF(L144,' ','Weather file name?',
     &        144,DCLIM,'weather file name',IER,nbhelp)
          else
            CALL EASKF(L144,' ','Weather file name?',
     &        96,DCLIM,'weather file name',IER,nbhelp)
          endif
          IF(L144(1:2).EQ.'  '.or.L144(1:4).eq.'UNKN')GOTO 290
          call st2file(L144,LCMDFL)
          IER=0
          rescan=.true.
        endif
      endif

C Create file explicitly (not relative to problem configuration path).
C First check if it exists: if so open, otherwise create with 0 length.
      if(ICLIM.eq.0)then
        ICLIM=IFIL
      endif
      CALL ERPFREE(ICLIM,ISTAT)

C Take into account the location of the weather file.
      lndbp=lnblnk(standarddbpath)
      if(ipathclim.eq.0.or.ipathclim.eq.1)then
        lltmp=LCMDFL
      elseif(ipathclim.eq.2)then
        write(lltmp,'(3a)') standarddbpath(1:lndbp),fs,
     &    LCMDFL(1:lnblnk(LCMDFL))  ! prepend db folder path
      endif

      write(LCLIM,'(a)') lltmp(1:lnblnk(lltmp))
      CALL CLMOPB(3,IER)     ! 3 - if exists use, otherwise create.
      if (CFVER.eq.0) RETURN ! New weather file, return.
      CALL CLMRDBMD(IER)
      IYEAR=ICYEAR

C If unable to open the weather database, wait 3 seconds then
C abort.
      if (IER.ne.0) then
        close(ieout)
        CALL ERPFREE(ieout,ISTAT)
        call edisp(iuout,
     &    'Not enough parameters/columns in weather file.')
        call pauses(3)
        call EPAGEND
        STOP
      endif

C << future records to read at this point with periods & documentation >>

C No problems reading file so assume full year period and flag that
C the period has been set (used in main routine to not keep asking
C user for period definition).
      INIT=1
      INEW=1

C If rescan was set to TRUE re-scan the climatelist file
C for period information. If no climatelist then the defaults
C set in clm.F will be used and the climatelist data will be
C reset to indicate the weather was not in the list.
      if(rescan)then
        IUF=ifil+1
        INQUIRE (FILE=cdblfil,EXIST=XST)
        if(XST)then
          call scancdblist(IUF,LCMDFL,'c',ok,ier)
          if(ok)then
            continue
          else
            clmlnam = 'not yet entered'
            clmlaid = 'not yet entered'
            clmlavail = 'OFFLINE'
            clmldbfile = ' '
            nbclmlhelp = 1
          endif
        endif
      endif

C      call edisp(iuout,' ')
C      WRITE(outs,'(2a)')' Weather data: ',CFLOC(1:lnblnk(CFLOC))
C      call edisp(iuout,outs)

C Get longitude in degrees minutes and seconds.
      ILONG=INT(CFLONG)                 ! integer portion of degrees
      FRAM=abs(CFLONG)-abs(real(ilong)) ! fraction portion of degrees
      FRAD2=FRAM*60                    ! into minutes
      ILONGM=INT(FRAD2)                ! integer part of minutes
      FRAS=abs(FRAD2)-abs(real(ilongm))! fraction portion of minutes
      FRAS2=FRAS*60                    ! seconds
C      write(6,*)'CFLONG fra frad',CLONG,FRAM,FRAD2,ILONG,ILONGM,FRAS
      if(CFLONG.gt.0.0)then
        write(longdms,'(i2,a,i2,a,f4.1,a)')
     &    ILONG,'deg ',ILONGM,minu,FRAS2,second
      else
        write(longdms,'(i3,a,i2,a,f4.1,a)')
     &    ILONG,'deg',ILONGM,minu,FRAS2,second
      endif
C      write(6,*) 'longdms ',longdms
C Get latitude in degrees minutes and seconds.
      ILAT=INT(CFLAT)                  ! integer portion of degrees
      FRAM=abs(CFLAT)-abs(real(ilat))  ! fraction portion of degrees
      FRAD2=FRAM*60                   ! into minutes
      ILATM=INT(FRAD2)                ! integer part of minutes
      FRAS=abs(FRAD2)-abs(real(ilatm))! fraction portion of minutes
      FRAS2=FRAS*60                   ! seconds
C      write(6,*)'CFLAT fra frad',clat,FRAM,FRAD2,ILAT,ILATM,FRAS,FRAS2
      if(CFLAT.ge.0.0)then
        write(latdms,'(i2,a,i2,a,f4.1,a)')
     &    ILAT,'deg ',ILATM,minu,FRAS2,second
       else
        write(latdms,'(i3,a,i2,a,f4.1,a)')
     &    ILAT,'deg',ILATM,minu,FRAS2,second
       endif
C      write(6,*) 'latdms ',latdms
     
      if(IDNGH.GT.0)then       ! Global horizontal.
        if(CFLAT.LT.0.0)then    ! if south
          ACLAT=ABS(CFLAT)
          if(CFLONG.LT.0.0)then ! if west
           ACLONG=ABS(CFLONG)
           WRITE(outs,109)ACLAT,latdms,ACLONG,longdms,IYEAR
 109       FORMAT(1X,F7.2,' (',a,')S',F7.2,' (',a,')W',': ',I4,2X,'GH')
          else                 ! if east
           WRITE(outs,108)ACLAT,latdms,CFLONG,longdms,IYEAR
 108       FORMAT(1X,F7.2,' (',a,')S',F7.2,' (',a,')E',': ',I4,2X,'GH')
          endif
        else                   ! if north
          if(CFLONG.LT.0.0)then ! if west
           ACLONG=ABS(CFLONG)
           WRITE(outs,107)CFLAT,latdms,ACLONG,longdms,IYEAR
 107       FORMAT(1X,F7.2,' (',a,')N',F7.2,' (',a,')W',': ',I4,2X,'GH')
          else                 ! if east
           WRITE(outs,102)CFLAT,latdms,CFLONG,longdms,IYEAR
 102       FORMAT(1X,F7.2,' (',a,')N',F7.2,' (',a,')E',': ',I4,2X,'GH')
          endif
        endif
        call edisp(iuout,outs)
        RETURN
      else                     ! Direct normal.
        if(CFLAT.LT.0.0)then    ! if south
          ACLAT=ABS(CFLAT)
          if(CFLONG.LT.0.0)then ! if west
           ACLONG=ABS(CFLONG)
           WRITE(outs,106)ACLAT,latdms,ACLONG,longdms,IYEAR
 106       FORMAT(1X,F7.2,' (',a,')S',F7.2,' (',a,')W',': ',I4,2X,'DN')
          else                 ! if east
           WRITE(outs,105)ACLAT,latdms,CFLONG,longdms,IYEAR
 105       FORMAT(1X,F7.2,' (',a,')S',F7.2,' (',a,')E',': ',I4,2X,'DN')
          endif
        else                   ! if north
          if(CFLONG.LT.0.0)then ! if west
           ACLONG=ABS(CFLONG)
           WRITE(outs,104)CFLAT,latdms,ACLONG,longdms,IYEAR
 104       FORMAT(1X,F7.2,' (',a,')N',F7.2,' (',a,')W',': ',I4,2X,'DN')
          else                 ! if east
           WRITE(outs,103)CFLAT,latdms,CFLONG,longdms,IYEAR
 103       FORMAT(1X,F7.2,' (',a,')N',F7.2,' (',a,')E',': ',I4,2X,'DN')
          endif
        endif
C        call edisp(iuout,outs)

C If in graphic mode echo to the graphic feedback title position.
        if(MMOD.eq.8)then
          imh=menuchw; onleft=0; onright=0
          CALL GHEAD(imh,onleft,onright)
        endif
        RETURN
      endif

C User warning.
 1000 call edisp(IUOUT,' No data in file')
      RETURN
      END

C ---------- CLMPUT 
C CLMPUT Writes one day's weather data to the current weather file.
      SUBROUTINE CLMPUT(IDAY)
#include "climate.h"

      COMMON/OUTIN/IUOUT,IUIN,IEOUT
      COMMON/FILEP/IFIL
      COMMON/RADTYP/IDNGH
      DIMENSION ICLM(24,MCM)
      character outs*124

      do i=1,CFMCM
        do j=1,24
          ICLM(j,i)=0
        enddo
      enddo
      do i=1,MCM
        icol=CMCOL(i)
        do j=1,24
          if (icol.gt.0) then
            if (i.eq.1 .or. i.eq.5) then
              ICLM(j,icol)=NINT(10.*CMRVAL(i,j))
            else
              ICLM(j,icol)=NINT(CMRVAL(i,j))
            endif
          endif
        enddo
      enddo

      IREC=IDAY
      WRITE(IFIL,REC=IREC,IOSTAT=ISTAT,ERR=666)
     &     ((ICLM(J,K),K=1,CFMCM),J=1,24)

  999 RETURN
  666 WRITE(IUOUT,'(A,I4,A)') ' Error ',ISTAT,
     &                        ' on writing weather database'
      call edisp(iuout,outs)
      GOTO 999

      END


C 'EPWTOBCLM' Allows a period of climatic data from EPW format ASCII file
C to be transferred to a binary weather file.  The ASCII file should 
C follow the formatting conventions of the EnergyPlus/Esp-r file format.
C All items are comma separated in the EPW file format. Some fields
C may be blank (i.e. no space between commas)

C First line of EPW file
C Keyword `LOCATION` followed by 9 data fields as follows
C   city <string>, state/province/region <string>, country <string>,
C   field source <string>, IWEC tag <integer>,
C   decimal degrees latitude [-90. to 90.] <real>,
C   decimal degrees longitude [-180. to 180.] (convert to esp-r equivalent) <real>
C   time zone hours relative to GMT [-12. to 12.] <real>
C   metre elevation [-1000. to 9999.] <real>

C Keyword `DESIGN CONDITIONS` followed by
C   number of conditions <integer>,
C   for each condition (one line) 26 integers (see documentation for details)

C Keyword `TYPICAL/EXTREME PERIODS` followed by
C   number of periods <integer>,
C   for each period (one line per period): description <string>,
C     start julian date <integer>, finish julian date <integer>

C Keyword `GROUND TEMPERATURES` followed by
C   number of ground temperatures <integer>,
C   for each set (one line per set): metre depth <real>, soil conductivity
C     in W/m-K <real>, soil density in kg/m3 <real>, soil specific
C     heat in J/kg-K, monthly temperatures in deg C <12 reals>

C Keyword `HOLIDAYS/DAYLIGHT SAVINGS` followed by
C   leap year observance <string `Yes` `No`>, daylight savings start day <integer>,
C   daylight savings finish day <integer>, number of holiday definitions <integer>
C   for each holiday a description <string> and julian date

C Keyword `COMMENTS 1` followed by text <string>

C Keyword `COMMENTS 2` followed by text <string>

C Keyword `DATA PERIODS` followed by 6 fields as follows:
C   number of data periods <integer>, number of records per hour <integer>,
C   description <string>, start day of week <string `Sunday` `Monday` `Tuesday`
C   `Wednesday` `Thursday` `Friday` `Saturday`>,
C   period start month/day <integer/integer>, period finish month/day <integer/integer>

C Data line fields are as follows:
C (1) year <integer>, (2) month <integer>, (3) day <integer>, 
C (4) hour <integer>, (5) minute <integer>,
C (6) data source and uncertainty flags <string 44-50 characgers long>,
C (7) dry bulb temp C <real>, (8) dew point temp C <real>, (9) RH% <real>,
C (10) atmospheric pressure Pa <real>, (11) extraterrestrial horizontal rad Wh/m2 <real>,
C (12) extraterrestrial direct normal radiation Wh/m2 <real>,
C (13) horiz infrared radiation from sky Wh/m2 <real>,
C (14) global horiz radiation Wh/m2 <real>, (15) direct normal Wh/m2 <real>,
C (16) diffuse horiz radiation Wh/m2 <real>, (17) global horiz illum lux <real>,
C (18) direct normal illum lux <real>, (19) diffuse horiz illum lux <real>,
C (20) Zenith illum Cd/m2 <real>, (21) wind direction degrees <real>,
C (22) wind speed m/sec <real>, (23) total sky cover <integer>,
C (24)  opaque sky cover <integer>, (25) km of visibility <real>,
C (26) cloud ceiling metres <real>, (27) present weather observation <integer>,
C (28) present weather codes <integer>, (29) mm of precipitable water <real>,
C (30) aerosol optical depth <thousandths>,
C (31) cm of snow depth <integer>, (32) days since last snowfall <integer>.
C (33) Albedo
C (34) Liquid precipitation
C (35) Liquid precipitation rate

C The data fields which can slot into the current esp-r file format (v2) are:
C (16) diffuse horiz radiation Wh/m2: cast to integer IDIF,
C (7) dry bulb temp C: cast to tenths of deg C ITMP,
C (15) direct normal Wh/m2: cast to integer IDNR,
C (22) wind speed m/sec: cast to tenths m/sec IVEL,
C (21) wind direction degrees: cast to integer degrees IDIR,
C (9) RH%: cast to integer percent IHUM,
C (23) total sky cover: use as-is ITSC,
C (24) opaque sky cover: use as-is IOSC,
C (10) atmospheric pressure: cast to integer IATM,

C For available ESP-r weather metrics, see header file climate.h.

C Direct normal radiation can be replaced by global horizontal radiation (W/M**2),
C with the radiation flag set appropriately after the file has been
C read in.
      SUBROUTINE EPWTOBCLM(LASCI,IUNIT,IER)
#include "esprdbfile.h"
#include "espriou.h"
#include "climate.h"
#include "help.h"
      
      integer lnblnk  ! function definition

      COMMON/OUTIN/IUOUT,IUIN,IEOUT

      COMMON/CLMDT1/CLMLOC
      COMMON/CLMSET/ICYEAR,ICDNGH,CLAT,CLONG

      COMMON/SET1/IYEAR,IBDOY,IEDOY,IFDAY,IFTIME
      COMMON/RADTYP/IDNGH

C String variables for location.
      common/CLMLOC1/city,provence,country,source,dayname,dpdescr

C Data for location.
      common/CLMLOC2/IWECid,ALONG,CTZONE,CELEV,IDPER,IREDPH

C Design conditions line of text.
      common/CLMDESP/dcline

C Comment lines.
      common/CLMCOM/clmcomment(2)
      dimension ICLMEPW(12,31,24,MCM)
      integer cfind(MCM)

      CHARACTER CLMLOC*42,WORD*24,uncert*50
      CHARACTER LASCI*96,outs*124,OUTSTR*248
      character dcline*248
      logical close

C Initial guess on string sizes.
      character city*32,provence*24,country*24,source*24
      character dayname*8,clmcomment*72,dpdescr*24

      helpinsub='cfiles'

      city=' '
      provence=' '
      country=' '
      source=' '
      dayname=' '
      dcline=' '
      clmcomment(1)=' '
      clmcomment(2)=' '
      uncert=' '
      irecloop=0
      imonstart=1
      idaystart=1
      imonfn=12
      idayfn=31
      iambpatot=0
      iambpaok=0
      itotskycovtot=0
      itotskycovok=0
      iopqskycovtot=0
      iopqskycovok=0

C Clear the ICLMEPW array.
      do 5 ip=1,12
        do 6 jp=1,31
          do 7 kp=1,24
            do 8 lp=1,6
              ICLMEPW(ip,jp,kp,lp)=0
   8        continue
   7      continue
   6    continue
   5  continue

C Check that the file unit is non-zero.
      if(iunit.eq.0)then
        call usrmsg('EPWTOBCLM: Problem with zero file unit for...',
     &    LASCI,'W')
        ier = 1
        return
      endif

C Open the ASCII file for reading.
      CALL EFOPSEQ(IUNIT,LASCI,1,IER)
      if(ier.ne.0)then
        call usrmsg(' Problem opening...',LASCI,'W')
        ier = 1
        return
      endif
      write(currentfile,'(a)') LASCI(1:lnblnk(LASCI))

C Read a line of text from the file, if end of file process data.
  42  CALL LSTRIPC(IUNIT,OUTSTR,99,ND,1,'epw data line',IER)
      if(ier.ne.0)goto 43
      if(OUTSTR(1:8).eq.'LOCATION')then

C Keyword `LOCATION` followed by 9 data fields as follows
C   city <string>, state/province/region <string>, country <string>,
C   field source <string>, IWEC tag <integer>,
C   decimal degrees latitude [-90. to 90.] <real>,
C   decimal degrees longitude [-180. (west) to 180. (east)] <real>
C   time zone hours relative to GMT [-12. to 12.] <real>
C   metre elevation [-1000. to 9999.] <real>
        K=8
        CALL EGETP(OUTSTR,K,city,'W','city name',IER)
        CALL EGETP(OUTSTR,K,provence,'W','state/province',IER)
        CALL EGETP(OUTSTR,K,country,'W','country',IER)
        CALL EGETP(OUTSTR,K,source,'W','source description',IER)
        CALL EGETWI(OUTSTR,K,IWECid,0,0,'-','IWEC id',IER)
        if (IER.ne.0) then
          IWECid=0
          IER=0
        endif
        CALL EGETWR(OUTSTR,K,CFLAT,-90.,90.,'W','CLAT',IER)

C Read long and convert to longitude difference. Multiply time zone
C difference by 15 to get degrees and then take difference with
C actual longitude to get esp-r CLONG
        CALL EGETWR(OUTSTR,K,ALONG,-180.,180.,'W','Actual LONG',IER)
        CALL EGETWR(OUTSTR,K,CTZONE,-12.,12.,'W','Time zone diff',IER)
        CTZ = CTZONE*15.0
        CFLONG = ALONG - CTZ
        CALL EGETWR(OUTSTR,K,CELEV,-1000.,9999.,'W','elevation m',IER)

C Concatenate city state country to CFLOC.
        write(outs,'(5a)') city(1:lnblnk(city)),' ',
     &    provence(1:lnblnk(provence)),' ',country(1:lnblnk(country))
        CFLOC=OUTS(1:42)

C Set radiation flag to direct normal.
        IDNGH=0
      elseif(OUTSTR(1:17).eq.'DESIGN CONDITIONS')then

C Keyword `DESIGN CONDITIONS` if the remainder of line is a 0 then
C none have been defined, otherwise read 26 numbers (see documentation
C for details)
        if(ND.gt.3)then
          CALL EGETRM(OUTSTR,K,dcline,'W','design conditions line',IER)
        endif
      elseif(OUTSTR(1:23).eq.'TYPICAL/EXTREME PERIODS')then

C Keyword `TYPICAL/EXTREME PERIODS` followed by
C   number of periods <integer>,
C   for each period (one line per period?): description <string>,
C     start julian date <integer>, finish julian date <integer>
C        CALL EGETWI(OUTSTR,K,??,0,??,'W','nb of typ periods',IER)
        continue
      elseif(OUTSTR(1:19).eq.'GROUND TEMPERATURES')then

C Keyword `GROUND TEMPERATURES` followed by
C   number of ground temperatures <integer>,
C   for each set (one line per set): metre depth <real>, soil conductivity
C     in W/m-K <real>, soil density in kg/m3 <real>, soil specific
C     heat in J/kg-K, monthly temperatures in deg C <12 reals>
C        CALL EGETWI(OUTSTR,K,??,0,??,'W','nb of ground temps',IER)
        continue
      elseif(OUTSTR(1:24).eq.'HOLIDAYS/DAYLIGHT SAVING')then

C Keyword `HOLIDAYS/DAYLIGHT SAVINGS` followed by
C   leap year observance <string `Yes` `No`>, daylight savings start day <integer>,
C   daylight savings finish day <integer>, number of holiday definitions <integer>
C   for each holiday a description <string> and julian date
C        CALL EGETW(OUTSTR,K,??,'W','yes no for daylight savings',IER)
C        CALL EGETWI(OUTSTR,K,??,1,365,'W','DS start day',IER)
C        CALL EGETWI(OUTSTR,K,??,1,365,'W','DS finish day',IER)
C        CALL EGETWI(OUTSTR,K,??,1,365,'W','holidays',IER)
        continue
      elseif(OUTSTR(1:10).eq.'COMMENTS 1')then
        if(ND.gt.2)then
          K=10
          CALL EGETRM(OUTSTR,K,clmcomment(1),'W','comment 1',IER)
        endif
      elseif(OUTSTR(1:10).eq.'COMMENTS 2')then
        if(ND.gt.2)then
          K=10
          CALL EGETRM(OUTSTR,K,clmcomment(2),'W','comment 2',IER)
        endif
      elseif(OUTSTR(1:12).eq.'DATA PERIODS')then

C Keyword `DATA PERIODS` followed by 6 fields as follows:
C   number of data periods <integer>, number of records per hour <integer>,
C   description <string>, start day of week <string `Sunday` `Monday` `Tuesday`
C   `Wednesday` `Thursday` `Friday` `Saturday`>,
C   period start month/day <integer/integer>, period finish month/day <integer/integer>
        K=12
        CALL EGETWI(OUTSTR,K,IDPER,1,2,'W','nb data periods',IER)
        CALL EGETWI(OUTSTR,K,IREDPH,1,2,'W','nb records per hr',IER)
        if(IREDPH.gt.1)then
          call usrmsg('This EPW file has more than one timestep per',
     &      'hour and cannot be converted to ESP-r format','W')
          ier=5
          return
        endif
        CALL EGETP(OUTSTR,K,dpdescr,'W','data period description',IER)
        CALL EGETW(OUTSTR,K,dayname,'W','day of week start',IER)

C Convert to IDS.
        WORD=' '
        CALL EGETP(OUTSTR,K,WORD,'W','period start',IER)
        if(WORD(3:3).eq.'/')then
          read(WORD(1:2),*)imonstart 
          read(WORD(4:5),*)idaystart
        elseif(WORD(2:2).eq.'/')then
          read(WORD(1:1),*)imonstart
          read(WORD(3:4),*)idaystart 
        endif
        call eday(idaystart,imonstart,IDS)

C Convert to IDF.
        WORD=' '
        CALL EGETP(OUTSTR,K,WORD,'W','period end',IER)
        if(WORD(3:3).eq.'/')then
          read(WORD(1:2),*)imonfn 
          read(WORD(4:5),*)idayfn 
        elseif(WORD(2:2).eq.'/')then
          read(WORD(1:1),*)imonfn 
          read(WORD(3:4),*)idayfn 
        endif
        call eday(idayfn,imonfn,IDF)

      else

C Data line fields are as follows:
C (1) year <integer>, (2) month <integer>, (3) day <integer>, 
C (4) hour <integer>, (5) minute <integer>,
C (6) data source and uncertainty flags <string 44-50 characgers long>,
C (7) dry bulb temp C <real>, (8) dew point temp C <real>, (9) RH% <real>,
C (10) atmospheric pressure Pa <real>, (11) extraterrestrial horizontal rad Wh/m2 <real>,
C (12) extraterrestrial direct normal radiation Wh/m2 <real>,
C (13) horiz infrared radiation from sky Wh/m2 <real>,
C (14) global horiz radiation Wh/m2 <real>, (15) direct normal Wh/m2 <real>,
C (16) diffuse horiz radiation Wh/m2 <real>, (17) global horiz illum lux <real>,
C (18) direct normal illum lux <real>, (19) diffuse horiz illum lux <real>,
C (20) Zenith illum Cd/m2 <real>, (21) wind direction degrees <real>,
C (22) wind speed m/sec <real>, (23) total sky cover <integer>,
C (24) opaque sky cover <integer>, (25) km of visibility <real>,
C (26) cloud ceiling metres <real>, (27) present weather observation <integer>,
C (28) present weather codes <integer>, (29) mm of precipitable water <real>,
C (30) aerosol optical depth <thousandths>,
C (31) cm of snow depth <integer>, (32) days since last snowfall <integer>.
C (33) Albedo < real >
C (34) Liquid precipitation depth mm < real >
C (35) Liquid precipitation rate mm/hr < real >

C Loop through days. If first line then set the year.
        irecloop=irecloop+1
        K=0
        CALL EGETWI(OUTSTR,K,lyear,1900,2051,'-','line year',IER)
        if(irecloop.eq.1) CFYEAR=lyear
        CALL EGETWI(OUTSTR,K,lmonth,1,12,'W','line month',IER)
        CALL EGETWI(OUTSTR,K,lday,1,31,'W','line day',IER)
        CALL EGETWI(OUTSTR,K,lhour,1,24,'W','line hour',IER)
        CALL EGETWI(OUTSTR,K,lminute,0,60,'W','line minute',IER)
        CALL EGETP(OUTSTR,K,uncert,'W','uncertency flags',IER)
        CALL EGETWR(OUTSTR,K,ambdbt ,0.0,0.0,'-','amb db T',IER)
        ITMP=nint(10.0*ambdbt)
        CALL EGETWR(OUTSTR,K,dewpt,0.0,0.0,'-','dewpoint T',IER)
        CALL EGETWR(OUTSTR,K,ambrh,0.0,100.0,'W','RH ',IER)
        IHUM=nint(ambrh)
        CALL EGETWR(OUTSTR,K,ambpa,0.0,0.0,'-','atmos press Pa ',IER)
        iambpatot=iambpatot+1
        call ECLOSE(ambpa,999999.,0.1,close)
        if (close) then ! data not available
          IATM=101325
        else
          iambpaok=iambpaok+1
          IATM=nint(ambpa)
        endif
        CALL EGETWR(OUTSTR,K,extthr,0.0,0.0,'-','extrter hor rad',IER)
        CALL EGETWR(OUTSTR,K,exttdnr,0.0,0.0,'-','extrter DN rad',IER)
        CALL EGETWR(OUTSTR,K,exttir,0.0,0.0,'-','extrter infrared',IER)

C In future might ask user if global horizontal or direct normal should
C be used. Assume that direct normal radiation value is correct and
C use it.
        CALL EGETWR(OUTSTR,K,globhor,0.0,1200.0,'W','glob horiz',IER)
        CALL EGETWR(OUTSTR,K,direcnor,0.0,1200.0,'W','direct normal R',
     &    IER)
        IDNR=nint(direcnor)
        CALL EGETWR(OUTSTR,K,difhoriz,0.0,1000.0,'W','diffuse horiz',
     &    IER)
        IDIF=nint(difhoriz)
        CALL EGETWR(OUTSTR,K,globhlux,0.0,0.0,'-','glob horiz illum',
     &    IER)
        CALL EGETWR(OUTSTR,K,dirnorlux,0.0,0.0,'-','dir normal illum',
     &    IER)
        CALL EGETWR(OUTSTR,K,difholux,0.0,0.0,'-','diff horiz illum',
     &    IER)
        CALL EGETWR(OUTSTR,K,zenithlux,0.0,0.0,'-','zenith illum',IER)
        CALL EGETWR(OUTSTR,K,winddir,-360.0,360.0,'W','wind direc',IER)
        IDIR=nint(winddir)
        CALL EGETWR(OUTSTR,K,winspeed,0.0,99.0,'W','wind speed',IER)
        IVEL=nint(10.0*winspeed)
        CALL EGETWI(OUTSTR,K,itotskycov,0,100,'W','total sky cover',IER)
        itsctot=itsctot+1
        if (itotskycov.eq.99) then ! data not available
          ITSC=0
        else
          itscok=itscok+1
          ITSC=itotskycov
        endif
        CALL EGETWI(OUTSTR,K,iopqskycov,0,100,'W','opaq sky cover',IER)
        iosctot=iosctot+1
        if (iopqskycov.eq.99) then ! data not available
          IOSC=0
        else
          ioscok=ioscok+1
          IOSC=iopqskycov
        endif
        CALL EGETWR(OUTSTR,K,viskm,0.0,0.0,'-','km of visibility',IER)
        CALL EGETWR(OUTSTR,K,cldceil,0.0,0.0,'-','cloud ceiling m',IER)
        CALL EGETWI(OUTSTR,K,iobserv,0,0,'-',
     &    'present weather observation',IER)
        CALL EGETWI(OUTSTR,K,icodes,0,0,'-','present weather codes',IER)
        CALL EGETWR(OUTSTR,K,precepmm,0.0,0.0,'-',
     &    'mm of precipitable water',IER)
        CALL EGETWR(OUTSTR,K,aerosol,0.0,0.0,'-',
     &    'aerosol optical depth',IER)
        CALL EGETWR(OUTSTR,K,cmsnow,0.0,0.0,'-','cm of snow depth',IER)
        CALL EGETWI(OUTSTR,K,idaysnow,0,0,'-',
     &     'days since last snowfall',IER)

C << update for albedo, percipitation depth and percipitation rate >>

C Need to place current data into array structure prior to writing
C out data to the binary file. Since going from integer ESP units
C to ESP units in binary file no need to worry about truncation errors.
        ICLMEPW(lmonth,lday,lhour,1)=ITMP
        ICLMEPW(lmonth,lday,lhour,2)=IDIF
        ICLMEPW(lmonth,lday,lhour,3)=IDNR
        ICLMEPW(lmonth,lday,lhour,4)=0
        ICLMEPW(lmonth,lday,lhour,5)=IVEL
        ICLMEPW(lmonth,lday,lhour,6)=IDIR
        ICLMEPW(lmonth,lday,lhour,7)=IHUM
        ICLMEPW(lmonth,lday,lhour,8)=ITSC
        ICLMEPW(lmonth,lday,lhour,9)=IOSC
        ICLMEPW(lmonth,lday,lhour,10)=IATM
        NCM=6 ! this may be updated later after we check optional metrics
        CMCOL(1:7)=(/1,2,3,0,4,5,6/)
        CMXST(1:7)=(/.true.,.true.,.true.,.false.,.true.,.true.,.true./)

C Currently, we will never have global horizontal from an EPW.
C cfind maps columns in the new file to weather metrics. This is needed
C even if the metric isn't included, because of the checks for missing
C data below. We assume global horizontal would always have been the
C last column in ICLMEPW. Column indices for optional metrics should be
C 0, and are set below after checking for missing data.
        cfind(1:10)=(/1,2,3,5,6,7,0,0,0,4/)
      endif
      goto 42

  43  continue

C Check if optional weather metrics are available.
C If there is no data, flag the metric as unavailable.
C If all data is present, use it.
C If there is some data, but not all, ask the user.
      ioptcmskip=0
      if (itotskycovok.gt.0) then
        if (itotskycovok.lt.itotskycovtot) then
          rtmp=real(itotskycovok)/real(itotskycovtot)*100.
          write(outs,'(a,f5.1,a)')
     &      'Only ',rtmp,'% of total cloud cover data is available!'
          helptopic='missing_cld_data'
          call gethelptext(helpinsub,helptopic,nbhelp)
          call EASKOK(outs,'Use it?',close,nbhelp)
          if (close) then
            NCM=NCM+1
            CMCOL(8)=NCM
            cfind(NCM)=8
            CMXST(8)=.true.
          else
            CMCOL(8)=0            
            cfind(MCM-1-ioptcmskip)=8
            ioptcmskip=ioptcmskip+1
            CMXST(8)=.false.
          endif
        else
          NCM=NCM+1
          CMCOL(8)=NCM
          cfind(NCM)=8
          CMXST(8)=.true.
        endif
      else        
        CMCOL(8)=0
        cfind(MCM-1-ioptcmskip)=8
        ioptcmskip=ioptcmskip+1
        CMXST(8)=.false.
      endif

      if (iopqskycovok.gt.0) then
        if (iopqskycovok.lt.iopqskycovtot) then
          rtmp=real(iopqskycovok)/real(iopqskycovtot)*100.
          write(outs,'(a,f5.1,a)')
     &      'Only ',rtmp,'% of oqaque cloud cover data is available!'
          call EASKOK(outs,'Use it?',close,nbhelp)
          if (close) then
            NCM=NCM+1
            CMCOL(9)=NCM
            cfind(NCM)=9
            CMXST(9)=.true.
          else
            CMCOL(9)=0            
            cfind(MCM-1-ioptcmskip)=9
            ioptcmskip=ioptcmskip+1
            CMXST(9)=.false.
          endif
        else
          NCM=NCM+1
          CMCOL(9)=NCM
          cfind(NCM)=9
          CMXST(9)=.true.
        endif
      else        
        CMCOL(9)=0
        cfind(MCM-1-ioptcmskip)=9
        ioptcmskip=ioptcmskip+1
        CMXST(9)=.false.
      endif

      if (iambpaok.gt.0) then
        if (iambpaok.lt.iambpatot) then
          rtmp=real(iambpaok)/real(iambpatot)*100.
          write(outs,'(a,f5.1,a)')
     &      'Only ',rtmp,'% of atmospheric pressure data is available!'
          helptopic='missing_prs_data'
          call gethelptext(helpinsub,helptopic,nbhelp)
          call EASKOK(outs,'Use it?',close,nbhelp)
          if (close) then
            NCM=NCM+1
            CMCOL(10)=NCM
            cfind(NCM)=10
            CMXST(10)=.true.
          else
            CMCOL(10)=0            
            cfind(MCM-1-ioptcmskip)=10
            ioptcmskip=ioptcmskip+1
            CMXST(10)=.false.
          endif
        else
          NCM=NCM+1
          CMCOL(10)=NCM
          cfind(NCM)=10
          CMXST(10)=.true.
        endif
      else        
        CMCOL(10)=0
        cfind(MCM-1-ioptcmskip)=10
        ioptcmskip=ioptcmskip+1
        CMXST(10)=.false.
      endif

      write(6,*)'CMXST',CMXST
      write(6,*)'CMCOL',CMCOL
      write(6,*)'cfind',cfind

C Write out information.
      do 51 im=1,12
        if(im.eq.1.or.im.eq.3.or.im.eq.5.or.im.eq.7)last=31
        if(im.eq.2)last=28
        if(im.eq.4.or.im.eq.6.or.im.eq.9.or.im.eq.11)last=30
        if(im.eq.8.or.im.eq.10.or.im.eq.12)last=31
        do 61 id=1,last
          CALL EDAY(id,im,IDD)

C Zero data that we do not wish to include.
          IREC=IDD
          do k=1,MCM
            if (.not.CMXST(k)) then
              do j=1,24
                ICLMEPW(im,id,J,K)=0
              enddo
            endif
          enddo

C Transfer this to the binary file.
          WRITE(ICLIM,REC=IREC,IOSTAT=ISTAT,ERR=1002)
     &       ((ICLMEPW(im,id,J,cfind(K)),K=1,MCM),J=1,24)
   61   continue
   51 continue

C Insert climatic year.
      IREC=366
      WRITE(ICLIM,REC=IREC,IOSTAT=ISTAT,ERR=1001)CFYEAR

C Insert location, latitude and longitude.
      IREC=IREC+1
      WRITE(ICLIM,REC=IREC,IOSTAT=ISTAT,ERR=1001)
     &  CFLOC,CFLAT,CFLONG

C Insert metric flags.
      IREC=IREC+1
      WRITE(ICLIM,REC=IREC,IOSTAT=ISTAT,ERR=1001)
     &  (CMCOL(i),i=1,MCM)

C Insert version - use latest version for the database no matter what
C version of ASCII we were given.
      CFVER=NMCM
      CFMCM=MCM
      IREC=IREC+1
      WRITE(ICLIM,REC=IREC,IOSTAT=ISTAT,ERR=1001)CFVER

      CALL CLMMDN2O

C Proposed additional records:
C IREC=369 text from comment line 1
C IREC=370 text from commect line 2
C IREC=371 number of design conditions, number of typical/extreme periods,
C number of ground temperatures, number of holidays, daylight flag, daylight
C start, daylight finish, data period start date, data period finish date
C IREC=372 ??
C IREC=373 ??
C IREC=374 ??
C IREC=375 ??
C IREC=376 ??
C IREC=377 ??

C Close ASCIIfile.
      CALL ERPFREE(IUNIT,ISTAT)
      RETURN

 1001 WRITE(outs,775)ISTAT
  775 FORMAT(' Error ',I7,' writing weather database metadata')
      call edisp(iuout,outs)
      CALL ERPFREE(IUNIT,ISTAT)
      RETURN

 1002 WRITE(outs,777)ISTAT
  777 FORMAT(' Error ',I7,' writing weather database')
      call edisp(iuout,outs)
      CALL ERPFREE(IUNIT,ISTAT)
      RETURN

      END





