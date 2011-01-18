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
C         to be transferred to a binary climate file.
C MKACLM: Allows any period of climatic data in binary climate file to
C         be transferred to an ASCII file.
C MKOREANCLM: Allows a period of climatic data in a suitable ASCII file
C         (KOREAN) to be transferred to a binary climate file.
C CFILIN: Takes a climate file name from a browse list, command
C         line or default and intialise it or open.
C CLMPUT: Writes one day's climate data to the current climate file.
C EPWTOBCLM: Allows a period of climatic data from EPW format ASCII file
C         to be transferred to a binary climate file.

C 'MKBCLM' Allows a period of climatic data in a suitable ASCII file
C to be transferred to a binary climate file.  The ASCII file should 
C contain the following values for each hour:
C col     description                 unit            variable
C 1. Diffuse solar on the horizontal (W/M**2)         IDIF
C 2. External dry bulb temperature   (TENTHS DEG.C)   ITMP
C 3. Direct normal solar intensity   (W/M**2)         IDNR
C    or
C    Global horizontal radiation (W/M**2)
C 4. Prevailing wind speed           (TENTHS M/S)     IVEL
C 5. Wind direction                  (DEGREES)        IDIR
C 6. Relative humidity               (PERCENT)        IHUM

C Each data line is comma separated and all values should be integers.
C Parameters 2 & 4 have been multiplied by 10 ie.  12.3 degrees
C is represented in the file as 123.  There should be 24 lines for
C each day of the period being transferred.
C Assume that the climate file has already been opened elsewhere.

C Item 3 can be replaced by global horizontal radiation (W/M**2),
C with the radiation flag set appropriately after the file has been
C read in.
C silent is a logical toggle, if .true. then do not interact with user.
      SUBROUTINE MKBCLM(ITRU,ICDESC,LASCI,IUNIT,IDS,IDF,silent,IER)
#include "esprdbfile.h"

      common/pophelp/h(60)
      COMMON/OUTIN/IUOUT,IUIN

      COMMON/CLMDT1/CLMLOC
      COMMON/CLMSET/ICYEAR,ICDNGH,CLAT,CLONG
      COMMON/SET1/IYEAR,IBDOY,IEDOY,IFDAY,IFTIME
      COMMON/RADTYP/IDNGH
      LOGICAL OK,DOK,silent
      dimension ICLM(24,6)

      CHARACTER CLMLOC*30
      CHARACTER H*72,LASCI*72,outs*124,OUTSTR*124

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

      if(silent)then
        IREAD=1
      else
        dok=.true.
        h(1)='If the ASCII file was exported from the climate'
        h(2)='module it will have a marker line at the start of'
        h(3)='each day. If the file does not have these markers'
        h(4)='then say no. '
        CALL ASKOK(' ',
     &    ' Does ASCII file have day demarcation lines?',OK,dok,4)
        IF(OK)IREAD=1
      endif

C Check and see if first line is *CLIMATE, if so read in site data.
C If the user is not requesting new site data then skip over lines. 
      CALL STRIPC(IUNIT,OUTSTR,0,ND,1,'line 1',IER)
      if(OUTSTR(1:8).eq.'*CLIMATE')then
        IF(ICDESC.NE.1)then
          CALL STRIPC(IUNIT,OUTSTR,0,ND,1,'site name',IER)
          CALL STRIPC(IUNIT,OUTSTR,0,ND,1,'yr lat lng idngh',IER)
          CALL STRIPC(IUNIT,OUTSTR,0,ND,1,'julian period',IER)
        elseIF(ICDESC.eq.1)then
          CALL STRIPC(IUNIT,OUTSTR,0,ND,1,'site name',IER)
          CLMLOC=OUTSTR(1:30)

          CALL STRIPC(IUNIT,OUTSTR,4,ND,1,'yr lat lng idngh',IER)
          K=0
          CALL EGETWI(OUTSTR,K,IYEAR,1900,2020,'W','year',IER)
          CALL EGETWR(OUTSTR,K,CLAT,-90.,90.,'W','CLAT',IER)
          CALL EGETWR(OUTSTR,K,CLONG,-15.,15.,'W','CLONG',IER)
          CALL EGETWI(OUTSTR,K,IDNGH,0,123,'W','IDNGH',IER)

          CALL STRIPC(IUNIT,OUTSTR,2,ND,1,'julian period',IER)
          K=0
          CALL EGETWI(OUTSTR,K,IDS,1,365,'W','ascii start',IER)
          CALL EGETWI(OUTSTR,K,IDF,1,365,'W','ascii end',IER)
        endif
      else

C First line was not *CLIMATE so old style, back up and to on to
C standard data reads.
        rewind(iunit,ERR=1001)

C If user has requested that new site information be inserted ask
C such questions now.
        IF(ICDESC.EQ.1)then

C Define the year if not already done.
          H(1)='The year is required to find the day of the week '
          H(2)='for a given julian day.'
          CALL EASKI(IYEAR,' ',' What is the year ? ',
     &             1900,'W',2005,'W',2003,'year',IER,2)

          CLMLOC=' '
          H(1)='Site name (30 char) for descriptive purposes. '
          CALL EASKS(CLMLOC,' ',' Climate site name ? ',
     &      30,'North_Pole','climate site. ',IER,1)

          H(1)='The climate latitude does not necessarily have to '
          H(2)='match the site latitude.  However, the difference '
          H(3)='should not be great or simulation results may be '
          H(4)='open to question.'
          CALL EASKR(CLAT,' ',' Climate latitude ? ',
     &             -89.9,'W',89.9,'W',30.0,'climate latitude',IER,4)

          CALL EASKR(CLONG,' ',' Longitude diference ?',
     &             -14.9,'W',14.9,'W',0.0,'climate long',IER,4)
        endif
      endif

C Loop through days.
      DO 10 I=IDS,IDF
        IDD=I
        IF(IREAD.EQ.1)then
          CALL STRIPC(IUNIT,OUTSTR,0,ND,1,'day ln',IER)
        endif
        DO 20 J=1,24

C Read one hour's data from ASCII file.
          CALL STRIPC(IUNIT,OUTSTR,0,ND,1,'climate data',IER)
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
   20   CONTINUE

C Transfer this to the binary file.
        IREC=IDD
        WRITE(ICLIM,REC=IREC,IOSTAT=ISTAT,ERR=1002)
     &       ((ICLM(J,K),K=1,6),J=1,24)
   10 CONTINUE

C Insert Climatic Year.
      IREC=366
      ICDNGH=IDNGH
      WRITE(ICLIM,REC=IREC,IOSTAT=ISTAT,ERR=1001)IYEAR,ICDNGH
      IREC=367
      WRITE(ICLIM,REC=IREC)CLMLOC
      IREC=368
      WRITE(ICLIM,REC=IREC)CLAT,CLONG

C Close ASCIIfile.
   19 CALL ERPFREE(IUNIT,ISTAT)
      RETURN

 1001 WRITE(outs,775)ISTAT
  775 FORMAT(' Error ',I7,' writing climate database year')
      call edisp(iuout,outs)
      GOTO 19

 1002 WRITE(outs,777)ISTAT
  777 FORMAT(' Error ',I7,' writing climate database')
      call edisp(iuout,outs)
      GOTO 19

      END

C MKACLM is the same code as in edatabase.f
C ************* MKACLM *************************************************
C 'MKACLM' Allows any period of climatic data in binary climate file to
C be transferred to an ASCII file - if act = E then and ESP-r file and
C if act = S an SQL tab separated file.  
C If file will begin with the key phrase *CLIMATE then it begins
C with site information.

C The ASCII file will contain the following values for each hour:
C col     description                 unit            variable
C 1. Diffuse solar on the horizontal (W/m**2)         IDIF
C 2. External dry bulb temperature   (TENTHS DEG.C)   ITMP
C 3. Direct normal solar intensity   (W/m**2)         IDNR
C    or
C    Global horizontal radiation (W/M**2)
C 4. Prevailing wind speed           (TENTHS m/s)     IVEL
C 5. Wind direction                                   IDIR
C 6. Relative humidity               (PERCENT)        IHUM

C Each line will hold 6 space separated values (as integers).
C Parameters 2 & 4 have been multiplied by 10 ie.  12.3 degrees are
C represented in the file as 123.  There should be 24 lines for each day
C of the period being transferred.

C Item 3 will be replaced by global horizontal radiation (W/M**2),
C if the radiation flag set was set to GH in the binary climate file.
      SUBROUTINE MKACLM(ACT,LASCI,IAF,IBD,IED,IER)
#include "esprdbfile.h"
      
      integer lnblnk  ! function definition

      COMMON/OUTIN/IUOUT,IUIN
      COMMON/CLMSET/ICYEAR,ICDNGH,CLAT,CLONG
      COMMON/SET1/IYEAR,IBDOY,IEDOY,IFDAY,IFTIME
      COMMON/RADTYP/IDNGH
      COMMON/CLMDT1/CLMLOC
      DIMENSION ICLM(24,6)

      CHARACTER LASCI*72,outs*124,CLMLOC*30,outsd*124
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

C Open ASCII file to receive climate data. If file exists, user must
C confirm whether it should be overwritten. Open can be local or remote.
      IF(IER.LT.0)THEN
        IER=1
        RETURN
      ENDIF

      if(act.eq.'E')then
        WRITE(IAF,'(A)',IOSTAT=ISTAT,ERR=1003)'*CLIMATE'

C Create a header section with site name, period etc.
        if(IDNGH.eq.0)then
          WRITE(IAF,30,IOSTAT=ISTAT,ERR=1003)LCLIM(1:lnblnk(LCLIM)),
     &            LASCI(1:lnblnk(LASCI))
  30      FORMAT('# ascii climate file from ',a,' binary db,',/,
     &    '# defined in: ',a,/,
     &    '# col 1: Diffuse solar on the horizontal (W/m**2)',/,
     &    '# col 2: External dry bulb temperature   (Tenths DEG.C)',/,
     &    '# col 3: Direct normal solar intensity   (W/m**2)',/,
     &    '# col 4: Prevailing wind speed           (Tenths m/s) ',/,
     &    '# col 5: Wind direction     (clockwise deg from north) ',/,
     &    '# col 6: Relative humidity               (Percent)')
        else
          WRITE(IAF,31,IOSTAT=ISTAT,ERR=1003)LCLIM(1:lnblnk(LCLIM)),
     &            LASCI(1:lnblnk(LASCI))
  31      FORMAT('# ascii climate file from ',a,' binary db,',/,
     &    '# defined in: ',a,/,
     &    '# col 1: Diffuse solar on the horizontal (W/m**2)',/,
     &    '# col 2: External dry bulb temperature   (Tenths DEG.C)',/,
     &    '# col 3: Global horizontal solar intensity   (W/m**2)',/,
     &    '# col 4: Prevailing wind speed           (Tenths m/s) ',/,
     &    '# col 5: Wind direction     (clockwise deg from north) ',/,
     &    '# col 6: Relative humidity               (Percent)')
        endif
        WRITE(IAF,'(A,A)',IOSTAT=ISTAT,ERR=1003)CLMLOC,'  # site name'
        outs=' '
        WRITE(outs,'(I6,2F8.2,I4)',IOSTAT=ISTAT,ERR=1002)IYEAR,CLAT,
     &      CLONG,IDNGH
        call SDELIM(outs,outsd,'C',IW)

C When writing clarify which type of solar radiation is used.
        if(IDNGH.eq.0)then
          write(IAF,'(1x,2a)')outsd(1:lnblnk(outsd)),
     &      '   # year, latitude, long diff, direct normal rad flag'
        else
          write(IAF,'(1x,2a)')outsd(1:lnblnk(outsd)),
     &      '   # year, latitude, long diff, global horiz rad flag'
        endif
        outs=' '
        WRITE(outs,'(2I6)',IOSTAT=ISTAT,ERR=1003)IBD,IED
        call SDELIM(outs,outsd,'C',IW)
        write(IAF,'(1x,2a)')outsd(1:lnblnk(outsd)),
     &    '    # period (julian days)'
      elseif(act.eq.'S')then
        lcl=lnblnk(CLMLOC)
        WRITE(IAF,'(15A)',IOSTAT=ISTAT,ERR=1003)'climate_site',tab,
     &    'datetime',tab,'db',tab,'dir_solar',tab,'diff_solar',tab,
     &    'wind_speed',tab,'wind_dir',tab,'rh'
      endif
 
      IDS=IBD
      IDF=IED
      DO 10 I=IDS,IDF

C Read in one day's data from binary file.
        IDD=I
        IREC=IDD
        READ(ICLIM,REC=IREC,IOSTAT=ISTAT,ERR=1000)
     &      ((ICLM(J,K),K=1,6),J=1,24)

        CALL EDAYR(IDD,IDY,IMN)
        if(act.eq.'E')then
          WRITE(IAF,12,IOSTAT=ISTAT,ERR=1001)IDY,IMN
   12     FORMAT('* day',I3,' month',I3)
        endif
        DO 20 J=1,24

C Now transfer data hour-by-hour to ASCII file.
          outs=' '
          if(act.eq.'E')then
            WRITE(outs,'(6I7)',IOSTAT=ISTAT,ERR=1002)(ICLM(J,K),K=1,6)
            call SDELIM(outs,outsd,'C',IW)
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
            db = real(iclm(j,2))/10.0
            dir_solar = real(iclm(j,3))
            diff_solar = real(iclm(j,1))
            wind_speed = real(iclm(j,4))/10.0
            wind_dir = real(iclm(j,5))
            rh = real(iclm(j,6))

C Write tab separated data out. Prepend whatever the current climate
C location description is for the site.
            WRITE(IAF,'(4A,f5.1,a,f6.1,a,f6.1,a,f6.1,a,f6.1,a,f5.1)',
     &        IOSTAT=ISTAT,ERR=1003) CLMLOC(1:lcl),tab,msg,tab,db,tab,
     &        dir_solar,tab,diff_solar,tab,wind_speed,tab,wind_dir,tab,
     &        rh
          endif
   20   CONTINUE
   10 CONTINUE

C Close ASCII file.
   15 CALL ERPFREE(IAF,ISTAT)
      RETURN

 1000 WRITE(outs,776)ISTAT,IREC
  776 FORMAT(' Error ',I4,' reading binary file record ',I4)
      call edisp(iuout,outs)
      GOTO 15
 1001 WRITE(outs,775)ISTAT
  775 FORMAT(' Error ',I4,' writing demarcation')
      call edisp(iuout,outs)
      GOTO 15

 1002 WRITE(outs,777)ISTAT
  777 FORMAT(' Error ',I4,' writing hourly data')
      call edisp(iuout,' ')
      GOTO 15

 1003 WRITE(outs,778)ISTAT
  778 FORMAT(' Error ',I7,' writing site data')
      call edisp(iuout,outs)
      GOTO 15

      END

C 'MKOREANCLM' Allows a period of climatic data in a suitable ASCII file
C to be transferred to a binary climate file.  The ASCII file should 
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
C Assume that the climate file has already been opened elsewhere. Note some
C files may have first line STN_ID if this is found skip and try next.

C silent is a logical toggle, if .true. then do not interact with user.
      SUBROUTINE MKOREANCLM(LASCI,IUNIT,ACT,IER)
#include "esprdbfile.h"

C Parameters
      character LASCI*72 ! file name to import
      integer IUNIT      ! unit number of the file (assumed already opened)
      character ACT*3    ! old pre 2006 files, new current files.

      COMMON/OUTIN/IUOUT,IUIN
      common/pophelp/h(60)
      
      COMMON/CLMDT1/CLMLOC
      COMMON/CLMSET/ICYEAR,ICDNGH,CLAT,CLONG
      COMMON/SET1/IYEAR,IBDOY,IEDOY,IFDAY,IFTIME
      COMMON/RADTYP/IDNGH
      COMMON/APPRDIFF/iapprox_diff

      dimension ICLM(24,6)

      CHARACTER CLMLOC*30,outs*124
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

      character h*72
      real longit_for_diffuse_calcs,differ_from_Greenwich
      integer IDRAD

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
      H(1)='The climate longitude does not necessarily have to '
      H(2)='match the site latitude.  However, the difference '
      H(3)='should not be great or simulation results may be '
      H(4)='open to question.'
      CALL EASKR(longit_for_diffuse_calcs,' ',
     &           ' Climate longitude (actual)? (west: positive) ',
     &           -180.0,'W',180.0,'W',30.0,'climate longitude',IER,4)

      H(1)='The local time difference from Greenwich is needed.'
      H(2)='Assume west is positive (e.g. use -2 for Greece) '
      CALL EASKR(differ_from_Greenwich,' ',
     &           ' Time difference from Greenwich? (west: positive) ',
     &           -24.0,'W',24.0,'W',0.0,'time_difference',IER,2)

C Export climate data to ASCII file (with similar name).
      h(1)='The default conversion model is based on a publication'
      h(2)='from Muneer: Solar Radiation and Daylight models (1997)'
      h(3)='ISBN: 0-7506-2495-7.'
      h(4)=' '
      h(5)='The Maxwell model is equation 3.4.2a and 3.4.2b of '
      h(6)='Muneer book on page 102. '
      h(7)='The Erbs, Klein and Duffie model is based on empirical '
      h(8)='correlations, documented in: Erbs D.G., Klein S.A., '
      h(9)='Duffie J.A. 1982. Estimation of the diffuse radiation'
      h(10)='fraction for hourly, daily and monthly-average global '
      h(11)='radiation. Solar Energy 28: pp.293-302.'
      h(12)=' '
      h(13)='The three methods tend to give different estimates of'
      h(14)='diffuse radiation for a given global horizontal solar'
      h(15)='radiation value. The default method tends to give higher'
      h(16)='diffuse values. '
      h(17)=' '
      h(18)='If in doubt, create a climate file for each of the'
      h(19)='methods and visually inspect the predictions.'
      h(20)=' '
      call easkabc
     &  ('There are three available models to approximate diffuse:',
     &  ' ','Muneer (default)','Maxwell','Erbs Klein & Duffie',
     &  IDIFF,20)

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
          CALL EGETWI(OUTSTR,K,iyr,1900,2020,'W','year index',IER)
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
            IGLOBH=IGLOBH*277
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

C Insert Climatic Year.
      IREC=366
      ICDNGH=IDNGH
      WRITE(ICLIM,REC=IREC,IOSTAT=ISTAT,ERR=1001)IYEAR,ICDNGH
      IREC=367
      WRITE(ICLIM,REC=IREC)CLMLOC
      IREC=368
      WRITE(ICLIM,REC=IREC)CLAT,CLONG

C Close ASCIIfile.
      CALL ERPFREE(IUNIT,ISTAT)
      call edisp(iuout,'Closing the Korean Met office file.')

      return  ! normal exit from subroutine

 1001 WRITE(outs,775)ISTAT
  775 FORMAT(' Error ',I7,' writing climate database year')
      call edisp(iuout,outs)
      return

 1002 WRITE(outs,777)ISTAT
  777 FORMAT(' Error ',I7,' writing climate database')
      call edisp(iuout,outs)
      return

      end

C ----------- CFILIN 
C CFILIN takes a climate file name from a browse list, command
C line or default and intialise it or open a zero length file if
C a new one is requested.
C silent is a logical toggle: true keeps user interactions to a minimum.
      SUBROUTINE CFILIN(silent)
#include "esprdbfile.h"
#include "espriou.h"
      
      integer lnblnk  ! function definition
      integer igraphiclib  ! external definition

      COMMON/OUTIN/IUOUT,IUIN
      COMMON/INPER/INIT
      COMMON/FILEP/IFIL
      COMMON/CLMDT1/CLMLOC
      COMMON/SET1/IYEAR,IBDOY,IEDOY,IFDAY,IFTIME
      COMMON/CLMSET/ICYEAR,ICDNGH,CLAT,CLONG
      COMMON/PERC/ID1,IM1,IT1,ID2,IM2,IT2,IDS,IDF,INEW
      COMMON/RADTYP/IDNGH
      common/pophelp/h(60)
      common/rcmd/LCMDFL

      logical XST,ok,silent,unixok
      
C Logical rescan = true if climatelist needs to be rescanned to
C pick up typical periods (i.e. if climate file name typed in
C or passed into clm module).
      logical rescan

      CHARACTER CLMLOC*30,H*72,outs*124,tab*1,outs248*248
      character LCMDFL*144,clmdb*144,L144*144
      tab = CHAR(9)

C Get help message set.
      H(1)='A climate file holds hourly values for diffuse solar'
      H(2)='intensity in W/m^2, dry bulb temp in tenths degrees C, '
      H(3)='direct normal or global horizontal solar intensity'
      H(4)='in W/m^2, wind speed in tenths m/s, wind direction deg'
      H(5)='from north (clockwise), and relative humidity in '
      H(6)='percent.  It also holds latitude, longitude difference'
      H(7)='from the nearest time zone and the year represented.'
      H(8)=' '
      H(9)='The normal location for climate files is:'
      H(10)=' /usr/esru/esp-r/climate '
      H(11)=' '
      H(12)='and any of these files may be analysed with clm.'
      H(13)=' '
      H(14)='Climate files are either test reference years, actual'
      H(15)='climate data or synthetic data.'
      rescan=.false.
      IUF=ifil+1

C If LCMDFL is 'UNKNOWN' then scan list of climate db files 
C (the name of which is in the users .esprc file) (or from the
C standard '/usr/esru/esp-r/climate/climatelist'. If clm is
C running in silent mode the user is not expected to select
C from a list so might as well exit the program.
      if(LCMDFL(1:7).eq.'UNKNOWN'.or.LCMDFL(1:2).eq.'  ')then
        if(silent)then
          write(outs248,'(3a)')'Climate exited because input file was',
     &      tab,LCMDFL(1:lnblnk(LCMDFL))
          call isunix(unixok)
          if(unixok)then  ! do not bother if DOS
            write(6,*)outs248(1:lnblnk(outs248))
          endif
          CALL EPAGEND
          STOP
        endif

        INQUIRE (FILE=cdblfil,EXIST=XST)
        if(XST)then

C rdblist returns typper common block for the selected
C climate file.
          if(IUF.eq.0)then
            IUF=ifil+1
          endif
          call usrmsg('Please select a climate set from the ',
     &                'list provided... ','-')
          call rdblist(IUF,clmdb,ier)
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
              CALL EASKF(LCMDFL,' New climate file (esp-r binary)?',
     &         ' ',96,DCLIM,'new climate file',IER,15)
            elseif(iglib.eq.2)then
              CALL EASKF(LCMDFL,' New climate file (esp-r binary)?',
     &         ' ',144,DCLIM,'new climate file',IER,15)
            else
              CALL EASKF(LCMDFL,' New climate file (esp-r binary)?',
     &         ' ',96,DCLIM,'new climate file',IER,15)
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
     &    ' No `climate db` list available so using default... ')
          call edisp(iuout,
     &    ' (Check with your administrator about `climatelist` file)')
          write(LCMDFL,'(a)') DCLIM(1:lnblnk(DCLIM))
 289      continue
          iglib = igraphiclib()  ! find out if X11 or GTK or text support only.
          if(iglib.eq.1.or.iglib.eq.3)then
            CALL EASKF(LCMDFL,' Climate file (esp-r binary format)?',
     &        ' ',96,DCLIM,'climate file',IER,15)
          elseif(iglib.eq.2)then
            CALL EASKF(LCMDFL,' Climate file (esp-r binary format)?',
     &        ' ',144,DCLIM,'climate file',IER,15)
          else
            CALL EASKF(LCMDFL,' Climate file (esp-r binary format)?',
     &        ' ',96,DCLIM,'climate file',IER,15)
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
              CALL EASKF(L144,' Climate file (esp-r binary format)?',
     &          ' ',96,DCLIM,'climate file name',IER,15)
            elseif(llt.ge.96.and.llt.lt.124)then
              CALL EASKF(L144,' Climate file (esp-r binary format)?',
     &          ' ',124,DCLIM,'climate file name',IER,15)
            elseif(llt.ge.124.and.llt.le.144)then
              CALL EASKF(L144,' Climate file (esp-r binary format)?',
     &          ' ',144,DCLIM,'climate file name',IER,15)
            endif
          elseif(iglib.eq.2)then
            CALL EASKF(L144,' Climate file (esp-r binary format)?',
     &        ' ',144,DCLIM,'climate file name',IER,15)
          else
            CALL EASKF(L144,' Climate file (esp-r binary format)?',
     &        ' ',96,DCLIM,'climate file name',IER,15)
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
      CALL FPRAND(ICLIM,ISTAT,144,0,LCMDFL)
      if(ISTAT.eq.-301)then
        CALL ERPFREE(ICLIM,ISTAT)
        CALL FPRAND(ICLIM,ISTAT,144,3,LCMDFL)
        if(.NOT.silent)call edisp(iuout,' opened new climate file')
        llf=lnblnk(LCMDFL)
        write(LCLIM,'(a)') LCMDFL(1:llf)
        RETURN
      elseif(ISTAT.ge.0)then
        CALL ERPFREE(ICLIM,ISTAT)
        CALL FPRAND(ICLIM,ISTAT,144,1,LCMDFL)
        if(.NOT.silent)call edisp(iuout,' opened climate file')
        llf=lnblnk(LCMDFL)
        write(LCLIM,'(a)') LCMDFL(1:llf)
        if(LCLIM.eq.DCLIM)call edisp(iuout,' Default climate assigned.')
      endif

      IREC=366
      READ(ICLIM,REC=IREC,IOSTAT=ISTAT,ERR=1000)IYEAR,IDNGH
      ICYEAR=IYEAR
      ICDNGH=IDNGH
      IF(IYEAR.EQ.0)goto 1000
      IREC=367
      READ(ICLIM,REC=IREC,IOSTAT=ISTAT,ERR=1000)CLMLOC
      IREC=368
      READ(ICLIM,REC=IREC,IOSTAT=ISTAT,ERR=1000)CLAT,CLONG

C << future records to read at this point with periods & documentation >>

C No problems reading file so assume full year period and flag that
C the period has been set (used in main routine to not keep asking
C user for period definition).
      INIT=1
      INEW=1

C If no documentation and periods know scan the climatelist
C file for this information. If no climatelist then the defaults
C set in clm.F will be used.
      if(rescan)then
        IUF=ifil+1
        INQUIRE (FILE=cdblfil,EXIST=XST)
        if(XST)then
          call scancdblist(IUF,LCMDFL,ok,ier)
        endif
      endif
      call edisp(iuout,' ')
      WRITE(outs,'(a,a30)')' Climate data: ',CLMLOC
      call edisp(iuout,outs)

C If IDNGH = 0 then direct normal, otherwise global horizontal
      IF(IDNGH.GT.0)goto 8
      IF(CLAT.LT.0.0)goto 11
      IF(CLONG.LT.0.0)goto 12
      WRITE(outs,103)CLAT,CLONG,IYEAR
 103  FORMAT(1X,F7.1,'N',F7.1,'E',': ',I4,2X,'DN')
      call edisp(iuout,outs)
      RETURN

   12 ACLONG=ABS(CLONG)
      WRITE(outs,104)CLAT,ACLONG,IYEAR
 104  FORMAT(1X,F7.1,'N',F7.1,'W',': ',I4,2X,'DN')
      call edisp(iuout,outs)
      RETURN

   11 ACLAT=ABS(CLAT)
      IF(CLONG.LT.0.0)goto 13
      WRITE(outs,105)ACLAT,CLONG,IYEAR
 105  FORMAT(1X,F7.1,'S',F7.1,'E',': ',I4,2X,'DN')
      call edisp(iuout,outs)
      RETURN

   13 ACLONG=ABS(CLONG)
      WRITE(outs,106)ACLAT,ACLONG,IYEAR
 106  FORMAT(1X,F7.1,'S',F7.1,'W',': ',I4,2X,'DN')
      call edisp(iuout,outs)
      RETURN

    8 IF(CLAT.LT.0.0)goto 14
      IF(CLONG.LT.0.0)goto 15
      WRITE(outs,102)CLAT,CLONG,IYEAR
 102  FORMAT(1X,F7.1,'N',F7.1,'E',': ',I4,2X,'GH')
      call edisp(iuout,outs)
      RETURN

   15 ACLONG=ABS(CLONG)
      WRITE(outs,107)CLAT,ACLONG,IYEAR
 107  FORMAT(1X,F7.1,'N',F7.1,'W',': ',I4,2X,'GH')
      call edisp(iuout,outs)
      RETURN

   14 ACLAT=ABS(CLAT)
      IF(CLONG.LT.0.0)goto 16
      WRITE(outs,108)ACLAT,CLONG,IYEAR
 108  FORMAT(1X,F7.1,'S',F7.1,'E',': ',I4,2X,'GH')
      call edisp(iuout,outs)
      RETURN

   16 ACLONG=ABS(CLONG)
      WRITE(outs,109)ACLAT,ACLONG,IYEAR
 109  FORMAT(1X,F7.1,'S',F7.1,'W',': ',I4,2X,'GH')
      call edisp(iuout,outs)
      RETURN

C New climate file
 1000 call edisp(IUOUT,' No data in file')
      RETURN
      END

C ---------- CLMPUT 
C CLMPUT Writes one day's climate data to the current climate file.
      SUBROUTINE CLMPUT(IDAY)
      PARAMETER (MT=24)
      COMMON/OUTIN/IUOUT,IUIN
      COMMON/FILEP/IFIL
      integer NDAY
      real CDIF,CTMP,CDNR,CVEL,CDIR,CHUM
      COMMON/CLMDAT/NDAY,CDIF(MT),CTMP(MT),CDNR(MT),CVEL(MT),
     A              CDIR(MT),CHUM(MT)
      DIMENSION ICLM(24,6)
      character outs*124

      NDAY=IDAY
      DO 40 J=1,24
        ICLM(J,1)=NINT(CDIF(J))
        ICLM(J,2)=NINT(10.*CTMP(J))
        ICLM(J,3)=NINT(CDNR(J))
        ICLM(J,4)=NINT(10.*CVEL(J))
        ICLM(J,5)=NINT(CDIR(J))
        ICLM(J,6)=NINT(CHUM(J))
   40 CONTINUE
      IREC=NDAY
      WRITE(IFIL,REC=IREC,IOSTAT=ISTAT,ERR=1000)
     &     ((ICLM(J,K),K=1,6),J=1,24)

  50  RETURN
1000  WRITE(IUOUT,'(A,I4,A)') ' Error ',ISTAT,
     &                        ' on writing climate database'
      call edisp(iuout,outs)
      GO TO 50

      END


C 'EPWTOBCLM' Allows a period of climatic data from EPW format ASCII file
C to be transferred to a binary climate file.  The ASCII file should 
C follow the formatting conventions of the EnergyPlus/Esp-r file format.
C All items are comma separated in the EPW file format. Some fields
C may be black (i.e. no space between commas)

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

C The data fields which can slot into the current esp-r file format are:
C (16) diffuse horiz radiation Wh/m2: cast to integer IDIF,
C (7) dry bulb temp C: cast to tenths of deg C ITMP,
C (15) direct normal Wh/m2: cast to integer IDNR,
C (22) wind speed m/sec: cast to tenths m/sec IVEL,
C (21) wind direction degrees: cast to integer degrees IDIR,
C (9) RH%: cast to integer percent IHUM.

C col     description                 unit            variable
C 1. Diffuse solar on the horizontal (W/M**2)         IDIF
C 2. External dry bulb temperature   (TENTHS DEG.C)   ITMP
C 3. Direct normal solar intensity   (W/M**2)         IDNR
C 4. Prevailing wind speed           (TENTHS M/S)     IVEL
C 5. Wind direction                  (DEGREES)        IDIR
C 6. Relative humidity               (PERCENT)        IHUM

C Item 3 can be replaced by global horizontal radiation (W/M**2),
C with the radiation flag set appropriately after the file has been
C read in.
      SUBROUTINE EPWTOBCLM(LASCI,IUNIT,IER)
#include "esprdbfile.h"
#include "espriou.h"
      
      integer lnblnk  ! function definition

      COMMON/OUTIN/IUOUT,IUIN

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
      dimension ICLMEPW(12,31,24,6)

      CHARACTER CLMLOC*30,WORD*24,uncert*50
      CHARACTER LASCI*72,outs*124,OUTSTR*248
      character dcline*248

C Initial guess on string sizes.
      character city*24,provence*24,country*24,source*24
      character dayname*8,clmcomment*72,dpdescr*24

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
        CALL EGETWR(OUTSTR,K,CLAT,-90.,90.,'W','CLAT',IER)

C Read long and convert to longitude difference. Multiply time zone
C difference by 15 to get degrees and then take difference with
C actual longitude to get esp-r CLONG
        CALL EGETWR(OUTSTR,K,ALONG,-180.,180.,'W','Actual LONG',IER)
        CALL EGETWR(OUTSTR,K,CTZONE,-12.,12.,'W','Time zone diff',IER)
        CTZ = CTZONE*15.0
        CLONG = ALONG - CTZ
        CALL EGETWR(OUTSTR,K,CELEV,-1000.,9999.,'W','elevation m',IER)

C Concatenate city state country to CLMLOC.
        write(outs,'(5a)') city(1:lnblnk(city)),' ',
     &    provence(1:lnblnk(provence)),' ',country(1:lnblnk(country))
        CLMLOC=OUTS(1:30)

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
      elseif(OUTSTR(1:25).eq.'HOLIDAYS/DAYLIGHT SAVINGS')then

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
     &      'hour and cannot yet be converted to esp','W')
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

C Debug...
C        write(6,*)idaystart,imonstart,IDS

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

C Debug.
C        write(6,*)idayfn,imonfn,IDF
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
C (24)  opaque sky cover <integer>, (25) km of visibility <real>,
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
        CALL EGETWI(OUTSTR,K,lyear,1900,2020,'-','line year',IER)
        if(irecloop.eq.1) IYEAR=lyear
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
        CALL EGETWI(OUTSTR,K,iopqskycov,0,100,'W','opaq sky cover',IER)
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
        ICLMEPW(lmonth,lday,lhour,1)=IDIF
        ICLMEPW(lmonth,lday,lhour,2)=ITMP
        ICLMEPW(lmonth,lday,lhour,3)=IDNR
        ICLMEPW(lmonth,lday,lhour,4)=IVEL
        ICLMEPW(lmonth,lday,lhour,5)=IDIR
        ICLMEPW(lmonth,lday,lhour,6)=IHUM
      endif
      goto 42

C When last line of file read then proceed to write out information.
  43  continue
      do 51 im=1,12
        if(im.eq.1.or.im.eq.3.or.im.eq.5.or.im.eq.7)last=31
        if(im.eq.2)last=28
        if(im.eq.4.or.im.eq.6.or.im.eq.9.or.im.eq.11)last=30
        if(im.eq.8.or.im.eq.10.or.im.eq.12)last=31
        do 61 id=1,last
          CALL EDAY(id,im,IDD)

C Transfer this to the binary file.
          IREC=IDD
          WRITE(ICLIM,REC=IREC,IOSTAT=ISTAT,ERR=1002)
     &       ((ICLMEPW(im,id,J,K),K=1,6),J=1,24)
   61   continue
   51 continue

C Insert Climatic Year.
      IREC=366
      ICDNGH=IDNGH
      WRITE(ICLIM,REC=IREC,IOSTAT=ISTAT,ERR=1001)IYEAR,ICDNGH
      IREC=367
      WRITE(ICLIM,REC=IREC)CLMLOC
      IREC=368
      WRITE(ICLIM,REC=IREC)CLAT,CLONG

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
   19 CALL ERPFREE(IUNIT,ISTAT)
      RETURN

 1001 WRITE(outs,775)ISTAT
  775 FORMAT(' Error ',I7,' writing climate database year')
      call edisp(iuout,outs)
      GOTO 19

 1002 WRITE(outs,777)ISTAT
  777 FORMAT(' Error ',I7,' writing climate database')
      call edisp(iuout,outs)
      GOTO 19

      END


