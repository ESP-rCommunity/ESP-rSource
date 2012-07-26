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

C csvstats     - the main controlling routine.
C SURVEYCSV    - scanning occupancy csv file to separate text summary files
C actsumscan   - Reads activity pattern summary file. Work in progress.
C GLOBALCSV    - Reads concatinated activity csv file to generate one text
C                summary file.
C hourlytoperiods - Recover periods from hourly data in the daypattern array.
C parpsf - allows terminal, size and file arguments to be passed.
C partfa - allows terminal, file and action arguments to be passed.

C ****************************************** csvstats ***************************************
C csvstats.F is the main routine which scans an activity csv file and generates
C a summary of the data.
C << todo: be able to scan summary files to check for daytypes over the
C << various profiles. Use -act to say whether it is a csv file or a
C << summary file
C *********************************** csvstats ******************************

      PROGRAM csvstats
#include "espriou.h"

      COMMON/SPAD/MMOD,LIMIT,LIMTTY
      COMMON/OUTIN/IUOUT,IUIN
      integer menuchw,igl,igr,igt,igb,igw,igwh
      COMMON/VIEWPX/menuchw,igl,igr,igt,igb,igw,igwh
      common/appw/iappw,iappx,iappy
      COMMON/FILEP/IFIL
      common/trc/itrc
      COMMON/INPER/INIT
      COMMON/OUTPCH/ICOUT

C Path to problem and command line file (if any).
      common/rpath/path
C      common/uhome/upath
      
C Significant figure reporting limit (NSIGFIG).
      common/SFIG/NSIGFIG

      CHARACTER INTER*144,INF*144
      character path*72,outs248*248
C      character upath*72
      character fs*1
      character act*16 ! c is csv file s is summary file

      character cVnum*16      ! returned from ESPrVersionNum
      character pagestitle*42 ! for banner title via epages call
      LOGICAL unixok

C Initial assumptions.
      call ezero
      ITRC=1
      IUOUT=6
      IUIN=5
      LIMTTY=24
      LIMIT =24
      IFIL=10
      NSIGFIG=3

C Get command line parameters.
      call partfa(MODL,inf,act)
C      call parpsf(MODL,iappw,iappx,iappy,inf)

C Set folder separator (fs) to \ or / as required.
      call isunix(unixok)
      if(unixok)then
        fs = char(47)
      else
        fs = char(92)
      endif
      write(path,'(a1,a1)')'.',fs

c Initialise output device, assume minimal trace, set reduced
C size of window display.
      MMOD=MODL
      iappw=570
      iappx=80
      iappy=100

C Set pixels high to iappw and pixels wide to factor in monitor size.
      iapphi=iappw
      iappwi=int(real(iappw)*(1024.0/780.0))

      menuchw = 24
      LIMTTY=24
      LIMIT =24

C Find the current ESP-r version number and add it to application title.
C      call ESPrVersionNum(cVnum)
      write(pagestitle,'(a)') 'ESP-r csv parser '

      CALL EPAGES(MMOD,IUIN,IUOUT,iappwi,iapphi,iappx,iappy,menuchw,
     &  pagestitle)

      IF(MMOD.EQ.-6)then
        ICOUT=0
      else
        ICOUT=IUOUT
      endif

      call edisp(IUOUT,' ')

C Find the user's home folder then get user's custom settings.
C      call usrhome(upath)

      INIT =0

C Take command line file name to INTER and use as initial survey file.
      if(inf(1:2).eq.'  '.or.inf(1:4).eq.'UNKN')then
        call edisp(iuout,'ERROR: no input file specified')
        call epwait
        call epagend
        STOP
      else
        INTER=inf
        write(OUTS248,'(A,A)')' the input file is: ',INTER
        CALL EDISP248(IUOUT,OUTS248,80)
      endif

C Call the survey subroutines, the main controlling routine with the 
C filename argument.  C for csv files, S for summary files,
C G for csv files to a global list.
      if(act(1:1).eq.'c'.or.act(1:1).eq.'C'.or.act(1:1).eq.'-')then
        CALL SURVEYCSV(INTER)
      elseif(act(1:1).eq.'s'.or.act(1:1).eq.'S')then

C << subroutine in planning >>
        write(6,*) 'tobe done...',INTER
        call actsumscan(INTER)

      elseif(act(1:1).eq.'g'.or.act(1:1).eq.'G')then

C << subroutine to be adapted >>
        CALL GLOBALCSV(INTER)
      endif

      END

C **************** SURVEYCSV **********************************************
C This is the main controlling routine for scanning the occupancy csv file.

      SUBROUTINE SURVEYCSV(INTER)
      
      integer lnblnk  ! function definition

      COMMON/OUTIN/IUOUT,IUIN
      COMMON/FILEP/IFIL

C Common blocks for day patterns collected.
      integer nbdaypattern  ! how many day patterns (thus far) 
      integer daysthispattern ! for each pattern the number of days found
                              ! for each activity
      real daypattern ! (daytype,dayval,hour)
      common/daypat/nbdaypattern,daysthispattern(599,0:700),
     &              daypattern(599,5,24)

      integer nbperpat     ! periods for each pattern
      real perpatstart     ! start time of period (up to 35 periods)
      real perpatend       ! end time of the period
      real perdata         ! data for the period
      common/dayperiod/nbperpat(599),perpatstart(599,35),
     &                 perpatend(599,35),perdata(599,5,35)

      character INTER*144,LFOUT*144,LFLABEL*144
      character LINDEX*12  ! 12 character to hold core file name e.g. 434
 
      integer LOOP ! hour counter
      integer LLOOP  ! counter for lines of the file that have been scanned
      integer LIHR ! hour of day on the current line
      integer ipat ! loop for patterns
      integer ih   ! loop for hours
      integer IOCIND
      real OCFRAC,LTFRAC,EQFRAC,HTSP,CLSP
      real dayvals  ! for OCFRAC,LTFRAC,EQFRAC,HTSP,CLSP
      dimension dayvals(5,24)
      logical close,oneclose
      dimension nonzero(3)  ! instances of non-zero values for OCFRAC,LTFRAC,EQFRAC
      dimension atleastonehour(3) ! days with at least one hour of occupancy, lights, equipment
      logical occuptoday    ! used each day to determine of there were any occupants
      logical lightstoday   ! used each day to determine of there were any lights
      logical equiptoday    ! used each day to determine of there were any equipment
      real nonzero,atleastonehour
      integer icurday  ! the current day of the year
      dimension columnsum(3)  ! summation of OCFRAC,LTFRAC,EQFRAC
      real columnsum
      character outs*144,outsd*144
      character outstr*124
      dimension calender(366)
      integer calender  ! for each Julian day the pattern index
      character loutc*96
      character actname*48
      integer loutlen,itrunc  ! for printing calendar list
      integer lnb,lnd         ! for length of file name
          
C Call the routines which read and process the data sent in the interaction file.

C Assume csv file is on IFIL.
      IUNIT=IFIL
      IUF=IFIL+1
      IUL=IFIL+2
      ACTNAME='UNKNOWN'  ! assume no file with activity labels

      CALL EFOPSEQ(IUNIT,INTER,1,IER)
      if(ier.ne.0)then
        call usrmsg(' Problem opening...',INTER,'W')
        ier = 1
        return
      endif

C Make up an output file with a .text ending matching the input file name.
      lnb=lnblnk(INTER)
      lnd=lnb-3
      if(INTER(lnd:lnb).eq.'.csv')then
        WRITE(LFOUT,'(2a)') INTER(1:lnd),'text'
      else
        WRITE(LFOUT,'(2a)') INTER(1:lnb),'text'
      endif

C Debug.
      write(6,*) LFOUT(1:lnblnk(LFOUT))
      CALL EFOPSEQ(IUF,LFOUT,4,IER)
      if(ier.eq.0)then
        continue
      else
        call usrmsg(' Problem opening...',LFOUT,'W')
        ier = 2
        return
      endif

C Check and see if the file EHWandAuxE.csv exists.
      LFLABEL='DHWandAuxE.csv'
      CALL EFOPSEQ(IUL,LFLABEL,1,IER)
      if(ier.eq.0)then

C If it exists, get the initial part of the file name and convert it
C into an integer iwhich
        lnd=lnb-4  ! get first part of the file name
        WRITE(LINDEX,'(a)') INTER(1:lnd)
        read(LINDEX,*,IOSTAT=IOS)iwhich
        CALL STRIPC(IUL,OUTSTR,0,ND,1,'line 1',IER)
        if(iwhich.gt.0.and.iwhich.lt.600)then
 49       CALL STRIPC(IUL,OUTSTR,0,ND,1,'data line',IER)
          if(IER.eq.0)then
            K=0
            CALL EGETWI(OUTSTR,K,IOC,0,700,'W','occup index',IER)
            if(IOC.eq.iwhich)then
              call egetdq(OUTSTR,K,ACTNAME,'-','room activity type',ier)
              CALL ERPFREE(IUL,ISTAT)  ! close the file
              goto 48
            else
              goto 49
            endif
          else
            CALL ERPFREE(IUL,ISTAT)  ! close the file
          endif
        endif
      else
        call usrmsg(' Problem opening...',LFLABEL,'W')
        iwhich=0
        ACTNAME='UNKNOWN'
      endif

C Initial values.
 48   occuptoday=.false.    ! no occupants
      lightstoday=.false.   ! no lights
      equiptoday=.false.    ! no equipment
      LOOP=0
      LLOOP=0
      nbdaypattern=0
      icurday=0
      close=.false.
      nonzero(1)=0.
      nonzero(2)=0.
      nonzero(3)=0.
      columnsum(1)=0.
      columnsum(2)=0.
      columnsum(3)=0.
      atleastonehour(1)=0.
      atleastonehour(2)=0.
      atleastonehour(3)=0.
      do 40 ipat=1,599
        daysthispattern(ipat,1)=0
        do 41 ih=1,24
          daypattern(ipat,1,ih)=0. ! OCFRAC
          daypattern(ipat,2,ih)=0. ! LTFRAC
          daypattern(ipat,3,ih)=0. ! EQFRAC
          daypattern(ipat,4,ih)=0. ! HTSP
          daypattern(ipat,5,ih)=0. ! CLSP
  41    continue
  40  continue

C Each line of the csv file is the same:
C column type description  range       variable  variable
C  1     int  index of activity
C  2     real occupant fraction 0.-1.
C  3     real lighting fraction 0.-1.
C  4     real equipment fraction 0.-1.
C  5     real heating setpoint degrees
C  6     real cooling setpoint degrees
C  7     integer hour of the day
C  8     string  year-month-day (no read)
 42   CALL STRIPC(IUNIT,OUTSTR,0,ND,1,'line 1',IER)
      if(IER.ne.0)then

C Debug.
        write(6,*) 'stripc end of file ',ier
        CALL ERPFREE(IUNIT,ISTAT)
      endif

C Test if end marker found.
      if(OUTSTR(1:3).eq.'END')then
        goto 99
      endif
       
      K=0
      CALL EGETWI(OUTSTR,K,IOCIND,0,700,'W','occup index',IER)
      CALL EGETWR(OUTSTR,K,OCFRAC,0.,1.,'W','occupant fraction',IER)
      CALL EGETWR(OUTSTR,K,LTFRAC,0.,1.,'W','lighting fraction',IER)
      CALL EGETWR(OUTSTR,K,EQFRAC,0.,1.,'W','equipment fraction',IER)
      CALL EGETWR(OUTSTR,K,HTSP,0.,100.,'W','heating setpoint',IER)
      CALL EGETWR(OUTSTR,K,CLSP,0.,100.,'W','coolinging setpoint',IER)
      CALL EGETWI(OUTSTR,K,LIHR,0,24,'W','hour',IER)
      if(ier.eq.0.and.LIHR.gt.0.and.LIHR.le.24)then
        LOOP=LOOP+1    ! increment
        LLOOP=LLOOP+1  ! increment
        dayvals(1,LIHR)=OCFRAC
        dayvals(2,LIHR)=LTFRAC
        dayvals(3,LIHR)=EQFRAC
        dayvals(4,LIHR)=HTSP
        dayvals(5,LIHR)=CLSP

C Increment instance counter if > zero.
        if(OCFRAC.gt.0.0)then
          nonzero(1)=nonzero(1)+1.
          occuptoday=.true.    ! occupants
        endif
        if(LTFRAC.gt.0.0)then
          nonzero(2)=nonzero(2)+1.
          lightstoday=.true.   ! lights
        endif
        if(EQFRAC.gt.0.0)then
          nonzero(3)=nonzero(3)+1.
          equiptoday=.true.    ! no equipment
        endif

C Add occupant lighting and equipment fractions to columnsum.
        columnsum(1)=columnsum(1)+OCFRAC
        columnsum(2)=columnsum(2)+LTFRAC
        columnsum(3)=columnsum(3)+EQFRAC

      endif

      if(LOOP.lt.24) goto 42  ! loop until we have 24 hours of data

C If there was an instance of occupants/lights/equip during 24 
C hours increment atleastonehour(1)
      if(occuptoday) atleastonehour(1)=atleastonehour(1)+1.0
      if(lightstoday) atleastonehour(2)=atleastonehour(2)+1.0
      if(equiptoday) atleastonehour(3)=atleastonehour(3)+1.0
      occuptoday=.false.    ! reset to no occupants
      lightstoday=.false.   ! reset to no lights
      equiptoday=.false.    ! reset to no equipment

      icurday=icurday+1  ! increment the day
      LOOP=0   ! reset LOOP

C If LOOP is 24 then either save the day values as the first pattern
C or test the day values against the known patterns.
      if(nbdaypattern.eq.0)then
        nbdaypattern=nbdaypattern+1
        daysthispattern(nbdaypattern,1)=
     &    daysthispattern(nbdaypattern,1)+1
        do 43 ih=1,24
          daypattern(nbdaypattern,1,ih)=dayvals(1,ih) ! OCFRAC
          daypattern(nbdaypattern,2,ih)=dayvals(2,ih) ! LTFRAC
          daypattern(nbdaypattern,3,ih)=dayvals(3,ih) ! EQFRAC
          daypattern(nbdaypattern,4,ih)=dayvals(4,ih) ! HTSP
          daypattern(nbdaypattern,5,ih)=dayvals(5,ih) ! CLSP
  43    continue
c        write(outs,'(a,i3)') 'First day pattern on day ',icurday
c        call edisp(iuout,outs)

C Debug.
c        write(6,'(24F5.2)') (dayvals(1,ih),ih=1,24)
C        write(6,'(24F5.2)') (dayvals(2,ih),ih=1,24)
C        write(6,'(24F5.2)') (dayvals(3,ih),ih=1,24)
C        write(6,*) (dayvals(4,ih),ih=1,24)
C        write(6,*) (dayvals(5,ih),ih=1,24)

C Point the pattern index to the Julian day. 
        calender(icurday)=nbdaypattern
        if(LLOOP.ge.8760) goto 99   ! use with 366 day files to stop at 365 days
C        if(LLOOP.ge.8784) goto 99
        goto 42  ! read another line of the file
      elseif(nbdaypattern.ge.1.and.nbdaypattern.lt.599)then

C Test current dayvals against all prior day patterns.
        TOL=0.01
        do 44 ipat=1,nbdaypattern
          oneclose=.true.
          do 45 ih=1,24
            do 46 iv=1,5
              R1=daypattern(ipat,iv,ih)
              R2=dayvals(iv,ih)
              call ECLOSE(R1,R2,TOL,CLOSE)
              if(.NOT.close)then
                oneclose=.false.  ! if there is a difference must have a new pattern
              endif
  46        continue
  45      continue

C If oneclose is still true then it matches a prior pattern so there is
C no need to check further.
          if(oneclose)then

C Debug.
C            write(outs,'(a,i3,a,i3)') 'Julian day pattern ',icurday,
C     &        ' matches pattern ',ipat
C            call edisp(iuout,outs)

C Point the pattern index to the Julian day. 
            calender(icurday)=ipat
            daysthispattern(ipat,1)=daysthispattern(ipat,1)+1
            if(LLOOP.ge.8760) goto 99    ! to force 365 days
C            if(LLOOP.ge.8784) goto 99
            goto 42  ! read another line of the file
          endif
  44    continue

C If we reached this point then none of the existing patterns was matched
C and thus we should create a new pattern
        nbdaypattern=nbdaypattern+1
        do 47 ih=1,24
          daypattern(nbdaypattern,1,ih)=dayvals(1,ih) ! OCFRAC
          daypattern(nbdaypattern,2,ih)=dayvals(2,ih) ! LTFRAC
          daypattern(nbdaypattern,3,ih)=dayvals(3,ih) ! EQFRAC
          daypattern(nbdaypattern,4,ih)=dayvals(4,ih) ! HTSP
          daypattern(nbdaypattern,5,ih)=dayvals(5,ih) ! CLSP
  47    continue

C Debug.
C        write(outs,'(a,i3,a,i3)') 'New Julian day pattern ',
C     &    nbdaypattern,' for Julian day ',icurday
C        call edisp(iuout,outs)
C        write(6,*) (dayvals(1,ih),ih=1,24)

C Point the pattern index to the Julian day. 
        calender(icurday)=nbdaypattern
        daysthispattern(nbdaypattern,1)=
     &    daysthispattern(nbdaypattern,1)+1
        if(LLOOP.ge.8760) goto 99    ! to force 365 days
C        if(LLOOP.ge.8784) goto 99
        goto 42  ! read another line of the file

      elseif(nbdaypattern.ge.599)then
        write(outs,'(a,i3,a)') 'Reached pattern ',
     &      nbdaypattern,' no more space'
        call edisp(iuout,outs)
      endif

C End of file reached or max number of patterns filled. Thus the
C collected data can be written out (comma separated).
 99   CALL EDISP(IUOUT,' ')

C Find the periods for the patterns
      call hourlytoperiods(ier)

      write(outs,'(a,i4,2a)')  '*Activity_pattern ',IOCIND,' named ',
     &  ACTNAME(1:lnblnk(ACTNAME))
      call edisp(IUF,outs)
      write(outs,'(a,i3)')  '*Days_scanned ',icurday
      call SDELIM(outs,outsd,'C',IW)
      call edisp(IUF,outsd)
      write(outs,'(a,i6)')  '*Hours_scanned ',LLOOP
      call SDELIM(outs,outsd,'C',IW)
      call edisp(IUF,outsd)
      write(outs,'(a,f6.0)')'*Non_zero_occupancy_hours',nonzero(1)
      call SDELIM(outs,outsd,'C',IW)
      call edisp(IUF,outsd)
      write(outs,'(a,f6.0)')'*Summation_of_occupancy_frac',columnsum(1)
      call SDELIM(outs,outsd,'C',IW)
      call edisp(IUF,outsd)
      write(outs,'(a,f6.0)')'*Days_with_occupancy',atleastonehour(1)
      call SDELIM(outs,outsd,'C',IW)
      call edisp(IUF,outsd)
      write(outs,'(a,f6.0)')'*Non_zero_light_hours',nonzero(2)
      call SDELIM(outs,outsd,'C',IW)
      call edisp(IUF,outsd)
      write(outs,'(a,f6.0)')'*Summation_of_light_frac',columnsum(2)
      call SDELIM(outs,outsd,'C',IW)
      call edisp(IUF,outsd)
      write(outs,'(a,f6.0)')'*Days_with_lights',atleastonehour(2)
      call SDELIM(outs,outsd,'C',IW)
      call edisp(IUF,outsd)
      write(outs,'(a,f6.0)')'*Non_zero_equipment_hours',nonzero(3)
      call SDELIM(outs,outsd,'C',IW)
      call edisp(IUF,outsd)
      write(outs,'(a,f6.0)')'*Summation_of_equipment_frac',columnsum(3)
      call SDELIM(outs,outsd,'C',IW)
      call edisp(IUF,outsd)
      write(outs,'(a,f6.0)')'*Days_with_equipment',atleastonehour(3)
      call SDELIM(outs,outsd,'C',IW)
      call edisp(IUF,outsd)
      write(outs,'(a,9f6.0)')'*SBEM',nonzero(1),columnsum(1),
     &  atleastonehour(1),nonzero(2),columnsum(2),atleastonehour(2),
     &  nonzero(3),columnsum(3),atleastonehour(3)
      call SDELIM(outs,outsd,'C',IW)
      call edisp(IUF,outsd)
      write(outs,'(a,i3)') '*Nb_of_patterns ',nbdaypattern
      call SDELIM(outs,outsd,'C',IW)
      call edisp(IUF,outsd)

C List out the patterns that have been found (comma separated).
      do 98 ipat=1,nbdaypattern
        write(outs,'(a,i3,a,F5.0,a)') '*Pattern',ipat,' used on ',
     &    daysthispattern(ipat,1),' days'
        call edisp(IUF,outs)
        write(outs,'(a,24F5.2)') '*Ocupfrac ',
     &    (daypattern(ipat,1,ih),ih=1,24)
        call SDELIM(outs,outsd,'C',IW)
        call edisp(IUF,outsd)
        write(outs,'(a,24F5.2)') '*Lightfrac ',
     &     (daypattern(ipat,2,ih),ih=1,24)
        call SDELIM(outs,outsd,'C',IW)
        call edisp(IUF,outsd)
        write(outs,'(a,24F5.2)') '*Equipfrac ',
     &    (daypattern(ipat,3,ih),ih=1,24)
        call SDELIM(outs,outsd,'C',IW)
        call edisp(IUF,outsd)
        write(outs,'(a,24F5.0)') '*HeatSP ',
     &    (daypattern(ipat,4,ih),ih=1,24)
        call SDELIM(outs,outsd,'C',IW)
        call edisp(IUF,outsd)
        write(outs,'(a,24F5.0)') '*CoolSP ',
     &    (daypattern(ipat,5,ih),ih=1,24)
        call SDELIM(outs,outsd,'C',IW)
        call edisp(IUF,outsd)

  98  continue

C List the calendar.
      call edisp(IUF,'*Index_each_julian_day_of_the_year:')
      itrunc=1
      ipos=1
      do while (itrunc.ne.0)
        call ailist(ipos,366,calender,366,'C',loutc,loutlen,itrunc)
C Debug
C        write(6,'(1x,a)',IOSTAT=ios,ERR=2) loutc(1:loutlen)

        call edisp(IUF,loutc)
        ipos=itrunc+1
      end do

C Write out the periods for each of the patterns.

C List out the patterns that have been found (comma separated).
      write(6,*) 'ipat actname ',IOCIND,ACTNAME
      write(outs,'(a,i4,a,a)') '*Schedule',IOCIND,' activity ',
     &  ACTNAME(1:lnblnk(ACTNAME))
      call edisp(IUF,outs)
      write(outs,'(a,i3)') '*Day_types',nbdaypattern
      call SDELIM(outs,outsd,'C',IW)
      call edisp(IUF,outsd)
      do 198 ipat=1,nbdaypattern
        write(outs,'(a,i3,a,i3)') '*Day_pattern ',ipat,' periods ',
     &    nbperpat(ipat)
        call SDELIM(outs,outsd,'C',IW)
        call edisp(IUF,outsd)
        do 199 iper=1,nbperpat(ipat)
          write(outs,'(a,2f7.3,3f8.4,2f7.2)') '*Period ',
     &      perpatstart(ipat,iper),
     &      perpatend(ipat,iper),perdata(ipat,1,iper),
     &      perdata(ipat,2,iper),perdata(ipat,3,iper),
     &      perdata(ipat,4,iper),perdata(ipat,5,iper)
          call SDELIM(outs,outsd,'C',IW)
          call edisp(IUF,outsd)
  199   continue
  198 continue
      CALL EDISP(IUF,'*End')
      CALL ERPFREE(IUF,ISTAT)  ! close the file

      RETURN
      END

C ********* ACTSUMSCAN
C Reads an activity pattern summary file. This is work in progress.

      subroutine actsumscan(INTER)

      COMMON/OUTIN/IUOUT,IUIN
      COMMON/FILEP/IFIL

C Common blocks for day patterns collected.
      integer nbdaypattern  ! how many day patterns (thus far) 
      integer daysthispattern  ! for each pattern the number of days found
                              ! for each activity
      real daypattern ! (daytype,dayval,hour)
      common/daypat/nbdaypattern,daysthispattern(599,0:700),
     &              daypattern(599,5,24)

      integer nbperpat     ! periods for each pattern
      real perpatstart     ! start time of period (up to 35 periods)
      real perpatend       ! end time of the period
      real perdata         ! data for the period
      common/dayperiod/nbperpat(599),perpatstart(599,35),
     &                 perpatend(599,35),perdata(599,5,35)

      character INTER*144,LFOUT*144,LFLABEL*144

C Decode: *Activity_pattern  100 named Open plan office
C IOCIND ACTNAME

C Decode: *Non_zero_occupancy_hours,3234
C nonzero(1)

C Decode: *Summation_of_occupancy_frac,1563
C columnsum(1)

C Decode: *Days_with_occupancy,365.
C atleastonehour(1)

C Decode: *Non_zero_light_hours,3234.
C nonzero(2)

C Decode: *Summation_of_light_frac,3234.
C columnsum(2)

C Decode: *Days_with_lights,365.
C atleastonehour(2)

C Decode: *Non_zero_equipment_hours,8760.
C nonzero(3)

C Decode: *Summation_of_equipment_frac,3510.
C columnsum(3)

C Decode: *Days_with_equipment,365.
C atleastonehour(2)

C Decode: *Nb_of_patterns,2
C nbdaypattern

C Decode: *Pattern  1 used on  261. days
C ipat daysthispattern(ipat)
 
C Decode: *Ocupfrac,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.25,0.50,0.75,0.75,0.50,0.50,0.75,0.75,0.75,0.25,0.00,0.00,0.00,0.00,0.00,0.00
C  (daypattern(ipat,1,ih),ih=1,24)

C Decode: *Lightfrac,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,1.00,1.00,1.00,1.00,1.00,1.00,1.00,1.00,1.00,1.00,0.00,0.00,0.00,0.00,0.00,0.00
C  (daypattern(ipat,2,ih),ih=1,24)

C Decode: *Equipfrac,0.05,0.05,0.05,0.05,0.05,0.05,0.05,0.05,1.00,1.00,1.00,1.00,1.00,1.00,1.00,1.00,1.00,1.00,0.05,0.05,0.05,0.05,0.05,0.05
C  (daypattern(ipat,3,ih),ih=1,24)

C Decode: *HeatSP,12.,12.,12.,12.,12.,12.,12.,22.,22.,22.,22.,22.,22.,22.,22.,22.,22.,22.,12.,12.,12.,12.,12.,12.
C  (daypattern(ipat,4,ih),ih=1,24)

C Decode: *CoolSP,100.,100.,100.,100.,100.,100.,100.,24.,24.,24.,24.,24.,24.,24.,24.,24.,24.,24.,100.,100.,100.,100.,100.,100.
C  (daypattern(ipat,5,ih),ih=1,24)

C Decode: *Schedule100 activity Open plan office
C (? skip this line)

C Decode: *Day_types,2
C nbdaypattern

C Decode: *Day_pattern,1,periods,9
C ipat nbperpat(ipat)

C Decode: *Period,0.000,8.000,0.0000,0.0000,0.0500,12.00,100.00
C   perpatstart(ipat,iper),perpatend(ipat,iper),perdata(ipat,1,iper),
C   perdata(ipat,2,iper),perdata(ipat,3,iper),
C   perdata(ipat,4,iper),perdata(ipat,5,iper)

C Decode: *End

      return
      end

C **************** GLOBALCSV **********************************************
C This scans a concatenated occupancy csv file (e.g. made of all the csv).
C It notices when a new activity happens via the value in the first column
C and writes to a single global text summary file.

      SUBROUTINE GLOBALCSV(INTER)

      COMMON/OUTIN/IUOUT,IUIN
      COMMON/FILEP/IFIL

C Common blocks for day patterns collected.
      integer nbdaypattern  ! how many day patterns (thus far) 
      integer daysthispattern  ! for each pattern the number of days found
                              ! for each activity
      real daypattern ! (daytype,dayval,hour)
      common/daypat/nbdaypattern,daysthispattern(599,0:700),
     &              daypattern(599,5,24)

      integer nbperpat     ! periods for each pattern
      real perpatstart     ! start time of period (up to 35 periods)
      real perpatend       ! end time of the period
      real perdata         ! data for the period
      common/dayperiod/nbperpat(599),perpatstart(599,35),
     &                 perpatend(599,35),perdata(599,5,35)

      integer iactindex    ! index of the activity in the order read in
      integer ndayusedby   ! how may references to this unique day
      integer idayusedby   ! the unique day index as used by each of
                           ! the activity index
      integer nbactivitiesread  ! eg from all the concatenated csv
      integer actnbdaytype ! number of day types associated with an
                           ! activity in ID order
      integer actdaytypelist ! list of day types used by activity
      common/allact/iactindex(0:700),ndayusedby(599),
     &  idayusedby(599,0:700),actnbdaytype(0:700),
     &  actdaytypelist(0:700,599)
      character actname*48 ! name of activity in ID order
      common/charact/actname(0:700)


      character INTER*144,LFOUT*144,LFLABEL*144
 
      integer LOOP ! hour counter
      integer LLOOP  ! counter for lines of the file that have been scanned
      integer LIHR ! hour of day on the current line
      integer ipat ! loop for patterns
      integer ih   ! loop for hours
      integer IOCIND
      integer k,k2 ! position in buffer
      integer loop700(700)   ! temporary arrays
      real OCFRAC,LTFRAC,EQFRAC,HTSP,CLSP
      real dayvals  ! for OCFRAC,LTFRAC,EQFRAC,HTSP,CLSP
      dimension dayvals(5,24)
      logical close,oneclose
      real nonzero
      dimension nonzero(3)  ! instances of non-zero values for OCFRAC,LTFRAC,EQFRAC
      logical foundact      ! true if activity index has been confirmed
      logical alredyknown   ! for testing if prior day type known by activity
      integer icurday  ! the current day of the year
      character outs*144,outsd*144
      character t48*48
      character outstr*124,outstrlbl*124
      integer calender     ! for each Julian day the pattern index
      integer actcalender  ! also remember for each of the activities
      dimension calender(366),actcalender(0:700,366)

      character loutc*96
      integer loutlen,itrunc  ! for printing calendar list
      integer lnb,lnd         ! for length of file name
      integer iwhichhis ! last known activity index
          
C Call the routines which read and process the data sent in the interaction file.

C Assume csv file is on IFIL.
      IUNIT=IFIL
      IUF=IFIL+1
      IUL=IFIL+2

      CALL EFOPSEQ(IUNIT,INTER,1,IER)
      if(ier.ne.0)then
        call usrmsg(' Problem opening...',INTER,'W')
        ier = 1
        return
      endif

C Make up an output file with a .text ending matching the input file name.
      lnb=lnblnk(INTER)
      lnd=lnb-3
      if(INTER(lnd:lnb).eq.'.csv')then
        WRITE(LFOUT,'(2a)') INTER(1:lnd),'text'
      else
        WRITE(LFOUT,'(2a)') INTER(1:lnb),'text'
      endif

C Debug.
      write(6,*) LFOUT(1:lnblnk(LFOUT))
      CALL EFOPSEQ(IUF,LFOUT,4,IER)
      if(ier.eq.0)then
        continue
      else
        call usrmsg(' Problem opening...',LFOUT,'W')
        ier = 2
        return
      endif

C Clear the arrays for all possible activities.
      nbactivitiesread=0
      do 30 k=0,700
        iactindex(k)=-1   ! set -1 for not referenced
        actname(k)='UNKNOWN'
  30  continue

C Check and see if the file EHWandAuxE.csv exists.
      LFLABEL='DHWandAuxE.csv'
      CALL EFOPSEQ(IUL,LFLABEL,1,IER)
      if(ier.eq.0)then

C If it exists, then remember each of the activity type labels.
        CALL STRIPC(IUL,OUTSTRLBL,0,ND,1,'label line 1',IER)
 49     CALL STRIPC(IUL,OUTSTRLBL,0,ND,1,'label data line',IER)
        if(IER.eq.0)then
          K2=0
          CALL EGETWI(OUTSTRLBL,K2,IOC,0,700,'W','occup index',IER)
          if(IOC.le.700)then
            call egetdq(OUTSTRLBL,K2,t48,'-','room activity type',ier)
            write(actname(ioc),'(a)') t48
            iactindex(ioc)=ioc   ! set activity index in ID order
            write(6,*) ioc,'  ',t48
            goto 49
          else
            goto 49
          endif
        else
          CALL ERPFREE(IUL,ISTAT)  ! close the file
        endif

      else
        call usrmsg(' Problem opening...',LFLABEL,'W')
      endif

C Initial values.
      LOOP=0
      LLOOP=0
      nbdaypattern=0
      icurday=0
      close=.false.
      nonzero(1)=0.
      nonzero(2)=0.
      nonzero(3)=0.
      do 40 ipat=1,599
        do 41 ih=1,24
          daypattern(ipat,1,ih)=0. ! OCFRAC
          daypattern(ipat,2,ih)=0. ! LTFRAC
          daypattern(ipat,3,ih)=0. ! EQFRAC
          daypattern(ipat,4,ih)=0. ! HTSP
          daypattern(ipat,5,ih)=0. ! CLSP
  41    continue
        ndayusedby(ipat)=0
        do 31 k2=0,700
          idayusedby(ipat,k2)=-1  ! set -1 for not referenced
          daysthispattern(ipat,k2)=0
  31    continue
  40  continue

C Clear the calendar.
      do 39 k2=1,365
        calender(k2)=0
        do 38 ih=0,700
          actcalender(ih,k2)=0
  38    continue
  39  continue

      foundact = .false.

C Each line of the csv file is the same:
C column type description  range       variable  variable
C  1     int  index of activity
C  2     real occupant fraction 0.-1.
C  3     real lighting fraction 0.-1.
C  4     real equipment fraction 0.-1.
C  5     real heating setpoint degrees
C  6     real cooling setpoint degrees
C  7     integer hour of the day
C  8     string  year-month-day (no read)
 42   CALL STRIPC(IUNIT,OUTSTR,0,ND,1,'csv line 1',IER)
      if(IER.ne.0)then

C Debug.
        write(6,*) 'stripc end of file ',ier
        CALL ERPFREE(IUNIT,ISTAT)
        goto 99
      endif

C Test if end marker found.
      if(OUTSTR(1:3).eq.'END')then
        goto 99
      endif

C Read the activity index (via 1st column).
      K=0
      CALL EGETWI(OUTSTR,K,IOCIND,0,700,'W','occup index',IER)

C If we have not established the occupancy index check for match.
      if(.NOT.foundact)then
        if(IOCIND.ge.0.and.IOCIND.lt.700)then
          do 51 k2=0,700
            if(iactindex(k2).eq.IOCIND)then
              iwhichhis = IOCIND       ! and reset history
              foundact = .true.        ! set logical
              nbactivitiesread = nbactivitiesread + 1
              iactindex(nbactivitiesread)=IOCIND ! in the order found remember index
            endif
  51      continue
        endif
      else

C Has the occupancy pattern changed? If it has then rescan for
C the new index & label and reset icurday.
        if(IOCIND.ne.iwhichhis)then
          do 50 k2=0,700
            if(iactindex(k2).eq.IOCIND)then
              iwhichhis = IOCIND       ! and reset history
              foundact = .true.        ! set logical
              icurday=0  ! reset
              nbactivitiesread = nbactivitiesread + 1
              iactindex(nbactivitiesread)=IOCIND ! in the order found remember index
              write(6,*) 'reset to 1 jan',nbactivitiesread,iocind
            endif
  50      continue

C Indices to reset.
          nonzero(1)=0.
          nonzero(2)=0.
          nonzero(3)=0.

C << anything to write out? >>
        else

        endif
      endif

C Get the rest of the line from the csv file.
      CALL EGETWR(OUTSTR,K,OCFRAC,0.,1.,'W','occupant fraction',IER)
      CALL EGETWR(OUTSTR,K,LTFRAC,0.,1.,'W','lighting fraction',IER)
      CALL EGETWR(OUTSTR,K,EQFRAC,0.,1.,'W','equipment fraction',IER)
      CALL EGETWR(OUTSTR,K,HTSP,0.,100.,'W','heating setpoint',IER)
      CALL EGETWR(OUTSTR,K,CLSP,0.,100.,'W','coolinging setpoint',IER)
      CALL EGETWI(OUTSTR,K,LIHR,0,24,'W','hour',IER)
      if(ier.eq.0.and.LIHR.gt.0.and.LIHR.le.24)then
        LOOP=LOOP+1    ! increment
        LLOOP=LLOOP+1  ! increment
        dayvals(1,LIHR)=OCFRAC
        dayvals(2,LIHR)=LTFRAC
        dayvals(3,LIHR)=EQFRAC
        dayvals(4,LIHR)=HTSP
        dayvals(5,LIHR)=CLSP

C Increment instance counter if > zero.
        if(OCFRAC.gt.0.0)then
          nonzero(1)=nonzero(1)+1.
        endif
        if(LTFRAC.gt.0.0)then
          nonzero(2)=nonzero(2)+1.
        endif
        if(EQFRAC.gt.0.0)then
          nonzero(3)=nonzero(3)+1.
        endif

      endif

      if(LOOP.lt.24) goto 42  ! loop until we have 24 hours of data

      if(icurday.lt.366)then
        icurday=icurday+1  ! increment the day
      else
        icurday=1
      endif
      LOOP=0   ! reset LOOP

C If LOOP is 24 then either save the day values as the first pattern
C or test the day values against the known patterns.
      if(nbdaypattern.eq.0)then
        nbdaypattern=nbdaypattern+1
        daysthispattern(nbdaypattern,IOCIND)=
     &    daysthispattern(nbdaypattern,IOCIND)+1
        do 43 ih=1,24
          daypattern(nbdaypattern,1,ih)=dayvals(1,ih) ! OCFRAC
          daypattern(nbdaypattern,2,ih)=dayvals(2,ih) ! LTFRAC
          daypattern(nbdaypattern,3,ih)=dayvals(3,ih) ! EQFRAC
          daypattern(nbdaypattern,4,ih)=dayvals(4,ih) ! HTSP
          daypattern(nbdaypattern,5,ih)=dayvals(5,ih) ! CLSP
  43    continue
        ndayusedby(nbdaypattern)=ndayusedby(nbdaypattern)+1 !increment for day
        idayusedby(nbdaypattern,ndayusedby(nbdaypattern))=IOCIND
        actnbdaytype(IOCIND)=actnbdaytype(IOCIND)+1  ! increment for activity
        actdaytypelist(IOCIND,actnbdaytype(IOCIND))=nbdaypattern  ! add to list
        write(outs,'(a,i3)') 'First day pattern on day ',icurday
        call edisp(iuout,outs)

C Debug.
        write(6,'(24F5.2)') (dayvals(1,ih),ih=1,24)
C        write(6,'(24F5.2)') (dayvals(2,ih),ih=1,24)
C        write(6,'(24F5.2)') (dayvals(3,ih),ih=1,24)
C        write(6,*) (dayvals(4,ih),ih=1,24)
C        write(6,*) (dayvals(5,ih),ih=1,24)

C Point the pattern index to the Julian day. 
        calender(icurday)=nbdaypattern
        actcalender(IOCIND,icurday)=nbdaypattern
        goto 42  ! read another line of the file

      elseif(nbdaypattern.ge.1.and.nbdaypattern.lt.599)then

C Test current dayvals against all prior day patterns.
        TOL=0.01
        do 44 ipat=1,nbdaypattern
          oneclose=.true.
          do 45 ih=1,24
            do 46 iv=1,5
              R1=daypattern(ipat,iv,ih)
              R2=dayvals(iv,ih)
              call ECLOSE(R1,R2,TOL,CLOSE)
              if(.NOT.close)then
                oneclose=.false.  ! if there is a difference must have a new pattern
              endif
  46        continue
  45      continue

C If oneclose is still true then it matches a prior pattern so there is
C no need to check further.
          if(oneclose)then

C Debug.
            write(outs,'(a,i3,a,i3,a,i3)') 'Julian day pattern ',
     &        icurday,' matches pattern ',ipat,' act ',iocind
            call edisp(iuout,outs)

C Point the pattern index to the Julian day. 
            calender(icurday)=ipat
            actcalender(IOCIND,icurday)=ipat
            daysthispattern(ipat,IOCIND)=daysthispattern(ipat,IOCIND)+1

C Check if this is also know by ndayusedby & idayusedby for day pattern ipat
            alredyknown=.false.
            do 53 k2=1,ndayusedby(ipat)
              if(idayusedby(ipat,k2).eq.IOCIND)then
                alredyknown=.true.
              endif
  53        continue
            if(.NOT.alredyknown)then
              ndayusedby(ipat)=ndayusedby(ipat)+1 !increment
              idayusedby(ipat,ndayusedby(ipat))=IOCIND
              actnbdaytype(IOCIND)=actnbdaytype(IOCIND)+1  ! increment for activity
              actdaytypelist(IOCIND,actnbdaytype(IOCIND))=ipat  ! add to list
            endif
            goto 42  ! read another line of the file
          endif
  44    continue

C If we reached this point then none of the existing patterns was matched
C and thus we should create a new pattern
        nbdaypattern=nbdaypattern+1
        daysthispattern(nbdaypattern,IOCIND)=
     &    daysthispattern(nbdaypattern,IOCIND)+1
        do 47 ih=1,24
          daypattern(nbdaypattern,1,ih)=dayvals(1,ih) ! OCFRAC
          daypattern(nbdaypattern,2,ih)=dayvals(2,ih) ! LTFRAC
          daypattern(nbdaypattern,3,ih)=dayvals(3,ih) ! EQFRAC
          daypattern(nbdaypattern,4,ih)=dayvals(4,ih) ! HTSP
          daypattern(nbdaypattern,5,ih)=dayvals(5,ih) ! CLSP
  47    continue
        ndayusedby(nbdaypattern)=ndayusedby(nbdaypattern)+1 !increment
        actnbdaytype(IOCIND)=actnbdaytype(IOCIND)+1  ! increment for activity
        idayusedby(nbdaypattern,ndayusedby(nbdaypattern))=IOCIND
        actdaytypelist(IOCIND,actnbdaytype(IOCIND))=nbdaypattern  ! add to list

C Debug.
        write(outs,'(a,i3,a,i3,a,i3)') 'New Julian day pattern ',
     &    nbdaypattern,' for Julian day ',icurday,' act ',
     &    iocind
        call edisp(iuout,outs)
        write(6,*) (dayvals(1,ih),ih=1,24)

C Point the pattern index to the Julian day. 
        calender(icurday)=nbdaypattern
        actcalender(IOCIND,icurday)=nbdaypattern
        goto 42  ! read another line of the file

      elseif(nbdaypattern.ge.599)then
        write(outs,'(a,i3,a)') 'Reached pattern ',
     &      nbdaypattern,' no more space'
        call edisp(iuout,outs)
      endif

C End of file reached or max number of patterns filled. Thus the
C collected data can be written out (comma separated).
 99   CALL EDISP(IUOUT,' ')

      write(outs,'(a,i8)')  '*Lines_scanned ',LLOOP
      call SDELIM(outs,outsd,'C',IW)
      call edisp(IUF,outsd)

C << there will also be several full patterns so this needs to
C << be a loop.

C Find the periods for each of the unique day patterns
      call hourlytoperiods(ier)

      write(outs,'(a,i3)') '*Nb_of_patterns',nbdaypattern
      call SDELIM(outs,outsd,'C',IW)
      call edisp(IUF,outsd)

C List out the unique day patterns that have been found (comma separated).
C Also list the indeix of activities that use this day pattern (via ailist).
      do 98 ipat=1,nbdaypattern
        write(outs,'(a,i3,a,i3,a)') '*Day_pattern ',ipat,
     &   ' referenced_by ',ndayusedby(ipat),' activities'
        call SDELIM(outs,outsd,'C',IW)
        call edisp(IUF,outsd)

C Cast idayusedby to single dimension array for use in ailist.
        do 76 k2=1,ndayusedby(ipat)
          loop700(k2)=idayusedby(ipat,k2)
  76    continue
        itrunc=1
        ipos=1
        do while (itrunc.ne.0)
          call ailist(ipos,ndayusedby(ipat),loop700,700,'C',loutc,
     &      loutlen,itrunc)
          call edisp(IUF,loutc)
          ipos=itrunc+1
        end do
        write(outs,'(a,24F5.2)') '*Ocupfrac ',
     &    (daypattern(ipat,1,ih),ih=1,24)
        call SDELIM(outs,outsd,'C',IW)
        call edisp(IUF,outsd)
        write(outs,'(a,24F5.2)') '*Lightfrac ',
     &     (daypattern(ipat,2,ih),ih=1,24)
        call SDELIM(outs,outsd,'C',IW)
        call edisp(IUF,outsd)
        write(outs,'(a,24F5.2)') '*Equipfrac ',
     &    (daypattern(ipat,3,ih),ih=1,24)
        call SDELIM(outs,outsd,'C',IW)
        call edisp(IUF,outsd)
        write(outs,'(a,24F5.0)') '*HeatSP ',
     &    (daypattern(ipat,4,ih),ih=1,24)
        call SDELIM(outs,outsd,'C',IW)
        call edisp(IUF,outsd)
        write(outs,'(a,24F5.0)') '*CoolSP ',
     &    (daypattern(ipat,5,ih),ih=1,24)
        call SDELIM(outs,outsd,'C',IW)
        call edisp(IUF,outsd)
        do 199 iper=1,nbperpat(ipat)
          write(outs,'(a,2f7.3,3f8.4,2f7.2)') '*Period ',
     &      perpatstart(ipat,iper),
     &      perpatend(ipat,iper),perdata(ipat,1,iper),
     &      perdata(ipat,2,iper),perdata(ipat,3,iper),
     &      perdata(ipat,4,iper),perdata(ipat,5,iper)
          call SDELIM(outs,outsd,'C',IW)
          call edisp(IUF,outsd)
  199   continue
  98  continue

C For each of the activity patterns that were compiled from 
C the csv file write out an entry. 
      do 201 iact=1,nbactivitiesread
        index=iactindex(iact)  ! shortcut to the activity index
        lan=lnblnk(actname(index))
        write(outs,'(a,i4,2a)')  '*Activity_pattern ',index,
     &    ' named ',ACTNAME(index)(1:lan)
        call edisp(IUF,outs)

C Document the list of patterns used by this activity
        ndt=actnbdaytype(index)

C List each of the day types and the periods of this activity.
C In the code we can just point back to the unique day pattern
C for the raw data we need.
        do 78 k2=1,ndt
          ipat=actdaytypelist(index,k2)  ! shortcut to day pattern
          write(outs,'(a,i3,a,i3)') '*Day_pattern ',ipat,' periods ',
     &      nbperpat(ipat)
          call SDELIM(outs,outsd,'C',IW)
          call edisp(IUF,outsd)
          do 198 iper=1,nbperpat(ipat)
            write(outs,'(a,2f7.3,3f8.4,2f7.2)') '*Period ',
     &        perpatstart(ipat,iper),
     &        perpatend(ipat,iper),perdata(ipat,1,iper),
     &        perdata(ipat,2,iper),perdata(ipat,3,iper),
     &        perdata(ipat,4,iper),perdata(ipat,5,iper)
            call SDELIM(outs,outsd,'C',IW)
            call edisp(IUF,outsd)
  198     continue

  78    continue

C List the calendar.
        call edisp(IUF,'*Index_each_julian_day_of_the_year:')
        do 77 k2=1,365
          calender(k2)=actcalender(iactindex(iact),k2)
  77    continue
        itrunc=1
        ipos=1
        do while (itrunc.ne.0)
          call ailist(ipos,366,calender,366,'C',loutc,loutlen,itrunc)
C Debug
C        write(6,'(1x,a)',IOSTAT=ios,ERR=2) loutc(1:loutlen)

          call edisp(IUF,loutc)
          ipos=itrunc+1
        end do

 201  continue

      CALL EDISP(IUF,'*End')
      CALL ERPFREE(IUF,ISTAT)  ! close the file

      RETURN
      END


C Recover periods from hourly data in the daypattern array.
      subroutine hourlytoperiods(ier)

C Common blocks for day patterns collected.
      integer nbdaypattern  ! how many day patterns (thus far) 
      integer daysthispattern  ! for each pattern the number of days found
                              ! for each activity
      real daypattern ! (daytype,dayval,hour)
      common/daypat/nbdaypattern,daysthispattern(599,0:700),
     &              daypattern(599,5,24)

      integer nbperpat     ! periods for each pattern
      real perpatstart     ! start time of period (up to 35 periods)
      real perpatend       ! end time of the period
      real perdata         ! data for the period
      common/dayperiod/nbperpat(599),perpatstart(599,35),
     &                 perpatend(599,35),perdata(599,5,35)

      logical occupchange   ! a change in occupancy
      logical lightchange   ! a change in lighting
      logical equipchange   ! a change in equipment
      logical heatchange    ! a change in heating setpoint
      logical coolchange    ! a change in cooling setpoint
      real occupwas,occupis ! past and current value of occupant
      real lightwas,lightis ! ditto lighting
      real equipwas,equipis ! ditto equipment
      real heatwas,heatis   ! ditto heating setpoint
      real coolwas,coolis   ! ditto cooling setpoint
      real tol              ! tollerance for change

      integer ipat  ! loop for patterns
      integer ihour ! loop for hours
      integer iper  ! counter for periods in the pattern day

C Initial values.
      occupchange=.false.; lightchange=.false.; equipchange=.false.
      heatchange=.false.; coolchange=.false.
      occupwas=0.0; occupis=0.0; lightwas=0.0; lightis=0.0
      equipwas=0.0; equipis=0.0; heatwas=0.0; heatis=0.0
      coolwas=0.0; coolis=0.0
      tol=0.01

C For each unique pattern extracted from the csv data scan for unique
C periods in each of the days. Expressing data in periods is more compact
C than holding data for each hour.
      do 42 ipat=1,nbdaypattern

C For the first hour of the day pattern
        ihour=1
        iper=1
        occupwas=daypattern(ipat,1,iper) 
        lightwas=daypattern(ipat,2,iper)
        equipwas=daypattern(ipat,3,iper)
        heatwas=daypattern(ipat,4,iper)
        coolwas=daypattern(ipat,5,iper)
        nbperpat(ipat)=1
        perpatstart(ipat,iper)=0.0    ! always zero for start
        perpatend(ipat,iper)=24.0     ! assume end of day
        perdata(ipat,1,iper)=occupwas ! first period has data from hour one
        perdata(ipat,2,iper)=lightwas
        perdata(ipat,3,iper)=equipwas
        perdata(ipat,4,iper)=heatwas
        perdata(ipat,5,iper)=coolwas
        do 41 ihour=2,24

C For each hour get the value for each topic and see if it differs from
C the 'was' value. If there has been any change in any of the values then
C we have a new period and the need for closing off the previous period.
          occupis=daypattern(ipat,1,ihour) ! OCFRAC
          lightis=daypattern(ipat,2,ihour) ! LTFRAC
          equipis=daypattern(ipat,3,ihour) ! EQFRAC
          heatis=daypattern(ipat,4,ihour)  ! HTSP
          coolis=daypattern(ipat,5,ihour)  ! CLSP
          call eclose(occupwas,occupis,tol,occupchange)
          call eclose(lightwas,lightis,tol,lightchange)
          call eclose(equipwas,equipis,tol,equipchange)
          call eclose(heatwas,heatis,tol,heatchange)
          call eclose(coolwas,coolis,tol,coolchange)
          if(occupchange.and.lightchange.and.equipchange.and.
     &       heatchange.and.coolchange)then
            continue  ! every thing is the same
          else

C Something changed so close off the pereiod, save the 'was'
C data and then set the timings and update the *was values.
            perpatend(ipat,iper)= real (ihour) ! close off period
            perdata(ipat,1,iper)=occupwas ! first period has data from hour one
            perdata(ipat,2,iper)=lightwas
            perdata(ipat,3,iper)=equipwas
            perdata(ipat,4,iper)=heatwas
            perdata(ipat,5,iper)=coolwas
C Debug.
            write(6,*) 'st end',perpatstart(ipat,iper),
     &        perpatend(ipat,iper),occupwas,lightwas,equipwas,
     &        heatwas,coolwas

            occupwas=occupis  ! shift current to past for each value
            lightwas=lightis
            equipwas=equipis
            heatwas=heatis
            coolwas=coolis
C Debug.
            write(6,*) iper,ihour
            iper=iper+1   ! increment the period
            nbperpat(ipat)=iper
            perpatstart(ipat,iper)= real (ihour)  ! mark start of next
            write(6,*) 'st next',iper,ihour,perpatstart(ipat,iper)
          endif

  41    continue
        perpatend(ipat,iper)=24.0  ! end of day for last period
        if(nbperpat(ipat).gt.1)then
          perdata(ipat,1,iper)=occupwas ! remember data for last period of the day
          perdata(ipat,2,iper)=lightwas
          perdata(ipat,3,iper)=equipwas
          perdata(ipat,4,iper)=heatwas
          perdata(ipat,5,iper)=coolwas
        endif
        write(6,*) 'last start end',iper,perpatstart(ipat,iper),
     &    perpatend(ipat,iper),occupwas,lightwas,equipwas
        write(6,*) 'finished 24 hours'

  42  continue

      return
      end

C Parse command lines.

C ********* parpsf (modified from code in startup.F)
C parpsf allowa terminal, size and config file arguments
C to be passed to the program from the invocation line.  

      subroutine parpsf(termtype,iappw,iappx,iappy,inf)
      
      integer lnblnk  ! function definition

      COMMON/OUTIN/IUOUT,IUIN

      integer termtype,iappw,iappx,iappy
      character*48 prog
      integer iargc,m
      character argument*72,mode*8,inf*144,appn*24,outs*248
      integer iXavail
      logical unixok

C Determine if an X graphics library (GTK or X11) was linked
C into binary, and set default terminal type as necessary
      IOS=0  ! initial value
      if ( iXavail() == 1 ) then 
        termtype = 8
        mode = 'graphic'
        m = iargc()
        i = 0
        call getarg(i,prog)
      else
        termtype = -1
        mode = 'text'
        m = iargc()
        i = 0
        call getarg(i,prog)
      endif

      iappw = 0
      iappx = 0
      iappy = 0
      inf  = 'UNKNOWN'
      argument = ' '

C Get arguments from command line: if no paramters just
C return, otherwise process parameters.
      if(m.eq.0)then
        return
      elseif(m.ge.1)then
  41    i= i+1
        if(i.gt.m)goto 42
        call getarg(i,argument)

        if(argument(1:5).eq.'-help')then

C Only give feedback if non-DOS.
          call isunix(unixok)
          if(unixok)then
            call edisp(iuout,'command line is csv file name')
            stop
          endif
        elseif(argument(1:5).eq.'-mode')then
          i=i+1
          call getarg(i,argument)
          if(argument(1:4).eq.'text')then
            termtype = -1
            mode = 'text'
          elseif(argument(1:4).eq.'page')then
            termtype = -2
            mode = 'page'
          elseif(argument(1:5).eq.'graph')then
            termtype = 8
            mode = 'graphic'
          elseif(argument(1:6).eq.'script')then
            termtype = -6
            mode = 'script'
          endif
        elseif(argument(1:2).eq.'-s')then
          i=i+1
          call getarg(i,argument)
          read(argument,*,IOSTAT=IOS,ERR=2)iappw
          i=i+1
          call getarg(i,argument)
          read(argument,*,IOSTAT=IOS,ERR=2)iappx
          i=i+1
          call getarg(i,argument)
          read(argument,*,IOSTAT=IOS,ERR=2)iappy
        elseif(argument(1:5).eq.'-file')then
          i=i+1
          call getarg(i,inf)
        endif
        goto 41

  42    continue

C Only give feedback if non-DOS.
        call isunix(unixok)
        if(unixok)then
c          write(outs,'(5a)') 'Starting csvstats ',
c     &      ' in mode ',mode(1:lnblnk(mode)),' with file ',
c     &      inf(1:lnblnk(inf))
c          call edisp248(iuout,outs,90)
        endif
        return
      endif

C Errors.
   2  call isunix(unixok)
      if(unixok)then
        if(IOS.eq.2)then
          write(6,'(a)')'parpsf: permission error getting parameters.'
        else
          write(6,'(a)')'parpsf: error extracting parameters.'
        endif
      endif
      return
      end

C *********
C << Issue: each csv file implies a possibly unique set of calendar day
C << types - and a model must have one calendar rather than lots of
C << calendars.

C << Task is to scan all of the profile summaries and determine
C << the number of unique day types for the whole collection of
C << profiles and then apply this back to the summaries. It would be
C << great if minimal editing or file manipulation was required.

C << Could there be a directory scan from within stats that located
C << all of the csv files, scanned each one in turn and produced
C << ONE summary file. That way we could do lots of manipulations
C << in memory and write out an aggregate.

C << the internal array sizes would need to accommodate the full set
C << of csv files - that would be huge...
C << logic might be...
C open folder, make up a list of all of the csv file names
C open a csv file
C set initial counters
C loop through sets of 24 hours
C find unique pattern as per normal, remember:
C   -> how many patterns for this csv
C   -> how many patterns globally 
C   -> pointer to julian day as well as activity pattern number
C   -> is it the 1st or 2nd or 3rd for this csv
C at end of the csv file clear the locals
C open the next csv file 
C loop through sets of 24 hours - compare these against the global
C   array of day patterns for matches and remember:
C   -> how many patterns for this csv
C   -> how many patterns globally 
C   -> pointer to julian day as well as activity pattern number
C  (a unique pattern many be used by many activities - do I need
C   an array for this?)

C  when there are no more in the list 
C write out each unique pattern found globally
C figure out the patterns (global) associated with each activity and
C write out the pattern index for each of 365 days for the activity

C the consolidated set of day types - we are looking for when the
C index (1st 2nd or 3rd for that csv) changes. 
C  1 1 1 1 1 1 1 1 1 2 1 1 1 1 1 1 1  act 1 (total of 2 day types)
C  1 1 1 2 1 1 1 1 1 1 1 1 3 1 1 1 1  act 2 (total of 3 day types)
C  1 1 1 1 1 1 1 2 2 2 1 1 3 3 3 1 1  act 3 (total of 3 day types)

C one 4th day for act 1 we need to have an alt day type which is
C the same as pattern 1 (could be nb of patterns for that activity plus
C this index so the revised layout for act 1 & 2 might be...
C  1 1 1 3 1 1 1 3 3 4 1 1 3 1 1 1 1  act 1 (1st revision)
C  1 1 1 2 1 1 1 1 1 1 1 1 3 1 1 1 1  act 2 (total of 3 day types)

C If we try and take into account act 3 then
C  1 1 1 3 1 1 1 3 3 4 1 1 3 5 5 1 1  act 1 (2nd revision 5=3=1  4=2)
C  1 1 1 2 1 1 1 4 4 4 1 1 6 4 4 1 1  act 2 (1st revision 4=1 6=3)
C  1 1 1 1 1 1 1 2 2 2 1 1 3 3 3 1 1  act 3 (total of 3 day types)

C imagine doing an activity 4, the pattern becomes ever more complicated
C as we work back and revise the earlier activities.

C BUT, if we go with the global indicies on each julidan day what
C does that do for us? Would that yield something that might work?

C ********* partfa (equivalent to partf with an action parameter)
C partfa allows terminal, file and action arguments
C  to be passed to the program from the invocation line.  
C  Logic allows for the -file token to be optional.

C This is a truncated version of partfa from startup.F which has
C fewer dependencies than the standard version.
      subroutine partfa(termtype,inf,act)
      
      integer lnblnk  ! function definition

      COMMON/OUTIN/IUOUT,IUIN
      common/APPNAME/cAppName
      character*12 cAppName
      integer termtype
      character*48 prog
      integer iargc,m
      character argument*72,mode*8,inf*144,appn*24,outs*248,act*16
      logical unixok


C Determine if an X graphics library (GTK or X11) was linked
C into binary, and set default terminal type as necessary

      if ( iXavail() == 1 ) then 
        termtype = 8
        mode = 'graphic'
        m = iargc()
        i = 0
        call getarg(i,prog)
      else
        termtype = -1
        mode = 'text'
        m = iargc()
        i = 0
        call getarg(i,prog)
      endif
      
      inf  = 'UNKNOWN'
      act = 'NONE'
      argument = ' '

C Get arguments from command line: if no paramters just
C return, otherwise process parameters.
      if(m.eq.0)then
        return
      elseif(m.ge.1)then
  41    i= i+1
        if(i.gt.m)goto 42
        call getarg(i,argument)

        if(argument(1:5).eq.'-help')then

C Only give feedback if non-DOS.
          call isunix(unixok)
          if(unixok)then
            call edisp(iuout,'command line is csv or summary file')
            stop
          endif
        elseif(argument(1:5).eq.'-mode')then
          i=i+1
          call getarg(i,argument)
          if(argument(1:4).eq.'text')then
            termtype = -1
            mode = 'text'
          elseif(argument(1:4).eq.'page')then
            termtype = -2
            mode = 'page'
          elseif(argument(1:5).eq.'graph')then
            termtype = 8
            mode = 'graphic'
          elseif(argument(1:6).eq.'script')then
            termtype = -6
            mode = 'script'
          endif
        elseif(argument(1:5).eq.'-file')then
          i=i+1
          call getarg(i,inf)
        elseif(argument(1:5).eq.'-act ')then
          i=i+1
          call getarg(i,act)
        else

C Assume argument is an input file without the -file token.
          write(inf,'(a)') argument(1:lnblnk(argument))
        endif
        goto 41

  42    continue

C Only give feedback if non-DOS.
        call isunix(unixok)
        if(unixok)then
          write(outs,'(8a)') 'Starting ',appn(1:lnblnk(appn)),
     &    ' in mode ',mode(1:lnblnk(mode)),' with file ',
     &    inf(1:lnblnk(inf)),' with action ',act(1:lnblnk(act))
C         call edisp248(iuout,outs,90)
        endif
        return
      endif
      end

C Dummy subroutines.
      SUBROUTINE ADJVIEW(IER)
      ier=0
      return
      end

      SUBROUTINE EMKVIEW(IUO,CFGOK,IER)
      logical cfgok
      ier=0
      return
      end

      SUBROUTINE EPKMLC(ISEL,PROMPT1,PROMPT2,IER)
      CHARACTER*(*) PROMPT1,PROMPT2
      ier=0
      return
      end

      SUBROUTINE INLNST(ITYP)
      return
      end

      SUBROUTINE PLELEV(direc)
      CHARACTER direc*1
      return
      end

      subroutine chgsun(isunhour)
      return
      end
