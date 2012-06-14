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
#include "building.h"
#include "model.h"
#include "esprdbfile.h"
#include "espriou.h"
#include "sbem.h"

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
      
C Significant figure reporting limit (NSIGFIG).
      common/SFIG/NSIGFIG

      CHARACTER INTER*144,INF*144
      character path*72,outs248*248
      character*144 lworking ! for processing the path of LSBEM
      character fs*1
      character act*16 ! c is csv file s is summary file

      character cVnum*16      ! returned from ESPrVersionNum
      character pagestitle*42 ! for banner title via epages call
      LOGICAL unixok
      character actbld*50   ! building type for activity in ID order
      common/charact/actbld(0:700)

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
      call usrhome(upath)
      if(unixok)then
        write(esprc,'(3a)') upath(1:lnblnk(upath)),fs,'.esprc'
      else
        write(esprc,'(3a)') upath(1:lnblnk(upath)),fs,'esprc'
      endif
      if(ITRC.eq.0)then
        call scesprc(esprc,IFIL+5,0,IIER)
      else
        call scesprc(esprc,IFIL+5,1,IIER)
      endif

C Debug.
      write(6,*) 'upath',upath
      write(6,*) 'esprc',esprc

C Scan the defaults file for default system configuration file.
C Make temporary use of file unit IFIL+5.  Note: escdef must come
C after scan of .esprc file.
      call escdef(IFIL+5,IER)


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

C Read default SBEM database. If lsbem has not yet been assigned
C then assign from default name (which has a
C path to the distribution database folder).
C Process DSBEM with findwhichdbpath. Use the value
C of ipathsbem to control file opening logic. This is similar
C to the logic used for the optics database which is also ASCII.
      ipathsbem=2
      if(isbem.eq.0)then
        write(lworking,'(a)') DSBEM(1:lnblnk(DSBEM))
        call findwhichdbpath('sbm',lworking,ier)
      endif

C Clear the SBEM common blocks
      do 7 j=1,MACL
        occupant_dens(j)=0.; fresh_air(j)=0.; equip_gain(j)=0.
        dhw_litres(j)=0.; display_lighting(j)=0.
        Hmainsetpoint(j)=0.; Cmainsetpoint(j)=0.
        fNotionalLighting(j)=0.; fTypicalLighting(j)=0.
        roomactname(j)='UNKNOWN'
        actbld(j)='UNKNOWN'
  7   continue
      call SBEMPR(IER)  ! read the sbem database

C List one of the items.
      iactindex=71
      write(6,*) 'fTypicalLighting ', fTypicalLighting(iactindex)
      write(6,*) 'roomactnam ',roomactname(iactindex)
      write(6,*) 'bld_order_index ',bld_order_index(iactindex)
      write(6,*) 'building type ',BTYPNAME(bld_order_index(iactindex))
      write(6,*) 'occupant_dens ',occupant_dens(iactindex)
      write(6,*) 'metabolic_rate ',metabolic_rate(iactindex)
      write(6,*) 'Cmainsetpoint ',Cmainsetpoint(iactindex)
      write(6,*) 'Hmainsetpoint ',Hmainsetpoint(iactindex)
      write(6,*) 'fresh_air ',fresh_air(iactindex)
      write(6,*) 'lighting_lux ',lighting_lux(iactindex)
      write(6,*) 'equip_gain ',equip_gain(iactindex)
      write(6,*) 'dhw_litres ',dhw_litres(iactindex)
      write(6,*) 'roomactdoc ',roomactdoc(iactindex)
      write(6,*) 'latent_ocup_percent ',
     &  latent_ocup_percent(iactindex)
      write(6,*) 'latent_equip_percent ',
     &  latent_equip_percent(iactindex)
      write(6,*) 'display_lighting ',display_lighting(iactindex)
      write(6,*) ' '

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
      character actname*72
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
C        write(6,'(24F5.2)') (dayvals(1,ih),ih=1,24)
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
        write(outs,'(a,i3,a,i3)') 'New Julian day pattern ',
     &    nbdaypattern,' for Julian day ',icurday
        call edisp(iuout,outs)
C        write(6,*) (dayvals(1,ih),ih=1,24)

C Point the pattern index to the Julian day. 
        calender(icurday)=nbdaypattern
        daysthispattern(nbdaypattern,1)=
     &    daysthispattern(nbdaypattern,1)+1
        if(LLOOP.ge.8760) goto 99    ! to force 365 days
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
#include "building.h"
#include "model.h"
C #include "esprdbfile.h"
C #include "espriou.h"
#include "sbem.h"

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
      character actbld*50   ! building type for activity in ID order
      common/charact/actbld(0:700)


      character INTER*144,LFOUT*144
 
      integer LOOP ! hour counter
      integer LLOOP  ! counter for lines of the file that have been scanned
      integer LIHR ! hour of day on the current line
      integer ipat ! loop for patterns
      integer ih   ! loop for hours
      integer IOCIND
      integer k,k2 ! position in buffer
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
      character dq*1
      character outstr*124
      integer calender     ! for each Julian day the pattern index
      integer actcalender  ! also remember for each of the activities
      dimension calender(366),actcalender(0:700,366)
      integer localender   ! for an activity the local day type
      integer locdaytype   ! for each activity a pointer from the global
                           ! day type to local day type for use in localender
      dimension localender(366),locdaytype(599)
      integer nbunknownactivities ! how many unknown to SBEM.db1 activities
      integer nbcsvunknownact,nbcsvunknownact2   ! how many unknown within the csv file

      character loutc*96
      integer loutlen,itrunc  ! for printing calendar list
      integer lnb,lnd         ! for length of file name
      integer iwhichhis ! last known activity index
          
C Call the routines which read and process the data sent in the interaction file.

C Assume csv file is on IFIL.
      IUNIT=IFIL
      IUF=IFIL+1
      dq=char(34)   ! double quote

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

C Set the arrays for all possible activities based on
C the common blocks from SBEM.db1 scan.
      nbactivitiesread=0  ! reset the count
      nbunknownactivities=0
      nbcsvunknownact=0
      nbcsvunknownact2=0
      do 30 k=1,700
        if(roomactname(k)(1:7).eq.'UNKNOWN')then
          iactindex(k)=-1   ! set -1 for not referenced
C          write(6,*) 'activity index ',k,' is UNKNOWN'
          nbunknownactivities=nbunknownactivities+1
        else
          iactindex(k)=k   ! set as referenced
          ldub=lnblnk(BTYPNAME(bld_order_index(k)))
          write(actbld(k),'(a)') BTYPNAME(bld_order_index(k))(1:ldub)
        endif
  30  continue

C Debug.
      write(6,*) 'nb of unknown sbem activities ',nbunknownactivities
C      write(6,*) iactindex

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
        do 31 k2=1,700
          idayusedby(ipat,k2)=-1  ! set -1 for not referenced
          daysthispattern(ipat,k2)=0
  31    continue
  40  continue

C Clear the calendar.
      do 39 k2=1,365
        calender(k2)=0
        do 38 ih=1,700
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
C If the SBEM.db1 did not mention an activity the value of iactindex
C will be a -1.  For now do not bother with csv file entries that
C are not included in SBEM.db1
      if(.NOT.foundact)then
        if(IOCIND.gt.0.and.IOCIND.lt.700)then
          do 51 k2=1,700
            if(iactindex(k2).eq.-1)then
               continue
            elseif(iactindex(k2).eq.IOCIND)then
              iwhichhis = IOCIND       ! and reset history
              foundact = .true.        ! set logical
              nbactivitiesread = nbactivitiesread + 1
              write(6,*) '51 confirm new activity ',iocind,
     &          nbactivitiesread
            else
              continue
            endif
  51      continue
        endif
      else

C Has the occupancy pattern changed? If it has then rescan for
C the new index & label and reset icurday.
        if(IOCIND.ne.iwhichhis)then
          do 50 k2=1,700
            if(iactindex(k2).eq.-1)then
              continue
            elseif(iactindex(k2).eq.IOCIND)then
              iwhichhis = IOCIND       ! and reset history
              foundact = .true.        ! set logical
              icurday=0  ! reset
              nbactivitiesread = nbactivitiesread + 1
              write(6,*) 'reset to 1 jan',nbactivitiesread,iocind
            else
              continue
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
C        write(6,'(24F5.2)') (dayvals(1,ih),ih=1,24)
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
C            write(outs,'(a,i3,a,i3,a,i3)') 'Julian day pattern ',
C     &        icurday,' matches pattern ',ipat,' act ',iocind
C            call edisp(iuout,outs)

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
C        write(6,*) (dayvals(1,ih),ih=1,24)

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

      write(outs,'(a,i4)') '*Nb_of_patterns',nbdaypattern
      call SDELIM(outs,outsd,'C',IW)
      call edisp(IUF,outsd)

C List out the unique day patterns that have been found (comma separated).
C Also list the indeix of activities that use this day pattern (via ailist).
      do 98 ipat=1,nbdaypattern
        write(outs,'(a,i3,a,i3,a)') '*Day_pattern ',ipat,
     &   ' referenced_by ',ndayusedby(ipat),' activities'
        call SDELIM(outs,outsd,'C',IW)
        call edisp(IUF,outsd)

C For each of the activities that reference this day type list
C their index and name and building type. Do not bother with
C room types which are not found in SBEM.db1.
        do 76 k2=1,ndayusedby(ipat)
          idu=idayusedby(ipat,k2)   ! the activity index
          if(iactindex(idu).eq.-1)then
            continue  ! there was an unused activity index slot found earlier
            write(6,*) '76 skipping a -1 k2 idu iactindex ',k2,idu,
     &        iactindex(idu)
          else
            ldu=lnblnk(roomactname(idu))  ! its name length
            if(ldu.gt.60) ldu=60          ! but only first 60 char
            if(roomactname(idu)(1:7).eq.'UNKNOWN')then
              continue
            else
              ldub=lnblnk(actbld(idu))  ! its building type length
              write(outs,'(a,i3,8a)') '*Refbyactivity,',idu,',',dq,
     &          roomactname(idu)(1:ldu),dq,',',dq,actbld(idu)(1:ldub),dq
              call edisp(IUF,outs)
            endif
          endif
  76    continue

C The list of 24 numbers could be printed out if needed.
C        write(outs,'(a,24F5.2)') '*Ocupfrac ',
C     &    (daypattern(ipat,1,ih),ih=1,24)
C        call SDELIM(outs,outsd,'C',IW)
C        call edisp(IUF,outsd)
C        write(outs,'(a,24F5.2)') '*Lightfrac ',
C     &     (daypattern(ipat,2,ih),ih=1,24)
C        call SDELIM(outs,outsd,'C',IW)
C        call edisp(IUF,outsd)
C        write(outs,'(a,24F5.2)') '*Equipfrac ',
C     &    (daypattern(ipat,3,ih),ih=1,24)
C        call SDELIM(outs,outsd,'C',IW)
C        call edisp(IUF,outsd)
C        write(outs,'(a,24F5.0)') '*HeatSP ',
C     &    (daypattern(ipat,4,ih),ih=1,24)
C        call SDELIM(outs,outsd,'C',IW)
C        call edisp(IUF,outsd)
C        write(outs,'(a,24F5.0)') '*CoolSP ',
C     &    (daypattern(ipat,5,ih),ih=1,24)
C        call SDELIM(outs,outsd,'C',IW)
C        call edisp(IUF,outsd)
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
C the csv file write out an entry.  Include only information
C on room acitivies scanned in from the SBEM.db1 database.

C Within the activites recovered from the csv find out how
C many were unknown.  Try it twice.
      do 202 iact=1,nbactivitiesread
        index=iactindex(iact)  ! shortcut to the activity index
        if(index.eq.-1)then
          nbcsvunknownact=nbcsvunknownact+1
        endif
  202 continue
      write(6,*) 'nb read nb unknown ',nbactivitiesread,
     &  nbunknownactivities,nbcsvunknownact

      do 203 iact=1,nbactivitiesread+nbcsvunknownact
        index=iactindex(iact)  ! shortcut to the activity index
        if(index.eq.-1)then
          nbcsvunknownact2=nbcsvunknownact2+1
        endif
  203 continue
      write(6,*) '2nd pass nb read nb unknown ',nbactivitiesread,
     &  nbunknownactivities,nbcsvunknownact2

      do 201 iact=1,nbactivitiesread+nbcsvunknownact2
        index=iactindex(iact)  ! shortcut to the activity index
        if(index.eq.-1)then
          continue  ! it was unknown within SBEM.db1
        else
          lan=lnblnk(roomactname(index))
          lanb=lnblnk(actbld(index))
          write(outs,'(a,i3,8a)')  '*Activity_pattern,',index,
     &      ',',dq,roomactname(index)(1:lan),dq,' in ',
     &      dq,actbld(index)(1:lanb),dq
          call edisp(IUF,outs)

C Write occupant related data to outs and then outsd and append comment.
          write(outs,'(a,f6.3,i4,i4)') '*occupant',
     &      occupant_dens(index),metabolic_rate(index),
     &      latent_ocup_percent(index)
          call SDELIM(outs,outsd,'C',IW)
          write(outs,'(2a)') outsd(1:lnblnk(outsd)),
     &      ' # density/m2 metablolic rate W latent %'
          call edisp(IUF,outs)

          write(outs,'(a,f7.3,i3)') '*equipment',equip_gain(index),
     &      latent_equip_percent(index)
          call SDELIM(outs,outsd,'C',IW)
          write(outs,'(2a)') outsd(1:lnblnk(outsd)),' # gain W latent %'
          call edisp(IUF,outs)

          write(outs,'(a,i6,2f7.3)') '*lighting',lighting_lux(index),
     &      fTypicalLighting(index),display_lighting(index)
          call SDELIM(outs,outsd,'C',IW)
          write(outs,'(2a)') outsd(1:lnblnk(outsd)),
     &      ' # lux typical W/m2 display W/m2'
          call edisp(IUF,outs)

          write(outs,'(a,f8.3)') '*dhw_litres',dhw_litres(index)
          call SDELIM(outs,outsd,'C',IW)
          write(outs,'(2a)') outsd(1:lnblnk(outsd)),' # litres/year'
          call edisp(IUF,outs)

          write(outs,'(a,f8.3)') '*fresh_air',fresh_air(index)
          call SDELIM(outs,outsd,'C',IW)
          write(outs,'(2a)') outsd(1:lnblnk(outsd)),
     &      ' # litres/person/sec'
          call edisp(IUF,outs)

          write(outs,'(a,2f7.3)') '*setponts',Hmainsetpoint(index),
     &      Cmainsetpoint(index)
          call SDELIM(outs,outsd,'C',IW)
          write(outs,'(2a)') outsd(1:lnblnk(outsd)),
     &    ' # occupied heating C cooling C'
          call edisp(IUF,outs)

C Document the list of patterns used by this activity
          ndt=actnbdaytype(index)

C List each of the day types and the periods of this activity.
C In the code we can just point back to the unique day pattern
C for the raw data we need.  The k2 value is the local count
C of day types.
          do 78 k2=1,ndt
            ipat=actdaytypelist(index,k2)  ! shortcut to day pattern
            write(outs,'(a,2i3,a,i3)') '*Day_pattern ',ipat,k2,
     &        ' periods ',nbperpat(ipat)
            call SDELIM(outs,outsd,'C',IW)
            call edisp(IUF,outsd)

C Create pointer from ipat to k2 for use with localender.
            locdaytype(ipat)=k2
            do 198 iper=1,nbperpat(ipat)
              write(outs,'(a,2f7.3,3f8.4,2f7.2)') '*Period ',
     &          perpatstart(ipat,iper),
     &          perpatend(ipat,iper),perdata(ipat,1,iper),
     &          perdata(ipat,2,iper),perdata(ipat,3,iper),
     &          perdata(ipat,4,iper),perdata(ipat,5,iper)
              call SDELIM(outs,outsd,'C',IW)
              call edisp(IUF,outsd)
  198       continue

  78      continue

C List the global day type calendar.
C          call edisp(IUF,'*Index_each_julian_day_of_the_year:')
          do 77 k2=1,365
            calender(k2)=actcalender(iactindex(iact),k2)
            localender(k2)=locdaytype(calender(k2))  ! convert to local
  77      continue

C Uncomment next lines to get report of global day types.
C        itrunc=1
C        ipos=1
C        do while (itrunc.ne.0)
C          call ailist(ipos,365,calender,366,'C',loutc,loutlen,itrunc)
C Debug
C        write(6,'(1x,a)',IOSTAT=ios,ERR=2) loutc(1:loutlen)

C          call edisp(IUF,loutc)
C          ipos=itrunc+1
C        end do

          call edisp(IUF,'*Local_index_each_julian_day_of_the_year:')
          itrunc=1
          ipos=1
          do while (itrunc.ne.0)
            call ailist(ipos,365,localender,366,'C',loutc,loutlen,
     &        itrunc)
C Debug
C        write(6,'(1x,a)',IOSTAT=ios,ERR=2) loutc(1:loutlen)

            call edisp(IUF,loutc)
            ipos=itrunc+1
          end do
        endif
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
C            write(6,*) 'st end',perpatstart(ipat,iper),
C     &        perpatend(ipat,iper),occupwas,lightwas,equipwas,
C     &        heatwas,coolwas

            occupwas=occupis  ! shift current to past for each value
            lightwas=lightis
            equipwas=equipis
            heatwas=heatis
            coolwas=coolis
C Debug.
C            write(6,*) iper,ihour
            iper=iper+1   ! increment the period
            nbperpat(ipat)=iper
            perpatstart(ipat,iper)= real (ihour)  ! mark start of next
C            write(6,*) 'st next',iper,ihour,perpatstart(ipat,iper)
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
C        write(6,*) 'last start end',iper,perpatstart(ipat,iper),
C     &    perpatend(ipat,iper),occupwas,lightwas,equipwas
C        write(6,*) 'finished 24 hours'

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


C ******************** scesprc (copied from esrucom/startup.F)
C scesprc scans the ESP-r dot file which should be located in the
C users home folder or in the esp-r distribution.  If not found
C then some defaults are set.
C Note: take this opportunity to instanciate common block stddbpath
C while we have information on where esp-r was installed.

      SUBROUTINE scesprc(LFDEF,IFDEF,ITRC,IER)
C Use espriou.h for climatelist file name and defdbfl.
#include "building.h"
#include "model.h"
#include "espriou.h"
#include "esprdbfile.h"

      integer lnblnk  ! function definition

      COMMON/OUTIN/IUOUT,IUIN

C External text editor.
      common/texted/tedlbl,teditor

C External document previewer.
      common/textpv/tprevlbl,tpreview

C External figure generator.
      common/figtool/figlbl,figexe

C External graphing tool.
      common/graphtool/grflbl,grfexe

C External folder browser.
      common/folderbr/foldlbl,foldexe

C A set of possible image formats which can be accepted within
C a model configuration file. These definitions are held in the
C so-called esprc file. To add additional image types and/or 3rd
C party display applications edit the ESP-r Install script which
C creates the esprc file.
C   imgtyp is the number of different image formats supported
C   fmttag (4 char) is a tag for each image formt (e.g. GIF XBM)
C   fmtexe (20 char) is the application name used to display
C          images of type fmttag.
      common/showimg/imgtyp,fmttag(5),fmtexe(5)

C A set of possible cad packages: labels and executables.
      common/cad3rd/cadlbl,cadexe,cadfmt

C Report generator: label and executable.
      common/rep3rd/replbl,repexe

C Save journal. journoo = ON or OFF
      common/journopt/journio,iuj,journcmd,jfile

C Graphic capture, window dump & text print: label, command.
      common/gprint/gprlbl,gprcmd
      common/gxwd/gxwdlbl,gxwdcmd
      common/tprint/tprlbl,tprcmd

C Where validation tests are kept.
      character vldlbl*20,vldfl*96
      common/vldlst/vldlbl,vldfl

      logical localdot,unixok,dll

      character*(*) LFDEF
      CHARACTER OUTSTR*124,WORD*20
      character outs*124,outs248*248
      character fmttag*4,fmtexe*20,cadlbl*20,cadexe*20,cadfmt*4
      character replbl*20,repexe*20
      character gprlbl*20,gprcmd*48
      character gxwdlbl*20,gxwdcmd*60
      character tprlbl*20,tprcmd*48
      character tedlbl*20,teditor*20
      character tprevlbl*20,tpreview*48
      character figlbl*20,figexe*48
      character grflbl*20,grfexe*48
      character foldlbl*20,foldexe*48
      character journcmd*20,jfile*72

C Altdot is an alternative location for the esprc file in
C the installation folder.
      character altdot*144
#include "espinstalldir.h"

C Check if running in dll mode, if so clear most of the
C initial values because they will not be used.
      call isadll(dll)
      if(dll)then
        teditor=' '
        tedlbl='no word processor'
        tpreview=' '
        tprevlbl='no document preview'
        figlbl='diagram editor'
        figexe=' '
        grflbl='no graphing tool'
        grfexe=' '
        foldlbl='no folder browser'
        foldexe=' '
        cadlbl='no CAD package'
        cadexe=' '
        cadfmt='DXF'
        replbl='No report generator'
        repexe='UNKNOWN'
        gprcmd='UNKNOWN'
        journcmd=' '
        jfile=' '
        imgtyp=1
        fmttag(1)='GIF'
        fmtexe(1)=' '
        gxwdlbl='no graphic dump'
      else

C Assume initial values for interactive use. Default to putting
C temporary files in the users home folder.
        teditor='vi'
        tedlbl='word processor'
        tpreview='GROFF -p '
        tprevlbl='document previewer'
        figlbl='diagram editor'
        figexe='xfig'
        grflbl='graphing tool'
        grfexe='xvgr'
        foldlbl='folder browser'
        foldexe='xfs '
        cadlbl='CAD package'
        cadexe=' '
        cadfmt='DXF'
        replbl='No report generator'
        repexe='UNKNOWN'
        gprcmd='UNKNOWN'
        journcmd=' '
        jfile=' '

C Assume two basic image types and that the application window dump
C will use xwud.
        imgtyp=2
        fmttag(1)='GIF'
        fmtexe(1)='xv'
        fmttag(2)='XWD'
        fmtexe(2)='xwud -in'
        gxwdlbl='Appl window dump'
      endif

C Set defaults and exemplars depending on whether Unix or NT.
      call isunix(unixok)
      ln=lnblnk(instpath)
      if(unixok)then
        write(gxwdcmd,'(3a)')'xwud -out ',upath(1:lnblnk(upath)),
     &  '/gxwd.xwd'
        tprlbl='Text dump'
        write(tprcmd,'(a,a)') upath(1:lnblnk(upath)),'/tx_dump'
        exemlbl='Standard exemplars'
        write(exemfl,'(2a)') instpath(1:ln),'/training/exemplars'
        dfdblbl='Standard defaults'
        write(defdbfl,'(2a)') instpath(1:ln),'/default'
        vldlbl='Validation standards'
        write(vldfl,'(2a)') instpath(1:ln),'/validation/stds_list'
        cdblbl='Standard climates'
        write(cdblfil,'(2a)') instpath(1:ln),'/climate/climatelist'
        write(altdot,'(2a)') instpath(1:ln),'/esprc'
        write(standarddbpath,'(2a)') instpath(1:ln),'/databases'
        write(standardclmpath,'(2a)') instpath(1:ln),'/climate'
      else
        write(gxwdcmd,'(a)')'xwud -out c:/temp/gxwd.xwd'
        tprlbl='Text dump'
        write(tprcmd,'(a)') 'c:/temp/tx_dump'
        exemlbl='Standard exemplars'
        write(exemfl,'(2a)') instpath(1:ln),'/training/exemplars'
        dfdblbl='Standard defaults'
        write(defdbfl,'(2a)') instpath(1:ln),'/default'
        vldlbl='Validation tests'
        write(vldfl,'(2a)') instpath(1:ln),'/validation/stds_list'
        cdblbl='Standard climates'
        write(cdblfil,'(2a)') instpath(1:ln),'/climate/climatelist'
        write(altdot,'(2a)') instpath(1:ln),'/esprc'
        write(standarddbpath,'(2a)') instpath(1:ln),'/databases'
        write(standardclmpath,'(2a)') instpath(1:ln),'/climate'
      endif

C Debug.
C      write(6,*) 'standard databases are at ',
C     &  standarddbpath(1:lnblnk(standarddbpath))

C If running in dll mode skip reading .esprc file.
      if(dll)then
        return
      endif

C Open the esprc file, return if not found.
      IER=0
      CALL ERPFREE(IFDEF,ISTAT)

C Debug.
C      write(6,*) 'lfdef is ',lfdef

      call FPOPEN(IFDEF,ISTAT,1,0,LFDEF)
      IF(ISTAT.LT.0)THEN
        CALL ERPFREE(IFDEF,ISTAT)
        call FPOPEN(IFDEF,ISTAT,1,0,altdot)
        IF(ISTAT.LT.0)THEN
          call edisp(iuout,
     &    '  No preferences (esprc or .esprc) file found so setting')
          call edisp(iuout,
     &    '  default exemplars, CAD, report & print links.')
          IER=1
          CALL ERPFREE(IFDEF,ISTAT)
          RETURN
        else
          localdot=.false.
          esprc=altdot  ! assign the install folder file as the esprc file.
          if(itrc.ge.0) call edisp(iuout,
     &      '  Preferences (esprc) file - standard version.')
          write(currentfile,'(a)') esprc(1:lnblnk(esprc))
        endif
      else
        localdot=.true.
        if(itrc.ge.0) call edisp(iuout,
     &    '  Preferences (.esprc) file - users version.')
        write(currentfile,'(a)') lfdef(1:lnblnk(lfdef))
      ENDIF

C Read '*ESPRC' tag.
      CALL STRIPC(IFDEF,OUTSTR,0,ND,1,'ESPRC tag',IER)
      if(OUTSTR(1:6).ne.'*ESPRC')then
        if(localdot)then
          write(outs248,'(2a)')LFDEF(:lnblnk(LFDEF)),
     &      ' not an .esprc file!'
        else
          write(outs248,'(2a)')altdot(:lnblnk(altdot)),
     &      ' not a .esprc file!'
        endif
        call edisp(iuout,OUTSTR)
        call edisp248(iuout,outs248,100)
        IER=1
        CALL ERPFREE(IFDEF,ISTAT)
        RETURN
      endif

C Take image types from the file.
      imgtyp=0


C If '*ESPRC' found then read db until '*end' found.
  44  continue
      CALL STRIPC(IFDEF,OUTSTR,0,ND,1,'dot sequences',IER)
      K=0
      CALL EGETW(OUTSTR,K,WORD,'W','dot sequences',IFLAG)
      if(WORD(1:5).eq.'*gprn')then

C Capture rectangular area of screen via capexgf is in wwlib.c
        CALL EGETP(OUTSTR,K,gprlbl,'W','rect area lbl',IER)
        CALL EGETP(OUTSTR,K,gprcmd,'W','rect area exe',IER)
        write(outs,'(a,a,2x,a)')' Rectangular area grab:',gprlbl,gprcmd
        if(itrc.gt.1)call edisp(iuout,outs)
        call capexgf(gprcmd)
      elseif(WORD(1:5).eq.'*gxwd')then

C Screen capture commands - dump whole screem via capexall in wwlib.c
        CALL EGETP(OUTSTR,K,gxwdlbl,'W','screen dump lbl',IER)
        CALL EGETP(OUTSTR,K,gxwdcmd,'W','screen dump exe',IER)
        write(outs,'(a,a,2x,a)')' Screen dump: ',gxwdlbl,gxwdcmd
        if(itrc.gt.1)call edisp(iuout,outs)
        call capexall(gxwdcmd)
      elseif(WORD(1:5).eq.'*tprn')then

C Caputre text buffer to a file, capextf is in wwlib.c
        CALL EGETP(OUTSTR,K,tprlbl,'W','text print lbl',IER)
        CALL EGETP(OUTSTR,K,tprcmd,'W','text print file',IER)
        write(outs,'(a,a,2x,a)')' Local text print: ',tprlbl,tprcmd
        if(itrc.gt.1)call edisp(iuout,outs)
        call capextf(tprcmd)
      elseif(WORD(1:4).eq.'*cad')then
        CALL EGETP(OUTSTR,K,cadlbl,'W','cad lbl',IER)
        CALL EGETP(OUTSTR,K,cadexe,'W','cad exe',IER)
        CALL EGETW(OUTSTR,K,cadfmt,'W','cad fnt',IER)
        write(outs,'(a,a,2x,a,2x,a)')' Local CAD: ',cadlbl,cadexe,cadfmt
        if(itrc.gt.1)call edisp(iuout,outs)
      elseif(WORD(1:13).eq.'*tutorial_app')then

C Tutorial application depreciated.
        continue
      elseif(WORD(1:8).eq.'*journal')then

C Turn journaling on/off.
        CALL EGETW(OUTSTR,K,journcmd,'W','on/off',IFLAG)
        if(journcmd(1:2).eq.'ON'.or.journcmd(1:2).eq.'on')then
          journio=1
        else
          journio=0
        endif
      elseif(WORD(1:14).eq.'*image_display')then
        imgtyp=imgtyp+1
        CALL EGETP(OUTSTR,K,fmttag(imgtyp),'W','image fmt',IER)
        CALL EGETP(OUTSTR,K,fmtexe(imgtyp),'W','img exe',IER)
        write(outs,'(4a)')' Images : ',fmttag(imgtyp),' via ',
     &    fmtexe(imgtyp)
        if(itrc.gt.1)call edisp(iuout,outs)
      elseif(WORD(1:7).eq.'*editor')then
        CALL EGETP(OUTSTR,K,tedlbl,'W','manual editor lbl',IER)
        CALL EGETP(OUTSTR,K,teditor,'W','editor exe',IER)
        write(outs,'(4a)')' Editor: ',tedlbl,' via ',teditor
        if(itrc.gt.1)call edisp(iuout,outs)
      elseif(WORD(1:11).eq.'*docpreview')then
        CALL EGETP(OUTSTR,K,tprevlbl,'W','document previewer lbl',IER)
        CALL EGETP(OUTSTR,K,tpreview,'W','doc previewer exe',IER)
        write(outs,'(4a)')' Preview: ',tprevlbl,' via ',tpreview
        if(itrc.gt.1)call edisp(iuout,outs)
      elseif(WORD(1:7).eq.'*figure')then
        CALL EGETP(OUTSTR,K,figlbl,'W','figure editor lbl',IER)
        CALL EGETP(OUTSTR,K,figexe,'W','figure editor exe',IER)
        write(outs,'(4a)')' Preview: ',figlbl,' via ',figexe
        if(itrc.gt.1)call edisp(iuout,outs)
      elseif(WORD(1:9).eq.'*graphing')then
        CALL EGETP(OUTSTR,K,grflbl,'W','graphing tool lbl',IER)
        CALL EGETP(OUTSTR,K,grfexe,'W','graphing tool exe',IER)
        write(outs,'(4a)')' Graphing: ',grflbl,' via ',grfexe
        if(itrc.gt.1)call edisp(iuout,outs)
      elseif(WORD(1:9).eq.'*folderbr')then
        CALL EGETP(OUTSTR,K,foldlbl,'W','folder br tool lbl',IER)
        CALL EGETRM(OUTSTR,K,foldexe,'W','folder tool exe',IER)
        write(outs,'(4a)')' Folder browsing: ',foldlbl,' via ',foldexe
        if(itrc.gt.1)call edisp(iuout,outs)
      elseif(WORD(1:10).eq.'*exemplars')then
        CALL EGETP(OUTSTR,K,exemlbl,'W','exemplars list lbl',IER)
        CALL EGETRM(OUTSTR,K,exemfl,'W','exemplars file',IER)
        write(outs248,'(2a)')'  Exemplars (model list) - ',exemfl
        if(itrc.gt.1)call edisp248(iuout,outs248,100)
      elseif(WORD(1:12).eq.'*db_defaults')then
        CALL EGETP(OUTSTR,K,dfdblbl,'W','def db list lbl',IER)
        CALL EGETRM(OUTSTR,K,defdbfl,'W','default db file',IER)
        write(outs248,'(2a)')'  Default file - ',defdbfl
        if(itrc.gt.1)call edisp248(iuout,outs248,100)
      elseif(WORD(1:16).eq.'*validation_stds')then
        CALL EGETP(OUTSTR,K,vldlbl,'W','validation list lbl',IER)
        CALL EGETRM(OUTSTR,K,vldfl,'W','validation stds file',IER)
        write(outs248,'(4a)')' Validation: ',vldlbl,' in ',vldfl
        if(itrc.gt.1)call edisp248(iuout,outs248,100)
      elseif(WORD(1:12).eq.'*db_climates')then
        CALL EGETP(OUTSTR,K,cdblbl,'W','clm db list lbl',IER)
        CALL EGETRM(OUTSTR,K,cdblfil,'W','clm db list file',IER)
        write(outs248,'(2a)')'  Weather (list) - ',cdblfil
        if(itrc.gt.1)call edisp248(iuout,outs248,100)
      elseif(WORD(1:11).eq.'*report_gen')then
        CALL EGETP(OUTSTR,K,replbl,'W','report gen lbl',IER)
        CALL EGETRM(OUTSTR,K,repexe,'W','report gen exe',IER)
        write(outs,'(4a)')' Reports: ',replbl,' via ',repexe
        if(itrc.gt.1)call edisp(iuout,outs)
      elseif(WORD(1:5).eq.'*year')then
      elseif(WORD(1:4).eq.'*end')then
        goto 45
      else
        call usrmsg(' Unknown dot command in... ',outstr,'W')
        goto 44
      endif

C Loop back for next command.
      goto 44

C Close file before exiting.
   45 CONTINUE
      if(localdot)then
        write(outs248,'(3a)') 'Scanning ',
     &         LFDEF(1:lnblnk(LFDEF)),' definitions.'
        if(itrc.gt.1)call edisp248(iuout,outs248,100)
      else
        write(outs248,'(3a)') 'Standard settings in: ',
     &         altdot(1:lnblnk(altdot)),' successfully scanned...'
        if(itrc.gt.1)call edisp248(iuout,outs248,100)
      endif
      CALL ERPFREE(IFDEF,ISTAT)
      RETURN
      END

C ******************** ESCDEF (copied from esrucom/startup.F)
C ESCDEF scans the ESP-r defaults definition file and returns
C the default file and database names via common block.
C It uses espinstalldir.h which was created by the Install
C script with the current esp-r install directory held
C in a string instpath.

C ESCDEF also scans default constructions to be offered
C for code complience purposes or for use defined uses.
      SUBROUTINE ESCDEF(IFDEF,IER)
#include "esprdbfile.h"
C Use espriou.h for climatelist file name and defdbfl.
#include "espriou.h"

      integer lnblnk  ! function definition

      COMMON/OUTIN/IUOUT,IUIN

C Defaults.
      character*96 DFCFG,DFCTL,DEFRLB,DAPROB,DAFRES,DPNF
      COMMON/DEFLT2/DFCFG,DFCTL,DEFRLB,DAFRES,DAPROB,DPNF
      COMMON/DEFLT3/DFCFD,DECMPDBFL,DICONDBFL
      common/deflt4/dinstpath

C Construction defaults:
C cdtype (char*8) identifier for each set of constructions
C cdtypelabel (char*32) menu select for each set of construcitons
C cdextwall (char*32) external wall
C cdextglz (char*32) external glazing/window
C cdextfrm (char*32) external frame
C cdextdisp (char*32) external display glazing
C cdextdoor (char*32) external door
C cdextvdoor (char*32) external vehicle door
C cdextflrf (char*32) external flat roof
C cdextslrf (char*32) external slope roof
C cdextskyl (char*32) external skylight
C cdextfnd (char*32) external foundation
C cdintfloor (char*32) internal floor
C cdintceil (char*32) internal ceiling
C cdintptn (char*32) internal partition
C cdintdoor (char*32) internal door
C cdintglz (char*32) internal glazing/window
      character cdtype*8,cdtypelabel*32,cdextwall*32,cdextglz*32
      character cdextfrm*32,cdextdisp*32,cdextdoor*32,cdextvdoor*32
      character cdextflrf*32,cdextslrf*32,cdextskyl*32,cdextfnd*32
      character cdintfloor*32,cdintceil*32,cdintptn*32,cdintdoor*32
      character cdintglz*32
      integer icdtypes
      common/deflt5t/icdtypes
      common/deflt5/cdtype(5),cdtypelabel(5),cdextwall(5),cdextglz(5),
     &  cdextfrm(5),cdextdisp(5),cdextdoor(5),cdextvdoor(5),
     &  cdextflrf(5),cdextslrf(5),cdextskyl(5),cdextfnd(5),
     &  cdintfloor(5),cdintceil(5),cdintptn(5),cdintdoor(5),cdintglz(5)

      CHARACTER OUTSTR*124,WORD*20,outs248*248

C << consider increasing size of these strings >>
      character*72 DFCFD
      character*72 DECMPDBFL,DICONDBFL
      character dinstpath*60,di*60
      logical unixok,dll
#include "espinstalldir.h"

C Assume default db names (in case of failure to find defaults
C file or failure reading it).

C << make a pass sorting out these default file names >>
      call isunix(unixok)
      if(unixok)then
        write(dinstpath,'(a)') instpath(1:lnblnk(instpath))
        write(di,'(a)') instpath(1:lnblnk(instpath))
        ln=lnblnk(di)
        write(DFCFG,'(2a)') di(1:min(ln,34)),
     &    '/training/basic/cfg/bld_basic.cfg'
        write(DFCTL,'(2a)') di(1:min(ln,34)),
     &    '/training/basic/ctl/bld_basic.ctl'
        write(DAPROB,'(2a)') di(1:min(ln,38)),
     &    'training/basic/nets/bld_basic_af1.afn'
        write(DPNF,'(2a)') di(1:min(ln,31)),
     &    '/training/plant/vent_simple/cfg/vent.cfg'
        write(DFCFD,'(2a)') di(1:min(ln,45)),
     &    '/training/cfd/template.dfd'
        write(DEFRLB,'(2a)') di(1:min(ln,51)),'/databases/test.res'
        write(DAFRES,'(2a)') di(1:min(ln,51)),'/databases/test.mfr'
        write(DCLIM,'(2a)') di(1:min(ln,51)),'/climate/clm67'
        write(DAPRES,'(2a)') di(1:min(ln,51)),'/databases/pressc.db1'
        write(DFCON,'(2a)') di(1:min(ln,51)),
     &    '/databases/material.db3.a'
        write(DFMUL,'(2a)') di(1:min(ln,51)),'/databases/multicon.db3'
        write(DOPTDB,'(2a)') di(1:min(ln,51)),'/databases/optics.db2'
        write(DPRFDB,'(2a)') di(1:min(ln,51)),
     &    '/databases/profiles.db1.a'
        write(DPCDB,'(2a)') di(1:min(ln,51)),'/databases/plantc.db1'
        write(DECMPDBFL,'(2a)') di(1:min(ln,51)),'/databases/elcomp.db1'
        write(DMCMPDBFL,'(2a)') di(1:min(ln,51)),'/databases/mscomp.db1'
        write(DICONDBFL,'(2a)') di(1:min(ln,51)),'/databases/icons.db1'
        write(dmdbnam,'(2a)') di(1:min(ln,51)),'/databases/mould.db1'
        write(DSBEM,'(2a)') di(1:min(ln,51)),'/databases/SBEM.db1'
      else
C        dinstpath = 'c:/esru/esp-r'
        write(dinstpath,'(a)') instpath(1:lnblnk(instpath))
        write(di,'(a)') instpath(1:lnblnk(instpath))
        ln=lnblnk(di)
        write(DFCFG,'(2a)') di(1:min(ln,34)),
     &    '/training/basic/cfg/bld_basic.cfg'
        write(DFCTL,'(2a)') di(1:min(ln,34)),
     &    '/training/basic/ctl/bld_basic.ctl'
        write(DAPROB,'(2a)') di(1:min(ln,38)),
     &    'training/basic/nets/bld_basic_af1.afn'
        write(DPNF,'(2a)') di(1:min(ln,31)),
     &    '/training/plant/vent_simple/cfg/vent.cfg'
        write(DFCFD,'(2a)') di(1:min(ln,45)),
     &    '/training/cfd/template.dfd'
        write(DEFRLB,'(2a)') di(1:min(ln,51)),'/databases/test.res'
        write(DAFRES,'(2a)') di(1:min(ln,51)),'/databases/test.mfr'
        write(DCLIM,'(2a)') di(1:min(ln,51)),'/climate/clm67'
        write(DAPRES,'(2a)') di(1:min(ln,51)),'/databases/pressc.db1'
        write(DFCON,'(2a)') di(1:min(ln,51)),
     &    '/databases/material.db3.a'
        write(DFMUL,'(2a)') di(1:min(ln,51)),'/databases/multicon.db3'
        write(DOPTDB,'(2a)') di(1:min(ln,51)),'/databases/optics.db2'
        write(DPRFDB,'(2a)') di(1:min(ln,51)),
     &    '/databases/profiles.db1.a'
        write(DPCDB,'(2a)') di(1:min(ln,51)),'/databases/plantc.db1'
        write(DECMPDBFL,'(2a)') di(1:min(ln,51)),'/databases/elcomp.db1'
        write(DMCMPDBFL,'(2a)') di(1:min(ln,51)),'/databases/mscomp.db1'
        write(DICONDBFL,'(2a)') di(1:min(ln,51)),'/databases/icons.db1'
        write(dmdbnam,'(2a)') di(1:min(ln,51)),'/databases/mould.db1'
        write(DSBEM,'(2a)') di(1:min(ln,51)),'/databases/SBEM.db1'
      endif

C << ?? Should there be default values for constructions? >>
      icdtypes = 0

C Check if running in dll mode, if so skip reading defaults file.
      call isadll(dll)
      if(dll)then
        return
      endif

C Open the defaults file, return if not found.
      IER=0
      CALL EFOPSEQ(IFDEF,defdbfl,1,IER)
      IF(IER.LT.0)THEN
        write(outs248,'(3a)') 'The file ',defdbfl(1:lnblnk(defdbfl)),
     &   ' was not found (assuming standard databases).'
        call edisp248(IUOUT,outs248,100)
        IER=1
        RETURN
      ENDIF

C Read '*ESP-r Defaults' tag.
      CALL STRIPC(IFDEF,OUTSTR,0,ND,1,'default tag',IER)
      if(OUTSTR(1:15).ne.'*ESP-r Defaults')then
        write(outs248,'(3a)') 'The file ',defdbfl(1:lnblnk(defdbfl)),
     &   ' is not a defaults file.'
        call edisp248(IUOUT,outs248,100)
        IER=1
        RETURN
      endif

C If '*ESP-r Defaults' found then read db until '*end' found.
  44  continue
      CALL STRIPC(IFDEF,OUTSTR,0,ND,1,'def names',IER)
      K=0
      CALL EGETW(OUTSTR,K,WORD,'W','def names',IFLAG)
      if(WORD(1:5).eq.'*ipth')then
        CALL EGETRM(OUTSTR,K,dinstpath,'W','instal folder',IER)
      elseif(WORD(1:4).eq.'*cfg')then
        CALL EGETRM(OUTSTR,K,DFCFG,'W','def config',IER)
      elseif(WORD(1:4).eq.'*ctl')then
        CALL EGETRM(OUTSTR,K,DFCTL,'W','def control',IER)
      elseif(WORD(1:4).eq.'*mfn')then
        CALL EGETRM(OUTSTR,K,DAPROB,'W','def mass flow net',IER)
      elseif(WORD(1:4).eq.'*pnf')then
        CALL EGETRM(OUTSTR,K,DPNF,'W','def mass flow net',IER)
      elseif(WORD(1:4).eq.'*dfd')then
        CALL EGETRM(OUTSTR,K,DFCFD,'W','def domain flow definit.',
     &       IER)
      elseif(WORD(1:4).eq.'*res')then
        CALL EGETRM(OUTSTR,K,DEFRLB,'W','def results lib',IER)
      elseif(WORD(1:4).eq.'*mfr')then
        CALL EGETRM(OUTSTR,K,DAFRES,'W','def results lib',IER)
      elseif(WORD(1:4).eq.'*prm')then
        CALL EGETRM(OUTSTR,K,DFCON,'W','def material db',IER)
      elseif(WORD(1:4).eq.'*mlc')then
        CALL EGETRM(OUTSTR,K,DFMUL,'W','def multilayer db',IER)
      elseif(WORD(1:4).eq.'*opt')then
        CALL EGETRM(OUTSTR,K,DOPTDB,'W','def optical db',IER)
      elseif(WORD(1:4).eq.'*prs')then
        CALL EGETRM(OUTSTR,K,DAPRES,'W','def pressure db',IER)
      elseif(WORD(1:4).eq.'*evn')then
        CALL EGETRM(OUTSTR,K,DPRFDB,'W','def profiles db',IER)
      elseif(WORD(1:4).eq.'*clm')then
        CALL EGETRM(OUTSTR,K,DCLIM,'W','def climate db',IER)
      elseif(WORD(1:4).eq.'*pdb')then
        CALL EGETRM(OUTSTR,K,DPCDB,'W','def plant comp db',IER)
      elseif(WORD(1:5).eq.'*ecdb')then
        CALL EGETRM(OUTSTR,K,DECMPDBFL,'W','def elec comp db',IER)
      elseif(WORD(1:5).eq.'*mcdb')then
        CALL EGETRM(OUTSTR,K,DMCMPDBFL,'W','def misc comp db',IER)
      elseif(WORD(1:5).eq.'*icdb')then
        CALL EGETRM(OUTSTR,K,DICONDBFL,'W','default icons',IER)
      elseif(WORD(1:5).eq.'*mldb')then
        CALL EGETRM(OUTSTR,K,dmdbnam,'W','default mycotoxin db',IER)
      elseif(WORD(1:5).eq.'*sbem')then
        CALL EGETRM(OUTSTR,K,DSBEM,'W','default SBEM db',IER)
      elseif(WORD(1:14).eq.'*constructions')then
  46    continue
        CALL STRIPC(IFDEF,OUTSTR,0,ND,1,'construction tags',IER)
        K=0
        CALL EGETW(OUTSTR,K,WORD,'W','constr tags',IFLAG)
        if(WORD(1:4).eq.'*set')then
          icdtypes=icdtypes+1
          CALL EGETW(OUTSTR,K,cdtype(icdtypes),'W',
     &      'construction set type',IER)
          CALL EGETRM(OUTSTR,K,cdtypelabel(icdtypes),'W',
     &      'construction set type menu',IER)
          goto 46
        elseif(WORD(1:9).eq.'*ext_wall')then
          CALL EGETW(OUTSTR,K,cdextwall(icdtypes),'W',
     &      'construction set ext wall',IER)
          goto 46
        elseif(WORD(1:8).eq.'*ext_glz')then
          CALL EGETW(OUTSTR,K,cdextglz(icdtypes),'W',
     &      'construction set ext glazing',IER)
          goto 46
        elseif(WORD(1:8).eq.'*ext_frm')then
          CALL EGETW(OUTSTR,K,cdextfrm(icdtypes),'W',
     &      'construction set ext frame',IER)
          goto 46
        elseif(WORD(1:9).eq.'*ext_disp')then
          CALL EGETW(OUTSTR,K,cdextdisp(icdtypes),'W',
     &      'construction set ext display glazing',IER)
          goto 46
        elseif(WORD(1:9).eq.'*ext_door')then
          CALL EGETW(OUTSTR,K,cdextdoor(icdtypes),'W',
     &      'construction set ext door',IER)
          goto 46
        elseif(WORD(1:10).eq.'*ext_vdoor')then
          CALL EGETW(OUTSTR,K,cdextvdoor(icdtypes),'W',
     &      'construction set ext vehicle door',IER)
          goto 46
        elseif(WORD(1:9).eq.'*ext_flrf')then
          CALL EGETW(OUTSTR,K,cdextflrf(icdtypes),'W',
     &      'construction set ext flat roof',IER)
          goto 46
        elseif(WORD(1:9).eq.'*ext_slrf')then
          CALL EGETW(OUTSTR,K,cdextslrf(icdtypes),'W',
     &      'construction set ext flat roof',IER)
          goto 46
        elseif(WORD(1:9).eq.'*ext_skyl')then
          CALL EGETW(OUTSTR,K,cdextskyl(icdtypes),'W',
     &      'construction set ext skylight',IER)
          goto 46
        elseif(WORD(1:8).eq.'*ext_fnd')then
          CALL EGETW(OUTSTR,K,cdextfnd(icdtypes),'W',
     &      'construction set ext foundation',IER)
          goto 46
        elseif(WORD(1:10).eq.'*int_floor')then
          CALL EGETW(OUTSTR,K,cdintfloor(icdtypes),'W',
     &      'construction set internal floor',IER)
          goto 46
        elseif(WORD(1:9).eq.'*int_ceil')then
          CALL EGETW(OUTSTR,K,cdintceil(icdtypes),'W',
     &      'construction set internal floor',IER)
          goto 46
        elseif(WORD(1:8).eq.'*int_ptn')then
          CALL EGETW(OUTSTR,K,cdintptn(icdtypes),'W',
     &      'construction set internal partition',IER)
          goto 46
        elseif(WORD(1:9).eq.'*int_door')then
          CALL EGETW(OUTSTR,K,cdintdoor(icdtypes),'W',
     &      'construction set internal door',IER)
          goto 46
        elseif(WORD(1:8).eq.'*int_glz')then
          CALL EGETW(OUTSTR,K,cdintglz(icdtypes),'W',
     &      'construction set internal glazing',IER)
          goto 46
        elseif(WORD(1:8).eq.'*end_set')then
          goto 46
        elseif(WORD(1:18).eq.'*end_constructions')then
          goto 44
        else
          goto 46   ! nothing matches this try another line.
        endif
      elseif(WORD(1:4).eq.'*end')then
        goto 45
      else
        call usrmsg(' Unknown default in... ',outstr,'W')
        goto 44
      endif

      if(ier.ne.0)then
        write(outs248,'(3a)') 'Problem detected in ',
     &    defdbfl(:lnblnk(defdbfl)),' while reading...'
        CALL EDISP248(IUOUT,outs248,100)
        CALL EDISP(IUOUT,outstr)
        goto 45
      endif

C Loop back for next default file.
      goto 44

C Close defaults file before exiting.
   45 CONTINUE
      CALL ERPFREE(IFDEF,ISTAT)
      RETURN
      END SUBROUTINE

C findwhichdbpath: takes a database file name and checks to see if
C            the path is local or absolute or standard database folder.

C ******* findwhichdbpath (copied from esrucom/esystem.F)
C findwhichdbpath takes a database file name and checks to
C see if the path is local or absolute or standard database folder.
C Depending on which database (the parameter topic) the
C common block file string and whichdbpath variable is updated.
C topic =  'opt' for optics, 'pdb' for plant component templates
C       =  'prs' for pressure coef, 'sbm' for UK SBEM database,
C       =  'mat' for common materials, 'evn' for event profiles
C       =  'mul' for common constructions, 'msc' for active components
C       =  'mld' for mould isopleths, 'clm' for weather
      subroutine findwhichdbpath(topic,file,ier)
      implicit none
#include "esprdbfile.h"

      integer lnblnk  ! function definition
      integer ier  ! typd for passed parameter

C Currently works with optical and plant template database.
      character*(*) file
      character topic*3 ! identify which type of database
      integer lndbp,lnclmp ! length of standard database weather paths
      integer lnwkg   ! for length of working file name
      logical unixok  ! to check for database path file separators

C Set OS and check that standarddbpath standardweatherpath have been set.
      lndbp=lnblnk(standarddbpath)
      lnclmp=lnblnk(standardclmpath)
      lnwkg=lnblnk(file)
      if(lndbp.lt.1.or.lnwkg.lt.1)then
        ier=2
        return
      endif
      call isunix(unixok)
      
      if(unixok)then
        if(file(1:7).eq.'../dbs/')then

C Local model dbs folder.
          if(topic(1:3).eq.'opt')then
            ipathoptdb=1  ! local folder for common optics
            write(LOPTDB,'(a)') file(1:lnwkg) ! assign directly
          elseif(topic(1:3).eq.'pdb')then
            ipathpcdb=1  ! local folder for plant database
            write(LPCDB,'(a)') file(1:lnwkg) ! assign directly
          elseif(topic(1:3).eq.'prs')then
            ipathapres=1  ! local folder for pressure distributions
            write(lapres,'(a)') file(1:lnwkg) ! assign directly
          elseif(topic(1:3).eq.'sbm')then
            ipathsbem=1  ! local folder for UK SBEM database
            write(lsbem,'(a)') file(1:lnwkg) ! assign directly
          elseif(topic(1:3).eq.'mat')then
            ipathmat=1  ! local folder for common materials
            write(LFMAT,'(a)') file(1:lnwkg) ! assign directly
          elseif(topic(1:3).eq.'evn')then
            ipathprodb=1  ! local folder for event profiles
            write(LPRFDB,'(a)') file(1:lnwkg) ! assign directly
          elseif(topic(1:3).eq.'mul')then
            ipathmul=1  ! local folder for common constructions
            write(LFMUL,'(a)') file(1:lnwkg) ! assign directly
          elseif(topic(1:3).eq.'msc')then
            ipathmsc=1  ! local folder for active components
            write(MCMPDBFL,'(a)') file(1:lnwkg) ! assign directly
          elseif(topic(1:3).eq.'mld')then
            ipathmould=1  ! local folder for mould isopleths
            write(lfmould,'(a)') file(1:lnwkg) ! assign directly
          elseif(topic(1:3).eq.'clm')then
            ipathclim=1  ! local folder for weather
            write(LCLIM,'(a)') file(1:lnwkg) ! assign directly
          endif
        elseif(file(1:lndbp).eq.standarddbpath(1:lndbp))then

C The initial part of database path matches so save the file name w/o path.
          if(topic(1:3).eq.'opt')then
            ipathoptdb=2
            write(LOPTDB,'(a)') file(lndbp+2:lnwkg)  ! without path
          elseif(topic(1:3).eq.'pdb')then
            ipathpcdb=2
            write(LPCDB,'(a)')  file(lndbp+2:lnwkg)  ! without path
          elseif(topic(1:3).eq.'prs')then
            ipathapres=2
            write(lapres,'(a)') file(lndbp+2:lnwkg)  ! without path
          elseif(topic(1:3).eq.'sbm')then
            ipathsbem=2
            write(lsbem,'(a)') file(lndbp+2:lnwkg) ! without path
          elseif(topic(1:3).eq.'mat')then
            ipathmat=2
            write(LFMAT,'(a)') file(lndbp+2:lnwkg) ! without path
          elseif(topic(1:3).eq.'evn')then
            ipathprodb=2
            write(LPRFDB,'(a)') file(lndbp+2:lnwkg) ! without path
          elseif(topic(1:3).eq.'mul')then
            ipathmul=2
            write(LFMUL,'(a)') file(lndbp+2:lnwkg) ! without path
          elseif(topic(1:3).eq.'msc')then
            ipathmsc=2
            write(MCMPDBFL,'(a)') file(lndbp+2:lnwkg) ! without path
          elseif(topic(1:3).eq.'mld')then
            ipathmould=2
            write(lfmould,'(a)') file(lndbp+2:lnwkg) ! without path
          elseif(topic(1:3).eq.'clm')then
            ipathclim=2
            write(LCLIM,'(a)') file(lndbp+2:lnwkg) ! without path
          endif
        elseif(file(1:lnclmp).eq.standardclmpath(1:lnclmp))then

C The initial part of the weather path matches save the file name w/o path.
          if(topic(1:3).eq.'clm')then
            ipathclim=2
            write(LCLIM,'(a)') file(lnclmp+2:lnwkg) ! without path
          endif
        else
          if(topic(1:3).eq.'opt')then
            ipathoptdb=0
            write(LOPTDB,'(a)') file(1:lnwkg)  ! assign directly
          elseif(topic(1:3).eq.'pdb')then
            ipathpcdb=0
            write(LPCDB,'(a)')  file(1:lnwkg)  ! assign directly
          elseif(topic(1:3).eq.'prs')then
            ipathapres=0
            write(lapres,'(a)') file(1:lnwkg)  ! assign directly
          elseif(topic(1:3).eq.'sbm')then
            ipathsbem=0
            write(lsbem,'(a)') file(1:lnwkg) ! assign directly
          elseif(topic(1:3).eq.'mat')then
            ipathmat=0
            write(LFMAT,'(a)') file(1:lnwkg)  ! assign directly
          elseif(topic(1:3).eq.'evn')then
            ipathprodb=0
            write(LPRFDB,'(a)') file(1:lnwkg) ! assign directly
          elseif(topic(1:3).eq.'mul')then
            ipathmul=0
            write(LFMUL,'(a)') file(1:lnwkg) ! assign directly
          elseif(topic(1:3).eq.'msc')then
            ipathmsc=0
            write(MCMPDBFL,'(a)') file(1:lnwkg) ! assign directly
          elseif(topic(1:3).eq.'mld')then
            ipathmould=0  ! local folder for mould isopleths
            write(lfmould,'(a)') file(1:lnwkg) ! assign directly
          elseif(topic(1:3).eq.'clm')then
            ipathclim=0
            write(LCLIM,'(a)') file(1:lnwkg) ! assign directly
          endif
        endif
      else
        if(file(1:7).eq.'..\\dbs\\')then
          if(topic(1:3).eq.'opt')then
            ipathoptdb=1  ! local folder for optical database
            write(LOPTDB,'(a)') file(1:lnwkg)  ! assign directly
          elseif(topic(1:3).eq.'pdb')then
            ipathpcdb=1  ! local folder for plant database
            write(LPCDB,'(a)') file(1:lnwkg)   ! assign directly
          elseif(topic(1:3).eq.'prs')then
            ipathapres=1  ! local folder for pressure database
            write(lapres,'(a)') file(1:lnwkg) ! assign directly
          elseif(topic(1:3).eq.'sbm')then
            ipathsbem=1
            write(lsbem,'(a)') file(1:lnwkg) ! assign directly
          elseif(topic(1:3).eq.'mat')then
            ipathmat=1  ! local folder for materials database
            write(LFMAT,'(a)') file(1:lnwkg) ! assign directly
          elseif(topic(1:3).eq.'evn')then
            ipathprodb=1  ! local folder for profiles database
            write(LPRFDB,'(a)') file(1:lnwkg) ! assign directly
          elseif(topic(1:3).eq.'mul')then
            ipathmul=1  ! local folder for MLC database
            write(LFMUL,'(a)') file(1:lnwkg) ! assign directly
          elseif(topic(1:3).eq.'msc')then
            ipathmsc=1  ! local folder for active components
            write(MCMPDBFL,'(a)') file(1:lnwkg) ! assign directly
          elseif(topic(1:3).eq.'mld')then
            ipathmould=1  ! local folder for mould isopleths
            write(lfmould,'(a)') file(1:lnwkg) ! assign directly
          elseif(topic(1:3).eq.'clm')then
            ipathclim=1  ! local folder for weather
            write(LCLIM,'(a)') file(1:lnwkg) ! assign directly
          endif
        elseif(file(1:lndbp).eq.standarddbpath(1:lndbp))then
          if(topic(1:3).eq.'opt')then
            ipathoptdb=2
            write(LOPTDB,'(a)') file(lndbp+2:lnwkg) ! without path
          elseif(topic(1:3).eq.'pdb')then
            ipathpcdb=2
            write(LPCDB,'(a)')  file(lndbp+2:lnwkg)  ! without path
          elseif(topic(1:3).eq.'prs')then
            ipathapres=2
            write(lapres,'(a)') file(lndbp+2:lnwkg)  ! without path
          elseif(topic(1:3).eq.'sbm')then
            ipathsbem=2
            write(lsbem,'(a)') file(lndbp+2:lnwkg) ! without path
          elseif(topic(1:3).eq.'mat')then
            ipathmat=2
            write(LFMAT,'(a)') file(lndbp+2:lnwkg) ! without path
          elseif(topic(1:3).eq.'evn')then
            ipathprodb=2
            write(LPRFDB,'(a)') file(lndbp+2:lnwkg) ! without path
          elseif(topic(1:3).eq.'mul')then
            ipathmul=2
            write(LFMUL,'(a)') file(lndbp+2:lnwkg) ! without path
          elseif(topic(1:3).eq.'msc')then
            ipathmsc=2
            write(MCMPDBFL,'(a)') file(lndbp+2:lnwkg) ! without path
          elseif(topic(1:3).eq.'mld')then
            ipathmould=2
            write(lfmould,'(a)') file(lndbp+2:lnwkg) ! without path
          elseif(topic(1:3).eq.'clm')then
            ipathclim=2
            write(LCLIM,'(a)') file(lndbp+2:lnwkg) ! without path
          endif
        elseif(file(1:lnclmp).eq.standardclmpath(1:lnclmp))then
          if(topic(1:3).eq.'clm')then
            ipathclim=2
            write(LCLIM,'(a)') file(lnclmp+2:lnwkg) ! without path
          endif
        else
          if(topic(1:3).eq.'opt')then
            ipathoptdb=0
            write(LOPTDB,'(a)') file(1:lnwkg)  ! assign directly
          elseif(topic(1:3).eq.'pdb')then
            ipathpcdb=0
            write(LPCDB,'(a)')  file(1:lnwkg)  ! assign directly
          elseif(topic(1:3).eq.'prs')then
            ipathapres=0
            write(lapres,'(a)') file(1:lnwkg)  ! assign directly
          elseif(topic(1:3).eq.'sbm')then
            ipathsbem=0
            write(lsbem,'(a)') file(1:lnwkg) ! assign directly
          elseif(topic(1:3).eq.'mat')then
            ipathmat=0
            write(LFMAT,'(a)') file(1:lnwkg)  ! assign directly
          elseif(topic(1:3).eq.'evn')then
            ipathprodb=0
            write(LPRFDB,'(a)') file(1:lnwkg) ! assign directly
          elseif(topic(1:3).eq.'mul')then
            ipathmul=0
            write(LFMUL,'(a)') file(1:lnwkg) ! assign directly
          elseif(topic(1:3).eq.'msc')then
            ipathmsc=0
            write(MCMPDBFL,'(a)') file(1:lnwkg) ! assign directly
          elseif(topic(1:3).eq.'mld')then
            ipathmould=0
            write(lfmould,'(a)') file(1:lnwkg) ! assign directly
          elseif(topic(1:3).eq.'clm')then
            ipathclim=0
            write(LCLIM,'(a)') file(1:lnwkg) ! assign directly
          endif
        endif
      endif

C Debug.
      write(6,*) file
      if(topic(1:3).eq.'opt')then
C        write(6,*) loptdb
C        write(6,*) 'findwhichdbpath ipath is ',ipathoptdb
C      elseif(topic(1:3).eq.'pdb')then
C        write(6,*) LPCDB
C        write(6,*) 'findwhichdbpath ipath is ',ipathpcdb
C      elseif(topic(1:3).eq.'prs')then
C        write(6,*) lapres
C        write(6,*) 'findwhichdbpath ipath is ',ipathapres
      elseif(topic(1:3).eq.'sbm')then
        write(6,*) lsbem
        write(6,*) 'findwhichdbpath ipath is ',ipathsbem
C      elseif(topic(1:3).eq.'mat')then
C        write(6,*) LFMAT
C        write(6,*) 'findwhichdbpath ipath is ',ipathmat
C      elseif(topic(1:3).eq.'evn')then
C        write(6,*) LPRFDB
C        write(6,*) 'findwhichdbpath ipath is ',ipathprodb
C      elseif(topic(1:3).eq.'mul')then
C        write(6,*) LFMUL
C        write(6,*) 'findwhichdbpath ipath is ',ipathmul
C      elseif(topic(1:3).eq.'msc')then
C        write(6,*) MCMPDBFL
C        write(6,*) 'findwhichdbpath ipath is ',ipathmsc
      endif

      return
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
