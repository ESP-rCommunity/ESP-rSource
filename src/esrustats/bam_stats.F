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

C bamstats     - the main controlling routine.
C partfa - allows terminal, file and action arguments to be passed.

C ****************************************** csvstats ***************************************
C bamstats.F is the main routine which scans standard or compressed BAM log files to create
C a file fit for using as an import to temporal file. 
C Use -act to say whether it is a std or compact BAM log file.

C BAM log files tend to be one per hour so a number of files will eventually need
C to be scanned. If we assume that the user has initially created a temporal file
C with one entity for each anticipated sensor (initially sized for a dozen BAM
C sensors, and initially filled with zeros.
C New data that is scanned can overwrite portions of the temporal file for each of
C the sensors.

C Facilities needed to scan the initial temporal file (ASCII) and create a scratch
C binary temporal file for manipulation.

C Whatever the frequency of the input file we want to dump 5 minute data.

C The logic for writing within waxrec.c is:
C    sprintf(line, "TEDDI,%s,%u,%u,%u,%u,%u,%u,%u,%u,%u", timestamp(teddiPacket->timestampReceived, 3), 
C       teddiPacket->deviceId, teddiPacket->version,teddiPacket->sampleCount, teddiPacket->sequence,
C       teddiPacket->unsent,teddiPacket->temp, teddiPacket->light, teddiPacket->battery, teddiPacket->humidity);
C    for (i = 0; i < teddiPacket->sampleCount; i++)
C    {
C        sprintf(number, ",%u", teddiPacket->pirData[i]);
C        strcat(line, number);
C    }
C    for (i = 0; i < teddiPacket->sampleCount; i++)
C    {
C        sprintf(number, ",%u", teddiPacket->audioData[i]);
C        strcat(line, number);
C    }
C        strcat(line, "\n");

C An older BAM log file (which logged when it got a message) looks like:
C TEDDI,1355832008.974,2012-12-18 04:00:08.974,83,4,24,191,0,38755,957,687,8467,266,240,234,233,226,234,263,236,242,251,269,274,297,265,241,266,249,255,248,269,235,270,258,258,5,10,8,4,8,5,8,6,5,4,5,5,4,4,4,5,4,5,4,5,5,4,5,5
C TEDDI,1355832009.349,2012-12-18 04:00:09.349,119,4,24,3803,0,38819,973,694,8228,262,270,284,254,263,248,256,258,254,258,271,256,264,257,268,266,260,266,254,257,260,271,265,253,12,5,10,7,13,9,6,5,4,5,4,4,5,7,5,4,4,5,5,5,5,5,4,4
C TEDDI,1355832010.459,2012-12-18 04:00:10.459,103,4,24,4019,0,38637,969,693,8160,270,270,247,203,205,201,209,204,193,210,189,200,198,204,200,183,204,186,198,191,194,208,196,188,13,9,7,5,5,5,5,5,5,7,5,5,5,6,6,7,6,5,5,5,5,4,4,5
C TEDDI,1355832010.709,2012-12-18 04:00:10.709,114,4,24,23860,0,38961,842,693,7491,306,222,122,154,180,346,304,246,769,497,182,95,138,129,165,181,213,219,214,234,217,249,245,246,6,8,7,8,6,7,9,14,9,10,11,12,40,37,93,45,16,11,10,11,9,10,13,7
C TEDDI,1355832011.474,2012-12-18 04:00:11.474,118,4,24,916,0,38739,933,696,8487,245,247,224,240,221,224,240,245,213,249,227,236,233,231,232,223,231,260,285,219,202,208,202,213,5,6,5,5,6,6,6,6,5,5,5,5,5,5,6,5,6,6,5,5,5,5,6,5
C TEDDI,1355832012.178,2012-12-18 04:00:12.178,116,4,24,6359,0,38809,974,697,8176,296,285,278,248,272,292,297,232,263,252,224,252,241,269,297,274,252,282,270,264,228,306,289,249,9,9,10,10,8,9,10,9,10,9,10,10,8,9,9,8,9,9,9,9,8,8,9,8
C TEDDI,1355832012.568,2012-12-18 04:00:12.568,106,4,24,18486,0,38950,979,689,7875,265,265,258,252,282,246,260,255,263,258,299,272,279,294,295,542,322,249,65,124,189,216,225,229,10,14,11,15,28,27,29,30,20,14,15,15,16,17,17,10,17,17,32,14,11,27,14,24

C More recent BAM log files (which dump every 30 seconds) look like:
C TEDDI,1371842699.462,2013-06-21 20:24:59.462,107,5,24,4,1,38979,718,700,7883,1023,1023,1023,1023,1023,1023,1023,1023,1023,1023,1023,1023,1023,1023,1023,1023,1023,1023,1023,1023,1023,1023,1023,1023,10,9,18,48,26,21,29,39,24,24,19,11,13,37,94,29,53,17,13,14,7,6,6,7
C TEDDI,1371842699.618,2013-06-21 20:24:59.618,74,5,24,15,96,38912,755,727,7571,107,0,0,0,0,1023,790,601,0,0,0,215,418,171,135,307,405,418,398,352,289,278,290,284,7,24,359,401,342,94,24,16,14,14,14,15,15,14,14,10,14,11,17,7,8,9,225,428
C TEDDI,1371842699.633,2013-06-21 20:24:59.633,74,5,24,16,72,38914,713,733,7571,290,297,266,116,93,158,252,222,329,351,297,270,157,263,411,384,329,149,370,328,265,299,335,354,94,24,11,10,12,11,12,11,12,631,535,167,43,19,9,8,7,7,10,16,8,10,7,7
C TEDDI,1371842699.633,2013-06-21 20:24:59.633,74,5,24,17,48,38914,714,725,7571,273,193,231,335,326,373,349,311,297,312,275,252,280,280,265,250,232,319,342,330,326,333,307,267,7,10,8,8,8,7,6,6,6,6,5,8,8,10,8,10,13,11,10,8,7,8,7,9
C TEDDI,1371842699.649,2013-06-21 20:24:59.649,74,5,24,18,24,38916,708,735,7571,258,250,251,277,266,300,294,229,260,292,268,244,238,253,266,265,287,291,318,299,311,310,318,292,8,9,9,17,20,9,21,22,12,15,13,13,14,13,17,11,10,13,12,12,11,13,11,12
C TEDDI,1371842699.665,2013-06-21 20:24:59.665,74,5,24,19,1,38916,700,734,7571,275,259,272,258,263,271,267,263,304,322,299,304,265,266,275,251,294,312,336,312,281,291,272,269,13,37,33,19,29,40,32,30,21,14,9,8,108,76,64,45,19,8,12,7,7,6,7,7
C TEDDI,1371842704.781,2013-06-21 20:25:04.781,95,5,24,10,96,38922,949,713,7778,1023,1023,1023,1023,1023,1023,1023,1023,1023,684,418,243,265,124,266,423,615,488,431,366,337,319,235,271,10,120,366,122,26,12,10,12,11,12,12,11,486,597,241,62,22,10,9,8,8,9,16,11
C The token are:
C   TEDDI - identifer
C   1355832008.974 - seconds since epoch
C   2012-12-18 - year month day
C   20:24:59.462 - hour minute second fraction of second
C   107 - sensor ID
C   4,24 or 5,24 - version of log file and number of PIR packets 
C   15 - sequence number (to detect duplicate transmissions)
C   96 - number of packets remaining
C   38979 raw temperature
C   718   raw light
C   700   raw battery
C   7883  raw humidity
C   1023,1023,1023,1023,1023,1023,1023,1023,1023,1023,1023,1023,1023,1023,1023,1023,1023,1023,1023,1023,1023,1023,1023,1023,  24 PIR values
C   10,9,18,48,26,21,29,39,24,24,19,11,13,37,94,29,53,17,13,14,7,6,6,7  24 raw audio values

C Compact BAM log files look like:
C TEDDI,1373301699.462,2013-07-08 17:41:39.462,117,5,24,199,1,25.72,926,285.0,713,47.57,33,457,258,23.5,79.0,70.8
C TEDDI,1373301685.880,2013-07-08 17:41:25.880,75,5,24,125,96,25.06,459,8.3,480,49.73,21,298,176,23.5,49.1,48.0
C TEDDI,1373301691.891,2013-07-08 17:41:31.891,75,5,24,126,72,25.06,466,8.8,488,49.73,25,310,280,23.5,49.1,48.0
C TEDDI,1373301697.986,2013-07-08 17:41:37.986,75,5,24,127,50,25.06,472,9.2,491,49.73,21,321,260,23.5,49.1,48.0
C TEDDI,1373301703.996,2013-07-08 17:41:43.996,75,5,24,128,26,25.06,473,9.3,491,49.73,21,316,224,23.5,49.1,48.0
C TEDDI,1373301709.875,2013-07-08 17:41:49.875,75,5,24,129,7,25.06,461,8.4,491,49.73,266,314,291,36.5,37.9,36.5
C TEDDI,1373301705.667,2013-07-08 17:41:45.667,117,5,24,200,96,25.72,922,285.0,705,47.57,0,357,226,42.8,65.8,49.1


C *********************************** csvstats ******************************

      PROGRAM bamstats
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

      CHARACTER INTER*144,INF*144,LFOUT*144
      character path*72
      character outs*123,outs248*248,loutstr*1000,WORD*24
      character fs*1
      character act*16 ! B is std BAM log file C compact log filecsv file
      character actd*1 ! B is std BAM log file C compact log filecsv file
      character datea*24

      character cVnum*16      ! returned from ESPrVersionNum
      character pagestitle*42 ! for banner title via epages call
      logical unixok
      logical closetofive  ! true if we near a 5 minute mark
      logical closetohour  ! true if we are near an hour mark
      logical alreadywritten ! set true when we have written out values at timeslot
                             ! need to figure out how to reset

      real juliand,julianfrac  ! returned julian day and fraction of day
      real secondsinday  ! seconds per day
      real secondsinhour ! seconds per hour
      real secondsin5min ! seconds per 5 minutes
      real accumulatedsec ! seconds up to the current hour
      real hourdayfrac   ! fraction of the julian day at each hour start
      dimension hourdayfrac(0:23)
      real fiveminfrac   ! fraction of the julian day for 5 minutes
      real oneminfrac    ! fraction of the julian day for 1 minute
      real tensecfrac    ! within 10 seconds
      real nowis         ! use with 5 minute incrementing test
      integer k,nd       ! for looping

C Local variables for reading raw BAM sensor data in the log file.
      integer isen,idum,iseq,ipacketsleft,irdbt,irlight,irrh
      real lux,lexponent ! for RH calculation
      real thehouris  ! the csv file starts at hour
      logical nearstartofhour ! if file started near hour mark

C Mapping between the sensor index and array of (up to) a dozen sensors.
      integer mapsentoarray
      dimension mapsentoarray(12)
      logical foundit  ! for establishing link between sensor index and mapsentoarray
      integer howmany  ! number of sensors found
      integer howmanynow ! counter for if we got all sensor data at this time

C Data for selected timestep for each sensor.
      real bamtmp,bamlight,bamhumidity,bampir,bamaudio
      dimension bamtmp(12),bamlight(12),bamhumidity(12),bampir(12)
      dimension bamaudio(12)

C Initial assumptions.
      call ezero
      ITRC=1
      IUOUT=6
      IUIN=5
      LIMTTY=24
      LIMIT =24
      IFIL=10
      NSIGFIG=3
      INTER=' '

C Clear mapping array.
      do ij=1,12
        mapsentoarray(ij)=0
      enddo

C Setup constants.
      secondsinday = 86400.0
      secondsinhour= 3600.0
      secondsin5min= 300.0

C Get command line parameters (MODL= text or graphic), inf = the BAM log file,
C act = b or B (standard file) or c C for compact.
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

      write(pagestitle,'(a)') 'ESP-r BAM parser '

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
        write(outs,'(a,a)') 'command line action is ',act
        call edisp(iuout,outs)
      endif

C Clear common blocks. <<  ?? >>

C Setup fractions of the day related to each hour for
C testing against. Also get the day fraction increment
C for 5 minutes.
      accumulatedsec=0.0
      hourdayfrac(0)=0.0
      do ij=1,23
        accumulatedsec=accumulatedsec+secondsinhour
        hourdayfrac(ij)=accumulatedsec/secondsinday
      enddo
      fiveminfrac=secondsin5min/secondsinday
      oneminfrac=60.0/secondsinday
      tensecfrac=10.0/secondsinday
      write(6,*) hourdayfrac

      actd='B'
      datea='2013-06-21 20:24:53.518'
      call bamdatetojulian(datea,actd,juliand,julianfrac)
      write(6,*) datea,' ',juliand,' ',julianfrac

      datea='2013-06-21 20:24:59.446'
      call bamdatetojulian(datea,actd,juliand,julianfrac)
      write(6,*) datea,' ',juliand,' ',julianfrac

      datea='2013-06-21 20:29:53.413'
      call bamdatetojulian(datea,actd,juliand,julianfrac)
      write(6,*) datea,' ',juliand,' ',julianfrac

C Call the survey subroutines, the main controlling routine with the 
C filename argument.  B for BAM standard csv files, C for compact 
C log files.
      if(act(1:1).eq.'b'.or.act(1:1).eq.'B'.or.act(1:1).eq.'-')then

C Assume csv file is on IFIL.
        IUNIT=IFIL
        IUF=IFIL+1
        CALL EFOPSEQ(IUNIT,INTER,1,IER)
        if(ier.ne.0)then
          call usrmsg(' Problem opening...',INTER,'W')
          stop
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
        write(6,*) 'reporting to > ',LFOUT(1:lnblnk(LFOUT))
        CALL EFOPSEQ(IUF,LFOUT,4,IER)
        if(ier.eq.0)then
          continue
        else
          call usrmsg(' Problem opening...',LFOUT,'W')
          stop
        endif
        write(6,*) 'working in progress...bB',INTER

C Discover how many different sensors are included. Read the first 24 lines.
        howmany=0      ! start with no sensors
        thehouris=0.0  ! clear assumption about when the csv file time starts
        nearstartofhour = .false.
        do ij=1,24
          call stripc1k(IUNIT,loutstr,0,ND,1,'log line',IER)
          K=0
          CALL EGETW(loutstr,K,WORD,'W','TEDDI token',IFLAG)
          CALL EGETW(loutstr,K,WORD,'W','ignore time number',IFLAG)
          CALL EGETP(loutstr,K,datea,'W','YYYY-MM-DD hh:mm:ss.f',IFLAG)

C Typically log files only cover an hour. See which hour this is by checking
C the timing of the 2nd line of the csv file. Note: if the initial lines do
C not begin near the start of an hour then try alternative logic.
          if(ij.eq.2)then
            actd='B'
            call bamdatetojulian(datea,actd,juliand,julianfrac)
            do ih=0,23
              closetohour=.false.
              call ECLOSE(julianfrac,hourdayfrac(ih),oneminfrac,
     &          closetohour)
              if(closetohour)then
                write(6,*) 'csv file is for hour ',ih,julianfrac
                thehouris=julianfrac
                nearstartofhour=.true.
              endif
            enddo
            if(nearstartofhour)then
              continue
            else

C See if julianfrac between known hour fractions.
              do ih=0,22
                if(julianfrac.gt.hourdayfrac(ih).and.
     &             julianfrac.lt.hourdayfrac(ih+1))then
                  thehouris=hourdayfrac(ih)
                  write(6,*) 'csv file is for hour ',ih,thehouris
                endif
              enddo
            endif
          endif

          CALL EGETWI(loutstr,K,isen,0,200,'W','sensor index',IFLAG)

C Has sensor id (isen) been mentioned? If so set foundit true.
          foundit=.false.
          do ik=1,12
            if(mapsentoarray(ik).eq.isen)then
               foundit=.true.
            endif
          enddo
          if(foundit)then
            continue
          else

C Did not find it so set the first open slot of mapsentoarray
            mapsentoarray(howmany+1)=isen
            howmany=howmany+1
            foundit=.true.
          endif
        enddo
        write(6,*) howmany,mapsentoarray
        rewind(IUNIT)   ! go back to the start of the csv file

C First test is to see if we can identify lines in the log file that match specific
C times and print out the information.

C We want the 3rd token on the line. Also remember the sequence number so that if
C the next read is the same then it can be skipped.
        lastseq=0
        howmanynow=0
 42     call stripc1k(IUNIT,loutstr,0,ND,1,'log line',IER)
        if(IER.ne.0)then

C Debug.
          write(6,*) 'stripc end of file ',ier
          CALL ERPFREE(IUNIT,ISTAT)
          stop
        endif
        K=0
        CALL EGETW(loutstr,K,WORD,'W','TEDDI token',IFLAG)
        CALL EGETW(loutstr,K,WORD,'W','ignore time number',IFLAG)
        CALL EGETP(loutstr,K,datea,'W','YYYY-MM-DD hh:mm:ss.fff',IFLAG)
C Debug.
C        write(6,*) '1st tokens ',WORD,' ',datea
        CALL EGETWI(loutstr,K,isen,0,200,'W','sensor index',IFLAG)
        CALL EGETWI(loutstr,K,idum,4,5,'W','version',IFLAG)
        CALL EGETWI(loutstr,K,idum,0,24,'W','pir packets',IFLAG)
        CALL EGETWI(loutstr,K,iseq,0,100000,'W',
     &    'sequence number',IFLAG)
        if(iseq.eq.lastseq)then
          write(6,*) 'Duplicate sequence ',iseq
          goto 42
        else
          lastseq=iseq
        endif

        CALL EGETWI(loutstr,K,ipacketsleft,0,250,'W',
     &    'nb of packets left',IER)
        if(ipacketsleft.gt.10)then
          write(6,*) 'to many packets left ',ipacketsleft
          goto 42
        endif

C Convert raw BAM data to dbt using calibration info.
        CALL EGETWI(loutstr,K,irdbt,0,40000,'W','raw temperature',IER)
        dbt= (0.01185055 * real(irdbt)) - 438.169086

C Convert BAM light to lux.
        CALL EGETWI(loutstr,K,irlight,0,1000,'W','raw light',IER)
        if(irlight.gt.850)then
          lux = 285.0
        else
          lexponent = real(irlight) * 0.0080849;
          lux = 0.202549 * exp(lexponent)
        endif

        CALL EGETWI(loutstr,K,idum,0,1000,'W','battery',IER)

C Convert raw BAM data to RH via calibration info.
        CALL EGETWI(loutstr,K,irrh,0,40000,'W','raw humidity',IER)
        rhl = (49.6 * log(real(irrh))) - 397.19
        actd='B'
        call bamdatetojulian(datea,actd,juliand,julianfrac)

C Are we close to the hour mark or one of the five minut marks.
        closetohour=.false.
        closetofive=.false.
        nowis=thehouris
        call ECLOSE(julianfrac,thehouris,tensecfrac,closetohour)
        if(closetohour)then
          write(6,*) 'closetohour ',closetohour,thehouris,datea
          goto 43
        endif

C See if it is within 10 seconds of one of the 5 minute slots.
        do ifive=1,12
          nowis=nowis+fiveminfrac
          call ECLOSE(julianfrac,nowis,tensecfrac,closetofive)
          if(closetofive)then
            write(6,*) 'closetofive ',closetofive,nowis,ifive,datea
            goto 43
          endif
        enddo

C If we are not near a desired time go and read another line.
  43    if(closetohour.or.closetofive)then
          continue
        else
          goto 42
        endif

C Stuff into the arrays if close to the desired time. And we need to ensure
C that we have collected data from all of the sensors before we skip forward in time.
C The following logic results in two passes of sensors near the timestep.
        do ij=1,howmany
          iwhich=mapsentoarray(ij)
          if(isen.eq.iwhich)then
            bamtmp(ij)=dbt
            bamlight(ij)=lux
            bamhumidity(ij)=rhl
            howmanynow=howmanynow+1  ! got another of the sensors
            write(IUF,*) datea,' ',juliand,' * ',isen,dbt,rhl,lux,
     &        howmanynow
            goto 44
          endif
        enddo
  44    continue

C If all sensors captured and remembered we do not need to check for several more minutes.
C So read lines equivalent to 3x the number of sensors.
        if(howmanynow.ge.howmany)then
          do ij=1,howmany*3
            call stripc1k(IUNIT,loutstr,0,ND,1,'skip lines',IER)
            if(IER.ne.0)then

C Debug.
              write(6,*) 'stripc end of file ',ier
              CALL ERPFREE(IUNIT,ISTAT)
              stop
            endif
          enddo
          howmanynow=0  ! reset
        endif
        goto 42

C << to be done >>

      elseif(act(1:1).eq.'c'.or.act(1:1).eq.'C')then

C << subroutine in planning >>
        write(6,*) 'tobe done...cC',INTER

      elseif(act(1:1).eq.'-')then
        write(6,*) 'done...',INTER
      endif

      END



C Parse command lines.

C *********
C Logic needed to scan standard BAM log files...
C Log file is for one hour. It tends to start with 2013-01-30 00:00:04.402
C and ends with 2013-01-30 00:59:59.324. Older (v4 files) will send data-as
C collected, (v5 files) will send 30 seconds worth in a burst.

C To collect at 5 minute intervals we should have an idea of 'when'
C we are looking for next and using bamdatetojulian to get timestamp.
C Also we need arrays to hold values for up to a dozen BAM sensors.

C The order that sensor data is collected is not fixed so also need
C to keep track of which bamtmp array matches which BAM sensor ID.

C Logic for writing an ASCII temporal file (header and/or contents)...



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
      CHARACTER OUTSTR*124,WORD*20,loutstr*248
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
C      elseif(topic(1:3).eq.'sbm')then
C        write(6,*) lsbem
C        write(6,*) 'findwhichdbpath ipath is ',ipathsbem
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

C ****** bamdatetojulian
C bamdatetojulian is passed date string (from BAM log) and returns a 
C julian day and fraction juliand.
C act is requested action 'B' is for BAM log timing format
C   which is: 2013-06-28 16:15:26.662
C << what other kind of date stamp to decode? >>

      subroutine bamdatetojulian(datea,act,juliand,julianfrac)
      common/OUTIN/IUOUT,IUIN
      character datea*24,act*1,datetest*24,word*12

C Setup indices to hold the day of the week (1-7), the month
C (1-12), the day of the month (1-31), the hour of the day (1-24), the
C minute of the hour (1-60) second of minute and the year.
      integer imon,idom,ihrod,imohr,isomn,iyears,ijday
      real secofhr
      real totalsecs
      real juliand,julianfrac
      real fiveminseconds  ! seconds in 5 minutes
      real seconds

      datetest=datea
      K=0
      CALL EGETW(datetest,K,WORD,'W','yyyy-mm-dd',IFLAG)
      read(word(1:4),*,IOSTAT=IOS,ERR=1002) iyears
      read(word(6:7),*,IOSTAT=IOS,ERR=1003) imon
      read(word(9:10),*,IOSTAT=IOS,ERR=1003) idom
C Debug.
C      write(6,*) iyears,' ',imon,' ',idom

      CALL EGETW(datetest,K,WORD,'W','hh:mm:ss.fff',IFLAG)
      read(word(1:2),*,IOSTAT=IOS,ERR=1002) ihrod
      read(word(4:5),*,IOSTAT=IOS,ERR=1003) imohr
      read(word(7:12),*,IOSTAT=IOS,ERR=1003) secofhr
C Debug.
C      write(6,*) ihrod,' ',imohr,' ',secofhr
      
C Find the julian day for the current year month and day.
      CALL EDAY(IDOM,IMON,IJDAY)

C Find the fraction of the day from the current hour minute and second.
C There are 86400 seconds in a day. Divding the total seconds up to the
C moment of the datetest by 86400 yields the fraction of the day.
      totalsecs=secofhr+(real(imohr)*60.0)+ (real(ihrod)*3600.0)
      juliand=real(ijday)+(totalsecs/86400.0)
      julianfrac=(totalsecs/86400.0)
C Debug.
C      write(6,*) totalsecs,' ',juliand,' ',julianfrac
      fiveminseconds =300.0/86400.0
      
      return

 1002 if(IOS.eq.2)then
        call edisp(iuout,
     &    'comparedate: permission error extracting hour.')
      else
        call edisp(iuout,'comparedate: error extracting hour.')
      endif
      return
 1003 if(IOS.eq.2)then
        call edisp(iuout,
     &    'comparedate: permission error extracting minute.')
      else
        call edisp(iuout,'comparedate: error extracting minute.')
      endif
      return
 1004 if(IOS.eq.2)then
        call edisp(iuout,
     &    'comparedate: permission error extracting second.')
      else
        call edisp(iuout,'comparedate: error extracting second.')
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
