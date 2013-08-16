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

C ****************************************** bamstats ***************************************
C bamstats.F is the main routine which scans standard or short BuildAX log files to create
C a file fit for using as an import to temporal file. 
C Use -act to say whether it is a std or short or units BuiltAX log file.

C BuildAX log files tend to be one per hour so a number of files will eventually need
C to be scanned. If we assume that the user has initially created a temporal file
C with one entity for each anticipated sensor (initially sized for a dozen BAM
C sensors, and initially filled with zeros.
C New data that is scanned can overwrite portions of the temporal file for each of
C the sensors.

C Whatever the frequency of the input file we want to dump 5 minute data.
C The logic would need to be generalized to dump at other frequencies.

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

C An older BuildAX log file (which logged when it got a message) looks like:
C TEDDI,1355832008.974,2012-12-18 04:00:08.974,83,4,24,191,0,38755,957,687,8467,266,240,234,233,226,234,263,236,242,251,269,274,297,265,241,266,249,255,248,269,235,270,258,258,5,10,8,4,8,5,8,6,5,4,5,5,4,4,4,5,4,5,4,5,5,4,5,5
C TEDDI,1355832009.349,2012-12-18 04:00:09.349,119,4,24,3803,0,38819,973,694,8228,262,270,284,254,263,248,256,258,254,258,271,256,264,257,268,266,260,266,254,257,260,271,265,253,12,5,10,7,13,9,6,5,4,5,4,4,5,7,5,4,4,5,5,5,5,5,4,4
C TEDDI,1355832010.459,2012-12-18 04:00:10.459,103,4,24,4019,0,38637,969,693,8160,270,270,247,203,205,201,209,204,193,210,189,200,198,204,200,183,204,186,198,191,194,208,196,188,13,9,7,5,5,5,5,5,5,7,5,5,5,6,6,7,6,5,5,5,5,4,4,5
C TEDDI,1355832010.709,2012-12-18 04:00:10.709,114,4,24,23860,0,38961,842,693,7491,306,222,122,154,180,346,304,246,769,497,182,95,138,129,165,181,213,219,214,234,217,249,245,246,6,8,7,8,6,7,9,14,9,10,11,12,40,37,93,45,16,11,10,11,9,10,13,7
C TEDDI,1355832011.474,2012-12-18 04:00:11.474,118,4,24,916,0,38739,933,696,8487,245,247,224,240,221,224,240,245,213,249,227,236,233,231,232,223,231,260,285,219,202,208,202,213,5,6,5,5,6,6,6,6,5,5,5,5,5,5,6,5,6,6,5,5,5,5,6,5
C TEDDI,1355832012.178,2012-12-18 04:00:12.178,116,4,24,6359,0,38809,974,697,8176,296,285,278,248,272,292,297,232,263,252,224,252,241,269,297,274,252,282,270,264,228,306,289,249,9,9,10,10,8,9,10,9,10,9,10,10,8,9,9,8,9,9,9,9,8,8,9,8
C TEDDI,1355832012.568,2012-12-18 04:00:12.568,106,4,24,18486,0,38950,979,689,7875,265,265,258,252,282,246,260,255,263,258,299,272,279,294,295,542,322,249,65,124,189,216,225,229,10,14,11,15,28,27,29,30,20,14,15,15,16,17,17,10,17,17,32,14,11,27,14,24

C More recent BuildAX log files (which dump every 30 seconds) look like:
C TEDDI,1371842699.462,2013-06-21 20:24:59.462,107,5,24,4,1,38979,718,700,7883,1023,1023,1023,1023,1023,1023,1023,1023,1023,1023,1023,1023,1023,1023,1023,1023,1023,1023,1023,1023,1023,1023,1023,1023,10,9,18,48,26,21,29,39,24,24,19,11,13,37,94,29,53,17,13,14,7,6,6,7
C TEDDI,1371842699.618,2013-06-21 20:24:59.618,74,5,24,15,96,38912,755,727,7571,107,0,0,0,0,1023,790,601,0,0,0,215,418,171,135,307,405,418,398,352,289,278,290,284,7,24,359,401,342,94,24,16,14,14,14,15,15,14,14,10,14,11,17,7,8,9,225,428
C TEDDI,1371842699.633,2013-06-21 20:24:59.633,74,5,24,16,72,38914,713,733,7571,290,297,266,116,93,158,252,222,329,351,297,270,157,263,411,384,329,149,370,328,265,299,335,354,94,24,11,10,12,11,12,11,12,631,535,167,43,19,9,8,7,7,10,16,8,10,7,7
C TEDDI,1371842699.633,2013-06-21 20:24:59.633,74,5,24,17,48,38914,714,725,7571,273,193,231,335,326,373,349,311,297,312,275,252,280,280,265,250,232,319,342,330,326,333,307,267,7,10,8,8,8,7,6,6,6,6,5,8,8,10,8,10,13,11,10,8,7,8,7,9
C TEDDI,1371842699.649,2013-06-21 20:24:59.649,74,5,24,18,24,38916,708,735,7571,258,250,251,277,266,300,294,229,260,292,268,244,238,253,266,265,287,291,318,299,311,310,318,292,8,9,9,17,20,9,21,22,12,15,13,13,14,13,17,11,10,13,12,12,11,13,11,12
C TEDDI,1371842699.665,2013-06-21 20:24:59.665,74,5,24,19,1,38916,700,734,7571,275,259,272,258,263,271,267,263,304,322,299,304,265,266,275,251,294,312,336,312,281,291,272,269,13,37,33,19,29,40,32,30,21,14,9,8,108,76,64,45,19,8,12,7,7,6,7,7
C TEDDI,1371842704.781,2013-06-21 20:25:04.781,95,5,24,10,96,38922,949,713,7778,1023,1023,1023,1023,1023,1023,1023,1023,1023,684,418,243,265,124,266,423,615,488,431,366,337,319,235,271,10,120,366,122,26,12,10,12,11,12,12,11,486,597,241,62,22,10,9,8,8,9,16,11
C The tokens are:
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

C Short BuildAX log files look like:
C TEDDI_SHORT,1376633374.815,2013-08-16 07:09:34.815,107,5,24,0,97,38736,423,674,11361,500,1023,1001,0,657,97
C TEDDI_SHORT,1376633380.837,2013-08-16 07:09:40.837,107,5,24,1,73,38738,461,667,11389,0,1023,146,5,11,6
C TEDDI_SHORT,1376633386.849,2013-08-16 07:09:46.849,107,5,24,2,49,38741,468,665,11446,134,207,170,4,7,4
C TEDDI_SHORT,1376633392.858,2013-08-16 07:09:52.858,107,5,24,3,25,38745,461,664,11439,16,924,369,0,20,15
C TEDDI_SHORT,1376633398.869,2013-08-16 07:09:58.869,107,5,24,4,1,38750,479,663,11400,4,999,528,0,16,14
C The tokens are:
C   TEDDI_SHORT - identifer
C   1376633374.815 - seconds since epoch
C   2013-08-16 - year month day
C   07:09:34.815 - hour minute second fraction of second
C   107 - sensor ID
C   4,24 or 5,24 - version of log file and number of PIR packets 
C   1 - sequence number (to detect duplicate transmissions)
C   97 - number of packets remaining
C   38736 raw temperature
C   423   raw light
C   674   raw battery
C   11361  raw humidity
C   500,1023,1001, raw PIR minimum peak average
C   0,657,97  raw audio minimum peak average

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
      character outs*123,outs248*248,loutstr*1000,WORD*24,delim*1
      character fs*1
      character act*16 ! B is std log file S short log file U units log file
      character actd*1 ! B is std log file S short log file U units log file
      character datea*24

C      character cVnum*16      ! returned from ESPrVersionNum
      character pagestitle*42 ! for banner title via epages call
      logical unixok
      logical closetofive  ! true if we near a 5 minute mark
      logical closetohour  ! true if we are near an hour mark

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
      real twentysecfrac ! within 20 seconds
      real thirtysecfrac ! within 30 seconds
      real fourtysecfrac ! within 40 seconds
      real nowis         ! use with 5 minute incrementing test
      real thejulianis   ! to remember current time
      real printedat     ! to remember julian when data printed
      integer k,nd       ! for looping

C Local variables for reading raw BAM sensor data in the log file.
      integer isen,idum,iver,ipacketsleft,irdbt,irlight,irrh
      integer ipir,ipirpeak   ! for pir values
      integer iaudio,iaudiopeak ! for audio values
      real lux,lexponent ! for RH calculation
      real rdbt,rlight,rrh,audio,audiopeak
      real thehouris  ! the csv file starts at hour
      logical nearstartofhour ! if file started near hour mark
      integer lastseq,iseq    ! sequence number current and prior
      dimension lastseq(12)
      real tabular
      dimension tabular(60)   ! array for writing out line of data
      integer ipos            ! position in tabular
      character t12a*12,t12b*12,t12c*12,t12d*12,t12e*12
      integer lnt12a,lnt12b,lnt12c,lnt12d,lnt12e,lntabcde


C Mapping between the sensor index and array of (up to) a dozen sensors.
      integer icurrentsenarray ! which part of the array to work with
      integer mapsentoarray,mapsent
      dimension mapsentoarray(12),mapsent(12)
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
      icurrentsenarray=0; iseq=0; thejulianis=0.0; printedat=-1.0
      do ij=1,12
        mapsentoarray(ij)=0
        bamtmp(ij)=0.0
        bamlight(ij)=0.0
        bamhumidity(ij)=0.0
      enddo

C Setup constants.
      secondsinday = 86400.0
      secondsinhour= 3600.0
      secondsin5min= 300.0

C Get command line parameters (MODL= text assumed), inf = the BuildAX log file,
C act = b or B (standard file) or s S for -format:short and u U for -format:units.
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
      twentysecfrac=20.0/secondsinday
      thirtysecfrac=30.0/secondsinday
      fourtysecfrac=45.0/secondsinday

      actd='B'   ! assume standard BuildAX log file
      datea='2013-06-21 20:24:53.518'
      call bamdatetojulian(datea,actd,juliand,julianfrac)
C      write(6,*) datea,' ',juliand,' ',julianfrac

      datea='2013-06-21 20:24:59.446'
      call bamdatetojulian(datea,actd,juliand,julianfrac)
C      write(6,*) datea,' ',juliand,' ',julianfrac

      datea='2013-06-21 20:29:53.413'
      call bamdatetojulian(datea,actd,juliand,julianfrac)
C      write(6,*) datea,' ',juliand,' ',julianfrac

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
      write(6,*) 'processing...',INTER

C Call the survey subroutines, the main controlling routine with the 
C filename argument.  B for BAM standard csv files, S for -format:short
C log files and U for -format:units format.
      if(act(1:1).eq.'b'.or.act(1:1).eq.'B'.or.
     &   act(1:1).eq.'s'.or.act(1:1).eq.'S'.or.
     &   act(1:1).eq.'u'.or.act(1:1).eq.'U'.or.act(1:1).eq.'-')then

C Discover how many different sensors are included. Read the first 100 lines.
C << If file is less than 100 lines need to trap error and jump to an
C << appropriate point.
        howmany=0      ! start with no sensors
        thehouris=0.0  ! clear assumption about when the csv file time starts
        nearstartofhour = .false.
        do ij=1,100
          call stripc1k(IUNIT,loutstr,0,ND,1,'log line',IER)
          if(IER.ne.0)then
            write(6,*) 'This was a short log file.'
            ier=0
            goto 44   ! this was a short log file so jump
          endif
          K=0

C << The logic use the TEDDI TEDDI_SHORT TEDDI_UNITS to set act
C << and thus no need for act (or act could be used for something else

          CALL EGETW(loutstr,K,WORD,'W','TEDDI token',IFLAG)
          CALL EGETW(loutstr,K,WORD,'W','ignore time number',IFLAG)
          CALL EGETP(loutstr,K,datea,'W','YYYY-MM-DD hh:mm:ss.f',IFLAG)

C Log files cover an unknown period. See which hour this is by checking
C the timing of the 2nd line of the csv file. Note: if the initial lines do
C not begin near the start of an hour use alternative logic.
C << can this logic be moved ?? >>
          if(ij.eq.2)then
            actd='B'
            call bamdatetojulian(datea,actd,juliand,julianfrac)
            do ih=0,23
              closetohour=.false.
              call ECLOSE(julianfrac,hourdayfrac(ih),oneminfrac,
     &          closetohour)
              if(closetohour)then
                thehouris=hourdayfrac(ih)  ! remember fraction
                thejulanis=juliand    ! remenber julian
                write(6,*) 'csv file is for hour ',ih,julianfrac,
     &            thehouris
                nearstartofhour=.true.
              endif
            enddo
            if(nearstartofhour)then
              continue
            else

C See if julianfrac between known hour fractions remember thehouris.
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
C Debug.
   44   write(6,*) 'how many sensors found ',howmany,mapsentoarray

C Sort the array (increasing) so that data can be written in a standard
C order rather than the random order found in the intial lines of the file.
        KFLAG=1
        call SORTI(mapsentoarray,mapsent,howmany,KFLAG)
        write(6,*) 'after sort ',howmany,mapsentoarray
        if(act(1:1).eq.'b'.or.act(1:1).eq.'B')then
          write(IUF,'(a)')'BuildAX summary from standard log'
        elseif(act(1:1).eq.'s'.or.act(1:1).eq.'S')then
          write(IUF,'(a)')'BuildAX summary from TEDDI_SHORT'
        elseif(act(1:1).eq.'u'.or.act(1:1).eq.'U')then
          write(IUF,'(a)')'BuildAX summary from TEDDI_UNITS'
        endif

        write(IUF,'(a,i2)')'# number of sensors ',howmany
        write(IUF,*)'# sensor list ',mapsentoarray

C Generate a header line with appropriate titles for the collected sensors
        irows=1
        K4=33   ! initial value for buffer
        loutstr = '#JulianTime,yyyy-mm-dd hh:mm:ss,'
        do loop = 1,howmany
          write(t12a,'(a,i3.3,a)') 's',mapsentoarray(loop),'_degC'
          lnt12a=lnblnk(t12a)
          write(t12b,'(a,i3.3,a)') 's',mapsentoarray(loop),'_Lux'
          lnt12b=lnblnk(t12b)
          write(t12c,'(a,i3.3,a)') 's',mapsentoarray(loop),'_RH'
          lnt12c=lnblnk(t12c)
          write(t12d,'(a,i3.3,a)') 's',mapsentoarray(loop),'_PIR'
          lnt12d=lnblnk(t12d)
          write(t12e,'(a,i3.3,a)') 's',mapsentoarray(loop),'_dbA'
          lnt12e=lnblnk(t12e)
          lntabcde = lnt12a + lnt12b + lnt12c + lnt12d + lnt12e
          KE4=K4+(lntabcde+5) ! set for 1k buffer
          write(loutstr(K4:KE4),'(10a)') t12a(1:lnt12a),',',
     &      t12b(1:lnt12b),',',t12c(1:lnt12c),',',t12d(1:lnt12d),',',
     &      t12e(1:lnt12e),','
C          write(6,*) loutstr(1:lnblnk(loutstr))
          K4=KE4
        enddo
        lnb=lnblnk(loutstr)+1   ! ensure we do not warp line
        write(IUF,'(A)',iostat=ios,err=2) loutstr(1:lnb)

C Noticed that rewind did not work so close and open instead.
C        rewind(IUNIT)   ! go back to the start of the csv file
        CALL ERPFREE(IUNIT,ISTAT)
        CALL EFOPSEQ(IUNIT,INTER,1,IER)
        if(ier.ne.0)then
          call usrmsg(' Problem re-opening...',INTER,'W')
          stop
        endif

C First test is to see if we can identify lines in the log file that match specific
C times (near hour or 5 minute mark) and print out the information.

C We want the 3rd token on the line. Also remember the sequence number so that if
C the next read is the same then it can be skipped.
        lastseq=0
        howmanynow=0
 42     call stripc1k(IUNIT,loutstr,0,ND,1,'log line',IER)
        if(IER.ne.0)then

C Debug.
          write(6,*) 'stripc end of file ',ier
          CALL ERPFREE(IUNIT,ISTAT)
          CALL ERPFREE(IUF,ISTAT)
          stop
        endif
        K=0
        CALL EGETW(loutstr,K,WORD,'W','TEDDI token',IFLAG)
        CALL EGETW(loutstr,K,WORD,'W','ignore time number',IFLAG)
        CALL EGETP(loutstr,K,datea,'W','YYYY-MM-DD hh:mm:ss.fff',IFLAG)
        CALL EGETWI(loutstr,K,isen,0,200,'W','sensor index',IFLAG)

C Find where in the data arrays to save current sensor info.
        icurrentsenarray=0  ! clear
        do ij=1,howmany
          iwhich=mapsentoarray(ij)
          if(isen.eq.iwhich)then
            icurrentsenarray=ij  ! which part of the data arrays
          endif
        enddo
        if(icurrentsenarray.eq.0)then
          write(6,*) 'unknown sensor index ',isen,' @ ',datea
          goto 42   ! read another line of the file
        endif

        CALL EGETWI(loutstr,K,iver,4,5,'W','version',IFLAG)
        CALL EGETWI(loutstr,K,idum,0,24,'W','pir packets',IFLAG)
        CALL EGETWI(loutstr,K,iseq,0,0,'-','sequence number',IFLAG)
        if(iseq.eq.lastseq(icurrentsenarray))then
C Debug
C          write(6,*) 'Duplicate sequence ',iseq,' for ',isen,' @ ',datea
          goto 42   ! read another line of the file
        else
          lastseq(icurrentsenarray)=iseq  ! remember sequence number
        endif

C If there are more than 10 pending packets ignore and read again.
        CALL EGETWI(loutstr,K,ipacketsleft,0,0,'-','nb packet lft',IER)
        if(ipacketsleft.gt.10)then
          goto 42
        endif

C Get raw BAM dbt data or dbt degree C data.
        if(act(1:1).eq.'b'.or.act(1:1).eq.'B')then
          CALL EGETWI(loutstr,K,irdbt,0,40000,'W','raw temp',IER)
        elseif(act(1:1).eq.'s'.or.act(1:1).eq.'S')then
          CALL EGETWI(loutstr,K,irdbt,0,40000,'W','raw temp',IER)
        elseif(act(1:1).eq.'u'.or.act(1:1).eq.'U')then
          CALL EGETWR(loutstr,K,rdbt,-20.0,100.0,'W','temp deg C',IER)
        endif

C Get BAM light data or Lux.
        if(act(1:1).eq.'b'.or.act(1:1).eq.'B')then
          CALL EGETWI(loutstr,K,irlight,0,1000,'-','raw light',IER)
        elseif(act(1:1).eq.'s'.or.act(1:1).eq.'S')then
          CALL EGETWI(loutstr,K,irlight,0,1000,'-','raw light',IER)
        elseif(act(1:1).eq.'u'.or.act(1:1).eq.'U')then
          CALL EGETWR(loutstr,K,rlight,0.0,25000.0,'W','light lux',IER)
        endif

        CALL EGETWI(loutstr,K,idum,0,0,'-','battery',IER)

C Get raw BAM RH data or RH % data.
        if(act(1:1).eq.'b'.or.act(1:1).eq.'B')then
          CALL EGETWI(loutstr,K,irrh,0,40000,'W','raw humidity',IER)
        elseif(act(1:1).eq.'s'.or.act(1:1).eq.'S')then
          CALL EGETWI(loutstr,K,irrh,0,40000,'W','raw humidity',IER)
        elseif(act(1:1).eq.'u'.or.act(1:1).eq.'U')then
          CALL EGETWR(loutstr,K,rrh,0.0,100.0,'W','humidity %',IER)
        endif

C Get raw PIR data and figure out peak or get 2nd token. In this case
C act = U is also in raw PIR data.
        if(act(1:1).eq.'b'.or.act(1:1).eq.'B')then
          ipirpeak=0  ! reset peak
          do loop = 1,24
            CALL EGETWI(loutstr,K,ipir,0,0,'-','raw pir',IER)
            if(ipir.gt.ipirpeak) ipirpeak=ipir
          enddo
        elseif(act(1:1).eq.'s'.or.act(1:1).eq.'S')then
          ipirpeak=0  ! reset peak
          CALL EGETWI(loutstr,K,ipir,0,0,'-','raw pir min',IER)
          CALL EGETWI(loutstr,K,ipirpeak,0,0,'-','raw pir max',IER)
          CALL EGETWI(loutstr,K,ipir,0,0,'-','raw pir avg',IER)
        elseif(act(1:1).eq.'u'.or.act(1:1).eq.'U')then
          ipirpeak=0  ! reset peak
          CALL EGETWI(loutstr,K,ipir,0,0,'-','raw pir min',IER)
          CALL EGETWI(loutstr,K,ipirpeak,0,0,'-','raw pir max',IER)
          CALL EGETWI(loutstr,K,ipir,0,0,'-','raw pir avg',IER)
        endif

C Get raw Audio and figure out peak or get 2nd token.
        if(act(1:1).eq.'b'.or.act(1:1).eq.'B')then
          iaudiopeak=0 ! reset peak
          do loop = 1,24
            CALL EGETWI(loutstr,K,iaudio,0,0,'-','raw audio',IER)
            if(iaudio.gt.iaudiopeak) iaudiopeak=iaudio
          enddo
        elseif(act(1:1).eq.'s'.or.act(1:1).eq.'S')then
          audiopeak=0.0 ! reset peak
          CALL EGETWI(loutstr,K,iaudio,0,0,'-','raw audio min',IER)
          CALL EGETWI(loutstr,K,iaudiopeak,0,0,'-','raw audio max',IER)
          CALL EGETWI(loutstr,K,iaudio,0,0,'-','raw audio avg',IER)
        elseif(act(1:1).eq.'u'.or.act(1:1).eq.'U')then
          audiopeak=0.0 ! reset peak
          CALL EGETWR(loutstr,K,audio,0.,0.,'-','audio dba min',IER)
          CALL EGETWR(loutstr,K,audiopeak,0.,0.,'-','audio dba max',IER)
          CALL EGETWR(loutstr,K,audio,0.,0.,'-','audio dba avg',IER)
        endif

C Convert timestep to julian.
        actd='B'
        call bamdatetojulian(datea,actd,juliand,julianfrac)

C Test if we have moved on to a subsequent hour. Use a one minute
C tollerance. Only do the test for the first two sensors.
        if(icurrentsenarray.le.2)then
          do ih=0,23
            nearstartofhour=.false.
            call ECLOSE(julianfrac,hourdayfrac(ih),oneminfrac,
     &        nearstartofhour)
            if(nearstartofhour)then 
              thehouris=hourdayfrac(ih)
              write(6,*) 'csv file is NOW for hour ',ih,julianfrac,
     &          thehouris
            endif
          enddo
        endif

C Are we close to hour mark or one of the five minute marks? Use
C a 10/20 sec tollerance for version 4 and 20/30/45 sec for version 5
C depending on how many sensors are being dealt with.
        closetohour=.false.
        closetofive=.false.
        nowis=thehouris
        if(iver.eq.4)then
          if(howmany.lt.5)then
            call ECLOSE(julianfrac,thehouris,tensecfrac,closetohour)
          else
            call ECLOSE(julianfrac,thehouris,twentysecfrac,closetohour)
          endif
        elseif(iver.eq.5)then
          if(howmany.lt.5)then
            call ECLOSE(julianfrac,thehouris,twentysecfrac,closetohour)
          elseif(howmany.ge.5.and.howmany.lt.8)then
            call ECLOSE(julianfrac,thehouris,thirtysecfrac,closetohour)
          elseif(howmany.ge.8)then
            call ECLOSE(julianfrac,thehouris,fourtysecfrac,closetohour)
          endif
        endif 
        if(closetohour)then
          thejulanis=juliand    ! remenber julian
          write(6,*) 'closetohour ',closetohour,thehouris,datea,
     &      thejulianis
          goto 43
        endif

C See if it is within 10 or 20 seconds of one of the 5 minute slots.
C AND also check that we have moved on at least a minute from the last
C time that we printed out information.
        do ifive=1,12
          nowis=nowis+fiveminfrac
          if(iver.eq.4)then
            if(howmany.lt.5)then
              call ECLOSE(julianfrac,nowis,tensecfrac,closetofive)
            else
              call ECLOSE(julianfrac,nowis,twentysecfrac,closetofive)
            endif
          else
            if(howmany.lt.5)then
              call ECLOSE(julianfrac,nowis,twentysecfrac,closetofive)
            elseif(howmany.ge.5.and.howmany.lt.8)then
              call ECLOSE(julianfrac,nowis,thirtysecfrac,closetofive)
            elseif(howmany.ge.8)then
              call ECLOSE(julianfrac,nowis,fourtysecfrac,closetofive)
            endif
          endif
          if(closetofive)then
            if(juliand.gt.(printedat+oneminfrac))then
              if(icurrentsenarray.le.2)then
                write(6,*) 'closetofive ',closetofive,nowis,ifive,datea
              endif
              thejulianis=juliand   ! also remember julian
              goto 43
            else
              write(6,*) 'a bit to soon ',datea,juliand,printedat
              goto 42
            endif
          endif
        enddo

C If we are not near a desired time go and read another line.
  43    if(closetohour.or.closetofive)then
          continue
        else
          goto 42
        endif

C Convert raw BAM data to dbt, RH lux using ESRU calibration info or
C assign if -format:units.
        if(act(1:1).eq.'b'.or.act(1:1).eq.'B'.or.
     &     act(1:1).eq.'s'.or.act(1:1).eq.'S')then
          dbt= (0.01185055 * real(irdbt)) - 438.169086
          bamtmp(icurrentsenarray)=dbt
        elseif(act(1:1).eq.'u'.or.act(1:1).eq.'U')then
          bamtmp(icurrentsenarray)=rdbt
        endif

        if(act(1:1).eq.'b'.or.act(1:1).eq.'B'.or.
     &     act(1:1).eq.'s'.or.act(1:1).eq.'S')then
          if(irlight.gt.850)then
            lux = 285.0
          else
            lexponent = real(irlight) * 0.0080849;
            lux = 0.202549 * exp(lexponent)
          endif
          bamlight(icurrentsenarray)=lux
        elseif(act(1:1).eq.'u'.or.act(1:1).eq.'U')then
          bamlight(icurrentsenarray)=rlight
        endif

        if(act(1:1).eq.'b'.or.act(1:1).eq.'B'.or.
     &     act(1:1).eq.'s'.or.act(1:1).eq.'S')then
          rhl = (49.6 * log(real(irrh))) - 397.19
          bamhumidity(icurrentsenarray)=rhl
        elseif(act(1:1).eq.'u'.or.act(1:1).eq.'U')then
          bamhumidity(icurrentsenarray)=rrh
        endif

C Remember the peak PIR value.
        if(act(1:1).eq.'b'.or.act(1:1).eq.'B'.or.
     &     act(1:1).eq.'s'.or.act(1:1).eq.'S')then
          bampir(icurrentsenarray)= real(ipirpeak)
        elseif(act(1:1).eq.'u'.or.act(1:1).eq.'U')then
          bampir(icurrentsenarray)= real(ipirpeak)
        endif

C Remember and convert iaudiopeak
        if(act(1:1).eq.'b'.or.act(1:1).eq.'B'.or.
     &     act(1:1).eq.'s'.or.act(1:1).eq.'S')then
          bamaudio(icurrentsenarray)=(8.06*log(real(iaudiopeak)))+23.5
        elseif(act(1:1).eq.'u'.or.act(1:1).eq.'U')then
          bamaudio(icurrentsenarray)=audiopeak
        endif

        howmanynow=howmanynow+1  ! got another sensor instance

C Unsorted write (e.g. in the order of original log file.
C        if(act(1:1).eq.'b'.or.act(1:1).eq.'B'.or.
C     &     act(1:1).eq.'s'.or.act(1:1).eq.'S')then
C          write(IUF,*) datea,' ',juliand,' * ',isen,dbt,rhl,lux,
C     &      icurrentsenarray,howmanynow
C        elseif(act(1:1).eq.'u'.or.act(1:1).eq.'U')then
C          write(IUF,*) datea,' ',juliand,' * ',isen,rdbt,rrh,rlight,
C     &      icurrentsenarray,howmanynow
C        endif

C If all sensors likely to have been captured dump out the data
C in sorted order (timestamp the same for each). This ensures
C that the bam arrays have been filled. Note: if data for a sensor
C is not found the last known value in the bam array would be 
C written. Example, if the sensor over-temperature.

C NOTE: for some log files we need to read extra lines
C in order to ensure we have captured all of the sensors
C In others we only slightly more than howmany.
        delim='C'

        if(iver.eq.4)then
          if(howmanynow.gt.howmany*2)then
            continue
          else
            goto 42
          endif
        elseif(iver.eq.5)then
          if(howmany.lt.5.and.howmanynow.gt.howmany+2)then
            continue
          else
            if(howmanynow.gt.howmany)then
              continue
            else
              goto 42
            endif
          endif
        endif

C Increment position in tabular array for the each of the 5 items associated
C with a BAM sensor.
        ipos=1
        do ij=1,howmany
          tabular(ipos)=bamtmp(ij)
          ipos=ipos+1
          tabular(ipos)=bamlight(ij)
          ipos=ipos+1
          tabular(ipos)=bamhumidity(ij)
          ipos=ipos+1
          tabular(ipos)=bampir(ij)
          ipos=ipos+1
          tabular(ipos)=bamaudio(ij)
          ipos=ipos+1

C Debug
C          write(IUF,*) datea,' ',thejulianis,' ',
C     &      mapsentoarray(ij),bamtmp(ij),bamlight(ij),
C     &      bamhumidity(ij),bampir(ij),bamaudio(ij)
        enddo

C Write a line with the BAM time as well as julian day fraction
C to support debug.
C        write(IUF,*) datea,' ',thejulianis
        itrunc=1
        ipos=1
        do while (itrunc.ne.0)
          call arlist(ipos,howmany*5,tabular,60,delim,loutstr,loutln,
     &      itrunc)
          if(ipos.eq.1) then
            WRITE(iuf,'(F10.6,4a)',IOSTAT=ios,ERR=2) thejulianis,',',
     &        datea(1:19),',',loutstr(1:loutln)
          else
            WRITE(iuf,'(1x,a)',IOSTAT=ios,ERR=2) loutstr(1:loutln)
          endif
          ipos=itrunc+1
        end do
        printedat=thejulianis  ! remember julian for this print
        howmanynow=0  ! reset
        goto 42

      elseif(act(1:1).eq.'c'.or.act(1:1).eq.'C')then

C Column (csv version) work in progress.

C Discover how many different sensors are included. Read the first 100 lines.
        howmany=0      ! start with no sensors
        thehouris=0.0  ! clear assumption about when the csv file time starts
        nearstartofhour = .false.
        do ij=1,100
          call stripc1k(IUNIT,loutstr,0,ND,1,'log line',IER)
          K=0
          CALL EGETW(loutstr,K,WORD,'W','TEDDI token',IFLAG)
          CALL EGETW(loutstr,K,WORD,'W','ignore time number',IFLAG)
          CALL EGETP(loutstr,K,datea,'W','YYYY-MM-DD hh:mm:ss.f',IFLAG)

C Log files cover an unknown period. See which hour this is by checking
C the timing of the 2nd line of the csv file. Note: if the initial lines do
C not begin near the start of an hour use alternative logic.
C << can this logic be moved ?? >>
          if(ij.eq.2)then
            actd='B'
            call bamdatetojulian(datea,actd,juliand,julianfrac)
            do ih=0,23
              closetohour=.false.
              call ECLOSE(julianfrac,hourdayfrac(ih),oneminfrac,
     &          closetohour)
              if(closetohour)then
                thehouris=hourdayfrac(ih)  ! remember fraction
                thejulanis=juliand    ! remenber julian
                write(6,*) 'csv file is for hour ',ih,julianfrac,
     &            thehouris
                nearstartofhour=.true.
              endif
            enddo
            if(nearstartofhour)then
              continue
            else

C See if julianfrac between known hour fractions remember thehouris.
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
C Debug.
        write(6,*) 'how many sensors found ',howmany,mapsentoarray

C Sort the array (increasing) so that data can be written in a standard
C order rather than the random order found in the intial lines of the file.
        KFLAG=1
        call SORTI(mapsentoarray,mapsent,howmany,KFLAG)
        write(6,*) 'after sort ',howmany,mapsentoarray
        write(IUF,'(a)')'BAM summary '
        write(IUF,'(a,i2)')'# number of sensors ',howmany
        write(IUF,*)'# sensor list ',mapsentoarray

C Generate a header line with appropriate titles for the collected sensors
        irows=1
        K4=33   ! initial value for buffer
        loutstr = '#JulianTime,yyyy-mm-dd hh:mm:ss,'
        do loop = 1,howmany
          write(t12a,'(a,i3.3,a)') 's',mapsentoarray(loop),'_degC'
          lnt12a=lnblnk(t12a)
          write(t12b,'(a,i3.3,a)') 's',mapsentoarray(loop),'_Lux'
          lnt12b=lnblnk(t12b)
          write(t12c,'(a,i3.3,a)') 's',mapsentoarray(loop),'_RH'
          lnt12c=lnblnk(t12c)
          write(t12d,'(a,i3.3,a)') 's',mapsentoarray(loop),'_PIR'
          lnt12d=lnblnk(t12d)
          write(t12e,'(a,i3.3,a)') 's',mapsentoarray(loop),'_dbA'
          lnt12e=lnblnk(t12e)
          lntabcde = lnt12a + lnt12b + lnt12c + lnt12d + lnt12e
          KE4=K4+(lntabcde+5) ! set for 1k buffer
          write(loutstr(K4:KE4),'(10a)') t12a(1:lnt12a),',',
     &      t12b(1:lnt12b),',',t12c(1:lnt12c),',',t12d(1:lnt12d),',',
     &      t12e(1:lnt12e),','
C          write(6,*) loutstr(1:lnblnk(loutstr))
          K4=KE4
        enddo
        lnb=lnblnk(loutstr)+1   ! ensure we do not warp line
        write(IUF,'(A)',iostat=ios,err=2) loutstr(1:lnb)

C Noticed that rewind did not work so close and open instead.
C        rewind(IUNIT)   ! go back to the start of the csv file
        CALL ERPFREE(IUNIT,ISTAT)
        CALL EFOPSEQ(IUNIT,INTER,1,IER)
        if(ier.ne.0)then
          call usrmsg(' Problem re-opening...',INTER,'W')
          stop
        endif

C First test is to see if we can identify lines in the log file that match specific
C times (near hour or 5 minute mark) and print out the information. Note that in
C the compact version there might be a minutes worth of data for one sensor before
C the next sensor data is shown and the time jumps back

C We want the 2nd token on the line. Also remember the sequence number so that if
C the next read is the same then it can be skipped.
        lastseq=0
        howmanynow=0
 142    call stripc1k(IUNIT,loutstr,0,ND,1,'log line',IER)
        if(IER.ne.0)then

C Debug.
          write(6,*) 'stripc end of file ',ier
          CALL ERPFREE(IUNIT,ISTAT)
          CALL ERPFREE(IUF,ISTAT)
          stop
        endif
        K=0
        CALL EGETW(loutstr,K,WORD,'W','TEDDI token',IFLAG)
        CALL EGETW(loutstr,K,WORD,'W','ignore time number',IFLAG)
        CALL EGETP(loutstr,K,datea,'W','YYYY-MM-DD hh:mm:ss.fff',IFLAG)
        CALL EGETWI(loutstr,K,isen,0,200,'W','sensor index',IFLAG)

C Find where in the data arrays to save current sensor info.
        icurrentsenarray=0  ! clear
        do ij=1,howmany
          iwhich=mapsentoarray(ij)
          if(isen.eq.iwhich)then
            icurrentsenarray=ij  ! which part of the data arrays
          endif
        enddo
        if(icurrentsenarray.eq.0)then
          write(6,*) 'unknown sensor index ',isen,' @ ',datea
          goto 142   ! read another line of the file
        endif

        CALL EGETWI(loutstr,K,iver,4,5,'W','version',IFLAG)
        CALL EGETWI(loutstr,K,idum,0,24,'W','pir packets',IFLAG)
        CALL EGETWI(loutstr,K,iseq,0,0,'-','sequence number',IFLAG)
        if(iseq.eq.lastseq(icurrentsenarray))then
C Debug
C          write(6,*) 'Duplicate sequence ',iseq,' for ',isen,' @ ',datea
          goto 142   ! read another line of the file
        else
          lastseq(icurrentsenarray)=iseq  ! remember sequence number
        endif

C If there are more than 20 pending packets ignore and read again.
        CALL EGETWI(loutstr,K,ipacketsleft,0,0,'-','nb packet lft',IER)
        if(ipacketsleft.gt.20)then
          goto 142
        endif

C Get dbt deg C data.
        CALL EGETWR(loutstr,K,rdbt,-20.0,100.0,'W','temperature C',IER)

C Get light Lux value.
        CALL EGETWR(loutstr,K,rlight,0.0,25000.0,'W','light lux',IER)

C Ignore raw battery value.
        CALL EGETWI(loutstr,K,idum,0,0,'-','battery',IER)

C Get RH % data.
        CALL EGETWR(loutstr,K,rrh,0.0,100.0,'W','humidity %',IER)

C Get raw PIR min max average data.
        ipirpeak=0  ! reset peak
        CALL EGETWI(loutstr,K,ipir,0,0,'-','pir min',IER)
        CALL EGETWI(loutstr,K,ipirpeak,0,0,'-','pir peak',IER)
        CALL EGETWI(loutstr,K,ipir,0,0,'-','pir avg',IER)

C Get Audio min peak average dbA.
        audiopeak=0.0 ! reset peak
        CALL EGETWR(loutstr,K,audio,0.,0.,'-','audio dba min',IER)
        CALL EGETWR(loutstr,K,audiopeak,0.,0.,'-','audio dba max',IER)
        CALL EGETWR(loutstr,K,audio,0.,0.,'-','audio dba avg',IER)

C Convert timestep to julian.
        actd='B'
        call bamdatetojulian(datea,actd,juliand,julianfrac)

C Test if we have moved on to a subsequent hour. Use a one minute
C tollerance. Only do the test for the first two sensors.
        if(icurrentsenarray.le.2)then
          do ih=0,23
            nearstartofhour=.false.
            call ECLOSE(julianfrac,hourdayfrac(ih),oneminfrac,
     &        nearstartofhour)
            if(nearstartofhour)then 
              thehouris=hourdayfrac(ih)
              write(6,*) 'csv file is NOW for hour ',ih,julianfrac,
     &          thehouris
            endif
          enddo
        endif

C Are we close to hour mark or one of the five minute marks? Use
C a 10 sec tollerance for version 4 and 20 sec for version 5.
        closetohour=.false.
        closetofive=.false.
        nowis=thehouris
        if(iver.eq.4)then
          if(howmany.lt.5)then
            call ECLOSE(julianfrac,thehouris,tensecfrac,closetohour)
          else
            call ECLOSE(julianfrac,thehouris,twentysecfrac,closetohour)
          endif
        elseif(iver.eq.5)then
          if(howmany.lt.5)then
            call ECLOSE(julianfrac,thehouris,twentysecfrac,closetohour)
          elseif(howmany.ge.5.and.howmany.lt.8)then
            call ECLOSE(julianfrac,thehouris,thirtysecfrac,closetohour)
          elseif(howmany.ge.8)then
            call ECLOSE(julianfrac,thehouris,fourtysecfrac,closetohour)
          endif
        endif 
        if(closetohour)then
          thejulanis=juliand    ! remenber julian
          write(6,*) 'closetohour ',closetohour,thehouris,datea,
     &      thejulianis
          goto 143
        endif

C See if it is within 10 or 20 seconds of one of the 5 minute slots.
C AND also check that we have moved on at least a minute from the last
C time that we printed out information.
        do ifive=1,12
          nowis=nowis+fiveminfrac
          if(iver.eq.4)then
            if(howmany.lt.5)then
              call ECLOSE(julianfrac,nowis,tensecfrac,closetofive)
            else
              call ECLOSE(julianfrac,nowis,twentysecfrac,closetofive)
            endif
          else
            if(howmany.lt.5)then
              call ECLOSE(julianfrac,nowis,twentysecfrac,closetofive)
            elseif(howmany.ge.5.and.howmany.lt.8)then
              call ECLOSE(julianfrac,nowis,thirtysecfrac,closetofive)
            elseif(howmany.ge.8)then
              call ECLOSE(julianfrac,nowis,fourtysecfrac,closetofive)
            endif
          endif
          if(closetofive)then
            if(juliand.gt.(printedat+oneminfrac))then
              if(icurrentsenarray.le.2)then
                write(6,*) 'closetofive ',closetofive,nowis,ifive,datea
              endif
              thejulianis=juliand   ! also remember julian
              goto 143
            else
              write(6,*) 'a bit to soon ',datea,juliand,printedat
              goto 142
            endif
          endif
        enddo

C If we are not near a desired time go and read another line.
 143    if(closetohour.or.closetofive)then
          continue
        else
          goto 142
        endif

C Copy dbt, RH lux to array.
        bamtmp(icurrentsenarray)=rdbt

        bamlight(icurrentsenarray)=rlight

        bamhumidity(icurrentsenarray)=rrh

C Remember the peak PIR value.
        bampir(icurrentsenarray)= real(ipirpeak)

C Remember and convert audiopeak
        bamaudio(icurrentsenarray)=audiopeak

        howmanynow=howmanynow+1  ! got another sensor instance

C Unsorted write (e.g. in the order of original log file.
C        write(IUF,*) datea,' ',juliand,' * ',isen,rdbt,rrh,rlight,
C     &    icurrentsenarray,howmanynow

C If all sensors likely to have been captured dump out the data
C in sorted order (timestamp the same for each). This ensures
C that the bam arrays have been filled. Note: if data for a sensor
C is not found the last known value in the bam array would be 
C written. Example, if the sensor over-temperature.

C NOTE: for some log files we need to read extra lines
C in order to ensure we have captured all of the sensors
C In others we only slightly more than howmany.
        delim='C'

        if(iver.eq.4)then
          if(howmanynow.gt.howmany*2)then
            continue
          else
            goto 142
          endif
        elseif(iver.eq.5)then
          if(howmany.lt.5.and.howmanynow.gt.howmany+2)then
            continue
          else
            if(howmanynow.gt.howmany)then
              continue
            else
              goto 142
            endif
          endif
        endif

C Increment position in tabular array for the each of the 5 items associated
C with a BAM sensor.
        ipos=1
        do ij=1,howmany
          tabular(ipos)=bamtmp(ij)
          ipos=ipos+1
          tabular(ipos)=bamlight(ij)
          ipos=ipos+1
          tabular(ipos)=bamhumidity(ij)
          ipos=ipos+1
          tabular(ipos)=bampir(ij)
          ipos=ipos+1
          tabular(ipos)=bamaudio(ij)
          ipos=ipos+1

C Debug
C          write(IUF,*) datea,' ',thejulianis,' ',
C     &      mapsentoarray(ij),bamtmp(ij),bamlight(ij),
C     &      bamhumidity(ij),bampir(ij),bamaudio(ij)
        enddo

C Write a line with the BAM time as well as julian day fraction
C to support debug.
C        write(IUF,*) datea,' ',thejulianis
        itrunc=1
        ipos=1
        do while (itrunc.ne.0)
          call arlist(ipos,howmany*5,tabular,60,delim,loutstr,loutln,
     &      itrunc)
          if(ipos.eq.1) then
            WRITE(iuf,'(F10.6,4a)',IOSTAT=ios,ERR=2) thejulianis,',',
     &        datea(1:19),',',loutstr(1:loutln)
          else
            WRITE(iuf,'(1x,a)',IOSTAT=ios,ERR=2) loutstr(1:loutln)
          endif
          ipos=itrunc+1
        end do
        printedat=thejulianis  ! remember julian for this print
        howmanynow=0  ! reset
        goto 142

      elseif(act(1:1).eq.'-')then

C No directive given.
        write(6,*) 'done...',INTER
        CALL ERPFREE(IUNIT,ISTAT)
        CALL ERPFREE(IUF,ISTAT)
        call epwait
        call epagend
        STOP

      endif

C tabular array write errors.
  2   write(6,*) 'could not write tabular data...',thejulianis
      CALL ERPFREE(IUNIT,ISTAT)
      CALL ERPFREE(IUF,ISTAT)
      call epwait
      call epagend
      STOP
      
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
C << ?? >>

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
      integer imon,idom,ihrod,imohr,iyears,ijday
      real secofhr
      real totalsecs
      real juliand,julianfrac

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
