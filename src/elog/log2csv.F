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

C This software is designed to parse sensor data log 
C files into a csv file at a user determined frequency. 
C The output consolidates all sensors into columns, one 
C row per timestep. A typical output is shown below:

C iOpt sensor log
C number of sensors  3
C include,degC
C include,RH
C sensor list 3,1,2
C JulianTime,yyyy-mm-dd,hh:mm,1B9_degC,1B9_PPM,1B9_RH,1BB_degC,1BB_PPM,1BB_RH,1BC_degC,1BC_PPM,1BC_RH,
C 112.000000,2021-04-22 00:00,18.400,464.00,57.000,17.800,455.00,58.500,19.800,445.00,52.000
C 112.003471,2021-04-22 00:05,18.400,464.00,57.000,17.800,455.00,58.500,19.750,437.50,52.000
C 112.006943,2021-04-22 00:10,18.400,449.00,57.000,17.800,449.00,58.500,19.700,430.00,52.000
C 112.010414,2021-04-22 00:15,18.400,449.00,57.000,17.800,449.00,58.500,19.650,429.00,52.000
C 112.013885,2021-04-22 00:20,18.300,453.00,57.000,17.700,465.00,58.500,19.600,428.00,52.000a 

C Example of scanning a BuildAX LRS log into 5 minute frequency:
C ./log2csv -ts 12 -act 2 -file galway_24oct_sensors.csv

C Example but focused on a single sensor:
C ./log2csv -ts 12 -act 2 -name 420B14AC -file galway_24oct_sensors.csv

C Example of scanning an iOpt multi-sensor log file:
C ./log2csv -ts 12 -act i -file 280421.csv

C Example but focused on a single sensor:
C ./log2csv -ts 12 -act i -name 70B3D55F200001BC -file 280421.csv

C logstats  - the main controlling routine.
C partfa    - allows terminal, file and action arguments to be passed.

C << TODO: 

C **********************
C Pending tasks
C a) allow for other-than-five-minute output - partly working!
C b) support working with data at 30s or 1m+ intervals..

C ***************** logstats *********************
C log2csv.F is the main routine which scans monitoring log files to
C create a file fit for using as an import to an ESP-r temporal file or for spreadsheets. 
C Use -act to say whether it is an iOpt file (i) std (b)  BuiltAX log file or
C -act 2 for BAX2 files.

C Whatever the frequency of the input file we want to dump [5] minute data.
C The logic would need to be generalized to dump at other frequencies.

C BAX2 files look like this:
C 2014/12/22,13:41:05,4245BE7B,-45,1,66,20,2831,35.99,211,87,26526,57798,1
C 2014/12/22,13:41:06,4202E232,-50,1,126,20,2580,37.78,211,227,10646,63600,1
C 2014/12/22,13:41:06,4278C783,-44,1,6,20,2694,38.92,209,235,31469,25996,1
C 2014/12/22,13:41:10,42EAEDED,-54,1,219,20,2297,40.70,185,80,63867,22973,5
C 2014/12/22,13:41:11,4202E232,-50,1,127,20,2580,37.78,211,227,10646,63653,1
C 2014/12/22,13:41:14,42EAEDED,-55,2,220,20,2297,40.70,185,80,63869,23227,5
C 2014/12/22,13:41:15,42B4744E,-64,1,58,20,2869,35.88,221,128,18640,15605,293
C 2014/12/22,13:41:16,4202E232,-51,1,128,20,2581,37.81,211,227,10646,63701,1
C The tokens are:
C   2014/12/22  year month day
C   13:41:05    hour:minute:second
C   4245BE7B    ID of BAX2 ENV sensor always 8 character long
C   -45         RSSI strength
C   1 or 2      Packet type 1 is normal 2 is PIR event
C   66          Sequence counter (0 to 255)
C   20          ??
C   2831        Battery level in mV
C   35.99       RH in %
C   211         Temperature if divided by 10 e.g. 211 is 21.1C
C   87          Lux level
C   26526       PIR counter
C   57798       PIR value which needs to be processed to form PIR power value
C   1           Magnetic reed switch count

C iOpt data looks like:
C device,time,temperature,co2,humidity,battery,dewpoint,room
C 70B3D55F200001BB,2021-04-22 00:04:40+00,17.8,455,58.5,3,9.55,lounge
C 70B3D55F200001BC,2021-04-22 00:02:19+00,19.8,445,52,3,9.65,main bedroom
C 70B3D55F200001B9,2021-04-22 00:02:48+00,18.4,464,57,3,9.72,second bedroom
C 70B3D55F200001BC,2021-04-22 00:09:51+00,19.7,430,52,3,9.56,main bedroom
C 70B3D55F200001B9,2021-04-22 00:10:20+00,18.4,449,57,3,9.72,second bedroom
C 70B3D55F200001BC,2021-04-22 00:17:20+00,19.6,428,52,3,9.47,main bedroom
C 70B3D55F200001BB,2021-04-22 00:12:09+00,17.8,449,58.5,3,9.55,lounge
C 70B3D55F200001B9,2021-04-22 00:17:48+00,18.3,453,57,3,9.63,second bedroom
C 70B3D55F200001BB,2021-04-22 00:19:38+00,17.7,465,58.5,3,9.46,lounge
C 70B3D55F200001BC,2021-04-22 00:24:51+00,19.6,439,52.5,3,9.61,main bedroom

C For monitored data which has erattic timings an alternative logic is used
C to create time bins at a fixed frequency over the day and then decode the timestamp
C of the sensor in order to place it in one of the bins.
C 1st scan file to discover how many sensors
C 2nd ....
C 3rd ....

C Weather Underground files might be subject to a future parse and look like:
C Date,Time,Temperature,DewPoint,Humidity,Wind,Speed,Gust,Pressure,Precip.Rate.,Precip.Aum.,UV,Solar
C 23/03/2021,00:04,8.2,5,80%,202.5,0.0km/h,0.0km/h,"1,010.50hPa",0.00mm,0.00mm,0,0w/m_
C 23/03/2021,00:09,8.2,5,80%,180,0.0km/h,0.0km/h,"1,010.50hPa",0.00mm,0.00mm,0,0w/m_
C 23/03/2021,00:14,8.2,5,80%,180,0.0km/h,0.0km/h,"1,010.16hPa",0.00mm,0.00mm,0,0w/m_
C 23/03/2021,00:19,8.2,5,80%,180,0.0km/h,0.0km/h,"1,010.16hPa",0.00mm,0.00mm,0,0w/m_
C 23/03/2021,00:24,8.2,5,80%,157.5,0.0km/h,0.0km/h,"1,010.16hPa",0.00mm,0.00mm,0,0w/m_
C 23/03/2021,00:29,8.2,5,80%,157.5,0.0km/h,0.0km/h,"1,010.16hPa",0.00mm,0.00mm,0,0w/m_
C 23/03/2021,00:34,8.2,5,80%,157.5,0.0km/h,0.0km/h,"1,010.16hPa",0.00mm,0.00mm,0,0w/m_
C 23/03/2021,00:39,8.2,5,80%,180,0.0km/h,0.0km/h,"1,010.16hPa",0.00mm,0.00mm,0,0w/m_
C 23/03/2021,00:44,8.2,5,80%,180,0.0km/h,0.0km/h,"1,010.16hPa",0.00mm,0.00mm,0,0w/m_
C 23/03/2021,00:49,8.2,5.1,81%,202.5,0.0km/h,0.0km/h,"1,010.16hPa",0.00mm,0.00mm,0,0w/m_
C 23/03/2021,00:54,8.1,5,81%,202.5,0.0km/h,0.0km/h,"1,010.16hPa",0.00mm,0.00mm,0,0w/m_

C If web page table is captured and saved to Latin 1 (not 8b
C Time Temperature Dew Point Humidity Wind Speed Gust Pressure Precip. Rate. Precip. Accum. UV Solar
C 12:04 AM 9.7 °C 9.4 °C 98 % SSW 1.6 km/h 3.6 km/h 1,000.00 hPa 0.00 mm 0.00 mm 0 9.8 w/m²
C 12:09 AM 9.7 °C 9.4 °C 98 % SSW 0.3 km/h 1.4 km/h 1,000.34 hPa 0.00 mm 0.00 mm 0 9.7 w/m²
C 12:14 AM 9.6 °C 9.3 °C 98 % SW 2.5 km/h 4.0 km/h 1,000.34 hPa 0.00 mm 0.00 mm 0 9.6 w/m²


C Note: arrays sized for up to 70 sensors with
C five data values fitting within a 4k character text buffer.

C *********************************** logstats ******************************

      PROGRAM logstats
#include "building.h"
#include "espriou.h"
#include "sbem.h"

      COMMON/SPAD/MMOD,LIMIT,LIMTTY
      COMMON/OUTIN/IUOUT,IUIN,IEOUT
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

      CHARACTER INTER*144,INF*144,LFOUT2*144
      character path*72
      character outs*123,outsd*123,outs248*248,outs248d*248
      character loutstr*1000,loutstr4k*4000,loutstr4ks*4000
      character WORD*24,delim*1
      character fs*1
      character act*16 ! i iOpt file 2 BuildAX version 2 B is std log log file
      character actd*1 ! for logfile timestamp format
      character datea*24
      character dateadash*10
      integer itol     ! time tollerance for use in finding relevant data

      integer fileformat  ! 3 is LRS 4 is iOpt
      integer incdbt  ! 1 is include dry bulb T 0 is ignore
      integer incrh   ! 1 is include RH 0 is ignore
      integer inclux  ! 1 is include LUX 0 is ignore
      integer incpir  ! 1 is include PIR 0 is ignore
      integer incsw   ! 1 is include switch 0 is ignore
      character repsen*16  ! sensor name to exclude if not '-' 

      character pagestitle*42 ! for banner title via epages call
      logical unixok
      logical closetohour  ! true if we are near an hour mark

      real juliand,julianfrac  ! returned julian day and fraction of day
      real secondsinday  ! seconds per day
      real secondsinhour ! seconds per hour
      real accumulatedsec ! seconds up to the current hour
      real hourdayfrac   ! fraction of the julian day at each hour start
      dimension hourdayfrac(0:24)
      real oneminfrac    ! fraction of the julian day for 1 minute
      real curnowis      ! julian fraction at current 5m interval
      real thejulianis   ! to remember current time
      integer thedayis   ! remember current day
      integer k,nd       ! for looping
      integer listloop   ! for looping survey

C Local variables for reading raw BuildAx sensor data in the log file.
      integer isen,idum,ipacketsleft,irdbt,irlight
      real lux,rdbt,rrh       ! for RH calculation
      real thehouris          ! the csv file starts at hour
      logical nearstartofhour ! if file started near hour mark
      logical restrictreport  ! if true focus is on one sensor
      logical foundrestricsensor ! if true the restrictreport sensor found
      integer iseq    ! sequence number current and prior

      parameter (MSN=70)
      real tabular
      dimension tabular(350)  ! array for writing ts line (to hold MSN*5)

C Data for time bins dayarray(bins in day,sensors,data) where:
C data 1 = temp C, 2 = RH, 3= CO2 , 4 = Lux, 5= PIR, 6 = PIR pwr, 7 = switch
      dimension dayarray(1440,10,7)  ! time bins to hold sensor data
      real tsdayfrac          ! fraction of day for requested timestep
      logical dayslotok
      dimension dayslotok(1440,10) ! true if sensor-time-slot filled
      dimension dayfracstart(1440)   ! day fraction start at each bin
      integer ipos            ! position in tabular
      character t12a*14,t12b*13,t12c*13,t12d*13,t12e*13
      character token*18
      integer lnt12a,lnt12b,lnt12c,lnt12d,lnt12e,lntabcde


C Mapping between the sensor index and array of (up to) MSN sensors (to
C match 7k output buffer).
      integer icurrentsenarray  ! which part of the array to work with
      integer mapsentoarray,mapsent,mapsenhits
      real sensecprev,senseccur ! initial and current julian day for each sensor
      real sendeltasec          ! typical delta day fraction for each sensor
      real deltas               ! average seconds between data for each sensor
      real pirdeltas            ! seconds between standard and pir packet
      real deltapircount        ! change in PIR count for current timestep
      real deltapircountsum     ! summation of PIR counts over 5 minute period
      real deltapirpwr          ! change in PIR power
      dimension mapsentoarray(MSN),mapsent(MSN),mapsenhits(MSN)
      dimension sensecprev(MSN),senseccur(MSN),sendeltasec(MSN)
      dimension deltas(MSN),pirdeltas(MSN),deltapircount(MSN)
      dimension deltapircountsum(MSN),deltapirpwr(MSN)
      character envname*8,name*8,envsorted*8         ! BuildAX ENV has 8 character hash string
      dimension envname(MSN),envsorted(MSN)
      character ioptname*16,ioptsorted*16,iname*16  ! iOpt name is 16 character string
      dimension ioptname(MSN),ioptsorted(MSN)
      logical foundit  ! for establishing link between sensor index and mapsentoarray
      logical found
      integer howmany  ! number of sensors found
      integer ij,ik,ih,loop  ! for looping
      integer nbixsen ! if greater than zero size of ixsen array
      character ixsen*16 
      dimension ixsen(4)  ! up to 4 sensors can be excluded when scanning

C Data for each sensor.
      real pbampirc,pbampirpwr
      dimension pbampirc(MSN)
      dimension pbampirpwr(MSN)

C Seconds since start of day for last instance of each sensor.
      real sssod
      dimension sssod(MSN)

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
      foundrestricsensor=.false.
      incdbt = 1   ! assume dbt included
      incrh = 1    ! assume RH included
      inclux = 1   ! assume LUX included
      incpir = 1   ! assume PIR included
      incsw = 1    ! assume switch count included
      inco2 = 1    ! assume CO2 to be included
      repsen = '-' ! assume all LRS sensors to be reported

C Clear mapping array.
      icurrentsenarray=0; iseq=0; thejulianis=0.0
      do ij=1,MSN
        envname(ij)='UNKNOWN '
        envsorted(ij)='UNKNOWN '
        ioptname(ij)='UNKNOWN '
        ioptsorted(ij)='UNKNOWN '
        mapsentoarray(ij)=0
        mapsenhits(ij)=0
        sensecprev(ij)=0.0
        senseccur(ij)=0.0
        sendeltasec(ij)=0.0
        deltas(ij)=0.0
        pirdeltas(ij)=0.0
        deltapirpwr(ij)=0.0
        deltapircount(ij)=0.0
        deltapircountsum(ij)=0.0
        pbampirc(ij)=0.0
        pbampirpwr(ij)=0.0
        sssod(ij)=0.0
      enddo

C Setup constants.
      secondsinday = 86400.0
      secondsinhour= 3600.0

C Get command line parameters (MODL= text assumed), inf = the sensor log file,
C act = i or I (iOpt file) or 2 for LRS log, l L list only and exit.
C itol is a time-tollerance number of seconds,
C incdbt  - 1 is include dry bulb T 0 is ignore if -nodbt command
C incrh   - 1 is include RH 0 is ignore if -norh command
C inclux  - 1 is include LUX 0 is ignore if -nolux command
C incpir  - 1 is include PIR 0 is ignore if -nopir command
C incsw   - 1 is include switch 0 is ignore if -nosw command
C nbixsen nominally zero, if > greater than zero then size of ixsen array
C ixsen   - arracy of sensor ID to ignore in report
C idumpts - timesteps per hour to dump (default is 12 tsph i.e. 5 minute)
      call partfa(MODL,inf,act,itol,incdbt,incrh,
     &  inclux,incpir,incsw,repsen,nbixsen,ixsen,idumpts)

C If repsen is non-blank then restrict report to a single sensor.
      write(6,*) 'repsen ',repsen
      if(repsen(1:1).ne.'-')then
        restrictreport = .true.
      else
        restrictreport = .false.
      endif

C Set time bin values based on idumpts parameter.
      idaycount=0
      accumulateddayfrac=0.0
      if(idumpts.ge.1.and.idumpts.le.60)then
        idaycount=24*idumpts                   ! how many bins in day
        tsdayfrac=1.0/real(idaycount)          ! feaction of day in one bin
        secondsints=secondsinday * tsdayfrac   ! seconds in a bin
        write(6,*) 'ts day fraction ',tsdayfrac,idaycount,secondsints

C Clear initial values for data slots for the day.
        do ij=1,idaycount
          if(ij.eq.1)then
            dayfracstart(ij)=0.0
          else
            accumulateddayfrac = accumulateddayfrac+tsdayfrac
            dayfracstart(ij) = accumulateddayfrac
          endif
          do isen=1,10
            dayslotok(ij,isen)=.false.
            do islot=1,7
              dayarray(ij,isen,islot)=0.0
            enddo
          enddo
        enddo
      endif
     
 442  continue
C      write(6,*) 'restrictreport ',restrictreport

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
      iappw=570; iappx=80; iappy=100

C Set pixels high to iappw and pixels wide to factor in monitor size.
      iapphi=iappw
      iappwi=int(real(iappw)*(1024.0/780.0))

      menuchw = 24; LIMTTY=24; LIMIT =24

      write(pagestitle,'(a)') 'ESP-r Sensor parser '

      lntitle=lnblnk(pagestitle)
      CALL EPAGES(MMOD,IUIN,IUOUT,iappwi,iapphi,iappx,iappy,menuchw,
     &  pagestitle,lntitle)

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
        write(currentfile,'(a)') inf(1:lnblnk(inf)) ! remember for error msg
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
      do ij=1,24
        accumulatedsec=accumulatedsec+secondsinhour
        hourdayfrac(ij)=accumulatedsec/secondsinday
      enddo
      oneminfrac=60.0/secondsinday

C Test versious formats of time.
C      actd='B'   ! assume standard BuildAX log file
C      datea='2013-06-21 20:24:53.518'
C      call datestamptojulian(datea,actd,juliand,julianfrac,ijday,tsec)
C      write(6,*) datea,' ',juliand,' ',julianfrac,' ',ijday,tsec

C      datea='2013-06-21 20:24:59.446'
C      call datestamptojulian(datea,actd,juliand,julianfrac,ijday,tsec)
C      write(6,*) datea,' ',juliand,' ',julianfrac,' ',ijday,tsec

C      actd='2'   ! BuildAX 2 log file
C      datea='2014/12/22,13:41:05'
C      call datestamptojulian(datea,actd,juliand,julianfrac,ijday,tsec1)
C      write(6,*) datea,' ',juliand,' ',julianfrac,' ',ijday,tsec1
C      datea='2014/12/22,13:52:24'
C      call datestamptojulian(datea,actd,juliand,julianfrac,ijday,tsec2)
C      write(6,*) datea,' ',juliand,' ',julianfrac,' ',ijday,tsec2
C      write(6,*) 'diff in seconds ',tsec2-tsec1

C iOpt file sample: device name is 16 char. 
C The date stamp is 2021-03-31 00:03:42+00   yyyy-mm-dd hh:mm:ss which is
C the same as the older BuildAX log format.
C device,time,temperature,co2,humidity,battery,dewpoint,room
C 70B3D55F200001B5,2021-03-31 00:03:42+00,20.4,1466,55.5,3,11.19,second bedroom
C 70B3D55F200001BA,2021-03-31 00:04:22+00,19.1,896,53.5,3,9.43,lounge
C      actd='B'   ! assume standard BuildAX log file
C      datea='2021-03-31 00:04:22+00'
C      call datestamptojulian(datea,actd,juliand,julianfrac,ijday,tsec)
C      write(6,*) datea,' ',juliand,' ',julianfrac,' ',ijday,tsec2
C      write(6,*) 'diff in seconds ',tsec2-tsec1

C Assume csv file is on IFIL.
      IUNIT=IFIL
      IUF2=IFIL+2    ! adjusted tsph output file.
      CALL EFOPSEQ(IUNIT,INTER,1,IER)
      if(ier.ne.0)then
        call usrmsg(' Problem opening...',INTER,'W')
        stop
      endif

C Make up an output file with a .text ending matching the input file name.
C If report restricted to one sensor adapt name to include sensor index.
C If just scanning do not bother creating an output file.
      if(act(1:1).eq.'l'.or.act(1:1).eq.'L')then
        listloop = 80000
      else
        listloop = 80000
        lnb=lnblnk(INTER)
        lnd=lnb-3
        lnr=lnb-4
        if(INTER(lnd:lnb).eq.'.csv')then
          if(restrictreport)then
            if(repsen(1:1).ne.'-')then
              WRITE(LFOUT2,'(4a)')INTER(1:lnr),'-',
     &          repsen(1:lnblnk(repsen)),'.text'
            endif
          else
            WRITE(LFOUT2,'(2a)') INTER(1:lnd),'text'
          endif
        else
          if(restrictreport)then
            if(repsen(1:1).ne.'-')then
              WRITE(LFOUT2,'(4a)')INTER(1:lnr),'-',
     &          repsen(1:lnblnk(repsen)),'.text'
            endif
          else
            WRITE(LFOUT2,'(2a)') INTER(1:lnb),'text'
          endif
        endif

C Debug.
        write(6,*) 'reporting to > ',LFOUT2(1:lnblnk(LFOUT2))
        CALL EFOPSEQ(IUF2,LFOUT2,4,IER)
        if(ier.eq.0)then
          continue
        else
          call usrmsg(' Problem opening...',LFOUT2,'W')
          stop
        endif
      endif
      write(6,*) 'processing...',INTER

C Call the survey subroutines, the main controlling routine with the 
C filename argument.  I for iOpt csv files or a '2' for BuildAX2.
C L scans and lists but must guess the type via the first token in the line
      if(act(1:1).eq.'i'.or.act(1:1).eq.'I'.or.
     &   act(1:1).eq.'-'.or.act(1:1).eq.'l'.or.
     &   act(1:1).eq.'L'.or.act(1:1).eq.'2')then

C Discover how many different sensors are included. Read the first listloop lines.
C Note: if sensors have been turned on in sequence it may be necessary to
C cut out the intial portion of the file when all sensors are not yet active.
        howmany=0      ! start with no sensors
        thehouris=0.0  ! clear assumption about when the csv file time starts
        nearstartofhour = .false.
        do ij=1,listloop
          call stripc1k(IUNIT,loutstr,0,ND,1,'log line',IER)
C Debug.
C          write(6,'(i5,2a)') ij,' ',loutstr(1:lnblnk(loutstr))
          if(IER.ne.0)then

C Reached EOF (IER returned as non-zero).
C Debug
            write(6,*) 'Rewinding the file after scan for sensors.'
            if(fileformat.eq.3)then
C              write(6,'(a,70i5)') 'list: ',mapsentoarray
            else
C              write(6,'(a,70i5)') 'list: ',mapsentoarray
            endif
            write(6,*) 'Sensor number-of-hits typical-interval (sec)'
            do ik=1,MSN
              if(mapsenhits(ik).gt.1)then

C Find the number of seconds between initial notice of a sensor and
C the last time it was noticed.
                sendeltasec(ik)=senseccur(ik)-sensecprev(ik)
                deltas(ik)=sendeltasec(ik)*86400/mapsenhits(ik)
                if(fileformat.eq.3)then
                  write(6,*) envname(ik),sendeltasec(ik),
     &              mapsenhits(ik),deltas(ik)
C                  write(6,*) envname(ik),mapsenhits(ik),deltas(ik)
                else
                  write(6,*) ioptname(ik),sendeltasec(ik),
     &              mapsenhits(ik),deltas(ik)
C                  write(6,*) mapsentoarray(ik),sendeltasec(ik),
C     &              mapsenhits(ik),deltas(ik)
C                  write(6,*) mapsentoarray(ik),mapsenhits(ik),deltas(ik)
                endif
              endif
            enddo
            ier=0
            goto 44   ! this was a short log file so jump
          endif
          K=0

C If the line begins with 2013... or 2020 or 2021 then it
C likely is a BuildAX ENV sensor.
          CALL EGETW(loutstr,K,WORD,'W','First token',IFLAG)
          if(WORD(1:3).eq.'201'.or.WORD(1:3).eq.'202')then
            if(ij.eq.1)then

C From the first line determine the format and determine closest hour.
              fileformat=3
              if(act(1:1).eq.'-') act='2'
              actd='2'
              write(datea,'(a)') loutstr(1:20)
              call datestamptojulian(datea,actd,juliand,julianfrac,
     &          ijday,tsec)
C Debug.
C              write(6,*) datea,' ',juliand,' ',julianfrac,' ',ijday
              thejulianis = juliand    ! in case no match at start of file kludge

C The initial day of the BuildAX data is ijday, remember this in thedayis
C Save the initial 10 characters i.e. '2021-04-22'
              thedayis = ijday
              write(dateadash,'(a)') datea(1:10)
              write(6,*)'line2 ',closetohour,thehouris,datea,
     &          thejulianis,dateadash

              do ih=0,23
                closetohour=.false.
                call ECLOSE(julianfrac,hourdayfrac(ih),oneminfrac,
     &            closetohour)
                if(closetohour)then
                  thehouris=hourdayfrac(ih)  ! remember fraction
                  thejulianis=juliand    ! remenber julian
C                  write(6,*) 'csv file is @ hour ',ih,julianfrac,
C     &              thehouris
                  nearstartofhour=.true.
                endif
              enddo
              if(nearstartofhour)then
                continue
              else

C See if julianfrac between known hour fractions remember thehouris.
                do ih=0,23
                  if(julianfrac.gt.hourdayfrac(ih).and.
     &               julianfrac.lt.hourdayfrac(ih+1))then
                    thehouris=hourdayfrac(ih)
C                    write(6,*) 'csv file is for hour ',ih,thehouris
                  endif
                enddo
              endif
            endif

C Re-establish time for this sensor.
            actd='2'
            write(datea,'(a)') loutstr(1:20)
            call datestamptojulian(datea,actd,juliand,julianfrac,
     &        ijday,tsec)

C To place the data in the relevant time bin compare julianfrac with the
C start of the bin dayfracstart().  Keep track of when we flip to a new
C day so the data collected can be dumped and the arrays cleared.
            found=.false.
            do loop=1,idaycount
              if(loop.eq.1)then
                if(julianfrac.lt.dayfracstart(2))then
                  found=.true.
                  itslot=1
                  exit
                endif
              else
                if(julianfrac.ge.dayfracstart(loop-1).and.
     &             julianfrac.lt.dayfracstart(loop))then
                  found=.true.
                  itslot=loop
                  exit
                endif
              endif
            enddo

C Scan other attributes of the ENV sensor.
            K=20   ! set scan position
            CALL EGETW(loutstr,K,WORD,'W','ENV name',IFLAG)
            write(name,'(a)') WORD(1:8)
            CALL EGETWI(loutstr,K,iradio,-200,0,'W','env radio',IFLAG)
            CALL EGETWI(loutstr,K,ipacket,0,2,'W','env packet',IFLAG)
            foundit=.false.

C Loop through all possible sensor names to see if already mentioned.
C If so set foundit to TRUE.
            do ik=1,MSN
              if(envname(ik)(1:8).eq.name(1:8))then
                if(ipacket.eq.1)then  ! only check std packets
                  mapsenhits(ik)=mapsenhits(ik)+1 ! keep track of std hits
                  if(mapsenhits(ik).eq.1)then
                    sensecprev(ik)=juliand   ! remember the first timestep
                  endif
                  senseccur(ik)=juliand      ! remember current
                  sendeltasec(ik)=senseccur(ik)-sensecprev(ik)
C Debug
C                  write(6,*) 'delta ',name,ij,ik,sensecprev(ik),
C     &              senseccur(ik),sendeltasec(ik)
                endif
                foundit=.true.
              endif
            enddo

C Also check to see if the sensor is one to exclude from the table.
            if(nbixsen.gt.0)then
              do ik=1,nbixsen
                if(ixsen(ik)(1:8).eq.name(1:8))then
                  foundit=.true.
                  write(6,*) 'excluding ',name
                endif
              enddo
            endif

            if(foundit)then
              continue  ! skip past this one
            else

C Did not find it so set the first open slot of mapsentoarray
              write(envname(howmany+1),'(a)') name(1:8)
              mapsentoarray(howmany+1)=howmany+1
              howmany=howmany+1
              if(ipacket.eq.1)then  ! only check std packets
                mapsenhits(howmany)=mapsenhits(howmany)+1 ! keep track of hits
                if(mapsenhits(howmany).eq.1)then
                  sensecprev(howmany)=juliand ! remember the first timestep
                endif
              endif
              foundit=.true.

C Also check if restricted reporting mode if this sensor matches
C the one mentioned in the -name command directive.
              if(restrictreport)then
                if(name(1:8).eq.repsen(1:8))then
                  foundrestricsensor=.true.
                  write(6,*) 'found the named sensor ',name
                endif
              endif
            endif

          else

C Dealing with another file format. The first token might be a
C 16 char hash such as 70B3D55F200001BB followed by the time string.
            write(token,'(a)') WORD(1:18)
            if(ij.eq.1)then
              if(token(1:6).eq.'device')then
                fileformat=4
                if(act(1:1).eq.'-') act='i'
              endif
              CYCLE    ! No need for reading further data, loop to read next line.
            elseif(ij.eq.2)then
              write(iname,'(a)') token(1:16)
    
              fileformat=4
              CALL EGETP(loutstr,K,datea,'W','YYYY-MM-DD hh:mm:ss.f',
     &          IFLAG)
              actd='B'
              call datestamptojulian(datea,actd,juliand,julianfrac,
     &          ijday,tsec)
C              write(6,*) datea,' ',juliand,' ',julianfrac,' ',ijday
              thejulianis=juliand    ! in case no match at start of file kludge
              write(6,*)'line2 ',closetohour,thehouris,datea,thejulianis

C The initial day of the iOpt data is ijday, remember this in thedayis.
C Save the initial 10 characters i.e. '2021-04-22'
              thedayis = ijday
              write(dateadash,'(a)') datea(1:10)

C To place the data in the relevant time bin compare julianfrac with the
C start of the bin dayfracstart().
              found=.false.
              do loop=1,idaycount
                if(loop.eq.1)then
                  if(julianfrac.lt.dayfracstart(2))then
                    found=.true.
                    itslot=1
                    exit
                  endif
                else
                  if(julianfrac.ge.dayfracstart(loop-1).and.
     &               julianfrac.lt.dayfracstart(loop))then
                    found=.true.
                    itslot=loop
                    exit
                  endif
                endif
              enddo

              do ih=0,23
                closetohour=.false.
                call ECLOSE(julianfrac,hourdayfrac(ih),oneminfrac,
     &            closetohour)
                if(closetohour)then
                  thehouris=hourdayfrac(ih)  ! remember fraction
                  thejulianis=juliand    ! remenber julian
C                  write(6,*) 'csv file is @ hour ',ih,julianfrac,
C     &              thehouris
                  nearstartofhour=.true.
                endif
              enddo
              if(nearstartofhour)then
                continue
              else

C See if julianfrac between known hour fractions remember thehouris.
                do ih=0,23
                  if(julianfrac.gt.hourdayfrac(ih).and.
     &               julianfrac.lt.hourdayfrac(ih+1))then
                    thehouris=hourdayfrac(ih)
C                    write(6,*) 'csv file is for hour ',ih,thehouris
                  endif
                enddo
              endif
            elseif(ij.gt.2)then
              write(iname,'(a)') token(1:16)
              fileformat=4
              CALL EGETP(loutstr,K,datea,'W','YYYY-MM-DD hh:mm:ss.f',
     &          IFLAG)
              actd='B'
              call datestamptojulian(datea,actd,juliand,julianfrac,
     &          ijday,tsec)
            endif

C Loop through all possible iOpt sensor names to see if already mentioned.
C If so set foundit to TRUE.
            foundit=.false.
            do ik=1,MSN
              if(ioptname(ik)(1:16).eq.iname(1:16))then
                mapsenhits(ik)=mapsenhits(ik)+1 ! keep track of hits
                if(mapsenhits(ik).eq.1)then
                  sensecprev(ik)=juliand   ! remember the first timestep
                endif
                senseccur(ik)=juliand      ! remember current
                sendeltasec(ik)=senseccur(ik)-sensecprev(ik)
C Debug
C                  write(6,*) 'delta ',iname,ij,ik,sensecprev(ik),
C     &              senseccur(ik),sendeltasec(ik)
                foundit=.true.
                EXIT   ! no need to loop any mmore.
              endif
            enddo

C Also check to see if the sensor is one to exclude from the table.
            if(nbixsen.gt.0)then
              do ik=1,nbixsen
                if(ixsen(ik)(1:16).eq.iname(1:16))then
                  foundit=.true.
                  write(6,*) 'excluding ',iname
                endif
              enddo
            endif

            if(foundit)then
              continue  ! skip past this one
            else

C Did not find it so set the first open slot of mapsentoarray
              write(ioptname(howmany+1),'(a)') iname(1:16)
              mapsentoarray(howmany+1)=howmany+1
              howmany=howmany+1
              write(6,*) 'found the named sensor ',iname
              mapsenhits(howmany)=mapsenhits(howmany)+1 ! keep track of hits
              if(mapsenhits(howmany).eq.1)then
                sensecprev(howmany)=juliand ! remember the first timestep
              endif
              foundit=.true.

C Also check if restricted reporting mode if this sensor matches
C the one mentioned in the -name command directive.
              if(restrictreport)then
                if(iname(1:16).eq.repsen(1:16))then
                  foundrestricsensor=.true.
                  write(6,*) 'found the named sensor ',iname
                endif
              endif
            endif

C Scan other attributes of an iOpt sensor. Figure out parsing logic for:
C device,time,temperature,co2,humidity,battery,dewpoint,room
C 70B3D55F200001BB,2021-04-21 23:57:11+00,17.9,451,58.5,3,9.64,lounge
C 70B3D55F200001B9,2021-04-21 23:55:17+00,18.5,438,57,3,9.82,second bedroom
C 70B3D55F200001BC,2021-04-21 23:54:51+00,19.9,442,52,3,9.75,main bedroom
            if(ij.gt.1)then
              CALL EGETWR(loutstr,K,rdbt,0.0,100.0,'W','db T',IER)
              CALL EGETWR(loutstr,K,rco2,0.0,3000.0,'W','CO2',IER)
              CALL EGETWR(loutstr,K,rrh,0.0,100.0,'W','humidity %',IER)
            endif
          endif
        enddo    ! of ij loop

C If we did not find the requested sensor exit with a warning.
        if(restrictreport)then
          if(foundrestricsensor)then
            continue
          else
            call edisp(iuout,'ERROR: requested sensor not found')
            call epwait
            call epagend
            STOP
          endif
        endif



C If command line requested a listing report and exit.
   44   if(act(1:1).eq.'l'.or.act(1:1).eq.'L')then

C Debug.
          write(6,*) 'sensors found: ',howmany

C If listing then be more verbose in feedback.
C Debug
          if(fileformat.eq.3)then
            write(6,'(70(a,1x))') (envname(J),J=1,howmany)
         else
            write(6,'(70(a,1x))')  (ioptname(J),J=1,howmany)
          endif
C          write(6,'(a,70i6)') 'hits: ',(mapsenhits(J),J=1,howmany)
          do ik=1,MSN
            if(mapsenhits(ik).gt.1)then
              sendeltasec(ik)=senseccur(ik)-sensecprev(ik)
              deltas(ik)=sendeltasec(ik)*86400/mapsenhits(ik)
C Debug
C              if(fileformat.eq.3)then
C                write(6,*) 'sensor timings: ',envname(ik),
C     &            sendeltasec(ik),mapsenhits(ik),deltas(ik)
C              else
C                write(6,*) 'sensor timings: ',ioptname(ik),
C     &            sendeltasec(ik),
C     &            mapsenhits(ik),sendeltasec(ik)/mapsenhits(ik),
C     &            deltas(ik)
C              endif
            endif
          enddo
        else
          if(fileformat.eq.3)then
            write(6,'(a,i3)') 'how many sensors found: ',
     &        howmany
            write(6,'(70(a,1x))') (envname(J),J=1,howmany)
          else
            write(6,'(70(a,1x))') (ioptname(J),J=1,howmany)
          endif
          do ik=1,MSN
            if(mapsenhits(ik).gt.1)then
              sendeltasec(ik)=senseccur(ik)-sensecprev(ik)
              deltas(ik)=sendeltasec(ik)*86400/mapsenhits(ik)
C Debug
              if(fileformat.eq.3)then
                write(6,*) 'sensor timings: ',envname(ik),deltas(ik)
              else
                write(6,*) 'sensor timings: ',ioptname(ik),deltas(ik)
              endif
            endif
          enddo
        endif

        if(fileformat.eq.3)then

C For each of the current envname and mapsentoarray copy to
C alternative arrays which will be sorted.
          do ij=1,howmany
            envsorted(ij)=envname(ij)
            mapsent(ij)=mapsentoarray(ij)
          enddo

C Perform alphabetical sort and re-arrange mapsent to follow.
          call SORTSTRAIC(envsorted,mapsent,howmany)
          do ij=1,howmany
            mapsentoarray(ij)=mapsent(ij) ! map back
          enddo
        else
          do ij=1,howmany
            ioptsorted(ij)=ioptname(ij)
            mapsent(ij)=mapsentoarray(ij)
          enddo
          call SORTSTRAIC(ioptsorted,mapsent,howmany)
          do ij=1,howmany
            mapsentoarray(ij)=mapsent(ij) ! map back
          enddo

C Debug
C          write(6,*) 'after sort mapsentoarray',howmany,mapsentoarray
C          write(6,*) 'after sort mapsent',howmany,mapsent
        endif


C Write the header to the output file.
        if(act(1:1).eq.'i'.or.act(1:1).eq.'I')then
          write(IUF2,'(a)')'iOpt sensor log'
        elseif(act(1:1).eq.'2')then
          write(IUF2,'(a)')'BuildAX summary from LRS'
        elseif(act(1:1).eq.'l'.or.act(1:1).eq.'L')then

C If just scanning this is a point where we report and exit.
          write(6,'(a)')'summary from log data'
          write(6,'(a,i2)')'# number of sensors ',howmany
          if(fileformat.eq.3)then
            write(outs248,'(70I4)') (mapsentoarray(J),J=1,howmany)
            call SDELIM(outs248,outs248d,'C',IW)
            write(6,'(2a)') '#  sorted sensor list ',
     &        outs248d(1:lnblnk(outs248d))
            write(outs248,'(70(a,1x))') (envname(J),J=1,howmany)
            call SDELIM(outs248,outs248d,'C',IW)
            write(6,'(2a)') '# orig sensor list   ',
     &        outs248d(1:lnblnk(outs248d))
            write(outs248,'(70(a,1x))') (envsorted(J),J=1,howmany)
            call SDELIM(outs248,outs248d,'C',IW)
            write(6,'(2a)') '# sorted sensor list ',
     &        outs248d(1:lnblnk(outs248d))
            write(outs248,'(70I4)') (mapsent(J),J=1,howmany)
            call SDELIM(outs248,outs248d,'C',IW)
            write(6,'(2a)') '# sorted sensor list ',
     &        outs248d(1:lnblnk(outs248d))
          else
            write(outs248,'(70I4)') (mapsentoarray(J),J=1,howmany)
            call SDELIM(outs248,outs248d,'C',IW)
            write(6,'(2a)') '# sensor list ',
     &        outs248d(1:lnblnk(outs248d))
          endif
C          write(6,*) 'sensor name and number of instances. '
C          do ik=1,howmany
C            if(fileformat.eq.3)then
C              write(6,*) ' ',envname(ik),mapsenhits(ik)
C            else
C              write(6,*) ' ',ioptname(ik),mapsentoarray(ik),
C     &            mapsenhits(ik)
C            endif
C          enddo  ! of ik

          call edisp(iuout,'Survey of sensors completed.')
          call epwait
          call epagend
          STOP
        endif

        if(.NOT.restrictreport)then
          write(IUF2,'(a,i2)')'number of sensors ',howmany
          if(incdbt.eq.0)then
            write(IUF2,'(a)')'ignore,degC'
          else
            write(IUF2,'(a)')'include,degC'
          endif
          if(fileformat.eq.3)then
            if(inclux.eq.0)then
              write(IUF2,'(a)')'ignore,Lux'
            else
              write(IUF2,'(a)')'include,Lux'
            endif
          endif
          if(incrh.eq.0)then
            write(IUF2,'(a)')'ignore,RH'
          else
            write(IUF2,'(a)')'include,RH'
          endif
          if(fileformat.eq.3)then
            if(incpir.eq.0)then
              write(IUF2,'(a)')'ignore,PIR'
            else
              write(IUF2,'(a)')'include,PIR'
            endif
            if(incsw.eq.0)then
              write(IUF2,'(a)')'ignore,SW'
            else
              write(IUF2,'(a)')'include,SW'
            endif
          endif
          if(fileformat.eq.3)then
            write(outs248,'(70(a,1x))') (envsorted(J),J=1,howmany)
          else
            write(outs248,'(70I4)') (mapsentoarray(J),J=1,howmany)
            write(6,'(70(a,1x))') (ioptsorted(J),J=1,howmany)
          endif
          call SDELIM(outs248,outs248d,'C',IW)
          write(IUF2,'(2a)') 'sensor list ',
     &      outs248d(1:lnblnk(outs248d))
        else
          write(IUF2,'(a)')'number of sensors  1'
          if(incdbt.eq.0)then
            write(IUF2,'(a)')'ignore,degC'
          else
            write(IUF2,'(a)')'include,degC'
          endif
          if(fileformat.eq.3)then
            if(inclux.eq.0)then
              write(IUF2,'(a)')'ignore,Lux'
            else
              write(IUF2,'(a)')'include,Lux'
            endif
          endif
          if(incrh.eq.0)then
            write(IUF2,'(a)')'ignore,RH'
          else
            write(IUF2,'(a)')'include,RH'
          endif
          if(fileformat.eq.3)then
            if(incpir.eq.0)then
              write(IUF2,'(a)')'ignore,PIR'
            else
              write(IUF2,'(a)')'include,PIR'
            endif
            if(incsw.eq.0)then
              write(IUF2,'(a)')'ignore,SW'
            else
              write(IUF2,'(a)')'include,SW'
            endif
          endif
          write(IUF2,'(2a)')'sensor list ',repsen
        endif

C Generate a header line with appropriate titles for the collected sensors.
C For EVN sensors use the sorted array of names. Otherwise the array
C mapsentoarry has already been sorted.
        K4=30   ! initial value for buffer
        loutstr4k = 'JulianTime,yyyy-mm-dd,hh:mm,'
        do loop = 1,howmany

C Check for restricted reporting.
          if(.NOT.restrictreport)then
            continue
          else
            if(envsorted(loop)(1:8).eq.repsen(1:8))then
              continue
            else
              cycle
            endif
          endif
          if(fileformat.eq.3)then

C If temperature included set, otherwise blank the string.
            if(incdbt.eq.1)then
              write(t12a,'(a,a)') envsorted(loop),'_degC,'
              lnt12a=lnblnk(t12a)
            else
              t12a = ' '; lnt12a=1
            endif

            if(inclux.eq.1)then
              write(t12b,'(a,a)') envsorted(loop),'_Lux,'
              lnt12b=lnblnk(t12b)
            else
              t12b = ' '; lnt12b=1
            endif

C If RH included set, otherwise blank the string.
            if(incrh.eq.1)then
              write(t12c,'(a,a)') envsorted(loop),'_RH,'
              lnt12c=lnblnk(t12c)
            else
              t12c = ' '; lnt12c=1
            endif

C If PIR included set, otherwise blank the string.
            if(incpir.eq.1)then
              write(t12d,'(a,a)') envsorted(loop),'_PIR,'
              lnt12d=lnblnk(t12d)
            else
              t12d = ' '; lnt12d=1
            endif

C If switch state included set, otherwise blank the string.
            if(incsw.eq.1)then
              write(t12e,'(a,a)') envsorted(loop),'_SW,'
              lnt12e=lnblnk(t12e)
            else
              t12e = ' '; lnt12e=1
            endif
            lntabcde = lnt12a + lnt12b + lnt12c + lnt12d + lnt12e
            KE4=K4+(lntabcde) ! set for 4k buffer
            write(loutstr4k(K4:KE4),'(5a)') t12a(1:lnt12a),
     &        t12b(1:lnt12b),t12c(1:lnt12c),t12d(1:lnt12d),
     &        t12e(1:lnt12e)
C            write(6,*) loutstr4k(1:lnblnk(loutstr))
            K4=KE4
          else

C If temperature included set, otherwise blank the string.
            if(incdbt.eq.1)then
              write(t12a,'(a,a)') ioptsorted(loop)(10:16),'_degC,'
              lnt12a=lnblnk(t12a)
            else
              t12a = ' '; lnt12a=1
            endif
            if(inco2.eq.1)then
              write(t12b,'(a,a)') ioptsorted(loop)(10:16),'_PPM,'
              lnt12b=lnblnk(t12b)
            else
              t12b = ' '; lnt12b=1
            endif

C If RH included set, otherwise blank the string.
            if(incrh.eq.1)then
              write(t12c,'(a,a)') ioptsorted(loop)(8:16),'_RH,'
              lnt12c=lnblnk(t12c)
            else
              t12c = ' '; lnt12c=1
            endif

            lntabcde = lnt12a + lnt12b + lnt12c
            KE4=K4+(lntabcde+3) ! set for 4k buffer
             write(loutstr4k(K4:KE4),'(3a)') t12a(1:lnt12a),
     &        t12b(1:lnt12b),t12c(1:lnt12c)
C            write(6,*) loutstr4k(1:lnblnk(loutstr))
            K4=KE4
          endif
        enddo
        lnb=lnblnk(loutstr4k)+1   ! ensure we do not warp line

C The above logic results in a single space for each removed heading item.
C Remove these spaces from the headings via local subroutine stblanks.
        call stblanks(loutstr4k,loutstr4ks)
        loutlns=lnblnk(loutstr4ks)

C Debug.
C          write(6,*) loutstr4k(1:lnblnk(loutstr4k))
C          write(6,*) loutstr4ks(1:lnblnk(loutstr4ks))
        write(IUF2,'(A)',iostat=ios,err=2) loutstr4ks(1:loutlns)
C        lnb=lnblnk(loutstr4k)+1   ! ensure we do not warp line
C        write(IUF2,'(A)',iostat=ios,err=2) loutstr4k(1:lnb)


C --------------- Rewind file via close and open.
        CALL ERPFREE(IUNIT,ISTAT)
        CALL EFOPSEQ(IUNIT,INTER,1,IER)
        if(ier.ne.0)then
          call usrmsg(' Problem re-opening...',INTER,'W')
          stop
        endif



C Test is to see if we can identify lines in the log file that match specific
C times (near hour or 5 minute mark) and print out the information.

C For LRS focus on the 3rd token on the line. Also remember the sequence number so that if
C the next read is the same then it can be skipped.
        if(fileformat.eq.3)then
          continue
        else          ! For iOpt read pat the intial line of the file.
          call stripc1k(IUNIT,loutstr,0,ND,1,'log line',IER)
        endif

        delim='C'     ! Set comma as delimeter in output.

C The main scan the log for data loop.
 42     call stripc1k(IUNIT,loutstr,0,ND,1,'log line',IER)

C Debug.
        write(6,'(a)') loutstr(1:lnblnk(loutstr))
        if(IER.ne.0)then

C Found EOF, write the last days data and then close the output files.
          write(6,*) 'at end of file ',ier
          CALL ERPFREE(IUNIT,ISTAT)
          if(fileformat.eq.3)then
            do ijj=1,idaycount
              ipos=1
              do 78 ik=1,howmany

C Check for restricted reporting.
                if(.NOT.restrictreport)then
                  continue
                else
                  if(envsorted(ik)(1:8).eq.repsen(1:8))then
                    continue
                  else
                    goto 78
                  endif
                endif
                if(dayslotok(ijj,ik))then
                  tabular(ipos)=dayarray(ijj,ik,1)  ! dbT
                  ipos=ipos+1
                  tabular(ipos)=dayarray(ijj,ik,4)  ! lux
                  ipos=ipos+1
                  tabular(ipos)=dayarray(ijj,ik,2)  ! RH
                  ipos=ipos+1
                  tabular(ipos)=dayarray(ijj,ik,5)  ! PIR
                  ipos=ipos+1
                  tabular(ipos)=dayarray(ijj,ik,7)  ! switch
                  ipos=ipos+1
                else

C Not data. If there is in preceeding and subsequent timesteps use
C avarage value.
                  if(dayslotok(ijj-1,ik).and.dayslotok(ijj+1,ik))then
                    v=(dayarray(ijj-1,ik,1)+dayarray(ijj+1,ik,1))
                    tabular(ipos)=v*0.5
                    ipos=ipos+1
                    v=(dayarray(ijj-1,ik,4)+dayarray(ijj+1,ik,4))
                    tabular(ipos)=v*0.5
                    ipos=ipos+1
                    v=(dayarray(ijj-1,ik,2)+dayarray(ijj+1,ik,2))
                    tabular(ipos)=v*0.5
                    ipos=ipos+1
                    v=(dayarray(ijj-1,ik,5)+dayarray(ijj+1,ik,5))
                    tabular(ipos)=v*0.5
                    ipos=ipos+1
                    v=(dayarray(ijj-1,ik,7)+dayarray(ijj+1,ik,7))
                    tabular(ipos)=v*0.5
                    ipos=ipos+1
                  elseif(dayslotok(ijj-1,ik))then
                    tabular(ipos)=dayarray(ijj-1,ik,1)  ! dbT
                    ipos=ipos+1
                    tabular(ipos)=dayarray(ijj-1,ik,4)  ! lux
                    ipos=ipos+1
                    tabular(ipos)=dayarray(ijj-1,ik,2)  ! RH
                    ipos=ipos+1
                    tabular(ipos)=dayarray(ijj-1,ik,5)  ! PIR
                    ipos=ipos+1
                    tabular(ipos)=dayarray(ijj-1,ik,7)  ! switch
                    ipos=ipos+1
                  elseif(dayslotok(ijj+1,ik))then
                    tabular(ipos)=dayarray(ijj+1,ik,1)  ! dbT
                    ipos=ipos+1
                    tabular(ipos)=dayarray(ijj+1,ik,4)  ! lux
                    ipos=ipos+1
                    tabular(ipos)=dayarray(ijj+1,ik,2)  ! RH
                    ipos=ipos+1
                    tabular(ipos)=dayarray(ijj+1,ik,5)  ! PIR
                    ipos=ipos+1
                    tabular(ipos)=dayarray(ijj+1,ik,7)  ! switch
                    ipos=ipos+1
                  else
                    tabular(ipos)=0.0
                    ipos=ipos+1
                    tabular(ipos)=0.0
                    ipos=ipos+1
                    tabular(ipos)=0.0
                    ipos=ipos+1
                    tabular(ipos)=0.0
                    ipos=ipos+1
                    tabular(ipos)=0.0
                    ipos=ipos+1
                  endif
                endif
  78          continue
              if(.NOT.restrictreport)then
                itotcolumns=howmany*5
              else
                itotcolumns=5
              endif
C              write(6,*) 'tabular ',ipos,(tabular(k),k=1,itotcolumns)
              if(incsw.eq.0) itotcolumns=itotcolumns-1
              if(incpir.eq.0) itotcolumns=itotcolumns-1
              if(incrh.eq.0) itotcolumns=itotcolumns-1
              if(incdbt.eq.0) itotcolumns=itotcolumns-1
              if(inclux.eq.0) itotcolumns=itotcolumns-1
              ipos=1   ! Reset tabular array position to start from.
              itrunc=1
              do while (itrunc.ne.0)
                call arlist(ipos,itotcolumns,tabular,350,delim,
     &            loutstr4k,loutln,itrunc)
                curnowis=real(thedayis)+dayfracstart(ijj)
                time=dayfracstart(ijj)*24.0
                IHR=IFAX(time)
                REM=(time-IHR)*60.0
                IMIN=INT(REM)
                WRITE(iuf2,'(F10.6,3a,i2.2,a,i2.2,2a)',IOSTAT=ios,ERR=2) 
     &            curnowis,',',dateadash(1:10),' ',ihr,':',imin,',',
     &            loutstr4k(1:loutln)
              enddo
            enddo

          elseif(fileformat.eq.4)then   ! iOpt log file
            do ijj=1,idaycount
              ipos=1
              do 79 ik=1,howmany

C Check for restricted reporting.
                if(.NOT.restrictreport)then
                  continue
                else
                  if(ioptsorted(ik)(1:16).eq.repsen(1:16))then
                    continue
                  else
                    goto 79
                  endif
                endif
                if(dayslotok(ijj,ik))then
                  tabular(ipos)=dayarray(ijj,ik,1)
                  ipos=ipos+1
                  tabular(ipos)=dayarray(ijj,ik,3)
                  ipos=ipos+1
                  tabular(ipos)=dayarray(ijj,ik,2)
                  ipos=ipos+1
                else

C Not data. If there is in preceeding and subsequent timesteps use
C avarage value.
                  if(dayslotok(ijj-1,ik).and.dayslotok(ijj+1,ik))then
                    v=(dayarray(ijj-1,ik,1)+dayarray(ijj+1,ik,1))
                    tabular(ipos)=v*0.5
                    ipos=ipos+1
                    v=(dayarray(ijj-1,ik,3)+dayarray(ijj+1,ik,3))
                    tabular(ipos)=v*0.5
                    ipos=ipos+1
                    v=(dayarray(ijj-1,ik,2)+dayarray(ijj+1,ik,2))
                    tabular(ipos)=v*0.5
                    ipos=ipos+1
                  elseif(dayslotok(ijj-1,ik))then
                    tabular(ipos)=dayarray(ijj-1,ik,1)
                    ipos=ipos+1
                    tabular(ipos)=dayarray(ijj-1,ik,3)
                    ipos=ipos+1
                    tabular(ipos)=dayarray(ijj-1,ik,2)
                    ipos=ipos+1
                  elseif(dayslotok(ijj+1,ik))then
                    tabular(ipos)=dayarray(ijj+1,ik,1)
                    ipos=ipos+1
                    tabular(ipos)=dayarray(ijj+1,ik,3)
                    ipos=ipos+1
                    tabular(ipos)=dayarray(ijj+1,ik,2)
                    ipos=ipos+1
                  else
                    tabular(ipos)=0.0
                    ipos=ipos+1
                    tabular(ipos)=0.0
                    ipos=ipos+1
                    tabular(ipos)=0.0
                    ipos=ipos+1
                  endif
                endif
 79           continue
              if(.NOT.restrictreport)then
                itotcolumns=howmany*3
              else
                itotcolumns=3
              endif
C              write(6,*) 'tabular ',ipos,(tabular(k),k=1,itotcolumns)
              if(incsw.eq.0) itotcolumns=itotcolumns-1
              if(incpir.eq.0) itotcolumns=itotcolumns-1
              if(incrh.eq.0) itotcolumns=itotcolumns-1
              if(incdbt.eq.0) itotcolumns=itotcolumns-1
              if(inclux.eq.0) itotcolumns=itotcolumns-1
              ipos=1   ! Reset tabular array position to start from.
              itrunc=1
              do while (itrunc.ne.0)
                call arlist(ipos,itotcolumns,tabular,350,delim,
     &            loutstr4k,loutln,itrunc)
                curnowis=real(thedayis)+dayfracstart(ijj)
                time=dayfracstart(ijj)*24.0
                IHR=IFAX(time)
                REM=(time-IHR)*60.0
                IMIN=INT(REM)
                WRITE(iuf2,'(F10.6,3a,i2.2,a,i2.2,2a)',IOSTAT=ios,ERR=2) 
     &            curnowis,',',dateadash(1:10),' ',ihr,':',imin,',',
     &            loutstr4k(1:loutln)
              end do
            enddo
          endif
          WRITE(iuf2,'(a)',IOSTAT=ios,ERR=2) '# end'
          CALL ERPFREE(IUF2,ISTAT)
          stop
        endif

C If fileformat is 3 then decode BuildAx generated csv file.
        if(fileformat.eq.3)then
          K=0
          CALL EGETW(loutstr,K,WORD,'W','First token',IFLAG)

C Remember how many seconds since the start of the day for
C use in plotting PIR power.
          write(datea,'(a)') loutstr(1:20)
          actd='2'   ! BuildAX 2 log file
          call datestamptojulian(datea,actd,juliand,julianfrac,ijday,
     &      tsec)
C          write(6,*) '-',datea,' ',juliand,' ',julianfrac,' ',ijday,
C     &      ' seconds ',tsec

C Identify the relevant time bin.
          do loop=1,idaycount
            if(loop.eq.1)then
              if(julianfrac.lt.dayfracstart(2))then
                itslot=1
                exit
              endif
            else
              if(julianfrac.ge.dayfracstart(loop-1).and.
     &           julianfrac.lt.dayfracstart(loop))then
                itslot=loop
                exit
              endif
            endif
          enddo


C If the data line datestamp has incremented write the accummulated
C days data and then clear the data array. If the data line datestamp
C has decremented then skip the BuildAx ntry.
          if(ijday.lt.thedayis) goto 42
          if(ijday.gt.thedayis)then
            write(6,*) '====== new day ========',ijday,thedayis
            do ijj=1,idaycount
              ipos=1
              do 77 ik=1,howmany

C Check for restricted reporting.
                if(.NOT.restrictreport)then
                  continue
                else
                  if(envsorted(ik)(1:8).eq.repsen(1:8))then
                    continue
                  else
                    goto 77
                  endif
                endif
                if(dayslotok(ijj,ik))then
                  tabular(ipos)=dayarray(ijj,ik,1)  ! dbT
                  ipos=ipos+1
                  tabular(ipos)=dayarray(ijj,ik,4)  ! lux
                  ipos=ipos+1
                  tabular(ipos)=dayarray(ijj,ik,2)  ! RH
                  ipos=ipos+1
                  tabular(ipos)=dayarray(ijj,ik,5)  ! PIR
                  ipos=ipos+1
                  tabular(ipos)=dayarray(ijj,ik,7)  ! switch
                  ipos=ipos+1
                else

C Not data. If there is in preceeding and subsequent timesteps use
C avarage value.
                  if(dayslotok(ijj-1,ik).and.dayslotok(ijj+1,ik))then
                    v=(dayarray(ijj-1,ik,1)+dayarray(ijj+1,ik,1))
                    tabular(ipos)=v*0.5
                    ipos=ipos+1
                    v=(dayarray(ijj-1,ik,4)+dayarray(ijj+1,ik,4))
                    tabular(ipos)=v*0.5
                    ipos=ipos+1
                    v=(dayarray(ijj-1,ik,2)+dayarray(ijj+1,ik,2))
                    tabular(ipos)=v*0.5
                    ipos=ipos+1
                    v=(dayarray(ijj-1,ik,5)+dayarray(ijj+1,ik,5))
                    tabular(ipos)=v*0.5
                    ipos=ipos+1
                    v=(dayarray(ijj-1,ik,7)+dayarray(ijj+1,ik,7))
                    tabular(ipos)=v*0.5
                    ipos=ipos+1
                  elseif(dayslotok(ijj-1,ik))then
                    tabular(ipos)=dayarray(ijj-1,ik,1)  ! dbT
                    ipos=ipos+1
                    tabular(ipos)=dayarray(ijj-1,ik,4)  ! lux
                    ipos=ipos+1
                    tabular(ipos)=dayarray(ijj-1,ik,2)  ! RH
                    ipos=ipos+1
                    tabular(ipos)=dayarray(ijj-1,ik,5)  ! PIR
                    ipos=ipos+1
                    tabular(ipos)=dayarray(ijj-1,ik,7)  ! switch
                    ipos=ipos+1
                  elseif(dayslotok(ijj+1,ik))then
                    tabular(ipos)=dayarray(ijj+1,ik,1)  ! dbT
                    ipos=ipos+1
                    tabular(ipos)=dayarray(ijj+1,ik,4)  ! lux
                    ipos=ipos+1
                    tabular(ipos)=dayarray(ijj+1,ik,2)  ! RH
                    ipos=ipos+1
                    tabular(ipos)=dayarray(ijj+1,ik,5)  ! PIR
                    ipos=ipos+1
                    tabular(ipos)=dayarray(ijj+1,ik,7)  ! switch
                    ipos=ipos+1
                  else
                    tabular(ipos)=0.0
                    ipos=ipos+1
                    tabular(ipos)=0.0
                    ipos=ipos+1
                    tabular(ipos)=0.0
                    ipos=ipos+1
                    tabular(ipos)=0.0
                    ipos=ipos+1
                    tabular(ipos)=0.0
                    ipos=ipos+1
                  endif
                endif
  77          continue
              if(.NOT.restrictreport)then
                itotcolumns=howmany*5
              else
                itotcolumns=5
              endif
C              write(6,*) 'tabular ',ipos,(tabular(k),k=1,itotcolumns)
              if(incsw.eq.0) itotcolumns=itotcolumns-1
              if(incpir.eq.0) itotcolumns=itotcolumns-1
              if(incrh.eq.0) itotcolumns=itotcolumns-1
              if(incdbt.eq.0) itotcolumns=itotcolumns-1
              if(inclux.eq.0) itotcolumns=itotcolumns-1
              ipos=1   ! Reset tabular array position to start from.
              itrunc=1
              do while (itrunc.ne.0)
                call arlist(ipos,itotcolumns,tabular,350,delim,
     &            loutstr4k,loutln,itrunc)
                curnowis=real(thedayis)+dayfracstart(ijj)
                time=dayfracstart(ijj)*24.0
                IHR=IFAX(time)
                REM=(time-IHR)*60.0
                IMIN=INT(REM)
                WRITE(iuf2,'(F10.6,3a,i2.2,a,i2.2,2a)',IOSTAT=ios,ERR=2) 
     &            curnowis,',',dateadash(1:10),' ',ihr,':',imin,',',
     &            loutstr4k(1:loutln)
              enddo
            enddo

C Clear the dayarray.
            do ijjj=1,idaycount
              do isen=1,10
                dayslotok(ijjj,isen)=.false.
                do islot=1,7
                  dayarray(ijjj,isen,islot)=0.0
                enddo
              enddo
            enddo
            thedayis = ijday   ! update thedayis
            write(dateadash,'(a)') datea(1:10)
          endif

C To place the data in the relevant time bin compare julianfrac with the
C start of the bin dayfracstart().  Keep track of when we flip to a new
C day so the data collected can be dumped and the arrays cleared.
          do loop=1,idaycount
            if(loop.eq.1)then
              if(julianfrac.lt.dayfracstart(2))then
                itslot=1
                exit
              endif
            else
              if(julianfrac.ge.dayfracstart(loop-1).and.
     &           julianfrac.lt.dayfracstart(loop))then
                itslot=loop
                exit
              endif
            endif
          enddo

C Scan other attributes of the ENV sensor.
          K=20   ! set scan position
          CALL EGETW(loutstr,K,WORD,'W','ENV name',IFLAG)
          write(name,'(a)') WORD(1:8)
          CALL EGETWI(loutstr,K,iradio,-200,0,'W','env radio',IFLAG)
          CALL EGETWI(loutstr,K,ipacket,0,2,'W','env packet',IFLAG)
          CALL EGETWI(loutstr,K,iseq,0,0,'-','sequence number',IFLAG)
          CALL EGETWI(loutstr,K,ipacketsleft,0,0,'-','unknown',IER)
          CALL EGETWI(loutstr,K,ibat,0,0,'-','battery mV',IER)
          CALL EGETWR(loutstr,K,rrh,0.0,100.0,'W','humidity %',IER)
          CALL EGETWI(loutstr,K,irdbt,-200,4000,'W','raw temp',IER)
          rdbt=real(irdbt)/10.0  ! convert to deg C
          CALL EGETWI(loutstr,K,irlight,0,10000,'-','lux level',IER)
          CALL EGETWI(loutstr,K,irpircount,0,65000,'-','PIR count',IER)
          CALL EGETWI(loutstr,K,irpvalue,0,65000,'-','PIR value',IER)
          CALL EGETWI(loutstr,K,iswcount,0,65000,'-','switch count',
     &      IER)

C Loop through all possible names in the alphabetically sorted list
C and set icurrentsenaray to the position returned by mapsent array.
          foundit=.false.
          do ik=1,howmany
            if(envsorted(ik)(1:8).eq.name(1:8))then

              icurrentsenarray=ik
              foundit=.true.
              if(ipacket.eq.1)then  ! a std packet
                sssod(icurrentsenarray)=tsec ! remember when
              else

C Type 2 packet. Remember the time to check against the prior time for this
C sensor. Use similar logic to live.js. If at start and pbampirc or pbampirpwr
C are zero then set deltapircount or deltapirpwr to one.
                pirdeltas(icurrentsenarray)=tsec-sssod(icurrentsenarray)
                if(pbampirc(icurrentsenarray).gt.0.0)then
                  deltapircount(icurrentsenarray)= 
     &              real(irpircount)-pbampirc(icurrentsenarray)
                else
                  deltapircount(icurrentsenarray)=1.0
                endif
                if(pbampirpwr(icurrentsenarray).gt.0.0)then
                  deltapirpwr(icurrentsenarray)=
     &              ((real(irpvalue)-pbampirpwr(icurrentsenarray))/
     &              pirdeltas(icurrentsenarray)) * 1.0
                else
                  deltapirpwr(icurrentsenarray)=1.0
                endif

C Increment deltapircountsum.
                deltapircountsum(icurrentsenarray)=
     &            deltapircountsum(icurrentsenarray)+
     &            deltapircount(icurrentsenarray)
C Debug
C                write(6,*) 'type 2 DS DC SDC DP ',icurrentsenarray,
C     &            pirdeltas(icurrentsenarray),
C     &            deltapircount(icurrentsenarray),
C     &            deltapircountsum(icurrentsenarray),
C     &            deltapirpwr(icurrentsenarray)

                sssod(icurrentsenarray)=tsec ! remember when
              endif
            endif
          enddo  ! of ik

C Also check to see if the sensor is one to exclude from the table.
          if(nbixsen.gt.0)then
            do ik=1,nbixsen
              if(ixsen(ik)(1:8).eq.name(1:8))then
                foundit=.false.
C                write(6,*) 'jump past -ex ',name
                goto 42
              endif
            enddo
          endif
        else

C iOpt decoding logic follows.
          K=0
          CALL EGETW(loutstr,K,WORD,'W','First token',IFLAG)
          write(token,'(a)') WORD(1:18)
          CALL EGETP(loutstr,K,datea,'W','YYYY-MM-DD hh:mm:ss.f',
     &      IFLAG)
          actd='B'
          call datestamptojulian(datea,actd,juliand,julianfrac,
     &      ijday,tsec)

C Identify the relevant time bin.
          do loop=1,idaycount
            if(loop.eq.1)then
              if(julianfrac.lt.dayfracstart(2))then
                itslot=1
                exit
              endif
            else
              if(julianfrac.ge.dayfracstart(loop-1).and.
     &           julianfrac.lt.dayfracstart(loop))then
                itslot=loop
                exit
              endif
            endif
          enddo

C If the iOpt data line datestamp has changed write the accummulated
C days data (temps,rh,co2) and then clear the data array. If the day
C decrements skip the entry.
          if(ijday.lt.thedayis) goto 42
          if(ijday.gt.thedayis)then
            write(6,*) '====== new day ========',ijday,thedayis
            do ijj=1,idaycount
              ipos=1
              do 80 ik=1,howmany

C Check for restricted reporting.
                if(.NOT.restrictreport)then
                  continue
                else
                  if(ioptsorted(ik)(1:16).eq.repsen(1:16))then
                    continue
                  else
                    goto 80
                  endif
                endif
                if(dayslotok(ijj,ik))then
                  tabular(ipos)=dayarray(ijj,ik,1)
                  ipos=ipos+1
                  tabular(ipos)=dayarray(ijj,ik,3)
                  ipos=ipos+1
                  tabular(ipos)=dayarray(ijj,ik,2)
                  ipos=ipos+1
                else

C No data. If there is in preceeding and subsequent timesteps use
C avarage value.
                  if(dayslotok(ijj-1,ik).and.dayslotok(ijj+1,ik))then
                    v=(dayarray(ijj-1,ik,1)+dayarray(ijj+1,ik,1))
                    tabular(ipos)=v*0.5
                    ipos=ipos+1
                    v=(dayarray(ijj-1,ik,3)+dayarray(ijj+1,ik,3))
                    tabular(ipos)=v*0.5
                    ipos=ipos+1
                    v=(dayarray(ijj-1,ik,2)+dayarray(ijj+1,ik,2))
                    tabular(ipos)=v*0.5
                    ipos=ipos+1
                  elseif(dayslotok(ijj-1,ik))then
                    tabular(ipos)=dayarray(ijj-1,ik,1)
                    ipos=ipos+1
                    tabular(ipos)=dayarray(ijj-1,ik,3)
                    ipos=ipos+1
                    tabular(ipos)=dayarray(ijj-1,ik,2)
                    ipos=ipos+1
                  elseif(dayslotok(ijj+1,ik))then
                    tabular(ipos)=dayarray(ijj+1,ik,1)
                    ipos=ipos+1
                    tabular(ipos)=dayarray(ijj+1,ik,3)
                    ipos=ipos+1
                    tabular(ipos)=dayarray(ijj+1,ik,2)
                    ipos=ipos+1
                  else
                    tabular(ipos)=0.0
                    ipos=ipos+1
                    tabular(ipos)=0.0
                    ipos=ipos+1
                    tabular(ipos)=0.0
                    ipos=ipos+1
                  endif
                endif
  80          continue
              if(.NOT.restrictreport)then
                itotcolumns=howmany*3
              else
                itotcolumns=3
              endif
C              write(6,*) 'tabular ',ipos,(tabular(k),k=1,itotcolumns)
              if(incsw.eq.0) itotcolumns=itotcolumns-1
              if(incpir.eq.0) itotcolumns=itotcolumns-1
              if(incrh.eq.0) itotcolumns=itotcolumns-1
              if(incdbt.eq.0) itotcolumns=itotcolumns-1
              if(inclux.eq.0) itotcolumns=itotcolumns-1
              ipos=1   ! Reset tabular array position to start from.
              itrunc=1
              do while (itrunc.ne.0)
                call arlist(ipos,itotcolumns,tabular,350,delim,
     &            loutstr4k,loutln,itrunc)
                curnowis=real(thedayis)+dayfracstart(ijj)
                time=dayfracstart(ijj)*24.0
                IHR=IFAX(time)
                REM=(time-IHR)*60.0
                IMIN=INT(REM)
                WRITE(iuf2,'(F10.6,3a,i2.2,a,i2.2,2a)',IOSTAT=ios,ERR=2) 
     &            curnowis,',',dateadash(1:10),' ',ihr,':',imin,',',
     &            loutstr4k(1:loutln)
                ipos=itrunc+1
              end do
            enddo
            do ijjj=1,idaycount
              do isen=1,10   ! Clear dayarray.
                dayslotok(ijjj,isen)=.false.
                do islot=1,7
                  dayarray(ijjj,isen,islot)=0.0
                enddo
              enddo
            enddo
            thedayis = ijday  ! update theayis
            write(dateadash,'(a)') datea(1:10)
          endif

C During the same day so collect iOpt data.
          CALL EGETWR(loutstr,K,rdbt,0.0,100.0,'W','db T',IER)
          CALL EGETWR(loutstr,K,rco2,0.0,3000.0,'W','CO2',IER)
          CALL EGETWR(loutstr,K,rrh,0.0,100.0,'W','humidity %',IER)

          foundit=.false.
          do ik=1,howmany
            if(ioptsorted(ik)(1:16).eq.token(1:16))then
              icurrentsenarray=ik
              foundit=.true.
              sssod(icurrentsenarray)=tsec ! remember when
            endif
          enddo  ! of ik

C Also check to see if the sensor is one to exclude from the table.
          if(nbixsen.gt.0)then
            do ik=1,nbixsen
              if(ixsen(ik)(1:16).eq.token(1:16))then
                foundit=.false.
C                write(6,*) 'jump past -ex ',name
                goto 42
              endif
            enddo
         endif
        endif


C Map sensors values into the dayarray(itslot,icurrentsenarray,1)
        dayarray(itslot,icurrentsenarray,1) = rdbt
        dayslotok(itslot,icurrentsenarray)  = .true.

C Convert light or CO2 raw value.
        if(act(1:1).eq.'i'.or.act(1:1).eq.'I')then
          dayarray(itslot,icurrentsenarray,3) = rco2
        elseif(act(1:1).eq.'2')then
          dayarray(itslot,icurrentsenarray,4) = real(irlight)
        endif

        dayarray(itslot,icurrentsenarray,2) = rrh

C Remember the peak PIR value or the PIR count and PIR power.
        if(act(1:1).eq.'i'.or.act(1:1).eq.'I')then
          dayarray(itslot,icurrentsenarray,5) = 0.0
          dayarray(itslot,icurrentsenarray,6) = 0.0
        elseif(act(1:1).eq.'2')then
          pbampirc(icurrentsenarray)= real(irpircount)
          pbampirpwr(icurrentsenarray)= real(irpvalue)
          dayarray(itslot,icurrentsenarray,5) = 
     &      deltapircountsum(icurrentsenarray)
          dayarray(itslot,icurrentsenarray,6) = real(irpvalue)
        endif

C For LRS remember the switch count.
        if(act(1:1).eq.'i'.or.act(1:1).eq.'I')then
          dayarray(itslot,icurrentsenarray,7) = 0.0
        elseif(act(1:1).eq.'2')then
          dayarray(itslot,icurrentsenarray,7) = iswcount
        endif

C Convert timestep to julian.
C And for the current sensor remember number of seconds into the day
C in case we need the time difference to calculate PIR power when
C a type 2 packet arrives.
        if(fileformat.eq.3)then
          actd='2'   ! BuildAX 2 log file
          call datestamptojulian(datea,actd,juliand,julianfrac,ijday,
     &      tsec)
        else
          actd='B'
          call datestamptojulian(datea,actd,juliand,julianfrac,ijday,
     &      tsec)
        endif
C        write(6,*) datea,' ',juliand,' ',julianfrac,' ',ijday


        if(fileformat.eq.3)then

C For each of the current sensors reset the PIR counts.
          do ij=1,howmany
            if(fileformat.eq.3)then
              deltapircountsum(ij)=0  ! reset
            endif
          enddo
        endif   ! only do buildax.  
        goto 42

      elseif(act(1:1).eq.'-')then

C No directive given.
        write(6,*) 'no direction provided in command line.',INTER
        CALL ERPFREE(IUNIT,ISTAT)
        CALL ERPFREE(IUF2,ISTAT)
        call epwait
        call epagend
        STOP

      endif

C tabular array write errors.
  2   write(6,*) 'could not write tabular data...',thejulianis
      CALL ERPFREE(IUNIT,ISTAT)
      CALL ERPFREE(IUF2,ISTAT)
      call epwait
      call epagend
      STOP
      
      END



C Parse command lines.

C *********
C Logic needed to scan standard BuildAx log files...
C Log file is for one hour. It tends to start with 2013-01-30 00:00:04.402
C and ends with 2013-01-30 00:59:59.324. Older (v4 files) will send data-as
C collected, (v5 files) will send 30 seconds worth in a burst.

C To collect at user defined intervals we should have an idea of 'when'
C we are looking for next and using datestamptojulian to get timestamp.
C Also we need arrays to hold values for up to a dozen sensors.

C Logic for writing an ASCII temporal file (header and/or contents)...
C << ?? >>

C ********* partfa (derived from partf with an action parameter)
C partfa allows terminal, file and action arguments
C  to be passed to the program from the invocation line.  
C  Logic allows for the -file token to be optional.
C  itol is tollerance in seconds 20 30 45 55
C  incdbt - 1 is include dry bulb T 0 is ignore if -nodbt command
C  incrh  - 1 is include RH 0 is ignore if -norh command
C  inclux - 1 is include LUX 0 is ignore if -nolux command
C  incpir - 1 is include PIR 0 is ignore if -nopir command
C  incsw  - 1 is include switch 0 is ignore if -nosw command
C  inco2  - 1 is include CO2  0 is ignore if -noco2 command
C  nbixsen nominally zero, if > greater than zero then size of ixsen array
C  ixsen  - arracy of sensor ID to ignore in report
C  idumpts - timestep to dump data

C This is derived from partfa in startup.F which has
C fewer dependencies than the standard version.
      subroutine partfa(termtype,inf,act,itol,incdbt,incrh,
     &  inclux,incpir,incsw,repsen,nbixsen,ixsen,idumpts)
      
      integer lnblnk  ! function definition

      COMMON/OUTIN/IUOUT,IUIN,IEOUT
      integer termtype
      character*48 prog
      integer iargc,m
      character argument*72,mode*8,inf*144,appn*24,outs*248,act*16
      logical unixok
      integer itol
      integer incdbt  ! 1 is include dry bulb T 0 is ignore
      integer incrh   ! 1 is include RH 0 is ignore
      integer inclux  ! 1 is include LUX 0 is ignore
      integer incpir  ! 1 is include PIR 0 is ignore
      integer incsw   ! 1 is include switch 0 is ignore
      integer idumpts ! timesteps per hour to dump (default is 12 tsph i.e. 5 minute)
      character repsen*16  ! sensor name to exclude if not '-'
      integer nbixsen ! if greater than zero size of ixsen array
      character ixsen*16 
      dimension ixsen(4)  ! up to 4 sensors can be excluded when scanning

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
      itol = 30    ! if no one says anything assume 30 sec tollerance
      incdbt = 1   ! assume dbt included
      incrh = 1    ! assume RH included
      inclux = 1   ! assume LUX included
      incpir = 1   ! assume PIR included
      incsw = 1    ! assume switch count included
      inco2 = 1    ! assume CO2 is included
      repsen = '-' ! assume all LRS sensors to be reported
      nbixsen=0    ! assume no exclusions
      ixsen(1)=' '; ixsen(2)=' '; ixsen(3)=' '; ixsen(4)=' ';
      idumpts = 12 ! assume 5 minute timestep

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
            call edisp(iuout,'command line is:')
            call edisp(iuout,' [-act {L|i|2} ]')
            call edisp(iuout,'   i is iOpt log')
            call edisp(iuout,'   2 is BuildAX LRS 2 log')
            call edisp(iuout,'   L scan and list sensors and timings')
            call edisp(iuout,' [-tol {10|20|30|40} ]')
            call edisp(iuout,'   10 20 30 or 40 seconds tollerance for')
            call edisp(iuout,'   identifying relevant timesteps in log')
            call edisp(iuout,' [-name <sensor name> ] if included')
            call edisp(iuout,'   restricts reporting to this sensor')
            call edisp(iuout,'   ')
            call edisp(iuout,' -nodbt exclude temperature')
            call edisp(iuout,' -norh  exclude relative humidity')
            call edisp(iuout,' -nolux exclude Lux')
            call edisp(iuout,' -nopir exclude PIR')
            call edisp(iuout,' -nosw exclude switch count')
            call edisp(iuout,
     &         ' -ex XXXXXXXX exclude sensor XXXXXXXX from report')
            call edisp(iuout,' -ts timesteps per hour')
            call edisp(iuout,' [-file <BuildAX log file name>]')
            call edisp(iuout,' [-help  this message')
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
        elseif(argument(1:5).eq.'-tol ')then
          i=i+1
          call getarg(i,argument)
          read(argument,*,IOSTAT=IOS,ERR=2)itol
        elseif(argument(1:4).eq.'-ts ')then
          i=i+1
          call getarg(i,argument)
          read(argument,*,IOSTAT=IOS,ERR=2)idumpts
        elseif(argument(1:5).eq.'-name')then
          i=i+1
          call getarg(i,argument)
          read(argument,*,IOSTAT=IOS,ERR=2) repsen
        elseif(argument(1:6).eq.'-nodbt')then
          incdbt=0
        elseif(argument(1:5).eq.'-norh')then
          incrh=0
        elseif(argument(1:6).eq.'-nolux')then
          inclux=0
        elseif(argument(1:6).eq.'-nopir')then
          incpir=0
        elseif(argument(1:5).eq.'-nosw')then
          incsw=0
        elseif(argument(1:3).eq.'-ex')then
          nbixsen=nbixsen+1  ! increment counter
          i=i+1
          call getarg(i,argument)
          read(argument,*,IOSTAT=IOS,ERR=2) ixsen(nbixsen)
          write(6,*) argument,nbixsen,ixsen(nbixsen)
        else

C Assume argument is an input file without the -file token.
          write(inf,'(a)') argument(1:lnblnk(argument))
        endif
        goto 41

  42    continue

C Only give feedback if non-DOS.
        call isunix(unixok)
        if(unixok)then
          write(outs,'(7a,i3)') 'Starting ',appn(1:lnblnk(appn)),
     &    ' with file ',inf(1:lnblnk(inf)),' with action ',
     &    act(1:lnblnk(act)),' tollerance ',itol
C         call edisp248(iuout,outs,90)
        endif
        return
      endif

C Errors.
   2  call isunix(unixok)
      if(unixok)then
        call edisp(iuout,'Problem with command line parameter.')
      endif
      end


C ****** datestamptojulian
C datestamptojulian is passed date string (from a sensor or log file
C and returns:
C a julian day (juliand) and fraction juliand (julianfrac) and
C the seconds since the start of the day (totalsecs).
C act is requested action 'B' is for BuildAX log timing format
C   which is: 2013-06-28 16:15:26.662
C or action '2' is for BuildAX 2 csv file timings
C      datea='2014/12/22,13:41:05'

      subroutine datestamptojulian(datea,act,juliand,julianfrac,ijday,
     &  totalsecs)
      common/OUTIN/IUOUT,IUIN,IEOUT
      character datea*24,act*1,datetest*24,word*12

C Setup indices to hold the day of the week (1-7), the month
C (1-12), the day of the month (1-31), the hour of the day (1-24), the
C minute of the hour (1-60) second of minute and the year.
      integer imon,idom,ihrod,imohr,iyears,ijday
      real secofhr
      real totalsecs
      real juliand,julianfrac

      datetest=datea
      if(act(1:1).eq.'B')then
        K=0
        CALL EGETW(datetest,K,WORD,'W','yyyy-mm-dd',IFLAG)
        read(word(1:4),*,IOSTAT=IOS,ERR=1002) iyears
        read(word(6:7),*,IOSTAT=IOS,ERR=1003) imon
        read(word(9:10),*,IOSTAT=IOS,ERR=1003) idom

C Debug.
C        write(6,*) 'B style ',iyears,' ',imon,' ',idom
      elseif(act(1:1).eq.'2')then
        K=0
        CALL EGETW(datetest,K,WORD,'W','yyyy/mm/dd',IFLAG)
        read(word(1:4),*,IOSTAT=IOS,ERR=1002) iyears
        read(word(6:7),*,IOSTAT=IOS,ERR=1003) imon
        read(word(9:10),*,IOSTAT=IOS,ERR=1003) idom

C Debug.
C        write(6,*) 'Buildax2 ',iyears,' ',imon,' ',idom

      endif

      if(act(1:1).eq.'B')then
        CALL EGETW(datetest,K,WORD,'W','hh:mm:ss.fff',IFLAG)
        read(word(1:2),*,IOSTAT=IOS,ERR=1002) ihrod
        read(word(4:5),*,IOSTAT=IOS,ERR=1003) imohr
        read(word(7:12),*,IOSTAT=IOS,ERR=1003) secofhr
C Debug.
C        write(6,*) 'Buildax1 ',ihrod,' ',imohr,' ',secofhr
      elseif(act(1:1).eq.'2')then
        CALL EGETW(datetest,K,WORD,'W','hh:mm:ss.fff',IFLAG)
        read(word(1:2),*,IOSTAT=IOS,ERR=1002) ihrod
        read(word(4:5),*,IOSTAT=IOS,ERR=1003) imohr
        read(word(7:8),*,IOSTAT=IOS,ERR=1003) secofhr
C Debug.
C        write(6,*) 'Buildax2 ',ihrod,' ',imohr,' ',secofhr

      endif
      
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
      end

C ************** stblanks
C stblanks: Given `string' strip blanks & some wildcards and return as `name'.
C Useful to check file names can work with a buffer up to 4K char.
      SUBROUTINE stblanks(string,name)
      CHARACTER*(*) string,name
      character phrase4k*4000

C Clear variables and get string lengths.
      name = ' '
      phrase4k = ' '
      ilname = LEN(name)
      illstr = max(1,lnblnk(string))

C Strip off any leading blanks from string, compact spaces and tabs
C between words and substitute _ for any &.
C Stop copying if end of name reached.
      K=0
      DO 99 I=1,illstr
        if(ichar(string(I:I)).eq.0)goto 99
        if(ichar(string(I:I)).eq.9)goto 99
        if(ichar(string(I:I)).eq.32)goto 99
        if(ichar(string(I:I)).lt.32)goto 100
        if(I.gt.ilname)goto 100

C Debug.
C        write(6,'(a,I4)')string(I:I),ichar(string(I:I))

        K=K+1
        if(string(I:I).eq.'&')then
          phrase4k(K:K)='_'
        else
          phrase4k(K:K)=string(I:I)
        endif
 99   CONTINUE
100   continue
      LN=max(1,lnblnk(phrase4k))
      write(name,'(a)') phrase4k(1:LN)

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
      
      SUBROUTINE EDMLDB2(chgdb,ACTION,isel,IER)
      logical chgdb
      character*1 ACTION
      integer isel,ier
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

C ******************** IFAX ******* from esru_lib.F
C An integer function returning the integer part of it's
C argument truncated towards negative infinity as follows:
C     V        IFAX(V)
C    1.0        1
C     .9        0
C     .1        0
C      0        0
C    -.1       -1
C    -.9       -1
C   -1         -1
C   -1.1       -2

      FUNCTION IFAX(V)
        IFAX = INT(V)
        IF (V.LT.0) IFAX=IFAX-1
      RETURN
      END
