C ESP-r is free software.  You can redistribute it and/or
C modify it under the terms of the GNU General Public
C License as published by the Free Software Foundation 
C (version 2 or later).

C ESP-r is distributed in the hope that it will be useful
C but WITHOUT ANY WARRANTY; without even the implied
C warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
C PURPOSE. See the GNU General Public License for more
C details.


C The file commonclm.f is a collection of subroutines associated
C with weather data that are used by multiple applications.

C CLMGET    Fetch a days (real) data from weather file into common block for use.
C AZALT     Calculate the solar azimuth and elevation angles at each hour
C           of the day for any site.
C clmddscan Scan a weather file and determine the degree days.
C CLMCHKMET Check that we have at least the required 6 weather parameters.
C CLMRDBMD  Read metadata at the end of weather file.
C CLMMDN2O  Map weather metadata commons in climate.h to old commons for all modules.
C CLMRDBD   Read a days (integer) data from a weather file.
C CLMDN2O   Map weather data commons in climate.h to old commons in bps.
C CLMOPB    Open a weather file.
C CLMARR    Arrange weather parameter column order.

C ********** CLMGET **********
C Recover one day's data from weather file.
C Assumes that the header data has already been read.
      SUBROUTINE CLMGET(IDAY,IER)
#include "climate.h"

      integer iday  ! passed in julian day of the year
      integer idngh,ier
      COMMON/OUTIN/IUOUT,IUIN,IEOUT
      integer iuout,iuin,ieout

      integer j,k

C Check if we are trying to read the same data we already have.
      if (IDAY.eq.CMDAYR) goto 999

C Read data from climate file.
C CMDAYR is set in CLMRDBD.
      if (IDAY.eq.365) then
        idayf=1
      else
        idayf=IDAY+1
      endif
      CALL CLMRDBD(IDAY,idayf,IER)
      if (IER.gt.0) goto 666

C Data now in CMIVAL. Convert this to actual values and put into CMRVAL.
      do j=1,24
        do k=1,MCM
          if (CMXST(K)) then
            if (k.eq.1 .or. k.eq.5) then
              CMRVAL(K,J)=real(CMIVAL(K,J))*0.1
            else
              CMRVAL(K,J)=real(CMIVAL(K,J))
            endif
          endif
        enddo
      enddo

  999 RETURN

  666 call edisp(iuout,
     &  'CLMGET error; substituting zeros for data.')
      do j=1,24
        do k=1,MCM
          CMRVAL(K,J)=0.0
        enddo
      enddo

      goto 999
      END

C ************ AZALT ************
C Calculates the solar azimuth and altitude angles at each hour
C of the day for local time.

C RLONGT = Longitudinal difference, East is positive, West is negative
C          of the reference meridian, e.g. Greenwich (0 deg) for the UK.
C RLATIT = Latitude, North is positive, South is negative.
      SUBROUTINE AZALT(IYD,ITS,RLONGT,RLATIT,SOLALT,SOLAZI)
#include "help.h"
      
      real esind,ecosd  ! function definitions

      integer IYD  ! julian day of the year
      integer ITS  ! time system 1 = local mean time, 2 = local apparent time
      real RLONGT,RLATIT,SOLALT,SOLAZI
      DIMENSION SOLALT(24),SOLAZI(24)
      
      integer INIT
      real GLAT,GLON
      common/OUTIN/IUOUT,IUIN,IEOUT
      integer iuout,iuin,ieout
      COMMON/UAZALT/INIT,GLAT,GLON

C Local definitions.
      logical OK,close
      integer IHR,ISTAT
      real PI,DECLIN,SDDECL,CDDECL,CDTIME,TIME,TIMCOE,ABSTMC,ASDTIM
      real ALT,SDLAT,CDLAT,AZMUTH
      real A,B,EQTIME,X,Y

      helpinsub='common3dv'  ! set for subroutine
      PI = 4.0 * ATAN(1.0)
      INIT = 0
      GLAT = 55.9
      GLON = -4.15

C CHECK LATITUDE AND LONGITUDE.
      IF(INIT.EQ.1)GOTO 18
      IF(RLATIT.GT.90.0.OR.RLATIT.LT.-90.0)goto 14
      IF(RLONGT.GT.15.0.OR.RLONGT.LT.-15.0)goto 14
 
C SIN AND COS OF LATITUDE.
   15 SDLAT=ESIND(RLATIT)
      CDLAT=ECOSD(RLATIT)

C EQUATION OF TIME.
      IF(ITS.EQ.2)goto 11
      A=1.978*IYD-160.22
      B=0.989*IYD-80.11
      EQTIME=0.1645*ESIND(A)-0.1255*ECOSD(B)-0.025*ESIND(B)

C SOLAR DECLINATION.
   11 A=280.1+0.9863*FLOAT(IYD)
      DECLIN=23.45*ESIND(A)
      SDDECL=ESIND(DECLIN)
      CDDECL=ECOSD(DECLIN)

C IF LMT IS USED THEN THE LONGITUDE AND EQUATION OF TIME
C WILL EFFECT THE SOLAR TIME AT THE USERS SITE. A
C CORRECTION WILL THEREFORE BE REQUIRED.
      DO 10 IHR=1,24
      IF(ITS.EQ.2)GOTO 1
      TIME=IHR+(EQTIME+(RLONGT/15.0))
      GOTO 2
    1 TIME=IHR
    2 TIMCOE=15.0*(12.0-TIME)
      CDTIME=ECOSD(TIMCOE)
      ABSTMC=ABS(TIMCOE)
C      SDTIME=ESIND(TIMCOE)
      ASDTIM=ESIND(ABSTMC)

C SOLAR ALTITUDE.
      ALT=SDLAT*SDDECL+CDLAT*CDDECL*CDTIME
      SOLALT(IHR)=ASIN(ALT)*180.0/PI

C SOLAR AZIMUTH.
      AZMUTH=(CDDECL*ASDTIM)/ECOSD(SOLALT(IHR))
      IF(AZMUTH.LT.-1.0)AZMUTH=-1.0
      IF(AZMUTH.GT.1.0)AZMUTH=1.0
      SOLAZI(IHR)=ASIN(AZMUTH)*180.0/PI

C Correct the azimuthal angle for time of day and whether
C in north or south hemispheres.
      X=CDTIME
      call eclose(RLATIT,0.0,0.001,close)
      IF(close)goto 13
      call eclose(RLATIT,90.0,0.001,close)
      IF(close)goto 8
      Y=(CDLAT/SDLAT)*(SDDECL/CDDECL)
      goto 9
    8 Y=0.0
      goto 9
   13 Y=100.0*(SDDECL/CDDECL)
    9 IF(Y-X)3,4,5
    3 IF(RLATIT.GE.0.0)goto 6
      goto 7
    5 IF(RLATIT.LT.0.0)goto 6
      goto 7
    4 IF(TIME.LE.12.0)SOLAZI(IHR)=90.0
      IF(TIME.GT.12.0)SOLAZI(IHR)=270.0
      goto 10
    6 IF(TIME.LE.12.0)SOLAZI(IHR)=180.0-SOLAZI(IHR)
      IF(TIME.GT.12.0)SOLAZI(IHR)=180.0+SOLAZI(IHR)
      goto 10
    7 IF(TIME.GT.12.0)SOLAZI(IHR)=360.0-SOLAZI(IHR)
   10 CONTINUE
      RETURN

   14 helptopic='azalt_limits'
      call gethelptext(helpinsub,helptopic,nbhelp)
      CALL EASKOK('Lat. +/- 89.9 deg. or Long. diff. +/- 14.9 deg.',
     &  'Continue with default values?',OK,nbhelp)
      if(.NOT.OK)then
        call pauses(3)   ! pause for 3 seconds then abort program.
        close(ieout)
        CALL ERPFREE(ieout,ISTAT)
        CALL EPAGEND
        STOP
      endif
   18 RLATIT=GLAT
      RLONGT=GLON
      INIT=1
      goto 15
      END

C ********** clmddscan **********
C Scans a weather file and determines the degree-days based
C on common block data in seasons.h.

C Parameters:
C  itru (integer) unit for reporting
C  act (1 char) '?' ask about best-fit weeks, 's' silent ok bast fit,
C               '-' do not record best-fit weeks.
C  topic (1 char) 'c' called from clm, 'p' from IPV
C  nseasons (integer) number of seasons to scan.
C  icuryear (integer) the applicable year

      subroutine clmddscan(itrc,itru,act,topic,nseasons,icuryear)
#include "building.h"
#include "seasons.h"
#include "ipvdata.h"
#include "climate.h"
#include "help.h"

      integer lnblnk  ! function definition

      COMMON/OUTIN/IUOUT,IUIN,IEOUT
      COMMON/SET1/IYEAR,IBDOY,IEDOY,IFDAY,IFTIME
      COMMON/CLMSET/ICYEAR,ICDNGH,CLAT,CLONG
      COMMON/RADTYP/IDNGH

C dm* variables are from ipvdata.h.
C sea* variables are from season.h

      character outs*124,PERST1*14,PERST2*44,PERST3*44
      character DESCR*7,DESCR1*10,title*40,hold*32,act*1,topic*1
      character DESCR2*8
      DIMENSION ALT(24),AZI(24),GHSUM(MSPS),DAYGLOB(MSPS)

      real DNJ       ! T below heating deg-hours
      real DNM       ! subtotal of heating deg-hours
      real XDN       ! seasonal hdd subtotal to write to seahddtot
      real HRM       ! subtotal of hours
      real UPJC      ! T above cooling base deg-hours
      real UPMC      ! subtotal of cooling deg-hours
      real XUPC      ! seasonal cdd subtotal to write to seacddtot
      real HRMC      ! hours in the season
      real DAYL      ! length of day (24 hours)
      real DIRH      ! hemispheric solar radiation

C For each of the seasons:
C  cddratio is the season / assessment ratio for cooling
C  hddratio is the season / assessment ratio for heating
      dimension cddratio(MIPVA)
      dimension hddratio(MIPVA)

      logical ok

C Clear initial values and parameters for whole period.
      helpinsub='common3dv'  ! set for subroutine
      DNM=0.
      HRM=0.
      UPMC=0.
      HRMC=0.
      DNI=0
      HRTOT=0.
      UPIC=0
      HRTOTC=0.

C Loop through each seasonal period.
      do 42 k=1,nseasons
        GHSUM(k)=0.
        DAYGLOB(k)=0.
        seahddwk(k)=0.
        seacddwk(k)=0.
        seahddtot(k)=0.
        seacddtot(k)=0.
        dmheat(k)=1.0
        dmcool(k)=1.0
        dmlight(k)=1.0
        dmsmlpw(k)=1.0
        dmfan(k)=1.0
        dmdhw(k)=1.0
        if(k.eq.1)then
          title='Winter (early year) season '
          DNI=0
          HRTOT=0.
          UPIC=0
          HRTOTC=0.
          IDSS=is1wins
          IDSF=is1winf
        elseif(k.eq.2)then
          title='Spring season '
          IDSS=is1sprs
          IDSF=is1sprf
        elseif(k.eq.3)then
          title='Summer season '
          IDSS=is1sums
          IDSF=is1sumf
        elseif(k.eq.4)then
          title='Autumn season '
          IDSS=is2sprs
          IDSF=is2sprf
        elseif(k.eq.5)then
          title='Winter (late year) season '
          IDSS=is2wins
          IDSF=is2winf
        endif
        CALL EPERSTR(icuryear,IDSS,0,IDSF,24,1,
     &             IFDAY,IFTIME,PERST1,PERST2,PERST3,IER)

C For each day in the season read climate data, set parameters and analyse.
        DO 10 I=IDSS,IDSF
          IDD=I
          CALL CLMGET(IDD,IER)
          CALL AZALT(IDD,1,CLONG,CLAT,ALT,AZI)
          DNJ=0
          UPJC=0.
          DAYL=24.0

C For each hour in the day work out radiation and then degree days.
          DO 20 J=1,24
            IF(ALT(J).LT.0.001)THEN
              DIRH=0.
            else
              IF(IDNGH.EQ.0)THEN
                DIRH=CMRVAL(3,J)*ESIND(ALT(J))
              ELSE
                DIRH=CMRVAL(4,J)-CMRVAL(2,J)
              ENDIF
              GHSUM(k)=(GHSUM(k)+DIRH+CMRVAL(2,J))
            endif

            TT=CMRVAL(1,J)
            IF(TT.GT.hddbaset)then
            else

C T below heating BASE,SAVE DEG-HRS IN DNJ
              DNJ=DNJ+(hddbaset-TT)
            endif
            IF(TT.GT.cddbaset)then

C T above cooling BASE ,SAVE DEG-HRS IN UPJC
              UPJC=UPJC+(TT-cddbaset)
            endif
   20     CONTINUE       ! end of the hour loop
          XDN=DNJ/24     ! convert from degree-hours to degree days
          XUPC=UPJC/24   ! and do this for cooling as well

C Update seasonal parameters and go to the next day.
          DNM=DNM+DNJ     ! increment heating degree hours
          HRM=HRM+DAYL    ! increment heating hours
          UPMC=UPMC+UPJC  ! increment cooling degree hours
          HRMC=HRMC+DAYL  ! increment cooling hours
   10   CONTINUE          ! end of the day-in-the-season loop

C Compute parameters (total & average degree days) for this season.
        XHR=24.0/HRM
        XHRC=24.0/HRMC
        XDN=DNM/24
        XUPC=UPMC/24
        YDN=XHR*XDN         ! heat dd avg/day
        seahddwk(k)=YDN*7.
        YUPC=XHRC*XUPC      ! cool dd avg/ay
        seacddwk(k)=YUPC*7.

C Number of days in season is IDSF-IDSS+1. DAYGLOB is daily mean
C global horizontal solar for season.
        DAYGLOB(k)=(GHSUM(k)/(IDSF-IDSS+1))/1000.

C Report on the current season.
        call eddisp(itru,' ')
        WRITE(outs,'(3a)')title(1:lnblnk(title)),' ',PERST2
        call eddisp(itru,outs)
        WRITE(outs,'(a,F8.2,a,F8.2,a,F9.2)')' Heat DD avg/day   ',YDN,
     &    '  Cool DD avg/day  ',YUPC,'  Rad avg/day ',DAYGLOB(k)
        call eddisp(itru,outs)
        WRITE(outs,'(a,F8.2,a,F8.2,a,F9.2)')' Heat DD avg/week  ',
     &    seahddwk(k),'  Cool DD avg/week ',seacddwk(k),
     &    '  Rad avg/week',DAYGLOB(k)*7.
        call eddisp(itru,outs)
        WRITE(outs,'(a,F8.2,a,F8.2,a,F9.2)')' Season heat DD    ',XDN,
     &     '  Season cool DD   ',XUPC,'  Rad total   ',GHSUM(k)/1000.
        call eddisp(itru,outs)
        seahddtot(k)=XDN
        seacddtot(k)=XUPC

C UPDATE WHOLE PERIOD
        HRTOT=HRTOT+HRM
        DNI=DNI+DNM
        DNM=0.
        HRM=0.
        HRTOTC=HRTOTC+HRMC
        UPIC=UPIC+UPMC
        UPMC=0.
        HRMC=0.
  42  continue   ! end of season loop

C Loop through each week in each season and check against the
C calculated average data.
C Loop through each seasonal period.
      helptopic='typical_week_scan'
      call gethelptext(helpinsub,helptopic,nbhelp)
      call easkab(' ','Options:','search for typical weeks',
     &  'cancel',iok,nbhelp)
      if(iok.eq.2)return

C Allow user to define weightings.
      helptopic='dd_weightings'
      call gethelptext(helpinsub,helptopic,nbhelp)
      write(hold,'(3f6.3)') hddw,cddw,radw
 143  CALL EASKS(HOLD,' ','Weightings for HDD, CDD & solar:',32,
     &  ' 1.0 1.0 1.0 ','weightings for hdd cdd solar',IER,nbhelp)
      K=0
      CALL EGETWR(HOLD,K,hddw,0.0,1.0,'W','hdd weight',IER)
      CALL EGETWR(HOLD,K,cddw,0.0,1.0,'W','cdd weight',IER)
      CALL EGETWR(HOLD,K,radw,0.0,1.0,'W','solar weight',IER)
      if(ier.ne.0)goto 143

C Loop through each of the assessment periods << MIPVA? >>.
      IWK=0
      do 43 k=1,5
        if(k.eq.1)then
          call edisp(itru,'Winter (early year) season.')
          IDSS=is1wins
          IDSF=is1winf
        elseif(k.eq.2)then
          call edisp(itru,'Spring season.')
          IDSS=is1sprs
          IDSF=is1sprf
        elseif(k.eq.3)then
          call edisp(itru,'Summer season.')
          IDSS=is1sums
          IDSF=is1sumf
        elseif(k.eq.4)then
          call edisp(itru,'Autumn season.')
          IDSS=is2sprs
          IDSF=is2sprf
        elseif(k.eq.5)then
          call edisp(itru,'Winter (late year) season.')
          IDSS=is2wins
          IDSF=is2winf
        endif
        iwkbest(k)=0
        iwkbstrt(k)=0
        wkdiff(k)=3.
        GHSUMW=0.
        CALL EPERSTR(icuryear,IDSS,0,IDSF,24,1,
     &             IFDAY,IFTIME,PERST1,PERST2,PERST3,IER)
        WRITE(outs,'(a)')PERST2
        call edisp(itru,outs)
        if(itru.ne.iuout)call edisp(iuout,outs)

C For each day read climate data, set parameters and analyse.
C Find day of week for start of the period. IWK is week number, IDSOW
C is the julian day at the start of the week.
        CALL EDAYR(IDSS,IDAYN,IMTHN)
        CALL EWEEKD(IDAYN,IMTHN,icuryear,IXDWK)
        IDSOW=IDSS
        ix=0

C For each day read climate data, set parameters and analyse.
        DO 11 I=IDSS,IDSF
          IDD=I
          ix=ix+1
          CALL CLMGET(IDD,IER)
          CALL AZALT(IDD,1,CLONG,CLAT,ALT,AZI)
          DNJ=0
          UPJC=0
          DO 21 J=1,24
            IF(ALT(J).LT.0.001)THEN
              DIRH=0.
            else
              IF(IDNGH.EQ.0)THEN
                DIRH=CMRVAL(3,J)*ESIND(ALT(J))
              ELSE
                DIRH=CMRVAL(4,J)-CMRVAL(2,J)
              ENDIF
              GHSUMW=(GHSUMW+DIRH+CMRVAL(2,J))
            endif

            TT=CMRVAL(1,J)
            IF(TT.GT.hddbaset)then
            else

C Temperature below heating BASE,SAVE DEG-HRS IN DNJ.
              DNJ=DNJ+(hddbaset-TT)
            endif
            IF(TT.GT.cddbaset)then

C Temperature above cooling BASE ,SAVE DEG-HRS IN UPJC.
              UPJC=UPJC+(TT-cddbaset)
            endif
   21     CONTINUE

C Convert day to D,M.
          II=I
          CALL EDAYR(II,IDD,IMM)
          CALL EWEEKD(IDD,IMM,icuryear,IDWK)

C Prepare for output, check if week complete.
          IF(II.eq.IDSS)GO TO 41
          IF(IDWK.ne.IXDWK)GO TO 41
          iwk=iwk+1

C Compute parameters (total & average degree days) for this week.
          XHR=24.0/HRM
          XHRC=24.0/HRMC
          XDN=DNM/24
          XUPC=UPMC/24
          call stdate(icuryear,IDSOW,DESCR,DESCR1,DESCR2)
          YDN=XHR*XDN
          YUPC=XHRC*XUPC
          WRITE(outs,'(a,i2,3a,2F8.2,a,F8.3)')' Week:',iwk,
     &      ' starting ',DESCR1,' DD total/week  ',XDN,XUPC,
     &      '  Rad total/week ',GHSUMW/1000.
          call edisp(itru,outs)
          if(itru.ne.iuout)call edisp(iuout,outs)

C Deal with radiation.
          if(seahddwk(k).gt.0.001)then
            PARM1=ABS(XDN-seahddwk(k))/seahddwk(k)
          else
            PARM1=0.
          endif
          if(seacddwk(k).gt.0.001)then
            PARM2=ABS(XUPC-seacddwk(k))/seacddwk(k)
          else
            PARM2=0.
          endif
          PARM3=ABS((GHSUMW/1000.)-(DAYGLOB(k)*7.))/(DAYGLOB(k)*7.)
          PARMTOT=(PARM1*hddw)+(PARM2*cddw)+(PARM3*radw)
          WRITE(outs,'(a,3F9.3,a,F9.3)')' Params (heat|cool|solar)',
     &      PARM1,PARM2,PARM3,' Params total ',PARMTOT
          if(itrc.gt.1) call edisp(itru,outs)

          if(PARMTOT.lt.wkdiff(k))then
            wkdiff(k)=PARMTOT  ! remember the weighted parameters
            iwkbest(k)=iwk     ! remember the week index
            iwkbstrt(k)=IDSOW  ! remember julian day at start of that week
            wkheatdd(k)=XDN
            wkcooldd(k)=XUPC
          endif

C Reset weekly total global horizontal.
          GHSUMW=0.

C Update whole period.
          IDSOW=II
          HRTOT=HRTOT+HRM
          DNI=DNI+DNM
          DNM=0.
          HRM=0.
          HRTOTC=HRTOTC+HRMC
          UPIC=UPIC+UPMC
          UPMC=0.
          HRMC=0.

C Convert to degree-days per day.
  41      CONTINUE
          XDN=DNJ/24
          XUPC=UPJC/24

C Update parameters for this week
          DNM=DNM+DNJ
          HRM=HRM+DAYL
          UPMC=UPMC+UPJC
          HRMC=HRMC+DAYL

C Go to next day.
   11   CONTINUE

C Compute parameters for last week.
        XHR=24.0/HRM
        XHRC=24.0/HRMC
        XDN=DNM/24
        XUPC=UPMC/24
        iwk=iwk+1
        call stdate(icuryear,IDSOW,DESCR,DESCR1,DESCR2)
        YDN=XHR*XDN
        YUPC=XHRC*XUPC
        WRITE(outs,'(a,i2,3a,2F8.2,a,F8.3)')' Week:',iwk,
     &    ' starting ',DESCR1,' DD total/week  ',XDN,XUPC,
     &    '  Rad total/week ',GHSUMW/1000.
        call edisp(itru,outs)
        if(itru.ne.iuout)call edisp(iuout,outs)

C Deal with radiation here for last week of season.
        if(seahddwk(k).gt.0.001)then
          PARM1=ABS(XDN-seahddwk(k))/seahddwk(k)
        else
          PARM1=0.
        endif
        if(seacddwk(k).gt.0.001)then
          PARM2=ABS(XUPC-seacddwk(k))/seacddwk(k)
        else
          PARM2=0.
        endif
        PARM3=ABS((GHSUMW/1000.)-(DAYGLOB(k)*7.))/(DAYGLOB(k)*7.)
        PARMTOT=(PARM1*hddw)+(PARM2*cddw)+(PARM3*radw)
        WRITE(outs,'(a,3F9.3,a,F9.3)')' Params (heat|cool|solar)',
     &    PARM1,PARM2,PARM3,' Params total ',PARMTOT
        call edisp(itru,outs)
        if(PARMTOT.lt.wkdiff(k))then
          wkdiff(k)=PARMTOT
          iwkbest(k)=iwk
          iwkbstrt(k)=IDSOW
          wkheatdd(k)=XDN
          wkcooldd(k)=XUPC
        endif

C Cast year and julian day at start of best fit week to a description.
        call stdate(icuryear,iwkbstrt(k),DESCR,DESCR1,DESCR2)
        WRITE(outs,'(a,i3,3a,F6.2,a,F6.2)')' Best week number is ',
     &    iwkbest(k),' ',DESCR1,' with HDD of ',wkheatdd(k),
     &    ' & CDD of ',wkcooldd(k)
        call edisp(itru,outs)
        if(itru.ne.iuout)call edisp(iuout,outs)

C Convert from julian day of the start of the assessment to the end
C of the assessment to number of days in period and get ratio with
C the number of days in the season.
        ijulst=iwkbstrt(k)
        ijulfn=iwkbstrt(k)+6

C Ratio of seasonal total heating and cooling degree days and the
C best assessment week heating and cooling degree days. Ensure that
C there is no divide by zero.
        if(wkheatdd(k).gt.0.01)then
          hddratio(k)=seahddtot(k)/wkheatdd(k)
        else
          hddratio(k)=1.0
        endif
        if(wkcooldd(k).gt.0.01)then
          cddratio(k)=seacddtot(k)/wkcooldd(k)
        else
          cddratio(k)=1.0
        endif
        if(k.eq.1)then
          dayratio=((is1winf-is1wins)+1)/((ijulfn-ijulst)+1)
          write(outs,'(a,F6.3)') 
     &      'early winter ratio of season/assessment days: ',dayratio
          call edisp(iuout,outs)
          write(outs,'(a,F6.3,A,F6.3)') 
     &      'early winter ratio of season/assessment heat DD: ',
     &      hddratio(k),' & cool DD:',cddratio(k)
          call edisp(iuout,outs)
          if(act.eq.'?')then
            helptopic='just_say_yes'
            call gethelptext(helpinsub,helptopic,nbhelp)
            call easkok('Use best-fit week for the 1st winter season',
     &       'typical assessment period in the climatelist?',ok,nbhelp)
            if(ok)then
              ia1wins = iwkbstrt(k)
              ia1winf = iwkbstrt(k)+6
            endif
          elseif(act.eq.'s')then
            ia1wins = iwkbstrt(k)
            ia1winf = iwkbstrt(k)+6
          endif
        elseif(k.eq.2)then
          dayratio=((is1sprf-is1sprs)+1)/((ijulfn-ijulst)+1)
C Debug...
          write(outs,'(a,F6.3)') 
     &      'spring ratio of season/assessment days: ',dayratio
          call edisp(iuout,outs)
          write(outs,'(a,F6.3,A,F6.3)') 
     &      'spring ratio of season/assessment heat DD: ',
     &      hddratio(k),' & cool DD:',cddratio(k)
          call edisp(iuout,outs)
          if(act.eq.'?')then
            helptopic='just_say_yes'
            call gethelptext(helpinsub,helptopic,nbhelp)
            call easkok('Use best-fit week for the spring season',
     &       'typical assessment period in the climatelist?',ok,nbhelp)
            if(ok)then
              ia1sprs = iwkbstrt(k)
              ia1sprf = iwkbstrt(k)+6
            endif
          elseif(act.eq.'s')then
            ia1sprs = iwkbstrt(k)
            ia1sprf = iwkbstrt(k)+6
          endif
        elseif(k.eq.3)then
          dayratio=((is1sumf-is1sums)+1)/((ijulfn-ijulst)+1)
          write(outs,'(a,F6.3)') 
     &      'summer ratio of season/assessment days: ',dayratio
          call edisp(iuout,outs)
          write(outs,'(a,F6.3,A,F6.3)') 
     &      'summer ratio of season/assessment heat DD: ',
     &      hddratio(k),' & cool DD:',cddratio(k)
          call edisp(iuout,outs)
          if(act.eq.'?')then
            helptopic='just_say_yes'
            call gethelptext(helpinsub,helptopic,nbhelp)
            call easkok('Use best-fit week for the summer season',
     &       'typical assessment period in the climatelist?',ok,nbhelp)
            if(ok)then
              iasums = iwkbstrt(k)
              iasumf = iwkbstrt(k)+6
            endif
          elseif(act.eq.'s')then
            iasums = iwkbstrt(k)
            iasumf = iwkbstrt(k)+6
          endif
        elseif(k.eq.4)then
          dayratio=((is2sprf-is2sprs)+1)/((ijulfn-ijulst)+1)
          write(outs,'(a,F6.3)') 
     &      'autumn ratio of season/assessment days: ',dayratio
          call edisp(iuout,outs)
          write(outs,'(a,F6.3,A,F6.3)') 
     &      'autumn ratio of season/assessment heat DD: ',
     &      hddratio(k),' & cool DD:',cddratio(k)
          call edisp(iuout,outs)
          if(act.eq.'?')then
            helptopic='just_say_yes'
            call gethelptext(helpinsub,helptopic,nbhelp)
            call easkok('Use best-fit week for the autumn season',
     &       'typical assessment period in the climatelist?',ok,nbhelp)
            if(ok)then
              ia2sprs = iwkbstrt(k)
              ia2sprf = iwkbstrt(k)+6
            endif
          elseif(act.eq.'s')then
            ia2sprs = iwkbstrt(k)
            ia2sprf = iwkbstrt(k)+6
          endif
        elseif(k.eq.5)then
          dayratio=((is2winf-is2wins)+1)/((ijulfn-ijulst)+1)
          write(outs,'(a,F6.3)') 
     &      'late winter ratio of season/assessment days: ',dayratio
          call edisp(iuout,outs)
          write(outs,'(a,F6.3,A,F6.3)') 
     &      'late winter ratio of season/assessment heat DD: ',
     &      hddratio(k),' & cool DD:',cddratio(k)
          call edisp(iuout,outs)
          if(act.eq.'?')then
            helptopic='just_say_yes'
            call gethelptext(helpinsub,helptopic,nbhelp)
            call easkok('Use best-fit week for the 2nd winter season',
     &       'typical assessment period in the climatelist?',ok,nbhelp)
            if(ok)then
              ia2wins = iwkbstrt(k)
              ia2winf = iwkbstrt(k)+6
            endif
          elseif(act.eq.'s')then
            ia2wins = iwkbstrt(k)
            ia2winf = iwkbstrt(k)+6
          endif
        endif

C For the current season set degree day or day ratios. For heating
C and cooling ensure that the dm* value is at least as large as the
C day ratio.
        if(hddratio(k).gt.dayratio)then
          dmheat(k)=hddratio(k)
        else
          dmheat(k)=dayratio
        endif
        if(cddratio(k).gt.dayratio)then
          dmcool(k)=cddratio(k)
        else
          dmcool(k)=dayratio
        endif
        dmlight(k)=dayratio
        dmsmlpw(k)=dayratio
        dmfan(k)=dayratio
        dmdhw(k)=dayratio
        helptopic='record_before_proceed'
        call gethelptext(helpinsub,helptopic,nbhelp)
        if(act.eq.'?')then
          continue
        else
          if(k.eq.5)then  ! jump out on the final standard season
            iok=1
          else
            call easkab('Options:',' ','next season','cancel',
     &        iok,nbhelp)
          endif
          if(iok.eq.2)return
        endif
        ix=1
 43   continue

      return
      end

C ********** CLMCHKMET **********
C CLMCHKMET checks that we have at least the required 6 metrics in a V2
C weather file. IER=0 if yes, 1 if no.
      SUBROUTINE CLMCHKMET(IER)
#include "climate.h"

      IER=1
      if (CMXST(1)           ! Dry bulb temperature
     &  .and.CMXST(2)        ! Diffuse horizontal solar intensity
     &  .and.(CMXST(3)       ! Direct normal solar intensity
     &    .or.CMXST(4))      ! Global horizonal solar intensity
     &  .and.CMXST(5)        ! Wind speed
     &  .and.CMXST(6)        ! Wind direction
     &  .and.CMXST(7)) IER=0 ! Relative humidity

      RETURN
      END

C ********** CLMRDBMD **********
C Reads metadata at the end of a binary climate database
C and sets values of commons CLMFIL and CLMMET in climate.h.
C It is assumed that the file is already open and the
C version is known (CFVER in climate.h).
      SUBROUTINE CLMRDBMD(IER)
#include "esprdbfile.h"
#include "climate.h"

      COMMON/CLMSET/ICYEAR,ICDNGH,CLAT,CLONG

      if (CFVER.eq.1) then
        IREC=366
        READ(ICLIM,REC=IREC,IOSTAT=ISTAT,ERR=666)CFYEAR,ICDNGH
        IREC=IREC+1
        READ(ICLIM,REC=IREC,IOSTAT=ISTAT,ERR=666)CFLOC
        IREC=IREC+1
        READ(ICLIM,REC=IREC,IOSTAT=ISTAT,ERR=666)CFLAT,CFLONG

        NCM=6
        CMXST(1:2)=(/.true.,.true./)
        CMCOL(1:2)=(/2,1/)
        if (ICDNGH.eq.123) then
          CMXST(3:4)=(/.false.,.true./)
          CMCOL(3:4)=(/0,3/)
        else
          CMXST(3:4)=(/.true.,.false./)
          CMCOL(3:4)=(/3,0/)
        endif
        CMXST(5:7)=(/.true.,.true.,.true./)
        CMCOL(5:7)=(/4,5,6/)
        do i=8,MCM
          CMXST(i)=.false.
          CMCOL(i)=0
        enddo

      elseif (CFVER.gt.1) then
        IREC=366
        READ(ICLIM,REC=IREC,IOSTAT=ISTAT,ERR=666)CFYEAR
        IREC=IREC+1
        READ(ICLIM,REC=IREC,IOSTAT=ISTAT,ERR=666)CFLOC,CFLAT,CFLONG
        IREC=IREC+1
        READ(ICLIM,REC=IREC,IOSTAT=ISTAT,ERR=666)(CMCOL(i),i=1,CFMCM)
        NCM=0
        do i=1,CFMCM
          if (CMCOL(i).ge.1) then
            CMXST(i)=.true.
            NCM=NCM+1
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

      else
        goto 666

      endif

      CALL CLMCHKMET(IER)
      CALL CLMMDN2O

 999  RETURN

 666  IER=1
      goto 999

      END

C ********** CLMMDN2O **********
C Maps values of new weather commons in climate.h to old
C commons, for metadata from a binary weather file.

      SUBROUTINE CLMMDN2O

#include "climate.h"

C in BPS.
      COMMON/DNORGH/IRTYPE
      INTEGER irtype

C in CLM.
      COMMON/RADTYP/IDNGH
      COMMON/CLMDT1/CLMLOC
      CHARACTER CLMLOC*42

C in both.
      COMMON/CLMSET/ICYEAR,ICDNGH,CLAT,CLONG
      INTEGER icyear,icdngh
      REAL clat,clong

      if (CMXST(4)) then
        IRTYPE=1
        IDNGH=123
        ICDNGH=123
      else
        IRTYPE=0
        IDNGH=0
        ICDNGH=0
      endif
      CLMLOC=CFLOC
      ICYEAR=CFYEAR
      CLAT=CFLAT
      CLONG=CFLONG

      RETURN
      END

C ********** CLMRDBD **********
C Reads IDAYP's weather data from a binary weather file, plus
C the first time step of IDAYF's weather data, and puts values
C into common CMIVAL.

      SUBROUTINE CLMRDBD(IDAYP,IDAYF,IER)

#include "climate.h"
#include "esprdbfile.h"

      COMMON/OUTIN/IUOUT,IUIN,IEOUT

      character outs*124
      integer CFIVAL(MCM,25)

C Check if we are trying to read the same data we already have.
      if (IDAYP.eq.CMDAYR) goto 999

C Read data.
      IREC=IDAYP
      READ(ICLIM,REC=IREC,IOSTAT=ISTAT,ERR=666)
     &  ((CFIVAL(i,j),i=1,CFMCM),j=1,24)
      IREC=IDAYF
      READ(ICLIM,REC=IREC,IOSTAT=ISTAT,ERR=666)
     &  (CFIVAL(i,25),i=1,CFMCM)


C Assign values to CMIVAL and range check. Do not warn if
C no atmospheric pressure has been defined.
      do j=1,25
        do i=1,MCM
          icol=CMCOL(i)
          if (icol.eq.0) then
            CMIVAL(i,j)=0
          else
            ival=CFIVAL(icol,j)
            if (ival.gt.CFMAX(i)) then
              l=lnblnk(CMNAMF(i))
              write(outs,'(3a,i2.2,a,i3.3,a)')' Warning: ',
     &          CMNAMF(i)(1:l),' at hour ',j,' day ',IDAYP,
     &          ' is greater than maximum'
              CALL EDISP(IUOUT,outs)
            elseif (ival.lt.CFMIN(i)) then
              if(i.eq.10.and.ival.eq.0)then
                continue  ! Case of pressure not having been set.
              else
                l=lnblnk(CMNAMF(i))
                write(outs,'(3a,i2.2,a,i3.3,a)')' Warning: ',
     &            CMNAMF(i)(1:l),' at hour ',j,' day ',IDAYP,
     &            ' is less than minimum'
                CALL EDISP(IUOUT,outs)
              endif
            endif
            CMIVAL(i,j)=ival
          endif
        enddo
      enddo

C Set the day of data we now have in commons.
      CMDAYR=IDAYP

  999 RETURN

  666 IER=1
      goto 999

      END

C ********** CLMDN2O **********
C Maps values of new weather commons in climate.h to old
C commons, for data from a binary weather file.

      SUBROUTINE CLMDN2O

#include "climate.h"
#include "building.h"

C in BPS.      
      common/clim/idif(mt),itmp(mt),idnr(mt),ivel(mt),idir(mt),
     &            ihum(mt),idiff,itmpf,idnrf,ivelf,idirf,ihumf

      do i=1,24
        idif(i)=CMIVAL(2,i)
        itmp(i)=CMIVAL(1,i)
        if (CMXST(3)) then
          idnr(i)=CMIVAL(3,i)
        elseif (CMXST(4)) then
          idnr(i)=CMIVAL(4,i)
        endif
        ivel(i)=CMIVAL(5,i)
        idir(i)=CMIVAL(6,i)
        ihum(i)=CMIVAL(7,i)
      enddo

      idiff=CMIVAL(2,25)
      itmpf=CMIVAL(1,25)
      if (CMXST(3)) then
        idnrf=CMIVAL(3,25)
      elseif (CMXST(4)) then
        idnrf=CMIVAL(4,25)
      endif
      ivelf=CMIVAL(5,25)
      idirf=CMIVAL(6,25)
      ihumf=CMIVAL(7,25)

      RETURN
      END

C ********** CLMOPB **********
C Attempts to open a binary weather file. Version 1 files have a
C fixed record width of 144 (24 hours * 6 parameters). Version 2
C weather files have a record width of 24 hours * MCM parameters 
C (defined in climate.h). This subroutine maintains back-compatibility
C using a list of all possible values for MCM, including previous
C versions (MCMALL in climate.h). The version of the weather file
C is also set (CFVER in climate.h).

C IEXIST is passed to EFOPRAN (in esru_lib.F):
C   0 - should exist, no message but error -301 if not,
C   1 - should exist, message & error -301 if not,
C   2 - should not exist, error 300 & message if exists,
C   3 - if exists use otherwise create,
C   4 - if exists ask user before overwriting, otherwise create.

      SUBROUTINE CLMOPB(IEXIST,IER)

#include "climate.h"
#include "esprdbfile.h"
#include "espriou.h"

      COMMON/OUTIN/IUOUT,IUIN,IEOUT
      
      logical fok,unixok
      character fs*1,llclmdb*144,outs*124
      integer itmp
      real rtmp,rtmp2

      CFVER=0

C Set folder separator (fs) to \ or / as required.
      call isunix(unixok)
      if(unixok)then
        fs = char(47)
      else
        fs = char(92)
      endif

C Handle path to the climate file.
      lndbp=lnblnk(standardclmpath)
      if(ipathclim.eq.0.or.ipathclim.eq.1)then
        llclmdb=LCLIM
      elseif(ipathclim.eq.2)then
        write(llclmdb,'(3a)') standardclmpath(1:lndbp),fs,
     &    LCLIM(1:lnblnk(LCLIM))
      endif

C Try to open the file.
      write(currentfile,'(a)') llclmdb(1:lnblnk(llclmdb))
      IER=0
      call ERPFREE(ICLIM,IER)   
      call EFOPRAN(ICLIM,llclmdb,MCM*24,0,IER)
      if (IER.eq.-301) then
        if (IEXIST.eq.2 .or. IEXIST.eq.3 .or. IEXIST.eq.4) then
          call ERPFREE(ICLIM,IER)   
          call EFOPRAN(ICLIM,llclmdb,MCM*24,IEXIST,IER)
          if (IER.ne.0) goto 666
          write(outs,'(2a)')' Opened new climate database ',
     &      LCLIM(1:lnblnk(LCLIM))
          call EDISP(IUOUT,outs)
          goto 999
        else
          goto 666
        endif
      elseif (IER.ne.0) then
        goto 666
      endif

C Check if we can read the 4th line of metadata. IOSTAT codes are
C implementation-dependant, so we just test for any error.
      IREC=369
      READ(ICLIM,REC=IREC,IOSTAT=ISTAT,ERR=6661)itmp
      CFVER=itmp
      CFMCM=MCM
      IER=0
      goto 999

C If we get to here, there has been an error. We might have opened
C the climate database with the wrong record width. Loop (backwards) 
C through MCMALL and try to open the database with previous possible
C record widths.
 6661 fok=.false. 
c      write(6,*)outs ! Debug
      do 6662 i=NMCM-1,1,-1
        call ERPFREE(ICLIM,IER)
        call EFOPRAN(ICLIM,llclmdb,MCMALL(i)*24,0,IER)

C If we have reached the first value in MCMALL, this could be a v1
C database in which case the 4th line of metadata doesn't exist; try the
C 3rd.
        if (i.eq.1) then
          IREC=368
          READ(ICLIM,REC=IREC,IOSTAT=ISTAT,ERR=6662)rtmp,rtmp2
          itmp=1

        else          
          READ(ICLIM,REC=IREC,IOSTAT=ISTAT,ERR=6662)itmp
        endif
        CFVER=itmp
        fok=.true.
        CFMCM=MCMALL(i)
        exit
 6662 continue

      if (fok) then
        IER=0
      else
        IER=ISTAT
        goto 666
      endif

 999  RETURN

 666  write(outs,'(2a)')'Error opening weather file ',
     &  LCLIM(1:lnblnk(LCLIM))
      call edisp(IUOUT,outs)
      goto 999

      END

C ********** CLMARR **********
C Arranges the column order of weather parameters, aiming for as
C close to the parameter order (defined in climate.h) as possible.
C Returns a boolean hasarr signalling if rearrangement was needed,
C and the new column numbers in cmcolarr (this will be the same
C as CMCOL if hasarr=.false.)

      SUBROUTINE CLMARR(hasarr,cmcolarr)

#include "climate.h"

      logical hasarr
      integer cmcolarr(MCM)

      hasarr=.false.

      icol=0
      do imet=1,MCM
        if (CMXST(imet)) then
          icol=icol+1
          if (CMCOL(imet).ne.icol) then
            if (.not.hasarr) hasarr=.true.
          endif
          cmcolarr(imet)=icol
        else
          cmcolarr(imet)=0
        endif
      enddo

      return
      end
