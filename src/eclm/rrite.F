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

C Subroutines in this file:
C  RRITE: allows one day of hourly values to be edited
C  SITEDIT: edits site related data in a weather file.

C ************ RRITE
C 'RRITE' allows one day of hourly values of weather
C parameters to be edtied by the user and merged into the
C weather file.
      SUBROUTINE RRITE
#include "esprdbfile.h"
#include "climate.h"
#include "help.h"

      COMMON/SPAD/MMOD,LIMIT,LIMTTY
      COMMON/OUTIN/IUOUT,IUIN,IEOUT
      COMMON/RADTYP/IDNGH
      common/appw/iappw,iappx,iappy
      integer menuchw,igl,igr,igt,igb,igw,igwh
      COMMON/VIEWPX/menuchw,igl,igr,igt,igb,igw,igwh
      COMMON/CLMDT1/CLMLOC
      COMMON/CLMSET/ICYEAR,ICDNGH,CLAT,CLONG
      COMMON/SET1/IYEAR,IBDOY,IEDOY,IFDAY,IFTIME
      COMMON/PERC/ID1,IM1,IT1,ID2,IM2,IT2,IDS,IDF,INEW
      DIMENSION IPICK(MCM)
      DIMENSION item(31),holdd(24)
      CHARACTER*42 CLMLOC
      CHARACTER outs*124,item*32,hold*10,key*1
      character head*16
      logical ok,altered,mxmn
      integer onleft  ! number of left axis
      integer onright ! number of right axis
      integer mitem,INO  ! max items and current menu item

#ifdef OSI
      integer iigl,iigr,iigt,iigb,iigw,iigwh
      integer iiw1,iiw2,iiw3,iiw4,iimenu
      integer ilf,igfw,igfh,ild,igdw,igdh
#else
      integer*8 iigl,iigr,iigt,iigb,iigw,iigwh
      integer*8 iiw1,iiw2,iiw3,iiw4,iimenu
      integer*8 ilf,igfw,igfh,ild,igdw,igdh
#endif

      helpinsub='rrite'  ! set for MAIN
      altered=.false.
  31  CONTINUE

C Day to be transferred.
      call edisp(iuout,' Single day to be transferred -')
      IDAY=1
      call ONEDAY(IFDAY,IDAY,IOD,IOM,IER)

C Read in the current data so partial editing possible.
      CALL CLMGET(IDAY,IER)

C Determine the record in the weather file.
      IREC=IDAY

C Set current metric.
      icurmet=1

  18  continue
      INO=-3
      if(MMOD.eq.8)then

C Because the menu is wider, resize the graphics area so that
C more of the graph can be seen.
        if(iappw.gt.0.and.iappw.lt.100)then
          menuchw = MAX0(int(43*iappw*0.01),18)
          LIMTTY= MAX0(int(8*iappw*0.01),4)
          LIMIT = MAX0(int(8*iappw*0.01),4)
        else
          menuchw = 43
          LIMTTY=8
          LIMIT =8
        endif

C Save period in commons and set to selected day.
        iid1=ID1; iid2=ID2; iim1=IM1; iim2=IM2
        iit1=IT1; iit2=IT2; iids=IDS; iidf=IDF
        ID1=IOD ; ID2=IOD ; IM1=IOM ; IM2=IOM
        IT1=1   ; IT2=24  ; IDS=IDAY; IDF=IDAY

C Select only the current metric.
        do imet=1,MCM
          if (CMXST(imet) .and. imet.eq.icurmet) then
            IPICK(imet)=1
          else
            IPICK(imet)=0
          endif
        enddo
        mxmn=.true.
        onleft=0
        if (IPICK(1).eq.1) onleft=onleft+1
        if (IPICK(5).eq.1) onleft=onleft+1
        if (IPICK(10).eq.1) onleft=onleft+1
        onright=0
        if (IPICK(2).eq.1 .or. IPICK(3).eq.1 .or. IPICK(4).eq.1) 
     &    onright=onright+1
        if (IPICK(6).eq.1) onright=onright+1
        if (IPICK(7).eq.1) onright=onright+1
        if (IPICK(8).eq.1 .or. IPICK(9).eq.1) onright=onright+1
        call CLMDRW(IPICK,mxmn,onleft,onright,'-')

C Restore saved period to commons.
        ID1=iid1; ID2=iid2; IM1=iim1; IM2=iim2
        IT1=iit1; IT2=iit2; IDS=iids; IDF=iidf
      endif

      write(item(1),'(2a)') '  ',clmloc(1:30)
      ln=lnblnk(CMNAMF(icurmet))
      write(item(2),'(2a)')'1 ',CMNAMF(icurmet)(1:ln)
      write(item(3),'(2a)')'  hr    ',CMUNIT(icurmet)

      do 40 I=1,24
        CALL EMKEY(I,KEY,IER)
        write (item(i+3),'(a1,i3,f10.1)')key,i,CMRVAL(icurmet,I)
  40  continue
      item(28)= ' _____________________________ '
      item(29)= '> update database'
      item(30)= '? help'
      item(31)= '- exit'
      mitem=31
      write(head,'(a,i3)') 'data for day ',IDAY
      call usrmsg(' ',' ','-')

      CALL EMENU(HEAD,ITEM,mitem,INO)
      if(INO.eq.mitem)then

C Check if user wishes to edit another day.
        if(altered)then
          helptopic='ok_to_update'
          call gethelptext(helpinsub,helptopic,nbhelp)
          CALL EASKOK(' ','Update database?',OK,nbhelp)
          if(OK)then
            call CLMPUT(IDAY)
            if (CFVER.eq.1) then
              IREC=366
              IADN=IDNGH
              WRITE(ICLIM,REC=IREC,IOSTAT=ISTAT,ERR=1000)CFYEAR,IADN
              IREC=367
              WRITE(ICLIM,REC=IREC,IOSTAT=ISTAT,ERR=1000)CFLOC
              IREC=368
              WRITE(ICLIM,REC=IREC,IOSTAT=ISTAT,ERR=1000)CFLAT,CFLONG
            else
              IREC=366
              WRITE(ICLIM,REC=IREC,IOSTAT=ISTAT,ERR=1000)
     &          CFYEAR
              IREC=IREC+1
              WRITE(ICLIM,REC=IREC,IOSTAT=ISTAT,ERR=1000)
     &          CFLOC,CFLAT,CFLONG
              IREC=IREC+1
              WRITE(ICLIM,REC=IREC,IOSTAT=ISTAT,ERR=1000)
     &          (CMCOL(i),i=1,CFMCM)
              IREC=IREC+1
              WRITE(ICLIM,REC=IREC,IOSTAT=ISTAT,ERR=1000)
     &          CFVER
            endif
          endif
          altered=.false.
        endif
        helptopic='one_day_at_a_time'
        call gethelptext(helpinsub,helptopic,nbhelp)
        CALL EASKOK(' ','Continue with another period?',OK,nbhelp)
        IF(OK)GOTO 31

C Reset to the normal graphics area before exiting.
        if(MMOD.eq.8)then
          if(iappw.gt.0.and.iappw.lt.100)then
            menuchw = MAX0(int(28*iappw*0.01),18)
            LIMTTY= MAX0(int(8*iappw*0.01),4)
            LIMIT = MAX0(int(8*iappw*0.01),4)
          else
            menuchw = 28
            LIMTTY=8
            LIMIT =8
          endif

C Setup and pass in parameters to win3d.
          iiw1=10; iiw2=22; iiw3=7; iiw4=3; iimenu=menuchw
          iigl=igl; iigr=igr; iigt=igt; iigb=igb; iigw=igw; iigwh=igwh
          ilf=2; ild=LIMTTY
          iglib = igraphiclib()  ! find out if X11 or GTK or text support only.
          if(iglib.eq.1)then
            call winclr
            CALL feedbox(iimenu,ilf,igfw,igfh)
            CALL opengdisp(iimenu,ild,ilf,igdw,igdh)
            call opencpw
            call opensetup
          endif
          CALL win3d(iimenu,iiw1,iiw2,iiw3,iiw4,
     &      iigl,iigr,iigt,iigb,iigw,iigwh)
          igl=int(iigl); igr=int(iigr); igt=int(iigt); igb=int(iigb)
          igw=int(iigw); igwh=int(iigwh)
        endif
        return
      elseif(INO.eq.mitem-1)then
        helptopic='edit_hourly_menu'
        call gethelptext(helpinsub,helptopic,nbhelp)
        CALL PHELPD('clm data edit',nbhelp,'-',0,0,IER)
      elseif(INO.eq.mitem-2)then

C Transfer all climatic data to file.
        call CLMPUT(IDAY)
        if (CFVER.eq.1) then
          IREC=366
          IADN=IDNGH
          WRITE(ICLIM,REC=IREC,IOSTAT=ISTAT,ERR=1000)CFYEAR,IADN
          IREC=367
          WRITE(ICLIM,REC=IREC,IOSTAT=ISTAT,ERR=1000)CFLOC
          IREC=368
          WRITE(ICLIM,REC=IREC,IOSTAT=ISTAT,ERR=1000)CFLAT,CFLONG
        else
          IREC=366
          WRITE(ICLIM,REC=IREC,IOSTAT=ISTAT,ERR=1000)
     &       CFYEAR
          IREC=IREC+1
          WRITE(ICLIM,REC=IREC,IOSTAT=ISTAT,ERR=1000)
     &       CFLOC,CFLAT,CFLONG
          IREC=IREC+1
          WRITE(ICLIM,REC=IREC,IOSTAT=ISTAT,ERR=1000)
     &       (CMCOL(i),i=1,CFMCM)
          IREC=IREC+1
          WRITE(ICLIM,REC=IREC,IOSTAT=ISTAT,ERR=1000)
     &       CFVER
        endif
        altered=.false.

C Check if user wishes to edit another day.
        helptopic='one_day_at_a_time'
        call gethelptext(helpinsub,helptopic,nbhelp)
        CALL EASKOK(' ','Continue with another period?',OK,nbhelp)
        IF(OK)GOTO 31
      elseif(INO.gt.3.and.INO.lt.29)then
        ifoc=ino-3
        WRITE(HOLD,'(f10.1)')CMRVAL(icurmet,ifoc)
        ln=lnblnk(CMNAMF(icurmet))
        write(outs,'(2a,i2)')CMNAMF(icurmet)(1:ln),' at hour ',ifoc
        helptopic='one_day_data_summary'
        call gethelptext(helpinsub,helptopic,nbhelp)
  42    CALL EASKS(HOLD,outs,CMUNIT(icurmet),10,' 0. ','clm datum',
     &    IER,nbhelp)
        K=0
        if (icurmet.eq.1 .or. icurmet.eq.5) then
          rtmpmax=real(CFMAX(icurmet))/10.
          rtmpmin=real(CFMIN(icurmet))/10.
        else          
          rtmpmax=real(CFMAX(icurmet))
          rtmpmin=real(CFMIN(icurmet))
        endif
        CALL EGETWR(HOLD,K,CMRVAL(icurmet,ifoc),rtmpmin,rtmpmax,'W',
     &    'clm datum',IER)
        if(ier.ne.0)goto 42
        altered=.true.

C Change focus metric.
      elseif (ino.eq.2) then
        call edisp(iuout,'Available weather metrics:')
        do imet=1,MCM
          if (CMXST(imet)) then
            write(outs,'(i2.2,1x,a)')imet,CMNAMF(imet)
            call EDISP(iuout,outs(1:lnblnk(outs)))
          endif
        enddo
        if (CFVER.eq.1) then
          itmp=7
        else
          itmp=CFMCM
        endif
        helptopic='pick_a_metric'
        call gethelptext(helpinsub,helptopic,nbhelp)
  295   call EASKI(icurmet,'See text feedback for available metrics',
     &    'Metric index:',1,'F',itmp,'F',1,'focus metric',IER,nbhelp)
        if (.not.CMXST(icurmet)) then
          call usrmsg('This metric not available.',
     &      'Please choose another.','W')
          goto 295
        endif
      else
        ino= -1
        goto 18
      endif
      ino= -2
      goto 18

   36 RETURN

1000  write(outs,'(A,I4)')' Problem writing weather data on rec ',IREC
      call usrmsg(outs,' returning....','W')
      goto 36

      END

C ********** SITEDIT
C SITEDIT edits site related data in a weather file.
      SUBROUTINE SITEDIT
#include "esprdbfile.h"
#include "climate.h"
#include "help.h"

      COMMON/FILEP/IFIL
      COMMON/RADTYP/IDNGH

      COMMON/CLMDT1/CLMLOC
      COMMON/CLMSET/ICYEAR,ICDNGH,CLAT,CLONG
      COMMON/SET1/IYEAR,IBDOY,IEDOY,IFDAY,IFTIME
      CHARACTER CLMLOC*42,T30*42,outs*124

      helpinsub='rrite'  ! set for MAIN

C Ask for the year (2007 convenintly has monday on 1 Jan).
      helpinsub='common' ! get from common.help
      helptopic='what_is_the_year'
      call gethelptext(helpinsub,helptopic,nbhelp)
      helpinsub='rrite'  ! reset for SITEDIT
      CALL EASKI(IYEAR,' ','Year',
     &         1900,'F',2051,'W',2019,'year',IER,nbhelp)
      CFYEAR=IYEAR

C Determine the weather data location.
      T30=CFLOC
      helptopic='what_is_descr'
      call gethelptext(helpinsub,helptopic,nbhelp)
      CALL EASKS(T30,' ','Weather station name',
     &  42,'North_Pole','weather site. ',IER,nbhelp)
      if(T30(1:2).ne.'  ') CFLOC=T30

      helpinsub='common' ! get from common.help
      helptopic='clm_latitude_match'
      call gethelptext(helpinsub,helptopic,nbhelp)
      helpinsub='rrite'  ! reset for SITEDIT
      CALL EASKR(CFLAT,' ','Station latitude?',
     &  -89.9,'W',89.9,'W',30.0,'weather latitude',IER,nbhelp)

      helpinsub='common' ! get from common.help
      helptopic='define_longitude_diff'
      call gethelptext(helpinsub,helptopic,nbhelp)
      helpinsub='rrite'  ! reset for SITEDIT
      CALL EASKR(CFLONG,' ',
     &  'Station longitude difference?',
     &  -14.9,'W',14.9,'W',0.0,'weather long diff',IER,nbhelp)

C Transfer weather metadata to file.
      if (CFVER.eq.1) then
        IREC=366
        IADN=IDNGH
        WRITE(ICLIM,REC=IREC,IOSTAT=ISTAT,ERR=1000)CFYEAR,IADN
        IREC=367
        WRITE(ICLIM,REC=IREC,IOSTAT=ISTAT,ERR=1000)CFLOC
        IREC=368
        WRITE(ICLIM,REC=IREC,IOSTAT=ISTAT,ERR=1000)CFLAT,CFLONG
      else
        IREC=366
        WRITE(ICLIM,REC=IREC,IOSTAT=ISTAT,ERR=1000)
     &     CFYEAR
        IREC=IREC+1
        WRITE(ICLIM,REC=IREC,IOSTAT=ISTAT,ERR=1000)
     &     CFLOC,CFLAT,CFLONG
        IREC=IREC+1
        WRITE(ICLIM,REC=IREC,IOSTAT=ISTAT,ERR=1000)
     &     (CMCOL(i),i=1,CFMCM)
        IREC=IREC+1
        WRITE(ICLIM,REC=IREC,IOSTAT=ISTAT,ERR=1000)
     &     CFVER
      endif

      CALL CLMMDN2O
   36 RETURN

1000  write(outs,'(A,I4)')' Problem writing site data on rec ',IREC
      call usrmsg(outs,' returning....','W')
      goto 36

      END
