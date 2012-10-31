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

C ************ RRITE
C 'RRITE' allows one day of hourly values (of the 6 weather
C parameters to be edtied by the user and merged into the
C weather file.
      SUBROUTINE RRITE
#include "esprdbfile.h"
      PARAMETER (MT=24)
      COMMON/SPAD/MMOD,LIMIT,LIMTTY
      COMMON/OUTIN/IUOUT,IUIN
      COMMON/RADTYP/IDNGH
      common/appw/iappw,iappx,iappy
      integer menuchw,igl,igr,igt,igb,igw,igwh
      COMMON/VIEWPX/menuchw,igl,igr,igt,igb,igw,igwh

      integer NDAY
      real CDIF,CTMP,CDNR,CVEL,CDIR,CHUM
      COMMON/CLMDAT/NDAY,CDIF(MT),CTMP(MT),CDNR(MT),CVEL(MT),
     A              CDIR(MT),CHUM(MT)

      COMMON/CLMDT1/CLMLOC
      COMMON/CLMSET/ICYEAR,ICDNGH,CLAT,CLONG
      COMMON/SET1/IYEAR,IBDOY,IEDOY,IFDAY,IFTIME
      DIMENSION item(31),holdd(24)
      CHARACTER*30 CLMLOC
      CHARACTER outs*124,item*40,holdd*34,hold*36,key*1
      character head*16
      logical ok,dok,altered
      integer onleft  ! number of left axis
      integer onright ! number of right axis
      integer mitem,INO  ! max items and current menu item

#ifdef OSI
      integer iigl,iigr,iigt,iigb,iigw,iigwh
      integer iiw1,iiw2,iiw3,iiw4,iimenu
#else
      integer*8 iigl,iigr,iigt,iigb,iigw,iigwh
      integer*8 iiw1,iiw2,iiw3,iiw4,iimenu
#endif

C For help messages
      character helpinsub*24,helptopic*24
      integer nbhelp     ! number of help lines found

      helpinsub='RRITE'  ! set for MAIN
      altered=.false.
      onleft = 2      ! assume all axes to be drawn
      onright = 3
  31  CONTINUE

C Day to be transferred.
      call edisp(iuout,' Single day to be transferred -')
      IDAY=1
      call ONEDAY(IFDAY,IDAY,IOD,IOM,IER)

C Read in the current data so partial editing possible.
      CALL CLMGET(IDAY)

C Determine the record in the weather file.
      IREC=IDAY
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
        call clmdrwd(iday,onleft,onright)
      endif

      write(item(1),'(2a)') '  ',clmloc
      item(2)=  '  hr  radiation  dry blb  RH   wind     '

C If IDNGH=123 then this is global horizontal.
      IF(IDNGH.EQ.0)THEN
        item(3)='      diff dir N  temp    %  direc speed'
      ELSEIF(IDNGH.EQ.123)THEN
        item(3)='      diff glo H  temp    %  direc speed'
      ENDIF
      do 40 I=1,24
        write (holdd(i),'(2i6,f6.1,2i5,f6.1)') INT(CDIF(I)),
     &    INT(CDNR(I)),CTMP(I),INT(CHUM(I)),INT(CDIR(I)),CVEL(I)
        CALL EMKEY(I,KEY,IER)
        write (item(i+3),'(a1,i3,a34)')key,i,holdd(i)
  40  continue
      item(28)= ' _______________________________________'
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
          dok=.true.
          helptopic='ok_to_update'
          call gethelptext(helpinsub,helptopic,nbhelp)
          CALL ASKOK(' ','Update database?',OK,dok,nbhelp)
          if(OK)then
            call CLMPUT(IDAY)
            IREC=366
            IADN=IDNGH
            WRITE(ICLIM,REC=IREC,IOSTAT=ISTAT,ERR=1000)IYEAR,IADN
            IREC=367
            WRITE(ICLIM,REC=IREC,IOSTAT=ISTAT,ERR=1000)CLMLOC
            IREC=368
            WRITE(ICLIM,REC=IREC,IOSTAT=ISTAT,ERR=1000)CLAT,CLONG
          endif
          altered=.false.
        endif
        dok=.false.
        helptopic='one_day_at_a_time'
        call gethelptext(helpinsub,helptopic,nbhelp)
        CALL ASKOK(' ','Continue with another period?',OK,dok,nbhelp)
        IF(OK)GOTO 31

C Reset to the normal graphics area before exiting.
        if(MMOD.eq.8)then
          if(iappw.gt.0.and.iappw.lt.100)then
            menuchw = MAX0(int(27*iappw*0.01),18)
            LIMTTY= MAX0(int(8*iappw*0.01),4)
            LIMIT = MAX0(int(8*iappw*0.01),4)
          else
            menuchw = 27
            LIMTTY=8
            LIMIT =8
          endif
          iglib = igraphiclib()  ! find out if X11 or GTK or text support only.
          if(iglib.eq.1)then
            call winclr
            CALL feedbox(menuchw,2,igfw,igfh)
            CALL opengdisp(menuchw,LIMTTY,2,igdw,igdh)
          endif

C Setup and pass in parameters to win3d.
          iiw1=10; iiw2=22; iiw3=7; iiw4=3; iimenu=menuchw
          iigl=igl; iigr=igr; iigt=igt; iigb=igb; iigw=igw; iigwh=igwh
          CALL win3d(iimenu,iiw1,iiw2,iiw3,iiw4,
     &      iigl,iigr,iigt,iigb,iigw,iigwh)
C          CALL win3d(menuchw,10,22,7,3,igl,igr,igt,igb,igw,igwh)
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
        IREC=366
        IADN=IDNGH
        WRITE(ICLIM,REC=IREC,IOSTAT=ISTAT,ERR=1000)IYEAR,IADN
        IREC=367
        WRITE(ICLIM,REC=IREC,IOSTAT=ISTAT,ERR=1000)CLMLOC
        IREC=368
        WRITE(ICLIM,REC=IREC,IOSTAT=ISTAT,ERR=1000)CLAT,CLONG
        altered=.false.

C Check if user wishes to edit another day.
        dok=.false.
        helptopic='one_day_at_a_time'
        call gethelptext(helpinsub,helptopic,nbhelp)
        CALL ASKOK(' ','Continue with another period?',OK,dok,nbhelp)
        IF(OK)GOTO 31
      elseif(INO.gt.3.and.INO.lt.29)then
        ifoc=ino-3
        WRITE(HOLD,'(a)')holdd(ifoc)
        IF(IDNGH.EQ.0)THEN
          write(outs,'(a,i2,a)')'At hour ',ifoc,
     &    ' diff rad, direct N rad, db temp, RH %, wind direc & speed'
        ELSEIF(IDNGH.EQ.123)THEN
          write(outs,'(a,i2,a)')'At hour ',ifoc,
     &     ' diff rad, glob hor rad, db temp, RH %, wind direc & speed'
        ENDIF
        helptopic='one_day_data_summary'
        call gethelptext(helpinsub,helptopic,nbhelp)
  42    CALL EASKS(HOLD,outs,' ',36,' 0. 0. 10. 10 0 0.0 ','clm data',
     &    IER,nbhelp)
        K=0
        CALL EGETWR(HOLD,K,CDIF(IFOC),0.0,1353.0,'W','dif sol',IER)
        CALL EGETWR(HOLD,K,CDNR(IFOC),0.0,1353.0,'W','dir sol',IER)
        CALL EGETWR(HOLD,K,CTMP(IFOC),-30.0,60.0,'W','db tmp',IER)
        CALL EGETWR(HOLD,K,CHUM(IFOC),0.0,100.0,'W','RH',IER)
        CALL EGETWR(HOLD,K,CDIR(IFOC),-359.0,359.0,'W','w dir',IER)
        CALL EGETWR(HOLD,K,CVEL(IFOC),0.0,60.0,'W','w vel',IER)
        if(ier.ne.0)goto 42
        altered=.true.
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
      COMMON/FILEP/IFIL
      COMMON/RADTYP/IDNGH

      COMMON/CLMDT1/CLMLOC
      COMMON/CLMSET/ICYEAR,ICDNGH,CLAT,CLONG
      COMMON/SET1/IYEAR,IBDOY,IEDOY,IFDAY,IFTIME
      CHARACTER CLMLOC*30,T30*30,outs*124

C For help messages
      character helpinsub*24,helptopic*24
      integer nbhelp     ! number of help lines found

      helpinsub='SITEDIT'  ! set for MAIN

C Ask for the year (2001 convenintly has monday on 1 Jan).
      helptopic='what_is_the_year'
      call gethelptext(helpinsub,helptopic,nbhelp)
      CALL EASKI(IYEAR,' ',' What is the year ? ',
     &         1900,'F',2015,'W',2001,'year',IER,nbhelp)
      ICYEAR=IYEAR

C Now transfer to the weather file.
      IREC=366
      IADN=IDNGH
      WRITE(IFIL,REC=IREC,IOSTAT=ISTAT,ERR=1000)IYEAR,IADN

C Determine the weather data location.
      T30=CLMLOC
      helptopic='what_is_descr'
      call gethelptext(helpinsub,helptopic,nbhelp)
      CALL EASKS(T30,' ',' Weather site name ? ',
     &    30,'North_Pole','weather site. ',IER,nbhelp)
      if(T30(1:2).ne.'  ') CLMLOC=T30
      IREC=367
      WRITE(IFIL,REC=IREC,IOSTAT=ISTAT,ERR=1000)CLMLOC

      helptopic='what_is_latitude'
      call gethelptext(helpinsub,helptopic,nbhelp)
      CALL EASKR(CLAT,' ','Weather site latitude:',
     &  -89.9,'W',89.9,'W',30.0,'weather latitude',IER,nbhelp)

      helptopic='what_is_long_diff'
      call gethelptext(helpinsub,helptopic,nbhelp)
      CALL EASKR(CLONG,' ','Weather site longitude difference:',
     &  -14.9,'W',14.9,'W',0.0,'weather long diff',IER,nbhelp)
      IREC=368
      WRITE(IFIL,REC=IREC,IOSTAT=ISTAT,ERR=1000)CLAT,CLONG

C Transfer all climatic data to file.
      IREC=366
      IADN=IDNGH
      WRITE(ICLIM,REC=IREC,IOSTAT=ISTAT,ERR=1000)IYEAR,IADN
      IREC=367
      WRITE(ICLIM,REC=IREC,IOSTAT=ISTAT,ERR=1000)CLMLOC
      IREC=368
      WRITE(ICLIM,REC=IREC,IOSTAT=ISTAT,ERR=1000)CLAT,CLONG

   36 RETURN

1000  write(outs,'(A,I4)')' Problem writing site data on rec ',IREC
      call usrmsg(outs,' returning....','W')
      goto 36

      END
