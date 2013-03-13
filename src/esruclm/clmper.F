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

C CLMDAY: defines a restricted period during a day for analysis
C         puropses. 
C ONEDAY: Provides interface to specification of one day.
C SELPER: Menu for period selection - user defined, monthly, typical etc.
C CLMPER: Provides interface to specification of a period of days.
C editemclimatelist - edit/create an item for climatelist file.
C mkitemclimatelist - write an item for climatelist file.
C DDRADSUM - looks for typical week in each season based on closest
C     degree days and radiation patterns.

C----------------------------------------

C CLMDAY: defines a restricted period during a day for analysis
C puropses. 
      SUBROUTINE CLMDAY(IER)
      COMMON/DAYSF/KDS,KDF
      CHARACTER hold*24

C For help messages
      character helpinsub*24,helptopic*24 ! strings for help
      integer nbhelp     ! number of help lines found

      helpinsub='CLMDAY'  ! set for cfiles
      helptopic='period_at_least'
      call gethelptext(helpinsub,helptopic,nbhelp)
      write(HOLD,'(I6,I4)') KDS,KDF
 10   CALL EASKS(HOLD,' ',' Start and end hour: ',
     &     24,' 1  24 ','start and end hour',IER,1)
      K=0
      CALL EGETWI(HOLD,K,KDS,1,23,'F','start',IER)
      CALL EGETWI(HOLD,K,KDF,KDS,24,'F','end',IER)
      if(ier.ne.0)then
        call usrmsg(' Problem getting the time',
     &              ' Please try again...','W')
        goto 10
      endif

      RETURN
      END

C ************* ONEDAY
C ONEDAY: Provides interface to specification of one day. It
C returns ID based on the current settings of IFDAY. 
      SUBROUTINE ONEDAY(IFDAY,IDAY,ID,IM,IER)
      CHARACTER hold*24

C For help messages
      character helpinsub*24,helptopic*24 ! strings for help
      integer nbhelp     ! number of help lines found

      helpinsub='ONEDAY'  ! set for cfiles

      IER=0
      helptopic='period_within_calendar'
      call gethelptext(helpinsub,helptopic,nbhelp)
  281 IF(IFDAY.EQ.0)THEN
        CALL EASKI(IDAY,' ',' d-o-y ? ',
     &             1,'F',365,'F',1,'start day',IER,nbhelp)
        CALL EDAYR(IDAY,ID,IM)
      ELSE
        CALL EDAYR(IDAY,ID,IM)
        write(HOLD,'(I6,I4)') ID,IM
 283    CALL EASKS(HOLD,' ',' Day-of-month & month: ',
     &     24,' 1  1 ','doy, month',IER,nbhelp)
        K=0
        CALL EGETWI(HOLD,K,ID,1,31,'F','day of month',IER)
        CALL EGETWI(HOLD,K,IM,1,12,'F','month',IER)
        IF(IER.ne.0)goto 283

C Check range, then convert to IDS.
        CALL EDAYCH(ID,IM,IERR)
        IF(IERR.EQ.1)goto 281
        CALL EDAY(ID,IM,IDAY)
      ENDIF

      RETURN
      END

C ************* SELPER
C SELPER: Menu for period selection - user defined, monthly, typical
C week or season (scanned from climatelist file).
      subroutine selper(ier)

C Manual inclusion of a parameter from building.h
      parameter (MSPS=10)
      parameter (MIPVA=MSPS)

#include "seasons.h"
C seasons.h provides typper and typsea

      COMMON/OUTIN/IUOUT,IUIN
      COMMON/SET1/IYEAR,IBDOY,IEDOY,IFDAY,IFTIME
      COMMON/PERC/ID1,IM1,IT1,ID2,IM2,IT2,IDS,IDF,INEW
      COMMON/CLMSET/ICYEAR,ICDNGH,CLAT,CLONG
      COMMON/PERPER/PERST2
      dimension pmenu(29)
      CHARACTER pmenu*24
      character PERST1*14,PERST2*44,PERST3*44
      integer NITEM,INO  ! max items and current menu item

C For help messages
      character helpinsub*24,helptopic*24 ! strings for help
      integer nbhelp     ! number of help lines found

      helpinsub='SELPER'  ! set for cfiles

C Assume whole day requested.
      ier=0
      IT1=1
      IT2=24
      pmenu(1) ='a User defined          '
      pmenu(2) ='b January               '
      pmenu(3) ='c February              '
      pmenu(4) ='d March                 '
      pmenu(5) ='e April                 '
      pmenu(6) ='f May                   '
      pmenu(7) ='g June                  '
      pmenu(8) ='h July                  '
      pmenu(9) ='i August                '
      pmenu(10)='j September             '
      pmenu(11)='k October               '
      pmenu(12)='l November              '
      pmenu(13)='m December              '
      pmenu(13)='n December              '
      pmenu(14)='  _____________________ '

C Adjust display if in southern hemisphere.
      if(CLAT.lt.0.0)then
        pmenu(15)='o Typical summer week   '
        pmenu(16)='p Typical autumn week   '
        pmenu(17)='q Typical winter week   '
        pmenu(18)='r Typical spring week   '
        pmenu(19)='s Typical summer week   '
        pmenu(20)='  _____________________ '
        pmenu(21)='t Summer (1st) season   '
        pmenu(22)='u Autumn season         '
        pmenu(23)='v Winter season         '
        pmenu(24)='w Spring season         '
        pmenu(25)='x Summer (2nd) season   '
      else
        pmenu(15)='o Typical winter week   '
        pmenu(16)='p Typical spring week   '
        pmenu(17)='q Typical summer week   '
        pmenu(18)='r Typical autumn week   '
        pmenu(19)='s Typical winter week   '
        pmenu(20)='  _____________________ '
        pmenu(21)='t Winter (1st) season   '
        pmenu(22)='u Spring season         '
        pmenu(23)='v Summer season         '
        pmenu(24)='w Autumn season         '
        pmenu(25)='x Winter (2nd) season   '
      endif
      pmenu(26)='y Whole year            '
      pmenu(27)='  _____________________ '
      pmenu(28)='? Help                  '
      pmenu(29)='- Exit                  '
      NITEM=29
      INO=-3
   4  CALL EMENU('  Periods',pmenu,NITEM,INO)
      if(INO.EQ.1)then
        call clmper(IFDAY,ier)
        return
      elseif(INO.EQ.2)then
        ID1=1; IM1=1
        ID2=31; IM2=1
      elseif(INO.EQ.3)then
        ID1=1; IM1=2
        ID2=28; IM2=2
      elseif(INO.EQ.4)then
        ID1=1; IM1=3
        ID2=31; IM2=3
      elseif(INO.EQ.5)then
        ID1=1; IM1=4
        ID2=30; IM2=4
      elseif(INO.EQ.6)then
        ID1=1; IM1=5
        ID2=31; IM2=5
      elseif(INO.EQ.7)then
        ID1=1; IM1=6
        ID2=30; IM2=6
      elseif(INO.EQ.8)then
        ID1=1; IM1=7
        ID2=31; IM2=7
      elseif(INO.EQ.9)then
        ID1=1; IM1=8
        ID2=31; IM2=8
      elseif(INO.EQ.10)then
        ID1=1; IM1=9
        ID2=30; IM2=9
      elseif(INO.EQ.11)then
        ID1=1; IM1=10
        ID2=31; IM2=10
      elseif(INO.EQ.12)then
        ID1=1; IM1=11
        ID2=30; IM2=11
      elseif(INO.EQ.13)then
        ID1=1; IM1=12
        ID2=31; IM2=12
      elseif(INO.EQ.15)then
        CALL EDAYR(ia1wins,ID1,IM1)
        CALL EDAYR(ia1winf,ID2,IM2)
      elseif(INO.EQ.16)then
        CALL EDAYR(ia1sprs,ID1,IM1)
        CALL EDAYR(ia1sprf,ID2,IM2)
      elseif(INO.EQ.17)then
        CALL EDAYR(iasums,ID1,IM1)
        CALL EDAYR(iasumf,ID2,IM2)
      elseif(INO.EQ.18)then
        CALL EDAYR(ia2sprs,ID1,IM1)
        CALL EDAYR(ia2sprf,ID2,IM2)
      elseif(INO.EQ.19)then
        CALL EDAYR(ia2wins,ID1,IM1)
        CALL EDAYR(ia2winf,ID2,IM2)
      elseif(INO.EQ.21)then
        CALL EDAYR(is1wins,ID1,IM1)
        CALL EDAYR(is1winf,ID2,IM2)
      elseif(INO.EQ.22)then
        CALL EDAYR(is1sprs,ID1,IM1)
        CALL EDAYR(is1sprf,ID2,IM2)
      elseif(INO.EQ.23)then
        CALL EDAYR(is1sums,ID1,IM1)
        CALL EDAYR(is1sumf,ID2,IM2)
      elseif(INO.EQ.24)then
        CALL EDAYR(is2sprs,ID1,IM1)
        CALL EDAYR(is2sprf,ID2,IM2)
      elseif(INO.EQ.25)then
        CALL EDAYR(is2wins,ID1,IM1)
        CALL EDAYR(is2winf,ID2,IM2)
      elseif(INO.EQ.26)then
        ID1=1; IM1=1
        ID2=31; IM2=12
      elseif(INO.EQ.28)then

C Help.
        helptopic='period_user_defined'
        call gethelptext(helpinsub,helptopic,nbhelp)
        CALL PHELPD('clm periods',nbhelp,'-',0,0,IER)
        ino=-1
        goto 4
      elseif(INO.EQ.29)then

C If no option is selected, select whole year
        ID1=1; IM1=1
        ID2=31; IM2=12
      else
        ino=-1
        goto 4
      endif

C Convert periods into julian before returning.
      INEW=1
      CALL EDAY(ID1,IM1,IIDS)
      CALL EDAY(ID2,IM2,IIDF)
      IDS=IIDS
      IDF=IIDF
      NTSPH=1
      CALL EPERSTR(IYEAR,IIDS,IT1,IIDF,IT2,NTSPH,
     &             IFDAY,IFTIME,PERST1,PERST2,PERST3,IER)
      call edisp(iuout,'The period selected is... ')
      call edisp(iuout,PERST2)
      return
      end

C ************* CLMPER
C CLMPER: Provides interface to specification of a period of days. It
C returns IDS and IDF based on the current settings of IFDAY. 
      SUBROUTINE CLMPER(IFDAY,IER)
      COMMON/PERC/ID1,IM1,IT1,ID2,IM2,IT2,IDS,IDF,INEW
      common/pophelp/h(60)
      CHARACTER H*72,hold*24
      
      integer ier  ! for error state
      integer ifday  ! indicates the format for days.

C For help messages
      character helpinsub*24,helptopic*24 ! strings for help
      integer nbhelp     ! number of help lines found

      helpinsub='CLMPER'  ! set for cfiles

      IER=0
      helptopic='period_output_time'
      call gethelptext(helpinsub,helptopic,nbhelp)

  281 IF(IFDAY.EQ.0)THEN
        write(HOLD,'(I6,I4)') IDS,IT1
        CALL EASKS(HOLD,' ',' Start Day-of-year & time: ',
     &     24,' 1  1 ','start doy and time',IER,nbhelp)
        K=0
        CALL EGETWI(HOLD,K,IDS,1,365,'F','start day of year',IER)
        CALL EGETWI(HOLD,K,IT1,0,24,'F','hour',IER)
        if(IER.ne.0)goto 281
        CALL EDAYR(IDS,ID1,IM1)

        write(HOLD,'(I6,I4)') IDF,IT2
        CALL EASKS(HOLD,' ',' End Day-of-year & time: ',
     &     24,' 364  24 ','end doy and time',IER,nbhelp)
        K=0
        CALL EGETWI(HOLD,K,IDF,1,365,'F','start day of year',IER)
        CALL EGETWI(HOLD,K,IT2,0, 24,'F','hour',IER)
        if(IER.ne.0)goto 281
        CALL EDAYR(IDF,ID2,IM2)
      ELSE
        CALL EDAYR(IDS,ID1,IM1)
        write(HOLD,'(I6,I4,I4)') ID1,IM1,IT1
        H(6)='Example:  6th March at 9h00 am is given as 6 3 9 '
 283    CALL EASKS(HOLD,' ',' Start Day-of-month, month & time: ',
     &     24,' 1  1  7.0 ','doy, month time',IER,nbhelp)
        K=0
        CALL EGETWI(HOLD,K,ID1,1,31,'F','start day of month',IER)
        CALL EGETWI(HOLD,K,IM1,1,12,'F','start month',IER)
        CALL EGETWI(HOLD,K,IT1,0,24,'F','time',IER)
        if(IER.ne.0)goto 283
        CALL EDAYCH(ID1,IM1,IERR)
        IF(IERR.EQ.1)goto 281
        CALL EDAY(ID1,IM1,IDS)

        CALL EDAYR(IDF,ID2,IM2)
        write(HOLD,'(I6,I4,I4)') ID2,IM2,IT2
 284    CALL EASKS(HOLD,' ',' End Day-of-month, month & time: ',
     &     24,' 31  12  24 ','end doy, month time',IER,nbhelp)
        K=0
        CALL EGETWI(HOLD,K,ID2,1,31,'F','end day of month',IER)
        CALL EGETWI(HOLD,K,IM2,1,12,'F','end month',IER)
        CALL EGETWI(HOLD,K,IT2,0,24,'F','time',IER)
        if(IER.ne.0)goto 284
        CALL EDAYCH(ID2,IM2,IERR)
        IF(IERR.EQ.1)goto 281
        CALL EDAY(ID2,IM2,IDF)
      ENDIF
      INEW=1

      RETURN
      END


C ************ editemclimatelist
C editemclimatelist: edit/create an item for climatelist file.
C Data fields include five seasons and typical assessment
C periods, documentation strings.

C << idea, add in the option to edit the climatelist file and
C << the text block from within clm!

      subroutine editemclimatelist()
#include "building.h"
#include "esprdbfile.h"
#include "espriou.h"
C espriou.h provides currentfile, cdblfil.
#include "seasons.h"
C seasons.h provides typper and typsea
      
      integer lnblnk  ! function definition

      COMMON/FILEP/IFIL
      common/OUTIN/IUOUT,IUIN
      COMMON/SPAD/MMOD,LIMIT,LIMTTY
      integer ifs,itfs,imfs
      COMMON/GFONT/IFS,ITFS,IMFS
      integer menuchw,igl,igr,igt,igb,igw,igwh
      COMMON/VIEWPX/menuchw,igl,igr,igt,igb,igw,igwh
C      common/appw/iappw,iappx,iappy

      COMMON/PERC/ID1,IM1,IT1,ID2,IM2,IT2,IDS,IDF,INEW
      COMMON/CLMDT1/CLMLOC
      COMMON/CLMSET/ICYEAR,ICDNGH,CLAT,CLONG
      COMMON/SET1/IYEAR,IBDOY,IEDOY,IFDAY,IFTIME
      integer NDAY
      real CDIF,CTMP,CDNR,CVEL,CDIR,CHUM
      COMMON/CLMDAT/NDAY,CDIF(MT),CTMP(MT),CDNR(MT),CVEL(MT),
     &              CDIR(MT),CHUM(MT)
      character clmlnam*32,clmlaid*72,clmldbfile*144,clmlavail*10
      character clmlhelp*72
      common/clmltext/clmlnam,clmlaid,clmldbfile,clmlavail,clmlhelp(60)
      common/clmlnum/nbclmlhelp

C External text editor.
      common/texted/tedlbl,teditor

C ITM is main item editing menu.
      character ITM(25)*38
      character NMTHNM(12)*3,DAYNAM(7)*3
      character descr*7,descrst*10,descrfn*10,descrstn*10
      character descrstp*10
      character DFILE*72,outs*124,louts*248
      character DESCRX*7,DESCR1X*10,DESCR1*10,hold*40
      CHARACTER CLMLOC*30,t32*32,d32*32,t72*72,d72*72
      character llasci*144,llitem*144,longtfile*144,longtfiledos*144
      character tedlbl*20,teditor*20,tmode*8,doit*248
      logical unixok,xst,concat,mxmn
      logical havedrawn   ! if the user has asked to draw graph
      logical altered     ! if user changes period.
      integer llt   ! length of string buffer
      DIMENSION IPICK(6)
      integer onleft  ! number of left axis
      integer onright ! number of right axis
      integer nitms,INO  ! max items and current menu item

#ifdef OSI
      integer iigl,iigr,iigt,iigb,iigw,iigwh
      integer iiw1,iiw2,iiw3,iiw4,iimenu
#else
      integer*8 iigl,iigr,iigt,iigb,iigw,iigwh
      integer*8 iiw1,iiw2,iiw3,iiw4,iimenu
#endif

C For help messages
      character helpinsub*24,helptopic*24 ! strings for help
      integer nbhelp     ! number of help lines found

     
      DATA NMTHNM/'Jan','Feb','Mar','Apr','May','Jun','Jul',
     &'Aug','Sep','Oct','Nov','Dec'/
      DATA DAYNAM/'Mon','Tue','Wed','Thu','Fri','Sat','Sun'/

C Check if Unix-based or DOS based.
      helpinsub='editemclimatelist'  ! set for clmper
      call isunix(unixok)
      havedrawn = .false.
      altered = .false.

C Graphic analysis. Enlarge graphics area slightly and reset graphic
C display period to the whole year.
      if(MMOD.EQ.8)then
        ID1=1; IM1=1
        ID2=31; IM2=12
        CALL EDAY(ID1,IM1,IIDS)
        CALL EDAY(ID2,IM2,IIDF)
        IDS=IIDS
        IDF=IIDF

C Altering the menu width sometimes causes re-draw issues so
C comment this out and let the menu cover over part of the graph
C for now.
C        if(iappw.gt.0.and.iappw.lt.100)then
C          menuchw = MAX0(int(42*iappw*0.01),20)
C          LIMTTY= MAX0(int(12*iappw*0.01),8)
C          LIMIT = MAX0(int(12*iappw*0.01),8)
C        else
C          menuchw = 42
C          LIMTTY=12
C          LIMIT =12
C        endif

      endif

    3 INO=-4
      IIER=0

C If clmlnam and clmlaid are not set fill them with default information.
      if(clmlnam(1:15).eq.'not yet entered')then
        write(clmlnam,'(2a)') CLMLOC(1:lnblnk(CLMLOC)),'  '
      endif
      if(clmlaid(1:15).eq.'not yet entered')then
        write(clmlaid,'(2a)') CLMLOC(1:lnblnk(CLMLOC)),
     &    ' was sourced from '
      endif
      WRITE(ITM(1),'(2A)')     'a menu str: ',clmlnam(1:26)
      WRITE(ITM(2),'(2A)')     'b menu aid: ',clmlaid(1:26)
      WRITE(ITM(3),'(2A)')     'c clm file: ',clmldbfile(1:26)
      WRITE(ITM(4),'(2A)')     'd available: ',clmlavail
      WRITE(ITM(5),'(A)')      ' ----seasons----------------------days'
      call stdate(iyear,is1wins,descr,descrst)
      call stdate(iyear,is1winf,descr,descrfn)
      jjd1=(is1winf-is1wins)+1
      call edayr(1,id1,im1)
      CALL EWEEKD(id1,im1,IYEAR,jjd1t)  ! get day of week at start of year.
      WRITE(ITM(6),'(4a,i4)')  'e  1st win ',descrst,'  ',descrfn,jjd1
      call stdate(iyear,is1sprs,descr,descrst)
      call stdate(iyear,is1sprf,descr,descrfn)
      jjd2=(is1sprf-is1sprs)+1
      WRITE(ITM(7),'(4a,i4)')  'f  spring  ',descrst,'  ',descrfn,jjd2
      call stdate(iyear,is1sums,descr,descrst)
      call stdate(iyear,is1sumf,descr,descrfn)
      jjd3=(is1sumf-is1sums)+1
      WRITE(ITM(8),'(4a,i4)')  'g  summer  ',descrst,'  ',descrfn,jjd3
      call stdate(iyear,is2sprs,descr,descrst)
      call stdate(iyear,is2sprf,descr,descrfn)
      jjd4=(is2sprf-is2sprs)+1
      WRITE(ITM(9),'(4a,i4)')  'h  autumn  ',descrst,'  ',descrfn,jjd4
      call stdate(iyear,is2wins,descr,descrst)
      call stdate(iyear,is2winf,descr,descrfn)
      jjd5=(is2winf-is2wins)+1
      WRITE(ITM(10),'(4a,i4)') 'i  2nd win ',descrst,'  ',descrfn,jjd5
      WRITE(ITM(11),'(A)')     ' ----typical periods--------------days'
      call stdate(iyear,ia1wins,descr,descrst)
      call stdate(iyear,ia1winf,descr,descrfn)
      jjd1=(ia1winf-ia1wins)+1
      WRITE(ITM(12),'(4a,i4)') 'j  1st win ',descrst,'  ',descrfn,jjd1
      call stdate(iyear,ia1sprs,descr,descrst)
      call stdate(iyear,ia1sprf,descr,descrfn)
      jjd2=(ia1sprf-ia1sprs)+1
      WRITE(ITM(13),'(4a,i4)') 'k  spring  ',descrst,'  ',descrfn,jjd2
      call stdate(iyear,iasums,descr,descrst)
      call stdate(iyear,iasumf,descr,descrfn)
      jjd3=(iasumf-iasums)+1
      WRITE(ITM(14),'(4a,i4)') 'l  summer  ',descrst,'  ',descrfn,jjd3
      call stdate(iyear,ia2sprs,descr,descrst)
      call stdate(iyear,ia2sprf,descr,descrfn)
      jjd4=(ia2sprf-ia2sprs)+1
      WRITE(ITM(15),'(4a,i4)') 'm  autumn  ',descrst,'  ',descrfn,jjd4
      call stdate(iyear,ia2wins,descr,descrst)
      call stdate(iyear,ia2winf,descr,descrfn)
      jjd5=(ia2winf-ia2wins)+1
      WRITE(ITM(16),'(4a,i4)') 'n  2nd win ',descrst,' ',descrfn,jjd5
      WRITE(ITM(17),'(A)')     '< scan weather for best-fit weeks'
      WRITE(ITM(18),'(A,i2,a)')' ----documentation (',nbclmlhelp,
     &  ')----------'
      WRITE(ITM(19),'(A)')     'o list/generate/edit documentation'
      WRITE(ITM(20),'(A)')     ' _________________________________'
      WRITE(ITM(21),'(A)')     '> save item documentation         '
      WRITE(ITM(22),'(A)')     '> edit climatelist file           '
      if(MMOD.EQ.8)then
        WRITE(ITM(23),'(A)')   '* graph ambient T & seasons       '
      else
        WRITE(ITM(23),'(A)')   '  not applicable                  '
      endif
      WRITE(ITM(24),'(A)')     '? help                            '
      WRITE(ITM(25),'(A)')     '- end                             '
      nitms=25

C If use already aksed for graph and date has change draw again.
      if(MMOD.EQ.8)then
        if(altered.and.havedrawn)then
          IPICK(1)=0; IPICK(2)=1; IPICK(3)=0
          IPICK(4)=0; IPICK(5)=0; IPICK(6)=0
          onleft=1
          onright=0
          if(IFS.ne.IMFS)call winfnt(IMFS)

C Setup and pass in parameters to win3d.
          iiw1=6; iiw2=12; iiw3=6; iiw4=3; iimenu=menuchw
          iigl=igl; iigr=igr; iigt=igt; iigb=igb; iigw=igw; iigwh=igwh
          CALL win3d(iimenu,iiw1,iiw2,iiw3,iiw4,
     &      iigl,iigr,iigt,iigb,iigw,iigwh)
          igl=int(iigl); igr=int(iigr); igt=int(iigt); igb=int(iigb)
          igw=int(iigw); igwh=int(iigwh)
          if(IFS.ne.IMFS)call winfnt(IFS)
          call startbuffer()

C Debug.
C          write(6,*)'xx igl igr igt igb igw igwh',
C     &      igl,igr,igt,igb,igw,igwh

          mxmn=.TRUE.
          CALL CLMDRW(IPICK,mxmn,onleft,onright,'s')
          havedrawn = .true.
        endif
      endif

C Context help
      helptopic='climate_list_facility'
      call gethelptext(helpinsub,helptopic,nbhelp)

      CALL EMENU('Weather list item',ITM,nitms,INO)
      if(INO.EQ.nitms)then

C Reset the graphic context prior to leaving. Currently commented out
C (to match the initial menuchw comment out above).
C        if(MMOD.EQ.8)then
C          if(iappw.gt.0.and.iappw.lt.100)then
C            menuchw = MAX0(int(29*iappw*0.01),12)
C            LIMTTY= MAX0(int(6*iappw*0.01),4)
C            LIMIT = MAX0(int(6*iappw*0.01),4)
C          else
C            menuchw = 29
C            LIMTTY=6
C            LIMIT =6
C          endif
C        endif
        return
      elseif(INO.eq.1)then

C String for the selection menu.
        helptopic='climate_list_menu_txt'
        call gethelptext(helpinsub,helptopic,nbhelp)
        t32=clmlnam
        if(t32(1:15).eq.'not yet entered')write(t32,'(2a)') CLMLOC,'  '
        write(d32,'(2a)') CLMLOC,'  '
        CALL EASKS(t32,'Text for the selection list menu',' ',32,
     &    d32,'clmlist name',IER,1)
        if(t32(1:2).ne.'  ')clmlnam=t32
      elseif(INO.eq.2)then

C Aide memorie for the menu.
        helptopic='climate_list_menu_aide'
        call gethelptext(helpinsub,helptopic,nbhelp)
        t72=clmlaid
        if(t72(1:15).eq.'not yet entered')then
          write(t72,'(2a)') CLMLOC(1:lnblnk(CLMLOC)),
     &    ' was sourced from '
        endif
        write(d72,'(2a)') CLMLOC(1:lnblnk(CLMLOC)),
     &    ' was sourced from '
        CALL EASKS(t72,'Aide memorie for the selection list menu',' ',
     &    72,d72,'clmlist aide',IER,1)
        if(t72(1:2).ne.'  ')clmlaid=t72
      elseif(INO.eq.3)then

C Installed location of weather file. If GTK then use file browser.
        helptopic='climate_list_loc'
        call gethelptext(helpinsub,helptopic,nbhelp)
        write(llasci,'(a)') lclim(1:lnblnk(lclim))

C Debug.
C        write(6,*) llasci

  290   llt=lnblnk(llasci)
        iglib = igraphiclib()  ! find out if X11 or GTK or text support only.
        if(iglib.eq.1.or.iglib.eq.3)then
          if(llt.lt.96)then
            CALL EASKF(LLASCI,'Weather file installed location:',
     &        ' ',96,DCLIM,'weather file location',IER,nbhelp)
          elseif(llt.ge.96.and.llt.lt.124)then
            CALL EASKF(LLASCI,'Weather file installed location:',
     &        ' ',124,DCLIM,'weather file location',IER,nbhelp)
          elseif(llt.ge.124.and.llt.le.144)then
            CALL EASKF(LLASCI,'Weather file installed location:',
     &        ' ',144,DCLIM,'weather file location',IER,nbhelp)
          endif
        elseif(iglib.eq.2)then
          CALL EASKF(LLASCI,'Weather file installed location:',
     &      ' ',144,DCLIM,'weather file location',IER,nbhelp)
        else
          CALL EASKF(LLASCI,'Weather file installed location:',
     &      ' ',96,DCLIM,'weather file location',IER,nbhelp)
        endif
        IF(LLASCI(1:2).EQ.'  '.or.LLASCI(1:4).eq.'UNKN')GOTO 290

C Cast back to 72 char name.
        llt=lnblnk(llasci)
        write(clmldbfile,'(a)') LLASCI(1:llt)
      elseif(INO.eq.4)then

C Toggle for available
        call FINDFIL(clmldbfile,XST)
        if(XST)then
          call usrmsg('The file was found in the install location',
     &      'so it is ok to mark it as ONLINE','P')
        else
          call usrmsg('The file was NOT found in the install location',
     &      'so check it prior to marking it as ONLINE','P')
        endif

        helptopic='climate_item_offline'
        call gethelptext(helpinsub,helptopic,nbhelp)
        call easkabc('Choices:',' ','online','offline','continue',IW,
     &    nbhelp)
        if(iw.eq.1) clmlavail = 'ONLINE'
        if(iw.eq.2) clmlavail = 'OFFLINE'
      elseif(INO.eq.6)then

C 1st winter season
C Edit the current period in terms of day and month. Also check to
C see if next period overlaps and correct.
   91   hold = ' '
        helptopic='climate_winter_season'
        call gethelptext(helpinsub,helptopic,nbhelp)
        call stdate(iyear,is1wins,descr,descrst)
        call stdate(iyear,is1winf,descr,descrfn)
        WRITE(outs,'(4a)')  ' 1st winter ',descrst,' - ',descrfn
        call edisp(iuout,outs)

C If first season is not 1 Jan then warn user.
        if(is1wins.ne.1)then
          write(outs,'(3a)') 'First day of season (',descrfn,
     &      ') is not the first day of the year!'
          call edisp(iuout,outs)
        endif 
        call edayr(is1wins,id1,im1)
        call edayr(is1winf,id2,im2)
        CALL EWEEKD(id1,im1,IYEAR,jjd1t)
        WRITE(HOLD,'(4I4)')id1,im1,id2,im2
        CALL EASKS(HOLD,'Season: start (day & month) and',
     &    'finish (day & month):',40,' 1 1 28 2','season',IIER,nbhelp)
        K=0
        CALL EGETWI(HOLD,K,id1,1,31,'W','season st day',IIER)
        CALL EGETWI(HOLD,K,im1,1,12,'W','season st month',IIER)
        CALL EGETWI(HOLD,K,id2,1,31,'W','season fn day',IIER)
        CALL EGETWI(HOLD,K,im2,1,12,'W','season fn month',IIER)
        CALL EDAY(id1,im1,is1wins)
        CALL EDAY(id2,im2,is1winf)
        if(is1sprs.eq.is1winf+1)then
          continue
        else
          is1sprs=is1winf+1
        endif
        altered = .true.
        if(iier.ne.0)goto 91
      elseif(INO.eq.7)then

C Spring season.
C Edit the current period in terms of day and month.
   92   hold = ' '
        helptopic='climate_spring_season'
        call gethelptext(helpinsub,helptopic,nbhelp)
        call stdate(iyear,is1sprs,descr,descrst)
        call stdate(iyear,is1sprf,descr,descrfn)
        WRITE(outs,'(4a)')  ' Spring ',descrst,' - ',descrfn
        call edisp(iuout,outs)
        call edayr(is1sprs,id1,im1)
        call edayr(is1sprf,id2,im2)
        CALL EWEEKD(id1,im1,IYEAR,jjd2t)
        if(jjd2t.ne.jjd1t)then
          if(jjd2t.gt.jjd1t)then
            jprev=is1sprs-(jjd2t-jjd1t)
            call stdate(iyear,jprev,descr,descrstp)
            jnext=is1sprs-(jjd2t-jjd1t)+7
            call stdate(iyear,jnext,descr,descrstn)
          elseif(jjd2t.lt.jjd1t)then
            jnext=is1sprs-(jjd1t-jjd2t)
            call stdate(iyear,jnext,descr,descrstn)
            jprev=is1sprs-(jjd1t-jjd2t)-7
            call stdate(iyear,jnext,descr,descrstp)
          endif
          write(louts,'(8a)') 'Spring starts on a ',
     &      DAYNAM(jjd2t),' and prior season starts on a ',
     &      DAYNAM(jjd1t),' so consider ',descrstp,' or ',
     &      descrstn
          call edisp248(iuout,louts,80)
        endif
        WRITE(HOLD,'(4I4)')id1,im1,id2,im2
        CALL EASKS(HOLD,'Season: start (day & month) and',
     &    'finish (day & month):',40,' 1 3 30 4','season',IIER,nbhelp)
        K=0
        CALL EGETWI(HOLD,K,id1,1,31,'W','season st day',IIER)
        CALL EGETWI(HOLD,K,im1,1,12,'W','season st month',IIER)
        CALL EGETWI(HOLD,K,id2,1,31,'W','season fn day',IIER)
        CALL EGETWI(HOLD,K,im2,1,12,'W','season fn month',IIER)
        CALL EDAY(id1,im1,is1sprs)
        CALL EDAY(id2,im2,is1sprf)
        if(is1sums.eq.is1sprf+1)then
          continue
        else
          is1sums=is1sprf+1
        endif
        if(is1sprs.eq.is1winf+1)then
          continue
        else
          is1winf=is1sprs-1
        endif
        altered = .true.
        if(iier.ne.0)goto 92
      elseif(INO.eq.8)then

C Summer season.
C Edit the current period in terms of day and month.
   93   hold = ' '
        helptopic='climate_summer_season'
        call gethelptext(helpinsub,helptopic,nbhelp)
        call stdate(iyear,is1sums,descr,descrst)
        call stdate(iyear,is1sumf,descr,descrfn)
        WRITE(outs,'(4a)')  ' Summer ',descrst,' - ',descrfn
        call edisp(iuout,outs)
        call edayr(is1sums,id1,im1)
        call edayr(is1sumf,id2,im2)
        CALL EWEEKD(id1,im1,IYEAR,jjd3t)
        if(jjd3t.ne.jjd1t)then
          if(jjd3t.gt.jjd1t)then
            jprev=is1sums-(jjd3t-jjd1t)
            call stdate(iyear,jprev,descr,descrstp)
            jnext=is1sums-(jjd3t-jjd1t)+7
            call stdate(iyear,jnext,descr,descrstn)
          elseif(jjd3t.lt.jjd1t)then
            jnext=is1sums-(jjd1t-jjd3t)
            call stdate(iyear,jnext,descr,descrstn)
            jprev=is1sums-(jjd1t-jjd3t)-7
            call stdate(iyear,jnext,descr,descrstp)
          endif
          write(louts,'(8a)') 'Summer starts on a ',
     &      DAYNAM(jjd3t),' and winter starts on a ',
     &      DAYNAM(jjd1t),' so consider ',descrstp,' or ',
     &      descrstn
          call edisp248(iuout,louts,80)
        endif
        WRITE(HOLD,'(4I4)')id1,im1,id2,im2
        CALL EASKS(HOLD,'Season: start (day & month) and',
     &    'finish (day & month):',40,' 1 5 31 8','season',IIER,nbhelp)
        K=0
        CALL EGETWI(HOLD,K,id1,1,31,'W','season st day',IIER)
        CALL EGETWI(HOLD,K,im1,1,12,'W','season st month',IIER)
        CALL EGETWI(HOLD,K,id2,1,31,'W','season fn day',IIER)
        CALL EGETWI(HOLD,K,im2,1,12,'W','season fn month',IIER)
        CALL EDAY(id1,im1,is1sums)
        CALL EDAY(id2,im2,is1sumf)
        if(is2sprs.eq.is1sumf+1)then
          continue
        else
          is2sprs=is1sumf+1
        endif
        if(is1sums.eq.is1sprf+1)then
          continue
        else
          is1sprf=is1sums-1
        endif
        altered = .true.
        if(iier.ne.0)goto 93
      elseif(INO.eq.9)then

C Autumn season.
C Edit the current period in terms of day and month.
   94   hold = ' '
        helptopic='climate_autumn_season'
        call gethelptext(helpinsub,helptopic,nbhelp)
        call stdate(iyear,is2sprs,descr,descrst)
        call stdate(iyear,is2sprf,descr,descrfn)
        WRITE(outs,'(4a)')  ' Autumn ',descrst,' - ',descrfn
        call edisp(iuout,outs)
        call edayr(is2sprs,id1,im1)
        call edayr(is2sprf,id2,im2)
        CALL EWEEKD(id1,im1,IYEAR,jjd4t)
        if(jjd4t.ne.jjd1t)then
          if(jjd4t.gt.jjd1t)then
            jprev=is2sprs-(jjd4t-jjd1t)
            call stdate(iyear,jprev,descr,descrstp)
            jnext=is2sprs-(jjd4t-jjd1t)+7
            call stdate(iyear,jnext,descr,descrstn)
          elseif(jjd4t.lt.jjd1t)then
            jnext=is2sprs-(jjd1t-jjd4t)
            call stdate(iyear,jnext,descr,descrstn)
            jprev=is2sprs-(jjd1t-jjd4t)-7
            call stdate(iyear,jnext,descr,descrstp)
          endif
          write(louts,'(8a)') 'Autumn starts on a ',
     &      DAYNAM(jjd4t),' and winter starts on a ',
     &      DAYNAM(jjd1t),' so consider ',descrstp,' or ',
     &      descrstn
          call edisp248(iuout,louts,80)
        endif
        WRITE(HOLD,'(4I4)')id1,im1,id2,im2
        CALL EASKS(HOLD,'Season: start (day & month) and',
     &    'finish (day & month):',40,' 1 9 31 10','season',IIER,nbhelp)
        K=0
        CALL EGETWI(HOLD,K,id1,1,31,'W','season st day',IIER)
        CALL EGETWI(HOLD,K,im1,1,12,'W','season st month',IIER)
        CALL EGETWI(HOLD,K,id2,1,31,'W','season fn day',IIER)
        CALL EGETWI(HOLD,K,im2,1,12,'W','season fn month',IIER)
        CALL EDAY(id1,im1,is2sprs)
        CALL EDAY(id2,im2,is2sprf)
        if(is2wins.eq.is2sprf+1)then
          continue
        else
          is2wins=is2sprf+1
        endif
        if(is2sprs.eq.is1sumf+1)then
          continue
        else
          is1sumf=is2sprs-1
        endif
        altered = .true.
        if(iier.ne.0)goto 94
      elseif(INO.eq.10)then

C 2nd winter season.
C Edit the current period in terms of day and month.
   95   hold = ' '
        helptopic='climate_winter2_season'
        call gethelptext(helpinsub,helptopic,nbhelp)
        call stdate(iyear,is2wins,descr,descrst)
        call stdate(iyear,is2winf,descr,descrfn)
        WRITE(outs,'(4a)')  ' 2nd winter ',descrst,' - ',descrfn
        call edisp(iuout,outs)
        call edayr(is2wins,id1,im1)
        call edayr(is2winf,id2,im2)
        CALL EWEEKD(id1,im1,IYEAR,jjd5t)
        if(jjd5t.ne.jjd1t)then
          if(jjd5t.gt.jjd1t)then
            jprev=is2wins-(jjd5t-jjd1t)
            call stdate(iyear,jprev,descr,descrstp)
            jnext=is2wins-(jjd5t-jjd1t)+7
            call stdate(iyear,jnext,descr,descrstn)
          elseif(jjd5t.lt.jjd1t)then
            jnext=is2wins-(jjd1t-jjd5t)
            call stdate(iyear,jnext,descr,descrstn)
            jprev=is2wins-(jjd1t-jjd5t)-7
            call stdate(iyear,jnext,descr,descrstp)
          endif
          write(louts,'(8a)') '2nd winter starts on a ',
     &      DAYNAM(jjd5t),' and 1st winter starts on a ',
     &      DAYNAM(jjd1t),' so consider ',descrstp,' or ',
     &      descrstn
          call edisp248(iuout,louts,80)
        endif
        WRITE(HOLD,'(4I4)')id1,im1,id2,im2
        CALL EASKS(HOLD,'Season: start (day & month) and',
     &    'finish (day & month):',40,' 1 11 31 12','season',IIER,nbhelp)
        K=0
        CALL EGETWI(HOLD,K,id1,1,31,'W','season st day',IIER)
        CALL EGETWI(HOLD,K,im1,1,12,'W','season st month',IIER)
        CALL EGETWI(HOLD,K,id2,1,31,'W','season fn day',IIER)
        CALL EGETWI(HOLD,K,im2,1,12,'W','season fn month',IIER)
        CALL EDAY(id1,im1,is2wins)
        CALL EDAY(id2,im2,is2winf)
        if(is2wins.eq.is2sprf+1)then
          continue
        else
          is2sprf=is2wins-1
        endif
        altered = .true.
        if(iier.ne.0)goto 95
      elseif(INO.eq.12)then

C 1st winter period
C Edit the current period in terms of day and month.
   96   hold = ' '
        helptopic='climate_winter1_season'
        call gethelptext(helpinsub,helptopic,nbhelp)
        call stdate(iyear,ia1wins,descr,descrst)
        call stdate(iyear,ia1winf,descr,descrfn)
        WRITE(outs,'(4a)')  ' 1st winter typical ',descrst,' - ',
     &    descrfn
        call edisp(iuout,outs)
        call edayr(ia1wins,id1,im1)
        call edayr(ia1winf,id2,im2)
        WRITE(HOLD,'(4I4)')id1,im1,id2,im2
        CALL EASKS(HOLD,'Typical: start (day & month) and',
     &    'finish (day & month):',40,' 9 1 15 1','typical',IIER,nbhelp)
        K=0
        CALL EGETWI(HOLD,K,id1,1,31,'W','typical st day',IIER)
        CALL EGETWI(HOLD,K,im1,1,12,'W','typical st month',IIER)
        CALL EGETWI(HOLD,K,id2,1,31,'W','typical fn day',IIER)
        CALL EGETWI(HOLD,K,im2,1,12,'W','typical fn month',IIER)
        CALL EDAY(id1,im1,ia1wins)
        CALL EDAY(id2,im2,ia1winf)
        if(iier.ne.0)goto 96
      elseif(INO.eq.13)then

C Spring period.
C Edit the current period in terms of day and month.
   97   hold = ' '
        helptopic='climate_spring_season'
        call gethelptext(helpinsub,helptopic,nbhelp)
        call stdate(iyear,ia1sprs,descr,descrst)
        call stdate(iyear,ia1sprf,descr,descrfn)
        WRITE(outs,'(4a)')  ' Spring typical ',descrst,' - ',
     &    descrfn
        call edisp(iuout,outs)
        call edayr(ia1sprs,id1,im1)
        call edayr(ia1sprf,id2,im2)
        WRITE(HOLD,'(4I4)')id1,im1,id2,im2
        CALL EASKS(HOLD,'Typical: start (day & month) and',
     &    'finish (day & month):',40,' 6 3 12 3','typical',IIER,nbhelp)
        K=0
        CALL EGETWI(HOLD,K,id1,1,31,'W','typical st day',IIER)
        CALL EGETWI(HOLD,K,im1,1,12,'W','typical st month',IIER)
        CALL EGETWI(HOLD,K,id2,1,31,'W','typical fn day',IIER)
        CALL EGETWI(HOLD,K,im2,1,12,'W','typical fn month',IIER)
        CALL EDAY(id1,im1,ia1sprs)
        CALL EDAY(id2,im2,ia1sprf)
        if(iier.ne.0)goto 97
      elseif(INO.eq.14)then

C Summer period.
C Edit the current period in terms of day and month.
   98   hold = ' '
        helptopic='climate_summer_season'
        call gethelptext(helpinsub,helptopic,nbhelp)
        call stdate(iyear,iasums,descr,descrst)
        call stdate(iyear,iasumf,descr,descrfn)
        WRITE(outs,'(4a)')  ' Summer typical ',descrst,' - ',
     &    descrfn
        call edisp(iuout,outs)
        call edayr(iasums,id1,im1)
        call edayr(iasumf,id2,im2)
        WRITE(HOLD,'(4I4)')id1,im1,id2,im2
        CALL EASKS(HOLD,'Typical: start (day & month) and',
     &    'finish (day & month):',40,' 11 7 17 7','typical',IIER,nbhelp)
        K=0
        CALL EGETWI(HOLD,K,id1,1,31,'W','typical st day',IIER)
        CALL EGETWI(HOLD,K,im1,1,12,'W','typical st month',IIER)
        CALL EGETWI(HOLD,K,id2,1,31,'W','typical fn day',IIER)
        CALL EGETWI(HOLD,K,im2,1,12,'W','typical fn month',IIER)
        CALL EDAY(id1,im1,iasums)
        CALL EDAY(id2,im2,iasumf)
        if(iier.ne.0)goto 98
      elseif(INO.eq.15)then

C Autumn period.
C Edit the current period in terms of day and month.
   99   hold = ' '
        helptopic='climate_autumn_season'
        call gethelptext(helpinsub,helptopic,nbhelp)
        call stdate(iyear,ia2sprs,descr,descrst)
        call stdate(iyear,ia2sprf,descr,descrfn)
        WRITE(outs,'(4a)')  ' Autumn typical ',descrst,' - ',
     &    descrfn
        call edisp(iuout,outs)
        call edayr(ia2sprs,id1,im1)
        call edayr(ia2sprf,id2,im2)
        WRITE(HOLD,'(4I4)')id1,im1,id2,im2
        CALL EASKS(HOLD,'Typical: start (day & month) and',
     &    'finish (day & month):',40,' 2 10 8 10','typical',IIER,nbhelp)
        K=0
        CALL EGETWI(HOLD,K,id1,1,31,'W','typical st day',IIER)
        CALL EGETWI(HOLD,K,im1,1,12,'W','typical st month',IIER)
        CALL EGETWI(HOLD,K,id2,1,31,'W','typical fn day',IIER)
        CALL EGETWI(HOLD,K,im2,1,12,'W','typical fn month',IIER)
        CALL EDAY(id1,im1,ia2sprs)
        CALL EDAY(id2,im2,ia2sprf)
        if(iier.ne.0)goto 99
      elseif(INO.eq.16)then

C 2nd winter period.
C Edit the current period in terms of day and month.
  100   hold = ' '
        helptopic='climate_winter2_season'
        call gethelptext(helpinsub,helptopic,nbhelp)
        call stdate(iyear,ia2wins,descr,descrst)
        call stdate(iyear,ia2winf,descr,descrfn)
        WRITE(outs,'(4a)')  ' 2nd winter typical ',descrst,' - ',
     &    descrfn
        call edisp(iuout,outs)
        call edayr(ia2wins,id1,im1)
        call edayr(ia2winf,id2,im2)
        WRITE(HOLD,'(4I4)')id1,im1,id2,im2
        CALL EASKS(HOLD,'Typical: start (day & month) and',
     &    'finish (day & month):',40,' 20 11 26 11','typical',
     &    IIER,nbhelp)
        K=0
        CALL EGETWI(HOLD,K,id1,1,31,'W','typical st day',IIER)
        CALL EGETWI(HOLD,K,im1,1,12,'W','typical st month',IIER)
        CALL EGETWI(HOLD,K,id2,1,31,'W','typical fn day',IIER)
        CALL EGETWI(HOLD,K,im2,1,12,'W','typical fn month',IIER)
        CALL EDAY(id1,im1,ia2wins)
        CALL EDAY(id2,im2,ia2winf)
        if(iier.ne.0)goto 100
      elseif(INO.eq.17)then

C Scan weather data for best-fit weeks.
          call DDRADSUM('?')
      elseif(INO.eq.19)then

C Documentations text option.
        helptopic='climate_list_doc'
        call gethelptext(helpinsub,helptopic,nbhelp)
        call easkabcd('Documentation options:',' ','List','Initialise',
     &    'Edit','Continue',idw,nbhelp)
        if(idw.eq.1)then
          if(nbclmlhelp.ge.0)then
            call edisp(iuout,' ')
            do 43 ij=1,nbclmlhelp
              call edisp(iuout,clmlhelp(ij))
  43        continue
          else
            call edisp(iuout,'There is no documentation.')
          endif
        elseif(idw.eq.2)then
          write(clmlhelp(1),'(2a)') 'Weather is ',clmloc 
          ACLAT=ABS(CLAT)
          ACLONG=ABS(CLONG)
          IF(CLONG.LT.0.0.AND.CLAT.GE.0.0)then
            WRITE(clmlhelp(2),102)ACLAT,ACLONG,IYEAR
 102        FORMAT(' Location:',F6.2,'N',F6.2,'W :',I5)
          elseif(CLONG.GE.0.0.AND.CLAT.LT.0.0)then
            WRITE(clmlhelp(2),103)ACLAT,ACLONG,IYEAR
 103        FORMAT(' Location:',F6.2,'S',F6.2,'E :',I5)
          elseif(CLONG.LT.0.0.AND.CLAT.LT.0.0)then
            WRITE(clmlhelp(2),104)ACLAT,ACLONG,IYEAR
 104        FORMAT(' Location:',F6.2,'S',F6.2,'W :',I5)
          else
            WRITE(clmlhelp(2),101)ACLAT,ACLONG,IYEAR
 101        FORMAT(' Location:',F6.2,'N',F6.2,'E :',I5)
          endif

C Use variant of code in clmmmm.F to generate text.
          write(clmlhelp(3),'(a)')
     &    ' Month    Minimum  Time          Maximum  Time         Mean'

C Initialise parameters for whole year.
          ID1=1;  IM1=1;  IT1=1
          ID2=31; IM2=12; IT2=24
          IDS=1;  IDF=365
          KDS=1;  KDF=24
          XMAXI=-1E+10; XMINI=1E+10
          XMEANI=0.; HRTOT=0.

C INITIALISE PARAMETERS FOR MONTHLY TOTALS.
          MTH1=IM1; XMAXM=-1E10; XMINM=1E10; XMEANM=0.; HRM=0.

C Find day of week for start of the period.
          CALL EDAYR(IDS,IDAYN,IMTHN)
      
C Loop for each day, reading data from weather file.
          DO 10 I=IDS,IDF
            IDD=I
            CALL CLMGET(IDD)

C Set parameters for current day and analyse max, min mean.
            XMAXJ=-1E+10
            XMINJ=1E+10
            XMEANJ=0
            JS=KDS
            JF=KDF
            IF(I.EQ.IDS)JS=MAX0(KDS,IT1)
            IF(I.EQ.IDF)JF=MIN0(KDF,IT2)
            DAYL=JF-JS+1
            DO 20 J=JS,JF
              X=CTMP(J)
              IF(X.LE.XMAXJ)GO TO 2
              XMAXJ=X
              JTMX=J
   2          IF(X.GE.XMINJ)GO TO 4
              XMINJ=X
              JTMN=J
   4          XMEANJ=XMEANJ+X
   20       CONTINUE

C Convert day to D,M values and compute parameters for this week or month.
            II=I
            CALL EDAYR(II,IDD,IMM)
            CALL EWEEKD(IDD,IMM,IYEAR,IDWK)
            if(IMM.EQ.MTH1)GO TO 40

            XMEAN=XMEANM/HRM
            XMAX=XMAXM
            XMIN=XMINM
            CALL EDAYR(KDMN,KDN,KMN)
            call stdate(iyear,KDMN,DESCR,DESCR1)
            CALL EDAYR(KDMX,KDX,KMX)
            call stdate(iyear,KDMX,DESCRX,DESCR1X)
            line = 3+kmn
            write(clmlhelp(line),
     &        '(1x,a,f9.1,a,i2,2a,F11.1,a,i2,2a,f8.1)')
     &        NMTHNM(KMN),XMIN,' @',KTMN,'h00 ',DESCR1(1:6),
     &        XMAX,' @',KTMX,'h00 ',DESCR1X(1:6),XMEAN

C Update parameters for whole period and reinitialise
            IF(XMAXM.LE.XMAXI)GO TO 22
            XMAXI=XMAXM
            IDMX=KDMX
            ITMX=KTMX
  22        IF(XMINM.GE.XMINI)GO TO 24
            XMINI=XMINM
            IDMN=KDMN
            ITMN=KTMN
  24        XMEANI=XMEANI+XMEANM
            HRTOT=HRTOT+HRM
            XMAXM=-1E10
            XMINM=1E10
            XMEANM=0.
            HRM=0.
            MTH1=IMM
  40        CONTINUE

C Update parameters for this month and go to the next day.
            IF(XMAXJ.LE.XMAXM)GO TO 42
            XMAXM=XMAXJ
            KDMX=I
            KTMX=JTMX
  42        IF(XMINJ.GE.XMINM)GO TO 44
            XMINM=XMINJ
            KDMN=I
            KTMN=JTMN
  44        XMEANM=XMEANM+XMEANJ
            HRM=HRM+DAYL
   10     CONTINUE

C Now compute parameters for last month.
          XMEAN=XMEANM/HRM
          XMAX=XMAXM
          XMIN=XMINM
          CALL EDAYR(KDMN,KDN,KMN)
          call stdate(iyear,KDMN,DESCR,DESCR1)
          CALL EDAYR(KDMX,KDX,KMX)
          call stdate(iyear,KDMX,DESCRX,DESCR1X)
          line = 15
          write(clmlhelp(line),
     &      '(1x,a,f9.1,a,i2,2a,F11.1,a,i2,2a,f8.1)')
     &      NMTHNM(KMN),XMIN,' @',KTMN,'h00 ',DESCR1(1:6),
     &      XMAX,' @',KTMX,'h00 ',DESCR1X(1:6),XMEAN

C Update parameters for whole period.
          IF(XMAXM.LE.XMAXI)GO TO 62
          XMAXI=XMAXM
          IDMX=KDMX
          ITMX=KTMX
  62      IF(XMINM.GE.XMINI)GO TO 64
          XMINI=XMINM
          IDMN=KDMN
          ITMN=KTMN
  64      XMEANI=XMEANI+XMEANM
          HRTOT=HRTOT+HRM

C Now for the `all line'.
          XMEAN=XMEANI/HRTOT
          XMAX=XMAXI
          XMIN=XMINI
          CALL EDAYR(IDMN,KDMN,KMMN)
          call stdate(iyear,IDMN,DESCR,DESCR1)
          CALL EDAYR(IDMX,KDMX,KMMX)
          call stdate(iyear,IDMX,DESCRX,DESCR1X)
          line = 16
          write(clmlhelp(line),
     &      '(a,f6.1,a,i2,2a,F7.1,a,i2,2a,f8.1)')
     &      ' Annual',XMIN,' @',ITMN,'h00 ',DESCR1,
     &      XMAX,' @',ITMX,'h00 ',DESCR1X,XMEAN
          write(clmlhelp(17),'(a)') ' ---Seasons & typical periods---'
          call stdate(iyear,is1wins,descr,descrst)
          call stdate(iyear,is1winf,descr,descrfn)
          write(clmlhelp(18),'(4a)') 'Winter season is ',descrst,' - ',
     &      descrfn
          call stdate(iyear,ia1wins,descr,descrst)
          write(clmlhelp(19),'(2a)') 'Typical winter week begins ',
     &      descrst
          call stdate(iyear,is1sprs,descr,descrst)
          call stdate(iyear,is1sprf,descr,descrfn)
          write(clmlhelp(20),'(4a)') 'Spring season is ',descrst,' - ',
     &      descrfn
          call stdate(iyear,ia1sprs,descr,descrst)
          write(clmlhelp(21),'(2a)') 'Typical spring week begins ',
     &      descrst
          call stdate(iyear,is1sums,descr,descrst)
          call stdate(iyear,is1sumf,descr,descrfn)
          write(clmlhelp(22),'(4a)') 'Summer season is ',descrst,' - ',
     &      descrfn
          call stdate(iyear,iasums,descr,descrst)
          write(clmlhelp(23),'(2a)') 'Typical summer week begins ',
     &      descrst
          call stdate(iyear,is2sprs,descr,descrst)
          call stdate(iyear,is2sprf,descr,descrfn)
          write(clmlhelp(24),'(4a)') 'Autumn season is ',descrst,' - ',
     &      descrfn
          call stdate(iyear,ia2sprs,descr,descrst)
          write(clmlhelp(25),'(2a)') 'Typical autumn week begins ',
     &      descrst
          call stdate(iyear,is2wins,descr,descrst)
          call stdate(iyear,is2winf,descr,descrfn)
          write(clmlhelp(26),'(4a)') 'Winter season is ',descrst,' - ',
     &      descrfn
          call stdate(iyear,ia2wins,descr,descrst)
          write(clmlhelp(27),'(2a)') 'Typical winter week begins ',
     &      descrst
          nbclmlhelp=27

C Remind the user about saving and editing.
          call usrmsg(
     &      'After generating text entry you should save it to file',
     &      'prior to editing it!','W')
        elseif(idw.eq.3)then

C Edit the block of text.
          if(unixok)then
            call addpath(llitem,longtfile,concat)
          else

C If running on a non-unix machine see if there are spaces in the name
C and change any / to \.
            call addpath(llitem,longtfile,concat)
            call cmdfiledos(longtfile,longtfiledos,ier)
            longtfile=' '
            longtfile=longtfiledos
          endif
          tmode='graph'
          if(teditor(1:2).eq.'  ')then
            call usrmsg(
     &    'The external word processor has not be defined. See your',
     &    'administrator about updating the esprc definition.','W')
          elseif(teditor(1:2).eq.'vi')then
            tmode='text'
          endif
          write(doit,'(a,2x,a,a)') teditor(1:lnblnk(teditor)),
     &      longtfile(1:lnblnk(longtfile)),' &'
          call usrmsg('Begining edit via',doit,'-')
          call runit(doit,tmode)
        elseif(idw.eq.4)then
          continue
        endif
      elseif(INO.eq.21)then

C Save climatelist text block to file, confirm file name and open,
C call mkitemclimatelist and then close file.
        write(llitem,'(2a)') lclim(1:lnblnk(lclim)),'.block'
        write(DFILE,'(2a)') lclim(1:lnblnk(lclim)),'.block'
        helptopic='climate_current_info'
        call gethelptext(helpinsub,helptopic,nbhelp)
        llt=lnblnk(llitem)
        iglib = igraphiclib()  ! find out if X11 or GTK or text support only.
        if(iglib.eq.1.or.iglib.eq.3)then
          if(llt.lt.96)then
            CALL EASKF(llitem,' Climatelist entry text block?',
     &        ' ',96,DFILE,'climatelist entry file',IER,nbhelp)
          elseif(llt.ge.96.and.llt.lt.124)then
            CALL EASKF(llitem,' Climatelist entry text block?',
     &        ' ',124,DFILE,'climatelist entry file',IER,nbhelp)
          elseif(llt.ge.124.and.llt.le.144)then
            CALL EASKF(llitem,' Climatelist entry text block?',
     &        ' ',144,DFILE,'climatelist entry file',IER,nbhelp)
          endif
        elseif(iglib.eq.2)then
          CALL EASKF(llitem,' Climatelist entry text block?',
     &      ' ',144,DFILE,'climatelist entry file',IER,nbhelp)
        else
          CALL EASKF(llitem,' Climatelist entry text block?',
     &      ' ',96,DFILE,'climatelist entry file',IER,nbhelp)
        endif
        iuf=ifil+1
        CALL EFOPSEQ(iuf,llitem,4,IER)
        call mkitemclimatelist(iuf,ier)
        CALL ERPFREE(iuf,ISTAT)

      elseif(INO.eq.22)then

C Edit the climatelist file. First warn the user.
        call usrmsg('WARNING: be careful when editing this file!',
     &    'The tag and data format should be maintained.','W')
        if(unixok)then
          call addpath(cdblfil,longtfile,concat)
        else

C If running on a non-unix machine see if there are spaces in the name
C and change any / to \.
          call addpath(cdblfil,longtfile,concat)
          call cmdfiledos(longtfile,longtfiledos,ier)
          longtfile=' '
          longtfile=longtfiledos
        endif
        tmode='graph'
        if(teditor(1:2).eq.'  ')then
          call usrmsg(
     &    'The external word processor has not be defined. See your',
     &    'administrator about updating the esprc definition.','W')
        elseif(teditor(1:2).eq.'vi')then
          tmode='text'
        endif
        write(doit,'(a,2x,a,a)') teditor(1:lnblnk(teditor)),
     &    longtfile(1:lnblnk(longtfile)),' &'
        call usrmsg('Begining edit via',doit,'-')
        call runit(doit,tmode)

      elseif(INO.eq.23)then

C Control graphics.
C Pre-define the period and the line type to draw and which
C axis are required and then directly call CLMDRW
        if(MMOD.EQ.8)then
          IPICK(1)=0; IPICK(2)=1; IPICK(3)=0
          IPICK(4)=0; IPICK(5)=0; IPICK(6)=0
          onleft=1
          onright=0
          if(IFS.ne.IMFS)call winfnt(IMFS)

C Setup and pass in parameters to win3d.
          iiw1=6; iiw2=12; iiw3=6; iiw4=3; iimenu=menuchw
          iigl=igl; iigr=igr; iigt=igt; iigb=igb; iigw=igw; iigwh=igwh
          CALL win3d(iimenu,iiw1,iiw2,iiw3,iiw4,
     &      iigl,iigr,iigt,iigb,iigw,iigwh)
          igl=int(iigl); igr=int(iigr); igt=int(iigt); igb=int(iigb)
          igw=int(iigw); igwh=int(iigwh)
          if(IFS.ne.IMFS)call winfnt(IFS)
          call startbuffer()

C Debug.
C          write(6,*)'xx igl igr igt igb igw igwh',
C     &      igl,igr,igt,igb,igw,igwh

          mxmn=.TRUE.
          CALL CLMDRW(IPICK,mxmn,onleft,onright,'s')
          havedrawn = .true.
        endif
      elseif(INO.eq.24)then
        helptopic='climate_list_facility'
        call gethelptext(helpinsub,helptopic,nbhelp)
        CALL PHELPD('clm list mgt',nbhelp,'-',0,0,IER)
      else
        INO=-4
        goto 3
      endif
      INO=-4
      goto 3
      
      end
 
C ************ mkitemclimatelist 
C Write an item for climatelist file to previously opened file on unit
C iuf, return ier=0 if ok. This item can either
C be as a fragment to be included in the climatelist file by
C the user or as part of an item management facility.

C << consider how to make this a common code for use in writing
C << to configuration file as well as climatelist and climate block

      subroutine mkitemclimatelist(iuf,ier)

C Manual inclusion of a parameter from building.h
      parameter (MSPS=10)
      parameter (MIPVA=MSPS)
      
#include "seasons.h"
C seasons.h provides typper and typsea
      
      integer lnblnk  ! function definition

      character clmlnam*32,clmlaid*72,clmldbfile*144,clmlavail*10
      character clmlhelp*72
      common/clmltext/clmlnam,clmlaid,clmldbfile,clmlavail,clmlhelp(60)
      common/clmlnum/nbclmlhelp
      COMMON/CLMDT1/CLMLOC
      COMMON/SET1/IYEAR,IBDOY,IEDOY,IFDAY,IFTIME

      character tab*1
      CHARACTER CLMLOC*30
      character descr*7,descrst*10,descrfn*10  ! for season dates
      character descrst1*10,descrfn1*10        ! for 2nd season dates

      tab=CHAR(9) 
     
C Make up the entry. If clmlnam and clmlaid has not be set fill it
C with default information.
      write(iuf,'(a)')'*item'
      if(clmlnam(1:15).eq.'not yet entered')then
        write(clmlnam,'(2a)') CLMLOC(1:lnblnk(CLMLOC)),'  '
      endif
      write(iuf,'(3a)')'*name',tab,clmlnam(1:lnblnk(clmlnam))
      if(clmlaid(1:15).eq.'not yet entered')then
        write(clmlaid,'(2a)') CLMLOC(1:lnblnk(CLMLOC)),
     &    ' was from an unspecified source.'
      endif
      write(iuf,'(3a)')'*aide',tab,clmlaid(1:lnblnk(clmlaid))
      write(iuf,'(3a)')'*dbfl',tab,clmldbfile(1:lnblnk(clmldbfile))
      write(iuf,'(3a)')'*avail',tab,clmlavail(1:lnblnk(clmlavail))
      call edayr(is1wins,id1,im1)
      call edayr(is1winf,id2,im2)
      call edayr(is2wins,id3,im3)
      call edayr(is2winf,id4,im4)
      call stdate(iyear,is1wins,descr,descrst)
      call stdate(iyear,is1winf,descr,descrfn)
      call stdate(iyear,is2wins,descr,descrst1)
      call stdate(iyear,is2winf,descr,descrfn1)
      write(iuf,'(a,8i3,8a)')'*winter_s',id1,im1,id2,im2,id3,im3,
     &  id4,im4,' # seasons ',descrst,'--',descrfn,' & ',descrst1,
     &    '--',descrfn1
      call edayr(is1sprs,id1,im1)
      call edayr(is1sprf,id2,im2)
      call edayr(is2sprs,id3,im3)
      call edayr(is2sprf,id4,im4)
      call stdate(iyear,is1sprs,descr,descrst)
      call stdate(iyear,is1sprf,descr,descrfn)
      call stdate(iyear,is2sprs,descr,descrst1)
      call stdate(iyear,is2sprf,descr,descrfn1)
      write(iuf,'(a,8i3,8a)')'*spring_s',id1,im1,id2,im2,id3,im3,
     &  id4,im4,' # seasons ',descrst,'--',descrfn,' & ',descrst1,
     &  '--',descrfn1
      call edayr(is1sums,id1,im1)
      call edayr(is1sumf,id2,im2)
      call stdate(iyear,is1sums,descr,descrst)
      call stdate(iyear,is1sumf,descr,descrfn)
      write(iuf,'(a,4i3,4a)')'*summer_s',id1,im1,id2,im2,
     &  ' # summer season ',descrst,'--',descrfn
      call edayr(ia1wins,id1,im1)
      call edayr(ia1winf,id2,im2)
      call edayr(ia2wins,id3,im3)
      call edayr(ia2winf,id4,im4)
      call stdate(iyear,ia1wins,descr,descrst)
      call stdate(iyear,ia1winf,descr,descrfn)
      call stdate(iyear,ia2wins,descr,descrst1)
      call stdate(iyear,ia2winf,descr,descrfn1)
      write(iuf,'(a,8i3,8a)')'*winter_t',id1,im1,id2,im2,id3,im3,
     &  id4,im4,' # typical ',descrst,'--',descrfn,' & ',descrst1,
     &  '--',descrfn1
      call edayr(ia1sprs,id1,im1)
      call edayr(ia1sprf,id2,im2)
      call edayr(ia2sprs,id3,im3)
      call edayr(ia2sprf,id4,im4)
      call stdate(iyear,ia1sprs,descr,descrst)
      call stdate(iyear,ia1sprf,descr,descrfn)
      call stdate(iyear,ia2sprs,descr,descrst1)
      call stdate(iyear,ia2sprf,descr,descrfn1)
      write(iuf,'(a,8i3,8a)')'*spring_t',id1,im1,id2,im2,id3,im3,
     &  id4,im4,' # typical ',descrst,'--',descrfn,' & ',descrst1,
     &  '--',descrfn1
      call edayr(iasums,id1,im1)
      call edayr(iasumf,id2,im2)
      call stdate(iyear,iasums,descr,descrst)
      call stdate(iyear,iasumf,descr,descrfn)
      write(iuf,'(a,4i3,4a)')'*summer_t',id1,im1,id2,im2,
     &  ' # typical summer ',descrst,'--',descrfn
      write(iuf,'(a)')'*help_start'
      if(nbclmlhelp.eq.0)then
        write(iuf,'(a)')' Supporting text has yet to be entered.'
      else
        do 42 ij=1,nbclmlhelp
          write(iuf,'(a)') clmlhelp(ij)(1:lnblnk(clmlhelp(ij)))
  42    continue
      endif
      write(iuf,'(a)')'*help_end'

      return
      end
 
C ******* DDRADSUM *******
C DDRADSUM looks for typical week in each season based on closest
C degree days and radiation patterns. If act = `?` then user is
C asked to confirm whether the best-fit week is to be used in the
C climatelist.  It calls clmddscan to do most of the work.
      SUBROUTINE DDRADSUM(act)
#include "building.h"
#include "seasons.h"
C seasons.h provides typper, typsea, sea* variables

      COMMON/OUTPCH/ICOUT
      COMMON/SET1/IYEAR,IBDOY,IEDOY,IFDAY,IFTIME
      common/exporttgi/ixopen,ixloc,ixunit

      character outs*124
      character act*1

      real HBT,CBT   ! local base temperatures for heating and cooling DD calcs

C For help messages
      character helpinsub*24,helptopic*24 ! strings for help
      integer nbhelp     ! number of help lines found

      helpinsub='DDRADSUM'  ! set for cfiles

      if(is1wins.eq.0.or.is2wins.eq.0.or.is1sprs.eq.0)then
        call usrmsg('No winter|transition|summer season definitions',
     &    'found in the weather database. Skipping.','W')
        return
      endif

C Initial weightings for heating dd, cooling dd, solar radiation (from seasons.h).
      hddw=1.0
      cddw=1.0
      radw=1.0

      helptopic='dd_base_temp'
      call gethelptext(helpinsub,helptopic,nbhelp)
      if(hddbaset.lt.0.1)then
        hddbaset=17.0
      endif
      HBT=hddbaset
      CALL EASKR(HBT,' ',' Heating base temperature ? ',
     &  -10.,'W',40.,'W',17.0,'DD heating base temp',IER,nbhelp)
      hddbaset=HBT

      if(cddbaset.lt.0.1)then
        cddbaset=21.0
      endif
      CBT=cddbaset
      CALL EASKR(CBT,' ',' Cooling base temperature ? ',
     &  -10.,'W',40.,'W',21.0,'DD cooling base temp',IER,nbhelp)
      cddbaset=CBT

C If output to file alter the edisp unit number.
      itru = icout
      if(ixopen.eq.1)then
        itru = ixunit
        call usrmsg(' Output being directed to file... ',' ','-')
      endif

C Reset text feedback and write out beginning of report.
      CALL CHEAD(30)
      WRITE(outs,12) hddbaset,cddbaset
  12  FORMAT(' Degree day analysis: heating base at',F6.1,' & cooling',
     &       F6.1,' Deg C')
      call edisp(itru,outs)
      call edisp(itru,'  ')
      
C Loop through the weather file and calculate data.
      itrc=2
      call clmddscan(itrc,itru,'?','c',5,iyear)

      RETURN
      END
