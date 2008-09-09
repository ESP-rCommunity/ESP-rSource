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
      common/pophelp/h(60)
      CHARACTER h*72,hold*24

      H(1)='The period must be at leas an hour long. '
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
      common/pophelp/h(60)
      CHARACTER H*72,hold*24

      IER=0
  281 IF(IFDAY.EQ.0)THEN
        H(1)='Period  must be within a calander year'
        CALL EASKI(IDAY,' ',' d-o-y ? ',
     &             1,'F',365,'F',1,'start day',IER,1)
        CALL EDAYR(IDAY,ID,IM)
      ELSE
        CALL EDAYR(IDAY,ID,IM)
        H(1)='Period  must be within a calander year'
        write(HOLD,'(I6,I4)') ID,IM
 283    CALL EASKS(HOLD,' ',' Day-of-month & month: ',
     &     24,' 1  1 ','doy, month',IER,1)
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
      COMMON/OUTIN/IUOUT,IUIN
      COMMON/SET1/IYEAR,IBDOY,IEDOY,IFDAY,IFTIME
      COMMON/PERC/ID1,IM1,IT1,ID2,IM2,IT2,IDS,IDF,INEW
      COMMON/CLMDT0/CLAT,CLONG

C Typical periods win1|trn1|sum|trn2|win2 with start & finish
      common/typper/ia1wins,ia1winf,ia1sprs,ia1sprf,
     &  iasums,iasumf,ia2sprs,ia2sprf,ia2wins,ia2winf

C Season definitions. 2 periods for winter (i.e. nov-dec and
C jan-feb), transition (i.e. mar-may & sep-oct) and one period for summer.
      common/typsea/is1wins,is1winf,is2wins,is2winf,is1sprs,is1sprf,
     &              is2sprs,is2sprf,is1sums,is1sumf
      common/pophelp/h(60)
      COMMON/PERPER/PERST2
      dimension pmenu(29)
      CHARACTER h*72,pmenu*24
      character PERST1*14,PERST2*44,PERST3*44

C Assume whole day requested.
      ier=0
   3  IT1=1
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
        ID1=1
        IM1=1
        ID2=31
        IM2=1
      elseif(INO.EQ.3)then
        ID1=1
        IM1=2
        ID2=28
        IM2=2
      elseif(INO.EQ.4)then
        ID1=1
        IM1=3
        ID2=31
        IM2=3
      elseif(INO.EQ.5)then
        ID1=1
        IM1=4
        ID2=30
        IM2=4
      elseif(INO.EQ.6)then
        ID1=1
        IM1=5
        ID2=31
        IM2=5
      elseif(INO.EQ.7)then
        ID1=1
        IM1=6
        ID2=30
        IM2=6
      elseif(INO.EQ.8)then
        ID1=1
        IM1=7
        ID2=31
        IM2=7
      elseif(INO.EQ.9)then
        ID1=1
        IM1=8
        ID2=31
        IM2=8
      elseif(INO.EQ.10)then
        ID1=1
        IM1=9
        ID2=30
        IM2=9
      elseif(INO.EQ.11)then
        ID1=1
        IM1=10
        ID2=31
        IM2=10
      elseif(INO.EQ.12)then
        ID1=1
        IM1=11
        ID2=30
        IM2=11
      elseif(INO.EQ.13)then
        ID1=1
        IM1=12
        ID2=31
        IM2=12
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
        ID1=1
        IM1=1
        ID2=31
        IM2=12
      elseif(INO.EQ.28)then

C Help.
        H(1)='Clm periods can be user defined, months or typical'
        H(2)='periods or seasons associated with a climate db (and'
        H(3)='specified in the /usr/esru/esp-r/climate/climatelist'
        H(4)='file).'
        H(5)='The year associated with a climate file can be used '
        H(6)='as the date within a problem configuration, but this '
        H(7)='is not the default. '
        CALL PHELPD('clm periods',7,'-',0,0,IER)
        ino=-1
        goto 4
      elseif(INO.EQ.29)then

C If no option is selected, select whole year
        ID1=1
        IM1=1
        ID2=31
        IM2=12
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

      IER=0
      H(1)='The output time defines the day, month and '
      H(2)='hour at which the output is requested. '
      H(3)='This must be no earlier than 0 hours or later '
      H(4)='than 24 hours. '
      H(5)=' '

  281 IF(IFDAY.EQ.0)THEN
        H(6)='Example:  6th March at 9h00 am is given as 64  9 '
        write(HOLD,'(I6,I4)') IDS,IT1
        CALL EASKS(HOLD,' ',' Start Day-of-year & time: ',
     &     24,' 1  1 ','start doy and time',IER,6)
        K=0
        CALL EGETWI(HOLD,K,IDS,1,365,'F','start day of year',IER)
        CALL EGETWI(HOLD,K,IT1,0,24,'F','hour',IER)
        if(IER.ne.0)goto 281
        CALL EDAYR(IDS,ID1,IM1)

        write(HOLD,'(I6,I4)') IDF,IT2
        CALL EASKS(HOLD,' ',' End Day-of-year & time: ',
     &     24,' 364  24 ','end doy and time',IER,6)
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
     &     24,' 1  1  7.0 ','doy, month time',IER,6)
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
     &     24,' 31  12  24 ','end doy, month time',IER,6)
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
      
      integer lnblnk  ! function definition

      COMMON/FILEP/IFIL
      common/OUTIN/IUOUT,IUIN
      COMMON/SPAD/MMOD,LIMIT,LIMTTY
      common/pophelp/h(60)
      COMMON/GFONT/IFS,ITFS,IMFS
      integer menuchw,igl,igr,igt,igb,igw,igwh
      COMMON/VIEWPX/menuchw,igl,igr,igt,igb,igw,igwh
C      common/appw/iappw,iappx,iappy

      COMMON/PERC/ID1,IM1,IT1,ID2,IM2,IT2,IDS,IDF,INEW
      COMMON/CLMDT1/CLMLOC
      COMMON/CLMDT0/CLAT,CLONG
      COMMON/SET1/IYEAR,IBDOY,IEDOY,IFDAY,IFTIME
      COMMON/CLMDAT/NDAY,CDIF(MT),CTMP(MT),CDNR(MT),CVEL(MT),
     &              CDIR(MT),CHUM(MT)
C Typical periods and season definitions.
      common/typper/ia1wins,ia1winf,ia1sprs,ia1sprf,
     &  iasums,iasumf,ia2sprs,ia2sprf,ia2wins,ia2winf
      common/typsea/is1wins,is1winf,is2wins,is2winf,is1sprs,is1sprf,
     &              is2sprs,is2sprf,is1sums,is1sumf
      common/clmltext/clmlnam,clmlaid,clmldbfile,clmlavail,clmlhelp(60)
      common/clmlnum/nbclmlhelp

C External text editor.
      common/texted/tedlbl,teditor

C ITM is main item editing menu.
      character ITM(25)*38
      character NMTHNM(12)*3,DAYNAM(7)*3
      character descr*7,descrst*10,descrfn*10,descrstn*10
      character descrstp*10,h*72
      character clmlnam*32,clmlaid*72,clmldbfile*72,clmlavail*10
      character clmlhelp*72,DFILE*72,outs*124,louts*248
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
     
      DATA NMTHNM/'Jan','Feb','Mar','Apr','May','Jun','Jul',
     &'Aug','Sep','Oct','Nov','Dec'/
      DATA DAYNAM/'Mon','Tue','Wed','Thu','Fri','Sat','Sun'/

C Check if Unix-based or DOS based.
      call isunix(unixok)
      havedrawn = .false.
      altered = .false.

C Graphic analysis. Enlarge graphics area slightly and reset graphic
C display period to the whole year.
      if(MMOD.EQ.8)then
        ID1=1
        IM1=1
        ID2=31
        IM2=12
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
      WRITE(ITM(17),'(A)')     '< scan climate for best-fit weeks'
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
          IPICK(1)=0
          IPICK(2)=1
          IPICK(3)=0
          IPICK(4)=0
          IPICK(5)=0
          IPICK(6)=0
          onleft=1
          onright=0
          if(IFS.ne.IMFS)call winfnt(IMFS)
          CALL win3d(menuchw,6,12,6,3,igl,igr,igt,igb,igw,igwh)
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
      h(1)='How to use the climate list facility: '
      h(2)=' '
      h(3)='Climate data resides in individual files and ESP includes'
      h(4)='a `climatelist` file which holds high level information'
      h(5)='about where climate these files are located as well as'
      h(6)='summary information on seasons and typical periods.'
      h(7)=' '
      h(8)='This menu allows you to set the definition of seasons'
      h(9)='during a year - winter @ start of year, spring, summer'
      h(10)='autumn, winter @ end of year (or summer @ start of year'
      h(11)='etc. at negative latitudes). One way to do this is to'
      h(12)='use your own criteria when scanning the heating and'
      h(13)='cooling degree day weekly reports, ambient temperature'
      h(13)='reports or graphs etc in this climate module. Then'
      h(14)='record the start and ending dates of each season.'
      h(15)=' '
      h(16)='This menu also allows you to specify typical assessment'
      h(17)='periods (one per season). The clm module includes an'
      h(18)='automaitc search for weeks which are closest to the'
      h(19)='seasonal values of (use weighted) heating and cooling'
      h(20)='degree days & solar radiation. If you use that facility' 
      h(21)='a typical week in each season will be found and can be'
      h(22)='saved into the `climatelist` file. '
      h(23)=' '
      h(24)='If you have a text editor defined you can edit the text'
      h(25)='created by the list/generate menu selection. You can'
      h(26)='also edit the climatelist file. BE CAREFUL if you'
      h(27)='corrupt the file then you may get an incomplete list.'
      h(28)=' '
      h(29)='If you want to see the context of the seasons use the'
      h(30)='option to graph ambient temperatures. This will also'
      h(31)='show you symbols along the top indicating the duration'
      h(32)='of each season as well as the typcial weeks in each'
      h(33)='season (drawn slightly below). '

      CALL EMENU('Climate list item',ITM,nitms,INO)
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
        H(1)='Text for the selection list menu (<=32 char). '
        t32=clmlnam
        if(t32(1:15).eq.'not yet entered')write(t32,'(2a)') CLMLOC,'  '
        write(d32,'(2a)') CLMLOC,'  '
        CALL EASKS(t32,'Text for the selection list menu',' ',32,
     &    d32,'clmlist name',IER,1)
        if(t32(1:2).ne.'  ')clmlnam=t32
      elseif(INO.eq.2)then

C Aide memorie for the menu.
        H(1)='Aide Memorie for the selection list menu (<=72 char). '
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

C Installed location of climate file. If GTK then use file browser.
        H(1)='This is the `installed` location of the climate file.'
        h(2)='(remember to copy new climate files to this folder)'
        write(llasci,'(a)') lclim(1:lnblnk(lclim))
C        write(6,*) llasci
  290   llt=lnblnk(llasci)
        iglib = igraphiclib()  ! find out if X11 or GTK or text support only.
        if(iglib.eq.1.or.iglib.eq.3)then
          if(llt.lt.96)then
            CALL EASKF(LLASCI,' Climate file installed location?',
     &        ' ',96,DCLIM,'climate file location',IER,3)
          elseif(llt.ge.96.and.llt.lt.124)then
            CALL EASKF(LLASCI,' Climate file installed location?',
     &        ' ',124,DCLIM,'climate file location',IER,3)
          elseif(llt.ge.124.and.llt.le.144)then
            CALL EASKF(LLASCI,' Climate file installed location?',
     &        ' ',144,DCLIM,'climate file location',IER,3)
          endif
        elseif(iglib.eq.2)then
          CALL EASKF(LLASCI,' Climate file installed location?',
     &      ' ',144,DCLIM,'climate file location',IER,3)
        else
          CALL EASKF(LLASCI,' Climate file installed location?',
     &      ' ',96,DCLIM,'climate file location',IER,3)
        endif
        IF(LLASCI(1:2).EQ.'  '.or.LLASCI(1:4).eq.'UNKN')GOTO 290

C Cast back to 72 char name.
        llt=MIN0(lnblnk(llasci),72)
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

        h(1)='Set to OFFLINE if climate files tend to be kept as'
        h(2)='compressed files or are held in a secure location. '
        call easkabc('Choices:',' ','online','offline','continue',IW,2)
        if(iw.eq.1) clmlavail = 'ONLINE'
        if(iw.eq.2) clmlavail = 'OFFLINE'
      elseif(INO.eq.6)then

C 1st winter season
C Edit the current period in terms of day and month. Also check to
C see if next period overlaps and correct.
   91   hold = ' '
        H(1)='Season start day & month and finish day and month. '
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
     &    'finish (day & month):',40,' 1 1 28 2','season',IIER,1)
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
        H(1)='Season start day & month and finish day and month. '
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
     &    'finish (day & month):',40,' 1 3 30 4','season',IIER,1)
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
        H(1)='Season start day & month and finish day and month. '
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
     &    'finish (day & month):',40,' 1 5 31 8','season',IIER,1)
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
        H(1)='Season start day & month and finish day and month. '
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
     &    'finish (day & month):',40,' 1 9 31 10','season',IIER,1)
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
        H(1)='Season start day & month and finish day and month. '
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
     &      DAYNAM(jjd4t),' and 1st winter starts on a ',
     &      DAYNAM(jjd1t),' so consider ',descrstp,' or ',
     &      descrstn
          call edisp248(iuout,louts,80)
        endif
        WRITE(HOLD,'(4I4)')id1,im1,id2,im2
        CALL EASKS(HOLD,'Season: start (day & month) and',
     &    'finish (day & month):',40,' 1 11 31 12','season',IIER,1)
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
        H(1)='Period start day & month and finish day and month. '
        call stdate(iyear,ia1wins,descr,descrst)
        call stdate(iyear,ia1winf,descr,descrfn)
        WRITE(outs,'(4a)')  ' 1st winter typical ',descrst,' - ',
     &    descrfn
        call edisp(iuout,outs)
        call edayr(ia1wins,id1,im1)
        call edayr(ia1winf,id2,im2)
        WRITE(HOLD,'(4I4)')id1,im1,id2,im2
        CALL EASKS(HOLD,'Typical: start (day & month) and',
     &    'finish (day & month):',40,' 9 1 15 1','typical',IIER,1)
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
        H(1)='Period start day & month and finish day and month. '
        call stdate(iyear,ia1sprs,descr,descrst)
        call stdate(iyear,ia1sprf,descr,descrfn)
        WRITE(outs,'(4a)')  ' Spring typical ',descrst,' - ',
     &    descrfn
        call edisp(iuout,outs)
        call edayr(ia1sprs,id1,im1)
        call edayr(ia1sprf,id2,im2)
        WRITE(HOLD,'(4I4)')id1,im1,id2,im2
        CALL EASKS(HOLD,'Typical: start (day & month) and',
     &    'finish (day & month):',40,' 6 3 12 3','typical',IIER,1)
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
        H(1)='Period start day & month and finish day and month. '
        call stdate(iyear,iasums,descr,descrst)
        call stdate(iyear,iasumf,descr,descrfn)
        WRITE(outs,'(4a)')  ' Summer typical ',descrst,' - ',
     &    descrfn
        call edisp(iuout,outs)
        call edayr(iasums,id1,im1)
        call edayr(iasumf,id2,im2)
        WRITE(HOLD,'(4I4)')id1,im1,id2,im2
        CALL EASKS(HOLD,'Typical: start (day & month) and',
     &    'finish (day & month):',40,' 11 7 17 7','typical',IIER,1)
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
        H(1)='Period start day & month and finish day and month. '
        call stdate(iyear,ia2sprs,descr,descrst)
        call stdate(iyear,ia2sprf,descr,descrfn)
        WRITE(outs,'(4a)')  ' Autumn typical ',descrst,' - ',
     &    descrfn
        call edisp(iuout,outs)
        call edayr(ia2sprs,id1,im1)
        call edayr(ia2sprf,id2,im2)
        WRITE(HOLD,'(4I4)')id1,im1,id2,im2
        CALL EASKS(HOLD,'Typical: start (day & month) and',
     &    'finish (day & month):',40,' 2 10 8 10','typical',IIER,1)
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
        H(1)='Period start day & month and finish day and month. '
        call stdate(iyear,ia2wins,descr,descrst)
        call stdate(iyear,ia2winf,descr,descrfn)
        WRITE(outs,'(4a)')  ' 2nd winter typical ',descrst,' - ',
     &    descrfn
        call edisp(iuout,outs)
        call edayr(ia2wins,id1,im1)
        call edayr(ia2winf,id2,im2)
        WRITE(HOLD,'(4I4)')id1,im1,id2,im2
        CALL EASKS(HOLD,'Typical: start (day & month) and',
     &    'finish (day & month):',40,' 20 11 26 11','typical',IIER,1)
        K=0
        CALL EGETWI(HOLD,K,id1,1,31,'W','typical st day',IIER)
        CALL EGETWI(HOLD,K,im1,1,12,'W','typical st month',IIER)
        CALL EGETWI(HOLD,K,id2,1,31,'W','typical fn day',IIER)
        CALL EGETWI(HOLD,K,im2,1,12,'W','typical fn month',IIER)
        CALL EDAY(id1,im1,ia2wins)
        CALL EDAY(id2,im2,ia2winf)
        if(iier.ne.0)goto 100
      elseif(INO.eq.17)then

C Scan climate for best-fit weeks.
          call DDRADSUM('?')
      elseif(INO.eq.19)then

C Documentations text option.
        h(1)='The list option displays each of the lines of '
        h(2)='documentation. The initialise option creates a '
        h(3)='standard summary of the climate data. The Edit '
        h(4)='option is not yet enabled (save and use a text editor).'
        call easkabcd('Documentation options:',' ','List','Initialise',
     &    'Edit','Continue',idw,4)
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
          write(clmlhelp(1),'(2a)') 'Climate is ',clmloc 
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
          ID1=1
          IM1=1
          IT1=1
          ID2=31
          IM2=12
          IT2=24
          IDS=1
          IDF=365
          KDS=1
          KDF=24
          XMAXI=-1E+10
          XMINI=1E+10
          XMEANI=0
          HRTOT=0.

C INITIALISE PARAMETERS FOR MONTHLY TOTALS.
          MTH1=IM1
          XMAXM=-1E10
          XMINM=1E10
          XMEANM=0.
          HRM=0.

C Find day of week for start of the period.
          CALL EDAYR(IDS,IDAYN,IMTHN)
      
C Loop for each day, reading data from climate file.
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
        h(1)='Information on the current climate file (seasons etc.)'
        h(2)='will be written to a small ASCII file which can be'
        h(3)='copied into /usr/esru/esp-r/climatelist file. '
        llt=lnblnk(llitem)
        iglib = igraphiclib()  ! find out if X11 or GTK or text support only.
        if(iglib.eq.1.or.iglib.eq.3)then
          if(llt.lt.96)then
            CALL EASKF(llitem,' Climatelist entry text block?',
     &        ' ',96,DFILE,'climatelist entry file',IER,3)
          elseif(llt.ge.96.and.llt.lt.124)then
            CALL EASKF(llitem,' Climatelist entry text block?',
     &        ' ',124,DFILE,'climatelist entry file',IER,3)
          elseif(llt.ge.124.and.llt.le.144)then
            CALL EASKF(llitem,' Climatelist entry text block?',
     &        ' ',144,DFILE,'climatelist entry file',IER,3)
          endif
        elseif(iglib.eq.2)then
          CALL EASKF(llitem,' Climatelist entry text block?',
     &      ' ',144,DFILE,'climatelist entry file',IER,3)
        else
          CALL EASKF(llitem,' Climatelist entry text block?',
     &      ' ',96,DFILE,'climatelist entry file',IER,3)
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
          IPICK(1)=0
          IPICK(2)=1
          IPICK(3)=0
          IPICK(4)=0
          IPICK(5)=0
          IPICK(6)=0
          onleft=1
          onright=0
          if(IFS.ne.IMFS)call winfnt(IMFS)
          CALL win3d(menuchw,6,12,6,3,igl,igr,igt,igb,igw,igwh)
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
        CALL PHELPD('clm list mgt',33,'-',0,0,IER)
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
      subroutine mkitemclimatelist(iuf,ier)
      
      integer lnblnk  ! function definition

C Typical periods and season definitions.
      common/typper/ia1wins,ia1winf,ia1sprs,ia1sprf,
     &  iasums,iasumf,ia2sprs,ia2sprf,ia2wins,ia2winf
      common/typsea/is1wins,is1winf,is2wins,is2winf,is1sprs,is1sprf,
     &              is2sprs,is2sprf,is1sums,is1sumf
      common/clmltext/clmlnam,clmlaid,clmldbfile,clmlavail,clmlhelp(60)
      common/clmlnum/nbclmlhelp
      COMMON/CLMDT1/CLMLOC

      character clmlnam*32,clmlaid*72,clmldbfile*72,clmlavail*10
      character clmlhelp*72,tab*1
      CHARACTER CLMLOC*30

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
      write(iuf,'(a,8i3)')'*winter_s',id1,im1,id2,im2,id3,im3,id4,im4
      call edayr(is1sprs,id1,im1)
      call edayr(is1sprf,id2,im2)
      call edayr(is2sprs,id3,im3)
      call edayr(is2sprf,id4,im4)
      write(iuf,'(a,8i3)')'*spring_s',id1,im1,id2,im2,id3,im3,id4,im4
      call edayr(is1sums,id1,im1)
      call edayr(is1sumf,id2,im2)
      write(iuf,'(a,4i3)')'*summer_s',id1,im1,id2,im2
      call edayr(ia1wins,id1,im1)
      call edayr(ia1winf,id2,im2)
      call edayr(ia2wins,id3,im3)
      call edayr(ia2winf,id4,im4)
      write(iuf,'(a,8i3)')'*winter_t',id1,im1,id2,im2,id3,im3,id4,im4
      call edayr(ia1sprs,id1,im1)
      call edayr(ia1sprf,id2,im2)
      call edayr(ia2sprs,id3,im3)
      call edayr(ia2sprf,id4,im4)
      write(iuf,'(a,8i3)')'*spring_t',id1,im1,id2,im2,id3,im3,id4,im4
      call edayr(iasums,id1,im1)
      call edayr(iasumf,id2,im2)
      write(iuf,'(a,4i3)')'*summer_t',id1,im1,id2,im2
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
C climatelist.
      SUBROUTINE DDRADSUM(act)
#include "building.h"
      
      integer lnblnk  ! function definition

      COMMON/OUTIN/IUOUT,IUIN
      COMMON/SPAD/MMOD,LIMIT,LIMTTY
      COMMON/OUTPCH/ICOUT
      common/pophelp/h(60)
      COMMON/SET1/IYEAR,IBDOY,IEDOY,IFDAY,IFTIME
      COMMON/CLMDT0/CLAT,CLONG
      COMMON/RADTYP/IDNGH
      common/exporttgi/ixopen,ixloc,ixunit

      COMMON/CLMDAT/NDAY,CDIF(MT),CTMP(MT),CDNR(MT),CVEL(MT),
     &              CDIR(MT),CHUM(MT)

C Season definitions. 2 periods for winter (i.e. nov-dec and
C jan-feb), transition (i.e. mar-may & sep-oct) and one period for summer.
      common/typsea/is1wins,is1winf,is2wins,is2winf,is1sprs,is1sprf,
     &              is2sprs,is2sprf,is1sums,is1sumf
      common/typper/ia1wins,ia1winf,ia1sprs,ia1sprf,
     &  iasums,iasumf,ia2sprs,ia2sprf,ia2wins,ia2winf

C dm* are the degree-day and day-ratio multipliers between the
C typical periods and the whole season for heating (DD), cooling (DD),
C lights (ratio), small power (ratio), fans (ratio), domestic hot
C water (ratio). For nipvassmt=1 these are initially set to 1.0.
C This information is potentially used to support IPV calculations.
      common/CLMDM/dmheat(MSPS),dmcool(MSPS),dmlight(MSPS),
     &             dmsmlpw(MSPS),dmfan(MSPS),dmdhw(MSPS)

      character H*72,outs*124,PERST1*14,PERST2*44,PERST3*44
      character DESCR*7,DESCR1*10,title*40,hold*32,act*1
      DIMENSION ALT(24),AZI(24),GHSUM(MSPS),DAYGLOB(MSPS)
      dimension seacddwk(MSPS),seahddtot(MSPS),seacddtot(MSPS)
      dimension seahddwk(MSPS)
      logical ok,dok

      if(is1wins.eq.0.or.is2wins.eq.0.or.is1sprs.eq.0)then
        call usrmsg('No winter|transition|summer season definitions',
     &    'found in the climate database. Skipping.','W')
        return
      endif

C Initial weightings for heading dd, cooling dd, solar radiation.
      hddw=1.0
      cddw=1.0
      radw=1.0

      H(1)='Degree day calculations are derived from a specific '
      H(2)='base temperature (deg C). '
      HBT=17.
      CALL EASKR(HBT,' ',' Heating base temperature ? ',
     &           -10.,'W',40.,'W',17.0,'DD heating base temp',IER,2)
      CBT=21.
      CALL EASKR(CBT,' ',' Cooling base temperature ? ',
     &           -10.,'W',40.,'W',21.0,'DD cooling base temp',IER,2)

C If output to file alter the edisp unit number.
      itru = icout
      if(ixopen.eq.1)then
        itru = ixunit
        call usrmsg(' Output being directed to file... ',' ','-')
      endif

C Reset text feedback.
      CALL CHEAD(30)
      DNM=0.
      HRM=0.
      UPMC=0.
      HRMC=0.

      WRITE(outs,12)HBT,CBT
  12  FORMAT(' Degree day analysis: heating base at',F6.1,' & cooling',
     &       F6.1,' Deg C')
      call edisp(itru,outs)
      call edisp(itru,'  ')

C INITIALISE PARAMETERS FOR WHOLE PERIOD
      DNI=0
      HRTOT=0.
      UPIC=0
      HRTOTC=0.

C Loop through each seasonal period.
      do 42 k=1,5
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
        CALL EPERSTR(IYEAR,IDSS,0,IDSF,24,1,
     &             IFDAY,IFTIME,PERST1,PERST2,PERST3,IER)

C For each day read climate data, set parameters and analyse.
        DO 10 I=IDSS,IDSF
          IDD=I
          CALL CLMGET(IDD)
          CALL AZALT(IDD,1,CLONG,CLAT,ALT,AZI)
          DNJ=0
          UPJC=0.
          DAYL=24.0
          DO 20 J=1,24
            IF(ALT(J).LT.0.001)THEN
              DIRH=0.
            else
              IF(IDNGH.EQ.0)THEN
                DIRH=CDNR(J)*ESIND(ALT(J))
              ELSE
                DIRH=CDNR(J)-CDIF(J)
              ENDIF
              GHSUM(k)=(GHSUM(k)+DIRH+CDIF(J))
            endif

            TT=CTMP(J)
            IF(TT.GT.HBT)then
            else

C T below heating BASE,SAVE DEG-HRS IN DNJ
              DNJ=DNJ+(HBT-TT)
            endif
            IF(TT.GT.CBT)then

C T above cooling BASE ,SAVE DEG-HRS IN UPJC
              UPJC=UPJC+(TT-CBT)
            endif
   20     CONTINUE
          XDN=DNJ/24
          XUPC=UPJC/24

C Update seasonal parameters and go to the next day.
          DNM=DNM+DNJ
          HRM=HRM+DAYL
          UPMC=UPMC+UPJC
          HRMC=HRMC+DAYL
   10   CONTINUE

C Compute parameters (total & average degree days) for this season.
        XHR=24.0/HRM
        XHRC=24.0/HRMC
        XDN=DNM/24
        XUPC=UPMC/24
        YDN=XHR*XDN
        seahddwk(k)=YDN*7.
        YUPC=XHRC*XUPC
        seacddwk(k)=YUPC*7.

C Number of days in season is IDSF-IDSS+1. DAYGLOB is daily mean
C global horizontal solar for season.
        DAYGLOB(k)=(GHSUM(k)/(IDSF-IDSS+1))/1000.

C Report on the current season.
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
  42  continue

C Loop through each week in each season and check against the
C calculated average data.
C Loop through each seasonal period.
      h(1)='Typical weeks are found by scanning the climate data'
      h(2)='for a week in each season which has the least deviation'
      h(3)='in heating degree days (DD), cooling degree days, and'
      h(4)='solar radiation. '
      h(5)=' '
      h(6)='It reports the parameters it uses to determine the'
      h(7)='best fit as well as several ratios between the best'
      h(8)='week and the season (ratio of julian days, ratio of'
      h(9)='heating degree days and ratio of cooling degree days'
      h(10)='all of which are parameters required for generating'
      h(11)='an integrated performance view (IPV).'
      call easkab('Options:',' ','search for typical weeks',
     &  'continue',iok,11)
      if(iok.eq.2)return

C Allow user to define weightings.
      H(1)='Weightings for heating degree days, cooling degree days'
      H(2)='and solar radiation can be given. The default is equal'
      H(3)='weightings for each.'
      write(hold,'(3f6.3)') hddw,cddw,radw
 143  CALL EASKS(HOLD,'Weightings for HDD CDD Solar:',' ',32,
     &  ' 1.0 1.0 1.0 ','weightings for hdd cdd solar',IER,3)
      K=0
      CALL EGETWR(HOLD,K,hddw,0.0,1.0,'W','hdd weight',IER)
      CALL EGETWR(HOLD,K,cddw,0.0,1.0,'W','cdd weight',IER)
      CALL EGETWR(HOLD,K,radw,0.0,1.0,'W','solar weight',IER)
      if(ier.ne.0)goto 143
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
        iwkbest=0
        iwkbstrt=0
        wkdiff=3.
        GHSUMW=0.
        CALL EPERSTR(IYEAR,IDSS,0,IDSF,24,1,
     &             IFDAY,IFTIME,PERST1,PERST2,PERST3,IER)
        WRITE(outs,'(a)')PERST2
        call edisp(itru,outs)
        if(itru.ne.iuout)call edisp(iuout,outs)

C For each day read climate data, set parameters and analyse.
C Find day of week for start of the period. IWK is week number, IDSOW
C is the julian day at the start of the week.
        CALL EDAYR(IDSS,IDAYN,IMTHN)
        CALL EWEEKD(IDAYN,IMTHN,IYEAR,IXDWK)
        IDSOW=IDSS
        ix=0

C For each day read climate data, set parameters and analyse.
        DO 11 I=IDSS,IDSF
          IDD=I
          ix=ix+1
          CALL CLMGET(IDD)
          CALL AZALT(IDD,1,CLONG,CLAT,ALT,AZI)
          DNJ=0
          UPJC=0
          DO 21 J=1,24
            IF(ALT(J).LT.0.001)THEN
              DIRH=0.
            else
              IF(IDNGH.EQ.0)THEN
                DIRH=CDNR(J)*ESIND(ALT(J))
              ELSE
                DIRH=CDNR(J)-CDIF(J)
              ENDIF
              GHSUMW=(GHSUMW+DIRH+CDIF(J))
            endif

            TT=CTMP(J)
            IF(TT.GT.HBT)then
            else

C T below heating BASE,SAVE DEG-HRS IN DNJ
              DNJ=DNJ+(HBT-TT)
            endif
            IF(TT.GT.CBT)then

C T above cooling BASE ,SAVE DEG-HRS IN UPJC
              UPJC=UPJC+(TT-CBT)
            endif
   21     CONTINUE

C Convert day to D,M
          II=I
          CALL EDAYR(II,IDD,IMM)
          CALL EWEEKD(IDD,IMM,IYEAR,IDWK)

C Prepare for output, check if week complete. << IDS ? >>
          IF(II.eq.IDSS)GO TO 41
          IF(IDWK.ne.IXDWK)GO TO 41
          iwk=iwk+1

C Compute parameters (total & average degree days) for this week.
          XHR=24.0/HRM
          XHRC=24.0/HRMC
          XDN=DNM/24
          XUPC=UPMC/24
          call stdate(iyear,IDSOW,DESCR,DESCR1)
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
          call edisp(itru,outs)
          if(PARMTOT.lt.wkdiff)then
            wkdiff=PARMTOT
            iwkbest=iwk
            iwkbstrt=IDSOW
            wkheatdd=XDN
            wkcooldd=XUPC
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

C CONVERT TO DEG-DAYS PER DAY
  41      CONTINUE
          XDN=DNJ/24
          XUPC=UPJC/24

C UPDATE PARAMETERS FOR THIS week
          DNM=DNM+DNJ
          HRM=HRM+DAYL
          UPMC=UPMC+UPJC
          HRMC=HRMC+DAYL

C AND GO TO NEXT DAY
   11   CONTINUE

C NOW LAST week, compute parameters for last week
        XHR=24.0/HRM
        XHRC=24.0/HRMC
        XDN=DNM/24
        XUPC=UPMC/24
        iwk=iwk+1
        call stdate(iyear,IDSOW,DESCR,DESCR1)
        YDN=XHR*XDN
        YUPC=XHRC*XUPC
        WRITE(outs,'(a,i2,3a,2F8.2,a,F8.3)')' Week:',iwk,
     &    ' starting ',DESCR1,' DD total/week  ',XDN,XUPC,
     &    '  Rad total/week ',GHSUMW/1000.
        call edisp(itru,outs)
        if(itru.ne.iuout)call edisp(iuout,outs)

C Deal with radition here for last week of season.
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
        if(PARMTOT.lt.wkdiff)then
          wkdiff=PARMTOT
          iwkbest=iwk
          iwkbstrt=IDSOW
          wkheatdd=XDN
          wkcooldd=XUPC
        endif
        call stdate(iyear,iwkbstrt,DESCR,DESCR1)
        WRITE(outs,'(a,i3,3a,F6.2)')' Best week is nb ',iwkbest,' ',
     &    DESCR1,' with minimum weighted difference of ',wkdiff
        call edisp(itru,outs)
        if(itru.ne.iuout)call edisp(iuout,outs)

C Convert from julian day of the start of the assessment to the end
C of the assessment to number of days in period and get ratio with
C the number of days in the season.
        ijulst=iwkbstrt
        ijulfn=iwkbstrt+6

C Ratio of seasonal totoal heating and cooling degree days and the
C best assessment week heating and cooling degree days.
        if(wkheatdd.gt.0.01)then
          hddratio=seahddtot(k)/wkheatdd
        else
          hddratio=1.0
        endif
        if(wkcooldd.gt.0.01)then
          cddratio=seacddtot(k)/wkcooldd
        else
          cddratio=1.0
        endif
        if(k.eq.1)then
          dayratio=((is1winf-is1wins)+1)/((ijulfn-ijulst)+1)
          write(outs,'(a,F6.3)') 
     &      'early winter ratio of season/assessment days: ',dayratio
          call edisp(iuout,outs)
          write(outs,'(a,F6.3,A,F6.3)') 
     &      'early winter ratio of season/assessment heat DD: ',
     &      hddratio,' & cool DD:',cddratio
          call edisp(iuout,outs)
          if(act.eq.'?')then
            dok=.true.
            h(1)='Unless you have better information, say yes. '
            call askok('Use best-fit week for the 1st winter season',
     &       'typical assessment period in the climatelist?',ok,dok,1)
            if(ok)then
              ia1wins = iwkbstrt
              ia1winf = iwkbstrt+6
            endif
          endif
        elseif(k.eq.2)then
          dayratio=((is1sprf-is1sprs)+1)/((ijulfn-ijulst)+1)
C Debug...
          write(outs,'(a,F6.3)') 
     &      'spring ratio of season/assessment days: ',dayratio
          call edisp(iuout,outs)
          write(outs,'(a,F6.3,A,F6.3)') 
     &      'spring ratio of season/assessment heat DD: ',
     &      hddratio,' & cool DD:',cddratio
          call edisp(iuout,outs)
          if(act.eq.'?')then
            dok=.true.
            h(1)='Unless you have better information, say yes. '
            call askok('Use best-fit week for the spring season',
     &       'typical assessment period in the climatelist?',ok,dok,1)
            if(ok)then
              ia1sprs = iwkbstrt
              ia1sprf = iwkbstrt+6
            endif
          endif
        elseif(k.eq.3)then
          dayratio=((is1sumf-is1sums)+1)/((ijulfn-ijulst)+1)
          write(outs,'(a,F6.3)') 
     &      'summer ratio of season/assessment days: ',dayratio
          call edisp(iuout,outs)
          write(outs,'(a,F6.3,A,F6.3)') 
     &      'summer ratio of season/assessment heat DD: ',
     &      hddratio,' & cool DD:',cddratio
          call edisp(iuout,outs)
          if(act.eq.'?')then
            dok=.true.
            h(1)='Unless you have better information, say yes. '
            call askok('Use best-fit week for the summer season',
     &       'typical assessment period in the climatelist?',ok,dok,1)
            if(ok)then
              iasums = iwkbstrt
              iasumf = iwkbstrt+6
            endif
          endif
        elseif(k.eq.4)then
          dayratio=((is2sprf-is2sprs)+1)/((ijulfn-ijulst)+1)
          write(outs,'(a,F6.3)') 
     &      'autumn ratio of season/assessment days: ',dayratio
          call edisp(iuout,outs)
          write(outs,'(a,F6.3,A,F6.3)') 
     &      'autumn ratio of season/assessment heat DD: ',
     &      hddratio,' & cool DD:',cddratio
          call edisp(iuout,outs)
          if(act.eq.'?')then
            dok=.true.
            h(1)='Unless you have better information, say yes. '
            call askok('Use best-fit week for the autumn season',
     &       'typical assessment period in the climatelist?',ok,dok,1)
            if(ok)then
              ia2sprs = iwkbstrt
              ia2sprf = iwkbstrt+6
            endif
          endif
        elseif(k.eq.5)then
          dayratio=((is2winf-is2wins)+1)/((ijulfn-ijulst)+1)
          write(outs,'(a,F6.3)') 
     &      'late winter ratio of season/assessment days: ',dayratio
          call edisp(iuout,outs)
          write(outs,'(a,F6.3,A,F6.3)') 
     &      'late winter ratio of season/assessment heat DD: ',
     &      hddratio,' & cool DD:',cddratio
          call edisp(iuout,outs)
          if(act.eq.'?')then
            dok=.true.
            h(1)='Unless you have better information, say yes. '
            call askok('Use best-fit week for the 2nd winter season',
     &       'typical assessment period in the climatelist?',ok,dok,1)
            if(ok)then
              ia2wins = iwkbstrt
              ia2winf = iwkbstrt+6
            endif
          endif
        endif

C For the current season set degree day or day ratios.
        dmheat(k)=hddratio
        dmcool(k)=cddratio
        dmlight(k)=dayratio
        dmsmlpw(k)=dayratio
        dmfan(k)=dayratio
        dmdhw(k)=dayratio
        if(act.eq.'?')then
          continue
        else
          h(1)='You might wish to record this information before going'
          h(2)='to the next season.'
          call easkab('Options:',' ','look at next season','continue',
     &      iok,2)
          if(iok.eq.2)return
        endif
        ix=1
 43   continue

      RETURN
      END
