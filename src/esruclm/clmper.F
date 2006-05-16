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
