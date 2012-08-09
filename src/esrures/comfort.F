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

C This file contains the following routines:
C  MOCMFT provides a zone comfort assessment for a
C         particular 24 hour period.

C scnocup: Scans occupancy in each of the zones of the probem.
C getocup: 
C DISCOMF: Returns local dicomfort information (PPD).
C DRAUGHT: PPD due to draughts (nothing calls this yet).
C OFFSETTEMP: air velocity required to maintain comfort (nothing calls this yet).
C ASHRAEZONE: inside/ outside ASHRAE comfort zone (nothing calls this yet).

C ******************** MOCMFT ********************
C MOCMFT provides a zone comfort assessment for a
C particular 24 hour period.
C If act = 'V' then provide verbose (standard) comfort facility,
C if act = 'I' to set comfort parameters before calling D or M,
C if act = 'D' return days PPD in GVAL,
C if act = 'M' return days PMV in GVAL.
C if act = 'E' return days PMV (effective temperature) in GVAL.

      SUBROUTINE MOCMFT(IDAY,IZONE,act)
#include "building.h"
#include "geometry.h"
#include "espriou.h"
      
      integer lnblnk  ! function definition

      COMMON/SPAD/MMOD,LIMIT,LIMTTY
      COMMON/OUTIN/IUOUT,IUIN
      COMMON/OUTPCH/ICOUT

      COMMON/SIMPIK/ISIM,ISTADD,ID1,IM1,ID2,IM2,ISDS,ISDF,NTS,ISAVE
      common/C21/IFCFG,cfgroot,LCFGF
      character lodlabel*6
      common/loadlabel/lodlabel(mcom,MGTY)
      common/exporttg/xfile,tg,delim
      COMMON/EXPORTI/ixopen,ixunit,ixpunit

C Comfort parameters and hash table.
      common/cmftpar/WVEL,ACTL,CLO,iocut,iocuset,ioucpz(MCOM,72)

C Pre calculated data (filtered).
      common/MMAFDAT/IFILT,RDFMAX(MCOM,3,MZRL),RDFMIN(MCOM,3,MZRL),
     &    RDFAVE(MCOM,3,MZRL),RDTFMAX(MCOM,3,MZRL),RDTFMIN(MCOM,3,MZRL)

      COMMON/GET2/XDUM(MTS),XDUM1(MTS),GVAL(MTS)

      DIMENSION ID(12),OP(MS),TS(MS),TSO(MS),QV(MS),ITEMS(14)
      character doit*248,tmode*8,tfile*72
      character longtfile*144,longtfiledos*144
      character LCFGF*72,cfgroot*24
      CHARACTER*25 DESC(12)
      character outs*124,TSTR*24,ZN*12
      character xfile*144,ITEMS*25,tg*1,delim*1,act*1
      character*8 ll1,ll2,ll3
      logical ok,dok,concat,unixok
      integer IOT  ! for radio button
      integer NITMS,INO ! max items and current menu item

#ifdef OSI
      integer ICPMOD     ! passed from tchild
#else
      integer*8 ICPMOD     ! passed from tchild
#endif

C For help messages
      character helpinsub*24   ! subroutine name
      character helptopic*24 ! string (unique) for topic
      integer nbhelp     ! number of help lines found

      DATA DESC/'very cold, danger        ','cold, shivering          ',
     &          'cool, unpleasant         ','slightly cool, acceptable',
     &          'comfortable, pleasant    ','slightly warm, acceptable',
     &          'warm, unpleasant         ','hot, very uncomfortable  ',
     &          'very hot, danger         ','unoccupied               ',
     &          'non-sedentary            ','out of range             '/

      DATA ID/31,28,31,30,31,30,31,31,30,31,30,31/

C Check if Unix or Windows.
      helpinsub='MOCMFT'  ! set for subroutine
      call isunix(unixok)

C Initialise comfort variables.
      if(act.eq.'I'.or.act.eq.'V')then
        OP(1)=-1.
      endif

C If initial settings and initial request ask parameters.
      if(act.eq.'I')then
        if(iocuset.eq.1)then
          dok=.true.
          helptopic='res_alter_comfort_par'
          call gethelptext(helpinsub,helptopic,nbhelp)
          call askok(' ','Use existing comfort parameters?',ok,dok,
     &      nbhelp)
          if(ok)return
        endif

C Confirm parameters and then exit.
        call edisp(iuout,' ')
        call edisp(iuout,'Please confirm general comfort parameters.')
        helptopic='res_review_comfort_par'
        call gethelptext(helpinsub,helptopic,nbhelp)
        call easkr(CLO,' ',' Clothing level ?',
     &    0.0,'W',3.0,'W',1.0,'clothing level',IER,nbhelp)

        helptopic='res_review_activity_par'
        call gethelptext(helpinsub,helptopic,nbhelp)
        CALL EASKAB(' ','Activity level units (see help):',
     &    'MET units','W/m^2 (body surface area)',IAU,nbhelp)
        if(IAU.eq.1)then

C When using MET units, convert back to W/M^2 for calculations.
C Assume default MET is equiv to 85W/m^2.
          ACTL = ACTL/58.2
          ACTLDEF = 85.0/58.2
          call easkr(ACTL,' ',' Activity level (MET)?',0.859,
     &      'W',6.013,'W',ACTLDEF,'activity level MET',IER,nbhelp)
          ACTL = ACTL * 58.2
        elseif(IAU.eq.2)then
          call easkr(ACTL,' ',' Activity level (W/m2)?',
     &      50.0,'F',350.0,'F',85.0,'activity level W/m2',IER,nbhelp)
        endif

        helptopic='res_comfort_air_velo'
        call gethelptext(helpinsub,helptopic,nbhelp)
        call easkr(WVEL,' ',' Air velocity ?',
     &    0.0,'W',5.0,'W',0.15,'air velocity',IER,nbhelp)

C Set representative casual gain and then search all zones and build
C up hash list of zone:time:occupancy.

C << revise for additional casual gain types >>
        write(ll1,'(1x,a6,1x)') lodlabel(IZONE,1)
        write(ll2,'(1x,a6,1x)') lodlabel(IZONE,2)
        write(ll3,'(1x,a6,1x)') lodlabel(IZONE,3)
        helptopic='res_comfort_occupied'
        call gethelptext(helpinsub,helptopic,nbhelp)
        IOT=1
        CALL EASKATOG('Which casual gain represents occupancy:',' ',
     &      'Always occupied',ll1,ll2,ll3,'time',' ',' ',IOT,nbhelp)
        if (IOT.gt.1.and.IOT.lt.5) then
          iocut=IOT-1
        elseif(IOT.eq.1)then
          iocut=0
        elseif(IOT.eq.5)then
          iocut=-1
        endif
        iocuset=1
        call scnocup(iier)
        IFILT=iocut
        return
      endif

C Initial day.
      if(act.eq.'V')then
        CALL EDAYR(IDAY,IDCO,IMCO)
      else
        CALL EDAYR(IDAY,IDCO,IMCO)
      endif
      NSUR=NZSUR(IZONE)

C Begin with high level menu if in verbose mode.
      if(act.eq.'D'.or.act.eq.'E'.or.act.eq.'M') goto 78
    4 INO=-4
      IER=0
      WRITE(ITEMS(1),'(A,A)')      'a Zone: ',zname(IZONE)
      WRITE(ITEMS(2),'(A,2I3)')    'b Day & month :',IDCO,IMCO
      IF(OP(1).lt.-0.5)THEN
        ITEMS(3)=                  'c MRT >> default        '
      ELSEIF(OP(1).gt.-0.5)THEN
        ITEMS(3)=                  'c                       '
      ENDIF
      ITEMS(4)   =                 '  _____________________ '
      WRITE(ITEMS(5),'(A,F5.2)')   'd CLO value     : ',CLO
      WRITE(ITEMS(6),'(A,F5.1)')   'e Activity level: ',ACTL
      WRITE(ITEMS(7),'(A,F5.1)')   'f Air velocity  : ',WVEL
      if(iocut.lt.1)then
        ITEMS(8) =                 'g Occupancy: time based '
      else
        WRITE(ITEMS(8),'(A,I1)')   'g Occup = casual type: ',iocut
      endif
      ITEMS(9)  =                  '  _____________________ '
      ITEMS(10) =                  '1 Assess average comfort'
      ITEMS(11) =                  '2 Assess local comfort  '
      ITEMS(13)  =                 '? Help                  '
      ITEMS(14)  =                 '- Exit                  '
    3 NITMS=14
      if(ixopen.eq.1)then
        ITEMS(12)='> Output >> file  '
        itru = ixunit
      elseif(ixopen.eq.0)then
        ITEMS(12)='> Output >> screen'
        itru = icout
      endif

C Help text for menu.
      helptopic='res_comfort_menu'
      call gethelptext(helpinsub,helptopic,nbhelp)

      INO=-2
      if(mmod.eq.8)then
        CALL EMENU('Comfort assessment',ITEMS,NITMS,INO)
      else
        CALL EMENU('Comfort',ITEMS,NITMS,INO)
      endif

      if(ino.eq.NITMS)then
        RETURN
      elseif(ino.eq.NITMS-1)then

C Help.
        helptopic='res_comfort_menu'
        call gethelptext(helpinsub,helptopic,nbhelp)
        CALL PHELPD('comfort',nbhelp,'-',0,0,IER)
      elseif(ino.eq.NITMS-2)then

C Alter redirect.
        call ctlexp(xfile,ixopen,ixunit,ixpunit,'T','Comfort',IER)
      elseif(ino.eq.1)then

C Change zone.
        call askzon(IZONE)
      elseif(ino.eq.2)then

C Find the day to analyse.
        helptopic='res_comfort_day'
        call gethelptext(helpinsub,helptopic,nbhelp)
        write(TSTR,'(3X,I3,I4)',IOSTAT=IOS,ERR=1)IDCO,IMCO
    8   CALL EASKS(TSTR,' ',' Day and month numbers ? ',
     &      24,'1 1 ','day & month no',IER,1)
        K=0
        CALL EGETWI(TSTR,K,IDCO,1,31,'W','day',IER)
        CALL EGETWI(TSTR,K,IMCO,1,12,'W','month',IER)
        if(ier.ne.0)goto 8

        IF(IDCO.LT.1.OR.IDCO.GT.ID(IMCO))GOTO 8
        CALL EDAY(IDCO,IMCO,IDAY)
        IF(IDAY.LT.ISDS)THEN
          call edisp(iuout,' Before simulation try again... ')
          goto 8
        ENDIF
        IF(IDAY.GT.ISDF)then
          call edisp(iuout,' After simulation try again... ')
          goto 8
        ENDIF
      elseif(ino.eq.3)then
          OP(1)=-1.
          goto 3
      elseif(ino.eq.5)then

C Clothing level.
        helptopic='res_clo_review'
        call gethelptext(helpinsub,helptopic,nbhelp)
        call easkr(CLO,' ',' Clothing level ?',
     &    0.0,'W',3.0,'W',1.0,'clothing level',IER,nbhelp)
      elseif(ino.eq.6)then

C Activity level, may be entered in MET units or in W/m^2.
        helptopic='res_MET_review'
        call gethelptext(helpinsub,helptopic,nbhelp)
        CALL EASKAB(' ','Activity level units:',
     &    'MET units','W/m^2 (body surface area)',IAU,nbhelp)
        if(IAU.eq.1)then
          call easkr(ACTL,' ',' Activity level ?',
     &      0.859,'W',6.013,'W',50.0,'activity level MET',IER,nbhelp)
          ACTL = ACTL * 58.2
        elseif(IAU.eq.2)then
          call easkr(ACTL,' ',' Activity level ?',
     &      50.0,'F',350.0,'F',85.0,'activity level W/m2',IER,nbhelp)
        endif
      elseif(ino.eq.7)then

C Air speed.
        helptopic='res_comfort_air_velo'
        call gethelptext(helpinsub,helptopic,nbhelp)
        call easkr(WVEL,' ',' Air velocity ?',
     &    0.0,'W',5.0,'W',0.15,'air velocity',IER,nbhelp)
      elseif(ino.eq.8)then

C Casual gain type.  Ask user which constitutes occupancy.

C << revise for additional casual gain types >>
        call edisp(iuout,' ')
        write(ll1,'(1x,a6,1x)') lodlabel(IZONE,1)
        write(ll2,'(1x,a6,1x)') lodlabel(IZONE,2)
        write(ll3,'(1x,a6,1x)') lodlabel(IZONE,3)
        helptopic='res_comfort_occupancy'
        call gethelptext(helpinsub,helptopic,nbhelp)
        IOT=1
        CALL EASKATOG('Which casual gain represents occupancy:',' ',
     &      'Always occupied',ll1,ll2,ll3,'time',' ',' ',IOT,nbhelp)
        if (IOT.gt.1.and.IOT.lt.5) then
          iocut=IOT-1
        elseif(IOT.eq.1)then
          iocut=0
        elseif(IOT.eq.5)then
          iocut=-1
        endif
        call usrmsg(' ',' ','-')
        call scnocup(iier)
        IFILT=iocut
      elseif(ino.eq.10)then
        goto 78
      elseif(ino.eq.11)then

C Start espvwf session.
        dok=.false.
        helptopic='res_comfort_local_vwf'
        call gethelptext(helpinsub,helptopic,nbhelp)
        CALL ASKOK(' ','Proceed with local vwf analysis?',OK,dok,nbhelp)
        IF(OK)then

C Get logical name of terminal type, expand problem name
C to include the path and create a string to drive ish.
          write(zn,'(A)') zname(IZONE)
          call tstamp('>','RES: start mft')
          doit = ' '
          call tchild(ICPMOD)
          call termode(ICPMOD,tmode)
          if(unixok)then
            call addpath(LCFGF,longtfile,concat)
          else

C If running on a non-unix machine see if there are spaces in the name
C and change any / to \.
            call addpath(LCFGF,longtfile,concat)
            call cmdfiledos(longtfile,longtfiledos,ier)
            longtfile=' '
            longtfile=longtfiledos
          endif

          write(doit,'(7a)') 'espvwf -mode ',tmode,
     &       ' -s 0 0 0 -file ',longtfile(1:lnblnk(longtfile)),
     &       ' -zone ',ZN(1:lnblnk(ZN)),' &'
          call usrmsg(doit,'starting vwf analysis.','-')
          call runit(doit,tmode)
        endif
      else
        INO=-1
        goto 3
      endif
      goto 4

C If output to file alter the edisp unit number.
 78   itru = icout
      if(ixopen.eq.1)then
        itru = ixunit
      endif

      if(act.eq.'V')then
        call edisp(itru,'  ')
        call edisp(itru,
     &   ' Note: PMV is Fanger predicted mean vote. PMV* is predicted')
        call edisp(itru,
     &   '       mean vote based on ET rather then TO.')
        call edisp(itru,'  ')
        write(outs,'(A,A,A,I3,A,I3)',IOSTAT=IOS,ERR=1)
     &    ' Comfort assessment for ',
     &    zname(IZONE)(1:lnzname(IZONE)),' on  Day',IDCO,
     &    ' of month',IMCO
        call edisp(itru,outs)
        call edisp(itru,' ')
        write(outs,172,IOSTAT=IOS,ERR=1)ACTL,CLO,WVEL
  172   FORMAT(' Activity level ',F6.2,', Clothing level',F6.2,
     &         ', Air speed',F6.2)
        call edisp(itru,outs)

        call edisp(itru,' Default mean radiant temperature')
        call edisp(itru,' ')
        write(outs,103)
  103   FORMAT('Time   t-air   t-mrt  rel.h   SET   PMV*   PMV   PPD',
     &         '   Comfort assessment')
        call edisp(itru,outs)
        write(outs,1103)
 1103   FORMAT('(hrs) (deg.C) (deg.C)  (%)  (deg.C) (-)    (-)   (%)',
     &         '      based on PMV')
        call edisp(itru,outs)
        call edisp(itru,' ')
      endif

C For each timestep in the day get the QW values used by MOMNRD.
      N=24*NTS
      DO 40 JTS=1,N
        ISET=ISIM
        call GZTMS(IDAY,IZONE,ISET,JTS,TS,TSO,TAIR,TMCON,A1,A2,A5,RH) 
        QV(1)=A1
        QV(2)=A2
        QV(3)=0.0
        QV(4)=0.0
        QV(5)=A5

C Get the current MRT in the zone.
        CALL MOMNRD(IZONE,TAIR,TS,QV,NSUR,TMRT)

C Current time.
        call DATIME(JTS,TIME)
        RHX=RH/100.

C Comment WK out since it is in input file for 2 node model.
C       WK=0.

C This is the main calculation routine. It is passed the
C current air temperature (TAIR), the MRT (TMRT), and the
C users values for wind velocity (WVEL) and clothing value (CLO).
C It returns an index of comfort (ICMFRT) and three variants
C of comfort.
C The LET* comfort routines implemented by Cor Pernot, FAGO-TNO.
      CALL LETDRIVR(TAIR,TMRT,WVEL,RHX,ACTL,CLO,SET,ICMFRT,PME,PMV,XPPD)

C If occupancy is assumed to be equivalent to a non-zero value
C of one of the casual gain types test here.
        ih=int(TIME+1.)
        call getocup(izone,iday,ih,ioc,ier)
        if(ioc.eq.0)ICMFRT=10

C Debug.
C        write(6,*)'icmfrt,izone,iday,ih,ioc',icmfrt,izone,iday,ih,ioc

C Verbal category only printed if activity level is sedentary.
C << is there a way to equate category if non-sedintary? >>
        IF(ACTL.GE.100.)ICMFRT=11
        if(act.eq.'V')then
          write(outs,106,IOSTAT=IOS,ERR=1)TIME,TAIR,TMRT,RH,SET,PME,
     &      PMV,XPPD,DESC(ICMFRT)
  106     FORMAT(F4.1,2X,F5.1,2X,F6.1,1X,F6.0,2X,F5.1,2X,F5.2,1X,
     &           F6.2,1X,F5.0,2X,A25)
          call edisp(itru,outs)
        elseif(act.eq.'M')then
          GVAL(JTS)=PMV
        elseif(act.eq.'E')then
          GVAL(JTS)=PME
        elseif(act.eq.'D')then
          GVAL(JTS)=XPPD
        endif
   40 CONTINUE

C Finished with that, seewhat user wants to do next.
      if(act.eq.'V')then
        helptopic='res_comfort_next_day'
        call gethelptext(helpinsub,helptopic,nbhelp)
        CALL EASKAB(' ','Comfort:',
     &    'Continue with next day','Return to menu',IWW,nbhelp)
        if(IWW.eq.1)then

C Check to see of next day is acceptable, recover casual gains
C (to test for occupancy in the next pass).
          IDCO=IDCO+1
          IF(IDCO.LT.1.OR.IDCO.GT.ID(IMCO))then
            IDCO=1
            IMCO=IMCO+1
          endif
          CALL EDAY(IDCO,IMCO,IDAY)
          IF(IDAY.GT.ISDF)then
            call edisp(iuout,' After simulation ... ')
            IDAY=ISDS
            CALL EDAYR(IDAY,IDCO,IMCO)
            goto 4
          ENDIF
          goto 78
        else
          goto 4
        endif
      elseif(act.eq.'M')then
        return
      elseif(act.eq.'E')then
        return
      elseif(act.eq.'D')then
        return
      endif

      RETURN
   1  call isunix(unixok)
      if(IOS.eq.2)then
        if(unixok)write(6,*)'MOCMFT: no permission to write string.'
      else
        if(unixok)write(6,*)'MOCMFT: internal string handling error.'
      endif
      END

C ******** scnocup
C Scans unaveraged occupancy in each of the zones of the probem. Weekdays are
C assumed to be in ioucpz(izone,[1-24]), saturdays ioucpz(izone,[25-48]),
C sundays ioucpz(izone,[49-72]), This hash table will need altering 
C if sub-hour casual gains definitions are implemented. 
      subroutine scnocup(ier)
#include "building.h"
#include "espriou.h"

      COMMON/SET1/IYEAR,IBDOY,IEDOY,IFDAY,IFTIME
      COMMON/SIMPIK/ISIM,ISTADD,ID1,IM1,ID2,IM2,ISDS,ISDF,NTS,ISAVE
      
      integer ncomp,ncon
      common/c1/ncomp,ncon
      COMMON/OUTIN/IUOUT,IUIN
      COMMON/FILEP/IFIL
      COMMON/GET1/VAL1(MZS,MTS),VAL2(MZS,MTS),VAL3(MZRL,MTS)
      common/cmftpar/WVEL,ACTL,CLO,iocut,iocuset,ioucpz(MCOM,72)
      common/recver/izver,ipver,iever

C Version of operations file. ip3ver=0 standard, =1 sorted with header
C =2 with more than 3 day types
      common/p3ver/ip3ver(MCOM)

      logical close15,close16,close17
      character hold*24,outs*124

C For help messages
      character helpinsub*24 ! subroutine name
      character helptopic*24 ! string (unique) for topic
      integer nbhelp     ! number of help lines found (will be non-zero
                         ! because default message will be created
      helpinsub='scnocup'  ! set for subroutine

C Find weekday saturday and sunday dates so can extract casual gains
C for each day type. This should be done for the first 7 days of the
C current simulation.
      idays=ISDF-ISDS
      if(idays.gt.7)then
        do 41, id = ISDS,ISDS+7
          CALL EDAYR(ID,IDAYN,IMTHN)
          CALL EWEEKD(IDAYN,IMTHN,IYEAR,IDWK)
          if(IDWK.EQ.7)then
            isun=id
          elseif(IDWK.EQ.6)then
            isat=id
          else
            imon=id
          endif
 41     continue
      else
        do 142, id = ISDS,ISDF
          CALL EDAYR(ID,IDAYN,IMTHN)
          CALL EWEEKD(IDAYN,IMTHN,IYEAR,IDWK)
          if(IDWK.EQ.7)then
            isun=id
          elseif(IDWK.EQ.6)then
            isat=id
          else
            imon=id
          endif
 142    continue
      endif
      if(iocut.eq.-1)then
        helptopic='res_weekday_occup_period'
        call gethelptext(helpinsub,helptopic,nbhelp)
        write(HOLD,'(a)') '  0  24  '
        CALL EASKS(HOLD,' ',' Weekday occupancy period: ',
     &     24,' 0  24 ','wkd occup period',IER,nbhelp)
        K=0
        CALL EGETWI(HOLD,K,iwkdst,0,24,'W','iwkdst',IER)
        CALL EGETWI(HOLD,K,iwkdfn,iwkdst,24,'W','iwkdst',IER)

        write(HOLD,'(a)') '  0  24  '
        CALL EASKS(HOLD,' ',' First weekend day occupancy period: ',
     &     24,' 0  24 ','sat occup period',IER,nbhelp)
        K=0
        CALL EGETWI(HOLD,K,isatst,0,24,'W','isatst',IER)
        CALL EGETWI(HOLD,K,isatfn,isatst,24,'W','isatst',IER)

        write(HOLD,'(a)') '  0  24  '
        CALL EASKS(HOLD,' ',' Second weekend day occupancy period: ',
     &     24,' 0  24 ','sat occup period',IER,nbhelp)
        K=0
        CALL EGETWI(HOLD,K,isunst,0,24,'W','isunst',IER)
        CALL EGETWI(HOLD,K,isunfn,isunst,24,'W','isunst',IER)
      elseif(iocut.eq.0)then
        iwkdst=0
        iwkdfn=24
        isatst=0
        isatfn=24
        isunst=0
        isunfn=24
      endif
      do 42, izone = 1,ncomp

C If izver >= 4 the read the data directly.
        if(izver.eq.4)then
          continue
        else
          CALL EROPER(0,iuout,IFIL+1,izone,IER)  ! for older library
          if(ip3ver(izone).eq.0)then
            write(outs,'(a,i2,a)') 
     &      'Operation file for zone ',izone,' periods being sorted!'
            call edisp(iuout,outs)
            call PROCESSOLDCAS(izone,0,iuout,IER)
          endif
        endif

C Get periods for weekday, and two weekend days. If no casual gains
C (occupancy type) at each hour then set ioucpz to zero.  This hash
C table will need altering if sub-hour casual gains definitions are
C implemented.
        if(imon.ne.0)CALL ZCASCF(IZONE,imon,0)
        do 43, it=1,24
          if(imon.ne.0)then
            itt=it*NTS
            call eclose(VAL3(15,itt),0.0,0.1,close15)
            call eclose(VAL3(16,itt),0.0,0.1,close16)
            call eclose(VAL3(17,itt),0.0,0.1,close17)
            ioucpz(izone,it)=1
            if(iocut.lt.1)then
              if(it.lt.iwkdst.or.it.gt.iwkdfn)ioucpz(izone,it)=0
            elseif(iocut.eq.1)then
              if(close15)ioucpz(izone,it)=0
            elseif(iocut.eq.2)then
              if(close16)ioucpz(izone,it)=0
            elseif(iocut.eq.3)then
              if(close17)ioucpz(izone,it)=0
            endif
          else
            ioucpz(izone,it)=0
          endif
  43    continue
        if(isat.ne.0)CALL ZCASCF(IZONE,isat,0)
        do 44, it=1,24
          if(isat.ne.0)then
            itt=it*NTS
            call eclose(VAL3(15,itt),0.0,0.1,close15)
            call eclose(VAL3(16,itt),0.0,0.1,close16)
            call eclose(VAL3(17,itt),0.0,0.1,close17)
            ioucpz(izone,it+24)=1
            if(iocut.lt.1)then
              if(it.lt.isatst.or.it.gt.isatfn)ioucpz(izone,it+24)=0
            elseif(iocut.eq.1)then
              if(close15)ioucpz(izone,it+24)=0
            elseif(iocut.eq.2)then
              if(close16)ioucpz(izone,it+24)=0
            elseif(iocut.eq.3)then
              if(close17)ioucpz(izone,it+24)=0
            endif
          else
            ioucpz(izone,it+24)=0
          endif
  44    continue
        if(isun.ne.0)CALL ZCASCF(IZONE,isun,0)
        do 45, it=1,24
          if(isun.ne.0)then
            itt=it*NTS
            call eclose(VAL3(15,itt),0.0,0.1,close15)
            call eclose(VAL3(16,itt),0.0,0.1,close16)
            call eclose(VAL3(17,itt),0.0,0.1,close17)
            ioucpz(izone,it+48)=1
            if(iocut.lt.1)then
              if(it.lt.isunst.or.it.gt.isunfn)ioucpz(izone,it+48)=0
            elseif(iocut.eq.1)then
              if(close15)ioucpz(izone,it+48)=0
            elseif(iocut.eq.2)then
              if(close16)ioucpz(izone,it+48)=0
            elseif(iocut.eq.3)then
              if(close17)ioucpz(izone,it+48)=0
            endif
          else
            ioucpz(izone,it+48)=0
          endif
  45    continue

C Debug.
C        write(6,*) 'zone:',izone,'  iocut:',iocut,'  iocuset:',iocuset
C        write(6,*) 'ioucpz (wkd)',(ioucpz(izone,j),j=1,24)
C        write(6,*) 'ioucpz (sat)',(ioucpz(izone,j),j=25,48)
C        write(6,*) 'ioucpz (sun)',(ioucpz(izone,j),j=49,72)

 42   continue
      return
      end

C ******** getocup
C Looks up occupancy patterns and returns in ioc for a given zone/day/hour.

C << todo - use of explicit read of casual gains >>

      subroutine getocup(izone,iday,ih,ioc,ier)
#include "building.h"
#include "geometry.h"
#include "net_flow.h"
#include "tdf2.h"

      common/recver/izver,ipver,iever
      COMMON/SET1/IYEAR,IBDOY,IEDOY,IFDAY,IFTIME
      COMMON/SIMPIK/ISIM,ISTADD,ID1,IM1,ID2,IM2,ISDS,ISDF,NTS,ISAVE
C      COMMON/OUTIN/IUOUT,IUIN
      
      integer ncomp,ncon
      common/c1/ncomp,ncon
      common/cmftpar/WVEL,ACTL,CLO,iocut,iocuset,ioucpz(MCOM,72)

      COMMON/GET1/VAL1(MZS,MTS),VAL2(MZS,MTS),VAL3(MZRL,MTS)

C.........START: USED FOR ACTIVITY TDF DATA.....................
      REAL X2a_QTOTOCCUP
      REAL X2c_QTOTEQUIP
C.........END: USED FOR ACTIVITY TDF DATA.....................

      DIMENSION VAL(MBITS+2)
      character outs*124
      logical found,close15,close16,close17
      real QCASR,QCASC,QCASL  ! total radiant/convective/latent
      real FRAC ! controlled fraction.
      real perocupc,perocupr,perocupl ! average occupant to write out
      real perlightc,perlightr,perlightl ! average lighting to write out
      real perequipc,perequipr,perequipl ! average equipment to write out
      real otherc,otherr,otherl ! average other (future expansion) to write out
      integer theonectld  ! if non-zero the casual gain type that is controlled.

C Range testing.
      if(izone.lt.1.or.izone.gt.ncomp.or.ih.lt.1)then
        write (outs,'(3(a,i3),a,a)') 
     &      'zone(',izone,'), day(',iday,'), hour(',ih,').  Assuming',
     &      ' zone is occupied.'
        call usrmsg(
     &      'getocup: out of range zone/day/hour index in:',outs,'W')
        ioc=1
        ier=1
        return
      endif

      IEND=24*NTS
      ISET=ISIM

C If assumption of occupied all hours then return 1.
      if(iocut.eq.0)then
        ioc=1
        return
      endif

C Find day of week.
      CALL EDAYR(iday,IDAYN,IMTHN)
      CALL EWEEKD(IDAYN,IMTHN,IYEAR,IDWK)
      if(IDWK.EQ.7)then
        ioc=ioucpz(izone,ih+48)
      elseif(IDWK.EQ.6)then
        ioc=ioucpz(izone,ih+24)
      else
        ioc=ioucpz(izone,ih)
      endif

C If izver >= 4 the read the data directly for the exceptional
C cases of temporal gains.
      if(izver.eq.4)then
        found = .false.
        if(ICASUAL(izone).ne.0)found = .true.  ! if single casual item
        if(ICASUAL3(izone).ne.0)found = .true. ! if 3 separate casual item
        if(IACTIV(izone).ne.0)found = .true.   ! if UK activity item
        if(found)then
          DO 141, I=1,IEND
            call getallcas(IDAY,IZONE,ISET,I,QCASR,QCASC,QCASL,FRAC,
     &      perocupc,perlightc,perequipc,otherc,perocupr,perlightr,
     &      perequipr,otherr,perocupl,perlightl,perequipl,otherl,
     &      theonectld)
            call eclose(perocupc,0.0,0.1,close15)
            call eclose(perocupr,0.0,0.1,close16)
            call eclose(perocupl,0.0,0.1,close17)
            ioc=1
            if(iocut.eq.1)then
              if(close15) ioc=0
            elseif(iocut.eq.2)then
              if(close16) ioc=0
            elseif(iocut.eq.3)then
              if(close17) ioc=0
            endif

C Debug.
C            if(ioc.gt.0) write(6,*) '* Found temporal occupancy @',itt

  141     continue
          return
        endif
      endif

C Older format results file. If there are separate temporal casual
C gains then explicitly gather information for this zone and day.
C If close to zero then zone is not occupied.
      found = .false.
      if(ICASUAL3(izone).ne.0)found = .true.
      if(found)then
        itt=ih*NTS
        call DATIME(ITT,TIME)
        TIMEF=real(ITT)/real(NTS)
        TIMEP=TIMEF - (1./real(NTS))
        itdi=ICASUAL3(izone)
        IFOC=itdi
        CALL RCTDFR(0,IDAY,ITT,TIMEF,VAL,ISD,IFOC,IER)
        X2a=VAL(ISD)
        X2b=VAL(ISD+3)
        X2c=VAL(ISD+6)
        call eclose(X2a,0.0,0.1,close15)
        call eclose(X2b,0.0,0.1,close16)
        call eclose(X2c,0.0,0.1,close17)
        ioc=1
        if(iocut.eq.1)then
          if(close15) ioc=0
        elseif(iocut.eq.2)then
          if(close16) ioc=0
        elseif(iocut.eq.3)then
          if(close17) ioc=0
        endif

C Debug.
C        if(ioc.gt.0) write(6,*) '* Found temporal occupancy @',itt

        return
      endif

C Following now the same pattern as the CASUAL3T for the "ACTIVITY" tdf
C entity.
      found = .false.
      if(IACTIV(izone).ne.0)found = .true.
      if(found)then
        itt=ih*NTS
        call DATIME(ITT,TIME)
        TIMEF=real(ITT)/real(NTS)
        TIMEP=TIMEF - (1./real(NTS))
        itdi=IACTIV(izone)
        IFOC=itdi
        CALL RCTDFR(0,IDAY,ITT,TIMEF,VAL,ISD,IFOC,IER)
        X2a_QTOTOCCUP=TMAR(1,1)*ITMAR(1,2)*VAL(ISD)*ZBASEA(izone)
        X2a=(X2a_QTOTOCCUP-((X2a_QTOTOCCUP*ITMAR(1,3))/100))/2

C<<< let the lighting gains to be 0 for now until there is a
C a decision on the calculation of the lighting gains>>
        X2b=0
        X2c_QTOTEQUIP=TMAR(1,4)*VAL(ISD+2)*ZBASEA(izone)
        X2c=(X2c_QTOTEQUIP-((X2c_QTOTEQUIP*ITMAR(1,5))/100))/2
 
        call eclose(X2a,0.0,0.1,close15)
        call eclose(X2b,0.0,0.1,close16)
        call eclose(X2c,0.0,0.1,close17)
        ioc=1
        if(iocut.eq.1)then
          if(close15) ioc=0
        elseif(iocut.eq.2)then
          if(close16) ioc=0
        elseif(iocut.eq.3)then
          if(close17) ioc=0
        endif

C Debug.
C        if(ioc.gt.0) write(6,*) '* Found temporal occupancy @',itt

      endif

      return
      end


C ************************ LOCOMF ************************
C LOCOMF: calculate local discomfort.

      SUBROUTINE LOCOMF(IDAY,IZONE,ICP,IMP,ISET)
#include "building.h"
      COMMON/OUTIN/IUOUT,IUIN
      COMMON/SIMPIK/ISIM,ISTADD,ID1,IM1,ID2,IM2,ISDS,ISDF,NTS,ISAVE
      COMMON/GET2/XDUM(MTS),XDUM1(MTS),GVAL(MTS)

      DIMENSION TPVAL(MTS)

C Firstly recover the temperature values. 
      if (ICP.eq.4) then

C Need dT between CFD cells. set to zero and warn user.
        call edisp(iuout,
     &    ' Missing information on local air T, setting dT to zero.')
C        CALL GZQM(IDAY,IZONE,ISET)
      elseif (ICP.eq.5) then

C Only need floor surface T.
        CALL GTMS(IDAY,IZONE,IMP,ISET)
      endif

C Store in a temporary array 
      N=24*NTS
      DO 20 I=1,N
        TPVAL(I)=GVAL(I)
  20  continue

C Call the routine to get the latent loading.
      CALL GZLL(IDAY,IZONE,ISET)       

C Output the sum of the two variables. 
      DO 30 I=1,N
        GVAL(I)=GVAL(I)+TPVAL(I)
  30  continue

      RETURN
      END

C ************************ DISCOMF ************************
C DISCOMF: Local thermal discomfort and Indoor air quality assessment 
C according to Standard prENV 1752. Also general thermal comfort is 
C treated according to Standards prENV 1752 and ISO EN 7730. 
C PPD is the output of the subroutine [in %]. The input is the 
C XX-value which is case dependent. 
C If iflag is returned as -1 then outside bounds of algorithm.
      SUBROUTINE DISCOMF(iflag,XX,PPD)

C At the time of testing lets know what input parameter we have: 

C Debug.
C    write(6,*)'flag, XX=',iflag,XX 

C Assume nobody is dissatisfied.
      PPD=0.0 

C Local thermal discomfort due to vertical air temperature difference 
C between head (@1.1m) and feet(@0.1m). 
C Input (XX): Temperature difference in [K] or [C]. 
      if(iflag.eq.1) then 
        if (XX.lt.1.0.or.XX.gt.8.0) then
          iflag=-1
        else
          PPD=0.7038+0.2974*XX**2.7810-0.084*exp(XX) 
        endif

C Local thermal discomfort due to warm or cool floors.   
C Input (XX): Floor temperature in [C]. 
      elseif(iflag.eq.2) then 
        if (XX.lt.5.0.or.XX.gt.40.0) then
          iflag=-1
        else
          PPD=59.5022 - (74.6871*XX) + (16.4158*LOG(XX)*(XX+9.3362)) 
        endif

C Local thermal discomfort due to radiant temperature asymmetry - warm ceiling.   
C Input (XX): Radiant temperature assymetry in [K] or [C].
      elseif(iflag.eq.3) then 
        XX=abs(XX)
        if (XX.gt.25.0) then
          iflag=-1
        elseif (XX.lt.1.0) then
          PPD=0.5
        else
          PPD=-0.9525 + 0.1865*exp(2.0755*XX**0.35)  
        endif

C Local thermal discomfort due to radiant temperature asymmetry - warm wall.   
C Input (XX): Radiant temperature assymetry in [K] or [C].
      elseif(iflag.eq.4) then 
        XX=abs(XX)
        if (XX.gt.30.0) then
          iflag=-1
        elseif (XX.lt.7.5) then
          PPD=1.0
        else
          PPD=-0.1112 + 0.0539*exp(1.4686*XX**0.36)
        endif

C Local thermal discomfort due to radiant temperature asymmetry - cool ceiling.   
C Input (XX): Radiant temperature assymetry in [K] or [C].
      elseif(iflag.eq.5) then 
        XX=abs(XX)
        if (XX.gt.16.0) then
          iflag=-1
        elseif (XX.lt.7.5) then
          PPD=1.0
        else
          PPD=-0.1056+0.0163*exp(1.5847*XX**0.49)
        endif

C Local thermal discomfort due to radiant temperature asymmetry - cool wall.   
C Input (XX): Radiant temperature assymetry in [K] or [C].
      elseif(iflag.eq.6) then 
        XX=abs(XX)
        if (XX.gt.16.0) then
          iflag=-1
        elseif (XX.lt.6.0) then
          PPD=0.4
        else
          PPD=-1.2568+0.0189*exp(1.9469*XX**0.47)
        endif

C Dissatisfied due to ventilation rate. 
C Input (XX): Ventilation rate given in [L/(s*standard person)] 
      elseif(iflag.eq.7) then 
        if(XX.ge.0.32) then
          PPD=395.0*exp(-1.83*XX**0.25) 
        else
          PPD=100.0
        endif

C Dissatisfied due to CO2 above outdoors. 
C Input: CO2 concentration as calculated by conc.F in [kg/kg].
C The CO2 value needed to interpolate the graph in the standard 
C is in [ppm_volume]. 
C The question: Actually why a thermal comfort standard should 
C use standard SI units ? 
      elseif(iflag.eq.8) then 

C First convert kg/kg to mg/m3 using density at 20 [C]: 
        XX=XX*1.12*1000000.0 

C Now convert mg/m3 to ppm_volume: 
        XX=XX*24.45/44.0  
        PPD=395.0*exp(-15.15*XX**(-0.25)) 

C Dissatisfied calculated from PMV (Predicted Mean Vote). Needed for general 
C Thermal comfort assessment. 
C Input: The predicted mean vote ranging [-3 -- +3]. 
      elseif(iflag.eq.9) then 
        PPD=100.0-95.0*exp(XX*XX*((-0.03353*XX*XX)-0.2179)) 
      endif

C Check for a reasonable result.
      if(PPD.gt.100.0) PPD=100.0
      if(PPD.lt.0.0) PPD=0.0 

      return
      end 

C ************************ DRAUGHT ************************
C DRAUGHT: Local thermal discomfort due to draught. 
C Useful only if k-epssilon turbulence model active from which 
C the turbulence intensity 
C is calculated 
      SUBROUTINE DRAUGHT(Tair,Cvel,Tenergy,PPD) 

      Tu=sqrt((2.0/3.0)*Tenergy)/(Cvel+1.0e-30)*100.0 
      PPD=(34.0-Tair)*(Cvel-0.05)**0.62*(0.37*Cvel*Tu+3.14)
 
C Check for a reasonable result.
      if(PPD.gt.100.0) PPD=100.0
      if(PPD.lt.0.0) PPD=0.0 
      if(Tair.lt.19.0.or.Tair.gt.27.0) PPD=100.0 

      return
      end

C ************************ OFFSETTEMP ************************
C OFFSETTEMP: Calculates the air speed required to offset 
C increased temperature according to Standard ANSI/ASHRAE 55-1992. 
C Applied only for summer conditions only. Here Cadd is the output 
C which is the additional air speed in [m/s] required to 
C offset the increased temperature. Max allowed to be 0.8 [m/s]. 
C Input: Temperature rise in [K] or [C]. This is the temperature 
C rise, (max [3 K] allowed !) above the allowed effective 
C temperature of 26 [C]. 
      SUBROUTINE OFFSETTEMP(Trise,Cadd)

      Cadd=-0.3252+0.5337*exp(0.2006*Trise**1.4) 

      return
      end

C ************************ ASHRAEZONE ************************
C ASHRAEZONE: Comfort range checked by ET* index according to 
C ANSI/ASHRAE 55-1992 Standard. Returns the effective 
C temperature ET* (variable ETst) and a logical 'inside',
C which governs whether we are or not in the comfort zone.  
C Needs as input to specify wheter we are in summer or in 
C winter regime (logical 'summer'), 
C Tair and the relative humidity, RH in [%].  
      SUBROUTINE ASHRAEZONE(Tair,RH,summer,ETst,inside)

      LOGICAL summer,inside 

C First define the saturation pressure Psat for the given temperature Tair. 
      TSAT=Tair 
      PSAT=611.0*exp(-1.91275E-04+7.258E-02*TSAT-2.939E-04 
     &       *TSAT**2+9.841E-07*TSAT**3-1.92E-09*TSAT**4) 

C Calculate the ambient water vapour pressure pa in [Pa].
      pa=PSAT*RH/100.0 

C Calculate the ET* temperature.  
      ETst=0.492*Tair+0.0019*pa+6.47 

C Check the comfort range for summer and winther.
      inside=.false. 
      if(summer) then 
        if(ETst.gt.23.0.and.ETst.lt.26.0) inside=.true. 
      else
        if(ETst.gt.20.0.and.ETst.lt.23.5) inside=.true. 
      endif

      return
      end 
