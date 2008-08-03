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

C This file contains the following routines:
C      MOCMFT
C      LETDRIVER
C      LETINIT
C      LETLOOP
C      PPD
C      VDD
C      LETINDEX
C      FANGER
C scnocup: Scans occupancy in each of the zones of the probem.
C getocup: 
C DISCOMF: Returns local dicomfort information (PPD).
C DRAUGHT: PPD due to draughts.
C OFFSETTEMP: air velocity required to maintain comfort.
C ASHRAEZONE: inside/ outside ASHRAE comfort zone.

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
      
      integer lnblnk  ! function definition

      COMMON/SPAD/MMOD,LIMIT,LIMTTY
      COMMON/OUTIN/IUOUT,IUIN
      COMMON/OUTPCH/ICOUT
      common/pophelp/h(60)

      COMMON/SIMPIK/ISIM,ISTADD,ID1,IM1,ID2,IM2,ISDS,ISDF,NTS,ISAVE
      COMMON/C20/NZSUR(MCOM),NZTV(MCOM)
      common/C21/IFCFG,cfgroot,LCFGF
      common/loadlabel/lodlabel(mcom,7)
      COMMON/precz/zname(MCOM),zdesc(MCOM)
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
      character outs*124,TSTR*24,H*72,zname*12,zdesc*64,ZN*12
      character xfile*144,ITEMS*25,tg*1,delim*1,act*1
      CHARACTER*6 lodlabel
      character*8 ll1,ll2,ll3
      logical ok,dok,concat,unixok

      DATA DESC/'very cold, danger        ','cold, shivering          ',
     &          'cool, unpleasant         ','slightly cool, acceptable',
     &          'comfortable, pleasant    ','slightly warm, acceptable',
     &          'warm, unpleasant         ','hot, very uncomfortable  ',
     &          'very hot, danger         ','unoccupied               ',
     &          'non-sedentary            ','out of range             '/

      DATA ID/31,28,31,30,31,30,31,31,30,31,30,31/

C Check if Unix or Windows.
      call isunix(unixok)

C Initialise comfort variables.
      if(act.eq.'I'.or.act.eq.'V')then
        OP(1)=-1.
      endif

C If initial settings and initial request ask parameters.
      if(act.eq.'I')then
        if(iocuset.eq.1)then
          dok=.true.
          h(1)='If you want to alter the current comfort parameters'
          h(2)='(clo value, metabolic rate, local air velocity) say '
          h(3)='no. '
          call askok(' ','Use existing comfort parameters?',ok,dok,3)
          if(ok)return
        endif

C Confirm parameters and then exit.
        call edisp(iuout,' ')
        call edisp(iuout,'Please confirm general comfort parameters.')
        H(1)='Clothing is specified in `clo` units between 0. & 3. '
        call easkr(CLO,' ',' Clothing level ?',
     &    0.0,'W',3.0,'W',1.0,'clothing level',IER,1)

        H(1)='Activity may be in MET units or W/m^2. '
        H(2)='Allowable range is 50 to 350 Watts.'
        H(3)='                or 0.859 to 6.013 MET.'
        CALL EASKAB(' ','Activity level units:',
     &    'MET units','W/m^2 (body surface area)',IAU,3)
        if(IAU.eq.1)then

C When using MET units, convert back to W/M^2 for calculations.
C Assume default MET is equiv to 85W/m^2.
          ACTL = ACTL/58.2
          ACTLDEF = 85.0/58.2
          call easkr(ACTL,' ',' Activity level (MET)?',
     &      0.859,'W',6.013,'W',ACTLDEF,'activity level MET',IER,3)
          ACTL = ACTL * 58.2
        elseif(IAU.eq.2)then
          call easkr(ACTL,' ',' Activity level (W/m2)?',
     &      50.0,'F',350.0,'F',85.0,'activity level W/m2',IER,3)
        endif

        H(1)='Air velocity in a zone is specified in m/sec. '
        H(2)='Allowable range is 0 to 5 m/sec.'
        call easkr(WVEL,' ',' Air velocity ?',
     &    0.0,'W',5.0,'W',0.15,'air velocity',IER,2)

C Set representative casual gain and then search all zones and build
C up hash list of zone:time:occupancy.
        write(ll1,'(1x,a6,1x)') lodlabel(IZONE,1)
        write(ll2,'(1x,a6,1x)') lodlabel(IZONE,2)
        write(ll3,'(1x,a6,1x)') lodlabel(IZONE,3)
        H(1)='To identify when during the day that comfort should be'
        H(2)='reported you have several options: '
        H(3)=' Always occupied - assess comfort 24/7 i.e. independent'
        H(4)='     of the occupant schedules with the zone operation'
        H(5)='     file.'
        H(6)=' Labels from the zone operations file - the 2nd - 4th'
        H(7)='     options of the in the dialog reflect what has been'
        H(8)='     defined in the zone operations file for casual gain'
        H(9)='     types ( if you select one of these then comfort is'
        H(10)='     reported for non-zero sensible gain periods.'
        H(11)=' time - you specify a start time and an end time for'
        H(12)='     a typical day and comfort is assessed during this'
        H(13)='     period.'
        H(14)=' '
        H(15)='If you have not altered the labels for the casual gains'
        H(16)='within the zone operations file the first Type is for'
        H(17)='occupants the second Type is lights and the third Type'
        H(18)='is Equipment (small power).'
        CALL EASKATOG('Which casual gain represents occupancy:',' ',
     &      'Always occupied',ll1,ll2,ll3,'time',' ',' ',IOT,18)
        if (IOT.gt.1.and.IOT.lt.5) then
          iocut=IOT-1
        elseif(IOT.eq.1)then
          iocut=0
        elseif(IOT.eq.5)then
          iocut=-1
        endif
        iocuset=1
        call scnocup(iier)
        if (iocut.ne.IFILT) call MMAPER
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
      H(1) ='Comfort analysis is based on the work of Fanger '
      H(2) ='and provides a listing of comfort conditions at each'
      H(3) ='timestep of a specific day.  '
      H(4) ='  '
      H(5) ='You may alter any of the variables shown in the menu'
      H(6) ='and recalculate comfort.  '

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
        CALL PHELPD('comfort',6,'-',0,0,IER)
      elseif(ino.eq.NITMS-2)then

C Alter redirect.
        call ctlexp(xfile,ixopen,ixunit,ixpunit,'T','Comfort',IER)
      elseif(ino.eq.1)then

C Change zone.
        call askzon(IZONE)
      elseif(ino.eq.2)then

C Find the day to analyse.
        H(1)='Comfort is analysed for a single day at a time. '
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
        H(1)='Clothing is specified in `clo` units between 0. & 3. '
        call easkr(CLO,' ',' Clothing level ?',
     &    0.0,'W',3.0,'W',1.0,'clothing level',IER,1)
      elseif(ino.eq.6)then

C Activity level, may be entered in MET units or in W/m^2.
        H(1)='Activity may be in MET units or W/m^2. '
        H(2)='Allowable range is 50 to 350 Watts.'
        H(3)='                or 0.859 to 6.013 MET.'
        CALL EASKAB(' ','Activity level units:',
     &    'MET units','W/m^2 (body surface area)',IAU,3)
        if(IAU.eq.1)then
          call easkr(ACTL,' ',' Activity level ?',
     &      0.859,'W',6.013,'W',50.0,'activity level MET',IER,1)
          ACTL = ACTL * 58.2
        elseif(IAU.eq.2)then
          call easkr(ACTL,' ',' Activity level ?',
     &      50.0,'F',350.0,'F',85.0,'activity level W/m2',IER,1)
        endif
      elseif(ino.eq.7)then

C Air speed.
        H(1)='Air velocity in a zone is specified in m/sec. '
        H(2)='Allowable range is 0 to 5 m/sec.'
        call easkr(WVEL,' ',' Air velocity ?',
     &    0.0,'W',5.0,'W',0.15,'air velocity',IER,2)
      elseif(ino.eq.8)then

C Casual gain type.  Ask user which constitutes occupancy.
        call edisp(iuout,' ')
        write(ll1,'(1x,a6,1x)') lodlabel(IZONE,1)
        write(ll2,'(1x,a6,1x)') lodlabel(IZONE,2)
        write(ll3,'(1x,a6,1x)') lodlabel(IZONE,3)
        H(1)='Select a casual gain type to base occupancy on.'
        H(2)='Labels show are from the first zone in the current'
        H(3)='list. Type 1 usually represents casual gains from '
        H(4)='people,although any of the three casual gain types can '
        H(5)='be used. '
        CALL EASKATOG('Which casual gain represents occupancy:',' ',
     &      'Always occupied',ll1,ll2,ll3,'time',' ',' ',IOT,5)
        if (IOT.gt.1.and.IOT.lt.5) then
          iocut=IOT-1
        elseif(IOT.eq.1)then
          iocut=0
        elseif(IOT.eq.5)then
          iocut=-1
        endif
        call usrmsg(' ',' ','-')
        call scnocup(iier)
        if (iocut.ne.IFILT) call MMAPER
        IFILT=iocut
      elseif(ino.eq.10)then
        goto 78
      elseif(ino.eq.11)then

C Start espvwf session.
        dok=.false.
        h(1)='A `vwf analysis` assess comfort at a specific position'
        h(2)='within a zone and makes use of viewfactors from a pre-'
        h(3)='defined body to the surfaces in the zone. '
        CALL ASKOK(' ',' Proceed with local vwf analysis?',OK,dok,3)
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
        call edisp(icout,' Comfort output being directed to file... ')
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
     &    zname(IZONE)(:lnblnk(zname(IZONE))),' on  Day',IDCO,
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

      N=24*NTS
      DO 40 JTS=1,N
        ISET=ISIM
        call GZTMS(IDAY,IZONE,ISET,JTS,TS,TSO,TAIR,TMCON,A1,A2,A5,RH) 
        QV(1)=A1
        QV(2)=A2
        QV(3)=0.0
        QV(4)=0.0
        QV(5)=A5
        CALL MOMNRD(IZONE,TAIR,TS,QV,NSUR,TMRT)

C Current time.
        call DATIME(JTS,TIME)
        RHX=RH/100.

C Comment WK out since it is in input file for 2 node model.
C       WK=0.

C The LET* comfort routines implemented by Cor Pernot, FAGO-TNO.
      CALL LETDRIVR(TAIR,TMRT,WVEL,RHX,ACTL,CLO,SET,ICMFRT,PME,PMV,XPPD)

C If occupancy is assumed to be equivalent to a non-zero value
C of one of the casual gain types test here.
        ih=int(TIME+1.)
        call getocup(izone,iday,ih,ioc,ier)
        if(ioc.eq.0)ICMFRT=10

C Debug...
C        write(6,*)'icmfrt,izone,iday,ih,ioc',icmfrt,izone,iday,ih,ioc

c Verbal category only printed if activity level is sedentary.
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

      if(act.eq.'V')then
        CALL EASKAB(' ','Comfort:',
     &    'Continue with next day','Return to menu',IWW,0)
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
   1  if(IOS.eq.2)then
         write(6,*) 'MOCMFT: no permission to write string.'
      else
         write(6,*) 'MOCMFT: error in internal string handling.'
      endif
      END

c ******************** LETDRIVER ********************

c John B. Pierce Foundation Copyright reference:

c             Gagge, Fobelets & Berglund
c             Standard Predictive Indices ...
c             Paper PO-86-14-1
c             Appendix 2, ASHRAE Transactions
c             Vol. 92, Pt 2, 1986.

c Program evaluates the physiological and sensory responses of
c human subjects to their thermal environment.
c The following members of the Pierce Lab. have made major
c contributions to the present model over the past 20 years:
c Berglund, Casby, Fobelets, Gagge, Gonzales, Hardy, Nishi, Oohori,
c Stolwijk, and others unnamed as visiting fellows.

c PART I:  Main Program 'LETDRIVR'

c Read input data file, compute appropriate variables when two
c alternatives exist for one input, call the initial values
c subroutine, call the minute by minute simulation of physiological
c responses, call the indices subroutine when specified by user,
c write output data file.
c Variables in common block LET1 are input variables.

      subroutine letdrivr(ta,erftr,vel,rhpa,act,clo,set,icmfrt,
     &                    pme,pmv,xppd)

      real lr,icl,im,kclo
c     character*10 word(52)

#include "gagge.h"

c Data entry: variables in common block LET1.
      ttsk   = 33.7
      ttcr   = 36.8
      csw    = 170.0
      cstr   = 0.5
      cdil   = 200.0
      bz     = 0.1
      skbfl  = 90.0
      rgswl  = 500.0
      tski   = 33.7
      tcri   = 36.8
      alphai = 0.1
      skbfi  = 6.3
      eski   = 7.3
      we     = 0.0
      kclo   = 0.15
      evef   = 1.0

C Patch from Cor 11/93.
      icl    = 0.45
      ata    = 1.0
      chcc   = 0.0
      itime  = 60
      isteps = 60
      ida    = 2
      idb    = 1
      idc    = 1
      idd    = 1

c Select ERF or TR (IDA=1 for ERF input, 2 for TR).
      if(ida.eq.1) then
         erf=erftr
         tr=ta+erf/4.33
 20      trold=tr
         tr=ta+erf/(.72*4.*5.67e-8*((tr+ta)/2.+273.15)**3)
         if(abs(tr-trold).gt.0.01) goto 20
      else
         tr=erftr
         erf=.72*4.*5.67e-8*((tr+ta)/2.+273.15)**3*(tr-ta)
      endif

c Select RH or PA (IDB=1 for RH input, 2 for PA).
      if(idb.eq.1) then
         rh=rhpa
         pa=rh*vdd(ta)
      else
         pa=rhpa
         rh=pa/vdd(ta)
      endif

c Select CLO or CLOE (IDC=1 for CLO input, 2 for CLOE).
c CLO = intrinsic clothing insulation, CLOE = effective cloth. insul.
c CLO mode : ref. point for heat tr. calc. is at clothing surface.
c Skin + clothing area is greater than skin area alone by factor FACL.
c CLOE mode : reference point is at skin surface; FACL=1.
c Note : KCLO=0 --> CLO=CLOE,  KCLO>0 --> CLO>CLOE.
c Need CLOE or CLO and FACL for first evaluation of CHR and TSK
c in LETLOOP.  After first iteration, all calculations use CLO.
      cloe=clo
      facl=1.
      if(idc.eq.1) facl=1.+kclo*clo
      chclo=1.e5
      if(clo.gt.0.01) chclo=1./(.155*clo)

c Select ICL or IM (IDD=1 for ICL input, 2 for IM input).
      if(idd.eq.2) im=icl

c Call initial values subroutine.
      call letinit(ta,vel,act,clo)

c Simulation of body temperature regulation.  Loop (1 min. time increments).
c itime,itim = total exposure time, integrated time since start.
      itim=0
  100 itim=itim+1
      call letloop(ta,act,clo)

c Compute indices only every isteps minutes.
      if(mod(itim,isteps).ne.0) goto 30
      call letindex(ta,vel,rhpa,act,clo,set,pme,pmv)

      xppd = ppd(pmv)

      x=0.
      time=float(itim)

 30   continue
      if(itim.lt.itime) goto 100

c Determination of the verbal category.
        if (pmv.ge.-0.5.and.pmv.le.0.5) icmfrt = 5
        if (pmv.gt.0.5.and.pmv.lt.0.8) icmfrt = 6
        if (pmv.ge.0.8.and.pmv.lt.2.0) icmfrt = 7
        if (pmv.ge.2.0.and.pmv.lt.3.5) icmfrt = 8
        if (pmv.ge.3.5.and.pmv.lt.4.5) icmfrt = 9
        if (pmv.ge.-0.8.and.pmv.lt.-0.5) icmfrt = 4
        if (pmv.ge.-2.0.and.pmv.lt.-0.8) icmfrt = 3
        if (pmv.ge.-3.5.and.pmv.lt.-2.0) icmfrt = 2
        if (pmv.ge.-4.5.and.pmv.lt.-3.5) icmfrt = 1
        if (pmv.lt.-4.5.or.pmv.gt.4.5) icmfrt = 12

      return
      end

c ******************** LETINIT ********************

c PART II: Initial Values Subroutine 'LETINIT.f'

c Compute initial physiological and heat transfer variables.
c Variables in common block LET2 are computed in this routine.

      subroutine letinit(ta,vel,act,clo)

      real lr,icl,im,kclo
#include "gagge.h"

c Set initial values for physiological variables:
c initial conditions at physiological thermal neutrality and
c steady state when default values are used,
c or final values of previous run when environments are chained.
      alpha=alphai
      tsk=tski
      tcr=tcri
      tbm=alpha*tsk+(1.-alpha)*tcr
      skbf=skbfi
      esk=eski

      if(clo.le.0.)then
         eveff=.38*vel**(-.29)
         icl=1.
      else
         eveff=.59*vel**(-.08)
      endif

c When input evef is positive, eveff=input value
      if(evef.gt.0.) eveff=evef
      if(ata.le.0.) ata=1.

c Activity level:
c activity may be entered in MET units or in W/m^2.
      if(act.lt.15.) act=act*58.2
      rm=act
      wk=we
      if(abs(we).lt.1.5) wk=wk*act

c All heat and mass transf. coef. here are expressed in W/K per sq.
c meter of skin surface.  Sk. surf. is evaluated by DuBois height-
c weight relation.
c First approximation for rad. heat tr. coeff. (fct of cloth. temp.)
c cloth.+ heat tr. coeff. at sea level.
      chr=4.7

c Calculation of conv. heat tr. coeff. (constant during exposure)
c chca corresponds to walking in still air
      chca=0.
      if((act/58.2).gt.0.85) chca=5.66*((act/58.2-.85)*ata)**.39

c chcv=room air movement.
      chcv=8.6*(vel*ata)**.53
      chc=amax1(chca,chcv)

c chc=3.0 still air sea level (corresponds to air velocity of .137 m/s).
      if(chc.lt.(3.*ata**.53))
     1   chc=3.*ata**.53
      if(chcc.gt.0.) chc=chcc

c Lewis relation at sea level and 25 C : lr=2.2 K/mmHg.
c 2.2 *760/101.325 = 16.50 K/kPa
      lr=16.50/ata

c ff=1 for air.  ff=.7 for helium at ATA>10.
      ff=1.

c If ata>10 correct chc, lr and ff for helium env.
      if(ata.ge.10.) then
         chc=2.*chc
         lr=.7*lr
         ff=.7
      endif

c Initial estimate of clothing temperature.
      tcl=(chclo*tsk+facl*(chc*ta+chr*tr))/(chclo+facl*(chc+chr))

      return
      end

c ******************** LETLOOP ********************

c PART III: Simulation of Body Temperature Regulation 'LETLOOP.f'

c Perform minute by minute integration of temperature regulation using
c physiological data from previous time step and current environmental
c conditions.
c Variables in common block LET3 are computed in this routine.

      subroutine letloop(ta,act,clo)
      real lr,icl,im,kclo
#include "gagge.h"

c Dry heat balance : solve for tcl and chr.
 21   tclold=tcl
      chr=4.*.72*5.67e-8*((tcl+tr)/2+273.15)**3
      tcl=(chclo*tsk+facl*(chc*ta+chr*tr))/(chclo+facl*(chc+chr))
      if(abs(tcl-tclold).gt.0.01) goto 21

c Heat flow from clothing surface to environment (facl=1. if cloe used).
      dry=facl*(chc*(tcl-ta)+chr*(tcl-tr))

c Compute transfer coefficients in terms of CLO (IDC=1 if CLO input).
c Dry heat exchange can be computed either in terms of CLO or CLOE
c in first iteration. CLO is used in all calculations afterwards.
      ctc=chr+chc
      if(idc.eq.1) then
         cloe=clo-(facl-1.)/(.155*facl*ctc)
      else
         clo=1.-kclo*cloe-kclo/(.155*ctc)
         clo=(-clo+sqrt(clo*clo+4.*kclo*cloe))/(2.*kclo)
         facl=1.+kclo*clo
      endif
      chclo=1.e5
      if(clo.gt.0.01) chclo=1./(.155*clo)

c Dry and latent respiratory heat losses.
c 5.8662 kPa=44 mmHg; .017251=.0023*760 mmHg/101.325 kPa
      eres=.017251*rm*(5.8662-pa)
      cres=.0014*rm*(34.-ta)*ata*ff

c Heat flows to skin and core (W/m^2).
      hfsk=(tcr-tsk)*(5.28+1.163*skbf)-dry-esk
      hfcr=rm-(tcr-tsk)*(5.28+1.163*skbf)-cres-eres-wk

c Thermal capacities (average man : 70 kg, 1.8 square meter).
c 58.2 W.min/(kg.K). Therm. capacities (TCCR, TCSK) in W.min/K.
      tccr=58.2*(1.-alpha)*70.
      tcsk=58.2*alpha*70.

c Temperature changes in 1 minute (DTSK, DTCR) in K/min.
      dtsk=(hfsk*1.8)/tcsk
      dtcr=(hfcr*1.8)/tccr
c      dtbm=alpha*dtsk+(1.-alpha)*dtcr
      tsk=tsk+dtsk
      pssk=vdd(tsk)
      tcr=tcr+dtcr

c Definition of vascular control signals and physiological
c temperature regulation. Physiological constants
c have been measured for average (unacclimatized) population
c (John B. Pierce Foundation, 1980-82).

c TTCR, TTSK, and TTBM and standard set points for core, skin and
c average body temperatures corresponding to physiol. neutrality
c Typical values for TTCR, and TTSK are 36.8, and 33.7.
c BZ is the stand. ratio of skin mass to total body mass (skin+core):
c BZ = 0.1.
c ALPHA is the actual ratio of skin mass to total body mass.
c Constants for vasodilation :     cdil = 200 liters/(m2.hr.K)
c               vasoconstriction : cstr = 0.1 dimensionless
c               reg. sweating  :   csw  = 170 g/(m2.hr).
c 6.3 liter/(m2.hr) is normal skin blood flow in the absence
c of any thermoregulatory vascular control.
c max. skin blood flow : skbfl = 90 liter/(m2.hr).
c maximum sweating rate : regswl = 500 g/m2.hr.

c Definition of vascular control signals.
      if(tsk.gt.ttsk) then
         warms=tsk-ttsk
         colds=0.
      else
         colds=ttsk-tsk
         warms=0.
      endif
      if(tcr.gt.ttcr) then
         warmc=tcr-ttcr
         coldc=0.
      else
         coldc=ttcr-tcr
         warmc=0.
      endif
      ttbm=bz*ttsk+(1.-bz)*ttcr
      tbm=alpha*tsk+(1.-alpha)*tcr
      if(tbm.gt.ttbm) then
         warmb=tbm-ttbm
         coldb=0.
      else
         coldb=ttbm-tbm
         warmb=0.
      endif

c Physiological temperature regulation.

c Control skin blood flow.
      dilat=cdil*warmc
      stric=cstr*colds
      skbf=(6.3+dilat)/(1.+stric)

c skbf is never below 0.5 liter/(m2.hr) nor above skbfl.
      if(skbf.lt.0.5) skbf=0.5
      if(skbf.gt.skbfl) skbf=skbfl

c Ratio of skin-core masses changes with skbf.
c (alpha,skbf)=(.15,6.3),(.45,1.24),(.05,90)
      alpha=.0417737+.7451832/(skbf+.585417)

c Control of regulatory sweating. REGSW in gr./m2.hr.
c 0.68 Wh/gr. is latent heat of evaporation of sweat.
      regsw=csw*warmb*exp(warms/10.7)
      if(regsw.gt.rgswl) regsw=rgswl
      ersw=.68*regsw

c Adjustment of metabolic heat due to shivering (Stolwijk, Hardy).
      rm=act+19.4*colds*coldc

c Evaluation of heat transfer by evaporation at skin surface.
c Mass transfer equation between skin and environment.
c RT is total vapor resistance of clothing + air layer.
c IM is efficency of mass transfer for (clothing + air layer)
c defined with respect to total (rad. and conv.) heat transfer.
c ICL is efficiency of mass transfer for clothing alone with respect
c to convective heat transfer.
c Reference : Woodcock, Breckenridge and Goldman.
      if (idd.eq.1) then
         rt=(1./lr)*(1./(facl*chc)+1./(chclo*icl))
         im=(1./(rt*lr))*(1./(facl*ctc)+1./chclo)
      else
         rt=(1./(im*lr))*(1./(facl*(chc+chr))+1./chclo)
         icl=facl*chc/(chclo*(rt*lr*facl*chc-1.))
      endif
      emax=(1./rt)*(pssk-pa)

c Air and hyperbaric environments.
      prsw=ersw/emax

c .06 is pdif for nonsweating skin --- Kerslake.
      pdif=(1.-prsw)*.06
      edif=pdif*emax
      esk=ersw+edif
      pwet=esk/emax

c Beginning of dripping (Sweat not evaporated on skin surface).
      if((pwet.ge.eveff).and.(emax.ge.0.)) then
         pwet=eveff
         prsw=(eveff-.06)/.94
         ersw=prsw*emax
         pdif=(1.-prsw)*.06
         edif=pdif*emax
         esk=ersw+edif
      endif

c When emax<0. condensation on skin occurs. Esk is latent heat
c exchange at skin surface.
      if(emax.lt.0.) then
         pdif=0.
         edif=0.
         esk=emax
         pwet=eveff
         prsw=eveff
         ersw=0.
      endif

c edrip = unevaporated sweat in air.
c edrip grams/sq.m/hr (swdrip on listing).
      edrip=(regsw*.68-prsw*emax)/0.68
      if(edrip.lt.0.) edrip=0.

c Vapor pressure at skin (as measured by dewpoint sensors).
      vpsk=pwet*pssk+(1.-pwet)*pa

c rhsk is skin relative humidity.
      rhsk=vpsk/pssk

      return
      end

c ******************** PPD ********************

c Funktie PPD berekent de PPD van Fanger aan
c de hand van de PMV-index.

        function ppd(pmv)

        pmv2 = pmv * pmv
        pmv4 = pmv2 * pmv2
        ppd = 100. - 95. * exp(-0.2179 * pmv2 - 0.03353 * pmv4)

        return
        end

c ******************** VDD ********************

c SVP subroutine: computes saturation water vapor
c pressure at saturation for given air temperature.
c input=degrees cel, output=kPa.

        function vdd(temp)
        vdd=exp(16.6536-4030.183/(temp+235.))
        return
        end

C ******************** LETINDEX ********************

C PART IV: Comfort and Sensory Indices 'LETINDEX.f'

C Compute environmental and comfort indices.
C Inputs to this routine are the physiological data from
C the temperature regulation simulation part.
C Variables in common block LET3 are computed in this routine.

      subroutine letindex(ta,vel,rhpa,act,clo,set,pme,pmv)

      real lr,icl,ias,ims,icls,im,kclo,kclos
#include "gagge.h"

C Statement Functions: calculation of ET* and SET* by Newton's
C iterative technique requires successive evaluations of the same
C function (ERR or ERRS).
      err(et1)=hsk-ctc*fcle*(tsk-et1)-pwet*lr*chc*facl*fpcl*
     &         (pssk-vdd(et1)/2.)
      errs(set1)=hsk-ctcs*fcles*(tsk-set1)-pwet*lr*chcs*facls*fpcls*
     &          (pssk-vdd(set1)/2.)

C DELTA is the infinitesimal increment for evaluation of derivatives
C in Newton's method.
      delta=1.e-4

C Section I : Real environment heat and mass transfer indices.

C In previous sections all heat and mass fluxes were calculated
C using the standard heat and mass transfer equations that can be
C found in any textbook (ASHRAE Handbook of Fundamentals, chapters 2
C and 3 for example).
C The following equations are specifiC to the field of thermal
C comfort (ASHRAE Handbook of Fundamentals, Chapter 8). Both
C methods lead to the same results. Some of the calculations are
C redundant but are kept for debugging purposes.

c Section I-a : Heat Transfer
c        temperature gradient (driving force) = tsk-to
c           to=ta+erf/ctc, ctc=chc+chr     effective radiant field mode
c           to=(chr*tr+chc*ta)/(chc+chr)   radiant temperature mode
c        dry heat transfer from skin to environment
c           dry=fcle*ctc*(tsk-to)    or   dry=facl*fcl*ctc*(tsk-to)
c           fcle=1./(1.+.155*ctc*cloe)    fcl=1./(1.+.155*facl*ctc*clo)
c           fcle=facl*fcl
c        effective sensible heat transfer coefficient
c           efctc=ctc*fcle                efctc=facl*fcl*ctc
c           also efctc=6.45/(clo+cloa),  cloa=1./(facl*ctc)
c Section I-b : Mass Transfer
c        pressure gradiant (driving force) = (pssk-pa)
c        latent heat transfer from skin to environment
c           esk=lr*facl*fpcl*chc*pwet*(pssk-pa)
c           fpcl=1./(1.+(.155/icl)*facl*chc*clo)
c        effective latent heat transfer coefficient
c           efche=lr*facl*fpcl*chc.  Also efche=lr*im*6.45/(clo+cloa)
c        icl and im are related:
c           im=fpcl*ia+(1.-fpcl)*icl,     ia=chc/ctc
c           1./im=fcl/ia+(1.-fcl)/icl
c        note: one can also define an effective mass transfer coeff.
c        however, the equation fpcle=facl*fpcl (analog of heat transfer)
c        is not valid: clo-cloe and fcl-fcle equations depend on the
c        total (radiative + convective) heat transfer whereas the
c        fpcl-fpcle relationship should depend on convection only.
c           fpcle=1./(1.+(.155/icl)*chc*cloe)
c           esk<>lr*fpcle*chc*pwet*(pssk-pa)

      to=ta+erf/ctc
      fcle=1./(1.+.155*ctc*cloe)
      fpcl=1./(1.+(.155/icl)*facl*chc*clo)
      efctc=ctc*fcle
      efche=lr*chc*facl*fpcl

c Section II : ET* (standardized humidity/actual CLO ,ATA and CHC).

c Calculation of heat storage: heat balance equation.
c Net metabolic rate - (respiratory heat loss) - (skin heat loss)
      store=(rm-wk)-(cres+eres)-(dry+esk)

c calculation of skin heat loss (HSK=DRY+ESK)
      hsk=ctc*fcle*(tsk-to)+pwet*lr*chc*facl*fpcl*(pssk-pa)

c HSK in ET* environment (50% rh) equals HSK in real environment.
c Get a low approximation for ET* and find ET* which
c satisfies the skin heat transfer equation by iteration.
c Newton's method : x new = x old - F(x)/F'(x) (from Taylor's series)
c ETOLD = old approx. for ET*, ERR (at the beginning of this routine)
c is the function of which the root (ET*) must be found.
c ERR is HSK in real environment minus HSK in ET* environment.
c (ERR2-ERR1)/DELTA is the numerical approximation to the derivative.
      etold=tsk-hsk/(ctc*fcle)
   90 continue
         err1=err(etold)
         err2=err(etold+delta)
         etnew=etold-delta*err1/(err2-err1)
         if(abs(etnew-etold).lt.0.01) goto 91
         etold=etnew
         goto 90
   91 et=etnew

c Section III : Standard effective temperature SET*
c Standardized humidity. CHC, CLO, ATA normalized for given activity.
c Same as ET* but normalize heat transfer coefficients.

c Standard environment.
      chrs=chr

c CHCS = standard conv. heat tr. coeff. (level walking/still air).
      chcs=0.
      if((act/58.2).gt.0.85) chcs=5.66*(act/58.2-.85)**.39

c Minimum value of chc at sea level = 3.0 (vel=.137 m/s).
      if(chcs.lt.3.) chcs=3.

c Standard MET-CLOS relation gives SET*=23.5 C when PMV=PMV*=0
c by using initial values, part II above.
      rn=rm-wk
      clos=1.5200/(rn/58.15+.6944)-.1835
      kclos=.25
      facls=1.+kclos*clos
      ctcs=chrs+chcs
      cloes=clos-(facls-1.)/(.155*facls*ctcs)
      fcles=1./(1.+.155*ctcs*cloes)
      fcls=1./(1.+.155*facls*ctcs*clos)

c IM or ICL can be selected as reference for standard environment.
c Here, IM is selected and ICL is derived from the Oohori relation.
c IMS = .45 for typical environment (per Breckenridge, 9/15/86)
c 1./IMS = FCLS/IAS + (1.-FCLS)/ICLS  (Oohori relation, 1985).
      ims=.45
      ias=chcs/ctcs
      icls=ims*ias*(1.-fcls)/(ias-fcls*ims)
      fpcls=1./(1.+(.155/icls)*chcs*clos*facls)

c HSK in SET* environment equals HSK in real environment.
c SET* using Newton's method (as in Section II, calculation of ET*).
      etold=tsk-hsk/(ctcs*fcles)
   92 continue
         err1=errs(etold)
         err2=errs(etold+delta)
         etnew=etold-delta*err1/(err2-err1)
         if(abs(etnew-etold).lt.0.01) goto 93
         etold=etnew
         goto 92
   93 set=etnew

c STO and SVPO are coordinates of psychrometric chart for
c Standard Environment.(standardized heat tr. coeff.)
c STO = standard operative temperature.
c DRY in STO-SVPO environment equals DRY in real environment.
      sto=ctc*fcle/(ctcs*fcles)*to+(1.-ctc*fcle/(ctcs*fcles))*tsk

c SVPO = standard operative vapor pressure.
c ESK in STO-SVPO environment equals ESK in real environment.
      svpo=(chc*facl*fpcl/(chcs*facls*fpcls))*pa+
     1 (1.-(chc*facl*fpcl)/(chcs*facls*fpcls))*pssk

c For standard environmental conditions with MET-CLO relation,
c TO is equal to STO.

c Section IV a : Fanger's comfort eqn. Predicted mean vote (PMV).

c ESW is evaporative heat loss by sweat only and is a measure of
c the heat load on the human body in zone of evaporative
c thermoregulation.
c Note: following eqn is the heat balance eqn with 0 storage.
      esw=rn-cres-eres-ctc*fcle*(tsk-to)-edif

c In cold climates, ESW is 0. ESW in eqn is replaced by negative
c STORE, i.e. rate of body cooling.
c In extremely warm climates, ESW=EMAX+STORE (rate of body heating).

c Fanger's reg. sweating at comfort threshold (PMV=0) is:
      ecomf=(rn-58.2)*.42
      if(ecomf.lt.0.) ecomf=0.

c PMV, the classic Fanger's index, is positive in warm climates.
c     pmv=(.303*exp(-.036*rm)+.028)*(esw-ecomf)
      closi = clo * 0.155
      call fanger(act,wk,closi,ta,tr,vel,pa,pmv)

c Section IV b : PMV* : rational alternative to Fanger's PMV in terms
c of ET* or SET instead of TO or STO to account for humidity.

c PMVET (PME in prgm) uses ET instead of TO.
c --- Gagge's new Tsk relation (21 june 1988, private communication)
      gtsk=33.103 + (0.4667 / 58.15) * rm * (1.-we)
      eswe=rn-cres-eres-ctc*fcle*(gtsk-et)-edif
      pme=(.303*exp(-.036*rm)+.028)*(eswe-ecomf)

c PMVSET (PMS in prgm) uses SET instead of STO.
      esws=rn-cres-eres-ctcs*fcles*(tsk-set)-edif
      pms=(.303*exp(-.036*rm)+.028)*(esws-ecomf)
c
c For standard environmental conditions with MET-CLO relation,
c PMVET is equal to PMVSET or PMV* as used in text.

c Section V : Warm indices derived from ESK, EMAX, W (skin wettedness)
c and WCRIT.

c EMAX is readjusted for EVEFF or PCRIT or ECRIT/EMAX.
      emax=emax*eveff

c DISC (discomfort) varies with relative thermoregulatory heat strain.
c Only valid for positive values of DISC (warm climates),
c does not apply to cold environments.
      disc=4.7*(ersw-ecomf)/(emax-ecomf-edif)

c Belding's classic heat stress index (HSI).
      ereq=rn-cres-eres-ctc*fcle*(tsk-to)
      hsi=100.*ereq/emax

c Belding's HSI is also an index of thermoregulatory strain.
c Heart rate (HR) is a fct of HSI and RM.
c HR data base (Gonzales, 1968) as follows:
c (RM,HSI,HR)=(1,10,75),(1,100,110),(3,18,92),(3,100,170).
      if(hsi.le.0.) then
         hr=1.885*rm/58.2+69.225
      else
         hr=(.281*rm/58.2+.108)*hsi+1.885*rm/58.2+69.225
      endif
      if(hr.gt.220.) hr=220.

c Section VI : Thermal sensation TSENS as funct. of mean body temp.
c TSENS is an alternate to PMV and DISC.
c This section not used in ASHRAE reference above.

c TBML is TBM when DISC and PMV are 0. (near lower limit of zone
c of evap.  regul.).
      tbml=(.194/58.15)*rn+36.301

c TBMH is TBM when HSI=100 (upper limit of zone of evap.regul.).
      tbmh=(.347/58.15)*rn+36.669

c TSENS=DISC=4.7*WCRIT when WCRIT (i.e. EVEFF) is less than unity.
c In cold, TSENS is a negative function of TBM.
c In extreme heat, when HSI>100, TSENS is a positive function of TBM.
      if(tbm.lt.tbml) then
         tsens=.4685*(tbm-tbml)
      elseif(tbm.lt.tbmh) then
         tsens=4.7*eveff*(tbm-tbml)/(tbmh-tbml)
      else
         tsens=4.7*eveff+.4685*(tbm-tbmh)
      endif

c For standard environment, EVEFF (WCRIT) is set at .85.
c For any standard environment at TA and 50% RH, PMVSET=TSENS.
c TSENS in the cold and the extreme heat as defined above is
c tentative and subject to modifications.

c Section VII : Other indices

c On Mollier chart, SLOPE is the slope of a locus of constant ET*
c or SET*. SLOPE in K/kPa.(ta=ordinate,pa=abscissa).
      slope=pwet*efche/efctc

c The line through ET* with slope equal to SLOPE on the Mollier
c chart can be viewed as a line of constant enthalpy of the total
c environment (taking into account clothing heat and mass transfer
c characteristics, skin wettedness and total dry heat transfer)
c for the characterization of the heat and mass transfer from a
c human subject.

c For the humid environment alone (no correction for skin wetted-
c ness, clothing, and radiative heat transfer) the psychrometric
c ratio is the Lewis Relation LR: 16.5 K/kPa (Combined heat and mass
c transfer from a flat wet plate due to convection).

c For the total environment (including clothing), the analogous
c psychrometric ratio is the slope corresponding to PWET = WCRIT,
c i.e.,the maximum wettedness.
c WCRIT*EFCHE/EFCTC or WCRIT*LR*IM

      return
      end

C ******************** FANGER ********************

C Subroutine to calculate Fanger's PMV index according
C to ISO 7730 - 1984.
      subroutine fanger(m,w,iclsi,ta,tr,var,pa,pmv)
      real m, iclsi, mw
      eps = 0.00015
      mw = m - w

C Compute the corresponding fcl value.
      fclf=1.05+0.645*iclsi
      if (iclsi.lt.0.078) fclf=fclf-0.05+0.645*iclsi
      fcic=iclsi*fclf
      p2=fcic*3.96
      p3=fcic*100
      tra=tr+273.
      taa=ta+273.
      p1=fcic*taa
      p4=308.7-0.028*mw+p2*(tra/100)**4

C First guess for surface temperature.
      tcla=taa+(35.5-ta)/(3.5*(6.45*iclsi+0.1))
      xn=tcla/100
      xf=xn

      hcf=12.1*sqrt(var)
      noi=0

C Compute surface temperature of clothing by iterations.
 100  xf=(xf+xn)/2
      hcn=2.38*abs(100*xf-taa)**0.25
      hc=amax1(hcf,hcn)
      xn=(p4+p1*hc-p2*xf**4)/(100.+p3*hc)
      noi=noi+1
      if (noi.gt.150) goto 200
      if (abs(xn-xf).gt.eps) goto 100
      tclf=100*xn-273.

C Compute predicted mean vote.
      pm1=3.96*fclf*(xn**4-(tra/100)**4)
      pm2=fclf*hc*(tclf-ta)
      pm3=0.303*exp(-0.036*m)+0.028
      pm4=0.0
      if (mw.gt.58.15) pm4=0.42*(mw-58.15)
      pmv=pm3*(mw-3.05*0.001*(5733.-6.99*mw-pa*1000.)
     &-pm4-1.7*0.00001*m*(5867.-pa*1000.)-0.0014*m*(34.-ta)-pm1-pm2)

C Allow more extreme pmv to be reported.
C      if (abs(pmv).gt.3.0) goto 200
      goto 300
 200  pmv=999999.999

 300  continue
      return
      end

C ******** scnocup
C Scans unaveraged occupancy in each of the zones of the probem. Weekdays are
C assumed to be in ioucpz(izone,[1-24]), saturdays ioucpz(izone,[25-48]),
C sundays ioucpz(izone,[49-72]), This hash table will need altering 
C if sub-hour casual gains definitions are implemented. 
      subroutine scnocup(ier)
#include "building.h"

      common/pophelp/h(60)
      COMMON/SET1/IYEAR,IBDOY,IEDOY,IFDAY,IFTIME
      COMMON/SIMPIK/ISIM,ISTADD,ID1,IM1,ID2,IM2,ISDS,ISDF,NTS,ISAVE
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
      character hold*24,h*72,outs*124

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
        h(1)='Weekday occupancy period (start and stop hours). '
        write(HOLD,'(a)') '  0  24  '
        CALL EASKS(HOLD,' ',' Weekday occupancy period: ',
     &     24,' 0  24 ','wkd occup period',IER,1)
        K=0
        CALL EGETWI(HOLD,K,iwkdst,0,24,'W','iwkdst',IER)
        CALL EGETWI(HOLD,K,iwkdfn,iwkdst,24,'W','iwkdst',IER)

        h(1)='First weekend day occupancy period (start & stop hours).'
        write(HOLD,'(a)') '  0  24  '
        CALL EASKS(HOLD,' ',' First weekend day occupancy period: ',
     &     24,' 0  24 ','sat occup period',IER,1)
        K=0
        CALL EGETWI(HOLD,K,isatst,0,24,'W','isatst',IER)
        CALL EGETWI(HOLD,K,isatfn,isatst,24,'W','isatst',IER)

        h(1)='2nd weekend day occupancy period (start & stop hours).'
        write(HOLD,'(a)') '  0  24  '
        CALL EASKS(HOLD,' ',' Second weekend day occupancy period: ',
     &     24,' 0  24 ','sat occup period',IER,1)
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

C Debug...
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
#include "tdf2.h"

      common/recver/izver,ipver,iever
      COMMON/SET1/IYEAR,IBDOY,IEDOY,IFDAY,IFTIME
      COMMON/SIMPIK/ISIM,ISTADD,ID1,IM1,ID2,IM2,ISDS,ISDF,NTS,ISAVE
C      COMMON/OUTIN/IUOUT,IUIN
      common/c1/ncomp,ncon
      common/cmftpar/WVEL,ACTL,CLO,iocut,iocuset,ioucpz(MCOM,72)

C Temporal db commons.
      COMMON/TDFFLG2/IALLCLM,ICASUAL(MCOM),IZIVENT(MCOM),IRAIRVL(MCOM),
     &       ISETPTT(MCOM),ICTLSTA(MCOM),ISKYLUX,IDBTEXT,IWINDVL,
     &       IWINDDR,IRELHUM,IDIFHSL,IDIRSOL,IGLOHSL,IOBJVEL,IOBJDIR,
     &       ISKYTMP,IGLOVRT,ICASUAL3(MCOM),IACTIV(MCOM),
     &       IBIDIRS(MCOM,MS)
      COMMON/GET1/VAL1(MZS,MTS),VAL2(MZS,MTS),VAL3(MZRL,MTS)

C.........START: USED FOR ACTIVITY TDF DATA.....................
C TDFINT are ranges allowed for integer fields in the instance.
      COMMON/TDFINT/ITMAR(MIT,MGAI),ITMIN(MIT,MGAI),ITMAX(MIT,MGAI)

      COMMON/TDFREL/TMAR(MIT,MGAR),TMIN(MIT,MGAR),TMAX(MIT,MGAR)
      REAL X2a_QTOTOCCUP
      REAL X2c_QTOTEQUIP
      
      COMMON/RECV5/SNA(MCOM,MS),ZBASEA(MCOM),ZTRANA(MCOM)
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
            if(ioc.gt.0) write(6,*) '* Found temporal occupancy @',itt
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
        if(ioc.gt.0) write(6,*) '* Found temporal occupancy @',itt
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
        if(ioc.gt.0) write(6,*) '* Found temporal occupancy @',itt
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
        write (6,*) 
     &        ' Missing information on local air T, setting dT to zero.'
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
