C ESP-r is free software.  You can redistribute it and/or
C modify it under the terms of the GNU General Public
C License as published by the Free Software Foundation 
C (version 2 or later).

C ESP-r is distributed in the hope that it will be useful
C but WITHOUT ANY WARRANTY; without even the implied
C warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
C PURPOSE. See the GNU General Public License for more
C details.


C This file contains the following routines:
C MOCMFT:  provides a zone comfort assessment for a particular 24 hour period.
C getocup: Determine if zone is occupied at time step, for filtering.
C DISCOMF: Returns local dicomfort information (PPD).
C DRAUGHT: PPD due to draughts (nothing calls this yet).
C OFFSETTEMP: air velocity required to maintain comfort (nothing calls this yet).
C ASHRAEZONE: inside/ outside ASHRAE comfort zone (nothing calls this yet).
C RCFDVRTS: Reads domain origins from a CFD domain file (needed for old CFD results files).
C VERTdT:  Calculate vertical temperature difference between top and
C          bottom of an MRT sensor, using CFD results.

C ************ MOCMFT
C Provides a zone comfort assessment for a particular 24 hour period.
C If act = 'V' then provide verbose (standard) comfort facility.
C        = 'I' set comfort parameters before calling D or M.
C        = 'D' return PPD in GVAL.
C        = 'M' return PMV in GVAL.
C        = 'E' return PMV (effective temperature) in GVAL.

      SUBROUTINE MOCMFT(IDAY,IZONE,IMRT,act)
#include "building.h"
#include "model.h"
#include "geometry.h"
#include "schedule.h"
#include "cfd.h"
#include "net_flow.h"
#include "net_flow_data.h"
#include "help.h"
      
      integer lnblnk  ! function definition

      COMMON/SPAD/MMOD,LIMIT,LIMTTY
      COMMON/OUTIN/IUOUT,IUIN,IEOUT
      COMMON/OUTPCH/ICOUT
      integer childterminal  ! picks up mmod from starting of prj
      common/childt/childterminal

      COMMON/SIMPIK/ISIM,ISTADD,ID1,IM1,ID2,IM2,ISDS,ISDF,NTS,ISAVE
      common/exporttg/xfile,tg,delim
      COMMON/EXPORTI/ixopen,ixunit,ixpunit
      COMMON/ZONPIK/NZ,NZNO(MCOM)

C Comfort parameters and hash table.
      common/cmftpar/WVEL,ACTL,CLO,iocut,iocuset

C Pre-calculated data (filtered).
      common/MMAFDAT/IFILT,RDFMAX(MCOM,3,MZRL),RDFMIN(MCOM,3,MZRL),
     &    RDFAVE(MCOM,3,MZRL),RDTFMAX(MCOM,3,MZRL),RDTFMIN(MCOM,3,MZRL)

      COMMON/GET2/XDUM(MTS),XDUM1(MTS),GVAL(MTS)
      COMMON/IGETFLG/IOCUPF,ialstused,IROC

      common/recver/izver,ipver,iever
      common/timfltr/iwkdst,iwkdfn,isatst,isatfn,isunst,isunfn

      DIMENSION ID(12),OP(MS),TS(MS),TSO(MS),QV(MS),ITEMS(14)
      character doit*248,tmode*8
      character longtfile*144,longtfiledos*144
      CHARACTER*25 DESC(12)
      character outs*124,TSTR*24,ZN*12,hold*24
      character xfile*144,ITEMS*25,tg*1,delim*1,act*1
      character*12 ll1,ll2,ll3
      logical ok,concat,unixok
      integer IOT  ! for radio button
      integer NITMS,INO ! max items and current menu item

      COMMON/FILEP/IFIL
      COMMON/ICFNOD/ICFD,ICP
      character LMRT*72
      real XC,YC,ZC
      integer IIX,IIY,IIZ
      common/CFDSV/IRECPC,ICFDSV,IEQSV(5+MCTM)
      integer igraphiclib      ! external definition
      logical XST

      COMMON/VARf/Uf(ntcelx,ntcely,ntcelz),Vf(ntcelx,ntcely,ntcelz),
     1            Wf(ntcelx,ntcely,ntcelz),
     2            P(ntcelx,ntcely,ntcelz),PP(ntcelx,ntcely,ntcelz),
     3            TEf(ntcelx,ntcely,ntcelz),EDf(ntcelx,ntcely,ntcelz)
      COMMON/TEMPf/Tf(ntcelx,ntcely,ntcelz),GAMH(ntcelx,ntcely,ntcelz),
     1             RESORT,NSWPT,URFT,FSDTT,PRANDL,PFUN

      COMMON/useSenPMV/useSenMRT(MCOM),useSenCFD(MCOM),IXCEL(MCOM),
     &                 IYCEL(MCOM),IZCEL(MCOM)
      logical useSenMRT,useSenCFD
      COMMON/cfdsmper/ICDYS,ICDYF,CFTS,CFTF
      integer IZLST(MCUB),ISLST(MCUB),NSEL
      COMMON/cfdfil/LCFD(MCOM),IFCFD(MCOM)
      CHARACTER LCFD*72
      COMMON/cfddoms/NCDOM,ICFDZ(MNZ)

C Directives for GOCFDGET (more efficient than RDCFDAT).
      COMMON/MFPICK/NMFGET,IMFGETNO(MFRS,8)
      COMMON/MFGET/FLOWVALS(MFRS),MFRECVR(MCNN+2+MCONTM)
      COMMON/MFPK2/NMFLST,IMFLIST(MFCNLST,MFCNEL+1)

      real PI,R,SA,CA,DX,DY

      DATA DESC/'very cold, danger        ','cold, shivering          ',
     &          'cool, unpleasant         ','slightly cool, acceptable',
     &          'comfortable, pleasant    ','slightly warm, acceptable',
     &          'warm, unpleasant         ','hot, very uncomfortable  ',
     &          'very hot, danger         ','unoccupied               ',
     &          'non-sedentary            ','out of range             '/

      DATA ID/31,28,31,30,31,30,31,31,30,31,30,31/

C Check if Unix or Windows.
      helpinsub='comfort'  ! set for subroutine
      call isunix(unixok)

C Initialise comfort variables.
      if(act.eq.'I'.or.act.eq.'V')then
        OP(1)=-1.
      endif

C If initial settings and initial request ask parameters.
      if(act.eq.'I')then
        if(iocuset.eq.1)then
          helptopic='res_alter_comfort_par'
          call gethelptext(helpinsub,helptopic,nbhelp)
          call easkok(' ','Use existing comfort parameters?',ok,
     &      nbhelp)
          if(ok)return
        endif

C Confirm parameters and then exit.
        helptopic='res_review_comfort_par'
        call gethelptext(helpinsub,helptopic,nbhelp)
        call easkr(CLO,' ','Clothing level?',
     &    0.0,'W',3.0,'W',1.0,'clothing level',IER,nbhelp)

        helptopic='res_review_activity_par'
        call gethelptext(helpinsub,helptopic,nbhelp)
        CALL EASKAB(' ','Activity level units?',
     &    'MET units','W/m^2 (body surface area)',IAU,nbhelp)
        if(IAU.eq.1)then

C When using MET units, convert back to W/M^2 for calculations.
C Assume default MET is equiv to 85W/m^2.
          ACTL = ACTL/58.2
          ACTLDEF = 85.0/58.2
          call easkr(ACTL,' ','Activity level (MET)?',0.859,
     &      'W',6.013,'W',ACTLDEF,'activity level MET',IER,nbhelp)
          ACTL = ACTL * 58.2
        elseif(IAU.eq.2)then
          call easkr(ACTL,' ','Activity level (W/m^2)?',
     &      50.0,'F',350.0,'F',85.0,'activity level W/m^2',IER,nbhelp)
        endif

        helptopic='res_comfort_air_velo'
        call gethelptext(helpinsub,helptopic,nbhelp)
        call easkr(WVEL,' ','Air velocity?',
     &    0.0,'W',5.0,'W',0.15,'air velocity',IER,nbhelp)

C Set representative casual gain then search all zones and build
C hash list of zone:time:occupancy.
        write(ll1,'(a)') lodlabel(IZONE,1)
        write(ll2,'(a)') lodlabel(IZONE,2)
        write(ll3,'(a)') lodlabel(IZONE,3)
        helptopic='res_comfort_occupied'
        call gethelptext(helpinsub,helptopic,nbhelp)
 936    IOT=1
        CALL EASKATOG(' ','Which casual gain represents occupancy?',
     &      'Always occupied',ll1,ll2,ll3,'time',' ',' ',IOT,nbhelp)
        if (IOT.gt.1.and.IOT.lt.5) then
          if (izver.lt.4) then
            write(outs,'(2a)')'Occupancy filtering not available for ',
     &       'legacy results files before version 4.'
            call usrmsg(outs,
     &   'Please regenerate results file or use time filtering.','W')
            goto 936
          else
            iocupf=1      
            iocut=IOT-1
          endif
        elseif(IOT.eq.1)then
          iocut=0
        elseif(IOT.eq.5)then
          iocut=-1
          write(HOLD,'(a)') '  0  24  '
          CALL EASKS(HOLD,' ','Weekday occupancy period?',
     &      24,' 0  24 ','wkd occup period',IER,nbhelp)
          K=0
          CALL EGETWI(HOLD,K,iwkdst,0,24,'W','iwkdst',IER)
          CALL EGETWI(HOLD,K,iwkdfn,iwkdst,24,'W','iwkdst',IER)

          write(HOLD,'(a)') '  0  24  '
          CALL EASKS(HOLD,' ','First weekend day occupancy period?',
     &     24,' 0  24 ','sat occup period',IER,nbhelp)
          K=0
          CALL EGETWI(HOLD,K,isatst,0,24,'W','isatst',IER)
          CALL EGETWI(HOLD,K,isatfn,isatst,24,'W','isatst',IER)

          write(HOLD,'(a)') '  0  24  '
          CALL EASKS(HOLD,' ','Second weekend day occupancy period?',
     &     24,' 0  24 ','sat occup period',IER,nbhelp)
          K=0
          CALL EGETWI(HOLD,K,isunst,0,24,'W','isunst',IER)
          CALL EGETWI(HOLD,K,isunfn,isunst,24,'W','isunst',IER)
        endif
        iocuset=1
        IFILT=iocut

        if (IER.ne.0) then
          call edisp(iuout,' ')
          call edisp(iuout,'Error setting up comfort parameters!')
        endif
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
      WRITE(ITEMS(1),'(A,A)')      'a zone: ',zname(IZONE)
      WRITE(ITEMS(2),'(A,2I3)')    'b day & month :',IDCO,IMCO
      IF(OP(1).lt.-0.5)THEN
        ITEMS(3)=                  'c MRT >> default        '
      ELSEIF(OP(1).gt.-0.5)THEN
        ITEMS(3)=                  'c                       '
      ENDIF
      ITEMS(4)   =                 '  _____________________ '
      WRITE(ITEMS(5),'(A,F5.2)')   'd CLO value     : ',CLO
      WRITE(ITEMS(6),'(A,F5.1)')   'e activity level: ',ACTL
      WRITE(ITEMS(7),'(A,F5.1)')   'f air velocity  : ',WVEL
      if(iocut.eq.0)then
        ITEMS(8) =                 'g occupancy: always     '
      elseif(iocut.eq.-1)then      
        ITEMS(8) =                 'g occupancy: time based '
      else
        WRITE(ITEMS(8),'(A,I1)')   'g occupancy: ',iocut
      endif
      ITEMS(9)  =                  '  _____________________ '
      ITEMS(10) =                  '1 assess average comfort'
      ITEMS(11) =                  '2 assess local comfort  '
      ITEMS(13)  =                 '? help                  '
      ITEMS(14)  =                 '- exit menu'
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

      if(mmod.eq.8)then
        INO=-1
        CALL EMENU('Comfort assessment',ITEMS,NITMS,INO)
      else
        INO=-2
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

C Change zone, set to 25 characters wide.
        call askzon(IZONE,25)
      elseif(ino.eq.2)then

C Find the day to analyse.
        helptopic='res_comfort_day'
        call gethelptext(helpinsub,helptopic,nbhelp)
        write(TSTR,'(3X,I3,I4)',IOSTAT=IOS,ERR=1)IDCO,IMCO
    8   CALL EASKS(TSTR,' ','Day and month?',
     &      24,'1 1 ','day & month no',IER,1)
        K=0
        CALL EGETWI(TSTR,K,IDCO,1,31,'W','day',IER)
        CALL EGETWI(TSTR,K,IMCO,1,12,'W','month',IER)
        if(ier.ne.0)goto 8

        IF(IDCO.LT.1.OR.IDCO.GT.ID(IMCO))GOTO 8
        CALL EDAY(IDCO,IMCO,IDAY)
        IF(IDAY.LT.ISDS)THEN
          call edisp(iuout,' ')
          call edisp(iuout,'Before simulation, try again!')
          goto 8
        ENDIF
        IF(IDAY.GT.ISDF)then
          call edisp(iuout,' ')
          call edisp(iuout,'After simulation, try again!')
          goto 8
        ENDIF
      elseif(ino.eq.3)then
          OP(1)=-1.
          goto 3
      elseif(ino.eq.5)then

C Clothing level.
        helptopic='res_clo_review'
        call gethelptext(helpinsub,helptopic,nbhelp)
        call easkr(CLO,' ','Clothing level?',
     &    0.0,'W',3.0,'W',1.0,'clothing level',IER,nbhelp)
      elseif(ino.eq.6)then

C Activity level, may be entered in MET units or in W/m^2.
        helptopic='res_MET_review'
        call gethelptext(helpinsub,helptopic,nbhelp)
        CALL EASKAB(' ','Activity level units?',
     &    'MET units','W/m^2 (body surface area)',IAU,nbhelp)
        if(IAU.eq.1)then
          call easkr(ACTL,' ','Activity level?',
     &      0.859,'W',6.013,'W',50.0,'activity level MET',IER,nbhelp)
          ACTL = ACTL * 58.2
        elseif(IAU.eq.2)then
          call easkr(ACTL,' ','Activity level?',
     &      50.0,'F',350.0,'F',85.0,'activity level W/m2',IER,nbhelp)
        endif
      elseif(ino.eq.7)then

C Air speed.
        helptopic='res_comfort_air_velo'
        call gethelptext(helpinsub,helptopic,nbhelp)
        call easkr(WVEL,' ','Default air velocity?',
     &    0.0,'W',5.0,'W',0.15,'air velocity',IER,nbhelp)
      elseif(ino.eq.8)then

C Casual gain type.  Ask user which constitutes occupancy.
        call edisp(iuout,' ')
        write(ll1,'(a)') lodlabel(IZONE,1)
        write(ll2,'(a)') lodlabel(IZONE,2)
        write(ll3,'(a)') lodlabel(IZONE,3)
        helptopic='res_comfort_occupied'
        call gethelptext(helpinsub,helptopic,nbhelp)
 937    IOT=1
        CALL EASKATOG(' ','Which casual gain represents occupancy?',
     &      'Always occupied',ll1,ll2,ll3,'time',' ',' ',IOT,nbhelp)
        if (IOT.gt.1.and.IOT.lt.5) then
          if (izver.lt.4) then
            write(outs,'(2a)')'Occupancy filtering not available',
     &       'for legacy results files before version 4.'
            call usrmsg(outs,
     &   'Please regenerate results file or use time filtering.','W')
            goto 937
          else      
            iocut=IOT-1
          endif
        elseif(IOT.eq.1)then
          iocut=0
        elseif(IOT.eq.5)then
          iocut=-1
          write(HOLD,'(a)') '  0  24  '
          CALL EASKS(HOLD,' ','Weekday occupancy period?',
     &      24,' 0  24 ','wkd occup period',IER,nbhelp)
          K=0
          CALL EGETWI(HOLD,K,iwkdst,0,24,'W','iwkdst',IER)
          CALL EGETWI(HOLD,K,iwkdfn,iwkdst,24,'W','iwkdst',IER)

          write(HOLD,'(a)') '  0  24  '
          CALL EASKS(HOLD,' ','First weekend day occupancy period?',
     &     24,' 0  24 ','sat occup period',IER,nbhelp)
          K=0
          CALL EGETWI(HOLD,K,isatst,0,24,'W','isatst',IER)
          CALL EGETWI(HOLD,K,isatfn,isatst,24,'W','isatst',IER)

          write(HOLD,'(a)') '  0  24  '
          CALL EASKS(HOLD,' ','Second weekend day occupancy period?',
     &     24,' 0  24 ','sat occup period',IER,nbhelp)
          K=0
          CALL EGETWI(HOLD,K,isunst,0,24,'W','isunst',IER)
          CALL EGETWI(HOLD,K,isunfn,isunst,24,'W','isunst',IER)
        endif
        call usrmsg(' ',' ','-')
        IFILT=iocut
      elseif(ino.eq.10)then
        goto 78
      elseif(ino.eq.11)then

C Start espvwf session.
        helptopic='res_comfort_local_vwf'
        call gethelptext(helpinsub,helptopic,nbhelp)
        CALL EASKOK(' ','Proceed with view factor analysis?',OK,nbhelp)
        IF(OK)then

C Get logical name of terminal type, expand problem name
C to include the path and create a string to drive ish.
          write(zn,'(A)') zname(IZONE)
          call tstamp('>','RES: start mft')
          doit = ' '
          call terminalmode(childterminal,tmode)
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
        call edisp(itru,' ')
        call edisp(itru,
     &   'PMV is Predicted Mean Vote. PMV* is PMV based on ')
        call edisp(itru,
     &   'effective rather than operative temperature.')
        write(outs,'(A,A,A,I3,A,I3)',IOSTAT=IOS,ERR=1)
     &    'Comfort assessment for ',
     &    zname(IZONE)(1:lnzname(IZONE)),' on day',IDCO,
     &    ' of month',IMCO
        call edisp(itru,' ')
        call edisp(itru,outs)
        write(outs,172,IOSTAT=IOS,ERR=1)ACTL,CLO,WVEL
  172   FORMAT('Activity level',F6.2,', Clothing level',F6.2,
     &         ', Default air speed',F6.2)
        call edisp(itru,' ')
        call edisp(itru,outs)
        call edisp(itru,' Default mean radiant temperature')
        write(outs,103)
  103   FORMAT('Time   t-air   t-mrt  rel.h   SET   PMV*   PMV   PPD',
     &         '   Comfort assessment')
        call edisp(itru,' ')
        call edisp(itru,outs)
        write(outs,1103)
 1103   FORMAT('(hrs) (deg.C) (deg.C)  (%)  (deg.C) (-)    (-)   (%)',
     &         '      based on PMV')
        call edisp(itru,' ')
        call edisp(itru,outs)
      endif

      ok=.false.
      if (IMRT.ne.0) then

C Put sensor MRT in GVAL.
        ISET=ISIM
        call SENMRT(IDAY,IZONE,IMRT,ISET,0)

C If there is a CFD domain for this zone, use air velocity and
C temperature from the grid point closest to the MRT sensor location.
        if (IFCFD(IZONE).gt.0) then

          call OPCFD('c',ok,IER)
          if (.not.ok.or.IER.ne.0) goto 924
      
C Read header data and set common block data.
          call RCFDLIBH(IZONE,IER)

C If old CFD results version, read CFD file to get axis vertices to
C initialise CFD - building domain transformation matrices.
          if (ICFDSV.eq.1.or.ICFDSV.eq.2) then
            CALL RCFDVRTS(IZONE,IER)
          endif
          CALL INIT_CFDTRANS(IER)
          if (IER.ne.0) then
            call edisp(iuout,' ')
            call edisp(iuout,'Problem reading CFD axis vertices!')
          else

C Find CFD point closest to MRT sensor.              
            CALL GRIDGEO
            PI = 4.0 * ATAN(1.0)
            R=PI/180.
            SA=SIN(CANG(IMRT)*R)
            CA=COS(CANG(IMRT)*R)
            DX=(DXC(IMRT)/2.)*CA-(DYC(IMRT)/2.)*SA
            DY=(DXC(IMRT)/2.)*SA+(DYC(IMRT)/2.)*CA
            CALL CFDTRANS(2,XOC(IMRT)+DX,YOC(IMRT)+DY,
     &                      ZOC(IMRT)+DZC(IMRT)/2,XC,YC,ZC,IER)
            CALL FDCFDPT(1,XC,YC,ZC,IIX,IIY,IIZ,IER)
            if (IER.ne.0) then
              call edisp(iuout,' ')
              call edisp(iuout,' Problem finding CFD point!')
            else
              IXCEL(IZONE)=IIX-1
              IYCEL(IZONE)=IIY-1
              IZCEL(IZONE)=IIZ-1
              ok=.true.
            endif
          endif      
c          write(outs,'(a,i1,a,i2,a)')
c     &      ' CFD results at location of MRT sensor ',IMRT,
c     &      ' in zone ',IZONE,' will be used for PMV calculations.'
c          call edisp(iuout,outs)
          goto 925
 924      write(outs,'(a,a,i1,a,i2,a)')'Error setting up CFD results',
     &      ' for PMV calculations at MRT sensor ',IMRT,' in zone ',
     &      IZONE,', CFD will not be used.'
          call edisp(iuout,' ')
          call edisp(iuout,outs)
        endif
      endif
      
C For each timestep in the day get the QW values used by MOMNRD.
 925  N=24*NTS
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

        WWVEL=WVEL
        if (IMRT.ne.0) then
          TMRT=GVAL(JTS)

          if (ok) then

C Check if there are CFD results available for this time step.
            CALL GETIFRM(IDAY,JTS,IFRAME,IER)

C Set up directives for GOCFDGET and retrieve velocity and temperature.
            CALL MOFLOWSU() ! clear commons
            NMFGET=2
            IMFGETNO(1,1)=18
            IMFGETNO(1,3)=ICFD
            IMFGETNO(1,4)=1
            IMFGETNO(2,1)=19
            IMFGETNO(2,3)=ICFD
            IMFGETNO(2,4)=1
            IMFLIST(1,1)=IXCEL(IZONE)
            IMFLIST(1,2)=IYCEL(IZONE)
            IMFLIST(1,3)=IZCEL(IZONE)
            CALL GOCFDGET(IFRAME,0,0,0,IER)
            if (IER.ne.0) IER=0; goto 185

C Get CFD air temperature at point closest to MRT sensor.
            WWVEL=FLOWVALS(1)
            TAIR=FLOWVALS(2)
          endif
        endif

C Current time.
 185    call DATIME(JTS,TIME)
        RHX=RH/100.

C This is the main calculation routine. It is passed the
C current air temperature (TAIR), the mean radiant temperature (TMRT)
C and the user's values for wind velocity (WVEL) and clothing level (CLO).
C It returns an index of comfort (ICMFRT) and three variants
C of comfort (from the LET* comfort routines implemented by Cor Pernot,
C FAGO-TNO.
        CALL LETDRIVR(TAIR,TMRT,WWVEL,RHX,ACTL,CLO,
     &                SET,ICMFRT,PME,PMV,XPPD)

C If occupancy is assumed to be equivalent to a non-zero value
C of one of the casual gain types test here.
        ih=int(TIME+1.)
        call getocup(izone,iday,JTS,ioc,ier)
        if(ioc.eq.0)ICMFRT=10

C Verbal category only printed if activity level is sedentary.
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
        helptopic='res_comfort_next_day'
        call gethelptext(helpinsub,helptopic,nbhelp)
        CALL EASKAB(' ','Options:',
     &    'continue with next day','return to menu',IWW,nbhelp)
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
        if(unixok)write(iuout,*)
     &    'MOCMFT: no permission to write string!'
      else
        if(unixok)write(iuout,*)
     &    'MOCMFT: internal string handling error!'
      endif
      END

C ********** getocup
C Look up occupancy patterns and returns yes or no (1 or 0) in ioc for
C a given zone/day/time step.

      subroutine getocup(izone,iday,istep,ioc,ier)
#include "building.h"
#include "net_flow.h"
#include "tdf2.h"

      common/recver/izver,ipver,iever
      COMMON/SET1/IYEAR,IBDOY,IEDOY,IFDAY,IFTIME
      COMMON/SIMPIK/ISIM,ISTADD,ID1,IM1,ID2,IM2,ISDS,ISDF,NTS,ISAVE
      COMMON/OUTIN/IUOUT,IUIN,IEOUT
      
      integer ncomp,ncon
      common/c1/ncomp,ncon
      common/cmftpar/WVEL,ACTL,CLO,iocut,iocuset
      common/timfltr/iwkdst,iwkdfn,isatst,isatfn,isunst,isunfn

      character outs*124
      logical found,close15,close16,close17
      real QCASR,QCASC,QCASL  ! total radiant/convective/latent
      real FRAC               ! controlled fraction.
      real perocupc,perocupr,perocupl    ! average occupant to write out
      real perlightc,perlightr,perlightl ! average lighting to write out
      real perequipc,perequipr,perequipl ! average equipment to write out
      real otherc,otherr,otherl ! average other (future expansion) to write out
      integer theonectld        ! if non-zero the casual gain type that is controlled.

      real ftime

      NDTS=24*NTS
      ISET=ISIM

C Range testing.
      if(izone.lt.1.or.izone.gt.ncomp.or.istep.gt.NDTS)then
        write (outs,'(3(a,i3),a,a)') 
     &      'zone(',izone,'), day(',iday,'), time step(',istep,').  ',
     &      'Assuming zone is occupied.'
        call usrmsg(
     &      'getocup: out of range zone/day/step index in:',outs,'W')
        ioc=1
        ier=1
        return
      endif

C If assumption of occupied all hours then return 1.
      if(iocut.eq.0)then
        ioc=1

C Filtered by user-defined times stored in common "timfltr".
      elseif(iocut.eq.-1)then
        ftime=istep/NTS
        CALL EDAYR(iday,IDAYN,IMTHN)
        CALL EWEEKD(IDAYN,IMTHN,IYEAR,IDWK)
        if(IDWK.eq.6)then
          ist=isatst
          ifn=isatfn
        elseif(IDWK.eq.7)then
          ist=isunst
          ifn=isunfn
        else
          ist=iwkdst
          ifn=iwkdfn
        endif

        if(ftime.gt.ist.and.ftime.le.ifn)then
          ioc=1
        else
          ioc=0
        endif

C Filtered by occupancy, associated with one casual gain type.
C NOTE: this functionality is unavailable for results files below
C version 4.
      else
        if(izver.ge.4)then
          call getallcas(IDAY,IZONE,ISET,ISTEP,QCASR,QCASC,QCASL,FRAC,
     &      perocupc,perlightc,perequipc,otherc,perocupr,perlightr,
     &      perequipr,otherr,perocupl,perlightl,perequipl,otherl,
     &      theonectld)

          ioc=0
          if(iocut.eq.1)then
            call eclose(perocupc,0.0,0.1,close15)
            call eclose(perocupr,0.0,0.1,close16)
            call eclose(perocupl,0.0,0.1,close17)
          elseif(iocut.eq.2)then
            call eclose(perlightc,0.0,0.1,close15)
            call eclose(perlightr,0.0,0.1,close16)
            call eclose(perlightl,0.0,0.1,close17)
          elseif(iocut.eq.3)then
            call eclose(perequipc,0.0,0.1,close15)
            call eclose(perequipr,0.0,0.1,close16)
            call eclose(perequipl,0.0,0.1,close17)
          endif
          if(.not.close15) ioc=1
          if(.not.close16) ioc=1
          if(.not.close17) ioc=1
        else
          write(outs,'(2a)')"getocup: occupancy filtering not ",
     &      "available for results libraries before version 4."
          call edisp(iuout,' ')
          call edisp(iuout,outs)
          write(outs,'(2a)')
     &      "Please regenerate results file or use time filtering.",
     &      "Assuming zone is occupied."
          call edisp(iuout,outs)
          ioc=1
          ier=1
        endif
      endif

      return
      end

C ********** LOCOMF
C Calculates local discomfort.

      SUBROUTINE LOCOMF(IDAY,IZONE,ICP,IMP,ISET)
#include "building.h"

      COMMON/OUTIN/IUOUT,IUIN,IEOUT
      COMMON/SIMPIK/ISIM,ISTADD,ID1,IM1,ID2,IM2,ISDS,ISDF,NTS,ISAVE
      COMMON/GET2/XDUM(MTS),XDUM1(MTS),GVAL(MTS)

      DIMENSION TPVAL(MTS)

C Recover temperature values. 
      if (ICP.eq.4) then

C Need dT between CFD cells. set to zero and warn user.
        call edisp(iuout,' ')
        call edisp(iuout,
     &    'Missing information on local air temperature,')
        call edisp(iuout,'setting dT to zero.')
      elseif (ICP.eq.5) then

C Only need floor surface temperature.
        CALL GTMS(IDAY,IZONE,IMP,ISET)
      endif

C Store in a temporary array.
      N=24*NTS
      DO 20 I=1,N
        TPVAL(I)=GVAL(I)
  20  continue

C Get the latent loading.
      CALL GZLL(IDAY,IZONE,ISET)       

C Output the sum of the two parameters. 
      DO 30 I=1,N
        GVAL(I)=GVAL(I)+TPVAL(I)
  30  continue

      RETURN
      END

C ********** DISCOMF
C Local thermal discomfort and indoor air quality assessment 
C according to Standard prENV 1752. General thermal comfort is 
C treated according to Standards prENV 1752 and ISO EN 7730. 
C PPD is the output in %. The input is the XX-value, which is
C case dependent. If iflag is returned as -1 then we are outside
C the bounds of the algorithm.

C 2017: Updated to use equations from BS EN ISO 7730:2005.
C Previous equations left commented, and metrics with no
C equivalent in BS EN ISO 7730:2005 noted as such.
C Note that draught is handled by a seperate subroutine as
C it requires more than one input to calculate.

      SUBROUTINE DISCOMF(iflag,XX,PPD)

C Initially assume no dissatisfaction.
      PPD=0.0 

C Local thermal discomfort due to vertical air temperature difference 
C between head (@1.1m) and feet(@0.1m). Input XX is temperature difference. 
      if(iflag.eq.1) then 
c        if (XX.lt.1.0.or.XX.gt.8.0) then
c          iflag=-1
c        else
c          PPD=0.7038+0.2974*XX**2.7810-0.084*exp(XX)
c        endif
        if (XX.gt.8.0) then
          iflag=-1
        elseif (XX.lt.0.0) then
          iflag=-2
        else
          PPD=100/(1+EXP(5.76-0.856*XX))
        endif

C Local thermal discomfort due to warm or cool floors.   
C Input XX is floor temperature. 
      elseif(iflag.eq.2) then 
c        if (XX.lt.5.0.or.XX.gt.40.0) then
c          iflag=-1
c        else
c          PPD=59.5022 - (74.6871*XX) + (16.4158*LOG(XX)*(XX+9.3362)) 
c        endif
        PPD=100-94*EXP(-1.387+0.118*XX-0.0025*XX**2)

C Local thermal discomfort due to radiant temperature asymmetry - warm ceiling.   
C Input XX is radiant temperature assymetry.
      elseif(iflag.eq.3) then 
        XX=abs(XX)
c        if (XX.gt.25.0) then
c          iflag=-1
c        elseif (XX.lt.1.0) then
c          PPD=0.5
c        else
c          PPD=-0.9525 + 0.1865*exp(2.0755*XX**0.35)  
c        endif
        if (XX.gt.23.0) then
          iflag=-1
        else
          PPD=100/(1+EXP(2.84-0.174*XX))-5.5
        endif

C Local thermal discomfort due to radiant temperature asymmetry - warm wall.   
C Input XX is radiant temperature assymetry.
      elseif(iflag.eq.4) then 
        XX=abs(XX)
c        if (XX.gt.30.0) then
c          iflag=-1
c        elseif (XX.lt.7.5) then
c          PPD=1.0
c        else
c          PPD=-0.1112 + 0.0539*exp(1.4686*XX**0.36)
c        endif
        if (XX.gt.35) then
          iflag=-1
        else
          PPD=100/(1+EXP(3.72-0.052*XX))-3.5
        endif

C Local thermal discomfort due to radiant temperature asymmetry - cool ceiling.   
C Input XX is radiant temperature assymetry.
      elseif(iflag.eq.5) then 
        XX=abs(XX)
c        if (XX.gt.16.0) then
c          iflag=-1
c        elseif (XX.lt.7.5) then
c          PPD=1.0
c        else
c          PPD=-0.1056+0.0163*exp(1.5847*XX**0.49)
c        endif
        if (XX.gt.15) then
          iflag=-1
        else
          PPD=100/(1+EXP(9.93-0.5*XX))
        endif

C Local thermal discomfort due to radiant temperature asymmetry - cool wall.   
C Input XX is radiant temperature assymetry.
      elseif(iflag.eq.6) then 
        XX=abs(XX)
c        if (XX.gt.16.0) then
c          iflag=-1
c        elseif (XX.lt.6.0) then
c          PPD=0.4
c        else
c          PPD=-1.2568+0.0189*exp(1.9469*XX**0.47)
c        endif
        if (XX.gt.15) then
          iflag=-1
        else
          PPD=100/(1+EXP(6.61-0.345*XX))
        endif

C Dissatisfied due to ventilation rate. 
C Input XX is ventilation rate given (l/s) 
      elseif(iflag.eq.7) then 
        if(XX.ge.0.32) then
          PPD=395.0*exp(-1.83*XX**0.25) ! No equivalent in BS EN ISO 7730:2005
        else
          PPD=100.0
        endif

C Dissatisfied due to CO2 above outdoors. 
C Input CO2 concentration as calculated by conc.F (kg/kg).
C The CO2 value needed to interpolate the graph in the standard
C is in ppmv.
      elseif(iflag.eq.8) then 

C Convert kg/kg to mg/m^3 using density at 20C: 
        XX=XX*1.12*1000000.0 

C Now convert mg/m^3 to ppmv. 
        XX=XX*24.45/44.0  
        PPD=395.0*exp(-15.15*XX**(-0.25)) ! No equivalent in BS EN ISO 7730:2005

C Dissatisfied calculated from PMV.
C Input is the PMV range [-3 -- +3]. 
      elseif(iflag.eq.9) then 
        PPD=100.0-95.0*exp(XX*XX*((-0.03353*XX*XX)-0.2179)) 
      endif

C Check for a reasonable result.
      if(PPD.gt.100.0) PPD=100.0
      if(PPD.lt.0.0) PPD=0.0 

      return
      end 

C ********** DRAUGHT
C Local thermal discomfort due to draught (from BS EN ISO 7730:2005).
C Useful only if k-epsilon turbulence model is active from which 
C the turbulence intensity is available.

      SUBROUTINE DRAUGHT(IDAY,ITS,IZONE,IMRT,iflag,PPD,IER)
#include "building.h"
#include "model.h"
#include "geometry.h"
#include "cfd.h"

      common/OUTIN/IUOUT,IUIN,IEOUT
      common/FILEP/IFIL
      common/rpath/path
      character path*72,LCMDFL*144,ltmpc*144,LTMP*72,lpath*72,outs*128

      common/VARf/Uf(ntcelx,ntcely,ntcelz),Vf(ntcelx,ntcely,ntcelz),
     1            Wf(ntcelx,ntcely,ntcelz),
     2            P(ntcelx,ntcely,ntcelz),PP(ntcelx,ntcely,ntcelz),
     3            TEf(ntcelx,ntcely,ntcelz),EDf(ntcelx,ntcely,ntcelz)
      common/TEMPf/Tf(ntcelx,ntcely,ntcelz),GAMH(ntcelx,ntcely,ntcelz),
     1             RESORT,NSWPT,URFT,FSDTT,PRANDL,PFUN
      common/CFDSV/IRECPC,ICFDSV,IEQSV(5+MCTM)
      common/cfddoms/NCDOM,ICFDZ(MNZ)

      real PI,R,SA,CA,DX,DY,HI(3),XC,YC,ZC
      integer IHI(3)
      character LMRT*72

C Read CFD header data and set common block data.
      lpath=path
      call fdroot(LCMDFL,path,LTMP)
      call RCFDLIBH(IZONE,IER)
      path=lpath

C If old CFD results version, read CFD file to get axis vertices to
C initialise CFD to building domain transformation matrices.
      if (ICFDSV.eq.1.or.ICFDSV.eq.2) then
        CALL RCFDVRTS(IZONE,IER)
      endif
      CALL INIT_CFDTRANS(IER)

C Equation is for draught at the neck, so use a point in the centre of
C the top face of the MRT sensor.
      IUF=IFIL+1
      LMRT=LVIEW(IZONE)
      call ERMRT(0,IUOUT,IUF,LMRT,IZONE,IER)      
      PI = 4.0 * ATAN(1.0)
      R=PI/180.
      SA=SIN(CANG(IMRT)*R)
      CA=COS(CANG(IMRT)*R)
      DX=(DXC(IMRT)/2.)*CA-(DYC(IMRT)/2.)*SA
      DY=(DXC(IMRT)/2.)*SA+(DYC(IMRT)/2.)*CA
      HI(1)=XOC(IMRT)+DX
      HI(2)=YOC(IMRT)+DY
      HI(3)=ZOC(IMRT)+DZC(IMRT)    

      CALL GRIDGEO
      CALL CFDTRANS(2,HI(1),HI(2),HI(3),XC,YC,ZC,IER)
      CALL FDCFDPT(1,XC,YC,ZC,IHI(1),IHI(2),IHI(3),IER)

      if (IER.ne.0) then
        write(outs,'(a)')
     &    'DRAUGHT: Could not retrieve air flow variables.'
        call edisp(iuout,outs)
        goto 999
      endif

C Check if there are CFD results available for this time step.
      CALL GETIFRM(IDAY,ITS,IFRAME,IER)
      if (IER.ne.0) then
        IER=0
        iflag=-2
        goto 999
      endif
  
      CALL RDCFDAT(IFRAME)

      Tair=Tf(IHI(1),IHI(2),IHI(3))
      Cvel=sqrt(Uf(IHI(1),IHI(2),IHI(3))**2+Vf(IHI(1),IHI(2),IHI(3))**2+
     &          Wf(IHI(1),IHI(2),IHI(3))**2)
c      Tenergy=TEf(IHI(1),IHI(2),IHI(3))

      if (Tair.lt.20.0.or.Tair.gt.26.0) then
        iflag=-1
        goto 999
      endif
      if (Cvel.gt.0.5) then
        iflag=-1
        goto 999
      endif

c      Tu=sqrt((2.0/3.0)*Tenergy)/(Cvel+1.0e-30)*100.0
c                                  ^^^^^^^^^^^^
c                                  What is this?
c      Tu=sqrt((2.0/3.0)*Tenergy)*100.0
C Use value of Tu specified in standard.
      Tu=40.0
      if (Tu.lt.10.0.or.Tu.gt.60.0) then
        iflag=-1
        goto 999
      endif

      if (Cvel.lt.0.05) Cvel=0.05

      PPD=(34.0-Tair)*(Cvel-0.05)**0.62*(0.37*Cvel*Tu+3.14)
 
C Check for a reasonable result.
      if(PPD.gt.100.0) PPD=100.0
      if(PPD.lt.0.0) PPD=0.0 
        
      iflag=1
 999  return

      end

C **********  OFFSETTEMP
C Calculate the air speed required to offsetincreased temperature
C according to ANSI/ASHRAE 55-1992. Applied only for summer conditions.
C Here Cadd is the output, which is the additional air speed required to 
C offset the increased temperature. Maximum allowed is 0.8 m/s. 
C Input: Temperature rise (maximum 3C) above an allowed effective 
C temperature of 26C.

      SUBROUTINE OFFSETTEMP(Trise,Cadd)

      Cadd=-0.3252+0.5337*exp(0.2006*Trise**1.4) 

      return
      end

C ********** ASHRAEZONE
C Comfort range checked by ET* index according to 
C ANSI/ASHRAE 55-1992. Returns the effective temperature
C ET* and a logical 'inside', which governs
C whether we are or are not within the comfort zone.  
C Needs input specifying wheter we are in summer or 
C winter (logical 'summer'), Tair and relative humidity.  

      SUBROUTINE ASHRAEZONE(Tair,RH,summer,ETst,inside)

      LOGICAL summer,inside 

C Define the saturation pressure, Psat, for the given temperature Tair. 
      TSAT=Tair 
      PSAT=611.0*exp(-1.91275E-04+7.258E-02*TSAT-2.939E-04 
     &       *TSAT**2+9.841E-07*TSAT**3-1.92E-09*TSAT**4) 

C Calculate the ambient water vapour pressure pa (Pa).
      pa=PSAT*RH/100.0 

C Calculate the ET* temperature.  
      ETst=0.492*Tair+0.0019*pa+6.47 

C Check the comfort range for summer and winter.
      inside=.false. 
      if(summer) then 
        if(ETst.gt.23.0.and.ETst.lt.26.0) inside=.true. 
      else
        if(ETst.gt.20.0.and.ETst.lt.23.5) inside=.true. 
      endif

      return
      end 

C ********** RCFDVRTS
C Reads a dfd file and assigns common variables:
C   iorg  - origin vertex
C   ixend - x axis end vertex
C   iyend - y axis end vertex
C   izend - z axis end vertex

      SUBROUTINE RCFDVRTS(IZONE,IER)
#include "building.h"
#include "cfd.h"

      COMMON/OUTIN/IUOUT,IUIN,IEOUT
      COMMON/FILEP/IFIL
      COMMON/cfdfil/LCFD(MCOM),IFCFD(MCOM)      
      CHARACTER LCFD*72

      common/GRDVRTS/iorg(MNZ),ixend(MNZ),iyend(MNZ),izend(MNZ),
     &  izende(MNZ)
      COMMON/cfddoms/NCDOM,ICFDZ(MNZ)
      COMMON/ICFNOD/ICFD,ICP

      CHARACTER OUTSTR*124,WORD*20
      
      IUNIT=IFIL+1
      CALL ERPFREE(IUNIT,IER)
      CALL EFOPSEQ(IUNIT,LCFD(IZONE),0,IER)
      if (IER.ne.0) then
        call edisp(iuout,' ')
        call edisp(iuout,'RCFDVRTS: cannot open file!')
        return
      endif
      
      do while (.true.)
        CALL STRIPC(IUNIT,OUTSTR,0,ND,0,'DFD line',IER)
        if (IER.ne.0) then
          call edisp(iuout,' ')
          call edisp(iuout,'RCFDVRTS: error reading line!')
          exit
        endif
        K=0
        CALL EGETW(OUTSTR,K,WORD,'W','DFD line 1st word',IER)
        if (WORD(1:5).eq.'*vrts') then
          CALL EGETWI(OUTSTR,K,itmp,0,0,'-','origin vert',IER)
          IORG(ICFD)=itmp
          CALL EGETWI(OUTSTR,K,itmp,0,0,'-','x end vert',IER)
          ixend(ICFD)=itmp
          CALL EGETWI(OUTSTR,K,itmp,0,0,'-','y end vert',IER)
          iyend(ICFD)=itmp
          CALL EGETWI(OUTSTR,K,itmp,0,0,'-','z end vert',IER)
          izend(ICFD)=itmp
          exit
        endif
      enddo

      return
      end

C ********** VERTdT
C Finds the vertical air temperature difference between the top
C of an MRT sensor and floor level. Uses CFD results for the same zone
C as the MRT sensor. Gets results for 1 day and puts these in GVAL.

      SUBROUTINE VERTdT(IDAY,IZONE,IMRT,IER)
#include "building.h"
#include "model.h"
#include "geometry.h"
#include "cfd.h"

      COMMON/OUTIN/IUOUT,IUIN,IEOUT
      COMMON/FILEP/IFIL
      common/rpath/path
      character path*72,LCMDFL*144,ltmpc*144,LTMP*72,lpath*72,outs*128
      COMMON/SIMPIK/ISIM,ISTADD,ID1,IM1,ID2,IM2,ISDS,ISDF,NTS,ISAVE
      COMMON/GET2/XDUM(MTS),XDUM1(MTS),GVAL(MTS)
      COMMON/TEMPf/Tf(ntcelx,ntcely,ntcelz),GAMH(ntcelx,ntcely,ntcelz),
     1             RESORT,NSWPT,URFT,FSDTT,PRANDL,PFUN
      common/CFDSV/IRECPC,ICFDSV,IEQSV(5+MCTM)
      COMMON/cfddoms/NCDOM,ICFDZ(MNZ)

      real PI,R,SA,CA,DX,DY,HI(3),XC,YC,ZC
      integer IHI(3),ILO(3)
      character LMRT*72

C Read CFD header data and set common block data.
      call RCFDLIBH(IZONE,IER)

C If old CFD results version, read CFD file to get axis vertices to
C initialise CFD to building domain transformation matrices.
      if (ICFDSV.eq.1.or.ICFDSV.eq.2) then
        CALL RCFDVRTS(IZONE,IER)
      endif
      CALL INIT_CFDTRANS(IER)

C Get coordinates in the center of top face of the MRT sensor.
      IUF=IFIL+1
      LMRT=LVIEW(IZONE)
      call ERMRT(0,IUOUT,IUF,LMRT,IZONE,IER)
      PI = 4.0 * ATAN(1.0)
      R=PI/180.
      SA=SIN(CANG(IMRT)*R)
      CA=COS(CANG(IMRT)*R)
      DX=(DXC(IMRT)/2.)*CA-(DYC(IMRT)/2.)*SA
      DY=(DXC(IMRT)/2.)*SA+(DYC(IMRT)/2.)*CA
      HI(1)=XOC(IMRT)+DX
      HI(2)=YOC(IMRT)+DY
      HI(3)=ZOC(IMRT)+DZC(IMRT)

C Find high and low CFD cells.
      CALL GRIDGEO
      CALL CFDTRANS(2,HI(1),HI(2),HI(3),XC,YC,ZC,IER)
      CALL FDCFDPT(1,XC,YC,ZC,IHI(1),IHI(2),IHI(3),IER)
      ILO(1)=IHI(1); ILO(2)=IHI(2); ILO(3)=2

      if (IER.ne.0) then
        write(outs,'(a)')
     &'VERTdT: Error finding head/foot vertical temperature difference!'
        call edisp(iuout,' ')
        call edisp(iuout,outs)
        goto 999
      endif

      if (IHI(1).eq.ILO(1).and.IHI(2).eq.ILO(2).and.IHI(3).eq.ILO(3))
     &  then
        write(outs,'(a,i2,a,i2,a)')
     &    'VERTdT: The top surface of MRT sensor ',IMRT,
     &    ' is too close to the floor in zone ',IZONE,'.'
        call edisp(iuout,' ')
        call edisp(iuout,outs)
        write(outs,'(2a)')          '        Either make the MRT',
     &    ' sensor bigger, or make the CFD grid finer.'
        CALL edisp(iuout,outs)
        IER=1
        goto 999
      endif

      N=24*NTS
      DO JTS=1,N

C Check if there are CFD results available for this time step.
        CALL GETIFRM(IDAY,JTS,IFRAME,IER)
        if (IER.ne.0) then
          IER=0
          goto 185
        endif
  
        CALL RDCFDAT(IFRAME)

C Get CFD air temperature at high cell.
        THI=Tf(IHI(1),IHI(2),IHI(3))

C Get CFD air temperature at low cell.
        TLO=Tf(ILO(1),ILO(2),ILO(3))
        GVAL(JTS)=abs(THI-TLO)

        cycle      
 185    GVAL(JTS)=-1.
      enddo

 999  RETURN
      END


