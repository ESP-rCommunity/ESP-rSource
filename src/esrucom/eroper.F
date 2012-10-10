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

C eroper.f provides the following facilities:
C  EROPER:   Reads all zone project data from a user-constructed
C            datafile.
C  ERZSCHED: Reads air and casual gain schedules from an operations
C            file outside the model.
C ZSCHEDHINT: Scans documentation in an operation file and reports 
C            so a user can decide if they want to use it.
C  EMKOPER:  Write zone operation common block data to file. 
C  VNTINFO:  English description of scheduled air flow and
C            control from zone operation common block data.
C  CASINFO:  English description of scheduled casual gains
C            from zone operation common block data.

C checkcascount scans current P3 & P3TYPE commons and refreshes 
C            loadcount() & load24() in common blocks loadcnt & loadall
C CPYCASIJ   Copy one casual period jx to another ix for zone icomp and 
C            daytype idaytype. 
C CPYCASIT   Copy one casual period jx to backup variables for zone icomp 
C            and daytype idaytype. 
C CPYCASTI   Copy from backup variables to casual period jx for zone icomp 
C            and daytype idaytype. 
C PROCESSOLDCAS: Processes gains in an old format operations file.
C PRECOMP    Calculates the apparent and reactive rectangular component
C            of a power comsuming load.
C checksort  Does a quick check of casual gains for sorted state.
C SORTCAS    Sort an array of casual gains by casual gain type and then by
C            starting time. (Uses a odified QUICKSORT).
C UPDOPR     Updates all operations data when day types are incremented

C ******************** EROPER 
C EROPER reads all zone project data from a user-constructed
C datafile.

C Common block variables are:

C ITCTL        - Thermostatic control index :
C                    control on zone coupled air temperature
C                    control on zone air temperature
C                  0 no control
C                    control on zone air temperature and infiltration
C                    control on zone coupled air and infiltration
C TLO, TUP,    - setpoint for low, upper and high
C THI
C ACIL, ACVL,  - low level (i.e. if below a setpoint) for control.
C IVL, TAL     
C ACIU, ACVU,  - mid level (i.e. if above a setpoint) 1st stage options
C IVU, TAU       for control.
C ACIH, ACVH,  - high level (2nd stage options) for control.
C IVH, TAH

C NAC          - number of distinct air change periods during day type
C                IDTY

C IACS,IACF    - start and finish hours of each of the above air change

C ACI          - natural infiltration air changes/hour for each period
C                relating to daytypes above respectively.

C ACV          - additional incoming air changes/hour for each period
C                relating to daytypes above respectively.

C IPT          - the additional incoming air (corresponding to ACV) 
C                can either be at constant temperature -
C                changing, if required, between each period - or set
C                at the time-dependent temperature of some coupled
C                zone. The IPT? variable controls this, where:
C                 IPT?=0 signifies a constant temperature will be specified
C                 IPT?=N (N>0) signifies that incoming air is at the
C                        time-dependent temperature of zone N.

C TA           - correspond to IPT?=0 and define the temperature of the
C                incoming air for each period above respectively.

C NCAS         - number of casual gains during each daytype

C ICGS,ICGF    - start and finish hours of each of the above casual gain
C                periods

C CMGS,CMGL,   - sensible and latent magnitude (in Watts) of
C                each casual gain

C RADC,CONC,   - radiant and convective portions (proportion
C                of 1) of each casual gain

C pf           - power factor of electrical load.
C ipf          - nature of load lagging (reactive), leading (capacative),
C                or unity (pure resistive).
C pwr          - real power consumption of the load (W).
C bvolt        - operational voltage of the load.
C iphas        - which phase the load is connected to (1-3) or
C                all 3 (4).

C Maximum number of air change periods/day MA=5
C Maximum number of casual gains/day       MC=20

      SUBROUTINE EROPER(ITRC,ITRU,IUO,ICOMP,IER)
      include "building.h"
      include "model.h"
      include "schedule.h"
      include "espriou.h"

C espriou.h provides currentfile.
      
      integer lnblnk  ! function definition

      common/caleni/nbdaytype,nbcaldays(MDTY),icalender(365)
      INTEGER NBDAYTYPE,NBCALDAYS,ICALENDER
      common/calena/calename,calentag(MDTY),calendayname(MDTY)
      CHARACTER CALENAME*32,CALENTAG*12,CALENDAYNAME*32
      COMMON/OUTIN/IUOUT,IUIN
      
      integer ncomp,ncon
      COMMON/C1/NCOMP,NCON

      COMMON/P2N/NAC(MDTY),IACS(MDTY,MA),IACF(MDTY,MA),ACI(MDTY,MA),
     &ACV(MDTY,MA),IPT(MDTY,MA),TA(MDTY,MA)
      INTEGER NAC,IACS,IACF,IPT
      REAL ACI,ACV,TA
      COMMON/P2CTL/ITCTL(MCOM),TLO(MCOM),TUP(MCOM),THI(MCOM),ACIL(MCOM),
     &             ACVL(MCOM),IVL(MCOM),TAL(MCOM),ACIU(MCOM),ACVU(MCOM),
     &             IVU(MCOM),TAU(MCOM),ACIH(MCOM),ACVH(MCOM),
     &             IVH(MCOM),TAH(MCOM)

      COMMON/P3N/NCAS(MDTY),ICGS(MDTY,MC),ICGF(MDTY,MC),
     &CMGS(MDTY,MC),CMGL(MDTY,MC),RADC(MDTY,MC),CONC(MDTY,MC)
      INTEGER NCAS,ICGS,ICGF
      REAL CMGS,CMGL,RADC,CONC

C Key word for each casual gain type (if ip3ver is 3). As follows:
C `-` no user defined meaning, `people` basic occupant, 
C 'lighting' basic lighting, `equipment` basic small power

C Bespoke types
C `net_utilities` same as icgtyp = 5, `ctlgain4` sams as icgtyp = 4,
C `cetc_appliances` NRCan appliances, `cetc_lights` NRC lights,
C `cetc_other` NRCan other electrical, `cetc_occupants` NRCan occupants
C `cetc_gas` NRCan gas appliances.

C Future type examples
C `pmv_people` people with clo and metabolic rate,
C `transpiraton` vegitation or latent sources, `waterdraw` DHW draws,
C `roomfan` future linked to flow component
      character caskeytype*16
      common/p3typekey/caskeytype(MDTY,MC)

C Common block P2UNIT defines the units for infiltration and ventilation
C for each zone and day type. The default value of zero is the same as
C the traditional (implied) unit in the version (ip3ver 0, 1, 2). A value
C of one signals m3/h and a value of 2 signals m3/s.
      integer INFUNIT,IVENTUNIT
      COMMON/P2UNIT/INFUNIT(MCOM,MDTY),IVENTUNIT(MCOM,MDTY)

C Electrical data flag and elctrical data.
      common/elecflg/ielf(mcom)
      COMMON/ELP3N/NEL(MDTY),PF(MDTY,MC),IPF(MDTY,MC),PWR(MDTY,MC),
     &BVOLT(MDTY,MC),IPHAS(MDTY,MC)
      INTEGER NEL,IPF,IPHAS
      REAL PF,PWR,BVOLT

C Error subroutine and error details for dll mode.
      common/dllerr/dllsubr,dllmesg

      CHARACTER outstr*124,outs*124,outs2*124
      character dllsubr*12,dllmesg*124,WORD*20,loutstr*248
      character dstmp*24
      logical havehi,dll
      integer idaytype  ! incremented for each new *day entry.
      integer ijk,ireader
      integer lsn  ! length of currentfile
      real reader
      integer irange,icur  ! for looping
      
      ijk=0
      ireader=0
      reader=0.0

C Check if running in dll mode.
      call isadll(dll)

C Initialise project data file.
      CALL EFOPSEQ(IUO,LPROJ(ICOMP),1,IER)
      IF(IER.NE.0)THEN
        write(outs,'(3a)') 'Operations file ',
     &    LPROJ(ICOMP)(1:lnblnk(LPROJ(ICOMP))),
     &    ' could not be opened.'
        if(dll)then
          dllsubr='EROPER'
          dllmesg=outs
          ier=2
          return
        else
          call edisp(iuout,outs)
          IER=1
          RETURN
        endif
      ENDIF
      write(currentfile,'(a)') LPROJ(ICOMP)(1:lnblnk(LPROJ(ICOMP)))
      ip3ver(icomp)=0   ! reset prior to detect

C Initialise the iphase arrays to avoid zero array elements in precal.
C Initialise the following arrays. This is important
C in case no electrical data was specified becasuse they
C are later used in precal.F as indexing arrays.
      do 700 idty=1,nbdaytype
        do 909 iph=1, mc
          iphas(idty,iph)=1
 909    continue
 700  continue

C Read lines from file, discarding comments. Look for header on
C newer files. Older files will begin with a description. To allow for
C description with spaces copy directly from OUTSTR rather than parsing
C it into words.
 
      CALL STRIPC(IUO,OUTSTR,0,ND,1,'oper name or header',IER)
      IF(IER.NE.0) goto 1002
      if(outstr(1:11).eq.'*Operations')then
        if(outstr(13:15).eq.'1.0')then
          ip3ver(icomp)=1
        elseif(outstr(13:15).eq.'2.0')then
          ip3ver(icomp)=2
        elseif(outstr(13:15).eq.'3.0')then
          ip3ver(icomp)=3
        endif

C Reset ip3ver if there are more than 3 day types.
        if(nbdaytype.gt.3)ip3ver(icomp)=2
        CALL STRIPC(IUO,OUTSTR,0,ND,1,'header',IER)
        IF(IER.NE.0) goto 1002
        K=0
        CALL EGETW(OUTSTR,K,WORD,'W','header tags',IER)
        IF(IER.NE.0) goto 1002
        if(WORD(1:5).eq.'*Date'.or.WORD(1:5).eq.'*date')then
          CALL EGETRM(OUTSTR,K,dstmp,'W','date stamp',IER)
        endif

C If ip3ver(icomp) is less than or equal to two then read the
C combined documentation (casual gains and air schedules) otherwise
C read a *air_schedules line and then the infiltration/ventilation
C documentation.

C << this logic may be preventing ventdesc from being correctly used >>

        if(ip3ver(icomp).le.2)then
          CALL LSTRIPC(IUO,LOUTSTR,0,ND,1,'oper notes',IER)
          oprdesc(icomp)=LOUTSTR
          ventdesc(icomp)=LOUTSTR
        elseif(ip3ver(icomp).eq.3)then
          CALL LSTRIPC(IUO,LOUTSTR,0,ND,1,'oper *air_schedules',IER)
          IF(IER.NE.0) goto 1002
          if(loutstr(1:14).eq.'*air_schedules')then
            CALL LSTRIPC(IUO,LOUTSTR,0,ND,1,'air sched notes',IER)
            ventdesc(icomp)=LOUTSTR
          else
            ventdesc(icomp)=LOUTSTR  ! set to same as oprdesc
          endif
        endif
      else

C We have a really old file which has documentation line first.
        oprdesc(icomp)=OUTSTR
        ventdesc(icomp)=OUTSTR
        ip3ver(icomp)=0
      endif

C If ip3ver(icomp) is less than or equal to two then set assumed
C units for air movement to ach. If ip3ver(icomp) is three then
C the pair of units needs to be established for each day type. 

C << put in a loop for each day type >> 
      if(ip3ver(icomp).le.2)then
        INFUNIT(icomp,1)=0
        INFUNIT(icomp,2)=0
        INFUNIT(icomp,3)=0
        IVENTUNIT(icomp,1)=0
        IVENTUNIT(icomp,2)=0
        IVENTUNIT(icomp,3)=0

C Thermostatic control data. If 4th item then
        havehi = .false.
        CALL STRIPC(IUO,OUTSTR,99,ND,1,'Thmst control',IER)
        K=0
        CALL EGETWI(OUTSTR,K,ITCTL(ICOMP),-14,4,'W','opr cntl',IER)
        CALL EGETWR(OUTSTR,K,TLO(ICOMP),0.,0.,'-','lower temp',IER)
        CALL EGETWR(OUTSTR,K,TUP(ICOMP),0.,0.,'-','mid temp',IER)
        if(nd.eq.4)then
          havehi=.true.
          CALL EGETWR(OUTSTR,K,THI(ICOMP),0.,0.,'-','hi temp',IER)
        else
          THI(ICOMP)=100.0
        endif
        IF(IER.NE.0) goto 1002

C Generate a brief descrition and proceed acording to control type.
        IY=ITCTL(ICOMP)
        if(IY.eq.-14)then
          ctlstr(icomp,1)='infil only: wind speed  '
        elseif(IY.eq.-13)then
          ctlstr(icomp,1)='infil only: ext db temp '
        elseif(IY.eq.-12)then
          ctlstr(icomp,1)='infil only: adj zone tmp'
        elseif(IY.eq.-11)then
          ctlstr(icomp,1)='infil only: zone temp   '
        elseif(IY.eq.-4)then
          ctlstr(icomp,1)='vent only: wind speed   '
        elseif(IY.eq.-3)then
          ctlstr(icomp,1)='vent only: ext db temp  '
        elseif(IY.eq.-2)then
          ctlstr(icomp,1)='vent only: adj zone temp'
        elseif(IY.eq.-1)then
          ctlstr(icomp,1)='vent only: zone temp    '
        elseif(IY.eq.4)then
          ctlstr(icomp,1)='infil & vent: wind speed'
        elseif(IY.eq.3)then
          ctlstr(icomp,1)='infil & vent: ext db T  '
        elseif(IY.eq.2)then
          ctlstr(icomp,1)='infil & vent: adj zone T'
        elseif(IY.eq.1)then
          ctlstr(icomp,1)='infil & vent: zone T    '
        elseif(IY.eq.0)then
          ctlstr(icomp,1)='no control of air flow  '
          IJK=1
        else

C << put in a loop for each day type >>
          ctlstr(icomp,1)='unknown flow control   '
          ctlstr(icomp,2)='unknown flow control   '
          ctlstr(icomp,3)='unknown flow control   '
          goto 1022
        endif

C << put in a loop for each day type >>
        ctlstr(icomp,2)=ctlstr(icomp,1)  ! set Sat and Sun
        ctlstr(icomp,3)=ctlstr(icomp,1)

        if(ijk.eq.1)goto 202

        CALL STRIPC(IUO,OUTSTR,0,ND,1,'Lower flow details',IER)
        K=0
        CALL EGETWR(OUTSTR,K,ACIL(ICOMP),0.,2000.,'W','l infil',IER)
        CALL EGETWR(OUTSTR,K,ACVL(ICOMP),0.,2000.,'W','l vent',IER)
        CALL EGETWI(OUTSTR,K,IVL(ICOMP),0,MCOM,'W','l vent zn',IER)
        CALL EGETWR(OUTSTR,K,TAL(ICOMP),0.,0.,'-','l vent tmp',IER)

        CALL STRIPC(IUO,OUTSTR,0,ND,1,'Mid flow details',IER)
        K=0
        CALL EGETWR(OUTSTR,K,ACIU(ICOMP),0.,2000.,'W','m infil',IER)
        CALL EGETWR(OUTSTR,K,ACVU(ICOMP),0.,2000.,'W','m vent',IER)
        CALL EGETWI(OUTSTR,K,IVU(ICOMP),0,MCOM,'W','m vent zn',IER)
        CALL EGETWR(OUTSTR,K,TAU(ICOMP),0.,0.,'-','m vent tmp',IER)
        IF(IER.NE.0) goto 1002
        if(havehi)then
          CALL STRIPC(IUO,OUTSTR,0,ND,1,'High flow details',IER)
          K=0
          CALL EGETWR(OUTSTR,K,ACIH(ICOMP),0.,2000.,'W','h inf',IER)
          CALL EGETWR(OUTSTR,K,ACVH(ICOMP),0.,2000.,'W','h vent',IER)
          CALL EGETWI(OUTSTR,K,IVH(ICOMP),0,MCOM,'W','h vent zn',IER)
          CALL EGETWR(OUTSTR,K,TAH(ICOMP),0.,0.,'-','h vent tmp',IER)
          IF(IER.NE.0) goto 1002
        else
          ACIH(ICOMP)=0.
          ACVH(ICOMP)=0.
          IVH(ICOMP)=0
          TAH(ICOMP)=0.
        endif

C Air change information.
  202   DO 400 IDTY=1,NBDAYTYPE
          CALL STRIPC(IUO,OUTSTR,0,ND,1,'flow periods',IER)
          K=0
          CALL EGETWI(OUTSTR,K,ireader,0,MA,'W','periods',IER)
          NAC(IDTY)=IREADER
          IF(NAC(IDTY).EQ.0)goto 400
          DO 10 I=1,NAC(IDTY)
            CALL STRIPC(IUO,OUTSTR,0,ND,1,'Period flow details',IER)
            K=0
            CALL EGETWI(OUTSTR,K,IREADER,0,24,'W','w flow start',IER)
            IACS(IDTY,I)=IREADER
            CALL EGETWI(OUTSTR,K,IREADER,0,24,'W','w flow end',IER)
            IACF(IDTY,I)=IREADER
            CALL EGETWR(OUTSTR,K,READER,0.,2000.,'W','w infil',IER)
            ACI(IDTY,I)=READER
            CALL EGETWR(OUTSTR,K,READER,0.,2000.,'W','w vent',IER)
            ACV(IDTY,I)=READER
            CALL EGETWI(OUTSTR,K,IREADER,0,NCOMP,'W','w vent z',IER)
            IPT(IDTY,I)=IREADER
            CALL EGETWR(OUTSTR,K,READER,0.,0.,'-','w vent tmp',IER)
            TA(IDTY,I)=READER
            IF(IACS(IDTY,I).GT.IACF(IDTY,I))then

C Found a period out of order, warn user and carry on.
              write(loutstr,'(6a)')
     &        ' day type ',calentag(idty),
     &        ' vent start-end mismatch in...',
     &        outstr(1:50),' of operation file ',
     &        currentfile(1:lnblnk(currentfile))
              call edisp248(iuout,loutstr,100)
              IER=1
            ENDIF
            IF(IPT(IDTY,I).EQ.ICOMP)GOTO 1009
 10       CONTINUE
 400    CONTINUE
        IF(IER.NE.0) goto 1002
      elseif(ip3ver(icomp).eq.3)then

C Thermostatic control data.
        havehi = .false.
        idaytype=0
        iperiod=0
  77    CALL STRIPC(IUO,OUTSTR,99,ND,1,'*day definition',IER)
        K=0
        CALL EGETW(OUTSTR,K,WORD,'W','air schedule tags',IER)
        IF(IER.NE.0) goto 1002
        if(WORD(1:4).eq.'*Day'.or.WORD(1:4).eq.'*day')then
          idaytype=idaytype+1
          iperiod=0   ! reset periods for new day type
          CALL EGETW(OUTSTR,K,WORD,'W','air day label',IER)
          CALL EGETWI(OUTSTR,K,IV,0,1,'W','opr infil unit',IER)
          INFUNIT(icomp,idaytype)=IV
          CALL EGETWI(OUTSTR,K,IV,0,1,'W','opr vent unit',IER)
          IVENTUNIT(icomp,idaytype)=IV

C << ITCTL needs to be updated for day type and period >>
          CALL EGETWI(OUTSTR,K,ITCTL(ICOMP),-14,4,'W','opr cntl',IER)
          goto 77
        elseif(WORD(1:7).eq.'*Period'.or.WORD(1:7).eq.'*period')then
          iperiod=iperiod+1
          CALL EGETWI(OUTSTR,K,IVS,0,24,'W','opr period start',IER)
          CALL EGETWI(OUTSTR,K,IVFIN,0,24,'W','opr period finish',IER)
          CALL EGETWR(OUTSTR,K,VALA,0.,2000.,'W','w infil',IER)
          CALL EGETWR(OUTSTR,K,VALB,0.,2000.,'W','w vent',IER)
          CALL EGETWI(OUTSTR,K,IVZ,0,NCOMP,'W','w vent z',IER)
          CALL EGETWR(OUTSTR,K,VALC,0.,0.,'-','w vent tmp',IER)
          NAC(idaytype)=NAC(idaytype)+1
          IACS(idaytype,iperiod)=IVS
          IACF(idaytype,iperiod)=IVFIN
          ACI(idaytype,iperiod)=VALA
          ACV(idaytype,iperiod)=VALB
          IPT(idaytype,iperiod)=IVZ
          TA(idaytype,iperiod)=VALC
          goto 77

        elseif(WORD(1:4).eq.'*Low'.or.WORD(1:4).eq.'*low')then
          CALL EGETWR(OUTSTR,K,TLO(ICOMP),0.,0.,'-','lower temp',IER)
          CALL EGETWR(OUTSTR,K,ACIL(ICOMP),0.,2000.,'W','l infil',IER)
          CALL EGETWR(OUTSTR,K,ACVL(ICOMP),0.,2000.,'W','l vent',IER)
          CALL EGETWI(OUTSTR,K,IVL(ICOMP),0,MCOM,'W','l vent zn',IER)
          CALL EGETWR(OUTSTR,K,TAL(ICOMP),0.,0.,'-','l vent tmp',IER)
          goto 77

        elseif(WORD(1:4).eq.'*Mid'.or.WORD(1:4).eq.'*mid')then
          CALL EGETWR(OUTSTR,K,TUP(ICOMP),0.,0.,'-','mid temp',IER)
          CALL EGETWR(OUTSTR,K,ACIU(ICOMP),0.,2000.,'W','m infil',IER)
          CALL EGETWR(OUTSTR,K,ACVU(ICOMP),0.,2000.,'W','m vent',IER)
          CALL EGETWI(OUTSTR,K,IVU(ICOMP),0,MCOM,'W','m vent zn',IER)
          CALL EGETWR(OUTSTR,K,TAU(ICOMP),0.,0.,'-','m vent tmp',IER)
          goto 77

        elseif(WORD(1:3).eq.'*Hi'.or.WORD(1:3).eq.'*hi')then
          CALL EGETWR(OUTSTR,K,THI(ICOMP),0.,0.,'-','hi temp',IER)
          CALL EGETWR(OUTSTR,K,ACIH(ICOMP),0.,2000.,'W','h inf',IER)
          CALL EGETWR(OUTSTR,K,ACVH(ICOMP),0.,2000.,'W','h vent',IER)
          CALL EGETWI(OUTSTR,K,IVH(ICOMP),0,MCOM,'W','h vent zn',IER)
          CALL EGETWR(OUTSTR,K,TAH(ICOMP),0.,0.,'-','h vent tmp',IER)
          goto 77

        elseif(WORD(1:8).eq.'*End_air'.or.WORD(1:8).eq.'*end_air')then

C At the end of the air schedule data is *end_air which should be followed
C by *casual_schedules and then the casual notes
          CALL LSTRIPC(IUO,LOUTSTR,0,ND,1,'oper *air_schedules',IER)
          if(loutstr(1:17).eq.'*casual_schedules')then
            CALL LSTRIPC(IUO,LOUTSTR,0,ND,1,'oper casual notes',IER)
            oprdesc(icomp)=LOUTSTR
          else
            oprdesc(icomp)=LOUTSTR
          endif

C Flow reporting if requested.
          if(ITRC.GE.1)CALL VENTINF(ICOMP,ITRU)
        endif
      endif


C Casual gains for different day types
      DO 600 IDTY=1,NBDAYTYPE
        CALL STRIPC(IUO,OUTSTR,0,ND,1,'gain periods for day type',IER)
        K=0
        CALL EGETWI(OUTSTR,K,IREADER,0,MC,'W','gain periods',IER)
        NCAS(IDTY)=IREADER
        IF(NCAS(IDTY).EQ.0)goto 600
        DO 40 I=1,NCAS(IDTY)
          CALL STRIPC(IUO,OUTSTR,99,ND,1,'Period gain detl',IER)
          K=0
          if(ND.eq.7.or.ND.eq.12)then
            CALL EGETWI(OUTSTR,K,IREADER,-3,5,'W','gain type',IER)
            ICGT(IDTY,I)=IREADER
          else
            ICGT(IDTY,I)=1
          endif
          CALL EGETWI(OUTSTR,K,IREADER,0,24,'W','gain st',IER)
          ICGS(IDTY,I)=IREADER
          CALL EGETWI(OUTSTR,K,IREADER,0,24,'W','gain fn',IER)
          ICGF(IDTY,I)=IREADER
          CALL EGETWR(OUTSTR,K,READER,0.,0.,'-','gain sens',IER)
          CMGS(IDTY,I)=READER
          CALL EGETWR(OUTSTR,K,READER,0.,0.,'-','gain latent',IER)
          CMGL(IDTY,I)=READER
          CALL EGETWR(OUTSTR,K,READER,0.,1.,'W','gain rad fr',IER)
          RADC(IDTY,I)=READER
          CALL EGETWR(OUTSTR,K,READER,0.,1.,'W','gain conv fr',IER)
          CONC(IDTY,I)=READER
          IF(ICGS(IDTY,I).GT.ICGF(IDTY,I))then

C Found a period out of order, warn user and carry on.
            write(loutstr,'(6a)')
     &      ' daytype ',calentag(idty),
     &      ' gains start-end mismatch in...',
     &      outstr(1:50),' of operation file ',
     &      currentfile(1:lnblnk(currentfile))
            call edisp248(iuout,loutstr,100)
            IER=1
          endif
          X=RADC(IDTY,I)+CONC(IDTY,I)
          IF(X.GT.1.1)then
            write(loutstr,'(6a)')
     &      ' day type ',calentag(idty),
     &      ' rad & conv fractions > 1.0 in...',
     &      outstr(1:50),' of operation file ',
     &      currentfile(1:lnblnk(currentfile))
            call edisp248(iuout,loutstr,100)
            ier=1
          endif
          IF(X.LT..95.AND.ITRC.GT.1)call edisp(iuout,
     &                            ' Casual gain rad:con sum < 1.0')
C Check if electrical data has also been saved.
          if(ND.eq.12)then
            CALL EGETWR(OUTSTR,K,READER,0.,1.0,'W','wkd pf',IER)
            PF(IDTY,I)=READER
            CALL EGETWI(OUTSTR,K,IREADER,-1,1,'W','wkd lag lead',IER)
            IPF(IDTY,I)=IREADER
            CALL EGETWR(OUTSTR,K,READER,0.,1000.,'-','wkd power',IER)
            PWR(IDTY,I)=READER
            CALL EGETWR(OUTSTR,K,READER,0.,1000.,'-','wkd vlt',IER)
            BVOLT(IDTY,I)=READER
            CALL EGETWI(OUTSTR,K,IREADER,1,4,'W','wkd phase',IER)
            IPHAS(IDTY,I)=IREADER
            IELF(ICOMP)=1
          endif
   40   CONTINUE
        IF(IER.NE.0) goto 1002
 600  CONTINUE

C Check to see if gain labels have been added to the end of the file.
      irange=0
      do 701 IDTY=1,NBDAYTYPE
        if (NCAS(IDTY).GT.0) then
          do I = 1,NCAS(IDTY)
            icur=ICGT(IDTY,I)
            if(iabs(icur).gt.irange) irange=iabs(icur)
          enddo
        endif
 701  continue
      if(irange.lt.3) irange=3
      write(6,*) 'casual gain read range is ',irange

C << This is hard-coded to 3 gain type labels. Revise code to check the
C << number of items on the line and loop accordingly.
      CALL STRIPC(IUO,OUTSTR,99,ND,1,'Type labels',IERV)
      IF(IERV.ne.0) THEN
        goto 1001
      ELSE
        if(irange.ne.nd)then
          write(loutstr,'(a,i2,4a)')
     &      'The number of different casual gain types ',irange,
     &      ' differs from tokens on the line...',
     &      outstr(1:50),' of operation file ',
     &      currentfile(1:lnblnk(currentfile))
          call edisp248(iuout,loutstr,100)
          ier=2
        endif
        K=0
        DO 1234 ITYP=1,irange
          CALL EGETW(OUTSTR,K,lodlabel(ICOMP,ITYP),'W',
     &         'type label',IER) 
 1234   CONTINUE
      ENDIF      
 1235 CONTINUE

C Gain reporting if requested.
      if(ITRC.GE.1)call CASINF(ICOMP,ITRU)

     
C Now free project data file.
      CALL ERPFREE(IUO,ISTAT)
      RETURN

C Error messages.
 1000 if(dll)then
        dllsubr='EROPER'
        dllmesg=outs
        ier=2
        CALL ERPFREE(IUO,ISTAT)
        return
      else
        call edisp(iuout,outs)
        IER=1
        CALL ERPFREE(IUO,ISTAT)
        RETURN
      endif

 1001 call usrmsg(' ',
     &  ' No casual gains type names found ...supplying defaults.','-')
        lodlabel(icomp,1)='Occupt'
        lodlabel(icomp,2)='Lights'
        lodlabel(icomp,3)='Equipt' 
        lodlabel(icomp,4)='Other ' 
        lodlabel(icomp,5)='AnnEl ' 
        lodlabel(icomp,6)='NA    ' 
        lodlabel(icomp,7)='NA    ' 
      goto 1235

 1002 write(outs,'(3a)') 'EROPER: conversion error in...',
     &  OUTSTR(1:50),'...'
      lsn=MIN0(lnblnk(currentfile),110)
      write(outs2,'(2a)') 'in: ',currentfile(1:lsn)
      if(dll)then
        dllsubr='EROPER'
        dllmesg=outs
        ier=2
        CALL ERPFREE(IUNIT,ios)
        return
      else
        call edisp(iuout,outs)
        call edisp(iuout,outs2)
        IER=1
        CALL ERPFREE(IUNIT,ios)
        RETURN
      endif

 1009 write(outs,'(2a)')' Vent not from `another` zone in ...',
     &  outstr(1:50)
      goto 1000
 1022 write(outs,'(2a)')' Scheduled infiltration control unknown in',
     &  outstr(1:50)
      goto 1000

      END

C ******************** ERZSCHED 
C ERZSCHED reads air and casual gain schedules from an operations
C        file outside the model into scratch data structure.

C Common block variables are:

C oprdesc      - operation notes (248 char)

C ITCTLC       - Thermostatic control index :
C                    control on zone coupled air temperature
C                    control on zone air temperature
C                  0 no control
C                    control on zone air temperature and infiltration
C                    control on zone coupled air and infiltration
C ACILC, ACVLC,  - low level (i.e. if below a setpoint) for control.
C IVLC, TALC     
C ACIUC, ACVUC,  - mid level (i.e. if above a setpoint) 1st stage options
C IVUC, TAUC       for control.
C ACIHC, ACVHC,  - high level (2nd stage options) for control.
C IVHC, TAHC    

C NACC,          - number of distinct air change periods during Weekdays,
C                  Saturdays and Sundays and other day types if defined

C IACSC,IACFC,   - start and finish hours of each of the above air change
C                  periods relating to Weekdays, Saturdays and Sundays
C                  and other day types if defined

C ACIC           - natural infiltration air changes/hour for each period
C                  relating to Weekdays, Saturdays and Sundays and 
C                  other day types if defined

C ACVC           - additional incoming air changes/hour for each period
C                  relating to Weekdays, Saturdays and Sundays and other
C                  day types if defined

C IPTC           - the additional incoming air (corresponding to ACV) 
C                  can either be at constant temperature -
C                  changing, if required, between each period - or set
C                  at the time-dependent temperature of some coupled
C                  zone. The IPT? variable controls this, where:
C                  IPT?=0 signifies a constant temperature will be specified
C                  IPT?=N (N>0) signifies that incoming air is at the
C                  time-dependent temperature of zone N.

C TAC,           - correspond to IPT?=0 and define the temperature of the
C                  incoming air for each period relating to Weekdays,
C                  Saturdays, Sundays and other day types if defined


C NCASC          - number of casual gains during a typical Weekday,
C                 Saturday and Sunday and other day types if defined

C ICGSC,ICGFC,   - start and finish hours of each of the above casual gain
C                  periods

C CMGSC,CMGLC,   - sensible and latent magnitude (in Watts) of
C                  each casual gain

C RADCC,CONCC,   - radiant and convective portions (proportion
C                  of 1) of each casual gain

C pfC           - power factor of electrical load.
C ipfC          - nature of load lagging (reactive), leading (capacative),
C                or unity (pure resistive).
C pwrC          - real power consumption of the load (W).
C bvoltC        - operational voltage of the load.
C iphasC        - which phase the load is connected to (1-3) or
C                all 3 (4).

C Maximum number of air change periods/day MA=5
C Maximum number of casual gains/day       MC=20
C IUO is the file unit to use
C FILE (variable width char) is the name of the file to scan.

      SUBROUTINE ERZSCHED(IUO,FILE,actflatfile,IER)
#include "building.h"
#include "espriou.h"
C espriou.h provides currentfile.
      
      integer lnblnk  ! function definition

      COMMON/OUTIN/IUOUT,IUIN
      common/pophelp/h(60)
      common/caleni/nbdaytype,nbcaldays(MDTY),icalender(365)
      INTEGER NBDAYTYPE,NBCALDAYS,ICALENDER

C Scratch version of operations file (as pattern in schedule.h).
      integer ip3verc
      common/p3verc/ip3verc

C Descriptive label for a zone load or casual gain types.
      character lodlabelc*6
      common/loadlabelc/lodlabelc(MGTY)

      CHARACTER oprdescc*248,ctlstrc*24
      COMMON/P1C/oprdescc,ctlstrc

C Scratch copy of air flow pattern from an operations file.
      COMMON/P2N/NACC(MDTY),IACSC(MDTY,MA),IACFC(MDTY,MA),ACIC(MDTY,MA),
     &ACVC(MDTY,MA),IPTC(MDTY,MA),TAC(MDTY,MA)
      INTEGER NACC,IACSC,IACFC,IPTC
      REAL ACIC,ACVC,TAC

      COMMON/P2CTLC/ITCTLC,TLOC,TUPC,THIC,ACILC,ACVLC,IVLC,TALC,
     &   ACIUC,ACVUC,IVUC,TAUC,ACIHC,ACVHC,IVHC,TAHC

      COMMON/P3NC/NCASC(MDTY),ICGSC(MDTY,MC),ICGFC(MDTY,MC),
     &CMGSC(MDTY,MC),CMGLC(MDTY,MC),RADCC(MDTY,MC),CONCC(MDTY,MC)

C Scratch version of P3TYPEN.
      COMMON/P3TYPENC/ICGTC(MDTY,MC)
      INTEGER NCASC,ICGSC,ICGFC,ICGTC
      REAL CMGSC,CMGLC,RADCC,CONCC

C Electrical data flag and elctrical data.
      common/elecflgc/ielfc
      COMMON/ELP3NC/NELC(MDTY),PFC(MDTY,MC),IPFC(MDTY,MC),PWRC(MDTY,MC),
     &BVOLTC(MDTY,MC),IPHASC(MDTY,MC)
      INTEGER NELC,IPFC,IPHASC
      REAL PFC,PWRC,BVOLTC

C Error subroutine and error details for dll mode.
      common/dllerr/dllsubr,dllmesg

      CHARACTER outstr*124,outs*124
      CHARACTER*(*) FILE
      character dllsubr*12,dllmesg*124,WORD*20,loutstr*248,outs2*124
      character dstmp*24,H*72
      character louts*248
      character actflatfile*1  ! action requested.
      logical havehi,dll,ok,dok
      INTEGER IDTY,IR
      REAL R

      dok=.false.
 
      if(actflatfile(1:1).eq.'W')then
C Compose help message
        H(1) ='The number of day types defined in the operations file'
        H(2) ='from outside the current model definitiion should be '
        H(3) ='the same as the number of day types defined in the '
        H(4) ='current model. If this is not the case then the data '
        H(5) ='imported may corrupt the operations file in the current '
        H(6) ='model.'
        CALL PHELPD('Warning',6,'-',0,0,IER)
        call askok(' ','Import data?',OK,dok,3)
      elseif(actflatfile(1:1).eq.'-')then
        ok=.true.
      endif
      if(.not.ok)return

C Check if running in dll mode.
      call isadll(dll)

C Initialise project data file.
      CALL EFOPSEQ(IUO,FILE,1,IER)
      IF(IER.NE.0)THEN
        write(louts,'(3a)') 'Operations file ',
     &    FILE(1:lnblnk(FILE)),' could not be opened.'
        if(dll)then
          dllsubr='ERZSCHED'
          write(dllmesg,'(a)') louts(1:124)
          ier=2
          return
        else
          call edisp248(iuout,louts,100)
          IER=1
          RETURN
        endif
      ENDIF
      write(currentfile,'(a)') FILE(1:lnblnk(FILE))
      ip3verc=0

C Initialise the iphase arrays to avoid zero array elements.
      do 909 iph=1, mc
        do 989 idty=1,nbdaytype
          iphasc(idty,iph)=1
 989    continue
 909  continue

C Read lines from file, discarding comments. Look for header on
C newer files. Older files will begin with a description. To allow for
C description with spaces copy directly from OUTSTR rather than parsing
C it into words.  
      CALL STRIPC(IUO,OUTSTR,0,ND,1,'oper name or header',IER)
      IF(IER.NE.0) goto 1002
      if(outstr(1:11).eq.'*Operations')then
        if(outstr(13:15).eq.'1.0')then
          ip3verc=1
        elseif(outstr(13:15).eq.'2.0')then
          ip3verc=2
        elseif(outstr(13:15).eq.'3.0')then
          ip3verc=3
        endif
        CALL STRIPC(IUO,OUTSTR,0,ND,1,'header',IER)
        IF(IER.NE.0) goto 1002
        K=0
        CALL EGETW(OUTSTR,K,WORD,'W','header tags',IER)
        IF(IER.NE.0) goto 1002
        if(WORD(1:5).eq.'*Date'.or.WORD(1:5).eq.'*date')then
          CALL EGETRM(OUTSTR,K,dstmp,'W','date stamp',IER)
        endif
        CALL LSTRIPC(IUO,LOUTSTR,0,ND,1,'oper notes',IER)
        oprdescc=LOUTSTR
      else
        oprdescc=OUTSTR
      endif

C Thermostatic control data. If 4th item then
      havehi = .false.
      CALL STRIPC(IUO,OUTSTR,99,ND,1,'Thmst control',IER)
      K=0
      CALL EGETWI(OUTSTR,K,ITCTLC,-14,4,'W','opr cntl',IER)
      CALL EGETWR(OUTSTR,K,TLOC,0.,0.,'-','lower temp',IER)
      CALL EGETWR(OUTSTR,K,TUPC,0.,0.,'-','mid temp',IER)
      if(nd.eq.4)then
        havehi=.true.
        CALL EGETWR(OUTSTR,K,THIC,0.,0.,'-','hi temp',IER)
      else
        THIC=100.0
      endif
      IF(IER.NE.0) goto 1002

C Generate a brief descrition and proceed acording to control type.
      IY=ITCTLC
      if(IY.eq.-14)then
        ctlstrc='infil only: wind speed  '
      elseif(IY.eq.-13)then
        ctlstrc='infil only: ext db temp '
      elseif(IY.eq.-12)then
        ctlstrc='infil only: adj zone tmp'
      elseif(IY.eq.-11)then
        ctlstrc='infil only: zone temp   '
      elseif(IY.eq.-4)then
        ctlstrc='vent only: wind speed   '
      elseif(IY.eq.-3)then
        ctlstrc='vent only: ext db temp  '
      elseif(IY.eq.-2)then
        ctlstrc='vent only: adj zone temp'
      elseif(IY.eq.-1)then
        ctlstrc='vent only: zone temp    '
      elseif(IY.eq.4)then
        ctlstrc='infil & vent: wind speed'
      elseif(IY.eq.3)then
        ctlstrc='infil & vent: ext db T  '
      elseif(IY.eq.2)then
        ctlstrc='infil & vent: adj zone T'
      elseif(IY.eq.1)then
        ctlstrc='infil & vent: zone T    '
      elseif(IY.eq.0)then
        ctlstrc='no control of air flow  '
        goto 22
      else
        ctlstrc='unknown flow control   '
        goto 1022
      endif

      CALL STRIPC(IUO,OUTSTR,0,ND,1,'Lower flow details',IER)
      K=0
      CALL EGETWR(OUTSTR,K,ACILC,0.,2000.,'W','l infil',IER)
      CALL EGETWR(OUTSTR,K,ACVLC,0.,2000.,'W','l vent',IER)
      CALL EGETWI(OUTSTR,K,IVLC,0,MCOM,'W','l vent zn',IER)
      CALL EGETWR(OUTSTR,K,TALC,0.,0.,'-','l vent tmp',IER)

      CALL STRIPC(IUO,OUTSTR,0,ND,1,'Mid flow details',IER)
      K=0
      CALL EGETWR(OUTSTR,K,ACIUC,0.,2000.,'W','m infil',IER)
      CALL EGETWR(OUTSTR,K,ACVUC,0.,2000.,'W','m vent',IER)
      CALL EGETWI(OUTSTR,K,IVUC,0,MCOM,'W','m vent zn',IER)
      CALL EGETWR(OUTSTR,K,TAUC,0.,0.,'-','m vent tmp',IER)
      IF(IER.NE.0) goto 1002
      if(havehi)then
        CALL STRIPC(IUO,OUTSTR,0,ND,1,'High flow details',IER)
        K=0
        CALL EGETWR(OUTSTR,K,ACIHC,0.,2000.,'W','h inf',IER)
        CALL EGETWR(OUTSTR,K,ACVHC,0.,2000.,'W','h vent',IER)
        CALL EGETWI(OUTSTR,K,IVHC,0,MCOM,'W','h vent zn',IER)
        CALL EGETWR(OUTSTR,K,TAHC,0.,0.,'-','h vent tmp',IER)
        IF(IER.NE.0) goto 1002
      else
        ACIHC=0.
        ACVHC=0.
        IVHC=0
        TAHC=0.
      endif

C Air change information.
 22   DO 400 IDTY=1,NBDAYTYPE
        CALL STRIPC(IUO,OUTSTR,0,ND,1,'flow periods',IER)
        K=0
        CALL EGETWI(OUTSTR,K,NACC(IDTY),0,MA,'W','flow periods',IER)
        IF(NACC(IDTY).EQ.0)goto 400
        DO 10 I=1,NACC(IDTY)
          CALL STRIPC(IUO,OUTSTR,0,ND,1,'Period flow details',IER)
          K=0
          CALL EGETWI(OUTSTR,K,IR,0,24,'W','flow start',IER)
          IACSC(IDTY,I)=IR
          CALL EGETWI(OUTSTR,K,IR,0,24,'W','flow end',IER)
          IACFC(IDTY,I)=IR
          CALL EGETWR(OUTSTR,K,R,0.,2000.,'W','infil',IER)
          ACIC(IDTY,I)=R
          CALL EGETWR(OUTSTR,K,R,0.,2000.,'W','vent',IER)
          ACVC(IDTY,I)=R
          CALL EGETWI(OUTSTR,K,IR,0,100,'W','vent z',IER)
          IPTC(IDTY,I)=IR
          CALL EGETWR(OUTSTR,K,R,0.,0.,'-','vent tmp',IER)
          TAC(IDTY,I)=R
   10   CONTINUE
 400  CONTINUE
      IF(IER.NE.0) goto 1002

C Casual gains.
      DO 600 IDTY=1,NBDAYTYPE
        CALL STRIPC(IUO,OUTSTR,0,ND,1,'gain periods',IER)
        K=0
        CALL EGETWI(OUTSTR,K,IR,0,MC,'W','periods',IER)
        NCASC(IDTY)=IR
        IF(NCASC(IDTY).EQ.0)goto 600
        DO 40 I=1,NCASC(IDTY)
          CALL STRIPC(IUO,OUTSTR,99,ND,1,'Period gain detl',IER)
          K=0
          if(ND.eq.7.or.ND.eq.12)then
            CALL EGETWI(OUTSTR,K,IR,-3,5,'W','day type',IER)
            ICGTC(IDTY,I)=IR
          else
            ICGTC(IDTY,I)=1
          endif
          CALL EGETWI(OUTSTR,K,IR,0,24,'W',' gain st',IER)
          ICGSC(IDTY,I)=IR
          CALL EGETWI(OUTSTR,K,IR,0,24,'W',' gain fn',IER)
          ICGFC(IDTY,I)=IR
          CALL EGETWR(OUTSTR,K,R,0.,0.,'-',' sens',IER)
          CMGSC(IDTY,I)=R
          CALL EGETWR(OUTSTR,K,R,0.,0.,'-',' latent',IER)
          CMGLC(IDTY,I)=R
          CALL EGETWR(OUTSTR,K,R,0.,1.,'W',' rad fr',IER)
          RADCC(IDTY,I)=R
          CALL EGETWR(OUTSTR,K,R,0.,1.,'W',' conv fr',IER)
          CONCC(IDTY,I)=R

C Check if electrical data has also been saved.
          if(ND.eq.12)then
            CALL EGETWR(OUTSTR,K,R,0.,1.0,'W',' pf',IER)
            PFC(IDTY,I)=R
            CALL EGETWI(OUTSTR,K,IR,-1,1,'W',' lag lead',IER)
            IPFC(IDTY,I)=IR
            CALL EGETWR(OUTSTR,K,R,0.,1000.,'-',' power',IER)
            PWRC(IDTY,I)=R
            CALL EGETWR(OUTSTR,K,R,0.,1000.,'-',' vlt',IER)
            BVOLTC(IDTY,I)=R
            CALL EGETWI(OUTSTR,K,IR,1,4,'W',' phase',IER)
            IPHASC(IDTY,I)=IR
            IELFC=1
          endif
   40   CONTINUE
  600 CONTINUE

C Check to see if gain labels have been added to the end of the file.
C Fill defaults prior to scanning the labels.
      CALL STRIPC(IUO,OUTSTR,99,ND,1,'Type labels',IERV)
      IF(IERV.ne.0) THEN
        goto 1001
      ELSE
        lodlabelc(1)='Occupt'
        lodlabelc(2)='Lights'
        lodlabelc(3)='Equipt' 
        lodlabelc(4)='Other ' 
        lodlabelc(5)='AnnEl ' 
        lodlabelc(6)='NA    ' 
        lodlabelc(7)='NA    ' 
        K=0
        DO 1234 ITYP=1,ND  ! for each token found
          CALL EGETW(OUTSTR,K,lodlabelc(ITYP),'W','type label',IER) 
 1234   CONTINUE
      ENDIF      
 1235 CONTINUE

C Now free project data file.
      CALL ERPFREE(IUO,ISTAT)
      RETURN

C Error messages.
 1000 if(dll)then
        dllsubr='ERZSCHED'
        write(dllmesg,'(a)') louts(1:124)
        ier=2
        CALL ERPFREE(IUO,ISTAT)
        return
      else
        call edisp248(iuout,louts,100)
        IER=1
        CALL ERPFREE(IUO,ISTAT)
        RETURN
      endif

 1001 call usrmsg(' ',
     &  ' No casual gains type names found ...supplying defaults.','-')
        lodlabelc(1)='Occupt'
        lodlabelc(2)='Lights'
        lodlabelc(3)='Equipt' 
        lodlabelc(4)='Other ' 
        lodlabelc(5)='AnnEl ' 
        lodlabelc(6)='NA    ' 
        lodlabelc(7)='NA    ' 
      goto 1235

 1002 write(outs,'(3a)') 'ERZSCHED: conversion error in...',
     &  OUTSTR(1:50),'...'
      lsn=MIN0(lnblnk(currentfile),110)
      write(outs2,'(2a)') 'in: ',currentfile(1:lsn)
      if(dll)then
        dllsubr='ERZSCHED'
        dllmesg=outs
        ier=2
        CALL ERPFREE(IUNIT,ios)
        return
      else
        call edisp(iuout,outs)
        call edisp(iuout,outs2)
        IER=1
        CALL ERPFREE(IUNIT,ios)
        RETURN
      endif

 1022 write(outs,'(2a)')' Scheduled infiltration control unknown in',
     &  outstr(1:50)
      goto 1000
      END

C ******************** ZSCHEDHINT 
C ZSCHEDHINT scans documentation in an operation file and reports on
C it so a user can decide if they want to use it. It is written to
C not alter any common blocks.
C IUO is the file unit to use
C FILE (variable width char) is the name of the file to scan.
      SUBROUTINE ZSCHEDHINT(IUO,FILE,IER)
#include "building.h"
#include "espriou.h"
C espriou.h provides currentfile.
      
      integer lnblnk  ! function definition

      COMMON/OUTIN/IUOUT,IUIN
      COMMON/POPHELP/H(60)

C Error subroutine and error details for dll mode.
      common/dllerr/dllsubr,dllmesg

      CHARACTER oprdescc*248,ctlstrc*24,outstr*124,outs*124
      CHARACTER*(*) FILE
      character dllsubr*12,dllmesg*124,WORD*20,loutstr*248
      character dstmp*24,h*72
      character louts*248
      logical havehi,dll,ok,dok
      integer nacc,ndty

      dimension nacc(mdty)

      dok=.true.
      IELFC=0

C Initialise number of day types
      NDTY=3

C Compose help message
      H(1) ='The number of day types defined in the operations file'
      H(2) ='from outside the current model definitiion should be '
      H(3) ='the same as the number of day types defined in the current'
      H(4) ='model. If this is not the case then the data imported may'
      H(5) ='corrupt the operations file in the current model.'
      h(6) ='You will be asked for the number of day types defined in '
      h(7) ='the model to which this operations file belongs. If you'
      h(8) ='have not altered it then this number is 3. Otherwise open'
      h(9) ='the model to which the operations file belongs, go to the '
      h(10)='context menu and choose calendar to view the number of '
      h(11)='day types this file has'
      CALL PHELPD('Warning',11,'-',0,0,IER)
      call askok(' ','Import data?',OK,dok,3)
      if(.not.ok)return
      CALL EASKI(ndty,' Number of day types',
     &      '(see help) ',0,'F',mdty,'F',3,'day types',IER,11) 

C Check if running in dll mode.
      call isadll(dll)

C Initialise project data file.
      CALL EFOPSEQ(IUO,FILE,1,IER)
      IF(IER.NE.0)THEN
        write(louts,'(3a)') 'Operations file ',
     &    FILE(1:lnblnk(FILE)),' could not be opened.'
        if(dll)then
          dllsubr='ERZSCHED'
          write(dllmesg,'(a)') louts(1:124)
          ier=2
          return
        else
          call edisp248(iuout,louts,100)
          IER=1
          RETURN
        endif
      ENDIF
      write(currentfile,'(a)') FILE(1:lnblnk(FILE))

C Read lines from file, discarding comments. Look for header on
C newer files. Older files will begin with a description. To allow for
C description with spaces copy directly from OUTSTR rather than parsing
C it into words.  
      CALL STRIPC(IUO,OUTSTR,0,ND,1,'oper name or header',IER)
      if(outstr(1:11).eq.'*Operations')then
        if(outstr(13:15).eq.'1.0')then
          ip3verc=1
        elseif(outstr(13:15).eq.'2.0')then
          ip3verc=2
        elseif(outstr(13:15).eq.'3.0')then
          ip3verc=3
        endif
        CALL STRIPC(IUO,OUTSTR,0,ND,1,'header',IER)
        K=0
        CALL EGETW(OUTSTR,K,WORD,'W','header tags',IER)
        if(WORD(1:5).eq.'*Date'.or.WORD(1:5).eq.'*date')then
          CALL EGETRM(OUTSTR,K,dstmp,'W','date stamp',IER)
        endif
        CALL LSTRIPC(IUO,LOUTSTR,0,ND,1,'oper notes',IER)
        oprdescc=LOUTSTR
      else
        oprdescc=OUTSTR
      endif
      call edisp(iuout,' Source operation notes:')
      call edisp248(iuout,oprdescc,80)

C Thermostatic control data. If 4th item then
      havehi = .false.
      CALL STRIPC(IUO,OUTSTR,99,ND,1,'Thmst control',IER)
      K=0
      CALL EGETWI(OUTSTR,K,ITCTLC,-14,4,'W','opr cntl',IER)
      if(nd.eq.4)havehi=.true.

C Generate a brief descrition and proceed acording to control type.
      IY=ITCTLC
      if(IY.eq.-14)then
        ctlstrc='infil only: wind speed  '
      elseif(IY.eq.-13)then
        ctlstrc='infil only: ext db temp '
      elseif(IY.eq.-12)then
        ctlstrc='infil only: adj zone tmp'
      elseif(IY.eq.-11)then
        ctlstrc='infil only: zone temp   '
      elseif(IY.eq.-4)then
        ctlstrc='vent only: wind speed   '
      elseif(IY.eq.-3)then
        ctlstrc='vent only: ext db temp  '
      elseif(IY.eq.-2)then
        ctlstrc='vent only: adj zone temp'
      elseif(IY.eq.-1)then
        ctlstrc='vent only: zone temp    '
      elseif(IY.eq.4)then
        ctlstrc='infil & vent: wind speed'
      elseif(IY.eq.3)then
        ctlstrc='infil & vent: ext db T  '
      elseif(IY.eq.2)then
        ctlstrc='infil & vent: adj zone T'
      elseif(IY.eq.1)then
        ctlstrc='infil & vent: zone T    '
      elseif(IY.eq.0)then
        ctlstrc='no control of air flow  '
        write(outs,'(a,a)') ' Control: ',ctlstrc
        call edisp(iuout,outs) 
        goto 22
      else
        ctlstrc='unknown flow control   '
      endif
      write(outs,'(a,a)') ' Control: ',ctlstrc
      call edisp(iuout,outs) 

      CALL STRIPC(IUO,OUTSTR,0,ND,1,'Lower flow details',IER)

      CALL STRIPC(IUO,OUTSTR,0,ND,1,'Mid flow details',IER)
      if(havehi)then
        CALL STRIPC(IUO,OUTSTR,0,ND,1,'High flow details',IER)
      endif

C Air change information.
 22   DO 400 IDTY=1,NDTY
        CALL STRIPC(IUO,OUTSTR,0,ND,1,'flow periods',IER)
        K=0
        CALL EGETWI(OUTSTR,K,NACC(idty),0,MA,'W','periods',IER)
        IF(NACC(idty).EQ.0)goto 4
        DO 10 I=1,NACC(idty)
          CALL STRIPC(IUO,OUTSTR,0,ND,1,'Period flow details',IER)
  10    CONTINUE
  4     WRITE(outs,'(A,2I3)')
     &  ' Number of air flow periods in day type no. ',idty,NACC(idty)
        call edisp(iuout,outs)
 400  CONTINUE

C Casual gains.
      DO 500 IDTY=1,NDTY      
        CALL STRIPC(IUO,OUTSTR,0,ND,1,' gain periods',IER)
        K=0
        CALL EGETWI(OUTSTR,K,NCASC,0,MC,'W',' periods',IER)
        IF(NCASC.EQ.0)goto 5
        DO 40 I=1,NCASC
          CALL STRIPC(IUO,OUTSTR,99,ND,1,'Period gain detl',IER)

C Check if electrical data has also been saved.
          if(ND.eq.12)IELFC=1
   40   CONTINUE
    5   WRITE(outs,'(A,I3,A,I3)')' Number of casual gains in day type ',
     &  IDTY,' = ',NCASC
        call edisp(iuout,outs)
        if(IELFC.eq.1)call edisp(iuout,
     &    'Electrical data has been defined for some periods.')
 500  CONTINUE

      if(ip3verc.eq.0) call edisp(iuout,
     &    'Operations file is an out of date format.')
      call edisp(iuout,' ')

C Now free project data file.
      CALL ERPFREE(IUO,ISTAT)
      RETURN
      END

C --------- EMKOPER
C Write zone operation common block data to file. It is assumed
C that this information has been checked.  OPFIL is the name of
C the file to be written to (confirm if to be overwritten).
      SUBROUTINE  EMKOPER(IUO,OPFIL,ICOMP,IER)
#include "building.h"
#include "geometry.h"
#include "schedule.h"
#include "espriou.h"
      
      integer lnblnk  ! function definition

C Parameters
      integer IUO         ! file unit
      character OPFIL*72  ! zone file name
      integer ICOMP       ! zone number
      integer IER         ! 0 OK IER 1 problem

      common/caleni/nbdaytype,nbcaldays(MDTY),icalender(365)
      INTEGER NBDAYTYPE,NBCALDAYS,ICALENDER,IDTY
      common/calena/calename,calentag(MDTY),calendayname(MDTY)
      CHARACTER CALENAME*32,CALENTAG*12,CALENDAYNAME*32
      COMMON/OUTIN/IUOUT,IUIN

      COMMON/P2N/NAC(MDTY),IACS(MDTY,MA),IACF(MDTY,MA),ACI(MDTY,MA),
     &ACV(MDTY,MA),IPT(MDTY,MA),TA(MDTY,MA)
      INTEGER NAC,IACS,IACF,IPT
      REAL ACI,ACV,TA
      COMMON/P2CTL/ITCTL(MCOM),TLO(MCOM),TUP(MCOM),THI(MCOM),ACIL(MCOM),
     &             ACVL(MCOM),IVL(MCOM),TAL(MCOM),ACIU(MCOM),ACVU(MCOM),
     &             IVU(MCOM),TAU(MCOM),ACIH(MCOM),ACVH(MCOM),
     &             IVH(MCOM),TAH(MCOM)
      COMMON/P3N/NCAS(MDTY),ICGS(MDTY,MC),ICGF(MDTY,MC),
     &CMGS(MDTY,MC),CMGL(MDTY,MC),RADC(MDTY,MC),CONC(MDTY,MC)
      INTEGER NCAS,ICGS,ICGF
      REAL CMGS,CMGL,RADC,CONC

C Electrical data flag and elctrical data.
      common/elecflg/ielf(mcom)
      COMMON/ELP3N/NEL(MDTY),PF(MDTY,MC),IPF(MDTY,MC),PWR(MDTY,MC),
     &BVOLT(MDTY,MC),IPHAS(MDTY,MC)
      INTEGER NEL,IPF,IPHAS
      REAL PF,PWR,BVOLT
      COMMON/Vld20/Vldtng

      character dstmp*24
      logical Vldtng
      integer irange,icur  ! for looping
      
      IER=0

C Open any existing file by this name (ask user for confirmation to
C over-write) or create a new file.
      if(Vldtng)then
        CALL EFOPSEQ(IUO,OPFIL,3,IER)
        write(currentfile,'(a)') OPFIL(1:lnblnk(OPFIL))
      else
        CALL EFOPSEQ(IUO,OPFIL,4,IER)
        write(currentfile,'(a)') OPFIL(1:lnblnk(OPFIL))
      endif
      IF(IER.LT.0)THEN
        IER=1
        RETURN
      ENDIF

C If version 1 write out header.
      call dstamp(dstmp)
      if(ip3ver(icomp).eq.1)then
        WRITE(IUO,'(A)',IOSTAT=IOS,ERR=1) '*Operations 1.0'
        WRITE(IUO,'(3a)',IOSTAT=IOS,ERR=1) '*date ',dstmp,
     &  '  # latest file modification '
      elseif(ip3ver(icomp).eq.2)then
        WRITE(IUO,'(A)',IOSTAT=IOS,ERR=1) '*Operations 2.0'
        WRITE(IUO,'(3a)',IOSTAT=IOS,ERR=1) '*date ',dstmp,
     &  '  # latest file modification '
      elseif(ip3ver(icomp).eq.3)then
        WRITE(IUO,'(A)',IOSTAT=IOS,ERR=1) '*Operations 3.0'
        WRITE(IUO,'(3a)',IOSTAT=IOS,ERR=1) '*date ',dstmp,
     &  '  # latest file modification '
      endif
      WRITE(IUO,30,IOSTAT=IOS,ERR=1)
     &  zname(ICOMP)(1:lnzname(ICOMP)),OPFIL(1:lnblnk(OPFIL))
  30  FORMAT('# operations of ',a,' defined in: ',/,'# ',a)

C Write the common block data to the file.
C If ipv3ver is 1 or 2 write ventilation control data as if
C there was only one day type and one period during the day.
      if(ip3ver(icomp).eq.1.or.ip3ver(icomp).eq.2)then

C << revise for multiple days and periods >>

        WRITE(IUO,'(A)',IOSTAT=IOS,ERR=1)
     &    oprdesc(ICOMP)(1:lnblnk(oprdesc(ICOMP)))
        WRITE(IUO,'(3A)',IOSTAT=IOS,ERR=1)
     &    '# control(',ctlstr(icomp,1),'), low mid & high setpoints '
        WRITE(IUO,'(I4,3F10.3)',IOSTAT=IOS,ERR=1)ITCTL(ICOMP),
     &      TLO(ICOMP),TUP(ICOMP),THI(ICOMP)

        IF(ITCTL(ICOMP).NE.0)THEN
          WRITE(IUO,'(A)',IOSTAT=IOS,ERR=1)
     &      '# lower: infil, vent, source, data'
          WRITE(IUO,'(2F10.3,I5,F10.3)')ACIL(ICOMP),ACVL(ICOMP),
     &        IVL(ICOMP),TAL(ICOMP)
          WRITE(IUO,'(A)',IOSTAT=IOS,ERR=1)
     &      '# middle: infil, vent, source, data'
          WRITE(IUO,'(2F10.3,I5,F10.3)')ACIU(ICOMP),ACVU(ICOMP),
     &      IVU(ICOMP),TAU(ICOMP)
          WRITE(IUO,'(A)',IOSTAT=IOS,ERR=1)
     &      '# high: infil, vent, source, data'
          WRITE(IUO,'(2F10.3,I5,F10.3)')ACIH(ICOMP),ACVH(ICOMP),
     &      IVH(ICOMP),TAH(ICOMP)
        ENDIF

      elseif(ip3ver(icomp).eq.3)then

C Write the infiltration and ventilation documentation
        WRITE(IUO,'(A)',IOSTAT=IOS,ERR=1)
     &    ventdesc(ICOMP)(1:lnblnk(ventdesc(ICOMP)))

C Write the infiltration and ventilation periods.
C << to be done >>

C Write the infiltration and ventilation control data periods.
C << to be done >>

C Finally write the documentation for casual gains.
        WRITE(IUO,'(A)',IOSTAT=IOS,ERR=1)
     &    oprdesc(ICOMP)(1:lnblnk(oprdesc(ICOMP)))

      endif

      DO 400 IDTY=1,NBDAYTYPE
        WRITE(IUO,'(1X,I5,2A)',IOSTAT=ISTAT,ERR=1)NAC(IDTY),
     &  '   # number of flow periods in day type: ',calentag(idty)
        IF (NAC(IDTY).GT.0)THEN
          WRITE(IUO,'(A)',IOSTAT=IOS,ERR=1)
     &    '# start, stop, infil, ventil, source, data'
          DO 1120 I = 1,NAC(IDTY)
            WRITE(IUO,5450,IOSTAT=IOS,ERR=1)IACS(IDTY,I),IACF(IDTY,I),
     &            ACI(IDTY,I),ACV(IDTY,I),IPT(IDTY,I),TA(IDTY,I)
 1120     CONTINUE
        ENDIF
 400  CONTINUE

C Check the range of icgt, remember in irange and use for looping.
      irange=0
      do 700 IDTY=1,NBDAYTYPE
        if (NCAS(IDTY).GT.0) then
          do I = 1,NCAS(IDTY)
            icur=ICGT(IDTY,I)
            if(iabs(icur).gt.irange) irange=iabs(icur)
          enddo
        endif
 700  continue
      if(irange.lt.3) irange=3
      write(6,*) 'casual gain range is ',irange

      DO 600 IDTY=1,NBDAYTYPE
        WRITE(IUO,'(1X,I5,2A)',IOSTAT=IOS,ERR=1)NCAS(IDTY),
     &  '   # number of casual gains in day type: ',calentag(idty)
        IF (NCAS(IDTY).GT.0)THEN
          IF(IELF(ICOMP).EQ.1) THEN
            WRITE(IUO,'(a,a)',IOSTAT=IOS,ERR=1)
     &      '# typ, sta, fin, sen, lat, rad, ',
     &      'con, pf, +/-, pwr, volt, pha'  
            DO 1159 I = 1,NCAS(IDTY)
              WRITE(IUO,5470,IOSTAT=IOS,ERR=1)ICGT(IDTY,I),ICGS(IDTY,I),
     &        ICGF(IDTY,I),CMGS(IDTY,I),CMGL(IDTY,I),RADC(IDTY,I),
     &        CONC(IDTY,I),PF(IDTY,I),IPF(IDTY,I),PWR(IDTY,I),
     &        BVOLT(IDTY,I),IPHAS(IDTY,I) 
1159        CONTINUE
          ELSE
            WRITE(IUO,'(A)',IOSTAT=IOS,ERR=1)
     &     '# type, start, stop, sens, latent, rad_frac, conv_frac'
            DO 1160 I = 1,NCAS(IDTY)
              WRITE(IUO,5460,IOSTAT=IOS,ERR=1)ICGT(IDTY,I),ICGS(IDTY,I),
     &        ICGF(IDTY,I),CMGS(IDTY,I),CMGL(IDTY,I),RADC(IDTY,I),
     &        CONC(IDTY,I)
1160        CONTINUE
          ENDIF
        ENDIF
 600  CONTINUE

5450  FORMAT(1X,I3,',',I3,',',2F9.3,I5,F9.3)
5460  FORMAT(1X,3(I4,','),F9.1,',',F9.1,',',F6.3,',',F6.3)
5470  FORMAT(1X,3(I3,','),F7.1,',',F7.1,',',F4.1,',',F4.1,',',F5.2,
     &       ',',I2,',',F7.1,',',F7.1,',',I2)

C Write out the type labels to the file for future reference. In version
C 3 this section is relocated and extended.
      WRITE(IUO,'(a)')'# Labels for gain types '
      WRITE(IUO,'(7(1X,A6))',IOSTAT=IOS,ERR=1) 
     &  (lodlabel(icomp,I),I=1,irange)
     
      CALL ERPFREE(IUO,ISTAT)
      RETURN

 1    if(IOS.eq.2)then
        call usrmsg(' No permission to write operations file!',
     &            ' returning to menu...','W')
      else
        call usrmsg(' Operations file transfer error !',
     &            ' returning to menu...','W')
      endif
      RETURN
      END


C ******************* VENTINF 
C VNTINFO provides an English description of scheduled air flow and
C control from zone operation common block data.

C << need to update reporting for different day types and ctl periods >>
      SUBROUTINE VENTINF(ICOMP,ITRU)
#include "building.h"
#include "geometry.h"
#include "schedule.h"

      common/calena/calename,calentag(MDTY),calendayname(MDTY)
      CHARACTER CALENAME*32,CALENTAG*12,CALENDAYNAME*32
      common/caleni/nbdaytype,nbcaldays(MDTY),icalender(365)
      INTEGER NBDAYTYPE,NBCALDAYS,ICALENDER

      COMMON/P2N/NAC(MDTY),IACS(MDTY,MA),IACF(MDTY,MA),ACI(MDTY,MA),
     &ACV(MDTY,MA),IPT(MDTY,MA),TA(MDTY,MA)
      INTEGER NAC,IACS,IACF,IPT
      REAL ACI,ACV,TA
      COMMON/P2CTL/ITCTL(MCOM),TLO(MCOM),TUP(MCOM),THI(MCOM),ACIL(MCOM),
     &             ACVL(MCOM),IVL(MCOM),TAL(MCOM),ACIU(MCOM),ACVU(MCOM),
     &             IVU(MCOM),TAU(MCOM),ACIH(MCOM),ACVH(MCOM),
     &             IVH(MCOM),TAH(MCOM)

      CHARACTER outs*124

C Write documentation on air schedules. 
      call edisp(itru,' Air schedule notes:')
      call edisp248(itru,ventdesc(icomp),72)
      if(ip3ver(icomp).le.2)then

C Older file versions only display ctlstr for first day and period.
        IY=ITCTL(ICOMP)
        if(IY.eq.-14)then
          ctlstr(icomp,1)='infil only: wind speed  '
        elseif(IY.eq.-13)then
          ctlstr(icomp,1)='infil only: ext db temp '
        elseif(IY.eq.-12)then
          ctlstr(icomp,1)='infil only: adj zone tmp'
        elseif(IY.eq.-11)then
          ctlstr(icomp,1)='infil only: zone temp   '
        elseif(IY.eq.-4)then
          ctlstr(icomp,1)='vent only: wind speed   '
        elseif(IY.eq.-3)then
          ctlstr(icomp,1)='vent only: ext db temp  '
        elseif(IY.eq.-2)then
          ctlstr(icomp,1)='vent only: adj zone temp'
        elseif(IY.eq.-1)then
          ctlstr(icomp,1)='vent only: zone temp    '
        elseif(IY.eq.4)then
          ctlstr(icomp,1)='infil & vent: wind speed'
        elseif(IY.eq.3)then
          ctlstr(icomp,1)='infil & vent: ext db T  '
        elseif(IY.eq.2)then
          ctlstr(icomp,1)='infil & vent: adj zone T'
        elseif(IY.eq.1)then
          ctlstr(icomp,1)='infil & vent: zone T    '
        elseif(IY.eq.0)then
          ctlstr(icomp,1)='no control of air flow  '
        else
          ctlstr(icomp,1)='unknown flow control    '
        endif
        write(outs,'(a,a)') ' Control: ',ctlstr(icomp,1)
        call edisp(itru,outs)

        if(IY.eq.0) goto 1225
        WRITE(outs,'(A,3F7.2)')' Lower/Middle/High temp setpoints: ',
     &    TLO(ICOMP),TUP(ICOMP),THI(ICOMP)
        call edisp(itru,outs)
        write(outs,'(20x,a,a)')'Infil. ac/h m^3/s  Vent. ac/h m^3/s ',
     &    ' from  data'
        call edisp(itru,outs)
        acilm = (VOL(ICOMP)*ACIL(ICOMP))/3600.0
        acvlm = (VOL(ICOMP)*ACVL(ICOMP))/3600.0
        WRITE(outs,'(A,F9.3,F8.4,F10.3,F8.4,I5,F10.3)')
     &    ' Lower range data ',ACIL(ICOMP),acilm,
     &    ACVL(ICOMP),acvlm,IVL(ICOMP),TAL(ICOMP)
        call edisp(itru,outs)

        acium = (VOL(ICOMP)*ACIU(ICOMP))/3600.0
        acvum = (VOL(ICOMP)*ACVU(ICOMP))/3600.0
        WRITE(outs,'(A,F9.3,F8.4,F10.3,F8.4,I5,F10.3)')
     &    ' Middle range data',ACIU(ICOMP),acium,
     &    ACVU(ICOMP),acvum,IVU(ICOMP),TAU(ICOMP)
        call edisp(itru,outs)

        acihm = (VOL(ICOMP)*ACIH(ICOMP))/3600.0
        acvhm = (VOL(ICOMP)*ACVH(ICOMP))/3600.0
        WRITE(outs,'(A,F9.3,F8.4,F10.3,F8.4,I5,F10.3)')
     &    ' High range data  ',ACIH(ICOMP),acihm,
     &    ACVH(ICOMP),acvhm,IVH(ICOMP),TAH(ICOMP)
        call edisp(itru,outs)
      elseif(ip3ver(icomp).eq.3)then

C Display ctlstr for day and period << convert to multi days >>.
        IY=ITCTL(ICOMP)
        if(IY.eq.-14)then
          ctlstr(icomp,1)='infil only: wind speed  '
        elseif(IY.eq.-13)then
          ctlstr(icomp,1)='infil only: ext db temp '
        elseif(IY.eq.-12)then
          ctlstr(icomp,1)='infil only: adj zone tmp'
        elseif(IY.eq.-11)then
          ctlstr(icomp,1)='infil only: zone temp   '
        elseif(IY.eq.-4)then
          ctlstr(icomp,1)='vent only: wind speed   '
        elseif(IY.eq.-3)then
          ctlstr(icomp,1)='vent only: ext db temp  '
        elseif(IY.eq.-2)then
          ctlstr(icomp,1)='vent only: adj zone temp'
        elseif(IY.eq.-1)then
          ctlstr(icomp,1)='vent only: zone temp    '
        elseif(IY.eq.4)then
          ctlstr(icomp,1)='infil & vent: wind speed'
        elseif(IY.eq.3)then
          ctlstr(icomp,1)='infil & vent: ext db T  '
        elseif(IY.eq.2)then
          ctlstr(icomp,1)='infil & vent: adj zone T'
        elseif(IY.eq.1)then
          ctlstr(icomp,1)='infil & vent: zone T    '
        elseif(IY.eq.0)then
          ctlstr(icomp,1)='no control of air flow  '
        else
          ctlstr(icomp,1)='unknown flow control    '
        endif
        write(outs,'(a,a)') ' Control: ',ctlstr(icomp,1)
        call edisp(itru,outs)

        if(IY.eq.0) goto 1225
        WRITE(outs,'(A,3F7.2)')' Lower/Middle/High temp setpoints: ',
     &    TLO(ICOMP),TUP(ICOMP),THI(ICOMP)
        call edisp(itru,outs)
        write(outs,'(20x,a,a)')'Infil. ac/h m^3/s  Vent. ac/h m^3/s ',
     &    ' from  data'
        call edisp(itru,outs)
        acilm = (VOL(ICOMP)*ACIL(ICOMP))/3600.0
        acvlm = (VOL(ICOMP)*ACVL(ICOMP))/3600.0
        WRITE(outs,'(A,F9.3,F8.4,F10.3,F8.4,I5,F10.3)')
     &    ' Lower range data ',ACIL(ICOMP),acilm,
     &    ACVL(ICOMP),acvlm,IVL(ICOMP),TAL(ICOMP)
        call edisp(itru,outs)

        acium = (VOL(ICOMP)*ACIU(ICOMP))/3600.0
        acvum = (VOL(ICOMP)*ACVU(ICOMP))/3600.0
        WRITE(outs,'(A,F9.3,F8.4,F10.3,F8.4,I5,F10.3)')
     &    ' Middle range data',ACIU(ICOMP),acium,
     &    ACVU(ICOMP),acvum,IVU(ICOMP),TAU(ICOMP)
        call edisp(itru,outs)

        acihm = (VOL(ICOMP)*ACIH(ICOMP))/3600.0
        acvhm = (VOL(ICOMP)*ACVH(ICOMP))/3600.0
        WRITE(outs,'(A,F9.3,F8.4,F10.3,F8.4,I5,F10.3)')
     &    ' High range data  ',ACIH(ICOMP),acihm,
     &    ACVH(ICOMP),acvhm,IVH(ICOMP),TAH(ICOMP)
        call edisp(itru,outs)

      endif

 1225 CONTINUE

C Write header for scheduled air flow.
      call edisp(itru,' ')
      call edisp(itru,'Scheduled air infiltration and ventilation: ')
      WRITE(outs,'(2A)')
     &'Daytype    Period     Infiltration     Ventilation     ',
     &'From Source'
      call edisp(itru,outs)
      WRITE(outs,'(2A)')
     &'           id Hours   Rate ac/h m3/s   Rate ac/h m3/s  ',
     &'Zone DegC'
      call edisp(itru,outs)

      DO 400 IDTY=1,NBDAYTYPE
        IF(NAC(IDTY).EQ.0)GOTO 400
        DO 1230 I=1,NAC(IDTY)
          acim = (VOL(ICOMP)*ACI(IDTY,I))/3600.0
          acvm = (VOL(ICOMP)*ACV(IDTY,I))/3600.0
          WRITE(outs,'(a,i2,i3,a,i2,F8.2,F8.4,F8.2,F8.4,I4,F9.2)')
     &      calentag(idty)(1:10),I,IACS(IDTY,I),' - ',IACF(IDTY,I),
     &      ACI(IDTY,I),acim,ACV(IDTY,I),acvm,IPT(IDTY,I),TA(IDTY,I)
          call edisp(itru,outs)
1230    CONTINUE
 400  CONTINUE
      RETURN
      END


C ****************** CASINF 
C CASINFO provides an English description of scheduled casual gains
C from zone operation common block data.
      SUBROUTINE CASINF(ICOMP,ITRU)

#include "building.h"
#include "schedule.h"

      common/caleni/nbdaytype,nbcaldays(MDTY),icalender(365)
      INTEGER NBDAYTYPE,NBCALDAYS,ICALENDER,IDTY
      common/calena/calename,calentag(MDTY),calendayname(MDTY)
      CHARACTER CALENAME*32,CALENTAG*12,CALENDAYNAME*32

      COMMON/P3N/NCAS(MDTY),ICGS(MDTY,MC),ICGF(MDTY,MC),
     &CMGS(MDTY,MC),CMGL(MDTY,MC),RADC(MDTY,MC),CONC(MDTY,MC)
      INTEGER NCAS,ICGS,ICGF
      REAL CMGS,CMGL,RADC,CONC

C Electrical data flag and elctrical data.
      common/elecflg/ielf(mcom)
      COMMON/ELP3N/NEL(MDTY),PF(MDTY,MC),IPF(MDTY,MC),PWR(MDTY,MC),
     &BVOLT(MDTY,MC),IPHAS(MDTY,MC)
      INTEGER NEL,IPF,IPHAS
      REAL PF,PWR,BVOLT

      CHARACTER outs*124
      CHARACTER llbl*9

      call edisp(itru,' ')
      call edisp(itru,'Notes: ')
      call edisp248(itru,oprdesc(icomp),72)
      call edisp(itru,' ')
      if(IELF(ICOMP).EQ.1)then
        write(outs,'(a,a)')'Daytype   Gain Type    Period Sensible ',
     &    ' Latent  Radiant  Convec p.f +/-  Power Voltage Phase'
        call edisp(itru,outs)
        write(outs,'(a,a)')'          No.  label   Hours  Magn.(W) ',
     &    ' Magn.(W) Frac     Frac    (-) (-)    (W)    (V) (rgb)'
        call edisp(itru,outs)
      else  
        write(outs,'(a,a)')'Daytype   Gain Type    Period Sensible ',
     &    ' Latent     Radiant    Convec'
        call edisp(itru,outs)
        write(outs,'(a,a)')'          No.  label   Hours  Magn.(W) ',
     &    ' Magn.(W)   Fraction   Fraction'
        call edisp(itru,outs)
      endif
      DO 600 IDTY=1,NBDAYTYPE
        if(NCAS(IDTY).eq.0) goto 600

        IF(NCAS(IDTY).GT.0)THEN
          IF(IELF(ICOMP).EQ.1) THEN
            DO 1269 I=1,NCAS(IDTY)
              if(ICGT(IDTY,I).gt.0)then
                write(llbl,'(a,a)')lodlabel(ICOMP,ICGT(IDTY,I)),'W  '
              elseif(ICGT(IDTY,I).eq.-1)then
                write(llbl,'(a,a)')lodlabel(ICOMP,iabs(ICGT(IDTY,I))),
     &            'm2p'
              elseif(ICGT(IDTY,I).lt.-1)then
                write(llbl,'(a,a)')lodlabel(ICOMP,iabs(ICGT(IDTY,I))),
     &            'Wm2'
              endif
              WRITE(outs,5361)calentag(idty)(1:10),I,llbl,ICGS(IDTY,I),
     &        '-',ICGF(IDTY,I),CMGS(IDTY,I),CMGL(IDTY,I),RADC(IDTY,I),
     &        CONC(IDTY,I),PF(IDTY,I),
     &        IPF(IDTY,I),PWR(IDTY,I),BVOLT(IDTY,I),IPHAS(IDTY,I)
5361          FORMAT(a,I3,1x,A9,I3,a,I2,F8.1,F8.1,F9.1,F9.1,
     &          1x,F7.2,I3,F7.1,F7.1,I3)
              call edisp(itru,outs)
1269        CONTINUE
          ELSE
            DO 1270 I=1,NCAS(IDTY)
              if(ICGT(IDTY,I).gt.0)then
                write(llbl,'(a,a)')lodlabel(ICOMP,ICGT(IDTY,I)),'W  '
              elseif(ICGT(IDTY,I).eq.-1)then
                write(llbl,'(a,a)')lodlabel(ICOMP,iabs(ICGT(IDTY,I))),
     &            'm2p'
              elseif(ICGT(IDTY,I).lt.-1)then
                write(llbl,'(a,a)')lodlabel(ICOMP,iabs(ICGT(IDTY,I))),
     &            'Wm2'
              endif
              WRITE(outs,5362)calentag(idty)(1:10),I,llbl,ICGS(IDTY,I),
     &        '-',ICGF(IDTY,I),CMGS(IDTY,I),CMGL(IDTY,I),RADC(IDTY,I),
     &        CONC(IDTY,I)
5362          FORMAT(a,I3,1x,A9,I3,a,I2,F9.1,F9.1,F11.2,F11.2)
              call edisp(itru,outs)
1270        CONTINUE
          ENDIF
          if(NBDAYTYPE.gt.3) call edisp(itru,' ')  ! if long list
        ENDIF
 600  CONTINUE
      RETURN
      END


C ************* checkcascount
C checkcascount scans current P3 & P3TYPE commons and refreshes 
C loadcount() & load24() in common blocks loadcnt & loadall
      subroutine checkcascount(icomp)
#include "building.h"
#include "schedule.h"

C Parameters
      integer icomp  ! index of the zone to scan

      COMMON/OUTIN/IUOUT,IUIN
      common/caleni/nbdaytype,nbcaldays(MDTY),icalender(365)
      INTEGER NBDAYTYPE,NBCALDAYS,ICALENDER,IDTY

      COMMON/P3N/NCAS(MDTY),ICGS(MDTY,MC),ICGF(MDTY,MC),
     &CMGS(MDTY,MC),CMGL(MDTY,MC),RADC(MDTY,MC),CONC(MDTY,MC)
      INTEGER NCAS,ICGS,ICGF
      REAL CMGS,CMGL,RADC,CONC

C loadcount(zone,gaintype,daytype) - nb of each gain type (whether
C   absolute or W/m2).
C loadm2count(zone,gaintype,daytype) - nb of each m2 gain types.
C loadmixed(zone,gaintype,daytype) - if zero then nothing, if one
C   then all non-zero are absolute, if two then all (non-zero) are in
C   W/m2 or person/m2, if three then non-zero are mixed.
C Does each gain type on each day:
C   start at zero load24(gaintype,daytype,1)=.true.
C   and end at 24 hours (load24(gaintype,daytype,2)=.true.

      common/loadcnt/loadcount(mcom,MGTY,MDTY),
     &  loadm2count(mcom,MGTY,MDTY),loadmixed(mcom,MGTY,MDTY)
      common/loadall/load24(MGTY,MDTY,2)
      logical load24

C Keep track of how many non-zero positive and negative casual
C gain types - in order to figure out value of loadmixed.
      dimension loadnonzero(7,MDTY),loadm2nonzero(7,MDTY)

C Zero the loadcount array for the current zone.
      ic=icomp
      do 40 ij=1,7
        DO 50 IDTY=1,NBDAYTYPE
          loadcount(ic,ij,IDTY)=0
          loadm2count(ic,ij,IDTY)=0
          loadnonzero(ij,IDTY)=0
          loadm2nonzero(ij,IDTY)=0
          loadmixed(ic,ij,IDTY)=0
          load24(ij,IDTY,1)=.false.
          load24(ij,IDTY,2)=.false.
 50     CONTINUE
 40   continue

C Debug.
C      write(6,*) 'Nb of periods for occ/lt/equip for each day type'

      DO 600 IDTY=1,NBDAYTYPE
      if(ncas(IDTY).gt.0)then
        do 41 ij=1,ncas(IDTY)
          if(icgt(IDTY,ij).eq.1)then
            loadcount(ic,1,IDTY)=loadcount(ic,1,IDTY)+1
            if(CMGS(IDTY,ij).gt.0.0)loadnonzero(1,1)=loadnonzero(1,1)+1
            if(ICGS(IDTY,ij).eq.0)load24(1,IDTY,1)=.true. 
            if(ICGF(IDTY,ij).eq.24)load24(1,IDTY,2)=.true. 
          elseif(icgt(IDTY,ij).eq.2)then
            loadcount(ic,2,IDTY)=loadcount(ic,2,IDTY)+1
            if(CMGS(IDTY,ij).gt.0.0)loadnonzero(2,1)=loadnonzero(2,1)+1
            if(ICGS(IDTY,ij).eq.0)load24(2,IDTY,1)=.true. 
            if(ICGF(IDTY,ij).eq.24)load24(2,IDTY,2)=.true. 
          elseif(icgt(IDTY,ij).eq.3)then
            loadcount(ic,3,IDTY)=loadcount(ic,3,IDTY)+1
            if(CMGS(IDTY,ij).gt.0.0)loadnonzero(3,1)=loadnonzero(3,1)+1
            if(ICGS(IDTY,ij).eq.0)load24(3,IDTY,1)=.true. 
            if(ICGF(IDTY,ij).eq.24)load24(3,IDTY,2)=.true. 
          elseif(icgt(IDTY,ij).eq.4)then
            loadcount(ic,4,IDTY)=loadcount(ic,4,IDTY)+1
            if(CMGS(IDTY,ij).gt.0.0)loadnonzero(4,1)=loadnonzero(4,1)+1
            if(ICGS(IDTY,ij).eq.0)load24(4,IDTY,1)=.true. 
            if(ICGF(IDTY,ij).eq.24)load24(4,IDTY,2)=.true. 
          elseif(icgt(IDTY,ij).eq.5)then
            loadcount(ic,5,IDTY)=loadcount(ic,5,IDTY)+1
            if(CMGS(IDTY,ij).gt.0.0)loadnonzero(5,1)=loadnonzero(5,1)+1
            if(ICGS(IDTY,ij).eq.0)load24(5,IDTY,1)=.true. 
            if(ICGF(IDTY,ij).eq.24)load24(5,IDTY,2)=.true. 
          elseif(icgt(IDTY,ij).eq.-1)then
            loadcount(ic,1,IDTY)=loadcount(ic,1,IDTY)+1
            loadm2count(ic,1,IDTY)=loadm2count(ic,1,IDTY)+1
            if(CMGS(IDTY,ij).gt.0.0)
     &      loadm2nonzero(1,1)=loadm2nonzero(1,1)+1
            if(ICGS(IDTY,ij).eq.0)load24(1,IDTY,1)=.true. 
            if(ICGF(IDTY,ij).eq.24)load24(1,IDTY,2)=.true. 
          elseif(icgt(IDTY,ij).eq.-2)then
            loadcount(ic,2,IDTY)=loadcount(ic,2,IDTY)+1
            loadm2count(ic,2,IDTY)=loadm2count(ic,2,IDTY)+1
            if(CMGS(IDTY,ij).gt.0.0)
     &      loadm2nonzero(2,1)=loadm2nonzero(2,1)+1
            if(ICGS(IDTY,ij).eq.0)load24(2,IDTY,1)=.true. 
            if(ICGF(IDTY,ij).eq.24)load24(2,IDTY,2)=.true. 
          elseif(icgt(IDTY,ij).eq.-3)then
            loadcount(ic,3,IDTY)=loadcount(ic,3,IDTY)+1
            loadm2count(ic,3,IDTY)=loadm2count(ic,3,IDTY)+1
            if(CMGS(IDTY,ij).gt.0.0)
     &      loadm2nonzero(3,1)=loadm2nonzero(3,1)+1
            if(ICGS(IDTY,ij).eq.0)load24(3,IDTY,1)=.true. 
            if(ICGF(IDTY,ij).eq.24)load24(3,IDTY,2)=.true. 
          endif
 41     continue

C Determine if any of the casual gain types were mixed.
        if(loadnonzero(1,1).eq.0.and.loadm2nonzero(1,1).eq.0)then
          loadmixed(ic,1,IDTY)=0
        elseif(loadnonzero(1,1).gt.0.and.loadm2nonzero(1,1).eq.0)then
          loadmixed(ic,1,IDTY)=1
        elseif(loadnonzero(1,1).eq.0.and.loadm2nonzero(1,1).gt.0)then
          loadmixed(ic,1,IDTY)=2
        elseif(loadnonzero(1,1).ne.loadm2nonzero(1,1))then
          loadmixed(ic,1,IDTY)=3
        endif
        if(loadnonzero(2,1).eq.0.and.loadm2nonzero(2,1).eq.0)then
          loadmixed(ic,2,IDTY)=0
        elseif(loadnonzero(2,1).gt.0.and.loadm2nonzero(2,1).eq.0)then
          loadmixed(ic,2,IDTY)=1
        elseif(loadnonzero(2,1).eq.0.and.loadm2nonzero(2,1).gt.0)then
          loadmixed(ic,2,IDTY)=2
        elseif(loadnonzero(2,1).ne.loadm2nonzero(2,1))then
          loadmixed(ic,2,IDTY)=3
        endif
        if(loadnonzero(3,1).eq.0.and.loadm2nonzero(3,1).eq.0)then
          loadmixed(ic,3,IDTY)=0
        elseif(loadnonzero(3,1).gt.0.and.loadm2nonzero(3,1).eq.0)then
          loadmixed(ic,3,IDTY)=1
        elseif(loadnonzero(3,1).eq.0.and.loadm2nonzero(3,1).gt.0)then
          loadmixed(ic,3,IDTY)=2
        elseif(loadnonzero(3,1).ne.loadm2nonzero(3,1))then
          loadmixed(ic,3,IDTY)=3
        endif
        if(loadnonzero(4,1).gt.0)loadmixed(ic,4,IDTY)=1
        if(loadnonzero(5,1).gt.0)loadmixed(ic,5,IDTY)=1
      endif
 600  continue

C Debug.
C      write(6,*) ' daytype number ',IDTY,
C     & ' loadcount loadm2count loadmixed'
C      write(6,*)loadcount(ic,1,1),loadcount(ic,2,1),loadcount(ic,3,1),
C     & loadcount(ic,4,1),loadcount(ic,5,1)
C      write(6,*)loadm2count(ic,1,1),loadm2count(ic,2,1),
C     &  loadm2count(ic,3,1)
C      write(6,*)loadmixed(ic,1,1),loadmixed(ic,2,1),loadmixed(ic,3,1),
C     &  loadmixed(ic,4,1),loadmixed(ic,5,1)

      return
      end

C ******************* CPYCASIJ ***********************
C CPYCASIJ Copy one casual period jx to another ix for zone icomp and 
C daytype idaytype. 

      SUBROUTINE CPYCASIJ(icomp,idaytype,ix,jx)
#include "building.h"
#include "schedule.h"

C Parameters
      integer icomp    ! current zone
      integer idaytype ! day type to sort
      integer ix       ! is the destination
      integer jx       ! is the source

      COMMON/P3N/NCAS(MDTY),ICGS(MDTY,MC),ICGF(MDTY,MC),
     &CMGS(MDTY,MC),CMGL(MDTY,MC),RADC(MDTY,MC),CONC(MDTY,MC)
      INTEGER NCAS,ICGS,ICGF,IDTY
      REAL CMGS,CMGL,RADC,CONC

C Electrical data flag and elctrical data.
      common/elecflg/ielf(mcom)
      COMMON/ELP3N/NEL(MDTY),PF(MDTY,MC),IPF(MDTY,MC),PWR(MDTY,MC),
     &BVOLT(MDTY,MC),IPHAS(MDTY,MC)
      INTEGER NEL,IPF,IPHAS
      REAL PF,PWR,BVOLT

      IDTY=IDAYTYPE
      ICGT(IDTY,ix)=ICGT(IDTY,jx)
      ICGS(IDTY,ix)=ICGS(IDTY,jx)
      ICGF(IDTY,ix)=ICGF(IDTY,jx)
      CMGS(IDTY,ix)=CMGS(IDTY,jx)
      CMGL(IDTY,ix)=CMGL(IDTY,jx)
      RADC(IDTY,ix)=RADC(IDTY,jx)
      CONC(IDTY,ix)=CONC(IDTY,jx)
      if(ielf(icomp).ne.0)then
        ipf(IDTY,ix)=ipf(IDTY,jx)
        iphas(IDTY,ix)=iphas(IDTY,jx)
        pf(IDTY,ix)=pf(IDTY,jx)
        pwr(IDTY,ix)=pwr(IDTY,jx)
        bvolt(IDTY,ix)=bvolt(IDTY,jx)
      endif
      return
      end


C ******************* CPYCASIT ***********************
C CPYCASIT Copy one casual period jx to backup variables for zone icomp 
C and daytype idaytype. 
      SUBROUTINE CPYCASIT(icomp,idaytype,jx,icgtX,icgsX,icgfX,cmgsX,
     &  cmglX,radcX,concX,ipfX,iphasX,pfX,pwrX,bvoltX)
#include "building.h"
#include "schedule.h"

C Parameters
      integer icomp    ! current zone
      integer idaytype ! day type to sort
      integer jx       ! is the source
      integer icgtX,icgsX,icgfX    ! temporary variables
      real cmgsX,cmglX,radcX,concX ! temporary variables
      integer ipfX,iphasX          ! temporary variables
      real pfX,pwrX,bvoltX         ! temporary variables

      COMMON/P3N/NCAS(MDTY),ICGS(MDTY,MC),ICGF(MDTY,MC),
     &CMGS(MDTY,MC),CMGL(MDTY,MC),RADC(MDTY,MC),CONC(MDTY,MC)
      INTEGER NCAS,ICGS,ICGF
      REAL CMGS,CMGL,RADC,CONC

C Electrical data flag and elctrical data.
      common/elecflg/ielf(mcom)
      COMMON/ELP3N/NEL(MDTY),PF(MDTY,MC),IPF(MDTY,MC),PWR(MDTY,MC),
     &BVOLT(MDTY,MC),IPHAS(MDTY,MC)
      INTEGER NEL,IPF,IPHAS
      REAL PF,PWR,BVOLT

      IDTY=IDAYTYPE
      ICGTX=ICGT(IDTY,jx)
      ICGSX=ICGS(IDTY,jx)
      ICGFX=ICGF(IDTY,jx)
      CMGSX=CMGS(IDTY,jx)
      CMGLX=CMGL(IDTY,jx)
      RADCX=RADC(IDTY,jx)
      CONCX=CONC(IDTY,jx)
      if(ielf(icomp).ne.0)then
        ipfX=ipf(IDTY,jx)
        iphasX=iphas(IDTY,jx)
        pfX=pf(IDTY,jx)
        pwrX=pwr(IDTY,jx)
        bvoltX=bvolt(IDTY,jx)
      else
        ipfX=0
        iphasX=1
        pfX=0.0
        pwrX=0.0
        bvoltX=0.0
      endif
      return
      end

C ******************* CPYCASTI ***********************
C CPYCASTI - Copy from backup variables to casual period jx for zone icomp 
C and daytype idaytype. 
      SUBROUTINE CPYCASTI(icomp,idaytype,jx,icgtX,icgsX,icgfX,cmgsX,
     &  cmglX,radcX,concX,ipfX,iphasX,pfX,pwrX,bvoltX)
#include "building.h"
#include "schedule.h"

C Parameters
      integer icomp    ! current zone
      integer idaytype ! day type to sort
      integer jx       ! is the source
      integer icgtX,icgsX,icgfX    ! temporary variables
      real cmgsX,cmglX,radcX,concX ! temporary variables
      integer ipfX,iphasX          ! temporary variables
      real pfX,pwrX,bvoltX         ! temporary variables

      COMMON/P3N/NCAS(MDTY),ICGS(MDTY,MC),ICGF(MDTY,MC),
     &CMGS(MDTY,MC),CMGL(MDTY,MC),RADC(MDTY,MC),CONC(MDTY,MC)
      INTEGER NCAS,ICGS,ICGF
      REAL CMGS,CMGL,RADC,CONC

C Electrical data flag and elctrical data.
      common/elecflg/ielf(mcom)
      COMMON/ELP3N/NEL(MDTY),PF(MDTY,MC),IPF(MDTY,MC),PWR(MDTY,MC),
     &BVOLT(MDTY,MC),IPHAS(MDTY,MC)
      INTEGER NEL,IPF,IPHAS
      REAL PF,PWR,BVOLT

      IDTY=IDAYTYPE
      ICGT(IDTY,jx)=ICGTX
      ICGS(IDTY,jx)=ICGSX
      ICGF(IDTY,jx)=ICGFX
      CMGS(IDTY,jx)=CMGSX
      CMGL(IDTY,jx)=CMGLX
      RADC(IDTY,jx)=RADCX
      CONC(IDTY,jx)=CONCX
      if(ielf(icomp).ne.0)then
        ipf(IDTY,jx)=ipfX
        iphas(IDTY,jx)=iphasX
        pf(IDTY,jx)=pfX
        pwr(IDTY,jx)=pwrX
        bvolt(IDTY,jx)=bvoltX
      else
        ipf(IDTY,jx)=0
        iphas(IDTY,jx)=1
        pf(IDTY,jx)=0.0
        pwr(IDTY,jx)=0.0
        bvolt(IDTY,jx)=0.0
      endif
      return
      end

C ********************* PROCESSOLDCAS *********************
C PROCESSOLDCAS: Processes gains in an old format operations file.
C Passed the zone index.
      SUBROUTINE PROCESSOLDCAS(ICOMP,ITRC,ITRU)
#include "building.h"
#include "geometry.h"
#include "schedule.h"

      COMMON/OUTIN/IUOUT,IUIN
      common/caleni/nbdaytype,nbcaldays(MDTY),icalender(365)
      INTEGER NBDAYTYPE,NBCALDAYS,ICALENDER,idty,idt
      common/calena/calename,calentag(MDTY),calendayname(MDTY)
      CHARACTER CALENAME*32,CALENTAG*12,CALENDAYNAME*32

      COMMON/P3N/NCAS(MDTY),ICGS(MDTY,MC),ICGF(MDTY,MC),
     &CMGS(MDTY,MC),CMGL(MDTY,MC),RADC(MDTY,MC),CONC(MDTY,MC)
      INTEGER NCAS,ICGS,ICGF
      REAL CMGS,CMGL,RADC,CONC

C Commons for electrical data
      COMMON/ELECFLG/IELF(MCOM)
      COMMON/ELP3N/NEL(MDTY),PF(MDTY,MC),IPF(MDTY,MC),PWR(MDTY,MC),
     &BVOLT(MDTY,MC),IPHAS(MDTY,MC)
      INTEGER NEL,IPF,IPHAS,istarthr,ifinishr,ip
      REAL PF,PWR,BVOLT

C loadcount(zone,gaintype,daytype) - nb of each gain type (whether
C   absolute or W/m2). Up to MGTY casual gain types are supported.
C loadm2count(zone,gaintype,daytype) - nb of each m2 gain types.
C loadmixed(zone,gaintype,daytype) - if zero then nothing, if one
C   then all non-zero are absolute, if two then all (non-zero) are in
C   W/m2 or person/m2, if three then non-zero are mixed.
      common/loadcnt/loadcount(mcom,MGTY,MDTY),
     &  loadm2count(mcom,MGTY,MDTY),loadmixed(mcom,MGTY,MDTY)

      logical close,needtodivide,writeit

      character outs*124

C Temporary arrays: MC array is sized for 24 hours X all gain types X 
C all day types.
C CMGSA total sensible W for each hour and gain type. 
C CMGSA total latent W for each hour and gain type). 
C RADCA total radiant casual W for the zone at each hour and gain type
C CONCA total conv casual W for the zone at each hour and gain type
C RADFAC implied radiant fraction RADCA/CMGSA
C CONFAC implied convective fraction CONCA/CMGSA
C ICGTYP the casual gain type (remembering negative index).
C PERSN keeps track of the original number of people/m2.
      real CMGSA,CMGLA,RADCA,CONCA,RADFAC,CONFAC,PERSN
      integer ICGTYP,LNCAS
      dimension CMGSA(MC,MGTY),CMGLA(MC,MGTY),RADCA(MC,MGTY)
      dimension CONCA(MC,MGTY),RADFAC(MC,MGTY),CONFAC(MC,MGTY)
      dimension ICGTYP(MC,MGTY),PERSN(MC,MGTY),LNCAS(MDTY)

C Logical for testing whether there is any difference in casual gains.
      logical bCMGSA,bCMGLA,bRADCA,bCONCA,bRADFAC,bCONFAC

C Temporary arrays for period electrical data.
C pfA power factor, ipfA leading/lagging, pwrA real power W,
C bvoltA voltage, iphasA phases assoc with electrical casual gain. 
C aparva is apparent power component VA.
C aparvar is reactive power component VAr.
      dimension pfA(mc,MGTY),ipfA(mc,MGTY),pwrA(mc,MGTY),bvoltA(mc,MGTY)
      dimension iphasA(mc,MGTY),aparva(mc,MGTY),aparvar(mc,MGTY)

C Logical for testing whether there is any difference in electrical gains.
      logical bpfA,bpwrA,bbvoltA
      real floor

C Warn user if the zone base area is still zero.
      call eclose(ZBASEA(ICOMP),0.0,0.01,close)
      if(close)then
        FLOOR=1.0
        write(outs,'(3a,F6.1,a)') 'The base area of ',
     &    zname(icomp)(1:lnzname(icomp)),
     &    ' has been set to ',FLOOR,'m^2.'
        call edisp(iuout,outs)
      else
        FLOOR=ZBASEA(ICOMP)
        write(outs,'(3a,F6.1,a)') 'The base area of ',
     &    zname(icomp)(1:lnzname(icomp)),
     &    ' is',FLOOR,'m^2.'
        call edisp(iuout,outs)
      endif

C Remember how many of each.
      DO 600 IDTY=1,NBDAYTYPE
        LNCAS(IDTY)=NCAS(IDTY)
 600  CONTINUE

C Find how many of each casual gain type for each day type as well
C as whether each profile needs to be absolute (casual gain type positive),
C or based on area (casual gain type negative) or if mixed +-.
      call checkcascount(icomp)
      
C Zero derived gains in the temporary array and set the type as per
C the call to checkcascount.
      do 130 IP=1,MC
        do 120 IG=1,MGTY
          CMGSA(IP,IG)=0.0
          CMGLA(IP,IG)=0.0
          RADCA(IP,IG)=0.0
          CONCA(IP,IG)=0.0
          RADFAC(IP,IG)=0.50
          CONFAC(IP,IG)=0.50
          DO 609 IDTY=1,NBDAYTYPE
            ISTARTHR=(IDTY-1)*24+1
            IFINISHR=IDTY*24
            if(ip.ge.ISTARTHR.and.ip.le.IFINISHR)then
              if(loadmixed(icomp,ig,IDTY).eq.2)then
                ICGTYP(IP,IG)= (-1)*IG
              else
                ICGTYP(IP,IG)= IG
              endif
            endif
 609      CONTINUE
          PERSN(IP,IG)=0.0
          pfA(IP,IG)=0.0
          ipfA(IP,IG)=0
          pwrA(IP,IG)=0.0
          bvoltA(IP,IG)=0.0
          iphasA(IP,IG)=0
          aparva(IP,IG)=0.0
          aparvar(IP,IG)=0.0
 120    continue
 130  continue

C For every hour in a day check if one of the casual gains
C is applicable. Time is on the half hour to make it easy to
C test for a casual gain period being between T1 and T2.
      DO 617 IDTY=1,NBDAYTYPE
      ISTARTHR=(IDTY-1)*24+1
      IFINISHR=IDTY*24
      time = (-0.5)
      do 200 ip=ISTARTHR,IFINISHR
        time = time +1.0
        if (NCAS(IDTY).gt.0) then
          do 210 I=1,NCAS(IDTY)
            T1=real(ICGS(IDTY,I))
            T2=real(ICGF(IDTY,I))
            ITYP=abs(ICGT(IDTY,I))
            if(time.gt.T1.and.time.lt.T2)then
              if (ICGT(IDTY,I).gt.0) then
                CMGSA(IP,ITYP)=CMGSA(IP,ITYP)+CMGS(IDTY,I)
                CMGLA(IP,ITYP)=CMGLA(IP,ITYP)+CMGL(IDTY,I)
                RADCA(IP,ITYP)=RADCA(IP,ITYP)+CMGS(IDTY,I)*RADC(IDTY,I)
                CONCA(IP,ITYP)=CONCA(IP,ITYP)+CMGS(IDTY,I)*CONC(IDTY,I)
                if(CMGSA(IP,ITYP).gt.0.0)then
                  RADFAC(IP,ITYP)= RADCA(IP,ITYP)/CMGSA(IP,ITYP)
                  CONFAC(IP,ITYP)= CONCA(IP,ITYP)/CMGSA(IP,ITYP)
                endif
                if (IELF(ICOMP).GT.0) then

C Sum real power for this gain type and hour. Use PRECOMP to
C calculate apparent power and reactive power.
                  PR=PWR(IDTY,I)
                  pwrA(IP,ITYP)= pwrA(IP,ITYP) + PR
                  call PRECOMP(PR,PF(IDTY,I),IPF(IDTY,I),PA,PQ)
                  aparva(IP,ITYP) = aparva(IP,ITYP) + PA
                  aparvar(IP,ITYP) = aparvar(IP,ITYP) + PQ
                  denom = sqrt((pwrA(IP,ITYP) * pwrA(IP,ITYP)) +
     &                         (aparvar(IP,ITYP) *aparvar(IP,ITYP)))
                  if(denom.gt.0.0)then
                    pfA(IP,ITYP)= pwrA(IP,ITYP) / denom
                  else
                    pfA(IP,ITYP)= PF(IDTY,I)
                  endif
                  ipfA(IP,ITYP)= IPF(IDTY,I)
                  bvoltA(IP,ITYP)= BVOLT(IDTY,I)
                  iphasA(IP,ITYP)= IPHAS(IDTY,I)
                endif
              elseif (ICGT(IDTY,I).eq.-1) then

C If -1 and m2/person is zero, the intention is no occupancy.
                call eclose(CMGS(IDTY,I),0.00,0.001,close)
                if (close) then
                  PERSN(IP,ITYP)=0.0
                else
                  PERSN(IP,ITYP)=FLOOR/CMGS(idty,I)
                endif
                CMGSA(IP,ITYP)=CMGSA(IP,ITYP) + PERSN(IP,ITYP)*95.0
                CMGLA(IP,ITYP)=CMGLA(IP,ITYP) + PERSN(IP,ITYP)*45.0
                RADCA(IP,ITYP)=RADCA(IP,ITYP) + PERSN(IP,ITYP)*95.0*
     &            RADC(IDTY,I)
                CONCA(IP,ITYP)=CONCA(IP,ITYP) + PERSN(IP,ITYP)*95.0*
     &            CONC(IDTY,I)
                if(CMGSA(IP,ITYP).gt.0.0)then
                  RADFAC(IP,ITYP)= RADCA(IP,ITYP)/CMGSA(IP,ITYP)
                  CONFAC(IP,ITYP)= CONCA(IP,ITYP)/CMGSA(IP,ITYP)
                endif
              elseif (ICGT(IDTY,I).lt.-1) then
                CMGSA(IP,ITYP)=CMGSA(IP,ITYP) + CMGS(IDTY,I)*FLOOR
                CMGLA(IP,ITYP)=CMGLA(IP,ITYP) + CMGL(IDTY,I)*FLOOR
                RADCA(IP,ITYP)=RADCA(IP,ITYP) + CMGS(IDTY,I)*
     &            RADC(IDTY,I)*FLOOR
                CONCA(IP,ITYP)=CONCA(IP,ITYP) + CMGS(IDTY,I)*
     &            CONC(IDTY,I)*FLOOR
                if(CMGSA(IP,ITYP).gt.0.0)then
                  RADFAC(IP,ITYP)= RADCA(IP,ITYP)/CMGSA(IP,ITYP)
                  CONFAC(IP,ITYP)= CONCA(IP,ITYP)/CMGSA(IP,ITYP)
                endif
                if (IELF(ICOMP).GT.0) then

C Sum real power for this gain type and hour. Use PRECOMP to
C calculate apparent power and reactive power.
                  PR=PWR(IDTY,I)*FLOOR
                  pwrA(IP,ITYP)= pwrA(IP,ITYP) + PR
                  call PRECOMP(PR,PF(IDTY,I),IPF(IDTY,I),PA,PQ)
                  aparva(IP,ITYP) = aparva(IP,ITYP) + PA
                  aparvar(IP,ITYP) = aparvar(IP,ITYP) + PQ
                  denom = sqrt((pwrA(IP,ITYP) * pwrA(IP,ITYP)) +
     &                         (aparvar(IP,ITYP) *aparvar(IP,ITYP)))
                  if(denom.gt.0.0)then
                    pfA(IP,ITYP)= pwrA(IP,ITYP) / denom
                  else
                    pfA(IP,ITYP)= PF(IDTY,I)
                  endif
                  ipfA(IP,ITYP)= IPF(IDTY,I)
                  bvoltA(IP,ITYP)= BVOLT(IDTY,I)
                  iphasA(IP,ITYP)= IPHAS(IDTY,I)
                endif
              endif
            endif
 210      continue
        endif
 200  continue
 617  CONTINUE

C Do post processing...
C Zero the number of gains and look for start and end periods.
      IDT=0
 5000 IDT=IDT+1
      ISTARTHR=(IDT-1)*24+1
      IFINISHR=IDT*24
      NCASwk=0
      NELwk=0

C For each casual gain type that had any periods (via an initial call
C to checkcascount and subsequent checks of n1/n2/n3 etc.) and weekday hour...
      if(itrc.ge.1)then
        write(itru,*) 'loadcount ',loadcount(icomp,1,IDT),
     &    loadcount(icomp,2,IDT),loadcount(icomp,3,IDT),
     &    loadcount(icomp,4,IDT),loadcount(icomp,5,IDT),
     &    loadm2count(icomp,1,IDT),loadm2count(icomp,2,IDT),
     &    loadm2count(icomp,3,IDT)
        write(itru,*) 'loadmix  ',loadmixed(icomp,1,IDT),
     &    loadmixed(icomp,2,IDT),loadmixed(icomp,3,IDT)
      endif
      do 300 ITYP=1,MGTY
        if(loadcount(icomp,ityp,IDT).eq.0)goto 300

C Test if casual gain index is mixed loadmixed(icomp,ityp,IDT)==2) in this schedule.
        needtodivide=.false.
        if(loadmixed(icomp,ityp,IDT).eq.2)needtodivide=.true.
        ihst=0
        ihcg=0 

C Loop thru first 24 hours, testing for differences if ip <= 23. On
C the 24th step, if the finish period is not 24 then also write out
C a final period for the current casual gain type. 
        do 310 ip=istarthr,ifinishr
          writeit=.false.
          if(ip.le.IFINISHR-1)then
            call ECLOSE(CMGSA(IP,ITYP),CMGSA(IP+1,ITYP),0.001,bCMGSA)
            call ECLOSE(CMGLA(IP,ITYP),CMGLA(IP+1,ITYP),0.001,bCMGLA)
            call ECLOSE(RADCA(IP,ITYP),RADCA(IP+1,ITYP),0.001,bRADCA)
            call ECLOSE(CONCA(IP,ITYP),CONCA(IP+1,ITYP),0.001,bCONCA)
            call ECLOSE(RADFAC(IP,ITYP),RADFAC(IP+1,ITYP),0.001,bRADFAC)
            call ECLOSE(CONFAC(IP,ITYP),CONFAC(IP+1,ITYP),0.001,bCONFAC)
            call ECLOSE(pfA(IP,ITYP),pfA(IP+1,ITYP),0.001,bpfA)
            call ECLOSE(pwrA(IP,ITYP),pwrA(IP+1,ITYP),0.001,bpwrA)
            call ECLOSE(bvoltA(IP,ITYP),bvoltA(IP+1,ITYP),0.001,bbvoltA)
            if(bCMGSA.and.bCMGLA.and.bRADCA.and.bCONCA.and.bRADFAC.and.
     &         bCONFAC.and.bpfA.and.bpwrA.and.bbvoltA)then
              continue
            else
              writeit=.true.
            endif
          elseif(ip.eq.IFINISHR.and.ihcg.ne.24)then
            writeit=.true.
          endif
          if(writeit)then
            NCASwk=NCASwk +1
            ihcg=ip-(ISTARTHR-1)

C Adjust for area based, for m2 per person just divide floor area by
C the number of persons to get back to the original CMGSA. For the
C case of mixed absolute and per unit area persn might be zero so
C trap for this case and set CMGSA to zero.
            if(needtodivide)then
              if(ICGTYP(IP,ITYP).eq.-1)then
                if(PERSN(IP,ITYP).gt.0.0)then
                  CMGSA(IP,ITYP) = FLOOR/PERSN(IP,ITYP)
                  CMGLA(IP,ITYP) = 0.0
                else
                  CMGSA(IP,ITYP) = 0.0
                  CMGLA(IP,ITYP) = 0.0
                endif
              elseif(ICGTYP(IP,ITYP).lt.-1)then
                CMGSA(IP,ITYP) = CMGSA(IP,ITYP)/FLOOR
                CMGLA(IP,ITYP) = CMGLA(IP,ITYP)/FLOOR
                if (IELF(ICOMP).GT.0) then
                  pwrA(IP,ITYP) = pwrA(IP,ITYP)/FLOOR
                endif
              endif
            endif

C Assign values.
            NCAS(IDT) = NCASwk
            ICGS(IDT,NCASwk) = ihst
            ICGF(IDT,NCASwk) = ihcg
            CMGS(IDT,NCASwk) = CMGSA(IP,ITYP)
            CMGL(IDT,NCASwk) = CMGLA(IP,ITYP)
            RADC(IDT,NCASwk) = RADFAC(IP,ITYP)
            CONC(IDT,NCASwk) = CONFAC(IP,ITYP)
            ICGT(IDT,NCASwk) = ICGTYP(IP,ITYP)

C If there is electrical data then also assign these values (note:
C electrical power is assumed to use the same units as the casual gain).
C And if there is no power at the timestep reset PF to zero.
            if (IELF(ICOMP).GT.0) then
              NELwk = NELwk + 1
              call eclose(pwrA(IP,ITYP),0.00,0.001,close)
              if(close)then
                PF(IDT,NELwk) = 0.0
              else
                PF(IDT,NELwk) = pfA(IP,ITYP)
              endif
              IPF(IDT,NELwk) = ipfA(IP,ITYP)
              PWR(IDT,NELwk) = pwrA(IP,ITYP)
              BVOLT(IDT,NELwk) = bvoltA(IP,ITYP)
              IPHAS(IDT,NELwk) = iphasA(IP,ITYP)
              if(itrc.ge.1)then
                WRITE(itru,5470,IOSTAT=IOS,ERR=1)ICGTYP(IP,ITYP),ihst,
     &            ihcg,CMGSA(IP,ITYP),CMGLA(IP,ITYP),RADFAC(IP,ITYP),
     &            CONFAC(IP,ITYP),pfA(IP,ITYP),ipfA(IP,ITYP),
     &            pwrA(IP,ITYP),bvoltA(IP,ITYP),iphasA(IP,ITYP)
              endif
            else
              if(itrc.ge.1)then
                WRITE(itru,5460,IOSTAT=IOS,ERR=1)ICGTYP(IP,ITYP),ihst,
     &          ihcg,CMGSA(IP,ITYP),CMGLA(IP,ITYP),RADFAC(IP,ITYP),
     &          CONFAC(IP,ITYP)
              endif
            endif
5460        FORMAT(1X,3(I4,','),F9.1,',',F9.1,',',F6.3,',',F6.3)
5470        FORMAT(1X,3(I3,','),F7.1,',',F7.1,',',F4.1,',',F4.1,
     &      ',',F5.2,',',I2,',',F7.1,',',F7.1,',',I2)
            ihst=ihcg
          endif
 310    continue
 300  continue
      if(itrc.ge.1)then
        write(itru,*) 'periods detected ',NCASwk
      endif

C See if any periods have been added. 
      if(LNCAS(IDT).ne.NCAS(IDT))
     &  write(outs,'(2A)')' Updated daytype: ',calentag(idt)
      IF(IDT.LT.NBDAYTYPE)GOTO 5000
      return

  1   if(IOS.eq.2)then
        call edisp(iuout,
     &    'PROCESSOLDCAS: permission error writing strings.')
      else
        call edisp(iuout,
     &    'PROCESSOLDCAS: error writing warning or strings.')
      endif
      return
      end

c ******************** PRECOMP ********************
C copy of code from esrubld/precal.F
C PRECOMP calculates the apparent and reactive rectangular component
C of a power comsuming load.
C Inputs are:
C   PWR - Real power consumption in W
C   PF  - Power factor of load (0.-1.)
C   IPF - Determines whether load in leading 1 lagging -1 or unity 0
C Outputs are:
C   PA - Apparent power component VA
C   PQ - Reactive power component +/- VAr
C Lagging currents return a positive Q Leading currents 
C return a negative value of Q.

      subroutine PRECOMP(PWRx,PFx,IPFx,PAx,PQx)
      
      real PWRx,PFx,PAx,PQx
      logical closer

      call eclose(PFx,0.00,0.0001,closer)
      if(closer)then
        PFx = 1.0
        PAx = PWRx
        PQx = 0.0
      else

C Calculate the phase angle from PF in rads
        PAx=ACOS(PFx)
      
C Calculate the rectangular co-ordinates
        PQx=PWRx*TAN(PAx)*real(IPFx)
        PAx=sqrt(PWRx**2+PQx**2)
      endif
      
      RETURN
      END


C ******************* checksort *********************
C checksort does a quick check of casual gains for sorted state.
C not extensive, but should catch most issues.
      subroutine checksort(icomp,idaytype,problem)
#include "building.h"
#include "schedule.h"

      COMMON/OUTIN/IUOUT,IUIN

C How many of each gain type is there each zone/gaintype/daytype.
      common/loadcnt/loadcount(mcom,MGTY,MDTY),
     &  loadm2count(mcom,MGTY,MDTY),loadmixed(mcom,MGTY,MDTY)

C Does each gain type on each day:
C   start at zero load24(gaintype,daytype,1)=.true.
C   and end at 24 hours (load24(gaintype,daytype,2)=.true.

      common/loadall/load24(MGTY,MDTY,2)
      logical load24
      integer idty

C logical problem set to true if daytype might not be sorted.
      logical problem
      character outs*124

      IDTY=IDAYTYPE
      if(ip3ver(icomp).eq.0)then
        write(outs,*) 'older format operations file in zone ',icomp
        call edisp(iuout,outs)
        problem = .true.
        return
      endif

C Check how many casual gains there are for each type on each day type.
      call checkcascount(icomp)

C How many occupant/lights/small power? << revise for more gain types >>
      n1=loadcount(icomp,1,IDTY)
      n2=loadcount(icomp,2,IDTY)
      n3=loadcount(icomp,3,IDTY)

C See if first period is 0hr or 1hr and last period is 24.
      if(n1.gt.0.and.(.NOT.load24(1,idaytype,1)))then
        write(outs,*)'initial occupant period not zero in zone ',icomp,
     &    ' daytype ',idaytype
        call edisp(iuout,outs)
        problem = .true.
      endif
      if(n2.gt.0.and.(.NOT.load24(2,idaytype,1)))then
        write(outs,*)'initial lighting period not zero in zone ',icomp,
     &    ' daytype ',idaytype
        call edisp(iuout,outs)
        problem = .true.
      endif
      if(n3.gt.0.and.(.NOT.load24(3,idaytype,1)))then
        write(outs,*)'initial small pwr period not zero in zone ',icomp,
     &    ' daytype ',idaytype
        call edisp(iuout,outs)
        problem = .true.
      endif
      if(n1.gt.0.and.(.NOT.load24(1,idaytype,2)))then
        write(outs,*) 'last occupant period not zero in zone ',icomp,
     &    ' daytype ',idaytype
        call edisp(iuout,outs)
        problem = .true.
      endif
      if(n2.gt.0.and.(.NOT.load24(2,idaytype,2)))then
        write(outs,*) 'last lighting period not zero in zone ',icomp,
     &    ' daytype ',idaytype
        call edisp(iuout,outs)
        problem = .true.
      endif
      if(n3.gt.0.and.(.NOT.load24(3,idaytype,2)))then
        write(outs,*) 'last small pwr period not zero in zone ',icomp,
     &    ' daytype ',idaytype
        call edisp(iuout,outs)
        problem = .true.
      endif

      return
      end


C ******************** UPDOPR 
C UPDOPR     Updates all operations data when day types are incremented

      SUBROUTINE UPDOPR
#include "building.h"
#include "model.h"
#include "schedule.h"

      COMMON/OUTIN/IUOUT,IUIN
      common/FILEP/IFIL
      
      integer ncomp,ncon
      COMMON/C1/NCOMP,NCON
      common/caleni/nbdaytype,nbcaldays(MDTY),icalender(365)
      INTEGER NBDAYTYPE,NBCALDAYS,ICALENDER
      common/pophelp/h(60)
      COMMON/P2N/NAC(MDTY),IACS(MDTY,MA),IACF(MDTY,MA),ACI(MDTY,MA),
     &ACV(MDTY,MA),IPT(MDTY,MA),TA(MDTY,MA)
      INTEGER NAC,IACS,IACF,IPT
      COMMON/P3N/NCAS(MDTY),ICGS(MDTY,MC),ICGF(MDTY,MC),
     &CMGS(MDTY,MC),CMGL(MDTY,MC),RADC(MDTY,MC),CONC(MDTY,MC)
      INTEGER NCAS,ICGS,ICGF
      REAL CMGS,CMGL,RADC,CONC
      COMMON/ELP3N/NEL(MDTY),PF(MDTY,MC),IPF(MDTY,MC),PWR(MDTY,MC),
     &BVOLT(MDTY,MC),IPHAS(MDTY,MC)
      INTEGER NEL,IPF,IPHAS
      REAL PF,PWR,BVOLT

      INTEGER ITRC,ITRU,IUO,ICOMP,IER,ISTAT,NBDAYTYPEHOLD
      CHARACTER H*72
      LOGICAL XST

C Initialise local variables
      ITRC=0
      ITRU=0
      IUO=IFIL+1
      IER=0
      ISTAT=0
      XST=.FALSE.
      NBDAYTYPEHOLD=NBDAYTYPE  ! remember NBDAYTYPE

       H(1)='Zone operations files that have been defined in this '
       H(2)='model need to be updated in order to reflect the new'
       H(3)='day type just added. You will be given the option to '
       H(4)='update (overwrite) the existing operations files.'
       H(5)='Choose to overwrite (default option) or save giving'
       H(6)='new names. If you choose to give new names then choose'
       H(7)='to remember the new file name, otherwise the model will'
       H(8)='be corrupted because operations files will not have the'
       H(9)='new day types.'

C For every zone in the model do the following...
      DO 100 ICOMP=1,NCOMP

C Check if operations file exists
        INQUIRE (FILE=LPROJ(ICOMP),EXIST=XST)
        IF(XST)THEN

C Display help 
          IF(ICOMP.EQ.1)CALL PHELPD('Information',9,'-',0,0,IER)

C Read operations file for this zone
          NBDAYTYPE=NBDAYTYPE-1
          CALL ERPFREE(IUO,ISTAT)
          CALL EROPER(ITRC,ITRU,IUO,ICOMP,IER)
          NBDAYTYPE=NBDAYTYPE+1
          IF(IER.NE.0)THEN
            CALL EDISP(IUOUT,'UPDOPR Error reading file')
            RETURN
          ENDIF

C Initialise the air flows and casual gains to zero for new day type
          NAC(NBDAYTYPE)=0
          DO 200 I=NAC(NBDAYTYPE-1),MA
            IACS(NBDAYTYPE,I)=0
            IACF(NBDAYTYPE,I)=0
            ACI(NBDAYTYPE,I)=0.
            ACV(NBDAYTYPE,I)=0.
            IPT(NBDAYTYPE,I)=0
            TA(NBDAYTYPE,I)=0.
 200      CONTINUE
          NCAS(NBDAYTYPE)=3
          DO 300 I=NCAS(NBDAYTYPE-1),MC
            ICGT(NBDAYTYPE,I)=0
            ICGS(NBDAYTYPE,I)=0
            ICGF(NBDAYTYPE,I)=0
            CMGS(NBDAYTYPE,I)=0.
            CMGL(NBDAYTYPE,I)=0.
            RADC(NBDAYTYPE,I)=0.
            CONC(NBDAYTYPE,I)=0.
            PF(NBDAYTYPE,I)=0.
            IPF(NBDAYTYPE,I)=0
            PWR(NBDAYTYPE,I)=0.
            BVOLT(NBDAYTYPE,I)=0.
            IPHAS(NBDAYTYPE,I)=0
 300      CONTINUE

C Make initial three gain periods (0-24) for occupancy, lights and small
C power. 
          DO 400 I=1,NCAS(NBDAYTYPE)
            ICGT(NBDAYTYPE,I)=I
            ICGF(NBDAYTYPE,I)=24
 400      CONTINUE

C Write zone operations file.
          ip3ver(icomp)=2
          CALL EMKOPER(IUO,LPROJ(ICOMP),ICOMP,IER)
        ENDIF
 100  CONTINUE
      NBDAYTYPE=NBDAYTYPEHOLD  ! restore the origianl nb of day types
      RETURN
      END
