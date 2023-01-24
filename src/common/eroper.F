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

C eroper.f provides the following facilities:
C  EROPER:   Reads all zone schedules from a user-constructed
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
C PROCESSOLDCAS: Processes casual gains to remove overlaps in periods.
C PRECOMP    Calculates the apparent and reactive rectangular component
C            of a power comsuming load.
C checksort  Does a quick check of casual gains for sorted state.
C SORTCAS    Sort an array of casual gains by casual gain type and then by
C            starting time. (Uses a odified QUICKSORT).
C UPDOPR     Updates all operations data when day types are incremented
C PIKCGTYP   Offer user choice of currently defined casual gain types.

C ******************** EROPER 
C EROPER reads zone schedules from a user-constructed datafile.

C Common block variables are:

C oprdesc      - operation notes (248 char)

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

C ICGT         - Casual gain type:
C                 1. Occupancy
C                 2. Lights
C                 3. Equipment
C                 4. User controllable gain (not fully utilised as yet)
C                -1. Occupancy as floor area per person
C                -2. Lights in w/m^2 per floor area
C                -3. Equipment in w/m^2 per floor area
C
C               Extra types for use with HOT3000 central and zonal base loads:
C                 Type 1 with bH3KExtentionsActive: BCD-App (Appliances - linked with BCD file)
C                 Type 2 with bH3KExtentionsActive: BCD-Lights (Lights linked with BCD file)
C                 Type 3 with bH3KExtentionsActive: BCD-OElec (Other Electrical linked with BCD file)
C                 Type 4 with bH3KExtentionsActive: BCD-Occ (Occupants linked with BCD file)

C lodlabel     - user supplied label for a gain
C caskeytype   - key word for gain see below 

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
C Maximum number of casual gain periods (MC) set in building.h

      SUBROUTINE EROPER(ITRC,ITRU,IUO,ICOMP,IER)
#include "building.h"
#include "model.h"
#include "schedule.h"
#include "espriou.h"
#include "bc_data.h"
      
      integer lnblnk  ! function definition

      common/caleni/nbdaytype,nbcaldays(MDTY),icalender(365)
      INTEGER NBDAYTYPE,NBCALDAYS,ICALENDER
      common/calena/calename,calentag(MDTY),calendayname(MDTY)
      CHARACTER CALENAME*32,CALENTAG*12,CALENDAYNAME*32
      COMMON/OUTIN/IUOUT,IUIN,IEOUT
      common/warncounter/iecount

C Session log file (for errors etc.)
      character iefile*72
      common/logs/iefile
      
      integer ncomp,ncon
      COMMON/C1/NCOMP,NCON

C Key word (caskeytype) for each casual gain slot to assign it a type
C (if ip3ver is >= 21). As follows:
C `-` not yet defined units are W and W/m2,
C `people` basic occupant units are W and m2/person, 
C 'lighting' basic lighting units are W and W/m2,
C `equipment` basic small power units are W and W/m2,
C 'other' as '-' non-specific casual gains units are W and W/m2
C `dynamic_people` occupant with metabolic units are number of people and met rate, 
C `net_utilities` same as legacy icgtyp = 5 (see ?? for documentation),
C 'ALOtherElectric' used with H3K other appliances,
C 'ALStoveNG' used with H3K stoves using natural gas,
C 'ALStoveElectric' used with H3K electrical stoves,
C 'ALDryer' used with H3K clothes dryers

C Future type examples
C `transpiraton` vegitation or latent sources, `waterdraw` DHW draws,
C `roomfan` future linked to flow component
      character caskeytypeper*16  ! for future use with ip3ver 30
      common/p3typekey/caskeytypeper(MDTY,MC)

C----------------------------------------------------------------------
C Common for data associated with HOT3000 central and zonal Base Loads.
C - Bart, Aug 28 2010
      common / H3K_BL_data / iH3K_Base_Loads(MCOM),
     &                       iBCD_column(MCOM,MDTY,MC),
     &                       fBL_fraction(MCOM,MDTY,MC),
     &                       nGain_types(MCOM,MDTY),
     &                       iGain_type(MCOM,MDTY,MC)

      integer iH3K_Base_Loads, iBCD_column, nGain_types, iGain_type
      real fBL_fraction
C----------------------------------------------------------------------

C Function determining if HOT3000 features enabled.
      logical bH3KExtentionsActive

C Common block P2UNIT defines the units for infiltration and ventilation
C for each zone and day type. The default value of zero is the same as
C the (implied) unit in the version (ip3ver 0, 1, 2). A value of
C of one signals m3/h and a value of 2 signals m3/s. << not yet used >>
      integer INFUNIT,IVENTUNIT
      COMMON/P2UNIT/INFUNIT(MCOM,MDTY),IVENTUNIT(MCOM,MDTY)

C Electrical data flag and elctrical data.
      common/elecflg/ielf(mcom)
      COMMON/ELP3N/NEL(MDTY),PF(MDTY,MC),IPF(MDTY,MC),PWR(MDTY,MC),
     &BVOLT(MDTY,MC),IPHAS(MDTY,MC)
      INTEGER NEL,IPF,IPHAS
      REAL PF,PWR,BVOLT
      logical isdynamic       ! true if reading a dynamic_people  

C isdynamicocup signals the dynamic people model per zone:
C 0 = no dynamic people
C 1 = CIBSE polynomial
C 2 = ASHRAE polynomial
C 3 = two-node model
C 4 = segmented model
      common/dynamico/isdynamicocup(MCOM)

C Error subroutine and error details for dll mode.
      common/dllerr/dllsubr,dllmesg

      CHARACTER outstr*124,outs*124,outs2*124
      character dllsubr*12,dllmesg*124,WORD*20,loutstr*248
      character dstmp*24
      character vctlstr*24,comment*76
      logical havehi,dll
      logical doupgrade       ! Flag that upgrade to v21 is needed.
      integer idaytype        ! Incremented for each new *day entry.
      integer ijk,ireader
      integer lsn             ! Length of currentfile.
      real reader
      integer irange,icur     ! for looping
      integer implieddaytype  ! counter for implied day types
      
      ijk=0
      ireader=0
      reader=0.0
      iH3K_Base_Loads(icomp) = 0
      implieddaytype = 0

C Check if running in dll mode.
      call isadll(dll)
      
      isdynamicocup(icomp)=0

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

C Debug.
C      write(6,*) '170 lodslot ',icomp,':',
C     & (lodslot(icomp,iph),iph=1,7)

C Read lines from file, discarding comments. Look for header on
C newer files. Older files will begin with a description. To allow for
C description with spaces copy directly from OUTSTR rather than parsing
C it into words.
 
      CALL STRIPC(IUO,OUTSTR,0,ND,1,'oper name or header',IER)
      IF(IER.NE.0) goto 1002
      if(outstr(1:11).eq.'*Operations')then
        doupgrade=.false.
        if(outstr(13:15).eq.'1.0')then
          ip3ver(icomp)=1; doupgrade=.true.
        elseif(outstr(13:15).eq.'2.0')then
          ip3ver(icomp)=2; doupgrade=.true.
        elseif(outstr(13:15).eq.'2.1')then
          ip3ver(icomp)=21
        elseif(outstr(13:15).eq.'3.0')then
          ip3ver(icomp)=30
        endif

C Ancient operation files assumed 3 day types. Mark doupgrade if there
C are more than 3 day types.
        if(nbdaytype.gt.3.and.ip3ver(icomp).lt.2) doupgrade=.true.
        CALL STRIPC(IUO,OUTSTR,0,ND,1,'header',IER)
        IF(IER.NE.0) goto 1002
        K=0
        CALL EGETW(OUTSTR,K,WORD,'W','header tags',IER)
        IF(IER.NE.0) goto 1002
        if(WORD(1:5).eq.'*Date'.or.WORD(1:5).eq.'*date')then
          CALL EGETRM(OUTSTR,K,dstmp,'W','date stamp',IER)
          if(IER.ne.0) IER=0  ! missing date stamp not fatal
        endif

C If ip3ver(icomp) is less than 21 then read the combined
C documentation (casual gains and air schedules). If equal to
C 21 read only the ventdesc here. If > 21 read the token
C *air_schedules line and then the infiltration/ventilation
C documentation.
        if(ip3ver(icomp).lt.21)then
          CALL LSTRIPC(IUO,LOUTSTR,0,ND,1,'oper notes',IER)
          oprdesc(icomp)=LOUTSTR
          ventdesc(icomp)=LOUTSTR
        elseif(ip3ver(icomp).eq.21)then
          CALL LSTRIPC(IUO,LOUTSTR,0,ND,1,'oper notes',IER)
          ventdesc(icomp)=LOUTSTR
        elseif(ip3ver(icomp).gt.21)then
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

C We have a really old file which has documentation line first so
C use the string buffer from the initial STRIPC call.
        oprdesc(icomp)=OUTSTR
        ventdesc(icomp)=OUTSTR
        ip3ver(icomp)=0
      endif

C If ip3ver(icomp) is less than or equal to 21 then set assumed
C units for air movement to ach. If ip3ver(icomp) is >= 30 then
C the pair of units needs to be established for each day type. 
      if(ip3ver(icomp).le.21)then

C << put in a loop for each day type >> 
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
        call ventctlstr(icomp,1,'S',vctlstr,comment)
        ctlstr(icomp,1)=vctlstr
        IY=ITCTL(ICOMP)
        if(IY.eq.0)then
          IJK=1
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

C Air change information. For each of the globally defined
C day types read flow data. If we get to *casual before we
C have reached nbdaytype then fill in place-holders.
  202   DO 400 IDTY=1,NBDAYTYPE
          CALL STRIPC(IUO,OUTSTR,0,ND,1,'flow periods',IER)
          if(OUTSTR(1:7).eq.'*Casual'.or.OUTSTR(1:7).eq.'*casual')then
            if(implieddaytype.lt.NBDAYTYPE)then
              i=NBDAYTYPE
              NAC(I)=1; IACS(I,1)=0; IACF(I,1)=24
              ACI(I,1)=0.0; ACV(I,1)=0.0; IPT(I,1)=0; TA(I,1)=0.0
              backspace(iuo)   ! unread the current line of the file
              goto 400
            endif
          endif
          K=0
          CALL EGETWI(OUTSTR,K,ireader,0,MA,'W','periods',IER)
          NAC(IDTY)=IREADER
          implieddaytype=implieddaytype+1  ! increment
          IF(NAC(IDTY).EQ.0)goto 400
          DO 10 I=1,NAC(IDTY)
            CALL STRIPC(IUO,OUTSTR,99,ND,1,'Period flow details',IER)

C If there are six items on the line then we have flow period data.
C However if there are more than 6 it is a casual gain. Logic is
C not robust for dealing with older files so warm user to update
C to a newer format first.
            if(ND.gt.6)then
              call edisp(iuout,' ')
              call edisp(iuout,
     &        'This older format operation file has fewer day types')
              call edisp(iuout,
     &        'Than are included in the model. It should be updated')
              call edisp(iuout,'first.')
              ier=1
              CALL ERPFREE(IUO,ISTAT)
              return
            endif
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
            if(IPT(IDTY,I).EQ.ICOMP)then
              write(outs,'(2a)')
     &          ' Vent not from `another` zone in ...',
     &           outstr(1:50)
              call edisp(iuout,outs)
            endif
 10       CONTINUE
 400    CONTINUE
        IF(IER.NE.0) goto 1002

      elseif(ip3ver(icomp).ge.30)then

C Newer format file. Thermostatic control data.
        havehi = .false.
        idaytype=0
        iperiod=0
        islot=0    ! assume no casual slots have been filled
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

C Flow reporting if requested.
          if(ITRC.GE.1)CALL VENTINF(ICOMP,ITRU)
          goto 77

        elseif(WORD(1:7).eq.'*Casual'.or.WORD(1:7).eq.'*casual')then
          CALL LSTRIPC(IUO,LOUTSTR,0,ND,1,'oper casual notes',IER)
          oprdesc(icomp)=LOUTSTR
          goto 77

        elseif(WORD(1:5).eq.'*Type'.or.WORD(1:5).eq.'*type')then

C Read high level attributes of the casual gains used in the zone
C such as user label, keyword, index that the period lines can use
C and two integer attributes to be used in post-processing.
C << update for dynamic people >>
          islot=islot+1   ! increment the casual gain slot
          CALL EGETW(OUTSTR,K,lodlabel(ICOMP,ISLOT),'W',
     &      'user casual gain label',IER) 
          CALL EGETW(OUTSTR,K,caskeytype(ICOMP,ISLOT),'W',
     &      'casual gain type keyword',IER) 
          if(caskeytype(ICOMP,ISLOT)(1:13).eq.'dynamicpeople') ! old file version
     &      caskeytype(ICOMP,ISLOT)='dynamic_people'
          CALL EGETWI(OUTSTR,K,lodslot(icomp,ISLOT),0,99,'W',
     &      'casual gain slot position',IER)
          CALL EGETWI(OUTSTR,K,lodatr1(icomp,ISLOT),0,99,'W',
     &      'casual gain atribute 1',IER)
          CALL EGETWI(OUTSTR,K,lodatr2(icomp,ISLOT),0,99,'W',
     &      'casual gain atribute 2',IER)
          goto 77   ! read another line

        elseif(WORD(1:9).eq.'*End_type'.or.
     &         WORD(1:9).eq.'*end_type')then
          continue   ! we are done with this section
        endif
      endif

C For ip3ver = 21 read section about casual types.
      if(ip3ver(icomp).ge.21)then
        CALL LSTRIPC(IUO,LOUTSTR,0,ND,1,'start of casual',IER)
        if(loutstr(1:7).eq.'*casual')then
          CALL LSTRIPC(IUO,LOUTSTR,0,ND,1,'oper casual notes',IER)
          oprdesc(icomp)=LOUTSTR
        else
          oprdesc(icomp)=LOUTSTR
        endif

C Read high level attributes of the casual gains used in the zone
C such as user label, keyword, index that the period lines can use
C and two integer attributes to be used in post-processing.

C << do we need to track how many slots have been filled so we can
C << later check that the period data points to a known slot?
        islot=0
  42    CALL STRIPC(IUO,OUTSTR,0,ND,1,'*type or *end_type',IER)
        if(outstr(1:5).eq.'*type')then
          islot=islot+1   ! increment the casual gain slot
          K=6
          CALL EGETW(OUTSTR,K,lodlabel(ICOMP,ISLOT),'W',
     &      'user casual gain label',IER) 
          CALL EGETW(OUTSTR,K,caskeytype(ICOMP,ISLOT),'W',
     &      'casual gain keyword',IER) 
          CALL EGETWI(OUTSTR,K,lodslot(icomp,ISLOT),0,99,'W',
     &      'casual gain slot position',IER)
          CALL EGETWI(OUTSTR,K,lodatr1(icomp,ISLOT),0,99,'W',
     &      'casual gain atribute 1',IER)
          CALL EGETWI(OUTSTR,K,lodatr2(icomp,ISLOT),0,99,'W',
     &      'casual gain atribute 2',IER)
          goto 42   ! read another line
        elseif(outstr(1:9).eq.'*end_type')then
          continue   ! we are done with this section
        endif
      endif

C Casual gains for different day types.
      implieddaytype = 0
 72   DO 600 IDTY=1,NBDAYTYPE
        CALL STRIPC(IUO,OUTSTR,0,ND,1,'gain periods for day type',IER)
        if(IER.ne.0)then

C We reached the end of the file before the global number of day types
C has been scanned. Fill in the place holder values. These will be
C included the next time the file is written.
          i=NBDAYTYPE
          NCAS(I)=3
          ICGS(I,1)=0; ICGF(I,1)=24; ICGUnit(I,1)=0
          CMGS(I,1)=0.0; CMGL(I,1)=0.0
          RADC(I,1)=0.6; CONC(I,1)=0.4  ! CIBSE Guide A 6.3
          ICGT(I,1)=1
          ICGS(I,2)=0; ICGF(I,2)=24; ICGUnit(I,2)=0
          CMGS(I,2)=0.0; CMGL(I,2)=0.0
          RADC(I,2)=0.3; CONC(I,2)=0.7  ! CIBSE Table A 6.5
          ICGT(I,2)=2 
          ICGS(I,3)=0; ICGF(I,3)=24; ICGUnit(I,3)=0
          CMGS(I,3)=0.0; CMGL(I,3)=0.0
          RADC(I,3)=0.4; CONC(I,3)=0.6
          ICGT(I,3)=3
          ier=0      ! reset error state
          goto 600   ! jump to post-processing`
        endif
        K=0
        CALL EGETWI(OUTSTR,K,IREADER,0,MC,'W','gain periods',IER)
        NCAS(IDTY)=IREADER
        implieddaytype=implieddaytype+1  ! increment
        nGain_types(ICOMP, IDTY) = IREADER     ! used for HOT3000 base loads 
        IF(NCAS(IDTY).EQ.0)goto 600
        DO 40 I=1,NCAS(IDTY)
          isdynamic=.false.
          CALL STRIPC(IUO,OUTSTR,99,ND,1,'Period gain detail',IER)
          numberofitems=ND

C If there were 3 items on the line it could be 'Occupt Lights Equipt'
C and if there is only 1 item on the line it is number of periods.
          if(numberofitems.eq.1.or.numberofitems.eq.3)then
            ii=NBDAYTYPE
            NCAS(II)=3
            ICGS(II,1)=0; ICGF(II,1)=24; ICGUnit(II,1)=0
            CMGS(II,1)=0.0; CMGL(II,1)=0.0
            RADC(II,1)=0.6; CONC(II,1)=0.4  ! CIBSE Guide A 6.3
            ICGT(II,1)=1
            ICGS(II,2)=0; ICGF(II,2)=24; ICGUnit(II,2)=0
            CMGS(II,2)=0.0; CMGL(II,2)=0.0
            RADC(II,2)=0.3; CONC(II,2)=0.7  ! CIBSE Table A 6.5
            ICGT(II,2)=2 
            ICGS(II,3)=0; ICGF(II,3)=24; ICGUnit(II,3)=0
            CMGS(II,3)=0.0; CMGL(II,3)=0.0
            RADC(II,3)=0.4; CONC(II,3)=0.6
            ICGT(II,3)=3
            backspace(iuo)   ! unread the current line of the file
            goto 40
          endif

C If dynamic_people then read into a different common block.
C For newer files the first item will be an overloaded casual gain index.
C The units implied depend on the value of ip3ver. For ip3ver <21 a 
C positive ICGT implies the unit is Watts. 
C If negative the unit is m2/p for occupants and W/m2 otherwise. 
C Ip3ver 21+ will already have scanned *type lines. and the lodslot
C value will be the same as iabs(ICGT).
          K=0
          if(numberofitems.ge.7)then
            CALL EGETWI(OUTSTR,K,IREADER,-3,7,'W',
     &        'overloaded gain type',IER)
            ICGT(IDTY,I)=IREADER
            iGain_type(ICOMP, IDTY,I) = IREADER ! used for HOT3000 base loads         
            ipslot=iabs(ireader)  ! get equivalent of lodslot
            if(ip3ver(icomp).lt.21)then
              if(ireader.gt.0)then
                ICGUnit(IDTY,I)=0
              elseif(ireader.eq.-1)then
                ICGUnit(IDTY,I)=2
              else
                ICGUnit(IDTY,I)=1
              endif
            elseif(ip3ver(icomp).eq.21)then
              if(ireader.gt.0)then
                if(caskeytype(icomp,ipslot)(1:14).eq.
     &             'dynamic_people')then
                  ICGUnit(IDTY,I)=3  ! number of people
                  isdynamic=.true.
                  isdynamicocup(icomp)=lodatr1(icomp,ipslot)
                else
                  ICGUnit(IDTY,I)=0
                  isdynamic=.false.
                endif
              elseif(ireader.lt.0)then
                if(caskeytype(icomp,ipslot)(1:14).eq.
     &             'dynamic_people')then
                  ICGUnit(IDTY,I)=3  ! number of people
                  isdynamic=.true.
                  isdynamicocup(icomp)=lodatr1(icomp,ipslot)
                elseif(caskeytype(icomp,ipslot)(1:6).eq.'people')then
                  ICGUnit(IDTY,I)=2
                  isdynamic=.false.
                else
                  ICGUnit(IDTY,I)=1
                endif
              endif
            else
C << still to be done for ip3ver >21 >>
            endif
          else
            ICGT(IDTY,I)=1    ! assume slot one if unspecified
            ICGUnit(IDTY,I)=0 ! assume unit is W
          endif
          
C Debug.
C          write(6,*) 'eroper 552 ',idty,i,ireader,ICGT(IDTY,I),
C     &      ICGUnit(IDTY,I)

C If dynamic_people then read into a different common block
          if(isdynamic)then
            CALL EGETWI(OUTSTR,K,IREADER,0,24,'W','gain st',IER)
            ICGS(IDTY,I)=IREADER
            CALL EGETWI(OUTSTR,K,IREADER,0,24,'W','gain fn',IER)
            ICGF(IDTY,I)=IREADER
            CMGS(IDTY,I)=0.0
            CMGL(IDTY,I)=0.0
            CALL EGETWR(OUTSTR,K,READER,0.,0.,'-','nb men',IER)
            tnbmen(IDTY,I)=READER
            CALL EGETWR(OUTSTR,K,READER,0.,0.,'-','nb women',IER)
            tnbwomen(IDTY,I)=READER
            CALL EGETWR(OUTSTR,K,READER,0.,0.,'-','nb children',IER)
            tnbchild(IDTY,I)=READER
            CALL EGETWR(OUTSTR,K,READER,0.,0.,'-','metabolic',IER)
            metabolic(IDTY,I)=READER
            CALL EGETWR(OUTSTR,K,READER,0.,0.,'-','clo',IER)
            clov(IDTY,I)=READER
            CALL EGETWR(OUTSTR,K,READER,0.,0.,'-','air velocity',IER)
            airvel(IDTY,I)=READER
            CALL EGETWR(OUTSTR,K,READER,0.,1.,'W','gain rad fr',IER)
            RADC(IDTY,I)=READER
            CALL EGETWR(OUTSTR,K,READER,0.,1.,'W','gain conv fr',IER)
            CONC(IDTY,I)=READER
          else
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
          end if

C Do reality checks on data thus far.
          IF(ICGS(IDTY,I).GT.ICGF(IDTY,I))then

C Found a period out of order, warn user and carry on.
            write(loutstr,'(6a)')' daytype ',calentag(idty),
     &        ' gains start-end mismatch in...',
     &        outstr(1:50),' of operation file ',
     &        currentfile(1:lnblnk(currentfile))
            call edisp248(iuout,loutstr,100)
            IER=1
          endif
          X=RADC(IDTY,I)+CONC(IDTY,I)
          IF(X.GT.1.1)then
            write(loutstr,'(6a)') ' day type ',calentag(idty),
     &        ' rad & conv fractions > 1.0 in...',
     &        outstr(1:50),' of operation file ',
     &        currentfile(1:lnblnk(currentfile))
            call edisp248(iuout,loutstr,100)
            ier=1
          endif
          IF(X.LT..95.AND.ITRC.GT.1)call edisp(iuout,
     &        ' Casual gain rad:con sum < 1.0')
          
C Check if electrical data has also been saved.
          if(.NOT.isdynamic.and.numberofitems.gt.7)then
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
            IELF(ICOMP)=1  ! Make note of electrical data inclusion.
          endif
   40   CONTINUE
        IF(IER.NE.0) goto 1002
 600  CONTINUE

C Check to see if gain labels have been added to the end of the file
C for ip3ver < 21. Loop to see the magintude of icgt already referenced.
      irange=0
      do 701 IDTY=1,NBDAYTYPE
        if (NCAS(IDTY).GT.0) then
          do I = 1,NCAS(IDTY)
            icur=ICGT(IDTY,I)
            if(iabs(icur).gt.irange) irange=iabs(icur)
          enddo
        endif
 701  continue

C For older files now assume there are always at least 3 different labels.
C Test if number of labels matches what was found when reading the period lines.
      if(ip3ver(icomp).lt.21)then
        if(irange.lt.3) irange=3
      endif

      if(ip3ver(icomp).lt.21)then
        CALL STRIPC(IUO,OUTSTR,99,ND,1,'gain labels',IERV)
        IF(IERV.ne.0) THEN
          goto 1001
        ELSE

C Warn user if the expected lables are not found.
          if(irange.ne.nd)then
            write(loutstr,'(a,i2,4a)')
     &        'The number of unique casual gain labels ',irange,
     &        ' differs from tokens on the line...',
     &        outstr(1:50),' of operation file ',
     &        currentfile(1:lnblnk(currentfile))
            call edisp248(iuout,loutstr,100)
            ier=2
          endif

C Gather each of the user labels. Depending on the file version
C guess or assign caskeytype(). lodslot is the same as ISOLT
C and the two attributes are zero.
          K=0
          DO 1234 ISLOT=1,irange
            CALL EGETW(OUTSTR,K,lodlabel(ICOMP,ISLOT),'W',
     &         'gain label',IER) 
            lodslot(icomp,ISLOT)=ISLOT; lodatr1(icomp,ISLOT)=0
            lodatr2(icomp,ISLOT)=0
            if(lodlabel(ICOMP,ISLOT)(1:6).eq.'Occupt')then
              caskeytype(icomp,ISLOT)='people       '
            elseif(lodlabel(ICOMP,ISLOT)(1:6).eq.'Lights')then
              caskeytype(icomp,ISLOT)='lighting     '
            elseif(lodlabel(ICOMP,ISLOT)(1:6).eq.'Equipt')then
              caskeytype(icomp,ISLOT)='equipment'
            elseif(lodlabel(ICOMP,ISLOT)(1:5).eq.'Small')then
              caskeytype(icomp,ISLOT)='equipment'
            else
              caskeytype(icomp,ISLOT)='-            '
            endif

C File included a 5th label and we assume that this is this reserved use.
            if(ISLOT.eq.5)then
              caskeytype(icomp,ISLOT)='net_utilities'
            endif
 1234     CONTINUE
        ENDIF 
        if(doupgrade)then
          ip3ver(icomp)=21  ! Upgrade the version for next write.
        endif
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
     &  ' No casual gains labels found ...supplying defaults.','-')
        lodlabel(icomp,1)='Occupants '
        lodlabel(icomp,2)='Lights    '
        lodlabel(icomp,3)='SmallPower' 
        lodlabel(icomp,4)='Other     ' 
        lodlabel(icomp,5)='Ann.El    ' 
        lodlabel(icomp,6)='Metabolic ' 
        lodlabel(icomp,7)='NA        ' 
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
#include "schedule.h"
#include "help.h"

      integer lnblnk  ! function definition

      COMMON/OUTIN/IUOUT,IUIN,IEOUT
      common/caleni/nbdaytype,nbcaldays(MDTY),icalender(365)
      INTEGER NBDAYTYPE,NBCALDAYS,ICALENDER

C Scratch version of operations file (following pattern in schedule.h).
      integer ip3verc
      common/p3verc/ip3verc

C Descriptive label for a zone load & casual gain types.
      character lodlabelc*12,caskeytypec*16
      common/loadlabelc/lodlabelc(MGTY),caskeytypec(MGTY)

      integer lodslotc    ! same meaning as loadkey common
      integer lodatr1c,lodatr2c
      common/loadkeyc/lodslotc(MGTY),lodatr1c(MGTY),
     &                lodatr2c(MGTY)

      CHARACTER oprdescc*248,ventdescc*248,ctlstrc*24
      COMMON/P1C/oprdescc,ventdescc,ctlstrc

C Scratch copy of air flow pattern from an operations file.
      COMMON/P2NC/NACC(MDTY),IACSC(MDTY,MA),IACFC(MDTY,MA),
     &            ACIC(MDTY,MA),ACVC(MDTY,MA),IPTC(MDTY,MA),
     &            TAC(MDTY,MA)
      INTEGER NACC,IACSC,IACFC,IPTC
      REAL ACIC,ACVC,TAC

C Scratch version of P2CTL
      COMMON/P2CTLC/ITCTLC,TLOC,TUPC,THIC,ACILC,ACVLC,IVLC,TALC,
     &   ACIUC,ACVUC,IVUC,TAUC,ACIHC,ACVHC,IVHC,TAHC

C Scratch version of P3N
      INTEGER NCASC,ICGSC,ICGFC,ICGUnitC
      REAL CMGSC,CMGLC,RADCC,CONCC
      REAL airvelC,tnbmenC,tnbwomenC,tnbchildC,metabolicC,cloC
      COMMON/P3NC/NCASC(MDTY),ICGSC(MDTY,MC),ICGFC(MDTY,MC),
     & ICGUnitC(MDTY,MC),CMGSC(MDTY,MC),CMGLC(MDTY,MC),
     & RADCC(MDTY,MC),CONCC(MDTY,MC),
     & tnbmenC(MDTY,MC),tnbwomenC(MDTY,MC),tnbchildC(MDTY,MC),
     & metabolicC(MDTY,MC),cloC(MDTY,MC),airvelC(MDTY,MC)

C Scratch version of P3TYPEN.
      INTEGER ICGTC
      COMMON/P3TYPENC/ICGTC(MDTY,MC)

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
      character dstmp*24
      character louts*248
      character actflatfile*1  ! action requested.
      logical havehi,dll,ok,dok
      logical needtoaddone     ! in case short on calendar day types
      INTEGER IDTY,IR,ndty
      REAL R

      dok=.false.
      needtoaddone=.false.
      helpinsub='eroper'  ! set for subroutine
      helptopic='read_opr_schedules'
      call gethelptext(helpinsub,helptopic,nbhelp)

C Initialise number of day types assuming that it matches that
C used in the current model.
      NDTY=nbdaytype
 
      if(actflatfile(1:1).eq.'W')then
C Compose help message
        CALL PHELPD('Warning',nbhelp,'-',0,0,IER)
        call easkok(' ','Import data?',OK,4)
      elseif(actflatfile(1:1).eq.'-')then
        ok=.true.
      endif
      if(.not.ok)return

C Check if running in dll mode.
      call isadll(dll)

C Initialise project data file.
 43   CALL EFOPSEQ(IUO,FILE,1,IER)
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
        do 989 idty=1,ndty
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
        elseif(outstr(13:15).eq.'2.1')then
          ip3verc=21
        elseif(outstr(13:15).eq.'3.0')then
          ip3verc=30
        endif
        CALL STRIPC(IUO,OUTSTR,0,ND,1,'header',IER)
        IF(IER.NE.0) goto 1002
        K=0
        CALL EGETW(OUTSTR,K,WORD,'W','header tags',IER)
        IF(IER.NE.0) goto 1002
        if(WORD(1:5).eq.'*Date'.or.WORD(1:5).eq.'*date')then
          CALL EGETRM(OUTSTR,K,dstmp,'W','date stamp',IER)
          if(IER.ne.0) IER=0  ! missing date stamp not fatal
        endif
        if(ip3verc.lt.21)then
          CALL LSTRIPC(IUO,LOUTSTR,0,ND,1,'oper notes',IER)
          oprdescc=LOUTSTR
          ventdescc=LOUTSTR
        elseif(ip3verc.eq.21)then
          CALL LSTRIPC(IUO,LOUTSTR,0,ND,1,'oper notes',IER)
          ventdescc=LOUTSTR
        elseif(ip3verc.gt.21)then
          CALL LSTRIPC(IUO,LOUTSTR,0,ND,1,'oper *air_schedules',IER)
          if(loutstr(1:14).eq.'*air_schedules')then
            CALL LSTRIPC(IUO,LOUTSTR,0,ND,1,'air sched notes',IER)
            ventdescc=LOUTSTR
          else
            ventdescc=LOUTSTR  ! set to same as oprdesc
          endif
        endif
      else
        write(oprdescc,'(a)') OUTSTR(1:lnblnk(OUTSTR))
      endif

C Thermostatic control data. If 4th item then
      if(ip3verc.le.21)then
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
          ACIHC=0.; ACVHC=0.; IVHC=0; TAHC=0.
        endif

C Air change information. Expect 1 item defining nb period. If six
C items then there was at least one extra day type. Could try to
C re-scan with different assumption.

C << logic needed to reconcile the difference in day types >>
 22     DO 400 IDTY=1,ndty
          CALL STRIPC(IUO,OUTSTR,99,ND,1,'flow periods',IER)
          if(ND.ne.1)then
            write(outs,'(2a,i2)') 'The source operation file may',
     &        ' assume more calendar day types ',ndty
            call edisp(iuout,outs)
            write(outs,'(a,i2,a)') 
     &        ' than is used in the model',nbdaytype,' please check.'
            call edisp(iuout,outs)
            goto 1002  ! can not yet deal with this
          endif
          K=0
          CALL EGETWI(OUTSTR,K,NACC(IDTY),0,MA,'W','flow periods',IER)
          IF(NACC(IDTY).EQ.0)goto 400
          DO 10 I=1,NACC(IDTY)

C Expect 6 items on each period line. If only one item then we got to
C the next days data a bit soon. Could try to rescan file.
C << more logic needed to add an additional day type data prior
C << to exiting the facility and transferring scratch commons
C << into the model.
            CALL STRIPC(IUO,OUTSTR,99,ND,1,'Period flow details',IER)
            if(ND.ne.6)then
              write(outs,'(2a,i2)') 'The source operation file may',
     &          ' assume fewer calendar day types ',ndty
              call edisp(iuout,outs)
              ndty=ndty-1    ! decrement and try again
              write(outs,'(a,i2,a,i2,a)') 'than is used in the model',
     &          nbdaytype,' trying',ndty,'....'
              call edisp(iuout,outs)
              CALL ERPFREE(IUO,ISTAT)
              needtoaddone=.true.
              goto 43
            endif
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
   10     CONTINUE
 400    CONTINUE
        IF(IER.NE.0) goto 1002

      elseif(ip3verc.ge.30)then

C This logic for scanning newer files still to be done.

      endif

C Casual gains.  

C For ip3ver = 21 read section about casual types.
      if(ip3verc.ge.21)then
        CALL LSTRIPC(IUO,LOUTSTR,0,ND,1,'start of casual',IER)
        if(loutstr(1:7).eq.'*casual')then
          CALL LSTRIPC(IUO,LOUTSTR,0,ND,1,'oper casual notes',IER)
          oprdescc=LOUTSTR
        else
          oprdescc=LOUTSTR
        endif

C Read scratch variables for *type.
        islot=0
  42    CALL STRIPC(IUO,OUTSTR,0,ND,1,'*type or *end_type',IER)
        if(outstr(1:5).eq.'*type')then
          islot=islot+1   ! increment the casual gain slot
          K=6
          CALL EGETW(OUTSTR,K,lodlabelc(ISLOT),'W',
     &      'user casual gain label',IER) 
          CALL EGETW(OUTSTR,K,caskeytypec(ISLOT),'W',
     &      'casual gain keyword',IER) 
          if(caskeytypec(ISLOT)(1:13).eq.'dynamicpeople') ! old file version
     &      caskeytypec(ISLOT)='dynamic_people'
          CALL EGETWI(OUTSTR,K,lodslotc(ISLOT),0,99,'W',
     &      'casual gain slot position',IER)
          CALL EGETWI(OUTSTR,K,lodatr1c(ISLOT),0,99,'W',
     &      'casual gain atribute 1',IER)
          CALL EGETWI(OUTSTR,K,lodatr2c(ISLOT),0,99,'W',
     &      'casual gain atribute 2',IER)
          goto 42   ! read another line
        elseif(outstr(1:9).eq.'*end_type')then

C Debug.
C          WRITE(6,*) 'lodlabelc ',lodlabelc
C          WRITE(6,*) 'caskeytypec ',caskeytypec
C          WRITE(6,*) 'lodslotc ',lodslotc
C          WRITE(6,*) 'lodatr1c ',lodatr1c
          continue   ! we are done with this section
        endif
      endif

C Casual gains expect 1 item for number of periods. If we get six
C items then there was at least one extra day type. Give warning.
      DO 600 IDTY=1,ndty
        CALL STRIPC(IUO,OUTSTR,99,ND,1,'gain periods',IER)
        if(ND.ne.1)then
          write(outs,'(2a,i2)') 'The source operation file may',
     &      ' assume more calendar day types ',ndty
          call edisp(iuout,outs)
          write(outs,'(a,i2,a)') 
     &      ' than is used in the model',nbdaytype,' please check.'
          call edisp(iuout,outs)
          goto 1002  ! can not yet deal with this
        endif
        K=0
        CALL EGETWI(OUTSTR,K,IR,0,MC,'W','periods',IER)
        NCASC(IDTY)=IR
        IF(NCASC(IDTY).EQ.0)goto 600
        DO 40 I=1,NCASC(IDTY)
          CALL STRIPC(IUO,OUTSTR,99,ND,1,'Period gain detail',IER)
          K=0
          if(ND.eq.7.or.ND.eq.12)then
            CALL EGETWI(OUTSTR,K,IR,-3,5,'W','overloaded type',IER)
            ICGTC(IDTY,I)=IR
            jicgt=iabs(ICGT(IDTY,I))
C            ipslot=iabs(ir)  ! get equivalent of lodslot
            if(ip3verc.lt.21)then
              if(ir.gt.0)then
                ICGUnitC(IDTY,I)=0
              elseif(ir.eq.-1)then
                ICGUnitC(IDTY,I)=2
              else
                ICGUnitC(IDTY,I)=1
              endif
            elseif(ip3verc.eq.21)then
              if(ir.gt.0)then
                if(caskeytypec(jicgt)(1:14).eq.
     &             'dynamic_people')then
                  ICGUnit(IDTY,I)=3  ! number of people
                else
                  ICGUnitC(IDTY,I)=0
                endif
              elseif(ir.lt.0)then
                if(caskeytypec(jicgt)(1:14).eq.
     &             'dynamic_people')then
                  ICGUnit(IDTY,I)=3  ! number of people
                elseif(caskeytypec(jicgt)(1:6).eq.'people')then
                  ICGUnit(IDTY,I)=2
                else
                  ICGUnitC(IDTY,I)=1
                endif
              endif
            else
C << still to be done for ip3ver >21 >>
            endif
          else
            ICGTC(IDTY,I)=1 ! assume slot one if unspecified
            ICGUnitC(IDTY,I)=1
          endif

C Debug.
C          write(6,*) ' ERZSCHED ',idty,i,ir,ICGTC(IDTY,I),
C     &      ICGUnitC(IDTY,I)

          CALL EGETWI(OUTSTR,K,IR,0,24,'W',' gain st',IER)
          ICGSC(IDTY,I)=IR
          CALL EGETWI(OUTSTR,K,IR,0,24,'W',' gain fn',IER)
          ICGFC(IDTY,I)=IR
          if(caskeytypec(jicgt)(1:14).eq.'dynamic_people')then
            CMGSC(IDTY,I)=0.0
            CMGLC(IDTY,I)=0.0
            CALL EGETWR(OUTSTR,K,R,0.,0.,'-',' tot men',IER)
            tnbmen(IDTY,I)=R
            CALL EGETWR(OUTSTR,K,R,0.,0.,'-',' tot women',IER)
            tnbwomen(IDTY,I)=R
            CALL EGETWR(OUTSTR,K,R,0.,0.,'-',' tot child',IER)
            tnbchild(IDTY,I)=R
            CALL EGETWR(OUTSTR,K,R,0.,0.,'-',' metabolic',IER)
            metabolic(IDTY,I)=R
            CALL EGETWR(OUTSTR,K,R,0.,0.,'-',' clo value',IER)
            clov(IDTY,I)=R
            CALL EGETWR(OUTSTR,K,R,0.,0.,'-',' air veloc',IER)
            airvel(IDTY,I)=R
          else
            CALL EGETWR(OUTSTR,K,R,0.,0.,'-',' sens',IER)
            CMGSC(IDTY,I)=R
            CALL EGETWR(OUTSTR,K,R,0.,0.,'-',' latent',IER)
            CMGLC(IDTY,I)=R
            tnbmen(IDTY,I)=0.0  ! fill commons for non-dynamic cases
            tnbwomen(IDTY,I)=0.0
            tnbchild(IDTY,I)=0.0
            metabolic(IDTY,I)=0.0
            airvel(IDTY,I)=0.0
            clov(IDTY,I)=0.0
          endif
          CALL EGETWR(OUTSTR,K,R,0.,1.,'W',' rad fr',IER)
          RADCC(IDTY,I)=R
          CALL EGETWR(OUTSTR,K,R,0.,1.,'W',' conv fr',IER)
          CONCC(IDTY,I)=R

C Check if electrical data has also been saved.
          if(ND.eq.12.or.ND.eq.15)then
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
            IELFC=1   ! Signal there is electrical data.
          endif
   40   CONTINUE
  600 CONTINUE

C Check to see if gain labels have been added to the end of the file.
C Fill defaults prior to scanning the labels.
      if(ip3verc.lt.21)then
        CALL STRIPC(IUO,OUTSTR,99,ND,1,'gain labels',IERV)
        IF(IERV.ne.0) THEN
          goto 1001
        ELSE
          K=0
          DO 1234 islot=1,ND  ! for each token found
            CALL EGETW(OUTSTR,K,lodlabelc(islot),'W','gain label',IER) 
            lodslotc(ISLOT)=ISLOT; lodatr1c(ISLOT)=0
            lodatr2c(ISLOT)=0
            if(lodlabelc(ISLOT)(1:6).eq.'Occupt')then
              caskeytypec(ISLOT)='people       '
            elseif(lodlabelc(ISLOT)(1:6).eq.'Lights')then
              caskeytypec(ISLOT)='lighting     '
            elseif(lodlabelc(ISLOT)(1:6).eq.'Equipt')then
              caskeytypec(ISLOT)='equipment'
            elseif(lodlabelc(ISLOT)(1:5).eq.'Small')then
              caskeytypec(ISLOT)='equipment'
            elseif(lodlabelc(ISLOT)(1:9).eq.'Metabolic')then
              caskeytypec(ISLOT)='dynamic_people'
            else
              caskeytypec(ISLOT)='-            '
            endif

C File included a 5th label and we assume that this is this reserved use.
            if(ISLOT.eq.5)then
              caskeytypec(ISLOT)='net_utilities'
            endif
 1234     CONTINUE
        ENDIF
      endif  
 1235 CONTINUE

C If we need to add in an additional calendar day type to ensure
C a match with the model do it here.
      if(needtoaddone)then
        NACC(NBDAYTYPE)=0
        NCASC(NBDAYTYPE)=0
      endif

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
     &  ' No casual gains labels found ...supplying defaults.','-')
      lodlabelc(1)='Occupants '
      caskeytypec(1)='people      '
      lodlabelc(2)='Lights    '
      caskeytypec(2)='lighting    '
      lodlabelc(3)='SmallPower' 
      caskeytypec(3)='equipment   '
      lodlabelc(4)='Otherstuff' 
      caskeytypec(4)='other       '  
      lodlabelc(5)='Ann.El    ' 
      caskeytypec(5)='net_utilities'  
      lodlabelc(6)='Metabolic ' 
      caskeytypec(6)='dynamic_people'  
      lodlabelc(7)='NA        ' 
      caskeytypec(7)='-           '  
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
C ZSCHEDHINT displays contents of an operation file and reports on
C it so a user can decide if they want to use it. It has no impact
C on data structures of the model.
C IUO is the file unit to use
C FILE (variable width char) is the name of the file to scan.

      SUBROUTINE ZSCHEDHINT(IUO,FILE,IER)
#include "building.h"
#include "espriou.h"
#include "help.h"

      integer lnblnk  ! function definition

      COMMON/OUTIN/IUOUT,IUIN,IEOUT
      common/caleni/nbdaytype,nbcaldays(MDTY),icalender(365)
      INTEGER NBDAYTYPE,NBCALDAYS,ICALENDER

C Error subroutine and error details for dll mode.
      common/dllerr/dllsubr,dllmesg

C Local variables (to protect info in common blovks)
      CHARACTER outs*124
      CHARACTER*(*) FILE
      character dllsubr*12,dllmesg*124
      character louts*248
      logical dll,ok,dok
      integer ndty
      character longtfile*144

      dok=.true.
      helpinsub='eroper'  ! set for subroutine
      helptopic='list_schedules'
      call gethelptext(helpinsub,helptopic,nbhelp)

C List out the file contents and then ask the user for agreement
C and for the number of day types. First check if running in dll mode.
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

C Display the contents of the file to the text feedback.
      call edisp(iuout,
     &  '---start of file-------------------------------------')
      write(currentfile,'(a)') FILE(1:lnblnk(FILE))
      write(longtfile,'(a)') file
      CALL LISTAS(IUO,longtfile,IER)

C Initialise number of day types assuming that it matches that
C used in the current model.
      NDTY=nbdaytype

      call edisp(iuout,
     &  '---end of file------------------------------------- ')
      call edisp(iuout,' ')
      write(outs,'(a,i2,a)') 'PLEASE NOTE: the current model assumes',
     &  nbdaytype,' day types in the year.'
      call edisp(iuout,outs)
      call edisp(iuout,
     &  'Review the contents listed to see if it matches. You will')
      call edisp(iuout,'be asked for the day types in the file.')
      call easkok(' ','Continue with import?',OK,nbhelp)
      if(.not.ok)return
      CALL EASKI(ndty,'Number of calendar day types:',
     &  'Confirm:',0,'F',mdty,'F',4,'day types',IER,nbhelp) 

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
C      COMMON/OUTIN/IUOUT,IUIN,IEOUT

C Electrical data flag and elctrical data.
      common/elecflg/ielf(mcom)
      COMMON/ELP3N/NEL(MDTY),PF(MDTY,MC),IPF(MDTY,MC),PWR(MDTY,MC),
     &BVOLT(MDTY,MC),IPHAS(MDTY,MC)
      INTEGER NEL,IPF,IPHAS
      REAL PF,PWR,BVOLT
      COMMON/OVRWT/AUTOVR
      logical AUTOVR
      logical isdynamic       ! true if reading a dynamic_people
      logical close_a,close_b ! test if any ventilation directives 
      character tokens*156,comment*76,aligned_str*156
      character vctlstr*24
      character dstmp*24
      integer irange,icur  ! for looping
      
      IER=0

C Open any existing file by this name (ask user for confirmation to
C over-write) or create a new file.
      if(AUTOVR)then
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

C If version 1/2/21/30 write out appropriate header. In order
C to account for smaller step changes use 10x the version number
C after 2.0  i.e. ip3ver 21 for 2.1.
      call dstamp(dstmp)
      write(comment,'(2a)') 'zone operation definiions for ',
     &  zname(ICOMP)(1:lnzname(ICOMP))
      if(ip3ver(icomp).eq.0)then
        continue
      elseif(ip3ver(icomp).eq.1)then
        write(tokens,'(a)') '*Operations 1.0'
        call align_comment(46,tokens,comment,aligned_str)
        write(IUO,'(a)') aligned_str(1:lnblnk(aligned_str))
      elseif(ip3ver(icomp).eq.2)then
        write(tokens,'(a)') '*Operations 2.0'
        call align_comment(46,tokens,comment,aligned_str)
        write(IUO,'(a)') aligned_str(1:lnblnk(aligned_str))
      elseif(ip3ver(icomp).eq.21)then
        write(tokens,'(a)') '*Operations 2.1'
        call align_comment(46,tokens,comment,aligned_str)
        write(IUO,'(a)') aligned_str(1:lnblnk(aligned_str))
      elseif(ip3ver(icomp).eq.30)then
        write(tokens,'(a)') '*Operations 3.0'
        call align_comment(46,tokens,comment,aligned_str)
        write(IUO,'(a)') aligned_str(1:lnblnk(aligned_str))
      endif
      write(tokens,'(2a)') '*date ',dstmp
      write(comment,'(a)') 'latest file modification'
      call align_comment(46,tokens,comment,aligned_str)
      write(IUO,'(a)') aligned_str(1:lnblnk(aligned_str))
      WRITE(IUO,'(a)',IOSTAT=IOS,ERR=1) 
     &  '# infiltration and ventilation notes:'

C Write either the combined or scheduled air description.
      if(ip3ver(icomp).lt.21)then
        WRITE(IUO,'(A)',IOSTAT=IOS,ERR=1)
     &    oprdesc(ICOMP)(1:lnblnk(oprdesc(ICOMP)))
      elseif(ip3ver(icomp).eq.21)then
        WRITE(IUO,'(A)',IOSTAT=IOS,ERR=1)
     &    ventdesc(ICOMP)(1:lnblnk(ventdesc(ICOMP)))
      endif

C Write air flow control data to the file.
C If ipv3ver is 0, 1 or 2 write ventilation control data as if
C there was only one day type and one period during the day.
      WRITE(IUO,'(a)',IOSTAT=IOS,ERR=1)
     &  '# infiltration and ventilation controls:'
      if(ip3ver(icomp).lt.21)then
        call ventctlstr(icomp,1,'S',vctlstr,comment)
        write(tokens,'(i4,3F10.2)') ITCTL(ICOMP),
     &      TLO(ICOMP),TUP(ICOMP),THI(ICOMP)
        call align_comment(46,tokens,comment,aligned_str)
        write(IUO,'(a)') aligned_str(1:lnblnk(aligned_str))

        IF(ITCTL(ICOMP).NE.0)THEN
          call ventctlstr(icomp,1,'L',vctlstr,comment)
          write(tokens,'(2F10.3,I5,F10.2)') ACIL(ICOMP),ACVL(ICOMP),
     &        IVL(ICOMP),TAL(ICOMP)
          call align_comment(46,tokens,comment,aligned_str)
          write(IUO,'(a)') aligned_str(1:lnblnk(aligned_str))

          call ventctlstr(icomp,1,'M',vctlstr,comment)
          write(tokens,'(2F10.3,I5,F10.2)') ACIU(ICOMP),ACVU(ICOMP),
     &      IVU(ICOMP),TAU(ICOMP)
          call align_comment(46,tokens,comment,aligned_str)
          write(IUO,'(a)') aligned_str(1:lnblnk(aligned_str))

          call ventctlstr(icomp,1,'H',vctlstr,comment)
          write(tokens,'(2F10.3,I5,F10.2)') ACIH(ICOMP),ACVH(ICOMP),
     &      IVH(ICOMP),TAH(ICOMP)
          call align_comment(46,tokens,comment,aligned_str)
          write(IUO,'(a)') aligned_str(1:lnblnk(aligned_str))
        ENDIF

      elseif(ip3ver(icomp).ge.21)then

C Write the infiltration and ventilation control data.
        call ventctlstr(icomp,1,'S',vctlstr,comment)
        write(tokens,'(i4,3F10.2)') ITCTL(ICOMP),
     &      TLO(ICOMP),TUP(ICOMP),THI(ICOMP)
        call align_comment(46,tokens,comment,aligned_str)
        write(IUO,'(a)') aligned_str(1:lnblnk(aligned_str))

        IF(ITCTL(ICOMP).NE.0)THEN
          call ventctlstr(icomp,1,'L',vctlstr,comment)
          write(tokens,'(2F10.3,I5,F10.2)') ACIL(ICOMP),ACVL(ICOMP),
     &        IVL(ICOMP),TAL(ICOMP)
          call align_comment(46,tokens,comment,aligned_str)
          write(IUO,'(a)') aligned_str(1:lnblnk(aligned_str))

          call ventctlstr(icomp,1,'M',vctlstr,comment)
          write(tokens,'(2F10.3,I5,F10.2)') ACIU(ICOMP),ACVU(ICOMP),
     &      IVU(ICOMP),TAU(ICOMP)
          call align_comment(46,tokens,comment,aligned_str)
          write(IUO,'(a)') aligned_str(1:lnblnk(aligned_str))

          call ventctlstr(icomp,1,'H',vctlstr,comment)
          write(tokens,'(2F10.3,I5,F10.2)') ACIH(ICOMP),ACVH(ICOMP),
     &      IVH(ICOMP),TAH(ICOMP)
          call align_comment(46,tokens,comment,aligned_str)
          write(IUO,'(a)') aligned_str(1:lnblnk(aligned_str))
        ENDIF

      endif

C For all versions, write each calendar day ventilation instructions.
      DO 400 IDTY=1,NBDAYTYPE
       write(tokens,'(i5)') NAC(IDTY)
       write(comment,'(2a)') 'number of flow periods during ',
     &   calentag(idty)
       call align_comment(46,tokens,comment,aligned_str)
       write(IUO,'(a)') aligned_str(1:lnblnk(aligned_str))
        IF (NAC(IDTY).GT.0)THEN
          do I = 1,NAC(IDTY)
            acim = (VOL(ICOMP)*ACI(IDTY,I))/3600.0
            acvm = (VOL(ICOMP)*ACV(IDTY,I))/3600.0
            write(tokens,'(i4,a,i3,a,2F9.3,i5,f9.2)') IACS(IDTY,I),',',
     &       IACF(IDTY,I),',',ACI(IDTY,I),ACV(IDTY,I),IPT(IDTY,I),
     &       TA(IDTY,I)
            if(IPT(idty,i).eq.0)then
              call eclose(ACV(IDTY,I),0.0,0.01,close_a)
              call eclose(TA(IDTY,I),0.0,0.01,close_b)
              if(close_a.and.close_b)then
                write(comment,'(a,f7.4,a)') 
     &          'start, stop, infil ach (',acim,'m^3/s)'
              else
                write(comment,'(a,f7.4,a,f7.4,a)') 
     &          'start, stop, infil ach (',acim,
     &          'm^3/s), vent ach (',acvm,
     &          'm^3/s), source, @C'
              endif
            else
              write(comment,'(a,f7.4,a,f7.4,2a)') 
     &        'start stop infil ach (',acim,
     &        'm^3/s) vent ach (',acvm,
     &        'm^3/s) from ',zname(IPT(idty,I))(1:10)
            endif
            call align_comment(46,tokens,comment,aligned_str)
            write(IUO,'(a)') aligned_str(1:lnblnk(aligned_str))
          enddo
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

      if(ip3ver(icomp).ge.21)then

C Write tag *casual and then documentation for casual gains.
        WRITE(IUO,'(A)',IOSTAT=IOS,ERR=1) 
     &    '# casual gain type definitions:'
        WRITE(IUO,'(A)',IOSTAT=IOS,ERR=1) '*casual'
        WRITE(IUO,'(A)',IOSTAT=IOS,ERR=1) '# notes on casual gains:'
        WRITE(IUO,'(A)',IOSTAT=IOS,ERR=1)
     &    oprdesc(ICOMP)(1:lnblnk(oprdesc(ICOMP)))

C For ip3ver >= 21 write casual gain attributes. Loop
C through all the possible casual gain slots for this zone
C and if lodslot is non-zero then write a *type line.
        WRITE(IUO,'(A)',IOSTAT=IOS,ERR=1) 
     &    '# casual label     type-key-word   slot index & attributes'
        isdynamic=.false.       ! Assume no dynamic people.
        do ir= 1,MGTY
          if(lodslot(icomp,ir).ne.0)then
            WRITE(IUO,'(4A,3i4)',IOSTAT=IOS,ERR=1) '*type ',
     &        lodlabel(icomp,ir),' ',caskeytype(icomp,ir),
     &        lodslot(icomp,ir),lodatr1(icomp,ir),lodatr2(icomp,ir)
            if(caskeytype(icomp,ir)(1:14).eq.'dynamic_people')then
               isdynamic=.true.  ! Signal different comments.
            endif
          endif         
        enddo
        WRITE(IUO,'(A)',IOSTAT=IOS,ERR=1) '*end_type '
      endif

      WRITE(IUO,'(a)',IOSTAT=IOS,ERR=1) '# casual gain schedules:'
      DO 600 IDTY=1,NBDAYTYPE
        WRITE(IUO,'(1X,I5,2A)',IOSTAT=IOS,ERR=1)NCAS(IDTY),
     &  '   # number of casual gains in day type: ',calentag(idty)
        IF (NCAS(IDTY).GT.0)THEN
          IF(IELF(ICOMP).EQ.1) THEN  ! Electrical included.
            if(ip3ver(icomp).lt.21)then
              WRITE(IUO,'(a,a)',IOSTAT=IOS,ERR=1)
     &        '# slot, period, sensible, latent, rad_frac, ',
     &        'conv_frac, pf, +/-, power, volt, phase'  
              DO I = 1,NCAS(IDTY)
                WRITE(IUO,5470,IOSTAT=IOS,ERR=1)ICGT(IDTY,I),
     &          ICGS(IDTY,I),ICGF(IDTY,I),CMGS(IDTY,I),CMGL(IDTY,I),
     &          RADC(IDTY,I),CONC(IDTY,I),PF(IDTY,I),IPF(IDTY,I),
     &          PWR(IDTY,I),BVOLT(IDTY,I),IPHAS(IDTY,I) 
              ENDDO
            else  ! Newer file format assume no dynamic_people if electrical.
              WRITE(IUO,'(a,a)',IOSTAT=IOS,ERR=1)
     &        '# slot, period, sensible, latent, rad_frac, ',
     &        'conv_frac, pf, +/-, power, volt, phase'  
              DO I = 1,NCAS(IDTY)
                WRITE(IUO,5470,IOSTAT=IOS,ERR=1)ICGT(IDTY,I),
     &          ICGS(IDTY,I),ICGF(IDTY,I),CMGS(IDTY,I),CMGL(IDTY,I),
     &          RADC(IDTY,I),CONC(IDTY,I),PF(IDTY,I),IPF(IDTY,I),
     &          PWR(IDTY,I),BVOLT(IDTY,I),IPHAS(IDTY,I) 
              ENDDO
            endif
          ELSE   ! Electrical data not included in file.
            if(ip3ver(icomp).lt.21)then
              WRITE(IUO,'(A)',IOSTAT=IOS,ERR=1)
     &         '# slot, period, sensible, latent, rad_frac, conv_frac'
              DO I = 1,NCAS(IDTY)
               WRITE(IUO,'(3(I4,a),2(F9.1,a),F7.2,a,F7.2)',
     &           IOSTAT=IOS,ERR=1) ICGT(IDTY,I),',',ICGS(IDTY,I),',',
     &           ICGF(IDTY,I),',',CMGS(IDTY,I),',',CMGL(IDTY,I),',',
     &           RADC(IDTY,I),',',CONC(IDTY,I)
              ENDDO  ! of I
            else
              if(isdynamic)then
                WRITE(IUO,'(2A)',IOSTAT=IOS,ERR=1)
     &            '# slot, period, nb men, nb women, nb child,',
     &            ' metabolic clo, air vel, rad_frac, conv_frac'
              else
                WRITE(IUO,'(A)',IOSTAT=IOS,ERR=1)
     &          '# slot, period, sensible, latent, rad_frac, conv_frac'
              endif

C Key logic is to check caskeytype for each period for dynamic_people.
              DO I = 1,NCAS(IDTY)
                jicgt=iabs(ICGT(IDTY,I))
                if(caskeytype(icomp,jicgt)(1:14).eq.
     &             'dynamic_people')then
                  WRITE(IUO,'(3(I4,a),6(F6.2,a),F7.2,a,F7.2)',
     &              IOSTAT=IOS,ERR=1) ICGT(IDTY,I),',',
     &              ICGS(IDTY,I),',',ICGF(IDTY,I),
     &              ',',tnbmen(IDTY,I),',',tnbwomen(IDTY,I),',',
     &              tnbchild(IDTY,I),',',metabolic(IDTY,I),',',
     &              clov(IDTY,I),',',airvel(IDTY,I),',',RADC(IDTY,I),
     &              ',',CONC(IDTY,I)
                else
                  WRITE(IUO,'(3(I4,a),2(F9.1,a),F7.2,a,F7.2)',
     &              IOSTAT=IOS,ERR=1) ICGT(IDTY,I),',',
     &              ICGS(IDTY,I),',',ICGF(IDTY,I),',',CMGS(IDTY,I),
     &              ',',CMGL(IDTY,I),',',RADC(IDTY,I),',',CONC(IDTY,I)
                endif           
              ENDDO  ! of I
            endif
          ENDIF
        ENDIF
 600  CONTINUE

5470  FORMAT(3(I4,','),F9.1,',',F9.1,',',F7.2,',',F8.2,',',F5.2,
     &       ',',I2,',',F7.1,',',F7.1,',',I2)

C Write out the gain labels to the file for future reference. In version
C >= 21 this section is relocated to ~line 1508.
      if(ip3ver(icomp).lt.21)then
        WRITE(IUO,'(a)')'# Labels for gains '
        WRITE(IUO,'(7(1X,A))',IOSTAT=IOS,ERR=1) 
     &    (lodlabel(icomp,I),I=1,irange)
      endif
     
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

C Markdown flag.
      logical markdown
      common/markdownflag/markdown
      character vctlstr*24,comment*76
      character outs*124

C Write documentation on air schedules. 
      if(markdown)then
        call edispxtr(itru,'Schedule notes ')
        call edisp248(itru,ventdesc(icomp),100)
        call edisp248(itru,oprdesc(icomp),100)
      else
        call edisp(itru,' Air schedule notes:')
        call edisp248(itru,ventdesc(icomp),82)
      endif
      if(ip3ver(icomp).le.21)then

C Older file versions only display ctlstr for first day and period.
        call ventctlstr(icomp,1,'S',vctlstr,comment)
        ctlstr(icomp,1)=vctlstr
        IY=ITCTL(ICOMP)
        if(markdown)then
          write(outs,'(a,a)') 'Flow control ',ctlstr(icomp,1)
          call edisp2tr(itru,outs)
        else
          write(outs,'(a,a)') ' Flow control: ',ctlstr(icomp,1)
          call edisp(itru,outs)
        endif

        if(IY.eq.0) goto 1225

        if(markdown)then
          write(outs,'(2a)')' Control Setpoint  Infiltration',
     &      '     Ventilation    Source  temp'
          call edisp(itru,outs)
          write(outs,'(2a)')' range             ac/h   m^3/s',
     &    '     ac/h   m^3/s   index   degC'
          call edisp(itru,outs)
        else
          write(outs,'(2a)')' Control Setpoint  Infiltration',
     &      '     Ventilation    Source  temp'
          call edisp(itru,outs)
          write(outs,'(2a)')' range             ac/h   m^3/s',
     &    '     ac/h   m^3/s   index   degC'
          call edisp(itru,outs)
        endif
        acilm = (VOL(ICOMP)*ACIL(ICOMP))/3600.0
        acvlm = (VOL(ICOMP)*ACVL(ICOMP))/3600.0
        WRITE(outs,'(A,F7.2,F9.3,F8.4,F10.3,F8.4,I5,F10.3)')
     &    ' Lower  ',TLO(ICOMP),ACIL(ICOMP),acilm,
     &    ACVL(ICOMP),acvlm,IVL(ICOMP),TAL(ICOMP)
        call edisp(itru,outs)
        write(outs,'(a,F5.1,a,F5.1,a)') ' Nominal between',
     &    TLO(ICOMP),' & ',TUP(ICOMP),' based on period data.'
        call edisp(itru,outs)

        acium = (VOL(ICOMP)*ACIU(ICOMP))/3600.0
        acvum = (VOL(ICOMP)*ACVU(ICOMP))/3600.0
        WRITE(outs,'(A,F7.2,F9.3,F8.4,F10.3,F8.4,I5,F10.3)')
     &    ' Middle ',TUP(ICOMP),ACIU(ICOMP),acium,
     &    ACVU(ICOMP),acvum,IVU(ICOMP),TAU(ICOMP)
        call edisp(itru,outs)

        acihm = (VOL(ICOMP)*ACIH(ICOMP))/3600.0
        acvhm = (VOL(ICOMP)*ACVH(ICOMP))/3600.0
        WRITE(outs,'(A,F7.2,F9.3,F8.4,F10.3,F8.4,I5,F10.3)')
     &    ' High   ',THI(ICOMP),ACIH(ICOMP),acihm,
     &    ACVH(ICOMP),acvhm,IVH(ICOMP),TAH(ICOMP)
        call edisp(itru,outs)
      elseif(ip3ver(icomp).ge.30)then

C Display ctlstr for day and period << convert to multi days >>.
        call ventctlstr(icomp,1,'S',vctlstr,comment)
        ctlstr(icomp,1)=vctlstr
        IY=ITCTL(ICOMP)
        if(markdown)then
          write(outs,'(a,a)') 'Control ',ctlstr(icomp,1)
          call edisp2tr(itru,outs)
        else
          write(outs,'(a,a)') ' Control: ',ctlstr(icomp,1)
          call edisp(itru,outs)
        endif

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
      if(markdown)then
        call edispxtr(itru,
     &    'Scheduled air infiltration and ventilation  ')
        call edisp(itru,' ')
        WRITE(outs,'(2A)')
     &   'Day type  ID  Hours   Infil. (ac/h)  Infil. (m^3^/s)  ',
     &   'Vent. (ac/h)  Vent. (m^3^/s)  From zone  Source DegC'
        call edisp(itru,outs)
        WRITE(outs,'(2A)')
     &   '--------  --  ------  -------------  ---------------  ',
     &   '------------  --------------  ---------  -----------'
        call edisp(itru,outs)
      else
        call edisp(itru,'Scheduled air infiltration and ventilation: ')
        WRITE(outs,'(2A)')
     &   'Daytype    Period     Infiltration     Ventilation     ',
     &   'From Source'
        call edisp(itru,outs)
        WRITE(outs,'(2A)')
     &   '           id Hours   Rate ac/h m3/s   Rate ac/h m3/s  ',
     &   'Zone DegC'
        call edisp(itru,outs)
      endif

      DO 400 IDTY=1,NBDAYTYPE
        IF(NAC(IDTY).EQ.0)GOTO 400
        DO 1230 I=1,NAC(IDTY)
          acim = (VOL(ICOMP)*ACI(IDTY,I))/3600.0
          acvm = (VOL(ICOMP)*ACV(IDTY,I))/3600.0
          if(markdown)then
            WRITE(outs,
     &      '(a,i2,i3,a,i2,F8.2,8x,F8.4,8x,F8.2,8x,F8.4,I11,F13.2)')
     &      calentag(idty)(1:10),I,IACS(IDTY,I),' - ',IACF(IDTY,I),
     &      ACI(IDTY,I),acim,ACV(IDTY,I),acvm,IPT(IDTY,I),TA(IDTY,I)
            call edisp(itru,outs)
          else
            WRITE(outs,'(a,i2,i3,a,i2,F8.2,F8.4,F8.2,F8.4,I4,F9.2)')
     &      calentag(idty)(1:10),I,IACS(IDTY,I),' - ',IACF(IDTY,I),
     &      ACI(IDTY,I),acim,ACV(IDTY,I),acvm,IPT(IDTY,I),TA(IDTY,I)
            call edisp(itru,outs)
          endif
1230    CONTINUE
 400  CONTINUE
      RETURN
      END


C ******************* VENTCTLSTR 
C Provides short and long phrases decoding ventilation control. 
C act='L' low range comment, 'M' middle range comment,
C 'H' high range comment, 'S' setpoints.

      SUBROUTINE VENTCTLSTR(ICOMP,IDTY,act,vctlstr,vcomment)
#include "building.h"
#include "geometry.h"
#include "schedule.h"

      common/calena/calename,calentag(MDTY),calendayname(MDTY)
      CHARACTER CALENAME*32,CALENTAG*12,CALENDAYNAME*32
      common/caleni/nbdaytype,nbcaldays(MDTY),icalender(365)
      INTEGER NBDAYTYPE,NBCALDAYS,ICALENDER

C Markdown flag.
      logical markdown
      common/markdownflag/markdown

      character*24 vctlstr         ! Overall air flow control
      character*76 vcomment        ! Expanded comment
      character*1 act
      CHARACTER outs*124

      itru=6

      if(ip3ver(icomp).le.21)then

C Older file versions only display ctlstr for first day and period.
        acilm = (VOL(ICOMP)*ACIL(ICOMP))/3600.0
        acvlm = (VOL(ICOMP)*ACVL(ICOMP))/3600.0
        acium = (VOL(ICOMP)*ACIU(ICOMP))/3600.0
        acvum = (VOL(ICOMP)*ACVU(ICOMP))/3600.0
        acihm = (VOL(ICOMP)*ACIH(ICOMP))/3600.0
        acvhm = (VOL(ICOMP)*ACVH(ICOMP))/3600.0
        IY=ITCTL(ICOMP)

        if(IY.eq.-14)then
          vctlstr='infil only: wind speed  '
        elseif(IY.eq.-13)then
          vctlstr='infil only: ext db temp '
        elseif(IY.eq.-12)then
          vctlstr='infil only: adj zone tmp'
        elseif(IY.eq.-11)then
          vctlstr='infil only: zone temp   '
        elseif(IY.eq.-4)then
          vctlstr='vent only: wind speed   '
        elseif(IY.eq.-3)then
          vctlstr='vent only: ext db temp  '
        elseif(IY.eq.-2)then
          vctlstr='vent only: adj zone temp'
        elseif(IY.eq.-1)then
          vctlstr='vent only: zone temp    '
        elseif(IY.eq.4)then
          vctlstr='infil & vent: wind speed'
        elseif(IY.eq.3)then
          vctlstr='infil & vent: ext db T  '
        elseif(IY.eq.2)then
          vctlstr='infil & vent: adj zone T'
        elseif(IY.eq.1)then
          vctlstr='infil & vent: zone T    '
        elseif(IY.eq.0)then
          vctlstr='no control of air flow  '
        else
          vctlstr='unknown flow control    '
        endif

C Generate string for the lower, middle and high controls.
C ITCTL(ICOMP) values -11 to -14 relate to infiltration only controls.
        if(IY.eq.-14.or.IY.eq.-13.or.IY.eq.-12.or.IY.eq.-11)then
          if(act.eq.'L')then
            WRITE(vcomment,'(a,f7.2,a,F7.4,a)')
     &        'lower  setpoint',TLO(ICOMP),' infil (',acilm,'m^3/s)'
          elseif(act.eq.'M')then
            WRITE(vcomment,'(a,f7.2,a,f7.4,a)')
     &        'middle setpoint',TUP(ICOMP),' infil (',acium,'m^3/s)'
          elseif(act.eq.'H')then
            WRITE(vcomment,'(a,f7.2,a,f7.4,a)')
     &        'high  setpoint',THI(ICOMP),' infil (',acihm,'m^3/s)'
          elseif(act.eq.'S')then
            write(vcomment,'(3a)') 'control index (',
     &        vctlstr(1:lnblnk(vctlstr)),
     &        '), low middle high setpoints'
          endif

C ITCTL(ICOMP) values -1 to -4 relate to ventilation only controls.
        elseif(IY.eq.-4.or.IY.eq.-3.or.IY.eq.-2.or.IY.eq.-1)then
          if(act.eq.'L')then
            if(IVL(ICOMP).eq.0)then
              WRITE(vcomment,'(A,F7.2,a,F7.4,a,I3,a,F7.2)')
     &        'lower setpoint',TLO(ICOMP),' ventil (',
     &        acvlm,'m^3/s) ',IVL(ICOMP),' @',TAL(ICOMP)
            else
              WRITE(vcomment,'(A,F7.2,a,F7.4,3a,F7.2)')
     &        'lower setpoint',TLO(ICOMP),' ventil (',
     &        acvlm,'m^3/s) from ',zname(IVL(ICOMP))(1:10),
     &        ' @',TAL(ICOMP)
            endif
          elseif(act.eq.'M')then
            if(IVU(ICOMP).eq.0)then
              WRITE(vcomment,'(A,F7.2,a,F7.4,a,I3,a,F7.2)')
     &        'middle setpoint',TUP(ICOMP),' ventil (',
     &        acvum,'m^3/s) ',IVU(ICOMP),' @',TAU(ICOMP)
            else
              WRITE(vcomment,'(A,F7.2,a,F7.4,3a,F7.2)')
     &        'middle setpoint',TUP(ICOMP),' ventil (',
     &        acvum,'m^3/s) from ',zname(IVU(ICOMP))(1:10),
     &        ' @',TAU(ICOMP)
            endif
          elseif(act.eq.'H')then
            if(IVH(ICOMP).eq.0)then
              WRITE(vcomment,'(A,F7.2,a,F7.4,a,I3,a,F7.2)')
     &        'high setpoint',THI(ICOMP),' ventil (',
     &        acvhm,'m^3/s) ',IVH(ICOMP),' @',TAH(ICOMP)
            else
              WRITE(vcomment,'(A,F7.2,a,F7.4,3a,F7.2)')
     &        'high setpoint',THI(ICOMP),' ventil (',
     &        acvhm,'m^3/s) from ',zname(IVH(ICOMP))(1:10),
     &        ' @',TAH(ICOMP)
            endif
          elseif(act.eq.'S')then
            write(vcomment,'(3a)') 'control index (',
     &        vctlstr(1:lnblnk(vctlstr)),
     &        '), low middle high setpoints'
          endif

C ITCTL(ICOMP) values 1 to 4 relate to infiltration and ventilation controls.
        elseif(IY.eq.4.or.IY.eq.3.or.IY.eq.2.or.IY.eq.1)then
          if(act.eq.'L')then
            if(IVL(ICOMP).eq.0)then
              WRITE(vcomment,'(a,f7.2,a,F8.4,a,F8.4,a,I3,a,F7.2)')
     &        'lower setpoint',TLO(ICOMP),' infil (',acilm,
     &        'm^3/s) vent (',acvlm,'m^3/s) ',IVL(ICOMP),' @',
     &        TAL(ICOMP)
            else
              WRITE(vcomment,'(a,f7.2,a,F8.4,a,F8.4,3a,F7.2)')
     &        'lower setpoint',TLO(ICOMP),' infil (',acilm,
     &        'm^3/s) vent (',acvlm,'m^3/s) from ',
     &        zname(IVL(ICOMP))(1:10),' @',TAL(ICOMP)
            endif
          elseif(act.eq.'M')then
            if(IVU(ICOMP).eq.0)then
              WRITE(vcomment,'(a,f7.2,a,F8.4,a,F8.4,a,I3,a,F7.2)')
     &        'middle setpoint',TUP(ICOMP),' infil (',acium,
     &        'm^3/s) vent (',acvum,'m^3/s) ',IVU(ICOMP),' @',
     &        TAU(ICOMP)
            else
              WRITE(vcomment,'(a,f7.2,a,F8.4,a,F8.4,3a,F7.2)')
     &        'middle setpoint',TUP(ICOMP),' infil (',acium,
     &        'm^3/s) vent (',acvum,'m^3/s) from ',
     &        zname(IVU(ICOMP))(1:10),' @',TAU(ICOMP)
            endif
          elseif(act.eq.'H')then
            if(IVH(ICOMP).eq.0)then
              WRITE(vcomment,'(a,f7.2,a,F8.4,a,F8.4,a,I3,a,F7.2)')
     &        'high setpoint',THI(ICOMP),' infil (',acihm,
     &        'm^3/s) vent (',acvhm,'m^3/s) ',IVH(ICOMP),' @',
     &        TAH(ICOMP)
            else
              WRITE(vcomment,'(a,f7.2,a,F8.4,a,F8.4,3a,F7.2)')
     &        'high setpoint',THI(ICOMP),' infil (',acihm,
     &        'm^3/s) vent (',acvhm,'m^3/s) from ',
     &         zname(IVH(ICOMP))(1:10),' @',TAH(ICOMP)
            endif
          elseif(act.eq.'S')then
            write(vcomment,'(3a)') 'control index (',
     &        vctlstr(1:lnblnk(vctlstr)),
     &        '), low mid high setpoints'
          endif
        elseif(IY.eq.0)then
          if(act.eq.'S')then
            write(vcomment,'(3a)') 'control index (',
     &        vctlstr(1:lnblnk(vctlstr)),
     &        '), low middle high setpoints'
          endif
        endif
        return
      elseif(ip3ver(icomp).ge.30)then

C Display ctlstr for day and period << convert to multi days >>.
        IY=ITCTL(ICOMP)
        if(IY.eq.-14)then
          vctlstr='infil only: wind speed  '
        elseif(IY.eq.-13)then
          vctlstr='infil only: ext db temp '
        elseif(IY.eq.-12)then
          vctlstr='infil only: adj zone tmp'
        elseif(IY.eq.-11)then
          vctlstr='infil only: zone temp   '
        elseif(IY.eq.-4)then
          vctlstr='vent only: wind speed   '
        elseif(IY.eq.-3)then
          vctlstr='vent only: ext db temp  '
        elseif(IY.eq.-2)then
          vctlstr='vent only: adj zone temp'
        elseif(IY.eq.-1)then
          vctlstr='vent only: zone temp    '
        elseif(IY.eq.4)then
          vctlstr='infil & vent: wind speed'
        elseif(IY.eq.3)then
          vctlstr='infil & vent: ext db T  '
        elseif(IY.eq.2)then
          vctlstr='infil & vent: adj zone T'
        elseif(IY.eq.1)then
          vctlstr='infil & vent: zone T    '
        elseif(IY.eq.0)then
          vctlstr='no control of air flow  '
        else
          vctlstr='unknown flow control    '
        endif
        if(markdown)then
          write(outs,'(a,a)') 'Control ',vctlstr
          call edisp2tr(itru,outs)
        else
          write(outs,'(a,a)') ' Control: ',vctlstr
          call edisp(itru,outs)
        endif
        return
      endif
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

C Markdown flag.
      logical markdown
      common/markdownflag/markdown

C Electrical data flag and elctrical data.
      common/elecflg/ielf(mcom)
      COMMON/ELP3N/NEL(MDTY),PF(MDTY,MC),IPF(MDTY,MC),PWR(MDTY,MC),
     &BVOLT(MDTY,MC),IPHAS(MDTY,MC)
      INTEGER NEL,IPF,IPHAS,jicgt
      REAL PF,PWR,BVOLT

      CHARACTER outs*124
      CHARACTER llbl*24,llbl2*25

      call edisp(itru,' ')
      if(markdown)then
        continue ! these notes already shown in VENTINF
C        call edispxtr(itru,'Notes  ')
C        call edisp248(itru,oprdesc(icomp),100)
      else
        call edisp(itru,'Notes: ')
        call edisp248(itru,oprdesc(icomp),72)
      endif
      call edisp(itru,' ')
      if(IELF(ICOMP).EQ.1)then
        write(outs,'(3a)')'Daytype   Gain Label    Type    Unit',
     &    ' Period Sensible  Latent  Radiant  Convec   p.f +/- ',
     &    ' Power Voltage Phase'
        call edisp(itru,outs)
        write(outs,'(3a)')'          No.                       ',
     &    ' Hours  Magn.(W) Magn.(W) Frac     Frac     (-) (-) ',
     &    '   (W)    (V) (rgb)'
        call edisp(itru,outs)
      else  
        if(markdown)then
          write(outs,'(2a)')
     &    'Daytype   ID   Label      Type       Unit  Hours  Sens.(W)',
     &    '  Late.(W)   Rad. Frac   Con. Frac'
          call edisp(itru,outs)
          write(outs,'(2a)')
     &    '--------  ---  ---------  ---------  ----  -----  --------',
     &    '  --------   ---------   ---------'
          call edisp(itru,outs)
        else
          write(outs,'(2a)')'Daytype   Gain Label     Type    Unit',
     &    ' Period Sensible  Latent     Radiant    Convec'
          call edisp(itru,outs)
          write(outs,'(2a)')'          No.                        ',
     &    '  Hours Magn.(W)  Magn.(W)   Fraction   Fraction'
          call edisp(itru,outs)
        endif
      endif
      DO 600 IDTY=1,NBDAYTYPE
        if(NCAS(IDTY).eq.0) goto 600

        IF(NCAS(IDTY).GT.0)THEN
          IF(IELF(ICOMP).EQ.1) THEN
            DO 1269 I=1,NCAS(IDTY)
              jicgt=iabs(ICGT(IDTY,I))
              if(caskeytype(icomp,jicgt)(1:14).eq.'dynamic_people')then
                if(markdown)then
                  write(llbl2,'(4a)')lodlabel(ICOMP,jicgt)(1:10),
     &            ' ',caskeytype(icomp,jicgt)(1:10),' ocu'
                else
                  write(llbl,'(4a)')lodlabel(ICOMP,jicgt)(1:10),
     &            ' ',caskeytype(icomp,jicgt)(1:10),'ocu'
                endif
              else
                if(ICGT(IDTY,I).gt.0)then
                  if(markdown)then
                    write(llbl2,'(4a)')lodlabel(ICOMP,jicgt)(1:10),
     &              ' ',caskeytype(icomp,jicgt)(1:10),' W  '
                  else
                    write(llbl,'(4a)')lodlabel(ICOMP,jicgt)(1:10),
     &              ' ',caskeytype(icomp,jicgt)(1:10),'W  '
                  endif
                elseif(ICGT(IDTY,I).eq.-1)then
                  if(markdown)then
                    write(llbl2,'(4a)')lodlabel(ICOMP,jicgt)(1:10),
     &              ' ',caskeytype(icomp,jicgt)(1:10),' m2p'
                  else
                    write(llbl,'(4a)')lodlabel(ICOMP,jicgt)(1:10),
     &              ' ',caskeytype(icomp,jicgt)(1:10),'m2p'
                  endif
                elseif(ICGT(IDTY,I).lt.-1)then
                  if(markdown)then
                    write(llbl2,'(4a)')lodlabel(ICOMP,jicgt)(1:10),
     &              ' ',caskeytype(icomp,jicgt)(1:10),' Wm2'
                  else
                    write(llbl,'(4a)')lodlabel(ICOMP,jicgt)(1:10),
     &              ' ',caskeytype(icomp,jicgt)(1:10),'Wm2'
                  endif
                endif
              endif
              if(caskeytype(icomp,jicgt)(1:14).eq.'dynamic_people')then
                if(markdown)then
                  WRITE(outs,'(a,I3,1x,A,I3,a,I2,6F6.2,F8.2,F8.2,1x,
     &              F7.2,I3,F7.1,F7.1,I3)')
     &              calentag(idty)(1:10),I,llbl2,
     &              ICGS(IDTY,I),'-',ICGF(IDTY,I),
     &              tnbmen(IDTY,I),tnbwomen(IDTY,I),tnbchild(IDTY,I),
     &              metabolic(IDTY,I),clov(IDTY,I),airvel(IDTY,I),
     &              RADC(IDTY,I),CONC(IDTY,I),PF(IDTY,I),
     &              IPF(IDTY,I),PWR(IDTY,I),BVOLT(IDTY,I),IPHAS(IDTY,I)
                else
                  WRITE(outs,'(a,I3,1x,A,I3,a,I2,6F6.2,F8.2,F8.2,1x,
     &              F7.2,I3,F7.1,F7.1,I3)')
     &              calentag(idty)(1:10),I,llbl,
     &              ICGS(IDTY,I),'-',ICGF(IDTY,I),
     &              tnbmen(IDTY,I),tnbwomen(IDTY,I),tnbchild(IDTY,I),
     &              metabolic(IDTY,I),clov(IDTY,I),airvel(IDTY,I),
     &              RADC(IDTY,I),CONC(IDTY,I),PF(IDTY,I),
     &              IPF(IDTY,I),PWR(IDTY,I),BVOLT(IDTY,I),IPHAS(IDTY,I)
                endif
              else
                if(markdown)then
                  WRITE(outs,5361)calentag(idty)(1:10),I,llbl2,
     &              ICGS(IDTY,I),'-',ICGF(IDTY,I),CMGS(IDTY,I),
     &              CMGL(IDTY,I),RADC(IDTY,I),CONC(IDTY,I),PF(IDTY,I),
     &              IPF(IDTY,I),PWR(IDTY,I),BVOLT(IDTY,I),IPHAS(IDTY,I)
                else
                  WRITE(outs,5361)calentag(idty)(1:10),I,llbl,
     &              ICGS(IDTY,I),'-',ICGF(IDTY,I),CMGS(IDTY,I),
     &              CMGL(IDTY,I),RADC(IDTY,I),CONC(IDTY,I),PF(IDTY,I),
     &              IPF(IDTY,I),PWR(IDTY,I),BVOLT(IDTY,I),IPHAS(IDTY,I)
                endif
 5361           FORMAT(a,I3,1x,A,I3,a,I2,F9.1,F9.1,F8.2,F8.2,
     &            1x,F7.2,I3,F7.1,F7.1,I3)
              endif
              call edisp(itru,outs)
 1269       CONTINUE
          ELSE

C No electrical data.
            DO 1270 I=1,NCAS(IDTY)
              jicgt=iabs(ICGT(IDTY,I))
              if(jicgt.eq.0)then
                write(6,*) 'Error scanning caskeytype on idty ',
     &            idty,' gain ',I,' in zone ',ICOMP
                goto 1270
              endif
              if(caskeytype(icomp,jicgt)(1:14).eq.'dynamic_people')then
                if(markdown)then
                  write(llbl2,'(4a)')lodlabel(ICOMP,jicgt)(1:10),
     &            ' ',caskeytype(icomp,jicgt)(1:10),' ocu'
                else
                  write(llbl,'(4a)')lodlabel(ICOMP,jicgt)(1:10),
     &            ' ',caskeytype(icomp,jicgt)(1:10),'ocu'
                endif
              else
                if(ICGT(IDTY,I).gt.0)then
                  if(markdown)then
                    write(llbl2,'(4a)')lodlabel(ICOMP,jicgt)(1:10),
     &              ' ',caskeytype(icomp,jicgt)(1:10),' W  '
                  else
                    write(llbl,'(4a)')lodlabel(ICOMP,jicgt)(1:10),
     &              ' ',caskeytype(icomp,jicgt)(1:10),'W  '
                  endif
                elseif(ICGT(IDTY,I).eq.-1)then
                  if(markdown)then
                    write(llbl2,'(4a)')lodlabel(ICOMP,jicgt)(1:10),
     &              ' ',caskeytype(icomp,jicgt)(1:10),' m2p'
                  else
                    write(llbl,'(4a)')lodlabel(ICOMP,jicgt)(1:10),
     &              ' ',caskeytype(icomp,jicgt)(1:10),'m2p'
                  endif
                elseif(ICGT(IDTY,I).lt.-1)then
                  if(markdown)then
                    write(llbl2,'(4a)')lodlabel(ICOMP,jicgt)(1:10),
     &              ' ',caskeytype(icomp,jicgt)(1:10),' Wm2'
                  else
                    write(llbl,'(4a)')lodlabel(ICOMP,jicgt)(1:10),
     &              ' ',caskeytype(icomp,jicgt)(1:10),'Wm2'
                  endif
                endif
              endif
              if(caskeytype(icomp,jicgt)(1:14).eq.'dynamic_people')then
                if(markdown)then
                  WRITE(outs,'(a,I4,2x,A,I4,a,I2,6F6.2,F11.2,F11.2)')
     &              calentag(idty)(1:10),I,llbl2,
     &              ICGS(IDTY,I),'-',ICGF(IDTY,I),
     &              tnbmen(IDTY,I),tnbwomen(IDTY,I),tnbchild(IDTY,I),
     &              metabolic(IDTY,I),clov(IDTY,I),airvel(IDTY,I),
     &              RADC(IDTY,I),CONC(IDTY,I)
                else
                  WRITE(outs,'(a,I3,1x,A,I3,a,I2,6F6.2,F11.2,F11.2)')
     &              calentag(idty)(1:10),I,llbl,
     &              ICGS(IDTY,I),'-',ICGF(IDTY,I),
     &              tnbmen(IDTY,I),tnbwomen(IDTY,I),tnbchild(IDTY,I),
     &              metabolic(IDTY,I),clov(IDTY,I),airvel(IDTY,I),
     &              RADC(IDTY,I),CONC(IDTY,I)
                endif
              else
                if(markdown)then
                  WRITE(outs,
     &              '(a,I3,2x,A,I5,a,I2,F10.1,F9.1,F11.2,F11.2)')
     &              calentag(idty)(1:10),I,llbl2,
     &              ICGS(IDTY,I),'-',ICGF(IDTY,I),CMGS(IDTY,I),
     &              CMGL(IDTY,I),RADC(IDTY,I),CONC(IDTY,I)
                else
                  WRITE(outs,
     &              '(a,I3,1x,A,I3,a,I2,F9.1,F9.1,F11.2,F11.2)')
     &              calentag(idty)(1:10),I,llbl,
     &              ICGS(IDTY,I),'-',ICGF(IDTY,I),CMGS(IDTY,I),
     &              CMGL(IDTY,I),RADC(IDTY,I),CONC(IDTY,I)
                endif
              endif
              call edisp(itru,outs)
1270        CONTINUE
          ENDIF
          if(NBDAYTYPE.gt.3)then
            if(.NOT.markdown) call edisp(itru,' ')  ! if long list
          endif
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

C      COMMON/OUTIN/IUOUT,IUIN,IEOUT
      common/caleni/nbdaytype,nbcaldays(MDTY),icalender(365)
      INTEGER NBDAYTYPE,NBCALDAYS,ICALENDER,IDTY

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

C << overloading of icgt to be revised >>
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

C Determine if any of the casual gains were mixed (i.e. different units
C at different periods).
C << ?? for slot 4 >>
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

      INTEGER IDTY

C Electrical data flag and elctrical data.
      common/elecflg/ielf(mcom)
      COMMON/ELP3N/NEL(MDTY),PF(MDTY,MC),IPF(MDTY,MC),PWR(MDTY,MC),
     &BVOLT(MDTY,MC),IPHAS(MDTY,MC)
      INTEGER NEL,IPF,IPHAS
      REAL PF,PWR,BVOLT

      IDTY=IDAYTYPE
      ICGT(IDTY,ix)=ICGT(IDTY,jx)
      ICGUnit(IDTY,ix)=ICGUnit(IDTY,jx)
      ICGS(IDTY,ix)=ICGS(IDTY,jx)
      ICGF(IDTY,ix)=ICGF(IDTY,jx)
      CMGS(IDTY,ix)=CMGS(IDTY,jx)
      CMGL(IDTY,ix)=CMGL(IDTY,jx)
      RADC(IDTY,ix)=RADC(IDTY,jx)
      CONC(IDTY,ix)=CONC(IDTY,jx)
      tnbmen(IDTY,ix)=tnbmen(IDTY,jx)
      tnbwomen(IDTY,ix)=tnbwomen(IDTY,jx)
      tnbchild(IDTY,ix)=tnbchild(IDTY,jx)
      metabolic(IDTY,ix)=metabolic(IDTY,jx)
      clov(IDTY,ix)=clov(IDTY,jx)
      airvel(IDTY,ix)=airvel(IDTY,jx)
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
      SUBROUTINE CPYCASIT(icomp,idaytype,jx,icgtX,icguX,icgsX,icgfX,
     &  cmgsX,cmglX,radcX,concX,ipfX,iphasX,pfX,pwrX,bvoltX,
     &  tnbmenx,tnbwomenx,tnbchildx,metabolicx,clox,airvelx)
#include "building.h"
#include "schedule.h"

C Parameters
      integer icomp    ! current zone
      integer idaytype ! day type to sort
      integer jx       ! is the source
      integer icgtX,icguX,icgsX,icgfX    ! temporary variables
      real cmgsX,cmglX,radcX,concX ! temporary variables
      integer ipfX,iphasX          ! temporary variables
      real pfX,pwrX,bvoltX         ! temporary variables
      real airvelx,tnbmenx,tnbwomenx,tnbchildx,metabolicx,clox

C Electrical data flag and elctrical data.
      common/elecflg/ielf(mcom)
      COMMON/ELP3N/NEL(MDTY),PF(MDTY,MC),IPF(MDTY,MC),PWR(MDTY,MC),
     &BVOLT(MDTY,MC),IPHAS(MDTY,MC)
      INTEGER NEL,IPF,IPHAS
      REAL PF,PWR,BVOLT

      IDTY=IDAYTYPE
      ICGTX=ICGT(IDTY,jx)
      ICGUX=ICGUnit(IDTY,jx)
      ICGSX=ICGS(IDTY,jx)
      ICGFX=ICGF(IDTY,jx)
      CMGSX=CMGS(IDTY,jx)
      CMGLX=CMGL(IDTY,jx)
      RADCX=RADC(IDTY,jx)
      CONCX=CONC(IDTY,jx)
      tnbmenx=tnbmen(IDTY,jx)
      tnbwomenx=tnbwomen(IDTY,jx)
      tnbchildx=tnbchild(IDTY,jx)
      metabolicx=metabolic(IDTY,jx)
      clox=clov(IDTY,jx)
      airvelx=airvel(IDTY,jx)
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
      SUBROUTINE CPYCASTI(icomp,idaytype,jx,icgtX,icguX,icgsX,icgfX,
     &  cmgsX,cmglX,radcX,concX,ipfX,iphasX,pfX,pwrX,bvoltX,
     &  tnbmenx,tnbwomenx,tnbchildx,metabolicx,clox,airvelx)
#include "building.h"
#include "schedule.h"

C Parameters
      integer icomp    ! current zone
      integer idaytype ! day type to sort
      integer jx       ! is the source
      integer icgtX,icguX,icgsX,icgfX    ! temporary variables
      real cmgsX,cmglX,radcX,concX ! temporary variables
      integer ipfX,iphasX          ! temporary variables
      real pfX,pwrX,bvoltX         ! temporary variables
      real airvelx,tnbmenx,tnbwomenx,tnbchildx,metabolicx,clox

C Electrical data flag and elctrical data.
      common/elecflg/ielf(mcom)
      COMMON/ELP3N/NEL(MDTY),PF(MDTY,MC),IPF(MDTY,MC),PWR(MDTY,MC),
     &BVOLT(MDTY,MC),IPHAS(MDTY,MC)
      INTEGER NEL,IPF,IPHAS
      REAL PF,PWR,BVOLT

      IDTY=IDAYTYPE
      ICGT(IDTY,jx)=ICGTX
      ICGUnit(IDTY,jx)=ICGUX
      ICGS(IDTY,jx)=ICGSX
      ICGF(IDTY,jx)=ICGFX
      CMGS(IDTY,jx)=CMGSX
      CMGL(IDTY,jx)=CMGLX
      RADC(IDTY,jx)=RADCX
      CONC(IDTY,jx)=CONCX
      tnbmen(IDTY,jx)=tnbmenx
      tnbwomen(IDTY,jx)=tnbwomenx
      tnbchild(IDTY,jx)=tnbchildx
      metabolic(IDTY,jx)=metabolicx
      clov(IDTY,jx)=clox
      airvel(IDTY,jx)=airvelx
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
C PROCESSOLDCAS: Processes gains in an old format operations file to remove
C overlaps. Passed the zone index.
      SUBROUTINE PROCESSOLDCAS(ICOMP,ITRC,ITRU)
#include "building.h"
#include "geometry.h"
#include "schedule.h"

      COMMON/OUTIN/IUOUT,IUIN,IEOUT
      common/caleni/nbdaytype,nbcaldays(MDTY),icalender(365)
      INTEGER NBDAYTYPE,NBCALDAYS,ICALENDER,idty,idt
      common/calena/calename,calentag(MDTY),calendayname(MDTY)
      CHARACTER CALENAME*32,CALENTAG*12,CALENDAYNAME*32

C Commons for electrical data
      COMMON/ELECFLG/IELF(MCOM)
      COMMON/ELP3N/NEL(MDTY),PF(MDTY,MC),IPF(MDTY,MC),PWR(MDTY,MC),
     &BVOLT(MDTY,MC),IPHAS(MDTY,MC)
      INTEGER NEL,IPF,IPHAS,istarthr,ifinishr,ip
      REAL PF,PWR,BVOLT

C loadcount(zone,gaintype,daytype) - number of each gain type (whether
C   absolute or W/m2). Up to MGTY casual gain types are supported.
C loadm2count(zone,gaintype,daytype) - number of each m2 gain types.
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
C ICGTYP the casual gain slot (negative index to be depreciated).
C ICGUn the units of the current period (not yet fully implemented)
C PERSN keeps track of the original number of people/m2.
      real CMGSA,CMGLA,RADCA,CONCA,RADFAC,CONFAC,PERSN
      integer ICGTYP,LNCAS
      dimension CMGSA(MC,MGTY),CMGLA(MC,MGTY),RADCA(MC,MGTY)
      dimension CONCA(MC,MGTY),RADFAC(MC,MGTY),CONFAC(MC,MGTY)
      dimension ICGTYP(MC,MGTY),PERSN(MC,MGTY),LNCAS(MDTY)
C      dimension ICGUn(MC,MGTY)

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

C Find how many in each casual gain slot for each day type as well
C as whether each profile needs to be absolute (casual gain type positive),
C or based on area (casual gain type negative) or if mixed +-.
      call checkcascount(icomp)
      
C Zero derived gains in the temporary array and set the type as per
C the call to checkcascount.

C << overloading of ICGT below review logic in loadmixed >>
      do 130 IP=1,MC
        do 120 IG=1,MGTY
          CMGSA(IP,IG)=0.0
          CMGLA(IP,IG)=0.0
          RADCA(IP,IG)=0.0
          CONCA(IP,IG)=0.0
          RADFAC(IP,IG)=0.50  ! should refine based on gain type
          CONFAC(IP,IG)=0.50
          DO 609 IDTY=1,NBDAYTYPE
            ISTARTHR=(IDTY-1)*24+1
            IFINISHR=IDTY*24
            if(ip.ge.ISTARTHR.and.ip.le.IFINISHR)then
              if(loadmixed(icomp,ig,IDTY).eq.2)then
                ICGTYP(IP,IG)= (-1)*IG
C                ICGUn(IP,IG) = ?
              else
                ICGTYP(IP,IG)= IG
C                ICGUn(IP,IG) = ?
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

C << overloading of ICGT below >>
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

C << overloading of ICGT here >>
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

C << overloading of ICGT >>
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

C Assign common block values. If a new period with zero sensible then set
C the radiant convective split based on its type.

C << overloading of ICGT here also a place to set ICGUNIT later try to avoid overload >>
            ICGT(IDT,NCASwk) = ICGTYP(IP,ITYP)
            if(ICGT(IDT,NCASwk).gt.0)then
              ICGUnit(IDT,NCASwk)=0
            elseif(ICGT(IDT,NCASwk).eq.-1)then
              ICGUnit(IDT,NCASwk)=2
            elseif(ICGT(IDT,NCASwk).lt.-1)then
              ICGUnit(IDT,NCASwk)=1
            endif
            NCAS(IDT) = NCASwk
            ICGS(IDT,NCASwk) = ihst
            ICGF(IDT,NCASwk) = ihcg
            CMGS(IDT,NCASwk) = CMGSA(IP,ITYP)
            CMGL(IDT,NCASwk) = CMGLA(IP,ITYP)
            if(CMGSA(IP,ITYP).gt.0.0)then
              RADC(IDT,NCASwk) = RADFAC(IP,ITYP)
              CONC(IDT,NCASwk) = CONFAC(IP,ITYP)
            else

C << place for caskeytype, lodslot & ICGUnit >>
              ITYPL=abs(ICGTYP(IP,ITYP))
              if(ITYPL.eq.1)then
                RADC(IDT,NCASwk)=0.6; CONC(IDT,NCASwk)=0.4
              elseif(ITYPL.eq.2)then
                RADC(IDT,NCASwk)=0.3; CONC(IDT,NCASwk)=0.7
              elseif(ITYPL.eq.3)then
                RADC(IDT,NCASwk)=0.4; CONC(IDT,NCASwk)=0.6
              else
                RADC(IDT,NCASwk)=0.5; CONC(IDT,NCASwk)=0.5
              endif
            endif

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

      COMMON/OUTIN/IUOUT,IUIN,IEOUT

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
      n4=loadcount(icomp,4,IDTY)

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
      if(n4.gt.0.and.(.NOT.load24(4,idaytype,1)))then
        write(outs,*)'initial 4th slot period not zero in zone ',icomp,
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
      if(n4.gt.0.and.(.NOT.load24(4,idaytype,2)))then
        write(outs,*) 'last small pwr period not zero in zone ',icomp,
     &    ' daytype ',idaytype
        call edisp(iuout,outs)
        problem = .true.
      endif

      return
      end


C ******************** UPDOPR 
C UPDOPR  Updates all operations data when day types are incremented.

      SUBROUTINE UPDOPR(iudayt)
#include "building.h"
#include "model.h"
#include "schedule.h"
#include "help.h"

      integer iudayt    ! user preference 0 create default one period or
                        ! >0 copy an existing day type pattern.
      COMMON/OUTIN/IUOUT,IUIN,IEOUT
      common/FILEP/IFIL
      
      integer ncomp,ncon
      COMMON/C1/NCOMP,NCON
      common/caleni/nbdaytype,nbcaldays(MDTY),icalender(365)
      INTEGER NBDAYTYPE,NBCALDAYS,ICALENDER

      COMMON/ELP3N/NEL(MDTY),PF(MDTY,MC),IPF(MDTY,MC),PWR(MDTY,MC),
     &BVOLT(MDTY,MC),IPHAS(MDTY,MC)
      INTEGER NEL,IPF,IPHAS
      REAL PF,PWR,BVOLT

      INTEGER ITRC,ITRU,IUO,ICOMP,IER,ISTAT,NBDAYTYPEHOLD
      LOGICAL XST

C Initialise local variables.
      ITRC=0
      ITRU=0
      IUO=IFIL+1
      IER=0
      ISTAT=0
      XST=.FALSE.
      NBDAYTYPEHOLD=NBDAYTYPE  ! remember NBDAYTYPE

      helpinsub='eroper'  ! set for subroutine
      helptopic='update_schedule_daytype'
      call gethelptext(helpinsub,helptopic,nbhelp)

C For every zone in the model do the following...
      DO 100 ICOMP=1,NCOMP

C Check if operations file exists
        INQUIRE (FILE=LPROJ(ICOMP),EXIST=XST)
        IF(XST)THEN

C Display help 
          IF(ICOMP.EQ.1)CALL PHELPD('Information',9,'-',0,0,IER)

C Read operations file for this zone. Temporarily decrement NBDAYTYPE
C to be able to scan existing operation files.
          NBDAYTYPE=NBDAYTYPE-1
          CALL ERPFREE(IUO,ISTAT)
          CALL EROPER(ITRC,ITRU,IUO,ICOMP,IER)
          NBDAYTYPE=NBDAYTYPE+1
          IF(IER.NE.0)THEN
            CALL EDISP(IUOUT,'UPDOPR Error reading file')
            RETURN
          ENDIF

C Initialise the air flows and casual gains to zero for new day type but set other
C values as typical.
          if(iudayt.le.0)then        ! set placeholder infiltration & ventilation
            NAC(NBDAYTYPE)=1
            DO I=NAC(NBDAYTYPE-1),MA
              IACS(NBDAYTYPE,I)=0
              IACF(NBDAYTYPE,I)=24
              ACI(NBDAYTYPE,I)=0.
              ACV(NBDAYTYPE,I)=0.
              IPT(NBDAYTYPE,I)=0
              TA(NBDAYTYPE,I)=0.
            ENDDO
          else
            NAC(NBDAYTYPE)=NAC(iudayt) ! Copy pattern from iudayt existing type.
            DO I=1,NAC(iudayt)
              IACS(NBDAYTYPE,I)=IACS(iudayt,I)
              IACF(NBDAYTYPE,I)=IACF(iudayt,I)
              ACI(NBDAYTYPE,I)=ACI(iudayt,I)
              ACV(NBDAYTYPE,I)=ACV(iudayt,I)
              IPT(NBDAYTYPE,I)=IPT(iudayt,I)
              TA(NBDAYTYPE,I)=TA(iudayt,I)
            ENDDO
          endif
          if(iudayt.le.0)then          ! set placeholder casual gains
            NCAS(NBDAYTYPE)=3
            DO I=NCAS(NBDAYTYPE-1),MC
              ICGT(NBDAYTYPE,I)=0
              ICGS(NBDAYTYPE,I)=0
              ICGF(NBDAYTYPE,I)=0
              ICGUnit(NBDAYTYPE,I)=0   ! Assume Watts
              CMGS(NBDAYTYPE,I)=0.
              CMGL(NBDAYTYPE,I)=0.
              RADC(NBDAYTYPE,I)=0.
              CONC(NBDAYTYPE,I)=0.
              PF(NBDAYTYPE,I)=0.
              IPF(NBDAYTYPE,I)=0
              PWR(NBDAYTYPE,I)=0.
              BVOLT(NBDAYTYPE,I)=0.
              IPHAS(NBDAYTYPE,I)=0
            ENDDO
          else
            NCAS(NBDAYTYPE)=NCAS(iudayt) ! copy pattern from iudayt
            DO I=1,NCAS(iudayt)
              ICGT(NBDAYTYPE,I)=ICGT(iudayt,I)
              ICGS(NBDAYTYPE,I)=ICGS(iudayt,I)
              ICGF(NBDAYTYPE,I)=ICGF(iudayt,I)
              ICGUnit(NBDAYTYPE,I)=ICGUnit(iudayt,I)
              CMGS(NBDAYTYPE,I)=CMGS(iudayt,I)
              CMGL(NBDAYTYPE,I)=CMGL(iudayt,I)
              RADC(NBDAYTYPE,I)=RADC(iudayt,I)
              CONC(NBDAYTYPE,I)=CONC(iudayt,I)
              PF(NBDAYTYPE,I)=PF(iudayt,I)
              IPF(NBDAYTYPE,I)=IPF(iudayt,I)
              PWR(NBDAYTYPE,I)=PWR(iudayt,I)
              BVOLT(NBDAYTYPE,I)=BVOLT(iudayt,I)
              IPHAS(NBDAYTYPE,I)=IPHAS(iudayt,I)
            ENDDO
          endif

C For placeholder variant, make initial three gain periods (0-24) for occupancy,
C lights and small power. 
          if(iudayt.le.0)then        ! set placeholder infiltration & ventilation
            DO 400 I=1,NCAS(NBDAYTYPE)
              ICGT(NBDAYTYPE,I)=I
              ICGF(NBDAYTYPE,I)=24
              if(I.eq.1)then
                RADC(NBDAYTYPE,I)=0.6
                CONC(NBDAYTYPE,I)=0.4
              elseif(I.eq.2)then
                RADC(NBDAYTYPE,I)=0.5
                CONC(NBDAYTYPE,I)=0.5
              elseif(I.eq.3)then
                RADC(NBDAYTYPE,I)=0.4
                CONC(NBDAYTYPE,I)=0.6
              endif
 400        CONTINUE
          endif

          ip3ver(icomp)=21   !  Write zone operations file.
          CALL EMKOPER(IUO,LPROJ(ICOMP),ICOMP,IER)
        ENDIF
 100  CONTINUE
      NBDAYTYPE=NBDAYTYPEHOLD  ! restore the origianl nb of day types
      RETURN
      END

      
C ******************** PIKCGTYP
C Offers the user a choice of currently defined casual gain types.
C Returns IWHICH=0 if cancel is selected or if there are no gain types.

      SUBROUTINE PIKCGTYP(ICOMP,IWHICH,NBHELP)

#include "building.h"
#include "schedule.h"

      COMMON/OUTIN/IUOUT,IUIN,IEOUT

      character*12 ll1,ll2,ll3,ll4
      character outs*124
      logical ok
      
      ok=.false.
      if (lodslot(icomp,1).gt.0) then
        write(ll1,'(a)') lodlabel(icomp,1)
        ok=.true.
      else
        write(ll1,'(a)') '-'
      endif
      if (lodslot(icomp,2).gt.0) then
        write(ll2,'(a)') lodlabel(icomp,2)
        ok=.true.
      else
        write(ll2,'(a)') '-'
      endif
      if (lodslot(icomp,3).gt.0) then
        write(ll3,'(a)') lodlabel(icomp,3)
        ok=.true.
      else
        write(ll3,'(a)') '-'
      endif
      if (lodslot(icomp,4).gt.0) then
        write(ll4,'(a)') lodlabel(icomp,4)
        ok=.true.
      else
        write(ll4,'(a)') '-'
      endif
      if (ok) then
        itmp=IWHICH
        CALL EASKATOG('Casual gain type?',' ',ll1,ll2,ll3,ll4,'cancel',
     &    ' ',' ',itmp,nbhelp)
        if (itmp.eq.5) then
          IWHICH=0
        else
          IWHICH=itmp
        endif
      else
        outs='No casual gain types defined for this zone.'
        call edisp(IUOUT,outs)
        IWHICH=0
      endif

      return
      end
