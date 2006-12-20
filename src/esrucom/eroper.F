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

C eroper.f provides the following facilities:
C  EROPER:   Reads all zone project data from a user-constructed
C            datafile.
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

C ******************** EROPER 
C EROPER reads all zone project data from a user-constructed
C datafile.

C Common block variables are:

C oprdesc      - operation notes (248 char)

C ITCTL        - Thermostatic control index :
C                    control on zone coupled air temperature
C                    control on zone air temperature
C                  0 no control
C                    control on zone air temperature and infiltration
C                    control on zone coupled air and infiltration
C ACIL, ACVL,  - low level (i.e. if below a setpoint) for control.
C IVL, TAL     
C ACIU, ACVU,  - mid level (i.e. if above a setpoint) 1st stage options
C IVU, TAU       for control.
C ACIH, ACVH,  - high level (2nd stage options) for control.
C IVH, TAH     
C NAC1, NAC2 & - number of distinct air change periods during Weekdays,
C NAC3           Saturdays and Sundays respectively.

C IACS1,IACF1, - start and finish hours of each of the above air change
C IACS2,IACF2,   periods relating to Weekdays, Saturdays and Sundays
C IACS3 & IACF3  respectively.

C ACI1, ACI2   - natural infiltration air changes/hour for each period
C & ACI3         relating to Weekdays, Saturdays and Sundays
C                respectively.

C ACV1, ACV2   - additional incoming air changes/hour for each period
C & ACV3         relating to Weekdays, Saturdays and Sundays
C                respectively.

C IPT1, IPT2   - the additional incoming air (corresponding to ACV1,
C & IPT3         ACV2 & ACV3) can either be at constant temperature -
C                changing, if required, between each period - or set
C                at the time-dependent temperature of some coupled
C                zone. The IPT? variable controls this, where:
C                 IPT?=0 signifies a constant temperature will be specified
C                 IPT?=N (N>0) signifies that incoming air is at the
C                        time-dependent temperature of zone N.

C TA1,TA2 &    - correspond to IPT?=0 and define the temperature of the
C TA3            incoming air for each period relating to Weekdays,
C                Saturdays and Sundays respectively.

C NCAS1,NCAS2  - number of casual gains during a typical Weekday,
C & NCAS3        Saturday and Sunday respectively.

C ICGT1,ICGT2  - Casual gain type:
C ICG3T           1. Occupancy
C                 2. Lights
C                 3. Equipment
C                 4. User controllable gain (not fully utilised as yet)
C                -1. Occupancy as floor area per person
C                -2. Lights in w/m^2 per floor area
C                -3. Equipment in w/m^2 per floor area

C ICGS1,ICGF1, - start and finish hours of each of the above casual gain
C ICGS2,ICGF2,   periods
C ICGS3 & ICGF3

C CMGS1,CMGL1, - sensible and latent magnitude (in Watts) of
C CMGS2,CMGL2,   each casual gain
C CMGS3 & CMGL3

C RADC1,CONC1, - radiant and convective portions (proportion
C RADC2,CONC2,   of 1) of each casual gain
C RADC3,CONC3

C pf1-3        - power factor of electrical load.
C ipf1-3       - nature of load lagging (reactive), leading (capacative),
C                or unity (pure resistive).
C pwr1-3       - real power consumption of the load (W).
C bvolt1-3     - operational voltage of the load.
C iphas1-3     - which phase the load is connected to (1-3) or
C                all 3 (4).

C Maximum number of air change periods/day MA=5
C Maximum number of casual gains/day       MC=20

      SUBROUTINE EROPER(ITRC,ITRU,IUO,ICOMP,IER)
#include "building.h"

      COMMON/OUTIN/IUOUT,IUIN
      COMMON/C1/NCOMP,NCON
      COMMON/C2/LSNAM,NCCODE(MCOM),LPROJ(MCOM),LGEOM(MCOM),
     &          LSHAD(MCOM),LTHRM(MCOM),INDUTL(MCOM),LUTIL(MCOM)

      COMMON/P1/oprdesc,ctlstr
      COMMON/P2/NAC1,IACS1(MA),IACF1(MA),ACI1(MA),ACV1(MA),
     &          IPT1(MA),TA1(MA),NAC2,IACS2(MA),IACF2(MA),ACI2(MA),
     &          ACV2(MA),IPT2(MA),TA2(MA),NAC3,IACS3(MA),IACF3(MA),
     &          ACI3(MA),ACV3(MA),IPT3(MA),TA3(MA)
      COMMON/P2CTL/ITCTL(MCOM),TLO(MCOM),TUP(MCOM),THI(MCOM),ACIL(MCOM),
     &             ACVL(MCOM),IVL(MCOM),TAL(MCOM),ACIU(MCOM),ACVU(MCOM),
     &             IVU(MCOM),TAU(MCOM),ACIH(MCOM),ACVH(MCOM),
     &             IVH(MCOM),TAH(MCOM)
      COMMON/P3/NCAS1,ICGS1(MC),ICGF1(MC),CMGS1(MC),CMGL1(MC),
     &          RADC1(MC),CONC1(MC),NCAS2,ICGS2(MC),ICGF2(MC),CMGS2(MC),
     &          CMGL2(MC),RADC2(MC),CONC2(MC),NCAS3,ICGS3(MC),ICGF3(MC),
     &          CMGS3(MC),CMGL3(MC),RADC3(MC),CONC3(MC)
      COMMON/P3TYPE/ICGT1(MC),ICGT2(MC),ICGT3(MC)

C Version of operations file. ip3ver=0 standard, =1 sorted with header
      common/p3ver/ip3ver

C Electrical data flag and elctrical data.
      common/elecflg/ielf(mcom)
      common/elp3/nel1,pf1(mc),ipf1(mc),pwr1(mc),bvolt1(mc),iphas1(mc),
     &            nel2,pf2(mc),ipf2(mc),pwr2(mc),bvolt2(mc),iphas2(mc),
     &            nel3,pf3(mc),ipf3(mc),pwr3(mc),bvolt3(mc),iphas3(mc)

C Descriptive label for a zone load or casual gain types.
      common/loadlabel/lodlabel(mcom,7)

C Current file (for use by low level I/O calls). Error subroutine
C and error details for dll mode.
      common/curfile/currentfile
      common/dllerr/dllsubr,dllmesg

      CHARACTER oprdesc*248,ctlstr*24,outstr*124,outs*124,lodlabel*6
      CHARACTER*72 LSNAM,LPROJ,LGEOM,LSHAD,LUTIL,LTHRM,currentfile
      character dllsubr*12,dllmesg*124,WORD*20,loutstr*248
      character dstmp*24
      logical havehi,dll

      IER=0

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
      currentfile=LPROJ(ICOMP)
      ip3ver=0

C Initialise the iphase arrays to avoid zero array elements in precal.
C Initialise the following arrays. This is important
C in case no electrical data was specified becasuse they
C are later used in precal.F as indexing arrays.
      do 909 iph=1, mc
        iphas1(iph)=1
        iphas2(iph)=1
        iphas3(iph)=1
 909  continue

C Read lines from file, discarding comments. Look for header on
C newer files. Older files will begin with a description. To allow for
C description with spaces copy directly from OUTSTR rather than parsing
C it into words.  
      CALL STRIPC(IUO,OUTSTR,0,ND,1,'oper name or header',IER)
      IF(IER.NE.0) goto 1002
      if(outstr(1:11).eq.'*Operations')then
        if(outstr(13:15).eq.'1.0')then
          ip3ver=1
        elseif(outstr(13:15).eq.'2.0')then
          ip3ver=2
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
        oprdesc=LOUTSTR
      else
        oprdesc=OUTSTR
      endif

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
        ctlstr='infil only: wind speed  '
      elseif(IY.eq.-13)then
        ctlstr='infil only: ext db temp '
      elseif(IY.eq.-12)then
        ctlstr='infil only: adj zone tmp'
      elseif(IY.eq.-11)then
        ctlstr='infil only: zone temp   '
      elseif(IY.eq.-4)then
        ctlstr='vent only: wind speed   '
      elseif(IY.eq.-3)then
        ctlstr='vent only: ext db temp  '
      elseif(IY.eq.-2)then
        ctlstr='vent only: adj zone temp'
      elseif(IY.eq.-1)then
        ctlstr='vent only: zone temp    '
      elseif(IY.eq.4)then
        ctlstr='infil & vent: wind speed'
      elseif(IY.eq.3)then
        ctlstr='infil & vent: ext db T  '
      elseif(IY.eq.2)then
        ctlstr='infil & vent: adj zone T'
      elseif(IY.eq.1)then
        ctlstr='infil & vent: zone T    '
      elseif(IY.eq.0)then
        ctlstr='no control of air flow  '
        goto 22
      else
        ctlstr='unknown flow control   '
        goto 1022
      endif

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

C Weekdays.
   22 CALL STRIPC(IUO,OUTSTR,0,ND,1,'Weekday flow periods',IER)
      K=0
      CALL EGETWI(OUTSTR,K,NAC1,0,MA,'W','Weekday periods',IER)
      IF(NAC1.EQ.0)goto 2
      DO 10 I=1,NAC1
        CALL STRIPC(IUO,OUTSTR,0,ND,1,'Period flow details',IER)
        K=0
        CALL EGETWI(OUTSTR,K,IACS1(I),0,24,'W','w flow start',IER)
        CALL EGETWI(OUTSTR,K,IACF1(I),0,24,'W','w flow end',IER)
        CALL EGETWR(OUTSTR,K,ACI1(I),0.,2000.,'W','w infil',IER)
        CALL EGETWR(OUTSTR,K,ACV1(I),0.,2000.,'W','w vent',IER)
        CALL EGETWI(OUTSTR,K,IPT1(I),0,NCOMP,'W','w vent z',IER)
        CALL EGETWR(OUTSTR,K,TA1(I),0.,0.,'-','w vent tmp',IER)
        IF(IACS1(I).GT.IACF1(I))then

C Found a period out of order, warn user and carry on.
          write(outs,'(2a)')' Weekday vent start-end mismatch in...',
     &      outstr(1:50)
          call edisp(iuout,outs)
          write(outs,'(2a)')' of operation file ',currentfile
          call edisp(iuout,outs)
          IER=1
        endif
        IF(IPT1(I).EQ.ICOMP)GOTO 1009
   10 CONTINUE

C Saturdays.
   2  CALL STRIPC(IUO,OUTSTR,0,ND,1,'Saturday flow periods',IER)
      K=0
      CALL EGETWI(OUTSTR,K,NAC2,0,MA,'W','Sat periods',IER)
      IF(NAC2.EQ.0)GOTO 3
      DO 20 I=1,NAC2
        CALL STRIPC(IUO,OUTSTR,0,ND,1,'Period flow details',IER)
        K=0
        CALL EGETWI(OUTSTR,K,IACS2(I),0,24,'W','Sat flow str',IER)
        CALL EGETWI(OUTSTR,K,IACF2(I),0,24,'W','Sat flow end',IER)
        CALL EGETWR(OUTSTR,K,ACI2(I),0.,2000.,'W','Sat infil',IER)
        CALL EGETWR(OUTSTR,K,ACV2(I),0.,2000.,'W','Sat vent',IER)
        CALL EGETWI(OUTSTR,K,IPT2(I),0,NCOMP,'W','Sat vent z',IER)
        CALL EGETWR(OUTSTR,K,TA2(I),0.,0.,'-','Sat vent tmp',IER)
        IF(IACS2(I).GT.IACF2(I))then

C Found a period out of order, warn user and carry on.
          write(outs,'(2a)')' Saturday vent start-end mismatch in...',
     &      outstr(1:50)
          call edisp(iuout,outs)
          write(outs,'(2a)')' of operation file ',currentfile
          call edisp(iuout,outs)
          IER=1
        endif

        IF(IPT2(I).EQ.ICOMP)goto 1009
   20 CONTINUE
      IF(IER.NE.0) goto 1002

C Sundays.
   3  CALL STRIPC(IUO,OUTSTR,0,ND,1,'Sunday flow periods',IER)
      K=0
      CALL EGETWI(OUTSTR,K,NAC3,0,MA,'W','Sat periods',IER)
      IF(NAC3.EQ.0)GOTO 4
      DO 30 I=1,NAC3
        CALL STRIPC(IUO,OUTSTR,0,ND,1,'Period flow details',IER)
        K=0
        CALL EGETWI(OUTSTR,K,IACS3(I),0,24,'W','Sun flow str',IER)
        CALL EGETWI(OUTSTR,K,IACF3(I),0,24,'W','Sun flow end',IER)
        CALL EGETWR(OUTSTR,K,ACI3(I),0.,2000.,'W','Sun infil',IER)
        CALL EGETWR(OUTSTR,K,ACV3(I),0.,2000.,'W','Sun vent',IER)
        CALL EGETWI(OUTSTR,K,IPT3(I),0,NCOMP,'W','Sun vent z',IER)
        CALL EGETWR(OUTSTR,K,TA3(I),0.,0.,'-','Sun vent tmp',IER)
        IF(IACS3(I).GT.IACF3(I))then

C Found a period out of order, warn user and carry on.
          write(outs,'(2a)')' Sunday vent start-end mismatch in...',
     &      outstr(1:50)
          call edisp(iuout,outs)
          write(outs,'(2a)')' of operation file ',currentfile
          call edisp(iuout,outs)
          IER=1
        endif
        IF(IPT3(I).EQ.ICOMP)goto 1009
   30 CONTINUE
      IF(IER.NE.0) goto 1002


C Flow reporting if requested.
      if(ITRC.GE.1)CALL VENTINF(ICOMP,ITRU)

C Casual gains.

C Weekdays.
   4  CALL STRIPC(IUO,OUTSTR,0,ND,1,'Weekday gain periods',IER)
      K=0
      CALL EGETWI(OUTSTR,K,NCAS1,0,MC,'W','Week periods',IER)
      IF(NCAS1.EQ.0)goto 5
      DO 40 I=1,NCAS1
        CALL STRIPC(IUO,OUTSTR,99,ND,1,'Period gain detl',IER)
        K=0
        if(ND.eq.7.or.ND.eq.12)then
          CALL EGETWI(OUTSTR,K,ICGT1(I),-3,5,'W','wkd type',IER)
        else
          ICGT1(I)=1
        endif
        CALL EGETWI(OUTSTR,K,ICGS1(I),0,24,'W','wkd gain st',IER)
        CALL EGETWI(OUTSTR,K,ICGF1(I),0,24,'W','wkd gain fn',IER)
        CALL EGETWR(OUTSTR,K,CMGS1(I),0.,0.,'-','wkd sens',IER)
        CALL EGETWR(OUTSTR,K,CMGL1(I),0.,0.,'-','wkd latent',IER)
        CALL EGETWR(OUTSTR,K,RADC1(I),0.,1.,'W','wkd rad fr',IER)
        CALL EGETWR(OUTSTR,K,CONC1(I),0.,1.,'W','wkd conv fr',IER)
        IF(ICGS1(I).GT.ICGF1(I))then

C Found a period out of order, warn user and carry on.
          write(outs,'(2a)')' Weekday gains start-end mismatch in...',
     &      outstr(1:50)
          call edisp(iuout,outs)
          write(outs,'(2a)')' of operation file ',currentfile
          call edisp(iuout,outs)
          IER=1
        endif
        X=RADC1(I)+CONC1(I)
        IF(X.GT.1.1)then
          write(outs,'(2a)')' Weekday rad & conv fractions > 1.0 in...',
     &      outstr(1:50)
          call edisp(iuout,outs)
          write(outs,'(2a)')' of operation file ',currentfile
          call edisp(iuout,outs)
          ier=1
        endif
        IF(X.LT..95.AND.ITRC.GT.1)call edisp(iuout,
     &                            ' Casual gain rad:con sum < 1.0')
C Check if electrical data has also been saved.
        if(ND.eq.12)then

         CALL EGETWR(OUTSTR,K,PF1(I),0.,1.0,'W','wkd pf',IER)
         CALL EGETWI(OUTSTR,K,IPF1(I),-1,1,'W','wkd lag lead',IER)
         CALL EGETWR(OUTSTR,K,PWR1(I),0.,1000.,'-','wkd power',IER)
         CALL EGETWR(OUTSTR,K,BVOLT1(I),0.,1000.,'-','wkd vlt',IER)
         CALL EGETWI(OUTSTR,K,IPHAS1(I),1,4,'W','wkd phase',IER)
         IELF(ICOMP)=1
        endif
   40 CONTINUE
      IF(IER.NE.0) goto 1002

C Saturdays.
   5  CALL STRIPC(IUO,OUTSTR,0,ND,1,'Saturday gain periods',IER)
      K=0
      CALL EGETWI(OUTSTR,K,NCAS2,0,MC,'W','Sat periods',IER)
      IF(NCAS2.EQ.0)GOTO 6
      DO 50 I=1,NCAS2
        CALL STRIPC(IUO,OUTSTR,99,ND,1,'Period gain details',IER)
        K=0
        if(ND.eq.7.or.ND.eq.12)then
          CALL EGETWI(OUTSTR,K,ICGT2(I),-3,5,'W','Sat type',IER)
        else
          ICGT2(I)=1
        endif
        CALL EGETWI(OUTSTR,K,ICGS2(I),0,24,'W','Sat gain st',IER)
        CALL EGETWI(OUTSTR,K,ICGF2(I),0,24,'W','Sat gain fn',IER)
        CALL EGETWR(OUTSTR,K,CMGS2(I),0.,0.,'-','Sat sens',IER)
        CALL EGETWR(OUTSTR,K,CMGL2(I),0.,0.,'-','Sat latent',IER)
        CALL EGETWR(OUTSTR,K,RADC2(I),0.,1.,'W','Sat rad fr',IER)
        CALL EGETWR(OUTSTR,K,CONC2(I),0.,1.,'W','Sat conv fr',IER)
        IF(ICGS2(I).GT.ICGF2(I))then

C Found a period out of order, warn user and carry on.
          write(outs,'(2a)')' Saturday gains start-end mismatch in...',
     &      outstr(1:50)
          call edisp(iuout,outs)
          write(outs,'(2a)')' of operation file ',currentfile
          call edisp(iuout,outs)
          IER=1
        endif
        X=RADC2(I)+CONC2(I)
        IF(X.GT.1.1)then
          write(outs,'(2a)')' Saturday rad & conv fractions > 1.0 in..',
     &      outstr(1:50)
          call edisp(iuout,outs)
          write(outs,'(2a)')' of operation file ',currentfile
          call edisp(iuout,outs)
          ier=1
        endif
        IF(X.LT..95.AND.ITRC.GT.1)call edisp(iuout,
     &                            ' Casual gain rad:con sum < 1.0')
        if(ND.eq.12)then
         CALL EGETWR(OUTSTR,K,PF2(I),0.,1.0,'W','Sat pf',IER)
         CALL EGETWI(OUTSTR,K,IPF2(I),-1,1,'W','Sat lag lead',IER)
         CALL EGETWR(OUTSTR,K,PWR2(I),0.,1000.,'-','Sat power',IER)
         CALL EGETWR(OUTSTR,K,BVOLT2(I),0.,1000.,'-','Sat vlt',IER)
         CALL EGETWI(OUTSTR,K,IPHAS2(I),1,4,'W','Sat phase',IER)
         IELF(ICOMP)=1
        endif
   50 CONTINUE

C Sundays.
   6  CALL STRIPC(IUO,OUTSTR,0,ND,1,'Sunday gain periods',IER)
      K=0
      CALL EGETWI(OUTSTR,K,NCAS3,0,MC,'W','Sun periods',IER)
      IF(NCAS3.EQ.0)GOTO 8
      DO 60 I=1,NCAS3
        CALL STRIPC(IUO,OUTSTR,99,ND,1,'Period gain details',IER)
        K=0
        if(ND.eq.7.or.ND.eq.12)then
          CALL EGETWI(OUTSTR,K,ICGT3(I),-3,5,'W','Sun type',IER)
        else
          ICGT3(I)=1
        endif
        CALL EGETWI(OUTSTR,K,ICGS3(I),0,24,'W','Sun gain st',IER)
        CALL EGETWI(OUTSTR,K,ICGF3(I),0,24,'W','Sun gain fn',IER)
        CALL EGETWR(OUTSTR,K,CMGS3(I),0.,0.,'-','Sun sens',IER)
        CALL EGETWR(OUTSTR,K,CMGL3(I),0.,0.,'-','Sun latent',IER)
        CALL EGETWR(OUTSTR,K,RADC3(I),0.,1.,'W','Sun rad fr',IER)
        CALL EGETWR(OUTSTR,K,CONC3(I),0.,1.,'W','Sun conv fr',IER)
        IF(ICGS3(I).GT.ICGF3(I))then

C Found a period out of order, warn user and carry on.
          write(outs,'(2a)')' Sunday gains start-end mismatch in...',
     &      outstr(1:50)
          call edisp(iuout,outs)
          write(outs,'(2a)')' of operation file ',currentfile
          call edisp(iuout,outs)
          IER=1
        endif
        X=RADC3(I)+CONC3(I)
        IF(X.GT.1.1)then
          write(outs,'(2a)')' Sunday rad & conv fractions > 1.0 in...',
     &      outstr(1:50)
          call edisp(iuout,outs)
          write(outs,'(2a)')' of operation file ',currentfile
          call edisp(iuout,outs)
          ier=1
        endif
        IF(X.LT..95.AND.ITRC.GT.1)call edisp(iuout,
     &                            ' Casual gain rad:con sum < 1.0')
        if(ND.eq.12)then
         CALL EGETWR(OUTSTR,K,PF3(I),0.,1.0,'W','Sun pf',IER)
         CALL EGETWI(OUTSTR,K,IPF3(I),-1,1,'W','Sun lag lead',IER)
         CALL EGETWR(OUTSTR,K,PWR3(I),0.,1000.,'-','Sun power',IER)
         CALL EGETWR(OUTSTR,K,BVOLT3(I),0.,1000.,'-','Sun vlt',IER)
         CALL EGETWI(OUTSTR,K,IPHAS3(I),1,4,'W','Sun phase',IER)
         IELF(ICOMP)=1
        endif
   60 CONTINUE

C Check to see if gain labels have been added to the end of the file
    8 CALL STRIPC(IUO,OUTSTR,99,ND,1,'Type labels',IERV)
      IF(ND.NE.3.OR.IERV.ne.0) THEN
        goto 1001
      ELSE
        K=0
        DO 1234 ITYP=1,3
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
        lodlabel(icomp,4)='NA    ' 
        lodlabel(icomp,5)='AnnEl ' 
        lodlabel(icomp,6)='NA    ' 
        lodlabel(icomp,7)='NA    ' 
      goto 1235

 1002 write(outs,'(3a)') 'Conversion error in...',OUTSTR(1:50),'...'
      if(dll)then
        dllsubr='EROPER'
        dllmesg=outs
        ier=2
        CALL ERPFREE(IUNIT,ios)
        return
      else
        call edisp(iuout,outs)
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

C --------- EMKOPER
C Write zone operation common block data to file. It is assumed
C that this information has been checked.  OPFIL is the name of
C the file to be written to (confirm if to be overwritten).
C ICOMP is the zone number.
C ITRC unit number for user output, IER=0 OK IER=1 problem.
      SUBROUTINE  EMKOPER(IUO,OPFIL,ICOMP,ITRU,IER)
#include "building.h"

      COMMON/OUTIN/IUOUT,IUIN
      COMMON/precz/zname(MCOM),zdesc(MCOM)
      COMMON/P1/oprdesc,ctlstr
      COMMON/P2/NAC1,IACS1(MA),IACF1(MA),ACI1(MA),ACV1(MA),
     &          IPT1(MA),TA1(MA),NAC2,IACS2(MA),IACF2(MA),ACI2(MA),
     &          ACV2(MA),IPT2(MA),TA2(MA),NAC3,IACS3(MA),IACF3(MA),
     &          ACI3(MA),ACV3(MA),IPT3(MA),TA3(MA)
      COMMON/P2CTL/ITCTL(MCOM),TLO(MCOM),TUP(MCOM),THI(MCOM),ACIL(MCOM),
     &             ACVL(MCOM),IVL(MCOM),TAL(MCOM),ACIU(MCOM),ACVU(MCOM),
     &             IVU(MCOM),TAU(MCOM),ACIH(MCOM),ACVH(MCOM),
     &             IVH(MCOM),TAH(MCOM)
      COMMON/P3/NCAS1,ICGS1(MC),ICGF1(MC),CMGS1(MC),CMGL1(MC),
     &          RADC1(MC),CONC1(MC),NCAS2,ICGS2(MC),ICGF2(MC),CMGS2(MC),
     &          CMGL2(MC),RADC2(MC),CONC2(MC),NCAS3,ICGS3(MC),ICGF3(MC),
     &          CMGS3(MC),CMGL3(MC),RADC3(MC),CONC3(MC)
      COMMON/P3TYPE/ICGT1(MC),ICGT2(MC),ICGT3(MC)

C Version of operations file. ip3ver=0 standard, =1 sorted with header
      common/p3ver/ip3ver

C Electrical data flag and elctrical data.
      common/elecflg/ielf(mcom)
      common/elp3/nel1,pf1(mc),ipf1(mc),pwr1(mc),bvolt1(mc),iphas1(mc),
     &            nel2,pf2(mc),ipf2(mc),pwr2(mc),bvolt2(mc),iphas2(mc),
     &            nel3,pf3(mc),ipf3(mc),pwr3(mc),bvolt3(mc),iphas3(mc)

C Descriptive label for a zone load or casual gain.
      common/loadlabel/lodlabel(mcom,7)
      COMMON/Vld20/Vldtng
      common/curfile/currentfile

      character OPFIL*72,zname*12,oprdesc*248,ctlstr*24,lodlabel*6
      character zdesc*64,currentfile*72
      character dstmp*24
      logical Vldtng

      IER=0

C Open any existing file by this name (ask user for confirmation to
C over-write) or create a new file.
      if(Vldtng)then
        CALL EFOPSEQ(IUO,OPFIL,3,IER)
        currentfile=OPFIL
      else
        CALL EFOPSEQ(IUO,OPFIL,4,IER)
        currentfile=OPFIL
      endif
      IF(IER.LT.0)THEN
        IER=1
        RETURN
      ENDIF

C If version 1 write out header.
      call dstamp(dstmp)
      if(ip3ver.eq.1)then
        WRITE(IUO,'(A)',IOSTAT=IOS,ERR=1) '*Operations 1.0'
        WRITE(IUO,'(3a)',IOSTAT=IOS,ERR=1) '*date ',dstmp,
     &  '  # latest file modification '
      elseif(ip3ver.eq.2)then
        WRITE(IUO,'(A)',IOSTAT=IOS,ERR=1) '*Operations 2.0'
        WRITE(IUO,'(3a)',IOSTAT=IOS,ERR=1) '*date ',dstmp,
     &  '  # latest file modification '
      endif
      WRITE(IUO,30,IOSTAT=IOS,ERR=1)
     &  zname(ICOMP)(1:lnblnk(zname(ICOMP))),OPFIL(:lnblnk(OPFIL))
  30  FORMAT('# operations of ',a,' defined in: ',/,'# ',a)

C Write the common block data to the file.
      WRITE(IUO,'(A)',IOSTAT=IOS,ERR=1)oprdesc(1:lnblnk(oprdesc))
      WRITE(IUO,'(3A)',IOSTAT=IOS,ERR=1)
     &  '# control(',ctlstr,'), low mid & high setpoints '
      WRITE(IUO,'(I4,3F10.3)',IOSTAT=IOS,ERR=1)ITCTL(ICOMP),
     &      TLO(ICOMP),TUP(ICOMP),THI(ICOMP)

      IF(ITCTL(ICOMP).NE.0)THEN
        WRITE(IUO,'(A)',IOSTAT=IOS,ERR=1)
     &    '# lower: infil, vent, source, data'
        WRITE(IUO,'(2F10.3,I5,F10.3)')ACIL(ICOMP),ACVL(ICOMP),
     &        IVL(ICOMP),TAL(ICOMP)
        WRITE(IUO,'(A)',IOSTAT=IOS,ERR=1)
     &    '# middle: infil, vent, source, data'
        WRITE(IUO,'(2F10.3,I5,F10.3)')ACIU(ICOMP),ACVU(ICOMP),
     &     IVU(ICOMP),TAU(ICOMP)
        WRITE(IUO,'(A)',IOSTAT=IOS,ERR=1)
     &    '# high: infil, vent, source, data'
        WRITE(IUO,'(2F10.3,I5,F10.3)')ACIH(ICOMP),ACVH(ICOMP),
     &     IVH(ICOMP),TAH(ICOMP)
      ENDIF

      WRITE(IUO,'(1X,I5,A)',IOSTAT=ISTAT,ERR=1)NAC1,
     &            '   # no Weekday flow periods'
      IF (NAC1 .GT. 0)THEN
        WRITE(IUO,'(A)',IOSTAT=IOS,ERR=1)
     &    '# Wkd: start, stop, infil, ventil, source, data'
        DO 1120 I = 1,NAC1
          WRITE(IUO,5450,IOSTAT=IOS,ERR=1)IACS1(I),IACF1(I),
     &          ACI1(I),ACV1(I),IPT1(I),TA1(I)
1120    CONTINUE
      ENDIF

      WRITE(IUO,'(1X,I5,A)',IOSTAT=IOS,ERR=1)NAC2,
     &            '   # no Saturday flow periods'
      IF (NAC2 .GT. 0)THEN
        WRITE(IUO,'(A)',IOSTAT=IOS,ERR=1)
     &    '# Sat: start, stop, infil, ventil, source, data'
        DO 1140 I = 1,NAC2
          WRITE(IUO,5450,IOSTAT=IOS,ERR=1)IACS2(I),IACF2(I),
     &          ACI2(I),ACV2(I),IPT2(I),TA2(I)
1140    CONTINUE
      ENDIF

      WRITE(IUO,'(1X,I5,A)',IOSTAT=IOS,ERR=1)NAC3,
     &            '   # no Sunday flow periods '
      IF (NAC3 .GT. 0)THEN
        WRITE(IUO,'(A)',IOSTAT=IOS,ERR=1)
     &    '# Sun: start, stop, infil, ventil, source, data'
        DO 1145 I = 1,NAC3
          WRITE(IUO,5450,IOSTAT=IOS,ERR=1)IACS3(I),IACF3(I),
     &          ACI3(I),ACV3(I),IPT3(I),TA3(I)
1145    CONTINUE
      ENDIF

      WRITE(IUO,'(1X,I5,A)',IOSTAT=IOS,ERR=1)NCAS1,
     &            '   # no Weekday casual gains '
      IF (NCAS1 .GT. 0)THEN
        IF(IELF(ICOMP).EQ.1) THEN
          WRITE(IUO,'(a,a)',IOSTAT=IOS,ERR=1)
     &      '# Wk: typ, sta, fin, sen, lat, rad, ',
     &      'con, pf, +/-, pwr, volt, pha'  
        DO 1159 I = 1,NCAS1
          WRITE(IUO,5470,IOSTAT=IOS,ERR=1)ICGT1(I),ICGS1(I),
     &          ICGF1(I),CMGS1(I),CMGL1(I),RADC1(I),CONC1(I),
     &          PF1(I),IPF1(I),PWR1(I),BVOLT1(I),IPHAS1(I) 
1159    CONTINUE
        ELSE
        WRITE(IUO,'(A)',IOSTAT=IOS,ERR=1)
     &    '# Wkd: type, start, stop, sens, latent, rad_frac, conv_frac'
        DO 1160 I = 1,NCAS1
          WRITE(IUO,5460,IOSTAT=IOS,ERR=1)ICGT1(I),ICGS1(I),
     &          ICGF1(I),CMGS1(I),CMGL1(I),RADC1(I),CONC1(I)
1160    CONTINUE
        ENDIF
      ENDIF

      WRITE(IUO,'(1X,I5,A)',IOSTAT=IOS,ERR=1)NCAS2,
     &            '   # no Saturday casual gains '
      IF (NCAS2 .GT. 0)THEN
        IF(IELF(ICOMP).EQ.1) THEN
          WRITE(IUO,'(a,a)',IOSTAT=IOS,ERR=1)
     &      '# Wk: typ, sta, fin, sen, lat, rad, ',
     &      'con, pf, +/-, pwr, volt, pha'  
        DO 1179 I = 1,NCAS2
          WRITE(IUO,5470,IOSTAT=IOS,ERR=1)ICGT2(I),ICGS2(I),
     &          ICGF2(I),CMGS2(I),CMGL2(I),RADC2(I),CONC2(I),
     &          PF2(I),IPF2(I),PWR2(I),BVOLT2(I),IPHAS2(I) 
1179    CONTINUE
        ELSE
        WRITE(IUO,'(A)',IOSTAT=IOS,ERR=1)
     &    '# Sat: type, start, stop, sens, latent, rad_frac, conv_frac'
        DO 1180 I = 1,NCAS2
          WRITE(IUO,5460,IOSTAT=IOS,ERR=1)ICGT2(I),ICGS2(I),
     &          ICGF2(I),CMGS2(I),CMGL2(I),RADC2(I),CONC2(I)
1180    CONTINUE
        ENDIF
      ENDIF

      WRITE(IUO,'(1X,I5,A)',IOSTAT=IOS,ERR=1)NCAS3,
     &            '   # no Sunday casual gains '
      IF (NCAS3 .GT. 0)THEN
        IF(IELF(ICOMP).EQ.1) THEN
          WRITE(IUO,'(a,a)',IOSTAT=IOS,ERR=1)
     &      '# Wk: typ, sta, fin, sen, lat, rad, ',
     &      'con, pf, +/-, pwr, volt, pha'  
        DO 1181 I = 1,NCAS3
          WRITE(IUO,5470,IOSTAT=IOS,ERR=1)ICGT3(I),ICGS3(I),
     &          ICGF3(I),CMGS3(I),CMGL3(I),RADC3(I),CONC3(I),
     &          PF3(I),IPF3(I),PWR3(I),BVOLT3(I),IPHAS3(I) 
1181    CONTINUE
        ELSE
        WRITE(IUO,'(A)',IOSTAT=IOS,ERR=1)
     &    '# Sun: type, start, stop, sens, latent, rad_frac, conv_frac'
        DO 1185 I = 1,NCAS3
          WRITE(IUO,5460,IOSTAT=IOS,ERR=1)ICGT3(I),ICGS3(I),
     &          ICGF3(I),CMGS3(I),CMGL3(I),RADC3(I),CONC3(I)
1185    CONTINUE
        ENDIF
      ENDIF

5450  FORMAT(1X,I3,',',I3,',',2F9.3,I5,F9.3)
5460  FORMAT(1X,3(I4,','),F9.1,',',F9.1,',',F6.3,',',F6.3)
5470  FORMAT(1X,3(I3,','),F7.1,',',F7.1,',',F4.1,',',F4.1,',',F5.2,
     &       ',',I2,',',F7.1,',',F7.1,',',I2)
C Write out the type labels to the file for future reference
      WRITE(IUO,'(a)')'# Labels for gain types '
      WRITE(IUO,'(3(1X,A6))',IOSTAT=IOS,ERR=1) 
     &  (lodlabel(icomp,I),I=1,3)

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
      SUBROUTINE VENTINF(ICOMP,ITRU)
#include "building.h"

      COMMON/P1/oprdesc,ctlstr
      COMMON/PREC2/VOL(MCOM)
      COMMON/P2/NAC1,IACS1(MA),IACF1(MA),ACI1(MA),ACV1(MA),
     &          IPT1(MA),TA1(MA),NAC2,IACS2(MA),IACF2(MA),ACI2(MA),
     &          ACV2(MA),IPT2(MA),TA2(MA),NAC3,IACS3(MA),IACF3(MA),
     &          ACI3(MA),ACV3(MA),IPT3(MA),TA3(MA)
      COMMON/P2CTL/ITCTL(MCOM),TLO(MCOM),TUP(MCOM),THI(MCOM),ACIL(MCOM),
     &             ACVL(MCOM),IVL(MCOM),TAL(MCOM),ACIU(MCOM),ACVU(MCOM),
     &             IVU(MCOM),TAU(MCOM),ACIH(MCOM),ACVH(MCOM),
     &             IVH(MCOM),TAH(MCOM)

      CHARACTER outs*124, oprdesc*248,ctlstr*24

C Don't bother writing this out because it is duplicated in casual gain report.
C      call edisp(itru,' Operation notes:')
C      call edisp248(itru,oprdesc,72)
      IY=ITCTL(ICOMP)
      if(IY.eq.-14)then
        ctlstr='infil only: wind speed  '
      elseif(IY.eq.-13)then
        ctlstr='infil only: ext db temp '
      elseif(IY.eq.-12)then
        ctlstr='infil only: adj zone tmp'
      elseif(IY.eq.-11)then
        ctlstr='infil only: zone temp   '
      elseif(IY.eq.-4)then
        ctlstr='vent only: wind speed   '
      elseif(IY.eq.-3)then
        ctlstr='vent only: ext db temp  '
      elseif(IY.eq.-2)then
        ctlstr='vent only: adj zone temp'
      elseif(IY.eq.-1)then
        ctlstr='vent only: zone temp    '
      elseif(IY.eq.4)then
        ctlstr='infil & vent: wind speed'
      elseif(IY.eq.3)then
        ctlstr='infil & vent: ext db T  '
      elseif(IY.eq.2)then
        ctlstr='infil & vent: adj zone T'
      elseif(IY.eq.1)then
        ctlstr='infil & vent: zone T    '
      elseif(IY.eq.0)then
        ctlstr='no control of air flow  '
      else
        ctlstr='unknown flow control    '
      endif
      write(outs,'(a,a)') ' Control: ',ctlstr
      call edisp(itru,outs) 

      if(IY.eq.0) goto 1225
      WRITE(outs,'(A,3F7.2)')' Lower/Middle/High temp setpoints: ',
     &  TLO(ICOMP),TUP(ICOMP),THI(ICOMP)
      call edisp(itru,outs)
      write(outs,'(20x,a,a)')'Infil. ac/h m^3/s  Vent. ac/h m^3/s ',
     &  ' from  data'
      call edisp(itru,outs)
      acilm = (VOL(ICOMP)*ACIL(ICOMP))/3600.0
      acvlm = (VOL(ICOMP)*ACVL(ICOMP))/3600.0
      WRITE(outs,'(A,F9.3,F8.4,F10.3,F8.4,I5,F10.3)')
     &  ' Lower range data ',ACIL(ICOMP),acilm,
     &  ACVL(ICOMP),acvlm,IVL(ICOMP),TAL(ICOMP)
      call edisp(itru,outs)

      acium = (VOL(ICOMP)*ACIU(ICOMP))/3600.0
      acvum = (VOL(ICOMP)*ACVU(ICOMP))/3600.0
      WRITE(outs,'(A,F9.3,F8.4,F10.3,F8.4,I5,F10.3)')
     &  ' Middle range data',ACIU(ICOMP),acium,
     &  ACVU(ICOMP),acvum,IVU(ICOMP),TAU(ICOMP)
      call edisp(itru,outs)

      acihm = (VOL(ICOMP)*ACIH(ICOMP))/3600.0
      acvhm = (VOL(ICOMP)*ACVH(ICOMP))/3600.0
      WRITE(outs,'(A,F9.3,F8.4,F10.3,F8.4,I5,F10.3)')
     &  ' High range data  ',ACIH(ICOMP),acihm,
     &  ACVH(ICOMP),acvhm,IVH(ICOMP),TAH(ICOMP)
      call edisp(itru,outs)

 1225 CONTINUE

      WRITE(outs,'(A,3I3)')
     & ' Number of Weekday Sat Sun air change periods =',NAC1,NAC2,NAC3
      call edisp(itru,' ')
      call edisp(itru,outs)
      if(NAC1.eq.0.and.NAC2.eq.0.and.NAC3.eq.0)then
        return
      else
        call edisp(itru,
     &'     Period   Infiltration   Ventilation     From Source')
        call edisp(itru,
     &'     id Hours Rate ac/h m3/s Rate ac/h m3/s  Zone Temp.')
        IF(NAC1.GT.0)THEN
          DO 1230 I=1,NAC1
            acim = (VOL(ICOMP)*ACI1(I))/3600.0
            acvm = (VOL(ICOMP)*ACV1(I))/3600.0
            WRITE(outs,'(a,2i3,a,i2,F9.2,F8.4,F8.2,F8.4,I4,F9.2)')'Wkd',
     &        I,IACS1(I),' - ',IACF1(I),ACI1(I),acim,ACV1(I),
     &                       acvm,IPT1(I),TA1(I)
            call edisp(itru,outs)
1230      CONTINUE
        ENDIF

        IF(NAC2.GT.0)THEN
          DO 1250 I=1,NAC2
            acim = (VOL(ICOMP)*ACI2(I))/3600.0
            acvm = (VOL(ICOMP)*ACV2(I))/3600.0
            WRITE(outs,'(a,2i3,a,i2,F9.2,F8.4,F8.2,F8.4,I4,F9.2)')'Sat',
     &        I,IACS2(I),' - ',IACF2(I),ACI2(I),acim,ACV2(I),
     &                    acvm,IPT2(I),TA2(I)
            call edisp(itru,outs)
1250      CONTINUE
        ENDIF

        IF(NAC3.GT.0)THEN
          DO 1255 I=1,NAC3
            acim = (VOL(ICOMP)*ACI3(I))/3600.0
            acvm = (VOL(ICOMP)*ACV3(I))/3600.0
            WRITE(outs,'(a,2i3,a,i2,F9.2,F8.4,F8.2,F8.4,I4,F9.2)')'Sun',
     &        I,IACS3(I),' - ',IACF3(I),ACI3(I),acim,ACV3(I),
     &                    acvm,IPT3(I),TA3(I)
            call edisp(itru,outs)
1255      CONTINUE
        ENDIF
      endif

      RETURN
      END


C ****************** CASINF 
C CASINFO provides an English description of scheduled casual gains
C from zone operation common block data.
      SUBROUTINE CASINF(ICOMP,ITRU)

#include "building.h"

      COMMON/P1/oprdesc,ctlstr
      COMMON/P3/NCAS1,ICGS1(MC),ICGF1(MC),CMGS1(MC),CMGL1(MC),
     &          RADC1(MC),CONC1(MC),NCAS2,ICGS2(MC),ICGF2(MC),CMGS2(MC),
     &          CMGL2(MC),RADC2(MC),CONC2(MC),NCAS3,ICGS3(MC),ICGF3(MC),
     &          CMGS3(MC),CMGL3(MC),RADC3(MC),CONC3(MC)
      COMMON/P3TYPE/ICGT1(MC),ICGT2(MC),ICGT3(MC)

C Electrical data flag and elctrical data.
      common/elecflg/ielf(mcom)
      common/elp3/nel1,pf1(mc),ipf1(mc),pwr1(mc),bvolt1(mc),iphas1(mc),
     &            nel2,pf2(mc),ipf2(mc),pwr2(mc),bvolt2(mc),iphas2(mc),
     &            nel3,pf3(mc),ipf3(mc),pwr3(mc),bvolt3(mc),iphas3(mc)

C Descriptive label for a zone load or casual gain types.
      common/loadlabel/lodlabel(mcom,7)

      CHARACTER outs*124
      CHARACTER oprdesc*248,ctlstr*24, lodlabel*6,llbl*9

      call edisp(itru,' ')
      call edisp(itru,'Notes: ')
      call edisp248(itru,oprdesc,72)
      IF(IELF(ICOMP).EQ.1) THEN
        WRITE(outs,'(A,3I3)')' Number of Weekday Sat Sun zone loads=',
     &    NCAS1,NCAS2,NCAS3
      ELSE
        WRITE(outs,'(A,3I3)')' Number of Weekday Sat Sun casual gains=',
     &    NCAS1,NCAS2,NCAS3
      ENDIF
      call edisp(itru,outs)
      if(NCAS1.eq.0.and.NCAS2.eq.0.and.NCAS3.eq.0)then
        return
      endif
      if(IELF(ICOMP).EQ.1)then
        write(outs,'(a,a)')'Day Gain Type   Period Sensible  ',
     &          'Latent  Radiant  Convec p.f +/-  Power Voltage Phase'
        call edisp(itru,outs)
        write(outs,'(a,a)')'    No.  labl   Hours  Magn.(W)  ',
     &          'Magn.(W) Frac     Frac    (-) (-)    (W)    (V) (rgb)'
        call edisp(itru,outs)
      else  
        write(outs,'(a,a)')'Day Gain Type     Period Sensible  ',
     &          'Latent     Radiant    Convec'
        call edisp(itru,outs)
        write(outs,'(a,a)')'    No.  labl     Hours  Magn.(W)  ',
     &          'Magn. (W)  Frac       Frac'
        call edisp(itru,outs)
      endif
      IF(NCAS1.GT.0)THEN
        IF(IELF(ICOMP).EQ.1) THEN
        DO 1269 I=1,NCAS1
          if(ICGT1(I).gt.0)then
            write(llbl,'(a,a)')lodlabel(ICOMP,ICGT1(I)),'W  '
          elseif(ICGT1(I).eq.-1)then
            write(llbl,'(a,a)')lodlabel(ICOMP,iabs(ICGT1(I))),'m2p'
          elseif(ICGT1(I).lt.-1)then
            write(llbl,'(a,a)')lodlabel(ICOMP,iabs(ICGT1(I))),'Wm2'
          endif
          WRITE(outs,5361)'Wkd',I,llbl,ICGS1(I),' -',
     &          ICGF1(I),CMGS1(I),CMGL1(I),RADC1(I),CONC1(I),PF1(I),
     &          IPF1(I),PWR1(I),BVOLT1(I),IPHAS1(I)
5361      FORMAT(a,I3,1x,A9,I3,a,I3,F8.1,F8.1,F9.1,F9.1,
     &       1x,F7.2,I3,F7.1,F7.1,I3)
          call edisp(itru,outs)
1269    CONTINUE
       ELSE
        DO 1270 I=1,NCAS1
          if(ICGT1(I).gt.0)then
            write(llbl,'(a,a)')lodlabel(ICOMP,ICGT1(I)),'W  '
          elseif(ICGT1(I).eq.-1)then
            write(llbl,'(a,a)')lodlabel(ICOMP,iabs(ICGT1(I))),'m2p'
          elseif(ICGT1(I).lt.-1)then
            write(llbl,'(a,a)')lodlabel(ICOMP,iabs(ICGT1(I))),'Wm2'
          endif
          WRITE(outs,5362)'Wkd',I,llbl,ICGS1(I),' -',
     &          ICGF1(I),CMGS1(I),CMGL1(I),RADC1(I),CONC1(I)
5362      FORMAT(a,I3,1x,A9,I3,a,I3,F9.1,F9.1,F11.2,F11.2)
          call edisp(itru,outs)
1270    CONTINUE
       ENDIF
      ENDIF

      IF(NCAS2.GT.0)THEN
       IF(IELF(ICOMP).EQ.1) THEN
        DO 1289 I=1,NCAS2
          if(ICGT2(I).gt.0)then
            write(llbl,'(a,a)')lodlabel(ICOMP,ICGT2(I)),'W  '
          elseif(ICGT2(I).eq.-1)then
            write(llbl,'(a,a)')lodlabel(ICOMP,iabs(ICGT2(I))),'m2p'
          elseif(ICGT2(I).lt.-1)then
            write(llbl,'(a,a)')lodlabel(ICOMP,iabs(ICGT2(I))),'Wm2'
          endif
          WRITE(outs,5361)'Sat',I,llbl,ICGS2(I),' -',
     &          ICGF2(I),CMGS2(I),CMGL2(I),RADC2(I),CONC2(I),PF2(I),
     &          IPF2(I),PWR2(I),BVOLT2(I),IPHAS2(I)
          call edisp(itru,outs)
1289    CONTINUE
       ELSE
        DO 1290 I=1,NCAS2
          if(ICGT2(I).gt.0)then
            write(llbl,'(a,a)')lodlabel(ICOMP,ICGT2(I)),'W  '
          elseif(ICGT2(I).eq.-1)then
            write(llbl,'(a,a)')lodlabel(ICOMP,iabs(ICGT2(I))),'m2p'
          elseif(ICGT2(I).lt.-1)then
            write(llbl,'(a,a)')lodlabel(ICOMP,iabs(ICGT2(I))),'Wm2'
          endif
          WRITE(outs,5362)'Sat',I,llbl,ICGS2(I),' -',ICGF2(I),
     &              CMGS2(I),CMGL2(I),RADC2(I),CONC2(I)
          call edisp(itru,outs)
1290    CONTINUE
       ENDIF
      ENDIF

      IF(NCAS3.GT.0)THEN
       IF(IELF(ICOMP).EQ.1) THEN
        DO 1291 I=1,NCAS3
          if(ICGT3(I).gt.0)then
            write(llbl,'(a,a)')lodlabel(ICOMP,ICGT3(I)),'W  '
          elseif(ICGT3(I).eq.-1)then
            write(llbl,'(a,a)')lodlabel(ICOMP,iabs(ICGT3(I))),'m2p'
          elseif(ICGT3(I).lt.-1)then
            write(llbl,'(a,a)')lodlabel(ICOMP,iabs(ICGT3(I))),'Wm2'
          endif
          WRITE(outs,5361)'Sun',I,llbl,ICGS3(I),' -',
     &          ICGF3(I),CMGS3(I),CMGL3(I),RADC3(I),CONC3(I),PF3(I),
     &          IPF3(I),PWR3(I),BVOLT3(I),IPHAS3(I)
          call edisp(itru,outs)
1291    CONTINUE
       ELSE
        DO 1295 I=1,NCAS3
          if(ICGT3(I).gt.0)then
            write(llbl,'(a,a)')lodlabel(ICOMP,ICGT3(I)),'W  '
          elseif(ICGT3(I).eq.-1)then
            write(llbl,'(a,a)')lodlabel(ICOMP,iabs(ICGT3(I))),'m2p'
          elseif(ICGT3(I).lt.-1)then
            write(llbl,'(a,a)')lodlabel(ICOMP,iabs(ICGT3(I))),'Wm2'
          endif
          WRITE(outs,5362)'Sun',I,llbl,ICGS3(I),' -',ICGF3(I),
     1         CMGS3(I),CMGL3(I),RADC3(I),CONC3(I)
          call edisp(itru,outs)
1295    CONTINUE
       ENDIF
      ENDIF

      RETURN
      END


C ************* checkcascount
C checkcascount scans current P3 & P3TYPE commons and refreshes 
C loadcount() & load24() in common blocks loadcnt & loadall
C Parameters:
C   icomp - current zone.
C   ier - set to 1 if there was a problem
      subroutine checkcascount(icomp,ier)
#include "building.h"

      COMMON/OUTIN/IUOUT,IUIN

      COMMON/P3/NCAS1,ICGS1(MC),ICGF1(MC),CMGS1(MC),CMGL1(MC),
     &RADC1(MC),CONC1(MC),NCAS2,ICGS2(MC),ICGF2(MC),CMGS2(MC),
     &CMGL2(MC),RADC2(MC),CONC2(MC),NCAS3,ICGS3(MC),ICGF3(MC),
     &CMGS3(MC),CMGL3(MC),RADC3(MC),CONC3(MC)
      COMMON/P3TYPE/ICGT1(MC),ICGT2(MC),ICGT3(MC)

C Version of operations file. ip3ver=0 standard, =1 sorted with header
      common/p3ver/ip3ver

C loadcount(zone,gaintype,daytype) - nb of each gain type (whether
C   absolute or W/m2).
C loadm2count(zone,gaintype,daytype) - nb of each m2 gain types.
C loadmixed(zone,gaintype,daytype) - if zero then nothing, if one
C   then all non-zero are absolute, if two then all (non-zero) are in
C   W/m2 or person/m2, if three then non-zero are mixed.
C Does each gain type on each day:
C   start at zero load24(gaintype,daytype,1)=.true.
C   and end at 24 hours (load24(gaintype,daytype,2)=.true.

      common/loadcnt/loadcount(mcom,7,3),loadm2count(mcom,7,3),
     &               loadmixed(mcom,7,3)
      common/loadall/load24(7,3,2)
      logical load24

C Keep track of how many non-zero positive and negative casual
C gain types - in order to figure out value of loadmixed.
      dimension loadnonzero(7,3),loadm2nonzero(7,3)

C Zero the loadcount array for the current zone.
      ic=icomp
      do 40 ij=1,7
        loadcount(ic,ij,1)=0
        loadcount(ic,ij,2)=0
        loadcount(ic,ij,3)=0
        loadm2count(ic,ij,1)=0
        loadm2count(ic,ij,2)=0
        loadm2count(ic,ij,3)=0
        loadnonzero(ij,1)=0
        loadnonzero(ij,2)=0
        loadnonzero(ij,3)=0
        loadm2nonzero(ij,1)=0
        loadm2nonzero(ij,2)=0
        loadm2nonzero(ij,3)=0
        loadmixed(ic,ij,1)=0
        loadmixed(ic,ij,2)=0
        loadmixed(ic,ij,3)=0
        load24(ij,1,1)=.false.
        load24(ij,2,1)=.false.
        load24(ij,3,1)=.false.
        load24(ij,1,2)=.false.
        load24(ij,2,2)=.false.
        load24(ij,3,2)=.false.
 40   continue

C Debug..
C      write(6,*) 'Nb of weekday/sat/sun periods for occ/lt/equip'
      if(ncas1.gt.0)then
        do 41 ij=1,ncas1
          if(icgt1(ij).eq.1)then
            loadcount(ic,1,1)=loadcount(ic,1,1)+1
            if(CMGS1(ij).gt.0.0) loadnonzero(1,1)=loadnonzero(1,1)+1
            if(ICGS1(ij).eq.0)load24(1,1,1)=.true. 
            if(ICGF1(ij).eq.24)load24(1,1,2)=.true. 
          elseif(icgt1(ij).eq.2)then
            loadcount(ic,2,1)=loadcount(ic,2,1)+1
            if(CMGS1(ij).gt.0.0) loadnonzero(2,1)=loadnonzero(2,1)+1
            if(ICGS1(ij).eq.0)load24(2,1,1)=.true. 
            if(ICGF1(ij).eq.24)load24(2,1,2)=.true. 
          elseif(icgt1(ij).eq.3)then
            loadcount(ic,3,1)=loadcount(ic,3,1)+1
            if(CMGS1(ij).gt.0.0) loadnonzero(3,1)=loadnonzero(3,1)+1
            if(ICGS1(ij).eq.0)load24(3,1,1)=.true. 
            if(ICGF1(ij).eq.24)load24(3,1,2)=.true. 
          elseif(icgt1(ij).eq.4)then
            loadcount(ic,4,1)=loadcount(ic,4,1)+1
            if(CMGS1(ij).gt.0.0) loadnonzero(4,1)=loadnonzero(4,1)+1
            if(ICGS1(ij).eq.0)load24(4,1,1)=.true. 
            if(ICGF1(ij).eq.24)load24(4,1,2)=.true. 
          elseif(icgt1(ij).eq.5)then
            loadcount(ic,5,1)=loadcount(ic,5,1)+1
            if(CMGS1(ij).gt.0.0) loadnonzero(5,1)=loadnonzero(5,1)+1
            if(ICGS1(ij).eq.0)load24(5,1,1)=.true. 
            if(ICGF1(ij).eq.24)load24(5,1,2)=.true. 
          elseif(icgt1(ij).eq.-1)then
            loadcount(ic,1,1)=loadcount(ic,1,1)+1
            loadm2count(ic,1,1)=loadm2count(ic,1,1)+1
            if(CMGS1(ij).gt.0.0) loadm2nonzero(1,1)=loadm2nonzero(1,1)+1
            if(ICGS1(ij).eq.0)load24(1,1,1)=.true. 
            if(ICGF1(ij).eq.24)load24(1,1,2)=.true. 
          elseif(icgt1(ij).eq.-2)then
            loadcount(ic,2,1)=loadcount(ic,2,1)+1
            loadm2count(ic,2,1)=loadm2count(ic,2,1)+1
            if(CMGS1(ij).gt.0.0) loadm2nonzero(2,1)=loadm2nonzero(2,1)+1
            if(ICGS1(ij).eq.0)load24(2,1,1)=.true. 
            if(ICGF1(ij).eq.24)load24(2,1,2)=.true. 
          elseif(icgt1(ij).eq.-3)then
            loadcount(ic,3,1)=loadcount(ic,3,1)+1
            loadm2count(ic,3,1)=loadm2count(ic,3,1)+1
            if(CMGS1(ij).gt.0.0) loadm2nonzero(3,1)=loadm2nonzero(3,1)+1
            if(ICGS1(ij).eq.0)load24(3,1,1)=.true. 
            if(ICGF1(ij).eq.24)load24(3,1,2)=.true. 
          endif
 41     continue

C Determine if any of the casual gain types were mixed.
        if(loadnonzero(1,1).eq.0.and.loadm2nonzero(1,1).eq.0)then
          loadmixed(ic,1,1)=0
        elseif(loadnonzero(1,1).gt.0.and.loadm2nonzero(1,1).eq.0)then
          loadmixed(ic,1,1)=1
        elseif(loadnonzero(1,1).eq.0.and.loadm2nonzero(1,1).gt.0)then
          loadmixed(ic,1,1)=2
        elseif(loadnonzero(1,1).ne.loadm2nonzero(1,1))then
          loadmixed(ic,1,1)=3
        endif
        if(loadnonzero(2,1).eq.0.and.loadm2nonzero(2,1).eq.0)then
          loadmixed(ic,2,1)=0
        elseif(loadnonzero(2,1).gt.0.and.loadm2nonzero(2,1).eq.0)then
          loadmixed(ic,2,1)=1
        elseif(loadnonzero(2,1).eq.0.and.loadm2nonzero(2,1).gt.0)then
          loadmixed(ic,2,1)=2
        elseif(loadnonzero(2,1).ne.loadm2nonzero(2,1))then
          loadmixed(ic,2,1)=3
        endif
        if(loadnonzero(3,1).eq.0.and.loadm2nonzero(3,1).eq.0)then
          loadmixed(ic,3,1)=0
        elseif(loadnonzero(3,1).gt.0.and.loadm2nonzero(3,1).eq.0)then
          loadmixed(ic,3,1)=1
        elseif(loadnonzero(3,1).eq.0.and.loadm2nonzero(3,1).gt.0)then
          loadmixed(ic,3,1)=2
        elseif(loadnonzero(3,1).ne.loadm2nonzero(3,1))then
          loadmixed(ic,3,1)=3
        endif
        if(loadnonzero(4,1).gt.0)loadmixed(ic,4,1)=1
        if(loadnonzero(5,1).gt.0)loadmixed(ic,5,1)=1
      endif

C Debug..
C      write(6,*) ' weekday loadcount loadm2count loadmixed'
C      write(6,*)loadcount(ic,1,1),loadcount(ic,2,1),loadcount(ic,3,1),
C     & loadcount(ic,4,1),loadcount(ic,5,1)
C      write(6,*)loadm2count(ic,1,1),loadm2count(ic,2,1),
C     &  loadm2count(ic,3,1)
C      write(6,*)loadmixed(ic,1,1),loadmixed(ic,2,1),loadmixed(ic,3,1),
C     &  loadmixed(ic,4,1),loadmixed(ic,5,1)

      if(ncas2.gt.0)then
        do 42 ij=1,ncas2
          if(icgt2(ij).eq.1)then
            loadcount(ic,1,2)=loadcount(ic,1,2)+1
            if(CMGS2(ij).gt.0.0) loadnonzero(1,2)=loadnonzero(1,2)+1
            if(ICGS2(ij).eq.0)load24(1,2,1)=.true. 
            if(ICGF2(ij).eq.24)load24(1,2,2)=.true. 
          elseif(icgt2(ij).eq.2)then
            loadcount(ic,2,2)=loadcount(ic,2,2)+1
            if(CMGS2(ij).gt.0.0) loadnonzero(2,2)=loadnonzero(2,2)+1
            if(ICGS2(ij).eq.0)load24(2,2,1)=.true. 
            if(ICGF2(ij).eq.24)load24(2,2,2)=.true. 
          elseif(icgt2(ij).eq.3)then
            loadcount(ic,3,2)=loadcount(ic,3,2)+1
            if(CMGS2(ij).gt.0.0) loadnonzero(3,2)=loadnonzero(3,2)+1
            if(ICGS2(ij).eq.0)load24(3,2,1)=.true. 
            if(ICGF2(ij).eq.24)load24(3,2,2)=.true. 
          elseif(icgt2(ij).eq.4)then
            loadcount(ic,4,2)=loadcount(ic,4,2)+1
            if(CMGS2(ij).gt.0.0) loadnonzero(4,2)=loadnonzero(4,2)+1
            if(ICGS2(ij).eq.0)load24(4,2,1)=.true. 
            if(ICGF2(ij).eq.24)load24(4,2,2)=.true. 
          elseif(icgt2(ij).eq.5)then
            loadcount(ic,5,2)=loadcount(ic,5,2)+1
            if(CMGS2(ij).gt.0.0) loadnonzero(5,2)=loadnonzero(5,2)+1
            if(ICGS2(ij).eq.0)load24(5,2,1)=.true. 
            if(ICGF2(ij).eq.24)load24(5,2,2)=.true. 
          elseif(icgt2(ij).eq.-1)then
            loadcount(ic,1,2)=loadcount(ic,1,2)+1
            loadm2count(ic,1,2)=loadm2count(ic,1,2)+1
            if(CMGS2(ij).gt.0.0) loadm2nonzero(1,2)=loadm2nonzero(1,2)+1
            if(ICGS2(ij).eq.0)load24(1,2,1)=.true. 
            if(ICGF2(ij).eq.24)load24(1,2,2)=.true. 
          elseif(icgt2(ij).eq.-2)then
            loadcount(ic,2,2)=loadcount(ic,2,2)+1
            loadm2count(ic,2,2)=loadm2count(ic,2,2)+1
            if(CMGS2(ij).gt.0.0) loadm2nonzero(2,2)=loadm2nonzero(2,2)+1
            if(ICGS2(ij).eq.0)load24(2,2,1)=.true. 
            if(ICGF2(ij).eq.24)load24(2,2,2)=.true. 
          elseif(icgt2(ij).eq.-3)then
            loadcount(ic,3,2)=loadcount(ic,3,2)+1
            loadm2count(ic,3,2)=loadm2count(ic,3,2)+1
            if(CMGS2(ij).gt.0.0) loadm2nonzero(3,2)=loadm2nonzero(3,2)+1
            if(ICGS2(ij).eq.0)load24(3,2,1)=.true. 
            if(ICGF2(ij).eq.24)load24(3,2,2)=.true. 
          endif
 42     continue

C Determine if any of the casual gain types were mixed.
        if(loadnonzero(1,2).eq.0.and.loadm2nonzero(1,2).eq.0)then
          loadmixed(ic,1,2)=0
        elseif(loadnonzero(1,2).gt.0.and.loadm2nonzero(1,2).eq.0)then
          loadmixed(ic,1,2)=1
        elseif(loadnonzero(1,2).eq.0.and.loadm2nonzero(1,2).gt.0)then
          loadmixed(ic,1,2)=2
        elseif(loadnonzero(1,2).ne.loadm2nonzero(1,2))then
          loadmixed(ic,1,2)=3
        endif
        if(loadnonzero(2,2).eq.0.and.loadm2nonzero(2,2).eq.0)then
          loadmixed(ic,2,2)=0
        elseif(loadnonzero(2,2).gt.0.and.loadm2nonzero(2,2).eq.0)then
          loadmixed(ic,2,2)=1
        elseif(loadnonzero(2,2).eq.0.and.loadm2nonzero(2,2).gt.0)then
          loadmixed(ic,2,2)=2
        elseif(loadnonzero(2,2).ne.loadm2nonzero(2,2))then
          loadmixed(ic,2,2)=3
        endif
        if(loadnonzero(3,2).eq.0.and.loadm2nonzero(3,2).eq.0)then
          loadmixed(ic,3,2)=0
        elseif(loadnonzero(3,2).gt.0.and.loadm2nonzero(3,2).eq.0)then
          loadmixed(ic,3,2)=1
        elseif(loadnonzero(3,2).eq.0.and.loadm2nonzero(3,2).gt.0)then
          loadmixed(ic,3,2)=2
        elseif(loadnonzero(3,2).ne.loadm2nonzero(3,2))then
          loadmixed(ic,3,2)=3
        endif
        if(loadnonzero(4,2).gt.0)loadmixed(ic,4,2)=1
        if(loadnonzero(5,2).gt.0)loadmixed(ic,5,2)=1
      endif

C Debug..
C      write(6,*) ' saturday loadcount loadm2count loadmixed'
C      write(6,*)loadcount(ic,1,2),loadcount(ic,2,2),loadcount(ic,3,2),
C     & loadcount(ic,4,1),loadcount(ic,5,1)
C      write(6,*)loadm2count(ic,1,2),loadm2count(ic,2,2),
C     &  loadm2count(ic,3,2)
C      write(6,*)loadmixed(ic,1,2),loadmixed(ic,2,2),loadmixed(ic,3,2),
C     &  loadmixed(ic,4,1),loadmixed(ic,5,1)
      if(ncas3.gt.0)then
        do 43 ij=1,ncas3
          if(icgt3(ij).eq.1)then
            loadcount(ic,1,3)=loadcount(ic,1,3)+1
            if(CMGS3(ij).gt.0.0) loadnonzero(1,3)=loadnonzero(1,3)+1
            if(ICGS3(ij).eq.0)load24(1,3,1)=.true. 
            if(ICGF3(ij).eq.24)load24(1,3,2)=.true. 
          elseif(icgt3(ij).eq.2)then
            loadcount(ic,2,3)=loadcount(ic,2,3)+1
            if(CMGS3(ij).gt.0.0) loadnonzero(2,3)=loadnonzero(2,3)+1
            if(ICGS3(ij).eq.0)load24(2,3,1)=.true. 
            if(ICGF3(ij).eq.24)load24(2,3,2)=.true. 
          elseif(icgt3(ij).eq.3)then
            loadcount(ic,3,3)=loadcount(ic,3,3)+1
            if(CMGS3(ij).gt.0.0) loadnonzero(3,3)=loadnonzero(3,3)+1
            if(ICGS3(ij).eq.0)load24(3,3,1)=.true. 
            if(ICGF3(ij).eq.24)load24(3,3,2)=.true. 
          elseif(icgt3(ij).eq.4)then
            loadcount(ic,4,3)=loadcount(ic,4,3)+1
            if(CMGS3(ij).gt.0.0) loadnonzero(4,3)=loadnonzero(4,3)+1
            if(ICGS3(ij).eq.0)load24(4,3,1)=.true. 
            if(ICGF3(ij).eq.24)load24(4,3,2)=.true. 
          elseif(icgt3(ij).eq.5)then
            loadcount(ic,5,3)=loadcount(ic,5,3)+1
            if(CMGS3(ij).gt.0.0) loadnonzero(5,3)=loadnonzero(5,3)+1
            if(ICGS3(ij).eq.0)load24(5,3,1)=.true. 
            if(ICGF3(ij).eq.24)load24(5,3,2)=.true. 
          elseif(icgt3(ij).eq.-1)then
            loadcount(ic,1,3)=loadcount(ic,1,3)+1
            loadm2count(ic,1,3)=loadm2count(ic,1,3)+1
            if(CMGS3(ij).gt.0.0) loadm2nonzero(1,3)=loadm2nonzero(1,3)+1
            if(ICGS3(ij).eq.0)load24(1,3,1)=.true. 
            if(ICGF3(ij).eq.24)load24(1,3,2)=.true. 
          elseif(icgt3(ij).eq.-2)then
            loadcount(ic,2,3)=loadcount(ic,2,3)+1
            loadm2count(ic,2,3)=loadm2count(ic,2,3)+1
            if(CMGS3(ij).gt.0.0) loadm2nonzero(2,3)=loadm2nonzero(2,3)+1
            if(ICGS3(ij).eq.0)load24(2,3,1)=.true. 
            if(ICGF3(ij).eq.24)load24(2,3,2)=.true. 
          elseif(icgt3(ij).eq.-3)then
            loadcount(ic,3,3)=loadcount(ic,3,3)+1
            loadm2count(ic,3,3)=loadm2count(ic,3,3)+1
            if(CMGS3(ij).gt.0.0) loadm2nonzero(3,3)=loadm2nonzero(3,3)+1
            if(ICGS3(ij).eq.0)load24(3,3,1)=.true. 
            if(ICGF3(ij).eq.24)load24(3,3,2)=.true. 
          endif

C Determine if any of the casual gain types were mixed.
        if(loadnonzero(1,3).eq.0.and.loadm2nonzero(1,3).eq.0)then
          loadmixed(ic,1,3)=0
        elseif(loadnonzero(1,3).gt.0.and.loadm2nonzero(1,3).eq.0)then
          loadmixed(ic,1,3)=1
        elseif(loadnonzero(1,3).eq.0.and.loadm2nonzero(1,3).gt.0)then
          loadmixed(ic,1,3)=2
        elseif(loadnonzero(1,3).ne.loadm2nonzero(1,3))then
          loadmixed(ic,1,3)=3
        endif
        if(loadnonzero(2,3).eq.0.and.loadm2nonzero(2,3).eq.0)then
          loadmixed(ic,2,3)=0
        elseif(loadnonzero(2,3).gt.0.and.loadm2nonzero(2,3).eq.0)then
          loadmixed(ic,2,3)=1
        elseif(loadnonzero(2,3).eq.0.and.loadm2nonzero(2,3).gt.0)then
          loadmixed(ic,2,3)=2
        elseif(loadnonzero(2,3).ne.loadm2nonzero(2,3))then
          loadmixed(ic,2,3)=3
        endif
        if(loadnonzero(3,3).eq.0.and.loadm2nonzero(3,3).eq.0)then
          loadmixed(ic,3,3)=0
        elseif(loadnonzero(3,3).gt.0.and.loadm2nonzero(3,3).eq.0)then
          loadmixed(ic,3,3)=1
        elseif(loadnonzero(3,3).eq.0.and.loadm2nonzero(3,3).gt.0)then
          loadmixed(ic,3,3)=2
        elseif(loadnonzero(3,3).ne.loadm2nonzero(3,3))then
          loadmixed(ic,3,3)=3
        endif
        if(loadnonzero(4,3).gt.0)loadmixed(ic,4,3)=1
        if(loadnonzero(5,3).gt.0)loadmixed(ic,5,3)=1
 43     continue
      endif

C Debug..
C      write(6,*)loadcount(ic,1,3),loadcount(ic,2,3),loadcount(ic,3,3),
C     & loadcount(ic,4,1),loadcount(ic,5,1)
C      write(6,*)loadmixed(ic,1,3),loadmixed(ic,2,3),loadmixed(ic,3,3),
C     &  loadmixed(ic,4,3),loadmixed(ic,5,3)
C      write(6,*) load24
      return
      end

C ******************* CPYCASIJ ***********************
C CPYCASIJ Copy one casual period jx to another ix for zone icomp and 
C daytype idaytype. 
C    icomp - current zone.
C    idaytype - day type to sort
C    ix is the destination
C    jx is the source

      SUBROUTINE CPYCASIJ(icomp,idaytype,ix,jx,ier)
#include "building.h"

      COMMON/OUTIN/IUOUT,IUIN
      COMMON/P3/NCAS1,ICGS1(MC),ICGF1(MC),CMGS1(MC),CMGL1(MC),
     &RADC1(MC),CONC1(MC),NCAS2,ICGS2(MC),ICGF2(MC),CMGS2(MC),
     &CMGL2(MC),RADC2(MC),CONC2(MC),NCAS3,ICGS3(MC),ICGF3(MC),
     &CMGS3(MC),CMGL3(MC),RADC3(MC),CONC3(MC)
      COMMON/P3TYPE/ICGT1(MC),ICGT2(MC),ICGT3(MC)

C Electrical data flag and elctrical data.
      common/elecflg/ielf(mcom)
      common/elp3/nel1,pf1(mc),ipf1(mc),pwr1(mc),bvolt1(mc),iphas1(mc),
     &            nel2,pf2(mc),ipf2(mc),pwr2(mc),bvolt2(mc),iphas2(mc),
     &            nel3,pf3(mc),ipf3(mc),pwr3(mc),bvolt3(mc),iphas3(mc)

      if(idaytype.eq.1)then
        ICGT1(ix)=ICGT1(jx)
        ICGS1(ix)=ICGS1(jx)
        ICGF1(ix)=ICGF1(jx)
        CMGS1(ix)=CMGS1(jx)
        CMGL1(ix)=CMGL1(jx)
        RADC1(ix)=RADC1(jx)
        CONC1(ix)=CONC1(jx)
        if(ielf(icomp).ne.0)then
          ipf1(ix)=ipf1(jx)
          iphas1(ix)=iphas1(jx)
          pf1(ix)=pf1(jx)
          pwr1(ix)=pwr1(jx)
          bvolt1(ix)=bvolt1(jx)
        endif
      elseif(idaytype.eq.2)then
        ICGT2(ix)=ICGT2(jx)
        ICGS2(ix)=ICGS2(jx)
        ICGF2(ix)=ICGF2(jx)
        CMGS2(ix)=CMGS2(jx)
        CMGL2(ix)=CMGL2(jx)
        RADC2(ix)=RADC2(jx)
        CONC2(ix)=CONC2(jx)
        if(ielf(icomp).ne.0)then
          ipf2(ix)=ipf2(jx)
          iphas2(ix)=iphas2(jx)
          pf2(ix)=pf2(jx)
          pwr2(ix)=pwr2(jx)
          bvolt2(ix)=bvolt2(jx)
        endif
      elseif(idaytype.eq.3)then
        ICGT3(ix)=ICGT3(jx)
        ICGS3(ix)=ICGS3(jx)
        ICGF3(ix)=ICGF3(jx)
        CMGS3(ix)=CMGS3(jx)
        CMGL3(ix)=CMGL3(jx)
        RADC3(ix)=RADC3(jx)
        CONC3(ix)=CONC3(jx)
        if(ielf(icomp).ne.0)then
          ipf3(ix)=ipf3(jx)
          iphas3(ix)=iphas3(jx)
          pf3(ix)=pf3(jx)
          pwr3(ix)=pwr3(jx)
          bvolt3(ix)=bvolt3(jx)
        endif
      endif
      return
      end


C ******************* CPYCASIT ***********************
C CPYCASIT Copy one casual period jx to backup variables for zone icomp 
C and daytype idaytype. 
C    icomp - current zone.
C    idaytype - day type to sort
C    jx is the source
C    icgt,icgs,icgf,cmgs,cmgl,radc,conc,ipf,iphas,pf,pwr,bvolt
C    are the temporary variables.
      SUBROUTINE CPYCASIT(icomp,idaytype,jx,icgt,icgs,icgf,cmgs,cmgl,
     &  radc,conc,ipf,iphas,pf,pwr,bvolt,ier)
#include "building.h"

      COMMON/OUTIN/IUOUT,IUIN
      COMMON/P3/NCAS1,ICGS1(MC),ICGF1(MC),CMGS1(MC),CMGL1(MC),
     &RADC1(MC),CONC1(MC),NCAS2,ICGS2(MC),ICGF2(MC),CMGS2(MC),
     &CMGL2(MC),RADC2(MC),CONC2(MC),NCAS3,ICGS3(MC),ICGF3(MC),
     &CMGS3(MC),CMGL3(MC),RADC3(MC),CONC3(MC)
      COMMON/P3TYPE/ICGT1(MC),ICGT2(MC),ICGT3(MC)

C Electrical data flag and elctrical data.
      common/elecflg/ielf(mcom)
      common/elp3/nel1,pf1(mc),ipf1(mc),pwr1(mc),bvolt1(mc),iphas1(mc),
     &            nel2,pf2(mc),ipf2(mc),pwr2(mc),bvolt2(mc),iphas2(mc),
     &            nel3,pf3(mc),ipf3(mc),pwr3(mc),bvolt3(mc),iphas3(mc)

      if(idaytype.eq.1)then
        ICGT=ICGT1(jx)
        ICGS=ICGS1(jx)
        ICGF=ICGF1(jx)
        CMGS=CMGS1(jx)
        CMGL=CMGL1(jx)
        RADC=RADC1(jx)
        CONC=CONC1(jx)
        if(ielf(icomp).ne.0)then
          ipf=ipf1(jx)
          iphas=iphas1(jx)
          pf=pf1(jx)
          pwr=pwr1(jx)
          bvolt=bvolt1(jx)
        else
          ipf=0
          iphas=1
          pf=0.0
          pwr=0.0
          bvolt=0.0
        endif
      elseif(idaytype.eq.2)then
        ICGT=ICGT2(jx)
        ICGS=ICGS2(jx)
        ICGF=ICGF2(jx)
        CMGS=CMGS2(jx)
        CMGL=CMGL2(jx)
        RADC=RADC2(jx)
        CONC=CONC2(jx)
        if(ielf(icomp).ne.0)then
          ipf=ipf2(jx)
          iphas=iphas2(jx)
          pf=pf2(jx)
          pwr=pwr2(jx)
          bvolt=bvolt2(jx)
        else
          ipf=0
          iphas=1
          pf=0.0
          pwr=0.0
          bvolt=0.0
        endif
      elseif(idaytype.eq.3)then
        ICGT=ICGT3(jx)
        ICGS=ICGS3(jx)
        ICGF=ICGF3(jx)
        CMGS=CMGS3(jx)
        CMGL=CMGL3(jx)
        RADC=RADC3(jx)
        CONC=CONC3(jx)
        if(ielf(icomp).ne.0)then
          ipf=ipf3(jx)
          iphas=iphas3(jx)
          pf=pf3(jx)
          pwr=pwr3(jx)
          bvolt=bvolt3(jx)
        else
          ipf=0
          iphas=1
          pf=0.0
          pwr=0.0
          bvolt=0.0
        endif
      endif
      return
      end

C ******************* CPYCASTI ***********************
C CPYCASTI - Copy from backup variables to casual period jx for zone icomp 
C and daytype idaytype. 
C    icomp - current zone.
C    idaytype - day type to sort
C    jx is the destination
C    icgt,icgs,icgf,cmgs,cmgl,radc,conc,ipf,iphas,pf,pwr,bvolt
C    are the temporary variables.
      SUBROUTINE CPYCASTI(icomp,idaytype,jx,icgt,icgs,icgf,cmgs,cmgl,
     &  radc,conc,ipf,iphas,pf,pwr,bvolt,ier)
#include "building.h"

      COMMON/OUTIN/IUOUT,IUIN
      COMMON/P3/NCAS1,ICGS1(MC),ICGF1(MC),CMGS1(MC),CMGL1(MC),
     &RADC1(MC),CONC1(MC),NCAS2,ICGS2(MC),ICGF2(MC),CMGS2(MC),
     &CMGL2(MC),RADC2(MC),CONC2(MC),NCAS3,ICGS3(MC),ICGF3(MC),
     &CMGS3(MC),CMGL3(MC),RADC3(MC),CONC3(MC)
      COMMON/P3TYPE/ICGT1(MC),ICGT2(MC),ICGT3(MC)

C Electrical data flag and elctrical data.
      common/elecflg/ielf(mcom)
      common/elp3/nel1,pf1(mc),ipf1(mc),pwr1(mc),bvolt1(mc),iphas1(mc),
     &            nel2,pf2(mc),ipf2(mc),pwr2(mc),bvolt2(mc),iphas2(mc),
     &            nel3,pf3(mc),ipf3(mc),pwr3(mc),bvolt3(mc),iphas3(mc)

      if(idaytype.eq.1)then
        ICGT1(jx)=ICGT
        ICGS1(jx)=ICGS
        ICGF1(jx)=ICGF
        CMGS1(jx)=CMGS
        CMGL1(jx)=CMGL
        RADC1(jx)=RADC
        CONC1(jx)=CONC
        if(ielf(icomp).ne.0)then
          ipf1(jx)=ipf
          iphas1(jx)=iphas
          pf1(jx)=pf
          pwr1(jx)=pwr
          bvolt1(jx)=bvolt
        else
          ipf1(jx)=0
          iphas1(jx)=1
          pf1(jx)=0.0
          pwr1(jx)=0.0
          bvolt1(jx)=0.0
        endif
      elseif(idaytype.eq.2)then
        ICGT2(jx)=ICGT
        ICGS2(jx)=ICGS
        ICGF2(jx)=ICGF
        CMGS2(jx)=CMGS
        CMGL2(jx)=CMGL
        RADC2(jx)=RADC
        CONC2(jx)=CONC
        if(ielf(icomp).ne.0)then
          ipf2(jx)=ipf
          iphas2(jx)=iphas
          pf2(jx)=pf
          pwr2(jx)=pwr
          bvolt2(jx)=bvolt
        else
          ipf2(jx)=0
          iphas2(jx)=1
          pf2(jx)=0.0
          pwr2(jx)=0.0
          bvolt2(jx)=0.0
        endif
      elseif(idaytype.eq.3)then
        ICGT3(jx)=ICGT
        ICGS3(jx)=ICGS
        ICGF3(jx)=ICGF
        CMGS3(jx)=CMGS
        CMGL3(jx)=CMGL
        RADC3(jx)=RADC
        CONC3(jx)=CONC
        if(ielf(icomp).ne.0)then
          ipf3(jx)=ipf
          iphas3(jx)=iphas
          pf3(jx)=pf
          pwr3(jx)=pwr
          bvolt3(jx)=bvolt
        else
          ipf3(jx)=0
          iphas3(jx)=1
          pf3(jx)=0.0
          pwr3(jx)=0.0
          bvolt3(jx)=0.0
        endif
      endif
      return
      end

C ********************* PROCESSOLDCAS *********************
C PROCESSOLDCAS: Processes gains in an old format operations file.
C Passed the zone index.
C Returns IER=0 if all ok.
      SUBROUTINE PROCESSOLDCAS(ICOMP,ITRC,ITRU,IER)
#include "building.h"

      COMMON/OUTIN/IUOUT,IUIN
      common/pophelp/h(60)

      COMMON/precz/zname(MCOM),zdesc(MCOM)
      common/PREC17/ZBASEA(MCOM),IBASES(MCOM,6),IUZBASEA(MCOM)

      COMMON/P3/NCAS1,ICGS1(MC),ICGF1(MC),CMGS1(MC),CMGL1(MC),
     &          RADC1(MC),CONC1(MC),NCAS2,ICGS2(MC),ICGF2(MC),CMGS2(MC),
     &          CMGL2(MC),RADC2(MC),CONC2(MC),NCAS3,ICGS3(MC),ICGF3(MC),
     &          CMGS3(MC),CMGL3(MC),RADC3(MC),CONC3(MC)
      COMMON/P3TYPE/ICGT1(MC),ICGT2(MC),ICGT3(MC)

C Commons for electrical data
      COMMON/ELP3/NEL1,PF1(MC),IPF1(MC),PWR1(MC),BVOLT1(MC),IPHAS1(MC),
     &            NEL2,PF2(MC),IPF2(MC),PWR2(MC),BVOLT2(MC),IPHAS2(MC),
     &            NEL3,PF3(MC),IPF3(MC),PWR3(MC),BVOLT3(MC),IPHAS3(MC)
      COMMON/ELECFLG/IELF(MCOM)

C Version of operations file. ip3ver=0 standard, =1 sorted with header
      common/p3ver/ip3ver

C loadcount(zone,gaintype,daytype) - nb of each gain type (whether
C   absolute or W/m2). Up to 7 different casual gain types are
C   supported.
C loadm2count(zone,gaintype,daytype) - nb of each m2 gain types.
C loadmixed(zone,gaintype,daytype) - if zero then nothing, if one
C   then all non-zero are absolute, if two then all (non-zero) are in
C   W/m2 or person/m2, if three then non-zero are mixed.
      common/loadcnt/loadcount(mcom,7,3),loadm2count(mcom,7,3),
     &               loadmixed(mcom,7,3)

      logical close,needtodivide,writeit

      character H*72,zname*12,zdesc*64,outs*124

C Temporary arrays: MC array is 72 so that is
C   sized for 24 hours X 3 day types. Array size should be
C   adjusted if additional day types added.
C CMGSA total sensible W for each hour (all day types). 
C CMGSA total latent W for each hour (all day types). 
C RADCA total radiant casual W for the zone at each hour (all day types)
C CONCA total conv casual W for the zone at each hour (all day types)
C RADFAC implied radiant fraction RADCA/CMGSA
C CONFAC implied convective fraction CONCA/CMGSA
C ICGTYP the casual gain type (remembering negative index).
C NPERSN keeps track of the original number of people/m2.
      dimension CMGSA(MC,7),CMGLA(MC,7),RADCA(MC,7)
      dimension CONCA(MC,7),RADFAC(MC,7),CONFAC(MC,7)
      dimension ICGTYP(MC,7),NPERSN(MC,7)

C Logical for testing whether there is any difference in casual gains.
      logical bCMGSA,bCMGLA,bRADCA,bCONCA,bRADFAC,bCONFAC

C Temporary arrays for period electrical data.
C pfA power factor, ipfA leading/lagging, pwrA real power W,
C bvoltA voltage, iphasA phases assoc with electrical casual gain. 
C aparva is apparent power component VA.
C aparvar is reactive power component VAr.
      dimension pfA(mc,7),ipfA(mc,7),pwrA(mc,7),bvoltA(mc,7)
      dimension iphasA(mc,7),aparva(mc,7),aparvar(mc,7)

C Logical for testing whether there is any difference in electrical gains.
      logical bpfA,bpwrA,bbvoltA

C Floor area is required.
      FLOOR=ZBASEA(ICOMP)
      write(outs,'(a,a,a,F6.1,a)') 'The base area of ',
     &  zname(icomp)(1:lnblnk(zname(icomp))),
     &  ' is',FLOOR,'m^2.'
      call edisp(iuout,outs)

C Remember how may of each.
      LNCAS1=NCAS1
      LNCAS2=NCAS2
      LNCAS3=NCAS3

C Find how many of each casual gain type for each day type as well
C as whether each profile needs to be absolute (casual gain type positive),
C or based on area (casual gain type negative) or if mixed +-.
      call checkcascount(icomp,ier)
      
C Zero derived gains in the temporary array and set the type as per
C the call to checkcascount.
      do 130 IP=1,MC
        do 120 IG=1,7
          CMGSA(IP,IG)=0.0
          CMGLA(IP,IG)=0.0
          RADCA(IP,IG)=0.0
          CONCA(IP,IG)=0.0
          RADFAC(IP,IG)=0.50
          CONFAC(IP,IG)=0.50
          if(ip.ge.1.and.ip.le.24)then
            if(loadmixed(icomp,ig,1).eq.2)then
              ICGTYP(IP,IG)= (-1)*IG
            else
              ICGTYP(IP,IG)= IG
            endif
          elseif(ip.ge.25.and.ip.le.48)then
            if(loadmixed(icomp,ig,2).eq.2)then
              ICGTYP(IP,IG)= (-1)*IG
            else
              ICGTYP(IP,IG)= IG
            endif
          elseif(ip.ge.49.and.ip.le.72)then
            if(loadmixed(icomp,ig,3).eq.2)then
              ICGTYP(IP,IG)= (-1)*IG
            else
              ICGTYP(IP,IG)= IG
            endif
          endif
          NPERSN(IP,IG)=0
          pfA(IP,IG)=0.0
          ipfA(IP,IG)=0
          pwrA(IP,IG)=0.0
          bvoltA(IP,IG)=0.0
          iphasA(IP,IG)=0
          aparva(IP,IG)=0.0
          aparvar(IP,IG)=0.0
 120    continue
 130  continue

C First do weekdays occupants, lights, small power and two reserved types.

C For every hour in a week day check if one of the casual gains
C is applicable. Time is on the half hour to make it easy to
C test for a casual gain period being between T1 and T2.
      time = (-0.5)
      do 200 ip=1,24
        time = time +1.0
        if (NCAS1.gt.0) then
          do 210 I=1,NCAS1
            T1=real(ICGS1(I))
            T2=real(ICGF1(I))
            ITYP=abs(ICGT1(I))
            if(time.gt.T1.and.time.lt.T2)then
              if (ICGT1(I).gt.0) then
                CMGSA(IP,ITYP)=CMGSA(IP,ITYP) + CMGS1(I)
                CMGLA(IP,ITYP)=CMGLA(IP,ITYP) + CMGL1(I)
                RADCA(IP,ITYP)=RADCA(IP,ITYP) + CMGS1(I)*RADC1(I)
                CONCA(IP,ITYP)=CONCA(IP,ITYP) + CMGS1(I)*CONC1(I)
                if(CMGSA(IP,ITYP).gt.0.0)then
                  RADFAC(IP,ITYP)= RADCA(IP,ITYP)/CMGSA(IP,ITYP)
                  CONFAC(IP,ITYP)= CONCA(IP,ITYP)/CMGSA(IP,ITYP)
                endif
                if (IELF(ICOMP).GT.0) then

C Sum real power for this gain type and hour. Use PRECOMP to
C calculate apparent power and reactive power.
                  PR=PWR1(I)
                  pwrA(IP,ITYP)= pwrA(IP,ITYP) + PR
                  call PRECOMP(PR,PF1(I),IPF1(I),PA,PQ)
                  aparva(IP,ITYP) = aparva(IP,ITYP) + PA
                  aparvar(IP,ITYP) = aparvar(IP,ITYP) + PQ
                  denom = sqrt((pwrA(IP,ITYP) * pwrA(IP,ITYP)) +
     &                         (aparvar(IP,ITYP) *aparvar(IP,ITYP)))
                  if(denom.gt.0.0)then
                    pfA(IP,ITYP)= pwrA(IP,ITYP) / denom
                  else
                    pfA(IP,ITYP)= PF1(I)
                  endif
                  ipfA(IP,ITYP)= IPF1(I)
                  bvoltA(IP,ITYP)= BVOLT1(I)
                  iphasA(IP,ITYP)= IPHAS1(I)
                endif
              elseif (ICGT1(I).eq.-1) then
                call eclose(CMGS1(I),0.00,0.001,close)
                if (close) then
                  NPERSN(IP,ITYP)=1
                else
                  NPERSN(IP,ITYP)=INT(FLOOR/CMGS1(I))
                endif
                CMGSA(IP,ITYP)=CMGSA(IP,ITYP) + NPERSN(IP,ITYP)*95.0
                CMGLA(IP,ITYP)=CMGLA(IP,ITYP) + NPERSN(IP,ITYP)*45.0
                RADCA(IP,ITYP)=RADCA(IP,ITYP) + NPERSN(IP,ITYP)*95.0*
     &            RADC1(I)
                CONCA(IP,ITYP)=CONCA(IP,ITYP) + NPERSN(IP,ITYP)*95.0*
     &            CONC1(I)
                if(CMGSA(IP,ITYP).gt.0.0)then
                  RADFAC(IP,ITYP)= RADCA(IP,ITYP)/CMGSA(IP,ITYP)
                  CONFAC(IP,ITYP)= CONCA(IP,ITYP)/CMGSA(IP,ITYP)
                endif
              elseif (ICGT1(I).lt.-1) then
                CMGSA(IP,ITYP)=CMGSA(IP,ITYP) + CMGS1(I)*FLOOR
                CMGLA(IP,ITYP)=CMGLA(IP,ITYP) + CMGL1(I)*FLOOR
                RADCA(IP,ITYP)=RADCA(IP,ITYP) + CMGS1(I)*RADC1(I)*FLOOR
                CONCA(IP,ITYP)=CONCA(IP,ITYP) + CMGS1(I)*CONC1(I)*FLOOR
                if(CMGSA(IP,ITYP).gt.0.0)then
                  RADFAC(IP,ITYP)= RADCA(IP,ITYP)/CMGSA(IP,ITYP)
                  CONFAC(IP,ITYP)= CONCA(IP,ITYP)/CMGSA(IP,ITYP)
                endif
                if (IELF(ICOMP).GT.0) then

C Sum real power for this gain type and hour. Use PRECOMP to
C calculate apparent power and reactive power.
                  PR=PWR1(I)*FLOOR
                  pwrA(IP,ITYP)= pwrA(IP,ITYP) + PR
                  call PRECOMP(PR,PF1(I),IPF1(I),PA,PQ)
                  aparva(IP,ITYP) = aparva(IP,ITYP) + PA
                  aparvar(IP,ITYP) = aparvar(IP,ITYP) + PQ
                  denom = sqrt((pwrA(IP,ITYP) * pwrA(IP,ITYP)) +
     &                         (aparvar(IP,ITYP) *aparvar(IP,ITYP)))
                  if(denom.gt.0.0)then
                    pfA(IP,ITYP)= pwrA(IP,ITYP) / denom
                  else
                    pfA(IP,ITYP)= PF1(I)
                  endif
                  ipfA(IP,ITYP)= IPF1(I)
                  bvoltA(IP,ITYP)= BVOLT1(I)
                  iphasA(IP,ITYP)= IPHAS1(I)
                endif
              endif
            endif
 210      continue
        endif
 200  continue

C Do saturday occupants, lights, small power.
      time = (-0.5)
      do 220 ip=25,48
        time = time +1.0
        if (NCAS2.gt.0) then
          do 230 I=1,NCAS2
            T1=real(ICGS2(I))
            T2=real(ICGF2(I))
            ITYP=abs(ICGT2(I))
            if(time.gt.T1.and.time.lt.T2)then
              if (ICGT2(I).gt.0) then
                CMGSA(IP,ITYP)=CMGSA(IP,ITYP) + CMGS2(I)
                CMGLA(IP,ITYP)=CMGLA(IP,ITYP) + CMGL2(I)
                RADCA(IP,ITYP)=RADCA(IP,ITYP) + CMGS2(I)*RADC2(I)
                CONCA(IP,ITYP)=CONCA(IP,ITYP) + CMGS2(I)*CONC2(I)
                if(CMGSA(IP,ITYP).gt.0.0)then
                  RADFAC(IP,ITYP)= RADCA(IP,ITYP)/CMGSA(IP,ITYP)
                  CONFAC(IP,ITYP)= CONCA(IP,ITYP)/CMGSA(IP,ITYP)
                endif
                if (IELF(ICOMP).GT.0) then

C Sum real power for this gain type and hour. Use PRECOMP to
C calculate apparent power and reactive power.
                  PR=PWR2(I)
                  pwrA(IP,ITYP)= pwrA(IP,ITYP) + PR
                  call PRECOMP(PR,PF2(I),IPF2(I),PA,PQ)
                  aparva(IP,ITYP) = aparva(IP,ITYP) + PA
                  aparvar(IP,ITYP) = aparvar(IP,ITYP) + PQ
                  denom = sqrt((pwrA(IP,ITYP) * pwrA(IP,ITYP)) +
     &                         (aparvar(IP,ITYP) *aparvar(IP,ITYP)))
                  if(denom.gt.0.0)then
                    pfA(IP,ITYP)= pwrA(IP,ITYP) / denom
                  else
                    pfA(IP,ITYP)= PF2(I)
                  endif
                  ipfA(IP,ITYP)= IPF2(I)
                  bvoltA(IP,ITYP)= BVOLT2(I)
                  iphasA(IP,ITYP)= IPHAS2(I)
                endif
              elseif (ICGT2(I).eq.-1) then
                call eclose(CMGS2(I),0.00,0.001,close)
                if (close) then
                  NPERSN(IP,ITYP)=1
                else
                  NPERSN(IP,ITYP)=INT(FLOOR/CMGS2(I))
                endif
                CMGSA(IP,ITYP)=CMGSA(IP,ITYP) + NPERSN(IP,ITYP)*95.0
                CMGLA(IP,ITYP)=CMGLA(IP,ITYP) + NPERSN(IP,ITYP)*45.0
                RADCA(IP,ITYP)=RADCA(IP,ITYP) + NPERSN(IP,ITYP)*95.0*
     &            RADC2(I)
                CONCA(IP,ITYP)=CONCA(IP,ITYP) + NPERSN(IP,ITYP)*95.0*
     &            CONC2(I)
                if(CMGSA(IP,ITYP).gt.0.0)then
                  RADFAC(IP,ITYP)= RADCA(IP,ITYP)/CMGSA(IP,ITYP)
                  CONFAC(IP,ITYP)= CONCA(IP,ITYP)/CMGSA(IP,ITYP)
                endif
              elseif (ICGT2(I).lt.-1) then
                CMGSA(IP,ITYP)=CMGSA(IP,ITYP) + CMGS2(I)*FLOOR
                CMGLA(IP,ITYP)=CMGLA(IP,ITYP) + CMGL2(I)*FLOOR
                RADCA(IP,ITYP)=RADCA(IP,ITYP) + CMGS2(I)*RADC2(I)*FLOOR
                CONCA(IP,ITYP)=CONCA(IP,ITYP) + CMGS2(I)*CONC2(I)*FLOOR
                if(CMGSA(IP,ITYP).gt.0.0)then
                  RADFAC(IP,ITYP)= RADCA(IP,ITYP)/CMGSA(IP,ITYP)
                  CONFAC(IP,ITYP)= CONCA(IP,ITYP)/CMGSA(IP,ITYP)
                endif
                if (IELF(ICOMP).GT.0) then
                  PR=PWR2(I)*FLOOR
                  pwrA(IP,ITYP)= pwrA(IP,ITYP) + PR
                  call PRECOMP(PR,PF2(I),IPF2(I),PA,PQ)
                  aparva(IP,ITYP) = aparva(IP,ITYP) + PA
                  aparvar(IP,ITYP) = aparvar(IP,ITYP) + PQ
                  denom = sqrt((pwrA(IP,ITYP) * pwrA(IP,ITYP)) +
     &                         (aparvar(IP,ITYP) *aparvar(IP,ITYP)))
                  if(denom.gt.0.0)then
                    pfA(IP,ITYP)= pwrA(IP,ITYP) / denom
                  else
                    pfA(IP,ITYP)= PF2(I)
                  endif
                  ipfA(IP,ITYP)= IPF2(I)
                  bvoltA(IP,ITYP)= BVOLT2(I)
                  iphasA(IP,ITYP)= IPHAS2(I)
                endif
              endif
            endif
 230      continue
        endif
 220  continue

C Do sunday occupants, lights, small power.
      time = (-0.5)
      do 240 ip=49,72
        time = time +1.0
        if (NCAS3.gt.0) then
          do 250 I=1,NCAS3
            T1=real(ICGS3(I))
            T2=real(ICGF3(I))
            ITYP=abs(ICGT3(I))
            if(time.gt.T1.and.time.lt.T2)then
              if (ICGT3(I).gt.0) then
                CMGSA(IP,ITYP)=CMGSA(IP,ITYP) + CMGS3(I)
                CMGLA(IP,ITYP)=CMGLA(IP,ITYP) + CMGL3(I)
                RADCA(IP,ITYP)=RADCA(IP,ITYP) + CMGS3(I)*RADC3(I)
                CONCA(IP,ITYP)=CONCA(IP,ITYP) + CMGS3(I)*CONC3(I)
                if(CMGSA(IP,ITYP).gt.0.0)then
                  RADFAC(IP,ITYP)= RADCA(IP,ITYP)/CMGSA(IP,ITYP)
                  CONFAC(IP,ITYP)= CONCA(IP,ITYP)/CMGSA(IP,ITYP)
                endif
                if (IELF(ICOMP).GT.0) then

C Sum real power for this gain type and hour. Use PRECOMP to
C calculate apparent power and reactive power.
                  PR=PWR3(I)
                  pwrA(IP,ITYP)= pwrA(IP,ITYP) + PR
                  call PRECOMP(PR,PF3(I),IPF3(I),PA,PQ)
                  aparva(IP,ITYP) = aparva(IP,ITYP) + PA
                  aparvar(IP,ITYP) = aparvar(IP,ITYP) + PQ
                  denom = sqrt((pwrA(IP,ITYP) * pwrA(IP,ITYP)) +
     &                         (aparvar(IP,ITYP) *aparvar(IP,ITYP)))
                  if(denom.gt.0.0)then
                    pfA(IP,ITYP)= pwrA(IP,ITYP) / denom
                  else
                    pfA(IP,ITYP)= PF3(I)
                  endif
                  ipfA(IP,ITYP)= IPF3(I)
                  bvoltA(IP,ITYP)= BVOLT3(I)
                  iphasA(IP,ITYP)= IPHAS3(I)
                endif
              elseif (ICGT3(I).eq.-1) then
                call eclose(CMGS3(I),0.00,0.001,close)
                if (close) then
                  NPERSN(IP,ITYP)=1
                else
                  NPERSN(IP,ITYP)=INT(FLOOR/CMGS3(I))
                endif
                CMGSA(IP,ITYP)=CMGSA(IP,ITYP) + NPERSN(IP,ITYP)*95.0
                CMGLA(IP,ITYP)=CMGLA(IP,ITYP) + NPERSN(IP,ITYP)*45.0
                RADCA(IP,ITYP)=RADCA(IP,ITYP) + NPERSN(IP,ITYP)*95.0*
     &            RADC3(I)
                CONCA(IP,ITYP)=CONCA(IP,ITYP) + NPERSN(IP,ITYP)*95.0*
     &            CONC3(I)
                if(CMGSA(IP,ITYP).gt.0.0)then
                  RADFAC(IP,ITYP)= RADCA(IP,ITYP)/CMGSA(IP,ITYP)
                  CONFAC(IP,ITYP)= CONCA(IP,ITYP)/CMGSA(IP,ITYP)
                endif
              elseif (ICGT3(I).lt.-1) then
                CMGSA(IP,ITYP)=CMGSA(IP,ITYP) + CMGS3(I)*FLOOR
                CMGLA(IP,ITYP)=CMGLA(IP,ITYP) + CMGL3(I)*FLOOR
                RADCA(IP,ITYP)=RADCA(IP,ITYP) + CMGS3(I)*RADC3(I)*FLOOR
                CONCA(IP,ITYP)=CONCA(IP,ITYP) + CMGS3(I)*CONC3(I)*FLOOR
                if(CMGSA(IP,ITYP).gt.0.0)then
                  RADFAC(IP,ITYP)= RADCA(IP,ITYP)/CMGSA(IP,ITYP)
                  CONFAC(IP,ITYP)= CONCA(IP,ITYP)/CMGSA(IP,ITYP)
                endif
                if (IELF(ICOMP).GT.0) then
                  PR=PWR3(I)*FLOOR
                  pwrA(IP,ITYP)= pwrA(IP,ITYP) + PR
                  call PRECOMP(PR,PF3(I),IPF3(I),PA,PQ)
                  aparva(IP,ITYP) = aparva(IP,ITYP) + PA
                  aparvar(IP,ITYP) = aparvar(IP,ITYP) + PQ
                  denom = sqrt((pwrA(IP,ITYP) * pwrA(IP,ITYP)) +
     &                         (aparvar(IP,ITYP) *aparvar(IP,ITYP)))
                  if(denom.gt.0.0)then
                    pfA(IP,ITYP)= pwrA(IP,ITYP) / denom
                  else
                    pfA(IP,ITYP)= PF3(I)
                  endif
                  ipfA(IP,ITYP)= IPF3(I)
                  bvoltA(IP,ITYP)= BVOLT3(I)
                  iphasA(IP,ITYP)= IPHAS3(I)
                endif
              endif
            endif
 250      continue
        endif
 240  continue

C Do post processing...
C Zero the number of gains and look for start and end periods.
      NCASwk=0
      NCASsat=0
      NCASsun=0
      NELwk=0
      NELsat=0
      NELsun=0

C For each casual gain type that had any periods (via an initial call
C to checkcascount and subsequent checks of n1/n2/n3 etc.) and weekday hour...
      IDT=1
      if(itrc.ge.1)then
        write(itru,*) 'loadcount wkday ',loadcount(icomp,1,IDT),
     &    loadcount(icomp,2,IDT),loadcount(icomp,3,IDT),
     &    loadcount(icomp,4,IDT),loadcount(icomp,5,IDT),
     &    loadm2count(icomp,1,IDT),loadm2count(icomp,2,IDT),
     &    loadm2count(icomp,3,IDT)
        write(itru,*) 'loadmix wkd ',loadmixed(icomp,1,IDT),
     &    loadmixed(icomp,2,IDT),loadmixed(icomp,3,IDT)
      endif
      do 300 ITYP=1,7
        if(loadcount(icomp,ityp,IDT).eq.0)goto 300

C Test if casual gain index is mixed loadmixed(icomp,ityp,IDT)==2) in this schedule.
        needtodivide=.false.
        if(loadmixed(icomp,ityp,IDT).eq.2)needtodivide=.true.
        ihst=0
        ihcg=0 

C Loop thru first 24 hours, testing for differences if ip <= 23. On
C the 24th step, if the finish period is not 24 then also write out
C a final period for the current casual gain type. 
        do 310 ip=1,24
          writeit=.false.
          if(ip.le.23)then
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
          elseif(ip.eq.24.and.ihcg.ne.24)then
            writeit=.true.
          endif
          if(writeit)then
            NCASwk=NCASwk +1
            ihcg=ip

C Adjust for area based, for m2 per person just divide floor area by
C the number of persons to get back to the original CMGSA.
            if(needtodivide)then
              if(ICGTYP(IP,ITYP).eq.-1)then
                CMGSA(IP,ITYP) = FLOOR/NPERSN(IP,ITYP)
                CMGLA(IP,ITYP) = 0.0
              elseif(ICGTYP(IP,ITYP).lt.-1)then
                CMGSA(IP,ITYP) = CMGSA(IP,ITYP)/FLOOR
                CMGLA(IP,ITYP) = CMGLA(IP,ITYP)/FLOOR
                if (IELF(ICOMP).GT.0) then
                  pwrA(IP,ITYP) = pwrA(IP,ITYP)/FLOOR
                endif
              endif
            endif

C Assign values.
            NCAS1 = NCASwk
            ICGS1(NCASwk) = ihst
            ICGF1(NCASwk) = ihcg
            CMGS1(NCASwk) = CMGSA(IP,ITYP)
            CMGL1(NCASwk) = CMGLA(IP,ITYP)
            RADC1(NCASwk) = RADFAC(IP,ITYP)
            CONC1(NCASwk) = CONFAC(IP,ITYP)
            ICGT1(NCASwk) = ICGTYP(IP,ITYP)

C If there is electrical data then also assign these values (note:
C electrical power is assumed to use the same units as the casual gain).
C And if there is no power at the timestep reset PF to zero.
            if (IELF(ICOMP).GT.0) then
              NELwk = NELwk + 1
              call eclose(pwrA(IP,ITYP),0.00,0.001,close)
              if(close)then
                PF1(NELwk) = 0.0
              else
                PF1(NELwk) = pfA(IP,ITYP)
              endif
              IPF1(NELwk) = ipfA(IP,ITYP)
              PWR1(NELwk) = pwrA(IP,ITYP)
              BVOLT1(NELwk) = bvoltA(IP,ITYP)
              IPHAS1(NELwk) = iphasA(IP,ITYP)
              if(itrc.ge.1)then
                WRITE(itru,5470,IOSTAT=IOS)ICGTYP(IP,ITYP),ihst,ihcg,
     &            CMGSA(IP,ITYP),CMGLA(IP,ITYP),RADFAC(IP,ITYP),
     &            CONFAC(IP,ITYP),pfA(IP,ITYP),ipfA(IP,ITYP),
     &            pwrA(IP,ITYP),bvoltA(IP,ITYP),iphasA(IP,ITYP)
              endif
            else
              if(itrc.ge.1)then
                WRITE(itru,5460,IOSTAT=IOS)ICGTYP(IP,ITYP),ihst,ihcg,
     &          CMGSA(IP,ITYP),CMGLA(IP,ITYP),RADFAC(IP,ITYP),
     &          CONFAC(IP,ITYP)
              endif
            endif
5460        FORMAT(1X,3(I4,','),F9.1,',',F9.1,',',F6.3,',',F6.3)
5470  FORMAT(1X,3(I3,','),F7.1,',',F7.1,',',F4.1,',',F4.1,',',F5.2,
     &       ',',I2,',',F7.1,',',F7.1,',',I2)
            ihst=ihcg
          endif
 310    continue
 300  continue

C For each casual gain type that had any periods (via an initial
C call to checkcascount) and saturday hour...
      IDT=2
      if(itrc.ge.1)then
        write(itru,*) 'loadcount sat ',loadcount(icomp,1,IDT),
     &    loadcount(icomp,2,IDT),loadcount(icomp,3,IDT),
     &    loadcount(icomp,4,IDT),loadcount(icomp,5,IDT),
     &    loadm2count(icomp,1,IDT),loadm2count(icomp,2,IDT),
     &    loadm2count(icomp,3,IDT)
        write(itru,*) 'loadmix sat ',loadmixed(icomp,1,IDT),
     &    loadmixed(icomp,2,IDT),loadmixed(icomp,3,IDT)
      endif
      do 320 ITYP=1,7
        if(loadcount(icomp,ityp,IDT).eq.0)goto 320

C Test if casual gain index is mixed loadmixed(icomp,ityp,IDT)==2) in this schedule.
        needtodivide=.false.
        if(loadmixed(icomp,ityp,IDT).eq.2)needtodivide=.true.
        ihst=0
        ihcg=0  

C Loop thru Saturday 24 hours, testing for differences if ip <= 47. On
C the 48th step, if the finish period is not 24 then also write out
C a final period for the current casual gain type. 
        do 330 ip=25,48
          writeit=.false.
          if(ip.le.47)then
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
          elseif(ip.eq.48.and.ihcg.ne.24)then
            writeit=.true.
          endif
          if(writeit)then
            NCASsat=NCASsat+1
            ihcg=ip-24 

C Adjust area based gains. 
            if(needtodivide)then
              if(ICGTYP(IP,ITYP).eq.-1)then
                CMGSA(IP,ITYP) = FLOOR/NPERSN(IP,ITYP)
                CMGLA(IP,ITYP) = 0.0
              elseif(ICGTYP(IP,ITYP).lt.-1)then
                CMGSA(IP,ITYP) = CMGSA(IP,ITYP)/FLOOR
                CMGLA(IP,ITYP) = CMGLA(IP,ITYP)/FLOOR
                if (IELF(ICOMP).GT.0) then
                  pwrA(IP,ITYP) = pwrA(IP,ITYP)/FLOOR
                endif
              endif
            endif

C Assign values.
            NCAS2 = NCASsat
            ICGS2(NCASsat) = ihst
            ICGF2(NCASsat) = ihcg
            CMGS2(NCASsat) = CMGSA(IP,ITYP)
            CMGL2(NCASsat) = CMGLA(IP,ITYP)
            RADC2(NCASsat) = RADFAC(IP,ITYP)
            CONC2(NCASsat) = CONFAC(IP,ITYP)
            ICGT2(NCASsat) = ICGTYP(IP,ITYP)
            if (IELF(ICOMP).GT.0) then
              NELsat = NELsat + 1
              call eclose(pwrA(IP,ITYP),0.00,0.001,close)
              if(close)then
                PF2(NELsat) = 0.0
              else
                PF2(NELsat) = pfA(IP,ITYP)
              endif
              IPF2(NELsat) = ipfA(IP,ITYP)
              PWR2(NELsat) = pwrA(IP,ITYP)
              BVOLT2(NELsat) = bvoltA(IP,ITYP)
              IPHAS2(NELsat) = iphasA(IP,ITYP)
              if(itrc.ge.1)then
                WRITE(itru,5470,IOSTAT=IOS)ICGTYP(IP,ITYP),ihst,ihcg,
     &            CMGSA(IP,ITYP),CMGLA(IP,ITYP),RADFAC(IP,ITYP),
     &            CONFAC(IP,ITYP),pfA(IP,ITYP),ipfA(IP,ITYP),
     &            pwrA(IP,ITYP),bvoltA(IP,ITYP),iphasA(IP,ITYP)
              endif
            else
              if(itrc.ge.1)then
                WRITE(itru,5460,IOSTAT=IOS)ICGTYP(IP,ITYP),ihst,ihcg,
     &            CMGSA(IP,ITYP),CMGLA(IP,ITYP),RADFAC(IP,ITYP),
     &            CONFAC(IP,ITYP)
              endif
            endif
            ihst=ihcg
          endif
 330    continue
 320  continue

C For each casual gain type that had any periods (via an initial
C call to checkcascount) and sunday hour...
      IDT=3
      if(itrc.ge.1)then
        write(itru,*) 'loadcount sunday ',loadcount(icomp,1,IDT),
     &    loadcount(icomp,2,IDT),loadcount(icomp,3,IDT),
     &    loadcount(icomp,4,IDT),loadcount(icomp,5,IDT),
     &    loadm2count(icomp,1,IDT),loadm2count(icomp,2,IDT),
     &    loadm2count(icomp,3,IDT)
        write(itru,*) 'loadmix sunday ',loadmixed(icomp,1,IDT),
     &    loadmixed(icomp,2,IDT),loadmixed(icomp,3,IDT)
      endif
      do 340 ITYP=1,7
        if(loadcount(icomp,ityp,IDT).eq.0)goto 340

C Test if casual gain index is mixed loadmixed(icomp,ityp,IDT)==2) in this schedule.
        needtodivide=.false.
        if(loadmixed(icomp,ityp,IDT).eq.2)needtodivide=.true.
        ihst=0
        ihcg=0  

C Loop thru Sunday 24 hours, testing for differences if ip <= 71. On
C the 72th step, if the finish period is not 24 then also write out
C a final period for the current casual gain type. 
        do 350 ip=49,72
          writeit=.false.
          if(ip.le.71)then
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
          elseif(ip.eq.72.and.ihcg.ne.24)then
            writeit=.true.
          endif
          if(writeit)then
            NCASsun=NCASsun+1
            ihcg=ip-48  

C Adjust area based gains. 
            if(needtodivide)then
              if(ICGTYP(IP,ITYP).eq.-1)then
                CMGSA(IP,ITYP) = FLOOR/NPERSN(IP,ITYP)
                CMGLA(IP,ITYP) = 0.0
              elseif(ICGTYP(IP,ITYP).lt.-1)then
                CMGSA(IP,ITYP) = CMGSA(IP,ITYP)/FLOOR
                CMGLA(IP,ITYP) = CMGLA(IP,ITYP)/FLOOR
                if (IELF(ICOMP).GT.0) then
                  pwrA(IP,ITYP) = pwrA(IP,ITYP)/FLOOR
                endif
              endif
            endif

C Assign values.
            NCAS3 = NCASsun
            ICGS3(NCASsun) = ihst
            ICGF3(NCASsun) = ihcg
            CMGS3(NCASsun) = CMGSA(IP,ITYP)
            CMGL3(NCASsun) = CMGLA(IP,ITYP)
            RADC3(NCASsun) = RADFAC(IP,ITYP)
            CONC3(NCASsun) = CONFAC(IP,ITYP)
            ICGT3(NCASsun) = ICGTYP(IP,ITYP)
            if (IELF(ICOMP).GT.0) then
              NELsun = NELsun + 1
              call eclose(pwrA(IP,ITYP),0.00,0.001,close)
              if(close)then
                PF3(NELsun) = 0.0
              else
                PF3(NELsun) = pfA(IP,ITYP)
              endif
              IPF3(NELsun) = ipfA(IP,ITYP)
              PWR3(NELsun) = pwrA(IP,ITYP)
              BVOLT3(NELsun) = bvoltA(IP,ITYP)
              IPHAS3(NELsun) = iphasA(IP,ITYP)
              if(itrc.ge.1)then
                WRITE(itru,5470,IOSTAT=IOS)ICGTYP(IP,ITYP),ihst,ihcg,
     &            CMGSA(IP,ITYP),CMGLA(IP,ITYP),RADFAC(IP,ITYP),
     &            CONFAC(IP,ITYP),pfA(IP,ITYP),ipfA(IP,ITYP),
     &            pwrA(IP,ITYP),bvoltA(IP,ITYP),iphasA(IP,ITYP)
              endif
            else
              if(itrc.ge.1)then
                WRITE(itru,5460,IOSTAT=IOS)ICGTYP(IP,ITYP),ihst,ihcg,
     &            CMGSA(IP,ITYP),CMGLA(IP,ITYP),RADFAC(IP,ITYP),
     &            CONFAC(IP,ITYP)
              endif
            endif
            ihst=ihcg
          endif
 350    continue
 340  continue
      if(itrc.ge.1)then
        write(itru,*) 'periods detected ',NCASwk,NCASsat,NCASsun
      endif

C See if any periods have been added.
      if(LNCAS1.ne.NCAS1)call edisp(iuout,'Weekdays updated.')
      if(LNCAS2.ne.NCAS2)call edisp(iuout,'Saturdays updated.')
      if(LNCAS3.ne.NCAS3)call edisp(iuout,'Sundays updated.')

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

      subroutine PRECOMP(PWR,PF,IPF,PA,PQ)
      
      real PWR,PF,PA,PQ
      logical close

      call eclose(PF,0.00,0.0001,close)
      if(close)then
        PF = 1.0
        PA = PWR
        PQ = 0.0
      else

C Calculate the phase angle from PF in rads
        PA=ACOS(PF)
      
C Calculate the rectangular co-ordinates
        PQ=PWR*TAN(PA)*real(IPF)
        PA=sqrt(PWR**2+PQ**2)
      endif
      
      RETURN
      END


C ******************* checksort *********************
C checksort does a quick check of casual gains for sorted state.
C not extensive, but should catch most issues.
      subroutine checksort(icomp,idaytype,problem,ier)
#include "building.h"

      COMMON/OUTIN/IUOUT,IUIN
      COMMON/P3/NCAS1,ICGS1(MC),ICGF1(MC),CMGS1(MC),CMGL1(MC),
     &RADC1(MC),CONC1(MC),NCAS2,ICGS2(MC),ICGF2(MC),CMGS2(MC),
     &CMGL2(MC),RADC2(MC),CONC2(MC),NCAS3,ICGS3(MC),ICGF3(MC),
     &CMGS3(MC),CMGL3(MC),RADC3(MC),CONC3(MC)
      COMMON/P3TYPE/ICGT1(MC),ICGT2(MC),ICGT3(MC)
      common/elecflg/ielf(mcom)
      common/elp3/nel1,pf1(mc),ipf1(mc),pwr1(mc),bvolt1(mc),iphas1(mc),
     &            nel2,pf2(mc),ipf2(mc),pwr2(mc),bvolt2(mc),iphas2(mc),
     &            nel3,pf3(mc),ipf3(mc),pwr3(mc),bvolt3(mc),iphas3(mc)

C Version of operations file. ip3ver=0 standard, =1 sorted with header
      common/p3ver/ip3ver

C How many of each gain type is there each zone/gaintype/daytype.
      common/loadcnt/loadcount(mcom,7,3),loadm2count(mcom,7,3),
     &               loadmixed(mcom,7,3)

C Does each gain type on each day:
C   start at zero load24(gaintype,daytype,1)=.true.
C   and end at 24 hours (load24(gaintype,daytype,2)=.true.
      common/loadall/load24(7,3,2)
      logical load24

C logical problem set to true if daytype might not be sorted.
      logical problem
      character outs*124

      if(ip3ver.eq.0)then
        write(outs,*) 'older format operations file in zone ',icomp
        call edisp(iuout,outs)
        problem = .true.
        return
      endif

C Check how many casual gains there are for each type on each day type.
      call checkcascount(icomp,ier)

C Set loop limits for current day type.
      if(idaytype.eq.1)then
        ncas=ncas1

C How many occupant/lights/small power?
        n1=loadcount(icomp,1,1)
        n2=loadcount(icomp,2,1)
        n3=loadcount(icomp,3,1)
      elseif(idaytype.eq.2)then
        ncas=ncas2

C How many occupant/lights/small power?
        n1=loadcount(icomp,1,2)
        n2=loadcount(icomp,2,2)
        n3=loadcount(icomp,3,2)
      elseif(idaytype.eq.3)then
        ncas=ncas3

C How many occupant/lights/small power?
        n1=loadcount(icomp,1,3)
        n2=loadcount(icomp,2,3)
        n3=loadcount(icomp,3,3)
      endif


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
