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

C This file includes:
C  SBEMPR   reads SBEM database
C  MKSBEM   writes a project specific *.ncm file
C  RSBEM    reads a project specific *.ncm file
C  MKSBEM2  writes out a project SBEM v1.1 *.ncm file

C ********************* sbempr
C Subroutine SBEMPR reads SBEM database
      SUBROUTINE SBEMPR(IER)
#include "esprdbfile.h"
C esprdbfile.h supplies LSBEM (SBEM database)
C and default file names for databases
#include "espriou.h"
C espriou.h provides currentfile.
#include "sbem.h"
      
      integer lnblnk  ! function definition

C Note: when reading the SBEM database the code assumes that
C the tags in the file are in a specific order.

C IFIL base file unit number (set in MAIN of each application).
      COMMON/FILEP/IFIL
      CHARACTER OUTSTR*124,LOUTSTR*1000,fs*1
      CHARACTER*42 WORD,FUELNAM
      CHARACTER SYSNAM*60
      CHARACTER*48 ACTNAME
      CHARACTER*72 ACTLNAME ! to hold name of activity in list of assoc bld
      CHARACTER*124 ACTDOC  ! to hold documentation string for activity
      character*144 lworking ! for processing the path of LSBEM
      integer NIT,lndbp,lnbl ! length of string
      logical unixok,near
      
C Initialise counters
      IDEBUG=0
      IFUEL=0
      ISYS=0
      IHSYS=0
      IBRUK=0

C Set folder separator (fs) to \ or / as required.
      call isunix(unixok)
      if(unixok)then
        fs = char(47)
      else
        fs = char(92)
      endif

C Read SBEM database. If lsbem has not yet been assigned
C then assign from default name (which has a
C path to the distribution database folder).
C Process DSBEM with findwhichdbpath. Use the value
C of ipathsbem to control file opening logic. This is similar
C to the logic used for the optics database which is also ASCII.
      if(isbem.eq.0)then
        write(lworking,'(a)') DSBEM(1:lnblnk(DSBEM))
        call findwhichdbpath('sbm',lworking,ier)
      endif

C In most ESP-r modules IFIL+6 is used for optical databases and
C air flow networks etc. so it is likely to be free for temporary
C use in scanning the SBEM database.
      IUF=IFIL+6
      if(ipathsbem.eq.0.or.ipathsbem.eq.1)then
        CALL EFOPSEQ(IUF,LSBEM,1,IER)
        if(IER.ne.0)then
          if(unixok)write(6,*) 'error while opening ',LSBEM
          ier=1
          return
        endif
      elseif(ipathsbem.eq.2)then
        lndbp=lnblnk(standarddbpath)
        write(lworking,'(3a)') standarddbpath(1:lndbp),fs,
     &    lsbem(1:lnblnk(lsbem))

C Debug.
        CALL EFOPSEQ(IUF,lworking,1,IER)
        if(IER.ne.0)then
          if(unixok)write(6,*) 'error while opening ',lworking
          ier=1
          return
        endif
      endif

C Remember the current file name for inclusion in errors.
      if(ipathsbem.eq.0.or.ipathsbem.eq.1)then
        write(currentfile,'(a)') LSBEM(1:lnblnk(LSBEM))
      elseif(ipathsbem.eq.2)then
        write(currentfile,'(a)') lworking(1:lnblnk(lworking))
      endif

C Start reading the tokens in the SBEM file. Many of the lines
C in the file are fixed format (data is expected at a specific
C point in the line).
      CALL STRIPC(IUF,OUTSTR,99,ND,1,'SBEM db',IER)
      IF(OUTSTR(1:5).NE.'*sbem')then
        CONTINUE ! WARNING MSG TO GO HERE
      endif
      CALL STRIPC(IUF,OUTSTR,99,ND,1,'SBEM db',IER)
      K=9
      CALL EGETWR(OUTSTR,K,VERSBEM,1.,2.,'W','Version no.',IER)
      CALL STRIPC(IUF,OUTSTR,99,ND,1,'SBEM db',IER)
      K=12
      CALL EGETWR(OUTSTR,K,SFPDEF,0.,20.,'W','SFP default.',IER)

C Read fuel types names and CO2 emission ratings
      CALL STRIPC(IUF,OUTSTR,99,ND,1,'SBEM db',IER)
      IF(OUTSTR(1:11).NE.'*fuel_start')then
        CONTINUE ! WARNING MEG TO GO HERE
      endif
 1    IFUEL=IFUEL+1
      FUELNAM=" "
      ILEN=1
      CALL STRIPC(IUF,OUTSTR,99,ND,1,'SBEM db',IER)
      IF(OUTSTR(1:9).EQ.'*end_fuel')GOTO 2
      K=0
      CALL EGETWI(OUTSTR,K,NIT,0,0,'-','fuel index no.',IER)
      CALL EGETWI(OUTSTR,K,NIT,0,0,'-','nb items',IER)
      DO 110 IIT=1,NIT
          CALL EGETW(OUTSTR,K,WORD,'W','fuel name',IER)
          WRITE(FUELNAM,'(A,1X,A)')FUELNAM(1:ILEN),WORD(1:lnblnk(WORD))
          ILEN=LNBLNK(WORD)+ILEN+1
 110  CONTINUE
      FUELNAME(IFUEL)=FUELNAM(1:ILEN)
      CALL EGETWR(OUTSTR,K,FUELCO2(IFUEL),0.001,10.,
     &            'W','fuel CO2 rating',IER)

C Debug...
      IF(IDEBUG.EQ.1)THEN
        if(unixok)write(6,*)fuelname(ifuel)!,fuelCO2(ifuel)
        if(unixok)write(6,*)'SBEM db read start'
      ENDIF
      GOTO 1

C Now read system types, default efficiencies & applicable HVAC systems
  2   CALL STRIPC(IUF,OUTSTR,99,ND,1,'SBEM db',IER)
      IF(OUTSTR(1:19).NE.'*system_types_start')then
        CONTINUE ! WARNING MEG TO GO HERE
      endif
  3   ISYS=ISYS+1
      CALL STRIPC(IUF,OUTSTR,99,ND,1,'SBEM db',IER)
      IF(OUTSTR(1:17).EQ.'*end_system_types')GOTO 4
      K=0
      CALL EGETWI(OUTSTR,K,NIT,0,0,'-','system index no.',IER)
      CALL EGETWI(OUTSTR,K,NIT,0,0,'-','nb items',IER)
      CALL EGETW(OUTSTR,K,WORD,'W','system name',IER)
      SYSNAM=WORD
      ILEN=lnblnk(SYSNAM)
      DO 200 IIT=2,NIT
        CALL EGETW(OUTSTR,K,WORD,'W','system name',IER)
        WRITE(SYSNAM,'(A,1X,A)')SYSNAM(1:ILEN),
     &    WORD(1:lnblnk(WORD))
        ILEN=LNBLNK(WORD)+ILEN+1
 200  CONTINUE
      SYSNAME(ISYS)=SYSNAM(1:ILEN)
      CALL EGETWR(OUTSTR,K,SYSEFF(ISYS),-1.,10.,
     &            'W','default efficiency',IER)

C Debug...
      IF(IDEBUG.EQ.2)THEN
        if(unixok)WRITE(6,*)SYSNAM(1:ilen),syseff(isys)
      ENDIF

      CALL EGETWI(OUTSTR,K,NIT,0,0,'-','nb items',IER)
      DO 30 IIT=1,NIT
        CALL EGETWI(OUTSTR,K,IHT,0,0,'-','system index no.',IER)
        ISYSAPP(ISYS,IHT)=1
 30   CONTINUE
      GOTO 3

C Now read HVAC system types 
 4    CALL STRIPC(IUF,OUTSTR,99,ND,1,'start HVAC systems',IER)
      IF(OUTSTR(1:18).NE.'*HVAC_system_start')then
        CONTINUE ! WARNING MEG TO GO HERE
      endif
 5    IHSYS=IHSYS+1
      CALL STRIPC(IUF,OUTSTR,99,ND,1,'HVAC system no.',IER)
      IF(OUTSTR(1:16).EQ.'*end_HVAC_system')GOTO 6
      CALL STRIPC(IUF,OUTSTR,99,ND,1,'HVAC system name',IER)
      HSYSNAME(IHSYS)=OUTSTR(1:70)

C Debug...
      IF(IDEBUG.EQ.2)THEN
        if(unixok)WRITE(6,*)HSYSNAME(IHSYS)
      ENDIF
      CALL STRIPC(IUF,OUTSTR,99,ND,1,'HVACGUIDE-HEAT no.s',IER)
      K=15
      CALL EGETWI(OUTSTR,K,NIT,0,0,'-','HVACGUIDE-HEAT no.',IER)

C Get heating system information
      DO 40 IIT=1,NIT
        CALL STRIPC(IUF,OUTSTR,99,ND,1,'BRUKL HEAT no.s',IER)

C Get fuel type and BRUKL index
        K=0
        CALL EGETWI(OUTSTR,K,IFUEL,0,0,'-','nb items',IER)
        CALL EGETWI(OUTSTR,K,IBRUK,0,0,'-','nb items',IER)

C Get number of system types with this BRUKL index for this HVAC system
        CALL EGETWI(OUTSTR,K,NSYS,0,0,'-','nb items',IER)
        DO 50 ISYS=1,NSYS
          CALL EGETWI(OUTSTR,K,ISS,0,0,'-','nb items',IER)
          IBRUKLH(IFUEL,ISS,IHSYS)=IBRUK
 50     CONTINUE
 40   CONTINUE

C Get cooling systerm information
      CALL STRIPC(IUF,OUTSTR,99,ND,1,'HVACGUIDE-COOL no.s',IER)
      K=15
      CALL EGETWI(OUTSTR,K,NSYS,0,0,'-','nb items',IER)
      CALL STRIPC(IUF,OUTSTR,99,ND,1,'HVACGUIDE-COOL no.s',IER)
      K=0
      DO 60 ISYS=1,NSYS
        CALL EGETWI(OUTSTR,K,IBRUK,0,0,'-','nb items',IER)
        IBRUKLC(ISYS,IHSYS)=IBRUK
 60   CONTINUE

C Read system efficiencies if cooling system is present 
C (IBRUKLC.ne.-1111), assuming MCS number of efficiencies are present
      IF(IBRUKLC(1,IHSYS).NE.-1111)THEN
        CALL STRIPC(IUF,OUTSTR,99,ND,1,'HVACGUIDE-COOL COP',IER)
        K=0
        DO 70 ICS=1,MCS
          CALL EGETWR(OUTSTR,K,SYSEFFC(ICS,IHSYS),-1.,10.,
     &            'W','default efficiency',IER)
C Debug...
          IF(IDEBUG.EQ.2)THEN
            if(unixok)write(6,*)"cool generator eff ",
     &        syseffc(ics,ihsys),ihsys
          ENDIF
 70     CONTINUE
      ENDIF

C Read SFP index and value
      CALL STRIPC(IUF,OUTSTR,99,ND,1,'HVACGUIDE-SFP',IER)
      CALL STRIPC(IUF,OUTSTR,99,ND,1,'HVACGUIDE-SFP',IER)
      K=0
      CALL EGETWI(OUTSTR,K,IBRUKLF(IHSYS),0,0,'-','nb items',IER)

C Read two more lines in the database. These lines are currently
C redundant because default SFP for all systems is the same
      CALL STRIPC(IUF,OUTSTR,99,ND,1,'HVACGUIDE-SFP',IER)
      CALL STRIPC(IUF,OUTSTR,99,ND,1,'HVACGUIDE-SFP',IER)
      CALL STRIPC(IUF,OUTSTR,99,ND,1,'Auxiliary energy',IER)
      K=0      
      CALL EGETW(OUTSTR,K,WORD,'W','Auxiliary flag',IER)
      if(word(1:17).eq.'*Auxiliary_energy')then 
        call stripc(iuf,outstr,99,nd,1,'table data',ier)
        K=0

C Data are in the database in the same order as in HVAC NCM document
C Variables are explained in sbem.h 
        call egetwr(outstr,k,tAEnergy(IHSYS),0.0,0.0,'-',
     &           'reference energy',ier)
        call egetwr(outstr,k,tASFPb(IHSYS),0.0,0.0,'-',
     &           'SFP coefficient',ier)
        call egetwr(outstr,k,tASFPa(IHSYS),0.0,0.0,'-',
     &           'SFP coefficient',ier)    
        call egetwr(outstr,k,tADLb(IHSYS),0.0,0.0,'-',
     &           'leakage coefficient',ier)        
        call egetwr(outstr,k,tADLa(IHSYS),0.0,0.0,'-',
     &           'leakage coefficient',ier)         
        GOTO 5               
      else
        GOTO 5
      endif
      
C Read SBEM building list order index (this is what the activities
C list below points back to), building types index and corresponding
C building type description. building types index 1 is for
C residential, 2 is for places of assembly and offices and shops
C and 3 is for industrial and storage.
 6    CALL STRIPC(IUF,OUTSTR,99,ND,1,'SBEM bldg type',IER)
      IF(OUTSTR(1:16).NE.'*bldg_type_start')then
        CONTINUE ! WARNING MsG TO GO HERE
      endif
      DO 80 IBT=1,MBT
        CALL STRIPC(IUF,OUTSTR,99,ND,1,'SBEM bldg type',IER)
        K=0
        CALL EGETWI(OUTSTR,K,ival,0,0,'-',
     &    'bldg order in list',IER)
        CALL EGETWI(OUTSTR,K,IBTYPNDX(IBT),0,0,'-',
     &    'bldg type index 1-2-3',IER)
        call egetrm(outstr,K,BTYPNAME(IBT),'W',
     &    'building type name',IER)

C Debug...
        IF(IDEBUG.EQ.1)THEN
          if(unixok)write(6,*)ibt,ival,IBTYPNDX(IBT),' ',BTYPNAME(IBT)
        ENDIF
 80   CONTINUE

C Read end of building type information
      CALL STRIPC(IUF,OUTSTR,99,ND,1,'SBEM bldg type',IER)

C Read building services strategy
      CALL STRIPC(IUF,OUTSTR,99,ND,1,'SBEM bldg type',IER)
      IF(OUTSTR(1:23).NE.'*bldg_services_strategy')then
        CONTINUE ! WARNING MsG TO GO HERE
      endif
      IBT=0
 7    CALL STRIPC(IUF,OUTSTR,99,ND,1,'SBEM bldg serv strategy',IER)
      IF(OUTSTR(1:14).EQ.'*end_bldg_serv')GOTO 8
      IBT=IBT+1
      K=0
      CALL EGETWR(OUTSTR,K,BLDIF(IBT),-1.,1.,'W',
     &'improvement factor',IER)
      CALL EGETWR(OUTSTR,K,BLDLZC(IBT),-1.,1.,'W',
     &'low and zero Carbon technology',IER)
      CALL EGETRM(OUTSTR,K,BLDSS(IBT),'W','bldg serv strategy',IER)

C Debug.
      IF(IDEBUG.EQ.1)THEN
        if(unixok)write(6,*)ibt,bldif(ibt),bldlzc(ibt),bldss(ibt)
      ENDIF
      GOTO 7

C Read building regulations type
 8    CALL STRIPC(IUF,OUTSTR,99,ND,1,'bldg regs',IER)
      IF(OUTSTR(1:17).NE.'*bldg_regulations')then
        CONTINUE ! WARNING MsG TO GO HERE
      endif
      IBT=0
 9    CALL STRIPC(IUF,OUTSTR,99,ND,1,'bldg regs',IER)
      IF(OUTSTR(1:13).EQ.'*end_bld_regs')GOTO 10
      IBT=IBT+1
      BLDREG(IBT)=OUTSTR(1:LNBLNK(OUTSTR))

C Debug.
      IF(IDEBUG.EQ.1)THEN
        if(unixok)write(6,*)ibt,bldreg(ibt)
      ENDIF
      GOTO 9

C Read building design stage
 10   CALL STRIPC(IUF,OUTSTR,99,ND,1,'bldg des stage',IER)
      IF(OUTSTR(1:9).NE.'*br_stage')then
        CONTINUE ! WARNING MsG TO GO HERE
      endif
      IBT=0
 11   CALL STRIPC(IUF,OUTSTR,99,ND,1,'bldg des stage',IER)
      IF(OUTSTR(1:13).EQ.'*end_br_stage')GOTO 12
      IBT=IBT+1
      BLDSTG(IBT)=OUTSTR(1:LNBLNK(OUTSTR))
      GOTO 11

C Read Scottish building types
 12   CALL STRIPC(IUF,OUTSTR,99,ND,1,'scot bldg type',IER)
      IF(OUTSTR(1:20).NE.'*scottish_bldg_types')then
        CONTINUE ! WARNING MsG TO GO HERE
      endif
      IBT=0
 13   CALL STRIPC(IUF,OUTSTR,99,ND,1,'scot bldg type',IER)
      IF(OUTSTR(1:24).EQ.'*end_scottish_bldg_types')GOTO 14
      IBT=IBT+1
      SBTYP(IBT)=OUTSTR(1:LNBLNK(OUTSTR))
      GOTO 13

C Read DHW generators, efficiencies and BRUKL indices
 14   CALL STRIPC(IUF,OUTSTR,99,ND,1,'DHW generators',IER)
      IF(OUTSTR(1:15).NE.'*DHW_generators')then
        CONTINUE ! WARNING MsG TO GO HERE 
      endif     
      IBT=1
 15   CALL STRIPC(IUF,OUTSTR,99,ND,1,'DHW generators',IER)
      IF(OUTSTR(1:19).EQ.'*end_DHW_generators')GOTO 16
      DHWGEN(IBT)=OUTSTR(1:25)
      K=25
      CALL EGETWR(OUTSTR,K,DHWEFF(IBT,1),-1.,1.,'W',
     &  'system eff before 1998',IER)
      CALL EGETWR(OUTSTR,K,DHWEFF(IBT,2),-1.,1.,'W',
     &  'system eff after 1998',IER)
      DO 90 IBR=1,MWS
        CALL EGETWI(OUTSTR,K,IBRUKLW(IBT,IBR),0,0,'-',
     &  'BRUKL DHW indices',IER)

C Debug.
      IF(IDEBUG.EQ.1)THEN
        if(unixok)write(6,*)dhwgen(ibt),
     &    dhweff(ibt,1),dhweff(ibt,2),ibruklw(ibt,ibr)
      ENDIF

 90   CONTINUE
      IBT=IBT+1
      GOTO 15

C Read activity types sorted by building type. The data is
C in three parts: the number of corresponding
C buildings that use this activity, a quoted string
C which has the name of the room type, followed by one
C data line for each corespinding building type which
C holds the static data associated with the activity.
 16   CALL STRIPC(IUF,OUTSTR,99,ND,1,'Activity types',IER)
      IF(OUTSTR(1:23).NE.'*activities_by_building')then
        call usrmsg('expecting *activities_by_building and got...',
     &    outstr,'W')  
      endif   
      IAT=1
 17   CALL STRIPC(IUF,OUTSTR,99,ND,1,'Activity types',IER)
      IF(OUTSTR(1:19).EQ.'*end_activities_by_')GOTO 18
      K=0
      CALL EGETWI(OUTSTR,K,NCBTP,0,0,'-','correspnd bld types',IER)
      call egetdq(OUTSTR,K,ACTNAME,'-','room activity type',IER)
      ATYPNAME(IAT)=ACTNAME
      ACTNAME=' '

C Debug.
      IF(IDEBUG.EQ.1)THEN
        if(unixok)WRITE(6,*)IAT,NCBTP,' ',ATYPNAME(IAT)
      ENDIF

C For each of the corresponding building types read in the static
C data as follows:
C a) building order indices from the *bldg_type_start section of the file
C b) index of the activity
C c) watts per m2 per 100 lux for notional building (3.75 for 
C    office/storage/industrial spaces and 5.2 for all other spaces)
C d) watts per m2 per 100 lux for typical building (4.5 for 
C    office/storage/industrial spaces and 6.2 for all other spaces)
C e) quoted string name of activity (roomactname)
C f) b_type order index (same as A) (bld_order_index)
C g) occupant density people/m2 (occupant_dens)
C h) metabolic rate W/person/m2 (metabolic_rate)
C i) Main cooling setpoint (Cmainsetpoint)
C j) Main heating set point (Hmainsetpoint) to use for notional
C       building overheating when naturally ventilated. This will be
C       the maximum hourly heating value in that activities timestep data.
C k) skip set back
C l) outside air per person litres/sec/person (fresh_air)
C m) lighting lux (lighting_lux)
C n) equipment W/m2  (equip_gain)
C o) domestic hot water litres/year/m2 (dhw_litres)
C p) skip humidity minimum %
C q) skip humidity maximum %
C r) quoted string description (roomactdoc)
C s) occupant latent percentage (of the whole gain) (latent_ocup_percent)
C t) equipment latent percentage (of the whole gain) (latent_equip_percent)
C u) lighting display W/m2  (display_lighting)

C << additional data items to be documented below >>
C 13 items fMonthEstSysHrs for each month assumed hours of env sys use
C    with 13th value as annual assumed hours of env systems use      
C v) hours of year with non-zero occupancy
C w) summation of hourly occupancy faction
C x) days which have (at least some) occupancy
C y) hours of the year with non-zero lighting
C z) summation of the hourly lighting fraction
C 1) days which have (at least some) lighting
C 2) hours of year with non-zero equipment
C 3) summation of hourly equipment fraction
c 4) days which have (at least some) equipment
C 5) local manual switching flag, (if 1 then local manual switching does not apply to this activity)
      DO 130 ICBTP=1,NCBTP
        CALL STRIPC1K(IUF,LOUTSTR,0,ND,1,'Activity types',IER)
        lnbl=lnblnk(LOUTSTR)  ! remember how long the line is
        K=0
        CALL EGETWI(LOUTSTR,K,IBTN,0,0,'-','crrspnd bld typ index',IER)
        CALL EGETWI(LOUTSTR,K,iactindex,0,0,'-','activity index',IER)

C Database version 1.1 includes values for lighting for the notional building
        call eclose(VERSBEM,1.0,0.001,near)
        if(.not.near)then
          CALL EGETWR(LOUTSTR,K,VAL,0.,10.,'W','notional Lght',IER)
          fNotionalLighting(iactindex)=VAL  
          CALL EGETWR(LOUTSTR,K,VAL,0.,10.,'W','typical Lght',IER)
          fTypicalLighting(iactindex)=VAL  
        endif     
        call egetdq(LOUTSTR,K,ACTLNAME,'-','room activity type',IER)
        write(roomactname(iactindex),'(a)') ACTLNAME(1:lnblnk(ACTLNAME))
        CALL EGETWI(LOUTSTR,K,index,0,0,'-','crrspnd bld typ index',IER)
        bld_order_index(iactindex)=index
        CALL EGETWR(LOUTSTR,K,VAL,0.,10.,'W','occupant density',IER)
        occupant_dens(iactindex)=VAL
        CALL EGETWI(LOUTSTR,K,IVAL,0,1000,'W','metabolic rate',IER)
        metabolic_rate(iactindex)=IVAL
        CALL EGETWR(LOUTSTR,K,VAL,0.,100.,'W','cooling setpoint',IER)
        Cmainsetpoint(iactindex)=VAL       
        CALL EGETWR(LOUTSTR,K,VAL,-50.,100.,'W','heating setpoint',IER)
        Hmainsetpoint(iactindex)=VAL
        CALL EGETWR(LOUTSTR,K,VAL,-50.,100.,'W','set-back',IER)
        CALL EGETWR(LOUTSTR,K,VAL,0.,200.,'W','outside air per prs',IER)
        fresh_air(iactindex)=VAL
        CALL EGETWI(LOUTSTR,K,IVAL,0,10000,'W','lighting lux',IER)
        lighting_lux(iactindex)=IVAL
        CALL EGETWR(LOUTSTR,K,VAL,0.,1000.,'W','equipment W/m2',IER)
        equip_gain(iactindex)=VAL
        CALL EGETWR(LOUTSTR,K,VAL,0.,50000.,'W',
     &    'domestic hot water litres/year/m2',IER)
        dhw_litres(iactindex)=VAL
        CALL EGETWI(LOUTSTR,K,IVAL,0,100,'W','humidity minimum %',IER)
        CALL EGETWI(LOUTSTR,K,IVAL,0,100,'W','humidity max %',IER)
        call egetdq(LOUTSTR,K,ACTDOC,'-','activity description',IER)
        write(roomactdoc(iactindex),'(a)') ACTDOC(1:lnblnk(ACTDOC))
        CALL EGETWI(LOUTSTR,K,IVAL,0,100,'W',
     &    'occupant latent percentage',IER)
        latent_ocup_percent(iactindex)=IVAL
        CALL EGETWI(LOUTSTR,K,IVAL,0,100,'W',
     &    'equipment latent percentage',IER)
        latent_equip_percent(iactindex)=IVAL
        CALL EGETWR(LOUTSTR,K,VAL,0.,100.,'W','lighting displ W/m2',IER)
        display_lighting(iactindex)=VAL

C Gather the fMonthEstSysHrs(MACL,13) and nonzerohours(MACL,3),
C and atleastonehour(MACL,3) and casualfracsum(MACL,3) and 
C local manual switching flag loc_man_sw(MACL) if we have
C not yet reached the end of the data line.
        if(K.ge.lnbl.or.K+1.ge.lnbl)then
          fMonthEstSysHrs(iactindex,1)=0.0  ! jan estimated system run hours
          fMonthEstSysHrs(iactindex,2)=0.0  ! feb estaimated system run hours
          fMonthEstSysHrs(iactindex,3)=0.0
          fMonthEstSysHrs(iactindex,4)=0.0
          fMonthEstSysHrs(iactindex,5)=0.0
          fMonthEstSysHrs(iactindex,6)=0.0
          fMonthEstSysHrs(iactindex,7)=0.0
          fMonthEstSysHrs(iactindex,8)=0.0
          fMonthEstSysHrs(iactindex,9)=0.0
          fMonthEstSysHrs(iactindex,10)=0.0
          fMonthEstSysHrs(iactindex,11)=0.0
          fMonthEstSysHrs(iactindex,12)=0.0
          fMonthEstSysHrs(iactindex,13)=0.0  ! annual estimated system run hours
          nonzerohours(iactindex,1)=0.0      ! hours of year with non-zero occupancy
          casualfracsum(iactindex,1)=0.0     ! summation of the hourly occupant
          atleastonehour(iactindex,1)=0.0    ! days which have (at least some) occupancy
          nonzerohours(iactindex,2)=0.0
          casualfracsum(iactindex,2)=0.0
          atleastonehour(iactindex,2)=0.0
          nonzerohours(iactindex,3)=0.0
          casualfracsum(iactindex,3)=0.0
          atleastonehour(iactindex,3)=0.0
          loc_man_sw(iactindex)=0
        else
          CALL EGETWR(LOUTSTR,K,VAL,0.,750.,'W','est sys run hours',IER)
          fMonthEstSysHrs(iactindex,1)=VAL  ! jan estimated system run hours
          CALL EGETWR(LOUTSTR,K,VAL,0.,750.,'W','est sys run hours',IER)
          fMonthEstSysHrs(iactindex,2)=VAL  ! feb estimated system run hours
          CALL EGETWR(LOUTSTR,K,VAL,0.,750.,'W','est sys run hours',IER)
          fMonthEstSysHrs(iactindex,3)=VAL  ! mar estimated system run hours
          CALL EGETWR(LOUTSTR,K,VAL,0.,750.,'W','est sys run hours',IER)
          fMonthEstSysHrs(iactindex,4)=VAL  ! apr estimated system run hours
          CALL EGETWR(LOUTSTR,K,VAL,0.,750.,'W','est sys run hours',IER)
          fMonthEstSysHrs(iactindex,5)=VAL  ! may estimated system run hours
          CALL EGETWR(LOUTSTR,K,VAL,0.,750.,'W','est sys run hours',IER)
          fMonthEstSysHrs(iactindex,6)=VAL  ! jun estimated system run hours
          CALL EGETWR(LOUTSTR,K,VAL,0.,750.,'W','est sys run hours',IER)
          fMonthEstSysHrs(iactindex,7)=VAL  ! jul estimated system run hours
          CALL EGETWR(LOUTSTR,K,VAL,0.,750.,'W','est sys run hours',IER)
          fMonthEstSysHrs(iactindex,8)=VAL  ! aug estimated system run hours
          CALL EGETWR(LOUTSTR,K,VAL,0.,750.,'W','est sys run hours',IER)
          fMonthEstSysHrs(iactindex,9)=VAL  ! sep estimated system run hours
          CALL EGETWR(LOUTSTR,K,VAL,0.,750.,'W','est sys run hours',IER)
          fMonthEstSysHrs(iactindex,10)=VAL  ! oct estimated system run hours
          CALL EGETWR(LOUTSTR,K,VAL,0.,750.,'W','est sys run hours',IER)
          fMonthEstSysHrs(iactindex,11)=VAL  ! nov estimated system run hours
          CALL EGETWR(LOUTSTR,K,VAL,0.,750.,'W','est sys run hours',IER)
          fMonthEstSysHrs(iactindex,12)=VAL  ! dec estimated system run hours
          CALL EGETWR(LOUTSTR,K,VAL,0.,8760.,'W',
     &      'annual est sys run hours',IER)
          fMonthEstSysHrs(iactindex,13)=VAL  ! annu estimated system run hours

          if(K.ge.lnbl.or.K+1.ge.lnbl)then

C If there are monthly estimates but no summary for activity casual
C gains fill these with zeros.
            nonzerohours(iactindex,1)=0.0      ! hours of year with non-zero occupancy
            casualfracsum(iactindex,1)=0.0     ! summation of the hourly occupant
            atleastonehour(iactindex,1)=0.0    ! days which have (at least some) occupancy
            nonzerohours(iactindex,2)=0.0
            casualfracsum(iactindex,2)=0.0
            atleastonehour(iactindex,2)=0.0
            nonzerohours(iactindex,3)=0.0
            casualfracsum(iactindex,3)=0.0
            atleastonehour(iactindex,3)=0.0
            loc_man_sw(iactindex)=0
          else
            CALL EGETWR(LOUTSTR,K,VAL,0.,8760.,'W',
     &        'non-zero hours with occupant',IER)
            nonzerohours(iactindex,1)=VAL
            CALL EGETWR(LOUTSTR,K,VAL,0.,8760.,'W',
     &        'summation of occupant fractions',IER)
            casualfracsum(iactindex,1)=VAL
            CALL EGETWR(LOUTSTR,K,VAL,0.,370.,'W',
     &        'days with at least some occupants',IER)
            atleastonehour(iactindex,1)=VAL

            CALL EGETWR(LOUTSTR,K,VAL,0.,8760.,'W',
     &        'non-zero hours with lights',IER)
            nonzerohours(iactindex,2)=VAL
            CALL EGETWR(LOUTSTR,K,VAL,0.,8760.,'W',
     &        'summation of lighting fracctions',IER)
            casualfracsum(iactindex,2)=VAL
            CALL EGETWR(LOUTSTR,K,VAL,0.,370.,'W',
     &        'days with at least some lights',IER)
            atleastonehour(iactindex,2)=VAL
            CALL EGETWR(LOUTSTR,K,VAL,0.,8760.,'W',
     &        'non-zero hours with equipment',IER)
            nonzerohours(iactindex,3)=VAL
            CALL EGETWR(LOUTSTR,K,VAL,0.,8760.,'W',
     &        'summation of equipment fractions',IER)
            casualfracsum(iactindex,3)=VAL
            CALL EGETWR(LOUTSTR,K,VAL,0.,370.,'W',
     &        'days with at least some equipment',IER)
            atleastonehour(iactindex,3)=VAL
            CALL EGETWI(LOUTSTR,K,IVAL,0,1,'W',
     &        'local manual switching flag',IER)
              loc_man_sw(iactindex)=IVAL
          endif
        endif
        IATYPNDX(IAT,IBTN)=1
        IF(IDEBUG.EQ.1.and.unixok)THEN
          IF(IATYPNDX(IAT,IBTN).EQ.1)WRITE(6,*)' btypes ',BTYPNAME(IBTN)
        ENDIF
 130  CONTINUE
      IAT=IAT+1
      GOTO 17

C Read lighting systems
 18   CALL STRIPC(IUF,OUTSTR,99,ND,1,'Lighting types',IER)
      IF(OUTSTR(1:14).NE.'*Lighting_type')then
        CONTINUE ! WARNING MsG TO GO HERE   
      endif   
      IAT=1
 19   CALL STRIPC(IUF,OUTSTR,99,ND,1,'Lighting types',IER)
      IF(OUTSTR(1:18).EQ.'*end_Lighting_type')GOTO 20
      K=0
      CALL EGETWI(OUTSTR,K,NINDX,0,0,'-','index no.',IER)
      CALL EGETWR(OUTSTR,K,PDRL(IAT,1),0.,35.,'W',
     &'light pwr den com',IER)
      CALL EGETWR(OUTSTR,K,PDRL(IAT,2),0.,35.,'W',
     &'light pwr den ind',IER)
      NLEN=LNBLNK(OUTSTR)      
      TLIGHT(IAT)=OUTSTR(K+1:NLEN)
      IAT=IAT+1
      GOTO 19

C Read insolation values and average annual wind speed
 20   CALL STRIPC(IUF,OUTSTR,99,ND,1,'Insolation',IER)
      IF(OUTSTR(1:17).NE.'*solar_insolation')then
        CONTINUE ! WARNING MsG TO GO HERE   
      endif   
      IAT=1
 21   CALL STRIPC1K(IUF,LOUTSTR,99,ND,1,'Insolation',IER)
      IF(LOUTSTR(1:21).EQ.'*end_solar_insolation')GOTO 22
      K=0
      CALL EGETW(LOUTSTR,K,WORD,'W','climate name',IER)
      SCLMTNM(IAT)=WORD
      CALL EGETWR(LOUTSTR,K,AAWS(IAT),0.,50.,'W',
     &'annual wind speed m/s',IER)
      CALL EGETWR(LOUTSTR,K,SAWS(IAT),0.,5.e7,'W',
     &'annual wind speed m/s**3',IER)
      DO 131 IBT=1,56 ! 56 items expected on line after wind speed
        CALL EGETWR(LOUTSTR,K,SLRINSO(IAT,IBT),0.,1400.,'W',
     &  'Solar insolation kwh/m2',IER)
 131  CONTINUE
      IAT=IAT+1
      GOTO 21
      
C Read air change per hour values
  22  CALL STRIPC(IUF,OUTSTR,99,ND,1,'CIBSE ach',IER)
      IF(OUTSTR(1:23).NE.'*start_CIBSE_tables_ach')then
        CONTINUE ! WARNING MsG TO GO HERE   
      endif
      DO 132 IAT=1,8
        CALL STRIPC(IUF,OUTSTR,99,ND,1,'CIBSE ach',IER)
        CIBSEBLDN(IAT)=OUTSTR(1:LNBLNK(OUTSTR))
        CALL STRIPC(IUF,OUTSTR,99,ND,1,'CIBSE ach',IER)
        CIBSEBLDD(IAT)=OUTSTR(1:LNBLNK(OUTSTR))
        WRITE(CIBSESTR1(IAT),'(3A)')CIBSEBLDN(IAT),'; ',CIBSEBLDD(IAT)
        CALL STRIPC(IUF,OUTSTR,99,ND,1,'CIBSE ach',IER)
        K=0
        CALL EGETWI(OUTSTR,K,ICIBSESTOR(IAT,1),0,0,'-','no. floors',IER)
        CALL EGETWI(OUTSTR,K,ICIBSESTOR(IAT,2),0,0,'-','no. floors',IER)
        CALL EGETWI(OUTSTR,K,ICIBSESTOR(IAT,3),0,0,'-','no. floors',IER)
        CALL EGETWI(OUTSTR,K,ICIBSESTOR(IAT,4),0,0,'-','no. floors',IER)
        CALL STRIPC(IUF,OUTSTR,99,ND,1,'CIBSE ach',IER)
        K=0
        CALL EGETWI(OUTSTR,K,ICIBSEAREA(IAT,1),0,0,'-','area',IER)
        CALL EGETWI(OUTSTR,K,ICIBSEAREA(IAT,2),0,0,'-','area',IER)
        CALL EGETWI(OUTSTR,K,ICIBSEAREA(IAT,3),0,0,'-','area',IER)
        CALL EGETWI(OUTSTR,K,ICIBSEAREA(IAT,4),0,0,'-','area',IER)
        DO 133 IBT=1,5
          CALL STRIPC(IUF,OUTSTR,99,ND,1,'CIBSE ach',IER)
          K=0
          CALL EGETWR(OUTSTR,K,CIBSEACH(IAT,1,IBT),0.,0.,'-','ACH',IER)
          CALL EGETWR(OUTSTR,K,CIBSEACH(IAT,2,IBT),0.,0.,'-','ACH',IER)
          CALL EGETWR(OUTSTR,K,CIBSEACH(IAT,3,IBT),0.,0.,'-','ACH',IER)
          CALL EGETWR(OUTSTR,K,CIBSEACH(IAT,4,IBT),0.,0.,'-','ACH',IER)
 133    CONTINUE

C Debug.
      IF(IDEBUG.EQ.1)THEN
        if(unixok)THEN
          WRITE(6,*)CIBSEBLDN(IAT),CIBSEBLDD(IAT)
          DO 135 IBT=1,4
            WRITE(6,*)ICIBSESTOR(IAT,IBT)
            WRITE(6,*)ICIBSEAREA(IAT,IBT)
            WRITE(6,*)CIBSEACH(IAT,IBT,1)
            WRITE(6,*)CIBSEACH(IAT,IBT,2)
            WRITE(6,*)CIBSEACH(IAT,IBT,3)
            WRITE(6,*)CIBSEACH(IAT,IBT,4)
            WRITE(6,*)CIBSEACH(IAT,IBT,5)
 135      CONTINUE
        ENDIF
      ENDIF
 132  CONTINUE
      CALL ERPFREE(IUF,ISTAT)
      RETURN
      END

************************* RSBEM *****************************
C Subroutine RSBEM reads a previously defined SBEM project file and
C fills relevant common blocks.
      SUBROUTINE RSBEM
#include "building.h"
#include "sbem.h"
#include "espriou.h"
      
      integer lnblnk  ! function definition

C espriou.h provides currentfile.
      common/FILEP/IFIL
      
      integer ncomp,ncon
      common/C1/NCOMP,NCON

      CHARACTER OUTSTR*124,loutstr*248
      CHARACTER WORD*48
      LOGICAL NEAR
      integer nlinks  ! for use in scanning repeating sections

      IUF=IFIL+1
      CALL EFOPSEQ(IUF,LASBEM,1,IER)

      write(currentfile,'(a)') LASBEM(1:lnblnk(LASBEM))
      CALL STRIPC(IUF,OUTSTR,99,ND,1,'ncm file',IER)

C Get version number
      IF(OUTSTR(1:9).EQ.'*SBEM 1.0')THEN
        IVERSION=1
      ELSEIF(OUTSTR(1:9).EQ.'*SBEM 1.1')THEN
        IVERSION=2
      ELSEIF(OUTSTR(1:9).EQ.'*SBEM 1.2')THEN

C Version 1.2 assumes that the project owner simulationist contacts
C are already known from the model cfg file.
        IVERSION=3
      ENDIF

      IF(OUTSTR(1:6).NE.'*SBEM ')THEN
        CONTINUE ! WARNING MSG TO GO HERE
      ELSE

C This logic assumes that the SBEM file is formatted in
C a specific format and the data is in a specific order.
C If this is not the case then some data corruption can happen.
C Newer files will have the UPR number as the 2nd line but older
C files will include the contact information lines before this.

C The contents of the file were written for a specific number of
C zones and if the model is updated only data for the number of
C items held in the file will be scanned.
   42   CALL STRIPC(IUF,OUTSTR,99,ND,1,'NCM data line',IER)
        if(OUTSTR(1:12).eq.'Project name')then
          K=14
          CALL EGETRM(OUTSTR,K,pjname,'W','pjname',IER)
          goto 42
        elseif(OUTSTR(1:16).eq.'Building address')then
          K=18
          CALL EGETRM(OUTSTR,K,baddress,'W','NCM baddress',IER)
          goto 42
        elseif(OUTSTR(1:13).eq.'Building city')then
          K=15
          CALL EGETRM(OUTSTR,K,bcity,'W','NCM bcity',IER)
          goto 42
        elseif(OUTSTR(1:17).eq.'Building Postcode')then
          K=19
          CALL EGETRM(OUTSTR,K,buildingPC,'W','NCM buildingPC',IER)
          goto 42
        elseif(OUTSTR(1:19).eq.'Building owner name')then
          K=21
          CALL EGETRM(OUTSTR,K,ownername,'W','NCM ownername',IER)
          goto 42
        elseif(OUTSTR(1:24).eq.'Building owner telephone')then
          K=26
          CALL EGETRM(OUTSTR,K,ownertel,'W','NCM ownertel',IER)
          goto 42
        elseif(OUTSTR(1:22).eq.'Building owner address')then
          K=24
          CALL EGETRM(OUTSTR,K,owneraddr,'W','NCM owneraddr',IER)
          goto 42
        elseif(OUTSTR(1:19).eq.'Building owner city')then
          K=21
          CALL EGETRM(OUTSTR,K,ownercity,'W','NCM ownercity',IER)
          goto 42
        elseif(OUTSTR(1:23).eq.'Building owner Postcode')then
          K=25
          CALL EGETRM(OUTSTR,K,ownerPC,'W','NCM ownerPC',IER)
          goto 42
        elseif(OUTSTR(1:14).eq.'Certifier name')then
          K=16
          CALL EGETRM(OUTSTR,K,certifname,'W','NCM certifname',IER)
          goto 42
        elseif(OUTSTR(1:19).eq.'Certifier telephone')then
          K=21
          CALL EGETRM(OUTSTR,K,certTel,'W','NCM certTel',IER)
          goto 42
        elseif(OUTSTR(1:17).eq.'Certifier address')then
          K=19
          CALL EGETRM(OUTSTR,K,certaddress,'W','NCM certaddress',IER)
          goto 42
        elseif(OUTSTR(1:14).eq.'Certifier city')then
          K=16
          CALL EGETRM(OUTSTR,K,certcity,'W','NCM certcity',IER)
          goto 42
        elseif(OUTSTR(1:18).eq.'Certifier postcode')then
          K=20
          CALL EGETRM(OUTSTR,K,certPC,'W','NCM certPC',IER)
          goto 42
        elseif(OUTSTR(1:10).eq.'UPR number')then

C If the 2nd line of the file is the UPR then do not jump back to 42.
          K=12
          CALL EGETRM(OUTSTR,K,UPRN,'W','NCM UPRN',IER)
        endif

        CALL STRIPC(IUF,OUTSTR,99,ND,1,'insp_date',IER)
        K=26
        CALL EGETWI(OUTSTR,K,D_inspect,1,31,'W','NCM insp_day',IER)
        CALL EGETWI(OUTSTR,K,M_inspect,1,12,'W','insp_month',IER)
        CALL EGETWI(OUTSTR,K,Y_inspect,1900,2080,'W','insp_year',IER)
        CALL STRIPC(IUF,OUTSTR,99,ND,1,'NCM Accr_Scheme',IER)
        K=13
        CALL EGETRM(OUTSTR,K,Accr_Scheme,'W','NCM Accr_Scheme',IER)
        CALL STRIPC(IUF,OUTSTR,99,ND,1,'NCM assessRegNumber',IER)
        K=19
        CALL EGETRM(OUTSTR,K,assessRegNumber,'W','assessRegNumber',IER)
        CALL STRIPC(IUF,OUTSTR,99,ND,1,'NCM emplTradname',IER)
        K=14
        CALL EGETRM(OUTSTR,K,empl_Trading_name,'W','emplTradname',IER)
        CALL STRIPC(IUF,OUTSTR,99,ND,1,'NCM TradAddress',IER)
        K=15
        CALL EGETRM(OUTSTR,K,addr_empl_Trading,'W','TradAddress',IER)
        CALL STRIPC(IUF,OUTSTR,99,ND,1,'NCM partyDisclosure',IER)
        K=18
        CALL EGETRM(OUTSTR,K,party_disclosure,'W','partyDisclosure',IER)
        CALL STRIPC(IUF,OUTSTR,99,ND,1,'NCM qualificAssessor',IER)
        K=25
        CALL EGETRM(OUTSTR,K,qualifications_assessor,'W',
     &    'qualificAssessor',IER)
        CALL STRIPC(IUF,OUTSTR,99,ND,1,'NCM insurerCompany',IER)
        K=17
        CALL EGETRM(OUTSTR,K,insurer_Company,'W','insurerCompany',IER)
        CALL STRIPC(IUF,OUTSTR,99,ND,1,'NCM insurPolicyNumber',IER)
        K=25
        CALL EGETRM(OUTSTR,K,policyNumberInsurance,'W',
     &    'insurPolicyNumber',IER)
        CALL STRIPC(IUF,OUTSTR,99,ND,1,'NCM insurStart_date',IER)
        K=37
        CALL EGETWI(OUTSTR,K,S_Dinsur,1,31,'W','insStartDay',IER)
        CALL EGETWI(OUTSTR,K,S_Minsur,1,12,'W','insStartMonth',IER)
        CALL EGETWI(OUTSTR,K,S_Yinsur,1900,2080,'W','insStartYear',IER)
        CALL STRIPC(IUF,OUTSTR,99,ND,1,'NCM insExpiry_date',IER)
        K=39
        CALL EGETWI(OUTSTR,K,E_Dinsur,1,31,'W','insExpDay',IER)
        CALL EGETWI(OUTSTR,K,E_Minsur,1,12,'W','insExpMonth',IER)
        CALL EGETWI(OUTSTR,K,E_Yinsur,1900,2080,'W','insExpYear',IER)        
        CALL STRIPC(IUF,OUTSTR,99,ND,1,'NCM insCoverLimit',IER)
        K=31
        CALL EGETWI(OUTSTR,K,pi_limit,0,0,'-','insCoverLimit',IER)
        CALL STRIPC(IUF,OUTSTR,99,ND,1,'NCM ComplexLevel',IER)
        K=20
        CALL EGETRM(OUTSTR,K,pjLevComplexity,'W','ComplexLevel',IER)
                
        CALL STRIPC(IUF,OUTSTR,99,ND,1,'NCM Building type index',IER)
        K=21
        CALL EGETWI(OUTSTR,K,ibusertyp,0,0,'-','bldg typ index.',IER)

C The first line is not used but is helpful when scanning the file in a 
C text editor
        CALL STRIPC(IUF,OUTSTR,99,ND,1,'NCM Building type',IER)
        CALL STRIPC(IUF,OUTSTR,99,ND,1,'NCM bldg services strategy',IER)
        K=28
        CALL EGETWI(OUTSTR,K,IBSS,0,0,'-','bldg services strategy',IER)
        CALL STRIPC(IUF,OUTSTR,99,ND,1,'NCM building regulations',IER)
        K=22
        CALL EGETWI(OUTSTR,K,IRGG,0,0,'-','building regulations',IER)
        CALL STRIPC(IUF,OUTSTR,99,ND,1,'NCM bldg design stage',IER)
        K=23
        CALL EGETWI(OUTSTR,K,ISTG,0,0,'-','bldg design stage',IER)
        CALL STRIPC(IUF,OUTSTR,99,ND,1,'NCM Scottish bldg type',IER)
        K=24
        CALL EGETWI(OUTSTR,K,ISBT,0,0,'-','Scottish bldg type',IER)
        CALL STRIPC(IUF,OUTSTR,99,ND,1,'Accredited const details',IER)
        K=54
        CALL EGETRM(OUTSTR,K,SBREF,'W','NCM file',IER)
        CALL STRIPC(IUF,OUTSTR,99,ND,1,'NCM Building permeability',IER)
        K=44
        CALL EGETWR(OUTSTR,K,BINF50,0.,15.,'W','bldg permeability',IER)
        CALL STRIPC(IUF,OUTSTR,99,ND,1,'NCM permeability compl chk',IER)
        K=60
        CALL EGETRM(OUTSTR,K,APCHK,'W','perm compliance checking',IER)
      ENDIF
      do 209 iz=1,ncomp
        CALL STRIPC(IUF,OUTSTR,99,ND,1,'NCM ach per zone',IER)
        if(OUTSTR(1:17).EQ.'Zone_permeability')then
          K=0
          CALL EGETW(OUTSTR,K,WORD,'W','key word',IER)
          CALL EGETW(OUTSTR,K,WORD,'W','zone',IER)
          CALL EGETWR(OUTSTR,K,ACH(IZ),0.0,0.0,'-',
     &      'zone-light heat gains',IER)
        endif
 209  continue

C Scan the NCM system portion of the file. Find out how many systems
C and the for each system scan the tokens.
 5    CALL STRIPC(IUF,OUTSTR,99,ND,1,'NCM HVAC systems',IER)
      IF(OUTSTR(1:13).EQ.'*HVAC Systems')THEN
        K=13
        CALL EGETWI(OUTSTR,K,NCMSYS,0,0,'-','HVAC nb systems',IER)
        IF(NCMSYS.EQ.0)GOTO 1001
        DO 101 ICMSYS=1,NCMSYS

C Skip past the line identifying start of the system. Read the
C whole of the next line as the name.
          CALL STRIPC(IUF,OUTSTR,99,ND,1,'NCM HVAC systems',IER)
          CALL STRIPC(IUF,OUTSTR,99,ND,1,'NCM HVAC system name',IER)
          HVACNAME(ICMSYS)=OUTSTR(1:LNBLNK(OUTSTR))

C Read the index number but do not bother with the text.
          CALL STRIPC(IUF,OUTSTR,99,ND,1,'HVAC system index',IER)
          K=0
          CALL EGETWI(OUTSTR,K,NMSYS,0,0,'-','HVAC systems index.',IER)
          INCMSYS(ICMSYS)=NMSYS

C Read ventilation strategy but do not bother with the text.
          CALL STRIPC(IUF,OUTSTR,99,ND,1,'HVAC system index',IER)
          K=0
          CALL EGETWI(OUTSTR,K,IVENT(ICMSYS),0,0,'-','vent index.',IER)

C The heat generator line has 3 tokens - efficiency, heat index but do not
C bother with the text forming the last token.
          CALL STRIPC(IUF,OUTSTR,99,ND,1,'heat generator',IER)
          K=0        
          CALL EGETWR(OUTSTR,K,HGEF(ICMSYS),0.,10.,'W','heat eff.',IER)
          CALL EGETWI(OUTSTR,K,IHGEF(ICMSYS),0,0,'-','heat index.',IER)

C The BRUKL heat gen index has one token
          CALL STRIPC(IUF,OUTSTR,99,ND,1,'BRUKL heat gen index',IER)
          K=0
          CALL EGETWI(OUTSTR,K,IBRUKH(ICMSYS),0,0,'-','fuel type',IER)

C The fuel type includes one index number and a label but the label is not used.
          CALL STRIPC(IUF,OUTSTR,99,ND,1,'fuel type',IER)
          K=0        
          CALL EGETWI(OUTSTR,K,IFTYP(ICMSYS),0,0,'-','fuel type',IER)

C The cool generator has a efficiency and an index but the name is not used.
          CALL STRIPC(IUF,OUTSTR,99,ND,1,'cool generator',IER)
          K=0        
          CALL EGETWR(OUTSTR,K,CGEF(ICMSYS),0.,10.,'W','cool eff.',IER)
          CALL ECLOSE(CGEF(ICMSYS),0.0,1.E-5,NEAR)
          IF(.NOT.NEAR)
     &    CALL EGETWI(OUTSTR,K,ICGEF(ICMSYS),0,0,'-','cool index.',IER)

C The BRUKL cool gen index is a single token.
          CALL STRIPC(IUF,OUTSTR,99,ND,1,'BRUKL cool gen index',IER)
          K=0
          CALL EGETWI(OUTSTR,K,IBRUKC(ICMSYS),0,0,'-','brukl Cool',IER)

C The specific fan power is a single number.
          CALL STRIPC(IUF,OUTSTR,99,ND,1,'SFP',IER)
          K=0        
          CALL EGETWR(OUTSTR,K,SFPHS(ICMSYS),0.,10.,'W','SFP',IER)

C The BRUKL fan index is a single number.
          CALL STRIPC(IUF,OUTSTR,99,ND,1,'BRUKL fan index',IER)
          K=0
          CALL EGETWI(OUTSTR,K,IBRUKF(ICMSYS),0,0,'-','BRUKL index',IER)

C Scan for the ductwork and AHU leakage class and skip it.
          CALL STRIPC(IUF,OUTSTR,99,ND,1,'leakage class',IER)

C Scan for Auxiliary energy calculation constants
          CALL STRIPC(IUF,OUTSTR,99,ND,1,'calc constants',IER)          
          K=0
          CALL EGETW(OUTSTR,K,WORD,'W','Auxil flag',IER)
          if(WORD(1:23).eq.'*AuxilEnergyCalculation')then
            call EGETWR(OUTSTR,K,duct_tDLd(ICMSYS),0.0,0.0,'-',
     &        'duct constant',IER)
            call EGETWR(OUTSTR,K,AHU_tDLd(ICMSYS),0.0,0.0,'-',
     &        'AHU constant',IER)

C Debug.
C            write(6,*) 'ncmrd: ',duct_tDLd(ICMSYS),' ',AHU_tDLd(ICMSYS)
          endif
 101    CONTINUE   ! end of the loop for each system
      ELSEIF(OUTSTR(1:23).EQ.'*Zone to system linkage')THEN
        nlinks=0
        K=24
        CALL EGETWI(OUTSTR,K,nlinks,0,NCOMP,'-','nb HVAC-Zn links',IER)
        GOTO 1002   ! drop down to label 1002 to read zone lines
      ELSE
        GOTO 5     ! loop back for another scan
      ENDIF

C Section linking systems and zones. This section of the file
C is fixed format. Manual editing can easily corrupt it. The
C line contains one item of interest beginning after column 44.
 1001 CALL STRIPC(IUF,OUTSTR,99,ND,1,'*Zone to systems linkage',IER)
      nlinks=0
      K=24
      CALL EGETWI(OUTSTR,K,nlinks,0,NCOMP,'-','nb HVAC-Zn links',IER)

 1002 DO 201 IZ=1,nlinks
        CALL STRIPC(IUF,OUTSTR,99,ND,1,
     &    'Zone to systems linkage data line',IER)
        K=44
        CALL EGETWI(OUTSTR,K,IHLZ(IZ),0,0,'-','zone-system links',IER)
 201  CONTINUE

C This section begines with *DHW generators. In the case of zero
C DHW generators the logic below should be skipped.
      CALL STRIPC(IUF,OUTSTR,99,ND,1,'DHW generators',IER)
      K=16
      CALL EGETWI(OUTSTR,K,NDHWSYS,0,0,'-','zone-system links',IER)
      if(NDHWSYS.gt.0)then
        DO 202 IDHWSYS=1,NDHWSYS
          CALL STRIPC(IUF,OUTSTR,99,ND,1,'DHW generators number',IER)
          CALL STRIPC(IUF,OUTSTR,99,ND,1,'DHW generators name',IER)
          DHWNAME(IDHWSYS)=OUTSTR(1:LNBLNK(OUTSTR))
          CALL STRIPC(IUF,OUTSTR,99,ND,1,'DHW generators data',IER)
          K=0
          CALL EGETWR(OUTSTR,K,HWEF(IDHWSYS),0.,10.,'W','DHW eff',IER)
          CALL EGETWI(OUTSTR,K,IDHWS(IDHWSYS),0,0,'-','dhw index',IER)
          CALL EGETWI(OUTSTR,K,IDHFL(IDHWSYS),0,0,'-',
     &      'DHW fuel index',IER)
          CALL EGETWI(OUTSTR,K,IBRUKW(IDHWSYS),0,0,'-',
     &      'DHW IBRUKW',IER)
          CALL EGETWI(OUTSTR,K,IDHWSS(IDHWSYS),0,0,'-',
     &      'DHW IDHWSS',IER)
          CALL STRIPC(IUF,OUTSTR,99,ND,1,'DHW supplementary data',IER)
          K=0

C Get DHW system supplementary information
          DO 2001 ISP=1,5
            CALL EGETWR(OUTSTR,K,DHWSPD(IDHWSYS,ISP),0.,10.,'-',
     &        'NCM DHWSPD',IER)
 2001     CONTINUE
 202    CONTINUE
      endif

C This section defines links between zones and DHW 
C (line is written even when there is none)
      CALL STRIPC(IUF,OUTSTR,99,ND,1,'DHW zone links',IER)
      nlinks=0
      K=21
      CALL EGETWI(OUTSTR,K,nlinks,0,NCOMP,'-','nb DHW-Zn links',IER)
      DO 203 IZ=1,nlinks
        CALL STRIPC(IUF,OUTSTR,99,ND,1,'Zone to DHW linkage',IER)
        K=13
        CALL EGETWR(OUTSTR,K,DEADLEG(IZ),0.,0.,'-',
     &    'zone-DHW links deadleg length',IER)
        CALL EGETWI(OUTSTR,K,IDHWLZ(IZ),0,0,'-',
     &    'zone-DHW links coupling index',IER)
 203  CONTINUE

C This section defines links between zones and activities.
C If there are more than 5 tokens on the line the 6th is the
C number of lines of data.
      CALL STRIPC(IUF,OUTSTR,99,ND,1,'activities zone links',IER)
      if(ND.gt.4)then
        nlinks=0
        K=26
        CALL EGETWI(OUTSTR,K,nlinks,0,NCOMP,'-','nb Act-Zn links',IER)
      else
        nlinks=NCOMP
      endif
      DO 204 IZ=1,nlinks
        CALL STRIPC(IUF,OUTSTR,99,ND,1,'Zone to activity linkage',IER)
        K=0
        CALL EGETW(OUTSTR,K,WORD,'W','Zone',IER)
        CALL EGETW(OUTSTR,K,WORD,'W','name of zone',IER)
        CALL EGETWI(OUTSTR,K,IVAL,0,0,'-','high level zn-activity',IER)
        IACTYTYP(IZ)=ival
        CALL EGETWI(OUTSTR,K,ival,0,0,'-','long list zn-activity',IER)
        theactivityindex(IZ)=ival
        CALL EGETRM(OUTSTR,K,WORD,'W','label for activity',IER)
 204  CONTINUE

C This section defines links between zones and lighting. There can be
C some long strings in this section so use LSTRIPC calls. This section
C is also fixed format looking for data after column 86 or 66 or 52.
C If there are more than 5 tokens on the line the number if links is
C given otherwise assume there is one for each zone.
      CALL STRIPC(IUF,OUTSTR,99,ND,1,'lighting type zone links',IER)
      if(ND.gt.5)then
        nlinks=0
        K=30
        CALL EGETWI(OUTSTR,K,nlinks,0,NCOMP,'-','nb Act-Zn links',IER)
      else
        nlinks=NCOMP
      endif
      DO 205 IZ=1,nlinks
        CALL LSTRIPC(IUF,LOUTSTR,99,ND,1,'Zone-lighting type linkage',
     &   IER)
        IF(LOUTSTR(1:12).EQ.'User_defined')THEN
        
C Check if there is a user defined entry for lighting heat gains
          ILIGHTUSER(IZ)=1
          K=86
          CALL EGETWR(LOUTSTR,K,LIGHTWATTAGE(IZ),0.0,0.0,'-',
     &    'zone-light heat gains',IER)  
        ELSEIF(LOUTSTR(1:12).EQ.'default_type')THEN     
          ILIGHTUSER(IZ)=0
          K=66
          CALL EGETWI(LOUTSTR,K,ILITYP(IZ),0,0,'-',
     &    'zone-light type links',IER)
        ELSE

C possibly an old format - do the same as the default type
C goto is avoided here by just using the same code as the "default_type"
          ILIGHTUSER(IZ)=0
          K=52
          CALL EGETWI(LOUTSTR,K,ILITYP(IZ),0,0,'-',
     &    'zone-light type links',IER)
        ENDIF

C If lighting controls have been defined then read these in as well
        IF(IVERSION.EQ.2)THEN
          CALL STRIPC(IUF,OUTSTR,99,ND,1,'lighting controls',IER)
          K=0
          CALL EGETWI(OUTSTR,K,Ilightcontrol(IZ),0,0,'-',
     &    'lighting control type',IER)
          CALL EGETWI(OUTSTR,K,Idaylightzoning(IZ),0,0,'-',
     &    'daylighting sensor location front or front_back',IER)
          CALL EGETWI(OUTSTR,K,Ipe_control(IZ),0,0,'-',
     &    'photoelectric control',IER)
          CALL EGETWI(OUTSTR,K,IOcc_sensing(IZ),0,0,'-',
     &    'Occupancy sensing',IER)
          CALL EGETWR(OUTSTR,K,PE_sensor_PP(IZ),0.0,0.0,'-',
     &    'Photoelectric sensor parasitic power',IER)  
          CALL EGETWR(OUTSTR,K,Occ_sensor_PP(IZ),0.0,0.0,'-',
     &    'Occupancy sensor parasitic power',IER)  
          CALL EGETWR(OUTSTR,K,DFFront(IZ),0.0,0.0,'-',
     &    'Front daylight factor',IER)  
          CALL EGETWR(OUTSTR,K,DFRear(IZ),0.0,0.0,'-',
     &    'Rear daylight factor',IER)  
        ENDIF
 205  CONTINUE

C Renewable energy systems information.
      CALL STRIPC(IUF,OUTSTR,99,ND,1,'renewable energy systems',IER)
      K=36
      CALL EGETWI(OUTSTR,K,NREN,0,0,'-','no. renewable systems',IER)
      DO 107 IREN=1,NREN 
        CALL STRIPC(IUF,OUTSTR,99,ND,1,'renewable systems',IER)
        K=0
        CALL EGETW(OUTSTR,K,RENNAME(IREN),'W','Name',IER)
        CALL EGETWI(OUTSTR,K,NRENTYPE(IREN),0,0,'-','Type',IER)
        DO 108 II=1,4
          CALL EGETWR(OUTSTR,K,RENDATA(IREN,II),0.0,0.0,'-',
     &    'suppl. data',IER)  
 108    CONTINUE
 107  CONTINUE   

C Check if there is a special case for which the stripped building has been 
C configured with the rules of the notional building. This is normally
C just for DSM testing requirements. 
      call stripc(iuf,outstr,99,nd,1,'special flag',ier)
      if(outstr(1:12).eq.'*dsm_testing')then !this is not flexible, so the flag needs to exist exactly here
        K=0
        CALL EGETW(OUTSTR,K,WORD,'W','skip flag',IER)
        CALL EGETWI(OUTSTR,K,iDsmTestingFlag,0,2,'-','DSM flag',IER)
      endif   
      RETURN
      END

************************* MKSBEM *****************************
C Subroutine MKSBEM writes out a project specific SBEM NCM file that holds
C data relevant to the UK National Calculation Method.
      SUBROUTINE MKSBEM
#include "building.h"
#include "geometry.h"
#include "sbem.h"
      
      integer lnblnk  ! function definition

      common/FILEP/IFIL
      common/C21/IFCFG,cfgroot,LCFGF
      
      integer ncomp,ncon
      common/C1/NCOMP,NCON

      CHARACTER CFGROOT*24,LCFGF*72,TMPSTR*64
      integer ireadactivityindex,lnb
      
      TMPSTR=' '
      IUF=IFIL+1

C Create SBEM file based on the root name of the model.
C<< Create facility to read/write file with user defined name
      WRITE(LASBEM,'(2a)')CFGROOT_O(1:LNBLNK(CFGROOT_O)),'_str.ncm'
      CALL EFOPSEQ(IUF,LASBEM,3,IER)

C Write high level project specific detail. 
      WRITE(IUF,'(A)',IOSTAT=IOS,ERR=3) '*SBEM 1.1'

C Model cfg file holds the building and owner and simulationist info.
C      WRITE(IUF,'(2A)',IOSTAT=IOS,ERR=3)'Project name, ',
C     &  pjname(1:lnblnk(pjname))
C      WRITE(IUF,'(2A)',IOSTAT=IOS,ERR=3)'Building address, ',
C     &  baddress(1:lnblnk(baddress))
C      WRITE(IUF,'(2A)',IOSTAT=IOS,ERR=3)'Building city, ',
C     &  bcity(1:lnblnk(bcity)) 
C      WRITE(IUF,'(2A)',IOSTAT=IOS,ERR=3)'Building Postcode, ',
C     &  buildingPC(1:lnblnk(buildingPC))
C      WRITE(IUF,'(2A)',IOSTAT=IOS,ERR=3)
C     &  'Building owner name, ',ownername(1:lnblnk(ownername))
C      WRITE(IUF,'(2A)',IOSTAT=IOS,ERR=3)
C     &  'Building owner telephone, ',ownertel(1:lnblnk(ownertel))
C      WRITE(IUF,'(2A)',IOSTAT=IOS,ERR=3)
C     &  'Building owner address, ',owneraddr(1:lnblnk(owneraddr))
C      WRITE(IUF,'(2A)',IOSTAT=IOS,ERR=3)
C     &  'Building owner city, ',ownercity(1:lnblnk(ownercity))
C      WRITE(IUF,'(2A)',IOSTAT=IOS,ERR=3)
C     &  'Building owner Postcode, ',ownerPC(1:lnblnk(ownerPC))
C      WRITE(IUF,'(2A)',IOSTAT=IOS,ERR=3)'Certifier name, ',
C     &  certifname(1:lnblnk(certifname))
C      WRITE(IUF,'(2A)',IOSTAT=IOS,ERR=3)'Certifier telephone, ',
C     &  certTel(1:lnblnk(certTel))
C      WRITE(IUF,'(2A)',IOSTAT=IOS,ERR=3)
C     &  'Certifier address, ',certaddress(1:lnblnk(certaddress))
C      WRITE(IUF,'(2A)',IOSTAT=IOS,ERR=3)'Certifier city, ',
C     &  certcity(1:lnblnk(certcity))
C      WRITE(IUF,'(2A)',IOSTAT=IOS,ERR=3)'Certifier postcode, ',
C     &  certPC(1:lnblnk(certPC))

      WRITE(IUF,'(2A)',IOSTAT=IOS,ERR=3)'UPR number, ',
     &  UPRN(1:lnblnk(UPRN))
   
      WRITE(IUF,'(A,I2,A,I2,A,I4)',IOSTAT=IOS,ERR=3)
     &  'Inspection date (D/M/Y) , ',D_inspect,' ',M_inspect,' ',
     &  Y_inspect    
      WRITE(IUF,'(2A)',IOSTAT=IOS,ERR=3)'Accr scheme, ',
     &  Accr_Scheme(1:lnblnk(Accr_Scheme))
      WRITE(IUF,'(2A)',IOSTAT=IOS,ERR=3)'assessor reg numb, ',
     &  assessRegNumber(1:lnblnk(assessRegNumber))
      WRITE(IUF,'(2A)',IOSTAT=IOS,ERR=3)'trading name, ',
     &  empl_Trading_name(1:lnblnk(empl_Trading_name))
      WRITE(IUF,'(2A)',IOSTAT=IOS,ERR=3)'trade address, ',
     &  addr_empl_Trading(1:lnblnk(addr_empl_Trading))
      WRITE(IUF,'(2A)',IOSTAT=IOS,ERR=3)'party disclosure, ',
     &  party_disclosure(1:lnblnk(party_disclosure))
      WRITE(IUF,'(2A)',IOSTAT=IOS,ERR=3)'qualifications assessor, ',
     &  qualifications_assessor(1:lnblnk(qualifications_assessor))

C Insurance section.
      WRITE(IUF,'(2A)',IOSTAT=IOS,ERR=3)'insurer company, ',
     &  insurer_Company(1:lnblnk(insurer_Company))
      WRITE(IUF,'(2A)',IOSTAT=IOS,ERR=3)'insurance policy number, ',
     &  policyNumberInsurance(1:lnblnk(policyNumberInsurance)) 

      WRITE(IUF,'(A,I2,A,I2,A,I4)',IOSTAT=IOS,ERR=3)
     &  'Insurance policy start date (D/M/Y) , ',S_Dinsur,' ',
     &  S_Minsur,' ',S_Yinsur
      
      WRITE(IUF,'(A,I2,A,I2,A,I4)',IOSTAT=IOS,ERR=3)
     &  'Insurance policy expiry date (D/M/Y) , ',E_Dinsur,' ',
     &  E_Minsur,' ',E_Yinsur    
 
      WRITE(IUF,'(A,I9)',IOSTAT=IOS,ERR=3)
     &  'Insurance policy cover limit , ',pi_limit
      WRITE(IUF,'(2A)',IOSTAT=IOS,ERR=3)'project complexity, ',
     &  pjLevComplexity(1:lnblnk(pjLevComplexity))
     
      WRITE(IUF,'(A,I4)',IOSTAT=IOS,ERR=3)'Building type index, ',
     &     ibusertyp    
      IF(IBUSERTYP.NE.0)THEN
        lnb=lnblnk(BTYPNAME(ibusertyp))
        WRITE(IUF,'(2A)',IOSTAT=IOS,ERR=3)'Building type, ',
     &    BTYPNAME(ibusertyp)(1:lnb)
      ELSE
        WRITE(IUF,'(2A)',IOSTAT=IOS,ERR=3)'Building type, ',
     &     'not defined yet'
      ENDIF
      IF(IBSS.NE.0)THEN
        lnb=lnblnk(BLDSS(IBSS))
        WRITE(IUF,'(A,I3,1X,A)',IOSTAT=IOS,ERR=3)
     &  'Building services strategy, ',IBSS,BLDSS(IBSS)(1:lnb)
      ELSE
        WRITE(IUF,'(A)',IOSTAT=IOS,ERR=3)
     &  'Building services strategy, 0 not defined yet'
      ENDIF
      lnb=lnblnk(BLDREG(IRGG))
      WRITE(IUF,'(A,I3,1X,A)',IOSTAT=IOS,ERR=3)
     &'Building Regulations, ',IRGG,BLDREG(IRGG)(1:lnb)
      lnb=lnblnk(BLDSTG(ISTG))
      WRITE(IUF,'(A,I3,1X,A)',IOSTAT=IOS,ERR=3)
     &'Building Design Stage, ',ISTG,BLDSTG(ISTG)(1:lnb)
      IF(ISBT.NE.0)THEN
        lnb=lnblnk(SBTYP(ISBT))
        WRITE(IUF,'(A,I3,1X,A)',IOSTAT=IOS,ERR=3)
     &  'Scottish building type, ',ISBT,SBTYP(ISBT)(1:lnb)
        WRITE(IUF,'(2A)',IOSTAT=IOS,ERR=3)
     &  'Accredited construction details (Scotland) followed?  ',
     &  SBREF(1:lnblnk(SBREF))
      ELSE
        WRITE(IUF,'(A,I3,1X,A)',IOSTAT=IOS,ERR=3)
     &  'Scottish building type, ',ISBT,
     &  ' Scottish regulations not being followed! '
        WRITE(IUF,'(A)',IOSTAT=IOS,ERR=3)
     &  'Accredited construction details (Scotland) followed?  N/A'
      ENDIF
      WRITE(IUF,'(A,F5.2)',IOSTAT=IOS,ERR=3)
     &'Building permeability at 50Pa in m3/m2.hour, ',BINF50
      IF(APCHK.NE.'NO ')APCHK='YES'
      WRITE(IUF,'(2A)',IOSTAT=IOS,ERR=3)
     &'Compliance check to be performed on air permeability value? ',
     &  APCHK(1:lnblnk(APCHK)) 

C For every current zone in the model write permeability.
      DO 205 IZ=1,NCOMP
        WRITE(IUF,'(2A,F6.4,A)',IOSTAT=IOS,ERR=3)
     &    'Zone_permeability ',zname(iz),ACH(IZ),' # ach'
 205  CONTINUE

C Write system level detail. This includes names and indices for HVAC
C system, heat and cool generators, DHW, ventilation fans and fuel. 
C Also write BRUKL indices as defined in
C Non Domestic Heating Cooling and Ventilation Compliance Guide
C 1st Edition of May 2006
C Published by Department of Communities and Local Government, UK
      WRITE(IUF,'(A,I4,A)',IOSTAT=IOS,ERR=3)'*HVAC Systems',
     &     NCMSYS,' # total number of systems'
      DO 101 ICMSYS=1,NCMSYS
        WRITE(IUF,'(A,I3)')'HVAC system number, ',ICMSYS
        WRITE(IUF,'(A)')'# HVAC system name, index and type '
        lnb=lnblnk(HVACNAME(ICMSYS))
        WRITE(IUF,'(A)')HVACNAME(ICMSYS)(1:lnb)
        lnb=lnblnk(HSYSNAME(INCMSYS(ICMSYS)))
        WRITE(IUF,'(1X,I4,1X,A)',IOSTAT=IOS,ERR=3)INCMSYS(ICMSYS),
     &     HSYSNAME(INCMSYS(ICMSYS))(1:lnb)

        WRITE(IUF,'(1X,I4,1X,A)',IOSTAT=IOS,ERR=3)IVENT(ICMSYS),
     &    ' #Ventilation strategy '

        WRITE(IUF,'(A)')'# heat generator efficiency, index and name'
        lnb=lnblnk(SYSNAME(IHGEF(ICMSYS)))
        WRITE(IUF,'(1X,F6.3,1X,I4,1X,A)',IOSTAT=IOS,ERR=3)
     &     HGEF(ICMSYS),IHGEF(ICMSYS),SYSNAME(IHGEF(ICMSYS))(1:lnb)
        IBRUK=IBRUKLH(IFTYP(ICMSYS),IHGEF(ICMSYS),INCMSYS(ICMSYS))
        IF(IBRUK.LE.0)IBRUK=-5555
        WRITE(IUF,'(1X,I5,1X,A)',IOSTAT=IOS,ERR=3)
     &    IBRUK,' # BRUKL heat generator index number'

        WRITE(IUF,'(A)')'# fuel type index and name'
        lnb=lnblnk(FUELNAME(IFTYP(ICMSYS)))
        WRITE(IUF,'(1X,I4,1X,A)',IOSTAT=IOS,ERR=3)IFTYP(ICMSYS),
     &     FUELNAME(IFTYP(ICMSYS))(1:lnb)

        WRITE(IUF,'(A)')'# cool generator efficiency, index and name '
        IF(ICGEF(ICMSYS).NE.0)THEN
          lnb=lnblnk(SYSNAME(ICGEF(ICMSYS)+29))
          WRITE(IUF,'(1X,F6.3,1X,I4,1X,A)',IOSTAT=IOS,ERR=3)CGEF(ICMSYS)
     &    ,ICGEF(ICMSYS),SYSNAME(ICGEF(ICMSYS)+29)(1:lnb)
        ELSE
          WRITE(IUF,'(1X,F6.3,1X,A)',IOSTAT=IOS,ERR=3)CGEF(ICMSYS),
     &    ' # No cool generator specified'
        ENDIF

C Set BRUKL cooling value to -5555 if cooling system is not defined
        if(icmsys.gt.0)then
          if(ICGEF(ICMSYS).gt.0.and.INCMSYS(ICMSYS).gt.0)then
            IBRUK=IBRUKLC(ICGEF(ICMSYS),INCMSYS(ICMSYS))
          else
            IBRUK=0
          endif
        else
          IBRUK=0
        endif
        IF(IBRUK.LE.0)IBRUK=-5555

C Set BRUKL cooling value if no cooling generator is present in HVAC system        
        if(icmsys.gt.0)then
          IF(INCMSYS(ICMSYS).LE.11)IBRUK=-1111
          WRITE(IUF,'(1X,I5,A)',IOSTAT=IOS,ERR=3)
     &      IBRUK,' # BRUKL cool generator index number'

          WRITE(IUF,'(1X,F6.3,1X,A)',IOSTAT=IOS,ERR=3)SFPHS(ICMSYS),
     &      ' # Specific fan power W/l/s (0 if fans N/A)'
          WRITE(IUF,'(1X,I5,1X,A)',IOSTAT=IOS,ERR=3)
     &      IBRUKLF(INCMSYS(ICMSYS)),' # BRUKL fan index number'
          WRITE(IUF,'(6a)',IOSTAT=IOS,ERR=3)'*LeakageClass',',',
     &      ductwork(ICMSYS)(1:LNBLNK(ductwork(ICMSYS))),',',
     &      AHUleakage(ICMSYS)(1:LNBLNK(AHUleakage(ICMSYS))),
     &      ' #classes for ductwork leakage & AHU leakage'
          WRITE(IUF,'(2a,f6.4,a,f6.4,a)',IOSTAT=IOS,ERR=3)
     &      '*AuxilEnergyCalculation',',',duct_tDLd(ICMSYS),',',
     &      AHU_tDLd(ICMSYS),
     &      ' # constants for ductwork leakage & AHU leakage'
        else
          IBRUK=-1111
          WRITE(IUF,'(1X,I5,A)',IOSTAT=IOS,ERR=3)
     &      IBRUK,' # BRUKL cool generator index number'
          WRITE(IUF,'(1X,A)',IOSTAT=IOS,ERR=3)
     &      ' 0.0 # Specific fan power W/l/s (0 if fans N/A)'
          WRITE(IUF,'(1X,A)',IOSTAT=IOS,ERR=3)
     &      ' 0.0 # BRUKL fan index number'
          WRITE(IUF,'(6a)',IOSTAT=IOS,ERR=3)'*LeakageClass',',',
     &    'Not_applicable',',','Not_applicable',
     &    ' #classes for ductwork leakage & AHU leakage'
          WRITE(IUF,'(6a)',IOSTAT=IOS,ERR=3)
     &    '*AuxilEnergyCalculation',',','0.0',',','0.0',
     &    ' # constants for ductwork leakage & AHU leakage'     
        endif
 101  CONTINUE

C Write building zone to HVAC system linkage information for all current
C zones in the model.
      WRITE(IUF,'(A,I4,A)',IOSTAT=IOS,ERR=3)'*Zone to system linkage',
     &     NCOMP,' # total number of zones'
      DO 102 IZ=1,NCOMP
        IF(IHLZ(IZ).EQ.0)THEN
          TMPSTR=' No HVAC system for this zone!'
        ELSE
          lnb=lnblnk(HVACNAME(IHLZ(IZ)))
          WRITE(TMPSTR,'(2A)')' ',HVACNAME(IHLZ(IZ))(1:lnb)
        ENDIF
        WRITE(IUF,'(3A,I3,A)',IOSTAT=IOS,ERR=3)
     &    'Zone ',ZNAME(IZ),' is linked to system number ',
     &    IHLZ(IZ),TMPSTR(1:lnblnk(TMPSTR))
 102  CONTINUE

C Write DHW generator information
      WRITE(IUF,'(A,I4,A)',IOSTAT=IOS,ERR=3)'*DHW generators',
     &     NDHWSYS,' # total number of DHW generators'
      DO 103 IDHWSYS=1,NDHWSYS
        WRITE(IUF,'(A,I4)',IOSTAT=IOS,ERR=3)'DHW generator number',
     &     IDHWSYS
        WRITE(IUF,'(A)',IOSTAT=IOS,ERR=3)
     &  '# name, efficiency, type index, fuel type index, BRUKL index'
        WRITE(IUF,'(A)',IOSTAT=IOS,ERR=3)
     &  '# type (0=simple,1=storage,2=storage with circulation loop)'
        WRITE(IUF,'(A)',IOSTAT=IOS,ERR=3)DHWNAME(IDHWSYS)
        WRITE(IUF,'(F6.3,1X,I4,1X,I4,1X,I4,1X,I4,1X,I1)',IOSTAT=IOS,
     &  ERR=3)HWEF(IDHWSYS),IDHWS(IDHWSYS),IDHFL(IDHWSYS),
     &  IBRUKLW(IDHWS(IDHWSYS),IDHFL(IDHWSYS)),IDHWSS(IDHWSYS)
        WRITE(IUF,'(A)',IOSTAT=IOS,ERR=3)
     &  '# storage tank m3, system loss MJ/mnth, circ loss W/m,'
        WRITE(IUF,'(A)',IOSTAT=IOS,ERR=3)
     &  '# pump power kW, loop length m'
        WRITE(IUF,'(F6.0,1X,F7.2,1X,F6.3,1X,F6.3,1X,F6.0)')
     &  (DHWSPD(IDHWSYS,IJKL),IJKL=1,5)
 103  CONTINUE

C Write building zone to DHW system linkage information
      WRITE(IUF,'(A,I4,A)',IOSTAT=IOS,ERR=3)'*Zone to DHW linkage',
     &     NCOMP,' # total number of zones'
      WRITE(IUF,'(A)',IOSTAT=IOS,ERR=3)
     &'#Zone        dead-leg (m)    linked DHW system no. & name'
      DO 104 IZ=1,NCOMP
        IF(IDHWLZ(IZ).EQ.0)THEN
          TMPSTR=' Default DHW system'
        ELSEIF(IDHWLZ(IZ).GT.0)THEN
          WRITE(TMPSTR,'(2A)')' ',DHWNAME(IDHWLZ(IZ))
        ELSEIF(IDHWLZ(IZ).LT.0)THEN
          WRITE(TMPSTR,'(2A)')' (HVAC system) ',HVACNAME(-IDHWLZ(IZ))
        ENDIF
        WRITE(IUF,'(A,4X,F5.2,6X,I3,1X,A)',IOSTAT=IOS,ERR=3)
     &    ZNAME(IZ),DEADLEG(IZ),IDHWLZ(IZ),
     &    TMPSTR(1:lnblnk(TMPSTR))
 104  CONTINUE

C Write building zone to activity type linkage information. Note
C use only the first 40 characters of roomactname. Write for all
C current zones even if there is no activity associated with the
c zone yet.
      WRITE(IUF,'(A,I4,A)',IOSTAT=IOS,ERR=3)
     &  '*Zone to activity linkage',NCOMP,' # total number of zones'
      DO 105 IZ=1,NCOMP
        ireadactivityindex=theactivityindex(IZ)
        if(ireadactivityindex.gt.0)then
          WRITE(IUF,'(2A,I4,I6,1X,2A)') 'Zone ',zname(iz),
     &      IACTYTYP(IZ),ireadactivityindex,
     &      roomactname(ireadactivityindex)(1:40),
     &      ' # is linked to activity number '    
        else
          WRITE(IUF,'(5A)') 'Zone ',zname(iz),' 0',' 0',
     &      '# is linked to activity number 0 (undefined activity)'
        endif
 105  CONTINUE

C Write building zone to lighting type linkage information. Write for
C all current zones even if no lighting has been defined.
      WRITE(IUF,'(A)',IOSTAT=IOS,ERR=3)'*Zone to lighting type linkage'
      DO 106 IZ=1,NCOMP

C Check if there is a user defined entry for lighting heat gains
        if(ILIGHTUSER(IZ).eq.1)then
          WRITE(IUF,'(6A,1X,F7.2)') 'User_defined',' ','Zone',
     &    zname(iz),
     &    ' uses heat gains from lights of (W/m2 per 100 lux): ',
     &    '  ',LIGHTWATTAGE(IZ)
             
C Otherwise take the suggested values (gains) for the specified 
C lighting type
        elseif(ILIGHTUSER(IZ).eq.0)then
          if(ILITYP(IZ).gt.0)then
            WRITE(IUF,'(5A,1X,I4,1X,A)') 'default_type',' ','Zone',
     &      zname(iz),' is linked to lighting type number ',
     &      ILITYP(IZ),TLIGHT(ILITYP(IZ))
          else
            WRITE(IUF,'(3A)') 'Zone ',zname(iz),
     &       ' is linked to lighting type number     0 (undefined type)'
          endif
        endif

C Also include lighting control information
        write(iuf,'(3A)')
     &  '# Light control type, daylight sensor, photoelectric control',
     &  ' occupancy sensing, PEsensor & occupancy sensor parasitic ',
     &  'power, approximate daylight factors for front and rear'
        write(iuf,'(4I4,4F5.2)')
     &  Ilightcontrol(IZ),Idaylightzoning(IZ),Ipe_control(IZ),
     &  IOcc_sensing(IZ),PE_sensor_PP(IZ),Occ_sensor_PP(IZ),
     &  DFFront(IZ),DFRear(IZ)
 106  CONTINUE

C Write renewable energy systems information
      WRITE(IUF,'(A,I4)',IOSTAT=IOS,ERR=3)
     &'*Number of renewable energy systems ',NREN
      DO 107 IREN=1,NREN 
        WRITE(IUF,'(A,I4,6F10.3)')RENNAME(IREN),NRENTYPE(IREN),
     &  (RENDATA(IREN,II),II=1,4)
 107  CONTINUE     
 
C Write special flag for dsm testing rules
      write(iuf,'(a,i1)',iostat=ios,err=3)
     &'*dsm_testing ', iDsmTestingFlag
      
      CALL ERPFREE(IUF,ISTAT)
      RETURN
 3    CALL USRMSG('Problem writing data to sbem file',' ','W')
      END


************************* MKSBEM2 *****************************
C Subroutine MKSBEM2 writes out a project SBEM v1.1 file that holds
C data relevant to the UK National Calculation Method.
      SUBROUTINE MKSBEM2
#include "building.h"
#include "geometry.h"
#include "sbem.h"
      
      integer lnblnk  ! function definition

      common/FILEP/IFIL
     
      integer ncomp,ncon
      common/C1/NCOMP,NCON

      CHARACTER TMPSTR*64
      character lasbem1*72  ! alternate name for testing
      integer ireadactivityindex,lnb,lnbb
      
      TMPSTR=' '
      IUF=IFIL+1

C Create SBEM file
C<< Create facility to read/write file with user defined name
      WRITE(LASBEM1,'(2a)')CFGROOT_O(1:LNBLNK(CFGROOT_O)),'_str.ncm1'
      CALL EFOPSEQ(IUF,LASBEM1,3,IER)

C Write high level project specific detail
      WRITE(IUF,'(A)',IOSTAT=IOS,ERR=3) '*SBEM 1.1'
      WRITE(IUF,'(2A)',IOSTAT=IOS,ERR=3)'Project_name,',
     &  pjname(1:lnblnk(pjname))
      WRITE(IUF,'(A)',IOSTAT=IOS,ERR=3)'*Site'
      WRITE(IUF,'(2A)',IOSTAT=IOS,ERR=3)'Building_address,',
     &  baddress(1:lnblnk(baddress))
      WRITE(IUF,'(2A)',IOSTAT=IOS,ERR=3)'Building_city,',
     &  bcity(1:lnblnk(bcity)) 
      WRITE(IUF,'(2A)',IOSTAT=IOS,ERR=3)'Building_Postcode,',
     &  buildingPC(1:lnblnk(buildingPC))
      WRITE(IUF,'(2A)',IOSTAT=IOS,ERR=3)
     &  'Building_owner_name,',ownername(1:lnblnk(ownername))
      WRITE(IUF,'(2A)',IOSTAT=IOS,ERR=3)
     &  'Building_owner_telephone,',ownertel(1:lnblnk(ownertel))
      WRITE(IUF,'(2A)',IOSTAT=IOS,ERR=3)
     &  'Building_owner_address,',owneraddr(1:lnblnk(owneraddr))
      WRITE(IUF,'(2A)',IOSTAT=IOS,ERR=3)
     &  'Building_owner_city,',ownercity(1:lnblnk(ownercity))
      WRITE(IUF,'(2A)',IOSTAT=IOS,ERR=3)
     &  'Building_owner_Postcode,',ownerPC(1:lnblnk(ownerPC))
      WRITE(IUF,'(A)',IOSTAT=IOS,ERR=3)'*Certifier'
      WRITE(IUF,'(2A)',IOSTAT=IOS,ERR=3)'Certifier_name,',
     &  certifname(1:lnblnk(certifname))
      WRITE(IUF,'(2A)',IOSTAT=IOS,ERR=3)'Certifier_telephone,',
     &  certTel(1:lnblnk(certTel))
      WRITE(IUF,'(2A)',IOSTAT=IOS,ERR=3)
     &  'Certifier_address,',certaddress(1:lnblnk(certaddress))
      WRITE(IUF,'(2A)',IOSTAT=IOS,ERR=3)'Certifier_city,',
     &  certcity(1:lnblnk(certcity))
      WRITE(IUF,'(2A)',IOSTAT=IOS,ERR=3)'Certifier_postcode,',
     &  certPC(1:lnblnk(certPC))
      WRITE(IUF,'(2A)',IOSTAT=IOS,ERR=3)'UPR_number,',
     &  UPRN(1:lnblnk(UPRN))
   
      WRITE(IUF,'(A,I2,A,I2,A,I4,A)',IOSTAT=IOS,ERR=3)
     &  'Inspection_date,',D_inspect,',',M_inspect,',',
     &  Y_inspect,' # (D/M/Y)'   
      WRITE(IUF,'(2A)',IOSTAT=IOS,ERR=3)'Accr_scheme,',
     &  Accr_Scheme(1:lnblnk(Accr_Scheme))
      WRITE(IUF,'(2A)',IOSTAT=IOS,ERR=3)'Assessor_reg_numb,',
     &  assessRegNumber(1:lnblnk(assessRegNumber))
      WRITE(IUF,'(2A)',IOSTAT=IOS,ERR=3)'Trading_name,',
     &  empl_Trading_name(1:lnblnk(empl_Trading_name))
      WRITE(IUF,'(2A)',IOSTAT=IOS,ERR=3)'Trade_address,',
     &  addr_empl_Trading(1:lnblnk(addr_empl_Trading))
      WRITE(IUF,'(2A)',IOSTAT=IOS,ERR=3)'Party_disclosure,',
     &  party_disclosure(1:lnblnk(party_disclosure))
      WRITE(IUF,'(2A)',IOSTAT=IOS,ERR=3)'Qualifications_assessor,',
     &  qualifications_assessor(1:lnblnk(qualifications_assessor))

C Insurance section.
      WRITE(IUF,'(A)',IOSTAT=IOS,ERR=3)'*Insurance'
      WRITE(IUF,'(2A)',IOSTAT=IOS,ERR=3)'Insurer_company,',
     &  insurer_Company(1:lnblnk(insurer_Company))
      WRITE(IUF,'(2A)',IOSTAT=IOS,ERR=3)'Insurance_policy_number,',
     &  policyNumberInsurance(1:lnblnk(policyNumberInsurance)) 

      WRITE(IUF,'(A,I2,A,I2,A,I4,A)',IOSTAT=IOS,ERR=3)
     &  'Insurance_policy_start_date,',S_Dinsur,',',
     &  S_Minsur,',',S_Yinsur,'  # (D/M/Y)'
      
      WRITE(IUF,'(A,I2,A,I2,A,I4,A)',IOSTAT=IOS,ERR=3)
     &  'Insurance_policy_expiry_date,',E_Dinsur,',',
     &  E_Minsur,',',E_Yinsur,'  # (D/M/Y)'    
 
      WRITE(IUF,'(A,I9)',IOSTAT=IOS,ERR=3)
     &  'Insurance_policy_cover_limit,',pi_limit
      WRITE(IUF,'(2A)',IOSTAT=IOS,ERR=3)'Project_complexity,',
     &  pjLevComplexity(1:lnblnk(pjLevComplexity))
     
      WRITE(IUF,'(A)',IOSTAT=IOS,ERR=3)'*Building'
      WRITE(IUF,'(A,I3)',IOSTAT=IOS,ERR=3)'Building_type_index,',
     &     ibusertyp    
      IF(IBUSERTYP.NE.0)THEN
        lnb=lnblnk(BTYPNAME(ibusertyp))
        WRITE(IUF,'(2A)',IOSTAT=IOS,ERR=3)'Building_type,',
     &    BTYPNAME(ibusertyp)(1:lnb)
      ELSE
        WRITE(IUF,'(2A)',IOSTAT=IOS,ERR=3)'Building_type,',
     &     'not defined yet'
      ENDIF
      IF(IBSS.NE.0)THEN
        lnb=lnblnk(BLDSS(IBSS))
        WRITE(IUF,'(A,I2,2A)',IOSTAT=IOS,ERR=3)
     &  'Building_services_strategy,',IBSS,',',BLDSS(IBSS)(1:lnb)
      ELSE
        WRITE(IUF,'(A)',IOSTAT=IOS,ERR=3)
     &  'Building_services_strategy, 0, not defined yet'
      ENDIF
      lnb=lnblnk(BLDREG(IRGG))
      WRITE(IUF,'(A,I2,2A)',IOSTAT=IOS,ERR=3)
     &'Building_regulations,',IRGG,',',BLDREG(IRGG)(1:lnb)
      lnb=lnblnk(BLDSTG(ISTG))
      WRITE(IUF,'(A,I2,2A)',IOSTAT=IOS,ERR=3)
     &'Building_design_stage,',ISTG,',',BLDSTG(ISTG)(1:lnb)
      IF(ISBT.NE.0)THEN
        lnb=lnblnk(SBTYP(ISBT))
        WRITE(IUF,'(A,I2,2A)',IOSTAT=IOS,ERR=3)
     &  'Scottish_building_type,',ISBT,',',SBTYP(ISBT)(1:lnb)
        WRITE(IUF,'(2A)',IOSTAT=IOS,ERR=3)
     &  'Accredited_construction_details_Scotland,',
     &  SBREF(1:lnblnk(SBREF))
      ELSE
        WRITE(IUF,'(A,I2,2A)',IOSTAT=IOS,ERR=3)
     &  'Scottish_building_type,',ISBT,',',
     &  'Scottish regulations not being followed'
        WRITE(IUF,'(A)',IOSTAT=IOS,ERR=3)
     &  'Accredited_construction_details_Scotland,N/A'
      ENDIF
      WRITE(IUF,'(A)',IOSTAT=IOS,ERR=3)'*Permeability'
      WRITE(IUF,'(A,F5.2)',IOSTAT=IOS,ERR=3)
     &'Building_permeability_at_50Pa,',BINF50
      IF(APCHK.NE.'NO ')APCHK='YES'
      WRITE(IUF,'(2A)',IOSTAT=IOS,ERR=3)
     &'Compliance_check_on_air_permeability,',
     &  APCHK(1:lnblnk(APCHK)) 

      DO 205 IZ=1,NCOMP
        lnb=lnblnk(zname(iz))
        WRITE(IUF,'(3A,F6.4,A)',IOSTAT=IOS,ERR=3)
     &  'Zone_permeability,',zname(iz)(1:lnb),',',ACH(IZ),
     &  ' # zone name, ach'
 205  CONTINUE

C Write system level detail. This includes names and indices for HVAC
C system, heat and cool generators, DHW, ventilation fans and fuel. 
C Also write BRUKL indices as defined in
C Non Domestic Heating Cooling and Ventilation Compliance Guide
C 1st Edition of May 2006
C Published by Department of Communities and Local Government, UK
      WRITE(IUF,'(A,I4,A)',IOSTAT=IOS,ERR=3)'*HVAC,',
     &  NCMSYS,' # total number of HVAC systems'
      DO 101 ICMSYS=1,NCMSYS
        WRITE(IUF,'(A)')'HVAC_system_start'
        lnb=lnblnk(HVACNAME(ICMSYS))
        WRITE(IUF,'(3A)',IOSTAT=IOS,ERR=3) 'HVAC_system_name,',
     &    HVACNAME(ICMSYS)(1:lnb),' # system name (from user)'
        lnbb=lnblnk(HSYSNAME(INCMSYS(ICMSYS)))
        WRITE(IUF,'(A,I3,3A)',IOSTAT=IOS,ERR=3)'HVAC_system_type,',
     &    INCMSYS(ICMSYS),',',
     &    HSYSNAME(INCMSYS(ICMSYS))(1:lnbb),' # index & type name'

        WRITE(IUF,'(A,F6.3,A)',IOSTAT=IOS,ERR=3)
     &    'HVAC_heat_gen_eff,',HGEF(ICMSYS),
     &    ' # heat gen efficiency'
        lnb=lnblnk(SYSNAME(IHGEF(ICMSYS)))
        WRITE(IUF,'(A,I4,3A)',IOSTAT=IOS,ERR=3)
     &    'HVAC_heat_gen_index,',IHGEF(ICMSYS),',',
     &    SYSNAME(IHGEF(ICMSYS))(1:lnb),
     &    ' # HVAC heat gen index & name'

        IBRUK=IBRUKLH(IFTYP(ICMSYS),IHGEF(ICMSYS),INCMSYS(ICMSYS))
        IF(IBRUK.LE.0)IBRUK=-5555
        WRITE(IUF,'(A,I4,A)',IOSTAT=IOS,ERR=3)
     &    'BRUKL_heat_gen_index,',IBRUK,
     &    ' # BRUKL heat generator index number'

        lnb=lnblnk(FUELNAME(IFTYP(ICMSYS)))
        WRITE(IUF,'(A,I4,3A)',IOSTAT=IOS,ERR=3)
     &    'HVAC_heat_gen_fuel,',IFTYP(ICMSYS),',',
     &    FUELNAME(IFTYP(ICMSYS))(1:lnb),
     &    ' # HVAC heat fuel type index & name'

        IF(ICGEF(ICMSYS).NE.0)THEN
          WRITE(IUF,'(A,F6.3,A)',IOSTAT=IOS,ERR=3)
     &      'HVAC_cool_gen_eff,',CGEF(ICMSYS),
     &      ' # cool gen efficiency'
          lnb=lnblnk(SYSNAME(ICGEF(ICMSYS)+29))
          WRITE(IUF,'(A,F6.3,A,I4,3A)',IOSTAT=IOS,ERR=3)
     &      'HVAC_cool_gen_index,',ICGEF(ICMSYS),',',
     &      SYSNAME(ICGEF(ICMSYS)+29)(1:lnb),
     &      ' # HVAC cool gen index & name'
        ELSE
          WRITE(IUF,'(A,F6.3,A)',IOSTAT=IOS,ERR=3)
     &      'HVAC_cool_gen_eff,',CGEF(ICMSYS),
     &      ' # No cool generator specified'
        ENDIF

C Set BRUKL cooling value to -5555 if cooling system is not defined
        if(icmsys.gt.0)then
          if(ICGEF(ICMSYS).gt.0.and.INCMSYS(ICMSYS).gt.0)then
            IBRUK=IBRUKLC(ICGEF(ICMSYS),INCMSYS(ICMSYS))
          else
            IBRUK=0
          endif
        else
          IBRUK=0
        endif
        IF(IBRUK.LE.0)IBRUK=-5555

C Set BRUKL cooling value if no cooling generator is present in HVAC system        
        if(icmsys.gt.0)then
          IF(INCMSYS(ICMSYS).LE.11)IBRUK=-1111
          WRITE(IUF,'(A,I5,A)',IOSTAT=IOS,ERR=3)
     &      'BRUKL_cool_gen_index,',IBRUK,
     &      ' # BRUKL cool generator index number'

          WRITE(IUF,'(A,F6.3,A)',IOSTAT=IOS,ERR=3)
     &      'HVAC_fan_power,',SFPHS(ICMSYS),
     &      ' # Specific fan power W/l/s (0 if fans N/A)'
          WRITE(IUF,'(A,I5,A)',IOSTAT=IOS,ERR=3)
     &      'BRUKL_fan_index,',IBRUKLF(INCMSYS(ICMSYS)),
     &      ' # BRUKL fan index number'

C Air leakage data.
          lnb=lnblnk(ductwork(ICMSYS))
          lnbb=lnblnk(AHUleakage(ICMSYS))
          WRITE(IUF,'(5a)',IOSTAT=IOS,ERR=3)'*LeakageClass,',
     &    ductwork(ICMSYS)(1:lnb),',',AHUleakage(ICMSYS)(1:lnbb),
     &    ' # classes for ductwork leakage & AHU leakage respectively'
          WRITE(IUF,'(2a,f6.4,a,f6.4,a)',IOSTAT=IOS,ERR=3)
     &    '*AuxilEnergyCalculation',',',duct_tDLd(ICMSYS),',',
     &    AHU_tDLd(ICMSYS),
     &    ' # constants for ductwork leakage & AHU leakage respectively'
        else
          IBRUK=-1111
          WRITE(IUF,'(I5,A)',IOSTAT=IOS,ERR=3)
     &      IBRUK,' # BRUKL cool generator index number'
          WRITE(IUF,'(A)',IOSTAT=IOS,ERR=3)
     &      ' 0.0 # Specific fan power W/l/s (0 if fans N/A)'
          WRITE(IUF,'(A)',IOSTAT=IOS,ERR=3)
     &      ' 0.0 # BRUKL fan index number'
          WRITE(IUF,'(3a)',IOSTAT=IOS,ERR=3)'*LeakageClass,',
     &    'Not_applicable,Not_applicable',
     &    ' #classes for ductwork leakage & AHU leakage respectively'
          WRITE(IUF,'(2a)',IOSTAT=IOS,ERR=3)
     &    '*AuxilEnergyCalculation,0.0,0.0',
     &    ' # constants for ductwork leakage & AHU leakage respectively'     
        endif
 101  CONTINUE

C Write building zone to HVAC system linkage information
      WRITE(IUF,'(A)',IOSTAT=IOS,ERR=3)'*Zone_to_system_linkage'
      WRITE(IUF,'(A)',IOSTAT=IOS,ERR=3)
     &'# Tag, Zone, HVAC index, HVAC name'
      DO 102 IZ=1,NCOMP
        IF(IHLZ(IZ).EQ.0)THEN
          TMPSTR='No HVAC system for this zone!'
        ELSE
          lnb=lnblnk(HVACNAME(IHLZ(IZ)))
          WRITE(TMPSTR,'(A)') HVACNAME(IHLZ(IZ))(1:lnb)
        ENDIF
        lnb=lnblnk(ZNAME(IZ))
        WRITE(IUF,'(3A,I3,2A)',IOSTAT=IOS,ERR=3)
     &   'Zone_Sys_link,',ZNAME(IZ)(1:lnb),',',
     &   IHLZ(IZ),',',TMPSTR(1:lnblnk(TMPSTR))
 102  CONTINUE

C Write DHW generator information
      WRITE(IUF,'(A,I4,A)',IOSTAT=IOS,ERR=3)'*DHW_generators,',
     &  NDHWSYS,' # total number of DHW generators'
      DO 103 IDHWSYS=1,NDHWSYS
        WRITE(IUF,'(A,I4)',IOSTAT=IOS,ERR=3)'DHW_generator_number,',
     &    IDHWSYS
        WRITE(IUF,'(A)',IOSTAT=IOS,ERR=3)
     &  '# name, efficiency, type index, fuel type index, BRUKL index'
        WRITE(IUF,'(A)',IOSTAT=IOS,ERR=3)
     &  '# type (0=simple,1=storage,2=storage with circulation loop)'
        lnb=lnblnk(DHWNAME(IDHWSYS))
        WRITE(IUF,'(2A,F6.3,A,I4,A,I4,A,I4,A,I4)',IOSTAT=IOS,ERR=3)
     &    DHWNAME(IDHWSYS)(1:lnb),',',HWEF(IDHWSYS),',',IDHWS(IDHWSYS),
     &    ',',IDHFL(IDHWSYS),',',IBRUKLW(IDHWS(IDHWSYS),
     &    IDHFL(IDHWSYS)),',',IDHWSS(IDHWSYS)
        WRITE(IUF,'(2A)',IOSTAT=IOS,ERR=3)
     &    '# storage tank m3, system loss MJ/mnth, circ loss W/m,',
     &    ' pump power kW, loop length m'
        WRITE(IUF,'(F6.0,1X,F7.2,1X,F6.3,1X,F6.3,1X,F6.0)')
     &    (DHWSPD(IDHWSYS,IJKL),IJKL=1,5)
 103  CONTINUE

C Write building zone to DHW system linkage information
      WRITE(IUF,'(A)',IOSTAT=IOS,ERR=3)'*Zone_to_DHW_linkage'
      WRITE(IUF,'(A)',IOSTAT=IOS,ERR=3)
     &'# Tag, Zone, dead-leg (m), linked DHW system no. & name'
      DO 104 IZ=1,NCOMP
        IF(IDHWLZ(IZ).EQ.0)THEN
          TMPSTR='Default DHW system'
        ELSEIF(IDHWLZ(IZ).GT.0)THEN
          WRITE(TMPSTR,'(A)')DHWNAME(IDHWLZ(IZ))
        ELSEIF(IDHWLZ(IZ).LT.0)THEN
          WRITE(TMPSTR,'(2A)')'(HVAC system) ',HVACNAME(-IDHWLZ(IZ))
        ENDIF
        lnb=lnblnk(ZNAME(IZ))
        WRITE(IUF,'(3A,F5.2,A,I3,2A)',IOSTAT=IOS,ERR=3)
     &    'Zone_DHW_link,',ZNAME(IZ)(1:lnb),',',DEADLEG(IZ),',',
     &    IDHWLZ(IZ),',',TMPSTR(1:lnblnk(TMPSTR))
 104  CONTINUE

C Write building zone to activity type linkage information
      WRITE(IUF,'(A)',IOSTAT=IOS,ERR=3)'*Zone_to_activity_linkage'
      WRITE(IUF,'(A)',IOSTAT=IOS,ERR=3)
     &'# Tag, Zone, activity type, activity (csv) index, name'
      DO 105 IZ=1,NCOMP
        ireadactivityindex=theactivityindex(IZ)
        if(ireadactivityindex.gt.0)then
          lnb=lnblnk(zname(iz))
          lnbb=lnblnk(roomactname(ireadactivityindex))
          WRITE(IUF,'(3A,I3,A,I4,2A)') 'Zone_activity_link,',
     &      zname(iz)(1:lnb),',',IACTYTYP(IZ),',',ireadactivityindex,
     &      ',',roomactname(ireadactivityindex)(1:lnbb)
        else
          lnb=lnblnk(zname(iz))
          WRITE(IUF,'(5A)') 'Zone_activity_link,',
     &      zname(iz)(1:lnb),',0',',0',
     &      ' # is linked to activity number 0 (undefined activity)'
        endif
 105  CONTINUE

C Write building zone to lighting type linkage information
      WRITE(IUF,'(A)',IOSTAT=IOS,ERR=3)'*Zone_to_lighting_type_linkage'
      DO 106 IZ=1,NCOMP

C Check if there is a user defined entry for lighting heat gains
        if(ILIGHTUSER(IZ).eq.1)then
          lnb=lnblnk(zname(iz))
          WRITE(IUF,'(3A,F7.2,A)') 'User_defined,Zone,',
     &    zname(iz)(1:lnb),',',LIGHTWATTAGE(IZ),
     &    ' # uses heat gains from lights of (W/m2 per 100 lux)'
     &    
             
C Otherwise take the suggested values (gains) for the specified 
C lighting type
        elseif(ILIGHTUSER(IZ).eq.0)then
          if(ILITYP(IZ).gt.0)then
            lnb=lnblnk(zname(iz))
            lnbb=lnblnk(TLIGHT(ILITYP(IZ)))
            WRITE(IUF,'(3A,I3,3A)') 'Default_type,Zone',
     &        zname(iz)(1:lnb),',',ILITYP(IZ),',',
     &        TLIGHT(ILITYP(IZ))(1:lnbb),
     &        ' # linked to lighting type number'
          else
            lnb=lnblnk(zname(iz))
            WRITE(IUF,'(3A)') 'Undefined_type,Zone',zname(iz)(1:lnb),
     &       ' # is linked to lighting type number 0 (undefined type)'
          endif
        endif
 106  CONTINUE

C Write renewable energy systems information.
      WRITE(IUF,'(A,I3)',IOSTAT=IOS,ERR=3)
     &'*Number_of_renewable_systems,',NREN
      if(NREN.gt.0)then
        DO 107 IREN=1,NREN 
          lnb=lnblnk(RENNAME(IREN))
          WRITE(IUF,'(2A,I4,A,6F10.3)')RENNAME(IREN)(1:lnb),',',
     &      NRENTYPE(IREN),',',(RENDATA(IREN,II),II=1,4)
 107    CONTINUE
      endif  
      
C Write special flag for dsm testing rules
      write(iuf,'(a,i1)',iostat=ios,err=3)
     &'*dsm_testing ', iDsmTestingFlag
             
      CALL ERPFREE(IUF,ISTAT)
      RETURN
 3    CALL USRMSG('Problem writing data to sbem file',' ','W')
      END
