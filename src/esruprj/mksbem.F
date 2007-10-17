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
C  SBMEDIT  define high level data associated with UK SBEM methodology.
C  EMKSBM   creates a model variant based on the SBEM methodology. 
C  ENLARGE  performs geometric enlargement (contraction) of a surface.
C  MKSBEM   writes a project specific *.ncm file
C  RSBEM    reads a project specific *.ncm file
C  NCMCAL   checks models, simulates notional and original model and
C           generates UK NCM data in the form of the BRUKL input file

C ********************* sbempr
C Subroutine SBEMPR reads SBEM database
      SUBROUTINE SBEMPR(IER)
#include "esprdbfile.h"
C esprdbfile.h supplies the following:
C LAPRES,IAPRES (for window pressure databases)
C LOPTDB,IOPTDB (for optical database)
C LPCDB/IPCDB (for plant template database)
C LPRFDB,IPRODB (for event profile database)
C LFMAT,IFMAT,LFMUL,IFMUL (for materials and constructions database)
C ICLIM,LCLIM (for climate file)
C LSBEM (SBEM database)
C default file names for databases
#include "espriou.h"
C espriou.h provides currentfile.
#include "sbem.h"

C IFIL base file unit number (set in MAIN of each application).
      common/OUTIN/IUOUT,IUIN
      COMMON/FILEP/IFIL
      COMMON/DEFLT2/DFCFG,DFCTL,DEFRLB,DAFRES,DAPROB,DPNF,DSBEM

      character*72 DFCFG,DFCTL,DEFRLB,DAPROB,DAFRES,DPNF,DSBEM
      CHARACTER*124 OUTSTR
      CHARACTER*42 WORD,FUELNAM
      CHARACTER SYSNAM*60

C Initialise counters
      IDEBUG=0
      IFUEL=0
      ISYS=0
      IHSYS=0
      IBRUK=0

C Read SBEM database
      LSBEM=DSBEM
      IUF=IFIL+1
      CALL EFOPSEQ(IUF,LSBEM,1,IER)
      write(currentfile,'(a)') LSBEM(1:lnblnk(LSBEM))
      CALL STRIPC(IUF,OUTSTR,99,ND,1,'SBEM db',IER)
      IF(OUTSTR(1:5).NE.'*sbem')CONTINUE ! WARNING MSG TO GO HERE
      CALL STRIPC(IUF,OUTSTR,99,ND,1,'SBEM db',IER)
      K=9
      CALL EGETWR(OUTSTR,K,VERSBEM,1.,2.,'W','Version no.',IER)
      CALL STRIPC(IUF,OUTSTR,99,ND,1,'SBEM db',IER)
      K=12
      CALL EGETWR(OUTSTR,K,SFPDEF,0.,20.,'W','Version no.',IER)

C Read fuel types names and CO2 emission ratings
      CALL STRIPC(IUF,OUTSTR,99,ND,1,'SBEM db',IER)
      IF(OUTSTR(1:11).NE.'*fuel_start')CONTINUE ! WARNING MEG TO GO HERE
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
          WRITE(FUELNAM,'(A,1X,A)')FUELNAM(1:ILEN),WORD
          ILEN=LNBLNK(WORD)+ILEN+1
 110  CONTINUE
      FUELNAME(IFUEL)=FUELNAM(1:ILEN)
      CALL EGETWR(OUTSTR,K,FUELCO2(IFUEL),0.001,10.,
     &            'W','fuel CO2 rating',IER)

C Debug...
      IF(IDEBUG.EQ.1)THEN
        write(6,*)fuelname(ifuel)!,fuelCO2(ifuel)
        write(6,*)'SBEM db read start'
      ENDIF
      GOTO 1

C Now read system types, default efficiencies & applicable HVAC systems
  2   CALL STRIPC(IUF,OUTSTR,99,ND,1,'SBEM db',IER)
      IF(OUTSTR(1:19).NE.'*system_types_start')CONTINUE ! WARNING MEG TO GO HERE
  3   ISYS=ISYS+1
      SYSNAM=" "
      ILEN=1
      CALL STRIPC(IUF,OUTSTR,99,ND,1,'SBEM db',IER)
      IF(OUTSTR(1:17).EQ.'*end_system_types')GOTO 4
      K=0
      CALL EGETWI(OUTSTR,K,NIT,0,0,'-','system index no.',IER)
      CALL EGETWI(OUTSTR,K,NIT,0,0,'-','nb items',IER)
      DO 20 IIT=1,NIT
          CALL EGETW(OUTSTR,K,WORD,'W','system name',IER)
          WRITE(SYSNAM,'(A,1X,A)')SYSNAM(1:ILEN),WORD
          ILEN=LNBLNK(WORD)+ILEN+1
 20   CONTINUE
      SYSNAME(ISYS)=SYSNAM(1:ILEN)
      CALL EGETWR(OUTSTR,K,SYSEFF(ISYS),-1.,10.,
     &            'W','default efficiency',IER)

C Debug...
      IF(IDEBUG.EQ.1)THEN
        WRITE(6,*)SYSNAM(1:ilen),syseff(isys)
      ENDIF

      CALL EGETWI(OUTSTR,K,NIT,0,0,'-','nb items',IER)
      DO 30 IIT=1,NIT
        CALL EGETWI(OUTSTR,K,IHT,0,0,'-','system index no.',IER)
        ISYSAPP(ISYS,IHT)=1
 30   CONTINUE
      GOTO 3

C Now read HVAC system types 
 4    CALL STRIPC(IUF,OUTSTR,99,ND,1,'start HVAC systems',IER)
      IF(OUTSTR(1:18).NE.'*HVAC_system_start')CONTINUE ! WARNING MEG TO GO HERE
 5    IHSYS=IHSYS+1
      CALL STRIPC(IUF,OUTSTR,99,ND,1,'HVAC system no.',IER)
      IF(OUTSTR(1:16).EQ.'*end_HVAC_system')GOTO 6
      CALL STRIPC(IUF,OUTSTR,99,ND,1,'HVAC system name',IER)
      HSYSNAME(IHSYS)=OUTSTR(1:70)

C Debug...
      IF(IDEBUG.EQ.1)THEN
        WRITE(6,*)HSYSNAME(IHSYS)
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
          IF(IDEBUG.EQ.1)THEN
            write(6,*)"cool generator eff ",syseffc(ics,ihsys),ihsys
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
      GOTO 5

C Read SBEM building types and corresponding building type index
C Interestingly there are 3 indices for 29 building types
 6    CALL STRIPC(IUF,OUTSTR,99,ND,1,'SBEM bldg type',IER)
      IF(OUTSTR(1:16).NE.'*bldg_type_start')CONTINUE ! WARNING MsG TO GO HERE
      DO 80 IBT=1,MBT
        CALL STRIPC(IUF,OUTSTR,99,ND,1,'SBEM bldg type',IER)
        K=0
        CALL EGETWI(OUTSTR,K,IBTYPNDX(IBT),0,0,'-','nb items',IER)
        BTYPNAME(IBT)=OUTSTR(3:45)
 80   CONTINUE

C Read end of building type information
      CALL STRIPC(IUF,OUTSTR,99,ND,1,'SBEM bldg type',IER)

C Read building services strategy
      CALL STRIPC(IUF,OUTSTR,99,ND,1,'SBEM bldg type',IER)
      IF(OUTSTR(1:23).NE.'*bldg_services_strategy')CONTINUE ! WARNING MsG TO GO HERE
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

C Debug...
      IF(IDEBUG.EQ.1)THEN
        write(6,*)ibt,bldif(ibt),bldlzc(ibt),bldss(ibt)
      ENDIF
      GOTO 7

C Read building regulations type
 8    CALL STRIPC(IUF,OUTSTR,99,ND,1,'bldg regs',IER)
      IF(OUTSTR(1:17).NE.'*bldg_regulations')CONTINUE ! WARNING MsG TO GO HERE
      IBT=0
 9    CALL STRIPC(IUF,OUTSTR,99,ND,1,'bldg regs',IER)
      IF(OUTSTR(1:13).EQ.'*end_bld_regs')GOTO 10
      IBT=IBT+1
      BLDREG(IBT)=OUTSTR(1:LNBLNK(OUTSTR))

C Debug...
      IF(IDEBUG.EQ.1)THEN
        write(6,*)ibt,bldreg(ibt)
      ENDIF
      GOTO 9

C Read building design stage
 10   CALL STRIPC(IUF,OUTSTR,99,ND,1,'bldg des stage',IER)
      IF(OUTSTR(1:9).NE.'*br_stage')CONTINUE ! WARNING MsG TO GO HERE
      IBT=0
 11   CALL STRIPC(IUF,OUTSTR,99,ND,1,'bldg des stage',IER)
      IF(OUTSTR(1:13).EQ.'*end_br_stage')GOTO 12
      IBT=IBT+1
      BLDSTG(IBT)=OUTSTR(1:LNBLNK(OUTSTR))
      GOTO 11

C Read Scottish building types
 12   CALL STRIPC(IUF,OUTSTR,99,ND,1,'scot bldg type',IER)
      IF(OUTSTR(1:20).NE.'*scottish_bldg_types')CONTINUE ! WARNING MsG TO GO HERE
      IBT=0
 13   CALL STRIPC(IUF,OUTSTR,99,ND,1,'scot bldg type',IER)
      IF(OUTSTR(1:24).EQ.'*end_scottish_bldg_types')GOTO 14
      IBT=IBT+1
      SBTYP(IBT)=OUTSTR(1:LNBLNK(OUTSTR))
      GOTO 13

C Read DHW generators, efficiencies and BRUKL indices
 14   CALL STRIPC(IUF,OUTSTR,99,ND,1,'DHW generators',IER)
      IF(OUTSTR(1:15).NE.'*DHW_generators')CONTINUE ! WARNING MsG TO GO HERE      
      IBT=1
 15   CALL STRIPC(IUF,OUTSTR,99,ND,1,'DHW generators',IER)
      IF(OUTSTR(1:19).EQ.'*end_DHW_generators')GOTO 16
      DHWGEN(IBT)=OUTSTR(1:25)
      K=25
      CALL EGETWR(OUTSTR,K,DHWEFF(IBT,1),-1.,1.,'W',
     &'system eff before 1998',IER)
      CALL EGETWR(OUTSTR,K,DHWEFF(IBT,2),-1.,1.,'W',
     &'system eff after 1998',IER)
      DO 90 IBR=1,MWS
        CALL EGETWI(OUTSTR,K,IBRUKLW(IBT,IBR),0,0,'-',
     &  'BRUKL DHW indices',IER)

C Debug...
      IF(IDEBUG.EQ.1)THEN
        write(6,*)dhwgen(ibt)
     &  ,dhweff(ibt,1),dhweff(ibt,2),ibruklw(ibt,ibr)
      ENDIF
 90   CONTINUE
      IBT=IBT+1
      GOTO 15

C Read activity types sorted by building type
 16   CALL STRIPC(IUF,OUTSTR,99,ND,1,'Activity types',IER)
      IF(OUTSTR(1:23).NE.'*activities_by_building')CONTINUE ! WARNING MsG TO GO HERE      
      IAT=1
 17   CALL STRIPC(IUF,OUTSTR,99,ND,1,'Activity types',IER)
      IF(OUTSTR(1:19).EQ.'*end_activities_by_')GOTO 18
      K=0
      CALL EGETWI(OUTSTR,K,NTSTR,0,0,'-','no. text strings',IER)
      CALL EGETWI(OUTSTR,K,NCBTP,0,0,'-','crrspnd bld typ',IER)
      ILEN=1
      DO 100 ITSTR=1,NTSTR
        CALL EGETW(OUTSTR,K,WORD,'W','system name',IER)
        WRITE(SYSNAM,'(A,1X,A)')SYSNAM(1:ILEN),WORD
        ILEN=LNBLNK(WORD)+ILEN+1
 100  CONTINUE
      ATYPNAME(IAT)=SYSNAM(1:40)

C Debug...
      IF(IDEBUG.EQ.1)THEN
        WRITE(6,*)IAT,' ',ATYPNAME(IAT)
      ENDIF
      DO 130 ICBTP=1,NCBTP
        CALL EGETWI(OUTSTR,K,IBTN,0,0,'-','crrspnd bld typ index',IER)
        IATYPNDX(IAT,IBTN)=1
        IF(IDEBUG.EQ.1)THEN
          IF(IATYPNDX(IAT,IBTN).EQ.1)WRITE(6,*)' btypes ',BTYPNAME(IBTN)
        ENDIF
 130  CONTINUE
      IAT=IAT+1
      GOTO 17
 18   CALL ERPFREE(IUF,ISTAT)
      RETURN
      END

C ********************* sbmedit
C sbmedit define high level data associated with UK NCM methodology.
      subroutine sbmedit
#include "building.h"
#include "sbem.h"
      common/OUTIN/IUOUT,IUIN
      common/pophelp/h(60)
      common/C1/NCOMP,NCON
      COMMON/precz/zname(MCOM),zdesc(MCOM)

      dimension IVALSS(29)       ! array to get epicks answer from
      DIMENSION ITEMS(33),ITEMX(8),KSYS(MHT),ITEMH(MCOM+4),PRSSN(MPT)
      DIMENSION KSTR(8),ITHND(MPT),HSYSNM(MCOM+1),JSTR(8)
      character outs*124,outs248*248,h*72
      character HEAD*25,ITEMS*50,D64*64,ITEMX*50,ITEMH*70,PRSSN*60
      CHARACTER HSYSNM*70,zname*12,zdesc*64,TMPSTR*64

C place (temporary?) call to read sbem db
      call sbempr(ier)

C Check if sbem project specific file exists if yes then read in details
C from it.
      IF(ISBEM.EQ.1)THEN
        CALL RSBEM
      ELSE

C Ask about building, owner and certifier detail.
        pjname=' not yet defined'
        baddress=' not yet defined'
        bcity=' not yet defined'
        buildingPC =' not yet defined'
        ownername=' not yet defined'
        ownertel=' not yet defined'
        owneraddr=' not yet defined'
        ownercity=' not yet defined'
        ownerPC=' not yet defined'
        certifname=' not yet defined'
        certTel=' not yet defined'
        certaddress=' not yet defined'
        certcity=' not yet defined'
        certPC=' not yet defined'
        IBSS=1
        IRGG=1
        ISTG=1
        BINF50=10.
      ENDIF

C Generate help text
 222  H(1) = ' Help text for UK NCM calculations still to'
      H(2) = ' be generated :)'

C Make high level menu asking for different parts of UK NCM 
      ITEMX(1) = 'Enter building, owner and certifier information'
      ITEMX(2) = 'Enter building type / regulations information '
      ITEMX(3) = 'Enter HVAC and systems information'
      ITEMX(4) = 'Enter Domestic Hot Water systems'
      ITEMX(5) = 'Link HVAC systems and zones '
      ITEMX(6) = 'Link DHW systems and zones '
      ITEMX(7) = 'Link activities and zones '
      ITEMX(8) = 'Display information '
      IX=1
      CALL EPICKS(IX,KSTR,' ','Define UK NCM data',
     &  50,8,ITEMX,'Define UK NCM data',IER,2)
      IF(KSTR(1).EQ.1)THEN

C Enter building, owner and vertifier information      
  15    CALL EDISP(IUOUT,
     &  'Enter/Confirm building, owner and certifier details')
        INO=-1
        ITEMS(1)  = 'a Project name: '
        write(ITEMS(2),'(2A)') '  ',pjname
        ITEMS(3)  = 'b Building address: '
        write(ITEMS(4),'(2A)') '  ',baddress
        ITEMS(5)  = 'c Building city: '
        write(ITEMS(6),'(2A)') '  ',bcity      
        ITEMS(7)  = 'd Building Postcode: '
        write(ITEMS(8),'(2A)') '  ',buildingPC 
        ITEMS(9)  = 'e Building owner name: '
        write(ITEMS(10),'(2A)') '  ',ownername
        ITEMS(11) = 'f Building owner telephone: '
        write(ITEMS(12),'(2A)') '  ',ownertel
        ITEMS(13) = 'g Building owner address: '
        write(ITEMS(14),'(2A)') '  ',owneraddr
        ITEMS(15) = 'h Building owner city: '
        write(ITEMS(16),'(2A)') '  ',ownercity
        ITEMS(17) = 'i Building owner Postcode: '
        write(ITEMS(18),'(2A)') '  ',ownerPC
        ITEMS(19) = 'j Certifier name: '
        write(ITEMS(20),'(2A)') '  ',certifname
        ITEMS(21) = 'k Certifier telephone: '
        write(ITEMS(22),'(2A)') '  ',certTel
        ITEMS(23) = 'l Certifier address: '
        write(ITEMS(24),'(2A)') '  ',certaddress
        ITEMS(25) = 'm Certifier city: '
        write(ITEMS(26),'(2A)') '  ',certcity
        ITEMS(27) = 'n Certifier postcode: '
        write(ITEMS(28),'(2A)') '  ',certPC
        ITEMS(29) = '# Enter all details            '
        ITEMS(30) = '  _____________________________'
        ITEMS(31) = '* list project details         '
        ITEMS(32) = '? help                         '
        ITEMS(33) = '- exit this menu               '
        nitms=33
        WRITE(HEAD,'(2A)')'Project details for BRUKL'

C Help text for this menu.
        H(1) ='This menu allows the definition of '
        H(2) ='project details. These will be used by '
        H(3) ='BRUKL for reporting purposes '

C Menu for zone geometry.
        CALL EMENU(HEAD,ITEMS,nitms,INO)
        D64='<this item has not been described>'
        IF(INO.EQ.nitms-1)THEN

C Produce help text for the menu.
          CALL PHELPD(HEAD,3,'-',0,0,IER)
        ELSEIF(INO.EQ.1)THEN
  
C Building/owner/certifier data description
          H(1)='Input a succinct description of the project name here.'
          CALL EASKS(pjname,'project name ?','(<64 characters)',
     &           64,D64,'project name',IER,1)
          call tstamp('>','PRJ: edited project description')
        ELSEIF(INO.EQ.3)THEN
  
C Building/owner/certifier data description
          H(1)='Input a succinct description of building address here'
          CALL EASKS(baddress,'building address ?','(<64 characters)',
     &           64,D64,'build address',IER,1)
          call tstamp('>','PRJ: edited project description')
        ELSEIF(INO.EQ.5)THEN
  
C Building/owner/certifier data description
          H(1)='Input a succinct description of the building city here'
          CALL EASKS(bcity,'building city ?','(<64 characters)',
     &           64,D64,'city building',IER,1)
          call tstamp('>','PRJ: edited project description')
        ELSEIF(INO.EQ.7)THEN
  
C Building/owner/certifier data description
          H(1)='Input for: building post code'
          CALL EASKS(buildingPC,'building post code ?',
     &    '(<64 characters)',64,D64,'building PC',IER,1)
          call tstamp('>','PRJ: edited project description')
        ELSEIF(INO.EQ.9)THEN

C Building/owner/certifier data description
          H(1)='Input a succinct description of the owner name here'
          CALL EASKS(ownername,'Name of owner ?','(<64 characters)',
     &           64,D64,'owner name',IER,1)
          call tstamp('>','PRJ: edited project description')
        ELSEIF(INO.EQ.11)THEN
  
C Building/owner/certifier data description
          H(1)='telephone number of the owner'
          CALL EASKS(ownertel,'Telephone of owner ?','(<64 characters)',
     &           64,D64,'owner tel',IER,1)
          call tstamp('>','PRJ: edited project description')
        ELSEIF(INO.EQ.13)THEN
  
C Building/owner/certifier data description
          H(1)='Address of the owner'
          CALL EASKS(owneraddr,'Address of owner ?','(<64 characters)',
     &           64,D64,'owner address',IER,1)
          call tstamp('>','PRJ: edited project description')
        ELSEIF(INO.EQ.15)THEN
  
C Building/owner/certifier data description
          H(1)='City (for owner)'
          CALL EASKS(ownercity,'City of owner ?','(<64 characters)',
     &           64,D64,'owner city',IER,1)
          call tstamp('>','PRJ: edited project description')
        ELSEIF(INO.EQ.17)THEN
  
C Building/owner/certifier data description
          H(1)='Post code (for owner)'
          CALL EASKS(ownerPC,'Post code for owner ?','(<64 characters)',
     &           64,D64,'owner post code',IER,1)
          call tstamp('>','PRJ: edited project description')
        ELSEIF(INO.EQ.19)THEN
  
C Building/owner/certifier data description
          H(1)='cetrifier name'
          CALL EASKS(certifname,'Name of certifier ?','(<64 characters)'
     &           ,64,D64,'certifier name',IER,1)
          call tstamp('>','PRJ: edited project description')
        ELSEIF(INO.EQ.21)THEN 
      
C Building/owner/certifier data description
          H(1)='cetrifier telephone'
          CALL EASKS(certTel,'Teleph. of certifier ?','(<64 characters)'
     &           ,64,D64,'certifier tel',IER,1)
          call tstamp('>','PRJ: edited project description')
        ELSEIF(INO.EQ.23)THEN 
      
C Building/owner/certifier data description
          H(1)='certifier address'
          CALL EASKS(certaddress,'Address of certifier ?',
     &    '(<64 characters)',64,D64,'certifier address',IER,1)
          call tstamp('>','PRJ: edited project description')
        ELSEIF(INO.EQ.25)THEN 
      
C Building/owner/certifier data description
          H(1)='cetrifier city'
          CALL EASKS(certcity,'City of certifier ?','(<64 characters)',
     &           64,D64,'certifier city',IER,1)
          call tstamp('>','PRJ: edited project description')
        ELSEIF(INO.EQ.27)THEN 
      
C Building/owner/certifier data description
          H(1)='cetrifier post code'
          CALL EASKS(certPC,'Post code of certifier ?',
     &    '(<64 characters)',64,D64,'certifier Post code',IER,1)
          call tstamp('>','PRJ: edited project description')
        ELSEIF(INO.EQ.29)THEN 

C Enter all information
          H(1)='enter required information'
          CALL EASKS(pjname,'project name ?','(<64 characters)',
     &         64,D64,'project name',IER,1)
          CALL EASKS(baddress,'building address ?','(<64 characters)',
     &         64,D64,'build address',IER,1)
          CALL EASKS(bcity,'building city ?','(<64 characters)',
     &         64,D64,'city building',IER,1)
          CALL EASKS(buildingPC,'building post code ?',
     &         '(<64 characters)',64,D64,'building PC',IER,1)
          CALL EASKS(ownername,'Name of owner ?','(<64 characters)',
     &         64,D64,'owner name',IER,1)
          CALL EASKS(ownertel,'Telephone of owner ?','(<64 characters)',
     &         64,D64,'owner tel',IER,1)
          CALL EASKS(owneraddr,'Address of owner ?','(<64 characters)',
     &         64,D64,'owner address',IER,1)
          CALL EASKS(ownercity,'City of owner ?','(<64 characters)',
     &         64,D64,'owner city',IER,1)
          CALL EASKS(ownerPC,'Post code for owner ?','(<64 characters)',
     &         64,D64,'owner post code',IER,1)
          CALL EASKS(certifname,'Name of certifier ?','(<64 characters)'
     &         ,64,D64,'certifier name',IER,1)
          CALL EASKS(certTel,'Teleph. of certifier ?','(<64 characters)'
     &         ,64,D64,'certifier tel',IER,1)
          CALL EASKS(certaddress,'Address of certifier ?',
     &         '(<64 characters)',64,D64,'certifier address',IER,1)
          CALL EASKS(certcity,'City of certifier ?','(<64 characters)',
     &         64,D64,'certifier city',IER,1)
          CALL EASKS(certPC,'Post code of certifier ?',
     &         '(<64 characters)',64,D64,'certifier Post code',IER,1)
        ELSEIF(INO.EQ.31)THEN 

C Display information
          write(outs,'(2a)')'Project name: ',pjname
          call edisp(iuout,outs)
          write(outs,'(2a)')'Building address: ',baddress
          call edisp(iuout,outs)
          write(outs,'(2a)')'Building city: ',bcity  
          call edisp(iuout,outs)
          write(outs,'(2a)')'Building Postcode: ',buildingPC  
          call edisp(iuout,outs)    
          write(outs,'(2a)')'Building owner name: ',ownername
          call edisp(iuout,outs)   
          write(outs,'(2a)')'Building owner telephone: ',ownertel
          call edisp(iuout,outs)   
          write(outs,'(2a)')'Building owner address: ',owneraddr 
          call edisp(iuout,outs) 
          write(outs,'(2a)')'Building owner city: ',ownercity 
          call edisp(iuout,outs) 
          write(outs,'(2a)')'Building owner Postcode: ',ownerPC
          call edisp(iuout,outs)     
          write(outs,'(2a)')'Certifier name: ',certifname 
          call edisp(iuout,outs)     
          write(outs,'(2a)')'Certifier telephone: ',certTel 
          call edisp(iuout,outs)     
          write(outs,'(2a)')'Certifier address: ',certaddress   
          call edisp(iuout,outs)   
          write(outs,'(2a)')'Certifier city: ',certcity  
          call edisp(iuout,outs)    
          write(outs,'(2a)')'Certifier postcode: ',certPC  
          call edisp(iuout,outs)          
        ENDIF
        IF(INO.NE.nitms)GOTO 15
      ELSEIF(KSTR(1).EQ.2)THEN

C Enter building services strategy 
        IX=1
        JSTR(1)=IBSS
 901    CALL EPICKS(IX,JSTR,' ','Define building services strategy',
     &  40,MSS,BLDSS,'Define building services strategy',IER,2)
        IF(JSTR(1).EQ.0)GOTO 901
        IF(JSTR(1).LE.3.AND.JSTR(1).GE.1)IBSS=JSTR(1)        

C Enter building regulations to be used
        IX=1
        JSTR(1)=IRGG
 902    CALL EPICKS(IX,JSTR,' ','Define building regulations to use',
     &  50,MREG,BLDREG,'Define building services strategy',IER,2)
        IF(JSTR(1).EQ.0)GOTO 902
        IF(JSTR(1).LE.4.AND.JSTR(1).GE.1)IRGG=JSTR(1)        

C Enter bulding design stage
        IX=1
        JSTR(1)=ISTG
 903    CALL EPICKS(IX,JSTR,' ','Define building regulations to use',
     &  12,MSTG,BLDSTG,'Define building services strategy',IER,2)
        IF(JSTR(1).EQ.0)GOTO 903
        IF(JSTR(1).LE.2.AND.JSTR(1).GE.1)ISTG=JSTR(1)        

C Enter Scottish building types if Scottish regs to be used
        IF(IRGG.EQ.2)THEN
          IX=1
          JSTR(1)=ISBT
 904      CALL EPICKS(IX,JSTR,' ','Define building type',
     &    40,MSBT,SBTYP,'Define building type',IER,2)
          IF(JSTR(1).EQ.0)GOTO 904
          IF(JSTR(1).LE.2.AND.JSTR(1).GE.1)ISBT=JSTR(1)        
          CALL EASKABC('Is the building built/designed following',
     &    'guidance in Accredited Construction Details (Scotland)',
     &    'yes','no','Do not know',IW,2)
          IF(IW.EQ.1)THEN
            SBREF='YES'
          ELSE
            SBREF='NO '
          ENDIF
        ELSE
          ISBT=0
        ENDIF

C Enter building permeability
        CALL EASKR(BINF50,
     &    'Enter building permeability at 50Pa in m3/m2.hour'
     &    ,' ',0.,'F',0.,'-',10.,'heat gen eff ',IER,2)
        CALL EASKABC(
     &  'Is a compliance check to be performed on the building',
     &  'regarding air permeability value?',
     &  'yes','no','Do not know',IW,2)
        IF(IW.EQ.2)THEN
          APCHK='NO '
        ELSE
          APCHK='YES'
        ENDIF

C Enter building use type
        ibtyp=IBTYPNDX(ibusertyp)

C Establish glazing fractions based on type of building
        IF(IBTYP.EQ.1)THEN
          ROOFFR=0.2
          WALLFR=0.3
        ELSEIF(IBTYP.EQ.2)THEN
          ROOFFR=0.2
          WALLFR=0.4
        ELSEIF(IBTYP.EQ.3)THEN
          ROOFFR=0.2
          WALLFR=0.15
        ENDIF
        if(ibusertyp.eq.0)then
          write(outs,'(2a)') 'The current building use is undefined ',
     &    'and the NCM guide table 4 is also undefined.'
          call edisp(iuout,outs)
        else
          write(outs,'(2a)') 'The current building use is ',
     &    BTYPNAME(ibusertyp)
          call edisp(iuout,outs)
          if(ibtyp.eq.1)then
            write(outs248,'(2a,f6.2,a,f6.2,a)')
     &      'The NCM guide table 4 reference is a residenatial ',
     &      'building with roof glazing fraction ',ROOFFR,
     &      ' and a wall glazing fraction ',WALLFR,'.'
          elseif(ibtyp.eq.2)then
            write(outs248,'(2a,f6.2,a,f6.2,a)')
     &      'The NCM guide table 4 reference is an office, shop or ',
     &      'place of assembly with roof glazing fraction ',ROOFFR,
     &      ' and a wall glazing fraction ',WALLFR,'.'
          elseif(ibtyp.eq.3)then
            write(outs248,'(2a,f6.2,a,f6.2,a)')
     &      'The NCM guide table 4 reference is an industrial or ',
     &      'storage building with roof glazing fraction ',ROOFFR,
     &      ' and a wall glazing fraction ',WALLFR,'.'
          endif
          call edisp248(iuout,outs248,80)
        endif
        h(1)='For the UK National Calculation Method you are required'
        h(2)='to select one of these building types. '
        h(3)=' '
        h(4)='Depending on the type of building the notional model will'
        h(5)='have different assumptions about glazing areas. The '
        h(6)='building type will also help in the selection of room '
        h(7)='use patterns. '
        IX=1
 905    CALL EPICKS(IX,IVALSS,' ','Building type:',
     &  42,29,BTYPNAME,'building types',IER,7)
        IF(IVALSS(1).EQ.0)GOTO 905
        if(IVALSS(1).gt.0)then
          ibusertyp=IVALSS(1)
          ibtyp = IBTYPNDX(ibusertyp)

C Establish glazing fractions based on type of building
          IF(IBTYP.EQ.1)THEN
            ROOFFR=0.2
            WALLFR=0.3
          ELSEIF(IBTYP.EQ.2)THEN
            ROOFFR=0.2
            WALLFR=0.4
          ELSEIF(IBTYP.EQ.3)THEN
            ROOFFR=0.2
            WALLFR=0.15
          ENDIF
        endif
      
C After setting up echo back what was selected.
        if(ibusertyp.eq.0)then
          write(outs,'(2a)') 'The current building use is undefined ',
     &    'and the NCM guide table 4 is also undefined.'
          call edisp(iuout,outs)
        else
          write(outs,'(2a)') 'The current building use is ',
     &    BTYPNAME(ibusertyp)
          call edisp(iuout,outs)
          if(ibtyp.eq.1)then
            write(outs248,'(2a,f6.2,a,f6.2,a)')
     &      'The NCM guide table 4 reference is a residenatial ',
     &      'building with roof glazing fraction ',ROOFFR,
     &      ' and a wall glazing fraction ',WALLFR,'.'
          elseif(ibtyp.eq.2)then
            write(outs248,'(2a,f6.2,a,f6.2,a)')
     &      'The NCM guide table 4 reference is an office, shop or ',
     &      'place of assembly with roof glazing fraction ',ROOFFR,
     &      ' and a wall glazing fraction ',WALLFR,'.'
          elseif(ibtyp.eq.3)then
            write(outs248,'(2a,f6.2,a,f6.2,a)')
     &      'The NCM guide table 4 reference is an industrial or ',
     &      'storage building with roof glazing fraction ',ROOFFR,
     &      ' and a wall glazing fraction ',WALLFR,'.'
          endif
          call edisp248(iuout,outs248,80)
        endif
      ELSEIF(KSTR(1).EQ.3)THEN

C Edit HVAC and system information
 205    IHO=-2
        H(1)='Help text for adding UK NCM HVAC systems'
        H(2)='This message still needs to be generated'
        DO 201 ICMSYS=1,NCMSYS
          WRITE(ITEMH(ICMSYS),'(A,1X,A)')CHAR(96+ICMSYS),
     &          HVACNAME(ICMSYS)
 201    CONTINUE
        ITEMH(NCMSYS+1)=' ----------------------------------------'
        ITEMH(NCMSYS+2)='+ add/delete'
        ITEMH(NCMSYS+3)='? help'
        ITEMH(NCMSYS+4)='- exit'
        NITEMH=NCMSYS+4
        CALL EMENU('HVAC systems defined',ITEMH,nitEmH,IHO)
        IF(IHO.EQ.NCMSYS+3)THEN
          CALL PHELPD('NCM HVAC help',2,'-',0,0,IER)
        ELSEIF(IHO.EQ.NCMSYS+2)THEN
          JC=0
          CALL EASKABC('Choose option',' ',
     &         'add','delete','cancel',JC,2)
          IF(JC.EQ.1)THEN

C Add HVAC and system information
            NCMSYS=NCMSYS+1
            H(1) = 'Choose the type of HVAC environment control system'
            H(2) = 'that satisfies / comes closest to your requirements'
            HVACNAME(NCMSYS)=' default HVAC system '
            CALL EASKS(HVACNAME(NCMSYS),'HVAC system name',
     &      '(<30 characters)',30,D64,'HVAC sys name',IER,1)
            IY=1
            KSYS(1)=0
 906        CALL EPICKS(IY,KSYS,' ','Select system type',
     &      70,MHT,HSYSNAME,'system types',IER,2)
            IF(KSYS(1).EQ.0)GOTO 906
            IHSYS=KSYS(1)
            IF(IHSYS.GE.1)THEN
              INCMSYS(NCMSYS)=IHSYS
            ENDIF

C Add corresponding heat generator, efficiency and fuel type
            IHCG=1
            DO 220 IPT=1,MPT
              IF(ISYSAPP(IPT,IHSYS).EQ.1)THEN
                PRSSN(IHCG)=SYSNAME(IPT)
                HGEF(IHCG)=SYSEFF(IPT)
                ITHND(IHCG)=IPT
                IHCG=IHCG+1
              ENDIF
 220        CONTINUE
 330        CALL EDISP(IUOUT,'Select heat generator')
            IY=1
            KSYS(1)=0
            CALL EPICKS(IY,KSYS,' ','Select heat generator',
     &      60,IHCG-1,PRSSN,'system types',IER,2)
            IF(KSYS(1).EQ.0)GOTO 330
            IHGEF(NCMSYS)=ITHND(KSYS(1))
            CALL EASKR(HGEF(NCMSYS),
     &      'Enter heat generator efficiency/COP or accept default'
     &      ,' ',0.,'F',0.,'-',10.,'heat gen eff ',IER,2)
 331        IY=1
            KSYS(1)=0
            CALL EDISP(IUOUT,'Select fuel type')
            CALL EPICKS(IY,KSYS,' ','Select fuel type',
     &      42,MFT,FUELNAME,'Fuel Types',IER,2)
            IF(KSYS(1).EQ.0)GOTO 331
            IFTYP(NCMSYS)=KSYS(1)

C Add corresponding cool generator if applicable
            IF(IBRUKLC(1,IHSYS).NE.-1111)THEN
              CALL EASKABC(' Choose cooling system',' ',SYSNAME(MPT-2),
     &        SYSNAME(MPT-1),SYSNAME(MPT),ICGEF(NCMSYS),2)
              CALL EASKABCD('Enter power rating of system (Watts)',' ',
     &        '0-100','101-500','501-750','751-3500',IADC,1)
              CALL EASKAB('Is this system in the ECA list',
     &        ' ','yes','no',IECA,2)
              ICS=ICGEF(NCMSYS)
              ICNDX=8*(ICS-1)+2*(IADC-1)+IECA
              CGEF(NCMSYS)=SYSEFFC(ICNDX,IHSYS)
              CALL EASKR(CGEF(NCMSYS),
     &        'Enter cool generator efficiency/COP or accept default'
     &        ,' ',0.,'F',0.,'-',10.,'cool gen eff ',IER,2)
            ELSE
              CGEF(NCMSYS)=0.0
              ICGEF(NCMSYS)=0
            ENDIF

C Ask for ventilation specific fan power SFC if applicable
            IF(IBRUKLF(IHSYS).NE.-1111)THEN
              SFPHS(NCMSYS)=SFPDEF
              CALL EASKR(SFPHS(NCMSYS),
     &        'Enter ventilation system specifc fan power SFC or',
     &        'accept default (Watts/liter/sec)',
     &        0.,'F',0.,'-',50.,'heat gen eff ',IER,2)
            ELSE
              SFPHS(NCMSYS)=0.0
            ENDIF
          ELSEIF(JC.EQ.2)THEN

C Delete HVAC and system information
            H(1) = 'Choose HVAC system to delete or simply exit without'
            H(2) = 'making any choice to retain all defined systems'
 907        IZ=1
            KSYS(1)=0
            CALL EPICKS(IZ,KSYS,' ','HVAC system to delete',
     &      70,NCMSYS,ITEMH,'HVAC system to delete',IER,2)
            IF(KSYS(1).EQ.0)GOTO 907
            IF(KSYS(1).GE.1)THEN
              DO 203 ICMSYS=KSYS(1),NCMSYS
                INCMSYS(ICMSYS)  = INCMSYS(ICMSYS+1)
                IHGEF(ICMSYS)    = IHGEF(ICMSYS+1)
                HGEF(ICMSYS)     = HGEF(ICMSYS+1)
                IFTYP(ICMSYS)    = IFTYP(ICMSYS+1)
                ICGEF(ICMSYS)    = ICGEF(ICMSYS+1)
                CGEF(ICMSYS)     = CGEF(ICMSYS+1)
                SFPHS(ICMSYS)    = SFPHS(ICMSYS+1)
                HVACNAME(ICMSYS) = HVACNAME(ICMSYS+1)
 203          CONTINUE
              NCMSYS=NCMSYS-1
            ENDIF
          ELSEIF(JC.EQ.3)THEN
            CONTINUE
          ENDIF
        ELSEIF(IHO.LE.NCMSYS+3.AND.IHO.GE.1)THEN

C Edit HVAC and system information
          H(1) = 'Choose HVAC system to relace current system'
          H(2) = '(see text feed back area) or exit to retain current'
          CALL EASKS(HVACNAME(IHO),'HVAC system name',
     &      '(<30 characters)',30,D64,'HVAC sys name',IER,1)
          IY=1
          KSYS(1)=0
 908      WRITE(OUTS,'(2A)')'Current HVAC system is ',
     &    HSYSNAME(INCMSYS(IHO))
          call edisp(iuout,OUTS)
          call edisp(iuout,
     &    'Choose HVAC system to replace it or exit to retain current')
          CALL EPICKS(IY,KSYS,' ','Select system type',
     &    70,MHT,HSYSNAME,'system types',IER,2)
          IF(KSYS(1).EQ.0)GOTO 908
          IHSYS=KSYS(1)
          IF(IHSYS.GE.1)THEN
            INCMSYS(IHO)=IHSYS
          ENDIF

C Edit corresponding heat generator, efficiency and fuel type
          IHCG=1
          DO 229 IPT=1,MPT
            IF(ISYSAPP(IPT,IHSYS).EQ.1)THEN
              PRSSN(IHCG)=SYSNAME(IPT)
              ITHND(IHCG)=IPT
              IHCG=IHCG+1
            ENDIF
 229      CONTINUE
 339      CALL EDISP(IUOUT,'Select heat generator')
          WRITE(OUTS,'(2A)')'Current heat generator is'
     &    ,SYSNAME(IHGEF(IHO))
          CALL EDISP(IUOUT,OUTS)
          IY=1
          KSYS(1)=0
          CALL EPICKS(IY,KSYS,' ','Select heat generator',
*     &    60,IHCG,PRSSN,'system types',IER,2)
     &    60,IHCG-1,PRSSN,'system types',IER,2)
          IF(KSYS(1).EQ.0)GOTO 339
          IHGEF(IHO)=ITHND(KSYS(1))
          CALL EASKR(HGEF(IHO),
     &    'Edit heat/cool generator efficiency/COP'
     &    ,' ',0.,'F',0.,'-',10.,'heat gen eff ',IER,2)
 341      IY=1
          KSYS(1)=0
          CALL EDISP(IUOUT,'Select fuel type')
          CALL EPICKS(IY,KSYS,' ','Select fuel type',
     &    42,MFT,FUELNAME,'Fuel Types',IER,2)
          IF(KSYS(1).EQ.0)GOTO 341
          IFTYP(IHO)=KSYS(1)

C Edit corresponding cool generator if applicable
          IF(IBRUKLC(1,IHSYS).NE.-1111)THEN
            CALL EASKABC(' Choose cooling system',' ',SYSNAME(MPT-2),
     &      SYSNAME(MPT-1),SYSNAME(MPT),ICGEF(IHO),2)
            CALL EASKABCD('Enter power rating of system (Watts)',' ',
     &      '0-100','101-500','501-750','751-3500',IADC,1)
            CALL EASKAB('Is this system in the ECA list',
     &      ' ','yes','no',IECA,2)
            ICS=ICGEF(IHO)
            ICNDX=8*(ICS-1)+2*(IADC-1)+IECA
            CGEF(IHO)=SYSEFFC(ICNDX,IHSYS)
            CALL EASKR(CGEF(IHO),
     &      'Edit cool generator efficiency/COP'
     &      ,' ',0.,'F',0.,'-',10.,'cool gen eff ',IER,2)
          ELSE
            CGEF(IHO)=0.0
            ICGEF(IHO)=0
          ENDIF

C Edit ventilation specific fan power SFC if applicable
          IF(IBRUKLF(IHSYS).NE.-1111)THEN
            SFPHS(IHO)=SFPDEF
            CALL EASKR(SFPHS(IHO),
     &      'Enter ventilation system specifc fan power SFC or',
     &      'accept default (Watts/liter/sec)',
     &      0.,'F',0.,'-',50.,'heat gen eff ',IER,2)
          ELSE
            SFPHS(IHO)=0.0
          ENDIF
        ENDIF
        IF(IHO.NE.NITEMH)GOTO 205
      ELSEIF(KSTR(1).EQ.4)THEN

C Define DHW systems
 207    IDO=-1
        DO 206 IDHWSYS=1,NDHWSYS
          WRITE(ITEMH(IDHWSYS),'(A,1X,A)')CHAR(96+IDHWSYS),
     &          DHWNAME(IDHWSYS)
 206    CONTINUE
        ITEMH(NDHWSYS+1)=' ----------------------------------------'
        ITEMH(NDHWSYS+2)='+ add/delete'
        ITEMH(NDHWSYS+3)='? help'
        ITEMH(NDHWSYS+4)='- exit'
        NITEMH=NDHWSYS+4
        CALL EMENU('DHW systems defined',ITEMH,nitEmH,IDO)
        IF(IDO.EQ.NITEMH-1)THEN
          CALL PHELPD('DHW help not defined (yet)',2,'-',0,0,IER)
        ELSEIF(IDO.EQ.NITEMH-2)THEN
          JC=0
          CALL EASKABC('Choose option',' ',
     &         'add','delete','cancel',JC,2)
          IF(JC.EQ.1)THEN
            NDHWSYS=NDHWSYS+1
            DHWNAME(NDHWSYS)=' default DHW generator '
            CALL EASKS(DHWNAME(NDHWSYS),'DHW system name',
     &      '(<30 characters)',30,D64,'DHW sys name',IER,1)
            IY=1
            KSYS(1)=0
 909        CALL EPICKS(IY,KSYS,' ','Select DHW system type',
     &      25,MDW,DHWGEN,'system types',IER,2)
            IF(KSYS(1).EQ.0)GOTO 909
            IDHWS(NDHWSYS)=KSYS(1)
            CALL EASKABC('Was the DHW generator built before 1998',
     &      ' ','yes','no','Do not know',IW,2)
            IF(IW.EQ.1)THEN
              HWEF(NDHWSYS)=DHWEFF(MDW,1)
            ELSE
              HWEF(NDHWSYS)=DHWEFF(MDW,2)
            ENDIF
            CALL EASKR(HWEF(NDHWSYS),
     &      'Enter system efficiency or accept default'
     &      ,' ',0.,'F',0.,'-',10.,'DHW gen eff ',IER,2)

C Make menu structure for DHW fuel types (This information is hardcoded
C and not taken from database because it is perceived that fuel types
C for DHW systems will not change over time)
            ITEMS(1)='natural gas                      '
            ITEMS(2)='LPG                              '
            ITEMS(3)='Biogas                           '
            ITEMS(4)='Oil                              '
            ITEMS(5)='Coal                             '
            ITEMS(6)='Biomass                          '
            ITEMS(7)='Waste heat                       '
            ITEMS(8)='Grid supplied electricity        '
            ITEMS(9)='                                 '
            IDA=1
            JSTR(1)=0
 910        CALL EPICKS(IDA,JSTR,
     &      ' ','Choose fuel type for DHW generator',
     &      30,8,ITEMS,'Choose fuel type for DHW generator',IER,2)
            IF(JSTR(1).EQ.0)GOTO 910
            IDHFL(NDHWSYS)=JSTR(1)
          ELSEIF(JC.EQ.2)THEN
            IDA=1
            JSTR(1)=0
 911        CALL EPICKS(IDA,JSTR,
     &      ' ','Choose DHW generator to delete',
     &      30,8,DHWNAME,'Choose DHW generator to delete',IER,2)
            IF(JSTR(1).EQ.0)GOTO 911
            IF(JSTR(1).NE.0)THEN
              NDHWSYS=NDHWSYS-1
              DO 211 IDHWSYS=JSTR(1),NDHWSYS
                DHWNAME(IDHWSYS)=DHWNAME(IDHWSYS+1)
                HWEF(IDHWSYS)=HWEF(IDHWSYS+1)
                IDHWS(IDHWSYS)=IDHWS(IDHWSYS+1)
                IDHFL(IDHWSYS)=IDHFL(IDHWSYS+1)
 211          CONTINUE
            ENDIF
          ENDIF
        ELSEIF(IDO.GE.1.AND.IDO.LE.NDHWSYS)THEN
          IDHWSYS=IDO
          CALL EASKS(DHWNAME(IDHWSYS),'DHW system name',
     &    '(<30 characters)',30,D64,'DHW sys name',IER,1)
          IY=1
          KSYS(1)=IDHWS(IDHWSYS)
 912      CALL EPICKS(IY,KSYS,' ','Select DHW system type',
     &    25,MDW,DHWGEN,'system types',IER,2)
          IF(KSYS(1).EQ.0)GOTO 912
          IDHWS(IDHWSYS)=KSYS(1)
          CALL EASKABC('Was the DHW generator built before 1998',
     &    ' ','yes','no','Do not know',IW,2)
          IF(IW.EQ.1)THEN
            HWEF(IDHWSYS)=DHWEFF(MDW,1)
          ELSE
            HWEF(IDHWSYS)=DHWEFF(MDW,2)
          ENDIF
          CALL EASKR(HWEF(IDHWSYS),
     &    'Enter system efficiency or accept previous'
     &    ,' ',0.,'F',0.,'-',10.,'DHW gen eff ',IER,2)

C Make menu structure for DHW fuel types (This information is hardcoded
C and not taken from database because it is perceived that fuel types
C for DHW systems will not change over time)
          ITEMS(1)='natural gas                   '
          ITEMS(2)='LPG                           '
          ITEMS(3)='Biogas                        '
          ITEMS(4)='Oil                           '
          ITEMS(5)='Coal                          '
          ITEMS(6)='Biomass                       '
          ITEMS(7)='Waste heat                    '
          ITEMS(8)='Grid supplied electricity     '
          ITEMS(9)='                              '
          IDA=1
          JSTR(1)=IDHFL(IDHWSYS)
 913      CALL EPICKS(IDA,JSTR,
     &    ' ','Choose fuel type for DHW generator',
     &    30,8,ITEMS,'Choose fuel type for DHW generator',IER,2)
          IF(JSTR(1).EQ.0)GOTO 913
          IDHFL(IDHWSYS)=JSTR(1)
        ENDIF
        IF(IDO.NE.NITEMH)GOTO 207
      ELSEIF(KSTR(1).EQ.5)THEN

C Link HVAC and zones
C If no zones specified in model advise user and go back to menu
        IF(NCOMP.EQ.0)THEN
          CALL EDISP(IUOUT,'No zones have been defined in the model')
          CALL EDISP(IUOUT,'Define zones before linking with systems')
        ELSE
          HSYSNM(1)='Zone not serviced by any HVAC system'
          DO 401 ISNM=2,NCMSYS+1
            HSYSNM(ISNM)=HVACNAME(ISNM-1)
 401      CONTINUE
          DO 102 IZ=1,NCOMP
            IF(IHLZ(IZ).EQ.0)THEN
              TMPSTR=' No HVAC system for this zone!'
            ELSE
              WRITE(TMPSTR,'(2A)')' ',HVACNAME(IZ)
            ENDIF
            WRITE(OUTS,'(3A,I3,A)')
     &       'Zone ',ZNAME(IZ),' is linked to system number ',
     &       IHLZ(IZ),TMPSTR
            CALL EDISP(IUOUT,OUTS)
 102      CONTINUE
          DO 402 IZ=1,NCOMP
            IA=1
            WRITE(OUTS,'(2A)')'Choose HVAC system for zone ',
     &      zname(IZ)(1:lnblnk(zname(IZ)))
            JSTR(1)=0
 914        CALL EPICKS(IA,JSTR,' ','Link zones to HVAC systems',
     &      70,NCMSYS+1,HSYSNM,OUTS(1:40),IER,2)
            IF(JSTR(1).EQ.0)GOTO 914
            IHLZ(IZ)=JSTR(1)-1
 402      CONTINUE
        ENDIF
      ELSEIF(KSTR(1).EQ.6)THEN

C Link DHW systems and zones
        IF(NCOMP.EQ.0)THEN
          CALL EDISP(IUOUT,'No zones have been defined in the model')
          CALL EDISP(IUOUT,'Define zones before linking with systems')
        ELSE
          HSYSNM(1)='Zone not serviced by any system'
          IF(NCMSYS.GE.1)THEN
            HSYSNM(2)='Zone serviced by HVAC system not DHW system'
          ELSE
            HSYSNM(2)='Zone serviced by HVAC system not DHW system N/A'
          ENDIF
          DO 403 ISNM=3,NDHWSYS+2
            HSYSNM(ISNM)=DHWNAME(ISNM-2)
 403      CONTINUE
          DO 109 IZ=1,NCOMP
            IF(IDHWLZ(IZ).EQ.0)THEN
              TMPSTR=' No DHW systems for this zone! '
            ELSEIF(IDHWLZ(IZ).GT.0)THEN
              WRITE(TMPSTR,'(2A)')' ',DHWNAME(IDHWLZ(IZ))
            ELSEIF(IDHWLZ(IZ).LT.0)THEN
              WRITE(TMPSTR,'(2A)')' (HVAC system) ',
     &        HVACNAME(-IDHWLZ(IZ))
            ENDIF
            WRITE(OUTS,'(3A,I3,A)')
     &       'Zone ',ZNAME(IZ),' is linked to system number ',
     &       ABS(IDHWLZ(IZ)),TMPSTR
            CALL EDISP(IUOUT,OUTS)
 109      CONTINUE
          DO 404 IZ=1,NCOMP
            IA=1
            WRITE(OUTS,'(2A)')'Choose DHW system for zone ',
     &      zname(IZ)(1:lnblnk(zname(IZ)))
            JSTR(1)=0
 915        CALL EPICKS(IA,JSTR,' ','Link zones to DHW systems',
     &      70,NDHWSYS+2,HSYSNM,OUTS(1:44),IER,2)
            IF(JSTR(1).EQ.0)GOTO 915
            IF(JSTR(1).EQ.1)THEN
              IDHWLZ(IZ)=0
            ELSEIF(JSTR(1).EQ.2)THEN
              IF(NCMSYS.NE.0)THEN
                IA=1
                KSYS(1)=0
 916            CALL EPICKS(IA,KSYS,' ',
     &          'Link zones-HVAC systems for DHW',30,NCMSYS,HVACNAME,
     &          'Link zones-HVAC systems for DHW',IER,2)
                IF(KSYS(1).EQ.0)GOTO 916
                IDHWLZ(IZ)=-KSYS(1)
              ELSE
                IDHWLZ(IZ)=0
              ENDIF
            ELSEIF(JSTR(1).GE.2.AND.JSTR(1).LE.2+NDHWSYS)THEN
              IDHWLZ(IZ)=JSTR(1)-2
            ENDIF
 404      CONTINUE
        ENDIF
      ELSEIF(KSTR(1).EQ.7)THEN
        IF(IBUSERTYP.NE.0)THEN
          DO 140 IZ=1,NCOMP
            ICAC=0
            DO 130 IAT=1,MAC
              IF(IATYPNDX(IAT,ibusertyp).EQ.1)THEN
                ICAC=ICAC+1
                ITEMS(ICAC)=ATYPNAME(IAT)
              ENDIF
 130        CONTINUE
            WRITE(OUTS,'(2A)')'Choose activity for zone ',
     &      zname(IZ)(1:lnblnk(zname(IZ)))
            IX=1
            CALL EPICKS(IX,KSYS,' ',OUTS(1:LNBLNK(OUTS)),
     &      40,ICAC,ITEMS,OUTS(1:LNBLNK(OUTS)),IER,2)
            ICBC=0
            DO 150 IAT=1,MAC
              IF(IATYPNDX(IAT,ibusertyp).EQ.1)THEN
                ICBC=ICBC+1
                IF(ICBC.EQ.KSYS(1))THEN
                  IACTYTYP(IZ)=IAT
                  write(outs,*)'for zone ',
     &            zname(iz)(1:lnblnk(zname(iz))),
     &            ' activity is:',atypname(iat)
                  CALL EDISP(IUOUT,OUTS(1:LNBLNK(OUTS)))
                ENDIF
              ENDIF
 150        CONTINUE
 140      CONTINUE
        ELSE
          CALL EDISP(IUOUT,'Define building type first')
        ENDIF
      ELSEIF(KSTR(1).EQ.8)THEN

C Display information
 19     CALL EDISP(IUOUT,'This facility is currently not available')
      ENDIF
      IF(KSTR(1).GE.1.AND.KSTR(1).LE.6)GOTO 222

C Save information to NCM project specific (*.ncm) file
      call mksbem
      call emkcfg('-',IER)
      return
      end

C ********************* EMKSBM
C EMKSBM creates a model variant based on the SBEM methodology. Details 
C of this can be found in relevant EU and UK building regulations. 
C This subroutine copies the cfg, cnn and mandatory zones files and
C makes a new model representative of the notional building. 
C Construction and Materials databases are also copied.
C Changes to the model are then made, geometry and construction files
C are changed and so are the constructions and materials databases. 
C All networks (plant, airflow, contaminants etc) are removed from the
C model.
      SUBROUTINE EMKSBM
#include "building.h"
#include "sbem.h"
      common/OUTIN/IUOUT,IUIN
      common/pophelp/h(60)

      COMMON/C1/NCOMP,NCON

C << G1 is probably not the common block to use for this >>
      COMMON/G1/X(MTV),Y(MTV),Z(MTV),NSUR,JVN(MS,MV),NVER(MS),NTV

      common/C2/LSNAM,NCCODE(MCOM),LPROJ(MCOM),LGEOM(MCOM),
     &          LSHAD(MCOM),LTHRM(MCOM),INDUTL(MCOM),LUTIL(MCOM)
      COMMON/C3/IC1(MCON),IE1(MCON),ICT(MCON),IC2(MCON),IE2(MCON)
      common/C3F/LCNN
      COMMON/C20/NZSUR(MCOM),NZTV(MCOM)
      common/C21/IFCFG,cfgroot,LCFGF
      COMMON/C24/IZSTOCN(MCOM,MS)
      COMMON/INDICS/IVF(MCOM),ISI(MCOM),IHC(MCOM),
     &              ITW(MCOM),ICGC(MCOM),IOBS(MCOM)
      common/UDESC/LVIEW(MCOM),LHCCO(MCOM),
     &             LTWIN(MCOM),LCGCIN(MCOM),ZOBS(MCOM)

      character CTYPE*4
      real gversion
      integer igupgrade
      COMMON/G0/CTYPE(MCOM),gversion(MCOM),igupgrade
      COMMON/G6/SSNAME(MCON),SSOTF(MCON),SSMLCN(MCON),SSVFC(MCON),
     &          SSOTHER(MCON,3),SSPARENT(MCON),SSUSE(MCON,2)
      COMMON/G7/SSNA(MCON),SSPAZI(MCON),SSPELV(MCON),SSPERIM(MCON),
     &          SSUREQN(MCON,4),SSURCOG(MCON,3),SSURVN(MCON,3)
      common/FILEP/IFIL
      common/spfldat/nsset,isset,isstup,isbnstep,ispnstep,issave,isavgh
      common/spflres/sblres(MSPS),sflres(MSPS),splres(MSPS),
     &  smstres(MSPS),selres(MSPS),sipvres

      CHARACTER ACT*1
      CHARACTER APE*4,EXT*4
      CHARACTER SSPARENT*12
      CHARACTER SSMLCN*32,SSVFC*4,SSOTF*32,SSOTHER*24,SSNAME*12,SSUSE*8
      CHARACTER CFGROOT*24
      CHARACTER LCFGF*72,LCNN*72,NNAME*72,H*72
      CHARACTER*72 LSNAM,LPROJ,LGEOM,LSHAD,LTHRM,LUTIL
      CHARACTER*72 LVIEW,LHCCO,LTWIN,LCGCIN,ZOBS
      character*72 sblres,sflres,splres,smstres,sipvres,selres
      CHARACTER OUTS*124
      character doit*248

C ivalsv keeps track of new viewfactor files which need analysys
C ivalsg keeps track of whether a geometry file has alread been copied.
      dimension ivalsv(MCOM),ivalsg(MCOM)

      DIMENSION IACT(MCON),COE(3),POINT(3),TRANS(3)
      logical unixok,docnn
      logical newgeo  ! to use for testing if new/old geometry file.

C Check if Notional Model Flag has been set (NCM data has been defined)
C If not then advise user and send him/her back
      IF(ISBEM.NE.1)THEN
        H(1) = 'You have not defined NCM details about the building. '
        H(2) = 'These can be defined by choosing model context in the '
        H(3) = 'browse / edit / simulate menu and then choosing  '
        H(4) = 'option UK NCM specifications. '
        H(5) = 'Do that before choosing to generate a Notional Model '
        CALL PHELPD('NCM details undefined!',5,'-',0,0,IER)
        RETURN
      ENDIF
      APE='_not'
      newgeo=.false.  ! assume older format geometry.

      H(1) = '****************** IMPORTANT *****************'
      H(2) = ' '
      H(3) = 'Make sure that the original model follows the following '
      H(4) = 'assumptions before generating the notional model '
      H(5) = '1. All external glazings are contained within other'
      H(6) = '   surfaces. '
      H(7) = '2. Except for doors no opaque surfaces are contained'
      H(8) = '   within other surfaces. e.g. it is not allowed to '
      H(9) = '   contain a surface meant to be an opaque wall within '
      H(10)= '   another wall.'
      H(11)= ' '
      H(12)= ' '
      CALL PHELPD('SBEM Assumptions',12,'-',0,0,IER)

C Find out what type of building it is and set up flag
      H(1) = 'Windows, doors and rooflight areas are modified in the '
      H(2) = 'notional building depending upon building type. There'
      H(3) = 'are three types of building: '
      H(4) = '1. Residential (where people temporarily or permanently'
      H(5) = '   reside)'
      H(6) = '2. Assembly (places of assembly, offices and shops)'
      H(7) = '3. Industrial (industrial or storage buildings)'
      CALL EASKABCD('What type of building is it? (see help)',' ',
     &  'residential','assembly',
     &  'industrial','cancel',IBTYP,7)
      IF(IBTYP.EQ.4)THEN
        RETURN

C Establish glazing fractions based on type of building
      ELSEIF(IBTYP.EQ.1)THEN
        ROOFFR=0.2
        WALLFR=0.3
      ELSEIF(IBTYP.EQ.2)THEN
        ROOFFR=0.2
        WALLFR=0.4
      ELSEIF(IBTYP.EQ.3)THEN
        ROOFFR=0.2
        WALLFR=0.15
      ENDIF
    
C The inital task is to make copies of the relevant zone files
C which is functionally equivalent to the code blocks in subroutine
C verman for the case of geometry and attribution (ILM=2).
      call isunix(unixok)

C Assume that a new connections file needs to be created.
      docnn=.true.

C Change name of configuration file to filename_not.cfg
      EXT='.cfg'
      CALL FNCNGR(LCFGF,APE,EXT,NNAME)
      LCFGF=NNAME
  
C Loop through each zone in the model...
      DO 550 ICOMP=1,NCOMP
        IUF=IFIL+2
        EXT='.geo'
        call eclose(gversion(icomp),1.1,0.01,newgeo)
        if(newgeo)then
          call georead(IUF,LGEOM(ICOMP),ICOMP,1,0,iuout,IER)
        else
          call egomin(IUF,LGEOM(ICOMP),ICOMP,1,0,iuout,IER)
        endif
        CALL FNCNGR(LGEOM(ICOMP),APE,EXT,NNAME)
        LGEOM(ICOMP)=NNAME
        if(newgeo)then
          call geowrite(IUF,LGEOM(ICOMP),ICOMP,ITRU,3,IER)
        else
          call emkgeo(IUF,LGEOM(ICOMP),ICOMP,ITRU,3,IER)
        endif

C Establish parents of surfaces in this zone (fill common blocks G6 G7)
        ACT='C'
        CALL SURREL(ACT,ICOMP,IER)

C Remember this geometry file has been done so that a later selection
C of constructions does not re-do it.
        ivalsg(icomp)=icomp

C If there is an existing shading file copy it to new name
C so that subsequent changes are applied to the new file.
        if(ISI(icomp).eq.1)then
          EXT='.shd'
          CALL FNCNGR(LSHAD(ICOMP),APE,EXT,NNAME)
          if(unixok)then
            WRITE(doit,'(4A)')'cp ',
     &        LSHAD(ICOMP)(1:LNBLNK(LSHAD(ICOMP))),' ',
     &        NNAME(1:LNBLNK(NNAME))
          else
            WRITE(doit,'(4A)')'copy /y ',
     &        LSHAD(ICOMP)(1:LNBLNK(LSHAD(ICOMP))),' ',
     &        NNAME(1:LNBLNK(NNAME))
          endif
          CALL USRMSG('copying shading file via:',doit,'-')
          CALL RUNIT(doit,'-')
          LSHAD(ICOMP)=NNAME
        endif

C If there is an existing viewfactor file copy it to new name
C so that subsequent changes are applied to the new file.
        if(IVF(icomp).eq.1)then
          EXT='.vwf'
          CALL FNCNGR(LVIEW(ICOMP),APE,EXT,NNAME)
          if(unixok)then
            WRITE(doit,'(4A)')'cp ',
     &        LVIEW(ICOMP)(1:LNBLNK(LVIEW(ICOMP))),' ',
     &        NNAME(1:LNBLNK(NNAME))
          else
            WRITE(doit,'(4A)')'copy /y ',
     &        LVIEW(ICOMP)(1:LNBLNK(LVIEW(ICOMP))),' ',
     &        NNAME(1:LNBLNK(NNAME))
          endif
          CALL USRMSG('copying viewfactor file via:',doit,'-')
          CALL RUNIT(doit,'-')
          LVIEW(ICOMP)=NNAME

C ? after copy it is probably not necessary to re-run the viewfactor analysis.
C          ivalsv(ic)=ivals(IC)
        endif

C If there is a heat transfer regime copy the exiting file
C so that subsequent changes are applied to the new file.
        if(IHC(icomp).eq.1)then
          EXT='.htc'
          CALL FNCNGR(LHCCO(ICOMP),APE,EXT,NNAME)
          if(unixok)then
            WRITE(doit,'(4A)')'cp ',
     &        LHCCO(ICOMP)(1:LNBLNK(LHCCO(ICOMP))),' ',
     &        NNAME(1:LNBLNK(NNAME))
          else
            WRITE(doit,'(4A)')'copy /y ',
     &        LHCCO(ICOMP)(1:LNBLNK(LHCCO(ICOMP))),' ',
     &        NNAME(1:LNBLNK(NNAME))
          endif
          CALL USRMSG('copying hc regime file via:',doit,'-')
          CALL RUNIT(doit,'-')
          LHCCO(ICOMP)=NNAME
        endif

C If there is a casual gain control file copy the exiting file
C so that subsequent changes are applied to the new file.
        if(ICGC(icomp).eq.1)then
          EXT='.cgc'
          CALL FNCNGR(LHCCO(ICOMP),APE,EXT,NNAME)
          if(unixok)then
            WRITE(doit,'(4A)')'cp ',
     &        LCGCIN(ICOMP)(1:LNBLNK(LCGCIN(ICOMP))),' ',
     &        NNAME(1:LNBLNK(NNAME))
          else
            WRITE(doit,'(4A)')'copy /y ',
     &        LCGCIN(ICOMP)(1:LNBLNK(LCGCIN(ICOMP))),' ',
     &        NNAME(1:LNBLNK(NNAME))
          endif
          CALL USRMSG('copying casual gain ctl file via:',doit,'-')
          CALL RUNIT(doit,'-')
          LCGCIN(ICOMP)=NNAME
        endif

C If there is an obstruction file copy the exiting file
C so that subsequent changes are applied to the new file.
        if(IOBS(icomp).eq.1)then
          EXT='.obs'
          CALL FNCNGR(ZOBS(ICOMP),APE,EXT,NNAME)
          if(unixok)then
            WRITE(doit,'(4A)')'cp ',
     &        ZOBS(ICOMP)(1:LNBLNK(ZOBS(ICOMP))),' ',
     &        NNAME(1:LNBLNK(NNAME))
          else
            WRITE(doit,'(4A)')'copy /y ',
     &        ZOBS(ICOMP)(1:LNBLNK(ZOBS(ICOMP))),' ',
     &        NNAME(1:LNBLNK(NNAME))
          endif
          CALL USRMSG('copying obstruction file via:',doit,'-')
          CALL RUNIT(doit,'-')
          ZOBS(ICOMP)=NNAME
        endif
 550  CONTINUE
      call edisp(iuout,
     &   'New zone files available for applying nominal rules.')

C Remind the user that other files have been copied.
      ishdq=-1
      ivfwq=-1
      do 43 iz=1,ncomp
        if(ISI(iz).eq.1)ishdq=1
        if(IVF(iz).eq.1)ivfwq=1
  43  continue
      if(ishdq.eq.1)then
        call usrmsg(
     &    'shading files found which will be dependant on future',
     &    'geometry changes. Shading files copied.','W')
        ishd=1
      endif
      if(ivfwq.eq.1)then
        call usrmsg(
     &    'viewfactor files found which will be dependant on future',
     &    'geometry changes. Viewfactor files copied.','W')
      endif

C At this point prepare to update the configuration and connections
C file for the nominal mode. 
C But first change names of results libraries zones, flow, plant,
C moisture, electrical and IPV
      DO 123 ISPS=1,NSSET
        EXT='.res'
        CALL FNCNGR(SBLRES(ISPS),APE,EXT,NNAME)
        if(NNAME(1:4).ne.'not_')SBLRES(ISPS)=NNAME
        EXT='.mfr'
        CALL FNCNGR(SFLRES(ISPS),APE,EXT,NNAME)
        if(NNAME(1:4).ne.'not_')SFLRES(ISPS)=NNAME
        EXT='.plr'
        CALL FNCNGR(SPLRES(ISPS),APE,EXT,NNAME)
        if(NNAME(1:4).ne.'not_')SPLRES(ISPS)=NNAME
        EXT='.msr'
        CALL FNCNGR(SMSTRES(ISPS),APE,EXT,NNAME)
        if(NNAME(1:4).ne.'not_')SMSTRES(ISPS)=NNAME
        EXT='.res'
        CALL FNCNGR(SELRES(ISPS),APE,EXT,NNAME)
        if(NNAME(1:4).ne.'not_')SELRES(ISPS)=NNAME
 123  CONTINUE
      EXT='.rep'
      CALL FNCNGR(SIPVRES,APE,EXT,NNAME)
      SIPVRES=NNAME
      LRCR=2

C Check length of cfgroot string.
      IROOTLEN=LNBLNK(CFGROOT)
      IF(IROOTLEN.GT.(24-3))THEN
        IROOTLEN=21
      ENDIF
      write(cfgroot,'(2a)')cfgroot(1:irootlen),ape

C If a variant connections file also required set this up.
      if(docnn)then
        EXT='.cnn'
        CALL FNCNGR(LCNN,APE,EXT,NNAME)
        LCNN=NNAME
      endif
      CALL EMKCFG('-',IER)
      WRITE (OUTS,'(2A)')'Writing new configuration file ',LCFGF
      CALL EDISP(IUOUT,OUTS)


C << the following logic does not work so is skipped for the current time >>

C Work out areas of all child surfaces (AREA) and transparent
C surfaces (TAREA) and orientation of child and parent (IACT)

C Loop through each of the connections in the model.


C IACT is action flag that means:
C 0 = Exterior surface has requisite amount of glazing, do nothing
C 1 = Exterior surface has no child surfaces and needs requisite amount
C     of glazing inserted
C 2 = Exterior surface has one or more child surfaces (all transparent)
C     but more glazed area needs to be added
C 3 = Exterior surface has one or more child surfaces (all opaque) and
C     needs requisite amount of glazing
C 4 = Exterior surface has mix of opaque and transparent children and
C     needs requisite amount of glazing
C          IACT(ISUR)=0
C          IF(NCHILD.EQ.0)THEN
C            IACT(ISUR)=1
C          ELSEIF(NCHILD.EQ.NTCHILD)THEN
C            IACT(ISUR)=2
C          ELSEIF(NTCHILD.EQ.0)THEN
C            IACT(ISUR)=3
C          ELSE
C            IACT(ISUR)=4
C          ENDIF

C          ICOMP=IC1(ISUR)  ! the zone associated with this connection.

C Find the number of surfaces in this zone.
C          NSUR=NZSUR(icomp)

C And we might need to re-scan geometry file.

C          CALL ESCZONE(ICOMP)   ! ?? why the G1 common blocks are not fresh.

C Modify geometry files to have exterior walls/roofs with prescribed 
C amount of glazing and doors. If the area of glazing is less than 
C prescribed make balance up with windows and rooflights.
C          KOUNTER=IACT(ISUR)
C          IF(KOUNTER.EQ.0)THEN
C            CONTINUE
C          ELSEIF(KOUNTER.EQ.1)THEN

C Enlarge/Diminish the glazed surface with vertex weighted centre of 
c gravity of the parent surface as centre of enlargement.
C            DO 560 I=1,3
C              COE(I)=SSURCOG(ISUR,I)
C 560        CONTINUE
C            IF(SSVFC(ISUR)(1:4).EQ.'VERT')THEN
C              FACTOR=SQRT(WALLFR)
C            ELSEIF(SSVFC(ISUR)(1:4).EQ.'SLOP'.OR.
C      &            SSVFC(ISUR)(1:4).EQ.'CEIL')THEN
C              FACTOR=SQRT(ROOFFR)
C            ENDIF

C << NVER and JVN are not the correct variable to use unless the zone
C << geometry has been refreshed. Probably need to use the 
C << same logic as in esrucfg.
C            DO 561 NV=1,NVER(ISUR)
C              POINT(1)=X(JVN(ISUR,NV))
C              POINT(2)=Y(JVN(ISUR,NV))
C              POINT(3)=Z(JVN(ISUR,NV))
C              CALL ENLARGE(POINT,TRANS,COE,FACTOR)

C Add new vertex to vertex list
C              X(JVN(NSUR+1,NV))=TRANS(1)
C              Y(JVN(NSUR+1,NV))=TRANS(2)
C              Z(JVN(NSUR+1,NV))=TRANS(3)
C 561        CONTINUE

C Make new surface based on these vertices
C << the array size for nver is not the same as for SSVFC
C            NSUR=NSUR+1
C            NVER(NSUR)=NVER(ISUR)
C            SSVFC(NSUR)=SSVFC(ISUR)
C            SSOTF(NSUR)='TRAN'
C            SSOTHER(NSUR,1)='EXTERIOR'

C << probably want I2.2 as the format statement but what
C << happens with more than 99 surfaces? >>
C            WRITE(SSNAME(NSUR),'(I2,A)')ISUR,'_notional'
C            IZSTOCN(ICOMP,NSUR)=NSUR
C          ELSEIF(KOUNTER.EQ.2)THEN
C          ELSEIF(KOUNTER.EQ.3)THEN
C          ELSEIF(KOUNTER.EQ.4)THEN
C          ENDIF
C        ENDIF
C 551  CONTINUE

C After having made the changes, save the nominal model configuration file.
C      CALL EMKCFG('-',IER)
C      WRITE (OUTS,'(2A)')'Writing new configuration file ',LCFGF
C      CALL EDISP(IUOUT,OUTS)

C << now what is in memory and in the files is the nominal model
C << do we leave it at that or return to the original model?

      RETURN
      END

************************* ENLARGE *****************************
C Subroutine ENLARGE performs geometric enlargement (contraction) of a
C surface and for a given vertex (POINT) it returns a transformed
C vertex (TRANS) based on a scale factor (factor) and a centre of
C enlargement (coe). TRANS will be somewhere along the line between
C COE and POINT. It is expected to be called once for each vertex
C of the surface who's size is to be adjusted.

C Factor can be a fraction (in which case it is a contraction of a
C geometric surface or may be greater than one in which case it is an
C enlargement.
      SUBROUTINE ENLARGE(POINT,TRANS,COE,FACTOR)
      DIMENSION POINT(3),COE(3),TRANS(3)
      DO 1 I=1,3
        TRANS(I)=(ABS(1.-FACTOR))*COE(I)+FACTOR*POINT(I)
 1    CONTINUE
      RETURN
      END

************************* RSBEM *****************************
C Subroutine RSBEM reads a previously defined SBEM project file and
C fills relevant common blocks.
      SUBROUTINE RSBEM
#include "building.h"
#include "sbem.h"
#include "espriou.h"
C espriou.h provides currentfile.
      common/FILEP/IFIL
      common/C1/NCOMP,NCON

      CHARACTER OUTSTR*124
      LOGICAL NEAR

      IUF=IFIL+1
      CALL EFOPSEQ(IUF,LASBEM,1,IER)

      write(currentfile,'(a)') LASBEM(1:lnblnk(LASBEM))
      CALL STRIPC(IUF,OUTSTR,99,ND,1,'ncm file',IER)
      IF(OUTSTR(1:9).NE.'*SBEM 1.0')THEN
        CONTINUE ! WARNING MSG TO GO HERE
      ELSE
        CALL STRIPC(IUF,OUTSTR,99,ND,1,'pjname',IER)
        pjname=outstr(14:77)
        CALL STRIPC(IUF,OUTSTR,99,ND,1,'baddress',IER)
        baddress=outstr(18:81)
        CALL STRIPC(IUF,OUTSTR,99,ND,1,'bcity',IER)
        bcity=outstr(15:78)
        CALL STRIPC(IUF,OUTSTR,99,ND,1,'buildingPC',IER)
        buildingPC=outstr(19:82)
        CALL STRIPC(IUF,OUTSTR,99,ND,1,'ownername',IER)
        ownername=outstr(21:84)
        CALL STRIPC(IUF,OUTSTR,99,ND,1,'ownertel',IER)
        ownertel=outstr(26:89)
        CALL STRIPC(IUF,OUTSTR,99,ND,1,'owneraddr',IER)
        owneraddr=outstr(24:87)
        CALL STRIPC(IUF,OUTSTR,99,ND,1,'ownercity',IER)
        ownercity=outstr(21:84)
        CALL STRIPC(IUF,OUTSTR,99,ND,1,'ownerPC',IER)
        ownerPC=outstr(25:88)
        CALL STRIPC(IUF,OUTSTR,99,ND,1,'certifname',IER)
        certifname=outstr(16:79)
        CALL STRIPC(IUF,OUTSTR,99,ND,1,'certTel',IER)
        certTel=outstr(21:84)
        CALL STRIPC(IUF,OUTSTR,99,ND,1,'certaddress',IER)
        certaddress=outstr(19:82)
        CALL STRIPC(IUF,OUTSTR,99,ND,1,'certcity',IER)
        certcity=outstr(16:79)
        CALL STRIPC(IUF,OUTSTR,99,ND,1,'certPC',IER)
        certPC=outstr(20:83)
        CALL STRIPC(IUF,OUTSTR,99,ND,1,'Building type index',IER)
        K=21
        CALL EGETWI(OUTSTR,K,ibusertyp,0,0,'-','bldg typ index.',IER)
        CALL STRIPC(IUF,OUTSTR,99,ND,1,'Building type',IER)
        CALL STRIPC(IUF,OUTSTR,99,ND,1,'bldg services strategy',IER)
        K=28
        CALL EGETWI(OUTSTR,K,IBSS,0,0,'-','bldg services strategy',IER)
        CALL STRIPC(IUF,OUTSTR,99,ND,1,'building regulations',IER)
        K=22
        CALL EGETWI(OUTSTR,K,IRGG,0,0,'-','building regulations',IER)
        CALL STRIPC(IUF,OUTSTR,99,ND,1,'bldg design stage',IER)
        K=23
        CALL EGETWI(OUTSTR,K,ISTG,0,0,'-','bldg design stage',IER)
        CALL STRIPC(IUF,OUTSTR,99,ND,1,'Scottish bldg type',IER)
        K=24
        CALL EGETWI(OUTSTR,K,ISBT,0,0,'-','Scottish bldg type',IER)
        CALL STRIPC(IUF,OUTSTR,99,ND,1,'Accredited const details',IER)
        K=54
        CALL EGETRM(OUTSTR,K,SBREF,'W','NCM file',IER)
        CALL STRIPC(IUF,OUTSTR,99,ND,1,'Building permeability',IER)
        K=44
        CALL EGETWR(OUTSTR,K,BINF50,0.,15.,'W','bldg permeability',IER)
        CALL STRIPC(IUF,OUTSTR,99,ND,1,'permeability compl chk',IER)
        K=60
        CALL EGETRM(OUTSTR,K,APCHK,'W','perm compliance checking',IER)
      ENDIF
      CALL STRIPC(IUF,OUTSTR,99,ND,1,'HVAC systems',IER)
      K=13
      CALL EGETWI(OUTSTR,K,NCMSYS,0,0,'-','HVAC systems.',IER)
      IF(NCMSYS.EQ.0)GOTO 1001
      DO 101 ICMSYS=1,NCMSYS
        CALL STRIPC(IUF,OUTSTR,99,ND,1,'HVAC systems',IER)
        CALL STRIPC(IUF,OUTSTR,99,ND,1,'HVAC systems',IER)
        HVACNAME(ICMSYS)=OUTSTR(1:LNBLNK(OUTSTR))
        CALL STRIPC(IUF,OUTSTR,99,ND,1,'HVAC systems',IER)
        K=0
        CALL EGETWI(OUTSTR,K,NMSYS,0,0,'-','HVAC systems index.',IER)
        INCMSYS(ICMSYS)=NMSYS
        CALL STRIPC(IUF,OUTSTR,99,ND,1,'heat generator',IER)
        K=0        
        CALL EGETWR(OUTSTR,K,HGEF(ICMSYS),0.,10.,'W','heat eff.',IER)
        CALL EGETWI(OUTSTR,K,IHGEF(ICMSYS),0,0,'-','heat index.',IER)
        CALL STRIPC(IUF,OUTSTR,99,ND,1,'BRUKL heat gen index',IER)
        K=0
        CALL EGETWI(OUTSTR,K,IBRUKH(ICMSYS),0,0,'-','fuel type',IER)
        CALL STRIPC(IUF,OUTSTR,99,ND,1,'fuel type',IER)
        K=0        
        CALL EGETWI(OUTSTR,K,IFTYP(ICMSYS),0,0,'-','fuel type',IER)
        CALL STRIPC(IUF,OUTSTR,99,ND,1,'cool generator',IER)
        K=0        
        CALL EGETWR(OUTSTR,K,CGEF(ICMSYS),0.,10.,'W','cool eff.',IER)
        CALL ECLOSE(CGEF(ICMSYS),0.0,1.E-5,NEAR)
        IF(.NOT.NEAR)
     &  CALL EGETWI(OUTSTR,K,ICGEF(ICMSYS),0,0,'-','cool index.',IER)
        CALL STRIPC(IUF,OUTSTR,99,ND,1,'BRUKL cool gen index',IER)
        K=0
        CALL EGETWI(OUTSTR,K,IBRUKC(ICMSYS),0,0,'-','fuel type',IER)
        CALL STRIPC(IUF,OUTSTR,99,ND,1,'SFP',IER)
        K=0        
        CALL EGETWR(OUTSTR,K,SFPHS(ICMSYS),0.,10.,'W','SFP',IER)
        CALL STRIPC(IUF,OUTSTR,99,ND,1,'BRUKL fan index',IER)
        K=0
        CALL EGETWI(OUTSTR,K,IBRUKF(ICMSYS),0,0,'-','fuel type',IER)
 101  CONTINUE
 1001 CALL STRIPC(IUF,OUTSTR,99,ND,1,'Zone to systems linkage',IER)
      DO 201 IZ=1,NCOMP
        CALL STRIPC(IUF,OUTSTR,99,ND,1,'Zone to systems linkage',IER)
        K=44
        CALL EGETWI(OUTSTR,K,IHLZ(IZ),0,0,'-','zone-system links',IER)
 201  CONTINUE
      CALL STRIPC(IUF,OUTSTR,99,ND,1,'DHW generators',IER)
      K=16
      CALL EGETWI(OUTSTR,K,NDHWSYS,0,0,'-','zone-system links',IER)
      DO 202 IDHWSYS=1,NDHWSYS
        CALL STRIPC(IUF,OUTSTR,99,ND,1,'DHW generators number',IER)
        CALL STRIPC(IUF,OUTSTR,99,ND,1,'DHW generators name',IER)
        DHWNAME(IDHWSYS)=OUTSTR(1:LNBLNK(OUTSTR))
        CALL STRIPC(IUF,OUTSTR,99,ND,1,'DHW generators name',IER)
        K=0
        CALL EGETWR(OUTSTR,K,HWEF(IDHWSYS),0.,10.,'W','eff',IER)
        CALL EGETWI(OUTSTR,K,IDHWS(IDHWSYS),0,0,'-','dhw index',IER)
        CALL EGETWI(OUTSTR,K,IDHFL(IDHWSYS),0,0,'-','fuel index',IER)
        CALL EGETWI(OUTSTR,K,IBRUKW(IDHWSYS),0,0,'-','BRUKL index',IER)
 202  CONTINUE
      CALL STRIPC(IUF,OUTSTR,99,ND,1,'DHW zone links',IER)
      DO 203 IZ=1,NCOMP
        CALL STRIPC(IUF,OUTSTR,99,ND,1,'Zone to DHW linkage',IER)
        K=48
        CALL EGETWI(OUTSTR,K,IDHWLZ(IZ),0,0,'-','zone-DHW links',IER)
 203  CONTINUE
      CALL STRIPC(IUF,OUTSTR,99,ND,1,'activities zone links',IER)
      DO 204 IZ=1,NCOMP
        CALL STRIPC(IUF,OUTSTR,99,ND,1,'Zone to activity linkage',IER)
        K=47
        CALL EGETWI(OUTSTR,K,IACTYTYP(IZ),0,0,'-','zone-DHW links',IER)
 204  CONTINUE
      
      RETURN
      END

************************* MKSBEM *****************************
C Subroutine MKSBEM writes out a project specific SBEM file that holds
C data relevant to the UK National Calculation Method.
      SUBROUTINE MKSBEM
#include "building.h"
#include "sbem.h"
      common/FILEP/IFIL
      common/C21/IFCFG,cfgroot,LCFGF
      common/C1/NCOMP,NCON
      COMMON/precz/zname(MCOM),zdesc(MCOM)

      CHARACTER CFGROOT*24,LCFGF*72,TMPSTR*64
      CHARACTER ZNAME*12,ZDESC*64
      TMPSTR=' '
      IUF=IFIL+1

C Create SBEM file
C<< Create facility to read/write file with user defined name
      IF(ISBEM.NE.1)THEN
        WRITE(LASBEM,'(2a)')CFGROOT(1:LNBLNK(CFGROOT)),'.ncm'
      ENDIF
      ISBEM=1
      CALL EFOPSEQ(IUF,LASBEM,3,IER)

C Write high level project specific detail
      WRITE(IUF,'(A)',IOSTAT=IOS,ERR=3) '*SBEM 1.0'
      K=0
      TMPSTR=PJNAME
      CALL EGETRM(TMPSTR,K,pjname,'W','NCM file',IER)
      WRITE(IUF,'(2A)',IOSTAT=IOS,ERR=3)'Project name, ',pjname
      K=0
      TMPSTR=baddress
      CALL EGETRM(TMPSTR,K,baddress,'W','NCM file',IER)
      WRITE(IUF,'(2A)',IOSTAT=IOS,ERR=3)'Building address, ',baddress
      K=0
      TMPSTR=bcity
      CALL EGETRM(TMPSTR,K,bcity,'W','NCM file',IER)
      WRITE(IUF,'(2A)',IOSTAT=IOS,ERR=3)'Building city, ',bcity 
      K=0
      TMPSTR=buildingPC
      CALL EGETRM(TMPSTR,K,buildingPC,'W','NCM file',IER)
      WRITE(IUF,'(2A)',IOSTAT=IOS,ERR=3)'Building Postcode, ',buildingPC
      K=0
      TMPSTR=ownername
      CALL EGETRM(TMPSTR,K,ownername,'W','NCM file',IER)
      WRITE(IUF,'(2A)',IOSTAT=IOS,ERR=3)
     &     'Building owner name, ',ownername
      K=0
      TMPSTR=ownertel
      CALL EGETRM(TMPSTR,K,ownertel,'W','NCM file',IER)
      WRITE(IUF,'(2A)',IOSTAT=IOS,ERR=3)
     &     'Building owner telephone, ',ownertel
      K=0
      TMPSTR=owneraddr
      CALL EGETRM(TMPSTR,K,owneraddr,'W','NCM file',IER)
      WRITE(IUF,'(2A)',IOSTAT=IOS,ERR=3)
     &     'Building owner address, ',owneraddr
      K=0
      TMPSTR=ownercity
      CALL EGETRM(TMPSTR,K,ownercity,'W','NCM file',IER)
      WRITE(IUF,'(2A)',IOSTAT=IOS,ERR=3)
     &     'Building owner city, ',ownercity
      K=0
      TMPSTR=ownerPC
      CALL EGETRM(TMPSTR,K,ownerPC,'W','NCM file',IER)
      WRITE(IUF,'(2A)',IOSTAT=IOS,ERR=3)
     &     'Building owner Postcode, ',ownerPC
      K=0
      TMPSTR=certifname
      CALL EGETRM(TMPSTR,K,certifname,'W','NCM file',IER)
      WRITE(IUF,'(2A)',IOSTAT=IOS,ERR=3)'Certifier name, ',certifname
      K=0
      TMPSTR=certTel
      CALL EGETRM(TMPSTR,K,certTel,'W','NCM file',IER)
      WRITE(IUF,'(2A)',IOSTAT=IOS,ERR=3)'Certifier telephone, ',certTel
      K=0
      TMPSTR=certaddress
      CALL EGETRM(TMPSTR,K,certaddress,'W','NCM file',IER)
      WRITE(IUF,'(2A)',IOSTAT=IOS,ERR=3)
     &     'Certifier address, ',certaddress
      K=0
      TMPSTR=certcity
      CALL EGETRM(TMPSTR,K,certcity,'W','NCM file',IER)
      WRITE(IUF,'(2A)',IOSTAT=IOS,ERR=3)'Certifier city, ',certcity
      K=0
      TMPSTR=certPC
      CALL EGETRM(TMPSTR,K,certPC,'W','NCM file',IER)
      WRITE(IUF,'(2A)',IOSTAT=IOS,ERR=3)'Certifier postcode, ',certPC
      WRITE(IUF,'(A,I4)',IOSTAT=IOS,ERR=3)'Building type index, ',
     &     ibusertyp
      K=0
      TMPSTR=BTYPNAME(ibusertyp)
      CALL EGETRM(TMPSTR,K,BTYPNAME(ibusertyp),'W','NCM file',IER)
      IF(IBUSERTYP.NE.0)THEN
        WRITE(IUF,'(2A)',IOSTAT=IOS,ERR=3)'Building type, ',
     &     BTYPNAME(ibusertyp)
      ELSE
        WRITE(IUF,'(2A)',IOSTAT=IOS,ERR=3)'Building type, ',
     &     'not defined yet'
      ENDIF
      IF(IBSS.NE.0)THEN
        WRITE(IUF,'(A,I3,1X,A)',IOSTAT=IOS,ERR=3)
     &  'Building services strategy, ',IBSS,BLDSS(IBSS)
      ELSE
        WRITE(IUF,'(A)',IOSTAT=IOS,ERR=3)
     &  'Building services strategy, 0 not defined yet'
      ENDIF
      WRITE(IUF,'(A,I3,1X,A)',IOSTAT=IOS,ERR=3)
     &'Building Regulations, ',IRGG,BLDREG(IRGG)
      WRITE(IUF,'(A,I3,1X,A)',IOSTAT=IOS,ERR=3)
     &'Building Design Stage, ',ISTG,BLDSTG(ISTG)
      IF(ISBT.NE.0)THEN
        WRITE(IUF,'(A,I3,1X,A)',IOSTAT=IOS,ERR=3)
     &  'Scottish building type, ',ISBT,SBTYP(ISBT)
        WRITE(IUF,'(2A)',IOSTAT=IOS,ERR=3)
     &  'Accredited construction details (Scotland) followed?  ',SBREF
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
     &APCHK 

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
        WRITE(IUF,'(A)')HVACNAME(ICMSYS)
        WRITE(IUF,'(1X,I4,1X,A)',IOSTAT=IOS,ERR=3)INCMSYS(ICMSYS),
     &     HSYSNAME(INCMSYS(ICMSYS))

        WRITE(IUF,'(A)')'# heat generator efficiency, index and name'
        WRITE(IUF,'(1X,F6.3,1X,I4,1X,A)',IOSTAT=IOS,ERR=3)
     &     HGEF(ICMSYS),IHGEF(ICMSYS),SYSNAME(IHGEF(ICMSYS))
        IBRUK=IBRUKLH(IFTYP(ICMSYS),IHGEF(ICMSYS),INCMSYS(ICMSYS))
        IF(IBRUK.LE.0)IBRUK=-5555
        WRITE(IUF,'(1X,I5,1X,A)',IOSTAT=IOS,ERR=3)
     &    IBRUK,' # BRUKL heat generator index number'

        WRITE(IUF,'(A)')'# fuel type index and name'
        WRITE(IUF,'(1X,I4,1X,A)',IOSTAT=IOS,ERR=3)IFTYP(ICMSYS),
     &     FUELNAME(IFTYP(ICMSYS))

        WRITE(IUF,'(A)')'# cool generator efficiency, index and name '
        IF(ICGEF(ICMSYS).NE.0)THEN
          WRITE(IUF,'(1X,F6.3,1X,I4,1X,A)',IOSTAT=IOS,ERR=3)CGEF(ICMSYS)
     &    ,ICGEF(ICMSYS),SYSNAME(ICGEF(ICMSYS)+29)
        ELSE
          WRITE(IUF,'(1X,F6.3,1X,A)',IOSTAT=IOS,ERR=3)CGEF(ICMSYS),
     &    ' # No cool generator specified'
        ENDIF

C Set BRUKL cooling value to -5555 if cooling system is not defined
        IBRUK=IBRUKLC(ICGEF(ICMSYS),INCMSYS(ICMSYS))
        IF(IBRUK.LE.0)IBRUK=-5555

C Set BRUKL cooling value if no cooling generator is present in HVAC system        
        IF(INCMSYS(ICMSYS).LE.10)IBRUK=-1111
        WRITE(IUF,'(1X,I5,1X,A)',IOSTAT=IOS,ERR=3)
     &    IBRUK,' # BRUKL cool generator index number'

        WRITE(IUF,'(1X,F6.3,1X,A)',IOSTAT=IOS,ERR=3)SFPHS(ICMSYS),
     &    ' # Specific fan power W/l/s (0 if fans N/A)'
        WRITE(IUF,'(1X,I5,1X,A)',IOSTAT=IOS,ERR=3)
     &    IBRUKLF(INCMSYS(ICMSYS)),' # BRUKL fan index number'
 101  CONTINUE

C Write building zone to HVAC system linkage information
      WRITE(IUF,'(A,I4,A)',IOSTAT=IOS,ERR=3)'*Zone to system linkage',
     &     NCOMP,' # total number of zones'
      DO 102 IZ=1,NCOMP
        IF(IHLZ(IZ).EQ.0)THEN
          TMPSTR=' No HVAC system for this zone!'
        ELSE
          WRITE(TMPSTR,'(2A)')' ',HVACNAME(INCMSYS(IHLZ(IZ)))
        ENDIF
        WRITE(IUF,'(3A,I3,A)',IOSTAT=IOS,ERR=3)
     &     'Zone ',ZNAME(IZ),' is linked to system number ',
     &     IHLZ(IZ),TMPSTR
 102  CONTINUE

C Write DHW generator information
      WRITE(IUF,'(A,I4,A)',IOSTAT=IOS,ERR=3)'*DHW generators',
     &     NDHWSYS,' # total number of DHW generators'
      DO 103 IDHWSYS=1,NDHWSYS
        WRITE(IUF,'(A,I4)',IOSTAT=IOS,ERR=3)'DHW generator number',
     &     IDHWSYS
        WRITE(IUF,'(A)',IOSTAT=IOS,ERR=3)
     &  '# name, efficiency, type index, fuel type index, BRUKL index'
        WRITE(IUF,'(A)',IOSTAT=IOS,ERR=3)DHWNAME(IDHWSYS)
        WRITE(IUF,'(F6.3,1X,I4,1X,I4,1X,I4,1X,I4)',IOSTAT=IOS,ERR=3)
     &  HWEF(IDHWSYS),IDHWS(IDHWSYS),
     &  IDHFL(IDHWSYS),IBRUKLW(IDHWS(IDHWSYS),IDHFL(IDHWSYS))
 103    CONTINUE

C Write building zone to DHW system linkage information
      WRITE(IUF,'(A,I4,A)',IOSTAT=IOS,ERR=3)'*Zone to DHW linkage',
     &     NCOMP,' # total number of zones'
      DO 104 IZ=1,NCOMP
        IF(IDHWLZ(IZ).EQ.0)THEN
          TMPSTR=' No HVAC system for this zone!'
        ELSEIF(IDHWLZ(IZ).GT.0)THEN
          WRITE(TMPSTR,'(2A)')' ',DHWNAME(IDHWLZ(IZ))
        ELSEIF(IDHWLZ(IZ).LT.0)THEN
          WRITE(TMPSTR,'(2A)')' (HVAC system) ',HVACNAME(-IDHWLZ(IZ))
        ENDIF
        WRITE(IUF,'(3A,I3,A)',IOSTAT=IOS,ERR=3)
     &     'Zone ',ZNAME(IZ),' is linked to DHW system number ',
     &     IDHWLZ(IZ),TMPSTR
 104  CONTINUE

C Write building zone to activity type linkage information
      WRITE(IUF,'(A)',IOSTAT=IOS,ERR=3)'*Zone to activity linkage'
      DO 105 IZ=1,NCOMP
        WRITE(IUF,'(3A,1X,I4,1X,A)')
     &  'Zone ',zname(iz),' is linked to activity number ',IACTYTYP(IZ),
     &  atypname(IACTYTYP(IZ))
 105  CONTINUE
      CALL ERPFREE(IUF,ISTAT)
      RETURN
 3    CALL USRMSG('Problem writing data to sbem file',' ','W')
      END
      
************************* NCMCAL *****************************
C Subroutine NCMCAL checks if sufficient information is present in the
C model in order to generate the BRUKL input file. Sufficient
C information being:
C 1. Information about project (building), owner and certifier. 
C    This information can be added from the context menu. 
C    If this information is not present then it asks the user for it.
C 2. Makes sure building type information is also present in the model.
C    If not then asks user for it.
C 3. There must be a working notional model in the cfg folder. If
C    not then automatically makes one. There is facility in the variants
C    option in the main project manager menu to manually make a notional
C    model.
C 4. Makes sure that SBEM plant and systems have been defined and that
C    there is a linkage between zones and plant types. If SBEM plant and
C    systems are not defined it provides user with the opportunity to do
C    so. 
C    NOTE: SBEM plant and systems are not part of dynamic thermal
C    simulation but make use of seasonal efficiency values to generate
C    carbon dioxide emissions.
C If sufficient information is not present it generates this 
C this information silently as far as possible.
C It then calls simulation for the actual building and notional model,
C Then maps energy use to zone linkage of SBEM plant and systems and
C generates Building Emission Rating and Target Emission Rating, BER
C and TER.
C It finally generate BRUKL input document and other SBEM reports.
      SUBROUTINE NCMCAL(IER)
#include "sbem.h"
#include "building.h"
      common/FILEP/IFIL
      common/C1/NCOMP,NCON
      COMMON/C3/IC1(MCON),IE1(MCON),ICT(MCON),IC2(MCON),IE2(MCON)
      COMMON/C20/NZSUR(MCOM),NZTV(MCOM)
      common/C21/IFCFG,cfgroot,LCFGF
      COMMON/C24/IZSTOCN(MCOM,MS)
      COMMON/G6/SSNAME(MCON),SSOTF(MCON),SSMLCN(MCON),SSVFC(MCON),
     &          SSOTHER(MCON,3),SSPARENT(MCON),SSUSE(MCON,2)
      COMMON/G7/SSNA(MCON),SSPAZI(MCON),SSPELV(MCON),SSPERIM(MCON),
     &          SSUREQN(MCON,4),SSURCOG(MCON,3),SSURVN(MCON,3)
      COMMON/MLC/NMLC,DESC(MMLC),DTHK(MMLC,ME),IPR(MMLC,ME),
     &           LAYERS(MMLC),DRAIR(MMLC,ME,3)

C Build variable (These should eventually be placed in build_info.h)
      character cBuilder*32          ! User who compiled system
      character cArchitecture*32     ! Machine archictecture
      character cOperSystem*32       ! Operating system
      character cOSversion*32        ! OS flavour/version
      character cHost*256            ! Host name
      character cDate*64             ! Build date 
      logical   bVersioned           ! Flag indicating if repository 
                                     !   is versioned 
      logical bRelease               ! Logical indicating if this is 
                                     !   a release version.
      logical bBuild_Info            ! Flag indicating if build info should 
                                     !   be dumped. 
      logical bModified              ! Flag indicating local copy contains 
                                     !   modifications
      character cRelease_Num*8       ! Release version
      character cBase*8              ! Base version
      character cURL*256             ! Repository URL
      character cSource*32           ! Repository version
      character cBranch*48           ! Branch name 
      character cRevision*32         ! Revision number 
      character cState*32            ! Flag indicating repository state.
      character cCC*32               ! Flags for compilers
      character cFC*32               !
      character cCPL*32              !
      character cXML_support*32      ! Flags for compile-time options
      character cXlibrary*32         !

      CHARACTER BRUKFL*72
      CHARACTER CFGROOT*24,LCFGF*72
      character*12 SN,ZSN,FN,ZFN,RN,ZRN,TRN,ZTRN
      CHARACTER SSPARENT*12,TMPSTR*24,DESC*48,TMPSTR2*70
      CHARACTER SSMLCN*32,SSVFC*4,SSOTF*32,SSOTHER*24,SSNAME*12,SSUSE*8
      LOGICAL CONTEXT,BNDRY,ISNEAR,XST
      REAL TMPREAL
#include "build_info.h"

C Check information about building, certifier and owner to be present
      
C Read *.ncm file and SBEM database
      CALL RSBEM
      CALL SBEMPR(IER)

C Following parameters to be present when writing BRUKL file
C Place holder static values are used currently
C CO2NOTPERM2 = notional building's CO2 emission rate in kg/m2 per year
C BER         = building emissions rate CO2/m2 year
C TER         = target emissions rate CO2/m2 year
C U-Values for walls
C U-Values for floors
C U-Values for roofs
C U-Values for glazings
C U-Values for personal doors
C U-Values for vehile access doors
C U-Values for high usage doors

      CO2NOTPERM2=100.0
      BER=90.
      TER=91.

C Start writing BRUKL file
C Building, owner, certifier detail...
      ILEN=LNBLNK(LASBEM)
      IF(LASBEM(ILEN-4:ILEN).EQ.'.ncm')THEN
        WRITE(BRUKFL,'(2A)')LASBEM(1:ILEN-4),'.inp'
      ELSE
        WRITE(BRUKFL,'(2a)')CFGROOT(1:LNBLNK(CFGROOT)),'.inp'
      ENDIF
      IUF=IFIL+1
      CALL EFOPSEQ(IUF,BRUKFL,3,IER)
      WRITE(IUF,'(A)',IOSTAT=IOS,ERR=3) '$'
      WRITE(IUF,'(A)',IOSTAT=IOS,ERR=3) 
     &'$ Compliance input file for BR PART L 2006'
      WRITE(IUF,'(a)',iostat=ios)  '$ ESP-r -> BRUKL 2.0.b'
      WRITE(IUF,'(A)',IOSTAT=IOS,ERR=3) '$'
      WRITE(IUF,'(A)',IOSTAT=IOS,ERR=3) '"BRUKL-PROJECT" = GENERAL'
      WRITE(IUF,'(4A)',IOSTAT=IOS,ERR=3)' P-NAME              = ',
     &'"',pjname(1:lnblnk(pjname)),' "'
      IF(IBUSERTYP.NE.0)THEN
        TMPSTR2=BTYPNAME(IBUSERTYP)
      ELSE
        TMPSTR2='Building type not defined (yet)'
      ENDIF
      WRITE(IUF,'(4A)',IOSTAT=IOS,ERR=3)' B-TYPE              = ',
     &'" ',TMPSTR2(1:LNBLNK(TMPSTR2)),' "'
      WRITE(IUF,'(4A)',IOSTAT=IOS,ERR=3)' B-ADDRESS           = ',
     &'"',BADDRESS(1:lnblnk(BADDRESS)),' "'
      WRITE(IUF,'(4A)',IOSTAT=IOS,ERR=3)' B-CITY              = ',
     &'"',BCITY(1:lnblnk(BCITY)),' "'
      WRITE(IUF,'(4A)',IOSTAT=IOS,ERR=3)' B-POSTCODE          = ',
     &'"',BUILDINGPC(1:lnblnk(BUILDINGPC)),' "'
      WRITE(IUF,'(4A)',IOSTAT=IOS,ERR=3)' O-NAME              = ',
     &'"',ownername(1:lnblnk(ownername)),' "'
      WRITE(IUF,'(4A)',IOSTAT=IOS,ERR=3)' O-TELEPHONE         = ',
     &'"',ownertel(1:lnblnk(ownertel)),' "'
      WRITE(IUF,'(4A)',IOSTAT=IOS,ERR=3)' O-ADDRESS           = ',
     &'"',owneraddr(1:lnblnk(owneraddr)),' "'
      WRITE(IUF,'(4A)',IOSTAT=IOS,ERR=3)' O-CITY              = ',
     &'"',ownercity(1:lnblnk(ownercity)),' "'
      WRITE(IUF,'(4A)',IOSTAT=IOS,ERR=3)' O-POSTCODE          = ',
     &'"',ownerPC(1:lnblnk(ownerPC)),' "'
      WRITE(IUF,'(4A)',IOSTAT=IOS,ERR=3)' C-NAME              = ',
     &'"',certifname(1:lnblnk(certifname)),' "'
      WRITE(IUF,'(4A)',IOSTAT=IOS,ERR=3)' C-TELEPHONE         = ',
     &'"',certTel(1:lnblnk(certTel)),' "'
      WRITE(IUF,'(4A)',IOSTAT=IOS,ERR=3)' C-ADDRESS           = ',
     &'"',certaddress(1:lnblnk(certaddress)),' "'
      WRITE(IUF,'(4A)',IOSTAT=IOS,ERR=3)' C-CITY              = ',
     &'"',certcity(1:lnblnk(certcity)),' "'
      WRITE(IUF,'(4A)',IOSTAT=IOS,ERR=3)' C-POSTCODE          = ',
     &'"',certPC(1:lnblnk(certPC)),' "'

C Calculation engine and interface detail...
      WRITE(IUF,'(2A)',IOSTAT=IOS,ERR=3)' CENGINE             = ',
     &'ESP-r'
      WRITE(IUF,'(6A)',IOSTAT=IOS,ERR=3)' CENGINE-VERSION     = ',
     &cBranch(1:lnblnk(cBranch)),'@r',
     &cRevision(1:lnblnk(cRevision)),' ',
     &cState(1:lnblnk(cState))
      WRITE(IUF,'(2A)',IOSTAT=IOS,ERR=3)' INTERFACE           = ',
     &'"ESP-r Project Manager"'
      WRITE(IUF,'(2A)',IOSTAT=IOS,ERR=3)' INTERFACE-VERSION   = ',
     &cRelease_num(1:lnblnk(cRelease_num))
      WRITE(IUF,'(a)',iostat=ios) ' ..'
      WRITE(IUF,'(2a)',iostat=ios) '"ESP-r" = ','COMPLIANCE'
      WRITE(IUF,'(2a)',iostat=ios) ' TYPE = ',
     &BLDREG(IRGG)(1:LNBLNK(BLDREG(IRGG)))
      WRITE(IUF,'(2a)',iostat=ios) ' BR-STAGE            = ',
     &BLDSTG(ISTG)(1:LNBLNK(BLDSTG(ISTG)))

C Enter Scottish building information if applicable
      IF(ISBT.NE.0)THEN
        WRITE(IUF,'(2a)',iostat=ios) ' BR-S6-BTYPE         = ',
     &  SBTYP(ISBT)(1:LNBLNK(SBTYP(ISBT)))
        WRITE(IUF,'(2a)',iostat=ios) ' BR-S6-ACCRED-CONST  = ',SBREF
      ENDIF
      WRITE(IUF,'(a)',iostat=ios) ' ..'

C CHECK1 detail...
      WRITE(IUF,'(a)',iostat=ios) '"CHECK1" = CHECK1'
      WRITE(IUF,'(a,1X,F5.1)',iostat=ios)' KG-CO2-NOT/M2       = ',
     &CO2NOTPERM2
      WRITE(IUF,'(a,1X,F5.1)',iostat=ios)' BER                 = ',BER
      WRITE(IUF,'(a,1X,F5.1)',iostat=ios)' TER                 = ',TER
      WRITE(IUF,'(a,1X,F4.2)',iostat=ios)' IF                  = ',
     &BLDIF(IBSS)
      WRITE(IUF,'(a,1X,F4.2)',iostat=ios)' RB                  = ',
     &BLDLZC(IBSS)
      WRITE(IUF,'(a)',iostat=ios) ' ..'

C CHECK2 detail...
      WRITE(IUF,'(a)',iostat=ios) '"CHECK2" = CHECK2'
      zuvmax = 0.0
      uvwall = 0.0
      uvmax = 0.0   
      icomp = 0
      uavwsum = 0.0
      uvwaver = 0.0
      vexposed = 0.0
      extopvert = 0

C variables for floors
      zuflmax = 0.0
      ufloor = 0.0
      uflmax = 0.0 
      uaflsum = 0.0
      uflaver = 0.0
      fexposed = 0.0

C variables for floors
      zuroofmax = 0.0
      uroof = 0.0
      uroofmax = 0.0 
      uaroofsum = 0.0
      uroofaver = 0.0
      rexposed = 0.0
       
      INPIC=NCOMP
      do 14 mz=1,inpic
        context=.true.
        CALL SURINFO(ICOMP,iuout,context) 
        bndry=.true.

        DO 2243 IS=1,NZSUR(mz)
          icomp = icomp+1
          icc=IZSTOCN(mz,is)
          TMPSTR=SSOTHER(ICC,1)
          if(ICT(icc).eq.0.and.TMPSTR(1:5).ne.'EXTER')bndry=.false.
          if(ICT(icc).eq.1.and.TMPSTR(1:5).ne.'SIMIL')bndry=.false.
          if(ICT(icc).eq.2.and.TMPSTR(1:5).ne.'CONST')bndry=.false.
          if(ICT(icc).eq.3.and.TMPSTR(1:5).eq.'CONST')bndry=.false.
          if(ICT(icc).eq.3.and.TMPSTR(1:5).eq.'SIMIL')bndry=.false.
          if(ICT(icc).eq.3.and.TMPSTR(1:5).eq.'EXTER')bndry=.false.
          if(ICT(icc).eq.3.and.TMPSTR(1:5).eq.'GROUN')bndry=.false.
          if(ICT(icc).eq.4.and.TMPSTR(1:5).ne.'GROUN')bndry=.false.
          if(ICT(icc).eq.5.and.TMPSTR(1:5).ne.'ADIAB')bndry=.false.
          if(ICT(icc).eq.6.and.TMPSTR(1:5).ne.'BASES')bndry=.false.
          if(ICT(icc).eq.0.and.TMPSTR(1:8).eq.'EXTERIOR')then 

C At the moment only the extrior surfaces are filtered. This has to
C be extended to all non-heated/cooled (non-conditioned) spaces 
C A way to separate doors is also needed
            if(SSOTF(icc)(1:4).eq.'OPAQ')then
              if(SSVFC(icc)(1:4).eq.'VERT')then

C Count the number of external opaque vertical surfaces to use for
C the Uaver calculations
C                extopvert = extopvert + 1

C Check the U value for each external surface.
                UVH = 0.0
                do 1511 ii=1,nmlc
                  if(SSMLCN(icc)(1:12).eq.DESC(ii)(1:12)) then

C Recover the ISO 6946 U values as in prjqa.F.
                    call etmldbu(0,itu,ii,UVH,UVU,UVD,UVI)
                  else
                    continue
                  endif
 1511           continue
           
C For vertical walls assume horizontal hc coef. 
C Add surface areas
                vexposed = vexposed + SSNA(icc)
                uvwall = UVH
                uavwsum = uavwsum + (SSNA(icc) * UVH)
                if(uvwall.gt.uvmax)then
                  uvmax = uvwall
                  SN = SSNAME(icc) 
                else
                endif

C Is the extra logic for sloped roofs needed here? They have
C stricter U value requirements
              elseif(SSVFC(icc)(1:4).eq.'CEIL'.or.
     &               SSVFC(icc)(1:4).eq.'SLOP')then
     
C Check the U value for each external surface.
                UVU = 0.0
                do 1513 ii=1,nmlc
                  if(SSMLCN(icc)(1:12).eq.DESC(ii)(1:12)) then

C Recover the ISO 6946 U values as in prjqa.F.
                    call etmldbu(0,itu,ii,UVH,UVU,UVD,UVI)
                  else
                    continue
                  endif
 1513           continue
           
C For roofs assume upward heat flow
C Add surface areas
                rexposed = rexposed + SSNA(icc)
                uroof = UVU
                uaroofsum = uaroofsum + (SSNA(icc) * UVU)
                if(uroof.gt.uroofmax)then
                  uroofmax = uroof
                  RN = SSNAME(icc) 
                else
                endif
              else
                continue 
              endif
            elseif(SSOTF(icc)(1:4).ne.'OPAQ')then

C Check the U value for each external window.
              UVH = 0.0
              UVU = 0.0
              do 1510 ii=1,nmlc
                if(SSMLCN(icc)(1:12).eq.DESC(ii)(1:12)) then

C Recover the ISO 6946 U values as in prjqa.F.
                  call etmldbu(0,itu,ii,UVH,UVU,UVD,UVI)
                else
                  continue
                endif
 1510         continue
           
C For vertical walls assume horizontal hc coef. 
C Add surface areas
              trexposed = trexposed + SSNA(icc)
              if(SSVFC(icc)(1:4).eq.'VERT')then
                utran = UVH
                uatrsum = uatrsum + (SSNA(icc) * UVH)
              elseif(SSVFC(icc)(1:4).eq.'CEIL'.or.
     &          SSVFC(icc)(1:4).eq.'SLOP')then
                utran = UVU
                uatrsum = uatrsum + (SSNA(icc) * UVU)
              else         
              endif
              if(utran.gt.utrmax)then
                utrmax = utran
                TRN = SSNAME(icc) 
              else
              endif
            else    
            endif 

C Check for floors (Umax, Uaver and name is again needed)
          elseif(ICT(icc).eq.3.or.ICT(icc).eq.4)then
            if(SSOTHER(icc,1)(1:6).eq.'GROUND')then
              if(SSVFC(icc)(1:4).eq.'FLOR')then
                UVD = 0.0    
                do 1512 ii=1,nmlc
                  if(SSMLCN(icc)(1:12).eq.DESC(ii)(1:12)) then

C Recover the ISO 6946 U values as in prjqa.F.
                    call etmldbu(0,itu,ii,UVH,UVU,UVD,UVI)
                  else
                    continue
                  endif
 1512           continue

C For floors assume downward heat flow 
C Add floor areas
                fexposed = fexposed + SSNA(icc)
                ufloor = UVD
                uaflsum = uaflsum + (SSNA(icc) * UVD)
                if(ufloor.gt.uflmax)then
                  uflmax = ufloor
                  FN = SSNAME(icc) 
                else
                endif
              endif
            endif 
          else
            continue
          endif
 2243   continue  

C Get max U value for walls (after scanning all zones) and the name of the surface
        if(uvmax.gt.zuvmax)then
          zuvmax= uvmax
          ZSN= SN
        else
        endif

C Get max U value for floors (after scanning all zones) and the name of the surface
        if(uflmax.gt.zuflmax)then
          zuflmax= uflmax
          ZFN= FN
        else
        endif

C Get max U value for roofs (after scanning all zones) and the name of the surface        
        if(uroofmax.gt.zuroofmax)then
          zuroofmax= uroofmax
          ZRN= RN
        else
        endif

C Get max U value for tranparent surfaces (after scanning all zones) and the name of the surface
        if(utrmax.gt.zutrmax)then
          zutrmax= utrmax
          ZTRN= TRN
        else
        endif

  14  continue
   
C average area weighted U value
      if(vexposed.gt.0.0)then
        uvwaver = uavwsum / vexposed
      else
        uvwaver = 0.0
      endif
      if(fexposed.gt.0.0)then
        uflaver = uaflsum / fexposed   
      else
        uflaver = 0.0
      endif
      if(rexposed.gt.0.0)then
        uroofaver = uaroofsum / rexposed   
      else
        uroofaver = 0.0
      endif
      if(trexposed.gt.0.0)then
        utraver = uatrsum / trexposed   
      else
        utraver = 0.0
      endif
      WRITE(IUF,'(a,f6.3)',iostat=ios)' WALL-U-AVE          = ',
     & uvwaver
      WRITE(IUF,'(a,f6.3)',iostat=ios)' WALL-U-MAX          = ',
     & zuvmax


C Identify the wall which has the maximum individual U value
      if(zuvmax.gt.0.0)then
        WRITE(IUF,'(2a)',iostat=ios)' WALL-MAX            = ',
     &       ZSN(1:lnblnk(ZSN))
      else     
        WRITE(IUF,'(2a)',iostat=ios)' WALL-MAX            = ',
     &    '"No external walls in project"'
      endif

C write out info for floors
      WRITE(IUF,'(a,f6.3)',iostat=ios)' FLOOR-U-AVE         = ',
     &    uflaver
      WRITE(IUF,'(a,f6.3)',iostat=ios)' FLOOR-U-MAX         = ',
     &    zuflmax


C Identify the floor which has the maximum individual U value
      if(zuflmax.gt.0.0)then
        WRITE(IUF,'(2a)',iostat=ios)' FLOOR-MAX           = ',
     &    ZFN(1:lnblnk(ZFN))
      else     
        WRITE(IUF,'(2a)',iostat=ios)' FLOOR-MAX           = ',
     &    '"No floors in project"'
      endif


C write out info for roofs
      WRITE(IUF,'(a,f6.3)',iostat=ios)' ROOF-U-AVE          = ',
     &    uroofaver
      WRITE(IUF,'(a,f6.3)',iostat=ios)' ROOF-U-MAX          = ',
     &    zuroofmax


C Identify the roof which has the maximum individual U value
      if(zuroofmax.gt.0.0)then
        WRITE(IUF,'(2a)',iostat=ios)' ROOF-MAX            = ',
     &    ZRN(1:lnblnk(ZRN))
      else     
        WRITE(IUF,'(2a)',iostat=ios)' ROOF-MAX            = ',
     &    '"No external roofs in project"'
      endif

C write out info for windows and rooflights
      WRITE(IUF,'(a,f6.3)',iostat=ios)' WINROOF-U-AVE       = ',
     &    utraver
      WRITE(IUF,'(a,f6.3)',iostat=ios)' WINROOF-U-MAX       = ',
     &    zutrmax


C Identify the window or rooflight which has the maximum individual U value
      if(zuroofmax.gt.0.0)then
        WRITE(IUF,'(2a)',iostat=ios)' WINROOF-MAX         = ',
     &    ZTRN(1:lnblnk(ZTRN))
      else     
        WRITE(IUF,'(2a)',iostat=ios)' WINROOF-MAX         = ',
     &    '"No external windows or rooflights in project"'
      endif

C For the moment use this:
      WRITE(IUF,'(2a)',iostat=ios)' P-DOOR-U-AVE        = ',
     &  ' 2'  

C For the moment use this:
      WRITE(IUF,'(2a)',iostat=ios)' P-DOOR-U-MAX        = ',
     &  ' 2' 

C For the moment use this:
      WRITE(IUF,'(2a)',iostat=ios)' P-DOOR-MAX          = ',
     &  ' espr_pDoor' 

C For the moment use this:
      WRITE(IUF,'(2a)',iostat=ios)' V-DOOR-U-AVE        = ',
     &  ' 2'  

C For the moment use this:
      WRITE(IUF,'(2a)',iostat=ios)' V-DOOR-U-MAX        = ',
     &  ' 0' 

C For the moment use this:
      WRITE(IUF,'(2a)',iostat=ios)' V-DOOR-MAX          = ',
     &  '"No vehicle access doors"' 

C For the moment use this:
      WRITE(IUF,'(2a)',iostat=ios)' H-DOOR-U-AVE        = ',
     &  ' 0'  

C For the moment use this:
      WRITE(IUF,'(2a)',iostat=ios)' H-DOOR-U-MAX        = ',
     &  ' 0' 

C For the moment use this:
      WRITE(IUF,'(2a)',iostat=ios)' H-DOOR-MAX          = ',
     &  '"No high usage doors"' 

C For the moment use this:
      WRITE(IUF,'(2a)',iostat=ios)' S-VENT-U-AVE        = ',
     &  ' 0'  

C For the moment use this:
      WRITE(IUF,'(2a)',iostat=ios)' S-VENT-U-MAX        = ',
     &  ' 0' 

C For the moment use this:
      WRITE(IUF,'(2a)',iostat=ios)' S-VENT-MAX          = ',
     &  '"No roof ventilators"'

C Write infiltration and permeability information
      WRITE(IUF,'(a,F5.2)',iostat=ios)' Q50-INF             = ',BINF50
      WRITE(IUF,'(2a)',iostat=ios)' Q50-INF-CHECK       = ',APCHK
      WRITE(IUF,'(a)',iostat=ios) ' ..'

C DHW checks 
      DO 40 IDHWSYS=1,NDHWSYS
        TMPSTR2=DHWNAME(IDHWSYS)
        write(iuf,'(3a)',iostat=ios)'"',TMPSTR2(1:LNBLNK(TMPSTR2)),
     &  '" = CHECK2-DHW'
        write(iuf,'(a,I5)',iostat=ios)' HVACGUIDE-DHW       = ',
     &  IBRUKW(IDHWSYS)
        write(iuf,'(a,F6.3)',iostat=ios)' DHW-ACT-EFF         = ',
     &  HWEF(IDHWSYS)
        WRITE(IUF,'(a)',iostat=ios) ' ..'
 40   CONTINUE

C Write information about systems
      DO 20  ICMSYS=1,NCMSYS
        TMPSTR2=HVACNAME(ICMSYS) 
        WRITE(IUF,'(3a)',iostat=ios)'"',TMPSTR2(1:LNBLNK(TMPSTR2)),
     &  '" = CHECK2-11'
        WRITE(IUF,'(a)',iostat=ios)' $HG-TYPE             = 46'
        WRITE(IUF,'(a,F6.3)',iostat=ios)' HEAT-GEN-SEFF       = ',
     &  HGEF(ICMSYS)
        TMPREAL=CGEF(ICMSYS)
        CALL ECLOSE(TMPREAL,0.0,0.01,ISNEAR)
        IF(ISNEAR)TMPREAL=0.1
        WRITE(IUF,'(a,F6.3)',iostat=ios)' COOL-GEN-SEER       = ',
     &  TMPREAL
        IBRUK=IBRUKLH(IFTYP(ICMSYS),IHGEF(ICMSYS),INCMSYS(ICMSYS))
        IF(IBRUK.LE.0)IBRUK=-5555
        WRITE(IUF,'(a,I5)',iostat=ios)' HVACGUIDE-HEAT      = ',IBRUK
        IBRUK=IBRUKLC(ICGEF(ICMSYS),INCMSYS(ICMSYS))
        IF(IBRUK.LE.0)IBRUK=-5555
        IF(INCMSYS(ICMSYS).LE.10)IBRUK=-1111
        WRITE(IUF,'(a,I5)',iostat=ios)' HVACGUIDE-COOL      = ',IBRUK
        WRITE(IUF,'(A,I5)',iostat=ios)' HVACGUIDE-SFP       = ',
     &  IBRUKLF(INCMSYS(ICMSYS))
        TMPREAL=SFPHS(ICMSYS)
        CALL ECLOSE(TMPREAL,0.0,0.01,ISNEAR)
        IF(ISNEAR)THEN
          WRITE(IUF,'(a)',iostat=ios)' SFP-ACT             = -6666'
        ELSE
          WRITE(IUF,'(a,F6.3)',iostat=ios)' SFP-ACT             = ',
     &    TMPREAL
        ENDIF
        WRITE(IUF,'(a)',iostat=ios) ' ..'
 20   CONTINUE

C Write remainder of the check information
      WRITE(IUF,'(a)',iostat=ios) '"CHECK3" = CHECK3'
      WRITE(IUF,'(a)',iostat=ios) ' ..'

      WRITE(IUF,'(a)',iostat=ios) '"CHECK4" = CHECK4'
      WRITE(IUF,'(a,F5.2)',iostat=ios) ' Q50-INF              = ',
     &  BINF50
      WRITE(IUF,'(a)',iostat=ios) ' ..'

      WRITE(IUF,'(a)',iostat=ios) '"CHECK5" = CHECK5'
      WRITE(IUF,'(a)',iostat=ios) ' ..'

      WRITE(IUF,'(a)',iostat=ios) '"CHECK6" = CHECK6'
      WRITE(IUF,'(a)',iostat=ios) ' ..'

      call edisp(iuout,'Completed export of data to BRUKL inp')
      CALL ERPFREE(IUF,ISTAT)
      RETURN
 3    CONTINUE ! error message to go here
      END
