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

C Note: when reading the SBEM database the code assumes that
C the tags in the file are in a specific order.

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
      IF(OUTSTR(1:5).NE.'*sbem')then
        CONTINUE ! WARNING MSG TO GO HERE
      endif
      CALL STRIPC(IUF,OUTSTR,99,ND,1,'SBEM db',IER)
      K=9
      CALL EGETWR(OUTSTR,K,VERSBEM,1.,2.,'W','Version no.',IER)
      CALL STRIPC(IUF,OUTSTR,99,ND,1,'SBEM db',IER)
      K=12
      CALL EGETWR(OUTSTR,K,SFPDEF,0.,20.,'W','Version no.',IER)

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
        write(6,*)fuelname(ifuel)!,fuelCO2(ifuel)
        write(6,*)'SBEM db read start'
      ENDIF
      GOTO 1

C Now read system types, default efficiencies & applicable HVAC systems
  2   CALL STRIPC(IUF,OUTSTR,99,ND,1,'SBEM db',IER)
      IF(OUTSTR(1:19).NE.'*system_types_start')then
        CONTINUE ! WARNING MEG TO GO HERE
      endif
  3   ISYS=ISYS+1
      SYSNAM=" "
      ILEN=1
      CALL STRIPC(IUF,OUTSTR,99,ND,1,'SBEM db',IER)
      IF(OUTSTR(1:17).EQ.'*end_system_types')GOTO 4
      K=0
      CALL EGETWI(OUTSTR,K,NIT,0,0,'-','system index no.',IER)
      CALL EGETWI(OUTSTR,K,NIT,0,0,'-','nb items',IER)
      DO 200 IIT=1,NIT
          CALL EGETW(OUTSTR,K,WORD,'W','system name',IER)
          WRITE(SYSNAM,'(A,1X,A)')SYSNAM(1:ILEN),
     &      WORD(1:lnblnk(WORD))
          ILEN=LNBLNK(WORD)+ILEN+1
 200  CONTINUE
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
      IF(OUTSTR(1:18).NE.'*HVAC_system_start')then
        CONTINUE ! WARNING MEG TO GO HERE
      endif
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
      IF(OUTSTR(1:16).NE.'*bldg_type_start')then
        CONTINUE ! WARNING MsG TO GO HERE
      endif
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

C Debug...
      IF(IDEBUG.EQ.1)THEN
        write(6,*)ibt,bldif(ibt),bldlzc(ibt),bldss(ibt)
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

C Debug...
      IF(IDEBUG.EQ.1)THEN
        write(6,*)ibt,bldreg(ibt)
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
      IF(OUTSTR(1:23).NE.'*activities_by_building')then
        CONTINUE ! WARNING MsG TO GO HERE   
      endif   
      IAT=1
 17   CALL STRIPC(IUF,OUTSTR,99,ND,1,'Activity types',IER)
      IF(OUTSTR(1:19).EQ.'*end_activities_by_')GOTO 18
      K=0
      CALL EGETWI(OUTSTR,K,NTSTR,0,0,'-','no. text strings',IER)
      CALL EGETWI(OUTSTR,K,NCBTP,0,0,'-','crrspnd bld typ',IER)
      ILEN=1
      DO 100 ITSTR=1,NTSTR
        CALL EGETW(OUTSTR,K,WORD,'W','system name',IER)
        WRITE(SYSNAM,'(A,1X,A)')SYSNAM(1:ILEN),
     &    WORD(1:lnblnk(WORD))
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

 20   CALL ERPFREE(IUF,ISTAT)
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
        CALL EGETWI(OUTSTR,K,IACTYTYP(IZ),0,0,'-',
     &  'zone-activity links',IER)
 204  CONTINUE
      CALL STRIPC(IUF,OUTSTR,99,ND,1,'lighting type zone links',IER)
      DO 205 IZ=1,NCOMP
        CALL STRIPC(IUF,OUTSTR,99,ND,1,'Zone-lighting type linkage',IER)
        K=52
        CALL EGETWI(OUTSTR,K,ILITYP(IZ),0,0,'-',
     &  'zone-light type links',IER)
 205  CONTINUE
      
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
      IF(IBUSERTYP.NE.0)THEN
        K=0
        TMPSTR=BTYPNAME(ibusertyp)
        CALL EGETRM(TMPSTR,K,BTYPNAME(ibusertyp),'W','NCM file',IER)
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
          IF(INCMSYS(ICMSYS).LE.10)IBRUK=-1111
          WRITE(IUF,'(1X,I5,1X,A)',IOSTAT=IOS,ERR=3)
     &      IBRUK,' # BRUKL cool generator index number'

          WRITE(IUF,'(1X,F6.3,1X,A)',IOSTAT=IOS,ERR=3)SFPHS(ICMSYS),
     &      ' # Specific fan power W/l/s (0 if fans N/A)'
          WRITE(IUF,'(1X,I5,1X,A)',IOSTAT=IOS,ERR=3)
     &      IBRUKLF(INCMSYS(ICMSYS)),' # BRUKL fan index number'
        else
          IBRUK=-1111
          WRITE(IUF,'(1X,I5,1X,A)',IOSTAT=IOS,ERR=3)
     &      IBRUK,' # BRUKL cool generator index number'
          WRITE(IUF,'(1X,A)',IOSTAT=IOS,ERR=3)
     &      ' 0.0 # Specific fan power W/l/s (0 if fans N/A)'
          WRITE(IUF,'(1X,A)',IOSTAT=IOS,ERR=3)
     &      ' 0.0 # BRUKL fan index number'
        endif
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
        if(IACTYTYP(IZ).gt.0)then
          WRITE(IUF,'(3A,1X,I4,1X,A)') 'Zone ',zname(iz),
     &      ' is linked to activity number ',IACTYTYP(IZ),
     &      atypname(IACTYTYP(IZ))
        else
          WRITE(IUF,'(3A)') 'Zone ',zname(iz),
     &      ' is linked to activity number     0 (undefined activity)'
        endif
 105  CONTINUE

C Write building zone to lighting type linkage information
      WRITE(IUF,'(A)',IOSTAT=IOS,ERR=3)'*Zone to lighting type linkage'
      DO 106 IZ=1,NCOMP
        if(ILITYP(IZ).gt.0)then
          WRITE(IUF,'(3A,1X,I4,1X,A)') 'Zone ',zname(iz),
     &      ' is linked to lighting type number ',ILITYP(IZ),
     &      TLIGHT(ILITYP(IZ))
        else
          WRITE(IUF,'(3A)') 'Zone ',zname(iz),
     &      ' is linked to lighting type number     0 (undefined type)'
        endif
 106  CONTINUE
      CALL ERPFREE(IUF,ISTAT)
      RETURN
 3    CALL USRMSG('Problem writing data to sbem file',' ','W')
      END
