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
C  SBMEDIT  define high level data associated with UK SBEM methodology.
C  EMKSBM   creates a model variant based on the SBEM methodology. 
C  ENLARGE  performs geometric enlargement (contraction) of a surface.
C  NCMCAL   checks models, simulates notional and original model and
C           generates UK NCM data in the form of the BRUKL input file

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
      DIMENSION ITEMS(33),ITEMX(9),KSYS(MHT),ITEMH(MCOM+4),PRSSN(MPT)
      DIMENSION KSTR(9),ITHND(MPT),HSYSNM(MCOM+1),JSTR(8),LSYS(10)
      character outs*124,outs248*248,h*72
      character HEAD*25,ITEMS*50,D64*64,ITEMX*50,ITEMH*70,PRSSN*60
      CHARACTER HSYSNM*70,KEY*1,zname*12,zdesc*64,TMPSTR*64
      character*35 ZITEMS(MCOM+4)
      real Air_infilt
      real light_input_watt !user input for heat gains from lights
                                  !instead of using the values from the sbem database
      
C place (temporary?) call to read sbem db
      call sbempr(ier)

C Check if sbem project specific file exists if yes then read in details
C from it.
      IF(ISBEM.EQ.1)THEN
        CALL RSBEM
      ELSE

C Ask about building, owner and certifier detail. Note that
C the initial call to clrprb will have cleared pjname, baddress,
C bcity, buildingPC, ownername, ownertel, owneraddr, ownercity
C and ownerPC. It will also clear certifname, certTel, certaddress
C and certPC.
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
      ITEMX(8) = 'Link lighting systems and zones'
      ITEMX(9) = 'Display information '
      IX=1
      CALL EPICKS(IX,KSTR,' ','Define UK NCM data',
     &  50,9,ITEMX,'Define UK NCM data',IER,2)
      IF(KSTR(1).EQ.1)THEN

C Enter building, owner and vertifier information      
  15    CALL EDISP(IUOUT,
     &  'Enter/Confirm building, owner and certifier details')
        INO=-1
        ITEMS(1)  = 'a Project name: '
        write(ITEMS(2),'(2A)') '  ',pjname(1:48)
        ITEMS(3)  = 'b Building address: '
        write(ITEMS(4),'(2A)') '  ',baddress(1:48)
        ITEMS(5)  = 'c Building city: '
        write(ITEMS(6),'(2A)') '  ',bcity(1:48)     
        ITEMS(7)  = 'd Building Postcode: '
        write(ITEMS(8),'(2A)') '  ',buildingPC(1:48) 
        ITEMS(9)  = 'e Building owner name: '
        write(ITEMS(10),'(2A)') '  ',ownername(1:48)
        ITEMS(11) = 'f Building owner telephone: '
        write(ITEMS(12),'(2A)') '  ',ownertel(1:48)
        ITEMS(13) = 'g Building owner address: '
        write(ITEMS(14),'(2A)') '  ',owneraddr(1:48)
        ITEMS(15) = 'h Building owner city: '
        write(ITEMS(16),'(2A)') '  ',ownercity(1:48)
        ITEMS(17) = 'i Building owner Postcode: '
        write(ITEMS(18),'(2A)') '  ',ownerPC(1:48)
        ITEMS(19) = 'j Certifier name: '
        write(ITEMS(20),'(2A)') '  ',certifname(1:48)
        ITEMS(21) = 'k Certifier telephone: '
        write(ITEMS(22),'(2A)') '  ',certTel(1:48)
        ITEMS(23) = 'l Certifier address: '
        write(ITEMS(24),'(2A)') '  ',certaddress(1:48)
        ITEMS(25) = 'm Certifier city: '
        write(ITEMS(26),'(2A)') '  ',certcity(1:48)
        ITEMS(27) = 'n Certifier postcode: '
        write(ITEMS(28),'(2A)') '  ',certPC(1:48)
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
          call tstamp('>','PRJ: edited project name')
        ELSEIF(INO.EQ.3)THEN
  
C Building/owner/certifier data description
          H(1)='Input a succinct description of building address here'
          CALL EASKS(baddress,'building address ?','(<64 characters)',
     &           64,D64,'build address',IER,1)
          call tstamp('>','PRJ: edited project address')
        ELSEIF(INO.EQ.5)THEN
  
C Building/owner/certifier data description
          H(1)='Input a succinct description of the building city here'
          CALL EASKS(bcity,'building city ?','(<64 characters)',
     &           64,D64,'city building',IER,1)
          call tstamp('>','PRJ: edited project city')
        ELSEIF(INO.EQ.7)THEN
  
C Building/owner/certifier data description
          H(1)='Input for: building post code'
          CALL EASKS(buildingPC,'building post code ?',
     &    '(<64 characters)',64,D64,'building PC',IER,1)
          call tstamp('>','PRJ: edited project post code')
        ELSEIF(INO.EQ.9)THEN

C Building/owner/certifier data description
          H(1)='Input a succinct description of the owner name here'
          CALL EASKS(ownername,'Name of owner ?','(<64 characters)',
     &           64,D64,'owner name',IER,1)
          call tstamp('>','PRJ: edited project owner name')
        ELSEIF(INO.EQ.11)THEN
  
C Building/owner/certifier data description
          H(1)='telephone number of the owner'
          CALL EASKS(ownertel,'Telephone of owner ?','(<64 characters)',
     &           64,D64,'owner tel',IER,1)
          call tstamp('>','PRJ: edited project owner phone')
        ELSEIF(INO.EQ.13)THEN
  
C Building/owner/certifier data description
          H(1)='Address of the owner'
          CALL EASKS(owneraddr,'Address of owner ?','(<64 characters)',
     &           64,D64,'owner address',IER,1)
          call tstamp('>','PRJ: edited project owner address')
        ELSEIF(INO.EQ.15)THEN
  
C Building/owner/certifier data description
          H(1)='City (for owner)'
          CALL EASKS(ownercity,'City of owner ?','(<64 characters)',
     &           64,D64,'owner city',IER,1)
          call tstamp('>','PRJ: edited project owner city')
        ELSEIF(INO.EQ.17)THEN
  
C Building/owner/certifier data description
          H(1)='Post code (for owner)'
          CALL EASKS(ownerPC,'Post code for owner ?','(<64 characters)',
     &           64,D64,'owner post code',IER,1)
          call tstamp('>','PRJ: edited project owner postcode')
        ELSEIF(INO.EQ.19)THEN
  
C Building/owner/certifier data description
          H(1)='cetrifier name'
          CALL EASKS(certifname,'Name of certifier ?','(<64 characters)'
     &           ,64,D64,'certifier name',IER,1)
          call tstamp('>','PRJ: edited project certifier')
        ELSEIF(INO.EQ.21)THEN 
      
C Building/owner/certifier data description
          H(1)='cetrifier telephone'
          CALL EASKS(certTel,'Teleph. of certifier ?','(<64 characters)'
     &           ,64,D64,'certifier tel',IER,1)
          call tstamp('>','PRJ: edited project certifier phone')
        ELSEIF(INO.EQ.23)THEN 
      
C Building/owner/certifier data description
          H(1)='certifier address'
          CALL EASKS(certaddress,'Address of certifier ?',
     &    '(<64 characters)',64,D64,'certifier address',IER,1)
          call tstamp('>','PRJ: edited project cert address')
        ELSEIF(INO.EQ.25)THEN 
      
C Building/owner/certifier data description
          H(1)='cetrifier city'
          CALL EASKS(certcity,'City of certifier ?','(<64 characters)',
     &           64,D64,'certifier city',IER,1)
          call tstamp('>','PRJ: edited project certifier city')
        ELSEIF(INO.EQ.27)THEN 
      
C Building/owner/certifier data description
          H(1)='cetrifier post code'
          CALL EASKS(certPC,'Post code of certifier ?',
     &    '(<64 characters)',64,D64,'certifier Post code',IER,1)
          call tstamp('>','PRJ: edited project cert post code')
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
          write(outs,'(2a)')'Project name: ',pjname(1:lnblnk(pjname))
          call edisp(iuout,outs)
          write(outs,'(2a)')'Building address: ',baddress
          call edisp(iuout,outs)
          write(outs,'(2a)')'Building city: ',bcity(1:lnblnk(bcity)) 
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
     &    40,MSS,BLDSS,'Define building services strategy',IER,2)
        IF(JSTR(1).EQ.0)GOTO 901
        IF(JSTR(1).LE.3.AND.JSTR(1).GE.1)IBSS=JSTR(1)        

C Enter building regulations to be used
        IX=1
        JSTR(1)=IRGG
 902    CALL EPICKS(IX,JSTR,' ','Define building regulations to use',
     &    50,MREG,BLDREG,'Define building services strategy',IER,2)
        IF(JSTR(1).EQ.0)GOTO 902
        IF(JSTR(1).LE.4.AND.JSTR(1).GE.1)IRGG=JSTR(1)        

C Enter bulding design stage
        IX=1
        JSTR(1)=ISTG
 903    CALL EPICKS(IX,JSTR,' ','Define building regulations to use',
     &    12,MSTG,BLDSTG,'Define building services strategy',IER,2)
        IF(JSTR(1).EQ.0)GOTO 903
        IF(JSTR(1).LE.2.AND.JSTR(1).GE.1)ISTG=JSTR(1)        

C Enter Scottish building types if Scottish regs to be used
        IF(IRGG.EQ.2)THEN
          IX=1
          JSTR(1)=ISBT
 904      CALL EPICKS(IX,JSTR,' ','Define building type',
     &      40,MSBT,SBTYP,'Define building type',IER,2)
          IF(JSTR(1).EQ.0)GOTO 904
          IF(JSTR(1).LE.2.AND.JSTR(1).GE.1)ISBT=JSTR(1)        
          CALL EASKABC('Is the building built/designed following',
     &      'guidance in Accredited Construction Details (Scotland)',
     &      'yes','no','Do not know',IW,2)
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
     &    'Enter building permeability at 50Pa in m3/m2.hour',
     &    ' ',0.,'F',0.,'-',10.,'heat gen eff ',IER,2)
        CALL EASKABC(
     &  'Is a compliance check to be performed on the building',
     &  'regarding air permeability value?',
     &  'yes','no','Do not know',IW,2)
        IF(IW.EQ.2)THEN
          APCHK='NO '
        ELSE
          APCHK='YES'
        ENDIF
        
C Enter air changes per hour (or) m3/s for every zone as air leakage
C This will be added to the ventilation rates taken from the tdf (as
C minimum fresh air requirements from the activities database)
C Begin with high level menu.
    3   INO=-4
        IER=0
        DO 111 IZ=1,NCOMP
          CALL EMKEY(IZ,KEY,IER)
          WRITE(ZITEMS(IZ),'(A1,1X,A,F6.2)')KEY,
     &         zname(IZ)(1:lnblnk(zname(IZ))),ACH(IZ)
 111    CONTINUE 
        M=NCOMP
        ZITEMS(M+1)= ' ______________________________ '
        ZITEMS(M+2)= '* copy (from operation files): NA'
        ZITEMS(M+3)= '? help                           '
        ZITEMS(M+4)= '- exit this menu                 '
        nitms=M+4

C Help text for this menu.
        H(1)='Infiltration rates and air leakage can be specified for'
        H(2)='for every zone in terms of air changes per hour. These'      
        H(3)='can be also copied from the operation files.'   
        
        CALL EMENU('zone air changes',ZITEMS,nitms,INO) 
        IF(INO.EQ.nitms)THEN
          continue
        ELSEIF(INO.EQ.nitms-1)THEN          

C Produce help text for the menu.
          CALL PHELPD('AIR LEAKAGE',3,'-',0,0,IER)
          goto 3
        ELSEIF(INO.EQ.nitms-2)THEN

C Need logic here to copy ach details from the existing operations -
C but an average 24h value   
          write(outs,'(A)') ' This facility is currently unavailable'
          call edisp(iuout,outs)
          goto 3
        ELSEIF(INO.GE.1.AND.INO.LT.nitms-3)THEN
          Air_infilt=0.0
          CALL EASKR(Air_infilt,' ','air changes for this zone?',
     &       0.0,'W',30.0,'W',0.0,'air changes for the zone',IER,0)  
          ACH(INO)=Air_infilt
          goto 3
        ENDIF

C Use existing building type if non-zero.
        if(ibusertyp.ne.0)then
          write(outs,'(a,i2,2a)') 'The current building type is ',
     &      ibusertyp,BTYPNAME(ibusertyp),'.'
          call edisp(iuout,outs)
        else
          write(outs,'(2a)') 'The current building use is undefined',
     &    'and the NCM guide table 4 is also undefined.'
          call edisp(iuout,outs)
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

C After setting up echo back what was selected.
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

C Link lighting systems with zones
        DO 160 IZ=1,NCOMP
            ILIGHTUSER(IZ)=0
          CALL EASKABC('Choose one of the options for defining lighting
     &heat gains from lights for: ',zname(IZ)(1:lnblnk(zname(IZ))),
     &      'user defined W/m2 per 100lux','pick from list','continue',
     &      Iwaylight,2)
          IF(Iwaylight.EQ.1)THEN

C flag needed for the casual.F to account for lighting gains
C depending on the way they are specified by the user.
            ILIGHTUSER(IZ)=1
            
C If users pick this then they have to demonstrate that this
C value is a realistic one
            light_input_watt=0.0
            CALL EASKR(light_input_watt,
     &      'Enter heat gains from lights in watts (assuming 100% on)
     &for zone',zname(IZ)(1:lnblnk(zname(IZ))),0.,'F',0.,'-',
     &      LIGHTWATTAGE(IZ),'light heat gains ',IER,2)          
            LIGHTWATTAGE(IZ)=light_input_watt
          ELSE
            WRITE(OUTS,'(2A)')'Choose lighting type for zone ',
     &      zname(IZ)(1:lnblnk(zname(IZ)))
            CALL EPICKS(IY,LSYS,' ',OUTS(1:LNBLNK(OUTS)),
     &      70,MILS,TLIGHT,OUTS(1:LNBLNK(OUTS)),IER,2)
            ILITYP(IZ)=LSYS(1)
          ENDIF
 160    CONTINUE
      ELSEIF(KSTR(1).EQ.9)THEN

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
#include "esprdbfile.h"
      common/OUTIN/IUOUT,IUIN
      common/pophelp/h(60)

      COMMON/RAY3/MODIFY,MODLEN,MODBND
      COMMON/gzonpik/izgfoc,nzg,nznog(mcom)
      COMMON/RAY6/LINSTY(MCON)

      COMMON/C1/NCOMP,NCON

C << G1 is probably not the common block to use for this >>
      COMMON/G1/X(MTV),Y(MTV),Z(MTV),NSUR,JVN(MS,MV),NVER(MS),NTV
      COMMON/G5/SNAME(MCOM,MS),SOTF(MS),SMLCN(MS),SVFC(MS),SOTHER(MS,3),
     &          SUSE(MS,2),SPARENT(MS)

      common/C2/LSNAM,NCCODE(MCOM),LPROJ(MCOM),LGEOM(MCOM),
     &          LSHAD(MCOM),LTHRM(MCOM),INDUTL(MCOM),LUTIL(MCOM)
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
      COMMON/G2/NGL(MS)
      COMMON/G6/SSNAME(MCON),SSOTF(MCON),SSMLCN(MCON),SSVFC(MCON),
     &          SSOTHER(MCON,3),SSPARENT(MCON),SSUSE(MCON,2)
      COMMON/G7/SSNA(MCON),SSPAZI(MCON),SSPELV(MCON),SSPERIM(MCON),
     &          SSUREQN(MCON,4),SSURCOG(MCON,3),SSURVN(MCON,3)

C G9 holds information on children of a surface.
      integer nbchild       ! how many children (up to 4)
      integer isgrandchild  ! if 1 child, if 2 grandchild
      character childname*12 ! name of the child surface
      common/G9/nbchild(MS),isgrandchild(MS,4),childname(MS,4)

      common/FILEP/IFIL
      common/spfldat/nsset,isset,isstup,isbnstep,ispnstep,issave,isavgh
      common/spflres/sblres(MSPS),sflres(MSPS),splres(MSPS),
     &  smstres(MSPS),selres(MSPS),sipvres

      CHARACTER*1 ACT
      CHARACTER*4 APE,EXT
      CHARACTER SSPARENT*12,SPARENT*12
      CHARACTER SSMLCN*32,SSVFC*4,SSOTF*32,SSOTHER*24,SSNAME*12,SSUSE*8
      CHARACTER CFGROOT*24
      CHARACTER LCFGF*72,LCNN*72,NNAME*72,H*72
      CHARACTER*72 LSNAM,LPROJ,LGEOM,LSHAD,LTHRM,LUTIL
      CHARACTER*72 LVIEW,LHCCO,LTWIN,LCGCIN,ZOBS
      character*72 sblres,sflres,splres,smstres,sipvres,selres
      CHARACTER OUTS*124,GUESS*24,ADF*124,SNM*12,USE*8
      character doit*248,guesstype*24
      CHARACTER SNAME*12,SOTF*32,SMLCN*32,SVFC*4,SOTHER*24,SUSE*8

C ivalsv keeps track of new viewfactor files which need analysys
C ivalsg keeps track of whether a geometry file has alread been copied.
      dimension ivalsv(MCOM),ivalsg(MCOM)

C grossarea holds the gross area of the parent + its children.
      dimension grossarea(MS)
C      real childwindowarea
C      real childroofwindowarea
C      real childdoorarea
C      real childdisplayarea
C      real parentwallarea
C      real parentroofarea
C      integer IDEBUG

C      DIMENSION IACT(MCON)  ! not yet used
      DIMENSION COE(3),POINT(3),TRANS(3),KVER(MS),LVER(MS)
      DIMENSION SNA(MS),PAZI(MS),PELV(MS),TARSURF(MCON)
      logical unixok,docnn,ok,dok
      logical newgeo  ! to use for testing if new/old geometry file.
      logical closeelv ! true if surface is close to vertical
      LOGICAL MODIFY,MODLEN,MODBND,FOUND,FOUNDA

C Initialise variables
C      IDEBUG=0
      dok=.true.
      GUESS='notional'

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

      H(1) = '*********************** IMPORTANT **********************'
      H(2) = ' '
      H(3) = 'Make sure that the original model follows the following '
      H(4) = 'assumptions before generating the notional model: '
      H(5) = '1. Exterior surfaces do not have child surfaces within '
      H(6) = '   child surfaces. (A child surface is a surface that is'
      H(7) = '   completely bounded by another surface - called parent)'
      H(8) = '   Consider making such a grandchild an independant child'
      H(9) = '   surface within another parent surface (see 4 below)'
      H(10)= '2. Each exterior surface needs to have a usage. You will'
      H(11)= '   be asked to provide it if not already defined in the'
      H(12)= '   geometry and attributions menu.'
      H(13)= '   Only the following usages can be associated with '
      H(14)= '   exterior surfaces that are parents: walls and roofs'
      H(15)= '   Any usage can be associated with child surfaces except'
      H(16)= '   walls and roofs. That is walls and roofs cannot be'
      H(17)= '   child surfaces. '
      H(18)= '3. Exterior surfaces should   | |                      '
      H(19)= '   not have obtuse interior   | |  obtuse interior     '
      H(20)= '   angles (e.g. an L shaped   | | /   angle here       '
      H(21)= '   polygon is obtuse angled   | |/_____                '
      H(22)= '   at the right angled bend)  |________                '
      H(23)= '4. Only one child surface is allowed per parent, if more '
      H(24)= '   child surfaces are required divide a parent into two '
      H(25)= '   surfaces and make one child in each.'
      H(26)= ' '
      H(27)= 'When generating the notional model all areas for parents '
      H(28)= 'and children (not glazings) are conserved. Additional '
      H(29)= 'glaing is added to all exterior surfaces as per UK NCM. '
      H(30)= ' '
      H(31)= 'In order to calculate the required glazed area, gross  '
      H(32)= 'parent area is used (e.g. if a wall has a door in it then'
      H(33)= 'the glazed area is some % of (wall area + door area))'
      H(34)= ' '
      H(35)= 'If glazing area in original is more than required for  '
      H(36)= 'notional then the same area is retained in notional model'
      H(37)= ' '
      H(38)= ' '
      H(39)= ' '
      H(40)= ' '
      H(41)= ' '
      H(42)= ' '
      CALL PHELPD('SBEM Assumptions',42,'-',0,0,IER)
      CALL ASKOK('Continue defining notional model? ',' ',OK,dok,42)
      if(.not.OK)return

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
  
C Copy standard databases containing UK NCM materials and constructions
C These files are UK_notional.constrdb and material.db3.a
C Once a generic method of doing this is developed include statement at
C the top of this subroutine can be removed (esprdbfiles.h)
      LFMAT='/usr/esru/esp-r/databases/material.db3.a'
      LFMUL='/usr/esru/esp-r/databases/UK_notional.constrdb'

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
        ACT='s'
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

C If there is a heat transfer regime copy the existing file
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

C If there is a casual gain control file copy the existing file
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

C If there is an obstruction file copy the existing file
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
        elseif(IOBS(icomp).eq.2)then
          continue   ! obstructions are within zone geometry file.
        endif
 550  CONTINUE
      call edisp(iuout,
     &   'New zone files available for applying notional rules.')

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
C        ishd=1   ! not yet used
      endif
      if(ivfwq.eq.1)then
        call usrmsg(
     &    'viewfactor files found which will be dependant on future',
     &    'geometry changes. Viewfactor files copied.','W')
      endif

C At this point prepare to update the configuration and connections
C file for the notional model
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
C      LRCR=2

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

C Establish glazing fractions based on UK NCM guidelines 
C (to read these from ncm file eventually)
      rooffr=0.2
      wallfr=0.4
      GLZFR=0.

C Loop through each zone in the model and if ivalsg was set earlier
C then read in the zone geometry file and re-gather information on
C parent child relationships
      DO 551 ICOMP=1,NCOMP
        IELF=0
        if(ivalsg(icomp).eq.icomp)then
          IUF=IFIL+2
          call eclose(gversion(icomp),1.1,0.01,newgeo)
          if(newgeo)then
            call georead(IUF,LGEOM(ICOMP),ICOMP,1,0,iuout,IER)
          else
            call egomin(IUF,LGEOM(ICOMP),ICOMP,1,0,iuout,IER)
          endif
          ACT='s'

C Code to loop through each surface to build a hash table so that each
C surface also knowns its children
          CALL SURREL(ACT,ICOMP,IER)

C Get surface areas via call to zinfo.
          call zinfo(iuout,sna,zoa,pazi,pelv,zvol)

C For each surface in this zone
          do 552 isur=1,nzsur(icomp)
            icn=IZSTOCN(icomp,isur)

C If surface is exterior then check if use is specified if not then ask
            if(ssother(icn,1)(1:8).eq.'EXTERIOR')then
              ADF=SSUSE(ICN,1)
              IF(ADF(1:1).EQ.'-')THEN
                izgfoc=icomp
                itrc=0
                MODIFY=.TRUE.
                CALL INLNST(1)
                LINSTY(ICN)=2
                CALL ADJVIEW(ITRC,IER)
                WRITE(OUTS,'(2A)')'Enter usage for surface ',SSNAME(ICN)
                CALL EDISP(IUOUT,' ')
                CALL EDISP(IUOUT,OUTS)
                CALL EDISP(IUOUT,' ')
                CALL EDITUSE(ICOMP,ISUR,ICN,GUESS)
              ENDIF
            endif
 552      continue

C Get gross area of this surface (add child areas to it)
C Areas of parents are hence held as parent + child in TARSURF and
C areas of children are held as 0.0
C For each surface in this zone
          do 555 isur=1,nzsur(icomp)
            icn=IZSTOCN(icomp,isur)
            if(ssother(icn,1)(1:8).eq.'EXTERIOR')then
              TARSURF(ICN)=SNA(ISUR)
              ADF=SSPARENT(ICN)
              IF(ADF(1:1).NE.'-')THEN
                DO 553 ISUR2=1,NZSUR(ICOMP)
                  ICN2=IZSTOCN(ICOMP,ISUR2)
                  SNM=SSNAME(ICN2)
                  IF(SNM(1:LNBLNK(SNM)).EQ.ADF(1:LNBLNK(ADF)))THEN
                    TARSURF(ICN2)=TARSURF(ICN2)+SNA(ISUR)
                    TARSURF(ICN)=0.0
                  ENDIF
 553            CONTINUE
              ENDIF
            endif
 555      continue

C For each surface in this zone
          do 554 isur=1,nzsur(icomp)
            icn=IZSTOCN(icomp,isur)
            ADF=SSPARENT(ICN)

C Figure out if this surface needs additional glazing or not
            if(ssother(icn,1)(1:8).eq.'EXTERIOR'.AND.
     &        ADF(1:1).EQ.'-')then
              GROSAREA=TARSURF(ICN)
              XNETAREA=SSNA(ICN)
              GLAZAREA=GROSAREA-XNETAREA
              SNM=SSUSE(ICN,1)
              IF(SNM(1:4).EQ.'WALL')THEN
                GLZFR=WALLFR
              ELSEIF(SNM(1:4).EQ.'ROOF')THEN
                GLZFR=ROOFFR
              ELSE
                WRITE(OUTS,'(2A)')SSNAME(ICN),
     &          ' not type WALL or ROOF glazing will not be added to it'
                CALL EDISP(IUOUT,OUTS)
                GLZFR=0.0
              ENDIF

C Determine case ICASE
C Case 1 = child is a window (in which case increase its area but do not
C          add any more surfaces, note that display window areas are not
C          changed and parents with display window children are Case 3)
C Case 2 = there are no children (in which case add glazed surface)
C Case 3 = child is not a window (in which case add a separate surface
C          that is required glazing area)
C The following procedure is followed: 
C For cases 1 and 3 geometrically (copy &) diminish the parent so child
C surface has requisite area (required glaing for case 1 and required
C glazing + area of non-glazed child for case 3). Now case 1 is done but
C further processing is required for case 3, this is done in a similar
C fashion to case 2
C For case 2 and 3 add in a new surface in the parent (for case 2) and
C child (for case 3). This new surface has required glazing area.
              REQDAREA=GLZFR*GROSAREA
              IF(GLAZAREA.LT.REQDAREA)THEN
                ADF=SSNAME(ICN)
                FOUND=.FALSE.
                DO 556 ISUR2=1,NZSUR(ICOMP)
                  IF(FOUND)GOTO 556
                  ICN2=IZSTOCN(ICOMP,ISUR2)
                  SNM=SSPARENT(ICN2)
                  USE=SSUSE(ICN2,1)
                  ICASE=0
                  IF(SNM(1:LNBLNK(SNM)).EQ.ADF(1:LNBLNK(ADF)))THEN
                    IF(USE.EQ.'C-WINDOW'.OR.USE.EQ.'S-WINDOW'.OR.
     &                USE(1:6).EQ.'WINDOW')THEN
                      ICASE=1
C                      ICSUR=ICN2  ! not yet used
                      ISSUR=ISUR2
                      FOUND=.TRUE.
                    ELSE
                      ICASE=3
C                      ICSUR=ICN2  ! not yet used
                      ISSUR=ISUR2
                      FOUND=.TRUE.
                    ENDIF
                  ELSE
                    ICASE=2
                  ENDIF
 556            CONTINUE
                ISUR2=ISSUR
                IF(ICASE.EQ.1.OR.ICASE.EQ.3)THEN

C Remove coordinates of child also present in parent. This information
C is held in KVER, there are IVX number of coordinates common in both
                  IVX=0
                  DO 2000 I1=1,NVER(ISUR) 
                    DO 2001 I2=1,NVER(ISUR2)
                      FOUNDA=.FALSE.
                      IF(JVN(ISUR,I1).EQ.JVN(ISUR2,I2))THEN

C If the coordinates of child are also present in another surface isur3
C then retain these coordinates otherwise mark for deletion.
                        DO 2098 ISUR3=1,NZSUR(ICOMP)
                          IF(ISUR3.NE.ISUR2.AND.ISUR3.NE.ISUR)THEN
                            DO 2099 I3=1,NVER(ISUR3)
                              IF(JVN(ISUR3,I3).EQ.JVN(ISUR,I1))
     &                          FOUNDA=.TRUE.
 2099                       CONTINUE
                          ENDIF
 2098                   CONTINUE
                        IF(.NOT.FOUNDA)THEN
                          IVX=IVX+1
                          KVER(IVX)=JVN(ISUR,I1)
                        ENDIF
                      ENDIF
                      LVER(I2)=JVN(ISUR2,I2)
 2001               CONTINUE
 2000             CONTINUE
                  LLIM=NVER(ISUR2)
                  IF(ICASE.EQ.1)THEN
                    SCALEFACTOR=SQRT(REQDAREA/GROSAREA)
                  ELSEIF(ICASE.EQ.3)THEN
                    SCALEFACTOR=SQRT((REQDAREA+SSNA(ICN2))/GROSAREA)
                  ENDIF
                  COE(1)=SSURCOG(ICN,1)
                  COE(2)=SSURCOG(ICN,2)
                  COE(3)=SSURCOG(ICN,3)
                  DO 2003 I1=1,NVER(ISUR)
                    DO 2004 I=1,IVX
                      IF(KVER(I).EQ.JVN(ISUR,I1))THEN
                        KVER(I)=0
                        JVN(ISUR,I1)=0
                        DO 2005 II1=I1,NVER(ISUR)
                          JVN(ISUR,II1)=JVN(ISUR,II1+1)
 2005                   CONTINUE
                      ENDIF
 2004               CONTINUE
 2003             CONTINUE
                  NVER(ISUR)=NVER(ISUR)-IVX

C Remove multiple instances of the same vertex reference from parent (as
C is common in parents with child surfaces)
                  IRM=0
                  DO 2006 I1=1,NVER(ISUR)
                    DO 2007 J1=1,NVER(ISUR)
                      IF(I1.NE.J1.AND.JVN(ISUR,I1).EQ.JVN(ISUR,J1))THEN
                        IRM=IRM+1
                        JVN(ISUR,J1)=JVN(ISUR,J1+1)
                      ENDIF
 2007               CONTINUE
 2006             CONTINUE
                  NVER(ISUR)=NVER(ISUR)-IRM

C Enlarge/diminish this surface now
                  DO 2008 NV=1,NVER(ISUR)
                    POINT(1)=X(JVN(ISUR,NV))
                    POINT(2)=Y(JVN(ISUR,NV))
                    POINT(3)=Z(JVN(ISUR,NV))
                    CALL ENLARGE(POINT,TRANS,COE,SCALEFACTOR)
                    X(NTV+NV)=TRANS(1)
                    Y(NTV+NV)=TRANS(2)
                    Z(NTV+NV)=TRANS(3)
                    JVN(ISUR2,NV)=NTV+NV
                    JVN(ISUR,2*NVER(ISUR)-NV+2)=NTV+NV
 2008             CONTINUE
                  JVN(ISUR,NVER(ISUR)+1)=JVN(ISUR,1)
                  JVN(ISUR,2*NVER(ISUR)+2)=JVN(ISUR,NVER(ISUR)+2)
                  NTV=NTV+NVER(ISUR)
                  NVER(ISUR2)=NVER(ISUR)
                  NVER(ISUR)=2*NVER(ISUR)+2

C Remove coordinates of the original child surface
C First sort LLIM (array holding coordinates to be deleted)
                  DO 2010 IS=1,LLIM-1
                    ISMALL=LVER(IS)
                    LOCSM=IS
                    DO 2011 JS=IS+1,LLIM
                      IF(LVER(JS).GT.ISMALL)THEN
                        ISMALL=LVER(JS)
                        LOCSM=JS
                      ENDIF
 2011               CONTINUE
                    LVER(LOCSM)=LVER(IS)
                    LVER(IS)=ISMALL
 2010             CONTINUE
                  DO 2009 ILIM=1,LLIM
                    CALL ADDVERT(icomp,LVER(ILIM),'D',IER)
 2009             CONTINUE
                ENDIF
                IF(ICASE.EQ.2.OR.ICASE.EQ.3)THEN

C Make one child in this surface
                  IF(ICASE.EQ.3)THEN
                    ISURF=ISUR2
                    ICN2=IZSTOCN(ICOMP,ISUR2)
                  ELSEIF(ICASE.EQ.2)THEN
                    ISURF=ISUR
                  ENDIF
                  IELF=IELF+1
                  ICON=IZSTOCN(icomp,nsur)+1
                  NSUR=NSUR+1
                  NZSUR(ICOMP)=NSUR
                  NZTV(ICOMP)=NTV
                  NGL(NSUR)=0
                  WRITE(SNAME(ICOMP,NSUR),'(2A)')
     &            CHAR(96+IELF),'_not_glz'
                  COE(1)=SSURCOG(ICN,1)
                  COE(2)=SSURCOG(ICN,2)
                  COE(3)=SSURCOG(ICN,3)
                  IF(ICASE.EQ.2)THEN
                    SCALEFACTOR=SQRT(REQDAREA/GROSAREA)
                  ELSEIF(ICASE.EQ.3)THEN
                    SCALEFACTOR=SQRT(REQDAREA/SSNA(ICN2))
                  ENDIF
                  ISUR2=NZSUR(ICOMP)
                  DO 2012 NV=1,NVER(ISURF)
                    POINT(1)=X(JVN(ISURF,NV))
                    POINT(2)=Y(JVN(ISURF,NV))
                    POINT(3)=Z(JVN(ISURF,NV))
                    CALL ENLARGE(POINT,TRANS,COE,SCALEFACTOR)
                    X(NTV+NV)=TRANS(1)
                    Y(NTV+NV)=TRANS(2)
                    Z(NTV+NV)=TRANS(3)
                    JVN(ISUR2,NV)=NTV+NV
                    JVN(ISURF,2*NVER(ISURF)-NV+2)=NTV+NV
 2012             CONTINUE
                  JVN(ISURF,NVER(ISURF)+1)=JVN(ISURF,1)
                  JVN(ISURF,2*NVER(ISURF)+2)=JVN(ISURF,NVER(ISURF)+2)
                  NTV=NTV+NVER(ISURF)
                  NVER(ISUR2)=NVER(ISURF)
                  NVER(ISURF)=2*NVER(ISURF)+2

C Update the connection list. Move all others up and then insert with
C default values.
                  guesstype='any_type'
                  call addedsurf(icomp,icon,2,ier)

C Update connections list before displaying notional model zone
                  call emkcfg('-',IER)
                  MODIFY=.TRUE.
                ENDIF

C Display what the notional model looks like
                modify=.true.
                CALL INLNST(1)
                nzg=ncomp
                do 2312 ic=1,ncomp
                  nznog(ic)=IC
 2312           continue
                izgfoc=ICOMP
                CALL ADJVIEW(ITRC,IER)
              ELSE
                CONTINUE
              ENDIF
            ENDIF
  554     CONTINUE
        ENDIF

C Write geometry file version 1.1 (1.0 does not support UK NCM)
        call zgupdate(itrc,icomp,ier)
        call geowrite(IUF,LGEOM(ICOMP),ICOMP,ITRU,3,IER)

C Update connections file before going on to the next zone
        call emkcfg('-',IER)
  551 continue


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
      zutrmax = 0.0
      uroof = 0.0
      uroofmax = 0.0 
      uaroofsum = 0.0
      uroofaver = 0.0
      rexposed = 0.0

      zutrmax  = 0.0
       
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
