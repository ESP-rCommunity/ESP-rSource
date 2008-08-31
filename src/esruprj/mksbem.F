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
C  BERTER   calculator of Building and Target Emissions Rating for CO2
C           according to SBEM - EPBD regulstions

C ********************* sbmedit
C sbmedit define high level data associated with UK NCM methodology.
      subroutine sbmedit
#include "building.h"
#include "sbem.h"
#include "tdf2.h"
#include "control.h"
      
      integer lnblnk  ! function definition

      common/OUTIN/IUOUT,IUIN
      common/FILEP/IFIL      
      common/pophelp/h(60)
      
      integer ncomp,ncon
      common/C1/NCOMP,NCON
      integer nccode,indutl
      character LSNAM*72,LPROJ*72,LGEOM*72,LSHAD*72,LTHRM*72,LUTIL*72
      common/C2/LSNAM,NCCODE(MCOM),LPROJ(MCOM),LGEOM(MCOM),
     &          LSHAD(MCOM),LTHRM(MCOM),INDUTL(MCOM),LUTIL(MCOM)
      COMMON/C20/NZSUR(MCOM),NZTV(MCOM)
      common/C21/IFCFG,cfgroot,LCFGF
      COMMON/C24/IZSTOCN(MCOM,MS)
      COMMON/G0/CTYPE(MCOM),gversion(MCOM),igupgrade
      COMMON/G6/SSNAME(MCON),SSOTF(MCON),SSMLCN(MCON),SSVFC(MCON),
     &          SSOTHER(MCON,3),SSPARENT(MCON),SSUSE(MCON,2)
      COMMON/RAY3/MODIFY,MODLEN,MODBND
      COMMON/RAY6/LINSTY(MCON)
      COMMON/precz/zname(MCOM),zdesc(MCOM)
      COMMON/gzonpik/izgfoc,nzg,nznog(mcom)
      common/paths/zonepth,netpth,ctlpth,imgpth,radpth,docpth,tmppth,
     &             dbspth
      COMMON/SET1/IYEAR,IBDOY,IEDOY,IFDAY,IFTIME

C TDF related.
C itdflg=0 none, 1 older file, 2 asci v2, 3 asci v3, -2 bin v2, -3 bin v3
      COMMON/TDFI/IUTDF,ITDFLG,IUTDFA
      COMMON/TDFFT/LTDF,LTDFA
      COMMON/TDFIO/LIMP,IUIMP,ITIMP,LEXP,IUEXP,ITEXP    
      COMMON/TDFLD2/ITDFTR(MIT,MBITS),IATDF(MIT,MBITS),ITCOL(MIT,MBITS)
      COMMON/TDAT/TABU(MTABC),TABT(MTABC),TABUMN(MTABC),TABUMX(MTABC)

C Need to write TDF info in the cfg file. Use next common(s) for this.
      COMMON/TDFFLG/ITEMSTD
      COMMON/TDFFLG0/DBTAG(MIT),DBTASK(MIT),DBZN(MIT),DBSN(MIT)  
      COMMON/HDAT/IHREC(MIT)
      COMMON/TDFIH2/NTSTAT(MIT),NTBITS(MIT),
     &              NTMAR(MIT),NTMAT(MIT),NTMAI(MIT)          

      common/sctl/tcps,ictyp,iclaw,cm(misc)
      common/cctl/icascf(mcom)
      common/ctlnm/znctldoc,plctldoc,flctldoc,elctldoc,glctldoc,
     &             opticdoc
 
      character znctldoc*248,plctldoc*248,flctldoc*248,elctldoc*248
      character glctldoc*248,opticdoc*248
      character LTDF*72,LTDFA*72
      CHARACTER DFILE*72,LIMP*72,LEXP*72
      character DBTAG*12,DBTASK*12,DBZN*15,DBSN*15
      
C Where ESP-r was installed (as recorded when it was compiled).
      common/deflt4/dinstpath
      character dinstpath*48
      character subpath*72         
      CHARACTER OUTSTR*124     
 
      integer igupgrade
      CHARACTER SSPARENT*12,ACT*1,ADF*124
      CHARACTER SSMLCN*32,SSVFC*4,SSOTF*32,SSOTHER*24,SSNAME*12,SSUSE*8
      dimension IVALSS(29)       ! array to get epicks answer from
      DIMENSION ITEMS(33),ITEMX(10),KSYS(MHT),ITEMH(MCOM+4),PRSSN(MPT)
      DIMENSION KSTR(10),ITHND(MPT),HSYSNM(MCOM+1),JSTR(8),LSYS(10)
      character cfgroot*24,LCFGF*72
      character zonepth*24,netpth*24,ctlpth*24,imgpth*24,radpth*24
      character docpth*24,tmppth*24,dbspth*24,fs*1,msgl2*48,tab*1
      character outs*124,outs248*248,h*72,CTYPE*4,GUESS*24
      character HEAD*25,ITEMS*50,D64*64,ITEMX*50,ITEMH*70,PRSSN*60
      CHARACTER HSYSNM*70,KEY*1,zname*12,zdesc*64,TMPSTR*64
      character*35 ZITEMS(MCOM+4)
      real Air_infilt,gversion
      real light_input_watt !user input for heat gains from lights
                            !instead of using the values from the sbem database
      LOGICAL NEWGEO,MODIFY,MODLEN,MODBND
      logical unixok,XST
      logical ctl_Zonelink_match !This si to allow the sorted activity indices to be matched with
                                 !the original ones. The zone numbers can then be used to link the
                                 !controls to zones.
      character TRYMATCH*40
      character delim*1,louts*248
      integer mainactivity       ! the index 1-25? of the general categories of activity
      real CDAY,RDOTY            ! reals for timestamp calculations
      integer itime,ip           ! integers for timestamp calculations
      integer iafil              ! file unit for writing temporal header
      integer icolumns           ! number of colums for tdf file depending on the unique non-zero activities

      integer inxt_column !the next available column in the tdf file
      DIMENSION iactiv_tdf_item(MCOM)
      DIMENSION iforuseintdfentry(MCOM)
      DIMENSION iactiv_tdf_items(MCOM)
      integer iactiv_tdf_item
      integer iforuseintdfentry
      integer icountnotequal  ! the final unique and non-zero number of activities that represent the number of entries
                              ! in the tdf file.
      integer iactiv_tdf_items 
      integer iarraycount !used to fill in the arrays and call arlist
      integer iskipandmatch ! counter to skip common unsorted activities. See comments in the code for additional info.
      integer i2skipandmatch !second counter in case iactiv_tdf_item(IZ).eq.theactivityindex(iCTLlink)
      
C For tdf to call arlist:
      DIMENSION COLUMNTDF(MTABC,8760)
      REAL COLUMNTDF
      DIMENSION COLUMNWRITE(MTABC)
      REAL COLUMNWRITE
      REAL val_converted
      integer iloc1,iloc2,iloc3,iloc4,iloc5  !for writing tdf location data in the file
            
C Set folder separator (fs) to \ or / as required.
      call isunix(unixok)
      if(unixok)then
        fs = char(47)
      else
        fs = char(92)
      endif
      tab=','

C place (temporary?) call to read sbem db
c      call sbempr(ier)

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
      ITEMX(10)= 'Update geometry '
      IX=1
      CALL EPICKS(IX,KSTR,' ','Define UK NCM data',
     &  50,10,ITEMX,'Define UK NCM data',IER,2)
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
     &    ' and the NCM guide table 4 is also undefined.'
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
          ROOFFRFR=0.30
          WALLFRFR=0.10

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
            mainactivity=KSYS(1)          
            write(TRYMATCH,'(a)') ITEMS(mainactivity)(1:40)
            DO 150 IAT=1,MACL

C Match the selected name with the name within the selected category 
              if(TRYMATCH(1:32).eq.roomactname(iat)(1:32))then

C When you find the name try to match the selected building index
C with the index of the activity inside this main category (i.e.
C the number after the matched string in the SBEM.db1 database
                if(ibusertyp.eq.bld_order_index(IAT))then

C This finds the selcted general index of the activity (i.e. 1-512)
C and can now be written in other places (e.g. ncm file for using it
C with tdf)
                  theactivityindex(IZ)=IAT
                  IACTYTYP(IZ)=mainactivity
                  write(outs,*)'for zone ',
     &            zname(iz)(1:lnblnk(zname(iz))),
     &            ' activity is:',roomactname(iat)
                  CALL EDISP(IUOUT,OUTS(1:LNBLNK(OUTS)))
                  goto 151
                endif             
              endif
 150        CONTINUE
 151        continue
 140      CONTINUE
        ELSE
          CALL EDISP(IUOUT,'Define building type first')
        ENDIF
      ELSEIF(KSTR(1).EQ.8)THEN

C Link lighting systems with zones
        DO 160 IZ=1,NCOMP
          ILIGHTUSER(IZ)=0
          CALL EASKABC('Choose one of the options for defining lighting
     &    heat gains from lights for: ',zname(IZ)(1:lnblnk(zname(IZ))),
     &    'user defined W/m2 per 100lux','pick from list','continue',
     &    Iwaylight,2)
          IF(Iwaylight.EQ.1)THEN

C flag needed for the casual.F to account for lighting gains
C depending on the way they are specified by the user.
            ILIGHTUSER(IZ)=1
            
C If users pick this then they have to demonstrate that this
C value is a realistic one
            light_input_watt=LIGHTWATTAGE(IZ)
            CALL EASKR(light_input_watt,
     &      'Enter heat gains from lights in watts (assuming 100% on)
     &      for zone:',zname(IZ)(1:lnblnk(zname(IZ))),0.,'F',0.,'-',
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
      CALL EDISP(IUOUT,'This facility is currently not available')

C Update all geometry files to version 1.1 from previous
      ELSEIF(KSTR(1).EQ.10)THEN
        GUESS='notional'
        DO 551 ICOMP=1,NCOMP
          IUF=IFIL+2
          CALL ECLOSE(GVERSION(ICOMP),1.1,0.01,NEWGEO)
          IF(NEWGEO)THEN
            CALL GEOREAD(IUF,LGEOM(ICOMP),ICOMP,1,0,IUOUT,IER)
          ELSE
            CALL EGOMIN(IUF,LGEOM(ICOMP),ICOMP,1,0,IUOUT,IER)
          ENDIF
          ACT='s'

C Code to loop through each surface to build a hash table so that each
C surface also knowns its children
          CALL SURREL(ACT,ICOMP,IER)

C For each surface in this zone
          DO 552 ISUR=1,NZSUR(ICOMP)
            ICN=IZSTOCN(ICOMP,ISUR)

C If surface is exterior then check if use is specified if not then ask
            IF(SSOTHER(ICN,1)(1:8).EQ.'EXTERIOR')THEN
              ADF=SSUSE(ICN,1)
              IF(ADF(1:1).EQ.'-')THEN
                IZGFOC=ICOMP
                ITRC=0
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
            ENDIF
 552      CONTINUE
          CALL GEOWRITE(IUF,LGEOM(ICOMP),ICOMP,ITRU,3,IER)
 551    CONTINUE
      ENDIF
      IF(KSTR(1).GE.1.AND.KSTR(1).LE.9)GOTO 222

C Save information to NCM project specific (*.ncm) file
      call mksbem

C If there is no temporal file generate initial file contents over
C the whole year at one tsph and with one activity item.
      if (LTDFA(1:4).eq.'UNKN'.or.LTDFA(1:2).eq.'  ') then
        XST=.false.

C Default name for temporal file.
        if(netpth(1:2).eq.'  '.or.netpth(1:2).eq.'./')then
          WRITE(LTDFA,'(2a)')cfgroot(1:lnblnk(cfgroot)),
     &      '_temporal.tdfa'
        else
          WRITE(LTDFA,'(4a)') netpth(1:lnblnk(netpth)),fs,
     &      cfgroot(1:lnblnk(cfgroot)),'_temporal.tdfa'
        endif
      else
        INQUIRE (FILE=LTDFA,EXIST=XST)
      endif
      if(XST)then

C There is an existing temporal file, scan it. NOTE: no checks
C yet made that it is hourly and full year.
        IER=0
        call supplyandcheck(LTDFA,'T',ier)
        if(ier.ne.0)then
          write(6,*) 'non-zero error after call to supplyandcheck'
        endif
      endif
      if(ISBEM.EQ.1)THEN
        icountnotequal=0
        DO 623 IZ=1,NCOMP
          iactiv_tdf_item(IZ)=0
          iactiv_tdf_items(IZ)=0
          iforuseintdfentry(IZ)=0
          iactiv_tdf_item(IZ)=theactivityindex(IZ)
  623   CONTINUE
  
C Set entries in ascending order.
        KFLAG=2
        call SORTI(iactiv_tdf_item,iactiv_tdf_items,NCOMP,KFLAG)
        iskipandmatch=1
        DO 622 IZ=1,NCOMP
          ctl_Zonelink_match=.false.
          if(iactiv_tdf_item(IZ).ne.iactiv_tdf_item(IZ-1))then
            write(6,*)'iz: ',iactiv_tdf_item(IZ),'iz-1: ',
     &                 iactiv_tdf_item(IZ-1)
C Reset the counter iskipandmatch to use for the opposite case (i.e.
C where iactiv_tdf_item(IZ).eq.iactiv_tdf_item(IZ-1)
C See comments at the place where iskipandmatch is used for more details
C about this counter
            iskipandmatch=1     
            icountnotequal=icountnotequal+1
            iforuseintdfentry(icountnotequal)=iactiv_tdf_item(IZ)
            write(6,*)'a:', icountnotequal

C Control items
C Need a counter here to skip common non-sorted activities and match the right one
C Example: A building with sorted activity indices of: 92,92,92,155,196 
C in needed to identify the zone number where this activities belong to in order
C to connect the control loops. Let's say: 2,4,5,3,1 respectively for this example.
C Unsorted this activities would have been: 155,92,196,92,92 and the zone numbers
C would obviously be 1,2,3,4,5 respectively.
            iCTLlink=1
            do while(.not.ctl_Zonelink_match.and.iCTLlink.le.ncomp)
              if(iactiv_tdf_item(IZ).eq.
     &            theactivityindex(iCTLlink))then
                ctl_Zonelink_match=.true.
              else
                iCTLlink=iCTLlink+1                  
              endif
            end do
            ibsn(icountnotequal,1)=0
            ibsn(icountnotequal,2)=0
            ibsn(icountnotequal,3)=0
            ibsn(icountnotequal,4)=0
            val_converted=REAL(icountnotequal)
            cm(2)=5.0*val_converted
            cm(3)=5.0*val_converted+1.
            iban(icountnotequal,1)=0
            iban(icountnotequal,2)=0
            iban(icountnotequal,3)=0
            nbcdt(icountnotequal)=1
            ibcdv(icountnotequal,1,1)=1
            ibcdv(icountnotequal,1,2)=365
            nbcdp(icountnotequal,1)=1
            tbcps(icountnotequal,1,1)=0.0
            ibctyp(icountnotequal,1,1)=0
            ibclaw(icountnotequal,1,1)=23
            cm(1)=2.0

C Update the control period data.
            bmiscd(icountnotequal,1,1,1)=cm(1)
            bmiscd(icountnotequal,1,1,2)=cm(2)
            bmiscd(icountnotequal,1,1,3)=cm(3)   

C Update the control file and link the control loop to the correct zone.
            icascf(iCTLlink)=icountnotequal
          elseif(iactiv_tdf_item(IZ).eq.iactiv_tdf_item(IZ-1))then
            iskipandmatch=iskipandmatch+1
            write(6,*)'iz_b: ',iactiv_tdf_item(IZ),'iz_b-1: ',
     &                 iactiv_tdf_item(IZ-1)
            write(6,*)'b:', icountnotequal
            i2skipandmatch=0
            iCTLlink=1
            do while(.not.ctl_Zonelink_match.and.iCTLlink.le.ncomp)
              if(iactiv_tdf_item(IZ).eq.
     &           theactivityindex(iCTLlink))then

C Need a counter here to skip common non-sorted activities and match the right one
C Example: A building with sorted activity indices of: 92,92,92,155,196 
C in needed to identify the zone number where this activities belong to in order
C to connect the control loops. Let's say: 2,4,5,3,1 respectively for this example.
C Unsorted this activities would have been: 155,92,196,92,92 and the zone numbers
C would obviously be 1,2,3,4,5 respectively. The first loop (IZ here) goes through the
C sorted indices and then the second loop (iCTLlink here) goes through the unsorted.
C In this second case here where iactiv_tdf_item(IZ).eq.iactiv_tdf_item(IZ-1)
C for example 92,92... the logic for the loop through the unsorted activities needs to
C know if the activity has already been scanned and skip it in order to take the right
C zone number and use for the control loops. This is why the counter iskipandmatch is
C used here.
C i2skipandmatch will be used as a second counter in case 
C iactiv_tdf_item(IZ).eq.theactivityindex(iCTLlink)
C This whole thing is done to determine which control function links to which zone
                i2skipandmatch=i2skipandmatch+1
                if(iskipandmatch.eq.i2skipandmatch)then
                  ctl_Zonelink_match=.true.
                else
                  ctl_Zonelink_match=.false.                                     
                endif
                iCTLlink=iCTLlink+1                   
              else
                iCTLlink=iCTLlink+1               
              endif
            end do 

C Update the control file and link the control loop to the correct zone.
C Using iCTLlink-1 because 1 has been added to iCTLlink when 
C the do while loop finished.
            icascf(iCTLlink-1)=icountnotequal
          endif            
  622   CONTINUE
        iloc1=0
        iloc2=0
        iloc3=0
        iloc4=0
        iloc5=0
        znctldoc='Specific controls for UK NCM activities '
        ncf=icountnotequal
        ICTLF=IFIL+1
        call usrmsg('Updating model control for temporal link...',
     &      ' ','-')
        call CTLWRT(ICTLF,IER)   

C Write out the temporal header as well as hourly data for the
C first temporal item.
        iafil=IUTDFA
        ITDFLG=3       
        itdyear=iyear
        CALL ERPFREE(iafil,ISTAT)
        CALL EFOPSEQ(iafil,LTDFA,4,IER)
        WRITE(iafil,'(a)',IOSTAT=ios,ERR=101) 'ASCIITDF3'
        WRITE(iafil,'(a)',IOSTAT=ios,ERR=101) 
     &    '# NWPR NITDF NTSPH itdyear,itdbdoy,itdedoy,columns'
        if(icountnotequal.gt.1)then

C write 5 columns per unique activity entry
          icolumns=5*icountnotequal
          WRITE(iafil,'(a,i2,a,i5,a,i3)',IOSTAT=ios,ERR=101) 
     &    '   20   ',icountnotequal,'    1',itdyear,'    1  365',
     &    icolumns
          WRITE(iafil,'(a)',IOSTAT=ios,ERR=101) '# NEXTRC,NEXTCL,NDBSTP'
          inxt_column=icolumns+1
          WRITE(iafil,'(a,i3,a)',IOSTAT=ios,ERR=101) '      1   ',
     &         inxt_column,' 8760'
        else
          icolumns=5
          WRITE(iafil,'(a,i5,a)',IOSTAT=ios,ERR=101) 
     &    '   20    1    1',itdyear,'    1  365    5'  
          WRITE(iafil,'(a)',IOSTAT=ios,ERR=101) '# NEXTRC,NEXTCL,NDBSTP'
          WRITE(iafil,'(a)',IOSTAT=ios,ERR=101) '      1      1   8760'
        endif
        WRITE(iafil,'(a)',IOSTAT=ios,ERR=101) 
     &    '*tdaid1,Set set of UK NCM occupancy patterns'
        WRITE(iafil,'(a)',IOSTAT=ios,ERR=101) '*tdaid2,-'
        DO 1 i=1,icountnotequal
        
C Get first the location of the columns to use the right data during simulation
          iloc1=5*(i-1)+1
          iloc2=5*(i-1)+2
          iloc3=5*(i-1)+3
          iloc4=5*(i-1)+4
          iloc5=5*(i-1)+5
          WRITE(iafil,'(a)',IOSTAT=ios,ERR=101) '*items'
          WRITE(iafil,'(2a,i4)',IOSTAT=ios,ERR=101) '*tag',tab,
     &          iforuseintdfentry(i)
          WRITE(iafil,'(a)',IOSTAT=ios,ERR=101) '*type,ACTIVITY'
          WRITE(iafil,'(a)',IOSTAT=ios,ERR=101) '*menu,Activity Type:'
          WRITE(iafil,'(a)',IOSTAT=ios,ERR=101) 
     &      '*aide,Activity db (DOE/SBEM)'

C NTBITS and NTSTAT are hardwired here to 5 and 11 respectively
          WRITE(iafil,'(a)',IOSTAT=ios,ERR=101) '*other,  11  5'
          WRITE(iafil,'(2a)',IOSTAT=ios,ERR=101) '*fields,16'
          WRITE(iafil,'(1a,F4.3,2a)',IOSTAT=ios,ERR=101) 
     &      'REAL  1  0   ',occupant_dens(iforuseintdfentry(i)),
     &      ' 0.000    100.000  ','Occupant density (pers/m2):'
          WRITE(iafil,'(1a,I3,2a)',IOSTAT=ios,ERR=101) 
     &      'INTG  2  0   ',metabolic_rate(iforuseintdfentry(i)),
     &      ' 0       999  ','Metabolic rate (W/pers):'
          WRITE(iafil,'(1a,I3,2a)',IOSTAT=ios,ERR=101) 
     &      'INTG  3  0   ',latent_ocup_percent(iforuseintdfentry(i)),
     &      ' 0       100  ','Occup. Latent gain % (0-100):'
          WRITE(iafil,'(1a,F6.3,2a)',IOSTAT=ios,ERR=101) 
     &      'REAL  4  0    ',equip_gain(iforuseintdfentry(i)),
     &      ' 0.000    400.000  ','Equipment (W/m2):'
          WRITE(iafil,'(1a,I3,2a)',IOSTAT=ios,ERR=101) 
     &      'INTG  5  0    ',latent_equip_percent(iforuseintdfentry(i)),
     &      ' 0       100  ','Equip. Latent gain % (0-100):'
          WRITE(iafil,'(1a,I3,2a)',IOSTAT=ios,ERR=101) 
     &      'INTG  6  0    ',lighting_lux(iforuseintdfentry(i)),
     &      ' 0       999  ','Number of luxes (lux):'
          WRITE(iafil,'(1a,F6.3,2a)',IOSTAT=ios,ERR=101) 
     &      'REAL  7  0    ',display_lighting(iforuseintdfentry(i)),
     &      ' 0.000    400.000  ','Display Lighting (W/m2):'
          WRITE(iafil,'(1a,F6.3,2a)',IOSTAT=ios,ERR=101) 
     &      'REAL  8  0    ',dhw_litres(iforuseintdfentry(i)),
     &      ' 0.000    400.000  ','Dom. Hot Water (l/d/m2):'
          WRITE(iafil,'(1a,F6.3,2a)',IOSTAT=ios,ERR=101) 
     &      'REAL  9  0    ',fresh_air(iforuseintdfentry(i)),
     &      ' 0.000    400.000  ','Outdoor air (l/s/pers.):'
          WRITE(iafil,'(2a)',IOSTAT=ios,ERR=101) 
     &      'INTG 10  0         0         0       100  ',
     &      'Min Humidity levels (%): '
          WRITE(iafil,'(2a)',IOSTAT=ios,ERR=101) 
     &      'INTG 11  0         0         0       100  ',
     &      'Max Humidity levels (%): '
          WRITE(iafil,'(a,i3,2a)',IOSTAT=ios,ERR=101) 
     &      'REAL 12 ',iloc1,'  0.000      0.000      1.000  ',
     &      'Occupant fraction:'
          WRITE(iafil,'(a,i3,2a)',IOSTAT=ios,ERR=101) 
     &      'REAL 13 ',iloc2,'   0.000      0.000      1.000  ',
     &      'Lights fraction:'
          WRITE(iafil,'(a,i3,2a)',IOSTAT=ios,ERR=101) 
     &      'REAL 14 ',iloc3,'   0.000      0.000      1.000  ',
     &      'Equipment fraction:'
          WRITE(iafil,'(a,i3,F6.2,2a)',IOSTAT=ios,ERR=101) 
     &      'REAL 15 ',iloc4,Hmainsetpoint(iforuseintdfentry(i)),
     &      '   -101.000    101.000  ','Heating setpoint (C):'
          WRITE(iafil,'(a,i3,F6.2,2a)',IOSTAT=ios,ERR=101) 
     &      'REAL 16 ',iloc5,Cmainsetpoint(iforuseintdfentry(i)),
     &      '   -101.000    101.000  ','Cooling setpoint (C):'
          WRITE(iafil,'(a)',IOSTAT=ios,ERR=101) '*end_item'
 1      CONTINUE    
        WRITE(iafil,'(a)',IOSTAT=ios,ERR=101) '*pointers'

C Need to predict the next value of ihrec. Do this by setting irec
C to the next position after the last item header. If the first
C item then the position will be 5.
        if(icountnotequal.gt.1)then

C It will always be NTBITS=5 here and NTSTAT=11 for NCM activities
          do 852 ibits_and_stats=1,icountnotequal
            NTBITS(ibits_and_stats)=5
            NTSTAT(ibits_and_stats)=11
  852     continue
          ihrec(1)=5            
          do 963 iwrpoint=2,icountnotequal
            ihrec(iwrpoint)=ihrec(iwrpoint-1)+
     &            NTBITS(iwrpoint-1)+NTSTAT(iwrpoint-1)+1
  963     continue
          itrunc=1
          ipos=1
          delim='C'
          do while (itrunc.ne.0)
            call ailist(ipos,icountnotequal,IHREC,MIT,delim,louts,
     &           loutln,itrunc)
            WRITE(iafil,'(1x,a)',IOSTAT=ios,ERR=101) louts(1:loutln)
            ipos=itrunc+1
          end do
        else
          ihrec(1)=5
          WRITE(iafil,'(1x,i3)',IOSTAT=ios,ERR=101) ihrec(1)
        endif
        WRITE(iafil,'(a)',IOSTAT=ios,ERR=101) '*tabular_data'
        WRITE(iafil,'(2a)',IOSTAT=ios,ERR=101) 
     &    '# Time Col 1 Col 2 Col 3 Col 4 Col 5 Col 6 Col 7 Col 8...'
        iarraycount=0
        DO 2 iimport=1,icountnotequal
          if(iforuseintdfentry(iimport).ge.1.and.
     &       iforuseintdfentry(iimport).lt.10)then
            write(subpath,'(6a,I1,a)',iostat=ios,err=212)
     &         dinstpath(1:lnblnk(dinstpath)),fs,'databases',fs,
     &        'UK_NCM',fs,iforuseintdfentry(iimport),'.csv' 
          elseif(iforuseintdfentry(iimport).ge.10.and.
     &           iforuseintdfentry(iimport).lt.100)then
            write(subpath,'(6a,I2,a)',iostat=ios,err=212)
     &         dinstpath(1:lnblnk(dinstpath)),fs,'databases',fs,
     &        'UK_NCM',fs,iforuseintdfentry(iimport),'.csv'           
          elseif(iforuseintdfentry(iimport).ge.100.and.
     &           iforuseintdfentry(iimport).lt.1000)then
            write(subpath,'(6a,I3,a)',iostat=ios,err=212)
     &         dinstpath(1:lnblnk(dinstpath)),fs,'databases',fs,
     &        'UK_NCM',fs,iforuseintdfentry(iimport),'.csv'
          endif 
          write(DFILE,'(a)')subpath(1:lnblnk(subpath))                 

C<<<<<Improve this part in the future>>>>>>>>>>>>
          LIMP=DFILE
          IF(ITIMP.EQ.1)THEN
            CALL ERPFREE(IUIMP,ISTAT)
            call findfil(LIMP,XST)
            IF(.NOT.XST)THEN
              msgl2='  '
              CALL USRMSG('This file does not exist.',msgl2,'W')
            ENDIF
          ENDIF

C Import data - open the import file and read them.
          IUNIT=IFIL+4
          CALL EFOPSEQ(IUNIT,LIMP,1,IER)
          if(IER.ne.0) then
            msgl2='Is the name correct?'
            CALL USRMSG('Could not open import file.',msgl2,'W')

C<<<<<<Return may not needed.. test>>>>>>>>>>>>>>>>>
            RETURN
          endif

C STRIP the specific line of the specific timestep
          DO 4 istrip=1,8760
            CALL STRIPC(IUNIT,OUTSTR,0,ND,1,'import raw data',IER)
            IF(IER.NE.0)GOTO 103

C Read value, associate with proper column and check if it changes
C the minimum/maximum of that column.
            K=0
            VAL=0.
            CALL EGETWR(OUTSTR,K,VAL,0.,0.,'-','imp data',IER)
            CALL EGETWR(OUTSTR,K,VAL,0.,0.,'-','imp data',IER)
            TABU(ITCOL(iimport,12))=VAL
            if(TABU(ITCOL(iimport,12)).lt.
     &        TABUMN(ITCOL(iimport,12)))
     &      TABUMN(ITCOL(iimport,12))=
     &      TABU(ITCOL(iimport,12))
            if(TABU(ITCOL(iimport,12)).gt.
     &        TABUMX(ITCOL(iimport,12)))
     &      TABUMX(ITCOL(iimport,12))=
     &      TABU(ITCOL(iimport,12))
            iarraycount=1+((iimport-1)*5)
            COLUMNTDF(iarraycount,istrip)=TABU(ITCOL(iimport,12))

            CALL EGETWR(OUTSTR,K,VAL,0.,0.,'-','imp data',IER)
            TABU(ITCOL(iimport,13))=VAL
            if(TABU(ITCOL(iimport,13)).lt.
     &      TABUMN(ITCOL(iimport,13)))
     &      TABUMN(ITCOL(iimport,13))=
     &      TABU(ITCOL(iimport,13))
            if(TABU(ITCOL(iimport,13)).gt.
     &      TABUMX(ITCOL(iimport,13)))
     &      TABUMX(ITCOL(iimport,13))=
     &      TABU(ITCOL(iimport,13))
            iarraycount=2+((iimport-1)*5)
            COLUMNTDF(iarraycount,istrip)=TABU(ITCOL(iimport,13))   
                       
            CALL EGETWR(OUTSTR,K,VAL,0.,0.,'-','imp data',IER)
            TABU(ITCOL(iimport,14))=VAL
            if(TABU(ITCOL(iimport,14)).lt.
     &      TABUMN(ITCOL(iimport,14)))
     &      TABUMN(ITCOL(iimport,14))=
     &      TABU(ITCOL(iimport,14))
            if(TABU(ITCOL(iimport,14)).gt.
     &      TABUMX(ITCOL(iimport,14)))
     &      TABUMX(ITCOL(iimport,14))=
     &      TABU(ITCOL(iimport,14))
            iarraycount=3+((iimport-1)*5)
            COLUMNTDF(iarraycount,istrip)=TABU(ITCOL(iimport,14))   
            
            CALL EGETWR(OUTSTR,K,VAL,0.,0.,'-','imp data',IER)
            TABU(ITCOL(iimport,15))=INT(VAL)
            if(TABU(ITCOL(iimport,15)).lt.
     &      TABUMN(ITCOL(iimport,15)))
     &      TABUMN(ITCOL(iimport,15))=
     &      TABU(ITCOL(iimport,15))
            if(TABU(ITCOL(iimport,15)).gt.
     &      TABUMX(ITCOL(iimport,15)))
     &      TABUMX(ITCOL(iimport,15))=
     &      TABU(ITCOL(iimport,15))
            iarraycount=4+((iimport-1)*5)
            COLUMNTDF(iarraycount,istrip)=TABU(ITCOL(iimport,15))   
            
            CALL EGETWR(OUTSTR,K,VAL,0.,0.,'-','imp data',IER)
            TABU(ITCOL(iimport,16))=INT(VAL)
            if(TABU(ITCOL(iimport,16)).lt.
     &      TABUMN(ITCOL(iimport,16)))
     &      TABUMN(ITCOL(iimport,16))=
     &      TABU(ITCOL(iimport,16))
            if(TABU(ITCOL(iimport,16)).gt.
     &      TABUMX(ITCOL(iimport,16)))
     &      TABUMX(ITCOL(iimport,16))=
     &      TABU(ITCOL(iimport,16))
            iarraycount=5+((iimport-1)*5)
            COLUMNTDF(iarraycount,istrip)=TABU(ITCOL(iimport,16))   

 4        CONTINUE

C End of file encountered, close the file and return to menu.
  103     msgl2=' End of import file reached'
          CALL USRMSG(' ',msgl2,'-')
          CALL ERPFREE(IUNIT,ISTAT)
          continue  

C<<<<<<Change this in the future >>>>>>>>>>>>>
C<<<Need to add "errors when creating string buffers." ??>>>>>>>>>>>>>
  212     write(6,*) 'subpath: ',subpath    
  2     continue 
        NTSPH=1
        itdbdoy=1
        DO 46 IP=1,8760
C          ITREC=IP+49
          CDAY=AINT(FLOAT(IP)/FLOAT(NTSPH*24))
          ITIME=IP-(INT(CDAY)*NTSPH*24)
          RDOTY=REAL(itdbdoy)+CDAY+(REAL(ITIME)/(REAL(NTSPH)*24.))
          itrunc=1
          ipos=1
          delim='C'
          do while (itrunc.ne.0)
            do 5 iwrite=1,iarraycount
              COLUMNWRITE(iwrite)=COLUMNTDF(iwrite,IP) 
  5         continue 
            call arlist(ipos,icolumns,COLUMNWRITE,
     &        iarraycount,delim,louts,loutln,itrunc)
            if(ipos.eq.1) then
              WRITE(iafil,'(F8.4,2a)',IOSTAT=ios,ERR=101) RDOTY,',',
     &          louts(1:loutln)
            else
              WRITE(iafil,'(1x,a)',
     &          IOSTAT=ios,ERR=101) louts(1:loutln)
            endif
            ipos=itrunc+1
          end do   
   46   CONTINUE

        WRITE(iafil,'(2a)',IOSTAT=ios,ERR=101) '*end_tabular_data'
        CALL ERPFREE(iafil,ISTAT)
      endif

      ITEMSTD=0 
     
      DO 555 IZ=1,NCOMP

C If there is an activity associated to the zone then assign 
C DBZN to use later on in the cfg file            
        if(theactivityindex(IZ).gt.0)then
          ITEMSTD=ITEMSTD+1

C Keep writing now the items needed for cfg file
          write(DBTAG(ITEMSTD),'(i4)')theactivityindex(IZ)
          write(DBZN(ITEMSTD),'(a)') zname(IZ)(1:12)
          DBSN(ITEMSTD)='ALL'
          DBTASK(ITEMSTD)='ACTIVITY'
        endif      
 555  CONTINUE
      call emkcfg('-',IER)
      return

 101  msgl2='  '
      CALL USRMSG('Could not write TDF header',msgl2,'W')
      IER=1
      goto 222
      end

C ********************* EMKSBM
C EMKSBM creates a model variant based on the SBEM methodology. Details 
C of this can be found in relevant EU and UK building regulations. 
C This subroutine copies the cfg, cnn and mandatory zones files and
C makes a new model representative of the notional building. 
C UK standard construction and materials databases are also copied.
C Changes to the model are then made, geometry and construction files
C are changed and so are the constructions and materials databases. 
C All networks (plant, airflow, contaminants etc) are removed from the
C model.
      SUBROUTINE EMKSBM
#include "building.h"
#include "sbem.h"
#include "esprdbfile.h"
      
      integer lnblnk  ! function definition

      common/OUTIN/IUOUT,IUIN
      common/pophelp/h(60)

      COMMON/RAY3/MODIFY,MODLEN,MODBND
      COMMON/gzonpik/izgfoc,nzg,nznog(mcom)
      COMMON/RAY6/LINSTY(MCON)

      integer ncomp,ncon
      COMMON/C1/NCOMP,NCON
      integer nccode,indutl
      character LSNAM*72,LPROJ*72,LGEOM*72,LSHAD*72,LTHRM*72,LUTIL*72
      common/C2/LSNAM,NCCODE(MCOM),LPROJ(MCOM),LGEOM(MCOM),
     &          LSHAD(MCOM),LTHRM(MCOM),INDUTL(MCOM),LUTIL(MCOM)
      COMMON/C3/IC1(MCON),IE1(MCON),ICT(MCON),IC2(MCON),IE2(MCON)
      common/C3F/LCNN
      COMMON/C20/NZSUR(MCOM),NZTV(MCOM)
      common/C21/IFCFG,cfgroot,LCFGF
      COMMON/C24/IZSTOCN(MCOM,MS)

C << G1 is probably not the common block to use for this >>
      COMMON/G1/X(MTV),Y(MTV),Z(MTV),NSUR,JVN(MS,MV),NVER(MS),NTV
      COMMON/G5/SNAME(MCOM,MS),SOTF(MS),SMLCN(MS),SVFC(MS),SOTHER(MS,3),
     &          SUSE(MS,2),SPARENT(MS)
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

C Where ESP-r was installed (as recorded when it was compiled).
      common/deflt4/dinstpath

      CHARACTER*1 ACT,fs
      character EXT*4    ! up to 4 characters in file extension 
      character APE*6    ! up to 6 characters in variant name
      CHARACTER SSPARENT*12,SPARENT*12
      CHARACTER SSMLCN*32,SSVFC*4,SSOTF*32,SSOTHER*24,SSNAME*12,SSUSE*8
      CHARACTER CFGROOT*24
      CHARACTER LCFGF*72,LCNN*72,NNAME*72,H*72
      CHARACTER*72 LVIEW,LHCCO,LTWIN,LCGCIN,ZOBS
      character*72 sblres,sflres,splres,smstres,sipvres,selres
      CHARACTER OUTS*124,GUESS*24,ADF*124,SNM*12,SNM2*12,USE*8,USE2*8
      character doit*248,guesstype*24,dinstpath*48
      CHARACTER SNAME*12,SOTF*32,SMLCN*32,SVFC*4,SOTHER*24,SUSE*8

C ivalsv keeps track of new viewfactor files which need analysys
C ivalsg keeps track of whether a geometry file has alread been copied.
      dimension ivalsv(MCOM),ivalsg(MCOM)

C grossarea holds the gross area of the parent + its children.
C      dimension grossarea(MS)
      DIMENSION COE(3),POINT(3),TRANS(3),KVER(MS),LVER(MS)
      DIMENSION SNA(MS),PAZI(MS),PELV(MS),TARSURF(MCON)
      logical unixok,docnn,ok,dok
      logical newgeo  ! to use for testing if new/old geometry file.
C      logical closeelv ! true if surface is close to vertical
      LOGICAL MODIFY,MODLEN,MODBND,FOUND,FOUNDA,QUIET,XST
      INTEGER IFRAME

C Set folder separator (fs) to \ or / as required.
      call isunix(unixok)
      if(unixok)then
        fs = char(47)
      else
        fs = char(92)
      endif

C Initialise variables
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
      APE='_not  '
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
      H(29)= 'glazing is added to all exterior surfaces as per UK NCM. '
      H(30)= '* if parent surface is not wall/roof no glazing is added'
      H(31)= 'In order to calculate the required glazed area, gross  '
      H(32)= 'parent area is used (e.g. if a wall has a door in it then'
      H(33)= 'the glazed area is some % of (wall area + door area))'
      H(34)= ' '
      H(35)= 'If glazing area in original is more than required for  '
      H(36)= 'notional then the same area is retained in notional model'
      H(37)= ' '
      H(38)= '5. If you have not attributed some use (eg door) to zone'
      H(39)= '   surfaces then do so before defining a notional model.'
      H(40)= '   Whereas you will be asked for use of exterior surfaces'
      H(41)= '   ESP-r makes following assupmtions about interior '
      H(42)= '   surfaces unless some use is attributed to these'
      H(43)= ' '
      H(44)= 'For interior surfaces it is assumed that all vertical '
      H(45)= 'surfaces are walls. Horizontal interior surfaces are '
      H(46)= 'internal roofs/ceilings. '
      H(47)= ' '
      H(48)= ' '
      H(49)= ' '
      H(50)= ' '
      H(51)= ' '
      H(52)= ' '
      
      CALL PHELPD('SBEM Assumptions',52,'-',0,0,IER)
      CALL ASKOK('Continue defining notional model? ',' ',OK,dok,52)
      if(.not.OK)return

C The inital task is to make copies of the relevant zone files
C which is functionally equivalent to the code blocks in subroutine
C verman for the case of geometry and attribution (ILM=2).
C Assume that a new connections file needs to be created.
      docnn=.true.

C Change name of configuration file to filename_not.cfg
      EXT='.cfg'
      CALL FNCNGR(LCFGF,APE,EXT,NNAME)
      LCFGF=NNAME
  
C Set flag for cfg file that this is a notional model
      INOTI=1

C Assign standard databases containing UK NCM materials and constructions
C These files are UK_notional.constrdb, material.db3.a and optics.db2 
      WRITE(LFMAT,'(5A)')DINSTPATH(1:LNBLNK(DINSTPATH)),
     &FS,'databases',FS,'material.db3.a'
      WRITE(LFMUL,'(5A)')DINSTPATH(1:LNBLNK(DINSTPATH)),
     &FS,'databases',FS,'UK_notional.constrdb'
      WRITE(LOPTDB,'(5A)')DINSTPATH(1:LNBLNK(DINSTPATH)),
     &FS,'databases',FS,'optics.db2'
      call opendb(ier)

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

C Make a copy of constructions file and tmc file if applicable
        IUNIT=12
        QUIET=.FALSE.
        call FINDFIL(LTHRM(ICOMP),XST)
        ITRU=6
        CALL ECONST(LTHRM(ICOMP),IUNIT,ICOMP,ITRC,ITRU,IER)
        EXT='.con'
        CALL FNCNGR(LTHRM(ICOMP),APE,EXT,NNAME)
        LTHRM(ICOMP)=NNAME
        CALL EMKCON(LTHRM(ICOMP),IUNIT,ICOMP,ITRU,QUIET,IER)
        call FINDFIL(LTWIN(ICOMP),XST)
        IF(.NOT.XST)GOTO 500
        IFU=13
        CALL ERTWIN(ITRC,ITRU,IFU,LTWIN(ICOMP),ICOMP,IER)
        EXT='.tmc'
        CALL FNCNGR(LTWIN(ICOMP),APE,EXT,NNAME)
        LTWIN(ICOMP)=NNAME
        CALL MKTWIN(ITRU,IFU,ICOMP,QUIET,IER)

C Establish parents of surfaces in this zone (fill common blocks G6 G7)
 500    ACT='s'
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
      rooffrfr=0.3
      wallfrfr=0.1
      GLZFR=0.
      itrc=0

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
                ijk=lnblnk(ssname(icn))
                WRITE(OUTS,'(3A)')'Surface ',SSNAME(ICN)(1:ijk),
     &          ' has use: something else. Confirm this in which case'
                CALL EDISP(IUOUT,' ')
                CALL EDISP(IUOUT,OUTS)
                CALL EDISP(IUOUT,
     &          'no glazing will be added to it or select other use')
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

C For all exterior surfaces that are also parents
            if(ssother(icn,1)(1:8).eq.'EXTERIOR'.AND.
     &        ADF(1:1).EQ.'-')then

C Figure out if this surface needs additional glazing or not
              GROSAREA=TARSURF(ICN)
              XNETAREA=SSNA(ICN)
              GLAZAREA=GROSAREA-XNETAREA
              SNM=SSUSE(ICN,1)
              IF(SNM(1:4).EQ.'WALL')THEN
                GLZFR=WALLFR
                FRMFR=WALLFRFR
              ELSEIF(SNM(1:4).EQ.'ROOF')THEN
                GLZFR=ROOFFR
                FRMFR=ROOFFRFR
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
C For special case 3 where the child surface is a window frame, make the
C area of the window up to requisite percentage including this frame
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
                      ISSUR=ISUR2
                      FOUND=.TRUE.
                    ELSE
                      ICASE=3
                      ISSUR=ISUR2
                      FOUND=.TRUE.
                    ENDIF
		    IF(USE(1:5).EQ.'FRAME')THEN
		      REQDAREA=GLZFR*GROSAREA*(1.0-FRMFR)
                      SSNA(ICN2)=GLZFR*GROSAREA*FRMFR
		      IF(REQDAREA.LT.0.0)THEN
			REQDAREA=0.0
			CALL EDISP(IUOUT,'Frame is too large for some')
			CALL EDISP(IUOUT,'windows consider revising')
		      ENDIF
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
                  ISUR2=NSUR
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
                  call addedsurf(icomp,icon,itrc,ier)

C Add surfaces attribution parameters
                  ICT(ICON)=0
                  IC2(ICON)=0
                  IE2(ICON)=0
                  SSOTHER(ICON,1)='EXTERIOR'
                  SSOTHER(ICON,2)='0'
                  SSOTHER(ICON,3)='0'
                  SSMLCN(ICON)='window_Not_G'
                  SSPARENT(ICON)=SSNAME(ICN)
                  SSOTF(ICON)='Glaz_notiona'
                  SSVFC(ICON)=SSVFC(ICN)
                  SSUSE(ICON,1)='C-WINDOW'
                  SSUSE(ICON,2)='-'

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
        NZSUR(ICOMP)=NSUR
        call zgupdate(itrc,icomp,ier)
        call geowrite(IUF,LGEOM(ICOMP),ICOMP,ITRU,3,IER)

C Update connections file before going on to the next zone
        call emkcfg('-',IER)
  551 continue

C Now add window frames (frames previously defined in the original are
C retained in the code above but only the areas are changed). Following
C code only adds frames to windows that do not have any framing.
      DO 700 ICOMP=1,NCOMP
        call georead(IUF,LGEOM(ICOMP),ICOMP,1,0,iuout,IER)
        DO 557 isur=1,nzsur(icomp)
          icn=IZSTOCN(icomp,isur)
          ADF=SSPARENT(ICN)

C For all exterior surfaces that are not parents and are transparent
          if(ssother(icn,1)(1:8).eq.'EXTERIOR'.AND.
     &      ADF(1:1).NE.'-'.AND.SSUSE(ICN,1)(3:8).EQ.'WINDOW')then
 	    SNM=SSUSE(ICN,1)(1:1)
 	    IF(SNM(1:1).EQ.'C'.OR.SNM(1:1).EQ.'D')THEN
 	      FRMFR=WALLFRFR
 	    ELSEIF(SNM(1:1).EQ.'S')THEN
 	      FRMFR=ROOFFRFR
 	    ENDIF

C Set flag that stating this glazed surface does not have a frame
            IFRAME=0

C Determine if frame needs to be added as a new surface or if such a
C surface is already present
            DO 753 ISUR2=1,NZSUR(ICOMP)
              ICN2=IZSTOCN(ICOMP,ISUR2)
              SNM2=SSPARENT(ICN2)
              USE=SSUSE(ICN2,1)
              IF(ADF(1:LNBLNK(ADF)).EQ.SNM2(1:LNBLNK(SNM2)).AND.
     &        USE(1:5).EQ.'FRAME')IFRAME=1
 753        CONTINUE

C If frame has been defined in the DO loop above then IFRAME would be
C set to 1. If it has not been set then define a frame for the window
            IF(IFRAME.EQ.0)THEN
              ISURF=ISUR
              IELF=IELF+1
              ICON=IZSTOCN(icomp,nsur)+1
              NSUR=NSUR+1
              NZTV(ICOMP)=NTV
              NGL(NSUR)=0
              WRITE(SNAME(ICOMP,NSUR),'(2A)')CHAR(96+IELF),'_frame'
              COE(1)=SSURCOG(ICN,1)
              COE(2)=SSURCOG(ICN,2)
              COE(3)=SSURCOG(ICN,3)
              SCALEFACTOR=SQRT(FRMFR)
              ISUR2=NSUR
              DO 2612 NV=1,NVER(ISURF)
                POINT(1)=X(JVN(ISURF,NV))
                POINT(2)=Y(JVN(ISURF,NV))
                POINT(3)=Z(JVN(ISURF,NV))
                CALL ENLARGE(POINT,TRANS,COE,SCALEFACTOR)
                X(NTV+NV)=TRANS(1)
                Y(NTV+NV)=TRANS(2)
                Z(NTV+NV)=TRANS(3)
                JVN(ISUR2,NV)=NTV+NV
                JVN(ISURF,2*NVER(ISURF)-NV+2)=NTV+NV
 2612         CONTINUE
              JVN(ISURF,NVER(ISURF)+1)=JVN(ISURF,1)
              JVN(ISURF,2*NVER(ISURF)+2)=JVN(ISURF,NVER(ISURF)+2)
              NTV=NTV+NVER(ISURF)
              NVER(ISUR2)=NVER(ISURF)
              NVER(ISURF)=2*NVER(ISURF)+2

C Update the connection list. Move all others up and then insert with
C default values.
              guesstype='any_type'
              call addedsurf(icomp,icon,itrc,ier)

C Add surfaces attribution parameters
              ICT(ICON)=0
              IC2(ICON)=0
              IE2(ICON)=0
              SSOTHER(ICON,1)='EXTERIOR'
              SSOTHER(ICON,2)='0'
              SSOTHER(ICON,3)='0'
              SSMLCN(ICON)='win_frame'
              SSPARENT(ICON)=SSNAME(ICN)
              SSOTF(ICON)='OPAQUE'
              SSVFC(ICON)=SSVFC(ICN)
              SSUSE(ICON,1)='FRAME'
              SSUSE(ICON,2)='-'

C Update connections list before displaying notional model zone
              call emkcfg('-',IER)
              MODIFY=.TRUE.
            ENDIF
          endif
 557    CONTINUE

C Write geometry file version 1.1 (1.0 does not support UK NCM)
        NZSUR(ICOMP)=NSUR
        call zgupdate(itrc,icomp,ier)
        call geowrite(IUF,LGEOM(ICOMP),ICOMP,ITRU,3,IER)

C Update connections file before going on to the next zone
        call emkcfg('-',IER)
 700  CONTINUE 

C Change constructions for different surfaces now based on use
      DO 600 ICOMP=1,NCOMP
        call georead(IUF,LGEOM(ICOMP),ICOMP,1,0,iuout,IER)
        DO 601 I=1,NZSUR(icomp)
          icc=izstocn(icomp,i)
          USE=SSUSE(ICC,1)
          USE2=SSUSE(ICC,2)
          ADF=SSVFC(icc)
          IF(ssother(icc,1)(1:8).eq.'EXTERIOR')THEN
            IF(USE(1:4).EQ.'WALL')THEN
              IF(IRGG.EQ.2)THEN ! Scottish regulations
                SSMLCN(ICC)='Wall_Scot_no'
              ELSEIF(IRGG.EQ.3)THEN ! Northern Ireland regulations
                SSMLCN(icc)='Wall_NI_noti'
              ELSE ! England and Wales regulations (and omissions)
                SSMLCN(ICC)='Wall_EW_noti'
              ENDIF
            ELSEIF(USE(1:4).EQ.'ROOF')THEN
              IF(USE2(1:4).EQ.'FLAT')THEN
                SSMLCN(ICC)='Fl_roof_noti'
              ELSE
                IF(IRGG.EQ.2)THEN ! Scottish regulations
                  SSMLCN(ICC)='SC_Pitch_roo'
                ELSE ! England, Wales and N. Ireland regulations
                  SSMLCN(ICC)='EW_notPitch_'
                ENDIF
              ENDIF
            ELSEIF(USE(3:6).EQ.'DOOR')THEN
              IF(USE(1:1).EQ.'V')THEN
                SSMLCN(ICC)='Vehicle_door'
              ELSE
                SSMLCN(ICC)='opDoorWind_n'
              ENDIF
            ELSEIF(USE(3:8).EQ.'WINDOW')THEN
              IF(USE(1:1).EQ.'D')THEN
                SSMLCN(ICC)='Not_DisplayG'
                SSOTF(ICC)='dispGlz_not_modCond'
              ELSE
                SSMLCN(ICC)='window_Not_G'
                SSOTF(ICC)='Glaz_notiona'
              ENDIF
            ELSEIF(USE(1:5).EQ.'FLOOR')THEN
              SSMLCN(ICC)='Sol_grnd_not'
            ELSEIF(USE(1:5).EQ.'FRAME')THEN
              SSMLCN(ICC)='win_frame'
            ELSE
              SNM=SSNAME(ICC)
              WRITE(OUTS,'(3A)')'No relevant use defined for ',
     &        SNM(1:LNBLNK(SNM)),' setting to use: external wall'
              CALL EDISP(IUOUT,OUTS)
              IF(IRGG.EQ.2)THEN ! Scottish regulations
                SSMLCN(ICC)='Wall_Scot_no'
              ELSEIF(IRGG.EQ.3)THEN ! Northern Ireland regulations
                SSMLCN(icc)='Wall_NI_noti'
              ELSE ! England and Wales regulations (and omissions)
                SSMLCN(ICC)='Wall_EW_noti'
              ENDIF
            ENDIF
          ELSE
            IF(USE(1:5).EQ.'FLOOR')THEN
              SSMLCN(ICC)='pa_fl_notion'
            ELSEIF(USE(1:4).EQ.'WALL')THEN
              SSMLCN(ICC)='party_wall_n'
            ELSEIF(USE(3:8).EQ.'WINDOW')THEN
              SSMLCN(ICC)='internal_glz'
              SSOTF(ICC)='Glaz_notiona'
            ELSEIF(ADF(1:4).EQ.'CEIL'.OR.ADF(1:4).EQ.'FLOR')THEN
              SSMLCN(ICC)='inv_pa_fl_no'
            ELSE
              SNM=SSNAME(ICC)
              WRITE(OUTS,'(3A)')'No relevant use defined for ',
     &        SNM(1:LNBLNK(SNM)),' setting internal wall'
              SSMLCN(ICC)='party_wall_n'
            ENDIF
          ENDIF
 601    CONTINUE

C Add 10% thermal bridge by silently calling appropriate subroutine
        CALL LINTHBRDG(ICOMP,'s',0.1)

C Write geometry file version 1.1 (1.0 does not support UK NCM)
        call geowrite(IUF,LGEOM(ICOMP),ICOMP,ITRU,3,IER)
        QUIET=.TRUE.
        CALL EDCON(ITRC,ITRU,ICOMP,QUIET,IER)
 600  CONTINUE
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
C If sufficient information is not present it generates 
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
      
      integer ncomp,ncon
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
      COMMON/precz/zname(MCOM),zdesc(MCOM)
      common/PREC17/ZBASEA(MCOM),IBASES(MCOM,12),IUZBASEA(MCOM),
     &  IZBASELIST(MCOM)
     
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
      character zname*12,zdesc*64
      LOGICAL CONTEXT,BNDRY,ISNEAR,XST
      REAL TMPREAL
      REAL tot_floor_area,tot_ext_area
#include "build_info.h"

C Check information about building, certifier and owner to be present
      
C Read *.ncm file and SBEM database
      CALL RSBEM
c      CALL SBEMPR(IER)

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

C Call calculator for BER and TER
      CALL BERTER(IER)
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
        WRITE(IUF,'(a,F6.3)',iostat=ios)' COOL-GEN-EER       = ',
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
      do 30 iz=1, ncomp
        WRITE(IUF,'(3a)',iostat=ios) '"',zname(iz)(1:lnblnk(zname(IZ))),
     &      '" = CHECK3'

C Leave this at Low risk for the moment but it has to change at the moment
C and be part of the calculation
        WRITE(IUF,'(2a)',iostat=ios) ' OVERHEATING-RISK      = ',
     &      'Low risk'     
        WRITE(IUF,'(a)',iostat=ios) ' ..'
 30   continue
      
      WRITE(IUF,'(a)',iostat=ios) '"CHECK4" = CHECK4'
      WRITE(IUF,'(a,F5.2)',iostat=ios) ' Q50-INF              = ',
     &  BINF50
      WRITE(IUF,'(a)',iostat=ios) ' ..'

      WRITE(IUF,'(a)',iostat=ios) '"CHECK5" = CHECK5'
      WRITE(IUF,'(a)',iostat=ios) ' ..'

      WRITE(IUF,'(a)',iostat=ios) '"CHECK6" = CHECK6'
      WRITE(IUF,'(a)',iostat=ios) ' ..'

      WRITE(IUF,'(a)',iostat=ios) '"BUILDING DATA" = BUILDING DATA'
      WRITE(IUF,'(a)',iostat=ios) ' ANALYSIS       =  ACTUAL'
      tot_floor_area=0.0
      do 31 iz=1, ncomp
        tot_floor_area=zbasea(iz)+tot_floor_area
 31   continue
      WRITE(IUF,'(a,f6.2)',iostat=ios) ' AREA           = ',
     &   tot_floor_area

C Calculate external area as (ext wall exposed + floor exposed 
C + roof exposed). This has been concluded looking SBEM 3.1 
C and BRUKL output
      tot_ext_area=vexposed+fexposed+rexposed
      WRITE(IUF,'(a,f6.2)',iostat=ios) ' AREA-EXT       = ',
     &   tot_ext_area

C This will have to be based on the location of the study
      WRITE(IUF,'(a)',iostat=ios) ' WEATHER       =  LON'

C Confirm that BINF50 instead of some sort of "translated" air changes
      WRITE(IUF,'(a,F5.2)',iostat=ios) ' Q50-INF              = ',
     &  BINF50

C Hardwiring values here as they are not clear what they mean from the
C BRUKL manual
      WRITE(IUF,'(2a)',iostat=ios) ' BUILDING-W/K         = ',
     &  ' 1822.41'

C Hardwiring values here as they are not clear what they mean from the
C BRUKL manual
      WRITE(IUF,'(2a)',iostat=ios) ' BUILDING-W/M2K       = ',
     &  ' 0.164924'

C Hardwiring values here as they are not clear what they mean from the
C BRUKL manual
      WRITE(IUF,'(2a)',iostat=ios) ' BUILDING-ALPHA       = ',
     &  ' 28.02'

C Hardwiring values here as they have not yet been calculated
      WRITE(IUF,'(2a)',iostat=ios) ' KWH/M2-HEAT         = ',
     &  ' 106.613'

C Hardwiring values here as they have not yet been calculated
      WRITE(IUF,'(2a)',iostat=ios) ' KWH/M2-COOL         = ',
     &  ' 0.'

C Hardwiring values here as they have not yet been calculated
      WRITE(IUF,'(2a)',iostat=ios) ' KWH/M2-AUX          = ',
     &  ' 0.'          

C Hardwiring values here as they have not yet been calculated
      WRITE(IUF,'(2a)',iostat=ios) ' KWH/M2-LIGHT        = ',
     &  ' 0.'   

C Hardwiring values here as they have not yet been calculated
      WRITE(IUF,'(2a)',iostat=ios) ' KWH/M2-DHW          = ',
     &  ' 0.'  

C Hardwiring values here as they have not yet been calculated
      WRITE(IUF,'(2a)',iostat=ios) ' KWH/M2-EQUP         = ',
     &  ' 0.'  

C Hardwiring values here as they have not yet been calculated
      WRITE(IUF,'(2a)',iostat=ios) ' KWH/M2-NATGAS       = ',
     &  ' 0.'  

C Hardwiring values here as they have not yet been calculated
      WRITE(IUF,'(2a)',iostat=ios) ' KWH/M2-LPG          = ',
     &  ' 0.'  

C Hardwiring values here as they have not yet been calculated
      WRITE(IUF,'(2a)',iostat=ios) ' KWH/M2-BIOGAS       = ',
     &  ' 0.'  

C Hardwiring values here as they have not yet been calculated
      WRITE(IUF,'(2a)',iostat=ios) ' KWH/M2-OIL              = ',
     &  ' 0.'  

C Hardwiring values here as they have not yet been calculated
      WRITE(IUF,'(2a)',iostat=ios) ' KWH/M2-COAL             = ',
     &  ' 0.'  

C Hardwiring values here as they have not yet been calculated
      WRITE(IUF,'(2a)',iostat=ios) ' KWH/M2-ANTHRACITE       = ',
     &  ' 0.'  

C Hardwiring values here as they have not yet been calculated
      WRITE(IUF,'(2a)',iostat=ios) ' KWH/M2-SMOKELESS        = ',
     &  ' 0.'  

C Hardwiring values here as they have not yet been calculated
      WRITE(IUF,'(2a)',iostat=ios) ' KWH/M2-DUELFUEL         = ',
     &  ' 0.'  

C Hardwiring values here as they have not yet been calculated
      WRITE(IUF,'(2a)',iostat=ios) ' KWH/M2-BIOMASS          = ',
     &  ' 0.'  

C Hardwiring values here as they have not yet been calculated
      WRITE(IUF,'(2a)',iostat=ios) ' KWH/M2-SUPELEC          = ',
     &  ' 0.'  

C Hardwiring values here as they have not yet been calculated
      WRITE(IUF,'(2a)',iostat=ios) ' KWH/M2-WASTEHEAT        = ',
     &  ' 0.'  

C Hardwiring values here as they have not yet been calculated
      WRITE(IUF,'(2a)',iostat=ios) ' KWH/M2-DISTRICT-HEATING = ',
     &  ' 0.' 

C Hardwiring values here as they have not yet been calculated
      WRITE(IUF,'(2a)',iostat=ios) ' KWH/M2-DISP             = ',
     &  ' 0.'  

C Hardwiring values here as they have not yet been calculated
      WRITE(IUF,'(2a)',iostat=ios) ' KWH/M2-PVS              = ',
     &  ' 0.'  

C Hardwiring values here as they have not yet been calculated
      WRITE(IUF,'(2a)',iostat=ios) ' KWH/M2-WIND             = ',
     &  ' 0.'  

C Hardwiring values here as they have not yet been calculated
      WRITE(IUF,'(2a)',iostat=ios) ' KWH/M2-CHP              = ',
     &  ' 0.'  

C Hardwiring values here as they have not yet been calculated
      WRITE(IUF,'(2a)',iostat=ios) ' KWH/M2-SES              = ',
     &  ' 0.'  

C Possibly activity number and area per zone
      WRITE(IUF,'(2a,52i4,52a,52f10.2,a)',iostat=ios) 
     &     ' ACT-AREA                = ',' { ',
     &  (theactivityindex(iz),' ',zbasea(iz),iz=1,ncomp),' }' 

C Here need a section for each HVAC system
C<<<<<<<<<<<<<<<<<<<<<<<FILL IN HVAC

C Notional building related data
      WRITE(IUF,'(a)',iostat=ios) '"BUILDING DATA" = BUILDING DATA'
      WRITE(IUF,'(a)',iostat=ios) ' ANALYSIS       =  NOTIONAL'
      tot_floor_area=0.0
      do 131 iz=1, ncomp
        tot_floor_area=zbasea(iz)+tot_floor_area
 131  continue
      WRITE(IUF,'(a,f6.2)',iostat=ios) ' AREA           = ',
     &   tot_floor_area

C Calculate external area as (ext wall exposed + floor exposed 
C + roof exposed). This has been concluded looking SBEM 3.1 
C and BRUKL output
      tot_ext_area=vexposed+fexposed+rexposed
      WRITE(IUF,'(a,f6.2)',iostat=ios) ' AREA-EXT       = ',
     &   tot_ext_area

C This will have to be based on the location of the study
      WRITE(IUF,'(a)',iostat=ios) ' WEATHER       =  LON'

C Confirm that BINF50 instead of some sort of "translated" air changes
      WRITE(IUF,'(a)',iostat=ios) ' Q50-INF              = 10'

C Hardwiring values here as they are not clear what they mean from the
C BRUKL manual
      WRITE(IUF,'(2a)',iostat=ios) ' BUILDING-W/K         = ',
     &  ' 1822.41'

C Hardwiring values here as they are not clear what they mean from the
C BRUKL manual
      WRITE(IUF,'(2a)',iostat=ios) ' BUILDING-W/M2K       = ',
     &  ' 0.164924'

C Hardwiring values here as they are not clear what they mean from the
C BRUKL manual
      WRITE(IUF,'(2a)',iostat=ios) ' BUILDING-ALPHA       = ',
     &  ' 28.02'

C Hardwiring values here as they have not yet been calculated
      WRITE(IUF,'(2a)',iostat=ios) ' KWH/M2-HEAT         = ',
     &  ' 106.613'

C Hardwiring values here as they have not yet been calculated
      WRITE(IUF,'(2a)',iostat=ios) ' KWH/M2-COOL         = ',
     &  ' 0.'

C Hardwiring values here as they have not yet been calculated
      WRITE(IUF,'(2a)',iostat=ios) ' KWH/M2-AUX          = ',
     &  ' 0.'          

C Hardwiring values here as they have not yet been calculated
      WRITE(IUF,'(2a)',iostat=ios) ' KWH/M2-LIGHT        = ',
     &  ' 0.'   

C Hardwiring values here as they have not yet been calculated
      WRITE(IUF,'(2a)',iostat=ios) ' KWH/M2-DHW          = ',
     &  ' 0.'  

C Hardwiring values here as they have not yet been calculated
      WRITE(IUF,'(2a)',iostat=ios) ' KWH/M2-EQUP         = ',
     &  ' 0.'  

C Hardwiring values here as they have not yet been calculated
      WRITE(IUF,'(2a)',iostat=ios) ' KWH/M2-NATGAS       = ',
     &  ' 0.'  

C Hardwiring values here as they have not yet been calculated
      WRITE(IUF,'(2a)',iostat=ios) ' KWH/M2-LPG          = ',
     &  ' 0.'  

C Hardwiring values here as they have not yet been calculated
      WRITE(IUF,'(2a)',iostat=ios) ' KWH/M2-BIOGAS       = ',
     &  ' 0.'  

C Hardwiring values here as they have not yet been calculated
      WRITE(IUF,'(2a)',iostat=ios) ' KWH/M2-OIL              = ',
     &  ' 0.'  

C Hardwiring values here as they have not yet been calculated
      WRITE(IUF,'(2a)',iostat=ios) ' KWH/M2-COAL             = ',
     &  ' 0.'  

C Hardwiring values here as they have not yet been calculated
      WRITE(IUF,'(2a)',iostat=ios) ' KWH/M2-ANTHRACITE       = ',
     &  ' 0.'  

C Hardwiring values here as they have not yet been calculated
      WRITE(IUF,'(2a)',iostat=ios) ' KWH/M2-SMOKELESS        = ',
     &  ' 0.'  

C Hardwiring values here as they have not yet been calculated
      WRITE(IUF,'(2a)',iostat=ios) ' KWH/M2-DUELFUEL         = ',
     &  ' 0.'  

C Hardwiring values here as they have not yet been calculated
      WRITE(IUF,'(2a)',iostat=ios) ' KWH/M2-BIOMASS          = ',
     &  ' 0.'  

C Hardwiring values here as they have not yet been calculated
      WRITE(IUF,'(2a)',iostat=ios) ' KWH/M2-SUPELEC          = ',
     &  ' 0.'  

C Hardwiring values here as they have not yet been calculated
      WRITE(IUF,'(2a)',iostat=ios) ' KWH/M2-WASTEHEAT        = ',
     &  ' 0.'  

C Hardwiring values here as they have not yet been calculated
      WRITE(IUF,'(2a)',iostat=ios) ' KWH/M2-DISTRICT-HEATING = ',
     &  ' 0.' 

C Hardwiring values here as they have not yet been calculated
      WRITE(IUF,'(2a)',iostat=ios) ' KWH/M2-DISP             = ',
     &  ' 0.'  

C Hardwiring values here as they have not yet been calculated
      WRITE(IUF,'(2a)',iostat=ios) ' KWH/M2-PVS              = ',
     &  ' 0.'  

C Hardwiring values here as they have not yet been calculated
      WRITE(IUF,'(2a)',iostat=ios) ' KWH/M2-WIND             = ',
     &  ' 0.'  

C Hardwiring values here as they have not yet been calculated
      WRITE(IUF,'(2a)',iostat=ios) ' KWH/M2-CHP              = ',
     &  ' 0.'  

C Hardwiring values here as they have not yet been calculated
      WRITE(IUF,'(2a)',iostat=ios) ' KWH/M2-SES              = ',
     &  ' 0.'  

C Possibly activity number and area per zone
      WRITE(IUF,'(2a,52i4,52a,52f10.2,a)',iostat=ios) 
     &     ' ACT-AREA                = ',' { ',
     &  (theactivityindex(iz),' ',zbasea(iz),iz=1,ncomp),' }' 
     
C Here need a section for each HVAC system
C<<<<<<<<<<<<<<<<<<<<<<<FILL IN HVAC



      WRITE(IUF,'(a)',iostat=ios) ' ..'

      call edisp(iuout,'Completed export of data to BRUKL inp')
      CALL ERPFREE(IUF,ISTAT)
      RETURN
 3    CONTINUE ! error message to go here
      END


************************* BERTER *****************************
C Subroutine BERTER runs silent simulations of the actual and notional
C building as described in SBEM - EPBD documents and generates building
C emissions rate and target emissions rate (BER and TER) in order to
C provide information about UK NCM code (non)compliance. Relevant
C sections of legislation are, Building Code Section 6 for Scotland,
C Part L for England and Wales and Part F for Northern Ireland.

      SUBROUTINE BERTER(IER)
#include "sbem.h"
#include "building.h"
      
      integer ncomp,ncon
      common/C1/NCOMP,NCON

      INTEGER ICOMP

C First run simulation of the actual building and fill arrays HEATREL
C and COOLREL

C Now run simulation of the notional building and fill arrays HEATNOT
C and COOLNOT

C Now calculate carbon dioxide emissions related to real building
c      BER=0.

      DO 100 ICOMP=1,NCOMP
c        BER=BER+HEATREL(ICOMP)*FUELCO2(IFTYP(ICOMP))/HGEF(IHLZ(ICOMP))+
c     &      COOLREL(ICOMP)*FUELCO2(IFTYP(ICOMP))/HGEF(IHLZ(ICOMP))
 100  CONTINUE
      IF(IER.EQ.0)THEN
        RETURN  ! SOMETHING WRONG HAS OCCURED
      ENDIF
      RETURN
      END














