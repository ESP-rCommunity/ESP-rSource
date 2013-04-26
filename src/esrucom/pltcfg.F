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
C      PLTCFG

C ******************** PLTCFG
C PLTCFG controls input of a data set which defines a plant
C configuration for simulation.
C Common block variables are:

C INDCFG      - configuration file index
C                2 = plant only

C IFLWN       - plant fluid flow simulation index (1 = on)
C ICFFS       - fluid flow network node associated with each plant
C               component. NB fluid flow network and plant energy
C               network need not be matched

C LPNAM       - the name of the plant system

C NPCOMP      - number of plant components

C NPCDAT(*,1) - component code number. Presently this code is only
C               used for reference purpose

C NPMTYP      - type of matrix required for configuration
C                1 = energy balance only
C                2 = mass balance, single phase
C                3 = mass balance, two phase

C NPSTA       - start address for each component in the database

C NCI         - number of possible control variables
C               Note that if any of these variables are actually
C               to be controlled, then a corresponding control
C               loop must be activated

C CDATA       - design (or upper) values for each control variable

C NPCON       - number of inter-component connections

C IPC1 & IPN1 - component and node numbers defining receiving component
C               (ie defining inlet node)

C IPCT        - plant component inter-connection type where:
C                1 = connected to identical environmental conditions
C                    as the source component;
C                2 = connected to constant and known environmental
C                    conditions (defined by PCONSD(?,1) and PCONSD(?,2));
C                3 = connected to another component (as defined
C                    by IPC2 and IPN2);
C                4 = connected to a building zone (as defined by
C                    PCONSD(?,1)) or ambient conditions (PCONSD(?,1)=0).

C IPC2 & IPN2 - component and node numbers defining the connected comp.

C PCONSD      - Array containing a list of constant data items for the
C               current connection.

C NCONT       - total number of plant component containment conditions
C               NB it is permissible to contain only some subset of the
C               total number of components. Unreferenced components
C               would then have no exchange with their environment

C IPCC        - defines a plant component for containment

C INDCP       - index which dictates the type of containment (as ICT
C               above but read 'component' for 'zone')

C CNTDAT      - environment (or surroundings) data for a plant component
C                if INDCP=0 ; outside air used and CNTDAT(1) used to
C                             indicated the variation from this value
C                if INDCP=1 ; temperature of a specified plant node.
C                             CNTDAT(1) identifies the component, and
C                             CNTDAT(2) the node therein, and CNTDAT(3)
C                             some constant temperature shift. CNTDAT(1)
C                             and CNTDAT(2) both zero is the self con-
C                             tainment case
C                if INDCP=2 ; constant temperature the value of which
C                             is in CNTDAT(1)
C                if INDCP=3 ; temperature of a building zone, where
C                             CNTDAT(1) identifies the zone number,
C                             CNTDAT(2) the surface number ; and
C                             CNTDAT(3) the node number from 'outside'
C                             If CNTDAT(2) and CNTDAT(3) both zero then
C                             zone air temp. used and if CNTDAT(3) zero
C                             use the inside surface node temperature
C Electrical information (for power consuming components only).
C 
C PFP(*)         - Power factor for component.
C IPFP(*)        - Power factor flag  for component leading 1, lagging -1
C                 or unity 0. 
C PWRP(*)        - Real power consumption of component. Power generation 
C                  is specified as a negative value.
C BVOLTP(*)      - Operational voltage of component.
C IPHP(*)        - Phase the component is connected to.
C 

C The following variables allow different parameters
C to be specified for a component. The value specified for
C NPI must be entered after CDATA record.

C NPI         - Indicates weather user wishes to specify different
C               values for component parameters. IF 0 then default
C               values in data base are used otherwise a list of NPI
C               items is given in the following record.
C
C TADATA(*,I) - Holds new (I) parameter values for component.
C
C pcdesc(*)   - Holds description of plant components.
C npref(*)    - Entry number in plant component database.

      SUBROUTINE PLTCFG(iuc,iunit1,itu,itrc)

C Parameters:
C  iuc is the unit number of the plant network file which is assumed
C      to be open.
C  iunit1 is the unit number of the plant database (this is opened
C      within the subroutine).
C  itu is the output channel for reporting (if itrc is > zero).
C  itrc the level of reporting (zero is quiet).

#include "plant.h"
#include "building.h"
#include "geometry.h"
#include "esprdbfile.h"
#include "gencompDB.h"
#include "gencompDB_common.h"
C esprdbfile.h supplies the following:
C LAPRES,IAPRES (for window pressure databases)
C LPCDB/IPCDB (for plant template database)
      
      integer lnblnk  ! function definition

      COMMON/ER1/IER1
      COMMON/outin/iuout,iuin

      COMMON/C6/INDCFG
      COMMON/C8/LPNAM
      COMMON/C9/NPCOMP,NCI(MPCOM),CDATA(MPCOM,MMISCD)
      COMMON/C9plus/UCdbNam(MPCOM),CdbCat(MPCOM),CDBDesc(MPCOM)

      COMMON/C10/NPCON,IPC1(MPCON),IPN1(MPCON),IPCT(MPCON),
     &           IPC2(MPCON),IPN2(MPCON),PCONDR(MPCON),PCONSD(MPCON,2)
      COMMON/C11/NCONT,IPCC(MPCOM),INDCP(MPCOM),CNTDAT(MPCOM,3)
      COMMON/C12PS/NPCDAT(MPCOM,9),IPOFS1(MCOEFG),IPOFS2(MCOEFG,MPVAR)
      COMMON/C13PS/NPMCOE,NPNOD,NPMTYP
      COMMON/FFN/IFLWN,ICFFS(MPCON)
      COMMON/PCPAR/NPI(MPCOM), TADATA(MPCOM,MADATA)
      common /datdsc/ mscdsc(madata), cvrdsc(mconvr)
      common /pcddsc/ pcdesc(maxpc), npref(mpcom)
      common/pcnam/pcname(mpcom)

C Electrical details for specified plant components
      common/pcelflg/ipcelf(mpcom)
      common/elpcp/NPEL,PFP(mpcom),IPFP(mpcom),PWRP(mpcom),
     &BVOLTP(mpcom),IPHP(mpcom)

C Common for new format component database
      COMMON/NCDBFORM/ICDBF

      COMMON/IsTRNSYS/Is_TRNSYS(MPCOM)


      LOGICAL close,FOUNDNUM(10),Is_TRNSYS
      CHARACTER OUTSTR*124,OUTS*124,word*20,LLTMP*144,cmsg*24
      CHARACTER LPNAM*72,argstr*12
      CHARACTER PZDESCR*25,UCDBNam*16, CdbCat*32, CDBDesc*72
      character*68 mscdsc, cvrdsc      
      character loutstr*248
      DIMENSION NPSTA(MAXPC),RVA(MMISCD),RVB(MPCON)
      DIMENSION RVC(MADATA), NEP(MPCOM)

C CEDATE required when reading plant data base record
C containing NMISC.
      CHARACTER CEDATE*16,pcdesc*80,pcname*15,CNTSTR*20,NUM*1
      character fs*1
      CHARACTER scantype*8
      character domain_s*16,category_s*16
      INTEGER   id_trnsysCMP  !trnsys component number
      integer lnpcdb ! length of file name
      integer lndbp   ! for length of standard database path
      integer ier
      logical unixok

C Set folder separator (fs) to \ or / as required.
      call isunix(unixok)
      if(unixok)then
        fs = char(47)
      else
        fs = char(92)
      endif

      call usrmsg(' ','Reading plant network description...','P')

C Older plant files may hold plant components database name as first
C non-comment entry in plant file. Even older files may hold plant
C components database name as the second non-comment line (The first
C being '* PLANT' or '* Plant'). Newer files do not hold any references
C to databases but have a title line.

      CALL LSTRIPC(iuc,LOUTSTR,0,ND,1,'* plant',IER)
      IF(IER.NE.0)RETURN
      LLTMP=LOUTSTR(1:lnblnk(LOUTSTR))
      IF(LLTMP.EQ.'* Plant'.OR.LLTMP.EQ.'* PLANT') THEN
        CALL LSTRIPC(iuc,LOUTSTR,0,ND,1,'* plant',IER)
        IF(IER.NE.0)RETURN
      ELSEIF(LLTMP(1:5).EQ.'ESP-r')THEN 

C New version plant file, get version number which should be fifth word 
C in the line. 
        K=0
        CALL EGETW(LOUTSTR,K,WORD,'W','irrelevant word',IER)
        CALL EGETW(LOUTSTR,K,WORD,'W','irrelevant word',IER)
        CALL EGETW(LOUTSTR,K,WORD,'W','irrelevant word',IER)
        CALL EGETW(LOUTSTR,K,WORD,'W','irrelevant word',IER)
        CALL EGETWI(LOUTSTR,K,IPFVER,0,3,'F','file ver number',IER)
      endif

C Pay attention to the implied path of the plant template db from cfg file.
      if(ipathpcdb.eq.0.or.ipathpcdb.eq.1)then
        lltmp=LPCDB
      elseif(ipathpcdb.eq.2)then
        lndbp=lnblnk(standarddbpath)
        write(lltmp,'(3a)') standarddbpath(1:lndbp),fs,
     &    lpcdb(1:lnblnk(lpcdb))  ! prepend db folder path
      endif

C Open plant components database on IUNIT1. IRECLN is width of plantdb
C -> but don't open the database if save-level 0 or 5 are specified!
      lnpcdb=lnblnk(lltmp)
      IRECLN=20
      ier=0
      ICDBf=0
      call EFOPRAN(IUNIT1,LLTMP,IRECLN,1,IER)   
      IF(IER.ne.0)THEN

C Attempt to open the file as a new format cdb (ASCII) file. 
        IERcdb=0
        scantype='open'
        CALL CDBScanDataBase(IUNIT1,lltmp,scantype,IERcdb)
        IF(IERcdb.ne.0)THEN
          GOTO 950
        ELSE     
C Set the components database flag to active.
          icdbf=1
          RETURN
        ENDIF
      ENDIF

*** CDB affected
C Read the total number of plant components (NTPC) in the database.
      IREC = 1
      READ(IUNIT1,REC=IREC,IOSTAT=ISTAT,ERR=950) PZDESCR
      IF (PZDESCR(1:25) .NE. ' PLANT COMPONENT DATABASE')then

C Attempt to open the file as a new format cdb (ASCII) file. 
        IERcdb=0
        scantype='open'
        CALL CDBScanDataBase(IUNIT1,lltmp,scantype,IERcdb)
        IF(IERcdb.ne.0)THEN        
          call lusrmsg('May not be a plant db!',LPCDB,'W')
        ELSE
C Set the components database flag to active.
          icdbf=1
        ENDIF
      endif

      if(icdbf.eq.0)then
        IREC=2
        READ(IUNIT1,REC=IREC,IOSTAT=ISTAT,ERR=950) NTPC,NXTREC
  
        if(NTPC.eq.0)then
          call lusrmsg('Plant db has no entries!',LPCDB,'W')
        endif
  
C Read the start address (NPSTA) for each component in the database.
        I1=1
        DO 42 I=1,MAXPC/20
          IREC=IREC+1
          II=I*20
          READ(IUNIT1,REC=IREC,IOSTAT=ISTAT,ERR=950) (NPSTA(J),J=I1,II)
          I1=II+1
  42     CONTINUE
      endif

C Configuration file plant data : first read system description.
      CALL STRIPC(iuc,OUTSTR,0,ND,1,'plnt system descr',IER)
      IF(IER.NE.0)RETURN
      LPNAM=OUTSTR(1:72)

C Read number of plant components in system.
      CALL STRIPC(iuc,OUTSTR,2,ND,1,'# of plant components',IER)
      IF(IER.NE.0)RETURN
      K=0
      CALL EGETWI(OUTSTR,K,NPCOMP,0,MPCOM,'F',
     &            '# of plant components',IER)
      
      CALL EGETWI(OUTSTR,K,NPMTYP,1,4,'F','type of matrix',IER)
      IF(IER.NE.0) GOTO 1001

C If at least one plant componet is not found, then reset simulation
C to 'building only'.      
      if ( NPCOMP < 1 ) then
        indcfg = 1
        return
      ENDIF
C  Set the electrical component counter to 0.
      npel=0

C Reporting.
      IF(ITRC.GE.1)THEN
        CALL EDISP(itu,' ')
        WRITE(LOUTSTR,'(a,i3,2a)') ' The plant network contains',
     &    NPCOMP,' components from ',LPCDB(1:LNPCDB)
        CALL EDISP248(itu,LOUTSTR,100)
      ENDIF

C Read plant component information and plant database start address
C Checks on plant component CDATA will be handled by CMP??S
      DO 44 IPCOMP=1,NPCOMP
        CALL STRIPC(iuc,OUTSTR,0,ND,1,'plant info',IER)
        IF(IER.NE.0)RETURN
        K=0
        CALL EGETW(OUTSTR,K,WORD,'W','component name',IER)
        pcname(ipcomp)=word(1:15)
C Store length of name for reporting later.
        iPltNameLen(ipcomp) = lnblnk(pcname(ipcomp))
        npcdat(ipcomp,1)=ipcomp

C **CDB affected!
        IF(ICDBf.EQ.1)THEN
          CALL EGETW(OUTSTR,K,WORD,'W','CDB database ref.',IER)
          UCDBNam(IPCOMP)=word(1:16)

C Get all of the components parameters
c          write(*,*) 'recovering ',UCdbNam(IPCOMP)
          argstr='parameters'
          domain_s='-'; category_s='-'
          CALL CDBGetItemData(domain_s,category_s,UCdbNam(IPCOMP),
     &      argstr)
          argstr="light"

C Get all of the components parameters
c          write(*,*) 'translating ',UCdbNam(IPCOMP)            
          CALL CDB2Plant(IPCOMP,argstr)
          NPCDAT(iPComp,8)=s_NNODE
          ICODE=s_ICODE
          NNODE=s_NNODE
          NMATX=s_NMATX
          NMISC=s_MISCD
c            nci(iPComp)=ncdata
c            write(*,*) 'done ',UCdbNam(IPCOMP)                        
        ELSE
          CALL EGETWI(OUTSTR,K,NPREF(IPCOMP),1,NTPC,'F',
     &              'plant db pointer',IER)
          IF(IER.NE.0) GOTO 1001
        ENDIF
          
C Read component description from database.
C ***CDB affected
        IF(ICDBF.eq.0)THEN
          irec=NPSTA(NPREF(IPCOMP))+2

C Bug - pcdesc should be an array of db plant components descriptors, not a
C descriptor for the plant component itself! 
          READ(IUNIT1,REC=IREC,IOSTAT=ISTAT,ERR=950) 
     &       pcdesc(NPREF(IPCOMP))
        ENDIF


C Determine number of control parameters
        CALL STRIPC(iuc,OUTSTR,0,ND,1,'no of ctl',IER)
        IF(IER.NE.0)RETURN
        K=0
        CALL EGETWI(OUTSTR,K,NCI(IPCOMP),0,0,'-','no of var',IER)

C Get all of the CDATA items from the configuration file.
        IRVA=NCI(IPCOMP)
        IF(IRVA.GT.0) THEN
          CALL EGETWRA(iuc,RVA,IRVA,0.,0.,'-','cntr var',IER)
          DO 12 KV=1,IRVA
            CDATA(IPCOMP,KV)=RVA(KV)
   12     CONTINUE
        END IF

C*** CDB affected!
C Get start address for component in old database
        IF(ICDBF.EQ.0)THEN
          NPCDAT(IPCOMP,2)=NPSTA(NPREF(IPCOMP))
        ELSE
          NPCDAT(IPCOMP,2)=0
        ENDIF

C Reporting.
C CDB affected!
        IF(ITRC.GE.1)THEN
          CALL EDISP(itu,' ')
          lo=lnblnk(pcname(ipcomp))
          if(ICDBf.eq.1)THEN
            WRITE(OUTS,'(3a,i2,a,i2,a,a16)')' Component: ',
     &        pcname(ipcomp)(1:lo),' (',IPCOMP,') code ',
     &        NPCDAT(IPCOMP,1),', db reference ',UCDBNam(IPCOMP)
          ELSE
            WRITE(OUTS,'(3a,i2,a,i2,a,i2)')' Component: ',
     &        pcname(ipcomp)(1:lo),' (',IPCOMP,') code ',
     &        NPCDAT(IPCOMP,1),', db reference ',NPREF(IPCOMP)
          ENDIF
          CALL EDISP(itu,OUTS)
          IF(NCI(IPCOMP).GT.0)THEN
            WRITE(OUTS,'(A,5F9.3)') ' Control data:',
     &           (CDATA(IPCOMP,J),J=1,NCI(IPCOMP))
            CALL EDISP(itu,OUTS)
          ELSE
            CALL EDISP(itu,' No Control data')
          ENDIF
        ENDIF

C Check if user wishes to specify different parameters
C for this component.
        CALL STRIPC(iuc,OUTSTR,99,ND,1,'alt comp param',IER)
        IF(IER.NE.0)RETURN
        K=0
        NEP(IPCOMP)=0
        IF(ND.GE.1)THEN
          CALL EGETWI(OUTSTR,K,NPI(IPCOMP),0,0,'-',
     &                'alt comp param',IER)
        ENDIF

C If a second data item exists on the line then electrical parameters will
C be present.
        IF (ND.EQ.2) THEN 
          CALL EGETWI(OUTSTR,K,NEP(IPCOMP),0,0,'-',
     &                'elect param',IER)  
        ENDIF
        

C CDB affected!!
        IF(NPI(IPCOMP).GT.0) THEN

C*** CDB affected!
C Find component record entry in data base to establish number
C of miscellaneous data items.
          IF(ICDBF.Eq.0)THEN
            IREC1=NPCDAT(IPCOMP,2)
            READ(IUNIT1,REC=IREC1,IOSTAT=ISTAT,ERR=950)
     &        AA,ICODE,CEDATE,NNODE,NMATX,NMISC
            
C Save number of nodes associated with each component.
            NPCDAT(iPComp,8) = NNODE
          ENDIF

C Check to see if required number of items is specified.
          if(npi(ipcomp).ne.nmisc) then
            write(OUTS,'(a,i3,a,i2,a,i2,a)')
     &        'Invalid no of parameters for component',
     &        ipcomp,' found ',npi(ipcomp),' expecting ',nmisc,
     &        ' please check plant component.'
            call edisp(iuout,OUTS)
            call epwait
            stop
          endif

C ***CDB affected
C Point to miscellaneous data record.
          IF(ICDBF.EQ.0)THEN
            IF(IPCDB.eq.0)THEN
              irec= NPSTA(NPREF(IPCOMP))+6+(nmatx-1)/IRECLN
              if(nmisc.gt.0) then

C Reading misc data items description and dummy value.
                do 100 i=1, nmisc
                  read(IUNIT1,rec=irec,iostat=istat,err=950)
     &                mscdsc(i), parval
                  irec=irec+1
  100           continue
              endif
            ENDIF
          ENDIF

C Read new parameters from configuration file.
          IRVA=NMISC
          IF(IRVA.GT.0) THEN
            CALL EGETWRA(iuc,RVC,IRVA,0.,0.,'-','alt dat',IER)
            DO 235 KV=1,IRVA
              TADATA(IPCOMP,KV)=RVC(KV)
  235       CONTINUE
          ENDIF
        ENDIF

C Read electrical parameters from the file ( if they exist !)
        IF(NEP(IPCOMP).GT.0) THEN
          IF (NEP(IPCOMP).NE.5) GOTO 1003
          CALL STRIPC(iuc,OUTSTR,0,ND,1,'elect data',IER)
          K=0
          CALL EGETWR(OUTSTR,K,VAL,0.0,1.0,'F','p.f',IER)
          PFP(IPCOMP)=VAL
          CALL EGETWI(OUTSTR,K,IVAL,-1,1,'F','ipf',IER)
          IPFP(IPCOMP)=IVAL
          CALL EGETWR(OUTSTR,K,VAL,0.,999.,'-','pwr',IER)
          PWRP(IPCOMP)=VAL
          CALL EGETWR(OUTSTR,K,VAL,0.,999.,'-','volt',IER)
          BVOLTP(IPCOMP)=VAL
          CALL EGETWI(OUTSTR,K,IVAL,1,4,'F','phase',IER) 
          IPHP(IPCOMP)=IVAL
          IF(IER.GT.0) GOTO 1003
          IPCELF(IPCOMP)=1
          NPEL=NPEL+1
        ENDIF           


C Reporting.
          IF(ITRC.GE.1.and.NPI(IPCOMP).gt.0)THEN
            write(outs,'(2a)') ' Modified parameters for ',
     &        pcname(ipcomp)
            call edisp(itu,outs)
            do 909 jj=1,nmisc
              write(outs,'(1x,a50,a3,g12.5)')
     &          mscdsc(jj),' : ',tadata(ipcomp,jj)
              call edisp(itu,outs)
  909       continue
          ENDIF

C Reporting.
          IF(ITRC.GE.1.and.NEP(IPCOMP).gt.0)THEN
            WRITE(OUTS,'(A,i3)')'Electrical data for component ',ipcomp
            call edisp(itu,outs)
            WRITE(OUTS,479) PFP(IPCOMP), IPFP(IPCOMP), PWRP(IPCOMP), 
     &      BVOLTP(IPCOMP), IPHP(IPCOMP)
  479       FORMAT(F5.3,1X,I3,1X,F10.3,1X,F10.3,1X,I3)
            call edisp(itu,outs)
            call edisp(itu,' ')
          ENDIF 

   44   CONTINUE

C Read number of plant component connections
        CALL STRIPC(iuc,OUTSTR,0,ND,1,'possible conn',IER)
        IF(IER.NE.0)RETURN
        K=0
        CALL EGETWI(OUTSTR,K,NPCON,0,MPCON,'F','plant con',IER)
        IF(NPCON.EQ.0) GOTO 48

C Reporting.
        IF(ITRC.GE.1)THEN
          CALL EDISP(itu,'  ')
          WRITE(OUTS,'(A,I2)')' Nb of plant component connections: ',
     &      NPCON
          CALL EDISP(itu,OUTS)
          CALL EDISP(itu,
     &    ' Con receiving component node type sending component node')
        ENDIF

C Read connections line by line into a string and then split this string
        DO 45 IPCON=1,NPCON
          CALL STRIPC(iuc,OUTSTR,0,ND,1,'plant info',IER)
          IF(IER.NE.0)RETURN
          K=0
          CALL EGETW(OUTSTR,K,WORD,'W','component name',IER)
          call findpc(npcomp,word,ipc1(ipcon))
          CALL EGETWI(OUTSTR,K,IPN1(IPCON),0,0,'-','node no',IER)
          CALL EGETWI(OUTSTR,K,IPCT(IPCON),0,0,'-','conn type',IER
     &      )
          CALL EGETW(OUTSTR,K,WORD,'W','component name',IER)
          call findpc(npcomp,word,ipc2(ipcon))
          CALL EGETWI(OUTSTR,K,IPN2(IPCON),0,0,'-','ref node',IER)
          CALL EGETWR(OUTSTR,K,PCONDR(IPCON),0.,0.,'-',
     &                'mass diversion ratio',IER)

C In case of type 2, also temperature (C) & humidity ratio (kg,v/kg,a)
          IF(IPCT(IPCON).EQ.2) THEN
            CALL EGETWR(OUTSTR,K,VAL,0.,0.,'-','temperature',IER)
            PCONSD(IPCON,1)=VAL
            CALL EGETWR(OUTSTR,K,VAL,0.,0.,'-','humid ratio',IER)
            PCONSD(IPCON,2)=VAL

C And in case of type 4, also the referenced building zone (0=ambient)
          ELSEIF(IPCT(IPCON).EQ.4) THEN
            CALL EGETWR(OUTSTR,K,VAL,0.,0.,'-','refer zone',IER)
            PCONSD(IPCON,1)=VAL
          ENDIF
          IF(IER.NE.0) GOTO 900

C Reporting.
          IF(ITRC.GE.1)THEN
            CALL EDISP(itu,' ')
            if(IPCT(IPCON).eq.1) cmsg='identical T + hum ratio.'
            if(IPCT(IPCON).eq.2) cmsg='known T + hum ratio.'
            if(IPCT(IPCON).eq.3) cmsg='from another component.'
            if(IPCT(IPCON).eq.4) cmsg='zone air or ambient.'
            lo=lnblnk(pcname(IPC1(IPCON)))
            lo2=lnblnk(pcname(IPC2(IPCON)))
            WRITE(OUTS,'(i3,3a,i2,5a,i2,a,3F8.2)') IPCON,
     &        ' ',pcname(IPC1(IPCON))(1:lo),' node',
     &        IPN1(IPCON),' (',cmsg(1:lnblnk(cmsg)),') ',
     &        pcname(IPC2(IPCON))(1:lo2),
     &        ' node',IPN2(IPCON),' details: ',PCONDR(IPCON),
     &        PCONSD(IPCON,1),PCONSD(IPCON,2)
            CALL EDISP(itu,OUTS)
C            WRITE(OUTS,7005)IPC2(IPCON),IPN2(IPCON),PCONDR(IPCON),
C     &                      PCONSD(IPCON,1),PCONSD(IPCON,2)
C 7005       FORMAT(' supp. data :',I3,I3,3F8.2)
C            CALL EDISP(itu,OUTS)
          ENDIF
   45   CONTINUE

C Now check connectivity information where possible
      do 46 ipcon=1,npcon
        if(ipc1(ipcon).lt.1.or.ipc1(ipcon).gt.npcomp) goto 913

        if(icdbf.eq.0)then !skip if using the new components database
          irec=npcdat(ipc1(ipcon),2)

C     The receiving node number is not checked for trnsys type
C     because the number in the plant database is a dummy value and the
C     trnsys input file has not been read yet.
C     The skip of this check will not cause any problem because the same
C     check is performed in MZPMXT.
          IF(NPREF(ipc1(ipcon)) .NE. 85) THEN
           read(iunit1,rec=irec,iostat=istat,err=950) a,b,c,d,e,f,NumNod
           if(ipn1(ipcon).lt.1.or.ipn1(ipcon).gt.NumNod) goto 914
          END IF
        endif !icdbf skip

        if(ipct(ipcon).lt.1.or.ipct(ipcon).gt.4) goto 913
        if(ipc2(ipcon).lt.1.or.ipc2(ipcon).gt.npcomp) goto 913

        if(icdbf.eq.0)then !skip if using the new components database
          irec=npcdat(ipc2(ipcon),2)

C     The sending node number is not checked for trnsys type
C     because the number in the plant database is a dummy value and the
C     trnsys input file has not been read yet.
C     The skip of this check will not cause any problem because the same
C     check is performed in MZPMXT.
          IF(NPREF(ipc2(ipcon)) .NE. 85) THEN
          read(iunit1,rec=irec,iostat=istat,err=950) a,b,c,d,e,f,NumNod
          if(ipn2(ipcon).lt.1.or.ipn2(ipcon).gt.NumNod) goto 914
          END IF
        endif ! end icdbf skip

         if(pcondr(ipcon).lt.0..or.pcondr(ipcon).gt.1.) goto 925
         if(ipct(ipcon).eq.4) then
          if(pconsd(ipcon,1).lt. (0.) .or.
     &       pconsd(ipcon,1).gt.real(mcom)) goto 913
         endif
   46   continue

C Read environmental conditions surrounding each plant component
C Read plant/building connection information for each component,
C i.e. an index plus an arbitary number of data values.
C One day, more checks will be performed on CNTDAT !
   48   CALL STRIPC(iuc,OUTSTR,0,ND,1,'containments',IER)
        IF(IER.NE.0)RETURN
        K=0
        CALL EGETWI(OUTSTR,K,NCONT,0,NPCOMP,'W','containments',IER)

C Reporting.
        IF(ITRC.GE.1)THEN
          CALL EDISP(itu,' ')
          WRITE(OUTS,'(A,I2)')' No of component containments = ',NCONT
          CALL EDISP(itu,OUTS)
          CALL EDISP(itu,'  Component      | Containment descr. | Type')
        ENDIF
        IF(NCONT.EQ.0) GOTO 50

        DO 49 ICONT=1,NCONT
          CALL STRIPC(iuc,OUTSTR,0,ND,1,'plant info',IER)
          IF(IER.NE.0)RETURN
          K=0
          CALL EGETW(OUTSTR,K,WORD,'W','component name',IER)
          call findpc(npcomp,word,ipcc(icont))
          CALL EGETWI(OUTSTR,K,INDCP(ICONT),0,3,'W',
     &         'containment type',IER)
          CALL EGETWR(OUTSTR,K,CNTDAT(ICONT,1),0.,0.,'-',
     &         'containment data 1',IER)
          CALL EGETWR(OUTSTR,K,CNTDAT(ICONT,2),0.,0.,'-',
     &         'containment data 2',IER)
          CALL EGETWR(OUTSTR,K,CNTDAT(ICONT,3),0.,0.,'-',
     &         'containment data 3',IER)

C ****CDB affected!
        IF(INDCP(ICONT).EQ.1) THEN
          call eclose(CNTDAT(ICONT,1),0.0,0.0001,close)
          IF(close) THEN
            IREC=NPCDAT(IPCC(ICONT),2)
            id_trnsysCMP = NPCDAT(IPCC(ICONT),1)
          ELSE
            IREC=NPCDAT(NINT(CNTDAT(ICONT,1)),2)
            id_trnsysCMP = NPCDAT(NINT(CNTDAT(ICONT,1)),1)
          ENDIF
          IF((NPREF(id_trnsysCMP) .EQ. 85).OR.Is_TRNSYS(IPCOMP)) THEN

C  Since a trnsys component has dummy values in the plant database,
C  its number of nodes needs to be read from an data file.
C  Another method is to purposely set a large number (ie., MNODEC)
C  in the database. In this case, reading file can be avoided here.
            CALL readTrnsysNodes(id_trnsysCMP,NumNod)
          ELSE
            IF(ICDBF.EQ.0)THEN
              READ(IUNIT1,REC=IREC,IOSTAT=ISTAT,ERR=950)A,B,C,D,E,F,
     &             NumNod
            ELSE

C Get all of the components parameters
              IPC=CNTDAT(ICONT,1)
              argstr='parameters'
              domain_s='-'; category_s='-'
              CALL CDBGetItemData(domain_s,category_s,UCdbNam(IPC),
     &          argstr)

              argstr="light"
C Decode data
              CALL CDB2Plant(IPC,argstr)
              NumNod=s_NNODE !numnod should really be held in common.
            ENDIF 
            END IF
            if(cntdat(icont,1).lt.0.0.or.cntdat(icont,1).gt.
     &            float(npcomp).or.cntdat(icont,2).lt.0.0.or.
     &            cntdat(icont,2).gt.float(NumNod)) goto 916
          ENDIF

C Reporting.
          IF(ITRC.GE.1)THEN
            CALL EDISP(itu,' ')
            IF(INDCP(ICONT).EQ.0) THEN
              WRITE(CNTSTR,'(A)') 'outside air'
            ELSEIF(INDCP(ICONT).EQ.1)THEN
              WRITE(CNTSTR,'(2A)') 'compt: ',
     &          PCNAME(NINT(CNTDAT(ICONT,1)))(1:13)
            ELSEIF(INDCP(ICONT).EQ.2)THEN
              WRITE(CNTSTR,'(A,F7.3)') 'fix temp: ',CNTDAT(ICONT,1)
            ELSEIF(INDCP(ICONT).EQ.3)THEN
              WRITE(CNTSTR,'(2A)') 'zone: ',ZNAME(NINT(CNTDAT(ICONT,1)))
            ENDIF
            WRITE(outs,'(I2,2x,A15,1X,A20,1X,I3,a,3F8.2)')ICONT,
     &        PCNAME(IPCC(ICONT)),CNTSTR,INDCP(ICONT),' details: ',
     &        (CNTDAT(ICONT,J),J=1,3)
            CALL EDISP(itu,OUTS)
          ENDIF
   49   CONTINUE

C Check if there is a plant fluid flow network available: 0 = no
   50   CALL STRIPC(iuc,OUTSTR,0,ND,1,'plant info',IER)
        IF(IER.NE.0)RETURN
        K=0
        CALL EGETWI(OUTSTR,K,IFLWN,0,1,'W','plant fluid index',IER)
        IF(IFLWN.EQ.0) goto 606

C If version 2 file then skip read of duplicated mass flow information
        NREAD=0
        IF(IPFVER.EQ.2)GOTO 3456

C It is possible that version number is not held in some version 2
C files. So need to perform following check
C If version number is held in plant file then no need to perform the
C following check otherwise determine which file version this is
        CALL STRIPC(iuc,OUTSTR,0,ND,1,'plant info',IER)

C Assume newer format version 2
        INEW=1
        K=0
        CALL EGETW(OUTSTR,K,WORD,'W','first word',IER)
        ILEN=LNBLNK(WORD)
        IF(ILEN.GT.10)ILEN=10 ! Only check maximum of first ten characters

C Check first word of this line. If all are numbers then newer format
C version 2 otherwise older format
        DO 141 IL=1,ILEN
          NUM=WORD(IL:IL)
          FOUNDNUM(IL)=.FALSE.
          DO 142 IN=48,57 ! Check for ASCII characters 0 to 9
            IF(.NOT.FOUNDNUM(IL))THEN
              IF(NUM.EQ.CHAR(IN))FOUNDNUM(IL)=.TRUE.
            ENDIF
 142      CONTINUE
          IF(FOUNDNUM(IL).AND.INEW.EQ.1)THEN
            INEW=1
          ELSE
            INEW=0
          ENDIF
 141    CONTINUE

C If older format file then the line just read was address of mass flow
C file. This can be safely ignored and also the next two lines
        IF(INEW.EQ.0)THEN
          CALL STRIPC(iuc,OUTSTR,0,ND,1,'duplicate information',IER)
          CALL STRIPC(iuc,OUTSTR,0,ND,1,'duplicate information',IER)
        ELSEIF(INEW.EQ.1)THEN

C This is newer format file the line just read was connection numbers
C Convert this line to numbers
          IF(NPCON.GE.6)THEN
            NREAD=6 ! Only expect six numbers on the line
          ELSE
            NREAD=NPCON
          ENDIF
          K=0
          DO 143 IREAD=1,NREAD
            CALL EGETWI(OUTSTR,K,ICFFS(IREAD),0,MPCOM,'F',
     &          'plt-afn link numbers',IER)
 143      CONTINUE
        ENDIF

C Determine plant inter-connection/fluid flow connection mapping
C Checks will be performed by MZPMXT
 3456   IRVB=NPCON-NREAD
        IF(IRVB.GT.0) THEN
          CALL EGETWRA(iuc,RVB,IRVB,0.,0.,'-','plt con lst',IER)
          DO 135 KV=1,IRVB
            ICFFS(KV+nread)=INT(RVB(KV))
  135     CONTINUE
        ENDIF

C Plant/mfs connections mapping.
        IF(ITRC.GE.1)THEN
          CALL EDISP(itu,' ')
          WRITE(OUTS,'(A)')
     &    '  plant inter-connection/fluid flow  connection mapping :'
          CALL EDISP(itu,OUTS)

C Determine number of lines to be written.
          NLINES=INT(NPCON/10)+1
          N=1
          DO 1011 I=1,NLINES
             WRITE(OUTS,'(10I4)')(ICFFS(J),J=N,N+9)
             CALL EDISP(ITU,OUTS)
             N=N+10
 1011     CONTINUE
        ENDIF

C For components with more than one node, sort
C their connections in acsending order according
C to recieving node number.
C This is essential for storing connected components
C coefficients in the correct location of the overall
C system matrix.
 606    do 709 ipcon=1,npcon-1
           large=999
           do 710 iconn=ipcon+1,npcon
              if(ipc1(iconn).eq.ipc1(ipcon).and.
     &           ipn1(iconn).lt.ipn1(ipcon).and.
     &           ipn1(iconn).lt.large) then
                 iconnt=iconn
                 large =ipn1(iconn)
              endif
 710       continue

C Changeover connections.
           if(large.ne.999) then
              write(outs,'(A,I5,A,I5)')
     &           ' changing over connections',ipcon,' and',iconnt
              call edisp(itu,outs)
              ipct1=ipc1(ipcon)
              ipnt1=ipn1(ipcon)          
              ipctt=ipct(ipcon)
              ipct2=ipc2(ipcon)
              ipnt2=ipn2(ipcon)
              pcondt=pcondr(ipcon)
              pcons1=pconsd(ipcon,1)
              pcons2=pconsd(ipcon,2)
              ipc1(ipcon)=ipc1(iconnt)
              ipn1(ipcon)=ipn1(iconnt)
              ipct(ipcon)=ipct(iconnt)
              ipc2(ipcon)=ipc2(iconnt)
              ipn2(ipcon)=ipn2(iconnt)
              pcondr(ipcon)=pcondr(iconnt)
              pconsd(ipcon,1)=pconsd(iconnt,1)
              pconsd(ipcon,2)=pconsd(iconnt,2)
              ipc1(iconnt)=ipct1
              ipn1(iconnt)=ipnt1
              ipct(iconnt)=ipctt
              ipc2(iconnt)=ipct2
              ipn2(iconnt)=ipnt2
              pcondr(iconnt)=pcondt
              pconsd(iconnt,1)=pcons1
              pconsd(iconnt,2)=pcons2
              
C Also change over flow connection.
              if(iflwn.eq.1) then
                 ipfc=icffs(ipcon)
                 icffs(ipcon)=icffs(iconnt)
                 icffs(iconnt)=ipfc
              endif   
           endif
 709    continue

C Close plant cfg file before exiting.
    6   CALL ERPFREE(IUC,ISTAT)
        RETURN

C Error handling
  900 IER=1
      CALL USRMSG(' ',' System config file error detected.','W')
      GOTO 6
  913 CALL USRMSG(' ','Plant component connectivity error.','W')
      GOTO 900
  914 CALL USRMSG(' ','No of plant component nodes in error.','W')
      GOTO 900
  916 CALL USRMSG(' ',' Building/plant connection error.','W')
      GOTO 900
  925 CALL USRMSG(' ',' Circuit mass div ratio out of range.','W')
      GOTO 900
  950 IER1=1
      CALL USRMSG(' ',' Plant component database file error.','W')
      GOTO 900

 1001 CALL USRMSG(' Conversion error in',OUTSTR,'W')
      IER=1
      GOTO 6

 1003 CALL USRMSG(' Error reading electrical data', OUTSTR,'W')
      GOTO 900
      end      

C ************************* findpc *********************
C This subroutine finds component index by searching 
C through pcname(?) until a match is found.
      subroutine findpc(npcomp,word,ipc)
#include "plant.h"

      common/pcnam/pcname(mpcom)
      character pcname*15, word*(*)

      do 10 i=1, npcomp
         if(pcname(i)(1:15).eq.word(1:15)) then
            ipc=i
            goto 20
         endif
   10 continue
   20 return
      end


