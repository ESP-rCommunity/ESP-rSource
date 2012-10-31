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

C ************************** ENETREWR *********************************
C enetrewr.F contains the following routines:
C  ENETREAD is the routine which reads an electrical network description file
C           into the common blocks.
C  ENETWRITE is the routine which writes out an electrical network description
C           file.

C The routine can be called in three modes 'R'-read and report 'N'-read silently. 
C 'S'-read silently and don't ask questions. 

C A description of the electrical network variables follows:

C ENTYPE       - the network type 1-d.c. 2-1-phase a.c. 3-multi-phase a.c.
C                4-balanced a.c. 5-mixed phase/a.c./d.c.
C ENTYPESTR    - string holding network type description (see above)
C ENDESC       - string holding a description of the network
C PHTYPSTR     - string holding phase types 1-d.c. 2-1-phase 3-3-phase
C                4-balanced
C PHASEANG     - array holding the default phase angle for each phase
C              - usually 0, 120, 240
C
C NENOD        - number of electrical nodes in the network
C ENODNO       - electrical node reference no
C ENODNAM      - string holding electrical node name
C ENODPHTYP    - phase type of node (relates to PHTYPSTR)
C ENODPH       - specific phase number associated with the node (1-3)
C ENODTYP      - electrical node type 1-variable 2-fixed voltage
C                3-calc-PV 4-calc-plant
C ENODTYPSTR   - string holding the type description
C ENODBASEV    - node base voltage for per unit analysis
C ENODBASEP    - base power value (all nodes)
C
C NHYBCOM      - number of hybrid components connected to the network
C HYBCOMNO     - the hybrid component ref no
C HYCOMAM      - string containing the hybrid component name
C HYCOMTYP     - hybrid component type no 1-zone load, 2-pv (special material)
C                3-plant component 
C HYCOMTYPSTR  - string containing the type description
C HYCOMPHTYP   - phase type of component (relates to PHTYPSTR)
C HYCONEN      - node(s) connected to the component 
C HYLOC        - location of hybrid component zone n load m
C                special material no. n
C                plant component no.  n 
C HYDESC       - string holding a description of the hybrid component
C NHYBDAT      - number of hybrid component data items
C HYBDAT       - the hybrid data items
C
C NPOWCOM      - the number of connected power-only components
C POWCOMNO     - the power only component ref no.
C POWCOMID     - power only component database id number
C POWCOMNAM    - string holding the power-only component name
C POWCOMPHTYP  - power only component phase type (relates to PHTYPSTR)
C POWCONEN     - nodes connected to the power only component
C POWCOMDESC   - string holding a description of the power only components
C NPOWCDAT     - number of data items associated with the power only component
C NPOWCDATS    - number of string data items associated with the power only component
C POWCDATS     - string data item for a power only component i.e. file name
C POWCDAT      - power only component numerical data item
C 
C NCONECOM     - the number of connecting components
C CONECOMNO    - connecting component no.
C CONECOMID    - the connecting component database i.d.
C CONECOMNAM   - string containing connecting component name
C CONECOMPHTYP - connecting component phase type (relates to PHTYPSTR)
C CONECOMNAM   - connecting components name
C CONECOMDESC  - conecting component description
C NCONECOMDAT  - number of connecting component data items
C CONECOMDAT   - connecting component data items
C
C NECON        - number of connections
C ECONO        - connection number
C CONPHTYP     - connection phase type (relates to PHTYPSTR)
C CONPH        - connection phase 1,2,3,4-all three (d.c. -1)
C CCNO         - the connecting component
C SENOD        - starting node(s)
C EENOD        - end node(s)
C
C *********************************************************************
      SUBROUTINE ENETREAD(MODE)

#include "power.h"
#include "espriou.h"
C Common Blocks
      
      integer lnblnk  ! function definition

      COMMON/OUTIN/IUOUT,IUIN
      COMMON/FILEP/IFIL
      COMMON/POPHELP/H(60)

C Common for electrical file 
      COMMON/ENTFILE/ENTFLNAM,IENTXIST

C The electrical network
      COMMON/ENET/ENTYPE

C More network-related info
      COMMON/ENETMSC/PHASEANG(MPHAS)

C The electrical nodes
      COMMON/ENOD/NENOD,ENODNO(MENOD),ENODPHTYP(MENOD),
     &ENODPH(MENOD),ENODTYP(MENOD),ENODBASEV(MENOD),
     &ENODBASEP

C The connected hybrid components
      COMMON/HYBE/NHYBCOM,HYBCOMNO(MHYCOM),HYCOMTYP(MHYCOM),
     &HYCOMPHTYP(MHYCOM),HYCONEN(MHYCOM,MPHAS),HYLOC(MHYCOM,MPHAS),
     &NHYBDAT(MHYCOM),HYBDAT(MHYCOM,MHYDAT)

C The connected power only components
      COMMON/POWC/NPOWCOM,POWCOMNO(MPOWCOM),POWCOMID(MPOWCOM),
     &POWCOMPHTYP(MPOWCOM),POWCONEN(MPOWCOM,MPHAS),
     &NPOWCDAT(MPOWCOM),NPOWCDATS(MPOWCOM),POWCDAT(MPOWCOM,MPWDAT)

C The connecting components
      COMMON/CONCOM/NCONECOM,CONECOMNO(MCONECOM),CONECOMID(MCONECOM),
     &CONECOMPHTYP(MCONECOM),NCONECOMDAT(MCONECOM),
     &CONECOMDAT(MCONECOM,MCCDAT)

C The electrical network topology
      COMMON/ECONN/NECON,ECONO(MECON),CONPHTYP(MECON),
     &CONPH(MECON,MPHAS),CCNO(MECON),SENOD(MECON,MPHAS),
     &EENOD(MECON,MPHAS)

C The descriptive strings
      COMMON/ECDESC/ENTYPESTR(MENTYP),PHTYPSTR(MPHATY),ENDESC,
     &ENODTYPSTR(MENOD),HYDESC(MHYCOM),CONECOMDESC(MCONECOM),
     &POWCOMDESC(MPOWCOM)
     
     
C Node and component names
      COMMON/ECNAMES/ENODNAM(MENOD),HYCOMNAM(MHYCOM),
     &HYCOMTYPSTR(MHYCOM),POWCOMNAM(MPOWCOM),POWCDATS(MPOWCOM,MPCDS),
     &CONECOMNAM(MCONECOM)

C Hybrid components that calculate nodal voltages
      COMMON/calcV_LOC/iplant_calcV_LOC(MENOD),iPV_calcV_LOC(MENOD)


C Common holds the id numbers of the nodes connected to the power conditioning
C unit (a power-only component model).
C pcunode array holds the nodes to which the pcu is connected where:
C   pcunode(i,1) is the node id of the sending node
C   pcunode(i,2) is the node id of the receiving node
C   where i is the power-only component index. 
      common/pcu/pcunode(mpowcom,3)    
      integer pcunode
      integer PCU_id         !- id number of the PCU power-only component model
      parameter( PCU_id = 20 )


C Plant component node connections
C iplt_conn_nodes holds the number of nodes the plant component is connected to
C   (1 or 2). If connected to two nodes it is assumed one is AC and the other DC
C   variable set to zero for hybrid components that are not plant components.
      common/pltnds/iplt_conn_nodes(MHYCOM),iplt_dcnode_id(MHYCOM),
     &              iplt_acnode_id(MHYCOM)
      integer  iplt_conn_nodes
      integer  iplt_dcnode_id        !- node index of the DC node the plant component connects to
      integer  iplt_acnode_id        !- node index of the AC node the plant component connects to

C Type casting (all varaiables and arrays cast explicitly)
      INTEGER NENOD,ENODNO,ENODPHTYP,ENODPH,ENODTYP,NHYBCOM,HYBCOMNO,
     &HYCOMTYP,HYCOMPHTYP,HYCONEN,HYLOC,NHYBDAT,NPOWCOM,POWCOMNO,
     &POWCOMID,POWCOMPHTYP,POWCONEN,NPOWCDAT,NPOWCDATS,NCONECOM,
     &CONPH,CONECOMNO,CONECOMID,CONECOMPHTYP,NCONECOMDAT,NECON,
     &ECONO,CONPHTYP,CCNO,SENOD,EENOD,iplant_calcV_LOC,iPV_calcV_LOC

      INTEGER IENTXIST,ENTYPE,IUOUT,IUIN,IFIL,IEUNIT,IVAL,I,J,IPHV,K,
     &INODE,IHYB,IPWC,IPCDS,ICNC,ICON,ILOCT,IPHN,IDLC,INC
     
      REAL PHASEANG,ENODBASEV,ENODBASEP,HYBDAT,POWCDAT,BIG,
     &CONECOMDAT,VAL,RVHY,RVPC,RVCC

      real fENFVersionNum         !- version number of electrical network file

C Characters
      CHARACTER MODE*1
      CHARACTER ENTYPESTR*12,ENODTYPSTR*12,ENODNAM*12,HYCOMTYPSTR*12
      character HYCOMNAM*12,POWCOMNAM*12,CONECOMNAM*12,PHTYPSTR*12
      CHARACTER ENDESC*72,HYDESC*72,POWCOMDESC*72,POWCDATS*72
      character CONECOMDESC*72,ENTFLNAM*72,LFIL*72,DEFLNAM*72,H*72
      character OUTSTR*124,OUTS*124

      LOGICAL OK,DOK,CONT

      DIMENSION RVHY(MHYDAT),RVPC(MPWDAT),RVCC(MCCDAT)
      DIMENSION IVPC(3)          !- array used to read PCU sending node and receiving node

      PARAMETER (BIG=1E+15)

C Set the logical variable to allow reading
      CONT=.TRUE.

C Set the variable for the number of data items printed per line in
C reporting 
      INC=7

C Set the version number
      fENFVersionNum = 0.

C Open the file with user defined or existing filename
  2   DEFLNAM='UNKNOWN'
      IF(ENTFLNAM(1:3).NE.'   '.AND.ENTFLNAM(1:3).NE.'UNK')THEN
        LFIL=ENTFLNAM
      ELSE
        LFIL='UNKNOWN'
      ENDIF
      H(1)='A power network description file holds information on'
      H(2)='the electrical network topography, the components'
      H(3)='which connect to the network and the electrical'
      H(4)='conductors between the nodes.'
      IF(MODE.EQ.'R'.OR.MODE.EQ.'N')THEN
        CALL EASKS(LFIL,' Electrical network description file? ',' ',72,
     &  DEFLNAM,' file open',IER,4)
        IF(LFIL(1:3).NE.'   '.AND.LFIL(1:3).NE.'UNK') THEN
          ENTFLNAM=LFIL
        ELSE
          CALL usrmsg('File name was blank - try again...',' ','W')
          GOTO 2
        ENDIF
      ELSE

C Read in silently.
        IF(LFIL(1:3).NE.'   '.AND.LFIL(1:3).NE.'UNK') THEN
          WRITE(OUTS,'(A,A)')' Reading electrical network ...',
     &ENTFLNAM(1:LNBLNK(ENTFLNAM))
          CALL USRMSG(OUTS,' ','-')
        ELSE
          CALL EDISP(IUOUT,'Warning: electrical network filename was')
          CALL EDISP(IUOUT,'blank - network has not been read in')
          RETURN
        ENDIF
      ENDIF

C Open the electrical network file.
      IEUNIT=IFIL+1
      CALL EFOPSEQ(IEUNIT,ENTFLNAM,1,IER)
      IF(IER.EQ.-301) THEN
        IF(MODE.NE.'S') THEN
          dok=.true.
          h(1)='Chance to correct error in file name. '
          WRITE(OUTS,'(3A)')
     &  'File ',LFIL(1:LNBLNK(LFIL)),' is as new network description.'
          CALL ASKOK(OUTS,'Create a new electrical network?',OK,dok,1)
          IF(OK) THEN
            RETURN
          ELSE
            GOTO 2
          ENDIF
        ELSE
          CALL EDISP(IUOUT,'Warning: electrical network filename was')
          CALL EDISP(IUOUT,'blank - network has not been read in')
          RETURN
        ENDIF
      ELSEIF(IER.NE.0.AND.IER.NE.-301) THEN
        dok=.true.
        h(1)='Chance to correct for error in file name or location.'
        WRITE(OUTS,'(3A)')
     &  ' Problem opening ',LFIL(1:LNBLNK(LFIL)),'.'
        CALL ASKOK(OUTS,' Try again ?',OK,dok,1)
        IF(OK) THEN
          GOTO 2
        ELSE
          RETURN
        ENDIF
      ENDIF

      CALL EDISP(IUOUT,' ')    
      WRITE(OUTS,'(A,A)') 
     &' Opened electrical network file: ',LFIL(1:LNBLNK(LFIL)) 
      CALL USRMSG(OUTS,' ','-')  
      write(currentfile,'(a)') LFIL(1:lnblnk(LFIL))
        
C Read the rest of the file.
 20   CALL STRIPC(IEUNIT,OUTSTR,99,ND,0,'check file',IER)
      IF(IER.NE.0) CONT=.FALSE. 

C Read the file version number (implemented in April 2005), if applicable
      if ( outstr(1:13).eq.'* FileVersion' ) then
        call stripc(ieunit,outstr,99,nd,0,'file ver',ier)
        k = 0
        call egetwr(outstr,k,fENFVersionNum,0.01,100.,'F',
     &              'version number',IER)

C Read the file data
      elseif(OUTSTR(1:6).EQ.'* elec') then
        CALL STRIPC(IEUNIT,OUTSTR,99,ND,0,'check file',IER)
        CALL EDISP(IUOUT,' ')

C Echo the .cfg file associated with the electrical network file.
        IF(MODE.EQ.'R') THEN
          WRITE(OUTS,'(2A)')
     &      ' Electrical network file associated with: ',
     &      OUTSTR(1:LNBLNK(OUTSTR))
          CALL EDISP(IUOUT,OUTS)
        ENDIF

C Read the electrical network description
        CALL STRIPC(IEUNIT,OUTSTR,99,ND,0,'check file',IER)
        IF(IER.NE.0) CONT=.FALSE.

C Reporting 
        WRITE(ENDESC,'(A)') OUTSTR(3:72)
        IF(MODE.EQ.'R') THEN
          CALL EDISP(IUOUT,' ')
          WRITE(OUTS,'(A)') ' Description: '
          CALL EDISP(IUOUT,OUTS)
          WRITE(OUTS,'(A)') ENDESC
          CALL EDISP(IUOUT,OUTS)
        ENDIF
      ELSEIF(OUTSTR(1:5).EQ.'* net') THEN
        CALL STRIPC(IEUNIT,OUTSTR,99,ND,0,'check file',IER)
        IF(IER.NE.0) CONT=.FALSE. 
        IF(OUTSTR(1:LNBLNK(OUTSTR)).EQ.' d.c.')THEN
          ENTYPE=1 
        ELSEIF(OUTSTR(1:LNBLNK(OUTSTR)).EQ.'  1-phase')THEN
          ENTYPE=2 
        ELSEIF(OUTSTR(1:LNBLNK(OUTSTR)).EQ.'  2-phase')THEN
          ENTYPE=3
        ELSEIF(OUTSTR(1:LNBLNK(OUTSTR)).EQ.'  3-phase')THEN
          ENTYPE=4 
        ELSEIF(OUTSTR(1:LNBLNK(OUTSTR)).EQ.'  balanced')THEN
          ENTYPE=5
        ELSEIF(OUTSTR(1:LNBLNK(OUTSTR)).EQ.'  mixed')THEN
          ENTYPE=6 
        ELSE
          WRITE(OUTS,'(A)') 'Error: network type not recognised.'
          CALL EDISP(IUOUT,OUTS)
          RETURN 
        ENDIF 
        WRITE(ENTYPESTR,'(A)')OUTSTR(1:12)
        IF(MODE.EQ.'R') THEN
          CALL EDISP(IUOUT,' ')
          WRITE(OUTS,'(A,A)') ' Network type: ',
     &      OUTSTR(1:LNBLNK(OUTSTR))          
          CALL EDISP(IUOUT,OUTS) 
        ENDIF
C Read the base power value
      ELSEIF(OUTSTR(1:6).EQ.'* base') THEN
        CALL STRIPC(IEUNIT,OUTSTR,99,ND,0,'check file',IER)
        IF(IER.NE.0) CONT=.FALSE.
        K=0
        CALL EGETWR(OUTSTR,K,VAL,0.,0.,'-','base VA',IER)
        IF(IER.NE.0) CONT=.FALSE.
        ENODBASEP=VAL

C Read the phase angles
      ELSEIF(OUTSTR(1:6).EQ.'* phas') THEN
        CALL STRIPC(IEUNIT,OUTSTR,99,ND,0,'check file',IER)
        IF(IER.NE.0) CONT=.FALSE.
        K=0
        DO 23 IPH=1,ND
          CALL EGETWR(OUTSTR,K,VAL,0.,360.,'-','base VA',IER)
          IF(IER.NE.0) CONT=.FALSE.
          PHASEANG(IPH)=VAL
  23    CONTINUE

C Reporting
        IF(MODE.EQ.'R') THEN
          CALL EDISP(IUOUT,' ')
          WRITE(OUTS,'(A)') 'Phase angles for each phase:'
          CALL EDISP(IUOUT,OUTS)
          WRITE(OUTS,'(10(2X,F7.2))') (PHASEANG(IPH),IPH=1,MPHAS)
          CALL EDISP(IUOUT,OUTS) 
        ENDIF

C Read in the nodal data       
      ELSEIF(OUTSTR(1:6).EQ.'* node') THEN
        CONT=.TRUE.
        CALL STRIPC(IEUNIT,OUTSTR,99,ND,0,'check file',IER)
        IF(IER.NE.0) CONT=.FALSE.
        K=0 
        CALL EGETWI(OUTSTR,K,IVAL,0,MENOD,'W','no. nodes',IER)
        IF(IER.NE.0) CONT=.FALSE.     
        NENOD=IVAL
        IF(MODE.EQ.'R') THEN
          CALL EDISP(IUOUT,' ')
          WRITE(OUTS,'(A,I3)') ' Number of electrical nodes: ', NENOD
          CALL EDISP(IUOUT,OUTS)
        ENDIF 

C Loop through the node data
        DO 30 INODE=1,NENOD
          CALL STRIPC(IEUNIT,OUTSTR,99,ND,0,'check file',IER) 
          IF(ND.NE.6.OR.IER.GT.0) CONT=.FALSE.
          K=0
          CALL EGETWI(OUTSTR,K,IVAL,0,MENOD,'W','node no.',IER) 
          ENODNO(INODE)=IVAL
          CALL EGETW(OUTSTR,K,OUTS,'W','node name.',IER) 
          WRITE(ENODNAM(INODE),'(A)') OUTS(1:12)
          CALL EGETW(OUTSTR,K,OUTS,'W','phase type',IER)
 
C Decode the string with the phase type
          CALL PHDECODE(OUTS,IPHV,1,IER)
          ENODPHTYP(INODE)=IPHV
          IF(IER.NE.0) CONT=.FALSE.
         
          CALL EGETWI(OUTSTR,K,IVAL,0,MPHAS,'W','phase no.',IER)
          ENODPH(INODE)=IVAL 
          CALL EGETW(OUTSTR,K,OUTS,'W','node type',IER)  
          IF(OUTS(1:LNBLNK(OUTS)).EQ.'variable') THEN
            ENODTYP(INODE)=1 
            ENODTYPSTR(INODE)= OUTS(1:LNBLNK(OUTS))
          ELSEIF(OUTS(1:LNBLNK(OUTS)).EQ.'fixed_V') THEN
            ENODTYP(INODE)=2 
            ENODTYPSTR(INODE)= OUTS(1:LNBLNK(OUTS))
          ELSEIF(OUTS(1:LNBLNK(OUTS)).EQ.'calc_PV') THEN
            ENODTYP(INODE)=3 
            ENODTYPSTR(INODE)= OUTS(1:LNBLNK(OUTS))
          ELSEIF(OUTS(1:LNBLNK(OUTS)).EQ.'calc_plant') THEN
            ENODTYP(INODE)=4 
            ENODTYPSTR(INODE)= OUTS(1:LNBLNK(OUTS))
          ELSE
            WRITE(OUTS,'(A,A)') 
     &      OUTS(1:LNBLNK(OUTS)),'is not a known node type.'
            RETURN
          ENDIF                
          CALL EGETWR(OUTSTR,K,VAL,0.,BIG,'W','base V.',IER)
          ENODBASEV(INODE)=VAL

C Get the PV or plant component number that calculates the nodal voltage
C (if applicable).
          IF( ENODTYP(INODE).eq.3 ) THEN
            CALL EGETWI(OUTSTR,K,IVAL,0,0,'-','plant comp no.',IER)
            iPV_calcV_LOC(INODE) = IVAL
          ELSEIF( ENODTYP(INODE).eq.4 ) THEN
            CALL EGETWI(OUTSTR,K,IVAL,0,0,'-','PV comp no.',IER)
            iplant_calcV_LOC(INODE) = IVAL
          ENDIF

C Reporting
          IF(MODE.EQ.'R') THEN
            WRITE(OUTS,'(A,A)')
     &      ' Node no.  Node name    Phase type  Phase Node type    ',
     &      'Base volt      Comp'
            CALL EDISP(IUOUT,OUTS)   
            WRITE(OUTS,1111) ENODNO(INODE),ENODNAM(INODE),
     &      PHTYPSTR(ENODPHTYP(INODE)),ENODPH(INODE),ENODTYPSTR(INODE),
     &      ENODBASEV(INODE)
 1111       FORMAT(5X,I3,2X,A12,2X,A12,2X,I3,2X,A12,2X,F10.2)
            CALL EDISP(IUOUT,OUTS)
          ENDIF
          CONT=.TRUE.
  30    CONTINUE

C Read the hybrid component data
      ELSEIF(OUTSTR(1:6).EQ.'* hybr') THEN
        CONT=.TRUE. 
        CALL STRIPC(IEUNIT,OUTSTR,99,ND,0,'no. hybrid',IER)
        IF(IER.NE.0) CONT=.FALSE.
        K=0
        CALL EGETWI(OUTSTR,K,IVAL,0,MHYCOM,'W','int. hyb',IER)
        NHYBCOM=IVAL
        IF(MODE.EQ.'R') THEN
          CALL EDISP(IUOUT,' ')
          WRITE(OUTS,'(A,I3)')' Number of hybrid components: ',NHYBCOM
          CALL EDISP(IUOUT,OUTS)
        ENDIF

        DO 40 IHYB=1,NHYBCOM
          CALL STRIPC(IEUNIT,OUTSTR,99,ND,0,'hybrid dat',IER)
          IF(IER.NE.0) CONT=.FALSE.
          K=0 
          CALL EGETWI(OUTSTR,K,IVAL,0,MHYCOM,'W','hybc no.',IER)
          HYBCOMNO(IHYB)=IVAL

C Read and set the hybrid component type
          CALL EGETW(OUTSTR,K,OUTS,'W','comp. type',IER)
          HYCOMTYPSTR(IHYB)=OUTS(1:LNBLNK(OUTS))
          IF(OUTS(1:LNBLNK(OUTS)).EQ.'zone') THEN
            HYCOMTYP(IHYB)=1
          ELSEIF(OUTS(1:LNBLNK(OUTS)).EQ.'spmaterial') THEN 
            HYCOMTYP(IHYB)=2 
          ELSEIF(OUTS(1:LNBLNK(OUTS)).EQ.'plant') THEN 
            HYCOMTYP(IHYB)=3
          ELSEIF(OUTS(1:LNBLNK(OUTS)).EQ.'flow') THEN
            HYCOMTYP(IHYB)=4
          ELSE
            CALL EDISP(IUOUT,' ') 
            WRITE(OUTS,'(A)') ' Undefined hybrid component defined.'
            CALL EDISP(IUOUT,OUTS) 
            RETURN
          ENDIF
     
          CALL EGETW(OUTSTR,K,OUTS,'W','comp. name',IER)
          IF(IER.NE.0) CONT=.FALSE.
          WRITE(HYCOMNAM(IHYB),'(A)') OUTS(1:12)

C Decode the phase type 
          CALL EGETW(OUTSTR,K,OUTS,'W','phase. type',IER)
          CALL PHDECODE(OUTS,IPHV,1,IER)
          IF(IER.NE.0) CONT=.FALSE.
          HYCOMPHTYP(IHYB)=IPHV

C Read the nodes to which the component connects
          DO 32 IPHN=1,MPHAS
            CALL EGETWI(OUTSTR,K,IVAL,0,MENOD,'W',
     &      '3-con comp',IER) 
            HYCONEN(IHYB,IPHN)=IVAL 
 32       CONTINUE

C Read the component location data
          DO 34 ILOCT=1,3
            CALL EGETWI(OUTSTR,K,IVAL,0,0,'-','con loc',IER)  
            HYLOC(IHYB,ILOCT)=IVAL
 34       CONTINUE

C Reporting
          IF(MODE.EQ.'R') THEN
            CALL EDISP(IUOUT,' ')
            WRITE(OUTS,'(A)')
     &' Comp. no. Comp. type    Comp. name    Phase type '
            CALL EDISP(IUOUT,OUTS)   
            WRITE(OUTS,1112) HYBCOMNO(IHYB),HYCOMTYPSTR(IHYB),
     &      HYCOMNAM(IHYB),PHTYPSTR(HYCOMPHTYP(IHYB))
 1112       FORMAT(5X,I3,2X,A12,2X,A12,2X,A12)
            CALL EDISP(IUOUT,OUTS)  
            WRITE(OUTS,'(A)')
     &' Nodal connections and comp. location:'
            CALL EDISP(IUOUT,OUTS) 
            WRITE(OUTS,1113) (HYCONEN(IHYB,I),I=1,3),
     &       (HYLOC(IHYB,J),J=1,3)
 1113       FORMAT(1X,3(2X,I3),5X,3(2X,I3))
            CALL EDISP(IUOUT,OUTS)
          ENDIF


C Read the hybrid plant component connections if necessary
          if ( fENFVersionNum.ge.1.00 .and. hycomtyp(ihyb) .eq. 3 ) then
            call stripc(ieunit,outstr,99,nd,0,'plt node conn',IER)
            k = 0
            call egetwi(outstr,k,ival,0,2,'W','pltcon',IER)
            iplt_conn_nodes(ihyb) = ival
            call egetwi(outstr,k,ival,0,menod,'W','dcnode',IER)
            iplt_dcnode_id(ihyb) = ival
            call egetwi(outstr,k,ival,0,menod,'W','acnode',IER)
            iplt_acnode_id(ihyb) = ival

C Reporting
            if ( mode.eq.'R' ) then
              call edisp(iuout,' ')
              if ( iplt_conn_nodes(ihyb).eq.2 ) then
                write(outs,'(A,I3,A)')
     &            ' The hybrid plant component is connected to ', 
     &            iplt_conn_nodes(ihyb), ' nodes.'
                call edisp(iuout,outs)
                write(outs,'(A,I3,A,I3)')
     &            ' DC node id: ', iplt_dcnode_id(ihyb),
     &            ' AC node id: ', iplt_acnode_id(ihyb)
                call edisp(iuout,outs)
              endif
            endif
          endif

C Read the hybrid component description
          CALL STRIPC(IEUNIT,OUTSTR,99,ND,0,'hybrid dat',IER) 
          IF(IER.NE.0) CONT=.FALSE.   
          WRITE(HYDESC(IHYB),'(A)') OUTSTR(3:72)
          IF(MODE.EQ.'R') THEN
            WRITE(OUTS,'(A)') ' Description: '
            CALL EDISP(IUOUT,OUTS)
            WRITE(OUTS,'(1X,A)') HYDESC(IHYB)
            CALL EDISP(IUOUT,OUTS)
          ENDIF

C Read the number of additional data items
          CALL STRIPC(IEUNIT,OUTSTR,99,ND,0,'hybrid dat',IER) 
          K=0   
          CALL EGETWI(OUTSTR,K,IVAL,0,MHYDAT,'W','hybdat',IER)
            NHYBDAT(IHYB)=IVAL
          IF(NHYBDAT(IHYB).GT.0) THEN
            CALL EGETWRA(IEUNIT,RVHY,NHYBDAT(IHYB),0.,0.,'-',
     &      'hybdat',IER)
            DO 36  I=1,NHYBDAT(IHYB)
              HYBDAT(IHYB,I)=RVHY(I)
36          CONTINUE
          ENDIF

C Reporting 
          IF(MODE.EQ.'R') THEN
            CALL EDISP(IUOUT,' ')
            WRITE(OUTS,'(A,I3,A)') 
     &      ' Component has ',NHYBDAT(IHYB),' data items:'
            CALL EDISP(IUOUT,OUTS) 
            IF(NHYBDAT(IHYB).GT.0) THEN
              IF(NHYBDAT(IHYB).LE.INC+1) THEN
                WRITE(OUTS,1114)
     &          (HYBDAT(IHYB,I),I=1,NHYBDAT(IHYB))
                CALL EDISP(IUOUT,OUTS)
              ELSE
                IDLC=1
38              WRITE(OUTS,1114)
     &          (HYBDAT(IHYB,I),I=IDLC,IDLC+INC)
1114            FORMAT(1X,8(2X,G12.5))
                CALL EDISP(IUOUT,OUTS)
                IDLC=IDLC+INC+1
                IF((IDLC+INC).GT.NHYBDAT(IHYB)) 
     &          INC=NHYBDAT(IHYB)-IDLC
                IF(IDLC.LE.NHYBDAT(IHYB).AND.IDLC.LT.MHYDAT) GOTO 38
                INC=7
              ENDIF
            ENDIF
          ENDIF
 40     CONTINUE 

          
C Read the power only component data
      ELSEIF(OUTSTR(1:6).EQ.'* powe')THEN
        CONT=.TRUE.
        CALL STRIPC(IEUNIT,OUTSTR,99,ND,0,'no. powerd',IER)
        IF(IER.NE.0) CONT=.FALSE.
        K=0
        CALL EGETWI(OUTSTR,K,IVAL,0,MHYCOM,'W','int. powc',IER)
        NPOWCOM=IVAL
        IF(MODE.EQ.'R') THEN
          CALL EDISP(IUOUT,' ')
          WRITE(OUTS,'(A,I3)')
     &    ' Number of power only components: ',NPOWCOM
          CALL EDISP(IUOUT,OUTS)
        ENDIF

        DO 50 IPWC=1,NPOWCOM
          CALL STRIPC(IEUNIT,OUTSTR,99,ND,0,'powerc dat',IER)
          IF(IER.NE.0) CONT=.FALSE.
          K=0 
          CALL EGETWI(OUTSTR,K,IVAL,0,MPOWCOM,'W','hybc no.',IER)
          POWCOMNO(IPWC)=IVAL

C Get the component database ID
          CALL EGETWI(OUTSTR,K,IVAL,0,0,'-','pwoc no.',IER)
          POWCOMID(IPWC)=IVAL

C Read the component name
          CALL EGETW(OUTSTR,K,OUTS,'W','powoc. type',IER)
          WRITE(POWCOMNAM(IPWC),'(A)') OUTS(1:12)

C Read and decode the phase type     
          CALL EGETW(OUTSTR,K,OUTS,'W','phase. type',IER)
          CALL PHDECODE(OUTS,IPHV,1,IER)
          IF(IER.NE.0) CONT=.FALSE.
          POWCOMPHTYP(IPWC)=IPHV

C Reporting 
          IF(MODE.EQ.'R') THEN
            CALL EDISP(IUOUT,' ')
            WRITE(OUTS,'(A)')
     &' Comp No. i.d.  Comp. name   Phase type  '
            CALL EDISP(IUOUT,OUTS)
            WRITE(OUTS,1115)
     &POWCOMNO(IPWC),POWCOMID(IPWC),POWCOMNAM(IPWC),
     &PHTYPSTR(POWCOMPHTYP(IPWC))
 1115       FORMAT(5X,I3,2X,I3,2X,A12,2X,A12)
            CALL EDISP(IUOUT,OUTS)
          ENDIF

C Read the nodes to which the component connects
          DO 42 IPHN=1,MPHAS
            CALL EGETWI(OUTSTR,K,IVAL,0,MENOD,'W',
     &      '3-con comp',IER) 
            POWCONEN(IPWC,IPHN)=IVAL 
 42       CONTINUE


C Reporting
          IF(MODE.EQ.'R') THEN
            CALL EDISP(IUOUT,' ')
            WRITE(OUTS,'(A)')' Connected to network nodes: '
            CALL EDISP(IUOUT,OUTS)
            WRITE(OUTS,'(3X,3(2X,I3))') (POWCONEN(IPWC,I),I=1,MPHAS)
            CALL EDISP(IUOUT,OUTS)
          ENDIF

C Read the power component description
          CALL STRIPC(IEUNIT,OUTSTR,99,ND,0,'powc dat',IER) 
          IF(IER.NE.0) CONT=.FALSE.   
          WRITE(POWCOMDESC(IPWC),'(A)') OUTSTR(3:72)

          IF(MODE.EQ.'R') THEN
            CALL EDISP(IUOUT,' ') 
            WRITE(OUTS,'(A)') ' Description: '
            CALL EDISP(IUOUT,OUTS)
            WRITE(OUTS,'(1X,A)') POWCOMDESC(IPWC)
            CALL EDISP(IUOUT,OUTS)
          ENDIF

C Read the number of additional data items
          CALL STRIPC(IEUNIT,OUTSTR,99,ND,0,'hybrid dat',IER)
          IF(IER.NE.0) CONT=.FALSE.   
          K=0 
C Get the number of data items  
          CALL EGETWI(OUTSTR,K,IVAL,0,MPWDAT,'W','pwcdat',IER)
            NPOWCDAT(IPWC)=IVAL

C Get the number of data strings
          CALL EGETWI(OUTSTR,K,IVAL,0,MPCDS,'W','pwcdats',IER)
             NPOWCDATS(IPWC)=IVAL

C Begin reading the data
          IF(NPOWCDAT(IPWC).GT.0) THEN
            CALL EGETWRA(IEUNIT,RVPC,NPOWCDAT(IPWC),0.,0.,'-',
     &      'hybdat',IER) 
            DO 44 IDLC=1,NPOWCDAT(IPWC)
              POWCDAT(IPWC,IDLC)=RVPC(IDLC)
 44         CONTINUE
          ENDIF
          IF(NPOWCDATS(IPWC).GT.0) THEN

C Read the any additional string data items
            CALL STRIPC(IEUNIT,OUTSTR,99,ND,0,'pwcrid dat',IER)
            IF(IER.NE.0) CONT=.FALSE. 
            K=0
            DO 46 IPCDS=1,NPOWCDATS(IPWC)
              CALL EGETW(OUTSTR,K,OUTS,'W','powc. dats',IER) 
              POWCDATS(IPWC,IPCDS)=OUTS(1:LNBLNK(OUTS))
 46         CONTINUE               
          ENDIF

C Reporting 
          IF(MODE.EQ.'R') THEN
            CALL EDISP(IUOUT,' ')
            WRITE(OUTS,'(A,I3,A)') 
     &      ' Component has ',NPOWCDAT(IPWC),' data items:'
            CALL EDISP(IUOUT,OUTS) 
            IF(NPOWCDAT(IPWC).GT.0) THEN
              IF(NPOWCDAT(IPWC).LE.INC+1) THEN
                WRITE(OUTS,1116)
     &          (POWCDAT(IPWC,I),I=1,NPOWCDAT(IPWC))
                CALL EDISP(IUOUT,OUTS)
              ELSE
                IDLC=1
48              WRITE(OUTS,1116) (POWCDAT(IPWC,I),I=IDLC,IDLC+INC)
1116            FORMAT(1X,8(2X,G12.5))
                CALL EDISP(IUOUT,OUTS)
                IDLC=IDLC+INC+1
                IF((IDLC+INC).GT.NPOWCDAT(IPWC)) 
     &            INC=NPOWCDAT(IPWC)-IDLC
                IF(IDLC.LE.NPOWCDAT(IPWC).AND.IDLC.LT.MPWDAT) GOTO 48
                INC=7
              ENDIF
            ENDIF
            IF(NPOWCDATS(IPWC).GT.0) THEN
              CALL EDISP(IUOUT,' ')
              WRITE(OUTS,'(A,I3,A)') 
     &        ' Component has ',NPOWCDATS(IPWC),' string data items:'
              CALL EDISP(IUOUT,OUTS)   
              DO 49 I=1, NPOWCDATS(IPWC)
                WRITE(OUTS,'(1X,A)') POWCDATS(IPWC,I) 
                CALL EDISP(IUOUT,OUTS)
 49           CONTINUE 
            ENDIF       
          ENDIF

C if power-only component is a PCU, read the id of the node sending current to
C the PCU and the node receiving current from the PCU
          if ( POWCOMID(IPWC) .eq. PCU_id ) then
            call egetwia(IEUNIT,IVPC,2,1,MENOD,'W',
     &      'pcudat',IER)
            do 80 i = 1,2
              pcunode(IPWC,i) = IVPC(i)
 80         continue

C Reporting
            if ( mode .eq. 'R' ) then
              call edisp(iuout,' ')
              write(outs,'(A,I3)')
     &        'ID of node sending current to PCU: ',pcunode(IPWC,1)
              call edisp(iuout,outs)
C             call edisp(iuout,' ')
              write(outs,'(A,I3)')
     &        'ID of node receiving current from PCU: ',pcunode(IPWC,2)
              call edisp(iuout,outs)
             endif
          endif

 50     CONTINUE
    
C Read the connecting component data
      ELSEIF(OUTSTR(1:12).EQ.'* connecting')THEN
        CONT=.TRUE.
        CALL STRIPC(IEUNIT,OUTSTR,99,ND,0,'no. conc',IER)
        IF(IER.NE.0) CONT=.FALSE.
        K=0
        CALL EGETWI(OUTSTR,K,IVAL,0,MCONECOM,'W','int. powc',IER)
        NCONECOM=IVAL
        IF(MODE.EQ.'R') THEN
          CALL EDISP(IUOUT,' ')
          WRITE(OUTS,'(A,I3)')
     &    ' Number of connecting components: ',NCONECOM
          CALL EDISP(IUOUT,OUTS)
        ENDIF

        DO 60 ICNC=1,NCONECOM
          CALL STRIPC(IEUNIT,OUTSTR,99,ND,0,'powerc dat',IER)
          IF(IER.NE.0) CONT=.FALSE.
          K=0 
          CALL EGETWI(OUTSTR,K,IVAL,0,MCONECOM,'W','hybcno.',IER)
          CONECOMNO(ICNC)=IVAL

C Get the component database ID
          CALL EGETWI(OUTSTR,K,IVAL,0,0,'-','pwoc no.',IER)
          CONECOMID(ICNC)=IVAL

C Read the component name
          CALL EGETW(OUTSTR,K,OUTS,'W','powoc. type',IER)
          WRITE(CONECOMNAM(CONECOMNO(ICNC)),'(A12)') OUTS(1:12)

C Read and decode the phase type     
          CALL EGETW(OUTSTR,K,OUTS,'W','phase. type',IER)
          CALL PHDECODE(OUTS,IPHV,1,IER)
          IF(IER.NE.0) CONT=.FALSE.
          CONECOMPHTYP(ICNC)=IPHV

C Reporting 
          IF(MODE.EQ.'R') THEN
            CALL EDISP(IUOUT,' ')
            WRITE(OUTS,'(A)')
     &' Comp No. i.d. Comp. name    Phase type  '
            CALL EDISP(IUOUT,OUTS)
            WRITE(OUTS,1117)CONECOMNO(ICNC),CONECOMID(ICNC),
     &CONECOMNAM(ICNC),PHTYPSTR(CONECOMPHTYP(ICNC))
 1117       FORMAT(5X,I3,2X,I3,2X,A12,2X,A12)
            CALL EDISP(IUOUT,OUTS)
          ENDIF

C Read the connecting component description
          CALL STRIPC(IEUNIT,OUTSTR,99,ND,0,'conc desc',IER) 
          IF(IER.NE.0) CONT=.FALSE.   
          WRITE(CONECOMDESC(ICNC),'(A)') OUTSTR(3:72)

C Reporting
          IF(MODE.EQ.'R') THEN
            CALL EDISP(IUOUT,' ')
            WRITE(OUTS,'(A)') ' Description: '
            CALL EDISP(IUOUT,OUTS)
            WRITE(OUTS,'(1X,A)')CONECOMDESC(ICNC)
            CALL EDISP(IUOUT,OUTS)
          ENDIF

C Read the number of additional data items
          CALL STRIPC(IEUNIT,OUTSTR,99,ND,0,'conc dat',IER)
          IF(IER.NE.0) CONT=.FALSE.   
          K=0   
          CALL EGETWI(OUTSTR,K,IVAL,0,MCCDAT,'W','pwcdat',IER)
          NCONECOMDAT(ICNC)=IVAL
          IF(NCONECOMDAT(ICNC).GT.0) THEN
            CALL EGETWRA(IEUNIT,RVCC,NCONECOMDAT(ICNC),0.,0.,'-',
     &      'concdat',IER) 
             DO 54 I=1,NCONECOMDAT(ICNC)
               CONECOMDAT(ICNC,I)=RVCC(I)
 54          CONTINUE
          ENDIF

C Reporting
          IF(MODE.EQ.'R') THEN
              CALL EDISP(IUOUT,' ')
              WRITE(OUTS,'(A,I3,A)') 
     &        ' Component has ',NCONECOMDAT(ICNC),' data items:'
              CALL EDISP(IUOUT,OUTS)
            IF(NCONECOMDAT(ICNC).GT.0) THEN
              IF(NCONECOMDAT(ICNC).LE.INC+1) THEN
                WRITE(OUTS,1118)
     &          (CONECOMDAT(ICNC,I),I=1,NCONECOMDAT(ICNC))
                CALL EDISP(IUOUT,OUTS)
              ELSE
                IDLC=1
 56             WRITE(OUTS,1118)
     &          (CONECOMDAT(ICNC,I),I=IDLC,IDLC+INC)
1118            FORMAT(1X,8(2X,G12.5))
                CALL EDISP(IUOUT,OUTS)
                IDLC=IDLC+INC+1
                IF((IDLC+INC).GT.NCONECOMDAT(ICNC)) 
     &            INC=NCONECOMDAT(ICNC)-IDLC
                IF(IDLC.LE.NCONECOMDAT(ICNC).AND.IDLC.LT.MCCDAT)GOTO 56
                INC=7
              ENDIF
            ENDIF
          ENDIF

 60     CONTINUE 

C Read the network topology
      ELSE IF(OUTSTR(1:13).EQ.'* connections') THEN
        CONT=.TRUE.
        CALL STRIPC(IEUNIT,OUTSTR,99,ND,0,'no. cons',IER)
        IF(IER.NE.0) CONT=.FALSE.
        K=0
        CALL EGETWI(OUTSTR,K,IVAL,0,MECON,'W','int. ncon',IER)
        NECON=IVAL
        IF(MODE.EQ.'R') THEN
          CALL EDISP(IUOUT,' ')
          WRITE(OUTS,'(A,I3)')
     &    ' Number of connections: ',NECON
          CALL EDISP(IUOUT,OUTS)
        ENDIF

        DO 70 ICON=1,NECON
          CALL STRIPC(IEUNIT,OUTSTR,99,ND,0,'con dat',IER)
          IF(IER.NE.0) CONT=.FALSE.
          K=0 
          CALL EGETWI(OUTSTR,K,IVAL,0,MCONECOM,'W','hybc no.',IER)
          ECONO(ICON)=IVAL


C Read and decode the connection phase type     
          CALL EGETW(OUTSTR,K,OUTS,'W','phase. type',IER)
          CALL PHDECODE(OUTS,IPHV,1,IER)
          IF(IER.NE.0) CONT=.FALSE.
          CONPHTYP(ICON)=IPHV

C Read the phase sequence 
          DO 61 IPHN=1,3
            CALL EGETWI(OUTSTR,K,IVAL,0,MPHAS,'W','phas no.',IER)
            CONPH(ICON,IPHN)=IVAL
 61       CONTINUE

C Get the connecting component ID
          CALL EGETWI(OUTSTR,K,IVAL,0,0,'-','pwoc no.',IER)
          CCNO(ICON)=IVAL

C Reporting 
          IF(MODE.EQ.'R') THEN
            CALL EDISP(IUOUT,' ') 
            WRITE(OUTS,'(A)')
     &      ' Conn. no. Conn. type  Phase. Conn. comp.'
            CALL EDISP(IUOUT,OUTS) 
            WRITE(OUTS,1120) ECONO(ICON),PHTYPSTR(CONPHTYP(ICON)),
     &      (CONPH(ICON,I),I=1,3),CONECOMNAM(CCNO(ICON))
 1120       FORMAT(5X,I3,2X,A12,1X,I1,1X,I1,1X,I1,1X,A12)
            CALL EDISP(IUOUT,OUTS) 
          ENDIF

C Read the nodes to which the component connects
          DO 62 IPHN=1,MPHAS
            CALL EGETWI(OUTSTR,K,IVAL,0,MENOD,'-',
     &      '3-con comp',IER) 
            SENOD(ICON,IPHN)=IVAL 
 62       CONTINUE
          DO 64 IPHN=1,MPHAS
            CALL EGETWI(OUTSTR,K,IVAL,0,MENOD,'-',
     &      '3-con comp',IER) 
            EENOD(ICON,IPHN)=IVAL 
 64       CONTINUE

C Reporting 
          IF(MODE.EQ.'R') THEN  
            WRITE(OUTS,'(A)')
     &' Connects electrical node(s):' 
            CALL EDISP(IUOUT,OUTS)
            WRITE(OUTS,1119) 
     &      (SENOD(ICON,I),I=1,MPHAS),'to ->',(EENOD(ICON,J),J=1,MPHAS)
 1119       FORMAT(1X,3(I3,2X),A5,2X,3(I3,2X)) 
            CALL EDISP(IUOUT,OUTS)         
          ENDIF  
 70     CONTINUE 

C End of file reached and sucessful read
      ELSE IF(OUTSTR(1:6).EQ.'** ele') THEN
        WRITE(OUTS,'(A)') ' File read successfully.'
        CALL USRMSG(OUTS,' ','-')   
        IENTXIST=1
        CLOSE(IEUNIT)
        RETURN

C Decription endings
      ELSE IF(OUTSTR(1:6).EQ.'** end'.AND.IER.EQ.0) THEN
        CONT=.TRUE.
      ELSE
        CALL EDISP(IUOUT,' ')
        WRITE(OUTS,'(A)') ' File not read sucessfully, aborting.'
        CALL EDISP(IUOUT,OUTS)
        CLOSE(IEUNIT)
        RETURN
      ENDIF
  
C Check if read continues, if error return.
      IF(CONT) GOTO 20

      CALL EDISP(IUOUT,' Error reading the power network file.')
      CLOSE(IEUNIT)
      RETURN

      END

C *********************************************************************
C ENETWRITE writes out a user defined power network file. The variables
C used are described in the complimentary routine ENETREAD.
C *********************************************************************

      SUBROUTINE ENETWRITE

#include "building.h"
#include "model.h"
#include "power.h"
      
      integer lnblnk  ! function definition

C Common Blocks

      COMMON/OUTIN/IUOUT,IUIN
      COMMON/FILEP/IFIL
      COMMON/POPHELP/H(60)

C Common for electrical file 
      COMMON/ENTFILE/ENTFLNAM,IENTXIST

C The electrical network
      COMMON/ENET/ENTYPE

C More network-related info
      COMMON/ENETMSC/PHASEANG(MPHAS)

C The electrical nodes
      COMMON/ENOD/NENOD,ENODNO(MENOD),ENODPHTYP(MENOD),
     &ENODPH(MENOD),ENODTYP(MENOD),ENODBASEV(MENOD),
     &ENODBASEP

C The connected hybrid components
      COMMON/HYBE/NHYBCOM,HYBCOMNO(MHYCOM),HYCOMTYP(MHYCOM),
     &HYCOMPHTYP(MHYCOM),HYCONEN(MHYCOM,MPHAS),HYLOC(MHYCOM,MPHAS),
     &NHYBDAT(MHYCOM),HYBDAT(MHYCOM,MHYDAT)

C The connected power only components
      COMMON/POWC/NPOWCOM,POWCOMNO(MPOWCOM),POWCOMID(MPOWCOM),
     &POWCOMPHTYP(MPOWCOM),POWCONEN(MPOWCOM,MPHAS),
     &NPOWCDAT(MPOWCOM),NPOWCDATS(MPOWCOM),POWCDAT(MPOWCOM,MPWDAT)

C The connecting components
      COMMON/CONCOM/NCONECOM,CONECOMNO(MCONECOM),CONECOMID(MCONECOM),
     &CONECOMPHTYP(MCONECOM),NCONECOMDAT(MCONECOM),
     &CONECOMDAT(MCONECOM,MCCDAT)

C The electrical network topology
      COMMON/ECONN/NECON,ECONO(MECON),CONPHTYP(MECON),
     &CONPH(MECON,MPHAS),CCNO(MECON),SENOD(MECON,MPHAS),
     &EENOD(MECON,MPHAS)

C The descriptive strings
      COMMON/ECDESC/ENTYPESTR(MENTYP),PHTYPSTR(MPHATY),ENDESC,
     &ENODTYPSTR(MENOD),HYDESC(MHYCOM),CONECOMDESC(MCONECOM),
     &POWCOMDESC(MPOWCOM)
     
C Node and component names
      COMMON/ECNAMES/ENODNAM(MENOD),HYCOMNAM(MHYCOM),
     &HYCOMTYPSTR(MHYCOM),POWCOMNAM(MPOWCOM),POWCDATS(MPOWCOM,MPCDS),
     &CONECOMNAM(MCONECOM)

C Hybrid components that calculate nodal voltages
      COMMON/calcV_LOC/iplant_calcV_LOC(MENOD),iPV_calcV_LOC(MENOD)

C Common holds the id numbers of the nodes connected to the power conditioning
C unit (a power-only component model)
      common/pcu/pcunode(mpowcom,3)    
      integer pcunode        !- (see note in ENETREWR)
      integer PCU_id         !- id number of the PCU power-only component model
      parameter( PCU_id = 20 )


C Plant component node connections
      common/pltnds/iplt_conn_nodes(MHYCOM),iplt_dcnode_id(MHYCOM),
     &              iplt_acnode_id(MHYCOM)
      integer  iplt_conn_nodes       !- (see note in ENETREWR)
      integer  iplt_dcnode_id        !- node index of the DC node the plant component connects to
      integer  iplt_acnode_id        !- node index of the AC node the plant component connects to


C Type casting (all varaiables and arrays cast explicitly)
      INTEGER NENOD,ENODNO,ENODPHTYP,ENODPH,ENODTYP,NHYBCOM,HYBCOMNO,
     &HYCOMTYP,HYCOMPHTYP,HYCONEN,HYLOC,NHYBDAT,NPOWCOM,POWCOMNO,
     &POWCOMID,POWCOMPHTYP,POWCONEN,NPOWCDAT,NPOWCDATS,NCONECOM,
     &CONPH,CONECOMNO,CONECOMID,CONECOMPHTYP,NCONECOMDAT,NECON,
     &ECONO,CONPHTYP,CCNO,SENOD,EENOD,iplant_calcV_LOC,iPV_calcV_LOC

      INTEGER IENTXIST,ENTYPE,IUOUT,IUIN,IFIL,IEUNIT,I,J,K,
     &INODE,IHYB,IPWC,ICNC,ICON,IPHN,IDLC,INC
     
      REAL PHASEANG,ENODBASEV,ENODBASEP,HYBDAT,POWCDAT,CONECOMDAT

      real fENFVersionNum            !-version number of electrical network file

C Characters
      CHARACTER*12 ENTYPESTR,ENODTYPSTR,ENODNAM,HYCOMTYPSTR,HYCOMNAM,
     &POWCOMNAM,CONECOMNAM,PHTYPSTR
      CHARACTER*72 ENDESC,HYDESC,POWCOMDESC,POWCDATS,CONECOMDESC,
     &ENTFLNAM,LFIL,DEFLNAM,H
      CHARACTER OUTS*124,longtfile*144
      LOGICAL CONCAT


C Set the variable for the number of data items printed per line in
C reporting (=INC+1) 
      INC=7

C Set the version number for the enf file (implemented in April 2005)
      fENFVersionNum = 1.00


C Ask the user for the file name and open the temporary file with
C unit ifil=iunit+1
      DEFLNAM='electrical_network.enf'
      IF(ENTFLNAM.NE.' ') THEN 
        LFIL=ENTFLNAM(1:LNBLNK(ENTFLNAM))
      ELSE
        LFIL=DEFLNAM(1:LNBLNK(DEFLNAM))
      ENDIF
      longtfile=' '

C Add the networks path to the filename if required.
C Why is there an addpath call at this point? >>
      CALL ADDPATH(LFIL,longtfile,CONCAT)
      IF(CONCAT) LFIL=longtfile(1:72)

C Ask the user for the file name and open the temporary file with
      H(1)='This file will contain all the information relating to the'
      H(2)='electrical network:                                       '
      H(3)='  general electrical network details;                     '
      H(4)='  electrical nodes;                                       '
      H(5)='  connected hybrid components (i.e. PV, plant, etc.);     '
      H(6)='  connected power only components (i.e. wind turbines);   '
      H(7)='  connecting components (i.e. lines, cables);             '
      H(8)='  network topology.                                       '

      CALL EASKS(LFIL,' ',' Electrical network filename ?',72,
     &  DEFLNAM,' elec file',IER,8)
      IF(LFIL(1:2).NE.'  ') ENTFLNAM=LFIL

C Open the file 
      IEUNIT=IFIL+1
      CALL EFOPSEQ(IEUNIT,ENTFLNAM,4,IER)
      IF(IER.NE.0) THEN
        WRITE(OUTS,'(A,A,A)') ' Problem reading ',
     &  ENTFLNAM(1:LNBLNK(ENTFLNAM)),' aborting.'
        CALL EDISP(IUOUT,OUTS)
        RETURN
      ENDIF

C Write out the version number
      write(ieunit,'(A)') '* FileVersion'
      write(ieunit,'(F10.2)') fENFVersionNum


C Write out the file header and general electrical network info.
      WRITE(IEUNIT,'(A)')'* electrical network'
      WRITE(IEUNIT,'(A)')
     &'# ESP-r Electrical network file created with configuration file:'
      WRITE(IEUNIT,'(2X,A)') LCFGF(1:LNBLNK(LCFGF))

C Write out the description
      WRITE(IEUNIT,'(A)')'# Network description:'
      WRITE(IEUNIT,'(2X,A)') ENDESC(1:LNBLNK(ENDESC))

C Write out the network type
      IF(ENTYPE.EQ.1)THEN
        WRITE(OUTS,'(A)') '  d.c.'
      ELSEIF(ENTYPE.EQ.2)THEN
        WRITE(OUTS,'(A)')'  1-phase'
      ELSEIF(ENTYPE.EQ.3)THEN
        WRITE(OUTS,'(A)')'  2-phase'
      ELSEIF(ENTYPE.EQ.4)THEN
        WRITE(OUTS,'(A)')'  3-phase'
      ELSEIF(ENTYPE.EQ.5)THEN
        WRITE(OUTS,'(A)')'  balanced'
      ELSEIF(ENTYPE.EQ.6)THEN
        WRITE(OUTS,'(A)')'  mixed'
      ELSE
        WRITE(OUTS,'(A)') 'Error: network type not recognised.'
        CALL EDISP(IUOUT,OUTS)
        RETURN 
      ENDIF 
      WRITE(IEUNIT,'(A)') '* net type '
      WRITE(IEUNIT,'(A)') OUTS(1:LNBLNK(OUTS))

C Write out the base power value (VA) for the network 
      WRITE(IEUNIT,'(A)') '* base power value (all nodes)'
      WRITE(IEUNIT,'(2X,F10.2)') ENODBASEP

C Write out the phase angle for each phase of for the network 
      WRITE(IEUNIT,'(A)') '* phase angles (each phase)'
      WRITE(IEUNIT,'(10(2X,F8.2))') (PHASEANG(IPH),IPH=1,MPHAS)

C Write out the node details
      WRITE(IEUNIT,'(A)') '# List and details of nodes in the network'
      WRITE(IEUNIT,'(A)') '* nodes'
      WRITE(IEUNIT,'(2X,I3)') NENOD
      WRITE(OUTS,'(A)')
     &'# No.    Node name   Phase type   Phase Node type    Base volt'
      WRITE(IEUNIT,'(A)') OUTS(1:LNBLNK(OUTS))
      DO 10  INODE=1,NENOD

C-------Plant or PV component that calculates nodal voltage.

C Debug.
C        write(6,*) 'Writing file: :',ENODTYP(INODE),
C     &             iplant_calcV_LOC(INODE)

        IF( ENODTYP(INODE).eq.3 ) THEN
          iNUM = iPV_calcV_LOC(INODE)
        ELSEIF( ENODTYP(INODE).eq.4 ) THEN
          iNUM = iplant_calcV_LOC(INODE)
        ELSE
          iNUM = 0
        ENDIF
        WRITE(OUTS,1111) ENODNO(INODE),ENODNAM(INODE),
     &  PHTYPSTR(ENODPHTYP(INODE)),ENODPH(INODE),ENODTYPSTR(INODE),
     &  ENODBASEV(INODE),iNUM
 1111   FORMAT(2X,I3,2X,A12,2X,A12,2X,I3,2X,A12,2X,F10.2,2x,I3)
        WRITE(IEUNIT,'(A)') OUTS(1:LNBLNK(OUTS))

   10 CONTINUE

C Write the node end marker
      WRITE(IEUNIT,'(A)') '** end nodes'

C Write out the hybrid component data
C Write the field marker
      WRITE(IEUNIT,'(A)')
     &'# List and details of load and generator connections to nodes'
      WRITE(IEUNIT,'(A)')'* hybrid components'
      WRITE(IEUNIT,'(2X,I3)') NHYBCOM

C Write out individual component details
      DO 20 IHYB=1,NHYBCOM
        OUTS=' '
        WRITE(OUTS(1:60),'(A)')
     &'# No. comp. type   comp. name      phase type  connects node'
        WRITE(OUTS(61:83),'(A)')'(s)  location   '
        WRITE(IEUNIT,'(A)') OUTS(1:LNBLNK(OUTS))

C Determine the correct string for the hybrid component type.
        IF(HYCOMTYP(IHYB).EQ.1) THEN
          HYCOMTYPSTR(IHYB)='zone'
        ELSEIF(HYCOMTYP(IHYB).EQ.2) THEN 
          HYCOMTYPSTR(IHYB)='spmaterial'
        ELSEIF(HYCOMTYP(IHYB).EQ.3) THEN 
          HYCOMTYPSTR(IHYB)='plant'
        ELSEIF(HYCOMTYP(IHYB).EQ.4) THEN
          HYCOMTYPSTR(IHYB)='flow'
        ELSE
          CALL EDISP(IUOUT,' ') 
          WRITE(OUTS,'(A)') ' Unknown hybrid component defined.'
          CALL EDISP(IUOUT,OUTS) 
          OUTS=' '
          RETURN
        ENDIF

C Write out the hybrid component information
        WRITE(IEUNIT,1112) HYBCOMNO(IHYB),HYCOMTYPSTR(IHYB),
     &  HYCOMNAM(IHYB),PHTYPSTR(HYCOMPHTYP(IHYB)),
     &  (HYCONEN(IHYB,I),I=1,3),(HYLOC(IHYB,J),J=1,3)
 1112   FORMAT(2X,I3,2X,A12,2X,A12,2X,A12,3(2X,I3),1X,3(2X,I3))


C Write out the node connections for hybrid plant components 
        if ( hycomtyp(ihyb).eq. 3 ) then
          write(outs,'(A)')
     &      '# plt comp node connections   DC node id   AC node id'
          write(ieunit,'(A)') outs(1:lnblnk(outs))
          write(ieunit,1122) iplt_conn_nodes(ihyb),
     &      iplt_dcnode_id(ihyb), iplt_acnode_id(ihyb)       !- these two variables only hold correct                             
        endif                                                !- values if component is connected to 2 nodes
1122    format(2X,I3,2X,I3,2X,I3)                        

        WRITE(IEUNIT,'(A)')'# description:'
        WRITE(IEUNIT,'(2X,A)') HYDESC(IHYB)(1:LNBLNK(HYDESC(IHYB)))
        WRITE(IEUNIT,'(A)')'# No. of additional data items:'
        WRITE(IEUNIT,'(2X,I3)') NHYBDAT(IHYB)
        IF(NHYBDAT(IHYB).GT.0) THEN
          IF(NHYBDAT(IHYB).LE.INC+1) THEN
            WRITE(OUTS,1114)
     &      (HYBDAT(IHYB,I),I=1,NHYBDAT(IHYB))
            WRITE(IEUNIT,'(A)') OUTS(1:LNBLNK(OUTS))
          ELSE
            IDLC=1
38          WRITE(OUTS,1114)
     &      (HYBDAT(IHYB,I),I=IDLC,IDLC+INC)
1114        FORMAT(8(2X,G12.5))
            WRITE(IEUNIT,'(A)') OUTS(1:LNBLNK(OUTS))
            IDLC=IDLC+INC+1
            IF((IDLC+INC).GT.NHYBDAT(IHYB)) 
     &      INC=NHYBDAT(IHYB)-IDLC
            IF(IDLC.LE.NHYBDAT(IHYB).AND.IDLC.LT.MHYDAT) GOTO 38
            INC=7
          ENDIF

        ENDIF

  20  CONTINUE

C Write the hybrid component end field marker
      WRITE(IEUNIT,'(A)') '** end hybrid'

C Write out the power only component data
      WRITE(IEUNIT,'(A)') 
     &'# List and details of power only components'
      WRITE(IEUNIT,'(A)') '* power-only components'
      WRITE(IEUNIT,'(2X,I3)') NPOWCOM

      DO 30 IPWC=1,NPOWCOM
        OUTS=' '
        WRITE(IEUNIT,'(A)') 
     &'# No.   i.d.  Comp. name   Phase type  links to nodes '
        WRITE(OUTS,1115)
     &  POWCOMNO(IPWC),POWCOMID(IPWC),POWCOMNAM(IPWC),
     &  PHTYPSTR(POWCOMPHTYP(IPWC)),(POWCONEN(IPWC,IPHN),IPHN=1,MPHAS)
 1115   FORMAT(2X,I3,2X,I3,2X,A12,2X,A12,3(2X,I3))
        WRITE(IEUNIT,'(A)') OUTS(1:LNBLNK(OUTS))

        WRITE(IEUNIT,'(A)')'# description:'
        WRITE(IEUNIT,'(2X,A)') 
     &POWCOMDESC(IPWC)(1:LNBLNK(POWCOMDESC(IPWC)))

        WRITE(IEUNIT,'(A)')'# no. of additional data items:'
        WRITE(IEUNIT,'(2X,2(I3,2X))') NPOWCDAT(IPWC),NPOWCDATS(IPWC)
        IF(NPOWCDAT(IPWC).GT.0) THEN
          OUTS=' '
          IF(NPOWCDAT(IPWC).LE.INC+1) THEN
            WRITE(OUTS,1116)
     &      (POWCDAT(IPWC,I),I=1,NPOWCDAT(IPWC))
            WRITE(IEUNIT,'(A)') OUTS(1:LNBLNK(OUTS))
          ELSE
            IDLC=1    
  48        WRITE(OUTS,1116)
     &      (POWCDAT(IPWC,I),I=IDLC,IDLC+INC)
1116        FORMAT(8(2X,G12.5))
            WRITE(IEUNIT,'(A)') OUTS(1:LNBLNK(OUTS))
            IDLC=IDLC+INC+1
            IF((IDLC+INC).GT.NPOWCDAT(IPWC)) 
     &        INC=NPOWCDAT(IPWC)-IDLC
            IF(IDLC.LE.NPOWCDAT(IPWC).AND.IDLC.LT.MPWDAT) GOTO 48
            INC=7
          ENDIF

        ENDIF
        IF(NPOWCDATS(IPWC).GT.0) THEN
          DO 49 I=1, NPOWCDATS(IPWC)
            OUTS=' '
            WRITE(OUTS,'(2X,A)') POWCDATS(IPWC,I) 
            WRITE(IEUNIT,'(A)') OUTS(1:LNBLNK(OUTS))
  49      CONTINUE 
        ENDIF

C If power-only component is a PCU, write out id of node sending current
C to PCU and id of node receiving current from PCU.
        if ( POWCOMID(IPWC) .eq. PCU_id ) then
          outs=' '
          write(outs,1121) pcunode(IPWC,1),pcunode(IPWC,2)
1121      format(2X,I3,2X,I3)
          write(ieunit,'(A)') outs(1:lnblnk(outs))
        endif 
  30  CONTINUE
      WRITE(IEUNIT,'(A)') '** end power-only '


C Write out the connecting component data
      WRITE(IEUNIT,'(A)')
     &'# Listing and Description of components connecting the nodes'
      WRITE(IEUNIT,'(A)') '* connecting components'
      WRITE(IEUNIT,'(2X,I3)') NCONECOM
      DO 40 ICNC=1,NCONECOM
        OUTS=' '
        WRITE(OUTS,'(A)')
     &'# No.  i.d. Comp. name    Phase type  '
        WRITE(IEUNIT,'(A)') OUTS(1:LNBLNK(OUTS))
        OUTS=' '
        WRITE(OUTS,1117)CONECOMNO(ICNC),CONECOMID(ICNC),
     &CONECOMNAM(ICNC),PHTYPSTR(CONECOMPHTYP(ICNC))
 1117   FORMAT(2X,I3,2X,I3,2X,A12,2X,A12)
        WRITE(IEUNIT,'(A)') OUTS(1:LNBLNK(OUTS))

        WRITE(IEUNIT,'(A)')'# description: '
        WRITE(IEUNIT,'(2X,A)')
     &CONECOMDESC(ICNC)(1:LNBLNK(CONECOMDESC(ICNC)))

        WRITE(IEUNIT,'(A)')'# No. of additional data items:'
        WRITE(IEUNIT,'(2X,I3)') NCONECOMDAT(ICNC)
        IF(NCONECOMDAT(ICNC).GT.0) THEN
          OUTS=' '
          IF(NCONECOMDAT(ICNC).LE.INC+1) THEN
            WRITE(OUTS,1118)
     &      (CONECOMDAT(ICNC,I),I=1,NCONECOMDAT(ICNC))
            WRITE(IEUNIT,'(A)') OUTS(1:LNBLNK(OUTS))
          ELSE
            IDLC=1
 56         WRITE(OUTS,1118)
     &      (CONECOMDAT(ICNC,I),I=IDLC,IDLC+INC)
1118        FORMAT(8(2X,G12.5))
            WRITE(IEUNIT,'(A)') OUTS(1:LNBLNK(OUTS))
            IDLC=IDLC+INC+1
            IF((IDLC+INC).GT.NCONECOMDAT(ICNC)) 
     &        INC=NCONECOMDAT(ICNC)-IDLC
            IF(IDLC.LE.NCONECOMDAT(ICNC).AND.IDLC.LT.MCCDAT)GOTO 56
            INC=7
          ENDIF
        ENDIF

   40 CONTINUE

      WRITE(IEUNIT,'(A)') '** end connecting components'

C Write out the connections information
      WRITE(IEUNIT,'(A)')
     &'# connections between nodes in the network '
      WRITE(IEUNIT,'(A)') '* connections'
      WRITE(IEUNIT,'(2X,I3)') NECON

      IF(NECON.GT.0) THEN
        OUTS=' '
        WRITE(OUTS(1:37),'(A)')
     &'# No. Conn. type  Phase.   Conn. com.'
        WRITE(OUTS(38:68),'(A)')  
     &'    Start nodes       End nodes'
        WRITE(IEUNIT,'(A)') OUTS(1:LNBLNK(OUTS))
        DO 50 ICON=1,NECON
          OUTS=' '
          WRITE(OUTS,1120) ECONO(ICON),PHTYPSTR(CONPHTYP(ICON)),
     &    (CONPH(ICON,K),K=1,MPHAS),CCNO(ICON),
     &    (SENOD(ICON,I),I=1,MPHAS),(EENOD(ICON,J),J=1,MPHAS)
 1120     FORMAT(2X,I3,2X,A12,1X,3(I1,1X),2X,I3,5X,3(2X,I3),3X,
     &3(2X,I3))
          WRITE(IEUNIT,'(A)') OUTS(1:LNBLNK(OUTS))
  50    CONTINUE
      ENDIF
      WRITE(IEUNIT,'(A)') '** end connections'

C Write out the end of the file marker.
      WRITE(IEUNIT,'(A)') '** electrical network end'


C Inform the user of  a sucessful write
      OUTS=' '
      WRITE(OUTS,'(A,A)') 'Written electrical network file ',ENTFLNAM
      CALL EDISP(IUOUT,OUTS)

Close the file and return
      CLOSE(IEUNIT)
      
      RETURN
      END
