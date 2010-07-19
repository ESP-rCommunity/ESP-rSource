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

C enetprj.F is the main power network definition file containing 
C subroutines relating to electrical network definition: 
C  ELECNET   - main controlling routine for network definition.
C  ELNODLST  - lists the current electrical nodes.
C  ELHYBLST  - lists the current hybrid components.
C  ELCNCLST  - lists the current connecting components.
C  ELCONLST  - lists the current electrical connections.
C  EDENOD    - editing facility for electical nodes.
C  EDEHYB    - editing facility for hybrid components.
C  EDECNC    - editing facility for electrical conductors.
C  EDEPOW    - editing facility for power components.
C  EDECON    - editing facility for electrical connections.
C  ENETCHECK - this routine checks that the network is properly.
C              connected.
C  ENETBASEV - assigns the base voltage value in the network.
 
C *******************************************************************
C A description of the main electrical network variables follows:

C ENTYPE       - the network type 1-d.c. 2-1-phase a.c. 3-3-phase a.c.
C                4-balanced a.c. 5-mixed phase/a.c./d.c.
C ENTYPESTR    - string holding network type description (see above)
C ENDESC       - string holding a description of the network
C PHTYPSTR     - string holding phase types 1-d.c. 2-1-phase 3-3-phase
C                4-balanced
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
C HYLOC        - location of hybrid component
C                zone                 n  load m  day type o
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
C CONECOMTYP   - string containing connecting component name
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
C ************************** ELECNET *********************************
C ELECNET is the main controlling routine for the electrical network 
C definition.  

      SUBROUTINE ELECNET
#include "power.h"
      
      integer lnblnk  ! function definition

      COMMON/OUTIN/IUOUT,IUIN
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
     

C Type casting (all varaiables and arrays cast explicitly)
      INTEGER NENOD,ENODNO,ENODPHTYP,ENODPH,ENODTYP,NHYBCOM,HYBCOMNO,
     &HYCOMTYP,HYCOMPHTYP,HYCONEN,HYLOC,NHYBDAT,NPOWCOM,POWCOMNO,
     &POWCOMID,POWCOMPHTYP,POWCONEN,NPOWCDAT,NPOWCDATS,NCONECOM,
     &CONPH,CONECOMNO,CONECOMID,CONECOMPHTYP,NCONECOMDAT,NECON,
     &ECONO,CONPHTYP,CCNO,SENOD,EENOD

      INTEGER IENTXIST,ENTYPE,IUOUT,IUIN,I
     
      REAL PHASEANG,ENODBASEV,ENODBASEP,HYBDAT,POWCDAT,
     &CONECOMDAT
     
      DIMENSION ITEM(35)
C Characters
      CHARACTER*12 ENTYPESTR,ENODTYPSTR,PHTYPSTR
      CHARACTER*72 ENDESC,HYDESC,POWCOMDESC,CONECOMDESC,
     &ENTFLNAM,H,STR
      CHARACTER*40 ITEM
      CHARACTER*124 OUTS

      LOGICAL OK,DOK,BASEV

C Give a message to user explaining they should have fully defined the 
C building side and the plant network prior to defining the electrical
C network.
      H(1)='You have picked electrical network definition. This '
      H(2)='network is best created after both the plant and '   
      H(3)='building side have been fully defined as power loads'
      H(4)='in each zone and plant components can be connected to '
      H(5)='the electrical network.'
      H(6)=' '
      H(7)='If you have not defined the building and plant yet then'
      H(8)='you are advised to exit this section.               '
      dok=.false.
      CALL ASKOK(' ','Proceed with electrical network definition?',
     &              OK,dok,8)
      IF(.NOT.OK) RETURN
      IENTXIST=0
      BASEV=.FALSE.

C Firstly initialise all the variables 
      CALL ENETINIT

C Read in any existing electrical file information. 
      IF(ENTFLNAM(1:3).NE.'   '.AND.ENTFLNAM(1:3).NE.'UNK') THEN
        CALL EDISP(IUOUT,' ')
        WRITE(OUTS,'(A,A)') 'Current network: ',
     &ENTFLNAM(1:LNBLNK(ENTFLNAM))
        dok=.true.
        h(1)='The synopsis is one way to review the current network.'
        CALL ASKOK(OUTS,
     &'Display a synopsis of the electrical network?',OK,dok,1)
        IF(OK) THEN
          CALL ENETREAD('R') 
        ELSE
          CALL ENETREAD('N')
        ENDIF
        IENTXIST=1
        BASEV=.TRUE.
      ENDIF
        
      CALL USRMSG(' ',' ','-')      
C Display a menu of options
  5   INO=-1
      WRITE(ITEM(1),'(A,A)') 'a  Network name: ',ENTFLNAM(1:22)
      WRITE(ITEM(2),'(A,A)') 'b  Network desciption: '
      WRITE(ITEM(3),'(2X,A)') ENDESC(1:38)
      ITEM(4)=' ---------------------------------------'
      IF(ENTYPE.EQ.0) THEN
        WRITE(ITEM(5),'(A)') 'c  Network type: UNKNOWN'
      ELSE
        WRITE(ITEM(5),'(A,A)') 'c  Network type: ',
     &  ENTYPESTR(ENTYPE)(1:12)
      ENDIF
      ITEM(6)=' ---------------------------------------'
      ITEM(7)='  Electrical network status            '
      ITEM(8)=' ---------------------------------------'
      WRITE(ITEM(9),'(A,3(1X,F4.0))')
     &'  Nom. phase angles :',(PHASEANG(I),I=1,MPHAS)
      ITEM(10)='i  Set nominal phase angles    '
      IF(BASEV) THEN
        ITEM(11)='j  Base voltages >>        Calculated'
      ELSE
        ITEM(11)='k  Base voltages>>         UNKNOWN'
      ENDIF
      WRITE(ITEM(12),'(A,F10.1)') '  Power base value:      ',ENODBASEP
      ITEM(13)='l  Set network power base               ' 
      ITEM(14)=' ---------------------------------------'    
      WRITE(ITEM(15),'(A,I3,A)')'  No. of nodes ...               (',
     &NENOD,')'
      ITEM(16)='d  Nodes                      '
      WRITE(ITEM(17),'(A,I3,A)')'  No. of connected HVAC/PV/etc ..(',
     &NHYBCOM,')'
      ITEM(18)='e  Connect HVAC/PV/Lights/etc.'
      WRITE(ITEM(19),'(A,I3,A)')
     &'  No. of power only components ..(',NPOWCOM,')'
      ITEM(20)='f  Power only components        '
      WRITE(ITEM(21),'(A,I3,A)')
     &'  No. of connecting components ..(',NCONECOM,')'
      ITEM(22)='g  Connecting components      '
      WRITE(ITEM(23),'(A,I3,A)')'  No. of connections ...         (',
     &NECON,')'
      ITEM(24)='h  Network connections         '
      ITEM(25)=' ---------------------------------------'
      ITEM(26)='>  Update network               '
      ITEM(27)='@  Check network for errors     '
      ITEM(28)='?  Help                         '
      ITEM(29)='-  End network definition       '
      NITEM=29     
      
C Help text for this menu.
      H(1)=' '
      H(2)='This part of the Project Manager allows you to define an'
      H(3)='electrical network, which can be simulated in tandem  '
      H(4)='with your building/plant/flow model. The electrical   '
      H(5)='network consists of several elements:  '
      H(6)=' '
      H(7)='NODES - these are the calculation points in the network '
      H(8)='and should be placed where conductors join, or where '
      H(9)='loads or power sources connect to the network.'
      H(10)='In multi-phase systems, multi-phase nodes are grouped'
      H(11)='in phase-sets, one node of the set represents a phase.'
      H(12)=' '
      H(13)='CONNECTING COMPONENTS - these convey electrical current' 
      H(14)='between the nodes in the network. They are impedance  '
      H(15)='models of the distribution components in an electrical '
      H(16)='system: lines, cables transformers. In addition to con-'
      H(17)='veying power, the transformer component also changes the'
      H(18)='voltage between two nodes.'
      H(19)=' '
      H(20)='HVAC/PV/LIGHTS/EQUIPMENT and POWER-ONLY COMPONENTS - '
      H(21)='these link into the network and either draw power '
      H(22)='from or inject power into the network. HVAC components  '
      H(23)='etc. are components which exist in other areas of your  '
      H(24)='model, i.e. lighting loads a PV array or a fan. '
      H(25)='Power-only components exist only in the electrical  '
      H(26)='world e.g. a wind turbine or a diesel generator set.'
      H(27)=' '
      H(28)='CONNECTIONS - these link the entire network together.'
      H(29)='A connection links two or more nodes using a connecting'
      H(30)='component, connections can be multi-phase, single-phase'
      H(31)='d.c. or balanced. Single phase components can be used'
      H(32)='in the connection of multi-phase sets of nodes but not'
      H(33)='vice versa. Phase-set nodes can be linked to single ' 
      H(34)='nodes, but only if the phase numbers of the connected '
      H(35)='nodes match. An a.c. to d.c. connection is possible,'
      H(36)='but only with AC/DC conversion devices.' 
      H(37)=' '    
      H(38)='The complete NETWORK consists of all the above elements'
      H(39)='connected together. Other information required includes'
      H(40)='the type of network: a.c., d.c., mixed or balanced plus' 
      H(41)='BASE VALUES of voltage and power. These are needed as' 
      H(42)='network solution is done in PER UNIT notation. Voltage '
      H(43)='base values for each node are generated automatically.'  

      CALL EMENU('Electrical network definition',ITEM,NITEM,INO)
      IF(INO.EQ.1) THEN

C Read in a network (firstly clearing the current problem)
        CALL ENETINIT
        CALL ENETREAD('R')
        BASEV=.TRUE. 
        CALL USRMSG(' ',' ','-')          
        GOTO 5
      ELSEIF(INO.EQ.2) THEN

C Ask for a new network description
        H(1)='If required you can enter a short description of your'
        H(2)='electrical enetwork here.                            '
        CALL EASKS(ENDESC,'The electrical network description:',' ',
     &    72,'none','net desc',IER,2)
        GOTO 5
      ELSEIF(INO.EQ.5) THEN

C Assign new network type
        H(1)='Select the network type which describes your network: '
        H(2)='d.c      - direct current only'
        H(3)='1,2,3    - phase a.c. only '
        H(4)='Balanced - a balanced 3-phase system '
        H(5)='Mixed    - Mix of 1,2,3-phase elements and possibly d.c.'
        H(6)='                         <the default> '
        CALL EASKATOG('Select network type:',' ',
     &'d.c.','1-phase','2-phase','3-phase','Balanced','Mixed',
     &'Continue',INP,6)
        IF(INP.EQ.1)THEN
          ENTYPE=1
        ELSEIF(INP.EQ.2)THEN
          ENTYPE=2
        ELSEIF(INP.EQ.3.OR.INP.EQ.4)THEN
          ENTYPE=3
        ELSEIF(INP.EQ.5)THEN
          ENTYPE=4
        ELSEIF(INP.EQ.6)THEN
          ENTYPE=5
        ELSEIF(INP.EQ.7)THEN
          GOTO 5
        ENDIF
        GOTO 5

      ELSEIF(INO.EQ.10) THEN
C  Set phase angles 
        IF(ENTYPE.EQ.1.OR.ENTYPE.EQ.2.OR.ENTYPE.EQ.4) THEN
          CALL EDISP(IUOUT,'Phase information is not needed for')
          CALL EDISP(IUOUT,'this type of system.')
          GOTO 5
        ENDIF
        H(1)='The phase angles for the system are the phase in the'
        H(2)='time domain in which each phase in the system operates'
        H(3)='where the difference in each phase angle is given by '
        H(4)='360/No. of phases. So for a three phase system the    '
        H(5)='phase angles should separated by 120 degrees.          '
        WRITE(STR,'(3(F4.0,3X))')(PHASEANG(I),I=1,MPHAS)
        CALL EASKS(STR,'Enter the phase angle for each network phase.',
     &'A -1. implies a particular phase is redundant.',72,
     &'0.   120.   240.',
     &'netwk phase angs',IER,5)
        K=0
        DO 10,I=1,MPHAS
        CALL EGETWR(STR,K,PHASEANG(I),0.,360.,'-','phase vals',
     &IER)
 10     CONTINUE
        GOTO 5   
      ELSEIF(INO.EQ.11) THEN

C Set the base voltages for all the nodes in the network. 
        CALL ENETBASEV 
        GOTO 5  
      ELSEIF(INO.EQ.13) THEN

C  Set base power value 
        H(1)='The base power value is used in the conversion of all  '
        H(2)='network power values to per-unit, where the per-unit   '
        H(3)='power is defined by: Pp.u.=Actual Power/Base Power     '
        H(4)='Choose a value that reflects the size of the power     '
        H(5)='system being modelled, e.g:                            ' 
        H(6)='1000    - small offices or dwellings                   '
        H(7)='10000   - medium sized buildings, small communities    '
        H(8)='1000000 - large CHP or community energy schemes        '
        H(9)='For multi-phase system enter the multi-phase base value'
        H(10)='e.g. for a three-phase system the three-phase base    '
        H(11)='value should be entered. The system base value will be'
        H(12)='this value divided by three.'
        CALL EASKR(ENODBASEP,'Enter the system base power value (VA)',
     &' ',0.,'F',0.,'-',1000.,'base pwr',IER,8)
        GOTO 5 
      ELSEIF(INO.EQ.16) THEN
        CALL ELNODLST('E',IDUM)
        GOTO 5
      ELSEIF(INO.EQ.18) THEN
        CALL ELHYBLST('E',IDUM)
        GOTO 5
      ELSEIF(INO.EQ.20) THEN
        CALL ELPOWLST('E',IDUM)  
        GOTO 5
      ELSEIF(INO.EQ.22) THEN 
        CALL ELCNCLST('E',IDUM)
        GOTO 5
      ELSEIF(INO.EQ.24) THEN
        CALL ELCONLST('E',IDUM) 
        GOTO 5 

      ELSEIF(INO.EQ.NITEM-3) THEN
        CALL ENETWRITE
        GOTO 5  
      ELSEIF(INO.EQ.NITEM-2) THEN
C Check the network for errors.
        CALL ENETCHECK
        GOTO 5      
      ELSEIF(INO.EQ.NITEM-1) THEN

C Display help menu.
        CALL PHELPD('pwr net help',43,'-',0,0,IER) 
        GOTO 5
      ELSEIF(INO.EQ.NITEM) then
        IF(.NOT.BASEV) THEN
          CALL EDISP(IUOUT,'Warning: base voltages not set!')
          OK=.FALSE.
          dok=.false.
          h(1)='If you have the required support information you'
          h(1)='may proceed with the base voltage calculation.'
          CALL ASKOK(' ','Proceed with base voltage calculation?',
     &      OK,dok,1)
          IF(OK) CALL ENETBASEV
          BASEV=.TRUE.
        ENDIF
        OK=.FALSE.
        dok=.true.
        h(1)='Recent changes are held in memory and may be lost if'
        h(2)='you do not save them to file. '
        CALL ASKOK('Save any changes you have made?',' ',OK,dok,2)
        IF(OK) CALL ENETWRITE
        RETURN
      ELSE
        INO=-4
        GOTO 5

      ENDIF


      RETURN 
      END
   

C ******************************* ELNODLST  ********************************************
C Presents a list of electrical nodes to the user and returns the selected node number.
C The routine operates in two modes:
C Edit mode - 'E', which allows nodes to be copied, added and deleted. 
C List mode - '-', which simply presents a list of nodes, one or more
C of which is selected

      SUBROUTINE ELNODLST(MODE,IPK)

#include "epara.h"
#include "power.h"

C Commons
      COMMON/POPHELP/H(60)

C The electrical nodes
      COMMON/ENOD/NENOD,ENODNO(MENOD),ENODPHTYP(MENOD),
     &ENODPH(MENOD),ENODTYP(MENOD),ENODBASEV(MENOD),
     &ENODBASEP

C Node and component names
      COMMON/ECNAMES/ENODNAM(MENOD),HYCOMNAM(MHYCOM),
     &HYCOMTYPSTR(MHYCOM),POWCOMNAM(MPOWCOM),POWCDATS(MPOWCOM,MPCDS),
     &CONECOMNAM(MCONECOM)

C The descriptive strings
      COMMON/ECDESC/ENTYPESTR(MENTYP),PHTYPSTR(MPHATY),ENDESC,
     &ENODTYPSTR(MENOD),HYDESC(MHYCOM),CONECOMDESC(MCONECOM),
     &POWCOMDESC(MPOWCOM)

C Hybrid components that calculate nodal voltages
      COMMON/calcV_LOC/iplant_calcV_LOC(MENOD),iPV_calcV_LOC(MENOD)

C Type casting (all varaiables and arrays cast explicitly)
      INTEGER NENOD,ENODNO,ENODPHTYP,ENODPH,ENODTYP,NODE
      INTEGER iplant_calcV_LOC,iPV_calcV_LOC

      DIMENSION VERT(35)

      REAL ENODBASEV,ENODBASEP

C Characters
      CHARACTER*1 MODE,KEY
      CHARACTER*54 VERT
      CHARACTER*12 ENTYPESTR,ENODTYPSTR,ENODNAM,HYCOMTYPSTR,HYCOMNAM,
     &POWCOMNAM,CONECOMNAM,PHTYPSTR
      CHARACTER*72 ENDESC,HYDESC,POWCOMDESC,POWCDATS,CONECOMDESC,
     &H


C Initialise special material menu size variables based on window size. 
C IVERT is the menu position, MVERT the current number of menu lines.
      ILEN=NENOD
      IPACT=CREATE
      CALL EKPAGE(IPACT)

   3  MHEAD=1
      MCTL=5
      ILEN=NENOD


C Initial menu entry setup.
      IER=0
      IVERT=-3

C Loop through the items until the page to be displayed. M is the 
C current menu line index. Build up text strings for the menu. 
      M=MHEAD
      DO 10 L=1,NENOD
        IF(L.GE.IST.AND.(L.LE.(IST+MIFULL)))THEN
          M=M+1
          CALL EMKEY(L,KEY,IER)
          IF( ENODTYP(L).eq.3 ) THEN
            iNUM = iPV_calcV_LOC(L)
          ELSEIF( ENODTYP(L).eq.4 ) THEN
            iNUM = iplant_calcV_LOC(L)
          ELSE
            iNUM = 0
          ENDIF
          WRITE(VERT(M),14)KEY,ENODNAM(L)(1:12),
     &PHTYPSTR(ENODPHTYP(L))(1:12),ENODPH(L),ENODTYPSTR(L)(1:12),
     &ENODBASEV(L),iNUM
   14     FORMAT(A1,1X,A12,1X,A12,1X,I2,1X,A12,1X,F5.0,1x,i3)
        ENDIF
   10 CONTINUE

      VERT(1)='  Node name   | Phase Type |No| Node type |Vbase | Con'
C Number of actual items displayed.
      MVERT=M+MCTL

C If a long list include page facility text.      
      IF(IPFLG.EQ.0)THEN  
        VERT(M+1)='  ________________________________  '
      ELSE
        WRITE(VERT(M+1),15)IPM,MPM 
   15   FORMAT   ('0 page: ',I2,' of ',I2,' ---------')
      ENDIF
      IF(MODE.EQ.'E') THEN
        VERT(M+2)  ='+ add/delete/copy item              '
        VERT(M+3)  ='                                    '
      ELSE
        VERT(M+2)  =' '
        VERT(M+3)  =' '
      ENDIF
      VERT(M+4)  ='? help                              '
      VERT(M+5)  ='- exit                              '

c Help text for this menu.
      IF(MODE.EQ.'E')THEN
        H(1)='The list shows the currently available nodes in the '
        H(2)='electrical network. Click in a node to edit it      '
        H(3)='or chose to add/delete/copy a node.                 '
      ELSE
        H(1)='The list shows the currently available nodes in the '
        H(2)='electrical network. Please select one from the list'
        H(3)='displayed.                                          '
      ENDIF

C Now display the menu.
 7    CALL EMENU(' Electrical nodes',VERT,MVERT,IVERT)
      IF(IVERT.LE.MHEAD)THEN

C Within the header so skip request.
        IVERT=-1
        GOTO 3
      ELSEIF(IVERT.EQ.MVERT)THEN
        IPK=-3
        RETURN
      ELSEIF(IVERT.EQ.MVERT-1)THEN 
        CALL PHELPD('Select node',3,'-',0,0,IER)
         GOTO 7
      ELSEIF(IVERT.EQ.MVERT-3)THEN 
        IF(MODE.EQ.'E') THEN

C Add/delete or copy a node. 
C Delete the chosen node.
          CALL EASKABCD('Select option or continue',' ','Add node',
     &'Delete node','Copy node','Continue',IOPT,0)
          IF(IOPT.EQ.1) THEN
            NODE=NENOD+1
            CALL EDENOD('ADD',NODE)
            ILEN=NENOD
            IPACT=CREATE
            CALL EKPAGE(IPACT)
          ELSEIF(IOPT.EQ.2) THEN
            CALL EMENU(' Delete which node:',VERT,MVERT,ID)
            CALL KEYIND(MVERT,ID,IFOC,IO)
            NODE=IFOC
            CALL EDENOD('DEL',NODE)
            ILEN=NENOD
            IPACT=CREATE
            CALL EKPAGE(IPACT)
          ELSEIF(IOPT.EQ.3) THEN
            CALL EMENU(' Copy which node:',VERT,MVERT,ID)
            CALL KEYIND(MVERT,ID,IFOC,IO)
            NODE=IFOC
            CALL EDENOD('CPY',NODE)
            ILEN=NENOD
            IPACT=CREATE
            CALL EKPAGE(IPACT)
          ELSE
            GOTO 3
          ENDIF
C Redisplay the menu
          GOTO 3
        ENDIF

      ELSEIF(IVERT.EQ.(MVERT-4))THEN
C If there are enough items allow paging control via EKPAGE.
        IF(IPFLG.EQ.1)THEN
          IPACT=EDIT
          CALL EKPAGE(IPACT)
        ENDIF
        GOTO 3
      ELSEIF(IVERT.GT.MHEAD.AND.IVERT.LT.(MVERT-MCTL+1)) THEN

C Return the picked node and allow editing
        IF(MODE.EQ.'E') THEN
          CALL KEYIND(MVERT,IVERT,IFOC,IO)
          NODE=IFOC
          CALL EDENOD('EDT',NODE)
          ILEN=NENOD
          IPACT=CREATE
          CALL EKPAGE(IPACT)
          GOTO 3
        ELSE
          CALL KEYIND(MVERT,IVERT,IFOC,IO)
          IPK=IFOC
        ENDIF
      ELSE
        GOTO 7
      ENDIF

      RETURN 
      END
C ******************************* ELHYBLST  ********************************************
C Presents a list of hybrid components to the user and returns the selected 
C component number.
C The routine operates in two modes:
C Edit mode - 'E', which allows components to be copied, added, edited and deleted. 
C List mode - '-', which simply presents a list of components, one or more
C of which is selected

      SUBROUTINE ELHYBLST(MODE,IPK)

#include "epara.h"
#include "power.h"

C Commons
      COMMON/POPHELP/H(60)

C The connected hybrid components
      COMMON/HYBE/NHYBCOM,HYBCOMNO(MHYCOM),HYCOMTYP(MHYCOM),
     &HYCOMPHTYP(MHYCOM),HYCONEN(MHYCOM,MPHAS),HYLOC(MHYCOM,MPHAS),
     &NHYBDAT(MHYCOM),HYBDAT(MHYCOM,MHYDAT)


C Node and component names
      COMMON/ECNAMES/ENODNAM(MENOD),HYCOMNAM(MHYCOM),
     &HYCOMTYPSTR(MHYCOM),POWCOMNAM(MPOWCOM),POWCDATS(MPOWCOM,MPCDS),
     &CONECOMNAM(MCONECOM)

C The descriptive strings
      COMMON/ECDESC/ENTYPESTR(MENTYP),PHTYPSTR(MPHATY),ENDESC,
     &ENODTYPSTR(MENOD),HYDESC(MHYCOM),CONECOMDESC(MCONECOM),
     &POWCOMDESC(MPOWCOM)

C Type casting (all variables and arrays cast explicitly)
      INTEGER NHYBCOM,HYBCOMNO,HYCOMTYP,HYCOMPHTYP,HYCONEN,HYLOC,
     &NHYBDAT

      REAL HYBDAT

      DIMENSION VERT(35)

C Characters
      CHARACTER*1 MODE,KEY
      CHARACTER*66 VERT
      CHARACTER*12 ENTYPESTR,ENODTYPSTR,ENODNAM,HYCOMTYPSTR,HYCOMNAM,
     &POWCOMNAM,CONECOMNAM,PHTYPSTR
      CHARACTER*72 ENDESC,HYDESC,POWCOMDESC,POWCDATS,CONECOMDESC,
     &H


C Initialise special material menu size variables based on window size. 
C IVERT is the menu position, MVERT the current number of menu lines.
      ILEN=NHYBCOM
      IPACT=CREATE
      CALL EKPAGE(IPACT)
   3  MHEAD=1
      MCTL=5
      ILEN=NHYBCOM


C Initial menu entry setup.
      IER=0
      IVERT=-3

C Loop through the items until the page to be displayed. M is the 
C current menu line index. Build up text strings for the menu. 
      M=MHEAD

      DO 10 L=1,NHYBCOM
        IF(L.GE.IST.AND.(L.LE.(IST+MIFULL)))THEN
          M=M+1
          CALL EMKEY(L,KEY,IER)
          WRITE(VERT(M),14)KEY,HYCOMNAM(L)(1:12),
     &HYCOMTYPSTR(L)(1:12),PHTYPSTR(HYCOMPHTYP(L))(1:12),
     &(HYCONEN(L,J),J=1,MPHAS),(HYLOC(L,K),K=1,3)
   14     FORMAT(A1,1X,A12,2X,A12,2X,A12,2X,3(I2,1X),2X,3(I2,1X))
        ENDIF
   10 CONTINUE

      VERT(1)=                    
     &'  Comp name   | Comp type   | Phase Type  |Con. Nods | Location '
C Number of actual items displayed.
      MVERT=M+MCTL

C If a long list include page facility text.      
      IF(IPFLG.EQ.0)THEN  
        VERT(M+1)='  ________________________________  '
      ELSE
        WRITE(VERT(M+1),15)IPM,MPM 
   15   FORMAT   ('0 page: ',I2,' of ',I2,' ---------')
      ENDIF
      IF(MODE.EQ.'E') THEN
        VERT(M+2)  ='+ add/delete/copy item              '
        VERT(M+3)  ='                                    '
      ELSE
        VERT(M+2)  =' '
        VERT(M+3)  =' '
      ENDIF
      VERT(M+4)  ='? help                              '
      VERT(M+5)  ='- exit                              '

C Help text for this menu.
      IF(MODE.EQ.'E')THEN
        H(1)='The list shows the HVAC, PV, lights, etc,. connected to'
        H(2)='the electrical network. Please select one from the    '
        H(3)='or chose to add/delete/copy a component.              '
      ELSE
        H(1)='The list shows the HVAC, PV, lights, etc., connected to'
        H(2)='the electrical network. Please select one from the    '
        H(3)='list displayed.                                       '
      ENDIF                                           

C Now display the menu.
 7    CALL EMENU(' Connected HVAC/PV/lights ...',VERT,MVERT,IVERT)
      IF(IVERT.LE.MHEAD)THEN

C Within the header so skip request.
        IVERT=-1
        GOTO 3
      ELSEIF(IVERT.EQ.MVERT)THEN
        RETURN
      ELSEIF(IVERT.EQ.MVERT-1)THEN 
        CALL PHELPD('Select node',3,'-',0,0,IER)
        GOTO 7
      ELSEIF(IVERT.EQ.MVERT-3)THEN 
        IF(MODE.EQ.'E') THEN
          CALL EASKABCD('Select option or continue',' ',
     &'Add','Delete','Copy','Continue',IWCH,0)
          IF(IWCH.EQ.1) THEN
            IHYB=99
            CALL EDEHYB('ADD',IHYB)
            ILEN=NHYBCOM
            IPACT=CREATE
            CALL EKPAGE(IPACT)
          ELSEIF(IWCH.EQ.2) THEN
C Add/delete or copy a node. 
C Delete the chosen node.
C But first check that the node is not connected to anything.
              CALL EMENU(' Delete which component:',VERT,MVERT,ID)
              CALL KEYIND(MVERT,ID,IFOC,IO)
              IHYB=IFOC
              CALL EDEHYB('DEL',IHYB)
              ILEN=NHYBCOM
              IPACT=CREATE
              CALL EKPAGE(IPACT)
            ELSEIF(IWCH.EQ.3) THEN
              CALL EDEHYB('CPY',IHYB)
              ILEN=NHYBCOM
              IPACT=CREATE
              CALL EKPAGE(IPACT)
            ELSE
              GOTO 3
          ENDIF
C Redisplay the menu
          GOTO 3
        ENDIF

      ELSEIF(IVERT.EQ.(MVERT-4))THEN

C If there are enough items allow paging control via EKPAGE.
        IF(IPFLG.EQ.1)THEN
          IPACT=EDIT
          CALL EKPAGE(IPACT)
        ENDIF
        GOTO 3
      ELSEIF(IVERT.GT.MHEAD.AND.IVERT.LT.(MVERT-MCTL+1)) THEN

C Return or edit the picked component
        IF(MODE.EQ.'E') THEN
          CALL KEYIND(MVERT,IVERT,IFOC,IO)
          IHYB=IFOC
          CALL EDEHYB('EDT',IHYB)    
          ILEN=NHYBCOM
          IPACT=CREATE
          CALL EKPAGE(IPACT)   
        ELSE
          CALL KEYIND(MVERT,IVERT,IFOC,IO)
          IPK=IFOC
        ENDIF
        GOTO 3
      ELSE
        GOTO 7
      ENDIF

      RETURN 
      END
C ******************************* ELPOWLST  ********************************************
C Presents a list of hybrid components to the user and returns the selected 
C component number.
C The routine operates in two modes:
C Edit mode - 'E', which allows components to be copied, added, edited and deleted. 
C List mode - '-', which simply presents a list of components, one or more
C of which is selected

      SUBROUTINE ELPOWLST(MODE,IPK)

#include "epara.h"
#include "power.h"

C Commons
      COMMON/POPHELP/H(60)

C The connected power only components
      COMMON/POWC/NPOWCOM,POWCOMNO(MPOWCOM),POWCOMID(MPOWCOM),
     &POWCOMPHTYP(MPOWCOM),POWCONEN(MPOWCOM,MPHAS),
     &NPOWCDAT(MPOWCOM),NPOWCDATS(MPOWCOM),POWCDAT(MPOWCOM,MPWDAT)


C Node and component names
      COMMON/ECNAMES/ENODNAM(MENOD),HYCOMNAM(MHYCOM),
     &HYCOMTYPSTR(MHYCOM),POWCOMNAM(MPOWCOM),POWCDATS(MPOWCOM,MPCDS),
     &CONECOMNAM(MCONECOM)

C The descriptive strings
      COMMON/ECDESC/ENTYPESTR(MENTYP),PHTYPSTR(MPHATY),ENDESC,
     &ENODTYPSTR(MENOD),HYDESC(MHYCOM),CONECOMDESC(MCONECOM),
     &POWCOMDESC(MPOWCOM)

C Type casting (all varaiables and arrays cast explicitly)
      INTEGER NPOWCOM,POWCOMNO,POWCOMID,POWCOMPHTYP,POWCONEN,NPOWCDAT,
     &NPOWCDATS

      REAL POWCDAT

      DIMENSION VERT(35)

C Characters
      CHARACTER*1 MODE,KEY
      CHARACTER*46 VERT
      CHARACTER*12 ENTYPESTR,ENODTYPSTR,ENODNAM,HYCOMTYPSTR,HYCOMNAM,
     &POWCOMNAM,CONECOMNAM,PHTYPSTR
      CHARACTER*72 ENDESC,HYDESC,POWCOMDESC,POWCDATS,CONECOMDESC,
     &H


C Initialise special material menu size variables based on window size. 
C IVERT is the menu position, MVERT the current number of menu lines.
      ILEN=NPOWCOM
      IPACT=CREATE
      CALL EKPAGE(IPACT)

  3   MHEAD=1
      MCTL=5
      ILEN=NPOWCOM

C Initial menu entry setup.
      IER=0
      IVERT=-3

C Loop through the items until the page to be displayed. M is the 
C current menu line index. Build up text strings for the menu. 
      M=MHEAD
      DO 10 L=1,NPOWCOM
        IF(L.GE.IST.AND.(L.LE.(IST+MIFULL)))THEN
          M=M+1
          CALL EMKEY(L,KEY,IER)
          WRITE(VERT(M),14)KEY,POWCOMNAM(L)(1:12),POWCOMID(L),
     &PHTYPSTR(POWCOMPHTYP(L))(1:12),(POWCONEN(L,J),J=1,MPHAS)
   14     FORMAT(A1,1X,A12,2X,I2,2X,A12,2X,3(I2,1X))
        ENDIF
   10 CONTINUE

      VERT(1)=
     &'  Comp. name   |id| Phase type  | Con. Nods '
      
C Number of actual items displayed.
      MVERT=M+MCTL

C If a long list include page facility text.      
      IF(IPFLG.EQ.0)THEN  
        VERT(M+1)='  ________________________________  '
      ELSE
        WRITE(VERT(M+1),15)IPM,MPM 
   15   FORMAT   ('0 page: ',I2,' of ',I2,' ---------')
      ENDIF
      IF(MODE.EQ.'E') THEN
        VERT(M+2)  ='+ add/delete/copy item              '
        VERT(M+3)  ='                                    '
      ELSE
        VERT(M+2)  =' '
        VERT(M+3)  =' '
      ENDIF
      VERT(M+4)  ='? help                              '
      VERT(M+5)  ='- exit                              '

C Help text for this menu.
   7  IF(MODE.EQ.'EDT')THEN
        H(1)='The list shows the available power only components '
        H(2)='in the electrical network. Click on one to edit it '
        H(3)='or chose to add/delete/copy a component.           '
      ELSE
        H(1)='The list shows the available power only components    '
        H(2)='in the electrical network. Please select one from the '
        H(3)='list displayed.                                       '
      ENDIF                                   

C Now display the menu.
      CALL EMENU(' Power only components:',VERT,MVERT,IVERT)
      IF(IVERT.LE.MHEAD)THEN

C Within the header so skip request.
        IVERT=-1
        GOTO 3
      ELSEIF(IVERT.EQ.MVERT)THEN
        RETURN
      ELSEIF(IVERT.EQ.MVERT-1)THEN
        CALL PHELPD('Select node',3,'-',0,0,IER)
        GOTO 7
      ELSEIF(IVERT.EQ.MVERT-3)THEN 
        IF(MODE.EQ.'E') THEN
          CALL EASKABCD('Select option or continue',' ',
     &'Add','Delete','Copy','Continue',IWCH,0)
          IF(IWCH.EQ.1) THEN
            IPOW=99
            CALL EDEPOW('ADD',IPOW)
            ILEN=NPOWCOM
            IPACT=CREATE
            CALL EKPAGE(IPACT)
          ELSEIF(IWCH.EQ.2) THEN
C Add/delete or copy a node. 
C Delete the chosen node.
C But first check that the node is not connected to anything.
            CALL EMENU('Delete which component:',VERT,MVERT,ID)
            CALL KEYIND(MVERT,ID,IFOC,IO)
            IPOW=IFOC
            CALL EDEPOW('DEL',IPOW)
            ILEN=NPOWCOM
            IPACT=CREATE
            CALL EKPAGE(IPACT)
          ELSEIF(IWCH.EQ.3) THEN
            CALL EMENU('Copy which component:',VERT,MVERT,ID)
            CALL KEYIND(MVERT,ID,IFOC,IO)
            IPOW=IFOC
            CALL EDEPOW('CPY',IPOW)
            ILEN=NPOWCOM
            IPACT=CREATE
            CALL EKPAGE(IPACT)
          ELSE
            GOTO 3
          ENDIF
C Redisplay the menu
          GOTO 3
        ENDIF

      ELSEIF(IVERT.EQ.(MVERT-4))THEN
C If there are enough items allow paging control via EKPAGE.
        IF(IPFLG.EQ.1)THEN
          IPACT=EDIT
          CALL EKPAGE(IPACT)
        ENDIF
        GOTO 3

      ELSEIF(IVERT.GT.MHEAD.AND.IVERT.LT.(MVERT-MCTL+1)) THEN

C Return or edit the picked component
        IF(MODE.EQ.'E') THEN
          CALL KEYIND(MVERT,IVERT,IFOC,IO)
          IPOW=IFOC 
          CALL EDEPOW('EDT',IPOW)   
          ILEN=NPOWCOM
          IPACT=CREATE
          CALL EKPAGE(IPACT)    
        ELSE
          CALL KEYIND(MVERT,IVERT,IFOC,IO)
          IPK=IFOC
        ENDIF
        GOTO 3
      ELSE
        GOTO 7
      ENDIF

      RETURN 
      END

C ******************************* ELCNCLST  ********************************************
C Presents a list of connecting components to the user and returns the selected 
C component number.
C The routine operates in two modes:
C Edit mode - 'E', which allows components to be copied, added edited and deleted. 
C List mode - '-', which simply presents a list of components, one or more
C of which is selected

      SUBROUTINE ELCNCLST(MODE,IPK)

#include "epara.h"
#include "power.h"

C Commons
      COMMON/POPHELP/H(60)

C The connecting components
      COMMON/CONCOM/NCONECOM,CONECOMNO(MCONECOM),CONECOMID(MCONECOM),
     &CONECOMPHTYP(MCONECOM),NCONECOMDAT(MCONECOM),
     &CONECOMDAT(MCONECOM,MCCDAT)

C Node and component names
      COMMON/ECNAMES/ENODNAM(MENOD),HYCOMNAM(MHYCOM),
     &HYCOMTYPSTR(MHYCOM),POWCOMNAM(MPOWCOM),POWCDATS(MPOWCOM,MPCDS),
     &CONECOMNAM(MCONECOM)

C The descriptive strings
      COMMON/ECDESC/ENTYPESTR(MENTYP),PHTYPSTR(MPHATY),ENDESC,
     &ENODTYPSTR(MENOD),HYDESC(MHYCOM),CONECOMDESC(MCONECOM),
     &POWCOMDESC(MPOWCOM)

C Type casting (all varaiables and arrays cast explicitly)
      INTEGER NCONECOM,CONECOMNO,CONECOMID,CONECOMPHTYP,NCONECOMDAT

      REAL CONECOMDAT

      DIMENSION VERT(35)

C Characters
      CHARACTER*1 MODE,KEY
      CHARACTER*34 VERT
      CHARACTER*12 ENTYPESTR,ENODTYPSTR,ENODNAM,HYCOMTYPSTR,HYCOMNAM,
     &POWCOMNAM,CONECOMNAM,PHTYPSTR
      CHARACTER*72 ENDESC,HYDESC,POWCOMDESC,POWCDATS,CONECOMDESC,
     &H


C Initialise special material menu size variables based on window size. 
C IVERT is the menu position, MVERT the current number of menu lines.
      ILEN=NCONECOM
      IPACT=CREATE
      CALL EKPAGE(IPACT)
  3   MHEAD=1
      MCTL=5
      ILEN=NCONECOM

C Initial menu entry setup.
      IER=0
      IVERT=-3

C Loop through the items until the page to be displayed. M is the 
C current menu line index. Build up text strings for the menu. 
      M=MHEAD

      DO 10 L=1,NCONECOM
        IF(L.GE.IST.AND.(L.LE.(IST+MIFULL)))THEN
          M=M+1
          CALL EMKEY(L,KEY,IER)
          WRITE(VERT(M),14)KEY,CONECOMNAM(L)(1:12),
     &CONECOMID(L),PHTYPSTR(CONECOMPHTYP(L))(1:12)
   14     FORMAT(A1,1X,A12,2X,I2,2X,A12)
        ENDIF
   10 CONTINUE

      VERT(1)=
     &'  Comp. name   |id| Phase type  '

C Number of actual items displayed.
      MVERT=M+MCTL

C If a long list include page facility text.      
      IF(IPFLG.EQ.0)THEN  
        VERT(M+1)='  _____________________________  '
      ELSE
        WRITE(VERT(M+1),15)IPM,MPM 
   15   FORMAT   ('0 page: ',I2,' of ',I2,' ------')
      ENDIF
      IF(MODE.EQ.'E') THEN
        VERT(M+2)  ='+ add/delete/copy item           '
        VERT(M+3)  ='                                 '
      ELSE
        VERT(M+2)  =' '
        VERT(M+3)  =' '
      ENDIF
      VERT(M+4)  ='? help                           '
      VERT(M+5)  ='- exit                           '

C Help text for this menu.
   7  IF(MODE.EQ.'E')THEN
        H(1)='The list shows the current connecting components '
        H(2)='in the electrical network. Click on one to edit it    '
        H(3)='or chose to add/delete/copy a component.              '
      ELSE
        H(1)='The list shows the current connecting components '
        H(2)='in the electrical network. Please select one from the '
        H(3)='list displayed.                                       '
      ENDIF                                 

C Now display the menu.
      CALL EMENU(' Connecting components: ',VERT,MVERT,IVERT)
      IF(IVERT.LE.MHEAD)THEN

C Within the header so skip request.
        IVERT=-1
        GOTO 3
      ELSEIF(IVERT.EQ.MVERT)THEN
        RETURN
      ELSEIF(IVERT.EQ.MVERT-1)THEN 
        CALL PHELPD('Select node',3,'-',0,0,IER)
        GOTO 7
      ELSEIF(IVERT.EQ.MVERT-3)THEN 
        IF(MODE.EQ.'E') THEN
          CALL EASKABCD('Select option or continue',' ',
     &      'Add','Delete','Copy','Continue',IWCH,0)
          IF(IWCH.EQ.1) THEN
            ICNC=99
            CALL EDECNC('ADD',ICNC)
            ILEN=NCONECOM
            IPACT=CREATE
            CALL EKPAGE(IPACT)
          ELSEIF(IWCH.EQ.2) THEN

C Add/delete or copy a node. 
C Delete the chosen node.
C But first check that the node is not connected to anything.
            CALL EMENU(' Delete which component:',VERT,MVERT,ID)
            CALL KEYIND(MVERT,ID,IFOC,IO)
            ICNC=IFOC
            CALL EDECNC('DEL',ICNC)
            ILEN=NCONECOM
            IPACT=CREATE
            CALL EKPAGE(IPACT)
          ELSEIF(IWCH.EQ.3) THEN
            CALL EMENU(' Copy which component:',VERT,MVERT,ID)
            CALL KEYIND(MVERT,ID,IFOC,IO)
            ICNC=IFOC
            CALL EDECNC('CPY',ICNC)
            ILEN=NCONECOM
            IPACT=CREATE
            CALL EKPAGE(IPACT)
          ELSE
            GOTO 3
          ENDIF
C Redisplay the menu
          GOTO 3
        ENDIF

      ELSEIF(IVERT.EQ.(MVERT-4))THEN
C If there are enough items allow paging control via EKPAGE.
        IF(IPFLG.EQ.1)THEN
          IPACT=EDIT
          CALL EKPAGE(IPACT)
        ENDIF
        GOTO 3

      ELSEIF(IVERT.GT.MHEAD.AND.IVERT.LT.(MVERT-MCTL+1)) THEN

C Return or edit the picked component
        IF(MODE.EQ.'E') THEN
          CALL KEYIND(MVERT,IVERT,IFOC,IO)
          ICNC=IFOC 
          CALL EDECNC('EDT',ICNC)      
          ILEN=NCONECOM
          IPACT=CREATE
          CALL EKPAGE(IPACT) 
        ELSE
          CALL KEYIND(MVERT,IVERT,IFOC,IO)
          IPK=IFOC
          RETURN
        ENDIF
        GOTO 3
      ELSE
        GOTO 7
      ENDIF

      RETURN 
      END

C ******************************* ELCONLST  ********************************************
C Presents a list of electrical connections to the user and returns the selected 
C connection number.
C The routine operates in two modes:
C Edit mode - 'E', which allows connections to be copied, added edited and deleted. 
C List mode - '-', which simply presents a list of connections, one or more
C of which is selected

      SUBROUTINE ELCONLST(MODE,IPK)

#include "epara.h"
#include "power.h"

C Commons
      COMMON/POPHELP/H(60)

C The electrical network topology
      COMMON/ECONN/NECON,ECONO(MECON),CONPHTYP(MECON),
     &CONPH(MECON,MPHAS),CCNO(MECON),SENOD(MECON,MPHAS),
     &EENOD(MECON,MPHAS)

C Node and component names
      COMMON/ECNAMES/ENODNAM(MENOD),HYCOMNAM(MHYCOM),
     &HYCOMTYPSTR(MHYCOM),POWCOMNAM(MPOWCOM),POWCDATS(MPOWCOM,MPCDS),
     &CONECOMNAM(MCONECOM)

C The descriptive strings
      COMMON/ECDESC/ENTYPESTR(MENTYP),PHTYPSTR(MPHATY),ENDESC,
     &ENODTYPSTR(MENOD),HYDESC(MHYCOM),CONECOMDESC(MCONECOM),
     &POWCOMDESC(MPOWCOM)

C Type casting (all varaiables and arrays cast explicitly)
      INTEGER NECON,ECONO,CONPHTYP,CONPH,CCNO,SENOD,EENOD

      DIMENSION VERT(35)

C Characters
      CHARACTER*1 MODE,KEY
      CHARACTER*60 VERT
      CHARACTER*12 ENTYPESTR,ENODTYPSTR,ENODNAM,HYCOMTYPSTR,HYCOMNAM,
     &POWCOMNAM,CONECOMNAM,PHTYPSTR
      CHARACTER*72 ENDESC,HYDESC,POWCOMDESC,POWCDATS,CONECOMDESC,
     &H


C Initialise special material menu size variables based on window size. 
C IVERT is the menu position, MVERT the current number of menu lines.
      ILEN=NECON
      IPACT=CREATE
      CALL EKPAGE(IPACT)

   3  MHEAD=1
      MCTL=5
      ILEN=NECON


C Initial menu entry setup.
      IER=0
      IVERT=-3

C Loop through the items until the page to be displayed. M is the 
C current menu line index. Build up text strings for the menu. 
      M=MHEAD

      DO 10 L=1,NECON
        IF(L.GE.IST.AND.(L.LE.(IST+MIFULL)))THEN
          M=M+1
          CALL EMKEY(L,KEY,IER)
          WRITE(VERT(M),14)KEY,CONECOMNAM(CCNO(L))(1:12),
     &PHTYPSTR(CONPHTYP(L))(1:12),
     &(CONPH(L,I),I=1,MPHAS),(SENOD(L,J),J=1,MPHAS),
     &(EENOD(L,K),K=1,MPHAS)

   14     FORMAT(A1,1X,A12,2X,A12,2X,3(1X,I1),2X,3(1X,I2),2X,3(1X,I2))
        ENDIF
   10 CONTINUE

      VERT(1)=
     &'   Conn comp  | Phase type   | Phase | Strt nod | End nod '
C Number of actual items displayed.
      MVERT=M+MCTL

C If a long list include page facility text.      
      IF(IPFLG.EQ.0)THEN  
        VERT(M+1)='  ________________________________  '
      ELSE
        WRITE(VERT(M+1),15)IPM,MPM 
   15   FORMAT   ('0 page: ',I2,' of ',I2,' ---------')
      ENDIF
      IF(MODE.EQ.'E') THEN
        VERT(M+2)  ='+ add/delete/copy item              '
        VERT(M+3)  ='                                    '
      ELSE
        VERT(M+2)  =' '
        VERT(M+3)  =' '
      ENDIF
      VERT(M+4)  ='? help                              '
      VERT(M+5)  ='- exit                              '

C Help text for this menu.
   7   IF(MODE.EQ.'E')THEN
        H(1)='The list shows the currently defined connections      '
        H(2)='in the electrical network. Click on one to edit it    '
        H(3)='or chose to add/delete/copy a component.              '
      ELSE
        H(1)='The list shows the currently defined connections      '
        H(2)='in the electrical network. Please select one from the '
        H(3)='list displayed.                                       '
      ENDIF                                 

C Now display the menu.
      CALL EMENU(' Connections: ',VERT,MVERT,IVERT)
      IF(IVERT.LE.MHEAD)THEN

C Within the header so skip request.
        IVERT=-1
        GOTO 3
      ELSEIF(IVERT.EQ.MVERT)THEN
        RETURN
      ELSEIF(IVERT.EQ.MVERT-1)THEN 
        CALL PHELPD('Select node',3,'-',0,0,IER)
        GOTO 7
      ELSEIF(IVERT.EQ.MVERT-3)THEN 
        IF(MODE.EQ.'E') THEN
          CALL EASKABCD('Select option or continue',' ',
     &      'Add','Delete','Copy','Continue',IWCH,0)
          IF(IWCH.EQ.1) THEN
            ICON=99
            CALL EDECON('ADD',ICON)
            ILEN=NECON
            IPACT=CREATE
            CALL EKPAGE(IPACT)
          ELSEIF(IWCH.EQ.2) THEN
C Add/delete or copy a node. 
C Delete the chosen node.
C But first check that the node is not connected to anything.
            CALL EMENU(' Delete which connection:',VERT,MVERT,ID)
            CALL KEYIND(MVERT,ID,IFOC,IO)
            ICON=IFOC
            CALL EDECON('DEL',ICON)
            ILEN=NECON
            IPACT=CREATE
            CALL EKPAGE(IPACT)             
          ELSEIF(IWCH.EQ.3) THEN
            CALL EMENU(' Copy which connection:',VERT,MVERT,ID)
            CALL KEYIND(MVERT,ID,IFOC,IO)
            ICON=IFOC
            CALL EDECON('CPY',ICON)
            ILEN=NECON
            IPACT=CREATE
            CALL EKPAGE(IPACT)
          ELSE
            GOTO 3
          ENDIF
C Redisplay the menu
          GOTO 3
        ENDIF

      ELSEIF(IVERT.EQ.(MVERT-4))THEN
C If there are enough items allow paging control via EKPAGE.
        IF(IPFLG.EQ.1)THEN
          IPACT=EDIT
          CALL EKPAGE(IPACT)
        ENDIF
        GOTO 3

      ELSEIF(IVERT.GT.MHEAD.AND.IVERT.LT.(MVERT-MCTL+1)) THEN

C Return or edit the picked component
        IF(MODE.EQ.'E') THEN
          CALL KEYIND(MVERT,IVERT,IFOC,IO)
          ICON=IFOC 
          CALL EDECON('EDT',ICON)    
          ILEN=NECON
          IPACT=CREATE
          CALL EKPAGE(IPACT)   
        ELSE
          CALL KEYIND(MVERT,IVERT,IFOC,IO)
          IPK=IFOC
        ENDIF
        GOTO 3
      ELSE
        GOTO 7
      ENDIF

      RETURN 
      END

C ******************************* EDENOD  ********************************************
C This routine edits the current electrical nodes according to various modes:
C 'ADD' - add an extra electrical node to the current list.
C 'DEL' - remove a node from the current list.
C 'CPY' - copy a node in the current list.


      SUBROUTINE EDENOD(MODE,INOD)

#include "power.h"
#include "plant.h"
#include "building.h"
      
      integer lnblnk  ! function definition

C Commons
      COMMON/POPHELP/H(60)
      COMMON/OUTIN/IUOUT,IUIN

C The electrical nodes
      COMMON/ENOD/NENOD,ENODNO(MENOD),ENODPHTYP(MENOD),
     &ENODPH(MENOD),ENODTYP(MENOD),ENODBASEV(MENOD),
     &ENODBASEP

C The electrical network topology
      COMMON/ECONN/NECON,ECONO(MECON),CONPHTYP(MECON),
     &CONPH(MECON,MPHAS),CCNO(MECON),SENOD(MECON,MPHAS),
     &EENOD(MECON,MPHAS)

C The connected power only components
      COMMON/POWC/NPOWCOM,POWCOMNO(MPOWCOM),POWCOMID(MPOWCOM),
     &POWCOMPHTYP(MPOWCOM),POWCONEN(MPOWCOM,MPHAS),
     &NPOWCDAT(MPOWCOM),NPOWCDATS(MPOWCOM),POWCDAT(MPOWCOM,MPWDAT)

C The connected hybrid components
      COMMON/HYBE/NHYBCOM,HYBCOMNO(MHYCOM),HYCOMTYP(MHYCOM),
     &HYCOMPHTYP(MHYCOM),HYCONEN(MHYCOM,MPHAS),HYLOC(MHYCOM,MPHAS),
     &NHYBDAT(MHYCOM),HYBDAT(MHYCOM,MHYDAT)

C Node and component names
      COMMON/ECNAMES/ENODNAM(MENOD),HYCOMNAM(MHYCOM),
     &HYCOMTYPSTR(MHYCOM),POWCOMNAM(MPOWCOM),POWCDATS(MPOWCOM,MPCDS),
     &CONECOMNAM(MCONECOM)

C The descriptive strings
      COMMON/ECDESC/ENTYPESTR(MENTYP),PHTYPSTR(MPHATY),ENDESC,
     &ENODTYPSTR(MENOD),HYDESC(MHYCOM),CONECOMDESC(MCONECOM),
     &POWCOMDESC(MPOWCOM)

C Hybrid components that calculate nodal voltages
      COMMON/calcV_LOC/iplant_calcV_LOC(MENOD),iPV_calcV_LOC(MENOD)

C Plant commons
      COMMON/PCELFLG/IPCELF(MPCOM)
      COMMON/C9/NPCOMP,NCI(MPCOM),CDATA(MPCOM,MMISCD)

C Special materials 
      common/spmatl/nspmnod,ispmloc(mspmnod,3),ispmtyp(mspmnod,2),
     &nnodat(mspmnod),spmdat(mspmnod,mspmdat) 

C Type casting (all varaiables and arrays cast explicitly)
      INTEGER NENOD,ENODNO,ENODPHTYP,ENODPH,ENODTYP,NECON,ECONO,
     &CONPHTYP,CONPH,CCNO,INOD,SENOD,EENOD,IADD,NANOD,IENOD,JENOD,
     &JLEN,IENT, NPOWCOM,POWCOMNO,POWCOMID,POWCOMPHTYP,POWCONEN,
     &NPOWCDAT,NPOWCDATS, NHYBCOM,HYBCOMNO,HYCOMTYP,HYCOMPHTYP,HYCONEN,
     &HYLOC,NHYBDAT,iplant_calcV_LOC,iPV_calcV_LOC

      DIMENSION STREN(MPHAS)

      REAL ENODBASEV,ENODBASEP
      
      LOGICAL OK,dok
C Characters
      CHARACTER*2 STREN
      CHARACTER*3 MODE
      CHARACTER*10 ROOT
      CHARACTER*12 ENTYPESTR,ENODTYPSTR,ENODNAM,HYCOMTYPSTR,HYCOMNAM,
     &POWCOMNAM,CONECOMNAM,PHTYPSTR,STR,SHSTR
      CHARACTER*72 ENDESC,HYDESC,POWCOMDESC,POWCDATS,CONECOMDESC,
     &H
      CHARACTER*124 OUTS

C Set up the local phase string endings
      STREN(1)='_1'
      STREN(2)='_2'
      STREN(3)='_3'

C Edit the node according to the calling mode
      IF(MODE.EQ.'ADD') THEN
        ROOT=' '
        JLEN=0

C Ask the user how many nodes they want to add.
        H(1)='The user has the option to add up to three nodes    '
        H(2)='depending on the nature of the point in the         '
        H(3)='network being modelled:                             '
        H(4)='1 - a one phase or balanced system node             '
        H(5)='2 - a two phase point in the network                '
        H(6)='3 - a three phase point in the network              '
        H(7)='Points of more than one phase in an a.c. system are '
        H(8)='described by a "multi-phase set" of 2-3 nodes, each '
        H(9)='of the form: root_name_phase_extension.             '
        H(10)='For example: generator_1, generator_2, generator_3 '
        H(11)='or lighting_1, lighting_2 or fan_1, fan_3, etc.    '
        NANOD=0
        CALL EASKATOG
     &  ('What type of node(s) do you want to add (see help)? ',
     &'(No. in brackets = No. of nodes added).',
     &'d.c (1)','1-phase (1)','2-phase (2) ','3-phase (3)',
     &'Balanced (1)','Continue',' ',INODT,11)
        
        IF(INODT.EQ.7.OR.INODT.EQ.6) RETURN

        IF(INODT.EQ.1.OR.INODT.EQ.2.OR.INODT.EQ.5)THEN
          NANOD=1
          CALL EDISP(IUOUT,' ')
          CALL EDISP(IUOUT,'Adding one node to the network')
        ELSEIF(INODT.EQ.3)THEN
          NANOD=2
          CALL EDISP(IUOUT,' ')
          CALL EDISP(IUOUT,'Adding two nodes to the network')
        ELSE
          NANOD=3
          CALL EDISP(IUOUT,' ')
          CALL EDISP(IUOUT,'Adding three nodes to the network')
        ENDIF

C Check the maximum number of nodes is not exceeded
        IF((NENOD+NANOD).GT.MENOD) THEN
          CALL EDISP(IUOUT,
     &'ERROR: the maximum number of nodes will be exceeded.')
          RETURN
        ENDIF 

C Store the old number of nodes (used in name checking)
        NND=NENOD

C Loop through each added node 
        DO 10 IADD=1,NANOD

          NENOD=NENOD+1
          IENOD=NENOD

C Set the node type
          IF(INODT.EQ.1) THEN
            ENODPHTYP(IENOD)=1
          ELSEIF(INODT.EQ.2) THEN
            ENODPHTYP(IENOD)=2
          ELSEIF(INODT.EQ.3) THEN
            ENODPHTYP(IENOD)=3
          ELSEIF(INODT.EQ.4) THEN
            ENODPHTYP(IENOD)=4
          ELSEIF(INODT.EQ.5)THEN
            ENODPHTYP(IENOD)=5
          ENDIF

C Set the node number
   5      ENODNO(IENOD)=IENOD 

          IF(IADD.EQ.1) THEN
            IENT=0

C Determine the type of the node in question
            H(1)='The voltage type specifies whether the nodal voltage'
            H(2)='is fixed by the user or calculated during the       '
            H(3)='simulation. Generally nodal voltages should be      '
            H(4)='calculated; however, those connected to the grid or '
            H(5)='a large power source can be considered as fixed.    '
            H(6)='Three methods are available for calculating nodal   '
            H(7)='voltages:                                           '
            H(8)='  - `Variable` means that the voltage is determined '
            H(9)='    by the solution of the power flow matrix of eqs.'
           H(10)='  - `PV` means that the voltage is calculated by    '
           H(11)='     the buiding domain`s PV model.                 '
           H(12)='  - `Plant` means that the voltage is calculated by '
           H(13)='    a component in the plant domain.                '
            CALL EASKABCD('What is the node type?',' ',
     &                    'Variable voltage',
     &                    'Fixed voltage',
     &                    'Calc by PV',
     &                    'Calc by plant component',
     &                    ENODTYP(IENOD),13)

C Set the node type for the multi-phase set
            IENT=ENODTYP(IENOD)
            IF(ENODTYP(IENOD).EQ.1)THEN
              ENODTYPSTR(IENOD)='variable'
            ELSEIF(ENODTYP(IENOD).EQ.2)THEN
              ENODTYPSTR(IENOD)='fixed_V'
            ELSEIF(ENODTYP(IENOD).EQ.3)THEN
              ENODTYPSTR(IENOD)='calc_PV'
C Present a list of the current special materials and have user select the special
C material that calculates the nodal voltage. Begin by chedking
C that there are special materials from which to choose.
              CALL USRMSG(' ',' ','-')
              if ( nspmnod .eq. 0 ) then
                WRITE(OUTS,'(A)')
     &            'No special materials have been detected, possibly'
                CALL EDISP(IUOUT,OUTS)
                WRITE(OUTS,'(A)')
     &            'no special materials have been defined.'
                CALL EDISP(IUOUT,OUTS)
              endif

C Display list of special materials
              ok = .true.
              do while (ok)
                call edsplist('LG',ISPM,IER)
                if ( ispm .gt. 0 ) then

C Check that the special material chosen is a PV model that calculates the
C voltage - i.e. of type 1 (ESP-r's original PV model)or type 5 (WATSUN-PV).
                  if ( (ispmtyp(ispm,1) .ne. 1) .and.
     &                 (ispmtyp(ispm,1) .ne. 5) ) then
                    h(1)='Only the original esp-r PV model or a type 5'
                    h(2)='WATSUN-PV can be used. '
                    dok=.true.
                    CALL ASKOK(
     &                'The selected item is not a valid PV component.',
     &                'Try again?',ok,dok,2)
                  else
                    iPV_calcV_LOC(IENOD) = ISPM
                    ok = .false.
                  endif
                else
                  dok=.true.
                  h(1)='Please select a valid component. If you say no'
                  h(2)='then it get you out of the question loop.'
                  CALL ASKOK('No special material component picked.',
     &              'Try again?',ok,dok,2)
                endif
              end do          
            ELSEIF(ENODTYP(IENOD).EQ.4)THEN
              ENODTYPSTR(IENOD)='calc_plant'
C-------------Have user select the plant component that calculates the nodal voltage.
C-------------First check that there are plant components from which to choose. 
              CALL USRMSG(' ',' ','-')
              IF(NPCOMP.EQ.0) THEN
                WRITE(OUTS,'(A)') 
     &            'No plant components have been detected, possibly'
                CALL EDISP(IUOUT,OUTS)
                WRITE(OUTS,'(A)') 'no plant has been defined.'
                CALL EDISP(IUOUT,OUTS)
              ENDIF
C-------------Plant components exist: now have user select one.
C-------------Ensure that plant component has electrical data.
              OK = .true.
              DO WHILE (OK)
                CALL ASKPCMP('Select plant comp: ','-',IPCOMP,IER)
                IF(IPCOMP.NE.0) THEN
                  IF(IPCELF(IPCOMP).NE.0) THEN
                    iplant_calcV_LOC(IENOD) = IPCOMP ! Plant comp that calcs V.
                    OK = .false.
                  ELSE
                    dok=.true.
                    h(1)='Please select a valid component. If you say'
                    h(2)='no then it get you out of the question loop.'
                    CALL ASKOK(
     &      'No elec info associated with this plant component so it',
     &      'cannot calc a nodal voltage. Try another component?',
     &                OK,DOK,2)
                  ENDIF
                ENDIF
              END DO
C-----------All nodal options have been exhausted.
            ELSE
            ENDIF
          ELSE
            IF(IENT.EQ.1)THEN
              ENODTYPSTR(IENOD)='variable'
            ELSE
              ENODTYPSTR(IENOD)='fixed_V'
            ENDIF
          ENDIF 

C Phase type for this node also set string length for naming conventions
          IF(NANOD.EQ.1) THEN
            ISTRL=12
            STR='Node_name'
            WRITE(H(1),'(A,I2,A)')
     &'Enter a ',ISTRL,' letter descriptive name for the node. Do not'
            H(2)='duplicate names. If the node is one of a multi'
            H(3)='-phase set then the name will be the root name.'
            SHSTR=ENODNAM(IENOD)
            CALL EASKS(SHSTR,
     &        'The name or root name of this node (see help) ?',
     &        '(No blanks) ',ISTRL,STR,'Node name',IER,3)
            ENODNAM(IENOD)=SHSTR
          ELSEIF(NANOD.GT.1) THEN
            ISTRL=10

C Allow the root to be set for the first node only. 
            IF(IADD.EQ.1) THEN
              ROOT='Root_name'
              ENODNAM(IENOD)=ROOT
              WRITE(H(1),'(A,I2,A)')
     &'Enter a ',ISTRL,' letter descriptive name for the node. Do not'
              H(2)='duplicate names. If the node is one of a multi'
              H(3)='-phase set then this name will be the root name.'
              SHSTR=ENODNAM(IENOD)
              CALL EASKS(SHSTR(1:10),
     &          'The name or root name of this node (see help) ?',
     &          '(No blanks) ',ISTRL,ROOT,'Node name',IER,3)
              ENODNAM(IENOD)=SHSTR(1:10)
              JLEN=LNBLNK(ENODNAM(IENOD))
              ROOT=ENODNAM(IENOD)(1:JLEN)
            ENDIF
          ENDIF

C Determine which phase the node is connected to.
          ENODPH(IENOD)=IADD
          H(1)='Specify which phase of the electrical network the node'
          H(2)='is associated with. Note, for balanced and d.c. nodes'
          H(3)='simply specify 1.'
          H(4)='NOTE: two nodes in a multi-phase set cannot have the'
          H(5)='same phase! ' 
          CALL USRMSG(' ',' ','-')   
          IF(NANOD.GT.1)THEN                             
            WRITE(OUTS,'(A,I3,3A)')'What phase is node ',IADD,' of ',
     &        ROOT(1:LNBLNK(ROOT)),' associated with (see help) ?'
            CALL EASKI(ENODPH(IENOD),OUTS,' ',1,'F',5,'F',
     &        IADD,'node phase',IER,5)

C Add the phase ending to the node name for nodes which form part of a 
C multi-phase set.  
            JLEN=LNBLNK(ROOT)
            WRITE(ENODNAM(IENOD),'(A,A)')
     &        ROOT(1:JLEN),STREN(ENODPH(IENOD))
          ELSE
            WRITE(OUTS,'(A,I3,3A)')'What phase is node ',IADD,' of ',
     &        ENODNAM(IENOD)(1:LNBLNK(ENODNAM(IENOD))),
     &        ' associated with (see help) ?'
            CALL EASKI(ENODPH(IENOD),OUTS,' ',1,'F',5,'F',
     &        IADD,'node phase',IER,5)
          ENDIF  

Check for duplicate names
          DO 30 IND=1,NND
            IF(LNBLNK(ENODNAM(IND)).EQ.LNBLNK(ENODNAM(IENOD)))THEN
              JLEN=LNBLNK(ENODNAM(IENOD))
              IF((ENODNAM(IND)(1:JLEN)).EQ.
     &           (ENODNAM(IENOD)(1:JLEN)))THEN
                IF(IENOD.NE.IND) THEN
                  WRITE(OUTS,'(3A)') ENODNAM(IND),' & ',
     &              ENODNAM(IENOD)
                  CALL EDISP(IUOUT,OUTS)
                  WRITE(OUTS,'(A,I3,A,I3)')
     &'Duplicate or similar names or root names found for nodes ',
     &IND,' & ',IENOD
                  CALL EDISP(IUOUT,OUTS)
                  CALL EDISP(IUOUT,' ')
                  GOTO 5
                ENDIF
              ENDIF
            ENDIF
  30      CONTINUE 
  10    CONTINUE

      ELSEIF(MODE.EQ.'DEL') THEN

C Ask if it is OK to delete associated connections and nodes, if not then return
        STR=' '
        ISTRL=0
        IENOD=INOD
        H(1)='When deleting a node any connections assoctated with'
        H(2)='that node must also be deleted. Also if the node is part'
        H(3)='of a multi-phase set then the other members of that set' 
        H(4)='must also be deleted. These deletions are done  '
        H(5)='automatically. Any linkages to power only components or'
        H(6)='hybrid components are nulled.'
        WRITE(OUTS,'(A)')'Delete associated connections and/or nodes?'
        dok=.false.
        CALL ASKOK(OUTS,' ',OK,dok,6)
        IF(.NOT.OK) RETURN

C Firstly check for connections incorporating the node and delete 
        DO 40 ICON=NECON,1,-1
          DO 50 IPH=1,MPHAS
            IF(SENOD(ICON,IPH).EQ.IENOD.OR.
     &         EENOD(ICON,IPH).EQ.IENOD)THEN
              CALL EDECON('DEL',ICON)
              WRITE(OUTS,'(A,I3)') 'Deleted connection ... ',ICON
              CALL EDISP(IUOUT,OUTS)
            ENDIF

C Also account for the change in the nodal list.
            IF(SENOD(ICON,IPH).GT.IENOD)THEN
              SENOD(ICON,IPH)=SENOD(ICON,IPH)-1
            ELSEIF(EENOD(ICON,IPH).GT.IENOD)THEN
              EENOD(ICON,IPH)=EENOD(ICON,IPH)-1
            ENDIF
  50      CONTINUE
  40    CONTINUE

C Check for linkages to power only and hybrid components and zero them.
C Also update linkages for changes to the nodal list.
        DO 60 IPWC=1,NPOWCOM
          DO 70 IPH=1,MPHAS
            IF(IENOD.EQ.POWCONEN(IPWC,IPH)) THEN 
              POWCONEN(IPWC,IPH)=0
              WRITE(OUTS,'(A,I3,A,I3)') 'Power comp. ',IPWC,
     &        ' disconnected from node ',  IENOD
            ENDIF

C Also account for the change in the nodal list.
            IF(POWCONEN(IPWC,IPH).GT.IENOD) THEN
              POWCONEN(IPWC,IPH)=POWCONEN(IPWC,IPH)-1
            ENDIF
  70      CONTINUE
  60    CONTINUE

C Now check for linkages to hybrid components and zero them. 
C Also update linkages for changes to the nodal list.
        DO 80 IHYC=1,NHYBCOM
          DO 90 IPH=1,MPHAS
            IF(IENOD.EQ.HYCONEN(IHYC,IPH)) THEN 
              HYCONEN(IHYC,IPH)=0
              WRITE(OUTS,'(A,I3,A,I3)') 'Hybrid comp. ',IHYC,
     &        ' disconnected from node ',  IENOD
            ENDIF

C Also account for the change in the nodal list.
            IF(HYCONEN(IHYC,IPH).GT.IENOD) THEN
              HYCONEN(IHYC,IPH)=HYCONEN(IHYC,IPH)-1
            ENDIF
  90      CONTINUE
  80    CONTINUE

C Firstly store the deleted node name and its length.
        STR=ENODNAM(IENOD)
        JLEN=LNBLNK(STR)
C Delete the node
        IF(IENOD.LT.NENOD)THEN
          DO 100 I=IENOD,NENOD-1
            ENODNO(I)=I 
            ENODNAM(I)=ENODNAM(I+1) 
            ENODTYP(I)=ENODTYP(I+1)
            ENODPH(I)=ENODPH(I+1)
            ENODPHTYP(I)=ENODPHTYP(I+1)
            ENODTYPSTR(I)=ENODTYPSTR(I+1)
            ENODBASEV(I)= ENODBASEV(I+1)
 100      CONTINUE
        ENDIF
        NENOD=NENOD-1
         

C Secondly check if the node is part of a three node set and delete those nodes as
C well. Also deal with linkages and connections to these nodes.
        DO 110 INODE=NENOD,1,-1
          IF(ENODPHTYP(INODE).EQ.3.OR.ENODPHTYP(INODE).EQ.4) THEN
            IF(LNBLNK(ENODNAM(INODE)).EQ.LNBLNK(STR))THEN
              IF(ENODNAM(INODE)(1:JLEN-2).EQ.STR(1:JLEN-2)) THEN
                IENOD=INODE

C Delete connections associated with this node  
                DO 120 ICON=NECON,1,-1
                  DO 130 IPH=1,MPHAS
                    IF(SENOD(ICON,IPH).EQ.IENOD.OR.EENOD(ICON,IPH).EQ.
     &                 IENOD)THEN
                      CALL EDECON('DEL',ICON)
                      WRITE(OUTS,'(A,I3)') 
     &                  'Deleted connection ... ',ICON
                      CALL EDISP(IUOUT,OUTS)
                    ENDIF

C Also account for the change in the nodal list.
                    IF(SENOD(ICON,IPH).GT.IENOD)THEN
                     SENOD(ICON,IPH)=SENOD(ICON,IPH)-1
                    ELSEIF(EENOD(ICON,IPH).GT.IENOD)THEN
                     EENOD(ICON,IPH)=EENOD(ICON,IPH)-1
                    ENDIF
 130              CONTINUE
 120            CONTINUE


C Check for linkages to power only and hybrid components and zero them.
                DO 140 IPWC=1,NPOWCOM
                  DO 150 IPH=1,MPHAS
                    IF(IENOD.EQ.POWCONEN(IPWC,IPH)) THEN 
                      POWCONEN(IPWC,IPH)=0
                      WRITE(OUTS,'(A,I3,A,I3)') 'Power comp. ',IPWC,
     &                ' disconnected from node ',  IENOD
                    ENDIF

C Also account for the change in the nodal list.
                    IF(POWCONEN(IPWC,IPH).GT.IENOD) THEN
                      POWCONEN(IPWC,IPH)=POWCONEN(IPWC,IPH)-1
                    ENDIF
 150              CONTINUE
 140            CONTINUE

C Now check for linkages to hybrid components and zero them. 
                DO 160 IHYC=1,NHYBCOM
                  DO 170 IPH=1,MPHAS
                    IF(IENOD.EQ.HYCONEN(IPWC,IPH)) THEN 
                      POWCONEN(IPWC,IPH)=0
                      WRITE(OUTS,'(A,I3,A,I3)') 'Hybrid comp. ',IHYC,
     &                ' disconnected from node ',  IENOD
                    ENDIF

C Also account for the change in the nodal list.
                    IF(HYCONEN(IHYC,IPH).GT.IENOD) THEN
                      HYCONEN(IHYC,IPH)=HYCONEN(IHYC,IPH)-1
                    ENDIF
 170              CONTINUE
 160            CONTINUE

                IF(IENOD.LT.NENOD)THEN
                  DO 190 I=IENOD,NENOD-1
                    ENODNO(I)=I
                    ENODNAM(I)=ENODNAM(I+1) 
                    ENODTYP(I)=ENODTYP(I+1)
                    ENODPH(I)=ENODPH(I+1)
                    ENODPHTYP(I)=ENODPHTYP(I+1)
                    ENODTYPSTR(I)=ENODTYPSTR(I+1)
                    ENODBASEV(I)= ENODBASEV(I+1)
 190              CONTINUE
                ENDIF
                NENOD=NENOD-1                
              ENDIF
            ENDIF
          ENDIF
 110    CONTINUE


      ELSEIF(MODE.EQ.'EDT') THEN 
        ISTRL=0
        STR=' '
        dok=.false.
        h(1)='A chance to continue if wrong node selected. '
        CALL ASKOK('Edit this node?',' ',OK,dok,1)
        IF(.NOT.OK) RETURN
        IENOD=INOD
C To be added

        ISTRL=12
        IF(ENODPHTYP(IENOD).EQ.3.OR.ENODPHTYP(IENOD).EQ.4) ISTRL=10
        WRITE(H(1),'(A,I2,A)')
     &'Enter a ',ISTRL,' letter descriptive name for the node. Do not'
        H(2)='duplicate names. If the node is one of a multi-phase'
        H(3)='set then this name will be the root name.           '
        H(4)=' '
        H(5)='Editing of nodal data is restricted to altering         '
        H(6)='the component name and the nodal type. If the phase type'
        H(7)='or phase number needs to be changed then please delete  '
        H(8)='the node and create a new one with the desired          '
        H(9)='properties.    '

C Save the old node name 
        STR=ENODNAM(IENOD)
        ENODNAM(IENOD)=ENODNAM(IENOD)(1:ISTRL)
        SHSTR=ENODNAM(IENOD)
        CALL EASKS(SHSTR(1:ISTRL),
     &'The name or root name of this node (see help) ?','(No blanks) ',
     &ISTRL,ENODNAM(IENOD)(1:ISTRL),'Node name',IER,9)
        ENODNAM(IENOD)(1:ISTRL)=SHSTR(1:ISTRL)
        IF(ENODPHTYP(IENOD).EQ.3.OR.ENODPHTYP(IENOD).EQ.4) THEN
          JLEN=LNBLNK(ENODNAM(IENOD))
          WRITE(ENODNAM(IENOD),'(A,A)')
     &ENODNAM(IENOD)(1:JLEN),STREN(ENODPH(IENOD))
        ENDIF


C Ask for the new node type. 
        H(1) ='The voltage type specifies whether the nodal voltage '
        H(2) ='is fixed by the user or calculated during the        '
        H(3) ='simulation. Generally nodal voltages should be       '
        H(4) ='calculated; however, those connected to the grid or  '
        H(5) ='a large power source can be considered as fixed.     '
        H(6) ='Three methods are available for calculating nodal    '
        H(7) ='voltages:                                            '
        H(8) ='  - `Variable` means that the voltage is determined  '
        H(9) ='    by the solution of the power flow matrix of eqs. '
        H(10)='  - `PV` means that the voltage is calculated by     '
        H(11)='     the buiding domain`s PV model.                  '
        H(12)='  - `Plant` means that the voltage is calculated by  '
        H(13)='    a component in the plant domain.                 '
        CALL EASKABCD('What is the node type?',' ',
     &                'Variable voltage',
     &                'Fixed voltage',
     &                'Calc by PV',
     &                'Calc by plant component',
     &                ENODTYP(IENOD),13)

C Set the node type for the multi-phase set
        IENT=ENODTYP(IENOD)
        IF(ENODTYP(IENOD).EQ.1)THEN
          ENODTYPSTR(IENOD)='variable'
        ELSEIF(ENODTYP(IENOD).EQ.2)THEN
          ENODTYPSTR(IENOD)='fixed_V'
        ELSEIF(ENODTYP(IENOD).EQ.3)THEN
          ENODTYPSTR(IENOD)='calc_PV'
C Present a list of the current special materials and have user select the special
C material that calculates the nodal voltage
          CALL USRMSG(' ',' ','-')
C Check that there are special materials from which to choose.
          if ( nspmnod .eq. 0 ) then
             WRITE(OUTS,'(A)')
     &              'No special materials have been detected, possibly'
             CALL EDISP(IUOUT,OUTS)
             WRITE(OUTS,'(A)') 'no special materials have been defined.'
             CALL EDISP(IUOUT,OUTS)
          endif
C Display list of special materials
          ok = .true.
          do while (ok)
             call edsplist('LG',ISPM,IER)
             if ( ispm .gt. 0 ) then
C Check that the special material chosen is a PV model that calculates the
C voltage - i.e. of type 1 (ESP-r's original PV model)or type 5 (WATSUN-PV).
                if ( (ispmtyp(ispm,1) .ne. 1) .and.
     &               (ispmtyp(ispm,1) .ne. 5) ) then
                  h(1)='Only the original esp-r PV model or a type 5 '
                  h(2)='WATSUN-PV can be used. '
                  dok=.true.
                  CALL ASKOK(
     &              'The selected item is not a valid PV component.',
     &              'Try again?',ok,dok,2)
                else
                  iPV_calcV_LOC(IENOD) = ISPM
                  ok = .false.
                endif
             else
               dok=.true.
               h(1)='Please select a valid component. If you say no'
               h(2)='then it get you out of the question loop.'
               CALL ASKOK('No special material component picked.',
     &           'Try again',ok,dok,2)
             endif
          end do          
        ELSEIF(ENODTYP(IENOD).EQ.4)THEN
          ENODTYPSTR(IENOD)='calc_plant'
C---------Have user select the plant component that calculates the nodal voltage.
          CALL USRMSG(' ',' ','-')
C---------First check that there are plant components from which to choose. 
          IF(NPCOMP.EQ.0) THEN
            WRITE(OUTS,'(A)') 
     &            'No plant components have been detected, possibly'
            CALL EDISP(IUOUT,OUTS)
            WRITE(OUTS,'(A)') 'no plant has been defined.'
            CALL EDISP(IUOUT,OUTS)
          ENDIF
C---------Plant components exist: now have user select one.
C---------Ensure that plant component has electrical data.
          OK = .true.
          DO WHILE (OK)
            CALL ASKPCMP('Select plant comp: ','-',IPCOMP,IER)
            IF(IPCOMP.NE.0) THEN
              IF(IPCELF(IPCOMP).NE.0) THEN
                iplant_calcV_LOC(IENOD) = IPCOMP ! Plant comp that calcs V.
                OK = .false.
              ELSE
                dok=.true.
                h(1)='Please select a valid component. If you say no'
                h(2)='then it get you out of the question loop.'
                CALL ASKOK(
     &   'No elec info associated with this plant component so it',
     &   'cannot calc a nodal voltage. Try another component?',
     &            OK,DOK,2)
              ENDIF
            ENDIF
          END DO
C-------All nodal options have been exhausted.
        ELSE
        ENDIF

C If if the node is part of a multi-phase set loop through and change the
C other nodal data. 
        IF(ENODPHTYP(IENOD).EQ.3.OR.ENODPHTYP(IENOD).EQ.4) THEN
          DO 200 I=1,NENOD
          IF(I.NE.IENOD) THEN
            JLEN=LNBLNK(STR)
            IF(ENODNAM(I)(1:JLEN-2).EQ.STR(1:JLEN-2))THEN
               JLEN=LNBLNK(ENODNAM(IENOD))
               ENODNAM(I)=ENODNAM(IENOD)
               WRITE(ENODNAM(I),'(A,A)')
     &ENODNAM(IENOD)(1:JLEN-2),STREN(ENODPH(I))
               ENODTYP(I)=ENODTYP(IENOD)
               ENODTYPSTR(I)=ENODTYPSTR(IENOD)
            ENDIF
          ENDIF
200       CONTINUE
        ENDIF
        
      ELSEIF(MODE.EQ.'CPY') THEN

        NANOD=0
C Check the maximum number of nodes is not exceeded
        IF(ENODPHTYP(INOD).EQ.3) THEN
          NANOD=2
        ELSEIF(ENODPHTYP(INOD).EQ.4) THEN
          NANOD=3
        ELSE
          NANOD=1
        ENDIF
        IF((NENOD+NANOD).GT.MENOD) THEN
          CALL EDISP(IUOUT,
     &'ERROR: maximum number of nodes will be exceeded ')   
          RETURN
        ENDIF  

        NENOD=NENOD+1
        IENOD=NENOD
C Firstly get the new name
  11    ISTRL=12
        IF(NANOD.GT.1) ISTRL=10
        H(1)='Give the copied node a unique name or root name.'
        ENODNAM(IENOD)='Copy_node'
        SHSTR=ENODNAM(IENOD)
        CALL EASKS(SHSTR,
     &'A name for the the copied node?','(no blanks) ',ISTRL,
     &ENODNAM(IENOD)(1:ISTRL),'copy name',IER,2)
         ENODNAM(IENOD)=SHSTR
Check for duplicate names
         DO 210 IND=1,NENOD-1
           JLEN=LNBLNK(ENODNAM(IENOD))
           IF((ENODNAM(IND)(1:JLEN)).EQ.
     &(ENODNAM(IENOD)(1:JLEN)))THEN
             IF(IENOD.NE.IND) THEN
               WRITE(OUTS,'(3A)') ENODNAM(IND),' & ',ENODNAM(IENOD)
               CALL EDISP(IUOUT,OUTS)
               WRITE(OUTS,'(A,I3,A,I3)')
     &'Duplicate or similar names found for nodes ',IND,' & ',IENOD
               CALL EDISP(IUOUT,OUTS)
               CALL EDISP(IUOUT,' ')
               GOTO 11
             ENDIF
           ENDIF
 210     CONTINUE

C Copy the node data, if the node is part of a multi-phase set then copy the 
C other nodes as well. 
        ENODNO(IENOD)=IENOD
        ENODPH(IENOD)=ENODPH(INOD)
        ENODTYP(IENOD)=ENODTYP(INOD)
        ENODPHTYP(IENOD)=ENODPHTYP(INOD)
        ENODBASEV(IENOD)=ENODBASEV(INOD)
        ENODTYPSTR(IENOD)=ENODTYPSTR(INOD)

C Add the string extension onto the root name.
        IF(NANOD.GT.1) WRITE(ENODNAM(IENOD),'(A,A)')
     &ENODNAM(IENOD)(1:JLEN),STREN(ENODPH(IENOD))


C Check for the other nodes in a multi node set to copy. 
        IF(ENODPHTYP(INOD).EQ.3.OR.ENODPHTYP(INOD).EQ.4) THEN

C Scan the existing nodel list (not the newly added node and check for 
C a name match. If a match is found then add a new copied node. 
          NNOD=NENOD-1
          STR=ENODNAM(INOD)
          JLEN=LNBLNK(STR)
          DO 220 I=1,NNOD
            IF(I.NE.INOD) THEN
              IF(ENODPHTYP(I).EQ.3.OR.ENODPHTYP(I).EQ.4) THEN
                IF(LNBLNK(ENODNAM(I)).EQ.LNBLNK(STR))THEN
                  IF(ENODNAM(I)(1:JLEN-2).EQ.STR(1:JLEN-2)) THEN

C Add a new node if a name match is found - copy this node's data
                    NENOD=NENOD+1
                    JENOD=NENOD
                    ENODNO(JENOD)=IENOD
                    ENODPH(JENOD)=ENODPH(I)
                    ENODTYP(JENOD)=ENODTYP(I)
                    ENODPHTYP(JENOD)=ENODPHTYP(I)
                    ENODBASEV(JENOD)=ENODBASEV(I)
                    ENODTYPSTR(JENOD)=ENODTYPSTR(I)
                    ENODNAM(JENOD)=ENODNAM(IENOD-1)
                    WRITE(ENODNAM(JENOD),'(A,A)')
     &ENODNAM(IENOD)(1:JLEN-2),STREN(ENODPH(I))
                  ENDIF
                ENDIF
              ENDIF 
            ENDIF         
 220      CONTINUE
        ENDIF
      ENDIF


      RETURN
      END

C ******************************* EDEHYB  ********************************************
C This routine edits the connected hybrid components HVAC, PV, lighting loads, equipment 
C loads according to various modes:
C 'ADD' - add a new HVAC/PV/Lighting/equipment load component.
C 'DEL' - delete the HVAC/PV/Lighting/equipment load component.
C 'EDT' - edit the HVAC/PV/Lighting/equipment load component.
C 'CPY' - copy a HVAC/PV/Lighting/equipment load component.

      SUBROUTINE EDEHYB(MODE,IHYB)

#include "power.h"
#include "plant.h"
#include "building.h"

      COMMON/POPHELP/H(60)
      COMMON/OUTIN/IUOUT,IUIN
      COMMON/FILEP/IFIL

C Electrical data flag for zones.
      COMMON/ELECFLG/IELF(MCOM)

C Plant commons
      COMMON/PCELFLG/IPCELF(MPCOM)
      COMMON/C9/NPCOMP,NCI(MPCOM),CDATA(MPCOM,MMISCD)

C Descriptive label for a zone load or casual gain types.
      COMMON/LOADLABEL/LODLABEL(MCOM,7)
      COMMON/P3TYPEN/ICGT(MDTY,MC)
      INTEGER ICGT

C The electrical nodes
      COMMON/ENOD/NENOD,ENODNO(MENOD),ENODPHTYP(MENOD),
     &ENODPH(MENOD),ENODTYP(MENOD),ENODBASEV(MENOD),
     &ENODBASEP

C The connected hybrid components
      COMMON/HYBE/NHYBCOM,HYBCOMNO(MHYCOM),HYCOMTYP(MHYCOM),
     &HYCOMPHTYP(MHYCOM),HYCONEN(MHYCOM,MPHAS),HYLOC(MHYCOM,MPHAS),
     &NHYBDAT(MHYCOM),HYBDAT(MHYCOM,MHYDAT)

C Node and component names
      COMMON/ECNAMES/ENODNAM(MENOD),HYCOMNAM(MHYCOM),
     &HYCOMTYPSTR(MHYCOM),POWCOMNAM(MPOWCOM),POWCDATS(MPOWCOM,MPCDS),
     &CONECOMNAM(MCONECOM)

C The descriptive strings
      COMMON/ECDESC/ENTYPESTR(MENTYP),PHTYPSTR(MPHATY),ENDESC,
     &ENODTYPSTR(MENOD),HYDESC(MHYCOM),CONECOMDESC(MCONECOM),
     &POWCOMDESC(MPOWCOM)

C Calendar commons
      common/caleni/nbdaytype,nbcaldays(MDTY),icalender(365)
      INTEGER NBDAYTYPE,NBCALDAYS,ICALENDER,IDTY,ITAG

C Plant component node connections
      common/pltnds/iplt_conn_nodes(MHYCOM),iplt_dcnode_id(MHYCOM),
     &              iplt_acnode_id(MHYCOM)
      integer  iplt_conn_nodes       !- holds the number of nodes the plant component
                                     !- is connected to (1 or 2). If connected to two
                                     !- nodes it is assumed one is AC and the other DC
                                     !- variable set to zero for hybrid components that
                                     !- are not plant components
      integer  iplt_dcnode_id        !- node index of the DC node the plant component
                                     !- connects to
      integer  iplt_acnode_id        !- node index of the AC node the plant component
                                     !- connects to


C Type casting (all varaiables and arrays cast explicitly)
      INTEGER NHYBCOM,HYBCOMNO,HYCOMTYP,HYCOMPHTYP,HYCONEN,HYLOC,
     &NHYBDAT,ENODNO,ENODPHTYP,ENODPH,ENODTYP,NENOD,NPCOMP,NCI,
     &I,J,K,IPHYB,IEHYB,IHYB,ISTRL,IAPPF

      REAL HYBDAT,ENODBASEV,ENODBASEP,CDATA
      DIMENSION TMPDESC(3),IVALS(5),TMPTYPSTR(3),IAPPF(3)

C Characters
      CHARACTER*3 MODE
      CHARACTER*6 LODLABEL
      CHARACTER*12 ENTYPESTR,ENODTYPSTR,ENODNAM,HYCOMTYPSTR,HYCOMNAM,
     &POWCOMNAM,CONECOMNAM,PHTYPSTR,TMPTYPSTR,SHSTR
      CHARACTER*72 ENDESC,HYDESC,POWCOMDESC,POWCDATS,CONECOMDESC,
     &H,TMPDESC,LNGSTR
      CHARACTER*124 OUTS
      LOGICAL OK,dok,PHCPT

      IF(MODE.EQ.'ADD'.OR.MODE.EQ.'EDT') THEN

C Set counter for a new hybrid component.
        IF(MODE.EQ.'ADD') THEN
          IEHYB=NHYBCOM+1
        ELSE
          dok=.false.
          h(1)='A chance to continue without editing component. '
          CALL ASKOK('Edit this component?',' ',OK,dok,1)
          IF(.NOT.OK) RETURN
C Set the number of the editing component to be edited and allow
C user to define component information (uses same code as new comp
C definition). 
          IEHYB=IHYB
        ENDIF

C Initialize flag for plant component node connections
        iplt_conn_nodes(iehyb) = 0
C Zero the node index variables
        iplt_dcnode_id(iehyb) = 0
        iplt_acnode_id(iehyb) = 0

C Set the component no.
        HYBCOMNO(IEHYB)=IEHYB

C Set default component descriptions, names and type names
  5     TMPDESC(1)='A zone-side electrical load'
        TMPTYPSTR(1)='zone'
        TMPDESC(2)='A power consuming/producing plant component'
        TMPTYPSTR(2)='spmaterial'
        TMPDESC(3)='A photovoltaic material'
        TMPTYPSTR(3)='plant'

C Ask the user if they want to add a zone/plant/special material component to the network
C and set the type and type string.
        H(1)='You can attach electrical loads and power sources to the'
        H(2)='electrical network. Please select the type of component '
        H(3)='you wish to attach: '
        H(4)='- a zone-side electrical load i.e. lighting;'
        H(5)='- a plant-side electrical load i.e. pump;   '
        H(6)='- a pv material in the building facade.     '
        H(7)='Note if none of these elements exist in the model     '
        H(8)='already, you will not be able to attach anything.      '
        CALL EASKABCD('Add which type of component ?',' ',
     &  'Zone electrical load','PV array','Plant component','Continue',
     &  IPHYB,8)   

        IF(IPHYB.EQ.4) RETURN 

C Set the type and the type string 
        HYCOMTYP(IEHYB)=IPHYB  
        HYCOMTYPSTR(IEHYB)=TMPTYPSTR(IPHYB)
               
C For the case of a zone load present a list of zones
        IF(IPHYB.EQ.1) THEN
          CALL USRMSG(' ',' ','-')
          IZ=-1
          WRITE(OUTS,'(A)') 
     &'In which zone is the load located?. '
 10       CALL ASKZONE(IZ,0,OUTS,'Select zone','-','load locate',IER)
          IF(IZ.EQ.0) RETURN

C Read in the selected zone's operations data ...
          IUF=IFIL+1
          CALL EROPER(0,IUOUT,IUF,IZ,IER)

          IF(IELF(IZ).EQ.0) THEN
             dok=.true.
             h(1)='The zone operation file selected did not contain'
             h(2)='any electrical data. You can select another zone.'
             CALL ASKOK('Zone has no electrical loads',
     &'try another zone?',OK,dok,2)
             IF(OK) THEN
               GOTO 10
             ELSE
               GOTO 5
             ENDIF
          ENDIF

C Get the appropriate day type and load. 
          H(1)='Each user specified gain type in each zone can be '
          H(2)='associated with a node in the electrical network. '
          H(3)='The cumulative power consumption of all the gains'
          H(4)='of that type (both absolute and per m2) is then   '
          H(5)='attributed to the node; provided load electrical data '
          H(6)='has been specified.'

C Scan through the zone loads and determine the active types
          TMPTYPSTR(1)=' '
          WRITE(TMPTYPSTR(1),'(A)') 'Inapplicable'
          IAPPF(1)=0
          TMPTYPSTR(2)=' '
          WRITE(TMPTYPSTR(2),'(A)') 'Inapplicable'
          IAPPF(2)=0
          TMPTYPSTR(3)=' '
          WRITE(TMPTYPSTR(3),'(A)') 'Inapplicable'
          IAPPF(3)=0

C Loop through each casual gain and determine if it is applicable or not.
          IF(NBDAYTYPE.GT.3)THEN
            CALL EDISP(IUOUT,'More than three day types are not')
            CALL EDISP(IUOUT,'supported for hybrid power components')
            CALL EDISP(IUOUT,'Warning generated from subroutine EDEHYB')
          ENDIF
          DO 500 IDTY=1,3
            ITAG=0
            DO 501 I=1,MC
              IF(ICGT(IDTY,I).EQ.1)ITAG=1
              IF(ITAG.EQ.1.AND.IAPPF(IDTY).EQ.0)THEN
                TMPTYPSTR(IDTY)=' '
                IF(LODLABEL(IZ,IDTY)(1:6).NE.'Occupt')THEN
                  TMPTYPSTR(IDTY)=' '
                  WRITE(TMPTYPSTR(IDTY),'(A)') LODLABEL(IZ,IDTY)
                  IAPPF(IDTY)=1
                ENDIF
              ENDIF
 501        CONTINUE
 500      CONTINUE
         
C Replace this with a menu if the number of load types increases.
          CALL USRMSG(' ',' ','-')
 16       CALL EASKABCD('What is the type of load do',
     &'you want to connect',TMPTYPSTR(1),TMPTYPSTR(2),
     &TMPTYPSTR(3),'Continue',ILOD,5)
          IF(ILOD.EQ.4) THEN
            RETURN
          ELSEIF(IAPPF(ILOD).EQ.0) THEN
            CALL EDISP
     &(IUOUT,'This load cannot be attached - try again')
            GOTO 16
          ELSE
            HYLOC(IEHYB,1)=IZ
            HYLOC(IEHYB,2)=ILOD
          ENDIF

C For the case of a PV material present a list of the current special materials
        ELSEIF(IPHYB.EQ.2) THEN
          CALL USRMSG(' ',' ','-')
 13       CALL EDSPLIST('LG',ISPM,IER) 
          CALL USRMSG(' ',' ','-')  
          IF(ISPM.GT.0) THEN
            HYLOC(IEHYB,1)=ISPM
          ELSE
            dok=.true.
            h(1)='ESP-r expected you to have selected a component and'
            h(2)='so here is your chance to try again. '
            WRITE(OUTS,'(A)')
     & 'No special material component picked so it cannot be connected'
            CALL EDISP(IUOUT,OUTS)
            CALL ASKOK(outs,'to the network. Try again?',OK,dok,2)
            IF(OK) THEN
              GOTO 13
            ELSE
              RETURN
            ENDIF
          ENDIF    

C Alternatively present a list of plant components - check the electrical flag is set.
        ELSEIF(IPHYB.EQ.3) THEN
          CALL USRMSG(' ',' ','-')

C Firstly check that there ARE plant components. 
          IF(NPCOMP.EQ.0) THEN
            WRITE(OUTS,'(A)') 
     &'No plant components have been detected, possibly no plant'
            CALL EDISP(IUOUT,OUTS)
            WRITE(OUTS,'(A)') 'has been defined.'
            CALL EDISP(IUOUT,OUTS)
          ENDIF
 12       CALL ASKPCMP('Select plant comp: ','-',IPCOMP,IER)
          IF(IPCOMP.NE.0) THEN
             IF(IPCELF(IPCOMP).NE.0) THEN
               HYLOC(IEHYB,1)=IPCOMP  

C Ask if the plant component is connected to 2 (vs 1) node
C This is to support cases where a plant component draws power from or
C generates power to two distinct nodes (one AC and the other DC).
C This feature is only supported for single phase types.
               call askok('Is the plant component connected to',
     &           'more than one node?', OK, .false.,1)
               if ( ok ) then
                 iplt_conn_nodes(iehyb) = 2
               else
                 iplt_conn_nodes(iehyb) = 1
               endif
             ELSE
               dok=.true.
            h(1)='ESP-r expected you to have selected a component with'
            h(2)='electrical data so here is your chance to try again.'
               CALL ASKOK(
     &'No electrical information associated with this plant component',
     &'so it cannot be connected to the network. Try another?',
     &OK,dok,2)
               IF(OK) THEN
                 GOTO 12
               ELSE
                 GOTO 5
               ENDIF                 
             ENDIF  
          ELSE   
            RETURN
          ENDIF 

        ENDIF

C If the hybrid component is not a plant component or if it is but it is 
C connected to one node then ask for the component phase type and
C based on this determine which node(s) hybrid component connects. 
        if ( iplt_conn_nodes(iehyb) .ne. 2 ) then
          H(1)='Please select the phase type of this component from'
          H(2)='the list.'
          IX=1
          CALL EPICKS(IX,IVALS,'Which phase type',
     &      'is this hybrid component?',12,5,PHTYPSTR,'phase type',
     &      IER,2)

          IF(IVALS(1).EQ.0) RETURN
          HYCOMPHTYP(IEHYB)=IVALS(1)

C Attach the component to a node in the network
          IF(HYCOMPHTYP(IEHYB).EQ.3) THEN
            NPICK=2
          ELSEIF(HYCOMPHTYP(IEHYB).EQ.4) THEN
            NPICK=3
          ELSE
            NPICK=1
          ENDIF

          TMPDESC(1)='Select FIRST node the component connects to: '
          TMPDESC(2)='Select SECOND node the component connects to: '
          TMPDESC(3)='Select THIRD node the component connects to: '

C Zero the existing locations
          HYCONEN(IEHYB,1)=0
          HYCONEN(IEHYB,2)=0
          HYCONEN(IEHYB,3)=0

C Loop to select attached nodes
          DO 30 I=1,NPICK

  35        CALL USRMSG(TMPDESC(I),' ','-')
            CALL ELNODLST('-',INOD)
            IF(INOD.LE.0.OR.INOD.GT.NENOD) RETURN

C Firstly check the phase types match d.c. can only be connected to d.c. Also
C if the component type is modelled as a balanced component (i.e. all phases
C are NOT modelled explicitly, then it must be connected to a balanced node.
C Otherwise all other connections are acceptable. 
            PHCPT=.TRUE.
            IF(ENODPHTYP(INOD).EQ.1.AND.HYCOMPHTYP(IEHYB).NE.1) THEN
              PHCPT=.FALSE.
            ELSEIF(ENODPHTYP(INOD).EQ.5.AND.HYCOMPHTYP(IEHYB).NE.5)THEN
              PHCPT=.FALSE.
            ELSE
              PHCPT=.TRUE.
            ENDIF
            IF(.NOT.PHCPT) THEN
              WRITE(OUTS,'(A)')
     &          'Incompatible phase types please select again'
              CALL EDISP(IUOUT,OUTS)
              GOTO 35
            ENDIF
          
Check the same node hasn't been picked twice
            IF(HYCONEN(IEHYB,ENODPH(INOD)).EQ.0) THEN
              HYCONEN(IEHYB,ENODPH(INOD))=INOD
            ELSE
              dok=.false.
              h(1)='Here is your chance to continue without over- '
              h(2)='writing the node already referencing this phase.'
              WRITE(OUTS,'(A)')
     &'A node has already been picked which occupies this phase'
              CALL ASKOK(OUTS,'Overwrite it?', OK,dok,2)
              IF(.NOT.OK) GOTO 35
              HYCONEN(IEHYB,ENODPH(INOD))=INOD
              WRITE(OUTS,'(A)') 'Node has been overwritten ....'
              CALL EDISP(IUOUT,OUTS)
              IF(NPICK.NE.1) THEN 
                WRITE(OUTS,'(A)') 
     &'Remember, because of the over write, the number of connected '
                CALL EDISP(IUOUT,OUTS) 
                WRITE(OUTS,'(A)') 
     &'nodes has not increased, so select another node to connect to.' 
                CALL EDISP(IUOUT,OUTS)     
                GOTO 35
              ENDIF
            ENDIF

  30      CONTINUE


C hybrid component is a plant component connected to 2 nodes
C It is assumed that one node is DC, the other AC.
        else

C This feature only supports single phase systems.
C Set phase type to d.c. - variable not used but set to avoid errors.
          HYCOMPHTYP(IEHYB) = 1

C Zero the existing locations
          HYCONEN(IEHYB,1) = 0
          HYCONEN(IEHYB,2) = 0
          HYCONEN(IEHYB,3) = 0

C Allow user to select the DC node connected to the hybrid plant component

          call usrmsg('Select the DC node the component connects to',
     &                ' ','-')

          call elnodlst('-',INOD)       !- display list of nodes

          if ( inod.le.0 .or. inod.gt.nenod ) return           !- no node selected

          iplt_dcnode_id(iehyb) = INOD

          hyconen(iehyb,1) = INOD      !- this is done to avoid errors in ENETASGN
                                       !- the hybrid component is linked to the
                                       !- dc node (phase 1)

C Allow user to select the AC node connected to the hybrid plant component
          ok = .true.
          do while (ok)
            call usrmsg('Select the AC node the component connects to',
     &                ' ','-')
            call elnodlst('-',INOD)                         !- display list of nodes

            if ( inod.le.0 .or. inod.gt.nenod ) return           !- no node selected

C Ensure that the AC node selected is not the same as the DC node
            if ( inod .ne. iplt_dcnode_id(iehyb) ) then
              iplt_acnode_id(iehyb) = INOD
              ok = .false.
            else
              dok=.true.
              h(1)='Please select a different nodes for AC and DC. Say'
              h(2)='no to get you out of the question loop.'
              call askok(
     &          'The AC node and DC node cannot be the same.',
     &          'Try again?',OK,dok,2)
            endif
          enddo

C Note: No check is done to ensure that the phase types of the DC node
C or the AC node chosen are correct.

        endif



C This space is reserved for the input of any additional hybrid component data.
C It is  currently not required.
C       <<HYBRID COMPONENT ADDITIONAL DATA>>

C Ask for the name of the component from the user
        ISTRL=12
        SHSTR=HYCOMNAM(IEHYB)
        CALL EASKS(SHSTR,
     &    'Enter a descriptive name for the component (see help)?',
     &    '(No blanks) ',ISTRL,' ','Hyb name',IER,3)
        HYCOMNAM(IEHYB)=SHSTR

C Ask for a description of the component from the user
        ISTRL=72
        LNGSTR=HYDESC(IEHYB)
        CALL EASKS(LNGSTR,
     &    'You can take this opportunity to enter a description ',
     &    'of this attached component. ',ISTRL,HYDESC(IEHYB),
     &    'Hyb desc',IER,3)
        HYDESC(IEHYB)=LNGSTR
        CALL USRMSG(' ',' ','-')

C If sucessful and a new component is being defined then
C increment the number of hybrid components by 1. 
        IF(IEHYB.EQ.NHYBCOM+1) NHYBCOM=NHYBCOM+1

      ELSEIF(MODE.EQ.'DEL') THEN
        IEHYB=IHYB
        IF(IEHYB.LT.NHYBCOM)THEN
          DO 50 I=IEHYB,NHYBCOM-1
            HYBCOMNO(I)=I
            HYCOMNAM(I)=HYCOMNAM(I+1) 
            HYCOMTYP(I)=HYCOMTYP(I+1)
            HYCOMTYPSTR(I)=HYCOMTYPSTR(I+1)
            HYDESC(I)=HYDESC(I+1)
            DO 60 J=1,MPHAS
              HYLOC(I,J)=HYLOC(I+1,J)
              HYCONEN(I,J)=HYCONEN(I+1,J)
 60         CONTINUE
            NHYBDAT(I)=NHYBDAT(I+1)
            DO 70 K=1,NHYBDAT(I)
              HYBDAT(I,K)=HYBDAT(I+1,K)
 70         CONTINUE
            iplt_conn_nodes(I) = iplt_conn_nodes(I+1)
            iplt_dcnode_id(I) = iplt_dcnode_id(I+1)
            iplt_acnode_id(I) = iplt_acnode_id(I+1)
 50       CONTINUE
        ENDIF
        NHYBCOM=NHYBCOM-1 

      ELSEIF(MODE.EQ.'CPY') THEN

C Copying is not practical for hybrid components. 
        CALL USRMSG('The copy facility is not available ',
     &'for HVAC/PV/.. components, define a new component instead.','-') 
        RETURN       

      ENDIF

      RETURN 
      END

C ******************************* EDEPOW  ********************************************
C This routine edits the current electrical power components according to various 
C modes:
C 'ADD' - add a new power component.
C 'DEL' - delete the current power component.
C 'EDT' - edit the current power component .
C 'CPY' - copy a power component.

      SUBROUTINE EDEPOW(MODE,IPOW)

#include "power.h"
      
      integer lnblnk  ! function definition

      COMMON/OUTIN/IUOUT,IUIN
      COMMON/POPHELP/H(60)  

C The electrical nodes
      COMMON/ENOD/NENOD,ENODNO(MENOD),ENODPHTYP(MENOD),
     &ENODPH(MENOD),ENODTYP(MENOD),ENODBASEV(MENOD),
     &ENODBASEP    

C The connected power only components
      COMMON/POWC/NPOWCOM,POWCOMNO(MPOWCOM),POWCOMID(MPOWCOM),
     &POWCOMPHTYP(MPOWCOM),POWCONEN(MPOWCOM,MPHAS),
     &NPOWCDAT(MPOWCOM),NPOWCDATS(MPOWCOM),POWCDAT(MPOWCOM,MPWDAT)


C Node and component names
      COMMON/ECNAMES/ENODNAM(MENOD),HYCOMNAM(MHYCOM),
     &HYCOMTYPSTR(MHYCOM),POWCOMNAM(MPOWCOM),POWCDATS(MPOWCOM,MPCDS),
     &CONECOMNAM(MCONECOM)

C The descriptive strings
      COMMON/ECDESC/ENTYPESTR(MENTYP),PHTYPSTR(MPHATY),ENDESC,
     &ENODTYPSTR(MENOD),HYDESC(MHYCOM),CONECOMDESC(MCONECOM),
     &POWCOMDESC(MPOWCOM)

C Common holds the id numbers of the nodes connected to the power conditioning
C unit (a power-only component model)
      common/pcu/pcunode(mpowcom,3)    
      integer pcunode         !- array holds the nodes to which the pcu is connected
                             !- pcunode(i,1) is the node id of the sending node
                             !- pcunode(i,2) is the node id of the receiving node
                             !- i is the power-only component index 
      integer PCU_id         !- id number of the PCU power-only component model
      parameter( PCU_id = 20 )



      DIMENSION SSTR(90),VAR(MPWDAT),SPMISC(MPWDAT),POWITEM(MPWDAT),
     &POWITEM2(MPWDAT),POWDESC(MPOWCOM),DADESC(MPWDAT),
     &POSTR(90), IVALS(5), TMPDESC(3)

C Type casting (all varaiables and arrays cast explicitly)
      INTEGER NPOWCOM,POWCOMNO,POWCOMID,POWCOMPHTYP,POWCONEN,NPOWCDAT,
     &NPOWCDATS,IEPOW,NIT,NPOWITEM2,IMOUT,IMOUT2,NENOD,ENODPHTYP,ENODPH,
     &ENODTYP,ENODNO

C Characters
      CHARACTER*3 MODE
      CHARACTER*12 ENTYPESTR,ENODTYPSTR,ENODNAM,HYCOMTYPSTR,HYCOMNAM,
     &POWCOMNAM,CONECOMNAM,PHTYPSTR,SHSTR
      CHARACTER*16 PODEFNAM
      CHARACTER*32 POWITEM,POWITEM2
      CHARACTER*72 ENDESC,HYDESC,POWCOMDESC,POWCDATS,CONECOMDESC,
     &H,SSTR,POWDESC,DADESC,POSTR,DATSTR,TMPDESC,LNGSTR
      CHARACTER*124 OUTS

      REAL POWCDAT,SPMISC,VAR,ENODBASEV,ENODBASEP 

      LOGICAL POREPT,PHCPT,OK,dok

      POREPT=.FALSE.

C Firstly set the affected connection
      IF(MODE.EQ.'ADD'.OR.MODE.EQ.'EDT') THEN

C Set counter for a new power component.
        IF(MODE.EQ.'ADD') THEN
          IEPOW=NPOWCOM+1
        ELSE
          dok=.false.
          h(1)='Chance to continue without editing the component.'
          CALL ASKOK('Edit this component?',' ',OK,dok,1)
          IF(.NOT.OK) RETURN

C Set the number of the editing component to be edited and allow
C user to define component information (uses same code as new comp
C definition). 
          IEPOW=IPOW
        ENDIF

C Set the component number.
        POWCOMNO(IEPOW)=IEPOW

C Display a list of power only classes to add, and select the class
  5     CALL MCDBSCAN(2,1,SSTR,VAR,SPMISC,NIT,IER)

        CALL USRMSG('Select component type:',' ','-')
        IF(IER.GT.0) THEN
           CALL EDISP(IUOUT,'Error reading component database')
           RETURN
        ENDIF
        DO 7 I=1,NIT
          WRITE(POWITEM(I),'(I2,1X,A)') I,SSTR(I)(1:28)
  7     CONTINUE
        POWITEM(NIT+1)=' * User defined          '
        POWITEM(NIT+2)='-------------------------'
        POWITEM(NIT+3)=' ? help'
        POWITEM(NIT+4)=' - exit this menu'
        NPOWITEM=NIT+4
        IMOUT=-1

C Help text for this menu.
  9     H(1)='The menu options relate to different categories'
        H(2)='of power-only components which may be selected '
        H(3)='for connection to the electrical network       '
        H(4)='. Select a category'

        CALL EMENU('Available Components',POWITEM,NPOWITEM,IMOUT)
        CALL USRMSG(' ',' ','-')
        IF(IMOUT.GT.0.AND.IMOUT.LE.NIT) THEN 
        
C Store the number of the selected power-only components category      
          NIT=IMOUT
        ELSEIF(IMOUT.EQ.NPOWITEM-1) then
          CALL PHELPD('Power-only comp.',4,'-',0,0,IER)
          GOTO 9
        ELSEIF(IMOUT.EQ.NPOWITEM-3) THEN
          GOTO 99
        ELSEIF(IMOUT.EQ.NPOWITEM) THEN
          RETURN
        ELSE
          IMOUT=-1
          GOTO  9
        ENDIF

C Scan the database and display a menu of the types which fall into the category
C Variable NIT is overloaded and returns the number of menu items. 
        CALL USRMSG('Select a component:',' ','-')
 11     CALL MCDBSCAN(2,2,SSTR,VAR,SPMISC,NIT,IER)
        DO 13 J=1,NIT
          POSTR(J)=SSTR(J)
          WRITE(POWITEM2(J),'(I2,1X,A)')J,POSTR(J)(1:28)
 13     CONTINUE
        IF(IER.GT.0) GOTO 5
 15     IF(NIT.GT.0) THEN
          POWITEM2(NIT+1)='------------------------'
          IF(POREPT) THEN
            POWITEM2(NIT+2)='>> additional info ON   '
          ELSE
            POWITEM2(NIT+2)='>> additional info OFF  '
          ENDIF
          POWITEM2(NIT+3)='? help'
          POWITEM2(NIT+4)='- exit this menu '
          NPOWITEM2=NIT+4
      
C Help text for this menu.
 17       H(1)='Select a power-only component from the list. Each one'
          H(2)='shown is associated with a set of pre-defined '
          H(3)='parameters, these can be altered if required. '
          H(4)='  '
          H(5)='  '
          H(6)='Switching the "additional info" toggle to ON lets the'
          H(7)='user view any more information associted with the   '
          H(8)='component; this appears in the text window. '

          CALL EMENU('Components in category',POWITEM2,
     &    NPOWITEM2,IMOUT2)
          CALL USRMSG(' ',' ','-')
        
          IF(IMOUT2.EQ.NPOWITEM2) THEN
            GOTO 9
          ELSEIF(IMOUT2.EQ.NPOWITEM2-1) THEN
            CALL PHELPD('Special material',8,'-',0,0,IER)
            GOTO 17
          ELSEIF(IMOUT2.EQ.NPOWITEM2-2) THEN
            IF(POREPT) THEN
              POREPT=.FALSE.
            ELSE
              POREPT=.TRUE.
            ENDIF
            GOTO 15
          ELSEIF(IMOUT2.GE.1.AND.IMOUT2.LE.NIT) THEN

C Store the name of the selected string and get the data.
            POWDESC(1)=POWITEM2(IMOUT2)(4:LNBLNK(POWITEM2(IMOUT2)))
            PODEFNAM=POWDESC(1)(1:LNBLNK(POWDESC(1)))

C Recover the power only components data and descriptive strings. Firstly
C Blank the descriptive strings
            SSTR(1)=POWDESC(1)
            CALL MCDBSCAN(2,3,SSTR,VAR,SPMISC,NIT,IER)
            IF(IER.GT.0) GOTO 11
            POWCOMID(IEPOW)=INT(SPMISC(2))
            NPOWCDAT(IEPOW)=INT(SPMISC(3))
            NPOWCDATS(IEPOW)=INT(SPMISC(4))

C Recover the component data.
            DO 25 IOPDT=1,NPOWCDAT(IEPOW) 
              POWCDAT(IEPOW,IOPDT)=VAR(IOPDT)
              DADESC(IOPDT)=SSTR(IOPDT)
  25        CONTINUE

            DO 27 IOPDT=2,(NPOWCDATS(IEPOW)*2),2 
              IOPDTI=IOPDT/2
              IF(POWCDATS(IEPOW,IOPDTI)(1:3).EQ.'UNK')THEN
                POWCDATS(IEPOW,IOPDTI)=SSTR(NPOWCDAT(IEPOW)+IOPDT-1)
              ENDIF
              DADESC(NPOWCDAT(IEPOW)+IOPDTI)=
     &SSTR(NPOWCDAT(IEPOW)+IOPDT)
  27        CONTINUE

            IF(POREPT) THEN
              POWDESC(1)=PODEFNAM
              SSTR(1)=POWDESC(1)
              CALL MCDBSCAN(2,4,SSTR,VAR,SPMISC,NIT,IER)
              IF(IER.GT.0) GOTO 11 
            ENDIF
          ELSE
            IMOUT2=-1
            GOTO 11
          ENDIF
C Set up the menu
        ELSE
          CALL EDISP(IUOUT,'No components found in this category')
          GOTO 5
        ENDIF

C Enter a name for the component. 
        SHSTR=POWCOMNAM(IEPOW)
 99     CALL EASKS(SHSTR,' ',' name for component ?',12,
     &  PODEFNAM(1:12),' pow nam ', IER,0)
        POWCOMNAM(IEPOW)=SHSTR

C Enter a description for the component. 
        LNGSTR=POWCOMDESC(IEPOW)
        CALL EASKS(LNGSTR,'You can take this oportunity to ',
     &'enter a description of this component ?',72,'none',' pow desc ',
     &IER,0)
        POWCOMDESC(IEPOW)=LNGSTR

C Get data items for the component.
        H(1)='Enter the required data for the power only component. '
        H(2)='Either accept the default values or enter your own'
        H(3)='data.                                             '
        CALL PHELPD('Power comp data',3,'-',0,0,IER)
        CALL EASKI(NPOWCDAT(IEPOW),' ',' Number of data items ?',1,'-',
     &  MPWDAT,'-',1,'power only mat. data',IER,3)
        IF(IER.EQ.0) THEN
          DATSP=0.0        
          DO 30 IDAT=1,NPOWCDAT(IEPOW)
            IF(DADESC(IDAT)(1:4).eq.'    ') then
              WRITE(OUTS,'(A,A2)') ' Enter data item: ',IDAT
            ELSE
              WRITE(OUTS,'(A)') DADESC(IDAT)
            ENDIF
            DATSP=POWCDAT(IEPOW,IDAT)
            CALL EASKR(DATSP,OUTS,' ',1.,'-',99.,'-',1.0,'SPM data',
     &         IER,0)
            POWCDAT(IEPOW,IDAT)=DATSP
  30      CONTINUE

C Recover the string data  
        CALL EASKI(NPOWCDATS(IEPOW),' ','Number of string data items ?',
     &  1,'-',MPWDAT,'-',1,'power only mat. data',IER,3)
          DO 32 IDAT=2,NPOWCDATS(IEPOW)*2,2
            IDATI=INT(IDAT/2)
            IF(DADESC(NPOWCDAT(IEPOW)+IDATI)(1:4).EQ.'none'.OR.
     &DADESC(NPOWCDAT(IEPOW)+IDATI)(1:2).EQ.'  ') THEN
              WRITE(OUTS,'(A,A2)') ' Enter string data item: ',IDATI
            ELSE
              WRITE(OUTS,'(A)') DADESC(NPOWCDAT(IEPOW)+IDATI)
            ENDIF
            H(1)='Enter the required data string for the power only  '
            H(2)='component. Either accept the default values or give'
            H(3)='your own data.                                     '
            CALL PHELPD('Power comp data',3,'-',0,0,IER)
            DATSTR=POWCDATS(IEPOW,IDATI)
            CALL EASKS(DATSTR,OUTS,' ',72,'UNKNOWN',' pow str',IER,0)
            POWCDATS(IEPOW,IDATI)=DATSTR(1:LNBLNK(DATSTR))
  32      CONTINUE

C blank the temporary data description array
          DO 40 J=1,MPWDAT
             DADESC(J)='  '
  40      CONTINUE
        ELSE
          CALL EDISP(IUOUT,'Error in data input - aborting')
          RETURN
        ENDIF


        if ( powcomid(IEPOW) .ne. PCU_id ) then           !- power-only component is not a PCU

C Connect the component  to the network
C Ask for the component phase type and based on this determine which node(s)
C hybrid component connects. 
          H(1)='Please select the phase type of this component from'
          H(2)='the list.'
          IX=1
          CALL EPICKS(IX,IVALS,'Which phase type',
     &      'is this component?',12,5,PHTYPSTR,'phase type',IER,2)

          IF(IVALS(1).EQ.0) RETURN
          POWCOMPHTYP(IEPOW)=IVALS(1)

C Attach the component to a node in the network
          IF(POWCOMPHTYP(IEPOW).EQ.3) THEN
            NPICK=2
          ELSEIF(POWCOMPHTYP(IEPOW).EQ.4) THEN
            NPICK=3
          ELSE
            NPICK=1
          ENDIF

          TMPDESC(1)='Select FIRST node the component connects to: '
          TMPDESC(2)='Select SECOND node the component connects to: '
          TMPDESC(3)='Select THIRD node the component connects to: '

C Zero the existing locations
          POWCONEN(IEPOW,1)=0
          POWCONEN(IEPOW,2)=0
          POWCONEN(IEPOW,3)=0

C Loop to select attached nodes
          DO 50 I=1,NPICK

  55        CALL USRMSG(TMPDESC(I),' ','-')
            CALL ELNODLST('-',INOD)
            IF(INOD.LE.0.OR.INOD.GT.NENOD) RETURN

C Firstly check the phase types match d.c. can only be connected to d.c. Also
C if the component type is modelled as a balanced component (i.e. all phases
C are NOT modelled explicitly, then it must be connected to a balanced node.
C Otherwise all other connections are acceptable. 
            PHCPT=.TRUE.
            IF(ENODPHTYP(INOD).EQ.1.AND.POWCOMPHTYP(IEPOW).NE.1) THEN
              PHCPT=.FALSE.
            ELSEIF(ENODPHTYP(INOD).EQ.5.AND.POWCOMPHTYP(IEPOW).NE.5)THEN
              PHCPT=.FALSE.
            ELSE
              PHCPT=.TRUE.
            ENDIF
            IF(.NOT.PHCPT) THEN
              WRITE(OUTS,'(A)')
     &        'Incompatible phase types please select again'
              CALL EDISP(IUOUT,OUTS)
              GOTO 55
            ENDIF
          
Check the same node hasn't been picked twice
            IF(POWCONEN(IEPOW,ENODPH(INOD)).EQ.0) THEN
              POWCONEN(IEPOW,ENODPH(INOD))=INOD
            ELSE
              dok=.false.
              h(1)='Here is your chance to continue without over- '
              h(2)='writing the node already referencing this phase.'
              CALL ASKOK(
     &   'A node has already been picked which occupies this phase',
     &   'Overwrite it?',OK,dok,2)
              IF(.NOT.OK) GOTO 55
              POWCONEN(IEPOW,ENODPH(INOD))=INOD
              WRITE(OUTS,'(A)') 'Node has been overwritten ....'
              CALL EDISP(IUOUT,OUTS)
              IF(NPICK.NE.1) THEN 
                WRITE(OUTS,'(A)') 
     &'Remember, because of the over write, the number of connected '
                CALL EDISP(IUOUT,OUTS) 
                WRITE(OUTS,'(A)') 
     &'nodes has not increased, so select another node to connect to.' 
                CALL EDISP(IUOUT,OUTS)     
                GOTO 55
              ENDIF
            ENDIF

  50      CONTINUE

        else              !- power only component is a PCU; only single-phase type supported

C Zero the existing locations
          POWCONEN(IEPOW,1)=0
          POWCONEN(IEPOW,2)=0
          POWCONEN(IEPOW,3)=0

C Set phase type to d.c. - variable not used in power-only component model
          POWCOMPHTYP(IEPOW) = 1


!- ask user to select node that sends current to PCU
          CALL USRMSG('Select the node sending current to the PCU',
     &                ' ','-')
          CALL ELNODLST('-',INOD)                         !- display list of nodes

          IF(INOD.LE.0.OR.INOD.GT.NENOD) RETURN           !- no node selected

          pcunode(IEPOW,1) = INOD

          POWCONEN(IEPOW,1) = INOD         !- this is done to avoid errors in ENETASGN
                                           !- the pcu power-only component is linked to the
                                           !- sending node (phase 1)

!- ask user to select node that receives current from PCU
!- make sure that sending node and receiving node are not the same

          ok = .true.
          do while (ok)
            CALL USRMSG(
     &        'Select the node receiving current from the PCU',
     &        ' ','-')
            CALL ELNODLST('-',INOD)                         !- display list of nodes
            IF(INOD.LE.0.OR.INOD.GT.NENOD) RETURN           !- no node selected

            if ( inod .ne. pcunode(IEPOW,1) ) then
              pcunode(IEPOW,2) = INOD
              ok = .false.
            else
              dok=.true.
              h(1)='Please select different nodes for sending and'
              h(2)='receiving. Say no to get out of the question loop.'
              CALL ASKOK(
     &          'The PCU receiving node cannot be the same as the ',
     &          'sending node. Try again?',OK,DOK,2)
            endif
          enddo

        endif


        IF(IEPOW.EQ.NPOWCOM+1) NPOWCOM=NPOWCOM+1

      ELSEIF(MODE.EQ.'DEL') THEN
        IF (IPOW.GE.0) THEN
          IEPOW=IPOW
        ELSE
          RETURN
        ENDIF

        IF(IEPOW.LT.NPOWCOM)THEN
          DO 60 I=IEPOW,NPOWCOM-1
            POWCOMNO(I)=I
            POWCOMID(I)=POWCOMID(I+1) 
            POWCOMNAM(I)=POWCOMNAM(I+1) 
            POWCOMPHTYP(I)=POWCOMPHTYP(I+1)
            POWCOMDESC(I)=POWCOMDESC(I+1)
            DO 70 J=1,MPHAS
              POWCONEN(I,J)=POWCONEN(I+1,J)
 70         CONTINUE
            NPOWCDAT(I)=NPOWCDAT(I+1)
            DO 80 K=1,NPOWCDAT(I)
              POWCDAT(I,K)=POWCDAT(I+1,K)
 80         CONTINUE
            NPOWCDATS(I)=NPOWCDATS(I+1)
            DO 90 K=1,NPOWCDATS(I)
              POWCDATS(I,K)=POWCDATS(I+1,K)
 90         CONTINUE
            pcunode(I,1) = pcunode(I+1,1)
            pcunode(I,2) = pcunode(I+1,2)
 60       CONTINUE
        ENDIF

        NPOWCOM=NPOWCOM-1
      ELSEIF(MODE.EQ.'CPY') THEN
        IF (IPOW.GE.0) THEN
          IEPOWC=IPOW
        ELSE
          RETURN
        ENDIF

        IEPOW=NPOWCOM+1

        POWCOMNO(IEPOW)=IEPOW
        POWCOMID(IEPOW)=POWCOMID(IEPOWC) 
        POWCOMNAM(IEPOW)=POWCOMNAM(IEPOWC) 
        POWCOMPHTYP(IEPOW)=POWCOMPHTYP(IEPOWC)
        POWCOMDESC(IEPOW)=POWCOMDESC(IEPOWC)
        DO 100 J=1,MPHAS
          POWCONEN(IEPOW,J)=0
100     CONTINUE
        NPOWCDAT(IEPOW)=NPOWCDAT(IEPOWC)
        DO 110 K=1,NPOWCDAT(IEPOWC)
          POWCDAT(IEPOW,K)=POWCDAT(IEPOWC,K)
110     CONTINUE
        NPOWCDATS(IEPOW)=NPOWCDATS(IEPOWC)
        DO 120 K=1,NPOWCDATS(IEPOWC)
          POWCDATS(IEPOW,K)=POWCDATS(IEPOWC,K)
120     CONTINUE
        pcunode(IEPOW,1) = 0
        pcunode(IEPOW,2) = 0

        NPOWCOM=NPOWCOM+1
      CALL USRMSG('Remember to connect this component to the network.',
     &' ','-')
      ENDIF

      RETURN 
      END

C ******************************* EDECNC  ********************************************
C This routine edits the current electrical connecting components according to various 
C modes:
C 'ADD' - add a new connection.
C 'DEL' - delete the current connection.
C 'EDT' - edit the current connection.
C 'CPY' - copy a connection.

      SUBROUTINE EDECNC(MODE,ICNC)

#include "power.h"
      
      integer lnblnk  ! function definition

      COMMON/OUTIN/IUOUT,IUIN
      COMMON/POPHELP/H(60)

C The electrical network topology
      COMMON/ECONN/NECON,ECONO(MECON),CONPHTYP(MECON),
     &CONPH(MECON,MPHAS),CCNO(MECON),SENOD(MECON,MPHAS),
     &EENOD(MECON,MPHAS)


C The connecting components
      COMMON/CONCOM/NCONECOM,CONECOMNO(MCONECOM),CONECOMID(MCONECOM),
     &CONECOMPHTYP(MCONECOM),NCONECOMDAT(MCONECOM),
     &CONECOMDAT(MCONECOM,MCCDAT)

C Node and component names
      COMMON/ECNAMES/ENODNAM(MENOD),HYCOMNAM(MHYCOM),
     &HYCOMTYPSTR(MHYCOM),POWCOMNAM(MPOWCOM),POWCDATS(MPOWCOM,MPCDS),
     &CONECOMNAM(MCONECOM)

C The descriptive strings
      COMMON/ECDESC/ENTYPESTR(MENTYP),PHTYPSTR(MPHATY),ENDESC,
     &ENODTYPSTR(MENOD),HYDESC(MHYCOM),CONECOMDESC(MCONECOM),
     &POWCOMDESC(MPOWCOM)

      DIMENSION SSTR(90),VAR(MPWDAT),SPMISC(MPWDAT),CCITEM(MPWDAT),
     &CCITEM2(MPWDAT),CNCDESC(MPOWCOM),DADESC(MPWDAT),
     &CCSTR(90), IVALS(5)
     

C Type casting (all varaiables and arrays cast explicitly)
      INTEGER NCONECOM,CONECOMNO,CONECOMID,CONECOMPHTYP,NCONECOMDAT,
     &NECON,ECONO,CONPHTYP,CONPH,CCNO,SENOD,EENOD,IUOUT,IUIN
     
      REAL CONECOMDAT,SPMISC
     
C Characters
      CHARACTER*3 MODE
      CHARACTER*12 ENTYPESTR,ENODTYPSTR,ENODNAM,HYCOMTYPSTR,HYCOMNAM,
     &POWCOMNAM,CONECOMNAM,PHTYPSTR,SHSTR
      CHARACTER*16 CCDEFNAM
      CHARACTER*32 CCITEM,CCITEM2
      CHARACTER*72 SSTR,CNCDESC,DADESC,CCSTR,HYDESC,ENDESC,POWCDATS,
     &CONECOMDESC,POWCOMDESC,H,LNGSTR
      CHARACTER*124 OUTS

      LOGICAL OK,DOK,CCREPT
      CCREPT=.FALSE.

      IF(MODE.EQ.'ADD'.OR.MODE.EQ.'EDT') THEN

C Firstly set the affected connection number 
        IF(MODE.EQ.'ADD')THEN
          IECNC=NCONECOM+1
        ELSE
          dok=.false.
          h(1)='Chance to continue without editing the component.'
          CALL ASKOK('Edit this component?',' ',OK,dok,1)
          IF(.NOT.OK) RETURN
C Set the number of the editing component to be edited and allow
C user to define component information (uses same code as new comp
C definition). 
          IECNC=ICNC  
        ENDIF

C Set the connecting component number
        CONECOMNO(IECNC)=IECNC

C Display a list of connecting component classes to add, and select the class
  5     CALL MCDBSCAN(3,1,SSTR,VAR,SPMISC,NIT,IER)

        CALL USRMSG('Select component type:',' ','-')
        IF(IER.GT.0) THEN
           CALL EDISP(IUOUT,'Error reading component database')
           RETURN
        ENDIF
        DO 7 I=1,NIT
          WRITE(CCITEM(I),'(I2,1X,A)') I,SSTR(I)(1:28)
  7     CONTINUE
        CCITEM(NIT+1)=' * User defined          '
        CCITEM(NIT+2)='-------------------------'
        CCITEM(NIT+3)=' ? help'
        CCITEM(NIT+4)=' - exit this menu'
        NCCITEM=NIT+4
        IMOUT=-1

C Help text for this menu.
  9     H(1)='The menu options relate to different categories'
        H(2)='of connecting components which may be selected '
        H(3)='for connection to the electrical network       '
        H(4)='. Select a category'

        CALL EMENU('Available Components',CCITEM,NCCITEM,IMOUT)

        IF(IMOUT.GT.0.AND.IMOUT.LE.NIT) THEN 
        
C Store the number of the selected connecting components category      
          NIT=IMOUT
        ELSEIF(IMOUT.EQ.NCCITEM-1) THEN
          CALL PHELPD('Power-only comp.',4,'-',0,0,IER)
          GOTO 9
        ELSEIF(IMOUT.EQ.NCCITEM-3) THEN
          GOTO 99
        ELSEIF(IMOUT.EQ.NCCITEM) THEN
          RETURN
        ELSE
          IMOUT=-1
          GOTO  9
        ENDIF

C Scan the database and display a menu of the types which fall into the category
C Variable NIT is overloaded and returns the number of menu items. 
        CALL USRMSG('Select a component:',' ','-')
 11     CALL MCDBSCAN(3,2,SSTR,VAR,SPMISC,NIT,IER)
        DO 13 J=1,NIT
          CCSTR(J)=SSTR(J)
          WRITE(CCITEM2(J),'(I2,1X,A)')J,CCSTR(J)(1:28)
 13     CONTINUE
        IF(IER.GT.0) GOTO 5
 15     IF(NIT.GT.0) THEN
          CCITEM2(NIT+1)='------------------------'
          IF(CCREPT) THEN
            CCITEM2(NIT+2)='>> additional info ON   '
          ELSE
            CCITEM2(NIT+2)='>> additional info OFF  '
          ENDIF
          CCITEM2(NIT+3)='? help'
          CCITEM2(NIT+4)='- exit this menu '
          NCCITEM2=NIT+4
      
C Help text for this menu.
  17      H(1)='Select a connecting component from the list. Each one'
          H(2)='shown is associated with a set of pre-defined '
          H(3)='parameters, these can be altered if required. '
          H(4)='  '
          H(5)='  '
          H(6)='Switching the "additional info" toggle to ON lets the'
          H(7)='user view any more information associted with the   '
          H(8)='component; this appears in the text window. '

          CALL EMENU('Components in category',CCITEM2,NCCITEM2,IMOUT2)
        
          IF(IMOUT2.EQ.NCCITEM2) THEN
            GOTO 9
          ELSEIF(IMOUT2.EQ.NCCITEM2-1) THEN
            CALL PHELPD('Connecting component',8,'-',0,0,IER)
            GOTO 17
          ELSEIF(IMOUT2.EQ.NCCITEM2-2) THEN
            IF(CCREPT) THEN
              CCREPT=.FALSE.
            ELSE
              CCREPT=.TRUE.
            ENDIF
            GOTO 15
          ELSEIF(IMOUT2.GE.1.AND.IMOUT2.LE.NIT) THEN

C Store the name of the selected string and get the data.
            CNCDESC(1)=CCITEM2(IMOUT2)(4:LNBLNK(CCITEM2(IMOUT2)))
            CCDEFNAM=CNCDESC(1)(1:LNBLNK(CNCDESC(1)))
            SSTR(1)=CNCDESC(1)

C Recover the power only components data and descriptive strings. Firstly
C Blank the descriptive strings
            CALL MCDBSCAN(3,3,SSTR,VAR,SPMISC,NIT,IER)
            IF(IER.GT.0) GOTO 11
            CONECOMID(IECNC)=INT(SPMISC(2))
            NCONECOMDAT(IECNC)=INT(SPMISC(3))

C Recover the component data.
            DO 25 ICCDT=1,NCONECOMDAT(IECNC)
              CNCDESC(ICCDT)=SSTR(ICCDT)
              CONECOMDAT(IECNC,ICCDT)=VAR(ICCDT)
              DADESC(ICCDT)=CNCDESC(ICCDT)
  25        CONTINUE


            IF(CCREPT) THEN
              CNCDESC(1)=CCDEFNAM
              SSTR(1)=CNCDESC(1)
              CALL MCDBSCAN(3,4,SSTR,VAR,SPMISC,NIT,IER)
              IF(IER.GT.0) GOTO 11 
            ENDIF
          ELSE
            IMOUT2=-1
            GOTO 11
          ENDIF
C Set up the menu
        ELSE
          CALL EDISP(IUOUT,'No components found in this category')
          GOTO 5
        ENDIF

C Enter a name for the component. 
        SHSTR=CONECOMNAM(IECNC)
  99    CALL EASKS(SHSTR,' ',' name for component ?',12,
     &  CCDEFNAM(1:12),' cnc nam ', IER,0)
        CONECOMNAM(IECNC)=SHSTR

C Enter a description for the component. 
        LNGSTR=CONECOMDESC(IECNC)
        CALL EASKS(LNGSTR,'You can take this oportunity to',
     &'enter a description of this component ?',72,'none',' cnc desc ',
     &IER,0)
        CONECOMDESC(IECNC)=LNGSTR

C Get data items for the component.
        H(1)='Enter the required data for the power only component. '
        H(2)='Either accept the default values or enter your own'
        H(3)='data.                                             '
        CALL PHELPD('Power comp data',3,'-',0,0,IER)
        CALL EASKI(NCONECOMDAT(IECNC),' ',' Number of data items ?',1,
     &'-',MPWDAT,'-',1,'con comp data',IER,3)
        IF(IER.EQ.0) THEN
          DATSP=0.0        
          DO 30 IDAT=1,NCONECOMDAT(IECNC)
            IF(DADESC(IDAT)(1:4).eq.'    ') then
              WRITE(OUTS,'(A,A2)') ' Enter data item: ',IDAT
            ELSE
              WRITE(OUTS,'(A)') DADESC(IDAT)
            ENDIF
            DATSP=CONECOMDAT(IECNC,IDAT)
            CALL EASKR(DATSP,OUTS,' ',1.,'-',99.,'-',1.0,'cnc data',
     &         IER,0)
            CONECOMDAT(IECNC,IDAT)=DATSP
  30      CONTINUE

C blank the temporary data description array
          DO 40 J=1,MPWDAT
             DADESC(J)=' '
  40      CONTINUE
        ELSE
          CALL EDISP(IUOUT,'Error in data input - aborting')
          RETURN
        ENDIF

C Connect the component  to the network
C Ask for the component phase type and based on this determine which node(s)
C hybrid component connects. 
        H(1)='Please select the phase type of this component from'
        H(2)='the list.'
        IX=1
        CALL EPICKS(IX,IVALS,'Which phase type',
     &'is this component?',12,5,PHTYPSTR,'phase type',IER,2)

        IF(IVALS(1).EQ.0) RETURN
        CONECOMPHTYP(IECNC)=IVALS(1)

        IF(IECNC.EQ.NCONECOM+1) NCONECOM=NCONECOM+1

      ELSEIF(MODE.EQ.'DEL') THEN
        IF (ICNC.GE.0) THEN
          IECNC=ICNC
        ELSE
          RETURN
        ENDIF

C First, if any connections contain the deleted component then delete that 
C connection.  

C Ask if it is OK to delete associated connections, if not then return
        dok=.false.
        H(1)='If a conecting component is deleted, the connections '
        H(2)='containing this component must also be deleted.      '
        CALL ASKOK('Delete connections containing this component?',
     &    ' ',OK,dok,2)
        IF(.NOT.OK) RETURN
        DO 50 ICON=NECON,1,-1
          IF(CCNO(ICON).EQ.IECNC) THEN
            WRITE(OUTS,'(A,I3)')'Deleting connection ... ',ICON
            CALL EDISP(IUOUT,OUTS) 
            CALL EDECON('DEL',ICON)
          ENDIF
 50     CONTINUE

        IF(IECNC.LT.NCONECOM)THEN
          DO 60 I=IECNC,NCONECOM-1
            CONECOMNO(I)=I
            CONECOMID(I+1)=CONECOMID(I) 
            CONECOMNAM(I)=CONECOMNAM(I+1) 
            CONECOMPHTYP(I)=CONECOMPHTYP(I+1)
            NCONECOMDAT(I)=NCONECOMDAT(I+1)
            DO 80 K=1,NCONECOMDAT(I)
              CONECOMDAT(I,K)=CONECOMDAT(I+1,K)
 80         CONTINUE
 60       CONTINUE
        ENDIF

        NCONECOM=NCONECOM-1

      ELSEIF(MODE.EQ.'CPY') THEN
        IF (ICNC.GE.0) THEN
          IECNCC=ICNC
        ELSE
          RETURN
        ENDIF

        IECNC=NCONECOM+1

        CONECOMNO(IECNC)=IECNC
        CONECOMID(IECNC)=CONECOMID(IECNCC) 
        CONECOMNAM(IECNC)=CONECOMNAM(IECNCC) 
        CONECOMPHTYP(IECNC)=CONECOMPHTYP(IECNCC)
        CONECOMDESC(IECNC)=CONECOMDESC(IECNCC)
        NCONECOMDAT(IECNC)=NCONECOMDAT(IECNCC)
        DO 110 K=1,NCONECOMDAT(IECNCC)
          CONECOMDAT(IECNC,K)=CONECOMDAT(IECNCC,K)
110     CONTINUE

        NCONECOM=NCONECOM+1
      
      ENDIF

      RETURN 
      END

C ******************************* EDECON  ********************************************
C This routine edits the current electrical connections according to various modes:
C 'ADD' - add a new connection.
C 'DEL' - delete the current connection.
C 'EDT' - edit the current connection.
C 'CPY' - copy a connection.

      SUBROUTINE EDECON(MODE,ICON)

#include "power.h"
      
      integer lnblnk  ! function definition

      COMMON/OUTIN/IUOUT,IUIN
      COMMON/POPHELP/H(60)

C The electrical nodes
      COMMON/ENOD/NENOD,ENODNO(MENOD),ENODPHTYP(MENOD),
     &ENODPH(MENOD),ENODTYP(MENOD),ENODBASEV(MENOD),
     &ENODBASEP

C The electrical network topology
      COMMON/ECONN/NECON,ECONO(MECON),CONPHTYP(MECON),
     &CONPH(MECON,MPHAS),CCNO(MECON),SENOD(MECON,MPHAS),
     &EENOD(MECON,MPHAS)

C The connecting components
      COMMON/CONCOM/NCONECOM,CONECOMNO(MCONECOM),CONECOMID(MCONECOM),
     &CONECOMPHTYP(MCONECOM),NCONECOMDAT(MCONECOM),
     &CONECOMDAT(MCONECOM,MCCDAT)

C Node and component names
      COMMON/ECNAMES/ENODNAM(MENOD),HYCOMNAM(MHYCOM),
     &HYCOMTYPSTR(MHYCOM),POWCOMNAM(MPOWCOM),POWCDATS(MPOWCOM,MPCDS),
     &CONECOMNAM(MCONECOM)

      INTEGER NECON,ECONO,CONPHTYP,CONPH,CCNO,SENOD,EENOD,NENOD,ENODNO,
     &ENODPHTYP,ENODPH,ENODTYP,NCONECOM,CONECOMNO,CONECOMID,
     &CONECOMPHTYP,JLEN1,JLEN2

      REAL ENODBASEP,ENODBASEV,CONECOMDAT

      CHARACTER*3 MODE
      CHARACTER*12 STR1,STR2
      CHARACTER*12 ENODNAM,HYCOMTYPSTR,HYCOMNAM,POWCOMNAM,CONECOMNAM
      CHARACTER*72 POWCDATS,H
      CHARACTER*124 OUTS

      LOGICAL OK,dok
      
      IF(MODE.EQ.'ADD'.OR.MODE.EQ.'EDT') THEN

C Set the connection number.
        IF(MODE.EQ.'ADD') THEN
          IECON=NECON+1
        ELSE
          dok=.false.
          h(1)='Chance to continue without editing the connection.'
          CALL ASKOK('Do you want to edit this connection?',' ',
     &      OK,dok,1)
          IF(OK)THEN
            IECON=ICON
          ELSE
            RETURN
          ENDIF
        ENDIF

C Set the connection number
        ECONO(IECON)=IECON

C Select the nodes to connect also note if connection is multi-phase.
  10    CALL USRMSG('Select the START node:',' ','-')
        CALL ELNODLST('-',INOD1)        
        CALL USRMSG(' ',' ','-')

        CALL USRMSG('Select the END node:',' ','-')
        CALL ELNODLST('-',INOD2)
        CALL USRMSG(' ',' ','-')

        IF(INOD2.EQ.INOD1) THEN
          dok=.true.
          h(1)='This is a chance to correct this error.'
          CALL ASKOK('You cannot connect a node to itself.',
     &      'Try again?',OK,dok,1)
          IF(OK) THEN
            GOTO 10
          ELSE
            RETURN
          ENDIF
        ELSEIF(ENODPH(INOD2).NE.ENODPH(INOD1)) THEN

C Exception for DC nodes
          IF(ENODPHTYP(INOD2).EQ.1.OR.ENODPHTYP(INOD2).EQ.1)THEN
            CONTINUE
          ELSE
            dok=.true.
            h(1)='This is your chance to correct this error. '
            CALL ASKOK('You cannot connect nodes of different',
     &        'phases together. Try again?',OK,dok,1)
      
            IF(OK) THEN
              GOTO 10
            ELSE
              RETURN
            ENDIF
          ENDIF
        ENDIF

C Select the connecting component and check the component type is 
C suitable. 
   20   CALL USRMSG ('Select CONNECTING conponent:',' ','-')
        CALL ELCNCLST('-',ICNC)
        CALL USRMSG (' ',' ','-')

C Check the connector is suitable for the connected nodes. 
C First balanced type components to balanced type nodes. 
        IF((CONECOMPHTYP(ICNC).EQ.5.AND.ENODPHTYP(INOD1).NE.5).OR.
     &(CONECOMPHTYP(ICNC).NE.5.AND.ENODPHTYP(INOD1).EQ.5)) THEN
          dok=.true.
          h(1)='This is your chance to correct this error.'
          CALL ASKOK('You can only connect a BALANCED connector to',
     &      'BALANCED-type node or vice-versa. Try again?',OK,dok,1)
          IF(OK) THEN
            GOTO 20
          ELSE
            RETURN
          ENDIF   

C DC can only connect to DC
        ELSEIF(((CONECOMPHTYP(ICNC).EQ.1.AND.ENODPHTYP(INOD1).NE.1).OR.
     &(CONECOMPHTYP(ICNC).NE.1.AND.ENODPHTYP(INOD1).EQ.1)).AND.
     &((CONECOMPHTYP(ICNC).EQ.1.AND.ENODPHTYP(INOD2).NE.1).OR.
     &(CONECOMPHTYP(ICNC).NE.1.AND.ENODPHTYP(INOD2).EQ.1))) THEN 

C Exception for AC/DC of DC/AC conversion components
          IF(CONECOMID(ICNC).GE.30.AND.CONECOMID(ICNC).LT.40)THEN
            CONTINUE
          ELSE
            dok=.true.
            h(1)='This is your chance to correct this error. '
            CALL ASKOK('You can only connect a DC connector to',
     &        'DC-type nodes or vice-versa. Try again?',OK,dok,1)
            IF(OK) THEN
              GOTO 20
            ELSE
              RETURN
            ENDIF   
          ENDIF

        ELSEIF(CONECOMPHTYP(ICNC).EQ.3.AND.ENODPHTYP(INOD1).EQ.2)THEN
          dok=.true.
          h(1)='This is your chance to correct this error. '
          CALL ASKOK(
     &      'You cannot connect a two-phase connector to a single-',
     &      'phase node, use a single phase connector. Try again?',
     &      OK,dok,1)
          IF(OK) THEN
            GOTO 20
          ELSE
            RETURN
          ENDIF 
        ELSEIF(CONECOMPHTYP(ICNC).EQ.4.AND.(ENODPHTYP(INOD1).EQ.2.OR.
     &ENODPHTYP(INOD1).EQ.3))THEN
          dok=.true.
          h(1)='This is your chance to correct this error. '
          CALL ASKOK(
     & 'You cannot connect a three-phase connector to a single-phase',
     & 'or two-phase node. Use another connector. Try again?',OK,dok,1)
          IF(OK) THEN
            GOTO 20
          ELSE
            RETURN
          ENDIF
 
C All other connection types are acceptable.
        ENDIF           

C Fill in the other connection details. 
        SENOD(IECON,ENODPH(INOD1))=INOD1 
        EENOD(IECON,ENODPH(INOD2))=INOD2 
        CONPH(IECON,ENODPH(INOD1))=ENODPH(INOD1) 
        CCNO(IECON)=ICNC
        CONPHTYP(IECON)=CONECOMPHTYP(ICNC)

C Fill in additional connection details for a 2 or 3-phase connection, firstly find the
C other nodes which will make up the connection.
        STR1=ENODNAM(INOD1)
        STR2=ENODNAM(INOD2)
        JLEN1=LNBLNK(ENODNAM(INOD1))
        JLEN2=LNBLNK(ENODNAM(INOD2))
        

C Auto detect the other nodes in the connection if multi-phase.
        IF(CONECOMPHTYP(ICNC).EQ.ENODPHTYP(INOD1).AND.
     &(ENODPHTYP(INOD1).EQ.4.OR.ENODPHTYP(INOD1).EQ.3))THEN

          CALL EDISP(IUOUT,' ')
          CALL EDISP(IUOUT,'Multi-phase connection, detecting other')
          CALL EDISP(IUOUT,'connected nodes ... ')

C Find the other start nodes. 
          DO 30 INODES=1,NENOD
            IF(ENODPHTYP(INODES).EQ.4.OR.ENODPHTYP(INODES).EQ.3)THEN
              IF(LNBLNK(ENODNAM(INODES)).EQ.JLEN1)THEN
                IF(ENODNAM(INODES)(1:JLEN1-2).EQ.STR1(1:JLEN1-2)) THEN
                  IF(ENODNAM(INODES)(1:JLEN1).NE.STR1(1:JLEN1)) THEN
                    SENOD(IECON,ENODPH(INODES))=INODES
                    CONPH(IECON,ENODPH(INODES))=ENODPH(INODES)
                    CALL EDISP(IUOUT,' ')
                    WRITE(OUTS,'(A,I3)') 'Connecting node ',INODES
                    CALL EDISP(IUOUT,OUTS)

C Find the complementary end node.             
                    DO 40 INODEF=1,NENOD
                      IF(ENODPHTYP(INODEF).EQ.4.OR.
     &                ENODPHTYP(INODEF).EQ.3) THEN
                        IF(LNBLNK(ENODNAM(INODEF)).EQ.JLEN2)THEN
                          IF(ENODNAM(INODEF)(1:JLEN2-2).EQ.
     &                    STR2(1:JLEN2-2))THEN
                            IF(ENODPH(INODEF).EQ.ENODPH(INODES)) THEN 
                              EENOD(IECON,ENODPH(INODEF))=INODEF
                              WRITE(OUTS,'(A,I3)') 'to ',INODEF
                              CALL EDISP(IUOUT,OUTS) 
                            ENDIF                            
                          ENDIF
                        ENDIF
                      ENDIF
 40                 CONTINUE 
                  ENDIF
                ENDIF
              ENDIF
            ENDIF
 30       CONTINUE

C Case of a two-phase component in a three phase nodal group               
        ELSEIF(CONECOMPHTYP(ICNC).EQ.3.AND.ENODPHTYP(INOD1).EQ.4) THEN

C Ask the user the other phase the component links
          H(1)='As you are connecting a two phase component to a ' 
          H(2)='three phase set, the program needs to know which'
          H(3)='phase combination the component connects. For   '
          H(4)='example 1 and 2, 2 and 3 or 1 and 3.            '
          H(5)='please pick the other phase.                    '
 50       CALL EASKABCD('Which other phase of the three-phases does',
     &'the two phase connector link to','one','two','three','continue',
     &IPH,5)
          IF(IPH.EQ.4) THEN
            RETURN
          ELSEIF(IPH.EQ.ENODPH(INOD1)) THEN
            dok=.true.
            h(1)='This is your chance to correct this error.'
            CALL ASKOK('This phase is already filled!','Try again',
     &        OK,dok,1)
            IF(OK) THEN
              GOTO 50
            ELSE
              RETURN
            ENDIF
          ENDIF

C Find the other start node. 
          DO 60 INODES=1,NENOD
            IF(ENODPHTYP(INODES).EQ.4) THEN
              IF(ENODPH(INODES).EQ.IPH) THEN
                IF(LNBLNK(ENODNAM(INODES)).EQ.JLEN1)THEN
                  IF(ENODNAM(INODES)(1:JLEN1-2).EQ.STR1(1:JLEN1-2)) THEN
                    IF(ENODNAM(INODES)(1:JLEN1).NE.STR1(1:JLEN1)) THEN
                      SENOD(IECON,ENODPH(INODES))=INODES
                      WRITE(OUTS,'(A,I3)') 'Connecting node ',INODES
                      CONPH(IECON,ENODPH(INODES))=ENODPH(INODES)

C Find the complementary end node.             
                      DO 70 INODEF=1,NENOD
                        IF(ENODPHTYP(INODEF).EQ.4) THEN
                          IF(LNBLNK(ENODNAM(INODEF)).EQ.JLEN2)THEN
                            IF(ENODNAM(INODEF)(1:JLEN2-2).EQ.
     &                        STR2(1:JLEN2-2))THEN
                              IF(ENODPH(INODEF).EQ.ENODPH(INODES)) THEN
                                EENOD(IECON,ENODPH(INODEF))=INODEF
                                WRITE(OUTS,'(A,I3)') 'to ',INODEF 
                              ENDIF 
                            ENDIF
                          ENDIF
                        ENDIF
 70                   CONTINUE 
                    ENDIF
                  ENDIF
                ENDIF
              ENDIF
            ENDIF
 60       CONTINUE
        ENDIF

C Increase the number of connctions if not editing. 
        IF(IECON.EQ.NECON+1) NECON=NECON+1
          
      ELSEIF(MODE.EQ.'DEL') THEN
        IF (IECON.GE.0) THEN
          IECON=ICON
        ELSE
          RETURN
        ENDIF
        IF(IECON.LT.NECON) THEN
          DO 80 I=IECON,NECON-1
            ECONO(I)=I
            CONPHTYP(I)=CONPHTYP(I+1)
            CCNO(I)=CCNO(I+1)
            DO 90 J=1,MPHAS
              SENOD(I,J)=SENOD(I+1,J)
              EENOD(I,J)=EENOD(I+1,J)
              CONPH(I,J)=CONPH(I+1,J)
  90        CONTINUE            
  80      CONTINUE  
        ENDIF
        NECON=NECON-1        
      ELSEIF(MODE.EQ.'CPY') THEN
        CALL EDISP(IUOUT,' ')
        CALL EDISP(IUOUT,'The copying of connections is not available,')
        CALL EDISP(IUOUT,'Create a new connection instead. ')
        CALL EDISP(IUOUT,' ')
      ENDIF

      RETURN 
      END



C ******************************* ENETBASEV  *******************************************
C This routine scans the network and assigns appropriate base values to each node so
C that their per-unit voltage is equal to or close to to 1.0:
C nominal voltage/fixed voltage = 1.0. 
C The routine then scans through connected nodes; If a node j is connected via a
C transformer then the base voltage on the other side of the connection changes:
C basevoltage(j)=basevoltage(i)*t.f. voltage ratio. The t.f. voltage ratio is the
C (nominal high voltage)/(nominal low voltage) or (nominal low voltage)/
C (nominal high voltage) depending on whether node j is the high voltage or 
C low voltage side of the transformer respectively. 
C <<later additions will include votage rules for a.c./d.c. conversion. 
C If two nodes are connected by any other type of component then the base
C voltage values on either side of the node are the same. 

      SUBROUTINE ENETBASEV

#include "power.h"
#include "building.h"
      
      integer lnblnk  ! function definition

C Commons
      COMMON/POPHELP/H(60)

C Trace
      COMMON/TRACE/ITCF,ITRACE(MTRACE),IZNTRC(MCOM),ITU

C The electrical nodes
      COMMON/ENOD/NENOD,ENODNO(MENOD),ENODPHTYP(MENOD),
     &ENODPH(MENOD),ENODTYP(MENOD),ENODBASEV(MENOD),
     &ENODBASEP

C The connecting components
      COMMON/CONCOM/NCONECOM,CONECOMNO(MCONECOM),CONECOMID(MCONECOM),
     &CONECOMPHTYP(MCONECOM),NCONECOMDAT(MCONECOM),
     &CONECOMDAT(MCONECOM,MCCDAT)     
     
C Node and component names
      COMMON/ECNAMES/ENODNAM(MENOD),HYCOMNAM(MHYCOM),
     &HYCOMTYPSTR(MHYCOM),POWCOMNAM(MPOWCOM),POWCDATS(MPOWCOM,MPCDS),
     &CONECOMNAM(MCONECOM)

C The electrical network topology
      COMMON/ECONN/NECON,ECONO(MECON),CONPHTYP(MECON),
     &CONPH(MECON,MPHAS),CCNO(MECON),SENOD(MECON,MPHAS),
     &EENOD(MECON,MPHAS)

C Type casting (all varaiables and arrays cast explicitly)
      INTEGER NENOD,ENODNO,ENODPHTYP,ENODPH,ENODTYP,NCONECOM,
     &CONPH,CONECOMNO,CONECOMID,CONECOMPHTYP,NCONECOMDAT,NECON,
     &ECONO,CONPHTYP,CCNO,SENOD,EENOD

      DIMENSION ISET(MPHAS),TFOK(MECON),BVSET(MENOD)

      INTEGER IENOD,ICON,IST,INOD,NSET
     
      REAL ENODBASEV,ENODBASEP,CONECOMDAT

C Characters      
      CHARACTER*12 ENODNAM,HYCOMTYPSTR,HYCOMNAM,POWCOMNAM,CONECOMNAM
      CHARACTER*72 H,POWCDATS

      LOGICAL MOVE,TFOK,BVSET,NEW_START,OK,dok

C Loop through each node and set the local flag BVSET to .false., this
C changes to .true. when the base voltage is set. 

      DO 5 IENOD=1,NENOD
        BVSET(IENOD)=.FALSE. 
        NSET=0
  5   CONTINUE

C Ask the user for the root node no the network.
      H(1)='In order to set the network base voltage values of the    '
      H(2)='network you must define at least one voltage in the       '
      H(3)='network; usually one which attaches to the grid and/or has'
      H(4)='a fixed voltage. The other base voltage values will then  '
      H(5)='be automatically calculated .                             '                   
      CALL PHELPD('Set base value',5,'-',0,0,IER)

C Present the user with a list of nodes. 
      CALL USRMSG('Select a node to act as reference :',' ','P')      
  3   CALL ELNODLST('-',IENODPK)

      IF(IENODPK.LT.1.OR.IENODPK.GT.NENOD) THEN 
        GOTO 3
      ELSE
        CONTINUE
      ENDIF

C Check the node type and issue a warning if it is not fixed. 
      IF(ENODTYP(IENODPK).NE.2) THEN
        dok=.true.
        h(1)='This is your chance to correct this error. '
        CALL ASKOK('Warning: this is NOT a node with a fixed ',
     &    'voltage. Select another node?',OK,dok,1)
        IF(OK) GOTO 3
      ENDIF

C Ask the user to set the base voltage value. 
      CALL EASKR(ENODBASEV(IENODPK),'Base voltage (V) for ',
     &'this node ? ',0.,'F',9999.,'W',240.,'Base volt',IER,5)
      BVSET(IENODPK)=.TRUE.
      NSET=1
      ISET(NSET)=IENODPK

C Loop through the network and assign nodes with the same name (i.e. 
C if the node is two/three phase) the same base voltage value. 
      DO 10 IENOD=1,NENOD
        LSTR=LNBLNK(ENODNAM(IENOD))-2
        IF(ENODNAM(IENOD)(1:LSTR).EQ.
     &ENODNAM(IENODPK)(1:LSTR)) THEN
          IF(IENOD.NE.IENODPK) THEN
            ENODBASEV(IENOD)=ENODBASEV(IENODPK)
            BVSET(IENOD)=.TRUE. 
            NSET=NSET+1
            ISET(NSET)=IENOD
          ENDIF
        ENDIF
  10  CONTINUE

C Loop through each connection and identify those with a transformer.
      DO 15 ICON=1,NECON
        ICMPTYP=CONECOMID(CCNO(ICON))
        IF(ICMPTYP.GE.10.AND.ICMPTYP.LT.21) TFOK(ICON)=.TRUE.
  15  CONTINUE

C Loop through connections and phases and find connected nodes. 
      DO 20 IST=1,NSET

C Search through the network starting at the picked node and assign
C base voltage values. 
        ICURNT=ISET(IST)

 30     MOVE=.FALSE.

        DO 40 ICON=1,NECON

C Skip if the current point has moved else check for a connecting node that
C doesn't have a connecting base value.
          IF(MOVE) THEN
            CONTINUE
          ELSE
            IF(EENOD(ICON,IST).EQ.ICURNT) THEN
              IF(.NOT.BVSET(SENOD(ICON,IST))) THEN
                 MOVE=.TRUE.
                 IF(TFOK(ICON)) THEN
                   ENODBASEV(SENOD(ICON,IST))=
     &ENODBASEV(ICURNT)*CONECOMDAT(CCNO(ICON),1)                
                 ELSE
                   ENODBASEV(SENOD(ICON,IST))=ENODBASEV(ICURNT)
                 ENDIF
                 BVSET(SENOD(ICON,IST))=.TRUE.
                 ICURNT=SENOD(ICON,IST)
              ENDIF
            ELSEIF(SENOD(ICON,IST).EQ.ICURNT) THEN
              IF(.NOT.BVSET(EENOD(ICON,IST))) THEN
                 MOVE=.TRUE.
                 IF(TFOK(ICON)) THEN
                   ENODBASEV(EENOD(ICON,IST))=
     &ENODBASEV(ICURNT)/CONECOMDAT(CCNO(ICON),1) 
                 ELSE
                   ENODBASEV(EENOD(ICON,IST))=ENODBASEV(ICURNT)
                 ENDIF
                 BVSET(EENOD(ICON,IST))=.TRUE.
                 ICURNT=EENOD(ICON,IST)
              ENDIF
            ENDIF
          ENDIF
  40    CONTINUE

        IF(MOVE) GOTO 30

C Loop through all the nodes with a base value to find one that has a connected node with anC unset base voltage.
        NEW_START=.FALSE.
        DO 50 INOD=1,NENOD 
          IF(BVSET(INOD)) THEN
            IF(.NOT.NEW_START) THEN 
              DO 60 ICON=1,NECON
                IF(SENOD(ICON,IST).EQ.INOD) THEN
                  IF(.NOT.BVSET(EENOD(ICON,IST))) THEN
                    ICURNT=SENOD(ICON,IST)
                    NEW_START=.TRUE.
                  ENDIF
                ELSEIF(EENOD(ICON,IST).EQ.INOD) THEN
                  IF(.NOT.BVSET(SENOD(ICON,IST))) THEN
                    ICURNT=EENOD(ICON,IST)
                    NEW_START=.TRUE.
                  ENDIF
                ENDIF
 60           CONTINUE
            ENDIF
          ENDIF
 50     CONTINUE 

        IF(NEW_START) GOTO 30

 20   CONTINUE
          
C Should be finished if no new_start is found. Redisplay the node menu 
C with the newly calulated values.
      CALL USRMSG('Calculated base voltages:',' ','P')

      CALL ELNODLST('-',IENODPK)
                  
C Trace 
      IF(ITRACE(2).GT.0) THEN
        DO 70 INOD=1,NENOD
          WRITE(ITU,*) 'Node :',INOD,' Base voltage : ',ENODBASEV(INOD)
 70     CONTINUE
      ENDIF

      RETURN
      END

C ******************************* ENETCHECK  *******************************************
C Scan the network through to check that the network topology is OK. Check that all
C nodes with different base voltage values are connected by a transformer and 
C that it is of the correct base voltage value. 

      SUBROUTINE ENETCHECK
#include "power.h"

      COMMON/OUTIN/IUOUT,IUIN


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

C Node and component names
      COMMON/ECNAMES/ENODNAM(MENOD),HYCOMNAM(MHYCOM),
     &HYCOMTYPSTR(MHYCOM),POWCOMNAM(MPOWCOM),POWCDATS(MPOWCOM,MPCDS),
     &CONECOMNAM(MCONECOM)

C The electrical network topology
      COMMON/ECONN/NECON,ECONO(MECON),CONPHTYP(MECON),
     &CONPH(MECON,MPHAS),CCNO(MECON),SENOD(MECON,MPHAS),
     &EENOD(MECON,MPHAS)
     

C Type casting (all varaiables and arrays cast explicitly)
      INTEGER NENOD,ENODNO,ENODPHTYP,ENODPH,ENODTYP,NHYBCOM,HYBCOMNO,
     &HYCOMTYP,HYCOMPHTYP,HYCONEN,HYLOC,NHYBDAT,NPOWCOM,POWCOMNO,
     &POWCOMID,POWCOMPHTYP,POWCONEN,NPOWCDAT,NPOWCDATS,NECON,
     &ECONO,CONPH,CONPHTYP,CCNO,SENOD,EENOD

      DIMENSION DBLECON(MECON),DBLEHYB(MHYCOM)

      INTEGER IUOUT,IUIN

      REAL ENODBASEV,ENODBASEP,HYBDAT,POWCDAT
     
C Characters      
      CHARACTER*12 ENODNAM,HYCOMTYPSTR,HYCOMNAM,POWCOMNAM,CONECOMNAM
      CHARACTER*72 POWCDATS
      CHARACTER*124 OUTS

      LOGICAL INODCON,DBLECON,DBLEHYB


      CALL EDISP(IUOUT,'  ')
      CALL EDISP(IUOUT,'Scanning electrical network for errors...') 

C Check for unconnected nodes.       
      DO 10 INOD=1,NENOD
        INODCON=.FALSE.
        DO 20 ICON=1,NECON
          DO 30 IPHAS=1,MPHAS
            IF(SENOD(ICON,IPHAS).EQ.INOD) INODCON=.TRUE.
            IF(EENOD(ICON,IPHAS).EQ.INOD) INODCON=.TRUE.
  30      CONTINUE
  20    CONTINUE
        IF(.NOT.INODCON) THEN
          WRITE(OUTS,'(A,I3,1X,A,A)')
     &    'Error: Node ',INOD,ENODNAM(INOD),
     &    'is not connected to the network.'
          CALL EDISP(IUOUT,OUTS)
        ENDIF
  10  CONTINUE

C Check double connections.
        DO 35 ICON=1,MECON
          DBLECON(ICON)=.FALSE.
 35     CONTINUE

        DO 40 ICONC=1,NECON
          DO 50 ICONS=1,NECON
            IF(.NOT.DBLECON(ICONC)) THEN
              DO 60 IPHAS=1,MPHAS
                IF((ICONC.NE.ICONS).AND.SENOD(ICONC,IPHAS).GT.0) THEN
                  IF((SENOD(ICONC,IPHAS).EQ.SENOD(ICONS,IPHAS)).AND.
     &(EENOD(ICONC,IPHAS).EQ.EENOD(ICONS,IPHAS))) THEN
                    DBLECON(ICONC)=.TRUE.
                    DBLECON(ICONS)=.TRUE.
                  ELSEIF((EENOD(ICONC,IPHAS).EQ.SENOD(ICONS,IPHAS)).AND.
     &(SENOD(ICONC,IPHAS).EQ.EENOD(ICONS,IPHAS))) THEN
                    DBLECON(ICONC)=.TRUE.
                    DBLECON(ICONS)=.TRUE.
                  ENDIF
                  IF(DBLECON(ICONC)) THEN
                     WRITE(OUTS,'(A,I3,A,I3,A,I3,A,I3,A,I3)')
     &              'Error: Duplicate connection between nodes ',
     &SENOD(ICONC,IPHAS),' and ',EENOD(ICONC,IPHAS), ' in connections ', 
     &ICONC,' and ',ICONS, ' in phase ',IPHAS
                    CALL EDISP(IUOUT,OUTS)
                  ENDIF
                ENDIF
  60          CONTINUE 
            ENDIF           
  50      CONTINUE
  40    CONTINUE

C Check for unconnected or badly connected boundary components.
        DO 65 IHYB=1,NHYBCOM
           DBLEHYB(IHYB)=.FALSE.
  65    CONTINUE

        DO 70 IHYB=1,NHYBCOM
          INONZ=0
          DO 80 IPHAS=1,MPHAS
            IF(HYCONEN(IHYB,IPHAS).GT.0) THEN
              INONZ=INONZ+1
            ENDIF
  80      CONTINUE
          IF(INONZ.EQ.0) THEN  
            WRITE(OUTS,'(A,I3,1X,A,A)')
     &      'Warning: Hybrid component  ',
     &IHYB,HYCOMNAM(IHYB),' is not connected to the network. '  
            CALL EDISP(IUOUT,OUTS)
           ELSEIF(INONZ.EQ.1.AND.
     &(HYCOMPHTYP(IHYB).EQ.3.OR.HYCOMPHTYP(IHYB).EQ.4))THEN
            WRITE(OUTS,'(A,I3,1X,A)')
     &      'Warning: Hybrid component  ',
     &IHYB,HYCOMNAM(IHYB)
            CALL EDISP(IUOUT,OUTS)  
            WRITE(OUTS,'(A)')
     &'is multi-phase but has only one phase connected to the network.'  
            CALL EDISP(IUOUT,OUTS)  
           ELSEIF(INONZ.EQ.2.AND.
     &HYCOMPHTYP(IHYB).EQ.4) THEN
            WRITE(OUTS,'(A,I3,1X,A)')
     &      'Warning: Hybrid component  ',
     &IHYB,HYCOMNAM(IHYB)
            CALL EDISP(IUOUT,OUTS)
            WRITE(OUTS,'(A)')
     &'is 3-phase but has only two phases connected to the network.'  
            CALL EDISP(IUOUT,OUTS)                 
          ENDIF

C Check for the same hybrid component attached to the network twice. 
          DO 85 JHYB=1,NHYBCOM
             IF(.NOT.DBLEHYB(IHYB).AND.(JHYB.NE.IHYB))THEN
               IF(HYLOC(IHYB,1).EQ.HYLOC(JHYB,1))THEN
                 IF(HYLOC(IHYB,2).EQ.HYLOC(JHYB,2))THEN
                   IF(HYLOC(IHYB,3).EQ.HYLOC(JHYB,3))THEN
                     IF(HYCOMTYP(IHYB).EQ.HYCOMTYP(JHYB))THEN
                       DBLEHYB(IHYB)=.TRUE.
                       DBLEHYB(JHYB)=.TRUE.
                       WRITE(OUTS,'(A,I3,1X,A)')
     &              'Warning: The hybrid component  ',
     &IHYB,HYCOMNAM(IHYB)
                       CALL EDISP(IUOUT,OUTS)
                       WRITE(OUTS,'(A,I3,1X,A)')
     &'is identical to component',IHYB,HYCOMNAM(JHYB)
                       CALL EDISP(IUOUT,OUTS)
                     ENDIF
                   ENDIF
                 ENDIF
               ENDIF
             ENDIF
  85      CONTINUE
  70    CONTINUE

        DO 90 IPWC=1,NPOWCOM
          INONZ=0
          DO 100 IPHAS=1,MPHAS
            IF(POWCONEN(IPWC,IPHAS).NE.0) THEN
              INONZ=INONZ+1
            ENDIF
  100      CONTINUE
          IF(INONZ.EQ.0) THEN  
            WRITE(OUTS,'(A,I3,1X,A,A)')
     &      'Warning: Power only component  ',
     &IPWC,POWCOMNAM(IPWC),' is not connected to the network. '  
            CALL EDISP(IUOUT,OUTS)
           ELSEIF(INONZ.EQ.1.AND.
     &(POWCOMPHTYP(IPWC).EQ.3.OR.POWCOMPHTYP(IPWC).EQ.4)) THEN
            WRITE(OUTS,'(A,I3,1X,A,A)')
     &      'Warning: Power only component  ',
     &IPWC,POWCOMNAM(IPWC),
     &' is multi-phase but has only one phase connected to the network.'  
            CALL EDISP(IUOUT,OUTS)  
           ELSEIF(INONZ.EQ.2.AND.
     &POWCOMPHTYP(IPWC).EQ.4) THEN
            WRITE(OUTS,'(A,I3,1X,A,A)')
     &      'Warning: Power only component  ',
     &IPWC,POWCOMNAM(IPWC),
     &' is 3-phase but has only two phases connected to the network.'  
            CALL EDISP(IUOUT,OUTS)                 
          ENDIF
  90    CONTINUE


      CALL EDISP(IUOUT,'Scan finished.') 
C Later add checks for components and transformers. 
      RETURN
      END
