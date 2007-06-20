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

C enetmisc.F contains housekeeping routines for the electrical network that
C are used by numerous esp-r modules.
C ENETINIT - This routine initialises/clears all relevant variables and strings 
C            in the electrical network.
C PHDECODE - This routine takes in a string OUTS containing a phase type and returns a 
C            phase value or vice versa.
C
C ELNODMLS - This routine presents a list of electrical nodes and allows one or more to
C            be selected.
C ELHYBMLS - This routine presents a list of hybrid components and allows one or more to
C            be selected.
C ELPOWMLS - This routine presents a list of power-only components, allowing one or more to
C            be selected.
C ELCNCMLS - This routine presents a list of connecting components, allowing one or more to
C            be selected.
C ELCONMLS - This routine presents a list of electrical connections, allowing one or more C C            to be selected.
C All the variables used in these routines are described in the file enetprj.F

C ************************** ENETINIT *********************************
C This routine initialises/clears all relevant variables and strings in the electrical
C network. For a listing of variable and string definitions see the ENETREWR 
C subroutine. 
      SUBROUTINE ENETINIT

#include "power.h"
C
C Common Blocks

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

C Type casting (all varaiables and arrays cast explicitly)
      INTEGER NENOD,ENODNO,ENODPHTYP,ENODPH,ENODTYP,NHYBCOM,HYBCOMNO,
     &HYCOMTYP,HYCOMPHTYP,HYCONEN,HYLOC,NHYBDAT,NPOWCOM,POWCOMNO,
     &POWCOMID,POWCOMPHTYP,POWCONEN,NPOWCDAT,NPOWCDATS,NCONECOM,
     &CONPH,CONECOMNO,CONECOMID,CONECOMPHTYP,NCONECOMDAT,NECON,
     &ECONO,CONPHTYP,CCNO,SENOD,EENOD

      INTEGER IENTXIST,ENTYPE,I,J
     
      REAL PHASEANG,ENODBASEV,ENODBASEP,HYBDAT,POWCDAT,CONECOMDAT

C Characters
      CHARACTER*12 ENTYPESTR,ENODTYPSTR,ENODNAM,HYCOMTYPSTR,HYCOMNAM,
     &POWCOMNAM,CONECOMNAM,PHTYPSTR
      CHARACTER*72 ENDESC,HYDESC,POWCOMDESC,POWCDATS,CONECOMDESC,
     &ENTFLNAM


C Set initial network variables
      IENTXIST=1
      IF(ENTFLNAM(1:2).EQ.'  ') ENTFLNAM='UNKNOWN'
      ENDESC='none'
      ENTYPE=5

      PHASEANG(1)=0.
      PHASEANG(2)=120.
      PHASEANG(3)=240.      
      
C Assign the default base power (1kVA)
      ENODBASEP=1000.

C Fill the network type string array
      ENTYPESTR(1)='d.c.' 
      ENTYPESTR(2)='1-phase' 
      ENTYPESTR(3)='multi-phase'
      ENTYPESTR(4)='balanced' 
      ENTYPESTR(5)='mixed' 

C Fill the phase type string array
      PHTYPSTR(1)='d.c.' 
      PHTYPSTR(2)='1-phase' 
      PHTYPSTR(3)='2-phase' 
      PHTYPSTR(4)='3-phase' 
      PHTYPSTR(5)='balanced'


C Clear all nodal data
      DO 10 I=1,MENOD
        NENOD=0
        ENODNAM(I)='UNKNOWN'
        ENODNO(I)=I
        ENODPHTYP(I)=2
        ENODPH(I)=1 
        ENODTYP(I)=1 
        ENODTYPSTR(I)='variable'
        ENODBASEV(I)=220.
  10  CONTINUE


C Clear all hybrid components data
      DO 20 I=1,MHYCOM
        NHYBCOM=0
        HYCOMNAM(I)='UNKNOWN'
        HYDESC(I)='none'
        HYBCOMNO(I)=I
        HYCOMTYP(I)=0
        HYCOMPHTYP(I)=2
        DO 23 J=1,MPHAS
          HYCONEN(I,J)=0
          HYLOC(I,J)=0
  23    CONTINUE
        NHYBDAT(I)=0
        DO 25 J=1,MHYDAT
          HYBDAT(I,J)=0.0
  25    CONTINUE
  20  CONTINUE

C Clear all power only components data
      DO 30 I=1,MPOWCOM
        NPOWCOM=0
        POWCOMNAM(I)='UNKNOWN'
        POWCOMDESC(I)='none'
        POWCOMNO(I)=I
        POWCOMID(I)=0
        POWCOMPHTYP(I)=2
        NPOWCDAT(I)=0
        NPOWCDATS(I)=0
        DO 33 J=2,MPCDS,2
          POWCDATS(I,J)='none'
          POWCDATS(I,J-1)='UNKNOWN'
  33    CONTINUE
        DO 35 J=1,MPWDAT
          POWCDAT(I,J)=0.0 
  35    CONTINUE        
  30  CONTINUE

C Clear all connecting component data
      DO 40 I=1,MCONECOM
        NCONECOM=0
        CONECOMNAM(I)='UNKNOWN'
        CONECOMDESC(I)='none'
        CONECOMNO(I)=0
        CONECOMID(I)=0 
        CONECOMPHTYP(I)=1
        NCONECOMDAT(I)=0
        DO 43 J=1,MCCDAT
          CONECOMDAT(I,J)=0.0 
  43    CONTINUE          
  40  CONTINUE

C Clear all connections data
      DO 50 I=1,MECON
        NECON=0
        ECONO(I)=0
        CONPHTYP(I)=2
        CCNO(I)=0
        DO 53 J=1,MPHAS
          CONPH(I,J)=0
          SENOD(I,J)=0 
          EENOD(I,J)=0
  53    CONTINUE  
  50  CONTINUE
        
      RETURN
      END
C ************************** PHDECODE *********************************
C If Mode =1 This routine takes in a string OUTS containing a phase type and returns a 
C phase value IPHV. Phase types: 1-d.c. 2-1-phase 3-2-phase 4-3-phase 5-balanced.
C
C If Mode =2 This routine takes in a  phase value IPHV and returns a 
C string OUTS. Phase types: 1-d.c. 2-1-phase 3-2-phase 4-3-phase
C 5-balanced

      SUBROUTINE PHDECODE(PHOUTS,IPHV,IMODE,IER)

      COMMON/OUTIN/IUOUT,IUIN

      CHARACTER PHOUTS*124
      
      INTEGER IPHV, IER, IMODE

C Set the error flag       
      IER=0

      IF(IMODE.EQ.1) THEN
C Decode the string passed to the variable
        IF(PHOUTS(1:LNBLNK(PHOUTS)).EQ.'d.c.') THEN
          IPHV=1  
        ELSEIF(PHOUTS(1:LNBLNK(PHOUTS)).EQ.'1-phase') THEN
          IPHV=2 
        ELSEIF(PHOUTS(1:LNBLNK(PHOUTS)).EQ.'2-phase') THEN
          IPHV=3
        ELSEIF(PHOUTS(1:LNBLNK(PHOUTS)).EQ.'3-phase') THEN
          IPHV=4
        ELSEIF(PHOUTS(1:LNBLNK(PHOUTS)).EQ.'balanced') THEN
          IPHV=5
        ELSE
          WRITE(PHOUTS,'(A,A)') 
     &    PHOUTS(1:LNBLNK(PHOUTS)),'is not a known phase type.'
          CALL EDISP(IUOUT,PHOUTS)
          IER=1
        ENDIF
      ELSEIF(IMODE.EQ.2) THEN
        IF(IPHV.EQ.1) THEN
          PHOUTS='d.c.'  
        ELSEIF(IPHV.EQ.2) THEN
          PHOUTS='1-phase' 
        ELSEIF(IPHV.EQ.3) THEN
          PHOUTS='2-phase'
        ELSEIF(IPHV.EQ.4) THEN
          PHOUTS='3-phase'
        ELSEIF(IPHV.EQ.5) THEN
          PHOUTS='balanced'
        ELSE
          WRITE(PHOUTS,'(A,A)') 
     &    IPHV,' is not a known phase number.'
          CALL EDISP(IUOUT,PHOUTS)
          IER=1
        ENDIF
      ELSE
          WRITE(PHOUTS,'(A,A)') 
     &    IMODE,' is not a known mode.'
          CALL EDISP(IUOUT,PHOUTS)
          IER=1
      ENDIF
      
    
      RETURN
      END
C ******************************* ELNODMLS  ********************************************
C Presents a list of electrical nodes to the user and returns the selected node number.

      SUBROUTINE ELNODMLS(NPK,IPK)

#include "epara.h"
#include "power.h"

C Commons
      COMMON/POPHELP/H(60)
      COMMON/OUTIN/IUOUT,IUIN
      COMMON/PMENU/MHEAD,MCTL,MIFULL,MFULL,IST,ILEN,IPM,MPM,IPFLG

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

C Type casting (all varaiables and arrays cast explicitly)
      INTEGER NENOD,ENODNO,ENODPHTYP,ENODPH,ENODTYP,MHEAD,ILEN

      DIMENSION VERT(35),IPK(MENOD)

      REAL ENODBASEV,ENODBASEP

C Characters
      CHARACTER*1 KEY
      CHARACTER*54 VERT
      CHARACTER*12 ENTYPESTR,ENODTYPSTR,ENODNAM,HYCOMTYPSTR,HYCOMNAM,
     &POWCOMNAM,CONECOMNAM,PHTYPSTR
      CHARACTER*72 ENDESC,HYDESC,POWCOMDESC,POWCDATS,CONECOMDESC,
     &H


C Zero picked items. 
      NPK=0
      DO 5 L=1,NENOD 
        IPK(L)=0
  5   CONTINUE  

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
          WRITE(VERT(M),14)KEY,ENODNAM(L)(1:12),
     &PHTYPSTR(ENODPHTYP(L))(1:12),ENODPH(L),ENODTYPSTR(L)(1:12),
     &ENODBASEV(L)
   14     FORMAT(A1,1X,A12,2X,A12,2X,I2,2X,A12,2X,F5.0)
        ENDIF
   10 CONTINUE

      VERT(1)='  Node name   | Phase Type   |No| Node type   |Vbase'
C Number of actual items displayed.
      MVERT=M+MCTL

C If a long list include page facility text.      
      IF(IPFLG.EQ.0)THEN  
        VERT(M+1)='  ________________________________  '
      ELSE
        WRITE(VERT(M+1),15)IPM,MPM 
   15   FORMAT   ('0 page: ',I2,' of ',I2,' ---------')
      ENDIF
      VERT(M+2)  =' '
      VERT(M+3)  =' '
      VERT(M+4)  ='? help                              '
      VERT(M+5)  ='- exit                              '

C Help text for menu.
  7   H(1)='The list shows the currently available nodes in the '
      H(2)='electrical network. Please select one from the list'
      H(3)='displayed.                                          '

C Now display the menu.
      CALL EMENU(' Electrical nodes',VERT,MVERT,IVERT)
      IF(IVERT.LE.MHEAD)THEN

C Within the header so skip request.
        IVERT=-1
        GOTO 3
      ELSEIF(IVERT.EQ.MVERT)THEN
        RETURN
      ELSEIF(IVERT.EQ.MVERT-1)THEN 
        CALL PHELPD('Select node',3,'-',0,0,IER) 
        GOTO 7
      ELSEIF(IVERT.EQ.(MVERT-4))THEN

C If there are enough items allow paging control via EKPAGE.
        IF(IPFLG.EQ.1)THEN
          IPACT=EDIT
          CALL EKPAGE(IPACT)
        ENDIF
        GOTO 3
      ELSEIF(IVERT.GT.MHEAD.AND.IVERT.LT.(MVERT-MCTL+1)) THEN

C Return the picked node
        CALL KEYIND(MVERT,IVERT,IFOC,IO)

C Check for duplicate picks
        DO 20 I=1,NPK
          IF(IPK(I).EQ.IFOC)THEN          
            CALL EDISP(IUOUT,'Already picked this item...')
            GOTO 7
          ENDIF
  20    CONTINUE
        NPK=NPK+1
        IPK(NPK)=IFOC
      ELSE
        GOTO 7
      ENDIF

      GOTO 3
      END

C ******************************* ELHYBMLS  ********************************************
C Presents a list of hybrid components to the user and returns the selected 
C component number.
      SUBROUTINE ELHYBMLS(NPK,IPK)
#include "epara.h"
#include "power.h"

C Commons
      COMMON/POPHELP/H(60)
      COMMON/OUTIN/IUOUT,IUIN
      COMMON/PMENU/MHEAD,MCTL,MIFULL,MFULL,IST,ILEN,IPM,MPM,IPFLG

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
     &NHYBDAT,MHEAD,ILEN

      REAL HYBDAT

      DIMENSION VERT(35),IPK(MHYCOM)

C Characters
      CHARACTER*1 KEY
      CHARACTER*66 VERT
      CHARACTER*12 ENTYPESTR,ENODTYPSTR,ENODNAM,HYCOMTYPSTR,HYCOMNAM,
     &POWCOMNAM,CONECOMNAM,PHTYPSTR
      CHARACTER*72 ENDESC,HYDESC,POWCOMDESC,POWCDATS,CONECOMDESC,
     &H


C Zero picked items. 
      NPK=0
      DO 5 L=1,NHYBCOM
        IPK(L)=0
  5   CONTINUE 

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
      VERT(M+2)  =' '
      VERT(M+3)  =' '
      VERT(M+4)  ='? help                              '
      VERT(M+5)  ='- exit                              '

C Help text for menu.
  7   H(1)='The list shows the available hybrid components '
      H(2)='in the electrical network. Please select one from the '
      H(3)='list displayed.                                       '

C Now display the menu.
      CALL EMENU(' Hybrid components ',VERT,MVERT,IVERT)
      CALL USRMSG('Select node(s) then exit',' ','-')
      IF(IVERT.LE.MHEAD)THEN

C Within the header so skip request.
        IVERT=-1
        GOTO 3
      ELSEIF(IVERT.EQ.MVERT)THEN
        CALL USRMSG(' ',' ','-')
        RETURN
      ELSEIF(IVERT.EQ.MVERT-1)THEN 
        CALL PHELPD('Select node',3,'-',0,0,IER)
        GOTO 7
      ELSEIF(IVERT.EQ.(MVERT-4))THEN

C If there are enough items allow paging control via EKPAGE.
        IF(IPFLG.EQ.1)THEN
          IPACT=EDIT
          CALL EKPAGE(IPACT)
        ENDIF
        GOTO 3
      ELSEIF(IVERT.GT.MHEAD.AND.IVERT.LT.(MVERT-MCTL+1)) THEN

C Return the picked component
        CALL KEYIND(MVERT,IVERT,IFOC,IO)

C Check for duplicate picks
        DO 20 I=1,NPK
          IF(IPK(I).EQ.IFOC)THEN          
            CALL EDISP(IUOUT,'Already picked this item...')
            GOTO 7
          ENDIF
 20     CONTINUE
        NPK=NPK+1
        IPK(NPK)=IFOC
      ELSE
        GOTO 7
      ENDIF

      GOTO 3
      END

C ******************************* ELPOWMLS  ********************************************
C Presents a list of hybrid components to the user and returns the selected 
C component number.
      SUBROUTINE ELPOWMLS(NPK,IPK)
#include "epara.h"
#include "power.h"

C Commons
      COMMON/POPHELP/H(60)
      COMMON/OUTIN/IUOUT,IUIN
      COMMON/PMENU/MHEAD,MCTL,MIFULL,MFULL,IST,ILEN,IPM,MPM,IPFLG

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
     &NPOWCDATS,MHEAD,ILEN

      REAL POWCDAT

      DIMENSION VERT(35),IPK(MPOWCOM)

C Characters
      CHARACTER*1 KEY
      CHARACTER*46 VERT
      CHARACTER*12 ENTYPESTR,ENODTYPSTR,ENODNAM,HYCOMTYPSTR,HYCOMNAM,
     &POWCOMNAM,CONECOMNAM,PHTYPSTR
      CHARACTER*72 ENDESC,HYDESC,POWCOMDESC,POWCDATS,CONECOMDESC,H

C Zero picked items. 
      NPK=0
      DO 5 L=1,NPOWCOM
        IPK(L)=0
  5   CONTINUE 

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
      VERT(M+2)  =' '
      VERT(M+3)  =' '
      VERT(M+4)  ='? help                              '
      VERT(M+5)  ='- exit                              '

C Help text for menu.
  7   H(1)='The list shows the available power only components    '
      H(2)='in the electrical network. Please select one from the '
      H(3)='list displayed.                                       '

C Now display the menu.
      CALL EMENU(' Power only components:',VERT,MVERT,IVERT)
      CALL USRMSG('Select component(s) then exit',' ','-')
      IF(IVERT.LE.MHEAD)THEN

C Within the header so skip request.
        IVERT=-1
        GOTO 3
      ELSEIF(IVERT.EQ.MVERT)THEN
        CALL USRMSG(' ',' ','-')
        RETURN
      ELSEIF(IVERT.EQ.MVERT-1)THEN
        CALL PHELPD('Select node',3,'-',0,0,IER)
        GOTO 7
      ELSEIF(IVERT.EQ.(MVERT-4))THEN

C If there are enough items allow paging control via EKPAGE.
        IF(IPFLG.EQ.1)THEN
          IPACT=EDIT
          CALL EKPAGE(IPACT)
        ENDIF
        GOTO 3
      ELSEIF(IVERT.GT.MHEAD.AND.IVERT.LT.(MVERT-MCTL+1)) THEN

C Return the picked component
        CALL KEYIND(MVERT,IVERT,IFOC,IO)

C Check for duplicate picks
        DO 20 I=1,NPK
          IF(IPK(I).EQ.IFOC)THEN          
            CALL EDISP(IUOUT,'Already picked this item...')
            GOTO 7
          ENDIF
 20     CONTINUE
        NPK=NPK+1
        IPK(NPK)=IFOC
      ELSE
        GOTO 7
      ENDIF

      GOTO 3
      END

C ******************************* ELCNCMLS  ********************************************
C Presents a list of connecting components to the user and returns the selected 
C component number.
      SUBROUTINE ELCNCMLS(NPK,IPK)
#include "epara.h"
#include "power.h"

C Commons
      COMMON/POPHELP/H(60)
      COMMON/OUTIN/IUOUT,IUIN
      COMMON/PMENU/MHEAD,MCTL,MIFULL,MFULL,IST,ILEN,IPM,MPM,IPFLG

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
      INTEGER NCONECOM,CONECOMNO,CONECOMID,CONECOMPHTYP,NCONECOMDAT,
     &MHEAD,ILEN

      REAL CONECOMDAT

      DIMENSION VERT(35),IPK(MCONECOM)

C Characters
      CHARACTER*1 KEY
      CHARACTER*34 VERT
      CHARACTER*12 ENTYPESTR,ENODTYPSTR,ENODNAM,HYCOMTYPSTR,HYCOMNAM,
     &POWCOMNAM,CONECOMNAM,PHTYPSTR
      CHARACTER*72 ENDESC,HYDESC,POWCOMDESC,POWCDATS,CONECOMDESC,
     &H

C Zero picked items. 
      NPK=0
      DO 5 L=1,NCONECOM
        IPK(L)=0
  5   CONTINUE 

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
      VERT(M+2)  =' '
      VERT(M+3)  =' '
      VERT(M+4)  ='? help                           '
      VERT(M+5)  ='- exit                           '

C Help text for menu.
  7   H(1)='The list shows the current connecting components '
      H(2)='in the electrical network. Please select one from the '
      H(3)='list displayed.                                       '

C Now display the menu.
      CALL EMENU(' Connecting components: ',VERT,MVERT,IVERT)
      CALL USRMSG('Select component(s) then exit',' ','-')
      IF(IVERT.LE.MHEAD)THEN

C Within the header so skip request.
        IVERT=-1
        GOTO 3
      ELSEIF(IVERT.EQ.MVERT)THEN
        CALL USRMSG(' ',' ','-')
        RETURN
      ELSEIF(IVERT.EQ.MVERT-1)THEN 
        CALL PHELPD('Select node',3,'-',0,0,IER)
        GOTO 7
      ELSEIF(IVERT.EQ.(MVERT-4))THEN

C If there are enough items allow paging control via EKPAGE.
        IF(IPFLG.EQ.1)THEN
          IPACT=EDIT
          CALL EKPAGE(IPACT)
        ENDIF
        GOTO 3
      ELSEIF(IVERT.GT.MHEAD.AND.IVERT.LT.(MVERT-MCTL+1)) THEN

C Return or edit the picked component
        CALL KEYIND(MVERT,IVERT,IFOC,IO)
C Check for duplicate picks
        DO 20 I=1,NPK
          IF(IPK(I).EQ.IFOC)THEN          
            CALL EDISP(IUOUT,'Already picked this item...')
            GOTO 7
          ENDIF
 20     CONTINUE
        NPK=NPK+1
        IPK(NPK)=IFOC
      ELSE
        GOTO 7
      ENDIF

      GOTO 3
      END

C ******************************* ELCONMLS  ********************************************
C Presents a list of electrical connections to the user and returns the selected 
C connection number.
      SUBROUTINE ELCONMLS(NPK,IPK)

#include "epara.h"
#include "power.h"

C Commons
      COMMON/POPHELP/H(60)
      COMMON/OUTIN/IUOUT,IUIN
      COMMON/PMENU/MHEAD,MCTL,MIFULL,MFULL,IST,ILEN,IPM,MPM,IPFLG

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
      INTEGER NECON,ECONO,CONPHTYP,CONPH,CCNO,SENOD,EENOD,
     &MHEAD,ILEN

      DIMENSION VERT(35),IPK(MECON)

C Characters
      CHARACTER*1 KEY
      CHARACTER*60 VERT
      CHARACTER*12 ENTYPESTR,ENODTYPSTR,ENODNAM,HYCOMTYPSTR,HYCOMNAM,
     &POWCOMNAM,CONECOMNAM,PHTYPSTR
      CHARACTER*72 ENDESC,HYDESC,POWCOMDESC,POWCDATS,CONECOMDESC,
     &H

C Zero picked items. 
      NPK=0
      DO 5 L=1,NECON 
        IPK(L)=0
  5   CONTINUE 

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
      VERT(M+2)  =' '
      VERT(M+3)  =' '
      VERT(M+4)  ='? help                              '
      VERT(M+5)  ='- exit                              '

C Help text for menu.
  7   H(1)='The list shows the currently defined connections      '
      H(2)='in the electrical network. Please select one from the '
      H(3)='list displayed.                                       '

C Now display the menu.
      CALL EMENU(' Connections: ',VERT,MVERT,IVERT)
      CALL USRMSG('Select connection(s) then exit',' ','-')
      IF(IVERT.LE.MHEAD)THEN

C Within the header so skip request.
        IVERT=-1
        GOTO 3

      ELSEIF(IVERT.EQ.MVERT)THEN
        CALL USRMSG(' ',' ','-')
        RETURN
      ELSEIF(IVERT.EQ.MVERT-1)THEN 
        CALL PHELPD('Select node',3,'-',0,0,IER)
        GOTO 7
      ELSEIF(IVERT.EQ.(MVERT-4))THEN

C If there are enough items allow paging control via EKPAGE.
        IF(IPFLG.EQ.1)THEN
          IPACT=EDIT
          CALL EKPAGE(IPACT)
        ENDIF
        GOTO 3
      ELSEIF(IVERT.GT.MHEAD.AND.IVERT.LT.(MVERT-MCTL+1)) THEN

C Return or edit the picked component
        CALL KEYIND(MVERT,IVERT,IFOC,IO)
C Check for duplicate picks
        DO 20 I=1,NPK
          IF(IPK(I).EQ.IFOC)THEN          
            CALL EDISP(IUOUT,'Already picked this item...')
            GOTO 7
          ENDIF
 20     CONTINUE
        NPK=NPK+1
        IPK(NPK)=IFOC
      ELSE
        GOTO 7
      ENDIF

      GOTO 3
      END
