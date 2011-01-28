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

C Contents:
C   ASKRNOD:  Presents a selection list of mass flow nodes with no calls
C             to editing routines. 
C   ASKRCMP:  Presents a selection list of mass flow components with no calls
C             to editing routines.
C   ASKRCON:  Presents a selection list of mass flow connection with no calls 
C             to editing routines.
                
C ********************** ASKRNOD
C ASKRNOD presents a list of mass flow nodes returning the index IS.
      SUBROUTINE ASKRNOD(head,MOD,IS,IER)

#include "net_flow.h"
#include "epara.h"
#include "espriou.h"
    
      COMMON/SPAD/MMOD,LIMIT,LIMTTY
      COMMON/MFLOW1/NNOD,NCMP,NCNN
      COMMON/MFLOW2/NDNAM(0:MNOD)
      COMMON/MFLOW3/NDFLD(MNOD),NDTYP(MNOD),HNOD(MNOD),SUPNOD(MNOD,MNDS)
      DIMENSION VERT(35),FTYPN(10)

      character*(*) head
      CHARACTER VERT*72,KEY*1,MOD*1,NDNAM*12,NODTYP*12,
     &FTYPN*6
      LOGICAL SELECT
      integer MVERT,IVERT  ! max items and current menu item

C For help messages
      character helpinsub*24 ! subroutine name
      character helptopic*24 ! string (unique) for topic
      integer nbhelp     ! number of help lines found (will be non-zero
                         ! because default message will be created
      helpinsub='ASKRNOD' ! set for subroutine

C Initialise node menu variables based on window size. 
C IVERT is the menu position, MVERT the current number 
C of menu lines based on selected list.
      IER=0
      SELECT=.FALSE.
      MHEAD=1
      MCTL=5
      ILEN=NNOD

C Initial menu entry setup.
      IPACT=CREATE
      CALL EKPAGE(IPACT)
   92 IER=0
      IVERT=-3
      ILEN=NNOD

C Set up fluid types (6 characters): up to 10 can be supported
      FTYPN(1)='air'
      FTYPN(2)='water'
      FTYPN(2)='steam'
C etc ...

C Loop through the items until the page to be displayed. M is the 
C current menu line index. Build up text strings for the menu. 
  3   M=MHEAD
      WRITE(VERT(1),7777)
7777  FORMAT('      Name    | Fluid |     Type        |',
     &' Height |  Data1  |  Data2')
      DO 10 L=1,ILEN                                    
        IF(L.GE.IST.AND.(L.LE.(IST+MIFULL)))THEN
          M=M+1
          CALL EMKEY(L,KEY,IER)

C Set up the node description string (12 chars) 
          if(NDTYP(L).eq.0)then
            nodtyp='internal    '
          elseif(NDTYP(L).eq.1)then
            nodtyp='intern fix P'
          elseif(NDTYP(L).eq.2)then
            nodtyp='bound fix P '
          elseif(NDTYP(L).eq.3)then
            nodtyp='bound wind P'
          endif
          WRITE(VERT(M),
     &'(A1,1X,A12,2X,A6,3X,A12,5X,F5.1,3X,F9.1,1X,F9.1)')
     &KEY,NDNAM(L),FTYPN(NDFLD(L)),NODTYP,HNOD(L),SUPNOD(L,1),
     &SUPNOD(L,2)
        ENDIF
   10 CONTINUE

C Number of actual items displayed.
      MVERT=M+MCTL

C If a long list include page facility text.      
      IF(IPFLG.EQ.0)THEN
        VERT(M+1)='  ________________ '
      ELSE
        WRITE(VERT(M+1),'(A,I2,A,I2)')'0 Page: ',IPM,' :',MPM 
      ENDIF

C If MOD has been passed as a M then add an item at the end.
      IF(MOD.EQ.'M'.OR.MOD.EQ.'m')THEN
        VERT(M+2)='+ add/delete/copy '
      ELSE
        VERT(M+2)='                   '
      ENDIF
      if(MMOD.EQ.8)then
        VERT(M+3)  ='                   '
      else
        VERT(M+3)  ='< Index select     '
      endif
      VERT(M+4)  ='? Help             '
      VERT(M+5)  ='- Exit             '

C Help text for this menu.
      helptopic='mfs_node_selection_menu'
      call gethelptext(helpinsub,helptopic,nbhelp)

C Display the menu.
      CALL USRMSG('Select a node:',' ','-')
      CALL EMENU(head,VERT,MVERT,IVERT)
      IF(IVERT.LE.MHEAD)THEN
        IVERT=-1
        goto 3
      ELSEIF(IVERT.EQ.MVERT)THEN

C If no selection has been made before exit then return with 0.
        IF(.NOT.SELECT)IS=0
        RETURN
      ELSEIF(IVERT.EQ.(MVERT-1))THEN
        helptopic='mfs_node_selection_menu'
        call gethelptext(helpinsub,helptopic,nbhelp)
        CALL PHELPD('node connection selection',nbhelp,'-',0,0,IER)
      ELSEIF(IVERT.EQ.(MVERT-2))THEN

C Script directed input if not in graphic mode.
        if(MMOD.EQ.8)goto 3
        IV=1
        helptopic='mfs_text_index_of_node'
        call gethelptext(helpinsub,helptopic,nbhelp)
  95    CALL EASKI(IV,' ',' Index of node ? ',
     &       1,'F',NNOD,'F',1,'script node',IER,nbhelp)
        if(IER.NE.0)goto 95
        IS=IV
        RETURN
      ELSEIF(IVERT.EQ.(MVERT-4))THEN

C If there are enough items allow paging control via EKPAGE.
        IF(IPFLG.EQ.1)THEN
          IPACT=EDIT
          CALL EKPAGE(IPACT)
        ENDIF
      ELSEIF(IVERT.GT.MHEAD.AND.IVERT.LT.(MVERT-MCTL+1))THEN
        CALL KEYIND(MVERT,IVERT,IFOC,IO)
        SELECT=.TRUE.
        IS=IFOC
        RETURN
      ELSE
        IVERT=-1
        goto 92
      ENDIF
      IVERT=-2
      goto 3

      END

C ********************** ASKRCMP
C ASKRCMP presents a list of mass flow components returning the index IS.
      SUBROUTINE ASKRCMP(head,MOD,IS,IER)

#include "net_flow.h"
#include "epara.h"
#include "espriou.h"
    
      COMMON/SPAD/MMOD,LIMIT,LIMTTY
      COMMON/MFLOW1/NNOD,NCMP,NCNN
      COMMON/MFLOW8/CMNAM(MCMP),LTPCMP(MCMP)
      COMMON/MFLOW9/ITPCMP(MCMP),ISDCMP(MCMP),ISDCNN(MCMP),
     &              SUPCMP(MCMP,MCMS)

      DIMENSION VERT(35)
      character*(*) head
      CHARACTER VERT*84,KEY*1,MOD*1
      CHARACTER LTPCMP*60,CMNAM*12
      LOGICAL SELECT
      integer MVERT,IVERT ! max items and current menu item

C For help messages
      character helpinsub*24 ! subroutine name
      character helptopic*24 ! string (unique) for topic
      integer nbhelp     ! number of help lines found (will be non-zero
                         ! because default message will be created
      helpinsub='ASKRCMP' ! set for subroutine

C Initialise node menu variables based on window size. 
C IVERT is the menu position, MVERT the current number 
C of menu lines based on selected list.
      IER=0
      SELECT=.FALSE.
      MHEAD=1
      MCTL=5
      ILEN=NCMP

C Initial menu entry setup.
      IPACT=CREATE
      CALL EKPAGE(IPACT)
   92 IER=0
      IVERT=-3
      ILEN=NCMP

C Loop through the items until the page to be displayed. M is the 
C current menu line index. Build up text strings for the menu. 
    3 M=MHEAD
      VERT(1)='      Name    |Type| Description ...'
      DO 10 L=1,ILEN
        IF(L.GE.IST.AND.(L.LE.(IST+MIFULL)))THEN
          M=M+1
          CALL EMKEY(L,KEY,IER)
          WRITE(VERT(M),'(A1,1X,A12,2X,I3,2X,A60)')KEY,CMNAM(L),
     &ITPCMP(L),LTPCMP(L)
        ENDIF
   10 CONTINUE

C Number of actual items displayed.
      MVERT=M+MCTL

C If a long list include page facility text.      
      IF(IPFLG.EQ.0)THEN
        VERT(M+1)='  ________________ '
      ELSE
        WRITE(VERT(M+1),'(A,I2,A,I2)')'0 Page: ',IPM,' :',MPM 
      ENDIF

C If MOD has been passed as a M then add an item at the end.
      IF(MOD.EQ.'M'.OR.MOD.EQ.'m')THEN
        VERT(M+2)='+ add/delete/copy  '
      ELSE
        VERT(M+2)='                   '
      ENDIF
      if(MMOD.EQ.8)then
        VERT(M+3)  ='                   '
      else
        VERT(M+3)  ='< Index select     '
      endif
      VERT(M+4)  ='? Help             '
      VERT(M+5)  ='- Exit             '

C Help text for this menu.
      helptopic='mfs_component_selection_menu'
      call gethelptext(helpinsub,helptopic,nbhelp)

C Display the menu.
      CALL EMENU(head,VERT,MVERT,IVERT)
      IF(IVERT.LE.MHEAD)THEN
        IVERT=-1
        goto 3
      ELSEIF(IVERT.EQ.MVERT)THEN

C If no selection has been made before exit then return with 0.
        IF(.NOT.SELECT)IS=0
        RETURN
      ELSEIF(IVERT.EQ.(MVERT-1))THEN

        helptopic='mfs_component_selection_menu'
        call gethelptext(helpinsub,helptopic,nbhelp)
        CALL PHELPD('node connection selection',nbhelp,'-',0,0,IER)

      ELSEIF(IVERT.EQ.(MVERT-2))THEN

C Script directed input if not in graphic mode.
        if(MMOD.EQ.8)goto 3
        IV=1
        helptopic='mfs_component_selection_text'
        call gethelptext(helpinsub,helptopic,nbhelp)
  96    CALL EASKI(IV,' ',' Index of component ? ',
     &         1,'F',NCMP,'F',1,'script comp',IER,nbhelp)
        if(IER.NE.0)goto 96
        IS=IV
        RETURN
      ELSEIF(IVERT.EQ.(MVERT-4))THEN

C If there are enough items allow paging control via EKPAGE.
        IF(IPFLG.EQ.1)THEN
          IPACT=EDIT
          CALL EKPAGE(IPACT)
        ENDIF
      ELSEIF(IVERT.GT.MHEAD.AND.IVERT.LT.(MVERT-MCTL+1))THEN
        CALL KEYIND(MVERT,IVERT,IFOC,IO)
        SELECT=.TRUE.
        IS=IFOC
        RETURN
      ELSE
        IVERT=-1
        goto 92
      ENDIF
      IVERT=-2
      goto 3

      END

C ********************** ASKRCON
C ASKRCON presents a list of mass flow connections returning the 
C Starting and ending node numbers.
      SUBROUTINE ASKRCON(head,MOD,INODPS,INODNE,IC,IER)

#include "net_flow.h"
#include "epara.h"
#include "espriou.h"
      COMMON/MFLOW1/NNOD,NCMP,NCNN
      COMMON/MFLOW2/NDNAM(0:MNOD)
      COMMON/MFLOW8/CMNAM(MCMP),LTPCMP(MCMP)
      COMMON/MFLW10/NODPS(MCNN),HGTPS(MCNN),NODNE(MCNN),HGTNE(MCNN),
     &              ITPCON(MCNN),NDSCNN(MCNN,MCNS)
      DIMENSION VERT(35)

      character*(*) head
      CHARACTER VERT*68,KEY*1,MOD*1
      CHARACTER LTPCMP*60,CMNAM*12,NDNAM*12
      LOGICAL SELECT
      integer MVERT,IVERT ! max items and current menu item

C For help messages
      character helpinsub*24 ! subroutine name
      character helptopic*24 ! string (unique) for topic
      integer nbhelp     ! number of help lines found (will be non-zero
                         ! because default message will be created
      helpinsub='ASKRCON'  ! set for subroutine

C Initialise node menu variables based on window size. 
C IVERT is the menu position, MVERT the current number 
C of menu lines based on selected list.
      IER=0
      SELECT=.FALSE.
      MHEAD=1
      MCTL=5
      ILEN=NCNN

C Initial menu entry setup.
      IPACT=CREATE
      CALL EKPAGE(IPACT)
   92 IER=0
      IVERT=-3
      ILEN=NCNN
C Loop through the items until the page to be displayed. M is the 
C current menu line index. Build up text strings for the menu. 
    3 M=MHEAD
      VERT(1)=
     &'    Node +ve  | dHght |to|    Node -ve  | dHght | via Component '
      DO 10 L=1,ILEN
        IF(L.GE.IST.AND.(L.LE.(IST+MIFULL)))THEN
          M=M+1
          CALL EMKEY(L,KEY,IER)
          WRITE(VERT(M),
     &'(A1,1X,A12,3X,F4.1,1X,A4,2X,A12,3X,F4.1,5X,A12)')KEY,
     &NDNAM(NODPS(L)),HGTPS(MCNN),'--> ',NDNAM(NODNE(L)),HGTNE(MCNN),
     &CMNAM(ITPCON(L))
        endif
   10 CONTINUE

C Number of actual items displayed.
      MVERT=M+MCTL

C If a long list include page facility text.      
      IF(IPFLG.EQ.0)THEN
        VERT(M+1)='  ________________ '
      ELSE
        WRITE(VERT(M+1),'(A,I2,A,I2)')'0 Page: ',IPM,' of ',MPM 
      ENDIF

C If MOD has been passed as a M then add an item at the end.
      IF(MOD.EQ.'M'.OR.MOD.EQ.'m')THEN
        VERT(M+2)='+ add/delete/copy  '
      ELSE
        VERT(M+2)='                   '
      ENDIF
      VERT(M+3)  ='< Index select     '
      VERT(M+4)  ='? Help             '
      VERT(M+5)  ='- Exit             '

C Help text for this menu.
      helptopic='mfs_connection_selection_menu'
      call gethelptext(helpinsub,helptopic,nbhelp)

C Display the menu.
      CALL EMENU(head,VERT,MVERT,IVERT)
      IF(IVERT.LE.MHEAD)THEN
        IVERT=-1
        goto 3
      ELSEIF(IVERT.EQ.MVERT)THEN

C If no selection has been made before exit then return with 0.
        IF(.NOT.SELECT)then
          INODPS=0
          INODNE=0
          IC=0
        endif
        RETURN
      ELSEIF(IVERT.EQ.(MVERT-1))THEN
        helptopic='mfs_connection_selection_menu'
        call gethelptext(helpinsub,helptopic,nbhelp)
        CALL PHELPD('connection selection',nbhelp,'-',0,0,IER)
      ELSEIF(IVERT.EQ.(MVERT-2))THEN

C Script directed input.
        IV=1
        helptopic='mfs_connection_selection_text'
        call gethelptext(helpinsub,helptopic,nbhelp)
  97    CALL EASKI(IV,' ',' Index of connection ? ',
     &       1,'F',NCNN,'F',1,'script conn',IER,nbhelp)
        if(IER.NE.0)goto 97
        IS=IV
        RETURN
      ELSEIF(IVERT.EQ.(MVERT-4))THEN

C If there are enough items allow paging control via EKPAGE.
        IF(IPFLG.EQ.1)THEN
          IPACT=EDIT
          CALL EKPAGE(IPACT)
        ENDIF
      ELSEIF(IVERT.GT.MHEAD.AND.IVERT.LT.(MVERT-MCTL+1))THEN

C Edit the selected connection.
        CALL KEYIND(MVERT,IVERT,IFOC,IO)
        SELECT=.TRUE.
        IC=IFOC       
          RETURN
      ELSE
        IVERT=-1
        goto 92
      ENDIF
      IVERT=-2
      goto 3

      END

