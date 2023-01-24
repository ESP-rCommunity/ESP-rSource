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


C Contains:
C     ASKNOD:  Presents a selection list of mass flow nodes.
C     ASKCMP:  Presents a selection list of mass flow components.
C     ASKCON:  Presents a selection list of mass flow connections. 

C ********************** ASKNOD
C ASKNOD presents a list of mass flow nodes returning the index IS.
C If MOD = 'M' then include option to add an item then the appropriate
C          interaction is begun.
      SUBROUTINE ASKNOD(head,MOD,IS,IER)
#include "building.h"
#include "net_flow.h"
#include "net_flow_data.h"
#include "epara.h"
#include "help.h"

C Parameters
      character*(*) head
      character MOD*1
      integer IS  ! index to return
      COMMON/AFN/IAIRN,LAPROB,ICAAS(MCOM)
      CHARACTER LAPROB*72      
      DIMENSION VERT(35),FTYPN(10)

      CHARACTER VERT*62,KEY*1,NODTYP*12,FTYPN*6
      LOGICAL SELECT
      integer MVERT,IVERT,IDVERT ! max items and current menu item

      helpinsub='felst'  ! set for subroutine

C Initialise node menu variables based on window size. 
C IVERT is the menu position, MVERT the current number 
C of menu lines based on selected list.
      IER=0
      SELECT=.FALSE.
      MHEAD=1
      MCTL=4
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
      FTYPN(3)='steam'
C etc ...

C Loop through the items until the page to be displayed. M is the 
C current menu line index. Build up text strings for the menu.
  3   M=MHEAD
      if(IAIRN.lt.3)then
        WRITE(VERT(1),'(2a)')
     &    '  Name        |Fluid |    Type    |',
     &    'Height |  Data1 |   Data2'
      else
        WRITE(VERT(1),'(2a)')
     &    '  Name        |Fluid |   Type    |',
     &    'Position      |Data1|Data2'
      endif
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
          if(IAIRN.lt.3)then
            WRITE(VERT(M),'(A,1X,A,1X,A,1X,A,F7.2,F9.1,F10.1)')
     &      KEY,NDNAM(L),FTYPN(NDFLD(L)),NODTYP,HNOD(L,3),SUPNOD(L,1),
     &      SUPNOD(L,2)
          else
            WRITE(VERT(M),'(A,1X,A,1X,A,1X,A,3F5.1,F5.1,F6.1)')
     &      KEY,NDNAM(L),FTYPN(NDFLD(L)),NODTYP,HNOD(L,1),HNOD(L,2),
     &      HNOD(L,3),SUPNOD(L,1),SUPNOD(L,2)
          endif
        ENDIF
   10 CONTINUE

C Number of actual items displayed.
      MVERT=M+MCTL

C If a long list include page facility text.      
      IF(IPFLG.EQ.0)THEN
        VERT(M+1)='  ________________________ '
      ELSE
        WRITE(VERT(M+1),'(A,I2,A,I2)')'0 Page: ',IPM,' :',MPM 
      ENDIF

C If MOD has been passed as a M then add an item at the end.
      IF(MOD.EQ.'M'.OR.MOD.EQ.'m')THEN
        VERT(M+2)='+ add/delete/copy node     '
      ELSE
        VERT(M+2)='                           '
      ENDIF
      VERT(M+3)  ='? Help                     '
      VERT(M+4)  ='- Exit                     '

C Help text for this menu.
      helptopic='mfs_node_selection'
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
        helptopic='mfs_node_selection'
        call gethelptext(helpinsub,helptopic,nbhelp)
        CALL PHELPD('node connection selection',nbhelp,'-',0,0,IER)
      ELSEIF(IVERT.EQ.(MVERT-2))THEN

C Allow a component to be added, checked for uniqueness and returned.
C Ability to delete node commented out until support code in place.
        IF(MOD.EQ.'M'.OR.MOD.EQ.'m')THEN

C Allow a node to be added deleted or copied.
          helptopic='mfs_node_management'
          call gethelptext(helpinsub,helptopic,nbhelp)
          CALL EASKABCD(' ','Options:','add','delete',
     &      'copy','cancel',IOPT,nbhelp)
          IF(IOPT.EQ.1)THEN
            IF(NNOD+1.LE.MNOD)THEN
              CALL MFNDES('A',99)        ! Add node.
              ILEN=NNOD; MHEAD=1; MCTL=4 ! Reset after return.
              IPACT=CREATE
              CALL EKPAGE(IPACT)
              IS=-1
              GOTO 92  ! instead of 3?
            ELSE
              call usrmsg(' ','Node list full!','W') 
              IS=0
              GOTO 3
            ENDIF
          ELSEIF(IOPT.EQ.2)THEN
            call usrmsg(' ','Delete which node?','-') 
            CALL EMENU(HEAD,VERT,MVERT,IDVERT)
            CALL KEYIND(MVERT,IDVERT,IFOC,IO)
            ID=IFOC
            call usrmsg(' ',' ','-')
            CALL MFNDES('D',ID)        ! Delete node.
            ILEN=NNOD; MHEAD=1; MCTL=4 ! Reset after return.
            IPACT=CREATE
            CALL EKPAGE(IPACT)
            GOTO 92
          ELSEIF(IOPT.EQ.3)THEN
            call usrmsg(' ','Copy which node?','-') 
            CALL EMENU(HEAD,VERT,MVERT,IDVERT)
            CALL KEYIND(MVERT,IDVERT,IFOC,IO)
            ID=IFOC
            call usrmsg(' ',' ','-')
            CALL MFNDES('C',ID)        ! Copy node.
            ILEN=NNOD; MHEAD=1; MCTL=4 ! Reset after return.
            IPACT=CREATE
            CALL EKPAGE(IPACT)
            GOTO 92
          ELSE
            GOTO 3
          ENDIF
        ELSE
          IVERT=-1
          goto 3
        ENDIF
      ELSEIF(IVERT.EQ.(MVERT-3))THEN

C If there are enough items allow paging control via EKPAGE.
        IF(IPFLG.EQ.1)THEN
          IPACT=EDIT
          CALL EKPAGE(IPACT)
        ENDIF
      ELSEIF(IVERT.GT.MHEAD.AND.IVERT.LT.(MVERT-MCTL+1))THEN
        CALL KEYIND(MVERT,IVERT,IFOC,IO)
        SELECT=.TRUE.
        IS=IFOC
        IF(MOD.EQ.'M'.OR.MOD.EQ.'m')THEN
          CALL MFNDES('E',IS)        ! Edit this node.
          ILEN=NNOD; MHEAD=1; MCTL=4 ! Reset after return.
          IPACT=CREATE
          CALL EKPAGE(IPACT)
          GOTO 92
        ELSE

C If not in edit mode then return selected node (IS) to the calling routine.       
          RETURN
        ENDIF
      ELSE
        IVERT=-1
        goto 92
      ENDIF
      IVERT=-2
      goto 3

      END

C ********************** ASKCMP
C ASKCMP presents a list of mass flow components returning the index IS.
C If MOD = 'M' then include option to add an item then the appropriate
C interaction is begun.
      SUBROUTINE ASKCMP(head,MOD,IS,IER)
#include "building.h"
#include "net_flow.h"
#include "net_flow_data.h"
#include "epara.h"
#include "help.h"

C Parameters passed.
      character*(*) head
      character MOD*1
      integer IS  ! index to return

      COMMON/AFN/IAIRN,LAPROB,ICAAS(MCOM)
      CHARACTER LAPROB*72      

      DIMENSION VERT(35)
      CHARACTER VERT*75,KEY*1
      LOGICAL SELECT
      integer MVERT,IVERT ! max items and current menu item

      helpinsub='mfelst'  ! set for subroutine

C Initialise node menu variables based on window size. 
C IVERT is the menu position, MVERT the current number 
C of menu lines based on selected list.
      IER=0
      SELECT=.FALSE.
      MHEAD=1
      MCTL=4
      ILEN=NCMP

C Initial menu entry setup.
      IPACT=CREATE
      CALL EKPAGE(IPACT)
   92 IER=0
      IVERT=-3
      ILEN=NCMP

C Loop through the items until the page to be displayed. M is the 
C current menu line index. Build up text strings for the menu. 
   3  M=MHEAD
      if(IAIRN.lt.3)then
        VERT(1)='  Name       |Type| Description ...'
      else
        VERT(1)='  Name       |Type| Position    Description ...'
      endif
      DO 10 L=1,ILEN
        IF(L.GE.IST.AND.(L.LE.(IST+MIFULL)))THEN
          M=M+1
          CALL EMKEY(L,KEY,IER)
          if(IAIRN.lt.3)then
            WRITE(VERT(M),'(A,1X,A,I4,2X,A)')KEY,CMNAM(L),
     &        ITPCMP(L),LTPCMP(L)(1:55)
          else
            WRITE(VERT(M),'(A,1X,A,I4,3F6.1,1x,A)')KEY,CMNAM(L),
     &        ITPCMP(L),HCMP(L,1,1),HCMP(L,1,2),HCMP(L,1,3),
     &        LTPCMP(L)(1:36)
          endif
        ENDIF
   10 CONTINUE

C Number of actual items displayed.
      MVERT=M+MCTL

C If a long list include page facility text.      
      IF(IPFLG.EQ.0)THEN
        VERT(M+1)='  _________________________ '
      ELSE
        WRITE(VERT(M+1),'(A,I2,A,I2)')'0 Page: ',IPM,' :',MPM 
      ENDIF

C If MOD has been passed as a M then add an item at the end.
      IF(MOD.EQ.'M'.OR.MOD.EQ.'m')THEN
        VERT(M+2)='+ add/delete/copy component '
      ELSE
        VERT(M+2)='                            '
      ENDIF
      VERT(M+3)  ='? Help                      '
      VERT(M+4)  ='- Exit                      '

C Help text for this menu.
      helptopic='mfs_component_selection'
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
        helptopic='mfs_component_selection'
        call gethelptext(helpinsub,helptopic,nbhelp)
        CALL PHELPD('node connection selection',nbhelp,'-',0,0,IER)
      ELSEIF(IVERT.EQ.(MVERT-2))THEN

C Allow a component to be added, checked for uniqueness and returned.
C Code to allow deletion commented out until support code in place.
        IF(MOD.EQ.'M'.OR.MOD.EQ.'m')THEN
C Allow a component to be added deleted or copied.
           helptopic='mfs_component_management'
           call gethelptext(helpinsub,helptopic,nbhelp)
           CALL EASKABCD(' ','Options:','add','delete',
     &'copy','cancel',IOPT,nbhelp)

C Add a component
           IF(IOPT.EQ.1)THEN
             IF(NCMP+1.LE.MCMP)THEN
               ID=99
               CALL EPMENSV
               CALL MFCDES('A',ID)
               CALL EPMENRC
               ILEN=NCMP
               IPACT=CREATE
               CALL EKPAGE(IPACT)
               IS=-1
               GOTO 3              
             ELSE
               call usrmsg(' ','Component list full!','W') 
               IS=0
               GOTO 3
             ENDIF

C Delete a component
           ELSEIF(IOPT.EQ.2)THEN

C Select component to delete
             call usrmsg(' ','Delete which component?','-') 
             CALL EMENU(HEAD,VERT,MVERT,IDVERT)
             CALL KEYIND(MVERT,IDVERT,IFOC,IO)
             ID=IFOC
             call usrmsg(' ',' ','-')
             CALL EPMENSV
             CALL MFCDES('D',ID)
             CALL EPMENRC
             ILEN=NCMP
             IPACT=CREATE
             CALL EKPAGE(IPACT)
             IS=-1
             GOTO 3 
           ELSEIF(IOPT.EQ.3)THEN

C Select component to copy
             call usrmsg(' ','Copy which component?','-') 
             CALL EMENU(HEAD,VERT,MVERT,IDVERT)
             CALL KEYIND(MVERT,IDVERT,IFOC,IO)
             ID=IFOC
             call usrmsg(' ',' ','-')
             CALL EPMENSV
             CALL MFCDES('C',ID)
             CALL EPMENRC
             ILEN=NCMP
             IPACT=CREATE
             CALL EKPAGE(IPACT)
             IS=-1
             GOTO 3 
           ELSE
             GOTO 3
           ENDIF
        ELSE
          IVERT=-1
          goto 3
        ENDIF
      ELSEIF(IVERT.EQ.(MVERT-3))THEN

C If there are enough items allow paging control via EKPAGE.
        IF(IPFLG.EQ.1)THEN
          IPACT=EDIT
          CALL EKPAGE(IPACT)
        ENDIF
      ELSEIF(IVERT.GT.MHEAD.AND.IVERT.LT.(MVERT-MCTL+1))THEN
        CALL KEYIND(MVERT,IVERT,IFOC,IO)
        SELECT=.TRUE.
        IS=IFOC
        IF(MOD.EQ.'M'.OR.MOD.EQ.'m')THEN

C Decode from the potential long list to the comp no via KEYIND.
          CALL EPMENSV
          CALL MFCDES('E',IS)
          CALL EPMENRC
          ILEN=NCMP
          IPACT=CREATE
          CALL EKPAGE(IPACT)
          GOTO 3
        ELSE
C If not in edit mode then return selected node (IS) to the calling routine.       
          RETURN
        ENDIF
      ELSE
        IVERT=-1
        goto 92
      ENDIF
      IVERT=-2
      goto 3

      END

C ********************** ASKCON
C ASKCON presents a list of mass flow connections returning the 
C Starting and ending node numbers.
C If MOD = 'M' then include option to add an item then the appropriate
C          interaction is begun.

      SUBROUTINE ASKCON(head,MOD,INODPS,INODNE,IC,IER)
#include "building.h"
#include "net_flow.h"
#include "net_flow_data.h"
#include "epara.h"
#include "help.h"
    
      DIMENSION VERT(35),ITEMDEL(MCNN),IVALS(MCNN)

      character*(*) head
      CHARACTER VERT*58,ITEMDEL*56,KEY*1,MOD*1
      LOGICAL SELECT
      CHARACTER PROMP2*56,PROMP1*56

      helpinsub='mfelst'  ! set for subroutine

C Initialise node menu variables based on window size. 
C IVERT is the menu position, MVERT the current number 
C of menu lines based on selected list.
      IER=0
      SELECT=.FALSE.
      MHEAD=1
      MCTL=4
      ILEN=NCNN

C Create list for copy and delete operations.
      DO L=1,NCNN
         WRITE(ITEMDEL(L),'(A12,F6.1,A5,A12,F6.1,2X,A12)')
     &    NDNAM(NODPS(L)),HGTPS(L),' --> ',NDNAM(NODNE(L)),HGTNE(L),
     &    CMNAM(ITPCON(L))
      ENDDO

C Initial menu entry setup.
      IPACT=CREATE
      CALL EKPAGE(IPACT)
   92 IER=0
      IVERT=-3
      ILEN=NCNN

C Loop through the items until the page to be displayed. M is the 
C current menu line index. Build up text strings for the menu. 
   3  M=MHEAD
      VERT(1)=
     &'  Node +ve    |dHght|to| Node -ve    |dHght|via Component'
      DO L=1,ILEN
        IF(L.GE.IST.AND.(L.LE.(IST+MIFULL)))THEN
          M=M+1
          CALL EMKEY(L,KEY,IER)
          WRITE(VERT(M),'(A1,1X,A12,F6.1,A5,A12,F6.1,2X,A12)')KEY,
     &NDNAM(NODPS(L)),HGTPS(L),' --> ',NDNAM(NODNE(L)),HGTNE(L),
     &CMNAM(ITPCON(L))
        endif
      ENDDO

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
      VERT(M+3)  ='? help             '
      VERT(M+4)  ='- exit             '

C Help text for this menu.
      helptopic='mfs_connection_menu'
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
        helptopic='mfs_connection_menu'
        call gethelptext(helpinsub,helptopic,nbhelp)
        CALL PHELPD('connection selection',nbhelp,'-',0,0,IER)
      ELSEIF(IVERT.EQ.(MVERT-2))THEN
        IF(MOD.EQ.'M'.OR.MOD.EQ.'m')THEN

C Allow a connection to be added deleted or copied.
          helptopic='connection_management'
          call gethelptext(helpinsub,helptopic,nbhelp)
          CALL EASKABCD(' ','Options:','add','delete',
     &      'copy','cancel',IOPT,nbhelp)
          IF(IOPT.EQ.1)THEN
            IF(NCNN+1.LE.MCNN)THEN
              CALL MFLDES('A',99)
              ILEN=NCNN
              IPACT=CREATE
              CALL EKPAGE(IPACT)
              IVERT=-1
              GOTO 3
            ELSE
              call usrmsg(' ','Connection list full!','W') 
              IVERT=-1
              GOTO 3
            ENDIF
          ELSEIF(IOPT.EQ.2)THEN
            call usrmsg('Delete which connection ? ',' ','-') 
            CALL EPMENSV
            NPICK=1
            PROMP1='Delete which connection?'
            PROMP2='Delete which connection?'
            CALL EPICKS(NPICK,IVALS,PROMP1,PROMP2,
     &        56,NCNN,ITEMDEL,' ',IER,nbhelp)
            IFOC=IVALS(1)
            CALL EPMENRC
            if(IFOC.ne.0)then
              ID=IFOC
            else
              call usrmsg('No connection selected.',' ','-')
              GOTO 3
            endif
            CALL MFLDES('D',ID)
            ILEN=NCNN
            IPACT=CREATE
            CALL EKPAGE(IPACT)
            GOTO 3
          ELSEIF(IOPT.EQ.3)THEN
            call usrmsg('Copy which connection ? ',' ','-') 
            CALL EPMENSV
            NPICK=1
            PROMP1='Copy which connection?'
            PROMP2='Copy which connection?'
            CALL EPICKS(INPICK,IVALS,PROMP1,PROMP2,
     &        56,NCNN,ITEMDEL,' ',IER,nbhelp)
            IFOC=IVALS(1)
            CALL EPMENRC
            if(IFOC.ne.0)then
              ID=IFOC
            else
              call usrmsg('No connection selected.',' ','-')
              GOTO 3
            endif
            CALL MFLDES('C',ID)
            ILEN=NCNN
            IPACT=CREATE
            CALL EKPAGE(IPACT)
            GOTO 3
          ELSE
            GOTO 3
          ENDIF
        ELSE
          IVERT=-1
          goto 3
        ENDIF
      ELSEIF(IVERT.EQ.(MVERT-3))THEN

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
        IF(MOD.EQ.'M'.OR.MOD.EQ.'m')THEN
          CALL MFLDES('E',IC)
          ILEN=NCNN
          IPACT=CREATE
          CALL EKPAGE(IPACT)
          GOTO 3
        ELSE

C If not in edit mode then return selected node (IS) to the calling routine.       
          RETURN
        ENDIF
      ELSE
        IVERT=-1
        goto 92
      ENDIF
      IVERT=-2
      goto 3

      END

