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
C
C What the code in this file does.
C   CDBDomainExplore - presents a list of the technical domains in the database and
C      allows user to manage those domains or 'drill down' further
C      to categories.
C   CDBCategoryExplore -  presents a list of the component categories in a domain, 
C     allows user to manage those categories or 'drill down' to view
C     spedific components.
C   CDBItemExplore -  presents a list of the components in a category, allows the user
C     to manage those components or 'drill down' to view the data associated
C    with that component. 
C   CDBPSetsExplore -  presents a list of the parameter sets associated with a component and 
C     allows the user to manage those or 'drill down' to view parameters in a
C     specific set [Uses CDBPSetList]
C   CDBParamExplore - presents a list of parameters from a set, allowing the user to manage 
C     and edit specific data entries [Uses CDBParamList,CDBParamEdit] 
C 

C ************************ SUBROUTINE ********************************** 
C ----------------------------------------------------------------------
      SUBROUTINE CDBDomainExplore(EDITABLE,ID,IML,IER)
C ----------------------------------------------------------------------
C ***********************************************************************
C Editable - logical flag if the data is editable
C ID - returned domain id
C IML - maximum level that can be viewed.

#include "epara.h"
#include "gencompDB.h"
#include "gencompDB_common.h"

      DIMENSION ICITEM(13)
      CHARACTER ICITEM*34, KEY*1

      INTEGER ID

      LOGICAL EDITABLE,EDITABLE2,OK,DOK

      IF(NBDOMAIN.gt.0)THEN 
        ILEN=NBDOMAIN
      ELSE
        ILEN=1
      ENDIF
      
      IPACT=CREATE
      CALL EKPAGE(IPACT)

  210 MHEAD=2
      MCTL=5

      IF(NBDOMAIN.GT.0)THEN 
        ILEN=NBDOMAIN
      ELSE
        ILEN=1
      ENDIF
      
      WRITE(ICITEM(1),'(A)') ' Domains in this database...' 
      WRITE(ICITEM(2),'(A)') '  _________________________ '   

      M=MHEAD
      IF(NBDOMAIN.GT.0)THEN
      DO 17 L=1,ILEN
        IF(L.GE.IST.AND.(L.LE.(IST+MIFULL)))THEN
          M=M+1
          CALL EMKEY(L,KEY,IER)
          WRITE(ICITEM(M),'(A1,1X,A32)')KEY, DOMAINMENU(L)(1:32)
        ENDIF
  17  CONTINUE
      ELSE
        IF(EDITABLE)THEN
          WRITE(ICITEM(M),'(A)')'Please create a domain ...'
        ELSE
          WRITE(ICITEM(M),'(A)')'No. domains in database'
        ENDIF
      ENDIF

C If there is a long list then allow paging.
      IF(IPFLG.EQ.0)THEN  
        ICITEM(M+1)='  _________________________ '
      ELSE
        WRITE(ICITEM(M+1),109)IPM,MPM 
  109   FORMAT   ('0 page: ',I2,' of ',I2,' --------')
      ENDIF
      IF(EDITABLE)THEN
        ICITEM(M+2)='> Add/delete/edit a domain'
        ICITEM(M+3)=' '
        ICITEM(M+4)='? Help'
        ICITEM(M+5)='- Exit this menu'
      ELSE
        ICITEM(M+2)=' '
        ICITEM(M+3)=' '
        ICITEM(M+4)='? Help'
        ICITEM(M+5)='- Exit this menu'        
      ENDIF

      NICITEM=M+MCTL
      IMOUT=-1

      CALL EMENU('Select a database domain:',ICITEM,NICITEM,IMOUT)

C First check for menu choices unnafected by the variable menu length. 
      IF(IMOUT.GT.MHEAD.AND.IMOUT.LE.NICITEM-MCTL) THEN
        CALL KEYIND(NICITEM,IMOUT,ICHOSE,IO)

C Return the name of the selected domain category. 
        IF(ICHOSE.GT.0.AND.ICHOSE.LE.NBDOMAIN)THEN
          ID=ICHOSE
          EDITABLE2=EDITABLE
          CALL EPMENSV
          IF(IML.GT.1)THEN
            IML2=IML
            CALL CDBCategoryExplore(EDITABLE2,ID,IC,IML2,IER)
            CALL EPMENRC
          ELSE
            RETURN
          ENDIF

C Re-build the user menu
          GOTO 210
        ELSE
          ID=0
        ENDIF

      ELSEIF(IMOUT.EQ.NICITEM)then
        RETURN
      ELSEIF(IMOUT.EQ.NICITEM-1)then

C Describe components and menu structure.
        call askabout('cdb ',0)

C Catch menu events that may be influenced by the variable menu structure.
      ELSEIF (IMOUT.EQ.NICITEM-3)THEN

C Delete a domain, first display access warning
        OK=.FALSE.
        dok=.true.
        CALL ASKOK(' ',' Continue ?',OK,dok,7)
        IF(.NOT.OK) then
          goto 210
        else
C         CALL ManageComponentsDB('DeleteDomain',idomain,IER)
          GOTO 210
        endif
      ELSEIF (IMOUT.EQ.NICITEM-4)THEN

C If there are enough items allow paging control via EKPAGE.
        IF(IPFLG.EQ.1)THEN
          IPACT=EDIT
          CALL EKPAGE(IPACT)
        ENDIF
        GOTO 210 
      ELSE
        IMOUT=-1
        GOTO 210
      ENDIF
      CALL EPAGE
      GOTO 210

      END

C ************************ SUBROUTINE ********************************** 
C ----------------------------------------------------------------------
      SUBROUTINE CDBCategoryExplore(EDITABLE,ID,IC,IML,IER)
C ----------------------------------------------------------------------
C ***********************************************************************


#include "epara.h"
#include "gencompDB.h"
#include "gencompDB_common.h"

      DIMENSION ICITEM(28)
      CHARACTER ICITEM*34, KEY*1
      INTEGER ID,IC,ID_S

      LOGICAL EDITABLE,EDITABLE2,OK,DOK

      ID_S=ID
      IF(NBCAT(ID).gt.0)THEN 
        ILEN=NBCAT(ID_S)
      ELSE
        ILEN=1
      ENDIF

      IPACT=CREATE
      CALL EKPAGE(IPACT)

  210 MHEAD=2
      MCTL=5

      IF(NBCAT(ID).GT.0)THEN 
        ILEN=NBCAT(ID_S)
      ELSE
        ILEN=1
      ENDIF
      
      WRITE(ICITEM(1),'(A)') ' Categories in this domain..' 
      WRITE(ICITEM(2),'(A)') '  _________________________ '   

      M=MHEAD
      IF(NBCAT(ID_S).GT.0)THEN
      DO 17 L=1,ILEN
        IF(L.GE.IST.AND.(L.LE.(IST+MIFULL)))THEN
          M=M+1
          CALL EMKEY(L,KEY,IER)
          WRITE(ICITEM(M),'(A1,1X,A32)')KEY, CATMENU(ID_S,L)(1:32)
        ENDIF
  17  CONTINUE
      ELSE
        IF(EDITABLE)THEN
          WRITE(ICITEM(M),'(A)')'Please create a category .. '
        ELSE
          WRITE(ICITEM(M),'(A)')'No. categories in this domain'
        ENDIF
      ENDIF

C If there is a long list then allow paging.
      IF(IPFLG.EQ.0)THEN  
        ICITEM(M+1)='  _________________________ '
      ELSE
        WRITE(ICITEM(M+1),109)IPM,MPM 
  109   FORMAT   ('0 page: ',I2,' of ',I2,' --------')
      ENDIF
      IF(EDITABLE)THEN
        ICITEM(M+2)='> Add/delete/edit a category'
        ICITEM(M+3)=' '
        ICITEM(M+4)='? Help'
        ICITEM(M+5)='- Exit this menu'
      ELSE
        ICITEM(M+2)=' '
        ICITEM(M+3)=' '
        ICITEM(M+4)='? Help'
        ICITEM(M+5)='- Exit this menu'        
      ENDIF

      NICITEM=M+MCTL
      IMOUT=-1

      CALL EMENU('Select a domain category:',ICITEM,NICITEM,IMOUT)

C First check for menu choices unnafected by the variable menu length. 
      IF(IMOUT.GT.MHEAD.AND.IMOUT.LE.NICITEM-MCTL) THEN
        CALL KEYIND(NICITEM,IMOUT,ICHOSE,IO)

C Return the name of the selected domain category. 
        IF(ICHOSE.GT.0.AND.ICHOSE.LE.NBCAT(ID))THEN
          EDITABLE2=EDITABLE
          IC=ICHOSE
          CALL EPMENSV
          IF(IML.GT.2)THEN
            IML2=IML
            CALL CDBItemExplore(EDITABLE2,ID_S,IC,IT,IML2,IER)
            CALL EPMENRC
          ELSE
            RETURN
          ENDIF


C Rebuld the user menu
          GOTO 210
        ELSE
          IC=0
        ENDIF

      ELSEIF(IMOUT.EQ.NICITEM)then
        RETURN
      ELSEIF(IMOUT.EQ.NICITEM-1)then

C Describe components and menu structure.
        call askabout('cdb ',0)

C Catch menu events that may be influenced by the variable menu structure.
      ELSEIF (IMOUT.EQ.NICITEM-3)THEN

C Delete a domain, first display access warning
        OK=.FALSE.
        dok=.true.
        CALL ASKOK(' ',' Continue ?',OK,dok,7)
        IF(.NOT.OK) then
          goto 210
        else
C         CALL ManageComponentsDB('DeleteDomain',idomain,IER)
          GOTO 210
        endif
      ELSEIF (IMOUT.EQ.NICITEM-4)THEN

C If there are enough items allow paging control via EKPAGE.
        IF(IPFLG.EQ.1)THEN
          IPACT=EDIT
          CALL EKPAGE(IPACT)
        ENDIF
        GOTO 210 
      ELSE
        IMOUT=-1
        GOTO 210
      ENDIF
      CALL EPAGE
      GOTO 210

      END

C ************************ SUBROUTINE ********************************** 
C ----------------------------------------------------------------------
      SUBROUTINE CDBItemExplore(EDITABLE,ID,IC,IT,IML,IER)
C ----------------------------------------------------------------------
C ***********************************************************************

#include "epara.h"
#include "gencompDB.h"
#include "gencompDB_common.h"

      DIMENSION ICITEM(34)
      CHARACTER ICITEM*64, KEY*1

      INTEGER ID,IC,IT,ID_S,IC_S,IER

      LOGICAL EDITABLE,EDITABLE2,OK,DOK

      ID_S=ID
      IC_S=IC
      IF(NBCAT(ID_S).gt.0)THEN 
        ILEN=NBITEM(ID_S,IC_S)
      ELSE
        ILEN=1
      ENDIF

      IPACT=CREATE
      CALL EKPAGE(IPACT)

  210 MHEAD=2
      MCTL=5

      IF(NBITEM(ID_S,IC_S).GT.0)THEN 
        ILEN=NBITEM(ID_S,IC_S)
      ELSE
        ILEN=1
      ENDIF
      
      WRITE(ICITEM(1),'(A)') ' Items in this category ... ' 
      WRITE(ICITEM(2),'(A)') '  _________________________ '   

      M=MHEAD
      IF(NBITEM(ID_S,IC_S).GT.0)THEN
      DO 17 L=1,ILEN
        IF(L.GE.IST.AND.(L.LE.(IST+MIFULL)))THEN
          M=M+1
          CALL EMKEY(L,KEY,IER)
          WRITE(ICITEM(M),'(A1,1X,A62)')KEY, ITEMMENU(ID_S,IC_S,L)(1:62)
        ENDIF
  17  CONTINUE
      ELSE
        IF(EDITABLE)THEN
          WRITE(ICITEM(M),'(A)')'Please create an item .. '
        ELSE
          WRITE(ICITEM(M),'(A)')'No items in this category'
        ENDIF
      ENDIF

C If there is a long list then allow paging.
      IF(IPFLG.EQ.0)THEN  
        ICITEM(M+1)='  _________________________ '
      ELSE
        WRITE(ICITEM(M+1),109)IPM,MPM 
  109   FORMAT   ('0 page: ',I2,' of ',I2,' --------')
      ENDIF
      IF(EDITABLE)THEN
        ICITEM(M+2)='> Add/delete/edit an item'
        ICITEM(M+3)=' '
        ICITEM(M+4)='? Help'
        ICITEM(M+5)='- Exit this menu'
      ELSE
        ICITEM(M+2)=' '
        ICITEM(M+3)=' '
        ICITEM(M+4)='? Help'
        ICITEM(M+5)='- Exit this menu'        
      ENDIF

      NICITEM=M+MCTL
      IMOUT=-1

      CALL EMENU('Select a category item:',ICITEM,NICITEM,IMOUT)

C First check for menu choices unnafected by the variable menu length. 
      IF(IMOUT.GT.MHEAD.AND.IMOUT.LE.NICITEM-MCTL) THEN
        CALL KEYIND(NICITEM,IMOUT,ICHOSE,IO)

C Return the name of the selected domain category. 
        IF(ICHOSE.GT.0.AND.ICHOSE.LE.NBITEM(ID_S,IC_S))THEN
          IT=ICHOSE
          EDITABLE2=EDITABLE
          CALL EPMENSV
          IF(IML.GT.3)THEN
            IML2=IML
            CALL CDBPSetsExplore(EDITABLE2,ID_S,IC_S,IT,IML2,IER)
            CALL EPMENRC
          ELSE
            RETURN
          ENDIF

C Re-build the user menu
          GOTO 210
        ELSE
          IT=0
        ENDIF

      ELSEIF(IMOUT.EQ.NICITEM)then
        RETURN
      ELSEIF(IMOUT.EQ.NICITEM-1)then

C Describe components and menu structure.
        call askabout('cdb ',0)

C Catch menu events that may be influenced by the variable menu structure.
      ELSEIF (IMOUT.EQ.NICITEM-3)THEN

C Delete a domain, first display access warning
        OK=.FALSE.
        dok=.true.
        CALL ASKOK(' ',' Continue ?',OK,dok,7)
        IF(.NOT.OK) then
          goto 210
        else
C         CALL ManageComponentsDB('DeleteDomain',idomain,IER)
          GOTO 210
        endif
      ELSEIF (IMOUT.EQ.NICITEM-4)THEN
C If there are enough items allow paging control via EKPAGE.
        IF(IPFLG.EQ.1)THEN
          IPACT=EDIT
          CALL EKPAGE(IPACT)
        ENDIF
        GOTO 210 
      ELSE
        IMOUT=-1
        GOTO 210
      ENDIF

      CALL EPAGE
      GOTO 210

      END

C ************************ SUBROUTINE ********************************** 
C ----------------------------------------------------------------------
      SUBROUTINE CDBPSetsExplore(EDITABLE,ID,IC,IT,IML,IER)
C ----------------------------------------------------------------------
C ***********************************************************************

#include "epara.h"
#include "gencompDB.h"
#include "gencompDB_common.h"

      DIMENSION ICITEM(34),S_DATCAT(Max_Datcat)
      CHARACTER ICITEM*34,KEY*1,S_DATCAT*12,TMP_S_DATCAT*12,SET*12
      CHARACTER DESC*72

      INTEGER ID,IC,IT,ID_S,IC_S,IT_S,IER

      LOGICAL EDITABLE,EDITABLE2,STATIC,OK,DOK,MATCH

      MATCH=.FALSE.

C Identify the number of data categories
  200 NDATCAT=0
      ID_S=ID
      IC_S=IC
      IT_S=IT
      SET=' '
      ICMP=ITEMPOINTER(ID_S,IC_S,IT_S)
      DESC=ITEMMENU(ID_S,IC_S,IT_S)


      DO 10 IP=1,Max_Datcat
  10  S_DATCAT(IP)=' '

      IF(NATRIBS(ICMP).NE.0)THEN
        DO 100 IAT=1,NATRIBS(ICMP)
          IF(NDATCAT.GT.0)THEN
            TMP_S_DATCAT=
     &TAGATR(ICMP,IAT,1)(1:LNBLNK(TAGATR(ICMP,IAT,1)))
            MATCH=.FALSE.
            DO 110 JAT=1,NDATCAT
              IF(TMP_S_DATCAT(1:LNBLNK(TMP_S_DATCAT)).EQ.
     &S_DATCAT(JAT)(1:LNBLNK(S_DATCAT(JAT))))THEN
                MATCH=.TRUE.
              ENDIF
  110     CONTINUE
            IF(.NOT.MATCH)THEN
              NDATCAT=NDATCAT+1
              S_DATCAT(NDATCAT)=TMP_S_DATCAT
            ENDIF
          ELSE
            NDATCAT=1
            S_DATCAT(1)=TAGATR(ICMP,IAT,1)(1:LNBLNK(TAGATR(ICMP,IAT,1)))
          ENDIF
  100  CONTINUE
      ENDIF

      IF(NDATCAT.gt.0)THEN 
        ILEN=NDATCAT
      ELSE
        ILEN=1
      ENDIF

      IPACT=CREATE
      CALL EKPAGE(IPACT)

  210 MHEAD=3
      MCTL=5

      IF(NDATCAT.GT.0)THEN 
        ILEN=NDATCAT
      ELSE
        ILEN=1
      ENDIF
      
      WRITE(ICITEM(1),'(A)') ' Parameter sets for:        ' 
      WRITE(ICITEM(2),'(1X,A32)') DESC
      WRITE(ICITEM(3),'(A)') '  _________________________ '   

      M=MHEAD
      IF(NBITEM(ID,IC).GT.0)THEN
      DO 17 L=1,ILEN
        IF(L.GE.IST.AND.(L.LE.(IST+MIFULL)))THEN
          M=M+1
          CALL EMKEY(L,KEY,IER)
          WRITE(ICITEM(M),'(A1,1X,A12)')KEY, S_DATCAT(L)(1:12)
        ENDIF
  17  CONTINUE
      ELSE
        IF(EDITABLE)THEN
          WRITE(ICITEM(M),'(A)')'Please add a parameter set..'
        ELSE
          WRITE(ICITEM(M),'(A)')'No parameter sets available'
        ENDIF
      ENDIF

C If there is a long list then allow paging.
      IF(IPFLG.EQ.0)THEN  
        ICITEM(M+1)='  _________________________ '
      ELSE
        WRITE(ICITEM(M+1),109)IPM,MPM 
  109   FORMAT   ('0 page: ',I2,' of ',I2,' --------')
      ENDIF
      IF(EDITABLE)THEN
        ICITEM(M+2)='> Add/delete/edit a param. set'
        ICITEM(M+3)=' '
        ICITEM(M+4)='? Help'
        ICITEM(M+5)='- Exit this menu'
      ELSE
        ICITEM(M+2)=' '
        ICITEM(M+3)=' '
        ICITEM(M+4)='? Help'
        ICITEM(M+5)='- Exit this menu'        
      ENDIF

      NICITEM=M+MCTL
      IMOUT=-1

      CALL EMENU('Select a parameter set:',ICITEM,NICITEM,IMOUT)

C First check for menu choices unnafected by the variable menu length. 
      IF(IMOUT.GT.MHEAD.AND.IMOUT.LE.NICITEM-MCTL) THEN
        CALL KEYIND(NICITEM,IMOUT,ICHOSE,IO)

C Return the name of the selected domain category. 
        IF(ICHOSE.GT.0.AND.ICHOSE.LE.NDATCAT)THEN
          SET=S_DATCAT(ICHOSE)(1:12)
          STATIC=.TRUE.
          EDITABLE2=EDITABLE
          CALL EPMENSV
          IF(IML.GT.4)THEN
            IML2=IML
            CALL 
     &CDBParamExplore(EDITABLE2,DESC,SET,STATIC,ICMP,IP,IML2,IER)
            CALL EPMENRC  
          ELSE
            RETURN
          ENDIF       
C Re-build the user menu.
          GOTO 200
        ELSE
          SET=' '
        ENDIF

      ELSEIF(IMOUT.EQ.NICITEM)then
        RETURN
      ELSEIF(IMOUT.EQ.NICITEM-1)then

C Describe components and menu structure.
        call askabout('cdb ',0)
  
      ELSEIF (IMOUT.EQ.NICITEM-3)THEN

C Delete a domain, first display access warning
        OK=.FALSE.
        dok=.true.
        CALL ASKOK(' ',' Continue ?',OK,dok,7)
        IF(.NOT.OK) then
          goto 210
        else
C           CALL ManageComponentsDB('DeleteDomain',idomain,IER)
          GOTO 210
        endif
      ELSEIF (IMOUT.EQ.NICITEM-4)THEN

C If there are enough items allow paging control via EKPAGE.
        IF(IPFLG.EQ.1)THEN
          IPACT=EDIT
          CALL EKPAGE(IPACT)
        ENDIF
        GOTO 210 
      ELSE
        IMOUT=-1
        GOTO 210
      ENDIF
      CALL EPAGE
      GOTO 210

      END

C ************************ SUBROUTINE ********************************** 
C ----------------------------------------------------------------------
      SUBROUTINE CDBParamExplore(EDIT1,DESC,SET,STATIC,ICMP,IP,IML,IER)
C ----------------------------------------------------------------------
C ***********************************************************************

#include "epara.h"
#include "gencompDB.h"
#include "gencompDB_common.h"

      DIMENSION ICITEM(34),subICITEM(Max_Parameters)
      CHARACTER ICITEM*64,KEY*1,SET*12,TMP_S_DATCAT*12,subICITEM*61
      CHARACTER SETSTR*12,DATAEXT*12,DATATYP*12,DESC_S*32,DESC*32

      INTEGER ICMP_S,IER,IP,IMOUT,NICITEM

      LOGICAL EDIT1,STATIC,OK,DOK,STAT
c      LOGICAL EDITABLE2
C Identify the number of data categories
      ICMP_S=ICMP
      SETSTR=SET
      DESC_S=DESC
      STAT=STATIC

C Set up the menu
  110 IF(NATRIBS(ICMP_S).EQ.0)THEN 
        ILEN=1
      ENDIF

      MHEAD=4
      MCTL=5  
      M=MHEAD

C Loop through the component data to find the members of the set.
      IF(NATRIBS(ICMP_S).GT.0)THEN
        ILEN=0
        DO 100 IAT=1,NATRIBS(ICMP_S)
          TMP_S_DATCAT=
     &TAGATR(ICMP_S,IAT,1)(1:LNBLNK(TAGATR(ICMP_S,IAT,1)))
          IF(TMP_S_DATCAT(1:LNBLNK(TMP_S_DATCAT)).EQ.
     &SETSTR(1:LNBLNK(SETSTR)))THEN

C check to see if static data is editable.
            IF(TAGATR(ICMP_S,IAT,5)(1:4).EQ.'stat'.AND..NOT.STAT)THEN
              CONTINUE
            ELSE
              ILEN=ILEN+1
              DATATYP=
     &TAGATR(ICMP_S,IAT,2)(1:lnblnk(TAGATR(ICMP_S,IAT,2)))
              DATAEXT=
     &TAGATR(ICMP_S,IAT,4)(1:lnblnk(TAGATR(ICMP_S,IAT,4)))
              IF(DATAEXT(1:4).EQ.'Exte'.OR.
     &DATAEXT(1:4).EQ.'exte')THEN
C First check for external data source [special case]
              WRITE(subICITEM(ILEN),'(A32,A3,A12,A3,A24)')
     &MENUATR(ICMP_S,IAT),'   ',ATRIB(ICMP_S,IAT,1)(1:6),
     &'     external source    '
C Formatting for real number
              ELSEIF(DATATYP(1:4).EQ.'Real'.OR.
     &DATATYP(1:4).EQ.'real')THEN
                  WRITE(subICITEM(ILEN),'(A32,A3,A12,A3,A8)')
     &MENUATR(ICMP_S,IAT),'   ',ATRIB(ICMP_S,IAT,1),'   ',
     &TAGATR(ICMP_S,IAT,6)(1:8)
      
C formatting for integer
              ELSEIF(DATATYP(1:3).EQ.'Int'.OR.
     &DATATYP(1:3).EQ.'int')THEN
                WRITE(subICITEM(ILEN),'(A32,A3,A12,A3,A8)')
     &MENUATR(ICMP_S,IAT),'   ',ATRIB(ICMP_S,IAT,1)(1:12),'   ',
     &TAGATR(ICMP_S,IAT,6)(1:8)
      
C Formatting for text
              ELSEIF(DATATYP(1:4).EQ.'Text'.OR.
     &DATATYP(1:4).EQ.'text')THEN
                  WRITE(subICITEM(ILEN),'(A32,A3,A12)')
     &MENUATR(ICMP_S,IAT),'   ',ATRIB(ICMP_S,IAT,1)(1:12)
              ENDIF
              IF(TAGATR(ICMP_S,IAT,5)(1:4).EQ.'stat') 
     &subICITEM(ILEN)(59:61)='[S]'
            ENDIF
          ENDIF
  100   CONTINUE

      ELSE
        M=M+1
        ILEN=1
        IF(EDIT1)THEN
          WRITE(ICITEM(ILEN),'(A)')'Please add a parameter ...'
        ELSE
          WRITE(ICITEM(ILEN),'(A)')'No parameters available'
        ENDIF
      ENDIF

      IPACT=CREATE
      CALL EKPAGE(IPACT)

  210 MHEAD=4
      MCTL=5
      M=MHEAD
      
      WRITE(ICITEM(1),'(A,A,A)') ' Parameter set [',
     &SETSTR(1:LNBLNK(SETSTR)),'] for :' 
      WRITE(ICITEM(2),'(1X,A32)') DESC_S
      WRITE(ICITEM(3),'(A)') '  _________________________ '   
      WRITE(ICITEM(4),'(A)') 
     &'id|          Description            |  Parameter   |  Units  '

      DO 150 L=1,ILEN
        IF(L.GE.IST.AND.(L.LE.(IST+MIFULL)))THEN
          M=M+1
          CALL EMKEY(L,KEY,IER)
          WRITE(ICITEM(M),'(1X,A1,1X,A)')KEY,subICITEM(L)
        ENDIF
  150 CONTINUE

C If there is a long list then allow paging.
      IF(IPFLG.EQ.0)THEN  
        ICITEM(M+1)='  _________________________ '
      ELSE
        WRITE(ICITEM(M+1),109)IPM,MPM 
  109   FORMAT   ('0 page: ',I2,' of ',I2,' --------')
      ENDIF
      IF(EDIT1)THEN
        ICITEM(M+2)='> Add/delete/edit a parameter'
        ICITEM(M+3)=' '
        ICITEM(M+4)='? Help'
        ICITEM(M+5)='- Exit this menu'
      ELSE
        ICITEM(M+2)=' '
        ICITEM(M+3)=' '
        ICITEM(M+4)='? Help'
        ICITEM(M+5)='- Exit this menu'        
      ENDIF

      NICITEM=M+MCTL

      IMOUT=-1

      CALL EMENU('Select a parameter:',ICITEM,NICITEM,IMOUT)
c      write(*,*) 'Selected ',IMOUT,' of ',NICITEM

C First check for menu choices unnafected by the variable menu length. 
      IF(IMOUT.GT.MHEAD.AND.IMOUT.LE.NICITEM-MCTL) THEN
        CALL KEYIND(NICITEM,IMOUT,ICHOSE,IO)

C Return the name of the selected domain category. 
        IF(ICHOSE.GT.0.AND.ICHOSE.LE.ILEN)THEN
          IP=ICHOSE
c          EDITABLE2=EDIT1
          IF(IML.GT.5)THEN
C          CALL CDBParamEdit()
          ENDIF
          GOTO 110
        ELSE
c          IPS=0
        ENDIF

      ELSEIF(IMOUT.EQ.NICITEM)then
        RETURN
      ELSEIF(IMOUT.EQ.NICITEM-1)then

C Describe components and menu structure.
        call askabout('cdb ',0)

      ELSEIF (IMOUT.EQ.NICITEM-3)THEN

C Parameter management first display access warning
        OK=.FALSE.
        dok=.true.
        CALL ASKOK(' ',' Continue ?',OK,dok,7)
        IF(.NOT.OK) then
          goto 210
        else
C         CALL ManageComponentsDB('Parameter',xxxx,IER)
          GOTO 210
        endif
      ELSEIF (IMOUT.EQ.NICITEM-4)THEN

C If there are enough items allow paging control via EKPAGE.
        IF(IPFLG.EQ.1)THEN
          IPACT=EDIT
          CALL EKPAGE(IPACT)
        ENDIF
        GOTO 210 
      ELSE
        IMOUT=-1
        GOTO 210
      ENDIF
      IMOUT=-1
      CALL EPAGE
      GOTO 210

      END

