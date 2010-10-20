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

C *********************************************************************
C This file contains the following subroutines:

C MENU3D displays the multi-gridding menu.
C DFNCMP displays the components definition menu.
C DFNCNC displays the connections definition menu.
C DFNCSS displays the surface-surface connections indices menu.
C DFNCSE displays the surface-edge connections indices menu.
C DFNCEE displays the edge-edge connections indices menu.
C DFNCEC displays the edge-corner connections indices menu.
C SELZON displays the zone selection menu.
C READ3D reads 3D gridding configuration file.
C SAVE3D saves 3D gridding configuration file.
C *********************************************************************

C ***************************  MENU3D  ********************************
C MENU3D displays the multi-gridding menu.
C *********************************************************************
      SUBROUTINE MENU3D(ITRC)
#include "building.h"
#include "geometry.h"

      COMMON/FILEP/IFIL
      common/pophelp/h(60)
      COMMON/RAY2/ITDSP,ITBND,ITEPT,ITZNM,ITSNM,ITVNO,ITORG,ITSNR,
     &            ITOBS,ITHLS,ITHLZ,ITGRD,GRDIS,ITPPSW
      COMMON/RAY3/MODIFY,MODLEN,MODBND

      COMMON/GR3D01/INDXS(MS),THKS(MS),DCOSS(MS,3,3)
      COMMON/GR3D21/NUMEDG,INDXE(MEZ),DCOSE(MEZ,3,3)
      COMMON/GR3D31/NUMCRN,INDXC(MCZ),DCOSC(MCZ,3,3)
      COMMON/GR3D06/INDXSS(MSSZ),INDXSE(MSEZ),INDXEE(MEEZ),INDXEC(MECZ)
      COMMON/GR3D51/NUMSS,IBDYSS(MSSZ,2),ILNBSS(MSSZ,2),IVRTSS(MSSZ,2)
      COMMON/GR3D56/NUMSE,IBDYSE(MSEZ,2),ILNBSE(MSEZ)
      COMMON/GR3D61/NUMEE,IBDYEE(MEEZ,2),IVRTEE(MEEZ)
      COMMON/GR3D66/NUMEC,IBDYEC(MECZ,2),IVRTEC(MECZ)
      COMMON/GR3D100/BLDG3D,ZONE3D(MCOM)
      COMMON/GR3D108/L3DCVS(MCOM),L3DCNC(MCOM),L3DNDC(MCOM),L3DTAQ(MCOM)

      LOGICAL ZONOK,MODIFY,MODBND,MODLEN,OK,DOK,BLDG3D,ZONE3D,XST

      CHARACTER ITEM(15)*26,SZN*15,H*72
      CHARACTER*72 L3DCVS,L3DCNC,L3DNDC,L3DTAQ
      integer ier
      integer MITEM,INO ! max items and current menu item

      SZN=' '
      IZ=0
      IUNIT=IFIL+1
      ZONOK=.FALSE.
      MODIFY=.FALSE.

C General image option flags.
      ITDSP=1
      ITBND=1
      ITEPT=0
      ITZNM=1
      ITSNM=1
      ITVNO=0
      ITORG=1
      ITSNR=1
      ITGRD=1
      GRDIS=0.0
      ITPPSW=0

C Draw the complete building.
      IDRW1=1
      IDRW2=0
      CALL GRAAPH(IDRW1,IDRW2)

C Display the menu.
   10 INO=-3
      WRITE(ITEM(1),'(A,A)')'1 Zone : ',SZN
      ITEM(2)='  ------------------------'
      ITEM(3)='2 Define components       '
      ITEM(4)='3 Define connections      '
      ITEM(5)='4 Default 1D model        '
      ITEM(6)='5 Default 3D model        '
      ITEM(7)='  ------------------------'
      ITEM(8)='6 Create 3D gridding files'
      ITEM(9)='7 Delete 3D gridding files'
      ITEM(10)='8 Save 3D conf. into file '
      ITEM(11)='9 Read 3D conf. from file '
      ITEM(12)='  ------------------------'
      ITEM(13)='s Support facilities      '
      ITEM(14)='? Help                    '
      ITEM(15)='- Exit                    '
      MITEM=15

C Help text for this menu.
      H(1)= 'This menu can be used to produce the required files'
      H(2)= 'by the simulator for multi-D simulation. It deals'
      H(3)= 'with a single zone at a time. The zone is divided'
      H(4)= 'into components (surfaces, edges, and corners)'
      H(5)= 'which are connected to each others via one or'
      H(6)= 'more of connection surfaces (surface-to-surface,'
      H(7)= 'surface-to-edge, edge-to-edge, and edge-to-corner).'
      H(8)= '  '
      H(9)= '    Each surface can be defined to be lumped or '
      H(10)='discretized. However, only the discretized surfaces'
      H(11)='can be connected to other components. Edges and'
      H(12)='corners can be not defined lumped, or discretized as '
      H(13)='they do not exist in the zone model). Both lumped'
      H(13)='and discretized edges and corners can be connected'
      H(14)='to other components.'
      H(15)='     '
      H(16)='    Connection surfaces can be activated (defined)'
      H(17)='or ignored (not defined). An activated connection'
      H(18)='surface should connect to components with the '
      H(19)='proper status.'
      H(20)='     '
      H(21)='The multi-gridding menu is the main menu for'
      H(22)='controlling the building constructions. In order to'
      H(23)='activate this menu the zone should be defined first.'
      H(24)='This will automaticaly produce a default 1D model '
      H(25)='(but in 3D configuration) for the selected zone. '
      H(26)='     '
      H(27)='By selecting option (5) a default 3D model will be'
      H(28)='produced for which all the gridding details of the'
      H(29)='edges and corners are related to the surfaces they'
      H(30)='are incontact with. To change the default 1D or 3D'
      H(31)='gridding options (2) and (3) can be used.'
      H(32)='     '
      H(33)='Instead of generating a model manually, the multi-D'
      H(34)='gridding details can be fed in via option (8).'
      H(35)='On the other hand the existing model can be '
      H(36)='saved for later use via option (7). Finally, the'
      H(37)='multi-gridding files (i.e., nodes coordinates,'
      H(38)='control volumes details, and connections '
      H(39)='infomations) can be produced (to be used later'
      H(40)='by BPS module) via option (6). Option (s) activates'
      H(41)='the menu for converting the multi-D gridding file'
      H(42)='from binary versions to ASCII and vice versa. This'
      H(43)='allows the manually edited, via any text editor,'
      H(44)='files to be used in producing the required multi-D'
      H(45)='gridding files.'

C Draw the appropriate graph.
      IF(MODIFY)CALL GRAAPH(IDRW1,IDRW2)
      CALL USRMSG(' ',' ','-')
      CALL EMENU('   Multi-Gridding',ITEM,MITEM,INO)
      IF(.NOT.ZONOK.AND.((INO.GE.3.AND.INO.LE.6).OR.INO.EQ.13))THEN

C Trap un-accepted options.
        CALL USRMSG(' ','the zone should be defined first.','W')
      ELSEIF(.NOT.ZONOK.AND.INO.GE.8.AND.INO.LE.11)THEN
        CALL USRMSG(' ','the zone should be defined first.','W')
      ELSEIF(INO.EQ.1)THEN

C Select the zone.
        IF(IDRW1.NE.1.OR.MODIFY)THEN
          IDRW1=1
          IDRW2=0
          CALL GRAAPH(IDRW1,IDRW2)
        ENDIF
        CALL SELZON(ITRC,SZN,ZONOK,IZ)
        IF(ZONOK)THEN
          IDRW1=2
          IDRW2=IZ
          CALL GRAAPH(IDRW1,IDRW2)
        ENDIF
      ELSEIF(INO.EQ.3)THEN

C Define components.
        CALL DFNCMP(IZ,SZN,IDRW1,IDRW2)
      ELSEIF(INO.EQ.4)THEN

C Define connections.
        CALL DFNCNC(IZ,SZN,IDRW1,IDRW2)
      ELSEIF(INO.EQ.5)THEN

C create the default 1D mesh for the current zone.
        DO 30 IS=1,NSUR
          INDXS(IS)=1
   30   CONTINUE
        DO 40 IEG=1,NUMEDG
          INDXE(IEG)=0
   40   CONTINUE
        DO 50 ICR=1,NUMCRN
          INDXC(ICR)=0
   50   CONTINUE
        DO 60 ISS=1,NUMSS
          INDXSS(ISS)=0
   60   CONTINUE
        DO 70 ISE=1,NUMSE
          INDXSE(ISE)=0
   70   CONTINUE
        DO 80 IEE=1,NUMEE
          INDXEE(IEE)=0
   80   CONTINUE
        DO 90 IEC=1,NUMEC
          INDXEC(IEC)=0
   90   CONTINUE
      ELSEIF(INO.EQ.6)THEN

C create the default 3D mesh for the current zone.
        DO 130 IS=1,NSUR
          INDXS(IS)=3
  130   CONTINUE
        DO 140 IEG=1,NUMEDG
          INDXE(IEG)=3
  140   CONTINUE
        DO 150 ICR=1,NUMCRN
          INDXC(ICR)=3
  150   CONTINUE
        DO 160 ISS=1,NUMSS
          INDXSS(ISS)=3
  160   CONTINUE
        DO 170 ISE=1,NUMSE
          INDXSE(ISE)=3
  170   CONTINUE
        DO 180 IEE=1,NUMEE
          INDXEE(IEE)=3
  180   CONTINUE
        DO 190 IEC=1,NUMEC
          INDXEC(IEC)=3
  190   CONTINUE
      ELSEIF(INO.EQ.8)THEN

C Create ESP-r 3D mesh.
C Check if there are un-acceptable 3D configuration informations.
        CALL CHCK3D(IWRNG,IERRR)
        IF(IERRR.LE.0)THEN
          IF(IWRNG.GT.0)THEN
            dok=.false.
            h(1)='A check of the 3D description found no errors but'
            h(2)='did find some issues that generated warnings. Please'
            h(3)='review the warnings to see if you should continue.'
            CALL ASKOK(' Warning messages were given',
     &        ' Are you sure you want to continue (see help)?',OK,dok,3)
            IF(OK)THEN

C Define the intersection points for all types of lines in surfaces.
              CALL INSPTS

C Create the control volumes for the surfaces.
              CALL CVSSRF

C Create the control volumes, nodes, and connections for all elements 
C in the zone.
              CALL CZCNVS(IZ,IER)
            ENDIF
          ELSE
            CALL INSPTS
            CALL CVSSRF
            CALL CZCNVS(IZ,IER)
          ENDIF
        ELSE
          CALL USRMSG(
     &   ' Fatal errors were detected while checking 3D configuration.',
     &   ' The 3D files will not be generated.','W')
        ENDIF
      ELSEIF(INO.EQ.9)THEN

C Delete the associated 3D file.
        IF(ZONE3D(IZ))THEN
          CALL FINDFIL(L3DCVS(IZ),XST)
          IF(XST)THEN
            ier=0
            CALL EFOPRAN(IUNIT,L3DCVS(IZ),5,3,IER)
            CALL EFDELET(IUNIT,ISTAT)
          ENDIF
          CALL FINDFIL(L3DCNC(IZ),XST)
          IF(XST)THEN
            ier=0
            CALL EFOPRAN(IUNIT,L3DCNC(IZ),7,3,IER)
            CALL EFDELET(IUNIT,ISTAT)
          ENDIF
          CALL FINDFIL(L3DNDC(IZ),XST)
          IF(XST)THEN
            ier=0
            CALL EFOPRAN(IUNIT,L3DNDC(IZ),3,3,IER)
            CALL EFDELET(IUNIT,ISTAT)
          ENDIF
          CALL FINDFIL(L3DTAQ(IZ),XST)
          IF(XST)THEN
            ier=0
            CALL EFOPRAN(IUNIT,L3DTAQ(IZ),2,3,IER)
            CALL EFDELET(IUNIT,ISTAT)
          ENDIF

C Project manager handles the utility files.
          call usrmsg(
     &      'Update these changes in the project manager zone com-',
     &      'position menu (the supplemental files).','W')
        ELSE
          CALL USRMSG(' ',' 3D files do not exist','W')
        ENDIF
      ELSEIF(INO.EQ.10)THEN

C Save gridding information into a file.
        CALL SAVE3D(IZ)
      ELSEIF(INO.EQ.11)THEN

C Read gridding from a file
        CALL READ3D(IZ)
      ELSEIF(INO.EQ.MITEM-2)THEN

C Support facilities.
        CALL SUPORT(IZ)
      ELSEIF(INO.EQ.MITEM-1)THEN

C Help.
        CALL PHELPD(' Multi-Gridding menu ',45,'-',0,0,IER)
      ELSEIF(INO.EQ.MITEM)THEN

C Return to the main multi-gridding menu.
        RETURN
      ENDIF
      GOTO 10
      END

C ***************************    DFNCMP    ****************************
C DFNCMP displays the components definition menu.
C *********************************************************************
      SUBROUTINE DFNCMP(IZ,SZN,IDRW1,IDRW2)

      common/pophelp/h(60)
      COMMON/RAY3/MODIFY,MODLEN,MODBND

      LOGICAL     MODIFY,MODLEN,MODBND

      DIMENSION ITEM(8)

      CHARACTER H*72,ITEM*26,SZN*15
      integer MITEM,INO  ! max items and current menu item

   10 INO=-3
      WRITE(ITEM(1),'(A,A)')'  zone : ',SZN
      ITEM(2)='  ------------------------'
      ITEM(3)='1 grid surfaces           '
      ITEM(4)='2 grid edges              '
      ITEM(5)='3 grid corners            '
      ITEM(6)='  ------------------------'
      ITEM(7)='? help                    '
      ITEM(8)='- Exit                    '
      MITEM=8
      IF(IDRW1.NE.2.OR.MODIFY)THEN
        IDRW1=2
        IDRW2=IZ
        CALL GRAAPH(IDRW1,IDRW2)
      ENDIF

C Help text for this menu,.
      H(1)=' The components menu allows the selection of the'
      H(2)=' component type whose gridding details are to be'
      H(3)=' modefied. Depending on your selection other menues'
      H(4)=' will be activated.'
      H(5)=' '

      CALL EMENU('   Components',ITEM,MITEM,INO)
      IF(INO.EQ.3)THEN

C Define surfaces.
        CALL GRDSRF(IZ,IDRW1,IDRW2)
      ELSEIF(INO.EQ.4)THEN

C Define edges.
        CALL GRDEDG(IZ,IDRW1,IDRW2)
      ELSEIF(INO.EQ.5)THEN

C Define corners.
        CALL GRDCRN(IZ,IDRW1,IDRW2)
      ELSEIF(INO.EQ.MITEM-1)THEN

C Help.
        CALL PHELPD('components definition',5,'-',0,0,IER)
      ELSEIF(INO.EQ.MITEM)THEN

C Return to the main multi-gridding menu.
        RETURN
      ENDIF
      GOTO 10
      END

C **************************    DFNCNC     ****************************
C DFNCNC displays the connections definition menu.
C *********************************************************************
      SUBROUTINE DFNCNC(ICOMP,SZN,IDRW1,IDRW2)

      common/pophelp/h(60)
      COMMON/RAY3/MODIFY,MODLEN,MODBND

      LOGICAL     MODIFY,MODLEN,MODBND

      CHARACTER H*72,ITEM(9)*26,SZN*15

   10 INO=-3
      WRITE(ITEM(1),'(A,A)')'  zone : ',SZN
      ITEM(2)='  ------------------------'
      ITEM(3)='1 surface-surface         '
      ITEM(4)='2 surface-edge            '
      ITEM(5)='3 edge-edge               '
      ITEM(6)='4 edge-corner             '
      ITEM(7)='  ------------------------'
      ITEM(8)='? help                    '
      ITEM(9)='- Exit                    '
      MITEM=9
      IF(IDRW1.NE.2.OR.MODIFY)THEN
        IDRW1=2
        IDRW2=ICOMP
        CALL GRAAPH(IDRW1,IDRW2)
      ENDIF

C Help text for this menu.
      H(1)=' '
      H(2)='    The connection surfaces menu allows the'
      H(3)=' selection of the connection surface type '
      H(4)=' whose gridding details are to be modified.'
      H(5)=' Depending on your selection other menues'
      H(6)=' will be activated.'
 
      CALL USRMSG(' ',' ','-')
      CALL EMENU('   Connections',ITEM,MITEM,INO)
      IF(INO.EQ.3)THEN

C Define surface-surface connections.
        CALL DFNCSS
      ELSEIF(INO.EQ.4)THEN

C Define surface-edge connections.
        CALL DFNCSE(ICOMP)
      ELSEIF(INO.EQ.5)THEN

C Define edge-edge connections.
        CALL DFNCEE(ICOMP)
      ELSEIF(INO.EQ.6)THEN

C Define edge-corner connections.
        CALL DFNCEC(ICOMP)
      ELSEIF(INO.EQ.MITEM-1)THEN

C Help.
       CALL PHELPD('connections definition',6,'-',0,0,IER)

C Return to the main multi-gridding menu.
      ELSEIF(INO.EQ.MITEM)THEN
        RETURN
      ENDIF
      GOTO 10
      END

C ****************************   DFNCSS    ****************************
C DFNCSS displays the surface-surface connections indices menu.
C *********************************************************************
      SUBROUTINE DFNCSS
#include "epara.h"
#include "building.h"

      common/pophelp/h(60)

      COMMON/GR3D06/INDXSS(MSSZ),INDXSE(MSEZ),INDXEE(MEEZ),INDXEC(MECZ)
      COMMON/GR3D51/NUMSS,IBDYSS(MSSZ,2),ILNBSS(MSSZ,2),IVRTSS(MSSZ,2)

      CHARACTER VERT(35)*33,KEY*1,H*72,STTS*4

C Initialise connection menu size variables based on window size. 
C IVERT is the menu position, MVERT the current number of menu lines.
      MHEAD=0
      MCTL=4
      ILEN=NUMSS
      IPACT=CREATE
      CALL EKPAGE(IPACT)

C Initial menu entry setup.
   10 IVERT=-3

C Loop through the items until the page to be displayed. M is the 
C current menu line index. Build up text strings for the menu. 
      M=MHEAD
      DO 20 L=1,ILEN
        IF(L.GE.IST.AND.(L.LE.(IST+MIFULL)))THEN
          M=M+1
          CALL EMKEY(L,KEY,IER)
          VERT(M)=' '
          IF(INDXSS(L).EQ.3)THEN
            STTS='| D '
          ELSE
            STTS='| N '
          ENDIF
          IV1=IVRTSS(L,1)
          IV2=IVRTSS(L,2)
          WRITE(VERT(M),'(A1,1X,A8,I2,A12,I2,A2,A5)')KEY,
     &              'vertex (',IV1,') : vertex (',IV2,') ',STTS
        ENDIF
   20 CONTINUE

C Number of actual items displayed.
      MVERT=M+MCTL

C If a long list include page facility text.      
      IF(IPFLG.EQ.0)THEN
        VERT(M+1)='  _____________________________'
      ELSE
        WRITE(VERT(M+1),'(A,I2,A,I2,A)')'0 Page ------- Part: ',IPM,
     &                                  ' of ',MPM,' ---'
      ENDIF
      VERT(M+2)  ='                             '
      VERT(M+3)  ='? Help                       '
      VERT(M+4)  ='- Exit                       '

C Help for this menu.
      H(1)=' The surface-to-surface connection surfaces menu'
      H(2)=' allows altering the status of the surface-to-'
      H(3)=' surface connection surfaces'
      H(4)=' '

C Now display the menu.
      CALL USRMSG(' ',' ','-')
      CALL EMENU('    surface-surface connections',
     &           VERT,MVERT,IVERT)
      IF(IVERT.GT.MHEAD.AND.IVERT.LT.(MVERT-MCTL+1))THEN

C Edit item identified by KEYIND.
        CALL KEYIND(MVERT,IVERT,IFOC,IO)
          IF(INDXSS(IFOC).EQ.3)THEN
            INDXSS(IFOC)=1
          ELSE
            INDXSS(IFOC)=3
          ENDIF
      ELSEIF(IVERT.EQ.(MVERT-3))THEN

C If there are enough items allow paging control via EKPAGE.
        IF(IPFLG.EQ.1)THEN
          IPACT=EDIT
          CALL EKPAGE(IPACT)
        ENDIF
      ELSEIF(IVERT.EQ.(MVERT-1))THEN

C Display help messages.
        CALL PHELPD('srf-srf connection surfaces',4,'-',0,0,IER)
      ELSEIF(IVERT.EQ.MVERT)THEN

C Return to multi-gridding menu.
        RETURN
      ENDIF
      GOTO 10
      END 

C ****************************   DFNCSE    ****************************
C DFNCSE displays the surface-edge connections indices menu.
C *********************************************************************
      SUBROUTINE DFNCSE(IZ)
#include "epara.h"
#include "building.h"
#include "geometry.h"

      COMMON/OUTIN/IUOUT,IUIN
      common/pophelp/h(60)
      COMMON/C24/IZSTOCN(MCOM,MS)

      COMMON/GR3D06/INDXSS(MSSZ),INDXSE(MSEZ),INDXEE(MEEZ),INDXEC(MECZ)
      COMMON/GR3D21/NUMEDG,INDXE(MEZ),DCOSE(MEZ,3,3)
      COMMON/GR3D24/ISFEDG(MEZ,2),IVXEDG(MEZ,2)
      COMMON/GR3D56/NUMSE,IBDYSE(MSEZ,2),ILNBSE(MSEZ)

      CHARACTER VERT(35)*35,KEY*1,H*72,STTS*4,ENAME*9,TEXT*72
      CHARACTER STATS*16

C Initialise connection menu size variables based on window size. 
C IVERT is the menu position, MVERT the current number of menu lines.
      MHEAD=0
      MCTL=4
      ILEN=NUMSE
      IPACT=CREATE
      CALL EKPAGE(IPACT)

C Initial menu entry setup.
   10 IVERT=-3

C Loop through the items until the page to be displayed. M is the 
C current menu line index. Build up text strings for the menu. 
      M=MHEAD
      DO 20 L=1,ILEN
        IF(L.GE.IST.AND.(L.LE.(IST+MIFULL)))THEN
          M=M+1
          CALL EMKEY(L,KEY,IER)
          VERT(M)=' '
          IF(INDXSE(L).EQ.3)THEN
            STTS='| D '
          ELSE
            STTS='| N '
          ENDIF
          IS1=IBDYSE(L,1)
          icon=IZSTOCN(iz,is1)
          WRITE(ENAME,'(A6,I2,A1)')'edge (',IBDYSE(L,2),')'
          WRITE(VERT(M),'(A1,1X,A12,A6,A9,1X,A5)')KEY,
     &              SSNAME(icon),'<-->',ENAME,STTS
        ENDIF
   20 CONTINUE

C Number of actual items displayed.
      MVERT=M+MCTL

C If a long list include page facility text.      
      IF(IPFLG.EQ.0)THEN
        VERT(M+1)='  _________________________________'
      ELSE
        WRITE(VERT(M+1),'(A,I2,A,I2,A)')'0 Page ------- Part: ',IPM,
     &                                  ' of ',MPM,' -----'
      ENDIF
      VERT(M+2)  ='! list existing edges        '
      VERT(M+3)  ='? Help                       '
      VERT(M+4)  ='- Exit                       '

C Help text for this menu.
      H(1)=' The surface-to-edge connection surfaces menu'
      H(2)=' allows altering the status of the surface-to-'
      H(3)=' edge connection surfaces'

C Now display the menu.
      CALL USRMSG(' ',' ','-')
      CALL EMENU(' surface-edge connections',
     &           VERT,MVERT,IVERT)
      IF(IVERT.GT.MHEAD.AND.IVERT.LT.(MVERT-MCTL+1))THEN

C Edit item identified by KEYIND.
        CALL KEYIND(MVERT,IVERT,IFOC,IO)
          IF(INDXSE(IFOC).EQ.3)THEN
            INDXSE(IFOC)=1
          ELSE
            INDXSE(IFOC)=3
          ENDIF
      ELSEIF(IVERT.EQ.(MVERT-3))THEN

C If there are enough items allow paging control via EKPAGE.
        IF(IPFLG.EQ.1)THEN
          IPACT=EDIT
          CALL EKPAGE(IPACT)
        ENDIF
      ELSEIF(IVERT.EQ.(MVERT-2))THEN

C List existing edges.
C        CALL EPAGE
        CALL EDISP(IUOUT,
     &    ' No.  IV1   IV2   surface-1      surface-2      status')
        CALL EDISP(IUOUT,
     &   '-----------------------------------------------------------')
        DO 30 IEG=1,NUMEDG
          IF(INDXE(IEG).EQ.1)THEN
            STATS='lumped'
          ELSEIF(INDXE(IEG).EQ.3)THEN
            STATS='discretized'
          ELSE
            STATS='not defined'
          ENDIF
          ISF1=ISFEDG(IEG,1)
          ISF2=ISFEDG(IEG,2)
          icon1=IZSTOCN(iz,isf1)
          icon2=IZSTOCN(iz,isf2)
          WRITE(TEXT,'(3(I3,3X),3(A12,3X))')IEG,IVXEDG(IEG,1),
     &      IVXEDG(IEG,2),SSNAME(icon1),SSNAME(icon2),STATS
          CALL EDISP(IUOUT,TEXT)
   30   CONTINUE
      ELSEIF(IVERT.EQ.(MVERT-1))THEN

C Display help messages.
        CALL PHELPD('srf-edge connection surfaces',4,'-',0,0,IER)
      ELSEIF(IVERT.EQ.MVERT)THEN

C Return to multi-gridding menu.
        RETURN
      ENDIF
      GOTO 10
      END 

C ****************************   DFNCEE    ****************************
C DFNCEE displays the edge-edge connections indices menu.
C *********************************************************************
      SUBROUTINE DFNCEE(IZ)
#include "epara.h"
#include "building.h"
#include "geometry.h"

      COMMON/OUTIN/IUOUT,IUIN
      common/pophelp/h(60)
      COMMON/C24/IZSTOCN(MCOM,MS)

      COMMON/GR3D06/INDXSS(MSSZ),INDXSE(MSEZ),INDXEE(MEEZ),INDXEC(MECZ)
      COMMON/GR3D21/NUMEDG,INDXE(MEZ),DCOSE(MEZ,3,3)
      COMMON/GR3D24/ISFEDG(MEZ,2),IVXEDG(MEZ,2)
      COMMON/GR3D61/NUMEE,IBDYEE(MEEZ,2),IVRTEE(MEEZ)

      CHARACTER TEXT*72,STATS*16
      CHARACTER VERT(35)*19,KEY*1,H*72,STTS*4

C Initialise connection menu size variables based on window size. 
C IVERT is the menu position, MVERT the current number of menu lines.
      MHEAD=0
      MCTL=4
      ILEN=NUMEE
      IPACT=CREATE
      CALL EKPAGE(IPACT)

C Initial menu entry setup.
   10 IVERT=-3

C Loop through the items until the page to be displayed. M is the 
C current menu line index. Build up text strings for the menu. 
      M=MHEAD
      DO 20 L=1,ILEN
        IF(L.GE.IST.AND.(L.LE.(IST+MIFULL)))THEN
          M=M+1
          CALL EMKEY(L,KEY,IER)
          VERT(M)=' '
          IF(INDXEE(L).EQ.3)THEN
            STTS='| D '
          ELSE
            STTS='| N '
          ENDIF
          IV1=IVRTEE(L)
          WRITE(VERT(M),'(A1,A9,I2,A2,A5)')KEY,' vertex (',IV1,') ',STTS
        ENDIF
   20 CONTINUE

C Number of actual items displayed.
      MVERT=M+MCTL

C If a long list include page facility text.      
      IF(IPFLG.EQ.0)THEN
        VERT(M+1)='  _________________'
      ELSE
        WRITE(VERT(M+1),'(A,I2,A,I2,A)')'0 Page [',IPM,' of ',MPM,' ]'
      ENDIF
      VERT(M+2)  ='! list edges      '
      VERT(M+3)  ='? Help            '
      VERT(M+4)  ='- Exit            '

C Help for this menu.
      H(1)=' The edge-to-edge connection surfaces menu'
      H(2)=' allows altering the status of the edge-to-'
      H(3)=' edge connection surfaces'

C Now display the menu.
      CALL EMENU(' edge-edge',VERT,MVERT,IVERT)

C Edit item identified by KEYIND.
      IF(IVERT.GT.MHEAD.AND.IVERT.LT.(MVERT-MCTL+1))THEN
        CALL KEYIND(MVERT,IVERT,IFOC,IO)
          IF(INDXEE(IFOC).EQ.3)THEN
            INDXEE(IFOC)=1
          ELSE
            INDXEE(IFOC)=3
          ENDIF
      ELSEIF(IVERT.EQ.(MVERT-3))THEN

C If there are enough items allow paging control via EKPAGE.
        IF(IPFLG.EQ.1)THEN
          IPACT=EDIT
          CALL EKPAGE(IPACT)
        ENDIF
      ELSEIF(IVERT.EQ.(MVERT-2))THEN

C List existing edges.
C        CALL EPAGE
        CALL EDISP(IUOUT,
     &    ' No.  IV1   IV2   surface-1      surface-2      status')
        CALL EDISP(IUOUT,
     &   '-----------------------------------------------------------')
        DO 30 IEG=1,NUMEDG
          IF(INDXE(IEG).EQ.1)THEN
            STATS='lumped'
          ELSEIF(INDXE(IEG).EQ.3)THEN
            STATS='discretized'
          ELSE
            STATS='not defined'
          ENDIF
          ISF1=ISFEDG(IEG,1)
          ISF2=ISFEDG(IEG,2)
          icon1=IZSTOCN(iz,isf1)
          icon2=IZSTOCN(iz,isf2)
          WRITE(TEXT,'(3(I3,3X),3(A12,3X))')IEG,IVXEDG(IEG,1),
     &      IVXEDG(IEG,2),SSNAME(icon1),SSNAME(icon2),STATS
          CALL EDISP(IUOUT,TEXT)
   30   CONTINUE
      ELSEIF(IVERT.EQ.(MVERT-1))THEN

C Display help messages.
        CALL PHELPD('edge-edge connection surfaces',3,'-',0,0,IER)
      ELSEIF(IVERT.EQ.MVERT)THEN

C Return to multi-gridding menu.
        RETURN
      ENDIF
      GOTO 10
      END 

C ****************************   DFNCEC    ****************************
C DFNCEC displays the edge-corner connections indices menu.
C *********************************************************************
      SUBROUTINE DFNCEC(IZ)
#include "epara.h"
#include "building.h"
#include "geometry.h"

      COMMON/OUTIN/IUOUT,IUIN
      common/pophelp/h(60)
      COMMON/C24/IZSTOCN(MCOM,MS)

      COMMON/GR3D06/INDXSS(MSSZ),INDXSE(MSEZ),INDXEE(MEEZ),INDXEC(MECZ)
      COMMON/GR3D21/NUMEDG,INDXE(MEZ),DCOSE(MEZ,3,3)
      COMMON/GR3D24/ISFEDG(MEZ,2),IVXEDG(MEZ,2)
      COMMON/GR3D31/NUMCRN,INDXC(MCZ),DCOSC(MCZ,3,3)
      COMMON/GR3D34/ISFCRN(MCZ,3),IVXCRN(MCZ)
      COMMON/GR3D66/NUMEC,IBDYEC(MECZ,2),IVRTEC(MECZ)

      CHARACTER TEXT*72,STATS*16
      CHARACTER VERT(35)*33,KEY*1,H*72,STTS*4,ENAME*9,CNAME*11

C Initialise connection menu size variables based on window size. 
C IVERT is the menu position, MVERT the current number of menu lines.
      MHEAD=0
      MCTL=5
      ILEN=NUMEC
      IPACT=CREATE
      CALL EKPAGE(IPACT)

C Initial menu entry setup.
   10 IVERT=-3

C Loop through the items until the page to be displayed. M is the 
C current menu line index. Build up text strings for the menu. 
      M=MHEAD
      DO 20 L=1,ILEN
        IF(L.GE.IST.AND.(L.LE.(IST+MIFULL)))THEN
          M=M+1
          CALL EMKEY(L,KEY,IER)
          VERT(M)=' '
          IF(INDXEC(L).EQ.3)THEN
            STTS='| D '
          ELSE
            STTS='| N '
          ENDIF
          WRITE(ENAME,'(A6,I2,A1)')'edge (',ABS(IBDYEC(L,1)),')'
          WRITE(CNAME,'(A8,I2,A1)')'corner (',ABS(IBDYEC(L,2)),')'
          WRITE(VERT(M),'(A1,1X,A9,A6,A11,A5)')KEY,
     &                  ENAME,' <--> ',CNAME,STTS
        ENDIF
   20 CONTINUE

C Number of actual items displayed.
      MVERT=M+MCTL

C If a long list include page facility text.      
      IF(IPFLG.EQ.0)THEN
        VERT(M+1)='  _______________________________'
      ELSE
        WRITE(VERT(M+1),'(A,I2,A,I2,A)')'0 Page ------- Part: ',IPM,
     &                                  ' of ',MPM,' -----'
      ENDIF
      VERT(M+2)  ='! list existing edges        '
      VERT(M+3)  ='* list existing corners      '
      VERT(M+4)  ='? Help                       '
      VERT(M+5)  ='- Exit                       '

C Help text for this menu.
      H(1)=' The edge-to-corner connection surfaces menu'
      H(2)=' allows altering the status of the edge-to-'
      H(3)=' corner connection surfaces'

C Now display the menu.
      CALL USRMSG(' ',' ','-')
      CALL EMENU(' edge-corner connections',
     &           VERT,MVERT,IVERT)

C Edit item identified by KEYIND.
      IF(IVERT.GT.MHEAD.AND.IVERT.LT.(MVERT-MCTL+1))THEN
        CALL KEYIND(MVERT,IVERT,IFOC,IO)
          IF(INDXEC(IFOC).EQ.3)THEN
            INDXEC(IFOC)=1
          ELSE
            INDXEC(IFOC)=3
          ENDIF
      ELSEIF(IVERT.EQ.(MVERT-4))THEN

C If there are enough items allow paging control via EKPAGE.
        IF(IPFLG.EQ.1)THEN
          IPACT=EDIT
          CALL EKPAGE(IPACT)
        ENDIF
      ELSEIF(IVERT.EQ.(MVERT-3))THEN

C List existing edges.
C        CALL EPAGE
        CALL EDISP(IUOUT,
     &    ' No.  IV1   IV2   surface-1      surface-2      status')
        CALL EDISP(IUOUT,
     &   '-----------------------------------------------------------')
        DO 30 IEG=1,NUMEDG
          IF(INDXE(IEG).EQ.1)THEN
            STATS='lumped'
          ELSEIF(INDXE(IEG).EQ.3)THEN
            STATS='discretized'
          ELSE
            STATS='not defined'
          ENDIF
          ISF1=ISFEDG(IEG,1)
          ISF2=ISFEDG(IEG,2)
          icon1=IZSTOCN(iz,isf1)
          icon2=IZSTOCN(iz,isf2)
          WRITE(TEXT,'(3(I3,3X),3(A12,3X))')IEG,IVXEDG(IEG,1),
     &      IVXEDG(IEG,2),SSNAME(icon1),SSNAME(icon2),STATS
          CALL EDISP(IUOUT,TEXT)
   30   CONTINUE
      ELSEIF(IVERT.EQ.(MVERT-2))THEN

C List existing corners.
C        CALL EPAGE
        WRITE(TEXT,'(2A)')' No.  vrtx  surface-1      surface-2',
     &         '      surface-3      status'
        CALL EDISP(IUOUT,TEXT)
        WRITE(TEXT,'(2A)')'------------------------------------------',
     &              '--------------------------'
        CALL EDISP(IUOUT,TEXT)
        DO 40 ICRN=1,NUMCRN
          IF(INDXC(ICRN).EQ.1)THEN
            STATS='lumped'
          ELSEIF(INDXC(ICRN).EQ.3)THEN
            STATS='discretized'
          ELSE
            STATS='not defined'
          ENDIF
          ISF1=ISFCRN(ICRN,1)
          ISF2=ISFCRN(ICRN,2)
          ISF3=ISFCRN(ICRN,3)
          icon1=IZSTOCN(iz,isf1)
          icon2=IZSTOCN(iz,isf2)
          icon3=IZSTOCN(iz,isf3)
          WRITE(TEXT,'(2(I3,3X),4(A12,3X))')ICRN,IVXCRN(ICRN),
     &          SSNAME(icon1),SSNAME(icon2),SSNAME(icon3),STATS
          CALL EDISP(IUOUT,TEXT)
   40   CONTINUE
      ELSEIF(IVERT.EQ.(MVERT-1))THEN

C Display help messages.
        CALL PHELPD('edge-corner connection surfaces',4,'-',0,0,IER)
      ELSEIF(IVERT.EQ.MVERT)THEN

C Return to multi-gridding menu.
        RETURN
      ENDIF
      GOTO 10
      END 

C ***************************      SELZON     *************************
C SELZON displays the zone selection menu.
C *********************************************************************
      SUBROUTINE SELZON(ITRC,SZN,ZONOK,IZ)
#include "building.h"
#include "model.h"

C geometry.h provides commons G0/G1 and G2.
#include "geometry.h"

      COMMON/OUTIN/IUOUT,IUIN
      common/pophelp/h(60)
      COMMON/FILEP/IFIL
      
      integer ncomp,ncon
      COMMON/C1/NCOMP,NCON

      COMMON/G1T/XFT(MV),ZFT(MV)
      COMMON/GR3D01/INDXS(MS),THKS(MS),DCOSS(MS,3,3)
      COMMON/GR3D09/XP(3),YP(3),ZP(3)
      COMMON/GR3D10/XMINS(MS),XMAXS(MS),ZMINS(MS),ZMAXS(MS)

      LOGICAL ZONOK

      DIMENSION IVALZ(MCOM)

      CHARACTER SZN*12,H*72

      IUNIT=IFIL+2

C Show the zone selection menu.
      H(1)='    Select the zone for which the multi-D gridding'
      H(2)=' details are to be created/modified.'
      H(3)=' '
      INPICK=1
   20 CALL EPICKS(INPICK,IVALZ,' ',' ',12,
     &            NCOMP,zname,' SELECT ZONE',IER,3)
      IZ=IVALZ(1)
      IF(IZ.GT.0.AND.IZ.LE.NCOMP)THEN
        SZN=zname(IZ)
        call georead(IUNIT,LGEOM(IZ),IZ,1,IUOUT,IER)
        IF(IER.EQ.0)CALL ECONST(LTHRM(IZ),IUNIT,IZ,ITRC,IUOUT,IER)
        IF(IER.EQ.0)THEN
          ZONOK=.TRUE.

C Initialize the multi-D gridding variables.
          CALL INGRD3

C Create the default gridding for all the surfaces in the zone.
          DO 30 IS=1,NSUR

C Find MAX & MIN surface coordinates.
            CALL TRNSF4(IS)
            XMAX=-1.E+8
            ZMAX=-1.E+8
            ZMIN=1.E+8
            XMIN=1.E+8
            NV=NVER(IS)
            DO 40 IVRT=1,NV
              XMAX=AMAX1(XMAX,XFT(IVRT))
              ZMAX=AMAX1(ZMAX,ZFT(IVRT))
              XMIN=AMIN1(XMIN,XFT(IVRT))
              ZMIN=AMIN1(ZMIN,ZFT(IVRT))
   40       CONTINUE
            XMAXS(IS)=XMAX
            ZMAXS(IS)=ZMAX
            ZMINS(IS)=ZMIN
            XMINS(IS)=XMIN

C Define the local X,Y,and Z axes for the current surface.
            DO 50 I123=1,3
              DCOSS(IS,1,I123)=XP(I123)
              DCOSS(IS,2,I123)=YP(I123)
              DCOSS(IS,3,I123)=ZP(I123)
   50       CONTINUE

C Define surface boundaries.
            CALL SRFBND(IS)

C Define surface default gridding.
            CALL DFTGRD(IS)
   30     CONTINUE

C Define the surfaces indentation from inside.
          CALL SRFIND

C Define the existing edges and corners.
          CALL TOTEDG
          CALL TOTCRN

C Define all available connections.
          CALL INTCNC

C Initialize the connections indentations.
          CALL INDCNC

C Initialize the edges and corners gridding.
          CALL INDEDG
          CALL INDCRN
        ELSE
          SZN=' '
          CALL USRMSG(' Problem detected while trying',
     &                ' to open zone geometry file.','W')
          ZONOK=.FALSE.
        ENDIF
      ELSEIF(IZ.EQ.0)THEN
        SZN=' '
        ZONOK=.FALSE.
        RETURN
      ELSE
        GOTO 20
      ENDIF
      RETURN
      END

C ***************************     READ3D     **************************
C READ3D reads 3D gridding cofiguration file.
C *********************************************************************
      SUBROUTINE READ3D(IZ)
#include "building.h"
#include "geometry.h"

      COMMON/FILEP/IFIL
      common/pophelp/h(60)

      COMMON/GR3D01/INDXS(MS),THKS(MS),DCOSS(MS,3,3)
      COMMON/GR3D02/XVRT(MS,MVS),ZVRT(MS,MVS)
      COMMON/GR3D03/NLINB(MS),NLIND(MS),NLINA(MS)
      COMMON/GR3D04/ILINE(MS,MLS,2),ILINB(MS,MLBS,2)
      COMMON/GR3D05/NVRTB(MS),NVRTD(MS),NVRTA(MS)
      COMMON/GR3D06/INDXSS(MSSZ),INDXSE(MSEZ),INDXEE(MEEZ),INDXEC(MECZ)
      COMMON/GR3D21/NUMEDG,INDXE(MEZ),DCOSE(MEZ,3,3)
      COMMON/GR3D24/ISFEDG(MEZ,2),IVXEDG(MEZ,2)
      COMMON/GR3D25/NDXE(MEZ),DXE(MEZ,MZE)
      COMMON/GR3D26/NDYE(MEZ),DYE(MEZ,MZE)
      COMMON/GR3D27/NDZE(MEZ),DZE(MEZ,MZE)
      COMMON/GR3D28/ITPEDG(MEZ,MZE,MZE,MZE)
      COMMON/GR3D31/NUMCRN,INDXC(MCZ),DCOSC(MCZ,3,3)
      COMMON/GR3D34/ISFCRN(MCZ,3),IVXCRN(MCZ)
      COMMON/GR3D35/NDXC(MEZ),DXC(MEZ,MZE)
      COMMON/GR3D36/NDYC(MEZ),DYC(MEZ,MZE)
      COMMON/GR3D37/NDZC(MEZ),DZC(MEZ,MZE)
      COMMON/GR3D38/ITPCRN(MCZ,MZE,MZE,MZE)
      COMMON/GR3D51/NUMSS,IBDYSS(MSSZ,2),ILNBSS(MSSZ,2),IVRTSS(MSSZ,2)
      COMMON/GR3D56/NUMSE,IBDYSE(MSEZ,2),ILNBSE(MSEZ)
      COMMON/GR3D61/NUMEE,IBDYEE(MEEZ,2),IVRTEE(MEEZ)
      COMMON/GR3D66/NUMEC,IBDYEC(MECZ,2),IVRTEC(MECZ)

      CHARACTER STRG*124,FILE3D*72
      CHARACTER H*72,WORD*124

      IER=0

      WRITE(WORD,'(A,A4)')zname(IZ)(1:lnzname(IZ)),'.c3d'
      FILE3D=WORD(1:72)

C Open the file.
      H(1)='    Input the name for the zone file in which'
      H(2)=' the multi-gridding configuration details exit.'
      H(3)=' '
      CALL EASKS(FILE3D,' 3D gridding configuration file name ?',' ',
     &       72,' ',' 3D gridding file ',IER,3)
      IUNIT=IFIL+1
      CALL EFOPSEQ(IUNIT,FILE3D,1,IER)
      IF(IER.LT.0)RETURN

C For all surfaces.
      CALL STRIPC(IUNIT,STRG,1,ND,1,' No. of surfaces ',IER)
      K=0
      CALL EGETWI(STRG,K,NSURF,NSUR,NSUR,'F',' surfaces ',IER)
      DO 20 I=1,NSUR
        CALL STRIPC(IUNIT,STRG,1,ND,1,' surfaces No. ',IER)
        K=0
        CALL EGETWI(STRG,K,ISRF,I,I,'F',' surfaces ',IER)
        CALL STRIPC(IUNIT,STRG,3,ND,1,' No. of vertices ',IER)
        K=0
        CALL EGETWI(STRG,K,NVRTB(I),0,MVS,'W',' boundary ',IER)
        CALL EGETWI(STRG,K,NVRTD(I),0,MVS,'W',' default ',IER)
        CALL EGETWI(STRG,K,NVRTA(I),0,MVS,'W',' added ',IER)
        DO 30 J=1,NVRTA(I)
          CALL STRIPC(IUNIT,STRG,2,ND,1,' X & Z coord. ',IER)
          K=0
          CALL EGETWR(STRG,K,XVRT(I,J),1.,2.,'-',' X ',IER)
          CALL EGETWR(STRG,K,ZVRT(I,J),1.,2.,'-',' Z ',IER)
   30   CONTINUE
        CALL STRIPC(IUNIT,STRG,3,ND,1,' No. of lines ',IER)
        K=0
        CALL EGETWI(STRG,K,NLINB(I),0,MLBS,'F',' boundary ',IER)
        CALL EGETWI(STRG,K,NLIND(I),0,MLS,'F',' default ',IER)
        CALL EGETWI(STRG,K,NLINA(I),0,MLS,'F',' added ',IER)
        DO 40 J=1,NLINB(I)
          CALL STRIPC(IUNIT,STRG,4,ND,1,' line ',IER)
          K=0
          CALL EGETWI(STRG,K,ILINE(I,J,1),0,MVS,'W','start',IER)
          CALL EGETWI(STRG,K,ILINE(I,J,2),0,MVS,'W',' end ',IER)
          CALL EGETWI(STRG,K,ILINB(I,J,1),0,MTV,'W','start',IER)
          CALL EGETWI(STRG,K,ILINB(I,J,2),0,MTV,'W',' end ',IER)
   40   CONTINUE
        DO 45 J=NLINB(I)+1,NLINA(I)
          CALL STRIPC(IUNIT,STRG,2,ND,1,' line ',IER)
          K=0
          CALL EGETWI(STRG,K,ILINE(I,J,1),0,MVS,'W','start',IER)
          CALL EGETWI(STRG,K,ILINE(I,J,2),0,MVS,'W',' end ',IER)
   45   CONTINUE
   20 CONTINUE

C For all the edges.
      CALL STRIPC(IUNIT,STRG,1,ND,1,' No. of edges ',IER)
      K=0
      CALL EGETWI(STRG,K,NEDG,NUMEDG,NUMEDG,'F',' edges ',IER)
      DO 50 I=1,NEDG
        CALL STRIPC(IUNIT,STRG,5,ND,1,' edge data ',IER)
        K=0
        CALL EGETWI(STRG,K,IEG,I,I,'F',' edge No. ',IER)
        CALL EGETWI(STRG,K,IVXEDG(I,1),0,MTV,'F',' vertex-1 ',IER)
        CALL EGETWI(STRG,K,IVXEDG(I,2),0,MTV,'F',' vertex-2 ',IER)
        CALL EGETWI(STRG,K,ISFEDG(I,1),0,MS,'F',' surface-1 ',IER)
        CALL EGETWI(STRG,K,ISFEDG(I,2),0,MS,'F',' surface-2 ',IER)
        CALL STRIPC(IUNIT,STRG,3,ND,1,' DZ,DX,DY ',IER)
        K=0
        CALL EGETWI(STRG,K,NDXE(I),0,MZE,'F',' DX ',IER)
        NE1=NDXE(I)
        CALL EGETWI(STRG,K,NDYE(I),0,MZE,'F',' DY ',IER)
        NE2=NDYE(I)
        CALL EGETWI(STRG,K,NDZE(I),0,MZE,'W',' DZ ',IER)
        NE3=NDZE(I)
        CALL STRIPC(IUNIT,STRG,NE1,ND,1,' NDX ',IER)
        K=0
        DO 55 IX=1,NE1
          CALL EGETWR(STRG,K,DXE(I,IX),1.,2.,'-',' DX ',IER)
   55   CONTINUE
        CALL STRIPC(IUNIT,STRG,NE2,ND,1,' NDY ',IER)
        K=0
        DO 56 IY=1,NE2
          CALL EGETWR(STRG,K,DYE(I,IY),1.,2.,'-',' DY ',IER)
   56   CONTINUE
        CALL STRIPC(IUNIT,STRG,NE3,ND,1,' NDZ ',IER)
        K=0
        DO 57 IL=1,NE3
          CALL EGETWR(STRG,K,DZE(I,IL),1.,2.,'-',' DZ ',IER)
   57   CONTINUE
        DO 60 IL=1,NDZE(I)
          CALL STRIPC(IUNIT,STRG,1,ND,1,' DZ ',IER)
          K=0
          CALL EGETWI(STRG,K,IDZ,IL,IL,'F',' IZ ',IER)

        DO 60 IY=NDYE(I),1,-1
          CALL STRIPC(IUNIT,STRG,NE1,ND,1,' construction ',IER)
          K=0

        DO 60 IX=1,NDXE(I)
          CALL EGETWI(STRG,K,ITPEDG(I,IX,IY,IL),0,1,'-',' ',IER)
   60   CONTINUE
  50  CONTINUE

C For all the corners.
      CALL STRIPC(IUNIT,STRG,1,ND,1,' No. of corners ',IER)
      K=0
      CALL EGETWI(STRG,K,NCRN,NUMCRN,NUMCRN,'F',' corners ',IER)
      DO 80 I=1,NCRN
        CALL STRIPC(IUNIT,STRG,5,ND,1,' corner data ',IER)
        K=0
        CALL EGETWI(STRG,K,ICR,I,I,'F',' corner No. ',IER)
        CALL EGETWI(STRG,K,IVXCRN(I),0,MTV,'F',' vertex ',IER)
        CALL EGETWI(STRG,K,ISFCRN(I,1),0,MS,'F',' surface-1 ',IER)
        CALL EGETWI(STRG,K,ISFCRN(I,2),0,MS,'F',' surface-2 ',IER)
        CALL EGETWI(STRG,K,ISFCRN(I,3),0,MS,'F',' surface-3 ',IER)
        CALL STRIPC(IUNIT,STRG,3,ND,1,' DZ,DX,DY ',IER)
        K=0
        CALL EGETWI(STRG,K,NDXC(I),0,MZE,'F',' DX ',IER)
        NE1=NDXC(I)
        CALL EGETWI(STRG,K,NDYC(I),0,MZE,'F',' DY ',IER)
        NE2=NDYC(I)
        CALL EGETWI(STRG,K,NDZC(I),0,MZE,'F',' DZ ',IER)
        NE3=NDZC(I)
        CALL STRIPC(IUNIT,STRG,NE1,ND,1,' DX ',IER)
        K=0
        DO 85 IX=1,NE1
          CALL EGETWR(STRG,K,DXC(I,IX),1.,2.,'-',' DX ',IER)
   85   CONTINUE
        CALL STRIPC(IUNIT,STRG,NE2,ND,1,' DY ',IER)
        K=0
        DO 86 IY=1,NE2
          CALL EGETWR(STRG,K,DYC(I,IY),1.,2.,'-',' DY ',IER)
   86   CONTINUE
        CALL STRIPC(IUNIT,STRG,NE3,ND,1,' DZ ',IER)
        K=0
        DO 87 IL=1,NE3
          CALL EGETWR(STRG,K,DZC(I,IL),1.,2.,'-',' DZ ',IER)
   87   CONTINUE
        DO 90 IL=1,NDZC(I)
          CALL STRIPC(IUNIT,STRG,1,ND,1,' IZ ',IER)
          K=0
          CALL EGETWI(STRG,K,IDZ,IL,IL,'F',' IZ ',IER)

        DO 90 IY=NDYC(I),1,-1
          CALL STRIPC(IUNIT,STRG,NE1,ND,1,' construction ',IER)
          K=0

        DO 90 IX=1,NDXC(I)
          CALL EGETWI(STRG,K,ITPCRN(I,IX,IY,IL),0,1,'-',' ',IER)
   90   CONTINUE
  80  CONTINUE

C Read the 3D status for each component and connection surface.
C Older versions of 3D configuration files do not have this, so if line
C does not exist assume the default status.

C For all surfaces.
      I2=0
      IDO=INT((NSUR-1)/10)+1
      DO 200 II=1,IDO
        I1=I2+1
        IF(II.EQ.IDO)THEN
          NDATA=NSUR-I2
          I2=NSUR
        ELSE
          NDATA=10
          I2=I2+10
        ENDIF
        CALL STRIPC(IUNIT,STRG,NDATA,ND,1,' surf status ',IER)
        IF(IER.NE.0)GOTO 1000
        K=0
        DO 210 IDATA=I1,I2
          CALL EGETWI(STRG,K,INDXS(IDATA),0,3,'W',' status ',IER)
 210    CONTINUE
 200  CONTINUE

C For all edges.
      I2=0
      IDO=INT((NUMEDG-1)/10)+1
      DO 220 II=1,IDO
        I1=I2+1
        IF(II.EQ.IDO)THEN
          NDATA=NUMEDG-I2
          I2=NUMEDG
        ELSE
          NDATA=10
          I2=I2+10
        ENDIF
        CALL STRIPC(IUNIT,STRG,NDATA,ND,1,' edge status ',IER)
        IF(IER.NE.0)GOTO 1000
        K=0
        DO 230 IDATA=I1,I2
          CALL EGETWI(STRG,K,INDXE(IDATA),0,3,'W',' status ',IER)
 230    CONTINUE
 220  CONTINUE

C For all corners.
      I2=0
      IDO=INT((NUMCRN-1)/10)+1
      DO 240 II=1,IDO
        I1=I2+1
        IF(II.EQ.IDO)THEN
          NDATA=NUMCRN-I2
          I2=NUMCRN
        ELSE
          NDATA=10
          I2=I2+10
        ENDIF
        CALL STRIPC(IUNIT,STRG,NDATA,ND,1,' corner status ',IER)
        IF(IER.NE.0)GOTO 1000
        K=0
        DO 250 IDATA=I1,I2
          CALL EGETWI(STRG,K,INDXC(IDATA),0,3,'W',' status ',IER)
 250    CONTINUE
 240  CONTINUE

C For all surface-surface connection surfaces.
      I2=0
      IDO=INT((NUMSS-1)/10)+1
      DO 300 II=1,IDO
        I1=I2+1
        IF(II.EQ.IDO)THEN
          NDATA=NUMSS-I2
          I2=NUMSS
        ELSE
          NDATA=10
          I2=I2+10
        ENDIF
        CALL STRIPC(IUNIT,STRG,NDATA,ND,1,' 3D status ',IER)
        IF(IER.NE.0)GOTO 1000
        K=0
        DO 310 IDATA=I1,I2
          CALL EGETWI(STRG,K,INDXSS(IDATA),0,3,'W',' status ',IER)
 310    CONTINUE
 300  CONTINUE

C For all surface-edge connection surfaces.
      I2=0
      IDO=INT((NUMSE-1)/10)+1
      DO 320 II=1,IDO
        I1=I2+1
        IF(II.EQ.IDO)THEN
          NDATA=NUMSE-I2
          I2=NUMSE
        ELSE
          NDATA=10
          I2=I2+10
        ENDIF
        CALL STRIPC(IUNIT,STRG,NDATA,ND,1,' 3D status ',IER)
        IF(IER.NE.0)GOTO 1000
        K=0
        DO 330 IDATA=I1,I2
          CALL EGETWI(STRG,K,INDXSE(IDATA),0,3,'W',' status ',IER)
 330    CONTINUE
 320  CONTINUE

C For all edge-edge connection surfaces.
      I2=0
      IDO=INT((NUMEE-1)/10)+1
      DO 340 II=1,IDO
        I1=I2+1
        IF(II.EQ.IDO)THEN
          NDATA=NUMEE-I2
          I2=NUMEE
        ELSE
          NDATA=10
          I2=I2+10
        ENDIF
        CALL STRIPC(IUNIT,STRG,NDATA,ND,1,' 3D status ',IER)
        IF(IER.NE.0)GOTO 1000
        K=0
        DO 350 IDATA=I1,I2
          CALL EGETWI(STRG,K,INDXEE(IDATA),0,3,'W',' status ',IER)
 350    CONTINUE
 340  CONTINUE

C For all edge-corner connection surfaces.
      I2=0
      IDO=INT((NUMEC-1)/10)+1
      DO 360 II=1,IDO
        I1=I2+1
        IF(II.EQ.IDO)THEN
          NDATA=NUMEC-I2
          I2=NUMEC
        ELSE
          NDATA=10
          I2=I2+10
        ENDIF
        CALL STRIPC(IUNIT,STRG,NDATA,ND,1,' 3D status ',IER)
        IF(IER.NE.0)GOTO 1000
        K=0
        DO 370 IDATA=I1,I2
          CALL EGETWI(STRG,K,INDXEC(IDATA),0,3,'W',' status ',IER)
 370    CONTINUE
 360  CONTINUE
 1000 CALL ERPFREE(IUNIT,ISTAT)
      RETURN
      END

C ****************************    SAVE3D   ****************************
C SAVE3D saves 3D gridding configuration file.
C *********************************************************************
      SUBROUTINE SAVE3D(IZ)
#include "building.h"
#include "geometry.h"

      COMMON/FILEP/IFIL
      common/pophelp/h(60)

      COMMON/GR3D01/INDXS(MS),THKS(MS),DCOSS(MS,3,3)
      COMMON/GR3D02/XVRT(MS,MVS),ZVRT(MS,MVS)
      COMMON/GR3D03/NLINB(MS),NLIND(MS),NLINA(MS)
      COMMON/GR3D04/ILINE(MS,MLS,2),ILINB(MS,MLBS,2)
      COMMON/GR3D05/NVRTB(MS),NVRTD(MS),NVRTA(MS)
      COMMON/GR3D06/INDXSS(MSSZ),INDXSE(MSEZ),INDXEE(MEEZ),INDXEC(MECZ)
      COMMON/GR3D21/NUMEDG,INDXE(MEZ),DCOSE(MEZ,3,3)
      COMMON/GR3D24/ISFEDG(MEZ,2),IVXEDG(MEZ,2)
      COMMON/GR3D25/NDXE(MEZ),DXE(MEZ,MZE)
      COMMON/GR3D26/NDYE(MEZ),DYE(MEZ,MZE)
      COMMON/GR3D27/NDZE(MEZ),DZE(MEZ,MZE)
      COMMON/GR3D28/ITPEDG(MEZ,MZE,MZE,MZE)
      COMMON/GR3D31/NUMCRN,INDXC(MCZ),DCOSC(MCZ,3,3)
      COMMON/GR3D34/ISFCRN(MCZ,3),IVXCRN(MCZ)
      COMMON/GR3D35/NDXC(MEZ),DXC(MEZ,MZE)
      COMMON/GR3D36/NDYC(MEZ),DYC(MEZ,MZE)
      COMMON/GR3D37/NDZC(MEZ),DZC(MEZ,MZE)
      COMMON/GR3D38/ITPCRN(MCZ,MZE,MZE,MZE)
      COMMON/GR3D51/NUMSS,IBDYSS(MSSZ,2),ILNBSS(MSSZ,2),IVRTSS(MSSZ,2)
      COMMON/GR3D56/NUMSE,IBDYSE(MSEZ,2),ILNBSE(MSEZ)
      COMMON/GR3D61/NUMEE,IBDYEE(MEEZ,2),IVRTEE(MEEZ)
      COMMON/GR3D66/NUMEC,IBDYEC(MECZ,2),IVRTEC(MECZ)

      CHARACTER*72 FILE3D
      CHARACTER H*72,WORD*124

      WRITE(WORD,'(A,A4)')zname(IZ)(1:lnzname(IZ)),'.c3d'
      FILE3D=WORD(1:72)
      H(1)='    Input the name for the zone file in which'
      H(2)=' the multi-gridding configuration details are'
      H(3)=' to be saved.'
      H(4)=' '
      CALL EASKS(FILE3D,' 3D gridding configuration file name ?',' ',
     &       72,' ',' 3D gridding file ',IER,4)
      IUNIT=IFIL+1
      CALL EFOPSEQ(IUNIT,FILE3D,4,IER)
      IF(IER.LT.0)RETURN
      WRITE(IUNIT,'(A)',IOSTAT=ISTAT,ERR=1000)'#    SURFACES  ******'
      WRITE(IUNIT,'(I6,A)',IOSTAT=ISTAT,ERR=1000)NSUR,
     &     '                   # total number of surfaces.'
      DO 20 I=1,NSUR
        WRITE(IUNIT,'(I6,A)',IOSTAT=ISTAT,ERR=1000)I,
     &  '                   # surface number.'
        WRITE(IUNIT,'(3I6,A)',IOSTAT=ISTAT,ERR=1000)NVRTB(I),
     &        NVRTD(I),NVRTA(I),
     &  '       # number of boundary & gridding vertices.'
        DO 30 J=1,NVRTA(I)
          WRITE(IUNIT,'(2F10.3,A,I4)',IOSTAT=ISTAT,ERR=1000)
     &          XVRT(I,J),ZVRT(I,J),
     &    '     # X & Z coordinates for vertex ',J
   30   CONTINUE
        WRITE(IUNIT,'(3I6,A)',IOSTAT=ISTAT,ERR=1000)NLINB(I),
     &        NLIND(I),NLINA(I),
     &  '       # number of boundary & gridding lines.'
        DO 40 J=1,NLINB(I)
          WRITE(IUNIT,'(I6,3I4,A,I4)',IOSTAT=ISTAT,ERR=1000)
     &        (ILINE(I,J,JK),JK=1,2),(ILINB(I,J,JK),JK=1,2),
     &    '       # start & end (mesh & geo.) vertices for line ',J
   40   CONTINUE
        DO 45 J=NLINB(I)+1,NLINA(I)
          WRITE(IUNIT,'(I6,I4,15X,A,I4)',IOSTAT=ISTAT,ERR=1000)
     &        (ILINE(I,J,JK),JK=1,2),
     &        '# start & end (mesh & geo.) vertices for line ',J
   45   CONTINUE
   20 CONTINUE
      WRITE(IUNIT,'(A)',IOSTAT=ISTAT,ERR=1000)'#    EDGES     ******'
      WRITE(IUNIT,'(I6,32X,A)',IOSTAT=ISTAT,ERR=1000)NUMEDG,
     &                   '# total number of edges.'
      DO 50 IEG=1,NUMEDG
        WRITE(IUNIT,'(5I6,A)',IOSTAT=ISTAT,ERR=1000)IEG,IVXEDG(IEG,1),
     &              IVXEDG(IEG,2),ISFEDG(IEG,1),ISFEDG(IEG,2),
     &           '        # edge No., start & end vertices & surfaces.'
        NE1=NDXE(IEG)
        NE2=NDYE(IEG)
        NE3=NDZE(IEG)
        WRITE(IUNIT,'(3I6,20X,A)',IOSTAT=ISTAT,ERR=1000)NE1,NE2,NE3,
     &            '# No. of DX, DY, & DZ.'
        WRITE(IUNIT,'(A12)')'# DX values.'
        WRITE(IUNIT,'(10F9.3)')(DXE(IEG,J),J=1,NE1)
        WRITE(IUNIT,'(A12)')'# DY values.'
        WRITE(IUNIT,'(10F9.3)')(DYE(IEG,J),J=1,NE2)
        WRITE(IUNIT,'(A12)')'# DZ values.'
        WRITE(IUNIT,'(10F9.3)')(DZE(IEG,J),J=1,NE3)
        DO 70 IDZ=1,NE3
          WRITE(IUNIT,'(A)',IOSTAT=ISTAT,ERR=1000)
     &        '# thermal property type for each volume.'
          WRITE(IUNIT,'(I6,32X,A)',IOSTAT=ISTAT,ERR=1000)IDZ,
     &                  '# level No.'
          DO 80 IY=NE2,1,-1
            WRITE(IUNIT,'(14I6)')(ITPEDG(IEG,IX,IY,IDZ),IX=1,NE1)
   80     CONTINUE
   70   CONTINUE
   50 CONTINUE
      WRITE(IUNIT,'(A)',IOSTAT=ISTAT,ERR=1000)'#    CORNERS   ******'
      WRITE(IUNIT,'(I6,32X,A)',IOSTAT=ISTAT,ERR=1000)NUMCRN,
     &     '# total number of corners.'
      DO 150 ICR=1,NUMCRN
        WRITE(IUNIT,'(5I6,A)',IOSTAT=ISTAT,ERR=1000)ICR,IVXCRN(ICR),
     &              ISFCRN(ICR,1),ISFCRN(ICR,2),ISFCRN(ICR,3),
     &              '        # corner No., vertex No. and surfaces.'
        NE1=NDXC(ICR)
        NE2=NDYC(ICR)
        NE3=NDZC(ICR)
        WRITE(IUNIT,'(3I6,20X,A)',IOSTAT=ISTAT,ERR=1000)NE1,
     &               NE2,NE3,'# total number of DX,DY & DZ.'
        WRITE(IUNIT,'(A12)')'# DX values.'
        WRITE(IUNIT,'(10F9.3)')(DXC(ICR,J),J=1,NE1)
        WRITE(IUNIT,'(A12)')'# DY values.'
        WRITE(IUNIT,'(10F9.3)')(DYC(ICR,J),J=1,NE2)
        WRITE(IUNIT,'(A12)')'# DZ values.'
        WRITE(IUNIT,'(10F9.3)')(DZC(ICR,J),J=1,NE3)
        DO 170 ILZ=1,NE3
          WRITE(IUNIT,'(A)',IOSTAT=ISTAT,ERR=1000)
     &   '# thermal property type for each volume.'
        WRITE(IUNIT,'(I6,32X,A)',IOSTAT=ISTAT,ERR=1000)ILZ,'# level No.'
          DO 180 IY=NE2,1,-1
            WRITE(IUNIT,'(14I6)')(ITPCRN(ICR,IX,IY,ILZ),IX=1,NE1)
  180     CONTINUE
  170   CONTINUE
  150 CONTINUE

C Save the 3D status for each component and connection surface.
      WRITE(IUNIT,'(A)',IOSTAT=ISTAT,ERR=1000)
     &                                 '# 3D status for each surface:'
      I2=0
      IDO=INT((NSUR-1)/10)+1
      DO 200 II=1,IDO
        I1=I2+1
        IF(II.EQ.IDO)THEN
          I2=NSUR
        ELSE
          I2=I2+10
        ENDIF
        WRITE(IUNIT,*,IOSTAT=ISTAT,ERR=1000)(INDXS(I),I=I1,I2)
 200  CONTINUE
      WRITE(IUNIT,'(A)',IOSTAT=ISTAT,ERR=1000)
     &                                 '# 3D status for each edge:'
      I2=0
      IDO=INT((NUMEDG-1)/10)+1
      DO 210 II=1,IDO
        I1=I2+1
        IF(II.EQ.IDO)THEN
          I2=NUMEDG
        ELSE
          I2=I2+10
        ENDIF
        WRITE(IUNIT,*,IOSTAT=ISTAT,ERR=1000)(INDXE(I),I=I1,I2)
 210  CONTINUE
      WRITE(IUNIT,'(A)',IOSTAT=ISTAT,ERR=1000)
     &                                 '# 3D status for each corner:'
      I2=0
      IDO=INT((NUMCRN-1)/10)+1
      DO 220 II=1,IDO
        I1=I2+1
        IF(II.EQ.IDO)THEN
          I2=NUMCRN
        ELSE
          I2=I2+10
        ENDIF
        WRITE(IUNIT,*,IOSTAT=ISTAT,ERR=1000)(INDXC(I),I=I1,I2)
 220  CONTINUE
      WRITE(IUNIT,'(A)',IOSTAT=ISTAT,ERR=1000)
     &              '# 3D status for each surface-surface connection:'
      I2=0
      IDO=INT((NUMSS-1)/10)+1
      DO 300 II=1,IDO
        I1=I2+1
        IF(II.EQ.IDO)THEN
          I2=NUMSS
        ELSE
          I2=I2+10
        ENDIF
        WRITE(IUNIT,*,IOSTAT=ISTAT,ERR=1000)(INDXSS(I),I=I1,I2)
 300  CONTINUE
      WRITE(IUNIT,'(A)',IOSTAT=ISTAT,ERR=1000)
     &              '# 3D status for each surface-edge connection:'
      I2=0
      IDO=INT((NUMSE-1)/10)+1
      DO 310 II=1,IDO
        I1=I2+1
        IF(II.EQ.IDO)THEN
          I2=NUMSE
        ELSE
          I2=I2+10
        ENDIF
        WRITE(IUNIT,*,IOSTAT=ISTAT,ERR=1000)(INDXSE(I),I=I1,I2)
 310  CONTINUE
      WRITE(IUNIT,'(A)',IOSTAT=ISTAT,ERR=1000)
     &              '# 3D status for each edge-edge connection:'
      I2=0
      IDO=INT((NUMEE-1)/10)+1
      DO 320 II=1,IDO
        I1=I2+1
        IF(II.EQ.IDO)THEN
          I2=NUMEE
        ELSE
          I2=I2+10
        ENDIF
        WRITE(IUNIT,*,IOSTAT=ISTAT,ERR=1000)(INDXEE(I),I=I1,I2)
 320  CONTINUE
      WRITE(IUNIT,'(A)',IOSTAT=ISTAT,ERR=1000)
     &              '# 3D status for each edge-corner connection:'
      I2=0
      IDO=INT((NUMEC-1)/10)+1
      DO 330 II=1,IDO
        I1=I2+1
        IF(II.EQ.IDO)THEN
          I2=NUMEC
        ELSE
          I2=I2+10
        ENDIF
        WRITE(IUNIT,*,IOSTAT=ISTAT,ERR=1000)(INDXEC(I),I=I1,I2)
 330  CONTINUE
      CALL ERPFREE(IUNIT,ISTAT)
      RETURN
1000  CALL USRMSG(' Error occured while writing ',
     &      ' to the 3D gridding configuration file ','W')
      CALL ERPFREE(IUNIT,ISTAT)
      RETURN
      END
