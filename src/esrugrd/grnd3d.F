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
C This file contains the following ground modilling subroutines:

C MENUGM displays the ground modelling menu.
C GNDBND displays the ground boundary menu.
C GNDMSH controls ground gridding.
C MKCVGZ performs gridding per ground layer.
C MKCVGY performs gridding per line of volumes within a ground layer.
C MKCVGX performs gridding per one material volume.
C SAVEGM saves the ground modelling configuration.
C READGM reads the ground modelling configuration.
C FHCGND draws a cross section (one layer) of ground.
C FHCBSF draws one of the six faces for ground.
C DRWGND draws the complete ground domain.
C *********************************************************************

C **************************    MENUGM    *****************************
C MENUGM displays the ground modelling menu.
C *********************************************************************
      SUBROUTINE MENUGM(IDRW1)
#include "building.h"

      COMMON/FILEP/IFIL
      COMMON/OUTIN/IUOUT,IUIN
      common/pophelp/h(60)
      COMMON/RAY3/MODIFY,MODLEN,MODBND
      COMMON/GRND05/NDXG,NDYG,NDZG,DXG(MGXYZ),DYG(MGXYZ),DZG(MGXYZ)
      COMMON/GRND06/ITPGND(MGXYZ,MGXYZ,MGXYZ)
      COMMON/GRND100/GRND3D
      COMMON/GRND108/LGDCVS,LGDCNC,LGDNDC,LGDTAQ,LGDNDD

      LOGICAL MODIFY,MODBND,MODLEN,OK,DOK,GRND3D,SAVEOK,XST,chdb
      DIMENSION LDRU(5)

      CHARACTER*72 LMTS,MSG,H,VALUES
      CHARACTER*72 LGDCVS,LGDCNC,LGDNDC,LGDNDD,LGDTAQ
      CHARACTER ITEM(16)*28

      MODIFY=.FALSE.
      SAVEOK=.FALSE.
      IUNIT=IFIL+1
      IL=1
      IDRW1=10
      IDRW2=0
      CALL GRAAPH(IDRW1,IDRW2)

C Display the menu.
   10 INO=-3
      WRITE(ITEM(1),'(A16,I2)')'1 mesh level >> ',IL
      ITEM(2)='  --------------------------'
      ITEM(3)='x define X - gridding       '
      ITEM(4)='y define Y - gridding       '
      ITEM(5)='z define Z - gridding       '
      ITEM(6)='b define surface boundary   '
      ITEM(7)='m create material rectangle '
      ITEM(8)='  --------------------------'
      ITEM(9)='2 Create 3D gridding files  '
      ITEM(10)='3 Delete 3D gridding files  '
      ITEM(11)='4 save 3D conf. into file   '
      ITEM(12)='5 read 3D conf. from file   '
      ITEM(13)='  --------------------------'
      ITEM(14)='s support facilities        '
      ITEM(15)='? help                      '
      ITEM(16)='- Exit                      '
      MITEM=16

C Help text for the menu.
      H(1)=' 3D ground models can be created via this menu.'
      H(2)=' First, the ground configuration should be defined.'
      H(3)=' This can be achieved by two methods. Firstly, menu'
      H(4)=' driven by (options x,y,z,b, and m). Secondly, by '
      H(5)=' loading a ground configuration file (option 5).'
      H(6)=' After that, the 3D ground files can be created by'
      H(7)=' selecting (option 2). To dis-activate the 3D ground'
      H(8)=' molling select (option 3).'
      H(9)=' The name of the 3D ground files are saved in the'
      H(10)=' system configuration file. For that, each time the'
      H(11)=' status or the files name is changed the '
      H(12)=' configuration file should be updated.'
 
C Draw the appropriate graph.
      IF(MODIFY)CALL GRAAPH(IDRW1,IDRW2)
      CALL USRMSG(' ',' ','-')
      CALL EMENU('   Ground Modelling',ITEM,MITEM,INO)
      IF(INO.EQ.1)THEN

C Show next mesh level.
        ILP=IL
        IL=IL+1
        IF(IL.GT.NDZG)IL=1
        IF(IL.NE.ILP)THEN
          IDRW1=11
          IDRW2=IL
          CALL GRAAPH(IDRW1,IDRW2)
        ENDIF
      ELSEIF(INO.EQ.3)THEN

C Define X - gridding.
        NDXT=1
        VALUES=' '
        ILAST=1
   15   IFRST=ILAST
        ILAST=ILAST+5
        IF(ILAST.GT.NDXG)ILAST=NDXG
        WRITE(VALUES,'(6(1X,F10.3))')(DXG(J),J=IFRST,ILAST)
        WRITE(MSG,'(2A,I2,A,I2,A)')'Define the X - gridding ',
     &    '(m) for intervals (',IFRST,') to (',(IFRST+5),').'
        h(1)='Data related to X gridding. '
        CALL EASKS(VALUES,MSG,' ',72,' ',' gridding intervals ',IER,1)
        CALL CHITMS(VALUES,NDX)
        IF(NDX.GT.6)THEN
          CALL USRMSG(' ',
     &             ' only six intervals are allowed at a time.','W')
          GOTO 10
        ENDIF
        NDXT=NDXT+NDX-1
        IF(NDXT.GT.MGXYZ)THEN
          CALL USRMSG(' ',' maximum allowable divisions exceeded.','W')
          GOTO 10
        ENDIF
        K=0
        DO 20 I=IFRST,NDXT
          CALL EGETWR(VALUES,K,DX,1.,1.E8,'F',
     &               'gridding intervals',IER)
          IF(IER.NE.0)GOTO 10
          DXG(I)=DX
   20   CONTINUE
        NDXG=NDXT
        IF(NDX.EQ.6.AND.NDXT.LT.MGXYZ)THEN
          ILAST=NDXT
          GOTO 15
        ENDIF
        IDRW1=11
        IDRW2=IL
        CALL GRAAPH(IDRW1,IDRW2)
      ELSEIF(INO.EQ.4)THEN

C Define Y - gridding.
        NDYT=1
        VALUES=' '
        ILAST=1
   25   IFRST=ILAST
        ILAST=ILAST+5
        IF(ILAST.GT.NDYG)ILAST=NDYG
        WRITE(VALUES,'(6(1X,F10.3))')(DYG(J),J=IFRST,ILAST)
        WRITE(MSG,'(2A,I2,A,I2,A)')'Define the Y - gridding ',
     &    '(m) for intervals (',IFRST,') to (',(IFRST+5),').'
        h(1)='Data related to Y gridding. '
        CALL EASKS(VALUES,MSG,' ',72,' ',' gridding intervals ',IER,1)
        CALL CHITMS(VALUES,NDY)
        IF(NDY.GT.6)THEN
          CALL USRMSG(' ',
     &             ' only six intervals are allowed at a time.','W')
          GOTO 10
        ENDIF
        NDYT=NDYT+NDY-1
        IF(NDYT.GT.MGXYZ)THEN
          CALL USRMSG(' ',' maximum allowable divisions exceeded.','W')
          GOTO 10
        ENDIF
        K=0
        DO 30 I=IFRST,NDYT
          CALL EGETWR(VALUES,K,DY,1.,1.E8,'F',
     &               'gridding intervals',IER)
          IF(IER.NE.0)GOTO 10
          DYG(I)=DY
   30   CONTINUE
        NDYG=NDYT
        IF(NDY.EQ.6.AND.NDYT.LT.MGXYZ)THEN
          ILAST=NDYT
          GOTO 25
        ENDIF
        IDRW1=11
        IDRW2=IL
        CALL GRAAPH(IDRW1,IDRW2)
      ELSEIF(INO.EQ.5)THEN

C Define Z - gridding.
        NDZT=1
        VALUES=' '
        ILAST=1
   35   IFRST=ILAST
        ILAST=ILAST+5
        IF(ILAST.GT.NDZG)ILAST=NDZG
        WRITE(VALUES,'(6(1X,F10.3))')(DZG(J),J=IFRST,ILAST)
        WRITE(MSG,'(2A,I2,A,I2,A)')'Define the Z - gridding ',
     &    '(m) for intervals (',IFRST,') to (',(IFRST+5),').'
        h(1)='Data related to Z gridding. '
        CALL EASKS(VALUES,MSG,' ',72,' ',' gridding intervals ',IER,1)
        CALL CHITMS(VALUES,NDZ)
        IF(NDZ.GT.6)THEN
          CALL USRMSG(' ',
     &             ' only six intervals are allowed at a time.','W')
          GOTO 10
        ENDIF
        NDZT=NDZT+NDZ-1
        IF(NDZT.GT.MGXYZ)THEN
          CALL USRMSG(' ',' maximum allowable divisions exceeded.','W')
          GOTO 10
        ENDIF
        K=0
        DO 40 I=IFRST,NDZT
          CALL EGETWR(VALUES,K,DZ,1.,1.E8,'F',
     &               'gridding intervals',IER)
          IF(IER.NE.0)GOTO 10
          DZG(I)=DZ
   40   CONTINUE
        NDZG=NDZT
        IF(NDZ.EQ.6.AND.NDZT.LT.MGXYZ)THEN
          ILAST=NDZT
          GOTO 35
        ENDIF
        IDRW1=11
        IDRW2=IL
        CALL GRAAPH(IDRW1,IDRW2)
      ELSEIF(INO.EQ.6)THEN

C Define surface boundary.
        CALL GNDBND(IDRW1)
      ELSEIF(INO.EQ.7)THEN

C Create a material rectangle.
        IF(IDRW1.NE.11)THEN
          IDRW1=10
          IDRW2=0
          CALL GRAAPH(IDRW1,IDRW2)
        ENDIF

C Ask if user wishes to browse through primitives to find suitable
C reference.
        dok=.true.
        h(1)='If you know the material db index you can say no and '
        h(2)='enter the number directly. Otherwise say yes and '
        h(3)='browse the database. '
        CALL ASKOK(' Browse material database to locate the material',
     &    ' index you want? A no will ask you for the index.',OK,dok,3)
        IF(OK)then
          iwhich=0
          CALL ELISTC(iwhich,chdb,'-',IER)
        else
          H(1)='Each item in the primitive construction db has a'
          H(2)='reference number. '
          H(3)='Air is reference 0.'
          CALL EASKI(IWHICH,' ',' Primitive db reference no: ',
     &        0,'F',600,'F',0,'primitive reference',IER,3)
        endif
        LDRU(1)=IWHICH
        WRITE(LMTS,'(4(1X,I2))')(LDRU(K),K=2,5)
        WRITE(MSG,'(2A)')'enter the material rectangle borders',
     &                      ' (left, down, right, and up)'
        h(1)='4 numbers are required. '
        CALL EASKS(LMTS,MSG,' ',32,' ',' rectangle limits ',IER,1)
        CALL CHITMS(LMTS,NUM)
        IF(NUM.NE.4)THEN
          CALL USRMSG(' ','four integers should be entered.','W')
          GOTO 10
        ENDIF
        K=0
        DO 50 L=2,5
          CALL EGETWI(LMTS,K,LDRU(L),0,100,'-',
     &                 'rectangle limits',IER)
   50   CONTINUE
        DO 60 IY=LDRU(3),LDRU(5)
          DO 70 IX=LDRU(2),LDRU(4)
            ITPGND(IX,IY,IL)=LDRU(1)
   70     CONTINUE
   60   CONTINUE
        IDRW1=11
        IDRW2=IL
        CALL GRAAPH(IDRW1,IDRW2)
      ELSEIF(INO.EQ.9)THEN

C Create 3D ground gridding files.
        CALL GNDMSH(IER)
        IF(IER.EQ.0)THEN
          GRND3D=.TRUE.
          SAVEOK=.TRUE.
        ENDIF
      ELSEIF(INO.EQ.10)THEN

C Delete 3D ground gridding files.
        IF(GRND3D)THEN
          CALL FINDFIL(LGDCVS,XST)
          IF(XST)THEN
            CALL EFOPRAN(IUNIT,LGDCVS,3,3,IER)
            CALL EFDELET(IUNIT,ISTAT)
          ENDIF
          CALL FINDFIL(LGDCNC,XST)
          IF(XST)THEN
            CALL EFOPRAN(IUNIT,LGDCNC,7,3,IER)
            CALL EFDELET(IUNIT,ISTAT)
          ENDIF
          CALL FINDFIL(LGDNDC,XST)
          IF(XST)THEN
            CALL EFOPRAN(IUNIT,LGDNDC,3,3,IER)
            CALL EFDELET(IUNIT,ISTAT)
          ENDIF
          CALL FINDFIL(LGDTAQ,XST)
          IF(XST)THEN
            CALL EFOPRAN(IUNIT,LGDTAQ,2,3,IER)
            CALL EFDELET(IUNIT,ISTAT)
          ENDIF
          GRND3D=.FALSE.
          SAVEOK=.TRUE.
        ELSE
          CALL USRMSG(' ',' 3D files do not exist','W')
        ENDIF
      ELSEIF(INO.EQ.11)THEN

C Save ground gridding configuration into a file.
        CALL SAVEGM

C Read ground gridding configuration from a file
      ELSEIF(INO.EQ.12)THEN
        CALL READGM
      ELSEIF(INO.EQ.(MITEM-2))THEN

C Support facilities.
        CALL SUPORT(-1)
      ELSEIF(INO.EQ.(MITEM-1))THEN

C Help.
        CALL PHELPD(' Multi-Gridding menu',12,'-',0,0,IER)
      ELSEIF(INO.EQ.MITEM)THEN

C Return to the main multi-gridding menu.
        IF(SAVEOK)THEN
          dok=.true.
          h(1)='Recent changes to the model might be lost if the '
          h(2)='data is not written to the configuration file. '
          h(3)='First check that no other ESP-r module is using '
          h(4)='this file. '
          CALL ASKOK(' ',
     &    'configuration should be updated. Ok to do this?',OK,dok,4)
          IF(OK)CALL EMKCFG('-',IER)
        ENDIF
        RETURN
      ENDIF
      GOTO 10
      END

C **************************    GNDBND    *****************************
C GNDBND displays the ground boundary menu.
C *********************************************************************
      SUBROUTINE GNDBND(IDRW1)
#include "building.h"
#include "epara.h"

      COMMON/OUTIN/IUOUT,IUIN
      COMMON/PMENU/MHEAD,MCTL,MIFULL,MFULL,IST,ILEN,IPM,MPM,IPFLG
      common/pophelp/h(60)
      COMMON/RAY3/MODIFY,MODLEN,MODBND
      COMMON/GRND05/NDXG,NDYG,NDZG,DXG(MGXYZ),DYG(MGXYZ),DZG(MGXYZ)
      COMMON/GRND10/NBOUND,IBOUND(MBTYP,3),IBSRF(6,MGXYZ,MGXYZ)

      LOGICAL MODIFY,MODBND,MODLEN

      DIMENSION LDRU(5),IVAL(MBTYP),IVALS(6)

      CHARACTER H*72,VALUES*32,MSG*72
      CHARACTER ITEM(14)*28,ITEMS(6)*18,VERT(35)*35,KEY*1

      IF(MODIFY.OR.IDRW1.NE.10)THEN
        IDRW1=10
        IDRW2=0
        CALL GRAAPH(IDRW1,IDRW2)
      ENDIF

C IVERT is the menu position, MVERT the current number of menu lines.
      IPACT=CREATE
      CALL EKPAGE(IPACT)

C Initial menu entry setup.
   10 IVERT=-3
      MHEAD=2
      MCTL=7
      ILEN=NBOUND
      VERT(1)='  | No |   associated data    '
      VERT(2)='  ---------------------------------' 

C Loop through the items until the page to be displayed. M is the 
C current menu line index. Build up text strings for the menu. 
      M=MHEAD
      DO 20 L=1,ILEN
        IF(L.GE.IST.AND.(L.LE.(IST+MIFULL)))THEN
          M=M+1
          CALL EMKEY(L,KEY,IER)
          WRITE(VERT(M),'(A1,3X,I2,3X,3I7)')KEY,L,(IBOUND(L,J),J=1,3)
        ENDIF
   20 CONTINUE

C Number of actual items displayed.
      MVERT=M+MCTL

C If a long list include page facility text.      
      IF(IPFLG.EQ.0)THEN
        VERT(M+1)='  ------------------------------'
      ELSE
        WRITE(VERT(M+1),'(A,I2,A,I2,A)')'0 Page ------- Part: ',IPM,
     &                                  ' of ',MPM,' -----'
      ENDIF
      VERT(M+2)  ='1 add new boundary type         '
      VERT(M+3)  ='2 delete existing boundary type '
      VERT(M+4)  ='3 define a boundary surface     '
      VERT(M+5)  ='  ------------------------------'
      VERT(M+6)  ='? Help                          '
      VERT(M+7)  ='- Exit                          '

C Draw the appropriate graph.
      IF(MODIFY)CALL GRAAPH(IDRW1,IDRW2)

C Help text for menu.
      H(1)=' By this menu the boundary types are defined for the'
      H(2)=' six surfaces of the ground 3D model. First, the'
      H(3)=' required boundary types should be created. Then,'
      H(4)=' they should be assigned to the required surfaces.'
      H(5)=' The default boundry is adiabatic. Hence, there is '
      H(6)=' no need to define these boundaries.'
      H(7)=' Within any surface, the boundary is defined per'
      H(8)=' increment. Therefore, different boundary types can'
      H(9)=' be defined for a single surface.'
      H(10)=' '
      H(11)=' The available connection types are:'
      H(12)=' 11  IZ  IN   with 3D node IN in zone IZ.'
      H(13)=' 21  IZ  IS   convective and radiative boundary '
      H(14)='              similar to exterior boundary for surface'
      H(15)='              IS in zone IZ.'
      H(16)=' 22  IZ  IS   convective only boundary similar to '
      H(17)='              exterier boundary for surface IS in '
      H(18)='              zone IZ.'
      H(19)=' 31  IZ  IS   convective and radiative boundary '
      H(20)='              similar to interior boundary for '
      H(21)='              surface IS in zone IZ.'
      H(22)=' 32  IZ  IS   convective only boundary similar to '
      H(23)='              interier boundary for surface IS in '
      H(24)='              zone IZ.'
      H(25)='>100 IS  IN   connected to 1D node in zone number'
      H(26)='              (type-100) surface IS and node IN.'
      H(27)=' NB for connection type (11), if IZ=-1, then '
      H(28)='    connection is with a node in the 3D ground model.'
      H(29)=' NB for connection type (31), the zone number must'
      H(30)='    be equal to the active zone.'

C Now display the menu.
      CALL USRMSG(' ',' ','-')
      CALL EMENU(' Boundary Types',VERT,MVERT,IVERT)

C Edit item identified by KEYIND.
      IF(IVERT.GT.MHEAD.AND.IVERT.LT.(MVERT-MCTL+1))THEN
        CALL KEYIND(MVERT,IVERT,IFOC,IO)
        WRITE(MSG,'(2A,I2,A)')'enter the associated data for ',
     &                   'boundary type number (',IFOC,').'
        VALUES=' '
        h(1)='<<to be completed>> '
        CALL EASKS(VALUES,MSG,' ',72,' ',' boundary type ',IER,1)
        CALL CHITMS(VALUES,NUM)
        IF(NUM.NE.3)THEN
          CALL USRMSG(' ','three integers should be entered.','W')
          GOTO 10
        ENDIF
        K=0
        DO 30 I=1,3
          CALL EGETWI(VALUES,K,IBOUND(IFOC,I),0,0,'-',
     &              ' boundary type ',IER)
   30   CONTINUE
      ELSEIF(IVERT.EQ.(MVERT-6).AND.IPFLG.EQ.1)THEN

C If there are enough items allow paging control via EKPAGE.
        IPACT=EDIT
        CALL EKPAGE(IPACT)
      ELSEIF(IVERT.EQ.(MVERT-5))THEN

C Add new boundary type.
        NMBR=NBOUND+1
        H(1)=' The available connection types are:'
        H(2)=' 11  IZ  IN   with 3D node IN in zone IZ.'
        H(3)=' 21  IZ  IS   convective and radiative boundary '
        H(4)='              similar to exterior boundary for surface'
        H(5)='              IS in zone IZ.'
        H(6)=' 22  IZ  IS   convective only boundary similar to '
        H(7)='              exterier boundary for surface IS in '
        H(8)='              zone IZ.'
        H(9)=' 31  IZ  IS   convective and radiative boundary '
        H(10)='              similar to interior boundary for '
        H(11)='              surface IS in zone IZ.'
        H(12)=' 32  IZ  IS   convective only boundary similar to '
        H(13)='              interier boundary for surface IS in '
        H(14)='              zone IZ.'
        H(15)='>100 IS  IN   connected to 1D node in zone number'
        H(16)='              (type-100) surface IS and node IN.'
        H(17)=' NB for connection type (11), if IZ=-1, then '
        H(18)='    connection is with a node in the 3D ground model.'
        H(19)=' NB for connection type (31), the zone number must'
        H(20)='    be equal to the active zone.'
        WRITE(MSG,'(2A,I2,A)')'  Enter the associated data for ',
     &                   'boundary type number (',NMBR,').'
        VALUES=' '
        CALL EASKS(VALUES,MSG,' ',72,' ',' boundary type ',IER,20)
        CALL CHITMS(VALUES,NUM)
        IF(NUM.NE.3)THEN
          CALL USRMSG(' ','three integers should be entered.','W')
          GOTO 10
        ENDIF
        K=0
        DO 40 I=1,3
          CALL EGETWI(VALUES,K,IBOUND(NMBR,I),0,0,'-',
     &              ' boundary type ',IER)
   40   CONTINUE
        NBOUND=NMBR
      ELSEIF(IVERT.EQ.(MVERT-4))THEN

C Delete existing boundary type.
        DO 50 I=1,NBOUND
          WRITE(ITEM(I),'(1X,I2,3X,3I7)')I,(IBOUND(I,J),J=1,3)
   50   CONTINUE
        INPK=1
   60   CALL EPICKS(INPK,IVAL,' ',' ',28,
     &              NBOUND,ITEM,'      DELETE ',IER,0)
        IBND=IVAL(1)
        IF(IBND.GT.0.AND.IBND.LE.NBOUND)THEN
          NBOUND=NBOUND-1
          DO 70 I=IBND,NBOUND
          DO 70 J=1,3
            IBOUND(I,J)=IBOUND(I+1,J)
   70     CONTINUE

C Update the boundary for all boundary surfaces.
          DO 80 I=1,NDXG
          DO 80 J=1,NDZG
            IF(IBSRF(1,I,J).GE.IBND)THEN
              IBSRF(1,I,J)=IBSRF(1,I,J)-1
              IF(IBSRF(1,I,J).LT.0)IBSRF(1,I,J)=0
            ENDIF
   80     CONTINUE
          DO 90 I=1,NDYG
          DO 90 J=1,NDZG
            IF(IBSRF(2,I,J).GE.IBND)THEN
              IBSRF(2,I,J)=IBSRF(2,I,J)-1
              IF(IBSRF(2,I,J).LT.0)IBSRF(2,I,J)=0
            ENDIF
   90     CONTINUE
          DO 100 I=1,NDXG
          DO 100 J=1,NDZG
            IF(IBSRF(3,I,J).GE.IBND)THEN
              IBSRF(3,I,J)=IBSRF(3,I,J)-1
              IF(IBSRF(3,I,J).LT.0)IBSRF(3,I,J)=0
            ENDIF
  100     CONTINUE
          DO 110 I=1,NDYG
          DO 110 J=1,NDZG
            IF(IBSRF(4,I,J).GE.IBND)THEN
              IBSRF(4,I,J)=IBSRF(4,I,J)-1
              IF(IBSRF(4,I,J).LT.0)IBSRF(4,I,J)=0
            ENDIF
  110     CONTINUE
          DO 120 I=1,NDXG
          DO 120 J=1,NDYG
            IF(IBSRF(5,I,J).GE.IBND)THEN
              IBSRF(5,I,J)=IBSRF(5,I,J)-1
              IF(IBSRF(5,I,J).LT.0)IBSRF(5,I,J)=0
            ENDIF
  120     CONTINUE
          DO 130 I=1,NDXG
          DO 130 J=1,NDYG
            IF(IBSRF(6,I,J).GE.IBND)THEN
              IBSRF(6,I,J)=IBSRF(6,I,J)-1
              IF(IBSRF(6,I,J).LT.0)IBSRF(6,I,J)=0
            ENDIF
  130     CONTINUE
        ELSEIF(IBND.EQ.0)THEN
          GOTO 10
        ELSE
          GOTO 60
        ENDIF
      ELSEIF(IVERT.EQ.(MVERT-3))THEN

C Define a boundary surface.
        IF(NBOUND.LT.1)THEN
          CALL USRMSG(' ','no boundary type is available.','W')
          GOTO 10
        ENDIF
        IF(IDRW1.NE.10)THEN
          IDRW1=10
          IDRW2=0
          CALL GRAAPH(IDRW1,IDRW2)
        ENDIF
        ITEMS(1)='surface (1 2 6 5)'
        ITEMS(2)='surface (2 3 7 6)'
        ITEMS(3)='surface (3 4 8 7)'
        ITEMS(4)='surface (4 1 5 8)'
        ITEMS(5)='surface (5 6 7 8)'
        ITEMS(6)='surface (4 3 2 1)'
        INPK=1
  140   WRITE(MSG,'(2A)')'select the surface within which ',
     &                      'the boudary surface is.'
        CALL EPICKS(INPK,IVALS,' ',MSG,18,6,ITEMS,'  Surfaces ',IER,0)
        ISRF=IVALS(1)
        IF(ISRF.GT.0.AND.ISRF.LE.6)THEN
          IDRW1=12
          IDRW2=ISRF
          CALL GRAAPH(IDRW1,IDRW2)
          WRITE(MSG,'(2A)')'enter the boundary type and borders',
     &                        ' (left, down, right, and up)'
          VALUES=' '
          h(1)='4 numbers are required. '
          CALL EASKS(VALUES,MSG,' ',72,' ',' boundary borders ',IER,1)
          CALL CHITMS(VALUES,NUM)
          IF(NUM.NE.5)THEN
            CALL USRMSG(' ','five integers should be entered.','W')
            GOTO 10
          ENDIF
          K=0
          DO 150 I=1,5
            CALL EGETWI(VALUES,K,LDRU(I),0,0,'-',' borders ',IER)
  150     CONTINUE
          IF(LDRU(1).GT.NBOUND.OR.LDRU(1).LT.0)THEN
            CALL USRMSG(' ',
     &          'The selected boundary type is not defined.','W')
            GOTO 10
          ENDIF
          LDRU(2)=MAX0(1,LDRU(2))
          LDRU(3)=MAX0(1,LDRU(3))
          LDRU(4)=MIN0(MGXYZ,LDRU(4))
          LDRU(5)=MIN0(MGXYZ,LDRU(5))
          IF(ISRF.EQ.1)THEN
            DO 160 I=LDRU(2),LDRU(4)
            DO 160 J=LDRU(3),LDRU(5)
              IBSRF(1,I,J)=LDRU(1)
  160       CONTINUE
          ELSEIF(ISRF.EQ.2)THEN
            DO 170 I=LDRU(2),LDRU(4)
            DO 170 J=LDRU(3),LDRU(5)
              IBSRF(2,I,J)=LDRU(1)
  170       CONTINUE
          ELSEIF(ISRF.EQ.3)THEN
            LSTT=(NDXG+1)-LDRU(4)
            LEND=(NDXG+1)-LDRU(2)
            DO 180 I=LSTT,LEND
            DO 180 J=LDRU(3),LDRU(5)
              IBSRF(3,I,J)=LDRU(1)
  180       CONTINUE
          ELSEIF(ISRF.EQ.4)THEN
            LSTT=(NDYG+1)-LDRU(4)
            LEND=(NDYG+1)-LDRU(2)
            DO 190 I=LSTT,LEND
            DO 190 J=LDRU(3),LDRU(5)
              IBSRF(4,I,J)=LDRU(1)
  190       CONTINUE
          ELSEIF(ISRF.EQ.5)THEN
            DO 200 I=LDRU(2),LDRU(4)
            DO 200 J=LDRU(3),LDRU(5)
              IBSRF(5,I,J)=LDRU(1)
  200       CONTINUE
          ELSE
            LSTT=(NDYG+1)-LDRU(5)
            LEND=(NDYG+1)-LDRU(3)
            DO 210 I=LDRU(2),LDRU(4)
            DO 210 J=LSTT,LEND
              IBSRF(6,I,J)=LDRU(1)
  210       CONTINUE
          ENDIF
          IDRW1=12
          IDRW2=ISRF
          CALL GRAAPH(IDRW1,IDRW2)
        ELSEIF(ISRF.EQ.0)THEN
          GOTO 10
        ELSE
          GOTO 140
        ENDIF
      ELSEIF(IVERT.EQ.(MVERT-1))THEN

C Display help messages.
        CALL PHELPD('connection section',30,'-',0,0,IER)
      ELSEIF(IVERT.EQ.MVERT)THEN

C Return to multi-gridding menu.
        RETURN
      ENDIF
      GOTO 10
      END

C ****************************    GNDMSH    ***************************
C GNDMSH controls ground gridding.
C *********************************************************************
      SUBROUTINE GNDMSH(IER)
#include "building.h"
#include "esprdbfile.h"

      COMMON/OUTIN/IUOUT,IUIN
      common/pophelp/h(60)
      COMMON/GRND05/NDXG,NDYG,NDZG,DXG(MGXYZ),DYG(MGXYZ),DZG(MGXYZ)
      COMMON/GRND06/ITPGND(MGXYZ,MGXYZ,MGXYZ)
      COMMON/GRND108/LGDCVS,LGDCNC,LGDNDC,LGDTAQ,LGDNDD
      COMMON/GR3D80/NNDS,NCNV,IR,ND1
      COMMON/GR3D110/ICVS,ICNC,INDC,INDD,ITAQ,ITLW,ILWV,ITF3

      DIMENSION IZZ(3),VZZ(5)

      CHARACTER    MSG*72,FLTYP*25
      CHARACTER*72 PNAM,TITL,H,LTMP
      CHARACTER*72 LGDCVS,LGDCNC,LGDNDC,LGDTAQ,LGDNDD

C Initialize the connections,nodes, and control volumes counters.
      NNDS=0
      NCNV=0
      IR=0

C Open the control volumes file.
      H(1)='   Enter the name for the file in which the associated'
      H(2)=' control volumes details to be saved. '
      H(3)=' '
      FLTYP='control volumes'
      LTMP= LGDCVS
      CALL FLNAME(-1,LTMP,FLTYP,'.cvs',3,IER)
      LGDCVS=LTMP
      CALL EFOPRAN(ICVS,LGDCVS,3,4,IER)
      IF(IER.NE.0)RETURN

C Open the connections file.
      H(2)=' connections details to be saved. '
      FLTYP='connections'
      LTMP=LGDCNC
      CALL FLNAME(-1,LTMP,FLTYP,'.cnc',3,IER)
      LGDCNC=LTMP
      CALL EFOPRAN(ICNC,LGDCNC,7,4,IER)
      IF(IER.NE.0)RETURN

C Create a binary file for saving the nodes data (location and the 
C numbers of the control volumes represented by each node).
      H(2)=' nodes global coordintes to be saved. '
      FLTYP='nodes coordintes'
      LTMP=LGDNDC
      CALL FLNAME(-1,LTMP,FLTYP,'.ndc',3,IER)
      LGDNDC=LTMP
      CALL EFOPRAN(INDC,LGDNDC,3,4,IER)
      IF(IER.NE.0)RETURN

C Create a binary file for saving the nodes data (location and the 
C numbers of the control volumes represented by each node).
      H(2)=' nodes initial temperature to be saved. '
      FLTYP='nodes temperature'
      LTMP=LGDTAQ
      CALL FLNAME(-1,LTMP,FLTYP,'.taq',3,IER)
      LGDTAQ=LTMP
      CALL EFOPRAN(ITAQ,LGDTAQ,2,4,IER)
      IF(IER.NE.0)RETURN

C Estimate the start up period for the ground.
      DNHCVL=0.
      RESISTM=1.E18
      DO 10 IX=1,NDXG
        DO 20 IY=1,NDYG
          RESIST=0.
          DO 30 IL=1,NDZG
            IEL=ITPGND(IX,IY,IL)
            CALL ERPCDB(IFMAT,IEL,IUOUT,IER,CND,DBDEN,
     &              DBSHT,E,A,DRV,TITL,PNAM)
            IF(IER.NE.0)STOP "error (801): while reading primative db."
            DNHCVL=DNHCVL+DXG(IX)*DYG(IY)*DZG(IL)*DBDEN*DBSHT
            RESIST=RESIST+DZG(IL)/CND
   30     CONTINUE
          RESISTM=AMIN1(RESISTM,RESIST)
   20   CONTINUE
   10 CONTINUE
      TDELX=0.
      TDELY=0.
      DO 40 IX=1,NDXG
        TDELX=TDELX+DXG(IX)
   40 CONTINUE
      DO 50 IY=1,NDYG
        TDELY=TDELY+DYG(IY)
   50 CONTINUE
      TCNSTGM=DNHCVL*RESISTM/(TDELX*TDELY)
      ITCNSTG=INT(TCNSTGM/(3600.*24.))+1
      IR=IR+1
      WRITE(ICNC,REC=IR,ERR=3)ITCNSTG,0.,0.,0.,0,0,0

C Create the nodes and control volumes and their associated connections.
      Z2=0.
      IZZ(1)=0
      VZZ(1)=0.
      VZZ(3)=0.
      VZZ(4)=0.
      VZZ(5)=0.
      IZZ(3)=0
      DO 100 IL=1,NDZG
        Z1=Z2
        Z2=Z2+DZG(IL)/4.
        IZZ(2)=IL
        VZZ(2)=Z2-Z1
        IZZ(3)=IZZ(3)+1
        CALL MKCVGZ(IZZ,VZZ)
        Z1=Z2
        Z2=Z2+DZG(IL)/2.
        IZZ(1)=IL
        VZZ(1)=(Z2-Z1)/2.
        VZZ(2)=VZZ(1)
        VZZ(4)=VZZ(3)
        VZZ(3)=(Z1+Z2)/2.
        VZZ(5)=Z1
        IZZ(3)=IZZ(3)+1
        CALL MKCVGZ(IZZ,VZZ)
        Z1=Z2
        Z2=Z2+DZG(IL)/4.
        VZZ(1)=Z2-Z1
        VZZ(4)=VZZ(3)
        VZZ(3)=Z2
        VZZ(5)=Z1
  100 CONTINUE
      IZZ(2)=0
      VZZ(2)=0.
      IZZ(3)=IZZ(3)+1
      CALL MKCVGZ(IZZ,VZZ)
      DO 500 ITQ=1,NNDS
        WRITE(ITAQ,REC=ITQ)0.0,15.0
  500 CONTINUE
      WRITE(ITAQ,REC=NNDS+1)0.0,0.0
      CALL EDISP(IUOUT,' Gridding process completed successfully.')
      CALL EDISP(IUOUT,' ')
      WRITE(MSG,'(A,I7)')' Total number of nodes:          ',NNDS
      CALL EDISP(IUOUT,MSG)
      WRITE(MSG,'(A,I7)')' Total number of control volumes:',NCNV
      CALL EDISP(IUOUT,MSG)
      WRITE(MSG,'(A,I7)')' Total number of connections:    ',IR
      CALL EDISP(IUOUT,MSG)
      CALL ERPFREE(ICVS,ISTAT)
      CALL ERPFREE(ICNC,ISTAT)
      CALL ERPFREE(INDC,ISTAT)
      CALL ERPFREE(ITAQ,ISTAT)
      RETURN
    3 STOP "error (802): while writing to the connections file."
      END

C *************************    MKCVGZ    ******************************
C MKCVGZ performs gridding per ground layer.
C *********************************************************************
      SUBROUTINE MKCVGZ(IZZ,VZZ)
#include "building.h"

      COMMON/GRND05/NDXG,NDYG,NDZG,DXG(MGXYZ),DYG(MGXYZ),DZG(MGXYZ)

      DIMENSION IYY(3),IZZ(3),VYY(5),VZZ(5)

      Y2=0.
      IYY(1)=0
      VYY(1)=0.
      VYY(3)=0.
      VYY(4)=0.
      VYY(5)=0.
      IYY(3)=0
      DO 10 IY=1,NDYG
        Y1=Y2
        Y2=Y2+DYG(IY)/4.
        IYY(2)=IY
        VYY(2)=Y2-Y1
        IYY(3)=IYY(3)+1
        CALL MKCVGY(IZZ,VZZ,IYY,VYY)
        Y1=Y2
        Y2=Y2+DYG(IY)/2.
        IYY(1)=IY
        VYY(1)=(Y2-Y1)/2.
        VYY(2)=VYY(1)
        VYY(4)=VYY(3)
        VYY(3)=(Y1+Y2)/2.
        VYY(5)=Y1
        IYY(3)=IYY(3)+1
        CALL MKCVGY(IZZ,VZZ,IYY,VYY)
        Y1=Y2
        Y2=Y2+DYG(IY)/4.
        VYY(1)=Y2-Y1
        VYY(4)=VYY(3)
        VYY(3)=Y2
        VYY(5)=Y1
   10 CONTINUE
      IYY(2)=0
      VYY(2)=0.
      IYY(3)=IYY(3)+1
      CALL MKCVGY(IZZ,VZZ,IYY,VYY)
      RETURN
      END

C *************************    MKCVGY    ******************************
C MKCVGY performs gridding per line of volumes within a ground layer.
C *********************************************************************
      SUBROUTINE MKCVGY(IZZ,VZZ,IYY,VYY)
#include "building.h"

      COMMON/GRND05/NDXG,NDYG,NDZG,DXG(MGXYZ),DYG(MGXYZ),DZG(MGXYZ)

      DIMENSION IXX(3),IYY(3),IZZ(3),VXX(5),VYY(5),VZZ(5)

      X2=0.
      IXX(1)=0
      VXX(1)=0.
      VXX(3)=0.
      VXX(4)=0.
      VXX(5)=0.
      IXX(3)=0
      DO 10 IX=1,NDXG
        X1=X2
        X2=X2+DXG(IX)/4.
        IXX(2)=IX
        VXX(2)=X2-X1
        IXX(3)=IXX(3)+1
        CALL MKCVGX(IZZ,VZZ,IYY,VYY,IXX,VXX)
        X1=X2
        X2=X2+DXG(IX)/2.
        IXX(1)=IX
        VXX(1)=(X2-X1)/2.
        VXX(2)=VXX(1)
        VXX(4)=VXX(3)
        VXX(3)=(X1+X2)/2.
        VXX(5)=X1
        IXX(3)=IXX(3)+1
        CALL MKCVGX(IZZ,VZZ,IYY,VYY,IXX,VXX)
        X1=X2
        X2=X2+DXG(IX)/4.
        VXX(1)=X2-X1
        VXX(4)=VXX(3)
        VXX(3)=X2
        VXX(5)=X1
   10 CONTINUE
      IXX(2)=0
      VXX(2)=0.
      IXX(3)=IXX(3)+1
      CALL MKCVGX(IZZ,VZZ,IYY,VYY,IXX,VXX)
      RETURN
      END

C *****************************    MKCVGX    **************************
C MKCVGX performs gridding per one material volume.
C *********************************************************************
      SUBROUTINE MKCVGX(IZZ,VZZ,IYY,VYY,IXX,VXX)
#include "building.h"
#include "esprdbfile.h"

      COMMON/OUTIN/IUOUT,IUIN
      COMMON/GRND05/NDXG,NDYG,NDZG,DXG(MGXYZ),DYG(MGXYZ),DZG(MGXYZ)
      COMMON/GRND06/ITPGND(MGXYZ,MGXYZ,MGXYZ)
      COMMON/GR3D80/NNDS,NCNV,IR,ND1
      COMMON/GR3D110/ICVS,ICNC,INDC,INDD,ITAQ,ITLW,ILWV,ITF3
      COMMON/GRND10/NBOUND,IBOUND(MBTYP,3),IBSRF(6,MGXYZ,MGXYZ)

      DIMENSION IXX(3),IYY(3),IZZ(3),VXX(5),VYY(5),VZZ(5)
      DIMENSION ACOND(3)

      CHARACTER PNAM*72,TITL*72

      DNHC=0.
      DO 10 II=1,3
        ACOND(II)=0.
   10 CONTINUE
      NDY=2*NDXG+1
      NDZ=NDY*(2*NDYG+1)
      DELX=VXX(1)+VXX(2)
      DELY=VYY(1)+VYY(2)
      DELZ=VZZ(1)+VZZ(2)
      AREAX=DELY*DELZ
      AREAY=DELX*DELZ
      AREAZ=DELX*DELY
      VOLM=DELX*DELY*DELZ
      DO 20 I1=1,2
      DO 20 I2=1,2
      DO 20 I3=1,2
      IF(IXX(I1).GT.0.AND.IYY(I2).GT.0.AND.IZZ(I3).GT.0)THEN
        IEL=ITPGND(IXX(I1),IYY(I2),IZZ(I3))
        CALL ERPCDB(IFMAT,IEL,IUOUT,IER,CND,DBDEN,
     &              DBSHT,E,A,DRV,TITL,PNAM)
        IF(IER.NE.0)STOP "error (803): while reading primative db."
        DNHC=DNHC+VXX(I1)*VYY(I2)*VZZ(I3)*DBDEN*DBSHT

C LEFT (RIGHT).
        IF((IXX(1).GT.0.AND.I1.EQ.1).OR.(IXX(1).EQ.0.AND.I1.EQ.2))THEN
          ACOND(1)=ACOND(1)+CND*VYY(I2)*VZZ(I3)
        ENDIF

C BEHIND (FRONT).
        IF((IYY(1).GT.0.AND.I2.EQ.1).OR.(IYY(1).EQ.0.AND.I2.EQ.2))THEN
          ACOND(2)=ACOND(2)+CND*VXX(I1)*VZZ(I3)
        ENDIF

C DOWN (UP).
        IF((IZZ(1).GT.0.AND.I3.EQ.1).OR.(IZZ(1).EQ.0.AND.I3.EQ.2))THEN
          ACOND(3)=ACOND(3)+CND*VXX(I1)*VYY(I2)
        ENDIF
      ENDIF
   20 CONTINUE
      DNHC=DNHC/VOLM
      ACOND(1)=ACOND(1)/AREAX
      ACOND(2)=ACOND(2)/AREAY
      ACOND(3)=ACOND(3)/AREAZ

C Create the control volume and node.
      NCNV=NCNV+1
      NNDS=NNDS+1
      ND1=NNDS
      WRITE(ICVS,REC=NCNV,ERR=1)NNDS,VOLM,DNHC
      WRITE(INDC,REC=NNDS,ERR=2)VXX(3),VYY(3),VZZ(3)

C Create the connection.
C LEFT.
      IF(IXX(1).EQ.0)THEN
        DIST=1.
        COND=1000.
        DO 30 I2=1,2
        DO 30 I3=1,2
          IF(IYY(I2).GT.0.AND.IZZ(I3).GT.0)THEN
            AREC=VYY(I2)*VZZ(I3)
            IBND=IBSRF(4,IYY(I2),IZZ(I3))
            IF(IBND.GT.NBOUND.OR.IBND.LT.0)THEN
              STOP "error (804): un-defined boundary type is refered."
            ELSEIF(IBND.GT.0)THEN
              ITYP=IBOUND(IBND,1)
              I1C=IBOUND(IBND,2)
              I2C=IBOUND(IBND,3)
              IR=IR+1
              WRITE(ICNC,REC=IR,ERR=3)ND1,DIST,AREC,COND,ITYP,I1C,I2C
            ENDIF
          ENDIF
   30   CONTINUE
      ELSE
        N2=ND1-1
        DIST=VXX(3)-VXX(4)
        AREC=AREAX
        COND=ACOND(1)
        ITYP=11
        IR=IR+1
        WRITE(ICNC,REC=IR,ERR=3)ND1,DIST,AREC,COND,ITYP,-1,N2
      ENDIF

C RIGHT.
      IF(IXX(2).EQ.0)THEN
        DIST=1.
        COND=1000.
        DO 40 I2=1,2
        DO 40 I3=1,2
          IF(IYY(I2).GT.0.AND.IZZ(I3).GT.0)THEN
            AREC=VYY(I2)*VZZ(I3)
            IBND=IBSRF(2,IYY(I2),IZZ(I3))
            IF(IBND.GT.NBOUND.OR.IBND.LT.0)THEN
              STOP "error (805): un-defined boundary type is refered."
            ELSEIF(IBND.GT.0)THEN
              ITYP=IBOUND(IBND,1)
              I1C=IBOUND(IBND,2)
              I2C=IBOUND(IBND,3)
              IR=IR+1
              WRITE(ICNC,REC=IR,ERR=3)ND1,DIST,AREC,COND,ITYP,I1C,I2C
            ENDIF
          ENDIF
   40   CONTINUE
      ENDIF

C BEHIND.
      IF(IYY(1).EQ.0)THEN
        DIST=1.
        COND=1000.
        DO 50 I1=1,2
        DO 50 I3=1,2
          IF(IXX(I1).GT.0.AND.IZZ(I3).GT.0)THEN
            AREC=VXX(I1)*VZZ(I3)
            IBND=IBSRF(1,IXX(I1),IZZ(I3))
            IF(IBND.GT.NBOUND.OR.IBND.LT.0)THEN
              STOP "error (806): un-defined boundary type is refered."
            ELSEIF(IBND.GT.0)THEN
              ITYP=IBOUND(IBND,1)
              I1C=IBOUND(IBND,2)
              I2C=IBOUND(IBND,3)
              IR=IR+1
              WRITE(ICNC,REC=IR,ERR=3)ND1,DIST,AREC,COND,ITYP,I1C,I2C 
            ENDIF
          ENDIF
   50   CONTINUE
      ELSE
        N2=NNDS-NDY
        DIST=VYY(3)-VYY(4)
        AREC=DELX*DELZ
        COND=ACOND(2)
        ITYP=11
        IR=IR+1
        WRITE(ICNC,REC=IR,ERR=3)NNDS,DIST,AREC,COND,ITYP,-1,N2
      ENDIF

C FRONT.
      IF(IYY(2).EQ.0)THEN
        DIST=1.
        COND=1000.
        DO 60 I1=1,2
        DO 60 I3=1,2
          IF(IXX(I1).GT.0.AND.IZZ(I3).GT.0)THEN
            AREC=VXX(I1)*VZZ(I3)
            IBND=IBSRF(3,IXX(I1),IZZ(I3))
            IF(IBND.GT.NBOUND.OR.IBND.LT.0)THEN
              STOP "error (807): un-defined boundary type is refered."
            ELSEIF(IBND.GT.0)THEN
              ITYP=IBOUND(IBND,1)
              I1C=IBOUND(IBND,2)
              I2C=IBOUND(IBND,3)
              IR=IR+1
              WRITE(ICNC,REC=IR,ERR=3)ND1,DIST,AREC,COND,ITYP,I1C,I2C
            ENDIF
          ENDIF
   60   CONTINUE
      ENDIF

C DOWN.
      IF(IZZ(1).EQ.0)THEN
        DIST=1.
        COND=1000.
        DO 70 I1=1,2
        DO 70 I2=1,2
          IF(IXX(I1).GT.0.AND.IYY(I2).GT.0)THEN
            AREC=VXX(I1)*VYY(I2)
            IBND=IBSRF(6,IXX(I1),IYY(I2))
            IF(IBND.GT.NBOUND.OR.IBND.LT.0)THEN
              STOP "error (808): un-defined boundary type is refered."
            ELSEIF(IBND.GT.0)THEN
              ITYP=IBOUND(IBND,1)
              I1C=IBOUND(IBND,2)
              I2C=IBOUND(IBND,3)
              IR=IR+1
              WRITE(ICNC,REC=IR,ERR=3)ND1,DIST,AREC,COND,ITYP,I1C,I2C
            ENDIF
          ENDIF
   70   CONTINUE
      ELSE
        N2=NNDS-NDZ
        DIST=VZZ(3)-VZZ(4)
        AREC=DELX*DELY
        COND=ACOND(3)
        ITYP=11
        IR=IR+1
        WRITE(ICNC,REC=IR,ERR=3)NNDS,DIST,AREC,COND,ITYP,-1,N2
      ENDIF

C UP.
      IF(IZZ(2).EQ.0)THEN
        DIST=1.
        COND=1000.
        DO 80 I1=1,2
        DO 80 I2=1,2
          IF(IXX(I1).GT.0.AND.IYY(I2).GT.0)THEN
            AREC=VXX(I1)*VYY(I2)
            IBND=IBSRF(5,IXX(I1),IYY(I2))
            IF(IBND.GT.NBOUND.OR.IBND.LT.0)THEN
              STOP "error (809): un-defined boundary type is refered."
            ELSEIF(IBND.GT.0)THEN
              ITYP=IBOUND(IBND,1)
              I1C=IBOUND(IBND,2)
              I2C=IBOUND(IBND,3)
              IR=IR+1
              WRITE(ICNC,REC=IR,ERR=3)ND1,DIST,AREC,COND,ITYP,I1C,I2C
            ENDIF
          ENDIF
   80   CONTINUE
      ENDIF
      RETURN
    1 STOP "error (810): while writing to the control volumes file."
    2 STOP "error (811): while writing to the nodes temporary file."
    3 STOP "error (812): while writing to the connections file."
      END

C *****************************    SAVEGM    **************************
C SAVEGM saves the ground modelling configuration.
C *********************************************************************
      SUBROUTINE SAVEGM
#include "building.h"

      COMMON/FILEP/IFIL
      COMMON/OUTIN/IUOUT,IUIN
      common/pophelp/h(60)
      COMMON/GRND05/NDXG,NDYG,NDZG,DXG(MGXYZ),DYG(MGXYZ),DZG(MGXYZ)
      COMMON/GRND06/ITPGND(MGXYZ,MGXYZ,MGXYZ)
      COMMON/GRND10/NBOUND,IBOUND(MBTYP,3),IBSRF(6,MGXYZ,MGXYZ)

      CHARACTER*72 FILGND,h*72

      FILGND='ground.c3d'
      h(1)='The ground configuration file << ... >> '
      CALL EASKS(FILGND,' 3D ground configuration file name ?',' ',
     &       72,' ',' 3D ground file ',IER,1)
      IUNIT=IFIL+1
      CALL EFOPSEQ(IUNIT,FILGND,4,IER)
      IF(IER.LT.0)RETURN
      WRITE(IUNIT,'(I6,10X,A)')NDXG,'# number of X - intervals.'
      JEND=0
   20 JSTT=JEND+1
      JEND=JSTT+4
      IF(JEND.GT.NDXG)JEND=NDXG
      WRITE(IUNIT,'(5(1X,F10.3))')(DXG(J),J=JSTT,JEND)
      IF(NDXG.GT.JEND)GOTO 20
      WRITE(IUNIT,'(I6,10X,A)')NDYG,'# number of Y - intervals.'
      JEND=0
   30 JSTT=JEND+1
      JEND=JSTT+4
      IF(JEND.GT.NDYG)JEND=NDYG
      WRITE(IUNIT,'(5(1X,F10.3))')(DYG(J),J=JSTT,JEND)
      IF(NDYG.GT.JEND)GOTO 30
      WRITE(IUNIT,'(I6,10X,A)')NDZG,'# number of Z - intervals.'
      JEND=0
   40 JSTT=JEND+1
      JEND=JSTT+4
      IF(JEND.GT.NDZG)JEND=NDZG
      WRITE(IUNIT,'(5(1X,F10.3))')(DZG(J),J=JSTT,JEND)
      IF(NDZG.GT.JEND)GOTO 40
      DO 50 IL=1,NDZG
        WRITE(IUNIT,'(A)')'# thermal property type for each volume.'
        WRITE(IUNIT,'(I3,10X,A)')IL,'# level No.'
      DO 50 IY=NDYG,1,-1
        WRITE(IUNIT,'(20I5)')(ITPGND(IX,IY,IL),IX=1,NDXG)
   50 CONTINUE
      WRITE(IUNIT,'(I6,10X,A)')NBOUND,'# number of boundary types.'
      DO 60 ITYP=1,NBOUND
        WRITE(IUNIT,'(4I6)')ITYP,(IBOUND(ITYP,J),J=1,3)
   60 CONTINUE
      WRITE(IUNIT,'(A)')'# boundary types for boundary surface (1)'
      DO 70 IL=NDZG,1,-1
        WRITE(IUNIT,'(20I5)')(IBSRF(1,IX,IL),IX=1,NDXG)
   70 CONTINUE
      WRITE(IUNIT,'(A)')'# boundary types for boundary surface (2)'
      DO 80 IL=NDZG,1,-1
        WRITE(IUNIT,'(20I5)')(IBSRF(2,IY,IL),IY=1,NDYG)
   80 CONTINUE
      WRITE(IUNIT,'(A)')'# boundary types for boundary surface (3)'
      DO 90 IL=NDZG,1,-1
        WRITE(IUNIT,'(20I5)')(IBSRF(3,IX,IL),IX=NDXG,1,-1)
   90 CONTINUE
      WRITE(IUNIT,'(A)')'# boundary types for boundary surface (4)'
      DO 100 IL=NDZG,1,-1
        WRITE(IUNIT,'(20I5)')(IBSRF(4,IY,IL),IY=NDYG,1,-1)
  100 CONTINUE
      WRITE(IUNIT,'(A)')'# boundary types for boundary surface (5)'
      DO 110 IY=NDYG,1,-1
        WRITE(IUNIT,'(20I5)')(IBSRF(5,IX,IY),IX=1,NDXG)
  110 CONTINUE
      WRITE(IUNIT,'(A)')'# boundary types for boundary surface (6)'
      DO 120 IY=1,NDYG
        WRITE(IUNIT,'(20I5)')(IBSRF(6,IX,IY),IX=1,NDXG)
  120 CONTINUE
      CALL ERPFREE(IUNIT,ISTAT)
      RETURN
      END

C *************************    READGM    ******************************
C READGM reads the ground modelling configuration.
C *********************************************************************
      SUBROUTINE READGM
#include "building.h"

      COMMON/FILEP/IFIL
      common/pophelp/h(60)
      COMMON/OUTIN/IUOUT,IUIN
      COMMON/GRND05/NDXG,NDYG,NDZG,DXG(MGXYZ),DYG(MGXYZ),DZG(MGXYZ)
      COMMON/GRND06/ITPGND(MGXYZ,MGXYZ,MGXYZ)
      COMMON/GRND10/NBOUND,IBOUND(MBTYP,3),IBSRF(6,MGXYZ,MGXYZ)

      CHARACTER FILGND*72,STRG*124,H*72

      FILGND='ground.c3d'
      h(1)='The 3D ground configuration file << ... >> '
      CALL EASKS(FILGND,' 3D ground configuration file name ?',' ',
     &       72,' ',' 3D ground file ',IER,1)
      IUNIT=IFIL+1
      CALL EFOPSEQ(IUNIT,FILGND,1,IER)
      IF(IER.LT.0)RETURN
      CALL STRIPC(IUNIT,STRG,1,ND,1,' No. of X - intervals ',IER)
      K=0
      CALL EGETWI(STRG,K,NDXG,1,MGXYZ,'F',' No. of DX ',IER)
      NDO=INT(NDXG/5)
      NCHCK=NDO*5
      IF(NCHCK.LT.NDXG)NDO=NDO+1
      IBR2=0
      DO 10 IDO=1,NDO
        IF(IDO.EQ.NDO)THEN
          NBR=NDXG-5*(NDO-1)
        ELSE
          NBR=5
        ENDIF
        IBR1=IBR2+1
        IBR2=IBR2+NBR
        CALL STRIPC(IUNIT,STRG,NBR,ND,1,' DX values ',IER)
        K=0
      DO 10 IX=IBR1,IBR2
        CALL EGETWR(STRG,K,DXG(IX),0.,1.E10,'F',' DX ',IER)
   10 CONTINUE
      CALL STRIPC(IUNIT,STRG,1,ND,1,' No. of Y - intervals ',IER)
      K=0
      CALL EGETWI(STRG,K,NDYG,1,MGXYZ,'F',' No. of DY ',IER)
      NDO=INT(NDYG/5)
      NCHCK=NDO*5
      IF(NCHCK.LT.NDYG)NDO=NDO+1
      IBR2=0
      DO 20 IDO=1,NDO
        IF(IDO.EQ.NDO)THEN
          NBR=NDYG-5*(NDO-1)
        ELSE
          NBR=5
        ENDIF
        IBR1=IBR2+1
        IBR2=IBR2+NBR
        CALL STRIPC(IUNIT,STRG,NBR,ND,1,' DY values ',IER)
        K=0
      DO 20 IY=IBR1,IBR2
        CALL EGETWR(STRG,K,DYG(IY),0.,1.E10,'F',' DY ',IER)
   20 CONTINUE
      CALL STRIPC(IUNIT,STRG,1,ND,1,' No. of Z - intervals ',IER)
      K=0
      CALL EGETWI(STRG,K,NDZG,1,MGXYZ,'F',' No. of DZ ',IER)
      NDO=INT(NDZG/5)
      NCHCK=NDO*5
      IF(NCHCK.LT.NDZG)NDO=NDO+1
      IBR2=0
      DO 30 IDO=1,NDO
        IF(IDO.EQ.NDO)THEN
          NBR=NDZG-5*(NDO-1)
        ELSE
          NBR=5
        ENDIF
        IBR1=IBR2+1
        IBR2=IBR2+NBR
        CALL STRIPC(IUNIT,STRG,NBR,ND,1,' DZ values ',IER)
        K=0
      DO 30 IL=IBR1,IBR2
        CALL EGETWR(STRG,K,DZG(IL),0.,1.E10,'F',' DZ ',IER)
   30 CONTINUE
      DO 40 IL=1,NDZG
        CALL STRIPC(IUNIT,STRG,1,ND,1,' level No. ',IER)
        K=0
        CALL EGETWI(STRG,K,LVL,IL,IL,'F',' No. of DZ ',IER)
      DO 40 IY=NDYG,1,-1
        CALL STRIPC(IUNIT,STRG,NDXG,ND,1,' thermal property ',IER)
        K=0
      DO 40 IX=1,NDXG
        CALL EGETWI(STRG,K,ITP,1,10000,'F','thermal property',IER)
        ITPGND(IX,IY,IL)=ITP
   40 CONTINUE
      CALL STRIPC(IUNIT,STRG,1,ND,1,' No. of boundary types ',IER)
      K=0
      CALL EGETWI(STRG,K,NBOUND,1,MBTYP,'F',' No. of types ',IER)
      DO 50 IBT=1,NBOUND
        CALL STRIPC(IUNIT,STRG,4,ND,1,' boundary type ',IER)
        K=0
        CALL EGETWI(STRG,K,NUM,IBT,IBT,'F',' type No. ',IER)
      DO 50 J=1,3
        CALL EGETWI(STRG,K,IBOUND(IBT,J),0,0,'-','  data ',IER)
   50 CONTINUE
      DO 60 IL=NDZG,1,-1
        CALL STRIPC(IUNIT,STRG,NDXG,ND,1,' surface (1) ',IER)
        K=0
      DO 60 IX=1,NDXG
        CALL EGETWI(STRG,K,NUM,0,NBOUND,'F',' type No. ',IER)
        IBSRF(1,IX,IL)=NUM
   60 CONTINUE
      DO 70 IL=NDZG,1,-1
        CALL STRIPC(IUNIT,STRG,NDYG,ND,1,' surface (2) ',IER)
        K=0
      DO 70 IY=1,NDYG
        CALL EGETWI(STRG,K,NUM,0,NBOUND,'F',' type No. ',IER)
        IBSRF(2,IY,IL)=NUM
   70 CONTINUE
      DO 80 IL=NDZG,1,-1
        CALL STRIPC(IUNIT,STRG,NDXG,ND,1,' surface (3) ',IER)
        K=0
      DO 80 IX=NDXG,1,-1
        CALL EGETWI(STRG,K,NUM,0,NBOUND,'F',' type No. ',IER)
        IBSRF(3,IX,IL)=NUM
   80 CONTINUE
      DO 90 IL=NDZG,1,-1
        CALL STRIPC(IUNIT,STRG,NDYG,ND,1,' surface (4) ',IER)
        K=0
      DO 90 IY=NDYG,1,-1
        CALL EGETWI(STRG,K,NUM,0,NBOUND,'F',' type No. ',IER)
        IBSRF(4,IY,IL)=NUM
   90 CONTINUE
      DO 100 IY=NDYG,1,-1
        CALL STRIPC(IUNIT,STRG,NDXG,ND,1,' surface (5) ',IER)
        K=0
      DO 100 IX=1,NDXG
        CALL EGETWI(STRG,K,NUM,0,NBOUND,'F',' type No. ',IER)
        IBSRF(5,IX,IY)=NUM
  100 CONTINUE
      DO 110 IY=1,NDYG
        CALL STRIPC(IUNIT,STRG,NDXG,ND,1,' surface (6) ',IER)
        K=0
      DO 110 IX=1,NDXG
        CALL EGETWI(STRG,K,NUM,0,NBOUND,'F',' type No. ',IER)
        IBSRF(6,IX,IY)=NUM
  110 CONTINUE
      CALL ERPFREE(IUNIT,ISTAT)
      RETURN
      END

C ****************************    FHCGND    ***************************
C FHCGND draws a cross section (one layer) of ground.
C *********************************************************************
      SUBROUTINE FHCGND(IL)
#include "building.h"

      COMMON/GFONT/IFS,ITFS,IMFS
      COMMON/VIEWPX/menuchw,igl,igr,igt,igb,igw,igh

      COMMON/GRND05/NDXG,NDYG,NDZG,DXG(MGXYZ),DYG(MGXYZ),DZG(MGXYZ)
      COMMON/GRND06/ITPGND(MGXYZ,MGXYZ,MGXYZ)

      CHARACTER LABELS*16

      XMN2=0.
      YMN2=0.
      XMX2=FLOAT(NDXG)
      YMX2=FLOAT(NDYG)
      XYMX=AMAX1(XMX2,YMX2)
      CALL startbuffer
      CALL WIN3D(28,10,10,5,3,IGL,IGR,IGT,IGB,IGW,IGWH)
      CALL AXISCALE(IGW,IGWH,XMN2,XMX2,YMN2,YMX2,XSC,YSC,SCA,XADD,YADD)
      CALL LINESCALE(IGL,XADD,SCA,IGB,YADD,SCA)
      CALL U2PIXEL(XMX2,YMX2,IX,IY)
      CALL ELINE(IX,IY,1)
      CALL ECIRC(IX,IY,3,1)
      CALL U2PIXEL(XMX2,YMN2,IX,IY)
      CALL ELINE(IX,IY,0)
      CALL ECIRC(IX,IY,3,1)
      CALL U2PIXEL(XMN2,YMN2,IX,IY)
      CALL ELINE(IX,IY,0)
      CALL ECIRC(IX,IY,3,1)
      CALL U2PIXEL(XMN2,YMX2,IX,IY)
      CALL ELINE(IX,IY,0)
      CALL ECIRC(IX,IY,3,1)
      CALL U2PIXEL(XMX2,YMX2,IX,IY)
      CALL ELINE(IX,IY,0)
      DO 10 I=1,NDXG-1
        CALL U2PIXEL(FLOAT(I),YMN2,IX1,IY1)
        CALL U2PIXEL(FLOAT(I),YMX2,IX2,IY2)
        CALL EDLINE(IX1,IY1,IX2,IY2,4)
   10 CONTINUE
      DO 20 J=1,NDYG-1
        CALL U2PIXEL(XMN2,FLOAT(J),IX1,IY1)
        CALL U2PIXEL(XMX2,FLOAT(J),IX2,IY2)
        CALL EDLINE(IX1,IY1,IX2,IY2,4)
   20 CONTINUE
      XCF=-0.5
      DO 30 I=1,NDXG
        XCF=XCF+1.
        YCF=-0.5
        DO 40 J=1,NDYG
          YCF=YCF+1.
          CALL U2PIXEL(XCF,YCF,IX,IY)
          WRITE(LABELS,'(I3)')ITPGND(I,J,IL)
          CALL ETLABEL(LABELS,XCF,YCF,0,ITFS)
   40   CONTINUE
   30 CONTINUE
      CALL VIEWTEXT('Construction Details',1,1,2)
      XLAB=XMN2-XYMX/20.
      YLAB=YMN2-XYMX/20.
      CALL ETLABEL('1',XLAB,YLAB,4,ITFS)
      XLAB=XMX2+XYMX/20.
      YLAB=YMN2-XYMX/20.
      CALL ETLABEL('2',XLAB,YLAB,4,ITFS)
      XLAB=XMX2+XYMX/20.
      YLAB=YMX2+XYMX/20.
      CALL ETLABEL('3',XLAB,YLAB,4,ITFS)
      XLAB=XMN2-XYMX/20.
      YLAB=YMX2+XYMX/20.
      CALL ETLABEL('4',XLAB,YLAB,4,ITFS)
      call forceflush()
      RETURN
      END

C ****************************    FHCBSF    ***************************
C FHCBSF draws one of the six faces for ground.
C *********************************************************************
      SUBROUTINE FHCBSF(ISRF)
#include "building.h"

      COMMON/GFONT/IFS,ITFS,IMFS
      COMMON/VIEWPX/menuchw,igl,igr,igt,igb,igw,igh

      COMMON/GRND05/NDXG,NDYG,NDZG,DXG(MGXYZ),DYG(MGXYZ),DZG(MGXYZ)
      COMMON/GRND07/VERTX(6,4)
      COMMON/GRND10/NBOUND,IBOUND(MBTYP,3),IBSRF(6,MGXYZ,MGXYZ)

      CHARACTER LABELS*4,VERTX*2

      IF(ISRF.EQ.1.OR.ISRF.EQ.3.OR.ISRF.GE.5)THEN
        NUMX=NDXG
      ELSE
        NUMX=NDYG
      ENDIF
      IF(ISRF.GE.1.AND.ISRF.LE.4)THEN
        NUMY=NDZG
      ELSE
        NUMY=NDYG
      ENDIF
      XMN2=0.
      YMN2=0.
      XMX2=FLOAT(NUMX)
      YMX2=FLOAT(NUMY)
      XYMX=AMAX1(XMX2,YMX2)
      CALL startbuffer()
      CALL WIN3D(33,10,10,5,3,IGL,IGR,IGT,IGB,IGW,IGWH)
      CALL AXISCALE(IGW,IGWH,XMN2,XMX2,YMN2,YMX2,XSC,YSC,SCA,XADD,YADD)
      CALL LINESCALE(IGL,XADD,SCA,IGB,YADD,SCA)
      CALL U2PIXEL(XMN2,YMN2,IX,IY)
      CALL ELINE(IX,IY,1)
      CALL ECIRC(IX,IY,3,1)
      CALL U2PIXEL(XMX2,YMN2,IX,IY)
      CALL ELINE(IX,IY,0)
      CALL ECIRC(IX,IY,3,1)
      CALL U2PIXEL(XMX2,YMX2,IX,IY)
      CALL ELINE(IX,IY,0)
      CALL ECIRC(IX,IY,3,1)
      CALL U2PIXEL(XMN2,YMX2,IX,IY)
      CALL ELINE(IX,IY,0)
      CALL ECIRC(IX,IY,3,1)
      CALL U2PIXEL(XMN2,YMN2,IX,IY)
      CALL ELINE(IX,IY,0)
      DO 10 I=1,NUMX-1
        CALL U2PIXEL(FLOAT(I),YMN2,IX1,IY1)
        CALL U2PIXEL(FLOAT(I),YMX2,IX2,IY2)
        CALL EDLINE(IX1,IY1,IX2,IY2,4)
   10 CONTINUE
      DO 20 J=1,NUMY-1
        CALL U2PIXEL(XMN2,FLOAT(J),IX1,IY1)
        CALL U2PIXEL(XMX2,FLOAT(J),IX2,IY2)
        CALL EDLINE(IX1,IY1,IX2,IY2,4)
   20 CONTINUE
      XCF=-0.5
      IF(ISRF.EQ.1)THEN
        DO 30 I=1,NUMX
          XCF=XCF+1.
          YCF=-0.5
        DO 30 J=1,NUMY
          YCF=YCF+1.
          CALL U2PIXEL(XCF,YCF,IX,IY)
          WRITE(LABELS,'(I3)')IBSRF(1,I,J)
          CALL ETLABEL(LABELS,XCF,YCF,0,ITFS)
   30   CONTINUE
      ELSEIF(ISRF.EQ.2)THEN
        DO 40 I=1,NUMX
          XCF=XCF+1.
          YCF=-0.5
        DO 40 J=1,NUMY
          YCF=YCF+1.
          CALL U2PIXEL(XCF,YCF,IX,IY)
          WRITE(LABELS,'(I3)')IBSRF(2,I,J)
          CALL ETLABEL(LABELS,XCF,YCF,0,ITFS)
   40   CONTINUE
      ELSEIF(ISRF.EQ.3)THEN
        DO 50 I=NUMX,1,-1
          XCF=XCF+1.
          YCF=-0.5
        DO 50 J=1,NUMY
          YCF=YCF+1.
          CALL U2PIXEL(XCF,YCF,IX,IY)
          WRITE(LABELS,'(I3)')IBSRF(3,I,J)
          CALL ETLABEL(LABELS,XCF,YCF,0,ITFS)
   50   CONTINUE
      ELSEIF(ISRF.EQ.4)THEN
        DO 60 I=NUMX,1,-1
          XCF=XCF+1.
          YCF=-0.5
        DO 60 J=1,NUMY
          YCF=YCF+1.
          CALL U2PIXEL(XCF,YCF,IX,IY)
          WRITE(LABELS,'(I3)')IBSRF(4,I,J)
          CALL ETLABEL(LABELS,XCF,YCF,0,ITFS)
   60   CONTINUE
      ELSEIF(ISRF.EQ.5)THEN
        DO 70 I=1,NUMX
          XCF=XCF+1.
          YCF=-0.5
        DO 70 J=1,NUMY
          YCF=YCF+1.
          CALL U2PIXEL(XCF,YCF,IX,IY)
          WRITE(LABELS,'(I3)')IBSRF(5,I,J)
          CALL ETLABEL(LABELS,XCF,YCF,0,ITFS)
   70   CONTINUE
      ELSE
        DO 80 I=1,NUMX
          XCF=XCF+1.
          YCF=-0.5
        DO 80 J=NUMY,1,-1
          YCF=YCF+1.
          CALL U2PIXEL(XCF,YCF,IX,IY)
          WRITE(LABELS,'(I3)')IBSRF(6,I,J)
          CALL ETLABEL(LABELS,XCF,YCF,0,ITFS)
   80   CONTINUE
      ENDIF
      CALL VIEWTEXT('Boundary Surface Details',1,1,2)
      XLAB=XMN2-XYMX/20.
      YLAB=YMN2-XYMX/20.
      CALL ETLABEL(VERTX(ISRF,1),XLAB,YLAB,2,ITFS)
      XLAB=XMX2+XYMX/20.
      YLAB=YMN2-XYMX/20.
      CALL ETLABEL(VERTX(ISRF,2),XLAB,YLAB,2,ITFS)
      XLAB=XMX2+XYMX/20.
      YLAB=YMX2+XYMX/20.
      CALL ETLABEL(VERTX(ISRF,3),XLAB,YLAB,4,ITFS)
      XLAB=XMN2-XYMX/20.
      YLAB=YMX2+XYMX/20.
      CALL ETLABEL(VERTX(ISRF,4),XLAB,YLAB,4,ITFS)
      call forceflush()

      RETURN
      END

C ******************************    DRWGND     ************************
C DRWGND draws the complete ground domain.
C *********************************************************************
      SUBROUTINE DRWGND
#include "building.h"

      COMMON/VIEWPX/menuchw,igl,igr,igt,igb,igw,igh
      COMMON/DRWMAT/TEMAT(4,4), TSMAT(4,4),
     &              ETMMAT(4,4), ETSMAT(4,4), TBOX(3,2)
      COMMON/GRND05/NDXG,NDYG,NDZG,DXG(MGXYZ),DYG(MGXYZ),DZG(MGXYZ)

      DIMENSION XGND(8),YGND(8),ZGND(8)
      DIMENSION XS(8),YS(8),ZS(8)

      THKX=0.
      DO 10 IX=1,NDXG
        THKX=THKX+DXG(IX)
   10 CONTINUE
      THKY=0.
      DO 20 IY=1,NDYG
        THKY=THKY+DYG(IY)
   20 CONTINUE
      THKZ=0.
      DO 30 IL=1,NDZG
        THKZ=THKZ+DZG(IL)
   30 CONTINUE
      XGND(1)=0.
      YGND(1)=0.
      ZGND(1)=0.
      XGND(2)=THKX
      YGND(2)=0.
      ZGND(2)=0.
      XGND(3)=THKX
      YGND(3)=THKY
      ZGND(3)=0.
      XGND(4)=0.
      YGND(4)=THKY
      ZGND(4)=0.
      XGND(5)=0.
      YGND(5)=0.
      ZGND(5)=THKZ
      XGND(6)=THKX
      YGND(6)=0.
      ZGND(6)=THKZ
      XGND(7)=THKX
      YGND(7)=THKY
      ZGND(7)=THKZ
      XGND(8)=0.
      YGND(8)=THKY
      ZGND(8)=THKZ

C Find 2D extremes coordinates.
      DO 40 I=1,8
        CALL ORTTRN(XGND(I),YGND(I),ZGND(I),TEMAT,AX,AY,AZ,IERR)
        CALL ORTTRN(AX,AY,AZ,ETSMAT,XS(I),YS(I),ZS(I),IERR)
   40 CONTINUE
      XMN2=AMIN1(XS(1),XS(2),XS(3),XS(4),XS(5),XS(6),XS(7),XS(8))
      YMN2=AMIN1(YS(1),YS(2),YS(3),YS(4),YS(5),YS(6),YS(7),YS(8))
      XMX2=AMAX1(XS(1),XS(2),XS(3),XS(4),XS(5),XS(6),XS(7),XS(8))
      YMX2=AMAX1(YS(1),YS(2),YS(3),YS(4),YS(5),YS(6),YS(7),YS(8))

C Clear the graphical feedback window.
      CALL startbuffer()
      CALL WIN3D(28,20,4,3,3,IGL,IGR,IGT,IGB,IGW,IGWH)
      CALL AXISCALE(IGW,IGWH,XMN2,XMX2,YMN2,YMX2,XSC,YSC,SCA,XADD,YADD)
      CALL LINESCALE(IGL,XADD,SCA,IGB,YADD,SCA)

C Locate and label the vertices.
      CALL U2PIXEL(XS(1),YS(1),IX,IY)
      CALL ECIRC(IX,IY,3,1)
      CALL VERTLBL(IX,IY,1,IER)
      CALL U2PIXEL(XS(2),YS(2),IX,IY)
      CALL ECIRC(IX,IY,3,1)
      CALL VERTLBL(IX,IY,2,IER)
      CALL U2PIXEL(XS(3),YS(3),IX,IY)
      CALL ECIRC(IX,IY,3,1)
      CALL VERTLBL(IX,IY,3,IER)
      CALL U2PIXEL(XS(4),YS(4),IX,IY)
      CALL ECIRC(IX,IY,3,1)
      CALL VERTLBL(IX,IY,4,IER)
      CALL U2PIXEL(XS(5),YS(5),IX,IY)
      CALL ECIRC(IX,IY,3,1)
      CALL VERTLBL(IX,IY,5,IER)
      CALL U2PIXEL(XS(6),YS(6),IX,IY)
      CALL ECIRC(IX,IY,3,1)
      CALL VERTLBL(IX,IY,6,IER)
      CALL U2PIXEL(XS(7),YS(7),IX,IY)
      CALL ECIRC(IX,IY,3,1)
      CALL VERTLBL(IX,IY,7,IER)
      CALL U2PIXEL(XS(8),YS(8),IX,IY)
      CALL ECIRC(IX,IY,3,1)
      CALL VERTLBL(IX,IY,8,IER)

C Draw the ground.
      CALL ETPLOT(XS(1),YS(1),0,0)
      DO 120 I=2,4
        CALL ETPLOT(XS(I),YS(I),1,0)
  120 CONTINUE
      CALL ETPLOT(XS(1),YS(1),1,0)
      DO 130 I=5,8
        CALL ETPLOT(XS(I),YS(I),1,0)
  130 CONTINUE
      CALL ETPLOT(XS(5),YS(5),1,0)
      CALL ETPLOT(XS(2),YS(2),0,0)
      CALL ETPLOT(XS(6),YS(6),1,0)
      CALL ETPLOT(XS(3),YS(3),0,0)
      CALL ETPLOT(XS(7),YS(7),1,0)
      CALL ETPLOT(XS(4),YS(4),0,0)
      CALL ETPLOT(XS(8),YS(8),1,0)
      call forceflush()

      RETURN
      END
