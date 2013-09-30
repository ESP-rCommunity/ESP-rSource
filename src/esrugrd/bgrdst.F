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

C MENUSG displays the strucrured gridding menu.
C LCLCMP displays the local components indices menu.
C IMPCMP displays the menu for defining imported component.
C DFNGRD displays the grid data menu.
C EDTMSH allows editing mesh widths.
C DVDMSH allows dividing mesh widths.
C DLTMSH allows deleting mesh widths.
C CHKREF checks if this width is referenced in the material or boundary 
C        geometries.
C DFNMTR displays the material geometry menu.
C MTRINP allows editing and adding material geometries.
C DFNBND displays the surface boundary menu.
C BNDINP allows editing and adding boundary geometries.
C SLCTZN displays the zone selection menu.
C *********************************************************************

C ***************************  MENUSG  ********************************
C MENUSG displays the strucrured gridding menu.
C *********************************************************************
      SUBROUTINE MENUSG(ITRC)

      COMMON/FILEP/IFIL
      COMMON/RAY2/ITDSP,ITBND,ITEPT,ITZNM,ITSNM,ITVNO,ITORG,ITSNR,
     &            ITOBS,ITHLS,ITHLZ,ITGRD,GRDIS,ITPPSW
      COMMON/RAY3/MODIFY,MODLEN,MODBND

      COMMON/GRSD100/IndxSt
      COMMON/GRSD101/LGrdSt

      LOGICAL ZONOK,MODIFY,MODBND,MODLEN,OK,DOK,SAVEOK,XST

      CHARACTER ITEM(13)*26,SZN*15
      CHARACTER*72 LGrdSt
      integer MITEM,INO ! max items and current menu item

C For help messages
      character helpinsub*24 ! subroutine name
      character helptopic*24 ! string (unique) for topic
      integer nbhelp     ! number of help lines found

      helpinsub='MENUSG'  ! set for subroutine

      SZN=' '
      IZ=0
      IUNIT=IFIL+1
      ZONOK=.FALSE.
      MODIFY=.FALSE.
      SAVEOK=.FALSE.

C General image option flags.
      ITDSP=1
      ITBND=1
      ITEPT=0
      ITZNM=0
      ITSNM=1
      ITVNO=1
      ITORG=1
      ITSNR=1
      ITGRD=1
      GRDIS=0.0
      ITPPSW=0

C Draw the complete building.
      IDRW1=1
      IDRW2=0

C Display the menu.
   10 INO=-3
      WRITE(ITEM(1),'(A,A)')'1 Zone : ',SZN
      ITEM(2)='  ------------------------'
      ITEM(3)='2 user defined component  '
      ITEM(4)='3 imported component      '
      ITEM(5)='  ------------------------'
      ITEM(6)='4 Save 3D conf. into file '
      ITEM(7)='5 Read 3D conf. from file '
      ITEM(8)='6 Delete 3D conf. file    '
      ITEM(9)='  ------------------------'
      ITEM(10)='! Update model cfg. file'
      ITEM(11)='  ------------------------'
      ITEM(12)='? Help                    '
      ITEM(13)='- Exit                    '
      MITEM=13

C Help text for menu.
      helptopic='grd_3d_mesh_menu'
      call gethelptext(helpinsub,helptopic,nbhelp)

C Draw the appropriate graph.
      IF(MODIFY)CALL GRAAPH(IDRW1,IDRW2)
      CALL USRMSG(' ',' ','-')
      CALL EMENU('  Structured Gridding',ITEM,MITEM,INO)

C Trap un-accepted options.
      IF(.NOT.ZONOK.AND.INO.GE.3.AND.INO.LE.4)THEN
        CALL USRMSG(' ','the zone should be defined first.','W')
      ELSEIF(.NOT.ZONOK.AND.INO.GE.6.AND.INO.LE.8)THEN
        CALL USRMSG(' ','the zone should be defined first.','W')
      ELSEIF(INO.EQ.1)THEN

C Select the zone.
        IF(IDRW1.NE.1.OR.MODIFY)THEN
          IDRW1=1
          IDRW2=0
          CALL GRAAPH(IDRW1,IDRW2)
        ENDIF
        CALL SLCTZN(ITRC,SZN,IZ,ZONOK)
        IF(ZONOK)THEN
          IDRW1=2
          IDRW2=IZ
          CALL GRAAPH(IDRW1,IDRW2)
        ENDIF
      ELSEIF(INO.EQ.3)THEN

C Define local components.
        CALL LCLCMP(IZ)
      ELSEIF(INO.EQ.4)THEN

C Define imported components.
        CALL IMPCMP(IZ)
      ELSEIF(INO.EQ.6)THEN

C Save gridding information into a file.
        CALL SAVEST(IZ,ITRC,IER)
        IF(IER.EQ.0)THEN
          SAVEOK=.TRUE.
          IndxSt=IZ
        ENDIF
      ELSEIF(INO.EQ.7)THEN

C Read gridding from a file
        CALL EASKS(LGrdSt,'Structured mesh file name ?',' ',
     &    72,' ',' Structured mesh file ',IER,nbhelp)
        CALL READST(IZ,ITRC,IER)
        IF(IER.EQ.0)THEN
          SAVEOK=.TRUE.
          IndxSt=IZ
        ENDIF
      ELSEIF(INO.EQ.8)THEN

C Delete strucrured gridding file.
        IF(IndxSt.EQ.IZ)THEN
          CALL FINDFIL(LGrdSt,XST)
          IF(XST)THEN
            CALL EFOPSEQ(IUNIT,LGrdSt,1,IER)
            CALL EFDELET(IUNIT,ISTAT)
            SAVEOK=.TRUE.
            IndxSt=0
          ENDIF
        ELSE
          CALL USRMSG(' ','Structured file does not exist','W')
        ENDIF
      ELSEIF(INO.EQ.10)THEN

C Update system configuration file.
        CALL EMKCFG('-',IER)
        IF(IER.EQ.0)THEN
          SAVEOK=.FALSE.
        ENDIF
      ELSEIF(INO.EQ.MITEM-1)THEN

C Help.
        helptopic='grd_3d_mesh_menu'
        call gethelptext(helpinsub,helptopic,nbhelp)
        CALL PHELPD('Multi-Gridding menu ',17,'-',0,0,IER)
      ELSEIF(INO.EQ.MITEM)THEN

C Return to the main multi-gridding menu.
        IF(SAVEOK)THEN
          dok=.true.
          CALL ASKOK(' ',
     &    'configuration should be updated. Ok to do this ?',
     &    OK,dok,nbhelp)
          IF(OK)CALL EMKCFG('-',IER)
        ENDIF
        RETURN
      ENDIF
      GOTO 10
      END

C ****************************   LCLCMP    ****************************
C LCLCMP displays the local components indices menu.
C *********************************************************************
      SUBROUTINE LCLCMP(IZ)
#include "epara.h"
#include "building.h"
#include "geometry.h"

      integer IZSTOCN
      COMMON/C24/IZSTOCN(MCOM,MS)

      COMMON/GRSD31/INDLCL(MS)

      CHARACTER KEY*1,ITEM(20)*31,STTS*13
      integer NITEMS,INO  ! max items and current menu item

C For help messages
      character helpinsub*24 ! subroutine name
      character helptopic*24 ! string (unique) for topic
      integer nbhelp     ! number of help lines found

      helpinsub='LCLCMP'  ! set for subroutine

C Initialise connection menu size variables based on window size. 
C INO is the menu position, NITEMS the current number of menu lines.
      MHEAD=0
      MCTL=4
      IPACT=CREATE
      CALL EKPAGE(IPACT)

C Initial menu entry setup.
   10 INO=-3
      ILEN=NSUR

C Loop through the items until the page to be displayed. M is the 
C current menu line index. Build up text strings for the menu. 
      M=MHEAD
      DO 20 L=1,ILEN
        IF(L.GE.IST.AND.(L.LE.(IST+MIFULL)))THEN
          M=M+1
          CALL EMKEY(L,KEY,IER)
          ITEM(M)=' '
          IF(INDLCL(L).EQ.1)THEN
            STTS='|     Defined'
          ELSE
            STTS='| Not Defined'
          ENDIF
          icon=IZSTOCN(iz,L)
          WRITE(ITEM(M),'(A1,1X,A12,2X,A14)')KEY,SSNAME(icon),STTS
        ENDIF
   20 CONTINUE

C Number of actual items displayed.
      NITEMS=M+MCTL

C If a long list include page facility text.      
      IF(IPFLG.EQ.0)THEN
        ITEM(M+1)='  _____________________________'
      ELSE
        WRITE(ITEM(M+1),'(A,I2,A,I2,A)')'0 Page ------- Part: ',IPM,
     &                                  ' of ',MPM,' ---'
      ENDIF
      ITEM(M+2)  ='                             '
      ITEM(M+3)  ='? Help                       '
      ITEM(M+4)  ='- Exit                       '

C Help text for this menu.
      helptopic='grd_component_status'
      call gethelptext(helpinsub,helptopic,nbhelp)

C Now display the menu.
      CALL USRMSG(' ',' ','-')
      CALL EMENU('Local Components Status',ITEM,NITEMS,INO)
      IF(INO.GT.MHEAD.AND.INO.LT.(NITEMS-MCTL+1))THEN

C Edit item identified by KEYIND.
        CALL KEYIND(NITEMS,INO,IFOC,IO)
          IF(INDLCL(IFOC).EQ.1)THEN
            INDLCL(IFOC)=0
          ELSE
            INDLCL(IFOC)=1
          ENDIF
      ELSEIF(INO.EQ.(NITEMS-3))THEN

C If there are enough items allow paging control via EKPAGE.
        IF(IPFLG.EQ.1)THEN
          IPACT=EDIT
          CALL EKPAGE(IPACT)
        ENDIF
      ELSEIF(INO.EQ.(NITEMS-1))THEN

C Display help messages.
        helptopic='grd_component_status'
        call gethelptext(helpinsub,helptopic,nbhelp)
        CALL PHELPD('local components ',nbhelp,'-',0,0,IER)
      ELSEIF(INO.EQ.NITEMS)THEN

C Return to multi-gridding menu.
        RETURN
      ENDIF
      GOTO 10
      END 

C ***************************    IMPCMP    ****************************
C IMPCMP displays the menu for defining imported component.
C *********************************************************************
      SUBROUTINE IMPCMP(IComp)

      COMMON/GRSD41/RefWidth,NRow,NCol,NLay

      DIMENSION ITEM(8)

      CHARACTER ITEM*22
      integer MITEM,INO ! max items and current menu item

C For help messages
      character helpinsub*24 ! subroutine name
      character helptopic*24 ! string (unique) for topic
      integer nbhelp     ! number of help lines found

      helpinsub='IMPCMP'  ! set for subroutine

      CALL INTSTM
      ICalc=0
      LView=4
      IF(NRow.GE.7)CALL FHDSTL(LView)
   10 INO=-3
      ITEM(1)='1 grid data           '
      ITEM(2)='2 material geometry   '
      ITEM(3)='3 surface boundary    '
      ITEM(4)='  --------------------'
      ITEM(5)='u update figure'
      ITEM(6)='  --------------------'
      ITEM(7)='? help                '
      ITEM(8)='- Exit                '
      MITEM=8

C Help text for menu.
      helptopic='import_SM_menu'
      call gethelptext(helpinsub,helptopic,nbhelp)

      CALL USRMSG(' ',' ','-')
      CALL EMENU('Importing',ITEM,MITEM,INO)
      IF(INO.EQ.1)THEN

C Define grid data.
        CALL DFNGRD
        ICalc=1
      ELSEIF(INO.EQ.2)THEN

C Define material geometry.
        CALL DFNMTR(IComp)
        ICalc=1
      ELSEIF(INO.EQ.3)THEN

C Define surface boundary.
        CALL DFNBND
        ICalc=1
      ELSEIF(INO.EQ.5)THEN

C Update figure.
        CALL FHDSTM(ICalc)
      ELSEIF(INO.EQ.MITEM-1)THEN

C Help.
        helptopic='import_SM_menu'
        call gethelptext(helpinsub,helptopic,nbhelp)
        CALL PHELPD('importing a component ',nbhelp,'-',0,0,IER)
      ELSEIF(INO.EQ.MITEM)THEN

C Return to the main multi-gridding menu.
        RETURN
      ENDIF
      GOTO 10
      END

C ****************************   DFNGRD    ****************************
C DFNGRD displays the grid data menu.
C *********************************************************************
      SUBROUTINE DFNGRD
#include "building.h"

      COMMON/OUTIN/IUOUT,IUIN

      COMMON/GRSD41/RefWidth,NRow,NCol,NLay
      COMMON/GRSD42/LRow(MROW),LCol(MCOL),LLay(MLAY)

      DIMENSION ITEM(13)

      CHARACTER ITEM*32,LstMsh*72
      LOGICAL IsEven

C For help messages
      character helpinsub*24 ! subroutine name
      character helptopic*24 ! string (unique) for topic
      integer nbhelp     ! number of help lines found

      helpinsub='DFNGRD'  ! set for subroutine

      ICalc=0
   10 INO=-3
      WRITE(ITEM(1),'(A,F7.4)')'1 reference mesh width: ',RefWidth
      WRITE(ITEM(2),'(A,I3)')'2 number of rows   : ',NRow
      WRITE(ITEM(3),'(A,I3)')'3 number of columns: ',NCol
      WRITE(ITEM(4),'(A,I3)')'4 number of layerss: ',NLay
      ITEM(5)='  --------------------'
      ITEM(6)='a edit mesh widths '
      ITEM(7)='b divide mesh widths '
      ITEM(8)='c delete mesh widths'
      ITEM(9)='  --------------------'
      ITEM(10)='u update figure   '
      ITEM(11)='! list mesh widths    '
      ITEM(12)='? help                '
      ITEM(13)='- Exit                '
      MITEM=13

C Hep text for this message.
      helptopic='grd_3D_mesh_defs'
      call gethelptext(helpinsub,helptopic,nbhelp)

      CALL USRMSG(' ',' ','-')
      CALL EMENU('Grid Data',ITEM,MITEM,INO)

C Index for figure update.
      IF(INO.GE.1.AND.INO.LE.8)THEN
        IF(INO.NE.5)ICalc=1
      ENDIF
      IF(INO.EQ.1)THEN

C Define grid data.
        CALL EASKR(RefWidth,' ','Enter the reference mesh width:',
     &     0.00099,'W',1.0,'W',0.001,'mesh width',IER,nbhelp)
      ELSEIF(INO.EQ.2)THEN
        MLmt=MROW*2+1
        CALL EASKI(NRowT,' ','Enter the number of rows:',1,'F',
     &        MLmt,'F',10,'number of rows',IER,nbhelp)
        IF(IsEven(NRowT))THEN
          CALL USRMSG('Odd numbers should be used',' ','W')
        ELSE
          NRow=NRowT
        ENDIF
      ELSEIF(INO.EQ.3)THEN
        MLmt=MCOL*2+1
        CALL EASKI(NColT,' ','Enter the number of columns:',1,'F',
     &    MLmt,'F',10,'number of columns',IER,nbhelp)
        IF(IsEven(NColT))THEN
          CALL USRMSG('Odd numbers should be used',' ','W')
        ELSE
          NCol=NColT
        ENDIF
      ELSEIF(INO.EQ.4)THEN
        MLmt=MLAY*2+1
        CALL EASKI(NLayT,' ','Enter the number of layers:',1,'F',
     &    MLmt,'F',10,'number of layers',IER,nbhelp)
        IF(IsEven(NLayT))THEN
          CALL USRMSG('Odd numbers should be used',' ','W')
        ELSE
          NLay=NLayT
        ENDIF
      ELSEIF(INO.EQ.6)THEN

C Edit mesh widths.
        CALL EASKABC('Which dimension:',' ','Row','Column','Layer',
     &    IW,nbhelp)
        IF(IW.EQ.1)THEN
          IF(NRow.GT.0)THEN
            CALL EDTMSH(NRow,LRow,'rows   ')
          ELSE
            CALL USRMSG(' ','Un acceptable number of rows.','W')
          ENDIF
        ELSEIF(IW.EQ.2)THEN
          IF(NCol.GT.0)THEN
            CALL EDTMSH(NCol,LCol,'columns')
          ELSE
            CALL USRMSG(' ','Un acceptable columns number.','W')
          ENDIF
        ELSEIF(IW.EQ.3)THEN
          IF(NLay.GT.0)THEN
            CALL EDTMSH(NLay,LLay,'layers ')
          ELSE
            CALL USRMSG(' ','Un acceptable number of layers.','W')
          ENDIF
        ENDIF
      ELSEIF(INO.EQ.7)THEN

C Divide mesh widths.
        CALL EASKABC('Which dimension:',' ','Row','Column','Layer',
     &    IW,nbhelp)
        IF(IW.EQ.1)THEN
          IF(NRow.GT.0)THEN
            CALL DVDMSH(NRow,LRow,IW)
          ELSE
            CALL USRMSG(' ','Un acceptable number of rows.','W')
          ENDIF
        ELSEIF(IW.EQ.2)THEN
          IF(NCol.GT.0)THEN
            CALL DVDMSH(NCol,LCol,IW)
          ELSE
            CALL USRMSG(' ','Un acceptable columns number.','W')
          ENDIF
        ELSEIF(IW.EQ.3)THEN
          IF(NLay.GT.0)THEN
            CALL DVDMSH(NLay,LLay,IW)
          ELSE
            CALL USRMSG(' ','Un acceptable number of layers.','W')
          ENDIF
        ENDIF
      ELSEIF(INO.EQ.8)THEN

C Delete mesh widths.
        CALL EASKABC('Which dimension:',' ','Row','Column','Layer',
     &    IW,nbhelp)
        IF(IW.EQ.1)THEN
          IF(NRow.GT.0)THEN
            CALL DLTMSH(NRow,LRow,IW)
          ELSE
            CALL USRMSG(' ','Un acceptable number of rows.','W')
          ENDIF
        ELSEIF(IW.EQ.2)THEN
          IF(NCol.GT.0)THEN
            CALL DLTMSH(NCol,LCol,IW)
          ELSE
            CALL USRMSG(' ','Un acceptable columns number.','W')
          ENDIF
        ELSEIF(IW.EQ.3)THEN
          IF(NLay.GT.0)THEN
            CALL DLTMSH(NLay,LLay,IW)
          ELSE
            CALL USRMSG(' ','Un acceptable number of layers.','W')
          ENDIF
        ENDIF
      ELSEIF(INO.EQ.(MITEM-3))THEN

C Update figure
        CALL FHDSTM(ICalc)
      ELSEIF(INO.EQ.(MITEM-2))THEN

C List mesh widths
        WRITE(LstMsh,'(11(I2,5X))')(J*2+1,J=0,10)
        CALL EDISP(IUOUT,LstMsh)
        WRITE(LstMsh,'(72A)')('-',J=1,72)
        CALL EDISP(IUOUT,LstMsh)
        CALL EDISP(IUOUT,'Row mesh widths:')
        NumRow=(NRow-1)/2
        NumCol=(NCol-1)/2
        NumLay=(NLay-1)/2
        IEnd=0
        NLoop=INT((NumRow-1)/10)+1
        DO 20 ILoop=1,NLoop
          IStart=IEnd+1
          IF(ILoop.EQ.NLoop)THEN
            IEnd=NumRow
          ELSE
            IEnd=IEnd+10
          ENDIF
          WRITE(LstMsh,'(10(1X,I6))')(LRow(J),J=IStart,IEnd)
          CALL EDISP(IUOUT,LstMsh)
   20   CONTINUE
        LngTot=0
        DO 21 J=2,NumRow-1
          LngTot=LngTot+LRow(J)
   21   CONTINUE
        TotLng=LngTot*RefWidth
   25   FORMAT(A33,F8.3,1X,A1)
        WRITE(LstMsh,25)'Total length (without borders): ',TotLng,'m'
        CALL EDISP(IUOUT,LstMsh)
        CALL EDISP(IUOUT,' ')
        CALL EDISP(IUOUT,'Column mesh widths:')
        IEnd=0
        NLoop=INT((NumCol-1)/10)+1
        DO 30 ILoop=1,NLoop
          IStart=IEnd+1
          IF(ILoop.EQ.NLoop)THEN
            IEnd=NumCol
          ELSE
            IEnd=IEnd+10
          ENDIF
          WRITE(LstMsh,'(10(1X,I6))')(LCol(J),J=IStart,IEnd)
          CALL EDISP(IUOUT,LstMsh)
   30   CONTINUE
        LngTot=0
        DO 31 J=2,NumCol-1
          LngTot=LngTot+LCol(J)
   31   CONTINUE
        TotLng=LngTot*RefWidth
        WRITE(LstMsh,25)'Total length (without borders): ',TotLng,'m'
        CALL EDISP(IUOUT,LstMsh)
        CALL EDISP(IUOUT,' ')
        CALL EDISP(IUOUT,'Layer mesh widths:')
        IEnd=0
        NLoop=INT((NumLay-1)/10)+1
        DO 40 ILoop=1,NLoop
          IStart=IEnd+1
          IF(ILoop.EQ.NLoop)THEN
            IEnd=NumLay
          ELSE
            IEnd=IEnd+10
          ENDIF
          WRITE(LstMsh,'(10(1X,I6))')(LLay(J),J=IStart,IEnd)
          CALL EDISP(IUOUT,LstMsh)
   40   CONTINUE
        LngTot=0
        DO 41 J=2,NumLay-1
          LngTot=LngTot+LLay(J)
   41   CONTINUE
        TotLng=LngTot*RefWidth
        WRITE(LstMsh,25)'Total length (without borders): ',TotLng,'m'
        CALL EDISP(IUOUT,LstMsh)
      ELSEIF(INO.EQ.MITEM-1)THEN

C Help.
        helptopic='grd_3D_mesh_defs'
        call gethelptext(helpinsub,helptopic,nbhelp)
        CALL PHELPD('importing a component ',nbhelp,'-',0,0,IER)
      ELSEIF(INO.EQ.MITEM)THEN

C Return to the main multi-gridding menu.
        RETURN
      ENDIF
      GOTO 10
      END

C *************************      EDTMSH      **************************
C EDTMSH allows editing mesh widths.
C *********************************************************************
      SUBROUTINE EDTMSH(NDim,LDim,DimName)
#include "building.h"
      integer iCountWords

      DIMENSION LDim(MLEV)

      CHARACTER*72 Promp,StrVal,Msg
      CHARACTER DimName*7

C For help messages
      character helpinsub*24 ! subroutine name
      character helptopic*24 ! string (unique) for topic
      integer nbhelp     ! number of help lines found

      helpinsub='EDTMSH'  ! set for subroutine

      helptopic='3d_mesh_width_edit'
      call gethelptext(helpinsub,helptopic,nbhelp)
      NumDim=(NDim-1)/2
      IEnd=0
      NLoop=INT((NumDim-1)/10)+1
      DO 10 ILoop=1,NLoop
        IStart=IEnd+1
        IF(ILoop.EQ.NLoop)THEN
          IEnd=NumDim
        ELSE
          IEnd=IEnd+10
        ENDIF
        WRITE(Promp,'(A27,A7,A8,I3,A,I3)')'Enter the mesh widths for ',
     &    DimName,' from ',IStart*2-1,' to ',IEnd*2+1
        WRITE(StrVal,'(10(1X,I6))')(LDim(J),J=IStart,IEnd)
  20    CALL EASKS(StrVal,Promp,' ',72,' ','mesh widths',IER,nbhelp)
        CALL USRMSG(' ',' ','-')
        NInp = iCountWords(StrVal)
        NExpct=IEnd-IStart+1
        IF(NInp.NE.NExpct)THEN
          WRITE(Msg,'(A,I3,A)')'You should enter ',NExpct,' values.'
          CALL USRMSG(' ',Msg,'W')
          GOTO 20
        ENDIF
        K=0
        DO 30 I=IStart,IEnd
          CALL EGETWI(StrVal,K,Lng,1,10000,'W','mesh widths',IER)
          IF(IER.NE.0)GOTO 20
          LDim(I)=Lng
   30   CONTINUE
   10 CONTINUE
      NDim=NumDim*2+1
      RETURN
      END

C *************************      DVDMSH      **************************
C DVDMSH allows dividing mesh widths.
C *********************************************************************
      SUBROUTINE DVDMSH(NDim,LDim,IndDim)
#include "building.h"
      integer iCountWords

      COMMON/GRSD51/NMtr,IMtrS(MMTR),IMtrE(MMTR)
      COMMON/GRSD52/IMRowS(MMTR),IMRowE(MMTR)
      COMMON/GRSD53/IMColS(MMTR),IMColE(MMTR)
      COMMON/GRSD54/IMLayS(MMTR),IMLayE(MMTR)
      COMMON/GRSD61/NBnd,IBndZ(MBND),IBndS(MBND)
      COMMON/GRSD62/IBRowS(MBND),IBRowE(MBND)
      COMMON/GRSD63/IBColS(MBND),IBColE(MBND)
      COMMON/GRSD64/IBLayS(MBND),IBLayE(MBND)

      DIMENSION LDim(MLEV)

      CHARACTER*72 Promp,StrVal,Msg
      CHARACTER DimName*6

C For help messages
      character helpinsub*24 ! subroutine name
      character helptopic*24 ! string (unique) for topic
      integer nbhelp     ! number of help lines found

      helpinsub='DVDMSH'  ! set for subroutine

      helptopic='3d_mesh_divide'
      call gethelptext(helpinsub,helptopic,nbhelp)
      IF(IndDim.EQ.1)THEN
        DimName='row'
        MaxLvl=MROW*2+1
      ELSEIF(IndDim.EQ.2)THEN
        DimName='column'
        MaxLvl=MCOL*2+1
      ELSEIF(IndDim.EQ.3)THEN
        DimName='layer'
        MaxLvl=MLAY*2+1
      ENDIF
      NumDim=(NDim-1)/2
      DO 10 ILvl=1,NumDim
        IEnd=ILvl*2+1
        IStart=IEnd-2
   91   FORMAT(A,A11,A9,I3,A5,I3,A16,I6,A1)
        WRITE(Promp,91)DimName,' mesh width',' between ',IStart,
     &    ' and ',IEnd,' (current value:',LDim(ILvl),')'
        CALL EASKABC(Promp,' ','next','divid','finish',IW,nbhelp)
        IF(IW.EQ.2)THEN
          NDiv=2
          CALL EASKI(NDiv,' ','Enter the number of divitions:',1,'F',
     &      10,'F',2,'number of divitions',IER,nbhelp)
          NAdd=NDiv-1
          NCHCK=NDim+2*NAdd
          IF(NCHCK.GT.MaxLvl)THEN
            CALL USRMSG('Maximum number of mesh widths exceeded.',
     &                  'Command ignored !','W')
          ELSE
            NumDim=NumDim+NAdd
            DO 20 I=NumDim,ILvl+NDiv,-1
              J=I-NAdd
              LDim(I)=LDim(J)
   20       CONTINUE
            NewL=INT(LDim(ILvl)/NDiv)
            WRITE(Promp,'(A27)')'Enter the mesh widths for '
            WRITE(StrVal,'(10(1X,I6))')(NewL,J=1,NDiv)
   70       CALL EASKS(StrVal,Promp,' ',72,' ','mesh widths',
     &        IER,nbhelp)
            CALL USRMSG(' ',' ','-')
            NInp = iCountWords(StrVal)
            IF(NInp.NE.NDiv)THEN
              WRITE(Msg,'(A,I3,A)')'You should enter ',NDiv,' values.'
              CALL USRMSG(' ',Msg,'W')
              GOTO 70
            ENDIF
            K=0
            DO 30 I=0,NAdd
              J=I+ILvl
              CALL EGETWI(StrVal,K,Lng,1,10000,'W','width',IER)
              IF(IER.NE.0)GOTO 70
              LDim(J)=Lng
   30       CONTINUE
            IDim=ILvl*2+1
            IDAdd=NAdd*2

C Update material geometries.
            DO 40 IMtr=1,NMtr
              IF(IndDim.EQ.1)THEN
                IF(IMRowS(IMtr).GE.IDim)IMRowS(IMtr)=IMRowS(IMtr)+IDAdd
                IF(IMRowE(IMtr).GE.IDim)IMRowE(IMtr)=IMRowE(IMtr)+IDAdd
              ELSEIF(IndDim.EQ.2)THEN
                IF(IMColS(IMtr).GE.IDim)IMColS(IMtr)=IMColS(IMtr)+IDAdd
                IF(IMColE(IMtr).GE.IDim)IMColE(IMtr)=IMColE(IMtr)+IDAdd
              ELSEIF(IndDim.EQ.3)THEN
                IF(IMLayS(IMtr).GE.IDim)IMLayS(IMtr)=IMLayS(IMtr)+IDAdd
                IF(IMLayE(IMtr).GE.IDim)IMLayE(IMtr)=IMLayE(IMtr)+IDAdd
              ENDIF
   40       CONTINUE

C Update boundary geometries.
            DO 50 IBnd=1,NBnd
              IF(IndDim.EQ.1)THEN
                IF(IBRowS(IBnd).GE.IDim)IBRowS(IBnd)=IBRowS(IBnd)+IDAdd
                IF(IBRowE(IBnd).GE.IDim)IBRowE(IBnd)=IBRowE(IBnd)+IDAdd
              ELSEIF(IndDim.EQ.2)THEN
                IF(IBColS(IBnd).GE.IDim)IBColS(IBnd)=IBColS(IBnd)+IDAdd
                IF(IBColE(IBnd).GE.IDim)IBColE(IBnd)=IBColE(IBnd)+IDAdd
              ELSEIF(IndDim.EQ.3)THEN
                IF(IBLayS(IBnd).GE.IDim)IBLayS(IBnd)=IBLayS(IBnd)+IDAdd
                IF(IBLayE(IBnd).GE.IDim)IBLayE(IBnd)=IBLayE(IBnd)+IDAdd
              ENDIF
   50       CONTINUE
          ENDIF
        ELSEIF(IW.EQ.3)THEN
          NDim=NumDim*2+1
          RETURN
        ENDIF
   10 CONTINUE
      NDim=NumDim*2+1
      RETURN
      END

C *************************      DLTMSH      **************************
C DLTMSH allows deleting mesh widths.
C *********************************************************************
      SUBROUTINE DLTMSH(NDim,LDim,IndDim)
#include "building.h"

      COMMON/OUTIN/IUOUT,IUIN
      COMMON/GRSD51/NMtr,IMtrS(MMTR),IMtrE(MMTR)
      COMMON/GRSD52/IMRowS(MMTR),IMRowE(MMTR)
      COMMON/GRSD53/IMColS(MMTR),IMColE(MMTR)
      COMMON/GRSD54/IMLayS(MMTR),IMLayE(MMTR)
      COMMON/GRSD61/NBnd,IBndZ(MBND),IBndS(MBND)
      COMMON/GRSD62/IBRowS(MBND),IBRowE(MBND)
      COMMON/GRSD63/IBColS(MBND),IBColE(MBND)
      COMMON/GRSD64/IBLayS(MBND),IBLayE(MBND)

      DIMENSION LDim(MLEV)

      CHARACTER*72 Promp
      CHARACTER DimName*6

C For help messages
      character helpinsub*24 ! subroutine name
      character helptopic*24 ! string (unique) for topic
      integer nbhelp     ! number of help lines found

      helpinsub='DLTMSH'  ! set for subroutine
      helptopic='grd_3d_mesh_delete'
      call gethelptext(helpinsub,helptopic,nbhelp)

      IF(IndDim.EQ.1)THEN
        DimName='row'
      ELSEIF(IndDim.EQ.2)THEN
        DimName='column'
      ELSEIF(IndDim.EQ.3)THEN
        DimName='layer'
      ENDIF
      NumDim=(NDim-1)/2
      DO 10 ILvl=1,NumDim
        IEnd=ILvl*2+1
        IStart=IEnd-2
   91   FORMAT(A,A11,A9,I3,A5,I3,A16,I6,A1)
        WRITE(Promp,91)DimName,' mesh width',' between ',IStart,
     &    ' and ',IEnd,' (current value:',LDim(ILvl),')'
        CALL EASKABC(Promp,' ','next','delete','finish',IW,nbhelp)
        IF(IW.EQ.2)THEN

C Check if this width is referenced in the material or boundary geometries.
          CALL CHKREF(ILvl,IndDim,IER)
          IF(IER.EQ.-1)THEN
            CALL USRMSG('The selected width is referenced by',
     &                 'a material geometry.     Command ignored!','W')
          ELSE
            IDim=ILvl*2+1
            NumDim=NumDim-1
            DO 20 I=ILvl,NumDim
              LDim(I)=LDim(I+1)
   20       CONTINUE

C Update material geometries.
            DO 40 IMtr=1,NMtr
              IF(IndDim.EQ.1)THEN
                IF(IMRowS(IMtr).GE.IDim)IMRowS(IMtr)=IMRowS(IMtr)-2
                IF(IMRowE(IMtr).GE.IDim)IMRowE(IMtr)=IMRowE(IMtr)-2
              ELSEIF(IndDim.EQ.2)THEN
                IF(IMColS(IMtr).GE.IDim)IMColS(IMtr)=IMColS(IMtr)-2
                IF(IMColE(IMtr).GE.IDim)IMColE(IMtr)=IMColE(IMtr)-2
              ELSEIF(IndDim.EQ.3)THEN
                IF(IMLayS(IMtr).GE.IDim)IMLayS(IMtr)=IMLayS(IMtr)-2
                IF(IMLayE(IMtr).GE.IDim)IMLayE(IMtr)=IMLayE(IMtr)-2
              ENDIF
   40       CONTINUE

C Update boundary geometries.
            DO 50 IBnd=1,NBnd
              IF(IndDim.EQ.1)THEN
                IF(IBRowS(IBnd).GE.IDim)IBRowS(IBnd)=IBRowS(IBnd)-2
                IF(IBRowE(IBnd).GE.IDim)IBRowE(IBnd)=IBRowE(IBnd)-2
              ELSEIF(IndDim.EQ.2)THEN
                IF(IBColS(IBnd).GE.IDim)IBColS(IBnd)=IBColS(IBnd)-2
                IF(IBColE(IBnd).GE.IDim)IBColE(IBnd)=IBColE(IBnd)-2
              ELSEIF(IndDim.EQ.3)THEN
                IF(IBLayS(IBnd).GE.IDim)IBLayS(IBnd)=IBLayS(IBnd)-2
                IF(IBLayE(IBnd).GE.IDim)IBLayE(IBnd)=IBLayE(IBnd)-2
              ENDIF
   50       CONTINUE
          ENDIF
        ELSEIF(IW.EQ.3)THEN
          NDim=NumDim*2+1
          RETURN
        ENDIF
   10 CONTINUE
      NDim=NumDim*2+1
      RETURN
      END

C *************************      CHKREF      **************************
C CHKREF checks if this width is referenced in the material or boundary 
C         geometries.
C *********************************************************************
      SUBROUTINE CHKREF(ILvl,IndDim,IER)
#include "building.h"

      COMMON/GRSD51/NMtr,IMtrS(MMTR),IMtrE(MMTR)
      COMMON/GRSD52/IMRowS(MMTR),IMRowE(MMTR)
      COMMON/GRSD53/IMColS(MMTR),IMColE(MMTR)
      COMMON/GRSD54/IMLayS(MMTR),IMLayE(MMTR)

      IER=-1
      IEnd=ILvl*2+1
      IStart=IEnd-2

C Check the material geometries first.
      DO 10 IMtr=1,NMtr
        IF(IndDim.EQ.1)THEN
          IF(IMRowS(IMtr).EQ.IStart.AND.IMRowE(IMtr).EQ.IEnd)RETURN
        ELSEIF(IndDim.EQ.2)THEN
          IF(IMColS(IMtr).EQ.IStart.AND.IMColE(IMtr).EQ.IEnd)RETURN
        ELSEIF(IndDim.EQ.3)THEN
          IF(IMLayS(IMtr).EQ.IStart.AND.IMLayE(IMtr).EQ.IEnd)RETURN
        ENDIF
   10 CONTINUE
      IER=0
      RETURN
      END

C ****************************   DFNMTR    ****************************
C DFNMTR displays the material geometry menu.
C *********************************************************************
      SUBROUTINE DFNMTR(IComp)
#include "epara.h"
#include "building.h"
#include "geometry.h"

      COMMON/OUTIN/IUOUT,IUIN

      integer IZSTOCN
      COMMON/C24/IZSTOCN(MCOM,MS)

      COMMON/GRSD51/NMtr,IMtrS(MMTR),IMtrE(MMTR)
      COMMON/GRSD52/IMRowS(MMTR),IMRowE(MMTR)
      COMMON/GRSD53/IMColS(MMTR),IMColE(MMTR)
      COMMON/GRSD54/IMLayS(MMTR),IMLayE(MMTR)

      CHARACTER KEY*1,LstMtr*72,SrfNm*12
      character*38 ITEM(20)

C For help messages
      character helpinsub*24 ! subroutine name
      character helptopic*24 ! string (unique) for topic
      integer nbhelp     ! number of help lines found

      helpinsub='DFNMTR'  ! set for subroutine

      ICalc=0

C Initialise connection menu size variables based on window size. 
C INO is the menu position, NITEMS the current number of menu lines.
      MHEAD=3
      MCTL=8
      ITEM(1)='  Material  |    Surface    | Element'
      ITEM(2)='   number   |     name      | number '
      ITEM(3)='  ___________________________________'
      IPACT=CREATE
      CALL EKPAGE(IPACT)

C Initial menu entry setup.
   10 INO=-3

C NMTR Number of Material geometry definitions.
      ILEN=NMtr

C Loop through the items until the page to be displayed. M is the 
C current menu line index. Build up text strings for the menu. 
      M=MHEAD
      DO 20 L=1,ILEN
        IF(L.GE.IST.AND.(L.LE.(IST+MIFULL)))THEN
          M=M+1
          CALL EMKEY(L,KEY,IER)
          ITEM(M)=' '
          IF(IMtrS(L).EQ.0.OR.IMtrE(L).EQ.0)THEN
            WRITE(ITEM(M),'(A1,3X,I2,7X,A12)')KEY,L,'no material'
          ELSE
            icon=IZSTOCN(IComp,IMtrS(L))
            SrfNm=SSNAME(icon)
            WRITE(ITEM(M),'(A1,3X,I2,7X,A12,6X,I3)')KEY,L,SrfNm,IMtrE(L)
          ENDIF
        ENDIF
   20 CONTINUE

C Number of actual items displayed.
      NITEMS=M+MCTL

C If a long list include page facility text.      
      IF(IPFLG.EQ.0)THEN
        ITEM(M+1)='  _________________________'
      ELSE
        WRITE(ITEM(M+1),'(A,I2,A,I2)')'0 Page ------- Part: ',IPM,
     &                                  ' of ',MPM
      ENDIF
      ITEM(M+2)  ='1 add material geometry'
      ITEM(M+3)  ='2 delete material geometry'
      ITEM(M+4)  ='! list material geometries'
      ITEM(M+5)  ='  _________________________'
      ITEM(M+6)  ='u update figure           '
      ITEM(M+7)  ='? Help                     '
      ITEM(M+8)  ='- Exit                     '

C Help text for the menu.
      helptopic='grd_mesh_mat_geom'
      call gethelptext(helpinsub,helptopic,nbhelp)

C Now display the menu.
      CALL USRMSG(' ',' ','-')
      CALL EMENU('Matrial Geometries',ITEM,NITEMS,INO)

C Index for figure update.
      IF(INO.GE.(NITEMS-6).AND.INO.LE.(NITEMS-5))ICalc=1
      IF(INO.GT.MHEAD.AND.INO.LT.(NITEMS-MCTL+1))THEN

C Edit item identified by KEYIND.
        CALL KEYIND(NITEMS,INO,IFOC,IO)
          CALL MTRINP(IFOC)
      ELSEIF(INO.EQ.(NITEMS-7))THEN

C If there are enough items allow paging control via EKPAGE.
        IF(IPFLG.EQ.1)THEN
          IPACT=EDIT
          CALL EKPAGE(IPACT)
        ENDIF
      ELSEIF(INO.EQ.(NITEMS-6))THEN

C Add material geometry
        Indx=NMtr+1
        IF(Indx.GT.MMTR)THEN
          CALL USRMSG(' ',
     &         'Maximum number of material geometries reached','W')
        ELSE
          NMtr=Indx
          CALL MTRINP(Indx)
        ENDIF
      ELSEIF(INO.EQ.(NITEMS-5).AND.NMtr.GT.0)THEN

C Delete material geometry
        ITEM(M+2)  =' '
        ITEM(M+3)  =' '
        ITEM(M+4)  =' '
  30    INO=-3
        CALL EMENU('Matrial Geometries',ITEM,NITEMS,INO)
        IF(INO.GT.MHEAD.AND.INO.LT.(NITEMS-MCTL+1))THEN
          CALL KEYIND(NITEMS,INO,IFOC,IO)
          NMtr=NMtr-1
          DO 40 IDelete=IFOC,NMtr
            IMtrS(IDelete)=IMtrS(IDelete+1)
            IMtrE(IDelete)=IMtrE(IDelete+1)
            IMRowS(IDelete)=IMRowS(IDelete+1)
            IMRowE(IDelete)=IMRowE(IDelete+1)
            IMColS(IDelete)=IMColS(IDelete+1)
            IMColE(IDelete)=IMColE(IDelete+1)
            IMLayS(IDelete)=IMLayS(IDelete+1)
            IMLayE(IDelete)=IMLayE(IDelete+1)
   40     CONTINUE
          IMtrS(IDelete)=0
          IMtrE(IDelete)=0
          IMRowS(IDelete)=0
          IMRowE(IDelete)=0
          IMColS(IDelete)=0
          IMColE(IDelete)=0
          IMLayS(IDelete)=0
          IMLayE(IDelete)=0
          GOTO 10
        ELSEIF(INO.EQ.NITEMS)THEN
          GOTO 10
        ENDIF
        GOTO 30
      ELSEIF(INO.EQ.(NITEMS-4))THEN

C List material geometries
        CALL EDISP(IUOUT,
     &    'No.  Surface   Element   R1  C1  L1      R1  C1  L1')
        CALL EDISP(IUOUT,
     &   '-----------------------------------------------------')
   51   FORMAT(1X,I2,1X,A12,1X,I2,2(5X,I3,1X,I3,1X,I3))
   52   FORMAT(1X,I2,1X,A12,3X   ,2(5X,I3,1X,I3,1X,I3))
        DO 50 Lst=1,NMtr
          IF(IMtrS(Lst).EQ.0.OR.IMtrE(Lst).EQ.0)THEN
           SrfNm='no material'
           WRITE(LstMtr,52)Lst,SrfNm,           IMRowS(Lst),IMColS(Lst),
     &                  IMLayS(Lst),IMRowE(Lst),IMColE(Lst),IMLayE(Lst)
          ELSE
           icon=IZSTOCN(IComp,IMtrS(Lst))
           SrfNm=SSNAME(icon)
           WRITE(LstMtr,51)Lst,SrfNm,IMtrE(Lst),IMRowS(Lst),IMColS(Lst),
     &                  IMLayS(Lst),IMRowE(Lst),IMColE(Lst),IMLayE(Lst)
          ENDIF
          CALL EDISP(IUOUT,LstMtr)
   50   CONTINUE
      ELSEIF(INO.EQ.(NITEMS-2))THEN

C Update figure
        CALL FHDSTM(ICalc)
      ELSEIF(INO.EQ.(NITEMS-1))THEN

C Display help messages.
        helptopic='grd_mesh_mat_geom'
        call gethelptext(helpinsub,helptopic,nbhelp)
        CALL PHELPD('local components ',nbhelp,'-',0,0,IER)
      ELSEIF(INO.EQ.NITEMS)THEN

C Return to multi-gridding menu.
        RETURN
      ENDIF
      GOTO 10
      END

C ****************************   MTRINP    ****************************
C MTRINP allows editing and adding material geometries.
C *********************************************************************
      SUBROUTINE MTRINP(I)
#include "building.h"
      integer iCountWords

      COMMON/GRSD41/RefWidth,NRow,NCol,NLay
      COMMON/GRSD51/NMtr,IMtrS(MMTR),IMtrE(MMTR)
      COMMON/GRSD52/IMRowS(MMTR),IMRowE(MMTR)
      COMMON/GRSD53/IMColS(MMTR),IMColE(MMTR)
      COMMON/GRSD54/IMLayS(MMTR),IMLayE(MMTR)

      CHARACTER*72 Promp,StrVal,Msg
      LOGICAL IsEven

C For help messages
      character helpinsub*24 ! subroutine name
      character helptopic*24 ! string (unique) for topic
      integer nbhelp     ! number of help lines found

      helpinsub='MTRINP'  ! set for subroutine
      helptopic='manage_mat_geom'
      call gethelptext(helpinsub,helptopic,nbhelp)
      
      WRITE(Promp,'(A)')'Enter the required indices:'
      WRITE(StrVal,'(10(1X,I3))')IMtrS(I),IMtrE(I),IMRowS(I),IMColS(I),
     &                         IMLayS(I),IMRowE(I),IMColE(I),IMLayE(I)
  10  CALL EASKS(StrVal,Promp,' ',72,' ','material geometry',
     &  IER,nbhelp)
      CALL USRMSG(' ',' ','-')
      NInp = iCountWords(StrVal)
      NExpct=8
      IF(NInp.NE.NExpct)THEN
        WRITE(Msg,'(A,I1,A)')'You should enter ',NExpct,' values.'
        CALL USRMSG(' ',Msg,'W')
        GOTO 10
      ENDIF
      K=0
      CALL EGETWI(StrVal,K,IMtrS(I),0,1,'-','surface No.',IER)
      CALL EGETWI(StrVal,K,IMtrE(I),0,1,'-','element No.',IER)
      NR=2*NRow+1
      NC=2*NCol+1
      NL=2*NLay+1
      CALL EGETWI(StrVal,K,IMRowS(I),1,NR,'W','start row',IER)
      IF(IsEven(IMRowS(I)))THEN
        CALL USRMSG(' ',' Box coordinates should be odd.','W')
        GOTO 10
      ENDIF
      CALL EGETWI(StrVal,K,IMColS(I),1,NC,'W','1st column',IER)
      IF(IsEven(IMColS(I)))THEN
        CALL USRMSG(' ',' Box coordinates should be odd.','W')
        GOTO 10
      ENDIF
      CALL EGETWI(StrVal,K,IMLayS(I),1,NL,'W','1st layer',IER)
      IF(IsEven(IMLayS(I)))THEN
        CALL USRMSG(' ',' Box coordinates should be odd.','W')
        GOTO 10
      ENDIF
      M1=IMRowS(I)+1
      CALL EGETWI(StrVal,K,IMRowE(I),M1,NR,'W','end row',IER)
      IF(IsEven(IMRowE(I)))THEN
        CALL USRMSG(' ',' Box coordinates should be odd.','W')
        GOTO 10
      ENDIF
      M1=IMColS(I)+1
      CALL EGETWI(StrVal,K,IMColE(I),M1,NC,'W','end column',IER)
      IF(IsEven(IMColE(I)))THEN
        CALL USRMSG(' ',' Box coordinates should be odd.','W')
        GOTO 10
      ENDIF
      M1=IMLayS(I)+1
      CALL EGETWI(StrVal,K,IMLayE(I),M1,NL,'W','end layer',IER)
      IF(IsEven(IMLayE(I)))THEN
        CALL USRMSG(' ',' Box coordinates should be odd.','W')
        GOTO 10
      ENDIF
      RETURN
      END


C ****************************   DFNBND    ****************************
C DFNBND displays the surface boundary menu.
C *********************************************************************
      SUBROUTINE DFNBND
#include "epara.h"
#include "building.h"

      COMMON/OUTIN/IUOUT,IUIN

      COMMON/GRSD61/NBnd,IBndZ(MBND),IBndS(MBND)
      COMMON/GRSD62/IBRowS(MBND),IBRowE(MBND)
      COMMON/GRSD63/IBColS(MBND),IBColE(MBND)
      COMMON/GRSD64/IBLayS(MBND),IBLayE(MBND)

      CHARACTER KEY*1,ExtInt*8,LstBnd*60
      character*32 ITEM(20)

C For help messages
      character helpinsub*24 ! subroutine name
      character helptopic*24 ! string (unique) for topic
      integer nbhelp     ! number of help lines found

      helpinsub='DFNBND'  ! set for subroutine

      ICalc=0

C Initialise connection menu size variables based on window size. 
C INO is the menu position, NITEMS the current number of menu lines.
      MHEAD=2
      MCTL=8
      ITEM(1)='   No | Zone | surf |  Int/Ext '
      ITEM(2)='  ______________________________'

      IPACT=CREATE
      CALL EKPAGE(IPACT)

C Initial menu entry setup.
   10 INO=-3

C NBnd Number of Boundary Surface Boxes definitions.
      ILEN=NBnd

C Loop through the items until the page to be displayed. M is the 
C current menu line index. Build up text strings for the menu. 
      M=MHEAD
   21 FORMAT(A1,3X,I2,3X,I2,4X,I3,5X,A8)
      DO 20 L=1,ILEN
        IF(L.GE.IST.AND.(L.LE.(IST+MIFULL)))THEN
          M=M+1
          IF(IBndS(L).LT.0)THEN
            ExtInt='External'
          ELSEIF(IBndS(L).GT.0)THEN
            ExtInt='Internal'
          ELSE
            ExtInt='UNKNOWN'
          ENDIF
          CALL EMKEY(L,KEY,IER)
          ITEM(M)=' '
          WRITE(ITEM(M),21)KEY,L,IBndZ(L),ABS(IBndS(L)),ExtInt
        ENDIF
   20 CONTINUE

C Number of actual items displayed.
      NITEMS=M+MCTL

C If a long list include page facility text.      
      IF(IPFLG.EQ.0)THEN
        ITEM(M+1)='  ______________________________'
      ELSE
        WRITE(ITEM(M+1),'(A,I2,A,I2)')'0 Page ------- Part: ',IPM,
     &                                  ' of ',MPM
      ENDIF
      ITEM(M+2)  ='1 add boundary box'
      ITEM(M+3)  ='2 delete boundary box'
      ITEM(M+4)  ='! list boundary boxes'
      ITEM(M+5)  ='  ______________________________'
      ITEM(M+6)  ='u update figure           '
      ITEM(M+7)  ='? Help                     '
      ITEM(M+8)  ='- Exit                     '

C Help text for this menu.
      helptopic='grd_boundary_surf'
      call gethelptext(helpinsub,helptopic,nbhelp)

C Now display the menu.
      CALL USRMSG(' ',' ','-')
      CALL EMENU('Surface Boundaries',ITEM,NITEMS,INO)

C Index for figure update.
      IF(INO.GE.(NITEMS-6).AND.INO.LE.(NITEMS-5))ICalc=1
      IF(INO.GT.MHEAD.AND.INO.LT.(NITEMS-MCTL+1))THEN

C Edit item identified by KEYIND.
        CALL KEYIND(NITEMS,INO,IFOC,IO)
          CALL BNDINP(IFOC)
      ELSEIF(INO.EQ.(NITEMS-7))THEN

C If there are enough items allow paging control via EKPAGE.
        IF(IPFLG.EQ.1)THEN
          IPACT=EDIT
          CALL EKPAGE(IPACT)
        ENDIF
      ELSEIF(INO.EQ.(NITEMS-6))THEN

C Add surface boundary.
        Indx=NBnd+1
        IF(Indx.GT.MBND)THEN
          CALL USRMSG(' ',
     &         'Maximum number of surface boundary boxes reached','W')
        ELSE
          NBnd=Indx
          CALL BNDINP(Indx)
        ENDIF
      ELSEIF(INO.EQ.(NITEMS-5).AND.NBnd.GT.0)THEN

C Delete surface boundary.
        ITEM(M+2)  =' '
        ITEM(M+3)  =' '
        ITEM(M+4)  =' '
  30    INO=-3
        CALL EMENU('Surface Boundaries',ITEM,NITEMS,INO)
        IF(INO.GT.MHEAD.AND.INO.LT.(NITEMS-MCTL+1))THEN
          CALL KEYIND(NITEMS,INO,IFOC,IO)
          NBnd=NBnd-1
          DO 40 IDelete=IFOC,NBnd
            IBndZ(IDelete)=IBndZ(IDelete+1)
            IBndS(IDelete)=IBndS(IDelete+1)
            IBRowS(IDelete)=IBRowS(IDelete+1)
            IBRowE(IDelete)=IBRowE(IDelete+1)
            IBColS(IDelete)=IBColS(IDelete+1)
            IBColE(IDelete)=IBColE(IDelete+1)
            IBLayS(IDelete)=IBLayS(IDelete+1)
            IBLayE(IDelete)=IBLayE(IDelete+1)
   40     CONTINUE
          IBndZ(IDelete)=0
          IBndS(IDelete)=0
          IBRowS(IDelete)=0
          IBRowE(IDelete)=0
          IBColS(IDelete)=0
          IBColE(IDelete)=0
          IBLayS(IDelete)=0
          IBLayE(IDelete)=0
          GOTO 10
        ELSEIF(INO.EQ.NITEMS)THEN
          GOTO 10
        ENDIF
        GOTO 30
      ELSEIF(INO.EQ.(NITEMS-4))THEN

C List surface boundary boxes.
        CALL EDISP(IUOUT,
     &    'No.  Zone  Surf.    R1  C1  L1      R1  C1  L1')
        CALL EDISP(IUOUT,
     &   '------------------------------------------------')
   51   FORMAT(1X,I2,3X,I2,3X,I3,2(5X,I3,1X,I3,1X,I3))
        DO 50 Lst=1,NBnd
          WRITE(LstBnd,51)Lst,IBndZ(Lst),IBndS(Lst),
     &         IBRowS(Lst),IBColS(Lst),IBLayS(Lst),IBRowE(Lst),
     &         IBColE(Lst),IBLayE(Lst)
          CALL EDISP(IUOUT,LstBnd)
   50   CONTINUE
      ELSEIF(INO.EQ.(NITEMS-2))THEN

C Update figure
        CALL FHDSTM(ICalc)
      ELSEIF(INO.EQ.(NITEMS-1))THEN

C Display help messages.
        helptopic='grd_boundary_surf'
        call gethelptext(helpinsub,helptopic,nbhelp)
        CALL PHELPD('local components ',nbhelp,'-',0,0,IER)
      ELSEIF(INO.EQ.NITEMS)THEN

C Return to multi-gridding menu.
        RETURN
      ENDIF
      GOTO 10
      END 

C ****************************   BNDINP    ****************************
C BNDINP allows editing and adding boundary geometries.
C *********************************************************************
      SUBROUTINE BNDINP(I)
#include "building.h"
      integer iCountWords

      COMMON/GRSD41/RefWidth,NRow,NCol,NLay
      COMMON/GRSD61/NBnd,IBndZ(MBND),IBndS(MBND)
      COMMON/GRSD62/IBRowS(MBND),IBRowE(MBND)
      COMMON/GRSD63/IBColS(MBND),IBColE(MBND)
      COMMON/GRSD64/IBLayS(MBND),IBLayE(MBND)

      CHARACTER*72 Promp,StrVal,Msg
      LOGICAL IsEven

C For help messages
      character helpinsub*24 ! subroutine name
      character helptopic*24 ! string (unique) for topic
      integer nbhelp     ! number of help lines found

      helpinsub='BNDINP'  ! set for subroutine

      WRITE(Promp,'(A)')'Enter the required indices:'
      WRITE(StrVal,'(8(3X,I3))')IBndZ(I),IBndS(I),IBRowS(I),
     &             IBColS(I),IBLayS(I),IBRowE(I),IBColE(I),IBLayE(I)
      helptopic='grd_manage_boundary'
      call gethelptext(helpinsub,helptopic,nbhelp)
  10  CALL EASKS(StrVal,Promp,' ',72,' ','boundary box',
     &  IER,nbhelp)
      CALL USRMSG(' ',' ','-')
      NInp = iCountWords(StrVal)
      NExpct=8
      IF(NInp.NE.NExpct)THEN
        WRITE(Msg,'(A,I1,A)')'You should enter ',NExpct,' values.'
        CALL USRMSG(' ',Msg,'W')
        GOTO 10
      ENDIF
      K=0
      CALL EGETWI(StrVal,K,IBndZ(I),0,1,'-','zone',IER)
      CALL EGETWI(StrVal,K,IBndS(I),0,1,'-','surface',IER)
      NR=2*NRow+1
      NC=2*NCol+1
      NL=2*NLay+1
      CALL EGETWI(StrVal,K,IBRowS(I),0,NR,'-','1st row',IER)
      IF(IsEven(IBRowS(I)))THEN
        CALL USRMSG(' ',' Box coordinates should be odd.','W')
        GOTO 10
      ENDIF
      CALL EGETWI(StrVal,K,IBColS(I),0,NC,'W','1st column',IER)
      IF(IsEven(IBColS(I)))THEN
        CALL USRMSG(' ',' Box coordinates should be odd.','W')
        GOTO 10
      ENDIF
      CALL EGETWI(StrVal,K,IBLayS(I),0,NL,'W','1st layer',IER)
      IF(IsEven(IBLayS(I)))THEN
        CALL USRMSG(' ',' Box coordinates should be odd.','W')
        GOTO 10
      ENDIF
      M1=IBRowS(I)
      CALL EGETWI(StrVal,K,IBRowE(I),M1,NR,'W','end row',IER)
      IF(IsEven(IBRowE(I)))THEN
        CALL USRMSG(' ',' Box coordinates should be odd.','W')
        GOTO 10
      ENDIF
      M1=IBColS(I)
      CALL EGETWI(StrVal,K,IBColE(I),M1,NC,'W','end column',IER)
      IF(IsEven(IBColE(I)))THEN
        CALL USRMSG(' ',' Box coordinates should be odd.','W')
        GOTO 10
      ENDIF
      M1=IBLayS(I)
      CALL EGETWI(StrVal,K,IBLayE(I),M1,NL,'W','end layer',IER)
      IF(IsEven(IBLayE(I)))THEN
        CALL USRMSG(' ',' Box coordinates should be odd.','W')
        GOTO 10
      ENDIF
      RETURN
      END


C ***************************      SLCTZN     *************************
C SLCTZN displays the zone selection menu.
C *********************************************************************
      SUBROUTINE SLCTZN(ITRC,SZN,IZ,ZONOK)
#include "building.h"
#include "model.h"

C geometry.h provides commons G0/G1/G2/precz.
#include "geometry.h"

      COMMON/OUTIN/IUOUT,IUIN
      COMMON/FILEP/IFIL
      
      integer ncomp,ncon
      COMMON/C1/NCOMP,NCON

      COMMON/GRSD31/INDLCL(MS)
      COMMON/GRSD41/RefWidth,NRow,NCol,NLay
      COMMON/GRSD42/LRow(MROW),LCol(MCOL),LLay(MLAY)
      COMMON/GRSD51/NMtr,IMtrS(MMTR),IMtrE(MMTR)
      COMMON/GRSD52/IMRowS(MMTR),IMRowE(MMTR)
      COMMON/GRSD53/IMColS(MMTR),IMColE(MMTR)
      COMMON/GRSD54/IMLayS(MMTR),IMLayE(MMTR)
      COMMON/GRSD61/NBnd,IBndZ(MBND),IBndS(MBND)
      COMMON/GRSD62/IBRowS(MBND),IBRowE(MBND)
      COMMON/GRSD63/IBColS(MBND),IBColE(MBND)
      COMMON/GRSD64/IBLayS(MBND),IBLayE(MBND)

      LOGICAL ZONOK

      DIMENSION IVALZ(MCOM)

      CHARACTER SZN*12

C For help messages
      character helpinsub*24 ! subroutine name
      character helptopic*24 ! string (unique) for topic
      integer nbhelp     ! number of help lines found

      helpinsub='SLCTZN'  ! set for subroutine

      IUNIT=IFIL+2

C Show the zone selection menu.
      helptopic='grd_multi_grid_zone'
      call gethelptext(helpinsub,helptopic,nbhelp)
      INPICK=1
   20 CALL EPICKS(INPICK,IVALZ,' ',' ',12,
     &            NCOMP,zname,' SELECT ZONE',IER,nbhelp)
      IZ=IVALZ(1)
      IF(IZ.GT.0.AND.IZ.LE.NCOMP)THEN
        SZN=zname(IZ)
        call georead(IUNIT,LGEOM(IZ),IZ,1,IUOUT,IER)
        IF(IER.EQ.0)CALL ECONST(LTHRM(IZ),IUNIT,IZ,ITRC,IUOUT,IER)
        IF(IER.EQ.0)THEN
          ZONOK=.TRUE.
          DO 10 IS=1,NSUR
            INDLCL(IS)=1
   10     CONTINUE
          RefWidth=0.001
          NRow=0
          NCol=0
          NLay=0
          DO 30 ILev=1,MROW
            LRow(ILev)=0
   30     CONTINUE
          DO 40 ILev=1,MCOL
            LCol(ILev)=0
   40     CONTINUE
          DO 50 ILev=1,MLAY
            LLay(ILev)=0
   50     CONTINUE
          NMtr=0
          DO 60 IMtr=1,MMTR
            IMtrS(IMtr)=0
            IMtrE(IMtr)=0
            IMRowS(IMtr)=0
            IMRowE(IMtr)=0
            IMColS(IMtr)=0
            IMColE(IMtr)=0
            IMLayS(IMtr)=0
            IMLayE(IMtr)=0
   60     CONTINUE
          NBnd=0
          DO 70 IBnd=1,MBND
            IBndZ(IBnd)=0
            IBndS(IBnd)=0
            IBRowS(IBnd)=0
            IBRowE(IBnd)=0
            IBColS(IBnd)=0
            IBColE(IBnd)=0
            IBLayS(IBnd)=0
            IBLayE(IBnd)=0
   70     CONTINUE
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

C ***************************   FHDSTM   ******************************
C FHDSTM controls the drawing of the free hand sketch for the cross 
C section of a given gridding level.
C *********************************************************************
      SUBROUTINE FHDSTM(ICalc)
      integer iCountWords

      COMMON/SPAD/MMOD,LIMIT,LIMTTY

      COMMON/GRSD41/RefWidth,NRow,NCol,NLay

      CHARACTER*72 Promp,Msg
      CHARACTER*12 StrVal
      LOGICAL IsEven

C For help messages
      character helpinsub*24 ! subroutine name
      character helptopic*24 ! string (unique) for topic
      integer nbhelp     ! number of help lines found

      helpinsub='FHDSTM'  ! set for subroutine

C Return if not in graghical mode.
      IF(MMOD.LT.8.OR.NRow.LT.7)RETURN

C Draw the cross section based on the required view dimension and level.
      helptopic='grd_draw_xsection'
      call gethelptext(helpinsub,helptopic,nbhelp)
      
      WRITE(Promp,'(A)')'Enter the view axeis and level indices:'
      WRITE(StrVal,'(A4)')'3  4'
  10  CALL EASKS(StrVal,Promp,' ',14,' ','View Indices',IER,nbhelp)
      CALL USRMSG(' ',' ','-')
      NInp = iCountWords(StrVal)
      NExpct=2
      IF(NInp.NE.NExpct)THEN
        WRITE(Msg,'(A,I1,A)')'You should enter ',NExpct,' values.'
        CALL USRMSG(' ',Msg,'W')
        GOTO 10
      ENDIF
      K=0
      CALL EGETWI(StrVal,K,IView,1,3,'-','view axis',IER)
      CALL EGETWI(StrVal,K,LView,0,1,'-','view level',IER)
      IF(IsEven(LView))THEN
        IF(IView.EQ.1)THEN
          IF(LView.LT.4.OR.LView.GT.(NRow-3))CALL USRMSG(' ',
     &           'view level out of range (level=4 assumed).','W')
          IF(ICalc.EQ.1)CALL INTSTM
          ICalc=0
          CALL FHDSTR(LView)
        ELSEIF(IView.EQ.2)THEN
          IF(LView.LT.4.OR.LView.GT.(NCol-3))CALL USRMSG(' ',
     &           'view level out of range (level=4 assumed).','W')
          IF(ICalc.EQ.1)CALL INTSTM
          ICalc=0
          CALL FHDSTC(LView)
        ELSEIF(IView.EQ.3)THEN
          IF(LView.LT.4.OR.LView.GT.(NLay-3))CALL USRMSG(' ',
     &           'view level out of range (level=4 assumed).','W')
          IF(ICalc.EQ.1)CALL INTSTM
          ICalc=0
          CALL FHDSTL(LView)
        ELSE
          CALL USRMSG(' ','view axis should be (1,2, or 3).','W')
        ENDIF
      ELSE
        CALL USRMSG(' ','view level should be even.','W')
      ENDIF
      RETURN
      END

C ***************************   INTSTM   ******************************
C INTSTM controls the drawing of the free hand sketch for the cross 
C section of a given gridding level.
C *********************************************************************
      SUBROUTINE INTSTM
#include "building.h"

      COMMON/GRSD41/RefWidth,NRow,NCol,NLay
      COMMON/GRSD51/NMtr,IMtrS(MMTR),IMtrE(MMTR)
      COMMON/GRSD52/IMRowS(MMTR),IMRowE(MMTR)
      COMMON/GRSD53/IMColS(MMTR),IMColE(MMTR)
      COMMON/GRSD54/IMLayS(MMTR),IMLayE(MMTR)
      COMMON/GRSD61/NBnd,IBndZ(MBND),IBndS(MBND)
      COMMON/GRSD62/IBRowS(MBND),IBRowE(MBND)
      COMMON/GRSD63/IBColS(MBND),IBColE(MBND)
      COMMON/GRSD64/IBLayS(MBND),IBLayE(MBND)
      COMMON/GRSD110/IndSN(MGRID)
      COMMON/GRSD111/NColLay

C Update the structured mesh configuration arrays.
      NColLay=NCol*NLay

C Create the 3D structured mesh and fill it with thermophysical properties, 
C or boundary index.

C NumGrid is the total number of grid points.
      NumGrid=NRow*NCol*NLay

C IndSN is the Index for Structured Nodes.
      DO 40 I=1,NumGrid
        IndSN(I)=0
   40 CONTINUE

C Apply the material geometries.
      DO 50 IMtr=1,NMtr
        IF(IMtrS(IMtr).EQ.0.OR.IMtrE(IMtr).EQ.0)THEN
          Indx=0
        ELSE
          Indx=IMtr
        ENDIF
      DO 50 IRow=IMRowS(IMtr)+1,IMRowE(IMtr),2
      DO 50 ICol=IMColS(IMtr)+1,IMColE(IMtr),2
      DO 50 ILay=IMLayS(IMtr)+1,IMLayE(IMtr),2
        I=IGridN(IRow,ICol,ILay)
        IndSN(I)=Indx
   50 CONTINUE

C Apply the boundary conditions.
      DO 60 IBnd=1,NBnd
        IZon=IBndZ(IBnd)
        ISrf=IBndS(IBnd)
        IF(IZon.LE.0.OR.ISrf.EQ.0)THEN
          Indx=0

C Internal surface.
        ELSEIF(ISrf.GT.0)THEN
          Indx=IBnd

C External surface.
        ELSEIF(ISrf.LT.0)THEN
          Indx=IBnd
        ENDIF
        DO 61 IRow=IBRowS(IBnd),IBRowE(IBnd),2
        DO 61 ICol=IBColS(IBnd)+1,IBColE(IBnd),2
        DO 61 ILay=IBLayS(IBnd)+1,IBLayE(IBnd),2

C Allow boundary condition only when it is defined at acceptable locations.
          I=IGridN(IRow-1,ICol,ILay)
          I1=IndSN(I)
          I=IGridN(IRow+1,ICol,ILay)
          I2=IndSN(I)
          IF((I1.GT.0.AND.I2.EQ.0).OR.(I1.EQ.0.AND.I2.GT.0))THEN
            I=IGridN(IRow,ICol,ILay)
            IndSN(I)=Indx
          ENDIF
   61   CONTINUE
        DO 62 IRow=IBRowS(IBnd)+1,IBRowE(IBnd),2
        DO 62 ICol=IBColS(IBnd),IBColE(IBnd),2
        DO 62 ILay=IBLayS(IBnd)+1,IBLayE(IBnd),2
          I=IGridN(IRow,ICol-1,ILay)
          I1=IndSN(I)
          I=IGridN(IRow,ICol+1,ILay)
          I2=IndSN(I)
          IF((I1.GT.0.AND.I2.EQ.0).OR.(I1.EQ.0.AND.I2.GT.0))THEN
            I=IGridN(IRow,ICol,ILay)
            IndSN(I)=Indx
          ENDIF
   62   CONTINUE
        DO 63 IRow=IBRowS(IBnd)+1,IBRowE(IBnd),2
        DO 63 ICol=IBColS(IBnd)+1,IBColE(IBnd),2
        DO 63 ILay=IBLayS(IBnd),IBLayE(IBnd),2
          I=IGridN(IRow,ICol,ILay-1)
          I1=IndSN(I)
          I=IGridN(IRow,ICol,ILay+1)
          I2=IndSN(I)
          IF((I1.GT.0.AND.I2.EQ.0).OR.(I1.EQ.0.AND.I2.GT.0))THEN
            I=IGridN(IRow,ICol,ILay)
            IndSN(I)=Indx
          ENDIF
   63   CONTINUE
   60 CONTINUE
      RETURN
      END

C ***************************   FHDSTR   ******************************
C FHDSTR draws the free hand sketch for the cross section of a given 
C gridding level in the layer dimension.
C *********************************************************************
      SUBROUTINE FHDSTR(IRow)
#include "building.h"

      COMMON/SPAD/MMOD,LIMIT,LIMTTY
      integer ifs,itfs,imfs
      COMMON/GFONT/IFS,ITFS,IMFS
      integer menuchw,igl,igr,igt,igb,igw,igwh
      COMMON/VIEWPX/menuchw,igl,igr,igt,igb,igw,igwh

      COMMON/GRSD41/RefWidth,NRow,NCol,NLay
      COMMON/GRSD110/IndSN(MGRID)

      CHARACTER Label*2,Title*17
#ifdef OSI
      integer igwid,igheight  ! for use with axiscale
      integer iside,isize,ifont,ipos     ! passed to viewtext etlabel
      integer iix,iiy,iix1,iiy1,iix2,iiy2
      integer iigl,iigr,iigt,iigb,iigw,iigwh
      integer iiw1,iiw2,iiw3,iiw4,iimenu
#else
      integer*8 igwid,igheight  ! for use with axiscale
      integer*8 iside,isize,ifont,ipos     ! passed to viewtext etlabel
      integer*8 iix,iiy,iix1,iiy1,iix2,iiy2
      integer*8 iigl,iigr,iigt,iigb,iigw,iigwh
      integer*8 iiw1,iiw2,iiw3,iiw4,iimenu
#endif

C Return if not in graghical mode.
      IF(MMOD.LT.8)RETURN

C Define the view parameters.
      XMin=0.
      YMin=0.
      XMax=FLOAT(NCol)
      YMax=FLOAT(NLay)
      call startbuffer()

C Setup and pass in parameters to win3d.
      iiw1=14; iiw2=14; iiw3=4; iiw4=3; iimenu=menuchw
      iigl=igl; iigr=igr; iigt=igt; iigb=igb; iigw=igw; iigwh=igwh
      CALL win3d(iimenu,iiw1,iiw2,iiw3,iiw4,
     &  iigl,iigr,iigt,iigb,iigw,iigwh)
      igl=int(iigl); igr=int(iigr); igt=int(iigt); igb=int(iigb)
      igw=int(iigw); igwh=int(iigwh)
      igwid=igw
      igheight=igwh

C Estimate the X, Y, XY scales.
      CALL AXISCALE(igwid,igheight,XMin,XMax,YMin,YMax,XSC,YSC,SCA,
     &               Xadd,Yadd)

C Define the active scale.
      CALL LINESCALE(iigl,Xadd,SCA,iigb,Yadd,SCA)

C Display header information.
      WRITE(Title,'(A14,I2)')'Row Number: ',IRow
      iside=1
      isize=1
      ifont=2
      call viewtext(Title,iside,isize,ifont)

C Draw a scale horizontal axis, move to beginning point.
      CALL U2PIXEL(XMin,YMin,iix,iiy)
      CALL ELINE(iix,iiy,1)
      X=XMax*.25
      CALL U2PIXEL(X,YMin,iix,iiy)
      CALL ELINE(iix,iiy,0)
      CALL ESYMBOL(iix,iiy,24,ITFS+2)
      X=X*1.5
      ipos=0
      isize=itfs+2
      CALL ETLABEL('Column',X,YMin,ipos,isize)

C Draw a scale vertical axis.
      CALL U2PIXEL(XMin,YMin,iix,iiy)
      CALL ELINE(iix,iiy,1)
      Y=YMax*0.25
      CALL U2PIXEL(XMin,Y,iix,iiy)
      CALL ELINE(iix,iiy,0)
      CALL ESYMBOL(iix,iiy,24,ITFS+2)
      Y=YMax*0.35
      CALL ETLABEL('Layer',XMin,Y,ipos,isize)

C Draw the gridding based on seperate control volumes.
      DO 10 ILay=4,(NLay-3),2
      DO 10 ICol=4,(NCol-3),2
        I=IGridN(IRow,ICol,ILay)
        IMat=IndSN(I)

C The current control volume (CV) is a material.
        IF(IMat.GT.0)THEN
          X=FLOAT(ICol)
          Y=FLOAT(ILay)
          WRITE(Label,'(I2)')IMat
          CALL ETLABEL(Label,X,Y,ipos,isize)

C Define the adjacent CVs.
          I=IGridN(IRow,ICol,ILay+2)
          IMatU=IndSN(I)
          I=IGridN(IRow,ICol,ILay-2)
          IMatD=IndSN(I)
          I=IGridN(IRow,ICol-2,ILay)
          IMatL=IndSN(I)
          I=IGridN(IRow,ICol+2,ILay)
          IMatR=IndSN(I)

C Define the X and Y coordinates for the corners of the current CV.
          X1=X-1.0
          X2=X+1.0
          Y1=Y-1.0
          Y2=Y+1.0

C Up edge details.
          IF(IMatU.EQ.0)THEN
            CALL U2PIXEL(X1,Y2,iix,iiy)
            CALL ELINE(iix,iiy,1)
            CALL U2PIXEL(X2,Y2,iix,iiy)
            CALL ELINE(iix,iiy,0)
            I=IGridN(IRow,ICol,ILay+1)
            IBnd=IndSN(I)
            IF(IBnd.GT.0)THEN
              WRITE(Label,'(I2)')IBnd
              CALL ETLABEL(Label,X,Y+2.0,ipos,isize)
            ENDIF
          ELSEIF(IMatU.GT.0)THEN
            CALL U2PIXEL(X1,Y2,iix1,iiy1)
            CALL U2PIXEL(X2,Y2,iix2,iiy2)
            CALL EDLINE(iix1,iiy1,iix2,iiy2,4)
          ENDIF

C Down edge details.
          IF(IMatD.EQ.0)THEN
            CALL U2PIXEL(X1,Y1,iix,iiy)
            CALL ELINE(iix,iiy,1)
            CALL U2PIXEL(X2,Y1,iix,iiy)
            CALL ELINE(iix,iiy,0)
            I=IGridN(IRow,ICol,ILay-1)
            IBnd=IndSN(I)
            IF(IBnd.GT.0)THEN
              WRITE(Label,'(I2)')IBnd
              CALL ETLABEL(Label,X,Y-2.0,ipos,isize)
            ENDIF
          ENDIF

C Left edge details.
          IF(IMatL.EQ.0)THEN
            CALL U2PIXEL(X1,Y1,iix,iiy)
            CALL ELINE(iix,iiy,1)
            CALL U2PIXEL(X1,Y2,iix,iiy)
            CALL ELINE(iix,iiy,0)
            I=IGridN(IRow,ICol-1,ILay)
            IBnd=IndSN(I)
            IF(IBnd.GT.0)THEN
              WRITE(Label,'(I2)')IBnd
              CALL ETLABEL(Label,X-2.0,Y,ipos,isize)
            ENDIF
          ENDIF

C Right edge details.
          IF(IMatR.EQ.0)THEN
            CALL U2PIXEL(X2,Y1,iix,iiy)
            CALL ELINE(iix,iiy,1)
            CALL U2PIXEL(X2,Y2,iix,iiy)
            CALL ELINE(iix,iiy,0)
            I=IGridN(IRow,ICol+1,ILay)
            IBnd=IndSN(I)
            IF(IBnd.GT.0)THEN
              WRITE(Label,'(I2)')IBnd
              CALL ETLABEL(Label,X+2.0,Y,ipos,isize)
            ENDIF
          ELSEIF(IMatR.GT.0)THEN
            CALL U2PIXEL(X2,Y1,iix1,iiy1)
            CALL U2PIXEL(X2,Y2,iix2,iiy2)
            CALL EDLINE(iix1,iiy1,iix2,iiy2,4)
          ENDIF
        ENDIF
   10 CONTINUE
      call forceflush()

      RETURN
      END

C ***************************   FHDSTC   ******************************
C FHDSTC draws the free hand sketch for the cross section of a given 
C gridding level in the layer dimension.
C *********************************************************************
      SUBROUTINE FHDSTC(ICol)
#include "building.h"

      COMMON/SPAD/MMOD,LIMIT,LIMTTY
      integer ifs,itfs,imfs
      COMMON/GFONT/IFS,ITFS,IMFS
      integer menuchw,igl,igr,igt,igb,igw,igwh
      COMMON/VIEWPX/menuchw,igl,igr,igt,igb,igw,igwh

      COMMON/GRSD41/RefWidth,NRow,NCol,NLay
      COMMON/GRSD110/IndSN(MGRID)

      CHARACTER Label*2,Title*17
#ifdef OSI
      integer igwid,igheight  ! for use with axiscale
      integer iside,isize,ifont,ipos     ! passed to viewtext etlabel
      integer iix,iiy,iix1,iiy1,iix2,iiy2
      integer iigl,iigr,iigt,iigb,iigw,iigwh
      integer iiw1,iiw2,iiw3,iiw4,iimenu
#else
      integer*8 igwid,igheight  ! for use with axiscale
      integer*8 iside,isize,ifont,ipos     ! passed to viewtext etlabel
      integer*8 iix,iiy,iix1,iiy1,iix2,iiy2
      integer*8 iigl,iigr,iigt,iigb,iigw,iigwh
      integer*8 iiw1,iiw2,iiw3,iiw4,iimenu
#endif

C Return if not in graghical mode.
      IF(MMOD.LT.8)RETURN

C Define the view parameters.
      XMin=0.
      YMin=0.
      XMax=FLOAT(NLay)
      YMax=FLOAT(NRow)
      call startbuffer()

C Setup and pass in parameters to win3d.
      iiw1=14; iiw2=14; iiw3=4; iiw4=3; iimenu=menuchw
      iigl=igl; iigr=igr; iigt=igt; iigb=igb; iigw=igw; iigwh=igwh
      CALL win3d(iimenu,iiw1,iiw2,iiw3,iiw4,
     &  iigl,iigr,iigt,iigb,iigw,iigwh)
      igl=int(iigl); igr=int(iigr); igt=int(iigt); igb=int(iigb)
      igw=int(iigw); igwh=int(iigwh)
      igwid=igw
      igheight=igwh

C Estimate the X, Y, XY scales.
      CALL AXISCALE(igwid,igheight,XMin,XMax,YMin,YMax,XSC,YSC,SCA,
     &              Xadd,Yadd)

C Define the active scale.
      CALL LINESCALE(iigl,Xadd,SCA,iigb,Yadd,SCA)

C Display header information.
      WRITE(Title,'(A14,I2)')'Column Number: ',ICol
      iside=1
      isize=1
      ifont=2
      call viewtext(Title,iside,isize,ifont)

C Draw a scale horizontal axis, move to beginning point.
      CALL U2PIXEL(XMax,YMax,iix,iiy)
      CALL ELINE(iix,iiy,1)
      X=XMax*0.75
      CALL U2PIXEL(X,YMax,iix,iiy)
      CALL ELINE(iix,iiy,0)
      CALL ESYMBOL(iix,iiy,24,ITFS+2)
      X=XMax*0.6
      ipos=0
      isize=itfs+2
      CALL ETLABEL('Layer',X,YMax,ipos,isize)

C Draw a scale vertical axis.
      CALL U2PIXEL(XMax,YMax,iix,iiy)
      CALL ELINE(iix,iiy,1)
      Y=YMax*0.75
      CALL U2PIXEL(XMax,Y,iix,iiy)
      CALL ELINE(iix,iiy,0)
      CALL ESYMBOL(iix,iiy,24,ITFS+2)
      Y=YMax*0.65
      CALL ETLABEL('Row',XMax,Y,ipos,isize)

C Draw the gridding based on seperate control volumes.
      DO 10 IRow=4,(NRow-3),2
      DO 10 ILay=4,(NLay-3),2
        I=IGridN(IRow,ICol,ILay)
        IMat=IndSN(I)

C The current control volume (CV) is a material.
        IF(IMat.GT.0)THEN
          X=FLOAT(NLay-ILay)
          Y=FLOAT(NRow-IRow)
          WRITE(Label,'(I2)')IMat

C 0=centred, 1=right, 2=centred top, 3=left,4=centered bottom.
          CALL ETLABEL(Label,X,Y,ipos,isize)

C Define the adjacent CVs.
          I=IGridN(IRow-2,ICol,ILay)
          IMatU=IndSN(I)
          I=IGridN(IRow+2,ICol,ILay)
          IMatD=IndSN(I)
          I=IGridN(IRow,ICol,ILay+2)
          IMatL=IndSN(I)
          I=IGridN(IRow,ICol,ILay-2)
          IMatR=IndSN(I)

C Define the X and Y coordinates for the corners of the current CV.
          X1=X+1.0
          X2=X-1.0
          Y1=Y+1.0
          Y2=Y-1.0

C Up edge details.
          IF(IMatU.EQ.0)THEN
            CALL U2PIXEL(X1,Y1,iix,iiy)
            CALL ELINE(iix,iiy,1)
            CALL U2PIXEL(X2,Y1,iix,iiy)
            CALL ELINE(iix,iiy,0)
            I=IGridN(IRow-1,ICol,ILay)
            IBnd=IndSN(I)
            IF(IBnd.GT.0)THEN
              WRITE(Label,'(I2)')IBnd
              CALL ETLABEL(Label,X,Y+2.0,ipos,isize)
            ENDIF
          ENDIF

C Down edge details.
          IF(IMatD.EQ.0)THEN
            CALL U2PIXEL(X1,Y2,iix,iiy)
            CALL ELINE(iix,iiy,1)
            CALL U2PIXEL(X2,Y2,iix,iiy)
            CALL ELINE(iix,iiy,0)
            I=IGridN(IRow+1,ICol,ILay)
            IBnd=IndSN(I)
            IF(IBnd.GT.0)THEN
              WRITE(Label,'(I2)')IBnd
              CALL ETLABEL(Label,X,Y-2.0,ipos,isize)
            ENDIF
          ELSEIF(IMatD.GT.0)THEN
            CALL U2PIXEL(X1,Y2,iix1,iiy1)
            CALL U2PIXEL(X2,Y2,iix2,iiy2)
            CALL EDLINE(iix1,iiy1,iix2,iiy2,4)
          ENDIF

C Left edge details.
          IF(IMatL.EQ.0)THEN
            CALL U2PIXEL(X2,Y1,iix,iiy)
            CALL ELINE(iix,iiy,1)
            CALL U2PIXEL(X2,Y2,iix,iiy)
            CALL ELINE(iix,iiy,0)
            I=IGridN(IRow,ICol,ILay+1)
            IBnd=IndSN(I)
            IF(IBnd.GT.0)THEN
              WRITE(Label,'(I2)')IBnd
              CALL ETLABEL(Label,X-2.0,Y,ipos,isize)
            ENDIF
          ELSEIF(IMatL.GT.0)THEN
            CALL U2PIXEL(X2,Y1,iix1,iiy1)
            CALL U2PIXEL(X2,Y2,iix2,iiy2)
            CALL EDLINE(iix1,iiy1,iix2,iiy2,4)
          ENDIF
        ENDIF

C Right edge details.
        IF(IMatR.EQ.0)THEN
          CALL U2PIXEL(X1,Y1,iix,iiy)
          CALL ELINE(iix,iiy,1)
          CALL U2PIXEL(X1,Y2,iix,iiy)
          CALL ELINE(iix,iiy,0)
          I=IGridN(IRow,ICol,ILay-1)
          IBnd=IndSN(I)
          IF(IBnd.GT.0)THEN
            WRITE(Label,'(I2)')IBnd
            CALL ETLABEL(Label,X+2.0,Y,ipos,isize)
          ENDIF
        ENDIF
   10 CONTINUE
      call forceflush()

      RETURN
      END

C ***************************   FHDSTL   ******************************
C FHDSTL draws the free hand sketch for the cross section of a given 
C gridding level in the layer dimension.
C *********************************************************************
      SUBROUTINE FHDSTL(ILay)
#include "building.h"

      COMMON/SPAD/MMOD,LIMIT,LIMTTY
      integer ifs,itfs,imfs
      COMMON/GFONT/IFS,ITFS,IMFS
      integer menuchw,igl,igr,igt,igb,igw,igwh
      COMMON/VIEWPX/menuchw,igl,igr,igt,igb,igw,igwh

      COMMON/GRSD41/RefWidth,NRow,NCol,NLay
      COMMON/GRSD110/IndSN(MGRID)

      CHARACTER Label*2,Title*17
#ifdef OSI
      integer igwid,igheight  ! for use with axiscale
      integer iside,isize,ifont,ipos     ! passed to viewtext etlabel
      integer iix,iiy,iix1,iiy1,iix2,iiy2
      integer iigl,iigr,iigt,iigb,iigw,iigwh
      integer iiw1,iiw2,iiw3,iiw4,iimenu
#else
      integer*8 igwid,igheight  ! for use with axiscale
      integer*8 iside,isize,ifont,ipos    ! passed to viewtext etlabel
      integer*8 iix,iiy,iix1,iiy1,iix2,iiy2
      integer*8 iigl,iigr,iigt,iigb,iigw,iigwh
      integer*8 iiw1,iiw2,iiw3,iiw4,iimenu
#endif

C Return if not in graghical mode.
      IF(MMOD.LT.8)RETURN

C Define the view parameters.
      XMin=0.
      YMin=0.
      XMax=FLOAT(NCol)
      YMax=FLOAT(NRow)
      call startbuffer()

C Setup and pass in parameters to win3d.
      iiw1=14; iiw2=14; iiw3=4; iiw4=3; iimenu=menuchw
      iigl=igl; iigr=igr; iigt=igt; iigb=igb; iigw=igw; iigwh=igwh
      CALL win3d(iimenu,iiw1,iiw2,iiw3,iiw4,
     &  iigl,iigr,iigt,iigb,iigw,iigwh)
      igl=int(iigl); igr=int(iigr); igt=int(iigt); igb=int(iigb)
      igw=int(iigw); igwh=int(iigwh)
      igwid=igw
      igheight=igwh

C Estimate the X, Y, XY scales.
      CALL AXISCALE(igwid,igheight,XMin,XMax,YMin,YMax,XSC,YSC,SCA,
     &               Xadd,Yadd)

C Define the active scale.
      CALL LINESCALE(iigl,Xadd,SCA,iigb,Yadd,SCA)

C Display header information.
      WRITE(Title,'(A14,I2)')'Layer Number: ',ILay
      iside=1
      isize=1
      ifont=2
      call viewtext(Title,iside,isize,ifont)

C Draw a scale horizontal axis, move to beginning point.
      CALL U2PIXEL(XMin,YMax,iix,iiy)
      CALL ELINE(iix,iiy,1)
      X=XMax/4.0
      CALL U2PIXEL(X,YMax,iix,iiy)
      CALL ELINE(iix,iiy,0)
      CALL ESYMBOL(iix,iiy,24,ITFS+2)
      X=X*1.5
      ipos=0
      isize=itfs+2
      CALL ETLABEL('Column',X,YMax,ipos,isize)

C Draw a scale vertical axis.
      CALL U2PIXEL(XMin,YMax,iix,iiy)
      CALL ELINE(iix,iiy,1)
      Y=YMax*0.75
      CALL U2PIXEL(XMin,Y,iix,iiy)
      CALL ELINE(iix,iiy,0)
      CALL ESYMBOL(iix,iiy,24,ITFS+2)
      Y=YMax*0.65
      CALL ETLABEL('Row',XMin,Y,ipos,isize)

C Draw the gridding based on seperate control volumes.
      DO 10 IRow=4,(NRow-3),2
      DO 10 ICol=4,(NCol-3),2
        I=IGridN(IRow,ICol,ILay)
        IMat=IndSN(I)

C The current control volume (CV) is a material.
        IF(IMat.GT.0)THEN
          X=FLOAT(ICol)
          Y=FLOAT(NRow-IRow)
          WRITE(Label,'(I2)')IMat
          CALL ETLABEL(Label,X,Y,ipos,isize)

C Define the adjacent CVs.
          I=IGridN(IRow-2,ICol,ILay)
          IMatU=IndSN(I)
          I=IGridN(IRow+2,ICol,ILay)
          IMatD=IndSN(I)
          I=IGridN(IRow,ICol-2,ILay)
          IMatL=IndSN(I)
          I=IGridN(IRow,ICol+2,ILay)
          IMatR=IndSN(I)

C Define the X and Y coordinates for the corners of the current CV.
          X1=X-1.0
          X2=X+1.0
          Y1=Y+1.0
          Y2=Y-1.0

C Up edge details.
          IF(IMatU.EQ.0)THEN
            CALL U2PIXEL(X1,Y1,iix,iiy)
            CALL ELINE(iix,iiy,1)
            CALL U2PIXEL(X2,Y1,iix,iiy)
            CALL ELINE(iix,iiy,0)
            I=IGridN(IRow-1,ICol,ILay)
            IBnd=IndSN(I)
            IF(IBnd.GT.0)THEN
              WRITE(Label,'(I2)')IBnd
              CALL ETLABEL(Label,X,Y+2.0,ipos,isize)
            ENDIF
          ENDIF

C Down edge details.
          IF(IMatD.EQ.0)THEN
            CALL U2PIXEL(X1,Y2,iix,iiy)
            CALL ELINE(iix,iiy,1)
            CALL U2PIXEL(X2,Y2,iix,iiy)
            CALL ELINE(iix,iiy,0)
            I=IGridN(IRow+1,ICol,ILay)
            IBnd=IndSN(I)
            IF(IBnd.GT.0)THEN
              WRITE(Label,'(I2)')IBnd
              CALL ETLABEL(Label,X,Y-2.0,ipos,isize)
            ENDIF
          ELSEIF(IMatD.GT.0)THEN
            CALL U2PIXEL(X1,Y2,iix1,iiy1)
            CALL U2PIXEL(X2,Y2,iix2,iiy2)
            CALL EDLINE(iix1,iiy1,iix2,iiy2,4)
          ENDIF

C Left edge details.
          IF(IMatL.EQ.0)THEN
            CALL U2PIXEL(X1,Y1,iix,iiy)
            CALL ELINE(iix,iiy,1)
            CALL U2PIXEL(X1,Y2,iix,iiy)
            CALL ELINE(iix,iiy,0)
            I=IGridN(IRow,ICol-1,ILay)
            IBnd=IndSN(I)
            IF(IBnd.GT.0)THEN
              WRITE(Label,'(I2)')IBnd
              CALL ETLABEL(Label,X-2.0,Y,ipos,isize)
            ENDIF
          ENDIF

C Right edge details.
          IF(IMatR.EQ.0)THEN
            CALL U2PIXEL(X2,Y1,iix,iiy)
            CALL ELINE(iix,iiy,1)
            CALL U2PIXEL(X2,Y2,iix,iiy)
            CALL ELINE(iix,iiy,0)
            I=IGridN(IRow,ICol+1,ILay)
            IBnd=IndSN(I)
            IF(IBnd.GT.0)THEN
              WRITE(Label,'(I2)')IBnd
              CALL ETLABEL(Label,X+2.0,Y,ipos,isize)
            ENDIF
          ELSEIF(IMatR.GT.0)THEN
            CALL U2PIXEL(X2,Y1,iix1,iiy1)
            CALL U2PIXEL(X2,Y2,iix2,iiy2)
            CALL EDLINE(iix1,iiy1,iix2,iiy2,4)
          ENDIF
        ENDIF
   10 CONTINUE
      call forceflush()

      RETURN
      END
