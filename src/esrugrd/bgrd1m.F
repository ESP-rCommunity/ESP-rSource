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
C  This file contains the following 1-D moisture node distribution 
C  subroutines:

C MENU1M generates the 1D moisture node distribution menu.
C AUTG1D generates the variable node distribution menu.
C LUMPMN generates the lumped construction menu.
C LISTND lists the node distribution in the text feedback window.
C *********************************************************************

C ******************************  MENU1M  *****************************
C MENU1D generates the 1D moisture node distribution menu.
C *********************************************************************
      SUBROUTINE MENU1M(IDRW1,IDRW2,ITRC)
#include "building.h"

      COMMON/FILEP/IFIL
      common/pophelp/h(60)
      COMMON/HELP/IHELPM
      COMMON/RAY3/MODIFY,MODLEN,MODBND
      COMMON/OUTIN/IUOUT,IUIN
      COMMON/precz/zname(MCOM),zdesc(MCOM)
      COMMON/PREC9/NCONST(MCOM),NELTS(MCOM,MS),NGAPS(MCOM,MS),
     &             NPGAP(MCOM,MS,MGP)

      character CTYPE*4
      real gversion
      integer igupgrade
      COMMON/G0/CTYPE(MCOM),gversion(MCOM),igupgrade
      COMMON/G6/SSNAME(MCON),SSOTF(MCON),SSMLCN(MCON),SSVFC(MCON),
     &          SSOTHER(MCON,3),SSPARENT(MCON),SSUSE(MCON,2)
      COMMON/C1/NCOMP,NCON
      COMMON/C2/LSNAM,NCCODE(MCOM),LPROJ(MCOM),LGEOM(MCOM),
     &          LSHAD(MCOM),LTHRM(MCOM),INDUTL(MCOM),LUTIL(MCOM)
      COMMON/C20/NZSUR(MCOM),NZTV(MCOM)
      COMMON/C24/IZSTOCN(MCOM,MS)
      COMMON/MOIST01/MSTROK,MSTRZN(MCOM)
      COMMON/MOIST02/LMOIST(MCOM)
      COMMON/MOIST03/MNDS,MNDZ(MCOM),MNDC(MCOM,MS),MNDL(MCOM,MS,ME)

      LOGICAL OK,DOK,MODIFY,MODLEN,MODBND,SAVEOK
      LOGICAL ZONOK,XST,MSTROK,MSTRZN
      logical newgeo  ! to use for testing if new/old geometry file.

      CHARACTER ITEM(13)*34,ITEMC(MS)*20,FLTYP*25
      CHARACTER*12 ZONENM,ZNAME
      CHARACTER SSMLCN*32,SSVFC*4,SSOTF*32,SSOTHER*24,SSNAME*12,SSUSE*8
      character SSPARENT*12
      character outs*124,outsd*124,outs72*72,OTHSTR*30
      CHARACTER zdesc*64
      CHARACTER*72 LSNAM,LPROJ,LGEOM,LSHAD,LTHRM,LUTIL
      CHARACTER*72 LMOIST,H,TXT,MSG1,LTMP

      DIMENSION IVALZ(MCOM),IVALS(MS)

      IUNIT=IFIL+1
      IZ=0
      ZONOK=.FALSE.
      SAVEOK=.FALSE.
      newgeo=.false.  ! assume older format geometry.

   10 INO=-3
      IF(ZONOK)THEN
        ZONENM=zname(IZ)
      ELSE
        ZONENM=' '
      ENDIF
      WRITE(ITEM(1),'(A17,A15)')'1 Selected zone: ',ZONENM
      ITEM(2)='  --------------------------------'
      ITEM(3)='a Assign n_nodes for all layers'
      ITEM(4)='b Assign n_nodes for a layer'
      ITEM(5)='c Assign hygric properties'
      ITEM(6)='  --------------------------------'
      ITEM(7)='x Read 1D moisture file'
      ITEM(8)='y Save 1D moisture file'
      ITEM(9)='z Delete 1D moisture file'
      ITEM(10)='  --------------------------------'
      ITEM(11)='! List nodes distribution'
      ITEM(12)='? Help'
      ITEM(13)='- Exit'
      MITEM=13

C Help for this menu.
      H(1)='  This menu can be used to define the hygric'
      H(2)='  nodal distribution and properties.  The '
      H(3)='  zone should be selected first.'
      H(4)='  After defining the hygric data, it should '
      H(5)='  be saved via (y) option'

C Draw the appropriate graph.
      IF(MODIFY)CALL GRAAPH(IDRW1,IDRW2)
      CALL USRMSG(' ',' ','-')
      CALL EMENU('   Moisture Transport',ITEM,MITEM,INO)

      IF(.NOT.ZONOK.AND.(INO.GE.3.AND.INO.LE.5))THEN

C Trap un-accepted options.
        CALL USRMSG(' ','the zone should be defined first.','W')
      ELSEIF(.NOT.ZONOK.AND.(INO.GE.7.AND.INO.LE.9))THEN
        CALL USRMSG(' ','the zone should be defined first.','W')
      ELSEIF(INO.EQ.1)THEN

C Select the zone.
C Before selecting new zone, update the utilities file.
        SAVEOK=.FALSE.

C Show the complete building.
        IF(IDRW1.NE.1.OR.MODIFY)THEN
          IDRW1=1
          IDRW2=0
          CALL GRAAPH(IDRW1,IDRW2)
        ENDIF

C Show the zone selection menu.
        H(1)='    Select the zone for which the moisture '
        H(2)=' details are to be created/modified.'
        H(3)=' '
        INPICK=1
   20   CALL EPICKS(INPICK,IVALZ,' ',' ',12,
     &                             NCOMP,zname,' Select Zone',IER,3)
        IZ2=IVALZ(1)
        IF(IZ2.GE.1.AND.IZ2.LE.NCOMP)THEN
          IZ=IZ2
          call eclose(gversion(IZ),1.1,0.01,newgeo)
          if(newgeo)then
            call georead(IUNIT,LGEOM(IZ),IZ,1,ITRC,IUOUT,IER)
          else
            call egomin(IUNIT,LGEOM(IZ),IZ,1,ITRC,IUOUT,IER)
          endif
          IF(IER.EQ.0)CALL ECONST(LTHRM(IZ),IUNIT,IZ,ITRC,IUOUT,IER)
          IF(IER.EQ.0)THEN
            ZONOK=.TRUE.
            NC=NCONST(IZ)
          ELSE
            CALL USRMSG(' Problem detected while trying',
     &                  ' to open zone geometry file.','W')
            ZONOK=.FALSE.
          ENDIF
        ELSEIF(IZ2.EQ.0)THEN
          IF(IZ.GE.1.AND.IZ.LE.NCOMP)THEN
            ZONOK=.TRUE.
          ELSE
            ZONOK=.FALSE.
          ENDIF
          GOTO 10
        ELSE
          GOTO 20
        ENDIF
        IF(ZONOK)THEN
          IDRW1=2
          IDRW2=IZ
          CALL GRAAPH(IDRW1,IDRW2)
        ENDIF
      ELSEIF(INO.EQ.3)THEN

C Assign n-nodes for all layers in the zone.
        H(1)=' The number of nodes per layer should be at '
        H(2)=' least (2). On the other hand, the the total '
        H(3)=' number of nodes per surface should not '
        WRITE(H(4),'(A,I2,A)')' exceed (',MNM-1,').'
        CALL EASKI(NND,' ','Enter the required number of nodes',
     &             2,'F',MNM-1,'F',2,' number of nodes ',IER,4)
        MNDZ(IZ)=0
        DO 40 IS=1,NC
          NE=NELTS(IZ,IS)
          MNDC(IZ,IS)=NND*NE+1
          MNDZ(IZ)=MNDZ(IZ)+MNDC(IZ,IS)
        DO 40 ILYR=1,NE
            MNDL(IZ,IS,ILYR)=NND
   40   CONTINUE
      ELSEIF(INO.EQ.4)THEN

C Assign the number of nodes in a layer.
C Show the constructions menu.
        INPICK=1
        DO 50 ISRF=1,NC
          icc=izstocn(iz,isrf)
          ITEMC(ISRF)=SSNAME(icc)
   50   CONTINUE
        H(1)=' Select the surface in which the required layers '
        H(2)=' are located. '
   60   CALL EPICKS(INPICK,IVALS,' ',' ',20,NC,ITEMC,' ',IER,2)
        IS2=IVALS(1)
        IF(IS2.GT.0.AND.IS2.LE.NC)THEN
          IS=IS2
          icc=izstocn(iz,is)
          NE=NELTS(IZ,IS)
          WRITE(TXT,'(10(2X,I2))')(MNDL(IZ,IS,IL),IL=1,NE)
          WRITE(MSG1,'(A30,A28,A12))')" Enter the number of moisture ",
     &    "nodes/layer in the surface: ",SSNAME(icc)
          CALL EASKS(TXT,MSG1,' ',72,' ',' data ',IER,0)
          CALL CHITMS(TXT,NCFF)
          IF(NCFF.NE.NE)THEN
            CALL USRMSG(' The number of data enetered should be',
     &      ' equal to number of layers in the surface !','W')
            GOTO 10
          ENDIF
          K=0
          DO 70 IL=1,NE
            CALL EGETWI(TXT,K,MNDL(IZ,IS,IL),2,MNM-1,'F',' ',IER)
   70     CONTINUE
        ELSEIF(IS.EQ.0)THEN
          GOTO 10
        ELSE
          GOTO 60
        ENDIF
      ELSEIF(INO.EQ.5)THEN

C Assign hygric properties.
        CALL HYGRIC(IZ)
      ELSEIF(INO.EQ.7)THEN

C Read a node distribution file.
        FLTYP='moisture'
        LTMP=LMOIST(IZ)
        CALL FLNAME(IZ,LTMP,FLTYP,'.mst',0,IER)
        CALL EMSTRR(LTMP,IUNIT,IZ,ITRC,IUOUT,IER)
        LMOIST(IZ)=LTMP
      ELSEIF(INO.EQ.8)THEN

C Save the current node distrbution in a file.
        FLTYP='moisture'
        LTMP=LMOIST(IZ)
        CALL FLNAME(IZ,LTMP,FLTYP,'.mst',0,IER)
        CALL EMSTRW(LTMP,IUNIT,IZ,ITRC,IUOUT,IER)
        LMOIST(IZ)=LTMP
        IF(IER.EQ.0)THEN
          SAVEOK=.TRUE.
        ELSE
          SAVEOK=.FALSE.
        ENDIF
      ELSEIF(INO.EQ.9)THEN

C Delete an existing 1D gridding file.
        IF(MSTRZN(IZ))THEN
          LTMP=LMOIST(IZ)
          CALL FINDFIL(LTMP,XST)
          IF(XST)THEN
            CALL EFOPSEQ(IUNIT,LTMP,3,IER)
            CALL EFDELET(IUNIT,ISTAT)
          ENDIF
          LMOIST(IZ)=' '
          MSTRZN(IZ)=.FALSE.
          SAVEOK=.TRUE.
        ELSE
          FLTYP='moisture'
          LTMP=LMOIST(IZ)
          CALL FLNAME(IZ,LTMP,FLTYP,'.mst',0,IER)
          CALL FINDFIL(LTMP,XST)
          IF(XST)THEN
            CALL EFOPSEQ(IUNIT,LTMP,3,IER)
            CALL EFDELET(IUNIT,ISTAT)
            LMOIST(IZ)=' '
          ELSE
            CALL USRMSG(' ',' moisture file does not exist.','W')
          ENDIF
        ENDIF
      ELSEIF(INO.EQ.MITEM-2)THEN

C list the node distribution in the text feedback window.
        CALL LISTMD(IZ)
      ELSEIF(INO.EQ.MITEM-1)THEN

C Display the help statements.
        IHELPM=1
        CALL PHELPD('hygric nodal',5,'-',0,0,IER)
      ELSEIF(INO.EQ.MITEM)THEN

C Return to the main simulator menu.
        IF(SAVEOK)THEN
          dok=.true.
          h(1)='Recent changes to the model should be recorded in the'
          h(2)='configuration file. Before saying yes, make sure that'
          h(3)='no other ESP-r module is accessing that file. '
          CALL ASKOK(' ',
     &    'configuration should be updated. Ok to do this?',OK,dok,3)
          IF(OK)CALL EMKCFG('-',IER)
        ENDIF
        RETURN
      ENDIF

C After each option selection, except EXIT, redisplay the node 
C distribution menu after performing the request.
      GOTO 10
      END

C *********************************************************************
      SUBROUTINE HYGRIC(IZ)
#include "building.h"

      common/pophelp/h(60)
      COMMON/HELP/IHELPM
      COMMON/OUTIN/IUOUT,IUIN
      COMMON/PREC9/NCONST(MCOM),NELTS(MCOM,MS),NGAPS(MCOM,MS),
     &             NPGAP(MCOM,MS,MGP)
      COMMON/G6/SSNAME(MCON),SSOTF(MCON),SSMLCN(MCON),SSVFC(MCON),
     &          SSOTHER(MCON,3),SSPARENT(MCON),SSUSE(MCON,2)
      COMMON/C24/IZSTOCN(MCOM,MS)
      COMMON/MOIST05/IPERM(MCOM,MS,ME),PERM(MCOM,MS,ME,3)
      COMMON/MOIST06/ISORP(MCOM,MS,ME),SORP(MCOM,MS,ME,3)
      COMMON/MOIST07/ITVAPD(MCOM,MS,ME),TVAPD(MCOM,MS,ME)

      CHARACTER ITEM(10)*35,ITEMC(MS)*20
      CHARACTER SURFNM*12
      CHARACTER H*72
      CHARACTER SSMLCN*32,SSVFC*4,SSOTF*32,SSOTHER*24,SSNAME*12,SSUSE*8
      character SSPARENT*12
      CHARACTER*72 MSG1,TXT,TEXT1,TEXT2

      DIMENSION IVALS(MS),IVALL(ME)

      IS=1
      IL=1
      NC=NCONST(IZ)
      NE=NELTS(IZ,IS)

   10 INO=-3
      IF(IS.GE.1.AND.IS.LE.NC)THEN
        icc=izstocn(iz,is)
        SURFNM=SSNAME(icc)
      ELSE
        SURFNM=' '
      ENDIF
      WRITE(ITEM(1),'(A,A12)')'1 Selected surface: ',SURFNM
      WRITE(ITEM(2),'(A,I5)')'2 Selected layer: ',IL
      ITEM(3)='  -----------------------------'
      ITEM(4)='x Enter moisture permeability'
      ITEM(5)='y Enter moisture capacity'
      ITEM(6)='z Enter thermal vapour diffusion'
      ITEM(7)='  -----------------------------'
      ITEM(8)='! List hygric properties'
      ITEM(9)='? Help'
      ITEM(10)='- Exit'
      MITEM=10

C Help text for this menu.
      H(1)='  This menu can be used for defining the hygric'
      H(2)='  properties of construction layers.  It deals'
      H(3)='  with one layer at a time.'

      CALL EMENU('      Hygric Properties',ITEM,MITEM,INO)
      IF(INO.EQ.1)THEN

C Select the surface.
C Show the constructions menu.
        INPICK=1
        DO 20 IS2=1,NC
          icc=izstocn(iz,is2)
          ITEMC(IS2)=SSNAME(icc)
   20   CONTINUE
        H(1)=' Select the surface '
   30   CALL EPICKS(INPICK,IVALS,' ',' ',20,NC,ITEMC,' ',IER,1)
        IS2=IVALS(1)
        IF(IS2.GE.1.AND.IS2.LE.NC)THEN
          IS=IS2
          IL=1
          NE=NELTS(IZ,IS)
        ELSEIF(IS.EQ.0)THEN
          GOTO 10
        ELSE
          GOTO 30
        ENDIF
      ELSEIF(INO.EQ.2)THEN

C Select the layer.
C Show the layers menu.
        INPICK=1
        DO 40 IL2=1,NE
          WRITE(ITEMC(IL2),'(A7,I2,A1)')'Layer (',IL2,')'
   40   CONTINUE
        H(1)=' Select the layer '
   50   CALL EPICKS(INPICK,IVALL,' ',' ',20,NE,ITEMC,' ',IER,1)
        IL2=IVALL(1)
        IF(IL2.GE.1.AND.IL2.LE.NE)THEN
          IL=IL2
        ELSEIF(IL.EQ.0)THEN
          GOTO 10
        ELSE
          GOTO 50
        ENDIF
      ELSEIF(INO.EQ.4)THEN

C Enter moisture permeability.
        WRITE(TXT,901)IPERM(IZ,IS,IL),(PERM(IZ,IS,IL,IT),IT=1,3)
 901    FORMAT(1X,I2,6(1X,F8.4))
        WRITE(MSG1,'(A))')
     &  " Enter the moisture permeability data for the selected layer"
        CALL EASKS(TXT,MSG1,' ',72,' ',' data ',IER,0)
        CALL CHITMS(TXT,NCFF)
        IF(NCFF.NE.4)THEN
          CALL USRMSG(' ',
     &    ' one index and three coefficents are expected !','W')
          GOTO 10
        ENDIF
        K=0
        CALL EGETWI(TXT,K,IDUMM,1,1,'F',' ',IER)
        DO 60 IT=1,3
          CALL EGETWR(TXT,K,PERM(IZ,IS,IL,IT),0.,0.,'-',' ',IER)
   60   CONTINUE
      ELSEIF(INO.EQ.5)THEN

C Enter moisture capacity.
        WRITE(TXT,901)ISORP(IZ,IS,IL),(SORP(IZ,IS,IL,IT),IT=1,3)
        WRITE(MSG1,'(A))')
     &  " Enter the moisture capacity data for the selected layer"
        CALL EASKS(TXT,MSG1,' ',72,' ',' data ',IER,0)
        CALL CHITMS(TXT,NCFF)
        IF(NCFF.NE.4)THEN
          CALL USRMSG(' ',
     &    ' one index and three coefficents are expected !','W')
          GOTO 10
        ENDIF
        K=0
        CALL EGETWI(TXT,K,IDUMM,1,1,'F',' ',IER)
        DO 70 IT=1,3
          CALL EGETWR(TXT,K,SORP(IZ,IS,IL,IT),0.,0.,'-',' ',IER)
   70   CONTINUE
      ELSEIF(INO.EQ.6)THEN

C Enter thermal vapour diffusion.
        WRITE(TXT,'(1X,I2,6(1X,F8.4))')ITVAPD(IZ,IS,IL),TVAPD(IZ,IS,IL)
        WRITE(MSG1,'(A))')
     &  " Enter the moisture capacity data for the selected layer"
        CALL EASKS(TXT,MSG1,' ',72,' ',' data ',IER,0)
        CALL CHITMS(TXT,NCFF)
        IF(NCFF.NE.2)THEN
          CALL USRMSG(' ',
     &    ' one index and one coefficent are expected !','W')
          GOTO 10
        ENDIF
        K=0
        CALL EGETWI(TXT,K,IDUMM,1,1,'F',' ',IER)
        CALL EGETWR(TXT,K,TVAPD(IZ,IS,IL),0.,0.,'-',' ',IER)
      ELSEIF(INO.EQ.(MITEM-2))THEN

C List hygric properties.
        CALL EDISP(IUOUT,' ')
        WRITE(TEXT1,'(A,A)')'-------------------------------------',
     &   '-----------------------------------'
        CALL EDISP(IUOUT,TEXT1)
        WRITE(TEXT2,210)' Property','Index','coefficiets'
  210   FORMAT(A9,17X,A5,13X,A11)
        CALL EDISP(IUOUT,TEXT2)
        CALL EDISP(IUOUT,TEXT1)
        WRITE(TEXT2,220)'permeability',IPERM(IZ,IS,IL),
     &                  (PERM(IZ,IS,IL,IP),IP=1,3)
        CALL EDISP(IUOUT,TEXT2)
        WRITE(TEXT2,220)'capacity    ',ISORP(IZ,IS,IL),
     &                  (SORP(IZ,IS,IL,IP),IP=1,3)
        CALL EDISP(IUOUT,TEXT2)
        WRITE(TEXT2,230)'thermal vapour diffusion',ITVAPD(IZ,IS,IL),
     &                  TVAPD(IZ,IS,IL)
        CALL EDISP(IUOUT,TEXT2)
  220   FORMAT(1X,A12,15X,I2,5X,3(F8.4,2X))
  230   FORMAT(1X,A24,3X,I2,5X,F8.4,2X)
        CALL EDISP(IUOUT,TEXT1)
      ELSEIF(INO.EQ.(MITEM-1))THEN

C Help.
        IHELPM=1
        CALL PHELPD('hygric props',3,'-',0,0,IER)
      ELSEIF(INO.EQ.MITEM)THEN

C Exit.
         RETURN
      ENDIF
      GOTO 10
      END

C ***************************    LISTMD   *****************************
C LISTMD lists the node distribution in the text feedback window.
C *********************************************************************
      SUBROUTINE LISTMD(ICOMP)
#include "building.h"

      COMMON/OUTIN/IUOUT,IUIN
      COMMON/PREC9/NCONST(MCOM),NELTS(MCOM,MS),NGAPS(MCOM,MS),
     &             NPGAP(MCOM,MS,MGP)
      COMMON/MOIST03/MNDS,MNDZ(MCOM),MNDC(MCOM,MS),MNDL(MCOM,MS,ME)

      CHARACTER*72 TEXT1,TEXT2

      WRITE(TEXT1,'(A,A)')'------------------------------------',
     &                    '------------------------------------'
      CALL EDISP(IUOUT,TEXT1)
      WRITE(TEXT2,'(A,A)')'  Surface               ',
     &                    '     No. of nodes per layer'
      CALL EDISP(IUOUT,TEXT2)
      CALL EDISP(IUOUT,TEXT1)
      DO 20 IS=1,NCONST(ICOMP)
        NE=NELTS(ICOMP,IS)
        WRITE(TEXT2,'(3X,I2,7X,8I6)')IS,(MNDL(ICOMP,IS,IL),IL=1,NE)
        CALL EDISP(IUOUT,TEXT2)
   20 CONTINUE
      CALL EDISP(IUOUT,TEXT1)
      RETURN
      END
