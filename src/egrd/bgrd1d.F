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
C  This file contains the following 1-D node distribution subroutines:

C MENU1D generates the 1D node distribution menu. It is called via the
C        main multi-D gridding menu.
C AUTG1D generates the variable node distribution menu.
C LUMPMN generates the lumped construction menu.
C LISTND lists the node distribution in the text feedback window.
C *********************************************************************

C ******************************  MENU1D  *****************************
C MENU1D generates the 1D node distribution menu. It is called via the 
C main multi-D gridding menu.
C *********************************************************************
      SUBROUTINE MENU1D
#include "building.h"
#include "geometry.h"

      COMMON/FILEP/IFIL
      COMMON/HELP/IHELPM
      COMMON/OUTIN/IUOUT,IUIN
      COMMON/PREC9/NCONST(MCOM),NELTS(MCOM,MS),NGAPS(MCOM,MS),
     &             NPGAP(MCOM,MS,MGP)
      
      integer ncomp,ncon
      COMMON/C1/NCOMP,NCON
      integer IZSTOCN
      COMMON/C24/IZSTOCN(MCOM,MS)
      COMMON/GR1D01/NNDS,NNDZ(MCOM),NNDC(MCOM,MS),NNDL(MCOM,MS,ME)
      COMMON/GR1D02/IADP,IVAR,FMIN,FREF,FMAX,NSTP
      COMMON/GR1D03/BCNV,ILUMP(MCOM,MS)
      COMMON/GR1D05/LGRD1D
      CHARACTER LGRD1D*72
      COMMON/GR1D06/IGR1D
      LOGICAL IGR1D

      LOGICAL OK,DOK,SAVEOK,XST
      CHARACTER ITEM(14)*31,ITEMZ(MCOM)*20,ITEMC(MS)*20,ITEML(ME)*20
      CHARACTER TEXT*40
      integer MITEM,INO ! max items and current menu item

      DIMENSION IVALSZ(MCOM),IVALSC(MS),IVALSL(ME)

C For help messages
      character helpinsub*24 ! subroutine name
      character helptopic*24 ! string (unique) for topic
      integer nbhelp     ! number of help lines found

      helpinsub='MENU1D'  ! set for subroutine

      IUNIT=IFIL+1
      SAVEOK=.FALSE.
      IF(IGR1D)CALL READ1D(LGRD1D,IUNIT,IUOUT,0,IER)
   10 INO=-3
      WRITE(ITEM(1),'(A,I5)')'  TOTAL NODES       : ',NNDS
      ITEM(2)='  -----------------------------'
      ITEM(3)='a Two nodes per each layer'
      ITEM(4)='b Assign n_nodes for a layer'
      ITEM(5)='c Automatic node distribution'
      ITEM(6)='d Create lumped constructions'
      ITEM(7)='  -----------------------------'
      ITEM(8)='e Read 1D gridding file'
      ITEM(9)='f Save 1D gridding file'
      ITEM(10)='g Delete 1D gridding file'
      ITEM(11)='  -----------------------------'
      ITEM(12)='! List nodes distribution'
      ITEM(13)='? Help'
      ITEM(14)='- Exit'
      MITEM=14

C Help text for menu.
      helptopic='1D_node_def_menu'
      call gethelptext(helpinsub,helptopic,nbhelp)

      CALL EMENU('  Nodes Distribution',ITEM,MITEM,INO)
      IF(INO.EQ.3)THEN

C Assign two nodes per each layer in the building.
        IF(IGR1D)SAVEOK=.TRUE.
        IGR1D=.FALSE.

C NNDS is the total number of nodes in the system (building).
        NNDS=0
        DO 20 ICOMP=1,NCOMP
          CALL NODTWO(ICOMP)
          NNDS=NNDS+NNDZ(ICOMP)
   20   CONTINUE

C IVAR is the index for variable (automatic) gridding.
C IADP is the index for dynamic gridding.
        IVAR=0
        IADP=0
      ELSEIF(INO.EQ.4)THEN

C Assign the number of nodes in a layer.
C Show the zones menu.
   30   INPICK=1
        DO 40 IZ=1,NCOMP
          ITEMZ(IZ)=zname(IZ)
   40   CONTINUE
        CALL EPICKS(INPICK,IVALSZ,' ',' ',20,NCOMP,ITEMZ,'  ',
     &    IER,nbhelp)
        MNU1=IVALSZ(1)

C Show the constructions menu.
        IF(MNU1.GT.0.AND.MNU1.LE.NCOMP)THEN
          ICOMP=MNU1
   50     NC=NCONST(ICOMP)
          INPICK=1
          DO 60 IS=1,NC
            icn1=izstocn(icomp,is)
            IF(ILUMP(ICOMP,IS).GE.1)THEN
              ITEMC(IS)=' ** LUMPED **'
            ELSE
              ITEMC(IS)=SSNAME(icn1)
            ENDIF
   60     CONTINUE
          CALL EPICKS(INPICK,IVALSC,' ',' ',20,NC,ITEMC,' ',
     &      IER,nbhelp)
          MNU2=IVALSC(1)
          IF(MNU2.GT.0.AND.MNU2.LE.NC)THEN
            ISRFC=MNU2

C Do not allow selecting lumped constructions.
            IF(ILUMP(ICOMP,ISRFC).GE.1)GOTO 50

C Show the layers menu for un-lumped constructions.
            NE=NELTS(ICOMP,ISRFC)
   70       INPICK=1
            DO 80 J=1,NE
              WRITE(ITEML(J),'(A,1X,I2,1X,A,I3)')'LAYER (',J,') : ',
     &                 NNDL(ICOMP,ISRFC,J)
   80       CONTINUE
            CALL EPICKS(INPICK,IVALSL,' ',' ',20,NE,ITEML,' ',
     &        IER,nbhelp)
            MNU3=IVALSL(1)
            IF(MNU3.GT.0.AND.MNU3.LE.NE)THEN
              IL=MNU3
              helptopic='nodes_per_layer'
              call gethelptext(helpinsub,helptopic,nbhelp)
              WRITE(TEXT,'(A,I2,A,I2,A,I2,A)')'for layer (',ICOMP,
     &                                      ',',ISRFC,',',IL,') :'

C Enter the required number of nodes for the selected layer.
              CALL EASKI(NND,'Enter the required number of nodes',TEXT,
     &          2,'F',MN-1,'F',2,' number of nodes ',IER,nbhelp)
              IDEL=NND-NNDL(ICOMP,ISRFC,IL)
              ICHKC=NNDC(ICOMP,ISRFC)+IDEL

C Check if the entered number of nodes per layer is acceptable or not.
C NB: the total number of nodes per a construction should not exceed (MN).
              IF(ICHKC.LE.MN)THEN
                NNDL(ICOMP,ISRFC,IL)=NND
                NNDC(ICOMP,ISRFC)=NNDC(ICOMP,ISRFC)+IDEL
                NNDZ(ICOMP)=NNDZ(ICOMP)+IDEL
                NNDS=NNDS+IDEL
              ELSE
                CALL USRMSG('required number of nodes is not',
     &                  'acceptable,.. last command cancelled !','W')
              ENDIF
              GOTO 70
            ELSEIF(IVALSL(1).EQ.0)THEN
              GOTO 50
            ELSE
              GOTO 70
            ENDIF
          ELSEIF(IVALSC(1).EQ.0)THEN
            GOTO 30
          ELSE 
            GOTO 50
          ENDIF
        ELSEIF(IVALSZ(1).EQ.0)THEN
          GOTO 10
        ELSE
          GOTO 30
        ENDIF
      ELSEIF(INO.EQ.5)THEN

C Automatic grid distribution is required.
        CALL AUTG1D
      ELSEIF(INO.EQ.6)THEN

C Creating lumped construction is required.
        CALL LUMPMN
      ELSEIF(INO.EQ.8)THEN

C Read a node distribution file.
        helptopic='1D_node_def_menu'
        call gethelptext(helpinsub,helptopic,nbhelp)
        CALL EASKS(LGRD1D,' 1D gridding configuration file name ?',' ',
     &    72,' ',' gridding confg. file ',IER,nbhelp)
        CALL READ1D(LGRD1D,IUNIT,IUOUT,ITRC,IER)
      ELSEIF(INO.EQ.9)THEN

C Save the current node distrbution in a file.
        CALL EASKS(LGRD1D,' 1D gridding configuration file name ?',' ',
     &    72,' ',' gridding confg. file ',IER,nbhelp)
        CALL SAVE1D(LGRD1D,IUNIT,IUOUT,ITRC,IER)
        IF(IER.EQ.0)THEN
          SAVEOK=.TRUE.
          IGR1D=.TRUE.
        ENDIF
      ELSEIF(INO.EQ.10)THEN

C Delete an existing 1D gridding file.
        IF(IGR1D)THEN
          CALL FINDFIL(LGRD1D,XST)
          IF(XST)THEN
            CALL EFOPSEQ(IUNIT,LGRD1D,3,IER)
            CALL EFDELET(IUNIT,ISTAT)
          ENDIF
          LGRD1D=' '
          IGR1D=.FALSE.
          SAVEOK=.TRUE.
        ELSE
          CALL USRMSG(' ',' 1D gridding file does not exist.','W')
        ENDIF
      ELSEIF(INO.EQ.MITEM-2)THEN

C list the node distribution in the text feedback window.
        CALL LISTND
      ELSEIF(INO.EQ.MITEM-1)THEN

C Display the help statements.
        helptopic='1D_node_def_menu'
        call gethelptext(helpinsub,helptopic,nbhelp)
        IHELPM=1
        CALL PHELPD('node distrib ',nbhelp,'-',0,0,IER)
      ELSEIF(INO.EQ.MITEM)THEN

C Return to the main simulator menu.
        IF(SAVEOK)THEN
          dok=.true.
          helptopic='save_recent_changes'
          call gethelptext(helpinsub,helptopic,nbhelp)
          CALL ASKOK(
     &      'The system configuration should be updated for changes.',
     &      '(if no other module is using it). Do this?',
     &      OK,dok,nbhelp)
          IF(OK)CALL EMKCFG('-',IER)
        ENDIF
        RETURN
      ENDIF

C After each option selection, except EXIT, redisplay the node distribution
C menu after performing the request.
      GOTO 10
      END

C *******************************  AUTG1D  ****************************
C AUTG1D generates the variable node distribution menu.
C *********************************************************************
      SUBROUTINE AUTG1D
#include "building.h"

      COMMON/HELP/IHELPM
      COMMON/OUTIN/IUOUT,IUIN
      COMMON/PREC9/NCONST(MCOM),NELTS(MCOM,MS),NGAPS(MCOM,MS),
     &             NPGAP(MCOM,MS,MGP)
      COMMON/PREC15/RGAPS(MCOM,MS,MGP)
      
      integer ncomp,ncon
      COMMON/C1/NCOMP,NCON
      COMMON/VTHP14/THRMLI(MCOM,MS,ME,7)
      COMMON/GR1D01/NNDS,NNDZ(MCOM),NNDC(MCOM,MS),NNDL(MCOM,MS,ME)
      COMMON/GR1D02/IADP,IVAR,FMIN,FREF,FMAX,NSTP
      COMMON/GR1D03/BCNV,ILUMP(MCOM,MS)
      COMMON/GR1D04/GAM,RGAM

      CHARACTER ITEM(14)*30,TEXT2*120,ONFF*3
      integer NITMS,INO ! max items and current menu item

      DIMENSION FRN(MS,ME)

C For help messages
      character helpinsub*24 ! subroutine name
      character helptopic*24 ! string (unique) for topic
      integer nbhelp     ! number of help lines found

      helpinsub='AUTG1D'  ! set for subroutine

      IF(IADP.EQ.1)THEN
         ONFF='ON '
      ELSE
         ONFF='OFF'
      ENDIF
   10 INO=-3

C The required time step in seconds.
      TMR=3600./FLOAT(NSTP)
      WRITE(ITEM(1),'(A,I5)')'  TOTAL NODES      :',NNDS
      IF(IVAR.EQ.1)THEN
        WRITE(ITEM(2),'(A,A)')'1 DYNAMIC GRIDDING :  ',ONFF
      ELSE
        ITEM(2)='  DYNAMIC GRIDDING :'
      ENDIF
      ITEM(3)='  ----------------------------'
      WRITE(ITEM(4),'(A,F10.3)')'a Minimum   (Fo)  :',FMIN
      WRITE(ITEM(5),'(A,F10.3)')'b Reference (Fo)  :',FREF
      WRITE(ITEM(6),'(A,F10.3)')'c Maximum   (Fo)  :',FMAX
      WRITE(ITEM(7),'(A,I3)')   'd Required (step/hr): ',NSTP
      ITEM(8)='  ----------------------------'
      ITEM(9)='e Invoke node distribution'
      ITEM(10)='  ----------------------------'
      ITEM(11)='f List layers Fourier No'
      ITEM(12)='! List nodes distribution'
      ITEM(13)='? Help'
      ITEM(14)='- Exit'

C Help text for menu.
      helptopic='variable_node_distr'
      call gethelptext(helpinsub,helptopic,nbhelp)

      CALL USRMSG(' ',' ','-')
      NITMS=14
      CALL EMENU('  Automatic Node Distribution',ITEM,NITMS,INO)
      IF(INO.EQ.2.AND.IVAR.EQ.1)THEN

C Switching the dynamic gridding index.
        IF(IADP.EQ.1)THEN
          IADP=0
          ONFF='OFF'
        ELSE
          IADP=1
          ONFF='ON '
        ENDIF
      ELSEIF(INO.EQ.4)THEN

C Assign the minimum Fourier number for, all layers, below which a warning
C will be given.
        CALL EASKR(FMIN,' ','Enter the minimum allowable Fourier No.:',
     &     0.,'F',FREF,'W',.5,' Minimum (Fo) ',IER,nbhelp)
      ELSEIF(INO.EQ.5)THEN

        CALL EASKR(FREF,' ','Enter the reference Fourier No. :',
     &    FMIN,'F',FMAX,'F',.5,' Reference (Fo) ',IER,nbhelp)
      ELSEIF(INO.EQ.6)THEN

C Assign the maximum Fourier number for, all layers, above which a warning
C will be given.
        IF(GAM.GT.0.999999)THEN
          FMAXD=999999.
        ELSE
          FMAXD=0.25/(1.-GAM)
        ENDIF
        CALL EASKR(FMAX,' ',
     &   'Enter the maximum allowable Fourier No. :',FREF,
     &   'W',1.,'-',FMAXD,' Maximum (Fo) ',IER,nbhelp)
      ELSEIF(INO.EQ.7)THEN

C Assign the building side number of time steps per hour
        CALL EASKI(NSTP,'Enter the building side  ',
     &    'number of time steps per hour:',1,'F',60,
     &    'F',1,' No. of time step/hr ',IER,nbhelp)
      ELSEIF(INO.EQ.9)THEN

C Invoke the automatic node distribution.
        IVAR=1
        DO 30 ICOMP=1,NCOMP
        DO 30 I=1,NCONST(ICOMP)
          ILUMP(ICOMP,I)=0
          CALL NDCONS(ICOMP,I)
   30   CONTINUE
      ELSEIF(INO.EQ.11)THEN

C Display the Fourier number for all the layers in the building.
        DO 60 ICOMP=1,NCOMP
          WRITE(TEXT2,'(A,A,I2,A)')'Surface               ',
     &         ' Fourier number for each layer in zone (',ICOMP,')'
          CALL EDISP(IUOUT,TEXT2)
          WRITE(TEXT2,'(A,A)')'-------------------------------------',
     &         '-----------------------------------------------------'
          CALL EDISP(IUOUT,TEXT2)
          DO 70 I=1,NCONST(ICOMP)
            NE=NELTS(ICOMP,I)
            NGP=NGAPS(ICOMP,I)
            M=1
            DO 80 J=1,NE
              IF(M.LE.NGP)THEN
                 IGP=NPGAP(ICOMP,I,M)
              ELSE
                IGP=-1
              ENDIF
              IF(IGP.EQ.J)THEN
                TERM=1.3*1005.5*RGAPS(ICOMP,I,M)/
     &               (TMR*THRMLI(ICOMP,I,J,4))
                M=M+1
              ELSE
                TERM=THRMLI(ICOMP,I,J,2)*
     &              THRMLI(ICOMP,I,J,3)/(THRMLI(ICOMP,I,J,1)*TMR)
              ENDIF
              FRN(I,J)=FLOAT(NNDL(ICOMP,I,J))**2./
     &                    (THRMLI(ICOMP,I,J,4)**2.*TERM)
   80       CONTINUE
C Non-standard fortran
C           WRITE(TEXT2,'(I3,2X,<NE>F10.2)')I,(FRN(I,J),J=1,NE)
            WRITE(TEXT2,'(I3,2X,8F10.2)')I,(FRN(I,J),J=1,NE)
            CALL EDISP(IUOUT,TEXT2)
   70     CONTINUE
          CALL EPAGEW
   60   CONTINUE
      ELSEIF(INO.EQ.12)THEN

C Display the nodes distribution throughout the building.
        CALL LISTND
      ELSEIF(INO.EQ.13)THEN

C Display the help statements.
        IHELPM=1
        CALL PHELPD('variable node',39,'-',0,0,IER)
      ELSEIF(INO.EQ.14)THEN 

C Return to the nodes distribution menu.
        RETURN
      ENDIF

C After each option selection, except EXIT, redisplay the automatic node 
C distribution menu after performing the request.
      GOTO 10
      END

C ******************************  LUMPMN  *****************************
C LUMPMN generates the lumped construction menu.
C *********************************************************************
      SUBROUTINE LUMPMN
#include "building.h"
#include "geometry.h"

      COMMON/FILEP/IFIL
      COMMON/HELP/IHELPM
      COMMON/OUTIN/IUOUT,IUIN
      COMMON/PREC9/NCONST(MCOM),NELTS(MCOM,MS),NGAPS(MCOM,MS),
     &             NPGAP(MCOM,MS,MGP)
      COMMON/PREC15/RGAPS(MCOM,MS,MGP)
      COMMON/PRECTC/ITMCFL(MCOM,MS),TMCT(MCOM,MTMC,5),
     &       TMCA(MCOM,MTMC,ME,5),TMCREF(MCOM,MTMC),TVTR(MCOM,MTMC)
      
      integer ncomp,ncon
      COMMON/C1/NCOMP,NCON
      COMMON/C3/IC1(MCON),IE1(MCON),ICT(MCON),IC2(MCON),IE2(MCON)
      integer IZSTOCN
      COMMON/C24/IZSTOCN(MCOM,MS)

      COMMON/VTHP14/THRMLI(MCOM,MS,ME,7)
      COMMON/GR1D01/NNDS,NNDZ(MCOM),NNDC(MCOM,MS),NNDL(MCOM,MS,ME)
      COMMON/GR1D03/BCNV,ILUMP(MCOM,MS)

      DIMENSION IVALSZ(MCOM),IVALSC(MS),HCSRF(MCOM,MS,2)
      DIMENSION BIOTNO(MCOM,MS)

      LOGICAL OK,dok

      CHARACTER ITEM(15)*31,TEXT2*120,MSG1*50
      CHARACTER ITEMZ(MCOM)*20,ITEMC(MS+2)*28,HCFILE*72,OUTSTR*124
      integer NITMS,INO ! max items and current menu item

C For help messages
      character helpinsub*24 ! subroutine name
      character helptopic*24 ! string (unique) for topic
      integer nbhelp     ! number of help lines found

      helpinsub='LUMPMN'  ! set for subroutine

C Initialise the external and internal convective heat transfer 
C coefficient.
      DO 10 ICOMP=1,NCOMP
      DO 10 I=1,NCONST(ICOMP)
        HCSRF(ICOMP,I,1)=5.
        HCSRF(ICOMP,I,2)=5.
   10 CONTINUE
      IBIOT=0
   30 INO=-3
      WRITE(ITEM(1),'(A,I5)')  '  TOTAL NODES: ',NNDS
      ITEM(2)='  -----------------------------'
      ITEM(3)='a Assign (hc) for a surface'
      ITEM(4)='b Assign (hc) for the building'
      ITEM(5)='  -----------------------------'
      ITEM(6)='c Input (hc) from a file'
      ITEM(7)='d save (hc) to a file'
      ITEM(8)='  -----------------------------'
      ITEM(9)='e Create lumped capacities'
      ITEM(10)='  -----------------------------'
      ITEM(11)='f List (hc) values'
      ITEM(12)='h List Biot number values'
      ITEM(13)='! List nodes distribution'
      ITEM(14)='? Help'
      ITEM(15)='- Exit'

C Help text for this menu.
      helptopic='lumped_constr_menu'
      call gethelptext(helpinsub,helptopic,nbhelp)

      CALL USRMSG(' ',' ','-')
      NITMS=15
      CALL EMENU(' Lumped Constructions',ITEM,NITMS,INO)
      IF(INO.EQ.3)THEN

C Assign the (hc) value for a surface.
C Display the available zones menu.
   40   INPICK=1
        DO 50 IZ=1,NCOMP
          ITEMZ(IZ)=zname(IZ)
   50   CONTINUE
        CALL EPICKS(INPICK,IVALSZ,' ',' ',20,NCOMP,ITEMZ,'  ',
     &    IER,nbhelp)
        MNU1=IVALSZ(1)

C Display the available constructions menu.
        IF(MNU1.GT.0.AND.MNU1.LE.NCOMP)THEN
          ICOMP=MNU1
          NC=NCONST(ICOMP)
          INPICK=1
          IEXT=1
   60     IF(IEXT.EQ.1)THEN
            ITEMC(1)='  External (hc) values'
            MSG1='For this surface, enter the value of external'
          ELSE
            ITEMC(1)='  Internal (hc) values'
            MSG1='For this surface, enter the value of internal'
          ENDIF
          ITEMC(2)='----------------------------'
          DO 70 IS=1,NC
            icn1=izstocn(icomp,is)
            WRITE(ITEMC(IS+2),'(A,A,F7.1)')SSNAME(icn1),':',
     &              HCSRF(ICOMP,IS,IEXT)
   70     CONTINUE
          CALL EPICKS(INPICK,IVALSC,' ',' ',28,NC+2,ITEMC,' ',
     &      IER,nbhelp)
          MNU2=IVALSC(1)
          IF(MNU2.EQ.1.AND.IEXT.EQ.2)THEN
            IEXT=1
            GOTO 60
          ELSEIF(MNU2.EQ.1.AND.IEXT.EQ.1)THEN
            IEXT=2
            GOTO 60
          ELSEIF(MNU2.GT.2.AND.MNU2.LE.NC+2)THEN
            ISRFC=MNU2-2
            CALL EASKR(HCSRF(ICOMP,ISRFC,IEXT),MSG1,
     &        'convective heat transfer coefficient :',0.,'F',
     &        10.,'W',5.,' conv. heat coeff. ',IER,nbhelp)
            GOTO 60
          ELSEIF(MNU2.EQ.0)THEN
            GOTO 40
          ELSE
            GOTO 60
          ENDIF
        ELSEIF(MNU1.EQ.0)THEN
          GOTO 30
        ELSE
          GOTO 40
        ENDIF
      ELSEIF(INO.EQ.4)THEN

C Assign (hc) for the complete building.
        CALL EASKR(BCNV,'Enter the value of external and internal',
     &    'convective heat transfer coefficient :',0.,'F',
     &    10.,'W',5.,' Internal conv. coeff. ',IER,nbhelp)
        DO 80 ICOMP=1,NCOMP
        DO 80 I=1,NCONST(ICOMP)
          HCSRF(ICOMP,I,1)=BCNV
          HCSRF(ICOMP,I,2)=BCNV
   80   CONTINUE
      ELSEIF(INO.EQ.6)THEN

C Read the (hc) values from a file.
  100   CALL EASKS(HCFILE,' Hc distribution file name ?',' ',
     &    72,' ',' (hc) distr. file ',IER,nbhelp)
        IUNIT=IFIL+1
        CALL EFOPSEQ(IUNIT,HCFILE,1,IER)
        IF(IER.LT.0)THEN
          dok=.false.
          helptopic='hc_distrib_not_found'
          call gethelptext(helpinsub,helptopic,nbhelp)
          CALL ASKOK(' Problem detected while trying to open ',
     &      ' (hc) distribution file... try again? ',OK,dok,nbhelp)
          IF(OK)THEN
            GOTO 100
          ELSE
            CALL ERPFREE(IUNIT,ISTAT)
            GOTO 30
          ENDIF
        ENDIF 
        DO 110 ICOMP=1,NCOMP
          NC=NCONST(ICOMP)
          DO 120 I=1,NC
            CALL STRIPC(IUNIT,OUTSTR,2,ND,1,
     &          ' (hc) distribution within zone ',IER)
            IF(IER.NE.0.OR.ND.NE.2)GOTO 1000
            K=0
            CALL EGETWR(OUTSTR,K,HCSRF(ICOMP,I,1),0.,10000.,
     &                      'F',' (hc) external  ',IER)
            CALL EGETWR(OUTSTR,K,HCSRF(ICOMP,I,2),0.,10000.,
     &                      'F',' (hc) internal  ',IER)
            IF(IER.NE.0)GOTO 1000
  120     CONTINUE
  110   CONTINUE
        CALL EDISP(IUOUT,' (hc) distribution file read succesfuly !')
        GOTO 1001
 1000   CALL USRMSG(' ',' Error in (hc) distribution file !','W')
 1001   CALL ERPFREE(IUNIT,ISTAT)
      ELSEIF(INO.EQ.7)THEN

C Save the current (hc) values in a file.
  150   CALL EASKS(HCFILE,' Grid configuration file name ?',' ',
     &    72,' ',' grid confg. file ',IER,nbhelp)
        IUNIT=IFIL+1
        CALL EFOPSEQ(IUNIT,HCFILE,4,IER)
        IF(IER.LT.0)THEN 
          dok=.false.
          helptopic='hc_distrib_not_found'
          call gethelptext(helpinsub,helptopic,nbhelp)
          CALL ASKOK(' Problem detected while trying to open ',
     &      ' (hc) distribution file... try again?',OK,dok,nbhelp)
          IF(OK)THEN
            GOTO 150
          ELSE
            CALL ERPFREE(IUNIT,ISTAT)
            GOTO 30
          ENDIF
        ENDIF 
        DO 160 ICOMP=1,NCOMP
          NC=NCONST(ICOMP)
        DO 160 I=1,NC
          WRITE(IUNIT,'(2F10.3)',IOSTAT=ISTAT,ERR=1100)
     &            HCSRF(ICOMP,I,1),HCSRF(ICOMP,I,2)
  160   CONTINUE
        CALL EDISP(IUOUT,' (hc) distribution saved succesfuly !')
        GOTO 1101
 1100   CALL USRMSG(' Error in (hc) distribution file !',
     &                    ' last command ignored !','W')
 1101   CALL ERPFREE(IUNIT,ISTAT)
      ELSEIF(INO.EQ.9)THEN

C Create lumped constructions.
C First, check the legality of the (hc) values assigned.
        DO 300 LCN=1,NCON
          IF(ICT(LCN).EQ.3)THEN
            LZ1=IC1(LCN)
            LC1=IE1(LCN)
            LZ2=IC2(LCN)
            LC2=IE2(LCN)
            DELT=HCSRF(LZ1,LC1,1)-HCSRF(LZ2,LC2,2)
            IF(ABS(DELT).GT.1.E-4)THEN
              CALL USRMSG(' ','Missmatched hc values at partitions','W')
              GOTO 30
            ENDIF
          ENDIF
  300   CONTINUE

C Initialise the node distribution variables.
        DO 310 IZ1=1,NCOMP
        DO 310 IS1=1,NCONST(IZ1)
          IF(ILUMP(IZ1,IS1).GE.1)THEN
            NNDS=NNDS+ILUMP(IZ1,IS1)-2
            NNDZ(IZ1)=NNDZ(IZ1)+ILUMP(IZ1,IS1)-2
            NNDC(IZ1,IS1)=ILUMP(IZ1,IS1)
            ILUMP(IZ1,IS1)=0
          ENDIF
  310   CONTINUE
        IBIOT=1
        ICON=0

C Calculate the Biot number for all constructions in the building.
        DO 180 ICOMP=1,NCOMP
        DO 180 I=1,NCONST(ICOMP)
          ICON=ICON+1
          CNDTVTY=1000.
          CLENGTH=0.
          NGP=NGAPS(ICOMP,I)
          M=1
          DO 200 J=1,NELTS(ICOMP,I)

C If the current layer is an air gap.
            IF(M.LE.NGP)THEN
              IGP=NPGAP(ICOMP,I,M)
            ELSE
              IGP=-1
            ENDIF
            IF(IGP.EQ.J)THEN
              GAPSK=THRMLI(ICOMP,I,J,4)/RGAPS(ICOMP,I,M)
              CNDTVTY=AMIN1(CNDTVTY,GAPSK)
              CLENGTH=CLENGTH+THRMLI(ICOMP,I,J,4)
              M=M+1

C If the current layer is a solid.
            ELSE
              CNDTVTY=AMIN1(CNDTVTY,THRMLI(ICOMP,I,J,1))
              CLENGTH=CLENGTH+THRMLI(ICOMP,I,J,4)
            ENDIF
  200     CONTINUE
          HTCF=AMAX1(HCSRF(ICOMP,I,1),HCSRF(ICOMP,I,2))

C The existance of (1.E-15) is to ensure domain value > zero.
          CLENGTH=CLENGTH*(HTCF/
     &              (HCSRF(ICOMP,I,1)+HCSRF(ICOMP,I,2)+1.E-15))
          BIOTNO(ICOMP,I)=HTCF*CLENGTH/CNDTVTY

C If the Biot number is <= 0.1  and the construction is not transparent.
          IF(BIOTNO(ICOMP,I).LT.0.1.AND.ITMCFL(ICOMP,I).EQ.0)THEN
            IF(ILUMP(ICOMP,I).EQ.0)THEN
              WRITE(TEXT2,'(2(A,I2),A)')'Construction (',
     &        ICOMP,',',I,') is replaced by three lumped capacities'
              CALL EDISP(IUOUT,TEXT2)
              ILUMP(ICOMP,I)=NNDC(ICOMP,I)
              NNDS=NNDS-NNDC(ICOMP,I)+3
              NNDZ(ICOMP)=NNDZ(ICOMP)-NNDC(ICOMP,I)+3
              NNDC(ICOMP,I)=3
            ENDIF
            IF(ICT(ICON).EQ.3)THEN
              IZ2=IC2(ICON)
              IS2=IE2(ICON)
              IF(ILUMP(IZ2,IS2).EQ.0)THEN
                WRITE(TEXT2,'(A,2(I2,A))')'Construction (',IZ2,
     &          ',',IS2,') is replaced by three lumped capacities'
                CALL EDISP(IUOUT,TEXT2)
                ILUMP(IZ2,IS2)=NNDC(IZ2,IS2)
                NNDS=NNDS-NNDC(IZ2,IS2)+3
                NNDZ(ICOMP)=NNDZ(IZ2)-NNDC(IZ2,IS2)+3
                NNDC(IZ2,IS2)=3
              ENDIF
            ENDIF
          ELSE
            ILUMP(ICOMP,I)=0
          ENDIF
  180   CONTINUE
      ELSEIF(INO.EQ.11)THEN

C Display the (hc) values in the text feedback window.
C        CALL EPAGE
        DO 210 ICOMP=1,NCOMP
          WRITE(TEXT2,'(A,A)')'Zone     Surface         ',
     &                        '(hc) external       (hc) internal '
          CALL EDISP(IUOUT,TEXT2)
          WRITE(TEXT2,'(A,A)')'---------------------------------',
     &                        '---------------------------'
          CALL EDISP(IUOUT,TEXT2)
          I=1
          WRITE(TEXT2,'(I3,7X,I3,14X,F8.2,12X,F8.2)')ICOMP,I,
     &                                     (HCSRF(ICOMP,I,J),J=1,2)
          CALL EDISP(IUOUT,TEXT2)
          DO 220 I=2,NCONST(ICOMP)
            WRITE(TEXT2,'(10X,I3,14X,F8.2,12X,F8.2)')I,
     &                                    (HCSRF(ICOMP,I,J),J=1,2)
            CALL EDISP(IUOUT,TEXT2)
  220     CONTINUE
          CALL EPAGEW
  210   CONTINUE
      ELSEIF(INO.EQ.12)THEN

C Display the Biot number values in the text feedback window.
        IF(IBIOT.EQ.0)THEN
          CALL USRMSG('The Biot numbers are not available.',
     &        'Option (e) must be selected to estimate first','W')
          GOTO 30
        ENDIF
C        CALL EPAGE
        DO 230 ICOMP=1,NCOMP
          CALL EDISP(IUOUT,' Zone    Surface            Biot number')
          CALL EDISP(IUOUT,'----------------------------------------')
          I=1
          WRITE(TEXT2,'(I3,7X,I3,14X,F9.2)')ICOMP,I,BIOTNO(ICOMP,I)
          CALL EDISP(IUOUT,TEXT2)
          DO 235 I=2,NCONST(ICOMP)
            WRITE(TEXT2,'(10X,I3,14X,F9.2)')I,BIOTNO(ICOMP,I)
            CALL EDISP(IUOUT,TEXT2)
  235     CONTINUE
          CALL EPAGEW
  230   CONTINUE
      ELSEIF(INO.EQ.13)THEN

C Display the node distribution in the feedback window.
        CALL LISTND
      ELSEIF(INO.EQ.14)THEN

C Display the help statements.
        helptopic='lumped_constr_menu'
        call gethelptext(helpinsub,helptopic,nbhelp)
        IHELPM=1
        CALL PHELPD('lumped',nbhelp,'-',0,0,IER)
      ELSEIF(INO.EQ.15)THEN

C Return to the 1-D gridding menu.
        RETURN
      ENDIF
      GOTO 30
      END

C ***************************    LISTND   *****************************
C LISTND lists the node distribution in the text feedback window.
C *********************************************************************
      SUBROUTINE LISTND
#include "building.h"

      COMMON/OUTIN/IUOUT,IUIN
      
      integer ncomp,ncon
      COMMON/C1/NCOMP,NCON
      COMMON/PREC9/NCONST(MCOM),NELTS(MCOM,MS),NGAPS(MCOM,MS),
     &             NPGAP(MCOM,MS,MGP)
      COMMON/GR1D01/NNDS,NNDZ(MCOM),NNDC(MCOM,MS),NNDL(MCOM,MS,ME)
      COMMON/GR1D03/BCNV,ILUMP(MCOM,MS)

      CHARACTER TEXT2*120

C      CALL EPAGE
      DO 10 ICOMP=1,NCOMP
        I=1
        WRITE(TEXT2,'(A,A)')'  Zone    Surface               ',
     &                      '     No. of nodes per layer'
        CALL EDISP(IUOUT,TEXT2)
        WRITE(TEXT2,'(A,A)')'------------------------------------',
     &                      '------------------------------------'
        CALL EDISP(IUOUT,TEXT2)
        IF(ILUMP(ICOMP,I).GE.1)THEN
          WRITE(TEXT2,'(3X,I2,7X,I2,7X,A)')ICOMP,I,
     &               '        *******  Lumped Construction  *******'
        ELSE
          NE=NELTS(ICOMP,I)
          WRITE(TEXT2,'(3X,I2,7X,I2,7X,8I6)')ICOMP,I,
     &                          (NNDL(ICOMP,I,J),J=1,NE)
        ENDIF
        CALL EDISP(IUOUT,TEXT2)
        DO 20 I=2,NCONST(ICOMP)
          IF(ILUMP(ICOMP,I).GE.1)THEN
            WRITE(TEXT2,'(12X,I2,7X,A)')I,
     &            '        *******  Lumped Construction  *******'
          ELSE
            NE=NELTS(ICOMP,I)
            WRITE(TEXT2,'(12X,I2,7X,8I6)')I,(NNDL(ICOMP,I,J),J=1,NE)
          ENDIF
          CALL EDISP(IUOUT,TEXT2)
   20   CONTINUE
        CALL EPAGEW
   10 CONTINUE
      RETURN
      END
