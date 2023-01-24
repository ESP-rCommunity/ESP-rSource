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


C This file contains the following subroutines for creating the 
C linear thermal conductivity menues:
C        lkmenu
C        dfnmnu
C        kbldng
C        kchkmn
C        lkzone
C        surfmn
C        kconst
C        klayer
C        layrmn
C        kindex
C        lklist
C        kcheck

C ******************************  LKMENU  *****************************
C LKMENU is called by INITPF (bps.F). It displays the main 
C thermal conductivity menu and according to user selections it 
C calls other subroutines or returns back to INITPF (bld.F or bps.F).
C ********************************************************************
      SUBROUTINE LKMENU(ICOMP)
#include "building.h"
#include "geometry.h"
#include "help.h"

      COMMON/HELP/IHELPM
      COMMON/OUTIN/IUOUT,IUIN,IEOUT
      integer IZSTOCN
      COMMON/C24/IZSTOCN(MCOM,MS)
      COMMON/T1/NE(MS),NAIRG(MS),IPAIRG(MS,MGP),RAIRG(MS,MGP)
      COMMON/VTHP02/IVKON(MCOM,MS,ME) 

      CHARACTER ITEM(9)*29,DPNDC*20
      CHARACTER TEXT*50,MSGSET*70
      integer NITEM,INO ! max items and current menu item
      LOGICAL OK

      helpinsub='blcond'  ! set for subroutine

  10  INO=-3
      ITEM(1)='a constant                   '
      ITEM(2)='b function of temperature    '
      ITEM(3)='c function of moisture       '
      ITEM(4)='d function of temp. & moist. '
      ITEM(5)='  _________________________  '
      ITEM(6)='<  reset to constant         '
      ITEM(7)='!  list layer dependencies   '
      ITEM(8)='?  help                      '
      ITEM(9)='-  exit menu                 '
      NITEM=9

C Helpl text for this menu.
      helptopic='adjustable_thermal_cond'
      call gethelptext(helpinsub,helptopic,nbhelp)

      CALL EMENU('Thermal Conductivity',ITEM,NITEM,INO)

C To define the layers of constant thermal conductivity.
      IF(INO.EQ.1)THEN
        TEXT='                 CONSTANT.  '

C Call DFNMNU , to define the range of layers to be set to constant
        CALL DFNMNU(TEXT,ISLC2)

C To define the whole system (building) as constant.
        IF(ISLC2.EQ.1)THEN
          CALL LKZONE(ICOMP,0)

C Define by multilayered constructions.
        ELSEIF(ISLC2.EQ.2)THEN
          CALL KCONST(ICOMP,0,TEXT)

C Defining by layers.
        ELSEIF(ISLC2.EQ.3)THEN
          CALL KLAYER(ICOMP,0,TEXT)
        ENDIF

C To define the layers of temperature dependent thermal conductivity.
      ELSEIF(INO.EQ.2)THEN
        TEXT='           TEMPERATURE  dependent. '
        CALL DFNMNU(TEXT,ISLC2)
        IF(ISLC2.EQ.1)THEN
          CALL LKZONE(ICOMP,1)
        ELSEIF(ISLC2.EQ.2)THEN
          CALL KCONST(ICOMP,1,TEXT)
        ELSEIF(ISLC2.EQ.3)THEN
          CALL KLAYER(ICOMP,1,TEXT)
        ENDIF

C To define the layers of moisture content dependent thermal conductivity.
      ELSEIF(INO.EQ.3)THEN
        TEXT='          MOISTURE CONTENT dependent. '
        CALL DFNMNU(TEXT,ISLC2)
        IF(ISLC2.EQ.1)THEN
          CALL LKZONE(ICOMP,2)
        ELSEIF(ISLC2.EQ.2)THEN
          CALL KCONST(ICOMP,2,TEXT)
        ELSEIF(ISLC2.EQ.3)THEN
          CALL KLAYER(ICOMP,2,TEXT)
        ENDIF

C To define the layers of temp. & moisture dependent thermal conductivity.
      ELSEIF(INO.EQ.4)THEN
        TEXT='       TEMP. &  MOISTURE CONTENT dependent.'
        CALL DFNMNU(TEXT,ISLC2)
        IF(ISLC2.EQ.1)THEN
          CALL LKZONE(ICOMP,3)
        ELSEIF(ISLC2.EQ.2)THEN
          CALL KCONST(ICOMP,3,TEXT)
        ELSEIF(ISLC2.EQ.3)THEN
          CALL KLAYER(ICOMP,3,TEXT)
        ENDIF

C Reset all layers to CONSTANT without warning messages.
      ELSEIF(INO.EQ.(NITEM-3))THEN
        CALL EDISP(IUOUT,'  ')
        WRITE(MSGSET,'(A,A)')
     &        'The thermal conductivity of all building',
     &        ' layers RESET to CONSTANT.'
        CALL EDISP(IUOUT,MSGSET)
        CALL LKZONE(ICOMP,-1)

C List the thermal conductivity of the layers.
      ELSEIF(INO.EQ.(NITEM-2))THEN
        MSGSET=' Surface Name   Layer Number      Dependence Type'
        CALL EDISP(IUOUT,MSGSET)
        MSGSET=' -------------------------------------------------'
        CALL EDISP(IUOUT,MSGSET)
        CALL EDISP(IUOUT,'  ')
        DO 20 I=1,NZSUR(icomp)
          ioc=IZSTOCN(icomp,i)
          NGP=NAIRG(I)
          M=1
          DO 30 J=1,NE(I)
            IF(NGP.GT.0.AND.M.LE.NGP)THEN
              IG=IPAIRG(I,M)
            ELSE
              IG=-1
            ENDIF
            IF(J.EQ.IG)THEN
              M=M+1
              DPNDC='Air Gap         '
            ELSEIF(IVKON(ICOMP,I,J).EQ.1)THEN
              DPNDC='Temperature     '
            ELSEIF(IVKON(ICOMP,I,J).EQ.2)THEN
              DPNDC='Moisture Content'
            ELSEIF(IVKON(ICOMP,I,J).EQ.3)THEN
              DPNDC='Temp. & Moisture'
            ELSE
              DPNDC='Constant        '
            ENDIF 
            WRITE(MSGSET,'(1X,2A,I2,2A)')
     &          SNAME(icomp,i),' ----- ',J,' ---------- ',DPNDC
            CALL EDISP(IUOUT,MSGSET)
   30     CONTINUE
   20   CONTINUE

C Help.
      ELSEIF(INO.EQ.(NITEM-1))THEN
        IHELPM=1
        CALL PHELPD('Thermal Conductivity',nbhelp,'-',0,0,IER)
      ELSEIF(INO.EQ.NITEM)THEN

C Exit.  Check the legality of the thermal configuration statements.
        KERR=0
        CALL KCHECK(ICOMP,KERR)
        IF(KERR.NE.0)THEN
          CALL EASKOK('Warning messages detected!',
     &                'Exit application?',OK,nbhelp)
          IF(.NOT.OK)GOTO 10
        ENDIF
        RETURN
      ENDIF

C After each command selection and execution (except the EXIT)
C redisplay the linear thermal conductivity menu . 
      GOTO 10
      END
                 
C ****************************   DFNMNU   ****************************
C This routine displayes a menu for defining the range of layers
C to be defined with constant, or temperature and/or moisture content
C dependent thermal conductivity.
C *******************************************************************
      SUBROUTINE DFNMNU(TEXT,INO)
#include "help.h"

      COMMON/HELP/IHELPM
      COMMON/OUTIN/IUOUT,IUIN,IEOUT

      CHARACTER*25 ITEM(6)
      CHARACTER TEXT*50
      integer NITEM,INO ! max items and current menu item

      helpinsub='blcond'  ! set for subroutine

   10 ITEM(1)='a select zone            '
      ITEM(2)='b select constructions   '
      ITEM(3)='l select layers          '
      ITEM(4)='  _______________________'
      ITEM(5)='? help                   '
      ITEM(6)='- exit menu              '
      NITEM=6

C Help text for this menu
      call edisp(iuout,'The focus for which thermal conductivity')
      call edisp(iuout,TEXT)
      helptopic='focus_of_thermal_cond'
      call gethelptext(helpinsub,helptopic,nbhelp)

      CALL EMENU('Layer Focus',ITEM,NITEM,INO)
      IF(INO.GE.1.OR.INO.LE.4)THEN
        RETURN
      ELSEIF(INO.EQ.(NITEM-1))THEN
        IHELPM=1
        CALL PHELPD('conductivity focus',nbhelp,'-',0,0,IER)
      ELSEIF(INO.EQ.NITEM)THEN
        INO=0
        RETURN
      ENDIF
      GOTO 10
      END

C ***************************  LKZONE  ****************************
C This routine is called when the user wants to define the
C thermal conductivity by zone.
C ****************************************************************
      SUBROUTINE LKZONE(ICOMP,INDX)
#include "building.h"
#include "geometry.h"

      COMMON/OUTIN/IUOUT,IUIN,IEOUT
      COMMON/T1/NE(MS),NAIRG(MS),IPAIRG(MS,MGP),RAIRG(MS,MGP)
      COMMON/VTHP02/IVKON(MCOM,MS,ME) 
      CHARACTER OUTS*124

      ICNT=0
      ICNTM=3
      DO 10 I=1,NZSUR(icomp)
        NGP=NAIRG(I)
        M=1
        DO 20 J=1,NE(I)
          IF(NGP.GT.0.AND.M.LE.NGP)THEN
            IG=IPAIRG(I,M)
          ELSE
            IG=-1
          ENDIF
          IF(J.EQ.IG)THEN
            M=M+1
          ELSEIF(INDX.EQ.-1)THEN
            IVKON(ICOMP,I,J)=0
          ELSE
            KCHK=IVKON(ICOMP,I,J)
            IF(KCHK.EQ.0.OR.KCHK.EQ.INDX)THEN
              IVKON(ICOMP,I,J)=INDX

C If the layer was NOT defined to be constant or of the the type 
C which is required now then warn the user , and call KCHKMN 
C via which the user will decide which type he wants
            ELSE
              CALL WRTLYR(ICOMP,I,J,OUTS)
              CALL EDISP(IUOUT,OUTS)
              CALL KCHKMN(KCHK,INDX,INDX2)
              IVKON(ICOMP,I,J)=INDX2
              ICNT=ICNT+1

C Check to be sure that the user did not selected this 
C option by mistake.
              IF(ICNT.GE.ICNTM)THEN
                CALL TRMMSG(ICNTM)
                IF(ICNTM.LE.0)RETURN
                ICNT=0
              ENDIF 
            ENDIF
          ENDIF 
   20   CONTINUE
   10 CONTINUE
      RETURN
      END

C *****************************  SURFMN  **************************
C Routine is called by KCONST and KLAYER. It displays a menu
C via which the user will decide in which surface he want to define 
C the thermal conductivity .
C ******************************************************************
      SUBROUTINE SURFMN(ICOMP,TEXT,INPICK,IVALS)
#include "building.h"
#include "geometry.h"
#include "help.h"

      COMMON/OUTIN/IUOUT,IUIN,IEOUT
      integer IZSTOCN
      COMMON/C24/IZSTOCN(MCOM,MS)
      DIMENSION IVALS(MS)

      CHARACTER*20 ITEM(MS)
      CHARACTER TEXT*50

      helpinsub='blcond'  ! set for subroutine

      DO 10 I=1,NZSUR(icomp)
        ioc=IZSTOCN(icomp,i)
        WRITE(ITEM(I),'(A)')SNAME(icomp,i)
   10 CONTINUE
      call edisp(iuout,'Select surfaces for which the conductivity is')
      call edisp(iuout,TEXT)
      helptopic='focus_of_surf_cond'
      call gethelptext(helpinsub,helptopic,nbhelp)
      CALL EPICKS(INPICK,IVALS,' ',' ',20,NZSUR(icomp),ITEM,
     &  'Surfaces',IER,nbhelp)
      RETURN
      END

C ***************************  KCONST  ****************************
C Routine will be called when the user wants to define the
C thermal conductivity by surface.
C ****************************************************************
      SUBROUTINE KCONST(ICOMP,INDX,TEXT)
#include "building.h"
#include "geometry.h"

      COMMON/OUTIN/IUOUT,IUIN,IEOUT
      COMMON/T1/NE(MS),NAIRG(MS),IPAIRG(MS,MGP),RAIRG(MS,MGP)
      COMMON/VTHP02/IVKON(MCOM,MS,ME) 
      DIMENSION IVALS(MS)
      CHARACTER OUTS*124,TEXT*50

      ICNT=0
      ICNTM=3
      INPICK2=NZSUR(icomp)
      CALL SURFMN(ICOMP,TEXT,INPICK2,IVALS)
      IF(INPICK2.EQ.0)RETURN
      DO 10 IN=1,INPICK2
        I=IVALS(IN)
        NGP=NAIRG(I)
        M=1
        DO 20 J=1,NE(I)
          IF(NGP.GT.0.AND.M.LE.NGP)THEN
            IG=IPAIRG(I,M)
          ELSE
            IG=-1
          ENDIF
          IF(J.EQ.IG)THEN
            M=M+1
          ELSEIF(INDX.EQ.-1)THEN
            IVKON(ICOMP,I,J)=0
          ELSE
            KCHK=IVKON(ICOMP,I,J)
            IF(KCHK.EQ.0.OR.KCHK.EQ.INDX)THEN
              IVKON(ICOMP,I,J)=INDX
            ELSE
              CALL WRTLYR(ICOMP,I,J,OUTS)
              CALL EDISP(IUOUT,OUTS)
              CALL KCHKMN(KCHK,INDX,INDX2)
              IVKON(ICOMP,I,J)=INDX2
              ICNT=ICNT+1
              IF(ICNT.GE.ICNTM)THEN
                CALL TRMMSG(ICNTM)
                IF(ICNTM.LE.0)RETURN
                ICNT=0
              ENDIF 
            ENDIF
          ENDIF  
   20   CONTINUE
   10 CONTINUE
      RETURN
      END

C ***************************  KLAYER  ****************************
C Routine will be called when the user wants to define the
C thermal conductivity by layer.
C ****************************************************************
      SUBROUTINE KLAYER(ICOMP,INDX,TEXT)
#include "building.h"

      COMMON/OUTIN/IUOUT,IUIN,IEOUT
      COMMON/T1/NE(MS),NAIRG(MS),IPAIRG(MS,MGP),RAIRG(MS,MGP)
      COMMON/VTHP02/IVKON(MCOM,MS,ME) 
      DIMENSION IVALS(MS),IVALL(ME)
      CHARACTER OUTS*124,TEXT*50

   10 INPICK2=1
      CALL SURFMN(ICOMP,TEXT,INPICK2,IVALS)
      IF(INPICK2.EQ.0)RETURN
      I=IVALS(1)
      INPICK3=NE(I)
      CALL LAYRMN(I,TEXT,INPICK3,IVALL)
      IF(INPICK3.EQ.0)GOTO 10
      DO 20 IN=1,INPICK3
        J=IVALL(IN)
        DO 30 IAIR=1,NAIRG(I)
          IGP=IPAIRG(I,IAIR)
          IF(IGP.EQ.J)GOTO 20
   30   CONTINUE
        KCHK=IVKON(ICOMP,I,J)
        IF(KCHK.EQ.0.OR.KCHK.EQ.INDX)THEN
          IVKON(ICOMP,I,J)=INDX
        ELSE
          CALL WRTLYR(ICOMP,I,J,OUTS)
          CALL EDISP(IUOUT,OUTS)
          CALL KCHKMN(KCHK,INDX,INDX2)
          IVKON(ICOMP,I,J)=INDX2
        ENDIF   
   20 CONTINUE
      GOTO 10
      END

C ****************************  LAYRMN  ******************************
C Routine is called by  KLAYER . It displays a menu via 
C which the user decides which layers to define the 
C thermal conductivity.
C *******************************************************************
      SUBROUTINE LAYRMN(I,TEXT,INPICK,IVALL)
#include "building.h"
#include "help.h"

      COMMON/OUTIN/IUOUT,IUIN,IEOUT
      COMMON/T1/NE(MS),NAIRG(MS),IPAIRG(MS,MGP),RAIRG(MS,MGP)
      DIMENSION IVALL(ME)
      CHARACTER*20 ITEM(ME)
      CHARACTER TEXT*50

      helpinsub='blcond'  ! set for subroutine

      NGP=NAIRG(I)
      M=1
      DO 10 J=1,NE(I)
        IF(NGP.GT.0.AND.M.LE.NGP)THEN
          IG=IPAIRG(I,M)
        ELSE
          IG=-1
        ENDIF
        IF(J.EQ.IG)THEN
          M=M+1
          WRITE(ITEM(J),'(A)')' ** Air Gap ** '
        ELSE
          WRITE(ITEM(J),'(A,1X,I2,1X,A)')'Layer (',J,')'
        ENDIF
   10 CONTINUE
      call edisp(iuout,'Select layers for which the conductivity is')
      call edisp(iuout,TEXT)
      helptopic='focus_of_layer_cond'
      call gethelptext(helpinsub,helptopic,nbhelp)
      CALL EPICKS(INPICK,IVALL,' ',' ',20,NE(I), ITEM,'Layers',
     &   IER,nbhelp)
      RETURN
      END

C *****************************  WRTLYR  ******************************
C WRTLYR fills up the layer name by specifying the zone and surface
C names beside the layer number.
C *********************************************************************
      SUBROUTINE WRTLYR(ICOMP,ICNST,ILYR,NAMELYR)
#include "building.h"
#include "geometry.h"
      
      integer lnblnk  ! function definition

      integer IZSTOCN
      COMMON/C24/IZSTOCN(MCOM,MS)

      CHARACTER NAMELYR*124
      integer lnsn

      ioc=IZSTOCN(icomp,icnst)
      lnsn=lnblnk(sname(icomp,icnst))

      WRITE(NAMELYR,'(5A,I2,A)') ' WARNING The layer ( ',
     &  zname(ICOMP)(1:lnzname(ICOMP)),', ',
     &  SNAME(icomp,icnst)(1:lnsn),', ',ILYR,') is already defined.'
      RETURN
      END

C *****************************  TRMMSG  ******************************
C TRMMSG controls the appearance of the termination message.
C *********************************************************************
      SUBROUTINE TRMMSG(ICNTM)
#include "help.h"

      COMMON/OUTIN/IUOUT,IUIN,IEOUT
      LOGICAL OK
      integer icntmt  ! local variable for editing
      character msg*72

      helpinsub='blcond'     ! set for subroutine

      helptopic='surf_cond_warning'
      call gethelptext(helpinsub,helptopic,nbhelp)
      CALL EASKOK('Possible problem!',
     &            'Terminate last command?',OK,nbhelp)
      IF(OK)THEN
        ICNTM=-1
      ELSE
        WRITE(msg,'(A,I2)')' The current number is',ICNTM
        call edisp(iuout,msg)
        icntmt=ICNTM
        CALL EASKI(ICNTMT,'After how many re_defining warning messages',
     &    'do you want the termination  message to appear?',
     &    1,'F',20,'W',3,'termination message?',IERI,nbhelp)
        if(ieri.eq.-3)then
          return
        else
          ICNTM=icntmt
        endif
      ENDIF
      RETURN
      END

C *****************************  KCHKMN  ****************************
C KCHKMN is called in case of redefining the thermal 
C conductivity of any layer. It will inform the user about the layer
C location, and lets him to decide which type he wants to select,
C the old type or the new type.
C *******************************************************************
      SUBROUTINE KCHKMN(KCHK,INDX,INDX2)
#include "help.h"

      CHARACTER*80 MSG1,MSG2
      LOGICAL OK

      helpinsub='blcond'  ! set for subroutine

      IF(KCHK.EQ.1)THEN
        MSG2='temperature dependent ... change?'
      ELSEIF(KCHK.EQ.2)THEN
        MSG2='moisture dependent ... change?'
      ELSE
        WRITE(MSG2,'(2A)')'temperature & moisture', 
     &         ' dependent ... change?'
      ENDIF
      MSG1='Layer conductivity defined as'
      helptopic='message_for_surf_cond'
      call gethelptext(helpinsub,helptopic,nbhelp)
      CALL EASKOK(MSG1,MSG2,OK,nbhelp)
      IF(OK)THEN
        INDX2=INDX
      ELSE
        INDX2=KCHK
      ENDIF
      RETURN
      END

C ******************************  KCHECK  *****************************
C Checks the legality of the thermal configuration statements defined 
C by the linear thermal conductivity menu .
C *********************************************************************
      SUBROUTINE KCHECK(ICOMP,KERR)
#include "building.h"
#include "geometry.h"

      COMMON/OUTIN/IUOUT,IUIN,IEOUT
      
      integer ncomp,ncon
      COMMON/C1/NCOMP,NCON
      COMMON/C3/IC1(MCON),IE1(MCON),ICT(MCON),IC2(MCON),IE2(MCON)
      COMMON/T1/NE(MS),NAIRG(MS),IPAIRG(MS,MGP),RAIRG(MS,MGP)
      COMMON/VTHP02/IVKON(MCOM,MS,ME)
      COMMON/VTHP14/THRMLI(MCOM,MS,ME,7)

      CHARACTER TEXT*115

C Check if a partition is defined similarly from both sides
      DO 10 I=1,NCON
        IF(ICT(I).EQ.3.AND.IC1(I).EQ.ICOMP)THEN
          DO 20 IL1=1,NE(IE1(I))
            IL2=NE(IE1(I))+1-IL1
            IV1=IVKON(IC1(I),IE1(I),IL1)
            IV2=IVKON(IC2(I),IE2(I),IL2)
            IF(IV1.NE.IV2)THEN
              WRITE(TEXT,'(3(A,I2),2A,I2,A)')' WARNING : layer (',
     &        IC1(I),',',IE1(I),',',IL1,') is not defined by similar ',
     &        'cond. type from zone (',IC2(I),')'
              CALL EDISP(IUOUT,TEXT)
              KERR=1
            ENDIF
   20     CONTINUE
        ENDIF
   10 CONTINUE

C Check if the thermal conductivity of a layer is defined to be 
C variable but no dependence factor is available in the construction 
C file(s) 
      DO 40 I=1,NZSUR(icomp)
        DO 50 J=1,NE(I)
          IF((IVKON(ICOMP,I,J).EQ.1.OR.IVKON(ICOMP,I,J).EQ.3).AND.
     &      ABS(THRMLI(ICOMP,I,J,6)).LT.1.E-12)THEN
            WRITE(TEXT,'(3(A,I3),A)')
     &        ' WARNING : thermal conductivity of layer (',
     &          ICOMP,',',I,',',J,') is temperature dependent'
            CALL EDISP(IUOUT,TEXT)
            WRITE(TEXT,'(2A)')'           BUT the dependence factor',
     &                  ' is not available in the construction file.'
            CALL EDISP(IUOUT,TEXT)
            KERR=1
          ENDIF
          IF(IVKON(ICOMP,I,J).EQ.2.OR.IVKON(ICOMP,I,J).EQ.3)THEN
            IF(ABS(THRMLI(ICOMP,I,J,7)).LT.1.E-12)THEN
              WRITE(TEXT,'(3(A,I3),A)')
     &        ' WARNING : thermal conductivity of layer (',
     &        ICOMP,',',I,',',J,') is moisture content dependent'
              CALL EDISP(IUOUT,TEXT)
              WRITE(TEXT,'(2A)')'           BUT the dependence ',
     &        'factor is not available in the construction file.'
              CALL EDISP(IUOUT,TEXT)
              KERR=1
            ENDIF
          ENDIF
   50   CONTINUE
   40 CONTINUE
      RETURN
      END
