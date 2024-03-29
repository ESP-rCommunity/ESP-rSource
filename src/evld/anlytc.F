C ESP-r is free software.  You can redistribute it and/or
C modify it under the terms of the GNU General Public
C License as published by the Free Software Foundation 
C (version 2 orlater).

C ESP-r is distributed in the hope that it will be useful
C but WITHOUT ANY WARRANTY; without even the implied
C warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
C PURPOSE. See the GNU General Public License for more
C details.


C *********************************************************************
C This file contains the following subroutines and functions:
C   AnlCnd
C   VldAdv
C   VldBnd
C   VldCon
C   VConEd
C   VEigen
C   NPEign
C   VldP
C   VldQ
C   Omg
C   AnlCnv
C   AnlRad
C   AnlMfl
C   VCpyDr
C   VMP111
C   VSaveP
C   VIn111
C   VRes11
C   VPSimu
C   VAn111


C ********** AnlCnd **********

      SUBROUTINE AnlCnd
#include "building.h"
#include "help.h"

      COMMON/OUTIN/IUOUT,IUIN,IEOUT
      COMMON/FILEP/IFIL
      COMMON/Vld01/NLayer,Cond(ME),Dens(ME),Cp(ME),DX(ME),NPLyr(ME)
      COMMON/Vld02/Tinit,IndBO,IndBI,Tout,Tin,Hout,Hin
      COMMON/Vld04/DTime,NTime,MonNod,NRoot,NIter
      COMMON/Vld10/VlDirR,VlDirL
      COMMON/OVRWT/AUTOVR
      logical AUTOVR
      common/deflt4/dinstpath

      character item(12)*28,STRS*124,STRA*124,MsgB*25,MsgC*8
      CHARACTER*72 Msg,MsgT,LstDtl,FileS,FileA,FileF
      character VlDirR*96,VlDirL*72
      CHARACTER Promp*72,dinstpath*60
      character dummy*12

      logical VldOK

      integer MITEM,INO ! max items and current menu item

C << TODO shift h()= statements into a *.help file. >>

      helpinsub='anlytc'  ! set for subroutine
      helptopic='AnlCnd'
      call gethelptext(helpinsub,helptopic,nbhelp)

      MonNod=1
      VldOK=.false.
      write(VlDirR,'(2a)') dinstpath(1:lnblnk(dinstpath)),
     &  '/validation/analytical/conduction01'
C      VlDirR='/usr/esru/esp-r/validation/analytical/conduction01'
   10 INO=-3
      ITEM(1)='1 construction       '
      ITEM(2)='2 initial temperature'
      ITEM(3)='3 external boundary  '
      ITEM(4)='4 internal boundary  '
      ITEM(5)='  ___________________'
      ITEM(6)='a advanced options   '
      ITEM(7)='l list details       '
      ITEM(8)='p perform calculation'
      ITEM(9)='c compare results    '
      ITEM(10)='  __________________ '
      ITEM(11)='? help               '
      ITEM(12)='- exit menu          '
      MITEM=12

      CALL EMENU('Conduction test',ITEM,MITEM,INO)

C Multi-layered construction.
      IF(INO.EQ.1)THEN
        if(MonNod.ne.1)then
          MonNod=1
        endif
        CALL VldCon

C Initial Temperature.
      ELSEIF(INO.EQ.2)THEN
        WRITE(Msg,'(A)')'Initial temperature?'
        H(1)='Enter the initial temperature for construction nodes.'
        CALL EASKR(Tinit,' ',Msg,0.,'-',1.,'-',0.,'temperature',IER,1)

C External boundary.
      ELSEIF(INO.EQ.3)THEN
        if(NLayer.lt.1)then
          call edisp(iuout,' ')
          call edisp(iuout,'Define construction first!')
        else
          MsgT='External boundary options:'
          MsgC='cancel'
          CALL EASKABC(' ',MsgT,'convective','adiabatic',MsgC,IW,0)

C Convective boundary.
          IF(IW.EQ.1)THEN
            IndBO=1
            CALL VldBnd(Tout,Hout)
            Tout=float(int(Tout))

C Adiabatic boundary.
          ELSEIF(IW.EQ.2)THEN
            IndBO=2
            Hout=0.0
          ENDIF
        endif

C Internal boundary.
      ELSEIF(INO.EQ.4)THEN
        if(NLayer.lt.1)then
          call edisp(iuout,' ')
          call edisp(iuout,'Define construction first!')
        else
          MsgT='Internal boundary options:'
          MsgB='constant surface temp.'
          MsgC='cancel'
          CALL EASKABC(' ',MsgT,'convective',MsgB,MsgC,IW,0)

C Convective boundary.
          IF(IW.EQ.1)THEN
            IndBI=1
            if(Hin.gt.99.9)Hin=6.0
            CALL VldBnd(Tin,Hin)

C Known surface temperature.
          ELSEIF(IW.EQ.2)THEN
            IndBI=3
            Hin=1.0e32
            WRITE(Promp,'(A)')'Surface temperature?'
            H(1)='The internal surface temperature will be held'
            H(2)='at this value.'
            CALL EASKR(Tin,' ',Promp,0.,'-',1.,'-',0.,' temp. ',IER,2)
          ENDIF
        endif

C Advanced options.
      ELSEIF(INO.EQ.MITEM-6)THEN
        CALL VldAdv

C List model details.
      ELSEIF(INO.EQ.MITEM-5)THEN
        CALL EPAGE
        WRITE(LstDtl,'(A,F8.2)')
     &       'Initial temperature (deg.C):', Tinit
        CALL EDISP(IUOUT,LstDtl)
        IF(IndBO.EQ.0)THEN
          WRITE(LstDtl,'(A)')'External boundary not defined.'
        ELSEIF(IndBO.EQ.1)THEN
          WRITE(LstDtl,'(a,a,f7.2,a,f7.2,a)')'Convective external ',
     &    'boundary (To= ',Tout,', hc= ',Hout,' ).'
        ELSEIF(IndBO.EQ.2)THEN
          WRITE(LstDtl,'(A)')'Adiabatic external boundary.'
        ELSEIF(IndBO.EQ.3)THEN
          WRITE(LstDtl,'(a,a,f7.2,a)')'Known external ',
     &    'surface temperature (To= ',Tout,' ).'
        ENDIF
        CALL EDISP(IUOUT,LstDtl)
        CALL EDISP(IUOUT,' ')
        WRITE(LstDtl,'(2a)')'   Conductivity   Density        Cp     ',
     &                     'Thickness    Nodes'
        CALL EDISP(IUOUT,LstDtl)
        WRITE(LstDtl,'(2a)')'========================================',
     &                      '===================='
        CALL EDISP(IUOUT,LstDtl)
        CALL EDISP(IUOUT,' ')
 601    FORMAT(I2,2X,F8.4,4X,F8.1,4X,F8.1,4X,F8.4,5X,I4)
        DO 20 J=1,NLayer
          WRITE(LstDtl,601)J,Cond(J),Dens(J),Cp(J),DX(J),NPLyr(J)
          CALL EDISP(IUOUT,LstDtl)
   20   CONTINUE
        WRITE(LstDtl,'(2a)')'----------------------------------------',
     &                      '--------------------'
        CALL EDISP(IUOUT,LstDtl)
        CALL EDISP(IUOUT,' ')
        IF(IndBI.EQ.0)THEN
          WRITE(LstDtl,'(A)')'Internal boundary not defined.'
        ELSEIF(IndBI.EQ.1)THEN
          WRITE(LstDtl,'(a,a,f7.2,a,f7.2,a)')'Convective internal ',
     &    'boundary (Ti= ',Tin,', hc= ',Hin,' ).'
        ELSEIF(IndBI.EQ.2)THEN
          WRITE(LstDtl,'(A)')'Adiabatic internal boundary.'
        ELSEIF(IndBI.EQ.3)THEN
          WRITE(LstDtl,'(a,a,f7.2,a)')'Known internal ',
     &    'surface temperature (Ti= ',Tin,' ).'
        ENDIF
        CALL EDISP(IUOUT,LstDtl)

C Perform calculation.
      ELSEIF(INO.EQ.MITEM-4)THEN
        if(NLayer.lt.1)then
          call edisp(iuout,' ')
          call edisp(iuout,'Define construction first!')
        elseif(IndBO.eq.0)then
          call edisp(iuout,' ')
          call edisp(iuout,'Define external boundary first!')
        elseif(IndBI.eq.0)then
          call edisp(iuout,' ')
          call edisp(iuout,'Define internal boundary first!')
        else
          AUTOVR=.true.

C Copy the default validation model.
          call VCpyDr(NPER,ier)

C Modify the validation model.
          if(ier.eq.0)call VMP111

C Save the modified validation model.
          if(ier.eq.0)call VSaveP(NPER,ier)

C Perform simulation.
          if(ier.eq.0)call VIn111(ier)
          if(ier.eq.0)call VRes11(ier)
          if(ier.eq.0)call VPSimu

C Perform analytical solution.
          if(ier.eq.0)call VAn111(ier)
          if(ier.eq.0)then
            VldOK=.true.
          else
            VldOK=.false.
          endif
          AUTOVR=.false.
        endif

C Compare results.
      ELSEIF(INO.EQ.MITEM-3)THEN
        if(VldOK)then
          H(1)='Select the destination for the comparison.'
          CALL EASKABC(' ','Output to:',
     &                'screen','file',' ',IW,1)
          IOutS=IFIL+20
          FileS='Simulation.out'
          CALL EFOPSEQ(IOutS,FileS,1,IER)
          IOutA=IFIL+21
          FileA='Analytical.out'
          CALL EFOPSEQ(IOutA,FileA,1,IER)
          if(IW.eq.1)then
            IOutC=IUOut
          elseif(IW.eq.2.or.IW.eq.3)then
            IOutF=IFIL+19
            call easks(FileF,' ','File name?',72,'Comparison.out',
     &         'name',IER,0)
            CALL EFOPSEQ(IOutF,FileF,4,IER)
            IOutC=IOutF
          endif
          WRITE(LstDtl,'(a)')'Time (hr)  Simulation  Analytical'
          call edisp(iuout,' ')
          call edisp(iuout,'Analytical test results')
          CALL EDISP(IOutC,LstDtl)
          CALL STRIPC(IOutS,STRS,2,ND,0,' ',IER)
          DO 30 ITime=0,NTime
            VTime=ITime*DTime/3600.0
            CALL STRIPC(IOutS,STRS,2,ND,0,' ',IER)
            K=0
            CALL EGETW(STRS,K,Dummy,'-','time',IER)
            CALL EGETWR(STRS,K,TempS,0.,0.,'-','temp',IER)
            CALL STRIPC(IOutA,STRA,2,ND,0,' ',IER)
            K=0
            CALL EGETW(STRA,K,Dummy,'-','time',IER)
            CALL EGETWR(STRA,K,TempA,0.,0.,'-','temp',IER)
            WRITE(LstDtl,'(2X,F8.2,5X,F7.2,3X,F7.2)')VTime,TempS,TempA
            CALL EDISP(IOutC,LstDtl)
   30     continue
          if(IW.eq.3)then
            CALL ERPFREE(IOutF,ISTAT)
          endif
          CALL ERPFREE(IOutS,ISTAT)
          CALL ERPFREE(IOutA,ISTAT)
        else
          call edisp(iuout,' ')
          call edisp(iuout,'Perform calculation first!')
        endif

C Help.
      ELSEIF(INO.EQ.MITEM-1)THEN
        H(1)='This menu supports the setting of the parameters'
        H(2)='of the analytical conduction test.'
        CALL PHELPD('vld opening',2,'-',0,0,IER)

C Return to the calling menu.
      ELSEIF(INO.EQ.MITEM)THEN
        RETURN
      ENDIF
      GOTO 10
      END


C ********** VldAdv **********

      SUBROUTINE VldAdv
#include "building.h"
#include "help.h"

      COMMON/GR1D04/GAM,RGAM
      COMMON/Vld01/NLayer,Cond(ME),Dens(ME),Cp(ME),DX(ME),NPLyr(ME)
      COMMON/Vld04/DTime,NTime,MonNod,NRoot,NIter

      character*32 item(11)
      CHARACTER*72 Msg
      integer NTimeT,NRootT,NIterT  ! local values for editing.
      integer MITEM,INO ! max items and current menu item

      helpinsub='anlytc'  ! set for subroutine
      helptopic='VldAdv'
      call gethelptext(helpinsub,helptopic,nbhelp)
      
   10 INO=-3
      WRITE(ITEM(1),'(A13,F8.1,A2)')'1 Time step: ',DTime,' s'
      WRITE(ITEM(2),'(A,I5)')'2 Number of steps: ',NTime
      if(MonNod.eq.1)then
        WRITE(ITEM(3),'(A)')'3 Monitored node: Ext. surface'
      elseif(MonNod.eq.-1)then
        WRITE(ITEM(3),'(A)')'3 Monitored node: Int. surface'
      else
        WRITE(ITEM(3),'(A,I3)')'3 Monitored node: ',MonNod
      endif
      ITEM(4)='  _____________________________'
      WRITE(ITEM(5),'(A,F5.2)')'a Implicitness degree: ',Gam
      ITEM(6)='  _____________________________'
      WRITE(ITEM(7),'(A,I6)')'4 Number of eigenvalues: ',NRoot
      WRITE(ITEM(8),'(A,I6)')'5 Maximum iterrations: ',NIter
      ITEM(9)='  _____________________________'
      ITEM(10)='? help '
      ITEM(11)='- exit menu'
      MITEM=11

      CALL EMENU('Advanced options',ITEM,MITEM,INO)

C Simulation time step.
      IF(INO.EQ.1)THEN
        WRITE(Msg,'(A)')'Simulation time step?'
        H(1)='Enter the simulation time step in seconds up to'
        H(2)='and including 3600.'
        CALL EASKR(DTime,' ',Msg,1.,'F',2.,'-',3600.,'time step',IER,2)
        DTime=3600.0/INT(3600.0/DTime+0.5)

C Number of simulation time steps.
      ELSEIF(INO.EQ.2)THEN
        WRITE(Msg,'(A)')'Number of simulation time steps:'
        H(1)='The simulation period will equal the number entered'
        H(2)='here multiplied by the given time step.'
        NTimeT=NTime
        CALL EASKI(NTimeT,' ',Msg,1,'F',3600,'W',60,'time steps',
     &    IERI,2)
        if(ieri.eq.-3)then
          goto 10
        else
          NTime=NTimeT
        endif

C Monitoring location.
      ELSEIF(INO.EQ.3)THEN
        WRITE(Msg,'(A)')'Monitoring location options:'
        H(1)='This defines an arbitrary location where the results will'
        H(2)='be monitored as the simulation proceeds.'
        CALL EASKABC(' ',Msg,'external surface','internal surface',
     &              'intra-construction',IW,2)
        IF(IW.EQ.1)THEN
          MonNod=1
        ELSEIF(IW.EQ.2)THEN
          MonNod=-1
        ELSE
          MaxNd=1
          do 100 il=1,NLayer
            MaxNd=MaxNd+NPLyr(il)
  100     continue
          WRITE(Msg,'(A)')'Construction node number?'
          H(1)='This is the construction node number commencing from'
          H(2)='node 1 located at the outside surface.' 
          MonNodT=MonNod
          CALL EASKI(MonNodT,Msg,' ',1,'F',MaxNd,'F',2,'node',IERI,2)
          if(ieri.eq.-3)then
            goto 10
          else
            MonNod=MonNodT
          endif
        ENDIF

C Zone side equations implicitness degree.
      ELSEIF(INO.EQ.5)THEN
        H(1)='Enter 0 for fully explicit, 1 for fully implicit'
        H(2)='and 0.5 for Crank-Nicolson'
        CALL EASKR(Gam,' ','Equations implicitness degree?',
     &         0.,'F',1.,'F',0.5,'implicitness degree',IER,2)

C Number of eigenvalues
      ELSEIF(INO.EQ.7)THEN
        WRITE(Msg,'(A)')'Number of eigenvalues:'
        H(1)='In general, the higher the number of eigenvalue'
        H(2)='the more accurate will be the analytical result.'
        NRootT=NRoot
        CALL EASKI(NRoot,' ',Msg,1,'F',1000,'W',100,'number',IERI,3)
        if(ieri.eq.-3)then
          goto 10
        else
          NRoot=NRootT
        endif

C Maximum iterrations
      ELSEIF(INO.EQ.8)THEN
        WRITE(Msg,'(A)')'Maximum number of iterrations?'
        H(1)='The sign-count method is used to determine the required'
        H(2)='eivenvalues. While this will always converge to the'
        H(3)='require value, it sometimes requires more iterations'
        H(4)='than the default value of 500. In this case raise the'
        H(5)='value. Note that small magnitude eigenvalues, which'
        H(6)='are the most effective in terms of result accuracy,'
        H(7)='will converge within the first 100 iterrations.'

        NIterT=NIter
        CALL EASKI(NIterT,' ',Msg,100,'W',3000,'W',500,'number',
     &    IERI,7)
        if(ieri.eq.-3)then
          goto 10
        else
          NIter=NIterT
        endif
      
C Menu help.
      ELSEIF(INO.EQ.MITEM-1)THEN
        H(1)='This is the advanced options menu. Because the'
        H(2)='default values of the displayed parameters are'
        H(3)='usually acceptable, you should not alter them'
        H(4)='unless you fully understand the implications.'

        CALL PHELPD('vld opening',4,'-',0,0,IER)

C Exit advanced options menu.
      ELSEIF(INO.EQ.MITEM)THEN
        RETURN
      ENDIF
      GOTO 10
      END


C ********** VldBnd **********

      SUBROUTINE VldBnd(TempB,HTranB)
#include "help.h"
      integer iCountWords

      character StrVal*25
      CHARACTER*72 Msg

      helpinsub='anlytc'  ! set for subroutine
      helptopic='VldBnd'
      call gethelptext(helpinsub,helptopic,nbhelp)
      
      WRITE(StrVal,'(2F9.1)')TempB,HTranB
      H(1)=' You should enter the following data:'
      H(2)=' 1. ambient temperature.'
      H(3)=' 2. conective heat transfer coefficient (hc).'
  10  CALL EASKS(StrVal,' ','Ambient temperature and hc value?',
     &       20,' ','boundary details',IER,3)
      CALL USRMSG('  ','  ','-')
      NInp = iCountWords(StrVal)
      NExpct=2
      IF(NInp.NE.NExpct)THEN
        WRITE(Msg,'(A,I1,A)')'You should enter ',NExpct,' values.'
        CALL USRMSG('  ',Msg,'W')
        GOTO 10
      ENDIF
      K=0
      CALL EGETWR(StrVal,K,TempB,1.,2.,'-',' Temperature ',IER)
      CALL EGETWR(StrVal,K,HTranB,0.,1.0e32,'W',' (hc) ',IER)
      RETURN
      END

C ********** VldCon **********

      SUBROUTINE VldCon
#include "epara.h"
#include "building.h"
#include "help.h"

      COMMON/Vld01/NLayer,Cond(ME),Dens(ME),Cp(ME),DX(ME),NPLyr(ME)

      CHARACTER KEY*1
      character*47 ITEM(20)
      integer NITEMS,INO ! max items and current menu item

      helpinsub='anlytc'  ! set for subroutine
      helptopic='VldCon'
      call gethelptext(helpinsub,helptopic,nbhelp)

C Initialise connection menu size variables based on window size. 
C INO is the menu position, NITEMS the current number of menu lines.
      MHEAD=2
      MCTL=6
      ITEM(1)='  No | Cond  | Density |  Cp   |  DX  | Node'
      ITEM(2)='  ____________________________________________'

      IPACT=CREATE
      CALL EKPAGE(IPACT)

C Initial menu entry setup.
   10 INO=-3

C NLayer number of layers in the building construction.
      ILEN=NLayer

C Loop through the items until the page to be displayed. M is the 
C current menu line index. Build up text strings for the menu. 
      M=MHEAD
  601 FORMAT(A1,1X,I2,1X,F7.3,2X,F7.1,2X,F7.1,2X,F6.3,2X,I3)
      DO 20 L=1,ILEN
        IF(L.GE.IST.AND.(L.LE.(IST+MIFULL)))THEN
          M=M+1
          CALL EMKEY(L,KEY,IER)
          ITEM(M)=' '
          WRITE(ITEM(M),601)KEY,L,Cond(L),Dens(L),Cp(L),DX(L),NPLyr(L)
        ENDIF
   20 CONTINUE

C Number of actual items displayed.
      NITEMS=M+MCTL

C If a long list include page facility text.      
      IF(IPFLG.EQ.0)THEN
        ITEM(M+1)='  ____________________________________________'
      ELSE
        WRITE(ITEM(M+1),'(A,I2,A,I2)')'0 Page ------- Part: ',IPM,
     &                                  ' of ',MPM
      ENDIF
      ITEM(M+2)  ='1 add a layer'
      ITEM(M+3)  ='2 delete a layer'
      ITEM(M+4)  ='  ____________________________________________'
      ITEM(M+5)  ='? Help                     '
      ITEM(M+6)  ='- Exit                     '

C Now display the menu.
      CALL USRMSG('  ','  ','-')
      CALL EMENU('Construction layers',ITEM,NITEMS,INO)

C Edit item identified by KEYIND.
      IF(INO.GT.MHEAD.AND.INO.LT.(NITEMS-MCTL+1))THEN
        CALL KEYIND(NITEMS,INO,IFOC,IO)
          CALL VConEd(IFOC,ier)

C If there are enough items allow paging control via EKPAGE.
      ELSEIF(INO.EQ.(NITEMS-5))THEN
        IF(IPFLG.EQ.1)THEN
          IPACT=EDIT
          CALL EKPAGE(IPACT)
        ENDIF

C Add a layer.
      ELSEIF(INO.EQ.(NITEMS-4))THEN
        Indx=NLayer+1
        IF(Indx.GT.ME)THEN
          CALL USRMSG('  ','Maximum number of layers reached','W')
        ELSE
          CALL VConEd(Indx,ier)
          if(ier.eq.0)NLayer=Indx
        ENDIF

C Delete surface boundary.
      ELSEIF(INO.EQ.(NITEMS-3).AND.NLayer.GT.0)THEN
        ITEM(M+2)  =' '
        ITEM(M+3)  =' '
        ITEM(M+4)  =' '
  30    INO=-3
        CALL EMENU('Surface Boundaries',ITEM,NITEMS,INO)
        IF(INO.GT.MHEAD.AND.INO.LT.(NITEMS-MCTL+1))THEN
          CALL KEYIND(NITEMS,INO,IFOC,IO)
          NLayer=NLayer-1
          DO 40 IDelete=IFOC,NLayer
            Cond(IDelete)=Cond(IDelete)
            Dens(IDelete)=Dens(IDelete+1)
            Cp(IDelete)=Cp(IDelete+1)
            DX(IDelete)=DX(IDelete+1)
            NPLyr(IDelete)=NPLyr(IDelete+1)
   40     CONTINUE
          Cond(IDelete)=0.
          Dens(IDelete)=0.
          Cp(IDelete)=0.
          DX(IDelete)=0.
          NPLyr(IDelete)=0
          GOTO 10
        ELSEIF(INO.EQ.NITEMS)THEN
          GOTO 10
        ENDIF
        GOTO 30

C Display help.
      ELSEIF(INO.EQ.(NITEMS-1))THEN
        H(1)='This menu is used to define the boundary'
        H(2)='surfaces. To define a box, the grid coordinates'
        H(3)='of two diagonally opposite corners of the box are'
        H(4)='required. The coordinates of the first corner should'
        H(5)='be smaller than those for the second corner.  The'
        H(6)='general rule is that each portion of a surface'
        H(7)='lying either inside or on the surface of the'
        H(8)='defined box will inherit the specified condition.'
        H(9)='New boundary boxes will superpose preceding ones.'

        CALL PHELPD('local components ',10,'-',0,0,IER)

C Return to multi-gridding menu.
      ELSEIF(INO.EQ.NITEMS)THEN
        RETURN
      ENDIF
      GOTO 10
      END 


C ********** VConEd **********

      SUBROUTINE VConEd(I,ier)
#include "building.h"
#include "help.h"
      integer iCountWords

      COMMON/Vld01/NLayer,Cond(ME),Dens(ME),Cp(ME),DX(ME),NPLyr(ME)

      CHARACTER*72 StrVal,Msg

      helpinsub='anlytc'  ! set for subroutine
      helptopic='VConEd'
      call gethelptext(helpinsub,helptopic,nbhelp)

      WRITE(StrVal,602)Cond(I),Dens(I),Cp(I),DX(I),NPLyr(I)
 602  FORMAT(2X,F8.4,2X,F8.1,2X,F8.1,2X,F8.4,2X,I4)
      H(1)=' Enter five values as follows.'
      H(2)=' 1. layer conductivity [W/(m.K)].'
      H(3)=' 2. layer density [kg/m^3].'
      H(4)=' 3. layer specific heat [J/(kg.k)].'
      H(5)=' 4. layer thickness [m]'
      H(6)=' 5. number of nodes representing the layer(s) (>2).'
  10  CALL EASKS(StrVal,' ',
     &       'k, rho, Cp, Dx, Nnodes?',72,' ','layer details',IER,6)
      if(ier.ne.0)return
      CALL USRMSG('  ','  ','-')
      NInp = iCountWords(StrVal)
      NExpct=5
      IF(NInp.NE.NExpct)THEN
        WRITE(Msg,'(A,I1,A)')'Enter ',NExpct,' values.'
        CALL USRMSG('  ',Msg,'W')
        GOTO 10
      ENDIF
      K=0
      CALL EGETWR(StrVal,K,Cond(I),0.,100.,'W','cond ',IER)
      if(ier.ne.0)return
      CALL EGETWR(StrVal,K,Dens(I),0.,10000.,'W','density ',IER)
      if(ier.ne.0)return
      CALL EGETWR(StrVal,K,Cp(I),0.,10000.,'W','Cp ',IER)
      if(ier.ne.0)return
      CALL EGETWR(StrVal,K,DX(I),0.,10.,'W','thickness ',IER)
      if(ier.ne.0)return
      CALL EGETWI(StrVal,K,NPLyr(I),2,MN-1,'F','nodes',IER)
      RETURN
      END


C ********** VEigen **********
C Determines the problem eigenvalues.

      SUBROUTINE VEigen(K,Beta)

      COMMON/Vld04/DTime,NTime,MonNod,NRoot,NIter

      DOUBLE PRECISION BetaU,BetaL,Beta,Accur

      LOGICAL UpFound
     
      Accur=1.0D-18
      UpFound=.FALSE.
      BetaL=0.0
      DO 100 II=1,NIter

C Define the upper bound for Beta.
         NPBeta=NPEign(Beta)
         IF(.NOT.UpFound)THEN
           IF(NPBeta.GE.K)THEN
             BetaU=Beta
             UpFound=.TRUE.
             Beta=(BetaU+BetaL)/2.0
           ELSE
             Beta=2.0*Beta
           ENDIF
         ELSE
           IF(NPBeta.LT.K)THEN
             BetaL=Beta
           ELSE
             BetaU=Beta
           ENDIF
           Beta=(BetaU+BetaL)/2.0            
           IF((BetaU-BetaL).LT.Accur)RETURN
         ENDIF
 100  CONTINUE
C      WRITE(*,*)"Eigenvalue was not determined."
      RETURN
      END


C ********** NPEign **********
C Determines the number of positive eigenvalues.

      FUNCTION NPEign(Beta)
#include "building.h"
#include "validation.h"

      COMMON/Vld01/NLayer,Cond(ME),Dens(ME),Cp(ME),DX(ME),NPLyr(ME)
      COMMON/Vld02/Tinit,IndBO,IndBI,Tout,Tin,Hout,Hin
      COMMON/Vld03/Alpha(ME),VX(MEP1)

      DOUBLE PRECISION Beta,A(MEP1),B(MEP1),D(MEP1),VldP,VldQ
      
      PI=3.141592654
      NL=NLayer

C Calculate the N0 value.
      N0=0
      DO 100 L=1,NL
        N0=N0+INT(Beta*DX(L)/(PI*SQRT(Alpha(L))))
 100  CONTINUE      

C Calculate the sign count of K.
      A(1)=Hout-VldP(1,Beta,VX(1))
      B(1)=0.0
      D(1)=A(1)
      DO 110 L=1,NL-1
            A(L+1)=VldQ(L,Beta,VX(L+1))-VldP(L+1,Beta,VX(L+1))
 110  CONTINUE
      A(NL+1)=VldQ(NL,Beta,VX(NL+1))+Hin

      DO 120 L=1,NL
        B(L+1)=VldP(L,Beta,VX(L+1))
 120  CONTINUE

      DO 200 L=2,NL+1
        D(L)=A(L)-(B(L)**2)/D(L-1)
 200  CONTINUE

      NSC=0
      DO 210 L=1,NL+1
        IF (D(L).LT.0.0)NSC=NSC+1
 210  CONTINUE
      NPEign=N0+NSC

      RETURN
      END


C ********** VldP **********
C Determines the VldP value.

      FUNCTION VldP(L,Beta,XX)
#include "building.h"
#include "validation.h"

      COMMON/Vld01/NLayer,Cond(ME),Dens(ME),Cp(ME),DX(ME),NPLyr(ME)
      COMMON/Vld03/Alpha(ME),VX(MEP1)

      DOUBLE PRECISION Beta,Omg,OmgL,VldP

      OmgL=Omg(L,Beta)
      VldP=-OmgL*Cond(L)*Dcos(OmgL*(VX(L+1)-XX))/Dsin(OmgL*DX(L))
      RETURN
      END


C ********** VldQ **********
C Determines the VldQ value.

      FUNCTION VldQ(L,Beta,xx)
#include "building.h"
#include "validation.h"

      DOUBLE PRECISION Beta,Omg,OmgL,VldQ

      COMMON/Vld01/NLayer,Cond(ME),Dens(ME),Cp(ME),DX(ME),NPLyr(ME)
      COMMON/Vld03/Alpha(ME),VX(MEP1)

      OmgL=Omg(L,Beta)
      VldQ=OmgL*Cond(L)*Dcos(OmgL*(XX-VX(L)))/Dsin(OmgL*DX(L))
      RETURN
      END

C ********** Omg **********
C Determines the Omg value.

      FUNCTION Omg(L,Beta)
#include "building.h"
#include "validation.h"

      COMMON/Vld03/Alpha(ME),VX(MEP1)

      DOUBLE PRECISION Omg,Beta

      Omg=Beta/SQRT(Alpha(L))
      RETURN
      END


C ********** VCpyDr **********

      SUBROUTINE VCpyDr(NPER,ier)
#include "building.h"
#include "model.h"
#include "help.h"

      COMMON/OUTIN/IUOUT,IUIN,IEOUT
      COMMON/FILEP/IFIL

      integer ncomp,ncon
      COMMON/C1/NCOMP,NCON
      common/rpath/path
      common/rcmd/LCMDFL
      COMMON/Vld10/VlDirR,VlDirL

      character doit*124,MODE*4
      CHARACTER*72 path
      character VlDirR*96,VlDirL*72
      character LCMDFL*144

      logical xst

      helpinsub='anlytc'  ! set for subroutine
      helptopic='VCpyDr'
      call gethelptext(helpinsub,helptopic,nbhelp)

C Check if the root validation folder is available.
      IER=0
      INQUIRE (FILE=VlDirR,EXIST=XST)
      if(.NOT.XST)then
        call usrmsg(' ','Validation folder unavailable!','W')
        ier=-1
        return
      endif

C Copy the default validation model.
      VlDirL='Analytical'
      H(1)='Folder to contain the test model and results (this'
      H(2)='will be created in your home directory).'
      CALL EASKS(VlDirL,' ','Model folder name?',72,' ',
     &                    'folder name',IER,2)
      WRITE(doit,'(2a,1x,3a)') 'rcp -r ',VlDirR(1:lnblnk(VlDirR)),
     &  upath(1:lnblnk(upath)),'/',VlDirL(1:lnblnk(VlDirL))
      call runit(doit,'-')
      WRITE(LCMDFL,'(5a)')upath(:lnblnk(upath)),'/',
     &        VlDirL(1:lnblnk(VlDirL)),'/cfg/','/conduction.cfg'
      call fdroot(LCMDFL,path,LCFGF)
      itrc=0
      IComp=1
      IUnit=IFIL+1
      IFCFG=IFIL+5
      IAPROB=IFIL+6
      MODE='ALL '
      CALL ERSYS(LCFGF,IFCFG,IAPROB,MODE,ITRC,IER)
      NZONES=NCOMP
      ISFSUM=0
      CALL ZDATA (ITRC,IER,NZONES,ISFSUM)
      call georead(IUnit,LGEOM(IComp),IComp,1,IUOUT,IER)
      CALL ECONST(LTHRM(IComp),IUnit,IComp,ITRC,IUOUT,IER)
      CALL EROPER(ITRC,IUOUT,IUnit,IComp,IER)
      CALL EZCTLR(IUnit,ITRC,IUOUT,IER)
      IF(IHC(IComp).EQ.1)
     &   call ehtcff(LHCCO(ICOMP),IUnit,IER)
      RETURN
      END


C ********** VMP111 **********
C Modifies the parameters of the test.

      SUBROUTINE VMP111
#include "building.h"
#include "geometry.h"
#include "control.h"
#include "schedule.h"

      integer ncomp,ncon
      COMMON/C1/NCOMP,NCON
      COMMON/C3/IC1(MCON),IE1(MCON),ICT(MCON),IC2(MCON),IE2(MCON)
      COMMON/HCFP/IHCDT,IHCFP(MDTY),ST(MDTY,MBP),EN(MDTY,MBP),
     &            HCI(MDTY,MBP,MS),HCE(MDTY,MBP,MS),ICTL(MDTY,MBP),
     &            IHCI(MDTY,MBP,MS),IHCE(MDTY,MBP,MS),
     &            CVdata(MDTY,MBP,MS,8)
      COMMON/T1/NE(MS),NAIRG(MS),IPAIRG(MS,MGP),RAIRG(MS,MGP)
      COMMON/T2/CON(MS,ME),DEN(MS,ME),SHT(MS,ME),THK(MS,ME)
      COMMON/PREC9/NCONST(MCOM),NELTS(MCOM,MS),NGAPS(MCOM,MS),
     &             NPGAP(MCOM,MS,MGP)
      COMMON/GR1D01/NNDS,NNDZ(MCOM),NNDC(MCOM,MS),NNDL(MCOM,MS,ME)
      COMMON/Vld01/NLayer,Cond(ME),Dens(ME),Cp(ME),DX(ME),NPLyr(ME)
      COMMON/Vld02/Tinit,IndBO,IndBI,Tout,Tin,Hout,Hin

      IComp=1
      nconst(IComp)=6
      do 100 isur=1,nsur
        NELTS(IComp,isur)=NLayer
        NGAPS(IComp,isur)=0
        ne(isur)=NLayer
        if(IndBO.eq.1)then
          HCE(1,1,isur)=Hout
        elseif(IndBO.eq.2)then
          HCE(1,1,isur)=15.0
        endif
        if(IndBI.eq.1)then
          HCI(1,1,isur)=Hin
        elseif(IndBI.eq.3)then
          HCI(1,1,isur)=10.0
        endif
        NAIRG(isur)=0
        NNDC(IComp,isur)=1
        do 110 il=1,NLayer
          con(isur,il)=Cond(il)
          den(isur,il)=Dens(il)
          sht(isur,il)=Cp(il)
          thk(isur,il)=DX(il)
          nndl(IComp,isur,il)=NPLyr(il)
          NNDC(IComp,isur)=NNDC(IComp,isur)+NPLyr(il)
 110    continue
 100  continue
      if(IndBI.eq.1)then
        IBSN(1,1)=0
        IBSN(1,1)=0
        IBSN(1,1)=0
        IBSN(1,1)=0
      elseif(IndBI.eq.3)then
        IBSN(1,1)=0
        IBSN(1,1)=1
        IBSN(1,1)=NNDC(1,1)
        IBSN(1,1)=0
      endif
      bmiscd(1,1,1,6)=Tin
      bmiscd(1,1,1,7)=Tin
      do 200 ic=1,ncon
        if(IndBO.eq.1)then
          ICT(ic)=2
          IC2(ic)=int(Tout)
          IE2(ic)=0
        elseif(IndBO.eq.2)then
          ICT(ic)=5
          IC2(ic)=0
          IE2(ic)=0
        endif
        IC2(ic)=int(Tout)
 200  continue
      return
      end


C ********** VSaveP **********
C Saves the parameters of the test to the model files.

      SUBROUTINE VSaveP(NPER,ier)
#include "building.h"
#include "model.h"

      COMMON/OUTIN/IUOUT,IUIN,IEOUT
      COMMON/FILEP/IFIL
      COMMON/GR1D05/LGRD1D
      CHARACTER LGRD1D*72
      COMMON/GR1D06/IGR1D
      LOGICAL IGR1D
      common/cctlnm/ctldoc,lctlf

      CHARACTER ctldoc*248,LCTLF*72

      logical QUIET,XST

      QUIET=.true.
      ITRC=0
      IComp=1

C Save configuration and connection files.
      CALL EMKCFG('-',IER)

C Save control file.
      if(LCTLF(1:2).eq.'  '.or.LCTLF(1:4).eq.'UNKN')then
        continue
      else
        ICTLF=IFIL+1
        CALL ERPFREE(ICTLF,ISTAT)
        XST=.false.
        call FINDFIL(LCTLF,XST)
        if(XST)CALL CTLWRT(ICTLF,IER)
      endif

C Save geometry file.
      IUF=IFIL+2
      call geowrite(IUF,LGEOM(ICOMP),IComp,IUOUT,3,IER)

C Save construction file.
      CALL EMKCON(LTHRM(ICOMP),IUF,IComp,QUIET,IER)

C Save operation file.
      CALL EMKOPER(IUF,LPROJ(ICOMP),IComp,IER)

C Save additional files if existing.
      if(IGR1D)CALL SAVE1D(LGRD1D,IUF,IUOUT,ITRC,IER)
      if(IHC(IComp).EQ.1)CALL EMKHTC(LHCCO(ICOMP),ICOMP,IUF,IUOUT,IER)

      return
      end


C ********** VIn111 **********
C Creates the script for bps simulation.

      SUBROUTINE VIn111(ier)

      COMMON/FILEP/IFIL
      common/rpath/path
      COMMON/GR1D04/GAM,RGAM
      COMMON/Vld02/Tinit,IndBO,IndBI,Tout,Tin,Hout,Hin
      COMMON/Vld04/DTime,NTime,MonNod,NRoot,NIter
      common/deflt4/dinstpath

      CHARACTER*72 FILE,path,dinstpath*60

      IUF=IFil+2
      FILE='RunBps.exe'
      CALL EFOPSEQ(IUF,FILE,3,IER)
      write(IUF,'(a)')'bps -mode text << EOF'
      write(IUF,'(a)')'a'
      write(IUF,'(2a)')path(1:lnblnk(path)),'conduction.cfg'
      write(IUF,'(a)')'b'
      write(IUF,'(2a)')dinstpath(1:lnblnk(dinstpath)),'/climate/clm67'
      write(IUF,'(a)')'c'
      write(IUF,'(2a)')path(1:lnblnk(path)),'conduction.res'
      write(IUF,'(a)')'1 1'
      idy=int(DTime*NTime/3600.0/24.0)+2
      call edayr(idy,id,im)
      write(IUF,'(i2,1x,i2)')id,im
      write(IUF,'(a)')'3'
      write(IUF,'(a)')'1'
      write(IUF,'(a)')'g'
      write(IUF,'(a)')'f'
      write(IUF,'(F7.2)')Tinit
      write(IUF,'(a)')'a'
      write(IUF,'(a)')'1'
      write(IUF,'(a)')'d'
      write(IUF,'(a)')'1'
      write(IUF,'(a)')'b'
      write(IUF,'(F6.2)')Gam
      write(IUF,'(a)')'-'
      write(IUF,'(a)')'*'
      write(IUF,'(a)')'s'
      write(IUF,'(a)')'n'
      write(IUF,'(a)')'conduction'
      write(IUF,'(a)')'y'
      write(IUF,'(a)')'y'
      write(IUF,'(a)')'-'
      write(IUF,'(a)')'-'
      write(IUF,'(a)')'EOF'
      CALL ERPFREE(IUF,ISTAT)
      return
      end

C *********************************************************************
C VRes11  creates the script file required for res module.
C *********************************************************************
      SUBROUTINE VRes11(ier)

      COMMON/FILEP/IFIL
      common/rpath/path
      COMMON/Vld04/DTime,NTime,MonNod,NRoot,NIter

      CHARACTER*72 FILE,path

      IUF=IFil+2
      FILE='RunRes.exe'
      CALL EFOPSEQ(IUF,FILE,3,IER)
      write(IUF,'(a)')'res -mode text << EOF'
      write(IUF,'(a,a)')path(1:lnblnk(path)),'conduction.res'
      write(IUF,'(a)')'c'
      write(IUF,'(a)')'g'
      write(IUF,'(a)')'b'
      write(IUF,'(a)')'l'
      write(IUF,'(a)')'-'
      write(IUF,'(a)')'a'
      write(IUF,'(a)')'1'
      write(IUF,'(a)')'>'
      write(IUF,'(a)')'b'
      write(IUF,'(a,a)')path(1:lnblnk(path)),'Simulation.out'
      write(IUF,'(a)')'Outside nodes temperature'
      write(IUF,'(a)')'!'
      write(IUF,'(a)')'-'
      write(IUF,'(a)')'-'
      write(IUF,'(a)')'-'
      write(IUF,'(a)')'EOF'
      CALL ERPFREE(IUF,ISTAT)

C      if(MonNod.eq.1)then
C        write(IUF,'(a)')'b'
C        write(IUF,'(a)')'a'
C      elseif(MonNod.eq.-1)then
C        write(IUF,'(a)')'a'
C        write(IUF,'(a)')'a'
C      else
C       write(IUF,'(a)')'c'
C        write(IUF,'(a)')'a'
C        write(IUF,'(I2)')MonNod
C      endif

      return
      end

C *********************************************************************
C VPSimu  execute bps simulation based on script file RunBps.exe.
C *********************************************************************
      SUBROUTINE VPSimu

      common/rpath/path

      character doit*124,path*72

      write(doit,'(3a)')'chmod 755 ',path(1:lnblnk(path)),
     &                     'RunBps.exe'
      call runit(doit,'-')
      write(doit,'(3a)')'rm -f ',path(1:lnblnk(path)),
     &                     'conduction.res'
      call runit(doit,'-')
      write(doit,'(3a)')'rm -f fort1964.tmp'
      call runit(doit,'-')
      write(doit,'(2a)')path(1:lnblnk(path)),
     &                 'RunBps.exe > fort1964.tmp'
      call runit(doit,'-')
      write(doit,'(3a)')'rm -f fort1964.tmp'
      call runit(doit,'-')
      write(doit,'(3a)')'rm -f ',path(1:lnblnk(path)),
     &                     'Simulation.out'
      call runit(doit,'-')
      write(doit,'(3a)')'rm -f ',path(1:lnblnk(path)),
     &                     'Simulation.out.par'
      call runit(doit,'-')
      write(doit,'(3a)')'chmod 755 ',path(1:lnblnk(path)),
     &                     'RunRes.exe'
      call runit(doit,'-')
      write(doit,'(2a)')path(1:lnblnk(path)),
     &                 'RunRes.exe > fort1964.tmp'
      call runit(doit,'-')
      write(doit,'(3a)')'rm -f fort1964.tmp'
      call runit(doit,'-')
      return
      end

C *********************************************************************
C VAn111  analytical solution.
C *********************************************************************
      SUBROUTINE VAn111(ier)
#include "building.h"
#include "validation.h"

      COMMON/FILEP/IFIL
      COMMON/Vld01/NLayer,Cond(ME),Dens(ME),Cp(ME),DX(ME),NPLyr(ME)
      COMMON/Vld02/Tinit,IndBO,IndBI,Tout,Tin,Hout,Hin
      COMMON/Vld03/Alpha(ME),VX(MEP1)
      COMMON/Vld04/DTime,NTime,MonNod,NRoot,NIter
      COMMON/Vld05/ILayer,VXX
      COMMON/Vld06/TermSt,VBeta(MEigen)

      DOUBLE PRECISION VBeta,Beta,BetaS,PsiSml(MEP1),PsiI
      DOUBLE PRECISION TermSt,Term,Term1,Term2,TermTU
      DOUBLE PRECISION TermT1,TermT2,TermT3,TermT4,TermT5,VNorm
      DOUBLE PRECISION VFunc,Omg,OmgL,Omg1,OmgI,OmgN,U1,Unp1
      DOUBLE PRECISION VldP,VldQ,TermTr,TempV

      CHARACTER*72 FILE

      LOGICAL Found

      IUF=IFil+1
      FILE='Analytical.out'
      CALL EFOPSEQ(IUF,FILE,3,IER)
      
      NodeC=1
      VXX=0.0
      VX(1)=0.0
      Found=.FALSE.
      DO 10 L=1,NLayer
        Alpha(L)=Cond(L)/(Dens(L)*Cp(L))
        VX(L+1)=VX(L)+DX(L)
        IF(MonNod.EQ.-1)THEN
          ILayer=L
          VXX=VXX+DX(L)
        ELSEIF(.NOT.Found)THEN
          IF(MonNod.LE.NodeC)THEN
            NodeD=MonNod-NodeC
            VXX=VXX+DX(L)*NodeD/NPLyr(L)
            ILayer=L
            Found=.TRUE.
          ELSE
            NodeC=NodeC+NPLyr(L)
            VXX=VXX+DX(L)
          ENDIF
        ENDIF
 10   CONTINUE

C Solve the steady state part of the analytical solution, and find the
C eigen values.
C Solve the steady state term.
      NL=NLayer
      I=ILayer
      TermS1=0.0
      DO 20 J=I+1,NL
        TermS1=TermS1+DX(J)/Cond(J)
 20   CONTINUE
      TermS2=0.0
      DO 30 J=1,I-1
        TermS2=TermS2+DX(J)/Cond(J)
 30   CONTINUE
      TermS3=TermS1+TermS2+DX(I)/Cond(I)
      TermS4=(1.0+Hin*((VX(I+1)-VXX)/Cond(I)+TermS1))*Hout*Tout
      TermS5=(1.0+Hout*((VXX-VX(I))/Cond(I)+TermS2))*Hin*Tin
      TermSt=(TermS4+TermS5)/(Hout+Hin+Hout*Hin*TermS3)

C Find the eigen values.
      DO 40 K=1,NRoot
        IF(K.EQ.1)THEN
          BetaS=0.0
        ELSE
          BetaS=VBeta(K-1)
        ENDIF
        Beta=BetaS+.1D-3
        CALL VEigen(K,Beta)
        VBeta(K)=Beta
 40   CONTINUE

C Solve the transient part of the solution.
      VTime=0.0
      TempV=Tinit
      WRITE(IUF,'(F10.1,4X,F8.2)')VTime,TempV
      DO 100 ITime=1,NTime
        VTime=DTime*ITime
        TermTr=0.0
        I=ILayer
        DO 200 K=1,NRoot
          Beta=VBeta(K)      
          PsiSml(1)=0.1D1
          Term=(VldP(1,Beta,VX(1))-Hout)*PsiSml(1)             
          PsiSml(2)=Term/VldP(1,Beta,VX(2))
          DO 210 L=2,NL
            Term1=VldP(L,Beta,VX(L))-VldQ(L-1,Beta,VX(L))
            Term2=VldP(L-1,Beta,VX(L))*PsiSml(L-1)
            PsiSml(L+1)=(Term1*PsiSml(L)-Term2)/VldP(L,Beta,VX(L+1))
 210      CONTINUE

C Calculate the Psi
          OmgI=Omg(I,Beta)
          TermT1=PsiSml(I)*Dsin(OmgI*(VX(I+1)-VXX))
          TermT2=PsiSml(I+1)*Dsin(OmgI*(VXX-VX(I)))
          PsiI=(TermT1+TermT2)/Dsin(OmgI*DX(I))

C Determine the norm
          VNorm=0.0
          DO 220 L=1,NL
            OmgL=Omg(L,Beta)
            TermT1=4.0*PsiSml(L)*PsiSml(L+1)
            TermT2=OmgL*DX(L)*Dcos(OmgL*DX(L))-Dsin(OmgL*DX(L))
            TermT3=PsiSml(L)**2+PsiSml(L+1)**2
            TermT4=Dsin(2.0*OmgL*DX(L))-2.0*OmgL*DX(L)
            TermT5=0.2D1*OmgL*(Dcos(0.2D1*OmgL*DX(L))-0.1D1)
            VNorm=VNorm+Dens(L)*Cp(L)*
     &                 (TermT1*TermT2+TermT3*TermT4)/TermT5
 220      CONTINUE

C Determine the integral function.
          VFunc=0.0
          DO 230 L=1,NL
            OmgL=Omg(L,Beta)
            TermT1=(PsiSml(L)+PsiSml(L+1))*(0.1D1-Dcos(OmgL*DX(L)))
            VFunc=VFunc+Dens(L)*Cp(L)*TermT1/(OmgL*Dsin(OmgL*DX(L)))
 230      CONTINUE
          VFunc=VFunc*Tinit
            
C Calculate the quantities U1 and Unp1.
          Omg1=Omg(1,Beta)
          TermT1=(0.1D1-Omg1*Cond(1)/Dtan(Omg1*DX(1)))*PsiSml(1)
          TermT2=Omg1*Cond(1)/Dsin(Omg1*DX(1))*PsiSml(2)
          U1=(TermT1+TermT2)/(1.0+Hout)
          OmgN=Omg(NL,Beta)
          TermT1=(0.1D1-OmgN*Cond(NL)/Dtan(OmgN*DX(NL)))*PsiSml(NL+1)
          TermT2=OmgN*Cond(NL)/Dsin(OmgN*DX(NL))*PsiSml(NL)
          Unp1=(TermT1+TermT2)/(1.0+Hin)
          TermTU=(Hout*Tout*U1+Hin*Tin*Unp1)/Beta**2

C Estimate the transient term.
          TermTr=TermTr+PsiI*Dexp(-Beta**2*VTime)/VNorm*(VFunc-TermTU)
 200    CONTINUE

C Estimate the temperature.
        TempV=TermSt+TermTr
        WRITE(IUF,'(F10.1,4X,F8.2)')VTime,TempV
 100  CONTINUE
      CALL ERPFREE(IUF,ISTAT)
      RETURN
      END
