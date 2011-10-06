C This file is part of the ESP-r system.
C Copyright Energy Systems Research Unit, University of
C Strathclyde, Glasgow Scotland, 2001 as well as
C Stephane.Citherlet@epfl.ch

C ESP-r is free software.  You can redistribute it and/or
C modify it under the terms of the GNU General Public
C License as published by the Free Software Foundation 
C (version 2 or later).

C ESP-r is distributed in the hope that it will be useful
C but WITHOUT ANY WARRANTY; without even the implied
C warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
C PURPOSE. See the GNU General Public License for more
C details.

C You should have received a copy of the GNU General Public
C License along with ESP-r. If not, write to the Free
C Software Foundation, Inc., 59 Temple Place, Suite 330,
C Boston, MA 02111-1307 USA.

C ECO 
C This interface provide the Building ECOBALANCE calculation
C The Ecobalance calculation is based on information extracted from 
C the building composites file (BCF), which contains all information 
C related to all composites used in the project (System)

C List of variables:
C  BCFNAME = Name of Building composites file (BCF)
C  IBCF = Unit channel of BCF (=22)
C  MENUSTR   (String) Menu item (max. of 35 char !)
C  Length    (Integer) Length of a string

C  UnitEner (CHARACTER) : Unit to display Energy results: 1=[kWh], 2=[MJ]
C  UnitMass (CHARACTER) : Unit to display Mass results: 1=[kg], 2=[g]
C  OldUnit (CHARACTER)  : Sting used to clear textual feedback if user change unit
C
C  ILoss (Integer) = Flag to include the break & loos or not in the calculation results
C  IRep (Integer) = Flag to define the reporting level (1: Summary , 2: Detailed)

      PROGRAM eco
      USE START_UP
#include "building.h"
#include "model.h"
#include "geometry.h"
#include "LCA.h"
#include "esprdbfile.h"
C esprdbfile.h supplies the following:
C LAPRES,IAPRES (for window pressure database)
C LOPTDB,IOPTDB (for optical database)
C LFMAT,IFMAT,LFMUL,IFMUL (for materials and constructions database)
C default file names for databases
#include "material.h"
#include "espriou.h"
#include "prj3dv.h"
      
      integer lnblnk  ! function definition

      integer igraphiclib  ! external definition

C Commons for I/O Channel, mode type and fonts.
      COMMON/OUTIN/IUOUT,IUIN
      integer ifs,itfs,imfs
      COMMON/GFONT/IFS,ITFS,IMFS
      COMMON/SPAD/MMOD,LIMIT,LIMTTY
      COMMON/surfsel/isursel(MCON),NbSurSel(MCOM),ISelCon(MCON),
     &               ISelComp(MCON)

C File unit
      COMMON/FILEP/IFIL

C Interface frame dimensions
      integer menuchw,igl,igr,igt,igb,igw,igwh
      COMMON/VIEWPX/menuchw,igl,igr,igt,igb,igw,igwh
      common/appcols/mdispl,nifgrey,ncset,ngset,nzonec
      common/appw/iappw,iappx,iappy

C View point, view direction, ...
      COMMON/initv/initvt,EYEMI(3),VIEWMI(3),ANGI

C Scale the project
      COMMON/RAY2/ITDSP,ITBND,ITEPT,ITZNM,ITSNM,ITVNO,ITORG,ITSNR,
     &            ITOBS,ITHLS,ITHLZ,ITGRD,GRDIS,ITPPSW
      COMMON/RAY3/MODIFY,MODLEN,MODBND

C Zone information
      COMMON/gzonpik/izgfoc,nzg,nznog(mcom)
      
      integer ncomp,ncon
      COMMON/C1/NCOMP,NCON
      COMMON/Displ/SelSurf(MCON)

C Composites information
      COMMON/CompPik/IComfoc,NComPik,LstComp(MMLC)
      COMMON/C21/IFCFG,cfgroot,LCFGF

C Defaults and paths
      COMMON/DEFLT2/DFCFG,DFCTL,DEFRLB,DAFRES,DAPROB,DPNF
      common/rpath/path
      COMMON/FOPENED/CFGOK,MLDBOK,MATDBOK,CTLOK,OPTKOK

C For exporting to file
      COMMON/exporttg/xfile,tg,delim
      COMMON/exporttgi/ixopen,ixloc,ixunit

C ECO specific commons 
      COMMON/ProjTxt/TxtCompo,TxtMat
      COMMON/EVGENS/BCFName,System,BCFVer
      COMMON/EVGENN/IBCF
      COMMON/EVGENL/BCFOK
      COMMON/LCAUnit/UnitEner,UnitMass,UnitImp
      COMMON/LCAFlag/ILoss,IcalType,Irep
      COMMON/CompDim/HeightCo,WidthCo, Perimeter, Area

      COMMON/EIAScnPt/FabPt(MMAT),TrPt(MMAT,MNbTrans),
     &                RecyPt(MMAT),ReTrPt(MMAT,MNbTrans),
     &                BurnPt(MMAT),BuTrPt(MMAT,MNbTrans),
     &                DumpPt(MMAT),DuTrPt(MMAT,MNbTrans),
     &                ComAsPt(MMLC,MNbAss),CoTrPt(MMLC,MNbTrans),
     &                MaintTyp(MMLC,MNbM),MaintPt(MMLC,MNbM),
     &                ComAsPa(MMLC,MNbAss),AsProjPt(MMLC,MNbAss),
     &                AsProjPa(MMLC,MNbAss),LayerCat(MMLC,ME),
     &                MainPart(MMLC,MNbM),LayerNam(MMLC,ME),
     &                LayerTyp(MMLC,ME)

      COMMON/DesMLC/MatNameco(MMAT),MatDesc(MMAT),MatCat(MMAT),
     &              CompoNam(MMLC), LayerDes(MMLC,ME)

      COMMON/SPECMASS/SMPLayer(MMLC,ME),SMSLayer(MMLC,ME),IWhich,
     &                SupFab(MMLC,ME),CompSMP(MMLC),CompSMS(MMLC),
     &                SupTr(MMLC,ME),SupTrMat(MMLC,ME),WeigtMat

      COMMON/EIAhigh/NbComp,NbMat,IMatID(MMLC,ME),ImatDbID(0:MMAT-1),
     &               LCIATag
      
C Significant figure reporting limit (NSIGFIG).
      common/SFIG/NSIGFIG

C Name of current application
      common/APPNAME/cAppName
      character cAppName*12

      integer legmat  ! local variable for legacy index.
      integer matarrayindex ! the indes within matdatarray
      logical closemat1,closemat2

      LOGICAL OK,CFGOK,MLDBOK,MATDBOK,CTLOK,OPTKOK,SelSurf
      LOGICAL MODIFY,MODLEN,MODBND,XST,chdb,BCFOK,unixok

      DIMENSION ITEM(18) 

      REAL HeightCo,WidthCo, Perimeter, Area
      CHARACTER BCFName*72,DBCFNam*72,System*72,BCFVer*5
      CHARACTER LCFGF*72,cfgroot*24
      CHARACTER outs*124,ITEM*35,Hold*24
      CHARACTER inf*144,LCMDFL*144
      CHARACTER*72 DFCFG,DFCTL,DEFRLB,DAPROB,DAFRES,DPNF
      CHARACTER path*72,L144*144
      CHARACTER NAM*72
C      CHARACTER tab*1
      character fs*1, TxtReport*24,dstmp*24
      CHARACTER TxtZone*31,TxtCompo*22,TxtMat*22, TxtLCA*28
      CHARACTER UnitEner*5, UnitMass*4, UnitImp*8, Oldunit*8
      CHARACTER xfile*144,tg*1,delim*1,MODE*4

      INTEGER IWHICH
      LOGICAL close

      CHARACTER*12 FabPt,TrPt,RecyPt,ReTrPt,BurnPt,BuTrPt,DumpPt,DuTrPt
      CHARACTER*12 ComAsPt,CoTrPt,MaintPt,AsProjPt
      CHARACTER*1 ComAsPa,MaintTyp,LayerCat,MainPart,AsProjPa,LayerTyp

      CHARACTER*12 CompoNam,LayerNam,MatNameco
      CHARACTER*72 MatDesc,MatCat,LayerDes

      integer iglib   ! if 1 then X11, if 2 then GTK, if 3 then text only.
      integer numberofzones ! to pass to updwire rather than ncomp
      integer NITEMS,INO ! max items and current menu item

C For help messages
      character helpinsub*24 ! subroutine name
      character helptopic*24 ! string (unique) for topic
      integer nbhelp     ! number of help lines found

C Initial assumptions (summary reporting).
      call ezero
      call curmodule('eco ')
      cAppName = 'eco'
      helpinapp='eco'  ! set once for the application
      helpinsub='eco'  ! set for MAIN
C      tab = CHAR(9)
      ITRC=1
      IUIN = 5
      IUOUT = 6
      ixunit = 7
      xfile = ' ' 
      LIMTTY=24
      LIMIT =24
      IFS=1
      ITFS=1
      NSIGFIG=3
      matver=0.0   ! initial assumption of binary materials database
      itrc=2
      ILoss = 0
      IRep= 1

C Configuration file on IFIL+5.
C Primitives on IFIL+13, mlc on IFIL+14, optics on IFIL +15.
C Temporary IFIL+1.
      IFIL = 10
      IFCFG=IFIL+5
      IUF=IFIL+6
      IFMAT=IFIL+13
      IFMUL=IFIL+14
      IOPTDB=IFIL+15
      IBCF=IFIL+22

C ECO initialiastion
      BCFNAME = 'UNKNOWN'
      DBCFNAM = 'UNKNOWN'
      DFCFG ='UNKNOWN'

C Get command line parameters.
      call parpsf(MODL,iappw,iappx,iappy,inf)

C Set folder separator (fs) to \ or / as required.
      call isunix(unixok)
      if(unixok)then
        fs = char(47)
      else
        fs = char(92)
      endif
      write(path,'(a1,a1)')'.',fs

C Set defaults.
      OK=.FALSE.
      CFGOK=.FALSE.
      MLDBOK=.FALSE.
      MATDBOK=.FALSE.
      CTLOK=.FALSE.
      OPTKOK=.FALSE.
      MODIFY=.FALSE.
      MODLEN=.FALSE.
      MODBND=.FALSE.
      XST=.FALSE.
      CHDB=.FALSE.
      Hold=' ' 
      TxtZone='UNKNOWN'
      TxtCompo='UNKNOWN'
      TxtMat='UNKNOWN'
      TxtLCA='N/A'
      TxtReport='Surface'
      UnitImp='[Imp]'
      UnitEner='[MJ]'
      UnitMass='[kg]'
      OldUnit=' '
      Area = 0.0
      Perimeter = 0.0
      WidthCo = 0.0
      HeightCo = 0.0
      IcalType=1
      nzg=0

C Name and category of the selected primitive
      Nam='  '
      LCFGF='UNKNOWN'

C Assume no focous zone.
C      ifocz = -1

C Initialise coordinates for eye point, view point and angle of view.
      EYEM(1)=-100.
      EYEM(2)=-100.
      EYEM(3)=100.
      VIEWM(1)=10.
      VIEWM(2)=10.
      VIEWM(3)=10.
      ANG=40.

C General image option flags.
      ITDSP=0
      ITBND=1
      ITEPT=0
      ITZNM=0
      ITSNM=1
      ITVNO=1
      ITORG=0
      ITSNR=0
      ITOBS=0
      ITGRD=0
      GRDIS=0.0
      ITPPSW=0
      IFS=1
      ITFS=1

C ier is standard error check variable. 0=OK
      ier=0

C Interface setup, initialise output device, assume minimal trace.
      MMOD=MODL
      if(iappw.eq.0.and.iappx.eq.0.and.iappy.eq.0)then
        iappw=670
        iappx=30
        iappy=30
      else
        if(iappx.le.0)iappx=30
        if(iappy.le.0)iappy=30
        if(iappw.le.200)then
          iappwi=int(670*iappw*0.01)
          iappw=iappwi
        elseif(iappw.gt.200)then
          continue
        endif
      endif

C Set pixels high to iappw and pixels wide to factor in monitor size.
      iapphi=iappw
      iappwi=int(real(iappw)*(1024.0/780.0))

      if(iappw.gt.0.and.iappw.lt.100)then
        menuchw = MAX0(int(37*iappw*0.01),16)
        LIMTTY= MAX0(int(14*iappw*0.01),6)
        LIMIT = MAX0(int(14*iappw*0.01),6)
      else
        menuchw = 37
        LIMTTY=14
        LIMIT =14
      endif
      IF(MMOD.EQ.8)THEN

C Set initial font sizes (IMFS is for graphs, IFS is for dialog & text feedback).
        IMFS=1
        IFS=1
        ITFS=1
        call userfonts(IFS,ITFS,IMFS)
      ELSE
        LIMTTY=16
        LIMIT =16
      ENDIF
      CALL EPAGES(MMOD,IUIN,IUOUT,iappwi,iapphi,iappx,iappy,menuchw,
     &'Ecobalance: enquiries to esru@strath.ac.uk')

C Open the text display box equal to LIMTTY if MMOD = 8.  Problem
C not yet know so pass 0 to the wireframe control routine (updwire).
C If starting as a reduced percentage of default then widen the
C graphic display to compensate.
      IF(MMOD.EQ.8)THEN
        CALL win3d(menuchw,4,1,1,3,igl,igr,igt,igb,igw,igwh)
        iglib = igraphiclib()  ! find out if X11 or GTK or text support only.
        igw=igr-igl
        igwh=igb-igt
        if(iglib.eq.1)then
          call opencpw
          call opensetup
          numberofzones=0
          call updwire(numberofzones)  ! pass local integer
          call updcapt(1)
          call updazi(1)
        endif
        call setzscale()
        call setgscale()
        mdispl=0
        nifgrey=0
        ncset=0
        ngset=0
        nzonec=0
        call foundcolour(mdispl,nifgrey,ncset,ngset,nzonec)
        call startbuffer()
       ENDIF

       CALL ESPrVersion("summary",cAppName,IUOUT)

C Find the user's home folder then get users custom settings.
      call usrhome(upath)
      if(unixok)then
        write(esprc,'(3a)') upath(1:lnblnk(upath)),fs,'.esprc'
      else
        write(esprc,'(3a)') upath(1:lnblnk(upath)),fs,'esprc'
      endif
      call scesprc(esprc,IFIL+1,0,IIER)

C Scan the defaults file for default configuration.
C Make temporary use of file unit IFIL+1.
      call escdef(IFIL+1,IER)

C Take `c' command line file name, convert to fortran string LCMDFL.
      if(inf(1:2).ne.'  '.and.inf(1:4).ne.'UNKN')then
        LCMDFL=inf
      else
        LCMDFL='UNKNOWN'
      endif

C If an input file has been specified then load,derive geometric
C properties, display it and then present the main menu.
  42  XST=.false.
      INQUIRE (FILE=LCMDFL,EXIST=XST)
      if(XST)then
        call fdroot(LCMDFL,path,LCFGF)
        call usrmsg('Scanning supplied problem...',' ','-')
        MODE='ALL '
        call ERSYS(LCFGF,IFCFG,IUF,MODE,itrc,IER)
        if(IER.eq.0)then
          NZONES=NCOMP
          CALL ZDATA (ITRC,IER,NZONES,ISFSUM)
          CFGOK=.TRUE.
          MODIFY=.TRUE.
          MODBND=.TRUE.
          MODLEN=.TRUE.
          nzg=NCOMP
          IF(MMOD.EQ.8)then
            numberofzones=ncomp
            call updwire(numberofzones)
          endif
          DO 444 I=1,nzg
            nznog(I)=I
 444      CONTINUE

C (Re)Set all surfaces to std line width.
          CALL INLNST(1)
          izgfoc=0
          if(initvt.eq.1)then
            EYEM(1)=EYEMI(1)
            EYEM(2)=EYEMI(2)
            EYEM(3)=EYEMI(3)
            VIEWM(1)=VIEWMI(1)
            VIEWM(2)=VIEWMI(2)
            VIEWM(3)=VIEWMI(3)
            ANG=ANGI
          endif
          CALL ADJVIEW(IER)

C Open core of databases.
          call module_opendb(ier)
          if(ier.ne.0)then
            call usrmsg(
     &        'Possible problems with one or more of the',
     &        'construction and optical databases, please check.','W')
            ier = 0
          endif
        endif
        TxtZone = 'UNKNOWN'

C No loaded project 
      ELSE
        TxtZone = 'N/A'
      endif


      IF (BCFName.ne.'UNKNOWN') THEN
        write(outs,'(a,a,a)') 'Loading ',BCFName(1:lnblnk(BCFName)),
     &      '...'
        call usrmsg(outs,' ','-')
        CALL SCNBCF(IBCF,BCFName,IER)
        IF (IER .GT. 0) THEN 
          CALL EDISP(iuout,'Loading constructions not completed..')
          BCFOK=.FALSE.
          BCFNAME='UNKNOWN'
        ELSE
          write(outs,'(a,a,a)') 'Loading ',BCFName(1:lnblnk(BCFName)),
     &        '...done.'
            call usrmsg(outs,' ','P')
        ENDIF
      ENDIF
      

C Main Menu
10    INO = -4

C Analyse the selected item to allow an LCA calculation
      IF (CFGOK) THEN
        IF (IcalType.eq.1) THEN
          IF (TxtZone(1:7).EQ.'UNKNOWN') THEN
            TxtLCA = 'N/A'
            TxtMat='UNKNOWN'
          ENDIF
        ELSEIF (IcalType.eq.2) THEN
          IF (TxtCompo(1:7).EQ.'UNKNOWN') THEN
            TxtLCA = 'N/A'
            TxtMat='UNKNOWN'
          ENDIF      
        ELSEIF (IcalType.eq.3) THEN
          IF (TxtMat(1:7).EQ.'UNKNOWN') TxtLCA = 'N/A'
          call eclose(WeigtMat,0.0,0.0001,close)
          IF (close) TxtMat='UNKNOWN'
        ENDIF
      ELSE 
        IF ((TxtZone(1:7).EQ.'UNKNOWN').AND.
     &      (TxtCompo(1:7).EQ.'UNKNOWN').AND.
     &      (TxtMat(1:7).EQ.'UNKNOWN')) THEN
          TxtLCA = 'N/A'
        ENDIF
      ENDIF

C Display menu
      write(ITEM(1),'(A,A)')      'a Problem : ',LCFGF(1:22)
      write(ITEM(2),'(A,A)')      '  Path : ',path(1:25)
      write(ITEM(3),'(A,A)')      'b BCF  : ',BCFName(1:25)
      ITEM(4)=                    '  ---------Approach----------- '
      if(IcalType.eq.1)then
        write(ITEM(5),'(A)')      'c Approach >> project based'
        write(ITEM(6),'(A,A)')    'd Zone : ',TxtZone(1:25)
        write(ITEM(7),'(A)')      '  '
      elseif(IcalType.eq.2)then
        write(ITEM(5),'(A,A)')    'e Approach >> Construction based'
        write(ITEM(6),'(A,A)')    'f Construction : ',TxtCompo(1:18)
        write(ITEM(7),'(A,F8.3)') '  Total area [m2] : ',Area
      elseif(IcalType.eq.3)then
        write(ITEM(5),'(A,A)')    'e Approach >> material based'
        write(ITEM(6),'(A,A)')    'f Material : ',TxtMat(1:22)
        write(ITEM(7),'(A,F8.3)') '  Total mass [kg]: ',WeigtMat
      endif
      ITEM(8) =                '  ----------Options-------------'
      write(ITEM(9),'(2a)')'1 Calculation level >> ',TxtReport(1:10)
      if(IRep.eq.1)then
        ITEM(10)=              '2 Reporting level >> Summary '
      else
        ITEM(10)=              '3 Reporting level >> Detailed '
      endif
      if(ILoss.eq.0)then
        ITEM(11)=              '4 Results >> Include loss         '
      else
        ITEM(11)=              '4 Results >> Does not include loss'
      endif
      write(ITEM(12),'(6a)')   '5 Units :  ', 
     &      UnitEner, ' - ', UnitMass, ' - ', UnitImp

      ITEM(13)=                '  ------------------------------- '
      write(ITEM(14),'(A,A)')  '* Calculation ',TxtLCA(1:18)
      ITEM(15) =               '  ------------------------------- '
      ITEM(16) =               'X Export results to file '
      ITEM(17)=                '? Help                            '
      ITEM(18)=                '- exit eco                        '
      NITEMS = 18

C Use askabout to instanciate the initial help messages (2nd parameter is one).
      call askabout('eco ',1)

C Present the main menu.
      CALL EMENU(' Eco Main menu',ITEM,NITEMS,INO)
      IF(INO.EQ.NITEMS)THEN 

c End program.
        call usrmsg(' Closing Eco desktop...',' ','P')
        CALL EPAGEND
        STOP

      elseif(INO.EQ.10)THEN

C Define the reporting level
        if(IRep.eq.1)then
          IRep = 2
        else
          IRep = 1 
        endif

      elseif(INO.EQ.NITEMS-1)THEN

C Help via the askabout facility in esru_ask.F The zero requests display
        call askabout('eco ',0)

      elseif(INO.EQ.12)THEN

C Define Units
        helptopic='unit_for_results'
        call gethelptext(helpinsub,helptopic,nbhelp)
        CALL easkabcd('Define the results unit.',
     &    'Do you want to modify unit for: ',' Energy ',' Mass ',
     &    ' Impacts ',' CANCEL ',IUnit,nbhelp)
        IF (IUnit.EQ.1) THEN
          CALL easkabc('Define the ENERGY unit.','Which unit : ',
     &      ' [kWh] ',' [MJ] ',' CANCEL ',IEnergy,nbhelp)
          OldUnit=UnitEner
          IF (IEnergy.EQ.1) THEN
            UnitEner='[kWh]'
          ELSE
            UnitEner='[MJ]'
          ENDIF
        ELSEIF (IUnit.EQ.2) THEN
          CALL easkabc('Define the MASS unit.',
     &      'Which unit : ','[kg]','[g]','CANCEL',IMass,nbhelp)
          OldUnit=UnitMass
          IF (IMass.EQ.1) THEN
            UnitMass='[kg]'
          ELSE
            UnitMass='[g]'
          ENDIF
        ELSEIF (IUnit.EQ.3) THEN
          CALL easkabc('Define the results format',
     &      'Which unit : ','Absolue','Relative (/m2)','CANCEL',
     &      IImp,nbhelp)
          OldUnit=UnitImp
          IF (IImp.EQ.1) THEN
            UnitImp='[Imp]'
          ELSE
            UnitImp='[Imp/m2]'
          ENDIF
        ELSEIF (IUnit.EQ.4) THEN
          GOTO 10
        ENDIF

      elseif(INO.EQ.16)THEN

C Export results to file
C First open the file, then write in and finally close the file
        helptopic='eco_export_current'
        call gethelptext(helpinsub,helptopic,nbhelp)
        itu = ixunit
        call ctlexp(xfile,ixopen,ixloc,ixunit,'T',' text',IER)
        write(outs,'(a)') 'Environmental impact assessment report.'
        call edisp(itu,outs)
        call dstamp(dstmp)
        write(outs,'(a,a)') 'Date: ',dstmp
        call edisp(itu,outs)
        CALL PreCalc
        if (IRep.eq.1) then
          CALL LCACalc(itrc,itu)
        else
          CALL LCADet(itrc,itu)
        endif
        call ctlexp(xfile,ixopen,ixloc,ixunit,'T',' text',IER)        
C        CALL ERPFREE(itu,ISTAT)
      goto 10

      elseif(INO.EQ.9)THEN

C Reporting level
        itrc=itrc+1

        if (IcalType.eq.1) then
          if(itrc.eq.5)then
            itrc=1
          endif
C MLC analysis
          if (itrc.eq.1) then
            TxtReport =             'Zone'
          elseif (itrc.eq.2) then
            TxtReport =             'Surface'
          elseif(itrc.eq.3)then
            TxtReport =             'Layer'
          elseif(itrc.eq.4)then
            TxtReport =             'Life cycle'
          endif
        elseif (IcalType.eq.2) then
          if(itrc.ge.4)then
            itrc=1
          endif

C MLC analysis
          if (itrc.eq.1) then
            TxtReport =             'Composite'
C            itrc=2
          elseif(itrc.eq.2)then
            TxtReport =             'Layer'
            itrc=3
          endif
        elseif(IcalType.eq.3)then

C Material analysis
          if(itrc.eq.3)then
            itrc=1
          endif
          if (itrc.eq.1) then
            TxtReport =               'Material level'
          elseif(itrc.eq.2)then
            TxtReport =               'Global level'
          endif
        endif

        goto 10

      elseif(INO .EQ. 11)THEN

C Include break and loss in results
        ILoss = ILoss+1
        if(ILoss .eq. 2)then
          ILoss = 0
        endif
        goto 10

      elseif (INO.eq.1) then 

C Select/change ESP-r configuration file.
C The X11 version will be returning only the name of the
C file, while the GTK version will be returning the
C name with the full path.
  289   helptopic='eco_model_file'
        call gethelptext(helpinsub,helptopic,nbhelp)
        L144=LCMDFL
        llt=lnblnk(L144)
        iglib = igraphiclib()  ! find out if X11 or GTK or text support only.
        if(iglib.eq.1.or.iglib.eq.3)then
          if(llt.lt.96)then
            CALL EASKF(L144,' Problem name? ',' ',96,DFCFG,
     &        'config file name',IER,nbhelp)
          elseif(llt.ge.96.and.llt.lt.124)then
            CALL EASKF(L144,' Problem name? ',' ',124,DFCFG,
     &        'config file name',IER,nbhelp)
          elseif(llt.ge.124.and.llt.le.144)then
            CALL EASKF(L144,' Problem name? ',' ',144,DFCFG,
     &        'config file name',IER,nbhelp)
          endif
        elseif(iglib.eq.2)then
          CALL EASKF(L144,' Problem name? ',' ',144,DFCFG,
     &      'config file name',IER,nbhelp)
        else
          CALL EASKF(L144,' Problem name? ',' ',96,DFCFG,
     &      'config file name',IER,nbhelp)
        endif

        IF(L144.EQ.' ')GOTO 289
        LCMDFL=L144
        goto 42

      elseif (INO.eq.3) then 

C Select/change BCF file.
        helptopic='eco_BCF_options'
        call gethelptext(helpinsub,helptopic,nbhelp)
  288   IF (BCFOK) THEN
          call easkabc('A BCF of the current problem is loaded.',
     &     'Do you want:','use it','load another','continue',Iw,nbhelp)
          if(iw.eq.3)then
            goto 10
          elseif(iw.eq.2)then
            CALL EASKS(BCFName,'New building composites file?',' ',72,
     &        DBCFNAM,'BCF file name',IER,nbhelp)
            IF(BCFName.EQ.' ')GOTO 288
          endif
        ELSE
          CALL EASKS(BCFName,'Building composites file?',' ',72,
     &      DBCFNAM,'BCF file name',IER,nbhelp)
          IF(BCFName.EQ.' ')GOTO 288
        ENDIF

C Attempt to read in the BCF file.
        XST=.false.
        INQUIRE (FILE=BCFName,EXIST=XST)
        if(XST)then
          BCFOK=.TRUE.
          write(outs,'(3a)') 'Loading ',BCFName(1:lnblnk(BCFName)),
     &      '...'
          call usrmsg(outs,' ','-')
          CALL SCNBCF(IBCF,BCFName,IER)
          IF (IER .GT. 0) THEN 
            CALL EDISP(iuout,'Loading composites not completed..')
            BCFOK=.FALSE.
            BCFNAME='UNKNOWN'
            GOTO 10
          ELSE
            write(outs,'(3a)') 'Loading ',BCFName(1:lnblnk(BCFName)),
     &        '...done.'
            call usrmsg(outs,' ','P')
          ENDIF
        ELSE
          CALL usrmsg('File doesn`t exist...','Select again','W')
          GOTO 288
        ENDIF
        GOTO 10

      elseif (INO.eq.5) then

        helptopic='eco_assessing_impacts'
        call gethelptext(helpinsub,helptopic,nbhelp)

        CALL EASKABC('Approaches to scope of LCA calculations:',' ',
     &   ' Project based ',' MLC based ',' Material based ',
     &   IcalType,nbhelp)
        GOTO 10
      ELSEIF (Ino.eq.6.and.IcalType.eq.1)then

C Display Zones list if project loaded and wait for INIPIC item selected.
        IF (CFGOK) THEN

C << update zoneslist to also ask for surfaces >>
          CALL Zoneslist
          IF (izgfoc.EQ.0) THEN
            CALL USRMSG(' No zone has been selected.',' ','-')
            GOTO 10
          ELSE

C Selection of the surfaces to include
C All the zones in the project have been selected
             helptopic='eco_select_manually'
             call gethelptext(helpinsub,helptopic,nbhelp)
             CALL EASKAB(' Do you want to select manually the surfaces',
     &        ' to include ',' No (include all) ',
     &        ' Yes, manually ',ixman,nbhelp)
             IF (ixman.EQ.1) THEN
               do 41 ixd=1,NZG
                  CALL ZoneDisp(NZNOG(ixd),0)
  41           continue
             ELSE
               do 40 ixd=1,NZG
                  CALL ZoneDisp(NZNOG(ixd),1)
  40           continue
             ENDIF
          ENDIF

C update menu text
          TxtZone = 'UNKNOWN' 
          if (nzg .EQ. 1) then
            write(TxtZone,'(a)') zname(nznog(1))(1:12)
            if (NbSurSel(nznog(1)) .EQ. 1) then
              TxtCompo =  Sname(nznog(1),Isursel(1))(1:12)
            elseif (NbSurSel(nznog(1)) .EQ. NbSurSel(nznog(1))) then
              TxtCompo =  ' All surfaces'
            elseif (NbSurSel(nznog(1)) .GT. 1) then
              TxtCompo = ' Several '
            endif
          elseif (nzg .LT. ncomp) then
            write(TxtZone,'(2a)')
     &        zname(nznog(1))(1:lnzname(nznog(1))),' + other'
          elseif (nzg .EQ. ncomp) then
             write(TxtZone,'(a)')' All zones'
          endif
          IF (nzg.EQ.1) then
            WRITE(TxtLCA,'(a,a)')' of ',TxtZone(1:lnblnk(TxtZone))
          ELSEIF (nzg.GT.1) then
            WRITE(TxtLCA,'(a)')' of selected zones'
          ELSEIF (nzg.EQ.ncomp) then
            WRITE(TxtLCA,'(a)')' of all zones'
          ENDIF          
        ELSE
          CALL USRMSG(' No project has been loaded.',' ','-')        
        ENDIF
        GOTO 10
 
      ELSEIF (Ino.eq.6.and.IcalType.eq.2)then

        NZG = 0
C Display the composites list.
        CALL CompoList
        IF (NComPik .EQ. 0) THEN
          CALL USRMSG(' No Composite has been selected.',' ','-')
          GOTO 10
        ELSEIF (NComPik .EQ. MMLC) THEN
          CALL USRMSG(' All composite have been selected.',' ','-')        
          TxtCompo=' All'
         GOTO 10
        ELSE
          TxtCompo= DESC(LstComp(1))(1:12)
          TxtMat= 'All'
        ENDIF

C Ask areas and perimeter for each selected composite
        helptopic='eco_no_zone_selected'
        call gethelptext(helpinsub,helptopic,nbhelp)
        write(Hold,'(2F8.3)')WidthCo,HeightCo
        CALL EASKS(Hold,' Environmental impacts of a composite :',
     &    'width and height [m] ',32,' 1. 1. ','width & height',
     &     IER,nbhelp)
        K=0
        CALL EGETWR(Hold,K,WidthCo,0.001,999.,'W','width',IER)
        CALL EGETWR(Hold,K,HeightCo,0.001,999.,'W','height',IER)
        CALL USRMSG(' ',' ','-')
        Area = HeightCo * WidthCo
        Perimeter = 2.*HeightCo + 2.*WidthCo

C update menu text
        IF (NComPik.EQ.1)  WRITE(TxtLCA,'(a,a)')' of ',
     &                           TxtCompo(1:lnblnk(TxtCompo))
        IF (NComPik.GT.1)  WRITE(TxtLCA,'(a)')' of selected composites'
        IF (NComPik.EQ.MMLC)  WRITE(TxtLCA,'(a)')' of all composites'
        GOTO 10

      ELSEIF (Ino.eq.6.and.IcalType.eq.3)then 

C Display Material list and allow selection of materials.
        NZG = 0
        IF (.NOT.BCFOK) THEN
          call usrmsg('No BCF file loaded',
     &                      'Select a BCF file first','W')
          goto 10
        ENDIF

C Find version of materials database.
 139    call eclose(matver,1.1,0.01,closemat1)
        call eclose(matver,1.2,0.01,closemat2)
        if(closemat1.or.closemat2)then
          continue
        else
          call usrmsg(
     &      'No materials data available. Re-load the model config-',
     &      'uration file or check model in the project manager.','W')
          goto 10
        endif

C Select from the available materials.
        iwhich=0
        CALL ELISTCMOD(iwhich,chdb,'-',IER)
        if(iwhich.eq.0)then
          CALL EASKABC('Your selection is `0` i.e. air. Options:',
     &      ' ','accept','reselect material','cancel',iwair,nbhelp)
          if(iwair.eq.2)then
            CALL ELISTCMOD(iwhich,chdb,'-',IER)
          elseif(iwair.eq.3)then
            GOTO 10
          endif
        endif

        legmat=IWHICH   ! which legacy index
        if(legmat.gt.0)then
          matarrayindex=mathash(legmat)  ! find matching array index

C And if matarrayindex is zero then resetn dbcon dbden dbsht.
          if(matarrayindex.eq.0)then
            DBCON=0.0; DBDEN=0.0; DBSHT=0.0 
            E=0.99; A=0.99; DRV=1.0
            NAM='AIR'
          else
            DBCON=matdbcon(matarrayindex)
            DBDEN=matdbden(matarrayindex)
            DBSHT=matdbsht(matarrayindex)
            E=matdbine(matarrayindex)
            A=matdbina(matarrayindex)
            DRV=matdbdrv(matarrayindex)
            write(NAM,'(a)') matname(matarrayindex)(1:32)
          endif
        endif

        call edisp(iuout,' ')
        write(outs,'(a,a)')'Selected category: ',
     &                      MatCat(IMatDbID(IWHICH))
        call edisp(iuout,outs)
        IF (IcalType.ne.3) THEN
          write(outs,'(a,a)')'Selected material: ',
     &                      MatNameco(IMatDbID(IWHICH))
          call edisp(iuout,outs)
        endif
        call edisp(iuout,' ')
        call edisp(iuout,
     & '  Material  | Conductivity | Density | Spec. heat | IR emis | So
     &                    lar abs. |  Vapour resistance')
        call edisp(iuout,
     & '            |    W/(m C)   |  kg/m^3 |  J/(kg C)  |   [-]   |   
     &   [-]   |         [-]       ')
        call edisp(iuout,' ')
        write (outs,99) MatNameco(IMatDbID(IWHICH))(1:12),DBCON,DBDEN,
     &                 DBSHT,E,A,DRV
   99   format(1x, A12, 3x, f7.1, 6x, f7.1, 6x, f5.1, 6x, f5.2, 7x,
     &          f5.2,9x, f7.1)
        call edisp(iuout,outs)

C ask for mass of material
        helptopic='eco_no_zone_selected'
        call gethelptext(helpinsub,helptopic,nbhelp)
        write(hold,'(F8.3)')WeigtMat
        CALL EASKS(HOLD,' Environmental impacts of a single material :',
     &  'material mass [kg] ',32,' 1. ','material mass',IER,nbhelp)
        K=0
        CALL EGETWR(HOLD,K,WeigtMat,0.001,100000.,'W','mass',IER)
        CALL USRMSG(' ',' ','-')

C Assign labels for main menu
        TxtMat = MatNameco(IMatDbID(IWHICH))
        TxtCompo = 'UNKNOWN'
        WRITE(TxtLCA,'(a,a)')' of ',MatNameco(IMatDbID(IWHICH))
        GOTO 10

C << potentially remember up to 3 materials for reporting on, need
C << common block to retain this information >>

      ELSEIF (Ino.eq.7.and.IcalType.eq.1)then 

C Display the composites list.
        CALL CompoList
        IF (NComPik .EQ. 0) THEN
          CALL USRMSG(' No Composite has been selected.',' ','-')
          GOTO 10
        ELSEIF (NComPik .EQ. NMLC) THEN
          CALL USRMSG(' All composite have been selected.',' ','-')        
          TxtCompo=' All'
         GOTO 10
        ELSE
          TxtCompo= DESC(LstComp(1))(1:12)
          TxtMat= 'All'
        ENDIF

      ELSEIF (INO.EQ.14) THEN

C LCA calculation.
        IF(TxtLCA(1:3) .NE. 'N/A') THEN 
          CALL edisp(iuout,'********** Request *************')
          if(IcalType.eq.1)then
            call edisp(iuout,' Approach : Project based')
          elseif(IcalType.eq.2)then
            call edisp(iuout,' Approach : Composite based')
            write(outs,'(a,F8.3)') ' Total area [m2] : ',Area
            call edisp(iuout,outs)
            write(outs,'(a,F8.3)') ' Total perimeter [m] : ',Perimeter
            call edisp(iuout,outs)
          elseif(IcalType.eq.3)then
            call edisp(iuout,' Approach : Material based')
          endif
          CALL PreCalc
          if (IRep.eq.1) then
            CALL LCACalc(itrc,iuout)
          else
            CALL LCADet(itrc,iuout)
          endif
        ELSE
          call edisp(iuout,' Not available. Select:')
          write(outs,'(a,a)')' one or many zone(s) and/or ',
     &      'one or many composite(s) and/or one or many material(s).'
          call edisp(iuout,outs)
          GOTO 10
        ENDIF

C End of menu code.
      ENDIF

C Return to main menu.
      GOTO 10
      END


C ************* imgdisp *************
C Dummy routine (needed for call back from c).
      subroutine imgdisp(iforce,focus,ier)
      character focus*4
     
      return
      end

C Dummy subroutine needed to compile (called from library code).
      SUBROUTINE PLELEV(direc)
      CHARACTER direc*1

      return
      end


