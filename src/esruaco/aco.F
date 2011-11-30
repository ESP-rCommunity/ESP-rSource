C This file is part of the ESP-r system.
C Copyright Energy Systems Research Unit, University of
C Strathclyde, Glasgow Scotland, 2001.

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

C RevTime : Interface for the reverberation time module
C Imgdisp : Dummy routine (needed for call back from c).

C ************RevTime*************
C Interface for reverberation time calculation. Handle the selection of:
C - project and BCF file
C - the zone selection for which the calculation has to be perform
C - the calculation method and the display mode of results
C
C List of variables:
C
C MethodRT (Integer) Reverberation time calculation method
C                    1 = Sabine, 2=Eyring & 3= Millington 
C CalcName (Char)    Name ofcalculation method according to MethodRT
C
C IOctave (Integer)  Frequency band calculation and display :
C                    1 = 1 octave or 2 = 1/3 octave
C IDisp (Integer)    Results display mode :1 = numerical or 2 = graphical
C ISE (Integer)      Equivalent surface display mode (only in numerical display mode)
C                    1 = Display or 2 = Hide
C TxtZone (Char)     Name of selected zone displayed in the main menu
C NbPub(MaxPub) (Integer)   Number of public(s)
C XNbFur(MaxFur)(Real)      Nb of unit or surface area for the current 
C                           furniture

      PROGRAM RevTime
#include "building.h"
#include "model.h"
#include "geometry.h"
#include "acoustic.h"
#include "esprdbfile.h"
C esprdbfile.h supplies the following:
C LAPRES,IAPRES (for window pressure database)
C LOPTDB,IOPTDB (for optical database)
C LFMAT,IFMAT,LFMUL,IFMUL (for materials and constructions database)
#include "material.h"
#include "espriou.h"
#include "prj3dv.h"
C prj3dv.h provides common blocks drwmat clip and image
      
      integer lnblnk  ! function definition

C Commons for I/O Channel, mode type and fonts.
      COMMON/OUTIN/IUOUT,IUIN
      COMMON/MOD/MODEL
      integer ifs,itfs,imfs
      COMMON/GFONT/IFS,ITFS,IMFS
      COMMON/SPAD/MMOD,LIMIT,LIMTTY

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

      COMMON/AirPro/RTair,RHR

C Defaults and paths
      COMMON/DEFLT2/DFCFG,DFCTL,DEFRLB,DAFRES,DAPROB,DPNF
      common/rpath/path
      COMMON/FOPENED/CFGOK,MLDBOK,MATDBOK,CTLOK,OPTKOK

      COMMON/C21/IFCFG,cfgroot,LCFGF

C For exporting to file
      COMMON/exporttg/xfile,tg,delim
      COMMON/exporttgi/ixopen,ixloc,ixunit
      COMMON/SET1/IYEAR,IBDOY,IEDOY,IFDAY,IFTIME

C ACO specific commons 
      COMMON/EVGENN/IBCF
      COMMON/EVGENL/BCFOK
      COMMON/MetNam/ACOName,CalcName(4), LabFreq(MNbAbs)
      COMMON/IFileACO/IACO

      COMMON/PubFurZone/NbPub(MaxPub),XNbFur(MaxFur),SNAeff(MS),
     &                  IndexPub(MaxPub),IndexFur(MaxFur)

      COMMON/CPubFurZone/CovSurf(MaxFur)

      COMMON/AbsCHAR/PubType(MaxPub),FurType(MaxFur),FurCat(MaxFur)
      COMMON/Zones/TxtZone
      
C Significant figure reporting limit (NSIGFIG).
      common/SFIG/NSIGFIG
C Name of current application
      common/APPNAME/cAppName
      
      character cAppName*12

      LOGICAL CFGOK,MLDBOK,MATDBOK,CTLOK,OPTKOK,ACOOK
      LOGICAL MODIFY,MODLEN,MODBND,XST,BCFOK,unixok

      DIMENSION ITEM(26) 

      CHARACTER BCFName*72,DBCFNam*72
      CHARACTER ACOName*72,DACONam*72,LCFGF*72,cfgroot*24
      CHARACTER outs*210,ITEM*35,MODE*4
      CHARACTER inf*144,LCMDFL*144
      CHARACTER*72 DFCFG,DFCTL,DEFRLB,DAPROB,DAFRES,DPNF
      CHARACTER*72 path
      CHARACTER fs*1,outsd*124
      CHARACTER TxtZone*72,CLMRF*72,TTmp*72
      CHARACTER CalcName*12,DESCR*7,DESCR1*10 
      CHARACTER tg*1,delim*1,xfile*144,dstmp*24,LabFreq*4
      CHARACTER PubType*12,FurType*12,FurCat*4,CovSurf*12

      character cVnum*16      ! returned from ESPrVersionNum
      character pagestitle*42 ! for banner title via epages call

      integer iglib   ! if 1 then X11, if 2 then GTK, if 3 then text only.
#ifdef OSI
      integer numberofzones ! to pass to updwire rather than ncomp
      integer iicapture,iiazi    ! to pass to updcapt updazi
      integer iigl,iigr,iigt,iigb,iigw,iigwh
      integer iiw1,iiw2,iiw3,iiw4,iimenu
#else
      integer*8 numberofzones
      integer*8 iigl,iigr,iigt,iigb,iigw,iigwh
      integer*8 iiw1,iiw2,iiw3,iiw4,iimenu
      integer*8 iicapture,iiazi   ! to pass to updcapt updazi
#endif
      integer NITEMS,INO  ! max items and current menu item

C For help messages
      character helpinsub*24 ! subroutine name
      character helptopic*24 ! string (unique) for topic
      integer nbhelp     ! number of help lines found (will be non-zero
                         ! because default message will be created

C Initial assumptions (summary reporting).
      call ezero
      call curmodule('aco ')
      cAppName = 'aco'
      helpinapp='aco'  ! set once for the application
      helpinsub='RevTime'  ! set for MAIN
      IUIN = 5
      IUOUT = 6
      ixunit = 7
      xfile = ' ' 
      LIMTTY=24
      LIMIT =24
      IFS=1
      ITFS=1
      itrc=0
      NSIGFIG=3
      matver=0.0   ! initial assumption of binary materials database

C Configuration file on IFIL+5.
C Primitives on IFIL+13, mlc on IFIL+14, optics on IFIL +15.
      IFIL = 10
      IFCFG=IFIL+5
      IUF=IFIL+6
      IOPTDB=IFIL+15
      IBCF=IFIL+22
      IACO=IFIL+23
      ICLMRF = IFIL+25
      IFMAT = IFIL+26
      IFMUL = IFIL+27

C ECO initialiastion <<hard coded for the atrium ofd the EOS example
C to be revised >>
C      BCFNAME = 'EOS.bcf'
C      ACOName =  'EOS_Aco.aco'
      BCFNAME = 'UNKNOWN'
      ACONAME = 'UNKNOWN'
      DFCFG ='new.cfg'
      CLMRF =' '
      DBCFNAM = '../dbm/test.bcf'
      DACONam =  './EOS_Aco.aco'

C Get command line parameters.
      call parpsf(MODL,iappw,iappx,iappy,inf)

C Set folder separator (fs) to \ or / as required.
      call isunix(unixok)
      if(unixok)then
        fs = char(47)
      else
        fs = char(92)
      endif
      write(path,'(2a)')'.',fs

C Set defaults.
      CFGOK=.FALSE.
      MLDBOK=.FALSE.
      MATDBOK=.FALSE.
      CTLOK=.FALSE.
      OPTKOK=.FALSE.
      MODIFY=.FALSE.
      MODLEN=.FALSE.
      MODBND=.FALSE.
      XST=.FALSE.
      LCFGF ='UNKNOWN'
      TxtZone='UNKNOWN'
      DO 3 I=1,MaxPub
        NbPub(I)= 0
        PubType(I)='none' 
 3    CONTINUE 
      DO 4 I=1,MaxFur
        XNbFur(I)= 0.
        FurType(I)='none' 
        CovSurf(I)='none' 
 4    CONTINUE 
      CalcName(1) = 'Sabine      '
      CalcName(2) = 'Eyring      '
      CalcName(3) = 'Millington  '
      CalcName(4) = 'All methods '
      MethodRT = 1
      IOctave = 2  
      IAirProp = 0  
      IDisp = 2
      RTair = 20.
      RHR = 60.
      IAirAb = 1
      ISE = 2
      nzg=0
      NZNOG(1) = 0

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
      MODEL=MODL
      MMOD=MODL
      if(iappw.eq.0.and.iappx.eq.0.and.iappy.eq.0)then
        iappw=660
        iappx=30
        iappy=30
      else
        if(iappx.lt.0)iappx=10
        if(iappy.lt.0)iappy=10
        if(iappw.le.200)then
          iappwi=int(660*iappw*0.01)
          iappw=iappwi
        elseif(iappw.gt.200)then
          continue
        endif
      endif

C Set pixels high to iappw and pixels wide to factor in monitor size.
      iapphi=iappw
      iappwi=int(real(iappw)*(1024.0/780.0))

      if(iappw.gt.0.and.iappw.lt.100)then
        menuchw = MAX0(int(38*iappw*0.01),16)
        LIMTTY= MAX0(int(12*iappw*0.01),6)
        LIMIT = MAX0(int(12*iappw*0.01),6)
      else
        menuchw = 38
        LIMTTY = 12
        LIMIT = 12
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

C Find the current ESP-r version number and add it to application title.
      call ESPrVersionNum(cVnum)
      write(pagestitle,'(2a)') 'ESP-r Acoustic ',cVnum(1:lnblnk(cVnum))

      CALL EPAGES(MMOD,IUIN,IUOUT,iappwi,iapphi,iappx,iappy,menuchw,
     &  pagestitle)

C Open the text display box equal to LIMTTY if MODEL = 8.  Problem
C not yet know so pass 0 to the wireframe control routine (updwire).
C If starting as a reduced percentage of default then widen the
C graphic display to compensate.
      IF(MMOD.EQ.8)THEN

C Setup and pass in parameters to win3d.
        iiw1=4; iiw2=1; iiw3=1; iiw4=3; iimenu=menuchw
        iigl=igl; iigr=igr; iigt=igt; iigb=igb; iigw=igw; iigwh=igwh
        CALL win3d(iimenu,iiw1,iiw2,iiw3,iiw4,
     &    iigl,iigr,iigt,iigb,iigw,iigwh)
C        CALL win3d(menuchw,4,1,1,3,igl,igr,igt,igb,igw,igwh)
        igl=int(iigl); igr=int(iigr); igt=int(iigt); igb=int(iigb)
        igw=int(iigw); igwh=int(iigwh)
        iglib = igraphiclib()  ! find out if X11 or GTK or text support only.
        if(iglib.eq.1)then
          call opencpw
          call opensetup
          numberofzones=0
          call updwire(numberofzones)  ! pass local integer
          iicapture=1; iiazi=1
          call updcapt(iicapture)
          call updazi(iiazi)
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

C If an input file has been specified then load and display it
C and then present the main menu. Derive master geometric data
C set for wireframe drawing.
  42  XST=.false.
      INQUIRE (FILE=LCMDFL,EXIST=XST)
      if(XST)then
        call fdroot(LCMDFL,path,LCFGF)
        call usrmsg('Scanning supplied problem...',' ','-')
        MODE='NONE'
        CALL ERSYS(LCFGF,IFCFG,IUF,MODE,0,IER)
        call usrmsg('Scanning supplied problem...done.',' ','-')
        if(IER.eq.0)then
          NZONES=NCOMP
          CALL ZDATA (ITRC,IER,NZONES,ISFSUM)
          CFGOK=.TRUE.
          MODIFY=.TRUE.
          MODBND=.TRUE.
          MODLEN=.TRUE.
          nzg=NCOMP
          IF(MODEL.EQ.8)then
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
     &        'construction and optical databases - Please check.','W')
            ier = 0
          endif
        endif
        TxtZone = 'UNKNOWN'

C No loaded project 
      ELSE
        TxtZone = 'N/A'
      endif

C If BCF and ACO file supply, just read them
      IF (BCFName(1:7).ne.'UNKNOWN') THEN
        write(outs,'(3a)') 'Loading ',BCFName(1:lnblnk(BCFName)),
     &    '...'
        call usrmsg(outs,' ','-')
        CALL SCNBCF(IBCF,BCFName,IER)
        IF (IER .GT. 0) THEN 
          CALL EDISP(iuout,'Loading constructions not completed..')
          BCFOK=.FALSE.
          BCFNAME='UNKNOWN'
        ELSE
          write(outs,'(3a)') 'Loading ',BCFName(1:lnblnk(BCFName)),
     &      '...done.'
          call usrmsg(outs,' ','P')
        ENDIF
      ENDIF

      IF (ACOName(1:7).ne.'UNKNOWN') THEN
        ACOOK=.TRUE.
        write(outs,'(3a)') 'Loading ',ACOName(1:lnblnk(ACOName)),
     &    '...'
        call usrmsg(outs,' ','-')
        call scnaco(1,IER)
        call scnzoneaco(TxtZone,1,IER)
        IF (IER .GT. 0) THEN 
          CALL EDISP(iuout,'Loading constructions not completed..')
          BCFOK=.FALSE.
          BCFNAME='UNKNOWN'
          GOTO 10
        ELSE
          write(outs,'(3a)') 'Loading ',BCFName(1:lnblnk(BCFName)),
     &      '...done.'
          call usrmsg(outs,' ','P')
        ENDIF
      ENDIF

C Main Menu
10    INO = -4
      write(ITEM(1),'(A,A22)')   'a Problem : ',LCFGF(1:22)
      write(ITEM(2),'(A,A25)')   '  Path : ',path(1:25)
      write(ITEM(3),'(A,A25)')   'b BCF  : ',BCFName(1:25)
      write(ITEM(4),'(A,A25)')   'c ACO  : ',ACOName(1:25)
      ITEM(5)=                 '  ---------- Zones --------------  '
      write(ITEM(6),'(A)')   'd Select zone(s) to include '
      if (TxtZone(1:6).eq.'UNKNOW') then
        write(ITEM(7),'(a)') '  People :  N/A'
      elseif (NbPub(1).EQ.0) then
        write(ITEM(7),'(A)') '  People : none'
      elseif (NbPub(2).GT.0) then
        write(ITEM(7),'(A)') '  People : Several types ...'
      else
        write(ITEM(7),'(A,I3,1x,A)') '  People : ',NbPub(1),
     &                               PubType(1)
      endif
      if (TxtZone(1:6).eq.'UNKNOW') then
        write(ITEM(8),'(a)') '  Furniture :  N/A'
      elseif (IndexFur(1).EQ.0) then
        write(ITEM(8),'(A)') '  Furniture : none'
      elseif (IndexFur(2).GT.0) then
        write(ITEM(8),'(A)') '  Furniture : Several types ...'
      else
        write(ITEM(8),'(A,F4.1,1x,A)') '  Furniture : ',XNbFur(1),
     &                               FurType(1)
      endif
      ITEM(9) =                ' ----------- Options ------------  '
      if(IOctave.eq.1)then
        ITEM(10)=             'e  Frequency band >> 1 octave     '
      elseif(IOctave.eq.2)then
        ITEM(10)=             'e  Frequency band >> 1/3 octave   '
      endif
      if(IDisp.eq.1)then
        if(IOctave.eq.1)then
          ITEM(11)=           'f  Display >> Numeric (1 lines)'
        elseif(IOctave.eq.2)then
          ITEM(11)=           'f  Display >> Numeric (3 lines)'
        endif
      elseif(IDisp.eq.2)then
        ITEM(11)=             'f  Display >> Numeric (1 line)'
      endif
      if(ISE.eq.1)then
        ITEM(12)=           'g  Output >> Only reverber. time'
      elseif(ISE.eq.2)then
        ITEM(12)=           'g  Output >> + Equivalent area '
      elseif(ISE.eq.3)then
        ITEM(12)=           'g  Output >> + Coeff. + Equiv. area'
      endif
      if(IAirAb.eq.1)then
        ITEM(13) =            'h  Air absorption >> included '
        if(IairProp.eq.0)then
          ITEM(14) =          'i  Air properties : Const. absorb.'         
        elseif(IairProp.eq.1)then
          ITEM(14) =          'i  Air properties : Highest absorb.'         
        elseif(IairProp.eq.2)then
          ITEM(14) =          'i  Air properties : Lowest absorb.'         
        endif
      elseif(IAirAb.eq.2)then 
        ITEM(13) =            'h  Air absorption >> Not included '
        ITEM(14) =            '                                  '         
      else
        ITEM(14) =            '                                  '         
      endif
      ITEM(15)=               ' --------- Calculation ----------  '
      write(ITEM(16),'(2A)')  'j  Method >> ',
     &                          CalcName(MethodRT)(1:12)
      if ((TxtZone.eq.'N/A').or.(TxtZone.eq.'UNKNOWN')) then
        write(ITEM(17),'(A)')  'g  Calculation N/A'
      else
        IF (BCFName.eq.'UNKNOWN') THEN
          write(ITEM(17),'(A)')'k  Start N/A'
        ELSE
          write(ITEM(17),'(A)')'k  Start '
        ENDIF
      endif
      ITEM(18) =              ' --------------------------------- '
      ITEM(19) =              'l Export results to file '
      ITEM(20) =              'm Clear textual feedback           '
      ITEM(21) =              '? Help                             '
      ITEM(22) =              '- Exit Aco                         '
      NITEMS = 22

C Use askabout to instanciate the initial help messages (2nd parameter is one).
      call askabout('aco ',1)

C Present the main menu.
      CALL EMENU('Reverberation time module',ITEM,NITEMS,INO)
      IF(INO.EQ.NITEMS)THEN

c End program Aco.
        call usrmsg(' Closing Aco desktop...',' ','P')
        CALL EPAGEND
        STOP
      elseif(INO.EQ.NITEMS-1)THEN

C Help via the askabout facility in esru_ask.F The zero requests display
        call askabout('aco ',0)
      elseif(INO.EQ.NITEMS-2) THEN
        call EPage
      elseif(INO .EQ. 16)THEN

C Calculation method selection
        MethodRT = MethodRT + 1
        if(MethodRT.eq.5)then
          MethodRT = 1
        endif
        goto 10
      elseif (INO.EQ.10) THEN

C Reporting frequency band selection
        IOctave  = IOctave + 1
        if (IOctave.eq.3) then
          IOctave = 1
        endif
        IF (IOctave.EQ.1) IDisp = 2
        goto 10
      elseif (INO.EQ.11) THEN

C Reporting level selection
        IDisp  = IDisp + 1
        if (IDisp.eq.3) then
          IDisp = 1
        endif
        goto 10
      elseif (INO.EQ.12) THEN

C Reporting output display selection
        ISE  = ISE + 1
        if (ISE.eq.4) then
          ISE = 1
        endif
        goto 10
      elseif (INO.EQ.13) THEN

C Air absorption
         helptopic='aco_air_absorption'
         call gethelptext(helpinsub,helptopic,nbhelp)
         CALL EASKAB('The air absorption','should be :','Inculded',
     &               'NOT included ',IairAb,nbhelp)
      elseif (INO.EQ.14) THEN

C Air properties
         helptopic='aco_air_properties'
         call gethelptext(helpinsub,helptopic,nbhelp)
         CALL EASKAB('The air absorption should be :','  ',
     &     'defined as constant ', 'extracted from res file ',
     &     IAirProp,nbhelp)
         IF (IAirProp.EQ.1) THEN
           IairProp=0
           helptopic='air_temp_for_absorb'
           call gethelptext(helpinsub,helptopic,nbhelp)
           CALL EASKR(RTair,' Give the ',' Air temperature [C]: ',0.,'
     &       W', 50.,'W',20.,'Constant air temperature in zone',
     &       IER,nbhelp)
           helptopic='air_rh_for_absorb'
           call gethelptext(helpinsub,helptopic,nbhelp)
           CALL EASKR(RHR,'Give the ','Air humidity [%] : ',
     &       0.,'F',100.,'F',50.,'Constant air humidity in zone',
     &       IER,nbhelp)
         ELSE

C Read the result file and determin the most and less air absorption 
C coefficient m
           helptopic='air_file_for_temps'
           call gethelptext(helpinsub,helptopic,nbhelp)
           TTmp=CLMRF
           CALL EASKS(TTmp,'Indoor climate file? ',
     &       '(see help for format)',72, ' ',
     &       'indoor climate file name',IER,nbhelp)
           CLMRF=TTmp
           if(CLMRF(1:2).EQ.'  ') then
             goto 10
           else
C If file exists, then open the file
             CALL EFOPSEQ(ICLMRF,CLMRF,1,IER)
             IF (IER.LT.0) then
               WRITE (outs,'(2a)')'Error while opening file: ',
     &           CLMRF(:lnblnk(CLMRF))
               CALL EDISP(iuout,Outs)
               IER=1
               goto 10
             ELSE
               FMax=0.
               FMin=1.
               TTMax=-100.
               TMin=100.
               RHMax=0.
               RHMin=100.
C When open, read its content
               WRITE (outs,'(4(a, 5x))')'Time step','Temp. [C]',
     &                                         ' RH [%]', 'M [m-1]'
               CALL EDISP(iuout,Outs)
  142          CALL STRIPC(ICLMRF,OUTSD,99,ND,1,'CLMRF',IER)
               if (OUTSD.ne.' ') then
                 K=0
                 CALL EGETWR(OUTSD,K,Time,0.,100000.,'W','Time',IER)
                 CALL EGETWR(OUTSD,K,CTair,-100.,100.,'W','Air T',IER)
                 CALL EGETWR(OUTSD,K,CHR,0.,100.,'W','Air RH',IER)
                 CurM = FactorM(CHR,CTair,4000)
                 IDay=INT(Time)
                 THR=Time
                 call STDATE(IYEAR,IDay,DESCR,DESCR1)
                 WRITE(outs,421)  Time, CTair, CHR, CurM
 421             FORMAT(1x,3(F6.2,5x),F7.5)
                 CALL EDISP(iuout,outs)
C Find the maximum absorption factor.
                 IF (CurM.GT.FMax) THEN
                   FMax=CurM
                   TTMax=CTair
                   RHMax=CHR
                 ENDIF
C Find the maximum absorption factor
                 IF (CurM.LT.FMin) THEN
                   FMin=CurM
                   TMin=CTair
                   RHMin=CHR
                 ENDIF
                 GOTO 142
               else
                 CALL ERPFREE(ICLMRF,ISTAT)
               endif
             ENDIF

             CALL EDISP(iuout,'The maximum air absorption is :')
             WRITE (outs,'(4(a, 5x))')'Time step','Temp. [C]',
     &         ' RH [%]', 'M [m-1]'
             CALL EDISP(iuout,Outs)
             write (outs,'(2a,3(F6.2,5x),F7.5)')DESCR(1:6),'@',
     &         THR,TTMax,RHMax,FMax 
             CALL EDISP(iuout,outs)
             CALL EDISP(iuout,' ')
             CALL EDISP(iuout,'The minimum air absorption is :')
             write (outs,'(2a,3(F6.2,5x),F7.5)')DESCR(1:6),'@',
     &         THR,TMin,RHMin,FMin 
             CALL EDISP(iuout,outs)
 
C Ask for which air condition the RT calculation should be perfromed
             helptopic='air_calc_high_or_low'
             call gethelptext(helpinsub,helptopic,nbhelp)
             CALL EASKAB(' Do you want to calculate the RT with :',' ',
     &        'the HIGHEST air absoprtion','the LOWEST air absoprtion',
     &         IairProp,nbhelp)
             IF (IairProp.EQ.1) THEN
               RTair=TTMax
               RHR=RHMax
             ELSE
               RTair=TMin
               RHR=RHMin
            ENDIF
           endif
        ENDIF
      elseif (INO.EQ.19) THEN
  
C Export results to file
C First open the file, then write in and finally close the file
        helptopic='export_aco_predictions'
        call gethelptext(helpinsub,helptopic,nbhelp)
        itu = ixunit
        call ctlexp(xfile,ixopen,ixloc,ixunit,'T',' text',IER)
        write(outs,'(a)') 'Reverberation time calculation report.'
        call edisp(itu,outs)
        write(outs,'(3a)') 'BCF file: ',path(1:lnblnk(path)), BCFName
        call edisp(itu,outs)
        write(outs,'(3a)') 'Aco file: ',path(1:lnblnk(path)), ACOName
        call edisp(itu,outs)
        call dstamp(dstmp)
        write(outs,'(2a)') 'Date: ',dstmp
        call edisp(itu,outs)
        write(outs,'(2a)') 'Zone: ',TxtZone
        call edisp(itu,outs)
        call DispTRev(IDisp,IOctave,ISE,MethodRT,IAirAb,itu)
        call ctlexp(xfile,ixopen,ixloc,ixunit,'T',' text',IER)        
        goto 10
      elseif (INO.eq.1) then 

C Select/change ESP-r configuration file.
        helptopic='model_cfg_file_name'
        call gethelptext(helpinsub,helptopic,nbhelp)
        TTmp=LCFGF
  289   CALL EASKS(TTmp,'Model configuration file name? ',' ',72,DFCFG,
     &    'config file name',IER,nbhelp)
        LCFGF=TTmp
        IF(LCFGF(1:2).EQ.'  ')GOTO 289
        write(LCMDFL,'(a)') TTmp(1:lnblnk(TTMP))
        goto 42
      elseif (INO.eq.3) then 

C Select/change BCF file.
        helptopic='model_bcf_file_name'
        call gethelptext(helpinsub,helptopic,nbhelp)
  288   IF (BCFOK) THEN
          call easkabc('A BCF of the current problem is loaded.',
     &      'Do you want: ','use it','load another','continue',
     &      Iw,nbhelp)
          if(iw.eq.3)then
            goto 10
          elseif(iw.eq.2)then
            TTmp=BCFName
            CALL EASKS(TTmp,'New building constructions file ?',
     &        ' ',72, DBCFNAM,'BCF file name',IER,nbhelp)
            BCFName=TTmp
            IF(BCFName(1:2).EQ.'  ')GOTO 288
          endif
        ELSE
          if (CFGOK) then
            TTmp=BCFName
            CALL EASKS(TTmp,'Building constructions file?',' ',72,
     &      DBCFNAM,'BCF file name',IER,nbhelp)
            BCFName=TTmp
            IF(BCFName(1:2).EQ.'  ')GOTO 288
          else
            call edisp(iuout,' No project has been loaded.')
            goto 10
          endif
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
            CALL EDISP(iuout,'Loading constructions not completed..')
            BCFOK=.FALSE.
            BCFNAME='UNKNOWN'
            GOTO 10
          ELSE
            write(outs,'(3a)') 'Loading ',BCFName(1:lnblnk(BCFName)),
     &        '...done.'
            call usrmsg(outs,' ','P')
          ENDIF
        else
          CALL usrmsg('File doesn`t exist...','Select again','W')
          GOTO 288
        endif
        GOTO 10
      elseif (INO.eq.4) then 

C Attempt to read Aco (occupants and furniture file).
        helptopic='include_occupants_dialog'
        call gethelptext(helpinsub,helptopic,nbhelp)

  290   IF (ACOOK) THEN
          call easkabc('A ACO file is loaded.','Do you want:',
     &      'to use it','load another','continue',Iw,nbhelp)
          if(iw.eq.3)then
            goto 10
          elseif(iw.eq.2)then
            TTmp=AcoName
            CALL EASKS(TTmp,'New ACO (Occupnats/Furniture) file ?',
     &        ' ',72, DACONam,'ACO file name',IER,nbhelp)
            AcoName=TTmp
            IF(ACOName(1:2).EQ.'  ') GOTO 10
          endif
        ELSE
          if (CFGOK) then
            TTmp=AcoName
            CALL EASKS(TTmp,' ACO (Occupnats/Furniture) file?',
     &        ' ',72,DACONam,'ACO file name',IER,nbhelp)
            ACOName=TTmp
            IF(ACOName.EQ.' ')GOTO 290
          else
            call edisp(iuout,' No ACO file has been loaded.')
            goto 10
          endif
        ENDIF

C Attempt to read in the BCF file.
        XST=.false.
        INQUIRE (FILE=ACOName,EXIST=XST)
        if(XST)then
          ACOOK=.TRUE.
          write(outs,'(3a)') 'Loading ',ACOName(1:lnblnk(ACOName)),
     &      '...'
          call usrmsg(outs,' ','-')
          call scnaco(1,IER)
          call scnzoneaco(TxtZone,1,IER)

          IF (IER .GT. 0) THEN 
            CALL EDISP(iuout,'Loading constructions not completed..')
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
          GOTO 290
        ENDIF
        GOTO 10
      elseif (INO.eq.6) then 

C Display Zones list if project loaded and wait for INIPIC item selected.
        IF (CFGOK) THEN
          CALL Zoneslist
          IF (izgfoc.EQ.0) THEN
            call edisp(iuout,' No zone has been selected.')
            GOTO 10
          ELSE
            Irep=0
            do 41 ixd=1,NZG
              ic=NZNOG(ixd)
              if(Irep.ne.3)then
                write(outs,'(3a)')
     &            'Do you want to see surface details in ',
     &            zname(NZNOG(ixd)),'?'
                helptopic='display_details_dialog'
                call gethelptext(helpinsub,helptopic,nbhelp)
                CALL EASKABC(outs,'  ',' Yes ',' No ',
     &            ' No for all ',Irep,nbhelp)
                if(Irep.eq.1)CALL ZoneDisp(ic)
              endif
              write(outs,'(3a)')' Zone ',zname(NZNOG(ixd)),
     &                          ' has been successfully selected.'
              call edisp(iuout,outs)
  41        continue
          ENDIF
            
C update menu text and public/furniture display
          call znlist(outsd,len,ierr) 
          Length = MIN0(len,100)
          TxtZone=outsd(1:Length)
          call scnzoneaco(TxtZone,0,IER)
        ELSE
          call edisp(iuout,' No project has been loaded.')        
        ENDIF
        GOTO 10
      elseif (INO.eq.7) then 

C Display public list if zone contains more than one public type
        IF (ACOOK) THEN
          call scnzoneaco(TxtZone,2,IER)
        ENDIF
        GOTO 10
      elseif (INO.eq.8) then 

C Display furniture list if project loaded and wait for INIPIC item selected.
        IF (ACOOK) THEN
          call scnzoneaco(TxtZone,3,IER)
        ENDIF
        GOTO 10
      ELSEIF (INO.EQ.17) THEN

C Test if input Ok before calculation and
C start reverberation time calculation and display results
        IF ((izgfoc .GT. 0).AND.(BCFOk)) THEN
          write(outs,'(2a)')' Reverberation time for: ', TxTZone
          call edisp(iuout,outs)
          call scnaco(0,IER)
          call scnzoneaco(TxtZone,0,IER)
          call DispTRev(IDisp,IOctave,ISE,MethodRT,IAirAb, iuout)
        ELSE
          IF (.NOT. BCFOk) THEN
            call edisp(iuout,' No selected BCF. Please, try again.')
          ELSEIF (izgfoc .EQ. 0) THEN
            call edisp(iuout,' No selected zone(s). Please, try again.')
          ENDIF    
          GOTO 10
        ENDIF

C End of menu code.
      ENDIF

C Return to main menu.
      GOTO 10
      END

C ************* imgdisp *************
C Dummy routine (needed for call back from c).
C keep it (SC)
      subroutine imgdisp(iforce,focus,ier)

      character focus*4
     
      return
      end

C Dummy subroutine needed to compile (called from library code).
      SUBROUTINE PLELEV(direc)
      CHARACTER direc*1
      return
      end



