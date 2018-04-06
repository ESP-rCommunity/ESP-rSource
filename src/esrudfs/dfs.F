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

C This is the stand-alone CFD module. It is able to simulate air flows
C inside rectangular or cubic zones of buildings. It employs the finite
C volume method in order to discretize the equations.

C ********************* DFS *********************
C DFS - Stand-alone CFD module.
C Passed iappw, iappx, iappy as pixel width of application, left and top offsets.
C if these are 0 then use default values.
      program dfs
      USE START_UP
#include "cfd.h"
#include "building.h"
#include "model.h"
#include "geometry.h"
#include "esprdbfile.h"
#include "material.h"
#include "espriou.h"
#include "prj3dv.h"

      integer lnblnk  ! function definition

      COMMON/OUTIN/IUOUT,IUIN
      COMMON/FILEP/IFIL
      COMMON/MFTRA/IMFTU
      COMMON/SHOUT/ICOUT
      COMMON/SPAD/MMOD,LIMIT,LIMTTY
      integer menuchw,igl,igr,igt,igb,igw,igwh
      COMMON/VIEWPX/menuchw,igl,igr,igt,igb,igw,igwh
      common/appcols/mdispl,nifgrey,ncset,ngset,nzonec
      common/appw/iappw,iappx,iappy
      integer childterminal  ! picks up mmod from starting of prj
      common/childt/childterminal
      integer ifs,itfs,imfs
      COMMON/GFONT/IFS,ITFS,IMFS
      COMMON/ALL/NI,NJ,NK,NIM1,NJM1,NKM1,NIM2,NJM2,NKM2
      common/INCALC/INCALU,INCALV,INCALW,INCALK,INCALD,INCALT,
     1              IZEROT,IZanKE,IMITZ

C Path to problem.
      common/rpath/path

C Defaults.
      COMMON/DEFLT3/DFCFD,DECMPDBFL,DICONDBFL

C CFD common blocks.
      common/cfdfil/LCFD(MCOM),IFCFD(MCOM)
      common/param2/TITLE(MNZ),CFTRFL(MNZ),LPHI(MNZ)
      COMMON/SIMT/TSTEP,NSINC
      COMMON/ICFNOD/ICFD,ICP
      COMMON/ICFCHN/ICFMON(MNZ),ICFTMP,ICFLIB

C Flow visualization common blocks.
      COMMON/VISFIL/LVEC,LBIT
      common/flvpar/Lflpar(MCOM)
      common/flvimg/imgtyp,IMOPTS

C UTIL common blocks.
      common/utlcom/photon,matlab,textout,flwvis,xslice,yslice,zslice
      common/slicom/islice,jslice,kslice

C Lisolv selector.
      common/SOLVER/ILISOL
      common/METHDS/ITURB(MNZ),IBUOY(MNZ)
      COMMON/GGDH/ GENB1(ntcelx,ntcely,ntcelz),GGDH

C Small openings.
      common/SSOinit/areaSSO

C Common precz when conflated should be cleared in stand alone mode.

C Significant figure reporting limit (NSIGFIG).
      common/SFIG/NSIGFIG

C Name of current application
      common/APPNAME/cAppName
      character cAppName*12

C For number of openings NOPEN
      COMMON/NDMAP/NOPEN(MNZ),MFNODE(MCFND,MNZ),IOPENi(MCFND,MNZ),
     &             IOPENf(MCFND,MNZ),JOPENi(MCFND,MNZ),
     &             JOPENf(MCFND,MNZ),KOPENi(MCFND,MNZ),
     &             KOPENf(MCFND,MNZ),FIXM(MCFND,MNZ),
     &             FIXT(MCFND,MNZ),FIXC(MCFND,MNZ),
     &             FIXK(MCFND,MNZ),FIXE(MCFND,MNZ),
     &             IWOPEN(MCFND,MNZ),ICFDCN(MCFND,MNZ),
     &             ICNACT(MCFND,MNZ),IVOLN(MCFND,MNZ)

C Saved state of the visualisation
      COMMON/CFDVIS/ISHSB,ISHAO,IFACES,ISHBLK

      CHARACTER*24 ITEM(11)
      CHARACTER inf*144,LCFD*72,DFCFD*72,Lflpar*72
      character path*72,longtfile*144,ETEXT*82
      CHARACTER doit*248,tmode*8,outs*124,fs*1,outstr*124
      CHARACTER*72 TITLE,CFTRFL,LPHI,RESFL,WORD
      integer islice,jslice,kslice
      CHARACTER*72 LVEC,LBIT,t72,vecfil
      character*72 DECMPDBFL,DICONDBFL

      character cVnum*16      ! returned from ESPrVersionNum
      character pagestitle*42 ! for banner title via epages call

      LOGICAL INCALU,INCALV,INCALW,INCALK,INCALD,INCALT
      LOGICAL IZEROT,IZanKE,IMITZ
      logical photon,matlab,textout,flwvis,xslice,yslice,zslice
      LOGICAL XST,LAST,concat,INPOK,unixok
      LOGICAL SIMOK,OK,dok,GGDH
      LOGICAL areaSSO

      integer iglib   ! if 1 then X11, if 2 then GTK, if 3 then text only.
      integer NITMS,INO,INOt,INO1,INO2  ! max items and current menu item
      integer itplots
      integer ISHSB,ISHAO,IFACES

C For help messages
      character helpinsub*24 ! subroutine name
      character helptopic*24 ! string (unique) for topic
      integer nbhelp     ! number of help lines found (will be non-zero
                         ! because default message will be created

#ifdef OSI
      integer iigl,iigr,iigt,iigb,iigw,iigwh
      integer iiw1,iiw2,iiw3,iiw4,iimenu
      integer iicapture,iiazi    ! to pass to updcapt updazi
      integer iside,isize,ifont

#else
      integer*8 iigl,iigr,iigt,iigb,iigw,iigwh
      integer*8 iiw1,iiw2,iiw3,iiw4,iimenu
      integer*8 iicapture,iiazi    ! to pass to updcapt updazi
      integer*8 iside,isize,ifont
#endif

C Clear 'conflated only' commons.
      do 2 I=1,MCOM
        zname(I)=' '
        lnzname(I)=0
        zdesc(I)=' '
        lnzdesc(I)=0
 2    continue

C Set defaults.
      GGDH=.false.

C defaults for visualisation
      ISHSB=1
      ISHAO=1
      IFACES=1

C Clear all arrays.
      call ezero
      call curmodule('dfs ')
      cAppName = 'dfs'
      helpinapp='dfs'  ! set once for the application
      helpinsub='dfs'  ! set for MAIN

C Initialization for matlab.
      islice=2
      jslice=2
      kslice=2
      xslice=.FALSE.
      yslice=.FALSE.
      zslice=.FALSE.

C Initialise strings.
      LVEC='UNKNOWN'
      LBIT='UNKNOWN'

C Default image format is 2d screen plot.
      imgtyp=0
      IMOPTS=1
      do 5 I=1,MCOM
        Lflpar(I)='UNKNOWN'
 5    continue

C Default SIMOK: simulation failed.
      SIMOK=.FALSE.

C CFD component number and building/mass flow component.
      ICFD=1
      ICP=1

C File units.
      IUOUT=6
      IUIN=5
      ICOUT=0
      IFIL=10
      LIMTTY=9
      LIMIT =9
      NSIGFIG=3
      IFL=IFIL+1
      ICFLIB=IFIL+2
      ICFMON=IFIL+3
      matver=0.0   ! initial assumption of binary materials database

C Temporary cfd file unit.
      ICFTMP=IFIL+4

      IFS=1; ITFS=1; IMFS=1
      path='./'
      call parpsf(MODL,iappw,iappx,iappy,inf)
      childterminal=MODL  ! remember so that child processes can run the same

C Set folder separator (fs) to \ or / as required.
      call isunix(unixok)
      if(unixok)then
        fs = char(47)
      else
        fs = char(92)
      endif
      write(path,'(a1,a1)')'.',fs

C Building and Mass Flow models are not participating.
      IBLD=0
      IMFS=0

C CFD component number and building/mass flow component.
      ICFD=1
      ICP=1

      TIMAX=86400.0
      TSTEP=3600.0

C Flag indicating that CFD `small opening' areas have not yet been calculated.
      areaSSO = .FALSE.

C Initialise coordinates for eye point, view point and angle of view.
      EYEM(1)=-100.; EYEM(2)=-100.; EYEM(3)=100.
      VIEWM(1)=10.; VIEWM(2)=10.; VIEWM(3)=10.
      ANG=40.

C General image option flags.
      ITDSP=0; ITBND=1; ITEPT=0
      ITZNM=0; ITSNM=1; ITVNO=1
      ITORG=0; ITSNR=1; ITOBS=0
      ITGRD=0; GRDIS=0.0
      ITPPSW=0; IFS=1; ITFS=1

C Initial choice for ascii export file.
      photon=.FALSE.
      matlab=.FALSE.
      textout=.FALSE.
      flwvis=.TRUE.

C Default lisolv.
      ILISOL=3

C Determine terminal type and set write unit to stderr for rule scripts.
      MMOD=MODL
      if(iappw.eq.0.and.iappx.eq.0.and.iappy.eq.0)then
        iappw=550
        iappx=90
        iappy=50
      else
        if(iappx.le.0)iappx=90
        if(iappy.le.0)iappy=50
        if(iappw.le.200)then
          iappwi=int(550*iappw*0.01)
          iappw=iappwi
        elseif(iappw.gt.200)then
          continue
        endif
      endif

C Set pixels high to iappw and pixels wide to factor in monitor size.
      iapphi=iappw
      iappwi=int(real(iappw)*(1024.0/780.0))

      if(iappw.gt.0.and.iappw.lt.100)then
        menuchw = MAX0(int(27*iappw*0.01),10)
        LIMTTY=8
        LIMIT =8
      else
        menuchw = 27
        LIMTTY=8
        LIMIT =8
      endif
      IF(MMOD.EQ.8)THEN

C Set initial font sizes (IMFS is for graphs, IFS is for dialog & text feedback).
        IMFS=1; IFS=1; ITFS=1
        call userfonts(IFS,ITFS,IMFS)
      ELSE
        LIMTTY=16
        LIMIT =16
      ENDIF

C Find the current ESP-r version number and add it to application title.
      call ESPrVersionNum(cVnum)
      write(pagestitle,'(2a)') 'ESP-r CFD Simulator ',
     &  cVnum(1:lnblnk(cVnum))

      CALL EPAGES(MMOD,IUIN,IUOUT,iappwi,iapphi,iappx,iappy,menuchw,
     &  pagestitle)

C Open the text display box equal to LIMTTY if MMOD = 8.
      IF(MMOD.EQ.8)THEN

C Setup and pass in parameters to win3d.
        iiw1=10; iiw2=12; iiw3=4; iiw4=4; iimenu=menuchw
        iigl=igl; iigr=igr; iigt=igt; iigb=igb; iigw=igw; iigwh=igwh
        CALL win3d(iimenu,iiw1,iiw2,iiw3,iiw4,
     &    iigl,iigr,iigt,iigb,iigw,iigwh)
        igl=int(iigl); igr=int(iigr); igt=int(iigt); igb=int(iigb)
        igw=int(iigw); igwh=int(iigwh)
        iglib = igraphiclib()  ! find out if X11 or GTK or text support only.
        if(iglib.eq.1)then
          call opencpw
          call opensetup
          iicapture=1; iiazi=1
          call updcapt(iicapture)
          call updazi(iiazi)
        endif
        call setgscale()
        call setcscale()
        mdispl=0; nifgrey=0; ncset=0; ngset=0; nzonec=0
        call foundcolour(mdispl,nifgrey,ncset,ngset,nzonec)
        call startbuffer()
      ENDIF

C << set trace on for debugging purposes >>
      itrc=1

C Set additional output units to stdout. Then redirect warning
C messages to stderr in case of rule script program control.
      IMFTU=IUOUT
      ICOUT=IUOUT
      IF(MMOD.EQ.-6) ICOUT=0

C Show version.
      CALL ESPrVersion("summary",cAppName,IUOUT)

C Find the user's home folder then get user's custom settings.
      call usrhome(upath)
      if(unixok)then
        write(esprc,'(3a)') upath(1:lnblnk(upath)),fs,'.esprc'
      else
        write(esprc,'(3a)') upath(1:lnblnk(upath)),fs,'esprc'
      endif
      call scesprc(esprc,IFL,0,IIER)

C Scan the defaults file for default configuration.
C Make temporary use of file unit IFL.
      call escdef(IFL,'-',IER)

C Take `c' command line file name, convert to fortran string LCFD
C and place as initial configuration file.
C << decide whether LCFD needs to be > 72 char >>
      if(inf(1:2).ne.'  '.and.inf(1:4).ne.'UNKN')then
        llt=lnblnk(inf)
        if(llt.le.72)then
          write(LCFD(1),'(a)') inf(1:lnblnk(inf))
        else
          write(LCFD(1),'(a)') inf(1:72)
        endif
        write(outs,'(a,a)')' the input file is: ',LCFD(1)
        call edisp(iuout,outs)
      else
        LCFD(1)='UNKNOWN'
      endif

C If an input file has been specified then load it and then
C return to the main menu.
      XST=.FALSE.
      INQUIRE (FILE=LCFD(1),EXIST=XST)
      IF(XST)THEN
C Set currentfile
        write(currentfile,'(a)')LCFD(1)(1:LNBLNK(LCFD(1)))

        CALL EFOPSEQ(IFL,LCFD(1),1,IER)

C Only one zone possible so set IZ to 1.
        IZ=1
        CALL STRIPC(IFL,OUTSTR,0,ND,1,'dfd line 1',IER)
        if(OUTSTR(1:7).eq.'*DFS V2')then
          CALL ERPFREE(IFL,ISTAT)
          call DFDREAD(IZ,0,iuout,IER)
        elseif(OUTSTR(1:15).eq.'DFS DESCRIPTION')then
          CALL ERPFREE(IFL,ISTAT)
          CALL EFOPSEQ(IFL,LCFD(1),1,IER)
          CALL CFDDTA(itrc,iuout,IZ,IBLD,IMFS,IER)
          CALL ERPFREE(IFL,ISTAT)
        ENDIF
      ENDIF
      IF((XST).AND.IER.EQ.0)THEN
        INPOK=.TRUE.

C plot grid.
        CALL INICNT
        CALL GRID(ier)
        call VGRID3D('---','xyz',iz)
        call VSB3D(0,iz)
        call VAO3D(0,iz)
      ELSE
        INPOK=.FALSE.
      ENDIF

C Initialise gridding parameters
      CALL INICNT
      CALL GRID(ier)

C Let the user pick a menu item.
   10 INO=-4
      ITEM(1) ='a Problem definition '
      ITEM(2) ='b Grid visualisation '
      ITEM(3) =' ____________________'
      ITEM(4) ='c Simulation toggles '
      ITEM(5) ='d Initiate simulation'
      ITEM(6) =' ____________________'
      ITEM(7) ='e Results analysis   '
      ITEM(8) =' ____________________'
      ITEM(9) ='f Model details      '
      ITEM(10) ='? Help '
      ITEM(11)='- Quit '
      NITMS=11

C Use askabout to instanciate the initial help messages (2nd parameter is one).
      call askabout('dfs ',1)

C Plot grid
      IF(MMOD.EQ.8)THEN
        if(abs(IFACES).eq.1)then
          call VGRID3D('---','xyz',iz)
        elseif(abs(IFACES).eq.2)then
          call VGRID3D('---','xzg',iz)
        elseif(abs(IFACES).eq.3)then
          call VGRID3D('---','yzg',iz)
        elseif(abs(IFACES).eq.4)then
          call VGRID3D('---','xyg',iz)
        endif
        if (ISHSB.eq.1) then
          call VSB3D(0,IZ)
        endif
        if (ISHAO.eq.1) then
          do 765 iaop=1,NOPEN(IZ)
            call VAO3D(iaop,iz)
  765     continue
c Comment the above 3 lines and un-comment the line below
c to remove bold lines around openings
c        call VAO3D(0,iz))        
        endif
      
        WRITE(etext,'(2A)')'Model: ',
     &    TITLE(IZ)(1:lnblnk(TITLE(IZ)))
        iside=1
        isize=1
        ifont=1
        call viewtext(etext,iside,isize,ifont)
      ENDIF

      CALL EMENU(' dfs',ITEM,NITMS,INO)
      IF(INO.EQ.1)THEN

C change configuration file name
C << Note: array index 1 is hard coded below! >>
        helptopic='dfs_config_file_name'
        call gethelptext(helpinsub,helptopic,nbhelp)
  289   CALL EASKF(LCFD(1),'Domain flow definition file name?','  ',
     &    72,DFCFD,'Domain flow definition file',IER,nbhelp)
        call usrmsg('File selected is',LCFD(1),'-')
        IF(LCFD(1).EQ.'  ')GOTO 289
        CALL ERPFREE(IFL,ISTAT)
        INQUIRE (FILE=LCFD(1),EXIST=XST)
        IF(.NOT.XST)THEN
          CALL EDISP(IUOUT,'  ')
          CALL EDISP(IUOUT,'Domain flow definition file not found. ')
          GOTO 289
        else
          write(currentfile,'(a)')LCFD(1)(1:LNBLNK(LCFD(1)))
          CALL EFOPSEQ(IFL,LCFD(1),1,IER)

C Only one zone possible so set IZ to 1.
          IZ=1
          CALL STRIPC(IFL,OUTSTR,0,ND,1,'dfd line 1',IER)
          if(OUTSTR(1:7).eq.'*DFS V2')then
            CALL ERPFREE(IFL,ISTAT)
            call DFDREAD(IZ,0,iuout,IER)
          elseif(OUTSTR(1:15).eq.'DFS DESCRIPTION')then
            CALL ERPFREE(IFL,ISTAT)
            CALL EFOPSEQ(IFL,LCFD(1),1,IER)
            CALL CFDDTA(itrc,iuout,IZ,IBLD,IMFS,IER)
            CALL ERPFREE(IFL,ISTAT)
          ENDIF
        ENDIF
        IF(IER.EQ.0)THEN
          INPOK=.TRUE.
        ELSE
          INPOK=.FALSE.
        ENDIF
      ELSEIF(INO.EQ.2)THEN

C Allow examination of grid.
        call cgd(1,IER)
      ELSEIF(INO.EQ.4)THEN

C Set simulation toggles.
        IF(.NOT.INPOK)then
          CALL EDISP(IUOUT,'  ')
          CALL EDISP(IUOUT,' Domain flow definition file not defined.')
          goto 10
        ENDIF
 62     INOt=-5
        if (ILISOL.eq.3) then
          ITEM(1) ='a Solver>> default'
        elseif (ILISOL.eq.4) then
          ITEM(1) ='a Solver>> bi-c-grad'
        endif
        if (.not.GGDH) then
          ITEM(2) ='b Buoy/Turb>> SingleGDH'
        else
          ITEM(2) ='b Buoy/Turb>> GeneralGDH'
        endif
        ITEM(3) =' ___________________'
        ITEM(4) ='? help'
        ITEM(5) ='- Exit'
        NITMS=5

C Help text for this sub-menu.
        helptopic='dfs_simulation_toggles'
        call gethelptext(helpinsub,helptopic,nbhelp)
        CALL EMENU('Simulation toggles',ITEM,NITMS,INOt)
        IF(INOt.EQ.1)THEN
          ILISOL=ILISOL+1
          if (ILISOL.gt.4) ILISOL=3
        ELSEIF(INOt.EQ.2)THEN
          if (GGDH) then
            GGDH=.false.
          else
            GGDH=.true.
          endif
          if (ITURB(ICFD).ne.1.or.IBUOY(ICFD).eq.0) GGDH=.false.
        ELSEIF(INOt.EQ.4)THEN

C HELP
          helptopic='dfs_simulation_toggles'
          call gethelptext(helpinsub,helptopic,nbhelp)
          CALL PHELPD('dfs simul toggles',nbhelp,'-',0,0,IER)
        ELSEIF(INOt.EQ.5)THEN
          goto 10
        endif
        goto 62
      ELSEIF(INO.EQ.5)THEN

C Invoke simulation.
        IF(.NOT.INPOK)then
          CALL EDISP(IUOUT,'  ')
          CALL EDISP(IUOUT,'Domain flow definition file not defined.')
          goto 10
        ENDIF

C Re-set graphic feedback.
        if (MMOD.EQ.8) then

C Setup and pass in parameters to win3d.
          iiw1=10; iiw2=8; iiw3=5; iiw4=3; iimenu=menuchw
          iigl=igl; iigr=igr; iigt=igt; iigb=igb; iigw=igw; iigwh=igwh
          CALL win3d(iimenu,iiw1,iiw2,iiw3,iiw4,
     &      iigl,iigr,iigt,iigb,iigw,iigwh)
          igl=int(iigl); igr=int(iigr); igt=int(iigt); igb=int(iigb)
          igw=int(iigw); igwh=int(iigwh)
          call startbuffer()
        endif

C Asking user about steady or transient is confusing as the
C transient option is the same thing over and over. Hide this
C option as it might only be of interest to experts.
        TIMAX=1.0E20
        TSTEP=1.0E20

C        CALL EASKAB('  ','What kind of simulation is?',
C     &    'Steady','Transient',IW,nbhelp)
C        IF(IW.EQ.1)then
C          TIMAX=1.0E20
C          TSTEP=1.0E20
C          GOTO 60
C        ENDIF
C   50   V=TIMAX/3600.0
C        CALL EASKR(V,'  ','Simulation period (h) ? ',
C     &    1E-5,'F',0.0,'-',24.0,'Simulation period',IER,nbhelp)
C        IF(IER.NE.0) GOTO 50
C        TIMAX=V*3600.0

C   51   V=TSTEP/3600.0
C        CALL EASKR(V,'  ','Simulation time-step (h) ? ',
C     &    1E-6,'F',TIMAX,'F',1.0,'Simulation time-step',IER,nbhelp)
C        IF(IER.NE.0) GOTO 51
C        TSTEP=V*3600.0

   60   NSINC=0
        TIME=0.0
        LAST=.FALSE.
        TIM1=TSTEP

 100    NSINC=NSINC+1
        CALL CFMNGE(ICP,ICFD)

C Simulation termination checks.
C 1) Error generated.
        IF (IER.NE.0) then
          SIMOK=.FALSE.
          call usrmsg('Solution failed.','  ','W')
          goto 10
        endif

C 2) Final timestep in transient simulation reached.
        IF (LAST) then
          SIMOK=.TRUE.
          call usrmsg('Simulation complete.','  ','W')
          goto 10
        endif

C 3) Timestep longer than simulation time (i.e. steady state).
        IF (TSTEP.GE.TIMAX) then
          SIMOK=.TRUE.
          call usrmsg('Simulation complete.','  ','W')
          goto 10
        endif

C Increment time and simulate next timestep.
        TIM1=TIM1+TSTEP
        IF(TIM1.GE.TIMAX)then
          TSTEP=TIMAX-TIME
          TIME=TIMAX
          LAST=.TRUE.
        ELSE
          TIME=TIM1
        ENDIF
        goto 100

      ELSEIF(INO.EQ.7)THEN

C Check for model (set bounds on domain for visualisation).
        IF(.NOT.INPOK)then
          CALL EDISP(IUOUT,'  ')
          CALL EDISP(IUOUT,' Domain flow definition file not defined.')
          goto 10
        ENDIF
   20   INO1=-5
        ITEM(1) ='a ASCII format file '
        ITEM(2) ='b PHOTON analysis   '
        ITEM(3) ='c MATLAB analysis   '
        ITEM(4) ='d ParaView format file'
        ITEM(5) ='e TECPLOT format file'
        ITEM(6) ='f Flow visualization '
        ITEM(7) ='- Exit               '
        NITMS=7

C Help text for results sub-menu.
        helptopic='dfs_results_menu'
        call gethelptext(helpinsub,helptopic,nbhelp)
        CALL EMENU(' results',ITEM,NITMS,INO1)
        IF(INO1.EQ.1)THEN

C The following will drive xten to view the ASCII output file.
C This should be make more generic later, by using the text
C editor preference specified in the `dot' file.
C Execute xten.
C Get logical name of terminal type, expand problem name
C to include the path and create a string to drive xten.
  389     CALL EASKS(RESFL,'Result file name for DFS predictions?',
     &      '  ',72,LPHI(1),' Result file',IER,nbhelp)
          IF(RESFL.EQ.'  ')GOTO 389
          INQUIRE (FILE=RESFL,EXIST=XST)
          IF(.NOT.XST)THEN
            CALL EDISP(IUOUT,'  ')
            CALL EDISP(IUOUT,' Result file not found.')
            goto 20
          ELSE
            doit = '  '
            call terminalmode(childterminal,tmode)
            call addpath(RESFL,longtfile,concat)
            write(doit,'(3a)') 'xten ',longtfile(:lnblnk(longtfile)),
     &        ' & '
            call runit(doit,tmode)
            GOTO 10
          ENDIF
        ELSEIF(INO1.EQ.2)THEN
          helptopic='dfs_results_menu'
          call gethelptext(helpinsub,helptopic,nbhelp)
          CALL PHELPD('PHOTON file',nbhelp,'-',0,0,IER)

C Execute PHOTON.
C Get logical name of terminal type, expand problem name
C to include the path and create a string to drive PHOTON.
          doit = '  '
          call terminalmode(childterminal,tmode)
          call addpath(RESFL,longtfile,concat)
          write(doit,'(3a)') 'runpho ',
     &      longtfile(:lnblnk(longtfile)),' & '
          INQUIRE (FILE='runpho',EXIST=XST)
          IF(XST)THEN
            call runit(doit,tmode)
          ELSE
            helptopic='dfs_results_menu'
            call gethelptext(helpinsub,helptopic,nbhelp)
            CALL PHELPD('PHOTON analysis',nbhelp,'-',0,0,IER)
          ENDIF
        ELSEIF(INO1.EQ.3)THEN

C Matlab-begin.
  70      IF (incalu.and.incalv.and.incalw)then

C Let the user pick a menu item.
            INO2=-4
            xslice=.FALSE.
            yslice=.FALSE.
            zslice=.FALSE.
            ITEM(1) ='a Slice in x-direction'
            ITEM(2) ='b Slice in y-direction'
            ITEM(3) ='c Slice in z-direction'
            ITEM(4) ='- Exit                '
            NITMS=4
  72        CALL EMENU(' Matlab',ITEM,NITMS,INO2)
            IF(INO2.EQ.0)THEN

C Wrong pick.
              INO2=-1
              GOTO 72
            ELSEIF(INO2.EQ.1)THEN
  74          CALL EASKI(islice,'  ','Enter X slice number ',
     &          2,'F',nim1,'F',2,' Slice number',IER,nbhelp)
              IF(IER.NE.0) GOTO 74
              xslice=.TRUE.
            ELSEIF(INO2.EQ.2)THEN
  76          CALL EASKI(jslice,'  ','Enter Y slice number ',
     &          2,'F',njm1,'F',2,' Slice number',IER,nbhelp)
              IF(IER.NE.0) GOTO 76
              yslice=.TRUE.
            ELSEIF(INO2.EQ.3)THEN
  78          CALL EASKI(kslice,'  ','Enter Z slice number ',
     &          2,'F',nkm1,'F',2,' Slice number',IER,nbhelp)
              IF(IER.NE.0) GOTO 78
              zslice=.TRUE.
            ELSEIF(INO2.EQ.4)THEN
              GOTO 20
            ENDIF
          ENDIF

          CALL PHELPD('MATLAB file',nbhelp,'-',0,0,IER)

C Assemble matlab file 'startup.m'.
          ifl = ICFLIB
          CALL EGETW(LPHI(ICFD),K,WORD,'W','  ',IER)
          WORD='startup.m'
          call efopseq(ifl,WORD,4,ier)
          call prnmat(ifl)
          CALL ERPFREE(IFL,ISTAT)

C Execute MATLAB with startup.m file (the MATLAB default).
          doit = '  '
          call terminalmode(childterminal,tmode)
          call addpath(RESFL,longtfile,concat)
          write(doit,'(3a)') '/usr/esru/bin/matlab ',
     &      longtfile(:lnblnk(longtfile)),' & '
          INQUIRE (FILE='/usr/esru/bin/matlab',EXIST=XST)
          IF(XST)THEN

C Graphs are defined in cfmatl.F.
            call runit(doit,tmode)
            if (incalu.and.incalv.and.incalw)goto 70
          ELSE
            CALL PHELPD('MATLAB analysis',nbhelp,'-',0,0,IER)
          ENDIF
        ELSEIF(INO1.EQ.4)THEN

C Write ParaView file for each timestep.
C For stand-alone: only one file is written (current timestep)!
          call PARAVIEWEND(1)
        ELSEIF(INO1.EQ.5)THEN

C Write TecPlot file for each timestep.
C For stand-alone: only one file is written (current timestep)!
          itplots=1
          call TECPLOTEND(itplots)
        ELSEIF(INO1.EQ.6)THEN

C Visualise flow.
C Check that we have results to visualise.
          if (.not.SIMOK) then
            helptopic='dfs_flow_vectors'
            call gethelptext(helpinsub,helptopic,nbhelp)
            call EASKABC('There are no results to visualise!',
     &        'Options:','Return to main menu',
     &        'Provide vectors file','Provide library file',IW,nbhelp)
            if (IW.eq.1) then
              goto 10
            elseif (IW.eq.2) then
              t72=LVEC
              call EASKS(t72,'Results file name?','  ',72,
     &          'CFD.vectors','vectors file',IER,nbhelp)
              call FINDFIL(t72,XST)
              IF(.NOT.XST) goto 10

C Call INICNT to set the grid counters and GRIDORTHO to set the mesh geometry.
C This is necessary if the grid has not yet been visualized or a simulation
C has not yet been performed.
              CALL INICNT
              CALL GRID(ier)
              LVEC=t72
              vecfil=t72
              call GETVEC(vecfil)
            elseif (IW.eq.3) then
              t72=LVEC
              call EASKS(t72,'Results library file name?','  ',72,
     &                   'test_vec.lib','library file',IER,nbhelp)
              call FINDFIL(t72,XST)
              IF(.NOT.XST) goto 10

C Call INICNT to set the grid counters and GRIDORTHO to set the mesh geometry.
C This is necessary if the grid has not yet been visualized or a simulation
C has not yet been performed.
              CALL INICNT
              CALL GRID(ier)
              call GETVECLIB(t72)
            endif
          endif

C Hard wire as elsewhere in dfs code for one esp-r thermal zone only.
          IZ=1
          call VISOUT(IZ)
        ELSEIF(INO1.EQ.7)THEN
          goto 10
        ENDIF
        GOTO 20
      ELSEIF(INO.EQ.9)THEN
C Report model details by re-reading input file with trace output.
        call usrmsg('Reporting details for model file: ',LCFD(1),'-')
        CALL ERPFREE(IFL,ISTAT)
        INQUIRE (FILE=LCFD(1),EXIST=XST)
        IF(.NOT.XST)THEN
          CALL EDISP(IUOUT,'  ')
          CALL EDISP(IUOUT,'Domain flow definition file not defined. ')
          CALL EDISP(IUOUT,
     &                'Please choose a model file with menu option a.')
          GOTO 10
        else
          write(currentfile,'(a)')LCFD(1)(1:LNBLNK(LCFD(1)))
          CALL EFOPSEQ(IFL,LCFD(1),1,IER)

C Only one zone possible so set IZ to 1.
          IZ=1
          CALL STRIPC(IFL,OUTSTR,0,ND,1,'dfd line 1',IER)
          if(OUTSTR(1:7).eq.'*DFS V2')then
            CALL ERPFREE(IFL,ISTAT)
            call DFDREAD(IZ,2,iuout,IER)
          elseif(OUTSTR(1:15).eq.'DFS DESCRIPTION')then
            CALL ERPFREE(IFL,ISTAT)
            CALL EFOPSEQ(IFL,LCFD(1),1,IER)
            CALL CFDDTA(itrc,iuout,IZ,IBLD,IMFS,IER)
            CALL ERPFREE(IFL,ISTAT)
          ENDIF
        ENDIF
        IF(IER.EQ.0)THEN
          INPOK=.TRUE.

C plot grid.
c          CALL INICNT
c          CALL GRID(ier)
c          call VGRID3D('---','xyz',iz)
c          call VSB3D(0,iz)
c          call VAO3D(0,iz)
c          IF(MMOD.EQ.8)THEN
c          WRITE(etext,'(2A)')'Model: ',
c     &        TITLE(IZ)(1:lnblnk(TITLE(IZ)))
c            iside=1
c            isize=1
c            ifont=1
c            call viewtext(etext,iside,isize,ifont)
c          ENDIF
        ELSE
          INPOK=.FALSE.
        ENDIF
      ELSEIF(INO.EQ.10)THEN

C Help via the askabout facility in esru_ask.F The zero requests display
        call askabout('dfs ',0)
      ELSEIF(INO.EQ.11)THEN

C Finish simulation.
C Should the results files be removed ?
        dok=.true.
        helptopic='dfs_ask_to_keep_results'
        call gethelptext(helpinsub,helptopic,nbhelp)
        CALL ASKOK(' ','Delete results file before exiting?',
     &    OK,dok,nbhelp)
        if (OK) then
          CALL EFDELET(ICFLIB,ISTAT)
          CALL EFDELET(ICFMON(ICFD),ISTAT)
        else
          CALL ERPFREE(ICFLIB,ISTAT)
          CALL ERPFREE(ICFMON(ICFD),ISTAT)
        endif

C Should the cfd temporary file be removed ?
        dok=.true.
        CALL ASKOK(' ','Delete temporary file before exiting?',
     &    OK,dok,nbhelp)
        if (OK) then
          CALL EFDELET(ICFTMP,ISTAT)
        else
          CALL ERPFREE(ICFTMP,ISTAT)
        endif

C Exit from ESRUcfd.
        CALL EPAGEND
        STOP
      ENDIF
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

      SUBROUTINE BASESIMP_INPUTS(ICOMP,IER)
      integer icomp,ier
      return
      end
      
      SUBROUTINE EDMLDB2(chgdb,ACTION,isel,IER)
      logical chgdb
      character*1 ACTION
      integer isel,ier
      ier=0
      return
      end

