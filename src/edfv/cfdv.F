C This file is part of the ESP-r system.
C Copyright Energy Systems Research Unit, University of
C Strathclyde, Glasgow Scotland, 2001-.

C ESP-r is free software.  You can redistribute it and/or
C modify it under the terms of the GNU General Public
C License as published by the Free Software Foundation
C (version 2 or later).

C ESP-r is distributed in the hope that it will be useful
C but WITHOUT ANY WARRANTY; without even the implied
C warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
C PURPOSE. See the GNU General Public License for more
C details.


C ********************* DFV *********************
C Stand-alone CFD display module. It takes an ASCII CFD
C performance-at-a-timestep file and displays a 3D vector image.
C Passed: iappw, iappx and iappy as pixel width of application, left
C and top offsets. If these are 0 then use default values.

C Pass it both the .dfd file and the timestep results file:
C cfdv -file ?.dfd -actfile cfd3dascii_0??? -range T T

      program dfv
      USE START_UP
#include "building.h"
#include "model.h"
#include "geometry.h"
#include "cfd.h"
#include "esprdbfile.h"
#include "material.h"
#include "espriou.h"
#include "prj3dv.h"
#include "help.h"

      integer lnblnk  ! function definition

      COMMON/OUTIN/IUOUT,IUIN,IEOUT
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

C Session log file (for errors etc.)
      character iefile*72
      common/logs/iefile

C Defaults.
      COMMON/DEFLT3/DFCFD,DECMPDBFL,DICONDBFL

C CFD common blocks.
      common/cfdfil/LCFD(MCOM),IFCFD(MCOM)
      common/param2/TITLE(MNZ),CFTRFL(MNZ),LPHI(MNZ)
      COMMON/SIMT/TSTEP,NSINC
      COMMON/ICFNOD/ICFD,ICP
      COMMON/ICFCHN/ICFMON(MNZ),ICFDBC(MNZ),ICFTMP,ICFLIB

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
     &             ICNACT(MCFND,MNZ),IVOLNOP(MCFND,MNZ)

C Saved state of the visualisation
      COMMON/MODVIS/IVISMOD
      COMMON/CFDVIS/HAS_GEOM,ISHSB,ISHAO,IFACES,ISHBLK,ISHSRC,ISHGEO,
     &              INITD
      logical HAS_GEOM
      character INITD*6

C Commons from mocfd.F (RDCFDAT)
      COMMON/GEOM/XP(ntcelx),YP(ntcely),ZP(ntcelz),
     1            DXEP(ntcelx),DXPW(ntcelx),DYNP(ntcely),DYPS(ntcely),
     2            DZHP(ntcelz),DZPL(ntcelz),
     3            SEW(ntcelx),SNS(ntcely),SHL(ntcelz),
     4            XU(ntcelx),YV(ntcely),ZW(ntcelz)

      common/vecXYZ/vecXbeg(ntcelx,ntcely,ntcelz),
     1              vecXend(ntcelx,ntcely,ntcelz),
     2              vecYbeg(ntcelx,ntcely,ntcelz),
     3              vecYend(ntcelx,ntcely,ntcelz),
     4              vecZbeg(ntcelx,ntcely,ntcelz),
     5              vecZend(ntcelx,ntcely,ntcelz)
      COMMON/VARf/Uf(ntcelx,ntcely,ntcelz),Vf(ntcelx,ntcely,ntcelz),
     1            Wf(ntcelx,ntcely,ntcelz),
     2            P(ntcelx,ntcely,ntcelz),PP(ntcelx,ntcely,ntcelz),
     3            TEf(ntcelx,ntcely,ntcelz),EDf(ntcelx,ntcely,ntcelz)
      COMMON/TEMPf/Tf(ntcelx,ntcely,ntcelz),GAMH(ntcelx,ntcely,ntcelz),
     1             RESORT,NSWPT,URFT,FSDTT,PRANDL,PFUN
      common/veccol/IVRED(ntcelx,ntcely,ntcelz),
     &              IVGRN(ntcelx,ntcely,ntcelz),
     &              IVBLU(ntcelx,ntcely,ntcelz)
      COMMON/LOCAGE/AGEf(ntcelx,ntcely,ntcelz) 
      real CFTIMS,CFTIMF    ! start and finish times for CFD assessment
      integer ICFDYS,ICFDYF ! julian day-of-year at start and end of CFD
      integer ICFAUX  ! helps to keep track of IFCFD()
      integer ICFVIEW ! toggle for preview via dfv during assessment
      integer ICFVIEWMINT,ICFVIEWMAXT  ! range of temperatures for dfv
      logical DFVinvoked
      COMMON/CFSEUP/CFTIMS,CFTIMF,ICFDYS,ICFDYF,ICFAUX(MCOM),ICFVIEW,
     &  ICFVIEWMINT,ICFVIEWMAXT,DFVinvoked
      common/EQTION3/CALLMA(MNZ),CALPOL(MCTM,MNZ),POLNAM(MCTM,MNZ),
     &               NCTM(MNZ),JHUMINDX(MNZ),URFC(MCTM)
      LOGICAL CALPOL,CALLMA
      COMMON/CFDPOL/POLCONCp(MCTM,ntcelx,ntcely,ntcelz),
     1              POLCONCf(MCTM,ntcelx,ntcely,ntcelz)
      common/flvcol/ISOPT(6),IBGOPT(6),SCmin,SCmax
      common/flvdat/Nview(MCOM),ISURvw(MCOM,6),ILAYvw(MCOM,6),
     &              IRESvw(MCOM,6),VLSCvw(MCOM,6),VTSCvw(MCOM,6),
     &              HLSCvw(MCOM,6),HTSCvw(MCOM,6),IFRQvw(MCOM,6)
      COMMON/VISSET/imgqua,NOXX,NOZZ,tscal,hLscal,hWscal,vLscal
      common/GRIDFN/NCELX(MNREG,MNZ),NCELY(MNREG,MNZ),NCELZ(MNREG,MNZ),
     &  NCELZE(MNREG,MNZ),XREG(MNREG,MNZ),YREG(MNREG,MNZ),
     &  ZREG(MNREG,MNZ),ZREGE(MNREG,MNZ),Xplaw(MNREG,MNZ),
     &  Yplaw(MNREG,MNZ),Zplaw(MNREG,MNZ),Zplawe(MNREG,MNZ),NREG(4,MNZ)

      integer ISIMDAY ! Julian day within the cfd3dasci file
      real SIMTIMEF   ! time associated with the cfd3dasci file
      COMMON/DFVtime/ISIMDAY,SIMTIMEF
      
C Ask to overwrite flag.
      COMMON/OVRWT/AUTOVR
      logical AUTOVR

      CHARACTER*24 ITEM(11)
      CHARACTER inf*144,LCFD*72,POLNAM*12
      CHARACTER DFCFD*72,Lflpar*72
      character cfd3dasci*72
      character path*72,longtfile*144,ETEXT*82
      CHARACTER doit*248,tmode*8,outs*124,fs*1,outstr*124
      CHARACTER*72 TITLE,CFTRFL,LPHI,RESFL,WORD
      integer islice,jslice,kslice
      CHARACTER*72 LVEC,LBIT
      character*72 DECMPDBFL,DICONDBFL
      character LCFDV*72
      character t24*24
      character dstmp*24,uname*24,tfile*72

      character cVnum*38      ! returned from ESPrVersionNum
      character pagestitle*68 ! for banner title via epages call

      LOGICAL INCALU,INCALV,INCALW,INCALK,INCALD,INCALT
      LOGICAL IZEROT,IZanKE,IMITZ
      logical photon,matlab,textout,flwvis,xslice,yslice,zslice
      LOGICAL XST,PXST,NXST,INPOK,unixok
      LOGICAL OK,GGDH,doslice
      LOGICAL areaSSO

      integer iglib   ! if 1 then X11, if 2 then GTK, if 3 then text only.
      integer NITMS  ! max items and current menu item
      integer ISHSB,ISHAO,IFACES
      integer ISTRW

C length of performance file plus last digits now previous and next.
      integer lncfd3d,cfd3dnow,cfd3dprev,cfd3dnext

C last 4 char in name, file name for previous and next timesteps.
      character last4*4,cfd3dasciprev*72,cfd3dascinext*72

#ifdef OSI
      integer iigl,iigr,iigt,iigb,iigw,iigwh
      integer iiw1,iiw2,iiw3,iiw4,iimenu
      integer iside,isize,ifont

#else
      integer*8 iigl,iigr,iigt,iigb,iigw,iigwh
      integer*8 iiw1,iiw2,iiw3,iiw4,iimenu
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
      ISHSB=1; ISHAO=1; IFACES=1

C Clear all arrays.
      call ezero
      call curmodule('dfv ')
      cAppName = 'dfv'
      helpinapp='dfs'  ! set once for the application
      helpinsub='dfv'  ! set for MAIN

C Initialization for matlab.
      islice=2; jslice=2; kslice=2
      xslice=.FALSE.; yslice=.FALSE.; zslice=.FALSE.
      doslice=.FALSE.

C Initialise strings.
      LVEC='UNKNOWN'
      LBIT='UNKNOWN'
      LCFDV='UNKNOWN'

C Default image format is 3d screen plot.
      imgtyp=3
      IMOPTS=0
      do 5 I=1,MCOM
        Lflpar(I)='UNKNOWN'
 5    continue

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
      matver=0.0     ! initial assumption of binary materials database
      ifs=1; itfs=1; imfs=1
      AUTOVR=.false. ! by default, ask to overwrite files

C Temporary cfd file unit.
      ICFTMP=IFIL+4

      path='./'

C inf is the zone DFD file the cfd3dasci takes the form of -actf ascii_dfs_results_file.
      SCmax=25.0; SCmin=10.0

      call parsedfv(MODL,iappw,iappx,iappy,inf,cfd3dasci,
     &  itmin,itmax)
       
      childterminal=MODL  ! remember so that child processes can run the same
      SCmin=real(itmin)
      SCmax=real(itmax)

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
      IVISMOD=2

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
        iappw=600
        iappx=90
        iappy=50
      else
        if(iappx.le.0)iappx=90
        if(iappy.le.0)iappy=50
        if(iappw.le.200)then
          iappwi=int(600*iappw*0.01)
          iappw=iappwi
        elseif(iappw.gt.200)then
          continue
        endif
      endif

C Set to ~square display.
      iapphi=iappw+70
      iappwi=iappw

      if(iappw.gt.0.and.iappw.lt.100)then
        menuchw = 2
        LIMTTY=4
        LIMIT =4
      else
        menuchw = 2
        LIMTTY=4
        LIMIT =4
      endif
      IF(MMOD.EQ.8)THEN

C Set initial font sizes (IMFS is for menus, IFS is for dialog & ITFS text feedback).
C Fonts 4-7 are proportional and 0-3 are fixed width. Use proportional for menus
C and dialog.
        IMFS=5
        IFS=5
        ITFS=4
#ifdef OSX
        IMFS=4
        IFS=4  ! use a smaller fonts
        ITFS=4
#endif
        call userfonts(IFS,ITFS,IMFS)
        call defaultfonts(IFS,ITFS,IMFS)  ! and remember these as defaults
      ELSE
        LIMTTY=12
        LIMIT =12
      ENDIF

C Find the current ESP-r version number and add it to application title.
      call ESPrVersionNum(cVnum)
      write(pagestitle,'(2a)') 'CFD viewer of ESP-r ',
     &  cVnum(1:lnblnk(cVnum))
      lntitle=lnblnk(pagestitle)
      CALL EPAGES(MMOD,IUIN,IUOUT,iappwi,iapphi,iappx,iappy,menuchw,
     &  pagestitle,lntitle)

C Open the text display box equal to LIMTTY if MMOD = 8.
      IF(MMOD.EQ.8)THEN

C Setup and pass in parameters to win3d.
C        iiw1=6; iiw2=4; iiw3=4; iiw4=2; iimenu=menuchw
        iiw1=2; iiw2=2; iiw3=3; iiw4=1; iimenu=menuchw
        iigl=igl; iigr=igr; iigt=igt; iigb=igb; iigw=igw; iigwh=igwh
        CALL win3d(iimenu,iiw1,iiw2,iiw3,iiw4,
     &    iigl,iigr,iigt,iigb,iigw,iigwh)
        igl=int(iigl); igr=int(iigr); igt=int(iigt); igb=int(iigb)
        igw=int(iigw); igwh=int(iigwh)
        iglib = igraphiclib()  ! find out if X11 or GTK or text support only.
        if(iglib.eq.1)then
          call opencpw
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

C Use askabout to instantiate the initial help messages (2nd parameter is one).
      call askabout('dfv ',1)

C Create and open a session log file based on user name, PID etc.
      uname=' '; tfile=' '
      call usrname(uname)
      call esppid(ipid)
        write(tfile,'(a,a1,a,a,i7,a)')upath(1:lnblnk(upath)),fs,'.',
     &    uname(1:lnblnk(uname)),ipid,'.log'
      call st2file(tfile,iefile)

C Open the session file.
      ieout=ifil+931          ! set to unused index 942
      open(ieout,file=iefile,status='UNKNOWN',err=903)
      write(ieout,'(a)')'Session log for cfdv'
      call dstamp(dstmp) ! get curret time
      write(ieout,'(2a)')'Date,',dstmp
      write(ieout,'(2a)')'User,',uname(1:lnblnk(uname))
  903 continue

C Help text for results sub-menu.
      helptopic='dfv_results_menu'
      call gethelptext(helpinsub,helptopic,nbhelp)

C Take `c' command line file name, convert to fortran string LCFDV
C and place as initial zone CFD configuration file.
C << decide whether LCFDV needs to be > 72 char >>
      if(inf(1:2).ne.'  '.and.inf(1:4).ne.'UNKN')then
        llt=lnblnk(inf)
        if(llt.le.72)then
          write(LCFDV,'(a)') inf(1:lnblnk(inf))
          write(LCFD(1),'(a)') inf(1:lnblnk(inf))
        else
          write(LCFDV,'(a)') inf(1:72)
          write(LCFD(1),'(a)') inf(1:72)
        endif
        write(outs,'(a,a)')' the input file is: ',LCFDV
C        call edisp(iuout,outs)
      else
        LCFDV='UNKNOWN'
        LCFD(1)='UNKNOWN'
      endif

C If an input file has been specified then load it and display
C its contents and return to the main dialog.
      XST=.FALSE.
      INQUIRE (FILE=LCFDV,EXIST=XST)
      IF(XST)THEN

C Set currentfile
        write(currentfile,'(a)')LCFDV(1:LNBLNK(LCFDV))
        CALL EFOPSEQ(IFL,LCFDV,1,IER)

C Only one zone possible so set IZ to 1.
        IZ=1
        CALL STRIPC(IFL,OUTSTR,0,ND,1,'dfd line 1',IER)
        if(OUTSTR(1:7).eq.'*DFS V2')then
          CALL ERPFREE(IFL,ISTAT)
          call DFDREAD(IZ,0,iuout,IER)
        elseif(OUTSTR(1:15).eq.'DFS DESCRIPTION')then
          CALL ERPFREE(IFL,ISTAT)
          CALL EFOPSEQ(IFL,LCFDV,1,IER)
          CALL CFDDTA(itrc,iuout,IZ,IBLD,IMFS,IER)
          CALL ERPFREE(IFL,ISTAT)
        ENDIF
      ENDIF
 77   IF((XST).AND.IER.EQ.0)THEN

C Having read in the DFD file we might be in a position
C to draw the grid of the domain.
        CALL INICNT    !  set the grid counters
        CALL GRID(ier) !  generate the grid

        if(cfd3dasci(1:2).ne.'  '.and.cfd3dasci(1:4).ne.'UNKN')then

C If display other than a slice confirm with user temp scale.
          if(.NOT.doslice)then
            write(t24,'(2f6.1)') SCmax,SCmin
 204        helptopic='contaminant_scale_req'
            call gethelptext(helpinsub,helptopic,nbhelp)
            call EASKS(t24,'  ',
     &      'max and min Temp:',24,'50.0 0.0','scale',IER,nbhelp)
            K=0
            call EGETWR(t24,K,SCmax,0.,0.,'-','maximum',IER)
            call EGETWR(t24,K,SCmin,0.,0.,'-','minimum',IER)
            if (SCmax.lt.SCmin) goto 204
          endif

C Read this timesteps data (equiv to RDCFDAT(ICFTS) in mocfd.F
          call readcfd3dascii(IZ,cfd3dasci,ISIMDAY,SIMTIMEF)

C As in mocfd.F and as passed into FTV.
          IM=1; ISUR=1; ilayer=5
          IRESvw(1,1)=3; imgqua=3
          ISURvw(1,1)=1; ISUR=1   ! south
C          VLSCvw(1,1)=1.0; vLscal=1.0 ! defaults
          VLSCvw(1,1)=1.0; vLscal=2.0 ! exagerate length slightly
          VTSCvw(1,1)=1.0; tscal=1.0
          HLSCvw(1,1)=1.0; hLscal=1.0
          HTSCvw(1,1)=1.0; hWscal=1.0
          IFRQvw(1,1)=4

C Set the number of pixels required for the image.  This is a function
C of the resolution specified by the user and the geometry of the model.
          CALL SETRES(ISUR)

C Fill commons for a nominal south facing surface so that TRNSF1 
C and LENS have something to work with. Base the dimensions on the
C size of the domain.
          XLEN=0.0; YLEN=0.0; ZLEN=0.0
          do IGR=1,NREG(1,ICFD)
            XLEN=XLEN+XREG(IGR,ICFD)
          enddo
          do IGR=1,NREG(2,ICFD)
            YLEN=YLEN+YREG(IGR,ICFD)
          enddo
          do IGR=1,NREG(3,ICFD)
            ZLEN=ZLEN+ZREG(IGR,ICFD)
          enddo

          nver(1)=4
          x(1)=0.; x(2)=XLEN; x(3)=XLEN; x(4)=0.
          y(1)=0.; y(2)=0.; y(3)=0.; y(4)=0.
          z(1)=0.; z(2)=0.; z(3)=ZLEN; z(4)=ZLEN
          jvn(1,1)=1; jvn(1,2)=2; jvn(1,3)=3; jvn(1,4)=4;
          XMN=0; YMN=0.; ZMN=0.
          XMX=XLEN; YMX=YLEN; ZMX=ZLEN
          VIEWM(1)=(XLEN*0.5); VIEWM(2)=(YLEN*0.5) 
          VIEWM(3)=(ZLEN*0.5)

C Translate and rotate the site co-ordinate system to a face co-ordinate
C system for the viewing surface.
          CALL TRNSF1(ISUR)

C Translate and rotate the start and end points of each flow vector to the face
C co-ordinate system of the face under consideration.
          CALL TRNSF2

C Draw the domain prior to the arrows. 
          HAS_GEOM=.FALSE.
          ISHSB=-1; ISHAO=-1; IFACES=1; ISHBLK=-1; ISHGEO=0
          call redraw(ier)

          if(doslice)then
            call pauses(1) 
          else
            call pauses(2) 
          endif

C Create vector info.
          do 200 K=2,NKM1
            do 201 J=2,NJM1
              do 202 I=2,NIM1
                hlfUatP=0.25*(Uf(I,J,K)+Uf(I+1,J,K))
                hlfVatP=0.25*(Vf(I,J,K)+Vf(I,J+1,K))
                hlfWatP=0.25*(Wf(I,J,K)+Wf(I,J,K+1))
C Debug.
C                write(6,*) ' IJK hlf ',I,J,K,hlfUatP,hlfVatP,hlfWatP
                VECXbeg(I,J,K) = XP(I) - hlfUatP
                VECXend(I,J,K) = XP(I) + hlfUatP
                VECYbeg(I,J,K) = YP(J) - hlfVatP
                VECYend(I,J,K) = YP(J) + hlfVatP
                VECZbeg(I,J,K) = ZP(K) - hlfWatP
                VECZend(I,J,K) = ZP(K) + hlfWatP

C Calculate colour of vector (prop to temperature)
                call ICOLOUR(25.0,15.0,Tf(i,j,k),IR,IG,IB)
                IVRED(i,j,k)=IR
                IVGRN(i,j,k)=IG
                IVBLU(i,j,k)=IB
 202          continue
 201        continue
 200      continue
          INPOK=.TRUE.

C Then in mocfd.F we go into local version of MAKS3DF.
          if(doslice)then
            CALL MAKS3DFISLICE(IFRAME,ISLICE)
            call pauses(1)
            write(outs,'(a,i2,a)') 'Current I slice is ',ISLICE,'.'
            call EASKABC(outs,'Option:','shift left','shift right',
     &        'cancel',IBR,nbhelp)
            if(IBR.eq.1)then
              ISLICE=ISLICE-1; XST=.true.
              goto 77
            elseif(IBR.eq.2)then
              ISLICE=ISLICE+1; XST=.true.
              goto 77
            elseif(IBR.eq.3)then
              doslice=.false.
            endif
          else
            CALL MAKS3DF(IFRAME)
            call pauses(2)
          endif
        else
          call usrmsg('CFD performance data file not defined!',
     &      'aborting.','W')
        endif
      ELSE
        INPOK=.FALSE.
      ENDIF

C Write the CFD title on the 3rd line of the header.
c      WRITE(etext,'(2A)')'Domain: ',
c     &  TITLE(IZ)(1:lnblnk(TITLE(IZ)))
c      iside=1
c      isize=1
c      ifont=1
c      call viewtext(etext,iside,isize,ifont)

C From this point need to draw as in MOCFD->VISMAK->FTV-MAKS3D
      call EASKABC('  ','Option:','exit application',
     &  'read another timestep','view an I slice',IBR,nbhelp)
      if(IBR.eq.1)then
        close(ieout)
        CALL ERPFREE(ieout,ISTAT)
        CALL EPAGEND
        STOP
      elseif(IBR.eq.2)then

C cfd3dasci files will tend to follow name pattern: cfd3dascii_2026
C where the last 4 characters are a timestep. Copy these and test
C incrementing or decrementing and see if such a file exists.
        lncfd3d=lnblnk(cfd3dasci)  ! find length
        write(last4,'(a)')cfd3dasci(lncfd3d-3:lncfd3d) ! get last 4 char
        read(last4,*) cfd3dnow  ! convert to integer
        cfd3dprev=cfd3dnow-1; cfd3dnext=cfd3dnow+1
        write(cfd3dasciprev,'(a,i4.4)') cfd3dasci(1:lncfd3d-4),cfd3dprev
        write(cfd3dascinext,'(a,i4.4)') cfd3dasci(1:lncfd3d-4),cfd3dnext

C Debug
C        write(6,*) 'current ',cfd3dasci(1:lncfd3d)
C        write(6,*) 'prev ',cfd3dasciprev(1:lncfd3d)
C        write(6,*) 'next ',cfd3dascinext(1:lncfd3d)
        XST=.FALSE.
        INQUIRE (FILE=cfd3dasciprev,EXIST=PXST)
        INQUIRE (FILE=cfd3dascinext,EXIST=NXST)
        doslice=.false.
        if(PXST.AND.NXST)then
          call easkabc(' ','Domain performance file:',
     &      'previous timestep','next timestep','other',IW,nbhelp)
          if(iw.eq.1)then
            cfd3dasci=cfd3dasciprev
            IER=0; XST=PXST
            goto 77  ! redraw it
          elseif(iw.eq.2)then
            cfd3dasci=cfd3dascinext
            IER=0; XST=NXST
            goto 77  ! redraw it
          elseif(iw.eq.3)then
            ISTRW=72
            CALL EASKF(cfd3dasci,' ','Domain performance name?',
     &        ISTRW,'cfd3dasci','Domain performance file',IER,nbhelp)
            XST=.FALSE.
            INQUIRE (FILE=cfd3dasci,EXIST=XST)
            IER=0
            goto 77  ! redraw it
          endif
        elseif(.NOT.PXST.AND.NXST)then
          call easkabc(' ','Domain performance file:',
     &      'N/A','next timestep','other',IW,nbhelp)
          if(iw.eq.2)then
            cfd3dasci=cfd3dascinext
            IER=0; XST=NXST
            goto 77  ! redraw it
          else
            ISTRW=72
            CALL EASKF(cfd3dasci,' ','Domain performance name?',
     &        ISTRW,'cfd3dasci','Domain performance file',IER,nbhelp)
            XST=.FALSE.
            INQUIRE (FILE=cfd3dasci,EXIST=XST)
            IER=0
            goto 77  ! redraw it
          endif
        elseif(PXST.AND.(.NOT.NXST))then
          call easkabc(' ','Domain performance file:',
     &      'previous timestep','N/A','other',IW,nbhelp)
          if(iw.eq.1)then
            cfd3dasci=cfd3dasciprev
            IER=0; XST=PXST
            goto 77  ! redraw it
          else
            ISTRW=72
            CALL EASKF(cfd3dasci,' ','Domain performance name?',
     &        ISTRW,'cfd3dasci','Domain performance file',IER,nbhelp)
            XST=.FALSE.
            INQUIRE (FILE=cfd3dasci,EXIST=XST)
            IER=0
            goto 77  ! redraw it
          endif
        endif
      elseif(IBR.eq.3)then
        IER=0; XST=.true.

C Try just drawing half way along the I axis.
        ISLICE=NINT(real(NIM1)/2.0)
        doslice=.true.
        goto 77  ! redraw slice
      endif

      END

C ********************* readcfd3dascii *********************
C Read ascii information from one time step with simulation
C day and time step returned via ISIMDAY & SIMTIMEF.

      SUBROUTINE readcfd3dascii(ICOMP,cfd3dasci,ISIMDAY,SIMTIMEF)
#include "building.h"
#include "cfd.h"
#include "espriou.h"

      COMMON/OUTIN/IUOUT,IUIN,IEOUT
      COMMON/FILEP/IFIL
      
      integer ncomp,ncon
      common/c1/ncomp,ncon
      COMMON/ICFNOD/ICFD,ICP

      COMMON/ALL/NI,NJ,NK,NIM1,NJM1,NKM1,NIM2,NJM2,NKM2
      COMMON/GEOM/XP(ntcelx),YP(ntcely),ZP(ntcelz),
     1            DXEP(ntcelx),DXPW(ntcelx),DYNP(ntcely),DYPS(ntcely),
     2            DZHP(ntcelz),DZPL(ntcelz),
     3            SEW(ntcelx),SNS(ntcely),SHL(ntcelz),
     4            XU(ntcelx),YV(ntcely),ZW(ntcelz)
      COMMON/cfdfil/LCFD(MCOM),IFCFD(MCOM)
      COMMON/cfdsmper/ICDYS,ICDYF,CFTS,CFTF
      COMMON/cfdhsh/NCFDSZ,NRCFDOM(MNZ)
      common/EQTION3/CALLMA(MNZ),CALPOL(MCTM,MNZ),POLNAM(MCTM,MNZ),
     &               NCTM(MNZ),JHUMINDX(MNZ),URFC(MCTM)
      common/CFDSV/IRECPC,ICFDSV,IEQSV(5+MCTM)
      
C NCDOM - number of CFD domains
C ICDOM - CFD domain selected for output
C ICFDZ - thermal zone associated with each CFD domain
      COMMON/cfddoms/NCDOM,ICDOM,ICFDZ(MNZ)
      COMMON/VARf/Uf(ntcelx,ntcely,ntcelz),Vf(ntcelx,ntcely,ntcelz),
     1            Wf(ntcelx,ntcely,ntcelz),
     2            P(ntcelx,ntcely,ntcelz),PP(ntcelx,ntcely,ntcelz),
     3            TEf(ntcelx,ntcely,ntcelz),EDf(ntcelx,ntcely,ntcelz)
      COMMON/TEMPf/Tf(ntcelx,ntcely,ntcelz),GAMH(ntcelx,ntcely,ntcelz),
     1             RESORT,NSWPT,URFT,FSDTT,PRANDL,PFUN
      COMMON/LOCAGE/AGEf(ntcelx,ntcely,ntcelz) 
      real CFTIMS,CFTIMF    ! start and finish times for CFD assessment
      integer ICFDYS,ICFDYF ! julian day-of-year at start and end of CFD
      integer ICFAUX  ! helps to keep track of IFCFD()
      integer ICFVIEW ! toggle for preview via dfv during assessment
      integer ICFVIEWMINT,ICFVIEWMAXT  ! range of temperatures for dfv
      logical DFVinvoked
      COMMON/CFSEUP/CFTIMS,CFTIMF,ICFDYS,ICFDYF,ICFAUX(MCOM),ICFVIEW,
     &  ICFVIEWMINT,ICFVIEWMAXT,DFVinvoked
      COMMON/CFDPOL/POLCONCp(MCTM,ntcelx,ntcely,ntcelz),
     1              POLCONCf(MCTM,ntcelx,ntcely,ntcelz)

      LOGICAL CALPOL,CALLMA
      CHARACTER LCFD*72,POLNAM*12
      character cfd3dasci*72
      character loutstr*1000,WORD*32
      character outs*124
      real VAL

C Set unit number.
      IER=0; ICTM=0; ISIMDAY=0; SIMTIMEF=0.0
      iunit=38
      CALL EFOPSEQ(iunit,cfd3dasci,1,IER)
      IF(IER.LT.0)THEN
        write(outs,'(3a)') 'cfd3dascii file ',
     &    cfd3dasci(1:lnblnk(cfd3dasci)),
     &    ' could not be opened.'
        call edisp(ITRU,outs)
        IER=1
        RETURN
      ENDIF
      write(currentfile,'(a)') cfd3dasci(1:lnblnk(cfd3dasci))
      
  42  CALL STRIPC1K(iunit,LOUTSTR,0,ND,1,'cfd3dlines',IER)
      IF(IER.NE.0) goto 1002
      K=0
      CALL EGETW(LOUTSTR,K,WORD,'W','header tags',IER)
      IF(IER.NE.0) goto 1002
      if(WORD(1:10).eq.'cfd3dascii')then
        continue
      elseif(WORD(1:14).eq.'end_cfd3dascii')then
        CLOSE ( UNIT=iunit )
        return
      elseif(WORD(1:7).eq.'head_1:')then

C Read head_1: MCEL1D,ICFDSV,(IEQSV(J),J=1,5+NCTM(ICFD)).
        CALL EGETWI(LOUTSTR,K,NCEL1D,0,0,'-','MCEL1D',IER)
        CALL EGETWI(LOUTSTR,K,ICFDSV,0,0,'-','ICFDSV',IER)
        CALL EGETWI(LOUTSTR,K,ICFD,0,0,'-','ICFD',IER)
        CALL EGETWI(LOUTSTR,K,NCOMP,0,0,'-','NCOMP',IER)
        DO J=1,5+NCTM(ICFD)
          CALL EGETWI(LOUTSTR,K,IEQSV(J),0,0,'-','IEQSV',IER)
        ENDDO
      elseif(WORD(1:7).eq.'head_2:')then
        
C Read head_2: (abs(ICFAUX(I)),I=1,NCOMP).
        DO I=1,NCOMP   ! where do we get NCOMP?
          CALL EGETWI(LOUTSTR,K,ICFAUX(I),0,0,'-','ICFAUX',IER)
        ENDDO

      elseif(WORD(1:7).eq.'head_3:')then

C Read head_3: ICFDYS,ICFDYF,CFTIMS,CFTIMF.
        CALL EGETWI(LOUTSTR,K,ICFDYS,0,0,'-','ICFDYS',IER)
        CALL EGETWI(LOUTSTR,K,ICFDYF,0,0,'-','ICFDYF',IER)
        CALL EGETWR(LOUTSTR,K,CFTIMS,0.,23.,'W','CFTIMS',IER)
        CALL EGETWR(LOUTSTR,K,CFTIMF,0.,24.,'W','CFTIMF',IER)

      elseif(WORD(1:7).eq.'head_4:')then

C Read head_4: NCTM(ICFD).
        CALL EGETWI(LOUTSTR,K,NCTM(ICFD),0,0,'-','NCTM(ICFD)',IER)

      elseif(WORD(1:7).eq.'head_5:')then

C Read head_5: ISIMDAY SIMTIMEF.
        K=12
        CALL EGETWI(LOUTSTR,K,ISIMDAY,0,0,'-','IDYP',IER)
        CALL EGETW(LOUTSTR,K,WORD,'W',' at ',IER)
        CALL EGETWR(LOUTSTR,K,SIMTIMEF,0.,24.,'W','CFTIMF',IER)

      elseif(WORD(1:4).eq.'POL:')then

C Read POLNAM: loop POLNAM(ICTM,ICFD).
        CALL EGETW(LOUTSTR,K,WORD,'W','header tags',IER)
        ICTM=ICTM+1
        write(POLNAM(ICTM,ICFD),'(a)') WORD(1:lnblnk(WORD))

      elseif(WORD(1:5).eq.'zone:')then

C Read zone.
        CALL EGETWI(LOUTSTR,K,ICOMP,0,0,'-','ICOMP',IER)

      elseif(WORD(1:4).eq.'all:')then

C Read all.
        CALL EGETWI(LOUTSTR,K,NI,0,0,'-','NI',IER)
        CALL EGETWI(LOUTSTR,K,NJ,0,0,'-','NJ',IER)
        CALL EGETWI(LOUTSTR,K,NK,0,0,'-','NK',IER)
        CALL EGETWI(LOUTSTR,K,NIM1,0,0,'-','NIM1',IER)
        CALL EGETWI(LOUTSTR,K,NJM1,0,0,'-','NJM1',IER)
        CALL EGETWI(LOUTSTR,K,NKM1,0,0,'-','NKM1',IER)
        CALL EGETWI(LOUTSTR,K,NIM2,0,0,'-','NIM2',IER)
        CALL EGETWI(LOUTSTR,K,NJM2,0,0,'-','NJM2',IER)
        CALL EGETWI(LOUTSTR,K,NKM2,0,0,'-','NKM2',IER)

      elseif(WORD(1:3).eq.'XU:')then

C Read XU.
        DO I=1,NI
          CALL EGETWR(LOUTSTR,K,XU(I),0.,0.,'-','XU(I)',IER)
        ENDDO

      elseif(WORD(1:3).eq.'YV:')then

C Read YV.
        DO I=1,NJ
          CALL EGETWR(LOUTSTR,K,YV(I),0.,0.,'-','YV(I)',IER)
        ENDDO

      elseif(WORD(1:3).eq.'ZW:')then

C Read ZW.
        DO I=1,NK
          CALL EGETWR(LOUTSTR,K,ZW(I),0.,0.,'-','ZW(I)',IER)
        ENDDO

      elseif(WORD(1:8).eq.'Cell_KJ:')then

C Read cells: get current index for K & J.
        ICTM=0  ! reset
        CALL EGETWI(LOUTSTR,K,KK,0,0,'-','KK',IER)
        CALL EGETWI(LOUTSTR,K,JJ,0,0,'-','JJ',IER)
        goto 42
      elseif(WORD(1:3).eq.'Uf:')then
        DO I=2,NI
          CALL EGETWR(LOUTSTR,K,VAL,0.,0.,'-','Uf()',IER)
          Uf(I,JJ,KK)=VAL
        ENDDO
      elseif(WORD(1:3).eq.'Vf:')then
        DO I=2,NI
          CALL EGETWR(LOUTSTR,K,VAL,0.,0.,'-','Vf()',IER)
          Vf(I,JJ,KK)=VAL
        ENDDO
      elseif(WORD(1:3).eq.'Wf:')then
        DO I=2,NI
          CALL EGETWR(LOUTSTR,K,VAL,0.,0.,'-','Wf()',IER)
          Wf(I,JJ,KK)=VAL
        ENDDO
      elseif(WORD(1:3).eq.'Tf:')then
        DO I=2,NI
          CALL EGETWR(LOUTSTR,K,VAL,0.,0.,'-','Tf()',IER)
          Tf(I,JJ,KK)=VAL
        ENDDO
      elseif(WORD(1:5).eq.'AGEf:')then
        DO I=2,NI
          CALL EGETWR(LOUTSTR,K,VAL,0.,0.,'-','AGEf()',IER)
          AGEf(I,JJ,KK)=VAL
        ENDDO
      elseif(WORD(1:5).eq.'POL:')then
        DO I=2,NI
          CALL EGETWR(LOUTSTR,K,VAL,0.,0.,'-','POLCONCf()',IER)
          ICTM=ICTM+1
          POLCONCf(ICTM,I,JJ,KK)=VAL
        ENDDO
      endif
      goto 42

C Errors for loutstr reads.
 1002 write(outs,'(3a)') 'readcfd3dascii: conversion error in...',
     &  LOUTSTR(1:50),'...'
      
      end


C ************* imgdisp *************
C Dummy routine (needed for call back from c).
      subroutine imgdisp(iforce,focus,ier)
      character focus*4

      return
      end

C Dummy subroutines needed to compile (called from library code).
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

C Dummy subroutines needed by e3dviews.F
      SUBROUTINE CNVBLK(XO,YO,ZO,DX,DY,DZ,A)
      return
      end

      SUBROUTINE CNVBLK3A(XO,YO,ZO,DX,DY,DZ,A,B,C)
      return
      end

      SUBROUTINE CNVBLKP(IZ,IB)
      return
      end

      SUBROUTINE CNVVISP(IZ,IB)
      return
      end

C Dummy subroutines needed by common3dv.F
      SUBROUTINE EPKMLC(ISEL,PROMPT1,PROMPT2,IER)
      CHARACTER*(*) PROMPT1,PROMPT2
      return
      end

      subroutine cfgtogg(icfg_type,icfgz,icfgn,icfgc,icfgdfn,icfgpln,
     &                   iicfgz,iicfgn,iicfgc,iicfgdfn,iicfgpln)
      return
      end

      subroutine cfgpk(act)
      character act*1
      return
      end

      SUBROUTINE ESCZONE(ICOMP)
      return
      end

      SUBROUTINE DRAWESP(ier)
      return
      end
      
      SUBROUTINE GRAAPH(IDRW1,IDRW2)
      return
      end

      SUBROUTINE ERCZONE(ICOMP)
      return
      end

      SUBROUTINE LN2AZ(x1,y1,z1,x2,y2,z2,azim,elev)
      return
      end

      SUBROUTINE EGOMST(IUNIT,ICOMP,LOBS,IR,ITRC,ITRU,IER)
      CHARACTER LOBS*72
      return
      end

      SUBROUTINE GEOREAD(IUNIT,LGEOMF,ICOMP,IR,ITRU,IER)
      CHARACTER LGEOMF*72
      return
      end

      SUBROUTINE EASKGEOF(PROMPT,CFGOK,IZONE,MOD,IMW,IER)
      character*(*) PROMPT
      character MOD*1
      LOGICAL CFGOK
      return
      end

C dintervalf: A fortran implementation of the c surboutine dinterval.
      subroutine dintervalf(v1,v2,dv,ndec,mode)
      real v1,v2,dv
      integer ndec,mode
C When 'mode'=1 the hour interval on the graphical time (x-axis) is
C set as follow:
C v=v2-v1 for v < 12 dv=1, v < 18 dv=2, v < 24 dv=3
C             v < 48 dv=6, v < 96 dv=12 else dv=24.
C Should be the same logic as in esru_x.c.
      real v,dvv,x,w
      integer ix
      if(mode.eq.0)then
        vv = v2 - v1
        v = abs(vv)
        x = log10(v)
        ix = nint(x)
        if (x.lt.0.0) ix=ix-2
        dx = real(ix)

        dz = 10.0**dx
        vr =  v / dz
        w = 10.0
        if (vr.lt.5.0) w = 5.0
        if (vr.lt.2.0) w = 2.0

        dvv = w * 0.1 * dz
        if (vv.lt.0.0) dvv = -dvv

        nd = 1 - ix
        if (w.eq.10.0)then
          nd=nd-1
        elseif (w.eq.5.0)then
          nd = 1
        elseif (w.eq.2.0)then
          nd = 2
        endif
        if (nd.lt.0) nd = 0
      else
        v = v2 - v1
        dvv = 168.0
        if (v.lt.4320.0) dvv = 48.0
        if (v.lt.1440.0) dvv = 24.0
        if (v.lt.338.0) dvv = 12.0
        if (v.lt.122.0) dvv = 8.0
        if (v.lt.50.0) dvv = 4.0
        if (v.lt.26.0) dvv = 3.0
        if (v.lt.20.0) dvv = 2.0
        if (v.lt.14.0) dvv = 1.0
        nd = 0
      endif
      dv = dvv
      ndec = nd
      return
      end

C Local variant of command line parsing.
C parcnv parse command line paramters for dfv.
      subroutine parsedfv(termtype,iappw,iappx,iappy,inf,cfd3dasci,
     &  itmin,itmax)
      
      integer lnblnk  ! function definition

      COMMON/OUTIN/IUOUT,IUIN,IEOUT

      integer iargc,m
      integer termtype
      integer i,iverb,itmin,itmax
      real conv
      character argument*72,prog*72,inf*144,outs*248,appn*24
      character cfd3dasci*72,mode*8

      inf  = 'UNKNOWN'
      cfd3dasci = 'UNKNOWN'
      iverb = 0
      itmin=15; itmax=30

C Get number of arguments and command name.
      termtype = 8
      mode = 'graphic'
      m = iargc()
      i = 0
      call getarg(i,prog)
      call findapp(prog,appn)

C If one parameter.
      if(m.eq.0)then
        stop
      elseif(m.ge.1)then
  41    i= i+1
        if(i.gt.m)goto 42
        call getarg(i,argument)
        if(argument(1:5).eq.'-help')then
         call edisp(iuout,'CFD performance viewer. ')
         call edisp(iuout,' Use: -file <zone dfd file> ')
         call edisp(iuout,' -actf <ascii timestep predictions file>')
         call edisp(iuout,' ')
         call edisp(iuout,'Optional tokens: ')
         call edisp(iuout,' -range <min T integer> <max T integer>')
         call edisp(iuout,' -v verbose mode')
         call edisp(iuout,' -help :this help message.')
         call edisp(iuout,' ')
         stop
        elseif(argument(1:2).eq.'-v')then
          iverb = 2   ! tell application to run with debug on
        elseif(argument(1:2).eq.'-s')then
          i=i+1
          call getarg(i,argument)
          read(argument,*,IOSTAT=IOS,ERR=2)iappw
          i=i+1
          call getarg(i,argument)
          read(argument,*,IOSTAT=IOS,ERR=2)iappx
          i=i+1
          call getarg(i,argument)
          read(argument,*,IOSTAT=IOS,ERR=2)iappy
        elseif(argument(1:6).eq.'-range')then
          i=i+1
          call getarg(i,argument)
          read(argument,*,IOSTAT=IOS,ERR=2)itmin
          i=i+1
          call getarg(i,argument)
          read(argument,*,IOSTAT=IOS,ERR=2)itmax
        elseif(argument(1:5).eq.'-file')then
          i=i+1
          call getarg(i,inf)
        elseif(argument(1:5).eq.'-actf')then
          i=i+1
          call getarg(i,cfd3dasci)
        endif
        goto 41

  42    continue
        write(outs,'(4a)') 'Starting dfv with zone dfd file ',
     &    inf(1:lnblnk(inf)),' and CFD ascii performance file ',
     &    cfd3dasci(1:lnblnk(cfd3dasci))
        call edisp248(iuout,outs,100)
        return
      endif

  2   write(6,*) 'error parsing command line'
      return
      end

C Draw a single slice at the I axis.
C ********************* MAKS3DF *********************
C Draw flows in 3d on screen using scaling factors of 
C first view.

      SUBROUTINE MAKS3DFISLICE(IFRAME,ISLICE)
#include "building.h"
#include "cfd.h"

      integer IFRAME,ISLICE

      integer menuchw,igl,igr,igt,igb,igw,igwh
      COMMON/VIEWPX/menuchw,igl,igr,igt,igb,igw,igwh
      common/appcols/mdispl,nifgrey,ncset,ngset,nzonec
      COMMON/VISSET/imgqua,NOX,NOZ,tscal,hLscal,hWscal,vLscal
      COMMON/TEMPf/Tf(ntcelx,ntcely,ntcelz),GAMH(ntcelx,ntcely,ntcelz),
     1             RESORT,NSWPT,URFT,FSDTT,PRANDL,PFUN
      COMMON/SIMPIK/ISIM,ISTADD,ID1,IM1,ID2,IM2,ISDS,ISDF,NTS,ISAVE
      COMMON/cfdsmper/ICDYS,ICDYF,CFTS,CFTF
      COMMON/cfdotper/ICDYOS,ICDYOF,CFTOS,CFTOF
      common/flvcol/ISOPT(6),IBGOPT(6),SCmin,SCmax
      COMMON/ALL/NI,NJ,NK,NIM1,NJM1,NKM1,NIM2,NJM2,NKM2
      common/vecXYZ/vecXbeg(ntcelx,ntcely,ntcelz),
     1              vecXend(ntcelx,ntcely,ntcelz),
     2              vecYbeg(ntcelx,ntcely,ntcelz),
     3              vecYend(ntcelx,ntcely,ntcelz),
     4              vecZbeg(ntcelx,ntcely,ntcelz),
     5              vecZend(ntcelx,ntcely,ntcelz)

C To support stand-alone display via DFV.
      integer ISIMDAY ! Julian day within a cfd3dasci file
      real SIMTIMEF   ! time associated with a cfd3dasci file
      COMMON/DFVtime/ISIMDAY,SIMTIMEF

      dimension Vstart(3),Vend(3),IC(6,2)
#ifdef OSI
      integer iside,isize,ifont     ! passed to viewtext
      integer iicol
      integer iid2,iid4
      integer iright      ! passed to eswline
      integer iimenu
#else
      integer*8 iside,isize,ifont     ! passed to viewtext
      integer*8 iicol
      integer*8 iid2,iid4
      integer*8 iright      ! passed to eswline
      integer*8 iimenu
#endif
      integer iiright   !passed to wstxpt
      logical colok,cscaleok
      real tempmin,tempmax,trange,colmult,NOW,FRACH
      character souts*48
      CHARACTER MTHNAM(12)*4,PDESCR*70

      DATA MTHNAM/' Jan',' Feb',' Mar',' Apr',' May',' Jun',' Jul',
     &            ' Aug',' Sep',' Oct',' Nov',' Dec'/
      
C Check if can draw in colour and with a range of colours.
      colok=.false.
      if(nzonec.ge.24)colok=.true.
      cscaleok=.false.
      if(ncset.gt.24)cscaleok=.true.
      
C Set a range of temps and colours similar to clmpsychart.F
C      write(6,*) 'within MAKS3D SCmin,SCmax',SCmin,SCmax
      tempmin=SCmin; tempmax=SCmax; trange=tempmax-tempmin
      colmult=(real(ncset)/trange)

C Need to reset scale factors - do this by redrawing the domain.
      iz=0

C Draw the domain prior to the arrows.
      call VGRID3D(0,2) ! draw only north and south face grid
C      write(6,*) 'imgqua,NOX,NOZ,tscal,hLscal,hWscal,vLscal'
C      write(6,*) imgqua,NOX,NOZ,tscal,hLscal,hWscal,vLscal
C Loop through all vectors at slice I.
      I=ISLICE
      do 101 J=2,NJM1
        do 102 K=2,NKM1
          Vstart(1)=vecXbeg(i,j,k)
          Vend(1)  =vecXend(i,j,k)
          Vstart(2)=vecYbeg(i,j,k)
          Vend(2)  =vecYend(i,j,k)
          Vstart(3)=vecZbeg(i,j,k)
          Vend(3)  =vecZend(i,j,k)
          temp=Tf(i,j,k) ! current cell temperature

C Scale vector length.
          do 104 IV=1,3
            SUM=Vstart(IV)+Vend(IV)
            DIF=(Vend(IV)-Vstart(IV))*vLscal
            Vstart(IV)=(SUM-DIF)/2.
            Vend(IV)  =(SUM+DIF)/2.
 104      continue
          hsf=hWscal/10.

C Setup colour for current cell arrow.
          icolindex=nint((tempmax-temp)*colmult)
C          write(6,*) 
C     &      'Vstart,Vend,temp,hsf,icolindex ',
C     &      i,j,k,Vstart,Vend,temp,hsf
          if(icolindex.le.0) icolindex = 1
          if(icolindex.gt.ncset) icolindex = ncset
          if(cscaleok)then
            iicol=icolindex
            call winscl('c',iicol)
          else
            iicol=0
            call winscl('-',iicol)
          endif

C Draw the arrow.
          call arrow(Vstart,Vend,0.5,hsf,IC,'r',2)
          call forceflush()
 102    continue
        call pausems(10)
 101  continue
      iicol=0  ! set back to black
      call winscl('-',iicol)
      call forceflush()

C Draw 10 steps along colour range.
C Write a title for the plot.
C Figure out the current time!  If IFRAME is zero then subroutine
C called within DFV so use alternative variables.
      if(IFRAME.eq.0)then
        CALL EDAYR(ISIMDAY,IDs,IMs)
        NOW=SIMTIMEF
C        write(6,*) 'IFRAME,IDs,IMs,NOW',IFRAME,IDs,IMs,NOW
        write(PDESCR,'(a,i3,2a,f5.2)')
     &    ' On',IDs,MTHNAM(IMs),' @',NOW
      else
        CALL EDAYR(ICDYOS,IDs,IMs)
        CALL EDAYR(ICDYOF,IDf,IMf)
        FRACH=1.0/float(NTS)  ! what part of hour
        NOW=CFTOS+((IFRAME-1)*FRACH)
        if(NOW.gt.24.1) NOW=0.0
C        write(6,*) 'IFRAME,FRACH,CFTOS,NOW',IFRAME,FRACH,CFTOS,NOW
        write(PDESCR,'(a,i3,2a,f5.2,a,i3,2a,f5.2,a,f5.2,a,f5.2)')
     &    ' During',IDs,MTHNAM(IMs),', hr ',CFTOS,' to',IDf,MTHNAM(IMf),
     &    ', hr ',CFTOF,' @',NOW
      endif
      iside=1 ! 1st line
      isize=0
      ifont=1
      call viewtext(PDESCR,iside,isize,ifont)
      write (souts,'(a,F5.1,a,F5.1)') ' Velocity vectors TdegC ',
     &  tempmin,'-T',tempmax
      iside=2 ! 2nd line
      isize=0
      ifont=1
      call viewtext(souts,iside,isize,ifont)
      iid2=igt-15      ! near middle height of text 
C      iright=igl+165   ! to right of text
      iright=igl+195   ! to right of text
      ra=trange/10.0
      temp=tempmin
      do loop=1,11
        if(loop.eq.1)then
          icolindex=nint((tempmax-temp)*colmult)
        else
          temp=temp+ra
          icolindex=nint((tempmax-temp)*colmult)
        endif
        if(icolindex.le.0) icolindex = 1
        if(icolindex.gt.ncset) icolindex = ncset
C        write(6,*) 'temp ',temp,icolindex
        if(cscaleok)then
          iicol=icolindex
          call winscl('c',iicol)
        else
          iicol=0
          call winscl('-',iicol)
        endif
        iright=iright+10
        call ecirc(iright,iid2,4,0)
        call forceflush()
      enddo

      return
      end

C Local version with fixed view point.
C ********************* MAKS3DF *********************
C MAKS3DF - draw flows in 3d on screen using scaling factors of 
C first view.
      SUBROUTINE MAKS3DF(IFRAME)
#include "building.h"
#include "cfd.h"

      integer menuchw,igl,igr,igt,igb,igw,igwh
      COMMON/VIEWPX/menuchw,igl,igr,igt,igb,igw,igwh
      common/appcols/mdispl,nifgrey,ncset,ngset,nzonec
      COMMON/VISSET/imgqua,NOX,NOZ,tscal,hLscal,hWscal,vLscal
      COMMON/TEMPf/Tf(ntcelx,ntcely,ntcelz),GAMH(ntcelx,ntcely,ntcelz),
     1             RESORT,NSWPT,URFT,FSDTT,PRANDL,PFUN
      COMMON/SIMPIK/ISIM,ISTADD,ID1,IM1,ID2,IM2,ISDS,ISDF,NTS,ISAVE
      COMMON/cfdsmper/ICDYS,ICDYF,CFTS,CFTF
      COMMON/cfdotper/ICDYOS,ICDYOF,CFTOS,CFTOF
      common/flvcol/ISOPT(6),IBGOPT(6),SCmin,SCmax
      COMMON/ALL/NI,NJ,NK,NIM1,NJM1,NKM1,NIM2,NJM2,NKM2
      common/vecXYZ/vecXbeg(ntcelx,ntcely,ntcelz),
     1              vecXend(ntcelx,ntcely,ntcelz),
     2              vecYbeg(ntcelx,ntcely,ntcelz),
     3              vecYend(ntcelx,ntcely,ntcelz),
     4              vecZbeg(ntcelx,ntcely,ntcelz),
     5              vecZend(ntcelx,ntcely,ntcelz)

C Saved state of the visualisation
      COMMON/MODVIS/IVISMOD
      COMMON/CFDVIS/HAS_GEOM,ISHSB,ISHAO,IFACES,ISHBLK,ISHSRC,ISHGEO,
     &              INITD
      logical HAS_GEOM
      character INITD*6

C To support stand-alone display via DFV.
      integer ISIMDAY ! Julian day within a cfd3dasci file
      real SIMTIMEF   ! time associated with a cfd3dasci file
      COMMON/DFVtime/ISIMDAY,SIMTIMEF

      dimension Vstart(3),Vend(3),IC(6,2)
#ifdef OSI
      integer iside,isize,ifont     ! passed to viewtext
      integer iicol
      integer iid2,iid4
      integer iright      ! passed to eswline
      integer iimenu
#else
      integer*8 iside,isize,ifont     ! passed to viewtext
      integer*8 iicol
      integer*8 iid2,iid4
      integer*8 iright      ! passed to eswline
      integer*8 iimenu
#endif
      integer iiright   !passed to wstxpt
      logical colok,cscaleok
      real tempmin,tempmax,trange,colmult,NOW,FRACH
      character souts*48
      CHARACTER MTHNAM(12)*4,PDESCR*70

      DATA MTHNAM/' Jan',' Feb',' Mar',' Apr',' May',' Jun',' Jul',
     &            ' Aug',' Sep',' Oct',' Nov',' Dec'/
      
C Check if can draw in colour and with a range of colours.
      colok=.false.
      if(nzonec.ge.24)colok=.true.
      cscaleok=.false.
      if(ncset.gt.24)cscaleok=.true.
      
C Set a range of temps and colours similar to clmpsychart.F
C      write(6,*) 'within MAKS3D SCmin,SCmax',SCmin,SCmax
      tempmin=SCmin; tempmax=SCmax; trange=tempmax-tempmin
      colmult=(real(ncset)/trange)

C Need to reset scale factors - do this by redrawing the domain.
      iz=0

C Draw the domain prior to the arrows.
      ISHSB=0; ISHAO=0; IFACES=1; ISHBLK=0; ISHSRC=0; ISHGEO=0
      call redraw(IER)
C      write(6,*) 'imgqua,NOX,NOZ,tscal,hLscal,hWscal,vLscal'
C      write(6,*) imgqua,NOX,NOZ,tscal,hLscal,hWscal,vLscal
C Loop through all vectors. To the I look from right to left
C so as to see more of the intermediate arrows.
C      do 10 I=2,NIM1
      do 10 I=NIM1,2,-1
        do 101 J=2,NJM1
          do 102 K=2,NKM1
            Vstart(1)=vecXbeg(i,j,k)
            Vend(1)  =vecXend(i,j,k)
            Vstart(2)=vecYbeg(i,j,k)
            Vend(2)  =vecYend(i,j,k)
            Vstart(3)=vecZbeg(i,j,k)
            Vend(3)  =vecZend(i,j,k)
            temp=Tf(i,j,k) ! current cell temperature

C Scale vector length.
            do 104 IV=1,3
              SUM=Vstart(IV)+Vend(IV)
              DIF=(Vend(IV)-Vstart(IV))*vLscal
              Vstart(IV)=(SUM-DIF)/2.
              Vend(IV)  =(SUM+DIF)/2.
 104        continue
            hsf=hWscal/10.

C Setup colour for current cell arrow.
            icolindex=nint((tempmax-temp)*colmult)
C            write(6,*) 
C     &        'Vstart,Vend,temp,hsf,icolindex ',
C     &        i,j,k,Vstart,Vend,temp,hsf
            if(icolindex.le.0) icolindex = 1
            if(icolindex.gt.ncset) icolindex = ncset
            if(cscaleok)then
              iicol=icolindex
              call winscl('c',iicol)
            else
              iicol=0
              call winscl('-',iicol)
            endif

C Draw the arrow.
            call arrow(Vstart,Vend,0.5,hsf,IC,'r',2)
            call forceflush()
 102      continue
          call pausems(10)
 101    continue
        iicol=0  ! set back to black
        call winscl('-',iicol)
      call pausems(20)
 10   continue
      call forceflush()

C Draw 10 steps along colour range.
C Write a title for the plot.
C Figure out the current time!  If IFRAME is zero then subroutine
C called within DFV so use alternative variables.
      if(IFRAME.eq.0)then
        CALL EDAYR(ISIMDAY,IDs,IMs)
        NOW=SIMTIMEF
C        write(6,*) 'IFRAME,IDs,IMs,NOW',IFRAME,IDs,IMs,NOW
        write(PDESCR,'(a,i3,2a,f5.2)')
     &    ' On',IDs,MTHNAM(IMs),' @',NOW
      else
        CALL EDAYR(ICDYOS,IDs,IMs)
        CALL EDAYR(ICDYOF,IDf,IMf)
        FRACH=1.0/float(NTS)  ! what part of hour
        NOW=CFTOS+((IFRAME-1)*FRACH)
        if(NOW.gt.24.1) NOW=0.0
C        write(6,*) 'IFRAME,FRACH,CFTOS,NOW',IFRAME,FRACH,CFTOS,NOW
        write(PDESCR,'(a,i3,2a,f5.2,a,i3,2a,f5.2,a,f5.2,a,f5.2)')
     &    ' During',IDs,MTHNAM(IMs),', hr ',CFTOS,' to',IDf,MTHNAM(IMf),
     &    ', hr ',CFTOF,' @',NOW
      endif
      iside=2 ! 2nd line
      isize=0
      ifont=1
      call viewtext(PDESCR,iside,isize,ifont)
      write (souts,'(a,F5.1,a,F5.1)') ' Velocity vectors T ',
     &  tempmin,'-T',tempmax
      iside=3 ! 3rd line
      isize=0
      ifont=1
      call viewtext(souts,iside,isize,ifont)
      iid2=igt-15      ! near middle height of text 
      iright=igl+165   ! to right of text
      ra=trange/10.0
      temp=tempmin
      do loop=1,11
        if(loop.eq.1)then
          icolindex=nint((tempmax-temp)*colmult)
        else
          temp=temp+ra
          icolindex=nint((tempmax-temp)*colmult)
        endif
        if(icolindex.le.0) icolindex = 1
        if(icolindex.gt.ncset) icolindex = ncset
C        write(6,*) 'temp ',temp,icolindex
        if(cscaleok)then
          iicol=icolindex
          call winscl('c',iicol)
        else
          iicol=0
          call winscl('-',iicol)
        endif
        iright=iright+10
        call ecirc(iright,iid2,4,0)
        call forceflush()
      enddo

      return
      end

      subroutine redrawbuttons()
      return
      end

C Subroutine dummy in case of GTK.

C      subroutine chgeye(EVX,EVY,EVZ,VX,VY,VZ,EAN,JITZNM,JITSNM,JITVNO,
C     &   JITOBS,JITSNR,JITGRD,JITORG,DIS,JITBND,JITDSP,JITHLS,JITHLZ,
C     &   JITPPSW)

C Passed paramters.
C      real EVX,EVY,EVZ,VX,VY,VZ,EAN,DIS

C Depending on computer type set integer size of passed parameters.
C#ifdef OSI
C      integer JITZNM,JITSNM,JITVNO,JITOBS,JITVIS,JITVOBJ,JITSNR,JITGRD
C      integer JITORG,JITBND,JITDSP,JITHLS,JITHLZ,JITPPSW
C#else
C      integer*8 JITZNM,JITSNM,JITVNO,JITOBS,JITVIS,JITVOBJ,JITSNR
C      integer*8 JITGRD,JITORG,JITBND,JITDSP,JITHLS,JITHLZ,JITPPSW
C#endif     
C      return
C      end

C      subroutine chgzonpik(jizgfoc,jnzg)
C#ifdef OSI
C      integer jizgfoc,jnzg  ! for use with viewtext
C#else
C      integer*8 jizgfoc,jnzg  ! for use with viewtext
C#endif
C      return
C      end

C      subroutine chgzonpikarray(jnznog,jnznogv)
C#ifdef OSI
C      integer jnznog,jnznogv
C#else
C      integer*8 jnznog,jnznogv
C#endif
C      return
C      end


