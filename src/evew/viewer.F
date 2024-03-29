C VIEWER MAIN PROGRAM

      program viewer
      USE START_UP
#include "building.h"
#include "model.h"
#include "viewer.h"
#include "viewer.h2"
#include "espriou.h"
#include "help.h"

      COMMON/OUTIN/IUOUT,IUIN,IEOUT
      COMMON/SPAD/MMOD,LIMIT,LIMTTY
      integer menuchw,igl,igr,igt,igb,igw,igwh
      COMMON/VIEWPX/menuchw,igl,igr,igt,igb,igw,igwh
      common/appcols/mdispl,nifgrey,ncset,ngset,nzonec
      integer ifs,itfs,imfs
      COMMON/GFONT/IFS,ITFS,IMFS
      COMMON /FIL/ IFUSE, IFO
      common/appw/iappw,iappx,iappy

C Path to problem and command line file (if any).
      common/rpath/path
      common/rcmd/LCMDFL

C Name of current application
      common/APPNAME/cAppName
      character cAppName*12

C Ask to overwrite flag.
      COMMON/OVRWT/AUTOVR
      logical AUTOVR

C Session log file (for errors etc.)
      character iefile*72
      common/logs/iefile

      logical     unixok

      character outs*124,inf*144,path*72,LCMDFL*144,fs*1
      character outs248*248
      character dstmp*24,uname*24,tfile*96

      character cVnum*38      ! returned from ESPrVersionNum
      character pagestitle*68 ! for banner title via epages call

#ifdef OSI
      integer iigl,iigr,iigt,iigb,iigw,iigwh
      integer iiw1,iiw2,iiw3,iiw4,iimenu
#else
      integer*8 iigl,iigr,iigt,iigb,iigw,iigwh
      integer*8 iiw1,iiw2,iiw3,iiw4,iimenu
#endif

      integer iglib   ! if 1 then X11, if 2 then GTK, if 3 then text only.

C Viewer works as follows:
C call input which initialises everything and reads in
C   the data file.
C call chat which is the interface controller.
C call plot which does most everything else.

C Initialise terminal and begin program.
      call ezero
      call curmodule('vew ')
      cAppName = 'vew'
      helpinapp='vew'  ! set once for the application
      helpinsub='vew'  ! set for MAIN
      IUIN = 5
      IUOUT = 6
      IFUSE = 7
      IFO = 10
      IFREC = 0
      ITRC=0
      IFS=1
      ITFS=0
      IMFS=0
      LIMTTY=6
      LIMIT =6
      call parpsf(MODL,iappw,iappx,iappy,inf)
      AUTOVR=.false. ! by default, ask to overwrite files

C Set folder separator (fs) to \ or / as required.
      call isunix(unixok)
      if(unixok)then
        fs = char(47)
      else
        fs = char(92)
      endif
      write(path,'(a1,a1)')'.',fs

C Get terminal type passed in command line and setup
C the opeing interface. Default size is iappwi & iapphi.
C If iappw <200 then take this as a percentage size.
      MMOD=MODL
      if(iappw.eq.0.and.iappx.eq.0.and.iappy.eq.0)then
        iappw=580
        iappx=60
        iappy=170
      else
        if(iappx.le.0)iappx=60
        if(iappy.le.0)iappy=170
        if(iappw.le.200)then
          iappwi=int(580*iappw*0.01)
          iappw=iappwi
        elseif(iappw.gt.200)then
          continue
        endif
      endif

C Set pixels high to iappw and pixels wide to factor in monitor size.
      iapphi=iappw
      iappwi=int(real(iappw)*(800.0/740.0))

      if(iappw.gt.0.and.iappw.lt.100)then
        menuchw = MAX0(int(11*iappw*0.01),8)
        LIMTTY=6
        LIMIT =6
      else
        menuchw = 11
        LIMTTY=6
        LIMIT =6
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
        LIMTTY=16
        LIMIT =16
      ENDIF

C Find the current ESP-r version number and add it to application title.
      call ESPrVersionNum(cVnum)
      write(pagestitle,'(2a)') 'Viewing facility of ESP-r',
     &  cVnum(1:lnblnk(cVnum))

      lntitle=lnblnk(pagestitle)
      CALL EPAGES(MMOD,IUIN,IUOUT,iappwi,iapphi,iappx,iappy,menuchw,
     &  pagestitle,lntitle)

C Open the text display box equal to LIMTTY if MMOD = 8.
      IF(MMOD.EQ.8)THEN

C Setup and pass in parameters to win3d.
        iiw1=1; iiw2=1; iiw3=1; iiw4=1; iimenu=menuchw
        iigl=igl; iigr=igr; iigt=igt; iigb=igb; iigw=igw; iigwh=igwh
        CALL win3d(iimenu,iiw1,iiw2,iiw3,iiw4,
     &    iigl,iigr,iigt,iigb,iigw,iigwh)
        igl=int(iigl); igr=int(iigr); igt=int(iigt); igb=int(iigb)
        igw=int(iigw); igwh=int(iigwh)
        iglib = igraphiclib()  ! find out if X11 or GTK or text support only.
        if(iglib.eq.1)then
          call opensetup
          call updazi(1)
        endif
        mdispl=0
        nifgrey=0
        ncset=0
        ngset=0
        nzonec=0
        call foundcolour(mdispl,nifgrey,ncset,ngset,nzonec)
        call startbuffer()
      ELSE
        
C Set reasonable defaults to allow viewer to export xfig views in text mode.
        igw=463; igwh=358
        igl=23; igb=378
      ENDIF

      CALL ESPrVersion("summary",cAppName,IUOUT)
      call edisp(iuout,'  ')  ! echo blank line
      call edisp(iuout,' Max no of:  points  surfaces  edges  bodies ')
      write(outs,'(11x,I8,I9,I8,I8)')MPT,MSF,MED,MBB
      call edisp(iuout,outs)

C Create and open a session log file based on user name, PID etc.
      uname=' '; tfile=' '
      call usrname(uname)
      call esppid(ipid)
        write(tfile,'(4a,i7,a)')upath(1:lnblnk(upath)),fs,'.',
     &    uname(1:lnblnk(uname)),ipid,'.log'
      call st2file(tfile,iefile)

C Open the session file.
      ieout=942          ! set to unused index 942
      open(ieout,file=iefile,status='UNKNOWN',err=903)
      write(ieout,'(a)')'Session log for viewer'
      call dstamp(dstmp) ! get curret time
      write(ieout,'(2a)')'Date,',dstmp
      write(ieout,'(2a)')'User,',uname(1:lnblnk(uname))
  903 continue

C Take command line file name as initail viewer file.
      if(inf(1:2).ne.'  '.and.inf(1:4).ne.'UNKN')then
        LCMDFL=inf
C        call edisp(iuout,'  ')  ! echo blank line
        write(outs248,'(a,a)')'The input file is: ',LCMDFL
C        call edisp248(iuout,outs248,80)
      else
C        call edisp(iuout,'  ')  ! echo blank line
        LCMDFL=' '
      endif

C init =0 INITIALISE INPUT, 1 ADD MORE DATA.
      init=0
      call input(init)

      call chat
      close(ieout)
      CALL ERPFREE(ieout,ISTAT)
      CALL EPAGEND
      stop

      end


      subroutine chgazi(icazi,ifrlk)
#include "viewer.h"
      COMMON/OUTIN/IUOUT,IUIN,IEOUT
      PI = 4.0 * ATAN(1.0)
      tdis= crow(DDDD,CCCC)
      call ln2az(CCCC(1),CCCC(2),CCCC(3),DDDD(1),DDDD(2),DDDD(3),az,el)
      aznew=az + float(icazi)
      RAD = PI/180.
      RYAZI = aznew*RAD
      RSALT = el*RAD
      Z = tdis*SIN(RSALT)
      XYDIS = tdis*COS(RSALT)
      IF (XYDIS .LT. 1E-6)THEN
        X = 0.
        Y = 0.
      ELSE
        X = XYDIS*SIN(RYAZI)
        Y = XYDIS*COS(RYAZI)
      ENDIF
      DDDD(1)=X+CCCC(1)
      DDDD(2)=Y+CCCC(2)
      DDDD(3)=Z+CCCC(3)
      CALL PLTTEK
      return
      end

      subroutine chgelev(icelev,ifrlk)
#include "viewer.h"
      COMMON/OUTIN/IUOUT,IUIN,IEOUT
      PI = 4.0 * ATAN(1.0)
      tdis= crow(DDDD,CCCC)
      call ln2az(CCCC(1),CCCC(2),CCCC(3),DDDD(1),DDDD(2),DDDD(3),az,el)
      elnew=el + float(icelev)
      RAD = PI/180.
      RYAZI = az*RAD
      RSALT = elnew*RAD
      Z = tdis*SIN(RSALT)
      XYDIS = tdis*COS(RSALT)
      IF (XYDIS .LT. 1E-6)THEN
        X = 0.
        Y = 0.
      ELSE
        X = XYDIS*SIN(RYAZI)
        Y = XYDIS*COS(RYAZI)
      ENDIF
      DDDD(1)=X+CCCC(1)
      DDDD(2)=Y+CCCC(2)
      DDDD(3)=Z+CCCC(3)
      CALL PLTTEK
      return
      end

      subroutine chgpan(ix,iy)
      return
      end

      subroutine chgzoom(imode)
      return
      end

      subroutine optview
      return
      end

      subroutine chgsun(isunhour)
      return
      end

      subroutine chgeye(EVX,EVY,EVZ,VX,VY,VZ,EAN,JITZNM,JITSNM,JITVNO,
     &   JITOBS,JITVIS,JITVOBJ,JITSNR,JITGRD,JITORG,GDIS,JITBND,JITDSP,
     &   JITHLS,JITHLZ,JITPPSW)

C Passed parameters.
      real EVX,EVY,EVZ,VX,VY,VZ,EAN,DIS

C Depending on computer type set integer size of passed parameters.
#ifdef OSI
      integer JITZNM,JITSNM,JITVNO,JITOBS,JITVIS,JITVOBJ,JITSNR,JITGRD
      integer JITORG,JITBND,JITDSP,JITHLS,JITHLZ,JITPPSW
#else
      integer*8 JITZNM,JITSNM,JITVNO,JITOBS,JITVIS,JITVOBJ,JITSNR
      integer*8 JITGRD,JITORG,JITBND,JITDSP,JITHLS,JITHLZ,JITPPSW
#endif
      return
      end

      subroutine chgzonpik(jizgfoc,jnzg)
#ifdef OSI
      integer jizgfoc,jnzg  ! for use with viewtext
#else
      integer*8 jizgfoc,jnzg  ! for use with viewtext
#endif
      return
      end

      subroutine chgzonpikarray(jnznog,jnznogv)
#ifdef OSI
      integer jnznog,jnznogv
#else
      integer*8 jnznog,jnznogv
#endif
      return
      end

C ******* LN2AZ  (same as in esru_misc.F)
C Recover azimuth & elevation from unit vector.
      SUBROUTINE LN2AZ(x1,y1,z1,x2,y2,z2,azim,elev)
      logical clx0,cly0,clx1,cly1,clxm1,clym1
      PI = 4.0 * ATAN(1.0)
      RAD = PI/180.
      vdx=x2-x1
      vdy=y2-y1
      vdz=z2-z1
      CROW = vdx**2 + vdy**2 + vdz**2
      CROW=SQRT(CROW)
      if (abs(crow).lt.0.001) then
        return
      endif

C Make vector into unit vector.
      vdx=vdx/crow
      vdy=vdy/crow
      vdz=vdz/crow
      elev=90.0-(acos(vdz)/RAD)

C Reduce the azimuth calculation to a 2D problem.
      dist = vdx**2 + vdy**2
      dist = sqrt(dist)

C Check if tollerably close to an axis.
      CALL ECLOSE(vdx,0.0,0.001,clx0)
      CALL ECLOSE(vdy,0.0,0.001,cly0)
      CALL ECLOSE(vdx,1.0,0.001,clx1)
      CALL ECLOSE(vdy,1.0,0.001,cly1)
      CALL ECLOSE(vdx,-1.0,0.001,clxm1)
      CALL ECLOSE(vdy,-1.0,0.001,clym1)
      if(clx0.and.cly1)then
        azim=0.
      elseif(clx1.and.cly0)then
        azim=90.
      elseif(clx0.and.clym1)then
        azim=180.
      elseif(clxm1.and.cly0)then
        azim=270.
      elseif(vdx.gt.0.0.and.vdy.gt.0.0)then
        azim=acos(vdy/dist)/RAD
      elseif(vdx.gt.0.0.and.vdy.lt.0.0)then
        azim=acos(vdy/dist)/RAD
      elseif(vdx.lt.0.0.and.vdy.lt.0.0)then
        azim=360.0-(acos(vdy/dist)/RAD)
      elseif(vdx.lt.0.0.and.vdy.gt.0.0)then
        azim=360.0-(acos(vdy/dist)/RAD)
      endif
      return
      end

C ***** dummy routine for c code active descriptions buttons.
      subroutine cfgpk(act)
      character act*1

      return
      end

C Dummy routines from common3dv.F
      SUBROUTINE ADJVIEW(IER)
      integer ier
      ier=0
      return
      end

      subroutine redraw(IER)
      ier=0
      return
      end

      SUBROUTINE EMKVIEW(IUO,CFGOK,IER)
      integer IUO,IER
      logical cfgok
      ier=0
      return
      end

      SUBROUTINE EPKMLC(ISEL,PROMPT1,PROMPT2,IER)
      CHARACTER*(*) PROMPT1,PROMPT2
      ier=0
      return
      end
      
      SUBROUTINE EDMLDB2(chgdb,ACTION,isel,IER)
      logical chgdb
      character*1 ACTION
      integer isel,ier
      ier=0
      return
      end

      SUBROUTINE INLNST(ITYP)
      integer ityp
      return
      end

      SUBROUTINE PLELEV(direc)
      CHARACTER direc*1
      return
      end
