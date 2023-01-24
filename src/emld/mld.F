C This file is part of the ESP-r system.
C Copyright Energy Systems Research Unit, University of
C Strathclyde, Glasgow Scotland, 2001-.

C ESP-r is free software. You can redistribute it and/or
C modify it under the terms of the GNU General Public
C License as published by the Free Software Foundation 
C (version 2 or later).

C ESP-r is distributed in the hope that it will be useful
C but WITHOUT ANY WARRANTY; without even the implied
C warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
C PURPOSE. See the GNU General Public License for more
C details.


      program mld
      USE START_UP
#include "building.h"
#include "model.h"
#include "mould.h"
#include "esprdbfile.h"
#include "espriou.h"

      common/minfo/mno(MOULD),species(MOULD)
      common/meqn/a0(MOULD),a1(MOULD),a2(MOULD),a3(MOULD)
      common/mglook/GROT(MLOOK,MOULD),GRORH(MLOOK,MOULD)
      common/mglook2/GROW(MLOOK,MOULD)
      common/moupk/ispp(mould),a(4,mould),mldnam(mould)
      common/mstats/imstat,ifloat(mould),ifloatp(mould),imdur1(mould)
     &,imdur0(mould),i1max(mould),i0max(mould),imup(mould),
     &imdn(mould)
      common/iopnmld/iopmld
      common/mdbase/mdbnam
      character mdbnam*144

      common/OUTIN/IUOUT,IUIN,IEOUT
      common/SHOUT/ICOUT
      common/FILEP/IFIL
      common/SPAD/MMOD,LIMIT,LIMTTY
      common/appw/iappw,iappx,iappy
      integer menuchw,igl,igr,igt,igb,igw,igwh
      common/VIEWPX/menuchw,igl,igr,igt,igb,igw,igwh
      integer ifs,itfs,imfs
      common/GFONT/IFS,ITFS,IMFS
      common/funit/iucfg, iupdb
      common/appcols/mdispl,nifgrey,ncset,ngset,nzonec

C Path to problem and command line file (if any).
      common/rpath/path
      common/rcmd/LCMDFL

C Session log file (for errors etc.)
      character iefile*72
      common/logs/iefile

C Defaults.
      character*96 DFCFG,DFCTL,DEFRLB,DAPROB,DAFRES,DPNF
      common/DEFLT2/DFCFG,DFCTL,DEFRLB,DAFRES,DAPROB,DPNF

C Name of current application
      common/APPNAME/cAppName
      character cAppName*12

C Ask to overwrite flag.
      COMMON/OVRWT/AUTOVR
      logical AUTOVR

      character*29 ITEM(10)
      character LCMDFL*144,inf*144,mldnam*48
      character path*72
      character species*64, grow*1, head*29,fs*1
      character dstmp*24,uname*24,tfile*72

      character cVnum*38      ! returned from ESPrVersionNum
      character pagestitle*68 ! for banner title via epages call

      logical unixok

#ifdef OSI
      integer iigl,iigr,iigt,iigb,iigw,iigwh
      integer iiw1,iiw2,iiw3,iiw4,iimenu
#else
      integer*8 iigl,iigr,iigt,iigb,iigw,iigwh
      integer*8 iiw1,iiw2,iiw3,iiw4,iimenu
#endif

      integer iglib   ! if 1 then X11, if 2 then GTK, if 3 then text only.
      integer nitem,ino ! max items and current menu item

C Initialize global common data.
      call ezero
      call curmodule('mld ')
      cAppName = 'mld'
      helpinapp='mld'  ! set once for the application
      IUOUT=6
      IUIN=5
      IFIL=10
      IFS=1
      ITFS=1
      IMFS=1
      iopmld=0
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

C Set unit numbers for configuration file and plant data
C base files respectively.
      iucfg = 7
      iupdb = 8

C Determine terminal type and set write unit to stderr for rule scripts.
      MODL=8
      MMOD=MODL
      if(iappw.eq.0.and.iappx.eq.0.and.iappy.eq.0)then
        iappw=540
        iappx=100
        iappy=150
      else
        if(iappx.le.0)iappx=100
        if(iappy.le.0)iappy=150
        if(iappw.le.200)then
          iappwi=int(540*iappw*0.01)
          iappw=iappwi
        elseif(iappw.gt.200)then
          continue
        endif
      endif

C Set pixels high to iappw and pixels wide to factor in monitor size.
      iapphi=iappw
      iappwi=int(real(iappw)*(1024.0/780.0))

      if(iappw.gt.0.and.iappw.lt.100)then
        menuchw = MAX0(int(32*iappw*0.01),24)
        LIMTTY=10
        LIMIT =10
      else
        menuchw = 32
        LIMTTY=10
        LIMIT =10
      endif
      IF(MMOD.EQ.8)THEN

C Set initial font sizes (IMFS is for menus, IFS is for dialog & ITFS text feedback).
C Fonts 4-7 are proportional and 0-3 are fixed width. Use proportional for menus
C and dialog.
        IMFS=5
        IFS=4
        ITFS=4
#ifdef OSX
        IMFS=4
        IFS=4  ! use a smaller fonts
        ITFS=4
#endif
        call userfonts(IFS,ITFS,IMFS)
        call defaultfonts(IFS,ITFS,IMFS)  ! and remember these as defaults
      ELSE
        IMFS=5 ! Text mode set LIMTTY larger for paging menus.
        IFS=4
        ITFS=4
        LIMTTY=30
        LIMIT =30
      ENDIF

C Find the current ESP-r version number and add it to application title.
      call ESPrVersionNum(cVnum)
      write(pagestitle,'(2a)') 'Mould analysis of ESP-r ',
     &  cVnum(1:lnblnk(cVnum))

      lntitle=lnblnk(pagestitle)
      CALL EPAGES(MMOD,IUIN,IUOUT,iappwi,iapphi,iappx,iappy,menuchw,
     &  pagestitle,lntitle)

C Open the text display box equal to LIMTTY if MMOD = 8.
      IF(MMOD.EQ.8)THEN

C Setup and pass in parameters to win3d.
        iiw1=12; iiw2=12; iiw3=4; iiw4=3; iimenu=menuchw
        iigl=igl; iigr=igr; iigt=igt; iigb=igb; iigw=igw; iigwh=igwh
        CALL win3d(iimenu,iiw1,iiw2,iiw3,iiw4,
     &    iigl,iigr,iigt,iigb,iigw,iigwh)
        igl=int(iigl); igr=int(iigr); igt=int(iigt); igb=int(iigb)
        igw=int(iigw); igwh=int(iigwh)
        iglib = igraphiclib()  ! find out if X11 or GTK or text support only.
        if(iglib.eq.1)then
          call opencpw
          call opensetup
        endif
        mdispl=0
        nifgrey=0
        ncset=0
        ngset=0
        nzonec=0
        call foundcolour(mdispl,nifgrey,ncset,ngset,nzonec)
        call startbuffer()
      ENDIF

C Take `c' command line file name, convert to fortran string LCMDFL.
      if(inf(1:2).ne.'  '.and.inf(1:4).ne.'UNKN')then
        LCMDFL=inf
        write(mdbnam,'(a)') inf(1:lnblnk(inf))
      else
        LCMDFL='UNKNOWN'
        mdbnam='UNKNOWN'
      endif

C Set additional output units to stdout. Then redirect warning
C messages to stderr in case of rule script program control.
      ICOUT=IUOUT
      IF(MMOD.EQ.-6) ICOUT=0

      CALL ESPrVersion("summary",cAppName,IUOUT)

C Create and open a session log file based on user name, PID etc.
      uname=' '; tfile=' '
      call usrname(uname)
      call esppid(ipid)
        write(tfile,'(a,a1,a,a,i7,a)')upath(1:lnblnk(upath)),fs,'.',
     &    uname(1:lnblnk(uname)),ipid,'.log'
      call st2file(tfile,iefile)

C Open the session file.
      ieout=ifil+932          ! set to unused index 942
      open(ieout,file=iefile,status='UNKNOWN',err=903)
      write(ieout,'(a)')'Session log for mld'
      call dstamp(dstmp) ! get curret time
      write(ieout,'(2a)')'Date,',dstmp
      write(ieout,'(2a)')'User,',uname(1:lnblnk(uname))
  903 continue

C Initialise the ststistical results
      imstat=0

  2   head='mould growth options'
      ino=-1
      item(1)='a mould species '
      item(2)='  ---------------      '
      if(imstat.eq.0) then
        item(3)='c statistics >> OFF  '
      else
         item(3)='c statistics >> ON  '
      endif
      item(4)='d graphs               '
      item(5)='                       '
      item(6)='---------------        '
      item(7)='? help                 '
      item(8)='- quit module          '
      nitem=8

C Use askabout to instantiate the initial help messages (2nd parameter is one).
      call askabout('mld ',1)

      call emenu(head,item,nitem,ino)

      if(ino.eq.1) then

C Recover mould names and display menu of moulds.
       call mycorecv(ino,1,0)

      elseif(ino.eq.3) then

C Set statistics reporting flag to ON or OFF  .
         if(imstat.eq.0) then
            imstat=1
            call mycostat(1,0.,0.)
         else
            imstat=0
         endif     
      elseif(ino.eq.4) then
        call mycograph
      elseif(ino.eq.7) then

C Help via the askabout facility in esru_ask.F The zero requests display
        call askabout('mld ',0)
        goto 2
      elseif(ino.eq.8) then
        close(ieout)
        CALL ERPFREE(ieout,ISTAT)
        CALL EPAGEND
        STOP
      else
         ino=-4
         goto 2
      endif
      goto 2
 
      end

C ***** Dummy display of images.
      subroutine cfgpk(act)
      character act*1

      return
      end

      subroutine imgdisp(iforce,focus,ier)
      character focus*4

      return
      end

C Dummy routines from common3dv.F needed to match implied
C calls in esru_lib.F
      SUBROUTINE ADJVIEW(IER)
      ier=0
      return
      end

      subroutine redraw(IER)
      ier=0
      return
      end

      subroutine chgazi(icazi,ifrlk)
      return
      end

      subroutine chgelev(icelev,ifrlk)
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
     &   JITOBS,JITSNR,JITGRD,JITORG,DIS,JITBND,JITDSP,JITHLS,JITHLZ,
     &   JITPPSW)

C Passed paramters.
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
      integer jizgfoc,jnzg    ! for use with viewtext
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

      SUBROUTINE EMKVIEW(IUO,CFGOK,IER)
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
      return
      end

      SUBROUTINE PLELEV(direc)
      CHARACTER direc*1
      return
      end
      
C dintervalf: A fortran implementation of the c surboutine dinterval.
C Copied from esrucom/esru_misc.F
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

C ******************** EMF3DREAD ********************
C Read a 3D mass flow network description file.

      SUBROUTINE EMF3DREAD(IFPRB,MODE,IER)
      integer IFPRB
      character MODE*1
      integer ier

      return
      end

