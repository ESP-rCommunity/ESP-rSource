C VIEWER MAIN PROGRAM

      program viewer
#include "viewer.h"
#include "viewer.h2"
      COMMON/OUTIN/IUOUT,IUIN
      common/pophelp/h(60)
      COMMON/SPAD/MMOD,LIMIT,LIMTTY
      COMMON/VIEWPX/menuchw,igl,igr,igt,igb,igw,igwh
      common/appcols/mdispl,nifgrey,ncset,ngset,nzonec
      COMMON/GFONT/IFS,ITFS,IMFS
      COMMON /FIL/ IFUSE, IFO
      common/appw/iappw,iappx,iappy

C Path to problem and command line file (if any).
      common/rpath/path
      common/rcmd/LCMDFL
      logical     unixok

      character outs*124,inf*144,path*72,LCMDFL*144,H*72,fs*1
      character outs248*248

C Viewer works as follows:
C call input which initialises everything and reads in
C   the data file.
C call chat which is the interface controller.
C call plot which does most everything else.

C Initialise terminal and begin program.
      call ezero
      call curmodule('vew ')
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
        iappw=560
        iappx=60
        iappy=170
      else
        if(iappx.le.0)iappx=60
        if(iappy.le.0)iappy=170
        if(iappw.le.200)then
          iappwi=int(560*iappw*0.01)
          iappw=iappwi
        elseif(iappw.gt.200)then
          continue
        endif
      endif

C Set pixels high to iappw and pixels wide to factor in monitor size.
      iapphi=iappw
      iappwi=int(real(iappw)*(800.0/780.0))

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

C Set initial font sizes (IMFS is for graphs, IFS is for dialog & text feedback).
        IMFS=0
        IFS=0
        ITFS=0
        call userfonts(IFS,ITFS,IMFS)
      ELSE
        LIMTTY=16
        LIMIT =16
      ENDIF
      CALL EPAGES(MMOD,IUIN,IUOUT,iappwi,iapphi,iappx,iappy,menuchw,
     &  'ESP-r Viewing Facility: enquiries to esru@strath.ac.uk')

C Open the text display box equal to LIMTTY if MMOD = 8.
      IF(MMOD.EQ.8)THEN
        CALL win3d(menuchw,1,1,1,1,igl,igr,igt,igb,igw,igwh)
#ifdef X11
        call opensetup
        call updazi(1)
#endif
        mdispl=0
        nifgrey=0
        ncset=0
        ngset=0
        nzonec=0
        call foundcolour(mdispl,nifgrey,ncset,ngset,nzonec)
        call startbuffer()
      ENDIF

      CALL EDISP(iuout,' viewer - Version 1.5a of January 2006.')
      call edisp(iuout,' Max no of:  points  surfaces  edges  bodies ')
      write(outs,'(11x,I8,I9,I8,I8)')MPT,MSF,MED,MBB
      call edisp(iuout,outs)

C Take command line file name as initail viewer file.
      if(inf(1:2).ne.'  '.and.inf(1:4).ne.'UNKN')then
        LCMDFL=inf
        write(outs248,'(a,a)')' the input file is: ',LCMDFL
        call edisp248(iuout,outs248,80)
      else
        LCMDFL=' '
      endif

C init =0 INITIALISE INPUT, 1 ADD MORE DATA.
      init=0
      call input(init)

      call chat
      CALL EPAGEND
      stop

      end

C CTLEXP *********** (cut back version from prj.F)
C CTLEXP: Control feedback to export wireframe or text feedback area.
C Assign user-specified export file - checking if local or remote.
C tg is a character T or G specifying text or graphic 
C info being saved.
      subroutine ctlexp(xfile,ixopen,ixloc,ixunit,tg,msg,IER)

      COMMON/OUTIN/IUOUT,IUIN
      common/pophelp/h(60)
      common/rpath/path

      character*(*) msg
      character H*72,xfile*72,path*72,outs*124,tg*1,tfile*72
      character longtfile*144
      logical concat

C Each call is a toggle. If open then close file. 
      ixopen=ixopen+1
      if(ixopen.GT.1)ixopen=0
      if(ixopen.eq.0)then
        write(outs,'(a,a)') 'closing export file: ',xfile
        call usrmsg(outs,' ','-')
        if(tg.eq.'G')then
          if(ixloc.eq.1)then
            longtfile=' '
            call addpath(xfile,longtfile,concat)
          else
            write(longtfile,'(a)') xfile(1:lnblnk(xfile))
          endif
          call wwcsetend
          call wwcclose(longtfile)
        endif
      elseif(ixopen.eq.1)then
        ixloc=0
        if(path.ne.'./'.and.path.ne.' ')then
           write(outs,'(A,A)') ' The current path is: ',path
           call edisp(iuout,outs)
         h(1)='If using path then name appended to path. Otherwise'
         h(2)='file put in folder where application was started.'
         h(3)='If `local` choice you may also give an absolute'
         h(4)='addressed file name ie. /tmp/junk.ww '
           CALL EASKAB(' The problem is in a remote folder... ',
     &      ' place export file:','using current path',
     &      'in local (or absolute) folder',ixloc,4)
        endif
        h(1)='This file is a text file which can be used in'
        h(2)='reports or for third party tools. '
        call easks(xfile,' export file name: ',' ',72,
     &    ' ','export file name',IER,2)

        write(outs,'(3A)')' opened ',xfile(1:lnblnk(xfile)),
     &                    ' for export.'
        call usrmsg(outs,' ','p')
        if(ixloc.eq.1)then
          if(tg.eq.'G')then

C Writing remotely, add the path to the given file name before
C passing request to wwlib.c
            longtfile=' '
            call addpath(xfile,longtfile,concat)
            call wwcopen(longtfile)
            call wwcsetstart
          endif
        else
          if(tg.eq.'G')then
            write(longtfile,'(a)') xfile(1:lnblnk(xfile))
            call wwcopen(longtfile)
            call wwcsetstart
          endif
        endif
        write(outs,'(1x,3A)') msg(1:lnblnk(msg)),
     &    ' >> to ',xfile(1:lnblnk(xfile))
        call usrmsg(outs,' ','p')
      endif
      RETURN
      END

      subroutine chgazi(icazi)
#include "viewer.h"
      COMMON/OUTIN/IUOUT,IUIN
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

      subroutine chgelev(icelev)
#include "viewer.h"
      COMMON/OUTIN/IUOUT,IUIN
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
      SUBROUTINE ADJVIEW(ITRC,IER)
      ier=0
      return
      end

      SUBROUTINE EMKVIEW(ITRC,IUO,CFGOK,IER)
      logical cfgok
      ier=0
      return
      end

      SUBROUTINE EPKMLC(ISEL,PROMPT1,PROMPT2,IER)
      CHARACTER*(*) PROMPT1,PROMPT2
      ier=0
      return
      end

      SUBROUTINE INLNST(ITYP)
      return
      end

      SUBROUTINE PLELEV(IUF,direc,ier)
      CHARACTER direc*1
      ier=0
      return
      end
