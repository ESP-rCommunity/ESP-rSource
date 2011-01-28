C VIEWER MAIN PROGRAM

      program viewer
#include "viewer.h"
#include "viewer.h2"
#include "espriou.h"
      COMMON/OUTIN/IUOUT,IUIN
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
      
      logical     unixok

      character outs*124,inf*144,path*72,LCMDFL*144,fs*1
      character outs248*248

      integer iglib   ! if 1 then X11, if 2 then GTK, if 3 then text only.

C For help messages
      character helpinsub*24 ! subroutine name
      character helptopic*24 ! string (unique) for topic
      integer nbhelp     ! number of help lines found (will be non-zero
                         ! because default message will be created

C Viewer works as follows:
C call input which initialises everything and reads in
C   the data file.
C call chat which is the interface controller.
C call plot which does most everything else.

C Initialise terminal and begin program.
      call ezero
      call curmodule('vew ')
      cAppName = 'vww'
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
        igw=igr-igl
        igwh=igb-igt
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
      ENDIF

      CALL ESPrVersion("summary",cAppName,IUOUT)
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
      SUBROUTINE ADJVIEW(IER)
      ier=0
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

      SUBROUTINE INLNST(ITYP)
      return
      end

      SUBROUTINE PLELEV(direc)
      CHARACTER direc*1
      return
      end

C test subroutinhe to capture help text from external file (to be
C found in the ESP-r distribution lib folder) with file ending
C of *.help
      subroutine gethelptext(sub,topic,nbhelp)
#include "espriou.h"
      
      integer lnblnk  ! function definition

C Parameters.
      character sub*24   ! subroutine name
      character topic*24 ! string (unique) for topic
      integer nbhelp     ! number of help lines found (will be non-zero
                         ! because default message will be created

C Commons.
      integer ifil
      COMMON/FILEP/IFIL
      character h*72
      common/pophelp/h(60)
      integer iuout,iuin
      COMMON/OUTIN/IUOUT,IUIN

C Local variables.
      logical unixok
      integer ln,lna,lnf   ! length of install path, app name app folder
      integer lns,lnt,lnhl ! length of subroutine, topic, helpline
      integer lno          ! length of outstr
      integer K            ! position
      character helpfil*72 ! the help file to load
      character folder*8   ! the source folder for the application
      character WORD*32    ! a token to read
      character outstr*124 ! for help phrases
      character outs*124 ! for messages
      character helpline*72 ! line of help text
      character sq*1        ! single quote
      character fs*1
      logical havesub       ! set true if subroutine matches
      logical havetopic     ! set true if topic matches
      character app*24      ! application name (local)
#include "espinstalldir.h"

C Get to work.
      app = helpinapp      ! set local app
      call isunix(unixok)  ! which folder separator to use
      if(unixok)then
        fs = char(47)
      else
        fs = char(92)
      endif
      ln=lnblnk(instpath)  ! where is ESP-r installed
      lna=lnblnk(app)      ! length of app name
      if(app(1:3).eq.'clm')then
        folder='esruclm'
      elseif(app(1:3).eq.'bps')then
        folder='esrubps'
      elseif(app(1:3).eq.'mld')then
        folder='esrumld'
      elseif(app(1:3).eq.'mrt')then
        folder='esrumrt'
      elseif(app(1:3).eq.'pdb')then
        folder='esrupdb'
      elseif(app(1:3).eq.'vew')then
        folder='esruvew'
      endif
      lnf=lnblnk(folder)   ! length of folder name
      lns=lnblnk(sub)      ! length of subroutine
      lnt=lnblnk(topic)    ! length of topic
      sq=char(39)   ! assign single quote

C Assume that the installer has placed the help text file in the
C distribution lib folder.
      write(helpfil,'(6a)') instpath(1:ln),fs,'lib',fs,folder(1:lnf),
     &  '.help'

      ihfil=ifil+2              ! set file unit number
      CALL ERPFREE(ihfil,ISTAT) ! free it
      CALL EFOPSEQ(ihfil,helpfil,1,IER)
      if(ier.ne.0)then
        call edisp(iuout,'Could not find help text file.')
        call edisp(iuout,helpfil)
        nbhelp=-1
        return
      endif

      nbhelp=0  ! set initial assumption
      havesub= .false.
      havetopic= .false.

C Check the header to see if this is a help text file.
      read(ihfil,'(a)',IOSTAT=IOS,ERR=1) outstr   ! scan header
      if(ios.ne.0) goto 1   ! jump if EOF
      K=0
      CALL EGETW(outstr,K,WORD,'W','*help_text',IER)
      if(WORD(1:10).ne.'*help_text')then
        call edisp(iuout,'Not a help text file.')
        call edisp(iuout,helpfil)
        goto 1
      endif
      CALL EGETW(outstr,K,WORD,'W','application',IER)
      if(WORD(1:lna).ne.app(1:lna))then
        write(outs,'(2a)') 'Not a help text file for ',app(1:lna)
        call edisp(iuout,outs)
        goto 1
      endif

C If we reached this point then we can go looking for a
C subroutine and topic match on each new line.
      scan: do
        read(ihfil,'(a)',IOSTAT=IOS,ERR=1) outstr   ! scan line
        if(ios.ne.0)  exit scan   ! jump if EOF
        lno=lnblnk(outstr)
        if(lno.le.1) cycle  ! jump if a blank line

        if(outstr(1:1).eq.'*')then  ! we had a key phrase
          K=0
          CALL EGETW(outstr,K,WORD,'W','*item',IER)
          if(WORD(1:5).eq.'*item')then
            CALL EGETW(outstr,K,WORD,'W','subroutine',IER)
            if(WORD(1:lns).eq.sub(1:lns))then

C We have a matching subroutine name remember this and if
C there are still more characters in the line then it
C probably is a topic.
              havesub = .true.  ! set true subroutine matches
              if(K.le.lno)then
                CALL EGETW(outstr,K,WORD,'W','topic',IER)
                if(WORD(1:lnt).eq.topic(1:lnt))then
                  havetopic = .true.     ! set true topic matches
                  cycle   ! keep scanning
                else
                  cycle   ! keep scanning
                endif
              else
                cycle   ! keep scanning
              endif
            else
              cycle   ! keep scanning
            endif
          elseif(WORD(1:8).eq.'*enditem')then

C Reached end of item exit scan, if this was the correct sub and topic then
C set number of help lines.
            if(havesub.and.havetopic)then
              CALL ERPFREE(ihfil,ISTAT) ! free it
              return
            else
              cycle  ! keep scanning
            endif
          elseif(WORD(1:6).eq.'*topic')then

C In case there are some old *topic lines in the help f
            if(lno.le.7) cycle  ! jump if no other token
            K=7
            CALL EGETW(outstr,K,WORD,'W','topic',IER)
            if(WORD(1:lnt).eq.topic(1:lnt))then
              havetopic = .true.     ! set true topic matches
              cycle   ! keep scanning
            else
              cycle   ! keep scanning
            endif
          elseif(WORD(1:11).eq.'*subroutine')then
            cycle   ! keep scanning
          endif

        elseif(outstr(1:1).eq.sq)then

C We have a line that starts with a quote. If we have reached the
C correct topic then read it otherwise scan another line.
          if(havesub.and.havetopic)then
            K=0
            call EGETDQ(outstr,K,helpline,'W','phrase',IER)
            lnhl=lnblnk(helpline)
            call edisp(iuout,helpline)
            nbhelp=nbhelp+1
            if(lnhl.le.1)then
              h(nbhelp)='  '
            else
              write(h(nbhelp),'(a)') helpline(1:lnhl)
            endif
            cycle   ! keep scanning
          else
            cycle   ! keep scanning
          endif
        else
 
C Does not match expectations.
          cycle   ! keep scanning
        endif
      end do scan

C The end of file reached. If there has not been a match make up a
C standard message.
  1   close(ihfil)
      if(havesub.and.havetopic)then
        return
      else
        h(1)='No contextual help text was found for the topic'
        write(h(2),'(5a)') topic(1:lnt),' in ',sub(1:lns),
     &    ' of application ',app(1:lna)
        nbhelp=2
        return
      endif

      end
