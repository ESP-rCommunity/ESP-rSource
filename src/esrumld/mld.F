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

C You should have received a copy of the GNU General Public
C License along with ESP-r. If not, write to the Free
C Software Foundation, Inc., 59 Temple Place, Suite 330,
C Boston, MA 02111-1307 USA.

      program mld
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

      common/OUTIN/IUOUT,IUIN
      common/SHOUT/ICOUT
      common/FILEP/IFIL
      common/SPAD/MMOD,LIMIT,LIMTTY
      common/appw/iappw,iappx,iappy
      common/pophelp/h(60)
      integer menuchw,igl,igr,igt,igb,igw,igwh
      common/VIEWPX/menuchw,igl,igr,igt,igb,igw,igwh
      integer ifs,itfs,imfs
      common/GFONT/IFS,ITFS,IMFS
      common/funit/iucfg, iupdb
      common/appcols/mdispl,nifgrey,ncset,ngset,nzonec

C Path to problem and command line file (if any).
      common/rpath/path
      common/rcmd/LCMDFL

C Defaults.
      common/DEFLT2/DFCFG,DFCTL,DEFRLB,DAFRES,DAPROB,DPNF

C Name of current application
      common/APPNAME/cAppName
      
      character cAppName*12

      character*72 DFCFG,DFCTL,DEFRLB,DAPROB,DAFRES,DPNF
      character*29 ITEM(10)
      character LCMDFL*144,inf*144,H*72,mldnam*48
      character path*72
      character species*64, grow*1, head*29,fs*1
      logical unixok

      integer iglib   ! if 1 then X11, if 2 then GTK, if 3 then text only.
      integer nitem,ino ! max items and current menu item

C For help messages
      character helpinsub*24 ! subroutine name
      character helptopic*24 ! string (unique) for topic
      integer nbhelp     ! number of help lines found (will be non-zero
                         ! because default message will be created

C Initialize global common data.
      call ezero
      call curmodule('mld ')
      cAppName = 'mld'
      helpinapp='mld'  ! set once for the application
      helpinsub='mld'  ! set for MAIN
      IUOUT=6
      IUIN=5
      IFIL=10
      IFS=1
      ITFS=1
      IMFS=1
      iopmld=0
      call parpsf(MODL,iappw,iappx,iappy,inf)

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
        iappw=520
        iappx=100
        iappy=150
      else
        if(iappx.le.0)iappx=100
        if(iappy.le.0)iappy=150
        if(iappw.le.200)then
          iappwi=int(520*iappw*0.01)
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
     &'ESP-r Mould Analysis: enquiries to esru@strath.ac.uk')

C Open the text display box equal to LIMTTY if MMOD = 8.
      IF(MMOD.EQ.8)THEN
        CALL win3d(menuchw,12,12,4,3,igl,igr,igt,igb,igw,igwh)
        igw=igr-igl
        igwh=igb-igt
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

C Set additional output units to stdout. Then redirect warning
C messages to stderr in case of rule script program control.
      ICOUT=IUOUT
      IF(MMOD.EQ.-6) ICOUT=0

      CALL ESPrVersion("summary",cAppName,IUOUT)

C Initialise the ststistical results
      imstat=0

  2   head='mould growth main menu'
      ino=-1
      item(1)='a Select mould species '
      item(2)='  ---------------      '
      if(imstat.eq.0) then
        item(3)='c Statistics >> OFF  '
      else
         item(3)='c Statistics >> ON  '
      endif
      item(4)='d Graphs               '
      item(5)='                       '
      item(6)='---------------        '
      item(7)='? Help                 '
      item(8)='- Exit                 '
      nitem=8

C Use askabout to instanciate the initial help messages (2nd parameter is one).
      call askabout('mld ',1)

      call emenu(head,item,nitem,ino)

      if(ino.eq.1) then

C Recover mould names and display menu of moulds.
       call mycorecv(ino,1)

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

      subroutine chgazi(icazi)
      return
      end

      subroutine chgelev(icelev)
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


