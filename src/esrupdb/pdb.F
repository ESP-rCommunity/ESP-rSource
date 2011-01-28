C This file is part of the ESP-r system.
C Copyright Energy Systems Research Unit, University of
C Strathclyde, Glasgow Scotland, 2001-6.

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

      program pdb

#include "building.h"
#include "model.h"
#include "plantdb.h"
#include "esprdbfile.h"
#include "espriou.h"

      integer lnblnk  ! function definition
      integer igraphiclib  ! external definition

C The Plant component DataBase program maintains a
C database containing plant component descriptions.
C The database holds the data needed by ESRUbps to
C allow simulation of plant operation.
C Facilities are provided to enter, list, edit and
C delete component descriptions.
      COMMON/SPAD/MMOD,LIMIT,LIMTTY
      integer menuchw,igl,igr,igt,igb,igw,igwh
      COMMON/VIEWPX/menuchw,igl,igr,igt,igb,igw,igwh

C Defaults.
      COMMON/DEFLT2/DFCFG,DFCTL,DEFRLB,DAFRES,DAPROB,DPNF

C Path to problem and command line file (if any).
      common/rpath/path
      common/rcmd/LCMDFL
      common/appw/iappw,iappx,iappy

      common/OUTIN/IUOUT,IUIN
      integer ifs,itfs,imfs
      COMMON/GFONT/IFS,ITFS,IMFS
      common/FILEP/IFIL
      COMMON/ERRS/ISTAT,IREC
      EQUIVALENCE (ERRFLG, ISTAT)

      common/PCCTL/MPC, NXTREC
      common/PCDATC/NPC,ITMLOC(MAXPC,2)

C Mass flow components data
      common/mfcomp/itpcmp(maxcon), isdcmp(maxcon), isdcnn(maxcon),
     &              supcmp(maxcon,maxflw), mfsdsc(maxcon,maxflw),
     &              ltpcmp(maxcon), isdifc(maxcon)

C Name of current application
      common/APPNAME/cAppName
      
      character cAppName*12

      logical     unixok,OK,DOK,XST
      CHARACTER ZITEM(9)*26 
      CHARACTER PZDESC*25,LTMP*72,cdesc*80,inf*144,L144*144
      character lcmdfl*144,outs*124,outs248*248
      character*72 DFCFG,DFCTL,DEFRLB,DAPROB,DAFRES,DPNF
      character path*72,fs*1
      character mfsdsc*68,ltpcmp*60 

      integer iglib   ! if 1 then X11, if 2 then GTK, if 3 then text only.
      integer ier
      integer NITEMS,INO ! max items and current menu item

C For help messages
      character helpinsub*24 ! subroutine name
      character helptopic*24 ! string (unique) for topic
      integer nbhelp     ! number of help lines found (will be non-zero
                         ! because default message will be created

C Initialise output device, assume minimal trace, and a smallish
C display.  If passed zero size and offsets use default, if size
C is <200 then take % of default, otherwise use passed width.
C If left & top offsets are 0 then use defaults.
      call ezero
      call curmodule('pdb ')
      cAppName = 'pdb'
      helpinapp='pdb'  ! set once for the application
      helpinsub='pdb'  ! set for MAIN
      IUOUT=6
      IUIN=5
      IFIL=10
      ifchk=0
      imod=0
      ltmp=' '
      ITFS=1
      call parpsf(MODL,iappw,iappx,iappy,inf)

C Set folder separator (fs) to \ or / as required.
      call isunix(unixok)
      if(unixok)then
        fs = char(47)
      else
        fs = char(92)
      endif
      write(path,'(a1,a1)')'.',fs

C Clear temporary descriptions.
      do 11 i=1,maxcon
        ltpcmp(i)=' '
        do 12 j=1,maxflw
          mfsdsc(i,j)=' '
  12    continue
  11  continue

      MMOD=MODL
      if(iappw.eq.0.and.iappx.eq.0.and.iappy.eq.0)then
        iappw=480
        iappx=85
        iappy=110
      else
        if(iappx.le.0)iappx=85
        if(iappy.le.0)iappy=110
        if(iappw.le.200)then
          iappwi=int(480*iappw*0.01)
          iappw=iappwi
        elseif(iappw.gt.200)then
          continue
        endif
      endif

C Set pixels high to iappw and pixels wide to factor in monitor size.
      iapphi=iappw
      iappwi=int(real(iappw)*(1024.0/780.0))

      if(iappw.gt.0.and.iappw.lt.100)then
        menuchw = MAX0(int(20*iappw*0.01),20)
        LIMIT =26
        LIMTTY=26
      else
        menuchw = 20
        LIMIT =26
        LIMTTY=26
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
     &'ESP-r Plant db Manager: enquiries to esru@strath.ac.uk')

      IF(MMOD.EQ.8)THEN
        CALL win3d(menuchw,7,1,1,3,igl,igr,igt,igb,igw,igwh)
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
      endif
      CALL ESPrVersion("summary",cAppName,IUOUT)
      call edisp(IUOUT,' ')

C Find the user's home folder then get users custom settings.
      call usrhome(upath)
      if(unixok)then
        write(esprc,'(3a)') upath(1:lnblnk(upath)),fs,'.esprc'
      else
        write(esprc,'(3a)') upath(1:lnblnk(upath)),fs,'esprc'
      endif
      call scesprc(esprc,IFIL+1,0,IIER)

C Scan the defaults file for default configuration.
C Make temporary use of file unit IFIL+1.  Note: escdef must come
C after scan of .esprc file.
      call escdef(IFIL+1,IER)

C Take command line file name as initial file.
      if(inf(1:2).ne.'  '.and.inf(1:4).ne.'UNKN')then
        LCMDFL=inf
        write(outs248,'(a,a)')' the input file is: ',LCMDFL
        call edisp248(iuout,outs248,80)
      else
        LCMDFL='UNKNOWN'
      endif

C Initialize plant component database file.
  100 helptopic='pdb_file_name_select'
      call gethelptext(helpinsub,helptopic,nbhelp)
      L144=LCMDFL
      llt=lnblnk(L144)

C The X11 version will be returning only the name of the
C file, while the GTK version will be returning the
C name with the full path.
      iglib = igraphiclib()  ! find out if X11 or GTK or text support only.
      if(iglib.eq.1.or.iglib.eq.3)then
        if(llt.lt.96)then
          CALL EASKF(L144,'Plant component database file name? ',' ',
     &      96,DPCDB,' Database name ',IER,nbhelp)
        elseif(llt.ge.96.and.llt.lt.124)then
          CALL EASKF(L144,'Plant component database file name? ',' ',
     &      124,DPCDB,' Database name ',IER,nbhelp)
        elseif(llt.ge.124.and.llt.le.144)then
          CALL EASKF(L144,'Plant component database file name? ',' ',
     &      144,DPCDB,' Database name ',IER,nbhelp)
        endif
      elseif(iglib.eq.2)then
        CALL EASKF(L144,'Plant component database file name? ',' ',
     &    144,DPCDB,' Database name ',IER,nbhelp)
      else
        CALL EASKF(L144,'Plant component database file name? ',' ',
     &    96,DPCDB,' Database name ',IER,nbhelp)
      endif

      if(L144(1:2).ne.'  ')LCMDFL=L144

C Find the path and local file name, if exists open otherwise fill.
      call fdroot(LCMDFL,path,LTMP)
      XST=.false.
      call findfil(LTMP,XST)
      if(XST)then

C Existing file, so check header.
C IRECLN= from plantdb.h file.
        ier=0
        call EFOPRAN(IFIL,LTMP,IRECLN,1,IER)
        if(ier.ne.0)goto 100

        IREC = 1
        READ (IFIL,REC=IREC,IOSTAT=ISTAT,ERR=1) PZDESC
        IF (PZDESC .NE. ' PLANT COMPONENT DATABASE')then
          call usrmsg('File is not a plant component database,',
     &      'please supply the file name again.','W')
          goto 100
        endif
        IREC = 2
        READ (IFIL,REC=IREC,IOSTAT=ISTAT,ERR=1) NPC,NXTREC

C Recover item pointers.
        NR = 1
        DO 110 I = 3, MHEADR
           NRR = NR + IRECLN - 1
           IREC = I
           READ (IFIL,REC=IREC,IOSTAT=ISTAT,ERR=1)
     &                          (ITMLOC(J,1),J=NR,NRR)
           NR = NR + IRECLN
  110   CONTINUE

C Recover item code numbers.
        call edisp(iuout,' ')
        call edisp(iuout,
     &  ' Database contains the following plant component entries:')
        call edisp(iuout,' ')
        DO 120 I = 1,NPC
          IREC = ITMLOC(I,1)
          READ (IFIL,REC=IREC,IOSTAT=ISTAT,ERR=1)IDUM, ITMLOC(I,2)
          irec=irec+2
          read(ifil,REC=IREC,IOSTAT=ISTAT,ERR=1) cdesc
          write(outs,'(i3,a,a80)') i,'- ',cdesc
          call edisp(iuout,outs)
  120   CONTINUE
      else

C New file.
        ier=0
        call EFOPRAN(IFIL,LTMP,IRECLN,3,IER)
        CALL PCUPDT (0)
        imod=1
        call usrmsg(' ',' Creating new database.','-')
      endif

C Build main menu.
      write(zitem(1),'(a,a)') 'a Db: ',LTMP(1:16)
      zitem(2)='  ____________________   '
      zitem(3)='b List & export compnts  '
      zitem(4)='c Insert & import compnts'
      zitem(5)='d Delete components      '
      zitem(6)='e Edit a component       '
      zitem(7)='  ____________________   '
      zitem(8)='? Help                   '
      zitem(9)='- Finish ESRUpdb         '

C Display menu.
  210 NITEMS = 9
      INO = -5

C Use askabout to instanciate the initial help messages (2nd parameter is one).
      call askabout('pdb ',1)

  220 CALL EMENU('  ESP-r plant db',ZITEM,NITEMS,INO)
      if(imod.ne.1.and.ifchk.eq.0.and.
     &       (ino.eq.4.or.ino.eq.5.or.ino.eq.6)) then

C Display warning message.
        helptopic='pdb_modification_warning'
        call gethelptext(helpinsub,helptopic,nbhelp)
        dok=.true.
        CALL ASKOK(' ',' Continue ?',OK,dok,nbhelp)
        IF(.NOT.OK) then
          goto 210
        else
          ifchk=1
        endif
      endif
      if(INO.EQ.9)then

C Exit from program.
        call usrmsg(' Closing plant dbase management...',' ','P')
        call erpfree(ifil,istat)
        CALL EPAGEND
        STOP
      elseif(INO .EQ. 8)then

C Describe components and menu structure.
        call askabout('pdb ',0)
      ELSEIF (INO .EQ. 1)then
        goto 100
      ELSEIF (INO .EQ. 3)then
        CALL PCLIST
      ELSEIF (INO .EQ. 4)then
        CALL PCENTR
      ELSEIF (INO .EQ. 5)then
        CALL PCDELT
      ELSEIF (INO .EQ. 6)then
        CALL PCEDIT(0)
      else
        ino=-1
        goto 220
      endif
      GOTO 210

C Error handling.
  1   dok=.true.
      helptopic='pdb_confusion_warning'
      call gethelptext(helpinsub,helptopic,nbhelp)
      write(outs,'(a)') 'Error in pdb main database scan. '
      CALL ASKOK(outs,' Exit from ESRUpdb ?',OK,dok,2)
      if(ok)then
        CALL EPAGEND
        stop
      endif
      goto 210

      END

C ******** list  ********
C This subroutine lists components in database in a window.
C Components colud then be picked from list.
C 'inpc'        : number of components.
C 'ival'        : array to contain selected component(s) position
C                 in list.
C 'nitems'      : allowed number of components to be selected
C                 from list.
C 'ix'          : returns number of selections made.
      subroutine list(inpc,ival,nitems,ix)
#include "plantdb.h"
#include "espriou.h"
      common/FILEP/IFIL
      COMMON/ERRS/ISTAT,IREC
      EQUIVALENCE (ERRFLG, ISTAT)

      common/PCDATC/NPC,ITMLOC(MAXPC,2)

      DIMENSION comp(MAXPC), ival(inpc)
      CHARACTER comp*80,cdesc*80,outs*124
      logical ok,dok

C For help messages
      character helpinsub*24 ! subroutine name
      character helptopic*24 ! string (unique) for topic
      integer nbhelp     ! number of help lines found (will be non-zero
                         ! because default message will be created

      helpinsub='list'  ! set for MAIN

C Write description of each component in string array.
      DO 10 L=1,inpc
         IREC = ITMLOC(l,1)+2
         read(ifil,REC=IREC,IOSTAT=ISTAT,ERR=1) cdesc
         WRITE(comp(l),14) cdesc
   14    FORMAT(A80)
   10 CONTINUE

C Now display the menu.
        IX=nitems
        CALL EPICKS(IX,IVAL,
     &   ' Please select component(s) from the list provided: ',
     &   ' (Exit) terminates selection.',60,npc,comp,
     &   'Components list',IER,0)

      return

  1   dok=.true.
      helptopic='pdb_could_not_scan'
      call gethelptext(helpinsub,helptopic,nbhelp)
      write(outs,'(a)') 'Error in pdb list database scan. '
      CALL ASKOK(outs,' Exit from ESRUpdb ?',OK,dok,2)
      if(OK)then
        CALL EPAGEND
        stop
      else
        return
      endif
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
      elseif(app(1:3).eq.'pdb')then
        folder='esrupdb'
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
