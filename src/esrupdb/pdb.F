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

#include "plantdb.h"
#include "esprdbfile.h"

      integer igraphiclib  ! external definition

C The Plant component DataBase program maintains a
C database containing plant component descriptions.
C The database holds the data needed by ESRUbps to
C allow simulation of plant operation.
C Facilities are provided to enter, list, edit and
C delete component descriptions.
C The routine PCHELP contains a description of a 
C database entry.
      common/pophelp/h(60)
      COMMON/SPAD/MMOD,LIMIT,LIMTTY
      COMMON/VIEWPX/menuchw,igl,igr,igt,igb,igw,igwh

C Defaults.
      COMMON/DEFLT2/DFCFG,DFCTL,DEFRLB,DAFRES,DAPROB,DPNF

C Path to problem and command line file (if any).
      common/rpath/path
      common/uhome/upath
      common/udot/esprc
      common/rcmd/LCMDFL
      common/appw/iappw,iappx,iappy

C Where default db list are kept.
      common/defdb/dfdblbl,defdbfl
      common/OUTIN/IUOUT,IUIN
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

      logical     unixok,OK,DOK,XST
      CHARACTER ZITEM(9)*26 
      CHARACTER ZDESC*25,LTMP*72,cdesc*80,h*72,inf*144,L144*144
      character lcmdfl*144,outs*124,outs248*248
      character*72 DFCFG,DFCTL,DEFRLB,DAPROB,DAFRES,DPNF
      character path*72,upath*72,esprc*72,fs*1
      character dfdblbl*20,defdbfl*72,mfsdsc*68,ltpcmp*60 

C Initialise output device, assume minimal trace, and a smallish
C display.  If passed zero size and offsets use default, if size
C is <200 then take % of default, otherwise use passed width.
C If left & top offsets are 0 then use defaults.
      call ezero
      call curmodule('pdb ')
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
        CALL win3d(menuchw,7,1,1,3,igl,igr,igt,igb,igw,igh)
#ifdef X11
        call opencpw
        call opentutorial
        call opensetup
#endif
        mdispl=0
        nifgrey=0
        ncset=0
        ngset=0
        nzonec=0
        call foundcolour(mdispl,nifgrey,ncset,ngset,nzonec)
        call startbuffer()
      endif
      call edisp(IUOUT,' ')
      write(outs,'(2a)')
     &  ' ESP-r Plant db manager: Version 2.6a of January 2006.',
     &  ' Copyright 2001-6 Energy'
      call edisp(IUOUT,outs)
      write(outs,'(2a)')
     & ' Systems Research Unit, University of',
     & ' Strathclyde, Glasgow Scotland.'
      call edisp(IUOUT,outs)
      call edisp(IUOUT,' ')

C Find the user's home folder then get users custom settings.
      call usrhome(upath)
      if(unixok)then
        write(esprc,'(a,a,a)') upath(1:lnblnk(upath)),fs,'.esprc'
      else
        write(esprc,'(a,a,a)') upath(1:lnblnk(upath)),fs,'esprc'
      endif
      call scesprc(esprc,IFIL+1,0,IIER)

C Scan the defaults file for default configuration.
C Make temporary use of file unit IFIL+1.  Note: escdef must come
C after scan of .esprc file.
      call escdef(dfdblbl,defdbfl,IFIL+1,IER)

C Take command line file name as initial file.
      if(inf(1:2).ne.'  '.and.inf(1:4).ne.'UNKN')then
        LCMDFL=inf
        write(outs248,'(a,a)')' the input file is: ',LCMDFL
        call edisp248(iuout,outs248,80)
      else
        LCMDFL='UNKNOWN'
      endif

C Initialize plant component database file.
  100 h(1)='The plant component database binary file holds the'
      h(2)='description of available components. If the name'
      h(3)='given matches an existing file, it will be read in'
      h(4)='and the components data will be available for'
      h(5)='listing and editing. If there is no such file then'
      h(6)='a new one will be created to hold data for new'
      h(7)='components.'
      L144=LCMDFL
      llt=lnblnk(L144)

C The X11 version will be returning only the name of the
C file, while the GTK version will be returning the
C name with the full path.
      iglib = igraphiclib()  ! find out if X11 or GTK or text support only.
      if(iglib.eq.1.or.iglib.eq.3)then
        if(llt.lt.96)then
          CALL EASKF(L144,'Plant component database file name? ',' ',
     &      96,DPCDB,' Database name ',IER,7)
        elseif(llt.ge.96.and.llt.lt.124)then
          CALL EASKF(L144,'Plant component database file name? ',' ',
     &      124,DPCDB,' Database name ',IER,7)
        elseif(llt.ge.124.and.llt.le.144)then
          CALL EASKF(L144,'Plant component database file name? ',' ',
     &      144,DPCDB,' Database name ',IER,7)
        endif
      elseif(iglib.eq.2)then
        CALL EASKF(L144,'Plant component database file name? ',' ',
     &    144,DPCDB,' Database name ',IER,7)
      else
        CALL EASKF(L144,'Plant component database file name? ',' ',
     &    96,DPCDB,' Database name ',IER,7)
      endif

      if(L144(1:2).ne.'  ')LCMDFL=L144

C Find the path and local file name, if exists open otherwise fill.
      call fdroot(LCMDFL,path,LTMP)
      XST=.false.
      call findfil(LTMP,XST)
      if(XST)then

C Existing file, so check header.
C IRECLN= from plantdb.h file.
        call EFOPRAN(IFIL,LTMP,IRECLN,1,IER)
        if(ier.ne.0)goto 100

        IREC = 1
        READ (IFIL,REC=IREC,IOSTAT=ISTAT,ERR=1) ZDESC
        IF (ZDESC .NE. ' PLANT COMPONENT DATABASE')then
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
        h(1) ='**WARNING**: In order to modify plant data base,'
        h(2) ='you must have write permission.'
        h(3) ='This means that if your data base file is '
        h(4) ='linked, then you must have write permission '
        h(5) ='to the file it is linked to.'
        h(6) ='Otherwise you need to copy plant '
        h(7) ='data base to your home folder before you'
        h(8) ='can modify it.'
        dok=.true.
        CALL ASKOK(' ',' Continue ?',OK,dok,8)
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
      CALL EPAGE
      GOTO 210

C Error handling.
  1   dok=.true.
      h(1)='Pdb is confused, it could not scan the database. This'
      h(2)='is an option to exit the application. '
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
      common/pophelp/h(60)
      common/FILEP/IFIL
      COMMON/ERRS/ISTAT,IREC
      EQUIVALENCE (ERRFLG, ISTAT)

      common/PCDATC/NPC,ITMLOC(MAXPC,2)

      DIMENSION comp(MAXPC), ival(inpc)
      CHARACTER comp*80,cdesc*80,outs*124,h*72
      logical ok,dok

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
      h(1)='Pdb is confused, it could not scan the database. This'
      h(2)='is an option to exit the application. '
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
      SUBROUTINE ADJVIEW(ITRC,IER)
      ier=0
      return
      end

      subroutine chgazi(icazi)
      return
      end

      subroutine chgelev(icelev)
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
