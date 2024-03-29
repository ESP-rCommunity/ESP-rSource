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


      program pdb
      USE START_UP
#include "building.h"
#include "model.h"
#include "plantdb.h"
#include "pdb_data.h"
#include "esprdbfile.h"
#include "espriou.h"
#include "help.h"

      integer lnblnk  ! function definition
      integer igraphiclib  ! external definition

C The Plant component template management program maintains
C a file containing plant component descriptions.
C The file holds the data needed by ESRUbps to
C allow simulation of plant operation.
C Facilities are provided to enter, list, edit and
C delete component descriptions.
      COMMON/SPAD/MMOD,LIMIT,LIMTTY
      integer menuchw,igl,igr,igt,igb,igw,igwh
      COMMON/VIEWPX/menuchw,igl,igr,igt,igb,igw,igwh

C Defaults.
      character*96 DFCFG,DFCTL,DEFRLB,DAPROB,DAFRES,DPNF
      COMMON/DEFLT2/DFCFG,DFCTL,DEFRLB,DAFRES,DAPROB,DPNF

C Path to problem and command line file (if any).
      common/rpath/path
      common/rcmd/LCMDFL
      common/appw/iappw,iappx,iappy

      common/OUTIN/IUOUT,IUIN,IEOUT
      common/warncounter/iecount
      integer ifs,itfs,imfs
      COMMON/GFONT/IFS,ITFS,IMFS
      common/FILEP/IFIL
      COMMON/ERRS/ISTAT,IREC
      EQUIVALENCE (ERRFLG, ISTAT)

C Session log file (for errors etc.)
      character iefile*72
      common/logs/iefile

C Name of current application
      common/APPNAME/cAppName
      character cAppName*12

C Ask to overwrite flag.
      COMMON/OVRWT/AUTOVR
      logical AUTOVR

      logical     unixok,OK,XST
      CHARACTER ZITEM(10)*30 
      CHARACTER PZDESC*25,LTMP*72,cdesc*80
      character inf*144,L144*144,paction*16,pact*16
      character lcmdfl*144,lltmp*144,ascif*96,pascif*96
      character outs*124,outs248*248
      character path*72,fs*1
      character tab*1
      character cVnum*38       ! returned from ESPrVersionNum
      character pagestitle*112 ! for banner title via epages call
      character buffer*124     ! for listing summary of components
      character dstmp*24,uname*24,tfile*72

      integer iglib   ! if 1 then X11, if 2 then GTK, if 3 then text only.
      integer ier
      integer NITEMS,INO ! max items and current menu item

#ifdef OSI
      integer iigl,iigr,iigt,iigb,iigw,iigwh
      integer iiw1,iiw2,iiw3,iiw4,iimenu
#else
      integer*8 iigl,iigr,iigt,iigb,iigw,iigwh
      integer*8 iiw1,iiw2,iiw3,iiw4,iimenu
#endif

C Initialise output device, assume minimal trace, and a smallish
C display.  If passed zero size and offsets use default, if size
C is <200 then take % of default, otherwise use passed width.
C If left & top offsets are 0 then use defaults.
      call ezero
      call curmodule('pdb ')
      cAppName = 'pdb'
      helpinapp='pdb'  ! set once for the application
      helpinsub='pdb'  ! set for MAIN
      tab = CHAR(9)
      IUOUT=6
      IUIN=5
      IFIL=10
      ifchk=0
      imod=0
      ltmp=' '
      ITFS=1
      AUTOVR=.false. ! by default, ask to overwrite files

C Parse the command line which might include instructions to carry
C out an ascii->bin or bin->ascii conversion.
C      call parpsf(MODL,iappw,iappx,iappy,inf)
      call parsfact(MODL,iappw,iappx,iappy,inf,pact,ascif)

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
        iappw=540
        iappx=85
        iappy=110
      else
        if(iappx.le.0)iappx=85
        if(iappy.le.0)iappy=110
        if(iappw.le.200)then
          iappwi=int(540*iappw*0.01)
          iappw=iappwi
        elseif(iappw.gt.200)then
          continue
        endif
      endif

C Set pixels high to iappw and pixels wide to factor in monitor size.
      iapphi=iappw
C      iappwi=int(real(iappw)*(1024.0/780.0))
      iappwi=int(real(iappw)*(1024.0/720.0))

      if(iappw.gt.0.and.iappw.lt.100)then
        menuchw = MAX0(int(24*iappw*0.01),20)
        LIMIT =26
        LIMTTY=26
      else
        menuchw = 32
        LIMIT =26
        LIMTTY=26
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
        IMFS=5
        IFS=4
        ITFS=4
        LIMTTY=30
        LIMIT =30
      ENDIF

C Find the current ESP-r version number and add it to application title.
      call ESPrVersionNum(cVnum)
      write(pagestitle,'(2a)') 'Plant component manager of ESP-r ',
     &  cVnum(1:lnblnk(cVnum))

      lntitle=lnblnk(pagestitle)
      CALL EPAGES(MMOD,IUIN,IUOUT,iappwi,iapphi,iappx,iappy,menuchw,
     &  pagestitle,lntitle)

      IF(MMOD.EQ.8)THEN

C Setup and pass in parameters to win3d.
        iiw1=7; iiw2=1; iiw3=1; iiw4=3; iimenu=menuchw
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
      endif
      CALL ESPrVersion("summary",cAppName,IUOUT)

C Find the user's home folder then get users custom settings.
      call usrhome(upath)
      if(unixok)then
        write(esprc,'(3a)') upath(1:lnblnk(upath)),fs,'.esprc'
      else
        write(esprc,'(3a)') upath(1:lnblnk(upath)),fs,'esprc'
      endif
      call scesprc(esprc,IFIL+1,0,IIER)

C Create and open a session log file based on user name, PID etc.
      uname=' '; tfile=' '
      call usrname(uname)
      call esppid(ipid)
        write(tfile,'(a,a1,a,a,i7,a)')upath(1:lnblnk(upath)),fs,'.',
     &    uname(1:lnblnk(uname)),ipid,'.log'
      call st2file(tfile,iefile)

C Open the session file.
      ieout=ifil+931          ! set to unused index 942
      iecount=0               ! clear count of errors
      open(ieout,file=iefile,status='UNKNOWN',err=903)
      write(ieout,'(a)')'Session log for pdb'
      call dstamp(dstmp) ! get curret time
      write(ieout,'(2a)')'Date,',dstmp
      write(ieout,'(2a)')'User,',uname(1:lnblnk(uname))
  903 continue

C Scan the defaults file for default configuration.
C Make temporary use of file unit IFIL+1.  Note: escdef must come
C after scan of .esprc file.
      call escdef(IFIL+1,'-',IER)

C Take command line file name as initial file.
      if(inf(1:2).ne.'  '.and.inf(1:4).ne.'UNKN')then
        LCMDFL=inf
        write(outs248,'(a,a)')'Input file is: ',LCMDFL
C        call edisp(iuout,'  ')
C        call edisp248(iuout,outs248,80)
      else
        LCMDFL='UNKNOWN'
      endif

C Take command line action parameter:
C If paction = 'asci2bin' then fill binary file `inf` with data 
C from `ascif` and exit.
C If paction is `bin2asci` file ascii file `ascif` with data from
C the binary file `inf` and exit.
      if(pact(1:1).ne.' ')then
        paction=pact
        if(paction(1:4).ne.'NONE')then
          write(outs,'(a,a)')' the action is: ',paction
          call edisp(iuout,outs)
        endif
        write(outs,'(a,a)')'pdb: the input action is: ',paction
        call tstamp('>',outs)
        if(paction(1:8).eq.'asci2bin')then

C Convert ascii file to binary (LCMDFL) and exit.
          pascif=ascif

          IUFIL = IFIL+1
          CALL FPOPEN (IUFIL,ISTAT,1,1,pascif)
          IF (ISTAT .EQ. -3)then

C Unable to open so advise, add to error log and stop.
            call usrmsg(
     &        'Plant component import file could not be opend',
     &        'so exiting without doing anything.','W')
            call edisp(ieout,
     &        'Plant component import file could not be opend.')
            iecount=iecount+1  ! retain the log file
            close(ieout)
            CALL ERPFREE(ieout,ISTAT)
            CALL EPAGEND
            STOP
          endif

C Create empty binary file. Use fdroot to set LTMP.
          call fdroot(LCMDFL,path,LTMP)
          ier=0
          call EFOPRAN(IFIL,LTMP,IRECLN,3,IER)
          CALL PCUPDT (0)
          imod=1

C Read components data from file. If unable to open advise user,
C add to error log and stop.
          READ (IUFIL,2) PZDESC
    2     FORMAT (A25,/)
          IF(PZDESC(1:25).NE.' PLANT COMPONENT DATABASE') THEN
            call usrmsg(
     &        'Import file does not appear to be a plant templates',
     &        'ascii file so exiting without doing anything.','W')
            call edisp(ieout,
     &        'Import file does not appear to be a plant template.')
            iecount=iecount+1  ! retain the log file
            close(ieout)
            CALL ERPFREE(ieout,ISTAT)
            CALL EPAGEND
            STOP
          endif
  212     CALL PCRDF(IUFIL)
          IF(INDXPC.GT.0) THEN
            CALL PCPUT (INDXPC)
            WRITE(buffer,'(a,i5,a)')
     &       ' Plant component entry ',INDXPC,' added'
            call edisp(iuout,buffer)
            GOTO 212  ! jump back and read more and dump to binary
          ELSE IF(INDXPC.LT.0) THEN
            GOTO 212  ! jump back and read more and dump to binary
          ELSE
            CALL ERPFREE(IUFIL,ISTAT)
          END IF

          call tstamp('>',
     &      'Finished plant component conversion ASCI2BIN')
          write(outs248,'(5a)')'Plant_conversion',tab,
     &      pascif(1:lnblnk(pascif)),tab,LCMDFL(1:lnblnk(LCMDFL))
          call edisp248(iuout,outs248,90)
          call pausems(1000)  ! so users can see completion message
          close(ieout)
          if(iecount.eq.0)then     ! if there were no warnings delete
            open(ieout,file=iefile,status='UNKNOWN')
            call efdelet(ieout,istat)
          endif
          CALL EPAGEND
          STOP
        elseif(paction(1:8).eq.'bin2asci')then

C Read in the supplied plant template file (LCMDFL), scan it to find
C number of components and where each component starts in the file.
          pascif=ascif
          call fdroot(LCMDFL,path,LTMP)
          XST=.false.
          call findfil(LTMP,XST)
          if(XST)then

C Existing file, so check header. IRECLN is from plantdb.h file.
            ier=0
            call EFOPRAN(IFIL,LTMP,IRECLN,1,IER)
            if(ier.ne.0)then
              call usrmsg(
     &          'Plant component templates file could not be opend',
     &          'so exiting without doing anything.','W')
              call edisp(ieout,
     &          'Plant component templates file could not be opend.')
              iecount=iecount+1  ! retain the log file
              close(ieout)
              CALL ERPFREE(ieout,ISTAT)
              CALL EPAGEND
              STOP
            endif

            IREC = 1
            READ (IFIL,REC=IREC,IOSTAT=ISTAT,ERR=1) PZDESC
            IF (PZDESC .NE. ' PLANT COMPONENT DATABASE')then
              call usrmsg(
     &          'File is not a plant component templates file,',
     &          'so exiting without doing anything.','W')
              call edisp(ieout,
     &          'File is not a plant component templates file.')
              iecount=iecount+1  ! retain the log file
              close(ieout)
              CALL ERPFREE(ieout,ISTAT)
              CALL EPAGEND
              STOP
            endif
            IREC = 2
            READ (IFIL,REC=IREC,IOSTAT=ISTAT,ERR=1) NPC,NXTREC

C Check library is not empty.
            IF (NPC .EQ. 0) THEN
              call usrmsg('No components in file so ascii file cannot',
     &                    'be created. Exiting with no action.','W')
              call edisp(ieout,
     &          'No components so ascii file cannot be created.')
              iecount=iecount+1  ! retain the log file
              close(ieout)
              CALL ERPFREE(ieout,ISTAT)
              CALL EPAGEND
              STOP
            ENDIF

C Recover item pointers which will be used by PCGET.
            NR = 1
            DO 111 I = 3, MHEADR
              NRR = NR + IRECLN - 1
              IREC = I
              READ (IFIL,REC=IREC,IOSTAT=ISTAT,ERR=1)
     &                          (ITMLOC(J,1),J=NR,NRR)
              NR = NR + IRECLN
  111       CONTINUE

C Set file unit for export and open it for writing.
            IUCHAN = IFIL+1
            CALL FPOPEN (IUCHAN,ISTAT,4,3,pascif)
            IF (ISTAT .LT. 0)then
              call usrmsg(
     &          'Unable to open ASCII export file so exiting without',
     &          'doing anything.','W')
              call edisp(ieout,
     &          'Unable to open ASCII export file so exiting.')
              iecount=iecount+1  ! retain the log file
              close(ieout)
              CALL ERPFREE(ieout,ISTAT)
              CALL EPAGEND
              STOP
            endif

C Write to file
            call edisp(iuchan,' PLANT COMPONENT DATABASE Listing')

C Output unordered.
            DO 810 I = 1, NPC
              CALL PCGET (I)
              IF (INDXPC .GT. 0) THEN
                CALL PCWRT (IUCHAN)
              ENDIF
  810       CONTINUE
            close(IUCHAN)
            write(outs248,'(5a)')'Plant_conversion',tab,
     &        LCMDFL(1:lnblnk(LCMDFL)),tab,pascif(1:lnblnk(pascif))
            call edisp248(iuout,outs248,90)
            call pausems(1000)  ! so users can see completion message
          else
            call usrmsg(
     &        'Unable to find plant file so exiting without',
     &        'doing any conversion.','W')
              call edisp(ieout,
     &          'Unable to find plant file so exiting')
              iecount=iecount+1  ! retain the log file
          endif
          close(ieout)
          if(iecount.eq.0)then     ! if there were no warnings delete
            open(ieout,file=iefile,status='UNKNOWN')
            call efdelet(ieout,istat)
          endif
          CALL EPAGEND
          STOP
        endif
      endif

C Take the command line plant template file and if it can be
C correctly read in do not ask the user for further confirmation.
      L144=LCMDFL  ! set initial guess to command line file
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
          call usrmsg(
     &      'File is not a plant component templates file,',
     &      'please supply the file name again.','W')
          goto 100
        endif
        IREC = 2
        READ (IFIL,REC=IREC,IOSTAT=ISTAT,ERR=1) NPC,NXTREC

C Recover item pointers.
        NR = 1
        DO 113 I = 3, MHEADR
           NRR = NR + IRECLN - 1
           IREC = I
           READ (IFIL,REC=IREC,IOSTAT=ISTAT,ERR=1)
     &                          (ITMLOC(J,1),J=NR,NRR)
           NR = NR + IRECLN
  113   CONTINUE

C Recover item code numbers.
        call edisp(iuout,' ')
        write(outs,'(a,i3,a)') 'There are ',NPC,
     &    ' plant component templates in the command line file.'
        call edisp(iuout,outs)
        goto 210
      endif


C Initialize plant component template file.
  100 helptopic='pdb_file_name_select'
      call gethelptext(helpinsub,helptopic,nbhelp)
      lltmp=' '  ! clear file name to be returned
      call edisp(iuout,'  ')  ! echo blank line
      L144=LCMDFL  ! set initial guess to command line file
      CALL EASKXORGTKF(L144,
     &  'Plant component templates (if new an empty file is created)',
     &  'Confirm:',DPCDB,lltmp,'plant templates',IER,nbhelp)

C If user cancel display the menu without loading file.
      if(ier.eq.-3)then
        goto 210  ! display menu.
      endif

C Set LCMDFL based on user confirmation.
      if(lltmp(1:2).ne.'  ')LCMDFL=lltmp

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
          call usrmsg(
     &      'File is not a plant component templates file,',
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
        write(outs,'(a,i3,a)') 'There are ',NPC,
     &    ' plant component template entries.'
        call edisp(iuout,outs)
      else

C New file.
        ier=0
        call EFOPRAN(IFIL,LTMP,IRECLN,3,IER)
        CALL PCUPDT (0)
        imod=1
        call usrmsg(' ','Creating new plant components file.','-')
      endif

C Build main menu.
  210  write(zitem(1),'(a,a)') 'a File: ',LTMP(1:19)
      zitem(2)='  _______________________   '
      zitem(3)='b list & export components  '
      zitem(4)='c insert & import components'
      zitem(5)='d delete components         '
      zitem(6)='e edit a component          '
      zitem(7)='  _______________________   '
      zitem(8)='! summary of entries        '
      zitem(9)='? help                      '
      zitem(10)='- quit module              '

C Display menu.
      NITEMS = 10
      INO = -5

C Use askabout to instantiate the initial help messages (2nd parameter is one).
      call askabout('pdb ',1)

  220 CALL EMENU('ESP-r plant components',ZITEM,NITEMS,INO)
      if(imod.ne.1.and.ifchk.eq.0.and.
     &       (ino.eq.4.or.ino.eq.5.or.ino.eq.6)) then

C Display warning message.
        helptopic='pdb_modification_warning'
        call gethelptext(helpinsub,helptopic,nbhelp)
        CALL EASKOK(' ','Continue?',OK,nbhelp)
        IF(.NOT.OK) then
          goto 210
        else
          ifchk=1
        endif
      endif
      if(INO.EQ.10)then

C Exit from program.
        call erpfree(ifil,istat)
        close(ieout)
        if(iecount.eq.0)then     ! if there were no warnings delete
          open(ieout,file=iefile,status='UNKNOWN')
          call efdelet(ieout,istat)
        endif
        CALL EPAGEND
        STOP
      elseif(INO .EQ. 9)then

C Describe components and menu structure.
        call askabout('pdb ',0)
      elseif(INO .EQ. 8)then

C Provide summary of entries. Note INDXPC is in common via pdb_data.h
C and it the identity index of the component.
        call edisp(iuout,' Entry  Model Description')
        DO 910 I = 1,NPC
          CALL PCGET (I)
          IF (INDXPC .EQ. I) THEN
            WRITE (buffer,15) INDXPC,ICODE,ZCDESC(1:LNBLNK(ZCDESC))
   15       FORMAT (I3,' =',I5,' ',A)
            call edisp(iuout,buffer)
          ENDIF
  910   CONTINUE

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
  1   helptopic='pdb_confusion_warning'
      call gethelptext(helpinsub,helptopic,nbhelp)
      write(outs,'(a)') 'Components database scan error!'
      iecount=iecount+1  ! retain the log file
      CALL EASKOK(outs,'Exit application?',OK,2)
      if(ok)then
        close(ieout)   ! retain the error log file.
        CALL ERPFREE(ieout,ISTAT)
        CALL EPAGEND
        stop
      endif
      goto 210

      END

C ******** list  ********
C This subroutine lists component templates in a window.
C Components colud then be picked from list.
C 'inpc'        : number of components.
C 'ival'        : array to contain selected component(s) position
C                 in list.
C 'nitems'      : allowed number of components to be selected
C                 from list.
C 'ix'          : returns number of selections made.
      subroutine list(inpc,ival,nitems,ix)
#include "plantdb.h"
#include "pdb_data.h"
#include "help.h"

      common/FILEP/IFIL
      COMMON/ERRS/ISTAT,IREC
      EQUIVALENCE (ERRFLG, ISTAT)

      DIMENSION comp(MAXPC), ival(inpc)
      CHARACTER comp*80,cdesc*80,outs*124
      logical ok

      helpinsub='pdb'       ! set for MAIN

C Write description of each component in string array.
      DO 10 L=1,inpc
         IREC = ITMLOC(l,1)+2
         read(ifil,REC=IREC,IOSTAT=ISTAT,ERR=1) cdesc
         WRITE(comp(l),14) cdesc
   14    FORMAT(A80)
   10 CONTINUE

C Help text for epicks << need to create this >>
      helptopic='pdb_could_not_scan'
      call gethelptext(helpinsub,helptopic,nbhelp)

C Now display the menu.
        IX=nitems
        CALL EPICKS(IX,IVAL,
     &   ' Please select component(s) from the list provided: ',
     &   ' (Exit) terminates selection.',60,npc,comp,
     &   'Components list',IER,nbhelp)

      return

  1   helptopic='pdb_could_not_scan'
      call gethelptext(helpinsub,helptopic,nbhelp)
      write(outs,'(a)') 'Components database scan error!'
      CALL EASKOK(outs,'Exit application?',OK,2)
      if(OK)then
        close(ieout)    ! retain the error log file
        CALL ERPFREE(ieout,ISTAT)
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
