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

C folders.f provides creation and editing facilities related to
C maintaining model folders and files:
C  pregist: register a new project.
C  pfolders: maintain model folders.
C  Edimage: controls specification of an image associated with the
C           model.
C  EDBUILD: edits building information for system configuration file.

C << TODO - working with radiance requires a rad folder. If not found
C << a folder should be created by prj prior to invoking e2r. And it
C << could be a check when saving older models to upgrade them.
C << here is some logic...
C          if(radpth(1:3).eq.'./ '.or.radpth(1:3).eq.'.\\ ')then
C            pathtype='radincfg'
C          elseif(radpth(1:7).eq.'../rad '.or.
C     &           radpth(1:7).eq.'..\\rad ')then
C            pathtype='radinrad'
C          endif

C ********** pregist
C Register a new project. Parameter act = 'i ' then initial pass,
C if 'e ' then provide editing menu, if 'sn' use passed parameters
C for the root and path to create a new model, if 'sc' silent continue,
C if 'sw' then pased root and model is assumed to be created silently 
C within the current folder.
C root (32 char) root name (for 'sn' case)
C mpath (72 char) path to model folder (for 'sn' case)
C menu (72 char) brief title of model
      subroutine pregist(act,root,mpath,menu,ier)
#include "building.h"
#include "model.h"
#include "site.h"
#include "esprdbfile.h"
#include "espriou.h"
C espriou.h provides currentfile and MFFOLD.
      
      integer lnblnk  ! function definition

C Passed in parameters.
      character root*32,mpath*72,menu*72

      common/FILEP/IFIL
      common/OUTIN/IUOUT,IUIN
      
      integer ncomp,ncon
      common/C1/NCOMP,NCON
      common/SET1/IYEAR,IBDOY,IEDOY,IFDAY,IFTIME
      common/C6/INDCFG
      common/PREC8/SLAT,SLON
      common/rpath/path
      common/rcmd/LCMDFL

C Plant network.
      COMMON/C23/IFPNF,LPNF

C Images.
      character imgfmt*4  ! GIF XBMP TIF JPG
      character imgfoc*4  ! FZON FNET FCTL FDFS
      character limgfil*72  ! file name (extend to 144 char)
      character imgdoc*248  ! text associated with image
      common/imagf/imgfmt(MIMG),imgfoc(MIMG),limgfil(MIMG),imgdoc(MIMG)

      integer noimg  ! number of images
      integer iton   ! zero if images not yet shown, one if yes
      common/imagfi/noimg,iton

C External text editor.
      common/texted/tedlbl,teditor

      common/FOPENED/CFGOK,MLDBOK,MATDBOK,CTLOK,OPTKOK,CFCDBOK

C Calendar.
      common/caleni/nbdaytype,nbcaldays(MDTY),icalender(365)
      INTEGER NBDAYTYPE,NBCALDAYS,ICALENDER

      logical XST,OK,dok,CFGOK,MLDBOK,MATDBOK,CTLOK,OPTKOK,CFCDBOK
      logical OKI,concat,unixok,clkok

      character dstmp*24,LCFTMP*32
      character*72 LTMP
      character path*72,LCMDFL*144,LPNF*72
      character odir*84,pwd*84
      character doit*248,longtfile*144,longtfiledos*144
      character tedlbl*20,teditor*20
      character uname*24,tmode*8
      character limg*72,act*2
      character iformat*4,ifocus*4
      character fs*1
      character subpath*72,action*3,outs*124
      character lltmp*144,lguess*144,lldef*144  ! for working with EASKXORGTKF
      character lpath*72,fname*72 ! for use with fdroot
      character cfgpath*144  ! full path to cfg folder for refocus
      character thecfgis*72 ! cfg file only

      integer iyeart    ! for local editing
      integer iglib  ! for detecting GTK or X11
      integer lnmp,lncr,lnod ! for with of mpath, cfgroot, odir strings
      integer itrcc   ! for silently rescanning new cfg file
      character tcname*248   ! for editing image notes
      character WORDS(12)*32,testw*32,folderreadme*96
      character MODE*4

C For help messages
      character helpinsub*24 ! subroutine name
      character helptopic*24 ! string (unique) for topic
      integer nbhelp     ! number of help lines found

      helpinsub='pregist'  ! set for subroutine

C Set folder separator (fs) to \ or / as required.
      call isunix(unixok)
      if(unixok)then
        fs = char(47)
      else
        fs = char(92)
      endif
      thecfgis='  '; cfgpath='  '

C If act = 'i' then initial pass, if 'e' then provide editing menu.

C Ask for source file, brief description and log file. Offer
C editing of log file, images, hypertext link and results summary.
      IER=0

  289 if(act(1:2).eq.'sc')then

C Silent continue, there is nothing to do (currently).
        return
      elseif(act(1:2).eq.'sn')then

C Silent new within the pre-registration process.

C Debug.
C        write(6,*)  ' pregist ',root
C        write(6,*)  ' pregist ',mpath

        write(LCFTMP,'(a)') root(1:lnblnk(root))
      elseif(act(1:2).eq.'sw')then

C Silent within the pre-registration process.

C Debug.
C        write(6,*)  ' pregist ',root
C        write(6,*)  ' pregist ',mpath

        write(LCFTMP,'(a)') root(1:lnblnk(root))

      elseif(act(1:1).eq.'i')then

        helptopic='cfg_overview'
        call gethelptext(helpinsub,helptopic,nbhelp)
        LCFTMP='  '
        CALL EASKSCMD(LCFTMP,' ','Model root name?',
     &    'cancel',clkok,32,'new_model','model root name',IER,nbhelp)
        call usrmsg(' ',' ','-')
        if(clkok) return         ! User selected cancel.

C Trap for blank root name.
        IF(LCFTMP(1:2).EQ.'  '.or.LCFTMP(1:4).eq.'UNKN')GOTO 289

C If we got this far in the 'i' mode clear common blocks. Note this
C clear is not done when called in the 'sn' or 'sw' mode because
C the meta file facility will have instanciated some common blocks
C which should not be cleared.
        call clrprb
      endif

C User did not cancel so instantiate data for new model.
C Current default version is v4.
      icfgv=4   ! alter this to 4 when working correctly
      INDCFG=0
      if(act(1:2).eq.'sn'.or.act(1:2).eq.'sw')then
        continue  ! META or gbXML file will already have set sitelat etc.
      else
        sitelat=55.9; SLAT=55.9; 
        sitelongdif=-4.1; SLON=-4.1
        siteexposureindex=1
        groundrefl=0.2
      endif
      if(act(1:2).eq.'sn'.or.act(1:2).eq.'sw')then
        continue  ! META or gbXML file will already have set zones etc.
      else
        NCOMP=0
        noimg=0
        LPNF='UNKNOWN'
        lmodellog='UNKNOWN'
        nbdaytype=0   ! reset to zero day types
      endif
      call st2file(LCFTMP,LCMDFL)  ! remove unprintable characters

C Derive the model root name from this.
C If sting > 4 char it might have a .cfg attached, otherwise add.
      call fdroot(LCMDFL,path,LCFGF)
      lcfgr=lnblnk(LCFGF)
      lcfgl=lcfgr-3
      if(lcfgr.gt.4)then
        if(LCFGF(lcfgl:lcfgr).eq.'.cfg')then
          if(lcfgl-1.le.32)then
            write(cfgroot,'(a)') LCFGF(1:lcfgl-1)
          else
            write(cfgroot,'(a)') LCFGF(1:32)
          endif
          write(thecfgis,'(a,a)')LCFGF(1:lcfgr)
        else
          if(lcfgr.le.32)then
            write(cfgroot,'(a)') LCFGF(1:lcfgr)
          else
            write(cfgroot,'(a)') LCFGF(1:32)
          endif
          write(LCFGF,'(a,a)')LCFGF(1:lcfgr),'.cfg'
          write(thecfgis,'(a,a)')LCFGF(1:lcfgr),'.cfg'
        endif
      else
        if(lcfgr.le.32)then
          write(cfgroot,'(a)') LCFGF(1:lcfgr)
        else
          write(cfgroot,'(a)') LCFGF(1:32)
        endif
        write(LCFGF,'(a,a)')LCFGF(1:lcfgr),'.cfg'
        write(thecfgis,'(a,a)')LCFGF(1:lcfgr),'.cfg'
      endif

C Check existance of this file (should not overwrite an existing file).
      CALL ERPFREE(IFCFG,ISTAT)
      call FINDFIL(LCFGF,XST)
      if(XST)then
        dok=.true.
        helptopic='cfg_overwrite_warning'
        call gethelptext(helpinsub,helptopic,nbhelp)
        if(act(1:2).eq.'sn')then
          OK=.true.
        elseif(act(1:2).eq.'sw')then
          OK=.true.
        elseif(act(1:1).eq.'i')then
          call askok('There is an existing configuration with',
     &    'this name. Do you want to overwrite it?',OK,dok,nbhelp)
        endif
        if(.not.OK)goto 289
      endif

C Ask where to put it.
C Get the current folder and display options to the user.
      odir=' '
      call usrdir(odir)
      lnod=lnblnk(odir)
      if(act(1:2).eq.'sw')then

        continue ! silent within the pre-registration process.

      elseif(act(1:2).eq.'sn')then

C Silent new within the pre-registration process.

C Create the folder mpath and inside that the standard model folders.
        lnmp=lnblnk(mpath)
        write(doit,'(2a)') 'mkdir ',mpath(1:lnmp)
        call usrmsg('Creating model folders...','  ','-')
        call runit(doit,'-')
        call pausems(100)
        call usrmsg('  ','  ','-')
        write(doit,'(4a)') 'mkdir ',mpath(1:lnmp),fs,'cfg'
        write(folderreadme,'(5a)') mpath(1:lnmp),fs,'cfg',fs,
     &    'Readme.txt'
        call runit(doit,'-')
        call pausems(100)
        write(currentfile,'(a)') folderreadme(1:lnblnk(folderreadme))
        IUNIT=IFIL+1
        CALL EFOPSEQ(IUNIT,folderreadme,3,IER)
        write(iunit,'(a)')'This folder holds the model configuration'
        write(iunit,'(a)')'[cfg] files and master connection lists'
        write(iunit,'(a)')'[cnn] for the project models.'
        CALL ERPFREE(IUNIT,ISTAT)

C Check that the control folder path is standard.
        call GETTOKENS(ctlpth,IW,WORDS)
        lnw=lnblnk(WORDS(IW))
        testw=WORDS(IW)
        if(testw(1:lnw).eq.'ctl'.or.testw(lnw:lnw).eq.'.')then
          write(doit,'(4a)') 'mkdir ',mpath(1:lnmp),fs,'ctl'
          write(ctlpth,'(3a)')'..',fs,'ctl'
          write(folderreadme,'(5a)') mpath(1:lnmp),fs,'ctl',fs,
     &    'Readme.txt'
        else
          write(doit,'(4a)') 'mkdir ',mpath(1:lnmp),fs,testw(1:lnw)
          write(folderreadme,'(5a)') mpath(1:lnmp),fs,testw(1:lnw),fs,
     &    'Readme.txt'
        endif
        call runit(doit,'-')
        call pausems(100)
        write(currentfile,'(a)') folderreadme(1:lnblnk(folderreadme))
        IUNIT=IFIL+1
        CALL EFOPSEQ(IUNIT,folderreadme,3,IER)
        write(iunit,'(a)')'This folder holds control definitions for'
        write(iunit,'(a)')'the model e.g. zone & flow & plant.'
        CALL ERPFREE(IUNIT,ISTAT)

C Check that the aim2 folder path is standard.
        call GETTOKENS(aimpth,IW,WORDS)
        lnw=lnblnk(WORDS(IW))
        testw=WORDS(IW)
        if(testw(1:lnw).eq.'aim2'.or.testw(lnw:lnw).eq.'.')then
          write(doit,'(4a)') 'mkdir ',mpath(1:lnmp),fs,'aim2'
          write(aimpth,'(3a)')'..',fs,'aim2'
        else
          write(doit,'(4a)') 'mkdir ',mpath(1:lnmp),fs,testw(1:lnw)
        endif
        call runit(doit,'-')
        call pausems(100)

C Check that the zones folder path is standard or was not yet defined.
        call GETTOKENS(zonepth,IW,WORDS)
        lnw=lnblnk(WORDS(IW))
        testw=WORDS(IW)
        if(testw(1:lnw).eq.'zones'.or.testw(lnw:lnw).eq.'.')then
          write(doit,'(4a)') 'mkdir ',mpath(1:lnmp),fs,'zones'
          write(zonepth,'(3a)')'..',fs,'zones'
          write(folderreadme,'(5a)') mpath(1:lnmp),fs,'zones',fs,
     &    'Readme.txt'
        else
          write(doit,'(4a)') 'mkdir ',mpath(1:lnmp),fs,testw(1:lnw)
          write(folderreadme,'(5a)') mpath(1:lnmp),fs,testw(1:lnw),fs,
     &    'Readme.txt'
        endif
        call runit(doit,'-')
        call pausems(100)
        write(currentfile,'(a)') folderreadme(1:lnblnk(folderreadme))
        IUNIT=IFIL+1
        CALL EFOPSEQ(IUNIT,folderreadme,3,IER)
        write(iunit,'(a)')'This folder holds zone-based model files'
        write(iunit,'(a)')'e.g. geometry construction tmc operations'
        CALL ERPFREE(IUNIT,ISTAT)

C Check that the nets folder path is standard.
        call GETTOKENS(netpth,IW,WORDS)
        lnw=lnblnk(WORDS(IW))
        testw=WORDS(IW)
        if(testw(1:lnw).eq.'nets'.or.testw(lnw:lnw).eq.'.')then
          write(doit,'(4a)') 'mkdir ',mpath(1:lnmp),fs,'nets'
          write(netpth,'(3a)')'..',fs,'nets'
          write(folderreadme,'(5a)') mpath(1:lnmp),fs,'nets',fs,
     &    'Readme.txt'
        else
          write(doit,'(4a)') 'mkdir ',mpath(1:lnmp),fs,testw(1:lnw)
          write(folderreadme,'(5a)') mpath(1:lnmp),fs,testw(1:lnw),fs,
     &    'Readme.txt'
        endif
        call runit(doit,'-')
        call pausems(100)
        write(currentfile,'(a)') folderreadme(1:lnblnk(folderreadme))
        IUNIT=IFIL+1
        CALL EFOPSEQ(IUNIT,folderreadme,3,IER)
        write(iunit,'(a)')'This folder holds network decriptions for'
        write(iunit,'(a)')'mass flows and electrical power. '
        CALL ERPFREE(IUNIT,ISTAT)

C Check that the images folder path is standard.
        call GETTOKENS(imgpth,IW,WORDS)
        lnw=lnblnk(WORDS(IW))
        testw=WORDS(IW)
        if(testw(1:lnw).eq.'images'.or.testw(lnw:lnw).eq.'.')then
          write(doit,'(4a)') 'mkdir ',mpath(1:lnmp),fs,'images'
          write(imgpth,'(3a)')'..',fs,'images'
          write(folderreadme,'(5a)') mpath(1:lnmp),fs,'images',fs,
     &    'Readme.txt'
        else
          write(doit,'(4a)') 'mkdir ',mpath(1:lnmp),fs,testw(1:lnw)
          write(folderreadme,'(5a)') mpath(1:lnmp),fs,testw(1:lnw),fs,
     &    'Readme.txt'
        endif
        call runit(doit,'-')
        call pausems(100)
        write(currentfile,'(a)') folderreadme(1:lnblnk(folderreadme))
        IUNIT=IFIL+1
        CALL EFOPSEQ(IUNIT,folderreadme,3,IER)
        write(iunit,'(a)')'This folder holds images associated with the'
        write(iunit,'(a)')'model e.g. site photos renderings and/or'
        write(iunit,'(a)')'figures which clarify the model.'
        CALL ERPFREE(IUNIT,ISTAT)

C Check that the doc folder path is standard.
        call GETTOKENS(docpth,IW,WORDS)
        lnw=lnblnk(WORDS(IW))
        testw=WORDS(IW)
        if(testw(1:lnw).eq.'doc'.or.testw(lnw:lnw).eq.'.')then
          write(doit,'(4a)') 'mkdir ',mpath(1:lnmp),fs,'doc'
          write(docpth,'(3a)')'..',fs,'doc'
          write(folderreadme,'(5a)') mpath(1:lnmp),fs,'doc',fs,
     &    'Readme.txt'
        else
          write(doit,'(4a)') 'mkdir ',mpath(1:lnmp),fs,testw(1:lnw)
          write(folderreadme,'(5a)') mpath(1:lnmp),fs,testw(1:lnw),fs,
     &    'Readme.txt'
        endif
        call runit(doit,'-')
        call pausems(100)
        write(currentfile,'(a)') folderreadme(1:lnblnk(folderreadme))
        IUNIT=IFIL+1
        CALL EFOPSEQ(IUNIT,folderreadme,3,IER)
        write(iunit,'(a)')'This folder holds documentation associated'
        write(iunit,'(a)')'with the model e.g. construction documents'
        write(iunit,'(a)')'lists of assumptions or project goals.'
        CALL ERPFREE(IUNIT,ISTAT)

        write(doit,'(4a)') 'mkdir ',mpath(1:lnmp),fs,'rad'
        call runit(doit,'-')
        call pausems(100)
        write(folderreadme,'(5a)') mpath(1:lnmp),fs,'rad',fs,
     &    'Readme.txt'
        write(currentfile,'(a)') folderreadme(1:lnblnk(folderreadme))
        IUNIT=IFIL+1
        CALL EFOPSEQ(IUNIT,folderreadme,3,IER)
        write(iunit,'(a)')'This folder holds Radiance scene files'
        write(iunit,'(a)')'created from the model e.g. *.rif *.mat'
        write(iunit,'(a)')'*.rad and view files.'
        CALL ERPFREE(IUNIT,ISTAT)

        write(radpth,'(3a)')'..',fs,'rad'

C Check that the hvac folder path is standard.
        call GETTOKENS(hvacpth,IW,WORDS)
        lnw=lnblnk(WORDS(IW))
        testw=WORDS(IW)
        if(testw(1:lnw).eq.'hvac'.or.testw(lnw:lnw).eq.'.')then
          write(doit,'(4a)') 'mkdir ',mpath(1:lnmp),fs,'hvac'
          write(hvacpth,'(3a)')'..',fs,'hvac'
        else
          write(doit,'(4a)') 'mkdir ',mpath(1:lnmp),fs,testw(1:lnw)
        endif
        call runit(doit,'-')
        call pausems(100)

C Check that the dbs folder path is standard.
        call GETTOKENS(dbspth,IW,WORDS)
        lnw=lnblnk(WORDS(IW))
        testw=WORDS(IW)
        if(testw(1:lnw).eq.'dbs'.or.testw(lnw:lnw).eq.'.')then
          write(doit,'(4a)') 'mkdir ',mpath(1:lnmp),fs,'dbs'
          write(dbspth,'(3a)')'..',fs,'dbs'
          write(folderreadme,'(5a)') mpath(1:lnmp),fs,'dbs',fs,
     &    'Readme.txt'
        else
          write(doit,'(4a)') 'mkdir ',mpath(1:lnmp),fs,testw(1:lnw)
          write(folderreadme,'(5a)') mpath(1:lnmp),fs,testw(1:lnw),fs,
     &    'Readme.txt'
        endif
        call runit(doit,'-')
        call pausems(100)
        write(currentfile,'(a)') folderreadme(1:lnblnk(folderreadme))
        IUNIT=IFIL+1
        CALL EFOPSEQ(IUNIT,folderreadme,3,IER)
        write(iunit,'(a)')'This folder holds local databases used with'
        write(iunit,'(a)')'the model e.g. materials & constructions'
        write(iunit,'(a)')'which differ from the corporate databases.'
        CALL ERPFREE(IUNIT,ISTAT)

C Check that the bsm (BASESIMP) folder path is standard.
        call GETTOKENS(bsmpth,IW,WORDS)
        lnw=lnblnk(WORDS(IW))
        testw=WORDS(IW)
        if(testw(1:lnw).eq.'bsm'.or.testw(lnw:lnw).eq.'.')then
          write(doit,'(4a)') 'mkdir ',mpath(1:lnmp),fs,'bsm'
          write(dbspth,'(3a)')'..',fs,'bsm'
        else
          write(doit,'(4a)') 'mkdir ',mpath(1:lnmp),fs,testw(1:lnw)
        endif
        call runit(doit,'-')
        call pausems(100)

C Re-establish pwd and then project folders.
        write(path,'(4a)')mpath(1:lnblnk(mpath)),fs,'cfg',fs
        call edisp(iuout,
     &      'System configuration file is located in folder:')
        call edisp(iuout,path)

C Write out the model log file within the model doc folder.
        uname=' '
        call usrname(uname)
        call usrdir(pwd)
        write(modeltitle,'(a)') menu(1:lnblnk(menu))
        lr=lnblnk(cfgroot)
        if(docpth(1:2).eq.'  '.or.docpth(1:2).eq.'./')then
          write(lmodellog,'(2a)') cfgroot(1:lr),'.log'
        elseif(docpth(1:3).eq.'../')then
          write(lmodellog,'(4a)') docpth(1:lnblnk(docpth)),fs,
     &      cfgroot(1:lr),'.log'
        else
          write(lmodellog,'(4a)') docpth(1:lnblnk(docpth)),fs,
     &      cfgroot(1:lr),'.log'
        endif
        write(currentfile,'(a)') lmodellog(1:lnblnk(lmodellog))
        IUNIT=IFIL+1
        CALL EFOPSEQ(IUNIT,lmodellog,3,IER)
        write(iunit,'(2a)')'Project notes for ',LCFGF(1:lnblnk(LCFGF))
        write(iunit,'(2a)')'Title: ',modeltitle(1:lnblnk(modeltitle))
        write(iunit,'(2a)')'In folder: ',pwd(1:lnblnk(pwd))
        write(iunit,'(2a)')'By: ',uname(1:lnblnk(uname))
        call dstamp(dstmp)
        write(iunit,'(2a)')'Date: ',dstmp
        write(iunit,'(a)')'Client: '
        write(iunit,'(a)')'Project reference: '
        write(iunit,'(a)')'_____________________________'
        write(iunit,'(a)')'Notes: '
        write(iunit,'(a)')'  '
        write(iunit,'(a)')'_____________________________'
        write(iunit,'(a)')'  '
        CALL ERPFREE(IUNIT,ISTAT)
        goto 77

      elseif(act(1:1).eq.'i')then

C Assume a standard set of folders to hold the model description.
        IW=2
      endif


C Setup the various paths to distributed folders and then create.
C Check longer paths cause the path variable to have a string buffer overflow.

C << consider uprating path string length to ~84 characters >>

      odir=' '
      call usrdir(odir)
      lnod=lnblnk(odir)
      lncr=lnblnk(cfgroot)
      if(lnod+lncr+6.lt.72)then
        continue
      else
        write(outs,'(6a)')odir(1:lnod),fs,cfgroot(1:lncr),fs,'cfg',fs
        call usrmsg(
     &    'The derived path (below) is > 72 char give shorter root',
     &    outs,'W')
        goto 289
      endif

C Instanciate standard folder names for use within model configuration file.
      write(zonepth,'(3a)')'..',fs,'zones'
      write(netpth,'(3a)')'..',fs,'nets'
      write(ctlpth,'(3a)')'..',fs,'ctl'
      write(aimpth,'(3a)')'..',fs,'aim2'
      write(imgpth,'(3a)')'..',fs,'images'
      write(radpth,'(3a)')'..',fs,'rad'
      write(docpth,'(3a)')'..',fs,'doc'
      write(dbspth,'(3a)')'..',fs,'dbs'
      write(bsmpth,'(3a)')'..',fs,'bsm'
      write(hvacpth,'(3a)')'..',fs,'hvac'
      lncr=lnblnk(cfgroot)
      write(odir,'(a)')cfgroot(1:lncr)
      lnod=lncr
      write(doit,'(2a)') 'mkdir ',cfgroot(1:lncr)
      call usrmsg('Creating folders...','  ','-')
      call runit(doit,'-')
      call pausems(100)
      write(doit,'(4a)') 'mkdir ',cfgroot(1:lncr),fs,'cfg'
      write(cfgpath,'(3a)') cfgroot(1:lncr),fs,'cfg'
      call runit(doit,'-')
      write(folderreadme,'(5a)') cfgroot(1:lncr),fs,'cfg',fs,
     &  'Readme.txt'
      write(currentfile,'(a)') folderreadme(1:lnblnk(folderreadme))
      IUNIT=IFIL+1
      CALL EFOPSEQ(IUNIT,folderreadme,3,IER)
      write(iunit,'(a)')'This folder holds the model configuration'
      write(iunit,'(a)')'[cfg] files and master connection lists'
      write(iunit,'(a)')'[cnn] for the project models.'
      CALL ERPFREE(IUNIT,ISTAT)
      call pausems(100)
      
      write(doit,'(4a)') 'mkdir ',cfgroot(1:lncr),fs,'ctl'
      call runit(doit,'-')
      write(folderreadme,'(5a)') cfgroot(1:lncr),fs,'ctl',fs,
     &  'Readme.txt'
      write(currentfile,'(a)') folderreadme(1:lnblnk(folderreadme))
      IUNIT=IFIL+1
      CALL EFOPSEQ(IUNIT,folderreadme,3,IER)
      write(iunit,'(a)')'This folder holds control definitions for'
      write(iunit,'(a)')'the model e.g. zone & flow & plant.'
      CALL ERPFREE(IUNIT,ISTAT)
      call pausems(100)
      
      write(doit,'(4a)') 'mkdir ',cfgroot(1:lncr),fs,'aim2'
      call runit(doit,'-')
      call pausems(100)
      
      write(doit,'(4a)') 'mkdir ',cfgroot(1:lncr),fs,'zones'
      call runit(doit,'-')
      write(folderreadme,'(5a)') cfgroot(1:lncr),fs,'zones',fs,
     &  'Readme.txt'
      write(currentfile,'(a)') folderreadme(1:lnblnk(folderreadme))
      IUNIT=IFIL+1
      CALL EFOPSEQ(IUNIT,folderreadme,3,IER)
      write(iunit,'(a)')'This folder holds zone-based model files'
      write(iunit,'(a)')'e.g. geometry construction tmc operations'
      CALL ERPFREE(IUNIT,ISTAT)
      call pausems(100)
      
      write(doit,'(4a)') 'mkdir ',cfgroot(1:lncr),fs,'nets'
      call runit(doit,'-')
      write(folderreadme,'(5a)') cfgroot(1:lncr),fs,'nets',fs,
     &  'Readme.txt'
      write(currentfile,'(a)') folderreadme(1:lnblnk(folderreadme))
      IUNIT=IFIL+1
      CALL EFOPSEQ(IUNIT,folderreadme,3,IER)
      write(iunit,'(a)')'This folder holds network decriptions for'
      write(iunit,'(a)')'mass flows and electrical power. '
      CALL ERPFREE(IUNIT,ISTAT)
      call pausems(100)
      
      write(doit,'(4a)') 'mkdir ',cfgroot(1:lncr),fs,'images'
      call runit(doit,'-')
      write(folderreadme,'(5a)') cfgroot(1:lncr),fs,'images',fs,
     &  'Readme.txt'
      write(currentfile,'(a)') folderreadme(1:lnblnk(folderreadme))
      IUNIT=IFIL+1
      CALL EFOPSEQ(IUNIT,folderreadme,3,IER)
      write(iunit,'(a)')'This folder holds images associated with the'
      write(iunit,'(a)')'model e.g. site photos renderings and/or'
      write(iunit,'(a)')'figures which clarify the model.'
      CALL ERPFREE(IUNIT,ISTAT)
      call pausems(100)
      
      write(doit,'(4a)') 'mkdir ',cfgroot(1:lncr),fs,'doc'
      call runit(doit,'-')
      write(folderreadme,'(5a)') cfgroot(1:lncr),fs,'doc',fs,
     &  'Readme.txt'
      write(currentfile,'(a)') folderreadme(1:lnblnk(folderreadme))
      IUNIT=IFIL+1
      CALL EFOPSEQ(IUNIT,folderreadme,3,IER)
      write(iunit,'(a)')'This folder holds documentation associated'
      write(iunit,'(a)')'with the model e.g. construction documents'
      write(iunit,'(a)')'lists of assumptions or project goals.'
      CALL ERPFREE(IUNIT,ISTAT)
      call pausems(100)
      
      write(doit,'(4a)') 'mkdir ',cfgroot(1:lncr),fs,'rad'
      write(folderreadme,'(5a)') cfgroot(1:lncr),fs,'rad',fs,
     &  'Readme.txt'
      call runit(doit,'-')
      write(currentfile,'(a)') folderreadme(1:lnblnk(folderreadme))
      IUNIT=IFIL+1
      CALL EFOPSEQ(IUNIT,folderreadme,3,IER)
      write(iunit,'(a)')'This folder holds Radiance scene files'
      write(iunit,'(a)')'created from the model e.g. *.rif *.mat'
      write(iunit,'(a)')'*.rad and view files.'
      CALL ERPFREE(IUNIT,ISTAT)
      call pausems(100)
      
      write(doit,'(4a)') 'mkdir ',cfgroot(1:lncr),fs,'dbs'
      call runit(doit,'-')
      write(folderreadme,'(5a)') cfgroot(1:lncr),fs,'dbs',fs,
     &  'Readme.txt'
      write(currentfile,'(a)') folderreadme(1:lnblnk(folderreadme))
      IUNIT=IFIL+1
      CALL EFOPSEQ(IUNIT,folderreadme,3,IER)
      write(iunit,'(a)')'This folder holds local databases used with'
      write(iunit,'(a)')'the model e.g. materials & constructions'
      write(iunit,'(a)')'which differ from the corporate databases.'
      CALL ERPFREE(IUNIT,ISTAT)
      call pausems(100)
      
      write(doit,'(4a)') 'mkdir ',cfgroot(1:lncr),fs,'hvac'
      call runit(doit,'-')
      call pausems(100)
      write(doit,'(4a)') 'mkdir ',cfgroot(1:lncr),fs,'bsm'
      call runit(doit,'-')
      call pausems(100)
      call usrmsg('Creating folders...done.','  ','P')

C Re-establish pwd and then project folders.  Note longer paths
C can cause the path variable to have a string buffer overflow.
      odir=' '
      call usrdir(odir)
      lnod=lnblnk(odir)
      lncr=lnblnk(cfgroot)
      if(lnod+lncr+6.lt.72)then
        call edisp(iuout,'  ')
        write(path,'(6a)')odir(1:lnod),fs,cfgroot(1:lncr),fs,'cfg',fs
        write(cfgpath,'(5a)')odir(1:lnod),fs,cfgroot(1:lncr),fs,'cfg'
        call edisp(iuout,
     &    'New model configuration file is in folder:')
        call edisp(iuout,path)
      else
        write(outs,'(6a)')odir(1:lnod),fs,cfgroot(1:lncr),fs,'cfg',fs
        call usrmsg(
     &    'The derived path (below) is > 72 char give shorter root',
     &    outs,'W')
        goto 289
      endif

C Having tested the explicit path reset.
      write(path,'(4a)') cfgroot(1:lncr),fs,'cfg',fs
      write(outs,'(2a)') 'Resetting model path to: ',path
      call edisp(iuout,outs)

C General description of the model.
  52  helptopic='cfg_model_descriptor'
      call gethelptext(helpinsub,helptopic,nbhelp)
      modeltitle='  '
      ltmp=modeltitle
      CALL EASKS(ltmp,' ','Model description?',
     &  72,'Base case model','model description',IER,nbehlp)
      if(ltmp(1:2).ne.'  '.or.lnblnk(ltmp).gt.3)then
        modeltitle=ltmp
      else
        call usrmsg('Sorry, a blank line is not acceptable.',
     &    'Enter a meaningful description (<7 characters)','W')
        goto 52
      endif

C Documentation.
  53  helptopic='cfg_model_log_file'
      call gethelptext(helpinsub,helptopic,nbhelp)
      if(lmodellog(1:2).eq.'  '.or.lmodellog(1:4).eq.'UNKN')then
        lr=lnblnk(cfgroot)
        if(docpth(1:2).eq.'  '.or.docpth(1:2).eq.'./')then
          write(lmodellog,'(2a)') cfgroot(1:lr),'.log'
        elseif(docpth(1:3).eq.'../')then
          write(lmodellog,'(4a)') docpth(1:lnblnk(docpth)),fs,
     &      cfgroot(1:lr),'.log'
        else
          write(lmodellog,'(4a)') docpth(1:lnblnk(docpth)),fs,
     &      cfgroot(1:lr),'.log'
        endif
      endif

C Detect if working on X11 or GTK. The file name returned (lltmp).
C If X11 lltmp will be the file name and if GTK it will include the
C full path to the file.
      iglib = igraphiclib()  ! find out if X11 or GTK or text support only.
      lltmp='  '
      lguess='  '
      lldef='project.log'
      if(iglib.eq.2)then
        call addpath(lmodellog,longtfile,concat)  ! expand path for GTK browser
        write(lguess,'(a)') longtfile(1:lnblnk(longtfile))
      else
        write(lguess,'(a)') lmodellog(1:lnblnk(lmodellog))
      endif
      call edisp(iuout,'  ')  ! echo blank line
      CALL EASKXORGTKF(lguess,
     &  'Project log file:',' ',lldef,lltmp,'log file',IER,nbhelp)

C If user request jump back and re-display the menu.
      if(ier.eq.-3)then
        goto 53  ! cancel detected, restore name and redisplay menu.
      endif

C Ask user whether file is in ../doc and if so use fdroot if GTK.
      if(iglib.eq.2)then
        dok=.true.
        CALL ASKOK('Is this file in the model ../doc folder?',
     &    ' ',OK,dok,3)
        if(OK)then
          call fdroot(lltmp,lpath,fname)
          write(lmodellog,'(3a)') docpth(1:lnblnk(docpth)),fs,
     &      fname(1:lnblnk(fname))
        endif
      else
        write(lmodellog,'(a)') lltmp(1:lnblnk(lltmp))
      endif

C See if file exists, otherwise create it and write basic
C information.
      uname=' '
      call usrname(uname)
      call usrdir(pwd)
      call FINDFIL(lmodellog,XST)
      IUNIT=IFIL+1
      if(XST)then
        CALL LISTAS(IUNIT,lmodellog,IER)
        IF(IER.ne.0)THEN
          dok=.true.
          CALL ASKOK('Problem detected with log file!',
     &               'Try again?',OK,dok,nbhelp)
          IF(OK)GOTO 53
        ENDIF
      else
        write(currentfile,'(a)') lmodellog(1:lnblnk(lmodellog))
        CALL EFOPSEQ(IUNIT,lmodellog,3,IER)
        write(iunit,'(a,a)')'Project notes for ',LCFGF(1:lnblnk(LCFGF))
        write(iunit,'(a,a)')'Title: ',modeltitle(1:lnblnk(modeltitle))
        write(iunit,'(a,a)')'In folder: ',pwd(1:lnblnk(pwd))
        write(iunit,'(a,a)')'By: ',uname(1:lnblnk(uname))
        call dstamp(dstmp)
        write(iunit,'(a,a)')'Date: ',dstmp
        write(iunit,'(a)')'Client: '
        write(iunit,'(a)')'Project reference: '
        write(iunit,'(a)')'_____________________________'
        write(iunit,'(a)')'Notes: '
        write(iunit,'(a)')'  '
        write(iunit,'(a)')'_____________________________'
        write(iunit,'(a)')'  '
        CALL ERPFREE(IUNIT,ISTAT)
      endif

C Allow user to edit the log file. Append to path if
C necessary.  If vi then spawn a new window to do the editing.

C << This would be a good place to offer an internal editing popup
C << which uses GTK facilities.
      dok=.false.
      helptopic='cfg_model_log_file'
      call gethelptext(helpinsub,helptopic,nbhelp)
      CALL ASKOK(' ','Edit log file?',OK,dok,nbhelp)
      IF(OK)then

C Check if Unix-based or DOS based.
        call isunix(unixok)
        if(unixok)then
          call addpath(lmodellog,longtfile,concat)
        else

C If running on a non-unix machine see if there are spaces in the name
C and change any / to \.
          call addpath(lmodellog,longtfile,concat)
          call cmdfiledos(longtfile,longtfiledos,ier)
          longtfile=' '
          longtfile=longtfiledos
        endif
        tmode='graph'
        if(teditor(1:2).eq.'vi')tmode='text'
        write(doit,'(a,2x,a,a)') teditor(1:lnblnk(teditor)),
     &    longtfile(1:lnblnk(longtfile)),' &'
        call runit(doit,tmode)
      endif

C Ask about images and a results summary.
      dok=.false.
      helptopic='cfg_model_images'
      call gethelptext(helpinsub,helptopic,nbhelp)
      CALL ASKOK(' ','Associate images now?',OKI,dok,nbhelp)
      if(OKI)then

C Provide a listing of the current contents of the ../images folder.
        subpath=' '
        lnod=lnblnk(odir)
        write(subpath,'(4a)') odir(1:lnod),fs,
     &    path(1:lnblnk(path)),imgpth(1:lnblnk(imgpth))
        action='fil'
        call getfileslist(subpath,action,nlistf)
        call printfileslist(outs,'p')

        write(limg,'(a)')imgpth(1:lnblnk(imgpth))
        iformat='GIF '
        ifocus='****'
 142    call edimage(limg,iformat,ifocus,iier)
        if(iier.eq.2)then
          continue
        elseif(iier.eq.0)then
          noimg=noimg+1
          imgfmt(noimg)=iformat
          imgfoc(noimg)=ifocus

C Edit image description if newer configuration file.
          if(icfgv.gt.3)then
            tcname=imgdoc(noimg)
            CALL EASKS248(tcname,' ','Image notes:',
     &        72,'associated image','image notes',IER,nbhelp)
            if(tcname(1:2).ne.'  ')imgdoc(noimg)=tcname
          endif
        endif
        if(noimg.lt.MIMG)then
          dok=.false.
          CALL ASKOK(' ',' Another image?',OK,dok,nbhelp)
          if(OK)then
            write(limg,'(a)')imgpth(1:lnblnk(imgpth))
            iformat='GIF '
            ifocus='****'
            goto 142
          endif
        endif
      endif

      if(act(1:1).eq.'i')then

C Ask for site position.
        helptopic='cfg_latitude_longitude'
        call gethelptext(helpinsub,helptopic,nbhelp)
        CALL EASKR(SLAT,' ','Site latitude?',
     &    -89.9,'W',89.9,'W',55.9,'site latitude',IER,nbhelp)
        IF(IER.EQ.0)sitelat=SLAT

        CALL EASKR(SLON,
     &   'Longitude difference from local time meridian (east +ve)?',
     &   ':',-15.0,'W',15.0,'W',-4.1,'longitude difference',IER,nbhelp)
        IF(IER.EQ.0)sitelongdif=SLON

C Ask for year, initialise and then print out the calendar if the
C user does not cancel the operation. Because there have been no
C day types created use a call to calenmanage to instanciate an
C initial set of days.
        helptopic='cfg_model_year'
        call gethelptext(helpinsub,helptopic,nbhelp)
        iyeart=iyear
        CALL EASKI(IYEART,' ','Assessment year?',
     &    1900,'W',2051,'W',2007,'assessment year',IERI,nbhelp)
        if(ieri.eq.-3) then
          call calenmanage('i',ier)
        else
          iyear=iyeart
          call calenmanage('i',ier)

C Comment out printing of initial calendar as it can cause a crash
C on some 64bit computers.
C          loop=8
C          loopst=1
C          call calenprint(iuout,'t',iyear,loopst,loop)
C          call edisp(iuout,' ')
C          call calenprint(iuout,'g',iyear,loopst,loop)
C          call edisp(iuout,' ')
        endif

        helptopic='cfg_model_other'
        call gethelptext(helpinsub,helptopic,nbhelp)
        CALL PHELPD('new site',nbhelp,'-',0,0,IER)
      endif

C Create a system configuration file based on the registration
C information.
   77 call tstamp('>','PRJ: create configuration')
      call tstamp('>',LCFGF)
      CALL EMKCFG('-',IER)
      if(ier.eq.0)then
        CFGOK=.TRUE.

C Open core of databases if not already done so.
        if(MLDBOK.and.MATDBOK.and.OPTKOK)then
          continue
        else
          call opendb(ier)
          if(ier.ne.0)then
            call usrmsg('Possible problem with the Constructions',
     &               'or Optical Properties db. Please check.','W')
            ier = 0
          endif
        endif

C If initial registration re-read the newly created model cfg
C file so that any explicit file paths for databases can get
C recognised as *std items and then written out in *std format.
        if(act(1:1).eq.'i')then
          itrcc=0
          MODE='ALL '
          IUF=IPRODB   ! assign second file unit to the events db unit
          CALL ERSYS(LCFGF,IFCFG,IUF,MODE,itrcc,IER)
          call pausems(100)
          CALL EMKCFG('-',IER)
        endif

C Refocus prj on the model cfg folder and re-load the model cfg file
C as is done in cadio.F
        call pausems(1000)
        call refocus(cfgpath,thecfgis,iret)
        if(iret.eq.0)then
          continue
        elseif(iret.eq.-2)then
          call edisp(iuout,
     &    'Features using Radiance calls may not work as expected.')
        endif
      endif
      return
      end

C ********** refocus
C refucus restarts prj within the model cfg folder and focused
C on a specific cfg file. After the restart it calls a STOP on
C the current prj. ier returned as -2 if user declines refocus.
      subroutine refocus(cfgpath,thecfgis,ier)
#include "building.h"
#include "model.h"

C Parameters
      character cfgpath*144 ! full path to cfg folder for refocus
      character thecfgis*72 ! cfg file only & cfg/cfg_file
      integer ier  ! zero if ok -2 if user delines refocus

      common/pophelp/h(60)
      character h*72
      common/OUTIN/IUOUT,IUIN

      integer lnblnk  ! function definition
      character doit*248
      character tmode*8
      logical unixok

C Set folder separator (fs) to \ or / as required.
      ier=0
      call isunix(unixok)
      write(6,*) 'using refocus'

C Test if we can start a new prj within the model cfg folder.
      if(unixok)then
        h(1)='To provide full functionality prj should'
        h(2)='restart in the model cfg folder. When prj'
        h(3)='has restarted this instance of prj will'
        h(4)='quit after a few seconds.'
        CALL EASKABC(
     &    'To provide full functionality prj needs to restart',
     &    'focused on the model cfg folder','yes restart',
     &    'continue','abort prj',IRT,4)
        if(irt.eq.1)then
          call pausems(1000)
          call usrmsg('Invoking new prj focused on the model',
     &      ' and this instance will close.','P')
          write(doit,'(5a)') 'cd ',cfgpath(1:lnblnk(cfgpath)),
     &      '; prj -file ',thecfgis(1:lnblnk(thecfgis)),' &'
          call edisp248(iuout,doit,100)
          call runit(doit,'-')
          call pausems(1000)

C Clear allocatable arrays
          CALL DeallocateAllArrays
          CALL EPAGEND
          STOP
        elseif(irt.eq.2)then
          ier=-2  ! pass back to calling code
          return
        elseif(irt.eq.3)then
          CALL DeallocateAllArrays
          CALL EPAGEND
          STOP
        endif
      else

C For Non-unix place a & between the commands and also set the
C text string tmode='bg' to partially mimic background mode.
        call edisp(iuout,'proposed command:')
        write(doit,'(4a)') 'cd ',cfgpath(1:lnblnk(cfgpath)),
     &    ' & prj -file ',thecfgis(1:lnblnk(thecfgis))
        call edisp248(iuout,doit,100)
        call edisp(iuout,'  ')
        call pausems(1000)
        h(1)='To provide full functionality prj should'
        h(2)='restart in the copied model cfg folder. When'
        h(3)='prj has restarted this instance of prj will'
        h(4)='persist until you close the DOS cmd window.'
        CALL EASKABC(
     &    'To provide full functionality prj needs to restart',
     &    'focused on the model cfg folder','yes restart',
     &    'continue','abort prj',IRT,4)
        if(irt.eq.1)then

C Separate two dos cmds with & but do not include & at end to force background
C mode via runit parameter. The initial prj tends not to close unless the
C DOS cmd window is Xed.
          doit='  '
          write(doit,'(4a)') 'cd ',cfgpath(1:lnblnk(cfgpath)),
     &      ' & prj -file ',thecfgis(1:lnblnk(thecfgis))
          call edisp248(iuout,doit,100)
          call pausems(1000)
C << see if we can implement a bg for tmode in esru_lib.F
          tmode='bg'
          call runit(doit,tmode)
          call pausems(1000)

C Clear allocatable arrays
          CALL DeallocateAllArrays
          CALL EPAGEND
          STOP
        elseif(irt.eq.2)then
          ier=-2
          return
        elseif(irt.eq.3)then
          CALL DeallocateAllArrays
          CALL EPAGEND
          STOP
        endif
      endif
      return

      end  ! of refocus
     
C ********** pfolders
C Maintain model folders.
      subroutine pfolders(ier)
#include "building.h"
#include "model.h"

C geometry.h provides commons G0 and G2.
#include "geometry.h"
#include "espriou.h"
      
      integer lnblnk  ! function definition

      common/FILEP/IFIL
      common/OUTIN/IUOUT,IUIN
      
      common/rpath/path

C External text editor.
      common/texted/tedlbl,teditor

      common/FOPENED/CFGOK,MLDBOK,MATDBOK,CTLOK,OPTKOK,CFCDBOK
      common/user/browse

      logical OK,DOK,CFGOK,MLDBOK,MATDBOK,CTLOK,OPTKOK,CFCDBOK
      logical concat,unixok,changedit,browse

      character*72 path,LFIL,odir
      character doit*248,sfile*72,snpfile*72
      character longtfile*144,longtfiledos*144
      character tedlbl*20,teditor*20
      character tmode*8,t24*24,outs*124
      character*33 ITEMS(25)
      character fs*1
      character subpath*72,action*3
      character theprimedirective*8
      integer lnod   ! length for odir
      integer nitems,INO ! max items and current menu item

C Logicals for testing XML
      logical xst  ! equivalent of similar on other side
      
C For help messages
      character helpinsub*24 ! subroutine name
      character helptopic*24 ! string (unique) for topic
      integer nbhelp     ! number of help lines found

      helpinsub='pfolders'  ! set for subroutine

C Set folder separator (fs) to \ or / as required.
      changedit=.false.
      call isunix(unixok)
      if(unixok)then
        fs = char(47)
      else
        fs = char(92)
      endif

C Get the current folder and display options to the user.
C first list any files with .cfg in name.
      odir=' '
      call usrdir(odir)
      lnod=lnblnk(odir)
      lnpath=lnblnk(path)

C If subpath is less than 72 char with odir + path use that
C otherwise just use path.

C Debug...
C      write(6,*) 'usrdir is ',odir(1:lnod)
C      write(6,*) 'path is ',path(1:lnpath)

      subpath=' '
      if(unixok)then
        if(lnod+lnpath+1.le.72)then
          write(subpath,'(3a)') odir(1:lnod),fs,path(1:lnpath)
        else
          write(subpath,'(a)') path(1:lnpath)
        endif
      else

C For native windows do not bother with usrdir.
        write(subpath,'(a)') path(1:lnpath)
      endif
      write(outs,'(2a)') 'configuration files in ',
     &  subpath(1:lnblnk(subpath))
      call edisp(iuout,outs)

#ifdef OSX
C In OSX slightly different logic. If path ls ./ then standard logic
C works. If path begins with /usr/esru then no need to prepend odir.
      if(path(1:2).eq.'./')then
        continue
      elseif(path(1:9).eq.'/usr/esru')then

C Debug.
C        write(6,*) 'OSX path is ',path

        subpath=' '
        write(subpath,'(a)') path(1:lnblnk(path))
        write(outs,'(2a)') 'configuration files in ',
     &    subpath(1:lnblnk(subpath))
        call edisp(iuout,outs)
        call edisp(iuout,' ')
      else
        continue
      endif
#endif

C Remind user of available cfg files.
      action='cfg'
      call getfileslist(subpath,action,nlistf)
      call printfileslist(outs,'p')

  42  continue
      INO=-4
      WRITE(ITEMS(1),'(2A)') '  name: ',LCFGF(1:24)
      WRITE(ITEMS(2),'(2A)') '  path: ',path(1:23)
      WRITE(ITEMS(3),'(A)')  '  .... Relative paths to .... '
      WRITE(ITEMS(4),'(2A)') 'b zones   : ',zonepth(1:20)
      WRITE(ITEMS(5),'(2A)') 'c control : ',ctlpth(1:20)
      WRITE(ITEMS(6),'(2A)') 'd aim2    : ',aimpth(1:20)
      WRITE(ITEMS(7),'(2A)') 'e network : ',netpth(1:20)
      WRITE(ITEMS(8),'(2A)') 'f images  : ',imgpth(1:20)
      WRITE(ITEMS(9),'(2A)') 'g document: ',docpth(1:20)
      WRITE(ITEMS(10),'(A)') '          : '
      WRITE(ITEMS(11),'(2A)')'i radiance: ',radpth(1:20)
      WRITE(ITEMS(12),'(2A)')'j local db: ',dbspth(1:20)
      WRITE(ITEMS(13),'(2A)')'k hvac    : ',hvacpth(1:20)
      WRITE(ITEMS(14),'(2A)')'k BASESIMP: ',bsmpth(1:20)
      ITEMS(15)  =           ' _____________________________ '
      ITEMS(16)  =           'l zone files                   '
      ITEMS(17)  =           'm scan and rewrite model files '
      ITEMS(18)  =           ' _____________________________ '
      WRITE(ITEMS(19),'(A)') 'n list ASCII file              '
      WRITE(ITEMS(20),'(A)') 'o edit ASCII file              '
      WRITE(ITEMS(21),'(A)') 'p list files in esp-r/databases'
      WRITE(ITEMS(22),'(A)') 'q list files in esp-r/climate  '
      WRITE(ITEMS(23),'(A)') 'r read                         '
      ITEMS(24)  =           '? help                         '
      ITEMS(25)  =           '- exit this menu               '
      nitems=25

C Help text for this menu.
      helptopic='cfg_path_overview'
      call gethelptext(helpinsub,helptopic,nbhelp)

      CALL EMENU('Folders and Files',ITEMS,nitems,INO)
      if(INO.EQ.nitems)then
        if(browse)then
          continue
        else
          if(changedit)then
            dok=.true.
            CALL ASKOK(' ','Update system configuration file?',
     &                 OK,dok,nbhelp)
            if(OK)CALL EMKCFG('-',IER)
            if(ier.eq.0)CFGOK=.TRUE.
            changedit=.false.
          endif
        endif
        return
      elseif(ino.eq.nitems-1)then
        helptopic='cfg_path_overview'
        call gethelptext(helpinsub,helptopic,nbhelp)
        CALL PHELPD('folders and files',nbhelp,'-',0,0,IER)
      elseif(ino.eq.nitems-2)then

C Test read of a X file type.
        call edisp(iuout,'No current test.')
      elseif(ino.eq.4)then

        if(.NOT.browse)then
          t24=zonepth
          CALL EASKS(t24,'Relative path from cfg',
     &      'folder to zones folder?',
     &       24,'../zones','relative zones path',IER,nbhelp)
          if(t24(1:2).ne.'  '.and.t24(1:4).ne.'UNKN')then
            zonepth=t24
            changedit=.true.
            sfile=' '
            snpfile=' '
            call browsefilelist('p','zon','geo',sfile,snpfile,nli,iier)
            sfile=' '
            snpfile=' '
            call browsefilelist('p','zon','opr',sfile,snpfile,nli,iier)
            sfile=' '
            snpfile=' '
            call browsefilelist('b','zon','geo',sfile,snpfile,nli,iier)

C Debug.
C            write(6,*) 'selected file is ',sfile
C            write(6,*) 'selected file is ',snpfile
C            write(6,*) 'iier is ',iier,nli

          endif
        else
          sfile=' '
          snpfile=' '
          call browsefilelist('p','zon','geo',sfile,snpfile,nli,iier)
        endif

      elseif(ino.eq.5)then

        if(.NOT.browse)then
          t24=ctlpth
          CALL EASKS(t24,'Relative path from cfg',
     &      'folder to controls folder?',
     &       24,'../ctl','relative controls path',IER,nbhelp)
          if(t24(1:2).ne.'  '.and.t24(1:4).ne.'UNKN')then
            ctlpth=t24
            changedit=.true.
            sfile=' '
            snpfile=' '
            call browsefilelist('p','ctl','fil',sfile,snpfile,nil,iier)
          endif
        else
          sfile=' '
          snpfile=' '
          call browsefilelist('p','ctl','fil',sfile,snpfile,nli,iier)
        endif

      elseif(ino.eq.6)then

        if(.NOT.browse)then
          t24=aimpth
          CALL EASKS(t24,'Relative path from cfg',
     &      'folder to aim2 folder?',
     &       24,'../aim2','relative aim2 path',IER,nbhelp)
          if(t24(1:2).ne.'  '.and.t24(1:4).ne.'UNKN')then
            aimpth=t24
            changedit=.true.
            sfile=' '
            snpfile=' '
            call browsefilelist('p','aim2','fil',sfile,snpfile,nil,iier)
          endif
        else
          sfile=' '
          snpfile=' '
          call browsefilelist('p','aim2','fil',sfile,snpfile,nli,iier)
        endif

      elseif(ino.eq.7)then

        if(.NOT.browse)then
          t24=netpth
          CALL EASKS(t24,'Relative path from cfg',
     &      'folder to networks folder?',
     &       24,'../nets','relative networks path',IER,nbhelp)
          if(t24(1:2).ne.'  '.and.t24(1:4).ne.'UNKN')then
            netpth=t24
            changedit=.true.
            sfile=' '
            snpfile=' '
            call browsefilelist('p','net','afn',sfile,snpfile,nli,iier)
            call browsefilelist('p','net','gnf',sfile,snpfile,nli,iier)
          endif
        else
          sfile=' '
          snpfile=' '
          call browsefilelist('p','net','afn',sfile,snpfile,nli,iier)
          call browsefilelist('p','net','gnf',sfile,snpfile,nli,iier)
        endif

      elseif(ino.eq.8)then

        if(.NOT.browse)then
          t24=imgpth
          CALL EASKS(t24,'Relative path from cfg',
     &      'folder to images folder?',
     &       24,'../images','relative img path',IER,nbhelp)
          if(t24(1:2).ne.'  '.and.t24(1:4).ne.'UNKN')then
            imgpth=t24
            changedit=.true.
            sfile=' '
            snpfile=' '
            call browsefilelist('p','img','fil',sfile,snpfile,nli,iier)
          endif
        else
          sfile=' '
          snpfile=' '
          call browsefilelist('p','img','fil',sfile,snpfile,nli,iier)
        endif

      elseif(ino.eq.9)then

        if(.NOT.browse)then
          t24=docpth
          CALL EASKS(t24,'Relative path from cfg',
     &       'folder to documents folder?',24,'../doc',
     &        'relative doc path',IER,nbhelp)
          if(t24(1:2).ne.'  '.and.t24(1:4).ne.'UNKN')then
            docpth=t24
            changedit=.true.
            sfile=' '
            snpfile=' '
            call browsefilelist('p','doc','fil',sfile,snpfile,nli,iier)
          endif
        else
          sfile=' '
          snpfile=' '
          call browsefilelist('p','doc','fil',sfile,snpfile,nli,iier)
        endif

      elseif(ino.eq.10)then

C Nothing uses this tmp option currently.

      elseif(ino.eq.11)then

        if(.NOT.browse)then
          t24=radpth
          CALL EASKS(t24,'Relative path from cfg folder',
     &      'folder to Radiance folder?',24,'../rad',
     &      'relative radiance path',IER,nbhelp)
          if(t24(1:2).ne.'  '.and.t24(1:4).ne.'UNKN')then
            radpth=t24
            changedit=.true.
            sfile=' '
            snpfile=' '
            call browsefilelist('p','rad','fil',sfile,snpfile,nli,iier)
          endif
        else
          sfile=' '
          snpfile=' '
          call browsefilelist('p','rad','fil',sfile,snpfile,nli,iier)
        endif

      elseif(ino.eq.12)then

C Users (currently) cannot alter dbspth.
C << interface for dbspath here >>
        subpath=' '
        lnod=lnblnk(odir)
        write(subpath,'(4a)') odir(1:lnod),fs,
     &    path(1:lnblnk(path)),dbspth(1:lnblnk(dbspth))
        write(outs,*) 'files in ',subpath
        action='fil'
        call getfileslist(subpath,action,nlistf)
        call printfileslist(outs,'p')

      elseif(ino.eq.13)then

        if(.NOT.browse)then
          t24=hvacpth
          CALL EASKS(t24,'Relative path from cfg',
     &      'folder to hvac folder?',
     &       24,'../hvac','relative hvac path',IER,nbhelp)
          if(t24(1:2).ne.'  '.and.t24(1:4).ne.'UNKN')then
            hvacpth=t24
            changedit=.true.
            sfile=' '
            snpfile=' '
            call browsefilelist('p','hva','hvac',sfile,snpfile,nli,iier)
          endif
        else
          sfile=' '
          snpfile=' '
          call browsefilelist('p','hva','hvac',sfile,snpfile,nli,iier)
        endif
      elseif(ino.eq.14)then

        if(.NOT.browse)then
          t24=bsmpth
          CALL EASKS(t24,'Relative path from cfg',
     &      'folder to BASESIMP folder?',
     &       24,'../bsm','relative bsm path',IER,nbhelp)
          if(t24(1:2).ne.'  '.and.t24(1:4).ne.'UNKN')then
            bsmpth=t24
            changedit=.true.
            sfile=' '
            snpfile=' '
            call browsefilelist('p','bsm','bsm',sfile,snpfile,nli,iier)
          endif
        else
          sfile=' '
          snpfile=' '
          call browsefilelist('p','bsm','bsm',sfile,snpfile,nli,iier)
        endif

      elseif(ino.eq.16)then

C Zone file composition.
        IC=-1
 254    CALL EASKGEOF('Select a zone to focus on:',CFGOK,IC,'-',IER)
        IF(IC.EQ.0.OR.IC.EQ.-1)GOTO 42
        CALL EDBUILD(IC,IER)

C Loop back and see if another zone is requested.
        IC=-1
        GOTO 254

      elseif(ino.eq.17)then

C Option to scan in current model files and then write them out
C again, updating the file format as per value of igupgrade.
C << to be done >>

      elseif(ino.eq.19)then

C List out an ASCII file.
    6   CALL EASKS(LFIL,' ','ASCII file name?',
     &    72,' ','ASCII file',IER,nbhelp)

        IUNIT=IFIL+1
        CALL LISTAS(IUNIT,LFIL,IER)
        IF(IER.LT.0)THEN
          dok=.false.
          CALL ASKOK('Problem detected while trying to open',
     &               ' ASCII file. Try again?',OK,dok,nbhelp)
          IF(OK)GOTO 6
        ENDIF
      elseif(ino.eq.20)then

C Allow user to edit a file. Append to path if necessary.
C If vi then spawn a new window to do the editing in.
        CALL EASKS(LFIL,' ','ASCII file to edit?',
     &              72,' ','ASCII file',IER,nbhelp)
        call isunix(unixok)
        if(unixok)then
          call addpath(LFIL,longtfile,concat)
        else

C If running on a non-unix machine see if there are spaces in the name
C and change any / to \.
          call addpath(LFIL,longtfile,concat)
          call cmdfiledos(longtfile,longtfiledos,ier)
          longtfile=' '
          longtfile=longtfiledos
        endif
        tmode='graph'
        if(teditor(1:2).eq.'vi')tmode='text'
        write(doit,'(a,2x,a,a)') teditor(1:lnblnk(teditor)),
     &    longtfile(1:lnblnk(longtfile)),' &'
        call runit(doit,tmode)

      elseif(ino.eq.21)then

C List files in corporate databases folder.
        sfile=' '
        snpfile=' '
        call browsefilelist('p','dbm','fil',sfile,snpfile,nli,iier)
        if(nli.gt.MFFOLD) then
          call usrmsg('The folder includes more files than the list',
     &      'can handle - please be careful!','W')
        endif
      elseif(ino.eq.22)then

C List files in corporate climate folder..
        sfile=' '
        snpfile=' '
        call browsefilelist('p','clm','fil',sfile,snpfile,nli,iier)
        if(nli.gt.MFFOLD) then
          call usrmsg('The folder includes more files than the list',
     &      'can handle - please be careful!','W')
        endif
      else
        goto 42
      endif
      goto 42

      end

C ********* edimage
C Edimage controls specification of an image associated with the
C model. All information is passed via parameters.
C limg (char*72) is the image file name
C iformat (char*4) image format 'GIF ' 'TIF ' 'XBMP' or 'XWD '
C ifocus (char*4) image focus '****' 'FZON' 'FNET' 'FCTL' 'FDFS' or 'FPER'
      subroutine edimage(limg,iformat,ifocus,ier)

C Remove comments to enable debug.
C      integer lnblnk  ! function definition
C      COMMON/FILEP/IFIL
C      common/OUTIN/IUOUT,IUIN
C      COMMON/SPAD/MMOD,LIMIT,LIMTTY

      logical XST,OK,dok
      character iformat*4,ifocus*4,limg*72,t5*5
C      character outs*124
      integer IRT,IRF  ! for radio buttons

C For help messages
      character helpinsub*24 ! subroutine name
      character helptopic*24 ! string (unique) for topic
      integer nbhelp     ! number of help lines found

      helpinsub='edimage'  ! set for subroutine

C Display current information.
      ier=0
C      call edisp(iuout,'The current image is:')
C      call edisp(iuout,'Format  Focus File')
C      write(outs,'(6a)') ' ',iformat,' ',ifocus,' ',
C     &  limg(1:lnblnk(limg))
C      call edisp(iuout,outs)

 242  helptopic='image_overview'
      call gethelptext(helpinsub,helptopic,nbhelp)
      CALL EASKS(limg,' ','Name of image file?',
     &          72,'first.gif','image file',IER,nbhelp)
      if(limg(1:2).eq.'  '.or.limg(1:4).eq.'UNKN')goto 242
      XST=.false.
      call findfil(limg,XST)
      if(.NOT.XST)then
        dok=.false.
        CALL ASKOK('Could not locate image file.','Try again?',
     &             OK,dok,nbhelp)
        if(OK)then
          goto 242
        else
          ier=2
          return
        endif
      endif

      IRT=1
      call EASKATOG(' ','Image format?',
     &  'GIF','TIFF','XBM','XWD','other',' ',' ',IRT,nbhelp)
      if(IRT.eq.1)then
        write(iformat,'(a4)') 'GIF '
      elseif(IRT.eq.2)then
        write(iformat,'(a4)') 'TIF '
      elseif(IRT.eq.3)then
        write(iformat,'(a4)') 'XBMP'
      elseif(IRT.eq.4)then
        write(iformat,'(a4)') 'XWD '
      elseif(IRT.eq.5)then
 145    t5='**** '
        CALL EASKS(t5,' ','Image type character code?',
     &               5,'****','image char code',IER,nbhelp)
        if(t5(1:1).eq.' ')goto 145
        iformat=t5(1:4)
      endif

      helptopic='image_topic'
      call gethelptext(helpinsub,helptopic,nbhelp)
      IRF=1
      call EASKATOG(' ','Image focus?',
     &  'general','building','networks','control',
     &  'CFD','performance','other',IRF,nbhelp)
      if(IRF.eq.1)then
        write(ifocus,'(a4)') '****'
      elseif(IRF.eq.2)then
        write(ifocus,'(a4)') 'FZON'
      elseif(IRF.eq.3)then
        write(ifocus,'(a4)') 'FNET'
      elseif(IRF.eq.4)then
        write(ifocus,'(a4)') 'FCTL'
      elseif(IRF.eq.5)then
        write(ifocus,'(a4)') 'FDFS'
      elseif(IRF.eq.6)then
        write(ifocus,'(a4)') 'FPER'
      else
 144    t5='**** '
        CALL EASKS(t5,' ','Image type character code?',
     &    5,'**** ','usr image focus',IER,nbhelp)
        if(t5(1:1).eq.' ')goto 144
        ifocus=t5(1:4)
      endif
      return
      end

C ************* EDBUILD 
C EDBUILD edits building information for system configuration file.
C ITRU unit number for user output. IER=0 indicates no error. 
      subroutine edbuild(IZ,IER)
#include "building.h"
#include "model.h"
#include "geometry.h"
#include "CFC_common.h"
      
      integer lnblnk  ! function definition

      COMMON/SPAD/MMOD,LIMIT,LIMTTY
      common/OUTIN/IUOUT,IUIN
      common/FILEP/IFIL
      common/rpath/path

      common/user/browse

C CFD
      common/cfdfil/LCFD(MCOM),IFCFD(MCOM)
      common/GR3D100/BLDG3D,ZONE3D(MCOM)
      LOGICAL BLDG3D,ZONE3D
      common/GR3D108/L3DCVS(MCOM),L3DCNC(MCOM),L3DNDC(MCOM),L3DTAQ(MCOM)
      common/MOIST01/MSTROK,MSTRZN(MCOM)
      LOGICAL MSTROK,MSTRZN
      common/MOIST02/LMOIST(MCOM)
      COMMON/GRSD100/IndxSt
      COMMON/GRSD101/LGrdSt

      logical browse
      logical newgeo  ! to use for testing if new/old geometry file.
      character*72 L3DCVS,L3DCNC,L3DNDC,L3DTAQ,LMOIST,LGrdSt
      dimension ITEMS(25)
      character ITEMS*72,head*25,path*72,L*72,ZN*12
      character LCFD*72,outs*124
      character D*72,T15*12,D15*12
      character sfile*72,snpfile*72,fs*1
      logical modcfg,XST,unixok
      integer MITEM,INO ! max items and current menu item

#ifdef OSI
      integer impx,impy,iwe
#else
      integer*8 impx,impy,iwe
#endif

C For help messages
      character helpinsub*24 ! subroutine name
      character helptopic*24 ! string (unique) for topic
      integer nbhelp     ! number of help lines found

      helpinsub='edbuild'  ! set for subroutine

C Note L is a temporary file and D is used for defaul file names.
      modcfg=.FALSE.
      newgeo=.false.  ! assume older format geometry.

C Set folder separator (fs) to \ or / as required.
      call isunix(unixok)
      if(unixok)then
        fs = char(47)
      else
        fs = char(92)
      endif

      IUF=IFIL+1

    3 INO=-4
      IER=0
      ZN=zname(IZ)

C If anything affecting configuration file changed then update it.
      if(modcfg)then
        CALL EMKCFG('-',IER)
        modcfg=.false.
      endif

C Find out the length of each of the files:names to be displayed.
      WRITE(head,'(A,A)') 'Files for ',ZN
      lt=lnblnk(head)
      la=LNBLNK(LGEOM(IZ))
      lb=LNBLNK(LTHRM(IZ))
      lc=LNBLNK(LPROJ(IZ))
      ld=LNBLNK(ZOBS(IZ))
      le=LNBLNK(LTWIN(IZ))
      lf=LNBLNK(LVIEW(IZ))
      lg=LNBLNK(LCGCIN(IZ))
      lh=LNBLNK(LSHAD(IZ))
      li=LNBLNK(LHCCO(IZ))
      ll=LNBLNK(LCFD(IZ))
      lm=LNBLNK(LMOIST(IZ))
      ln=LNBLNK(lcfcin(IZ))

C Find maximum with of text and title. IWA is for required files,
C IWC is for optional files, IW is the overall width.
      IWA=MAX0(36,la+18,lb+18,lc+18,lt)
      if(IWA.le.72)then
        IWB=IWA-18
      else
        IWB=29
      endif
      IWC=
     & MAX0(36,ld+22,le+22,lf+22,lg+22,lh+22,li+22,ll+22,lm+22,ln+22)
      if(IWC.le.72)then
        IWD=IWC-22
      else
        IWD=26
      endif
      IW=MAX0(36,IWB+18,IWD+22)

      WRITE(ITEMS(1),'(2A)')  '  path : ',path(1:IW-10)
      WRITE(ITEMS(2),'(2A)')  'a zone name     : ',zname(IZ)
      ITEMS(3)=               ' _____________________________    '
      ITEMS(4)=               ' mandatory descriptions ...       '
      if(LGEOM(IZ)(1:7).eq.'UNKNOWN')then
        WRITE(ITEMS(5),'(A)') 'b  geometry     : not yet created'
      else
        WRITE(ITEMS(5),'(2A)')'b  geometry     : ',LGEOM(IZ)(1:IWB)
      endif
      if(LTHRM(IZ)(1:7).eq.'UNKNOWN')then
        WRITE(ITEMS(6),'(A)') 'c  construction : not yet created'
      else
        WRITE(ITEMS(6),'(2A)')'c  construction : ',LTHRM(IZ)(1:IWB)
      endif
      if(LPROJ(IZ)(1:7).eq.'UNKNOWN')then
        WRITE(ITEMS(7),'(A)') 'd  operations   : not yet created'
      else
        WRITE(ITEMS(7),'(2A)')'d  operations   : ',LPROJ(IZ)(1:IWB)
      endif
      ITEMS(8)=               ' _____________________________    '
      ITEMS(9)=               ' extended descriptions ...        '

C If older model indicate solar obstructions. For newer models such
C data can be in the geometry file. Test for version of geometry file
C and revise label as appropriate.

C << to be done >>
      if(ZOBS(IZ)(1:7).eq.'UNKNOWN')then
        WRITE(ITEMS(10),'(A)') 'e  obstructions     : '
      else
        WRITE(ITEMS(10),'(2A)') 'e  obstructions     : ',ZOBS(IZ)(1:IWD)
      endif
      if(LTWIN(IZ)(1:7).eq.'UNKNOWN')then
       WRITE(ITEMS(11),'(A)') 'g  transparent cnstr: '
      else
       WRITE(ITEMS(11),'(2A)') 'g  transparent cnstr: ',LTWIN(IZ)(1:IWD)
      endif
      if(LVIEW(IZ)(1:7).eq.'UNKNOWN')then
       WRITE(ITEMS(12),'(A)') 'h  view factors     : '
      else
       WRITE(ITEMS(12),'(2A)') 'h  view factors     : ',LVIEW(IZ)(1:IWD)
      endif
      if(LCGCIN(IZ)(1:7).eq.'UNKNOWN')then
        WRITE(ITEMS(13),'(A)') 'i  casual gain cntrl: '
      else
      WRITE(ITEMS(13),'(2A)') 'i  casual gain cntrl: ',LCGCIN(IZ)(1:IWD)
      endif
      if(LSHAD(IZ)(1:7).eq.'UNKNOWN')then
       WRITE(ITEMS(14),'(A)') 'j  shading          : '
      else
       WRITE(ITEMS(14),'(2A)') 'j  shading          : ',LSHAD(IZ)(1:IWD)
      endif
      if(LHCCO(IZ)(1:7).eq.'UNKNOWN')then
       WRITE(ITEMS(15),'(A)') 'k  convection cntrl : '
      else
       WRITE(ITEMS(15),'(2A)') 'k  convection cntrl : ',LHCCO(IZ)(1:IWD)
      endif
      if(LCFD(IZ)(1:7).eq.'UNKNOWN')then
       WRITE(ITEMS(16),'(A)') 'o  cfd domain       : '
      else
       WRITE(ITEMS(16),'(2A)') 'o  cfd domain       : ',LCFD(IZ)(1:IWD)
      endif
      if(ZONE3D(IZ))then
        WRITE(ITEMS(17),'(A)')'p  adaptive gridding: included'
      else
        WRITE(ITEMS(17),'(A)')'p  adaptive gridding: '
      endif
      if(MSTRZN(IZ))then
       WRITE(ITEMS(18),'(2A)')'q  moisture trans.  : ',
     &                           LMOIST(IZ)(1:IWD)
      else
       WRITE(ITEMS(18),'(A)') 'q  moisture trans.  : '
      endif
      if(IZ.eq.IndxSt)then
       WRITE(ITEMS(19),'(2A)')'r  structured mesh  : ',LGrdSt(1:IWD)
      else
       WRITE(ITEMS(19),'(A)') 'r  structured mesh  : '
      endif
      if(lcfcin(IZ)(1:7).eq.'UNKNOWN')then
       WRITE(ITEMS(20),'(A)') 's  CFC constr: '
      else
       WRITE(ITEMS(20),'(2A)')'s  CFC constr: ',lcfcin(IZ)(1:IWD)
      endif
      ITEMS(21)='  _____________________________    '
      ITEMS(22)='* check existence of zone files    '
      ITEMS(23)='? help                             '
      ITEMS(24)='- exit this menu                   '
      MITEM=24

C Help text for this menu.
      helptopic='cfg_zone_files'
      call gethelptext(helpinsub,helptopic,nbhelp)

      WRITE(head,'(A,A)') 'Files for ',ZN
  2   if(MMOD.EQ.8)then
        impx=0
        impy=0
        iwe=iw
        CALL VWMENU(head,ITEMS,MITEM,impx,impy,IWE,irpx,irpy,INO)
      else
        CALL EMENU(head,ITEMS,MITEM,INO)
      endif
      IF(INO.EQ.MITEM)THEN
        RETURN
      ELSEIF(INO.EQ.MITEM-1)THEN

C Produce help text for the menu.
        helptopic='cfg_zone_files'
        call gethelptext(helpinsub,helptopic,nbhelp)
        CALL PHELPD('utility file contents',nbhelp,'-',0,0,IER)
      ELSEIF(INO.EQ.MITEM-2)THEN

C Check existence of this zone's files.
        IUF=IFIL+1
        call FINDFIL(LPROJ(IZ),XST)
        L=LPROJ(IZ)
        if(.NOT.XST)call usrmsg(' Operations file missing!',L,'W')
        call FINDFIL(LGEOM(IZ),XST)
        L=LGEOM(IZ)
        if(.NOT.XST)call usrmsg(' Geometry file missing!',L,'W')
        call FINDFIL(LTHRM(IZ),XST)
        L=LTHRM(IZ)
        if(.NOT.XST)call usrmsg(' Construct. file missing!',L,'W')
        call FINDFIL(LVIEW(IZ),XST)
        L=LVIEW(IZ)
        if(.NOT.XST.and.IVF(IZ).eq.1)call usrmsg(
     &     ' View factors file missing!',L,'W')
        call FINDFIL(ZOBS(IZ),XST)
        L=ZOBS(IZ)
        if(.NOT.XST.and.IOBS(IZ).eq.1)call usrmsg(
     &    ' Obstructions file missing!',L,'W')
        call FINDFIL(LHCCO(IZ),XST)
        L=LHCCO(IZ)
        if(.NOT.XST.and.IHC(IZ).eq.1)call usrmsg(
     &     ' Heat Transfer coefficients file missing!',L,'W')
        call FINDFIL(LTWIN(IZ),XST)
        L=LTWIN(IZ)
        if(.NOT.XST.and.ITW(IZ).eq.1)call usrmsg(
     &     ' TMC file missing!',L,'W')
        call FINDFIL(LCGCIN(IZ),XST)
        L=LCGCIN(IZ)
        if(.NOT.XST.and.ICGC(IZ).eq.1)call usrmsg(
     &    ' Casual gain control file missing!',L,'W')
        call FINDFIL(LSHAD(IZ),XST)
        L=LSHAD(IZ)
        if(.NOT.XST.and.ISI(IZ).eq.1)call usrmsg(
     &    ' Shading/ insolation file missing!',L,'W')
        call FINDFIL(LMOIST(IZ),XST)
        L=LMOIST(IZ)
        if(.NOT.XST.and.MSTRZN(IZ))call usrmsg(
     &    ' moisture transport file missing!',L,'W')
        call FINDFIL(LGrdSt,XST)
        L=LGrdSt
        if(.NOT.XST.and.IZ.eq.IndxSt)call usrmsg(
     &    ' structured mesh file missing!',L,'W')
        call FINDFIL(lcfcin(IZ),XST)
        L=lcfcin(IZ)
        if(.NOT.XST.and.icfc(IZ).eq.1)call usrmsg(
     &     ' CFC file missing!',L,'W')
      ELSEIF(INO.EQ.2)THEN

C Edit zone name - read and then write the geometry file.
        if(browse)then
          call usrmsg('Cannot update model while in browse ',
     &                'mode, you must `own` the model!','W')
          goto 3
        endif
        call georead(IUF,LGEOM(IZ),IZ,1,iuout,IER)
        helptopic='cfg_zone_name'
        call gethelptext(helpinsub,helptopic,nbhelp)
        T15=zname(IZ)
        D15='new_zone'
        CALL EASKS(T15,' ','Zone name?',12,D15,'zone name',
     &    IER,nbhelp)
        if(T15.NE.' ')then
          call st2name(T15,zname(IZ))
          lnzname(IZ)=lnblnk(zname(IZ))  ! updated the length of this string.
          call eclose(gversion(IZ),1.1,0.01,newgeo)
          if(igupgrade.eq.2.and.(.NOT.newgeo))then
            gversion(iz) =1.1
            newgeo = .true.
          endif
          if(newgeo)then
            call geowrite(IUF,LGEOM(IZ),IZ,iuout,3,IER)
          else
            call emkgeo(IUF,LGEOM(IZ),IZ,3,IER)
          endif
          ZN=zname(IZ)
        endif
      ELSEIF(INO.EQ.5)THEN
        if(browse)then
          call usrmsg('Cannot update model while in browse ',
     &                'mode, you must `own` the model!','W')
          goto 3
        endif
        helptopic='cfg_geo_file_name'
        call gethelptext(helpinsub,helptopic,nbhelp)
        if(zonepth(1:2).eq.'  '.or.zonepth(1:2).eq.'./')then
          WRITE(D,'(A,A4)')ZN(1:lnblnk(ZN)),'.geo'
        else
          WRITE(D,'(3A,A4)') zonepth(1:lnblnk(zonepth)),'/',
     &      ZN(1:lnblnk(ZN)),'.geo'
        endif
        L=LGEOM(IZ)
        write(outs,'(3a)')' Geometry file for ',ZN(1:lnblnk(ZN)),' ?'
   41   CALL EASKS(L,outs,' ',72,D,'Z geom',IER,nbhelp)
        IF(L.eq.' '.or.IER.NE.0)GOTO 41
        if(LGEOM(IZ).ne.L)modcfg=.true.
        LGEOM(IZ)=L
      ELSEIF(INO.EQ.6)THEN
        if(browse)then
          call usrmsg('Cannot update model while in browse ',
     &                'mode, you must `own` the model!','W')
          goto 3
        endif
        helptopic='cfg_con_file_name'
        call gethelptext(helpinsub,helptopic,nbhelp)
        if(zonepth(1:2).eq.'  '.or.zonepth(1:2).eq.'./')then
          WRITE(D,'(A,A4)')ZN(1:lnblnk(ZN)),'.con'
        else
          WRITE(D,'(3A,A4)') zonepth(1:lnblnk(zonepth)),'/',
     &      ZN(1:lnblnk(ZN)),'.con'
        endif
        CALL EASKABC(' For zone construction file',
     &      ' :','edit name','browse','continue',IW,nbhelp)
        if(IW.EQ.1)then
          L=LTHRM(IZ)
        elseif(IW.EQ.2)then
          sfile=' '
          snpfile=' '
          call browsefilelist('?','zon','con',sfile,snpfile,nfile,iier)
          if(nfile.gt.0)then
            sfile=' '
            snpfile=' '
            call browsefilelist('b','zon','con',sfile,snpfile,nfile,
     &                          iier)
            if(snpfile(1:2).ne.'  ')then
              write(L,'(3a)')zonepth(1:lnblnk(zonepth)),fs,
     &          snpfile(1:lnblnk(snpfile))
            else
              L=LTHRM(IZ)
            endif
          else
            L=LTHRM(IZ)
          endif
        elseif(IW.EQ.3)then
          goto 3
        endif
        write(outs,'(3a)')' Construction file for ',ZN(1:lnblnk(ZN)),
     &    ' ?'
   42   CALL EASKS(L,outs,' ',72,D,'Z constr',IER,nbhelp)
        IF(L.eq.' '.or.IER.NE.0)GOTO 42
        if(LTHRM(IZ).ne.L)modcfg=.true.
        LTHRM(IZ)=L
      ELSEIF(INO.EQ.7)THEN
        if(browse)then
          call usrmsg('Cannot update model while in browse ',
     &                'mode, you must `own` the model!','W')
          goto 3
        endif
        helptopic='cfg_opr_file_name'
        call gethelptext(helpinsub,helptopic,nbhelp)
        if(zonepth(1:2).eq.'  '.or.zonepth(1:2).eq.'./')then
          WRITE(D,'(A,A4)')ZN(1:lnblnk(ZN)),'.opr'
        else
          WRITE(D,'(3A,A4)') zonepth(1:lnblnk(zonepth)),'/',
     &      ZN(1:lnblnk(ZN)),'.opr'
        endif
        CALL EASKABC(' For zone operation file',
     &      ' :','edit name','browse','continue',IW,nbhelp)
        if(IW.EQ.1)then
          L=LPROJ(IZ)
        elseif(IW.EQ.2)then
          sfile=' '
          snpfile=' '
          call browsefilelist('?','zon','opr',sfile,snpfile,nfile,iier)
          if(nfile.gt.0)then
            sfile=' '
            snpfile=' '
            call browsefilelist('b','zon','opr',sfile,snpfile,nfile,
     &                           iier)
            if(snpfile(1:2).ne.'  ')then
              write(L,'(3a)')zonepth(1:lnblnk(zonepth)),fs,
     &          snpfile(1:lnblnk(snpfile))
            else
              L=LPROJ(IZ)
            endif
          else
            L=LPROJ(IZ)
          endif
        elseif(IW.EQ.3)then
          goto 3
        endif
        write(outs,'(3a)')' Operations file for ',ZN(1:lnblnk(ZN)),' ?'
   40   CALL EASKS(L,outs,' ',72,D,'Z operations',IER,nbhelp)
        IF(L.eq.' '.or.IER.NE.0)GOTO 40
        if(LPROJ(IZ).ne.L)modcfg=.true.
        LPROJ(IZ)=L
      ELSEIF(INO.EQ.10)THEN

C If older model then offer browsing. If v1.1 geometry then there
C is no need for obstruction file.
        if(browse)then
          call usrmsg('Cannot update model while in browse ',
     &                'mode, you must `own` the model!','W')
          goto 3
        endif
        call eclose(gversion(IZ),1.1,0.01,newgeo)
        if(newgeo)then
          call usrmsg(
     &      'Solar obstructions are held in zone geometry file',
     &      'so this option not applicable.','W')
        else
          helptopic='cfg_obs_file_name'
          call gethelptext(helpinsub,helptopic,nbhelp)
          if(IOBS(IZ).eq.1)then
            CALL EASKABCD(' For zone obstruction file',' :',
     &        'edit name','browse','dereference it','continue',
     &        IW,nbhelp)
            if(IW.EQ.1)then
              L=ZOBS(IZ)
            elseif(IW.EQ.2)then
              sfile=' '
              snpfile=' '
              call browsefilelist('?','zon','obs',sfile,snpfile,nfile,
     &                          iier)
              if(nfile.gt.0)then
                sfile=' '
                snpfile=' '
                call browsefilelist('b','zon','obs',sfile,snpfile,
     &                          nfile,iier)
                if(snpfile(1:2).ne.'  ')then
                  write(L,'(3a)')zonepth(1:lnblnk(zonepth)),fs,
     &              snpfile(1:lnblnk(snpfile))
                else
                  L=ZOBS(IZ)
                endif
              else
                L=ZOBS(IZ)
              endif
            elseif(IW.EQ.3)then
              ZOBS(IZ)=' '
              IOBS(IZ)=0
              modcfg=.TRUE.
              goto 3
            elseif(IW.EQ.4)then
              goto 3
            endif
          else

C There is no zone obstructions file so offer restricted choice.
            CALL EASKABC(' For zone obstruction file',
     &        ' :','edit name','browse','continue',IW,nbhelp)
            if(IW.EQ.1)then
              L=' '
            elseif(IW.EQ.2)then
              sfile=' '
              snpfile=' '
              call browsefilelist('?','zon','obs',sfile,snpfile,nfile,
     &                          iier)
              if(nfile.gt.0)then
                sfile=' '
                snpfile=' '
                call browsefilelist('b','zon','obs',sfile,snpfile,
     &                          nfile,iier)
                if(snpfile(1:2).ne.'  ')then
                  write(L,'(3a)')zonepth(1:lnblnk(zonepth)),fs,
     &              snpfile(1:lnblnk(snpfile))
                else
                  L=ZOBS(IZ)
                endif
              else
                L=ZOBS(IZ)
              endif
            elseif(IW.EQ.3)then
              goto 3
            endif
          endif
          if(zonepth(1:2).eq.'  '.or.zonepth(1:2).eq.'./')then
            WRITE(D,'(A,A4)')ZN(1:lnblnk(ZN)),'.obs'
          else
            WRITE(D,'(3A,A4)') zonepth(1:lnblnk(zonepth)),'/',
     &        ZN(1:lnblnk(ZN)),'.obs'
          endif
          write(outs,'(3a)')'Obstruction file for ',ZN(1:lnblnk(ZN)),'?'
   47     CALL EASKS(L,outs,' ',72,D,'Z obstr',IER,nbhelp)
          IF(L(1:1).eq.' '.or.IER.NE.0)GOTO 47
          ZOBS(IZ)=L
          IOBS(IZ)=1
          modcfg=.TRUE.
        endif
      ELSEIF(INO.EQ.11)THEN
        if(browse)then
          call usrmsg('Cannot update model while in browse ',
     &                'mode, you must `own` the model!','W')
          goto 3
        endif
        helptopic='cfg_tmc_file_name'
        call gethelptext(helpinsub,helptopic,nbhelp)
        if(ITW(IZ).eq.1)then
          CALL EASKABCD(' For the zone TMC file',' :',
     &      'change name','browse','dereference it','continue',
     &      IW,nbhelp)
          if(IW.EQ.1)then
            L=LTWIN(IZ)
          elseif(IW.EQ.2)then
            sfile=' '
            snpfile=' '
            call browsefilelist('?','zon','tmc',sfile,snpfile,nfile,
     &                           iier)
            if(nfile.gt.0)then
              sfile=' '
              snpfile=' '
              call browsefilelist('b','zon','tmc',sfile,snpfile,nfile,
     &                           iier)
              if(snpfile(1:2).ne.'  ')then
                write(L,'(3a)')zonepth(1:lnblnk(zonepth)),fs,
     &            snpfile(1:lnblnk(snpfile))
              else
                L=LTWIN(IZ)
              endif
            else
              L=LTWIN(IZ)
            endif
          elseif(IW.EQ.3)then
            LTWIN(IZ)=' '
            ITW(IZ)=0
            modcfg=.TRUE.
            goto 3
          elseif(IW.EQ.4)then
            goto 3
          endif
        else

C There is no zone optic file so offer restricted choice.
          CALL EASKABC(' For the zone TMC file',' :',
     &      'edit name','browse','continue',IW,nbhelp)
          if(IW.EQ.1)then
            L=' '
          elseif(IW.EQ.2)then
            sfile=' '
            snpfile=' '
            call browsefilelist('?','zon','tmc',sfile,snpfile,nfile,
     &                          iier)
            if(nfile.gt.0)then
              sfile=' '
              snpfile=' '
              call browsefilelist('b','zon','tmc',sfile,snpfile,nfile,
     &                           iier)
              if(snpfile(1:2).ne.'  ')then
                write(L,'(3a)')zonepth(1:lnblnk(zonepth)),fs,
     &            snpfile(1:lnblnk(snpfile))
              else
                L=LTWIN(IZ)
              endif
            else
              L=LTWIN(IZ)
            endif
          elseif(IW.EQ.3)then
            goto 3
          endif
        endif
        if(zonepth(1:2).eq.'  '.or.zonepth(1:2).eq.'./')then
          WRITE(D,'(A,A4)')ZN(1:lnblnk(ZN)),'.tmc'
        else
          WRITE(D,'(3A,A4)') zonepth(1:lnblnk(zonepth)),'/',
     &      ZN(1:lnblnk(ZN)),'.tmc'
        endif
        write(outs,'(3a)')' TMC file for ',ZN(1:lnblnk(ZN)),' ?'
   51   CALL EASKS(L,outs,' ',72,D,'tmc file',IER,nbhelp)
        IF(L.eq.' '.or.IER.NE.0)GOTO 51
        LTWIN(IZ)=L
        ITW(IZ)=1
        modcfg=.TRUE.
      ELSEIF(INO.EQ.12)THEN
        if(browse)then
          call usrmsg('Cannot update model while in browse ',
     &                'mode, you must `own` the model!','W')
          goto 3
        endif
        helptopic='cfg_vwf_file_name'
        call gethelptext(helpinsub,helptopic,nbhelp)
        if(IVF(IZ).eq.1)then
          CALL EASKABCD(' For zone view factor file',' :',
     &      'change name','browse','dereference it','continue',
     &      IW,nbhelp)
          if(IW.EQ.1)then
            L=LVIEW(IZ)
          elseif(IW.EQ.2)then
            sfile=' '
            snpfile=' '
            call browsefilelist('?','zon','vwf',sfile,snpfile,nfile,
     &                           iier)
            if(nfile.gt.0)then
              sfile=' '
              snpfile=' '
              call browsefilelist('b','zon','vwf',sfile,snpfile,nfile,
     &                            iier)
              if(snpfile(1:2).ne.'  ')then
                write(L,'(3a)')zonepth(1:lnblnk(zonepth)),fs,
     &            snpfile(1:lnblnk(snpfile))
              else
                L=LVIEW(IZ)
              endif
            else
              L=LVIEW(IZ)
            endif
          elseif(IW.EQ.3)then
            LVIEW(IZ)='UNKNOWN'
            IVF(IZ)=0
            modcfg=.TRUE.
            goto 3
          elseif(IW.EQ.4)then
            goto 3
          endif
        else

C There is no zone viewfactor file so offer restricted choice.
          CALL EASKABC(' For zone view factor file',' :',
     &      'edit name','browse','continue',IW,nbhelp)
          if(IW.EQ.1)then
            L=' '
          elseif(IW.EQ.2)then
            sfile=' '
            snpfile=' '
            call browsefilelist('?','zon','vwf',sfile,snpfile,nfile,
     &                          iier)
            if(nfile.gt.0)then
              sfile=' '
              snpfile=' '
              call browsefilelist('b','zon','vwf',sfile,snpfile,nfile,
     &                           iier)
              if(snpfile(1:2).ne.'  ')then
                write(L,'(3a)')zonepth(1:lnblnk(zonepth)),fs,
     &            snpfile(1:lnblnk(snpfile))
              else
                L=LVIEW(IZ)
              endif
            else
              L=LVIEW(IZ)
            endif
          elseif(IW.EQ.3)then
            goto 3
          endif
        endif
        if(zonepth(1:2).eq.'  '.or.zonepth(1:2).eq.'./')then
          WRITE(D,'(A,A4)')ZN(1:lnblnk(ZN)),'.vwf'
        else
          WRITE(D,'(3A,A4)') zonepth(1:lnblnk(zonepth)),'/',
     &      ZN(1:lnblnk(ZN)),'.vwf'
        endif
        write(outs,'(3a)')' Viewfactor file for ',ZN(1:lnblnk(ZN)),' ?'
   46   CALL EASKS(L,outs,' ',72,D,'view factor',IER,nbhelp)
        IF(L.eq.' '.or.IER.NE.0)GOTO 46
        LVIEW(IZ)=L
        IVF(IZ)=1
        modcfg=.TRUE.
      ELSEIF(INO.EQ.13)THEN
        if(browse)then
          call usrmsg('Cannot update model while in browse ',
     &                'mode, you must `own` the model!','W')
          goto 3
        endif
        helptopic='cfg_cgc_file_name'
        call gethelptext(helpinsub,helptopic,nbhelp)
        if(ICGC(IZ).eq.1)then
          CALL EASKABCD(' For zone casual gain control file',' :',
     &      'change name','browse','dereference it','continue',
     &      IW,nbhelp)
          if(IW.EQ.1)then
            L=LCGCIN(IZ)
          elseif(IW.EQ.2)then
            sfile=' '
            snpfile=' '
            call browsefilelist('?','zon','cgc',sfile,snpfile,nfile,
     &                          iier)
            if(nfile.gt.0)then
              sfile=' '
              snpfile=' '
              call browsefilelist('b','zon','cgc',sfile,snpfile,nfile,
     &                          iier)
              if(snpfile(1:2).ne.'  ')then
                write(L,'(3a)')zonepth(1:lnblnk(zonepth)),fs,
     &            snpfile(1:lnblnk(snpfile))
              else
                L=LCGCIN(IZ)
              endif
            else
              L=LCGCIN(IZ)
            endif
          elseif(IW.EQ.3)then
            LCGCIN(IZ)='UNKNOWN'
            ICGC(IZ)=0
            modcfg=.TRUE.
            goto 3
          elseif(IW.EQ.4)then
            goto 3
          endif
        else

C There is no zone casual gain control file so offer restricted choice.
          CALL EASKABC(' For zone casual gain control file',' :',
     &      'edit name','browse','continue',IW,nbhelp)
          if(IW.EQ.1)then
            L=' '
          elseif(IW.EQ.2)then
            sfile=' '
            snpfile=' '
            call browsefilelist('?','zon','cgc',sfile,snpfile,nfile,
     &                          iier)
            if(nfile.gt.0)then
              sfile=' '
              snpfile=' '
              call browsefilelist('b','zon','cgc',sfile,snpfile,nfile,
     &                          iier)
              if(snpfile(1:2).ne.'  ')then
                write(L,'(3a)')zonepth(1:lnblnk(zonepth)),fs,
     &            snpfile(1:lnblnk(snpfile))
              else
                L=LCGCIN(IZ)
              endif
            else
              L=LCGCIN(IZ)
            endif
          elseif(IW.EQ.3)then
            goto 3
          endif
        endif
        if(zonepth(1:2).eq.'  '.or.zonepth(1:2).eq.'./')then
          WRITE(D,'(A,A4)')ZN(1:lnblnk(ZN)),'.cgc'
        else
          WRITE(D,'(3A,A4)') zonepth(1:lnblnk(zonepth)),'/',
     &      ZN(1:lnblnk(ZN)),'.cgc'
        endif
        L=' '
        write(outs,'(3a)')' Casual gain control file for ',
     &    ZN(1:lnblnk(ZN)),' ?'
   52   CALL EASKS(L,outs,' ',72,D,'casual gain control file',
     &    IER,nbhelp)
        IF(L.eq.' '.or.IER.NE.0)GOTO 52
        LCGCIN(IZ)=L
        ICGC(IZ)=1
        modcfg=.TRUE.
      ELSEIF(INO.EQ.14)THEN
        if(browse)then
          call usrmsg('Cannot update model while in browse ',
     &                'mode, you must `own` the model!','W')
          goto 3
        endif
        helptopic='cfg_shd_file_name'
        call gethelptext(helpinsub,helptopic,nbhelp)
        if(ISI(IZ).eq.1)then
          CALL EASKABCD(' For zone shading/ insolation file',' :',
     &      'change name','browse','dereference it','continue',
     &      IW,nbhelp)
          if(IW.EQ.1)then
            L=LSHAD(IZ)
          elseif(IW.EQ.2)then
            sfile=' '
            snpfile=' '
            call browsefilelist('?','zon','shd',sfile,snpfile,nfile,
     &                         iier)
            if(nfile.gt.0)then
              sfile=' '
              snpfile=' '
              call browsefilelist('b','zon','shd',sfile,snpfile,nfile,
     &                           iier)
              if(snpfile(1:2).ne.'  ')then
                write(L,'(3a)')zonepth(1:lnblnk(zonepth)),fs,
     &            snpfile(1:lnblnk(snpfile))
              else
                L=LSHAD(IZ)
              endif
            else
              L=LSHAD(IZ)
            endif
          elseif(IW.EQ.3)then
            LSHAD(IZ)='UNKNOWN'
            ISI(IZ)=0
            modcfg=.TRUE.
            goto 3
          elseif(IW.EQ.4)then
            goto 3
          endif
        else

C There is no zone shading file so offer restricted choice.
          CALL EASKABC(' For zone shading /insolation file',' :',
     &      'edit name','browse','continue',IW,nbhelp)
          if(IW.EQ.1)then
            L=' '
          elseif(IW.EQ.2)then
            sfile=' '
            snpfile=' '
            call browsefilelist('?','zon','shd',sfile,snpfile,nfile,
     &                          iier)
            if(nfile.gt.0)then
              sfile=' '
              snpfile=' '
              call browsefilelist('b','zon','shd',sfile,snpfile,nfile,
     &                           iier)
              if(snpfile(1:2).ne.'  ')then
                write(L,'(3a)')zonepth(1:lnblnk(zonepth)),fs,
     &            snpfile(1:lnblnk(snpfile))
              else
                L=LSHAD(IZ)
              endif
            else
              L=LSHAD(IZ)
            endif
          elseif(IW.EQ.3)then
            goto 3
          endif
        endif
        if(zonepth(1:2).eq.'  '.or.zonepth(1:2).eq.'./')then
          WRITE(D,'(A,A4)')ZN(1:lnblnk(ZN)),'.shd'
        else
          WRITE(D,'(3A,A4)') zonepth(1:lnblnk(zonepth)),'/',
     &      ZN(1:lnblnk(ZN)),'.shd'
        endif
        L=LSHAD(IZ)
        write(outs,'(3a)')' Shading/ insolation file for ',
     &    ZN(1:lnblnk(ZN)),' ?'
   48   CALL EASKS(L,outs,' ',72,D,'shd db',IER,nbhelp)
        IF(L.eq.' '.or.IER.NE.0)GOTO 48
        LSHAD(IZ)=L
        ISI(IZ)=1
        modcfg=.TRUE.
      ELSEIF(INO.EQ.15)THEN
        if(browse)then
          call usrmsg('Cannot update model while in browse ',
     &                'mode, you must `own` the model!','W')
          goto 3
        endif
        helptopic='cfg_htc_file_name'
        call gethelptext(helpinsub,helptopic,nbhelp)
        if(IHC(IZ).eq.1)then
          CALL EASKABCD(' For the convection regime file',' :',
     &      'change name','browse','dereference it','continue',
     &      IW,nbhelp)
          if(IW.EQ.1)then
            L=LHCCO(IZ)
          elseif(IW.EQ.2)then
            sfile=' '
            snpfile=' '
            call browsefilelist('?','zon','htc',sfile,snpfile,nfile,
     &                           iier)
            if(nfile.gt.0)then
              sfile=' '
              snpfile=' '
              call browsefilelist('b','zon','htc',sfile,snpfile,nfile,
     &                             iier)
              if(snpfile(1:2).ne.'  ')then
                write(L,'(3a)')zonepth(1:lnblnk(zonepth)),fs,
     &            snpfile(1:lnblnk(snpfile))
              else
                L=LHCCO(IZ)
              endif
            else
              L=LHCCO(IZ)
            endif
          elseif(IW.EQ.3)then
            LHCCO(IZ)='UNKNOWN'
            IHC(IZ)=0
            modcfg=.TRUE.
            goto 3
          elseif(IW.EQ.4)then
            goto 3
          endif
        else

C There is no zone view factor file so offer restricted choice.
          CALL EASKABC(' For zone convection regime file',' :',
     &      'edit name','browse','continue',IW,nbhelp)
          if(IW.EQ.1)then
            L=' '
          elseif(IW.EQ.2)then
            sfile=' '
            snpfile=' '
            call browsefilelist('?','zon','htc',sfile,snpfile,nfile,
     &                          iier)
            if(nfile.gt.0)then
              sfile=' '
              snpfile=' '
              call browsefilelist('b','zon','htc',sfile,snpfile,nfile,
     &                          iier)
              if(snpfile(1:2).ne.'  ')then
                write(L,'(3a)')zonepth(1:lnblnk(zonepth)),fs,
     &            snpfile(1:lnblnk(snpfile))
              else
                L=LHCCO(IZ)
              endif
            else
              L=LHCCO(IZ)
            endif
          elseif(IW.EQ.3)then
            goto 3
          endif
        endif
        if(zonepth(1:2).eq.'  '.or.zonepth(1:2).eq.'./')then
          WRITE(D,'(A,A4)')ZN(1:lnblnk(ZN)),'.htc'
        else
          WRITE(D,'(3A,A4)') zonepth(1:lnblnk(zonepth)),'/',
     &      ZN(1:lnblnk(ZN)),'.htc'
        endif
        L=LHCCO(IZ)
        write(outs,'(3a)')' Convection regime file for ',
     &    ZN(1:lnblnk(ZN)),' ?'
   49   CALL EASKS(L,outs,' ',72,D,'Z hc',IER,nbhelp)
        IF(L.eq.' '.or.IER.NE.0)GOTO 49
        LHCCO(IZ)=L
        IHC(IZ)=1
        modcfg=.TRUE.
      ELSEIF(INO.EQ.16)THEN
        if(browse)then
          call usrmsg('Cannot update model while in browse ',
     &                'mode, you must `own` the model!','W')
          goto 3
        endif
        helptopic='cfg_cfd_file_name'
        call gethelptext(helpinsub,helptopic,nbhelp)
        if(lnblnk(LCFD(IZ)).eq.0)then
          continue
        elseif(LCFD(IZ)(1:7).eq.'UNKNOWN')then
          continue
        else
          CALL EASKABC(' For the zone domain flow file',
     &      ' :','change name','dereference it','cancel',IW,nbhelp)
          if(IW.EQ.2)then
            LCFD(IZ)='UNKNOWN'
            IFCFD(IZ)=0
            modcfg=.TRUE.
            call usrmsg(
     &      'If there are other CFD domains it may be necessary to',
     &      'save and then reload the model configuration.','W')
            goto 3
          endif
        endif
        if(netpth(1:2).eq.'  '.or.netpth(1:2).eq.'./')then
          WRITE(D,'(A,A4)')ZN(1:lnblnk(ZN)),'.dfd'
        else
          WRITE(D,'(3A,A4)') netpth(1:lnblnk(netpth)),'/',
     &       ZN(1:lnblnk(ZN)),'.dfd'
        endif
        L=LCFD(IZ)
        write(outs,'(3a)')' Domain flow file for ',ZN(1:lnblnk(ZN)),' ?'
   53   CALL EASKS(L,outs,' ',72,D,'Domain flow model file',IER,nbhelp)
        IF(L.eq.' '.or.IER.NE.0)GOTO 53
        LCFD(IZ)=L
        modcfg=.TRUE.
      ELSEIF(INO.EQ.17)THEN
        if(browse)then
          call usrmsg('Cannot update model while in browse ',
     &                'mode, you must `own` the model!','W')
          goto 3
        endif
        helptopic='cfg_grd_file_name'
        call gethelptext(helpinsub,helptopic,nbhelp)
        if(ZONE3D(IZ))then
          CALL EASKABC(' Four addaptive gridding files',
     &      ' :','change names','dereference ','cancel',IW,nbhelp)
          if(IW.EQ.2)then
            L3DCVS(IZ)='UNKNOWN'
            L3DCNC(IZ)='UNKNOWN'
            L3DNDC(IZ)='UNKNOWN'
            L3DTAQ(IZ)='UNKNOWN'
            ZONE3D(IZ)=.false.
            modcfg=.TRUE.
            goto 3
          elseif(IW.EQ.3)then
            goto 3
          endif
        endif
        WRITE(L,'(A,A4)')zname(IZ)(1:lnzname(IZ)),'.cvs'
        D = 'new.cvs'
        write(outs,'(3a)')' 3D control volumes file for ',
     &    ZN(1:lnblnk(ZN)),' ?'
        CALL EASKS(L,outs,' ',72,D,'control volumes',IER,nbhelp)
        if(L(1:2).ne.'  ')L3DCVS(IZ)=L

        WRITE(L,'(A,A4)')zname(IZ)(1:lnzname(IZ)),'.cnc'
        D = 'new.cnc'
        CALL EASKS(L,' 3D volumes connections file?',
     &      ' ',72,D,'control connections file',IER,nbhelp)
        if(L(1:2).ne.'  ')L3DCNC(IZ)=L

        WRITE(L,'(A,A4)')zname(IZ)(1:lnzname(IZ)),'.ndc'
        D = 'new.ndc'
        CALL EASKS(L,' 3D coordinates file?',
     &      ' ',72,D,'3D coordinates file',IER,nbhelp)
        if(L(1:2).ne.'  ')L3DNDC(IZ)=L

        WRITE(L,'(A,A4)')zname(IZ)(1:lnzname(IZ)),'.3dt'
        D = 'new.3dt'
        CALL EASKS(L,' 3D node temperatures file?',
     &      ' ',72,D,'3D temperatures file',IER,nbhelp)
        if(L(1:2).ne.'  ')L3DTAQ(IZ)=L
        ZONE3D(IZ)=.true.
        modcfg=.TRUE.
      ELSEIF(INO.EQ.18)THEN
        if(browse)then
          call usrmsg('Cannot update model while in browse ',
     &                'mode, you must first `own` the model!','W')
          goto 3
        endif
        helptopic='moist_transp_file_name'
        call gethelptext(helpinsub,helptopic,nbhelp)
        if(MSTRZN(IZ))then
          CALL EASKABC(' For moisture transport file',
     &      ' :','change name','dereference it','cancel',IW,nbhelp)
          if(IW.EQ.2)then
            LMOIST(IZ)=' '
            MSTRZN(IZ)=.false.
            goto 3
          elseif(IW.EQ.3)then
            goto 3
          endif
        endif
        WRITE(L,'(A,A4)')zname(IZ)(1:lnzname(IZ)),'.mst'
        D = 'new.mst'
        write(outs,'(3a)')' Moisture transport file for ',
     &    ZN(1:lnblnk(ZN)),' ?'
        CALL EASKS(L,outs,' ',72,D,'moisture file',IER,nbhelp)
        if(L(1:2).ne.'  ')then
          LMOIST(IZ)=L
          MSTRZN(IZ)=.true.
        endif
      ELSEIF(INO.EQ.19)THEN
        if(browse)then
          call usrmsg('Cannot update model while in browse ',
     &                'mode, you must `own` the model!','W')
          goto 3
        endif
        helptopic='struc_mesh_file_name'
        call gethelptext(helpinsub,helptopic,nbhelp)
        if(IZ.eq.IndxSt)then
          CALL EASKABC(' For structured mesh file',
     &      ' :','change name','dereference it','cancel',IW,nbhelp)
          if(IW.EQ.2)then
            LGrdSt=' '
            IndxSt=0
            goto 3
          elseif(IW.EQ.3)then
            goto 3
          endif
        endif
        WRITE(L,'(A,A4)')zname(IZ)(1:lnzname(IZ)),'.csm'
        D = 'new.csm'
        write(outs,'(3a)')' Structured mesh file for ',
     &    ZN(1:lnblnk(ZN)),' ?'
        CALL EASKS(L,outs,' ',72,D,'structured file',IER,nbhelp)
        if(L(1:2).ne.'  ')then
          LGrdSt=L
          IndxSt=IZ
        endif
      ELSEIF(INO.EQ.20)THEN
        if(browse)then
          call usrmsg('Cannot update model while in browse ',
     &                'mode, you must `own` the model!','W')
          goto 3
        endif
        helptopic='cfg_cfc_file_name'
        call gethelptext(helpinsub,helptopic,nbhelp)
        if(icfc(IZ).eq.1)then
          CALL EASKABCD(' For the zone CFC file',' :',
     &      'change name','browse','dereference it','continue',
     &      IW,nbhelp)
          if(IW.EQ.1)then
            L=lcfcin(IZ)
          elseif(IW.EQ.2)then
            sfile=' '
            snpfile=' '
            call browsefilelist('?','zon','cfc',sfile,snpfile,nfile,
     &                           iier)
            if(nfile.gt.0)then
              sfile=' '
              snpfile=' '
              call browsefilelist('b','zon','cfc',sfile,snpfile,nfile,
     &                           iier)
              if(snpfile(1:2).ne.'  ')then
                write(L,'(3a)')zonepth(1:lnblnk(zonepth)),fs,
     &            snpfile(1:lnblnk(snpfile))
              else
                L=lcfcin(IZ)
              endif
            else
              L=lcfcin(IZ)
            endif
          elseif(IW.EQ.3)then
            lcfcin(IZ)=' '
            icfc(IZ)=0
            modcfg=.TRUE.
            goto 3
          elseif(IW.EQ.4)then
            goto 3
          endif
        else

C There is no zone cfc file so offer restricted choice.
          CALL EASKABC(' For the zone CFC file',' :',
     &      'edit name','browse','continue',IW,nbhelp)
          if(IW.EQ.1)then
            L=' '
          elseif(IW.EQ.2)then
            sfile=' '
            snpfile=' '
            call browsefilelist('?','zon','cfc',sfile,snpfile,nfile,
     &                          iier)
            if(nfile.gt.0)then
              sfile=' '
              snpfile=' '
              call browsefilelist('b','zon','cfc',sfile,snpfile,nfile,
     &                           iier)
              if(snpfile(1:2).ne.'  ')then
                write(L,'(3a)')zonepth(1:lnblnk(zonepth)),fs,
     &            snpfile(1:lnblnk(snpfile))
              else
                L=lcfcin(IZ)
              endif
            else
              L=lcfcin(IZ)
            endif
          elseif(IW.EQ.3)then
            goto 3
          endif
        endif
        if(zonepth(1:2).eq.'  '.or.zonepth(1:2).eq.'./')then
          WRITE(D,'(A,A4)')ZN(1:lnblnk(ZN)),'.cfc'
        else
          WRITE(D,'(3A,A4)') zonepth(1:lnblnk(zonepth)),'/',
     &      ZN(1:lnblnk(ZN)),'.cfc'
        endif
        write(outs,'(3a)')' CFC file for ',ZN(1:lnblnk(ZN)),' ?'
  511   CALL EASKS(L,outs,' ',72,D,'cfc file',IER,nbhelp)
        IF(L.eq.' '.or.IER.NE.0)GOTO 511
        lcfcin(IZ)=L
        icfc(IZ)=1
        modcfg=.TRUE.
      ELSE
        INO=-4
        GOTO 2
      ENDIF
      INO=-4
      GOTO 3

      END

