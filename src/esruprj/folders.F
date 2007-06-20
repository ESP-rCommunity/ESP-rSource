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
C  EDBUILD: edits building information for system configuration file.

C ********** pregist
C Register a new project. Parameter act = 'i' then initial pass.
      subroutine pregist(act,ier)
#include "building.h"
#include "espriou.h"
C espriou.h provides currentfile.

      common/FILEP/IFIL
      common/OUTIN/IUOUT,IUIN
      common/pophelp/h(60)
      common/C1/NCOMP,NCON
      common/C2/LSNAM,NCCODE(MCOM),LPROJ(MCOM),LGEOM(MCOM),
     &          LSHAD(MCOM),LTHRM(MCOM),INDUTL(MCOM),LUTIL(MCOM)
      common/C4/XLAT,XLON
      common/SET1/IYEAR,IBDOY,IEDOY,IFDAY,IFTIME
      COMMON/C5/IXPOS,GREF,ITGREF,GREF12(12),SREF,NSNOW(12),SNFNAM
      CHARACTER*72 SNFNAM
      common/C6/INDCFG
      common/C21/IFCFG,cfgroot,LCFGF
      common/CFGV/icfgv
      common/PREC8/SLAT,SLON
      common/rpath/path
      common/rcmd/LCMDFL
      common/paths/zonepth,netpth,ctlpth,imgpth,radpth,docpth,tmppth,
     &             dbspth

C Plant network.
      COMMON/C23/IFPNF,LPNF

C Project log file.
      common/LOG/LPRJLG

C Images.
      common/IMAGF/imgfmt(10),imgfoc(10),limgfil(10),noimg,iton

C External text editor.
      common/texted/tedlbl,teditor

      common/FOPENED/CFGOK,MLDBOK,MATDBOK,CTLOK,OPTKOK

C Calendar.
      common/caleni/nbdaytype,nbcaldays(10),icalender(365)
      dimension nwlistf(50)

      logical XST,OK,dok,CFGOK,MLDBOK,MATDBOK,CTLOK,OPTKOK
      logical OKI,concat,unixok,clkok

      character dstmp*24,LCFTMP*24
      character*72 LSNAM,LPROJ,LGEOM,LSHAD,LTHRM,LUTIL,LTMP
      character path*72,LCMDFL*144,LPRJLG*72,LPNF*72,odir*72
      character LCFGF*72,H*72,cfgroot*24
      character*72 DFCFG
      character doit*248,tfile*72,longtfile*144,longtfiledos*144
      character tedlbl*20,teditor*20
      character uname*24,pwd*40,tmode*8
      character imgfmt*4,imgfoc*4,limgfil*72,limg*72,act*1
      character iformat*4,ifocus*4
      character zonepth*24,netpth*24,ctlpth*24,imgpth*24,radpth*24,fs*1
      character docpth*24,tmppth*24,dbspth*24
      character*72 listf(50),listfc(50)
      character subpath*72,action*3,outs*124

      integer iyeart    ! for local editing

C Set folder separator (fs) to \ or / as required.
      call isunix(unixok)
      if(unixok)then
        fs = char(47)
      else
        fs = char(92)
      endif

C If act = 'i' then initial pass, if 'e' then provide editing menu.

C Ask for source file, brief description and log file. Offer
C editing of log file, images, hypertext link and results summary.
      IER=0

  289 H(1)='The system configuration file holds the definition'
      H(2)='of the zone(s) and/ or plant and/ or control to be'
      H(3)='simulated, including references to associated files.'
      h(4)='The `root` name you supply will be used in a series of'
      h(5)='system level files. '
      h(6)=' '
      h(7)='If you selected this option in error click on continue.'
      LCFTMP='  '
      CALL EASKSCNCL(LCFTMP,
     &  ' System configuration name? (root name for model) ',
     &  ' ','cancel',clkok,24,'new_model','system root name',IER,7)
      call usrmsg(' ',' ','-')
      if(clkok)then
        return         ! User clicked on continue button.
      else

C Trap for blank root name.
        IF(LCFTMP(1:2).EQ.'  '.or.LCFTMP(1:4).eq.'UNKN')GOTO 289

C User did not cancel so instanciate data for new model.
        call clrprb
        icfgv=3
        INDCFG=0
        XLAT=50.0
        SLAT=50.0
        XLON=0.0
        SLON=0.0
        IXPOS=1
        GREF=0.2
        NCOMP=0
        noimg=0
        LPNF='UNKNOWN'
        LPRJLG='UNKNOWN'
        nbdaytype=0
        call st2file(LCFTMP,LCMDFL)
      endif
C      CALL EASKS(LCFTMP,
C     &  ' System configuration name? (root name for model) ',
C     &  ' ',24,'new_model','system root name',IER,5)

C Derive the model root name from this.
C If sting > 4 char it might have a .cfg attached, otherwise add.
      call fdroot(LCMDFL,path,LCFGF)
      lcfgr=lnblnk(LCFGF)
      lcfgl=lcfgr-3
      if(lcfgr.gt.4)then
        if(LCFGF(lcfgl:lcfgr).eq.'.cfg')then
          write(cfgroot,'(a)') LCFGF(1:lcfgl-1)
        else
          write(cfgroot,'(a)') LCFGF(1:lcfgr)
          write(LCFGF,'(a,a)')LCFGF(1:lcfgr),'.cfg'
        endif
      else
        write(cfgroot,'(a)') LCFGF(1:lcfgr)
        write(LCFGF,'(a,a)')LCFGF(1:lcfgr),'.cfg'
      endif

C Check existance of this file (should not overwrite an existing file).
      CALL ERPFREE(IFCFG,ISTAT)
      call FINDFIL(LCFGF,XST)
      if(XST)then
        dok=.true.
        h(1)='If you overwrite the existing configuration file you'
        h(2)='will `loose-the-past` unless you have backed up your'
        h(3)='model.  '
        h(4)='If you want to create a new model variant say no and '
        h(5)='provide an different file name. '
        call askok('There is an existing configuration with',
     &  'this name. Do you want to overwrite it? ',OK,dok,5)
        if(.not.OK)goto 289
      endif

C Ask where to put it.
C Get the current folder and display options to the user.
      odir=' '
      call usrdir(odir)
      H(1)='Esp-r holds the model in a standard set of folders '
      H(2)='and descriptive files. '
      H(3)=' '
      H(4)='Based on information you have supplied: '
      H(5)=' '
      write(H(6),'(3x,a,a)')cfgroot(1:lnblnk(cfgroot)),
     &  ' (project folder)'
      write(H(7),'(3x,a,a)')cfgroot(1:lnblnk(cfgroot)),
     &  '/cfg (system files)'
      write(H(8),'(3x,a,a)')cfgroot(1:lnblnk(cfgroot)),
     &  '/ctl (control files)'
      write(H(9),'(3x,a,a)')cfgroot(1:lnblnk(cfgroot)),
     &  '/zones (zone files)'
      write(H(10),'(3x,a,a)')cfgroot(1:lnblnk(cfgroot)),
     &  '/nets (networks)'
      write(H(11),'(3x,a,a)')cfgroot(1:lnblnk(cfgroot)),
     &  '/doc (reports and notes)'
      write(H(12),'(3x,a,a)')cfgroot(1:lnblnk(cfgroot)),
     &  '/temp (odds+ends)'
      write(H(13),'(3x,a,a)')cfgroot(1:lnblnk(cfgroot)),
     &  '/dbs (project databases)'
      H(14)=' '
      H(15)='will be created. '
      dok=.true.
      CALL ASKOK('Proceed with setting up your model?',
     &  '(see help for details of folders)',ok,dok,15)
      if(ok)then
        write(zonepth,'(3a)')'..',fs,'zones'
        write(netpth,'(3a)')'..',fs,'nets'
        write(ctlpth,'(3a)')'..',fs,'ctl'
        write(imgpth,'(3a)')'..',fs,'images'
        write(radpth,'(3a)')'..',fs,'rad'
        write(docpth,'(3a)')'..',fs,'doc'
        write(tmppth,'(3a)')'..',fs,'temp'
        write(dbspth,'(3a)')'..',fs,'dbs'
        write(odir,'(a)')cfgroot(1:lnblnk(cfgroot))
        write(doit,'(2a)') 'mkdir ',cfgroot(1:lnblnk(cfgroot))
        call usrmsg('creating folder:',doit,'-')
        call runit(doit,'-')
        call pausems(400)
        write(doit,'(4a)') 'mkdir ',cfgroot(1:lnblnk(cfgroot)),fs,'cfg'
        call usrmsg('creating folder:',doit,'-')
        call runit(doit,'-')
        call pausems(400)
        write(doit,'(4a)') 'mkdir ',cfgroot(1:lnblnk(cfgroot)),fs,'ctl'
        call usrmsg('creating folder:',doit,'-')
        call runit(doit,'-')
        call pausems(400)
        write(doit,'(4a)') 'mkdir ',cfgroot(1:lnblnk(cfgroot)),fs,
     &      'zones'
        call usrmsg('creating folder:',doit,'-')
        call runit(doit,'-')
        call pausems(400)
        write(doit,'(4a)') 'mkdir ',cfgroot(1:lnblnk(cfgroot)),fs,
     &      'nets'
        call usrmsg('creating folder:',doit,'-')
        call runit(doit,'-')
        call pausems(400)
        write(doit,'(4a)') 'mkdir ',cfgroot(1:lnblnk(cfgroot)),fs,
     &      'images'
        call usrmsg('creating folder:',doit,'-')
        call runit(doit,'-')
        call pausems(400)
        write(doit,'(4a)') 'mkdir ',cfgroot(1:lnblnk(cfgroot)),fs,'doc'
        call usrmsg('creating folder:',doit,'-')
        call runit(doit,'-')
        call pausems(400)
        write(doit,'(4a)') 'mkdir ',cfgroot(1:lnblnk(cfgroot)),fs,
     &      'temp'
        call usrmsg('creating folder:',doit,'-')
        call runit(doit,'-')
        call pausems(400)
        write(doit,'(4a)') 'mkdir ',cfgroot(1:lnblnk(cfgroot)),fs,'rad'
        call usrmsg('creating folder:',doit,'-')
        call runit(doit,'-')
        call pausems(400)
        write(doit,'(4a)') 'mkdir ',cfgroot(1:lnblnk(cfgroot)),fs,'dbs'
        call usrmsg('creating folder:',doit,'-')
        call runit(doit,'-')
        call pausems(400)
        call usrmsg('  ','  ','-')

C Re-establish pwd and then project folders.
        odir=' '
        call usrdir(odir)
        write(path,'(6a)')odir(1:lnblnk(odir)),fs,
     &      cfgroot(1:lnblnk(cfgroot)),fs,'cfg',fs
        call edisp(iuout,'The configuration will be in: ')
        call edisp(iuout,path)
        write(path,'(4a)')cfgroot(1:lnblnk(cfgroot)),fs,'cfg',fs
        call edisp(iuout,'(resetting path to: ')
        call edisp(iuout,path)
      else
        return
      endif

C General description of the model.
  52  H(1)='The model has an associated 72 character descriptor'
      H(2)='which is used later as an identifier, e.g. Low energy'
      H(3)='school with optimum start/ stop control heating system.'
      LSNAM='  '
      ltmp=LSNAM
      CALL EASKS(ltmp,
     &  ' Please supply a brief description of the model:',
     &  ' ',72,'base case run','model description',IER,3)
      if(ltmp(1:2).ne.'  '.or.lnblnk(ltmp).gt.3)then
        LSNAM=ltmp
      else
        call usrmsg('Sorry, a blank is not a description.',
     &    'Documenting your work is a good habit ;-)','W')
        goto 52
      endif

C Documentation.
  53  H(1)='The project log file contains descriptive text about'
      H(2)='the project.  Keep it up to date via a text editor.'
      if(LPRJLG(1:2).eq.'  '.or.LPRJLG(1:4).eq.'UNKN')then
        write(LPRJLG,'(a,a)') cfgroot(1:lnblnk(cfgroot)),'.log'
      endif
      ltmp=LPRJLG
      CALL EASKS(ltmp,
     &  'A project log file (notes/documentation) for this project,',
     &  'Its name? ',72,'project.notes','log file',IER,2)
      if(ltmp(1:2).ne.'  '.and.ltmp(1:4).ne.'UNKN')then
        LPRJLG=ltmp
      else
        goto 53
      endif

C See if file exists, otherwise create it and write basic
C information.
      uname=' '
      call usrname(uname)
      call usrdir(pwd)
      call FINDFIL(LPRJLG,XST)
      IUNIT=IFIL+1
      if(XST)then
        CALL LISTAS(IUNIT,LPRJLG,IER)
        IF(IER.ne.0)THEN
          dok=.true.
          h(1)='The log file exists, but could not be scanned.'
          h(2)='Check that it does not contain non-printable '
          h(3)='characters or is a blank file. '
          CALL ASKOK(' Problem detected while trying to open',
     &                ' log file. Try again?',OK,dok,3)
          IF(OK)GOTO 53
        ENDIF
      else
        write(currentfile,'(a)') LPRJLG(1:lnblnk(LPRJLG))
        CALL EFOPSEQ(IUNIT,LPRJLG,3,IER)
        write(iunit,'(a,a)')'Project notes for ',LCFGF(1:lnblnk(LCFGF))
        write(iunit,'(a,a)')'Description: ',LSNAM(1:lnblnk(LSNAM))
        write(iunit,'(a,a)')'In folder: ',pwd(1:lnblnk(pwd))
        write(iunit,'(a,a)')'By: ',uname(1:lnblnk(uname))
        call dstamp(dstmp)
        write(iunit,'(a,a)')'Date: ',dstmp
        write(iunit,'(a)')'For: '
        write(iunit,'(a)')'Project reference: '
        write(iunit,'(a)')'Notes: '
        write(iunit,'(a)')'_____________________________'
        write(iunit,'(a)')' '
        write(iunit,'(a)')' '
        CALL ERPFREE(IUNIT,ISTAT)
      endif

C Allow user to edit the job notes file. Append to path if
C necessary.  If vi then spawn a new window to do the editing.

C << this would be a good place to offer an internal editing popup
C << which uses GTK facilities.
      dok=.false.
      h(1)='Assuming you have an editor associated with esp-r '
      h(2)='you can edit the log file. '
      h(3)=' '
      h(4)='The log file can be used for different purposes. One'
      h(5)='use is to record the sequence of tasks. Another use '
      h(6)='is to provide documentation on the model. '
      CALL ASKOK(' ',' Edit the log file?',OK,dok,6)
      IF(OK)then

C Check if Unix-based or DOS based.
        call isunix(unixok)
        if(unixok)then
          call addpath(LPRJLG,longtfile,concat)
        else

C If running on a non-unix machine see if there are spaces in the name
C and change any / to \.
          call addpath(LPRJLG,longtfile,concat)
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
      h(1)='A model can include references to images and the'
      h(2)='images can be associated with some aspect of simulation'
      h(3)='such as geometry, networks or performance. '
      h(4)=' '
      h(5)='Images associated with a model are usually held in the '
      h(6)='`images` folder. '
      h(7)=' '
      h(8)='If you have images please either place them in the'
      h(9)='`images` folder of the model (a good idea because they'
      h(10)='then become part of the model) or provide an absolute'
      h(11)='path to the image file.'
      h(12)=' '
      h(13)='You can either tell ESP-r about them now or add them'
      h(14)='to the model later on via the ~model context->images~'
      h(15)='menu item.'
      CALL ASKOK('There may be images which are related to this ',
     &  'project. Associate images now ?',OKI,dok,15)
      if(OKI)then

C Provide a listing of the current contents of the ../images folder.
        subpath=' '
        write(subpath,'(4a)') odir(1:lnblnk(odir)),fs,
     &  path(1:lnblnk(path)),imgpth(1:lnblnk(imgpth))
        call clearfilelist(listf,50)
        action='fil'
        call getfilelist(subpath,action,listf,nwlistf,nlistf)
        call copyfilelist(listf,nwlistf,listfc,50,maxw)
        call printfilelist(outs,'p',listfc,50,nlistf)

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
        endif
        if(noimg.lt.10)then
          dok=.false.
          h(1)='If you have more images say ok. '
          CALL ASKOK(' ',' Another image?',OK,dok,1)
          if(OK)then
            write(limg,'(a)')imgpth(1:lnblnk(imgpth))
            iformat='GIF '
            ifocus='****'
            goto 142
          endif
        endif
      endif
      if(act.eq.'i')then

C Ask for site position.
        H(1)='Site latitude is degrees, north +ve, e.g. 55.9 for'
        H(2)='Glasgow.'
        CALL EASKR(SLAT,' ',' Site latitude?',
     &       -89.9,'W',89.9,'W',55.9,'site latitude',IER,2)
        IF(IER.EQ.0)XLAT=SLAT

        H(1)='Site longitude difference is degrees from some local'
        H(2)='reference meridian, east +ve, e.g. -4.1 for Glasgow'
        H(3)='relative to the Greenwich meridian.'
        CALL EASKR(SLON,' ',' Longitude difference from reference?',
     &       -15.0,'W',15.0,'W',0.0,'site londitude',IER,3)
        IF(IER.EQ.0)XLON=SLON

C Ask for year, initialise and then print out the calendar if the
C user does not cancel the operation.
        H(1)='The project controls and scheduling uses a year'
        H(2)='index. You can use the one specified with the climate'
        H(3)='file or give the current year.'
        H(4)=' '
        H(5)='A camcel request allows you to proceed without changing'
        H(6)='the schedules year (it does not yet cancel the creation'
        H(7)='of a new model.'
        iyeart=iyear
        CALL EASKI(IYEART,' ',' Year for schedules and assessments?',
     &       1900,'W',2010,'W',2000,'year',IERI,7)
        if(ieri.eq.-3) then
          call calenmanage('i',ier)
        else
          iyear=iyeart
          call calenmanage('i',ier)
          loop=8
          loopst=1
          call calenprint(iuout,'t',iyear,loopst,loop,ier)
          call edisp(iuout,' ')
          call calenprint(iuout,'g',iyear,loopst,loop,ier)
          call edisp(iuout,' ')
        endif

        H(1)='Other site related information such as ground '
        H(2)='reflection and wind exposure can be found in the '
        H(3)='`model context` selection. Please CONFIRM that '
        H(4)='the site information is correct for your model. '
        CALL PHELPD('new site',4,'-',0,0,IER)

C Create a system configuration file based on the registration
C information.
        call tstamp('>','PRJ: create configuration')
        call tstamp('>',LCFGF)
        CALL EMKCFG('-',IER)
        if(ier.eq.0)CFGOK=.TRUE.
        return
      endif
      return
      end

C ********** pfolders
C Maintain model folders.
      subroutine pfolders(act,ier)
#include "espriou.h"

      common/FILEP/IFIL
      common/OUTIN/IUOUT,IUIN
      common/pophelp/h(60)
      common/C21/IFCFG,cfgroot,LCFGF
      common/CFGV/icfgv
      common/rpath/path
      common/paths/zonepth,netpth,ctlpth,imgpth,radpth,docpth,tmppth,
     &             dbspth

C External text editor.
      common/texted/tedlbl,teditor

      common/FOPENED/CFGOK,MLDBOK,MATDBOK,CTLOK,OPTKOK
      common/user/browse

C Remember width of file/folder names passed back from getfilelist.
      dimension nwlistf(50)
      logical OK,DOK,CFGOK,MLDBOK,MATDBOK,CTLOK,OPTKOK
      logical concat,unixok,changedit,browse

      character*72 path,LFIL,odir
      character LCFGF*72,H*72,cfgroot*24
      character doit*248,tfile*72,sfile*72,snpfile*72
      character longtfile*144,longtfiledos*144
      character tedlbl*20,teditor*20
      character tmode*8,t24*24,act*1,outstr*124,outs*124
      character*33 ITEMS(19)
      character*72 listf(50),listfc(50)
      character zonepth*24,netpth*24,ctlpth*24,imgpth*24,radpth*24,fs*1
      character docpth*24,tmppth*24,dbspth*24
      character subpath*72,action*3

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

C Debug...
C      write(6,*) 'usrdir is ',odir
C      write(6,*) 'path is ',path
      subpath=' '
      write(subpath,'(3a)') odir(1:lnblnk(odir)),fs,
     &  path(1:lnblnk(path))
      write(outs,'(2a)') 'configuration files in ',
     &  subpath(1:lnblnk(subpath))

#ifdef OSX
C In OSX slightly different logic. If path ls ./ then standard logic
C works. If path begins with /usr/esru then no need to prepend odir.
      if(path(1:2).eq.'./')then
        continue
      elseif(path(1:9).eq.'/usr/esru')then
        write(6,*) 'OSX path is ',path
        subpath=' '
        write(subpath,'(a)') path(1:lnblnk(path))
        write(outs,'(2a)') 'configuration files in ',
     &    subpath(1:lnblnk(subpath))
        call edisp(iuout,outs)
      else
        continue
      endif
#endif
      call clearfilelist(listf,50)
      action='cfg'
      call getfilelist(subpath,action,listf,nwlistf,nlistf)
      call copyfilelist(listf,nwlistf,listfc,50,maxw)
      call printfilelist(outs,'p',listfc,50,nlistf)

  42  continue
      INO=-4
      WRITE(ITEMS(1),'(2A)') '  name : ',LCFGF(1:24)
      WRITE(ITEMS(2),'(2A)') '  path : ',path(1:23)
      WRITE(ITEMS(3),'(A)')  '  ___relative paths to_______ '
      WRITE(ITEMS(4),'(2A)') 'b  zones   : ',zonepth(1:20)
      WRITE(ITEMS(5),'(2A)') 'c  control : ',ctlpth(1:20)
      WRITE(ITEMS(6),'(2A)') 'd  network : ',netpth(1:20)
      WRITE(ITEMS(7),'(2A)') 'e  images  : ',imgpth(1:20)
      WRITE(ITEMS(8),'(2A)') 'f  document: ',docpth(1:20)
      WRITE(ITEMS(9),'(2A)') 'g  temp    : ',tmppth(1:20)
      WRITE(ITEMS(10),'(2A)')'h  radiance: ',radpth(1:20)
      WRITE(ITEMS(11),'(2A)')'i  local db: ',dbspth(1:20)
      ITEMS(12)  =           ' _____________________________ '
      ITEMS(13)  =           'j  zone files                  '
      ITEMS(14)  =           ' _____________________________ '
      WRITE(ITEMS(15),'(A)') 'k look at an ASCII file        '
      WRITE(ITEMS(16),'(A)') 'l edit an ASCII file           '
      WRITE(ITEMS(17),'(A)') 'm strip model file annotations '
      ITEMS(18)  =           '? help                         '
      ITEMS(19)  =           '- exit this menu               '
      nitems=19

C Help text for this menu.
      H(1)='This menu allows you to edit the information related'
      H(2)='to a project`s folders and files.'
      H(3)=' '
      H(4)='The path options allow new files to be positioned in '
      H(5)='standard folders.  The paths are relative to the '
      H(6)='system configuration file. '
      H(7)=' '
      H(8)='One option lists a specified ASCII file - sometimes'
      H(9)='useful if you have need to directly view the contents'
      H(10)='of a problem describing entity such as the system'
      H(11)='configuration file or a zone geometry file.  There'
      H(12)='is also an option to strip comments from files where'
      H(13)='that will make them compatible with older versions'
      H(14)='of the Project Manager.  '
      H(15)=' '
      H(16)='Editing an ASCII file - such as the job notes'
      H(17)='file - requires a text editor which is currently'
      H(18)='defined to be:'
      write(H(19),'(3a)')'      ',teditor(1:lnblnk(teditor)),'.'
      H(20)='This can be re-defined in your .esprc file.'

      CALL EMENU('Project folders',ITEMS,nitems,INO)
      IF(INO.EQ.nitems)THEN
        if(browse)then
          continue
        else
          if(changedit)then
            dok=.true.
            h(1)='ESP-r thinks that recent changes have been made'
            h(2)='with are in memory, but which might be lost if '
            h(3)='they are not saved to file. '
            h(4)='If in doubt, agree to saving the file. '
            CALL ASKOK(' ','Update configuration file?',OK,dok,4)
            if(OK)CALL EMKCFG('-',IER)
            if(ier.eq.0)CFGOK=.TRUE.
            changedit=.false.
          endif
        endif
        return
      elseif(ino.eq.nitems-1)then
        CALL PHELPD('folders and files',20,'-',0,0,IER)
      elseif(ino.eq.4)then
        if(.NOT.browse)then
          H(1)='Give relative path from cfg to zones folder.'
          t24=zonepth
          CALL EASKS(t24,'Relative path (from cfg) to zones?',
     &      ' ',24,'../zones','relative zones path',IER,1)
          if(t24(1:2).ne.'  '.and.t24(1:4).ne.'UNKN')then
            zonepth=t24
            changedit=.true.
            sfile=' '
            snpfile=' '
            call edisp(iuout,' ')
            call browsefilelist('p','zon','geo',sfile,snpfile,nli,iier)
            sfile=' '
            snpfile=' '
            call edisp(iuout,' ')
            call browsefilelist('p','zon','opr',sfile,snpfile,nli,iier)
            sfile=' '
            snpfile=' '
            call browsefilelist('b','zon','geo',sfile,snpfile,nli,iier)
C Debug...
C            write(6,*) 'selected file is ',sfile
C            write(6,*) 'selected file is ',snpfile
C            write(6,*) 'iier is ',iier,nli
          endif
        else
          sfile=' '
          snpfile=' '
          call edisp(iuout,' ')
          call browsefilelist('p','zon','geo',sfile,snpfile,nli,iier)
        endif
      elseif(ino.eq.5)then
        if(.NOT.browse)then
          H(1)='Give relative path from cfg to controls folder.'
          t24=ctlpth
          CALL EASKS(t24,'Relative path (from cfg) to controls?',
     &      ' ',24,'../ctl','relative controls path',IER,1)
          if(t24(1:2).ne.'  '.and.t24(1:4).ne.'UNKN')then
            ctlpth=t24
            changedit=.true.
            sfile=' '
            snpfile=' '
            call edisp(iuout,' ')
            call browsefilelist('p','ctl','fil',sfile,snpfile,nil,iier)
          endif
        else
          sfile=' '
          snpfile=' '
          call edisp(iuout,' ')
          call browsefilelist('p','ctl','fil',sfile,snpfile,nli,iier)
        endif
      elseif(ino.eq.6)then
        if(.NOT.browse)then
          H(1)='Give relative path from cfg to networks folder.'
          t24=netpth
          CALL EASKS(t24,'Relative path (from cfg) to networks?',
     &      ' ',24,'../nets','relative networks path',IER,1)
          if(t24(1:2).ne.'  '.and.t24(1:4).ne.'UNKN')then
            netpth=t24
            changedit=.true.
            sfile=' '
            snpfile=' '
            call edisp(iuout,' ')
            call browsefilelist('p','net','afn',sfile,snpfile,nli,iier)
            call edisp(iuout,' ')
            call browsefilelist('p','net','gnf',sfile,snpfile,nli,iier)
            endif
        else
          sfile=' '
          snpfile=' '
          call edisp(iuout,' ')
          call browsefilelist('p','net','afn',sfile,snpfile,nli,iier)
          call edisp(iuout,' ')
          call browsefilelist('p','net','gnf',sfile,snpfile,nli,iier)
        endif
      elseif(ino.eq.7)then
        if(.NOT.browse)then
          H(1)='Give relative path from cfg to images folder.'
          t24=imgpth
          CALL EASKS(t24,'Relative path (from cfg) to images?',
     &      ' ',24,'../images','relative img path',IER,1)
          if(t24(1:2).ne.'  '.and.t24(1:4).ne.'UNKN')then
            imgpth=t24
            changedit=.true.
            sfile=' '
            snpfile=' '
            call edisp(iuout,' ')
            call browsefilelist('p','img','fil',sfile,snpfile,nli,iier)
          endif
        else
          sfile=' '
          snpfile=' '
          call edisp(iuout,' ')
          call browsefilelist('p','img','fil',sfile,snpfile,nli,iier)
        endif
      elseif(ino.eq.8)then
        if(.NOT.browse)then
          H(1)='Give relative path from cfg to documents folder.'
          t24=docpth
          CALL EASKS(t24,'Relative path (from cfg) to documents?',
     &      ' ',24,'../doc','relative doc path',IER,1)
          if(t24(1:2).ne.'  '.and.t24(1:4).ne.'UNKN')then
            docpth=t24
            changedit=.true.
            sfile=' '
            snpfile=' '
            call edisp(iuout,' ')
            call browsefilelist('p','doc','fil',sfile,snpfile,nli,iier)
          endif
        else
          sfile=' '
          snpfile=' '
          call edisp(iuout,' ')
          call browsefilelist('p','doc','fil',sfile,snpfile,nli,iier)
        endif
      elseif(ino.eq.9)then
        if(.NOT.browse)then
          H(1)='Give relative path from cfg to temporary folder.'
          H(2)='Holds transient information.'
          t24=tmppth
          CALL EASKS(t24,'Relative path (from cfg) to temp folder?',
     &      ' ',24,'../doc','relative temp path',IER,2)
          if(t24(1:2).ne.'  '.and.t24(1:4).ne.'UNKN')then
            tmppth=t24
            changedit=.true.
            sfile=' '
            snpfile=' '
            call edisp(iuout,' ')
            call browsefilelist('p','tmp','fil',sfile,snpfile,nli,iier)
          endif
        else
          sfile=' '
          snpfile=' '
          call edisp(iuout,' ')
          call browsefilelist('p','tmp','fil',sfile,snpfile,nli,iier)
        endif
      elseif(ino.eq.10)then
        if(.NOT.browse)then
          H(1)='Give relative path from cfg to radiance folder.'
          h(2)='(where sky, materials, surfaces held) '
          t24=radpth
          CALL EASKS(t24,
     &      'Relative path (from cfg) to radiance descriptive files?',
     &      ' ',24,'../rad','relative radiance path',IER,2)
          if(t24(1:2).ne.'  '.and.t24(1:4).ne.'UNKN')then
            radpth=t24
            changedit=.true.
            sfile=' '
            snpfile=' '
            call edisp(iuout,' ')
            call browsefilelist('p','rad','fil',sfile,snpfile,nli,iier)
          endif
        else
          sfile=' '
          snpfile=' '
          call edisp(iuout,' ')
          call browsefilelist('p','rad','fil',sfile,snpfile,nli,iier)
        endif
      elseif(ino.eq.11)then
        subpath=' '
        write(subpath,'(4a)') odir(1:lnblnk(odir)),fs,
     &    path(1:lnblnk(path)),dbspth(1:lnblnk(dbspth))
        write(outs,*) 'files in ',subpath
        call clearfilelist(listf,50)
        action='fil'
        call getfilelist(subpath,action,listf,nwlistf,nlistf)
        call copyfilelist(listf,nwlistf,listfc,50,maxw)
        call printfilelist(outs,'p',listfc,50,nlistf)

C << interface for dbspath here >>

      elseif(ino.eq.13)then

C Zone file composition.
        IC=-1
 254    CALL EASKGEOF('Select a zone to focus on:',CFGOK,IC,'-',IER)
        IF(IC.EQ.0.OR.IC.EQ.-1)GOTO 42
        CALL EDBUILD(IC,IER)

C Loop back and see if another zone is requested.
        IC=-1
        GOTO 254
      elseif(ino.eq.15)then

C List out an ASCII file.
        H(1)='The specified ASCII file will be read and displayed'
        H(2)='in the text window page by page.'
    6   CALL EASKS(LFIL,' ASCII file name?',
     &    ' ',72,' ','ASCII file',IER,2)

        IUNIT=IFIL+1
        CALL LISTAS(IUNIT,LFIL,IER)
        IF(IER.LT.0)THEN
          dok=.false.
          h(1)='Chance to correct error in file name or location. '
          CALL ASKOK(' Problem detected while trying to open',
     &                ' ASCII file.  Try again?',OK,dok,1)
          IF(OK)GOTO 6
        ENDIF
      elseif(ino.eq.16)then

C Allow user to edit a file. Append to path if necessary.
C If vi then spawn a new window to do the editing in.
        H(1)='An ASCII format file can be edited using the current'
        H(2)='defined text editor.'
        CALL EASKS(LFIL,' ASCII file to edit ? ',
     &    ' ',72,' ','ASCII file',IER,2)
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
      elseif(ino.eq.17)then

C Ask for commented file name and open it, IER=-301 if not found.
        H(1)='An annotated file may be stripped of all comments'
        H(2)='so that it is compatible with older applications.'
    8   CALL EASKS(LFIL,' Commented file name?',
     &    ' ',72,' ','commented file name',IER,2)

        IUNIT=IFIL+1
        CALL EFOPSEQ(IUNIT,LFIL,1,IER)
        IF(IER.LT.0)GOTO 8

C Ask for name of file to put uncommented text into.
        H(1)='An annotated file may be stripped of all comments'
        H(2)='so that it is compatible with older applications.'
    9   CALL EASKS(SFILE,' Uncommented file name?',
     &    ' ',72,' ','Uncommented file name',IER,2)

c Place output into IFIL+2, first close that unit, open any exiting
C file with this name, delete it and create a new file.
        ISUNIT=IFIL+2
        CALL EFOPSEQ(ISUNIT,SFILE,4,IER)
        IF(IER.LT.0)GOTO 9

C Loop until end of file encountered and display stripped text.
   73   CALL STRIPC(IUNIT,OUTSTR,0,ND,0,'stripping',IER)
        IF(IER.EQ.2)goto 72
        IF(IER.EQ.1)GOTO 42
        WRITE(ISUNIT,'(a)')OUTSTR(1:LNBLNK(OUTSTR))
        CALL edisp(iuout,OUTSTR)
        goto 73
   72   CALL USRMSG(' ',' reached end of source file ','-')
        CALL ERPFREE(ISUNIT,ISTAT)
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

      COMMON/FILEP/IFIL
      common/OUTIN/IUOUT,IUIN
      common/pophelp/h(60)
      COMMON/SPAD/MMOD,LIMIT,LIMTTY

      logical XST,OK,dok
      character iformat*4,ifocus*4,limg*72,t5*5
      character H*72,outs*124

C Display current information.
      ier=0
      call edisp(iuout,'The current image is:')
      call edisp(iuout,'Format  Focus File')
      write(outs,'(6a)') ' ',iformat,' ',ifocus,' ',limg(1:lnblnk(limg))
      call edisp(iuout,outs)

 242  H(1)='Images will be displayed when the project is '
      H(2)='loaded.  The default image type is GIF. '
      CALL EASKS(limg,' Name of image file? ',
     &  ' ',72,'first.gif','image file',IER,2)
      if(limg(1:2).eq.'  '.or.limg(1:4).eq.'UNKN')goto 242
      XST=.false.
      call findfil(limg,XST)
      if(.NOT.XST)then
        dok=.false.
        h(1)='Chance to change file name or location. '
        CALL ASKOK('Could not locate this image.','Try again?',OK,dok,1)
        if(OK)then
          goto 242
        else
          ier=2
          return
        endif
      endif

      H(1)='Select one of these standard image types. Such types '
      H(2)='should have a related display application specified '
      H(3)='in the .esprc file ie. *image_display  TIF     xv.'
      call EASKATOG(' Format of image:',' ',
     &  'GIF','TIFF','XBM','XWD','other',' ',' ',IRT,3)
      if(IRT.eq.1)then
        write(iformat,'(a4)') 'GIF '
      elseif(IRT.eq.2)then
        write(iformat,'(a4)') 'TIF '
      elseif(IRT.eq.3)then
        write(iformat,'(a4)') 'XBMP'
      elseif(IRT.eq.4)then
        write(iformat,'(a4)') 'XWD '
      elseif(IRT.eq.5)then
 145    H(1)='Image files can have a 4 character format. Special'
        H(2)='format tags can be linked to display applications '
        H(3)='in the .esprc file. ie. XWD xwud -in '
        t5='**** '
        CALL EASKS(t5,' Character code (see help) for image type?',
     &    ' ',5,'****','usr image type',IER,3)
        if(t5(1:1).eq.' ')goto 145
        iformat=t5(1:4)
      endif

      h(1)='If an image is general it will be displayed when the '
      h(2)='model is loaded, otherwise it will be displayed only'
      h(3)='in a particular context - say with plant facilities. '
      call EASKATOG(' Focus of image:',' ',
     &  'general','site/zone/gridding','networks','control',
     &  'CFD','performance','other',IRF,3)
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
 144    H(1)='Image files can have a 4 character focus. Special'
        H(2)='focus tags are: FGRD (3D gridding), F??? (). '
        t5='**** '
        CALL EASKS(t5,' Character code (see help) for image focus?',
     &    ' ',5,'**** ','usr image focus',IER,2)
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

      COMMON/SPAD/MMOD,LIMIT,LIMTTY
      common/OUTIN/IUOUT,IUIN
      common/FILEP/IFIL
      common/pophelp/h(60)
      common/rpath/path
      common/paths/zonepth,netpth,ctlpth,imgpth,radpth,docpth,tmppth,
     &             dbspth
      common/C2/LSNAM,NCCODE(MCOM),LPROJ(MCOM),LGEOM(MCOM),
     &          LSHAD(MCOM),LTHRM(MCOM),INDUTL(MCOM),LUTIL(MCOM)
      common/CFGV/icfgv
      COMMON/precz/zname(MCOM),zdesc(MCOM)
      common/user/browse

      common/INDICS/IVF(MCOM),ISI(MCOM),IHC(MCOM),
     &              ITW(MCOM),ICGC(MCOM),IOBS(MCOM)
      common/UDESC/LVIEW(MCOM),LHCCO(MCOM),
     &             LTWIN(MCOM),LCGCIN(MCOM),ZOBS(MCOM)

C CFD
      common/cfdfil/LCFD(MCOM),IFCFD(MCOM)
      common/GR3D100/BLDG3D,ZONE3D(MCOM)
      common/GR3D108/L3DCVS(MCOM),L3DCNC(MCOM),L3DNDC(MCOM),L3DTAQ(MCOM)
      common/MOIST01/MSTROK,MSTRZN(MCOM)
      common/MOIST02/LMOIST(MCOM)
      COMMON/GRSD100/IndxSt
      COMMON/GRSD101/LGrdSt

      logical browse,BLDG3D,ZONE3D,MSTROK,MSTRZN
      character*72 LVIEW,LHCCO,LTWIN,LCGCIN,ZOBS
      character*72 LSNAM,LPROJ,LGEOM,LSHAD,LTHRM,LUTIL
      character*72 L3DCVS,L3DCNC,L3DNDC,L3DTAQ,LMOIST,LGrdSt
      dimension ITEMS(25)
      character ITEMS*72,zname*12,zdesc*64,head*25,path*72,L*72,ZN*12
      character LCFD*72,H*72,outs*124
      character D*72,T15*12,D15*12
      character zonepth*24,netpth*24,ctlpth*24,imgpth*24,radpth*24
      character docpth*24,tmppth*24,dbspth*24
      character sfile*72,snpfile*72,fs*1
      logical modcfg,XST,unixok

C Note L is a temporary file and D is used for defaul file names.
      modcfg=.FALSE.

C Set folder separator (fs) to \ or / as required.
      call isunix(unixok)
      if(unixok)then
        fs = char(47)
      else
        fs = char(92)
      endif

      IER=0
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

C Find maximum with of text and title. IWA is for required files,
C IWC is for optional files, IW is the overall width.
      IWA=MAX0(36,la+18,lb+18,lc+18,lt)
      if(IWA.le.72)then
        IWB=IWA-18
      else
        IWB=29
      endif
      IWC=MAX0(36,ld+22,le+22,lf+22,lg+22,lh+22,li+22,ll+22,lm+22)
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
      ITEMS(20)='  _____________________________    '
      ITEMS(21)='* check existence of zone files    '
      ITEMS(22)='? help                             '
      ITEMS(23)='- exit this menu                   '
      MITEM=23

C Help text for thie menu.
      H(1)='The files defining the composition of a zone are'
      H(2)='listed in this menu. In the case of a blank entry,'
      H(3)='the file is optional and is not currently defined.'
      H(4)=' '
      H(5)='If the menu says `not yet defined` then the file is'
      H(6)='required before simulations can proceed.  Such files'
      H(7)='will be created when the related facilities have been'
      H(8)='accessed (ie. geometry files are created as you define'
      H(9)='each zone).  '
      H(10)=' '
      H(11)='The default names of files is the zone name with'
      H(12)='an extension indicating the file type.'
      H(13)=' '
      H(14)='The primary use of this facility is to confirm the'
      H(15)='current state of the model and to assist in the'
      H(16)='sharing of files (i.e. when several zones share the'
      H(17)='same constructions).  The model definition will be'
      H(18)='updated after each change you make.'

      WRITE(head,'(A,A)') 'Files for ',ZN
  2   if(MMOD.EQ.8)then
        CALL VWMENU(head,ITEMS,MITEM,0,0,IW,irpx,irpy,INO)
      else
        CALL EMENU(head,ITEMS,MITEM,INO)
      endif
      IF(INO.EQ.MITEM)THEN
        RETURN
      ELSEIF(INO.EQ.MITEM-1)THEN

C Produce help text for the menu.
        CALL PHELPD('utility file contents',18,'-',0,0,IER)
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
      ELSEIF(INO.EQ.2)THEN

C Edit zone name - read and then write the geometry file.
        if(browse)then
          call usrmsg('Cannot update model while in browse ',
     &                'mode, you must `own` the model!','W')
          goto 3
        endif
        CALL EGOMIN(IUF,LGEOM(IZ),IZ,1,0,iuout,IER)
        H(1)='The zone description is used as an identifier during'
        H(2)='results analysis.  Names should be unique.'
        T15=zname(IZ)
        D15='new_zone'
        CALL EASKS(T15,' ',' Zone name?',12,D15,'zone name',IER,2)
        if(T15.NE.' ')then
          call st2name(T15,zname(IZ))
          CALL EMKGEO(IUF,LGEOM(IZ),IZ,iuout,3,IER)
          ZN=zname(IZ)
        endif
      ELSEIF(INO.EQ.5)THEN
        if(browse)then
          call usrmsg('Cannot update model while in browse ',
     &                'mode, you must `own` the model!','W')
          goto 3
        endif
        H(1)='The zone geometry file is mandatory and defines the'
        H(2)='closed body of a thermal zone.'
        if(zonepth(1:2).eq.'  '.or.zonepth(1:2).eq.'./')then
          WRITE(D,'(A,A4)')ZN(1:lnblnk(ZN)),'.geo'
        else
          WRITE(D,'(3A,A4)') zonepth(1:lnblnk(zonepth)),'/',
     &      ZN(1:lnblnk(ZN)),'.geo'
        endif
        L=LGEOM(IZ)
        write(outs,'(3a)')' Geometry file for ',ZN(1:lnblnk(ZN)),' ?'
   41   CALL EASKS(L,outs,' ',72,D,'Z geom',IER,2)
        IF(L.eq.' '.or.IER.NE.0)GOTO 41
        if(LGEOM(IZ).ne.L)modcfg=.true.
        LGEOM(IZ)=L
      ELSEIF(INO.EQ.6)THEN
        if(browse)then
          call usrmsg('Cannot update model while in browse ',
     &                'mode, you must `own` the model!','W')
          goto 3
        endif
        H(1)='The zone construction file is mandatory and defines'
        H(2)='the thermo-physical properties of the thermal zone.'
        if(zonepth(1:2).eq.'  '.or.zonepth(1:2).eq.'./')then
          WRITE(D,'(A,A4)')ZN(1:lnblnk(ZN)),'.con'
        else
          WRITE(D,'(3A,A4)') zonepth(1:lnblnk(zonepth)),'/',
     &      ZN(1:lnblnk(ZN)),'.con'
        endif
        CALL EASKABC(' For zone construction file',
     &      ' :','edit name','browse','continue',IW,2)
        if(IW.EQ.1)then
          L=LTHRM(IZ)
        elseif(IW.EQ.2)then
          sfile=' '
          snpfile=' '
          call edisp(iuout,' ')
          call browsefilelist('?','zon','con',sfile,snpfile,nfile,
     &      iier)
          if(nfile.gt.0)then
            sfile=' '
            snpfile=' '
            call browsefilelist('b','zon','con',sfile,snpfile,nfile,
     &        iier)
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
   42   CALL EASKS(L,outs,' ',72,D,'Z constr',IER,2)
        IF(L.eq.' '.or.IER.NE.0)GOTO 42
        if(LTHRM(IZ).ne.L)modcfg=.true.
        LTHRM(IZ)=L
      ELSEIF(INO.EQ.7)THEN
        if(browse)then
          call usrmsg('Cannot update model while in browse ',
     &                'mode, you must `own` the model!','W')
          goto 3
        endif
        H(1)='The zone operation file is mandatory and defines'
        H(2)='infiltration, ventilation and casual gains.'
        if(zonepth(1:2).eq.'  '.or.zonepth(1:2).eq.'./')then
          WRITE(D,'(A,A4)')ZN(1:lnblnk(ZN)),'.opr'
        else
          WRITE(D,'(3A,A4)') zonepth(1:lnblnk(zonepth)),'/',
     &      ZN(1:lnblnk(ZN)),'.opr'
        endif
        CALL EASKABC(' For zone operation file',
     &      ' :','edit name','browse','continue',IW,2)
        if(IW.EQ.1)then
          L=LPROJ(IZ)
        elseif(IW.EQ.2)then
          sfile=' '
          snpfile=' '
          call edisp(iuout,' ')
          call browsefilelist('?','zon','opr',sfile,snpfile,nfile,
     &      iier)
          if(nfile.gt.0)then
            sfile=' '
            snpfile=' '
            call browsefilelist('b','zon','opr',sfile,snpfile,nfile,
     &        iier)
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
   40   CALL EASKS(L,outs,' ',72,D,'Z operations',IER,2)
        IF(L.eq.' '.or.IER.NE.0)GOTO 40
        if(LPROJ(IZ).ne.L)modcfg=.true.
        LPROJ(IZ)=L
      ELSEIF(INO.EQ.10)THEN
        if(browse)then
          call usrmsg('Cannot update model while in browse ',
     &                'mode, you must `own` the model!','W')
          goto 3
        endif
        H(1)='The zone obstructions file contains the geometry of'
        H(2)='obstruction blocks for use in shading/ insolation'
        H(3)='calculations. You may browse for existing files.'
        if(IOBS(IZ).eq.1)then
          CALL EASKABCD(' For zone obstruction file',' :',
     &      'edit name','browse','dereference it','continue',IW,3)
          if(IW.EQ.1)then
            L=ZOBS(IZ)
          elseif(IW.EQ.2)then
            sfile=' '
            snpfile=' '
            call edisp(iuout,' ')
            call browsefilelist('?','zon','obs',sfile,snpfile,nfile,
     &        iier)
            if(nfile.gt.0)then
              sfile=' '
              snpfile=' '
              call browsefilelist('b','zon','obs',sfile,snpfile,nfile,
     &          iier)
              if(snpfile(1:2).ne.'  ')then
                write(L,'(3a)')zonepth(1:lnblnk(zonepth)),fs,
     &            snpfile(1:lnblnk(snpfile))
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
     &      ' :','edit name','browse','continue',IW,3)
          if(IW.EQ.1)then
            L=' '
          elseif(IW.EQ.2)then
            sfile=' '
            snpfile=' '
            call edisp(iuout,' ')
            call browsefilelist('?','zon','obs',sfile,snpfile,nfile,
     &        iier)
            if(nfile.gt.0)then
              sfile=' '
              snpfile=' '
              call browsefilelist('b','zon','obs',sfile,snpfile,nfile,
     &          iier)
              if(snpfile(1:2).ne.'  ')then
                write(L,'(3a)')zonepth(1:lnblnk(zonepth)),fs,
     &            snpfile(1:lnblnk(snpfile))
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
     &      ZN(1:lnblnk(ZN)),'.obs'
        endif
        write(outs,'(3a)')' Obstruction file for ',ZN(1:lnblnk(ZN)),' ?'
   47   CALL EASKS(L,outs,' ',72,D,'Z obstr',IER,3)
        IF(L(1:1).eq.' '.or.IER.NE.0)GOTO 47
        ZOBS(IZ)=L
        IOBS(IZ)=1
        modcfg=.TRUE.
      ELSEIF(INO.EQ.11)THEN
        if(browse)then
          call usrmsg('Cannot update model while in browse ',
     &                'mode, you must `own` the model!','W')
          goto 3
        endif
        H(1)='The zone optics (TMC) file contains the optical'
        H(2)='properties of transparent surfaces and offering'
        H(3)='a more exact treatment of, for example, windows.'
        if(ITW(IZ).eq.1)then
          CALL EASKABCD(' For the zone TMC file',' :',
     &      'change name','browse','dereference it','continue',IW,3)
          if(IW.EQ.1)then
            L=LTWIN(IZ)
          elseif(IW.EQ.2)then
            sfile=' '
            snpfile=' '
            call edisp(iuout,' ')
            call browsefilelist('?','zon','tmc',sfile,snpfile,nfile,
     &        iier)
            if(nfile.gt.0)then
              sfile=' '
              snpfile=' '
              call browsefilelist('b','zon','tmc',sfile,snpfile,nfile,
     &          iier)
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
     &      'edit name','browse','continue',IW,3)
          if(IW.EQ.1)then
            L=' '
          elseif(IW.EQ.2)then
            sfile=' '
            snpfile=' '
            call edisp(iuout,' ')
            call browsefilelist('?','zon','tmc',sfile,snpfile,nfile,
     &        iier)
            if(nfile.gt.0)then
              sfile=' '
              snpfile=' '
              call browsefilelist('b','zon','tmc',sfile,snpfile,nfile,
     &          iier)
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
   51   CALL EASKS(L,outs,' ',72,D,'tmc file',IER,6)
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
        H(1)='The zone view factor file supports a more exact'
        H(2)='treatment of longwave radiation exchange.'
        H(3)='You may also browse for existing files.'
        if(IVF(IZ).eq.1)then
          CALL EASKABCD(' For zone view factor file',' :',
     &      'change name','browse','dereference it','continue',IW,3)
          if(IW.EQ.1)then
            L=LVIEW(IZ)
          elseif(IW.EQ.2)then
            sfile=' '
            snpfile=' '
            call edisp(iuout,' ')
            call browsefilelist('?','zon','vwf',sfile,snpfile,nfile,
     &        iier)
            if(nfile.gt.0)then
              sfile=' '
              snpfile=' '
              call browsefilelist('b','zon','vwf',sfile,snpfile,nfile,
     &          iier)
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
     &      'edit name','browse','continue',IW,3)
          if(IW.EQ.1)then
            L=' '
          elseif(IW.EQ.2)then
            sfile=' '
            snpfile=' '
            call edisp(iuout,' ')
            call browsefilelist('?','zon','vwf',sfile,snpfile,nfile,
     &        iier)
            if(nfile.gt.0)then
              sfile=' '
              snpfile=' '
              call browsefilelist('b','zon','vwf',sfile,snpfile,nfile,
     &          iier)
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
   46   CALL EASKS(L,outs,' ',72,D,'view factor',IER,4)
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
        H(1)='The zone casual gain control file allows a casual'
        H(2)='gain type to be controlled on the basis of daylight'
        H(3)='levels as sensed by one or more sensors located within'
        H(4)='or outwith a zone and against a selected control law.'
        if(ICGC(IZ).eq.1)then
          CALL EASKABCD(' For zone casual gain control file',' :',
     &      'change name','browse','dereference it','continue',IW,4)
          if(IW.EQ.1)then
            L=LCGCIN(IZ)
          elseif(IW.EQ.2)then
            sfile=' '
            snpfile=' '
            call edisp(iuout,' ')
            call browsefilelist('?','zon','cgc',sfile,snpfile,nfile,
     &        iier)
            if(nfile.gt.0)then
              sfile=' '
              snpfile=' '
              call browsefilelist('b','zon','cgc',sfile,snpfile,nfile,
     &          iier)
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
     &      'edit name','browse','continue',IW,3)
          if(IW.EQ.1)then
            L=' '
          elseif(IW.EQ.2)then
            sfile=' '
            snpfile=' '
            call edisp(iuout,' ')
            call browsefilelist('?','zon','cgc',sfile,snpfile,nfile,
     &        iier)
            if(nfile.gt.0)then
              sfile=' '
              snpfile=' '
              call browsefilelist('b','zon','cgc',sfile,snpfile,nfile,
     &          iier)
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
   52   CALL EASKS(L,outs,' ',72,D,'casual gain control file',IER,6)
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
        H(1)='The shading/ insolation file contains the results of'
        H(2)='a separate shading and insolation analysis for use'
        H(3)='throughout a simulation to dictate solar injection'
        H(4)='locations.'
        if(ISI(IZ).eq.1)then
          CALL EASKABCD(' For zone shading/ insolation file',' :',
     &      'change name','browse','dereference it','continue',IW,4)
          if(IW.EQ.1)then
            L=LSHAD(IZ)
          elseif(IW.EQ.2)then
            sfile=' '
            snpfile=' '
            call edisp(iuout,' ')
            call browsefilelist('?','zon','shd',sfile,snpfile,nfile,
     &        iier)
            if(nfile.gt.0)then
              sfile=' '
              snpfile=' '
              call browsefilelist('b','zon','shd',sfile,snpfile,nfile,
     &          iier)
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
     &      'edit name','browse','continue',IW,3)
          if(IW.EQ.1)then
            L=' '
          elseif(IW.EQ.2)then
            sfile=' '
            snpfile=' '
            call edisp(iuout,' ')
            call browsefilelist('?','zon','shd',sfile,snpfile,nfile,
     &        iier)
            if(nfile.gt.0)then
              sfile=' '
              snpfile=' '
              call browsefilelist('b','zon','shd',sfile,snpfile,nfile,
     &          iier)
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
   48   CALL EASKS(L,outs,' ',72,D,'shd db',IER,6)
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
        H(1)='The convection regime file contains data for'
        H(2)='controlling the calculation of internal and/or external'
        H(3)='surface convection heat transfer for selected surfaces.'
        H(4)='It is possible to override the default convection'
        H(5)='coefficient correlations for specified surfaces and for'
        H(6)='specified time intervals. You can specify fixed'
        H(7)='coefficients, select from amongst a suite of specialised'
        H(8)='correlations, or impose `adaptive` control over the'
        H(9)='convection calculations.'
        if(IHC(IZ).eq.1)then
          CALL EASKABCD(' For the convection regime file',' :',
     &      'change name','browse','dereference it','continue',IW,9)
          if(IW.EQ.1)then
            L=LHCCO(IZ)
          elseif(IW.EQ.2)then
            sfile=' '
            snpfile=' '
            call edisp(iuout,' ')
            call browsefilelist('?','zon','htc',sfile,snpfile,nfile,
     &        iier)
            if(nfile.gt.0)then
              sfile=' '
              snpfile=' '
              call browsefilelist('b','zon','htc',sfile,snpfile,nfile,
     &          iier)
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

C There is no zone viewfactor file so offer restricted choice.
          CALL EASKABC(' For zone convection regime file',' :',
     &      'edit name','browse','continue',IW,3)
          if(IW.EQ.1)then
            L=' '
          elseif(IW.EQ.2)then
            sfile=' '
            snpfile=' '
            call edisp(iuout,' ')
            call browsefilelist('?','zon','htc',sfile,snpfile,nfile,
     &        iier)
            if(nfile.gt.0)then
              sfile=' '
              snpfile=' '
              call browsefilelist('b','zon','htc',sfile,snpfile,nfile,
     &          iier)
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
   49   CALL EASKS(L,outs,' ',72,D,'Z hc',IER,6)
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
        H(1)='The domain flow file allows the association of a'
        H(2)='computational fluid dynamics (CFD) domain with a'
        H(3)='zone.  This allows intra-zone air movement to be'
        H(4)='evaluated.'
        if(lnblnk(LCFD(IZ)).eq.0)then
          continue
        elseif(LCFD(IZ)(1:7).eq.'UNKNOWN')then
          continue
        else
          CALL EASKABC(' For the zone domain flow file',
     &      ' :','change name','dereference it','cancel',IW,4)
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
   53   CALL EASKS(L,outs,' ',72,D,'Domain flow model file',IER,6)
        IF(L.eq.' '.or.IER.NE.0)GOTO 53
        LCFD(IZ)=L
        modcfg=.TRUE.
      ELSEIF(INO.EQ.17)THEN
        if(browse)then
          call usrmsg('Cannot update model while in browse ',
     &                'mode, you must `own` the model!','W')
          goto 3
        endif
        H(1)='There are four adaptive gridding files which must be'
        H(2)='specified for each zone to which such gridding is to'
        H(3)='be applied. Check documentation for more information.'
        if(ZONE3D(IZ))then
          CALL EASKABC(' For addaptive gridding files',
     &      ' :','change names','dereference ','cancel',IW,4)
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
        WRITE(L,'(A,A4)')zname(IZ)(1:lnblnk(zname(IZ))),'.cvs'
        D = 'new.cvs'
        H(1)='The 3D control volumes file defines ...'
        H(2)='<< more text to be added here >>'
        write(outs,'(3a)')' 3D control volumes file for ',
     &    ZN(1:lnblnk(ZN)),' ?'
        CALL EASKS(L,outs,' ',72,D,'control volumes',IER,2)
        if(L(1:2).ne.'  ')L3DCVS(IZ)=L

        WRITE(L,'(A,A4)')zname(IZ)(1:lnblnk(zname(IZ))),'.cnc'
        D = 'new.cnc'
        H(1)='The 3D node connections file defines ...'
        H(2)='<< more text to be added here >>'
        CALL EASKS(L,' 3D volumes connections file?',
     &      ' ',72,D,'control connections file',IER,2)
        if(L(1:2).ne.'  ')L3DCNC(IZ)=L

        WRITE(L,'(A,A4)')zname(IZ)(1:lnblnk(zname(IZ))),'.ndc'
        D = 'new.ndc'
        H(1)='The 3D nodes coordinates file defines ...'
        H(2)='<< more text to be added here >>'
        CALL EASKS(L,' 3D coordinates file?',
     &      ' ',72,D,'3D coordinates file',IER,2)
        if(L(1:2).ne.'  ')L3DNDC(IZ)=L

        WRITE(L,'(A,A4)')zname(IZ)(1:lnblnk(zname(IZ))),'.3dt'
        D = 'new.3dt'
        H(1)='The 3D nodes temperatures file defines ...'
        H(2)='<< more text to be added here >>'
        CALL EASKS(L,' 3D node temperatures file?',
     &      ' ',72,D,'3D temperatures file',IER,2)
        if(L(1:2).ne.'  ')L3DTAQ(IZ)=L
        ZONE3D(IZ)=.true.
        modcfg=.TRUE.
      ELSEIF(INO.EQ.18)THEN
        if(browse)then
          call usrmsg('Cannot update model while in browse ',
     &                'mode, you must `own` the model!','W')
          goto 3
        endif
        H(1)='Moisture transport data is required for adaptive '
        H(2)='gridding use in the current zone.'
        H(3)='Check documentation for more information.'
        if(MSTRZN(IZ))then
          CALL EASKABC(' For moisture transport file',
     &      ' :','change name','dereference it','cancel',IW,3)
          if(IW.EQ.2)then
            LMOIST(IZ)=' '
            MSTRZN(IZ)=.false.
            goto 3
          elseif(IW.EQ.3)then
            goto 3
          endif
        endif
        WRITE(L,'(A,A4)')zname(IZ)(1:lnblnk(zname(IZ))),'.mst'
        D = 'new.mst'
        H(1)='The moisture file holds moisture transport data'
        H(2)='for nodes. << more text to be added here >>'
        write(outs,'(3a)')' Moisture transport file for ',
     &    ZN(1:lnblnk(ZN)),' ?'
        CALL EASKS(L,outs,' ',72,D,'moisture file',IER,2)
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
        H(1)='structure mesh configuration file is required for '
        H(2)='structured gridding of the current zone.'
        H(3)='Check documentation for more information.'
        if(IZ.eq.IndxSt)then
          CALL EASKABC(' For structured mesh file',
     &      ' :','change name','dereference it','cancel',IW,3)
          if(IW.EQ.2)then
            LGrdSt=' '
            IndxSt=0
            goto 3
          elseif(IW.EQ.3)then
            goto 3
          endif
        endif
        WRITE(L,'(A,A4)')zname(IZ)(1:lnblnk(zname(IZ))),'.csm'
        D = 'new.csm'
        H(1)='The structured mesh file holds structured mesh data'
        H(2)='for a zone.'
        write(outs,'(3a)')' Structured mesh file for ',
     &    ZN(1:lnblnk(ZN)),' ?'
        CALL EASKS(L,outs,' ',72,D,'structured file',IER,2)
        if(L(1:2).ne.'  ')then
          LGrdSt=L
          IndxSt=IZ
        endif
      ELSE
        INO=-4
        GOTO 2
      ENDIF
      INO=-4
      GOTO 3

      END
