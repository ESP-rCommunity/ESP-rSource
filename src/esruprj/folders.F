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

C ********** pregist
C Register a new project. Parameter act = 'i ' then initial pass,
C if 'e ' then provide editing menu, if 'sn' use passed parameters
C for the root and path and folder layout to create a new model,
C if 'sc' silent continue, if 'sw' then pased root and folder layout
C but model is assumed to be created silently within the current folder.
C root (24 char) root name (for 'sn' case)
C mpath (72 char) path to model folder (for 'sn' case)
C folder (12 char) is 'single' or 'distributed' folder structure
C menu (72 char) brief title of model
      subroutine pregist(act,root,mpath,folder,menu,ier)
#include "building.h"
#include "espriou.h"
C espriou.h provides currentfile and MFFOLD.
      
      integer lnblnk  ! function definition

      common/FILEP/IFIL
      common/OUTIN/IUOUT,IUIN
      common/pophelp/h(60)
      
      integer ncomp,ncon
      common/C1/NCOMP,NCON
      integer nccode,indutl
      character LSNAM*72,LPROJ*72,LGEOM*72,LSHAD*72,LTHRM*72,LUTIL*72
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

      common/FOPENED/CFGOK,MLDBOK,MATDBOK,CTLOK,OPTKOK

C Calendar.
      common/caleni/nbdaytype,nbcaldays(10),icalender(365)
      dimension nwlistf(MFFOLD)

C Passed in parameters.
      character root*24,mpath*72,folder*12,menu*72

      logical XST,OK,dok,CFGOK,MLDBOK,MATDBOK,CTLOK,OPTKOK
      logical OKI,concat,unixok,clkok

      character dstmp*24,LCFTMP*24
      character*72 LTMP
      character path*72,LCMDFL*144,LPRJLG*72,LPNF*72,odir*72
      character LCFGF*72,H*72,cfgroot*24
      character*72 DFCFG
      character doit*248,tfile*72,longtfile*144,longtfiledos*144
      character tedlbl*20,teditor*20
      character uname*24,pwd*40,tmode*8
      character limg*72,act*2
      character iformat*4,ifocus*4
      character zonepth*24,netpth*24,ctlpth*24,imgpth*24,radpth*24,fs*1
      character docpth*24,tmppth*24,dbspth*24
      character*72 listf(MFFOLD),listfc(MFFOLD)
      character subpath*72,action*3,outs*124

      integer iyeart    ! for local editing
      integer lnmp,lncr ! for with of mpath & cfgroot strings
      character tcname*248   ! for editing image notes

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

  289 if(act(1:2).eq.'sc')then

C Silent continue, there is nothing to do (currently).
        return
      elseif(act(1:2).eq.'sn')then

C Silent new within the pre-registration process.

C Debug.
C        write(6,*)  ' pregist ',root,' ',folder
C        write(6,*)  ' pregist ',mpath

        write(LCFTMP,'(a)') root(1:lnblnk(root))
      elseif(act(1:2).eq.'sw')then

C Silent within the pre-registration process.

C Debug.
C        write(6,*)  ' pregist ',root,' ',folder
C        write(6,*)  ' pregist ',mpath

        write(LCFTMP,'(a)') root(1:lnblnk(root))

      elseif(act(1:1).eq.'i')then

        H(1)='The model configuration file holds the definition'
        H(2)='of the building, plant and control systems to be'
        H(3)='simulated, including references to associated files.'
        h(4)='The name you supply will be used as the root name'
        h(5)='for the various files corresponding to the model'
        h(6)='you will later establish. Up to 24 characters allowed.'
        LCFTMP='  '
        CALL EASKSCMD(LCFTMP,' ','Model root name?',
     &     'cancel',clkok,24,'new_model','model root name',IER,6)
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
C At some point switch from default version 3 to v4.
      icfgv=4   ! alter this to 4 when working correctly
      INDCFG=0
      XLAT=55.9
      SLAT=55.9
      XLON=-4.1
      SLON=-4.1
      IXPOS=1
      GREF=0.2
      NCOMP=0
      noimg=0
      LPNF='UNKNOWN'
      LPRJLG='UNKNOWN'
      nbdaytype=0
      call st2file(LCFTMP,LCMDFL)

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
        h(1)='If you overwrite the existing configuration file its'
        h(2)='contents will be lost unless you have backed up your'
        h(3)='model.'
        h(4)=' '
        h(5)='If you want to create a new model variant say no and'
        h(6)='enter a different file name.'
        if(act(1:2).eq.'sn')then
          OK=.true.
        elseif(act(1:2).eq.'sw')then
          OK=.true.
        elseif(act(1:1).eq.'i')then
          call askok('There is an existing configuration with',
     &    'this name. Do you want to overwrite it? ',OK,dok,6)
        endif
        if(.not.OK)goto 289
      endif

C Ask where to put it.
C Get the current folder and display options to the user.
      odir=' '
      call usrdir(odir)
      if(act(1:2).eq.'sw')then
        if(folder(1:6).eq.'single')then

C Assume all types of files go in the current folder. Set path
C variable to equal mpath (which was passed into this subroutine).
          lnmp=lnblnk(mpath)
          write(path,'(a)') mpath(1:lnmp)
          write(zonepth,'(2a)')'.',fs
          write(netpth,'(2a)')'.',fs
          write(ctlpth,'(2a)')'.',fs
          write(imgpth,'(2a)')'.',fs
          write(radpth,'(2a)')'.',fs
          write(docpth,'(2a)')'.',fs
          write(tmppth,'(2a)')'.',fs
          write(dbspth,'(2a)')'.',fs

C Re-establish pwd and then project folders.
          call edisp(iuout,
     &        'System configuration file is located in folder:')
          call edisp(iuout,path)

C Write out the model log file and then jump past the standard
C dialogs.
          uname=' '
          call usrname(uname)
          call usrdir(pwd)
          write(LSNAM,'(a)') menu(1:lnblnk(menu))
          write(LPRJLG,'(2a)') cfgroot(1:lnblnk(cfgroot)),'.log'
          write(currentfile,'(a)') LPRJLG(1:lnblnk(LPRJLG))
          IUNIT=IFIL+1
          CALL EFOPSEQ(IUNIT,LPRJLG,3,IER)
          write(iunit,'(2a)')'Project notes for ',LCFGF(1:lnblnk(LCFGF))
          write(iunit,'(2a)')'Description: ',LSNAM(1:lnblnk(LSNAM))
          write(iunit,'(2a)')'In folder: ',pwd(1:lnblnk(pwd))
          write(iunit,'(2a)')'By: ',uname(1:lnblnk(uname))
          call dstamp(dstmp)
          write(iunit,'(2a)')'Date: ',dstmp
          write(iunit,'(a)')'Client: '
          write(iunit,'(a)')'Project reference: '
          write(iunit,'(a)')'_____________________________'
          write(iunit,'(a)')'Notes: '
          write(iunit,'(a)')'_____________________________'
          CALL ERPFREE(IUNIT,ISTAT)
          goto 77
        endif

      elseif(act(1:2).eq.'sn')then

        if(folder(1:6).eq.'single')then

C Assume all types of files go in the single project folder (mpath).
          lnmp=lnblnk(mpath)
          write(doit,'(2a)') 'mkdir ',mpath(1:lnmp)
          call usrmsg('Creating model folder:',doit,'-')
          call runit(doit,'-')
          call pausems(400)
          call usrmsg('  ','  ','-')
          write(zonepth,'(2a)')'.',fs
          write(netpth,'(2a)')'.',fs
          write(ctlpth,'(2a)')'.',fs
          write(imgpth,'(2a)')'.',fs
          write(radpth,'(2a)')'.',fs
          write(docpth,'(2a)')'.',fs
          write(tmppth,'(2a)')'.',fs
          write(dbspth,'(2a)')'.',fs

C Re-establish pwd and then project folders.
          write(path,'(2a)')mpath(1:lnblnk(mpath)),fs
          call edisp(iuout,
     &        'System configuration file is located in folder:')
          call edisp(iuout,path)

        elseif(folder(1:11).eq.'distributed')then

C Create the folder mpath and inside that the standard folders.
          lnmp=lnblnk(mpath)
          write(doit,'(2a)') 'mkdir ',mpath(1:lnmp)
          call usrmsg('Creating model folder:',doit,'-')
          call runit(doit,'-')
          call pausems(400)
          call usrmsg('  ','  ','-')
          write(doit,'(4a)') 'mkdir ',mpath(1:lnmp),fs,'cfg'
          call usrmsg('Creating folder:',doit,'-')
          call runit(doit,'-')
          call pausems(400)
          write(doit,'(4a)') 'mkdir ',mpath(1:lnmp),fs,'ctl'
          call usrmsg('Creating folder:',doit,'-')
          call runit(doit,'-')
          call pausems(400)
          write(doit,'(4a)') 'mkdir ',mpath(1:lnmp),fs,'zones'
          call usrmsg('Creating folder:',doit,'-')
          call runit(doit,'-')
          call pausems(400)
          write(doit,'(4a)') 'mkdir ',mpath(1:lnmp),fs,'nets'
          call usrmsg('Creating folder:',doit,'-')
          call runit(doit,'-')
          call pausems(400)
          write(doit,'(4a)') 'mkdir ',mpath(1:lnmp),fs,'images'
          call usrmsg('Creating folder:',doit,'-')
          call runit(doit,'-')
          call pausems(400)
          write(doit,'(4a)') 'mkdir ',mpath(1:lnmp),fs,'doc'
          call usrmsg('Creating folder:',doit,'-')
          call runit(doit,'-')
          call pausems(400)
          write(doit,'(4a)') 'mkdir ',mpath(1:lnmp),fs,'rad'
          call usrmsg('Creating folder:',doit,'-')
          call runit(doit,'-')
          call pausems(400)
          write(doit,'(4a)') 'mkdir ',mpath(1:lnmp),fs,'dbs'
          call usrmsg('Creating folder:',doit,'-')
          call runit(doit,'-')
          call pausems(400)

          write(zonepth,'(3a)')'..',fs,'zones'
          write(netpth,'(3a)')'..',fs,'nets'
          write(ctlpth,'(3a)')'..',fs,'ctl'
          write(imgpth,'(3a)')'..',fs,'images'
          write(radpth,'(3a)')'..',fs,'rad'
          write(docpth,'(3a)')'..',fs,'doc'
          write(tmppth,'(3a)')'..',fs,'tmp'
          write(dbspth,'(3a)')'..',fs,'dbs'

C Re-establish pwd and then project folders.
          write(path,'(4a)')mpath(1:lnblnk(mpath)),fs,'cfg',fs
          call edisp(iuout,
     &        'System configuration file is located in folder:')
          call edisp(iuout,path)
        endif 

C Write out the model log file.
        uname=' '
        call usrname(uname)
        call usrdir(pwd)
        write(LSNAM,'(a)') menu(1:lnblnk(menu))
        write(LPRJLG,'(2a)') cfgroot(1:lnblnk(cfgroot)),'.log'
        write(currentfile,'(a)') LPRJLG(1:lnblnk(LPRJLG))
        IUNIT=IFIL+1
        CALL EFOPSEQ(IUNIT,LPRJLG,3,IER)
        write(iunit,'(2a)')'Project notes for ',LCFGF(1:lnblnk(LCFGF))
        write(iunit,'(2a)')'Description: ',LSNAM(1:lnblnk(LSNAM))
        write(iunit,'(2a)')'In folder: ',pwd(1:lnblnk(pwd))
        write(iunit,'(2a)')'By: ',uname(1:lnblnk(uname))
        call dstamp(dstmp)
        write(iunit,'(2a)')'Date: ',dstmp
        write(iunit,'(a)')'Client: '
        write(iunit,'(a)')'Project reference: '
        write(iunit,'(a)')'_____________________________'
        write(iunit,'(a)')'Notes: '
        write(iunit,'(a)')'_____________________________'
        CALL ERPFREE(IUNIT,ISTAT)
        goto 77

      elseif(act(1:1).eq.'i')then

C Offer choice of single folder or standard set of folders to hold
C the model description.
        H(1)='ESP-r organises a model within a standard set of folders.'
        H(2)=' '
        H(3)='Based on information you have supplied,'
        H(4)=' '
        lncr=lnblnk(cfgroot)
        write(H(5),'(3x,2a)')cfgroot(1:lncr),' (project folder)'
        write(H(6),'(3x,2a)')cfgroot(1:lncr),'/cfg (system files)'
        write(H(7),'(3x,2a)')cfgroot(1:lncr),'/ctl (control files)'
        write(H(8),'(3x,2a)')cfgroot(1:lncr),'/zones (zone files)'
        write(H(9),'(3x,2a)')cfgroot(1:lncr),'/nets (networks)'
        write(H(10),'(3x,2a)')cfgroot(1:lncr),'/doc (reports and notes)'
        write(H(11),'(3x,2a)')cfgroot(1:lncr),'/tmp (odds+ends)'
        write(H(12),'(3x,2a)')cfgroot(1:lncr),'/dbs (project databases)'
        H(13)=' '
        H(14)='will be created. '
        H(15)=' '
        H(16)='Or, if the model will be small (one or two zones)'
        H(17)='all the files can be in one folder shown below:'
        H(18)=' '
        write(H(19),'(3x,a,a)')cfgroot(1:lnblnk(cfgroot)),
     &    ' (project folder)'
        CALL EASKABC(' Create model in:',
     &    '(see help)','single folder','standard set of folders',
     &    'cancel',IW,19)
      endif

      if(IW.eq.1)then

C Assume all types of files go in the single project folder.
        write(zonepth,'(2a)')'.',fs
        write(netpth,'(2a)')'.',fs
        write(ctlpth,'(2a)')'.',fs
        write(imgpth,'(2a)')'.',fs
        write(radpth,'(2a)')'.',fs
        write(docpth,'(2a)')'.',fs
        write(tmppth,'(2a)')'.',fs
        write(dbspth,'(2a)')'.',fs
        lncr=lnblnk(cfgroot)
        write(odir,'(a)')cfgroot(1:lncr)
        write(doit,'(2a)') 'mkdir ',cfgroot(1:lncr)
        call usrmsg('Creating folder:',doit,'-')
        call runit(doit,'-')
        call pausems(400)
        call usrmsg('  ','  ','-')

C Re-establish pwd and then project folders.
        odir=' '
        call usrdir(odir)
        write(path,'(4a)')odir(1:lnblnk(odir)),fs,cfgroot(1:lncr),fs
        call edisp(iuout,
     &        'System configuration file is located in folder:')
        call edisp(iuout,path)
        write(path,'(2a)') cfgroot(1:lncr),fs
        write(outs,'(2a)') 'Resetting path to: ',path
        call edisp(iuout,outs)

      elseif(IW.eq.2)then

C Setup the various paths to distributed folders and then create.
        write(zonepth,'(3a)')'..',fs,'zones'
        write(netpth,'(3a)')'..',fs,'nets'
        write(ctlpth,'(3a)')'..',fs,'ctl'
        write(imgpth,'(3a)')'..',fs,'images'
        write(radpth,'(3a)')'..',fs,'rad'
        write(docpth,'(3a)')'..',fs,'doc'
        write(tmppth,'(3a)')'..',fs,'tmp'
        write(dbspth,'(3a)')'..',fs,'dbs'
        lncr=lnblnk(cfgroot)
        write(odir,'(a)')cfgroot(1:lncr)
        write(doit,'(2a)') 'mkdir ',cfgroot(1:lncr)
        call usrmsg('Creating folder:',doit,'-')
        call runit(doit,'-')
        call pausems(400)
        write(doit,'(4a)') 'mkdir ',cfgroot(1:lncr),fs,'cfg'
        call usrmsg('Creating folder:',doit,'-')
        call runit(doit,'-')
        call pausems(400)
        write(doit,'(4a)') 'mkdir ',cfgroot(1:lncr),fs,'ctl'
        call usrmsg('Creating folder:',doit,'-')
        call runit(doit,'-')
        call pausems(400)
        write(doit,'(4a)') 'mkdir ',cfgroot(1:lncr),fs,'zones'
        call usrmsg('Creating folder:',doit,'-')
        call runit(doit,'-')
        call pausems(400)
        write(doit,'(4a)') 'mkdir ',cfgroot(1:lncr),fs,'nets'
        call usrmsg('Creating folder:',doit,'-')
        call runit(doit,'-')
        call pausems(400)
        write(doit,'(4a)') 'mkdir ',cfgroot(1:lncr),fs,'images'
        call usrmsg('Creating folder:',doit,'-')
        call runit(doit,'-')
        call pausems(400)
        write(doit,'(4a)') 'mkdir ',cfgroot(1:lncr),fs,'doc'
        call usrmsg('Creating folder:',doit,'-')
        call runit(doit,'-')
        call pausems(400)
        write(doit,'(4a)') 'mkdir ',cfgroot(1:lncr),fs,'tmp'
        call usrmsg('Creating folder:',doit,'-')
        call runit(doit,'-')
        call pausems(400)
        write(doit,'(4a)') 'mkdir ',cfgroot(1:lncr),fs,'rad'
        call usrmsg('Creating folder:',doit,'-')
        call runit(doit,'-')
        call pausems(400)
        write(doit,'(4a)') 'mkdir ',cfgroot(1:lncr),fs,'dbs'
        call usrmsg('Creating folder:',doit,'-')
        call runit(doit,'-')
        call pausems(400)
        call usrmsg('  ','  ','-')

C Re-establish pwd and then project folders.
        odir=' '
        call usrdir(odir)
        write(path,'(6a)')odir(1:lnblnk(odir)),fs,
     &      cfgroot(1:lnblnk(cfgroot)),fs,'cfg',fs
        call edisp(iuout,
     &        'System configuration file is located in folder:')
        call edisp(iuout,path)
        write(path,'(4a)') cfgroot(1:lncr),fs,'cfg',fs
        write(outs,'(2a)') 'Resetting path to: ',path
        call edisp(iuout,outs)

      elseif(IW.eq.3)then
        return
      endif

C General description of the model.
  52  H(1)='A model has an associated descriptor (<72 characters,'
      H(2)='which is used later as an identifier, e.g. Low energy'
      H(3)='building with optimum start/stop plant control.'
      LSNAM='  '
      ltmp=LSNAM
      CALL EASKS(ltmp,' ','Model description?',
     &           72,'Base case model','model description',IER,3)
      if(ltmp(1:2).ne.'  '.or.lnblnk(ltmp).gt.3)then
        LSNAM=ltmp
      else
        call usrmsg('Sorry, a blank line is not acceptable.',
     &    'Enter a meaningful description (<7 characters)','W')
        goto 52
      endif

C Documentation.
  53  H(1)='The project log file holds a description of the project'
      H(2)='and the analysis intentions. It is located in the `cgf`'
      H(3)='directory. Keep it up to date via a text editor.'
      if(LPRJLG(1:2).eq.'  '.or.LPRJLG(1:4).eq.'UNKN')then
        write(LPRJLG,'(a,a)') cfgroot(1:lnblnk(cfgroot)),'.log'
      endif
      ltmp=LPRJLG
      CALL EASKS(ltmp,' ','Project log file?',
     &           72,'project.log','log file',IER,3)
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
          h(2)='Check that it is not an empty file or contains'
          h(3)='non-printable characters. '
          CALL ASKOK('Problem detected with log file!',
     &                'Try again?',OK,dok,3)
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
        write(iunit,'(a)')'Client: '
        write(iunit,'(a)')'Project reference: '
        write(iunit,'(a)')'_____________________________'
        write(iunit,'(a)')'Notes: '
        write(iunit,'(a)')'_____________________________'
        CALL ERPFREE(IUNIT,ISTAT)
      endif

C Allow user to edit the log file. Append to path if
C necessary.  If vi then spawn a new window to do the editing.

C << This would be a good place to offer an internal editing popup
C << which uses GTK facilities.
      dok=.false.
      h(1)='The log file is used to document your model and record the'
      h(2)='purpose and outcomes of commissioned simulations. Update'
      h(3)='it using a text editor as your modelling project evolves.'
      h(4)='The file is located in the `cfg`directory of your model'
      h(5)='folder.'
      h(6)=' '
      h(7)='Assuming you have an editor associated with ESP-r (defined'
      h(8)='via your .esprc file), you can edit the log file now.'

      CALL ASKOK(' ','Edit log file?',OK,dok,8)
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
      h(1)='A model can usefully include references to images'
      h(2)='depicting different model aspects such as geometry,'
      h(3)='flow networks, simulation results and the like.'
      h(4)='Such images are usually located in the `images` folder.'
      h(5)=' '
      h(6)='You can either identify such images now or add them'
      h(7)='later via the `model context->images` menu item.'
      CALL ASKOK(' ','Associate images now?',OKI,dok,7)
      if(OKI)then

C Provide a listing of the current contents of the ../images folder.
        subpath=' '
        write(subpath,'(4a)') odir(1:lnblnk(odir)),fs,
     &  path(1:lnblnk(path)),imgpth(1:lnblnk(imgpth))
        call clearfilelist(listf,MFFOLD)
        action='fil'
        call getfilelist(subpath,action,listf,nwlistf,nlistf)
        call copyfilelist(listf,nwlistf,listfc,MFFOLD,maxw)
        call printfilelist(outs,'p',listfc,MFFOLD,nlistf)

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

C Edit operation description if newer file.
          if(icfgv.gt.3)then
            H(1)='Notes (<248 char) for images can help with' 
            H(2)='communicating supporting information.'
            tcname=imgdoc(noimg)
            CALL EASKS248(tcname,' ','Image notes:',
     &        72,'associated image','image notes',IER,2)
            if(tcname(1:2).ne.'  ')imgdoc(noimg)=tcname
          endif
        endif
        if(noimg.lt.MIMG)then
          dok=.false.
          h(1)='If you have more images select ok.'
          CALL ASKOK(' ',' Another image?',OK,dok,1)
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
        H(1)='Site latitude in degrees, where North is +ve, e.g. 55.9'
        H(2)='for Glasgow.'
        CALL EASKR(SLAT,' ','Site latitude?',
     &       -89.9,'W',89.9,'W',55.9,'site latitude',IER,2)
        IF(IER.EQ.0)XLAT=SLAT

        H(1)='Site longitude difference is the difference in degrees'
        H(2)='from the local reference meridian, East is +ve, e.g.'
        H(3)='-4.1 for Glasgow (relative to the Greenwich Meridian).'
        CALL EASKR(SLON,' ','Longitude difference?',
     &       -15.0,'W',15.0,'W',-4.1,'longitude difference',IER,3)
        IF(IER.EQ.0)XLON=SLON

C Ask for year, initialise and then print out the calendar if the
C user does not cancel the operation.
        H(1)='The assessment year is required for event scheduling.'
        H(2)='You can either use the value offered or provide an'
        H(3)='alternative.'
        H(4)=' '
        H(5)='The cancel option will accept the year associated with'
        H(6)='the climate file used in a simultion.'
        iyeart=iyear
        CALL EASKI(IYEART,' ','Assessment year?',
     &       1900,'W',3000,'W',2007,'assessment year',IERI,6)
        if(ieri.eq.-3) then
          call calenmanage('i',ier)
        else
          iyear=iyeart
          call calenmanage('i',ier)
          loop=8
          loopst=1
C          call calenprint(iuout,'t',iyear,loopst,loop)
C          call edisp(iuout,' ')
C          call calenprint(iuout,'g',iyear,loopst,loop)
C          call edisp(iuout,' ')
        endif

        H(1)='Other site information, such as ground reflectivity and'
        H(2)='wind exposure, have been automatically set to default'
        H(3)='values and these may be changed via the `model context`'
        H(4)='menu option accessed via the `browse/edit/simulate`'
        H(5)='option of the present menu.'
        H(6)=' '
        H(7)='If you have additional day types (e.g. summer holiday) '
        H(8)='that you want to define, do this before working with '
        H(9)='any schedules. '
        CALL PHELPD('new site',9,'-',0,0,IER)
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
        return
      endif
      return
      end

C ********** pfolders
C Maintain model folders.
      subroutine pfolders(act,ier)
#include "building.h"

C geometry.h provides commons G0 and G2.
#include "geometry.h"
#include "espriou.h"
      
      integer lnblnk  ! function definition

      common/FILEP/IFIL
      common/OUTIN/IUOUT,IUIN
      common/pophelp/h(60)
      
      integer ncomp,ncon
      common/C1/NCOMP,NCON
      integer nccode,indutl
      character LSNAM*72,LPROJ*72,LGEOM*72,LSHAD*72,LTHRM*72,LUTIL*72
      common/C2/LSNAM,NCCODE(MCOM),LPROJ(MCOM),LGEOM(MCOM),
     &          LSHAD(MCOM),LTHRM(MCOM),INDUTL(MCOM),LUTIL(MCOM)
      common/C3F/LCNN
      common/C6/INDCFG
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
      dimension nwlistf(MFFOLD)
      logical OK,DOK,CFGOK,MLDBOK,MATDBOK,CTLOK,OPTKOK
      logical concat,unixok,changedit,browse

      character*72 path,LFIL,odir
      character LCFGF*72,H*72,cfgroot*24
      character*72 LCNN
      character doit*248,tfile*72,sfile*72,snpfile*72
      character longtfile*144,longtfiledos*144
      character tedlbl*20,teditor*20
      character tmode*8,t24*24,act*1,outstr*124,outs*124
      character*33 ITEMS(21)
      character*72 listf(MFFOLD),listfc(MFFOLD)
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

C If subpath is less than 72 char with odir + path use that
C otherwise just use path.

C Debug...
C      write(6,*) 'usrdir is ',odir
C      write(6,*) 'path is ',path

      subpath=' '
      lnodir=lnblnk(odir)
      lnpath=lnblnk(path)
      if(lnodir+lnpath+1.le.72)then
        write(subpath,'(3a)') odir(1:lnodir),fs,
     &  path(1:lnpath)
      else
        write(subpath,'(a)') path(1:lnpath)
      endif
      write(outs,'(2a)') 'configuration files in ',
     &  subpath(1:lnblnk(subpath))

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
      call clearfilelist(listf,MFFOLD)
      action='cfg'
      call getfilelist(subpath,action,listf,nwlistf,nlistf)
      call copyfilelist(listf,nwlistf,listfc,MFFOLD,maxw)
      call printfilelist(outs,'p',listfc,MFFOLD,nlistf)

  42  continue
      INO=-4
      WRITE(ITEMS(1),'(2A)') '  name: ',LCFGF(1:24)
      WRITE(ITEMS(2),'(2A)') '  path: ',path(1:23)
      WRITE(ITEMS(3),'(A)')  '  .... Relative paths to .... '
      WRITE(ITEMS(4),'(2A)') 'b zones   : ',zonepth(1:20)
      WRITE(ITEMS(5),'(2A)') 'c control : ',ctlpth(1:20)
      WRITE(ITEMS(6),'(2A)') 'd network : ',netpth(1:20)
      WRITE(ITEMS(7),'(2A)') 'e images  : ',imgpth(1:20)
      WRITE(ITEMS(8),'(2A)') 'f document: ',docpth(1:20)
      WRITE(ITEMS(9),'(2A)') 'g temp.   : ',tmppth(1:20)
      WRITE(ITEMS(10),'(2A)')'h radiance: ',radpth(1:20)
      WRITE(ITEMS(11),'(2A)')'i local db: ',dbspth(1:20)
      ITEMS(12)  =           ' _____________________________ '
      ITEMS(13)  =           'j zone files                   '
      ITEMS(14)  =           'k scan and rewrite model files '
      ITEMS(15)  =           ' _____________________________ '
      WRITE(ITEMS(16),'(A)') 'l list ASCII file              '
      WRITE(ITEMS(17),'(A)') 'm edit ASCII file              '
      WRITE(ITEMS(18),'(A)') 'n list files in esp-r/databases'
      WRITE(ITEMS(19),'(A)') 'o list files in esp-r/climate  '
      ITEMS(20)  =           '? help                         '
      ITEMS(21)  =           '- exit this menu               '
      nitems=21

C Help text for this menu.
      H(1)='This menu allows you to edit the information related'
      H(2)='to a project`s folders and files.'
      H(3)=' '
      H(4)='The path options allow new files to be positioned in'
      H(5)='standard folders. The paths are relative to the'
      H(6)='location of the system configuration file.'
      H(7)=' '
      H(8)='The option to examine a specified ASCII file is'
      H(9)='useful if you have need to directly view the contents'
      H(10)='of a problem describing entity such as the system'
      H(11)='configuration file or a zone geometry file. There'
      H(12)='is also an option to strip comments from files where'
      H(13)='that will make them compatible with older versions'
      H(14)='of the Project Manager.'
      H(15)=' '
      H(16)='Editing an ASCII file - such as the project log'
      H(17)='file - requires a text editor which is currently'
      write(H(18),'(3a)')'defined to be `',teditor(1:lnblnk(teditor)),
     &                   '`.'
      H(19)='This can be re-defined in your .esprc file.'

      CALL EMENU('Folders and Files',ITEMS,nitems,INO)
      IF(INO.EQ.nitems)THEN
        if(browse)then
          continue
        else
          if(changedit)then
            dok=.true.
            h(1)='ESP-r thinks that recent model changes have been' 
            h(2)='made. Because these changes are held only in'
            h(3)='computer memory they will be lost if not saved to'
            h(4)='file. Agree to updating the file.'
            CALL ASKOK(' ','Update system configuration file?',
     &                 OK,dok,4)
            if(OK)CALL EMKCFG('-',IER)
            if(ier.eq.0)CFGOK=.TRUE.
            changedit=.false.
          endif
        endif
        return
      elseif(ino.eq.nitems-1)then
        CALL PHELPD('folders and files',19,'-',0,0,IER)
      elseif(ino.eq.4)then
        if(.NOT.browse)then
          H(1)='Give relative path from cfg to zones folder.'
          H(2)='For example `../zones`.'
          t24=zonepth
          CALL EASKS(t24,'Relative path from cfg',
     &      'folder to zones folder?',
     &       24,'../zones','relative zones path',IER,2)
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
          H(1)='Give relative path from cfg to controls folder.'
          H(2)='For example `../ctl`.'
          t24=ctlpth
          CALL EASKS(t24,'Relative path from cfg',
     &      'folder to controls folder?',
     &       24,'../ctl','relative controls path',IER,2)
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
          H(1)='Give relative path from cfg to networks folder.'
          H(2)='For example `../nets`.'
          t24=netpth
          CALL EASKS(t24,'Relative path from cfg',
     &      'folder to networks folder?',
     &       24,'../nets','relative networks path',IER,2)
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
      elseif(ino.eq.7)then
        if(.NOT.browse)then
          H(1)='Give relative path from cfg to images folder.'
          H(2)='For example `../images`.'
          t24=imgpth
          CALL EASKS(t24,'Relative path from cfg',
     &      'folder to images folder?',
     &       24,'../images','relative img path',IER,2)
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
      elseif(ino.eq.8)then
        if(.NOT.browse)then
          H(1)='Give relative path from cfg to documents folder.'
          H(2)='For example `../docs`.'
          t24=docpth
          CALL EASKS(t24,'Relative path from cfg',
     &       'folder to documents folder?',24,'../doc',
     &        'relative doc path',IER,1)
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
      elseif(ino.eq.9)then
        if(.NOT.browse)then
          H(1)='Give relative path from cfg to temporary folder.'
          H(2)='For example `../tmp`.'
          H(2)='Holds transient information.'
          t24=tmppth
          CALL EASKS(t24,'Relative path from cfg',
     &      'folder to temporary folder?',24,'../doc',
     &      'relative temp path',IER,2)
          if(t24(1:2).ne.'  '.and.t24(1:4).ne.'UNKN')then
            tmppth=t24
            changedit=.true.
            sfile=' '
            snpfile=' '
            call browsefilelist('p','tmp','fil',sfile,snpfile,nli,iier)
          endif
        else
          sfile=' '
          snpfile=' '
          call browsefilelist('p','tmp','fil',sfile,snpfile,nli,iier)
        endif
      elseif(ino.eq.10)then
        if(.NOT.browse)then
          H(1)='Give relative path from cfg to Radiance folder.'
          H(2)='For example `../rad`.'
          t24=radpth
          CALL EASKS(t24,'Relative path from cfg folder',
     &      'folder to Radiance folder?',24,'../rad',
     &      'relative radiance path',IER,2)
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
      elseif(ino.eq.11)then
        subpath=' '
        write(subpath,'(4a)') odir(1:lnblnk(odir)),fs,
     &    path(1:lnblnk(path)),dbspth(1:lnblnk(dbspth))
        write(outs,*) 'files in ',subpath
        call clearfilelist(listf,MFFOLD)
        action='fil'
        call getfilelist(subpath,action,listf,nwlistf,nlistf)
        call copyfilelist(listf,nwlistf,listfc,MFFOLD,maxw)
        call printfilelist(outs,'p',listfc,MFFOLD,nlistf)

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

      elseif(ino.eq.14)then

C Option to scan in current model files and then write them out
C again, updating the file format as per value of igupgrade.
C << to be done >>

      elseif(ino.eq.16)then

C List out an ASCII file.
        H(1)='The specified ASCII file will be read and displayed'
        H(2)='in the text window page by page.'
    6   CALL EASKS(LFIL,' ','ASCII file name?',
     &            72,' ','ASCII file',IER,2)

        IUNIT=IFIL+1
        CALL LISTAS(IUNIT,LFIL,IER)
        IF(IER.LT.0)THEN
          dok=.false.
          h(1)='Chance to correct error in file name or location.'
          CALL ASKOK(' Problem detected while trying to open',
     &                ' ASCII file. Try again?',OK,dok,1)
          IF(OK)GOTO 6
        ENDIF
      elseif(ino.eq.17)then

C Allow user to edit a file. Append to path if necessary.
C If vi then spawn a new window to do the editing in.
        H(1)='An ASCII format file can be edited using the current'
        H(2)='defined text editor.'
        CALL EASKS(LFIL,' ','ASCII file to edit?',
     &              72,' ','ASCII file',IER,2)
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
      elseif(ino.eq.18)then

C List files in corporate databases folder.
        sfile=' '
        snpfile=' '
        call browsefilelist('p','dbm','fil',sfile,snpfile,nli,iier)
        if(nli.gt.MFFOLD) then
          call usrmsg('The folder includes more files than the list',
     &      'can handle - please be careful!','W')
        endif
      elseif(ino.eq.19)then

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

C      integer lnblnk  ! function definition
      COMMON/FILEP/IFIL
      common/OUTIN/IUOUT,IUIN
      common/pophelp/h(60)
      COMMON/SPAD/MMOD,LIMIT,LIMTTY

      logical XST,OK,dok
      character iformat*4,ifocus*4,limg*72,t5*5
      character H*72,outs*124

C Display current information.
      ier=0
C      call edisp(iuout,'The current image is:')
C      call edisp(iuout,'Format  Focus File')
C      write(outs,'(6a)') ' ',iformat,' ',ifocus,' ',
C     &  limg(1:lnblnk(limg))
C      call edisp(iuout,outs)

 242  H(1)='Images will be displayed when the project is loaded.'
      H(2)='The default image type is GIF.'
      CALL EASKS(limg,' ','Name of image file?',
     &          72,'first.gif','image file',IER,2)
      if(limg(1:2).eq.'  '.or.limg(1:4).eq.'UNKN')goto 242
      XST=.false.
      call findfil(limg,XST)
      if(.NOT.XST)then
        dok=.false.
        h(1)='Chance to change file name or location.'
        CALL ASKOK('Could not locate image file.','Try again?',
     &             OK,dok,1)
        if(OK)then
          goto 242
        else
          ier=2
          return
        endif
      endif

      H(1)='Select one of these standard image types. Each type'
      H(2)='must have a related display application specified'
      H(3)='in your .esprc file, e.g. *image_display GIF xv.'
      call EASKATOG(' ','Image format?',
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
        CALL EASKS(t5,' ','Image type character code?',
     &               5,'****','image char code',IER,3)
        if(t5(1:1).eq.' ')goto 145
        iformat=t5(1:4)
      endif

      h(1)='If an image is tagged `general` then it will be'
      h(2)='displayed when the model is loaded, otherwise it'
      h(3)='will only be displayed when requested via a graphic'
      h(4)='display button.'
      call EASKATOG(' ','Image focus?',
     &  'general','building','networks','control',
     &  'CFD','performance','other',IRF,4)
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
 144    H(1)='Image files have a 4 character focus tag, e.g.'
        H(2)='FZON for zones, FNET for networks etc.'
        t5='**** '
        CALL EASKS(t5,' ','Image type character code?',
     &    5,'**** ','usr image focus',IER,2)
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
#include "geometry.h"
#include "CFC_common.h"
      
      integer lnblnk  ! function definition

      COMMON/SPAD/MMOD,LIMIT,LIMTTY
      common/OUTIN/IUOUT,IUIN
      common/FILEP/IFIL
      common/pophelp/h(60)
      common/rpath/path
      common/paths/zonepth,netpth,ctlpth,imgpth,radpth,docpth,tmppth,
     &             dbspth

      integer nccode,indutl
      character LSNAM*72,LPROJ*72,LGEOM*72,LSHAD*72,LTHRM*72,LUTIL*72
      common/C2/LSNAM,NCCODE(MCOM),LPROJ(MCOM),LGEOM(MCOM),
     &          LSHAD(MCOM),LTHRM(MCOM),INDUTL(MCOM),LUTIL(MCOM)
      common/CFGV/icfgv
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
      logical newgeo  ! to use for testing if new/old geometry file.
      character*72 LVIEW,LHCCO,LTWIN,LCGCIN,ZOBS
      character*72 L3DCVS,L3DCNC,L3DNDC,L3DTAQ,LMOIST,LGrdSt
      dimension ITEMS(25)
      character ITEMS*72,head*25,path*72,L*72,ZN*12
      character LCFD*72,H*72,outs*124
      character D*72,T15*12,D15*12
      character zonepth*24,netpth*24,ctlpth*24,imgpth*24,radpth*24
      character docpth*24,tmppth*24,dbspth*24
      character sfile*72,snpfile*72,fs*1
      logical modcfg,XST,unixok

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
       WRITE(ITEMS(20),'(2A)') 's  CFC constr: ',lcfcin(IZ)(1:IWD)
      endif
      ITEMS(21)='  _____________________________    '
      ITEMS(22)='* check existence of zone files    '
      ITEMS(23)='? help                             '
      ITEMS(24)='- exit this menu                   '
      MITEM=24

C Help text for this menu.
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
        call eclose(gversion(IZ),1.1,0.01,newgeo)
        if(newgeo)then
          call georead(IUF,LGEOM(IZ),IZ,1,iuout,IER)
        else
          call egomin(IUF,LGEOM(IZ),IZ,1,0,iuout,IER)
        endif
        H(1)='The zone name is used as an identifier during'
        H(2)='results analysis. Names should be unique.'
        T15=zname(IZ)
        D15='new_zone'
        CALL EASKS(T15,' ','Zone name?',12,D15,'zone name',IER,2)
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
   40   CALL EASKS(L,outs,' ',72,D,'Z operations',IER,2)
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
          H(1)='The zone obstructions file contains the geometry of'
          H(2)='obstruction blocks for use in shading/ insolation'
          H(3)='calculations. You may browse for existing files.'
          if(IOBS(IZ).eq.1)then
            CALL EASKABCD(' For zone obstruction file',' :',
     &        'edit name','browse','dereference it','continue',IW,3)
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
     &        ' :','edit name','browse','continue',IW,3)
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
   47     CALL EASKS(L,outs,' ',72,D,'Z obstr',IER,3)
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
     &      'edit name','browse','continue',IW,3)
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
     &      'edit name','browse','continue',IW,3)
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
     &      'edit name','browse','continue',IW,3)
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
     &      'edit name','browse','continue',IW,3)
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
     &      'edit name','browse','continue',IW,3)
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
        WRITE(L,'(A,A4)')zname(IZ)(1:lnzname(IZ)),'.cvs'
        D = 'new.cvs'
        H(1)='The 3D control volumes file defines ...'
        H(2)='<< more text to be added here >>'
        write(outs,'(3a)')' 3D control volumes file for ',
     &    ZN(1:lnblnk(ZN)),' ?'
        CALL EASKS(L,outs,' ',72,D,'control volumes',IER,2)
        if(L(1:2).ne.'  ')L3DCVS(IZ)=L

        WRITE(L,'(A,A4)')zname(IZ)(1:lnzname(IZ)),'.cnc'
        D = 'new.cnc'
        H(1)='The 3D node connections file defines ...'
        H(2)='<< more text to be added here >>'
        CALL EASKS(L,' 3D volumes connections file?',
     &      ' ',72,D,'control connections file',IER,2)
        if(L(1:2).ne.'  ')L3DCNC(IZ)=L

        WRITE(L,'(A,A4)')zname(IZ)(1:lnzname(IZ)),'.ndc'
        D = 'new.ndc'
        H(1)='The 3D nodes coordinates file defines ...'
        H(2)='<< more text to be added here >>'
        CALL EASKS(L,' 3D coordinates file?',
     &      ' ',72,D,'3D coordinates file',IER,2)
        if(L(1:2).ne.'  ')L3DNDC(IZ)=L

        WRITE(L,'(A,A4)')zname(IZ)(1:lnzname(IZ)),'.3dt'
        D = 'new.3dt'
        H(1)='The 3D nodes temperatures file defines the'
        H(2)='location of additional construction nodes'
        H(3)='corresponding to the finer discretisation'
        H(4)='of a multi-layered construction.'
        CALL EASKS(L,' 3D node temperatures file?',
     &      ' ',72,D,'3D temperatures file',IER,4)
        if(L(1:2).ne.'  ')L3DTAQ(IZ)=L
        ZONE3D(IZ)=.true.
        modcfg=.TRUE.
      ELSEIF(INO.EQ.18)THEN
        if(browse)then
          call usrmsg('Cannot update model while in browse ',
     &                'mode, you must first `own` the model!','W')
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
        WRITE(L,'(A,A4)')zname(IZ)(1:lnzname(IZ)),'.mst'
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
        WRITE(L,'(A,A4)')zname(IZ)(1:lnzname(IZ)),'.csm'
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
      ELSEIF(INO.EQ.20)THEN
        if(browse)then
          call usrmsg('Cannot update model while in browse ',
     &                'mode, you must `own` the model!','W')
          goto 3
        endif
        H(1)='The zone complex fenestration constructions (CFC)'
        H(2)='provides a more exact treatment for windows which'
        H(3)='can include operable slat-type shading layers.'
        if(icfc(IZ).eq.1)then
          CALL EASKABCD(' For the zone CFC file',' :',
     &      'change name','browse','dereference it','continue',IW,3)
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
     &      'edit name','browse','continue',IW,3)
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
  511   CALL EASKS(L,outs,' ',72,D,'cfc file',IER,6)
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
