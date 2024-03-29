C This file is part of the ESP-r system.
C Copyright Energy Systems Research Unit, University of
C Strathclyde, Glasgow, Scotland, 2001-.

C ESP-r is free software.  You can redistribute it and/or
C modify it under the terms of the GNU General Public
C License as published by the Free Software Foundation
C (version 2 orlater).

C ESP-r is distributed in the hope that it will be useful
C but WITHOUT ANY WARRANTY; without even the implied
C warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
C PURPOSE. See the GNU General Public License for more
C details.

C filelist.F contains:
C  browsefilelist: presents user with list of files recovered from
C    a call to getfileslist to select from.
C  printfileslist: reports a list of file and/or folder names
C    recovered from call to C function getfileslist.


C ******************** browsefilelist ********************
C Presents user with list of files recovered from
C a call to getfileslist.
C  act     - 'p' print packet list, 'l' line by line listing,
C            'b' browse (popup menu), '?' determine if files exist.
C  where   - where to look for the file.
C  what    - what type of file.
C  sfile   - file name returned (including the path from the 
C            current working directory).
C  snpfile - the file name returned (without path).
C  nlist   - the number of files matching criteria.
C  ier     - returned as 2 if problem writing file.

      subroutine browsefilelist(act,where,what,sfile,snpfile,nlist,ier)
#include "espriou.h"
#include "building.h"
#include "model.h"
#include "help.h"

      integer lnblnk  ! function definition

      COMMON/OUTIN/IUOUT,IUIN,IEOUT
      common/rpath/path

C Where ESP-r is installed.
      common/deflt4/dinstpath
      
      integer nboflistf        ! number of folders or file names passed
      integer listfoldertype   ! 0 if unused, 1 if folder, 2 if file
      integer lenlistfolder    ! width of each string
      character*72 listfolder  ! array of folder or file names
      common/listfold/nboflistf,listfoldertype(MFFOLD),
     &                lenlistfolder(MFFOLD),listfolder(MFFOLD)

C 'where' can be: `cfg` (looks in cfg folder), `dbs` (looks in ../dbs),
C   `zon` (looks in ../zones), `net` (looks in ../nets),
C   `ctl` (looks in ../ctl), `img` (looks in ../images),
C   `rad` (looks in ../rad), `doc` (looks in ../doc),
C   `usr` (looks in user's home folder).
C   `pat` (looks in {esp-r install folder}/training/pattern).
C   `clm` (looks in {esp-r install folder}/climate).
C   `dbm` (looks in {esp-r install folder}/databases).
C   `hva`(looks in ../hvac), `bsm` (looks in ../bsm).
C   `msc` (looks in ../msc).

C 'what' can be: `dir` list of folders, `fil` any files in the folders,
C   `cfg` configuration files, `ctl` control files,
C   `afn` air flow networks, `gnf` graphic network files,
C   `res` results files, `mfr` mass flow results
C   `geo` geometry files, `opr` operation files,
C   `con` construction files, `obs` obstruction files,
C   `vwf` viewfactor files, `tmc` optics files,
C   `shd` shading files, `cgc` casual gain control,
C   `htc` heat transfer control,
C   `gdb` generic database, `gda` ascii generic database
C   `dba` ascii database,
C   `dbb` binary database or climate file (for future use),
C   `xbm` X bitmap files, `gif` gif image files,
C   `ipv` IPV definition file, `rep` IPV report file
C   `qac` QA contents file, `zip` cflo3 zip (geometry) file, 
C   `hva` hvac files, `bsm` (looks for BASESIMP files),
C   `msc` miscel. files,
C   `jsn` .json files.

      integer ivlist
      dimension ivlist(MFFOLD)
      character where*3,what*3,sfile*72,snpfile*72,tsfile*72
      character path*72
      character fs*1,act*1
      character dinstpath*60
      integer lex
      character*72 listfolders(MFFOLD)  ! for sorted
      integer lenlistfolders(MFFOLD)    ! for sorted

C Array for browse menu.
      character*70 listfc70(MFFOLD)
      character*70 listfc70s(MFFOLD)

C relative is the portion of the path which needs to be prepended
C to the selected file to have it relative to the cfg folder.
      character action*3,outs*248,relative*60
      character subpath*84   ! combined path
      character subpathd*84  ! to use for native windows
      character odir*84      ! the directory executable was started from
      logical unixok
      integer lnodir, lnpath

#ifdef OSI
      integer nnlistf  ! for use with getfileslist
#else
      integer*8 nnlistf
#endif

      helpinsub='filelist'  ! set for subroutine
      helptopic='browse_file_list'
      call gethelptext(helpinsub,helptopic,nbhelp)

C Set folder separator (fs) to / or \ as required.
      ier=0
      call isunix(unixok)
      if(unixok)then
        fs = char(47)
      else
        fs = char(92)
      endif

C Save any file name passed to this subroutine.
      tsfile=sfile

C Make up the path (usually relative to the cfg folder of the
C model. 'odir' is the current working directory (i.e. where
C project manager was invoked). With OSX, 'odir' is the full path
C to the current working directory.
      subpath=' '; odir=' '
      call usrdir(odir)
      lnodir=lnblnk(odir); lnpath=lnblnk(path)

      if(where(1:3).eq.'cfg')then
        write(subpath,'(3a)',iostat=ios,err=1) odir(1:lnblnk(odir)),fs,
     &    path(1:lnblnk(path))
        write(relative,'(a)',iostat=ios,err=2) path(1:lnblnk(path))
      elseif(where(1:3).eq.'dbs')then

C If the initial parts of odir and path are the same then only need
C to use one of them.
        write(subpath,'(5a)',iostat=ios,err=1) odir(1:lnblnk(odir)),fs,
     &    path(1:lnblnk(path)),dbspth(1:lnblnk(dbspth)),fs
        write(relative,'(2a)',iostat=ios,err=2) path(1:lnblnk(path)),
     &    dbspth(1:lnblnk(dbspth))
      elseif(where(1:3).eq.'tmp')then
        write(subpath,'(4a)',iostat=ios,err=1) odir(1:lnblnk(odir)),fs,
     &    path(1:lnblnk(path)),tmppth(1:lnblnk(tmppth))
        write(relative,'(2a)',iostat=ios,err=2) path(1:lnblnk(path)),
     &    tmppth(1:lnblnk(tmppth))
      elseif(where(1:3).eq.'zon')then
        write(subpath,'(4a)',iostat=ios,err=1) odir(1:lnblnk(odir)),fs,
     &    path(1:lnblnk(path)),zonepth(1:lnblnk(zonepth))
        write(relative,'(2a)',iostat=ios,err=2) path(1:lnblnk(path)),
     &    zonepth(1:lnblnk(zonepth))
      elseif(where(1:3).eq.'net')then
        write(subpath,'(4a)',iostat=ios,err=1) odir(1:lnblnk(odir)),fs,
     &    path(1:lnblnk(path)),netpth(1:lnblnk(netpth))
        write(relative,'(2a)',iostat=ios,err=2) path(1:lnblnk(path)),
     &    netpth(1:lnblnk(netpth))
      elseif(where(1:3).eq.'ctl')then
        write(subpath,'(4a)',iostat=ios,err=1) odir(1:lnblnk(odir)),fs,
     &    path(1:lnblnk(path)),ctlpth(1:lnblnk(ctlpth))
        write(relative,'(2a)',iostat=ios,err=2) path(1:lnblnk(path)),
     &    ctlpth(1:lnblnk(ctlpth))
      elseif(where(1:3).eq.'msc')then
        write(subpath,'(4a)',iostat=ios,err=1) odir(1:lnblnk(odir)),fs,
     &    path(1:lnblnk(path)),mscpth(1:lnblnk(mscpth))
        write(relative,'(2a)',iostat=ios,err=2) path(1:lnblnk(path)),
     &    mscpth(1:lnblnk(mscpth))
      elseif(where(1:3).eq.'img')then
        write(subpath,'(4a)',iostat=ios,err=1) odir(1:lnblnk(odir)),fs,
     &    path(1:lnblnk(path)),imgpth(1:lnblnk(imgpth))
        write(relative,'(2a)',iostat=ios,err=2) path(1:lnblnk(path)),
     &    imgpth(1:lnblnk(imgpth))
      elseif(where(1:3).eq.'rad')then
        write(subpath,'(4a)',iostat=ios,err=1) odir(1:lnblnk(odir)),fs,
     &    path(1:lnblnk(path)),radpth(1:lnblnk(radpth))
        write(relative,'(2a)',iostat=ios,err=2) path(1:lnblnk(path)),
     &    radpth(1:lnblnk(radpth))
      elseif(where(1:3).eq.'doc')then
        write(subpath,'(4a)',iostat=ios,err=1) odir(1:lnblnk(odir)),fs,
     &    path(1:lnblnk(path)),docpth(1:lnblnk(docpth))
        write(relative,'(2a)',iostat=ios,err=2) path(1:lnblnk(path)),
     &    docpth(1:lnblnk(docpth))
      elseif(where(1:3).eq.'usr')then
        write(subpath,'(2a)',iostat=ios,err=1) upath(1:lnblnk(upath)),
     &    fs
        write(relative,'(2a)',iostat=ios,err=2) upath(1:lnblnk(upath)),
     &    fs
      elseif(where(1:3).eq.'pat')then
        write(subpath,'(6a)',iostat=ios,err=1)
     &    dinstpath(1:lnblnk(dinstpath)),fs,'training',fs,'pattern',fs
        write(relative,'(6a)',iostat=ios,err=2)
     &    dinstpath(1:lnblnk(dinstpath)),fs,'training',fs,'pattern',fs
      elseif(where(1:3).eq.'clm')then
        if(unixok)then
          write(subpath,'(4a)',iostat=ios,err=1)
     &      dinstpath(1:lnblnk(dinstpath)),fs,'climate',fs
          write(relative,'(4a)',iostat=ios,err=2)
     &      dinstpath(1:lnblnk(dinstpath)),fs,'climate',fs
        else
          write(subpathd,'(4a)',iostat=ios,err=1)
     &      dinstpath(1:lnblnk(dinstpath)),fs,'climate',fs
          call backslashit(subpathd,subpath)
          write(relative,'(a)',iostat=ios,err=2)
     &      subpath(1:lnblnk(subpath))
        endif
      elseif(where(1:3).eq.'dbm')then

C The install path works ok as-is in Unix but needs to be
C backslashed for Windows.
        if(unixok)then
          write(subpath,'(4a)',iostat=ios,err=1)
     &      dinstpath(1:lnblnk(dinstpath)),fs,'databases',fs
        else
          write(subpathd,'(4a)',iostat=ios,err=1)
     &      dinstpath(1:lnblnk(dinstpath)),fs,'databases',fs
          call backslashit(subpathd,subpath)
        endif
        write(relative,'(a)',iostat=ios,err=2)
     &    subpath(1:lnblnk(subpath))
      endif

#ifdef MINGW
C In DOS no need to prepend odir (just use the relative path defined above).
      write(subpath,'(a)',iostat=ios,err=2)
     &  relative(1:lnblnk(relative))
#endif
#ifdef OSX
C In OSX no need to prepend odir (just use the relative path defined above).
      write(subpath,'(a)',iostat=ios,err=2)
     &  relative(1:lnblnk(relative))
#endif

      if(what(1:3).eq.'dir')then
        action='dir'
        write(outs,*) 'folders in ',subpath
      elseif(what(1:3).eq.'fil')then
        action='fil'
        write(outs,*) 'files in ',subpath
      elseif(what(1:3).eq.'cfg')then
        action='cfg'
        write(outs,*) 'models in ',subpath
      elseif(what(1:3).eq.'ctl')then
        action='ctl'
        write(outs,*) 'control files in ',subpath
      elseif(what(1:3).eq.'msc')then
        action='msc'
        write(outs,*) 'miscel. files in ',subpath
      elseif(what(1:3).eq.'hva')then
        action='hva'
        write(outs,*) 'hvac models in ',subpath
      elseif(what(1:3).eq.'afn')then
        action='afn'
        write(outs,*) 'network files in ',subpath
      elseif(what(1:3).eq.'gnf')then
        action='gnf'
        write(outs,*) 'network files in ',subpath
      elseif(what(1:3).eq.'res')then
        action='res'
        write(outs,*) 'results files in ',subpath
      elseif(what(1:3).eq.'mfr')then
        action='mfr'
        write(outs,*) 'flow results files in ',subpath
      elseif(what(1:3).eq.'geo')then
        action='geo'
        write(outs,*) 'geometry files in ',subpath
      elseif(what(1:3).eq.'opr')then
        action='opr'
        write(outs,*) 'operation files in ',subpath
      elseif(what(1:3).eq.'con')then
        action='con'
        write(outs,*) 'construction files in ',subpath
      elseif(what(1:3).eq.'obs')then
        action='obs'
        write(outs,*) 'obstruction files in ',subpath
      elseif(what(1:3).eq.'vwf')then
        action='vwf'
        write(outs,*) 'viewfactor files in ',subpath
      elseif(what(1:3).eq.'tmc')then
        action='tmc'
        write(outs,*) 'zone optics files in ',subpath
      elseif(what(1:3).eq.'shd')then
        action='shd'
        write(outs,*) 'zone shading files in ',subpath
      elseif(what(1:3).eq.'cgc')then
        action='cgc'
        write(outs,*) 'zone casual control files in ',subpath
      elseif(what(1:3).eq.'bsm')then
        action='bsm'
        write(outs,*) 'zone BASESIMP files in ',subpath
      elseif(what(1:3).eq.'htc')then
        action='htc'
        write(outs,*) 'zone convection regime files in ',subpath
      elseif(what(1:3).eq.'gdb')then
        action='gdb'
        write(outs,*) 'generic database files in ',subpath
      elseif(what(1:3).eq.'gda')then
        action='gda'
        write(outs,*) 'ascii generic database files in ',subpath
      elseif(what(1:3).eq.'dba')then
        action='dba'
        write(outs,*) 'ascii database files in ',subpath
      elseif(what(1:3).eq.'dbb')then
        action='dbb'
        write(outs,*) 'binary database files in ',subpath
      elseif(what(1:3).eq.'xbm')then
        action='xbm'
        write(outs,*) 'X bitmap files in ',subpath
      elseif(what(1:3).eq.'gif')then
        action='gif'
        write(outs,*) 'gif image files in ',subpath
      elseif(what(1:3).eq.'ipv')then
        action='ipv'
        write(outs,*) 'IPV definition files in ',subpath
      elseif(what(1:3).eq.'rep')then
        action='rep'
        write(outs,*) 'IPV report files in ',subpath
      elseif(what(1:3).eq.'qac')then
        action='qac'
        write(outs,*) 'QA contents files in ',subpath
      elseif(what(1:3).eq.'zip')then
        action='zip'
        write(outs,*) 'ZIP/cflo3 files in ',subpath
      elseif(what(1:3).eq.'jsn')then
        action='jsn'
        write(outs,*) '.json files in ',subpath
      endif

      call edisp(iuout,' ')
      call edisp248(iuout,outs,90) 

C Clear the list of files and recover a list of files matching
C the current criteria.
      call getfileslist(subpath,action,nnlistf)
      nlistf=int(nnlistf)

C Depending on `act`, print or browse or just return with number of files.
      if(act.eq.'p')then
        call printfileslist(outs,'p')
        nlist=nboflistf
        return
      elseif(act.eq.'?')then
        nlist=nboflistf
        return
      elseif(act.eq.'l')then
        call printfileslist(outs,'l')
        nlist=nboflistf
        return
      elseif(act.eq.'b')then

C Try to use information gathered from file scan.
        if(nboflistf.gt.0)then
          ix=1
          maxw=1
          do 76 ij=1,nboflistf
            listfc70(ij)=' '
            lex=lenlistfolder(ij)
            if(lex.gt.70) lex=70
            if(lex.le.1)then
              write(listfc70(ij),'(a)') '  '
            else
              write(listfc70(ij),'(a)') listfolder(ij)(1:lex)
            endif
            if(lex.gt.maxw) maxw=lex  ! ensure widest string can be displayed
            if(maxw.lt.15) maxw=15    ! ensure title can be read
  76      continue

C Sort both file name lists in ascending order.
          do ij=1,nboflistf
            listfc70s(ij)=listfc70(ij)
            listfolders(ij)=listfolder(ij)
          enddo
          call SORTSTRA(listfc70s,nboflistf)
          call SORTSTRA(listfolders,nboflistf)
          do ij=1,nboflistf
            lenlistfolders(ij)=lnblnk(listfolders(ij))
          enddo

C Present menu of sorted files and then use listfolders
C (which is sorted the same way) for the returned file name.
          CALL EPICKS(IX,ivlist,outs,' ',
     &      maxw,nboflistf,listfc70s,'Available files',IER,nbhelp)
          if(ix.eq.1)then
            write(sfile,'(3a)')relative(1:lnblnk(relative)),fs,
     &        listfolders(ivlist(ix))(1:lenlistfolders(ivlist(ix)))
            write(snpfile,'(a)')
     &        listfolders(ivlist(ix))(1:lenlistfolders(ivlist(ix)))
          else

C Nothing selected, revert to original name.
            sfile=tsfile
            snpfile=tsfile
          endif
        else
          write(outs,'(2a)') relative(1:lnblnk(relative)),'. Reverting.'
          call edisp(iuout,outs)
          call usrmsg('There are no files matching the criteria in',
     &      relative,'W')
          sfile=tsfile
          snpfile=tsfile
C          write(sfile,'(a)')relative(1:lnblnk(relative))
          ier=2
        endif
      endif
      return

C Errors when creating string buffers.
   1  if(IOS.eq.2)then
        write(outs,*)'browsefilelist: no permission to make subpath: ',
     &    subpath
        call edisp(iuout,outs)
      else
        write(outs,*)'browsefilelist: error creating subpath: ',subpath
        call edisp(iuout,outs)
      endif
      ier=2
      return

   2  if(IOS.eq.2)then
        write(outs,*)
     &    'browsefilelist: no permission to create relative path: ',
     &    relative
        call edisp(iuout,outs)
      else
        write(outs,*) 'browsefilelist: error creating relative path: ',
     &    relative
        call edisp(iuout,outs)
      endif
      ier=2
      return

C Example of manual use of getfileslist, printfileslist.
C The first lists out geometry file, the 2nd block lists operations files.
C      subpath=' '
C      write(subpath,'(4a)') odir(1:lnblnk(odir)),fs,
C     &  path(1:lnblnk(path)),zonepth(1:lnblnk(zonepth))
C      write(outs,*) 'geometry files in ',subpath
C      action='geo'
C      call getfileslist(subpath,action,nlistf)
C      call printfileslist(outs,'p')
C      write(outs,*) 'operation files in ',subpath
C      action='opr'
C      call getfileslist(subpath,action,nlistf)
C      call printfileslist(outs,'p')
      end  ! of browsefilelist

C ******************** browsefilelistmulti ********************
C Presents user with list of files recovered from
C a call to getfileslist and returns multiple files.
C  act     - 'p' print packet list, 'l' line by line listing,
C            'b' browse (popup menu), '?' determine if files exist.
C  where   - where to look for the file.
C  what    - what type of file.
C  sfile   - file name returned (including the path from the 
C            current working directory).
C  snpfile - the file name returned (without path).
C  nlist   - the number of files matching criteria.
C  ier     - returned as 2 if problem writing file.

      subroutine browsefilelistmulti(act,where,what,sfilelist,
     &  snpfilelist,nlist,ier)
#include "espriou.h"
#include "building.h"
#include "model.h"
#include "help.h"

      integer lnblnk  ! function definition

C Parameters.
      character act*1
      character where*3,what*3
      character*72 sfilelist(MFFOLD),snpfilelist(MFFOLD)
      integer nlist
      integer ier

      COMMON/OUTIN/IUOUT,IUIN,IEOUT
      common/rpath/path

C Where ESP-r is installed.
      common/deflt4/dinstpath
      
      integer nboflistf  ! how many folders or file names passed
      integer listfoldertype   ! zero if unused one if folder two if file
      integer lenlistfolder    ! width of each string
      character*72 listfolder  ! array of folder or file names
      common/listfold/nboflistf,listfoldertype(MFFOLD),
     &                lenlistfolder(MFFOLD),listfolder(MFFOLD)

C where can be: `cfg` (looks in cfg folder), `dbs` (looks in ../dbs),
C   `zon` (looks in ../zones), `net` (looks in ../nets),
C   `ctl` (looks in ../ctl), `img` (looks in ../images),
C   `rad` (looks in ../rad), `doc` (looks in ../doc),
C   `usr` (looks in users home folder).
C   `pat` (looks in {where esp-r installed}/training/pattern).
C   `clm` (looks in {where esp-r installed}/climate).
C   `dbm` (looks in {where esp-r installed}/databases).
C   `hva`(looks in ../hvac), `bsm` (looks in ../bsm).

C what can be: `dir` list of folders, `fil` any files in the folders,
C   `cfg` configuration files, `ctl` control files,
C   `afn` air flow networks, `gnf` graphic network files,
C   `res` results files, `mfr` mass flow results
C   `geo` geometry files, `opr` operation files,
C   `con` construction files, `obs` obstruction files,
C   `vwf` viewfactor files, `tmc` optics files,
C   `shd` shading files, `cgc` casual gain control,
C   `htc` heat transfer control,
C   `gdb` generic database, `gda` ascii generic database
C   `dba` ascii database,
C   `dbb` binary database or climate file (for future use),
C   `xbm` X bitmap files, `gif` gif image files,
C   `ipv` IPV definition file, `rep` IPV report file
C   `qac` QA contents file, `zip` cflo3 zip (geometry) file, 
C   `hva` hvac files, `bsm` (looks for BASESIMP files),
C   `jsn` .json files.
      integer ivlist
      dimension ivlist(MFFOLD)
      character sfile*72,snpfile*72,tsfile*72
      character path*72
      character fs*1
      character dinstpath*60
      integer lex
      character*72 listfolders(MFFOLD)  ! for sorted
      integer lenlistfolders(MFFOLD)    ! for sorted

C Array for browse menu.
      character*70 listfc70(MFFOLD)
      character*70 listfc70s(MFFOLD)

C 'relative' is the portion of the path which needs to be prepended
C to the selected file to be relative to the model cfg folder.
      character action*3,outs*248,relative*60,msg*42
      character subpath*84   ! combined path
      character subpathd*84  ! to use for native windows
      character odir*84      ! the folder in which prj was invoked
      logical unixok
      integer lnodir, lnpath

#ifdef OSI
      integer nnlistf  ! for use with getfileslist
#else
      integer*8 nnlistf
#endif

      helpinsub='filelist'
      helptopic='browse_file_list_mul'
      call gethelptext(helpinsub,helptopic,nbhelp)

C Set folder separator (fs) to / or \ as required.
      ier=0
      call isunix(unixok)
      if(unixok)then
        fs = char(47)
      else
        fs = char(92)
      endif

C Clear arrays.
      nlist=0
      do loop=1,MFFOLD
        sfilelist(loop)='  '
        snpfilelist(loop)='  '
      enddo
      
C Save file name passed to this subroutine.
C      tsfile=sfile

C Make up the path (usually relative to the cfg folder of the
C model. odir is the current working directory (which is where the
C project manager was started from). On OSX odir is the full path
C to the current working directory.
      subpath=' '; odir=' '
      call usrdir(odir)
      lnodir=lnblnk(odir); lnpath=lnblnk(path)

      if(where(1:3).eq.'cfg')then
        write(subpath,'(3a)',iostat=ios,err=1) odir(1:lnblnk(odir)),fs,
     &    path(1:lnblnk(path))
        write(relative,'(a)',iostat=ios,err=2) path(1:lnblnk(path))
      elseif(where(1:3).eq.'dbs')then

C If the initial parts of odir and path are the same then only need
C to use one of them.
        write(subpath,'(5a)',iostat=ios,err=1) odir(1:lnblnk(odir)),fs,
     &    path(1:lnblnk(path)),dbspth(1:lnblnk(dbspth)),fs
        write(relative,'(2a)',iostat=ios,err=2) path(1:lnblnk(path)),
     &    dbspth(1:lnblnk(dbspth))
      elseif(where(1:3).eq.'tmp')then
        write(subpath,'(4a)',iostat=ios,err=1) odir(1:lnblnk(odir)),fs,
     &    path(1:lnblnk(path)),tmppth(1:lnblnk(tmppth))
        write(relative,'(2a)',iostat=ios,err=2) path(1:lnblnk(path)),
     &    tmppth(1:lnblnk(tmppth))
      elseif(where(1:3).eq.'zon')then
        write(subpath,'(4a)',iostat=ios,err=1) odir(1:lnblnk(odir)),fs,
     &    path(1:lnblnk(path)),zonepth(1:lnblnk(zonepth))
        write(relative,'(2a)',iostat=ios,err=2) path(1:lnblnk(path)),
     &    zonepth(1:lnblnk(zonepth))
      elseif(where(1:3).eq.'net')then
        write(subpath,'(4a)',iostat=ios,err=1) odir(1:lnblnk(odir)),fs,
     &    path(1:lnblnk(path)),netpth(1:lnblnk(netpth))
        write(relative,'(2a)',iostat=ios,err=2) path(1:lnblnk(path)),
     &    netpth(1:lnblnk(netpth))
      elseif(where(1:3).eq.'ctl')then
        write(subpath,'(4a)',iostat=ios,err=1) odir(1:lnblnk(odir)),fs,
     &    path(1:lnblnk(path)),ctlpth(1:lnblnk(ctlpth))
        write(relative,'(2a)',iostat=ios,err=2) path(1:lnblnk(path)),
     &    ctlpth(1:lnblnk(ctlpth))
      elseif(where(1:3).eq.'msc')then
        write(subpath,'(4a)',iostat=ios,err=1) odir(1:lnblnk(odir)),fs,
     &    path(1:lnblnk(path)),mscpth(1:lnblnk(mscpth))
        write(relative,'(2a)',iostat=ios,err=2) path(1:lnblnk(path)),
     &    mscpth(1:lnblnk(mscpth))
      elseif(where(1:3).eq.'img')then
        write(subpath,'(4a)',iostat=ios,err=1) odir(1:lnblnk(odir)),fs,
     &    path(1:lnblnk(path)),imgpth(1:lnblnk(imgpth))
        write(relative,'(2a)',iostat=ios,err=2) path(1:lnblnk(path)),
     &    imgpth(1:lnblnk(imgpth))
      elseif(where(1:3).eq.'rad')then
        write(subpath,'(4a)',iostat=ios,err=1) odir(1:lnblnk(odir)),fs,
     &    path(1:lnblnk(path)),radpth(1:lnblnk(radpth))
        write(relative,'(2a)',iostat=ios,err=2) path(1:lnblnk(path)),
     &    radpth(1:lnblnk(radpth))
      elseif(where(1:3).eq.'doc')then
        write(subpath,'(4a)',iostat=ios,err=1) odir(1:lnblnk(odir)),fs,
     &    path(1:lnblnk(path)),docpth(1:lnblnk(docpth))
        write(relative,'(2a)',iostat=ios,err=2) path(1:lnblnk(path)),
     &    docpth(1:lnblnk(docpth))
      elseif(where(1:3).eq.'usr')then
        write(subpath,'(2a)',iostat=ios,err=1) upath(1:lnblnk(upath)),
     &    fs
        write(relative,'(2a)',iostat=ios,err=2) upath(1:lnblnk(upath)),
     &    fs
      elseif(where(1:3).eq.'pat')then
        write(subpath,'(6a)',iostat=ios,err=1)
     &    dinstpath(1:lnblnk(dinstpath)),fs,'training',fs,'pattern',fs
        write(relative,'(6a)',iostat=ios,err=2)
     &    dinstpath(1:lnblnk(dinstpath)),fs,'training',fs,'pattern',fs
      elseif(where(1:3).eq.'clm')then
        if(unixok)then
          write(subpath,'(4a)',iostat=ios,err=1)
     &      dinstpath(1:lnblnk(dinstpath)),fs,'climate',fs
          write(relative,'(4a)',iostat=ios,err=2)
     &      dinstpath(1:lnblnk(dinstpath)),fs,'climate',fs
        else
          write(subpathd,'(4a)',iostat=ios,err=1)
     &      dinstpath(1:lnblnk(dinstpath)),fs,'climate',fs
          call backslashit(subpathd,subpath)
          write(relative,'(a)',iostat=ios,err=2)
     &      subpath(1:lnblnk(subpath))
        endif
      elseif(where(1:3).eq.'dbm')then

C The install path works ok as-is in Unix variants, however it
C needs to be backslashed to properly work in Native Windows.
        if(unixok)then
          write(subpath,'(4a)',iostat=ios,err=1)
     &      dinstpath(1:lnblnk(dinstpath)),fs,'databases',fs
        else
          write(subpathd,'(4a)',iostat=ios,err=1)
     &      dinstpath(1:lnblnk(dinstpath)),fs,'databases',fs
          call backslashit(subpathd,subpath)
        endif
        write(relative,'(a)',iostat=ios,err=2)
     &    subpath(1:lnblnk(subpath))
      endif

#ifdef MINGW
C In DOS no need to prepend odir (just use the relative path defined above).
      write(subpath,'(a)',iostat=ios,err=2)
     &  relative(1:lnblnk(relative))
#endif
#ifdef OSX
C In OSX no need to prepend odir (just use the relative path defined above).
      write(subpath,'(a)',iostat=ios,err=2)
     &  relative(1:lnblnk(relative))
#endif

      if(what(1:3).eq.'dir')then
        action='dir'
        write(outs,*) 'folders in ',subpath
      elseif(what(1:3).eq.'fil')then
        action='fil'
        write(outs,*) 'files in ',subpath
      elseif(what(1:3).eq.'cfg')then
        action='cfg'
        write(outs,*) 'models in ',subpath
      elseif(what(1:3).eq.'ctl')then
        action='ctl'
        write(outs,*) 'control files in ',subpath
      elseif(what(1:3).eq.'msc')then
        action='msc'
        write(outs,*) 'miscel. files in ',subpath
      elseif(what(1:3).eq.'hva')then
        action='hva'
        write(outs,*) 'hvac models in ',subpath
      elseif(what(1:3).eq.'afn')then
        action='afn'
        write(outs,*) 'network files in ',subpath
      elseif(what(1:3).eq.'gnf')then
        action='gnf'
        write(outs,*) 'network files in ',subpath
      elseif(what(1:3).eq.'res')then
        action='res'
        write(outs,*) 'results files in ',subpath
      elseif(what(1:3).eq.'mfr')then
        action='mfr'
        write(outs,*) 'flow results files in ',subpath
      elseif(what(1:3).eq.'geo')then
        action='geo'
        write(outs,*) 'geometry files in ',subpath
      elseif(what(1:3).eq.'opr')then
        action='opr'
        write(outs,*) 'operation files in ',subpath
      elseif(what(1:3).eq.'con')then
        action='con'
        write(outs,*) 'construction files in ',subpath
      elseif(what(1:3).eq.'obs')then
        action='obs'
        write(outs,*) 'obstruction files in ',subpath
      elseif(what(1:3).eq.'vwf')then
        action='vwf'
        write(outs,*) 'viewfactor files in ',subpath
      elseif(what(1:3).eq.'tmc')then
        action='tmc'
        write(outs,*) 'zone optics files in ',subpath
      elseif(what(1:3).eq.'shd')then
        action='shd'
        write(outs,*) 'zone shading files in ',subpath
      elseif(what(1:3).eq.'cgc')then
        action='cgc'
        write(outs,*) 'zone casual control files in ',subpath
      elseif(what(1:3).eq.'bsm')then
        action='bsm'
        write(outs,*) 'zone BASESIMP files in ',subpath
      elseif(what(1:3).eq.'htc')then
        action='htc'
        write(outs,*) 'zone convection regime files in ',subpath
      elseif(what(1:3).eq.'gdb')then
        action='gdb'
        write(outs,*) 'generic database files in ',subpath
      elseif(what(1:3).eq.'gda')then
        action='gda'
        write(outs,*) 'ascii generic database files in ',subpath
      elseif(what(1:3).eq.'dba')then
        action='dba'
        write(outs,*) 'ascii database files in ',subpath
      elseif(what(1:3).eq.'dbb')then
        action='dbb'
        write(outs,*) 'binary database files in ',subpath
      elseif(what(1:3).eq.'xbm')then
        action='xbm'
        write(outs,*) 'X bitmap files in ',subpath
      elseif(what(1:3).eq.'gif')then
        action='gif'
        write(outs,*) 'gif image files in ',subpath
      elseif(what(1:3).eq.'ipv')then
        action='ipv'
        write(outs,*) 'IPV definition files in ',subpath
      elseif(what(1:3).eq.'rep')then
        action='rep'
        write(outs,*) 'IPV report files in ',subpath
      elseif(what(1:3).eq.'qac')then
        action='qac'
        write(outs,*) 'QA contents files in ',subpath
      elseif(what(1:3).eq.'zip')then
        action='zip'
        write(outs,*) 'ZIP/cflo3 files in ',subpath
      elseif(what(1:3).eq.'jsn')then
        action='jsn'
        write(outs,*) '.json files in ',subpath
      endif

      call edisp(iuout,' ')
      call edisp248(iuout,outs,90) 

C Clear the list of files and recover a list of files matching
C the current criteria.
      call getfileslist(subpath,action,nnlistf)
      nlistf=int(nnlistf)
      
C Depending on `act`, print or browse or just return with number of files.
      if(act.eq.'p')then
        call printfileslist(outs,'p')
        nlist=nboflistf
        return
      elseif(act.eq.'?')then
        nlist=nboflistf
        return
      elseif(act.eq.'l')then
        call printfileslist(outs,'l')
        nlist=nboflistf
        return
      elseif(act.eq.'b')then

C Try to use information gathered from file scan.
        if(nboflistf.gt.0)then
          ix=1
          maxw=1
          do 76 ij=1,nboflistf
            listfc70(ij)=' '
            lex=lenlistfolder(ij)

            if(lex.gt.70) lex=70
            if(lex.le.1)then
              write(listfc70(ij),'(a)') '  '
            else
              write(listfc70(ij),'(a)') listfolder(ij)(1:lex)
            endif
            if(lex.gt.maxw) maxw=lex  ! ensure widest string can be displayed
            if(maxw.lt.15) maxw=15    ! ensure title can be read
  76      continue

C Sort both file name lists in ascending order.
          do ij=1,nboflistf
            listfc70s(ij)=listfc70(ij)
            listfolders(ij)=listfolder(ij)
          enddo
          call SORTSTRA(listfc70s,nboflistf)
          call SORTSTRA(listfolders,nboflistf)
          do ij=1,nboflistf
            lenlistfolders(ij)=lnblnk(listfolders(ij))
          enddo

C Present menu of sorted files and then use listfolders
C (which is sorted the same way) to select up to nboflistf.
          write(msg,'(a,i2,a)') 'Select up to ',nboflistf,' files.'
          call edisp(iuout,msg)
          IX=nboflistf
          CALL EPICKS(IX,ivlist,outs,' ',
     &      maxw,nboflistf,listfc70s,'Available json files',IER,nbhelp)
          if(ix.ge.1)then
            do loop=1,IX
             write(sfilelist(loop),'(3a)')
     &       relative(1:lnblnk(relative)),fs,
     &       listfolders(ivlist(loop))(1:lenlistfolders(ivlist(loop)))
             write(snpfilelist(loop),'(a)')
     &        listfolders(ivlist(loop))(1:lenlistfolders(ivlist(loop)))

           enddo
           nlist=IX  ! pass back the number of files selected
          endif
        else
          call usrmsg('There are no files matching the criteria in',
     &      relative,'W')
          ier=2
        endif
      endif
      return

C Errors when creating string buffers.
   1  if(IOS.eq.2)then
        write(outs,*)'browsefilelist: no permission to make subpath: ',
     &    subpath
        call edisp(iuout,outs)
      else
        write(outs,*)'browsefilelist: error creating subpath: ',subpath
        call edisp(iuout,outs)
      endif
      ier=2
      return

   2  if(IOS.eq.2)then
        write(outs,*)
     &    'browsefilelist: no permission to create relative path: ',
     &    relative
        call edisp(iuout,outs)
      else
        write(outs,*) 'browsefilelist: error creating relative path: ',
     &    relative
        call edisp(iuout,outs)
      endif
      ier=2
      return

C Example of manual use of getfileslist, printfileslist.
C The first lists out geometry file, the 2nd block lists operations files.
C      subpath=' '
C      write(subpath,'(4a)') odir(1:lnblnk(odir)),fs,
C     &  path(1:lnblnk(path)),zonepth(1:lnblnk(zonepth))
C      write(outs,*) 'geometry files in ',subpath
C      action='geo'
C      call getfileslist(subpath,action,nlistf)
C      call printfileslist(outs,'p')
C      write(outs,*) 'operation files in ',subpath
C      action='opr'
C      call getfileslist(subpath,action,nlistf)
C      call printfileslist(outs,'p')
      end  ! of browsefilelistmulti


C ******************** printfileslist ********************
C Reports a list of file and/or folder names
C recovered from call to C function getfileslist.

      subroutine printfileslist(title,act)
#include "espriou.h"
      
      integer nboflistf  ! how many folders or file names passed
      integer listfoldertype   ! zero if unused one if folder two if file
      integer lenlistfolder    ! width of each string
      character*72 listfolder  ! array of folder or file names
      common/listfold/nboflistf,listfoldertype(MFFOLD),
     &                lenlistfolder(MFFOLD),listfolder(MFFOLD)

C Passed parameters.
      character*(*) title   ! title of the printed list
      character act*1       ! action l is list p is packed lines

      common/OUTIN/IUOUT,IUIN,IEOUT
      character t96*96
      character*72 printfile
      integer lntitle

      character*72 listfolders(MFFOLD)  ! for sorted
      integer lenlistfolders(MFFOLD)    ! for sorted

      if(nboflistf.eq.0)then
        return
      else

C Sort file name list in ascending order.
        do ij=1,nboflistf
          listfolders(ij)=listfolder(ij)
        enddo
        call SORTSTRA(listfolders,nboflistf)
        do ij=1,nboflistf
          lenlistfolders(ij)=lnblnk(listfolders(ij))
        enddo

        call edisp(iuout,'  ')
        lntitle=lnblnk(title)
        call edisp248(iuout,title,90)
        if(act.eq.'l')then
          do 42 i=1,nboflistf
            if(lenlistfolders(i).gt.1)then  ! ignore blank or 1 character names
              write(printfile,'(a)') 
     &          listfolders(i)(1:lenlistfolders(i))
              call edisp(iuout,printfile)
            endif
  42      continue
          return
        elseif(act.eq.'p')then
          itrunc=1
          ipos=1
          do while (itrunc.ne.0)
            call asflist(ipos,nboflistf,listfolders,MFFOLD,'S',t96,
     &        louts,itrunc)
            call edisp(iuout,t96)
            ipos=itrunc+1
          end do
          return
        endif
      endif
      end

