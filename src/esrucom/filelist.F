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

C filelist.F contains:
C  browsefilelist: presents user with list of files recovered from
C    a call to getfilelist to select from. 
C  clearfilelist: clears a string array (char*72 listf(50))
C    used before call to getfilelist.
C  copyfilelist: takes string array returned from getfilelist and copies
C    it into a string array suitable for use on fortran side.
C  printfilelist: reports a list of file and/or folder names
C    recovered from call to C function getfilelist.

C ********* browsefilelist
C browsefilelist presents user with list of files recovered from
C a call to getfilelist to select from. 
C Parameter act (chr*1) 'p' print packet list, 'l' line by line
C   list, 'b' browse (popup menu), '?' find if any files exist.
C Parameter `where` (char*3) says where to look for the file,
C parameter `what` (char*3) says what type of file,
C parameter `sfile` is the file name returned (including the
C   path from current working directory to file).
C parameter `snpfile` is the file name returned (without path).
C parameter nlist is the number of files matching criteria.
C parameter ier returned as 2 if problem writing file name path.

      subroutine browsefilelist(act,where,what,sfile,snpfile,nlist,ier)
#include "espriou.h"
C espriou.h for parameter MFFOLD

      common/pophelp/h(60)
      common/uhome/upath
      common/rpath/path
      common/paths/zonepth,netpth,ctlpth,imgpth,radpth,docpth,tmppth,
     &             dbspth

C Where ESP-r was installed (as recorded when it was compiled).
      common/deflt4/dinstpath

C where can be: `cfg` (looks in cfg folder), `dbs` (looks in ../dbs),
C   `zon` (looks in ../zones), `net` (looks in ../nets),
C   `ctl` (looks in ../ctl), `img` (looks in ../images),
C   `rad` (looks in ../rad), `doc` (looks in ../doc),
C   `tmp` (looks in ../temp), `usr` (looks in users home folder).
C   `pat` (looks in {where esp-r installed}/training/pattern).
C   `clm` (looks in {where esp-r installed}/climate).
C   `dbm` (looks in {where esp-r installed}/databases).

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
C   `qac` QA contents file, `zip` cflo3 zip (geometry) file. 

      dimension nwlistf(MFFOLD),ivlist(MFFOLD)
      character where*3,what*3,sfile*72,snpfile*72,tsfile*72,h*72
      character*72 upath,path,odir
      character*72 listf(MFFOLD),listfc(MFFOLD)
      character zonepth*24,netpth*24,ctlpth*24,imgpth*24,radpth*24
      character docpth*24,tmppth*24,dbspth*24,fs*1,act*1
      character dinstpath*48

C Array for browse menu.
      character*70 listfc70(MFFOLD)

C relative is the portion of the path which needs to be prepended
C to the selected file to have it relative to the cfg folder.
      character subpath*72,action*3,outs*124,relative*60
      logical unixok

C Set folder separator (fs) to \ or / as required.
      ier=0
      call isunix(unixok)
      if(unixok)then
        fs = char(47)
      else
        fs = char(92)
      endif

C Save back any file name passed into the subroutine.
      tsfile=sfile

C Make up the path (usually relative to the cfg folder of the
C model. odir is the current working directory (which is where the
C project manager was started from). 
      subpath=' '
      call usrdir(odir)
      if(where(1:3).eq.'cfg')then
        write(subpath,'(3a)',iostat=ios,err=1) odir(1:lnblnk(odir)),fs,
     &    path(1:lnblnk(path))
        write(relative,'(a)',iostat=ios,err=2) path(1:lnblnk(path))
      elseif(where(1:3).eq.'dbs')then
        write(subpath,'(4a)',iostat=ios,err=1) odir(1:lnblnk(odir)),fs,
     &    path(1:lnblnk(path)),dbspth(1:lnblnk(dbspth))
        write(relative,'(2a)',iostat=ios,err=2) path(1:lnblnk(path)),
     &    dbspth(1:lnblnk(dbspth))
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
      elseif(where(1:3).eq.'tmp')then
        write(subpath,'(4a)',iostat=ios,err=1) odir(1:lnblnk(odir)),fs,
     &    path(1:lnblnk(path)),tmppth(1:lnblnk(tmppth))
        write(relative,'(2a)',iostat=ios,err=2) path(1:lnblnk(path)),
     &    tmppth(1:lnblnk(tmppth))
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
        write(subpath,'(4a)',iostat=ios,err=1)
     &    dinstpath(1:lnblnk(dinstpath)),fs,'climate',fs
        write(relative,'(4a)',iostat=ios,err=2)
     &    dinstpath(1:lnblnk(dinstpath)),fs,'climate',fs
      elseif(where(1:3).eq.'dbm')then
        write(subpath,'(4a)',iostat=ios,err=1)
     &    dinstpath(1:lnblnk(dinstpath)),fs,'databases',fs
        write(relative,'(4a)',iostat=ios,err=2)
     &    dinstpath(1:lnblnk(dinstpath)),fs,'databases',fs
      endif

#ifdef OSX
C In OSX if path begins with /usr/esru then no need to prepend odir
C (just use the relative path defined above).
      if(path(1:2).eq.'./')then
        continue
      elseif(path(1:9).eq.'/usr/esru')then
        subpath=' '
        if(where(1:3).eq.'usr')then
          write(subpath,'(2a)',iostat=ios,err=1)
     &      upath(1:lnblnk(upath)),fs
        else
          write(subpath,'(a)',iostat=ios,err=2)
     &      relative(1:lnblnk(relative))
        endif
      else
        continue
      endif
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
      endif

C Clear the list of files and recover a list of files matching
C the current criteria. Copy this list into listfc for use by
C fortran code.
      call clearfilelist(listf,MFFOLD)
      call getfilelist(subpath,action,listf,nwlistf,nlistf)
      call copyfilelist(listf,nwlistf,listfc,MFFOLD,maxw)

C Depending on `act`, print or browse or just return with number of files.
      if(act.eq.'p')then
        call printfilelist(outs,'p',listfc,MFFOLD,nlistf)
        nlist=nlistf
        return
      elseif(act.eq.'?')then
        nlist=nlistf
        return
      elseif(act.eq.'l')then
        call printfilelist(outs,'l',listfc,MFFOLD,nlistf)
        nlist=nlistf
        return
      elseif(act.eq.'b')then

C Try to use information gathered from file scan.
        if(nlistf.gt.0)then
          h(1)='The following files match the search criteria.'
          h(2)='Select one to use or, if none selected, it will'
          h(3)='revert to prior value. '
          ix=1
          do 76 ij=1,nlistf
            listfc70(ij)=' '
            write(listfc70(ij),'(a)') listfc(ij)(1:nwlistf(ij))
  76      continue
          CALL EPICKS(IX,ivlist,outs,' ',
     &      maxw,nlistf,listfc70,'Available files',IER,3)
          if(ix.eq.1)then
            write(sfile,'(3a)')relative(1:lnblnk(relative)),fs,
     &        listfc(ivlist(ix))(1:nwlistf(ivlist(ix)))
            write(6,*) sfile
            write(snpfile,'(a)') 
     &        listfc(ivlist(ix))(1:nwlistf(ivlist(ix)))
            write(6,*) snpfile
          else

C Nothing selected, revert to original name. << expand logic here >>
            sfile=tsfile
            snpfile=tsfile
          endif
        else
          write(outs,'(2a)') relative(1:lnblnk(relative)),'. Reverting.'
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
   1  write(6,*) 'browsefilelist: error creating subpath: ',subpath
      ier=2
      return

   2  write(6,*) 'browsefilelist: error creating relative path: ',
     &  relative
      ier=2
      return

C Example of manual use of clearfilelist, getfilelist, copyfilelist, printfilelist.
C The first lists out geometry file, the 2nd block lists operations files.
C      subpath=' '
C      write(subpath,'(4a)') odir(1:lnblnk(odir)),fs,
C     &  path(1:lnblnk(path)),zonepth(1:lnblnk(zonepth))
C      write(outs,*) 'geometry files in ',subpath
C      call clearfilelist(listf,MFFOLD)
C      action='geo'
C      call getfilelist(subpath,action,listf,nwlistf,nlistf)
C      call copyfilelist(listf,nwlistf,listfc,MFFOLD,maxw)
C      call printfilelist(outs,'p',listfc,MFFOLD,nlistf)
C      write(outs,*) 'operation files in ',subpath
C      call clearfilelist(listf,MFFOLD)
C      action='opr'
C      call getfilelist(subpath,action,listf,nwlistf,nlistf)
C      call copyfilelist(listf,nwlistf,listfc,MFFOLD,maxw)
C      call printfilelist(outs,'p',listfc,MFFOLD,nlistf)
      end

C ********* clearfilelist
C clearfilelist clears a string array of size iasize which should
C be equivalent to MFFOLD (char*72 listf(iasize))
C used before call to getfilelist.
      subroutine clearfilelist(listf,iasize)
      character*72 listf(iasize)
      do 42 i=1,iasize
        listf(i)=' '
  42  continue
      return
      end

C ********* copyfilelist
C copyfilelist takes string array of size iasize which should
C be equivalent to MFFOLD (char*72 listf(iasize)) returned
C from call to getfilelist and copies it into another string
C array (char*72 listfc(iasize)). nwlistf(iasize) is the length of each
C string (returned from getfilelist call).
C maxw is the maximum width of any string
C << Kludge necessary to get lnblnk to correctly determine the
C length of each string >>.
      subroutine copyfilelist(listf,nwlistf,listfc,iasize,maxw)
      character*72 listf(iasize),listfc(iasize)
      dimension nwlistf(iasize)
      maxw=0
      do 42 i=1,iasize
        listfc(i)=' '
        if(nwlistf(i).gt.0)then
          if(nwlistf(i).gt.maxw) maxw = nwlistf(i)
          write(listfc(i),'(a)') listf(i)(1:nwlistf(i))
        endif
  42  continue
      return
      end

C ********* printfilelist
C printfilelist reports a list of file and/or folder names
C recovered from call to C function getfilelist.
C listf: file/folder names - a string array (char*72 listf(iasize))
C act: (char*1) `l` as list, `p` as packed line(s).
      subroutine printfilelist(title,act,listf,iasize,nlist)
      common/OUTIN/IUOUT,IUIN
      character*(*) title
      character act*1,t96*96
      character*72 listf(iasize)

      if(nlist.eq.0)then
        return
      else
        call edisp(iuout,title)
        if(act.eq.'l')then
          do 42 i=1,nlist
C            lln=MAX0(lnblnk(listf(i)),1)
C            call edisp(iuout,listf(i)(1:lln))
            call edisp(iuout,listf(i))
  42      continue
          return
        elseif(act.eq.'p')then
          itrunc=1
          ipos=1
          do while (itrunc.ne.0)
            call asflist(ipos,nlist,listf,iasize,'S',t96,louts,itrunc)
            call edisp(iuout,t96)
            ipos=itrunc+1
          end do
          return
        endif
      endif
      end

