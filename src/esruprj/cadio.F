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

C Project Manager: file cadio.F comprising
C exportcad: controls model export facilities.
C cadin:     Drives the conversion of CAD input into data model.
C rexmpl:    Scan the exemplars file and offer user list/info on exemplars.
C wtarentry: write entry in tar names file accountin for ../
C archiveit:  Archive the current model.

C ********** exportcad
C exportcad: controls model export facilities.
      subroutine exportcad(itrc,IER)

#include "building.h"
#include "model.h"
      
      integer lnblnk  ! function definition

      COMMON/SPAD/MMOD,LIMIT,LIMTTY
      common/OUTIN/IUOUT,IUIN

      common/C21/IFCFG,cfgroot,LCFGF
      common/rpath/path
      common/appw/iappw,iappx,iappy
      common/user/browse

      common/FOPENED/CFGOK,MLDBOK,MATDBOK,CTLOK,OPTKOK
      COMMON/exporttg/xfile,tg,delim
      COMMON/exporttgi/ixopen,ixloc,ixunit
      COMMON/RAY3/MODIFY,MODLEN,MODBND

      logical OK,DOK,CFGOK,MLDBOK,MATDBOK,CTLOK,OPTKOK,concat
      logical MODIFY,MODLEN,MODBND,unixok,browse

      DIMENSION ITEMS1(10)

      character LFIL*72,path*72,LCFGF*72
      character ITEMS1*34,doit*248,outs*124,outs248*248
      character cfgroot*24,tmode*12
      character xfile*144,tg*1,delim*1,ffile*72,dvfil*72
      character ETEXT*82
      character longtfile*144,longtfiledos*144,L144*144,lltmp*144
      character brw*8   ! to signal model is being browsed
      character fs*1
      logical oke
      integer NITMS,INO1 ! max items and current menu item

C Passed parameters for cfgtogg
#ifdef OSI
      integer icfg_type  ! model cfg type
      integer icfgz      ! if non-zero then there are zones
      integer icfgn      ! if non-zero then there are networks
      integer icfgc      ! if non-zero then cfg file known
      integer icfgdfn    ! if non-zero then cdf domain exists
      integer iicfgz     ! there are zone related images
      integer iicfgn     ! there are network related images
      integer iicfgc     ! there are control related images
      integer iicfgdfn   ! there are cfd related images
#else
      integer*8 icfg_type,icfgz,icfgn,icfgc,icfgdfn,iicfgz
      integer*8 iicfgn,iicfgc,iicfgdfn
#endif

#ifdef OSI
      integer iside,isize,ifont     ! passed to viewtext
      integer ICPMOD     ! passed from tchild
#else
      integer*8 iside,isize,ifont     ! passed to viewtext
      integer*8 ICPMOD   ! passed from tchild
#endif

C For help messages
      character helpinsub*24 ! subroutine name
      character helptopic*24 ! string (unique) for topic
      integer nbhelp     ! number of help lines found

      helpinsub='exportcad'  ! set for subroutine

      if(.NOT.cfgok)then
        call usrmsg(' You must load a model before',
     &              ' you can export it!','W')
         return
      endif

C Set folder separator (fs) to \ or / as required.
      call isunix(unixok)
      if(unixok)then
        fs = char(47)
      else
        fs = char(92)
      endif
      ioout=2  ! set initial value
      lfil='  '

   33 INO1=-4
      ITEMS1(1) ='a to VIEWER (Abacus)       '
      ITEMS1(2) ='b to DXF (V12-V14)         '
      ITEMS1(3) ='c to RADIANCE              '
      ITEMS1(4) ='d to xfig (vector ploting) '
      ITEMS1(5) ='e to MicroGDS (THINGS file)'
      ITEMS1(6) ='f to EnergyPlus            '
      ITEMS1(7) ='g to VRML #2.0             '
      ITEMS1(8) ='h to ESP-r meta file       '
      ITEMS1(9)='? help                     '
      ITEMS1(10)='- exit this menu           '

C Help text for this menu.
      helptopic='cad_export_choices'
      call gethelptext(helpinsub,helptopic,nbhelp)
      NITMS=10
      CALL EMENU('Export Model',ITEMS1,NITMS,INO1)

C Prepare filter command lines with path.

      if(INO1.EQ.1.or.INO1.EQ.2.or.INO1.eq.9.or.INO1.eq.10)then
        continue
      else

C Note: this sets up initial part of LFIL but some later
C code tends to overwrite this and thus the user choice
C is lost. 
        iw=0
        if(path(1:2).ne.'./'.and.path(1:2).ne.'  ')then
          write(outs,'(A,A)') 'The current path is: ',path
          call edisp(iuout,outs)
          CALL EASKAB('The model is in a remote folder.',
     &    'Place exported file(s) in:','current path','local folder',
     &    IW,nbhelp)
        endif
        if(iw.eq.1)then
          write(LFIL,'(a)') path(1:lnblnk(path))
        else
          LFIL='./'
        endif
      endif

 11   IF(INO1.EQ.1)THEN

C Construct a 'VIEWER' format file.  First get file name taking
C into account whether the user is working in the model folder
C or remotely.
        call tstamp('>','PRJ: export to viewer')
        iw=0
        if(path(1:2).ne.'./'.and.path(1:2).ne.'  ')then
          write(outs,'(A,A)') 'The current path is: ',path
          call edisp(iuout,outs)
          CALL EASKAB('The model is in a remote folder.',
     &    'Place exported file in:','current path','local folder',
     &    IW,nbhelp)
        endif
        if(iw.eq.1)then
          write(L144,'(4a)') path(1:lnblnk(path)),fs,
     &      LCFGF(1:lnblnk(LCFGF)),'.vew'
        else
          write(L144,'(3a)') './',LCFGF(1:lnblnk(LCFGF)),'.vew'
        endif
        dvfil='std.vew'  ! default file name
        doit = ' '

C The X11 version will be returning only the name of the
C file, while the GTK version will be returning the
C name with the full path. L144 is the suggested name and
C lltmp is the file name returned by the user selection.
        lltmp='  '
        call edisp(iuout,'  ')  ! echo a blank line
        CALL EASKXORGTKF(L144,
     &    'Viewer export file name?',' ',
     &    dvfil,lltmp,'viewer file',IER,nbhelp)

C If user request jump back and re-display the menu.
        if(ier.eq.-3)then
          return  ! cancel detected, return.
        endif
        if(ier.ne.0.or.lltmp(1:2).eq.'  ')then
          goto 11
        endif
        if(unixok)then
          call addpath(LCFGF,longtfile,concat)  ! for the model cfg file
        else

C If running on a non-unix machine see if there are spaces in the name
C and change any / to \.
          call addpath(LCFGF,longtfile,concat)  ! for the model cfg file
          call cmdfiledos(longtfile,longtfiledos,ier)
          longtfile=' '
          longtfile=longtfiledos
        endif
        if(itrc.le.1)then
          write(doit,'(4a)') 'ecnv -obs -if esp -in ',
     &      longtfile(1:lnblnk(longtfile)),' -of viewer -out ',
     &      lltmp(1:lnblnk(lltmp))
          call usrmsg('starting conversion via',doit,'-')
          call runit(doit,'-')
        elseif(itrc.gt.1)then
          write(doit,'(4a)') 'ecnv -v -obs -if esp -in ',
     &      longtfile(1:lnblnk(longtfile)),' -of viewer -out ',
     &      lltmp(1:lnblnk(lltmp))
          call usrmsg('starting conversion via',doit,'-')
        endif
        tmode = 'text'
        call runit(doit,tmode)
        write(outs248,'(3a)')'The file ',lltmp(1:lnblnk(lltmp)),
     &    ' has been created.'
        call edisp248(iuout,outs248,90)

      ELSEIF(INO1.EQ.2)THEN

C DXF creation. Process similar to ecnv filter for VIEWER.
        call tstamp('>','PRJ: export to DXF')
        iw=0
        if(path(1:2).ne.'./'.and.path(1:2).ne.'  ')then
          write(outs,'(A,A)') 'The current path is: ',path
          call edisp(iuout,outs)
          CALL EASKAB('The model is in a remote folder.',
     &    'Place exported file in:','current path','local folder',
     &    IW,nbhelp)
        endif
        if(iw.eq.1)then
          write(L144,'(4a)') path(1:lnblnk(path)),fs,
     &      LCFGF(1:lnblnk(LCFGF)),'.DXF'
        else
          write(L144,'(3a)') './',LCFGF(1:lnblnk(LCFGF)),'.DXF'
        endif
        dvfil='std.DXF'  ! default file name
        doit = ' '

C L144 is the suggested name and lltmp is the file name returned
C by the user selection.
        lltmp='  '
        call edisp(iuout,'  ')  ! echo a blank line
        CALL EASKXORGTKF(L144,'DXF export file name?',' ',
     &    dvfil,lltmp,'dxf file',IER,nbhelp)

C If user request jump back and re-display the menu.
        if(ier.eq.-3)then
          return  ! cancel detected, return.
        endif
        if(ier.ne.0.or.lltmp(1:2).eq.'  ')then
          goto 11
        endif
        call edisp(iuout,' Performing model conversion.')
        if(unixok)then
          call addpath(LCFGF,longtfile,concat)
        else

C If running on a non-unix machine see if there are spaces in the name
C and change any / to \.
          call addpath(LCFGF,longtfile,concat)
          call cmdfiledos(longtfile,longtfiledos,ier)
          longtfile=' '
          longtfile=longtfiledos
        endif
        if(itrc.le.1)then
          write(doit,'(4a)') 'ecnv -obs -if esp -in ',
     &       longtfile(1:lnblnk(longtfile)),
     &      ' -of dxf -out ',lltmp(1:lnblnk(lltmp))
        elseif(itrc.gt.1)then
          write(doit,'(4a)') 'ecnv -v -obs -if esp -in ',
     &       longtfile(1:lnblnk(longtfile)),
     &      ' -of dxf -out ',lltmp(1:lnblnk(lltmp))
        endif
        call usrmsg('Starting conversion via',doit,'-')
        tmode = 'text'
        call runit(doit,tmode)
        write(outs248,'(3a)')'The file ',lltmp(1:lnblnk(lltmp)),
     &    ' has been created.'
        call edisp248(iuout,outs248,90)

      ELSEIF(INO1.EQ.3)THEN

C RADIANCE creation. Starts e2r with current configuration file.
        dok=.true.
        CALL ASKOK(' ','Run RADIANCE conversion tool?',OK,dok,nbhelp)
        if(OK)then

C If browsing pass this information to e2r.
          if(browse)then
            brw = ' -b yes'
          else
            brw = ' -b no '
          endif
          call tstamp('>','PRJ: export to RADIANCE')
          doit = ' '
          call tchild(ICPMOD)
          call termode(ICPMOD,tmode)
          if(unixok)then
            call addpath(LCFGF,longtfile,concat)
          else
            call addpath(LCFGF,longtfile,concat)
            call cmdfiledos(longtfile,longtfiledos,ier)
            longtfile=' '
            longtfile=longtfiledos
          endif
          if(iappw.gt.0.and.iappw.le.200)then
            write(doit,'(a,3i4,4a)') 'e2r -s ',
     &        iappw,iappx+35,iappy+40,' -file ',
     &        longtfile(1:lnblnk(longtfile)),brw,' &'
          else
            write(doit,'(4a)') 'e2r -s 0 0 0 -file ',
     &        longtfile(1:lnblnk(longtfile)),brw,' &'
          endif
          call usrmsg('Starting Radiance conversion via',doit,'-')
          call runit(doit,'-')
        endif

      ELSEIF(INO1.EQ.4)THEN

C Toggle output redirection to xfig.
        dok=.true.
        call askok(' ','Is wireframe composition ok?',oke,dok,nbhelp)
        if(.NOT.oke)then
          call usrmsg('Adjust wireframe image and',
     &      'then re-select this option.','W')
          goto 33
        endif

        if (MMOD.eq.8) then

C User is in graphic mode so open the export file and then request that
C the wireframe is redrawn (in the C code the low level calls will be
C writing out the vector information to the export file.
          write(xfile,'(2a)') cfgroot(1:lnblnk(cfgroot)),'_wire'
          call ctlexp(xfile,ixopen,ixloc,ixunit,'G','wire frame',IER)

C If user canceled the wireframe export loop back.
          if(ier.eq.-3)then
            goto 33
          endif
          call usrmsg('sending wireframe... ',' ','-')
          MODIFY=.TRUE.
          CALL ADJVIEW(IER)
          call cfgtogg(icfg_type,icfgz,icfgn,icfgc,icfgdfn,
     &                 iicfgz,iicfgn,iicfgc,iicfgdfn)
          call opencfg(icfg_type,icfgz,icfgn,icfgc,icfgdfn,
     &                 iicfgz,iicfgn,iicfgc,iicfgdfn)
          WRITE(etext,'(2A)')'Model: ',modeltitle(1:lnblnk(modeltitle))
          iside=1
          isize=1
          ifont=1
          call viewtext(etext,iside,isize,ifont)

C Toggle the wirefram export file closed.
          call usrmsg('sending wireframe...done.',' ','-')
          call ctlexp(xfile,ixopen,ixloc,ixunit,'G','Wire frame',IER)
          MODIFY=.TRUE.

C Check if conversion to xfig wanted.
          dok=.true.
          call askok(' ','Convert output to xfig format?',ok,dok,nbhelp)
          if(ok)then
            write(ffile,'(a,a4)') xfile(1:lnblnk(xfile)),'.fig'
            doit = ' '
            write(doit,'(4a)') 'ecnv -if ww -in ',
     &        xfile(1:lnblnk(xfile)),' -of xfig -out ',
     &        ffile(1:lnblnk(ffile))
            call usrmsg('Starting xfig conversion via',doit,'-')
            call runit(doit,'-')

C << still to be done - remove the initial _wire file after fig file
C << has been created

          endif
        endif

      ELSEIF(INO1.EQ.5)THEN

C Export to THINGS (MicroGDS THF) file. Treat obstructions as clusters.
        call tstamp('>','PRJ: export to THINGS')
        doit = ' '
        write(LFIL,'(2a)') cfgroot(1:lnblnk(cfgroot)),'.THF'
        CALL EASKS(LFIL,' ','MicroGDS THINGS file name?'
     &                ,72,' ','THF file',IER,nbhelp)
C        call edisp(iuout,'Performing model conversion.')

C Why is the addpath not used?
        if(unixok)then
          call addpath(LCFGF,longtfile,concat)
        else
          call addpath(LCFGF,longtfile,concat)
          call cmdfiledos(longtfile,longtfiledos,ier)
          longtfile=' '
          longtfile=longtfiledos
        endif
        if(itrc.le.1)then
          write(doit,'(4a)') 'ecnv -obs -if esp -in ',
     &       longtfile(1:lnblnk(longtfile)),
     &      ' -of thf -out ',LFIL(:lnblnk(LFIL))
        elseif(itrc.gt.1)then
          write(doit,'(4a)') 'ecnv -v -obs -if esp -in ',
     &       longtfile(1:lnblnk(longtfile)),
     &      ' -of thf -out ',LFIL(1:lnblnk(LFIL))
        endif
        call usrmsg('Starting THF conversion via',doit,'-')
        tmode = 'text'
        call runit(doit,tmode)
        write(outs,'(3a)')' The file ',LFIL(1:lnblnk(LFIL)),
     &    ' has been created.'
        call edisp(iuout,outs)

      ELSEIF(INO1.EQ.6)THEN

C Export to EnergyPlus IDF file (work in progress)
        call tstamp('>','PRJ: export to energyplus')
        doit = ' '
        write(LFIL,'(2a)') cfgroot(1:lnblnk(cfgroot)),'.idf'
        CALL EASKS(LFIL,' ','EnergyPlus input file name?',
     &        72,' ','EnergyPlus export file',IER,nbhelp)

        ever=3.0
        evert=ever
        CALL EASKR(evert,'EnergyPlus IDF file version?','(3.0)',
     &    3.0,'F',3.0,'F',1.4,'file version',IERI,nbhelp)
        if(ieri.eq.-3)then
          continue
        else
          call edisp(iuout,' Performing model conversion.')
          itrcc=2
          ioout=2
          ever=evert
          open(ioout,file=lfil,status='UNKNOWN',err=901)
          call e2eplus(itrcc,ioout,ever,ier)
          close(ioout)
          call edisp(iuout,' Completed model conversion.')
        endif

      ELSEIF(INO1.EQ.7)THEN

C Export to VRML (2.0) file.
        call tstamp('>','PRJ: export to VRML')
        doit = ' '
        write(LFIL,'(2a)') cfgroot(1:lnblnk(cfgroot)),'.vrml'
        CALL EASKS(LFIL,' ','VRML file name?',
     &             72,' ','vrml file',IER,nbhelp)
C        call edisp(iuout,' Performing model conversion.')
        if(unixok)then
          call addpath(LCFGF,longtfile,concat)
        else

C If running on a non-unix machine see if there are spaces in the name
C and change any / to \.
          call addpath(LCFGF,longtfile,concat)
          call cmdfiledos(longtfile,longtfiledos,ier)
          longtfile=' '
          longtfile=longtfiledos
        endif
        if(itrc.le.1)then
          write(doit,'(4a)') 'ecnv -if esp -in ',
     &      longtfile(1:lnblnk(longtfile)),
     &      ' -of vrml -out ',LFIL(1:lnblnk(LFIL))
        elseif(itrc.gt.1)then
          write(doit,'(4a)') 'ecnv -v -if esp -in ',
     &      longtfile(1:lnblnk(longtfile)),
     &      ' -of vrml -out ',LFIL(1:lnblnk(LFIL))
        endif
C        call usrmsg('Starting VRML conversion via',doit,'-')
        tmode = 'text'
        call runit(doit,tmode)
C        write(outs,'(3a)')' The file ',LFIL(1:lnblnk(LFIL)),
C     &    ' has been created.'
C        call edisp(iuout,outs)
C        call edisp(iuout,'Use a VRML viewer to explore it.')

      ELSEIF(INO1.EQ.8)THEN

C Export to ESP-r meta file.
        call tstamp('>','PRJ: export to ESP-r meta')
        doit = ' '
        write(LFIL,'(2a)') cfgroot(1:lnblnk(cfgroot)),'.mesp'
        CALL EASKS(LFIL,' ','ESP-r meta file name?',
     &             72,' ','ESP-r meta file',IER,nbhelp)
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
        ioout=2
        call METAWRITE(ioout,longtfile,'-',IER)

      ELSEIF(INO1.EQ.9)THEN
      
C Pop up the help text.
       CALL PHELPD('export menu',nbhelp,'-',0,0,IER)

      ELSEIF(INO1.EQ.10)THEN
        RETURN
      else
        goto 33
      endif
      goto 33

C Error.
 901  call isunix(unixok)
      if(unixok)write(6,*) 'Unable to open IDF file ',LFIL,' on ',ioout
      return
     
      END

C ******** cadin
C CADIN drives the conversion of CAD input into data model.
C used by the Project Manager.
C Parameter ichoice is currently not used.
      subroutine cadin(itrc,ichoice,ier)
      
      integer lnblnk  ! function definition

      common/FILEP/IFIL
      INTEGER :: ifil
      COMMON/OUTIN/IUOUT,IUIN
      INTEGER :: iuout,iuin
      
      integer ncomp,ncon
      common/C1/NCOMP,NCON
      common/C21/IFCFG,cfgroot,LCFGF
      INTEGER :: IFCFG
      common/rcmd/LCMDFL

C Indicator of possible focus zone.
      common/user/browse

C External cad package.
      common/cad3rd/cadlbl,cadexe,cadfmt

      LOGICAL     OK,DOK,concat,browse,typez,confirm,ckpath,unixok
      logical itisanexemplar  ! passed to newprb to ensure correct browse mode.

      character LCFGF*72,LFIL*72,LCMDFL*144
      character outs*124,OUTSTR*124,cfgroot*24,doit*248
      character cadlbl*20,cadexe*20,cadfmt*4
      character longtfile*144,longtfiledos*144

C For help messages
      character helpinsub*24 ! subroutine name
      character helptopic*24 ! string (unique) for topic
      integer nbhelp     ! number of help lines found

      helpinsub='cadin'  ! set for subroutine

C Set file unit and check if running Unix-like OS.
      ifun=ifil+1
      call isunix(unixok)

C CAD definition and importing.
      helptopic='cad_import_choices'
      call gethelptext(helpinsub,helptopic,nbhelp)

C Tell the user to create a model configuration file.
      CALL PHELPD('CAD warning',nbhelp,'-',0,0,IER)

C Ask user which format of CAD file will be scanned in.
      CALL EASKABC('Specify which CAD format to work with',
     &  'Options:','DXF (version 12-14)','other CAD format',
     &  'cancel',IRT,nbhelp)
      if(irt.eq.1)then
        cadfmt='DXF '
      elseif(irt.eq.2)then
        call usrmsg(
     &    'Alternative CAD input format is work in progress.',
     &    '  ','-')
        return
      elseif(irt.eq.3)then
        return
      endif

C If there is a currently loaded model then convert it.
      if(NCOMP.gt.0.and.cadfmt(1:3).eq.'DXF')then
        call tstamp('>','PRJ: start CAD')
        doit = ' '
        CALL EASKS(LFIL,' Put current model into DXF file named: ',
     &    ' ',72,' ','dxf export file',IER,nbhelp)
        call edisp(iuout,' Performing model conversion.')
        if(unixok)then
          call addpath(LCFGF,longtfile,concat)
        else

C If running on a non-unix machine see if there are spaces in the name
C and change any / to \.
          call addpath(LCFGF,longtfile,concat)
          call cmdfiledos(longtfile,longtfiledos,ier)
          longtfile=' '
          longtfile=longtfiledos
        endif
        if(itrc.le.1)then
          write(doit,'(4a)') 'ecnv -obs -if esp -in ',
     &      longtfile(1:lnblnk(longtfile)),
     &      ' -of dxf -out ',LFIL(1:lnblnk(LFIL))
        elseif(itrc.gt.1)then
          write(doit,'(4a)') 'ecnv -v -obs -if esp -in ',
     &      longtfile(1:lnblnk(longtfile)),
     &      ' -of dxf -out ',LFIL(1:lnblnk(LFIL))
        endif
      endif

      if(NCOMP.gt.0)then
        call runit(doit,'-')
        write(outs,'(3a)')' The file ',LFIL(1:lnblnk(LFIL)),
     &    ' has been created.'
        call edisp(iuout,outs)
      endif

      write(outs,'(3a)') ' Proceed with ',
     &  cadlbl(1:lnblnk(cadlbl)),' ?'
      dok=.true.
      CALL ASKOK(outs,'(no will just generate the file)',
     &  OK,dok,nbhelp)
      IF(OK)then
        if(cadfmt(1:3).eq.'ZIP')then
          continue
        elseif(cadfmt(1:3).eq.'DXF')then
          call runit(cadexe,'graph')
        endif
      endif

 28   dok=.true.
      CALL ASKOK(' ','Import the CAD transfer file?',OK,dok,nbhelp)
      IF(OK)then
        CALL EASKS(LFIL,' File to import ? ',
     &    ' ',72,' ','cad import file',IER,nbhelp)

C Read the first couple of lines of the import file to see if it
C is a DXF file.
        CALL ERPFREE(ifun,ISTAT)
        call FPOPEN(ifun,ISTAT,1,0,LFIL)
        IF(ISTAT.LT.0)THEN
          write(outs,'(a,a)')'Could not find: ',LFIL(1:lnblnk(LFIL))
          dok=.false.
          call askok(outs,'Try again?',OK,dok,nbhelp)
          if(OK)then
            goto 28
          else
            return
          endif
        endif
        CALL STRIPC(ifun,OUTSTR,0,ND,1,'CAD first line',IER)
        CALL STRIPC(ifun,OUTSTR,0,ND,1,'CAD 2nd line',IER)
        if(OUTSTR(1:7).eq.'SECTION')then
          typez=.false.
          call edisp(iuout,'Based on 2nd line of file:')
          call edisp(iuout,outstr)
          call edisp(iuout,'this file is assumed to be a DXF file.')
        endif
        CALL ERPFREE(ifun,ISTAT)

        if(unixok)then
          call addpath(LCFGF,longtfile,concat)
        else

C If running on a non-unix machine see if there are spaces in the name
C and change any / to \.
          call addpath(LCFGF,longtfile,concat)
          call cmdfiledos(longtfile,longtfiledos,ier)
          longtfile=' '
          longtfile=longtfiledos
        endif
        call edisp(iuout,' Performing model conversion.')
        call tstamp('>','PRJ: import from CAD')
        if(itrc.le.1)then
          write(doit,'(4a)') 'ecnv -obs -mm -if dxf -in ',
     &      LFIL(1:lnblnk(LFIL)),' -of esp -u upgrade -out ',
     &      longtfile(1:lnblnk(longtfile))
        elseif(itrc.gt.1)then
          write(doit,'(4a)')'ecnv -v -obs -mm -if dxf -in ',
     &      LFIL(1:lnblnk(LFIL)),' -of esp -u upgrade -out ',
     &      longtfile(1:lnblnk(longtfile))
        endif
        call usrmsg('doing conversion via',doit,'-')
        call runit(doit,'-')

C Re-read the upgraded configuration file. Reconstruct the path.
        itisanexemplar=.false.
        browse=.false.
        confirm=.true.
        ckpath=.false.
        LCMDFL=longtfile
        call NEWPRB(ITRC,confirm,ckpath,itisanexemplar,IER)
      endif
      return
      end

C ********* rexmpl
C rexmpl: scan the exemplars file, parse information and offer the user
C selection of or information about exemplars. 
C useraction is returned - 'browse' or 'ownit' or 'continue'
C iexfil (integer) is the unit number for the exemplar file
C exemplar (char 144) is the full path to selected cfg file.
C useraction (char 8) is passed in to specify the task.
C ier (integer) is returned as 1 if no exemplars file found.
      subroutine rexmpl(iexfil,exemplar,useraction,ier)
#include "building.h"
#include "model.h"
#include "espriou.h"
C espriou.h provides currentfile, defdbfl exemfl.
      
      integer lnblnk  ! function definition

      COMMON/SPAD/MMOD,LIMIT,LIMTTY
      common/OUTIN/IUOUT,IUIN
      
      integer ncomp,ncon
      common/C1/NCOMP,NCON
      common/C6/INDCFG
      INTEGER :: INDFCG

C Images - included to warn user on subsequent browsing sessions
C to quit the image browser.
      character imgfmt*4  ! GIF XBMP TIF JPG
      character imgfoc*4  ! FZON FNET FCTL FDFS
      character limgfil*72  ! file name (extend to 144 char)
      character imgdoc*248  ! text associated with image
      common/imagf/imgfmt(MIMG),imgfoc(MIMG),limgfil(MIMG),imgdoc(MIMG)

      integer noimg  ! number of images
      integer iton   ! zero if images not yet shown, one if yes
      common/imagfi/noimg,iton

      dimension name(30),mitem(30),label(30),inl2in(30),group(15)
      dimension gitem(18),gh(13),cfgitem(30),inl2ngr(30)
      character name*70,label*42,mitem*72,gitem*44,OUTSTR*124,WORD*20
      character group*42,tgroup*42,key*1,look*70,tlook*70
      character exemplar*144,line*72,outs*124,rootp*72,separ*42
      character doit*248
      character gh*72,cfgitem*144,exdescr*81,doc*72,head*42
      character cfg*144,cfg_path*72,cfg_root*72,doc_file*72

C Strings for dos version of rootp and upath and a double quote.
      character rootpdos*72,upathdos*72,rootpath*72,modelroot*72
      character dq*1,fs*1,thecfgis*72
      character useraction*8
      character lltmp*144,lguess*144,dguess*144
      logical XST,unixok
      integer NCOG,ICOG,NCO,ICO,icod ! max items and current menu item

#ifdef OSI
      integer impx,impy,iwe
#else
      integer*8 impx,impy,iwe
#endif

C For help messages
      common/pophelp/h(60)
      character h*72
      character helpinsub*24 ! subroutine name
      character helptopic*24 ! string (unique) for topic
      integer nbhelp     ! number of help lines found

      helpinsub='rexmpl'  ! set for subroutine

C Make up a double quote and file separator for xcopy.
      useraction ='-'
      call isunix(unixok)
      if(unixok)then
        fs = char(47)
        dq = char(34)
      else
        fs = char(92)
        dq = char(34)
      endif

C Open the exemplar file.
  41  IER=0
      if(exemfl(1:1).eq.'/')then
        call FPOPEN(iexfil,ISTAT,1,0,exemfl)
      else
        CALL EFOPSEQ(iexfil,exemfl,1,IER)
      endif
      if(ier.ne.0) return
C Read the header and see if '*EXEMPLARS'
      CALL STRIPC(iexfil,OUTSTR,0,ND,0,'exemplars header',IER)
      if(outstr(1:10).ne.'*EXEMPLARS')then
        call edisp(iuout,' Not an exemplars file!')
        ier=1
        return
      endif

C In the first pass make up the display, beginning with
C an organizational header and the `names' of each exemplar.
C Clear group related help and length of menu.
      do 2 i=1,13
        gh(i)=' '
  2   continue
      exemplar='UNKNOWN'
      ngr=0
      ing=0
      inl=0
      inl2ngr(1)=0
      igrsel=0
      icod=0

 12   CALL STRIPC(iexfil,OUTSTR,0,ND,0,'group id',IER)
      if(ier.ne.0)goto 901
      K=0
      CALL EGETW(outstr,K,WORD,'W','group id',IER)
      if(WORD(1:6).eq.'*group')then
        if(ngr.lt.15)then
          ngr=ngr+1
          inl=inl+1
          ing=ing+1
          CALL EMKEY(ing,KEY,IER)
          call egetrm(outstr,K,group(ngr),'W','group id',IER)
          write(gitem(inl),'(a1,1x,a42)') key, group(ngr)(1:42)
          inl2ngr(inl)=ngr
        endif
        goto 12
      elseif(WORD(1:6).eq.'*label')then
        inl=inl+1
        separ=' '
        call egetrm(outstr,K,separ,'W','label',IER)
        write(gitem(inl),'(2x,a42)') separ(1:42)
        inl2ngr(inl)=0
        goto 12
      else
        goto 12
      endif
 901  CALL ERPFREE(iexfil,ISTAT)

C Present the display.
 142  ICOG= -2
      gitem(inl+1)=  '  _______________________________'
      gitem(inl+2)=  '? help                           '
      gitem(inl+3)=  '- exit this menu                 '
      NCOG=inl+3
      IW=0
      do 141 ij=1,NCOG
        if(lnblnk(gitem(ij)).gt.IW)IW=lnblnk(gitem(ij))
 141  continue

C Help text for this menu.
      helptopic='exemplar_menu_help'
      call gethelptext(helpinsub,helptopic,nbhelp)

      if(MMOD.EQ.8)then
        impx=0
        impy=0
        iwe=iw
        CALL VWMENU('Exemplar Categories',gitem,NCOG,
     &    impx,impy,iwe,irpx,irpy,ICOG)
      else
        CALL EMENU('Exemplar Categories',gitem,NCOG,ICOG)
      endif
      IF(ICOG.EQ.NCOG)THEN
        CALL ERPFREE(iexfil,ISTAT)
        return
      ELSEIF(ICOG.EQ.(NCOG-1))THEN

C Help @ db file level.
        helptopic='exemplar_menu_help'
        call gethelptext(helpinsub,helptopic,nbhelp)
        CALL PHELPD('exemplar menu',nbhelp,'-',0,0,IER)
      ELSEIF(ICOG.GT.0.AND.ICOG.LT.(NCOG-2))THEN
        if(gitem(ICOG)(1:2).eq.'  ')goto 142
        if(inl2ngr(ICOG).gt.0.and.inl2ngr(ICOG).le.15)then
          igrsel=inl2ngr(ICOG)
        else
          goto 142
        endif
C Re-open the exemplar file.
        if(exemfl(1:1).eq.'/')then
          call FPOPEN(iexfil,ISTAT,1,0,exemfl)
        else
          CALL EFOPSEQ(iexfil,exemfl,1,IER)
        endif
 14     CALL STRIPC(iexfil,OUTSTR,0,ND,0,'group id',IER)
        if(ier.ne.0)goto 902
        K=0
        CALL EGETW(outstr,K,WORD,'W','group id',IER)
        if(WORD(1:6).eq.'*group')then
          call egetrm(outstr,K,tgroup,'W','group id',IER)
          if(tgroup(1:42).eq.group(igrsel)(1:42))then

C We have a match with the selected group so now fill the menu.
            nghelp=0
            in=0
            inl=0
 16         CALL STRIPC(iexfil,OUTSTR,0,ND,0,'group',IER)
            if(ier.ne.0)goto 90
            K=0
            CALL EGETW(outstr,K,WORD,'W','group id',IER)
            if(WORD(1:6).eq.'*group')then

C If we have reached the next group then we have scanned far enough.
              goto 90
            elseif(WORD(1:5).eq.'*help')then
              nghelp=nghelp+1
              call egetrm(outstr,K,gh(nghelp),'W','help',IER)
              goto 16
            elseif(WORD(1:5).eq.'*name')then
              in=in+1
              inl=inl+1
              call egetrm(outstr,K,name(in),'W','name',IER)
              write(label(inl),'(a)')'-'
              CALL EMKEY(in,KEY,IER)
              write(mitem(inl),'(a1,1x,a)') KEY,name(in)
              inl2in(inl)=in
              goto 16
            elseif(WORD(1:4).eq.'*cfg')then
              call egetrm(outstr,K,cfgitem(in),'W','cfgitem',IER)
              goto 16
            elseif(WORD(1:4).eq.'*log')then

C In case of older exemplar file.
              goto 16
            elseif(WORD(1:6).eq.'*label')then
              inl=inl+1
              call egetrm(outstr,K,label(inl),'W','label',IER)
              write(mitem(inl),'(2x,a)') label(inl)
              inl2in(inl)=0
              goto 16
            else
              goto 16
            endif
  90        CALL ERPFREE(iexfil,ISTAT)
            goto 42
          else
            goto 14
          endif
          goto 14
        else
          goto 14
        endif
      endif
 902  CALL ERPFREE(iexfil,ISTAT)
      goto 142

C Present the display.
  42  ICO= -2
      mitem(in+1)=  '  _______________________________'
      mitem(in+2)=  '* exemplar documentation         '
      mitem(in+3)=  '? help                           '
      mitem(in+4)=  '- exit this menu                 '
      NCO=in+4
      if(MMOD.EQ.8)then
        head = group(igrsel)(1:lnblnk(group(igrsel)))
      else
        write(head,'(a)') group(igrsel)(1:12)
      endif
      IWE=0
      do 143 ij=1,NCO
        if(lnblnk(mitem(ij)).gt.IWE)IWE=lnblnk(mitem(ij))
 143  continue

C Help text for this menu. Append the help text read from the
C exemplars file to the existing help text found in the esruprj.help
C file for this topic.
      helptopic='exemplar_set_browse'
      call gethelptext(helpinsub,helptopic,nbhelp)
      if(nghelp.gt.0)then
        do 33 ih=1,nghelp
          lex=MIN0(lnblnk(gh(ih)),72)
          write(h(nbhelp+ih),'(a)')gh(ih)(1:lex)
 33     continue
      endif

      if(MMOD.EQ.8)then
        impx=0
        impy=0
        CALL VWMENU(head,mitem,NCO,impx,impy,IWE,irpx,irpy,ICO)
      else
        CALL EMENU(head,mitem,NCO,ICO)
      endif
      IF(ICO.EQ.NCO)THEN

C Exit from detail level back to group level.
        CALL ERPFREE(iexfil,ISTAT)
        goto 41
      ELSEIF(ICO.EQ.(NCO-1))THEN

C Display the basic help plus help from exemplars file.
        CALL PHELPD('exemplar menu',nbhelp+nghelp,'-',0,0,IER)
      ELSEIF(ICO.EQ.(NCO-2))THEN
        icod=0
        if(MMOD.EQ.8)then
          impx=0
          impy=0
          CALL VWMENU('Select exemplar...',mitem,NCO,impx,impy,IWE,
     &      irpx,irpy,icod)
        else
          CALL EMENU('Select exemplar...',mitem,NCO,icod)
        endif
        if(ICOD.GE.1.AND.ICOD.LT.(NCO-3))then

C Open the related configuration file (if it exists), find its log file.
          CALL ERPFREE(iexfil+1,ISTAT)
          cfg=cfgitem(ICOD)
          INQUIRE (FILE=cfg,EXIST=XST)
          if(XST)then
            call fdroot(cfg,cfg_path,cfg_root)
            call FPOPEN(iexfil+1,ISTAT,1,0,cfg)
  242       CALL STRIPC(iexfil+1,OUTSTR,0,ND,0,'cfg scan',IER)
            if(ier.ne.0)goto 902
            if(OUTSTR(1:10).eq.'* PROJ LOG')then
              CALL STRIPC(iexfil+1,OUTSTR,0,ND,1,'log file',IER)
              doc=OUTSTR(1:LNBLNK(OUTSTR))
              write(doc_file,'(a,a)')cfg_path(1:lnblnk(cfg_path)),
     &          doc(1:lnblnk(doc))
              CALL ERPFREE(iexfil+1,ISTAT)
              call tstamp('>','PRJ: list exemplar documentation')
              CALL LISTAS(iexfil+1,doc_file,IER)
            else
              goto 242
            endif
          else
             call usrmsg('This exemplar is currently off-line.',
     &            'See your ESP-r administrator for access.','W')
          endif
        endif
      ELSEIF(ICO.GE.1.AND.ICO.LT.(NCO-3))THEN

C Present information on an exemplar.
        if(inl2in(ICO).eq.0)goto 42
        look=name(inl2in(ICO))
        if(exemfl(1:1).eq.'/')then
          call FPOPEN(iexfil,ISTAT,1,0,exemfl)
        else
          CALL EFOPSEQ(iexfil,exemfl,1,IER)
        endif
        CALL STRIPC(iexfil,OUTSTR,0,ND,0,'exemplar 1st line',IER)
        CALL STRIPC(iexfil,OUTSTR,0,ND,0,'group id',IER)
  3     CALL STRIPC(iexfil,OUTSTR,0,ND,0,'item lines',IER)
        if(ier.ne.0)goto 92

        line=' '
        exemplar=' '
        rootp=' '
        K=0
        CALL EGETW(outstr,K,WORD,'W','*name tag',IER)
        if(WORD(1:5).eq.'*name')then
          call egetrm(outstr,K,tlook,'W','name',IER)
          if(tlook.eq.look)then
            lex=MIN0(lnblnk(tlook),70)
            write(exdescr,'(a,a)') 'Model: ',tlook(1:lex)
  4         CALL STRIPC(iexfil,OUTSTR,0,ND,0,'item line',IER)
            if(ier.ne.0)goto 92
            K=0
            CALL EGETW(outstr,K,WORD,'W','*cfg tag',IER)
            if(WORD(1:4).eq.'*end')then
              goto 92
            elseif(WORD(1:4).eq.'*cfg')then
              call egetrm(outstr,K,exemplar,'W','name',IER)
              call edisp(iuout,' Model is:')
              call edisp(iuout,exemplar)

C If running on a Windows machine get the last token within the string exemplar,
C which is the name of the configuration file. Work backwards from the end
C of the string till a file separator of either DOS or Unix type is reached.
              call isunix(unixok)
              if(.NOT.unixok)then
                thecfgis=' '
                ilexem = lnblnk(exemplar)
                ipos = ilexem
 344            continue
                ipos=ipos-1
                if(exemplar(ipos:ipos).eq.fs)then
                  write(thecfgis,'(a)') exemplar(ipos+1:ilexem)
                  call edisp(iuout,'The model is...')
                  call edisp(iuout,thecfgis)
                elseif(exemplar(ipos:ipos).eq.'/')then
                  write(thecfgis,'(a)') exemplar(ipos+1:ilexem)
                  call edisp(iuout,'The model is...')
                  call edisp(iuout,thecfgis)
                else
                  if(ipos.eq.1)then
                    call edisp(iuout,' No folder separator found.')
                  elseif(ipos.gt.1)then
                    goto 344
                  endif
                endif
              endif

C Check to see if the configuration file exists. If it does not then
C warn the user that the model is off-line and return
C to the list of exemplars.
              CALL ERPFREE(iexfil+1,ISTAT)
              INQUIRE (FILE=exemplar,EXIST=XST)
              if(.NOT.XST)then
                call tstamp('>','PRJ: found off-line exemplar')
                call usrmsg('The selected exemplar is currently',
     &             'off-line.  See your administrator for access.','W')
                exemplar='UNKNOWN'
                CALL ERPFREE(iexfil+1,ISTAT)
                rewind(iexfil,ERR=92)
                goto 41
              endif
              goto 4
            elseif(WORD(1:5).eq.'*lcfg')then

C For older format file, skip past local configuration file name.
              goto 4
            elseif(WORD(1:5).eq.'*root')then
              call egetrm(outstr,K,rootp,'W','exem root dir',IER)
              goto 4
            elseif(WORD(1:5).eq.'*aide')then

C For older format file, normally ignore *aide line.
              call egetrm(outstr,K,line,'W','brief description',IER)
              goto 4
            elseif(WORD(1:4).eq.'*log')then
              call egetrm(outstr,K,line,'W','old format log',IER)
              goto 4
            elseif(WORD(1:5).eq.'*item')then

C Reached the end of the item, check if it is to be chosen and
C if there are zones existing, remove them from memory.
              CALL EASKABCD(exdescr,'Actions:','browse',
     &          '`own` it','select another','cancel',IW,nbhelp)
              if(IW.eq.1)then
                call tstamp('>','PRJ: browse exemplar')
                useraction='browse  '
              elseif(IW.eq.2)then
                if(upath.ne.' ')then

C See if can derive the local configuration name.
                  call fdroot(rootp,cfg_path,cfg_root)
                  lex=lnblnk(exemplar)
                  lcfgr=lnblnk(cfg_root)

C Debug the model source.
C                  write(outs,'(a,2i4,2a)') 'cfg_root = ',lex,lcfgr,
C     &              '  ',cfg_root(1:lcfgr)
C                  call edisp(iuout,outs)
                  do 342 ilex=1,lex-lcfgr
                    if(cfg_root(1:lcfgr).eq.
     &                exemplar(ilex:ilex+lcfgr-1))then
                      goto 343
                    endif
  342             continue
  343             continue
                  call isunix(unixok)
                  if(unixok)then

C Offer user choice of home folder or another location. In GTK
C there is a file browser returning folder name via lltmp.
                    write(lguess,'(a)') upath(1:lnblnk(upath))
                    write(dguess,'(a)') upath(1:lnblnk(upath))
                    call edisp(iuout,'  ')  ! echo a blank line
                    CALL EASKXORGTKF(lguess,
     &                'Place model as subfolder of?',' ',
     &                dguess,lltmp,'models folder',IER,nbhelp)

                    write(doit,'(4a)') 'cp -r ',
     &               rootp(1:lnblnk(rootp)),' ',lltmp(1:lnblnk(lltmp))
                  else

C Debug statements for native windows version.  If lnblankrp or
C lnblankup is one then there are no spaces in the string so double quotes
C are not required.  But in either case it is useful to change any '/'
C in rootp and upath to '\'. 
                    rootpdos=' '
                    upathdos=' '
                    lnrp=lnblnk(rootp)
                    lnblankrp=iprevblnk(rootp,lnrp)

C Debug the model source.
C                    write(outs,'(a,2i3,2a)') 'rootp = ',lnrp,lnblankrp,
C     &                ' ',rootp(1:lnrp)
C                    call edisp(iuout,outs)
                    call backslashit(rootp,rootpdos)
                    write(outs,'(2a)') 'Getting model from...',
     &                rootpdos(1:lnrp)
                    call edisp(iuout,outs)

C For an xcopy command we need the destination to also include the
C model folder so use fdroot to extract this from rootp.
C                    call fdroot(rootp,rootpath,modelroot)
                    call fdroot(rootpdos,rootpath,modelroot)
                    lnmr=lnblnk(modelroot)

C Debug the model destination.
C                    write(outs,'(2a)') 'the model root folder is = ',
C     &                modelroot(1:lnmr)
C                    call edisp(iuout,outs)
                    call edisp(iuout,' ')

C Offer user choice of home folder or another location. In GTK
C there is a file browser returning folder name via lltmp.
                    write(lguess,'(a)') upath(1:lnblnk(upath))
                    write(dguess,'(a)') upath(1:lnblnk(upath))
                    call edisp(iuout,'  ')  ! echo a blank line
                    CALL EASKXORGTKF(lguess,
     &                'Place model as subfolder of?',' ',
     &                dguess,lltmp,'models folder',IER,nbhelp)
                    lnup=lnblnk(lltmp)
                    lnblankup=iprevblnk(lltmp,lnup)
                    call backslashit(lltmp,upathdos)
C Debug.
                    write(outs,'(a,2i3,2a)') 'upath = ',lnup,lnblankup,
     &                ' ',upath(1:lnup)
                    call edisp(iuout,outs)
                    write(outs,'(2a)') 'destination = ',upathdos(1:lnup)
                    call edisp(iuout,outs)
                    if(lnblankrp.gt.1.and.lnblankup.gt.1)then

C There are blanks in both source or destination strings.
                      write(doit,'(10a)') 'xcopy /e /i /y /c ',dq,
     &                  rootpdos(1:lnrp),dq,' ',dq,
     &                  upathdos(1:lnup),fs,modelroot(1:lnmr),dq
                    elseif(lnblankrp.eq.1.and.lnblankup.gt.1)then

C There are blanks in destination string.
                      write(doit,'(8a)') 'xcopy /e /i /y /c ',
     &                  rootpdos(1:lnrp),' ',dq,
     &                  upathdos(1:lnup),fs,modelroot(1:lnmr),dq
                    elseif(lnblankrp.gt.1.and.lnblankup.eq.1)then

C There are blanks in source string.
                      write(doit,'(8a)') 'xcopy /e /i /y /c ',dq,
     &                  rootpdos(1:lnrp),dq,' ',
     &                  upathdos(1:lnup),fs,modelroot(1:lnmr)
                    elseif(lnblankrp.eq.1.and.lnblankup.eq.1)then

C There are no blanks in either source or destination strings.
                      write(doit,'(6a)') 'xcopy /e /i /y /c ',
     &                  rootpdos(1:lnrp),' ',
     &                  upathdos(1:lnup),fs,modelroot(1:lnmr)
                    endif
                  endif
                  call lusrmsg(
     &               'Copying exemplar to your destination folder.',
     &               doit,'-')
                  call runit(doit,'-')
                  if(unixok)then

C If Unix-like then append tail of exemplar string to the users selected
C path to get the new exemplar name.
                    write(exemplar,'(3a)') lltmp(1:lnblnk(lltmp)),fs,
     &                exemplar(ilex:lnblnk(exemplar))
                  else

C If Windows based then append the modelroot string to the path to make
C up the exemplar name.
                    lnmr=lnblnk(modelroot)
                    write(exemplar,'(7a)')upathdos(1:lnblnk(upathdos)),
     &                fs,modelroot(1:lnmr),fs,'cfg',fs,
     &                thecfgis(1:lnblnk(thecfgis))
                  endif
                  call edisp(iuout,'The model (that you own) is now:')
                  call edisp(iuout,exemplar)
                  call tstamp('>','PRJ: owning exemplar')
                  call tstamp('>',exemplar)
                  useraction='ownit   '
                endif
              elseif(IW.eq.3)then

C User asked to select another, rewind the file and jump back to
C the top menu to re-resent groups.
                exemplar='UNKNOWN'
                REWIND(iexfil,ERR=92)
                useraction='continue'
                goto 41
              elseif(IW.eq.4)then

C User asked to continue, close the file set ier and return to top
C level menu of prj.
                exemplar='UNKNOWN'
                CALL ERPFREE(iexfil,ISTAT)
                useraction='continue'
                ier=2
                return
              endif
              if(IW.eq.1.or.IW.eq.2)then
                CALL ERPFREE(iexfil,ISTAT)
                if(ncomp.gt.0.or.INDCFG.eq.2)then
                  call usrmsg('Please wait... ',
     &                        'Clearing previous model.','-')
                  call startbuffer()
                  call clrprb
                endif

C If the image browser was started in a previous session warn
C the user to kill it.
                if(iton.eq.1)then
                  noimg=0
                  iton=0
                endif
                return
              endif
              goto 92
            else
              goto 4
            endif
          else
            goto 3
          endif
        else
          goto 3
        endif
      else
        goto 42
      endif
      goto 42

  92  CALL ERPFREE(iexfil,ISTAT)
      goto 42

      end

C ***** wtarentry
C wtarentry: write entry in tar names file taking into account case where file
C name begins with ../ .
      subroutine wtarentry(iunit,pathome,tr,ltr,tp,ltp,lf)
      integer lnblnk  ! function definition

C Parameters
      integer iunit   ! file unit for written entries
      logical pathome ! true uses tp, false uses tr
      character tp*32 ! path including final file separator
      character tr*32 ! path without 
      character*(*) lf ! file name for entries
      integer ltr,ltp  ! length of strings tp and tr
      
      character fs*1
      logical unixok

      call isunix(unixok)
      if(unixok)then
        fs = char(47)
      else
        fs = char(92)
      endif

      llf=lnblnk(lf)
      lltr=ltr
      lltp=ltp
      if(llf.eq.0)then
        return
      elseif(lf(1:7).eq.'UNKNOWN')then
        return
      elseif(lf(1:3).eq.'../')then
        if(.NOT.pathome)then
          write(IUNIT,'(3a)',IOSTAT=IOS,ERR=2) tr(1:lltr),fs,lf(4:llf)
        else
          write(IUNIT,'(3a)',IOSTAT=IOS,ERR=2) tp(1:lltp),fs,lf(1:llf)
        endif
      elseif(lf(1:2).eq.'./')then
        write(IUNIT,'(3a)',IOSTAT=IOS,ERR=2) tp(1:lltp),fs,lf(3:llf)
      else
        write(IUNIT,'(3a)',IOSTAT=IOS,ERR=2) tp(1:lltp),fs,lf(1:llf)
      endif
      return

C Error.
    2 if(IOS.eq.2)then
        CALL USRMSG('Permissions issue writing tar names entry',lf,'W')
      else
        CALL USRMSG('Problem writing tar names entry',lf,'W')
      endif
      call ERPFREE(IUNIT,ISTAT)
      return

      end


C ***** archiveit
C archiveit: archives the current model by building up an "include" file to
C pass to tar archive. Detect where the current model is located
C and if the user is browsing the model.
C << to be done - then name file generated is never cleaned up >>
      subroutine archiveit(ier)

      USE AIM2_InputData, ONLY: iAIM2,LAIM2
      
      IMPLICIT NONE

#include "building.h"
#include "model.h"
#include "uncertainty.h"
#include "esprdbfile.h"
C esprdbfile.h supplies the following:
C LAPRES,IAPRES (for window pressure databases)
C LOPTDB,IOPTDB (for optical database)
C LPCDB/IPCDB (for plant template database)
C LPRFDB,IPRODB (for event profile database)
C LFMAT,IFMAT,LFMUL,IFMUL (for materials and constructions database)
C ICLIM,LCLIM (for climate file)
#include "schedule.h"
      
      integer lnblnk  ! function definition

      common/OUTIN/IUOUT,IUIN
      INTEGER :: iuout,iuin
      common/FILEP/IFIL
      INTEGER :: ifil
      common/rpath/path
      
      integer ncomp,ncon
      COMMON/C1/NCOMP,NCON
      COMMON/C6/INDCFG
      INTEGER :: INDCFG
      common/C21/IFCFG,cfgroot,LCFGF
      INTEGER :: ifcfg
      COMMON/C23/IFPNF,LPNF
      INTEGER :: IFPNF
      COMMON/AFN/IAIRN,LAPROB,ICAAS(MCOM)
      INTEGER :: iairn,icaas
      common/LLOG/LPRJLG
      common/cctlnm/ctldoc,lctlf
      common/FOPENED/CFGOK,MLDBOK,MATDBOK,CTLOK,OPTKOK
      common/GTFIL/GTGEOM
      common/cfdfil/LCFD(MCOM),IFCFD(MCOM)
      INTEGER :: ifcfd
      common/rcmd/LCMDFL
      common/user/browse
      common/IPVF/lipvdatf
      COMMON/GR1D05/LGRD1D
      COMMON/GR1D06/IGR1D
      COMMON/GRND100/GRND3D
      COMMON/GRND108/LGDCVS,LGDCNC,LGDNDC,LGDTAQ,LGDNDD
      COMMON/GRSD100/IndxSt
      INTEGER :: IndxSt
      COMMON/GRSD101/LGrdSt
      common/GR3D100/BLDG3D,ZONE3D(MCOM)
      LOGICAL :: BLDG3D,ZONE3D
      common/GR3D108/L3DCVS(MCOM),L3DCNC(MCOM),L3DNDC(MCOM),L3DTAQ(MCOM)
      COMMON/VTHP18/LNLTHP
      COMMON/VTHP31/INTHPS,INTHPZ(MCOM)
      common/MOIST01/MSTROK,MSTRZN(MCOM)
      LOGICAL :: MSTROK,MSTRZN
      common/MOIST02/LMOIST(MCOM)
      common/spmfxst/ispmxist,spflnam
      common/entfile/entflnam, ientxist
      INTEGER :: ispmxist,ientxist
      COMMON/UA1/LUALF,LCNG(MNCNG),LLOC(MNIL)
      character imgfmt*4  ! GIF XBMP TIF JPG
      character imgfoc*4  ! FZON FNET FCTL FDFS
      character limgfil*72  ! file name (extend to 144 char)
      character imgdoc*248  ! text associated with image
      common/imagf/imgfmt(MIMG),imgfoc(MIMG),limgfil(MIMG),imgdoc(MIMG)

      integer noimg  ! number of images
      integer iton   ! zero if images not yet shown, one if yes
      common/imagfi/noimg,iton
      common/hvacinfo/ihvacflag,hvacfile
      INTEGER :: ihvacflag
      common/bsmtdef/iBSIMP(MCOM),LBSIMP(MCOM)
      INTEGER :: ibsimp

      character LUALF*72,LCNG*15,LLOC*15,LCFGF*72
      character LPRJLG*72,LPNF*72,LMOIST*72
      character*72 LCFD
      character LCMDFL*144,LFIL*72
      
C The string buffer doit and option used for tar command.
      character doit*248,option*4
      character path*72,cfgroot*24,curdir*72
      character LAPROB*72
      CHARACTER*72 LGDCVS,LGDCNC,LGDNDC,LGDNDD,LGDTAQ,LGRD1D,LNLTHP
      character tp*32,tr*32
      character ctldoc*248,LCTLF*72,LGrdSt*72
      character GTGEOM*72,lipvdatf*72
      character fs*1
      character LTMPFL*72
      character LF*72,LFL*144
      CHARACTER spflnam*72,entflnam*72
      CHARACTER*72 L3DCVS,L3DCNC,L3DNDC,L3DTAQ
      character LLASCI*144
      character hvacfile*72,LBSIMP*72
      character outs*124

      logical browse,XST,unixok
      LOGICAL INTHPS,INTHPZ,IGR1D,GRND3D
      LOGICAL CFGOK,MLDBOK,MATDBOK,CTLOK,OPTKOK

C Patdot is true if path is ./, patcfg is true if path is cfg,
C patroot is true if path is at root level. 
      logical patdot,patcfg,patroot,pathome,localdb
      character WORDS(12)*32
C      character WORDS*32

      INTEGER :: lc,j,iw,iuf,iunit,ito,istat,ipid,img,ifold,i,ier
      INTEGER :: iaccess,icerr,ios,lcd,llf,lr,ltp,ltr

      iunit=ifil+1
      patdot=.false.
      patcfg=.false.
      patroot=.false.
      pathome=.false.

C Determine if user is browsing.
      if(browse)then
        call usrmsg('You do not own the current model and so are',
     &    'not allowed to archive it.','W')
        ier=1
        return
      endif

C Set folder separator (fs) to \ or / as required. Create a temporary
C file to hold the names of the files to be archived. Use esppid to
C get the process number. If that file happens to already exist then
C remove it and then open for writing.
      call isunix(unixok)
      if(unixok)then
        fs = char(47)
        call esppid(ipid)
        write(LTMPFL,'(a,i7,a)') cfgroot(1:lnblnk(cfgroot)),ipid,
     &    '.names'
        call st2file(LTMPFL,LFIL)
        write (LTMPFL,'(3a)')upath(1:lnblnk(upath)),fs,
     &    LFIL(1:lnblnk(LFIL))
        INQUIRE (FILE=LTMPFL(1:lnblnk(LTMPFL)),EXIST=XST)
        if(XST)then
          call edisp(iuout,'Removing existing name file.')
          call FPOPEN(IUNIT,ISTAT,1,1,LTMPFL)
          call EFDELET(IUNIT,ISTAT)
        endif
        call FPOPEN(IUNIT,ISTAT,1,2,LTMPFL)
      else
        fs = char(92)
      endif

C Determine the current path to the top level folder of the model.
      call usrdir(curdir)
      lcd = lnblnk(curdir)

C Check and see if curdir is the same as upath (i.e. esp-r started
C in users home folder.
      if(curdir(1:lcd).eq.upath(1:lnblnk(upath)))then

C Can user write at the curdir folder?
        ifold=-1
        call ckaccess(ifold,iaccess,icerr,curdir)
        if(iaccess.eq.1)then
          call isunix(unixok)
          if(unixok)write(6,*) 'cannot write at',curdir(1:lcd)
        endif

C Determine how to treat the path. See if the root name falls into the
C path somewhere.
        call GETTOKENS(path,IW,WORDS)
        lr=lnblnk(cfgroot)
        if(IW.gt.0)then
          do 94 ito=1,IW

C If cfg in tokens then check folder above and see if it matches the
C model root name. If so check and see if it is the first token (if
C so then easy to write, if not then try for the token before that).
            if(WORDS(ito)(1:3).eq.'cfg'.and.ito.gt.1)then
              if((ito-1).eq.1)then
                write(tp,'(3a)') WORDS(ito-1)(1:lnblnk(WORDS(ito-1))),
     &            fs,'cfg'
                ltp=lnblnk(tp)
                write(tr,'(a)') WORDS(ito-1)(1:lnblnk(WORDS(ito-1)))
                ltr=lnblnk(tr)
                patroot=.true.
                pathome=.true.
              elseif((ito-1).eq.2)then
                write(tp,'(5a)') WORDS(ito-2)(1:lnblnk(WORDS(ito-2))),
     &            fs,WORDS(ito-1)(1:lnblnk(WORDS(ito-1))),fs,'cfg'
                ltp=lnblnk(tp)
                write(tr,'(a)') WORDS(ito-1)(1:lnblnk(WORDS(ito-1)))
                ltr=lnblnk(tr)
                patroot=.true.
                pathome=.true.
              elseif((ito-1).eq.3)then
                write(tp,'(7a)') WORDS(ito-3)(1:lnblnk(WORDS(ito-3))),
     &            fs,WORDS(ito-2)(1:lnblnk(WORDS(ito-2))),
     &            fs,WORDS(ito-1)(1:lnblnk(WORDS(ito-1))),fs,'cfg'
                ltp=lnblnk(tp)
                write(tr,'(a)') WORDS(ito-1)(1:lnblnk(WORDS(ito-1)))
                ltr=lnblnk(tr)
                patroot=.true.
                pathome=.true.
              endif
            endif
  94      continue
        endif
        if(patroot)goto 98
      endif


C Can user write at the path folder?
      ifold=-1
      call ckaccess(ifold,iaccess,icerr,path)
      if(iaccess.eq.1)then
        call isunix(unixok)
        if(unixok)write(6,*) 'cannot write at',path(1:lnblnk(path))
      endif

C Determine how to treat the path. If the start of the path matches
C the root name check for rootname/cfg folders.
      lr=lnblnk(cfgroot)
      write(tp,'(3a)') cfgroot(1:lr),fs,'cfg'
      write(tr,'(a)') cfgroot(1:lr)
      if(path(1:lr).eq.cfgroot(1:lr))then
        if(path(1:lr+4).eq.tp(1:lr+4))then
          ltp=lnblnk(tp)
          write(tr,'(a)') cfgroot(1:lr)
          ltr=lnblnk(tr)
          patroot=.true.
        endif
      elseif(path(1:2).eq.'./')then
        call GETTOKENS(curdir,IW,WORDS)
        if(IW.gt.0)then

C Find out how many characters are in the two folders above where we
C are now so this can be subtracted from curdir for testing.
          write(LF,'(3a)') WORDS(IW-1)(1:lnblnk(WORDS(IW-1))),
     &            fs,WORDS(IW)(1:lnblnk(WORDS(IW)))
          llf=lnblnk(LF)
          write(LF,'(a)') curdir(1:lcd-llf)

C Debug.
          call isunix(unixok)
          if(unixok)then
            write(outs,*) 'checking writing at ',LF(1:lnblnk(LF))
            call usrmsg(outs,' ','-')
          endif
          ifold=-1
          call ckaccess(ifold,iaccess,icerr,LF)
          if(ifold.eq.1.and.icerr.eq.0)then
            write(tp,'(3a)') WORDS(IW-1)(1:lnblnk(WORDS(IW-1))),
     &            fs,WORDS(IW)(1:lnblnk(WORDS(IW)))
            ltp=lnblnk(tp)
            write(tr,'(a)') WORDS(IW-1)(1:lnblnk(WORDS(IW-1)))
            ltr=lnblnk(tr)
            patdot=.true.
            goto 98
          else

C There is no such folder so check the full path to find out which folder
C is one level above the cfg folder.
            call usrmsg('Cannot write tar file in folder above the',
     &        'model so canceling archive request.','W')
            return
          endif
        endif
      elseif(path(1:3).eq.'cfg')then

C User started esp-r one folder above the cfg folder. First test if
C cfg is appended to the curdir and this is a true path. If so then
C will do a cd ../ before the tar command and so cfgroot can be included
C in the tp name.
        call GETTOKENS(curdir,IW,WORDS)
        if(IW.gt.0)then

C Find out how many characters are in the folder above where we
C are now so this can be subtracted from curdir for testing.
          write(LF,'(a)') WORDS(IW)(1:lnblnk(WORDS(IW)))
          llf=lnblnk(LF)
          write(LF,'(a)') curdir(1:lcd-llf)

C Debug.
          call isunix(unixok)
          if(unixok)then
            write(outs,*) 'checking writing at ',LF(1:lnblnk(LF))
            call usrmsg(outs,' ','-')
          endif
          ifold=-1
          call ckaccess(ifold,iaccess,icerr,LF)
          if(ifold.eq.1.and.icerr.eq.0)then
            patcfg=.true.
            write(tp,'(3a)') WORDS(IW)(1:lnblnk(WORDS(IW))),fs,'cfg'
            ltp=lnblnk(tp)
            write(tr,'(a)') WORDS(IW)(1:lnblnk(WORDS(IW)))
            ltr=lnblnk(tr)
            goto 98
          else

C There is no such folder so check the full path to find out which folder
C is one level above the cfg folder.
            call usrmsg('Cannot write tar file in folder above the',
     &        'model so canceling archive request.','W')
            return
          endif
        endif
      else

C There is no such folder so check the full path to find out which folder
C is one level above the cfg folder.
        call GETTOKENS(path,IW,WORDS)
        if(IW.gt.0)then
          do 95 ito=1,IW
            if(WORDS(ito)(1:3).eq.'cfg'.and.ito.gt.1)then
              write(tp,'(3a)') WORDS(ito-1)(1:lnblnk(WORDS(ito-1))),
     &          fs,'cfg'
              ltp=lnblnk(tp)
              write(tr,'(a)') WORDS(ito-1)(1:lnblnk(WORDS(ito-1)))
              ltr=lnblnk(tr)
            endif
  95      continue
        endif
      endif

C Do a final check to see if can write in curdir + tp
      ifold=-1
      write(LF,'(3a)') curdir(1:lnblnk(curdir)),fs,tp(1:ltp)
      call ckaccess(ifold,iaccess,icerr,LF)
      if(iaccess.eq.1)then
        call usrmsg(
     &    'Cannot resolve file paths in order to write archive...',
     &    LF,'W')
        return
      endif

C Build up a list of the current files used in the model.
  98  write(IUNIT,'(3a)',IOSTAT=IOS,ERR=2) tp(1:ltp),fs,
     &  LCFGF(1:lnblnk(LCFGF))
      write(IUNIT,'(3a)',IOSTAT=IOS,ERR=2) tp(1:ltp),fs,
     &  LCNN(1:lnblnk(LCNN))

C For each of the databases figure out if the database is
C local (if ipath* value is 1).
      do 41 i=1,7
        localdb=.false.
        if(i.eq.1)then
          LFL=LFMAT
          if(ipathmat.eq.1) localdb=.true.
        elseif(i.eq.2)then
          LFL=LFMUL
          if(ipathmul.eq.1) localdb=.true.
        elseif(i.eq.3)then
          LFL=LOPTDB
          if(ipathoptdb.eq.1) localdb=.true.
        elseif(i.eq.4)then
          LFL=LAPRES
          if(ipathapres.eq.1) localdb=.true.
        elseif(i.eq.5)then
          LFL=LPRFDB
          if(ipathprodb.eq.1) localdb=.true.
        elseif(i.eq.6)then
          LFL=LCLIM
        elseif(i.eq.7)then
          LFL=LPCDB
          if(ipathpcdb.eq.1) localdb=.true.
        endif
        if(localdb.and.LFL(1:4).ne.'UNKN')then
          call wtarentry(iunit,pathome,tr,ltr,tp,ltp,LFL)
        endif
  41  continue

C Write control file if it exists.
      if(LCTLF(1:4).eq.'UNKN'.or.LCTLF(1:2).eq.'  ')then
        continue
      else
        lc=lnblnk(ctlpth)
        if(ctlpth(1:lc).eq.LCTLF(1:lc))then
          if(ctlpth(1:3).eq.'../')then
            if(.NOT.pathome)then
              write(IUNIT,'(5a)',IOSTAT=IOS,ERR=2) tr(1:ltr),fs,
     &          ctlpth(4:lc),fs,LCTLF(lc+2:lnblnk(LCTLF))
            else
              write(IUNIT,'(5a)',IOSTAT=IOS,ERR=2) tp(1:ltp),fs,
     &          ctlpth(1:lc),fs,LCTLF(lc+2:lnblnk(LCTLF))
            endif
          elseif(ctlpth(1:2).eq.'./')then
           write(IUNIT,'(5a)',IOSTAT=IOS,ERR=2) tp(1:ltp),fs,
     &       ctlpth(3:lc),fs,LCTLF(lc+1:lnblnk(LCTLF))
          endif
        else
          call wtarentry(iunit,pathome,tr,ltr,tp,ltp,LCTLF)
        endif
      endif

C Write HOT3000 hvac file if known.
      if(ihvacflag.eq.0)then
        continue
      else
        call wtarentry(iunit,pathome,tr,ltr,tp,ltp,hvacfile)
      endif

C Write HOT3000 AIM-2 file if known.
      if(iAIM2.eq.0)then
        continue
      elseif(LAIM2(1:7).eq.'UNKNOWN'.or.LAIM2(1:2).eq.'  ')then
        continue
      else
        lc=lnblnk(aimpth)
        if(aimpth(1:lc).eq.LAIM2(1:lc))then
          if(aimpth(1:3).eq.'../')then
            if(.NOT.pathome)then
              write(IUNIT,'(5a)',IOSTAT=IOS,ERR=2) tr(1:ltr),fs,
     &          aimpth(4:lc),fs,LAIM2(lc+2:lnblnk(LAIM2))
            else
              write(IUNIT,'(5a)',IOSTAT=IOS,ERR=2) tp(1:ltp),fs,
     &          aimpth(1:lc),fs,LAIM2(lc+2:lnblnk(LAIM2))
            endif
          elseif(aimpth(1:2).eq.'./')then
            write(IUNIT,'(5a)',IOSTAT=IOS,ERR=2) tp(1:ltp),fs,
     &        aimpth(3:lc),fs,LAIM2(lc+1:lnblnk(LAIM2))
          endif
        else
          call wtarentry(iunit,pathome,tr,ltr,tp,ltp,LAIM2)
        endif
      endif

C Write uncertainty analysis library file if known.
      call wtarentry(iunit,pathome,tr,ltr,tp,ltp,LUALF)

C Write images if any.
      if(noimg.gt.0)then
        lc=lnblnk(imgpth)
        do 42 img=1,noimg
          LF=limgfil(img)
          if(imgpth(1:lc).eq.LF(1:lc))then
            if(imgpth(1:3).eq.'../')then
              if(.NOT.pathome)then
                write(IUNIT,'(5a)',IOSTAT=IOS,ERR=2) tr(1:ltr),fs,
     &            imgpth(4:lc),fs,LF(lc+2:lnblnk(LF))
              else
                write(IUNIT,'(5a)',IOSTAT=IOS,ERR=2) tp(1:ltp),fs,
     &            imgpth(1:lc),fs,LF(lc+2:lnblnk(LF))
              endif
            elseif(imgpth(1:2).eq.'./')then
              write(IUNIT,'(5a)',IOSTAT=IOS,ERR=2) tp(1:ltp),fs,
     &          imgpth(3:lc),fs,LF(lc+1:lnblnk(LF))
            endif
          else
            call wtarentry(iunit,pathome,tr,ltr,tp,ltp,LF)
          endif
 42     continue
      endif

C Write non-specific demads.
      if(bdmds(1:4).eq.'UNKN'.or.bdmds(1:2).eq.'  ')then
        continue
      else
        lc=lnblnk(ctlpth)
        if(ctlpth(1:lc).eq.bdmds(1:lc))then
          if(ctlpth(1:3).eq.'../')then
            if(.NOT.pathome)then
              write(IUNIT,'(5a)',IOSTAT=IOS,ERR=2) tr(1:ltr),fs,
     &          ctlpth(4:lc),fs,bdmds(lc+2:lnblnk(bdmds))
            else
              write(IUNIT,'(5a)',IOSTAT=IOS,ERR=2) tp(1:ltp),fs,
     &          ctlpth(1:lc),fs,bdmds(lc+2:lnblnk(bdmds))
            endif
          elseif(ctlpth(1:2).eq.'./')then
           write(IUNIT,'(5a)',IOSTAT=IOS,ERR=2) tp(1:ltp),fs,
     &      ctlpth(3:lc),fs,bdmds(lc+1:lnblnk(bdmds))
          endif
        else
          call wtarentry(iunit,pathome,tr,ltr,tp,ltp,bdmds)
        endif
      endif

      if(lradcf(1:4).eq.'UNKN'.or.lradcf(1:2).eq.'  ')then
        continue
      else
        call wtarentry(iunit,pathome,tr,ltr,tp,ltp,lradcf)
      endif

C Non-linear thermophysical properties configuration file.

C << ?? why write to unit 6 rather than iunit ?? >>
       call isunix(unixok)
       if(INTHPS)then
        if(unixok)WRITE(6,'(2A)') '*nlt ',LNLTHP(1:lnblnk(LNLTHP))
      endif
      if(IGR1D)then
        if(unixok)WRITE(6,'(2A)') '*g1d ',LGRD1D(1:lnblnk(LGRD1D))
      endif
      if(spflnam(1:4).eq.'UNKN'.or.spflnam(1:2).eq.'  ')then
        continue
      else
        if(ispmxist.gt.0)then
          call wtarentry(iunit,pathome,tr,ltr,tp,ltp,spflnam)
        endif
      endif
      if(ientxist.gt.0)then
        write(IUNIT,'(a)') entflnam(1:lnblnk(entflnam))
      endif

C Temporal database.
      if(LTDF(1:4).eq.'UNKN'.or.LTDF(1:2).eq.'  ')then
        continue
      else
        if(LTDF(1:3).eq.'../')then
          if(.NOT.pathome)then
            write(IUNIT,'(3a)',IOSTAT=IOS,ERR=2) tr(1:ltr),fs,
     &        LTDF(4:lnblnk(LTDF))
          else
            write(IUNIT,'(3a)',IOSTAT=IOS,ERR=2) tp(1:ltp),fs,
     &        LTDF(1:lnblnk(LTDF))
          endif
        elseif(LTDF(1:2).eq.'./')then
          write(IUNIT,'(3a)',IOSTAT=IOS,ERR=2) tp(1:ltp),fs,
     &     LTDF(3:lnblnk(LTDF))
        else
          write(IUNIT,'(3a)',IOSTAT=IOS,ERR=2) tp(1:ltp),fs,
     &     LTDF(1:lnblnk(LTDF))
        endif
      endif

C Project log file.
      call wtarentry(iunit,pathome,tr,ltr,tp,ltp,LPRJLG)

C Ground information.
      IF(GRND3D)THEN
        call wtarentry(iunit,pathome,tr,ltr,tp,ltp,LGDCVS)
        call wtarentry(iunit,pathome,tr,ltr,tp,ltp,LGDCNC)
        call wtarentry(iunit,pathome,tr,ltr,tp,ltp,LGDNDC)
        call wtarentry(iunit,pathome,tr,ltr,tp,ltp,LGDTAQ)
      ENDIF

C Ground topology.
      call wtarentry(iunit,pathome,tr,ltr,tp,ltp,GTGEOM)

C Write zone files.
      DO 920 I = 1,NCOMP
        do 921 J = 1,10
          LF='UNKNOWN'
          if(J.eq.1)then
            LF=LGEOM(I)
          elseif(J.eq.2)then
            LF=LPROJ(I)
          elseif(J.eq.3)then
            LF=LTHRM(I)
          elseif(J.eq.4.and.IOBS(I).EQ.1)then
            LF=ZOBS(I)  ! if obs is separate file
          elseif(J.eq.5.and.ITW(I).EQ.1)then
            LF=LTWIN(I)
          elseif(J.eq.6.and.IVF(I).EQ.1)then
            LF=LVIEW(I)
          elseif(J.eq.7.and.ICGC(I).EQ.1)then
            LF=LCGCIN(I)
          elseif(J.eq.8.and.ISI(I).EQ.1)then
            LF=LSHAD(I)  ! binary shading file
          elseif(J.eq.9.and.ISI(I).EQ.1)then
            write(LF,'(2a)')LSHAD(I)(1:lnblnk(LSHAD(I))),'.a'  !ascii version
            call FINDFIL(LF,XST)
            if(XST)then
              continue ! it was found
            else
              LF='UNKNOWN'
            endif
          elseif(J.eq.10.and.IHC(I).EQ.1)then
            LF=LHCCO(I)
          endif
          if(LF(1:4).eq.'UNKN'.or.LF(1:2).eq.'  ')then
            continue
          else
            lc=lnblnk(zonepth)
            if(zonepth(1:lc).eq.LF(1:lc))then
              if(zonepth(1:3).eq.'../')then
                if(.NOT.pathome)then
                  write(IUNIT,'(5a)',IOSTAT=IOS,ERR=2) tr(1:ltr),fs,
     &              zonepth(4:lc),fs,LF(lc+2:lnblnk(LF))
                else
                  write(IUNIT,'(5a)',IOSTAT=IOS,ERR=2)  tp(1:ltp),fs,
     &              zonepth(1:lc),fs,LF(lc+2:lnblnk(LF))
                endif
              elseif(zonepth(1:2).eq.'./')then
                write(IUNIT,'(5a)',IOSTAT=IOS,ERR=2) tr(1:ltr),fs,
     &            zonepth(3:lc),fs,LF(lc+1:lnblnk(LF))
              endif
            else
              call wtarentry(iunit,pathome,tr,ltr,tp,ltp,LF)
            endif
          endif
 921    continue

C Write zone BASESIMP description if known.
        if(iBSIMP(i).eq.0)then
          continue
        else
          LF=LBSIMP(I)
          call wtarentry(iunit,pathome,tr,ltr,tp,ltp,LF)
        endif

C Write zone CFD file if known.
        if(lnblnk(LCFD(I)).eq.0)then
          continue
        elseif(LCFD(I)(1:7).eq.'UNKNOWN'.or.LCFD(I)(1:2).eq.'  ')then
          continue
        else
          LF=LCFD(I)
          lc=lnblnk(zonepth)
          if(zonepth(1:lc).eq.LF(1:lc))then
            if(zonepth(1:3).eq.'../')then
              if(.NOT.pathome)then
                write(IUNIT,'(5a)',IOSTAT=IOS,ERR=2) tr(1:ltr),fs,
     &            zonepth(4:lc),fs,LF(lc+2:lnblnk(LF))
              else
                write(IUNIT,'(5a)',IOSTAT=IOS,ERR=2) tp(1:ltp),fs,
     &            zonepth(1:lc),fs,LF(lc+2:lnblnk(LF))
              endif
            elseif(zonepth(1:2).eq.'./')then
              write(IUNIT,'(5a)',IOSTAT=IOS,ERR=2) tr(1:ltr),fs,
     &         zonepth(3:lc),fs,LF(lc:lnblnk(LF))
            endif
          else
            call wtarentry(iunit,pathome,tr,ltr,tp,ltp,LF)
          endif
        endif

C The following write statements do not detect the file location
        if(ZONE3D(I))then
          LF=L3DCVS(I)
          call wtarentry(iunit,pathome,tr,ltr,tp,ltp,LF)
          LF=L3DCNC(I)
          call wtarentry(iunit,pathome,tr,ltr,tp,ltp,LF)
          LF=L3DNDC(I)
          call wtarentry(iunit,pathome,tr,ltr,tp,ltp,LF)
          LF=L3DTAQ(I)
          call wtarentry(iunit,pathome,tr,ltr,tp,ltp,LF)
        endif
        if(MSTRZN(I))then
          LF=LMOIST(I)
          call wtarentry(iunit,pathome,tr,ltr,tp,ltp,LF)
        endif
        if(IndxSt.EQ.I)then
          call wtarentry(iunit,pathome,tr,ltr,tp,ltp,LGrdSt)
        endif
 920  continue

C Flow network.
      if(IAIRN.EQ.1)then
        LF=LAPROB
        lc=lnblnk(netpth)
        if(netpth(1:lc).eq.LF(1:lc))then
          if(netpth(1:3).eq.'../')then
            if(.NOT.pathome)then
              write(IUNIT,'(5a)',IOSTAT=IOS,ERR=2) tr(1:ltr),fs,
     &          netpth(4:lc),fs,LF(lc+2:lnblnk(LF))
            else
              write(IUNIT,'(5a)',IOSTAT=IOS,ERR=2) tp(1:ltp),fs,
     &          netpth(1:lc),fs,LF(lc+2:lnblnk(LF))
            endif
          elseif(netpth(1:2).eq.'./')then
            write(IUNIT,'(5a)',IOSTAT=IOS,ERR=2) tr(1:ltr),fs,
     &       netpth(3:lc),fs,LF(lc:lnblnk(LF))
          endif
        else
          call wtarentry(iunit,pathome,tr,ltr,tp,ltp,LF)
        endif
      endif
      if(INDCFG.EQ.2.or.INDCFG.EQ.3)then

C Plant network information.
        LF=LPNF
        lc=lnblnk(netpth)
        if(netpth(1:lc).eq.LF(1:lc))then
          if(netpth(1:3).eq.'../')then
            if(.NOT.pathome)then
              write(IUNIT,'(5a)',IOSTAT=IOS,ERR=2) tr(1:ltr),fs,
     &          netpth(4:lc),fs,LF(lc+2:lnblnk(LF))
            else
              write(IUNIT,'(5a)',IOSTAT=IOS,ERR=2) tp(1:ltp),fs,
     &          netpth(1:lc),fs,LF(lc+2:lnblnk(LF))
            endif
          elseif(netpth(1:2).eq.'./')then
            write(IUNIT,'(5a)',IOSTAT=IOS,ERR=2) tr(1:ltr),fs,
     &        netpth(3:lc),fs,LF(lc:lnblnk(LF))
          endif
        else
          call wtarentry(iunit,pathome,tr,ltr,tp,ltp,LF)
        endif
      endif
      call ERPFREE(IUNIT,ISTAT)

C Depending on current folder invoke tar with list of file names and
C create a tar file which includes root name of the project and the
C current pid number (to prevent overwriting).
C Use ifdef compiler variable passed into Make to identify which
C tar option to use. Most typical is -T but Sun uses -I.
      option=' -T '
#ifdef SUN
      option=' -I '
#endif

C Unix/Linux/Cygwin versions of tar.
      call esppid(ipid)
      write(LFIL,'(a,i7,a)') cfgroot(1:lnblnk(cfgroot)),ipid,'.tar'
      call st2file(LFIL,LF)
      if(patdot)then
        write(doit,'(4a)',IOSTAT=IOS,ERR=3) 'cd ../../; tar cf ',
     &    LF(1:lnblnk(LF)),option,LTMPFL(1:lnblnk(LTMPFL))
        call usrmsg('creating archive via',doit,'-')
        call runit(doit,'-')
      endif
      if(patcfg)then
        write(doit,'(4a)',IOSTAT=IOS,ERR=3) 'cd ../; tar cf ',
     &    LF(1:lnblnk(LF)),option,LTMPFL(1:lnblnk(LTMPFL))
        call usrmsg('creating archive via',doit,'-')
        call runit(doit,'-')
      endif
      if(patroot)then
        write(doit,'(4a)',IOSTAT=IOS,ERR=3) 'cd ./; tar cf ',
     &    LF(1:lnblnk(LF)),option,LTMPFL(1:lnblnk(LTMPFL))
        call usrmsg('creating archive via',doit,'-')
        call runit(doit,'-')
      endif
      call usrmsg(' ',' ','P')

      return

    2 if(IOS.eq.2)then
        CALL USRMSG('Permission issue writing data',' ','W')
      else
        CALL USRMSG('Problem writing data',' ','W')
      endif
      call ERPFREE(IUNIT,ISTAT)
      return
    3 if(IOS.eq.2)then
        CALL USRMSG('Permission issue writing archive command.',' ','W')
      else
        CALL USRMSG('Problem writing archive command.',' ','W')
      endif
      return
      end
