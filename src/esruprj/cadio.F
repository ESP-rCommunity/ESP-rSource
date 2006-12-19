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

      COMMON/SPAD/MMOD,LIMIT,LIMTTY
      common/OUTIN/IUOUT,IUIN
      common/pophelp/h(60)
      COMMON/C2/LSNAM,NCCODE(MCOM),LPROJ(MCOM),LGEOM(MCOM),
     &          LSHAD(MCOM),LTHRM(MCOM),INDUTL(MCOM),LUTIL(MCOM)
      common/C21/IFCFG,cfgroot,LCFGF
      common/rpath/path
      common/appw/iappw,iappx,iappy

      common/FOPENED/CFGOK,MLDBOK,CONDBOK,CTLOK,OPTKOK
      COMMON/exporttg/xfile,tg,delim
      COMMON/exporttgi/ixopen,ixloc,ixunit
      COMMON/RAY3/MODIFY,MODLEN,MODBND

      logical OK,DOK,CFGOK,MLDBOK,CONDBOK,CTLOK,OPTKOK,concat
      logical MODIFY,MODLEN,MODBND,unixok

      DIMENSION ITEMS1(11)

      character LFIL*72,H*72,path*72,LCFGF*72,tfile*72
      character ITEMS1*34,doit*248,outs*124,cfgroot*24,tmode*12
      character xfile*72,tg*1,delim*1,ffile*72
      CHARACTER*72 LSNAM,LPROJ,LGEOM,LSHAD,LTHRM,LUTIL
      character ETEXT*82
      character longtfile*144,longtfiledos*144
      logical oke

      if(.NOT.cfgok)then
        call usrmsg(' You must load a model before',
     &              ' you can export it!','W')
         return
      endif

C Check if Unix-based or DOS based.
      call isunix(unixok)

   33 INO1=-4
      ITEMS1(1) ='a to VIEWER (hidden line)  '
      ITEMS1(2) ='b to DXF (CAD export file) '
      ITEMS1(3) ='c to ZIP (CAD modeller)    '
      ITEMS1(4) ='d to RADIANCE              '
      ITEMS1(5) ='e to xfig (vector ploting) '
      ITEMS1(6) ='f to THF (THINGS file)     '
      ITEMS1(7) ='g to TSBI3                 '
      ITEMS1(8) ='h to EnergyPlus            '
      ITEMS1(9) ='i to VRML 2.0              '
      ITEMS1(10) ='? help'
      ITEMS1(11) ='- exit this menu'

C Help text for this menu.
      H(1)= 'The EXPORT facility allows you to take the current'
      H(2)= 'model definition and export information in a format'
      H(3)= 'as required by the target applications.'
      H(4)= ' '
      H(5)= 'Currently supported targets include:'
      H(6)= ' VIEWER (a hidden line perspective program)'
      H(7)= ' DXF files'
      H(8)= ' ZIP files'
      H(9)= ' RADIANCE (a lighting simulation package)'
      H(10)=' TSBI3 (a Danish simulation package)'
      H(11)=' '
      H(12)=' VRML (Version 2.0 with a matching html file which'
      H(13)='   holds surface attributes'
      H(14)=' '
      h(15)='It is possible to export the wireframe image of the'
      h(16)='model to a vector file for use in the xfig application.'
 
      CALL EMENU('Exports',ITEMS1,11,INO1)

C Prepare filter command lines with path.

C << the code that creates the initial LFIL is not yet updated to work with DOS paths with spaces. >>
      iw=0
      if(path.ne.'./'.and.path.ne.' ')then
        write(outs,'(A,A)') ' The current path is: ',path
        call edisp(iuout,outs)
        CALL EASKAB(' You are in a remote foldere.  Place the',
     &   ' export file(s):','using current path','in local folder',
     &   IW,0)
      endif
      if(iw.eq.1)then
        write(LFIL,'(a)') path(1:lnblnk(path))
      else
        LFIL='./'
      endif
      IF(INO1.EQ.1)THEN

C Construct a 'VIEWER' format file.  First get file name.
        call tstamp('>','PRJ: export to viewer')
        doit = ' '
        H(1)='A VIEWER format file may be used with the'
        H(2)='hidden line perspective program VIEWER.'
        write(LFIL,'(2a)') cfgroot(1:lnblnk(cfgroot)),'.vew'
        CALL EASKS(LFIL,' File name for image data ? ',
     &    ' ',72,' ','VIEWER input file',IER,2)
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
     &      ' -of viewer -out ',LFIL(1:lnblnk(LFIL))
        elseif(itrc.gt.1)then
          write(doit,'(4a)') 'ecnv -v -obs -if esp -in ',
     &       longtfile(1:lnblnk(longtfile)),
     &      ' -of viewer -out ',LFIL(1:lnblnk(LFIL))
        endif
        call usrmsg('Starting conversion via',doit,'-')
        tmode = 'text'
        call runit(doit,tmode)
        write(outs,'(3a)')' The file ',LFIL(1:lnblnk(LFIL)),
     &    ' has been created.'
        call edisp(iuout,outs)
      ELSEIF(INO1.EQ.2)THEN

C DXF creation.  Process similar to ecnv filter for VIEWER.
        call tstamp('>','PRJ: export to DXF')
        doit = ' '
        H(1)='A DXF file may be passed to some CAD packages,'
        H(2)='the current target is AutoCAD V12.'
        write(LFIL,'(2a)') cfgroot(1:lnblnk(cfgroot)),'.DXF'
        CALL EASKS(LFIL,' DXF file name ? ',
     &    ' ',72,' ','dxf export file',IER,2)
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
     &      ' -of dxf -out ',LFIL(1:lnblnk(LFIL))
        elseif(itrc.gt.1)then
          write(doit,'(4a)') 'ecnv -v -obs -if esp -in ',
     &       longtfile(1:lnblnk(longtfile)),
     &      ' -of dxf -out ',LFIL(1:lnblnk(LFIL))
        endif
        call usrmsg('Starting conversion via',doit,'-')
        tmode = 'text'
        call runit(doit,tmode)
        write(outs,'(3a)')' The file ',LFIL(1:lnblnk(LFIL)),
     &    ' has been created.'
        call edisp(iuout,outs)
      ELSEIF(INO1.EQ.3)THEN

C XZIP creation.  Process similar to ecnv filter for VIEWER.
        call tstamp('>','PRJ: export to xzip')
        doit = ' '
        H(1)='A ZIP file may be passed to the XZIP CAD package.'
        write(LFIL,'(2a)') cfgroot(1:lnblnk(cfgroot)),'_zip'
        CALL EASKS(LFIL,' XZIP file name ? ',
     &    ' ',72,' ','zip export file',IER,1)
        call edisp(iuout,' Performing model conversion.')
        call tchild(ICPMOD)
        call termode(ICPMOD,tmode)
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
     &       longtfile(1:lnblnk(longtfile)),
     &      ' -of zip -out ',LFIL(1:lnblnk(LFIL))
        elseif(itrc.gt.1)then
          write(doit,'(4a)') 'ecnv -v -if esp -in ',
     &       longtfile(1:lnblnk(longtfile)),
     &      ' -of zip -out ',LFIL(1:lnblnk(LFIL))
        endif
        call usrmsg('Starting conversion via',doit,'-')
        call runit(doit,tmode)
        write(outs,'(3a)')' The file ',LFIL(1:lnblnk(LFIL)),
     &    ' has been created.'
        call edisp(iuout,outs)
      ELSEIF(INO1.EQ.4)THEN

C RADIANCE creation.  Starts e2r with configuration file.
        H(1)='If your model includes zone or ground geometry'
        H(2)='of solar obstructions you can visualise them via'
        H(3)='the ESP-r Radiance desktop.'
        H(4)='It is assumed that your computer has Radiance setup'
        H(5)='and the Radiance modules are in your PATH. '
        dok=.true.
        CALL ASKOK(' ',' Run the RADIANCE desktop? ',OK,dok,5)
        if(OK)then
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
            write(doit,'(a,3i4,3a)') 'e2r -s ',
     &        iappw,iappx+35,iappy+40,' -file ',
     &        longtfile(1:lnblnk(longtfile)),' &'
          else
            write(doit,'(3a)') 'e2r -s 0 0 0 -file ',
     &        longtfile(1:lnblnk(longtfile)),' &'
          endif
          call usrmsg('Starting Radiance conversion via',doit,'-')
          call runit(doit,'-')
        endif
      ELSEIF(INO1.EQ.5)THEN

C Toggle output redirection to xfig.
        h(1)='It is possible to redirect the wireframe image of the'
        h(2)='model to a vetor file for use in the xfig application. '
        h(3)='Before exporting adjust the model view and then '
        h(4)='resize graphic window slightly to force redraw.'
        h(5)=' '
        h(6)='Use this facility if you have xfig on your machine. '
        dok=.true.
        call askok('Is the wireframe composition ok?',
     &    '(last chance to ajust it before export to xfig)',oke,dok,6)
        if(.NOT.oke)then
          call usrmsg('Returning to menu. Adjust wireframe image',
     &      'and then re-select this option.','W')
          goto 33
        endif
        if(ixopen.eq.1)then
          call ctlexp(xfile,ixopen,ixloc,ixunit,'T','geom text',IER)
        else
          CALL EASKABC('Feedback redirection:',' ','graphic',
     &                                            'text','cancel',IW,4)
          if (IW.eq.1.and.MMOD.eq.8) then
            write(xfile,'(2a)') cfgroot(1:lnblnk(cfgroot)),'_wire'
            call ctlexp(xfile,ixopen,ixloc,ixunit,'G','wire frame',IER)
            call usrmsg('sending wireframe... ',' ','-')
            MODIFY=.TRUE.
            CALL ADJVIEW(ITRC,IER)
            call cfgtogg(icfg_type,icfgz,icfgn,icfgc,icfgdfn,
     &                               iicfgz,iicfgn,iicfgc,iicfgdfn)
            call opencfg(icfg_type,icfgz,icfgn,icfgc,icfgdfn,
     &                               iicfgz,iicfgn,iicfgc,iicfgdfn)
            WRITE(ETEXT,'(2A)')'Project: ',LSNAM(1:lnblnk(LSNAM))
            CALL viewtext(ETEXT,1,1,1)
            call usrmsg('sending wireframe...done.',' ','-')
            call ctlexp(xfile,ixopen,ixloc,ixunit,'G','Wire frame',IER)
            MODIFY=.TRUE.

C Check if conversion to xfig wanted.
            dok=.true.
            h(1)='Assuming the export was sucessful you can proceed.'
            call askok(' ','Convert output to xfig format?',ok,dok,1)
            if(ok)then
              write(ffile,'(a,a4)') xfile(1:lnblnk(xfile)),'.fig'
              doit = ' '
              write(doit,'(4a)') 'ecnv -if ww -in ',
     &               xfile(1:lnblnk(xfile)),' -of xfig -out ',
     &               ffile(1:lnblnk(ffile))
              call usrmsg('Starting xfig conversion via',doit,'-')
              call runit(doit,'-')
            endif
          elseif (IW.eq.2) then
            call ctlexp(xfile,ixopen,ixloc,ixunit,'T','geom text',IER)
          endif
        endif
      ELSEIF(INO1.EQ.6)THEN

C Export to THINGS (MicroGDS THF) file. Treat obstructions as clusters.
        call tstamp('>','PRJ: export to THINGS')
        doit = ' '
        H(1)='A THF (THINGS) format file may be used with the'
        H(2)='CAD package MicroGDS.'
        h(3)='Note that obstructions are treated as clusters of '
        h(4)='surfaces. '
        write(LFIL,'(2a)') cfgroot(1:lnblnk(cfgroot)),'.THF'
        CALL EASKS(LFIL,' File name for THF CAD data ? ',
     &    ' ',72,' ','THF file',IER,4)
        call edisp(iuout,' Performing model conversion.')

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
      ELSEIF(INO1.EQ.7)THEN

C Export to TSBI3 (Danish simulation tool) file.
        call tstamp('>','PRJ: export to TSBI3')
        doit = ' '
        H(1)='A TSBI3 format file may be used with the simulation'
        H(2)='package TSBI3 .'
        h(3)='Note that zone schedules are not fully translated.'
        h(4)=' '
        write(LFIL,'(2a)') cfgroot(1:lnblnk(cfgroot)),'.TSBI3'
        CALL EASKS(LFIL,' File name for TSBI3 file ? ',
     &    ' ',72,' ','TSBI3 file',IER,4)
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
          write(doit,'(4a)') 'ecnv -if esp -in ',
     &      longtfile(1:lnblnk(longtfile)),
     &      ' -of tsbi3 -out ',LFIL(1:lnblnk(LFIL))
        elseif(itrc.gt.1)then
          write(doit,'(4a)') 'ecnv -v -if esp -in ',
     &      longtfile(1:lnblnk(longtfile)),
     &      ' -of tsbi3 -out ',LFIL(1:lnblnk(LFIL))
        endif
        call usrmsg('Starting TSBI3 conversion via',doit,'-')
        tmode = 'text'
        call runit(doit,tmode)
        write(outs,'(3a)')' The file ',LFIL(1:lnblnk(LFIL)),
     &    ' has been created.'
        call edisp(iuout,outs)
      ELSEIF(INO1.EQ.8)THEN

C Export to EnergyPlus IDF file (work in progress)
        call tstamp('>','PRJ: export to energyplus')
        doit = ' '
        H(1)='A EnergyPlus IDF (input data file) can be created from'
        H(2)=' '
        H(3)=' '
        write(LFIL,'(2a)') cfgroot(1:lnblnk(cfgroot)),'.idf'
        CALL EASKS(LFIL,' EnergyPlus IDF file name ? ',
     &    ' ',72,' ','EnergyPlus export file',IER,1)
        call edisp(iuout,' Performing model conversion.')
        ioout=2
        open(ioout,file=lfil,status='UNKNOWN',err=901)
        call e2eplus(2,ioout,ier)
        close(ioout)
      ELSEIF(INO1.EQ.9)THEN

C Export to VRML (2.0) file.
        call tstamp('>','PRJ: export to VRML')
        doit = ' '
        H(1)='A VRML format file may be used with a number of'
        H(2)='3rd party packages and web browser plug-ins'
        h(3)='to visually explore a model.'
        h(4)=' '
        write(LFIL,'(2a)') cfgroot(1:lnblnk(cfgroot)),'.vrml'
        CALL EASKS(LFIL,' File name for VRML file ? ',
     &    ' ',72,' ','vrml file',IER,4)
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
          write(doit,'(4a)') 'ecnv -if esp -in ',
     &      longtfile(1:lnblnk(longtfile)),
     &      ' -of vrml -out ',LFIL(1:lnblnk(LFIL))
        elseif(itrc.gt.1)then
          write(doit,'(4a)') 'ecnv -v -if esp -in ',
     &      longtfile(1:lnblnk(longtfile)),
     &      ' -of vrml -out ',LFIL(1:lnblnk(LFIL))
        endif
        call usrmsg('Starting VRML conversion via',doit,'-')
        tmode = 'text'
        call runit(doit,tmode)
        write(outs,'(3a)')' The file ',LFIL(1:lnblnk(LFIL)),
     &    ' has been created.'
        call edisp(iuout,outs)
        call edisp(iuout,'Use a VRML viewer to explore it.')
      ELSEIF(INO1.EQ.10)THEN
       CALL PHELPD('export menu',16,'-',0,0,IER)
      ELSEIF(INO1.EQ.11)THEN
        RETURN
      else
        goto 33
      endif
      goto 33

C Error.
 901  write(6,*) 'Unable to open IDF file ',LFIL,' on ',ioout
      return
     
      END

C ******** cadin
C CADIN drives the conversion of CAD input into data model.
C used by the Project Manager.
C Parameter ichoice is currently not used.
      subroutine cadin(itrc,ichoice,ier)
      common/OUTIN/IUOUT,IUIN
      common/FILEP/IFIL
      common/C1/NCOMP,NCON
      common/C21/IFCFG,cfgroot,LCFGF
      common/rcmd/LCMDFL

C Indicator of possible focus zone.
      common/user/browse

C External cad package.
      common/cad3rd/cadlbl,cadexe,cadfmt

      common/pophelp/h(60)
      LOGICAL     OK,DOK,concat,browse,typez,confirm,ckpath,unixok

      character LCFGF*72,H*72,LFIL*72,LCMDFL*144
      character outs*124,OUTSTR*124,cfgroot*24,doit*248,tfile*72
      character cadlbl*20,cadexe*20,cadfmt*4
      character longtfile*144,longtfiledos*144

C Set file unit and check if running Unix-like OS.
      ifun=ifil+1
      call isunix(unixok)

C CAD definition and importing.
      H(1)='Executing CAD program by which the zone geometry'
      H(2)='may be defined.  The file created will be requested'
      H(3)='later for model importing.'
      H(4)=' '
      H(5)='If the CAD program creates DXF files then PLEASE'
      H(6)='refer to the tutorial tool for directions on'
      H(7)='appropriate use and current constraints.'
      H(8)=' '
      H(9)='If the CAD program is able to create ESP compatable'
      H(10)='files then please assign zone names so that the'
      H(11)='conversion process will proceed smoothly.'
      H(12)=' '
      if(NCOMP.ne.0)then

C Tell the user to create a model configuration file. If the model is
C not yet known, check to see if a model has been passed in the
C command line.
        H(13)='The current model includes zones.  Please make'
        H(14)='careful note of any changes you make to the model'
        H(15)='so that you can restore contiguity when importing'
        H(16)='from the CAD package.'
        CALL PHELPD('CAD warning',16,'-',0,0,IER)
      else
        CALL PHELPD('CAD warning',11,'-',0,0,IER)
      endif

C If there is a currently loaded model then convert it.
      if(NCOMP.gt.0.and.cadfmt(1:3).eq.'DXF')then
        call tstamp('>','PRJ: start CAD')
        doit = ' '
        H(1)='A DXF file is required by this CAD package please,'
        H(2)='supply a name.'
        CALL EASKS(LFIL,' Put current model into DXF file named: ',
     &    ' ',72,' ','dxf export file',IER,2)
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
      elseif(NCOMP.gt.0.and.cadfmt(1:3).eq.'ZIP')then
        doit = ' '
        H(1)='A ZIP file may be passed to the XZIP CAD package.'
        CALL EASKS(LFIL,' Put current model into ZIP file named: ',
     &    ' ',72,' ','zip export file',IER,1)
        call edisp(iuout,' Performing model conversion.')
        if(unixok)then
          call addpath(LCFGF,longtfile,concat)
        else
          call addpath(LCFGF,longtfile,concat)
          call cmdfiledos(longtfile,longtfiledos,ier)
          longtfile=' '
          longtfile=longtfiledos
        endif
        if(itrc.le.1)then
          write(doit,'(4a)') 'ecnv -if esp -in ',
     &      longtfile(1:lnblnk(longtfile)),
     &      ' -of zip -out ',LFIL(1:lnblnk(LFIL))
        elseif(itrc.gt.1)then
          write(doit,'(4a)') 'ecnv -v -if esp -in ',
     &      longtfile(1:lnblnk(longtfile)),
     &      ' -of zip -out ',LFIL(1:lnblnk(LFIL))
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
      h(1)='Assuming that the 3rd party tool is installed on your '
      h(2)='machine it should be ok to proceed. If you just want to'
      h(3)='generate the file for the tool say no at this point. '
      CALL ASKOK(' ',outs,OK,dok,3)
      IF(OK)then

C << might do a cd to the current configuration location? before running zip >>
        if(cadfmt(1:3).eq.'ZIP')then
          if(NCOMP.gt.0)then
            write(doit,'(a,2x,a)') cadexe,LFIL(1:lnblnk(LFIL))
            call runit(doit,'graph')
          else
            call runit(cadexe,'graph')
          endif
        elseif(cadfmt(1:3).eq.'DXF')then
          call runit(cadexe,'graph')
        endif
      endif

 28   dok=.true.
      h(1)='Assuming that the 3rd party tool generated a file and you'
      h(2)='know the location of that files it should be ok to proceed.'
      CALL ASKOK(' ',' Read the transfer file?',OK,dok,2)
      IF(OK)then
        H(1)='A DXF format file may be converted if it has been'
        H(2)='created using layer and entity constraints.'
        h(3)=' '
        h(4)='If the file is from `xzip` supply its name (if you'
        h(5)='saved the zip file in the folder that the CAD tool'
        h(6)='suggested you will not need to give the path). '
        CALL EASKS(LFIL,' File to import ? ',
     &    ' ',72,' ','cad import file',IER,6)

C Read the first couple of lines of the import file to see if it
C is a ZIP or DXF file.
        CALL ERPFREE(ifun,ISTAT)
        call FPOPEN(ifun,ISTAT,1,0,LFIL)
        IF(ISTAT.LT.0)THEN
          write(outs,'(a,a)')'Could not find: ',LFIL(1:lnblnk(LFIL))
          dok=.false.
          h(1)='Ouch, ESP-r did not find the file in the location you'
          h(2)='specified. Double check before trying again. '
          call askok(outs,'(check location) Try again?',OK,dok,2)
          if(OK)then
            goto 28
          else
            return
          endif
        endif
        CALL STRIPC(ifun,OUTSTR,0,ND,1,'CAD first line',IER)
        if(OUTSTR(1:8).eq.'COM data')then
          typez=.true.
          call edisp(iuout,'Based on initial line of file:')
          call edisp(iuout,outstr)
          call edisp(iuout,'this file is assumed to be a ZIP file.')
        else
          CALL STRIPC(ifun,OUTSTR,0,ND,1,'CAD 2nd line',IER)
          if(OUTSTR(1:7).eq.'SECTION')then
            typez=.false.
            call edisp(iuout,'Based on 2nd line of file:')
            call edisp(iuout,outstr)
            call edisp(iuout,'this file is assumed to be a DXF file.')
          endif
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
        if(typez)then
          if(itrc.le.1)then
            write(doit,'(4a)') 'ecnv -if zip -in ',
     &        LFIL(1:lnblnk(LFIL)),' -of esp -u upgrade -out ',
     &        longtfile(1:lnblnk(longtfile))
          elseif(itrc.gt.1)then
            write(doit,'(4a)')'ecnv -v -if zip -in ',
     &        LFIL(1:lnblnk(LFIL)),' -of esp -u upgrade -out ',
     &        longtfile(1:lnblnk(longtfile))
          endif
        else
          if(itrc.le.1)then
            write(doit,'(4a)') 'ecnv -obs -mm -if dxf -in ',
     &        LFIL(1:lnblnk(LFIL)),' -of esp -u upgrade -out ',
     &        longtfile(1:lnblnk(longtfile))
          elseif(itrc.gt.1)then
            write(doit,'(4a)')'ecnv -v -obs -mm -if dxf -in ',
     &        LFIL(1:lnblnk(LFIL)),' -of esp -u upgrade -out ',
     &        longtfile(1:lnblnk(longtfile))
          endif
        endif
        call usrmsg('doing conversion via',doit,'-')
        call runit(doit,'-')

C Re-read the upgraded configuration file. Reconstruct the path.
        browse=.false.
        confirm=.true.
        ckpath=.false.
        LCMDFL=longtfile
        call NEWPRB(ITRC,confirm,ckpath,IER)
      endif
      return
      end

C ********* rexmpl
C rexmpl: scan the exemplars file, parse information and offer the user
C selection of or information about exemplars. 
      subroutine rexmpl(iexfil,exfil,exemplar,ier)
      COMMON/SPAD/MMOD,LIMIT,LIMTTY
      common/OUTIN/IUOUT,IUIN
      common/pophelp/h(60)
      common/C1/NCOMP,NCON
      common/C6/INDCFG
      common/uhome/upath

C Images - included to warn user on subsequent browsing sessions
C to quit the image browser.
      common/IMAGF/imgfmt(10),imgfoc(10),limgfil(10),noimg,iton

      dimension name(30),mitem(30),label(30),inl2in(30),group(15)
      dimension gitem(18),gh(13),cfgitem(30),inl2ngr(30)
      character name*70,label*42,mitem*72,gitem*44,OUTSTR*124,WORD*20
      character exfil*72,group*42,tgroup*42,key*1,look*70,tlook*70
      character exemplar*144,line*72,H*72,outs*124,rootp*72,separ*42
      character imgfmt*4,imgfoc*4,limgfil*72,doit*248,upath*72
      character gh*72,cfgitem*144,exdescr*81,doc*72,head*42
      character cfg*144,cfg_path*72,cfg_root*72,doc_file*72

C Strings for dos version of rootp and upath and a double quote.
      character rootpdos*72,upathdos*72,rootpath*72,modelroot*72
      character dq*1,fs*1,thecfgis*72
      logical XST,unixok

C Make up a double quote and file separator for xcopy.
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
      if(exfil(1:1).eq.'/')then
        call FPOPEN(iexfil,ISTAT,1,0,exfil)
      else
        CALL EFOPSEQ(iexfil,exfil,1,IER)
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
C Clear group related help.
      do 2 i=1,13
        gh(i)=' '
  2   continue
      exemplar='UNKNOWN'
      ngr=0
      ing=0
      inl=0
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
      h(1)='These exemplar sets are available for browsing.'
      h(2)='Each set comprises a number of designs which'
      h(3)='relate to different possible uses of ESP-r.'
      h(4)=' '
      h(5)='Exemplar designs are used for training purposes'
      h(6)='or as a record of interesting designs which'
      h(7)='might serve as the starting point in some'
      h(8)='future project.'
      h(9)=' '
      h(10)='Note to Administrators: the `exemplars` file must'
      h(11)='be kept up-to-date by appending new designs using'
      h(12)='the key words explained in the exemplar file.'

      if(MMOD.EQ.8)then
        CALL VWMENU('Exemplar Sets',gitem,NCOG,0,0,IW,irpx,irpy,ICOG)
      else
        CALL EMENU('Exemplar Sets',gitem,NCOG,ICOG)
      endif
      IF(ICOG.EQ.NCOG)THEN
        CALL ERPFREE(iexfil,ISTAT)
        return
      ELSEIF(ICOG.EQ.(NCOG-1))THEN

C Help @ db file level.
        CALL PHELPD('exemplar menu',12,'-',0,0,IER)
      ELSEIF(ICOG.GT.0.AND.ICOG.LT.(NCOG-2))THEN
        if(gitem(ICOG)(1:2).eq.'  ')goto 142
        if(inl2ngr(ICOG).gt.0.and.inl2ngr(ICOG).le.15)then
          igrsel=inl2ngr(ICOG)
        else
          goto 142
        endif
C Re-open the exemplar file.
        if(exfil(1:1).eq.'/')then
          call FPOPEN(iexfil,ISTAT,1,0,exfil)
        else
          CALL EFOPSEQ(iexfil,exfil,1,IER)
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

C Help text for this menu.
      h(1)='To browse one of the exemplar designs, simply'
      h(2)='select it.  If you wish to modify the model'
      h(3)='then you must first copy it into your folder.'
      h(4)=' '
      if(nghelp.gt.0)then
        h(5)='The current set of exemplars: '
        do 33 ih=1,nghelp
          lex=MIN0(lnblnk(gh(ih)),72)
          write(h(5+ih),'(a)')gh(ih)(1:lex)
 33     continue
      endif

      if(MMOD.EQ.8)then
        CALL VWMENU(head,mitem,NCO,0,0,IWE,irpx,irpy,ICO)
      else
        CALL EMENU(head,mitem,NCO,ICO)
      endif
      IF(ICO.EQ.NCO)THEN

C Exit from detail level back to group level.
        CALL ERPFREE(iexfil,ISTAT)
        goto 41
      ELSEIF(ICO.EQ.(NCO-1))THEN

C Help @ db file level.
        if(nghelp.gt.0)then
          CALL PHELPD('exemplar menu',5+nghelp,'-',0,0,IER)
        else
          CALL PHELPD('exemplar menu',3,'-',0,0,IER)
        endif
      ELSEIF(ICO.EQ.(NCO-2))THEN
        if(MMOD.EQ.8)then
          CALL VWMENU('Select exemplar...',mitem,NCO,0,0,IWE,irpx,irpy,
     &      ICOD)
        else
          CALL EMENU('Select exemplar...',mitem,NCO,ICOD)
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
             call usrmsg('The selected exemplar is currently',
     &            'off-line.  See your administrator for access.','W')
          endif
        endif
      ELSEIF(ICO.GE.1.AND.ICO.LT.(NCO-3))THEN

C Present information on an exemplar.
        if(inl2in(ICO).eq.0)goto 42
        look=name(inl2in(ICO))
        if(exfil(1:1).eq.'/')then
          call FPOPEN(iexfil,ISTAT,1,0,exfil)
        else
          CALL EFOPSEQ(iexfil,exfil,1,IER)
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
              call edisp(iuout,' Configuration is: ')
              call edisp(iuout,exemplar)

C If running on a windows machine get the last token within the string exemplar
C which is the name of the configuration file. Work backwards from the end
C of the string till a file separator of either DOS or Unix type is reached.
              call isunix(unixok)
              if(.NOT.unixok)then
                thecfgis=' '
                ilexem = lnblnk(exemplar)
                ipslash = ilexem
                ipos = ilexem
 344            continue
                ipos=ipos-1
                if(exemplar(ipos:ipos).eq.fs)then
                  write(thecfgis,'(a)') exemplar(ipos+1:ilexem)
                  call edisp(iuout,'The model configuration file is...')
                  call edisp(iuout,thecfgis)
                elseif(exemplar(ipos:ipos).eq.'/')then
                  write(thecfgis,'(a)') exemplar(ipos+1:ilexem)
                  call edisp(iuout,'The model configuration file is...')
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
              H(1)='When browsing a model you are prevented from'
              H(2)='altering it although you can use it to run'
              H(3)='simulations.  If you want to modify the model'
              H(4)='then you must firstly copy it to your own'
              H(5)='folder by selecting the `own it` option.'
              H(6)=' '
              H(7)='If you change your mind you can select a different'
              H(8)='rxemplar model or select continue to do nothing.'
              H(9)=' '
              H(10)='Note: ESP-r running native on Windows sometimes'
              H(11)='fails to copy an exemplar model to the users  '
              H(12)='home folder if the path has spaces in it. In'
              H(13)='this case you should use the operating system'
              H(14)='commands (e.g. zip or xcopy) to copy the model'
              H(15)='folders to a location where you can work on them.'
              CALL EASKABCD(exdescr,'(see help) ','browse',
     &          '`own` it','select another exemplar','continue',IW,15)
              if(IW.eq.1)then
                call tstamp('>','PRJ: browse exemplar')
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
                    write(doit,'(4a)') 'cp -r ',
     &                rootp(1:lnblnk(rootp)),' ',upath(1:lnblnk(upath))
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
                    lnup=lnblnk(upath)
                    lnblankup=iprevblnk(upath,lnup)
                    call backslashit(upath,upathdos)
C Debug.
C                    write(outs,'(a,2i3,2a)') 'upath = ',lnup,lnblankup,
C     &                ' ',upath(1:lnup)
C                    call edisp(iuout,outs)
C                    write(outs,'(2a)') 'destination = ',upathdos(1:lnup)
C                    call edisp(iuout,outs)
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
                  call lusrmsg('Copying exemplar to your home folder.',
     &               doit,'-')
                  call runit(doit,'-')
                  if(unixok)then

C If Unix-like then append tail of exemplar string to the users default
C path to get the new exemplar name.
                    write(exemplar,'(3a)')upath(1:lnblnk(upath)),fs,
     &                exemplar(ilex:lnblnk(exemplar))
                  else

C If Windows based then append the modelroot string to the path to make
C up the exemplar name.
                    lnmr=lnblnk(modelroot)
                    write(exemplar,'(7a)')upathdos(1:lnblnk(upathdos)),
     &                fs,modelroot(1:lnmr),fs,'cfg',fs,
     &                thecfgis(1:lnblnk(thecfgis))
                  endif
                  call edisp(iuout,'Your model (that you own) is now:')
                  call edisp(iuout,exemplar)
                  call tstamp('>','PRJ: owning exemplar')
                  call tstamp('>',exemplar)
                endif
              elseif(IW.eq.3)then

C User asked to select another, rewind the file and jump back to
C the top menu to re-resent groups.
                exemplar='UNKNOWN'
                REWIND(iexfil,ERR=92)
                goto 41
              elseif(IW.eq.4)then

C User asked to continue, close the file set ier and return to top
C level menu of prj.
                exemplar='UNKNOWN'
                CALL ERPFREE(iexfil,ISTAT)
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
      subroutine wtarentry(iunit,pathome,tr,ltr,tp,ltp,lf,ier)
      logical pathome,unixok
      character tp*32,tr*32,lf*72,fs*1

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
    2 CALL USRMSG(' Problem writing tar names entry',lf,'W')
      call ERPFREE(IUNIT,ISTAT)
      return

      end


C ***** archiveit
C archiveit: archives the current model by building up an "include" file to
C pass to tar archive. Detect where the current model is located
C and if the user is browsing the model.
C << to be done - then name file generated is never cleaned up >>
      subroutine archiveit(ier)
#include "building.h"
#include "uncertainty.h"
      common/OUTIN/IUOUT,IUIN
      common/FILEP/IFIL
      common/rpath/path
      common/uhome/upath
      COMMON/C1/NCOMP,NCON
      common/C2/LSNAM,NCCODE(MCOM),LPROJ(MCOM),LGEOM(MCOM),
     &          LSHAD(MCOM),LTHRM(MCOM),INDUTL(MCOM),LUTIL(MCOM)
      COMMON/C6/INDCFG
      COMMON/C7/LPCDB
      common/C21/IFCFG,cfgroot,LCFGF
      common/C22/ICLIM,LCLIM
      COMMON/C23/IFPNF,LPNF
      common/C3F/LCNN
      common/INDICS/IVF(MCOM),ISI(MCOM),IHC(MCOM),
     &              ITW(MCOM),ICGC(MCOM),IOBS(MCOM)
      common/UDESC/LVIEW(MCOM),LHCCO(MCOM),
     &             LTWIN(MCOM),LCGCIN(MCOM),ZOBS(MCOM)
      COMMON/AFN/IAIRN,LAPROB,LAPRES,LAFRES,ICAAS(MCOM)
      common/LOG/LPRJLG
      common/radcfg/LRADCF
      COMMON/BL1/dmdsdesc,bdmds
      common/cctlnm/ctldoc,lctlf
      COMMON/PRODB/LPRFDB,IPRODB
      COMMON/CONDB/LFCON,IFCON,LFMUL,IFMUL
      common/FOPENED/CFGOK,MLDBOK,CONDBOK,CTLOK,OPTKOK
      COMMON/GOPTDB/LOPTDB,IOPTDB
      COMMON/TDFFT/LTDF,ltdfa
      common/GTFIL/GTGEOM
      common/cfdfil/LCFD(MCOM),IFCFD(MCOM)
      common/rcmd/LCMDFL
      common/paths/zonepth,netpth,ctlpth,imgpth,radpth,docpth,tmppth,
     &             dbspth
      common/user/browse
      common/IPVF/lipvdatf
      COMMON/GR1D05/LGRD1D
      COMMON/GR1D06/IGR1D
      COMMON/GRND100/GRND3D
      COMMON/GRND108/LGDCVS,LGDCNC,LGDNDC,LGDTAQ,LGDNDD
      COMMON/GRSD100/IndxSt
      COMMON/GRSD101/LGrdSt
      common/GR3D100/BLDG3D,ZONE3D(MCOM)
      common/GR3D108/L3DCVS(MCOM),L3DCNC(MCOM),L3DNDC(MCOM),L3DTAQ(MCOM)
      COMMON/VTHP18/LNLTHP
      COMMON/VTHP31/INTHPS,INTHPZ(MCOM)
      common/MOIST01/MSTROK,MSTRZN(MCOM)
      common/MOIST02/LMOIST(MCOM)
      common/spmfxst/ispmxist,spflnam
      common/entfile/entflnam, ientxist
      COMMON/UA1/LUALF,LCNG(MNCNG),LLOC(MNIL)
      COMMON/IMAGF/imgfmt(10),imgfoc(10),limgfil(10),noimg,iton
      common/hvacinfo/ihvacflag,hvacfile
      common/bsmtdef/iBSIMP(MCOM),LBSIMP(MCOM)
      common/aim2def/iAIM2,LAIM2

      character LUALF*72,LCNG*15,LLOC*15,LCFGF*72
      character*72 LFMUL,LFCON,LPRJLG,LPNF,LMOIST
      character*72 LSNAM,LPROJ,LGEOM,LSHAD,LTHRM,LUTIL,LCNN
      character*72 LVIEW,LHCCO,LTWIN,LCGCIN,ZOBS,LCFD
      character LCMDFL*144,LFIL*72,doit*248
      character path*72,upath*72,cfgroot*24,curdir*72
      character*72 LCLIM,LAPROB,LAPRES,LAFRES,LPCDB,LPRFDB,LOPTDB
      CHARACTER*72 LGDCVS,LGDCNC,LGDNDC,LGDNDD,LGDTAQ,LGRD1D,LNLTHP
      character imgfmt*4,imgfoc*4,limgfil*72,tp*32,tr*32
      character ctldoc*248,LCTLF*72,lradcf*72,LGrdSt*72
      character LTDF*72,LTDFA*72,GTGEOM*72,lipvdatf*72
      character zonepth*24,netpth*24,ctlpth*24,imgpth*24,radpth*24,fs*1
      character docpth*24,tmppth*24,dbspth*24,LTMPFL*72,LF*72
      CHARACTER dmdsdesc*248,bdmds*72,spflnam*72,entflnam*72
      CHARACTER*72 L3DCVS,L3DCNC,L3DNDC,L3DTAQ,LASCI
      character hvacfile*72,LAIM2*72,LBSIMP*72

      logical browse,XST,unixok
      LOGICAL INTHPS,INTHPZ,IGR1D,GRND3D
      LOGICAL BLDG3D,ZONE3D,MSTROK,MSTRZN
      LOGICAL CFGOK,MLDBOK,CONDBOK,CTLOK,OPTKOK

C Patdot is true if path is ./, patcfg is true if path is cfg,
C patroot is true if path is at root level. 
      logical patdot,patcfg,patroot,pathome
      character WORDS(12)*32
C      character WORDS*32

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

C Set folder separator (fs) to \ or / as required.
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
          call edisp(iuout,'Removing existing name file. ')
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
        if(iaccess.eq.1)write(6,*) 'cannot write at',curdir(1:lcd)

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
      if(iaccess.eq.1)write(6,*) 'cannot write at',path(1:lnblnk(path))

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
          write(6,*) 'checking writing at ',LF(1:lnblnk(LF))
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
          write(6,*) 'checking writing at ',LF(1:lnblnk(LF))
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
      do 41 i=1,7
        if(i.eq.1)then
          LF=LFCON
        elseif(i.eq.2)then
          LF=LFMUL
        elseif(i.eq.3)then
          LF=LOPTDB
        elseif(i.eq.4)then
          LF=LAPRES
        elseif(i.eq.5)then
          LF=LPRFDB
        elseif(i.eq.6)then
          LF=LCLIM
        elseif(i.eq.7)then
          LF=LPCDB
        endif
        if(LF(1:9).ne.'/usr/esru'.and.LF(1:4).ne.'UNKN')then
          call wtarentry(iunit,pathome,tr,ltr,tp,ltp,LF,ier)

C Place to write out ascii version of constructions db if a local
C file exists.
          if(i.eq.1)then
            IUF=IFIL+2
            write(LASCI,'(2a)') LF(1:lnblnk(LF)),'.a'
            CALL MKAPRC(IUOUT,LASCI,IUF,IER)
            call wtarentry(iunit,pathome,tr,ltr,tp,ltp,LASCI,ier)
          endif
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
          call wtarentry(iunit,pathome,tr,ltr,tp,ltp,LCTLF,ier)
        endif
      endif

C Write HOT3000 hvac file if known.
      if(ihvacflag.eq.0)then
        continue
      else
        call wtarentry(iunit,pathome,tr,ltr,tp,ltp,hvacfile,ier)
      endif

C Write HOT3000 AIM-2 file if known.
      if(iAIM2.eq.0)then
        continue
      else
        call wtarentry(iunit,pathome,tr,ltr,tp,ltp,LAIM2,ier)
      endif

C Write uncertainty analysis library file if known.
      call wtarentry(iunit,pathome,tr,ltr,tp,ltp,LUALF,ier)

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
            call wtarentry(iunit,pathome,tr,ltr,tp,ltp,LF,ier)
          endif
 42     continue
      endif

C IPV file, if known.
      call wtarentry(iunit,pathome,tr,ltr,tp,ltp,lipvdatf,ier)

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
          call wtarentry(iunit,pathome,tr,ltr,tp,ltp,bdmds,ier)
        endif
      endif

      if(lradcf(1:4).eq.'UNKN'.or.lradcf(1:2).eq.'  ')then
        continue
      else
        call wtarentry(iunit,pathome,tr,ltr,tp,ltp,lradcf,ier)
      endif

C Non-linear thermophysical properties configuration file.
      if(INTHPS)then
        WRITE(6,'(2A)') '*nlt ',LNLTHP(1:lnblnk(LNLTHP))
      endif
      if(IGR1D)then
        WRITE(6,'(2A)') '*g1d ',LGRD1D(1:lnblnk(LGRD1D))
      endif
      if(spflnam(1:4).eq.'UNKN'.or.spflnam(1:2).eq.'  ')then
        continue
      else
        if(ispmxist.gt.0)then
          call wtarentry(iunit,pathome,tr,ltr,tp,ltp,spflnam,ier)
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
      call wtarentry(iunit,pathome,tr,ltr,tp,ltp,LPRJLG,ier)

C Ground information.
      IF(GRND3D)THEN
        call wtarentry(iunit,pathome,tr,ltr,tp,ltp,LGDCVS,ier)
        call wtarentry(iunit,pathome,tr,ltr,tp,ltp,LGDCNC,ier)
        call wtarentry(iunit,pathome,tr,ltr,tp,ltp,LGDNDC,ier)
        call wtarentry(iunit,pathome,tr,ltr,tp,ltp,LGDTAQ,ier)
      ENDIF

C Ground topology.
      call wtarentry(iunit,pathome,tr,ltr,tp,ltp,GTGEOM,ier)

C Write zone files.
      DO 920 I = 1,NCOMP
        do 921 J = 1,9
          LF='UNKNOWN'
          if(J.eq.1)then
            LF=LGEOM(I)
          elseif(J.eq.2)then
            LF=LPROJ(I)
          elseif(J.eq.3)then
            LF=LTHRM(I)
          elseif(J.eq.4.and.IOBS(I).EQ.1)then
            LF=ZOBS(I)
          elseif(J.eq.5.and.ITW(I).EQ.1)then
            LF=LTWIN(I)
          elseif(J.eq.6.and.IVF(I).EQ.1)then
            LF=LVIEW(I)
          elseif(J.eq.7.and.ICGC(I).EQ.1)then
            LF=LCGCIN(I)
          elseif(J.eq.8.and.ISI(I).EQ.1)then
            LF=LSHAD(I)
          elseif(J.eq.9.and.IHC(I).EQ.1)then
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
              call wtarentry(iunit,pathome,tr,ltr,tp,ltp,LF,ier)
            endif
          endif
 921    continue

C Write zone BASESIMP description if known.
        if(iBSIMP(i).eq.0)then
          continue
        else
          LF=LBSIMP(I)
          call wtarentry(iunit,pathome,tr,ltr,tp,ltp,LF,ier)
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
            call wtarentry(iunit,pathome,tr,ltr,tp,ltp,LF,ier)
          endif
        endif

C The following write statements do not detect the file location
        if(ZONE3D(I))then
          LF=L3DCVS(I)
          call wtarentry(iunit,pathome,tr,ltr,tp,ltp,LF,ier)
          LF=L3DCNC(I)
          call wtarentry(iunit,pathome,tr,ltr,tp,ltp,LF,ier)
          LF=L3DNDC(I)
          call wtarentry(iunit,pathome,tr,ltr,tp,ltp,LF,ier)
          LF=L3DTAQ(I)
          call wtarentry(iunit,pathome,tr,ltr,tp,ltp,LF,ier)
        endif
        if(MSTRZN(I))then
          LF=LMOIST(I)
          call wtarentry(iunit,pathome,tr,ltr,tp,ltp,LF,ier)
        endif
        if(IndxSt.EQ.I)then
          call wtarentry(iunit,pathome,tr,ltr,tp,ltp,LGrdSt,ier)
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
          call wtarentry(iunit,pathome,tr,ltr,tp,ltp,LF,ier)
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
          call wtarentry(iunit,pathome,tr,ltr,tp,ltp,LF,ier)
        endif
      endif
      call ERPFREE(IUNIT,ISTAT)

C Depending on current folder invoke tar with list of file names and
C create a tar file which includes root name of the project and the
C current pid number (to prevent overwriting).
C Use ifdef compiler variable passed into Make to identify which
C block of code to use.
#ifdef SUN
C Unix version of tar.
      call esppid(ipid)
      write(LFIL,'(a,i7,a)') cfgroot(1:lnblnk(cfgroot)),ipid,'.tar'
      call st2file(LFIL,LF)
      if(patdot)then
        write(doit,'(4a)',IOSTAT=IOS,ERR=3) 'cd ../../; tar cf ',
     &    LF(1:lnblnk(LF)),' -I ',LTMPFL(1:lnblnk(LTMPFL))
        call usrmsg('creating archive via',doit,'-')
        call runit(doit,'-')
      endif
      if(patcfg)then
        write(doit,'(4a)',IOSTAT=IOS,ERR=3) 'cd ../; tar cf ',
     &    LF(1:lnblnk(LF)),' -I ',LTMPFL(1:lnblnk(LTMPFL))
        call usrmsg('creating archive via',doit,'-')
        call runit(doit,'-')
      endif
      if(patroot)then
        write(doit,'(4a)',IOSTAT=IOS,ERR=3) 'cd ./; tar cf ',
     &    LF(1:lnblnk(LF)),' -I ',LTMPFL(1:lnblnk(LTMPFL))
        call usrmsg('creating archive via',doit,'-')
        call runit(doit,'-')
      endif
#endif

#ifdef LIN
C Linux and OSX version of tar.
      call esppid(ipid)
      write(LFIL,'(a,i7,a)') cfgroot(1:lnblnk(cfgroot)),ipid,'.tar'
      call st2file(LFIL,LF)
      if(patdot)then
        write(doit,'(4a)',IOSTAT=IOS,ERR=3) 'cd ../../; tar cf ',
     &    LF(1:lnblnk(LF)),' -T ',LTMPFL(1:lnblnk(LTMPFL))
        call usrmsg('creating archive via',doit,'-')
        call runit(doit,'-')
      endif
      if(patcfg)then
        write(doit,'(4a)',IOSTAT=IOS,ERR=3) 'cd ../; tar cf ',
     &    LF(1:lnblnk(LF)),' -T ',LTMPFL(1:lnblnk(LTMPFL))
        call usrmsg('creating archive via',doit,'-')
        call runit(doit,'-')
      endif
      if(patroot)then
        write(doit,'(4a)',IOSTAT=IOS,ERR=3) 'cd ./; tar cf ',
     &    LF(1:lnblnk(LF)),' -T ',LTMPFL(1:lnblnk(LTMPFL))
        call usrmsg('creating archive via',doit,'-')
        call runit(doit,'-')
      endif
#endif

#ifdef CIGW
C Cygwin/Linux version of tar.
      call esppid(ipid)
      write(LFIL,'(a,i7,a)') cfgroot(1:lnblnk(cfgroot)),ipid,'.tar'
      call st2file(LFIL,LF)
      if(patdot)then
        write(doit,'(4a)',IOSTAT=IOS,ERR=3) 'cd ../../; tar cf ',
     &    LF(1:lnblnk(LF)),' -T ',LTMPFL(1:lnblnk(LTMPFL))
        call usrmsg('creating archive via',doit,'-')
        call runit(doit,'-')
      endif
      if(patcfg)then
        write(doit,'(4a)',IOSTAT=IOS,ERR=3) 'cd ../; tar cf ',
     &    LF(1:lnblnk(LF)),' -T ',LTMPFL(1:lnblnk(LTMPFL))
        call usrmsg('creating archive via',doit,'-')
        call runit(doit,'-')
      endif
      if(patroot)then
        write(doit,'(4a)',IOSTAT=IOS,ERR=3) 'cd ./; tar cf ',
     &    LF(1:lnblnk(LF)),' -T ',LTMPFL(1:lnblnk(LTMPFL))
        call usrmsg('creating archive via',doit,'-')
        call runit(doit,'-')
      endif
#endif

#ifdef MINGW
C mingw version of tar.
      call esppid(ipid)
      write(LFIL,'(a,i7,a)') cfgroot(1:lnblnk(cfgroot)),ipid,'.tar'
      call st2file(LFIL,LF)
      if(patdot)then
        write(doit,'(4a)',IOSTAT=IOS,ERR=3) 'cd ../../; tar cf ',
     &    LF(1:lnblnk(LF)),' -T ',LTMPFL(1:lnblnk(LTMPFL))
        call usrmsg('creating archive via',doit,'-')
        call runit(doit,'-')
      endif
      if(patcfg)then
        write(doit,'(4a)',IOSTAT=IOS,ERR=3) 'cd ../; tar cf ',
     &    LF(1:lnblnk(LF)),' -T ',LTMPFL(1:lnblnk(LTMPFL))
        call usrmsg('creating archive via',doit,'-')
        call runit(doit,'-')
      endif
      if(patroot)then
        write(doit,'(4a)',IOSTAT=IOS,ERR=3) 'cd ./; tar cf ',
     &    LF(1:lnblnk(LF)),' -T ',LTMPFL(1:lnblnk(LTMPFL))
        call usrmsg('creating archive via',doit,'-')
        call runit(doit,'-')
      endif
#endif
      return

    2 CALL USRMSG(' Problem writing data',' ','W')
      call ERPFREE(IUNIT,ISTAT)
      return
    3 CALL USRMSG(' Problem writing archive command.',' ','W')
      return
      end
