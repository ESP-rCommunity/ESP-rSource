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
#include "prj3dv.h"
      
      integer lnblnk  ! function definition

      COMMON/SPAD/MMOD,LIMIT,LIMTTY
      common/OUTIN/IUOUT,IUIN
      integer childterminal  ! picks up mmod from starting of prj
      common/childt/childterminal

      common/rpath/path
      common/appw/iappw,iappx,iappy
      integer iappw,iappx,iappy
      common/user/browse

      common/FOPENED/CFGOK,MLDBOK,MATDBOK,CTLOK,OPTKOK,CFCDBOK
      COMMON/exporttg/xfile,tg,delim
      COMMON/exporttgi/ixopen,ixloc,ixunit

      logical OK,DOK,CFGOK,MLDBOK,MATDBOK,CTLOK,OPTKOK,concat,CFCDBOK
      logical unixok,browse
      integer iappwpc ! application height as % of nominal size

      DIMENSION ITEMS1(11)

      character LFIL*72,path*72,ltmp*72
      character dradcf*72   ! default name
      character ITEMS1*34,doit*248,outs*124,outs248*248
      character tmode*12
      character xfile*144,tg*1,delim*1,ffile*72,dvfil*72
      character ETEXT*82
      character longtfile*144,longtfiledos*144,L144*144,lltmp*144
      character brw*8   ! to signal model is being browsed
      character fs*1
      logical oke,modcfg
      integer NITMS,INO1 ! max items and current menu item
      integer lnradcf,lnltmp  ! lengths of file names

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
#else
      integer*8 iside,isize,ifont     ! passed to viewtext
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
      ITEMS1(9) ='i to VEF file              '
      ITEMS1(10)='? help                     '
      ITEMS1(11)='- exit this menu           '

C Help text for this menu.
      helptopic='cad_export_choices'
      call gethelptext(helpinsub,helptopic,nbhelp)
      NITMS=11
      CALL EMENU('Export Model',ITEMS1,NITMS,INO1)

C Prepare filter command lines with path.

      if(INO1.EQ.1.or.INO1.EQ.2.or.INO1.EQ.3.or.INO1.EQ.5.or.
     &   INO1.eq.6.or.INO1.eq.7.or.INO1.eq.8.or.INO1.eq.9.or.
     &   INO1.eq.10.or.INO1.eq.11)then
        continue
      else

C Note: this sets up initial part of LFIL but some later
C code tends to overwrite this and thus the user choice
C is lost. 
C Debug  write(6,*) 'the pwd command was ',
C     &    pwdinitial(1:lnblnk(pwdinitial))
C        write(6,*) ' pwdtocfg is ',pwdtocfg(1:lnblnk(pwdtocfg))

        iw=0
        if(path(1:2).ne.'./'.and.path(1:2).ne.'  ')then
          call edisp(iuout,'  ')
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

C Construct a 'VIEWER' format file.  First compose file name taking
C into account pwdinitial and pwdtocfg.
C Debug  write(6,*) 'the pwd command was ',
C     &    pwdinitial(1:lnblnk(pwdinitial))
C        write(6,*) ' pwdtocfg is ',pwdtocfg(1:lnblnk(pwdtocfg))
        if(pwdtocfg(1:1).eq.'!')then
          write(L144,'(4a)')path(1:lnblnk(path)),fs,
     &      cfgroot(1:lnblnk(cfgroot)),'.vew'
        else
          write(L144,'(6a)') pwdinitial(1:lnpwdi),fs,
     &      pwdtocfg(1:lnblnk(pwdtocfg)),fs,
     &      cfgroot(1:lnblnk(cfgroot)),'.vew'
        endif
C Debug write(6,*) 'b l144 is ',L144(1:lnblnk(L144))

        call tstamp('>','PRJ: export to viewer')

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

C Ensure full explicit path for unix or non-unix.
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
        goto 33  ! display the menu again

      ELSEIF(INO1.EQ.2)THEN

C DXF creation. Process similar to ecnv filter for VIEWER.
        write(6,*) 'the pwd command was ',
     &    pwdinitial(1:lnblnk(pwdinitial))
        write(6,*) ' pwdtocfg is ',pwdtocfg(1:lnblnk(pwdtocfg))
        if(pwdtocfg(1:1).eq.'!')then
          write(L144,'(4a)')path(1:lnblnk(path)),fs,
     &      cfgroot(1:lnblnk(cfgroot)),'.DXF'
        else
          write(L144,'(6a)') pwdinitial(1:lnpwdi),fs,
     &      pwdtocfg(1:lnblnk(pwdtocfg)),fs,
     &      cfgroot(1:lnblnk(cfgroot)),'.DXF'
        endif
C Debug write(6,*) 'd l144 is ',L144(1:lnblnk(L144))

        call tstamp('>','PRJ: export to DXF')
        dvfil='std.DXF'  ! default file name
        doit = ' '

C L144 is the suggested name and lltmp is the file name returned
C by the user selection.
        lltmp='  '
        call edisp(iuout,'  ')  ! echo a blank line
        CALL EASKXORGTKF(L144,'DXF export file name?',' ',
     &    dvfil,lltmp,'dxf file',IER,nbhelp)

C Debug.
C        write(6,*) 'l144 ',l144
C        write(6,*) 'lltmp ',lltmp

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
        goto 33

      ELSEIF(INO1.EQ.3)THEN

C Check if model path for radiance has been set, otherwise create a
C ../rad folder. If there is no Radiance Scene file name make it based
C on the model root name. Take into account where prj was started.
        modcfg=.false.
        if(pwdtocfg(1:1).eq.'!')then
          if(radpth(1:3).eq.'./ '.or.radpth(1:3).eq.'.\\ ')then
            CALL PHELPD('visu-rad-warning',7,'-',0,0,IER)
            write(radpth,'(3a)')'..',fs,'rad'
            write(doit,'(4a)') 'mkdir ',
     &        cfgroot(1:lnblnk(cfgroot)),fs,'rad'
            call usrmsg('Creating folder for radiance model:',doit,'P')
            call runit(doit,'-')

C And update radpath. The L144 is for future use of EASKXORGTKF
            write(radpth,'(3a)') '..',fs,'rad'
            modcfg=.true.
          endif

          if(lradcf(1:7).eq.'UNKNOWN')then  ! setup in form of ../rad/xx
            write(lradcf,'(6a)') '..',fs,'rad',fs,
     &        cfgroot(1:lnblnk(cfgroot)),'.rcf'
C            write(L144,'(8a)')path(1:lnblnk(path)),fs,'..',fs,'rad',
C     &        fs,cfgroot(1:lnblnk(cfgroot)),'.rcf'
            modcfg=.true.
          endif
        else

C Prj has not been invoked from within the model cfg folder.
          if(radpth(1:3).eq.'./ '.or.radpth(1:3).eq.'.\\ ')then
            CALL PHELPD('visu-rad-warning',7,'-',0,0,IER)
            write(radpth,'(3a)')'..',fs,'rad'
            write(doit,'(8a)') 'mkdir ',pwdinitial(1:lnpwdi),fs,
     &        pwdtocfg(1:lnblnk(pwdtocfg)),fs,'..',fs,'rad'
            call usrmsg('Creating folder for radiance model:',doit,'P')
            call runit(doit,'-')

C And update radpath. The L144 is for future use of EASKXORGTKF
            write(radpth,'(3a)') '..',fs,'rad'
            modcfg=.true.
          endif

          if(lradcf(1:7).eq.'UNKNOWN')then
            write(lradcf,'(2a)') cfgroot(1:lnblnk(cfgroot)),'.rcf'
C            write(L144,'(10a)') pwdinitial(1:lnpwdi),fs,
C     &        pwdtocfg(1:lnblnk(pwdtocfg)),fs,'..',fs,'rad',fs,
C     &        cfgroot(1:lnblnk(cfgroot)),'.rcf'
            modcfg=.true.
          endif
        endif

C Check rcf name and contents of file on return from e2r.
 319    ltmp=lradcf
        dradcf='scene.rcf'
        CALL EASKS(ltmp,' Radiance scene file for this model?',
     &    '  ',72,dradcf,'Radiance scene file name',IER,nbhelp)
        if(ltmp.eq.' ')goto 319 

C rcf name OK and differs from prior name, update cfg file and start e2r.
        lnradcf=lnblnk(lradcf)
        lnltmp=lnblnk(ltmp)
        if(ltmp(1:lnltmp).eq.lradcf(1:lnradcf))then
          continue
        else
          modcfg=.true.
          lradcf = ltmp
        endif

C RADIANCE creation. Starts e2r with current configuration file.
        dok=.true.
        CALL ASKOK(' ','Run RADIANCE conversion tool?',OK,dok,nbhelp)
        if(OK)then

C Save cfg file including ../rad folder and lradcf file name and start e2r.
          if(modcfg)then
            call tstamp('>','PRJ: save configuration (with rcf file)')
            CALL EMKCFG('-',IER)
          endif

C Set browsing to no.
          brw = ' -b no '
          call tstamp('>','PRJ: export to RADIANCE')
          doit = ' '
          call terminalmode(childterminal,tmode)
          if(unixok)then
            call addpath(LCFGF,longtfile,concat)
          else
            call addpath(LCFGF,longtfile,concat)
            call cmdfiledos(longtfile,longtfiledos,ier)
            longtfile=' '
            longtfile=longtfiledos
          endif
          if(iappw.eq.630)then
            iappwpc=100
          else
            iappwpc=nint(100.0*(real(iappw)/630.0))  ! reconstitute %
          endif
          if(iappwpc.gt.0.and.iappwpc.le.200)then
            write(doit,'(a,3i4,4a)') 'e2r -s ',
     &        iappwpc,iappx+35,iappy+40,' -file ',
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
          MODIFYVIEW=.TRUE.
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
          MODIFYVIEW=.TRUE.

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
        if(pwdtocfg(1:1).eq.'!')then
          write(L144,'(4a)')path(1:lnblnk(path)),fs,
     &      cfgroot(1:lnblnk(cfgroot)),'.THF'
        else
          write(L144,'(6a)') pwdinitial(1:lnpwdi),fs,
     &      pwdtocfg(1:lnblnk(pwdtocfg)),fs,
     &      cfgroot(1:lnblnk(cfgroot)),'.THF'
        endif

        call tstamp('>','PRJ: export to DXF')
        dvfil='std.THF'  ! default file name
        doit = ' '

C L144 is the suggested name and lltmp is the file name returned
C by the user selection.
        lltmp='  '
        call edisp(iuout,'  ')  ! echo a blank line
        CALL EASKXORGTKF(L144,'MicroGDS THINGS file name?',' ',
     &    dvfil,lltmp,'THF file',IER,nbhelp)

C If user request jump back and re-display the menu.
        if(ier.eq.-3)then
          return  ! cancel detected, return.
        endif
        if(ier.ne.0.or.lltmp(1:2).eq.'  ')then
          goto 11
        endif
        call tstamp('>','PRJ: export to THINGS')
        doit = ' '
        call edisp(iuout,'Performing model conversion.')

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
     &      ' -of thf -out ',lltmp(:lnblnk(lltmp))
        elseif(itrc.gt.1)then
          write(doit,'(4a)') 'ecnv -v -obs -if esp -in ',
     &       longtfile(1:lnblnk(longtfile)),
     &      ' -of thf -out ',lltmp(1:lnblnk(lltmp))
        endif
        call usrmsg('Starting THF conversion via',doit,'-')
        tmode = 'text'
        call runit(doit,tmode)
        write(outs248,'(3a)')'The file ',lltmp(1:lnblnk(lltmp)),
     &    ' has been created.'
        call edisp248(iuout,outs248,90)
        goto 33

      ELSEIF(INO1.EQ.6)THEN

C Export to EnergyPlus IDF file (work in progress)
        if(pwdtocfg(1:1).eq.'!')then
          write(L144,'(4a)')path(1:lnblnk(path)),fs,
     &      cfgroot(1:lnblnk(cfgroot)),'.idf'
        else
          write(L144,'(6a)') pwdinitial(1:lnpwdi),fs,
     &      pwdtocfg(1:lnblnk(pwdtocfg)),fs,
     &      cfgroot(1:lnblnk(cfgroot)),'.idf'
        endif

        call tstamp('>','PRJ: export to energyplus')
        dvfil='std.idf'  ! default file name
        doit = ' '

C L144 is the suggested name and lltmp is the file name returned
C by the user selection.
        lltmp='  '
        call edisp(iuout,'  ')  ! echo a blank line
        CALL EASKXORGTKF(L144,'EnergyPlus input file name?',' ',
     &    dvfil,lltmp,'E+ idf file',IER,nbhelp)

C If user request jump back and re-display the menu.
        if(ier.eq.-3)then
          return  ! cancel detected, return.
        endif
        if(ier.ne.0.or.lltmp(1:2).eq.'  ')then
          goto 11
        endif

C Gather information about IDF file version.
        ever=3.0
        evert=ever
        CALL EASKR(evert,'EnergyPlus IDF file version?',
     &    '(3.0 4.0 5.0 6.0 7.2)',
     &    3.0,'F',7.3,'F',6.0,'file version',IERI,nbhelp)
        if(ieri.eq.-3)then
          continue
        else
          call edisp(iuout,' Performing model conversion.')
          itrcc=2
          ioout=2
          ever=evert
          open(ioout,file=lltmp,status='UNKNOWN',err=901)
          call e2eplus(itrcc,ioout,ever,ier)
          close(ioout)
          write(outs248,'(3a)')'The file ',lltmp(1:lnblnk(lltmp)),
     &      ' has been created.'
          call edisp248(iuout,outs248,90)
        endif
        goto 33

      ELSEIF(INO1.EQ.7)THEN

C Export to VRML (2.0) file.
        if(pwdtocfg(1:1).eq.'!')then
          write(L144,'(4a)')path(1:lnblnk(path)),fs,
     &      cfgroot(1:lnblnk(cfgroot)),'.vrml'
        else
          write(L144,'(6a)') pwdinitial(1:lnpwdi),fs,
     &      pwdtocfg(1:lnblnk(pwdtocfg)),fs,
     &      cfgroot(1:lnblnk(cfgroot)),'.vrml'
        endif

        call tstamp('>','PRJ: export to VRML')
        dvfil='std.vrml'  ! default file name
        doit = ' '

C The X11 version will be returning only the name of the
C file, while the GTK version will be returning the
C name with the full path. L144 is the suggested name and
C lltmp is the file name returned by the user selection.
        lltmp='  '
        call edisp(iuout,'  ')  ! echo a blank line
        CALL EASKXORGTKF(L144,'VRML file name?',' ',
     &    dvfil,lltmp,'vrml file',IER,nbhelp)

C If user request jump back and re-display the menu.
        if(ier.eq.-3)then
          return  ! cancel detected, return.
        endif
        if(ier.ne.0.or.lltmp(1:2).eq.'  ')then
          goto 11
        endif

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
     &      ' -of vrml -out ',lltmp(1:lnblnk(lltmp))
        elseif(itrc.gt.1)then
          write(doit,'(4a)') 'ecnv -v -if esp -in ',
     &      longtfile(1:lnblnk(longtfile)),
     &      ' -of vrml -out ',lltmp(1:lnblnk(lltmp))
        endif
C        call usrmsg('Starting VRML conversion via',doit,'-')
        tmode = 'text'
        call runit(doit,tmode)
        write(outs248,'(3a)')'The file ',lltmp(1:lnblnk(lltmp)),
     &    ' has been created.'
        call edisp248(iuout,outs248,90)
        call edisp(iuout,'Use a VRML viewer to explore it.')
        goto 33  ! display the menu again

      ELSEIF(INO1.EQ.8)THEN

C Export to ESP-r meta file.
        if(pwdtocfg(1:1).eq.'!')then
          write(L144,'(4a)')path(1:lnblnk(path)),fs,
     &      cfgroot(1:lnblnk(cfgroot)),'.mesp'
        else
          write(L144,'(6a)') pwdinitial(1:lnpwdi),fs,
     &      pwdtocfg(1:lnblnk(pwdtocfg)),fs,
     &      cfgroot(1:lnblnk(cfgroot)),'.mesp'
        endif

        call tstamp('>','PRJ: export to ESP-r meta')
        dvfil='std.mesp'  ! default file name
        doit = ' '

C L144 is the suggested name and lltmp is the file name returned
C by the user selection.
        lltmp='  '
        call edisp(iuout,'  ')  ! echo a blank line
        CALL EASKXORGTKF(L144,'ESP-r meta file name?',' ',
     &    dvfil,lltmp,'ESP-r Meta file',IER,nbhelp)

C If user request jump back and re-display the menu.
        if(ier.eq.-3)then
          return  ! cancel detected, return.
        endif
        if(ier.ne.0.or.lltmp(1:2).eq.'  ')then
          goto 11
        endif

        ioout=2
        call METAWRITE(ioout,lltmp,'-',IER)
        write(outs248,'(3a)')'The file ',lltmp(1:lnblnk(lltmp)),
     &    ' has been created.'
        call edisp248(iuout,outs248,90)
        goto 33

      ELSEIF(INO1.EQ.9)THEN

C Export to acrobat VEF file.
        if(pwdtocfg(1:1).eq.'!')then
          write(L144,'(4a)')path(1:lnblnk(path)),fs,
     &      cfgroot(1:lnblnk(cfgroot)),'.vef'
        else
          write(L144,'(6a)') pwdinitial(1:lnpwdi),fs,
     &      pwdtocfg(1:lnblnk(pwdtocfg)),fs,
     &      cfgroot(1:lnblnk(cfgroot)),'.vef'
        endif

        call tstamp('>','PRJ: export to VEF')
        dvfil='std.vef'  ! default file name
        doit = ' '

C L144 is the suggested name and lltmp is the file name returned
C by the user selection.
        lltmp='  '
        call edisp(iuout,'  ')  ! echo a blank line
        CALL EASKXORGTKF(L144,'acrobat VEF file name?',' ',
     &    dvfil,lltmp,'acrobat vef file',IER,nbhelp)

C If user request jump back and re-display the menu.
        if(ier.eq.-3)then
          return  ! cancel detected, return.
        endif
        if(ier.ne.0.or.lltmp(1:2).eq.'  ')then
          goto 11
        endif

        ioout=2
        CALL EASKABC('Specify GEN bodies or separate polygons with',
     &    'glazing clear:','GEN bodies','with transparent windows',
     &    'cancel',IRT,nbhelp)
        if(IRT.eq.3)then
          continue
        else
          CALL EASKABC('Colour options:',' ','All zones grey',
     &                            'Coloured zones','cancel',IRT2,nbhelp)
          if (IRT2.eq.3) then
            continue
          else
            IRT2=IRT2-1
            CALL EFOPSEQ(ioout,lltmp,3,IER)
            if(IRT.eq.1)then
              call e2vef(itrc,ioout,'g',IRT2)  ! see end of this file
            elseif(IRT.eq.2)then
              call e2vef(itrc,ioout,'p',IRT2)
            endif
            CALL ERPFREE(ioout,ios)
            write(outs248,'(3a)')'The file ',lltmp(1:lnblnk(lltmp)),
     &      ' has been created.'
            call edisp248(iuout,outs248,90)
          endif
        endif
        goto 33

      ELSEIF(INO1.EQ.10)THEN
      
C Pop up the help text.
       CALL PHELPD('export menu',nbhelp,'-',0,0,IER)

      ELSEIF(INO1.EQ.11)THEN
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
C Used by the Project Manager.
      subroutine cadin(itrc,ier)
#include "building.h"
#include "model.h"
      
      integer lnblnk  ! function definition

      common/FILEP/IFIL
      INTEGER :: ifil
      COMMON/OUTIN/IUOUT,IUIN
      INTEGER :: iuout,iuin
      
      integer ncomp,ncon
      common/C1/NCOMP,NCON
      common/rcmd/LCMDFL

C Indicator of possible focus zone.
      common/user/browse

C External cad package.
      common/cad3rd/cadlbl,cadexe,cadfmt

      LOGICAL     OK,DOK,concat,browse,confirm,ckpath,unixok
      logical itisanexemplar  ! passed to newprb to ensure correct browse mode.

      character LFIL*72,LCMDFL*144
      character outs*124,OUTSTR*124,doit*248
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
      subroutine rexmpl(itrc,iexfil,exemplar,useraction,ier)
#include "building.h"
#include "model.h"
#include "espriou.h"
C espriou.h provides currentfile, defdbfl exemfl.
      
      integer lnblnk  ! function definition

C Passed parameters.
      integer itrc    ! indicates feedback level
      integer iexfil  ! unit number for the exemplar file
      character exemplar*144 ! full path to selected cfg file
      character useraction*8 ! passed in to specify the task
      integer ier     ! returned as 1 if no exemplars file found

      COMMON/SPAD/MMOD,LIMIT,LIMTTY
      common/OUTIN/IUOUT,IUIN
      
      integer ncomp,ncon
      common/C1/NCOMP,NCON
      INTEGER :: INDFCG
      common/C6/INDCFG

C Images - included to warn user on subsequent browsing sessions
C to quit the image browser.
C      character imgfmt*4  ! GIF XBMP TIF JPG
C      character imgfoc*4  ! FZON FNET FCTL FDFS
C      character limgfil*72  ! file name (extend to 144 char)
C      character imgdoc*248  ! text associated with image
C      common/imagf/imgfmt(MIMG),imgfoc(MIMG),limgfil(MIMG),imgdoc(MIMG)

      integer noimg  ! number of images
      integer iton   ! zero if images not yet shown, one if yes
      common/imagfi/noimg,iton

      dimension name(30),mitem(30),label(30),inl2in(30),group(15)
      dimension gitem(18),gh(13),cfgitem(30),inl2ngr(30),itemlog(30)
      character name*70,label*42,mitem*72,gitem*44,OUTSTR*124,WORD*20
      character group*42,tgroup*42,key*1,look*70,tlook*70
      character outs*124,rootp*84,separ*42
      character doit*248
      character gh*72,cfgitem*144,exdescr*81,doc*72,head*42
      character cfg*144,cfg_path*84,cfg_root*72,doc_file*96
      character itemlog*144,ilog*144

C Strings for dos version of rootp and upath and a double quote.
      character rootpdos*84,upathdos*72,rootpath*72,modelroot*72
      character dq*1,fs*1,thecfgis*72,cfgcfg*42
      character lltmp*144,lguess*144,dguess*144
      logical XST,unixok
      integer NCOG,ICOG,NCO,ICO,icod ! max items and current menu item
      integer loop  ! for looping
      integer iglib   ! if 1 then X11, if 2 then GTK, if 3 then text only.

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
      iglib = igraphiclib()  ! find out if GTK.

C Open the exemplar file. This is also a jump back point in case
C the user decides to cancel the selection.
  41  IER=0
      if(exemfl(1:1).eq.'/')then
        call FPOPEN(iexfil,ISTAT,1,0,exemfl)
      else
        CALL EFOPSEQ(iexfil,exemfl,1,IER)
      endif
      if(ier.ne.0) return

C Clear some of the local strings.
      do 38 loop=1,30
        name(loop)='  '
        label(loop)='  '
        cfgitem(loop)='  '
        itemlog(loop)='  '
        inl2in(loop)=0
        inl2ngr(loop)=0
        if(loop.le.15) group(loop)='  '
  38  continue

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
      ilog='  '
      ngr=0
      ing=0
      inl=0
      igrsel=0
      icod=0
      cfgcfg='  '

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

C Exit from the menu.
        CALL ERPFREE(iexfil,ISTAT)
        return

      ELSEIF(ICOG.EQ.(NCOG-1))THEN

C Help @ db file level.
        helptopic='exemplar_menu_help'
        call gethelptext(helpinsub,helptopic,nbhelp)
        CALL PHELPD('exemplar menu',nbhelp,'-',0,0,IER)

      ELSEIF(ICOG.GT.0.AND.ICOG.LT.(NCOG-2))THEN

C User selected an exemplar category from the menu display.
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

C We have a match with the selected group so now fill the menu
C strings so they can be presented to the user.
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

C In case there is a log file mentioned.
              call egetrm(outstr,K,itemlog(in),'W','item log',IER)
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

C Present the list of this category of exemplars.
  42  ICO= -2
      mitem(in+1)=  '  _______________________________'
      mitem(in+2)=  '* view exemplar documentation    '
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

C Make up a menu as wide as required if in graphic mode.
      if(MMOD.EQ.8)then
        impx=0
        impy=0
        CALL VWMENU(head,mitem,NCO,impx,impy,IWE,irpx,irpy,ICO)
      else
        CALL EMENU(head,mitem,NCO,ICO)
      endif

C Process the user selection.
      IF(ICO.EQ.NCO)THEN

C Exit from detail level back to group level.
        CALL ERPFREE(iexfil,ISTAT)
        goto 41

      ELSEIF(ICO.EQ.(NCO-1))THEN

C Display the basic help plus help from exemplars file.
        CALL PHELPD('exemplar menu',nbhelp+nghelp,'-',0,0,IER)

      ELSEIF(ICO.EQ.(NCO-2))THEN

C User picked the documentation item in the menu. Present the list
C again so the user can select which model they want to find more
C about.
        call edisp(iuout,'  ')
        call edisp(iuout,
     &    'Select which exemplar you want to know more about.')
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
              call edisp(iuout,'  ')
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

C User selected an exemplar, attempt to access it.
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
              if(iglib.eq.2.or.iglib.eq.3)then
                call edisp(iuout,'  ')
                call edisp(iuout,'Examplar model is:')  ! echo if gtk
                call edisp(iuout,exemplar)
              endif

C If running on a Windows machine get the last token within the string exemplar,
C which is the name of the configuration file. Work backwards from the end
C of the string till a file separator of either DOS or Unix type is reached.
              call isunix(unixok)
              if(.NOT.unixok)then
                thecfgis=' '
                ilexem = lnblnk(exemplar)  ! last character
                ipos = ilexem
 344            continue
                ipos=ipos-1
                if(exemplar(ipos:ipos).eq.fs)then
                  write(thecfgis,'(a)') exemplar(ipos+1:ilexem)
                  if(itrc.gt.0)then
                    call edisp(iuout,'  ')
                    call edisp(iuout,'The model configuration file is:')
                    call edisp(iuout,thecfgis)
                  endif
                elseif(exemplar(ipos:ipos).eq.'/')then
                  write(thecfgis,'(a)') exemplar(ipos+1:ilexem)
                  if(itrc.gt.0)then
                    call edisp(iuout,'  ')
                    call edisp(iuout,'The model configuration file is:')
                    call edisp(iuout,thecfgis)
                  endif
                else
                  if(ipos.eq.1)then
                    call edisp(iuout,
     &                'Warning: No folder separator found.')
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
            elseif(WORD(1:5).eq.'*root')then
              call egetrm(outstr,K,rootp,'W','exem root dir',IER)
              goto 4
            elseif(WORD(1:4).eq.'*log')then

C In case there is a log file mentioned.
              call egetrm(outstr,K,ilog,'W','item log',IER)
              goto 4
            elseif(WORD(1:5).eq.'*item')then

C If a log file mentioned in exemplars file show it.
              if(ilog(1:2).ne.'  ')then
                call edisp(iuout,'  ')
                CALL LISTAS(iexfil+1,ilog,IER)
              endif

C Reached the end of the item, check if it is to be chosen and
C if there are zones existing, remove them from memory.
              CALL EASKAB(exdescr,'Actions:',
     &          'proceed','cancel',IW,nbhelp)
              if(IW.eq.1)then
                if(upath.ne.' ')then

C See if can derive the local configuration name. cfg_root is the last
C word in the path to the exemplar root folder.
                  call fdroot(rootp,cfg_path,cfg_root)
                  lex=lnblnk(exemplar)
                  lcfgr=lnblnk(cfg_root)

C Debug the model source.
C                  write(outs,'(a,2i4,2a)') 'cfg_root = ',lex,lcfgr,
C     &              '  ',cfg_root(1:lcfgr)
C                  call edisp(iuout,outs)

C Loop along the exemplar string to locate the position of the root
C folder in the path of the exemplar.
                  ilex=1
                  do 342 ilex=1,lex-lcfgr
                    if(cfg_root(1:lcfgr).eq.
     &                exemplar(ilex:ilex+lcfgr-1))then
                      goto 343
                    endif
  342             continue
  343             continue

C The balance of the exemplar string after the cfg_root.
                  write(cfgcfg,'(a)') exemplar(ilex+lcfgr:lex)
                  write(6,*) cfgcfg,' ',ilex+lcfgr,lex
                  call isunix(unixok)
                  if(unixok)then

C Offer user choice of home folder plus the exemplar root name. In GTK
C there is a file browser returning folder name via lltmp. If lltmp
C folder does not exist then need to create it.
                    write(lguess,'(3a)') upath(1:lnblnk(upath)),fs,
     &                cfg_root(1:lcfgr)
                    write(dguess,'(3a)') upath(1:lnblnk(upath)),fs,
     &                cfg_root(1:lcfgr)
                    call edisp(iuout,'  ')  ! echo a blank line
                    CALL EASKXORGTKF(lguess,' ','Place model in:',               
     &                dguess,lltmp,'models folder',IER,nbhelp)

                    write(doit,'(2a)') 'mkdir ',lltmp(1:lnblnk(lltmp))
                    call runit(doit,'-')
                    call pausems(100)

C Make up the command to copy from the exemplar source folder to the
C users defined destination. If we add cfgcfg to lltmp we should have
C the folder where the model cfg file is located.
                    write(doit,'(5a)') 'cp -r ',rootp(1:lnblnk(rootp)),
     &                fs,'*  ',lltmp(1:lnblnk(lltmp))
                  else

C Non-unix, if lnblankrp or lnblankup is one then there are no spaces in the 
C string so double quotes are not required.  Change any '/'
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
                    if(itrc.gt.0)then
                      write(outs,'(2a)') 'Getting model from...',
     &                  rootpdos(1:lnrp)
                      call edisp(iuout,outs)
                    endif

C For an xcopy command we need the destination to also include the
C model folder so use fdroot to extract this from rootp.
                    call fdroot(rootpdos,rootpath,modelroot)

                    call edisp(iuout,' ')

C Offer user choice of home folder plus the exemplar root name. In GTK
C there is a file browser returning folder name via lltmp.
                    write(lguess,'(3a)') upath(1:lnblnk(upath)),fs,
     &                cfg_root(1:lcfgr)
                    write(dguess,'(3a)') upath(1:lnblnk(upath)),fs,
     &                cfg_root(1:lcfgr)
                    call edisp(iuout,'  ')  ! echo a blank line
                    CALL EASKXORGTKF(lguess,' ','Place model in:',
     &                dguess,lltmp,'models folder',IER,nbhelp)

                    lnup=lnblnk(lltmp)
                    lnblankup=iprevblnk(lltmp,lnup)
                    call backslashit(lltmp,upathdos)

C Debug.
C                    write(outs,'(a,2i3,2a)') 'upath = ',lnup,lnblankup,
C     &                ' ',upath(1:lnup)
C                    call edisp(iuout,outs)
C                    write(outs,'(2a)') 'destination = ',upathdos(1:lnup)
C                    call edisp(iuout,outs)

C If upathdos folder does not exist then need to create it.
                    if(lnblankup.gt.1)then

C There are blanks in destination strings.
                      write(doit,'(4a)') 'mkdir ',dq,upathdos(1:lnup),dq
                    elseif(lnblankup.eq.1)then

C There are no blanks in destination string.
                      write(doit,'(2a)') 'mkdir ',upathdos(1:lnup)
                    endif
                    call runit(doit,'-')

                    if(lnblankrp.gt.1.and.lnblankup.gt.1)then

C There are blanks in both source or destination strings.
                      write(doit,'(11a)') 'xcopy /e /i /y /c ',dq,
     &                  rootpdos(1:lnrp),fs,'*',dq,'  ',dq,
     &                  upathdos(1:lnup),fs,dq
                    elseif(lnblankrp.eq.1.and.lnblankup.gt.1)then

C There are blanks in destination string.
                      write(doit,'(9a)') 'xcopy /e /i /y /c ',
     &                  rootpdos(1:lnrp),fs,'*  ',dq,
     &                  upathdos(1:lnup),fs,dq
                    elseif(lnblankrp.gt.1.and.lnblankup.eq.1)then

C There are blanks in source string.
                      write(doit,'(9a)') 'xcopy /e /i /y /c ',dq,
     &                  rootpdos(1:lnrp),fs,'*',dq,'  ',
     &                  upathdos(1:lnup),fs
                    elseif(lnblankrp.eq.1.and.lnblankup.eq.1)then

C There are no blanks in either source or destination strings.
                      write(doit,'(6a)') 'xcopy /e /i /y /c ',
     &                  rootpdos(1:lnrp),fs,'*  ',
     &                  upathdos(1:lnup),fs
                    endif
                  endif  ! of non-unix

C If trace is verbose echo the command.
                  if(itrc.gt.0)then
                    call edisp(iuout,
     &                'Copying exemplar to destination folder via:')
                    call edisp248(iuout,doit,100)
                  endif
                  call runit(doit,'-')
                  if(unixok)then

C If Unix-like then append tail of exemplar string (cfgcfg) to the users selected
C path to get the new exemplar name.
                    write(exemplar,'(2a)') lltmp(1:lnblnk(lltmp)),
     &                cfgcfg(1:lnblnk(cfgcfg))
                    write(6,*) exemplar
                  else

C If Windows based then append cfg & cfg file string to the path to make
C up the exemplar name (this assumes a standard layout of model).
                    write(exemplar,'(5a)')upathdos(1:lnblnk(upathdos)),
     &                fs,'cfg',fs,thecfgis(1:lnblnk(thecfgis))
                  endif
                  call edisp(iuout,'  ')
                  call edisp(iuout,'The model configuration file is:')
                  call edisp(iuout,exemplar)
                  call tstamp('>','PRJ: owning exemplar')
                  call tstamp('>',exemplar)
                  useraction='ownit   '
                endif
              elseif(IW.eq.2)then

C User asked to cancel current selection, rewind the file and jump back to
C the top menu to re-resent groups.
                exemplar='UNKNOWN'
                REWIND(iexfil,ERR=92)
                useraction='continue'
                goto 41
              endif

C If there was a prior model in memory (e.g. some zones or a plant
C only model) clear it.
              CALL ERPFREE(iexfil,ISTAT)
              if(ncomp.gt.0.or.INDCFG.eq.2)then
                call usrmsg('Please wait... ',
     &                      'Clearing previous model.','-')
                call startbuffer()
                call clrprb
              endif

C If the image browser was started in a previous session warn
C the user to kill it.
              if(iton.eq.1)then
                noimg=0
                iton=0
              endif
              return  ! we now know the model we want to load
            else
              goto 4
            endif  ! of finding a matching item token
          else
            goto 3
          endif
        else
          goto 3
        endif      ! of processing a name token
      else
        goto 42
      endif        ! of processing user selection
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
C UA1 in uncertainty.h
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
#include "plant.h"
#include "power.h"

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
      COMMON/C23/IFPNF,LPNF
      INTEGER :: IFPNF
      COMMON/AFN/IAIRN,LAPROB,ICAAS(MCOM)
      INTEGER :: iairn,icaas
      common/cctlnm/ctldoc,lctlf
      common/FOPENED/CFGOK,MLDBOK,MATDBOK,CTLOK,OPTKOK,CFCDBOK
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
      INTEGER :: ispmxist
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

      character LPNF*72,LMOIST*72
      character*72 LCFD
      character LCMDFL*144,LFIL*72
      
C The string buffer doit and option used for tar command.
      character doit*248,option*4
      character path*72
      character curdir*84
      character LAPROB*72
      CHARACTER*72 LGDCVS,LGDCNC,LGDNDC,LGDNDD,LGDTAQ,LGRD1D,LNLTHP
      character tp*42,tr*32
      character ctldoc*248,LCTLF*72,LGrdSt*72
      character GTGEOM*72,lipvdatf*72
      character fs*1
      character LTMPFL*72
      character LF*72,LFL*144
      CHARACTER spflnam*72
      CHARACTER*72 L3DCVS,L3DCNC,L3DNDC,L3DTAQ
      character LLASCI*144
      character hvacfile*72,LBSIMP*72
      character outs*124

      logical browse,XST,unixok
      LOGICAL INTHPS,INTHPZ,IGR1D,GRND3D
      LOGICAL CFGOK,MLDBOK,MATDBOK,CTLOK,OPTKOK,CFCDBOK

C Patdot is true if path is ./, patcfg is true if path is cfg,
C patroot is true if path is at root level. 
      logical patdot,patcfg,patroot,pathome,localdb
      character WORDS(12)*32
C      character WORDS*32

      INTEGER :: lc,j,iw,iuf,iunit,ito,istat,ipid,img,ifold,i,ier
      INTEGER :: iaccess,icerr,ios,lcd,llf,lr,ltp,ltr
      integer iwds,lniwd,lniwd1

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
          if(unixok)write(6,*) 'iaccess is ',iaccess,icerr,ifold,curdir
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
        iwds=iw   ! remember how many words
        if(iwds.gt.0)then

C Find out how many characters are in the two folders above where we
C are now so this can be subtracted from curdir for testing.
          lniwd1=lnblnk(WORDS(iwds-1))
          lniwd=lnblnk(WORDS(iwds))
          write(LF,'(3a)') WORDS(iwds-1)(1:lniwd1),
     &            fs,WORDS(iwds)(1:lniwd)
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
            lniwd1=lnblnk(WORDS(iwds-1))
            lniwd=lnblnk(WORDS(iwds))
            write(tp,'(3a)') WORDS(iwds-1)(1:lniwd1),
     &            fs,WORDS(iwds)(1:lniwd)
            ltp=lnblnk(tp)
            write(tr,'(a)') WORDS(iwds-1)(1:lniwd1)
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
        iwds=iw   ! remember how many words
        if(iwds.gt.0)then

C Find out how many characters are in the folder above where we
C are now so this can be subtracted from curdir for testing.
          lniwd=lnblnk(WORDS(iwds))
          write(LF,'(a)') WORDS(iwds)(1:lniwd)
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
            write(tp,'(3a)') WORDS(iwds)(1:lniwd),fs,'cfg'
            ltp=lnblnk(tp)
            write(tr,'(a)') WORDS(iwds)(1:lniwd)
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
        iwds=iw   ! remember how many words
        if(iwds.gt.0)then
          do 95 ito=1,iwds
            lniwd=lnblnk(WORDS(ito-1))
            if(WORDS(ito)(1:3).eq.'cfg'.and.ito.gt.1)then
              write(tp,'(3a)') WORDS(ito-1)(1:lniwd),fs,'cfg'
              ltp=lnblnk(tp)
              write(tr,'(a)') WORDS(ito-1)(1:lniwd)
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
      do 41 i=1,8
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
        elseif(i.eq.8)then
          LFL=lfmould
          if(ipathmould.eq.1) localdb=.true.
        endif
        if(localdb.and.LFL(1:4).ne.'UNKN')then
          call wtarentry(iunit,pathome,tr,ltr,tp,ltp,LFL)
        endif
  41  continue

C Write model contents file if it exists.
C << to be done >>

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
          write(LF,'(a)') limgfil(img)
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

C Write non-specific demands.
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

C Write Radiance scene file.
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
      call wtarentry(iunit,pathome,tr,ltr,tp,ltp,lmodellog)

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
            write(LF,'(a)') LGEOM(I)
          elseif(J.eq.2)then
            write(LF,'(a)') LPROJ(I)
          elseif(J.eq.3)then
            write(LF,'(a)') LTHRM(I)
          elseif(J.eq.4.and.IOBS(I).EQ.1)then
            write(LF,'(a)') ZOBS(I)  ! if obs is separate file
          elseif(J.eq.5.and.ITW(I).EQ.1)then
            write(LF,'(a)') LTWIN(I)
          elseif(J.eq.6.and.IVF(I).EQ.1)then
            write(LF,'(a)') LVIEW(I)
          elseif(J.eq.7.and.ICGC(I).EQ.1)then
            write(LF,'(a)') LCGCIN(I)
          elseif(J.eq.8.and.ISI(I).EQ.1)then
            write(LF,'(a)') LSHAD(I)  ! binary shading file
          elseif(J.eq.9.and.ISI(I).EQ.1)then
            write(LF,'(2a)')LSHAD(I)(1:lnblnk(LSHAD(I))),'.a'  !ascii version
            call FINDFIL(LF,XST)
            if(XST)then
              continue ! it was found
            else
              LF='UNKNOWN'
            endif
          elseif(J.eq.10.and.IHC(I).EQ.1)then
            write(LF,'(a)') LHCCO(I)
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
          write(LF,'(a)') LBSIMP(I)
          call wtarentry(iunit,pathome,tr,ltr,tp,ltp,LF)
        endif

C Write zone CFD file if known.
        if(lnblnk(LCFD(I)).eq.0)then
          continue
        elseif(LCFD(I)(1:7).eq.'UNKNOWN'.or.LCFD(I)(1:2).eq.'  ')then
          continue
        else
          write(LF,'(a)') LCFD(I)
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
          write(LF,'(a)') L3DCVS(I)
          call wtarentry(iunit,pathome,tr,ltr,tp,ltp,LF)
          write(LF,'(a)') L3DCNC(I)
          call wtarentry(iunit,pathome,tr,ltr,tp,ltp,LF)
          write(LF,'(a)') L3DNDC(I)
          call wtarentry(iunit,pathome,tr,ltr,tp,ltp,LF)
          write(LF,'(a)') L3DTAQ(I)
          call wtarentry(iunit,pathome,tr,ltr,tp,ltp,LF)
        endif
        if(MSTRZN(I))then
          write(LF,'(a)') LMOIST(I)
          call wtarentry(iunit,pathome,tr,ltr,tp,ltp,LF)
        endif
        if(IndxSt.EQ.I)then
          call wtarentry(iunit,pathome,tr,ltr,tp,ltp,LGrdSt)
        endif
 920  continue

C Flow network.
      if(IAIRN.EQ.1)then
        write(LF,'(a)') LAPROB
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
        write(LF,'(a)') LPNF
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

C e2vef takes an esp-r problem definition and creates a vef
C vertex-edge-face file in file unit ioout (assumed to already be opened).
      subroutine e2vef(itrc,ioout,act,icol)
#include "building.h"
#include "model.h"
#include "geometry.h"

C Parameter
      character act*1  ! g for GEN p for PLA
      COMMON/OUTIN/IUOUT,IUIN
      COMMON/FILEP/IFIL

      COMMON/C1/NCOMP,NCON
      COMMON/GB1/XB(12),YB(12),ZB(12),JVNB(6,4)
      integer IZSTOCN
      COMMON/C24/IZSTOCN(MCOM,MS)

      dimension ndum(MS)

      character outs*124,zsn*28

C Setup standard assumptions.
      IFIL=11

C Assume geometry or obstructions on IFIL+8 
      ITA1 = IFIL+8

C If VEF output required.
      INPIC=NCOMP
      do 4 mz=1,inpic
        newfoc=mz

C Read in the zone geometry.
        WRITE(outs,'(a,a)')' Scanning : ',LGEOM(newfoc)
        CALL edisp(iuout,outs)

C Depending on version of geometry file scan it.
        if(gversion(mz).lt.1.1) then
          CALL EGOMIN(ITA1,LGEOM(newfoc),newfoc,1,ITRC,IUOUT,IER)
          if(iobs(mz).eq.0)then
            continue   ! no obstructions
          elseif(iobs(mz).eq.1)then
            CALL ERPFREE(ITA1,ISTAT)
            CALL EGOMST(ITA1,newfoc,ZOBS(newfoc),0,ITRC,ITRU,IER)
          endif
        elseif(gversion(mz).ge.1.1) then
          call georead(ITA1,LGEOM(newfoc),newfoc,1,iuout,ier)
          if(iobs(mz).eq.0)then
            continue   ! no obstructions
          elseif(iobs(mz).eq.1)then
            CALL ERPFREE(ITA1,ISTAT)
            CALL EGOMST(ITA1,newfoc,ZOBS(newfoc),0,ITRC,ITRU,IER)
          elseif(iobs(mz).eq.2)then
            continue   ! obs within geo file
          endif
        endif

C Write colour directive to VEF file.
C << Currently hard-coded for 1 colour per zone.
C << Currently limited to default 15 colours in Akrobat.
        if (icol.eq.1) then
          WRITE(ioout,'(a)')' COL'
C Calculate colour reference. Repeat colours every 15 zones.
          icolref=newfoc-15*(((newfoc+14)/15)-1)
          WRITE(ioout,'(i2.1)')icolref
        endif

C Write surface information to the VEF file. (Rotation set to 0 deg).
C If act is g then use GEN bodies.
        if(act.eq.'g'.or.act.eq.'G')then
          WRITE(ioout,'(a,a12)')'GEN ',zname(newfoc)
          WRITE(ioout,'(2I4,a)')NTV,NSUR,' 0'
          DO J=1,NTV
            WRITE(ioout,'(3F10.5)')X(J),Y(J),Z(J)
          ENDDO

          DO J=1,NSUR
            WRITE(ioout,'(2X,I3,1X,24I3)')NVER(J),(JVN(J,K),K=1,NVER(J))
            ndum(J)=0
          ENDDO

          write(ioout,5611)(ndum(I),I=1,NSUR)
          write(ioout,5611)(ndum(I),I=1,NSUR)
5611      FORMAT(1X,32(I2))
          write(ioout,'(a)')' 3 0 0 0 '
        elseif(act.eq.'p'.or.act.eq.'P')then

C Write each surface as a PLA type. Proceed with VIS 0 or 2 depending
C on if it is transparent. Also mark each with an identity zone:name.
C Note: does not know quite how to treat CFC and CFC2 so just treats
C as opaque.
          do J=1,NSUR
            icn=IZSTOCN(newfoc,j)  ! get connection
            lsn=lnblnk(sname(newfoc,j))
            write(zsn,'(3a)') zname(newfoc)(1:lnzname(newfoc)),
     &        ':',sname(newfoc,j)(1:lsn)
            if (SSOTF(icn)(1:4).ne.'OPAQ'.and.
     &          SSOTF(icn)(1:4).ne.'CFC '.and.
     &          SSOTF(icn)(1:4).ne.'CFC2') then
              write(ioout,'(a)')' VIS '
              write(ioout,'(a)')' 0 '
            else
              write(ioout,'(a)')' VIS '
              write(ioout,'(a)')' 2 '
            endif
            WRITE(ioout,'(2a)')'PLA ',zsn(1:lnblnk(zsn))
            WRITE(ioout,'(I4)') NVER(J)
            DO I=1,NVER(J)
              WRITE(ioout,'(3F11.5)')
     &          X(JVN(J,I)),Y(JVN(J,I)),Z(JVN(J,I))
            enddo
          enddo
        endif

C If there are obstructions convert them to general polygons
C and write them out.
        if(iobs(mz).eq.0)then
          continue
        elseif(iobs(mz).eq.1.or.iobs(mz).eq.2)then

          DO 301 IB=1,nbobs(mz)
            if(BLOCKTYP(mz,IB)(1:4).eq.'obs ')then
              CALL CNVBLK(XOB(mz,IB),YOB(mz,IB),ZOB(mz,IB),
     &          DXOB(mz,IB),DYOB(mz,IB),DZOB(mz,IB),
     &          BANGOB(mz,IB,1))
            elseif(BLOCKTYP(mz,IB)(1:4).eq.'obs3')then
              CALL CNVBLK3A(XOB(mz,IB),YOB(mz,IB),ZOB(mz,IB),
     &          DXOB(mz,IB),DYOB(mz,IB),DZOB(mz,IB),
     &          BANGOB(mz,IB,1),BANGOB(mz,IB,2),BANGOB(mz,IB,3))
            elseif(BLOCKTYP(mz,IB)(1:4).eq.'obsp')then
              call CNVBLKP(mz,IB) ! convert obsp type.
            endif
C << and then what... look in e2rform.F near line 1120
C << there are 8 vertices and 6 surfaces
            lnz=lnblnk(zname(mz))
            WRITE(ioout,'(4a)') 'GEN OBS:',zname(mz)(1:lnz),':',
     &        BLOCKNAME(mz,IB)(1:LNBLOCKNAME(mz,IB))
            WRITE(ioout,'(a)')' 8  6  0'
            DO 41 J=1,8
              if (BLOCKTYP(mz,ib).eq.'obs ') then
                WRITE(ioout,'(3F10.5)')XB(J),YB(J),ZB(J)
              elseif (BLOCKTYP(mz,ib).eq.'obs3') then
                WRITE(ioout,'(3F10.5)')XB(J),YB(J),ZB(J)
              elseif (BLOCKTYP(mz,ib).eq.'obsp') then
                WRITE(ioout,'(3F10.5)')XBP(mz,IB,J),YBP(mz,IB,J),
     &            ZBP(mz,IB,J)
              endif
   41       continue
            DO 51 J=1,6
              WRITE(ioout,'(a,4I3)') ' 4 ',(JVNB(J,K),K=1,4)
              ndum(J)=0
   51       continue

            write(ioout,5611)(ndum(I),I=1,6)
            write(ioout,5611)(ndum(I),I=1,6)
            write(ioout,'(a)')' 3 0 0 0 '

  301     CONTINUE
        endif

   4  continue

      return
      END

