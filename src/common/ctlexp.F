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

C   CTLEXP: Control feedback to export wireframe or text feedback area.
C   CTLEXPMFS: mfs module varaitn for export of text/graphic feedback area.
C   CTLDUMPT: Control text dump (ctlexp T mode equiv without dialog).

C *********** CTLEXP ***********
C Control feedback to export wireframe or text feedback area.
C Assign user-specified export file - checking if local or remote.
C tg is a character T, X, or G specifying text, tabular or graphic
C   info being saved.
C xfile (width variable) is the initial name of the file to be exported
C msg (width variable) is prepended to dialog as export happens
C ixopen is a toggle reset each time ctlexp is called to signal open or close file.
C ixunit is the file unit number for the export file.
C ixloc signals whether path is local or remote and is passed back
C   to common block for retention until the second call to ctlexp

      subroutine ctlexp(xfile,ixopen,ixloc,ixunit,tg,msg,IER)
#include "help.h"

      integer lnblnk  ! function definition

      COMMON/OUTIN/IUOUT,IUIN,IEOUT
      common/rpath/path

C << need to find equivalent to autook common block variable >>
C << outs probably needs to be longer if xfile is longer >>
      character*(*) msg,xfile
      character path*72,outs*124,outs248*248,tg*1,ixpfile*144
      character longtfile*144,longtfiledos*144
      logical concat,remote,unixok
      integer lenxfile
      integer ISTRW

C Initial name for export parameter file is xfile+par.
      lenxfile=lnblnk(xfile)
      if(xfile(1:1).ne.' '.and.lenxfile.gt.2.and.lenxfile.lt.140)then
        write(ixpfile,'(2a)') xfile(1:lnblnk(xfile)),'.par'
      else
        ixpfile = 'data.grt.par'
      endif

C Check if Unix-based or DOS based.
      call isunix(unixok)
      helpinsub='ctlexp'  ! set for subroutine

C Each call is a toggle. If open then close text or graphics command
C file. If graphics, ask if file should be further converted.
      ixopen=ixopen+1
      if(ixopen.GT.1)ixopen=0
      if(ixopen.eq.0)then
        write(outs248,'(a,a)') 'closing export file: ',xfile
        call edisp(iuout,' ')
        call edisp248(iuout,outs248,100)
        if(tg.eq.'X')then
          CALL ERPFREE(ixunit,ISTAT)
        elseif(tg.eq.'T')then
          CALL ERPFREE(ixunit,ISTAT)
        elseif(tg.eq.'G')then
          if(ixloc.eq.1)then
            longtfile=' '
            if(unixok)then
              call addpath(xfile,longtfile,concat)
            else

C If running on a non-unix machine see if there are spaces in the name
C and change any / to \.
              call addpath(xfile,longtfile,concat)
              call cmdfiledos(longtfile,longtfiledos,ier)
              longtfile=' '
              longtfile=longtfiledos
            endif
          else
            write(longtfile,'(a)') xfile(1:lnblnk(xfile))
          endif
          call wwcsetend
          call wwcclose(longtfile)
        endif
      elseif(ixopen.eq.1)then
        ixloc=0
        remote=.false.
        call isunix(unixok)
        if(unixok)then
          if(path(1:2).ne.'./'.and.path(1:2).ne.'  ')remote=.true.
        else
          if(path(1:2).ne.'  ')remote=.true.
          if(ichar(path(1:1)).eq.46.and.
     &       ichar(path(2:2)).eq.92)remote=.false.
        endif
        if(remote)then
           write(outs,'(A,A)') 'The current path is: ',path
           call edisp(iuout,' ')
           call edisp(iuout,outs)
           helptopic='export_location'
           call gethelptext(helpinsub,helptopic,nbhelp)
           write(outs,'(A,A)') 'The model is in folder ',path
           CALL EASKABC(outs,'Place export file in:',
     &       'model folder','user-defined folder','cancel',
     &       ixloc,nbhelp)
           if(ixloc.eq.3)then
             ixopen=0
             return    ! does ixopen need to be reset?
           endif
        else
          ixloc = 2   ! ixloc needs to be non-zero for subsequent code to work.
        endif
        helptopic='export_file_type'
        call gethelptext(helpinsub,helptopic,nbhelp)
        iglib = igraphiclib()  ! find out if X11 or GTK or text support only.
        llt=lnblnk(xfile)
        if(iglib.eq.1.or.iglib.eq.3)then
          if(llt.lt.96)then
            ISTRW=96
          elseif(llt.ge.96.and.llt.lt.124)then
            ISTRW=124
          elseif(llt.ge.124.and.llt.le.144)then
            ISTRW=144
          endif
        elseif(iglib.eq.2)then
          ISTRW=144
        else
          ISTRW=96
        endif
        CALL EASKF(xfile,' ','Export file name?',
     &    ISTRW,'export.txt','binary events db',IER,nbhelp)

C If user requested cancel then return after resetting ixopen to zero.
        if(ier.eq.-3)then
          ixopen=0
          return
        endif

C The grt parameter file name might need to be changed.
        lenxfile=lnblnk(xfile)
        if(xfile(1:1).ne.' '.and.lenxfile.gt.2)then
          write(ixpfile,'(2a)') xfile(1:lnblnk(xfile)),'.par'
        else
          ixpfile = 'data.grt.par'
        endif

        write(outs248,'(3A)')' opened ',xfile(1:lnblnk(xfile)),
     &                       ' for export.'
        call edisp248(iuout,outs248,100)
        if(ixloc.eq.1)then
          if(tg.eq.'X')then
            call efopseq(ixunit,xfile,4,IER)
            if(ier.ne.0)return
          elseif(tg.eq.'T')then
            call efopseq(ixunit,xfile,4,IER)
            if(ier.ne.0)return
          elseif(tg.eq.'G')then

C Writing remotely, add the path to the given file name before
C passing request to wwlib.c
            longtfile=' '
            if(unixok)then
              call addpath(xfile,longtfile,concat)
            else

C If running on a non-unix machine see if there are spaces in the name
C and change any / to \.
              call addpath(xfile,longtfile,concat)
              call cmdfiledos(longtfile,longtfiledos,ier)
              longtfile=' '
              longtfile=longtfiledos
            endif
            call edisp248(iuout,longtfile,100)
            call edisp248(iuout,xfile,100)
            call wwcopen(longtfile)
            call wwcsetstart
          endif
        elseif(ixloc.eq.2)then
          if(tg.eq.'X')then
            CALL ERPFREE(ixunit,ISTAT)
            call FPOPEN(ixunit,ISTAT,1,3,xfile)
            if(ISTAT.lt.0)return
          elseif(tg.eq.'T')then
            CALL ERPFREE(ixunit,ISTAT)
            call FPOPEN(ixunit,ISTAT,1,3,xfile)
            if(ISTAT.lt.0)return
          elseif(tg.eq.'G')then
            write(longtfile,'(a)') xfile(1:lnblnk(xfile))
            call wwcopen(longtfile)
            call wwcsetstart
          endif
        elseif(ixloc.eq.3)then
          return
        endif
        write(outs248,'(1x,3A)') msg(1:lnblnk(msg)),
     &    ' >> to ',xfile(1:lnblnk(xfile))
        call edisp248(iuout,outs248,100)
      endif
      RETURN
      END

C ******************** CTLEXPMFS ********************
C Controls feedback to export file or text area.
C Assign user-specified export file - checking if local or remote.
C tg     - character T, X, or G specifying text, tabular or graphic
C          info being saved.
C xfile  - the initial name of the file to be exported.
C msg    - prepended to dialog as export happens.
C ixopen - a toggle reset each time ctlexp is called to signal open or close file.
C ixunit - file unit number for the export file.
      subroutine ctlexpmfs(xfile,ixopen,ixunit,ixpunit,tg,msg,IER)
#include "help.h"

      integer lnblnk  ! function definition

      COMMON/OUTIN/IUOUT,IUIN,IEOUT
      common/rcmd/LCMDFL
      common/rpath/path

      character*(*) msg,xfile
      character path*72,outs*124,outs248*248,tg*1
      character ixpfile*72,ttext*42,LCMDFL*144
      character longtfile*144,longtfiledos*144
      logical concat,remote,unixok
      integer lenxfile
      integer ISTRW

C Initial name for export parameter file is ixfile+par.
      lenxfile=lnblnk(xfile)
      if(xfile(1:1).ne.' '.and.lenxfile.gt.2)then
        write(ixpfile,'(2a)') xfile(1:lnblnk(xfile)),'.par'
      else
        ixpfile = 'data.grt.par'
      endif
      helpinsub='ctlexp'  ! set for subroutine

C Each call is a toggle.
      ixopen=ixopen+1
      if(ixopen.GT.1)ixopen=0
      if(ixopen.eq.0)then
        write(outs248,'(a,a)') 'closing: ',xfile(1:lnblnk(xfile))
        call edisp248(iuout,outs248,100)
        if(tg.eq.'X')then
          CALL ERPFREE(ixunit,ISTAT)
          CALL ERPFREE(ixpunit,ISTAT)
          write(outs248,'(2a)') 'closing parameter file: ',
     &      ixpfile
          call edisp248(iuout,outs248,100)
          call usrmsg(outs,'reporting >> to display ','-')
        elseif(tg.eq.'T')then
          CALL ERPFREE(ixunit,ISTAT)
          CALL ERPFREE(ixpunit,ISTAT)
        elseif(tg.eq.'G')then
          call wwcsetend
          call wwcclose(xfile)
        endif
      elseif(ixopen.eq.1)then
        iw=0
        remote=.false.
        call isunix(unixok)
        if(unixok)then
          if(path(1:2).ne.'./'.and.path(1:2).ne.'  ')remote=.true.
        else
          if(path(1:2).ne.'  ')remote=.true.
          if(ichar(path(1:1)).eq.46.and.
     &       ichar(path(2:2)).eq.92)remote=.false.
        endif
        if(remote)then
          write(outs,'(A,A)') 'The current path is: ',path
          call edisp(iuout,outs)
          helptopic='export_location'
          call gethelptext(helpinsub,helptopic,nbhelp)
          CALL EASKABC(outs,'Locate export file:',
     &      'in model folder','another folder','cancel',iw,nbhelp)
          if(iw.eq.3)then
            ixopen=0
            return    ! does ixopen need to be reset?
          endif
        endif
        helptopic='export_file_type'
        call gethelptext(helpinsub,helptopic,nbhelp)
        iglib = igraphiclib()  ! find out if X11 or GTK or text support only.
        llt=lnblnk(xfile)
        if(iglib.eq.1.or.iglib.eq.3)then
          if(llt.lt.96)then
            ISTRW=96
          elseif(llt.ge.96.and.llt.lt.124)then
            ISTRW=124
          elseif(llt.ge.124.and.llt.le.144)then
            ISTRW=144
          endif
        elseif(iglib.eq.2)then
          ISTRW=144
        else
          ISTRW=96
        endif
        CALL EASKF(xfile,' ','Export file name?',
     &    ISTRW,'export.txt','binary events db',IER,nbhelp)

C If user requested a cancel then return after resetting ixopen.
        if(ier.eq.-3)then
          ixopen=0
          return
        endif

        write(outs248,'(3A)')' opened ',xfile(1:lnblnk(xfile)),
     &                       ' for export.'
        call edisp248(iuout,outs248,100)
        if(iw.eq.1)then
          if(tg.eq.'X')then
            call efopseq(ixunit,xfile,4,IER)
            if(ier.ne.0)return
            write(ixpfile,'(a,a)') xfile(1:lnblnk(xfile)),'.par'
            call efopseq(ixpunit,ixpfile,4,IER)
            write(outs248,'(3A)')' opened ',ixpfile(1:lnblnk(ixpfile)),
     &                           ' for export parameters.'
            call edisp248(iuout,outs248,100)
          elseif(tg.eq.'T')then
            call efopseq(ixunit,xfile,4,IER)
            if(ier.ne.0)return
          elseif(tg.eq.'G')then

C Writing remotely, add the path to the given file name before
C passing request to wwlib.c
            call isunix(unixok)
            longtfile=' '
            if(unixok)then
              call addpath(xfile,longtfile,concat)
            else

C If running on a non-unix machine see if there are spaces in the name
C and change any / to \.
              call addpath(xfile,longtfile,concat)
              call cmdfiledos(longtfile,longtfiledos,ier)
              longtfile=' '
              longtfile=longtfiledos
            endif
            call wwcopen(longtfile)
            call wwcsetstart
          endif
        else
          if(tg.eq.'X')then
            CALL ERPFREE(ixunit,ISTAT)
            call FPOPEN(ixunit,ISTAT,1,3,xfile)
            if(ISTAT.lt.0)return
            write(ixpfile,'(a,a)') xfile(1:lnblnk(xfile)),'.par'
            CALL ERPFREE(ixpunit,ISTAT)
            call FPOPEN(ixpunit,ISTAT,1,3,ixpfile)
            write(outs248,'(3A)')' opened ',ixpfile(1:lnblnk(ixpfile)),
     &                           ' for export parameters.'
            call edisp248(iuout,outs248,100)
          elseif(tg.eq.'T')then
            CALL ERPFREE(ixunit,ISTAT)
            call FPOPEN(ixunit,ISTAT,1,3,xfile)
            if(ISTAT.lt.0)return
          elseif(tg.eq.'G')then
            call wwcopen(xfile)
            call wwcsetstart
          endif
        endif
        write(outs248,'(1x,3A)') msg(1:lnblnk(msg)),
     &    ' >> to ',xfile(1:lnblnk(xfile))
        call edisp248(iuout,outs248,100)

C If tabular info then create the headder to the xvgr parameter file.
        if(tg.eq.'X')then
          write(ixpunit,'(a)') '# ACE/gr parameter file'
          write(ixpunit,'(a)') '# '
          write(ixpunit,'(a)') 'page 5'
          write(ixpunit,'(a)') 'page inout 5'
          write(ixpunit,'(a)') 'link page off'
          write(ixpunit,'(a)') 'with g0'
          write(ixpunit,'(a)') 'g0 on'
          write(ixpunit,'(a)') 'g0 type xy'
          write(ixpunit,'(a)') 'g0 autoscale type AUTO'
          write(ixpunit,'(a)') '  default linestyle 1'
          write(ixpunit,'(a)') '  default linewidth 1'
          write(ixpunit,'(a)') '  default color 1'
          write(ixpunit,'(a)') '  default char size 1.0000'
          write(ixpunit,'(a)') '  default font 2'
          write(ixpunit,'(a)') '  default font source 0'
          write(ixpunit,'(a)') '  default symbol size 0.5000'
          write(ixpunit,'(a)') '  view xmin 0.15'
          write(ixpunit,'(a)') '  view xmax 0.85'
          write(ixpunit,'(a)') '  view ymin 0.15'
          write(ixpunit,'(a)') '  view ymax 0.85'

          helptopic='export_graph_title'
          call gethelptext(helpinsub,helptopic,nbhelp)
          ttext = 'Simulation Results'
          call easks(ttext,'Title for 3rd party graph: ',' ',42,
     &      'Simulation Results ','3rd party title',IER,nbhelp)
          write(ixpunit,'(3a)') 'title "',
     &      ttext(1:lnblnk(ttext)),'"'
          write(ixpunit,'(a)') 'title font 4'
          write(ixpunit,'(a)') 'title size 1.25'

C Include the name of the results library as a subtitle.
          write(ixpunit,'(3a)') 'subtitle "',
     &       LCMDFL(1:lnblnk(LCMDFL)),'"'
          write(ixpunit,'(a)') 'subtitle font 4'
          write(ixpunit,'(a)') 'subtitle size 1.00'

C Typical xaxis stuff.
          write(ixpunit,'(a)') '  xaxis  label "Day of year"'
          write(ixpunit,'(a)') '  xaxis  label place auto'
          write(ixpunit,'(a)') '  xaxis  label char size 1.0000'
          write(ixpunit,'(a)') '  xaxis  label font 4'
          write(ixpunit,'(a)') '  xaxis  label color 1'
          write(ixpunit,'(a)') '  xaxis  label linewidth 1'
          write(ixpunit,'(a)') '  xaxis  ticklabel on'
          write(ixpunit,'(a)') '  xaxis  ticklabel type auto'
          write(ixpunit,'(a)') '  xaxis  ticklabel layout horizontal'
          write(ixpunit,'(a)') '  xaxis  ticklabel  op bottom'
          write(ixpunit,'(a)') '  xaxis  ticklabel char size 0.75000'

C Typical yaxis stuff.
          write(ixpunit,'(a)') '  yaxis  tick on'
          write(ixpunit,'(a)') '  yaxis  label "Data"'
          write(ixpunit,'(a)') '  yaxis  label place auto'
          write(ixpunit,'(a)') '  yaxis  label char size 1.0000'
          write(ixpunit,'(a)') '  yaxis  label font 4'
          write(ixpunit,'(a)') '  yaxis  label color 1'
          write(ixpunit,'(a)') '  yaxis  label linewidth 1'
          write(ixpunit,'(a)') '  yaxis  ticklabel on'
          write(ixpunit,'(a)') '  yaxis  ticklabel type auto'
          write(ixpunit,'(a)') '  yaxis  ticklabel layout horizontal'
          write(ixpunit,'(a)') '  yaxis  ticklabel  op left'
          write(ixpunit,'(a)') '  yaxis  ticklabel char size 0.75000'
          write(ixpunit,'(a)') '  legend on'
          write(ixpunit,'(a)') '  legend vgap 2'
          write(ixpunit,'(a)') '  legend hgap 1'
          write(ixpunit,'(a)') '  legend x1 0.2'
          write(ixpunit,'(a)') '  legend y1 0.8'
          write(ixpunit,'(a)') '  legend length 4'
          write(ixpunit,'(a)') '  legend color 1 '
          write(ixpunit,'(a)') '  legend linestyle 1'
          write(ixpunit,'(a)') '  legend linewidth 1'
          write(ixpunit,'(a)') '  legend font 4'
          write(ixpunit,'(a)') '  legend char size 0.75000'
        endif
      endif
      RETURN
      END

C ******************** CTLDUMPT ********************
C Control toggle between dump to text feedback and file.
C xfile  - name of the file to be exported.
C msg    - prepended to dialog as export happens.
C ixopen - toggle reset each time ctlexp is called to signal open or close file.
C ixunit - file unit number for the export file.
C ixloc  - signals whether path is local or remote.

C (A variant of ctlexp which does not have a dialog for file name
C if the user is working remotely from the model.)
      subroutine ctldumpt(xfile,ixopen,ixloc,ixunit,msg,IER)

      integer lnblnk  ! function definition

      COMMON/OUTIN/IUOUT,IUIN,IEOUT
      common/rpath/path

      character*(*) msg,xfile
      character path*72,outs248*248
      logical remote,unixok

C Check if Unix-based or DOS based.
      call isunix(unixok)

C Each call is a toggle. If open then close text file.
      ixopen=ixopen+1
      if(ixopen.GT.1)ixopen=0
      if(ixopen.eq.0)then
        write(outs248,'(a,a)') 'closing export file: ',xfile
        call edisp248(iuout,outs248,100)
        CALL ERPFREE(ixunit,ISTAT)
      elseif(ixopen.eq.1)then
        ixloc=0
        remote=.false.
        if(unixok)then
          if(path(1:2).ne.'./'.and.path(1:2).ne.'  ')remote=.true.
        else
          if(path(1:2).ne.'  ')remote=.true.
          if(ichar(path(1:1)).eq.46.and.
     &       ichar(path(2:2)).eq.92)remote=.false.
        endif
        if(remote)then
          ixloc=1   ! put file in model folder
          call efopseq(ixunit,xfile,4,IER)
          if(ier.ne.0)return
        else
          ixloc = 2   ! ixloc needs to be non-zero for subsequent code to work.
          CALL ERPFREE(ixunit,ISTAT)
          call FPOPEN(ixunit,ISTAT,1,3,xfile)
          if(ISTAT.lt.0)return
        endif

        write(outs248,'(1x,3A)') msg(1:lnblnk(msg)),
     &    ' >> to ',xfile(1:lnblnk(xfile))
        call edisp248(iuout,outs248,100)
      endif
      RETURN
      END
