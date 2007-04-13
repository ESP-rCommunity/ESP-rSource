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

C ctlexp.F includes the following subroutines which are used
C by a number of ESP-r modules (a different version of ctlexp
C is used by esrumfs and esrures but those viariants may be
C moved into this file at some point in the future.

C   CTLEXP: Control feedback to export wireframe or text feedback area.
C   CTLEXPMFS: mfs module varaitn for export of text/graphic feedback area.

C CTLEXP ***********
C CTLEXP: Control feedback to export wireframe or text feedback area.
C Assign user-specified export file - checking if local or remote.
C tg is a character T, X, or G specifying text, tabular or graphic 
C   info being saved.
C xfile (width variable) is the initial name of the file to be exported
C msg (width variable) is prepended to dialog as export happens
C ixopen is a toggle reset each time ctlexp is called to signal open or close file.
C ixunit is the file unit number for the export file.
C ixloc signals whether path will is local or remote and is passed back
C   to common block for retention until the second call to ctlexp
      subroutine ctlexp(xfile,ixopen,ixloc,ixunit,tg,msg,IER)

      COMMON/OUTIN/IUOUT,IUIN
      common/pophelp/h(60)
      common/rpath/path

C << need to find equivalent to autook common block variable >>
C << outs probably needs to be longer if xfile is longer >>
      character*(*) msg,xfile
      character H*72,path*72,outs*124,outs248*248,tg*1,ixpfile*144
      character longtfile*144,longtfiledos*144
      logical concat,remote,unixok
      integer lenxfile

C Initial name for export parameter file is xfile+par.
      lenxfile=lnblnk(xfile)
      if(xfile(1:1).ne.' '.and.lenxfile.gt.2)then
        write(ixpfile,'(2a)') xfile(1:lnblnk(xfile)),'.par'
      else
        ixpfile = 'data.grt.par'
      endif

C Check if Unix-based or DOS based.
      call isunix(unixok)

C Each call is a toggle. If open then close text or graphics command
C file. For the graphics ask if file should be further converted.
      ixopen=ixopen+1
      if(ixopen.GT.1)ixopen=0
      if(ixopen.eq.0)then
        write(outs248,'(a,a)') 'closing export file: ',xfile
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
           write(outs,'(A,A)') ' The current path is: ',path
           call edisp(iuout,outs)
           h(1)='If using path then the export file will be placed in'
           h(2)='the model folder. Otherwise nominate a folder and'
           h(3)='file name for the export file (i.e. /tmp/junk.exp or'
           h(4)='/home/ralph/reports/junk.exp) '
           h(5)=' '
           h(6)='If you selected this option in error click continue.'
           write(outs,'(A,A)') ' The model is in folder ',path
           CALL EASKABC(outs,' place export file:',
     &       'in the model folder','user defined folder','continue',
     &       ixloc,6)
           if(ixloc.eq.3)then
            ixopen=0
             return    ! does ixopen need to be reset?
           endif
        else
          ixloc = 2   ! ixloc needs to be non-zero for subsequent code to work.
        endif
        h(1)='This file is a text file which can be used in'
        h(2)='reports or for third party tools. '
        iglib = igraphiclib()  ! find out if X11 or GTK or text support only.
        llt=lnblnk(xfile)
        if(iglib.eq.1.or.iglib.eq.3)then
          if(llt.lt.96)then
            CALL EASKF(xfile,' Export file name? ',
     &        ' ',96,'export.txt','export file name',IER,2)
          elseif(llt.ge.96.and.llt.lt.124)then
            CALL EASKF(xfile,' Export file name? ',
     &        ' ',124,'export.txt','export file name',IER,2)
          elseif(llt.ge.124.and.llt.le.144)then
            CALL EASKF(xfile,' Export file name? ',
     &        ' ',144,'export.txt','export file name',IER,2)
          endif
        elseif(iglib.eq.2)then
          CALL EASKF(xfile,' Export file name? ',
     &      ' ',144,'export.txt','export file name',IER,2)
        else
          CALL EASKF(xfile,' Export file name? ',
     &      ' ',96,'export.txt','binary events db',IER,2)
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

C CTLEXPMFS ***********
C This is a variant of code from esrures/utils.F which does not have
C the autook option built in. It does include code for creating the
C .par file contents and this might be compared with the code in
C esrucom/ctlexp.F to see if a merge might happen.

C CTLEXPMFS: Control feedback to export file or text feedback area.
C Assign user-specified export file - checking if local or remote.
C tg is a character T, X, or G specifying text, tabular or graphic 
C   info being saved.
C xfile (width variable) is the initial name of the file to be exported
C msg (width variable) is prepended to dialog as export happens
C ixopen is a toggle reset each time ctlexp is called to signal open or close file.
C ixunit is the file unit number for the export file.
      subroutine ctlexpmfs(xfile,ixopen,ixunit,ixpunit,tg,msg,IER)

      COMMON/OUTIN/IUOUT,IUIN
      common/pophelp/h(60)
      common/rcmd/LCMDFL
      common/rpath/path

      character*(*) msg,xfile
      character H*72,path*72,outs*124,outs248*248,tg*1
      character ixpfile*72,ttext*42,LCMDFL*144
      character longtfile*144,longtfiledos*144
      logical concat,remote,unixok
      integer lenxfile

C Initial name for export parameter file is ixfile+par.
      lenxfile=lnblnk(xfile)
      if(xfile(1:1).ne.' '.and.lenxfile.gt.2)then
        write(ixpfile,'(2a)') xfile(1:lnblnk(xfile)),'.par'
      else
        ixpfile = 'data.grt.par'
      endif

C Each call is a toggle.
      ixopen=ixopen+1
      if(ixopen.GT.1)ixopen=0
      if(ixopen.eq.0)then
        write(outs,'(a,a)') 'closing export file: ',xfile
        call usrmsg(outs,' ','-')
        if(tg.eq.'X')then
          CALL ERPFREE(ixunit,ISTAT)
          CALL ERPFREE(ixpunit,ISTAT)
          write(outs248,'(2a)') 'closing export parameter file: ',
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
          write(outs,'(A,A)') ' The current path is: ',path
          call edisp(iuout,outs)
          h(1)='If using path then the export file will be placed in'
          h(2)='the model folder. Otherwise nominate a folder and'
          h(3)='file name for the export file (i.e. /tmp/junk.exp or'
          h(4)='/home/ralph/reports/junk.exp) '
          h(5)=' '
          h(6)='If you selected this option in error click continue.'
          CALL EASKABC(outs,' place export file:',
     &      'in the model folder','user defined folder','continue',
     &      iw,6)
          if(iw.eq.3)then
            ixopen=0
            return    ! does ixopen need to be reset?
          endif
        endif
        h(1)='This file is a text file which can be used in'
        h(2)='reports or for third party tools. '
        iglib = igraphiclib()  ! find out if X11 or GTK or text support only.
        llt=lnblnk(xfile)
        if(iglib.eq.1.or.iglib.eq.3)then
          if(llt.lt.96)then
            CALL EASKF(xfile,' Export file name? ',
     &        ' ',96,'export.txt','export file name',IER,2)
          elseif(llt.ge.96.and.llt.lt.124)then
            CALL EASKF(xfile,' Export file name? ',
     &        ' ',124,'export.txt','export file name',IER,2)
          elseif(llt.ge.124.and.llt.le.144)then
            CALL EASKF(xfile,' Export file name? ',
     &        ' ',144,'export.txt','export file name',IER,2)
          endif
        elseif(iglib.eq.2)then
          CALL EASKF(xfile,' Export file name? ',
     &      ' ',144,'export.txt','export file name',IER,2)
        else
          CALL EASKF(xfile,' Export file name? ',
     &      ' ',96,'export.txt','binary events db',IER,2)
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

          h(1)='3rd party graphing usually has a main title...'
          ttext = 'Simulation Results'
          call easks(ttext,'Title for 3rd party graph: ',' ',42,
     &      'Simulation Results ','3rd party title',IER,1)
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
