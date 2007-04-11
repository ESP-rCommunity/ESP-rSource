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
      character H*72,path*72,outs*124,tg*1,ixpfile*72
      character longtfile*144,longtfiledos*144
      logical concat,remote,unixok
      integer lenxfile

C Initial name for export parameter file is xfile+par.
      lenxfile=lnblnk(xfile)
      if(xfile(1:1).ne.' '.and.lenxfile.gt.2)then
        write(ixpfile,'(a,a)') xfile(1:lnblnk(xfile)),'.par'
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
        write(outs,'(a,a)') 'closing export file: ',xfile
        call edisp(iuout,outs)
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
         h(2)='the model folder. Otherwise nominate a folder and file'
         h(3)='name for the export file (i.e. /tmp/junk.exp or'
         h(4)='/home/ralph/reports/junk.exp) '
         h(5)=' '
         h(6)='If you selected this option in error click continue.'
           write(outs,'(A,A)') ' The model is in folder ',path
           CALL EASKABC(outs,' place export file:',
     &       'in the model folder','user defined folder','continue',
     &       ixloc,6)
        else
          ixloc = 2   ! ixloc needs to be non-zero for subsequent code to work.
        endif
        h(1)='This file is a text file which can be used in'
        h(2)='reports or for third party tools. '
        call easks(xfile,' export file name: ',' ',72,
     &    ' ','export file name',IER,2)

        write(outs,'(3A)')' opened ',xfile(1:lnblnk(xfile)),
     &                       ' for export.'
        call edisp(iuout,outs)
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
        write(outs,'(1x,3A)') msg(:lnblnk(msg)),
     &    ' >> to ',xfile(1:lnblnk(xfile))
        call edisp(iuout,outs)
      endif
      RETURN
      END
