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

C File dossupport.F providing manipulation of file names for DOS
C folders which may have spaces in the names.
C dbdoscopy - takes a source file and a destination file (with
C   path) and converts any forward slashes to backwards slashes and detects
C   spaces and creates and returns the doit command.

C dblongdoscopy - takes a source file (up to 144 char) and a 
C   destination file (with path up to 144 char) and converts any forward
C   slashes to backwards slashes and detects spaces and creates and returns 
C   an ldoit (300 char) command.

C cmdfiledos - takes a source file which might have spaces in the path
C as well as forward slashes and it modifies the file name to use
C backslashes and to bracket it in double quotes if there are spaces.
C source (up to 144 char) source file (with path).

C imgfiledos - takes a source file which might have spaces in the path
C as well as forward slashes and it modifies the file name to use
C backslashes but does NOT bracket it in double quotes if there are spaces.
C source (up to 144 char) source file (with path).
C

C ********** dbdoscopy *********
C dbdoscopy takes a source file and a destination file (with
C   path) and converts any forward slashes to backwards slashes and detects
C   spaces and creates and returns the doit command.
C source (72 char) source file (with path).
C lcopy (72 char) destination file (with path)
C message (48 char) feedback to echo when preparing doit
C doit (248 char) the command to execute
C ier is zero if ok.
      subroutine dbdoscopy(source,lcopy,message,doit,ier)
      common/OUTIN/IUOUT,IUIN

C Strings for dos version of source and destination of db file.
      character rootpdos*72,upathdos*72,dq*1,outs*124
      character source*72,lcopy*72,message*48,doit*248
      logical unixok
      integer lnrp  ! length of source file name
      integer lnblankrp ! position of blank in source file
      integer lnup  ! length of destination file name
      integer lnblankup ! position of blank in source file

C If running unix this subroutine should not have been called.
      call isunix(unixok)
      if(unixok)then
        call usrmsg('dbdoscopy is being called on a Unix/Linux/OSX',
     &    'computer environment - which could cause errors.','W')
      endif

C Make up a double quote for dos copy command, reset ier to zero
C and clear the doit return string buffer.
      dq = char(34)
      ier=0
      doit=' '

C Debug statements for native windows version. If lnblankrp or
C lnblankup is one then there are no spaces in the string so double quotes
C are not required.  But in either case it is useful to change any '/'
C in rootp and upath to '\'. 
      rootpdos=' '
      upathdos=' '
      lnrp=lnblnk(source)
      lnblankrp=iprevblnk(source,lnrp)
      call backslashit(source,rootpdos)
      write(outs,'(3a)') message,' ',rootpdos(1:lnrp)
      call edisp(iuout,outs)
C      write(outs,'(a,2i3)') 'lnrp lnblankrp ',lnrp,lnblankrp
C      call edisp(iuout,outs)

      lnup=lnblnk(lcopy)
      lnblankup=iprevblnk(lcopy,lnup)
      if(lnblankup.eq.1)then
        upathdos = lcopy
      else
        call backslashit(lcopy,upathdos)
      endif
      write(outs,'(2a)') 'destination = ',upathdos(1:lnup)
      call edisp(iuout,outs)
C      write(outs,'(a,2i3)') 'lnup lnblankup ',lnup,lnblankup
C      call edisp(iuout,outs)

C Create the doit command based on where there are blanks in the strings.
      if(lnblankrp.gt.1.and.lnblankup.gt.1)then

C There are blanks in both source or destination strings.
        write(doit,'(8a)',IOSTAT=IOS,ERR=552) 'copy /y ',
     &    dq,rootpdos(1:lnrp),dq,' ',dq,upathdos(1:lnup),dq
      elseif(lnblankrp.eq.1.and.lnblankup.gt.1)then

C There are blanks in destination string.
        write(doit,'(6a)',IOSTAT=IOS,ERR=552) 'copy /y ',
     &    rootpdos(1:lnrp),' ',dq,upathdos(1:lnup),dq
      elseif(lnblankrp.gt.1.and.lnblankup.eq.1)then

C There are blanks in source string.
        write(doit,'(6a)',IOSTAT=IOS,ERR=552) 'copy /y ',
     &    dq,rootpdos(1:lnrp),dq,' ',upathdos(1:lnup)
      elseif(lnblankrp.eq.1.and.lnblankup.eq.1)then

C There are no blanks in either source or destination strings.
        write(doit,'(4a)',IOSTAT=IOS,ERR=552) 'copy /y ',
     &    rootpdos(1:lnrp),' ',upathdos(1:lnup)
      endif

      return

C Errors.
 552  call usrmsg(
     &  'Error creating the dos copy command (might not work)',
     &  doit,'W')
      ier=1
      return
      end

C ********** dblongdoscopy *********
C dblongdoscopy - takes a source file (up to 144 char) and a 
C   destination file (with path up to 144 char) and converts any forward
C   slashes to backwards slashes and detects spaces and creates and returns 
C   an ldoit (300 char) command.
C source (144 char) source file (with path).
C lcopy (144 char) destination file (with path)
C message (48 char) feedback to echo when preparing doit
C ldoit (300 char) the command to execute
C ier is zero if ok.

      subroutine dblongdoscopy(source,lcopy,message,ldoit,ier)
      common/OUTIN/IUOUT,IUIN

C Strings for dos version of source and destination of db file.
      character rootpdos*144,upathdos*144,dq*1,louts*248
      character source*144,lcopy*144,message*48,ldoit*300
C      character outs*124
      logical unixok
      integer lnrp  ! length of source file name
      integer lnblankrp ! position of blank in source file
      integer lncp  ! length of destination file name
      integer lnblankup ! position of blank in source file

C Set folder separator (fs) to \ or / as required.
      call isunix(unixok)
      if(unixok)then
        call usrmsg('dbdoscopy is being called on a Unix/Linux/OSX',
     &    'computer environment - which could cause errors.','W')
      endif

C Make up a double quote for dos copy command, reset ier to zero
C and clear the ldoit return string buffer.
      dq = char(34)
      ier=0
      ldoit=' '

C Debug statements for native windows version. If lnblankrp or
C lnblankup is one then there are no spaces in the string so double quotes
C are not required.  But in either case it is useful to change any '/'
C in rootp and upath to '\'. 
      rootpdos=' '
      upathdos=' '
      lnrp=lnblnk(source)
      lnblankrp=iprevblnk(source,lnrp)
      call backslashit(source,rootpdos)
      write(louts,'(3a)') message,' ',rootpdos(1:lnrp)
      call edisp248(iuout,louts,100)
C      write(outs,'(a,2i3)') 'lnrp lnblankrp ',lnrp,lnblankrp
C      call edisp(iuout,outs)

C Change any / to \ and if lnblankup returns as a 1 then there
C was nothing to change so copy lcopyto upathdos.
      lncp=lnblnk(lcopy)
      lnblankup=iprevblnk(lcopy,lncp)
      if(lnblankup.eq.1)then
        upathdos = lcopy
      else
        call backslashit(lcopy,upathdos)
      endif
      write(louts,'(2a,2i3)') 'destination = ',upathdos(1:lncp),
     &  lncp,lnblankup
      call edisp248(iuout,louts,100)

C Create the ldoit command based on where there are blanks in the strings.
      if(lnblankrp.gt.1.and.lnblankup.gt.1)then

C There are blanks in both source or destination strings.
        write(ldoit,'(8a)',IOSTAT=IOS,ERR=552) 'copy /y ',
     &    dq,rootpdos(1:lnrp),dq,' ',dq,upathdos(1:lncp),dq
      elseif(lnblankrp.eq.1.and.lnblankup.gt.1)then

C There are blanks in destination string.
        write(ldoit,'(6a)',IOSTAT=IOS,ERR=552) 'copy /y ',
     &    rootpdos(1:lnrp),' ',dq,upathdos(1:lncp),dq
      elseif(lnblankrp.gt.1.and.lnblankup.eq.1)then

C There are blanks in source string.
        write(ldoit,'(6a)',IOSTAT=IOS,ERR=552) 'copy /y ',
     &    dq,rootpdos(1:lnrp),dq,' ',upathdos(1:lncp)
      elseif(lnblankrp.eq.1.and.lnblankup.eq.1)then

C There are no blanks in either source or destination strings.
        write(ldoit,'(4a)',IOSTAT=IOS,ERR=552) 'copy /y ',
     &    rootpdos(1:lnrp),' ',upathdos(1:lncp)
      endif

      return

C Errors.
 552  lsn=MIN0(lnblnk(ldoit),248)
      call lusrmsg(
     &  'Error creating the dos copy command (might not work)',
     &  ldoit(1:lsn),'W')
      ier=1
      return
      end

C ********** cmdfiledos *********
C cmdfiledos takes a source file which might have spaces in the path
C as well as forward slashes and it modifies the file name to use
C backslashes and to bracket it in double quotes if there are spaces.
C source (up to 144 char) source file (with path).
C patched (up to 144 char) revised file (with \ and possibly ")
C ier is zero if ok.
      subroutine cmdfiledos(source,patched,ier)
      common/OUTIN/IUOUT,IUIN

C Strings for dos version of source and destination of db file.
      character dq*1,outs*248
      character*(*) source,patched
      character sourcedos*144
      logical unixok
      integer lnrp  ! length of source file name
      integer lnblankdos ! position of blank in source file

C Set folder separator (fs) to \ or / as required.
C and a double quote character available in case of spaces in name.
      call isunix(unixok)
      if(unixok)then
        call usrmsg('cmdfiledos is being called on a Unix/Linux/OSX',
     &    'computer environment - which could cause errors.','W')
      else
        dq = char(34)
      endif

C Reset ier to zero
      ier=0
      lnrp=lnblnk(source)     ! what is the last char in string
      lnblankdos=iprevblnk(source,lnrp)  ! find location of space before that

C Debug the file name.
      write(outs,'(a,2i3,2a)',IOSTAT=IOS,ERR=552) 'dos name = ',lnrp,
     &  lnblankdos,' ',source(1:lnrp)
      call edisp248(iuout,outs,90)
      sourcedos=' '
      call backslashit(source,sourcedos)   ! change any forward to back slash.
      write(outs,'(2a)',IOSTAT=IOS,ERR=552) 'adjust slashes ',
     &  sourcedos(1:lnrp)
      call edisp248(iuout,outs,90)
      if(lnblankdos.gt.1)then
        patched=' '
        write(patched,'(3a)',IOSTAT=IOS,ERR=552) dq,sourcedos(1:lnrp),dq
        write(outs,'(2a)') 'quoted is ',patched(1:lnblnk(patched))
        call edisp248(iuout,outs,90)
      else
        patched=' '
        patched=sourcedos
        write(outs,'(2a)',IOSTAT=IOS,ERR=552) 'patched is ',
     &    patched(1:lnblnk(patched))
        call edisp248(iuout,outs,80)
      endif

      return

C Errors.
 552  call usrmsg('Error creating dos file name (might not work)',
     &  patched,'W')
      ier=1
      return
      end


C ********** imgfiledos *********
C imgfiledos takes a source file which might have spaces in the path
C as well as forward slashes and it modifies the file name to use
C backslashes but does NOT bracket it in double quotes if there are spaces.
C source (up to 144 char) source file (with path).
C patched (up to 144 char) revised file (with \)
C ier is zero if ok.
      subroutine imgfiledos(source,patched,ier)
      common/OUTIN/IUOUT,IUIN

C Strings for dos version of source and destination of db file.
      character outs*248
      character*(*) source,patched
      character sourcedos*144
      logical unixok
      integer lnrp  ! length of source file name
      integer lnblankdos ! position of blank in source file

C Set folder separator (fs) to \ or / as required.
C and a double quote character available in case of spaces in name.
      call isunix(unixok)
      if(unixok)then
        call usrmsg('cmdfiledos is being called on a Unix/Linux/OSX',
     &    'computer environment - which could cause errors.','W')
      else
        continue
      endif

C Reset ier to zero
      ier=0
      lnrp=lnblnk(source)     ! what is the last char in string
      lnblankdos=iprevblnk(source,lnrp)  ! find location of space before that

C Debug the file name.
      write(outs,'(a,2i3,2a)',IOSTAT=IOS,ERR=552) 'dos name = ',lnrp,
     &  lnblankdos,' ',source(1:lnrp)
      call edisp248(iuout,outs,90)
      sourcedos=' '
      call backslashit(source,sourcedos)   ! change any forward to back slash.
      write(outs,'(2a)',IOSTAT=IOS,ERR=552) 'adjust slashes ',
     &  sourcedos(1:lnrp)
      call edisp248(iuout,outs,90)

      patched=' '
      patched=sourcedos
      write(outs,'(2a)',IOSTAT=IOS,ERR=552) 'patched is ',
     &  patched(1:lnblnk(patched))
      call edisp248(iuout,outs,80)

      return

C Errors.
 552  call usrmsg('Error creating dos file name (might not work)',
     &  patched,'W')
      ier=1
      return
      end

