C This file is part of the ESP-r system.
C Copyright Energy Systems Research Unit, University of
C Strathclyde, Glasgow, Scotland, 2001.

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

C The file esru_ib.f is a collection of low-level ESP-r fortran
C libraries that are applicable to all versions of ESP-r (that is,
C X11, GTK and noX). Code pretaining to version-specific configurations
C should be placed in the libGTK, libX11 and libNONGtk files.

C  st2name: Given `string' swap blanks & wildcards to _ return as `name'.
C  st2file: Given `string' strip blanks & wildcards and return as `name'.
C  backslashit: Given `string' swap / to \ : return as `name'.
C  iprevblnk: Given a string, return position of blank just before ipos.
C  inextblnk: Given a string, return position of blank just after ipos.
C  icutstr: Given a string, cuts icut characters from position ipos.
C  EASKAB:  Generic A/B choice facility returning 1 or 2 according to which
C           of the choices has been chosen.
C  EASKABC: Generic A/B/C choice facility returning 1, 2 or 3 according
C           to which of the choices has been chosen.
C  EASKABCD: Generic A/B/C/D choice facility returning 1-4 according
C           to which of the choices has been chosen.

C  DAYCLK:  Print day, month, day no. and time based on the julian day & time.
C  FDROOT:  Given a file name see if it contains a path.
C  EFOPSEQ: Open a sequential file with existance flag & path check.
C  EFOPRAN: Open a random access file with existance flag & path check.
C  ADDPATH: Return file name appended onto the path and logical concat.
C  GETTOKENS checks a string, returning nb of tokens and array of tokens.
C  C2FSTR:  Convert c function returned string to fortran format.
C  EKPAGE:  Maps key characters, pages & array indexs in long display lists.
C  KEYIND:  Decodes EMENU index and returns the array index of the item.
C  EPMENSV: saves menu definitions (common block PMENU).
C  EPMENRC: recovers menu definitions (common block PMENU) from PMENUSV.
C  EPAGE:   Screen control: page without waiting.
C  EPAGEW:  Screen control: Wait before paging.
C  EPWAIT:  Screen control: Wait without paging.
C  EPAGEND: Screen control: Page then close window if open.


C ************** st2name
C Given `string' swap blanks & wildcards to _ : return as `name'.
      SUBROUTINE st2name(string,name)
      CHARACTER*(*) string,name
      character phrase*124

C Clear variables and get string lengths.
      name = ' '
      phrase = ' '
      ilname = LEN(name)
      illstr = max(1,lnblnk(string))

C Strip off any leading blanks from string, substitute `_'
C between words and for any ` \ / ~ & * ( ) ^ # < > ' ` " '.
C Stop copying if end of name reached.
      K=0
      DO 99 I=1,illstr
        IF(string(I:I).NE.' '.OR.K.GE.1)THEN
C          write(6,'(a,I4)')string(I:I),ichar(string(I:I))
          if(ichar(string(I:I)).lt.32)goto 100
          if(I.gt.ilname)goto 100
          K=K+1
          if(string(I:I).eq.' ')then
            phrase(K:K)='_'
          elseif(string(I:I).eq.'/')then
            phrase(K:K)='_'
          elseif(string(I:I).eq.char(34))then
            phrase(K:K)='_'
          elseif(string(I:I).eq.char(39))then
            phrase(K:K)='_'
          elseif(string(I:I).eq.char(44))then
            phrase(K:K)='_'
          elseif(string(I:I).eq.char(91))then
            phrase(K:K)='_'
          elseif(string(I:I).eq.char(92))then
            phrase(K:K)='_'
          elseif(string(I:I).eq.char(93))then
            phrase(K:K)='_'
          elseif(string(I:I).eq.char(96))then
            phrase(K:K)='_'
          elseif(string(I:I).eq.'*')then
            phrase(K:K)='_'
          elseif(string(I:I).eq.'~')then
            phrase(K:K)='_'
          elseif(string(I:I).eq.'&')then
            phrase(K:K)='_'
          elseif(string(I:I).eq.'(')then
            phrase(K:K)='_'
          elseif(string(I:I).eq.')')then
            phrase(K:K)='_'
          elseif(string(I:I).eq.'>')then
            phrase(K:K)='_'
          elseif(string(I:I).eq.'<')then
            phrase(K:K)='_'
          elseif(string(I:I).eq.'^')then
            phrase(K:K)='_'
          elseif(string(I:I).eq.'#')then
            phrase(K:K)='_'
          else
            phrase(K:K)=string(I:I)
          endif
        ENDIF
 99   CONTINUE
100   continue
      LN=max(1,lnblnk(phrase))
      write(name,'(a)') phrase(1:LN)

      return
      end

C ************** st2file
C st2file: Given `string' strip blanks & wildcards and return as `name'.
C Useful to check file names.
      SUBROUTINE st2file(string,name)
      CHARACTER*(*) string,name
      character phrase*124

C Clear variables and get string lengths.
      name = ' '
      phrase = ' '
      ilname = LEN(name)
      illstr = max(1,lnblnk(string))

C Strip off any leading blanks from string, compact spaces and tabs
C between words and substitute _ for any ` & * ( ) ^ # < > ' ` " ' or commas.
C Stop copying if end of name reached.
      K=0
      DO 99 I=1,illstr
        if(ichar(string(I:I)).eq.0)goto 99
        if(ichar(string(I:I)).eq.9)goto 99
        if(ichar(string(I:I)).eq.32)goto 99
        if(ichar(string(I:I)).lt.32)goto 100
        if(I.gt.ilname)goto 100
C        write(6,'(a,I4)')string(I:I),ichar(string(I:I))
        K=K+1
        if(string(I:I).eq.char(34))then
          phrase(K:K)='_'
        elseif(string(I:I).eq.char(39))then
          phrase(K:K)='_'
        elseif(string(I:I).eq.char(44))then
          phrase(K:K)='_'
        elseif(string(I:I).eq.char(91))then
          phrase(K:K)='_'
        elseif(string(I:I).eq.char(93))then
          phrase(K:K)='_'
        elseif(string(I:I).eq.char(96))then
          phrase(K:K)='_'
        elseif(string(I:I).eq.'*')then
          phrase(K:K)='_'
        elseif(string(I:I).eq.'~')then
          phrase(K:K)='_'
        elseif(string(I:I).eq.'&')then
          phrase(K:K)='_'
        elseif(string(I:I).eq.'(')then
          phrase(K:K)='_'
        elseif(string(I:I).eq.')')then
          phrase(K:K)='_'
        elseif(string(I:I).eq.'>')then
          phrase(K:K)='_'
        elseif(string(I:I).eq.'<')then
          phrase(K:K)='_'
        elseif(string(I:I).eq.'^')then
          phrase(K:K)='_'
        elseif(string(I:I).eq.'#')then
          phrase(K:K)='_'
        else
          phrase(K:K)=string(I:I)
        endif
 99   CONTINUE
100   continue
      LN=max(1,lnblnk(phrase))
      write(name,'(a)') phrase(1:LN)

      return
      end

C ************** backslashit
C backslashit: Given `string' swap / to \ : return as `name'.
      SUBROUTINE backslashit(string,name)
      CHARACTER*(*) string,name
      character phrase*144,bs*1

C Clear variables and get string lengths.
      name = ' '
      phrase = ' '
      ilname = LEN(name)
      illstr = max(1,lnblnk(string))

C Make up a \ character.
      bs = char(92)

C Strip off any leading blanks from string, substitute \ for /
C between words and dtop copying if end of name reached.
      K=0
      DO 99 I=1,illstr
        IF(string(I:I).NE.' '.OR.K.GE.1)THEN
C          write(6,'(a,I4)')string(I:I),ichar(string(I:I))
          if(ichar(string(I:I)).lt.32)goto 100
          if(I.gt.ilname)goto 100
          K=K+1
          if(string(I:I).eq.'/')then
            phrase(K:K)=bs
          else
            phrase(K:K)=string(I:I)
          endif
        ENDIF
 99   CONTINUE
100   continue
      LN=max(1,lnblnk(phrase))
      write(name,'(a)') phrase(1:LN)

      return
      end

C ********** iprevblnk(string,ipos)
C prevblnk: given a string, return position of blank just before ipos.
      function iprevblnk(string,ipos)
      character*(*) string
      character a*1,b*1
      integer right

C right is the defined length of string.
      right=len(string)
      lnb=lnblnk(string)

C If requested position beyond the length of the string return the
C declared length of the string.
      if(ipos.gt.right)then
        iprevblnk=right
        return
      endif

C If requrested position is beyond lnblnk then return lnblnk.
      if(ipos.gt.lnb)then
        iprevblnk=lnb
        return
      endif

C Set position for checking to ipos
      iprevblnk=ipos
      right=ipos
 42   continue
      right=right-1
      if(right.le.0)then

C If position zero reached set to one and return.
        iprevblnk = 1
        return
      else

C If the current character is non blank then continue leftwards.
        a=string(right:right)
        if(right.gt.1)then
          b=string(right-1:right-1)
          if(ichar(a).gt.32)then
            goto 42
          elseif(ichar(a).eq.32.or.ichar(a).eq.9)then

C If the current character is a blank and the character to its
C left is also a blank carry on stepping leftwards, otherwise return.
            if(ichar(b).eq.32.or.ichar(b).eq.9)then
              goto 42
            else
              iprevblnk = right
              return
            endif
          endif
        else

C If the current position is 1 do not bother with position zero.
          if(ichar(a).gt.32)then
            goto 42
          elseif(ichar(a).eq.32.or.ichar(a).eq.9)then
            iprevblnk = right
            return
          endif
        endif
      endif
      end

C ********** inextblnk(string,ipos)
C inextblnk: given a string, return position of blank just after ipos
C (or the end of the string if that happens first).
      function inextblnk(string,ipos)
      character*(*) string
      character a*1,b*1
      integer right

C right is the defined length of string.
      right=len(string)
      lnb=lnblnk(string)

C If requested position beyond the length of the string return the
C declared length of the string.
      if(ipos.gt.right)then
        inextblnk=right
        return
      endif

C If requrested position is beyond lnblnk then return lnblnk.
      if(ipos.gt.lnb)then
        inextblnk=lnb
        return
      endif

C Set position for checking to ipos
      inextblnk=ipos
      right=ipos
 42   continue
      right=right+1
      if(right.ge.lnb)then

C If position zero reached end of string set to lnb and return.
        inextblnk = lnb
        return
      else

C If the current character is non blank then continue rightwards.
        a=string(right:right)
        b=string(right+1:right+1)
        if(ichar(a).gt.32)then
          goto 42
        elseif(ichar(a).eq.32.or.ichar(a).eq.9)then

C If the current character is a blank and the character to its
C left is also a blank carry on stepping leftwards, otherwise return.
          if(ichar(b).eq.32.or.ichar(b).eq.9)then
            goto 42
          else
            inextblnk = right
            return
          endif
        endif
      endif
      end

C ************* icutstr(string,ipos,icut)
C icutstr: given a string, cuts icut characters from position ipos
C and shifts latter portion of the string down icut chars. Returns
C zero if no error.
      function icutstr(string,ipos,icut)
      character*(*) string
      integer right

C right is the defined length of string.
      right=len(string)
      lnb=lnblnk(string)

C If requested text to cut is beyond the length of the string return
C with no action.
      if(ipos+icut.gt.right)then
        icutstr=1
        return
      endif

C If requested text to cut starts within the string, but carrys on
C beyond the lnblnk then shorten string to ipos.
      if(ipos.lt.lnb.and.ipos+icut.gt.lnb)then
        write(string,'(a)') string(1:ipos)
        icutstr=0
        return
      else
        write(string,'(2a)') string(1:ipos),string(ipos+icut:lnb)
        icutstr=0
        return
      endif
      end





C  ERPFREE: Is used to close any file.
C  EFDELET: Delete the current file opened under IUN and return ISTAT.
C  FPOPEN:  is used to open a file with a name.
C  FPRAND:  is used to open a file with a name for random access.


C These routines are used to open a user-specified file
C with a meaningful error message if this is not possible.

C << with a bit of work the 'e' version calls can do this
C << task as long as the path is temporarily cleared
C << this would simplify much of the logic.

C  FPOPEN  open an ascii file - use EFOPSEQ instead for model files.
C  FPRAND  open a binary file - use EFOPRAN instead for model files.

C File opening parameter conventions:
C  IUN   is the unit number on which the file is to be opened;
C  ISTAT   returns a status value as follows:-

C   >0     file was successfully opened
C   -1     input line was a ?
C   -2     input line was not recognisable as a file title,
C          and an error message has been output
C   -3     input line (file name)was 'NONE' NOTHING OPENED,RETURN

C   -300   file already exists (  IXIST  =2), and
C          an error message has been output
C   -301   file does not exist, but no error message has
C          been output (  IXIST=0  )

C other <0 error opening file, and an error message has been
C          output (value returned is minus the value returned in the
C          second argument of a call to   ERRSNS)

C MODE determines the access mode of the file as follows:-
C
C  1-3  read and write
C    4  append (write only on to end of file)

C   FOR FORMATTED I/O LET MODE -VE BE FOR UNFORMATTED I/O
C   -1 TO -4   AS ABOVE FOR UNFORMATTED SEQUENTIAL ACCESS

C   >4 <0   Length of direct access record

C IXIST determines whether the file is expected to exist:-
C   0  file is expected to exist; return with
C      ISTAT=-301   if it does not, but
C      do not output an error message.  This
C      can be used to test for existence of a default
C      file.

C   1  file is expected to exist; error if it does not
C   2  file is expected not to exist; error if it does
C   3  file used if it exists; zero length file created if not


C ****************** FPOPEN
C FPOPEN Opens the file with standard parameter conventions for
C IUN, ISTAT, MODE, and IXIST;  FNARG is the file name.
        SUBROUTINE FPOPEN(IUN,ISTAT,MODE,IXIST,FNARG)
        CHARACTER*(*) FNARG
        LOGICAL        XST

        lfn=max(1,LNBLNK(fnarg))

        IF (FNARG(1:min(lfn,7)).eq.'UNKNOWN'
     &        .or.FNARG(1:min(lfn,2)).eq.'  ') THEN
          WRITE (6,*)' !+Files named UNKNOWN cannot be opened'
          ISTAT=-400
          RETURN
        ENDIF

C Check if file exists and its type
        INQUIRE (FILE=FNARG,EXIST=XST)

        IF (XST.AND.(IXIST.EQ.2)) THEN
            WRITE (6,*)' !+File already exists',fnarg
            ISTAT=-300
            RETURN
        ENDIF

        IF (.NOT.XST.AND.(IXIST.LE.1)) THEN

C File does not exist, set error and return.
          IF ( IXIST .EQ. 1 )THEN
            call edisp(6,' File doesn`t exist: ')
            call edisp(6,FNARG)
          ENDIF
          ISTAT=-301
          RETURN
        ENDIF


C Check mode:   mode 1-4,5-8 is sequential access
        IF ((MODE.LT.0).OR.(MODE.GT.10)) THEN

C Open file for direct access.
            IF (MODE.GT.0) THEN
              MODE = MODE - 10
              IF (XST) THEN

C ASCII RECORDS,4 CHAR PER WORD,RECL IN CHARS
                OPEN (IUN,FILE=fnarg(1:lfn),ACCESS='DIRECT',RECL=MODE*4,
     &                  FORM='FORMATTED',STATUS='OLD',IOSTAT=ISTAT)
              ELSE
                OPEN (IUN,FILE=fnarg(1:lfn),ACCESS='DIRECT',RECL=MODE*4,
     &                  FORM='FORMATTED',STATUS='NEW',IOSTAT=ISTAT)
              ENDIF
            ELSE

C***BINARY UNFORMATTED D.A. FILE
C ON VAX/VMS  RECL=  -MODE WORDS PER RECORD
C ON UNIX     RECL=  -MODE*4  BYTES PER RECORD

              MODE= -MODE
              IF (XST) THEN
                OPEN (IUN,FILE=fnarg(1:lfn),ACCESS='DIRECT',RECL=MODE*4,
     &               FORM='UNFORMATTED',STATUS='OLD',IOSTAT=ISTAT)
              ELSE
                OPEN (IUN,FILE=fnarg(1:lfn),ACCESS='DIRECT',RECL=MODE*4,
     &               FORM='UNFORMATTED',STATUS='NEW',IOSTAT=ISTAT)
              ENDIF
            ENDIF
        ELSE

c Sequential access.
C***V3.4  ADD READONLY FOR MODE=1 OR 5 FOR VAX SYSTEMS
C  THIS SHOULD BE REMOVED IF YOUR SYSTEM DOESNT SUPPORT 'READONLY'
         IF(MODE.EQ.1)THEN
C  MODE=1-4 FORMATTED (DEFAULT)
            IF (XST) THEN
              OPEN (IUN,FILE=fnarg(1:lfn),ACCESS='SEQUENTIAL',
     &              STATUS='OLD',IOSTAT=ISTAT)
            ELSE
              OPEN (IUN,FILE=fnarg(1:lfn),ACCESS='SEQUENTIAL',
     &              STATUS='NEW',IOSTAT=ISTAT)
            ENDIF
         ELSE If(MODE.LT.5) THEN
C READ & WRITE MODE=1-4 FORMATTED (DEFAULT)
            IF (XST) THEN
              OPEN (IUN,FILE=fnarg(1:lfn),ACCESS='SEQUENTIAL',
     &              STATUS='OLD',IOSTAT=ISTAT)
            ELSE
              OPEN (IUN,FILE=fnarg(1:lfn),ACCESS='SEQUENTIAL',
     &              STATUS='NEW',IOSTAT=ISTAT)
            ENDIF

C MODE =5  READONLY UNFORMATTED.
         ELSE IF (MODE .EQ. 5) THEN
           IF (XST) THEN
             OPEN (IUN,FILE=fnarg(1:lfn),FORM='UNFORMATTED',
     &             ACCESS='SEQUENTIAL',STATUS='OLD',IOSTAT=ISTAT)
           ELSE
             OPEN (IUN,FILE=fnarg(1:lfn),FORM='UNFORMATTED',
     &             ACCESS='SEQUENTIAL',STATUS='NEW',IOSTAT=ISTAT)
           ENDIF

         ELSE IF (MODE .LT. 9) THEN
c READ & WRITE Unformatted.
            IF (XST) THEN
              OPEN (IUN,FILE=fnarg(1:lfn),FORM='UNFORMATTED',
     &              ACCESS='SEQUENTIAL',STATUS='OLD',IOSTAT=ISTAT)
            ELSE
              OPEN (IUN,FILE=fnarg(1:lfn),FORM='UNFORMATTED',
     &              ACCESS='SEQUENTIAL',STATUS='NEW',IOSTAT=ISTAT)
            ENDIF
          endif
        ENDIF

C***3.5
      IF(ISTAT.EQ.0 .AND. .NOT. XST) ISTAT=1

      RETURN
      END

C ******************* FPRAND
C  Opens a random access file with name FNARG.
C  LENG determines the record length and type as follows:-
C       <0      -( LENG ) characters per record, ASCII records.

C       =0      128 words per record,   IMAGE   MODE RECORDS
C               (BUFFER COUNT IS FORCED TO 1 TO ENSURE WRITES
C               ARE PERFORMED IN ORDER REQUESTED)
C
C       >0      (LENG  ) words per record, ff BINARY   records
C IUN,ISTAT, and IXIST are the same as standard conventions.
        SUBROUTINE FPRAND(IUN,ISTAT,LENG,IXIST,FNARG)
        CHARACTER*(*) FNARG
        lfn = lnblnk(FNARG)

        IF (FNARG(1:min(lfn,7)).eq.'UNKNOWN') THEN
          WRITE (6,*)' !+Files named UNKNOWN cannot be opened'
          ISTAT=-400
          RETURN
        ENDIF

C Convert length to mode for FPOPEN.
        IF (LENG.EQ.0) MODE=-128
        IF (LENG.GT.0) MODE=-LENG
        IF (LENG.LT.0) MODE=10-LENG
C Open file.
        CALL FPOPEN (IUN,ISTAT,MODE,IXIST,FNARG)
        RETURN
        END


c ******************** ERPFREE
C ERPFREE is used to close any file.
      SUBROUTINE ERPFREE(IUN,ISTAT)
      LOGICAL OPND
      IF(IUN.NE.0)THEN
        INQUIRE(IUN,OPENED=OPND)
        IF(OPND)CLOSE(IUN)
        ISTAT=0
      ELSE
        CALL USRMSG(' ',' Unable to free error channel! ','W')
        ISTAT=1
      ENDIF
      RETURN
      END

c ******************** EFDELET
C EFDELET: Delete file IUN and return ISTAT for compatibility.
      SUBROUTINE EFDELET(IUN,ISTAT)
      LOGICAL OPND
      IF(IUN.NE.0)THEN
        INQUIRE(IUN,OPENED=OPND)
        IF(OPND)CLOSE(IUN,STATUS='DELETE')
        ISTAT=0
      ELSE
        CALL USRMSG(' ',' Unable to delete error channel! ','W')
        ISTAT=1
      ENDIF
      RETURN
      END

C ******************** EASKAB
C EASKAB Generic choice facility returning 1 or 2 according to which
C of the choices has been chosen.
C MSG1 and MSG2 are prompts (upto 124 char) to be printed.
C AOPT & BOPT are the text strings describing the
C options available, (these should not be too long).
      SUBROUTINE EASKAB(MSG1,MSG2,AOPT,BOPT,IWHICH,NHELP)

      CHARACTER*(*) MSG1,MSG2,AOPT,BOPT
      character COPT,DOPT*2,EOPT*2,FOPT*2,GOPT*2

      COPT=' '
      DOPT=' '
      EOPT=' '
      FOPT=' '
      GOPT=' '
      call EASKATOG(MSG1,MSG2,AOPT,BOPT,COPT,DOPT,EOPT,FOPT,GOPT,
     &  IWHICH,NHELP)
      RETURN
      END

C ******************** EASKABC
C EASKABC Generic choice facility returning 1, 2 or 3 according to which
C of the choices has been chosen. See EASKATOG for parameter documentation.
      SUBROUTINE EASKABC(MSG1,MSG2,AOPT,BOPT,COPT,IWHICH,NHELP)
      COMMON/SPAD/MMOD,LIMIT,LIMTTY

      CHARACTER*(*) MSG1,MSG2,AOPT,BOPT,COPT
      character DOPT*2,EOPT*2,FOPT*2,GOPT*2

      DOPT=' '
      EOPT=' '
      FOPT=' '
      GOPT=' '
      call EASKATOG(MSG1,MSG2,AOPT,BOPT,COPT,DOPT,EOPT,FOPT,GOPT,
     &  IWHICH,NHELP)
      RETURN
      END

C ******************** EASKABCD
C EASKABCD Generic choice facility returning 1, 2, 3 or 4 according to which
C of the choices has been chosen. See EASKATOG for parameter documentation.
      SUBROUTINE EASKABCD(MSG1,MSG2,AOPT,BOPT,COPT,DOPT,IWHICH,NHELP)
      COMMON/SPAD/MMOD,LIMIT,LIMTTY

      CHARACTER*(*) MSG1,MSG2,AOPT,BOPT,COPT,DOPT
      character EOPT*2,FOPT*2,GOPT*2

      EOPT=' '
      FOPT=' '
      GOPT=' '
      call EASKATOG(MSG1,MSG2,AOPT,BOPT,COPT,DOPT,EOPT,FOPT,GOPT,
     &  IWHICH,NHELP)
      RETURN
      END


C ******************** bSPEqual ******************
C Determine if two SP numbers are within machine round-off error.
C
C The floating point computations in this function are
C based on the following paper:
C
C Goldberg, David (1991). "What Every Computer Scientest Should Know
C   About Floating Point Arithmetic", Computing Surveys, March.
C
C   (Available at:
C      http://www.physics.ohio-state.edu/~dws/grouplinks/floating_point_math.pdf
C    as of 8-8-2007)
C

      logical function bSPEqual(fFloat1, fFloat2)
      implicit none

C Passed floats
      real fFloat1, fFloat2

C Significand portions of passed floats
      real fSig1, fSig2

C Exponential portion of passed floats.
      integer iExp1, iExp2

C Upper and lower bounds for exponential root-search
      integer iExpLB, iExpUB
      
C Signs associated with passed floats
      integer iSign1, iSign2
      integer bit_size

      logical bExpFound

C Flag for initialization
      logical bInitialized
      data bInitialized/.false./
      save bInitialized

C min-exponent for system
      integer iMin_exp
      save iMin_exp

C Round-off error
      real fRoundoffError

      logical bClose

C Components for float:

      integer iFloatBasis
      integer iFloatPrecision
      save iFloatBasis
      save iFloatPrecision
      
!       print*, "bSPEqual: comparing ", fFloat1, " and ", fFloat2
!
      call eclose ( fFloat1, fFloat2, 1.0E-03, bClose )
      if ( .not. bclose ) then
        bSPEqual = .false.
        return
      endif

C Establish the minimum exponent distinguisable on the system
      if ( .not. bInitialized ) then

C Initialize base and precision parameters according to
C IEEE 754.

        iFloatBasis = 2
        
        select case  ( bit_size( iExp1 ) )

C Single precision
          case (32)

            iFloatPrecision = 24
            iMin_exp        = -126
           
C Single-extended precision
          case (43)

            iFloatPrecision = 32
            iMin_exp        = -1022

C Double precisision
          case (64)

            iFloatPrecision = 43
            iMin_exp        = -1022

C Double-extended precision
          case (79)

            iFloatPrecision = 79
            iMin_exp        = -16382

          case default

C Perhaps a better error message could go here?
            STOP "bSPEqual: System is not IEEE 754 conformant."

        end select

        bInitialized = .true.

      endif
      
!       print*, "iMin_exp:", iMin_exp, "iFloatPrecision:",iFloatPrecision

C Establish signs for arguments, and convert to absolute avalues.

      if ( fFloat1 .LT. 0.0 ) then
        iSign1 = -1
        fFloat1 = fFloat1 * (-1.0)
      else
        iSign1 =  1
      endif

      if ( fFloat2 .LT. 0.0 ) then
        iSign2 = -1
        fFloat2 = fFloat2 * (-1.0)
      else
        iSign2 =  1
      endif 

C Determine the exponent of the first arguement.
      bExpFound = .false. 
      iExpLB = -1 * iMin_exp
      iExpUB =      iMin_exp + 1

      do while ( .not. bExpFound )

        iExp1 = int( float ( iExpLB + iExpUB ) / 2.0 )

        fSig1 = fFloat1 / 2.0 ** float ( iExp1 )

!         print*, "  - iExp1:", iExp1, "fSig1:", fSig1, iExpLB, iExpUB,
!      &          iMin_exp

        if ( fSig1 * real ( iSign1 ) .lt. 1.0 ) then

          iExpLB = iExp1
        
        elseif ( fSig1 * real ( iSign1 ) .ge. 2.0 ) then

          iExpUB = iExp1

        else

          bExpFound = .true.

        endif 
      
      enddo

!       print*, "Exponent of arg 1:", iExp1

C Determine the exponent of second arguement.
      bExpFound = .false. 
      iExpLB = -1 * iMin_exp
      iExpUB =      iMin_exp + 1

      do while ( .not. bExpFound )


        iExp2 = int( float ( iExpLB + iExpUB ) / 2.0 )

        fSig2 = fFloat2 / 2.0 ** float ( iExp2 )

!         print*, "  - iExp2:", iExp2, "fSi2:", fSig2, iExpLB, iExpUB,
!      &          iMin_exp

        if ( fSig2 * real ( iSign2 ) .lt. 1.0 ) then

          iExpLB = iExp2
        
        elseif ( fSig2 * real ( iSign2 ) .ge. 2.0 ) then

          iExpUB = iExp2

        else

          bExpFound = .true.

        endif 
      
      enddo

!       print*, "Exponent of arg 2:", iExp2


C Now compute potential round-off error.
C
C Error is:
C
C     ( ( basis / 2 ) * basis ^ ( - precision ) ) * basis ^ exponent 
C
C (See Goldberg 1991).

      fRoundoffError =
     &    (   ( real ( iFloatBasis ) / 2.0 )
     &      * real ( iFloatBasis ) ** real ( -1 * iFloatPrecision )
     &     ) *
     &     real ( iFloatBasis ) ** real ( max ( iExp1, iExp2 ) )

C Finally, compare difference between arguements with potential
C round-off error. Report numbers as equal if round-off error
C exceeds difference.

      if ( ABS ( fFloat1 - fFloat2 ) .lt. fRoundoffError ) then

        bSPEqual = .true.

      else

        bSPEqual = .false.

      endif 

!       print*, "Round-off error:", fRoundoffError, abs(fFloat1 - fFloat2)
!       print*, "bSPEqual result:", bSPEqual

      return
      end

C ******************** DAYCLK
C DAYCLK will print the day, month, day no. and time based on the day
C of the year IYD and clock time in hours TIME.
      SUBROUTINE DAYCLK(IYD,TIME,ITU)

      CHARACTER*5 DAY(4)
      CHARACTER*5 RMONTH(12)
      character outs*124

      DATA DAY/'st of','nd of','rd of','th of'/
      DATA RMONTH/' Jan.',' Feb.',' Mar.',' Apr.',' May.',' June',
     &' July',' Aug.',' Sept',' Oct.',' Nov.',' Dec.'/

      CALL EDAYR(IYD,ID,IM)
      IF(TIME.LE.24.0)goto  1
      TIME=TIME-24.0
      IYD=IYD+1
    1 IS=4
      IF(ID.EQ.1.OR.ID.EQ.21.OR.ID.EQ.31)IS=1
      IF(ID.EQ.2.OR.ID.EQ.22)IS=2
      IF(ID.EQ.3.OR.ID.EQ.23)IS=3
      IHR=IFAX(TIME)
      REM=(TIME-IHR)*60.0
      IMIN=INT(REM)
      IF(IMIN.LT.10.0)THEN
        WRITE(outs,4)IYD,ID,DAY(IS),RMONTH(IM),IHR,IMIN
    4   FORMAT(' Day No. = ',I3,' (i.e. ',I2,A5,A5,'),  Time ',
     &         I2,':0',I1,' Hours')
        call edisp(itu,outs)
      ELSE
        WRITE(outs,3)IYD,ID,DAY(IS),RMONTH(IM),IHR,IMIN
    3   FORMAT(' Day No. = ',I3,' (i.e. ',I2,A5,A5,'),  Time ',
     &         I2,':',I2,' Hours')
        call edisp(itu,outs)
      ENDIF
      RETURN
      END

C ******* FDROOT
C Given a file name (fstring) see if it contains a (path) and a (filen).
C Note: mingw compilers require fstring to be declaired length (not *(*).
      SUBROUTINE fdroot(fstring,path,filen)
      COMMON/OUTIN/IUOUT,IUIN
      CHARACTER*(*) path,filen,fstring
C      character fstring*72
      character fs*1
      logical unixok
      integer ilbfstr  ! last actual char in fstring
      integer ipos     ! the leftward moving character position
      integer ilstr    ! declaired length of fstring
      integer ilenfilen ! declaired length of filen to be returned
      integer ilenpath ! declaired length of path to be returned

C Set fileseparator depending on OS.
      call isunix(unixok)
      if(unixok)then
        fs = char(47)
      else
        fs = char(92)
      endif

C Clear variables and get string lengths.
      filen = ' '
      ilbfstr = lnblnk(fstring)
      ipos = ilbfstr
      ilstr = LEN(fstring)
      ilenfilen = LEN(filen)
      ilenpath = LEN(path)

C Loop backwards from last non-blank character until a separator is found.
 344  continue
      ipos=ipos-1
      if(fstring(ipos:ipos).eq.fs)then
        filen = ' '
        write(filen,'(a)',IOSTAT=IOS,ERR=1) fstring(ipos+1:ilbfstr)
        path = ' '
        if(ipos.gt.ilenpath)then
          write(path,'(a)',IOSTAT=IOS,ERR=1) fstring(1:ilenpath)
        else
          write(path,'(a)',IOSTAT=IOS,ERR=1) fstring(1:ipos)
        endif
        return
      elseif(fstring(ipos:ipos).eq.'/')then
        filen = ' '
        write(filen,'(a)',IOSTAT=IOS,ERR=1) fstring(ipos+1:ilbfstr)
        path = ' '
        if(ipos.gt.ilenpath)then
          write(path,'(a)',IOSTAT=IOS,ERR=1) fstring(1:ilenpath)
        else
          write(path,'(a)',IOSTAT=IOS,ERR=1) fstring(1:ipos)
        endif
        return
      else

C Arrived at the first character. There have been no file separators
C so filen is fstring (truncated if necessary) and the path becomes
C the equivalent of where-i-am-now.
        if(ipos.eq.1)then
C          call edisp(iuout,'No folder separator found.')
          if(ilstr.ge.ilenfilen)then
            filen = ' '
            write(filen,'(a)',IOSTAT=IOS,ERR=1)fstring(1:ilenfilen)
            write(path,'(a1,a1)',IOSTAT=IOS,ERR=1) '.',fs
            return
          elseif(ilstr.lt.ilenfilen)then
            write(filen,'(a)')fstring(1:lnblnk(fstring))
            write(path,'(a1,a1)',IOSTAT=IOS,ERR=1) '.',fs
            return
          endif
        elseif(ipos.gt.1)then
          goto 344
        endif
      endif

      return

   1  if(IOS.eq.2)then
        call edisp(iuout,
     &  'fdroot: permission error composing path or file from string.')
        call edisp(iuout,fstring)
      else
        call edisp(iuout,
     &    'fdroot: error composing path or file from string.')
        call edisp(iuout,fstring)
      endif
      return
      end
      
C ******************** EFOPSEQ
C EFOPSEQ: Open a sequential ASCII file and return appropriate
C messages and error status.
C IUN is the file unit number, SFILE is the file name.
C IEXIST is a flag denoting:
C   0 - should exist, no message but error -301 if not.
C   1 - should exist, message & error -301 if not.
C   2 - should not exist, error 300 & message if exists.
C   3 - if exists use otherwise create.
C   4 - if exists ask user before overwriting otherwise create.
C IER is the error status (0 is OK).
C If the path is other than `./' but is not `/usr/esru' then
C concatenate path with sfile.

      SUBROUTINE EFOPSEQ(IUN,SFILE,IEXIST,IER)
C      COMMON/OUTIN/IUOUT,IUIN
      common/pophelp/h(60)

      LOGICAL XST,OK,concat,DOK
      CHARACTER*(*) SFILE
C      character outs*144
      CHARACTER LFIL*72,h*72
      character longtfile*144

      LFIL=' '
      longtfile=' '

C Debug.
C      WRITE(outs,'(2A)')' EFOPSEQ: ',SFILE(1:lnblnk(SFILE))
C      call edisp(iuout,outs)

C Add path to file if required then get length of file name.
      XST=.false.
      if(SFILE(1:2).eq.'  ')then
        IER=-301
        return
      elseif(SFILE(1:4).eq.'UNKN')then
        IER=-301
        return
      elseif(SFILE(1:4).eq.'unkn')then
        IER=-301
        return
      endif

      call addpath(SFILE,longtfile,concat)

 10   ltf=max(1,LNBLNK(longtfile))
      IF(IUN.NE.0)CALL ERPFREE(IUN,ISTAT)
      INQUIRE (FILE=longtfile(1:ltf),EXIST=XST)

C Depending on whether the file exists or not process according
C to the value of IEXIST:
      IF(IEXIST.EQ.0.OR.IEXIST.EQ.1)THEN
        IF(XST)THEN
          OPEN(IUN,FILE=longtfile(1:ltf),ACCESS='SEQUENTIAL',
     &            STATUS='OLD',IOSTAT=ISTAT)
        ELSE

C File should exist, print message if IEXIST=1 and not found.
          IF(IEXIST.EQ.1)THEN
            CALL LUSRMSG('Could not find',longtfile,'W')
            IER=-301
            RETURN
          elseif(IEXIST.EQ.0)THEN
            IER=-301
            RETURN
          ENDIF
        ENDIF
      ELSEIF(IEXIST.EQ.2)THEN

C File should not exist, error=-300 if found.
        IF(XST)THEN
          CALL LUSRMSG(longtfile,'already exists (and should not)!',
     &      'W')
          IER=-300
          RETURN
        ELSE
          OPEN(IUN,FILE=longtfile(1:ltf),ACCESS='SEQUENTIAL',
     &            STATUS='NEW',IOSTAT=ISTAT)
        ENDIF
      ELSEIF(IEXIST.EQ.3)THEN

C Use existing file or create a new one.
        IF(XST)THEN
          OPEN(IUN,FILE=longtfile(1:ltf),ACCESS='SEQUENTIAL',
     &            STATUS='OLD',IOSTAT=ISTAT)
        ELSE
          OPEN(IUN,FILE=longtfile(1:ltf),ACCESS='SEQUENTIAL',
     &            STATUS='NEW',IOSTAT=ISTAT)
        ENDIF
      ELSEIF(IEXIST.EQ.4)THEN

C IEXIST=4 if file exists, ask user if it should be deleted and
C overwritten with a file by the same name.
        IF(XST)THEN
          OPEN(IUN,FILE=longtfile(1:ltf),ACCESS='SEQUENTIAL',
     &            STATUS='OLD',IOSTAT=ISTAT)
          DOK=.true.
          H(1)='The displayed file exists. Please confirm if you '
          H(2)='want to overwrite it. A no allows you to rename.'
          H(3)='You might want to rename the current file, if '
          H(4)='for example you made a mistake and want to '
          H(5)='create a temporary file name.'
          NHELP=5
          CALL ASKOK(longtfile,'Overwrite this file?',OK,DOK,NHELP)
          IF(OK)THEN
            CALL EFDELET(IUN,ISTAT)
            OPEN(IUN,FILE=longtfile(1:ltf),ACCESS='SEQUENTIAL',
     &           STATUS='NEW',IOSTAT=ISTAT)
          ELSE

C Close the initial file before asking the user for new file.
  83        CALL ERPFREE(IUN,ISTAT)
            H(1)='Give the file name relative to the configuration file'
            H(2)='location.'
            H(3)='If you choose to remember the new file name, it will'
            H(4)='be included in the model. If, for example you made a '
            H(5)='mistake and enter a temporary file name, this will'
            H(6)='not affect your model. '
            write(LFIL,'(a)')SFILE(1:lnblnk(SFILE))
            CALL EASKS(LFIL,'Revised file name?',' ',72,' ',
     &        'revised file name',IER,6)
            IF(LFIL(1:2).eq.'  ')goto 83
            call addpath(LFIL,longtfile,concat)
            DOK=.true.
            NHELP=6
            CALL ASKOK(' ',' Remember the new file name?',
     &        OK,DOK,NHELP)
            if(OK)SFILE=LFIL
            goto 10
          ENDIF
        ELSE
          OPEN(IUN,FILE=longtfile(1:ltf),ACCESS='SEQUENTIAL',
     &            STATUS='NEW',IOSTAT=ISTAT)
        ENDIF
      ELSE
        IER=1
        RETURN
      ENDIF

C If problem in opening then report difficulty.
      IF(ISTAT.LT.0)THEN
        CALL LUSRMSG('Error opening ',longtfile,'W')
        IER=ISTAT
      ENDIF

      RETURN
      END

C ******************** EFOPRAN
C EFOPRAN: Open a random access file and return appropriate
C messages and error status.
C IUN is the file unit number, SFILE is the file name, IRW is
C record width.  IEXIST is a flag denoting:
C   0 - should exist, no message but error -301 if not.
C   1 - should exist, message & error -301 if not.
C   2 - should not exist, error 300 & message if exists.
C   3 - if exists use otherwise create.
C   4 - if exists ask user before overwriting otherwise create.
C IER is the error status (0 is OK).
C If the path is other than `./' but is not `/usr/esru' then
C concatenate path with sfile.

      SUBROUTINE EFOPRAN(IUN,SFILE,IRW,IEXIST,IER)
C      COMMON/OUTIN/IUOUT,IUIN
      common/pophelp/h(60)

      LOGICAL XST,OK,concat,DOK
      CHARACTER*(*) SFILE
C      character outs*144
      CHARACTER LFIL*72,h*72
      character longtfile*144

C Debug.
C      WRITE(outs,'(A,A)')' EFOPRAN: ',SFILE(1:lnblnk(SFILE))
C      call edisp(iuout,outs)

      LFIL=' '
      longtfile=' '
      XST=.false.
      if(SFILE(1:2).eq.'  ')then
        IER=-301
        return
      elseif(SFILE(1:4).eq.'UNKN')then
        IER=-301
        return
      elseif(SFILE(1:4).eq.'unkn')then
        IER=-301
        return
      endif

      call addpath(SFILE,longtfile,concat)

C Length of file.
  10  ltf=max(1,LNBLNK(longtfile))
      IF(IUN.NE.0)CALL ERPFREE(IUN,ISTAT)
      INQUIRE (FILE=longtfile(1:ltf),EXIST=XST)

C Depending on whether the file exists or not process according
C to the value of IEXIST:
      IF(IEXIST.EQ.0.OR.IEXIST.EQ.1)THEN
        IF(XST)THEN
          OPEN(IUN,FILE=longtfile(1:ltf),ACCESS='DIRECT',
     &        RECL=IRW*4,FORM='UNFORMATTED',STATUS='OLD',IOSTAT=ISTAT)
        ELSE

C File should exist, print message if IEXIST=1 and not found.
          IF(IEXIST.EQ.1)THEN
            CALL LUSRMSG('Could not find',longtfile,'W')
            IER=-301
            RETURN
          elseif(IEXIST.EQ.0)THEN
            IER=-301
            RETURN
          ENDIF
        ENDIF
      ELSEIF(IEXIST.EQ.2)THEN

C File should not exist, error=-300 if found.
        IF(XST)THEN
          CALL LUSRMSG(longtfile,
     &      'already exists (and should not)!','W')
          IER=-300
          RETURN
        ELSE
          OPEN(IUN,FILE=longtfile(1:ltf),ACCESS='DIRECT',
     &        RECL=IRW*4,FORM='UNFORMATTED',STATUS='NEW',IOSTAT=ISTAT)
        ENDIF
      ELSEIF(IEXIST.EQ.3)THEN

C Use existing file or create a new one.
        IF(XST)THEN
          OPEN(IUN,FILE=longtfile(1:ltf),ACCESS='DIRECT',
     &        RECL=IRW*4,FORM='UNFORMATTED',STATUS='OLD',IOSTAT=ISTAT)
        ELSE
          OPEN(IUN,FILE=longtfile(1:ltf),ACCESS='DIRECT',
     &        RECL=IRW*4,FORM='UNFORMATTED',STATUS='NEW',IOSTAT=ISTAT)
        ENDIF
      ELSEIF(IEXIST.EQ.4)THEN

C IEXIST=4 if file exists, ask user if it should be deleted and
C overwritten with a file by the same name.
        IF(XST)THEN
          OPEN(IUN,FILE=longtfile(1:ltf),ACCESS='DIRECT',
     &        RECL=IRW*4,FORM='UNFORMATTED',STATUS='OLD',IOSTAT=ISTAT)
          DOK=.true.
          H(1)='The displayed file exists. Please confirm if you '
          H(2)='want to overwrite it. A no allows you to rename.'
          H(3)='You might want to rename the current file, if '
          H(4)='for example you made a mistake and want to '
          H(5)='create a temporary file name.'
          NHELP=5
          CALL ASKOK(longtfile,'Overwrite this file? ',OK,DOK,NHELP)
          IF(OK)THEN
            CALL EFDELET(IUN,ISTAT)
            OPEN(IUN,FILE=longtfile(1:ltf),ACCESS='DIRECT',
     &        RECL=IRW*4,FORM='UNFORMATTED',STATUS='NEW',IOSTAT=ISTAT)
          ELSE

C Close the initial file before asking the user for new file.
C Pass back through with alternative file name.
  83        CALL ERPFREE(IUN,ISTAT)
            H(1)='Give the file name relative to the configuration file'
            H(2)='location.'
            H(3)='If you choose to remember the new file name, it will'
            H(4)='be included in the model. If, for example you made a '
            H(5)='mistake and enter a temporary file name, this will'
            H(6)='not affect your model. '
            write(LFIL,'(a)')SFILE(1:lnblnk(SFILE))
            CALL EASKS(LFIL,' File name?',' ',72,' ','file name',IER,6)
            if(LFIL(1:2).eq.'  ')goto 83
            call addpath(LFIL,longtfile,concat)
            goto 10
          ENDIF
        ELSE
          OPEN(IUN,FILE=longtfile(1:ltf),ACCESS='DIRECT',
     &        RECL=IRW*4,FORM='UNFORMATTED',STATUS='NEW',IOSTAT=ISTAT)
        ENDIF
      ELSE
C        CALL USRMSG(' ','Unknown existence flag passed!','W')
        IER=1
        RETURN
      ENDIF

C If problem in opening then report difficulty.
      IF(ISTAT.LT.0)THEN
        CALL LUSRMSG('Error opening file ',longtfile,'W')
        IER=ISTAT
      ENDIF

      RETURN
      END

C ******************** FINDFIL
C FINDFIL: Check for existance of a file SFILE (with path) & return XST.
C If the path is other than `./' but is not `/usr/esru' then
C concatenate path with sfile before looking.  If file name is
C blank or UNKNOWN then return XST=.FALSE.

      SUBROUTINE FINDFIL(SFILE,XST)

      LOGICAL xst,concat
      CHARACTER SFILE*72,longtfile*144
      integer ltf   ! position of last character in the string.

      xst=.false.
      if(SFILE(1:2).eq.'  ')return
      if(SFILE(1:4).eq.'UNKN')return
      if(SFILE(1:4).eq.'unkn')return
      call addpath(SFILE,longtfile,concat)

C Does the file exist?
      ltf=max(1,LNBLNK(longtfile))
      INQUIRE (FILE=longtfile(1:ltf),EXIST=xst)

      RETURN
      END

C ******************** addpath
C ADDPATH: Return file name appended onto the path and logical concat.
C IUOUT is the message channel, SFILE is the file name.
C If the path does not begin with '/' or '?:' then
C concatenate path with sfile.
C If path is '  ' or './' do not concatenate
C This version includes cross-platform logic.
      SUBROUTINE addpath(SFILE,tfile,concat)
      COMMON/OUTIN/IUOUT,IUIN
      common/rpath/path
      LOGICAL concat,unixok
      CHARACTER*(*) sfile,tfile
      CHARACTER path*72,fs*1,fs1*1,tp*12,tp1*12,outs*124,a1*1
C      character a2*1

C Define path and file name lengths and check operating system.
      LN=max(1,lnblnk(path))
      LNS=max(1,lnblnk(sfile))
      call isunix(unixok)

C Set folder separator (fs) to \ or / and the folders to ignore as required.
      fs = char(47)
      fs1 = char(92)
      write(tp,'(2a)') '.',fs
      write(tp1,'(2a)') '.',fs1
      write(a1,'(a)') sfile(1:1)
C      write(a2,'(a)') sfile(2:2)

C Assume that we will add the path to the file name.
      concat = .true.

      if (path(1:2).eq.tp(1:2).or.path(1:2).eq.tp1(1:2).or.
     &    path(1:2).eq.'  ') then

C Path is defined starting ./ or .\ or we are running locally (path='  ')
C therefore do not add path.
        concat = .false.
      elseif(unixok)then

C If running on unix and sfile begins with a / then an absolute path has
C been defined - do not add path.
        if (a1.eq.'/') then
          concat = .false.
        elseif (((ICHAR(a1).gt.64.and.ICHAR(a1).lt.91).or.
     &           (ICHAR(a1).gt.96.and.ICHAR(a1).lt.123)).and.
     &            sfile(2:2).eq.':') then

C Special case: windows absolute path being used on unix - substitute /usr/ for initial c:\
C and change all \ to /.
          concat = .false.
          write (tfile,'(2a)') '/usr/',sfile(4:LNS)
          LT=max(1,lnblnk(tfile))
          sfile=tfile
          do 10 I=1,LT
            if (tfile(I:I).eq.fs1) then
              write (sfile(I:I),'(a2)') fs
            endif
 10       continue
          write(outs,*)
     &      'addpath: warning found windows path changing to unix.'
          call edisp(iuout,outs)
          write(outs,*) 'addpath: ',sfile
          call edisp(iuout,outs)
        endif
      else

C If running on windows and sfile begins ?: then an absolute path has
C been defined - do not add path.
        if (((ICHAR(a1).gt.64.and.ICHAR(a1).lt.91).or.
     &       (ICHAR(a1).gt.96.and.ICHAR(a1).lt.123)).and.
     &        sfile(2:2).eq.':') then
          concat = .false.
        elseif (a1.eq.'/') then

C Special case: unix absolute path being used on windows - substitute c: for initial /
          concat = .false.
          write (tfile,'(2a)') 'C:',sfile(5:LNS)
          sfile=tfile
          write(outs,*)
     &      'addpath: warning found unix path changing to windows.'
          call edisp(iuout,outs)
          write(outs,*) 'addpath: ',sfile
          call edisp(iuout,outs)
        endif
      endif

      if(concat)then
        LN=max(1,lnblnk(path))
        LNS=max(1,lnblnk(sfile))
        write(tfile,'(2a)',iostat=ios,err=1)path(1:LN),sfile(1:LNS)
      else
        tfile=sfile
      endif
      return
   1  if(IOS.eq.2)then
        call lusrmsg('addpath: permission error writing file name',
     &    tfile,'W')
      else
        call lusrmsg('addpath: error writing file name',tfile,'W')
      endif
      return
      end

C ********************* GETTOKENS
C GETTOKENS checks a character string (A), returning the number of data
C items (IW) separated by a file separator and an array of tokens/words
C (up to 12 32character words returned) that were in the string.
C Useful for parsing file paths.  Note to keep from
C overwriting the string passed, deal with a copy.
      SUBROUTINE GETTOKENS(A,IW,WORDS)
      CHARACTER*(*) A
      CHARACTER*32 WORDS(12)
      CHARACTER B*124,C*1,fs*1
      logical unixok

C Determine the file separator and clear the WORDS to return.
      call isunix(unixok)
      if(unixok)then
        fs = char(47)
      else
        fs = char(92)
      endif
      do 9 i=1,12
        WORDS(i)='  '
  9   continue

      lastcc = MIN0(LNBLNK(A),123)
      WRITE(B,'(A)',IOSTAT=ios,ERR=1) A(1:lastcc+1)

C Loop through B and convert all fileseparators to blanks.
      DO 100 I=1,LNBLNK(B)+1
        C=B(I:I)
        IF(ICHAR(C).EQ.47.or.ICHAR(C).eq.92) B(I:I)=' '
  100 CONTINUE

C Loop through B and see how many 'words'.
      K=0
      IW=0
  102 IW=IW+1

C Start by skipping blanks before the word.
   10 K=K+1
      C=B(K:K)
      IF(C.EQ.' ') GOTO 10

C Loop character by character until separator is found.
   20 K=K+1
      C=B(K:K)
      if(K.GE.LNBLNK(B))then

C Have determined the number of tokes so loop through each and
C assign WORDS.
        if(IW.gt.0)then
          KK=0
          do 42 ij=1,IW
            CALL EGETW(B,KK,WORDS(ij),'W','token',IER)
            if(ier.ne.0)goto 2
  42      continue
        endif
        RETURN
      endif
      IF(C.EQ.' ')GOTO 102
      GOTO 20
   1  if(IOS.eq.2)then
        write(6,*) 'GETTOKENS: permission error in internal write: ',A
      else
        write(6,*) 'GETTOKENS: error in internal write: ',A
      endif
      return
   2  if(IOS.eq.2)then
        write(6,*) 'GETTOKENS: permission error in getting token: ',A
      else
        write(6,*) 'GETTOKENS: error in getting token: ',A
      endif
      return

      END

C ************ c2fstr
C Convert c function returned string to fortran format.
C Strip off the last character (end of line mark) as well as any
C leading blanks from string returned.

C Noticed problems in the native windows compile with null
C characters. This might be because of the direct copy was
C used if windows detected and this preserves unprintable
C characters. Revised to use the same logic for both Unix and
C Dos.
      SUBROUTINE c2fstr(cstr,fstr)
      CHARACTER*(*) cstr,fstr

      fstr = ' '    ! clear the return string
      K=0
      DO 99 I=1,LEN(cstr)
        IF(cstr(I:I).NE.' '.OR.K.GE.1)THEN
C         WRITE(6,'(a,I4)')cstr(I:I),ichar(cstr(I:I))
          if(ichar(cstr(I:I)).lt.32)goto 100
          K=K+1
          if(K.gt.LEN(fstr))goto 100
          fstr(K:K)=cstr(I:I)
        ENDIF
 99   CONTINUE
 100  return

C   1  write(6,*) 'c2fstr: error writing: ',cstr
C      return
      end

C ******* termode
C Given an index `mode` return a terminal text string.
      SUBROUTINE termode(mode,tmode)
      CHARACTER*(*) tmode
      tmode = ' '
      if(mode.eq.-1)then
        tmode = 'text'
      elseif(mode.eq.-2)then
        tmode = 'page'
      elseif(mode.eq.8)then
        tmode = 'graphic'
      elseif(mode.eq.-6)then
        tmode = 'script'
      endif

      RETURN
      END

C ***************** EKPAGE
C EKPAGE maps key characters, pages & array indexs in long display lists.
C No menu should have more than 26 data selections (plus heading and
C control lines) so that key characters (a-z) for arrays will not be
C repeated. Notes on variables:
C IPACT  : page option, if IPACT= CREATE then only update the common
C          block PMENU, otherwise ask for action to be taken.
C IPM,MPM: current and last page of menu,
C IST    : is the data array index at the beginning of the menu.
      SUBROUTINE EKPAGE(IPACT)
#include "epara.h"

      COMMON/SPAD/MMOD,LIMIT,LIMTTY
      common/pophelp/h(60)
      CHARACTER H*72

C If creating menu don't ask questions. Allow movement forward or backwards
C in the control menu.
      IGTP=1  ! if creating assume first page
      IF(IPACT.EQ.CREATE)THEN
        IX=1
      ELSEIF(IPACT.lt.0)then
        IX=3
        IGTP=IABS(IPACT)
      ELSE

C If only two pages then automatically switch to the page not being displayed,
C otherwise ask what to do.
        if (MPM.eq.2) then
          if (IPM.eq.1) then
            IX=5
          else
            IX=2
          endif
        else
          H(1)='In order to manage long lists of selections or data'
          H(2)='a number of viewports may be necessary.'
          CALL EASKABC(' ',' Menu-page options:','previous','next',
     &                 'goto page',IW,2)
          IF(IW.EQ.3)then
            H(1)='If page greater than length then last page assumed.'
            CALL EASKI(IGTP,' ',' Which page ? ',1,'F',1,'-',1,
     &        'menu page',IER,1)
            IX=3
          elseif(IW.EQ.2)then
            IX=5
          elseif(IW.EQ.1)then
            IX=2
          endif
          CALL USRMSG(' ',' ','-')
        endif
      ENDIF
      IF(IX.EQ.1)THEN

C Build list from beginning of the item array, find out how many lines
C are allowed in a menu on this screen (MFULL), how many items can
C fit in the main section of the menu (MIFULL) and limit this to 26
C so that the automaticly supplied key character remains within the
C range a-z.  If more items that allowed set tag for paging.
        IF(MMOD.LT.8)THEN
          MFULL=LIMTTY-2
        ELSEIF(MMOD.EQ.8)THEN
          MFULL=30
        ENDIF
        MIFULL=MFULL-(MHEAD+MCTL)
        IF(MIFULL.GT.26)THEN
          MIFULL=26
          MFULL=MIFULL+MHEAD+MCTL
        ENDIF
        IF(ILEN.LE.MIFULL)THEN
          IPFLG=0
        ELSE
          IPFLG=1
        ENDIF
        IST=1
      ELSEIF(IX.EQ.2)THEN

C Move to previous page and rebuild list.
        IPACT=PREV
        IF(IPM.GT.1)THEN
          IST=IST-MIFULL
        ENDIF
      ELSEIF(IX.EQ.3)THEN

C Move to a particular page. Begin at first and increment.
        if(IGTP.eq.1)then
          IST=1
        else
          IST=1
          do 42 ij=2,IGTP
            IF((IST+MIFULL).LE.ILEN)IST=IST+MIFULL
  42      continue
        endif
      ELSEIF(IX.EQ.5)THEN

C Move to the next page and rebuild list if not the last page.
        IPACT=NEXT
        IF((IST+MIFULL).LE.ILEN)THEN
          IST=IST+MIFULL
        ENDIF
      ELSE

C Unknown option.
        RETURN
      ENDIF

C Determine current page and last page of menu. Base the current page
C number on the starting item of the current menu unless the last page.
      PAGE=(FLOAT(IST+MIFULL-1)/FLOAT(MIFULL))
      IF(PAGE.LT.1.0)PAGE=1.0
      IPM=INT(PAGE)
      PAGE=(FLOAT(ILEN)/FLOAT(MIFULL))
      IF(PAGE.LT.1.0)PAGE=1.0
      IF((PAGE-AINT(PAGE)).GT.0.0)PAGE=AINT(PAGE)+1.0
      MPM=INT(PAGE)

      RETURN
      END

C ******************* KEYIND
C KEYIND decodes the index INO and returns the array index IA of the item
C displayed and whether INO is within the list.
      SUBROUTINE KEYIND(MENUL,INO,IA,INOUT)
#include "epara.h"

C Assume blank returned key, 0 array index, outwith list.
      IA=0
      INOUT=0

      IF(INO.GT.MHEAD.AND.INO.LT.(MENUL-MCTL+1))THEN
        INOUT=1
        IA=INO-MHEAD+IST-1
      ENDIF
      RETURN
      END

C ********************** EPMENSV
C EPMENSV pushes the current state of common block PMENU into common block
C PMENUSV.  Up to 5 levels are maintained.
C This is useful if a second menu is about to be called and
C knowledge of the first is required.  See also EPMENRC which pops the
C information back into common PMENU.
      SUBROUTINE EPMENSV
#include "epara.h"
      COMMON/PMENUSV/MHEAD1(5),MCTL1(5),MIFUL1(5),MFUL1(5),IST1(5),
     &               ILEN1(5),IPM1(5),MPM1(5),IPFLG1(5),MILEV
      MILEV=MILEV+1
      if(MILEV.gt.5)then
C        write(6,*) 'Number of nested levels is',MILEV
      endif
      do 42 j=5,2,-1
        i=j-1
        MHEAD1(j)=MHEAD1(i)
        MCTL1(j)=MCTL1(i)
        MIFUL1(j)=MIFUL1(i)
        MFUL1(j)=MFUL1(i)
        IST1(j)=IST1(i)
        ILEN1(j)=ILEN1(i)
        IPM1(j)=IPM1(i)
        MPM1(j)=MPM1(i)
        IPFLG1(j)=IPFLG1(i)
  42  continue
      MHEAD1(1)=MHEAD
      MCTL1(1)=MCTL
      MIFUL1(1)=MIFULL
      MFUL1(1)=MFULL
      IST1(1)=IST
      ILEN1(1)=ILEN
      IPM1(1)=IPM
      MPM1(1)=MPM
      IPFLG1(1)=IPFLG

      RETURN

      END

C *********************** EPMENRC
C EPMENRC recovers menu definitions (common block PMENU) from common
C block PMENUSV.
C This is useful if returning from a second menu.  See also EPMENSV.
      SUBROUTINE EPMENRC
#include "epara.h"
      COMMON/PMENUSV/MHEAD1(5),MCTL1(5),MIFUL1(5),MFUL1(5),IST1(5),
     &               ILEN1(5),IPM1(5),MPM1(5),IPFLG1(5),MILEV
      MILEV=MILEV-1
      if(MILEV.le.0)then
C        write(6,*) 'Number of nested levels is',MILEV
        MILEV=0
      endif
      MHEAD=MHEAD1(1)
      MCTL=MCTL1(1)
      MIFULL=MIFUL1(1)
      MFULL=MFUL1(1)
      IST=IST1(1)
      ILEN=ILEN1(1)
      IPM=IPM1(3)
      MPM=MPM1(1)
      IPFLG=IPFLG1(1)
      do 42 i=2,5
        j=i-1
        MHEAD1(j)=MHEAD1(i)
        MCTL1(j)=MCTL1(i)
        MIFUL1(j)=MIFUL1(i)
        MFUL1(j)=MFUL1(i)
        IST1(j)=IST1(i)
        ILEN1(j)=ILEN1(i)
        IPM1(j)=IPM1(i)
        MPM1(j)=MPM1(i)
        IPFLG1(j)=IPFLG1(i)
  42  continue
      RETURN

      END

C+++++++++++++++++++++++++++++++++++++++++++++++++++++++
      SUBROUTINE EPAGE
C EPAGE: Screen control: page without waiting.
      IPAGE=1
      IWAIT=0
      IEND=0
      CALL EMPAGE(IPAGE,IWAIT,IEND)
      RETURN
      END

C+++++++++++++++++++++++++++++++++++++++++++++++++++++++
      SUBROUTINE EPAGEW
C EPAGEW: Screen control: Wait before paging.
      IPAGE=1
      IWAIT=1
      IEND=0
      CALL EMPAGE(IPAGE,IWAIT,IEND)
      RETURN
      END

C+++++++++++++++++++++++++++++++++++++++++++++++++++++++
      SUBROUTINE EPWAIT
C EPWAIT: Screen control: Wait without paging.
      IPAGE=0
      IWAIT=1
      IEND=0
      CALL EMPAGE(IPAGE,IWAIT,IEND)
      RETURN
      END

C+++++++++++++++++++++++++++++++++++++++++++++++++++++++
      SUBROUTINE EPAGEND
C EPAGEND: Screen control: Page then close window if open.
      IPAGE=1
      IWAIT=0
      IEND=1
      CALL EMPAGE(IPAGE,IWAIT,IEND)
      RETURN
      END
      
