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

C Components for float:

      integer iFloatBasis
      integer iFloatPrecision
      save iFloatBasis
      save iFloatPrecision
      
!       print*, "bSPEqual: comparing ", fFloat1, " and ", fFloat2

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
        fFloat1 = fFloat1 * -1.0
      else
        iSign1 =  1
      endif

      if ( fFloat2 .LT. 0.0 ) then
        iSign2 = -1
        fFloat2 = fFloat2 * -1.0
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

      

      
