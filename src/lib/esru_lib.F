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

C  ang3vtx: Get angle between three vertex.
C  CLOSE3D: Calc min dist between two lines in 3D return dist and closest points.
C  CROW:    Function returns shortest dist between two points P(3) and Q(3).
C  CROWXYZ: Function returns shortest dist between two points Px,Py,Pz and Qx,Qy,Qz.
C  UVXYZ:   Returns Unit vector Ux,Uy,Uz from two points Px,Py,Pz and Qx,Qy,Qz.
C  UVAB:    Returns Unit vector U of vector A.
C  PLNDIS:  Finds distance DIST from a point (x,y,z) to a plane (eq EQN).
C  PLNDANG: Finds dihedral angel between two planes given their equations.
C  AVER:    Returns the centre of gravity of an polygon array.
C  CROSS:   Performs a cross-product on vectors A() & B() returning in C().
C  CROSS2:  Performs a cross-product on vectors passing parameters as ax,ay,az etc.
C  DOT3(a,b,product) Return dot product of two vectors a & b.
C  ZEROS:   Clear a 4x4 array prior to doing vieweing transforms.
C  ECLOSE:  Checks tolerance between two real numbers.
C  ESIND:   Function returning SIN of angle where angle is given in degrees.
C  ECOSD:   Function returning COS of angle where angle is given in degrees.
C  ETAND:   Function returning TAN of angle where angle is given in degrees.
C  IFAX :   Integer function returning the integer part of it's argument.
C  EAZALT:  Computes the solar azimuth & altitude angles at current time.
C  AGNXYZ:  Given solar azi & elev return viewing coords @ 1000m.
C  ORTTRN:  Multiplies a point (XM,YM,ZM) by the transform matrix
C           TMAT to return the point XO,YO,Z0.
C  VECTRN:  Transforms a vector VECIN by the 4x4 (homogeneious) matrix
C           TMAT and returns the vector VECOUT.
C  VECPLN:  Returns the point of intersection X,Y,Z between a line
C           defined by X1,Y1,Z1 & X2,Y2,Z2 and a plane defined in PEQN.
C  HMATMUL: Multiplies the homogenous (4x4) matrices A by B returning C.
C  HREVMAT: Takes the homogenous perspective transformation PER and
C           returns it's inverse REP making use of CROUT.
C  CROUT:   Inverts a nonsymetric square matrix A (order N), returning
C           the matrix B and IERR =-1 if matrix is singular.
C  DPACC:   Provides double precision accumulation of inner products for
C           CROUT in the form SUM(+,-)SUM(+,-)AB.
C  EYEMAT:  Provides transform eyepoint - viewpoint....

C  INTSTR: Converts integer into string (10 char) w/o leading blanks.
C  RELSTR: Converts a real into a string (12 char) w/o leading blanks.
C  REL16STR: Converts a real into a string (16 char) w/o leading blanks.
C  EXPSTR: Converts a exponential into a string (10 char) w/o leading blanks.
C  ARLIST: takes a real array (rlist) and builds a packed string.
C  AILIST: takes an int array (ilist) and builds a packed string.
C  ASLIST: takes an string array (list*24) and builds a packed string.
C  ASLIST2 takes the range (inst to inil) items of an string array (list)
C  ASFLIST: takes an string array (list*48) and builds a packed string.
C  STRIPC: Strips comments from a ASCII file str (124 char) & returns data.
C  LSTRIPC: Strips comments from a ASCII file str (248 char) & returns data.
C  STRIPC400 strips comments from a ASCII file (400 char long) string and returns the data.
C  CHITMS: Checks a character string & returns the number of data items.
C  LCHITMS Checks a (248) character string (A), returning the number of data
C  CHITMS400: Checks a (400) character string & returns the number of data items.
C  CHARCH: Routine to check a string for a specific number of data items.
C  NOYES:  INTEGER FUNCTION to read the answer Y,y,1,N,n,0 to a question.
C  IFIRST: Function returning ASCII value for 1st char in a string ISTR.

C  EDAY:   Returns the year day number when passed the day of month & month.
C  EDAYR: returns the day and month numbers from the day-of-year.
C  EWEEKD: returns the day of the week given the day of month, month
C          and year as integers.
C  EDAYCH: Checks for errors in the users specification of the day & month.
C  DATTIM: returns UNIX time via a string in the form : 16 Sep 73 14:23.
C  STDATE: Takes the day of year and returns two descriptive strings:
C          DESCR takes the form '12 Jan' & DESCR1 takes the form 'Fri 12 Jan'.
C  ESTIME: Takes an integer timestep and returns two string descriptions:
C          DESCRH in the form '12h28' & DESCRD which takes the form of 12.46.
C  EDTIME: takes an real time and returns two string descriptions:
C          DESCRH in the form '12h28' and DESCRD which takes the form of 12.46,
C  EPERSTR: creates three strings representing the start and
C           stop time of a diary period.


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
      

C ******* ang3vtx
C Recover angle between three vertex (i.e. between two lines).
C Angel between lines: finds angle A between two lines'
C given 3 vertex as in:    * 1
C                           a \
C                     3 *------* 2
      SUBROUTINE ang3vtx(x1,y1,z1,x2,y2,z2,x3,y3,z3,ang)
      dimension vd21(3),vd32(3)
      PI = 4.0 * ATAN(1.0)
      RAD = PI/180.
      vdx21=x1-x2
      vdy21=y1-y2
      vdz21=z1-z2
      vdx32=x3-x2
      vdy32=y3-y2
      vdz32=z3-z2
      CROW21 = vdx21**2 + vdy21**2 + vdz21**2
      CROW21=SQRT(CROW21)
      if (abs(crow21).lt.0.001) then
        return
      endif
      CROW32 = vdx32**2 + vdy32**2 + vdz32**2
      CROW32=SQRT(CROW32)
      if (abs(crow32).lt.0.001) then
        return
      endif

C Make vectors into unit vectors.
      vd21(1)=vdx21/crow21
      vd21(2)=vdy21/crow21
      vd21(3)=vdz21/crow21
      vd32(1)=vdx32/crow32
      vd32(2)=vdy32/crow32
      vd32(3)=vdz32/crow32

C Get dot product.
      call dot3(vd21,vd32,prod)
      ang=ACOS(prod)/RAD
      return
      end

C ********************** CLOSE3D **********************
C CLOSE3D - calc min dist between two lines in 3D return dist and
C closest points.
C PA,VA define line A (point and vector), SA is parametric variable at closest point
C PB,VB define line B (point and vector), SB is parametric variable at closest point
C DIST minimum distance between lines - if negative then error
C CA,CB are closest points on two lines (if intesect DIST=0 and CA=CB)
C Theory:
C  At intersection the values of the parametric variables will
C  produce the same x,y,z values.  Two equations are formed and
C  solved (these are easily derived from the parametric form of
C  the line equations but the variable names are used below).
C    SA.VA.VB - SB.VBsq = RHSa
C    SA.VAsq - SB.VA.VB = RHSb

      SUBROUTINE CLOSE3D(PA,VA,SA,PB,VB,SB,DIST,CA,CB)

      DIMENSION PA(3),VA(3),PB(3),VB(3),CA(3),CB(3)
      DIMENSION PD(3)

C Check for parallel lines.
C Calculate some data here for use later.
      VAsq=0.
      VBsq=0.
      VAVB=0.0
      do 5 I=1,3
        VAVB=VAVB+VA(I)*VB(I)
        VAsq=VAsq+VA(I)*VA(I)
        VBsq=VBsq+VB(I)*VB(I)
 5    continue
      ALEN=sqrt(VAsq)
      BLEN=sqrt(VBsq)
      DOT=VAVB/(ALEN*BLEN)
      DOT=abs(DOT)-1.
      if (abs(DOT).lt.0.0001) then

C Check if colinear.
        DOT=0.
        ALEN=0.
        do 7 I=1,3
          DOT=DOT+(PA(I)-PB(I))*VB(I)
          ALEN=ALEN+(PA(I)-PB(I))*(PA(I)-PB(I))
 7      continue
        ALEN=sqrt(ALEN)
        DOT=DOT/(ALEN*BLEN)
        DOT=abs(DOT)-1.
        if (abs(DOT).gt.0.0001) then
          DIST=-99.
          return
        endif
      endif

C Calculate closest distance between test line and current edge.
C PD is the distance from the test point to the initial point on
C the edge.
      do 10 I=1,3
        PD(I)=PB(I)-PA(I)
 10   continue
      RHSa=0.0
      RHSb=0.0
      do 20 I=1,3
        RHSa=RHSa+PD(I)*VB(I)
        RHSb=RHSb+PD(I)*VA(I)
 20   continue

C VAVB will equal 0.0 if lines are at right angles this simplifies
C solution of simultaneous equations.
      if (abs(VAVB).lt.0.0001) then
        SB=(RHSa/VBsq)*(-1.)
        SA=RHSb/VAsq
      else

C Solve simultaneous equations.
        SB=(((VAVB*RHSb)/VAsq)-RHSa) / (VBsq-((VAVB*VAVB)/VAsq))
        SA=(RHSa+VBsq*SB)/VAVB
      endif

C Calc closest points.
      do 30 I=1,3
        CA(I)=SA*VA(I)+PA(I)
        CB(I)=SB*VB(I)+PB(I)
 30   continue

C Calc distance between them.
      DIST=CROW(CA,CB)

      return
      end

C ******************** CROW
C CROW: Function returning the distance 'as the crow flies'
C between the two points P and Q in X Y Z space.
      FUNCTION CROW(P,Q)
      DIMENSION  P(3), Q(3)
      CROW = ((P(1)-Q(1))*(P(1)-Q(1))) + ((P(2)-Q(2))*(P(2)-Q(2))) +
     &       ((P(3)-Q(3))*(P(3)-Q(3)))
      CROW = SQRT(CROW)

      RETURN
      END

C ******************** CROWXYZ
C CROWXYZ: Function returning the distance 'as the crow flies'
C between the two X Y Z points.
      FUNCTION CROWXYZ(PX,PY,PZ,QX,QY,QZ)
      CROWXYZ = ((PX-QX)*(PX-QX))+((PY-QY)*(PY-QY))+((PZ-QZ)*(PZ-QZ))
      CROWXYZ = SQRT(CROWXYZ)

      RETURN
      END

C ******************** UVXYZ
C UVXYZ: Subroutine returning Unit vector from two points along a
C line expressed as X Y Z points.
      subroutine UVXYZ(PX,PY,PZ,QX,QY,QZ,UX,UY,UZ)
      CROWXYZ = ((PX-QX)*(PX-QX))+((PY-QY)*(PY-QY))+((PZ-QZ)*(PZ-QZ))
      CROWXYZ = SQRT(CROWXYZ)
      if (abs(crowxyz).lt.0.001) then
        return
      endif
      UX=(PX-QX)/crowxyz
      UY=(PY-QY)/crowxyz
      UZ=(PZ-QZ)/crowxyz

      RETURN
      END

C ******************** UVAB
C UVAB: Subroutine returning Unit normal vector U of vector A.
      subroutine UVAB(A,U,ierr)
      dimension A(3),U(3)
      ierr=0
      S = A(1)*A(1) + A(2)*A(2) + A(3)*A(3)
      S = SQRT(S)
      if (abs(S).lt.0.001) then
        ierr=-1
        return
      endif
      U(1)=A(1)/S
      U(2)=A(2)/S
      U(3)=A(3)/S

      RETURN
      END

C ******************** PLNDANG(AEQN,BEQN,DANG)
C Finds dihedral angel between two planes given their equations.
      subroutine plndang(aeqn,beqn,dang)
      dimension aeqn(4),beqn(4)
      PI = 4.0 * ATAN(1.0)
      RAD = PI/180.0
      COSDAN = AEQN(1)*BEQN(1)+AEQN(2)*BEQN(2)+AEQN(3)*BEQN(3)
      DANG = ACOS(COSDAN) * RAD
      return
      end

C ******************** PLNDIS(EQN,X,Y,Z,DIST)
C PLNDIS finds distance DIST from a point (x,y,z) to a plane (eq EQN).
C If DIST=0 then one the plane, if DIST > 0 then point on the side
C of the normal vector, if DIST < 0 then on the opposite side.
      subroutine PLNDIS(EQN,X,Y,Z,DIST)
      DIMENSION EQN(4)
      DIST =   EQN(1)*X + EQN(2)*Y + EQN(3)*Z - EQN(4)
      RETURN
      END

C ******************** AVER
C AVER returns the centre of gravity CG for an array
C of vertex points PNT with NP points in it.
      SUBROUTINE AVER(MNV,NP,PNT,CG)
      DIMENSION PNT(MNV,3),CG(3)

      DO 20 J=1,3
        CG(J)=0.
        DO 10 K=1,NP
          CG(J)=CG(J)+PNT(K,J)
   10   CONTINUE
        CG(J)=CG(J)/NP
   20 CONTINUE

      RETURN
      END

C ******************** CROSS
C CROSS performs a cross-product of two vectors A(X,Y,Z) and B(X,Y,Z),
C and returns the result in C(X,Y,Z).
      SUBROUTINE CROSS(A,B,C)
      DIMENSION A(3),B(3),C(3)
      C(1)=0.0
      C(2)=0.0
      C(3)=0.0
      C(1)=A(2)*B(3)-A(3)*B(2)
      C(2)=A(3)*B(1)-A(1)*B(3)
      C(3)=A(1)*B(2)-A(2)*B(1)
      RETURN
      END

C ******************** CROSS2
C CROSS2 performs a cross-product of two vectors AX,AY,AZ and BX,BY,BZ,
C and returns the result in CX,CY,CZ.
      subroutine cross2(ax,ay,az, bx,by,bz, cx,cy,cz)
      cx = ay*bz - az*by
      cy = az*bx - ax*bz
      cz = ax*by - ay*bx
      return
      end

C ******************** DOT3(a,b,product)
C Return dot product of two vectors a & b.
      subroutine dot3(a,b,product)
      dimension a(3),b(3)
      product=a(1)*b(1)+a(2)*b(2)+a(3)*b(3)
      return
      end

C ******************** ZEROS
C ZEROS Clear a 4x4 array prior to doing viewering transforms.
      SUBROUTINE ZEROS(A)
      DIMENSION A(4,4)
      A(1,1)=0.0
      A(1,2)=0.0
      A(1,3)=0.0
      A(1,4)=0.0

      A(2,1)=0.0
      A(2,2)=0.0
      A(2,3)=0.0
      A(2,4)=0.0

      A(3,1)=0.0
      A(3,2)=0.0
      A(3,3)=0.0
      A(3,4)=0.0

      A(4,1)=0.0
      A(4,2)=0.0
      A(4,3)=0.0
      A(4,4)=0.0

      RETURN
      END

C ******************** ECLOSE
C ECLOSE allows two real numbers R1 & R2 to be checked for closeness
C to a given tolerance TOL and returns CLOSE = .TRUE. or .FALSE.
      SUBROUTINE ECLOSE(R1,R2,TOL,CLOSE)
      LOGICAL CLOSE

      IF(ABS(R1-R2).LT.TOL)THEN
        CLOSE = .TRUE.
      ELSE
        CLOSE = .FALSE.
      ENDIF

      RETURN
      END

C ******************** ESIND
      FUNCTION ESIND (DEG)
C Returns SIN of angle where angle is given in degrees.
      PI = 4.0 * ATAN(1.0)
      RAD = PI/180.
      ESIND = SIN (RAD*DEG)
      RETURN
      END

C ******************** ECOSD
      FUNCTION ECOSD (DEG)
C Returns COS of angel where angle is given in degrees.
      PI = 4.0 * ATAN(1.0)
      RAD = PI/180.
      ECOSD = COS (RAD*DEG)
      RETURN
      END

C ******************** ETAND
      FUNCTION ETAND (DEG)
C Returns TAN of angel where angle is given in degrees.
      PI = 4.0 * ATAN(1.0)
      RAD = PI/180.
      ETAND = TAN (RAD*DEG)
      RETURN
      END

C ******************** IFAX
C IFAX is an integer function returning the integer part of it's
C argument truncated towards negative infinity as follows:
C     V        IFAX(V)
C    1.0        1
C     .9        0
C     .1        0
C      0        0
C    -.1       -1
C    -.9       -1
C   -1         -1
C   -1.1       -2
      FUNCTION IFAX(V)
        IFAX = INT(V)
        IF (V.LT.0) IFAX=IFAX-1
      RETURN
      END

C ******************** EAZALT
C EAZALT computes the solar azimuth and altitude angles at the current
C time (REAL). The 'ISUNUP' variable determines whether the sun
C is up (=1) or down (=0). The solar angles are computed relative
C to local mean time (e.g. Greenwich is the reference time zone for
C the UK).
      SUBROUTINE EAZALTS(TIMEH,ID,SLAT,SLON,ISUNUP,SAZI1,SALT1)
      logical CLOSE
      SAZI1=0.
      SALT1=0.
      ISUNUP=1
      R=3.14159/180.
      DAY=FLOAT(ID)
      HOUR=TIMEH

c Calculate equation of time.
      A=1.978*DAY-160.22
      B=0.989*DAY-80.11
      EQT=0.1645*ESIND(A)-0.1255*ECOSD(B)-0.025*ESIND(B)

c Compute declination.
      A=280.1+0.9863*DAY
      DEC=23.45*ESIND(A)
      SDEC=SIN(DEC*R)
      CDEC=COS(DEC*R)

c Compute solar altitude.
      TIME=HOUR+(EQT+SLON/15.)
      TIMCOE=15.0*(12.0-TIME)
      CDTIME=COS(TIMCOE*R)
      ABST=ABS(TIMCOE)
      SABST=SIN(ABST*R)
      SSLAT=SIN(SLAT*R)
      CSLAT=COS(SLAT*R)
      SALT=ASIN(SSLAT*SDEC+CSLAT*CDEC*CDTIME)/R
      IF(SALT.LT.0.)goto 1

C Solar aziumth.
      AZMUTH=(CDEC*SABST)/ECOSD(SALT)
      IF(AZMUTH.LT.-1.0)AZMUTH=-1.0
      IF(AZMUTH.GT.1.0)AZMUTH=1.0
      SAZI=ASIN(AZMUTH)/R

C Correct the azimuthal angle for time of day and whether in north or
C south hemispheres.
      XX=CDTIME
      CALL ECLOSE(SLAT,0.0,0.01,CLOSE)
      IF(CLOSE)goto 13
      CALL ECLOSE(SLAT,90.0,0.01,CLOSE)
      IF(CLOSE)goto 8
      YY=(CSLAT/SSLAT)*(SDEC/CDEC)
      goto 9
    8 YY=0.0
      goto 9
   13 YY=10.0*(SDEC/CDEC)

C This is one place where gfortran 4.3 complains.
    9 IF(YY-XX)3,4,5
    3 IF(SLAT.GE.0.0)goto 6
      goto 7
    5 IF(SLAT.LT.0.0)goto 6
      goto 7
    4 IF(TIME.LE.12.0)SAZI=90.0
      IF(TIME.GT.12.0)SAZI=270.0
      goto 2
    6 IF(TIME.LE.12.0)SAZI=180.0-SAZI
      IF(TIME.GT.12.0)SAZI=180.0+SAZI
      goto 2
    7 IF(TIME.GT.12.0)SAZI=360.0-SAZI
    2 SAZI1=SAZI
      SALT1=SALT
      goto 11
    1 ISUNUP=0
   11 CONTINUE
      RETURN
      END

C---- AGNXYZ -----------------------------------------------------
C AGNXYZ: Given the solar azimuth and elevation return viewing coords
C         at a distance of 1000 metres from origin. ???
      SUBROUTINE ANGXYZ(YAZI,SALT,X,Y,Z)

      DATA SUNDIS /1000./

      PI = 4.0 * ATAN(1.0)

      RAD = PI/180.
      RYAZI = YAZI*RAD
      RSALT = SALT*RAD

      Z = SUNDIS*SIN(RSALT)
      XYDIS = SUNDIS*COS(RSALT)

      IF (XYDIS .LT. 1E-6)THEN
        X = 0.
        Y = 0.
        RETURN
      ELSE
        X = XYDIS*SIN(RYAZI)
        Y = XYDIS*COS(RYAZI)
        RETURN
      ENDIF

      END

C ****************** ORTTRN
C ORTTRN multiplies a point (XM,YM,ZM) by the transform matrix
C TMAT to return the point XO,YO,Z0.  If the points are to close
C together then IER=-1.
      SUBROUTINE ORTTRN(XM,YM,ZM,TMAT,XO,YO,ZO,IERR)
      DIMENSION TMAT(4,4)

      IERR = 0

      XO = XM*TMAT(1,1)+YM*TMAT(2,1)+ZM*TMAT(3,1)+TMAT(4,1)
      YO = XM*TMAT(1,2)+YM*TMAT(2,2)+ZM*TMAT(3,2)+TMAT(4,2)
      ZO = XM*TMAT(1,3)+YM*TMAT(2,3)+ZM*TMAT(3,3)+TMAT(4,3)
      T4 = XM*TMAT(1,4)+YM*TMAT(2,4)+ZM*TMAT(3,4)+TMAT(4,4)

      IF (ABS(T4) .LT. 10E-6)THEN
        IERR = -1
        RETURN
      ENDIF

      XO = XO/T4
      YO = YO/T4
      ZO = ZO/T4

      RETURN
      END

C ******************* VECTRN
C VECTRN transforms a vector VECIN by the 4x4 (homogeneious) matrix TMAT
C and returns the vector VECOUT.  If IERR is < 0 then a fatal error.
      SUBROUTINE VECTRN(VECIN,TMAT,VECOUT,IERR)

      DIMENSION VECIN(3),VECOUT(3),TMAT(4,4)

      IERR = 0

      VECOUT(1) = VECIN(1)*TMAT(1,1) + VECIN(2)*TMAT(2,1) +
     &            VECIN(3)*TMAT(3,1) + TMAT(4,1)

      VECOUT(2) = VECIN(1)*TMAT(1,2) + VECIN(2)*TMAT(2,2) +
     &            VECIN(3)*TMAT(3,2) + TMAT(4,2)

      VECOUT(3) = VECIN(1)*TMAT(1,3) + VECIN(2)*TMAT(2,3) +
     &            VECIN(3)*TMAT(3,3) + TMAT(4,3)

      T4 = VECIN(1)*TMAT(1,4) + VECIN(2)*TMAT(2,4) +
     &            VECIN(3)*TMAT(3,4) + TMAT(4,4)

      IF (ABS(T4) .LT. 10E-6)THEN
        IERR = -1
        RETURN
      ELSE
        VECOUT(1) = VECOUT(1)/T4
        VECOUT(2) = VECOUT(2)/T4
        VECOUT(3) = VECOUT(3)/T4
        RETURN
      ENDIF

      END

C ********************* VECPLN
C VECPLN returns the point of intersection X,Y,Z between a line defined
C by X1,Y1,Z1 & X2,Y2,Z2 and a plane defined in PEQN.
C IERR is -1 if denominator is close to zero or negative.
C IERR is 0 if no problem found.
      SUBROUTINE  VECPLN(X1,Y1,Z1,X2,Y2,Z2,PEQN,X,Y,Z,IERR)
      DIMENSION  PEQN(4)

      IERR=0

      F = X2 - X1
      G = Y2 - Y1
      H = Z2 - Z1

      DENOM = SQRT(F*F + G*G + H*H)

      F = F/DENOM
      G = G/DENOM
      H = H/DENOM

      DENOM = PEQN(1)*F + PEQN(2)*G + PEQN(3)*H

      IF (ABS(DENOM).LT.0.0001)THEN
        IERR=-1
        RETURN
      ENDIF

      T = -(PEQN(1)*X1+PEQN(2)*Y1+PEQN(3)*Z1-PEQN(4))/DENOM

      X = X1 + F*T
      Y = Y1 + G*T
      Z = Z1 + H*T

      RETURN
      END

C ******************** HMATMUL
C HMATMUL multiplies the homogenous (4x4) matrices A by B returning C.
C Matrix A is premultiplied and matrix B is postmultiplied.
      SUBROUTINE HMATMUL(A,B,C)

      DIMENSION A(4,4),B(4,4),C(4,4)

      DO 20 I=1,4
        C(I,1)=A(I,1)*B(1,1)+A(I,2)*B(2,1)+A(I,3)*B(3,1)+A(I,4)*B(4,1)
        C(I,2)=A(I,1)*B(1,2)+A(I,2)*B(2,2)+A(I,3)*B(3,2)+A(I,4)*B(4,2)
        C(I,3)=A(I,1)*B(1,3)+A(I,2)*B(2,3)+A(I,3)*B(3,3)+A(I,4)*B(4,3)
        C(I,4)=A(I,1)*B(1,4)+A(I,2)*B(2,4)+A(I,3)*B(3,4)+A(I,4)*B(4,4)
   20 CONTINUE

      RETURN
      END

C ******************** HREVMAT
C HREVMAT takes the homogenous perspective transformation PER and
C returns it's inverse REP making use of CROUT.
      SUBROUTINE HREVMAT(PER,REP,IERR)

      DIMENSION PER(4,4),REP(4,4),A(4,4),WS(4)

      IA=4
      IB=4
      DO 20 J=1,4
        A(J,1)=PER(J,1)
        A(J,2)=PER(J,2)
        A(J,3)=PER(J,3)
        A(J,4)=PER(J,4)
   20 CONTINUE

      CALL CROUT(A,4,WS,REP,IA,IB,IFL)

C IERR > 0, if ok, IERR=-1 if matrix singular.
      IERR=0
      IF(IFL.LT.0)IERR = -1
      RETURN

      END

C ******************** CROUT
C CROUT inverts a nonsymetric square matrix A (order N), returning
C the matrix B and IERR =-1 if matrix is singular.
C WS is a workspace vector of dimension N, IA is the
C first dimension of A and IB is the second dimension.
C based on crout factorization based on code from
C Alan Bridges and Wilkinson and Reinsch 'Linear Algebra'
C Springer Verlag, 1971, PP 93-110

C The decomposition of A=LU, where L
C is a lower triangular matrix, and U is a unit
C upper triangular matrix, is performed and
C overwritten on A, omitting the unit diagonal of
C U. A record of any interchanges made to the rows
C of A is kept in WS(I), such that the I-th row and
C the WS(I)-th row were interchanged at the I-th step.
C The factorisation will fail if A, modified by the
C the rounding errors, is singular (or almost singular).
C Makes use of subroutine DPACC to accumulate
C sums of inner products.
      SUBROUTINE CROUT(A,N,WS,B,IA,IB,IER)
      common/OUTIN/IUOUT,IUIN

      DOUBLE PRECISION D1
      DIMENSION A(IA,N),B(IB,N),WS(N)

C Machine dependent rounding error, such that 1.0+EPS > 1.0
      EPS=2.0E-10
      IER=0
      N1=N
      DO 10 I = 1,N
        DO 20 J = 1,N
          B(I,J) = 0.0
   20   CONTINUE
        B(I,I) = 1.0
   10 CONTINUE

      KK=IA*N
      D1=1.D0
      ID=0
      DO 30 I=1,N
        I1=I
        I2=I1+IA
        CALL DPACC(A,I1,I2,KK,A,I1,I2,KK,0.0,Y,N,1)
        WS(I)=1.0/SQRT(Y)
  30  CONTINUE

      DO 40 K=1,N
        LL=K-1
        L=K
        K1=K+1
        X=0.0
        DO 50 I=K,N
          I1=I
          I2=I1+IA
          I3=LL*IA+1
          I4=I3+1
          CALL DPACC(A,I1,I2,KK,A,I3,I4,KK,A(I,K),Y,LL,2)
          A(I,K)=Y
          Y=ABS(Y*WS(I))
          IF(Y.LE.X)GO TO 50
          X=Y
          L=I
50      CONTINUE

        IF(L.EQ.K)GO TO 60
        D1=-D1
        DO 70 J=1,N
          Y=A(K,J)
          A(K,J)=A(L,J)
          A(L,J)=Y
  70    CONTINUE

        WS(L)=WS(K)
60      WS(K)=L
        D1=D1*DBLE(A(K,K))
        IF(X.LT.(8.0*EPS))THEN
          IER=-1
          call edisp(iuout,' CROUT: matrix is singular ')
          call edisp(iuout,'        and cannot be inverted.')
          RETURN
        ENDIF

80      IF(DABS(D1).LT.1.D0)GO TO 90
        D1=D1*0.625D-1
        ID=ID+4
        GO TO 80

90      IF(DABS(D1).GE.0.625D-4)GO TO 85
        D1=D1*0.16D2
        ID=ID-4
        GO TO 90

85      X=-1.0/A(K,K)
        IF(K.EQ.N)GO TO 40
        DO 100 J=K1,N
          I1=K
          I2=I1+IA
          I3=(J-1)*IA+1
          I4=I3+1
          CALL DPACC(A,I1,I2,KK,A,I3,I4,KK,A(K,J),Y,LL,3)
          A(K,J)=X*Y
  100   CONTINUE

40    CONTINUE

      L=IB*N
      DO 110 I=1,N
        J=INT(WS(I)+0.5)
        IF(J.EQ.I)GO TO 110
        DO 120 K=1,N1
          X=B(I,K)
          B(I,K)=B(J,K)
          B(J,K)=X
  120   CONTINUE
110   CONTINUE

      DO 130 K=1,N1
        K1=K-1
        DO 140 I=1,N
          I0=I-1
          I1=I
          I2=I1+IA
          I3=K1*IB+1
          I4=I3+1
          CALL DPACC(A,I1,I2,KK,B,I3,I4,L,B(I,K),X,I0,4)
          B(I,K)=X/A(I,I)
  140   CONTINUE

        I=N+1
        DO 150 I5=1,N
          I=I-1
          I0=N-I
          I1=I*IA+I
          I2=I1+N
          I3=K1*IB+I+1
          I4=I3+1
          CALL DPACC(A,I1,I2,KK,B,I3,I4,L,B(I,K),X,I0,4)
          B(I,K)=X
  150   CONTINUE
130   CONTINUE

      RETURN
      END

C ******************** DPACC
C DPACC provides double precision accumulation of inner products for
C CROUT in the form SUM(+,-)SUM(+,-)AB.
C A is the vector on left, I & J numerical identifiers of first two
C elements of A in the multiplaction and IJ is the dimension of A.
C For vector B the parameters K,L,KL are analogous to I,J,IJ.
C X is the quantity to be added to the product of the specified
C elements of vectors A & B.
C SUM is the result, N is a counter, IND is an indicator as follows:
C   IF IND=1 SUM=AB+X
C   IF IND=2 SUM=X-AB
C   IF IND=3 SUM=AB-X
C   IF IND=4 SUM=-AB-X = -(AB+X)

      SUBROUTINE DPACC(A,I,J,IJ,B,K,L,KL,X,SUM,N,IND)

      DOUBLE PRECISION P,Q,R
      DIMENSION A(IJ),B(KL)

      R=0.D0
      IF(I.GT.IJ.OR.K.GT.KL)GO TO 10
      IF(N.GT.0)THEN
        M=J-I
        M1=L-K
        DO 20 IK=1,N
          I1=I+(IK-1)*M
          P=DBLE(A(I1))
          I1=K+(IK-1)*M1
          Q=DBLE(B(I1))
          R=R+P*Q
   20   CONTINUE
      ENDIF

10    P=DBLE(X)
      IF(IND.EQ.1)THEN
        SUM=SNGL(P+R)
      ELSEIF(IND.EQ.2)THEN
        SUM=SNGL(P-R)
      ELSEIF(IND.EQ.3)THEN
        SUM=SNGL(R-P)
      ELSEIF(IND.EQ.4)THEN
        SUM=SNGL(-P-R)
      ENDIF

      RETURN
      END

C ****************** EYEMAT
C EYEMAT provides transform eyepoint - viewpoint....
      SUBROUTINE EYEMAT(EP,VP,SCALE,TMAT,RMAT)
      common/OUTIN/IUOUT,IUIN

      DIMENSION  EP(3),VP(3),TMAT(4,4),RMAT(4,4)
      character outs*124

      DELTA = 10E-6

      VPRIME = SQRT((EP(1)-VP(1))**2+(EP(2)-VP(2))**2)
      IF (ABS(VPRIME).GT.DELTA) GOTO 90
      VPRIME = 0.
      SINTET = 0.
      COSTET = -1.
      GOTO 100

   90 SINTET = (EP(1)-VP(1))/VPRIME
      COSTET = (EP(2)-VP(2))/VPRIME

  100 CONTINUE

      EPRIME = EP(3)-VP(3)
      IF (ABS(VPRIME).GT.DELTA) GOTO 110
      IF (ABS(EPRIME).GT.DELTA) GOTO 110
      SINPHI = 1.0
      COSPHI = 0.0
      write(outs,*) ' EP is ',EP,' VP is ',VP
      call edisp(iuout,outs)
      call edisp(iuout,'Eye point & viewed point too close together')
      call edisp(iuout,'so a view || to the Z axis is assumed.')
      GOTO 130

  110 DENOM = SQRT(EPRIME**2+VPRIME**2)
      COSPHI = VPRIME/DENOM
      SINPHI = EPRIME/DENOM

  130 CONTINUE

      TMAT(1,1) = -COSTET
      TMAT(1,2) = -SINTET*SINPHI
      TMAT(1,3) = -SINTET*COSPHI
      TMAT(1,4) = 0.0

      TMAT(2,1) = SINTET
      TMAT(2,2) = -COSTET*SINPHI
      TMAT(2,3) = -COSTET*COSPHI
      TMAT(2,4) = 0.0

      TMAT(3,1) = 0.0
      TMAT(3,2) = COSPHI
      TMAT(3,3) = -SINPHI
      TMAT(3,4) = 0.0

      TMAT(4,1) = EP(1)*COSTET - EP(2)*SINTET
      TMAT(4,2) = (EP(1)*SINTET + EP(2)*COSTET)*SINPHI - EP(3)*COSPHI
      TMAT(4,3) = (EP(1)*SINTET + EP(2)*COSTET)*COSPHI + EP(3)*SINPHI
      TMAT(4,4) = 1.0/SCALE

      RMAT(1,1) = -COSTET
      RMAT(1,2) = SINTET
      RMAT(1,3) = 0.0
      RMAT(1,4) = 0.0

      RMAT(2,1) = -SINPHI*SINTET
      RMAT(2,2) = -COSTET*SINPHI
      RMAT(2,3) = COSPHI
      RMAT(2,4) = 0.0

      RMAT(3,1) = -COSPHI*SINTET
      RMAT(3,2) = -COSTET*COSPHI
      RMAT(3,3) = -SINPHI
      RMAT(3,4) = 0.0

      RMAT(4,1) = EP(1)*SCALE
      RMAT(4,2) = EP(2)*SCALE
      RMAT(4,3) = EP(3)*SCALE
      RMAT(4,4) = SCALE

      RETURN
      END

C ***************** INTSTR
C INTSTR converts an integer into a string (10 char long) with no
C leading blanks. ISWD is the length of the resulting string.
      SUBROUTINE INTSTR(INTIN,FSTR,ISWD,IFLAG)
      common/OUTIN/IUOUT,IUIN

      CHARACTER*10 CSTR, FSTR
      character outs*124
      IFLAG=0
      ISWD=0
      fstr = ' '

C Internal write to STR.
      WRITE(CSTR,'(I10)',ERR=999)INTIN
      K=0
      DO 99 I=1,LEN(cstr)
        IF(cstr(I:I).NE.' '.OR.K.GE.1)THEN
          if(ichar(cstr(I:I)).lt.32)goto 100
          K=K+1
          fstr(K:K)=cstr(I:I)
        ENDIF
 99   CONTINUE

  100 ISWD=max(1,LNBLNK(FSTR))
      RETURN

 999  WRITE(outs,*) ' INTSTR: invalid integer: ',INTIN
      call edisp(iuout,outs)
      IFLAG=1
      RETURN

      END

C *********************** RELSTR
C RELSTR converts a real into a string (12 char) with no leading
C blanks. ISWD is the actual length of the resulting string. Takes
C the magnitude of the number into account.
      SUBROUTINE RELSTR(RELIN,FSTR,ISWD,IFLAG)
      common/OUTIN/IUOUT,IUIN

      CHARACTER*12 CSTR, FSTR
      character outs*124
      logical close

      IFLAG=0
      ISWD=0
      fstr = ' '

C Internal write to STR. If trivally close to zero set, otherwise
C write out in an appropriate format given the magnitude of the
C real value.
      rval=relin
      CALL ECLOSE(rval,0.00,0.000001,CLOSE)
      if(close)then
        rval=0.00
        fstr='0.00'
        iswd=4
        return
      endif
      if(abs(rval).gt.1000000.0)then
        WRITE(CSTR,'(1PE12.6)',IOSTAT=ios,ERR=999)RVAL
      elseif(abs(rval).le.1000000.0.and.abs(rval).gt.10000.0)then
        WRITE(CSTR,'(F12.3)',IOSTAT=ios,ERR=999)RVAL
      elseif(abs(rval).le.10000.0.and.abs(rval).gt.0.1)then
        WRITE(CSTR,'(F11.5)',IOSTAT=ios,ERR=999)RVAL
      elseif(abs(rval).le.0.1.and.abs(rval).gt.0.00001)then
        WRITE(CSTR,'(F12.10)',IOSTAT=ios,ERR=999)RVAL
      else
        WRITE(CSTR,'(F12.4)',IOSTAT=ios,ERR=999)RVAL
      endif
      K=0
      DO 99 I=1,LEN(cstr)
        IF(cstr(I:I).NE.' '.OR.K.GE.1)THEN
          if(ichar(cstr(I:I)).lt.32)goto 100
          K=K+1
          fstr(K:K)=cstr(I:I)
        ENDIF
 99   CONTINUE

  100 ISWD=max(1,LNBLNK(FSTR))
      RETURN

 999  if(IOS.eq.2)then
        WRITE(outs,*) ' RELSTR: permission issue: ',RELIN
      else
        WRITE(outs,*) ' RELSTR: invalid real or > 12 char: ',RELIN
      endif
      call edisp(iuout,outs)
      IFLAG=1
      RETURN

      END


C *********************** REL16STR
C REL16STR converts a real into a string (16 char) with no leading
C blanks. ISWD is the actual length of the resulting string. Takes
C the magnitude of the number into account.
      SUBROUTINE REL16STR(RELIN,FSTR,ISWD,IFLAG)
      common/OUTIN/IUOUT,IUIN

      CHARACTER*16 CSTR, FSTR
      character outs*124
      logical close

      IFLAG=0
      ISWD=0
      fstr = ' '

C Internal write to STR. If trivally close to zero set, otherwise
C write out in an appropriate format given the magnitude of the
C real value.
      rval=relin
      CALL ECLOSE(rval,0.00,0.000001,CLOSE)
      if(close)then
        rval=0.00
        fstr='0.00'
        iswd=4
        return
      endif
      if(abs(rval).gt.1000000.0)then
        WRITE(CSTR,'(1PE14.6)',IOSTAT=ios,ERR=999)RVAL
      elseif(abs(rval).le.1000000.0.and.abs(rval).gt.10000.0)then
        WRITE(CSTR,'(G15.5)',IOSTAT=ios,ERR=999)RVAL
      elseif(abs(rval).le.10000.0.and.abs(rval).gt.0.1)then
        WRITE(CSTR,'(G14.5)',IOSTAT=ios,ERR=999)RVAL
      elseif(abs(rval).le.0.1.and.abs(rval).gt.0.00001)then
        WRITE(CSTR,'(F10.7)',IOSTAT=ios,ERR=999)RVAL
      elseif(rval.le.0.00001)then
        WRITE(CSTR,'(1PE14.6)',IOSTAT=ios,ERR=999)RVAL
      else
        WRITE(CSTR,'(F14.4)',IOSTAT=ios,ERR=999)RVAL
      endif
      K=0
      DO 99 I=1,LEN(cstr)
        IF(cstr(I:I).NE.' '.OR.K.GE.1)THEN
          if(ichar(cstr(I:I)).lt.32)goto 100
          K=K+1
          fstr(K:K)=cstr(I:I)
        ENDIF
 99   CONTINUE

  100 ISWD=max(1,LNBLNK(FSTR))
      RETURN

 999  if(IOS.eq.2)then
        WRITE(outs,*) ' REL16STR: permission issue: ',RELIN
      else
        WRITE(outs,*) ' REL16STR: invalid real or > 12 char: ',RELIN
      endif
      call edisp(iuout,outs)
      IFLAG=1
      RETURN

      END


C *********************** EXPSTR
C EXPSTR converts a exponential into a string (10 char) with no leading
C blanks. ISWD is the actual length of the resulting string.
      SUBROUTINE EXPSTR(RELIN,FSTR,ISWD,IFLAG)
      common/OUTIN/IUOUT,IUIN

      CHARACTER*10 CSTR, FSTR
      character outs*124

      ISWD=0
      fstr = ' '

C Internal write to STR.
      WRITE(CSTR,'(1PE10.3)',IOSTAT=ISTAT,ERR=999)RELIN
      K=0
      DO 99 I=1,LEN(cstr)
        IF(cstr(I:I).NE.' '.OR.K.GE.1)THEN
          if(ichar(cstr(I:I)).lt.32)goto 100
          K=K+1
          fstr(K:K)=cstr(I:I)
        ENDIF
 99   CONTINUE

  100 ISWD=max(1,LNBLNK(FSTR))
      RETURN

 999  WRITE(outs,*) ' EXPSTR: invalid real or > 10 char: ',RELIN
      call edisp(iuout,outs)
      IFLAG=1
      RETURN

      END

C ******************** ARLIST ********************
C ARLIST takes the first (inst to inrl) items of a real array (rlist)
C of array size (inrs) and builds a packed string (pckstr)
C and returns pckstr and the actual character width (length). If itrunc
C is zero then all items written, else the index of the last item
C which was written. Delm is delimeter between items.

      SUBROUTINE ARLIST(inst,inrl,rlist,inrs,delm,pckstr,length,itrunc)
      dimension rlist(inrs)
      character*(*) pckstr
      CHARACTER item*16,delm*1

      itrunc=0
      LS=LEN(pckstr)
      length=0

C Proceed to fill as much of pckstr as possible, set ierr to one
C if the array cannot be written out fully in the space available.
      pckstr=' '
      ix=1
      ixl=0
      do 43 i=inst,inrl
        item=' '
        rval=rlist(i)
        call rel16str(rval,item,lna,ier)

C Write next portion of pckstr and if not at the end write a
C separating comma. If attempting to write past the end of
C pckstr then set itrunc to the last successfully written
C rlist index.
        if(lna.eq.1)then
          ixl=ix
        else
          ixl=ix+(lna-1)
        endif
        if(ixl+1.lt.LS)then
          write(pckstr(ix:ixl),'(a)')item(1:lna)
          if(i.lt.inrl)then
            if(delm.eq.'T')then
              write(pckstr(ixl+1:ixl+1),'(a)') CHAR(9)
            elseif(delm.eq.'S')then
              write(pckstr(ixl+1:ixl+1),'(a)') ' '
            elseif(delm.eq.'C')then
              write(pckstr(ixl+1:ixl+1),'(a)') ','
            endif
            ix=ix+lna+1
          else
            ix=ix+lna+1
          endif
        else
          length=ix
          itrunc = i-1
          goto 1
        endif
  43  continue
      length=ixl
      return

   1  continue
C      write(6,*) 'ARLIST: truncation writing array at item: ',itrunc,
C     &  ' & pos ',length
      return
      END

C ******************** AILIST ********************
C AILIST takes the range (inst to inil) items of an integer array (ilist)
C of array size (inisz) and builds a packed string (pckstr) and
C returns pckstr and written character width (length). If itrunc
C is zero then all items written, else the index of the last item
C which was written. Delm is delimeter between items.

      SUBROUTINE AILIST(inst,inil,ilist,inisz,delm,pckstr,length,itrunc)
      dimension ilist(inisz)
      character*(*) pckstr
      CHARACTER item*10,delm*1

      itrunc=0
      LS=LEN(pckstr)
      length=0

C Proceed to fill as much of pckstr as possible, set itrunc to the
C last sucessful items of the array cannot be written out fully in
C the space available.
      pckstr=' '
      ix=1
      ixl=0
      do 43 i=inst,inil
        item=' '
        ival=ilist(i)
        CALL INTSTR(ival,item,lna,IER)

C Write next portion of pckstr and if not at the end write a
C separating comma before looping back for another item. If a
C single character then ixl is ix.
        if(lna.eq.1)then
          ixl=ix
        else
          ixl=ix+(lna-1)
        endif
        if(ixl+1.lt.LS)then
          write(pckstr(ix:ixl),'(a)')item(1:lna)
          if(i.lt.inil)then
            if(delm.eq.'T')then
              write(pckstr(ixl+1:ixl+1),'(a)') CHAR(9)
            elseif(delm.eq.'S')then
              write(pckstr(ixl+1:ixl+1),'(a)') ' '
            elseif(delm.eq.'C')then
              write(pckstr(ixl+1:ixl+1),'(a)') ','
            endif
            ix=ix+lna+1
          else
            ix=ix+lna
          endif
        else
          length=ix
          itrunc = i-1
          goto 1
        endif
  43  continue
      length=ixl
      return

   1  continue
C      write(6,*) 'AILIST: truncation writing array at item: ',itrunc,
C     &  ' & pos ',length
      return
      END

C ******************** ASLIST ********************
C ASLIST takes the range (inst to inil) items of an string array (list)
C of array size (inisz) and builds a packed string (pckstr) and
C returns pckstr and written character width (length). If itrunc
C is zero then all items written, else the index of the last item
C which was written. Delm is delimeter between items. It is assumed
C that each string array item is less than 32 characters wide.

      SUBROUTINE ASLIST(inst,inil,list,inisz,delm,pckstr,length,itrunc)
      dimension list(inisz)
      character*(*) pckstr,list
      CHARACTER item*32,delm*1

      itrunc=0
      LS=LEN(pckstr)
      length=0

C Proceed to fill as much of pckstr as possible, set itrunc to the
C last sucessful items of the array cannot be written out fully in
C the space available.
      pckstr=' '
      ix=1
      ixl=0
      do 43 i=inst,inil
        lna=max(1,LNBLNK(list(i)))
        if(lna.gt.32) lna=32
        write(item,'(a)') list(i)(1:lna)

C Write next portion of pckstr and if not at the end write a
C separating comma before looping back for another item. If a
C single character then ixl is ix.
        if(lna.eq.1)then
          ixl=ix
        else
          ixl=ix+(lna-1)
        endif
        if(ixl+1.lt.LS)then
          write(pckstr(ix:ixl),'(a)')item(1:lna)
          if(i.lt.inil)then
            if(delm.eq.'T')then
              write(pckstr(ixl+1:ixl+1),'(a)') CHAR(9)
            elseif(delm.eq.'S')then
              write(pckstr(ixl+1:ixl+1),'(a)') ' '
            elseif(delm.eq.'C')then
              write(pckstr(ixl+1:ixl+1),'(a)') ','
            endif
            ix=ix+lna+1
          else
            ix=ix+lna+1
          endif
        else
          length=ix
          itrunc = i-1
          goto 1
        endif
  43  continue
      length=ixl
      return

   1  continue
C      write(6,*) 'ASLIST: truncation writing array at item: ',itrunc,
C     &  ' & pos ',length
      return
      END

C ******************** ASLIST2 ********************
C ASLIST2 takes the range (inst to inil) items of an string array (list)
C of 2_dimensional array size (inisz,inisz2) and builds a packed string
C (pckstr) of the row (inirw) and
C returns pckstr and written character width (length). If itrunc
C is zero then all items written, else the index of the last item
C which was written. Delm is delimeter between items. It is assumed
C that each string array item is less than 24 characters wide.

      SUBROUTINE ASLIST2(inst,inil,list,inisz,inisz2,inirw,
     &delm,pckstr,length,itrunc)
      dimension list(inisz,inisz2)
      character*(*) pckstr,list
      CHARACTER item*24,delm*1

      itrunc=0
      LS=LEN(pckstr)
      length=0

C Proceed to fill as much of pckstr as possible, set itrunc to the
C last sucessful items of the array cannot be written out fully in
C the space available.
      pckstr=' '
      ix=1
      ixl=0
      do 43 i=inst,inil
        lna=max(1,LNBLNK(list(i,inirw)))
        if(lna.gt.24) lna=24
        write(item,'(a)') list(i,inirw)(1:lna)

C Write next portion of pckstr and if not at the end write a
C separating comma before looping back for another item. If a
C single character then ixl is ix.
        if(lna.eq.1)then
          ixl=ix
        else
          ixl=ix+(lna-1)
        endif
        if(ixl+1.lt.LS)then
          write(pckstr(ix:ixl),'(a)')item(1:lna)
          if(i.lt.inil)then
            if(delm.eq.'T')then
              write(pckstr(ixl+1:ixl+1),'(a)') CHAR(9)
            elseif(delm.eq.'S')then
              write(pckstr(ixl+1:ixl+1),'(a)') ' '
            elseif(delm.eq.'C')then
              write(pckstr(ixl+1:ixl+1),'(a)') ','
            endif
            ix=ix+lna+1
          else
            ix=ix+lna+1
          endif
        else
          length=ix
          itrunc = i-1
          goto 1
        endif
  43  continue
      length=ixl
      return

   1  continue
C      write(6,*) 'ASLIST2: truncation writing array at item: ',itrunc,
C     &  ' & pos ',length
      return
      END

C ******************** ASFLIST ********************
C ASFLIST takes the range (inst to inil) items of an string array (list)
C of array size (inisz) and builds a packed string (pckstr) and
C returns pckstr and written character width (length). If itrunc
C is zero then all items written, else the index of the last item
C which was written. Delm is delimeter between items. It is assumed
C that each string array item is less than 48 characters wide.

      SUBROUTINE ASFLIST(inst,inil,list,inisz,delm,pckstr,length,itrunc)
      dimension list(inisz)
      character*(*) pckstr,list
      CHARACTER item*48,delm*1

      itrunc=0
      LS=LEN(pckstr)
      length=0

C Proceed to fill as much of pckstr as possible, set itrunc to the
C last sucessful items of the array cannot be written out fully in
C the space available.
      pckstr=' '
      ix=1
      ixl=0
      do 43 i=inst,inil
        lna=max(1,LNBLNK(list(i)))
        if(lna.gt.48) lna=48
        write(item,'(a)') list(i)(1:lna)

C Write next portion of pckstr and if not at the end write a
C separating comma before looping back for another item. If a
C single character then ixl is ix.
        if(lna.eq.1)then
          ixl=ix
        else
          ixl=ix+(lna-1)
        endif
        if(ixl+1.lt.LS)then
          write(pckstr(ix:ixl),'(a)')item(1:lna)
          if(i.lt.inil)then
            if(delm.eq.'T')then
              write(pckstr(ixl+1:ixl+1),'(a)') CHAR(9)
            elseif(delm.eq.'S')then
              write(pckstr(ixl+1:ixl+1),'(a)') ' '
            elseif(delm.eq.'C')then
              write(pckstr(ixl+1:ixl+1),'(a)') ','
            endif
            ix=ix+lna+1
          else
            ix=ix+lna+1
          endif
        else
          length=ix
          itrunc = i-1
          goto 1
        endif
  43  continue
      length=ixl
      return

   1  continue
C      write(6,*) 'ASFLIST: truncation writing array at item: ',itrunc,
C     &  ' & pos ',length
      return
      END

C ******************** STRIPC
C STRIPC strips comments from a ASCII file string and returns the data.
C It assumes that if a string begins with a '#' then the whole line is
C a comment an the next line is read.  If a ' #' is discovered within
C a line the rest of the line is removed.
C IER=0 if ok. MSG is a text string used in error messages. If
C IR=0 then acts silently, otherwise notes when EOF found.
C IEXP is the number of expected items in the line:
C   IEXP = 0 means don't care or already know no. items - don't check
C   IEXP >0  means a specific number of items expected (error if not)
C   IEXP = 99 check number of items and return in ITEMS

      SUBROUTINE STRIPC(INPCH,OUTSTR,IEXP,ITEMS,IR,MSG,IER)
#include "espriou.h"

      CHARACTER*124 tmp,STRING,OUTSTR
      character loutstr*248
      CHARACTER*(*) MSG

C Read a line of the file, strip off any trailing blanks, if the first
C character is a # then read the next line from the file.
      IER=0
    8 READ(INPCH,10,IOSTAT=ISTAT,ERR=101,END=102)STRING
   10 FORMAT(A124)
      tmp=STRING(1:LNBLNK(STRING))

C Take the string and check for a #, discarding any text which follows.
      iloc = INDEX(tmp,'#')
      if(iloc.eq.1)then
        goto 8
      elseif(iloc.eq.0)then
        OUTSTR=tmp
      elseif(iloc.gt.1)then
        OUTSTR=tmp(1:ILOC-1)
      endif

C Find out the number of separate words/string groupings.
      if(IEXP.eq.99)then
        CALL CHITMS(OUTSTR,ITEMS)
      elseif(IEXP.eq.0)then
        ITEMS=0
      elseif(IEXP.gt.0)then
        CALL CHITMS(OUTSTR,ITEMS)
        if(IEXP.ne.ITEMS)then
          LN=max(1,lnblnk(currentfile))
          LNM=max(1,lnblnk(MSG))
          if(currentfile(1:2).ne.'  ')then
            WRITE(loutstr,'(3A,I3,3A)')'In ',currentfile(1:LN),
     &        ' expecting',IEXP,' items (',MSG(1:LNM),') in...'
            CALL LUSRMSG(loutstr,OUTSTR,'F')
          else
            WRITE(loutstr,'(A,I3,3A)')' Expecting',IEXP,' items (',
     &         MSG(1:LNM),') in...'
            CALL LUSRMSG(loutstr,OUTSTR,'F')
          endif
        endif
      endif

    4 RETURN

  101 IER=1
      IF(IR.EQ.1)THEN
        LN=max(1,lnblnk(currentfile))
        LNM=max(1,lnblnk(MSG))
        if(currentfile(1:2).ne.'  ')then
          WRITE(loutstr,'(5A)')' In ',currentfile(1:LN),
     &      ' error reading (',MSG(1:LNM),') in...'
        else
          WRITE(loutstr,'(A,A)',IOSTAT=IOS,ERR=1)' Error reading: ',
     &      MSG(1:LNM),') in...'
        endif
        CALL LUSRMSG(loutstr,OUTSTR,'W')
      ENDIF
      goto 4

  102 IER=2
      IF(IR.EQ.1)THEN
        LN=max(1,lnblnk(currentfile))
        LNM=max(1,lnblnk(MSG))
        if(currentfile(1:2).ne.'  ')then
          WRITE(loutstr,'(5A)')' In ',currentfile(1:LN),
     &      ' error reading (',MSG(1:LNM),') EOF sensed.'
        else
          WRITE(loutstr,'(3A)',IOSTAT=IOS,ERR=1)' Error reading ',
     &      MSG(1:LNM),' EOF sensed.'
        endif
        CALL LUSRMSG(' ',loutstr,'-')
      ENDIF
      goto 4

   1  if(IOS.eq.2)then
        write(6,*) 'STRIPC: permission error writing warning ',OUTSTR
      else
        write(6,*) 'STRIPC: error writing warning re: ',OUTSTR
      endif

      END

C ******************** LSTRIPC
C LSTRIPC strips comments from a ASCII file (long) string and returns the data.
C It assumes that if a string begins with a '#' then the whole line is
C a comment an the next line is read.  If a ' #' is discovered within
C a line the rest of the line is removed.
C IER=0 if ok. MSG is a text string used in error messages. If
C IR=0 then acts silently, otherwise notes when EOF found.
C IEXP is the number of expected items in the line:
C   IEXP = 0 means don't care or already know no. items - don't check
C   IEXP >0  means a specific number of items expected (error if not)
C   IEXP = 99 check number of items and return in ITEMS

      SUBROUTINE LSTRIPC(INPCH,OUTSTR,IEXP,ITEMS,IR,MSG,IER)
#include "espriou.h"

      CHARACTER*248 OUTSTR,STRING,TMP
      CHARACTER loutstr*248
      CHARACTER*(*) MSG

C Read a line of the file, strip off any trailing blanks, if the first
C character is a # then read the next line from the file.
      IER=0
    8 READ(INPCH,10,IOSTAT=ISTAT,ERR=101,END=102)STRING
   10 FORMAT(A)
      tmp=STRING(1:LNBLNK(STRING))

C Take the string and check for a #, discarding any text which follows.
      iloc = INDEX(tmp,'#')
      if(iloc.eq.1)then
        goto 8
      elseif(iloc.eq.0)then
        OUTSTR=tmp
      elseif(iloc.gt.1)then
        OUTSTR=tmp(1:ILOC-1)
      endif

C Find out the number of separate words/string groupings.
      if(IEXP.eq.99)then
        CALL LCHITMS(OUTSTR,ITEMS)
      elseif(IEXP.eq.0)then
        ITEMS=0
      elseif(IEXP.gt.0)then
        CALL LCHITMS(OUTSTR,ITEMS)
        if(IEXP.ne.ITEMS)then
          LN=max(1,lnblnk(currentfile))
          LNM=max(1,lnblnk(MSG))
          if(currentfile(1:2).ne.'  ')then
            WRITE(loutstr,'(3A,I3,3A)')' In ',currentfile(1:LN),
     &        ' expecting',IEXP,' items (',MSG(1:LNM),') in...'
            CALL LUSRMSG(loutstr,OUTSTR,'F')
          else
            WRITE(loutstr,'(A,I3,3A)')' Expecting',IEXP,' items (',
     &         MSG(1:LNM),') in...'
            CALL LUSRMSG(loutstr,OUTSTR,'F')
          endif
        endif
      endif

    4 RETURN

  101 IER=1
      IF(IR.EQ.1)THEN
        LN=max(1,lnblnk(currentfile))
        LNM=max(1,lnblnk(MSG))
        if(currentfile(1:2).ne.'  ')then
          WRITE(loutstr,'(5A)')' In ',currentfile(1:LN),
     &      ' error reading (',MSG(1:LNM),') in...'
        else
          WRITE(loutstr,'(A,A)',IOSTAT=IOS,ERR=1)' Error reading: ',
     &      MSG(1:LNM),') in...'
        endif
        CALL LUSRMSG(loutstr,OUTSTR,'W')
      ENDIF
      goto 4

  102 IER=2
      IF(IR.EQ.1)THEN
        LN=max(1,lnblnk(currentfile))
        LNM=max(1,lnblnk(MSG))
        if(currentfile(1:2).ne.'  ')then
          WRITE(loutstr,'(5A)')' In ',currentfile(1:LN),
     &      ' error reading (',MSG(1:LNM),') EOF sensed.'
        else
          WRITE(loutstr,'(3A)',IOSTAT=IOS,ERR=1)' Error reading ',
     &      MSG(1:LNM),' EOF sensed.'
        endif
        CALL LUSRMSG(' ',loutstr,'-')
      ENDIF
      goto 4

   1  if(IOS.eq.2)then
        write(6,*) 'LSTRIPC: permission error writing warning ',OUTSTR
      else
        write(6,*) 'LSTRIPC: error writing warning re: ',OUTSTR
      endif
      return

      END

C ******************** STRIPC400
C STRIPC400 strips comments from a ASCII file (400 char long) string and returns the data.
C It assumes that if a string begins with a '#' then the whole line is
C a comment an the next line is read.  If a ' #' is discovered within
C a line the rest of the line is removed.
C IER=0 if ok. MSG is a text string used in error messages. If
C IR=0 then acts silently, otherwise notes when EOF found.
C IEXP is the number of expected items in the line:
C   IEXP = 0 means don't care or already know no. items - don't check
C   IEXP >0  means a specific number of items expected (error if not)
C   IEXP = 99 check number of items and return in ITEMS

      SUBROUTINE STRIPC400(INPCH,OUTSTR,IEXP,ITEMS,IR,MSG,IER)
#include "espriou.h"

      CHARACTER*400 OUTSTR,STRING,TMP
      CHARACTER loutstr*248
      character truncstr*248   ! truncate outsr for reporting.
      CHARACTER*(*) MSG

C Read a line of the file, strip off any trailing blanks, if the first
C character is a # then read the next line from the file.
      IER=0
    8 READ(INPCH,10,IOSTAT=ISTAT,ERR=101,END=102)STRING
   10 FORMAT(A)
      tmp=STRING(1:LNBLNK(STRING))

C Take the string and check for a #, discarding any text which follows.
      iloc = INDEX(tmp,'#')
      if(iloc.eq.1)then
        goto 8
      elseif(iloc.eq.0)then
        OUTSTR=tmp
      elseif(iloc.gt.1)then
        OUTSTR=tmp(1:ILOC-1)
      endif
      lastcc = MIN0(LNBLNK(OUTSTR),247)
      write(truncstr,'(a)') OUTSTR(1:lastcc)

C Find out the number of separate words/string groupings.
      if(IEXP.eq.99)then
        CALL CHITMS400(OUTSTR,ITEMS)
      elseif(IEXP.eq.0)then
        ITEMS=0
      elseif(IEXP.gt.0)then
        CALL CHITMS400(OUTSTR,ITEMS)
        if(IEXP.ne.ITEMS)then
          LN=max(1,lnblnk(currentfile))
          LNM=max(1,lnblnk(MSG))
          if(currentfile(1:2).ne.'  ')then
            WRITE(loutstr,'(3A,I3,3A)')' In ',currentfile(1:LN),
     &        ' expecting',IEXP,' items (',MSG(1:LNM),') in...'
            CALL LUSRMSG(loutstr,truncstr,'F')
          else
            WRITE(loutstr,'(A,I3,3A)')' Expecting',IEXP,' items (',
     &         MSG(1:LNM),') in...'
            CALL LUSRMSG(loutstr,truncstr,'F')
          endif
        endif
      endif

    4 RETURN

  101 IER=1
      IF(IR.EQ.1)THEN
        LN=max(1,lnblnk(currentfile))
        LNM=max(1,lnblnk(MSG))
        if(currentfile(1:2).ne.'  ')then
          WRITE(loutstr,'(5A)')' In ',currentfile(1:LN),
     &      ' error reading (',MSG(1:LNM),') in...'
        else
          WRITE(loutstr,'(A,A)',IOSTAT=IOS,ERR=1)' Error reading: ',
     &      MSG(1:LNM),') in...'
        endif
        CALL LUSRMSG(loutstr,OUTSTR,'W')
      ENDIF
      goto 4

  102 IER=2
      IF(IR.EQ.1)THEN
        LN=max(1,lnblnk(currentfile))
        LNM=max(1,lnblnk(MSG))
        if(currentfile(1:2).ne.'  ')then
          WRITE(loutstr,'(5A)')' In ',currentfile(1:LN),
     &      ' error reading (',MSG(1:LNM),') EOF sensed.'
        else
          WRITE(loutstr,'(3A)',IOSTAT=IOS,ERR=1)' Error reading ',
     &      MSG(1:LNM),' EOF sensed.'
        endif
        CALL LUSRMSG(' ',loutstr,'-')
      ENDIF
      goto 4

   1  if(IOS.eq.2)then
        write(6,*) 'LSTRIPC: permission error writing warning ',OUTSTR
      else
        write(6,*) 'LSTRIPC: error writing warning re: ',OUTSTR
      endif
      return

      END

C ********************* CHITMS
C CHITMS checks a character string (A), returning the number of data
C items (IW) separated by ' ' tab or ','.  Note to keep from
C overwriting the string passed, deal with a copy.
      SUBROUTINE CHITMS(A,IW)
      CHARACTER*(*) A
      CHARACTER B*124,C*1

      lastcc = MIN0(LNBLNK(A),123)
      WRITE(B,'(A)',IOSTAT=IOS,ERR=1) A(1:lastcc+1)
C      WRITE(B,'(A)') A(1:LNBLNK(A)+1)

C Loop through B and convert all ',' and 'tabs' to blanks.
      DO 100 I=1,LNBLNK(B)+1
        C=B(I:I)
        IF(C.EQ.','.OR.ICHAR(C).EQ.9) B(I:I)=' '
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
      IF(K.GE.LNBLNK(B))RETURN
      IF(C.EQ.' ') GOTO 102
      GOTO 20

   1  if(IOS.eq.2)then
        write(6,*) 'CHITMS: permission error in internal write: ',A
      else
        write(6,*) 'CHITMS: error in internal write: ',A
      endif
      return

      END

C ********************* LCHITMS
C LCHITMS checks a long character string (A), returning the number of data
C items (IW) separated by ' ' tab or ',' or '|'.  Note to keep from
C overwriting the string passed, deal with a copy.
      SUBROUTINE LCHITMS(A,IW)
      CHARACTER*(*) A
      CHARACTER B*248,C*1

      lastcc = MIN0(LNBLNK(A),247)
      WRITE(B,'(A)',IOSTAT=IOS,ERR=1) A(1:lastcc+1)

C Loop through B and convert all ',' 'tabs' '|' to blanks.
      DO 100 I=1,LNBLNK(B)+1
        C=B(I:I)
        IF(C.EQ.','.OR.ICHAR(C).EQ.9.OR.ICHAR(C).EQ.124) B(I:I)=' '
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
      IF(K.GE.LNBLNK(B))RETURN
      IF(C.EQ.' ') GOTO 102
      GOTO 20

   1  if(IOS.eq.2)then
        write(6,*) 'LCHITMS: permission error in internal write: ',A
      else
        write(6,*) 'LCHITMS: error in internal write: ',A
      endif
      return

      END

C ********************* CHITMS400
C CHITMS400 checks a 400 long char string (A), returning the number of data
C items (IW) separated by ' ' tab or ',' or '|'.  Note to keep from
C overwriting the string passed, deal with a copy.
      SUBROUTINE CHITMS400(A,IW)
      CHARACTER*(*) A
      CHARACTER B*400,C*1

      lastcc = MIN0(LNBLNK(A),247)
      WRITE(B,'(A)',IOSTAT=IOS,ERR=1) A(1:lastcc+1)

C Loop through B and convert all ',' 'tabs' '|' to blanks.
      DO 100 I=1,LNBLNK(B)+1
        C=B(I:I)
        IF(C.EQ.','.OR.ICHAR(C).EQ.9.OR.ICHAR(C).EQ.124) B(I:I)=' '
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
      IF(K.GE.LNBLNK(B))RETURN
      IF(C.EQ.' ') GOTO 102
      GOTO 20

   1  if(IOS.eq.2)then
        write(6,*) 'CHITMS400: permission error in internal write: ',A
      else
        write(6,*) 'CHITMS400: error in internal write: ',A
      endif
      return

      END

C ******************* CHARCH
C CHARCH: Older routine to check a string for a number of data items.
C This is an older version of CHITMS, however it modifies the string A.
      SUBROUTINE CHARCH(A,ND,IERR)
      CHARACTER*72 A

C Number of commas required.
      NC=ND

C Assign field (comma) counter, character counter
C and field start indicator.
      IC=0
      ICC=0
      ISC=0

      DO 10 I=1,72

C Jump if required number of commas found.
      IF(IC.EQ.NC)goto 10

      ICC=I
      IF(A(I:I).EQ.',')goto 1
      IF(A(I:I).EQ.' ')goto 2
      goto 3

    2 IF(ISC.EQ.0)goto 10
      A(I:I)=','
      goto 1

    3 IF(ISC.EQ.0)ISC=I
      goto 10

    1 IC=IC+1
      ISC=0

   10 CONTINUE

      IERR=-ICC
      IF(IC.NE.NC)IERR=1

      RETURN
      END

C ******************** NOYES
C NOYES is an INTEGER FUNCTION which is used to read the answer to a
C question.  A line is read from the user, and the value of the
C function is as follows:-
C   1 The answer was '1', ' Y', or '  YES'
C   0 The answer was '0', ' N', or '  NO'
C  -1 otherwise.
      FUNCTION NOYES(J)
      PARAMETER ( IUIN=5 )
      CHARACTER IA*3

      READ(IUIN,100)IA
 100  FORMAT(A3)
      IF(IA.EQ.'1'.OR.IA.EQ.'Y'.OR.IA.EQ.'y'
     &            .OR.IA.EQ.'yes'.OR.IA.EQ.'YES')THEN
        NOYES=1
        RETURN
      ELSEIF(IA.EQ.'0'.OR.IA.EQ.'N'.OR.IA.EQ.'n'
     &                .OR.IA.EQ.'no'.OR.IA.EQ.'NO')THEN
        NOYES=0
        RETURN
      ELSE
        NOYES=-1
        RETURN
      ENDIF
      END

c ******************** IFIRST
C IFIRST: Returns the ASCII value for the first character in
C a string ISTR.
      FUNCTION IFIRST(ISTR)
      CHARACTER*1 JSTR
      CHARACTER*(*) ISTR
      JSTR=ISTR(1:1)
      IFIRST=ICHAR(JSTR)
      RETURN
      END

c ******************** EDAY
C EDAY Returns year day number IYDN when passed the day of the month
C IDAYN and the month number IMTHN. 1st Jan= 1, 31st Dec=365, no leap
C years considered.
      SUBROUTINE EDAY(IDAYN,IMTHN,IYDN)
      DIMENSION MONTH(12)
      DATA MONTH/31,28,31,30,31,30,31,31,30,31,30,31/
      IYDN=IDAYN
      IF(IMTHN.EQ.1)RETURN
      DO 10 I=2,IMTHN
        IYDN=IYDN+MONTH(I-1)
   10 CONTINUE
      RETURN
      END

C *********************** EDAYR
C 'EDAYR' returns the day and month numbers from the day-of-year where:
C day-of-year 1 = 1st January and day-of-year 365 = 31st December.
C NO LEAP YEARS ARE CONSIDERED!
      SUBROUTINE EDAYR(IYDN,IDAYN,IMTHN)
      DIMENSION MONTH(12),IACTOT(11)
      DATA MONTH/31,28,31,30,31,30,31,31,30,31,30,31/
      DATA IACTOT/31,59,90,120,151,181,212,243,273,304,334/
      IMTHN=1

C Determine the month number.
      DO 10 I=1,11
        IF(IYDN.GT.IACTOT(I))IMTHN=IMTHN+1
   10 CONTINUE

C Determine the day of the month.
      IF(IMTHN.EQ.1)THEN
        IDAYN=IYDN
        RETURN
      ELSE
        IDAYN=0
        J=IMTHN-1
        DO 20 I=1,J
          IDAYN=IDAYN+MONTH(I)
   20   CONTINUE
        IDAYN=IYDN-IDAYN
        RETURN
      ENDIF
      END

C ********************** EWEEKD
C 'EWEEKD' returns the day of the week given the day of month, month
C and year as integers.
C MON=1, TUE=2, WED=3, THU=4, FRI=5, SAT=6 AND SUN=7.
      SUBROUTINE EWEEKD(ND,NM,NY,IDAY)
      common/OUTIN/IUOUT,IUIN
      DIMENSION MMTH(12),MYR(5)
      character outs*124
      DATA MMTH/1,4,4,0,2,5,0,3,6,1,4,6/
      DATA MYR/2,4,2,0,6/

      IF(ND.EQ.0.OR.NM.EQ.0.OR.NY.EQ.0)goto 50
      IL1=MOD(NY,100)
      IL2=MOD(IL1,4)
      IL3=MOD(NY,1000)
      LEAP=0
      IF(IL2.NE.0)goto 11
      IF(IL3.EQ.0.OR.IL1.NE.0)LEAP=1
   11 IF(LEAP.EQ.0.AND.NM.EQ.2.AND.ND.EQ.29)goto 50
      II=IL1/4
      IDAY=IL1+II+ND+MMTH(NM)
      IF(LEAP.EQ.1.AND.(NM.EQ.1.OR.NM.EQ.2))IDAY=IDAY-1
      IL2=INT(.01*(NY-IL1+1))
      IL2=IL2-15
      IF(IL2.LE.0.OR.IL2.GT.5)goto 50
      IL3=MYR(IL2)
      IF(IL2.NE.2.OR.NM.GT.9)goto 12
      IF(NM.EQ.9.AND.ND.GE.14)goto 12
      IF(NM.EQ.9.AND.ND.GT.2)goto 50
      IL3=1
   12 IDAY=IDAY+IL3+5
      IDAY=MOD(IDAY,7)+1
      goto  10
   50 WRITE(outs,'(a,3I6,a)')' The date ',ND,NM,NY,' is illegal.'
      call edisp(iuout,outs)
   10 CONTINUE
      RETURN
      END

C ********************* EDAYCH
C EDAYCH will check for errors in the users specification of the day and
C month under consideration.
      SUBROUTINE EDAYCH(ID,IM,IERR)
      common/OUTIN/IUOUT,IUIN
      DIMENSION MNTH(12)
      DATA MNTH/31,28,31,30,31,30,31,31,30,31,30,31/
      IERR=0
      IF(IM.LT.1.OR.IM.GT.12)THEN
        call edisp(iuout,' Month value outwith allowable range. ')
        IERR=1
      ENDIF
      IF(ID.LT.1.OR.ID.GT.MNTH(IM))THEN
        call edisp(iuout,' Day value outwith allowable monthly range.')
        IERR=1
      ENDIF
      RETURN
      END

C ********************* DATTIM
C DATTIM returns UNIX time via a string in the form : 16 Sep 73 14:23.
      SUBROUTINE DATTIM(DT)
      CHARACTER*(*) DT
      CHARACTER*24 TUNIX

      CALL FDATE(TUNIX)
      DT(1:6) = TUNIX(5:10)
      DT(7:7) = ' '
      DT(10:15) = TUNIX(11:16)
      RETURN
      END

C ******************** STDATE
C STDATE takes the day of year and returns two descriptive strings.
C DESCR takes the form '12-Jan' and DESCR1 takes the form 'Fri-12-Jan'.
      SUBROUTINE STDATE(IYEAR,IDOY,DESCR,DESCR1)
      common/OUTIN/IUOUT,IUIN
      DIMENSION MTHNAM(12),DAYNAM(7)

      CHARACTER MTHNAM*3,DAYNAM*3,DESCR*7,DESCR1*10
      DATA MTHNAM/'Jan','Feb','Mar','Apr','May','Jun','Jul','Aug',
     &            'Sep','Oct','Nov','Dec'/
      DATA DAYNAM/'Mon','Tue','Wed','Thu','Fri','Sat','Sun'/

C Determine whether IDAY is a weekday, saturday or sunday.
C Determine the month and day of month via EDAYR.
      CALL EDAYR(IDOY,IDAYN,IMTHN)
      CALL EWEEKD(IDAYN,IMTHN,IYEAR,IDWK)

C Build up the descriptive text strings if a known month. Trap out
C of range idwk and imthn to prevent string array crashes.
      if(imthn.gt.0.and.imthn.le.12)then
        WRITE(DESCR,'(i2.2,2a)',ERR=1)IDAYN,'-',MTHNAM(IMTHN)
        if(idwk.gt.0.and.idwk.le.7)then
          WRITE(DESCR1,'(2a,i2.2,2a)',ERR=1)DAYNAM(IDWK),'-',IDAYN,'-',
     &      MTHNAM(IMTHN)
        else
          WRITE(DESCR1,'(a,i2.2,2a)',ERR=1) 'XXX-',IDAYN,'-',
     &      MTHNAM(IMTHN)
        endif
      else
        WRITE(DESCR,'(i2.2,a)',ERR=1)IDAYN,'-XXX'
        if(idwk.gt.0.and.idwk.le.7)then
          WRITE(DESCR1,'(2a,i2.2,a)',ERR=1)DAYNAM(IDWK),'-',IDAYN,'-XXX'
        else
          WRITE(DESCR1,'(a,i2.2,a)',ERR=1) 'XXX-',IDAYN,'-XXX'
        endif
      endif

      RETURN
 1    call usrmsg('STDATE: problem writing date strings.',' ','W')
      END

C ******************** ESTIME
C ESTIME takes an integer timestep and returns two string descriptions:
C DESCRH in the form '12h28' and DESCRD which takes the form of 12.46,
C As well as the time as a fraction of a day TIMER.
C STIME takes timestep averaging IDAVER into account ie. 0=averaging,
C 1=no averaging. NTS is the number of timesteps per hour.
      SUBROUTINE ESTIME(NTS,IDAVER,ITIME,DESCRH,DESCRD,TIMER)
      CHARACTER*5 DESCRH, DESCRD

      ADJUST=0.
      IF(IDAVER.EQ.0)ADJUST=1.0/(FLOAT(NTS)*2.0)
      TIMER=FLOAT(ITIME)/FLOAT(NTS)
      TIMER=TIMER-ADJUST
      MIN=INT((TIMER-AINT(TIMER))*60.)
      WRITE(DESCRH,'(i2.2,a,i2.2)',ERR=1)INT(TIMER),'h',MIN
      WRITE(DESCRD,'(f5.2)',IOSTAT=IOS,ERR=1)TIMER
      TIMER=TIMER/24.0
      RETURN

 1    if(IOS.eq.2)then
        call usrmsg('ESTIME: permission writing time strings.',' ','W')
      else
        call usrmsg('ESTIME: problem writing time strings.',' ','W')
      endif
      return

      END

C ******************** EDTIME
C EDTIME takes an real time and returns two string descriptions:
C DESCRH in the form '12h28' and DESCRD which takes the form of 12.46,
C As well as the time as a fraction of a day TIMER.
      SUBROUTINE EDTIME(TIME,DESCRH,DESCRD,TIMER)
      CHARACTER*5 DESCRH, DESCRD

      MIN=INT((TIME-AINT(TIME))*60.)
      WRITE(DESCRH,'(i2.2,a,i2.2)',ERR=1)INT(TIME),'h',MIN
      WRITE(DESCRD,'(f5.2)',IOSTAT=IOS,ERR=1)TIME
      TIMER=TIME/24.0
      RETURN

 1    if(IOS.eq.2)then
        call usrmsg('EDTIME: permission writing time strings.',' ','W')
      else
        call usrmsg('EDTIME: problem writing time strings.',' ','W')
      endif
      return

      END

C ************* EPERSTR
C EPERSTR creates three strings representing the start and stop
C time of a diary period based on the preferred time & date
C display format.
C IFDAY  0 gives 'DOY 10', 1 gives '10 Jan', 2 gives 'Fri 10 Jan'
C IFTIME 0 gives '10h30', 1 gives '10.50', 2 gives '0.4375'
C PERST1 (14 char) is:' 10h00  15h30 ',' 10.00  15.50 ',' 0.4375 0.6458'
C PERST3 (44 char):
C if IFDAY=0 then it is: 'period: DOY 100 to DOY 112, 1990'
C if IFDAY=1 then it is: 'period: 10 Jan to 31 Jan, 1990'
C if IFDAY=2 then it is: 'period: Mon 10 Jan to Mon 17 Jan, 1990'
C PERST2 (44 char) includes the time of day but not the year.
C IER=0 OK, IER=1 problem.
C BTIM and PETIM are in terms of decimal fractions of a day.
      SUBROUTINE EPERSTR(IYEAR,IBDOY,IBTIM,IEDOY,IETIM,NTSPH,
     &           IFDAY,IFTIME,PERST1,PERST2,PERST3,IER)
      CHARACTER T1H*5, T1D*5, T2H*5, T2D*5
      CHARACTER PERST1*14,PERST2*44,PERST3*44,DS*7,DS1*10,DE*7,DE1*10

      IER=0

C Generate view period string based on IBDOY,IEDOY,BTIM,ETIM
      CALL STDATE(IYEAR,IBDOY,DS,DS1)
      CALL STDATE(IYEAR,IEDOY,DE,DE1)
      CALL ESTIME(NTSPH,1,IBTIM,T1H,T1D,TIMER1)
      CALL ESTIME(NTSPH,1,IETIM,T2H,T2D,TIMER2)
      FD1=FLOAT(IBDOY)+TIMER1
      FD2=FLOAT(IEDOY)+TIMER2
      IF(IFTIME.EQ.0)THEN
        WRITE(PERST1,1,IOSTAT=IOS,ERR=10)T1H,T2H
    1   FORMAT(' ',A5,'  ',A5,' ')
      ELSEIF(IFTIME.EQ.1)THEN
        WRITE(PERST1,1,IOSTAT=IOS,ERR=10)T1D,T2D
      ELSEIF(IFTIME.EQ.2)THEN
        WRITE(PERST1,2,IOSTAT=IOS,ERR=10)TIMER1,TIMER2
    2   FORMAT(2F7.4)
      ENDIF

      IF(IFDAY.EQ.0)THEN
        WRITE(PERST3,3,IOSTAT=IOS,ERR=10)IBDOY,IEDOY,IYEAR
    3   FORMAT('period: DOY ',I3,' to DOY ',I3,', ',I4)
        IF(IFTIME.EQ.0)THEN
          WRITE(PERST2,4,IOSTAT=IOS,ERR=10)IBDOY,T1H,IEDOY,T2H
    4     FORMAT('period: DOY ',I3,' @ ',A5,' to DOY ',I3,' @ ',A5)
        ELSEIF(IFTIME.EQ.1)THEN
          WRITE(PERST2,4,IOSTAT=IOS,ERR=10)IBDOY,T1D,IEDOY,T2D
        ELSEIF(IFTIME.EQ.2)THEN
          WRITE(PERST2,5,IOSTAT=IOS,ERR=10)FD1,FD2
    5     FORMAT('period: DOY ',F10.6,' to DOY ',F10.6)
        ENDIF
      ELSEIF(IFDAY.EQ.1)THEN
        WRITE(PERST3,6,IOSTAT=IOS,ERR=10)DS,DE,IYEAR
    6   FORMAT('period: ',A7,' to ',A7,', ',I4)
        IF(IFTIME.EQ.0)THEN
          WRITE(PERST2,7,IOSTAT=IOS,ERR=10)DS,T1H,DE,T2H
    7     FORMAT('period: ',A7,' @ ',A5,' to ',A7,' @ ',A5)
        ELSE
          WRITE(PERST2,7,IOSTAT=IOS,ERR=10)DS,T1D,DE,T2D
        ENDIF
      ELSEIF(IFDAY.EQ.2)THEN
        WRITE(PERST3,8,IOSTAT=IOS,ERR=10)DS1,DE1,IYEAR
    8   FORMAT('period: ',A10,' to ',A10,', ',I4)
        IF(IFTIME.EQ.0)THEN
          WRITE(PERST2,9,IOSTAT=IOS,ERR=10)DS1,T1H,DE1,T2H
    9     FORMAT('period: ',A10,'@',A5,' - ',A10,'@',A5)
        ELSE
          WRITE(PERST2,9,IOSTAT=IOS,ERR=10)DS1,T1D,DE1,T2D
        ENDIF
      ENDIF
      RETURN

  10  if(IOS.eq.2)then
        write(6,*) 'EPERSTR: permission error writing strings.'
      else
        write(6,*) 'EPERSTR: error writing warning or strings.'
      endif
      return

      END
