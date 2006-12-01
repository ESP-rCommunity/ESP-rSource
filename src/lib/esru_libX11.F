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

C The file esru_cut_lib.f is a collection of text and bid-mapped terminal
C control Fortran Functions and Subroutines:
C  EASKI:   Ask user for an integer with prompt, error messages & range
C           checking as passed parameters.
C  EASKR:   Ask user for a real number with prompt, error messages & range
C           checking as passed parameters.
C  EASKE:   Ask user for a real in exponential format, otherwise as easkr.
C  EASKF:   Ask user for a file name with prompt, error messages & help.
C  EASKS:   Ask user for a string with prompt, error messages & help.
C EASKS248: Edit a long (248 char) string in sections.
C  EASKSCMD Ask user for a string with prompt, alt, error messages & help.
C  EASKS2CMD Ask user for a string with prompt, 2 alts, error messages & help.
C  st2name: Given `string' swap blanks & wildcards to _ return as `name'.
C  st2file: Given `string' strip blanks & wildcards and return as `name'.
C backslashit: Given `string' swap / to \ : return as `name'.
C iprevblnk: Given a string, return position of blank just before ipos.
C inextblnk: Given a string, return position of blank just after ipos.
C  icutstr: Given a string, cuts icut characters from position ipos.
C  EPICKS:  Allows several selections to be made from an array of strings.
C  MENUATOL Single item menu pick with items passed via parameters.
C  ASKOK:   Generic yes/no/default facility returning OK as a logical parameter.
C  EASKAB:  Generic A/B choice facility returning 1 or 2 according to which
C           of the choices has been chosen.
C  EASKABC: Generic A/B/C choice facility returning 1, 2 or 3 according
C           to which of the choices has been chosen.
C EASKABCD: Generic A/B/C/D choice facility returning 1-4 according
C           to which of the choices has been chosen.
C  EASKPER: Provides interface to specification of a period of days.
C eAskPerYear: Provides interface to specification of a multiyear period of days.
C easkPerGen: Handles specification of both single-year and multi-year
C           simulation periods.
C  DAYCLK:  Print day, month, day no. and time based on the julian day & time.

C  FDROOT:  Given a file name see if it contains a path.
C  EFOPSEQ: Open a sequential file with existance flag & path check.
C  EFOPRAN: Open a random access file with existance flag & path check.
C  FINDFIL: Check existance of SFILE (with path) & return logical XST.
C  ADDPATH: Return file name appended onto the path and logical concat.
C  GETTOKENS checks a string, returning nb of tokens and array of tokens.

C  C2FSTR:  Convert c function returned string to fortran format.
C  TERMODE: Returns logical nane of terminal type.
C  EKPAGE:  Maps key characters, pages & array indexs in long display lists.
C  KEYIND:  Decodes EMENU index and returns the array index of the item.
C  EPMENSV: saves menu definitions (common block PMENU).
C  EPMENRC: recovers menu definitions (common block PMENU) from PMENUSV.
C  EPAGE:   Screen control: page without waiting.
C  EPAGEW:  Screen control: Wait before paging.
C  EPWAIT:  Screen control: Wait without paging.
C  EPAGEND: Screen control: Page then close window if open.
C  EMPAGE:  Low level screen control for paging based on terminal model.
C  ELINC:   Controls scratch pad output for text screens.
C  EPAGES:  Initialise terminal, set up a scratch pad & line count.
C  SETLINC: Allows the user to change the length of the text page.
C  EMENU:   Control variable width menu display on various terminals.
C  EWMENU:  Is the binding to C function for menu dialogue.
C  VWMENU:  Is the binding to C function for variable width menu.
C  EMKEY:   Returns key (a-z) for a menu item based on data array index.
C  USRMSG:  Generic message/prompt facility for all terminal types.
C  LUSRMSG:  Generic long message/prompt facility for all terminal types.
C  EDISP:   Generic send text to scrolling display (text or graphic).
C EDISP248 Displays a 248 char block of text (text or graphic).
C  EDDISP:  As edisp with text separated with current delimiter.
C  SDELIM:  Replaces blanks in a string A with alternative delimiter.
C  PHELPD:  Displays the current contents of common pophelp.
C  PHELPW:  Returns the width IWH of the longest popup help string.
C  UPDVIEW: Called from C to pass back updates to common VIEWPX & GFONT
C  LISTAS:  General read & display of an ascii file.

C  isadll:  Checks if module is being used as a dll (silent running).
C  isunix:  Checks if machine type is Unix or NT.
C  usrhome: Returns user's home directory.
C  usrname: Returns user's login name.
C  usrdir:  Returns user's current directory.
C  esppid:  Find current process number.
C  tstamp:  Date stamp with message.
C  dstamp:  Get date stamp in the form: Fri Jan 23 09:34:31 1998.
C  comparedate: is passed two date strings (generated by call to dstamp)
C    and returns 1 if first date is more current, 0 if the same, otherwise -1.
C  getsecs: Get computer clock seconds.
C  EGETW:  Finds first word after pos k in a string (' 'or','or tab separated).
C  iEGetArrW: integer function that breaks a string into an array.
C  EGETP:  Finds first phrase after pos k in a string (tab separated).
C  EGETWI: As EGETW for an integer with range checking & error messages.
C  EGETWR: As EGETW for a real with range checking & error messages.
C  EGETWRA: Recovers (IRVA) reals of real array (RVA) from an ASCII file.
C  EGETWIA: Recovers (IRVA) int of array (IVA) from an ASCII file.
C EGETAGWIA recovers (IRVA) integers of integer array (IVA) from a string
C   TSTR (from position K) and if TSTR does not hold all of the array then
C   it continues reading from an ASCII file (unit IAF) reading as
C   many lines as necessary to recover the data.
C EGETAGWRA recovers (IRVA) reals of real array (RVA) from a string
C   TSTR (from position K) and if TSTR does not hold all of the array then
C   it continues reading from an ASCII file (unit IAF) reading as
C   many lines as necessary to recover the data.
C  EGETRM: Returns the remainder (RSTR with no leading blanks) from
C          a text string (TSTR) after position k.
C  EPERSTR: creates three strings representing the start and
C           stop time of a diary period.
C  INTSTR: Converts integer into string (10 char) w/o leading blanks.
C  RELSTR: Converts a real into a string (12 char) w/o leading blanks.
C  REL16STR: Converts a real into a string (16 char) w/o leading blanks.
C  EXPSTR: Converts a exponential into a string (10 char) w/o leading blanks.
C  ARLIST: takes a real array (rlist) and builds a packed string.
C  AILIST: takes an int array (ilist) and builds a packed string.
C  ASLIST: takes an string array (list*24) and builds a packed string.
C  ASLIST2 takes the range (inst to inil) items of an string array (list)
C  ASFLIST: takes an string array (list*48) and builds a packed string.
C  RELCNV: Converts 20 char word into real.
C  STRIPC: Strips comments from a ASCII file str (124 char) & returns data.
C  LSTRIPC: Strips comments from a ASCII file str (248 char) & returns data.
C  CHITMS: Checks a character string & returns the number of data items.
C  CHARCH: Routine to check a string for a specific number of data items.
C  NOYES:  INTEGER FUNCTION to read the answer Y,y,1,N,n,0 to a question.
C  IFIRST: Function returning ASCII value for 1st char in a string ISTR.

C The following routines are available for file and date functions:
C  ERPFREE: Is used to close any file.
C  EFDELET: Delete the current file opened under IUN and return ISTAT.

C  FPOPEN:  is used to open a file with a name.
C  FPRAND:  is used to open a file with a name for random access.

C  EDAY:   Returns the year day number when passed the day of month & month.
C  STDATE: Takes the day of year and returns two descriptive strings:
C          DESCR takes the form '12 Jan' & DESCR1 takes the form 'Fri 12 Jan'.
C  ESTIME: Takes an integer timestep and returns two string descriptions:
C          DESCRH in the form '12h28' & DESCRD which takes the form of 12.46.
C  EDAYR: returns the day and month numbers from the day-of-year.
C  EWEEKD: returns the day of the week given the day of month, month
C          and year as integers.
C  EDAYCH: Checks for errors in the users specification of the day & month.
C  DATTIM: returns UNIX time via a string in the form : 16 Sep 73 14:23.
C  SITELL2S: Takes site lat & long and returns descrptive string.
C  DAYCLK: Prints the day, month, day no. & time based on DOY & decimal hour.

C  EPROMPT: Does nothing, for compatibility only.

C  ESIND:   Function returning SIN of angle where angle is given in degrees.
C  ECOSD:   Function returning COS of angle where angle is given in degrees.
C  ETAND:   Function returning TAN of angle where angle is given in degrees.
C  IFAX :   Integer function returning the integer part of it's argument.
c  EAZALT:  Computes the solar azimuth & altitude angles at current time.
C  AGNXYZ:  Given solar azi & elev return viewing coords @ 1000m.
C  EPAREA:  Calculates the area for any closed polygon.
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
C  WIREPK:  Called from 'C' upon a wireframe control button pick.
C  EVSET:   provides setup environment for wire frame view w/in prj.
C  ASKTIM:  Enquire month and day and time (real for view).
C  ang3vtx: Get angle between three vertex.
C  CLOSE3D: Calc min dist between two lines in 3D return dist and closest points.
C  CROW:    Function returns shortest dist between two points P(3) and Q(3).
C  CROWXYZ: Function returns shortest dist between two points Px,Py,Pz and Qx,Qy,Qz.
C  UVXYZ:   Returns Unit vector Ux,Uy,Uz from two points Px,Py,Pz and Qx,Qy,Qz.
C  UVAB:    Returns Unit vector U of vector A.
C  PLNDIS:  Finds distance DIST from a point (x,y,z) to a plane (eq EQN).
C  AVER:    Returns the centre of gravity of an polygon array.
C  PLNDANG: Finds dihedral angel between two planes given their equations.
C  CROSS:   Performs a cross-product on vectors A() & B() returning in C().
C  CROSS2:  Performs a cross-product on vectors passing parameters as ax,ay,az etc.
C  DOT3(a,b,product) Return dot product of two vectors a & b.
C  ZEROS:   Clear a 4x4 array prior to doing vieweing transforms.
C  ECLOSE:  Checks tolerance between two real numbers.
C  SOLAIR:  Returns solair temperature.
C DNOTZERO: Function returns non zero value with the same sign (dbl precision).
C ANOTZERO: Function returns non zero value with the same sign (sngl precision).
C  SIGFIG:  Returns number to specified number of significant figures.
C  SIpre:   Returns suitable SI prefix for number supplied.
C Startbuffer: wrapper around call to win3dclr

C ******************** EASKI ********************
C EASKI is a facility for asking the user for an integer which
C incorporates the prompt, error messages and range checking
C as passed parameters as follows:

C IVAL is the integer returned, PROMP1 & PROMP2 are the prompts to be
C given to the user (similar to the syntax of USRMSG which allows
C two lines or a leading blank line).
C MIN is the minimum value, MAX is the maximum value.
C MINACT and MAXACT are actions to take if the range is exceeded:
C   'W' means warn user but accept, 'F' means refuse/fail to
C   accept value and ask again, '-' means no range checking.
C IDEFLT is the default value if 'D','d' typed.
C If a space or carriage return is typed then IVAL is not changed.
C ERMSG is a string appended to the range checking or read error
C to identify the value. IER is the error state, if 0 then OK.
C NHELP is the number of help lines and H() is the array
C of text strings to be printed out if '?','H','h' is typed
C by the user.
C Examples:
C Ask user for zone number.
C        ICOMP=1
C        H(1)='The zone number is required only for reporting '//
C     &           'purposes.'
C        CALL EASKI(ICOMP,' ',' What is the zone number? ',
C     &             1,'F',MCOM,'W',1,'zone number',IER,1)

      SUBROUTINE EASKI(IVAL,PROMP1,PROMP2,MINV,MINACT,
     &                 MAXV,MAXACT,IDEFLT,ERMSG,IER,NHELP)

      COMMON/OUTIN/IUOUT,IUIN
      COMMON/SPAD/MMOD,LIMIT,LIMTTY
      common/pophelp/h(60)
      dimension dh(60)
      CHARACTER*(*) PROMP1,PROMP2,ERMSG
      CHARACTER MINACT*1,MAXACT*1,A*1,WORD*10,STR1*10,STR2*10,H*72
      CHARACTER ask*10,OUTSTR*124,DSTR*124,dh*72
      LOGICAL OK,DOK

      IER=0

C If IVAL is outwith range for failure then set to default.
      if(MINACT.EQ.'F'.and.IVAL.LT.MINV)IVAL=IDEFLT
      if(MAXACT.EQ.'F'.and.IVAL.GT.MAXV)IVAL=IDEFLT

   20 WORD=' '
      ask ='        '
      WRITE(ask,'(I5)',iostat=ios,err=1)IVAL

C Generate help and default strings.
      CALL INTSTR(IDEFLT,STR2,IW2,IER)
      WRITE(DSTR,27,iostat=ios,err=1)STR2(1:IW2)
   27 FORMAT(' The default value (',a,') will be used.')

      CALL INTSTR(IVAL,STR2,IW2,IER)
      CALL INTSTR(IDEFLT,STR1,IW1,IER)
      IF(NHELP.GT.0)THEN
        WRITE(H(NHELP+1),65,iostat=ios,err=1)STR1(1:IW1),STR2(1:IW2)
      ELSE
        LN=max(1,LNBLNK(ERMSG))
        WRITE(H(1),64,iostat=ios,err=1)ERMSG(1:LN)
   64   FORMAT('No help available for ',a)
        WRITE(H(2),65,iostat=ios,err=1)STR1(1:IW1),STR2(1:IW2)
   65   FORMAT('Default (d):',a,' or Current (ok):',a)
      ENDIF

C Begin with prompts.
      IF(MMOD.EQ.8)THEN

C If in graphic mode use graphic faciltiies to edit the number.
        idef=0
        iquery=0
        iwidth=10
        CALL openaskbox(PROMP1,PROMP2,iwidth)
        WORD=' '
        if(NHELP.GT.0)then
          CALL PHELPW(NHELP+1,IHW,IER)
          call dupphelp(dh)
          call updhelp(dh,NHELP+1,IHW)
        else
          CALL PHELPW(2,IHW,IER)
          call dupphelp(dh)
          call updhelp(dh,2,IHW)
        endif
        CALL askdialog(ask,idef,iquery)

C Strip off the last character (end of line mark) as well as any
C leading blanks from string returned.
        call c2fstr(ask,WORD)
        ask='        '
        IF(idef.EQ.1)THEN

C A 'D' or 'd' detected, set to default value.
          call edisp(iuout,DSTR)
          IVAL=IDEFLT
          call usrmsg('  ','  ','-')
          RETURN
        ENDIF
        IF(WORD.EQ.' ')THEN

C A blank, carriage return or line feed encountered, assume user
C wishes to use prior/existing integer value.
          CALL INTSTR(IVAL,STR2,IW2,IER)
          WRITE(OUTSTR,55,iostat=ios,err=1)STR2(1:IW2)
          CALL USRMSG(' ',OUTSTR,'-')
          RETURN
        ENDIF
      ELSE

C In text mode, remind user of current value.
        write(OUTSTR,'(3a)')'(currently: ',STR2(1:IW2),')'
        call edisp(iuout,OUTSTR)
        CALL USRMSG(PROMP1,PROMP2,'?')
        READ(IUIN,24)WORD
   24   FORMAT(A10)
        A=WORD(1:1)
        IF(LNBLNK(WORD).EQ.1)THEN
          IF(A.EQ.'D'.OR.A.EQ.'d')THEN

C A 'D' or 'd' detected, set to default value.
            call edisp(iuout,DSTR)
            IVAL=IDEFLT
            RETURN
          ELSEIF(A.EQ.'H'.OR.A.EQ.'h'.OR.A.EQ.'?')THEN

C A probable request for help has been received. Print out the lines
C of help supplied and then provide the prompt again.
            IF(NHELP.GT.0)THEN
              CALL PHELPD('ask user integer',NHELP+1,'-',0,0,IER)
            ELSE
              CALL PHELPD('ask user integer',2,'-',0,0,IER)
            ENDIF
            GOTO 20
          ENDIF
        ELSEIF(WORD.EQ.' ')THEN

C A blank, carriage return or line feed encountered, assume user
C wishes to use prior/existing integer value.
          CALL INTSTR(IVAL,STR2,IW2,IER)
          WRITE(IUOUT,55,iostat=ios,err=1)STR2(1:IW2)
   55     FORMAT(' The existing value (',a,') will be used.')
          RETURN
        ENDIF
      ENDIF

C Decode the string into an integer.
      read(WORD,*,ERR=999)IVALT
      CALL INTSTR(IVALT,STR1,IW1,IFLAG)
      IF(IFLAG.NE.0)THEN
        CALL USRMSG(' ',' Did not understand, try again.','-')
        IFLAG=0
        GOTO 20
      ENDIF

C Check IVALT against minimum and respond based on MINACT.
      IF(IVALT.LT.MINV)THEN
        CALL INTSTR(MINV,STR2,IW2,IER)
        LN=max(1,LNBLNK(ERMSG))
        WRITE(OUTSTR,28,iostat=ios,err=1)ERMSG(1:LN),
     &    STR1(1:IW1),STR2(1:IW2)
   28   FORMAT(' The value of ',a,' (',a,') is less than ',a,'!')
        IF(MINACT.EQ.'W')THEN
          DOK=.true.
          CALL ASKOK(OUTSTR,' Is this OK?',OK,DOK,NHELP)
          IVAL=IVALT
          IF(.NOT.OK)GOTO 20
          IF(MMOD.EQ.8)call usrmsg('  ','  ','-')
          RETURN
        ELSEIF(MINACT.EQ.'F')THEN
          CALL USRMSG(OUTSTR,'Please re-enter.','W')
          GOTO 20
        ELSEIF(MINACT.EQ.'-')THEN
          IVAL=IVALT
          IF(MMOD.EQ.8)call usrmsg('  ','  ','-')
          RETURN
        ENDIF
      ELSEIF(IVALT.GT.MAXV)THEN
        CALL INTSTR(MAXV,STR2,IW2,IER)
        LN=max(1,LNBLNK(ERMSG))
        WRITE(OUTSTR,39,iostat=ios,err=1)ERMSG(1:LN),
     &    STR1(1:IW1),STR2(1:IW2)
   39   FORMAT(' The value of ',a,' (',a,')',' is greater than ',a,'!')
        IF(MAXACT.EQ.'W')THEN
          DOK=.true.
          CALL ASKOK(OUTSTR,' Is this ok?',OK,DOK,NHELP)
          IVAL=IVALT
          IF(.NOT.OK)GOTO 20
          IF(MMOD.EQ.8)call usrmsg('  ','  ','-')
          RETURN
        ELSEIF(MAXACT.EQ.'F')THEN
          CALL USRMSG(OUTSTR,'Please re-enter.','W')
          GOTO 20
        ELSEIF(MAXACT.EQ.'-')THEN
          IVAL=IVALT
          IF(MMOD.EQ.8)call usrmsg('  ','  ','-')
          RETURN
        ENDIF
      ELSE
        IVAL=IVALT
      ENDIF

C Return to calling module
      IF(MMOD.EQ.8)call usrmsg('  ','  ','-')
      RETURN

999   CALL USRMSG(' ',' Did not understand, try again.','-')
      GOTO 20
   1  write(6,*) 'EASKI: error composing message or prompt.'
      return

      END

C ******************** EASKR
C EASKR is a facility for asking the user for an real which
C incorporates the prompt, error messages and range checking
C as passed parameters as follows:
C      CALL EASKR(RVAL,PROMP1,PROMP2,RMIN,MINACT,
C     &           RMAX,MAXACT,DEFLT,ERMSG,IER,NHELP)

C RVAL is the real returned, PROMP1 & PROMP2 are the prompts to be
C given to the user (similar to the syntax of USRMSG which allows
C two lines or a leading blank line).
C RMIN is the minimum value, RMAX is the maximum value.
C MINACT and MAXACT are actions to take if the range is exceeded:
C   'W' means warn user but accept, 'F' means refuse/fail to
C   accept value and ask again, '-' means no range checking.
C DEFLT is the default value if 'D','d', or return typed.
C ERMSG is a string appended to the range checking or read error
C to identify the value. IER is the error state, if 0 then OK.
C NHELP is the number of help lines and H() is the array
C of text strings to be printed out if '?','H','h' is typed
C by the user.
C Example:
C    ....
C Read a site latitude.
C        XLAT=50.0
C        H(1)='The latitude is required for calculating the '//
C     &           'sun position, it should '
C        H(2)='be similar to that assumed for the climate file.'
C        CALL EASKR(XLAT,' ',' What is the site latitude (deg) ? ',
C     &             -89.9,'F',89.9,'F',35.0,'site latitude',IER,2)
C    ...

      SUBROUTINE EASKR(RVAL,PROMP1,PROMP2,RMIN,MINACT,
     &                 RMAX,MAXACT,DEFLT,ERMSG,IER,NHELP)

      COMMON/OUTIN/IUOUT,IUIN
      COMMON/SPAD/MMOD,LIMIT,LIMTTY
      common/pophelp/h(60)
      dimension dh(60)
      CHARACTER*(*) PROMP1,PROMP2,ERMSG
      CHARACTER MINACT*1,MAXACT*1,A*1,WORD*16,STR1*16,STR2*16,H*72
      CHARACTER ask*16,OUTSTR*124,DSTR*124,dh*72,outs*124
      LOGICAL OK,close,DOK

      IER=0

C If RVAL is outwith range for failure then set to default.
      if(MINACT.EQ.'F'.and.RVAL.LT.RMIN)RVAL=DEFLT
      if(MAXACT.EQ.'F'.and.RVAL.GT.RMAX)RVAL=DEFLT

C Check that value is not trivially close to zero (ie not instanciated
C on the fortran side before the call).
      CALL ECLOSE(RVAL,0.00,0.000001,CLOSE)
      if(close)RVAL=0.00
   20 WORD=' '
      ask='           '
      if(rval.gt.1.E+6)then
        WRITE(ask,'(1PE14.6)',IOSTAT=ios,ERR=999)RVAL
      elseif(rval.le.1.E+6.and.rval.gt.1.E+4)then
        WRITE(ask,'(G15.5)',IOSTAT=ios,ERR=999)RVAL
      elseif(rval.le.1.E+4.and.rval.gt.1.E-3)then
        WRITE(ask,'(G14.4)',IOSTAT=ios,ERR=999)RVAL
      elseif(rval.le.1.E-3.and.rval.gt.1.E-4)then
        WRITE(ask,'(F10.7)',IOSTAT=ios,ERR=999)RVAL
      elseif(rval.le.1.E-4)then
        WRITE(ask,'(1PE14.6)',IOSTAT=ios,ERR=999)RVAL
      else
        WRITE(ask,'(F14.4)',IOSTAT=ios,ERR=999)RVAL
      endif

C Generate help and default strings.
      CALL REL16STR(DEFLT,STR2,IW2,IER)
      WRITE(DSTR,27,iostat=ios,err=1)STR2(1:IW2)
   27 FORMAT(' The default value (',a,') will be used.')

      CALL REL16STR(DEFLT,STR1,IW1,IER)
      CALL REL16STR(RVAL,STR2,IW2,IER)
      IF(NHELP.GT.0)THEN
        WRITE(H(NHELP+1),63,iostat=ios,err=1)STR1(1:IW1),STR2(1:IW2)
   63   FORMAT('Type d D (default:',a,
     &         ') or return (current:',a,')')
      ELSE
        LN=max(1,LNBLNK(ERMSG))
        WRITE(H(1),64,iostat=ios,err=1)ERMSG(1:LN)
   64   FORMAT(' No help available for ',a)
        WRITE(H(2),65,iostat=ios,err=1)STR1(1:IW1),STR2(1:IW2)
   65   FORMAT('Type d D (default:',a,
     &         ') or return (current:',a,')')
      ENDIF

C Begin with prompts.
      IF(MMOD.EQ.8)THEN

C If in graphic mode use graphic faciltiies to edit the number.
        idef=0
        iquery=0
        iwidth=16
        CALL openaskbox(PROMP1,PROMP2,iwidth)
        WORD=' '
        if(NHELP.GT.0)then
          CALL PHELPW(NHELP+1,IHW,IER)
          call dupphelp(dh)
          call updhelp(dh,NHELP+1,IHW)
        else
          CALL PHELPW(2,IHW,IER)
          call dupphelp(dh)
          call updhelp(dh,2,IHW)
        endif
        CALL askdialog(ask,idef,iquery)

C Strip off the last character (end of line mark) as well as any
C leading blanks from string returned.
        call c2fstr(ask,WORD)
        IF(idef.EQ.1)THEN

C A 'D' or 'd' detected, set to default value.
          call edisp(iuout,DSTR)
          RVAL=DEFLT
          call usrmsg('  ','  ','-')
          RETURN
        ENDIF
        IF(WORD.EQ.' ')THEN

C A blank, carriage return or line feed encountered, assume user
C wishes to use prior/existing real value.
          CALL REL16STR(RVAL,STR2,IW2,IER)
          WRITE(OUTSTR,55,iostat=ios,err=1)STR2(1:IW2)
          CALL USRMSG(' ',OUTSTR,'-')
          RETURN
        ENDIF
      ELSE

C In text mode, remind user of current value.
        write(OUTSTR,'(3a)')'(currently: ',STR2(1:IW2),')'
        call edisp(iuout,OUTSTR)
        CALL USRMSG(PROMP1,PROMP2,'?')
        READ(IUIN,24)WORD
   24   FORMAT(A16)
        A=WORD(1:1)
        IF(LNBLNK(WORD).EQ.1)THEN
          IF(A.EQ.'D'.OR.A.EQ.'d')THEN

C A 'D' or 'd' detected, set to default value.
            call edisp(iuout,DSTR)
            RVAL=DEFLT
            RETURN
          ELSEIF(A.EQ.'H'.OR.A.EQ.'h'.OR.A.EQ.'?')THEN

C A probable request for help has been received. Print out the lines of
C help supplied and then provide the prompt again.
            IF(NHELP.GT.0)THEN
              CALL PHELPD('ask user real',NHELP+1,'-',0,0,IER)
            ELSE
              CALL PHELPD('ask user real',2,'-',0,0,IER)
            ENDIF
            GOTO 20
          endif
        ELSEIF(WORD.EQ.' ')THEN

C A carriage return or line feed encountered, assume user wishes to
C use prior/existing real value.
          CALL REL16STR(RVAL,STR2,IW2,IER)
          WRITE(IUOUT,55,iostat=ios,err=1)STR2(1:IW2)
   55     FORMAT(' The existing value (',a,') will be used.')
          RETURN
        ENDIF
      ENDIF

C Decode the string into an real.
      read(WORD,*,ERR=89)rvalt
      CALL REL16STR(RVALT,STR1,IW1,IER)
      IF(IER.NE.0)THEN
        CALL USRMSG(' ',' Did not understand, try again..','-')
        IER=0
        GOTO 20
      ENDIF

C Check RVALT against minimum and respond based on MINACT.
      IF(RVALT.LT.RMIN)THEN
        CALL REL16STR(RMIN,STR2,IW2,IER)
        LN=max(1,LNBLNK(ERMSG))
        WRITE(OUTSTR,28,iostat=ios,err=1)ERMSG(1:LN),
     &    STR1(1:IW1),STR2(1:IW2)
   28   FORMAT(' The value of ',a,' (',a,') is less than ',a,'!')
        IF(MINACT.EQ.'W')THEN
          DOK=.false.
          CALL ASKOK(OUTSTR,' Is this OK ? ',OK,DOK,NHELP)
          RVAL=RVALT
          IF(.NOT.OK)GOTO 20
          IF(MMOD.EQ.8)call usrmsg('  ','  ','-')
          RETURN
        ELSEIF(MINACT.EQ.'F')THEN
          CALL USRMSG(OUTSTR,'Please re-enter: ','W')
          GOTO 20
        ELSEIF(MINACT.EQ.'-')THEN
          RVAL=RVALT
          IF(MMOD.EQ.8)call usrmsg('  ','  ','-')
          RETURN
        ENDIF
      ELSEIF(RVALT.GT.RMAX)THEN
        CALL REL16STR(RMAX,STR2,IW2,IER)
        LN=max(1,LNBLNK(ERMSG))
        WRITE(OUTSTR,39,iostat=ios,err=1)ERMSG(1:LN),
     &    STR1(1:IW1),STR2(1:IW2)
   39   FORMAT(' The value of ',a,' (',a,')',' is greater than ',a,'!')
        IF(MAXACT.EQ.'W')THEN
          DOK=.false.
          CALL ASKOK(OUTSTR,' Is this ok? ',OK,DOK,NHELP)
          RVAL=RVALT
          IF(.NOT.OK)GOTO 20
          IF(MMOD.EQ.8)call usrmsg('  ','  ','-')
          RETURN
        ELSEIF(MAXACT.EQ.'F')THEN
          CALL USRMSG(OUTSTR,'Please re-enter: ','W')
          GOTO 20
        ELSEIF(MAXACT.EQ.'-')THEN
          RVAL=RVALT
          IF(MMOD.EQ.8)call usrmsg('  ','  ','-')
          RETURN
        ENDIF
      ELSE
        RVAL=RVALT
      ENDIF

C Return to calling module
      IF(MMOD.EQ.8)call usrmsg('  ','  ','-')
      RETURN
 89   CALL USRMSG(' ',' Did not understand, try again..','-')
      GOTO 20
   1  write(6,*) 'EASKR: error composing message or prompt.'
      return

 999  WRITE(outs,*) ' EASKR: invalid real or > 16 char: ',RVAL
      call edisp(iuout,outs)
      RETURN

      END

C ******************** EASKE
C EASKE is a facility for asking the user for an real number
C in exponential format which incorporates the prompt, error
C messages and range checking as passed parameters as follows:
C      CALL EASKE(RVAL,PROMP1,PROMP2,RMIN,MINACT,
C     &           RMAX,MAXACT,DEFLT,ERMSG,IER,NHELP)

C RVAL is the real returned, PROMP1 & PROMP2 are the prompts to be
C given to the user (similar to the syntax of USRMSG which allows
C two lines or a leading blank line).
C RMIN is the minimum value, RMAX is the maximum value.
C MINACT and MAXACT are actions to take if the range is exceeded:
C   'W' means warn user but accept, 'F' means refuse/fail to
C   accept value and ask again, '-' means no range checking.
C DEFLT is the default value if 'D','d', or return typed.
C ERMSG is a string appended to the range checking or read error
C to identify the value. IER is the error state, if 0 then OK.
C NHELP is the number of help lines and H() is the array
C of text strings to be printed out if '?','H','h' is typed
C by the user.

      SUBROUTINE EASKE(RVAL,PROMP1,PROMP2,RMIN,MINACT,
     &                 RMAX,MAXACT,DEFLT,ERMSG,IER,NHELP)

      COMMON/OUTIN/IUOUT,IUIN
      COMMON/SPAD/MMOD,LIMIT,LIMTTY
      common/pophelp/h(60)
      dimension dh(60)
      CHARACTER*(*) PROMP1,PROMP2,ERMSG
      CHARACTER MINACT*1,MAXACT*1,A*1,WORD*16,STR1*10,STR2*10,H*72
      CHARACTER ask*16,OUTSTR*124,DSTR*124,dh*72
      LOGICAL OK,close,DOK

      IER=0

C If RVAL is outwith range for failure then set to default.
      if(MINACT.EQ.'F'.and.RVAL.LT.RMIN)RVAL=DEFLT
      if(MAXACT.EQ.'F'.and.RVAL.GT.RMAX)RVAL=DEFLT

C Check that value is not trivially close to zero (ie not instanciated
C on the fortran side before the call).
      CALL ECLOSE(RVAL,0.00,0.000001,CLOSE)
      if(close)RVAL=0.00
   20 WORD=' '
      ask='           '
      WRITE(ask,'(1PE14.6)')RVAL

C Generate help and default strings.
      CALL EXPSTR(DEFLT,STR2,IW2,IER)
      WRITE(DSTR,27,iostat=ios,err=1)STR2(1:IW2)
   27 FORMAT(' The default value (',a,') will be used.')

      CALL EXPSTR(DEFLT,STR1,IW1,IER)
      CALL EXPSTR(RVAL,STR2,IW2,IER)
      IF(NHELP.GT.0)THEN
        WRITE(H(NHELP+1),63,iostat=ios,err=1)STR1(1:IW1),STR2(1:IW2)
   63   FORMAT('Type d D (default:',a,
     &         ') or return (current:',a,')')
      ELSE
        LN=max(1,LNBLNK(ERMSG))
        WRITE(H(1),64,iostat=ios,err=1)ERMSG(1:LN)
   64   FORMAT(' No help available for ',a)
        WRITE(H(2),65,iostat=ios,err=1)STR1(1:IW1),STR2(1:IW2)
   65   FORMAT('Type d D (default:',a,
     &         ') or return (current:',a,')')
      ENDIF

C Begin with prompts.
C If in terminal type 8 (dialogue box) use ww facilities to get the
C string to convert.
      IF(MMOD.EQ.8)THEN
        idef=0
        iquery=0
        iwidth=16
        CALL openaskbox(PROMP1,PROMP2,iwidth)
        WORD=' '
        if(NHELP.GT.0)then
          CALL PHELPW(NHELP+1,IHW,IER)
          call dupphelp(dh)
          call updhelp(dh,NHELP+1,IHW)
        else
          CALL PHELPW(2,IHW,IER)
          call dupphelp(dh)
          call updhelp(dh,2,IHW)
        endif
        CALL askdialog(ask,idef,iquery)

C Strip off the last character (end of line mark) as well as any
C leading blanks from string returned.
        call c2fstr(ask,WORD)
        IF(idef.EQ.1)THEN

C A 'D' or 'd' detected, set to default value.
          call edisp(iuout,DSTR)
          RVAL=DEFLT
          call usrmsg('  ','  ','-')
          RETURN
        ENDIF
        IF(WORD.EQ.' ')THEN

C A blank, carriage return or line feed encountered, assume user
C wishes to use prior/existing real value.
          CALL EXPSTR(RVAL,STR2,IW2,IER)
          WRITE(OUTSTR,55,iostat=ios,err=1)STR2(1:IW2)
          CALL USRMSG(' ',OUTSTR,'-')
          RETURN
        ENDIF
      ELSE

C In text mode, remind user of current value.
        write(OUTSTR,'(3a)')'(currently: ',STR2(1:IW2),')'
        call edisp(iuout,OUTSTR)
        CALL USRMSG(PROMP1,PROMP2,'?')
        READ(IUIN,24)WORD
   24   FORMAT(A16)
        A=WORD(1:1)
        IF(LNBLNK(WORD).EQ.1)THEN
          IF(A.EQ.'D'.OR.A.EQ.'d')THEN

C A 'D' or 'd' detected, set to default value.
            call edisp(iuout,DSTR)
            RVAL=DEFLT
            RETURN
          ELSEIF(A.EQ.'H'.OR.A.EQ.'h'.OR.A.EQ.'?')THEN

C A probable request for help has been received. Print out the lines of
C help supplied and then provide the prompt again.
            IF(NHELP.GT.0)THEN
              CALL PHELPD('ask user real',NHELP+1,'-',0,0,IER)
            ELSE
              CALL PHELPD('ask user real',2,'-',0,0,IER)
            ENDIF
            GOTO 20
          endif
        ELSEIF(WORD.EQ.' ')THEN

C A carriage return or line feed encountered, assume user wishes to
C use prior/existing real value.
          CALL EXPSTR(RVAL,STR2,IW2,IER)
          WRITE(IUOUT,55,iostat=ios,err=1)STR2(1:IW2)
   55     FORMAT(' The existing value (',a,') will be used.')
          RETURN
        ENDIF
      ENDIF

C Decode the string into an real.
      read(WORD,*,ERR=89)rvalt
      CALL EXPSTR(RVALT,STR1,IW1,IER)
      IF(IER.NE.0)THEN
        CALL USRMSG(' ',' Did not understand, try again..','-')
        IER=0
        GOTO 20
      ENDIF

C Check RVALT against minimum and respond based on MINACT.
      IF(RVALT.LT.RMIN)THEN
        CALL EXPSTR(RMIN,STR2,IW2,IER)
        LN=max(1,LNBLNK(ERMSG))
        WRITE(OUTSTR,28,iostat=ios,err=1)ERMSG(1:LN),
     &    STR1(1:IW1),STR2(1:IW2)
   28   FORMAT(' The value of ',a,' (',a,') is less than ',a,'!')
        IF(MINACT.EQ.'W')THEN
          DOK=.false.
          CALL ASKOK(OUTSTR,' Is this OK ? ',OK,DOK,NHELP)
          RVAL=RVALT
          IF(.NOT.OK)GOTO 20
          IF(MMOD.EQ.8)call usrmsg('  ','  ','-')
          RETURN
        ELSEIF(MINACT.EQ.'F')THEN
          CALL USRMSG(OUTSTR,'Please re-enter: ','W')
          GOTO 20
        ELSEIF(MINACT.EQ.'-')THEN
          RVAL=RVALT
          IF(MMOD.EQ.8)call usrmsg('  ','  ','-')
          RETURN
        ENDIF
      ELSEIF(RVALT.GT.RMAX)THEN
        CALL EXPSTR(RMAX,STR2,IW2,IER)
        LN=max(1,LNBLNK(ERMSG))
        WRITE(OUTSTR,39,iostat=ios,err=1)ERMSG(1:LN),
     &    STR1(1:IW1),STR2(1:IW2)
   39   FORMAT(' The value of ',a,' (',a,')',' is greater than ',a,'!')
        IF(MAXACT.EQ.'W')THEN
          DOK=.false.
          CALL ASKOK(OUTSTR,' Is this ok? ',OK,DOK,NHELP)
          RVAL=RVALT
          IF(.NOT.OK)GOTO 20
          IF(MMOD.EQ.8)call usrmsg('  ','  ','-')
          RETURN
        ELSEIF(MAXACT.EQ.'F')THEN
          CALL USRMSG(OUTSTR,'Please re-enter: ','W')
          GOTO 20
        ELSEIF(MAXACT.EQ.'-')THEN
          RVAL=RVALT
          IF(MMOD.EQ.8)call usrmsg('  ','  ','-')
          RETURN
        ENDIF
      ELSE
        RVAL=RVALT
      ENDIF

C Return to calling module
      IF(MMOD.EQ.8)call usrmsg('  ','  ','-')
      RETURN
 89   CALL USRMSG(' ',' Did not understand, try again..','-')
      GOTO 20
   1  write(6,*) 'EASKE: error composing message or prompt.'
      return

      END

C ************* EASKF
C EASKF is a facility for asking the user for a file name which
C incorporates the prompt, error messages and help facilities
C as follows:

C FILEN is the string returned, ISTRW is its length.
C PROMP1 & PROMP2 are the prompts using the same syntax as USRMSG.
C If a space is typed then FILEN is not changed.
C ERMSG is a string placed at the top of the selection menu and to
C any range checking or read errors to identify the value.
C  IER is the error state, if 0 then OK.
C NHELP is the number of help lines and H() is the array
C of text strings to be printed out if '?','H','h' is typed
C by the user.
C DSTR is a string to use as a default.
C << this is a place holder so the X version has similar functionality
C << to the GTK version. It should be extended to support browsing etc.

      SUBROUTINE EASKF(FILEN,PROMP1,PROMP2,ISTRW,DSTR,ERMSG,IER,NHELP)

      COMMON/SPAD/MMOD,LIMIT,LIMTTY
      COMMON/OUTIN/IUOUT,IUIN
      common/pophelp/h(60)

      dimension dh(60)
      CHARACTER*(*) PROMP1,PROMP2,ERMSG,FILEN,DSTR
      CHARACTER WORD144*144,H*72,A*1,LASTS*144,OUTSTR*124,dh*72
      LOGICAL OK,DOK

   20 IER=0

C Remember the file name in case user wants to abort.
      WORD144=' '
      last=max(1,lnblnk(FILEN))
      if(last.lt.144) then
        LASTS=FILEN(1:last)
      else
        LASTS=FILEN(1:144)
      endif
      if (last.gt.6) then
        if(FILEN(1:7).eq.'UNKNOWN'.or.FILEN(1:7).eq.'unknown'.or.
     &     FILEN(1:7).eq.'Unknown') FILEN='  '
      endif

C Generate custom help text and use local var for NHELP.
      NHL=NHELP
      IF(NHL.EQ.0)THEN
        LN=max(1,LNBLNK(ERMSG))
        WRITE(H(1),64,iostat=ios,err=1)ERMSG(1:LN)
   64   FORMAT('No help available for ',a,'.')
        NHL=1
      ENDIF

C At this point get user input via dialogue box or Fortran read in
C the text window.
C Note that the string read in is limited to ISTRW characters wide.
C If in terminal type 8 use dialogue box.
      IF(MMOD.EQ.8)THEN
        idef=0
        iquery=0
        CALL openaskbox(PROMP1,PROMP2,ISTRW)
        WORD144=' '
        if(NHELP.GT.0)then
          CALL PHELPW(NHELP,IHW,IER)
          call dupphelp(dh)
          call updhelp(dh,NHELP,IHW)
        else
          CALL PHELPW(1,IHW,IER)
          call dupphelp(dh)
          call updhelp(dh,1,IHW)
        endif
        lmmod=mmod
        CALL askdialog(FILEN,idef,iquery)

C PATCH to restore mmod (occassioanlly gets corrupted in linux)
        if(lmmod.ne.mmod)mmod=lmmod

C Strip off the last character (end of line mark) as well as any
C leading blanks from string returned.
        call c2fstr(FILEN,WORD144)

        IF(idef.EQ.1)THEN
          LN=max(1,LNBLNK(DSTR))
          WRITE(OUTSTR,'(3a)',iostat=ios,err=1)' The default is ',
     &      DSTR(1:LN),'.'
          DOK=.true.
          CALL ASKOK(OUTSTR,' Is this ok?',OK,DOK,NHL)
          IF(OK)THEN
            FILEN=DSTR(1:LN)
            call usrmsg('  ','  ','-')
            RETURN
          ELSE
            GOTO 20
          ENDIF
        ENDIF
        IF(WORD144.EQ.' ')THEN

C A blank, carriage return or line feed encountered, assume user
C wishes to use prior/existing string.
          IF(LASTS(1:2).NE.'  ')THEN
            WRITE(IUOUT,55,iostat=ios,err=1)LASTS(1:last)
            if(last.lt.144)then
              write(FILEN,'(a)',iostat=ios,err=3) LASTS(1:last)
            else
              write(FILEN,'(a)',iostat=ios,err=3) LASTS(1:144)
            endif
            RETURN
          ELSE
            CALL USRMSG(' The current file name is blank!',
     &                  ' Please re-enter.','W')
            GOTO 20
          ENDIF
        ENDIF

C Must have input a character string.
        write(FILEN,'(a)',iostat=ios,err=3) WORD144(1:ISTRW)
        call usrmsg('  ','  ','-')
        RETURN
      ELSE

C In text mode, remind user of current string.
        if(FILEN(1:2).eq.'  ')then
          call edisp(iuout,'(currently blank)')
        else
          write(OUTSTR,'(3a)',iostat=ios,err=2)'(currently: ',
     &     FILEN(1:last),')'
          call edisp(iuout,OUTSTR)
        endif
        CALL USRMSG(PROMP1,PROMP2,'?')
        READ(IUIN,'(A144)')WORD144
        A=WORD144(1:1)
        IF(lnblnk(WORD144).EQ.1)THEN
          IF(A.EQ.'D'.OR.A.EQ.'d')THEN

C A 'D' or 'd' detected, set to default.
            LN=max(1,LNBLNK(DSTR))
            WRITE(OUTSTR,'(3a)',iostat=ios,err=1)' The default is ',
     &        DSTR(1:LN),'.'
            DOK=.true.
            CALL ASKOK(OUTSTR,' Is this ok?',OK,DOK,NHL)
            IF(OK)THEN
              FILEN=DSTR(1:LN)
              RETURN
            ELSE
              write(FILEN,'(a)',iostat=ios,err=3) LASTS(1:last)
              GOTO 20
            ENDIF
          ELSEIF(A.EQ.'H'.OR.A.EQ.'h'.OR.A.EQ.'?')THEN

C A probable request for help has been received. Print out the lines
C of help supplied and then provide the prompt again.
            CALL PHELPD('ask file name',NHL,'-',0,0,IER)
            GOTO 20
          ELSE

C Neither help no default, could be a single character string was
C requested.
            write(FILEN,'(a)',iostat=ios,err=3) WORD144(1:ISTRW)
            RETURN
          ENDIF
        ELSEIF(WORD144(1:2).EQ.'  ')THEN

C A blank, carriage return or line feed encountered, assume user
C wishes to use prior/existing file name.
          IF(LASTS(1:2).NE.'  '.or.last.ge.1)then
            WRITE(IUOUT,55,iostat=ios,err=1)LASTS(1:last)
   55       FORMAT(' The name (',a,') will be used.')
            write(FILEN,'(a)',iostat=ios,err=3) LASTS(1:last)
            RETURN
          ELSE
            CALL USRMSG(' The current file name is blank!',
     &                  ' Please re-enter.','W')
            GOTO 20
          ENDIF
        ELSE

C Must have input a character string.
          write(FILEN,'(a)',iostat=ios,err=3) WORD144(1:ISTRW)
          RETURN
        ENDIF
      ENDIF

      RETURN
   1  write(6,*) 'EASKF: error composing message or prompt.'
      return
   2  write(6,*) 'EASKF: error composing prompt.'
      return
   3  write(6,*) 'EASKF: error writing file name.'
      return
      END

C ************* EASKS
C EASKS is a facility for asking the user for a text string which
C incorporates the prompt, error messages and help facilities
C as follows:

C STRVAL is the string returned, ISTRW is its length.
C PROMP1 & PROMP2 are the prompts using the same syntax as USRMSG.
C If a space is typed then STRVAL is not changed.
C ERMSG is a string placed at the top of the selection menu and to
C any range checking or read errors to identify the value.
C  IER is the error state, if 0 then OK.
C NHELP is the number of help lines and H() is the array
C of text strings to be printed out if '?','H','h' is typed
C by the user.
C DSTR is a string to use as a default.

C Example:
C    ....
C test getting a string.
C        H(1)='The site name is a test of the menu help facilities.'
C        XNAME=' '
C        DNAME=' '
C        CALL EASKS(XNAME,' ',' What is the site name ?',
C     &   13,DNAME,'test sites',IER,1)
C        CALL USRMSG(' ',' selected string is:'//XNAME,'-')

      SUBROUTINE EASKS(STRVAL,PROMP1,PROMP2,ISTRW,DSTR,ERMSG,IER,NHELP)

      COMMON/SPAD/MMOD,LIMIT,LIMTTY
      COMMON/OUTIN/IUOUT,IUIN
      common/pophelp/h(60)

      dimension dh(60)
      CHARACTER*(*) PROMP1,PROMP2,ERMSG,STRVAL,DSTR
      CHARACTER WORD*92,H*72,A*1,LASTS*72,OUTSTR*124,dh*72
      LOGICAL OK,DOK

   20 IER=0
      WORD=' '
      last=max(1,lnblnk(STRVAL))
      LASTS=STRVAL(1:last)
      if (last.gt.6) then
        if(STRVAL(1:7).eq.'UNKNOWN'.or.STRVAL(1:7).eq.'unknown'.or.
     &     STRVAL(1:7).eq.'Unknown') STRVAL='  '
      endif

C Generate custom help text and use local var for NHELP.
      NHL=NHELP
      IF(NHL.EQ.0)THEN
        LN=max(1,LNBLNK(ERMSG))
        WRITE(H(1),64,iostat=ios,err=1)ERMSG(1:LN)
   64   FORMAT('No help available for ',a,'.')
        NHL=1
      ENDIF

C At this point get user input via dialogue box or Fortran read in
C the text window.
C Note that the string read in is limited to ISTRW characters wide.
C If in terminal type 8 use dialogue box.
      IF(MMOD.EQ.8)THEN
        idef=0
        iquery=0
        CALL openaskbox(PROMP1,PROMP2,ISTRW)
        WORD=' '
        if(NHELP.GT.0)then
          CALL PHELPW(NHELP,IHW,IER)
          call dupphelp(dh)
          call updhelp(dh,NHELP,IHW)
        else
          CALL PHELPW(1,IHW,IER)
          call dupphelp(dh)
          call updhelp(dh,1,IHW)
        endif
        lmmod=mmod
        CALL askdialog(STRVAL,idef,iquery)

C PATCH to restore mmod (occassioanlly gets corrupted in linux)
        if(lmmod.ne.mmod)mmod=lmmod

C Strip off the last character (end of line mark) as well as any
C leading blanks from string returned. It is assumed that STRVAL
C passed into the subroutine will fit within a 92 character WORD buffer.
        call c2fstr(STRVAL,WORD)

        IF(idef.EQ.1)THEN
          LN=max(1,LNBLNK(DSTR))
          WRITE(OUTSTR,'(3a)',iostat=ios,err=1)' The default is ',
     &      DSTR(1:LN),'.'
          DOK=.true.
          CALL ASKOK(OUTSTR,' Is this ok?',OK,DOK,NHL)
          IF(OK)THEN
            STRVAL=DSTR(1:LN)
            call usrmsg('  ','  ','-')
            RETURN
          ELSE
            GOTO 20
          ENDIF
        ENDIF
        IF(WORD.EQ.' ')THEN

C A blank, carriage return or line feed encountered, assume user
C wishes to use prior/existing string.
          IF(LASTS.NE.' ')THEN
            WRITE(IUOUT,55,iostat=ios,err=1)LASTS(1:last)
            STRVAL=LASTS(1:last)
            call usrmsg('  ','  ','-')
            RETURN
          ELSE
            CALL USRMSG(' The current string is blank!',
     &                  ' Please re-enter.','W')
            GOTO 20
          ENDIF
        ENDIF

C Must have input a character string.
        STRVAL=WORD(1:ISTRW)
        call usrmsg('  ','  ','-')
        RETURN
      ELSE

C In text mode, remind user of current string.
        if(STRVAL(1:2).eq.'  ')then
          call edisp(iuout,'(currently blank)')
        else
          write(OUTSTR,'(3a)',iostat=ios,err=2)'(currently: ',
     &     STRVAL(1:last),')'
          call edisp(iuout,OUTSTR)
        endif
        CALL USRMSG(PROMP1,PROMP2,'?')
        READ(IUIN,'(A)')WORD
        A=WORD(1:1)
        IF(lnblnk(WORD).EQ.1)THEN
          IF(A.EQ.'D'.OR.A.EQ.'d')THEN

C A 'D' or 'd' detected, set to default.
            LN=max(1,LNBLNK(DSTR))
            WRITE(OUTSTR,'(3a)',iostat=ios,err=1)' The default is ',
     &        DSTR(1:LN),'.'
            DOK=.true.
            CALL ASKOK(OUTSTR,' Is this ok?',OK,DOK,NHL)
            IF(OK)THEN
              STRVAL=DSTR(1:LN)
              RETURN
            ELSE
              STRVAL=LASTS(1:last)
              GOTO 20
            ENDIF
          ELSEIF(A.EQ.'H'.OR.A.EQ.'h'.OR.A.EQ.'?')THEN

C A probable request for help has been received. Print out the lines
C of help supplied and then provide the prompt again.
            CALL PHELPD('ask user string',NHL,'-',0,0,IER)
            GOTO 20
          ELSE

C Neither help no default, could be a single character string was
C requested.
            STRVAL=WORD(1:ISTRW)
            RETURN
          ENDIF
        ELSEIF(WORD(1:2).EQ.'  ')THEN

C A blank, carriage return or line feed encountered, assume user
C wishes to use prior/existing string.
          IF(LASTS(1:2).NE.'  '.or.last.ge.1)then
            WRITE(IUOUT,55,iostat=ios,err=1)LASTS(1:last)
   55       FORMAT(' The existing string (',a,') will be used.')
            STRVAL=LASTS(1:last)
            RETURN
          ELSE
            CALL USRMSG(' The current string is blank!',
     &                  ' Please re-enter.','W')
            GOTO 20
          ENDIF
        ELSE

C Must have input a character string.
          STRVAL=WORD(1:ISTRW)
          RETURN
        ENDIF
      ENDIF

      RETURN
   1  write(6,*) 'EASKS: error composing message or prompt.'
      return
   2  write(6,*) 'EASKS: error composing prompt.'
      return
      END

C ************* EASKS248
C EASKS248 edit a long (248 char) string in sections.

C STRVAL is the string returned, ISTRW is its length.
C PROMP1 & PROMP2 are the prompts using the same syntax as USRMSG.
C If a space is typed then STRVAL is not changed.
C ERMSG is a string placed at the top of the selection menu and to
C any range checking or read errors to identify the value.
C  IER is the error state, if 0 then OK.
C NHELP is the number of help lines and H() is the array
C of text strings to be printed out if '?','H','h' is typed
C by the user.
C DSTR is a string to use as a default.
      SUBROUTINE EASKS248(STRVAL,PROMP1,PROMP2,ISTRW,DSTR,ERMSG,IER,
     &   NHELP)

      COMMON/OUTIN/IUOUT,IUIN
      common/pophelp/h(60)
      CHARACTER*(*) PROMP1,PROMP2,ERMSG,STRVAL,DSTR
      CHARACTER H*72,t72*72,t72a*72,t72b*72,t72c*72
      LOGICAL next

C Find breakpoints near 72 144 216 and 248 characters.
C If STRVAL null make into a blank string.
      ipall=lnblnk(STRVAL)
      if(ipall.le.1) STRVAL='  '
      ip72=iprevblnk(STRVAL,72)
      ip144=iprevblnk(STRVAL,ip72+71)
      ip216=iprevblnk(STRVAL,ip144+71)
      ip248=iprevblnk(STRVAL,248)

C Create working strings for each section of text. Either as blanks
C in the case of a short initial string or in lengths of ~72 char.
C Remember the lengths in lt72 lt72a etc. in case user edits only
C a portion of the text block.
      t72=' '
      t72a=' '
      t72b=' '
      t72c=' '
      write(t72,'(a)') STRVAL(1:ip72)
      lt72=max(1,lnblnk(t72))
      if(ip144.gt.ip72)write(t72a,'(a)') STRVAL(ip72+1:ip144)
      lt72a=max(1,lnblnk(t72a))
      if(ip216.gt.ip144)write(t72b,'(a)') STRVAL(ip144+1:ip216)
      lt72b=max(1,lnblnk(t72b))
      if(ip248.gt.ip216)write(t72c,'(a)') STRVAL(ip216+1:ip248)
      lt72c=max(1,lnblnk(t72c))

      call edisp(iuout,'Current...')
      call edisp248(iuout,STRVAL,72)
      call edisp(iuout,' ')

C Logic supports stepwise scrolling within the text. 243 label
C is the point for editing the initial portion of the text. 244 label
C is the point for editing 2nd tranche of text.
 243  CALL EASKSCMD(t72,PROMP1,PROMP2,'>',next,ISTRW,DSTR,
     &  'synp 1',IER,NHELP)

C lt72 is the number of characters found after editing. If this is
C less than 72 then cut characters from ipvsynop.
      lt72=max(1,lnblnk(t72))
      if(next)then

C Edit second section.
        CALL EASKS2CMD(t72a,PROMP1,PROMP2,'|<','>',
     &    inext,ISTRW,DSTR,ERMSG,IER,NHELP)
        lt72a=max(1,lnblnk(t72a))
        if(inext.eq.1)then

C If text to left requested go back to start.
          goto 243
        elseif(inext.eq.2)then

C If text to right requested, load 3rd portion and offer option to
C jump to earlier or last section of text.
          CALL EASKS2CMD(t72b,PROMP1,PROMP2,'|<','>|',
     &      inext,ISTRW,DSTR,ERMSG,IER,NHELP)
          lt72b=max(1,lnblnk(t72b))
          if(inext.eq.1)then

C Go back to start.
            next=.true.
            goto 243
          elseif(inext.eq.2)then

C Work with last portion of string, offer option to jump back
C to the start.
            CALL EASKSCMD(t72c,PROMP1,PROMP2,'|<..',next,ISTRW,
     &        DSTR,ERMSG,IER,NHELP)
            lt72c=max(1,lnblnk(t72c))
            if(next)then
              goto 243
            endif
          endif
        endif
      endif

C When writing out the combined text, put a space between each section.
      call usrmsg(' ',' ','-')
      itwid=lt72+lt72a+lt72b+lt72c+3
      if(itwid.le.248)then
        write(STRVAL,'(7a)') t72(1:lt72),' ',t72a(1:lt72a),' ',
     &    t72b(1:lt72b),' ',t72c(1:lt72c)
      else
        itwid=248-(lt72+lt72a+lt72b+3)
        write(STRVAL,'(7a)') t72(1:lt72),' ',t72a(1:lt72a),' ',
     &    t72b(1:lt72b),' ',t72c(1:itwid)
      endif
      call edisp(iuout,'Revised...')
      call edisp248(iuout,STRVAL,72)
      call edisp(iuout,' ')
      return
      end

C ************* EASKCMD
C EASKSCMD is a facility for asking the user for a text string which
C incorporates the prompt, alternative command, error messages and
C help facilities as follows:

C STRVAL is the string returned, ISTRW is its length.
C PROMP1 & PROMP2 are the prompts using the same syntax as USRMSG.
C CMD is an alternative command string, CMDACT is logical if the
C command action is selected.
C If a space is typed then STRVAL is not changed.
C ERMSG is a string placed at the top of the selection menu and to
C any range checking or read errors to identify the value.
C  IER is the error state, if 0 then OK.
C NHELP is the number of help lines and H() is the array
C of text strings to be printed out if '?','H','h' is typed
C by the user.
C DSTR is a string to use as a default.

C Example:
C    ....
C test getting a string.
C        H(1)='The site name is a test of the menu help facilities.'
C        XNAME=' '
C        DNAME=' '
C        CALL EASKSCMD(XNAME,' ',' What is the site name ?','Use map',CACT,
C     &   13,DNAME,'test sites',IER,1)
C        CALL USRMSG(' ',' selected string is:'//XNAME,'-')

      SUBROUTINE EASKSCMD(STRVAL,PROMP1,PROMP2,CMD,CMDACT,ISTRW,DSTR,
     &  ERMSG,IER,NHELP)

      COMMON/OUTIN/IUOUT,IUIN
      common/pophelp/h(60)
      COMMON/SPAD/MMOD,LIMIT,LIMTTY

      dimension dh(60)
      CHARACTER*(*) PROMP1,PROMP2,CMD,ERMSG,STRVAL,DSTR
      CHARACTER WORD*72,H*72,A*1,LASTS*72,OUTSTR*124,dh*72
      LOGICAL OK,CMDACT,DOK

   20 IER=0
      CMDACT=.FALSE.
      WORD=' '
      last=max(1,lnblnk(STRVAL))
      LASTS=STRVAL(1:last)

C Generate custom help text.
      IF(NHELP.EQ.0)THEN
        LN=max(1,LNBLNK(ERMSG))
        WRITE(H(1),64)ERMSG(1:LN)
   64   FORMAT('No help available for ',a,'.')
      ENDIF

C At this point get user input via dialogue box or Fortran read in
C the text window.
C Note that the string read in is limited to ISTRW characters wide.
C If in terminal type 8 use dialogue box.
      IF(MMOD.EQ.8)THEN
        idef=0
        iquery=0
        CALL openaskaltbox(PROMP1,PROMP2,CMD,ISTRW)
        WORD=' '
        if(NHELP.GT.0)then
          CALL PHELPW(NHELP,IHW,IER)
          call dupphelp(dh)
          call updhelp(dh,NHELP,IHW)
        else
          CALL PHELPW(1,IHW,IER)
          call dupphelp(dh)
          call updhelp(dh,1,IHW)
        endif
        CALL askaltdialog(STRVAL,CMD,idef,iquery)

C Strip off the last character (end of line mark) as well as any
C leading blanks from string returned.
        call c2fstr(STRVAL,WORD)

        IF(idef.EQ.1)THEN
          LN=max(1,LNBLNK(DSTR))
          WRITE(OUTSTR,'(3a)')' The default is ',
     &      DSTR(1:LN),'.'
          DOK=.true.
          CALL ASKOK(OUTSTR,' Is this ok?',OK,DOK,NHELP)
          IF(OK)THEN
            STRVAL=DSTR(1:LN)
            RETURN
          ELSE
            GOTO 20
          ENDIF
        ENDIF
        IF(idef.EQ.2)THEN
          IF(WORD.EQ.' ')THEN

C A blank, carriage return or line feed encountered, assume user
C wishes to use prior/existing string.
            IF(LASTS(1:2).NE.'  ')THEN
              WRITE(IUOUT,55)LASTS(1:last)
              STRVAL=LASTS(1:last)
              CMDACT=.TRUE.
              RETURN
            ENDIF
          ENDIF

C Must have input a character string.
          STRVAL=WORD(1:ISTRW)
          CMDACT=.TRUE.
          RETURN
        endif
        IF(WORD.EQ.' ')THEN

C A blank, carriage return or line feed encountered, assume user
C wishes to use prior/existing string.
          IF(LASTS(1:2).NE.'  ')THEN
            WRITE(IUOUT,55)LASTS(1:last)
            STRVAL=LASTS(1:last)
            CMDACT=.FALSE.
            RETURN
          ELSE
            CALL USRMSG(' The current string is blank!',
     &                  ' Please re-enter.','W')
            GOTO 20
          ENDIF
        ENDIF

C Must have input a character string.
        STRVAL=WORD(1:ISTRW)
        CMDACT=.FALSE.
        RETURN
      ELSE
        CALL USRMSG(PROMP1,PROMP2,'?')
        READ(IUIN,'(A72)')WORD
        A=WORD(1:1)
        IF(lnblnk(WORD).EQ.1)THEN
          IF(A.EQ.'D'.OR.A.EQ.'d')THEN

C A 'D' or 'd' detected, set to default.
            LN=max(1,LNBLNK(DSTR))
            WRITE(OUTSTR,'(3a)')' The default is ',
     &        DSTR(1:LN),'.'
            DOK=.true.
            CALL ASKOK(OUTSTR,' Is this ok?',OK,DOK,NHELP)
            IF(OK)THEN
              STRVAL=DSTR(1:LN)
              RETURN
            ELSE
              STRVAL=LASTS(1:last)
              GOTO 20
            ENDIF
          ELSEIF(A.EQ.'H'.OR.A.EQ.'h'.OR.A.EQ.'?')THEN

C A probable request for help has been received. Print out the lines
C of help supplied and then provide the prompt again.
            IF(NHELP.GT.0)THEN
              CALL PHELPD('ask user string',NHELP,'-',0,0,IER)
            ELSE
              CALL PHELPD('ask user string',1,'-',0,0,IER)
            ENDIF
            GOTO 20
          ELSE

C Neither help no default, could be a single character string was
C requested.
            STRVAL=WORD(1:ISTRW)
            RETURN
          ENDIF
        ELSEIF(WORD.EQ.' ')THEN

C A blank, carriage return or line feed encountered, assume user
C wishes to use prior/existing string.
          IF(LASTS(1:2).NE.'  ')THEN
            WRITE(IUOUT,55)LASTS(1:last)
   55       FORMAT(' The existing string (',a,') will be used.')
            STRVAL=LASTS(1:last)
            RETURN
          ELSE
            CALL USRMSG(' The current string is blank!',
     &                  ' Please re-enter.','W')
            GOTO 20
          ENDIF
        ELSE

C Must have input a character string.
          STRVAL=WORD(1:ISTRW)
          RETURN
        ENDIF
      ENDIF
      RETURN
      END

C ************* EASKS2CMD
C EASKS2CMD is a facility for asking the user for a text string which
C incorporates the prompt, two alternative commands, error messages and
C help facilities as follows:

C STRVAL is the string returned, ISTRW is its length.
C PROMP1 & PROMP2 are the prompts using the same syntax as USRMSG.
C CMD & CND2 are alternative command strings, if ICACT is non-zero
C a command action is selected.
C If a space is typed then STRVAL is not changed.
C ERMSG is a string placed at the top of the selection menu and to
C any range checking or read errors to identify the value.
C  IER is the error state, if 0 then OK.
C NHELP is the number of help lines and H() is the array
C of text strings to be printed out if '?','H','h' is typed
C by the user.
C DSTR is a string to use as a default.

C Example:
C    ....
C test getting a string.
C        H(1)='The site name is a test of the menu help facilities.'
C        XNAME=' '
C        DNAME=' '
C        CALL EASKS2CMD(XNAME,' ','Site name ?','Use map','Use iwec',ICACT,
C     &   13,DNAME,'test sites',IER,1)
C        CALL USRMSG(' ',' selected string is:'//XNAME,'-')

      SUBROUTINE EASKS2CMD(STRVAL,PROMP1,PROMP2,CMD,CMD2,ICACT,ISTRW,
     &  DSTR,ERMSG,IER,NHELP)

      COMMON/OUTIN/IUOUT,IUIN
      common/pophelp/h(60)
      COMMON/SPAD/MMOD,LIMIT,LIMTTY

      dimension dh(60)
      CHARACTER*(*) PROMP1,PROMP2,CMD,CMD2,ERMSG,STRVAL,DSTR
      CHARACTER WORD*72,H*72,A*1,LASTS*72,OUTSTR*124,dh*72
      LOGICAL OK,DOK

   20 IER=0
      ICACT=0
      WORD=' '
      last=max(1,lnblnk(STRVAL))
      LASTS=STRVAL(1:last)

C Generate custom help text.
      IF(NHELP.EQ.0)THEN
        LN=max(1,LNBLNK(ERMSG))
        WRITE(H(1),64)ERMSG(1:LN)
   64   FORMAT('No help available for ',a,'.')
      ENDIF

C At this point get user input via dialogue box or Fortran read in
C the text window.
C Note that the string read in is limited to ISTRW characters wide.
C If in terminal type 8 use dialogue box.
      IF(MMOD.EQ.8)THEN
        idef=0
        iquery=0
        CALL openask2altbox(PROMP1,PROMP2,CMD,CMD2,ISTRW)
        WORD=' '
        if(NHELP.GT.0)then
          CALL PHELPW(NHELP,IHW,IER)
          call dupphelp(dh)
          call updhelp(dh,NHELP,IHW)
        else
          CALL PHELPW(1,IHW,IER)
          call dupphelp(dh)
          call updhelp(dh,1,IHW)
        endif
        CALL ask2altdialog(STRVAL,CMD,CMD2,idef,iquery)

C Debug.
C        write(6,*) 'just after ask2altdialog idef iquery',idef,iquery

C Strip off the last character (end of line mark) as well as any
C leading blanks from string returned.
        call c2fstr(STRVAL,WORD)

        IF(idef.EQ.1)THEN
          LN=max(1,LNBLNK(DSTR))
          WRITE(OUTSTR,'(3a)')' The default is ',
     &      DSTR(1:LN),'.'
          DOK=.true.
          CALL ASKOK(OUTSTR,' Is this ok?',OK,DOK,NHELP)
          IF(OK)THEN
            STRVAL=DSTR(1:LN)
            RETURN
          ELSE
            GOTO 20
          ENDIF
        ENDIF
        IF(idef.EQ.2)THEN
          IF(WORD.EQ.' ')THEN

C A blank, carriage return or line feed encountered, assume user
C wishes to use prior/existing string.
            IF(LASTS(1:2).NE.'  ')THEN
              WRITE(IUOUT,55)LASTS(1:last)
              STRVAL=LASTS(1:last)
              ICACT=1
              RETURN
            ENDIF
          ENDIF

C Must have input a character string.
          STRVAL=WORD(1:ISTRW)
          ICACT=1
          RETURN
        endif
        IF(idef.EQ.3)THEN
          IF(WORD.EQ.' ')THEN

C A blank, carriage return or line feed encountered, assume user
C wishes to use prior/existing string.
            IF(LASTS(1:2).NE.'  ')THEN
              WRITE(IUOUT,55)LASTS(1:last)
              STRVAL=LASTS(1:last)
              ICACT=2
              RETURN
            ENDIF
          ENDIF

C Must have input a character string.
          STRVAL=WORD(1:ISTRW)
          ICACT=2
          RETURN
        endif
        IF(WORD.EQ.' ')THEN

C A blank, carriage return or line feed encountered, assume user
C wishes to use prior/existing string.
          IF(LASTS(1:2).NE.'  ')THEN
            WRITE(IUOUT,55)LASTS(1:last)
            STRVAL=LASTS(1:last)
            ICACT=0
            RETURN
          ELSE
            CALL USRMSG(' The current string is blank!',
     &                  ' Please re-enter.','W')
            GOTO 20
          ENDIF
        ENDIF

C Must have input a character string.
        STRVAL=WORD(1:ISTRW)
        ICACT=0
        RETURN
      ELSE
        CALL USRMSG(PROMP1,PROMP2,'?')
        READ(IUIN,'(A72)')WORD
        A=WORD(1:1)
        IF(lnblnk(WORD).EQ.1)THEN
          IF(A.EQ.'D'.OR.A.EQ.'d')THEN

C A 'D' or 'd' detected, set to default.
            LN=max(1,LNBLNK(DSTR))
            WRITE(OUTSTR,'(3a)')' The default is ',
     &        DSTR(1:LN),'.'
            DOK=.true.
            CALL ASKOK(OUTSTR,' Is this ok?',OK,DOK,NHELP)
            IF(OK)THEN
              STRVAL=DSTR(1:LN)
              RETURN
            ELSE
              STRVAL=LASTS(1:last)
              GOTO 20
            ENDIF
          ELSEIF(A.EQ.'H'.OR.A.EQ.'h'.OR.A.EQ.'?')THEN

C A probable request for help has been received. Print out the lines
C of help supplied and then provide the prompt again.
            IF(NHELP.GT.0)THEN
              CALL PHELPD('ask user string',NHELP,'-',0,0,IER)
            ELSE
              CALL PHELPD('ask user string',1,'-',0,0,IER)
            ENDIF
            GOTO 20
          ELSE

C Neither help no default, could be a single character string was
C requested.
            STRVAL=WORD(1:ISTRW)
            RETURN
          ENDIF
        ELSEIF(WORD.EQ.' ')THEN

C A blank, carriage return or line feed encountered, assume user
C wishes to use prior/existing string.
          IF(LASTS(1:2).NE.'  ')THEN
            WRITE(IUOUT,55)LASTS(1:last)
   55       FORMAT(' The existing string (',a,') will be used.')
            STRVAL=LASTS(1:last)
            RETURN
          ELSE
            CALL USRMSG(' The current string is blank!',
     &                  ' Please re-enter.','W')
            GOTO 20
          ENDIF
        ELSE

C Must have input a character string.
          STRVAL=WORD(1:ISTRW)
          RETURN
        ENDIF
      ENDIF
      RETURN
      END

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
C backslashit: given `string' swap / to \ : return as `name'.
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


C ************* EPICKS
c EPICKS is a facility allowing a number of selections to be made
C from an array of strings passed into the routine. EPICKS
C incorporates the prompt, error messages and returns an array of
C selected indexes as follows:

C PROMP1 & PROMP2 are the prompts using the same syntax as USRMSG.
C INPICK is passed as the number of items which are allowed to
C   be selected. If = NSTALT then the prompt will include the
C   phrase '* ALL' otherwise if INPICK < NSTALT then the prompt
C   will be in the form '* pick 2 items'. On return INPICK becomes
C   the actual number of items selected ( if 0 then none).
C IVALS is an array of NSTALT size such that:
C   IVALS(1) is the first index selected,
C   IVALS(2) is the second index ...
C   IVALS(INPICK) is the last index chosen.
C NSTALT is the number of selection strings STALT passed.
C ERMSG is a string appended to the range checking or read error
C to identify the value. IER is the error state, if 0 then OK.
C NHELP is the number of help lines and H() is the array
C of text strings to be printed out if '?','H','h' is typed
C by the user.

C Note that there is no particular limit on the number of alternate
C strings which can be passed to the subroutine as the selection menu
C allows paging.
C User-defined text strings and string alternatives
C will be truncated at the width ISTRW.

      SUBROUTINE EPICKS(INPICK,IVALS,PROMP1,PROMP2,
     &                ISTRW,NSTALT,STALT,ERMSG,IER,NHELP)

C For page control of the menus.
      integer CREATE,PREV,NEXT,EDIT
      PARAMETER (CREATE=1,PREV=3,NEXT=6,EDIT=20)
      common/pophelp/h(60)
      COMMON/SPAD/MMOD,LIMIT,LIMTTY

      COMMON/PMENU/MHEAD,MCTL,MIFULL,MFULL,IST,ILEN,IPM,MPM,IPFLG
      LOGICAL FOUND,ALL

      DIMENSION IVALS(NSTALT)
      dimension lista(35),listb(35)
      character lista*74,listb*74,KEY*1,outs*124
      CHARACTER*(*) PROMP1,PROMP2,ERMSG,STALT(*)
      CHARACTER H*72,SEL*40

C At this point bring up a menu with the string alternatives, assuming
C there is more than one alternative to pick from.
      IF(NSTALT.LT.1)THEN
        CALL USRMSG(PROMP1,' Number of menu items too small.','W')
        IER=1
        RETURN
      ENDIF

C Initialise menu size variables based on window size.
C IVERT is the menu position, MVERT the current number of menu lines.
C If paged menu include another control line (blank for readability).
      CALL USRMSG(PROMP1,PROMP2,'-')
      ALL=.FALSE.
      MHEAD=0
      MCTL=5
      ILEN=NSTALT
      IPACT=CREATE
      CALL EKPAGE(IPACT)
      IALLOW=INPICK

C Clear IVALS and INPICK.
      IER=0
      INPICK=0
      DO 40 I=1,NSTALT
        IVALS(I)=0
   40 CONTINUE

C Initial menu entry setup.
   92 IER=0
      IVERT=-3

C Generate text for title.
    3 lne=max(1,LNBLNK(ERMSG))
      IF(lne.GT.ISTRW)WRITE(SEL,'(1X,A)')ERMSG(1:ISTRW)
      IF(lne.LE.ISTRW)WRITE(SEL,'(1X,A)')ERMSG(1:lne)

C Loop through the items until the page to be displayed. M is the
C current menu line index. Build up text strings for the menu.
      M=MHEAD
      DO 10 L=1,ILEN
        IF(L.GE.IST.AND.(L.LE.(IST+MIFULL)))THEN
          M=M+1
          CALL EMKEY(L,KEY,IER)
          LISTA(M)=' '
          WRITE(LISTA(M),'(A1,1X,A)')KEY,STALT(L)(1:ISTRW)
          do 20 K=1,INPICK
            if (IVALS(K).eq.L) then
              WRITE(LISTA(M),'(A1,1X,A,A)')KEY,STALT(L)(1:ISTRW),' *'
            endif
 20       continue
        ENDIF
   10 CONTINUE

C If multi page menu and *ALL or Limit message to be included
C insert an additional blank line.
      IF(IPFLG.NE.0.AND.IALLOW.GT.1)then
        M=M+1
        LISTA(M)=' '
      endif

C Number of actual items displayed.
      MVERT=M+MCTL

C Adjust prompt for All items or just as a message warning user how
C many items can be selected.
      IF(IALLOW.EQ.NSTALT)THEN

C Include 'ALL' as the final item.
        IF(ISTRW.LE.8)THEN
          LISTA(M+1) ='* All  '
        ELSEIF(ISTRW.GT.8.AND.ISTRW.LE.18)THEN
          LISTA(M+1) ='* All items  '
        ELSEIF(ISTRW.GT.18)THEN
          LISTA(M+1) ='* All items in list    '
        ENDIF
        ALL=.TRUE.
      ELSEIF(IALLOW.EQ.1)THEN

C If only a single pick write nothing here.
        LISTA(M+1) ='                                               '
        ALL=.FALSE.
      ELSEIF(IALLOW.GT.1.AND.IALLOW.LT.NSTALT)THEN
        if(IALLOW.le.99)then
          IF(ISTRW.GT.8.AND.ISTRW.LE.16)THEN
            WRITE(LISTA(M+1),'(A,I2,A)')' (Lmt:',IALLOW,')'
          ELSEIF(ISTRW.GT.16)THEN
            WRITE(LISTA(M+1),'(A,I2,A)')' (Limit:',IALLOW,' items)'
          ENDIF
        elseif(IALLOW.gt.99)then
          IF(ISTRW.GT.8.AND.ISTRW.LE.16)THEN
            WRITE(LISTA(M+1),'(A,I3,A)')' (Lmt:',IALLOW,')'
          ELSEIF(ISTRW.GT.16)THEN
            WRITE(LISTA(M+1),'(A,I3,A)')' (Limit:',IALLOW,' items)'
          ENDIF
        endif
        ALL=.FALSE.
      ENDIF

C If a long list include page facility text.
      IF(IPFLG.EQ.0)THEN
        LISTA(M+2)='  _____________________________________________ '
      ELSE
        IF(ISTRW.LE.8)THEN
          LISTA(M+2)='0 Page '
        ELSEIF(ISTRW.GT.8.AND.ISTRW.LE.16)THEN
          WRITE(LISTA(M+2),114)IPM,MPM
  114     FORMAT   ('0 Page: ',I2,':',I2)
        ELSEIF(ISTRW.GT.16.AND.ISTRW.LE.28)THEN
          WRITE(LISTA(M+2),115)IPM,MPM
  115     FORMAT   ('0 Page Part: ',I2,' of ',I2)
        ELSEIF(ISTRW.GT.28)THEN
          WRITE(LISTA(M+2),116)IPM,MPM
  116     FORMAT   ('0 Page --- Part: ',I2,' of ',I2,' ---')
        ENDIF
      ENDIF
      if(MMOD.EQ.8)then
        LISTA(M+3)='                                                '
      else
        LISTA(M+3)='< Index select                                  '
      endif
      LISTA(M+4)  ='? Help                                          '
      LISTA(M+5)  ='- Exit                                          '

C Now display the menu (depending on the width of the strings).
      IF(MMOD.EQ.8)THEN
        call VWMENU(SEL,LISTA,MVERT,0,0,ISTRW+4,irpx,irpy,ivert)
      ELSE
        DO 144 IJ=1,MVERT
          if((ISTRW+4).LT.74)then
            write(listb(IJ),'(A)')lista(IJ)(1:ISTRW+4)
          else
            write(listb(IJ),'(A)')lista(IJ)(1:74)
          endif
  144   CONTINUE
        CALL EMENU(':',listb,MVERT,IVERT)
      ENDIF

      IF(IVERT.LE.MHEAD)THEN

C Within the header so skip request.
        IVERT=-1
        goto 3
      ELSEIF(IVERT.EQ.MVERT)THEN
        RETURN
      ELSEIF(IVERT.EQ.(MVERT-1))THEN

C Produce help text for the menu.
        IF(NHELP.GT.0.AND.NHELP.LT.60)then
          WRITE(H(NHELP+1),65)
          CALL PHELPD('user pick string',NHELP+1,'-',0,0,IER)
        ELSEIF(NHELP.EQ.60)then
          CALL PHELPD('user pick string',NHELP,'-',0,0,IER)
        ELSEIF(NHELP.EQ.0)then
          WRITE(H(1),'(A,A)')' No help available for ',ERMSG
          WRITE(H(2),65)
          CALL PHELPD('user pick string',2,'-',0,0,IER)
        ENDIF
  65    format(' Type key to select, repeat as',
     &         ' required. -Exit to return marked list.')
      ELSEIF(IVERT.EQ.(MVERT-2))THEN
        if(MMOD.EQ.8)then
          IVERT=-1
          goto 92
        endif

C Script directed input if not in graphic mode.
        H(1)='Use this for script mode - if one item required then'
        H(2)='type `1`.  The index of `c` is `3`.'
        INPICK=1
  93    CALL EASKI(INPICK,' ',' No of items to pick?',
     &     1,'F',IALLOW,'F',1,'script no of items',IER,2)
        if(IER.NE.0)goto 93
        DO 94 I=1,INPICK
  95      write(outs,'(A,I2)') ' Index (number) of item ',I
          CALL EASKI(IV,outs,' ',1,'F',NSTALT,'F',I,'script itm',IER,3)
          if(IER.NE.0)goto 95
          IVALS(I)=IV
  94    CONTINUE
        call usrmsg(' ',' ','-')
        RETURN
      ELSEIF(IVERT.EQ.(MVERT-3))THEN

C If there are enough items allow paging control via EKPAGE.
        IF(IPFLG.EQ.1)THEN
          IPACT=EDIT
          CALL EKPAGE(IPACT)
        ENDIF
      ELSEIF(IVERT.EQ.(MVERT-4))THEN

C User selected all items, so process and return if ALL is true,
C otherwise this is a dummy pick.
        IF(ALL)THEN
          DO 42 I=1,NSTALT
            IVALS(I)=I
  42      CONTINUE
          INPICK=NSTALT
        ENDIF
      ELSEIF(IVERT.GT.MHEAD.AND.IVERT.LT.(MVERT-MCTL+1))THEN

C Look through previous selections and see if IFOC is unique, if
C so update IVALS and loop back for another.
        CALL KEYIND(MVERT,IVERT,IFOC,IO)
        FOUND=.FALSE.
        IF(INPICK.GT.0)THEN
          DO 44 J=1,INPICK
            IF(IVALS(J).EQ.IFOC.or.FOUND) then
              FOUND=.TRUE.
              if (J+1.gt.NSTALT) then
                IVALS(J)=0
              else
                IVALS(J)=IVALS(J+1)
              endif
            endif
  44      CONTINUE
          IF(.NOT.FOUND)THEN
            if (INPICK.lt.IALLOW) then
              INPICK=INPICK+1
              IVALS(INPICK)=IFOC
            endif
          ELSE
            INPICK=INPICK-1
          ENDIF
        ELSEIF(INPICK.EQ.0)THEN
          INPICK=1
          IVALS(INPICK)=IFOC
        ENDIF
      ELSE

C Not one of the legal menu choices.
        IVERT=-1
        goto 92
      ENDIF
      IVERT=-2
      goto 3

      END

C **************** MENUATOL
C MENUATOL presents a list of up to a dozen items to select via a
C menu format but with the text for each selection passed (as in
C easkatog). It is assumed that the user will make one selection
C only and if nothing selected it returns an index of zero.
C
C It is passed a prompt, menu title, menu width (mw characters),
C default index (idindex) and current index (index) if zero then
C assumes no current selection. If user exits without selection then
C index is returned as zero if index was passed in as zero and
C is reset to index value if non-zero was passed.
C Automatic key characters are suppressed and are assumed to be
C passed within the parameter list text.
      SUBROUTINE MENUATOL(prompt,title,AOPT,BOPT,COPT,DOPT,EOPT,FOPT,
     &  GOPT,HOPT,IOPT,JOPT,KOPT,LOPT,index,idindex,nhelp)

      COMMON/SPAD/MMOD,LIMIT,LIMTTY
      common/pophelp/h(60)
      LOGICAL SELECT
      CHARACTER*(*) prompt,title,AOPT,BOPT,COPT,DOPT,EOPT,FOPT,GOPT
      CHARACTER*(*) HOPT,IOPT,JOPT,KOPT,LOPT

      DIMENSION VERT(16)
      CHARACTER VERT*48,KEY*1,prompt2*36,h*72

C Generate default help text, clear local menu string and remember
C the incomming index.
      if(nhelp.eq.0)then
        write(H(1),64,iostat=ios,err=1)
  64    format('No help available for this selection list.')
      endif
      DO 9 L=1,16
        VERT(L)='  '
   9  CONTINUE
      lindex=index

C See how many items there are to present.
      nopt=12
      lt=max(1,LNBLNK(title))
      la=max(1,LNBLNK(AOPT))
      lb=max(1,LNBLNK(BOPT))
      lc=max(1,LNBLNK(COPT))
      ld=max(1,LNBLNK(DOPT))
      le=max(1,LNBLNK(EOPT))
      lf=max(1,LNBLNK(FOPT))
      lg=max(1,LNBLNK(GOPT))
      lh=max(1,LNBLNK(HOPT))
      li=max(1,LNBLNK(IOPT))
      lj=max(1,LNBLNK(JOPT))
      lk=max(1,LNBLNK(KOPT))
      ll=max(1,LNBLNK(LOPT))
      if(ll.le.1)then
        nopt=11
        VERT(12)='  '
      else
        WRITE(VERT(12),'(a)')LOPT(1:ll)
      endif
      if(lk.le.1)then
        nopt=10
        VERT(11)='  '
      else
        WRITE(VERT(11),'(a)')KOPT(1:lk)
      endif
      if(lj.le.1)then
        nopt=9
        VERT(10)='  '
      else
        WRITE(VERT(10),'(a)')JOPT(1:lj)
      endif
      if(li.le.1)then
        nopt=8
        VERT(9)='  '
      else
        WRITE(VERT(9),'(a)')IOPT(1:li)
      endif
      if(lh.le.1)then
        nopt=7
        VERT(8)='  '
      else
        WRITE(VERT(8),'(a)')HOPT(1:lh)
      endif
      if(lg.le.1)then
        nopt=6
        VERT(7)='  '
      else
        WRITE(VERT(7),'(a)')GOPT(1:lg)
      endif
      if(lf.le.1)then
        nopt=5
        VERT(6)='  '
      else
        WRITE(VERT(6),'(a)')FOPT(1:lf)
      endif
      if(le.le.1)then
        nopt=4
        VERT(5)='  '
      else
        WRITE(VERT(5),'(a)')EOPT(1:le)
      endif
      if(ld.le.1)then
        nopt=3
        VERT(4)='  '
      else
        WRITE(VERT(4),'(a)')DOPT(1:ld)
      endif
      if(lc.le.1)then
        nopt=2
        VERT(3)='  '
      else
        WRITE(VERT(3),'(a)')COPT(1:lc)
      endif
      WRITE(VERT(2),'(a)')BOPT(1:lb)
      WRITE(VERT(1),'(a)')AOPT(1:la)

C Find maximum with of items and title.
      ISTRW=MAX0(15,la,lb,lc,ld,le,lf,lg,lh,li,lj,lk,ll,lt)

C IVERT is the menu position, MVERT the current number of menu lines.
      SELECT=.FALSE.
      MCTL=4

C Initial menu entry setup.
   92 IER=0
      IVERT=-3

C Build up text strings for the control portion of the menu.
C Number of actual items displayed.
      MVERT=nopt+MCTL

      VERT(nopt+1)=  '  __________________________'
      if(ISTRW.lt.25)VERT(nopt+1)=  '  ______________________'
      if(ISTRW.lt.21)VERT(nopt+1)=  '  __________________'
      if(ISTRW.lt.17)VERT(nopt+1)=  '  ______________'
      if(idindex.ne.0)then
        CALL EMKEY(idindex,KEY,IER)
        write(VERT(nopt+2),'(a,a)') '* Deflt is item ',KEY
        if(ISTRW.lt.17)write(VERT(nopt+2),'(a,a)') '* Deflt is ',KEY
      else
        VERT(nopt+2)='                        '
      endif
      VERT(nopt+3)  ='? help                  '
      VERT(nopt+4)  ='- exit this menu        '

      if(index.le.0)then
        write(prompt2,'(a)') '  '
      else
        CALL EMKEY(index,KEY,IER)
        write(prompt2,'(3a)') '(',KEY,' is suggested)'
      endif

C Display the menu, if in graphic mode pass display width.
      call usrmsg(PROMPT,PROMPT2,'-')
      if(MMOD.EQ.8)then
        call VWMENU(title,VERT,MVERT,0,0,ISTRW+2,irpx,irpy,ivert)
      else
        CALL EMENU(title,VERT,MVERT,IVERT)
      endif
      IF(IVERT.EQ.MVERT)THEN

C If no selection has been made, if lindex=0 then return zero
C otherwise reset index to lindex. before exit then display error message.
        if(.NOT.SELECT)then
          if(lindex.eq.0)then
            index = 0
            return
          else
            index = lindex
            return
          endif
        endif
        return
      ELSEIF(IVERT.EQ.(MVERT-1))THEN

C Produce help text (defined prior to subroutine call).
        CALL PHELPD(title,nhelp,'-',0,0,IER)
      ELSEIF(IVERT.EQ.(MVERT-2).and.idindex.ne.0)THEN

C Take default item.
        index=idindex
        RETURN
      ELSEIF(IVERT.GE.1.AND.IVERT.LT.(MVERT-MCTL+1))THEN
        SELECT=.TRUE.
        index = IVERT
        RETURN
      else

C Not one of the legal menu choices.
        IVERT=-1
        goto 92
      endif
      IVERT=-2
      goto 92

   1  write(6,*) 'MENUATOL: error composing message or prompt.'
      return

      END

C ******************** ASKOK
C ASKOK Generic choice facility returning logical variable
C from a yes no default prompt (supports help).
C In the case of a graphic menu the messages will appear in a
C dialogue box at the bottom of the graphic window.
C If NHELP=-1 there is no default option.
      SUBROUTINE ASKOK(MSG1,MSG2,OK,DOK,NHELP)

      COMMON/SPAD/MMOD,LIMIT,LIMTTY
      COMMON/OUTIN/IUOUT,IUIN
      common/pophelp/h(60)
      CHARACTER*(*) MSG1,MSG2
      CHARACTER ANS*2,MSG3*124,H*72
      logical ok,dok,DEFLT

C Check if a default defined and use local var for NHELP.
      DEFLT=.true.
      NHL=NHELP
      if (NHL.eq.-1) then
        DEFLT=.false.
        NHL=0
      endif

C Generate custom help text.
      if(NHL.eq.0)then
        H(1)='Sorry, no help available for this dialog.'
        NHL=1
      endif

C Print out message according to the combination of strings passed. Use
C the dialogue box if terminal type 8.
      IF(MMOD.LT.8)THEN

C Setup a single line menu to pick from via EASKS.
   21   LN=max(1,LNBLNK(MSG2))
        if (DEFLT) then
          if(DOK)then
            WRITE(MSG3,'(2a)')MSG2(1:LN),' [Y]es [N]o [D]efault (yes) ?'
          else
            WRITE(MSG3,'(2a)')MSG2(1:LN),' [Y]es [N]o [D]efault (no) ?'
          endif
        else
          WRITE(MSG3,'(2a)')MSG2(1:LN),' [Y]es [N]o ?'
        endif
        LN1=max(1,lnblnk(MSG1))
        LN3=max(1,lnblnk(MSG3))
        ans = '  '
        CALL ELINC(1)
        WRITE(IUOUT,'(a)')MSG1(1:LN1)
        WRITE(IUOUT,'(a)')MSG3(1:LN3)

C Querry yes or no.
        CALL EPROMPT
        READ(IUIN,'(A2)',IOSTAT=IOS,ERR=1)ANS
        IF(ANS(1:1).EQ.'Y'.OR.ANS(1:1).EQ.'y')THEN
          OK=.TRUE.
        ELSEIF(ANS(1:1).EQ.'N'.OR.ANS(1:1).EQ.'n')THEN
          OK=.FALSE.
        ELSEIF(DEFLT.and.(ANS(1:1).EQ.'D'.OR.ANS(1:1).EQ.'d'))THEN
          OK=DOK
        ELSE
          CALL USRMSG(' ',' You must make a choice!','-')
          GOTO 21
        ENDIF
      ELSEIF(MMOD.EQ.8)THEN

C Querry yes or no or default.
        if (DEFLT) then
          if(DOK)then
            call EASKATOG(MSG1,MSG2,'yes','no','default (yes)',' ',' ',
     &      ' ',' ',IW,NHL)
          else
            call EASKATOG(MSG1,MSG2,'yes','no','default (no)',' ',' ',
     &      ' ',' ',IW,NHL)
          endif
        else
          call EASKATOG(MSG1,MSG2,'yes','no',' ',' ',' ',
     &      ' ',' ',IW,NHL)
        endif
        IF(IW.eq.1)THEN
          OK=.TRUE.
        ELSEIF(IW.eq.2)THEN
          OK=.FALSE.
        ELSEIF(DEFLT.and.IW.eq.3)THEN
          OK=DOK
        ENDIF
        call usrmsg('  ','  ','-')
      ENDIF
      RETURN

   1  write(6,*) 'ASKOK: error in dialog '
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

C ******************** EASKATOG
C EASKABCD Generic choice of 3 - 7 items returning 1 to 7 according to
C which of the choices has been chosen.
C MSG1 and MSG2 are prompts (upto 124 char) to be printed.
C AOPT, BOPT & COPT, DOPT, EOPT, FOPT, GOPT are the text strings describing
C the options available, (these should not be too long).  If all optons
C are filled with text then all presneted.  If and option (beginning
C with FOPT and working backwards is a single blank space then fewer
C options are presented.
C In the case of a graphic menu the c function abcdefbox will be called
C and the messages will appear in a dialogue box at the bottom of the
C graphic window.
      SUBROUTINE EASKATOG(MSG1,MSG2,AOPT,BOPT,COPT,DOPT,EOPT,FOPT,GOPT,
     &  IWHICH,NHELP)

      COMMON/OUTIN/IUOUT,IUIN
      common/pophelp/h(60)
      COMMON/SPAD/MMOD,LIMIT,LIMTTY
      CHARACTER*(*) MSG1,MSG2,AOPT,BOPT,COPT,DOPT,EOPT,FOPT,GOPT
      CHARACTER ANS*2,MSG3*124,H*72

C Generate custom help text and copy NHELP to local variable.
      NHL=NHELP
      if(NHL.eq.0)then
        H(1)='Sorry, no help available for this dialog.'
        NHL=1
      endif

C See how many items there are to present.
      nopt=7
      la=max(1,LNBLNK(AOPT))
      lb=max(1,LNBLNK(BOPT))
      lc=max(1,LNBLNK(COPT))
      ld=max(1,LNBLNK(DOPT))
      le=max(1,LNBLNK(EOPT))
      lf=max(1,LNBLNK(FOPT))
      lg=max(1,LNBLNK(GOPT))
      LN2=max(1,LNBLNK(MSG2))
      if(lg.le.1)nopt=6
      if(lf.le.1)nopt=5
      if(le.le.1)nopt=4
      if(ld.le.1)nopt=3
      if(lc.le.1)nopt=2

C Print out message according to the combination of strings passed.
C Setup a single line menu to pick from via EASKS.
      IF(MMOD.LT.8)THEN
   21   if(nopt.eq.2)then
          WRITE(MSG3,'(7a)')' ',MSG2(1:LN2),
     &    '  a) ',AOPT(1:la),', b) ',BOPT(1:lb),' ? '
        elseif(nopt.eq.3)then
          WRITE(MSG3,'(9a)')' ',MSG2(1:LN2),
     &    '  a) ',AOPT(1:la),', b) ',BOPT(1:lb),
     &    ', c) ',COPT(1:lc),' ? '
        elseif(nopt.eq.4)then
          WRITE(MSG3,'(11a)')' ',MSG2(1:LN2),
     &    '  a) ',AOPT(1:la),', b) ',BOPT(1:lb),
     &    ', c) ',COPT(1:lc),', e) ',DOPT(1:ld),' ? '
        elseif(nopt.eq.5)then
          WRITE(MSG3,'(13a)')' ',MSG2(1:LN2),
     &    '  a) ',AOPT(1:la),', b) ',BOPT(1:lb),', c) ',COPT(1:lc),
     &    ', e) ',DOPT(1:ld),', f) ',EOPT(1:le),' ? '
        elseif(nopt.eq.6)then
          WRITE(MSG3,'(15a)')' ',MSG2(1:LN2),
     &    ' a) ',AOPT(1:la),' b) ',BOPT(1:lb),' c) ',COPT(1:lc),
     &    ' e) ',DOPT(1:ld),' f) ',EOPT(1:le),' g) ',FOPT(1:lf),' ? '
        elseif(nopt.eq.7)then
          WRITE(MSG3,'(17a)')' ',MSG2(1:LN2),
     &    ' a) ',AOPT(1:la),' b) ',BOPT(1:lb),' c) ',COPT(1:lc),
     &    ' e) ',DOPT(1:ld),' f) ',EOPT(1:le),' g) ',FOPT(1:lf),
     &    ' i) ',GOPT(1:lg),' ? '
        endif
        ans = '  '
        call usrmsg(msg1,msg3,'?')
        read(iuin,'(A2)')ANS

        IF(ANS(1:1).EQ.'a'.OR.ANS(1:1).EQ.'A')THEN
          IWHICH=1
        ELSEIF(ANS(1:1).EQ.'b'.OR.ANS(1:1).EQ.'B')THEN
          IWHICH=2
        ELSEIF(ANS(1:1).EQ.'c'.OR.ANS(1:1).EQ.'C')THEN
          IWHICH=3
        ELSEIF(ANS(1:1).EQ.'e'.OR.ANS(1:1).EQ.'E')THEN
          IWHICH=4
        ELSEIF(ANS(1:1).EQ.'f'.OR.ANS(1:1).EQ.'F')THEN
          IWHICH=5
        ELSEIF(ANS(1:1).EQ.'g'.OR.ANS(1:1).EQ.'G')THEN
          IWHICH=6
        ELSEIF(ANS(1:1).EQ.'i'.OR.ANS(1:1).EQ.'I')THEN
          IWHICH=7
        ELSEIF(ANS(1:1).EQ.'h'.OR.ANS(1:1).EQ.'H'.or.
     &         ANS(1:1).eq.'?')THEN

C A probable request for help has been received. Print out the lines
C of help supplied and then provide the prompt again.
           CALL PHELPD('two choice dialog',NHL,'-',0,0,IER)
           GOTO 21
        elseif(ANS(1:2).eq.'  ')then
          call usrmsg(' The current answer is blank!',
     &                ' Please re-enter.','W')
          goto 21
        ELSE
          CALL USRMSG(' ',' You must make a choice!','-')
          GOTO 21
        ENDIF
      ELSEIF(MMOD.EQ.8)THEN

C Querry which one.
   19   CALL abcdefbox(MSG1,MSG2,AOPT,BOPT,COPT,DOPT,EOPT,FOPT,GOPT,IW)
        IF(IW.EQ.8)THEN

C A probable request for help has been received. Print out the lines
C of help supplied and then provide the prompt again.
          CALL PHELPD('this multi-choice',NHL,'-',0,0,IER)
          GOTO 19
        ENDIF
        IWHICH=IW
      ENDIF
      RETURN

   1  write(6,*) 'EASKatog: error composing message or prompt.'
      return
      END

C ************* EASKPER
C EASKPER: Provides legacy interface to the more general eAskPerGen below.
      SUBROUTINE eAskPer(PROMP1,IBDOY,IEDOY,IFDAY,IER)
      CHARACTER*(*) PROMP1
      integer ibdoy,iedoy,ifday,iBYr,iEYr,ier

      logical bMY_enabled
      parameter ( bMY_enabled = .false. )

C Note: There's no need to initialize iBYr and iEYr. They're not
C referenced when multiyear simulations are disabled

      call eAskPerGen(PROMP1,IBDOY,IEDOY,
     &                 iBYr,iEYr,IFDAY,bMY_enabled,IER)

      return
      end


C ************* EAskPerYear
C EASKPER: Provides multi-year capable interface to the more general
C eAskPerGen below.
      SUBROUTINE eAskPerYear(PROMP1,IBDOY,IEDOY,iBYr,iEYr,IFDAY,IER)
      CHARACTER*(*) PROMP1
      integer ibdoy,iedoy,ifday,iBYr,iEYr,ier

      logical bMY_enabled
      parameter ( bMY_enabled = .true. )

C initial values of iBYr iEYr are not need for single-year simulation
      call eAskPerGen(PROMP1,IBDOY,IEDOY,iBYr,iEYr,IFDAY,bMY_enabled,
     &                IER)

      return
      end


C ************* eAskPerGen
C eAskPerGen: Provides interface to specification of a multiyear period of days. It
C returns IBDOY and IEDOY based on the current settings of IFDAY.
C iBYr,iEYr are the begining year and ending year of the period.
C Promp1 gives the context of the request for a period.
      SUBROUTINE eAskPerGen(PROMP1,IBDOY,IEDOY,iBYr,iEYr,IFDAY,
     &                      bMY_enabled,IER)
      common/pophelp/h
      CHARACTER*(*) PROMP1
      CHARACTER*72 h(60)
      character HOLD*16

C Error flag
      logical bDateOK

C Flag indicating if multi-year simulations are in use
      logical bMY_enabled

      IER=0

      bDateOk = .false.

      do while ( .not. bDateOK )

C Initialize success flag, and character buffer index.
        bDateOK = .true.
        k=0

C IFDAY describes how the date will be formatted:
C   IFDAY = 0 -> DDD (1->365)
C   IFDAY = 1 -> DD MM (1->31,1->12)

        IF(IFDAY.EQ.0)THEN
C Day of year to be provided

          if ( .not. bMY_enabled ) then
C Single year version
            H(1)='Period  must be within a calendar year.'

C Get start & end day
            write(HOLD,'(I4,I4)')IBDOY,IEDOY
            CALL EASKS(HOLD,PROMP1,' Start and end day of year?',16,
     &      ' 1  1 ','start & end day of year',IER,1)

            CALL EGETWI(HOLD,K,IBDOY,1,365,'F','start doy',IER)
            if ( ier .ne. 0 ) bDateOK = .false.

            CALL EGETWI(HOLD,K,IEDOY,1,365,'F','end doy',IER)
            if ( ier .ne. 0 ) bDateOK = .false.


          else
C Multi-year version
            H(1)='Provide day-of-year and year for start'
            H(2)='and end dates.'

C Get start day and year
            write(HOLD,'(I4,I5)') IBDOY,iBYr
            CALL EASKS(HOLD,PROMP1,' Start day and year?',16,
     &      ' 1  2000 ','start day and year',IER,2)

            CALL EGETWI(HOLD,K,IBDOY,1,365,'F','start doy',IER)
            if  ( ier .ne. 0 ) BDateok = .false.

            CALL EGETWI(HOLD,K,iBYr,1900,2100,'F','start year',IER)
            if  ( ier .ne. 0 ) BDateok = .false.

C Reset string index K
            K=0

C Get end day and year
            write(HOLD,'(I4,I5)') IEDOY,iEYr
            CALL EASKS(HOLD,PROMP1,' End day and year?',16,
     &      ' 365 2000 ','end day and year',IER,2)

            CALL EGETWI(HOLD,K,IEDOY,1,365,'F','end doy',IER)
            if  ( ier .ne. 0 ) BDateok = .false.

            CALL EGETWI(HOLD,K,iEYr,1900,2100,'F','end year',IER)
            if  ( ier .ne. 0 ) BDateok = .false.

          endif !<- matches if ( .not. bMY_enabled )...

        ELSEIF ( IFDAY .eq. 1 .or. IFDAY .eq. 2 ) THEN

C Day & month or day, month & year to be provided
C Start date: Convert to dd mm format:

          CALL EDAYR(IBDOY,IBD,IBM)

          if ( .not. bMY_enabled ) then
C Get day & month
            write(HOLD,'(I2,A,I2)') IBD,' ',IBM
            H(1)='Period must be within a calendar year.'
            H(2)='( `1 1` to `31 12` )'
            CALL EASKS(HOLD,PROMP1,' Start day & month (DD MM)?',
     &        16,' 1  1 ','start day and month',ier,2)

          else
C Get day, month & year
C Append year to string
            write (HOLD, '(I2,A,I2,A,I4)') IBD,' ',IBM,' ',iBYr

            H(1)='Specify simulation start day, month and year'
            H(2)='in `DD MM YYYY` format. Note: Leap years are'
            H(3)='not presently supported.'
            CALL EASKS(HOLD,PROMP1,
     &        ' Start day, month & year (DD MM YYYY)?',
     &        16,' 1 1 2000','start day, month and year',IER,3)
          endif

C Error handling.
          if ( ier .ne. 0 ) bDateOK = .false.


C Convert stings to integers and flag errors
          K=0
          CALL EGETWI(HOLD,K,IBD,1,31,'F','start dom',ier)
          if ( ier .ne. 0 ) bDateOK = .false.

          CALL EGETWI(HOLD,K,IBM,1,12,'F','start month',ier)
          if ( ier .ne. 0 ) bDateOK = .false.

C Convert year string, if necessary
          if ( bMY_enabled ) then

            CALL EGETWI ( HOLD, K, iBYr, 1900, 2100, 'F',
     &        'start year', ier)

            if ( ier .ne. 0 ) bDateOK = .false.

          endif

C Check range, then convert to IBDOY.
          if ( bDateOK ) CALL EDAYCH(IBD,IBM,ier)

          if ( ier .eq. 1 ) bDateOK = .false.

          if ( bDateOK ) CALL EDAY(IBD,IBM,IBDOY)

C Only ask for end date if start-date successfully defined
          if ( bDateOK ) then

C End date, set at least equal to the beginning day.
            if ( .not. bMY_enabled .and. IEDOY.le.IBDOY ) then
              IEBODY = IBDOY + 1
            elseif ( bMY_enabled ) then

C Convert end-date into EDAYR friendly format and
C check that end date is not before start-date.
              iEDOY = iEDoy - ( iEYr - iBYr ) * 365
              if ( ( iEYr .eq. iBYr .and. IEDOY .le. IBDOY ) .or.
     &             ( iEYr .lt. iBYr ) ) then
                IEDOY=IBDOY+1
                iEyr=iBYr
                if (iEDOY .gt. 365 ) then
                  iEDOY = 1
                  iEYr = iEyr + 1
                endif
              endif
            endif ! <- matches ' elseif (bMY_enabled)...

C Convert end-day to day-month format
            CALL EDAYR(IEDOY,IED,IEM)

            if ( .not. bMY_enabled ) then
              write(HOLD,'(I2,A,I2)') IED,' ',IEM
              H(1)='Period  must be within a calendar year.'
              H(2)='( `1 1` to `31 12` )'
              CALL EASKS(HOLD,PROMP1,' End day & month (DD MM)?',
     &          16,' 1  1 ','start day and month',ier,2)

            else

C Get day, month & year  and append year to string.
              write (HOLD, '(I2,A,I2,A,I4)') IED,' ',IEM,' ',iEYr
              H(1)='Specify simulation start day, month and year'
              H(2)='in `DD MM YYYY` format. Note: Leap years are'
              H(3)='not presently supported.'

              CALL EASKS(HOLD,PROMP1,
     &          ' End day, month & year (DD MM YYYY)?',
     &          16,' 31 12 2000','end day, month and year',IER,3)

            endif

            if ( ier .ne. 0 ) bDateOK = .false.

C Convert strings to integers and errortrap
            K=0
            CALL EGETWI(HOLD,K,IED,1,31,'F','end dom',IER)
            if ( ier .ne. 0 ) bDateOK = .false.

            CALL EGETWI(HOLD,K,IEM,1,12,'F','end month',IER)
            if ( ier .ne. 0 ) bDateOK = .false.

C Convert year string, if necessary
            if ( bMY_enabled ) then

              CALL EGETWI ( HOLD, K, iEYr, 1900, 2100, 'F',
     &          'end year', ier)

              if ( ier .ne. 0 ) bDateOK = .false.

            endif

C Check range, then convert to IEDOY.
            if ( bDateOK )  CALL EDAYCH(IED,IEM, ier )

            if ( ier .ne. 0 ) bDateOK = .false.

            if ( bDateOK ) CALL EDAY(IED,IEM,IEDOY)

C For multi-year simulations, convert IEDOY:
            IEDOY = IEDOY + ( iEYr - iBYr ) * 365

          endif
        endif
      enddo

      RETURN
      END

c ******************** EDAYCLK
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
      character fs*1,fs1*1
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
C          call edisp(iuout,' No folder separator found.')
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

   1  call edisp(iuout,
     &  'fdroot: error composing path or file from string.')
      call edisp(iuout,fstring)
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
      COMMON/OUTIN/IUOUT,IUIN
      common/pophelp/h(60)

      LOGICAL XST,OK,concat,DOK
      CHARACTER*(*) SFILE
      CHARACTER outs*144,LFIL*72,h*72
      character tfile*144,longtfile*144

      LFIL=' '
      longtfile=' '

C Debug.
C      WRITE(outs,'(2A)')' EFOPSEQ: ',SFILE(1:lnblnk(SFILE))
C      call edisp(iuout,outs)

C Add path to file if required then get length of file name.
      XST=.false.
      IER=-301
      if(SFILE(1:2).eq.'  ')return
      if(SFILE(1:4).eq.'UNKN')return
      if(SFILE(1:4).eq.'unkn')return
      call addpath(SFILE,longtfile,concat)
      IER=0

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
          CALL LUSRMSG(longtfile,'already exists (and it should not)!',
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
        CALL USRMSG(' ',' Unknown existence flag passed!','W')
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
      COMMON/OUTIN/IUOUT,IUIN
      common/pophelp/h(60)

      LOGICAL XST,OK,concat,DOK
      CHARACTER*(*) SFILE
      CHARACTER outs*144,LFIL*72,h*72
      character tfile*144,longtfile*144

C Debug.
C      WRITE(outs,'(A,A)')' EFOPRAN: ',SFILE(1:lnblnk(SFILE))
C      call edisp(iuout,outs)

      LFIL=' '
      longtfile=' '
      XST=.false.
      IER=-301
      if(SFILE(1:2).eq.'  ')return
      if(SFILE(1:4).eq.'UNKN')return
      if(SFILE(1:4).eq.'unkn')return
      call addpath(SFILE,longtfile,concat)
      IER=0

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
     &      'already exists (and it should not)!','W')
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
        CALL USRMSG(' ',' Unknown existence flag passed!','W')
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

c ******************** FINDFIL
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
      CHARACTER path*72,fs*1,fs1*1,tp*12,tp1*12,outs*124,a1*1,a2*1

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
      write(a2,'(a)') sfile(2:2)

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
C   1  write(outs,*) 'addpath: error writing file name: ',tfile
   1  call lusrmsg('addpath: error writing file name',tfile,'W')
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
   1  write(6,*) 'GETTOKENS: error in internal write: ',A
      return
   2  write(6,*) 'GETTOKENS: error in getting token: ',A
      return

      END

C ************ c2fstr
C Convert c function returned string to fortran format.
C Strip off the last character (end of line mark) as well as any
C leading blanks from string returned.
      SUBROUTINE c2fstr(cstr,fstr)
      CHARACTER*(*) cstr,fstr
      logical unixok

      call isunix(unixok)
      if(.NOT.unixok)then

C For NT copy one string to the other.
        write(fstr,'(a)',iostat=ios,err=1) cstr
        return
      else
        fstr = ' '
        K=0
        DO 99 I=1,LEN(cstr)
          IF(cstr(I:I).NE.' '.OR.K.GE.1)THEN
C           WRITE(6,'(a,I4)')cstr(I:I),ichar(cstr(I:I))
            if(ichar(cstr(I:I)).lt.32)goto 100
            K=K+1
            if(K.gt.LEN(fstr))goto 100
            fstr(K:K)=cstr(I:I)
          ENDIF
 99     CONTINUE
      endif
 100  return

   1  write(6,*) 'c2fstr: error writing: ',cstr
      return
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

C For page control of the menus.
      integer CREATE,PREV,NEXT,EDIT
      PARAMETER (CREATE=1,PREV=3,NEXT=6,EDIT=20)
      COMMON/PMENU/MHEAD,MCTL,MIFULL,MFULL,IST,ILEN,IPM,MPM,IPFLG
      COMMON/SPAD/MMOD,LIMIT,LIMTTY
      common/pophelp/h(60)
      CHARACTER H*72

C If creating menu don't ask questions. Allow movement forward or backwards
C in the control menu.
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
      COMMON/PMENU/MHEAD,MCTL,MIFULL,MFULL,IST,ILEN,IPM,MPM,IPFLG

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
      COMMON/PMENU/MHEAD,MCTL,MIFULL,MFULL,IST,ILEN,IPM,MPM,IPFLG
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
      COMMON/PMENU/MHEAD,MCTL,MIFULL,MFULL,IST,ILEN,IPM,MPM,IPFLG
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

C+++++++++++++++++++++++++++++++++++++++++++++++++++++++
      SUBROUTINE EMPAGE(IPAG,IW,IEND)
C EMPAGE: Low level screen control for paging based on terminal MMOD.
C The available terminal see EPAGES.
      COMMON/SPAD/MMOD,LIMIT,LIMTTY
      COMMON/OUTIN/IUOUT,IUIN
      CHARACTER DUMMY*1,blnk*2,blnk2*2,cont*8

      cont='continue'
      blnk='  '
      blnk2='  '

      IF(MMOD.EQ.-2)THEN

C MMOD=-2 TELETYPE: wait if full, page with blank line.
        IF(IEND.EQ.0)THEN
          IF(IPAG.EQ.1)THEN
            WRITE(IUOUT,301)
  301       FORMAT(/,' ',50('-'),'>>',$)
            READ(IUIN,'(A1)')DUMMY
          ENDIF
          LIMIT=LIMTTY
          RETURN
        ELSE
          RETURN
        ENDIF
      ELSEIF(MMOD.EQ.-1)THEN

C MMOD=-1 Text: never wait, page with blank line.
        IF(IEND.EQ.0)THEN
          IF(IPAG.EQ.1)THEN
            WRITE(IUOUT,'(a)') '  '
C            WRITE(IUOUT,300)
C 300        FORMAT(/,' ',72('-'),/)
          ENDIF
          LIMIT=LIMTTY
          RETURN
        ELSE
          RETURN
        ENDIF
      ELSEIF(MMOD.EQ.-6)THEN

C MMOD=-6 SCRIPT: never wait, no page separator.
        IF(IEND.EQ.0)THEN
          LIMIT=LIMTTY
          RETURN
        ELSE
          RETURN
        ENDIF
      ELSEIF(MMOD.EQ.8)THEN

C Bitmapped screen routines.
        IF(IEND.EQ.0)THEN

C If scrolling text display then clear it.
          IF(IPAG.EQ.1)then
            CALL egdispclr
            LIMIT=LIMTTY
          endif
          if(IW.eq.1)then
            call continuebox(blnk,blnk2,cont)
          endif
          RETURN
        ELSE
          call winfin
        ENDIF
      ENDIF
      END

c ******************** ELINC
C ELINC: Controls scratch pad output for text screens which returns:
C For TTY & LPT ELINC tests if N lines fit on the page, if yes then the
C line cout is updated to give lines left on page, if not the terminal is
C paged and a new limit is set.
      SUBROUTINE ELINC(N)
      COMMON/SPAD/MMOD,LIMIT,LIMTTY

      NN=IABS(N)

      IF(MMOD.EQ.-6.OR.MMOD.EQ.-2.OR.MMOD.EQ.-1)THEN

C Line printer or tty terminal.
        LNEW=LIMIT-NN
        IF(LNEW.LT.0)THEN
          CALL EPAGEW

C Updata the line cout on new page if N>0.
          IF(N.GT.0)LIMIT=LIMIT-NN
          RETURN
        ELSE
          IF(N.LT.0)RETURN
          LIMIT=LNEW
          RETURN
        ENDIF
      ELSEIF(MMOD.EQ.8)THEN

C Bitmapped screen: if text screen full prompt with >> in
C graphics window, then clear graphics window.
        LNEW=LIMIT-NN
        IF(LNEW.LT.0)THEN
          CALL EPAGEW

C Updata the line cout on new page if N>0.
          IF(N.GT.0)LIMIT=LIMIT-NN
          RETURN
        ELSE
          IF(N.LT.0)RETURN
          LIMIT=LNEW
          RETURN
        ENDIF
      ENDIF
      END

C++++++++++ EPAGES +++++++++++++++++++++++++++++++++++++++++++++
C EPAGES: Initialise terminal, set up a scratch pad counter depending
C on terminal type.  LIMIT is the number of line output for a full page
C and is initially set at 24 lines (this can be changed by a call to
C SETLINC. The parameter TITLE will appear in the window heading.
C The method of page termination depends on the MMOD number.
C The available terminal types are:
C type -6 = batch/shell/function button mode.
C type -2 = teletype with waiting.
C type -1 = teletype.
C type  8 = bitmapped with dialogue box.
C iappwi is the requested pixel width, iapphi is the requested pixel height
C iappx & iappy are the upper-left position on the monitor <not yet implemented >
C menuchw is the initial request for menu width (in characters)
      SUBROUTINE EPAGES(MODEL,IIN,IOUT,iappwi,iapphi,iappx,iappy,
     &  menuchw,TITLE)
C     SUBROUTINE EPAGES(MODEL,IIN,IOUT,TITLE)
      COMMON/SPAD/MMOD,LIMIT,LIMTTY
      common/gfont/ifs,itfs,imfs
      CHARACTER*(*) TITLE
      CHARACTER HEAD*255

      CHARACTER*5 MM

c If MMOD set to valid index, assume it, dont ask.
      MM=' '
  10  IF(MM.EQ.'-6   '.OR.MODEL.EQ.-6)THEN
        MMOD=-6
        LIMTTY=24
        LIMIT=24
        RETURN
      ELSEIF(MM.EQ.'-2   '.OR.MODEL.EQ.-2)THEN
        MMOD=-2
        LIMTTY=24
        LIMIT=24
        RETURN
      ELSEIF(MM.EQ.'-1   '.OR.MODEL.EQ.-1)THEN
        MMOD=-1
        LIMTTY=24
        LIMIT=24
        RETURN
      ELSEIF(MM.eq.'8    '.OR.MODEL.EQ.8)THEN
        MMOD = 8
        write (HEAD,'(a)') TITLE

C Pass application size via sizehwxy.
        if(iappwi.ge.100)then
          ihight=iapphi
          iwidth=iappwi
          call sizehwxy(ihight,iwidth,iappx,iappy)
        endif
        call jwinint(8,HEAD)
        call winfnt(1)

C Open the dialogue box and begin message then open the
C feedback box with call to feedbox, passing it the initial
C menu character width mechchw. Note, nothing uses igfw or
C igfh. Then call opengdisp.
        call feedbox(menuchw,2,igfw,igfh)
        call opengdisp(menuchw,limtty,2,igdw,igdh)
        CALL msgbox(' ',' ')
        RETURN
      ELSE
        WRITE(IOUT,200)
 200    FORMAT(' Terminal mode (? gives options): ')
        READ(IIN,101,ERR=500,IOSTAT=IOS)MM
 101    FORMAT(A5)
        IF(MM.EQ.'-6   '.OR.MM.EQ.'-2   '.OR.
     &     MM.EQ.'-1   '.OR.MM.EQ.'8    ')THEN
          goto 10
        ELSEIF(MM(1:1).EQ.'?')THEN
          WRITE(IOUT,102)
 102      FORMAT(/
     &  ' This program can be run in either a text mode or a',/,
     &  ' mixed text and graphic mode on a workstation.',/,
     &  ' The following choices are available: ',//,
     &  ' -6 workstation shell mode (to redirect output)',/,
     &  ' -2 text based display & user dialogue with page control',/,
     &  ' -1 text based display & user dialogue',/,
     &  '  8 graphic display & user dialogue',/)
          GOTO 10
        ELSE
          WRITE(IOUT,'(a,a5,a)')'Did not understand your response: ',
     &      MM,'.  Please try again.'
          GOTO 10
        ENDIF
      ENDIF
      RETURN
  500 call edisp(iout,'Error reading terminal type.  Try again.')
      goto 10
      END


C++++++++++ EMENU +++++++++++++++++++++++++++++++++++++++++++++
C EMENU: Control menu display on various terminals.  Name is a character
C string to form the heading of menu, ITEMS is an array of character
C strings making up the menu, NITMS is the number of items in the menu,
C INO is the number of the item chosen.
      SUBROUTINE EMENU(NAME,ITEMS,NITMS,INO)
      COMMON/OUTIN/IUOUT,IUIN
      COMMON/SPAD/MMOD,LIMIT,LIMTTY
      CHARACTER*1 JJ
      CHARACTER*(*) NAME,ITEMS(NITMS)
      CHARACTER*80 tmpa
      logical odd

      tmpa='                                                          '

C First loop through items and find the longest string.
      ITLENG=max(1,LNBLNK(NAME))
      ISLENG=0
      IMLENG=0
      DO 80 IL=1,NITMS
        ISLENG=max(1,LNBLNK(ITEMS(IL)))
        IF(ISLENG.GT.IMLENG)IMLENG=ISLENG
  80  CONTINUE

C See if an even or odd number of items in list.
      im=MOD(NITMS,2)
      odd=.false.
      if(im.eq.1) odd=.true.

C Generate menu depending on terminal.
      if(MMOD.eq.8)then

C Bitmapped screen :  do calls for raster menu, 0,0 are passed
C to define std upper right position, IRPX,IRPY not used.
        CALL EWMENU(NAME,ITEMS,NITMS,0,0,IRPX,IRPY,INO)
        return
      else
        IF(INO.GE.-1)GO TO 20

C Print out the menu. If text is > 36 char or there are few items
C then restrict to a single column. If less than 36 characters
C print double column.
        IF(IMLENG.GT.36.OR.NITMS.LE.6)THEN
          CALL ELINC(NITMS)
          LN=max(1,LNBLNK(ITEMS(1)))
          WRITE(IUOUT,900,iostat=ios,err=1)NAME(1:ITLENG),ITEMS(1)(1:LN)
  900     FORMAT(/,1X,a,': ',a)
          DO 92 K=2,NITMS
            LN=max(1,LNBLNK(ITEMS(K)))
            WRITE(IUOUT,901,iostat=ios,err=2)tmpa(1:ITLENG),
     &       ITEMS(K)(1:LN)
  901       FORMAT(1X,a,'  ',a)
   92     CONTINUE
        ELSEIF(IMLENG.LT.24)THEN
          MNULEN=NITMS/2
          CALL ELINC(MNULEN)
          WRITE(IUOUT,902,iostat=ios,err=2)NAME(1:ITLENG),
     &      ITEMS(1)(1:IMLENG),ITEMS(MNULEN+1)(1:IMLENG)
  902     FORMAT(/,1X,a,':  ',a,'   ',a)
          DO 93 K=2,MNULEN
            WRITE(IUOUT,903,iostat=ios,err=2)tmpa(1:ITLENG),
     &        ITEMS(K)(1:IMLENG),ITEMS(K+MNULEN)(1:IMLENG)
  903       FORMAT(1X,a,'   ',a,'   ',a)
   93     CONTINUE
          IF(odd)THEN
            LN=max(1,LNBLNK(ITEMS(NITMS)))
            WRITE(IUOUT,904,iostat=ios,err=2)tmpa(1:ITLENG),
     &        ITEMS(NITMS)(1:LN)
  904       FORMAT (1X,a,':  ',a)
          ENDIF
        ELSEIF(IMLENG.GE.24.and.IMLENG.LE.36)THEN
          MNULEN=(NITMS/2)
          CALL ELINC(MNULEN)
          WRITE(IUOUT,'(2a)',iostat=ios,err=2)NAME(1:ITLENG),':'
          DO 193 K=1,MNULEN
            WRITE(IUOUT,'(2X,a,3x,a)',iostat=ios,err=2)
     &        ITEMS(K)(1:IMLENG),ITEMS(K+MNULEN)(1:IMLENG)
  193     CONTINUE
          IF(odd)THEN
            LN=max(1,LNBLNK(ITEMS(NITMS)))
            WRITE(IUOUT,'(2X,a)',iostat=ios,err=2)
     &        ITEMS(NITMS)(1:LN)
          ENDIF
        ENDIF

C Solicit Reply.
  20    LN=max(1,LNBLNK(NAME))
        WRITE(IUOUT,104)NAME(1:LN)
 104    FORMAT(/,' ',a,':?> ',$)
        READ(IUIN,105,iostat=ios,ERR=20)JJ
 105    FORMAT(A1)
        ICUR=IFIRST(JJ)

C TEST REPLY IS IN MENU
        DO 23 JNO=1,NITMS
          INO=JNO
          IF(ICUR.EQ.IFIRST(ITEMS(JNO)))RETURN
  23    CONTINUE
        DO 24 JNO=1,NITMS
          INO=JNO
          IF(ICUR-32.EQ.IFIRST(ITEMS(JNO)))RETURN
  24    CONTINUE
      ENDIF
   1  call edisp(IUOUT,'emenu: error composing prompt.')
      return
   2  call edisp(IUOUT,'emenu: error composing item.')
      return
      END


C++++++++++ EWMENU +++++++++++++++++++++++++++++++++++++++++++++
C EWMENU: Is the binding to C function for menu dialogue.  It
C allows the string widths to be variable widths. Uses width of
C items in call to evwmenu.
      SUBROUTINE EWMENU(name,items,nitms,impx,impy,irpx,irpy,ino)
      character*(*) name, items(*)
      character itypes*40

C Append a null terminator to the title string if possible.
      iw=LEN(items(1))
      write(itypes,9)
   9  format(40('-'))
      call updmenu(items,itypes,nitms,iw)
      call evwmenu (name,impx,impy,iw,irpx,irpy,ino,ipflg,iuresp)
      if (ino.lt.0) then

C Overload INO with click position and button.  A nine digit number is
C used the first digit is the mouse button and the next 4 represent the
C x pixel and the last 4 the y pixel.
        ino=-100000000*iuresp-10000*irpx-irpy
C Debug...        write(6,*) 'EWMENU (fortran) ',iw,irpx,irpy,ino,ipflg,iuresp
      endif
      return
      end

C++++++++++ VWMENU +++++++++++++++++++++++++++++++++++++++++++++
C VWMENU: Is the binding to C function for menu dialogue.  It
C allows the string widths to be variable widths.
      SUBROUTINE VWMENU(name,items,nitms,impx,impy,iw,irpx,irpy,ino)
      character*(*) name, items(*)
      character itypes*40

      write(itypes,9)
   9  format(40('-'))
      call updmenu(items,itypes,nitms,iw)
      call evwmenu (name,impx,impy,iw,irpx,irpy,ino,ipflg,iuresp)
      return
      end

C *********************** EMKEY
C EMKEY returns a key character for a menu item (a-z) based on the array
C index of the item.  Uses ICHPK(26), 'a','b'... from esrulib...
      SUBROUTINE EMKEY(IAI,KEY,IER)
      DIMENSION ICHPK(26)
      CHARACTER*1 ICHPK,KEY,mesg*36,bl*2

      DATA ICHPK/'a','b','c','d','e','f','g','h','i','j','k','l','m',
     &       'n','o','p','q','r','s','t','u','v','w','x','y','z'/

      IER=0
      IF(IAI.LT.1)THEN
        IER=1
        mesg='EMKEY: negative index supplied '
        bl='  '
        CALL USRMSG(mesg,bl,'W')
        RETURN
      ELSE
        IX=MOD(IAI,26)
        IF(IX.EQ.0)IX=26
        KEY=ICHPK(IX)
C Debug...
C        WRITE(iuout,27)IAI,IX,KEY
C   27   FORMAT('EMKEY: IAI,IX,KEY',2I4,1X,A1)
      ENDIF
      RETURN
      END


c ******************** USRMSG
c Generic error reporting facility.  MSG1
C and MSG2 are text strings (upto 124 char) to be printed.  LEVEL is a
C single character 'W' or 'w' for warning (in graphic mode followed by
C a clearing of dialog), 'F' or 'f' for Fatal error,
C '-' to only print the messages, '?' is a prompt in text mode. If
C LEVEL is 'P' or 'p' then pause breifly before continuing. In the case of a
C fatal error STOP will be called, otherwise execution will return to
C the calling point.
C In the case of a graphic menu the c function msg_box will be called
C and the messages will appear in a dialogue box at the bottom of the
C graphic window. The user must have previously called open_msg_box(2).

      SUBROUTINE USRMSG(MSG1,MSG2,LEVEL)
      COMMON/SPAD/MMOD,LIMIT,LIMTTY
      common/OUTIN/IUOUT,IUIN
      CHARACTER*(*) MSG1,MSG2
      CHARACTER outs*124,LEVEL*1,blnk*2,blnk2*2,cont*8

      IF(MSG2(1:1).EQ.'-')MSG2=' '
      blnk='  '
      blnk2='  '
      cont='continue'
      LN1=max(1,LNBLNK(MSG1))
      LN2=max(1,LNBLNK(MSG2))

C Print out message according to the combination of strings passed. Use
C the dialogue box if terminal type 8.
      IF(MMOD.NE.8)THEN
        IF(LEVEL.EQ.'-'.or.LEVEL.eq.'P'.or.LEVEL.eq.'p')THEN
          WRITE(outs,'(A)',iostat=ios,err=1) MSG1(1:LN1)
          call edisp(iuout,outs)
        ELSEIF(LEVEL.EQ.'W'.OR.LEVEL.EQ.'w')THEN
          WRITE(outs,201,iostat=ios,err=1)MSG1(1:LN1)
 201      FORMAT(' Warning: ',a)
          call edisp(iuout,outs)
        ELSEIF(LEVEL.EQ.'F'.OR.LEVEL.EQ.'f')THEN
          WRITE(outs,202,iostat=ios,err=1)MSG1(1:LN1)
 202      FORMAT(' Fatal error: ',a)
          call edisp(iuout,outs)

C Single line prompt.
        ELSEIF(LEVEL.EQ.'?'.AND.MSG2(1:LN2).EQ.' ')THEN
          WRITE(iuout,203,iostat=ios,err=1)MSG1(1:LN1)
 203      FORMAT(a,' ',$)
        ELSEIF(LEVEL.EQ.'?'.AND.MSG2(1:LN2).NE.' ')THEN
          WRITE(outs,'(A)',iostat=ios,err=1)MSG1(1:LN1)
          call edisp(iuout,outs)
        ELSE
          call edisp(iuout,' Incorrect syntax in USRMSG ')
        ENDIF

        IF(MSG2(1:LN2).NE.' ')THEN
          IF(LEVEL.EQ.'?')THEN
            WRITE(iuout,203,iostat=ios,err=1) MSG2(1:LN2)
          ELSE
            WRITE(outs,'(A)',iostat=ios,err=1) MSG2(1:LN2)
            call edisp(iuout,outs)
          ENDIF
        ENDIF
        IF(LEVEL.EQ.'F'.OR.LEVEL.EQ.'f')then
          call pauses(1)
          STOP
        endif
        RETURN
      ELSEIF(MMOD.EQ.8)THEN
        IF(LEVEL.EQ.'-'.OR.LEVEL.EQ.'?')THEN
          CALL msgbox(MSG1,MSG2)
        ELSEIF(LEVEL.EQ.'W'.OR.LEVEL.EQ.'w')THEN

C If warning mode then clear continuebox after use clicks continue.
          CALL continuebox(MSG1,MSG2,cont)
          CALL msgbox(blnk,blnk2)
        ELSEIF(LEVEL.EQ.'P'.OR.LEVEL.EQ.'p')THEN
          CALL msgbox(MSG1,MSG2)
          call pauses(1)
        ELSEIF(LEVEL.EQ.'F'.OR.LEVEL.EQ.'f')THEN

C Place in text window since about to exit from program.
          WRITE(iuout,202,iostat=ios,err=1)MSG1(1:LN1)
          WRITE(iuout,'(A)',iostat=ios,err=1)MSG2(1:LN2)
        ELSE
          CALL msgbox(' ',' Incorrect syntax in USRMSG.')
          call pauses(1)
        ENDIF

        IF(LEVEL.EQ.'F'.OR.LEVEL.EQ.'f')then
          call pauses(2)
          STOP
        endif
        RETURN
      ENDIF
    1 call edisp(iuout,'USRMSG: error composing prompt. ')
      return
      END

c ******************** LUSRMSG
c Generic error reporting facility.  MSG1
C and MSG2 are text strings (upto 248 char) to be printed.  LEVEL is a
C single character 'W' or 'w' for warning (in graphic mode followed by
C a clearing of dialog), 'F' or 'f' for Fatal error,
C '-' to only print the messages, '?' is a prompt in text mode. If
C LEVEL is 'P' or 'p' then pause breifly before continuing. In the case of a
C fatal error STOP will be called, otherwise execution will return to
C the calling point.
C In the case of a graphic menu the c function msg_box will be called
C and the messages will appear in a dialogue box at the bottom of the
C graphic window. The user must have previously called open_msg_box(2).

      SUBROUTINE LUSRMSG(MSG1,MSG2,LEVEL)
      COMMON/SPAD/MMOD,LIMIT,LIMTTY
      common/OUTIN/IUOUT,IUIN
      CHARACTER*(*) MSG1,MSG2
      CHARACTER outs*124,LEVEL*1,blnk*2,blnk2*2,cont*8

      IF(MSG2(1:1).EQ.'-')MSG2=' '
      blnk='  '
      blnk2='  '
      cont='continue'
      LN1=max(1,LNBLNK(MSG1))
      LN2=max(1,LNBLNK(MSG2))

C Print out message according to the combination of strings passed. Use
C the dialogue box if terminal type 8.
      IF(MMOD.NE.8)THEN
        IF(LEVEL.EQ.'-'.or.LEVEL.eq.'P'.or.LEVEL.eq.'p')THEN
          if(lnblnk(MSG1).gt.124)then
            WRITE(outs,'(A)',iostat=ios,err=1) MSG1(1:124)
          else
            WRITE(outs,'(A)',iostat=ios,err=1) MSG1(1:LN1)
          endif
          call edisp(iuout,outs)
        ELSEIF(LEVEL.EQ.'W'.OR.LEVEL.EQ.'w')THEN
          if(lnblnk(MSG1).gt.114)then
            WRITE(outs,201,iostat=ios,err=1)MSG1(1:114)
          else
            WRITE(outs,201,iostat=ios,err=1)MSG1(1:LN1)
          endif
 201      FORMAT(' Warning: ',a)
          call edisp(iuout,outs)
        ELSEIF(LEVEL.EQ.'F'.OR.LEVEL.EQ.'f')THEN
          if(lnblnk(MSG1).gt.110)then
            WRITE(outs,202,iostat=ios,err=1)MSG1(1:110)
          else
            WRITE(outs,202,iostat=ios,err=1)MSG1(1:LN1)
          endif
 202      FORMAT(' Fatal error: ',a)
          call edisp(iuout,outs)

C Single line prompt.
        ELSEIF(LEVEL.EQ.'?'.AND.MSG2(1:LN2).EQ.' ')THEN
          if(lnblnk(msg1).gt.124)then
            WRITE(iuout,203,iostat=ios,err=1)MSG1(1:124)
          else
            WRITE(iuout,203,iostat=ios,err=1)MSG1(1:LN1)
          endif
 203      FORMAT(a,' ',$)
        ELSEIF(LEVEL.EQ.'?'.AND.MSG2(1:LN2).NE.' ')THEN
          if(lnblnk(msg1).gt.124)then
            WRITE(outs,'(A)',iostat=ios,err=1)MSG1(1:124)
          else
            WRITE(outs,'(A)',iostat=ios,err=1)MSG1(1:LN1)
          endif
          call edisp(iuout,outs)
        ELSE
          call edisp(iuout,' Incorrect syntax in USRMSG ')
        ENDIF

        IF(MSG2(1:LN2).NE.' ')THEN
          IF(LEVEL.EQ.'?')THEN
            WRITE(iuout,203,iostat=ios,err=1) MSG2(1:LN2)
          ELSE
            if(lnblnk(msg2).gt.124)then
              WRITE(outs,'(A)',iostat=ios,err=1) MSG2(1:124)
            else
              WRITE(outs,'(A)',iostat=ios,err=1) MSG2(1:LN2)
            endif
            call edisp(iuout,outs)
          ENDIF
        ENDIF
        IF(LEVEL.EQ.'F'.OR.LEVEL.EQ.'f')then
          call pauses(1)
          STOP
        endif
        RETURN
      ELSEIF(MMOD.EQ.8)THEN
        IF(LEVEL.EQ.'-'.OR.LEVEL.EQ.'?')THEN
          CALL msgbox(MSG1,MSG2)
        ELSEIF(LEVEL.EQ.'W'.OR.LEVEL.EQ.'w')THEN

C If warning mode then clear continuebox after use clicks continue.
          CALL continuebox(MSG1,MSG2,cont)
          CALL msgbox(blnk,blnk2)
        ELSEIF(LEVEL.EQ.'P'.OR.LEVEL.EQ.'p')THEN
          CALL msgbox(MSG1,MSG2)
          call pauses(1)
        ELSEIF(LEVEL.EQ.'F'.OR.LEVEL.EQ.'f')THEN

C Place in text window since about to exit from program.
          WRITE(iuout,202,iostat=ios,err=1)MSG1(1:LN1)
          WRITE(iuout,'(A)',iostat=ios,err=1)MSG2(1:LN2)
        ELSE
          CALL msgbox(' ',' Incorrect syntax in USRMSG.')
          call pauses(1)
        ENDIF

        IF(LEVEL.EQ.'F'.OR.LEVEL.EQ.'f')then
          call pauses(2)
          STOP
        endif
        RETURN
      ENDIF
    1 call edisp(iuout,'USRMSG: error composing prompt. ')
      return
      END

C ********************* EDISP
C EDISP is a generic routine which displays lines of text passed to it
C in a format depending on the terminal type:
C For types -1 -2 9 does a fortran write to channel IUOUT,
C For types -6 writes to ICOUT,
C For type 8 manages the text which is passed to egdisp for
C treatment as a scrolling window.
C Use egdispclr to clear the display window on a new page.
C The maximum width of text which can be accommodated is 124 characters.
      SUBROUTINE EDISP(ITRU,MSG)
      COMMON/SPAD/MMOD,LIMIT,LIMTTY
      COMMON/OUTIN/IUOUT,IUIN
      common/textbuf/dispbuf(360)
      common/textbufl/indexbuf,lnbuf(360)
      CHARACTER*(*) MSG
      CHARACTER WWMSG*124,dispbuf*124

C Temporarily set ICOUT.
      ICOUT=0

C Create matching string to pass to wwlib.c and hold in dispbuf.
C Use logic similar to that in egdisp.
      lnm=max0(1,lnblnk(MSG))
      lcc = MIN0(lnm,124)
      if(lcc.eq.0) lcc=1
      write(WWMSG,'(a)',iostat=ios,err=1)MSG(1:lcc)

      if(indexbuf.lt.360)then
        indexbuf=indexbuf+1
        lnbuf(indexbuf)=lcc
        dispbuf(indexbuf)='  '
        write(dispbuf(indexbuf),'(a)') MSG(1:lcc)
      else
        do 42 i=1,359
          dispbuf(i)=dispbuf(i+1)
          lnbuf(i)=lnbuf(i+1)
  42    continue
        lnbuf(indexbuf)=lcc
        dispbuf(indexbuf)='  '
        write(dispbuf(indexbuf),'(a)') MSG(1:lcc)
      endif
      IF(MMOD.EQ.8.AND.ITRU.NE.IUOUT)THEN

C If trace channel etc then just write to file.
        WRITE(ITRU,'(A)',iostat=ios,err=1)MSG(1:lnm)
      ELSEIF(MMOD.EQ.8.AND.ITRU.EQ.IUOUT)THEN
          LINE=LIMTTY-LIMIT
          LIMIT=LIMIT-1
          if (LIMIT.lt.1) LIMIT=1
          call espad(limit,limtty,line)
          CALL egdisp(WWMSG,LINE)
      ELSEIF(MMOD.EQ.-6)THEN

C Write this to error channel if user specified channel = ICOUT
C otherwise redirect to user display.
        IF(ITRU.EQ.ICOUT)THEN
          WRITE(ICOUT,'(A)',iostat=ios,err=1)MSG(1:lnm)
        ELSE
          CALL ELINC(1)
          WRITE(ITRU,'(A)',iostat=ios,err=1)MSG(1:lnm)
        ENDIF
      ELSE

C Write this to standard text window.
        CALL ELINC(1)
        WRITE(ITRU,'(A)',iostat=ios,err=1)MSG(1:lnm)
      ENDIF

      RETURN
   1  write(6,*) 'edisp: error writing text: ',MSG
      return
   2  write(6,*) 'edisp: error writing text: ',WWMSG
      return
      END


C ********************* EDISP248
C EDISP248 displays a 248 char block of text passed to it
C in a format depending on the terminal type. If it will not
C fit on one line, subsequent lines are used and breaks are
C set based on nearest width to iwid. 
      SUBROUTINE EDISP248(ITRU,MSG,iwid)
      COMMON/OUTIN/IUOUT,IUIN
      CHARACTER*(*) MSG
      CHARACTER outs*144    ! the text buffer for printing one line
      integer lenmsg,lenalts  ! length of the passed msg

C If width for word-warp is not reasonable, return.
      if(iwid.lt.24.or.iwid.gt.144)then
        call edisp(iuout,'edisp248 width is overly short or long')
        return
      endif

C Remember length of MSG
      lenmsg=0
      lenmsg=max0(1,lnblnk(MSG))

C Find blanks near iwid points along the text block. It is
C assumed that iwid is ~72, but can be 48-144. In the case of
C long words burried in the block of text we might get cases
C where ipb is the same as ipa. Use call to inextblnk to find
C the next blank after ipos. If the next blank is farther along
C the string than we would normally warp to this indicates we
C have a long phrase. If is is before we would normally warp
C then it might be because we have reaced the end of the string.
      iwidhalf=iwid/2 
      ipa=iprevblnk(MSG,iwid)     ! find blank before iwid
      ipanext=inextblnk(MSG,iwid) ! find blank after iwid
      ipb=iprevblnk(MSG,ipa+iwid) ! find blank nearest next iwid point
      if(ipa.eq.ipb)then
        if(ipanext.ge.ipa+iwid)then
          ipb=ipanext             ! found a long phrase
        elseif(ipanext.eq.lenmsg)then
          ipb=lenmsg              ! at end of MSG
        else
          ipb=iprevblnk(MSG,ipa+iwid+iwidhalf)
          if(ipa.eq.ipb)then

C If ipb is still the same test from double the iwid.
C This should catch phrases that are approximately
C double the width of iwid (like file names with explicit paths).
            ipb=iprevblnk(MSG,ipa+iwid+iwid)
          endif
        endif
      endif 

C Repeat logic for potential third line. First ensure we
C are not testing beyond the length of the string.
      if(ipb+iwid.ge.lenmsg)then
        ipc=lenmsg
      else
        ipc=iprevblnk(MSG,ipb+iwid)
        ipcnext=inextblnk(MSG,ipb+iwid)
        if(ipb.eq.ipc)then
          if(ipcnext.ge.ipb+iwid)then
            ipc=ipcnext
          elseif(ipcnext.eq.lenmsg)then
            ipc=lenmsg
          else
            ipc=iprevblnk(MSG,ipb+iwid+iwidhalf)
            if(ipb.eq.ipc)then
              ipc=iprevblnk(MSG,ipb+iwid+iwid)
            endif
          endif
        endif
      endif

C Repeat logic for potential fourth line. First ensure we
C are not testing beyond the length of the string.
      if(ipc+iwid.ge.lenmsg)then
        ipd=lenmsg
      else
        ipd=iprevblnk(MSG,ipc+iwid)
        ipdnext=inextblnk(MSG,ipc+iwid)
        if(ipc.eq.ipd)then
          if(ipdnext.ge.ipc+iwid)then
            ipd=ipdnext
          elseif(ipdnext.eq.lenmsg)then
            ipd=lenmsg
          else
            ipd=iprevblnk(MSG,ipc+iwid+iwidhalf)
            if(ipc.eq.ipd)then
              ipd=iprevblnk(MSG,ipc+iwid+iwid)
            endif
          endif
        endif
      endif

C Repeat logic for potential fifth line. First ensure we
C are not testing beyond the length of the string.
      if(ipd+iwid.ge.lenmsg)then
        ipe=lenmsg
      else
        ipe=iprevblnk(MSG,ipd+iwid)
        ipenext=inextblnk(MSG,ipd+iwid)
        if(ipd.eq.ipe)then
          if(ipenext.ge.ipc+iwid)then
            ipe=ipenext
          elseif(ipenext.eq.lenmsg)then
            ipe=lenmsg
          else
            ipe=iprevblnk(MSG,ipd+iwid+iwidhalf)
            if(ipd.eq.ipe)then
              ipe=iprevblnk(MSG,ipd+iwid+iwid)
            endif
          endif
        endif
      endif

C And the 6th line.
      if(ipe+iwid.ge.lenmsg)then
        ipf=lenmsg
      else
        ipf=iprevblnk(MSG,ipe+iwid)
      endif

C Debug.
      lenalts=0
      lenalts=max0(lenmsg,ipa,ipb,ipc,ipd,ipe,ipf)
C      write(6,*) 'blanks @ ',ipa,ipb,ipc,ipd,ipe,ipf,
C     &  'lenmsg lenalts ',lenmsg,lenalts
C      write(6,*) 'n blanks @ ',ipanext,ipcnext,ipdnext,ipenext

C Process first block of text, and if subsequent blocks are
C non-blank, do them as well. If iwid is less than 72 there
C will be more lines.
      outs=' '
      write(outs,'(a)') MSG(1:ipa)
      call edisp(itru,outs)
      if(ipb.gt.ipa)then

C Trap phrases longer than the outs buffer.
        if(ipb-(ipa+1).gt.143)then
          ipbb = ipa+143
        else
          ipbb = ipb
        endif
        outs=' '
        write(outs,'(a)') MSG(ipa+1:ipbb)
        call edisp(itru,outs)
      endif
      if(ipc.gt.ipb)then
        if(ipc-(ipb+1).gt.143)then
          ipcc = ipb+143
        else
          ipcc = ipc
        endif
        outs=' '
        write(outs,'(a)') MSG(ipb+1:ipcc)
        call edisp(itru,outs)
      endif
      if(ipd.gt.ipc)then
        if(ipd-(ipc+1).gt.143)then
          ipdd = ipc+143
        else
          ipdd = ipd
        endif
        outs=' '
        write(outs,'(a)') MSG(ipc+1:ipdd)
        call edisp(itru,outs)
      endif
      if(ipe.gt.ipd)then
        if(ipe-(ipd+1).gt.143)then
          ipee = ipd+143
        else
          ipee = ipe
        endif
        outs=' '
        write(outs,'(a)') MSG(ipd+1:ipee)
        call edisp(itru,outs)
      endif
      if(ipf.gt.ipe)then
        outs=' '
        write(outs,'(a)') MSG(ipe+1:ipf)
        call edisp(itru,outs)
      endif

      RETURN
      END

C ********************* clrtextbuf
C clrtextbuf clears the text buffer common blocks.
      subroutine clrtextbuf()
      common/textbuf/dispbuf(360)
      common/textbufl/indexbuf,lnbuf(360)
      character dispbuf*124

      indexbuf=0
      do 42 i=1,360
        dispbuf(i)='  '
        lnbuf(i)=1
  42  continue
      return
      end

C ****** write current text buffer to an appropraite file
      subroutine proftxdump()

C Path to model and command line file (if any).
      COMMON/SPAD/MMOD,LIMIT,LIMTTY
      COMMON/FILEP/IFIL
      common/rpath/path
      common/uhome/upath
      common/pophelp/h(60)
      common/C21/IFCFG,cfgroot,LCFGF
      common/textbuf/dispbuf(360)
      common/textbufl/indexbuf,lnbuf(360)

      DIMENSION sstr(1),list(4),iswidth(1),listact(4),dh(60)
      character path*72,upath*72,h*72,tfile*60,uname*24,fs*1
      character sstr*60,title*18,ltypes*4,list*17,cfgroot*24
      character LCFGF*72,dispbuf*124,dh*72
      logical unixok,there
      integer ltf  ! position of last character in string.

      if(MMOD.ne.8)return

C Set folder separator (fs) to \ or / as required.
      call isunix(unixok)
      if(unixok)then
        fs = char(47)
      else
        fs = char(92)
      endif
      uname=' '
      call usrname(uname)

      h(1)='The text displayed in the feedback area is '
      h(2)='held in a buffer which can be written to a '
      h(3)='file either overwriting or appending the   '
      h(4)='text in the file (tick only one box).      '
      h(5)='The file name can be edited. Complete edit '
      h(6)='with Carriage Return.                       '
      nhelp=6
      CALL PHELPW(NHELP,IHW,IER)
      call dupphelp(dh)
      call updhelp(dh,NHELP,IHW)
      tfile='  '
      LNU=max(1,lnblnk(upath))
      if(cfgroot(1:2).eq.'  ')then
        LN=max(1,lnblnk(uname))
        write(sstr(1),'(4a)') upath(1:LNU),fs,uname(1:LN),'_text'
      else
        LN=max(1,lnblnk(cfgroot))
        write(sstr(1),'(4a)') upath(1:LNU),fs,cfgroot(1:LN),'_text'
      endif
      iswidth(1)=60
      title='Export text buffer'
      ltypes='ltte'
      list(1)='options:         '
      list(2)='write/create file'
      list(3)='append to file   '
      list(4)='file:            '
      listact(1) = 0
      listact(2) = 1
      listact(3) = 0
      listact(4) = 0
      impx = 0
      impy = 0
      impcwth = 67
      nstr = 1
      nlist = 4
  43  call fprofma(sstr,nstr,title,list,nlist,ltypes,impx,impy,impcwth,
     &    iswidth,listact,ino,nhelp)

      write(6,*)listact,'  ',ino
      if(ino.eq.-1)then

C If no action return.
        return
      elseif(ino.eq.(nlist+1))then

C If signaled ok then attempt to write out file.
        write(6,*)sstr(1)
        tfile=sstr(1)
        ltf=max(1,LNBLNK(tfile))
        iuf=ifil+1
        there=.false.
        INQUIRE(FILE=tfile(1:ltf),EXIST=there)
        if(there)then
          close(iuf)
#ifdef F90
          open(iuf,file=tfile(1:ltf),position='APPEND',
     &      status='UNKNOWN',err=1)
#else
          open(iuf,file=tfile(1:ltf),ACCESS='APPEND',status='UNKNOWN',
     &      err=1)
#endif
        else
          close(iuf)
          open(iuf,file=tfile,status='UNKNOWN',err=1)
        endif
        if(indexbuf.gt.1)then
          do 42 i=1,indexbuf
            write(iuf,'(a)',iostat=ios,err=2) dispbuf(i)(1:lnbuf(i))
  42      continue
        endif
        close(iuf)
        return
      elseif(ino.eq.1)then

C If user has switched from create to append update display (reset
C impx impy before recalling fprofma.
        if(listact(2).eq.1)listact(3)=0
        impx = 0
        impy = 0
        goto 43
      elseif(ino.eq.2)then
        if(listact(3).eq.1)listact(2)=0
        impx = 0
        impy = 0
        goto 43
      else
        impx = 0
        impy = 0
        goto 43
      endif
      return

   1  call usrmsg('Could not open/write dump file...',tfile,'W')
      return
   2  call usrmsg('Could not append to dump file...',tfile,'W')
      return

      end

C ****** invoke graphics capture proforma
      subroutine profgrdump()

C Path to model and command line file (if any).
      COMMON/SPAD/MMOD,LIMIT,LIMTTY
      COMMON/FILEP/IFIL
      common/rpath/path
      common/uhome/upath
      common/pophelp/h(60)
      common/C21/IFCFG,cfgroot,LCFGF

C Graphic capture, window dump: label, command.
      common/gprint/gprlbl,gprcmd
      common/gxwd/gxwdlbl,gxwdcmd

      DIMENSION sstr(1),list(4),iswidth(1),listact(4),dh(60)
      character path*72,upath*72,h*72,tfile*60,uname*24,fs*1
      character sstr*60,title*16,ltypes*4,list*17,cfgroot*24
      character LCFGF*72,dh*72,gcmd*90
      character gprlbl*20,gprcmd*48
      character gxwdlbl*20,gxwdcmd*60

      logical unixok

      if(MMOD.ne.8)return

C Set folder separator (fs) to \ or / as required.
      call isunix(unixok)
      if(unixok)then
        fs = char(47)
      else
        fs = char(92)
      endif
      uname=' '
      call usrname(uname)

      h(1)='A screen image of a user selected portion '
      h(2)='of the screen or the whole screen can be   '
      h(3)='captured. It uses the application defined  '
      h(4)='in the esprc file (typically import from   '
      h(5)='the ImageMagick suite). A GIF image is     '
      h(6)='assumed. Edit the file name as required.   '
      nhelp=6
      CALL PHELPW(NHELP,IHW,IER)
      call dupphelp(dh)
      call updhelp(dh,NHELP,IHW)
      tfile='  '
      LNU=max(1,lnblnk(upath))
      if(cfgroot(1:2).eq.'  ')then
        LN=max(1,lnblnk(uname))
        write(sstr(1),'(4a)') upath(1:LNU),fs,uname(1:LN),'_.gif'
      else
        LN=max(1,lnblnk(cfgroot))
        write(sstr(1),'(4a)') upath(1:LNU),fs,cfgroot(1:LN),'_.gif'
      endif
      iswidth(1)=60
      title='capture graphics'
      ltypes='ltte'
      list(1)='options:         '
      list(2)='user defined area'
      list(3)='whole of screen  '
      list(4)='file:            '
      listact(1) = 0
      listact(2) = 1
      listact(3) = 0
      listact(4) = 0
      impx = 0
      impy = 0
      impcwth = 67
      nstr = 1
      nlist = 4
  43  call fprofma(sstr,nstr,title,list,nlist,ltypes,impx,impy,impcwth,
     &    iswidth,listact,ino,nhelp)

      write(6,*)listact,'  ',ino
      if(ino.eq.-1)then

C If no action return.
        return
      elseif(ino.eq.(nlist+1))then

C If signaled ok then attempt to write out file.
        write(6,*)sstr(1)
        tfile=sstr(1)
        ltf=max(1,lnblnk(tfile))
        if(listact(2).eq.1)then
          LN=max(1,lnblnk(gprcmd))
          write(gcmd,'(3a)') gprcmd(1:LN),' ',tfile(1:ltf)
        elseif(listact(3).eq.1)then
          LN=max(1,lnblnk(gxwdcmd))
          write(gcmd,'(3a)') gxwdcmd(1:LN),' ',tfile(1:ltf)
        endif
        write(6,*)gcmd
        call runit(gcmd,'-')
        return
      elseif(ino.eq.1)then

C If user has switched from create to append update display (reset
C impx impy before recalling fprofma.
        if(listact(2).eq.1)listact(3)=0
        impx = 0
        impy = 0
        goto 43
      elseif(ino.eq.2)then
        if(listact(3).eq.1)listact(2)=0
        impx = 0
        impy = 0
        goto 43
      else
        impx = 0
        impy = 0
        goto 43
      endif
      return

      end

C ********************* EDDISP
C EDDISP is a generic routine which displays lines of text passed to it
C in a format depending on the terminal type and the currently set
C delimiter:
      SUBROUTINE EDDISP(ITRU,MSG)
      common/exporttg/xfile,tg,delim
      COMMON/EXPORTI/ixopen,ixunit,ixpunit
      CHARACTER*(*) MSG
      CHARACTER WWMSG*124
      character xfile*72,tg*1,delim*1,dg*1

C If delimiter set to alternative then process text before edisp call.
      if(delim.eq.'-')then
        call edisp(itru,MSG)
      else

C If using X delimeter (tagged data) then set the delimeter to a comma.
C Tagging is handled when the initial string is created.
        dg=delim
        if (delim.eq.'X') dg='C'
        call SDELIM(MSG,WWMSG,dg,IW)
        call edisp(itru,WWMSG)
      endif
      return
      END

C ********************* SDELIM
C SDELIM replaces blanks in a string A with alternative delimiter and
C returns in B. If the last character in the string is the alternative
C delimiter then replace it with a blank.
      SUBROUTINE SDELIM(A,B,delm,IW)
      CHARACTER*(*) A,B
      CHARACTER C*1,CL*1,delm*1


C Depending on the replacement separator, convert all existing
C ' ', ',' and 'tabs' to the new separator. Loop through filled
C part of string A plus one character.
      LS=LEN(A)
      LSN=max(1,LNBLNK(A))
      lsmn=MIN0(LS,LSN+1)
      B=' '
      K=0
      DO 99 I=1,lsmn
        C=A(I:I)
        if(K.eq.0)then

C Check for initial blanks, commas or tabs.
          if(C.eq.' '.or.C.EQ.','.OR.ICHAR(C).EQ.9)then
            goto 99
          else
            K=K+1
            B(K:K)=C
            CL=C
          endif
        else

C If a separator found, convert it unless the previous character
C was a separator in which case it can be skipped.
          if(C.eq.' '.or.C.EQ.','.OR.ICHAR(C).EQ.9)then
            if(CL.eq.' '.or.CL.EQ.','.OR.ICHAR(CL).EQ.9)then
              goto 99
            else
              K=K+1
              if(delm.eq.'T')then
                B(K:K)=CHAR(9)
              elseif(delm.eq.'S')then
                B(K:K)=' '
              elseif(delm.eq.'C')then
                B(K:K)=','
              elseif(delm.eq.'N')then
                K=K-1
              endif
            endif
            CL=C
            goto 99
          else
            K=K+1
            B(K:K)=A(I:I)
            CL=A(I:I)
          endif
        endif
 99   CONTINUE

C If the last character is a separator replace it with a blank
C (so trailing commas and tabs are not written out).
      lastb=max(1,lnblnk(b))
      C=B(lastb:lastb)
      if(delm.eq.'T'.and.ICHAR(C).EQ.9)then
        B(lastb:lastb)=' '
      elseif(delm.eq.'S')then
        continue
      elseif(delm.eq.'C'.and.C.eq.',')then
        B(lastb:lastb)=' '
      endif

      END

C ************************ PHELPD
C PHELPD displays the current contents of common pophelp in a form
C appropriate to the current terminal type. MSG is a short descriptive
C string for the subject. NHELP is the number of lines to be displayed.
C IBX & IBY are the preferred coords of the lower left corner if in
C terminal type 8.
      SUBROUTINE PHELPD(MSG,NHELP,OTHER,IBX,IBY,IER)

      COMMON/SPAD/MMOD,LIMIT,LIMTTY
      common/OUTIN/IUOUT,IUIN
      COMMON/FILEP/IFIL
      common/pophelp/h(60)
      dimension dh(60),lh(30),exh(60)
      CHARACTER*(*) MSG,OTHER
      CHARACTER h*72,outs*73,dh*72,lh*73,exh*72
      character tut_path*25,ltut*72,IA*3
      logical extra,XST
      integer ltf    ! last actual character in the string.

C dh is duplicate help list, lh is the list (page) to be displayed.
C exh is 'show me more' text array read in from external file.
      extra = .false.
      if(OTHER(1:1).ne.'-')then
        tut_path='/usr/esru/tutorial/'
        write(LTUT,'(A,A)') tut_path(1:lnblnk(tut_path)),'esp-r.tut'
        iftut=IFIL-1
        if(iftut.NE.0)CALL ERPFREE(iftut,ISTAT)
        XST=.FALSE.
        ltf=max(1,LNBLNK(ltut))
        INQUIRE (FILE=ltut(1:ltf),EXIST=XST)
        if(XST)then
          open(iftut,FILE=ltut(1:lnblnk(ltut)),ACCESS='SEQUENTIAL',
     &      STATUS='OLD',IOSTAT=ISTAT)
          if(ISTAT.eq.0)then
            extra = .true.
          else
            extra = .false.
          endif
        endif
        CALL ERPFREE(IFTUT,ISTAT)
      endif

 41   IER=0
      IF(NHELP.eq.0)THEN
        IF(MMOD.EQ.8)THEN
          if(LNBLNK(MSG).le.50)then
            LN=max(1,LNBLNK(MSG))
            WRITE(H(1),64,iostat=ios,err=1)MSG(1:LN)
          else
            WRITE(H(1),64,iostat=ios,err=1)MSG(1:50)
          endif
          CALL PHELPW(1,IHW,IER)
          call dupphelp(dh)
          call updhelp(dh,1,IHW)
          ipflg=0
          CALL egphelp(IBX,IBY,IPFLG,0,iuresp)
        ELSE
          call edisp(iuout,' ')
          if(LNBLNK(MSG).le.50)then
            LN=max(1,LNBLNK(MSG))
            WRITE(outs,64,iostat=ios,err=1)MSG(1:LN)
          else
            WRITE(outs,64,iostat=ios,err=1)MSG(1:50)
          endif
          call edisp(iuout,outs)
          call edisp(iuout,' ')
        ENDIF
        return
      ENDIF

      lsmn=MIN0(lnblnk(msg),67)
      lsmn=max(1,LSMN)
      write(outs,'(a,a)',iostat=ios,err=1)'HELP: ',msg(1:lsmn)
      call tstamp('>',outs)
      ILEN=NHELP
      call HPAGE('create',ILEN,MIFULL,MFULL,IST,IPM,MPM,IPFLG)
      IF(MMOD.EQ.8)THEN
        CALL PHELPW(NHELP,IHW,IER)
        call dupphelp(dh)
        call updhelp(dh,NHELP,IHW)
        if(extra)then
          CALL egphelp(IBX,IBY,IPFLG,1,iuresp)
          if(iuresp.eq.1)then
            call SHOWMEH(IFTUT,LTUT,OTHER,nhi,ihw2,exh,IER)
            call updhelp(exh,nhi,ihw2)
            CALL egphelp(IBX,IBY,IPFLG,0,iuresp)
          endif
        else
          CALL egphelp(IBX,IBY,IPFLG,0,iuresp)
        endif
      ELSE

C Loop through the items until the page to be displayed. M is the
C current line index.
   42   m=0
        DO 51 I=1,ILEN
          IF(I.GE.IST.AND.(I.LE.(IST+MIFULL)))THEN
            M=M+1
            LN=max(1,LNBLNK(H(I)))
            WRITE(lh(m),'(1X,A)',iostat=ios,err=1) H(I)(1:LN)
          ENDIF
   51   CONTINUE
        if(IPFLG.eq.1)then
          M=M+1
          WRITE(lh(m),'(A)')' ___________________________'
          M=M+1
          WRITE(lh(m),116)IPM,MPM
  116     FORMAT (' Page --- Part: ',I2,' of ',I2,' ---')
        endif
        DO 52 J=1,M
          LN=max(1,LNBLNK(lh(J)))
          WRITE(outs,'(A)',iostat=ios,err=1) lh(J)(1:LN)
          CALL edisp(iuout,outs)
   52   CONTINUE
        if(IPFLG.eq.0)then
          if(extra)then

C Use text based dialog functions to avoid recursion.
            call edisp(iuout,'Show additional information (y/n) ?')
            READ(IUIN,'(A3)',IOSTAT=IOS,ERR=1)IA
            if(IA(1:1).eq.'y'.or.IA(1:1).eq.'Y')iw1=4
            if(IA(1:1).eq.'n'.or.IA(1:1).eq.'N')return
          else
            return
          endif
        else
          if(IPM.eq.1)then
            if(extra)then
              call edisp(iuout,
     &        ' Options:  a) -, b) next, c) continue, e) show me more')
            else
              call edisp(iuout,
     &        ' Options:  a) -, b) next, c) continue ')
            endif
            READ(IUIN,'(A3)',IOSTAT=IOS,ERR=1)IA
            if(IA(1:1).eq.'a'.or.IA(1:1).eq.'A')iw1=1
            if(IA(1:1).eq.'b'.or.IA(1:1).eq.'B')iw1=2
            if(IA(1:1).eq.'c'.or.IA(1:1).eq.'C')iw1=3
            if(IA(1:1).eq.'e'.or.IA(1:1).eq.'E')iw1=4
          elseif(IPM.eq.MPM)then
            if(extra)then
              call edisp(iuout,
     &    ' Options:  a) previous, b) -, c) continue, e) show me more')
            else
              call edisp(iuout,
     &        ' Options:  a) previous, b) -, c) continue ')
            endif
            READ(IUIN,'(A3)',IOSTAT=IOS,ERR=1)IA
            if(IA(1:1).eq.'a'.or.IA(1:1).eq.'A')iw1=1
            if(IA(1:1).eq.'b'.or.IA(1:1).eq.'B')iw1=2
            if(IA(1:1).eq.'c'.or.IA(1:1).eq.'C')iw1=3
            if(IA(1:1).eq.'e'.or.IA(1:1).eq.'E')iw1=4
          else
            if(extra)then
              call edisp(iuout,
     & ' Options:  a) previous, b) next, c) continue, e) show me more')
            else
              call edisp(iuout,
     &        ' Options:  a) previous, b) next, c) continue ')
            endif
            READ(IUIN,'(A3)',IOSTAT=IOS,ERR=1)IA
            if(IA(1:1).eq.'a'.or.IA(1:1).eq.'A')iw1=1
            if(IA(1:1).eq.'b'.or.IA(1:1).eq.'B')iw1=2
            if(IA(1:1).eq.'c'.or.IA(1:1).eq.'C')iw1=3
            if(IA(1:1).eq.'e'.or.IA(1:1).eq.'E')iw1=4
          endif
        endif
        if(iw1.eq.1)then
          call HPAGE('prev',ILEN,MIFULL,MFULL,IST,IPM,MPM,IPFLG)
          call edisp(iuout,'  ')
          goto 42
        elseif(iw1.eq.2)then
          call HPAGE('next',ILEN,MIFULL,MFULL,IST,IPM,MPM,IPFLG)
          call edisp(iuout,'  ')
          goto 42
        elseif(iw1.eq.3)then
          call edisp(iuout,'  ')
          return
        elseif(iw1.eq.4)then

C Recover up to 60 lines of extern text.
          call SHOWMEH(IFTUT,LTUT,OTHER,nhi,ihw,exh,IER)
          ILEN=nhi
          call HPAGE('create',ILEN,MIFULL,MFULL,IST,IPM,MPM,IPFLG)

C Loop through the extra items until the page to be displayed. M is the
C current line index.
  142     m=0
          DO 151 I=1,ILEN
            IF(I.GE.IST.AND.(I.LE.(IST+MIFULL)))THEN
              M=M+1
              WRITE(lh(m),'(1X,A)',iostat=ios,err=1) exh(I)(1:ihw)
            ENDIF
  151     CONTINUE
          if(IPFLG.eq.1)then
            M=M+1
            WRITE(lh(m),'(A)')' ____________________________'
            M=M+1
            WRITE(lh(m),216)IPM,MPM
  216       FORMAT (' Extra --- Part: ',I2,' of ',I2,' ---')
          endif
          DO 152 J=1,M
            LN=max(1,LNBLNK(lh(J)))
            WRITE(outs,'(A)',iostat=ios,err=1) lh(J)(1:LN)
            CALL edisp(iuout,outs)
  152     CONTINUE
          if(IPFLG.eq.0)then
            call edisp(iuout,'  ')
            goto 41
          else
            if(IPM.eq.1)then
              call edisp(iuout,' Options: a) -, b) next, c) continue')
              READ(IUIN,'(A3)',IOSTAT=IOS,ERR=1)IA
              if(IA(1:1).eq.'a'.or.IA(1:1).eq.'A')iw2=1
              if(IA(1:1).eq.'b'.or.IA(1:1).eq.'B')iw2=2
              if(IA(1:1).eq.'c'.or.IA(1:1).eq.'C')iw2=3
            elseif(IPM.eq.MPM)then
              call edisp(iuout,
     &            ' Options: a) previous, b) -, c) continue')
              READ(IUIN,'(A3)',IOSTAT=IOS,ERR=1)IA
              if(IA(1:1).eq.'a'.or.IA(1:1).eq.'A')iw2=1
              if(IA(1:1).eq.'b'.or.IA(1:1).eq.'B')iw2=2
              if(IA(1:1).eq.'c'.or.IA(1:1).eq.'C')iw2=3
            else
              call edisp(iuout,
     &          ' Options: a) previous, b) next, c) continue')
              READ(IUIN,'(A3)',IOSTAT=IOS,ERR=1)IA
              if(IA(1:1).eq.'a'.or.IA(1:1).eq.'A')iw2=1
              if(IA(1:1).eq.'b'.or.IA(1:1).eq.'B')iw2=2
              if(IA(1:1).eq.'c'.or.IA(1:1).eq.'C')iw2=3
            endif
            if(iw2.eq.1)then
              call HPAGE('prev',ILEN,MIFULL,MFULL,IST,IPM,MPM,IPFLG)
              call edisp(iuout,'  ')
              goto 142
            elseif(iw2.eq.2)then
              call HPAGE('next',ILEN,MIFULL,MFULL,IST,IPM,MPM,IPFLG)
              call edisp(iuout,'  ')
              goto 142
            elseif(iw2.eq.3)then
              call edisp(iuout,'  ')
              goto 41
            endif
            goto 41
          endif
        endif
      endif
   64 FORMAT('No help available for ',a,'.')
      RETURN

   1  write(6,*) 'EHELPD: error writing help message '
      return
      END

C ******** HPAGE
C Control paging of pop-up help.
      SUBROUTINE HPAGE(act,ILEN,MIFULL,MFULL,IST,IPM,MPM,IPFLG)
      COMMON/SPAD/MMOD,LIMIT,LIMTTY
      character*(*) act

      if(act(1:6).eq.'create')then
        IST=1
        MFULL=LIMTTY-2
        MCTL = 2
        MIFULL=MFULL-MCTL
        IF(ILEN.LE.MIFULL)THEN
          IPFLG=0
          IPM = 1
          MPM = 1
        ELSE
          IPFLG=1
          PAGE=(FLOAT(IST+MIFULL-1)/FLOAT(MIFULL))
          IF(PAGE.LT.1.0)PAGE=1.0
          IPM=INT(PAGE)
          PAGE=(FLOAT(ILEN)/FLOAT(MIFULL))
          IF(PAGE.LT.1.0)PAGE=1.0
          IF((PAGE-AINT(PAGE)).GT.0.0)PAGE=AINT(PAGE)+1.0
          MPM=INT(PAGE)
        ENDIF
        return
      elseif(act(1:4).eq.'next')then
        IF((IST+MIFULL).LT.ILEN)IST=IST+MIFULL
         PAGE=(FLOAT(IST+MIFULL-1)/FLOAT(MIFULL))
         IF(PAGE.LT.1.0)PAGE=1.0
         IPM=INT(PAGE)
         PAGE=(FLOAT(ILEN)/FLOAT(MIFULL))
         IF(PAGE.LT.1.0)PAGE=1.0
         IF((PAGE-AINT(PAGE)).GT.0.0)PAGE=AINT(PAGE)+1.0
         MPM=INT(PAGE)
         return
      elseif(act(1:4).eq.'prev')then
        IF(IPM.GT.1)IST=IST-MIFULL
        PAGE=(FLOAT(IST+MIFULL-1)/FLOAT(MIFULL))
        IF(PAGE.LT.1.0)PAGE=1.0
        IPM=INT(PAGE)
        PAGE=(FLOAT(ILEN)/FLOAT(MIFULL))
        IF(PAGE.LT.1.0)PAGE=1.0
        IF((PAGE-AINT(PAGE)).GT.0.0)PAGE=AINT(PAGE)+1.0
        MPM=INT(PAGE)
        return
      endif

      return
      end

C ************************ SHOWMEH
C SHOWMEH extracts additional help text matching the key OTHER from
C an external file (LTUT) and returns it in exh along with the
C number of lines (nhi) and their maximum width (ihw).
      SUBROUTINE SHOWMEH(IFTUT,LTUT,OTHER,nhi,ihw,exh,IER)

      common/OUTIN/IUOUT,IUIN
      dimension exh(60)
      CHARACTER*(*) OTHER
      CHARACTER exh*72,ltut*72,TOPIK*17,outstr*124
      logical XST
      integer ltf  ! position of last character in the string.

C Regenerate the topic string to match.
      TOPIK=' '
      WRITE(TOPIK,'(A5,A)')'*tut_',OTHER(1:12)
      if(iftut.NE.0)CALL ERPFREE(iftut,ISTAT)
      XST=.FALSE.
      ltf=max(1,LNBLNK(ltut))
      INQUIRE (FILE=ltut(1:ltf),EXIST=XST)
      if(XST)then
        open(iftut,FILE=ltut(1:lnblnk(ltut)),ACCESS='SEQUENTIAL',
     &    STATUS='OLD',IOSTAT=IOS)
        if(IOS.ne.0)goto 82
      endif

C Loop until the topic found or end of file encountered.
C If the first character is a # then read the next line from the file.
      IIER=0
    8 READ(iftut,10,IOSTAT=ios,ERR=2,END=102)OUTSTR
   10 FORMAT(A124)
      IF(OUTSTR(1:1).EQ.'#')THEN
        GOTO 8
      ELSE

C Look for particular topic and read/display until next "*tut_".
        IF(OUTSTR(1:17).EQ.TOPIK)THEN
          nhi = 0
          IHW=0
   85     READ(iftut,10,IOSTAT=IOS,ERR=2,END=102)OUTSTR
          IF(OUTSTR(1:1).EQ.'#')then
            GOTO 85
          elseif(OUTSTR(1:5).eq.'*tut_')THEN
            GOTO 82
          else
            nhi = nhi +1
            if(nhi.lt.60)then
              WRITE(exh(nhi),'(A)',iostat=ios,err=1)OUTSTR(1:72)
              IX=max(1,LNBLNK(exh(nhi)))
              IF(IX.GT.IHW)IHW=IX
              GOTO 85
            else
              goto 82
            endif
          endif
        ELSE
          GOTO 8
        ENDIF
      endif
   82 call edisp(iuout,' ')
      CALL ERPFREE(IFTUT,ISTAT)
      return

   1  write(6,*) 'showmeh: error writing additional help messages. '
      return
   2  write(6,*) 'showmeh: error writing show me more message '
      return

  102 IER=0
      call edisp(iuout,' End of supplemental information file. ')
      CALL ERPFREE(IFTUT,ISTAT)
      return
      END


C ************************ dupphelp
C dupphelp copies current pop-uphelp common into a string array so that
C C code does not destroy it.
      SUBROUTINE dupphelp(dh)

      common/pophelp/h(60)
      dimension dh(60)
      CHARACTER H*72,dh*72

      DO 50 I=1,60
        last=max(LNBLNK(h(I)),1)
        WRITE(dh(i),'(A)',iostat=ios,err=1)h(I)(1:last)
   50 CONTINUE
      RETURN
   1  write(6,*) 'dupphelp: error copying help line. '
      return
      END

C ********************* PHELPW
C PHELPW returns the width IWH of the longest text string in the common
C pophelp where IL is the number of lines of help under consideration.
      SUBROUTINE PHELPW(IL,IHW,IER)
      common/pophelp/h(60)
      CHARACTER h*72
      IER=0
      IHW=0
      IF(IL.GT.0)THEN
        DO 10 I=1,IL
          IX=max(1,LNBLNK(h(I)))
          IF(IX.GT.IHW)IHW=IX
  10    CONTINUE
      ENDIF
      RETURN
      END

C ********* UPDVIEW
C UPDVIEW is called from C code in wwlib.c with values to update common
C blocks GFONT, VIEWPX and SPAD. Set REFRESH=true.  Set MODIFY=true
C to force any wire-frame images to be redrawn.
      subroutine updview(ifsc,itfsc,imfsc,ilc,irc,itc,ibc,iwc,ihc,lttyc)
      COMMON/GFONT/IFS,ITFS,IMFS
      COMMON/VIEWPX/menuchw,igl,igr,igt,igb,igw,igwh
      COMMON/SPAD/MMOD,LIMIT,LIMTTY
      COMMON/FRESH/refrsh
      COMMON/RAY3/MODIFY,MODLEN,MODBND
      LOGICAL MODIFY,MODLEN,MODBND
      logical refrsh

      IFS=ifsc
      ITFS=itfsc
      IMFS=imfsc
      igl=ilc
      igr=irc
      igt=itc
      igb=ibc
      igw=iwc
      igwh=ihc
      LIMTTY=lttyc
      LIMIT=1
      refrsh=.TRUE.
      MODIFY=.TRUE.

      return
      end

C ********* LISTAS
C General read of an ascii file.
      SUBROUTINE LISTAS(iunit,LFIL,IER)
      COMMON/OUTIN/IUOUT,IUIN
      character LFIL*72,outstr*124
      logical XST
      integer ltf   ! position of last character in the string.

C List out an ASCII file, if file name begins with "/" open
C it directly.

C << does not yet test for Windows path >>
      if(LFIL(1:1).eq.'/')then
        ltf=max(1,LNBLNK(LFIL))
        INQUIRE (FILE=LFIL(1:ltf),EXIST=XST)
        IF (XST) THEN
          LN=max(1,lnblnk(LFIL))
          OPEN (IUNIT,FILE=LFIL(1:LN),ACCESS='SEQUENTIAL',
     &              STATUS='OLD',IOSTAT=ISTAT)
        else
          call usrmsg('Problem detected while trying to open',LFIL,'W')
          return
        endif
      else
        CALL EFOPSEQ(IUNIT,LFIL,1,IER)
      endif
      IF(IER.LT.0)THEN
        call usrmsg(' Problem detected while trying to open',LFIL,'W')
        return
      ENDIF

C Actual read of ASCII file.
    7 READ(IUNIT,10,IOSTAT=IOS,END=102)OUTSTR
   10 FORMAT(A124)
      CALL  EDISP(IUOUT,OUTSTR)
      goto 7

C End of file encountered, close the file and return to menu.
  102 CALL USRMSG(' ',' End of file reached!','-')
      CALL ERPFREE(IUNIT,ISTAT)
      return
      end

C ************* isadll
C Find if module is being used as a dll (silent running).
      subroutine isadll(yes)
      logical yes

C NOTE: Edit as required for machine environment, normally
C will return false.
      yes = .false.
      return
      end

C ************* isunix
C Find if machine is unix.
C NOTE: uses compiler variable -DMINGW to signal .false.
      subroutine isunix(yes)
      logical yes

C [Unix or Linux or Cygwin gets true].
      yes = .true.
#ifdef MINGW
      yes = .false.
#endif
      return
      end

C ************* usrhome
C Find users home directory.
      subroutine usrhome(upath)
      COMMON/OUTIN/IUOUT,IUIN
      character*(*) upath
      character homedrive*24,homepath*48,outs*124
      logical unixok

C Find if Unix or NT.
      call isunix(unixok)
      if(unixok)then
        call getenv('HOME',upath)
      else
        call getenv ('HOMEDRIVE',homedrive)
        write(outs,*) 'homedrive is ',homedrive
        call edisp(IUOUT,outs)
        call getenv ('HOMEPATH',homepath)
        write(outs,*) 'homepath is ',homepath
        call edisp(IUOUT,outs)
        if(homedrive(1:1).ne.' '.and.homepath(1:1).ne.' ')then
          write(upath,'(2a)') homedrive(1:lnblnk(homedrive)),
     &      homepath(1:lnblnk(homepath))
        else
          upath = 'c:/esru'
        endif
      endif
      return
      end

C ************* usrname
C Find users name.
      subroutine usrname(uname)
      character*(*) uname
      logical unixok

C Find if Unix or NT.
      call isunix(unixok)
      if(unixok)then
        call getenv('USER',uname)
      else
        uname = 'user'
      endif
      return
      end

C ************* usrdir
C Find current folder.
      subroutine usrdir(upwd)
      character*(*) upwd
      integer getcwd

      i= getcwd(upwd)
      if(i.ne.0)write(6,*) 'problem recovering current folder'
      return
      end

C ************* esppid
C Find current process number.
      subroutine esppid(ipid)
      integer getpid
      logical unixok

C Find if Unix or NT.
      call isunix(unixok)
      if(unixok)then
        ipid = getpid()
      else
        ipid = 999
      endif
      return
      end

C ****** Date stamp routine.
      subroutine tstamp(act,msg)
      common/OUTIN/IUOUT,IUIN
      common/journopt/journio,iuj,journcmd,jfile

      character*(*) msg
      character dstmp*24,act*1,outs*124,journcmd*20,jfile*72
      logical there

      call dstamp(dstmp)
      if(act.eq.'-')then
        write(outs,'(A,A)')' Current time is : ',dstmp
        call edisp(iuout,outs)
      elseif(act.eq.'m')then
        LN=max(1,lnblnk(msg))
        write(outs,'(3A)')msg(1:LN),' @ ',dstmp
        call edisp(iuout,outs)
      elseif(act.eq.'>')then

C If there is a journal file append the current message to it and close.
        LN=max(1,lnblnk(msg))
        write(outs,'(3A)')msg(1:LN),' @ ',dstmp
        ltf=max(1,LNBLNK(jfile))
        if(journio.eq.1.and.
     &    (jfile(1:2).ne.'  '.and.jfile(1:4).ne.'UNKN'))then
          there=.false.
          INQUIRE(FILE=jfile(1:ltf),EXIST=there)
          if(there)then
            close(iuj)
#ifdef F90
            open(iuj,file=jfile(1:ltf),position='APPEND',
     &        status='UNKNOWN',err=1)
#else
            open(iuj,file=jfile(1:ltf),ACCESS='APPEND',status='UNKNOWN',
     &        err=1)
#endif
            LN=max(1,lnblnk(outs))
            write(iuj,'(a)',iostat=ios,err=2)outs(1:LN)
            close(iuj)
          endif
        endif
      endif

      return
   1  call edisp(iuout,'Error opening journal file, continuing.')
      return
   2  call edisp(iuout,'Error updating journal file, continuing.')
      close(iuj)
      return
      end

C ****** Define ectime function as fixed length character*24
      character*24 FUNCTION ECTIME(ICTIME)
      integer ICTIME
#ifdef GCC4
      call CTIME(ICTIME,ECTIME)
#else
      character*24 CTIME
      ECTIME = CTIME(ICTIME)
#endif
      return
      end

C ****** Get date stamp in the form: Fri Jan 23 09:34:31 1998.
C Used to isolate code from system details.
      subroutine dstamp(date_str)

      integer time
      character date_str*24
      character ectime*24

C Unix date function.
      ictime=time()
      write(date_str,'(A)',iostat=ios,err=1) ectime(ictime)
      return
   1  write(6,*) 'dstamp: error getting time. '
      return
      end

C ****** comparedate
C comparedate is passed two date strings (generated by call to dstamp).
C act is requested action '?' is ??
C dif is positive if datea is more current than dateb.
      subroutine comparedate(datea,dateb,act,idif)
      character datea*24,dateb*24,act*1,datetest*24
      character word*24

C Setup arrays of indices to hold the day of the week (1-7), the month
C (1-12), the day of the month (1-31), the hour of the day (1-24), the
C minute of the hour (1-60) and the year.
      dimension imon(2),idom(2),ihrod(2),imohr(2),iyears(2)
      dimension isohr(2)

C Note datea and dateb are expeced to be in the form of:
C Fri Oct 17 11:13:16 2003.
C Scan each date in turn.
      do 42 i=1,2
        if(i.eq.1)datetest=datea
        if(i.eq.2)datetest=dateb
        K=0
        CALL EGETW(datetest,K,WORD,'W','day of week',IFLAG)
        CALL EGETW(datetest,K,WORD,'W','month name',IFLAG)
        if(word(1:3).eq.'Jan')then
          imon(i)=1
        elseif(word(1:3).eq.'Feb')then
          imon(i)=2
        elseif(word(1:3).eq.'Mar')then
          imon(i)=3
        elseif(word(1:3).eq.'Apr')then
          imon(i)=4
        elseif(word(1:3).eq.'May')then
          imon(i)=5
        elseif(word(1:3).eq.'Jun')then
          imon(i)=6
        elseif(word(1:3).eq.'Jul')then
          imon(i)=7
        elseif(word(1:3).eq.'Aug')then
          imon(i)=8
        elseif(word(1:3).eq.'Sep')then
          imon(i)=9
        elseif(word(1:3).eq.'Oct')then
          imon(i)=10
        elseif(word(1:3).eq.'Nov')then
          imon(i)=11
        elseif(word(1:3).eq.'Dec')then
          imon(i)=12
        endif
        CALL EGETWI(datetest,K,idom(i),1,31,'W','day of month',IER)

C Parse time phrase in the form 11:13:16
        CALL EGETW(datetest,K,WORD,'W','time phrase',IFLAG)
        read(WORD(1:2),*,IOSTAT=IOS,ERR=1002)ihrod(i)
        read(WORD(4:5),*,IOSTAT=IOS,ERR=1003)imohr(i)
        read(WORD(7:8),*,IOSTAT=IOS,ERR=1004)isohr(i)
        CALL EGETWI(datetest,K,iyears(i),1970,2010,'-','year',IER)
  42  continue

C First compare the years. If the same compare the months.
      diffhours = 0.0
      if(iyears(1).eq.iyears(2))then
        if(imon(1).eq.imon(2))then

C Year and month are the same, compare the day of the month.
          if(idom(1).eq.idom(2))then

C Year, month, day-of-month are the same, compare the hour of the day.
            if(ihrod(1).eq.ihrod(2))then

C Compare the minute of the hour.
              if(imohr(1).eq.imohr(2))then

C Compare the second of the minute.
                if(isohr(1).eq.isohr(2))then
                  idif = 0
                elseif(isohr(1).gt.isohr(2))then
                  idif = 1
                elseif(isohr(1).lt.isohr(2))then
                  idif = -1
                endif
              elseif(imohr(1).gt.imohr(2))then
                idif = 1
              elseif(imohr(1).lt.imohr(2))then
                idif = -1
              endif
            elseif(ihrod(1).gt.ihrod(2))then
              idif = 1
            elseif(ihrod(1).lt.ihrod(2))then
              idif = -1
            endif
          elseif(idom(1).gt.idom(2))then
            idif = 1
          elseif(idom(1).lt.idom(2))then
            idif = -1
          endif
        elseif(imon(1).gt.imon(2))then
          idif = 1
        elseif(imon(1).lt.imon(2))then
          idif = -1
        endif
      elseif(iyears(1).gt.iyears(2))then
        idif = 1
      elseif(iyears(1).lt.iyears(2))then
        idif = -1
      endif
      return

 1002 call edisp(6,'comparedate: error extracting hour.')
      return
 1003 call edisp(6,'comparedate: error extracting minute.')
      return
 1004 call edisp(6,'comparedate: error extracting second.')
      return
      end

C ****** Get computer clock seconds.
C Used to isolate fortran code from system specifics.
C Edit for machine type.
      subroutine getsecs(ictime)
      integer time

      ictime=time()
      return
      end

C ************* runit
C Execute a command string, in text (runs in a new xterm) or graphics
C mode.  Assumes that command string terminates in a '&' if the
C user wishes to run in background mode. For minGW xterm is not
C used.
      subroutine runit(cmd,tg)
      common/OUTIN/IUOUT,IUIN
      character*(*) cmd,tg
      character tmp*248,tmode*4
      integer system
      logical unixok

C Find if Unix or NT.
      call isunix(unixok)
      if(unixok)then
        LN=max(1,lnblnk(tg))
        LN=min0(LN,4)
        write (tmode,'(a)') tg(1:LN)
        if(tmode(1:4).eq.'text')then
          LN=max(1,lnblnk(cmd))
          write(tmp,'(a,a)') 'xterm -e ',cmd(1:LN)
          i = system(tmp)
        else
          i = system(cmd)
        endif
        if(i.gt.0.and.i.le.126)then
          call edisp(iuout,'task terminated with an error condition')
        elseif(i.eq.127)then
          call edisp(iuout,'task requested was unknown.')
        endif
        return
      else
        LN=lnblnk(cmd)
        if (cmd(LN:LN).eq."&") then
          write (tmp,'(2a)') 'start ',cmd(1:LN-1)
          write (cmd,'(a)') tmp(1:lnblnk(tmp))
        endif
        i = system(cmd)
        if(i.gt.0.and.i.le.126)then
          call edisp(iuout,'task terminated with an error condition')
        elseif(i.eq.127)then
          call edisp(iuout,'task requested was unknown.')
        endif
C        call cissue(cmd,ifail)
        return
      endif
      end

C ***************** iEGetArrW
C iEGetArrW reads all of the words in a 248 character string, and populates a character
C array with each word. Words are separated by blanks, commas, or tab: WORD,WORD,WORD
C or WORD WORD WORD or WORD, WORD, WORD  are all valid.
      integer function iEGetArrW(cString,cWORDS)
      implicit none

      character*248 cString, cWords(124)
      character*1 cChar
      logical bWord
      integer iStrLoc, iWordLoc, iEnd, iWordCount, iEr
      parameter (iEnd=248)

C Empty word array
      do iWordCount = 1, 124
        cWords(iWordCount) = ' '
      enddo

C Reset word counter
      iWordCount = 0

C Loop through string
      do iStrLoc = 1, iEnd

C Get current character
        cChar = cString(iStrLoc:iStrLoc)

C If string is a word separation character, move on
        if ( cChar .eq. ' ' .or.
     &       cChar .eq. ',' .or.
     &       iChar(cChar) .eq. 9 ) then

           bWord = .false.
        else

C is this a new word?
          if ( .not. bWord ) then

            bWord = .true.
            iWordCount = iWordCount + 1
            iWordLoc = 1

          endif

C Copy current character into word buffer
          cWords(iWordCount)(iWordLoc:iWordLoc) = cChar

          iWordLoc = iWordLoc + 1

        endif

      enddo

      iEGetArrW = iWordCount

      return
      end

C ***************** EGETW
C EGETW gets first WORD after position K from the STRING of
C characters. Words are separated by blanks, commas, or tab: WORD,WORD,WORD
C or WORD WORD WORD or WORD, WORD, WORD  are all valid.  Provides a warning
C message if ACT='W', a failure message if ACT='F' and does
C no message if ACT='-'.  Modified after:
C G.N. Walton, US Nat. Institute of Standards and Technology
C     LS     - maximum length of STRING
C     L      - current position in WORD
C     LW     - maximum length of WORD

      SUBROUTINE EGETW(STRING,K,WORD,ACT,MSG,IER)
#include "espriou.h"
C espriou.h provides currentfile.
      common/OUTIN/IUOUT,IUIN

      CHARACTER*(*) WORD, STRING, MSG
      CHARACTER ACT*1,A*1,outstr*124

      IER=0
      WORD=' '
      LS=LEN(STRING)
      LW=LEN(WORD)
      L=0

C Start by skipping blanks and tabs before the word.
   10 K=K+1
      IF(K.GT.LS) GOTO 999
      A=STRING(K:K)
      IF(A.EQ.' '.OR.A.EQ.','.OR.(ICHAR(A).EQ.9)) GOTO 10

C Copy WORD from STRING, character by character until separator found.
   20 L=L+1
      IF(L.GT.LW) GOTO 100
      WORD(L:L)=A
      K=K+1
      IF(K.GT.LS) GOTO 999
      A=STRING(K:K)
      IF(A.EQ.' '.OR.A.EQ.','.OR.(ICHAR(A).EQ.9)) GO TO 100
      GOTO 20

  100 RETURN

  999 ier=1
      if(ACT.EQ.'-')then
        RETURN
      elseif(ACT.EQ.'W')then
        LN=max(1,lnblnk(currentfile))
        LNM=max(1,lnblnk(MSG))
        if(currentfile(1:2).ne.'  ')then
          WRITE(OUTSTR,'(5a)',IOSTAT=IOS,ERR=1)
     &      'WARNING: in ',currentfile(1:LN),
     &      ' past end of line for ',MSG(1:LNM),' in...'
        else
          WRITE(OUTSTR,'(3a)',IOSTAT=IOS,ERR=1)
     &      'WARNING: Past end of line for ',
     &      MSG(1:LNM),' (word) in...'
        endif
      elseif(ACT.EQ.'F')then
        ier=1
        LN=max(1,lnblnk(currentfile))
        LNM=max(1,lnblnk(MSG))
        if(currentfile(1:2).ne.'  ')then
          WRITE(OUTSTR,'(5a)',IOSTAT=IOS,ERR=1)
     &      'FAILURE: in ',currentfile(1:LN),
     &      ' past end of line for ',MSG(1:LNM),' in...'
        else
          WRITE(OUTSTR,'(3a)',IOSTAT=IOS,ERR=1)
     &      'FAILURE: Past end of line for ',
     &      MSG(1:LNM),' (word) in...'
        endif
      endif
      CALL EDISP(iuout,OUTSTR)
      if(lnblnk(STRING).gt.123)then
        write(6,*) STRING
      else
        CALL EDISP(iuout,STRING)
      endif
      GOTO 100
   1  write(6,*) 'EGETW: error writing warning re: ',STRING

      END

C ***************** EGETP
C EGETP gets first PHRASE after position K from the STRING of
C characters. Phrases are separated by tabs or commas. Provides a warning
C message if ACT='W', a failure message if ACT='F' and does
C no message if ACT='-'.
      SUBROUTINE EGETP(STRING,K,PHRASE,ACT,MSG,ier)
#include "espriou.h"
      common/OUTIN/IUOUT,IUIN

      CHARACTER*(*) PHRASE, STRING, MSG
      CHARACTER ACT*1,A*1,outstr*124

C LS is th maximum length of STRING, L the current position,
C LW the maximum length of PHRASE.
      ier=0
      PHRASE=' '
      LS=LEN(STRING)
      LW=LEN(PHRASE)
      L=0

C Start by skipping blanks and tabs before the PHRASE.
   10 K=K+1
      IF(K.GT.LS) GOTO 999
      A=STRING(K:K)
      IF(A.EQ.' '.OR.A.EQ.','.OR.(ICHAR(A).EQ.9)) GOTO 10

C Copy PHRASE from STRING, character by character until tab or a
C comma is found.
   20 L=L+1
      IF(L.GT.LW) GOTO 100
      PHRASE(L:L)=A
      K=K+1
      IF(K.GT.LS) GOTO 999
      A=STRING(K:K)
      IF(ICHAR(A).EQ.9.or.A.eq.',') GO TO 100
      GOTO 20

  100 RETURN

  999 if(ACT.EQ.'-')then
        RETURN
      elseif(ACT.EQ.'W')then
        ier=1
        LN=max(1,lnblnk(currentfile))
        LNM=max(1,lnblnk(MSG))
        if(currentfile(1:2).ne.'  ')then
          WRITE(OUTSTR,'(5a)',IOSTAT=IOS,ERR=1)
     &      'WARNING: in ',currentfile(1:LN),
     &      ' past end of line for ',MSG(1:LNM),' (phrase) in...'
        else
          WRITE(OUTSTR,'(3a)',IOSTAT=IOS,ERR=1)
     &      'WARNING: Past end of line for ',
     &      MSG(1:LNM),' (phrase) in...'
        endif
      elseif(ACT.EQ.'F')then
        LN=max(1,lnblnk(currentfile))
        LNM=max(1,lnblnk(MSG))
        if(currentfile(1:2).ne.'  ')then
          WRITE(OUTSTR,'(5a)',IOSTAT=IOS,ERR=1)
     &      'FAILURE: in ',currentfile(1:LN),
     &      ' past end of line for ',MSG(1:LNM),' (phrase) in...'
        else
          WRITE(OUTSTR,'(3a)',IOSTAT=IOS,ERR=1)
     &      'FAILUREW: Past end of line for ',
     &      MSG(1:LNM),' (phrase) in...'
        endif
      endif
      CALL EDISP(iuout,OUTSTR)
      if(lnblnk(STRING).gt.123)then
        write(6,*) STRING
      else
        CALL EDISP(iuout,STRING)
      endif
      GOTO 100
   1  write(6,*) 'EGETP: error writing warning re: ',STRING

      END

C ***************** EGETWI
C EGETWI gets first word after position K from the STRING of
C characters and converts it into an integer IV, tests it against
C the minimum MN and the maximum MX and provides a warning
C message if ACT='W', a failure message if ACT='F' and does
C no range checking if ACT='-'. Words may be separated by blanks,
C commas, or tab: WORD,WORD,WORD or WORD WORD WORD or WORD, WORD, WORD
C are all valid.
      SUBROUTINE EGETWI(STRING,K,IV,MN,MX,ACT,MSG,IER)
#include "espriou.h"
      common/OUTIN/IUOUT,IUIN

      CHARACTER*(*) STRING, MSG
      CHARACTER ACT*1,STR1*10,STR2*10,WORD*20,OUTSTR*124

C Pick up line and lenght for error messages.
      ils=max(1,lnblnk(STRING))
      if(ils.gt.105)ils=105

      IER=0
      WORD=' '
      CALL EGETW(STRING,K,WORD,'-','integer',IER)
      IF(IER.NE.0)RETURN
      read(WORD,*,IOSTAT=IOS,ERR=1002)iv

C If range checking disabled, jump out of routine.
      IF(ACT.EQ.'-')RETURN

C Make up reporting string.
      CALL INTSTR(IV,STR1,IW1,IER)

C Check IV against minimum and respond based on ACT.
      IF(IV.LT.MN)THEN
        CALL INTSTR(MN,STR2,IW2,IER)
        IF(ACT.EQ.'W')THEN
          call edisp(iuout,' ')
          LN=max(1,lnblnk(currentfile))
          LNM=max(1,lnblnk(MSG))
          if(currentfile(1:2).ne.'  ')then
            WRITE(OUTSTR,'(4a)',IOSTAT=IOS,ERR=1)
     &        'WARNING: in ',currentfile(1:LN),': ',STRING(1:ils)
          else
            write(OUTSTR,'(a,a)',IOSTAT=IOS,ERR=1)' WARNING in: ',
     &      STRING(1:ils)
          endif
          call edisp(iuout,OUTSTR)
          write(OUTSTR,'(7a)',IOSTAT=IOS,ERR=1)' the ',MSG(1:LNM),
     &      ' value (',STR1(1:IW1),') < normal minimum ',STR2(1:IW2),'!'
          call edisp(iuout,OUTSTR)
        ELSEIF(ACT.EQ.'F')THEN
          call edisp(iuout,' ')
          LN=max(1,lnblnk(currentfile))
          LNM=max(1,lnblnk(MSG))
          if(currentfile(1:2).ne.'  ')then
            WRITE(OUTSTR,'(4a)',IOSTAT=IOS,ERR=1)
     &        'FAILURE: in ',currentfile(1:LN),': ',STRING(1:ils)
          else
            write(OUTSTR,'(a,a)',IOSTAT=IOS,ERR=1)' FAILURE in: ',
     &        STRING(1:ils)
          endif
          call edisp(iuout,OUTSTR)
          write(OUTSTR,'(7a)',IOSTAT=IOS,ERR=1)' the ',MSG(1:LNM),
     &       ' value (',STR1(1:IW1),') < allowable minimum ',
     &       STR2(1:IW2),'!'
          call edisp(iuout,outstr)
          IER=1
          RETURN
        ENDIF
      ELSEIF(IV.GT.MX)THEN
        CALL INTSTR(MX,STR2,IW2,IER)
        IF(ACT.EQ.'W')THEN
          call edisp(iuout,' ')
          LN=max(1,lnblnk(currentfile))
          LNM=max(1,lnblnk(MSG))
          if(currentfile(1:2).ne.'  ')then
            WRITE(OUTSTR,'(4a)',IOSTAT=IOS,ERR=1)
     &        'WARNING: in ',currentfile(1:LN),': ',STRING(1:ils)
          else
            write(OUTSTR,'(a,a)',IOSTAT=IOS,ERR=1)' WARNING in: ',
     &        STRING(1:ils)
          endif
          call edisp(iuout,OUTSTR)
          write(OUTSTR,'(7a)',IOSTAT=IOS,ERR=1)' the ',MSG(1:LNM),
     &      ' value (',STR1(1:IW1),') > normal maximum ',STR2(1:IW2),'!'
          call edisp(iuout,OUTSTR)
        ELSEIF(ACT.EQ.'F')THEN
          call edisp(iuout,' ')
          LN=max(1,lnblnk(currentfile))
          LNM=max(1,lnblnk(MSG))
          if(currentfile(1:2).ne.'  ')then
            WRITE(OUTSTR,'(4a)',IOSTAT=IOS,ERR=1)
     &        'FAILURE: in ',currentfile(1:LN),': ',STRING(1:ils)
          else
            write(OUTSTR,'(a,a)',IOSTAT=IOS,ERR=1)' FAILURE in: ',
     &        STRING(1:ils)
          endif
          call edisp(iuout,OUTSTR)
          write(OUTSTR,'(7a)',IOSTAT=IOS,ERR=1) ' the ',MSG(1:LNM),
     &       ' value (',STR1(1:IW1),') > allowable maximum ',
     &       STR2(1:IW2),'!'
          call edisp(iuout,outstr)
          IER=1
          RETURN
        ENDIF
      ENDIF

  100 RETURN

 1002 if(currentfile(1:2).ne.'  ')then
        LN=max(1,lnblnk(currentfile))
        LNM=max(1,lnblnk(MSG))
        WRITE(OUTSTR,'(6a)',IOSTAT=IOS,ERR=1) 'Failed in ',
     &    currentfile(1:LN),': conversion of ',
     &    MSG(1:LNM),' value in ',WORD
      else
        LNM=max(1,lnblnk(MSG))
        WRITE(OUTSTR,1003,IOSTAT=IOS,ERR=1)MSG(1:LNM),WORD
 1003   FORMAT(' Failed: conversion of ',A,' value in ',A)
      endif
      CALL EDISP(iuout,OUTSTR)
      IER=2
      GOTO 100
   1  write(6,*) 'EGETWI: error writing warning re: ',WORD

      END

C **************** EGETWR
C EGETWR gets first word after position K from the STRING of
C characters and converts it into a real number RV, tests it against
C the minimum RMN and the maximum RMX and provides a warning
C message if RACT='W', a failure message if RACT='F' and does
C no range checking if RACT='-'. Words may be separated by blanks,
C commas, or tab: WORD,WORD,WORD or WORD WORD WORD or WORD, WORD, WORD
C are all valid.
      SUBROUTINE EGETWR(STRING,K,RV,RMN,RMX,RACT,MSG,IER)
#include "espriou.h"
      common/OUTIN/IUOUT,IUIN

C LOUTSTR is for messages to the user and should be long enough
c to prevent truncation of messge contents.
      CHARACTER*(*) STRING, MSG
      CHARACTER RACT*1,STR1*16,STR2*16,WORD*20,LOUTSTR*248

C Pick up line and length for error messages.
      ils=max(1,lnblnk(STRING))
      if(ils.gt.230)ils=230

      IER=0
      WORD=' '
      CALL EGETW(STRING,K,WORD,'-','real',IER)
      IF(IER.NE.0) goto 1002
      read(WORD,*,ERR=1002)rv

C If range checking disabled, jump out of routine.
      IF(RACT.EQ.'-')RETURN

C Make up reporting string.
      CALL REL16STR(RV,STR1,IW1,IER)

C Check RV against minimum and respond based on RACT.
      IF(RV.LT.RMN)THEN
        CALL REL16STR(RMN,STR2,IW2,IER)
        IF(RACT.EQ.'W')THEN
          call edisp(iuout,' ')
          LN=max(1,lnblnk(currentfile))
          LNM=max(1,lnblnk(MSG))
          if(currentfile(1:2).ne.'  ')then
            WRITE(LOUTSTR,'(4a)',IOSTAT=IOS,ERR=1)
     &        'WARNING: in ',currentfile(1:LN),': ',STRING(1:ils)
          else
            write(LOUTSTR,'(a,a)',IOSTAT=IOS,ERR=1)' WARNING in: ',
     &        STRING(1:ils)
          endif
          call edisp248(iuout,LOUTSTR,90)
          write(LOUTSTR,'(7a)',IOSTAT=IOS,ERR=1)' the ',MSG(1:LNM),
     &      ' value (',STR1(1:IW1),') < normal minimum ',STR2(1:IW2),'!'
          call edisp248(iuout,LOUTSTR,90)
        ELSEIF(RACT.EQ.'F')THEN
          call edisp(iuout,' ')
          LN=max(1,lnblnk(currentfile))
          LNM=max(1,lnblnk(MSG))
          if(currentfile(1:2).ne.'  ')then
            WRITE(LOUTSTR,'(4a)',IOSTAT=IOS,ERR=1)
     &        'FAILURE: in ',currentfile(1:LN),': ',STRING(1:ils)
          else
            write(LOUTSTR,'(a,a)',IOSTAT=IOS,ERR=1)' FAILURE in: ',
     &      STRING(1:ils)
          endif
          call edisp248(iuout,LOUTSTR,90)
          write(LOUTSTR,'(7a)',IOSTAT=IOS,ERR=1)' the ',MSG(1:LNM),
     &       ' value (',STR1(1:IW1),') < allowable minumum ',
     &       STR2(1:IW2),'!'
          call edisp248(iuout,loutstr,90)
          IER=1
          RETURN
        ENDIF
      ELSEIF(RV.GT.RMX)THEN
        CALL REL16STR(RMX,STR2,IW2,IER)
        IF(RACT.EQ.'W')THEN
          call edisp(iuout,' ')
          LN=max(1,lnblnk(currentfile))
          LNM=max(1,lnblnk(MSG))
          if(currentfile(1:2).ne.'  ')then
            WRITE(LOUTSTR,'(4a)',IOSTAT=IOS,ERR=1)
     &        'WARNING: in ',currentfile(1:LN),': ',STRING(1:ils)
          else
            write(LOUTSTR,'(a,a)',IOSTAT=IOS,ERR=1)' WARNING in: ',
     &      STRING(1:ils)
          endif
          call edisp248(iuout,LOUTSTR,90)
          write(LOUTSTR,'(7a)',IOSTAT=IOS,ERR=1)' the ',MSG(1:LNM),
     &       ' value (',STR1(1:IW1),') > normal  maximum ',
     &       STR2(1:IW2),'!'
          call edisp248(iuout,LOUTSTR,90)
        ELSEIF(RACT.EQ.'F')THEN
          call edisp(iuout,' ')
          LN=max(1,lnblnk(currentfile))
          LNM=max(1,lnblnk(MSG))
          if(currentfile(1:2).ne.'  ')then
            WRITE(LOUTSTR,'(4a)',IOSTAT=IOS,ERR=1)
     &        'FAILURE: in ',currentfile(1:LN),': ',STRING(1:ils)
          else
            write(LOUTSTR,'(a,a)',IOSTAT=IOS,ERR=1)' FAILURE in: ',
     &      STRING(1:ils)
          endif
          call edisp248(iuout,LOUTSTR,90)
          write(LOUTSTR,'(7a)',IOSTAT=IOS,ERR=1)' the ',MSG(1:LNM),
     &       ' value (',STR1(1:IW1),') > allowable maximum ',
     &       STR2(1:IW2),'!'
          call edisp248(iuout,loutstr,90)
          IER=1
          RETURN
        ENDIF
      ENDIF

  100 RETURN

 1002 if(currentfile(1:2).ne.'  ')then
        LN=max(1,lnblnk(currentfile))
        LNM=max(1,lnblnk(MSG))
        WRITE(LOUTSTR,'(6a)',IOSTAT=IOS,ERR=1) 'Failed in ',
     &    currentfile(1:LN),': conversion of ',
     &    MSG(1:LNM),' value in ',WORD
      else
        LNM=max(1,lnblnk(MSG))
        WRITE(LOUTSTR,1003,IOSTAT=IOS,ERR=1)MSG(1:LNM),WORD
 1003   FORMAT(' Failed: conversion of ',A,' value in ',A)
      endif
      CALL EDISP248(iuout,LOUTSTR,90)
      IER=2
      GOTO 100
   1  write(6,*) 'EGETR: error writing warning re: ',STRING

      END

C **************** EGETWRA
C EGETWRA recovers (IRVA) reals of real array (RVA) from an ASCII file
C (unit IAF) reading as many lines as necessary to recover the data.
C IER=2 if EOF is reached before INUM items have been parsed. IER=1 if
C there was a problem reading it. Each value is tested against
C the minimum RMN and the maximum RMX and provides a warning
C message if RACT='W', a failure message if RACT='F' and does
C no range checking if RACT='-'. Words may be separated by blanks,
C commas, or tab: WORD,WORD,WORD or WORD WORD WORD or WORD, WORD, WORD
C are all valid.
C Note: all reads are able to accept commented files.
      SUBROUTINE EGETWRA(IAF,RVA,IRVA,RMN,RMX,RACT,MSG,IER)
      common/OUTIN/IUOUT,IUIN
      DIMENSION RVA(*)
      CHARACTER*(*) MSG
      CHARACTER OUTS*248,MSG1*124,RACT*1

      IF(IRVA.LE.0)GOTO 99

C Read a line from the file, if no problems set position to 0 and begin
C to parse words.
      CALL LSTRIPC(IAF,OUTS,0,ND,1,MSG,IER)
      LN=max(1,LNBLNK(OUTS))
      IF(IER.NE.0)RETURN
      K=0
      DO 12 KV=1,IRVA

C If character position is < actual length of string parse another
C word, otherwise read another line from the file.
        IF(K.LT.LN)THEN
          CALL EGETWR(OUTS,K,VAL,RMN,RMX,RACT,MSG,IERV)
          IF(IERV.NE.0) THEN
            CALL LSTRIPC(IAF,OUTS,0,ND,0,MSG,IER)
            LN=max(1,LNBLNK(OUTS))
            IF(IER.NE.0)RETURN
            K=0
            CALL EGETWR(OUTS,K,VAL,RMN,RMX,RACT,MSG,IERV)
          ENDIF
        ELSE
          CALL LSTRIPC(IAF,OUTS,0,ND,0,MSG,IER)
          LN=max(1,LNBLNK(OUTS))
          IF(IER.NE.0)RETURN
          K=0
          CALL EGETWR(OUTS,K,VAL,RMN,RMX,RACT,MSG,IERV)
        ENDIF
        IF(IERV.NE.0) GOTO 1001
        RVA(KV)=VAL
   12 CONTINUE

  100 RETURN

   99 CALL EDISP(iuout,' array size zero, skipping read.')
      GOTO 100

 1001 LNM=max(1,lnblnk(MSG))
      WRITE(MSG1,1002,IOSTAT=IOS,ERR=1)MSG(1:LNM),VAL,K
 1002 FORMAT(' Problem reading ',A,' value= ',F9.3,' @ pos ',
     &       I3,' in the string:')
      CALL EDISP(iuout,MSG1)
      if(LNM.ge.124)then
        write(6,*) 'EGETWRA: error writing: ',OUTS
      else
        CALL EDISP(iuout,OUTS)
      endif
      IER=1
      GOTO 100
   1  write(6,*) 'EGETWRA: error writing warning re: ',OUTS

      END

C *************** EGETWIA
C EGETWIA recovers (IRVA) integers of integer array (IVA) from an
C ASCII file (unit IAF) reading as many lines as necessary to
C recover the data.
C IER=2 if EOF is reached before IRVA items have been parsed. IER=1 if
C there was a problem reading it. Each value is tested against
C the minimum IRMN and the maximum IRMX and provides a warning
C message if IACT='W', a failure message if IACT='F' and does
C no range checking if IACT='-'. Words may be separated by blanks,
C commas, or tab: WORD,WORD,WORD or WORD WORD WORD or WORD, WORD, WORD
C are all valid.
C Note: all reads are able to accept commented files.
      SUBROUTINE EGETWIA(IAF,IVA,IRVA,IRMN,IRMX,IACT,MSG,IER)
      common/OUTIN/IUOUT,IUIN
      DIMENSION IVA(*)
      CHARACTER*(*) MSG
      CHARACTER OUTS*248,MSG1*124,IACT*1

      IF(IRVA.LE.0)GOTO 99

C Read a line from the file, if no problems set position to 0 and begin
C to parse words.
      CALL LSTRIPC(IAF,OUTS,0,ND,1,MSG,IER)
      LN=max(1,LNBLNK(OUTS))
      IF(IER.NE.0)RETURN
      K=0
      DO 12 KV=1,IRVA

C If character position is < actual length of string parse another
C word, otherwise read another line from the file.
        IF(K.LT.LN)THEN
          CALL EGETWI(OUTS,K,IVAL,IRMN,IRMX,IACT,MSG,IERV)
          IF(IERV.NE.0) THEN
            CALL LSTRIPC(IAF,OUTS,0,ND,0,MSG,IER)
            LN=max(1,LNBLNK(OUTS))
            IF(IER.NE.0)RETURN
            K=0
            CALL EGETWI(OUTS,K,IVAL,IRMN,IRMX,IACT,MSG,IERV)
          ENDIF
        ELSE
          CALL LSTRIPC(IAF,OUTS,0,ND,0,MSG,IER)
          LN=max(1,LNBLNK(OUTS))
          IF(IER.NE.0)RETURN
          K=0
          CALL EGETWI(OUTS,K,IVAL,IRMN,IRMX,IACT,MSG,IERV)
        ENDIF
        IF(IERV.NE.0) GOTO 1001
        IVA(KV)=IVAL
   12 CONTINUE

  100 RETURN

   99 CALL EDISP(iuout,' array size zero, skipping read.')
      GOTO 100

 1001 LNM=max(1,lnblnk(MSG))
      WRITE(MSG1,1002,IOSTAT=IOS,ERR=1)MSG(1:LNM),IVAL,K
 1002 FORMAT(' Problem reading ',A,' value= ',I8,' @ pos ',
     &       I3,' in the string:')
      CALL EDISP(iuout,MSG1)
      if(LNM.ge.124)then
        write(6,*) 'EGETWIA: error writing ',OUTS
      else
        CALL EDISP(iuout,OUTS)
      endif
      IER=1
      GOTO 100
   1  write(6,*) 'EGETWIA: error writing warning re: ',OUTS

      END

C *************** EGETAGWIA
C EGETAGWIA recovers (IRVA) integers of integer array (IVA) from a string
C TSTR (from position K) and if TSTR does not hold all of the array then
C it continues reading from an ASCII file (unit IAF) reading as
C many lines as necessary to recover the data.
C IER=2 if EOF is reached before IRVA items have been parsed. IER=1 if
C there was a problem reading it. Each value is tested against
C the minimum IRMN and the maximum IRMX and provides a warning
C message if IACT='W', a failure message if IACT='F' and does
C no range checking if IACT='-'. Words may be separated by blanks,
C commas, or tab: WORD,WORD,WORD or WORD WORD WORD or WORD, WORD, WORD
C are all valid.
C Note: all reads are able to accept commented files.
      SUBROUTINE EGETAGWIA(TSTR,K,IAF,IRVA,IVA,IRMN,IRMX,IACT,MSG,IER)
      common/OUTIN/IUOUT,IUIN
      DIMENSION IVA(*)
      CHARACTER*(*) TSTR,MSG
      CHARACTER OUTS*248,IACT*1,MSG1*124
      logical swapover

      IF(IRVA.LE.0)GOTO 99

C LN is the actual length of TSTR
C Initially set swapover = false. When TSTR exhaused set swapover
C = true and read from outs.
      swapover=.false.
      ier=0
      LN=max(1,lnblnk(TSTR))
      DO 12 KV=1,IRVA

C If character position is < actual length of string parse another
C word, otherwise read another line from the file.
        IF(K.LT.LN)THEN
          if(swapover)then
            CALL EGETWI(outs,K,IVAL,IRMN,IRMX,IACT,MSG,IERV)
          else
            CALL EGETWI(TSTR,K,IVAL,IRMN,IRMX,IACT,MSG,IERV)
          endif
          IF(IERV.NE.0) THEN
            CALL LSTRIPC(IAF,OUTS,0,ND,0,MSG,IER)
            LN=max(1,LNBLNK(OUTS))
            IF(IER.NE.0)RETURN
            K=0
            CALL EGETWI(OUTS,K,IVAL,IRMN,IRMX,IACT,MSG,IERV)
            swapover=.true.
          ENDIF
        ELSE
          CALL LSTRIPC(IAF,OUTS,0,ND,0,MSG,IER)
          LN=max(1,LNBLNK(OUTS))
          IF(IER.NE.0)RETURN
          K=0
          CALL EGETWI(OUTS,K,IVAL,IRMN,IRMX,IACT,MSG,IERV)
          swapover=.true.
        ENDIF
        IF(IERV.NE.0) GOTO 1001
        IVA(KV)=IVAL
   12 CONTINUE

  100 RETURN

   99 CALL EDISP(iuout,' array size zero, skipping read.')
      GOTO 100

 1001 LNM=max(1,lnblnk(MSG))
      WRITE(MSG1,1002,IOSTAT=IOS,ERR=1)MSG(1:LNM),IVAL,K
 1002 FORMAT(' Problem reading ',A,' value= ',I8,' @ pos ',
     &       I3,' in the string:')
      CALL EDISP(iuout,MSG1)
      if(LNM.ge.124)then
        write(6,*) 'EGETAGWIA: error writing ',OUTS
      else
        CALL EDISP(iuout,OUTS)
      endif
      IER=1
      GOTO 100
   1  write(6,*) 'EGETAGWIA: error writing warning re: ',OUTS
      end

C *************** EGETAGWRA
C EGETAGWRA recovers (IRVA) reals of real array (RVA) from a string
C TSTR (from position K) and if TSTR does not hold all of the array then
C it continues reading from an ASCII file (unit IAF) reading as
C many lines as necessary to recover the data.
C IER=2 if EOF is reached before IRVA items have been parsed. IER=1 if
C there was a problem reading it. Each value is tested against
C the minimum RMN and the maximum RMX and provides a warning
C message if IACT='W', a failure message if IACT='F' and does
C no range checking if RACT='-'. Words may be separated by blanks,
C commas, or tab: WORD,WORD,WORD or WORD WORD WORD or WORD, WORD, WORD
C are all valid.
C Note: all reads are able to accept commented files.
      SUBROUTINE EGETAGWRA(TSTR,K,IAF,IRVA,RVA,RMN,RMX,RACT,MSG,IER)
      common/OUTIN/IUOUT,IUIN
      DIMENSION RVA(*)
      CHARACTER*(*) TSTR,MSG
      CHARACTER OUTS*248,RACT*1,MSG1*124
      logical swapover

      IF(IRVA.LE.0)GOTO 99

C LN is the actual length of TSTR.
      ier=0
      LN=max(1,lnblnk(TSTR))

C Initially set swapover = false. When TSTR exhaused set swapover
C = true and read from outs.
      swapover=.false.
      DO 12 KV=1,IRVA

C If character position is < actual length of string parse another
C word, otherwise read another line from the file.
        IF(K.LT.LN)THEN
          if(swapover)then
            CALL EGETWR(OUTS,K,VAL,RMN,RMX,RACT,MSG,IERV)
          else
            CALL EGETWR(TSTR,K,VAL,RMN,RMX,RACT,MSG,IERV)
          endif
          IF(IERV.NE.0) THEN
            CALL LSTRIPC(IAF,OUTS,0,ND,0,MSG,IER)
            LN=max(1,LNBLNK(OUTS))
            IF(IER.NE.0)RETURN
            K=0
            CALL EGETWR(OUTS,K,VAL,RMN,RMX,RACT,MSG,IERV)
            swapover=.true.
          ENDIF
        ELSE
          CALL LSTRIPC(IAF,OUTS,0,ND,0,MSG,IER)
          LN=max(1,LNBLNK(OUTS))
          IF(IER.NE.0)RETURN
          K=0
          CALL EGETWR(OUTS,K,VAL,RMN,RMX,RACT,MSG,IERV)
          swapover=.true.
        ENDIF
        IF(IERV.NE.0) GOTO 1001
        RVA(KV)=VAL
   12 CONTINUE

  100 RETURN

   99 CALL EDISP(iuout,' array size zero, skipping read.')
      GOTO 100

 1001 LNM=max(1,lnblnk(MSG))
      WRITE(MSG1,1002,IOSTAT=IOS,ERR=1)MSG(1:LNM),VAL,K
 1002 FORMAT(' Problem reading ',A,' value= ',F9.3,' @ pos ',
     &       I3,' in the string:')
      CALL EDISP(iuout,MSG1)
      if(LNM.ge.124)then
        write(6,*) 'EGETWRA: error writing: ',OUTS
      else
        CALL EDISP(iuout,OUTS)
      endif
      IER=1
      GOTO 100
   1  write(6,*) 'EGETWRA: error writing warning re: ',OUTS
      end

C ***************** EGETRM
C EGETRM returns the remainder of a text string TSTR after position k
C in RSTR where RSTR has no leading blanks.
      SUBROUTINE EGETRM(TSTR,K,RSTR,ACT,MSG,IER)
#include "espriou.h"
      common/OUTIN/IUOUT,IUIN


      CHARACTER*(*) TSTR,RSTR,MSG
      character ACT*1,A*1,OUTSTR*124,outs*124

      IER=0

C LS is th maximum length of STRING, L the current position,
C LR the maximum length of RSTR.
      ier=0
      RSTR=' '
      LS=LEN(TSTR)
      LR=LEN(RSTR)
      L=0

C Start by skipping blanks and tabs before TSTR.
   10 K=K+1
      IF(K.GT.LS) GOTO 999
      A=TSTR(K:K)
      IF(A.EQ.' '.OR.A.EQ.','.OR.(ICHAR(A).EQ.9)) GOTO 10

C Copy RSTR from TSTR character by character until either end
C of RSTR or TSTR.
   20 L=L+1
      IF(L.GT.LR)goto 100
      RSTR(L:L)=A
      K=K+1
      IF(K.GT.LS) goto 100
      A=TSTR(K:K)
      GOTO 20

  100 RETURN

  999 if(ACT.EQ.'-')then
        RETURN
      elseif(ACT.EQ.'W')then
        ier=1
        LN=max(1,lnblnk(currentfile))
        LNM=max(1,lnblnk(MSG))
        if(currentfile(1:2).ne.'  ')then
          WRITE(OUTSTR,'(5a)',IOSTAT=IOS,ERR=1) 'WARNING in ',
     &      currentfile(1:LN),': no chars found for ',
     &      MSG(1:LNM),' string in:'
        else
          WRITE(OUTSTR,'(3a)',IOSTAT=IOS,ERR=1)
     &     'WARNING: no characters found for ',MSG(1:LNM),' string in:'
        endif
      elseif(ACT.EQ.'F')then
        LN=max(1,lnblnk(currentfile))
        LNM=max(1,lnblnk(MSG))
        if(currentfile(1:2).ne.'  ')then
          WRITE(OUTSTR,'(5a)',IOSTAT=IOS,ERR=1) 'FAILURE in ',
     &      currentfile(1:LN),': no chars found for ',
     &      MSG(1:LNM),' string in:'
        else
          WRITE(OUTSTR,'(3a)',IOSTAT=IOS,ERR=1)
     &      'FAILURE: no characters found for ',MSG(1:LNM),' string in:'
        endif
      endif
      CALL EDISP(iuout,OUTSTR)
      if(lnblnk(TSTR).gt.123)then
        write(6,*) TSTR
      else
        CALL EDISP(iuout,TSTR)
      endif
      GOTO 100
   1  write(outs,*) 'EGETRM: error writing warning re: ',RSTR
      call edisp(iuout,outs)

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

 999  WRITE(outs,*) ' RELSTR: invalid real or > 12 char: ',RELIN
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

 999  WRITE(outs,*) ' REL16STR: invalid real or > 16 char: ',RELIN
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
      IFLAG=0
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
C that each string array item is less than 24 characters wide.

      SUBROUTINE ASLIST(inst,inil,list,inisz,delm,pckstr,length,itrunc)
      dimension list(inisz)
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
        lna=max(1,LNBLNK(list(i)))
        if(lna.gt.24) lna=24
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

      CHARACTER*124 tmp,STRING,OUTSTR,MSG1
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
            WRITE(tmp,'(3A,I3,3A)')'In ',currentfile(1:LN),' expecting',
     &        IEXP,' items (',MSG(1:LNM),') in...'
            CALL USRMSG(tmp,OUTSTR,'F')
          else
            WRITE(tmp,'(A,I3,3A)')' Expecting',IEXP,' items (',
     &         MSG(1:LNM),') in...'
            CALL USRMSG(tmp,OUTSTR,'F')
          endif
        endif
      endif

    4 RETURN

  101 IER=1
      IF(IR.EQ.1)THEN
        LN=max(1,lnblnk(currentfile))
        LNM=max(1,lnblnk(MSG))
        if(currentfile(1:2).ne.'  ')then
          WRITE(MSG1,'(5A)')' In ',currentfile(1:LN),
     &      ' error reading (',MSG(1:LNM),') in...'
        else
          WRITE(MSG1,'(A,A)',IOSTAT=IOS,ERR=1)' Error reading: ',
     &      MSG(1:LNM),') in...'
        endif
        CALL USRMSG(MSG1,OUTSTR,'W')
      ENDIF
      goto 4

  102 IER=2
      IF(IR.EQ.1)THEN
        LN=max(1,lnblnk(currentfile))
        LNM=max(1,lnblnk(MSG))
        if(currentfile(1:2).ne.'  ')then
          WRITE(tmp,'(5A)')' In ',currentfile(1:LN),
     &      ' error reading (',MSG(1:LNM),') EOF sensed.'
        else
          WRITE(tmp,'(3A)',IOSTAT=IOS,ERR=1)' Error reading ',
     &      MSG(1:LNM),' EOF sensed.'
        endif
        CALL USRMSG(' ',tmp,'-')
      ENDIF
      goto 4
   1  write(6,*) 'STRIPC: error writing warning re: ',OUTSTR

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
      CHARACTER*124 MSG1
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
            WRITE(tmp,'(3A,I3,3A)')' In ',currentfile(1:LN),
     &        ' expecting',IEXP,' items (',MSG(1:LNM),') in...'
            CALL LUSRMSG(tmp,OUTSTR,'F')
          else
            WRITE(tmp,'(A,I3,3A)')' Expecting',IEXP,' items (',
     &         MSG(1:LNM),') in...'
            CALL LUSRMSG(tmp,OUTSTR,'F')
          endif
        endif
      endif

    4 RETURN

  101 IER=1
      IF(IR.EQ.1)THEN
        LN=max(1,lnblnk(currentfile))
        LNM=max(1,lnblnk(MSG))
        if(currentfile(1:2).ne.'  ')then
          WRITE(MSG1,'(5A)')' In ',currentfile(1:LN),
     &      ' error reading (',MSG(1:LNM),') in...'
        else
          WRITE(MSG1,'(A,A)',IOSTAT=IOS,ERR=1)' Error reading: ',
     &      MSG(1:LNM),') in...'
        endif
        CALL LUSRMSG(MSG1,OUTSTR,'W')
      ENDIF
      goto 4

  102 IER=2
      IF(IR.EQ.1)THEN
        LN=max(1,lnblnk(currentfile))
        LNM=max(1,lnblnk(MSG))
        if(currentfile(1:2).ne.'  ')then
          WRITE(tmp,'(5A)')' In ',currentfile(1:LN),
     &      ' error reading (',MSG(1:LNM),') EOF sensed.'
        else
          WRITE(tmp,'(3A)',IOSTAT=IOS,ERR=1)' Error reading ',
     &      MSG(1:LNM),' EOF sensed.'
        endif
        CALL LUSRMSG(' ',tmp,'-')
      ENDIF
      goto 4
   1  write(6,*) 'STRIPC: error writing warning re: ',OUTSTR

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
   1  write(6,*) 'CHITMS: error in internal write: ',A

      END

C ********************* LCHITMS
C LCHITMS checks a long character string (A), returning the number of data
C items (IW) separated by ' ' tab or ','.  Note to keep from
C overwriting the string passed, deal with a copy.
      SUBROUTINE LCHITMS(A,IW)
      CHARACTER*(*) A
      CHARACTER B*248,C*1

      lastcc = MIN0(LNBLNK(A),247)
      WRITE(B,'(A)',IOSTAT=IOS,ERR=1) A(1:lastcc+1)

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
   1  write(6,*) 'CHITMS: error in internal write: ',A

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

        IF (FNARG(1:7).eq.'UNKNOWN'.or.FNARG(1:2).eq.'  ') THEN
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
        lfn=max(1,LNBLNK(fnarg))

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

        IF (FNARG(1:7).eq.'UNKNOWN') THEN
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
        WRITE(PERST1,1,IOSTAT=ISTAT,ERR=10)T1H,T2H
    1   FORMAT(' ',A5,'  ',A5,' ')
      ELSEIF(IFTIME.EQ.1)THEN
        WRITE(PERST1,1,IOSTAT=ISTAT,ERR=10)T1D,T2D
      ELSEIF(IFTIME.EQ.2)THEN
        WRITE(PERST1,2,IOSTAT=ISTAT,ERR=10)TIMER1,TIMER2
    2   FORMAT(2F7.4)
      ENDIF

      IF(IFDAY.EQ.0)THEN
        WRITE(PERST3,3,IOSTAT=ISTAT,ERR=10)IBDOY,IEDOY,IYEAR
    3   FORMAT('period: DOY ',I3,' to DOY ',I3,', ',I4)
        IF(IFTIME.EQ.0)THEN
          WRITE(PERST2,4,IOSTAT=ISTAT,ERR=10)IBDOY,T1H,IEDOY,T2H
    4     FORMAT('period: DOY ',I3,' @ ',A5,' to DOY ',I3,' @ ',A5)
        ELSEIF(IFTIME.EQ.1)THEN
          WRITE(PERST2,4,IOSTAT=ISTAT,ERR=10)IBDOY,T1D,IEDOY,T2D
        ELSEIF(IFTIME.EQ.2)THEN
          WRITE(PERST2,5,IOSTAT=ISTAT,ERR=10)FD1,FD2
    5     FORMAT('period: DOY ',F10.6,' to DOY ',F10.6)
        ENDIF
      ELSEIF(IFDAY.EQ.1)THEN
        WRITE(PERST3,6,IOSTAT=ISTAT,ERR=10)DS,DE,IYEAR
    6   FORMAT('period: ',A7,' to ',A7,', ',I4)
        IF(IFTIME.EQ.0)THEN
          WRITE(PERST2,7,IOSTAT=ISTAT,ERR=10)DS,T1H,DE,T2H
    7     FORMAT('period: ',A7,' @ ',A5,' to ',A7,' @ ',A5)
        ELSE
          WRITE(PERST2,7,IOSTAT=ISTAT,ERR=10)DS,T1D,DE,T2D
        ENDIF
      ELSEIF(IFDAY.EQ.2)THEN
        WRITE(PERST3,8,IOSTAT=ISTAT,ERR=10)DS1,DE1,IYEAR
    8   FORMAT('period: ',A10,' to ',A10,', ',I4)
        IF(IFTIME.EQ.0)THEN
          WRITE(PERST2,9,IOSTAT=ISTAT,ERR=10)DS1,T1H,DE1,T2H
    9     FORMAT('period: ',A10,'@',A5,' - ',A10,'@',A5)
        ELSE
          WRITE(PERST2,9,IOSTAT=ISTAT,ERR=10)DS1,T1D,DE1,T2D
        ENDIF
      ENDIF
      RETURN
  10  write(6,*) 'EPERSTR: error writing warning or strings.'
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

C Build up the descriptive text strings.
      WRITE(DESCR,'(i2.2,2a)',ERR=1)IDAYN,'-',MTHNAM(IMTHN)
      WRITE(DESCR1,'(2a,i2.2,2a)',ERR=1)DAYNAM(IDWK),'-',IDAYN,'-',
     &    MTHNAM(IMTHN)

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
      WRITE(DESCRD,'(f5.2)',IOSTAT=ISTAT,ERR=1)TIMER
      TIMER=TIMER/24.0
      RETURN
 1    call usrmsg('ESTIME: problem writing time strings.',' ','W')
      END

C ******************** EDTIME
C EDTIME takes an real time and returns two string descriptions:
C DESCRH in the form '12h28' and DESCRD which takes the form of 12.46,
C As well as the time as a fraction of a day TIMER.
      SUBROUTINE EDTIME(TIME,DESCRH,DESCRD,TIMER)
      CHARACTER*5 DESCRH, DESCRD

      MIN=INT((TIME-AINT(TIME))*60.)
      WRITE(DESCRH,'(i2.2,a,i2.2)',ERR=1)INT(TIME),'h',MIN
      WRITE(DESCRD,'(f5.2)',IOSTAT=ISTAT,ERR=1)TIME
      TIMER=TIME/24.0
      RETURN
 1    call usrmsg('EDTIME: problem writing time strings.',' ','W')
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

C ********************
C Takes latitude and longitude difference and returns descriptive
C string.
      subroutine sitell2s(clat,clong,descr)
      character descr*16

      IF(CLAT.LT.0.0)goto 11
      IF(CLONG.LT.0.0)goto 12
      WRITE(descr,'(F7.1,a,F7.1,a)')CLAT,'N',CLONG,'E'
      RETURN

   12 ACLONG=ABS(CLONG)
      WRITE(descr,'(F7.1,a,F7.1,a)')CLAT,'N',ACLONG,'W'
      RETURN

   11 ACLAT=ABS(CLAT)
      IF(CLONG.LT.0.0)goto 13
      WRITE(descr,'(F7.1,a,F7.1,a)')ACLAT,'S',CLONG,'E'
      RETURN

   13 ACLONG=ABS(CLONG)
      WRITE(descr,'(F7.1,a,F7.1,a)')ACLAT,'S',ACLONG,'W'
      RETURN

      end

C ******************** EPROMPT
C EPROMPT: Does nothing, for compatibility only.
      SUBROUTINE EPROMPT
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
C time (REAL).   The 'ISUNUP' variable determines whether the sun
C is up (=1) or down (=0).   The solar angles are computed relative
C to local mean time (ie Greenwich is the reference time zone for
C Britain).
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

C ********* WIREPK
C WIREPK is called from C code in wwlib.c with its current idea as to
C the number of zones which have currently been selected for display.
C Compilation of wwlib.c assumes this will be available in each modules
C code somewhere.  If not needed then provide a dummy.
      subroutine wirepk(inpk)
#include "building.h"

C izgfoc is the graphic focus zone, nzg is the number of zones selected
C and nznog array of selected zone indices.
      COMMON/OUTIN/IUOUT,IUIN
      COMMON/gzonpik/izgfoc,nzg,nznog(mcom)
      if(inpk.ne.nzg)then
        call edisp(iuout,'Mismatch betwen inpk and nzg... ')
      endif
      CALL EVSET(1,'B',IER)
      if(ier.ne.0)then
        call edisp(iuout,'Problem after wire button pick... ')
      endif

      return
      end

C ************* EVSET
C EVSET provides setup environment for wire frame view w/in prj.
C ITRC = trace level, IER=0 OK, IER=1 problem. If izgfoc is not 0
C then it represents the zone which is currently being edited.
      SUBROUTINE EVSET(ITRC,act,IER)
#include "building.h"
#include "espriou.h"

      COMMON/FILEP/IFIL
      COMMON/SPAD/MMOD,LIMIT,LIMTTY
      COMMON/OUTIN/IUOUT,IUIN
      common/pophelp/h(60)
      COMMON/gzonpik/izgfoc,nzg,nznog(mcom)
      COMMON/C1/NCOMP,NCON
      common/rpath/path
      common/appw/iappw,iappx,iappy
      COMMON/VIEWPX/menuchw,igl,igr,igt,igb,igw,igwh
      COMMON/GFONT/IFS,ITFS,IMFS
      COMMON/precz/zname(MCOM),zdesc(MCOM)
      COMMON/PREC8/SLAT,SLON
      COMMON/MLC/NMLC,DESC(MMLC),DTHK(MMLC,ME),IPR(MMLC,ME),
     &           LAYERS(MMLC),DRAIR(MMLC,ME,3)

C ITDSP labels toggle: all surf + obs = 0, all surf = 1, ext = 2,
C       partn = 3, similar = 4, surfs + obs+ ground = 5, ground only = 6
C ITBND bounds toggle: static = 0, optimum = 1, zone focus = 2
C ITEPT is not yet used.
C ITZNM zone name toggle: display = 0, hidden = 1
C ITSNM surface name toggle: display = 0, hidden = 1
C ITORG origin toggle: display = 0, hidden = 1
C ITSNR surf normal toggle: display = 0, hidden = 1.
C ITOBS obstruction toggle: not yet enabled.
C ITHLS highlight toggle: normal 0, constr 1, trans/opaq 2, part atrib 3
C ITHLZ additional qualifier for ITHLS.
C ITGRD grid toggle: display = 0, hidden = 1
C ITVNO vertex toggle: display = 0, hidden = 1
C ITPPSW current view - perspective/plan/south/west
      COMMON/RAY2/ITDSP,ITBND,ITEPT,ITZNM,ITSNM,ITVNO,ITORG,ITSNR,
     &            ITOBS,ITHLS,ITHLZ,ITGRD,GRDIS,ITPPSW
      COMMON/RAY3/MODIFY,MODLEN,MODBND
      COMMON/RAY5/ZCOG(MCOM,3),XMN,YMN,ZMN,XMX,YMX,ZMX
      COMMON/RAY7/ZXMN(MCOM),ZYMN(MCOM),ZZMN(MCOM),ZXMX(MCOM),
     &            ZYMX(MCOM),ZZMX(MCOM),ZBFLG(MCOM)
      COMMON/IMAGE/IMT,EYEM(3),VIEWM(3),HITH,YON,ANG,HANG,WIDE
      COMMON/FOPENED/CFGOK,MLDBOK,CONDBOK,CTLOK,OPTKOK

      LOGICAL CFGOK,MLDBOK,CONDBOK,CTLOK
      logical MODIFY,MODLEN,MODBND
      LOGICAL OPTKOK,CLOSE,OK,dok,found,concat,redraw
      DIMENSION ITEMS(23),IVALS(MCOM),FALT(7),IVAL(7),MTHNAM(12)
      CHARACTER ITEMS*32,H*72,zname*12,zdesc*64,LFIL*72,outs*124
      CHARACTER FALT*37,MTHNAM*3,ETEXT*60
      CHARACTER DESCRH*5,DESCRD*5,path*72
      character doit*248,tmode*8,longtfile*144, hold*24,act*1
      CHARACTER DESC*48,head*19

      DATA MTHNAM/'Jan','Feb','Mar','Apr','May','Jun','Jul','Aug',
     &            'Sep','Oct','Nov','Dec'/

C If not in graphic mode return.
      if(MMOD.lt.8)return

C Initial menu entry setup with one line header, 3 control lines.
      redraw=.false.
      IER=0
      ITPPSW=0
      IUF=IFIL+1

C Menu loop.
   92 IER=0
      IVERT=-3

      WRITE(ITEMS(1),'(A,3F6.1)')'a eyepoint :',(EYEM(J),J=1,3)
      WRITE(ITEMS(2),'(A,3F6.1)')'b viewpoint:',(VIEWM(J),J=1,3)
      WRITE(ITEMS(3),'(A,F6.1)') 'c angle of view:',ANG
      if(ITPPSW.eq.0)then
        ITEMS(4) ='d Display >> perspective     '
      elseif(ITPPSW.eq.1)then
        ITEMS(4) ='d Display >> plan view       '
      elseif(ITPPSW.eq.2)then
        ITEMS(4) ='d Display >> south elevation '
      elseif(ITPPSW.eq.3)then
        ITEMS(4) ='d Display >> west elevation  '
      endif
      ITEMS(5)   =' _____________________________  '

      IF(ITDSP.EQ.0)THEN
        ITEMS(6) ='e display >> surfaces & obstruct'
      ELSEIF(ITDSP.EQ.1)THEN
        ITEMS(6) ='e display >> all surfaces       '
      ELSEIF(ITDSP.EQ.2)THEN
        ITEMS(6) ='e display >> exterior surfaces  '
      ELSEIF(ITDSP.EQ.3)THEN
        ITEMS(6) ='e display >> partitions/int surf'
      ELSEIF(ITDSP.EQ.4)THEN
        ITEMS(6) ='e display >> const/boundary surf'
      ELSEIF(ITDSP.EQ.5)THEN
        ITEMS(6) ='e display >> surfs+obstr+ground'
      ELSEIF(ITDSP.EQ.6)THEN
        ITEMS(6) ='e display >> ground only'
      ENDIF

C Hilight surface based on OPQ/TRAN or material.
      IF(ITHLS.EQ.0)THEN
        ITEMS(7) ='f highlight >> normal           '
      ELSEIF(ITHLS.EQ.1)THEN
        if(ITHLZ.gt.0)then
          WRITE(ITEMS(7),'(a,A12)')'f highlight >> ',DESC(ITHLZ)(1:12)
        else
          ITEMS(7) ='f highlight >> composition      '
        endif
      ELSEIF(ITHLS.EQ.2)THEN
        ITEMS(7) ='f highlight >> transparent:opaq '
      ELSEIF(ITHLS.EQ.3)THEN
        ITEMS(7) ='f highlight >> partial attribute'
      ENDIF

      IF(ITBND.EQ.0)THEN
        ITEMS(8) ='g view bounds >> static         '
      ELSEIF(ITBND.EQ.1)THEN
        ITEMS(8) ='g view bounds >> optimum        '
      ELSEIF(ITBND.EQ.2)THEN
        ITEMS(8) ='g view bounds >> focus on zone  '
      ENDIF
      ITEMS(9)   =' _____________________________  '

      IF(ITZNM.EQ.0)THEN
        ITEMS(10)='h zone names       >> display   '
      ELSEIF(ITZNM.EQ.1)THEN
        ITEMS(10)='h zone names       >> hidden    '
      ENDIF

      IF(ITSNM.EQ.0)THEN
        ITEMS(11)='i surface names    >> display   '
      ELSEIF(ITSNM.EQ.1)THEN
        ITEMS(11)='i surface names    >> hidden    '
      ENDIF

      IF(ITVNO.EQ.0)THEN
        ITEMS(12)='j vertex number    >> display   '
      ELSEIF(ITVNO.EQ.1)THEN
        ITEMS(12)='j vertex number    >> hidden    '
      ENDIF

      IF(ITORG.EQ.0)THEN
        ITEMS(13)='k site origin      >> display   '
      ELSEIF(ITORG.EQ.1)THEN
        ITEMS(13)='k site origin      >> hidden    '
      ENDIF

      IF(ITGRD.EQ.0)THEN
        ITEMS(14)='l site grid        >> display   '
      ELSEIF(ITGRD.EQ.1)THEN
        ITEMS(14)='l site grid        >> hidden    '
      ENDIF
      CALL ECLOSE(GRDIS,0.0,0.001,CLOSE)
      IF(CLOSE)THEN
        ITEMS(15)='m grid distance:      optimum   '
      ELSE
        WRITE(ITEMS(15),23)GRDIS
  23    FORMAT('m grid distance:',F5.2,' (metres)')
      ENDIF

      IF(ITSNR.EQ.0)THEN
        ITEMS(16)='n surface normals  >> display   '
      ELSEIF(ITSNR.EQ.1)THEN
        ITEMS(16)='n surface normals  >> hidden    '
      ENDIF

      ITEMS(17)  =' _____________________________  '

      ITEMS(18)  ='* zones to include in image     '
      ITEMS(19)  ='! refresh image                 '
      ITEMS(20)  ='1 hidden line view              '
      ITEMS(21)  ='2 views from sun                '
      ITEMS(22)  ='? help                          '
      ITEMS(23)  ='- exit this menu                '
      MVERT=23

C If image info altered (viewpoint) then update display.
      IF(MODIFY)THEN
        if(ITPPSW.eq.0)then
          CALL INLNST(1)
          CALL ADJVIEW(ITRC,IER)
          IF(ier.NE.0)RETURN
          MODLEN=.FALSE.
          MODBND=.FALSE.
          MODIFY=.FALSE.
        elseif(ITPPSW.eq.1)then
          if(redraw)call PLELEV(IUF,'P',ier)
        elseif(ITPPSW.eq.2)then
          if(redraw)call PLELEV(IUF,'S',ier)
        elseif(ITPPSW.eq.3)then
          if(redraw)call PLELEV(IUF,'E',ier)
        endif
      ENDIF

C Now display the menu. If in response to wireframe control button
C then shift its position slighty (taking into account the current
C width of the control menu.
      if(act.eq.'-')then
        head='Viewing Environment'
        CALL EMENU(head,ITEMS,MVERT,IVERT)
      elseif(act.eq.'B')then
        call findrtb(iappr,iappt,iappb)
        head='  Wireframe control'
        if(menuchw.lt.10)then
          call vwmenu (head,ITEMS,MVERT,iappr-160,
     &      iappb-50,32,irpx,irpy,IVERT)
        elseif(menuchw.ge.10.and.menuchw.lt.20)then
          call vwmenu (head,ITEMS,MVERT,iappr-120,
     &      iappb-50,32,irpx,irpy,IVERT)
        elseif(menuchw.ge.20.and.menuchw.lt.30)then
          call vwmenu (head,ITEMS,MVERT,iappr-60,
     &      iappb-50,32,irpx,irpy,IVERT)
        elseif(menuchw.ge.30.and.menuchw.lt.35)then
          call vwmenu (head,ITEMS,MVERT,iappr-20,
     &      iappb-50,32,irpx,irpy,IVERT)
        elseif(menuchw.ge.35)then
          call vwmenu (head,ITEMS,MVERT,iappr-10,
     &      iappb-50,32,irpx,irpy,IVERT)
        endif
      endif
      IF(IVERT.EQ.MVERT)THEN
        RETURN
      ELSEIF(IVERT.EQ.1)THEN
        H(1)='The eyepoint is in metres in the coordinate system '
        H(2)='of the site. '
  43    WRITE(HOLD,'(1x,3f7.1)')EYEM(1),EYEM(2),EYEM(3)
        CALL EASKS(HOLD,' Eye point  X  Y  Z (in metres): ',
     &     '  ',24,' -100. -100. 100. ','eye point coord',IER,2)
        K=0
        CALL EGETWR(HOLD,K,EYEM(1),-999.,999.,'W','X cord',IER)
        CALL EGETWR(HOLD,K,EYEM(2),-999.,999.,'W','Y cord',IER)
        CALL EGETWR(HOLD,K,EYEM(3),0.,999.,'W','eye Z cord',IER)
        if(ier.ne.0)goto 43
        MODLEN=.TRUE.
        MODIFY=.TRUE.
      ELSEIF(IVERT.EQ.2)THEN

C Present viewpoint coords for editing then parse data from HOLD.
        H(1)='The viewpoint is in metres in the coordinate system'
        H(2)='of the site and defaults to the centre of gravity '
        H(3)='of the body. '
  42    WRITE(HOLD,'(1x,3f7.1)')VIEWM(1),VIEWM(2),VIEWM(3)
        CALL EASKS(HOLD,' Viewpoint  X  Y  Z (in metres): ',
     &     '  ',24,' 2. 2. 1. ','viewpoint coord',IER,3)
        K=0
        CALL EGETWR(HOLD,K,VIEWM(1),-999.,999.,'W','X cord',IER)
        CALL EGETWR(HOLD,K,VIEWM(2),-999.,999.,'W','Y cord',IER)
        CALL EGETWR(HOLD,K,VIEWM(3),0.,999.,'W','view Z cord',IER)
        if(ier.ne.0)goto 42
        MODLEN=.TRUE.
        MODIFY=.TRUE.
      ELSEIF(IVERT.EQ.3)THEN
        H(1)='The angle of view is in degrees, allowable values'
        H(2)='are from 1 to 89 degrees. '
        CALL EASKR(ANG,' ',' New angle of view ? ',
     &             1.0,'W',89.0,'W',40.,'angle ofview',IER,2)
        HANG=ANG/2.0
        MODLEN=.TRUE.
        MODIFY=.TRUE.
      ELSEIF(IVERT.EQ.4)THEN
        H(1)='In plan view you can change the viewing parameters,'
        H(2)='the elevations are primarily for checking. '
        CALL EASKABCD(' View choices: ',' ','Perspective',
     &    'plan view ','south elevation ','west elevation ?',IW,2)
        if(IW-1.ne.ITPPSW)then
          redraw =.true.
          MODIFY=.TRUE.
        endif
        ITPPSW=IW-1
      ELSEIF(IVERT.EQ.6)THEN

C If configuration file loaded then can filter surfaces by location.
C Allow this selection by text string.
        IF(CFGOK)THEN
          H(1)='The image may be filtered so that specific aspects'
          H(2)='of the model may be studied.  The current filters '
          H(3)='are shown in the list. '
          FALT(1)='all surfaces and shading obstructions'
          FALT(2)='all surfaces (no obstructions)       '
          FALT(3)='exterior surfaces only               '
          FALT(4)='partitions & interior surfaces only  '
          FALT(5)='surf connected to ground or constant '
          FALT(6)='all surfaces, obstructions & ground  '
          FALT(7)='ground topology only                 '
          IV=1
          CALL EPICKS(IV,IVAL,' ',' Image filter: ',
     &      37,7,FALT,' image filters',IER,1)
          IF(IV.GT.0)THEN
            ITDSP=IVAL(1)-1
          ELSE
            GOTO 92
          ENDIF
        ELSE
          ITDSP=1
        ENDIF
        MODLEN=.TRUE.
        MODBND=.TRUE.
        MODIFY=.TRUE.
      ELSEIF(IVERT.EQ.7)THEN

C Hilight control.
        h(1)='Display of surfaces can be limited to surfaces which'
        h(1)='match some filter. '
        CALL EASKATOG('Highlight choices: ',' ','normal','composition',
     &    'opaq:transp','partial attrib','continue ?',' ',' ',IW,2)
        ITHLS=IW-1
        if(ITHLS.eq.0)then
          MODIFY=.true.
        elseif(ITHLS.eq.1)then
          CALL EPKMLC(ISEL,'Select a construction to hilight.',' ',IER)
          ITHLZ=ISEL
          MODIFY=.TRUE.
        elseif(ITHLS.eq.2)then
          CALL EASKABC(' Opacity choices: ',' ','hilight opaque',
     &    'hilight transparent','continue ?',IW,2)
          ITHLZ=IW
          MODIFY=.TRUE.
        elseif(ITHLS.eq.3)then
          MODIFY=.TRUE.
        endif
      ELSEIF(IVERT.EQ.8)THEN

C So what does ITBND=3 (focus on zone) actually mean?
        ITBND=ITBND+1
        IF(ITBND.GT.1)ITBND=0
        MODIFY=.TRUE.
      ELSEIF(IVERT.EQ.10)THEN
        ITZNM=ITZNM+1
        IF(ITZNM.GT.1)ITZNM=0
        MODIFY=.TRUE.
      ELSEIF(IVERT.EQ.11)THEN
        ITSNM=ITSNM+1
        IF(ITSNM.GT.1)ITSNM=0
        MODIFY=.TRUE.
      ELSEIF(IVERT.EQ.12)THEN
        ITVNO=ITVNO+1
        IF(ITVNO.GT.1)ITVNO=0
        MODIFY=.TRUE.
      ELSEIF(IVERT.EQ.13)THEN

C Site origin, see if w/in bounds to flag bounds and lens...
        ITORG=ITORG+1
        IF(ITORG.GT.1)ITORG=0
        IF(1.1.GT.XMX.OR.1.1.LT.XMN) MODBND=.TRUE.
        IF(1.1.GT.YMX.OR.1.1.LT.YMN) MODBND=.TRUE.
        IF(0.0.GT.ZMX.OR.0.0.LT.ZMN) MODBND=.TRUE.
        if(MODBND)then
          MODLEN=.TRUE.
          MODIFY=.TRUE.
        endif
      ELSEIF(IVERT.EQ.14)THEN
        ITGRD=ITGRD+1
        IF(ITGRD.GT.1)ITGRD=0
        MODBND=.TRUE.
        MODLEN=.TRUE.
        MODIFY=.TRUE.
      ELSEIF(IVERT.EQ.15)THEN
        H(1)='The distance between site grid dotted lines can be'
        H(2)='altered for clarity, especially in large zones. '
        H(3)='If set to 0 then an optimum value will be found.'
        CALL EASKR(GRDIS,' ',' Grid distance (metres)? ',
     &             0.0,'F',10.0,'W',0.,'grid distance',IER,3)
        MODIFY=.TRUE.
      ELSEIF(IVERT.EQ.16)THEN
        ITSNR=ITSNR+1
        IF(ITSNR.GT.1)ITSNR=0
        MODIFY=.TRUE.
      ELSEIF(IVERT.EQ.18)THEN

C Select zones to include, if one zone is the principal focus then
C make sure it is included in the list.
        IF(CFGOK)THEN
          H(1)=' Pick one, several or all zones for inclusion'
          INPIC=NCOMP
          CALL EPICKS(INPIC,IVALS,' ',' Which zones to include: ',
     &      12,NCOMP,zname,' zone list',IER,1)
          nzg = inpic
          if(izgfoc.ne.0)then
            found=.false.
            do 44 mz=1,nzg
              nznog(mz)=IVALS(mz)
              if(ivals(mz).eq.izgfoc)found=.true.
  44        continue
            if(.NOT.found)then
              nzg=nzg+1
              nznog(nzg)=izgfoc
            endif
          elseif(izgfoc.eq.0)then
            do 24 mz=1,nzg
              nznog(mz)=IVALS(mz)
  24        continue
          endif
          MODBND=.TRUE.
          MODLEN=.TRUE.
          MODIFY=.TRUE.
        ELSE
          CALL USRMSG(' Model not yet defined!',
     &    ' Please use option a of model definition menu.','W')
          RETURN
        ENDIF
      ELSEIF(IVERT.EQ.19.AND.INPIC.GE.1)THEN

C Refresh the image.
        MODLEN=.TRUE.
        MODBND=.TRUE.
        MODIFY=.TRUE.
        do 444,ix=1,ncomp
          ZBFLG(ix)=0.
  444   continue
        CALL ADJVIEW(ITRC,IER)
        IF(ier.NE.0)RETURN
        MODLEN=.FALSE.
        MODBND=.FALSE.
        MODIFY=.FALSE.
      ELSEIF(IVERT.EQ.20)THEN

C Construct a 'viewer' format file. The trace level defines whether
C information about the zones is displayed or not. Because users
C who may be working in remote folders may wish to save locally
C trap this condition.
        IUO=IFIL+2
        iw=0
        if(path.ne.'./'.and.path.ne.' ')then
          write(outs,'(A,A)') ' The current path is: ',path
          call edisp(iuout,outs)
          CALL EASKAB(' You are in a remote folder...  place the ',
     &    ' viewing info:','using the path','in local folder',IW,0)
         endif

C Get file name.
        H(1)='A viewer format file may be used with the 3rd party'
        H(2)='wire-frame & hidden line program VIEWER.'
        CALL EASKS(LFIL,' File name for image data ? ',
     &    ' ',72,' ','viewer input file',IER,2)
        if(iw.eq.1)then
          CALL EFOPSEQ(IUO,LFIL,4,IER)
          currentfile=LFIL
        else
          CALL ERPFREE(IUO,ISTAT)
          call FPOPEN(IUO,ISTAT,1,3,LFIL)
          currentfile=LFIL
        endif
        CALL EMKVIEW(ITRC,IUO,CFGOK,IER)
        IF(IER.NE.0)THEN
          CALL USRMSG(' ',' problem creating image file... ','W')
          goto 92
        ENDIF
        dok=.true.
        h(1)='Assuming the conversion of the model geometry into '
        h(2)='the hidden line viewing file was successfull, you'
        h(3)='can now view the model via the ESP-r module `viewer`.'
        h(4)='Be sure to tidy up any *.vew files that might'
        h(5)='have been created. '
        CALL ASKOK(' ','Display hidden line views?',OK,dok,5)
        IF(OK)then

C Get logical name of child process terminal type, expand model
C name to include the path (if applicable) and create a string to
C drive viewer.
          doit = ' '
          call tchild(ICPMOD)
          call termode(ICPMOD,tmode)
          if(iw.eq.1)then
            call addpath(LFIL,longtfile,concat)
          else
            write(longtfile,'(a)') LFIL(1:lnblnk(LFIL))
          endif
          if(iappw.gt.0.and.iappw.le.200)then
            write(doit,'(3a,3i4,3a)') 'viewer -mode ',tmode,
     &        ' -s ',iappw,iappx+25,iappy+20,' -file ',
     &        longtfile(1:lnblnk(longtfile)),' &'
          else
            write(doit,'(5a)') 'viewer -mode ',tmode,
     &        ' -s 0 0 0 -file ',longtfile(1:lnblnk(longtfile)),' &'
          endif
          call usrmsg(' ','starting hidden line viewer.','-')
          call runit(doit,tmode)
        endif
      ELSEIF(IVERT.EQ.21)THEN

C Draw the zones and any obstructions based on sun
C position. Confirm the lat, long, request day and time,
C compute positon and update the view.
        write(outs,'(A,F6.2,A,F6.2)')
     &  ' Current latitude = ',SLAT,', longitude difference = ',SLON
        call edisp(iuout,' ')
        call edisp(iuout,outs)
        call edisp(iuout,' ')
        call edisp(iuout,' Date and time for the view... ')
        NTS=1
        IMO=6
        IDO=1
        STIME=1.

C Use the ASKTIM in esru_lib.F (as opposed to ASKRTIM in esrures/utils.F).
  67    CALL ASKTIM(2,NTS,IMO,IDO,IJDAY,STIME,IT,IER)
  68    CALL EAZALTS(STIME,IJDAY,SLAT,SLON,ISUNUP,SAZI,SALT)
        IF(SALT.GT.0.0)THEN
          CALL ANGXYZ(SAZI,SALT,X1,Y1,Z1)
          EYEM(1)=X1
          EYEM(2)=Y1
          EYEM(3)=Z1
          ITSNM=1
          MODIFY=.TRUE.
          MODLEN=.TRUE.
          CALL ADJVIEW(itrc,IER)

C Generate a heading for the view.
          CALL EDTIME(STIME,DESCRH,DESCRD,TIMER)
          WRITE(ETEXT,'(A,I2,1X,A3,A,A5,A,F6.1,A,F5.1)')
     &      ' View: ',IDO,MTHNAM(IMO),' @',DESCRH,
     &      '  azim:',SAZI,' elev:',SALT
          CALL viewtext(ETEXT,2,1,IFS)
          CALL EASKAB(' ',' Next viewpoint:',
     &        'next hour','exit',IW,0)
          if(IW.eq.1)then
            STIME=STIME+((60.0/FLOAT(NTS))/60.0)
            goto 68
          else
            ITSNM=0
            goto 92
          endif
        else
          CALL EASKAB(' View point below horizon:',' ',
     &        'specify another time','exit',IW,0)
          if(IW.eq.1)goto 67
          ITSNM=0
        endif
      ELSEIF(IVERT.EQ.22)THEN

C Produce help text for the  menu.
        H(1)='The 3dv menu provides control of the perspective image'
        H(2)='of the geometry begin browsed or edited. In most cases'
        H(3)='the image will be refreshed as editing takes place.'
        H(5)='The surface normal has yet to be enabled.'
        H(6)='In the case of inserted vertices which have not yet'
        H(7)='been associated with surfaces will be shown as circles'
        H(8)='within the image.'
        CALL PHELPD('3dv menu',8,'-',0,0,IER)
      ELSE
C Not one of the legal menu choices.
        IVERT=-1
        GOTO 92
      ENDIF
      IVERT=-2
      GOTO 92

      END

C ******************** ASKTIM
C ASKTIM is a standard call to enquire which month and day and time
C (point in time for snapshot analysis. Returns IMO (month), IDO (day
C of month), IJDAY (day of year for output), TIME (real representation),
C IT (timestep). IFDAY is a toggle provided in setres.f to control
C the display and input of periods - 0 = julian day, 1 or 2 = day of
C month.
      SUBROUTINE ASKTIM(IFDAY,NTS,IMO,IDO,IJDAY,TIME,IT,IER)

      COMMON/OUTIN/IUOUT,IUIN
      common/pophelp/h(60)

      character H*72,HOLD*24
      DIMENSION ID(12)
      DATA ID/31,28,31,30,31,30,31,31,30,31,30,31/

      IER=0
      H(1)='The output time defines the day, month and decimal '
      H(2)='hour at which the output is requested. '
      H(3)='This must be no earlier than 0.00 hours or later '
      H(4)='than 24.00 hours. '
      H(5)=' '
      IF(IFDAY.EQ.0)THEN
        CALL EDAY(IDO,IMO,IJDAY)
        write(HOLD,'(I6,F6.1)') IJDAY,TIME
        H(6)='Example:  6th March at 9h30 am is given as 64  9.5 '
      else
        write(HOLD,'(I6,I4,F6.1)') IDO,IMO,TIME
        H(6)='Example:  6th March at 9h30 am is given as 6 3 9.5 '
      endif

  281 IF(IFDAY.EQ.0)THEN
        CALL EASKS(HOLD,' ',' Day-of-year & time: ',
     &     24,' 1  7.0 ','doy and time',IER,6)
        K=0
        CALL EGETWI(HOLD,K,IJDAY,1,365,'F','day of year',IER)
        CALL EGETWR(HOLD,K,TIME,0.0,24.0,'F','time',IER)
        if(IER.ne.0)goto 281
        CALL EDAYR(IJDAY,IDO,IMO)
      ELSE
 283    CALL EASKS(HOLD,' ',' Day-of-month, month & time: ',
     &     24,' 1  1  7.0 ','doy, month time',IER,6)
        K=0
        CALL EGETWI(HOLD,K,IDO,1,31,'F','day of month',IER)
        CALL EGETWI(HOLD,K,IMO,1,12,'F','month',IER)
        CALL EGETWR(HOLD,K,TIME,0.0,24.0,'F','time',IER)
        if(IER.ne.0)goto 283
        CALL EDAY(IDO,IMO,IJDAY)
      ENDIF

C Check range.
      IF(IDO.GT.ID(IMO))THEN
        call edisp(iuout,' Day past end of month... try again. ')
        goto 281
      ENDIF
      CALL EDAY(IDO,IMO,IJDAY)

C Convert time to time-step number.
      XX=TIME+(1.0/(FLOAT(NTS)*2.0))
      IT=INT(XX)*NTS
      IF(IT.EQ.0)IT=1

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
      DOT=0.
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

c ******************** SOLAIR ********************
c Routine to compute sol-air temperature.
      FUNCTION SOLAIR(T,QF,QD)

c A temporary measure.
      SOLAIR=T
      RETURN
      END

C ********************** DNOTZERO **********************
C DNOTZERO returns a non zero value with the same sign.
      DOUBLE PRECISION FUNCTION DNOTZERO(A)

      double precision A,small
      SMALL=1E-35

      if (abs(A).lt.(SMALL)) then

C Value is approx zero, return small with correct sign.
        DNOTZERO=SIGN(SMALL,A)
C        write (6,*) 'dzero ',A,DNOTZERO
      else
        DNOTZERO=A
      endif

      RETURN
      END

C ********************** ANOTZERO **********************
C ANOTZERO returns a non zero value with the same sign.
      REAL FUNCTION ANOTZERO(A)

      SMALL=1E-19

      if (abs(A).lt.(SMALL)) then

C Value is approx zero, return small with correct sign.
        ANOTZERO=SIGN(SMALL,A)
C        write (6,*) 'Azero ',A,ANOTZERO
      else
        ANOTZERO=A
      endif

      RETURN
      END

C ***************** SIGFIG *****************
C SIGFIG returns number to required significant figure level.
C R - real number
C NSIG - number of significant figureds required.
C RNO - real number to NSIG figures
C STR - string version of RNO
C LSTR - length of STR

       subroutine sigfig(R,NSIG,RNO,STR,LSTR)

       character*12 STR, TMP, FMT

       STR='  '

C Need to check if number is greater or less than one (i.e. should
C zeros be counted.
       if (abs(R).gt.1.) then

C Compare number against required number if sig figs.
         if (abs(R).gt.(10.**NSIG)) then

C Need to turn last NZD digits to zero's.
           NZD=0
 100       if (abs(R)/(10.**NZD).gt.(10.**NSIG)) then
             NZD=NZD+1
             goto 100
           endif
           RNO=real(NINT(R/10.**NZD)*10.**NZD)
           if (NZD.gt.6.or.RNO.gt.1.0e12) then
             write (FMT,'(a,i2,a)') '(g12.',NSIG,')'
           else
             write (FMT,'(a)') '(f12.0)'
           endif
           write (STR,FMT) RNO
         else

C Need to preserve NDP decimal places.
           NDP=0
 110       if (abs(R)*10.**NDP.lt.(10.**NSIG)) then
             NDP=NDP+1
             goto 110
           endif
           NDP=NDP-1
           RNO=real(NINT(R*10.**NDP))/10.**NDP
           if(NDP.lt.0) NDP=0
           write (FMT,'(a,i2,a)') '(f12.',NDP,')'
           write (STR,FMT) RNO
         endif
       else

C Need to preserve NSIG decimal places.
         RNO=real(NINT(R*10.**NSIG))/10.**NSIG
         write (FMT,'(a,i2,a)') '(f12.',NSIG,')'
         write (STR,FMT) RNO
       endif

C Strip leading blanks from text string.
       K=1
       call EGETRM(STR,K,TMP,'-','generating sig fig string ',IER)
       write (STR,'(a)') TMP
       LSTR=max(1,lnblnk(STR))

       return
       end

C ***************** SIpre *****************
C SIpre returns suitable SI prefix for number supplied.  NOTE that
C   it is assumed that the value is supplied in the base SI unit, for
C   example supply a flux in W not kW.
C
C R - real number
C NSIG - number of significant figureds required.
C RNO - real number to NSIG figures, including prefix
C STR - string version of RNO
C LSTR - length of STR
C PRE - prefix
C SYM - symbol

       subroutine SIpre(R,NSIG,RNO,STR,LSTR,PRE,SYM)

       character*12 STR
       character SYM*1, SYMH(6)*1, SYML(6)*1
       character PRE*5, PREH(6)*5, PREL(6)*5

       data SYMH/'k','M','G','T','P','E'/
       data PREH/'kilo ','mega ','giga ','tera ','peta ','exa  '/
       data SYML/'m','u','n','p','f','a'/
       data PREL/'milli','micro','nano ','pico ','femto','atto '/

C Set default output
       RNO=R
       STR=' '
       SYM=' '
       PRE=' '

C Need to check if big or small number
       if (R.gt.1.) then
         IP=0
 100     if (R.gt.(1000.**IP)) then
           IP=IP+1
           goto 100
         endif
         IP=IP-1
         if (IP.gt.0.and.IP.lt.7) then
           RX=R/(1000.**IP)
           call sigfig(RX,NSIG,RNO,STR,LSTR)
           PRE=PREH(IP)
           SYM=SYMH(IP)
         else
           call sigfig(R,NSIG,RNO,STR,LSTR)
         endif
       else
         IP=0
 110     if (R.lt.(0.001**IP)) then
           IP=IP+1
           goto 110
         endif
         if (IP.gt.0.and.IP.lt.7) then
           RX=R*(1000.**IP)
           call sigfig(RX,NSIG,RNO,STR,LSTR)
           PRE=PREL(IP)
           SYM=SYML(IP)
         else
           call sigfig(R,NSIG,RNO,STR,LSTR)
         endif

       endif

       return
       end

C ******** startbuffer
C Wrapper around call to win3dclr in esru_x.c to match
C the functionality of startbuffer in the GTK version.
       subroutine startbuffer()
       call win3dclr
       return
       end

C ***************** PRONAM *****************
C Subroutine pronam returns the characters of a string after the last
C occurance of '/' or '\'

      SUBROUTINE PRONAM(longstr,last)

      CHARACTER*(*) LONGSTR,LAST

      ILEN=LNBLNK(LONGSTR)
      DO 90 I=1,ILEN
        IF(LONGSTR(I:I).EQ.'/'.OR.LONGSTR(I:I).EQ.'\\')LOCUR=I
 90   CONTINUE
      LAST=LONGSTR(LOCUR:ILEN)
      RETURN
      END

